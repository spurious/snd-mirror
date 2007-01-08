;;; various mix and track related utilities
;;;
;;; (pan-mix file frame envelope) mixes file into current sound starting at frame using envelope to pan (0: all chan 0, 1: all chan 1)
;;; (mix->vct id) return mix data in vct
;;; (snap-mix-to-beat (at-tag-position)) forces dragged mix to end up on a beat
;;; (delete-all-mixes) removes all mixes
;;; (find-mix sample snd chn) returns the id of the mix at the given sample, or #f
;;; (save-mix mix filename) saves mix data in file filename
;;; (mix-maxamp id) maxamp of mix
;;;
;;; (track->vct track (chan 0)) place track data in vct
;;; (save-track track filename (chan 0)) save track data in file
;;; (filter-track track coeffs) filter track data
;;; (reverse-track track) reverses the mix order
;;; (delete-all-tracks) removes all mixes that have an associated track (sets all amps to 0)
;;; (set-all-tracks new-id) places all mixes in track new-id
;;;
;;; (transpose-track track semitones) transposes each mix in track  by semitones
;;; (retempo-track track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)
;;; (track-maxamp id) maxamp of track
;;;
;;; mix-property associates a property list with a mix
;;; track-property associates a property list with a track
;;; mix-click-sets-amp sets up hook functions so that mix click zeros amps, then subsequent click resets to the before-zero value

(use-modules (ice-9 common-list))
(provide 'snd-mix.scm)

(if (not (defined? 'remove-if))
    (define (remove-if pred l) ; from guile/ice-9/common-list.scm
      (let loop ((l l) (result '()))
	(cond ((null? l) (reverse! result))
	      ((pred (car l)) (loop (cdr l) result))
	      (else (loop (cdr l) (cons (car l) result)))))))

(if (not (defined? 'some))
    (define (some pred l . rest)
      (cond ((null? rest)
	     (let mapf ((l l))
	       (and (not (null? l))
		    (or (pred (car l)) (mapf (cdr l))))))
	    (else (let mapf ((l l) (rest rest))
		    (and (not (null? l))
			 (or (apply pred (car l) (map car rest))
			     (mapf (cdr l) (map cdr rest)))))))))


(define (tree-for-each func tree)
  "(tree-for-each func tree) applies func to every leaf of 'tree'"
  (cond ((null? tree) '())
	((not (pair? tree)) (func tree))
	(else (tree-for-each func (car tree))
	      (tree-for-each func (cdr tree)))))

(define+ (tree-for-each-reversed func tree)
  "(tree-for-each-reversed func tree) applies func to every leaf of 'tree' moving in reverse through all the lists"
  (define (flatten lst)
    ;; there's probably a more elegant way to do this
    (define (list-p val)
      (and (list? val)
	   (not (null? val))))
    (cond ((null? lst) '())
	  ((list-p lst)
	   (if (list-p (car lst))
	       (append (flatten (car lst)) (flatten (cdr lst)))
	       (cons (car lst) (flatten (cdr lst)))))
	  (#t lst)))
  (for-each func (reverse (flatten tree))))



;;; ---------------- mixes ----------------

(define (mix-sound file start)
  "(mix-sound file start) mixes file (all chans) at start in the currently selected sound."
  (mix file start #t))

(define (delete-all-mixes)
  "(delete-all-mixes) removes all mixes (sets all amps to 0)"
  (as-one-edit
    (lambda ()
      (tree-for-each
        (lambda (id)
          (delete-mix id))
        (mixes)))))


(define* (find-mix sample :optional snd chn)
  "(find-mix sample :optional snd chn) returns the id of the mix at the given sample, or #f"
  (let ((mix-list (mixes (or snd (selected-sound) (car (sounds))) (or chn (selected-channel snd) 0))))
    (call-with-current-continuation
     (lambda (found-it)
       (for-each
	(lambda (n)
	  (if (= (mix-position n) sample)
	      (found-it n)))
	mix-list)
       #f))))


;;; -------- pan-mix --------

(define* (pan-mix name :optional (beg 0) (envelope 1.0) snd (chn 0) auto-delete)
  "(pan-mix file (start 0) (envelope 1.0) snd (chn 0) (auto-delete #f)) mixes 'file' into the sound 'snd'
starting at start (in samples) using 'envelope' to pan (0: all chan 0, 1: all chan 1).
So, (pan-mix \"oboe.snd\" .1 '(0 0 1 1)) goes from all chan 0 to all chan 1.  If
the variable with-tags is #t, the resultant mixes are placed in their own track, and 
the track envelope controls the panning. 
If 'envelope' is a scaler, it is turned into an evelope at that value. 'auto-delete' determines
whether the in-coming file should be treated as a temporary file and deleted when the mix
is no longer accessible."

  (let ((index (or snd (selected-sound) (and (sounds) (car (sounds)))))
	(old-with-mix-tags (with-mix-tags)))
    (if (not (sound? index))
	(throw 'no-such-sound (list "pan-mix" snd)))
    (if (not (file-exists? name))
	(throw 'no-such-file (list "pan-mix" name)))
    (let ((new-mix
	   (dynamic-wind
	    (lambda () (set! (with-mix-tags) #t))
	    (lambda ()
	      (let ((incoming-chans (mus-sound-chans name))
		    (receiving-chans (chans index))
		    (old-sync (sync index))
		    (track-func (if (list? envelope) envelope (list 0 envelope 1 envelope))))
		(if (= receiving-chans 1)
		    (if (= incoming-chans 1)
			(let ((id (mix name beg 0 index 0 #t auto-delete)))
			  (if (list? envelope)
			      (set! (mix-amp-env id 0) envelope)
			      (set! (mix-amp id 0) envelope))
			  id)
			(as-one-edit
			 ;; incoming chans > 2 ignored
			 (lambda ()
			   (let* ((trk (make-track))
				  (mix0 (mix name beg 0 index 0 #t auto-delete trk))
				  (mix1 (mix name beg 1 index 0 #t auto-delete trk)))
			     (set! (mix-inverted? mix1) #t)
			     (set! (track-amp-env trk) track-func)
			     mix0))))
		    (let* ((chan0 chn)
			   (chan1 (modulo (1+ chn) receiving-chans)))
		      ;; receiving-chans >= 2, so start-chan comes into play
		      (if (= incoming-chans 1)
			  (dynamic-wind
			   (lambda ()
			     (set! (sync index) #f))
			   (lambda ()
			     (as-one-edit
			      (lambda ()
				(let* ((trk (make-track))
				       (mix0 (mix name beg 0 index chan0 #t auto-delete trk))
				       (mix1 (mix name beg 0 index chan1 #t auto-delete trk)))
				  (set! (mix-inverted? mix1) #t)
				  (set! (track-amp-env trk) track-func)
				  mix0))))
			   (lambda ()
			     (set! (sync index) old-sync)))
			  (let ((new-sync 0))
			    ;; incoming chans > 2 ignored
			    (for-each (lambda (s) (if (>= (sync s) new-sync) (set! new-sync (1+ (sync s))))) (sounds))
			    (dynamic-wind
			     (lambda ()
			       (set! (sync index) new-sync))
			     (lambda ()
			       (as-one-edit
				(lambda ()
				  (let* ((trk (make-track))
					 (mix0 (mix name beg 0 index chan0 #t auto-delete trk))
					 (mix1 (mix name beg 1 index chan1 #t auto-delete trk)))
				    (set! (mix-inverted? mix1) #t)
				    (set! (track-amp-env trk) track-func)
				    mix0))))
			     (lambda ()
			       (set! (sync index) old-sync)))))))))
	    (lambda ()
	      (set! (with-mix-tags) old-with-mix-tags)))))
      (if (and (mix? new-mix)
	       (not old-with-mix-tags))
	  (if (track? (mix-track new-mix))
	      (lock-track (mix-track new-mix))
	      (set! (mix-locked? new-mix) #t)))
      new-mix)))

(define* (pan-mix-selection :optional (beg 0) (envelope 1.0) snd (chn 0))
  "(pan-mix-selection (start 0) (envelope 1.0) snd (chn 0)) mixes the current selection  into the sound 'snd'
starting at 'start' (in samples) using 'envelope' to pan (0: all chan 0, 1: all chan 1)."
  (if (not (selection?))
      (throw 'no-active-selection (list "pan-mix-selection"))
      (pan-mix (save-selection (snd-tempnam)) beg envelope snd chn #t)))

(define* (pan-mix-region reg :optional (beg 0) (envelope 1.0) snd (chn 0))
  "(pan-mix-region reg (start 0) (envelope 1.0) snd (chn 0)) mixes the given region into the sound 'snd' 
starting at 'start' (in samples) using 'envelope' to pan (0: all chan 0, 1: all chan 1)."
  (if (not (region? reg))
      (throw 'no-such-region (list "pan-mix-region" reg))
      (pan-mix (save-region reg (snd-tempnam)) beg envelope snd chn #t)))

(define* (pan-mix-vct v :optional (beg 0) (envelope 1.0) snd (chn 0))
  "(pan-mix-vct v (start 0) (envelope 1.0) snd (chn 0)) mixes the vct data into the sound 'snd' 
starting at 'start' (in samples) using 'envelope' to pan (0: all chan 0, 1: all chan 1)."
  (let* ((temp-file (snd-tempnam))
	 (fd (mus-sound-open-output temp-file (srate snd) 1 #f #f "")))
    (mus-sound-write fd 0 (1- (vct-length v)) 1 (vct->sound-data v))
    (mus-sound-close-output fd (* 4 (vct-length v)))
    (pan-mix temp-file beg envelope snd chn #t)))

(define (mix->vct id)
  "(mix->vct id) returns mix's data in vct"
  (if (mix? id)
      (let* ((len (mix-frames id))
	     (v (make-vct len))
	     (reader (make-mix-sample-reader id)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (vct-set! v i (read-mix-sample reader)))
	(free-sample-reader reader)
	v)
      (throw 'no-such-mix (list "mix->vct" id))))

(define (save-mix id filename)
  "(save-mix id filename) saves mix data (as floats) in file filename"
  (let ((v (mix->vct id))
	(fd (mus-sound-open-output filename (srate) 1 #f #f "")))
    (mus-sound-write fd 0 (1- (vct-length v)) 1 (vct->sound-data v))
    (mus-sound-close-output fd (* 4 (vct-length v)))))

(define (mix-maxamp id)
  "(mix-maxamp id) returns the max amp in the given mix"
  (if (mix? id)
      (let* ((len (mix-frames id))
	     (peak 0.0)
	     (reader (make-mix-sample-reader id)))
	(set! peak (abs (read-mix-sample reader)))
	(do ((i 1 (1+ i)))
	    ((= i len))
	  (let ((val (abs (read-mix-sample reader))))
	    (if (> val peak)
		(set! peak val))))
	(free-sample-reader reader)
	peak)
      (throw 'no-such-mix (list "mix-maxamp" id))))
	  

(define* (snap-mix-to-beat :optional (at-tag-position #f))
  "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute).  reset mix-release-hook to cancel"
  (add-hook! mix-release-hook
	     (lambda (id samps-moved)
	       (let* ((offset (if at-tag-position (mix-tag-position id) 0))
		      (samp (+ samps-moved (mix-position id) offset))
		      (snd (car (mix-home id)))
		      (chn (cadr (mix-home id)))
		      (bps (/ (beats-per-minute snd chn) 60.0))
		      (sr (srate snd))
		      (beat (floor (/ (* samp bps) sr)))
		      (lower (inexact->exact (floor (/ (* beat sr) bps))))
		      (higher (inexact->exact (floor (/ (* (1+ beat) sr) bps)))))
		 (set! (mix-position id)
		       (if (< (- samp lower) (- higher samp))
			   (max 0 (- lower offset))
			   (- higher offset)))
		 #t))))


;;; --------------------------------------------------------------------------------

(define all-mix-properties '())

(define mix-properties
  (make-procedure-with-setter
   (lambda (id)
     "(mix-properties id) accesses the property list of mix 'id'"
     (let ((data (assoc id all-mix-properties)))
       (if data
	   (cdr data)
           '())))
   (lambda (id new-val)
     (let ((old-val (assoc id all-mix-properties)))
       (if old-val
	   (set-cdr! old-val new-val)
	   (set! all-mix-properties (cons (cons id new-val) all-mix-properties)))
       new-val))))
     
(define mix-property
  (make-procedure-with-setter
   (lambda (key id)
     "(mix-property key id) returns the value associated with 'key' in the given mix's property list, or #f"
     (if (mix? id)
	 (let ((data (assoc key (mix-properties id))))
	   (if data
	       (cdr data)
	       #f))
	 (throw 'no-such-mix (list "mix-property" id))))
   (lambda (key id new-val)
     (if (mix? id)
	 (let ((old-val (assoc key (mix-properties id))))
	   (if old-val
	       (set-cdr! old-val new-val)
	       (set! (mix-properties id) (cons (cons key new-val) (mix-properties id))))
	   new-val)
	 (throw 'no-such-mix (list "set! mix-property" id))))))

(add-hook! close-hook
	   (lambda (snd)
	     (if (not (null? all-mix-properties))
		 ;; prune out inactive mix properties
		 (set! all-mix-properties (remove-if (lambda (val)
						       (not (mix? (car val))))
						     all-mix-properties)))))

(define (mix-click-sets-amp)
  (add-hook! mix-click-hook 
	     (lambda (n)
	       (let ((zeroed (mix-property :zero n)))
		 (if (not zeroed)
		     (let ((amps '()))
		       (do ((i (1- (mix-chans n)) (1- i)))
			   ((< i 0))
			 (set! amps (cons (mix-amp n i) amps)))
		       (set! (mix-property :amps n) amps)
		       (do ((i 0 (1+ i)))
			   ((= i (mix-chans n)))
			 (set! (mix-amp n i) 0.0))
		       (set! (mix-property :zero n) #t))
		     (let ((amps (mix-property :amps n)))
		       (do ((i 0 (1+ i)))
			   ((= i (mix-chans n)))
			 (set! (mix-amp n i) (list-ref amps i)))
		       (set! (mix-property :zero n) #f)))
		 #t))))


;;; --------------------------------------------------------------------------------

(define (delete-all-tracks)
  "(delete-all-tracks) removes all mixes that have an associated track (sets all amps to 0)"
  (as-one-edit
    (lambda ()
      (tree-for-each
        (lambda (id)
	  (if (not (= (mix-track id) 0))
	      (delete-mix id)))
        (mixes)))))


(define (reverse-track trk)
  "(reverse-track) reverses the order of its mixes (it changes various mix begin times)"
  (if (some mix? (track trk))
      (let* ((ids-in-order (sort (track trk)
				 (lambda (a b)
				   (> (mix-position a)
				      (mix-position b))))))
	(as-one-edit
	 (lambda ()
	   (for-each (lambda (id pos)
		       (set! (mix-position id) pos))
		     ids-in-order
		     (reverse (map mix-position ids-in-order))))))
      (throw 'no-such-track (list "reverse-track" trk))))


(define* (track->vct trk :optional (chan 0))
  "(track->vct track (chan 0)) places track data in vct"
  (if (track? trk)
      (if (< chan (track-chans trk))
	  (let* ((len (track-frames trk chan))
		 (v (make-vct len))
		 (rd (make-track-sample-reader trk chan)))
	    (run
	     (lambda ()
	       (do ((i 0 (1+ i)))
		   ((= i len) v)
		 (vct-set! v i (read-track-sample rd))))))
	  (throw 'no-such-channel (list "track->vct" chan)))
      (throw 'no-such-track (list "track->vct" trk))))

(define* (save-track trk filename :optional (chan #t))
  "(save-track track filename (chan #t)) saves track data (as floats) in file filename"
  (if (track? trk)
      (let ((chans (track-chans trk)))
	(if (or (and (eq? chan #t)
		     (= chans 1))
		(and (integer? chan)
		     (< chan chans)))
	    (let* ((current-chan (if (eq? chan #t) 0 chan))
		   (v (track->vct trk current-chan))
		   (fd (mus-sound-open-output filename (srate) 1 #f #f "written by save-track")))
	      (mus-sound-write fd 0 (1- (vct-length v)) 1 (vct->sound-data v))
    	      (mus-sound-close-output fd (* 4 (vct-length v))))
	    (if (and (eq? chan #t)
		     (> chans 0))
		(let* ((fd (mus-sound-open-output filename (srate) chans #f #f "written by save-track"))
		       (len (track-frames trk))
		       (pos (track-position trk))
		       (sd (make-sound-data chans len)))
		  (do ((i 0 (1+ i)))
		      ((= i chans))
		    (let* ((chan-len (track-frames trk i))
			   (chan-pos (- (track-position trk i) pos))
			   (reader (make-track-sample-reader trk i)) ; beg is next arg if it's needed
			   (chan-end (+ chan-pos chan-len)))
		      (do ((j chan-pos (1+ j)))
			  ((= j chan-end))
			(sound-data-set! sd i j (read-track-sample reader)))))
		  (mus-sound-write fd 0 (1- len) chans sd)
		  (mus-sound-close-output fd (* len chans 4)))
		(throw 'no-such-channel (list "save-track" chan)))))
      (throw 'no-such-track (list "save-track" trk))))
	  

(define* (track-maxamp id :optional chan)
  "(track-maxamp id :optional chan) returns the max amp in the given track"
  (if (track? id)
      (if (number? chan)
	  (let* ((len (track-frames id chan))
		 (peak 0.0)
		 (reader (make-track-sample-reader id chan 0)))
	    (set! peak (abs (read-track-sample reader)))
	    (do ((i 1 (1+ i)))
		((= i len))
	      (let ((val (abs (read-track-sample reader))))
		(if (> val peak)
		    (set! peak val))))
	    (free-sample-reader reader)
	    peak)
	  (let ((maxs '())
		(chans (track-chans id)))
	    (do ((chn 0 (1+ chn)))
		((= chn chans))
	      (set! maxs (cons (track-maxamp id chn) maxs)))
	    (reverse maxs)))
      (throw 'no-such-track (list "track-maxamp" id))))
	  
(define (transpose-track trk semitones)
  "(transpose-track track semitones) transposes each mix in track by semitones"
  (let ((mult (expt 2.0 (/ semitones 12.0))))
    (set! (track-speed trk) (* (track-speed trk) mult))))

(define (retempo-track trk tempo)
  "(retempo-track track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)"
  (set! (track-tempo trk) (* (track-tempo trk) tempo)))

;;; (retempo-track 1 2.0)

(define (filter-track track-id fir-filter-coeffs)
  "(filter-track track coeffs) filters track data using FIR filter coeffs: (filter-track track-id '(.1 .2 .3 .3 .2 .1))"
  (if (track? track-id)
      (let ((order (length fir-filter-coeffs))
	    (chans (track-chans track-id)))
	(do ((chan 0 (1+ chan)))
	    ((= chan chans))
	  (let ((beg (track-position track-id chan))
		(dur (track-frames track-id chan))
		(flt (make-fir-filter order (list->vct fir-filter-coeffs)))
		(reader (make-track-sample-reader track-id chan 0)))
	    (map-channel (lambda (y)
			   (let ((val (read-track-sample reader)))
			     (+ y (- (fir-filter flt val) val))))
			 beg (+ dur order) #f #f #f "filter-track"))))
      (throw 'no-such-track (list "filter-track" track-id))))


;;; --------------------------------------------------------------------------------

(define all-track-properties '())

(define track-properties
  (make-procedure-with-setter
   (lambda (id)
     "(track-properties trk) accesses the property list of track 'trk'"
     (let ((data (assoc id all-track-properties)))
       (if data
	   (cdr data)
           '())))
   (lambda (id new-val)
     (let ((old-val (assoc id all-track-properties)))
       (if old-val
	   (set-cdr! old-val new-val)
	   (set! all-track-properties (cons (cons id new-val) all-track-properties)))
       new-val))))
     
(define track-property
  (make-procedure-with-setter
   (lambda (key id)
     "(track-property key id) returns the value associated with 'key' in the given track's property list, or #f"
     (if (track? id)
	 (let ((data (assoc key (track-properties id))))
	   (if data
	       (cdr data)
	       #f))
	 (throw 'no-such-track (list "track-property" id))))
   (lambda (key id new-val)
     (if (track? id)
	 (let ((old-val (assoc key (track-properties id))))
	   (if old-val
	       (set-cdr! old-val new-val)
	       (set! (track-properties id) (cons (cons key new-val) (track-properties id))))
	   new-val)
	 (throw 'no-such-track (list "set! track-property" id))))))


;;; --------------------------------------------------------------------------------

(define (mix-click-info n)
  "(mix-click-info n) is a mix-click-hook function that describes a mix and its properties"
  (help-dialog "Mix Help"
	       (format #f "Mix ~A:~%  position: ~D = ~,3F secs~%  length: ~D (~,3F secs)
~%  in: ~A[~D]~A~A~A~%  scalers: ~A~%  speed: ~A~%  envs: ~{~A~^~%    ~}~A~A"
		       (if (mix-name n)
			   (format #f "~S (~D)" (mix-name n) n)
			   (format #f "~D" n))
		       (mix-position n)
		       (exact->inexact (/ (mix-position n) (srate (car (mix-home n)))))
		       (mix-frames n)
		       (exact->inexact (/ (mix-frames n) (srate (car (mix-home n)))))
		       (short-file-name (car (mix-home n))) (cadr (mix-home n))
		       (if (mix-locked? n) ", (locked)" "")
		       (if (mix-inverted? n) ", (inverted)" "")
		       (if (not (= (mix-track n) 0))
			   (format #f "~%  track: ~A" (mix-track n))
			   "")
		       (let ((scls '()))
			 (do ((i (1- (mix-chans n)) (1- i)))
			     ((< i 0) scls)
			   (set! scls (cons (mix-amp n i) scls))))
		       (mix-speed n)
		       (let ((es '()))
			 (do ((i (1- (mix-chans n)) (1- i)))
			     ((< i 0) es)
			   (set! es (cons (mix-amp-env n i) es))))
		       (if (not (= (mix-tag-position n) 0))
			   (format #f "~%  tag-position: ~A" (mix-tag-position n))
			   "")
		       (let ((props (mix-properties n)))
			 (if (and (list? props)
				  (not (null? props)))
			     (format #f "~%  properties: '~A" props)
			     ""))))
  #t)


;;; -------- mix-name->id and track-name->id 

(define (mix-name->id name)
  "(mix-name->id name) returns the mix id associated with 'name'"
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (snd)
	(do ((chn 0 (1+ chn)))
	    ((= chn (channels snd)))
	  (for-each
	   (lambda (m)
	     (if (and (string? (mix-name m))
		      (string=? (mix-name m) name))
		 (return m)))
	   (mixes snd chn))))
      (sounds))
     'no-such-mix)))

(define (track-name->id name)
  "(track-name->id name) returns the track id associated with 'name'"
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (trk)
	(if (and (string? (track-name trk))
		 (string=? (track-name trk) name))
	    (return trk)))
      (tracks))
     'no-such-track)))


