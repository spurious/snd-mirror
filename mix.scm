;;; this file is being changed to move most of the previous track
;;;   functions into Snd -- all the previous functionality will be
;;;   re-implemented eventually.



;;; various mix and track related utilities
;;;
;;; (pan-mix file frame envelope) mixes file into current sound starting at frame using envelope to pan (0: all chan 0, 1: all chan 1)
;;; (mix->vct id) return mix data in vct
;;; (snap-mix-to-beat (at-anchor)) forces dragged mix to end up on a beat
;;; (delete-all-mixes) removes all mixes
;;; (find-mix sample snd chn) returns the id of the mix at the given sample, or #f
;;; TODO: (save-mix mix filename)
;;; TODO: (clone-mix mix new-beg snd chn) or (copy-mix...) -- the distinction being that "clone" saves state, copy makes a new (initial) state
;;;       would be nice here to use same temp file (with tempfile_ctr snd-io.c) = md->in_filename always (no arrays for tagged mixes)
;;; TODO: add mix-file-name (parallel file-name)
;;;       snd-mix 375 multichannel_deletion already an option, so we could just change md->temporary 
;;; TODO: (clone-track track new-beg) or (copy-track...)
;;;
;;; (track->vct track) place track data in vct
;;; (save-track track filename) save track data in file
;;; (filter-track track coeffs) filter track data
;;; (reverse-track track) reverses the mix order
;;; (delete-all-tracks) removes all mixes that have an associated track (sets all amps to 0)
;;; (set-all-tracks new-id) places all mixes in track new-id
;;;
;;; (transpose-track track semitones) transposes each mix in track  by semitones
;;; (retempo-track track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)
;;;
;;; mix-property associates a property list with a mix
;;; track-property associates a property list with a track
;;; mix-click-sets-amp sets up hook functions so that mix click zeros amps, then subsequent click resets to the before-zero value



(define (tree-for-each func tree)
  "(tree-for-each func tree) applies func to every leaf of 'tree'"
  (cond ((null? tree) '())
	((not (pair? tree)) (func tree))
	(else (tree-for-each func (car tree))
	      (tree-for-each func (cdr tree)))))

(define (tree-for-each-reversed func tree)
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


(define* (find-mix sample #:optional (snd #f) (chn #f))
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

(define (invert-envelope env)
  (if (null? env) 
      '()
      (cons (car env) 
	    (cons (- 1.0 (cadr env)) 
		  (invert-envelope (cddr env))))))

;;; TODO: fix pan-mix or automate 
(define* (pan-mix name #:optional (beg 0) (envelope 1.0) snd (chn 0))
  "(pan-mix file (start 0) (envelope 1.0) snd chn) mixes 'file' into the sound 'snd' \
starting at start (in samples) using 'envelope' to pan (0: all chan 0, 1: all chan 1).\
So, (pan-mix \"oboe.snd\" .1 '(0 0 1 1)) goes from all chan 0 to all chan 1.  If\
the variable with-tags is #t, the resultant mixes are syncd together, and \
the envelope is reflected in each channel's mix-amp-env; if you subsequently \
change one channel's envelope, the other is automatically reset as well.\
If 'envelope' is a scaler, it represents the channel 0 scaler; 1.0 - amp is\
the channel 1 scaler, and a subsquent set! of either mix-amp is reflected\
in the other channel. 'chn' is the start channel for all this (logical channel 0)."

  (let ((index (or snd (selected-sound) (and (sounds) (car (sounds))))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "pan-mix" snd)))
    (if (not (file-exists? name))
	(throw 'no-such-file (list "pan-mix" name)))
    (let ((incoming-chans (mus-sound-chans name))
	  (receiving-chans (chans index))
	  (old-sync (sync index))
	  (inverted-envelope
	   (if (list? envelope)
	       (invert-envelope envelope)
	       (- 1.0 envelope)))
	  (mix-func (if (list? envelope) mix-amp-env mix-amp)))
      (if (= receiving-chans 1)
	  (if (= incoming-chans 1)
	      (let ((id (mix name beg 0 index 0)))
		(if (list? envelope)
		    (set! (mix-amp-env id 0) envelope)
		    (set! (mix-amp id 0) envelope))
		id)
	      (as-one-edit
	       ;; incoming chans > 2 ignored
	       (lambda ()
		 (let ((mix0 (mix name beg 0 index)))
		   (set! (mix-func mix0 0) envelope)
		   (set! (mix-func mix0 1) inverted-envelope)
		   (if (with-mix-tags)
		       (set! (mix-property 'pan-mix mix0) (list mix0 0)))
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
		      (let ((mix0 (mix name beg 0 index chan0))
			    (mix1 (mix name beg 0 index chan1)))
			  (set! (mix-func mix0 0) envelope)
			  (set! (mix-func mix1 0) inverted-envelope)
			  (if (with-mix-tags)
			      (let ((trk (make-track))) ; needed for position and speed changes
				(set! (mix-track mix0) trk)
				(set! (mix-track mix1) trk)
				(set! (mix-property 'pan-mix mix0) (list mix1 0))
				(set! (mix-property 'pan-mix mix1) (list mix0 0))))
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
			(let* ((mix0 (mix name beg #f index chan0))
			       (mix1 (1+ mix0)))
			  (set! (mix-func mix0 0) envelope)
			  (set! (mix-func mix1 1) inverted-envelope)
			  (if (with-mix-tags)
			      (let ((trk (make-track)))
				(set! (mix-track mix0) trk)
				(set! (mix-track mix1) trk)
				(set! (mix-property 'pan-mix mix0) (list mix1 1))
				(set! (mix-property 'pan-mix mix1) (list mix0 0))))
			  mix0))))
		   (lambda ()
		     (set! (sync index) old-sync))))))))))


(define (mix->vct id)
  "(mix->vct id) returns mix's data in vct"
  (if (mix? id)
      (let* ((len (mix-frames id))
	     (v (make-vct len))
	     (reader (make-mix-sample-reader id)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (vct-set! v i (next-mix-sample reader)))
	(free-mix-sample-reader reader)
	v)
      (throw 'no-such-mix (list "mix->vct" id))))

(define (mix-maxamp id)
  (if (mix? id)
      (let* ((len (mix-frames id))
	     (peak 0.0)
	     (reader (make-mix-sample-reader id)))
	(set! peak (abs (next-mix-sample reader)))
	(do ((i 1 (1+ i)))
	    ((= i len))
	  (let ((val (abs (next-mix-sample reader))))
	    (if (> val peak)
		(set! peak val))))
	(free-mix-sample-reader reader)
	peak)
      (throw 'no-such-mix (list "mix-maxamp" id))))
	  

(define* (snap-mix-to-beat #:optional (at-anchor #f))
  "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute).  reset mix-release-hook to cancel"
  (add-hook! mix-release-hook
	     (lambda (id samps-moved)
	       (let* ((offset (if at-anchor (mix-anchor id) 0))
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
						     all-mix-properties)))
	     #f))

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


(define* (track->vct trk #:optional (chan 0))
  "(track->vct track (chan 0)) places track data in vct"
  (if (track? trk)
      (if (< chan (track-chans trk))
	  (vct-map! (make-vct (track-frames trk chan)) (make-track-sample-reader trk chan))
	  (throw 'no-such-channel (list "track->vct" chan)))
      (throw 'no-such-track (list "track->vct" trk))))

(define* (save-track trk filename #:optional (chan #t))
  "(save-track track filename (chan #t)) saves track data (as floats) in file filename"
  (if (track? trk)
      (let ((chans (track-chans trk)))
	(if (or (and (eq? chan #t)
		     (= chans 1))
		(and (integer? chan)
		     (< chan chans)))
	    (let* ((current-chan (if (eq? chan #t) 0 chan))
		   (v (track->vct trk current-chan))
		   (fd (open-sound-file filename 1 (srate) "written by save-track")))
	      (vct->sound-file fd v (vct-length v))
	      (close-sound-file fd (* 4 (vct-length v))))
	    (if (and (eq? chan #t)
		     (> chans 0))
		(let* ((fd (open-sound-file filename chans (srate) "written by save-track"))
		       (len (track-frames trk))
		       (pos (track-position trk))
		       (v (make-vct (* chans len))))
		  (do ((i 0 (1+ i)))
		      ((= i chans))
		    (let ((chan-len (track-frames trk i))
			  (chan-pos (- (track-position trk i) pos))
			  (reader (make-track-sample-reader trk i))) ; beg is next arg if it's needed
		      (do ((j 0 (1+ j)))
			  ((= j chan-len))
			(vct-set! v (+ i (* chans (+ chan-pos j))) (next-track-sample reader)))))
		  (vct->sound-file fd v (vct-length v))
		  (close-sound-file fd (* 4 (vct-length v))))
		(throw 'no-such-channel (list "save-track" chan)))))
      (throw 'no-such-track (list "save-track" trk))))
	  
(define (track-maxamp id chan)
  (if (track? id)
      (let* ((len (track-frames id chan))
	     (peak 0.0)
	     (reader (make-track-sample-reader id)))
	(set! peak (abs (next-track-sample reader)))
	(do ((i 1 (1+ i)))
	    ((= i len))
	  (let ((val (abs (next-track-sample reader))))
	    (if (> val peak)
		(set! peak val))))
	(free-track-sample-reader reader)
	peak)
      (throw 'no-such-track (list "track-maxamp" id))))
	  
(define (transpose-track trk semitones)
  "(transpose-track track semitones) transposes each mix in track  by semitones"
  (let ((mult (expt 2.0 (/ semitones 12.0))))
    (set! (track-speed trk) (* (track-speed trk) mult))))

(define (retempo-track trk tempo)
  "(retempo-track track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)"
  (if (and (not (= tempo 1.0)) (not (= tempo 0.0)))
      (if (some mix? (track trk))
	  (let ((track-beg (track-position trk))
		(new-tempo (/ 1.0 tempo))) ;make tempo>1.0 go faster
	    (if track-beg
		(as-one-edit 
		 (lambda ()
		   (for-each (lambda (a) 
			       (if (mix? a)
				   (set! (mix-position a) (+ track-beg 
							     (inexact->exact (floor (* new-tempo 
										(- (mix-position a) track-beg))))))))
			     (track trk))))))
	  (throw 'no-such-track (list "retempo-track" trk tempo)))))

;;; (retempo-track '(0 1) 2.0)

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
			   (let ((val (next-track-sample reader)))
			     (+ y (- (fir-filter flt val) val))))
			 beg (+ dur order) #f #f #f "filter-track"))))
      (throw 'no-such-track (list "filter-track" track-id))))


;;; --------------------------------------------------------------------------------

(define all-track-properties '())

(define track-properties
  (make-procedure-with-setter
   (lambda (id)
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
