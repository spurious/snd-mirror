;;; various mix related functions
;;;
;;; (mix->vct id) return mix data in vct
;;; (snap-mix-to-beat) forces dragged mix to end up on a beat
;;; (silence-all-mixes) sets all mix amps to 0.0
;;; (find-mix sample snd chn) returns the id of the mix at the given sample, or #f
;;; (save-mix mix filename) saves mix data in file filename
;;; (mix-maxamp id) maxamp of mix
;;;
;;; mix-property associates a property list with a mix
;;; mix-click-sets-amp sets up hook functions so that mix click zeros amps, then subsequent click resets to the before-zero value

(use-modules (ice-9 common-list))
(provide 'snd-mix.scm)


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


(define (mix-sound file start)
  "(mix-sound file start) mixes file (all chans) at start in the currently selected sound."
  (mix file start #t))


(define (silence-all-mixes)
  "(silence-all-mixes) sets all mix amps to 0"
  (as-one-edit
    (lambda ()
      (tree-for-each
        (lambda (id)
          (set! (mix-amp id) 0.0))
        (mixes)))))


(define* (find-mix sample :optional snd chn)
  "(find-mix sample :optional snd chn) returns the id of the mix at the given sample, or #f"
  (let ((mix-list (mixes (or snd (selected-sound) (car (sounds))) (or chn (selected-channel snd) 0))))
    (call-with-exit
     (lambda (found-it)
       (for-each
	(lambda (n)
	  (if (= (mix-position n) sample)
	      (found-it n)))
	mix-list)
       #f))))


(define (mix->vct id)
  "(mix->vct id) returns mix's data in vct"
  (if (mix? id)
      (let* ((len (mix-length id))
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
  (if (mix? id)
      (if (< (mix-length id) 1000000)
	  (let ((v (mix->vct id))
		(fd (mus-sound-open-output filename (srate) 1 #f #f "")))
	    (mus-sound-write fd 0 (1- (vct-length v)) 1 (vct->sound-data v))
	    (mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) (vct-length v))))
	  (let* ((buflen 10000)
		 (sd (make-sound-data 1 buflen))
		 (len (mix-length id))
		 (reader (make-mix-sample-reader id)))
	    (do ((buf 0 (+ buf buflen)))
		((>= buf len))
	      (do ((i 0 (1+ i)))
		  ((= i buflen))
		(sound-data-set! sd 0 i (read-mix-sample reader)))
	      (mus-sound-write fd 0 (1- buflen) 1 sd))
	    (free-sample-reader reader)
	    (mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) len))))
      (throw 'no-such-mix (list "save-mix" id))))


(define (mix-maxamp id)
  "(mix-maxamp id) returns the max amp in the given mix"
  (if (mix? id)
      (let* ((len (mix-length id))
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
	  

;;; -------- snap dragged mix(es) to the nearest beat

(define (snap-mix-1 id samps-moved)
  (let* ((samp (+ samps-moved (mix-position id)))
	 (snd (car (mix-home id)))
	 (chn (cadr (mix-home id)))
	 (bps (/ (beats-per-minute snd chn) 60.0))
	 (sr (srate snd))
	 (beat (floor (/ (* samp bps) sr)))
	 (lower (inexact->exact (floor (/ (* beat sr) bps))))
	 (higher (inexact->exact (floor (/ (* (1+ beat) sr) bps)))))
    (set! (mix-position id)
	  (if (< (- samp lower) (- higher samp))
	      (max 0 lower)
	      higher))
    #t))

(define (snap-mix-to-beat)
  "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute).  (remove-hook! mix-release-hook snap-mix-1) to cancel."
  (add-hook! mix-release-hook snap-mix-1 #t))


(define (snap-syncd-mixes-1 id samps-moved)
  (let* ((samp (+ samps-moved (mix-position id)))
	 (snd (car (mix-home id)))
	 (chn (cadr (mix-home id)))
	 (bps (/ (beats-per-minute snd chn) 60.0))
	 (sr (srate snd))
	 (beat (floor (/ (* samp bps) sr)))
	 (lower (inexact->exact (floor (/ (* beat sr) bps))))
	 (higher (inexact->exact (floor (/ (* (1+ beat) sr) bps))))
	 (new-position (if (< (- samp lower) (- higher samp))
			   (max 0 lower)
			   higher))
	 (true-samps-moved (- new-position (mix-position id))))
    (if (= (mix-sync id) 0)
	(set! (mix-position id) new-position)
	(move-mixes (syncd-mixes (mix-sync id)) true-samps-moved))
    #t))
 
(define (snap-syncd-mixes-to-beat)
  "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute). \
All mixes sync'd to it are also moved the same number of samples. (remove-hook! mix-release-hook snap-syncd-mixes-1) to cancel."
  (add-hook! mix-release-hook snap-syncd-mixes-1 #t))




;;; --------- mix-property

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


;;; -------- mix-click-sets-amp

(define (mix-click-sets-amp)
  (add-hook! mix-click-hook 
	     (lambda (n)
	       (let ((zeroed (mix-property :zero n)))
		 (if (not zeroed)
		     (begin
		       (set! (mix-property :amp n) (mix-amp n))
		       (set! (mix-amp n) 0.0)
		       (set! (mix-property :zero n) #t))
		     (begin
		       (set! (mix-amp n) (mix-property :amp n))
		       (set! (mix-property :zero n) #f)))
		 #t))))


;;; ---------- mix-click-info

(define (mix-click-info n)
  "(mix-click-info n) is a mix-click-hook function that describes a mix and its properties"
  (help-dialog "Mix Help"
	       (format #f "Mix ~A:~%  position: ~D = ~,3F secs~%  length: ~D (~,3F secs)
~%  in: ~A[~D]~%  scaler: ~A~%  speed: ~A~%  env: ~A~A"
		       (if (mix-name n)
			   (format #f "~S (~D)" (mix-name n) n)
			   (format #f "~D" n))
		       (mix-position n)
		       (exact->inexact (/ (mix-position n) (srate (car (mix-home n)))))
		       (mix-length n)
		       (exact->inexact (/ (mix-length n) (srate (car (mix-home n)))))
		       (short-file-name (car (mix-home n))) 
		       (cadr (mix-home n))
		       (mix-amp n)
		       (mix-speed n)
		       (mix-amp-env n)
		       (let ((props (mix-properties n)))
			 (if (and (list? props)
				  (not (null? props)))
			     (format #f "~%  properties: '~A" props)
			     ""))))
  #t)


;;; -------- mix-name->id

(define (mix-name->id name)
  "(mix-name->id name) returns the mix id associated with 'name'"
  (call-with-exit
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


;;; ---------------- backwards compatibilty

(define (delete-mix id) 
  "(delete-mix id) sets the mix's amp to 0.0"
  (set! (mix-amp id) 0.0))



;;; -------- mix lists (used to be called "tracks")
;;;
;;; to use these based on a mix-sync setting, use syncd-mixes below:
;;;   (scale-mixes (syncd-mixes 2) 2.0) scales all mixes whose mix-sync field is 2 by 2.0.

(define (scale-mixes mix-list scl)
  "(scale-mixes mix-list scl) scales the amplitude of each mix in 'mix-list' by 'scl'"
  (as-one-edit
   (lambda ()
     (for-each
      (lambda (m)
	(set! (mix-amp m) (* scl (mix-amp m))))
      mix-list))))


(define (silence-mixes mix-list)
  "(silence-mixes mix-list) sets the amplitude of each mix in 'mix-list' to 0.0"
  (scale-mixes mix-list 0.0))


(define (move-mixes mix-list samps)
  "(move-mixes mix-list samps) moves each mix in 'mix-list' by 'samps' samples"
  (as-one-edit
   (lambda ()
     (for-each
      (lambda (m)
	(set! (mix-position m) (max 0 (+ (mix-position m) samps))))
      mix-list))))


(define (src-mixes mix-list sr)
  "(src-mixes mix-list sr) multiplies the speed (resampling ratio) of each mix in 'mix-list' by 'sr'"
  (if (not (= sr 0.0))
      (as-one-edit
       (lambda ()
	 (for-each
	  (lambda (m)
	    (set! (mix-speed m) (* (mix-speed m) sr)))
	  mix-list)))))


(define (transpose-mixes mix-list semitones)
  "(transpose-mixes mix-list semitones) transposes each mix in mix-list by semitones"
  (if (not (= semitones 0))
      (src-mixes mix-list (expt 2.0 (/ semitones 12.0)))))


(define (color-mixes mix-list col)
  "(color-mixes mix-list color) sets the tag and waveform color of each mix in 'mix-list' to 'color'"
  (for-each
   (lambda (m)
     (set! (mix-color m) col))
   mix-list))


(define (set-mixes-tag-y mix-list new-y)
  "(set-mixes-tag-y mix-list new-y) sets the mix tag vertical position of each mix in 'mix-list' to 'new-y'.  The \
position is measured from the top of the graph, so higher tag-y values position the tag lower in the graph. For \
example, if you know the frequency of the mix sound, you can reflect that in the tag height with: \n\n\
\n\
   (set! (mix-tag-y mix-id) (inexact->exact (round (* 100 (- 1.0 (/ (log (/ freq 40.0)) (* (log 2.0) 7)))))))\n"

  (for-each
   (lambda (m)
     (set! (mix-tag-y m) new-y))
   mix-list))
  

(define (mixes-maxamp mix-list)
  "(mixes-maxamp mix-list) returns the maximum amplitude of the data in the mixes in 'mix-list'"
  (let ((mx 0.0))
    (for-each
     (lambda (m)
       (set! mx (max mx (mix-maxamp m))))
     mix-list)
    mx))


(define (scale-tempo mix-list tempo-scl)
  "(scale-tempo mix-list scl) changes the rate at which the mixes in 'mix-list' occur to reflect \
the tempo scaler 'scl'.  If 'scl' is 2.0, for example, the mixes are re-positioned so that they \
happen twice as slowly (the data is not resampled -- each mix is untouched except that its begin time \
may change)"

  (let* ((first-beg (mix-position (car mix-list)))
	 (last-beg first-beg))
    (for-each
     (lambda (m)
       (let ((pos (mix-position m)))
	 (set! first-beg (min first-beg pos))
	 (set! last-beg (max last-beg pos))))
     (cdr mix-list))
    (as-one-edit
     (lambda ()
       (for-each
	(lambda (m)
	  (let ((diff (inexact->exact (round (* tempo-scl (- (mix-position m) first-beg))))))
	    (if (not (= diff 0))
		(set! (mix-position m) (+ first-beg diff)))))
	mix-list)))))

;;; reverse-mix-list is (scale-tempo mix-list -1.0)


(define (mixes-length mix-list)
  "(mixes-length mix-list) returns the number of samples between the start of the earliest mix and the \
last end of the mixes in 'mix-list'"
  (1+ (- (apply max (map (lambda (m) 
			   (+ (mix-position m) (mix-length m))) 
			 mix-list))
	 (apply min (map mix-position mix-list)))))

  
(define (save-mixes mix-list filename)
  "(save-mixes mix-list filename) saves the data of the mixes in 'mix-list' in 'filename'"
  (let* ((len (mixes-length mix-list))
	 (beg (apply min (map mix-position mix-list)))
	 (data (make-vct len)))
    (for-each
     (lambda (m)
       (vct-add! data (mix->vct m) (- (mix-position m) beg)))
     mix-list)
    (let ((fd (mus-sound-open-output filename (srate) 1 #f #f "")))
      (mus-sound-write fd 0 (1- len) 1 (vct->sound-data data))
      (mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) (vct-length data))))))


(if (not (provided? 'snd-env.scm)) (load-from-path "env.scm"))

(define (env-mixes mix-list overall-amp-env)
  "(env-mixes mix-list amp-env) applies 'amp-env' as a global amplitude envelope to the mixes in 'mix-list'"
  (let* ((mix-begs (map mix-position mix-list))
	 (mix-ends (map (lambda (m) (+ (mix-position m) (mix-length m))) mix-list))
	 (beg (apply min mix-begs))
	 (end (apply max mix-ends))
	 (first-x (car overall-amp-env))
	 (last-x (envelope-last-x overall-amp-env))
	 (x-scale (/ (- last-x first-x) (1+ (- end beg)))))
    (as-one-edit
     (lambda ()
       (for-each 
	(lambda (m)
	  (let* ((beg-x (+ first-x (* x-scale (- (mix-position m) beg))))
		 (end-x (+ first-x (* x-scale (- (+ (mix-position m) (mix-length m)) beg))))
		 (wenv (window-envelope beg-x end-x overall-amp-env)))
	    (if (null? (mix-amp-env m))
		(set! (mix-amp-env m) wenv)
		(set! (mix-amp-env m) (multiply-envelopes (mix-amp-env m) wenv)))))
	mix-list)))))
  

(define* (sync-all-mixes :optional (new-sync 1))
  ;; a replacement for set-all-tracks in snd-8
  "(sync-all-mixes :optional (new-sync 1)) sets the mix-sync field of every active mix to new-sync"
  (for-each
   (lambda (snd-m)
     (for-each
      (lambda (chn-m)
	(for-each
	 (lambda (m)
	   (set! (mix-sync m) new-sync))
	 chn-m))
      snd-m))
   (mixes)))


(define (syncd-mixes sync)
  "(syncd-mixes sync) returns a list (possibly null) of all mixes whose mix-sync field is set to 'sync'"
  (if (<= sync 0)
      (list)
      (let ((mix-list '()))
	(for-each
	 (lambda (snd-m)
	   (for-each
	    (lambda (chn-m)
	      (for-each
	       (lambda (m)
		 (if (= (mix-sync m) sync)
		     (set! mix-list (cons m mix-list))))
	       chn-m))
	    snd-m))
	 (mixes))
	mix-list)))

	
(define (play-mixes mix-list)
  "(play-mixes mix-list) plays the mixes in 'mix-list'"
  (let* ((sorted-mixes (sort mix-list (lambda (a b) (< (mix-position a) (mix-position b)))))
	 (now (mix-position (car sorted-mixes))))
    (play (lambda ()
	    (while (and (not (null? sorted-mixes))
			(= now (mix-position (car sorted-mixes))))
	      (play-mix (car sorted-mixes))
	      (set! sorted-mixes (cdr sorted-mixes)))
	    (set! now (1+ now))
	    (if (null? sorted-mixes)
		#f
		0.0)))))


;;; -------- pan-mix --------

(define* (pan-mix name beg pan :optional snd (chn 0) auto-delete)

  "(pan-mix file start pan-env :optional snd (chn 0) (auto-delete #f)) mixes 'file' into the sound 'snd'
starting at 'start' (in samples) using 'pan-env' to decide how to split the sound between the output channels (0: all chan 0, 1: all chan 1).
So, (pan-mix \"oboe.snd\" 0 '(0 0 1 1)) goes from all chan 0 to all chan 1.
'auto-delete' determines whether the in-coming file should be treated as a temporary file and deleted when the mix
is no longer accessible.  pan-mix returns a list of the id's of the mixes performing the
panning operation."

  (let* ((index (or snd (selected-sound) (and (sounds) (car (sounds)))))
	 (deletion-choice (if auto-delete 3 0)) ; multichannel deletion case
	 (end-deletion-choice (if (= deletion-choice 3) 4 0)))
    (if (not (sound? index))
	(throw 'no-such-sound (list "pan-mix" snd)))
    (if (not (file-exists? name))
	(throw 'no-such-file (list "pan-mix" name)))
    
    (as-one-edit
     (lambda ()

       (define (invert-envelope e)
	 (if (null? e)
	     '()
	     (append (list (car e) (- 1.0 (cadr e)))
		     (invert-envelope (cddr e)))))

       (let ((incoming-chans (mus-sound-chans name))
	     (receiving-chans (chans index)))

	 (if (= incoming-chans 1)

	     ;; mono input

	     (if (= receiving-chans 1)

		 ;; mono to mono = just scale or envelope
		 (let ((id (mix name beg 0 index 0 (with-mix-tags) auto-delete))) ; file start in-chan snd chn ...
		   (if (mix? id)
		       (set! (mix-amp-env id) (invert-envelope pan)))
		   (list id))

		 ;; mono to stereo
		 (let ((id0 (mix name beg 0 index 0 (with-mix-tags) deletion-choice))
		       (id1 (mix name beg 0 index 1 (with-mix-tags) end-deletion-choice)))
		   (if (and (mix? id0)
			    (mix? id1))
		       (begin
			 (set! (mix-amp-env id0) (invert-envelope pan))
			 (set! (mix-amp-env id1) pan)))
		   (list id0 id1)))

	     ;; stero input
	     
	     (if (= receiving-chans 1)

		 ;; stereo -> mono => scale or envelope both input chans into the output
		 (let ((id0 (mix name beg 0 index 0 (with-mix-tags) deletion-choice))
		       (id1 (mix name beg 1 index 0 (with-mix-tags) end-deletion-choice)))
		   (if (and (mix? id0)
			    (mix? id1))
		       (begin
			 (set! (mix-amp-env id0) (invert-envelope pan))
			 (set! (mix-amp-env id1) (invert-envelope pan))))
		   (list id0 id1))

		 ;; stereo -> stereo => incoming chans are treated equally, each panned into outputs
		 (let ((id00 (mix name beg 0 index 0 (with-mix-tags) deletion-choice))
		       (id01 (mix name beg 0 index 1 (with-mix-tags) deletion-choice))
		       (id10 (mix name beg 1 index 0 (with-mix-tags) deletion-choice))
		       (id11 (mix name beg 1 index 1 (with-mix-tags) end-deletion-choice)))
		   (if (and (mix? id00)
			    (mix? id01)
			    (mix? id10)
			    (mix? id11))
		       (begin
			 (set! (mix-amp-env id00) (invert-envelope pan))
			 (set! (mix-amp-env id10) (invert-envelope pan))
			 (set! (mix-amp-env id01) pan)
			 (set! (mix-amp-env id11) pan)))
		   (list id00 id01 id10 id11)))))))))


(define* (pan-mix-selection beg pan :optional snd chn)

  "(pan-mix-selection start pan-env :optional snd chn) mixes the current selection  into the sound 'snd'
starting at 'start' (in samples) using 'pan-env' to pan (0: all chan 0, 1: all chan 1)."

  (if (not (selection?))
      (throw 'no-active-selection (list "pan-mix-selection"))
      (pan-mix (save-selection (snd-tempnam)) beg pan snd chn #t)))


(define* (pan-mix-region reg beg pan :optional snd chn)

  "(pan-mix-region reg start pan-env :optional snd chn) mixes the given region into the sound 'snd' 
starting at 'start' (in samples) using 'pan-env' to pan (0: all chan 0, 1: all chan 1)."

  (if (not (region? reg))
      (throw 'no-such-region (list "pan-mix-region" reg))
      (pan-mix (save-region reg (snd-tempnam)) beg pan snd chn #t)))


(define* (pan-mix-vct v beg pan :optional snd chn)

  "(pan-mix-vct v start pan-env :optional snd chn) mixes the vct data into the sound 'snd' 
starting at 'start' (in samples) using 'pan-env' to pan (0: all chan 0, 1: all chan 1)."

  (let* ((temp-file (snd-tempnam))
	 (fd (mus-sound-open-output temp-file (srate snd) 1 #f #f "")))
    (mus-sound-write fd 0 (1- (vct-length v)) 1 (vct->sound-data v))
    (mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) (vct-length v)))
    (pan-mix temp-file beg pan snd chn #t)))



;;; -------- delay-channel-mixes

(define* (delay-channel-mixes beg dur :optional snd chn)
  "(delay-channel-mixes beg dur :optional snd chn) adds dur (which can be negative) to the \
begin time of each mix that starts after beg in the given channel"
  (for-each
   (lambda (m)
     (if (>= (mix-position m) beg)
	 (set! (mix-position m) (max 0 (+ (mix-position m) dur)))))
   (mixes (or snd 
	      (selected-sound) 
	      (car (sounds))) 
	  (or chn 0))))


