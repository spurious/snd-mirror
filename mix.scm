;;; support for "tracks" in Snd and various mix-related utilities
;;;
;;;   HISTORY: changed 20-Jan-02 to use generalized set, etc
;;;
;;; mixes:
;;;
;;; (mix-name->id name) given name of mix return id
;;; (pan-mix file frame envelope) mixes file into current (stereo) sound starting at frame using envelope to pan (0: all chan 0, 1: all chan 1)
;;; (mix->vct id) return mix data in vct
;;; (snap-mix-to-beat) forces dragged mix to end up on a beat
;;; (delete-mix id) removes mix (can be undone)
;;; (delete-all-mixes) removes all mixes
;;;
;;;
;;; tracks (a "track" is a list of mix id numbers):
;;;
;;; (make-track trk mix-list) puts each mix (referenced by its id number) in mix-list into track trk
;;; (track trk) returns the list of mixes (id numbers) currently in track trk
;;; (track->vct track) place track data in vct
;;; (save-track track filename) save track data in file
;;; (filter-track track coeffs) filter track data
;;; (reverse-track track) reverses the mix order
;;; (delete-track track) deletes all mixes associated with track (sets amps to 0)
;;; (delete-all-tracks) removes all mixes that have an associated track (sets all amps to 0)
;;; (set-all-tracks new-id) places all mixes in track new-id
;;;
;;; settable:     track-amp, track-speed, track-position, track-tempo, track-color
;;; not settable: track-length, track-end
;;; 
;;; mix-panel hooks: mix-panel-applies-to-track
;;;
;;; (set! (track-amp track) new-amp) sets the amp of each mix in track to new-amp
;;;   (incf-track-amp track change) increments the amp of each mix in track by change
;;;
;;; (set! (track-speed track) new-speed) sets the speed (srate) of each mix in track to new-speed
;;;   (transpose-track track semitones) transposes each mix in track  by semitones
;;; the mix-speed-changed-hook can be set to respeed-track to have the entire track follow the mix panel's speed slider
;;;
;;; (track-position track) returns the begin time of track (the minimum mix begin sample associated with track)
;;;   (set! (track-position track) new-beg) moves all mixes in track so that the track starts at new-beg
;;;   (track-end track) returns endpoint (maximum frame in mixes) of track
;;;   (track-length track) returns number of samples between track start and end
;;; the mix-position-changed-hook can be set to reposition-track to have the entire track follow as we drag a mix
;;; 
;;; (set! (track-tempo track) tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)
;;; (set! (track-color track) color) changes the associated mix colors to color
;;; (track-color track) returns track color
;;; (set! (track-amp-env track chan) env) set overall track amp env
;;;
;;; (mix-panel-applies-to-track) causes multichannel mixes to be placed in a separate track,
;;;   and subsequent srate or position changes affect all channels in parallel (this makes
;;;   it easier to mix multichannel files into other multichannel files and keep the mixed
;;;   channels in sync)

(load-from-path "env.scm") ; multiply-envelope and window-envelope for track-amp-env

(define (tree-for-each func tree)
  "(tree-for-each func tree) applies func to every leaf of 'tree'"
  (cond ((null? tree) '())
	((not (pair? tree)) (func tree))
	(else (tree-for-each func (car tree))
	      (tree-for-each func (cdr tree)))))



;;; ---------------- mixes ----------------

(define (delete-mix id)
  "(delete-mix id) removes mix (sets amp to 0 -- can be undone)"
  (if (mix? id)
      (as-one-edit
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i (mix-chans id)))
	   (set! (mix-amp id i) 0.0))
	 (set! (mix-locked id) #t)))
      (throw 'no-such-mix (list "delete-mix" id))))

(define (delete-all-mixes)
  "(delete-all-mixes) removes all mixes (sets all amps to 0)"
  (as-one-edit
    (lambda ()
      (tree-for-each
        (lambda (id)
          (delete-mix id))
        (mixes)))))


(define (mix-name->id name)
  "(mix-name->id name) -> id of named mix"
  ;; (intended for regex-style track creation)
  (call-with-current-continuation
   (lambda (return-name)
     (tree-for-each
      (lambda (n)
	(if (string=? name (mix-name n))
	    (return-name n)))
      (mixes))
     (throw 'no-such-mix (list "mix-name->id" name)))))


(define (pan-mix name start envelope)

  "(pan-mix file start envelope) mixes file into current (stereo) sound \
starting at start (a frame number) using envelope to pan (0: all chan 0, 1: all chan 1).\n\
(pan-mix \"oboe.snd\" .1 '(0 0 1 1)) goes from all chan 0 to all chan 1"

  (let* ((index (or (selected-sound) (car (sounds))))
	 (beg (inexact->exact (* start (srate index)))))
    (if (= (chans index) 2)
	(letrec ((invert-envelope (lambda (env)
				    (cond ((null? env) '())
					  ((cons (car env) 
						 (cons (- 1.0 (cadr env)) 
						       (invert-envelope (cddr env))))))))
		 (mix-0 (mix name beg 0 index 0))
		 (mix-1 (mix name beg 0 index 1)))
	  (as-one-edit
	   (lambda ()
	     (set! (mix-amp-env mix-0 0) (invert-envelope envelope))
	     (set! (mix-amp-env mix-1 0) envelope))))
	(throw 'wrong-number-of-channels (list "pan-mix" name start envelope)))))


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
	  

(define (snap-mix-to-beat)
  "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute).  reset mix-position-changed-hook to cancel"
  (add-hook! mix-position-changed-hook
	     (lambda (id samps-moved)
	       (let* ((samp (+ samps-moved (mix-position id)))
		      (snd (car (mix-home id)))
		      (chn (cadr (mix-home id)))
		      (bps (/ (beats-per-minute snd chn) 60.0))
		      (sr (srate snd))
		      (beat (floor (/ (* samp bps) sr)))
		      (lower (inexact->exact (/ (* beat sr) bps)))
		      (higher (inexact->exact (/ (* (1+ beat) sr) bps))))
		 (set! (mix-position id)
		       (if (< (- samp lower) (- higher samp))
			   lower
			   higher))
		 #t))))



;;; ---------------- tracks ----------------
;;; a track is a list of mix ids

(define (unused-track)
  "(unused-track) returns a track number that is not currently in use"
  (let ((ntrack 0))
    (tree-for-each (lambda (n)
		     (set! ntrack (max ntrack (mix-track n))))
		   (mixes))
    (+ ntrack 1)))

(define (set-all-tracks new-num)
  "(set-all-tracks new-id) places all mixes in track new-id"
  (tree-for-each 
    (lambda (n)
      (set! (mix-track n) new-num))
    (mixes)))



(define (make-track id mixes)
  "(make-track trk mix-list) puts each mix (referenced by its id number) in mix-list into track trk. (make-track 2 '(2 4 1))"
  ;; loop through mixes setting mix-track to id, return mixes with #f where not mix?
  (map (lambda (a)
	 (if (mix? a)
	     (begin
	       (set! (mix-track a) id)
	       a)
	     #f))
       mixes))

(define (track id)
  "(track trk) returns the list of mixes (mix id numbers) currently in track trk"
  (let ((trk '()))
    (tree-for-each (lambda (n)
		  (if (= id (mix-track n))
		      (set! trk (cons n trk))))
		(mixes))
    trk))
	   

(define (delete-track track)
  "(delete-track track) deletes all mixes associated with track (sets amps to 0.0)"
  (as-one-edit
    (lambda ()
      (for-each 
        (lambda (a) 
          (delete-mix a))
        track))))

(define (delete-all-tracks)
  "(delete-all-tracks) removes all mixes that have an associated track (sets all amps to 0)"
  (as-one-edit
    (lambda ()
      (tree-for-each
        (lambda (id)
	  (if (not (= (mix-track id) 0))
	      (delete-mix id)))
        (mixes)))))


;;; filter-track 

(define (filter-track track fir-filter-coeffs)
  "(filter-track track coeffs) filters track data using FIR filter coeffs by appling \
the filter to the underlying mixes: (filter-track (track 1) '(.1 .2 .3 .3 .2 .1))"
  (if (some mix? track)
      (let ((order (length fir-filter-coeffs)))
	(as-one-edit
	 (lambda ()
	   (for-each (lambda (a)
		       (if (mix? a)
			   (let ((chans (mix-chans a)))
			     (do ((chan 0 (1+ chan)))
				 ((= chan chans))
			       (let* ((flt (make-fir-filter order (list->vct fir-filter-coeffs)))
				      (newlen(+ order (mix-frames a)))
				      (samps (make-vct newlen))
				      (reader (make-sample-reader 0 (list a) chan)))
				 (vct-map! samps (lambda ()
						   (fir-filter flt (next-sample reader))))
				 (free-sample-reader reader)
				 (vct->samples 0 newlen samps (list a) chan))))))
		     track))))
      (throw 'no-such-track (list "filter-track" track))))


;;; track-position

(define track-position
  (make-procedure-with-setter
   (lambda (track)
     "(track-position track) returns the begin time of track"
     (letrec ((track-pos
	       (lambda (trk)
		 (if (null? trk)
		     '()
		     (if (mix? (car trk))
			 (cons (mix-position (car trk)) (track-pos (cdr trk)))
			 (track-pos (cdr trk)))))))
       (let ((pos (track-pos track)))
	 (if (null? pos)
	     (throw 'no-such-track (list "track-position" track))
	     (apply min pos)))))
   (lambda (track new-position)
     "(set! (track-position track) new-position) moves all mixes in track so that the track starts at new-position"
     (let* ((track-beg (track-position track))
	    (change (- new-position track-beg)))
       (as-one-edit 
	(lambda ()
	  (for-each (lambda (a) 
		      (if (mix? a)
			  (set! (mix-position a) (+ change (mix-position a)))))
		    track)))))))

(define (set-track-position track pos) (set! (track-position track) pos)) ; backwards compatibility

(define (reverse-track track)
  "(reverse-track) reverses the order of its mixes (it changes various mix begin times)"
  (if (some mix? track)
      (let* ((ids-in-order (sort track
				 (lambda (a b)
				   (> (mix-position a)
				      (mix-position b))))))
	(as-one-edit
	 (lambda ()
	   (for-each (lambda (id pos)
		       (set! (mix-position id) pos))
		     ids-in-order
		     (reverse (map mix-position ids-in-order))))))
      (throw 'no-such-track (list "reverse-track" track))))


(define (track-end track)
  "(track-end track) returns endpoint (maximum frame in mixes) of track"
  (letrec ((track-pos
	    (lambda (trk)
	      (if (null? trk)
		  '()
		  (if (mix? (car trk))
		      (cons (+ (mix-position (car trk)) 
			       (mix-frames (car trk))) 
			    (track-pos (cdr trk)))
		      (track-pos (cdr trk)))))))
    (let ((pos (track-pos track)))
      (if (null? pos)
	  (throw 'no-such-track (list "track-end" track))
	  (apply max pos)))))

(define (track-length track)
  "(track-length track) returns number of samples between track start and end"
  (- (track-end track) (track-position track)))


(define (track->vct track)
  "(track->vct track) places track data in vct"
  (vct-map! 
   (make-vct 
    (track-length track)) 
   (make-track-sample-reader 
    (mix-track (car track)))))

(define (save-track track filename)
  "(save-track track filename) saves track data (as floats) in file filename"
  (let ((v (track->vct track))
	(fd (open-sound-file filename 1 (srate) ""))) ; chans?
    (vct->sound-file fd v (vct-length v))
    (close-sound-file fd (* 4 (vct-length v)))))



;;; track amplitude 

(define track-amp
  (make-procedure-with-setter
   (lambda (track)
     "(track-amp track) returns the amp associated with track"
     (if (some mix? track)
	 (let ((maxamp 0.0))
	   (for-each
	    (lambda (id)
	      (if (mix? id)
		  (do ((i 0 (1+ i)))
		      ((= i (mix-chans id)))
		    (set! maxamp (max maxamp (mix-amp id i))))))
	    track)
	   maxamp)
	 (throw 'no-such-track (list "track-amp" track))))
   (lambda (track new-amp)
     (if (some mix? track)
	 (as-one-edit
	  (lambda ()
	    (for-each (lambda (a) 
			(if (mix? a)
			    (do ((i 0 (1+ i)))
				((= i (mix-chans a)))
			      (set! (mix-amp a i) new-amp)))) ; is this the "right thing" -- maybe leave 0 amps 0 if chans>1?
		      track)))
	 (throw 'no-such-track (list "set! track-amp" track new-amp))))))

(define (set-track-amp track new-amp) (set! (track-amp track) new-amp)) ; backwards compatibility

(define (incf-track-amp track change)
  "(incf-track-amp track change) increments the amp of each mix in track by change"
  (if (not (= change 0.0))
      (if (some mix? track)
	  (as-one-edit
	   (lambda ()
	     (for-each (lambda (a) 
			 (if (mix? a)
			     (do ((i 0 (1+ i)))
				 ((= i (mix-chans a)))
			       (set! (mix-amp a i) (max 0.0 (+ (mix-amp a i) change))))))
		       track)))
	  (throw 'no-such-track (list "incf-track-amp" track change)))))


;;; track speed (srate)

(define track-speed
  (make-procedure-with-setter
   (lambda (track)
     "(track-speed track) returns the speed associated with track (presumably the first mix's speed)"
     (if (some mix? track)
	 (call-with-current-continuation
	  (lambda (return)
	    (for-each
	     (lambda (id)
	       (if (mix? id)
		   (return (mix-speed id))))
	     track)))
	 (throw 'no-such-track (list "track-speed" track))))
   (lambda (track new-speed)
     (if (not (= new-speed 0.0))
	 (if (some mix? track)
	     (as-one-edit
	      (lambda ()
		(for-each (lambda (a) 
			    (if (mix? a)
				(set! (mix-speed a) new-speed)))
			  track)))
	     (throw 'no-such-track (list "set! track-speed" track new-speed)))))))

(define (set-track-speed track new-speed) (set! (track-speed track) new-speed)) ; backwards...

(define (transpose-track track semitones)
  "(transpose-track track semitones) transposes each mix in track  by semitones"
  (let ((mult (expt 2.0 (/ semitones 12.0))))
    (as-one-edit
     (lambda ()
       (for-each (lambda (a)
		   (if (mix? a)
		       (set! (mix-speed a) (* mult (mix-speed a)))))
		 track)))))


;;; track tempo -- what is track-tempo?

(define (set-track-tempo track tempo)
  "(set-track-tempo track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)"
  (if (and (not (= tempo 1.0)) (not (= tempo 0.0)))
      (if (some mix? track)
	  (let ((track-beg (track-position track))
		(new-tempo (/ 1.0 tempo))) ;make tempo>1.0 go faster
	    (if track-beg
		(as-one-edit 
		 (lambda ()
		   (for-each (lambda (a) 
			       (if (mix? a)
				   (set! (mix-position a) (+ track-beg 
							     (inexact->exact (* new-tempo 
										(- (mix-position a) track-beg)))))))
			     track)))))
	  (throw 'no-such-track (list "set-track-tempo" track tempo)))))

;;; (set-track-tempo '(0 1) 2.0)


;;; track color 

(define track-color
  (make-procedure-with-setter
   (lambda (track)
     "(track-color track) returns track color"
     (if (null? track)
	 (mix-color)
	 (some (lambda (id) (and (mix? id) (mix-color id))) track)))
   (lambda (track new-color)
     "(set! (track-color track) color) changes the associated mix colors to color"
     (if (some mix? track)
	 (for-each (lambda (a) 
		     (if (mix? a)
			 (set! (mix-color a) new-color)))
		   track)
	 (throw 'no-such-track (list "set! track-color" track new-color))))))

(define (set-track-color track new-color) (set! (track-color track) new-color))

;;; (set-track-color '(0 1) (make-color 0 0 1))




;;; track-env applies an envelope to a track by enveloping the underlying mix input sounds

(define (set-track-amp-env track chan env)
  "(set-track-amp-env track chan env) sets overall track amplitude envelope"
  (let ((beg (track-position track))
	(len (track-length track)))
    (as-one-edit
     (lambda ()
       (for-each 
	(lambda (a) 
	  (if (and (mix? a) (< chan (mix-chans a)))
	      (set! (mix-amp-env a chan) 
		    (multiply-envelopes 
		     (window-envelope (/ (- (mix-position a) beg) len)
				      (/ (- (+ (mix-position a) (mix-frames a)) beg) len)
				      env)
		     (mix-amp-env a chan)))))
	track)))))

; (set-track-amp-env (track 1) 0 '(0 0 1 1))


						  
(define (mix-panel-applies-to-track)
  ;; these functions use the mix related hooks to affect the entire associated track

  (add-hook! mix-position-changed-hook 
	     (lambda (id samps-moved)
	       ;; id = mix that moved (via mouse), move all other mixes in its track by the same amount
	       (let ((trk (mix-track id)))
		 (if (> trk 0)
		     (let ((track-mixes (track trk)))
		       (as-one-edit
			(lambda ()
			  (for-each (lambda (a) 
				      (if (mix? a)
					  (set! (mix-position a) (+ samps-moved (mix-position a)))))
				    track-mixes)))
		       #t)
		     #f))))

  (add-hook! mix-speed-changed-hook
	     (lambda (id)
	       (let ((trk (mix-track id)))
		 (if (> trk 0)
		     (let ((track-mixes (track trk))
			   (new-speed (mix-speed id)))
		       (set-track-speed track-mixes new-speed)
		       #t)
		     #f))))

  (add-hook! multichannel-mix-hook
	     (lambda (mix-ids)
	       (let ((new-track (unused-track)))
		 (for-each 
		  (lambda (n) 
		    (set-mix-track n new-track)) 
		  mix-ids)))))





