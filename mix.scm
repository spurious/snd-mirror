;;; support for "tracks" in Snd and various mix-related utilities
;;;
;;; (pan-mix file frame envelope) mixes file into current sound starting at frame using envelope to pan (0: all chan 0, 1: all chan 1)
;;; (mix->vct id) return mix data in vct
;;; (snap-mix-to-beat (at-anchor)) forces dragged mix to end up on a beat
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
;;; settable:     track-amp, track-speed, track-position, track-color
;;; not settable: track-frames, track-end
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
;;;   (track-frames track) returns number of samples between track start and end
;;; 
;;; (retempo-track track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)
;;; (set! (track-color track) color) changes the associated mix colors to color
;;; (track-color track) returns track color
;;; (env-track track chan) env) set overall track amp env
;;;
;;; (sync-multichannel-mixes) causes multichannel mixes to be placed in a separate track,
;;;   and subsequent srate or position changes affect all channels in parallel (this makes
;;;   it easier to mix multichannel files into other multichannel files and keep the mixed
;;;   channels in sync)
;;;
;;; mix-property associates a property list with a mix
;;; mix-click-sets-amp sets up hook functions so that mix click zeros amps, then subsequent click resets to the before-zero value

(load-from-path "env.scm") ; multiply-envelope and window-envelope for env-track

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
  (if (null? (sounds))
      (throw 'no-such-sound (list "mix-sound" "no sound to mix into")))
  (if (not (file-exists? file))
      (throw 'no-such-file (list "mix-sound" file)))
  (let* ((snd (or (selected-sound) (car (sounds))))
	 (old-sync (sync snd))
	 (new-sync 1))
    (for-each (lambda (s) (if (>= (sync s) new-sync) (set! new-sync (1+ (sync s))))) (sounds))
    (set! (sync snd) new-sync)
    (let ((val (mix file start #f snd)))
      (set! (sync snd) old-sync)
      val)))

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



;;; -------- pan-mix --------

(define (invert-envelope env)
  (if (null? env) 
      '()
      (cons (car env) 
	    (cons (- 1.0 (cadr env)) 
		  (invert-envelope (cddr env))))))

(define (pan-mix-amp-changes id chan)
  (if (< chan 2)
      (let ((chan-prop (mix-property 'pan-mix id)))
	(if chan-prop
	    (let ((other-mix (car chan-prop))
		  (other-chan (cadr chan-prop)))
	      (if (= other-mix id)
		  (set! (mix-amp id (if (= chan 0) 1 0)) (- 1.0 (mix-amp id chan)))
		  (if (= chan (cadr (mix-property 'pan-mix other-mix)))
		      (set! (mix-amp other-mix other-chan) (- 1.0 (mix-amp id chan)))))))))
  #f)

(define (pan-mix-amp-env-changes id chan)
  (if (< chan 2)
      (let ((chan-prop (mix-property 'pan-mix id)))
	(if chan-prop
	    (let ((other-mix (car chan-prop))
		  (other-chan (cadr chan-prop)))
	      (if (= other-mix id)
		  (set! (mix-amp-env id (if (= chan 0) 1 0)) (invert-envelope (mix-amp-env id chan)))
		  (if (= chan (cadr (mix-property 'pan-mix other-mix)))
		      (set! (mix-amp-env other-mix other-chan) (invert-envelope (mix-amp-env id chan)))))))))
  #f)

(define (pan-mix-resampled id)
  (let ((chan-prop (mix-property 'pan-mix id)))
    (if chan-prop
	(multichannel-mix-resampled id))
    #f))
  
(define (pan-mix-moved id samps)
  (let ((chan-prop (mix-property 'pan-mix id)))
    (if chan-prop
	(multichannel-mix-moved id samps))
    #f))
  

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
      (if (or (> receiving-chans 1)
	      (> incoming-chans 1))
	  (begin
	    (if (not (hook-member pan-mix-moved mix-dragged-hook))
		(add-hook! mix-dragged-hook pan-mix-moved))
	    (if (not (hook-member pan-mix-amp-changes mix-amp-changed-hook))
		(add-hook! mix-amp-changed-hook pan-mix-amp-changes))
	    (if (not (hook-member pan-mix-amp-env-changes mix-amp-env-changed-hook))
		(add-hook! mix-amp-env-changed-hook pan-mix-amp-env-changes))
	    (if (not (hook-member pan-mix-resampled mix-speed-changed-hook))
		(add-hook! mix-speed-changed-hook pan-mix-resampled))))
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
			      (let ((trk (unused-track))) ; needed for position and speed changes
				(set! (mix-sync mix0) trk)
				(set! (mix-sync mix1) trk)
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
			      (let ((trk (unused-track)))
				(set! (mix-sync mix0) trk)
				(set! (mix-sync mix1) trk)
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
	  

(define* (snap-mix-to-beat #:optional (at-anchor #f))
  "(snap-mix-to-beat) forces a dragged mix to end up on a beat (see beats-per-minute).  reset mix-dragged-hook to cancel"
  (add-hook! mix-dragged-hook
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
	   
(define (next-mix-in-track id)
  "(next-mix-in-track trk) selects and returns the id of the next mix in the given track, or #f is there isn't one"
  (let ((cur-mix (or (selected-mix) -1))
	(next-is-it (or (not (number? (selected-mix)))
			(not (= id (mix-track (selected-mix)))))))
    (call-with-current-continuation
     (lambda (return)
       (tree-for-each 
	(lambda (mix-id)
	  (if (and next-is-it
		   (= (mix-track mix-id) id))
	      (begin
		(set! (selected-mix) mix-id)
		(return mix-id))
	      (if (= mix-id cur-mix)
		  (set! next-is-it #t))))
	(mixes))
       #f))))

(define (previous-mix-in-track id)
  "(previous-mix-in-track trk) selects and returns the id of the previous mix in the given track, or #f is there isn't one"
  (let ((cur-mix (or (selected-mix) -1))
	(next-is-it (or (not (number? (selected-mix)))
			(not (= id (mix-track (selected-mix)))))))
    (call-with-current-continuation
     (lambda (return)
       (tree-for-each-reversed
	(lambda (mix-id)
	  (if (and next-is-it
		   (= (mix-track mix-id) id))
	      (begin
		(set! (selected-mix) mix-id)
		(return mix-id))
	      (if (= mix-id cur-mix)
		  (set! next-is-it #t))))
	(mixes))
       #f))))

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

(define (track-frames track)
  "(track-frames track) returns number of samples between track start and end"
  (- (track-end track) (track-position track)))


(define (track->vct track)
  "(track->vct track) places track data in vct"
  (vct-map! 
   (make-vct 
    (track-frames track)) 
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

(define (transpose-track track semitones)
  "(transpose-track track semitones) transposes each mix in track  by semitones"
  (let ((mult (expt 2.0 (/ semitones 12.0))))
    (as-one-edit
     (lambda ()
       (for-each (lambda (a)
		   (if (mix? a)
		       (set! (mix-speed a) (* mult (mix-speed a)))))
		 track)))))


(define (retempo-track track tempo)
  "(retempo-track track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)"
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
	  (throw 'no-such-track (list "retempo-track" track tempo)))))

;;; (retempo-track '(0 1) 2.0)


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



;;; env-track applies an envelope to a track by enveloping the underlying mix input sounds

(define (env-track track chan env)
  "(env-track track chan env) sets overall track amplitude envelope"
  (let ((beg (track-position track))
	(len (track-frames track)))
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

; (env-track (track 1) 0 '(0 0 1 1))

(if (not (defined? 'hook-member))
    (define (hook-member value hook) 
      (member value (hook->list hook))))

(define (multichannel-mix-to-track mix-ids)
  (let ((new-track (unused-track)))
    (for-each 
     (lambda (n) 
       (set! (mix-track n) new-track)) 
     mix-ids)))

(define (multichannel-mix-moved id samps-moved)
  ;; id = mix that moved (via mouse), move all other mixes in its track by the same amount
  (let ((trk (mix-track id)))
    (if (> trk 0)
	(let ((track-mixes (track trk)))
	  (as-one-edit
	   (lambda ()
	     (for-each (lambda (a) 
			 (if (and (mix? a)
				  (not (= a id)))
			     (set! (mix-position a) (+ samps-moved (mix-position a)))))
		       track-mixes)))))
    #f))

(define (multichannel-mix-resampled id)
  (let ((trk (mix-track id)))
    (if (> trk 0)
	(let ((track-mixes (track trk))
	      (new-speed (mix-speed id)))
	  (set! (track-speed track-mixes) new-speed)
	  #t)
	#f)))

(define sync-multichannel-mixes
  (make-procedure-with-setter
   (lambda ()
     (if (not (hook-member multichannel-mix-to-track multichannel-mix-hook))
	 (add-hook! multichannel-mix-hook multichannel-mix-to-track))
     (if (not (hook-member multichannel-mix-moved mix-dragged-hook))
	 (add-hook! mix-dragged-hook multichannel-mix-moved))
     (if (not (hook-member multichannel-mix-resampled mix-speed-changed-hook))
	 (add-hook! mix-speed-changed-hook multichannel-mix-resampled)))
   (lambda (on)
     (if (and on 
	      (not (hook-member multichannel-mix-to-track multichannel-mix-hook)))
	 (add-hook! multichannel-mix-hook multichannel-mix-to-track)
	 (remove-hook! multichannel-mix-hook multichannel-mix-to-track))
     (if (and on
	      (not (hook-member multichannel-mix-moved mix-dragged-hook)))	      
	 (add-hook! mix-dragged-hook multichannel-mix-moved)
	 (remove-hook! mix-dragged-hook multichannel-mix-moved))
     (if (and on
	      (not (hook-member multichannel-mix-resampled mix-speed-changed-hook)))
	 (add-hook! mix-speed-changed-hook multichannel-mix-resampled)
	 (remove-hook! mix-speed-changed-hook multichannel-mix-resampled)))))




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

  
