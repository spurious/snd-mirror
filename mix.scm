;;; support for "tracks" in Snd and various mix-related utilities
;;;
;;; mixes:
;;; (mix-name->id name) given name of mix return id
;;; (pan-mix file frame envelope) mixes file into current (stereo) sound starting at frame using envelope to pan (0: all chan 0, 1: all chan 1)
;;; (mix->vct id) return current state of mix in vct
;;;
;;; tracks:
;;; (make-track id mix-list) puts each mix (referenced by its id number) in mix-list into track id
;;; (track id) returns the list of mixes (id numbers) currently in track id
;;;
;;;   in the following "track" is a list of mixes (as returned by the track function above)
;;; (set-track-amp track new-amp) sets the amp of each mix in track to new-amp
;;;   (incf-track-amp track change) increments the amp of each mix in track by change
;;;
;;; (set-track-speed track new-speed) sets the speed (srate) of each mix in track to new-speed
;;;   (transpose-track track semitones) transposes each mix in track  by semitones
;;; the mix-speed-changed-hook can be set to respeed-track to have the entire track follow a mix console's speed slider
;;;
;;; (track-position track) returns the begin time of track (the minimum mix begin sample associated with track)
;;;   (set-track-position track new-beg) moves all mixes in track so that the track starts at new-beg
;;;   (track-end track) returns endpoint (maximum frame in mixes) of track
;;;   (track-length track) returns number of samples between track start and end
;;; the mix-position-changed-hook can be set to reposition-track to have the entire track follow as we drag a mix console
;;; 
;;; (set-track-tempo track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)
;;; (set-track-color track color) changes the associated mix console colors to color
;;; (track-color track) returns track color
;;; (track->vct track) place track data in vct
;;; (save-track track filename) save track data in file
;;; (set-track-amp-env track chan env) set overall track amp env
;;; (filter-track track coeffs) filter track data


(load "env.scm")


;;; find mix id given mix name (for regex-style track creation)

(define (tree-apply func tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (func tree))
	(else (tree-apply func (car tree))
	      (tree-apply func (cdr tree)))))

(define mix-name->id
  (lambda (name)
    "(mix-name->id name) -> id of named mix"
    (call-with-current-continuation
     (lambda (return-name)
       (tree-apply
	(lambda (n)
	  (if (string=? name (mix-name n))
	      (return-name n)))
	(mixes))
       (throw 'no-such-mix (list "mix-name->id" name))))))

;;; pan mix (if input is not mono, only chan 0 is mixed)

(define pan-mix 
  (lambda (name start envelope)
    "(pan-mix file start envelope) mixes file into current (stereo) sound\n\
    starting at start (frame) using envelope to pan (0: all chan 0, 1: all chan 1)"
    (let* ((index (selected-sound))
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
	       (set-mix-amp-env mix-0 0 (invert-envelope envelope))
	       (set-mix-amp-env mix-1 0 envelope))))
	  (snd-print "pan-mix output should be stereo")))))

;;; (pan-mix "oboe.snd" .1 '(0 0 1 1)) goes from all chan 0 to all chan 1


(define mix->vct
  (lambda (id)
    "(mix->vct id) returns current state of mix's data in vct"
    (if (mix? id)
	(let* ((len (mix-length id))
	       (v (make-vct len))
	       (reader (make-mix-sample-reader id)))
	  (do ((i 0 (1+ i)))
	      ((= i len))
	    (vct-set! v i (next-mix-sample reader)))
	  (free-mix-sample-reader reader)
	  v)
	#f)))  ;throw 'no-such-mix?
	  


;;; a track is a list of mix ids
(define make-track 
  (lambda (id mixes)
    "(make-track id mix-list) puts each mix (referenced by its id number) in mix-list into track id"
    ;; loop through mixes setting mix-track to id, return mixes with #f where not mix?
    (map (lambda (a)
	   (if (mix? a)
	       (begin
		 (set-mix-track a id)
		 a)
	       #f))
	 mixes)))

;;; (make-track 2 '(3 12 5)) -> '(3 12 5)

(define track
  (lambda (id)
    "(track id) returns the list of mixes (id numbers) currently in track id"
    (let ((trk '()))
      (tree-apply (lambda (n)
		    (if (= id (mix-track n))
			(set! trk (cons n trk))))
		  (mixes))
      trk)))
	   
;;; (track 1) returns a list of mixes in track 1


;;; track amplitude 

(define incf-track-amp 
  (lambda (track change)
    "(incf-track-amp track change) increments the amp of each mix in track by change"
    (if (not (= change 0.0))
	(as-one-edit
	 (lambda ()
	   (map (lambda (a) 
		  (if (mix? a)
		      (do ((i 0 (1+ i)))
			  ((= i (mix-chans a)))
			(set-mix-amp a i (max 0.0 (+ (mix-amp a i) change))))))
		track))))))

(define set-track-amp 
  (lambda (track new-amp)
    "(set-track-amp track new-amp) sets the amp of each mix in track to new-amp"
    (as-one-edit
     (lambda ()
       (map (lambda (a) 
	      (if (mix? a)
		  (do ((i 0 (1+ i)))
		      ((= i (mix-chans a)))
		    (set-mix-amp a i new-amp))))
	    track)))))

;;; (set-track-amp '(0 1) .5)

;;; track speed (srate)

(define set-track-speed
  (lambda (track new-speed)
    "(set-track-speed track new-speed) sets the speed (srate) of each mix in track to new-speed"
    (if (not (= new-speed 0.0))
	(as-one-edit
	 (lambda ()
	   (map (lambda (a) 
		  (if (mix? a)
		      (set-mix-speed a new-speed)))
		track))))))

(define transpose-track
  (lambda (track semitones)
    "(transpose-track track semitones) transposes each mix in track  by semitones"
    (let ((mult (expt 2.0 (/ semitones 12.0))))
      (as-one-edit
       (lambda ()
	 (map (lambda (a)
		(if (mix? a)
		    (set-mix-speed a (* mult (mix-speed a)))))
	      track))))))

;;; (transpose-track '(0 1) 5)


;;; track-position

(define track-position
  (lambda (track)
    "(track-position track) returns the begin time of track (the minimum mix begin sample associated with track)"
    (letrec ((track-pos
	      (lambda (trk)
		(if (null? trk)
		    '()
		    (if (mix? (car trk))
			(cons (mix-position (car trk)) (track-pos (cdr trk)))
			(track-pos (cdr trk)))))))
      (let ((pos (track-pos track)))
	(if (null? pos)
	    #f                   ;throw 'no-such-track?
	    (apply min pos))))))

(define set-track-position
  (lambda (track new-position)
    "(set-track-position track new-beg) moves all mixes in track so that the track starts at new-beg"
    (let* ((track-beg (track-position track))
	   (change (- new-position track-beg)))
      (if track-beg
	  (as-one-edit 
	   (lambda ()
	     (map (lambda (a) 
		    (if (mix? a)
			(set-mix-position a (+ change (mix-position a)))))
		  track)))))))

(define track-end
  (lambda (track)
    "(track-end track) returns endpoint (maximum frame in mixes) of track"
    (letrec ((track-pos
	      (lambda (trk)
		(if (null? trk)
		    '()
		    (if (mix? (car trk))
			(cons (+ (mix-position (car trk)) 
				 (mix-length (car trk))) 
			      (track-pos (cdr trk)))
			(track-pos (cdr trk)))))))
      (let ((pos (track-pos track)))
	(if (null? pos)
	    #f                   ;throw 'no-such-track?
	    (apply max pos))))))

(define track-length
  (lambda (track)
    "(track-length track) returns number of samples between track start and end"
    (let ((beg (track-position track))
	  (end (track-end track)))
      (if (and beg end) (- end beg) 0))))


;;; track tempo

(define set-track-tempo 
  (lambda (track tempo)
    "(set-track-tempo track tempo) changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)"
    (if (and (not (= tempo 1.0)) (not (= tempo 0.0)))
	(let ((track-beg (track-position track))
	      (new-tempo (/ 1.0 tempo))) ;make tempo>1.0 go faster
	  (if track-beg
	      (as-one-edit 
	       (lambda ()
		 (map (lambda (a) 
			(if (mix? a)
			    (set-mix-position a (+ track-beg 
						   (inexact->exact (* new-tempo 
								      (- (mix-position a) track-beg)))))))
		      track))))))))

;;; (set-track-tempo '(0 1) 2.0)

(define set-track-color 
  (lambda (track new-color)
    "(set-track-color track color) changes the associated mix console colors to color"
    (map (lambda (a) 
	   (if (mix? a)
	       (set-mix-color new-color a)))
	 track)))

;;; (set-track-color '(0 1) (make-color 0 0 1))

(define track-color
  (lambda (track)
    "(track-color track) returns track color"
    (if (null? track)
	(mix-color)
	(mix-color (car track)))))


(define track->vct
  (lambda (track)
    "(track->vct track) places track data in vct"
    (let* ((len (track-length track))
	   (trk-id (mix-track (car track)))
	   (tf (make-track-sample-reader trk-id))
	   (v (make-vct len)))
      (vct-map! v (lambda () (next-track-sample tf)))
      (free-track-sample-reader tf)
      v)))

(define save-track 
  (lambda (track filename) ;writes data as floats
    "(save-track track filename) saves track data in file filename"
    (let ((v (track->vct track))
	  (fd (open-sound-file filename 1 (srate) "")))
      (vct->sound-file fd v (vct-length v))
      (close-sound-file fd (* 4 (vct-length v))))))


;;; track-env applies an envelope to a track by enveloping the underlying mix input sounds

(define set-track-amp-env
  (lambda (track chan env) ; meant to mimic set-mix-amp-env
    "(set-track-amp-env track chan env) sets overall track amplitude envelope"
    (let ((beg (track-position track))
	  (len (track-length track)))
      (as-one-edit
       (lambda ()
	 (map (lambda (a) 
		(if (and (mix? a) (< chan (mix-chans a)))
		    (set-mix-amp-env a chan (multiply-envelopes 
					     (window-envelope (/ (- (mix-position a) beg) len)
							      (/ (- (+ (mix-position a) (mix-length a)) beg) len)
							      env)
					     (mix-amp-env a chan)))))
	      track))))))

; (set-track-amp-env (track 1) 0 '(0 0 1 1))


;;; filter-track 

(define filter-track
  (lambda (track fir-filter-coeffs)
    "(filter-track track coeffs) filters track data using FIR filter coeffs"
    (let ((order (length fir-filter-coeffs)))
      (as-one-edit
       (lambda ()
	 (map (lambda (a)
		(if (mix? a)
		    (let ((chans (mix-chans a)))
		      (do ((chan 0 (1+ chan)))
			  ((= chan chans))
			(let* ((flt (make-fir-filter order (list->vct fir-filter-coeffs)))
			       (newlen(+ order (mix-length a)))
			       (samps (make-vct newlen))
			       (reader (make-sample-reader 0 (list a) chan)))
			  (vct-map! samps (lambda ()
					    (fir-filter flt (next-sample reader))))
			  (free-sample-reader reader)
			  (vct->samples 0 newlen samps (list a) chan))))))
	      track))))))

; (filter-track (track 1) '(.1 .2 .3 .3 .2 .1))

						  
			      
      
;;; the next functions use the mix related hooks to affect the entire associated track

(define reposition-track 
  (lambda (id samps-moved)
    ;; id = mix that moved (via mouse), move all other mixes in its track by the same amount
    (let ((trk (mix-track id)))
      (if (> trk 0)
	  (let ((track-mixes (track trk)))
	    (as-one-edit
	     (lambda ()
	       (map (lambda (a) 
		      (if (mix? a)
			  (set-mix-position a (+ samps-moved (mix-position a)))))
		    track-mixes)))
	    #t)
	  #f))))

(add-hook! mix-position-changed-hook reposition-track)

(define respeed-track
  (lambda (id)
    (let ((trk (mix-track id)))
      (if (> trk 0)
	  (let ((track-mixes (track trk))
		(new-speed (mix-speed id)))
	    (set-track-speed track-mixes new-speed)
	    #t)
	  #f))))

(add-hook! mix-speed-changed-hook respeed-track)

(define unused-track
  (lambda ()
    (let ((ntrack 0))
      (tree-apply (lambda (n)
		    (set! ntrack (max ntrack (mix-track n))))
		  (mixes))
      (+ ntrack 1))))

(define sync-multichannel-mixes
  (lambda (mix-ids)
    (let ((new-track (unused-track)))
      (map (lambda (n) (set-mix-track n new-track)) mix-ids))))

(add-hook! multichannel-mix-hook sync-multichannel-mixes)
