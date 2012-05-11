;;; use the lisp graph section of the display as an envelope editor
;;;
;;; (start-enveloping) sets this in progress (for subsequently opened sounds)
;;; (stop-enveloping) turns it off
;;; (channel-envelope snd chn) returns the current envelope associated with snd's channel chn
;;; (set! (channel-envelope snd chn) env) sets it to a new list of breakpoints
;;;
;;; (play-with-envs snd) sets channel amps during playback from the associated enved envelopes
;;; (play-panned snd) pans a mono sound following its enved envelope into a stereo sound

(provide 'snd-enved.scm)

(define channel-envelope
  (make-procedure-with-setter

   (lambda (snd chn)
     "(channel-envelope snd chn) returns the current enved envelope associated with snd's channel chn"
     (or (channel-property 'enved-envelope snd chn)
	 '()))

   (lambda (snd chn new-env)
     (set! (channel-property 'enved-envelope snd chn) new-env)
     (graph new-env "" 0.0 1.0 0.0 1.0 snd chn))))


(define (create-initial-envelopes hook)
  (let ((snd (hook 'snd)))
    (do ((i 0 (+ 1 i)))
	((= i (channels snd)))
      (set! (dot-size snd i) 8)
      (set! (channel-envelope snd i) (list 0.0 1.0 1.0 1.0)))))


;;; if click on point, delete,
;;; if click/drag on point, move it until release
;;; if click off point, add (and subsequent drag)

(define mouse-down 0)
(define mouse-up 0)
(define click-time 10) ; .1 sec?
(define mouse-pos 0)
(define mouse-new #f)

(define (mouse-press-envelope hook)
  (let ((snd (hook 'snd))
	(chn (hook 'chn))
	(ux (hook 'x))
	(uy (hook 'y))
	(mouse-radius .03))

    (define (add-envelope-point x y cur-env)
      (let ((new-env '()))
	(define (search-point e)
	  (if (null? e)
	      (append new-env (list x y))
	      (if (= (car e) x)
		  (append new-env (list x y) (cddr e))
		  (if (> (car e) x)
		      (append new-env (list x y) e)
		      (begin
			(set! new-env (append new-env (list (car e) (cadr e))))
			(search-point (cddr e)))))))
	(search-point cur-env)))

    (define (envelope-position x cur-env)
      (define (search-point e pos)
	(if (= (car e) x)
	    pos
	    (search-point (cddr e) (+ pos 2))))
      (search-point cur-env 0))

    (define (on-dot? x y cur-env pos)
      (and (not (null? cur-env))
	   (or (and (< (abs (- (car cur-env) x)) mouse-radius)
		    (< (abs (- (cadr cur-env) y)) mouse-radius)
		    pos)
	       (on-dot? x y (cddr cur-env) (+ pos 2)))))
  
    (let* ((x (max 0.0 (min ux 1.0)))
	   (y (max 0.0 (min uy 1.0)))
	   (cur-env (channel-envelope snd chn))
	   (pos (on-dot? x y cur-env 0)))
      (set! mouse-new (not pos))
      (set! mouse-down (get-internal-real-time))
      (if (not pos)
	  (let ((new-x (max 0.001 (min x .999))))
	    (set! (channel-envelope snd chn) 
		  (add-envelope-point new-x y cur-env))
	    (set! mouse-pos (envelope-position new-x (channel-envelope snd chn))))
	  (set! mouse-pos pos)))))

(define (mouse-drag-envelope hook)
  (let ((snd (hook 'snd))
	(chn (hook 'chn))
	(x (hook 'x))
	(y (hook 'y)))
  ;; point exists, needs to be edited with check for various bounds

    (define (edit-envelope-point pos x y cur-env)
      (let ((new-env '()))
	(define (search-point e npos)
	  (if (= npos pos)
	      (append new-env (list x y) (cddr e))
	      (begin
		(set! new-env (append new-env (list (car e) (cadr e))))
		(search-point (cddr e) (+ npos 2)))))
	(search-point cur-env 0)))
    
    (let* ((cur-env (channel-envelope snd chn))
	   (lx (if (= mouse-pos 0)
		   0.0
		   (if (>= mouse-pos (- (length cur-env) 2))
		       1.0
		       (max (+ (list-ref cur-env (- mouse-pos 2)) .001)
			    (min x
				 (- (list-ref cur-env (+ mouse-pos 2)) .001))))))
	   (ly (max 0.0 (min y 1.0))))
      (set! (channel-envelope snd chn) 
	    (edit-envelope-point mouse-pos lx ly cur-env))
      (update-lisp-graph snd chn))))

(define (mouse-release-envelope hook)
  (let ((snd (hook 'snd))
	(chn (hook 'chn))
	(ux (hook 'x))
	(uy (hook 'y))
	(axis (hook 'axis)))

    (define (remove-envelope-point pos cur-env)
      (let ((new-env '()))
	(define (search-point e npos)
	  (if (null? e)
	      new-env
	      (if (= pos npos)
		  (append new-env (cddr e))
		  (begin
		    (set! new-env (append new-env (list (car e) (cadr e))))
		    (search-point (cddr e) (+ npos 2))))))
	(search-point cur-env 0)))
    
    (if (= axis lisp-graph)
	(let ((cur-env (channel-envelope snd chn)))
	  (set! mouse-up (get-internal-real-time))
	  (if (and (not mouse-new)
		   (<= (- mouse-up mouse-down) click-time)
		   (not (= mouse-pos 0))
		   (not (>= mouse-pos (- (length cur-env) 2))))
	      (set! (channel-envelope snd chn)
		    (remove-envelope-point mouse-pos cur-env)))
	  (update-lisp-graph snd chn)
	  (set! mouse-new #f)
	  (set! (hook 'result) #t)))))

(define (enveloping-key-press hook)
  (let ((snd (hook 'snd))
	(chn (hook 'chn))
	(key (hook 'key))
	(state (hook 'state)))

    ;; C-g returns to original env
    ;; C-. applies current env to amplitude
    (if (and (= key (char->integer #\.))
	     (= state 4))
	(begin
	  (env-channel (channel-envelope snd chn) 0 (frames snd chn) snd chn)
	  (set! (hook 'result) #t))
	(if (and (= key (char->integer #\g))
		 (= state 4))
	    (begin
	      (set! (channel-envelope snd chn) '(0.0 1.0 1.0 1.0))
	      (set! (hook 'result) #t))))))

(define (start-enveloping)
  "(start-enveloping) starts the enved processes, displaying an envelope editor in each channel"
  (hook-push after-open-hook create-initial-envelopes)
  (hook-push mouse-press-hook mouse-press-envelope)
  (hook-push mouse-drag-hook mouse-drag-envelope)
  (hook-push mouse-click-hook mouse-release-envelope)
  (hook-push key-press-hook enveloping-key-press))

(define (stop-enveloping)
  "(stop-enveloping) turns off the enved channel-specific envelope editors"
  (hook-remove after-open-hook create-initial-envelopes)
  (hook-remove mouse-press-hook mouse-press-envelope)
  (hook-remove mouse-drag-hook mouse-drag-envelope)
  (hook-remove mouse-click-hook mouse-release-envelope)
  (hook-remove key-press-hook enveloping-key-press))


;;; --------------------------------------------------------------------------------
;;;
;;; some examples of using this envelope editor

(define* (play-with-envs (sound #f))
  "(play-with-envs snd) sets channel amps during playback from the associated enved envelopes"
  (let ((chans (channels sound)))
    (do ((chan 0 (+ 1 chan)))
	((= chan chans))
      (let ((player (make-player sound chan))
	    (e (make-env (channel-envelope sound chan) 
			 :length (floor (/ (frames sound chan) (dac-size))))))
	(add-player player 0 -1 -1 (lambda (reason) (set! (hook-functions play-hook) '())))
	(hook-push play-hook (lambda (hook)
			       ;; if dac buffer size in frames is not dac-size, we should do something debonair
			       (set! (amp-control player) (env e))))))
    (start-playing chans (srate sound))))

(define (play-panned sound)
  "(play-panned snd) pans a mono sound following its enved envelope into a stereo sound"
  (let* ((bufsize 256)
	 (data (make-sound-data 2 bufsize))
	 (bytes (* bufsize 4))
	 (audio-fd (mus-audio-open-output mus-audio-default (srate sound) 2 mus-lshort bytes))
	 (samp 0)
	 (len (frames sound 0)))
    (snd-print (format #f "audio-fd: ~A " audio-fd))
    (if (not (= audio-fd -1))
	(let ((e (make-env (channel-envelope sound 0) :length (floor (/ len bufsize)))))
	  (catch #t
		 (lambda ()
		   (do ((res #f (let* ((scaler (env e))
				       (samps0 (channel->vct samp bufsize))
				       (samps1 (vct-copy samps0)))
				  (vct->sound-data (vct-scale! samps0 scaler) data 0)
				  (vct->sound-data (vct-scale! samps1 (- 1.0 scaler)) data 1)
				  (mus-audio-write audio-fd data bufsize)
				  (set! samp (+ samp bufsize))
				  (>= samp len))))
		       (res)))
		 (lambda args (display (format #f ";play-panned error: ~A" args))))
	  (mus-audio-close audio-fd)))))


;;; (start-enveloping)
;;; (open-sound "oboe.snd")


