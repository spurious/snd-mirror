;;; use the lisp graph section of the display as an envelope editor
;;;
;;; (start-enveloping) sets this in progress (for subsequently opened sounds)
;;; (stop-enveloping) turns it off
;;; (channel-envelope snd chn) returns the current envelope associated with snd's channel chn
;;; (set! (channel-envelope snd chn) env) sets it to a new list of breakpoints


(use-modules (ice-9 common-list))


(define envelopes '())
;;; a list of lists, each inner list representing one channel: (snd chn envelope)

(define (remove-sound-envelopes snd)
  (set! envelopes (remove-if 
		   (lambda (n) 
		     (= (car n) snd)) 
		   envelopes)))

(define (remove-channel-envelope snd chn)
  (set! envelopes (remove-if 
		   (lambda (n) 
		     (and (= (car n) snd) 
			  (= (cadr n) chn))) 
		   envelopes)))

(define (search-envelopes snd chn envs)
  (if (not (null? envs))
      (let ((cur (car envs)))
	(if (and (= (car cur) snd)
		 (= (cadr cur) chn))
	    (caddr cur)
	    (search-envelopes snd chn (cdr envs))))
      '()))

(define channel-envelope
  (make-procedure-with-setter
   (lambda (snd chn)
     (search-envelopes snd chn envelopes))
   (lambda (snd chn new-env)
     (remove-channel-envelope snd chn)
     (graph new-env "" 0.0 1.0 0.0 1.0 snd chn)
     (set! envelopes (cons (list snd chn new-env) envelopes)))))

(define (create-initial-envelopes snd)
  (do ((i 0 (1+ i)))
      ((= i (channels snd)))
    (set! envelopes (cons (list snd i (list 0.0 1.0 1.0 1.0)) envelopes))
    (set! (dot-size snd i) 8)
    (graph (list 0.0 1.0 1.0 1.0) "" 0.0 1.0 0.0 1.0 snd i)))

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

(define (edit-envelope-point pos x y cur-env)
  (let ((new-env '()))
    (define (search-point e npos)
      (if (= npos pos)
	  (append new-env (list x y) (cddr e))
	  (begin
	    (set! new-env (append new-env (list (car e) (cadr e))))
	    (search-point (cddr e) (+ npos 2)))))
    (search-point cur-env 0)))

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

(define (envelope-position x cur-env)
  (define (search-point e pos)
    (if (= (car e) x)
	pos
	(search-point (cddr e) (+ pos 2))))
  (search-point cur-env 0))

(define mouse-radius .03)

(define (on-dot? x y cur-env pos)
  (and (not (null? cur-env))
       (or (and (< (abs (- (car cur-env) x)) mouse-radius)
		(< (abs (- (cadr cur-env) y)) mouse-radius)
		pos)
	   (on-dot? x y (cddr cur-env) (+ pos 2)))))
  

;;; if click on point, delete,
;;; if click/drag on point, move it until release
;;; if click off point, add (and subsequent drag)

(define mouse-down 0)
(define mouse-up 0)
(define click-time 10) ; .1 sec?
(define mouse-pos 0)
(define mouse-new #f)

(define (mouse-press-envelope snd chn button state ux uy)
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
	(set! mouse-pos pos))))

(define (mouse-drag-envelope snd chn button state x y)
  ;; point exists, needs to be edited with check for various bounds
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
    (update-lisp-graph snd chn)))

(define (mouse-release-envelope snd chn button state x y)
  (let ((cur-env (channel-envelope snd chn)))
    (set! mouse-up (get-internal-real-time))
    (if (and (not mouse-new)
	     (<= (- mouse-up mouse-down) click-time)
	     (not (= mouse-pos 0))
	     (not (>= mouse-pos (- (length cur-env) 2))))
	(set! (channel-envelope snd chn)
	      (remove-envelope-point mouse-pos cur-env)))
    (update-lisp-graph snd chn)
    (set! mouse-new #f)))


(define (start-enveloping)
  (set! envelopes '())
  (add-hook! after-open-hook create-initial-envelopes)
  (add-hook! close-hook remove-sound-envelopes)
  (add-hook! mouse-press-hook mouse-press-envelope)
  (add-hook! mouse-drag-hook mouse-drag-envelope)
  (add-hook! mouse-release-hook mouse-release-envelope))

(define (stop-enveloping)
  (remove-hook! after-open-hook create-initial-envelopes)
  (remove-hook! close-hook remove-sound-envelopes)
  (remove-hook! mouse-press-hook mouse-press-envelope)
  (remove-hook! mouse-drag-hook mouse-drag-envelope)
  (remove-hook! mouse-release-hook mouse-release-envelope)
  (set! envelopes '()))


;;; --------------------------------------------------------------------------------
;;;
;;; some examples of using this envelope editor

(define play-with-envs
  ;; sets channel amps from envs (change amp below to whatever control-panel field is of interest)
  (lambda (sound)
    (let ((chans (chans sound)))
      (add-hook! stop-playing-hook 
		 (lambda (s) 
		   (reset-hook! play-hook)
		   (reset-hook! stop-playing-hook)))
      (do ((chan 0 (1+ chan)))
	  ((= chan chans))
	(let ((player (make-player sound chan))
	      (e (make-env (channel-envelope sound chan) 
			   :end (inexact->exact (/ (frames sound chan) 
						   (dac-size))))))
	  (add-player player)
	  (add-hook! play-hook (lambda (fr)
				 ;; if fr (dac buffer size in frames) is not dac-size, we should do something debonair
				 (set! (amp player) (env e))))))
      (start-playing chans (srate sound)))))

(define play-panned
  ;; assume sound has one channel and we want to pan it into 2 chans following env
  (lambda (sound)
    (let* ((bufsize 256)
	   (data (make-sound-data 2 bufsize))
           (bytes (* bufsize 4))
	   (audio-fd (mus-audio-open-output mus-audio-default (srate sound) 2 mus-lshort bytes))
	   (samp 0)
	   (len (frames sound 0)))
      (snd-print (format #f "audio-fd: ~A " audio-fd))
      (if (not (= audio-fd -1))
	  (let ((e (make-env (channel-envelope sound 0) :end (inexact->exact (/ len bufsize)))))
	    (do ((res #f (let* ((scaler (env e))
				(samps0 (samples->vct samp bufsize))
				(samps1 (vct-copy samps0)))
			   (vct->sound-data (vct-scale! samps0 scaler) data 0)
			   (vct->sound-data (vct-scale! samps1 (- 1.0 scaler)) data 1)
			   (mus-audio-write audio-fd data bufsize)
			   (set! samp (+ samp bufsize))
			   (>= samp len))))
		(res))
	    (mus-audio-close audio-fd))))))


;;; (start-enveloping)
;;; (open-sound "oboe.snd")


