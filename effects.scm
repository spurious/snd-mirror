;;; add an effects menu, as per request of Dave Phillips

(define effects-menu (add-to-main-menu "Effects"))

(add-to-menu effects-menu "reverse" (lambda () (reverse-sound)))
(add-to-menu effects-menu "normalize" (lambda () (scale-to 1.0)))

;;; next functions are taken from examp.scm

(define vct (lambda args (list->vct args)))

(define compand
  (lambda ()
    (let* ((tbl (vct -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250 
		     0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000)))
      ;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")
      (lambda (inval)
	(let ((index (+ 8.0 (* 8.0 inval))))
	  (array-interp tbl index 17))))))

(define all-chans
  (lambda ()
    (let ((sndlist '())
	  (chnlist '()))
      (map (lambda (snd)
	     (do ((i (1- (channels snd)) (1- i)))
		 ((< i 0))
	       (set! sndlist (cons snd sndlist))
	       (set! chnlist (cons i chnlist))))
	   (sounds))
      (list sndlist chnlist))))

(define map-chan-with-sync
  (lambda (func origin)
    (let ((snc (syncing)))
      (if (> snc 0)
	  (apply map
		 (lambda (snd chn)
		   (if (= (syncing snd) snc)
		       (map-chan func #f #f origin snd chn)))
		 (all-chans))
	  (map-chan func #f #f origin)))))

(add-to-menu effects-menu "compand" (lambda () (map-chan-with-sync (compand) "compand")))

(define reverb-amount .1)

(define (reverberate)
  (save-control-panel)
  (reset-control-panel)
  (set! (reverbing) #t)
  (set! (reverb-scale) reverb-amount)
  (call-apply)
  (restore-control-panel))

(add-to-menu effects-menu "reverberate" (lambda () (reverberate)))

(define echo-length .5)
(define echo-amount .2)

(define echo 
  (lambda ()
    (let ((del (make-delay (round (* echo-length (srate))))))
      (lambda (inval)
	(+ inval (delay del (* echo-amount (+ (tap del) inval))))))))

(add-to-menu effects-menu "echo" (lambda () (map-chan-with-sync (echo) "echo")))
