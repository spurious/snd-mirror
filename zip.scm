;;; create the 'digital zipper' effect
;;; a not-very-debonair way to fade out file1 and fade in file2

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(debug-enable 'debug 'backtrace)
(read-enable 'positions)
(read-set! keywords 'prefix) ;this so we can use ":" as the keyword prefix

(define ifloor (lambda (n) (inexact->exact (floor n))))
(define pi 3.141592653589793)

(define max-envelope
  (lambda (e mx)
    (if (null? e)
	mx
      (max-envelope (cddr e) (max mx (abs (cadr e)))))))

(define make-zipper 
  (lambda (ramp-env frame-size frame-env)
    (let ((max-size (+ 1 (ceiling (* (srate) frame-size)))))
      (list
       (/ 20.0 (srate))  ;low-start
       (- 1.0 (/ 20.0 (srate))) ;high-start
       0           ;frame-loc
       0           ;cursamples
       (make-vct max-size)      ;frame
       (make-vct max-size)      ;frame1
       (make-vct max-size)      ;frame2
       frame-env   ;frame length in samples (can be envelope, function etc)
       ramp-env   ;ramp location (where we are between the sounds)
       ;; both are called on every sample
       ))))

(define zip-low-start (lambda (zp) (list-ref zp 0)))
(define zip-high-start (lambda (zp) (list-ref zp 1)))
(define zip-frame-loc (lambda (zp) (list-ref zp 2)))
(define set-zip-frame-loc (lambda (zp val) (list-set! zp 2 val)))
(define zip-cursamples (lambda (zp) (list-ref zp 3)))
(define set-zip-cursamples (lambda (zp val) (list-set! zp 3 val)))
(define zip-frame (lambda (zp) (list-ref zp 4)))
(define zip-frame1 (lambda (zp) (list-ref zp 5)))
(define zip-frame2 (lambda (zp) (list-ref zp 6)))
(define zip-fe (lambda (zp) (list-ref zp 7)))
(define zip-rampe (lambda (zp) (list-ref zp 8)))

(define zipper 
  (lambda (zp input1 input2)
    (let* ((ramp-loc ((zip-rampe zp)))
	   (frame-samples (ifloor ((zip-fe zp))))
	   (frame1 (zip-frame1 zp))
	   (frame2 (zip-frame2 zp)))
      (if (<= ramp-loc (zip-low-start zp))
	  (begin
	    (set-zip-frame-loc zp 0)
	    (input1))

	  (if (>= ramp-loc (zip-high-start zp))
	      (begin
		(set-zip-frame-loc zp 0)
		(input2))

	      ;; else we're in the ramp phase
	      ;;  read frame if we're within its bounds
	      (begin
		(if (>= (zip-frame-loc zp) (zip-cursamples zp))
		    ;; now get next portion of the ramp
		    (begin
		      (set-zip-frame-loc zp 0)
		      (set-zip-cursamples zp frame-samples)
		      (let* ((changept (ifloor (* frame-samples ramp-loc)))
			     (samp1 (/ 1.0 (- 1.0 ramp-loc)))
			     (samp2 (/ 1.0 ramp-loc)))
			(do ((k 0 (1+ k)))
			    ((= k frame-samples))
			  (vct-set! frame1 k (input1))
			  (vct-set! frame2 k (input2)))
			;; now resample each dependent on location in ramp (samp1 and samp2 are increments)
			(vct-fill! (zip-frame zp) 0.0)
			(let ((start-ctr 0.0))
			  (do ((k 0 (1+ k)))
			      ((= k changept))
			    (let* ((ictr (ifloor start-ctr))
				   (y0 (vct-ref frame2 ictr))
				   (y1 (vct-ref frame2 (+ ictr 1))))
			      (vct-set! (zip-frame zp) k (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			      (set! start-ctr (+ start-ctr samp2)))))
			(let ((start-ctr 0.0)
			      (m changept))
			  (do ((k 0 (1+ k)))
			      ((= k (- frame-samples changept)))
			    (let* ((ictr (ifloor start-ctr))
				   (y0 (vct-ref frame1 ictr))
				   (y1 (vct-ref frame1 (+ ictr 1))))
			      (vct-set! (zip-frame zp) m (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			      (set! start-ctr (+ start-ctr samp1))
			      (set! m (+ m 1))))))))

		(let ((result (vct-ref (zip-frame zp) (zip-frame-loc zp))))
		  (set-zip-frame-loc zp (+ (zip-frame-loc zp) 1))
		  result)))))))

(define test-zip
  (lambda ()
    (let* ((dur (frames))
	   (zp (make-zipper (let ((e (make-env '(0 0 1 1) :end dur)))
			      (lambda () 
				(env e)))
			    0.05
			    (lambda () (* (srate) 0.05))))
	  (reader0 (make-sample-reader 0 0 0))
	  (reader1 (make-sample-reader 0 1 0)))
      (map-chan (lambda (val)
	          (if val
		      (zipper zp 
			      (lambda () 
				(next-sample reader0)) 
			      (lambda () 
				(next-sample reader1)))
		      #f))))))

;;; this is also good if the same file is used twice -- sort of like a CD player gone berserk


;;; old form
; (zipper 0 1 "fyow.snd" "now.snd" '(0 0 1 1) .05)
; (zipper 0 3 "mb.snd" "fyow.snd" '(0 0 1.0 0 1.5 1.0 3.0 1.0) .025)
