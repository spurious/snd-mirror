;;; create the 'digital zipper' effect
;;; a not-very-debonair way to fade out file1 and fade in file2
;;; this is also good if the same file is used twice -- sort of like a CD player gone berserk

(use-modules (ice-9 optargs))

(define* (make-zipper ramp-env #:optional frame-size frame-env)
  "(make-zipper ramp-env #:optional frame-size frame-env) makes a zipper generator.  'ramp-env' is 
a thunk (normally a ramp from 0 to 1) which sets where we are in the zipping process, 
'frame-size' is the maximum frame length during the zip in seconds (defaults to 0.05), and 
'frame-env' is a thunk returning the current frame size during the zip process."

  (let ((max-size (+ 1 (ceiling (* (srate) (or frame-size 0.05))))))
    (list
     20                       ;min section len in samples
     0                        ;frame-loc
     0                        ;cursamples
     (make-vct max-size)      ;frame
     (make-vct max-size)      ;frame1
     (make-vct max-size)      ;frame2
     (or frame-env            ;frame length in samples (can be envelope, function etc)
	 (lambda () (* (srate) 0.05)))
     ramp-env                 ;ramp location (where we are between the sounds)
     )))

(define (zipper zp input1 input2)
  "(zipper zip in1 in2) creates the digital zipper sound effect using zipper generator 'zip' and the two input thunks 'in1' and 'in2'"

  (define (ifloor n) (inexact->exact (floor n)))

  ;; zipper struct refs
  (define (zip-low-start zip) (list-ref zip 0))
  (define (zip-frame-loc zip) (list-ref zip 1))
  (define (set-zip-frame-loc zip val) (list-set! zip 1 val))
  (define (zip-cursamples zip) (list-ref zip 2))
  (define (set-zip-cursamples zip val) (list-set! zip 2 val))
  (define (zip-frame zip) (list-ref zip 3))
  (define (zip-frame1 zip) (list-ref zip 4))
  (define (zip-frame2 zip) (list-ref zip 5))
  (define (zip-fe zip) (list-ref zip 6))
  (define (zip-rampe zip) (list-ref zip 7))

  (let* ((ramp-loc ((zip-rampe zp)))
	 (frame-samples (ifloor ((zip-fe zp))))
	 (frame1 (zip-frame1 zp))
	 (frame2 (zip-frame2 zp))
	 (chunk-len (inexact->exact (* frame-samples ramp-loc))))
    (if (<= chunk-len (zip-low-start zp))
	(begin
	  (set-zip-frame-loc zp 0)
	  (input1))

	(if (>= chunk-len (- frame-samples (zip-low-start zp)))
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
		    (do ((k 0 (1+ k)))
			((= k frame-samples))
		      (vct-set! frame1 k (input1))
		      (vct-set! frame2 k (input2)))
		    ;; now resample each dependent on location in ramp (samp1 and samp2 are increments)
		    (vct-fill! (zip-frame zp) 0.0)
		    (let ((start-ctr 0.0)
			  (samp2 (/ frame-samples chunk-len)))
		      (do ((k 0 (1+ k)))
			  ((= k chunk-len))
			(let* ((ictr (ifloor start-ctr))
			       (y0 (vct-ref frame2 ictr))
			       (y1 (vct-ref frame2 (+ ictr 1))))
			  (vct-set! (zip-frame zp) k (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			  (set! start-ctr (+ start-ctr samp2)))))
		    (let ((start-ctr 0.0)
			  (samp1 (/ frame-samples (- frame-samples chunk-len))))
		      (do ((k chunk-len (1+ k)))
			  ((= k frame-samples))
			(let* ((ictr (ifloor start-ctr))
			       (y0 (vct-ref frame1 ictr))
			       (y1 (vct-ref frame1 (+ ictr 1))))
			  (vct-set! (zip-frame zp) k (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			  (set! start-ctr (+ start-ctr samp1)))))))
	      
	      (let ((result (vct-ref (zip-frame zp) (zip-frame-loc zp))))
		(set-zip-frame-loc zp (+ (zip-frame-loc zp) 1))
		result))))))


;; (zip-sound 0 1 "fyow.snd" "now.snd" '(0 0 1 1) .05)
;; (zip-sound 0 3 "mb.snd" "fyow.snd" '(0 0 1.0 0 1.5 1.0 3.0 1.0) .025)

(define* (zip-sound beg dur file1 file2 #:optional ramp size)
  "(zip-sound beg dur file1 file2 #:optional ramp-env size) zips the two files and mixes the result into the current sound"
  (let ((zip (make-zipper (let ((e (make-env (or ramp (list 0 0 1 1)) :end dur)))
			    (lambda ()
			      (env e)))
			  (or size 0.05)
			  (lambda () (inexact->exact (* (srate) (or size 0.05))))))
	(read0 (make-sample-reader 0 file1))
	(read1 (make-sample-reader 0 file2))
	(read2 (make-sample-reader beg))
	(data (make-vct dur)))
    (vct-map! data
	      (lambda ()
		(+ (zipper zip read0 read1) (read2))))
    (vct->channel data beg)))


#!
(define (ramp-test)
  (let ((data (make-vct 10000)))
    (new-sound "new-0.snd")
    (do ((i 0 (1+ i))) ((= i 10000)) (vct-set! data i (* i .0001)))
    (vct->samples 0 10000 data 0)
    (new-sound "new-1.snd")
    (do ((i 0 (1+ i))) ((= i 10000)) (vct-set! data i (- 1.0 (* i .0001))))
    (vct->samples 0 10000 data 1)
    (let* ((dur (frames))
	   (zp (make-zipper (let ((e (make-env '(0 0 1 1) :end dur)))
			      (lambda () 
				(env e)))
			    0.05
			    (lambda () (* (srate) 0.05))))
	  (reader0 (make-sample-reader 0 0 0))
	  (reader1 (make-sample-reader 0 1 0)))
      (map-chan (lambda (val)
		  (zipper zp reader0 reader1))))))
!#

