;;; bounded brownian noise ("green noise" was Michael McNabb's name for it)
;;; on each step, the bounds are checked, and if exceeded, the step is sent the other direction.

(use-modules (ice-9 optargs))

(if (not (defined? 'pi)) (define pi 3.14159265))
(if (not (defined? 'two-pi)) (define two-pi (* 2 pi)))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))

(def-clm-struct grn 
  (output 0.0 :type float) 
  (amp .1 :type float) 
  (lo -1.0 :type float) 
  (hi 1.0 :type float) 
  (phase 0.0 :type float) 
  (freq 0.0 :type float) 
  (incr 0.0 :type float))


(define (brownian-noise gr) ; unbounded output
  (set! (grn-output gr) (+ (grn-output gr) (mus-random (grn-amp gr))))
  (grn-output gr))


(define* (make-green-noise :key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0))
  (make-grn :freq (hz->radians frequency)
	    :amp amplitude
	    :hi high
	    :lo low))

(define (green-noise r sweep)
  (if (>= (grn-phase r) two-pi)
      (begin
       (do () ((< (grn-phase r) two-pi)) (set! (grn-phase r) (- (grn-phase r) two-pi)))
       (let ((val (mus-random (grn-amp r))))
	 (set! (grn-output r) (+ (grn-output r) val))
	 (if (not (<= (grn-lo r) (grn-output r) (grn-hi r)))
	     (set! (grn-output r) (- (grn-output r) (* 2 val)))))))
  (set! (grn-phase r) (+ (grn-phase r) (+ (grn-freq r) sweep)))
  (do () ((>= (grn-phase r) 0.0)) (set! (grn-phase r) (+ (grn-phase r) two-pi)))
  (grn-output r))


(define* (make-green-noise-interp :key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0))
  (make-grn :freq (hz->radians frequency)
	    :hi high
	    :lo low
	    :amp amplitude
	    :incr (* (mus-random amplitude) (/ frequency (mus-srate)))))

(define (green-noise-interp r sweep)
  (set! (grn-output r) (+ (grn-output r) (grn-incr r)))
  (if (>= (grn-phase r) two-pi)
      (let* ((val (mus-random (grn-amp r)))
	     (newg (+ (grn-output r) val)))
	(if (not (<= (grn-lo r) newg (grn-hi r)))
	    (set! val (- val)))
	(set! (grn-incr r) (* val (/ (+ (grn-freq r) sweep) two-pi)))
	(do () ((< (grn-phase r) two-pi)) (set! (grn-phase r) (- (grn-phase r) two-pi)))))
  ;; (both grn-freq and sweep are in terms of radians/sample, so by dividing by two-pi, we get
  ;; the distance we go to the next new number in terms of 0..1 (i.e. inverse of number of
  ;; samples per period) -- this is equivalent to the multiply in the make function.
  (set! (grn-phase r) (+ (grn-phase r) (+ (grn-freq r) sweep)))
  (do () ((>= (grn-phase r) 0.0)) (set! (grn-phase r) (+ (grn-phase r) two-pi)))
  (grn-output r))

#|
(definstrument (green1 beg end freq amp lo hi)
  (let ((grn (make-green-noise :frequency freq :amplitude amp :high hi :low lo)))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (green-noise grn 0.0) *output*))))))

(definstrument (green2 beg end freq amp lo hi)
  (let ((grn (make-green-noise-interp :frequency freq :amplitude amp :high hi :low lo)))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (green-noise-interp grn 0.0) *output*))))))

(with-sound () (green1 0 10000 1000 0.1 -0.5 0.5) (green2 10000 20000 1000 0.1 -0.5 0.5))


(definstrument (green3 start dur freq amp amp-env noise-freq noise-width noise-max-step)
  ;; brownian noise on amp env
  (let* ((grn (make-green-noise-interp :frequency noise-freq :amplitude noise-max-step :high (* 0.5 noise-width) :low (* -0.5 noise-width)))
	 (osc (make-oscil freq))
	 (e (make-env amp-env :scaler amp :duration dur))
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate)))))))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* (env e) 
		    (+ 1.0 (green-noise-interp grn 0.0))
		    (oscil osc)) 
	       *output*))))))

(with-sound () (green3 0 2.0 440 .5 '(0 0 1 1 2 1 3 0) 100 .2 .02))


(definstrument (green4 start dur freq amp freq-env gliss noise-freq noise-width noise-max-step)
  ;; same but on freq env
  (let* ((grn (make-green-noise-interp :frequency noise-freq :amplitude noise-max-step :high (* 0.5 noise-width) :low (* -0.5 noise-width)))
	 (osc (make-oscil freq))
	 (e (make-env freq-env :scaler gliss :duration dur))
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate)))))))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* amp (oscil osc (hz->radians (+ (env e) (green-noise-interp grn 0.0)))))
	       *output*))))))

(with-sound () (green4 0 2.0 440 .5 '(0 0 1 1 2 1 3 0) 440 100 100 10))
|#
