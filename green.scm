;;; bounded brownian noise ("green noise" was Michael McNabb's name for it)
;;; on each step, the bounds are checked, and if exceeded, the step is sent the other direction.

(use-modules (ice-9 optargs))

(provide 'snd-green.scm)

(if (not (defined? 'two-pi)) (define two-pi (* 2 pi)))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))

(def-clm-struct grnoi
  (output 0.0 :type float) 
  (amp .1 :type float) 
  (lo -1.0 :type float) 
  (hi 1.0 :type float) 
  (phase 0.0 :type float) 
  (freq 0.0 :type float) 
  (incr 0.0 :type float))


(define (brownian-noise gr) ; unbounded output
  "(brownian-noise gr) produces brownian noise from a green noise generator"
  (set! (grnoi-output gr) (+ (grnoi-output gr) (mus-random (grnoi-amp gr))))
  (grnoi-output gr))


(define* (make-green-noise :key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0))
  "(make-green-noise :key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0)) makes a green-noise generator"
  (make-grnoi :freq (hz->radians frequency)
	      :amp amplitude
	      :hi high
	      :lo low))

(define (green-noise r sweep)
  "(green-noise r sweep) runs a green-noise generator"
  (if (>= (grnoi-phase r) two-pi)
      (begin
       (do () ((< (grnoi-phase r) two-pi)) (set! (grnoi-phase r) (- (grnoi-phase r) two-pi)))
       (let ((val (mus-random (grnoi-amp r))))
	 (set! (grnoi-output r) (+ (grnoi-output r) val))
	 (if (not (<= (grnoi-lo r) (grnoi-output r) (grnoi-hi r)))
	     (set! (grnoi-output r) (- (grnoi-output r) (* 2 val)))))))
  (set! (grnoi-phase r) (+ (grnoi-phase r) (+ (grnoi-freq r) sweep)))
  (do () ((>= (grnoi-phase r) 0.0)) (set! (grnoi-phase r) (+ (grnoi-phase r) two-pi)))
  (grnoi-output r))


(define* (make-green-noise-interp :key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0))
  "(make-green-noise-interp :key (frequency 440.0) (amplitude 1.0) (high 1.0) (low -1.0)) makes an interpolating green-noise generator"
  (make-grnoi :freq (hz->radians frequency)
	      :hi high
	      :lo low
	      :amp amplitude
	      :incr (* (mus-random amplitude) (/ frequency (mus-srate)))))

(define (green-noise-interp r sweep)
  "(green-noise-interp r sweep) runs an interpolating green-noise generator"
  (set! (grnoi-output r) (+ (grnoi-output r) (grnoi-incr r)))
  (if (>= (grnoi-phase r) two-pi)
      (let* ((val (mus-random (grnoi-amp r)))
	     (newg (+ (grnoi-output r) val)))
	(if (not (<= (grnoi-lo r) newg (grnoi-hi r)))
	    (set! val (- val)))
	(set! (grnoi-incr r) (* val (/ (+ (grnoi-freq r) sweep) two-pi)))
	(do () ((< (grnoi-phase r) two-pi)) (set! (grnoi-phase r) (- (grnoi-phase r) two-pi)))))
  ;; (both grn-freq and sweep are in terms of radians/sample, so by dividing by two-pi, we get
  ;; the distance we go to the next new number in terms of 0..1 (i.e. inverse of number of
  ;; samples per period) -- this is equivalent to the multiply in the make function.
  (set! (grnoi-phase r) (+ (grnoi-phase r) (+ (grnoi-freq r) sweep)))
  (do () ((>= (grnoi-phase r) 0.0)) (set! (grnoi-phase r) (+ (grnoi-phase r) two-pi)))
  (grnoi-output r))

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
	 (beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur))))
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
	 (beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* amp (oscil osc (hz->radians (+ (env e) (green-noise-interp grn 0.0)))))
	       *output*))))))

(with-sound () (green4 0 2.0 440 .5 '(0 0 1 1 2 1 3 0) 440 100 100 10))


;;; to see the spectral differences:

(add-transform "psd" "spectrum" 0.0 1.0
	       ;; check the log/log plot
	       (lambda (len reader)
		 (let ((data (make-vct (* 2 len))))
		   (do ((i 0 (1+ i)))
		       ((= i (* 2 len)))
		     (vct-set! data i (reader)))
		   (snd-spectrum (autocorrelate data)))))

(let ((ind (new-sound "test.snd" :size 20000)))

  ;; white noise -- spectrum is flat at 1.0
  (map-channel (lambda (y) 
		 (*  0.5 (- (random 2.0) 1.0))))

  ;; brownian falls off rapidly
  (let ((gr (make-green-noise 10000 .01))) 
    (map-channel (lambda (y) 
		   (* .01 (brownian-noise gr)))))

  ;; more or less 1/f -- falls off slowly
  ;;   my informal explanation is that each time we bounce off an edge, we're transferring energy from
  ;;   from a low frequency into some higher frequency.  This is still Brownian noise however --
  ;;   see Resnick, "Adventures in Stochastic Processes" p 497.

  (let ((gr (make-green-noise 20000 1))) 
    (map-channel (lambda (y) 
		   (green-noise gr 0.0)))) 

  ;; Orfanidis rands 1/f idea:
  (let ((rans (make-vector 12))
	(oct 2.0))
    (do ((i 0 (1+ i)))
	((= i 12))
      (vector-set! rans i (make-rand (/ (srate) oct)))
      (set! oct (* 2 oct)))
    (map-channel (lambda (y)
		   (let ((sum 0.0))
		     (do ((i 0 (1+ i)))
			 ((= i 12))
		       (set! sum (+ sum (rand (vector-ref rans i)))))
		     (/ sum 12.0))))))

|#
