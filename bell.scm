(define fm-bell
  (lambda (startime dur frequency amplitude amp-env index-env index)
    (let* ((beg (inexact->exact (* startime (srate))))
	   (len (inexact->exact (* dur (srate))))
	   (end (+ beg len))
	   (fmInd1 (hz->radians (* 32.0 frequency)))
	   (fmInd2 (hz->radians (* 4.0 (- 8.0 (/ frequency 50.0)))))
	   (fmInd3 (* fmInd2 0.705 (- 1.4 (/ frequency 250.0))))  
	   (fmInd4 (hz->radians (* 32.0 (- 20 (/ frequency 20)))))
	   (mod1 (make-oscil :frequency (* frequency 2)))
	   (mod2 (make-oscil :frequency (* frequency 1.41)))
	   (mod3 (make-oscil :frequency (* frequency 2.82)))
	   (mod4 (make-oscil :frequency (* frequency 2.4)))
	   (car1 (make-oscil :frequency frequency))
	   (car2 (make-oscil :frequency frequency))
	   (car3 (make-oscil :frequency (* frequency 2.4)))
	   (indf (make-env index-env index dur))
	   (ampf (make-env amp-env amplitude dur))
	   (out-data (make-vct len)))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(let ((fmenv (env indf)))
	  (vct-set! out-data i 
		    (* (env ampf)
		       (+ (oscil car1 (* fmenv fmInd1 (oscil mod1)))
			  (* .15 (oscil car2 (* fmenv 
						(+ (* fmInd2 (oscil mod2))
						   (* fmInd3 
						      (oscil mod3))))))
			  (* .15 (oscil car3 (* fmenv 
						fmInd4 
						(oscil mod4)))))))))
      (mix-vct out-data beg))))


;(define fbell '(0 1 2 1.1000 25 .7500 75 .5000 100 .2000 ))
;(define abell '(0 0 .1000 1 10 .6000 25 .3000 50 .1500 90 .1000 100 0 ))
;(fm-bell 0.0 1.0 220.0 .5 abell fbell 1.0)
