;; CLM implementation of Xenakis' Dynamic Stochastic Synthesis as heard in
;; his GENDY3, S.709, Legende d'Eer, etc.
;; 12/17/03
;; revised 01/22/06
;; Bill Sack wsack@buffalo.edu

;; revised slightly to accommodate the run macro, Bill 13-Jun-06

(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))
(if (not (provided? 'snd-env.scm)) (load-from-path "env.scm"))

(definstrument 
  (stochastic start dur :key
	      (amp .9) (bits 16) (xmin 1) (xmax 20) (xwig 0) (xstep 1) (ywig 0) (xfb 0)
	      (init-array '((10 0) (10 1) (10 0) (10 -.7) (10 0) (10 .5) 
			    (10 0) (10 -.3) (10 0) (10 .2) (10 0) (10 -.1))))
  ;;some explanation of the parameters:
  ;;amp - scales overall amplitude
  ;;bits - the resolution of the wave's amplitude dimension
  ;;xmin - minimum number of samples between time breakpoints. must be equal to or greater than 1
  ;;xmax - maximum number of samples between time breakpoints.
  ;;xwig - amplitude applied to random walk function in time dimension
  ;;xstep - quantization of freedom in time dimension, in samples. minimum of 1
  ;;ywig - amplitude applied to random walk function in amplitude dimension, in 
  ;;       percent of overall possible amplitude
  ;;xfb - an attempt at an FIR low-pass filter - the old (n + (x * n-1)) trick,
  ;;      not really that useful
  ;;init-array - initial x and y breakpoints for wave. x values must be 
  ;;             integers and 1 or greater, y values between -1.0 and 1.0
  (let* ((y 0.0) (dx 0) (prev-dx 0) (dy 0.0)
	 (j 0.0) (m 0) (dt 0) (output 0.0) 
	 (oldy 0.0) (xdev 0) (ydev 0)
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (d-click (make-env (list 0 1 (- end 100) 1 end 0) :duration dur))
	 (b (expt 2 (- bits 1))); because we use signed ints - see (- b) below
	 ;;make vct to hold x,y breakpoints
	 (xy-array (make-vct (* (length init-array) 2)))
	 (xy-array-l (inexact->exact (vct-length xy-array)))
	 )
    ;;fill xy-array with values from init-array
    (do ((iy 0 (+ iy 2));;index for reading values from init-array (a 2-dimensional list)
	 (jy 0 (+ jy 1)));;index for writing to xy-array (a 1-dimensional vct)
	((= iy xy-array-l) xy-array)
      (vct-set! xy-array iy 
		(list-ref (list-ref init-array jy) 0))
      (vct-set! xy-array (+ iy 1)
		;;convert signed float y values into signed integers 
		(inexact->exact 
		 (floor (* b
			   (list-ref (list-ref init-array jy) 1)
			   )))
		))
    (ws-interrupt?) ;;does this really belong here?
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (if (= dx dt);;when current sample is a breakpoint
	     (begin
	       (set! dx (inexact->exact (vct-ref xy-array (modulo m xy-array-l))))
	       (set! y (vct-ref xy-array (+ (modulo m xy-array-l) 1)))
	       (set! prev-dx (inexact->exact (vct-ref xy-array (modulo (- m 2) xy-array-l))))
	       (set! dy (- y oldy))
	       (set! oldy y)
	       ;;straight uniform distribution for y
	       (set! ydev (inexact->exact (round (* (- 1.0 (random 2.0)) (* .01 b ywig)))))
	       ;;gaussian distribution for x
	       (set! xdev 
		     (* xstep (inexact->exact (round 
					       (* xwig 
						  (* (sqrt (* -2.0 (log (- 1 (random 1.0)))))
						     (cos (* 6.283185307179586 (random 1.0)))))))))
	       (vct-set! xy-array (modulo m xy-array-l)
			 ;;mirror stuff for x
			 (cond ((or  (< (round xmax) (+ dx xdev))
				     (> (round xmin)(+ dx xdev)))
				(max (min ;;this mirror is attentuated
				      (inexact->exact (round (+ (* xfb prev-dx) (* (- 1  xfb) (+ dx (- xdev))))))
				      (inexact->exact (round xmax))) (inexact->exact (round xmin))))
			       (else (inexact->exact (round (+ (* xfb prev-dx)
							       (* (- 1  xfb) (+ dx xdev))))))))
	       (vct-set! xy-array (+ (modulo m xy-array-l) 1)
			 ;;mirror stuff for y 
			 (cond ((or (< b (+ y ydev)) (> (- b) (+ y ydev)))
				(max (min (+ y (- ydev)) b) (- b)))
			       (else (+ y ydev))))
	       (set! m (+ m 2))
	       (set! dt 0)))
	 (set! dt (+ 1 dt))
	 (set! j (+ j (/ dy dx)));linear interpolation
	 (set! output (/ j b));normalization -1 to 1
	 (outa i (* amp output (env d-click)) *output*))))))

;(with-sound (:statistics #t)(stochastic 0 10 :xwig .25 :ywig 10.0))
