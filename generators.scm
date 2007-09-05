(load "t.scm")

;;; these try to mimic existing gens (mainly oscil), so "frequency" and "initial-phase" are placed first
;;;   so the make function places those two args first


;;; --------------------------------------------------------------------------------
;;; sndclm.html G&R 2nd col last row (with normalization)

;;; expcs: sum of cosines with exponentially decaying amps

(def-clm-struct (expcs
		 :make-wrapper 
		 (lambda (g)
		   (set! (expcs-osc g) (make-oscil (expcs-frequency g) (expcs-initial-phase g)))
		   (if (<= (expcs-t g) 0.0) (set! (expcs-t g) 0.00001))
		   (set! (expcs-cosh-t g) (cosh (expcs-t g)))
		   (let ((exp-t (exp (- (expcs-t g)))))
		     (set! (expcs-offset g) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
		     (set! (expcs-scaler g) (* (sinh (expcs-t g)) (expcs-offset g))))
		   g))
  (frequency 440.0) 
  (initial-phase 0.0)
  (t 1.0 :type float)
  (osc #f :type clm) scaler offset cosh-t)


(define (expcs gen fm)
  (declare (gen expcs) (fm float))
  (- (/ (expcs-scaler gen) 
	(- (expcs-cosh-t gen) (oscil (expcs-osc gen) fm)))
     (expcs-offset gen)))


#|
(with-sound (:clipped #f)
  (let ((gen (make-expcs 100 :t 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (expcs gen 0.0) *output*))))))


;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:clipped #f)  
  (let ((gen (make-expcs 100 :t 0.1))
	(t-env (make-env '(0 .1 1 2) :end 20000)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(set! (expcs-t gen) (env t-env))
	(set! (expcs-cosh-t gen) (cosh (expcs-t gen)))
	(let ((exp-t (exp (- (expcs-t gen)))))
	  (set! (expcs-offset gen) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
	  (set! (expcs-scaler gen) (* (sinh (expcs-t gen)) (expcs-offset gen))))
	(outa i (expcs gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------
;;; sndclm.html (G&R) 1st col 5th row (sum of odd sines)

;;; sum of n odd sines

(def-clm-struct (oddsin 
		 :make-wrapper
		 (lambda (g)
		   (if (< (oddsin-n g) 1) (set! (oddsin-n g) 1))
		   (set! (oddsin-osc g) (make-oscil 
					 (oddsin-frequency g) 
					 (oddsin-initial-phase g)))
		   (set! (oddsin-nosc g) (make-oscil 
					  (* (oddsin-n g) (oddsin-frequency g)) 
					  (* (oddsin-n g) (oddsin-initial-phase g))))
		   (set! (oddsin-norm g) (if (= (oddsin-n g) 1) 1.0
					     (/ (if (= (oddsin-n g) 2) 1.29
						    (if (= (oddsin-n g) 3) 1.34
							(if (< (oddsin-n g) 6) 1.36
							    (if (< (oddsin-n g) 18) 1.37
								1.379))))
						(oddsin-n g))))
		   g))
  (frequency 440.0) 
  (initial-phase 0.0)
  (n 1 :type int)
  (osc #f :type clm) (nosc #f :type clm) (norm 0.0 :type float))

(define (oddsin gen fm)
  (declare (gen oddsin) (fm float))
  (let ((o1 (oscil (oddsin-nosc gen) (* (oddsin-n gen) fm)))
	(o2 (oscil (oddsin-osc gen) fm))) 
    (if (< (abs o2) 1.0e-9)
	0.0
	(/ (* (oddsin-norm gen) o1 o1) o2))))
	   
#|
;;; get normalization:
(do ((i 1 (1+ i))) 
    ((= i 30))
  (let ((v (with-sound (:output (make-vct 1000) :clipped #f)
		       (let ((gen (make-oddsin 40.0 :n i)))
			 (do ((k 0 (1+ k)))
			     ((= k 1000))
			   (outa k (oddsin gen 0.0) *output*))))))
    (snd-display "~A: ~A ~A" i (vct-peak v) (/ i (vct-peak v)))))
|#

#|
(with-sound (:clipped #f)
  (let ((gen (make-oddsin 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (oddsin gen 0.0) *output*))))))
|#


;;; what happens here if "n" changes?
;;; --------------------------------------------------------------------------------


