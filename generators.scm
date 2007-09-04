(load "t.scm")


;;; sndclm.html G&R 2nd col last row (with normalization)

(def-clm-struct (expcs
		 :make-wrapper (lambda (g)
				 (set! (expcs-osc g) (make-oscil (expcs-frequency g) (expcs-initial-phase g)))
				 (if (<= (expcs-t g) 0.0) (set! (expcs-t g) 0.00001))
				 (set! (expcs-cosh-t g) (cosh (expcs-t g)))
				 (let ((exp-t (exp (- (expcs-t g)))))
				   (set! (expcs-offset g) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
				   (set! (expcs-scaler g) (* (sinh (expcs-t g)) (expcs-offset g))))
				 g))
  (osc #f :type clm) scaler offset t cosh-t 
  (frequency 440.0) (initial-phase 0.0)) ; last 2 for make-oscil

(define (expcs gen fm)
  (declare (gen expcs) (fm float))
  (- (/ (expcs-scaler gen) 
	(- (expcs-cosh-t gen) (oscil (expcs-osc gen) fm)))
     (expcs-offset gen)))


#|
(with-sound (:clipped #f)
  (let ((gen (make-expcs :frequency 100 :t 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (expcs gen 0.0) *output*))))))


;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:clipped #f)  
  (let ((gen (make-expcs :frequency 100 :t 0.1))
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

(def-clm-struct (oddsin 
		 :make-wrapper (lambda (g)
				 (set! (oddsin-osc g) (make-oscil (oddsin-frequency g) (oddsin-initial-phase g)))
				 (set! (oddsin-nosc g) (make-oscil (* (oddsin-n g) (oddsin-frequency g)) (* (oddsin-n g) (oddsin-initial-phase g))))
				 g))
  (osc #f :type clm) (nosc #f :type clm) (n 0 :type int)
  (frequency 440.0) (initial-phase 0.0))

(define (oddsin gen fm)
  (declare (gen oddsin) (fm float))
  (let ((o1 (oscil (oddsin-nosc gen) (* (oddsin-n gen) fm))))
    (/ (* o1 o1)
       (oscil (oddsin-osc gen) fm))))

;;; amplitude normalization is approximately (/ 1.4 n)

#|
(with-sound (:clipped #f)
  (let ((gen (make-oddsin :frequency 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (oddsin gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------


