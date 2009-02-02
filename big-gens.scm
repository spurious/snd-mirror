(provide 'snd-big-gens.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))


(define (big-hz->radians hz)
  (/ (* hz 2 pi)
     (mus-srate)))

(define (big-radians->hz rad)
  (/ (* rad (mus-srate))
     (* 2 pi)))

(define (big-db->linear x)
  (expt 10.0 (/ x 20.0)))

(define (big-linear->db x)
  (if (> x 0.0)
      (* 20.0 (log x 10))
      -100.0))

;;; -------- polynomial --------
(define (big-polynomial coeffs x)
  (let* ((top (- (vector-length coeffs) 1))
	 (sum (vector-ref coeffs top)))
    (do ((i (- top 1) (- i 1)))
	((< i 0) sum)
      (set! sum (+ (* x sum)
		   (vector-ref coeffs i))))))


;;; -------- oscil --------
(defgenerator (big-oscil 
  :make-wrapper 
    (lambda (g)
      (set! (big-oscil-frequency g) (big-hz->radians (big-oscil-frequency g)))
      g))
  (frequency *clm-default-frequency*) (angle 0.0))

(define* (big-oscil gen :optional (fm 0.0) (pm 0.0))
  (declare (gen big-oscil) (fm float))
  (let ((x (big-oscil-angle gen)))
    (set! (big-oscil-angle gen) (+ fm x (big-oscil-frequency gen)))
    (sin (+ x pm))))


;;; -------- ncos --------
(defgenerator (big-ncos
  :make-wrapper
   (lambda (g)
     (if (<= (big-ncos-n g) 0)
	 (set! (big-ncos-n g) 1))
     (set! (big-ncos-r g) (/ (big-ncos-n g)))
     (set! (big-ncos-frequency g) (big-hz->radians (big-ncos-frequency g)))
     g))
   (frequency *clm-default-frequency*) (n 1 :type int) (angle 0.0) (r 1.0))

(define* (big-ncos gen :optional (fm 0.0))
  (declare (gen big-ncos) (fm float))
  (let* ((n (big-ncos-n gen))
	 (x (big-ncos-angle gen))
	 (scl (big-ncos-r gen))
	 (den (* 2 (sin (/ x 2)))))
    (set! (big-ncos-angle gen) (+ fm x (big-ncos-frequency gen)))
    (if (= den 0.0)
	1.0
	(min 1.0 (* scl (- (/ (sin (* (+ n 1/2) x)) den) 1/2))))))



;;; TODO: rest of big-gens?

