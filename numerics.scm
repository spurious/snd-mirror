(use-modules (ice-9 optargs) (ice-9 format))

(provide 'snd-numerics.scm)

;;; random stuff I needed at one time or another while goofing around...


(define factorial
  (let* ((num-factorials 128)
	 (factorials (let ((temp (make-vector num-factorials 0)))
		       (vector-set! temp 0 1) ; is this correct?
		       (vector-set! temp 1 1)
		       temp)))
    (lambda (n)
      (if (> n num-factorials)
	  (let ((old-num num-factorials)
		(old-facts factorials))
	    (set! num-factorials n)
	    (set! factorials (make-vector num-factorials 0))
	    (do ((i 0 (1+ i)))
		((= i old-num))
	      (vector-set! factorials i (vector-ref old-facts i)))))
      (if (zero? (vector-ref factorials n))
	  (vector-set! factorials n (* n (factorial (1- n)))))
      (vector-ref factorials n))))

(define (binomial-direct n m) ; "n-choose-m" might be a better name (there are much better ways to compute this -- see below)
  (/ (factorial n)
     (* (factorial m) (factorial (- n m)))))

(define (n-choose-k n k)
  "(n-choose-k n k) computes the binomial coefficient C(N,K)"
  (let ((mn (min k (- n k))))
    (if (< mn 0)
	0
	(if (= mn 0)
	    1
	    (let* ((mx (max k (- n k)))
		   (cnk (1+ mx)))
	      (do ((i 2 (1+ i)))
		  ((> i mn) cnk)
		(set! cnk (/ (* cnk (+ mx i)) i))))))))

(define binomial n-choose-k)



;;; from Numerical Recipes
(define (plgndr l m x)			;Legendre polynomial P m/l (x), m and l integer
					;0 <= m <= l and -1<= x <= 1 (x real)
  (if (or (< m 0) 
	  (> m l) 
	  (> (abs x) 1.0))
      (snd-error "invalid arguments to plgndr")
      (let ((pmm 1.0)
	    (fact 0.0) 
	    (somx2 0.0))
	(if (> m 0)
	    (begin
	      (set! somx2 (sqrt (* (- 1.0 x) (+ 1.0 x))))
	      (set! fact 1.0)
	      (do ((i 1 (1+ i)))
		  ((> i m))
		(set! pmm (* (- pmm) fact somx2))
		(set! fact (+ fact 2.0)))))
	(if (= l m) 
	    pmm
	    (let ((pmmp1 (* x pmm (+ (* 2 m) 1))))
	      (if (= l (+ m 1)) 
		  pmmp1
		  (let ((pk 0.0)) ; NR used "ll" which is unreadable
		    (do ((k (+ m 2) (1+ k)))
			((> k l))
		      (set! pk (/ (- (* x (- (* 2 k) 1) pmmp1) 
				      (* (+ k m -1) pmm)) 
				   (- k m)))
		      (set! pmm pmmp1)
		      (set! pmmp1 pk))
		    pk)))))))
