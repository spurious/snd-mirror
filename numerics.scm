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


;;; A&S (bessel.lisp)
(define (legendre-polynomial a x) ; sum of weighted polynomials (m=0)
  (let ((n (1- (vector-length a))))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r x)
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (1+ k)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (/ (- (* r x (+ (* 2 k) 1))  (* s k)) (+ k 1)))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

#|
;; (legendre-polynomial (vector ... 1.0[at i] ...) x) = (plgndr i 0 x)
(do ((i 0 (1+ i))) 
    ((= i 10))
   (let ((lv (legendre-polynomial (let ((v (make-vector 10 0.0))) 
				    (vector-set! v i 1.0) 
				    v) 
				  0.5))
	 (pv (plgndr i 0 0.5)))
     (if (fneq lv pv)
	 (snd-display ";lv: ~A, pv: ~A (~A)" lv pv i))))

(define (pow-x pow x)
  ;; A&S p798
  (if (= pow 0)
      (legendre-polynomial (vector 1) x)
      (if (= pow 1)
	  (legendre-polynomial (vector 0 1) x)
	  (if (= pow 2)
	      (* (/ 1.0 3.0) (legendre-polynomial (vector 1 0 2) x))
	      (if (= pow 3)
		  (* (/ 1.0 5.0) (legendre-polynomial (vector 0 3 0 2) x))
		  (if (= pow 4)
		      (* (/ 1.0 35.0) (legendre-polynomial (vector 7 0 20 0 8) x))
		      (if (= pow 5)
			  (* (/ 1.0 63.0) (legendre-polynomial (vector 0 27 0 28 0 8) x))
			  (if (= pow 6) 
			      (* (/ 1.0 231.0) (legendre-polynomial (vector 33 0 110 0 72 0 16) x))
			      'oops))))))))
(for-each
 (lambda (x)
   (for-each
    (lambda (pow)
      (let ((lv (pow-x pow x))
	    (sv (expt x pow)))
	(if (fneq lv sv)
	    (snd-display ";~A ^ ~A = ~A ~A?" x pow lv sv))))
    (list 0 1 2 3 4 5 6)))
 (list 2.0 0.5 0.1 -0.5 3.0 0.8))

;; if l odd, there seems to be sign confusion:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (1+ i))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (outa i (plgndr 1 1 (cos theta)) *output*)
    (let ((x (sin theta)))
      (outb i (- x) *output*))))

;; this works:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (1+ i))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (let ((x (cos theta)))
      (outa i (plgndr 3 0 x) *output*)
      (outb i (* 0.5 x (- (* 5 x x) 3)) *output*))))
|#

;;; PERHAPS: cheb 4th as part of waveshaper?
;;; PERHAPS: sum-of-cosines-bank?
;;; TODO: add the div close to 0 check to dsp.scm sum-of-sines stuff
;;; PERHAPS: use correlate + lisp grf to show correlation from cursor and 0 