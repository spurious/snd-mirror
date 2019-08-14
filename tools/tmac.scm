(define size 500000)

(define-macro (m2 a b) `(+ ,a ,@b 1))
(define (f2)
  (let ((x 2)
	(y 0))
    (do ((j 0 (+ j 1)))
	((= j 1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! y (m2 x (x x)))
	(if (not (= y (+ (* 3 x) 1)))
	    (format *stderr* "y: ~A~%" y))))))

(f2)


(define-expansion (m3 a b) `(+ ,a ,@b 1))
(define (f3)
  (let ((x 2)
	(y 0))
    (do ((j 0 (+ j 1)))
	((= j 1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! y (m3 x (x x)))
	(if (not (= y (+ (* 3 x) 1)))
	    (format *stderr* "y: ~A~%" y))))))
(f3)


(define (f4 m) (+ 2 (m 3)))
(define (f4-test mx)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (f4 mx)))

(define-macro (m4 a) `(+ ,a 1))
(f4-test m4)

(define-macro (m5 a . b) `(+ ,a ,@b))
(f4-test m5)

(define-macro* (m6 (a 21)) `(+ ,a 1))
(f4-test m6)

(define (f5-test)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (m5 1 3 4 5)))
(f5-test)

(define-macro (m6 a b) `(+ ,a ,@b))
(define (f6-test mx)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (mx 1 (3 4 5))))
(f6-test m6)

(define (f7-test mx)
  (do ((i 0 (+ i 1)))
      ((= i size))
    (mx 1 ())))
(f7-test m6)

(define (f8-test mx)
  (let loop ((ctr size))
    (mx 1 3 4 5)
    (if (= ctr 0)
	0
	(loop (- ctr 1)))))
(f8-test m5)

(exit)
