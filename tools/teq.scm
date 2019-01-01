;;; cyclic/shared timing tests

;;; equal? write/object->string/format cyclic-sequences

(define* (make-circular-list n init)
  (let ((l (make-list n init)))
    (set-cdr! (list-tail l (- n 1)) l)))

(define list-0 (list 1 2 3 4))
(define vect-0 (vector 1 2 3 4))
(define let-0 (inlet :a 1 :b 2))
(define hash-0 (hash-table :a 1 :b 2))

(define list-1 (make-circular-list 1))
(set-car! list-1 #t)
(define hash-1 (hash-table :a list-1))
(define vect-1 (vector list-1))
(define let-1 (inlet :a list-1))
(define list-2 (list list-1 list-1))
(define list-3 (make-circular-list 3))
(define vect-2 (let ((z (vector 1 2)))
		 (let ((y (list 1 z 2)))
		   (let ((x (hash-table 'x y)))
		     (set! (z 1) x)
		     z))))
(define vect-3 (let ((x '(1 2)))
		 (let ((y (list x x)))
		   (vector x y))))
(define vect-4 (let ((v (vector 1 2 3 4))
		     (lst (list 1 2)))
		 (set-cdr! (cdr lst) lst)
		 (set! (v 0) v)
		 (set! (v 3) lst)))
(define hash-2 (let ((h1 (make-hash-table 11)))
		 (hash-table-set! h1 "hi" h1)))
(define list-4 (let ()
		 (define make-node list)
		 (define prev (dilambda (lambda (node) (node 0)) (lambda (node val) (set! (node 0) val))))
		 (define next (dilambda (lambda (node) (node 2)) (lambda (node val) (set! (node 2) val))))
		 ;(define data (dilambda (lambda (node) (node 1)) (lambda (node val) (set! (node 1) val))))
		 (let* ((head (make-node () 0 ()))
			(cur head))
		   (do ((i 1 (+ i 1)))
		       ((= i 8))
		     (let ((next-node (make-node cur i ())))
		       (set! (next cur) next-node)
		       (set! cur (next cur))))
		   (set! (next cur) head)
		   (set! (prev head) cur)
		   head)))

(define let-2 (let ((hi 3))
		(let ((e (curlet)))
		  (set! hi (curlet)) 
		  e)))
(define let-3 (let ((e (inlet 'a 0 'b 1)))
		(let ((e1 (inlet 'a e)))
		  (set! (e 'b) e1)
		  e)))
(define let-4 (inlet :a vect-0 :b list-0))
(define hash-3 (hash-table :a vect-0 :b list-0))
(define hash-4 (hash-table :a hash-1))

(define-constant teq-vars (list list-0 list-1 list-2 list-3 list-4
			      vect-0 vect-1 vect-2 vect-3 vect-4
			      hash-0 hash-1 hash-2 hash-3 hash-4
			      let-0 let-1 let-2 let-3 let-4))

;(format *stderr* "~A ~A ~A ~A ~A~%" (length hash-0) (length hash-1) (length hash-2) (length hash-3) (length hash-4))

(set! (*s7* 'initial-string-port-length) 64)
#|
(define (tests size)
  (let ((str #f)
	(p (open-output-string))
	(iter #f))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! iter (make-iterator teq-vars))
      (do ((j 0 (+ j 1))
	   (vj (iterate iter) (iterate iter)))
	  ((= j 20))
	(do ((k 0 (+ k 1)))
	    ((= k 20))
	  (if (equal? vj (vector-ref teq-vars k))
	      (if (not (= j k))
		  (format *stderr* "oops! (~D ~D): ~A ~A~%" j k vj (vector-ref teq-vars k)))))
	(write vj p)
	(set! str (get-output-string p #t))
	(set! str (object->string vj))
	(set! str (format #f "~A~%" vj))
	(set! str (cyclic-sequences vj))))
    (close-output-port p)))
|#

(define (tests size)
  (let ((str #f)
	(vj #f)
	(p (open-output-string)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (do ((a teq-vars (cdr a)))
	  ((null? a))
	(set! vj (car a))
	(do ((b teq-vars (cdr b)))
	    ((null? b))
	  (if (equal? vj (car b))
	      (if (not (eq? a b))
		  (format *stderr* "oops!: ~A ~A~%"  a b))))
	(write vj p)
	(set! str (get-output-string p #t))
	(set! str (object->string vj))
	(set! str (format #f "~A~%" vj))
	(set! str (cyclic-sequences vj))))
    (close-output-port p)))
#|
;; almost as fast
(define (tests size)
  (let ((str #f)
	(p (open-output-string))
	(k 0) (j 0))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! k 0)
      (for-each 
       (lambda (vj)
	 (set! j 0)
	 (for-each 
	  (lambda (w)
	    (if (equal? vj w)
		(if (not (= j k))
		    (format *stderr* "oops! (~D ~D): ~A ~A~%" j k vj w)))
	    (set! j (+ j 1)))
	  teq-vars)
	 (set! k (+ k 1))
	 (write vj p)
	 (set! str (get-output-string p #t))
	 (set! str (object->string vj))
	 (set! str (format #f "~A~%" vj))
	 (set! str (cyclic-sequences vj)))
       teq-vars))
    (close-output-port p)))
|#


(tests 10000)

(s7-version)
(exit)



	
