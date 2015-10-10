(let ((new-env (sublet (curlet) (cons 'init_func 'block_init)))) ; load calls init_func if possible
  (load "s7test-block.so" new-env))

(define (test-copy size)
  (let ((old-string (make-string size #\a))
	(old-bvect (make-byte-vector size 1))
	(old-pair (make-list size #\a))
	(old-vector (make-vector size #\a))
	(old-vectorf (make-vector size 1.0))
	(old-vectori (make-vector size 1))
	(old-fvect (make-float-vector size 1.0))
	(old-ivect (make-int-vector size 1))
	(old-hash (make-hash-table size))
	(old-let #f)
	(old-block (make-block size)))
    (set! old-let (inlet))

    (do ((i 0 (+ i 1)))
	((= i size))
      (hash-table-set! old-hash i #\a))
    (do ((i 0 (+ i 1)))
	((= i size))
      (varlet old-let (string->symbol (number->string i)) #\a))

    (let ((new-string (make-string size #\space))
	  (new-bvect (make-byte-vector size 0))
	  (new-pair (make-list size 1))
	  (new-vector (make-vector size 1))
	  (new-fvect (make-float-vector size 1.0))
	  (new-ivect (make-int-vector size 1))
	  (new-hash (make-hash-table (* size 2)))
	  (new-let (inlet))
	  (new-block (make-block size)))
      
      (copy old-string)
      (copy old-pair)
      (copy old-vector)
      (copy old-fvect)
      (copy old-ivect)
      (copy old-hash)
      (copy old-let)
      (copy old-bvect)
      (copy old-block)
      
      (length old-string)
      (length old-pair)
      (length old-vector)
      (length old-fvect)
      (length old-ivect)
      (length old-hash)
      (length old-let)
      (length old-bvect)
      (length old-block)
      
      (fill! old-string #\a)
      (fill! new-string #\space)
      (fill! old-pair #\a)
      (fill! new-pair 1)
      (fill! old-vector #\a)
      (fill! new-vector 1)
      (fill! old-fvect 1.0)
      (fill! new-fvect 1.0)
      (fill! old-ivect 1)
      (fill! new-ivect 1)
      (fill! old-bvect 1)
      (fill! new-bvect 0)
      (fill! old-block 0.0)
      (fill! new-block 1.0)
      
      (copy old-string new-string)
      (copy old-vector new-string)
      (copy old-pair new-string)
      (copy old-bvect new-string)
      
      (copy old-bvect new-bvect)
      (copy old-ivect new-bvect)
      (copy old-vectori new-bvect)
      (copy old-string new-bvect)
      
      (copy old-pair new-pair)
      (copy old-string new-pair)
      (copy old-vector new-pair)
      (copy old-hash new-pair)
      (copy old-fvect new-pair)
      (copy old-ivect new-pair)
      (copy old-let new-pair)
      (copy old-block new-pair)
      
      (copy old-vector new-vector)
      (copy old-pair new-vector)
      (copy old-string new-vector)
      (copy old-fvect new-vector)
      (copy old-ivect new-vector)
      (copy old-hash new-vector)
      (copy old-let new-vector)
      (copy old-block new-vector)
      
      (copy old-fvect new-fvect)
      (copy old-ivect new-fvect)
      (copy old-vectorf new-fvect)
      (copy old-block new-fvect)
      
      (copy old-ivect new-ivect)
      (copy old-fvect new-ivect)
      (copy old-vectori new-ivect)
      (copy old-bvect new-ivect)
      
      (copy old-hash new-hash)
      (copy old-let new-hash)
      
      (copy old-let new-let)
      
      (copy old-fvect new-block)
      (copy old-block new-block))
    
    (let ((nsize (/ size 2))
	  (start (/ size 4)))
      (let ((new-string (make-string size #\space))
	    (new-pair (make-list size 1))
	    (new-vector (make-vector size 1))
	    (new-fvect (make-float-vector size 1.0))
	    (new-ivect (make-int-vector size 1))
	    (new-hash (make-hash-table (* size 2)))
	    (new-let (inlet))
	    (new-bvect (make-byte-vector size 255))
	    (new-block (make-block size)))
	
	(copy old-string new-string start (+ start nsize))
	(copy old-vector new-string start (+ start nsize))
	(copy old-pair new-string start (+ start nsize))
	
	(copy old-bvect new-bvect start (+ start nsize))
	(copy old-vectori new-bvect start (+ start nsize))
	(copy old-ivect new-bvect start (+ start nsize))
	(copy old-string new-bvect start (+ start nsize))
	
	(copy old-pair new-pair start (+ start nsize))
	(copy old-string new-pair start (+ start nsize))
	(copy old-vector new-pair start (+ start nsize))
	(copy old-hash new-pair start (+ start nsize))
	(copy old-fvect new-pair start (+ start nsize))
	(copy old-ivect new-pair start (+ start nsize))
	(copy old-let new-pair start (+ start nsize))
	(copy old-block new-pair start (+ start nsize))
	
	(copy old-vector new-vector start (+ start nsize))
	(copy old-pair new-vector start (+ start nsize))
	(copy old-string new-vector start (+ start nsize))
	(copy old-fvect new-vector start (+ start nsize))
	(copy old-ivect new-vector start (+ start nsize))
	(copy old-hash new-vector start (+ start nsize))
	(copy old-let new-vector start (+ start nsize))
	(copy old-block new-vector start (+ start nsize))
	
	(copy old-fvect new-fvect start (+ start nsize))
	(copy old-ivect new-fvect start (+ start nsize))
	(copy old-vectorf new-fvect start (+ start nsize))
	(copy old-block new-fvect start (+ start nsize))
	
	(copy old-ivect new-ivect start (+ start nsize))
	(copy old-fvect new-ivect start (+ start nsize))
	(copy old-vectori new-ivect start (+ start nsize))
	(copy old-bvect new-ivect start (+ start nsize))
	
	(copy old-hash new-hash start (+ start nsize))
	(copy old-let new-hash start (+ start nsize))
	
	(copy old-let new-let start (+ start nsize))
	
	(copy old-fvect new-block start (+ start nsize))
	(copy old-block new-block start (+ start nsize))))
    
    (reverse old-string)
    (reverse old-pair)
    (reverse old-vector)
    (reverse old-fvect)
    (reverse old-ivect)
    (reverse old-hash)
    (reverse old-bvect)
    (reverse old-block)
    
    (reverse! old-string)
    (reverse! old-pair)
    (reverse! old-vector)
    (reverse! old-fvect)
    (reverse! old-ivect)
    (reverse! old-bvect)
    (reverse! old-block)
    ))

(define-expansion (test tst expected)
  `(let ((val ,tst))
     (if (not (equal? val ,expected))
	 (format *stderr* "~S: ~S but expected ~S~%" ',tst val ,expected))))

(define (test-append size)
  (let ((strs ())
	(vecs ())
	(fvecs ())
	(ivecs ())
	(ifvecs ())
	(allvecs ())
	(bvecs ())
	(lsts ()))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! strs (cons (make-string size (integer->char (+ 1 (random 255)))) strs))
      (set! bvecs (cons (->byte-vector (make-string size (integer->char (random 256)))) bvecs))
      (set! vecs (cons (make-vector size i) vecs))
      (set! ivecs (cons (make-int-vector size i) ivecs))
      (set! fvecs (cons (make-float-vector size (* i 1.0)) fvecs))
      (set! ifvecs (cons (make-vector size (if (even? i) (* i 1.0) i) #t) ifvecs))
      (set! allvecs (cons (make-vector size (if (even? i) (* i 1.0) i) (not (zero? (modulo i 3)))) allvecs))
      (set! lsts (cons (make-list size i) lsts)))
    (let ((lst (apply append lsts))
	  (vec (apply vector-append vecs))
	  (fvec (apply vector-append fvecs))
	  (ivec (apply vector-append ivecs))
	  (ifvec (apply vector-append ifvecs))
	  (allvec (apply vector-append allvecs))
	  (str (apply string-append strs))
	  (bvec (->byte-vector (apply string-append bvecs))))
      (test (vector? vec) #t)
      (test (length vec) (* size size))
      (test (float-vector? fvec) #t)
      (test (length fvec) (* size size))
      (test (int-vector? ivec) #t)
      (test (length ivec) (* size size))
      (test (vector? allvec) #t)
      (test (length allvec) (* size size))
      (test (vector? ifvec) #t)
      (test (length ifvec) (* size size))
;      (test (float-vector? ifvec) #t)
;      (test (int-vector? ifvec) #f)
      (test (pair? lst) #t)
      (test (length lst) (* size size))
      (test (string? str) #t)
      (test (length str) (* size size))
      (test (byte-vector? bvec) #t)
      (test (length bvec) (* size size))
      )))
      

(define (t)
  (do ((i 0 (+ i 1)))
      ((= i 10000))
    (test-copy 100))
  (do ((i 1 (* i 10)))
      ((> i 1000))
    (test-append i)))

(t)

(s7-version)
(exit)
