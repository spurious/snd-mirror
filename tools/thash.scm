;(set! (*s7* 'gc-stats) 6)
;heap ca 30*size!

(define-constant symbols (make-vector 1000000))
(define-constant strings (make-vector 1000000))

(define (test-hash size)

  (format *stderr* "~D " size)

  (let ((int-hash (make-hash-table size))
	(p (cons #f #f)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (hash-table-set! int-hash i i))
    (do ((i 0 (+ i 1)))	
	((= i size))
      (if (not (= (hash-table-ref int-hash i) i))
	  (display "oops")))
    (for-each (lambda (key&value)
		(if (not (= (car key&value) (cdr key&value)))
		    (display "oops"))) ;(format *stderr* "hash iter ~A~%" key&value)))
	      (make-iterator int-hash p))
    (fill! int-hash #f))

  (let ((int-hash (make-hash-table size =)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (hash-table-set! int-hash i i))
    (do ((i 0 (+ i 1)))	
	((= i size))
      (if (not (= (hash-table-ref int-hash i) i))
	  (display "oops")))
    (fill! int-hash #f))


  (let ((flt-hash (make-hash-table size)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (hash-table-set! flt-hash (* i 2.0) i))
    (do ((i 0 (+ i 1)))	
	((= i size))
      (if (not (= (hash-table-ref flt-hash (* 2.0 i)) i))
	  (display "oops")))
    (fill! flt-hash #f))


  (let ((sym-hash (make-hash-table size)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (hash-table-set! sym-hash (vector-set! symbols i (string->symbol (vector-set! strings i (number->string i)))) i))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (if (not (= (hash-table-ref sym-hash (vector-ref symbols i)) i)) 
	  (display "oops")))
    (fill! sym-hash #f))


  (let ((str-hash (make-hash-table size eq?)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (hash-table-set! str-hash (vector-ref strings i) i))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (if (not (= (hash-table-ref str-hash (vector-ref strings i)) i)) 
	  (display "oops")))
    (fill! str-hash #f))


  (let ((sym-hash (make-hash-table size eq?)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (hash-table-set! sym-hash (vector-ref symbols i) i))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (if (not (= (hash-table-ref sym-hash (vector-ref symbols i)) i)) 
	  (display "oops")))
    (fill! sym-hash #f))


  (let ((chr-hash (make-hash-table 256)))
    (do ((i 0 (+ i 1))) 
	((= i 256)) 
      (hash-table-set! chr-hash (integer->char i) i))
    (do ((i 0 (+ i 1))) 
	((= i 256)) 
      (if (not (= (hash-table-ref chr-hash (integer->char i)) i))
	  (display "oops")))
    (fill! chr-hash #f))


  (let ((any-hash (make-hash-table size eq?)))
    (if (= size 1)
	(hash-table-set! any-hash (vector-set! strings 0 (list 0)) 0)
	(do ((i 0 (+ i 2))
	     (j 1 (+ j 2)))
	    ((= i size))
	  (hash-table-set! any-hash (vector-set! strings i (list i)) i)
	  (hash-table-set! any-hash (vector-set! strings j (int-vector j)) j)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (if (not (= i (hash-table-ref any-hash (vector-ref strings i))))
	  (display "oops")))
    (fill! any-hash #f))


  (let ((any-hash1 (make-hash-table size eq?)))
    (if (= size 1)
	(hash-table-set! any-hash1 (vector-set! strings 0 (inlet :a 0)) 0)
	(do ((i 0 (+ i 2))
	     (j 1 (+ j 2))
	     (x 0.0 (+ x 2.0)))
	    ((= i size))
	  (hash-table-set! any-hash1 (vector-set! strings i (inlet :a i)) i)
	  (hash-table-set! any-hash1 (vector-set! strings j (float-vector x)) j)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (if (not (= i (hash-table-ref any-hash1 (vector-ref strings i))))
	  (display "oops")))
    (vector-fill! strings #f)
    (fill! any-hash1 #f))


  (let ((cmp-hash (make-hash-table size)))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (hash-table-set! cmp-hash (make-rectangular i i) i))
    (do ((i 0 (+ i 1))) 
	((= i size)) 
      (if (not (= (hash-table-ref cmp-hash (make-rectangular i i)) i)) 
	  (display "oops")))
    (fill! cmp-hash #f))

  )


(for-each test-hash (list 1 10 100 1000 10000 100000 1000000))

;;; ----------------------------------------
(format *stderr* "reader~%")

(define data "/home/bil/test/bench/src/bib")
(define counts (make-hash-table (expt 2 18) string=?))

(define (reader)
  (let ((port (open-input-file data))
	(new-pos 0))
    (do ((line (read-line port) (read-line port)))
	((eof-object? line))
      (set! new-pos 0)
      (do ((pos (char-position #\space line 0) (char-position #\space line (+ pos 1))))
	  ((not pos))
	(unless (= pos new-pos)
	  (let* ((start (do ((k new-pos (+ k 1))) ; char-position here is slower!
			    ((or (= k pos)
				 (char-alphabetic? (string-ref line k)))
			     k)))
		 (end (do ((k (- pos 1) (- k 1)))
			  ((or (= k start)
			       (char-alphabetic? (string-ref line k)))
			   (+ k 1)))))
	    (when (> end start)
	      (let ((word (substring line start end)))
		(let ((refs (or (hash-table-ref counts word) 0)))
		  (hash-table-set! counts word (+ refs 1)))))))
	(set! new-pos (+ pos 1))))

    (close-input-port port)
    (sort! (copy counts (make-vector (hash-table-entries counts))) 
	   (lambda (a b) (> (cdr a) (cdr b))))))

(set! counts (reader))

(if (or (not (string=? (car (counts 0)) "the"))
	(not (= (cdr (counts 0)) 62063)))
    (do ((i 0 (+ i 1))) 
	((= i 40)) 
      (format *stderr* "~A: ~A~%" (car (counts i)) (cdr (counts i)))))
;;; ----------------------------------------

;(gc)
(s7-version)

(exit)
