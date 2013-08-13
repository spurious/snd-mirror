(provide 'write.scm)

;;; write-readably: write object to port in such a way that a subsequent (eval (read port)) returns the same object
;;; pretty-print:   the usual pretty printer, intended for s7
;;; checkpoint:     use write-readably to save the current runtime state of s7 as a loadable scheme file




;;; -------------------------------- write-readably --------------------------------

(define* (write-readably obj (port (current-output-port)))

  ;; I'm currently moving this into s7.c using :readable as 2nd object->string arg, or format with ~W

  (define (replacement obj info)
    (let ((data (assq obj info)))
      (and data (cadr data))))

  (define (needs-definition? obj info)
    (let ((data (assq obj info)))
      (and data (not (caddr data)))))

  (define (circle-length obj lst init)
    (if (eq? lst obj)
	init
	(circle-length obj (cdr lst) (+ init 1))))

  (define (write-readably-1 obj port structs)

    (cond ((pair? obj)
	   (let ((len (length obj)))

	     ;; dotted list
	     (if (negative? len)
		 (begin
		   (do ((lst obj (cdr lst)))
		       ((not (pair? lst))
			(write-readably-1 lst port structs))
		     (format port "(cons ")
		     (write-readably-1 (car lst) port structs)
		     (if (pair? lst) (format port " ")))
		   (do ((i 0 (+ i 1)))
		       ((= i (- len)))
		     (format port ")")))

		 ;; circular list
		 (if (infinite? len)
		     (let ((sym (replacement obj structs))
			   (define? (needs-definition? obj structs)))
		       ;; (wr (let ((lst (list 1 2 3))) (set! (cdr (cdr lst)) lst) lst))

		       (if define?
			   (let ((clen (circle-length obj (cdr obj) 1)))
			     (format port "(let () (set! ~A (list" sym)
			     (do ((i 0 (+ i 1)))
				 ((= i clen))
			       (format port " ~S" (obj i)))
			     (format port " #f)) (set-cdr! (list-tail ~A ~D) ~A) ~A" sym (- clen 1) sym sym)
			     (let ((data (assq obj structs)))
			       (set-car! (cddr data) #t)))
			   (format port "~A" sym)))

		     ;; normal list
		     (begin
		       (format port "(list")
		       (do ((lst obj (cdr lst)))
			   ((not (pair? lst)))
			 (format port " ")
			 (write-readably-1 (car lst) port structs))
		       (format port ")"))))))
	  
	  ((vector? obj)
	   (let ((dims (vector-dimensions obj))
		 (len (length obj)))
	     (if (= (length dims) 1)
		 (begin
		   (format port "(vector")
		   (do ((i 0 (+ i 1)))
		       ((= i len)
			(format port ")"))
		     (format port " ")
		     (write-readably-1 (obj i) port structs)))
		 
		 ;; if self-reference, just like above but subsititute #f and save index if ref found, then at end do the sets
		 
		 (let ((indices (make-list (length dims) 0)))
		   (format port "(let ((v (make-vector '~A)))" dims) ; do we need a gensym here?
		   (do ((i 0 (+ i 1)))
		       ((= i len))
		     (format port " (set! (v ~{~A~^ ~}) " indices)
		     (let ((nobj (apply obj indices)))
		       (if (eq? nobj obj)
			   (format port "v")
			   (write-readably-1 nobj port structs)))
		     (format port ")")
		     (call-with-exit
		      (lambda (done)
			(do ((k (- (length indices) 1) (- k 1)))
			    ((< k 0))
			  (set! (indices k) (+ (indices k) 1))
			  (if (= (indices k) (dims k))
			      (set! (indices k) 0)
			      (done))))))
		   (format port " v)")))))
	  
	  ((hash-table? obj)
	   (let ((iter (make-hash-table-iterator obj))
		 (len (length obj)))
	     (format port "(let ((ht (make-hash-table ~D)))" len)
	     (do ((key&value (iter) (iter)))
		 ((null? key&value)
		  (format port " ht)"))
	       (format port " (set! (ht ")
	       (let ((key (car key&value))
		     (val (cdr key&value)))
		 (if (eq? key obj)
		     (format port "ht")
		     (write-readably-1 key port structs))
		 (format port ") ")
		 (if (eq? val obj)
		     (format port "ht")
		     (write-readably-1 val port structs)))
	       (format port ")"))))
	  
	  ((environment? obj)
	   (let ((lst (environment->list obj)))
	     (format port "(environment")
	     (for-each
	      (lambda (kv)
		(format port " (cons ")
		(write-readably-1 (car kv) port structs)
		(format port " " )
		(write-readably-1 (cdr kv) port structs)
		(format port ")"))
	      lst)
	     (format port ")")))
	  
	  ((procedure? obj)
	   (let ((f (procedure-source obj))
		 (e (procedure-environment obj))
		 (elist #f))
	     
	     (define (env-memq e ce)
	       (and ce
		    (not (eq? ce (global-environment)))
		    (or (eq? e ce)
			(env-memq e (outer-environment ce)))))
	     
	     (define (tree-env-match source envir)
	       (if (pair? source)
		   (or (tree-env-match (car source) envir)
		       (tree-env-match (cdr source) envir))
		   (and (symbol? source)
			(member source 
				(or elist
				    (set! elist (environment->list envir)))
				(lambda (a b) (eq? a (car b)))))))
	     
	     (let ((e-ok (or (null? f)
			     (eq? e (global-environment))
			     (env-memq e (current-environment))
			     (not (tree-env-match f e)))))
	       (if (not e-ok)
		   (begin
		     ;; (write-readably (let ((a 1)) (lambda (b) (+ a b)))) -> (let ((a 1)) (lambda (b) (+ a b)))
		     
		     ;; TODO: restrict to used vars, and climb the entire chain
		     
		     (format port "(let (")
		     (for-each 
		      (lambda (slot)
			(format port "(~A ~A)" (car slot) (cdr slot)))
		      elist)
		     (format port ") ")))
	       
	       (if (pair? f)
		   (write f port)
		   (write obj port))
	       
	       (if (not e-ok)
		   (format port ")")))))
	  
	  ((macro? obj)
	   (let* ((source (procedure-source obj))
		  (arglist (cadar (cdaddr source)))
		  (body (cddar (cdaddr source)))
		  (starred (eq? (caar (cdaddr source)) 'lambda*)))
	     (format port "(symbol->value (define-~Aacro~A (_m_" (if (bacro? obj) "b" "m") (if starred "*" ""))
	     (if (symbol? arglist)
		 (format port " . ~A" arglist)
		 (if (not (null? arglist))
		     (begin
		       (format port " ")
		       (for-each (lambda (p) (write p port)) arglist))))
	     (format port ") ")
	     (for-each (lambda (p) (write p port)) body)
	     (format port "))")))
	  
	  ((input-port? obj)
	   (if (eq? obj *stdin*)
	       (format port "*stdin*")
	       (if (port-closed? obj)
		   (format port "(call-with-input-string \"\" (lambda (p) p))")
		   (error 'write-readably "can't write ~A readably" obj))))
	  
	  ((output-port? obj)
	   (if (eq? obj *stdout*)
	       (format port "*stdout*")
	       (if (eq? obj *stderr*)
		   (format port "*stderr*")
		   (if (port-closed? obj)
		       (format port "(let ((p #f)) (call-with-output-string (lambda (np) (set! p np))) p)")
		       (error 'write-readably "can't write ~A readably" obj)))))
	  
	  ;; if open/file: ftell for loc, then lseek to return, need also the mode if write
	  ;;    but input file if internal string has these numbers already and can be reset to that place in C
	  ;;    so how to save/restore here?
	  ;;    (file-position f) settable?
	  ;; in the others, no hope I think -- can there be a global string port? yes but it's closed

	  ((raw-pointer? obj)
	   (error 'write-readably "can't write ~A readably" obj))

	  (else 
	   ;; it's hard to catch errors here because things like #<eof> come through this branch
	   (let ((str (object->string obj :readable)))
	     (display str port)))))
  

  ;; main function
  (let ((structs ()) ; (collect-recurrent-structures obj))  ; watch out for circles, etc
	(info ()))
    
    ;; if structs, change it into an alist: ((orig-obj . new-name) ...)
    (if (not (null? structs))
	(begin
	  (format port "(let (")
	  (for-each
	   (lambda (orig-obj)
	     (let ((new-sym (gensym "wr")))
	       (set! info (cons (list orig-obj new-sym #f) info))
	       (format port "(~A #f)" new-sym)))
	   structs)
	  (format port ")")))
    (write-readably-1 obj port info)
    (if (not (null? structs))
	(format port "))"))
    ))

;;; (wr (let ((lst (list 1 2 3))) (set! (cdr (cdr lst)) lst) lst))
;;;   ;structs: (#1=(1 2 . #1#))

(define (wr obj)
  (with-output-to-string
    (lambda ()
      (write-readably obj))))


(define (test-write-readably)
  ;; special things
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (not (eq? n obj))
	     (format *stderr* "~A not eq? ~A (~S)~%" n obj str)))))
   (list #<eof> #<undefined> #<unspecified> #t #f #true #false else ()
	 lambda lambda* begin case if do quote set! let let* letrec
	 cond and or define define* define-constant define-macro
	 defmacro define-macro* define-bacro define-bacro*
	 with-baffle with-environment
	 *stdin* *stdout* *stderr*
	 ))
  
  ;; characters
  
  (do ((i 0 (+ i 1)))
      ((= i 256))
    (let ((c (integer->char i)))
      (let ((str (with-output-to-string
		   (lambda ()
		     (write-readably c)))))
	(let ((nc (with-input-from-string str
		    (lambda ()
		      (eval (read)))))) ; no need for eval here or in some other cases, but might as well be consistent
	  (if (not (eq? c nc))
	      (format *stderr* "~C (~D) != ~C (~S)~%" c i nc str))))))
  
  ;; integers
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((nn (with-input-from-string str
		   (lambda ()
		     (eval (read))))))
	 (if (or (not (integer? n))
		 (not (integer? nn))
		 (not (= n nn)))
	     (format *stderr* "~D != ~D (~S)~%" n nn str)))))
   (list 0 1 3 most-positive-fixnum -0 -1 -3 most-negative-fixnum
	 -9223372036854775808 9223372036854775807
	 ))
  
  ;; but unless gmp at read end we'll fail with most-positive-fixnum+1
  ;; -> check *features* at start of read
  
  ;; ratios
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((nn (with-input-from-string str
		   (lambda ()
		     (eval (read))))))
	 (if (or (not (rational? n))
		 (not (rational? nn))
		 (not (= n nn)))
	     (format *stderr* "~A != ~A (~S)~%" n nn str)))))
   (list 1/2 -1/2 123456789/2 -2/123456789 2147483647/2147483646 312689/99532
	 -9223372036854775808/3 9223372036854775807/2  1/1428571428571429 1/1152921504606846976
	 ))
  
  ;; reals
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((nn (with-input-from-string str
		   (lambda ()
		     (eval (read))))))
	 (if (or (not (real? n))
		 (not (real? nn))
		 (not (morally-equal? n nn)))
	     (format *stderr* "~A != ~A (~S)~%" n nn str)))))
   (list 1.0 0.0 -0.0 pi 0.1 -0.1 0.9999999995 9007199254740993.1 (sqrt 2) 1/100000000000
	 1.5e-16 1.5e16 3.141592653589793238462643383279502884197169399375105820 1e-300 8.673617379884e-19
	 1/0 (- 1/0) (real-part (log 0)) (- (real-part (log 0)))
	 ))
  
  ;; complex
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((nn (with-input-from-string str
		   (lambda ()
		     (eval (read))))))
	 (if (or (not (complex? n))
		 (not (complex? nn))
		 (not (morally-equal? n nn)))
	     (format *stderr* "~A != ~A (~S)~%" n nn str)))))
   (list 0+i 0-i 1+i 1.4+i 3.0+1.5i
	 (log 0) (- (log 0)) 
	 (make-rectangular 1/0 1.0) (make-rectangular 1/0 1/0) (make-rectangular 1.0 1/0) ; default: nan+1i nannani 1nani!
	 (make-rectangular 1/0 (real-part (log 0))) (make-rectangular (real-part (log 0)) 1/0) 
	 1e-14+1e14i 0+1e-16i (make-rectangular pi pi)
	 ))
  
  ;; strings/bytevectors
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (or (not (string? n))
		 (not (string? obj))
		 (not (string=? n obj))
		 (and (bytevector? n)
		      (not (bytevector? obj))))
	     (format *stderr* "~S not string=? ~S (~S)~%" n obj str)))))
   (list "" "abc" (string #\newline) "#<abc>" "a\"b\"c" "a\\b\nc" "aBc"
	 (let ((s (make-string 4 #\space))) (set! (s 3) #\null) s) ; writes as "   \x00"
	 "ab
c"
	 (string #\a #\b #\null #\c #\escape #\newline)
	 (string #\x (integer->char #xf0) #\x)
	 (string #\null)
	 #u8() #u8(0 1 2 3) 
	 (let ((str (make-string 256 #\null)))
	   (do ((i 0 (+ i 1)))
	       ((= i 256) str)
	     (set! (str i) (integer->char i))))
	 ))
  
  ;; symbols/keywords
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (or (not (symbol? n))
		 (not (symbol? obj))
		 (not (eq? n obj)))
	     (format *stderr* "~A not eq? ~A (~S)~%" n obj str)))))
   (list 'abc :abc abc:
	 (symbol "a") (symbol "#<>")
	 (gensym "|") (gensym "#<>") (gensym "}")
	 :: ':abc
	 (gensym "\\") 
	 ))
  
  ;; lists (circular, dotted)
  ;;
  ;; TODO: test circles in all structs
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (or (not (pair? n))
		 (not (pair? obj))
		 (not (equal? n obj)))
	     (format *stderr* "'~A not equal? '~A (~S)~%" n obj str)))))
   (list '(1) '(1 . 2) '((1 ()) 3)
	 '(1 2 . 3) '(1 2 3 . 4) '(())
	 ))
  
  ;; vectors
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (or (not (vector? n))
		 (not (vector? obj))
		 (not (equal? n obj)))
	     (format *stderr* "'~A not equal? '~A (~S)~%" n obj str)))))
   (list #() #(1) #(1 #(2)) #2d((1 2) (3 4))
	 #3d(((1 2 3) (4 5 6) (7 8 9)) ((9 8 7) (6 5 4) (3 2 1)))
	 #2d()
	 ))
  
  ;; hash-tables
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (or (not (hash-table? n))
		 (not (hash-table? obj))
		 (not (equal? n obj)))
	     (format *stderr* "'~A not equal? '~A (~S)~%" n obj str)))))
   (list (hash-table '(a . 1))
	 (hash-table '(a . 1) (cons 'b "hi"))
	 (let ((ht (make-hash-table 31)))
	   (set! (ht 1) 321)
	   (set! (ht 2) 123)
	   ht)
	 (hash-table)
	 ))
  
  ;; environments/s7 envs like error-env? what about the outer-env chain?
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (or (not (environment? n))
		 (not (environment? obj))
		 (not (equal? n obj)))
	     (format *stderr* "'~A not equal? '~A (~S)~%" n obj str)))))
   (list (environment '(a . 1))
	 (environment)
	 ))
  
  ;; closures/functions/built-in (C) functions + the setters thereof
  
  (for-each
   (lambda (n)
     (let ((str (with-output-to-string
		  (lambda ()
		    (write-readably n)))))
       (let ((obj (with-input-from-string str
		    (lambda ()
		      (eval (read))))))
	 (if (or (not (procedure? n))
		 (not (procedure? obj))
		 (not (equal? (procedure-source n) (procedure-source obj))))
	     (format *stderr* "'~A not equal? '~A (~S)~%" n obj str)))))
   (list abs
	 (lambda () 1)
	 (lambda (a) (+ a 1))
	 (lambda args (display args) (cdr args))
	 (lambda* (a b) (or a b))
	 (let ((a 1)) (lambda (b) (+ a b)))
	 ))

  ;;  (write-readably (let ((a 1)) (lambda (b) (+ a b)))) -> (lambda (b) (+ a b))
  
  ;; c-object? (could we test run it, and complain if something unequal comes back?)
  ;; what about cases like (#_abs -1) -- we'll see the function itself and need its name
  ;;   but (#_macroexpand ...)? (#_letrec* ...)?
  
  )

(test-write-readably)


;;; -------------------------------- pretty-print --------------------------------

(define* (pretty-print obj (port (current-output-port)) (column 0))

  (define (pretty-print-1 obj port column)
    (define (spaces n) 
      (write-char #\newline port)
      (do ((i 0 (+ i 1))) ((= i n)) (write-char #\space port)))
    
    (define (stacked-list lst col)
      (let ((added 0))
	(if (keyword? (car lst))
	    (begin
	      (write (car lst) port)
	      (write-char #\space port)
	      (set! added (+ 1 (length (object->string (car lst)))))
	      (set! lst (cdr lst))))
	(if (pair? lst)
	    (begin
	      (pretty-print-1 (car lst) port (+ col added))
	      (set! added 0)
	      (do ((l1 (cdr lst) (cdr l1)))
		  ((null? l1))
		(spaces col)
		(if (keyword? (car l1))
		    (begin
		      (write (car l1) port)
		      (write-char #\space port)
		      (set! added (+ 1 (length (object->string (car l1)))))
		      (set! l1 (cdr l1))))
		(if (pair? l1)
		    (pretty-print-1 (car l1) port (+ col added)))
		(set! added 0))))))
    
    (define (stacked-split-list lst col)
      (if (pair? lst)
	  (begin
	    (write-char #\( port)
	    (write (caar lst) port)
	    (write-char #\space port)
	    (pretty-print-1 (cadar lst) port (+ col (length (symbol->string (caar lst))) 2))
	    (write-char #\) port)
	    (do ((l1 (cdr lst) (cdr l1)))
		((null? l1))
	      (spaces col)
	      (write-char #\( port)
	      (write (caar l1) port)
	      (write-char #\space port)
	      (pretty-print-1 (cadar l1) port (+ col (length (symbol->string (caar l1))) 2))
	      (write-char #\) port)))))
    
    (define (messy-number z)
      (if (real? z)
	  (if (or (nan? z)
		  (infinite? z))
	      (object->string z)
	      (if (= z pi)
		  "pi"
		  (format #f "~,4f" z)))
	  (format "~A~A~Ai" 
		  (messy-number (real-part z))
		  (if (negative? (imag-part z)) "-" "+")
		  (messy-number (abs (imag-part z))))))

    (define (any-keyword? lst)
      (and (pair? lst)
	   (or (keyword? (car lst))
	       (any-keyword? (cdr lst)))))
    
    (cond ((number? obj)
	   (if (rational? obj)
	       (write obj port)
	       (display (messy-number obj) port)))
	  
	  ((pair? obj)
	   (case (car obj)
	     
	     ((lambda lambda* define* define-macro define-macro* define-bacro define-bacro* with-environment)
	      (format port "(~A ~A" (car obj) (cadr obj))
	      (spaces (+ column 2))
	      (stacked-list (cddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((defmacro defmacro*)
	      (format port "(~A ~A ~A" (car obj) (cadr obj) (caddr obj))
	      (spaces (+ column 2))
	      (stacked-list (cdddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((define)
	      (format port "(~A ~A " (car obj) (cadr obj))
	      (if (pair? (cadr obj))
		  (begin
		    (spaces (+ column 2))
		    (stacked-list (cddr obj) (+ column 2))
		    (write-char #\) port))
		  (write (caddr obj) port)))
	     
	     ((do)
	      (format port "(do (")
	      (if (pair? (cadr obj))
		  (stacked-list (cadr obj) (+ column 5)))
	      (write-char #\) port)
	      (spaces (+ column 4))
	      (write (caddr obj) port)
	      (spaces (+ column 2))
	      (stacked-list (cdddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((cond)
	      (format port "(cond ")
	      (stacked-list (cdr obj) (+ column 6))
	      (write-char #\) port))
	     
	     ((or and)
	      (if (> (length (object->string obj)) 40)
		  (begin
		    (format port "(~A " (car obj))
		    (stacked-list (cdr obj) (+ column 2 (length (symbol->string (car obj)))))
		    (write-char #\) port))
		  (write obj port)))
	     
	     ((case)
	      (format port "(case ~A" (cadr obj))
	      (spaces (+ column 2))
	      (stacked-list (cddr obj) (+ column 2))
	      (write-char #\) port))
	     
	     ((begin call-with-exit call/cc call-with-current-continuation with-baffle)
	      (format port "(~A" (car obj))
	      (if (pair? (cdr obj))
		  (begin
		    (spaces (+ column 2))
		    (stacked-list (cdr obj) (+ column 2))))
	      (write-char #\) port))
	     
	     ((if)
	      (let ((objstr (object->string obj))
		    (ifcol (+ column 4)))
		(if (< (length objstr) 40)
		    (display objstr port)
		    (begin
		      (format port "(if ")
		      (pretty-print-1 (cadr obj) port ifcol)
		      (spaces (+ column 4))
		      (pretty-print-1 (caddr obj) port ifcol)
		      (if (not (null? (cdddr obj)))
			  (begin
			    (spaces (+ column 4))
			    (pretty-print-1 (cadddr obj) port ifcol)))
		      (write-char #\) port)))))
	     
	     ((let let* letrec letrec*)
	      (let ((head-len (length (symbol->string (car obj)))))
		(if (symbol? (cadr obj))
		    (begin
		      (format port "(~A ~A (" (car obj) (cadr obj))
		      (stacked-split-list (caddr obj) (+ column head-len (length (symbol->string (cadr obj))) 4)))
		    (begin
		      (format port "(~A (" (car obj))
		      (stacked-split-list (cadr obj) (+ column head-len 3))))
		(write-char #\) port)
		(spaces (+ column 2))
		(stacked-list (if (symbol? (cadr obj)) (cdddr obj) (cddr obj)) (+ column 2))
		(write-char #\) port)))
	     
	     ((set!)
	      (let ((str (object->string obj)))
		(if (> (length str) 60)
		    (let ((settee (object->string (cadr obj))))
		      (format port "(set! ~A" settee)
		      (if (> (length settee) 20)
			  (begin
			    (spaces (+ column 6))
			    (pretty-print-1 (caddr obj) port (+ column 6)))
			  (begin
			    (write-char #\space port)
			    (pretty-print-1 (caddr obj) port (+ column 7 (length settee))))))
		    (display str port))))
	     
	     ((quote)
	      (write-char #\' port)
	      (pretty-print-1 (cadr obj) port column))
	     
	     (else
	      (let* ((objstr (object->string obj))
		     (strlen (length objstr)))
		(if (< strlen 60)
		    (display objstr port)
		    (let ((lstlen (length obj)))
		      (if (or (infinite? lstlen)
			      (< lstlen 2))
			  (display objstr port)
			  (let* ((carstr (object->string (car obj)))
				 (carstrlen (length carstr)))
			    (format port "(~A" carstr)
			    (if (any-keyword? (cdr obj))
				(begin
				  (spaces (+ column 2))
				  (stacked-list (cdr obj) (+ column 2)))
				(let ((line-len (ceiling (/ (- strlen carstrlen) 40))))
				  (if (= lstlen 2)
				      (begin
					(write-char #\space port)
					(pretty-print-1 (cadr obj) port (+ column 2 carstrlen)))
				      (if (< lstlen 5)
					  (begin
					    (write-char #\space port)
					    (stacked-list (cdr obj) (+ column 2 carstrlen)))
					  (let ((lst (cdr obj)))
					    (do ((i 1 (+ i line-len)))
						((>= i lstlen))
					      (do ((k 0 (+ k 1)))
						  ((or (null? lst)
						       (= k line-len)))
						(format port " ~A" (car lst))
						(set! lst (cdr lst)))
					      (if (not (null? lst))
						  (spaces (+ column carstrlen 2)))))))))))))))))
	  (else
	   (write obj port))))
  
  (pretty-print-1 obj port column))


(define (pp obj)
  (with-output-to-string
    (lambda ()
      (pretty-print obj))))

(define (test-pretty-print)

  (if (not (string=? (pp '(lambda* (a b) (+ a b) (* 1 2))) "(lambda* (a b)\n  (+ a b)\n  (* 1 2))"))
      (format *stderr* "pp 1"))

  (if (not (string=? (pp '(let ((a 1) (b 2)) (+ a b))) "(let ((a 1)\n      (b 2))\n  (+ a b))"))
      (format *stderr* "pp 2"))

  (if (not (string=? (pp '(let () (+ a b))) "(let ()\n  (+ a b))"))
      (format *stderr* "pp 2a"))

  (if (not (string=? (pp '(begin (+ 1 2) (* 2 3))) "(begin\n  (+ 1 2)\n  (* 2 3))"))
      (format *stderr* "pp 3"))

  (if (not (string=? (pp '(case a ((a b c) 1) ((d) 2) (else 3))) "(case a\n  ((a b c) 1)\n  ((d) 2)\n  (else 3))"))
      (format *stderr* "pp 4"))

  (if (not (string=? (pp '(cond ((> a 1) 2) ((< a 3) 3) (#t 4))) "(cond ((> a 1) 2)\n      ((< a 3) 3)\n      (#t 4))"))
      (format *stderr* "pp 5"))

  (if (not (string=? (pp '(if a '(1 2 3))) "(if a '(1 2 3))"))
      (format *stderr* "pp7"))
  )

(test-pretty-print)

#|
(let ((st (symbol-table)))
  (do ((i 0 (+ i 1))) 
      ((= i (vector-length st)))
    (for-each
     (lambda (sym)
       (if (defined? sym)
	   (let ((val (symbol->value sym)))
	     (format *stderr* "~A ~A " sym val)
	     (format *stderr* "~A" (if (or (procedure? val)
					   (macro? val))
				       (pp (procedure-source val))
				       (pp val)))
	     (newline *stderr*))))
     (st i))))
  
|#


 
;;; -------------------------------- checkpoint --------------------------------

(define* (checkpoint (file "checkpoint-s7.scm"))
  (call-with-output-file file
    (lambda (p)
      (let ((st (symbol-table)))
	(do ((i 0 (+ i 1))) 
	    ((= i (vector-length st)))
	  (for-each
	   (lambda (sym)
	     (if (defined? sym)
		 (let ((choice (*autoload* sym)))
		   (if (string? choice)
		       (format p "(if (not (defined? '~A)) (load ~S))~%" sym choice)
		       (if (procedure? choice)
			   (format p "(if (not (defined? '~A)) ((~S) (current-environment)))~%" sym choice))))))
	   (st i)))

      ;; now presumably we've loaded all the findable files, and called the autoload functions
      ;; run through the table again checking for diffs or omissions -- will this cover all the s7 settings?

	(do ((i 0 (+ i 1))) 
	    ((= i (vector-length st)))
	  (for-each
	   (lambda (sym)
	     (if (and (defined? sym)
		      (not (constant? sym))
		      (not (memq sym '(i st p file 
					 
					 ;; s7
					 multiple-value-bind letrec* *load-path* macroexpand *safety*
					 *#readers* *vector-print-length* *gc-stats* multiple-value-set! cond-expand
					 *features* call-with-values

					 ;; snd
					 *snd-opened-sound* break break-ok break-exit undo-edit redo-edit
					 ))))
		 ;; here we need to leave out built-in functions like abs 
		 (let ((choice (*autoload* sym)))
		   (if (and (not choice)
			    (not (string=? (object->string (symbol->value sym)) (object->string sym))))
		       (begin
			 (catch #t
			   (lambda ()
			     (let ((str (with-output-to-string
					  (lambda ()
					    (write-readably (symbol->value sym))))))
			       (format p "(if (not (defined? '~A)) (define ~A ~A))~%" sym sym str)))
			   (lambda args
			     (format *stderr* "~A not saved~%" sym))))))))
	   (st i)))

	;; now look for changes?  This probably can't work for macros/functions (not equal? anyway)

      )))
  #f)
