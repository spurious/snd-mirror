(set! (hook-functions *unbound-variable-hook*) ())
(set! *vector-print-length* 6)


(if (provided? 'snd)
    (begin
      (format *stderr* "this won't work in Snd!~%") ; see t705.scm
      (exit)))

;(load "stuff.scm")
;(load "r7rs.scm")
(require mockery.scm)

(define data-file #f) ;(open-output-file "output-of-t455"))
(define max-args 3)

(define-constant one 1)

(define mock-number (*mock-number* 'mock-number))
(define mock-pair (*mock-pair* 'mock-pair))
(define mock-string (*mock-string* 'mock-string))
(define mock-char (*mock-character* 'mock-char))
(define mock-vector (*mock-vector* 'mock-vector))
(define mock-symbol (*mock-symbol* 'mock-symbol))
(define mock-hash-table* (*mock-hash-table* 'mock-hash-table*))

(define np (list 0 1 2 3 4))
(define mp (mock-pair '(0 1 2 3 4)))
(define nv (vector 0 1 2 3 4))
(define mv (mock-vector 0 1 2 3 4))
(define ns "01234")
(define ms (mock-string #\0 #\1 #\2 #\3 #\4))

(define constants (list #f #t () #\a (/ most-positive-fixnum) (/ -1 most-positive-fixnum) 1.5+i
			"hi455" :key hi: 'hi (list 1) (list 1 2) (cons 1 2) (list (list 1 2)) (list (list 1)) (list ()) #() 
			1/0+i 0+0/0i 0+1/0i 1+0/0i 0/0+0i 0/0+0/0i 1+1/0i 0/0+i cons ''2 
			1+i 1+1e10i 1e15+1e15i 0+1e18i 1e18 (integer->char 255) (string (integer->char 255)) 1e308 
			most-positive-fixnum most-negative-fixnum (- most-positive-fixnum 1) (+ most-negative-fixnum 1)
			-1 0 0.0 1 1.5 1.0-1.0i 3/4 #\null -63 (make-hash-table) (hash-table '(a . 2) '(b . 3))
			'((1 2) (3 4)) '((1 (2)) (((3) 4))) "" (list #(1) "1") '(1 2 . 3) (list (cons 'a 2) (cons 'b 3))
			#(1 2) (vector 1 '(3)) (let ((x 3)) (lambda (y) (+ x y))) abs 'a 'b one
			(lambda args args) (lambda* ((a 3) (b 2)) (+ a b)) (lambda () 3)
			(sublet () (cons 'a 1)) (rootlet)
			*load-hook*  *error-hook* (make-random-state 123)
			quasiquote macroexpand cond-expand begin let letrec* if case cond (call-with-exit (lambda (goto) goto))
			;(with-baffle (call/cc (lambda (cc) cc)))
			(string #\a #\null #\b) #2d((1 2) (3 4)) (inlet 'a 2 'b 3)
			#<undefined> #<eof> #<unspecified> (make-vector 3 0 #t) (make-vector 3 -1.4 #t)
			(make-vector '(2 3) "hi") #("hiho" "hi" "hoho") (make-shared-vector (make-vector '(2 3) 1 #t) '(6))
			(make-shared-vector (make-shared-vector (make-vector '(2 3) 1.0 #t) '(6)) '(2 2))
			(vector-ref #2d((#(1 2 3)) (#(3 4 5))) 0 0) (define-macro (m a) `(+ ,a 1))
			(c-pointer 0) (c-pointer -1) :readable :else (define-bacro* (m (a 1)) `(+ ,a 1))
			(bytevector 0 1 2) (bytevector) (bytevector 255 0 127) (make-hash-table-iterator (hash-table '(a . 2)))
			(lambda (dir) 1.0) (float-vector) (make-float-vector '(2 32)) 
			;(openlet (inlet 'value 1 '+ (lambda args 1)))
			;all-env

			(mock-number 0) (mock-number 2) (mock-number 1-i) (mock-number 4/3) (mock-number 2.0)
			(mock-string #\h #\o #\h #\o)
			(mock-pair '(2 3 4))
			(mock-char #\b)
			(mock-symbol 'c)
			(mock-vector 1 2 3 4)
			(mock-hash-table* 'b 2)
			))

(define low 0)
(define arglists (vector (make-list 1) (make-list 2) (make-list 3) (make-list 4) (make-list 5) (make-list 6)))

(define (autotest func args args-now args-left)
  ;; args-left is at least 1, args-now starts at 0, args starts at ()
  ;(if (macro? func) (format *stderr* "~A: ~D ~D (~D ~D): ~A~%" func (length args) args-now low args-left args))
    
  (call-with-exit
   (lambda (quit)
     (if (>= args-now low)
	 (catch #t 
	   (lambda () 
	     (cond ((func (values args)) => ;(apply func args) => 
		    (lambda (val) 
		      (if data-file
			  (format data-file "(~S~{ ~S~}) -> ~S~%" func args val))))))
	   (lambda any
	     (if (and (> args-now 0)
		      (memq (car any) '(wrong-type-arg syntax-error))
		      (not (memq func (list map for-each /))))
		 (quit)))))
     
     (let ((c-args (vector-ref arglists args-now)))
       (copy args c-args)

       (let ((p (list-tail c-args args-now)))
	 (if (= args-left 1)
	     (call-with-exit
	      (lambda (quit)
		(set-car! p (car constants))
		(catch #t
		  (lambda ()
		    (cond ((apply func c-args) => 
			   (lambda (val)
			     (if data-file
				 (format data-file "(~S~{ ~S~}) -> ~S~%" func c-args val))))))
		  (lambda any 
		    (if (and (memq (car any) '(wrong-type-arg syntax-error))
			     (pair? (cdr (cadr any)))
			     (pair? (cddr (cadr any)))
			     (integer? (caddr (cadr any))) ; if just 1 arg, arg num can be omitted
			     (< (caddr (cadr any)) low))
			(quit))))
		 
		(for-each
		 (lambda (c)
		   (catch #t 
		     (lambda () 
		       (set-car! p c)
		       (cond ((apply func c-args) => 
			      (lambda (val)
				(if data-file
				    (format data-file "(~S~{ ~S~}) -> ~S~%" func c-args val))))))
		     (lambda any 
		       'error)))
		 (cdr constants))))
	   
	     (for-each
	      (lambda (c)
		(set-car! p c)
		(autotest func c-args (+ args-now 1) (- args-left 1)))
	      constants)))))))

;(set! low 3)
;(autotest string-set! () 0 3)

(define baddies (list "exit" "emergency-exit" "abort" "autotest" 
		      "all" "delete-file" "system" "set-cdr!" "stacktrace" "test-sym"
		      "varlet" "dilambda" "gc" "cond-expand" "reader-cond"
		      "openlet" "coverlet" "eval" "vector" "list" "cons" "m" "hash-table*" "hash-table" "values"
		      "object->string"

		      "mus-audio-close" "mus-audio-read" "mus-audio-write" "mus-audio-open-output"
		      "boolean=?" "symbol=?" "symbol-table"

		      (reader-cond ((> max-args 2) "copy"))
		      (reader-cond ((>= max-args 3) "hash-table-set!" "vector-set!" "let-set!"))
		      (reader-cond ((> max-args 3) "map" "for-each")) ; #t = omit

		      "mock-number" "mock-pair" "mock-string" "mock-char" "mock-vector" 
		      "mock-symbol" "mock-port" "mock-hash-table"
		      "outlet-member" "make-method" "make-object"))

(define (test-sym sym)
  (if (defined? sym)
      (let ((f (symbol->value sym)))
	(let ((argn (and (or (procedure? f) (let? f)) (arity f))))
	  (if argn
	      (let ((bottom (car argn))
		    (top (min (cdr argn) max-args))
		    (strname (symbol->string sym)))
		(if (not (or (memq (strname 0) '(#\{ #\[ #\())
			     (member strname baddies)))
		    (begin
		      (if (< top bottom)
			  (format *stderr* ";~A (bottom: ~A, top: ~A)...~%" sym bottom top)
			  (format *stderr* ";~A...~%" sym))
		      (format data-file ";~A...~%" sym)
		      (set! low bottom)
		      (if (zero? (cdr argn))
			  (let ((val (f)))
			    (if (and val data-file)
				(format data-file "(~S) -> ~S~%" sym val)))
			  (autotest f () 0 top))))))))))

(define (all)
  (let ((st (symbol-table)))
    (for-each test-sym st)
    (if data-file (close-output-port data-file))
    (format #t "~%all done~%")))

(all)
