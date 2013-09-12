(set! (hook-functions *unbound-variable-hook*) ())
(set! *vector-print-length* 6)
;(set! *gc-stats* #t)

(if (provided? 'snd)
    (begin
      (format *stderr* "this won't work in Snd!~%")
      (exit)))

(define data-file (open-output-file "output-of-t455"))

(define constants (list #f #t () #\a (/ 1 most-positive-fixnum) (/ -1 most-positive-fixnum) 1.5+i
			"hi455" :hi hi: 'hi (list 1) (list 1 2) (cons 1 2) '() (list (list 1 2)) (list (list 1)) (list ()) #() 
			1/0+i 0+0/0i 0+1/0i 1+0/0i 0/0+0i 0/0+0/0i 1+1/0i 0/0+i cons ''2 
			1+i 1+1e10i 1e15+1e15i 0+1e18i 1e18 (integer->char 255) (string (integer->char 255)) 1e308 
			most-positive-fixnum most-negative-fixnum (- most-positive-fixnum 1) (+ most-negative-fixnum 1)
			-1 0 0.0 1 1.5 1.0+1.0i 3/4 #\null -63 (make-hash-table) ;(hash-table '(a . 2) '(b .3))
			'((1 2) (3 4)) '((1 (2)) (((3) 4))) '(()) "" (list #(1) "1") '(1 2 . 3)
			#(1 2) (vector 1 '(3)) (let ((x 3)) (lambda (y) (+ x y))) abs (lambda args args) (lambda* ((a 3) (b 2)) (+ a b))
			(augment-environment '() (cons 'a 1)) (current-environment) (global-environment)
			*load-hook*  *error-hook* (make-random-state 123) *vector-print-length* *gc-stats*
			quasiquote macroexpand cond-expand begin let (c-pointer 0) pi (call-with-exit (lambda (goto) goto))
			(string #\a #\null #\b) #2d((1 2) (3 4))
			#<undefined> #<eof> #<unspecified> (make-vector 3 0 #t) (make-vector 3 -1.4 #t)
			(make-vector '(2 3) "hi") #("hiho" "hi" "hoho") (make-shared-vector (make-vector '(2 3) 1 #t) '(6))
			(make-shared-vector (make-shared-vector (make-vector '(2 3) 1.0 #t) '(6)) '(2 2))
			(vector-ref #2d((#(1 2 3)) (#(3 4 5))) 0 0)
			))

(define low 0)
(define arglists (vector (make-list 1) (make-list 2) (make-list 3) (make-list 4)))

(define (autotest func args args-now args-left)
  ;; args-left is at least 1, args-now starts at 0, args starts at ()
  ;(format *stderr* "~A: ~D ~D (~D ~D): ~A~%" func (length args) args-now low args-left args)
  (call-with-exit
   (lambda (quit)
     (if (>= args-now low)
	 (catch #t 
	   (lambda () 
	     (let ((val (apply func args)))
	       (if val
		   (format data-file "(~S ~{~S~^ ~}) -> ~S~%" func args val))))
	   (lambda any
	     (if (or (eq? (car any) 'wrong-type-arg)
		     (not (memq func (list map for-each /))))
		 (quit)))))
     
     (let ((c-args (vector-ref arglists args-now)))
       (copy args c-args)

       (let ((p (list-tail c-args args-now)))
	 (if (= args-left 1)
	     (for-each
	      (lambda (c)
		(catch #t 
		  (lambda () 
		    (set-car! p c)
		    ;;(list-set! c-args args-now c)
		    ;;(format *stderr* "~A " c-args)
		    (let ((val (apply func c-args)))
		      (if val
			  (format data-file "(~S ~{~S~^ ~}) -> ~S~%" func c-args val))))
		(lambda any 'error)))
	    constants)
	   
	   (for-each
	    (lambda (c)
	      (set-car! p c)
	      ;;(list-set! c-args args-now c)
	      (autotest func c-args (+ args-now 1) (- args-left 1)))
	    constants)))))))

(let ((st (symbol-table)))
  (do ((i 0 (+ i 1))) 
      ((= i (length st))
       (s7-version)
       (close-output-port data-file)
       (format #t "~%all done~%"))
    (let ((lst (st i)))
      (for-each 
       (lambda (sym)
	 (if (defined? sym)
	     (let ((f (symbol->value sym)))
	       (if (or (aritable? f 0)
		       (aritable? f 1)
		       (aritable? f 2)
		       (aritable? f 3))
		   (if (procedure? f)
		       (let ((strname (symbol->string sym)))
			 (if (or (member (strname 0) '(#\{ #\[ #\())
				 (member strname '("exit" "emergency-exit" "abort" "unoptimize" "autotest" "delete-file" "system" "set-cdr!" "stacktrace"
						   "augment-environment!" "make-procedure-with-setter" "open-environment")))
			     (format data-file ";skip ~A for now~%" sym) ; no time! no time!
			     (let ((top (if (aritable? f 3) 3
					    (if (aritable? f 2) 2
						(if (aritable? f 1) 1 0))))
				   (bottom (if (aritable? f 0) 0
					       (if (aritable? f 1) 1
						   (if (aritable? f 2) 2 3)))))
			       (format data-file ";whack on ~A...~%" sym)
			       (set! low bottom)
			       (if (zero? top)
				   (let ((val (f)))
				     (if val (format data-file "(~S) -> ~S~%" sym val)))
				   (autotest f () 0 top)))))
		       (begin
			 (if (or #t (member (symbol->string sym) 
					    '("do" "begin" "set!" "constants" "st" "i" "lst"
					      "define" "define*" "define-expansion" "symbol-table"
					      "define-macro" "define-bacro" "define-macro*" "define-bacro*"
					      "defmacro" "defmacro*" "define-constant")))
			     (format data-file ";skip ~A for now~%" sym)
			     (begin
			       (format data-file ";whack on ~A...~%" sym)
			       (set! low 0)
			       (autotest f '() 0 3)))))))))
       lst))))
