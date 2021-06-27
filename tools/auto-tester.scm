;;; this is an extension of tauto.scm, an auto-tester

(define with-methods #f)
;(set! (*s7* 'profile) 1)

(for-each (lambda (x) 
	    (unless (memq (car x) '(make-string make-byte-vector *features* *libraries* *#readers*)) ; last 2 for sandbox
	      (immutable! (car x))))
	  (rootlet))

(define (no-set s v)
  (error 'bad "can't set ~S" s))

(immutable! quote)

(for-each (lambda (x)
	    (when (syntax? (symbol->value x))
	      (set! (setter x) no-set)
	      (immutable! x)))
	  (symbol-table))

;(define debugging (provided? 'debugging))
;(when (provided? 'profiling) (load "profile.scm"))
;(set! (hook-functions *load-hook*) (list (lambda (hook) (format () "loading ~S...~%" (hook 'name)))))

(define (cycler size)
  (let ((cp-lst (make-list 3 #f))
	(it-lst (make-list 3 #f)))
    (let ((bases (vector (make-list 3 #f)
			 (make-vector 3 #f)
			 (hash-table 'a 1 'b 2 'c 3)
			 (inlet 'a 1 'b 2 'c 3)
			 (make-iterator it-lst)
			 (c-pointer 1 cp-lst)))
	  (sets ()))
	  
      (do ((blen (length bases))
	   (i 0 (+ i 1)))
	  ((= i size))
	(let ((r1 (random blen))
	      (r2 (random blen))
	      (loc (random 3)))
	  (let ((b1 (bases r1))
		(b2 (bases r2)))
	    (case (type-of b1)
	      ((pair?)
	       (if (> (random 10) 3)
		   (begin
		     (set! (b1 loc) b2)
		     (set! sets (cons (list r1 loc r2) sets)))
		   (begin
		     (set-cdr! (cddr b1) (case loc ((0) b1) ((1) (cdr b1)) (else (cddr b1))))
		     (set! sets (cons (list r1 (+ loc 3) r2) sets)))))

	      ((vector?)
	       (set! (b1 loc) b2)
	       (set! sets (cons (list r1 loc r2) sets)))

	      ((hash-table? let?)
	       (let ((key (#(a b c) loc)))
		 (set! (b1 key) b2)
		 (set! sets (cons (list r1 key r2) sets))))

	      ((c-pointer?)
	       (set! (cp-lst loc) b2)
	       (set! sets (cons (list r1 loc r2) sets)))
		
	      ((iterator?)
	       (set! (it-lst loc) b2)
	       (set! sets (cons (list r1 loc r2) sets)))))))

      (object->string (bases (random 6)) :readable))))

(require stuff.scm)
(load "write.scm")
(define (pp-checked obj)
  (let-temporarily ((((funclet pretty-print) '*pretty-print-cycles*) #t)) (pp obj)))
(require case.scm)
(define match?  ((funclet 'case*) 'case*-match?))

(when with-methods 
  (load "mockery.scm")
  (define-constant mock-number (*mock-number* 'mock-number))
  (define-constant mock-pair (*mock-pair* 'mock-pair))
  (define-constant mock-string (*mock-string* 'mock-string))
  (define-constant mock-char (*mock-char* 'mock-char))
  (define-constant mock-vector (*mock-vector* 'mock-vector))
  (define-constant mock-symbol (*mock-symbol* 'mock-symbol))
  (define-constant mock-hash-table (*mock-hash-table* 'mock-hash-table))
  (define-constant mock-c-pointer (*mock-c-pointer* 'mock-c-pointer))
  (define-constant mock-port (*mock-port* 'mock-port))
  (define-constant mock-random-state (*mock-random-state* 'mock-random-state)))

(set! (*s7* 'safety) 1) ; protect copy (in define-expansion evaluation) from circular lists

(set! (*s7* 'max-stack-size) 32768)
(set! (*s7* 'max-heap-size) (ash 1 22)) ; 8M
(set! (*s7* 'max-port-data-size) (ash 1 28))
;(set! (*s7* 'gc-stats) #t)
(set! (*s7* 'print-length) 4096)
(set! (*s7* 'max-string-length) 500000)
(set! (*s7* 'max-list-length) 10000)
(set! (*s7* 'max-vector-length) 10000)
(set! (*s7* 'max-vector-dimensions) 10)
(set! (*s7* 'autoloading?) #f)
(set! (*s7* 'equivalent-float-epsilon) 1.0e-6)
(set! (current-output-port) #f)

(define ostr "")
(define estr "")
(define nostr "")

(define error-type #f)
(define error-info ())
(define false #f)
(define-constant _undef_ (car (with-input-from-string "(#_asdf 1 2)" read)))
(define kar car)
(set! (setter kar) (lambda (sym e) (error 'oops "kar not settable: ~A" ostr)))
(define-constant _1234_ 1234)
(define-constant _dilambda_ (dilambda (lambda (x) (+ x 1)) (lambda (x y) (+ x y))))
(define __var2__ 3)
(set! (setter '__var2__) (lambda (s v) (if (integer? v) v 3)))
(define _definee_ #f)
(define x 0)
(define my-let let)
(define my-with-baffle with-baffle)
(define* (my-make-byte-vector size (init 0)) (make-byte-vector size init))
(define* (my-make-string size (init #\a)) (make-string size init))

(define fvref float-vector-ref)
(define ivref int-vector-ref)
(define vref vector-ref)
(define fvset float-vector-set!)
(define ivset int-vector-set!)
(define vset vector-set!)
(define adder +)

(define (_vals_) (values #f 1 2))
(define (_vals1_) (values 1 #f 2))
(define (_vals2_) (values 1 2 #f))
(define (finite? n) (not (or (nan? n) (infinite? n))))
(define (more-values) (values 1 2 3 4))
(define (_vals3_ x) (values x x))
(define (_vals4_ x y) (values x y))
(define (_vals5_ x y z) (values x y z))
(define (_vals6_ w x y z) (values w x y z))

(define* (_vals3s_ x) (values x x))
(define* (_vals4s_ x y) (values x y))
(define* (_vals5s_ x y z) (values x y z))
(define* (_vals6s_ w x y z) (values w x y z))

(define (_svals3_ x) (* x x))
(define (_svals4_ x y) (* x y))
(define (_svals5_ x y z) (* x y z))
(define (_svals6_ w x y z) (* w x y z))

(define* (_svals3s_ x) (* x x))
(define* (_svals4s_ x y) (* x y))
(define* (_svals5s_ x y z) (* x y z))
(define* (_svals6s_ w x y z) (* w x y z))

(define (s7-print-length) (*s7* 'print-length))
(define (s7-max-string-length) (*s7* 'max-string-length))
(define (s7-max-list-length) (*s7* 'max-list-length))
(define (s7-max-vector-length) (*s7* 'max-vector-length))
(define (s7-max-vector-dimensions) (*s7* 'max-vector-dimensions))
(define (s7-default-hash-table-length) (*s7* 'default-hash-table-length))
(define (s7-initial-string-port-length) (*s7* 'initial-string-port-length))
(define (s7-safety) (*s7* 'safety))
(define (s7-set-safety val) (set! (*s7* 'safety) val))
(define (s7-autoloading?) (*s7* 'autoloading?))
(define (s7-max-stack-size) (*s7* 'max-stack-size))
(define (s7-stacktrace-defaults) (copy (*s7* 'stacktrace-defaults)))
(define (s7-gc-stats) (*s7* 'gc-stats))
(define (s7-undefined-identifier-warnings) (*s7* 'undefined-identifier-warnings))
;(define (s7-set-print-length x) (set! (*s7* 'print-length) x))
(define (s7-set-stacktrace-defaults x) (set! (*s7* 'stacktrace-defaults) x))

#|
(define (s7-set-default-rationalize-error x) (set! (*s7* 'default-rationalize-error) x))
(define (s7-set-max-string-length x) (set! (*s7* 'max-string-length) x))
(define (s7-set-max-list-length x) (set! (*s7* 'max-list-length) x))
(define (s7-set-max-vector-length x) (set! (*s7* 'max-vector-length) x))
(define (s7-set-max-vector-dimensions x) (set! (*s7* 'max-vector-dimensions) x))
(define (s7-set-default-hash-table-length x) (set! (*s7* 'default-hash-table-length) x))
(define (s7-set-initial-string-port-length x) (set! (*s7* 'initial-string-port-length) x))
(define (s7-set-undefined-identifier-warnings x) (set! (*s7* 'undefined-identifier-warnings) x))
(define (s7-set-autoloading? x) (set! (*s7* 'autoloading?) x))
(define (s7-set-max-stack-size x) (set! (*s7* 'max-stack-size) x))
(define (s7-set-gc-stats x) (set! (*s7* 'gc-stats) x))

(define (s7-catches) (*s7* 'catches))
(define (s7-stack-top) (*s7* 'stack-top))
(define (s7-stack-size) (*s7* 'stack-size))
(define (s7-symbol-table) (*s7* 'symbol-table))
(define (s7-rootlet-size) (*s7* 'rootlet-size))
(define (s7-heap-size) (*s7* 'heap-size))
(define (s7-free-heap-size) (*s7* 'free-heap-size))
(define (s7-gc-freed) (*s7* 'gc-freed))
|#
(define (s7-c-types) (*s7* 'c-types))
(define (s7-profile-info) (*s7* 'profile-info))
;(define (s7-history) (*s7* 'history))
(define (s7-history-size) (*s7* 'history-size))

;(define (s7-set-equivalent-float-epsilon x) (set! (*s7* 'equivalent-float-epsilon) x))
;(define (s7-set-hash-table-float-epsilon x) (set! (*s7* 'hash-table-float-epsilon) x))
(define (s7-set-bignum-precision x) (set! (*s7* 'bignum-precision) x))
(define (s7-set-float-format-precision x) (set! (*s7* 'float-format-precision) x))

(define (s7-default-rationalize-error) (*s7* 'default-rationalize-error))
(define (s7-equivalent-float-epsilon) (*s7* 'equivalent-float-epsilon))
(define (s7-hash-table-float-epsilon) (*s7* 'hash-table-float-epsilon))
(define (s7-bignum-precision) (*s7* 'bignum-precision))
(define (s7-float-format-precision) (*s7* 'float-format-precision))
(define (s7-default-random-state) (*s7* 'default-random-state))
(define (s7-cpu-time) (*s7* 'cpu-time))
;(define (s7-file-names) (*s7* 'file-names))

(define-macro (_mac_ x) `(+ ,x 1))
(define-macro* (_mac*_ (x 1)) `(+ ,x 1))
(define-bacro (_bac_ x) `(+ ,x 1))
(define-bacro* (_bac*_ (x 1)) `(+ ,x 1))
(define (_fnc_ x) (+ x 1))
(define* (_fnc*_ (x 1)) (+ x 1))
(define (_fnc1_ x) (apply + (list x 1)))
(define (_fnc2_ x) (- x 1))
(define (_fnc3_ x) (* x 2.0))
(define (_fnc4_ x) (/ x))
(define (_fnc5_ x) (not (pair? x)))
;(define (_fnc6_ x) (unless (let? x) (let-temporarily (((*s7* 'safety) 1)) (fill! x #\a))))
;;; (define (_fnc7_ x) (let-temporarily (((*s7* 'safety) 1)) (reverse! x)))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (fibr n)
  (if (>= n 2)
      (+ (fibr (- n 1))
         (fibr (- n 2)))
      n))

(define (fibf n)
  (if (< n 2.0)
      n
      (+ (fibf (- n 1.0))
         (fibf (- n 2.0)))))


(define (local-random . args)
  (type-of (apply random args)))

(define (local-read-string . args)
  (with-input-from-file "/home/bil/cl/all-lg-results"
    (lambda ()
      (read-string (min 1000 (car args))))))

(define (checked-eval code)
  (and (pair? code)
       (< (length code) 100)
       (null? (cyclic-sequences code))
       (eval code)))

(define (local-varlet . args)
  (let ((e (car args)))
    (when (and (let? e)
	       (not (eq? e (rootlet)))
	       (not (defined? 'error-type e)))
      (apply varlet e (cdr args)))))

(define (local-let-set! . args)
  (let ((e (car args)))
    (when (and (let? e)
	       (not (eq? e (rootlet)))
	       (not (defined? 'error-type e)))
      (apply let-set! e (cdr args)))))

(define (checked-hash-table . args)
  (let ((_h_ (make-hash-table (*s7* 'default-hash-table-length) equivalent?)))
    (do ((key/value args (cddr key/value)))
	((null? key/value) _h_)
      (if (not (pair? (cdr key/value)))
	  (error 'wrong-number-of-args "no value")
	  (set! (_h_ (car key/value)) (cadr key/value))))))

(define (checked-read-char . args) (with-input-from-string "0123" (lambda () (apply read-char args))))
(define (checked-read-byte . args) (with-input-from-string "0123" (lambda () (apply read-byte args))))
(define (checked-read-line . args) (with-input-from-file "s7test.scm" (lambda () (apply read-line args))))
(define (checked-read-string . args) (with-input-from-file "s7test.scm" (lambda () (apply read-string args))))
(define (checked-read . args) (with-input-from-file "dsp.scm" (lambda () (apply read args))))
(define (checked-reverse! . args) (reverse! (copy (car args))))
(define (checked-port-line-number . args) (apply port-line-number args) 0)
;(define (checked-let-set! . args) (apply let-set! (curlet) args))
;(define (checked-varlet . args) (apply varlet (sublet (curlet)) args))
;(define (checked-cutlet . args) (apply cutlet (sublet (curlet)) args))
(define (checked-procedure-source . args) (copy (procedure-source (car args)) :readable))
;(define (checked-pp obj) (string? (pp obj)))

(load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))

#|
(define lint-no-read-error #t)
(define linter (let ()
		 (let-temporarily (((*s7* 'autoloading?) #t))
		   (load "lint.scm"))
		 (lambda (str)
		   (call-with-output-string
		    (lambda (op)
		      (call-with-input-string str
			(lambda (ip)
			  (lint ip op))))))))
|#

(define-expansion (_dw_ . args)
  `(dynamic-wind #f (lambda () ,@args) #f))

(define-expansion (_dw_out_ . args)
  `(let ((_port_ #f)
	 (_old_port_ #f))
     (dynamic-wind
	 (lambda ()
	   (set! _old_port_ (current-output-port))
	   (set! _port_ (open-output-string))
	   (set-current-output-port _port_))
	 (lambda ()
	   ,@args
	   (flush-output-port _port_)
	   (get-output-string _port_))
	 (lambda ()
	   (close-output-port _port_)
	   (set-current-output-port _old_port_)))))

(define-expansion (_cw_ . args)
  `(call-with-exit (lambda (_x_) (_x_ ,@args))))

(define-expansion (_cc_ . args)
  `(call/cc (lambda (_x_) (_x_ ,@args))))

(define-expansion (_ct1_ . args)
  `(catch 'oops (lambda () (call-with-exit (lambda (goto) (values ,@args)))) (lambda args 'error)))

(define-expansion (_ct2_ . args)
  `(catch 'oops (lambda () (call-with-exit (lambda (goto) (goto ,@args)))) (lambda args 'error)))

(define-expansion (_mem1_ . args)
  `(member 1 (list 3 2) (lambda (a b) ,@args)))

(define-expansion (_mem2_ . args)
  `(assoc 1 (list (list 3 2) (list 2)) (lambda (a b) ,@args)))


(define-expansion (_ft1_ . args)
  `(let ((_f_ (lambda () ,@args))) (_f_) (_f_)))

(define-expansion (_ft2_ . args)
  `(let () (define (_f_) ,@args) (define (g) (_f_)) (g) (g)))

(define (_rf11_ i x) (if (> i 0) (_rf11_ (- i 1) x) (x)))
(define-expansion (_rf1_ . args)
  `(let ((y 0)) (_rf11_ 1 (lambda () ,@args))))

(define-expansion (_rf3_ . args)
  `(let ((y 0)) (_rf11_ 1 (lambda () (begin ,@args)))))

(define (_rf22_ i x) (if (> i 0) (_rf22_ (- i 1) x) (x i)))
(define-expansion (_rf2_ . args) 
  `(let () (_rf22_ 1 (lambda (y) (begin ,@args)))))

(define-expansion (_do1_ . args)
  `(with-output-to-string 
     (lambda () 
       (do ((i 0 (+ i 1)))
	   ((= i 1))
	 ,@(map (lambda (x)
		  (list 'display x))
		args)))))

(define-expansion (_do2_ . args)
  `(with-output-to-string 
     (lambda () 
       ,@(map (lambda (x)
		(list 'display x))
	      args))))

(define-expansion (_do4_ . args)
  `(do ((__var__ #f)
	(_i_ 0 (+ _i_ 1)))
       ((= _i_ 1) __var__)
     (set! __var__ ,@args)))

(define-expansion (_do5_ . args)
  `(let ((__var__ #f))
     (let doer ((_i_ 0))
       (if (= _i_ 1)
	   __var__
	   (begin
	     (set! __var__ ,@args)
	     (doer (+ _i_ 1)))))))

(define-expansion (_do3_ . args)
  `(let ((exiter (vector #f))) (do ,(car args) ((vector-ref exiter 0) 1) ,@(cdr args) (vector-set! exiter 0 #t))))

(define-expansion (_cop1_ . args)
  `(let ((x begin ,@args))
     x))

(define-expansion (_cop2_ . args)
  `(let ((x begin ,@args))
     (copy x)))

(define-expansion (_rd3_ . args)
  `(let ((port #f))
     (dynamic-wind
	 (lambda ()
	   (set! port (open-input-string (format #f "~W" (car (list ,@args))))))
	 (lambda ()
	   (read port))
	 (lambda ()
	   (close-input-port port)))))

(define-expansion (_rd4_ . args)
  `(with-input-from-string
       (object->string (car (list ,@args)) :readable) ; defaults to ~S
     read))

(define-expansion (_rd5_ . args)
  `(let ((port #f))
     (dynamic-wind
	 (lambda ()
	   (set! port (open-input-string (format #f "~S" (car (list ,@args))))))
	 (lambda ()
	   (read-line port))
	 (lambda ()
	   (close-input-port port)))))

(define-expansion (_rd6_ . args)
  `(with-input-from-string
       (object->string (car (list ,@args)))
     read-line))

(define-expansion (_rd7_ . args)
  `(with-input-from-file "/home/bil/cl/all-lg-results"
     (lambda ()
       ,@args)))

(define-expansion (_rd8_ . args)
  `(let ((old-port (current-input-port)))
     (dynamic-wind
	 (lambda ()
	   (set-current-input-port (open-input-file "/home/bil/cl/all-lg-results")))
	 (lambda ()
	   ,@args)
	 (lambda ()
	   (close-input-port (current-input-port))
	   (set-current-input-port old-port)))))

(define-expansion (_wr1_ . args)
  `(let ((port #f))
     (dynamic-wind
	 (lambda ()
	   (set! port (open-output-string)))
	 (lambda ()
	   (format port "~S" (car (list ,@args)))
	   (get-output-string port))
	 (lambda ()
	   (close-output-port port)))))

(define-expansion (_wr2_ . args)
  `(call-with-output-string
     (lambda (port)
       (write (car (list ,@args)) port))))

(define-expansion (_wr3_ . args)
  `(format #f "~W" (car (list ,@args))))

(define-expansion (_wr4_ . args)
  `(object->string (car (list ,@args)) :readable))

(define-expansion (_let1_ . args)
  `(let-temporarily ((x 1)) (call-with-exit (lambda (go) (go ,@args)))))

(define-expansion (_let2_ . args)
  `(call-with-exit (lambda (go) (let-temporarily ((x 1)) (go ,@args)))))

(define-expansion (_iter_ . args)
  `(let ((erg (list-values ,@args)))
     (let ((iter (if (iterator? erg) erg (make-iterator erg)))
	   (result ()))
       (do ((x (iter) (iter)))
	   ((iterator-at-end? iter) (reverse result))
	 (set! result (cons x result))))))

(define-expansion (_map_ . args)
  `(map values (list ,@args)))

(define-expansion (_cat1_ . args)
  `(catch 'oops
     (lambda ()
       (catch 'not-oops
	 (lambda ()
	   (throw 'oops ,@args))
	 (lambda (t i)
	   'error)))
     (lambda (t i)
       'error)))

(define-expansion (_cat2_ . args)
  `(catch 'oops
     (lambda ()
       (catch 'not-oops
	 (lambda ()
	   (error 'oops ,@args))
	 (lambda (t i)
	   'error)))
     (lambda (t i)
       'error)))

#|
(define-expansion (_fe1_ . args)
  `(for-each (lambda (n) (n 0)) (list ,@args)))

(define-expansion (_fe2_ . args)
  `(do ((x (list ,@args) (cdr x)))
       ((null? x) #unspscified>)
     ((car x) 0)))

(define-expansion (_fe3_ . args)
  `(for-each (lambda (n) (set! (n) 0)) (list ,@args)))

(define-expansion (_fe4_ . args)
  `(do ((x (list ,@args) (cdr x)))
       ((null? x) #unspscified>)
     (set! ((car x)) 0)))

(define-macro (trace f)
  (let ((old-f (gensym)))
    `(define ,f 
       (let ((,old-f ,f))
	 (apply lambda 'args 
		`((format () "(~S ~{~S~^ ~}) -> " ',',f args)
		  (let ((val (apply ,,old-f args))) 
		    (format () "~S~%" val) 
		    val)))))))

(define-expansion (_tr1_ . args)
  `(with-output-to-string
     (lambda ()
       (define (tracy . pars) pars)
       (trace tracy)
       (apply tracy ,@args ()))))

(define-expansion (_tr2_ . args)
  `(with-output-to-string
     (lambda ()
       ((lambda pars
	  (format () "(tracy ~{~S~^ ~}) -> ~S~%" pars pars))
	,@args))))
|#


(define-constant ims (immutable! (string #\a #\b #\c)))
(define-constant imbv (immutable! (byte-vector 0 1 2)))
(define-constant imbv2 (immutable! #u2d((1 2 3) (4 5 6))))
(define-constant imbv3 (immutable! #u3d(((1 2 3) (1 2 4)) ((1 2 5) (1 2 6)) ((1 2 7) (1 2 8)))))
(define-constant imv (immutable! (vector 0 1 2)))
(define-constant imv2 (immutable! #2d((1 2 3) (4 5 6))))
(define-constant imv3 (immutable! #3d(((1 2 3) (1 2 4)) ((1 2 5) (1 2 6)) ((1 2 7) (1 2 8)))))
(define-constant imiv (immutable! (int-vector 0 1 2)))
(define-constant imiv2 (immutable! #i2d((1 2 3) (4 5 6))))
(define-constant imiv3 (immutable! #i3d(((1 2 3) (1 2 4)) ((1 2 5) (1 2 6)) ((1 2 7) (1 2 8)))))
(define-constant imfv (immutable! (float-vector 0 1 2)))
(define-constant imfv2 (immutable! #r2d((1 2 3) (4 5 6))))
(define-constant imfv3 (immutable! #r3d(((1 2 3) (1 2 4)) ((1 2 5) (1 2 6)) ((1 2 7) (1 2 8)))))
(define-constant imi (immutable! (inlet 'a 3 'b 2)))
(define-constant imh (immutable! (hash-table 'a 1 'b 2)))
(define-constant imp (immutable! (cons 0 (immutable! (cons 1 (immutable! (cons 2 ())))))))
(define-constant imb (immutable! (block 0.0 1.0 2.0)))
(when with-methods
  (define-constant imfi (immutable! (mock-port (open-input-string "asdf"))))
  (define-constant imfo (immutable! (mock-port (open-output-string))))
  (define-constant imr (immutable! (mock-random-state 123456))))

(define-constant bigi0 (bignum 0))
(define-constant bigi1 (bignum 1))
(define-constant bigi2 (bignum 123))
(define-constant bigf2 (bignum 123.0))
(define-constant bigrat (bignum 1/2))
(define-constant bigflt (bignum 1.5))
(define-constant bigcmp (bignum 1+2i))

(define-constant vvv (let ((v (make-vector '(2 2)))) (set! (v 0 0) "asd") (set! (v 0 1) #r(4 5 6)) (set! (v 1 0) '(1 2 3)) (set! (v 1 1) 32) v))
(define-constant vvvi (let ((v (make-vector '(2 2)))) (set! (v 0 0) "asd") (set! (v 0 1) #r(4 5 6)) (set! (v 1 0) '(1 2 3)) (set! (v 1 1) 32) (immutable! v)))
(define-constant vvvf (immutable! (vector abs log sin)))

(define-constant a1 (immutable! (hash-table +nan.0 1)))
(define-constant a2 (immutable! (inlet :a (hash-table 'b 1))))
(define-constant a3 (openlet (immutable! (inlet :a 1))))
(define-constant a4 (subvector #i2d((1 2) (3 4))))
(define-constant a5 (subvector #i2d((1 2) (3 4)) 0 4 '(4)))
(define-constant a6 (subvector #i2d((1 2) (3 4)) 1 3 '(2 1)))

(define-constant x1 12345)
(define-constant x2 32.123)
(define-constant x3 2/31)
(define-constant x4 32.0+2.0i)
(define-constant x5 +nan.0)
(define-constant x6 +inf.0)
(define-constant x7 #\a)
(define-constant x8 :hi)
(define-constant x9 'hi)

(define typed-hash (make-hash-table 8 eq? (cons symbol? integer?)))
(define typed-vector (make-vector 8 'a symbol?))
(define typed-let (immutable! (let ((a 1)) (set! (setter 'a) integer?) (curlet))))
(define constant-let (immutable! (let () (define-constant a 1) (curlet))))

(define-constant fvset float-vector-set!)
(define-constant htset hash-table-set!)

(set! (hook-functions *unbound-variable-hook*) ())
(define max-stack (*s7* 'stack-top))
(define last-error-type #f)
(define old-definee #f)

(define (tp val) ; omits trailing " if val long and already a string
  (let ((str (object->string val)))
    (if (< (length str) 512)
	str
	(string-append (substring str 0 509) "..."))))

(define with-bignums (provided? 'bignums))
(define (bool/int? x) (or (boolean? x) (integer? x)))

(set! (hook-functions *read-error-hook*) ())

(let ((functions (vector 'not '= '+ 'cdr 'real? 'rational? 'number? '> '- 'integer? 'apply 'subvector? 'subvector-position 'subvector-vector
			  'abs '* 'null? 'imag-part '/ 'vector-set! 'equal? 'magnitude 'real-part 'pair? 'max 'nan? 'string->number 'list
			  'negative? 'cons 'string-set! 'list-ref 'eqv? 'positive? '>= 'expt 'number->string 'zero? 'floor 'denominator 'integer->char 
			  'string? 'min '<= 'char->integer 'cos 'rationalize 'cadr 'sin 'char=? 
			  'list-set! 'defined? 'memq 'string-ref 'log 
			  'for-each 'map
			  'round 'ceiling 'truncate 'string=? 'atan 'eof-object? 'numerator 'char? 'cosh 'member 'vector 
			  'even? 'string-append 'char-upcase 'sqrt 'my-make-string
			  'char-alphabetic? 'odd? 'call-with-exit 'tanh 'copy 'sinh 'make-vector
			  'string 'char-ci=? 'caddr 'tan 'reverse 'cddr 'append 'vector? 'list? 'exp 'acos 'asin 'symbol? 'char-numeric? 'string-ci=? 
			  'char-downcase 'acosh 'vector-length 'asinh 'format 'make-list 
			  'sort! 'atanh 'modulo 'make-polar 'gcd 'angle 'remainder 'quotient 'lcm 
			  'char-whitespace? 'assoc 'procedure? 'char<? 
			  'inexact->exact 'vector->list 'boolean? 'undefined? 'unspecified?
			  'caar (if with-bignums '* 'ash) 'list-tail 'symbol->string 'string->symbol 'exact->inexact 
			  'object->string 'char>? 'symbol->value 'cadar 'integer-decode-float 'string-copy 'cdddr 'logand 'cadddr 
			  'with-input-from-string 'substring 'string->list 'char-upper-case? 
			  'hash-table-set! 'cddddr 'string<? 'dynamic-wind 'call-with-input-file 'error 
			  'lognot 'cdar 'char-ci>=? 'string>=? 
			  'dilambda 'string-ci<? 'char<=? 'logior 'char-ci<=? 'assv 
			  'string>? 'char-ci>? 'char-lower-case? 'string-ci>=? 'string-ci>? 'string<=? 'caadr 'char-ci<? 
			  ;'reverse! ; quasiquoted lists are problematic
			  'string-ci<=? 'cadadr 'cdadr 'provided? 'caaaar 'caaddr 'caddar 'cdaaar 'cdaadr 'cdaddr 'cddar 
			  ;'fill! ; see _fnc6_
			  'hash-table-ref 'list->vector 'caaadr 'caaar 'caadar 'cadaar 'cdadar 'cdddar 'string-fill! 'cdaar 'cddaar 'cddadr 
			  'symbol->keyword ; 'string->keyword 'symbol ; size grows
			  'keyword->symbol 'keyword?
			  'logxor  'memv 'char-ready? 
			  'exact? 'integer-length 'port-filename 
			  'char>=? 
			  'string-length 'list->string 'inexact? 
			  'with-input-from-file 'type-of
			  'vector-fill! 
			  'peek-char 
			  'make-hash-table 'make-weak-hash-table 'weak-hash-table? 'hash-code
			  'macro? 
			  'quasiquote 
			  'immutable? 'char-position 'string-position
			  'infinite? 
			  'vector-dimensions 'vector-dimension 'vector-rank 'get-output-string 
			  'sublet 'inlet
			  
			  'call-with-input-string 'documentation 
			  'continuation? 'hash-table? 'port-closed? 'port-position 'port-file
			  'output-port? 'input-port? 
			  ;'provide 
			  'call-with-output-string 
			  'checked-hash-table
			  'with-output-to-string 
			  'dilambda?
			  'hook-functions 
			  'c-pointer->list 'c-pointer-info 'c-pointer-type 'c-pointer-weak1 'c-pointer-weak2
			  ;'show-profile

			  ;'make-hook 
			  'let 'let* 'letrec 'letrec*
			  'lambda 'lambda*  ; these cause built-ins to become locals if with-method=#f?
			  'macro 'macro* 'bacro 'bacro* ; -- same as lambda above
			  ;'define* 'define-macro 'define-macro* 'define-bacro 'define-bacro*
			  ;'multiple-value-bind ; (multiple-value-bind (if) ...) gets all kinds of trouble
			  'call-with-values
			  'object->let

			  'open-input-string 'open-output-string 
			  'open-input-file 
			  'open-input-function 'open-output-function
			  ;'define
			  'newline
			  'random-state 
			  'gensym
			  'case*
			  ;'do
			  ;'if 'begin 'cond 'case 'or 'and 'with-let 'with-baffle 'when 'unless 'let-temporarily
			  'byte-vector-set! 'my-make-byte-vector 
			  'write-char 'call/cc 'write-byte 'write-string 
			  'file-mtime
			  'write 'display 
			  ;'outlet 
			  'directory->list 
			  ;'set!
			  'set-car! 
			  'call-with-output-file 'with-output-to-file 
			  ;'read-char 'read-byte 'read-line 'read-string 'read ; stdin=>hangs
			  'checked-read-char 'checked-read-line 'checked-read-string 'checked-read-byte ;'checked-read
			  'checked-reverse! 'checked-port-line-number 
			  'checked-varlet 'checked-cutlet
			  'close-input-port 
			  ;'current-input-port ;-- too many (read...)
			  ;'set-current-input-port ; -- collides with rd8 etc
                          ;'set-cdr!
                          ;'unlet ;-- spurious diffs
                          'port-line-number ;-- too many spurious diffs
			  'load  ;'current-error-port ;-- spurious output
			  ;'close-output-port 
			  'hash-table ; -- handled as equivalent via checked-hash-table
			  ;'current-output-port 
			  ;'cutlet 
			  ;'set-current-error-port ;-- too many bogus eq? complaints
			  ;'define-constant 
			  ;'curlet ; (length (curlet)) too many times
 			  ;'open-output-file
			  ;'delete-file 'set-current-output-port 
			  'autoload
			  ;'varlet ;-- error exits, chaos in rootlet
			  ;'eval ; -- can't use if signature (circular program) or (make-list (max-list-len))
			  'checked-eval
			  ;'immutable!
			  'checked-procedure-source
			  ;'owlet ;too many uninteresting diffs
			  ;'gc  ; slower?
			  ;'reader-cond ;-- cond test clause can involve unbound vars: (null? i) for example
			  ;'funclet ; '*function* ; tons of output
			  ;'random 
			  ;;; 'quote
			  '*error-hook*
			  ;'cond-expand 
			  ;'random-state->list 
                          ;'pair-line-number 'pair-filename ; -- too many uninteresting diffs
			  ;'let-set! ;-- rootlet troubles?
			  ;'coverlet ;-- blocks block's equivalent?
                          'help ;-- snd goes crazy
			  'macroexpand ;-- uninteresting objstr stuff
			  'signature ; -- circular lists cause infinite loops with (e.g.) for-each??
			  'eval-string 
			  'tree-memq 'tree-set-memq 'tree-count 'tree-leaves
			  'tree-cyclic?
                          'require
			  'else '_mac_ '_mac*_ '_bac_ '_bac*_ 
			  '_fnc_ '_fnc*_ '_fnc1_ '_fnc2_ '_fnc3_ '_fnc4_ '_fnc5_ ;'_fnc6_
			  '=>
			  
			  'constant?
			  'openlet 
			  '*unbound-variable-hook* '*load-hook* '*rootlet-redefinition-hook* '*missing-close-paren-hook* ;'*read-error-hook* ;'*after-gc-hook*
			  '*autoload*
			  'sequence? 'directory? 'hash-table-entries 
			  'arity 'logbit? 
			  'random-state? 'throw 'float-vector-set! 'make-iterator 'complex 
			  'let-ref 'int-vector 'aritable? 'gensym? 'syntax? 'iterator-at-end? 'let? 
			  'subvector 'float-vector 'iterator-sequence 'getenv 'float-vector-ref 
			  'cyclic-sequences 'let->list 
			  
			  'setter 'int-vector? 
			  'int-vector-set! 'c-object? 'c-object-type 'proper-list? 'symbol->dynamic-value 'vector-append 
			  'flush-output-port 'c-pointer 'make-float-vector 
			  'iterate 'float-vector? 
			  'apply-values
			  'values
			  'byte-vector-ref 'file-exists? 'make-int-vector 'string-downcase 'string-upcase 
			  'byte-vector 'equivalent? 
			  'c-pointer? 'int-vector-ref
			  'float? 
			  'list-values 'byte-vector? 'openlet? 'iterator? 
			  'string->byte-vector 'byte-vector->string

			  'checked-pp
#|
			  's7-catches 
			  's7-stack-top 's7-stack 
			  's7-symbol-table 
			  ;'s7-stacktrace-defaults ; clobbered by reverse!
			  ;'s7-set-print-length 
			  ;'s7-set-stacktrace-defaults
			  ;'s7-set-default-rationalize-error
			  ;'s7-set-equivalent-float-epsilon 
			  ;'s7-set-hash-table-float-epsilon 
			  ;'s7-set-bignum-precision 
			  ;'s7-set-float-format-precision ; tedious quibbles
			  's7-set-max-string-length 
			  's7-set-max-list-length 
			  's7-set-max-vector-length 
			  's7-set-max-vector-dimensions
			  's7-set-default-hash-table-length
			  's7-set-initial-string-port-length
			  's7-set-undefined-identifier-warnings 's7-set-autoloading? 's7-set-max-stack-size
			  's7-set-gc-stats
			  's7-set-safety
|#
			  's7-undefined-identifier-warnings 
			  's7-profile-info 
			  's7-autoloading? 

			  's7-safety 
			  's7-c-types 
			  ;;'s7-history ;-- causes stack to grow?
			  ;'s7-print-length 's7-max-string-length 's7-max-list-length 's7-max-vector-length 's7-max-vector-dimensions 's7-default-hash-table-length
			  's7-initial-string-port-length 's7-history-size
			  's7-default-rationalize-error 's7-equivalent-float-epsilon
			  's7-hash-table-float-epsilon 's7-bignum-precision 
			  's7-float-format-precision 
			  ;'s7-default-random-state 
			  ;'s7-cpu-time
			  ;;'s7-file-names ; one is *stdin* -> infinite loop if read*
			  ;'s7-rootlet-size 's7-heap-size 's7-free-heap-size 's7-gc-freed 's7-stack-size 's7-max-stack-size 's7-gc-stats

			  'block 'make-block 'block? 'block-ref 'block-set! 
			  'blocks 'unsafe-blocks 'blocks1 'unsafe-blocks1 'blocks3 'unsafe-blocks3 'blocks4 'unsafe-blocks3 'blocks5 
			  'block-reverse! 'subblock 'block-append 'block-let
			  'simple-block? 'make-simple-block ;'make-c-tag ; -- uninteresting diffs

			  'fvref 'ivref 'vref 'fvset 'ivset 'vset 'adder

			  'undefined-function
			  ;'subsequence 
			  'empty? 'indexable?
			  ;'adjoin 'cdr-assoc
			  'progv ;'value->symbol -- correctly different values sometimes, progv localizes
			  ;'and-let* 'string-case 'concatenate
			  '2^n? 'lognor 'ldb 'clamp 
			  ; reverse! etc 

			  'log-n-of ; uninteresting complaints
			  ;;'sandbox -- slow
			  'circular-list? ;;'hash-table->alist -- hash map order problem
			  'weak-hash-table 'byte? 'the 'lognand 'logeqv 
			  'local-random 'local-read-string 'local-varlet 'local-let-set!
			  'pp-checked
			  'kar '_dilambda_ '_vals_ '_vals1_ '_vals2_ 
                          '_vals3_ '_vals4_ '_vals5_ '_vals6_ '_vals3s_ '_vals4s_ '_vals5s_ '_vals6s_ 
                          '_svals3_ '_svals4_ '_svals5_ '_svals6_ '_svals3s_ '_svals4s_ '_svals5s_ '_svals6s_ 
			  ;'match?
			  'catch 'length 'eq? 'car '< 'assq 'complex? 'vector-ref 
			  ;'linter
			  'ifa 'ifb ; place-holders
			  ))
	 
      (args (vector "-123" "1234" "-3/4" "-1" "1/2" "1+i" "1-i" "0+i" "0-i" "(expt 2 32)" "4294967297" "1001" "10001"
		    "3441313796169221281/1720656898084610641" "1855077841/1311738121" "4478554083/3166815962" "20057446674355970889/10028723337177985444"
		    "(cosh 128)" "(cosh (bignum 128.0))" "(bignum -1/2)" "123456789.123456789" "(bignum 1234)" "(bignum 1234.1234)" "(bignum 1+i)"
		    "(bignum +inf.0)" "(bignum +nan.0)" "(bignum -inf.0)" "(bignum 0+i)" "(bignum 0.0)" "(bignum 0-i)"
#|
		    ;(reader-cond ((provided? 'gmp)
				  "(if (> (random 1.0) 0.5) -123 (bignum -123))"
				  "(if (> (random 1.0) 0.5) 1/3 (bignum 1/3))"
				  "(if (> (random 1.0) 0.5) -2.0 (bignum -2.0))"
				  "(if (> (random 1.0) 0.5) 1+i (bignum 1+i))"
				  "(if (> (random 1.0) 0.5) 8796093022208 (bignum 8796093022208))"
				  "(if (> (random 1.0) 0.5) 1855077841/1311738121 (bignum 1855077841/1311738121))"
				  "(if (> (random 1.0) 0.5) 1.0e18 (bignum 1.0e18))"
				  "(if (> (random 1.0) 0.5) +inf.0 (bignum +inf.0))"
				  "(if (> (random 1.0) 0.5) +nan.0 (bignum +nan.0))"
				  "(if (> (random 1.0) 0.5) 0+0/0i (bignum 0+0/0i))"
				  "(if (> (random 1.0) 0.5) 0/0+0/0i (bignum 0/0+0/0i))"
				  "(if (> (random 1.0) 0.5) 9223372036854775806 (bignum 9223372036854775806))"
				  "(if (> (random 1.0) 0.5) (vector 123) (vector (bignum 123)))"
				  "(if (> (random 1.0) 0.5) 2.5 (bignum 2.5))"
				  "(if (> (random 1.0) 0.5) 1.5 (bignum 1.5))"
				  "(if (> (random 1.0) 0.5) 5/2 (bignum 5/2))"
				  "(if (> (random 1.0) 0.5) 3/2 (bignum 3/2))"
				  
				  "18446744073709551614"
				  "18446744073709551614/3"
				  "18446744073709551615.0"
				  "18446744073709551614+i"
				  "-18446744073709551616"
				  "-18446744073709551616/3"
				  "-18446744073709551617.0"
				  "-18446744073709551616-18446744073709551614i"
				  "9223372036854775807"
				  "-9223372036854775807"
				  "9007199254740992"
				  "9223372036854775800.1"
				  "-9223372036854775800.1+i"
				  "92233720368547758/19"
				  "92233720368547758/5"
				  "-4611686018427387905"
				  "4611686018427387904"
				  ;))
|#

		    "=>" 
		    "\"ho\"" ":ho" "'ho" "(list 1)" "(list 1 2)" "(cons 1 2)" "'()" "(list (list 1 2))" "(list (list 1))" "(list ())" "=>" 
		    "#f" "#t" "()" "#()" "\"\"" "'#()" ; ":write" -- not this because sr2 calls write and this can be an arg to sublet redefining write
		    ":readable" ":rest" ":allow-other-keys" ":a" ":frequency" ":scaler" ; for blocks5 s7test.scm
		    "1/0+i" "0+0/0i" "0+1/0i" "1+0/0i" "0/0+0/0i" "0/0+i" "+nan.0-3i" "+inf.0-nan.0i"
		    "cons" "\"ra\"" "''2"
		    "#\\a" "#\\A" "\"str1\"" "\"STR1\"" "#\\0" "0+." ".0-"
		    "(make-hook)" "(make-hook '__x__)"
		    "1+i" "0+i" "(ash 1 43)"  "(fib 8)" "(fibr 8)" "(fibf 8.0)"
		    "(integer->char 255)" "(string (integer->char 255))" "(string #\\null)" "(byte-vector 0)"
		    "pi" "+nan.0" "+inf.0" "-inf.0" "-nan.0"
		    "(list)" "(string)" "#r()" "#u()" "(vector)" "#i()" "(make-iterator #(10 20))" "#i(1)"
		    "0" "1" "4" "1.0" "-1.0" "1.0+123.0i" "3/4" "(make-vector 3)" "(make-string 3 #\\space)" "(make-vector '(2 3))"
		    "'((111 2222) (3 4))" "'((1 (2)) (((3) 4)))" "(byte-vector 255)" "(make-byte-vector '(2 3) 0)"
		    "#(123 223)" "(vector 1 '(3))" "(let ((x 3)) (lambda (y) (+ x y)))" "abs" "(lambda sym-args sym-args)" "#u(0 1)"
		    "'((1) (vector 1))" "(abs x)" "(symbol? x)" "(cons x x)" 
		    "(dilambda (lambda () 1) (lambda (a) a))" "quasiquote" "macroexpand" "(lambda* ((a 1) (b 2)) (+ a b))" 
		    "(dilambda (lambda args args) (lambda args args))" "(dilambda (lambda* (a b) a) (lambda* (a b c) c))"
		    "((lambda (a) (+ a 1)) 2)" "((lambda* ((a 1)) (+ a 1)) 1)" "(lambda (a) (values a (+ a 1)))" "((lambda (a) (values a (+ a 1))) 2)"
		    "(define-macro (_m1_ a) `(+ ,a 1))" "(define-bacro (_b1_ a) `(* ,a 2))"
		    "(macro (a) `(+ ,a 1))" "(bacro (a) `(* ,a 2))" "(macro* (a (b 1)) `(+ ,a ,b))" "(bacro* (a (b 2)) `(* ,a ,b))"
		    "(string #\\c #\\null #\\b)" "#2d((100 200) (3 4))" "#r(0 1)" "#i2d((101 201) (3 4))" "#r2d((.1 .2) (.3 .4))" "#i1d(15 25)"
		    "(values 1 2)" "(values)" "(values #\\c 3 1.2)" "(values \"ho\")" "(values 1 2 3 4 5 6 7 8 9 10)" "(values (define b1 3))"
		    "(apply values (make-list 128 1/2))"
		    "(values 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65)"
		    "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24"
		    "(log 1.0) (log 2.0)"
		    "(log 1.0) (log 2.0) (log 3.0)"
		    "(log 1.0) (log 2.0) (log 3.0) (log 4.0)"
		    "(log 1.0) (log 2.0) (log 3.0) (log 4.0) (log 5.0)"
		    ;"(log 1.0) (log 2.0) (log 3.0) (log 4.0) (log 5.0) (log 6.0) (log 7.0) (log 8.0) (log 9.0) (log 10.0) (log 11.0) (log 12.0) (log 13.0) (log 14.0) (log 15.0) (log 16.0) (log 17.0) (log 18.0) (log 19.0) (log 20.0) (log 21.0) (log 22.0) (log 23.0) (log 24.0) (log 25.0) (log 26.0) (log 27.0) (log 28.0) (log 29.0) (log 30.0) (log 31.0) (log 32.0) (log 33.0) (log 34.0) (log 35.0) (log 36.0) (log 37.0) (log 38.0) (log 39.0) (log 40.0) (log 41.0) (log 42.0) (log 43.0) (log 44.0) (log 45.0) (log 46.0) (log 47.0) (log 48.0) (log 49.0) (log 50.0) (log 51.0) (log 52.0) (log 53.0) (log 54.0) (log 55.0) (log 56.0) (log 57.0) (log 58.0) (log 59.0) (log 60.0) (log 61.0) (log 62.0) (log 63.0) (log 64.0) (log 65.0) (log 66.0) (log 67.0) (log 68.0) (log 69.0) (log 70.0) (log 71.0) (log 72.0) (log 73.0) (log 74.0) (log 75.0) (log 76.0) (log 77.0) (log 78.0) (log 79.0) (log 80.0) (log 81.0) (log 82.0) (log 83.0) (log 84.0) (log 85.0) (log 86.0) (log 87.0) (log 88.0) (log 89.0) (log 90.0) (log 91.0) (log 92.0) (log 93.0) (log 94.0) (log 95.0) (log 96.0) (log 97.0) (log 98.0) (log 99.0) (log 100.0)"
		    "`(x)" "`(+ x 1)" "`(x 1)" "`((x))" "`((+ x 1))" "`(((+ x 1)))" "`((set! x (+ x 1)) (* x 2))" "`((x 1))" "`(((x 1))) "
		    "`(x . 1)" "`((x . 1))" "`(1)" "`((1))" "`((1) . x)" "'(- 1)" 
		    "(+ i 1)" "(pi)"
		    "'(())" "'((()))" "(random-state 1234)" 
                    "((if (> 3 2) abs log) 1)" "((if (> 3 2) + -) 3 2)"
		    "((if (> 3 2) or and) #t #f)"

                    ;; experiments -- trying to hit every eval branch
                    "(apply + (make-list 2 3))" "(let ((a 1) (b 2) (c 3)) (+ a b c))" "(let ((x '(\"asdf\"))) (apply format #f x))"
                    "(cons (cons + -) *)" "(list (list quasiquote +) -1)" "(let ((s '(1 2))) (list (car s) (cdr s)))"
                    "(let ((i 3)) (list i (expt 2 i)))" "(more-values)" "(- (+ x x) (* x x))"

		    "(c-pointer 0 'integer?)" "(c-pointer -1)" "(c-pointer 1234 1.0 2.0 3.0)" "(c-pointer (bignum 1) (vector) (vector 1) (vector 2))"
		    "(inlet 'integer? (lambda (f) #f))" "(inlet 'a 1)" 
		    "(openlet (inlet 'abs (lambda (x) (if (real? x) (if (< x 0.0) (- x) x) (error 'wrong-type-arg \"not a real\")))))"
		    "(openlet (inlet 'zero? (lambda (x) (if (number? x) (= x 0.0) (error 'wrong-type-arg \"not a number\")))))"
		    "(inlet 'a (inlet 'b 1))"
		    "'(15 26 . 36)" 
		    ;" . " ; -- read-errors
		    "((i 0 (+ i 1)))" ;"(null? i)" 
		    "(= i 2)" "(zero? i)" "((null? i) i)"
		    "(#t ())" 
		    "`(+ ,a ,@b)" "`(+ ,a ,b)" "`(+ ,a ,b ,@c)" "`(+ ,a b ,@c ',d)"
		    "_definee_"
		    "(hash-table 'a 1.5)" "(hash-table)" "(hash-table 'a (hash-table 'b 1))"
		    "(weak-hash-table 1.0 'a 2.0 'b 3.0 'c)"
		    "(make-iterator (list 11 22 33))" "(make-iterator (int-vector 1 2 3))" "(make-iterator (string #\\1))" "(make-iterator x)" 
		    "(make-iterator (make-vector '(2 3) #f))" "(make-iterator #r())"
		    "(make-iterator (hash-table 'a -1/2 'b 2))" 
		    "(make-iterator (weak-hash-table 1.0 'a 2.0 'b 3.0 'c))"
		    "(make-iterator (let ((lst '((a . 1) (b . 2) (c . 2)))
                                          (+iterator+ #t)) 
                                      (lambda () 
                                        (if (pair? lst) 
                                            (let ((res (list (caar lst) (cdar lst))))
                                              (set! lst (cdr lst)) res)
                                            #<eof>))))"
		    "(display (call/cc (lambda (return)
					(let ((val \"line 1~%line 2~%line 3\"))
					  (call-with-input-string val
					    (lambda (p) (return 'oops)))))))"
		    "(display (call-with-exit (lambda (return)
					(let ((val \"line 1~%line 2~%line 3\"))
					  (with-input-from-string val
					    (lambda (p) (return 'oops)))))))"
		    "(display (call/cc (lambda (return)
					  (call-with-output-string
					    (lambda (p) (return 'oops))))))"

		    "#<eof>" "#<undefined>" "#<unspecified>" "#unknown" "___lst" "#<bignum: 3>" 
		    "#<>" "#<label:>" "#<...>"
		    "#_and" "'#_or" "#_abs" "#_+" 
		    "#o123" "#b101" "#\\newline" "#\\alarm" "#\\delete" "#_cons" "#x123.123" "#\\x65"
		    "(provide 'pizza)" "(require pizza)"
		    
		    "(call-with-exit (lambda (goto) goto))"
		    "(with-baffle (call/cc (lambda (cc) (cc 1))))"
		    "(symbol->string 'x)" "(symbol \"a b\")" "(symbol \"(\\\")\")"
		    "(call-with-exit (lambda (return) (return 'ce)))"
		    "(call-with-exit (lambda (return) (let ((x 1) (y 2)) (return x y))))"
		    "(call/cc (lambda (return) (return 'oops)))"
		    "(call/cc (lambda (return) (let ((x 1) (y 2)) (return x y))))"
		    "(let ((x 1)) (dynamic-wind (lambda () (set! x 2)) (lambda () (+ x 1)) (lambda () (set! x 1))))"

		    "(let-temporarily ((x 1)) x)" "(let-temporarily ((x #(1)) (i 0)) i)" 

		    "1+1e10i" "1e15-1e15i" "0+1e18i" "-1e18" 
		    "(begin (real-part (random 0+i)))"
		    ;"(random 1.0)" ; number->string so lengths differ
		    "(random 1)"
		    ;"(else ())" "(else (f x) B)"
		    "(else)"
		    "else" "x" "(+ x 1)" "(+ 1/2 x)" "(abs x)" "(+ x 1 2+i)" "(* 2 x 3.0 4)" "((x 1234))" "((x 1234) (y 1/2))" "'x" "(x 1)"
		    "_undef_" "(begin |undef1|)"

		    "+signature+" "+documentation+" "+setter+" "+iterator+"
		    "(let ((+documentation+ \"help\")) (lambda (x) x))"
		    "(let ((+iterator+ #t)) (lambda () #<eof>))"
		    "(let ((+signature+ (list 'integer? 'integer?))) (lambda (x) (logand x 1)))"
		    "(let ((x 1)) (let ((+setter+ (lambda (val) (set! x val)))) (lambda () x)))"

		    "__var2__"
		    ; "\"~S~%\"" "\"~A~D~X\"" "\"~{~A~^~}~%\"" "\"~NC~&\"" ; -- creates files by these names?
		    "(call-with-input-file \"s7test.scm\" (lambda (p) p))"
		    "(call-with-output-string (lambda (p) p))"

		    "ims" "imbv" "imv" "imiv" "imfv" "imi" "imp" "imh"
		    "imv2" "imv3" "imfv2" "imfv3" "imiv2" "imiv3" "imbv2" "imbv3"
		    "vvv" "vvvi" "vvvf" "typed-hash" "typed-vector" "typed-let" "constant-let"
		    "a1" "a2" "a3" "a4" "a5" "a6"
		    "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9"

		    "(make-hash-table 8 eq? (cons symbol? integer?))"
		    "(make-hash-table 8 equivalent? (cons symbol? #t))"
		    "(let ((a 1)) (set! (setter 'a) integer?) (curlet))"

		    "bigi0" "bigi1" "bigi2" "bigrat" "bigflt" "bigcmp" "bigf2"
		    "(ims 1)" "(imbv 1)" "(imv 1)" "(imb 1)" "(imh 'a)"

		      "(make-iterator (block 1 2 3))"
		      "(vector-dimensions (block))" 
		      "(append (block) (block))"
		      "(make-vector 3 (block 0.0) block?)"
		      "(make-hash-table 8 #f (cons symbol? block?))"
		      "(make-block 2)" "(block 1.0 2.0 3.0)" "(block)"
		      "imb" 

		    (reader-cond
		     (with-methods
		      "imfi" "imfo" "imr"
		      "(mock-number 0)" "(mock-number 1-i)" "(mock-number 4/3)" "(mock-number 2.0)"
		      "(mock-string #\\h #\\o #\\h #\\o)"
		      "(mock-pair 2 3 4)"
		      "(mock-char #\\b)"
		      "(mock-symbol 'c)"
		      "(mock-vector 1 2 3 4)"
		      "(mock-hash-table 'b 2)"
		      "(mock-c-pointer -1)"
		      "(mock-random-state 1234)"))
		    "'value"

		    "(subvector 0 3 (vector 0 1 2 3 4))" "(substring \"0123\" 2)"
		    "(let-temporarily ((x 1234)) (+ x 1))"
		    "(error 'oops \"an error!\")"
		    "(define b2 32)"

		    ;"quote" "when" ;"'" "(quote)" "(quote . 1)" "(when)" "(when . 1)"
		    ;"if" ; causes stack overflow when used as lambda arg name and (()... loop)
		    "begin" "cond" "case" "when" "unless" "letrec" "letrec*" "or" "and" "let-temporarily"
		    ;"lambda*" "lambda" ;-- cyclic body etc
		    ;"let" "let*" "do" "set!" "with-let" ;"define" "define*" "define-macro" "define-macro*" "define-bacro" "define-bacro*"

		    "(let ((<1> (vector #f))) (set! (<1> 0) <1>) <1>)"
		    "(let ((<1> (inlet :a #f))) (set! (<1> :a) <1>) <1>)"
		    "(let ((<1> (hash-table))) (set! (<1> 'a) <1>) <1>)"
		    "(let ((<1> #f) (<2> (vector #f))) (set! <1> (make-iterator <2>)) (set! (<2> 0) <1>) <1>)"
		    "(let ((<1> (list 1 #f))) (set! (<1> 1) (let ((<L> (list #f 3))) (set-car! <L> <1>) <L>)) <1>)"
		    "(let ((cp (list 1))) (set-cdr! cp cp) (list '+ 1 (list 'quote cp)))"

		    "(let ((lst (list '+ 1))) (set-cdr! (cdr lst) (cdr lst)) (apply lambda () lst ()))"
		    "(let ((lst (list '+ 1))) (set-cdr! (cdr lst) (cdr lst)) (apply lambda* () lst ()))"

		    "(gensym \"g_123\")"
		    "(make-list 256 1)"
		    "(make-vector '(2 3) 1)"             "(make-vector '(12 14) #<undefined>)"
		    "(make-byte-vector '(2 3) 1)"        "(make-byte-vector '(4 32) 255)"
		    "(make-string 256 #\\1)"             "(make-string 1024 #\\null)"
		    "(make-int-vector '(2 3) 1)"         "(make-int-vector '(2 128) (*s7* 'most-negative-fixnum))"
		    "(make-float-vector '(2 3) 1)"       "(make-float-vector '(128 3) pi)"
		    "(make-vector 3 'a symbol?)"         "(make-vector '(2 3 4 3 2 3 4) 1)"
		    "(make-vector 3 1+i complex?)"       "(make-vector (make-list 10 2))"
		    "(make-vector 3 #<eof> eof-object?)" "(make-vector (make-list 256 1))"
		    "(make-vector 3 '(1) pair?)"
		    "(make-vector 3 :rest keyword?)"
		    "(make-vector '(2 3) boolean?)"
		    "(make-vector '(2 3) symbol?)"
		    "(make-hash-table 8 #f (cons symbol? integer?))"
		    "(let ((i 32)) (set! (setter 'i) integer?) (curlet))"

		    "(make-vector 3 #f bool/int?)"
		    "(make-float-vector 3 1.0 (lambda (x) (< x pi)))"
		    "(make-int-vector 3 1 (lambda (x) (< x 3)))"
		    "(make-byte-vector 3 1 (lambda (x) (> x 0)))"
		    "(let ((a 1.0)) (set! (setter 'a) (lambda (x) (real? x))) (curlet))"
		    "(make-hash-table 8 #f (cons (lambda (x) (symbol? x)) (lambda (x) (integer? x))))"
		    "(make-vector 3 #f (lambda (x) #f))" ; if not an error, someone forgot to call it
		    "(make-hash-table 8 #f (cons (lambda (x) #f) (lambda (x) #f)))" ; same
		    "(make-vector 3 #f (let ((calls 0)) (lambda (x) (set! calls (+ calls 1)) (= calls 1))))" ; 2 calls = error I hope

		    "(immutable! #(1 2))" "(immutable! #r(1 2))" "(immutable! \"asdf\")" "(immutable! '(1 2))" "(immutable! (hash-table 'a 1))"
		    "(lambda (x) (fill! (copy x) 0))"

		    "(begin (list? (*s7* 'catches)))"
		    "(begin (integer? (*s7* 'stack-top)))"
		    "(begin (list? (*s7* 'stacktrace-defaults)))"

		    "(let loop ((i 2)) (if (> i 0) (loop (- i 1)) i))"

		    ;"(rootlet)" ;"(curlet)"
		    "(make-simple-block 3)"
		    "*s7*"

		    "(symbol (make-string 130 #\\a))"
		    "(symbol \"1\\\\\")" "#\\xff"  "#\\backspace" ":0" "(list (list 1 2) (cons 1 2))" 
		    "#i2d((1 1 1) (2 2 2) (1 1 1))" "(subvector (vector 1 2 3 4 5 6) 0 6 '(2 3))"
		    "(let ((<1> (vector #f #f #f))) (set! (<1> 0) <1>) (set! (<1> 1) <1>) (set! (<1> 2) <1>) <1>)"
		    "#i3d(((1 2 3) (3 4 5)) ((5 6 1) (7 8 2)))"
		    "(hash-table +nan.0 1)" "#\\7" "(inlet :a (hash-table 'b 1))" "(openlet (immutable! (inlet :a 1)))"
		    "(subvector #i2d((1 2) (3 4)))" "(subvector #i2d((1 2) (3 4)) 0 4 '(4))" "(subvector #i2d((1 2) (3 4)) 1 3 '(2 1))"

		    "my-let" "my-with-baffle" "fvset" "htset"
		    #f #f #f
		    ))

      (codes (vector 
	      (list "(do ((x 0.0 (+ x 0.1)) (i 0 (+ i 1))) ((>= x .1) " "(let ((x 0.1) (i 1)) (begin ")
	      (list "(do ((x 0) (i 0 (+ i 1))) ((= i 1) x) (set! x " "(let ((x 0) (i 0)) (set! x ")
	      ;; (list "((lambda x " "((lambda* ((x ())) ")
	      ;;(list "((lambda* ((x 1)) " "(let* ((_aaa_ 1) (x _aaa_)) (begin ")
	      (list "(cond (else " "(case x (else ")
	      (list "(case false ((#f) " "(case false ((1) #t) (else ")
	      (list "(call-with-exit (lambda (_x_) " "(call/cc (lambda (_x_) ")
	      (list "(if (not x) (begin " "(if x #<unspecified> (begin ")
	      (list "(cond ((not false) " "(unless false (begin ")
	      (list "(let () (let-temporarily ((x 1)) " "(let () (let ((x 1)) ")
	      (list "(begin (_let1_ " "(begin (_let2_ ")
	      (list "(begin (_dw_ " "((lambda () ")
	      (list "(begin (append " "(apply append (list ")
	      (list "(begin (with-let (inlet 'i 0) " "(with-let (inlet) (let ((i 0)) ")
	      (list "(list (_cw_ " "(list (values ")
	      (list "(do () ((not false) " "(begin (when (not false) ")
	      ;;(list "((define-macro (_m_) " "((define-bacro (_m_) ") ; circular source if signature in body
	      (list "(for-each display (list " "(for-each (lambda (x) (display x)) (list ")
	      (list "(begin (_ct1_ " "(begin (_ct2_ ")
	      (list "(with-output-to-string (lambda () " "(begin (_dw_out_ ")
	      (list "(begin (_rf1_ " "(begin (_rf2_ ")
	      (list "(begin (_rf1_ " "(begin (_rf3_ ")
	      (list "(let () (_do1_ " "(let () (_do2_ ")
	      (list "(let () (let-temporarily ((x 1234)) (call-with-exit (lambda (goto) (goto 1))) "
		    "(let () (let-temporarily ((x 1234)) (call/cc (lambda (goto) (goto 1))) ")
	      (list "(let ((x 1)) (immutable! 'x) (begin " "((lambda* ((x 1)) (immutable! 'x) ")
	      (list "(do ((i 0 (+ i 1))) ((= i 1)) (do ((j 0 (+ j 1))) ((= j 1)) "
		    "(do ((i 0 (+ i 1))) ((= i 1)) (let ((j 0)) ")
	      (list "(or (_cop1_ " "(and (_cop2_ ")
	      (list "(let () (_do4_ " "(let () (_do5_ ")
	      (list "(begin (_ft1_ " "(begin (_ft2_ ")
	      (list "(begin (_rd3_ " "(begin (_rd4_ ")
	      (list "(begin (_rd5_ " "(begin (_rd6_ ")
	      (list "(begin (_rd7_ " "(begin (_rd8_ ")
	      (list "(format #f \"~S\" (list " "(object->string (list ")
	      (list "(begin (_wr1_ " "(begin (_wr2_ ")
	      (list "(begin (_wr3_ " "(begin (_wr4_ ")
	      (list "(begin (vector " "(apply vector (list ")
	      (list "(begin (string " "(apply string (list ")
	      (list "(begin (float-vector " "(apply float-vector (list ")
	      (list "(begin (values " "(apply values (list ")
	      (list "(vector (values " "(apply vector (list ")
	      (list "(vector 1 (values " "(apply vector (list 1 ")
	      (list "(begin (let ((x 0)) (set! (setter 'x) integer?) " 
		    "(begin (let ((x 0)) (set! (setter 'x) (lambda (s v) (if (integer? v) v (error \"setter ~A not integer\" v)))) ")

	      (list "(begin (do ((i 0 (+ i 1))) ((= i 1)) " "(let ((__x__ 1)) (do ((i 0 (+ i __x__))) ((= i __x__)) ")

	      ;(list "(cond ((= x 0) " "(begin (when (= x 0) ")
	      ;(list "(_fe1_ " "(_fe2_ ")
	      ;(list "(_fe3_ " "(_fe4_ ")
	      ;(list "(begin (_tr1_ " "(begin (_tr2_ ")
	      ;(list "(begin (_mem1_ " "(begin (_mem2_ ")
	      ;(list "(list (_cw_ " "(list (_cc_ ")

              (list "(begin ((lambda (a) (sort! a >)) " "(begin ((lambda (a) (sort! a (lambda (x y) (not (<= x y))))) ")
	      (list "(begin (_iter_ " "(begin (_map_ ")
	      (list "(begin (_cat1_ " "(begin (_cat2_ ")
	      (list "(begin (let ((+ -)) " "(begin (let () (define + -) ")
	      (list "(let ((+ -)) (let ((cons list)) " "(let ((cons list)) (let ((+ -)) ")
	      (list "(let () (with-baffle " "(let ((mwb with-baffle)) (mwb ")
	      ))
      
      (chars (vector #\( #\( #\) #\space))) ; #\' #\/ #\# #\, #\` #\@ #\. #\:))  ; #\\ #\> #\space))

  (let ((clen (length chars))
	(flen (length functions))
	(alen (length args))
	(codes-len (length codes))
	(args-ran (+ 3 (random 5)))
	(both-ran (+ 4 (random 8))))

    (define (get-arg)
      (let ((str (args (random alen))))
	(if (string? str) ; else #f
	    str
	    (cycler (+ 3 (random 3))))))

    (define (rf-symbol->string sym)
      (if (or (> (random 100) 5)
	      (syntax? sym)
	      (syntax? (symbol->value sym)))
	  (symbol->string sym)
	  (if (< (random 10) 3) 
	      (string-append "(let () " (symbol->string sym) ")")
	      (if (< (random 10) 3)
		  (string-append "((vector " (symbol->string sym) ") 0)")
		  (string-append "#_" (symbol->string sym))))))

    (define (fix-op op)
      (case op
	((set!) "set! _definee_") ;"set!")
	((let) "let ()")   ; need to block infinite loops like (let abs () (abs))
	((let*) "let* ()")
	((do) "_do3_")
	((call-with-output-file) "call-with-output-file \"/dev/null\" ")
	((with-output-to-file) "with-output-to-file \"/dev/null\" ")
	((define define* define-macro define-macro* define-bacro define-bacro*) (format #f "~A _definee_ " op))
	((eval) "checked-eval")
	((ifa) "(if (integer? _definee_) + -)")
	((ifb) "(if (integer? _definee_) when unless)")
	(else => rf-symbol->string)))

    (define make-expr
      (let ((parens 1)
	    (dqs 0)
	    (j 1)
	    (str (make-string 8192 #\space)))
	(lambda (size)
	  (set! parens 1)
	  (set! dqs 0)
	  (set! j 1)
	  (fill! str #\space)
	  (set! (str 0) #\()

          (let ((opstr (fix-op (functions (random flen)))))
            (string-copy opstr str j)
            (set! j (+ j (length opstr))))
	  
	  (set! (str j) #\space)
	  (set! j (+ j 1))
	  
	  (do ((k 1 (+ k 1)))
	      ((= k size))
	    
	    (set! (str j) (chars (random clen)))
	    (if (= dqs 1)
		(if (and (char=? (str j) #\")
			 (or (= j 0)
			     (not (char=? (str (- j 1)) #\\))))
		    (set! dqs 0))
		
		;; else not in a string constant
		(case (str j)
		  ((#\()
		   (set! parens (+ parens 1))

                   (let ((opstr (fix-op (functions (random flen)))))
                     (string-copy opstr str (+ j 1))
                     (set! j (+ j 1 (length opstr))))
		   (set! (str j) #\space))
		  
		  ((#\))
		   (set! parens (- parens 1))
		   (when (negative? parens)
		     (set! (str j) #\space)
		     (set! parens 0)))
		  
		  ((#\space)
		   (let ((nargs (random args-ran)))
		     (do ((n 0 (+ n 1)))
			 ((= n nargs))

                       (let ((argstr (get-arg)))
                         (string-copy argstr str (+ j 1))
                         (set! j (+ j (length argstr) 1)))
		       ;(set! j (+ j 1))
		       (set! (str j) #\space))))
		  
		  ((#\")
		   (set! dqs 1))))
	    
	    (set! j (+ j 1)))
	  
	  (if (= dqs 1)
	      (begin
		(set! (str j) #\")
		;(set! j (+ j 1))
		))
	  
	  (if (> parens 0)
	      (do ((k parens (- k 1))
		   (n j (+ n 1)))
		  ((= k 0)
		   (set! j n))
		(string-set! str n #\))))
	  
	  ;(format *stderr* "~A~%" (substring str 0 j))
	  (substring str 0 j))))

    (define (type-eqv? v1 v2 v3 v4)
      (let ((v1-type (type-of v1)))
	(and (or (eq? v1-type (type-of v2)) (and (number? v1) (number? v2) (= v1 v2)))
	     (or (eq? v1-type (type-of v3)) (and (number? v1) (number? v3) (= v1 v3)))
	     (or (eq? v1-type (type-of v4)) (and (number? v1) (number? v4) (= v1 v4))))))

    (define (same-type? val1 val2 val3 val4 str str1 str2 str3 str4)
      (cond ((not (type-eqv? val1 val2 val3 val4))
	     (unless (or (memq error-type '(out-of-range wrong-type-arg baffled!)) ; _rd3_ vs _rd4_ for example where one uses dynamic-wind which has built-in baffles
			 (and (number? val1)
			      (or (nan? val1)
				  (infinite? val1)
				  (and (equivalent? val1 val2) 
				       (equivalent? val1 val3) 
				       (equivalent? val1 val4))))
			 (equal? val1 "0")
			 (string-position "set! _definee_" str)
			 (and (iterator? _definee_) 
			      (string-position "_definee_" str)))
	       (let ((errstr (and (or (eq? val1 'error)
				      (eq? val2 'error)
				      (eq? val3 'error)
				      (eq? val4 'error))
				  (format #f "    ~S: ~S~%" error-type 
					  (if (pair? error-info)
					      (tp (apply format #f (car error-info) (cdr error-info)))
					      error-info)))))
		 (unless (and errstr 
			      (or (not (string? errstr))
				  (string-position "unbound" errstr)
				  (string-position "circular" errstr)))
		   (when (string-position "_definee_" str) (format *stderr* "_definee_: ~W~%" old-definee))
		   (when (string-position "bigrat" str) (format *stderr* "bigrat: ~W" bigrat))
		   (when (string-position "-inf.0" str) (format *stderr* "-inf.0: ~W" -inf.0))
		   (format *stderr* "~%~%~S~%~S~%~S~%~S~%    ~A~%    ~A~%    ~A~%    ~A~%" 
			   str1 str2 str3 str4 
			   (tp val1) (tp val2) (tp val3) (tp val4))
		   (if (string? errstr) (display errstr *stderr*))))))

	    ((or (catch #t (lambda () (openlet? val1)) (lambda args #t)) ; (openlet? (openlet (inlet 'openlet? ()))) -> error: attempt to apply nil to (inlet 'openlet? ())
		 (string-position "(set!" str1)
		 (string-position "gensym" str1)))

	    ((symbol? val1)
	     (if (gensym? val1)
		 (unless (and (gensym? val2)
			      (gensym? val3)
			      (gensym? val4))
		   (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~A ~A ~A ~A~%" 
			   str str1 str2 str3 str4 
			   (tp val1) (tp val2) (tp val3) (tp val4)))
		 (unless (and (eq? val1 val2)
			      (eq? val1 val3)
			      (eq? val1 val4))
		   (when (string-position "_definee_" str) (format *stderr* "_definee_: ~W~%" old-definee))
		   (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~A ~A ~A ~A~%" 
			   str str1 str2 str3 str4 
			   (tp val1) (tp val2) (tp val3) (tp val4))
		   (if (or (eq? val1 'error)
			   (eq? val2 'error)
			   (eq? val3 'error)
			   (eq? val4 'error))
		       (catch #t
			 (lambda ()
			   (format *stderr* "    ~S: ~S~%" error-type 
				   (if (pair? error-info)
				       (tp (apply format #f (car error-info) (cdr error-info)))
				       error-info)))
			 (lambda args
			   (format *stderr* "error in format in t725: ~S~%" (list str val1 val2 val3 val4))))))))
	    
	    ((sequence? val1) ; there are too many unreadable/unequivalent-but the same cases to check these by element (goto, continuation, ...)
	     (let ((len1 (length val1)))
	       (unless (or (provided? 'gmp)
			   (let? val1)
			   (hash-table? val1)
			   (and (eqv? len1 (length val2))
				(eqv? len1 (length val3))
				(eqv? len1 (length val4)))
			   (and (string? val1)
				(string->number val1))
			   (string-position "set! _definee_" str) 
			   (and (iterator? _definee_) 
				(string-position "_definee_" str)))
		 (when (string-position "_definee_" str) (format *stderr* "_definee_: ~W~%" old-definee))
		 (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%    ~A~%    ~A~%    ~A~%    ~A~%~%" 
			 str str1 str2 str3 str4 
			 (tp val1) (tp val2) (tp val3) (tp val4)))))
	    
	    ((number? val1)
	     (if (or (and (nan? val1)
			  (not (and (nan? val2) (nan? val3) (nan? val4))))
		     (and (infinite? val1)
			  (not (and (infinite? val2) (infinite? val3) (infinite? val4))))
		     (and (finite? val1)
			  (not (and (finite? val2) (finite? val3) (finite? val4))))
		     (and (not (= val1 val2))
			  (not (zero? val1))
			  (finite? val1) (real? val1) (real? val2) (real? val3) (real? val4) 
			  (or (and (negative? val1) (or (positive? val2) (positive? val3) (positive? val4)))
			      (and (positive? val1) (or (negative? val2) (negative? val3) (negative? val4))))))
		 (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%    ~A~%    ~A~%    ~A~%    ~A~%~%" 
			 str str1 str2 str3 str4 
			 (tp val1) (tp val2) (tp val3) (tp val4))))
	    
	    ((or (boolean? val1)
		 (syntax? val1)
		 (unspecified? val1)
		 (char? val1)
		 (memq val1 '(#<eof> ())))
	     (unless (or (and (eq? val1 val2)
			      (eq? val1 val3)
			      (eq? val1 val4))
			 (string-position "set! _definee_" str) 
			 (and (iterator? _definee_) 
			      (string-position "_definee_" str)))
	       (when (string-position "_definee_" str) (format *stderr* "_definee_: ~W~%" old-definee))
	       (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~A ~A ~A ~A~%" 
		       str str1 str2 str3 str4 
		       (tp val1) (tp val2) (tp val3) (tp val4))))
	    
	    ((or (undefined? val1)
		 (c-object? val1))
	     (unless (and (equal? val1 val2)
			  (equal? val1 val3)
			  (equal? val1 val4))
	       (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~A ~A ~A ~A~%" 
		       str str1 str2 str3 str4 
		       (tp val1) (tp val2) (tp val3) (tp val4))))
	  ))

    (define (eval-it str) 
      (set! (current-output-port) #f)
      ;(format *stderr* "~S~%" str)
      (set! estr str)
      (set! old-definee _definee_)
      (when with-methods (get-output-string imfo #t))
      (catch #t 
	(lambda ()
	  (car (list (eval-string str))))
	(lambda (type info)
	  (set! error-type type)
	  (set! error-info info)
	  (when (and last-error-type
		     (not (eq? error-type last-error-type)))
	    (format *stderr* "~S ~S~%" last-error-type error-type)
	    (set! last-error-type error-type))
	  (if (and (eq? type 'stack-too-big)
		   (not (string-position "lambda" str)))
	      (format *stderr* "stack overflow from ~S~%" str))
	  (when (eq? type 'heap-too-big)
	    (format *stderr* "heap overflow from ~S~%" str))
	  (unless (or (not (eq? type 'read-error))
		      (string-position "junk" (car info))
		      (string-position "clobbered" (car info))
		      (string-position "unexpected" (car info))
		      (string-position "eval-string" str))
	    ;; "unexpected" close paren from: (eval-string (reverse (object->string ()))) -> (eval-string ")(")
	    (format *stderr* "read-error from ~S: ~S~%" str (apply format #f info))
	    (if (string-position "a1" str) (format *stderr* "a1: ~W~%" a1))
	    (if (string-position "a2" str) (format *stderr* "a2: ~W~%" a2))
	    (if (string-position "a3" str) (format *stderr* "a3: ~W~%" a3))
	    (if (string-position "a4" str) (format *stderr* "a4: ~W~%" a4)))
          'error)))

    (define (try-both str)
      ;(if (string-position "*s7*" str) (format *stderr* "~A~%" str))
      ;(if (> (random 1000) 990) (gc))
      (set! ostr str)
      (set! (current-output-port) #f)
      ;(procedure-source pp-checked)

      (catch #t 
	(lambda () 
	  (s7-optimize (list (catch #t 
			       (lambda ()
				 (with-input-from-string str read))
			       (lambda args ())))))
	(lambda arg 'error))

      (set! last-error-type #f)
      (let* ((outer (codes (random codes-len)))
	     (str1 (string-append "(let ((x #f) (i 0)) " (car outer) str ")))"))
	     (str2 (string-append "(let () (define (func) " str1 ") (func))"))
	     (str3 (string-append "(let ((x #f) (i 0)) " (cadr outer) str ")))"))
	     (str4 (string-append "(let () (define (func) " str3 ") (func))")))
	(let ((val1 (eval-it str1))
	      (val2 (eval-it str2))
	      (val3 (eval-it str3))
	      (val4 (eval-it str4)))
	  ;(gc) (gc)
	  (set! (*s7* 'print-length) 4096)
	  (same-type? val1 val2 val3 val4 str str1 str2 str3 str4)
	  (unless (hash-table? a1)
	    (format *stderr* "a1: ~S, str: ~S~%" a1 str))
	  ))
#|
      (let* ((outer (codes (random codes-len)))
	     (str1 (string-append "(let () (define* (_f_ (x #f) (i 0)) " (car outer) str "))) (_f_))"))
	     (str2 (string-append "(apply (lambda (x i) " (car outer) str "))) (list #f 0))"))
	     (str3 (string-append "(let () (define (_f_ x i) " (cadr outer) str "))) (_f_ #f 0))"))
	     (str4 (string-append "(let () (apply (lambda* ((x #f) (i 0)) " (cadr outer) str "))) ()))")))
	(let ((val1 (eval-it str1))
	      (val2 (eval-it str2))
	      (val3 (eval-it str3))
	      (val4 (eval-it str4)))
	  ;(gc) (gc)
	  (same-type? val1 val2 val3 val4 str str1 str2 str3 str4)
	  ))
|#
      )

    (define dots (vector "." "-" "+" "-"))
    (define (test-it)
      (do ((m 0 (+ m 1))
	   (n 0))
	  ((= m 100000000)
	   (format *stderr* "reached end of loop??~%"))
	
	(when (zero? (modulo m 100000))
	  (set! m 0)
	  (set! n (+ n 1))
	  (if (= n 4) (set! n 0))
	  (format *stderr* "~A" (vector-ref dots n)))
	
	(try-both (make-expr (+ 1 (random both-ran)))) ; min 1 here not 0, was 6
	))

    (catch #t
      test-it
      (lambda (type info)
	(format *stderr* "~%~%outer: ~S ~S from ~S~%" type 
		(catch #t
		  (lambda ()
		    (apply format #f info))
		  (lambda (t i)
		    (list 'outer-format-error t i info)))
		estr)
	(format *stderr* "owlet: ~S~%" (owlet))))))

;;; (let () ((lambda () str))) (let () (define _f_ (lambda () str)) (_f_))
;;; (let _f_ ((x #f) (i 0)) str)
;;; (do ((x #f) (i 0) (_k_ str)) ((= i 0) _k_))

