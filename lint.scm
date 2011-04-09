;;; lint for s7 scheme
;;;
;;; non-standard stuff we need: 
;;;      procedure-arity, symbol->value, pair-line-number, keyword handlers, defined? 
;;; easily translated: 
;;;      call-with-exit, implicit indexing, procedure-with-setter, provide, hash-table


(provide 'lint.scm)

(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #f)
(define *load-file-first* #f)


;;; --------------------------------------------------------------------------------
;;; for snd-test.scm
(set! *#readers* 
      (cons (cons #\_ (lambda (str)
			(if (string=? str "__line__")
			    (port-line-number)
			    #f)))
            *#readers*))
;;; --------------------------------------------------------------------------------


;;; --------------------------------------------------------------------------------
;;; function argument data

(define lint
  (let ((argument-data
	 (hash-table (cons 'gensym string?)
		     (cons 'symbol->string symbol?)
		     (cons 'string->symbol string?)
		     (cons 'symbol->value symbol?)
		     (cons 'symbol->keyword symbol?)
		     (cons 'keyword->symbol keyword?)
		     (cons 'open-input-file (list string? string?))
		     (cons 'open-output-file (list string? string?))
		     (cons 'open-input-string string?)
		     (cons 'call-with-output-file (list string? procedure?))
		     (cons 'call-with-input-file (list string? procedure?))
		     (cons 'call-with-input-string (list string? procedure?))
		     (cons 'call-with-output-string procedure?)
		     (cons 'with-input-from-file (list string? procedure?))
		     (cons 'with-input-from-string (list string? procedure?))
		     (cons 'with-output-to-file (list string? procedure?))
		     (cons 'with-output-to-string procedure?)
		     (cons 'provide symbol?)
		     
		     (cons 'even? integer?)
		     (cons 'odd? integer?)
		     (cons 'integer-length integer?)
		     (cons 'logior (list integer? integer?))
		     (cons 'logxor (list integer? integer?))
		     (cons 'logand (list integer? integer?))
		     (cons 'lognot (list integer? integer?))
		     (cons 'ash (list integer? integer?))
		     (cons 'integer-decode-float integer?)
		     (cons 'integer->char integer?)
		     
		     (cons 'numerator rational?)
		     (cons 'denominator rational?)
		     
		     (cons 'make-polar number?)
		     (cons 'make-rectangular number?)
		     (cons 'magnitude number?)
		     (cons 'angle number?)
		     (cons 'real-part number?)
		     (cons 'imag-part number?)
		     (cons 'rationalize (list number? number?))
		     (cons 'abs number?)
		     (cons 'exp number?)
		     (cons 'log (list number? number?))
		     (cons 'sin number?)
		     (cons 'cos number?)
		     (cons 'tan number?)
		     (cons 'asin number?)
		     (cons 'acos number?)
		     (cons 'atan (list number? number?))
		     (cons 'sinh number?)
		     (cons 'cosh number?)
		     (cons 'tanh number?)
		     (cons 'asinh number?)
		     (cons 'acosh number?)
		     (cons 'atanh number?)
		     (cons 'sqrt number?)
		     (cons 'expt (list number? number?))
		     (cons 'floor number?)
		     (cons 'ceiling number?)
		     (cons 'truncate number?)
		     (cons 'round number?)
		     (cons 'lcm (list real? real?))
		     (cons 'gcd (list real? real?))
		     (cons '+ number?)
		     (cons '- number?)
		     (cons '* number?)
		     (cons '/ number?)
		     (cons 'max real?)
		     (cons 'min real?)
		     (cons 'quotient (list real? real?))
		     (cons 'remainder (list real? real?))
		     (cons 'modulo (list real? real?))
		     (cons '= number?)
		     (cons '< real?)
		     (cons '> real?)
		     (cons '<= real?)
		     (cons '>= real?)
		     (cons 'zero? number?)
		     (cons 'positive? number?)
		     (cons 'negative? number?)
		     (cons 'infinite? number?)
		     (cons 'nan? number?)
		     (cons 'inexact->exact real?)
		     (cons 'exact->inexact real?)
		     (cons 'random number?)
		     (cons 'exact? number?)
		     (cons 'inexact? number?)
		     
		     (cons 'number->string (list number? integer?))
		     (cons 'string->number (list string? integer?))
		     
		     (cons char-upcase char?)
		     (cons char-downcase char?)
		     (cons char->integer char?)
		     (cons char-upper-case? char?)
		     (cons char-lower-case? char?)
		     (cons char-alphabetic? char?)
		     (cons char-numeric? char?)
		     (cons char-whitespace? char?)
		     (cons char=? char?)
		     (cons char<? char?)
		     (cons char>? char?)
		     (cons char<=? char?)
		     (cons char>=? char?)
		     (cons char-ci=? char?)
		     (cons char-ci<? char?)
		     (cons char-ci>? char?)
		     (cons char-ci<=? char?)
		     (cons char-ci>=? char?)
		     
		     (cons 'make-string (list integer? char?))
		     (cons 'string char?)
		     
		     (cons 'string-length string?)
		     (cons 'string-ref (list string? integer?))
		     (cons 'string-set! (list string? integer? char?))
		     (cons 'string=? string?)
		     (cons 'string<? string?)
		     (cons 'string>? string?)
		     (cons 'string<=? string?)
		     (cons 'string>=? string?)
		     (cons 'string-ci=? string?)
		     (cons 'string-ci<? string?)
		     (cons 'string-ci>? string?)
		     (cons 'string-ci<=? string?)
		     (cons 'string-ci>=? string?)
		     (cons 'string-append string?)
		     (cons 'string-fill! (list string? char?))
		     (cons 'string-copy string?)
		     (cons 'substring (list string? integer? integer?))
		     (cons 'string->list string?)
		     
		     (cons 'list->string list?)
		     (cons 'list->vector list?)
		     (cons 'car list?)
		     (cons 'cdr list?)
		     (cons 'caar list?)
		     (cons 'cadr list?)
		     (cons 'cdar list?)
		     (cons 'cddr list?)
		     (cons 'caaar list?)
		     (cons 'caadr list?)
		     (cons 'cadar list?)
		     (cons 'cdaar list?)
		     (cons 'caddr list?)
		     (cons 'cdddr list?)
		     (cons 'cdadr list?)
		     (cons 'cddar list?)
		     (cons 'caaaar list?)
		     (cons 'caaadr list?)
		     (cons 'caadar list?)
		     (cons 'cadaar list?)
		     (cons 'caaddr list?)
		     (cons 'cadddr list?)
		     (cons 'cadadr list?)
		     (cons 'caddar list?)
		     (cons 'cdaaar list?)
		     (cons 'cdaadr list?)
		     (cons 'cdadar list?)
		     (cons 'cddaar list?)
		     (cons 'cdaddr list?)
		     (cons 'cddddr list?)
		     (cons 'cddadr list?)
		     (cons 'cdddar list?)
		     (cons 'list-ref (list list? integer?))
		     (cons 'list-set! (list list? integer?))
		     (cons 'list-tail list?)
		     
		     (cons 'vector->list vector?)
		     (cons 'vector-fill! (list vector?))
		     (cons 'vector-length vector?)
		     (cons 'vector-ref (list vector? integer?))
		     (cons 'vector-set! (list vector? integer?))
		     (cons 'vector-dimensions vector?)
		     
		     (cons 'make-hash-table integer?)
		     (cons 'hash-table-ref (list hash-table? integer?))
		     (cons 'hash-table-set! (list hash-table? integer?))
		     (cons 'hash-table-size hash-table?)
		     (cons 'make-hash-table-iterator hash-table?)

		     (cons 'hook-arity hook?)
		     (cons 'hook-functions hook?)
		     (cons 'hook-documentation hook?)
		     (cons 'make-hook (list list? string?))
		     (cons 'hook procedure?)
		     
		     (cons 'call/cc procedure?)
		     (cons 'call-with-current-continuation procedure?)
		     (cons 'call-with-exit procedure?)
		     
		     (cons 'load string?)
		     (cons 'eval-string string?)
		     (cons 'dynamic-wind (list procedure? procedure? procedure?))))
	
	(no-side-effect-functions (list 'not '= '+ 'cdr 'real? 'rational? 'number? '> '- 'integer? 'catch 
					'length 'eq? 'car '< 'assq 'complex? 'vector-ref 'random 'abs '* 
					'null? 'imag-part '/ 'equal? 'magnitude 'real-part 'pair? 'max 'nan? 
					'string->number 'list 'negative? 'cons 'list-ref 'eqv? 'positive? '>= 
					'expt 'number->string 'zero? 'floor 'denominator 'integer->char 
					'string? 'min '<= 'char->integer 'cos 'rationalize 'cadr 'sin 'char=? 
					'map 'defined? 'memq 'string-ref 'log 'for-each 'round 'ceiling 
					'truncate 'string=? 'atan 'eof-object? 'numerator 'make-rectangular 
					'char? 'cosh 'member 'vector 'even? 'string-append 'char-upcase 'sqrt
					'make-string 'char-alphabetic? 'odd? 'call-with-exit 'tanh 'copy 'sinh 
					'make-vector 'string 'char-ci=? 'caddr 'tan 'reverse 'cddr 'append 
					'vector? 'list? 'exp 'acos 'asin 'symbol? 'char-numeric? 'string-ci=? 
					'char-downcase 'acosh 'vector-length 'asinh 'make-list 'atanh 'modulo 
					'make-polar 'gcd 'angle 'gensym 'remainder 'quotient 'lcm 'char-whitespace? 
					'assoc 'procedure? 'char<? 'inexact->exact 'vector->list 'boolean? 'caar
					'ash 'list-tail 'symbol->string 'string->symbol 'exact->inexact 
					'object->string 'char>? 'symbol->value 'cadar 'integer-decode-float 
					'string-copy 'cdddr 'logand 'cadddr 'substring 'string->list 
					'char-upper-case? 'cddddr 'string<? 'dynamic-wind 'lognot 'cdar 
					'char-ci>=? 'string>=? 'make-procedure-with-setter 'string-ci<? 'char<=?
					'logior 'char-ci<=? 'assv 'string>? 'char-ci>? 'char-lower-case? 'string-ci>=? 
					'string-ci>? 'string<=? 'caadr 'char-ci<? 'string-ci<=? 'cadadr 'cdadr 
					'provided? 'caaaar 'caaddr 'caddar 'cdaaar 'cdaadr 'cdaddr 'cddar
					'hash-table-ref 'list->vector 'caaadr 'caaar 'caadar 'cadaar 'cdadar 'cdddar
					'cdaar 'cddaar 'cddadr 'procedure-arity 'keyword? 'memv 'char-ready?
					'symbol->keyword 'logxor 'exact? 'integer-length 'port-filename 'char>=? 
					'string-length 'list->string 'inexact? 'procedure-source 'symbol 'make-hash-table 
					'current-error-port 'macro? 'quasiquote 'constant? 'infinite? 'vector-dimensions
					'make-type 'make-keyword 'keyword->symbol 'procedure-documentation 'procedure-environment 
					'port-line-number 'continuation? 'hash-table? 'port-closed? 'current-environment 
					'output-port? 'input-port?  'hash-table 'hash-table-size 'make-random-state
					'current-output-port 'current-input-port 'initial-environment 'global-environment 
					's7-version 'procedure-with-setter? 'hook? 'hook-arity 'hook-functions 
					'hook-documentation 'make-hook 'hook-apply 'hook))

	(function-types (hash-table (cons 'procedure-with-setter? #f)
				    (cons 'gensym 'symbol)
				    (cons 'symbol? #f)
				    (cons 'symbol->string "")
				    (cons 'string->symbol 'symbol)
				    (cons 'symbol 'symbol)
				    (cons 'environment? #f)
				    (cons 'provided? #f)
				    (cons 'defined? #f)
				    (cons 'constant? #f)
				    (cons 'macro? #f)
				    (cons 'keyword? #f)
				    (cons 'keyword->symbol 'symbol)
				    (cons 'input-port? #f)
				    (cons 'output-port? #f)
				    (cons 'port-closed? #f)
				    (cons 'char-ready? #f)
				    (cons 'eof-object? #f)
				    (cons 'make-polar 1)
				    (cons 'make-rectangular 1)
				    (cons 'magnitude 1)
				    (cons 'angle 1)
				    (cons 'real-part 1)
				    (cons 'imag-part 1)
				    (cons 'numerator 1)
				    (cons 'denominator 1)
				    (cons 'rationalize 1)
				    (cons 'abs 1)
				    (cons 'exp 1)
				    (cons 'log 1)
				    (cons 'sin 1)
				    (cons 'cos 1)
				    (cons 'tan 1)
				    (cons 'asin 1)
				    (cons 'acos 1)
				    (cons 'atan 1)
				    (cons 'sinh 1)
				    (cons 'cosh 1)
				    (cons 'tanh 1)
				    (cons 'asinh 1)
				    (cons 'acosh 1)
				    (cons 'atanh 1)
				    (cons 'sqrt 1)
				    (cons 'expt 1)
				    (cons 'floor 1)
				    (cons 'ceiling 1)
				    (cons 'truncate 1)
				    (cons 'round 1)
				    (cons 'lcm 1)
				    (cons 'gcd 1)
				    (cons '+ 1)
				    (cons '- 1)
				    (cons '* 1)
				    (cons '/ 1)
				    (cons 'max 1)
				    (cons 'min 1)
				    (cons 'quotient 1)
				    (cons 'remainder 1)
				    (cons 'modulo 1)
				    (cons '= #f)
				    (cons '< #f)
				    (cons '> #f)
				    (cons '<= #f)
				    (cons '>= #f)
				    (cons 'even? #f)
				    (cons 'odd? #f)
				    (cons 'zero? #f)
				    (cons 'positive? #f)
				    (cons 'negative? #f)
				    (cons 'infinite? #f)
				    (cons 'nan? #f)
				    (cons 'inexact->exact 1)
				    (cons 'exact->inexact 1)
				    (cons 'integer-length 1)
				    (cons 'logior 1)
				    (cons 'logxor 1)
				    (cons 'logand 1)
				    (cons 'lognot 1)
				    (cons 'ash 1)
				    (cons 'random-state->list '())
				    (cons 'integer-decode-float 1)
				    (cons 'exact? #f)
				    (cons 'inexact? #f)
				    (cons 'number? #f)
				    (cons 'integer? #f)
				    (cons 'real? #f)
				    (cons 'complex? #f)
				    (cons 'rational? #f)
				    (cons 'number->string "")
				    (cons 'string->number 1)
				    (cons 'char-upcase #\a)
				    (cons 'char-downcase #\a)
				    (cons 'char->integer 1)
				    (cons 'integer->char #\a)
				    (cons 'char-upper-case? #f)
				    (cons 'char-lower-case? #f)
				    (cons 'char-alphabetic? #f)
				    (cons 'char-numeric? #f)
				    (cons 'char-whitespace? #f)
				    (cons 'char? #f)
				    (cons 'char=? #f)
				    (cons 'char<? #f)
				    (cons 'char>? #f)
				    (cons 'char<=? #f)
				    (cons 'char>=? #f)
				    (cons 'char-ci=? #f)
				    (cons 'char-ci<? #f)
				    (cons 'char-ci>? #f)
				    (cons 'char-ci<=? #f)
				    (cons 'char-ci>=? #f)
				    (cons 'string? #f)
				    (cons 'make-string "")
				    (cons 'string-length 1)
				    (cons 'string-ref #\a)
				    (cons 'string=? #f)
				    (cons 'string<? #f)
				    (cons 'string>? #f)
				    (cons 'string<=? #f)
				    (cons 'string>=? #f)
				    (cons 'string-ci=? #f)
				    (cons 'string-ci<? #f)
				    (cons 'string-ci>? #f)
				    (cons 'string-ci<=? #f)
				    (cons 'string-ci>=? #f)
				    (cons 'string-append "")
				    (cons 'string-copy "")
				    (cons 'string "")
				    (cons 'list->string "")
				    (cons 'string->list '())
				    (cons 'object->string "")
				    (cons 'null? #f)
				    (cons 'list? #f)
				    (cons 'pair? #f)
				    (cons 'cons '())
				    (cons 'list '())
				    (cons 'make-list '())
				    (cons 'length 1)
				    (cons 'vector? #f)
				    (cons 'vector->list '())
				    (cons 'list->vector #())
				    (cons 'vector #())
				    (cons 'vector-length 1)
				    (cons 'make-vector #())
				    (cons 'continuation? #f)
				    (cons 'map '())
				    (cons 'procedure? #f)
				    (cons 'not #f)
				    (cons 'boolean? #f)
				    (cons 'eq? #f)
				    (cons 'eqv? #f)
				    (cons 'equal? #f)))
	)
    
    (define (env-member arg env)
      (member arg env (lambda (a b) (eq? a (car b)))))

    (define (side-effect? form env)
      (if (pair? form)
	  (or (not (member (car form) no-side-effect-functions)) ; if func is not in that list, make no assumptions about it
	      (call-with-exit
	       (lambda (return)
		 (for-each
		  (lambda (f)
		    (if (side-effect? f env)
			(return #t)))
		  (cdr form))
		 #f)))
	  (and (symbol? form)
	       (and (not (member form no-side-effect-functions))
		    (not (env-member form env))))))
    
    (define (check-args name line-number head form checkers)
      (let ((arg-number 1))
	(call-with-exit
	 (lambda (done)
	   (for-each 
	    (lambda (arg)
	      (let ((checker (if (list? checkers) (car checkers) checkers)))  
		(if (pair? arg)
		    (let ((op (function-types (car arg))))
		      (if (and op
			       (not (checker op)))
			  (format #t "  ~A (line ~D): ~A's argument ~D should be a~A ~A: ~S:~%        ~S~%" 
				  name line-number head arg-number 
				  (if (eq? checker integer?) "n" "")
				  checker arg form)))
		    (if (and (not (symbol? arg))
			     (not (checker arg)))
			(format #t "  ~A (line ~D): ~A's argument ~D should be a~A ~A: ~S:~%        ~S~%" 
				name line-number head arg-number 
				(if (eq? checker integer?) "n" "")
				checker arg form)))
		(if (list? checkers)
		    (if (null? (cdr checkers))
			(done)
			(set! checkers (cdr checkers))))
		(set! arg-number (+ arg-number 1))))
	    (cdr form))))))
    
  ;;; --------------------------------------------------------------------------------
    
    (define (ref-var name env)
      (call-with-exit         
       (lambda (ok)
	 (for-each
	  (lambda (var)
	    (if (eq? name (car var))
		(begin 
		  (set! (var 1) #t)               ; (list-set! var 1 #t)
		  (ok))))
	  env)
;	 (if (not (defined? name))
;	     (format #t "~A is undefined?~%" name))
	 ))
      env)
    
    (define (proper-list lst)
      (if (pair? lst)
	  (cons (car lst) 
		(if (pair? (cdr lst)) 
		    (proper-list (cdr lst)) 
		    (if (null? (cdr lst)) 
			'() 
			(list (cdr lst)))))
	  lst))
    
    (define (keywords lst)
      (let ((keys 0))
	(for-each 
	 (lambda (arg)
	   (if (keyword? arg)
	       (set! keys (+ keys 1))))
	 lst)
	keys))
    
    (define (report-usage name line-number type head vars)
      (if (and (not (eq? head 'begin)) ; begin can redefine = set a variable
	       (not (null? vars)))
	  (do ((cur vars (cdr cur))
	       (rst (cdr vars) (cdr rst)))
	      ((null? rst))
	    (let ((repeat (env-member (caar cur) rst)))
	      (if repeat
		  (format #t "  ~A (line ~D): ~A ~A ~A is declared twice~%" name line-number head type (caar cur))))))
      (let ((set '())
	    (unused '()))
	(for-each 
	 (lambda (arg)
	   (if (member (car arg) '(quote if begin let let* letrec cond case or and do set! with-environment lambda lambda* define
					 define* defmacro defmacro* define-macro define-macro* define-bacro define-bacro* define-constant))
	       (format #t "  ~A (line ~D): ~A ~A named ~A is asking for trouble~%" name line-number head type (car arg))
	       (if (not (symbol? (car arg)))
		   (format #t "  ~A (line ~D): bad ~A ~A name: ~S~%" name line-number head type (car arg))))
	   
	   (if (not (cadr arg))
	       (if (caddr arg)
		   (set! set (cons (car arg) set))
		   (set! unused (cons (car arg) unused)))))
	 vars)
	(if (not (null? set))
	    (format #t "  ~A (line ~D): ~A ~A~A ~{~A~^, ~} set, but not used~%" 
		    name line-number head type (if (> (length set) 1) "s" "") (reverse set)))
	(if (not (null? unused))
	    (format #t "  ~A (line ~D): ~A ~A~A ~{~A~^, ~} not used~%" 
		    name line-number head type (if (> (length unused) 1) "s" "") (reverse unused)))))
    
    (define (walk-body name line-number head body env)
      (if (and (pair? body)
	       (string? (car body)) ; ;possible doc string
	       (not (member head '(let let* letrec letrec* do))))
	  (set! body (cdr body)))
      (let ((ctr 0)
	    (len (length body)))
	(for-each
	 (lambda (f)
	   (if (and (< ctr (- len 1))
		    (not (side-effect? f env)))
	       (format #t "  ~A (line ~D): ~S in ~A body has no effect~%" 
		       name line-number f head))
	   (set! env (walk name f env))
	   (set! ctr (+ ctr 1)))
	 body)))
    
    (define (walk-function head name args val line-number env)
      (if (null? args)
	  (begin
	    (walk-body name line-number head val env)
	    (append (list (list name #f #f (list head args))) env))
	  (if (or (symbol? args) 
		  (pair? args))
	      (let ((arg-data (if (symbol? args)
				  (list (list args #f #f))
				  (map
				   (lambda (arg)
				     (if (symbol? arg)
					 (if (member arg '(:optional :key :rest :allow-other-keys))
					     (values)                  ; map omits this entry 
					     (list arg #f #f))
					 (if (or (not (pair? arg))
						 (not (= (length arg) 2))
						 (not (member head '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
					     (format #t "  ~A (line ~D): strange parameter ~A~%" name line-number arg)
					     (list (car arg) #f #f))))
				   (proper-list args)))))
		(walk-body name line-number head val (append arg-data env))
		(if *report-unused-parameters* (report-usage name line-number 'parameter head arg-data))
		(append (list (list name #f #f (list head args))) env))
	      (begin
		(format #t "  ~A (line ~D): strange ~A parameter list ~A~%" name line-number head args)
		env))))
    
    (define (walk name form env)
      (if (symbol? form)
	  (ref-var form env)
	  
	  (if (pair? form)
	      (let ((head (car form))
		    (line-number (pair-line-number form)))
		(case head

		  ((defmacro defmacro*)
		   (if (or (< (length form) 4)
			   (not (symbol? (cadr form))))
		       (format #t "  ~A (line ~D): ~A declaration is messed up: ~S~%"
			       name line-number head form)
		       (let ((sym (cadr form))
			     (args (caddr form))
			     (body (cdddr form)))
			 (walk-function head sym args body line-number env))))
		  
		  ((define define* 
		     define-constant 
		     define-expansion define-macro define-macro* define-bacro define-bacro*
		     definstrument)
		   
		   (let ((sym (cadr form))
			 (val (cddr form)))
		     (if (symbol? sym)
			 (begin
			   (if (member head '(define define-constant))
			       (let ((len (length form)))
				 (if (not (= len 3))
				     (format #t "  ~A (line ~D): ~S has ~A value~A?~%"
					     name line-number form (if (< len 3) "no" "too many") (if (< len 3) "" "s"))))
			       (format #t "  ~A (line ~D): ~S is messed up~%" name line-number form))
			   (if (not (null? (cddr form))) 
			       (walk sym (caddr form) env))
			   (append (list (list sym #f #f)) env))
			 (if (pair? sym)
			     (walk-function head (car sym) (cdr sym) val line-number env)
			     (begin
			       (format #t "  ~A (line ~D): strange form: ~S~%" head line-number form)
			       env)))))
		  
		  ((lambda lambda*)
		   (walk-function head name (cadr form) (cddr form) line-number env))
		  ;; the lambda case includes stuff like call/cc
		  
		  ((set!)
		   (if (not (= (length form) 3))
		       (format #t "  ~A (line ~D): set! has too ~A arguments: ~S~%" 
			       name line-number (if (> (length form) 3) "many" "few") form))
		   (let* ((settee (cadr form))
			  (setval (caddr form)))
		     (if (pair? settee)
			 (begin
			   (walk name settee env) ; this counts as a reference since it's by reference so to speak
			   (set! settee (do ((sym (car settee) (car sym)))
					    ((not (pair? sym)) sym)))))
		     (if (symbol? settee)
			 (call-with-exit
			  (lambda (ok)
			    (for-each
			     (lambda (var)
			       (if (eq? settee (car var))
				   (begin 
				     (set! (var 2) #t)  ; (list-set! var 2 #t)
				     (ok))))
			     env)))
			 (if (not (pair? settee))
			     (format #t "  ~A (line ~D): bad set? ~A~%" name line-number form)))
		     (walk name setval env)))
		  
		  ((quote) 
		   (if (not (= (length form) 2))
		       (format #t "  ~A (line ~D): quote has too ~A arguments: ~S~%" 
			       name line-number (if (> (length form) 2) "many" "few") form))
		   env)
		  
		  ((case)
		   ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		   (walk name (cadr form) env) ; the selector
		   (let ((all-keys '()))
		     (for-each
		      (lambda (clause)
			(let ((keys (car clause))
			      (exprs (cdr clause)))
			  (if (pair? keys)
			      (for-each
			       (lambda (key)
				 (if (or (vector? key)
					 (string? key)
					 (list? key)
					 (hash-table? key)
					 (hook? key))
				     (format #t "  ~A (line ~D): case key ~S in ~S is unlikely to work (case uses eqv?)~%" name line-number key clause))
				 (if (member key all-keys)
				     (format #t "  ~A (line ~D): repeated case key ~S in ~S~%" name line-number key clause)))
			       keys)
			      (if (not (eq? keys 'else))
				  (format #t "  ~A (line ~D): bad case key ~S in ~S~%" name line-number keys clause)))
			  (set! all-keys (append (if (pair? keys) keys (list keys)) all-keys))
			  (walk-body name line-number head exprs env)))
		      (cddr form))
		     env))
		  
		  ((do)
		   (let ((vars '()))
		     ;; walk the init forms before adding the step vars to env
		     (do ((bindings (cadr form) (cdr bindings)))
			 ((null? bindings))
		       (walk name (cadar bindings) env)
		       (set! vars (append (list (list (caar bindings) #f #f)) vars)))
		     ;; walk the step exprs
		     (do ((bindings (cadr form) (cdr bindings)))
			 ((null? bindings))
		       (if (not (null? (cddar bindings)))
			   (walk name (caddar bindings) (append vars env))))
		     (walk-body name line-number head (cddr form) (append vars env))
		     (report-usage name line-number 'variable head vars)
		     env))
		  
		  ((let)
		   (let* ((named-let (if (symbol? (cadr form)) (cadr form) #f)))
		     (let ((vars (if named-let (list (list named-let #f #f)) '())))
		       (do ((bindings (if named-let (caddr form) (cadr form)) (cdr bindings)))
			   ((null? bindings))
			 (walk name (cadar bindings) env)
			 (set! vars (append (list (list (caar bindings) #f #f)) vars)))
		       (walk-body name line-number head (if named-let (cdddr form) (cddr form)) (append vars env))
		       (report-usage name line-number 'variable head vars)
		       env)))
		  
		  ((let*)
		   (let ((vars '()))
		     (do ((bindings (cadr form) (cdr bindings)))
			 ((null? bindings))
		       ;; report-usage checks for non-symbols here
		       (walk name (cadar bindings) (append vars env))
		       (set! vars (append (list (list (caar bindings) #f #f)) vars)))
		     (walk-body name line-number head (cddr form) (append vars env))
		     (report-usage name line-number 'variable head vars)
		     env))
		  
		  ((letrec letrec*)
		   (let ((vars '()))
		     (do ((bindings (cadr form) (cdr bindings)))
			 ((null? bindings))
		       (set! vars (append (list (list (caar bindings) #f #f)) vars))
		       (walk name (cadar bindings) (append vars env)))
		     (walk-body name line-number head (cddr form) (append vars env))
		     (report-usage name line-number 'variable head vars)
		     env))

		  ((begin)
		   (let* ((ctr 0)
			  (body (cdr form))
			  (len (length body))
			  (vars env))
		     (for-each
		      (lambda (f)
			(if (and (< ctr (- len 1))
				 (not (side-effect? f env)))
			    (format #t "  ~A (line ~D): ~S in ~A body has no effect~%" 
				    name line-number f head))
			(set! vars (walk name f vars))
			(set! ctr (+ ctr 1)))
		      body)
		     (if (not (eq? vars env))
			 (let ((nvars '()))
			   (do ((v vars (cdr v)))
			       ((or (null? v)
				    (eq? v env)))
			     (set! nvars (cons (car v) nvars)))
			   (report-usage name line-number 'local-variable head nvars))) ; this is actually not right, but it's better than nothing
		     vars))
		  
		  ((format snd-display clm-print)
		   (let ((control-string (if (string? (cadr form)) (cadr form) (caddr form)))
			 (args (if (string? (cadr form)) (cddr form) (cdddr form))))
		     
		     (define (count-directives str)
		       
		       (define (t-time str i len)
			 (call-with-exit
			  (lambda (return)
			    (do ((k i (+ k 1)))
				((= k len) #f)
			      (if (not (char-numeric? (str k)))
				  (return (char-ci=? (str k) #\t)))))))
		       
		       (let ((curlys 0)
			     (len (length str))
			     (dirs 0)
			     (tilde-time #f))
			 (do ((i 0 (+ i 1)))
			     ((= i len))
			   (let ((c (str i)))
			     (if tilde-time
				 (begin
				   (if (and (= curlys 0)
					    (not (member c '(#\~ #\T #\t #\& #\% #\^ #\newline #\})))
					    (not (t-time str i len)))
				       (set! dirs (+ dirs 1)))
				   (set! tilde-time #f)
				   (if (char=? c #\{)
				       (set! curlys (+ curlys 1))
				       (if (char=? c #\})
					   (set! curlys (- curlys 1)))))
				 (if (char=? c #\~)
				     (set! tilde-time #t)))))
			 (if (not (= curlys 0))
			     (format #t "  ~A (line ~D): ~A has ~D unmatched ~A~A:~%        ~S~%"
				     name line-number head (abs curlys) (if (positive? curlys) "{" "}") (if (> curlys 1) "s" "") form))
			 dirs))
		     
		     (if (not (string? control-string))
			 (if (not (pair? args))
			     (format #t "  ~A (line ~D): ~S looks suspicious~%" name line-number form))
			 (let ((ndirs (count-directives control-string))
			       (nargs (if (or (null? args) (pair? args)) (length args) 0)))
			   (if (not (= ndirs nargs))
			       (format #t "  ~A (line ~D): ~A has ~A arguments:~%       ~S~%" 
				       name line-number head 
				       (if (> ndirs nargs) "too few" "too many")
				       form))))
		     (walk name (cdr form) env)))
		  
		  (else  ; if begin cond and or with-environment
		   ;; we can't expand macros so free variables can confuse the usage checks
		   
		   (let ((local-value (env-member head env)))
		     (if local-value
			 (let ((fdata (car local-value)))
			   (if (= (length fdata) 4)
			       (let ((type (car (fdata 3)))
				     (args (cadr (fdata 3))))
				 (let ((rst (or (not (pair? args))
						(negative? (length args))
						(member :rest args)))
				       (pargs (if (pair? args) (proper-list args) '())))
				   (let ((call-args (length (cdr form)))
					 (decl-args (max 0 (- (length pargs) (keywords pargs) (if rst 1 0)))))
				     (let ((req (if (eq? type 'define) decl-args 0))
					   (opt (if (eq? type 'define) 0 decl-args)))
				       (if (< call-args req)
					   (format #t "  ~A (line ~D): ~A needs ~D argument~A:~%        ~S~%" 
						   name line-number head req (if (> req 1) "s" "") form)
					   (if (and (not rst)

						    (> (- call-args (keywords (cdr form))) (+ req opt)))
					       (format #t "  ~A (line ~D): ~A has too many arguments:~%        ~S~%" 
						       name line-number head form)))))))))
			 
			 (if (and (symbol? head)
				  (defined? head)
				  (procedure? (symbol->value head)))
			     ;; check arg number
			     (let ((arity (procedure-arity (symbol->value head)))
				   (args (length (cdr form))))
			       (if (pair? arity)
				   (if (not (procedure-with-setter? (symbol->value head))) ; set! case is confusing here
				       (if (< args (car arity))
					   (format #t "  ~A (line ~D): ~A needs at least ~D argument~A:~%        ~S~%" 
						   name line-number head (car arity) (if (> (car arity) 1) "s" "") form)
					   (if (and (not (caddr arity))
						    (> (- args (keywords (cdr form))) (+ (car arity) (cadr arity))))
					       (format #t "  ~A (line ~D): ~A has too many arguments:~%        ~S~%" 
						       name line-number head form)))
				       (let ((req (max (arity 0) (arity 3)))
					     (min-req (min (arity 0) (arity 3)))
					     (opt (max (arity 1) (arity 4)))
					     (rst (or (arity 2) (arity 5))))
					 (if (< args min-req)
					     (format #t "  ~A (line ~D): ~A needs at least ~D argument~A:~%        ~S~%" 
						     name line-number head min-req (if (> min-req 1) "s" "") form)
					     (if (and (not rst)
						      (> (- args (keywords (cdr form))) (+ req opt)))
						 (format #t "  ~A (line ~D): ~A has too many arguments:~%        ~S~%" 
							 name line-number head form))))))
			       
			       (if (and (pair? (cdr form)) ; there are args
					(not (eq? name 'defgenerator)))
				   (begin
				     ;; if keywords, check that they are acceptable
				     ;;    this only applies to lambda*'s that have been previously loaded (lint doesn't create them)
				     (let ((source (procedure-source head)))
				       (if (and (pair? source)
						(eq? (car source) 'lambda*))
					   (let ((decls (cadr source)))
					     (if (not (member :allow-other-keys decls))
						 (for-each
						  (lambda (arg)
						    (if (and (keyword? arg)
							     (not (member arg '(:rest :key :optional :allow-other-keywords))))
							(if (not (member arg decls (lambda (a b) 
										     (if (pair? b) 
											 (eq? (keyword->symbol a) (car b))
											 (eq? (keyword->symbol a) b)))))
							    (format #t "  ~A (line ~D): ~A keyword argument ~A (in ~S) does not match any argument in ~S~%"
								    name line-number head arg form decls))))
						  (cdr form))))))
				     
				     (case head
				       ((eq?) (if (or (number? (cadr form))
						      (char? (cadr form))
						      (and (not (null? (cddr form)))
							   (or (number? (caddr form))
							       (char? (caddr form)))))
						  (format #t "  ~A (line ~D): eq? doesn't work reliably with args like ~S~%" 
							  name line-number form)))
				       
				       ((not) (if (and (not (symbol? (cadr form)))
						       (not (pair? (cadr form)))
						       (not (eq? (cadr form) #f)))
						  (format #t "  ~A (line ~D): ~S is always #f~%" name line-number form)))
				       
				       ((catch)
					(if (and (not (symbol? (cadr form)))
						 (not (boolean? (cadr form)))
						 (or (not (pair? (cadr form)))
						     (not (eq? (caadr form) 'quote))))
					    (format #t "  ~A (line ~D): catch tag ~S is unreliable~%" name line-number (cadr form))))
				       
				       (else
					;; now try to check arg types for egregious errors
					(let ((arg-data (argument-data head)))
					  (if arg-data
					      (check-args name line-number head form arg-data)))))))))))
		   
		   (if (eq? head 'if)
		       (if (> (length form) 4)
			   (format #t "  ~A (line ~D): if has too many clauses: ~S~%" name line-number form)
			   (if (< (length form) 3)
			       (format #t "  ~A (line ~D): if has too few clauses: ~S~%" name line-number form))))

		   (let ((vars env))
		     (for-each
		      (lambda (f)
			(set! vars (walk name f vars)))
		      form))
		   env)))
	      
	      ;; else form is a constant or something
	      env)))
    
    ;;; --------------------------------------------------------------------------------

    (lambda (file)
      (if *load-file-first* ; this can improve the error checks
	  (load file))
      (let ((fp (catch #t
		       (lambda ()
			 (open-input-file file))
		       (lambda args
			 (format #t "  can't open ~S: ~A~%" file (apply format #f (cdr args)))
			 #f))))
	(if (input-port? fp)
	    (let ((vars '()))
	      (format #t ";~A~%" file)
	      (do ((form (read fp) (read fp)))
		  ((eof-object? form))
		(set! vars (walk (if (symbol? form) form (car form)) form vars)))
	      (if *report-unused-top-level-functions* (report-usage file 0 'top-level-function #f vars))
	      (close-input-port fp)))))))


;;; TODO: 
;;; undef'd or misspelled stuff
;;; if all parts known, eval (error check etc)
;;; keep track of local lambda - functions and their args (like define etc)
;;; shadowed pars
;;; :arg checking doesn't work

