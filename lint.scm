;;; lint for s7 scheme

(provide 'lint.scm)

(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #f)
(define *report-undefined-variables* #f)
(define *report-shadowed-variables* #f)
(define *report-minor-stuff* #f)          ; let*, docstring checks, (= 1.0 x), numerical and boolean simplification

(define *load-file-first* #f)


;;; --------------------------------------------------------------------------------
;;; for snd-test.scm

(if (provided? 'snd)
    (set! *#readers* 
	  (cons (cons #\_ (lambda (str)
			    (if (string=? str "__line__")
				(port-line-number)
				#f)))
		*#readers*)))


;;; --------------------------------------------------------------------------------
;;; for Guile

(if (not (provided? 's7))
    (begin
      ;; here's an attempt to make this work in Guile 1.8/2.0
      ;;   (lint also uses catch, object->string)

      (use-modules (ice-9 format))

      (define hash-table-ref hash-ref)
      (define hash-table-set! hash-set!)

      (define (hash-table . args)
	(let ((ht (make-hash-table)))
	  (do ((lst args (cdr lst)))
	      ((null? lst) ht)
	    (hash-table-set! ht (car lst) (cdr lst)))))

      (define (pair-line-number pair) 0) ; the line number reported below is actually that of the enclosing right paren
      (define call-with-exit call/cc)

      (define (symbol->value sym)
	(symbol-binding #f sym))

      (define (procedure-arity f)
	(procedure-property f 'arity))

      (define (keyword? obj) #f)
      (define (keyword->symbol obj) obj)

      (define (eval-string . args) #f)
      (define (lint-eval f) (eval f (interaction-environment))) ; set lint-eval to #f if eval is not available
      (define (lint-values) '())

      (define (lint-member val lst func)
	(if (null? lst)
	    #f
	    (if (func val (car lst))
		lst
		(lint-member val (cdr lst) func))))

      (define (procedure-source f) #f) ; this only affects define* keyword checking
      
      (define (lint-length lst)
	(define (length-1 lst len)
	  (if (null? lst)
	      len
	      (if (not (pair? lst))
		  (- len)
		  (length-1 (cdr lst) (+ len 1)))))
	(length-1 lst 0))

      (define (lint-map func lst)
	(let ((result '()))
	  (for-each
	   (lambda (arg)
	     (let ((val (func arg)))
	       (if (not (null? val))
		   (set! result (cons val result)))))
	   lst)
	  (reverse result)))

      ;; in a pinch, object->string could perhaps be (format #f "~A" obj)
      ;;   and catch could be: (define (catch tag func err) (func))
      )

    ;; s7 
    (begin
      (define lint-eval eval)
      (define lint-values values)
      (define lint-map map)
      (define lint-length length)
      (define lint-member member)))

	

;;; --------------------------------------------------------------------------------

(define lint
  (let ((no-side-effect-functions 
	 (hash-table 
	  (map 
	   (lambda (op) 
	     (cons op #t)) 
	   (list '* '+ '- '/ '< '<= '= '> '>= 
		 'abs 'acos 'acosh 'and 'angle 'append 'ash 'asin 'asinh 'assoc 'assq 'assv 'atan 'atanh 
		 'begin 'boolean? 
		 'caaaar 'caaadr 'caaar 'caadar 'caaddr 'caadr 'caar 'cadaar 'cadadr 'cadar 'caddar 'cadddr 'caddr 'cadr 
		 'call-with-exit 'car 'case 'catch 'catch 'cdaaar 'cdaadr 'cdaar 'cdadar 'cdaddr 'cdadr 'cdar 'cddaar 'cddadr 
		 'cddar 'cdddar 'cddddr 'cdddr 'cddr 'cdr 'ceiling 'char->integer 'char-alphabetic? 'char-ci<=? 'char-ci<? 
		 'char-ci=? 'char-ci>=? 'char-ci>? 'char-downcase 'char-lower-case? 'char-numeric? 'char-ready? 'char-upcase 
		 'char-upper-case? 'char-whitespace? 'char<=? 'char<? 'char=? 'char>=? 'char>? 'char? 'complex? 'cond 
		 'cons 'constant? 'continuation? 'copy 'cos 'cosh 'current-environment 'current-error-port 'current-input-port 'current-output-port 
		 'defined? 'denominator 'do 'dynamic-wind 
		 'eof-object? 'eq? 'equal? 'eqv? 'even? 'exact->inexact 'exact? 'exp 'expt 
		 'floor 'for-each 
		 'gcd 'gensym 'global-environment 
		 'hash-table 'hash-table-ref 'hash-table-size 'hash-table? 'hook 'hook-apply 'hook-arity 'hook-documentation 'hook-functions 'hook? 
		 'if 'imag-part 'inexact->exact 'inexact? 'infinite? 'initial-environment 'input-port?  'integer->char 'integer-decode-float 
		 'integer-length 'integer? 
		 'keyword->symbol 'keyword? 
		 'lambda 'lcm 'length 'let 'let* 'letrec 'letrec* 'list 'list->string 'list->vector 'list-ref 'list-tail 
		 'list? 'log 'logand 'logior 'lognot 'logxor 
		 'macro? 'magnitude 'make-hash-table 'make-hook 'make-keyword 'make-list 'make-polar 'make-procedure-with-setter 
		 'make-random-state 'make-rectangular 'make-string 'make-type 'make-vector 'map 'max 'member 'memq 'memv 'min 'modulo 
		 'nan? 'negative? 'not 'null? 'number->string 'number? 'numerator 
		 'object->string 'odd? 'or 'output-port? 
		 'pair? 'port-closed? 'port-filename 'port-line-number 'positive? 'procedure-arity 'procedure-documentation 'procedure-environment 
		 'procedure-source 'procedure-with-setter? 'procedure? 'provided? 
		 'quasiquote 'quote 'quotient 
		 'random 'rational? 'rationalize 'real-part 'real? 'remainder 'reverse 'round 
		 's7-version 'sin 'sinh 'sqrt 'string 'string->list 'string->number 'string->symbol 'string-append 'string-ci<=? 'string-ci<? 
		 'string-ci=? 'string-ci>=? 'string-ci>? 'string-copy 'string-length 'string-ref 'string<=? 'string<? 'string=? 'string>=? 
		 'string>? 'string? 'substring 'symbol 'symbol->keyword 'symbol->string 'symbol->value 'symbol? 
		 'tan 'tanh 'truncate 
		 'vector 'vector->list 'vector-dimensions 'vector-length 'vector-ref 'vector? 
		 'zero?))))

	(function-types (hash-table 
			 (cons '* 2) ; if it might be an int, use 2, else 1
			 (cons '+ 2)
			 (cons '- 2)
			 (cons '/ 2)
			 (cons '< #t)
			 (cons '<= #t)
			 (cons '= #t)
			 (cons '> #t)
			 (cons '>= #t)
			 (cons 'abs 2)
			 (cons 'acos 1)
			 (cons 'acosh 1)
			 (cons 'angle 2)
			 (cons 'ash 2)
			 (cons 'asin 1)
			 (cons 'asinh 1)
			 (cons 'assoc 'list-or-f)
			 (cons 'assq 'list-or-f)
			 (cons 'assv 'list-or-f)
			 (cons 'atan 1)
			 (cons 'atanh 1)
			 (cons 'boolean? #t)
			 (cons 'ceiling 2)
			 (cons 'char->integer 2)
			 (cons 'char-alphabetic? #t)
			 (cons 'char-ci<=? #t)
			 (cons 'char-ci<? #t)
			 (cons 'char-ci=? #t)
			 (cons 'char-ci>=? #t)
			 (cons 'char-ci>? #t)
			 (cons 'char-downcase #\a)
			 (cons 'char-lower-case? #t)
			 (cons 'char-numeric? #t)
			 (cons 'char-ready? #t)
			 (cons 'char-upcase #\a)
			 (cons 'char-upper-case? #t)
			 (cons 'char-whitespace? #t)
			 (cons 'char<=? #t)
			 (cons 'char<? #t)
			 (cons 'char=? #t)
			 (cons 'char>=? #t)
			 (cons 'char>? #t)
			 (cons 'char? #t)
			 (cons 'close-input-port (if #f #f))
			 (cons 'close-output-port (if #f #f))
			 (cons 'complex? #t)
			 (cons 'cons '(1))
			 (cons 'constant? #t)
			 (cons 'continuation? #t)
			 (cons 'cos 2)
			 (cons 'cosh 1)
			 (cons 'defined? #t)
			 (cons 'denominator 2)
			 (cons 'display (if #f #f))
			 (cons 'environment? #t)
			 (cons 'eof-object? #t)
			 (cons 'eq? #t)
			 (cons 'equal? #t)
			 (cons 'eqv? #t)
			 (cons 'even? #t)
			 (cons 'exact->inexact 1)
			 (cons 'exact? #t)
			 (cons 'exp 2)
			 (cons 'expt 2)
			 (cons 'floor 2)
			 (cons 'for-each (if #f #f))
			 (cons 'gcd 2)
			 (cons 'gensym 'symbol)
			 (cons 'imag-part 2)
			 (cons 'inexact->exact 2)
			 (cons 'inexact? #t)
			 (cons 'infinite? #t)
			 (cons 'input-port? #t)
			 (cons 'integer->char #\a)
			 (cons 'integer-decode-float '(1))
			 (cons 'integer-length 2)
			 (cons 'integer? #t)
			 (cons 'keyword->symbol 'symbol)
			 (cons 'keyword? #t)
			 (cons 'lcm 2)
			 (cons 'length 2)
			 (cons 'list '(1))
			 (cons 'list->string "")
			 (cons 'list->vector #())
			 (cons 'list? #t)
			 (cons 'log 2)
			 (cons 'logand 2)
			 (cons 'logior 2)
			 (cons 'lognot 2)
			 (cons 'logxor 2)
			 (cons 'macro? #t)
			 (cons 'magnitude 2)
			 (cons 'make-list '(1))
			 (cons 'make-polar 2) 
			 (cons 'make-rectangular 2)
			 (cons 'make-string "")
			 (cons 'make-vector #())
			 (cons 'map '(1))
			 (cons 'max 2)
			 (cons 'member 'list-or-f)
			 (cons 'memq 'list-or-f)
			 (cons 'memv 'list-or-f)
			 (cons 'min 2)
			 (cons 'modulo 2)
			 (cons 'nan? #t)
			 (cons 'negative? #t)
			 (cons 'newline (if #f #f))
			 (cons 'not #t)
			 (cons 'null? #t)
			 (cons 'number->string "")
			 (cons 'number? #t)
			 (cons 'numerator 2)
			 (cons 'object->string "")
			 (cons 'odd? #t)
			 (cons 'output-port? #t)
			 (cons 'pair? #t)
			 (cons 'peek-char 'char-or-eof)
			 (cons 'port-closed? #t)
			 (cons 'positive? #t)
			 (cons 'procedure? #t)
			 (cons 'provided? #t)
			 (cons 'quotient 2)
			 (cons 'random-state->list '(1))
			 (cons 'rational? #t)
			 (cons 'rationalize 2)
			 (cons 'read-byte 'number-or-eof)
			 (cons 'read-char 'char-or-eof)
			 (cons 'read-line 'string-or-eof)
			 (cons 'real-part 2)
			 (cons 'real? #t)
			 (cons 'remainder 2)
			 (cons 'round 2)
			 (cons 'sin 2)
			 (cons 'sinh 1)
			 (cons 'sqrt 2)
			 (cons 'string "")
			 (cons 'string->list '(1))
			 (cons 'string->number 'number-or-f)
			 (cons 'string->symbol 'symbol)
			 (cons 'string-append "")
			 (cons 'string-ci<=? #t)
			 (cons 'string-ci<? #t)
			 (cons 'string-ci=? #t)
			 (cons 'string-ci>=? #t)
			 (cons 'string-ci>? #t)
			 (cons 'string-copy "")
			 (cons 'string-length 2)
			 (cons 'string-ref #\a)
			 (cons 'string<=? #t)
			 (cons 'string<? #t)
			 (cons 'string=? #t)
			 (cons 'string>=? #t)
			 (cons 'string>? #t)
			 (cons 'string? #t)
			 (cons 'symbol 'symbol)
			 (cons 'symbol->string "")
			 (cons 'symbol? #t)
			 (cons 'tan 2)
			 (cons 'tanh 1)
			 (cons 'truncate 2)
			 (cons 'vector #())
			 (cons 'vector->list '(1))
			 (cons 'vector-length 2)
			 (cons 'vector? #t)
			 (cons 'write (if #f #f))
			 (cons 'write-char (if #f #f))
			 (cons 'zero? #t)
			 (cons 'procedure-with-setter? #t)))
	(loaded-files #f)
	(globals #f)
	(undefined-identifiers #f)
	(last-simplify-boolean-line-number -1)
	(last-simplify-numeric-line-number -1))
    
    
    ;;; --------------------------------------------------------------------------------

    (define (integer-between-2-and-16? radix) 
      (and (integer? radix) 
	   (<= 2 radix 16)))
    
    (define (non-negative-integer? index)
      (and (integer? index) 
	   (not (negative? index))))
    
    (define (non-zero-number? x)
      (and (number? x)
	   (not (zero? x))))
    
    (define (real-but-not-rational? x)
      (and (real? x)
	   (not (rational? x))))
    
    (define (any-real? lst) ; ignore 0.0 and 1.0 in this since they normally work
      (and (pair? lst)
	   (or (and (number? (car lst))
		    (not (rational? (car lst)))
		    (not (= (car lst) 0.0))
		    (not (= (car lst) 1.0)))
	       (any-real? (cdr lst)))))
    
    (define (thunk? p)
      (and (procedure? p)
	   (let ((arity (procedure-arity p)))
	     (and (= (car arity) 0)
		  (= (cadr arity) 0)
		  (not (caddr arity))))))
    
    (define (integer-between-0-and-255? i) 
      (and (integer? i) (<= 0 i 255)))
    
    (define (non-constant-string? str)
      (and (string? str)
	   (not (constant? str))))
    
    (define (non-constant-list? lst)
      (and (pair? lst)
	   (not (constant? lst))))
    
    (define (non-constant-vector? v)
      (and (vector? v)
	   (not (constant? v))))
    
    (define (sequence? obj)
      ;; scheme and C types here are ok, so...
      (and (not (number? obj))
	   (not (char? obj))
	   (not (boolean? obj))
	   (not (symbol? obj))))
    
    (define (pair-or-null? obj) ; list? is proper-list?
      (or (pair? obj)
	  (null? obj)))
    

    (define (truncated-list->string form)
      ;; return form -> string with limits on its length
      (let* ((str (object->string form))
	     (len (string-length str)))
	(if (< len 40)
	    (format #f " ~A" str)
	    (if (<= len 80)
		(format #f "~%        ~A" str)
		(call-with-exit
		 (lambda (return)
		   (do ((i 80 (- i 1)))
		       ((= i 40) 
			(format #f "~%        ~A..." str))
		     (if (char-whitespace? (string-ref str i))
			 (return (format #f "~%        ~A..." (substring str 0 (+ i 1))))))))))))


    (define (lists->string f1 f2)
      ;; same but 2 strings that may need to be lined up vertically
      (let* ((str1 (object->string f1))
	     (len1 (string-length str1))
	     (str2 (object->string f2))
	     (len2 (string-length str2)))
	(if (< (+ len1 len2) 20)
	    (format #f " ~A -> ~A" str1 str2)
	    (if (< (+ len1 len2) 70)
		(format #f "~%        ~A -> ~A" str1 str2)
		(format #f "~%        ~A ->~%        ~A" str1 str2)))))


    (define (env-member? arg env)
      ;; is arg in env using car
      (or (lint-member arg env (lambda (a b) (eq? a (car b))))
	  (hash-table-ref globals arg)))


    (define (env-member arg env)
      ;; find data associated with arg in env
      (let ((lst (lint-member arg env (lambda (a b) 
					(if (not (pair? b))
					    (format #t "env-member ~A: ~A ~A in ~A~%" arg a b env))
					(eq? a (car b))))))
	(if (pair? lst)
	    (car lst)
	    (hash-table-ref globals arg))))


    (define (side-effect? form env)
      ;; could evaluation of form have any side effects (like IO etc)

      (if (pair? form)
	  (or (and (not (hash-table-ref no-side-effect-functions (car form))) ; if func is not in that list, make no assumptions about it
		   (or (not (eq? (car form) 'format))                 ; (format #f ...)
		       (cadr form)))
	      (call-with-exit
	       (lambda (return)
		 (for-each
		  (lambda (f)
		    (if (side-effect? f env)
			(return #t)))
		  (cdr form))
		 #f)))
	  (and (symbol? form)
	       (not (hash-table-ref no-side-effect-functions form))
	       (not (env-member? form env)))))


    (define (just-constants? form env)
      ;; can we probably evaluate form given just built-in stuff?
      (define (lint-constant? arg)
	(or (number? arg)
	    (string? arg)
	    (null? arg)
	    (boolean? arg)
	    (char? arg)))
      (or (lint-constant? form)
	  (and (pair? form)
	       (or (and (hash-table-ref no-side-effect-functions (car form))
			(not (env-member (car form) env))) ; e.g. exp declared locally as a list
		   (lint-constant? (car form)))
	       (just-constants? (cdr form) env))))


    (define (just-symbols? form)
      (or (null? form)
	  (symbol? form)
	  (and (pair? form)
	       (symbol? (car form))
	       (just-symbols? (cdr form)))))

    
    (define (repeated-member? lst env)
      (and (pair? lst)
	   (or (and (or (not (pair? (car lst)))
			(not (side-effect? (car lst) env)))
		    (pair? (cdr lst))
		    (member (car lst) (cdr lst)))
	       (repeated-member? (cdr lst) env))))


    (define (check-for-repeated-args name line-number head form env)
      (if (repeated-member? (cdr form) env)
	  (if (or (member head '(eq? eqv? equal?))
		  (and (= (lint-length form) 3)
		       (member head '(= / max min < > <= >= - quotient remainder modulo and or
				      string=? string<=? string>=? string<? string>?
				      char=? char<=? char>=? char<? char>?))))
	      (format #t "  ~A (line ~D): this looks odd:~A~%"
		      name line-number 
		      ;; sigh (= a a) could be used to check for non-finite numbers, I suppose,
		      ;;   and (/ 0 0) might be deliberate (as in gmp)
		      (truncated-list->string form))
	      (if (member head '(= max min < > <= >= and or
				 string=? string<=? string>=? string<? string>?
				 char=? char<=? char>=? char<? char>?))
		  (format #t "  ~A (line ~D): it looks odd to have repeated arguments in~A~%"
			  name line-number (truncated-list->string form))))))


    (define (check-for-repeated-args-with-not name line-number form env)

      (define (repeated-member-with-not? lst env)
	(and (pair? lst)
	     (or (and (or (not (pair? (car lst)))
			  (not (side-effect? (car lst) env)))
		      (or (member (list 'not (car lst)) (cdr lst))
			  (and (pair? (car lst))
			       (eq? (caar lst) 'not)
			       (= (lint-length (car lst)) 2)
			       (member (cadar lst) (cdr lst)))))
		 (repeated-member-with-not? (cdr lst) env))))
      
      (if (repeated-member-with-not? (cdr form) env)
	  (format #t "  ~A (line ~D): this looks odd:~A~%"
		  name line-number 
		  (truncated-list->string form))))


    (define (check-args name line-number head form checkers env)
      ;; check for obvious argument type problems
      (let ((arg-number 1))
	(call-with-exit
	 (lambda (done)
	   (for-each 
	    (lambda (arg)
	      (let ((checker (if (list? checkers) 
				 (car checkers) 
				 checkers)))  
		(if (pair? arg)
		    (let ((op (hash-table-ref function-types (car arg))))
		      (case op
			((number-or-f list-or-f)
			 (if checker
			     (format #t "  ~A (line ~D): ~A argument ~D might be #f:~A~%"
				     name line-number head arg-number
				     (truncated-list->string form))))
			((number-or-eof char-or-eof string-or-eof)
			 (if checker
			     (format #t "  ~A (line ~D): ~A argument ~D might be the eof object:~A~%"
				     name line-number head arg-number
				     (truncated-list->string form))))
			(else
			 (if (or (and op
				      (not (checker op)))
				 (and (just-constants? arg env)
				      (catch #t 
					     (lambda ()
					       (and lint-eval
						    (not (checker (lint-eval arg)))))
					     (lambda ignore-catch-error-args
					       #f))))
			     (format #t "  ~A (line ~D): ~A's argument ~D should be a~A ~A: ~S:~A~%" 
				     name line-number head arg-number 
				     (if (char=? (string-ref (procedure-name checker) 0) #\i) "n" "")
				     checker arg 
				     (truncated-list->string form))
			     
			     (if (and (eq? (car arg) 'if)
				      (= (lint-length arg) 3)
				      (not (checker (if #f #f))))
				 (format #t "  ~A (line ~D): ~A arg might be ~A:~A~%"
					 name line-number head
					 (if #f #f)
					 (truncated-list->string form)))))))

		    (if (and (not (symbol? arg))
			     (not (checker arg)))
			(format #t "  ~A (line ~D): ~A's argument ~D should be a~A ~A: ~S:~A~%" 
				name line-number head arg-number 
				(if (char=? (string-ref (procedure-name checker) 0) #\i) "n" "")
				checker arg 
				(truncated-list->string form))))
		(if (list? checkers)
		    (if (null? (cdr checkers))
			(done)
			(set! checkers (cdr checkers))))
		(set! arg-number (+ arg-number 1))))
	    (cdr form))))))


    (define (ref-var name env)
      ;; if name is in env, set its "I've been referenced" flag
      (let ((data (env-member name env)))
	(if (pair? data)
	    (list-set! data 1 #t))))


    (define (set-var name env)
      (let ((data (env-member name env)))
	(if (pair? data)
	    (list-set! data 2 #t))))

    
    (define (proper-list lst)
      ;; return lst as a proper list
      (if (pair? lst)
	  (cons (car lst) 
		(if (pair? (cdr lst)) 
		    (proper-list (cdr lst)) 
		    (if (null? (cdr lst)) 
			'() 
			(list (cdr lst)))))
	  lst))

    
    (define (keywords lst)
      ;; count keywords in lst
      (let ((keys 0))
	(for-each 
	 (lambda (arg)
	   (if (keyword? arg)
	       (set! keys (+ keys 1))))
	 lst)
	keys))

    
    (define (caar-member obj lst)
      (and (pair? lst)
	   (or (and (pair? (car lst))
		    (eq? obj (caar lst)))
	       (caar-member obj (cdr lst)))))

    
    (define (tree-member sym tree)
      (and (pair? tree)
	   (or (eq? (car tree) sym)
	       (and (pair? (car tree))
		    (tree-member sym (car tree)))
	       (tree-member sym (cdr tree)))))


    (define (tree-car-member sym tree)
      (and (pair? tree)
	   (or (eq? (car tree) sym)
	       (and (pair? (car tree))
		    (tree-car-member sym (car tree)))
	       (and (pair? (cdr tree))
		    (call-with-exit
		     (lambda (return)
		       (for-each
			(lambda (subtree)
			  (if (tree-car-member sym subtree)
			      (return #t)))
			(cdr tree))
		       #f))))))

    
    (define (tree-member-ignoring-car sym tree)
      ;; return #t if sym is in tree, but not treated as a function
      ;; this is a mess!
      (or (eq? sym tree)
	  (and (pair? tree)
	       (or (and (pair? (car tree))
			(tree-member-ignoring-car sym (car tree)))
		   (and (pair? (cdr tree))
			(call-with-exit
			 (lambda (return)
			   (for-each
			    (lambda (l)
			      (if (or (eq? sym l)
				      (and (pair? l)
					   (tree-member-ignoring-car sym l)))
				  (return #t)))
			    (cdr tree))
			   #f)))
		   (eq? sym (cdr tree))))))

    
    (define (remove x list) 
      (cond ((null? list) '()) 
	    ((eq? (car list) x) (cdr list)) 
	    (else (cons (car list) 
			(remove x (cdr list))))))


    (define (simplify-boolean form true false env)
      ;; (or)->#f, (or x) -> x, (or x ... from here on we know x is #f), (or x #t...) -> (or x #t), any constant expr can be collapsed
      ;;   (or ... (or ...) ...) -> or of all, (or ... #f ...) toss the #f
      ;; similarly for and
      ;; (or ... (not (and ...))) -> (or ... (not x) [from here we know x is true] (not y)...)
      ;; (or ... (not (and x1 x2 ...))) -> (or ... (not x1) (not x2)...), but is that simpler?

      ;; I wonder how far this could be pushed
      ;;   (or x1 x2 x1) -> (or x1 x2) 
      ;;   (and x1 x2 x1) -> (and x2 x1)

      (define (classify e)
	;; do we already know that e is true or false?
	(if (member e true)
	    #t ; the simple boolean is passed back which will either be dropped or will stop the outer expr build
	    (if (member e false)
		#f
		;; eval of a constant expression here is tricky -- for example, (sqrt 2) should not be turned into 1.414...
		(if (and lint-eval
			 (just-constants? e env))
		    (catch #t
			   (lambda ()
			     (let ((val (lint-eval e)))
			       (if (boolean? val)
				   val
				   e)))
			   (lambda ignore e))
		    e))))

      (define (store e value and/or)
	;; we can't make any assumptions about the expression if it might have side effects
	;;   for example (or (= (oscil o) 0.0) (= (oscil o) 0.0)) can't be reduced
	(if (not (side-effect? e env))
	    (if (eq? and/or 'or)
		(if (eq? value #t)              ; or, so it's false if unknown
		    (set! true (cons e true))
		    (set! false (cons e false)))
		(if (eq? value #f)
		    (set! false (cons e false))
		    (set! true (cons e true))))))

      (define (remove-duplicates-and-reverse lst)
	(let ((new-lst '()))
	  (for-each
	   (lambda (e)
	     (if (or (not (member e new-lst))
		     (not (member e true)))
		 (set! new-lst (cons e new-lst))))
	   lst)
	  new-lst))
	  

      (if (or (not (pair? form))
	      (not (member (car form) '(or and not))))
	  (classify form)
	  (let ((len (lint-length form)))
	    
	    (case (car form)
	      
	      ((not)
	       (if (= len 2)
		   (let* ((arg (cadr form))
			  (val (if (and (pair? arg)
					(member (car arg) '(and or not)))
				   (classify (simplify-boolean arg true false env))
				   (classify arg))))
		     (if (boolean? val)
			 (not val)
			 (if (or (and (not (symbol? arg))
				      (not (pair? arg)))
				 (and (pair? arg)
				      (not (env-member? (car arg) env))
				      (not (member (hash-table-ref function-types (car arg)) '(#f #t list-or-f number-or-f)))))
			     #f
			     (if (and (pair? arg)
				      (pair? (cdr arg))
				      (eq? (car arg) 'not))
				 (cadr arg)
				 (if (not (equal? val arg))
				     `(not ,val)
				     form)))))
		   form))
	      
	      ((or)
	       (if (= len 1)
		   #f
		   (if (= len 2)
		       (classify (cadr form))
		       (let ((new-form '()))
			 (do ((exprs (cdr form) (cdr exprs)))
			     ((null? exprs) 
			      (if (null? new-form)
				  #f
				  (if (null? (cdr new-form))
				      (car new-form)
				      `(or ,@(reverse new-form)))))
			   (let* ((e (car exprs))
				  (val (classify e)))
			     
			     (if (and (pair? val)
				      (member (car val) '(and or not)))
				 (set! val (classify (simplify-boolean e true false env))))
			     
			     (if (not (eq? val #f))                 ; #f in or is ignored
				 (if (or (eq? val #t)               ; #t or any non-#f constant in or ends the expression
					 (and (not (pair? val))
					      (not (symbol? val))))
				     (begin
				       (if (null? new-form)         ; (or x1 123) -> value of x1, so we can't throw it away here, unlike the and case
					   (set! new-form `(,val))
					   (set! new-form (append `(,val) new-form))) ; reversed when returned
				       (set! exprs '(#t)))
				     
				     ;; (or x1 x2 x1) -> (or x1 x2) is ok because if we get to x2, x1 is #f, so trailing x1 would still be #f
				     
				     (if (and (pair? e)             ; (or ...) -> splice into current
					      (eq? (car e) 'or))
					 (set! exprs (append e (cdr exprs))) ; we'll skip the 'or in do step
					 (begin                     ; else add it to our new expression with value #f
					   (store e val 'or)
					   (set! new-form (cons val new-form))))))))))))
	      
	      ((and)
	       (if (= len 1)
		   #t
		   (if (= len 2)
		       (classify (cadr form))
		       (let ((new-form '())
			     (preceding? #f))
			 (do ((exprs (cdr form) (cdr exprs)))
			     ((null? exprs) 
			      (if (null? new-form)
				  #t
				  (if (null? (cdr new-form))
				      (car new-form)
				      (if preceding?
					  `(and ,@(remove-duplicates-and-reverse new-form))
					  `(and ,@(reverse new-form))))))

			   (let* ((e (car exprs))
				  (val (classify e)))
			     
			     (if (and (pair? val)
				      (member (car val) '(and or not)))
				 (set! val (classify (simplify-boolean e true false env))))
			     
			     ;; (and x1 x2 x1) is not reducible, unless to (and x2 x1)
			     ;;   the final thing has to remain at the end, but can be deleted earlier if it can't short-circuit the evaluation
			     
			     (if (eq? val #t)                 ; #t in and is (eventually) ignored
				 (if (and (not (eq? e #t))
					  (not (just-constants? e env)))
				     (begin
				       (set! preceding? #t)
				       (set! new-form (cons e new-form))))
				 (if (eq? val #f)             ; #f in and ends the expression
				     (begin
				       (if (or (null? new-form)   
					       (just-symbols? new-form))
					   (set! new-form '(#f))
					   (set! new-form (append '(#f) new-form)))
				       (set! exprs '(#f)))
				     (if (and (pair? e)       ; if (and ...) splice into current
					      (eq? (car e) 'and))
					 (set! exprs (append e (cdr exprs)))
					 (begin                 ; else add it to our new expression with value #t
					   (store e val 'and)
					   (set! new-form (cons val new-form))))))))))))
	      ))))


    (define (numeric? op)
      (member op '(+ * - / 
		   sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh 
		   log exp expt sqrt make-polar make-rectangular
		   imag-part real-part abs magnitude angle max min exact->inexact
		   modulo remainder quotient lcd gcd
		   rationalize inexact->exact
		   logior lognot logxor logand numerator denominator 
		   floor round truncate ceiling ash)))


    (define (simplify-numerics form env)
      ;;   I first tried a table of rules, but the code was unreadable, so
      ;;   here I'll split out each case by hand.
      ;; This is not an agressive simplification.

      ;; this returns a form, possibly the original simplified
      (let ((complex-result? (lambda (op) (member op '(+ * - / 
						       sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh 
						       log exp expt sqrt make-polar make-rectangular))))
	    (real-result? (lambda (op) (member op '(imag-part real-part abs magnitude angle max min exact->inexact
						    modulo remainder quotient lcd gcd))))
	    (rational-result? (lambda (op) (member op '(rationalize inexact->exact))))
	    (integer-result? (lambda (op) (member op '(logior lognot logxor logand numerator denominator 
						       floor round truncate ceiling ash)))))

	(define (inverse-op op)
	  (case op 
	    ((sin) 'asin) ((cos) 'acos) ((tan) 'atan) ((asin) 'sin) ((acos) 'cos) ((atan) 'tan)
	     ((sinh) 'asinh) ((cosh) 'acosh) ((tanh) 'atanh) ((asinh) 'sinh) ((acosh) 'cosh) ((atanh) 'tanh)
	     ((log) exp) ((exp) log)))

	
	(define (remove-all x list) 
	  (cond ((null? list) '()) 
		((equal? (car list) x) (remove-all x (cdr list)))
		(else (cons (car list) (remove-all x (cdr list))))))

	(define (remove-duplicates lst)
	  (letrec ((rem-dup
		    (lambda (lst nlst)
		      (cond ((null? lst) nlst)
			    ((member (car lst) nlst) (rem-dup (cdr lst) nlst))
			    (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
	    (reverse (rem-dup lst '()))))

	(define (splice-if f lst)
	  (cond ((null? lst) '())
		((pair? lst)
		 (if (and (pair? (car lst))
			  (f (caar lst)))
		     (append (splice-if f (cdar lst)) (splice-if f (cdr lst)))
		     (cons (car lst) (splice-if f (cdr lst)))))
		(#t lst)))

	(define (just-rationals? form)
	  (or (null? form)
	      (rational? form)
	      (and (pair? form)
		   (rational? (car form))
		   (just-rationals? (cdr form)))))
    
	(define (just-integers? form)
	  (or (null? form)
	      (integer? form)
	      (and (pair? form)
		   (integer? (car form))
		   (just-integers? (cdr form)))))
    
	(define (simplify-arg x)
	  (if (or (number? x)
		  (not (pair? x))
		  (env-member (car x) env)
		  (not (hash-table-ref no-side-effect-functions (car x))))
	      x
	      (let ((f (simplify-numerics x env)))
		(if (and (pair? f)
			 lint-eval
			 (just-rationals? f))
		    (catch #t
			   (lambda ()
			     (lint-eval f))
			   (lambda ignore f))
		    f))))

	(let* ((args (map simplify-arg (cdr form)))
	       (len (lint-length args)))

	  (case (car form)
	    ((+)
	     (case len
	       ((0) 0)
	       ((1) (car args))
	       (else 
		(let ((val (remove-all 0 (splice-if (lambda (x) (eq? x '+)) args))))
		  (case (lint-length val)
		    ((0) 0)
		    ((1) (car val))
		    (else 
		     (if (just-rationals? val)
			 (apply + val)
			 `(+ ,@val))))))))

	    ((*)
	     (case len
	       ((0) 1)
	       ((1) (car args))
	       (else 
		(let ((val (remove-all 1 (splice-if (lambda (x) (eq? x '*)) args))))
		  (case (lint-length val)
		    ((0) 0)
		    ((1) (car val))
		    (else 
		     (if (just-rationals? val)
			 (apply * val)
			 `(* ,@val))))))))

	    ((-)
	     (case len
	       ((0) form)
	       ((1) ; negate
		(if (number? (car args))
		    (- (car args))
		    (if (not (list? (car args)))
			`(- ,@args)
			(case (lint-length (car args))
			  ((2) (if (eq? (caar args) '-)
				   (cadar args)          ; (- (- x)) -> x
				   `(- ,@args)))
			  ((3) (if (eq? (caar args) '-)
				   `(- ,(caddr (car args)) ,(cadr (car args))) ; (- (- x y)) -> (- y x)
				   `(- ,@args)))
			  (else `(- ,@args))))))
	       ((2) 
		(if (just-rationals? args)
		    (apply - args)
		    (if (equal? (car args) 0)
			`(- ,(cadr args))     ; (- 0 x) -> (- x)
			(if (equal? (cadr args) 0)
			    (car args)        ; (- x 0) -> x
			    (if (and (pair? (car args))
				     (eq? (caar args) '-)
				     (> (lint-length (car args)) 2))
				`(- ,@(cdar args) ,(cadr args)) ; (- (- x y) z) -> (- x y z) but leave (- (- x) ...)
				`(- ,@args))))))
	       (else 
		(if (just-rationals? args)
		    (apply - args)
		    (let ((nargs (remove-all 0 (cdr args))))
		      (if (null? nargs)
			  (car args) ; (- x 0 0 0)?
			  (if (and (equal? (car args) 0)
				   (= (lint-length nargs) 1))
			      `(- ,(car nargs)) ; (- 0 0 0 x)?
			      `(- ,@(cons (car args) nargs)))))))))

	    ((/)
	     (case len
	       ((0) form)
	       ((1) ; invert
		(if (number? (car args))
		    (if (zero? (car args))
			`(/ ,(car args))
			(/ (car args)))
		    (if (pair? (car args))
			(if (and (= (lint-length (car args)) 2)
				 (eq? (caar args) '/))
			    (cadar args)
			    `(/ ,@args))
			form)))
	       (else 
		(if (and (just-rationals? args)
			 (not (member 0 args))
			 (not (member 0.0 args)))
		    (apply / args)
		    `(/ ,@(cons (car args) (remove-all 1 (cdr args))))))))

	    ((sin cos asin acos sinh cosh tanh asinh acosh atanh log exp)
	     (if (and (= len 1)
		      (pair? (car args))
		      (= (lint-length (car args)) 2)
		      (eq? (caar args) (inverse-op (car form))))
		 (cadar args)
		 `(,(car form) ,@args)))

	    ((sqrt)
	     (if (and (pair? args)
		      (equal? (car args) 0))
		 0
		 `(sqrt ,@args)))

	    ((floor round ceiling truncate numerator)
	     (if (= len 1)
		 (if (or (integer? (car args))
			 (and (pair? (car args))
			      (integer-result? (caar args))))
		     (car args)
		     `(,(car form) ,@args))
		 form))
		 
	    ((abs magnitude)
	     (if (= len 1)
		 (if (and (pair? (car args))
			  (member (caar args) '(abs magnitude)))
		     (car args)
		     (if (rational? (car args))
			 (abs (car args))
			 `(,(car form) ,@args)))
		 form))

	    ((imag-part)
	     (if (= len 1)
		 (if (or (not (real? (car args)))
			 (and (pair? (car args))
			      (complex-result? (caar args))))
		     `(imag-part ,@args)
		     0.0)
		 form))

	    ((real-part)
	     (if (= len 1)
		 (if (or (real? (car args))
			 (and (pair? (car args))
			      (real-result? (caar args))))
		     (car args)
		     `(real-part ,@args))
		 form))

	    ((denominator)
	     (if (= len 1)
		 (if (or (integer? (car args))
			 (and (pair? (car args))
			      (integer-result? (caar args))))
		     1
		     `(denominator ,@args))
		 form))

	    ((rationalize make-polar make-rectangular lognot logxor ash modulo remainder quotient exact->inexact tan)
	     (if (just-rationals? args)
		 (apply (symbol->value (car form)) args)
		 `(,(car form) ,@args)))

	    ((atan expt angle) ; (angle -1) and (* 4 (atan 1)) are common ways to get pi, so don't optimize these 
	     `(,(car form) ,@args))

	    ((inexact->exact)
	     (if (provided? 's7)
		 (if (= len 1)
		     (if (or (rational? (car args))
			     (and (pair? (car args))
				  (or (rational-result? (caar args))
				      (integer-result? (caar args)))))
			 (car args)
			 `(inexact->exact ,@args))
		     form)
		 `(inexact->exact ,@args)))

	    ((logior)
	     (set! args (remove-duplicates args))
	     (if (null? args)
		 0
		 (if (member -1 args)
		     -1
		     (if (just-integers? args)
			 (apply logior args)
			 `(logior ,@args)))))

	    ((logand)
	     (set! args (remove-duplicates args))
	     (if (null? args)
		 -1
		 (if (member 0 args)
		     0
		     (if (just-integers? args)
			 (apply logand args)
			 `(logand ,@args)))))
	     
	    ((gcd)
	     (set! args (remove-duplicates args))
	     (if (null? args)
		 0
		 (if (member 1 args)
		     1
		     (if (just-integers? args)
			 (apply gcd args)
			 `(gcd ,@(remove-duplicates args))))))
	     
	    ((lcm)
	     (set! args (remove-duplicates args))
	     (if (null? args)
		 1
		 (if (member 0 args)
		     0
		     (if (just-integers? args)
			 (apply lcm args)
			 `(lcm ,@args)))))

	    ((max min)
	     (set! args (remove-duplicates args))
	     (if (= len 1)
		 (car args)
		 (if (just-rationals? args)
		     (apply (symbol->value (car form)) args)
		     `(,(car form) ,@args))))
	    
	    (else `(,(car form) ,@args))))))

    
    (define (check-special-cases name line-number head form env)
      (case head
	((load) ; pick up the top level declarations
	 (if (>= (lint-length form) 2)
	     (scan form))
	 env)
	
	((= equal?)
	 (if (and *report-minor-stuff*
		  (> (lint-length form) 2)
		  (any-real? (cdr form)))
	     (format #t "  ~A (line ~D): ~A can be troublesome with floats:~A~%"
		     name line-number head 
		     (truncated-list->string form))))
	
	((if)
	 (let ((len (lint-length form)))
	   (if (> len 4)
	       (format #t "  ~A (line ~D): if has too many clauses: ~S~%" 
		       name line-number form)
	       (if (< len 3)
		   (format #t "  ~A (line ~D): if has too few clauses: ~S~%" 
			   name line-number form)
		   
		   (if *report-minor-stuff*
		       (let ((expr (if (and (pair? (cadr form))
					    (member (car (cadr form)) '(and or not)))
				       (simplify-boolean (cadr form) '() '() env)
				       (cadr form))))
			 (if (and (boolean? (list-ref form 2))
				  (not (null? (cdddr form)))
				  (boolean? (list-ref form 3))
				  (not (eq? (list-ref form 2) (list-ref form 3)))) ; !
			     (format #t "  ~A (line ~D): possible simplification:~A~%"
				     name line-number
				     (lists->string form (if (list-ref form 2)
							     expr
							     `(not ,expr)))))
			 (if (and (= len 4)
				  (equal? (caddr form) (cadddr form)))
			     (format #t "  ~A (line ~D): if is not needed here:~A~%"
				     name line-number 
				     (truncated-list->string form)))))))))
	
	((car cdr)
	 (if (and *report-minor-stuff*
		  (pair? (cadr form))
		  (eq? (car (cadr form)) 'cons))
	     (format #t "  ~A (line ~D): (~A~A) is the same as~A~%"
		     name line-number head
		     (truncated-list->string (cadr form))
		     (if (eq? head 'car)
			 (truncated-list->string (cadr (cadr form)))
			 (truncated-list->string (caddr (cadr form)))))))
	
	((and or not)
	 (if (and *report-minor-stuff*
		  (not (= line-number last-simplify-boolean-line-number)))
	     (let ((val (simplify-boolean form '() '() env)))
	       (set! last-simplify-boolean-line-number line-number)
	       (if (not (equal? form val))
		   (format #t "  ~A (line ~D): possible simplification:~A~%"
			   name line-number 
			   (lists->string form val))))))
	
	((call/cc call-with-current-continuation)
	 (let ((continuation (and (pair? (cdr form))
				  (pair? (cadr form))
				  (eq? (caadr form) 'lambda)
				  (pair? (cdadr form))
				  (pair? (cadadr form))
				  (car (cadadr form)))))
	   (if (symbol? continuation)
	       (let ((body (cddadr form)))
		 (if (not (eq? continuation (car body)))
		     (if (not (tree-member continuation body))
			 (format #t "  ~A (line ~D): ~A is not needed:~A~%"
				 name line-number head 
				 (truncated-list->string form))
			 (if (and (provided? 's7)
				  (not (tree-member-ignoring-car continuation body)))
			     (format #t "  ~A (line ~D): ~A could be call-with-exit:~A~%"	
				     name line-number head 
				     (truncated-list->string form)))))))))
	
	((/)
	 (if (not (null? (cdr form)))
	     (if (and (null? (cddr form))
		      (number? (cadr form))
		      (zero? (cadr form)))
		 (format #t "  ~A (line ~D): attempt to invert zero:~A~%"
			 name line-number 
			 (truncated-list->string form))
		 (if (and (not (null? (cddr form)))
			  (member 0 (cddr form)))
		     (format #t "  ~A (line ~D): attempt to divide by 0:~A~%"
			     name line-number
			     (truncated-list->string form))))))

	((sort!)
	 (if (member (caddr form) '(= <= >= eq? eqv? equal?
				      string=? string<=? string>=? char=? char<=? char>=?
				      string-ci=? string-ci<=? string-ci>=? char-ci=? char-ci<=? char-ci>=?))
	     (format #t "  ~A (line ~D): sort! with ~A may hang:~A~%"
		     name line-number head 
		     (truncated-list->string form))))))


    (define check-call 
      (let ((argument-data
	     (hash-table 
	      (cons '* number?)
	      (cons '+ number?)
	      (cons '- number?)
	      (cons '/ number?)
	      (cons '< real?)
	      (cons '<= real?)
	      (cons '= number?)
	      (cons '> real?)
	      (cons '>= real?)
	      (cons 'abs number?)
	      (cons 'acos number?)
	      (cons 'acosh number?)
	      (cons 'angle number?)
	      (cons 'ash (list integer? integer?))
	      (cons 'asin number?)
	      (cons 'asinh number?)
	      (cons 'atan (list number? number?))
	      (cons 'atanh number?)
	      (cons 'caaaar pair?)
	      (cons 'caaadr pair?)
	      (cons 'caaar pair?)
	      (cons 'caadar pair?)
	      (cons 'caaddr pair?)
	      (cons 'caadr pair?)
	      (cons 'caar pair?)
	      (cons 'cadaar pair?)
	      (cons 'cadadr pair?)
	      (cons 'cadar pair?)
	      (cons 'caddar pair?)
	      (cons 'cadddr pair?)
	      (cons 'caddr pair?)
	      (cons 'cadr pair?)
	      (cons 'call-with-current-continuation procedure?)
	      (cons 'call-with-exit procedure?)
	      (cons 'call-with-input-file (list string? procedure?))
	      (cons 'call-with-input-string (list string? procedure?))
	      (cons 'call-with-output-file (list string? procedure?))
	      (cons 'call-with-output-string procedure?)
	      (cons 'call/cc procedure?)
	      (cons 'car pair?)
	      (cons 'cdaaar pair?)
	      (cons 'cdaadr pair?)
	      (cons 'cdaar pair?)
	      (cons 'cdadar pair?)
	      (cons 'cdaddr pair?)
	      (cons 'cdadr pair?)
	      (cons 'cdar pair?)
	      (cons 'cddaar pair?)
	      (cons 'cddadr pair?)
	      (cons 'cddar pair?)
	      (cons 'cdddar pair?)
	      (cons 'cddddr pair?)
	      (cons 'cdddr pair?)
	      (cons 'cddr pair?)
	      (cons 'cdr pair?)
	      (cons 'ceiling real?)
	      (cons 'char->integer char?)
	      (cons 'char-alphabetic? char?)
	      (cons 'char-ci<=? char?)
	      (cons 'char-ci<? char?)
	      (cons 'char-ci=? char?)
	      (cons 'char-ci>=? char?)
	      (cons 'char-ci>? char?)
	      (cons 'char-downcase char?)
	      (cons 'char-lower-case? char?)
	      (cons 'char-numeric? char?)
	      (cons 'char-upcase char?)
	      (cons 'char-upper-case? char?)
	      (cons 'char-whitespace? char?)
	      (cons 'char<=? char?)
	      (cons 'char<? char?)
	      (cons 'char=? char?)
	      (cons 'char>=? char?)
	      (cons 'char>? char?)
	      (cons 'cos number?)
	      (cons 'cosh number?)
	      (cons 'denominator rational?)
	      (cons 'dynamic-wind (list thunk? thunk? thunk?))
	      (cons 'eval-string string?)
	      (cons 'even? integer?)
	      (cons 'exact->inexact real?)
	      (cons 'exact? number?)
	      (cons 'exp number?)
	      (cons 'expt (list number? number?))
	      (cons 'fill! (list sequence?))
	      (cons 'floor real?)
	      (cons 'gcd (list real? real?))
	      (cons 'gensym string?)
	      (cons 'hash-table-ref (list hash-table?))
	      (cons 'hash-table-set! (list hash-table?))
	      (cons 'hash-table-size hash-table?)
	      (cons 'hook procedure?)
	      (cons 'hook-arity hook?)
	      (cons 'hook-documentation hook?)
	      (cons 'hook-functions hook?)
	      (cons 'imag-part number?)
	      (cons 'inexact->exact real?)
	      (cons 'inexact? number?)
	      (cons 'infinite? number?)
	      (cons 'integer->char integer-between-0-and-255?)
	      (cons 'integer-decode-float real-but-not-rational?)
	      (cons 'integer-length integer?)
	      (cons 'keyword->symbol keyword?)
	      (cons 'lcm (list real? real?))
	      (cons 'length sequence?)
	      (cons 'list->string list?)
	      (cons 'list->vector list?)
	      (cons 'list-ref (list pair-or-null? non-negative-integer?))
	      (cons 'list-set! (list non-constant-list? non-negative-integer?))
	      (cons 'list-tail (list pair-or-null? non-negative-integer?))
	      (cons 'load string?)
	      (cons 'log (list number? non-zero-number?))
	      (cons 'logand (list integer? integer?))
	      (cons 'logior (list integer? integer?))
	      (cons 'lognot (list integer? integer?))
	      (cons 'logxor (list integer? integer?))
	      (cons 'magnitude number?)
	      (cons 'make-hash-table non-negative-integer?)
	      (cons 'make-hash-table-iterator hash-table?)
	      (cons 'make-hook (list list? string?))
	      (cons 'make-polar real?)
	      (cons 'make-rectangular real?)
	      (cons 'make-string (list non-negative-integer? char?))
	      (cons 'max real?)
	      (cons 'min real?)
	      (cons 'modulo (list real? real?))
	      (cons 'nan? number?)
	      (cons 'negative? real?)
	      (cons 'number->string (list number? integer-between-2-and-16?))
	      (cons 'numerator rational?)
	      (cons 'odd? integer?)
	      (cons 'open-input-file (list string? string?))
	      (cons 'open-input-string string?)
	      (cons 'open-output-file (list string? string?))
	      (cons 'positive? real?)
	      (cons 'provide symbol?)
	      (cons 'quotient (list real? real?))
	      (cons 'random number?)
	      (cons 'rationalize (list real? real?))
	      (cons 'real-part number?)
	      (cons 'remainder (list real? real?))
	      (cons 'reverse sequence?)
	      (cons 'reverse! sequence?)
	      (cons 'round real?)
	      (cons 'sin number?)
	      (cons 'sinh number?)
	      (cons 'sort! (list sequence? procedure?))
	      (cons 'sqrt number?)
	      (cons 'string char?)
	      (cons 'string->list string?)
	      (cons 'string->number (list string? integer-between-2-and-16?))
	      (cons 'string->symbol string?)
	      (cons 'string-append string?)
	      (cons 'string-ci<=? string?)
	      (cons 'string-ci<? string?)
	      (cons 'string-ci=? string?)
	      (cons 'string-ci>=? string?)
	      (cons 'string-ci>? string?)
	      (cons 'string-copy string?)
	      (cons 'string-fill! (list non-constant-string? char?))
	      (cons 'string-length string?)
	      (cons 'string-ref (list string? non-negative-integer?))
	      (cons 'string-set! (list non-constant-string? non-negative-integer? char?))
	      (cons 'string<=? string?)
	      (cons 'string<? string?)
	      (cons 'string=? string?)
	      (cons 'string>=? string?)
	      (cons 'string>? string?)
	      (cons 'substring (list string? non-negative-integer? non-negative-integer?))
	      (cons 'symbol->keyword symbol?)
	      (cons 'symbol->string symbol?)
	      (cons 'symbol->value symbol?)
	      (cons 'tan number?)
	      (cons 'tanh number?)
	      (cons 'truncate real?)
	      (cons 'vector->list vector?)
	      (cons 'vector-dimensions vector?)
	      (cons 'vector-fill! (list non-constant-vector?))
	      (cons 'vector-length vector?)
	      (cons 'vector-ref (list vector? non-negative-integer?))
	      (cons 'vector-set! (list non-constant-vector? non-negative-integer?))
	      (cons 'with-input-from-file (list string? thunk?))
	      (cons 'with-input-from-string (list string? thunk?))
	      (cons 'with-output-to-file (list string? thunk?))
	      (cons 'with-output-to-string thunk?)
	      (cons 'zero? number?))))

	(lambda (name line-number head form env)
	  (let ((fdata (env-member head env)))
	    (if (pair? fdata)
		;; a local var
		(let ()
		  (if (= (lint-length fdata) 4)
		      (let ((type (car (list-ref fdata 3)))
			    (args (cadr (list-ref fdata 3))))
			(let ((rst (or (not (pair? args))
				       (negative? (lint-length args))
				       (member ':rest args)))
			      (pargs (if (pair? args) (proper-list args) '())))
			  
			  (let ((call-args (lint-length (cdr form)))
				(decl-args (max 0 (- (lint-length pargs) (keywords pargs) (if rst 1 0)))))
			    (let ((req (if (eq? type 'define) decl-args 0))
				  (opt (if (eq? type 'define) 0 decl-args)))
			      (if (< call-args req)
				  (format #t "  ~A (line ~D): ~A needs ~D argument~A:~A~%" 
					  name line-number head 
					  req (if (> req 1) "s" "") 
					  (truncated-list->string form))
				  (if (and (not rst)
					   (> (- call-args (keywords (cdr form))) (+ req opt)))
				      (format #t "  ~A (line ~D): ~A has too many arguments:~A~%" 
					      name line-number head 
					      (truncated-list->string form))))
			      (if (eq? type 'define*)
				  (if (not (member ':allow-other-keys pargs))
				      (for-each
				       (lambda (arg)
					 (if (and (keyword? arg)
						  (not (member arg '(:rest :key :optional))))
					     (if (not (lint-member (keyword->symbol arg) pargs 
								   (lambda (a b)
								     (if (pair? b) 
									 (eq? a (car b))
									 (eq? a b)))))
						 (format #t "  ~A (line ~D): ~A keyword argument ~A (in ~S) does not match any argument in ~S~%"
							 name line-number head arg form pargs))))
				       (cdr form))))))))))
		;; not local var
		(if (and (symbol? head)
			 (defined? head)
			 (procedure? (symbol->value head)))
		    ;; check arg number

		    (let ((arity (procedure-arity (symbol->value head)))
			  (args (lint-length (cdr form))))

		      (if (pair? arity)
			  (if (not (procedure-with-setter? (symbol->value head))) ; set! case is confusing here
			      (if (< args (car arity))
				  (format #t "  ~A (line ~D): ~A needs ~A~D argument~A:~A~%" 
					  name line-number head 
					  (if (and (= 0 (cadr arity)) (not (caddr arity))) "" "at least ")
					  (car arity) 
					  (if (> (car arity) 1) "s" "") 
					  (truncated-list->string form))
				  (if (and (not (caddr arity))
					   (> (- args (keywords (cdr form))) (+ (car arity) (cadr arity))))
				      (format #t "  ~A (line ~D): ~A has too many arguments:~A~%" 
					      name line-number head 
					      (truncated-list->string form))))
			      
			      (let ((req (max (list-ref arity 0) (list-ref arity 3)))
				    (min-req (min (list-ref arity 0) (list-ref arity 3)))
				    (opt (max (list-ref arity 1) (list-ref arity 4)))
				    (rst (or (list-ref arity 2) (list-ref arity 5))))
				(if (< args min-req)
				    (format #t "  ~A (line ~D): ~A needs at least ~D argument~A:~A~%" 
					    name line-number head 
					    min-req (if (> min-req 1) "s" "") 
					    (truncated-list->string form))
				    (if (and (not rst)
					     (> (- args (keywords (cdr form))) (+ req opt)))
					(format #t "  ~A (line ~D): ~A has too many arguments:~A~%" 
						name line-number head 
						(truncated-list->string form)))))))
		      
		      (if (pair? (cdr form)) ; there are args
			  (begin
			    ;; if keywords, check that they are acceptable
			    ;;    this only applies to lambda*'s that have been previously loaded (lint doesn't create them)
			    (let ((source (procedure-source head)))
			      (if (and (pair? source)
				       (eq? (car source) 'lambda*))
				  (let ((decls (cadr source)))
				    (if (not (member ':allow-other-keys decls))
					(for-each
					 (lambda (arg)
					   (if (and (keyword? arg)
						    (not (member arg '(:rest :key :optional))))
					       (if (not (lint-member arg decls 
								     (lambda (a b) 
								       (if (pair? b) 
									   (eq? (keyword->symbol a) (car b))
									   (eq? (keyword->symbol a) b)))))
						   (format #t "  ~A (line ~D): ~A keyword argument ~A (in ~S) does not match any argument in ~S~%"
							   name line-number head arg form decls))))
					 (cdr form))))))
			    
			    (case head
			      ((eq?) 
			       (if (or (number? (cadr form))
				       (char? (cadr form))
				       (and (not (null? (cddr form)))
					    (or (number? (caddr form))
						(char? (caddr form)))))
				   (format #t "  ~A (line ~D): eq? doesn't work reliably with args like ~S~%" 
					   name line-number form))
			       (check-for-repeated-args name line-number head form env)
			       (check-for-repeated-args-with-not name line-number form env))
			      
			      ((eqv?) 
			       (if (or (vector? (cadr form))
				       (string? (cadr form))
				       (and (not (null? (cddr form)))
					    (or (vector? (caddr form))
						(string? (caddr form)))))
				   (format #t "  ~A (line ~D): eqv? doesn't work reliably with args like ~S~%" 
					   name line-number form))
			       (check-for-repeated-args name line-number head form env)
			       (check-for-repeated-args-with-not name line-number form env))
			      
			      ((map for-each)
			       (let* ((len (lint-length form))
				      (args (- len 2)))
				 (if (< len 3)
				     (format #t "  ~A (line ~D): ~A missing argument~A in:~A~%"
					     name line-number head 
					     (if (= len 2) "" "s") 
					     (truncated-list->string form)))
				 (let ((func (cadr form))
				       (arity #f))
				   (if (and (symbol? func)
					    (defined? func)
					    (procedure? (symbol->value func)))
				       (set! arity (procedure-arity (symbol->value func)))
				       
				       (if (and (pair? (cadr form))
						(member (caadr form) '(lambda lambda*))
						(pair? (cadr (cadr form))))
					   (let ((arglen (lint-length (cadr (cadr form)))))
					     (if (eq? (cadr form) 'lambda)
						 (if (negative? arglen)
						     (set! arity (list (abs arglen) 0 #t))
						     (set! arity (list arglen 0 #f)))
						 (if (negative? arglen)
						     (set! arity (list 0 (abs arglen) #t))
						     (set! arity (list 0 arglen (member ':rest (cadr (cadr form))))))))))
				   
				   (if (pair? arity)
				       (if (< args (car arity))
					   (format #t "  ~A (line ~D): ~A has too few arguments in: ~A~%"
						   name line-number head 
						   (truncated-list->string form))
					   (if (and (not (caddr arity))
						    (> args (+ (car arity) (cadr arity))))
					       (format #t "  ~A (line ~D): ~A has too many arguments in: ~A~%"
						       name line-number head 
						       (truncated-list->string form))))))))
			      
			      ((catch)
			       (if (and (not (symbol? (cadr form)))
					(not (boolean? (cadr form)))
					(or (not (pair? (cadr form)))
					    (not (eq? (caadr form) 'quote))))
				   (format #t "  ~A (line ~D): catch tag ~S is unreliable~%" 
					   name line-number
					   (cadr form))))
			      
			      (else
			       ;; we've already checked for head in env-member above
			       (check-for-repeated-args name line-number head form env)

			       ;; now try to check arg types for egregious errors
			       (let ((arg-data (hash-table-ref argument-data head)))
				 (if arg-data
				     (check-args name line-number head form arg-data env)
				     )))))))))))))
    

    (define (get-generator form) ; defgenerator funcs
      (let ((name (if (pair? (cadr form))
		      (car (cadr form))
		      (cadr form))))
	;; auto-define make-name, name?, name-field for each field (also set! case?)
	(let ((make-name (string->symbol (string-append "make-" (symbol->string name))))
	      (name? (string->symbol (string-append (symbol->string name) "?")))
	      (methods (string->symbol (string-append (symbol->string name) "-methods"))))

	  (hash-table-set! globals make-name (list make-name #f #f))
	  (hash-table-set! globals name? (list name? #f #f))
	  (hash-table-set! globals methods (list methods #f #f))

	  (for-each
	   (lambda (field)
	     (let ((fname (string->symbol 
			   (string-append 
			    (symbol->string name) "-" (symbol->string (if (pair? field)
									  (car field)
									  field))))))
	       (hash-table-set! globals fname (list fname #f #f (list 'lambda (list 'gen))))))
	   (cddr form)))))


    (define (load-walk form)
      ;; check form for top-level declarations, if load seen, and we haven't seen that file, load it
      (let ((head (car form)))
	(case head
	  ((begin)
	   (load-walk (cdr form)))

	  ((define-constant defvar define-envelope)
	   (hash-table-set! globals (cadr form) (list (cadr form) #f #f)))

	  ((defmacro defmacro*)
	   (hash-table-set! globals (cadr form) (list (cadr form) #f #f (list head (caddr form)))))

	  ((define define* definstrument define-expansion define-macro define-macro* define-bacro define-bacro*)
	   (if (pair? (cadr form))
	       (hash-table-set! globals (car (cadr form)) (list (car (cadr form)) #f #f (list head (cdr (cadr form)))))
	       (hash-table-set! globals (cadr form) (list (cadr form) #f #f))))

	  ((defgenerator)
	   (get-generator form))

	  ((if)
	   (if (pair? (cddr form))
	       (if (pair? (cdddr form))
		   (begin
		     (load-walk (cadddr form))
		     (load-walk (caddr form)))
		   (load-walk (caddr form)))))

	  ((load)
	   (if (>= (lint-length form) 2)
	       (scan form))))))


    (define (scan form)
      (let ((file (cadr form)))
	(if (and (string? file)
		 (not (member file loaded-files)))
	    (let ((fp (catch #t
			     (lambda ()
			       (open-input-file file))
			     (lambda args
			       (format #t "  can't load ~S: ~A~%" file (apply format #f (cadr args)))
			       #f))))
	      (if (input-port? fp)
		  (begin
		    (set! loaded-files (cons file loaded-files))
		    ;(format #t "  (scanning ~S)~%" file)
		    (do ((form (read fp) (read fp)))
			((eof-object? form))
		      (if (and (pair? form)
			       (pair? (cdr form)))
			  (load-walk form)))
		    (close-input-port fp)))))))


    (define (binding-ok? name line-number head binding env second-pass)
      ;; check let-style variable binding for various syntactic problems
      (if (not (pair? binding))
	  (begin 
	    (if (not second-pass)
		(format #t "  ~A (line ~D): ~A binding is not a list? ~S~%" 
			name line-number head binding))
	    #f)
	  (if (not (symbol? (car binding)))
	      (begin 
		(if (not second-pass)
		    (format #t "  ~A (line ~D): ~A variable is not a symbol? ~S~%" 
			    name line-number head binding))
		#f)
	      (if (keyword? (car binding))
		  (begin 
		    (if (not second-pass)
			(format #t "  ~A (line ~D): ~A variable is a keyword? ~S~%" 
				name line-number head binding))
		    #f)
		  (if (null? (cdr binding))
		      (begin 
			(if (not second-pass)
			    (format #t "  ~A (line ~D): ~A variable value is missing? ~S~%" 
				    name line-number head binding))
			#f)
		      (if (and (not (= (lint-length binding) 2))
			       (not (eq? head 'do)))
			  (begin
			    (if (not second-pass)
				(format #t "  ~A (line ~D): ~A binding is messed up: ~S~%" 
					name line-number head binding))
			    #f)
			  (begin
			    (if (and *report-shadowed-variables*
				     (not second-pass)
				     (env-member? (car binding) env))
				(format #t "  ~A (line ~D): ~A variable ~A in ~S shadows an earlier declaration~%" 
					name line-number head (car binding) binding))
			    #t)))))))


    (define (report-usage name line-number type head vars)
      ;; report unused or set-but-unreferenced variables
      (if (and (not (eq? head 'begin)) ; begin can redefine = set a variable
	       (not (null? vars)))
	  (do ((cur vars (cdr cur))
	       (rst (cdr vars) (cdr rst)))
	      ((null? rst))
	    (let ((repeat (lint-member (caar cur) rst (lambda (a b) (eq? a (car b))))))
	      ;; not env-member? here because the same name might be used as a global
	      (if repeat
		  (format #t "  ~A (line ~D): ~A ~A ~A is declared twice~%" name line-number head type (caar cur))))))

      (let ((set '())
	    (unused '()))
	(for-each 
	 (lambda (arg)
	   (if (member (car arg) '(quote if begin let let* letrec cond case or and do set! 
				   with-environment lambda lambda* define defvar define-envelope
				   define* defmacro defmacro* define-macro define-macro* 
				   define-bacro define-bacro* define-constant))
	       (format #t "  ~A (line ~D): ~A ~A named ~A is asking for trouble~%" name line-number head type (car arg))
	       (if (not (symbol? (car arg)))
		   (format #t "  ~A (line ~D): bad ~A ~A name: ~S~%" name line-number head type (car arg))))

	   (set! undefined-identifiers (remove (car arg) undefined-identifiers))

	   (if (not (cadr arg))
	       (if (caddr arg)
		   (set! set (cons (car arg) set))
		   (set! unused (cons (car arg) unused)))))
	 vars)

	(if (not (null? set))
	    (format #t "  ~A (line ~D): ~A ~A~A ~{~A~^, ~} set, but not used~%" 
		    name line-number head type (if (> (lint-length set) 1) "s" "") (reverse set)))
	(if (not (null? unused))
	    (format #t "  ~A (line ~D): ~A ~A~A ~{~A~^, ~} not used~%" 
		    name line-number head type (if (> (lint-length unused) 1) "s" "") (reverse unused)))))
    

    (define (lint-walk-body name line-number head body env)
      ;; walk a body (a list of forms, the value of the last of which might be returned)

      (if (or (not (list? body))
	      (negative? (lint-length body)))
	  (format #t "  ~A (line ~D): stray dot? ~A~%" 
		  name line-number (truncated-list->string body))

	  (let ((ctr 0)
		(len (lint-length body)))
	    (for-each
	     (lambda (f)
	       (if (< ctr (- len 1)) ; not the last form, so its value is ignored
		   (begin
		     (if (and (pair? f)
			      (eq? (car f) 'map))
			 (format #t "  ~A (line ~D): map could be for-each:~A~%" 
				 name line-number 
				 (truncated-list->string f)))

		     (if (not (side-effect? f env))
			 (format #t "  ~A (line ~D): this could be omitted:~A~%" 
				 name line-number 
				 (truncated-list->string f)))))

	       (if (and (pair? f)
			(member head '(defmacro defmacro* define-macro define-macro* define-bacro define-bacro*))
			(tree-member 'unquote f))
		   (format #t "  ~A (line ~D): ~A possibly has too many unquotes:~A~%"
			   name line-number head
			   (truncated-list->string f)))

	       (set! env (lint-walk name f env))
	       (set! ctr (+ ctr 1)))
	     body))))


    (define (lint-walk-function-body name line-number head args arg-data body env)
      ;; walk function body, with possible doc string at the start
      
      (if (and (pair? body)
	       (not (null? (cdr body)))
	       (string? (car body)))
	  (begin
	    (if *report-minor-stuff*
		(let* ((doc (car body))
		       (doclen (string-length doc))
		       (func (object->string name))
		       (funclen (if (string? func) (string-length func) -1)))
		  ;; check, then discard the doc string
		  ;;   look for the current function name and arg names.
		  
		  (if (and (> doclen funclen)
			   (char=? (string-ref doc 0) #\()
			   (string=? (substring doc 1 (+ funclen 1)) func))
		      (let ((p 1)
			    (end 0))
			(do ((i 1 (+ i 1)))
			    ((or (= p 0)
				 (= i doclen)))
			  (if (char=? (string-ref doc i) #\()
			      (set! p (+ p 1))
			      (if (char=? (string-ref doc i) #\))
				  (set! p (- p 1))))
			  (if (= p 0) 
			      (set! end i)))
			
			(if (not (zero? p))
			    (format #t "  ~A (line ~D): docstring is messed up: ~S~%" name line-number doc)
			    (if (< end doclen)
				(let* ((arglst (catch #t 
						      (lambda ()
							(eval-string (string-append "'" (substring doc 0 (+ 1 end)))))
						      (lambda ignore-catch-error-args 
							#f)))
				       (keys (if arglst (keywords arglst) 0))
				       (argn (if (or (pair? arglst) 
						     (null? arglst)) 
						 (- (lint-length (proper-list arglst)) keys 1) 
						 0)))
				  (if (and arglst
				       (not (= (lint-length arg-data) argn)))
				  (format #t "  ~A (line ~D): possible docstring mismatch:~%       ~S~%        ~S~%" 
					  name line-number (substring doc 0 (+ end 1)) (append (list name) args))))))))))

	    ;; in any case, skip the docstring during the walk
	    (set! body (cdr body))))

      (lint-walk-body name line-number head body env))


    (define (lint-walk-function head name args val line-number env)
      ;; check out function arguments (adding them to the current env), then walk its body, (name == function name, val == body)

      (if (null? args)
	  (begin
	    (lint-walk-function-body name line-number head args '() val env)
	    (append (list (list name #f #f (list head args))) env))

	  (if (or (symbol? args) 
		  (pair? args))
	      (let ((arg-data (if (symbol? args)                            ; this is getting arg names to add to the environment
				  (list (list args #f #f))
				  (lint-map
				   (lambda (arg)
				     (if (symbol? arg)
					 (if (member arg '(:optional :key :rest :allow-other-keys))
					     (lint-values)                  ; map omits this entry 
					     (list arg #f #f))
					 (if (or (not (pair? arg))
						 (not (= (lint-length arg) 2))
						 (not (member head '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
					     (begin
					       (format #t "  ~A (line ~D): strange parameter for ~A: ~S~%" 
						       name line-number head arg)
					       (lint-values))
					     (list (car arg) #f #f))))
				   (proper-list args)))))

		(lint-walk-function-body name line-number head args arg-data val (append arg-data env))
		(if *report-unused-parameters* 
		    (report-usage name line-number 'parameter head arg-data))
		(append (list (list name #f #f (list head args))) env))

	      (begin
		(format #t "  ~A (line ~D): strange ~A parameter list ~A~%" name line-number head args)
		env))))
    

    (define (lint-walk name form env)
       ;; walk a form 

      (if (symbol? form)
	  (begin
	    (ref-var form env)
	    env)
	  
	  (if (pair? form)
	      (let ((head (car form))
		    (line-number (pair-line-number form)))
		(case head

		  ;; ---------------- defmacro ----------------
		  ((defmacro defmacro*)
		   (if (or (< (lint-length form) 4)
			   (not (symbol? (cadr form))))
		       (format #t "  ~A (line ~D): ~A declaration is messed up: ~S~%"
			       name line-number head form)
		       (let ((sym (cadr form))
			     (args (caddr form))
			     (body (cdddr form)))
			 (if (and (pair? args)
				  (repeated-member? args env))
			     (format #t "  ~A (line ~D): ~A parameter is repeated:~A~%"
				     name line-number head 
				     (truncated-list->string args)))
			 (lint-walk-function head sym args body line-number env))))

		  ;; ---------------- define ----------------		  
		  ((define define* 
		     define-constant defvar define-envelope
		     define-expansion define-macro define-macro* define-bacro define-bacro*
		     definstrument)
		   
		   (if (< (lint-length form) 2)
		       (begin
			 (format #t "  ~A (line ~D): ~S makes no sense~%" name line-number form)
			 env)
		       (let ((sym (cadr form))
			     (val (cddr form)))
			 (if (symbol? sym)
			     (begin
			       (if (member head '(define define-constant defvar define-envelope))
				   (let ((len (lint-length form)))
				     (if (not (= len 3))
					 (format #t "  ~A (line ~D): ~S has ~A value~A?~%"
						 name line-number form 
						 (if (< len 3) "no" "too many") 
						 (if (< len 3) "" "s"))))
				   (format #t "  ~A (line ~D): ~S is messed up~%" name line-number form))
			       (if (not (null? (cddr form))) 
				   (lint-walk sym (caddr form) env))

			       (if (equal? sym val)
				   (format #t "  ~A (line ~D): this ~A is either not needed, or an error:~A~%" 
					   name line-number head 
					   (truncated-list->string form)))

			       (append (list (list sym #f #f)) env))

			     (if (pair? sym)
				 (begin
				   (if (and (pair? (cdr sym))
					    (repeated-member? (proper-list (cdr sym)) env))
				       (format #t "  ~A (line ~D): ~A parameter is repeated:~A~%"
					       name line-number head 
					       (truncated-list->string sym)))
				       
				   (lint-walk-function head (car sym) (cdr sym) val line-number env))
				 (begin
				   (format #t "  ~A (line ~D): strange form: ~S~%" head line-number form)
				   env))))))

		  ;; ---------------- defgenerator ----------------
		  ((defgenerator)
		   (get-generator form)
		   env)

		  ;; ---------------- lambda ----------------		  
		  ((lambda lambda*)
		   (if (< (lint-length form) 3)
		       (begin
			 (format #t "  ~A (line ~D): ~A is messed up in ~A~%"
				 name line-number head 
				 (truncated-list->string form))
			 env)
		       (begin
			 (if (and (pair? (cadr form))
				  (repeated-member? (proper-list (cadr form)) env))
			     (format #t "  ~A (line ~D): ~A parameter is repeated:~A~%"
				     name line-number head 
				     (truncated-list->string (cadr form))))
			 (lint-walk-function head name (cadr form) (cddr form) line-number env))))
		  ;; the lambda case includes stuff like call/cc

		  ;; ---------------- set! ----------------		  
		  ((set!)
		   (if (not (= (lint-length form) 3))
		       (begin
			 (format #t "  ~A (line ~D): set! has too ~A arguments: ~S~%" 
				 name line-number 
				 (if (> (lint-length form) 3) "many" "few") 
				 form)
			 env)
		       (let ((settee (cadr form))
			     (setval (caddr form)))
			 (if (pair? settee)
			     (begin
			       (lint-walk name settee env) ; this counts as a reference since it's by reference so to speak
			       (set! settee (do ((sym (car settee) (car sym)))
						((not (pair? sym)) sym)))))
			 (if (symbol? settee)
			     (set-var settee env)
			     (if (not (pair? settee))
				 (format #t "  ~A (line ~D): bad set? ~A~%" 
					 name line-number form)))
			 (if (equal? settee setval)
			     (format #t "  ~A (line ~D): pointless set!~A~%" 
				     name line-number 
				     (truncated-list->string form)))
			 (lint-walk name setval env))))

		  ;; ---------------- quote ----------------		  
		  ((quote) 
		   (let ((len (lint-length form)))
		     (if (negative? len)
			 (format #t "  ~A (line ~D): stray dot in quote's arguments? ~S~%"
				 name line-number form)
			 (if (not (= len 2))
			     (format #t "  ~A (line ~D): quote has too ~A arguments: ~S~%" 
				     name line-number 
				     (if (> (lint-length form) 2) "many" "few") 
				     form))))
		   env)

		  ;; ---------------- cond ----------------
		  ((cond)
		   (let ((ctr 0)
			 (len (- (lint-length form) 1)))
		     (if (negative? len)
			 (format #t "  ~A (line ~D): cond is messed up:~A~%" 
				 name line-number
				 (truncated-list->string form))
			 (for-each
			  (lambda (clause)
			    (set! ctr (+ ctr 1))
			    (if (not (pair? clause))
				(format #t "  ~A (line ~D): cond clause is messed up: ~A~%"
					name line-number
					(truncated-list->string clause))
				(begin
				  (if (boolean? (car clause))
				      (if (not (car clause))
					  (format #t "  ~A (line ~D): cond clause will never be evaluated:~A~%"
						  name line-number 
						  (truncated-list->string clause))
					  (if (not (= ctr len))
					      (format #t "  ~A (line ~D): cond #t clause is not the last: ~A~%"
						      name line-number 
						      (truncated-list->string form))))
				      (if (eq? (car clause) 'else)
					  (if (not (= ctr len))
					      (format #t "  ~A (line ~D): cond else clause is not the last: ~A~%"
						      name line-number 
						      (truncated-list->string form)))
					  (lint-walk name (car clause) env)))
				  (if (pair? (cdr clause))
				      (if (eq? (cadr clause) '=>)
					  (if (not (pair? (cddr clause)))
					      (format #t "  ~A (line ~D): cond => target is messed up: ~A~%"
						      name line-number
						      (truncated-list->string clause))
					      (lint-walk name (caddr clause) env))
					  (lint-walk-body name line-number head (cdr clause) env))
				      (if (not (null? (cdr clause)))
					  (format #t "  ~A (line ~D): cond clause is messed up: ~A~%"
						  name line-number
						  (truncated-list->string clause)))))))
			  (cdr form)))
		     env))
		  
		  
		  ;; ---------------- case ----------------		  
		  ((case)
		   ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		   (if (< (lint-length form) 3)
		       (format #t "  ~A (line ~D): case is messed up: ~A~%"
			       name line-number 
			       (truncated-list->string form))
		       (begin
			 (lint-walk name (cadr form) env) ; the selector
			 (let ((all-keys '())
			       (ctr 0)
			       (len (lint-length (cddr form))))
			   (for-each
			    (lambda (clause)
			      (set! ctr (+ ctr 1))
			      (if (not (pair? clause))
				  (format #t "  ~A (line ~D): case clause should be a list: ~A~%"
					  name line-number 
					  (truncated-list->string clause))
				  (let ((keys (car clause))
					(exprs (cdr clause)))
				    (if (pair? keys)
					(if (negative? (lint-length keys))
					    (format #t "  ~A (line ~D): stray dot in case case key list: ~A~%"
						    name line-number 
						    (truncated-list->string clause))
					    (for-each
					     (lambda (key)
					       (if (or (vector? key)
						       (string? key)
						       (list? key)
						       (hash-table? key)
						       (hook? key))
						   (format #t "  ~A (line ~D): case key ~S in ~S is unlikely to work (case uses eqv?)~%" 
							   name line-number key clause))
					       (if (member key all-keys)
						   (format #t "  ~A (line ~D): repeated case key ~S in ~S~%" 
							   name line-number key clause)))
					     keys))
					(if (not (eq? keys 'else))
					    (format #t "  ~A (line ~D): bad case key ~S in ~S~%" 
						    name line-number keys clause)
					    (if (not (= ctr len))
						(format #t "  ~A (line ~D): case else clause is not the last:~A~%"
							name line-number 
							(truncated-list->string (cddr form))))))
				    (set! all-keys (append (if (and (pair? keys)
								    (not (negative? (lint-length keys))))
							       keys 
							       (list keys))
							   all-keys))
				    (lint-walk-body name line-number head exprs env))))
			    (cddr form)))))
		   env)

		  ;; ---------------- do ----------------		  
		  ((do)
		   (let ((vars '()))
		     (if (or (< (lint-length form) 3)
			     (not (list? (cadr form)))
			     (not (list? (caddr form))))
			 (format #t "  ~A (line ~D): do is messed up: ~A~%" 
				 name line-number 
				 (truncated-list->string form))

			 (let ((step-vars (cadr form)))

			   ;; walk the init forms before adding the step vars to env
			   (do ((bindings step-vars (cdr bindings)))
			       ((null? bindings))
			     (if (binding-ok? name line-number head (car bindings) env #f)
				 (begin
				   (lint-walk name (cadar bindings) env)
				   (set! vars (append (list (list (caar bindings) #f #f)) vars)))))

			   ;; walk the step exprs
			   (do ((bindings step-vars (cdr bindings)))
			       ((null? bindings))
			     (if (and (binding-ok? name line-number head (car bindings) env #t)
				      (pair? (cddar bindings)))
				 (lint-walk name (caddar bindings) (append vars env))))

			   ;; walk the body and end stuff (it's too tricky to find infinite do loops)
			   (lint-walk-body name line-number head (cddr form) (append vars env))
			   (report-usage name line-number 'variable head vars)))
		     env))
		  
		  ;; ---------------- let ----------------		  
		  ((let)
		   (if (< (lint-length form) 3)
		       (format #t "  ~A (line ~D): let is messed up: ~A~%" 
			       name line-number 
			       (truncated-list->string form))
		       (let ((named-let (if (symbol? (cadr form)) (cadr form) #f)))
			 (let ((vars (if named-let (list (list named-let #f #f)) '())))
			   (do ((bindings (if named-let (caddr form) (cadr form)) (cdr bindings)))
			       ((null? bindings))
			     (if (binding-ok? name line-number head (car bindings) env #f)
				 (begin
				   (if (and (not (env-member? (caar bindings) env))
					    (pair? (cadar bindings))
					    (eq? 'lambda (car (cadar bindings)))
					    (tree-car-member (caar bindings) (cadar bindings)))
				       (format #t "  ~A (line ~D): let variable ~A is called in its binding?  perhaps let should be letrec:~A~%"
					       name line-number (caar bindings) 
					       (truncated-list->string bindings)))
				   (lint-walk name (cadar bindings) env)
				   (set! vars (append (list (list (caar bindings) #f #f)) vars)))))

			   ;; this, and others like it, should add internally defined stuff to vars
			   ;; currently (let () (define x 3) 4) does not report the unused variable x
			   (lint-walk-body name line-number head (if named-let (cdddr form) (cddr form)) (append vars env))
			   (report-usage name line-number 'variable head vars))))
		   env)
		  
		  ;; ---------------- let* ----------------		  
		  ((let*)
		   (if (< (lint-length form) 3)
		       (format #t "  ~A (line ~D): let* is messed up: ~A~%" 
			       name line-number 
			       (truncated-list->string form))
		       (let ((vars '()))
			 (do ((bindings (cadr form) (cdr bindings)))
			     ((null? bindings))
			   (if (binding-ok? name line-number head (car bindings) env #f)
			       (begin
				 (lint-walk name (cadar bindings) (append vars env))
				 (set! vars (append (list (list (caar bindings) #f #f)) vars)))))

			 (if (and *report-minor-stuff* ; maybe we need *report-very-minor-stuff* !
				  (call-with-exit
				   (lambda (return)
				     (for-each
				      (lambda (v)
					(if (list-ref v 1)
					    (return #f)))
				      vars)
				     #t)))
			     (format #t "  ~A (line ~D): let* could be let:~A~%" 
				     name line-number 
				     (truncated-list->string form)))

			 (lint-walk-body name line-number head (cddr form) (append vars env))
			 (report-usage name line-number 'variable head vars)))
		   env)
		  
		  ;; ---------------- letrec ----------------		  
		  ((letrec letrec*)
		   (if (< (lint-length form) 3)
		       (format #t "  ~A (line ~D): ~A is messed up: ~A~%" 
			       name line-number head
			       (truncated-list->string form))
		       (let ((vars '()))
			 (if (null? (cadr form))
			     (format #t "  ~A (line ~D): ~A could be let:~A~%"
				     name line-number head 
				     (truncated-list->string form)))
			 (do ((bindings (cadr form) (cdr bindings)))
			     ((null? bindings))
			   (if (binding-ok? name line-number head (car bindings) env #f)
			       (set! vars (append (list (list (caar bindings) #f #f)) vars))))
			 (let ((new-env (append vars env)))
			   (do ((bindings (cadr form) (cdr bindings)))
			       ((null? bindings))
			     (if (binding-ok? name line-number head (car bindings) env #t)
				 (lint-walk name (cadar bindings) new-env)))
			   (lint-walk-body name line-number head (cddr form) new-env))
			 (report-usage name line-number 'variable head vars)))
		   env)

		  ;; ---------------- begin ----------------
		  ((begin)
		   (if (negative? (lint-length form))
		       (begin
			 (format #t "  ~A (line ~D): stray dot in begin? ~A~%"
				 name line-number
				 (truncated-list->string form))
			 env)
		       (let* ((ctr 0)
			      (body (cdr form))
			      (len (lint-length body))
			      (vars env))
			 (for-each
			  (lambda (f)
			    (if (< ctr (- len 1))
				(if (and (pair? f)
					 (eq? (car f) 'map))
				    (format #t "  ~A (line ~D): map could be for-each:~A~%" 
					    name line-number 
					    (truncated-list->string f))
				    (if (not (side-effect? f env))
					(format #t "  ~A (line ~D): this could be omitted:~A~%"
						name line-number
						(truncated-list->string f)))))

			    (if (and (pair? f)
				     (eq? (car f) 'begin))
				(format #t "  ~A (line ~D): redundant begin:~A~%"
					name line-number
					(truncated-list->string form)))

			    (set! vars (lint-walk name f vars))
			    (set! ctr (+ ctr 1)))
			  body)
			 (if (not (eq? head 'begin))
			     (if (not (eq? vars env))
				 (let ((nvars '()))
				   (do ((v vars (cdr v)))
				       ((or (null? v)
					    (eq? v env)))
				     (set! nvars (cons (car v) nvars)))
				   (report-usage name line-number 'local-variable head nvars)))) ; this is not right, but it's better than nothing
		     vars)))
		  
		  ;; ---------------- format ----------------		  
		  ((format snd-display clm-print)
		   (if (< (lint-length form) 3)
		       (begin
			 (if (< (lint-length form) 2)
			     (format #t "  ~A (line ~D): ~A has too few arguments:~A~%"
				     name line-number head 
				     (truncated-list->string form)))
			 env)
		       (let ((control-string (if (string? (cadr form)) (cadr form) (caddr form)))
			     (args (if (string? (cadr form)) (cddr form) (cdddr form))))
			 
			 (define (count-directives str name line-number form)
			   (let ((curlys 0)
				 (len (string-length str))
				 (dirs 0)
				 (tilde-time #f))
			     (do ((i 0 (+ i 1)))
				 ((= i len))
			       (let ((c (string-ref str i)))
				 (if tilde-time
				     (begin
				       (if (= curlys 0)
					   (if (and (not (member c '(#\~ #\T #\t #\& #\% #\^ #\newline #\}))) ; ~* consumes an arg
						    (not (call-with-exit
							  (lambda (return)
							    (do ((k i (+ k 1)))
								((= k len) #f)
							      (if (and (not (char-numeric? (string-ref str k)))
								       (not (char=? (string-ref str k) #\,)))
								  (return (char-ci=? (string-ref str k) #\t))))))))
					       (let ((dir (string-ref str i)))
						 ;; the possibilities are endless, so I'll stick to the simplest
						 (if (and (not (char-numeric? dir))
							  (not (member dir '(#\A #\S #\C #\F #\E #\G #\O #\D #\B #\X #\, #\{ #\} #\@ #\P #\*
									     #\a #\s #\c #\f #\e #\g #\o #\d #\b #\x #\p))))
						     (format #t "  ~A (line ~D): unrecognized format directive: ~C in ~S, ~S~%"
							     name line-number dir str form))
						 (set! dirs (+ dirs 1)))))
				       (set! tilde-time #f)
				       (if (char=? c #\{)
					   (set! curlys (+ curlys 1))
					   (if (char=? c #\})
					       (set! curlys (- curlys 1)))))
				     (if (char=? c #\~)
					 (set! tilde-time #t)))))

			     (if tilde-time
				 (format #t "  ~A (line ~D): ~A control string ends in tilde:~A~%"
					 name line-number head
					 (truncated-list->string form)))

			     (if (not (= curlys 0))
				 (format #t "  ~A (line ~D): ~A has ~D unmatched ~A~A:~A~%"
					 name line-number head 
					 (abs curlys) 
					 (if (positive? curlys) "{" "}") 
					 (if (> curlys 1) "s" "") 
					 (truncated-list->string form)))
			     dirs))
			 
			 (if (not (string? control-string))
			     (if (not (list? args))
				 (format #t "  ~A (line ~D): ~S looks suspicious~%" 
					 name line-number form))
			     (let ((ndirs (count-directives control-string name line-number form))
				   (nargs (if (or (null? args) (pair? args)) (lint-length args) 0)))
			       (if (not (= ndirs nargs))
				   (format #t "  ~A (line ~D): ~A has ~A arguments:~A~%" 
					   name line-number head 
					   (if (> ndirs nargs) "too few" "too many")
					   (truncated-list->string form)))))
			 (lint-walk name (cdr form) env))))
		  
		  ;; ---------------- other schemes ----------------		  
		  ((define-syntax let-syntax letrec-syntax define-module re-export case-lambda) ; for other's code
		   env) 

		  ;; ---------------- declare ----------------		  
		  ((declare) ; for the run macro
		   (for-each
		    (lambda (decl)
		      (if (not (pair? decl))
			  (format #t "   ~A (line ~D): run declare statement is messed up: ~S~%" 
				  name line-number form)
			  (if (not (env-member? (car decl) env))
			      (format #t "  ~A (line ~D): run declare statement variable name ~A is unknown: ~S~%"
				      name line-number (car decl) form))))
		    (cdr form))
		   env)

		  ;; ---------------- everything else ----------------		  
		  (else  ; if and or with-environment

		   (if (negative? (lint-length form))
		       (format #t "  ~A (line ~D): stray dot? ~A~%" 
			       name line-number 
			       (truncated-list->string form))
		       (begin
			 (check-call name line-number head form env)
			 (check-special-cases name line-number head form env)

			 (if (and *report-minor-stuff*
				  (not (= line-number last-simplify-numeric-line-number))
				  (not (env-member? head env))
				  (numeric? head))
			     (let ((val (simplify-numerics form env)))
			       (set! last-simplify-numeric-line-number line-number)
			       (if (not (equal? form val))
				   (format #t "  ~A (line ~D): possible simplification:~A~%"
					   name line-number 
					   (lists->string form val)))))

			 ;; walk everything looking for undefined vars (saved until we finish the file)
			 (let ((vars env))
			   (for-each
			    (lambda (f)
			      ;; look for names we don't know about
			      (if (and (symbol? f)
				       (not (keyword? f))
				       (not (eq? f name))
				       (not (eq? f '=>))
				       (not (defined? f))
				       (not (env-member? f vars)))
				  (if (not (lint-member f undefined-identifiers 
							(lambda (a b) 
							  (eq? a (car b)))))
				      (set! undefined-identifiers (cons (list f name line-number 
									      (truncated-list->string form)) 
									undefined-identifiers))))
			      (set! vars (lint-walk name f vars)))
			    form))))
		   env)))
	      
	      ;; else form is a constant or something
	      env)))
    
    ;;; --------------------------------------------------------------------------------
    
    (lambda (file . args)
      "(lint file) looks for infelicities in file's scheme code"
      (set! undefined-identifiers '())
      (set! globals (make-hash-table))
      (set! loaded-files '())
      
      (if *load-file-first* ; this can improve the error checks
	  (load file))
      (let ((fp (catch #t
		       (lambda ()
			 (open-input-file file))
		       (lambda args
			 (format #t "  can't open ~S: ~A~%" file (apply format #f (cadr args)))
			 #f))))

	(if (input-port? fp)
	    (let ((vars '())
		  (line 0)
		  (last-form #f)
		  (last-line-number -1))
	      (format #t ";~A~%" file)
	      (set! loaded-files (cons file loaded-files))

	      (if (not (null? args))
		  (for-each
		   (lambda (f)
		     (hash-table-set! no-side-effect-functions f #t))
		   (car args)))

	      (do ((form (read fp) (read fp)))
		  ((eof-object? form))
		(if (pair? form)
		    (set! line (max line (pair-line-number form))))

		(if (and (not (= last-line-number -1))
			 (not (side-effect? last-form vars)))
		    (format #t "  top-level (line ~D): this has no effect:~A~%" 
			    last-line-number
			    (truncated-list->string last-form)))
		(set! last-form form)
		(set! last-line-number line)

		(set! vars (lint-walk (if (symbol? form) 
					  form 
					  (if (pair? form) 
					      (car form)
					      #f))
				      form 
				      vars)))

	      (if *report-unused-top-level-functions* 
		  (report-usage file 0 'top-level-function #f vars))

	      (if *report-undefined-variables*
		  (for-each
		   (lambda (var)
		     (if (not (env-member? (car var) vars))
			 (format #t "  ~A (line ~D): undefined identifier ~A in:~A~%"
				 (list-ref var 1)
				 (list-ref var 2)
				 (list-ref var 0)
				 (list-ref var 3))))
		   undefined-identifiers))

	      (close-input-port fp)))))))


