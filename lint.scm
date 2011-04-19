;;; lint for s7 scheme

(provide 'lint.scm)

(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #f)
(define *report-undefined-variables* #f)
(define *report-shadowed-variables* #f)
(define *report-minor-stuff* #f)          ; let* -> let, if expr #f#t cases, docstring checks, (= 1.0 x), numerical redundancies

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


(if (not (provided? 's7))
    (begin
      ;; here's an attempt to make this work in Guile 1.8
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
      )
    (begin
      (define lint-eval eval)
      (define lint-values values)
      (define lint-map map)
      (define lint-length length)
      (define lint-member member)))

	

;;; --------------------------------------------------------------------------------
;;; function data

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
		     (cons 'with-input-from-file (list string? thunk?))
		     (cons 'with-input-from-string (list string? thunk?))
		     (cons 'with-output-to-file (list string? thunk?))
		     (cons 'with-output-to-string thunk?)
		     (cons 'provide symbol?)
		     
		     (cons 'even? integer?)
		     (cons 'odd? integer?)
		     (cons 'integer-length integer?)
		     (cons 'logior (list integer? integer?))
		     (cons 'logxor (list integer? integer?))
		     (cons 'logand (list integer? integer?))
		     (cons 'lognot (list integer? integer?))
		     (cons 'ash (list integer? integer?))
		     (cons 'integer-decode-float real-but-not-rational?)
		     (cons 'integer->char integer-between-0-and-255?)
		     
		     (cons 'numerator rational?)
		     (cons 'denominator rational?)
		     
		     (cons 'make-polar real?)
		     (cons 'make-rectangular real?)
		     (cons 'magnitude number?)
		     (cons 'angle number?)
		     (cons 'real-part number?)
		     (cons 'imag-part number?)
		     (cons 'rationalize (list real? real?))
		     (cons 'abs number?)
		     (cons 'exp number?)
		     (cons 'log (list number? non-zero-number?))
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
		     (cons 'floor real?)
		     (cons 'ceiling real?)
		     (cons 'truncate real?)
		     (cons 'round real?)
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
		     (cons 'positive? real?)
		     (cons 'negative? real?)
		     (cons 'infinite? number?)
		     (cons 'nan? number?)
		     (cons 'inexact->exact real?)
		     (cons 'exact->inexact real?)
		     (cons 'random number?)
		     (cons 'exact? number?)
		     (cons 'inexact? number?)
		     
		     (cons 'number->string (list number? integer-between-2-and-16?))
		     (cons 'string->number (list string? integer-between-2-and-16?))
		     
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
		     
		     (cons 'make-string (list non-negative-integer? char?))
		     (cons 'string char?)
		     
		     (cons 'string-length string?)
		     (cons 'string-ref (list string? non-negative-integer?))
		     (cons 'string-set! (list non-constant-string? non-negative-integer? char?))
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
		     (cons 'string-fill! (list non-constant-string? char?))
		     (cons 'string-copy string?)
		     (cons 'substring (list string? non-negative-integer? non-negative-integer?))
		     (cons 'string->list string?)
		     
		     (cons 'list->string list?)
		     (cons 'list->vector list?)
		     (cons 'car pair?)
		     (cons 'cdr pair?)
		     (cons 'caar pair?)
		     (cons 'cadr pair?)
		     (cons 'cdar pair?)
		     (cons 'cddr pair?)
		     (cons 'caaar pair?)
		     (cons 'caadr pair?)
		     (cons 'cadar pair?)
		     (cons 'cdaar pair?)
		     (cons 'caddr pair?)
		     (cons 'cdddr pair?)
		     (cons 'cdadr pair?)
		     (cons 'cddar pair?)
		     (cons 'caaaar pair?)
		     (cons 'caaadr pair?)
		     (cons 'caadar pair?)
		     (cons 'cadaar pair?)
		     (cons 'caaddr pair?)
		     (cons 'cadddr pair?)
		     (cons 'cadadr pair?)
		     (cons 'caddar pair?)
		     (cons 'cdaaar pair?)
		     (cons 'cdaadr pair?)
		     (cons 'cdadar pair?)
		     (cons 'cddaar pair?)
		     (cons 'cdaddr pair?)
		     (cons 'cddddr pair?)
		     (cons 'cddadr pair?)
		     (cons 'cdddar pair?)
		     (cons 'list-ref (list pair-or-null? non-negative-integer?))
		     (cons 'list-set! (list non-constant-list? non-negative-integer?))
		     (cons 'list-tail (list pair-or-null? non-negative-integer?))
		     
		     (cons 'vector->list vector?)
		     (cons 'vector-fill! (list non-constant-vector?))
		     (cons 'vector-length vector?)
		     (cons 'vector-ref (list vector? non-negative-integer?))
		     (cons 'vector-set! (list non-constant-vector? non-negative-integer?))
		     (cons 'vector-dimensions vector?)
		     
		     (cons 'make-hash-table non-negative-integer?)
		     (cons 'hash-table-ref (list hash-table?))
		     (cons 'hash-table-set! (list hash-table?))
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

		     (cons 'length sequence?)
		     (cons 'reverse sequence?)
		     (cons 'reverse! sequence?)
		     (cons 'fill! (list sequence?))
		     (cons 'sort! (list sequence? procedure?))
		     
		     (cons 'load string?)
		     (cons 'eval-string string?)
		     (cons 'dynamic-wind (list thunk? thunk? thunk?))))
	
	(no-side-effect-functions (list 'not 'and 'or 'let* 'let 'letrec 'letrec* 'lambda 'do 'case 'catch 'cond 'begin 'if 'quote
					'= '+ 'cdr 'real? 'rational? 'number? '> '- 'integer? 'catch 
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
				    (cons 'make-polar 2) ; if it can be an int, use 2, else 1
				    (cons 'make-rectangular 2)
				    (cons 'magnitude 2)
				    (cons 'angle 2)
				    (cons 'real-part 2)
				    (cons 'imag-part 2)
				    (cons 'numerator 2)
				    (cons 'denominator 2)
				    (cons 'rationalize 2)
				    (cons 'abs 2)
				    (cons 'exp 2)
				    (cons 'log 2)
				    (cons 'sin 2)
				    (cons 'cos 2)
				    (cons 'tan 2)
				    (cons 'asin 1)
				    (cons 'acos 1)
				    (cons 'atan 1)
				    (cons 'sinh 1)
				    (cons 'cosh 1)
				    (cons 'tanh 1)
				    (cons 'asinh 1)
				    (cons 'acosh 1)
				    (cons 'atanh 1)
				    (cons 'sqrt 2)
				    (cons 'expt 2)
				    (cons 'floor 2)
				    (cons 'ceiling 2)
				    (cons 'truncate 2)
				    (cons 'round 2)
				    (cons 'lcm 2)
				    (cons 'gcd 2)
				    (cons '+ 2)
				    (cons '- 2)
				    (cons '* 2)
				    (cons '/ 2)
				    (cons 'max 2)
				    (cons 'min 2)
				    (cons 'quotient 2)
				    (cons 'remainder 2)
				    (cons 'modulo 2)
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
				    (cons 'inexact->exact 2)
				    (cons 'exact->inexact 1)
				    (cons 'integer-length 2)
				    (cons 'logior 2)
				    (cons 'logxor 2)
				    (cons 'logand 2)
				    (cons 'lognot 2)
				    (cons 'ash 2)
				    (cons 'random-state->list '(1))
				    (cons 'integer-decode-float '(1))
				    (cons 'exact? #f)
				    (cons 'inexact? #f)
				    (cons 'number? #f)
				    (cons 'integer? #f)
				    (cons 'real? #f)
				    (cons 'complex? #f)
				    (cons 'rational? #f)
				    (cons 'number->string "")
				    (cons 'string->number 2) ; or #f -- can this cause confusion?
				    (cons 'char-upcase #\a)
				    (cons 'char-downcase #\a)
				    (cons 'char->integer 2)
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
				    (cons 'string-length 2)
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
				    (cons 'string->list '(1))
				    (cons 'object->string "")
				    (cons 'null? #f)
				    (cons 'list? #f)
				    (cons 'pair? #f)
				    (cons 'cons '(1))
				    (cons 'list '(1))
				    (cons 'make-list '(1))
				    (cons 'length 2)
				    (cons 'vector? #f)
				    (cons 'vector->list '(1))
				    (cons 'list->vector #())
				    (cons 'vector #())
				    (cons 'vector-length 2)
				    (cons 'make-vector #())
				    (cons 'continuation? #f)
				    (cons 'map '(1))
				    (cons 'procedure? #f)
				    (cons 'not #f)
				    (cons 'boolean? #f)
				    (cons 'eq? #f)
				    (cons 'eqv? #f)
				    (cons 'equal? #f)))

	;; (list value value-check identity-if-one-arg inverse-op redundant-op)
	(numerical-ops (hash-table (cons '+ (list 1+i #f #t #f '+))
				   (cons '* (list 1+i #f #t #f '*))
				   (cons 'imag-part (list 1.0 (lambda (val) (and (real? val) 0.0)) #f #f #f))
				   (cons 'real-part (list 1.0 #f #f #f #f))
				   (cons 'magnitude (list 1.0 #f #f #f 'magnitude))
				   (cons 'angle (list 1.0 #f #f #f #f))
				   (cons 'numerator (list 1 integer? #f #f 'numerator))
				   (cons 'denominator (list 1 (lambda (val) (and (integer? val) 1)) #f #f #f))
				   (cons 'abs (list 1.0 #f #f #f 'abs))
				   (cons 'sin (list 1+i #f #f 'asin #f))
				   (cons 'cos (list 1+i #f #f 'acos #f))
				   (cons 'asin (list 1+i #f #f 'sin #f))
				   (cons 'acos (list 1+i #f #f 'cos #f))
				   (cons 'sinh (list 1+i #f #f 'asinh #f))
				   (cons 'cosh (list 1+i #f #f 'acosh #f))
				   (cons 'tanh (list 1+i #f #f 'atanh #f))
				   (cons 'asinh (list 1+i #f #f 'sinh #f))
				   (cons 'acosh (list 1+i #f #f 'cosh #f))
				   (cons 'atanh (list 1+i #f #f 'tanh #f))
				   (cons 'floor (list 1 integer? #f #f 'floor))
				   (cons 'ceiling (list 1 integer? #f #f 'ceiling))
				   (cons 'truncate (list 1 integer? #f #f 'truncate))
				   (cons 'round (list 1 integer? #f #f 'round))
				   (cons 'max (list 1.0 #f #t #f 'max))
				   (cons 'min (list 1.0 #f #t #f 'min))
				   (cons 'inexact->exact (list 1 #f #f #f 'inexact->exact))
				   (cons 'exact->inexact (list 1.0 #f #f #f 'exact->inexact))
				   (cons 'logior (list 1 #f #t #f 'logior))
				   (cons 'logxor (list 1 #f #t #f #f))
				   (cons 'logand (list 1 #f #t #f 'logand))
				   (cons 'lognot (list 1 #f #f #f #f))
				   (cons 'ash (list 1 #f #f #f #f))
				   ))
					 
	(loaded-files #f)
	(globals #f)
	(undefined-identifiers #f)
	)
    
    ;;; --------------------------------------------------------------------------------

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
	  (or (and (not (member (car form) no-side-effect-functions)) ; if func is not in that list, make no assumptions about it
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
	       (not (member form no-side-effect-functions))
	       (not (env-member? form env)))))


    (define (just-constants? form)
      (define (lint-constant? arg)
	(or (number? arg)
	    (string? arg)
	    (null? arg)
	    (char? arg)))
      (or (lint-constant? form)
	  (and (pair? form)
	       (or (member (car form) no-side-effect-functions)
		   (lint-constant? (car form)))
	       (just-constants? (cdr form)))))


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


    (define (check-for-repeated-args-with-not name line-number head form env)

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


    (define (check-args name line-number head form checkers)
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
		      (if (or (and op
				   (not (checker op)))
			      (and (just-constants? arg)
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
				      (truncated-list->string form)))))

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

      (define (classify e)
	;; do we already know that e is true or false?
	(if (member e true)
	    #t ; the simple boolean is passed back which will either be dropped or will stop the outer expr build
	    (if (member e false)
		#f
		(if (just-constants? e)
		    (catch #t 
			   (lambda ()
			     (if lint-eval
				 (if (lint-eval arg) #t #f) ; we want the actual booleans here
				 e))
			   (lambda ignore-catch-error-args
			     e))
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
				      (not (boolean? (hash-table-ref function-types (car arg))))))
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
				 (if (eq? val #t)                   ; #t in or ends the expression
				     (begin
				       (if (or (null? new-form)
					       (just-symbols? new-form))
					   (set! new-form '(#t))
					   (set! new-form (append '(#t) new-form))) ; reversed when returned
				       (set! exprs '(#t)))
				     
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
		       (let ((new-form '()))
			 (do ((exprs (cdr form) (cdr exprs)))
			     ((null? exprs) 
			      (if (null? new-form)
				  #t
				  (if (null? (cdr new-form))
				      (car new-form)
				      `(and ,@(reverse new-form)))))
			   (let* ((e (car exprs))
				  (val (classify e)))

			     (if (and (pair? val)
				      (member (car val) '(and or not)))
				 (set! val (classify (simplify-boolean e true false env))))

			     (if (not (eq? val #t))           ; #t in and is ignored
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

    (define (get-generator form)
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
      ;; check form form top-level declarations, if load seen, and we haven't seen that file, load it
      (let ((head (car form)))
	(case head
	  ((begin)
	   (load-walk (cdr form)))

	  ((define-constant defvar)
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
	   (scan form)))))


    (define (scan form)
      (let ((file (cadr form)))
	(if (and (string? file)
		 (not (member file loaded-files)))
	    (let ((fp (catch #t
			     (lambda ()
			       (open-input-file file))
			     (lambda args
			       (format #t "  can't load ~S: ~A~%" file (apply format #f (cdr args)))
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
      ;; report unused or set-but-unreferenced variable
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
				   with-environment lambda lambda* define defvar
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

	       (set! env (lint-walk name line-number f env))
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
				       (argn (if (list? arglst) (- (lint-length (proper-list arglst)) keys 1) 0)))
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
    

    (define (lint-walk name line form env) ; line currently not used
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
		     define-constant defvar
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
			       (if (member head '(define define-constant defvar))
				   (let ((len (lint-length form)))
				     (if (not (= len 3))
					 (format #t "  ~A (line ~D): ~S has ~A value~A?~%"
						 name line-number form 
						 (if (< len 3) "no" "too many") 
						 (if (< len 3) "" "s"))))
				   (format #t "  ~A (line ~D): ~S is messed up~%" name line-number form))
			       (if (not (null? (cddr form))) 
				   (lint-walk sym line-number (caddr form) env))

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
			       (lint-walk name line-number settee env) ; this counts as a reference since it's by reference so to speak
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
			 (lint-walk name line-number setval env))))

		  ;; TODO: apply proc args -- here if we know proc we can check the args (len/type)
		  ;;       also the type for other arg checks can be got from proc (rather than car)
		  
		  ;; also can eval be checked?
		  ;; also can we simplify complicated boolean exprs?  numerical?


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
					  (lint-walk name line-number (car clause) env)))
				  (if (pair? (cdr clause))
				      (if (eq? (cadr clause) '=>)
					  (if (not (pair? (cddr clause)))
					      (format #t "  ~A (line ~D): cond => target is messed up: ~A~%"
						      name line-number
						      (truncated-list->string clause))
					      (lint-walk name line-number (caddr clause) env))
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
			 (lint-walk name line-number (cadr form) env) ; the selector
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
				   (lint-walk name line-number (cadar bindings) env)
				   (set! vars (append (list (list (caar bindings) #f #f)) vars)))))

			   ;; walk the step exprs
			   (do ((bindings step-vars (cdr bindings)))
			       ((null? bindings))
			     (if (and (binding-ok? name line-number head (car bindings) env #t)
				      (pair? (cddar bindings)))
				 (lint-walk name line-number (caddar bindings) (append vars env))))

			   ;; walk the body and end stuff (it's too tricky to find infinite do loops)
			   (lint-walk-body name line-number head (cddr form) (append vars env))
			   (report-usage name line-number 'variable head vars)))
		     env))
		  
		  ;; ---------------- let ----------------		  
		  ((let)
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
			       (lint-walk name line-number (cadar bindings) env)
			       (set! vars (append (list (list (caar bindings) #f #f)) vars)))))
		       (lint-walk-body name line-number head (if named-let (cdddr form) (cddr form)) (append vars env))
		       (report-usage name line-number 'variable head vars)
		       env)))
		  
		  ;; ---------------- let* ----------------		  
		  ((let*)
		   (let ((vars '()))
		     (do ((bindings (cadr form) (cdr bindings)))
			 ((null? bindings))
		       (if (binding-ok? name line-number head (car bindings) env #f)
			   (begin
			     (lint-walk name line-number (cadar bindings) (append vars env))
			     (set! vars (append (list (list (caar bindings) #f #f)) vars)))))
		     (if (and *report-minor-stuff*
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
		     (report-usage name line-number 'variable head vars)
		     env))
		  
		  ;; ---------------- letrec ----------------		  
		  ((letrec letrec*)
		   (let ((vars '()))
		     (do ((bindings (cadr form) (cdr bindings)))
			 ((null? bindings))
		       (if (binding-ok? name line-number head (car bindings) env #f)
			   (set! vars (append (list (list (caar bindings) #f #f)) vars))))
		     (let ((new-env (append vars env)))
		       (do ((bindings (cadr form) (cdr bindings)))
			   ((null? bindings))
			 (if (binding-ok? name line-number head (car bindings) env #t)
			     (lint-walk name line-number (cadar bindings) new-env)))
		       (lint-walk-body name line-number head (cddr form) new-env))
		     (report-usage name line-number 'variable head vars)
		     env))

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

			    (set! vars (lint-walk name line-number f vars))
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
			 (lint-walk name line-number (cdr form) env))))
		  
		  ;; ---------------- other schemes ----------------		  
		  ((define-syntax let-syntax letrec-syntax define-module re-export) ; for other's code
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
		   ;; we can't expand macros so free variables can confuse the usage checks
		   ;; this block returns the current env at the end

		   (if (negative? (lint-length form))
		       (format #t "  ~A (line ~D): stray dot? ~A~%" 
			       name line-number 
			       (truncated-list->string form))
		       (begin

			 (let ((fdata (env-member head env)))
			   (if (pair? fdata)
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
						      (cdr form)))))))))) ; end if local-value
			       
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
					      (check-for-repeated-args-with-not name line-number head form env))
					     
					     ((eqv?) 
					      (if (or (vector? (cadr form))
						      (string? (cadr form))
						      (and (not (null? (cddr form)))
							   (or (vector? (caddr form))
							       (string? (caddr form)))))
						  (format #t "  ~A (line ~D): eqv? doesn't work reliably with args like ~S~%" 
							  name line-number form))
					      (check-for-repeated-args name line-number head form env)
					      (check-for-repeated-args-with-not name line-number head form env))
					     
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
						    (check-args name line-number head form arg-data)
						    ))))))))))
			 
			 (if *report-minor-stuff*
			     (begin
			       (if (and (> (lint-length form) 2)
					(member head '(equal? =))
					(any-real? (cdr form)))
				   (format #t "  ~A (line ~D): ~A can be troublesome with floats:~A~%"
					   name line-number head 
					   (truncated-list->string form))
				   
				   (case head
				     ((load) ; pick up the top level declarations
				      (scan form)
				      env)
				     
				     ;; TODO: add require for slib
				     ;;   (also how does Rick get things loaded?)
				     
				     ((if)
				      (let ((len (lint-length form)))
					(if (> len 4)
					    (format #t "  ~A (line ~D): if has too many clauses: ~S~%" 
						    name line-number form)
					    (if (< len 3)
						(format #t "  ~A (line ~D): if has too few clauses: ~S~%" 
							name line-number form)
						
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

						  ;; PERHAPS: unravel complicated ifs
						  ;; also dependencies that are checked twice

						  (if (and (= len 4)
							   (equal? (caddr form) (cadddr form)))
						      (format #t "  ~A (line ~D): if is not needed here:~A~%"
							      name line-number 
							      (truncated-list->string form))))))))
				     
				     ((logior)
				      (if (member -1 (cdr form))
					  (format #t "  ~A (line ~D): this is always -1:~A~%"
						  name line-number 
						  (truncated-list->string form))))
				     
				     ((logand lcm)
				      (if (member 0 (cdr form))
					  (format #t "  ~A (line ~D): this is always 0:~A~%"
						  name line-number 
						  (truncated-list->string form))))
				     
				     ((ash)
				      (if (and (= (lint-length form) 3)
					       (not (equal? (cadr form) 1)) ; ignore (ash 1 0)
					       (equal? (caddr form) 0))
					  (format #t "  ~A (line ~D): this is always ~S:~A~%"
						  name line-number (cadr form) 
						  (truncated-list->string form))))
				     
				     ((/)
				      (let ((len (lint-length form)))
					(if (or (and (= len 2)
						     (member (cadr form) '(0 0.0)))
						(and (> len 2)
						     (or (member 0 (cddr form))
							 (member 0.0 (cddr form)))))
					    (format #t "  ~A (line ~D): possible divide by zero:~A~%"
						    name line-number 
						    (truncated-list->string form)))))
				     
				     ((gcd)
				      (if (member 1 (cdr form))
					  (format #t "  ~A (line ~D): this is always 1:~A~%"
						  name line-number 
						  (truncated-list->string form))))
				     
				     ((car cdr)
				      (if (and (pair? (cadr form))
					       (eq? (car (cadr form)) 'cons))
					  (format #t "  ~A (line ~D): (~A~A) is the same as~A~%"
						  name line-number head
						  (truncated-list->string (cadr form))
						  (if (eq? head 'car)
						      (truncated-list->string (cadr (cadr form)))
						      (truncated-list->string (caddr (cadr form)))))))
				     
				     ((and or not)
				      (let ((val (simplify-boolean form '() '() env)))
					(if (not (equal? form val))
					    (format #t "  ~A (line ~D): possible simplification:~A~%"
						    name line-number 
						    (lists->string form val)))))

				     ((sort!)
				      (if (member (caddr form) '(= <= >= eq? eqv? equal?
								   string=? string<=? string>=? char=? char<=? char>=?
								   string-ci=? string-ci<=? string-ci>=? char-ci=? char-ci<=? char-ci>=?))
					  (format #t "  ~A (line ~D): sort! with ~A may hang:~A~%"
						  name line-number head 
						  (truncated-list->string form))))
				     ))
		       
			       (if (not (env-member? head env))
				   (let ((num-info (hash-table-ref numerical-ops head)))
				     (if num-info
					 (let ((len (lint-length form))
					       (checker (list-ref num-info 1))
					       (id-if-one (list-ref num-info 2))
					       (inverse (list-ref num-info 3))
					       (redundant (list-ref num-info 4)))
					   (if (= len 2)
					       (if id-if-one
						   (format #t "  ~A (line ~D): ~A is not needed in~A~%"
							   name line-number head 
							   (truncated-list->string form))
						   (if (pair? (cadr form))
						       (let ((in-op (caadr form)))
							 (if (and inverse
								  (eq? in-op inverse))
							     (format #t "  ~A (line ~D):~A is the same as~A~%"
								     name line-number
								     (truncated-list->string form)
								     (truncated-list->string (cadr (cadr form))))
							     (if (and redundant
								      (eq? redundant in-op))
								 (format #t "  ~A (line ~D): redundant ~A in~A~%"
									 name line-number redundant
									 (truncated-list->string form))
								 (if checker
								     (let ((call-info (hash-table-ref numerical-ops in-op)))
								       (if call-info
									   (let ((val (checker (car call-info))))
									     (if (number? val)
										 (format #t "  ~A (line ~D): ~A is always ~A~%"
											 name line-number
											 (truncated-list->string form)
											 val)
										 (if val
										     (format #t "  ~A (line ~D): ~A is redundant in~A~%"
											     name line-number head
											     (truncated-list->string form)
											     ))))))))))))
					       (if (and redundant
							(caar-member redundant (cdr form)))
						   (format #t "  ~A (line ~D): embedded ~A is redundant in~A~%"
							   name line-number redundant 
							   (truncated-list->string form))))))))))

			 (let ((vars env))
			   (for-each
			    (lambda (f)
			      ;; look for names we don't know about
			      (if (and (symbol? f)
				       (not (keyword? f))
				       (not (member name '(defgenerator define-record))) ; TODO: omit? or add support for define-record? sound-let define-envelope
				       (not (eq? f name))
				       (not (member (car form) '(apply map for-each))) ; TODO: omit after fixing apply?
				       (not (eq? f '=>))
				       (not (defined? f))
				       (not (env-member? f vars)))
				  (if (not (lint-member f undefined-identifiers (lambda (a b) (eq? a (car b)))))
				      (set! undefined-identifiers (cons (list f name line-number (truncated-list->string form)) undefined-identifiers))))
			      (set! vars (lint-walk name line-number f vars)))
			    form))))
		   env)))
	      
	      ;; else form is a constant or something
	      env)))
    
    ;;; --------------------------------------------------------------------------------
    
    (lambda (file)
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
			 (format #t "  can't open ~S: ~A~%" file (apply format #f (cdr args)))
			 #f))))

	(if (input-port? fp)
	    (let ((vars '())
		  (line 0))
	      (format #t ";~A~%" file)
	      (set! loaded-files (cons file loaded-files))
	      (do ((form (read fp) (read fp)))
		  ((eof-object? form))
		(if (pair? form)
		    (set! line (max line (pair-line-number form))))
		(set! vars (lint-walk (if (symbol? form) 
					  form 
					  (if (pair? form) 
					      (car form)
					      #f))
				      line
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

