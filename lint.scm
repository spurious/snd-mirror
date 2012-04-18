;;; lint for s7 scheme

(provide 'lint.scm)

(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #f)
(define *report-multiply-defined-top-level-functions* #f) ; same name defined at top level in more than one file
(define *report-undefined-variables* #f)
(define *report-shadowed-variables* #f)
(define *report-minor-stuff* #t)          ; let*, docstring checks, (= 1.5 x), numerical and boolean simplification

(define *load-file-first* #f)
(define start-up-environment (global-environment))
(define *current-file* "")
(define *top-level-objects* (make-hash-table))


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


(define lint
  (let ()
    (define +boolean+ #t)
    (define +any+ #f)
    (define +not-integer+ 1/2)
    (define +number+ 2)
    (define +integer+ 3)
    (define +string+ " ")
    (define +list+ (list 1))
    (define +vector+ (vector 1))
    (define +unspecified+ #<unspecified>)
    (define +symbol+ 'symbol)
    (define +character+ #\a)

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

    (define (non-null-string? x)
      (and (string? x)
	   (not (string=? x ""))))
    
    (define (non-null-vector? x)
      (and (vector? x)
	   (not (equal? x #()))))
    
    (define (thunk? p)
      (and (procedure? p)
	   (let ((arity (procedure-arity p)))
	     (and (= (car arity) 0)
		  (= (cadr arity) 0)
		  (not (caddr arity))))))
    
    (define (thunkable? p)
      (and (procedure? p)
	   (= (car (procedure-arity p)) 0)))
    
    (define (one-argable? p)
      (and (procedure? p)
	   (let ((arity (procedure-arity p)))
	     (or (= (car arity) 1)
		 (and (= (car arity) 0)
		      (or (> (cadr arity) 0)
			  (caddr arity)))))))
    
    (define (integer-between-0-and-255? i) 
      (and (integer? i) (<= 0 i 255)))
    
    (define (sequence? obj)
      ;; scheme and C types here are ok, so...
      (and (not (number? obj))
	   (not (char? obj))
	   (not (boolean? obj))
	   (not (symbol? obj))))
    
    (define (pair-or-null? obj) ; list? is proper-list?
      (or (pair? obj)
	  (null? obj)))
    
    (let ((no-side-effect-functions 
	   (apply hash-table 
		  (map 
		   (lambda (op) 
		     (cons op #t)) 
		   '(* + - / < <= = > >= 
		       abs acos acosh and angle append ash asin asinh assoc assq assv atan atanh 
		       begin boolean? 
		       caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr 
		       call-with-exit car case catch cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr 
		       cddar cdddar cddddr cdddr cddr cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<? 
		       char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? char-ready? char-upcase 
		       char-upper-case? char-whitespace? char<=? char<? char=? char>=? char>? char? complex? cond 
		       cons constant? continuation? copy cos cosh current-environment current-error-port current-input-port current-output-port 
		       defined? denominator do dynamic-wind 
		       eof-object? eq? equal? eqv? even? exact->inexact exact? exp expt 
		       floor for-each 
		       gcd gensym global-environment 
		       hash-table hash-table-ref hash-table-size hash-table? hook hook-apply hook-arity hook-documentation hook-functions hook? 
		       if imag-part inexact->exact inexact? infinite? initial-environment input-port?  integer->char integer-decode-float 
		       integer-length integer? 
		       keyword->symbol keyword? 
		       lambda lcm length let let* letrec letrec* list list->string list->vector list-ref list-tail 
		       list? log logand logior lognot logxor 
		       macro? magnitude make-hash-table make-hook make-keyword make-list make-polar make-procedure-with-setter 
		       make-random-state make-rectangular make-string make-type make-vector map max member memq memv min modulo 
		       nan? negative? not null? number->string number? numerator 
		       object->string odd? or output-port? 
		       pair? port-closed? port-filename port-line-number positive? procedure-arity procedure-documentation procedure-environment 
		       procedure-source procedure-with-setter? procedure? provided? 
		       quasiquote quote quotient 
		       random rational? rationalize real-part real? remainder reverse round 
		       s7-version sin sinh sqrt string string->list string->number string->symbol string-append string-ci<=? string-ci<? 
		       string-ci=? string-ci>=? string-ci>? string-copy string-length string-ref string<=? string<? string=? string>=? 
		       string>? string? substring symbol symbol->keyword symbol->string symbol->value symbol? 
		       tan tanh truncate 
		       vector vector->list vector-dimensions vector-length vector-ref vector? 
		       zero?))))
	  
	  (function-types (hash-table 
			   (cons '* +number+)
			   (cons '+ +number+)
			   (cons '- +number+)
			   (cons '/ +number+)
			   (cons '< +boolean+)
			   (cons '<= +boolean+)
			   (cons '= +boolean+)
			   (cons '> +boolean+)
			   (cons '>= +boolean+)
			   (cons 'abs +number+)
			   (cons 'acos +not-integer+)
			   (cons 'acosh +not-integer+)
			   (cons 'angle +number+)
			   (cons 'ash +integer+)
			   (cons 'asin +not-integer+)
			   (cons 'asinh +not-integer+)
			   (cons 'assoc 'list-or-f)
			   (cons 'assq 'list-or-f)
			   (cons 'assv 'list-or-f)
			   (cons 'atan +not-integer+)
			   (cons 'atanh +not-integer+)
			   (cons 'boolean? +boolean+)
			   (cons 'ceiling +integer+)
			   (cons 'char->integer +integer+)
			   (cons 'char-alphabetic? +boolean+)
			   (cons 'char-ci<=? +boolean+)
			   (cons 'char-ci<? +boolean+)
			   (cons 'char-ci=? +boolean+)
			   (cons 'char-ci>=? +boolean+)
			   (cons 'char-ci>? +boolean+)
			   (cons 'char-downcase +character+)
			   (cons 'char-lower-case? +boolean+)
			   (cons 'char-numeric? +boolean+)
			   (cons 'char-ready? +boolean+)
			   (cons 'char-upcase +character+)
			   (cons 'char-upper-case? +boolean+)
			   (cons 'char-whitespace? +boolean+)
			   (cons 'char<=? +boolean+)
			   (cons 'char<? +boolean+)
			   (cons 'char=? +boolean+)
			   (cons 'char>=? +boolean+)
			   (cons 'char>? +boolean+)
			   (cons 'char? +boolean+)
			   (cons 'close-input-port +unspecified+)
			   (cons 'close-output-port +unspecified+)
			   (cons 'complex? +boolean+)
			   (cons 'cons +list+)
			   (cons 'constant? +boolean+)
			   (cons 'continuation? +boolean+)
			   (cons 'cos +number+)
			   (cons 'cosh +not-integer+)
			   (cons 'defined? +boolean+)
			   (cons 'denominator +integer+)
			   (cons 'display +unspecified+)
			   (cons 'environment? +boolean+)
			   (cons 'eof-object? +boolean+)
			   (cons 'eq? +boolean+)
			   (cons 'equal? +boolean+)
			   (cons 'eqv? +boolean+)
			   (cons 'even? +boolean+)
			   (cons 'exact->inexact +not-integer+)
			   (cons 'exact? +boolean+)
			   (cons 'exp +number+)
			   (cons 'expt +number+)
			   (cons 'floor +integer+)
			   (cons 'for-each +unspecified+)
			   (cons 'gcd +number+)
			   (cons 'gensym +symbol+)
			   (cons 'imag-part +number+)
			   (cons 'inexact->exact +number+)
			   (cons 'inexact? +boolean+)
			   (cons 'infinite? +boolean+)
			   (cons 'input-port? +boolean+)
			   (cons 'integer->char +character+)
			   (cons 'integer-decode-float +list+)
			   (cons 'integer-length +integer+)
			   (cons 'integer? +boolean+)
			   (cons 'keyword->symbol +symbol+)
			   (cons 'keyword? +boolean+)
			   (cons 'lcm +number+)
			   (cons 'length +integer+)
			   (cons 'list +list+)
			   (cons 'list->string +string+)
			   (cons 'list->vector +vector+)
			   (cons 'list? +boolean+)
			   (cons 'log +number+)
			   (cons 'logand +integer+)
			   (cons 'logior +integer+)
			   (cons 'lognot +integer+)
			   (cons 'logxor +integer+)
			   (cons 'macro? +boolean+)
			   (cons 'magnitude +number+)
			   (cons 'make-list +list+)
			   (cons 'make-polar +number+) 
			   (cons 'make-rectangular +number+)
			   (cons 'make-string +string+)
			   (cons 'make-vector +vector+)
			   (cons 'map +list+)
			   (cons 'max +number+)
			   (cons 'member 'list-or-f)
			   (cons 'memq 'list-or-f)
			   (cons 'memv 'list-or-f)
			   (cons 'min +number+)
			   (cons 'modulo +number+)
			   (cons 'nan? +boolean+)
			   (cons 'negative? +boolean+)
			   (cons 'newline +unspecified+)
			   (cons 'not +boolean+)
			   (cons 'null? +boolean+)
			   (cons 'number->string +string+)
			   (cons 'number? +boolean+)
			   (cons 'numerator +integer+)
			   (cons 'object->string +string+)
			   (cons 'odd? +boolean+)
			   (cons 'output-port? +boolean+)
			   (cons 'pair? +boolean+)
			   (cons 'peek-char 'char-or-eof)
			   (cons 'port-closed? +boolean+)
			   (cons 'positive? +boolean+)
			   (cons 'procedure? +boolean+)
			   (cons 'procedure-arity 'list-or-f)
			   (cons 'provided? +boolean+)
			   (cons 'quotient +number+)
			   (cons 'random-state->list +list+)
			   (cons 'rational? +boolean+)
			   (cons 'rationalize +number+)
			   (cons 'read-byte 'number-or-eof)
			   (cons 'read-char 'char-or-eof)
			   (cons 'read-line 'string-or-eof)
			   (cons 'real-part +number+)
			   (cons 'real? +boolean+)
			   (cons 'remainder +number+)
			   (cons 'round +integer+)
			   (cons 'sin +number+)
			   (cons 'sinh +not-integer+)
			   (cons 'sqrt +number+)
			   (cons 'string +string+)
			   (cons 'string->list +list+)
			   (cons 'string->number 'number-or-f)
			   (cons 'string->symbol +symbol+)
			   (cons 'string-append +string+)
			   (cons 'string-ci<=? +boolean+)
			   (cons 'string-ci<? +boolean+)
			   (cons 'string-ci=? +boolean+)
			   (cons 'string-ci>=? +boolean+)
			   (cons 'string-ci>? +boolean+)
			   (cons 'string-copy +string+)
			   (cons 'string-length +integer+)
			   (cons 'string-ref +character+)
			   (cons 'string<=? +boolean+)
			   (cons 'string<? +boolean+)
			   (cons 'string=? +boolean+)
			   (cons 'string>=? +boolean+)
			   (cons 'string>? +boolean+)
			   (cons 'string? +boolean+)
			   (cons 'symbol +symbol+)
			   (cons 'symbol->string +string+)
			   (cons 'symbol? +boolean+)
			   (cons 'tan +number+)
			   (cons 'tanh +not-integer+)
			   (cons 'truncate +integer+)
			   (cons 'vector +vector+)
			   (cons 'vector->list +list+)
			   (cons 'vector-length +integer+)
			   (cons 'vector? +boolean+)
			   (cons 'write +unspecified+)
			   (cons 'write-char +unspecified+)
			   (cons 'zero? +boolean+)
			   (cons 'procedure-with-setter? +boolean+)))
	  (argument-data (hash-table
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
			  (cons 'call-with-current-continuation one-argable?)
			  (cons 'call-with-exit one-argable?)
			  (cons 'call-with-input-file (list string? procedure?)) ; maybe these should also be one-argable? 
			  (cons 'call-with-input-string (list string? procedure?))
			  (cons 'call-with-output-file (list string? procedure?))
			  (cons 'call-with-output-string procedure?)
			  (cons 'call/cc one-argable?)
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
			  (cons 'dynamic-wind (list thunkable? thunkable? thunkable?))
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
			  (cons 'list-set! (list pair? non-negative-integer?))
			  (cons 'list-tail (list pair-or-null? non-negative-integer?))
			  (cons 'load non-null-string?)
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
			  (cons 'make-procedure-with-setter procedure?)
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
			  (cons 'string-fill! (list string? char?))
			  (cons 'string-length string?)
			  (cons 'string-ref (list non-null-string? non-negative-integer?))
			  (cons 'string-set! (list non-null-string? non-negative-integer? char?))
			  (cons 'string<=? string?)
			  (cons 'string<? string?)
			  (cons 'string=? string?)
			  (cons 'string>=? string?)
			  (cons 'string>? string?)
			  (cons 'substring (list string? non-negative-integer? non-negative-integer?))
			  (cons 'symbol->keyword symbol?)
			  (cons 'symbol->string symbol?)
			  (cons 'symbol->value (list symbol?)) ; opt arg is env
			  (cons 'tan number?)
			  (cons 'tanh number?)
			  (cons 'truncate real?)
			  (cons 'vector->list vector?)
			  (cons 'vector-dimensions vector?)
			  (cons 'vector-fill! (list vector?))
			  (cons 'vector-length vector?)
			  (cons 'vector-ref (list non-null-vector? non-negative-integer?))
			  (cons 'vector-set! (list non-null-vector? non-negative-integer?))
			  (cons 'with-input-from-file (list string? thunk?))
			  (cons 'with-input-from-string (list string? thunk?))
			  (cons 'with-output-to-file (list string? thunk?))
			  (cons 'with-output-to-string thunk?)
			  (cons 'write-byte (list integer-between-0-and-255?))
			  (cons 'zero? number?)))
	  
	  (loaded-files #f)
	  (globals #f)
	  (generators '())
	  (undefined-identifiers #f)
	  (other-identifiers #f)
	  (last-simplify-boolean-line-number -1)
	  (last-simplify-numeric-line-number -1))
      
      (define (->type c)
	(cond ((integer? c) +integer+)
	      ((number? c) +not-integer+)
	      ((string? c) +string+)
	      ((char? c) +character+)
	      ((null? c) +list+)
	      ((vector? c) +vector+)
	      ((eq? c #<unspecified>) +unspecified+)
	      ((boolean? c) +boolean+)
	      ((symbol? c) +symbol+)
	      ((pair? c)
	       (if (symbol? (car c))
		   (hash-table-ref function-types (car c))
		   (if (pair? (car c))
		       +any+ ; might be expr as func
		       +list+)))
	      (#t +any+)))
    
      ;; --------------------------------------------------------------------------------
      
      (define (truncated-list->string form)
	(format #f "~%        ~80A" form))
      
      (define (lint-format str name line . args)
	(if (and (> line 0)
		 (< line 100000))
	    (apply format #t (string-append "  ~A (line ~D): " str "~%") name line args)
	    (apply format #t (string-append "  ~A: " str "~%") name args)))
      
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
      
      
      (define (env-member arg env)
	;; find data associated with arg in env
	(or (assq arg env)
	    (hash-table-ref globals arg)))

      
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
		 (let ((e (env-member form env)))
		   (or (not e)
		       (and (>= (length e) 4)
			    (not (null? (list-ref e 3))))
		       ))))) ; it is a local function
      
      (define (lint-constant? arg)
	(or (number? arg)
	    (string? arg)
	    (null? arg)
	    (boolean? arg)
	    (char? arg)))
      
      (define (just-constants? form env)
	;; can we probably evaluate form given just built-in stuff?
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
	    (if (or (memq head '(eq? eqv? equal?))
		    (and (= (length form) 3)
			 (memq head '(= / max min < > <= >= - quotient remainder modulo lcm gcd and or
					string=? string<=? string>=? string<? string>?
					char=? char<=? char>=? char<? char>?))))
		(lint-format "this looks odd:~A"
			     name line-number 
			     ;; sigh (= a a) could be used to check for non-finite numbers, I suppose,
			     ;;   and (/ 0 0) might be deliberate (as in gmp)
			     (truncated-list->string form))
		(if (memq head '(= max min < > <= >= and or
				   string=? string<=? string>=? string<? string>?
				   char=? char<=? char>=? char<? char>?))
		    (lint-format "it looks odd to have repeated arguments in~A"
				 name line-number (truncated-list->string form))))))
      
      
      (define (check-for-repeated-args-with-not name line-number form env)
	
	(define (repeated-member-with-not? lst env)
	  (and (pair? lst)
	       (or (and (or (not (pair? (car lst)))
			    (not (side-effect? (car lst) env)))
			(or (memq (list 'not (car lst)) (cdr lst))
			    (and (pair? (car lst))
				 (eq? (caar lst) 'not)
				 (= (length (car lst)) 2)
				 (member (cadar lst) (cdr lst)))))
		   (repeated-member-with-not? (cdr lst) env))))
	
	(if (repeated-member-with-not? (cdr form) env)
	    (lint-format "this looks odd:~A"
			 name line-number 
			 (truncated-list->string form))))
      
      
      (define (check-args name line-number head form checkers env)
	;; check for obvious argument type problems
	;; name = overall caller, head = current caller, checkers = proc or list of procs for checking args
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
			(if (and (symbol? op)
				 (not (eq? symbol +symbol+)))
			    (if (and checker
				     *report-minor-stuff*)
				(if (memq op '(number-or-f list-or-f))
				    (lint-format "~A argument ~D might be #f:~A"
						 name line-number head arg-number
						 (truncated-list->string form))
				    (if (memq op '(number-or-eof char-or-eof string-or-eof))
					(lint-format "~A argument ~D might be the eof object:~A"
						     name line-number head arg-number
						     (truncated-list->string form)))))
			    (if (or (and op
					 (not (checker op)))
				    (and (just-constants? arg env)
					 (catch #t 
						(lambda ()
						  (and eval
						       (not (checker (eval arg)))))
						(lambda ignore-catch-error-args
						  #f))))
				(lint-format "~A's argument ~D should be a~A ~A: ~S:~A" 
					     name line-number head arg-number 
					     (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
					     checker arg 
					     (truncated-list->string form))
				
				(if (and (eq? (car arg) 'if)
					 (= (length arg) 3)
					 (not (checker #<unspecified>)))
				    (lint-format "~A argument might be ~A:~A"
						 name line-number head
						 #<unspecified>
						 (truncated-list->string form))))))
		      
		      (if (symbol? arg)
			  ;; if we're in a loop of some sort and the set! follows the ref,
			  ;;   this can be fooled.
			  (let ((var-data (env-member arg env)))
			    (if (and (pair? var-data)
				     (not (list-ref var-data 1)) ; a stop-gap -- refd?
				     (not (list-ref var-data 2)) ;               set?
				     (>= (length var-data) 5)
				     (not (member (list-ref var-data 4) (list +any+ +symbol+)))
				     (not (checker (list-ref var-data 4))))
				(lint-format "~A's argument ~D might not be a~A ~A: ~S:~A" 
					     name line-number head arg-number 
					     (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
					     checker arg
					     (truncated-list->string form))))

			  (if (not (checker arg))
			      (lint-format "~A's argument ~D should be a~A ~A: ~S:~A" 
					   name line-number head arg-number 
					   (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
					   checker arg 
					   (truncated-list->string form)))))
		  (if (list? checkers)
		      (if (null? (cdr checkers))
			  (done)
			  (set! checkers (cdr checkers))))
		  (set! arg-number (+ arg-number 1))))
	      (cdr form))))))
      
      
      (define (ref? name env)
	(let ((data (env-member name env)))
	  (if (pair? data)
	      (list-ref data 1))))
      
      
      (define (set? name env)
	(let ((data (env-member name env)))
	  (if (pair? data)
	      (list-ref data 2))))
      

      (define (set-ref? name env)
	;; if name is in env, set its "I've been referenced" flag
	(let ((data (env-member name env)))
	  (if (pair? data)
	      (list-set! data 1 #t))))
      
      
      (define (set-set? name env)
	(let ((data (env-member name env)))
	  (if (pair? data)
	      (list-set! data 2 #t)))) ; "I've been set"
      
      
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
      
      
      (define (remove x lst)
	(cond ((null? lst) '()) 
	      ((eqv? (car lst) x) (cdr lst)) ; x might be a number so eq? is not safe
	      (else (cons (car lst) 
			  (remove x (cdr lst))))))
      
      
      (define (remove-all x lst) 
	(cond ((null? lst) '()) 
	      ((equal? (car lst) x) (remove-all x (cdr lst)))
	      (else (cons (car lst) (remove-all x (cdr lst))))))
      
      
      (define (simplify-boolean in-form true false env)
	;; (or)->#f, (or x) -> x, (or x ... from here on we know x is #f), (or x #t...) -> (or x #t), any constant expr can be collapsed
	;;   (or ... (or ...) ...) -> or of all, (or ... #f ...) toss the #f
	;; similarly for and
	;; (or ... (not (and ...))) -> (or ... (not x) [from here we know x is true] (not y)...)
	;; (or ... (not (and x1 x2 ...))) -> (or ... (not x1) (not x2)...), but is that simpler?
	
	;; I wonder how far this could be pushed
	;;   (or x1 x2 x1) -> (or x1 x2) 
	;;   (and x1 x2 x1) -> (and x2 x1)
	
	(define (bsimp uform)
	  ;; find and remove any expressions that have no effect on the outcome
	  (if (or (not (pair? uform))
		  (not (memq (car uform) '(and or not)))
		  (side-effect? uform env)
		  (not eval))
	      uform
	      
	      (let ((vars '())
		    (associated-exprs '())
		    (ctr 0))
		
		(define (tree-remove-all x lst) 
		  (cond ((null? lst) '()) 
			((equal? (car lst) x) (tree-remove-all x (cdr lst)))
			((pair? (car lst)) (cons (tree-remove-all x (car lst)) (tree-remove-all x (cdr lst))))
			(else (cons (car lst) (tree-remove-all x (cdr lst))))))
		
		(define (canonical-tree lst)
		  (let ((data (assoc lst associated-exprs)))
		    (if data
			(cdr data)
			(if (pair? lst) 
			    (cons (canonical-tree (car lst)) 
				  (canonical-tree (cdr lst))) 
			    lst))))
		
		(define (cdr-assoc val lst)
		  (if (not (pair? lst))
		      #f
		      (if (equal? (cdar lst) val)
			  (car lst)
			  (cdr-assoc val (cdr lst)))))
		
		(define (expand expr)
		  (let ((data (cdr-assoc expr associated-exprs)))
		    (if data
			(copy (car data))
			(if (pair? expr)
			    (cons (expand (car expr))
				  (expand (cdr expr)))
			    expr))))
		
		(define (bool-walk form func) 
		  (if (and (pair? form)
			   (memq (car form) '(and or not)))
		      (for-each
		       (lambda (e)
			 (bool-walk e func))
		       (cdr form))
		      (func form)))
		
		(bool-walk uform (lambda (val) 
				   (if (and (or (pair? val)
						(symbol? val))
					    (not (assoc val associated-exprs))
					    (not (memq val '(and or not)))) ; (not not)
				       (let ((new-var (string->symbol (format #f "bool-~D" ctr))))
					 (set! vars (cons new-var vars))
					 (set! associated-exprs (cons (cons val new-var) associated-exprs))
					 (set! ctr (+ ctr 1))))))
		
		(if (or (null? vars)
			(> (length vars) 8))
		    uform
		    (let* ((len (length vars))
			   (vsize (expt 2 len)) ; 2^n possible cases
			   (v (make-vector vsize))
			   (vals '())
			   (nonf (make-vector len))
			   (cur 0)
			   (ctr 0)
			   (form (canonical-tree uform)))
		      
		      (for-each
		       (lambda (var)
			 (do ((i cur (+ i 1)))
			     ((not (tree-member i form))
			      (set! cur (+ i 1))
			      (vector-set! nonf ctr i)
			      (set! ctr (+ ctr 1)))))
		       vars)
		      
		      (do ((ctr 0 (+ ctr 1)))
			  ((= ctr vsize))
			(vector-set! v ctr (eval `((lambda ,vars ,form)
						   ,@(let ((pos -1))
						       (map (lambda (var)
							      (set! pos (+ pos 1))
							      (and (logbit? ctr pos) 
								   (vector-ref nonf pos)))
							    vars)))))
			(if (not (member (vector-ref v ctr) vals))
			    (set! vals (cons (vector-ref v ctr) vals))))
		      
		      (if (= (length vals) 1)
			  (car vals)
			  (let ((none-vars '())
				(pos -1))
			    (for-each
			     (lambda (var)
			       (set! pos (+ pos 1))
			       (let ((bit (ash 1 pos)))
				 (call-with-exit
				  (lambda (return)
				    (do ((ctr 0 (+ ctr 1)))
					((= ctr vsize)
					 (set! none-vars (cons var none-vars)))
				      (if (zero? (logand ctr bit))
					  (if (not (equal? (vector-ref v ctr) (vector-ref v (logior ctr bit))))
					      (return))))))))
			     vars)
			    
			    (if (not (null? none-vars))
				(begin
				  (for-each
				   (lambda (nv)
				     (set! form (tree-remove-all nv form)))
				   none-vars)
				  (expand form))
				uform))))))))
	
	(define (true? e)
	  (or (member e true)
	      (and (pair? e)
		   (= (length e) 2)
		   (or (member e true 
				    (lambda (a b)
				      ;; if a follows b, and b is true, do we know already know that a?
				      ;; (and (< x1 12) (real? x1) (= x1 1)) -> (and (< x1 12) (= x1 1))
				      (and (pair? b)
					   (or (and (= (length b) 2)
						    (equal? (cadr a) (cadr b))
						    (case (car a)
						      ((complex?)  (memq (car b) '(number? real? rational? integer? even? odd? 
											   positive? negative? zero? exact? inexact?)))
						      ((number?)   (memq (car b) '(complex? real? rational? integer? even? odd? 
											    positive? negative? zero? exact? inexact?)))
						      ((real?)     (memq (car b) '(rational? integer? even? odd? positive? negative? exact? inexact?)))
						      ((rational?) (memq (car b) '(integer? even? odd?)))
						      ((integer?)  (memq (car b) '(even? 'odd)))
						      (else #f)))
					       (and (> (length b) 2)
						    (member (cadr a) (cdr b))
						    (case (car a)
						      ((complex? number?) (eq? (car b) '=))
						      ((real?)            (memq (car b) '(< > <= >=)))
						      (else #f)))))))
		       (and (pair? (cadr e))
			    (case (car e)
			      ((complex? number?) (number? (hash-table-ref function-types (caadr e))))
			      ((exact? rational?) (eq? (caadr e) 'inexact->exact))
			      ((inexact? real?)   (eq? (caadr e) 'exact->inexact))
			      ((char?)            (char? (hash-table-ref function-types (caadr e))))
			      ((string?)          (string? (hash-table-ref function-types (caadr e))))
			      ((vector?)          (vector? (hash-table-ref function-types (caadr e))))
			      (else #f)))))))
	
	(define (false? e)
	  
	  (define (bad-arg-match a b)
	    
	    ;; these accept only the given type and can return a boolean (so their value in a boolean expression is not known in advance)
	    (define (number-op? x) (memq x '(= < > <= >= even? odd? positive? negative? zero?)))
	    (define (char-op? x)   (memq x '(char=? char<? char<=? char>? char>=? char-ci=? char-ci<? char-ci<=? char-ci>? char-ci>=? 
						    char-alphabetic? char-numeric? char-whitespace? char-lower-case? char-upper-case?)))
	    (define (list-op? x)   (memq x '(caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr 
						    cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar 
						    cddddr cdddr cddr cdr list-ref)))
	    (define (string-op? x) (memq x '(string=? string<? string<=? string>? string>=? string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?)))
	    
	    (case a
	      ((complex? number? real? rational? integer?) 
	       (or (char-op? b)     ; that is, if these are false, then a non-number was accepted
		   (list-op? b)     ;    earlier as a valid argument, so it can't be a number
		   (string-op? b))) ;    (or (char=? x1 #\a) (complex? x1) x1) -> (or (char=? x1 #\a) x1)
	      ((char?) 
	       (or (number-op? b)
		   (list-op? b)
		   (string-op? b)))
	      ((string?) 
	       (or (char-op? b)
		   (list-op? b)
		   (number-op? b)))
	      ((list?) 
	       (or (char-op? b)
		   (number-op? b)
		   (string-op? b)))
	      ((boolean? procedure? symbol? hook? continuation? environment?)
	       (or (char-op? b)
		   (number-op? b)
		   (list-op? b)
		   (string-op? b)))
	      (else #f)))
	  
	  (or (member e false)
	      (and (pair? e)
		   (= (length e) 2)
		   (or (member e false (lambda (a b)
					      (and (pair? b)
						   (>= (length b) 2)
						   (member (cadr a) (cdr b))
						   (bad-arg-match (car a) (car b)))))
		       (member e true (lambda (a b)
					     (and (pair? b)
						  (>= (length b) 2)
						  (member (cadr a) (cdr b))
						  (bad-arg-match (car a) (car b)))))
		       (and (eq? (car e) 'null?)
			    (pair? (cadr e))
			    (eq? (hash-table-ref function-types (caadr e)) 'list-or-f))))))
	
	
	(define (contradictory? ands)
	  (let ((vars '()))
	    (call-with-exit
	     (lambda (return)
	       (do ((b ands (cdr b)))
		   ((null? b) #f)
		 (if (and (pair? b)
			  (pair? (car b))
			  (pair? (cdar b)))
		     (let* ((func (caar b))
			    (arg-type (or (hash-table-ref argument-data func)
					  (and (memq func '(string? pair? symbol? number? hash-table? hook? boolean? char? vector? procedure?))
					       (symbol->value func))
					  (and (memq func '(complex? integer? rational? real?))
					       number?)
					  (and (memq func '(null? list?))
					       pair?)))
			    (args (cdar b)))
		       (if (memq arg-type (list integer? real? rational? complex?))
			   (set! arg-type number?)
			   (if (eq? arg-type list?)
			       (set! arg-type pair?)))
		       
		       (if (and (procedure? arg-type)
				(not (eq? arg-type sequence?)))
			   (for-each
			    (lambda (arg)
			      (if (symbol? arg)
				  (let ((type (assq arg vars)))
				    (if (not type)
					(set! vars (cons (cons arg arg-type) vars))
					(if (not (eq? (cdr type) arg-type))
					    (return #t))))))
			    args)))))))))
	
	
	(define (classify e)
	  ;; do we already know that e is true or false?
	  ;;   some cases subsume others: if we know (integer? x) is true, (complex? x) is also true
	  (if (true? e)
	      #t ; the simple boolean is passed back which will either be dropped or will stop the outer expr build
	      (if (false? e)
		  #f
		  ;; eval of a constant expression here is tricky -- for example, (sqrt 2) should not be turned into 1.414...
		  (if (and eval
			   (just-constants? e env))
		      (catch #t
			     (lambda ()
			       (let ((val (eval e)))
				 (if (boolean? val)
				     val
				     e)))
			     (lambda ignore e))
		      e))))
	
	(define (store e value and/or)
	  ;; we can't make any assumptions about the expression if it might have side effects
	  ;;   for example (or (= (oscil o) 0.0) (= (oscil o) 0.0)) can't be reduced
	  
	  (if (not (side-effect? e env))
	      (let ((its-true (if (eq? and/or 'or)
				  (eq? value #t)             ; or, so it's false if unknown
				  (not (eq? value #f)))))
		(if its-true
		    (set! true (cons e true))
		    (set! false (cons e false))))))
	
	(let ((form (bsimp in-form)))
					; (if (not (equal? form in-form)) (format #t "bsimp ~A -> ~A~%" in-form form))
	  
	  (if (or (not (pair? form))
		  (not (memq (car form) '(or and not))))
	      (classify form)
	      (let ((len (length form)))
		
		(case (car form)
		  
		  ((not)
		   (if (= len 2)
		       (let* ((arg (cadr form))
			      (val (if (and (pair? arg)
					    (memq (car arg) '(and or not)))
				       (classify (simplify-boolean arg true false env))
				       (classify arg))))
			 (if (boolean? val)
			     (not val)
			     (if (or (and (not (symbol? arg))
					  (not (pair? arg)))
				     (and (pair? arg)
					  (not (env-member (car arg) env))
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
					  (memq (car val) '(and or not)))
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
			   (if (contradictory? (cdr form))
			       #f
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
					      (memq (car val) '(and or not)))
					 (set! val (classify (simplify-boolean e true false env))))
				     
				     ;; (and x1 x2 x1) is not reducible, unless to (and x2 x1)
				     ;;   the final thing has to remain at the end, but can be deleted earlier if it can't short-circuit the evaluation,
				     ;;   but if there are expressions following the first x1, we can't be sure that it is not
				     ;;   protecting them:
				     ;;       (and false-or-0 (display (list-ref lst false-or-0)) false-or-0)
				     ;;   so I'll not try to optimize that case.  But (and x x) is optimizable.
				     
				     (if (eq? val #t)
					 (if (and (not (equal? e #t))
						  (or (not (pair? e))
						      (not (eq? (hash-table-ref function-types (car e)) #t))))
					     (if (or (null? new-form)
						     (not (equal? e (car new-form))))
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
						 (if (not (and (pair? e)                   ; (and ... (or ... 123) ...) -> splice out or
							       (not (null? (cdr exprs)))
							       (eq? (car e) 'or)
							       (> (length e) 2)
							       (let ((last (list-ref e (- (length e) 1))))
								 (and last ; (or ... #f)
								      (not (pair? last))
								      (not (symbol? last))))))
						     (begin                 ; else add it to our new expression with value #t
						       (store e val 'and)
						       (set! new-form (cons val new-form)))))))))))))
		   ))))))
      
      
      (define (numeric? op)
	(memq op '(+ * - / 
		     sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh 
		     log exp expt sqrt make-polar make-rectangular
		     imag-part real-part abs magnitude angle max min exact->inexact
		     modulo remainder quotient lcd gcd
		     rationalize inexact->exact
		     logior lognot logxor logand numerator denominator 
		     floor round truncate ceiling ash)))
      
      (define (splice-if f lst)
	(cond ((null? lst) '())
	      ((pair? lst)
	       (if (and (pair? (car lst))
			(f (caar lst)))
		   (append (splice-if f (cdar lst)) (splice-if f (cdr lst)))
		   (cons (car lst) (splice-if f (cdr lst)))))
	      (#t lst)))
      
      (define (simplify-numerics form env)
	;;   I first tried a table of rules, but the code was unreadable, so
	;;   here I'll split out each case by hand.
	;; This is not an agressive simplification.
	
	;; this returns a form, possibly the original simplified
	(let ((complex-result? (lambda (op) (memq op '(+ * - / 
							 sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh 
							 log exp expt sqrt make-polar make-rectangular))))
	      (real-result? (lambda (op) (memq op '(imag-part real-part abs magnitude angle max min exact->inexact
							      modulo remainder quotient lcd gcd))))
	      (rational-result? (lambda (op) (memq op '(rationalize inexact->exact))))
	      (integer-result? (lambda (op) (memq op '(logior lognot logxor logand numerator denominator 
							      floor round truncate ceiling ash)))))
	  
	  (define (inverse-op op)
	    (case op 
	      ((sin) 'asin) ((cos) 'acos) ((tan) 'atan) ((asin) 'sin) ((acos) 'cos) ((atan) 'tan)
	      ((sinh) 'asinh) ((cosh) 'acosh) ((tanh) 'atanh) ((asinh) 'sinh) ((acosh) 'cosh) ((atanh) 'tanh)
	      ((log) exp) ((exp) log)))
	  
	  
	  (define (remove-duplicates lst)
	    (letrec ((rem-dup
		      (lambda (lst nlst)
			(cond ((null? lst) nlst)
			      ((member (car lst) nlst) (rem-dup (cdr lst) nlst))
			      (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
	      (reverse (rem-dup lst '()))))
	  
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
			   eval
			   (just-rationals? f))
		      (catch #t
			     (lambda ()
			       (eval f))
			     (lambda ignore f))
		      f))))
	  
	  (let* ((args (map simplify-arg (cdr form)))
		 (len (length args)))
	    
	    (case (car form)
	      ((+)
	       (case len
		 ((0) 0)
		 ((1) (car args))
		 (else 
		  (let ((val (remove-all 0 (splice-if (lambda (x) (eq? x '+)) args))))
		    (case (length val)
		      ((0) 0)
		      ((1) (car val))                     ; (+ x) -> x
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
		    (case (length val)
		      ((0) 0)
		      ((1) (car val))                     ; (* x) -> x
		      (else 
		       (if (just-rationals? val)
			   (apply * val)
			   (if (member 0 val)             ; (* x 0 2) -> 0
			       0 
			       (if (and (= len 2)
					(member -1 val))
				   `(- ,@(remove -1 val)) ; (* -1 x) -> (- x)
				   `(* ,@val))))))))))
	      
	      ((-)
	       (case len
		 ((0) form)
		 ((1) ; negate
		  (if (number? (car args))
		      (- (car args))
		      (if (not (list? (car args)))
			  `(- ,@args)
			  (case (length (car args))
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
			  `(- ,(cadr args))                ; (- 0 x) -> (- x)
			  (if (equal? (cadr args) 0)
			      (car args)                   ; (- x 0) -> x
			      (if (equal? (car args) (cadr args))
				  0                        ; (- x x)
				  (if (and (pair? (car args))
					   (eq? (caar args) '-)
					   (> (length (car args)) 2))
				      `(- ,@(cdar args) ,(cadr args)) ; (- (- x y) z) -> (- x y z) but leave (- (- x) ...)
				      `(- ,@args)))))))
		 (else 
		  (if (just-rationals? args)
		      (apply - args)
		      (let ((nargs (remove-all 0 (splice-if (lambda (x) (eq? x '+)) (cdr args)))) ; (- x a (+ b c) d) -> (- x a b c d)
			    (first-arg (car args)))
			(if (member first-arg nargs)
			    (begin
			      (set! nargs (remove first-arg nargs)) ; remove once
			      (set! first-arg 0)))
			(if (null? nargs)
			    first-arg                     ; (- x 0 0 0)?
			    (if (and (equal? first-arg 0)
				     (= (length nargs) 1))
				(if (number? (car nargs))
				    (- (car nargs))
				    `(- ,(car nargs)))    ; (- 0 0 0 x)?
				`(- ,@(cons first-arg nargs)))))))))
	      
	      ((/)
	       (case len
		 ((0) form)
		 ((1) ; invert
		  (if (number? (car args))
		      (if (zero? (car args))
			  `(/ ,(car args))
			  (/ (car args)))
		      (if (pair? (car args))
			  (if (and (= (length (car args)) 2)
				   (eq? (caar args) '/))
			      (cadar args)
			      `(/ ,@args))
			  form)))
		 (else ; one other verbose case is (/ 1 x) -> (/ x)
		  (if (and (just-rationals? args)
			   (not (member 0 args))
			   (not (member 0.0 args)))
		      (apply / args)
		      (let ((nargs 
			     (if (> len 2) ; (/ x a (* b 1 c) d) -> (/ x a b c d) but not short cases
				 (remove-all 1 (splice-if (lambda (x) (eq? x '*)) (cdr args)))
				 (remove-all 1 (cdr args)))))
			(if (null? nargs) ; (/ x 1 1) -> x
			    (car args)
			    `(/ ,@(cons (car args) nargs))))))))
	      
	      ((sin cos asin acos sinh cosh tanh asinh acosh atanh exp)
	       (if (and (= len 1)
			(pair? (car args))
			(= (length (car args)) 2)
			(eq? (caar args) (inverse-op (car form))))
		   (cadar args)
		   (if (and (= len 1)
			    (equal? (car args) 0))
		       (case (car form)
			 ((sin asin sinh asinh tanh atanh) 0)
			 ((exp cos cosh) 1)
			 (else `(,(car form) 0)))
		       `(,(car form) ,@args))))
	      
	      ((log)
	       (if (and (= len 1)
			(pair? (car args))
			(= (length (car args)) 2)
			(eq? (caar args) 'exp))
		   (cadar args)
		   (if (and (= len 2)
			    (equal? (car args) (cadr args)))
		       (if (integer? (car args))
			   1
			   1.0)
		       `(log ,@args))))
	      
	      ((sqrt)
	       (if (and (pair? args)
			(rational? (car args))
			(= (car args) (* (sqrt (car args)) (sqrt (car args)))))
		   (sqrt (car args))
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
		   (catch #t ; catch needed here for things like (ash 2 64)
			  (lambda ()
			    (apply (symbol->value (car form)) args))
			  (lambda ignore
			    `(,(car form) ,@args)))
		   `(,(car form) ,@args)))
	      
	      ((atan expt angle) ; (angle -1) and (* 4 (atan 1)) are common ways to get pi, so don't simplify these 
	       `(,(car form) ,@args))
	      
	      ((inexact->exact)
	       (if (= len 1)
		   (if (or (rational? (car args))
			   (and (pair? (car args))
				(or (rational-result? (caar args))
				    (integer-result? (caar args)))))
		       (car args)
		       `(inexact->exact ,@args))
		   form))
	      
	      ((logior)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'logior)) args)))
	       (if (null? args)
		   0
		   (if (member -1 args)
		       -1
		       (if (just-integers? args)
			   (apply logior args)
			   `(logior ,@args)))))
	      
	      ((logand)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'logand)) args)))
	       (if (null? args)
		   -1
		   (if (member 0 args)
		       0
		       (if (just-integers? args)
			   (apply logand args)
			   `(logand ,@args)))))
	      
	      ((gcd)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'gcd)) args)))
	       (if (null? args)
		   0
		   (if (member 1 args)
		       1
		       (if (just-integers? args)
			   (catch #t  ; maybe (gcd -9223372036854775808 -9223372036854775808)
				  (lambda ()
				    (apply gcd args))
				  (lambda ignore
				    `(gcd ,@args)))
			   `(gcd ,@args)))))
	      
	      ((lcm)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'lcm)) args)))
	       (if (null? args)
		   1
		   (if (member 0 args)
		       0
		       (if (just-integers? args)
			   (catch #t
				  (lambda ()
				    (apply lcm args))
				  (lambda ignore
				    `(lcm ,@args)))
			   `(lcm ,@args)))))
	      
	      ((max min)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x (car form))) args)))
	       (if (= len 1)
		   (car args)
		   (if (just-rationals? args)
		       (apply (symbol->value (car form)) args)
		       `(,(car form) ,@args))))
	      
	      (else `(,(car form) ,@args))))))
      
      
      (define (check-special-cases name line-number head form env)
	(case head
	  ((load) ; pick up the top level declarations
	   (if (>= (length form) 2)
	       (scan form))
	   env)
	  
	  ((= equal?)
	   (if (and *report-minor-stuff*
		    (> (length form) 2)
		    (any-real? (cdr form)))
	       (lint-format "~A can be troublesome with floats:~A"
			    name line-number head 
			    (truncated-list->string form))))
	  
	  ((memq assq)
	   (if (= (length form) 3)
	       (if (or (and (number? (cadr form))
			    (not (rational? (cadr form))))
		       (string? (cadr form))
		       (vector? (cadr form)))
		   (lint-format "this is always #f:~A"
				name line-number
				(truncated-list->string form)))))
	  
	  ((memv assv)
	   (if (= (length form) 3)
	       (if (or (string? (cadr form))
		       (vector? (cadr form)))
		   (lint-format "this is problematic -- perhaps use ~A instead:~A"
				name line-number
				(if (eq? head 'memv) 'member 'assoc)
				(truncated-list->string form)))))
	  
	  ((member)
	   (if (= (length form) 4)
	       (let ((func (list-ref form 3)))
		 (if (or (eq? func 'eq?)
			 (and (pair? func)
			      (= (length func) 3)
			      (eq? (car func) 'lambda)
			      (pair? (cadr func))
			      (pair? (caddr func))
			      (let ((eq (caddr func))
				    (args (cadr func)))
				(and (memq (car eq) '(eq? eqv? equal?))
				     (eq? (car args) (cadr eq))
				     (pair? (caddr eq))
				     (eq? (car (caddr eq)) 'car)
				     (pair? (cdr (caddr eq)))
				     (eq? (cadr args) (cadr (caddr eq)))))))
		     (lint-format "member might perhaps be ~A"
				  name line-number
				  (if (eq? (car (caddr func)) 'eq?) 'assq
				      (if (eq? (car (caddr func)) 'eqv?) 'assv 'assoc)))))))
	  
	  ((if)
	   (let ((len (length form)))
	     (if (> len 4)
		 (lint-format "if has too many clauses: ~S" 
			      name line-number form)
		 (if (< len 3)
		     (lint-format "if has too few clauses: ~S" 
				  name line-number form)
		     
		     (if *report-minor-stuff*
			 (let ((expr (simplify-boolean (cadr form) '() '() env)))
			   (if (equal? expr #t)
			       (lint-format "possible simplification:~A"
					    name line-number
					    (lists->string form (caddr form)))
			       (if (equal? expr #f)
				   (if (null? (cdddr form))
				       (if (not (equal? (caddr form) #f))
					   (lint-format "~S is never #t:~A"
							name line-number (cadr form)
							(truncated-list->string form)))
				       (lint-format "possible simplification:~A"
						    name line-number
						    (lists->string form (cadddr form))))
				   (if (and (boolean? (list-ref form 2))
					    (not (null? (cdddr form)))
					    (boolean? (list-ref form 3))
					    (not (eq? (list-ref form 2) (list-ref form 3)))) ; !
				       (lint-format "possible simplification:~A"
						    name line-number
						    (lists->string form (if (list-ref form 2)
									    expr
									    `(not ,expr))))
				       (if (and (= len 4)
						(equal? (caddr form) (cadddr form)))
					   (lint-format "if is not needed here:~A"
							name line-number 
							(truncated-list->string form))))))))))))
	  
	  ((car cdr)
	   (if (and *report-minor-stuff*
		    (pair? (cadr form))
		    (eq? (car (cadr form)) 'cons))
	       (lint-format "(~A~A) is the same as~A"
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
		     (lint-format "possible simplification:~A"
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
			   (lint-format "~A is not needed:~A"
					name line-number head 
					(truncated-list->string form))
			   (if (not (tree-member-ignoring-car continuation body))
			       (lint-format "~A could be call-with-exit:~A"	
					    name line-number head 
					    (truncated-list->string form)))))))))
	  
	  ((/)
	   (if (not (null? (cdr form)))
	       (if (and (null? (cddr form))
			(number? (cadr form))
			(zero? (cadr form)))
		   (lint-format "attempt to invert zero:~A"
				name line-number 
				(truncated-list->string form))
		   (if (and (not (null? (cddr form)))
			    (member 0 (cddr form)))
		       (lint-format "attempt to divide by 0:~A"
				    name line-number
				    (truncated-list->string form))))))
	  
	  ((copy)
	   (if (not (null? (cdr form)))
	       (if (or (number? (cadr form))
		       (boolean? (cadr form))
		       (char? (cadr form))
		       (and (pair? (cadr form))
			    (or (eq? (caadr form) 'copy)
				(eq? (caadr form) 'string-copy))))
		   (lint-format "~A could be ~A" 
				name line-number form (cadr form)))))
	  
	  ((string-copy)
	   (if (and (not (null? (cdr form)))
		    (pair? (cadr form))
		    (or (eq? (caadr form) 'copy)
			(eq? (caadr form) 'string-copy)))
	       (lint-format "~A could be ~A" 
			    name line-number form (cadr form))))
	  
	  ((string-append)
	   (if (not (= line-number last-simplify-boolean-line-number))
	       (let ((args (remove-all "" (splice-if (lambda (x) (eq? x 'string-append)) (cdr form)))))
		 (if (null? args)
		     (lint-format "this is pointless:~A"
				  name line-number
				  (truncated-list->string form))
		     (if (< (length args) (length (cdr form)))
			 (lint-format "possible simplification:~A"
				      name line-number
				      (lists->string form `(string-append ,@args)))))
		 (set! last-simplify-boolean-line-number line-number))))
	  
	  ((object->string)
	   (if (and (not (null? (cdr form)))
		    (pair? (cadr form))
		    (eq? (caadr form) 'object->string))
	       (lint-format "~A could be ~A" 
			    name line-number form (cadr form))))
	  
	  ((reverse list->vector vector->list list->string string->list symbol->string string->symbol number->string)
	   (let ((inverses '((reverse . reverse) 
			     (list->vector . vector->list)
			     (vector->list . list->vector)
			     (symbol->string . string->symbol)
			     (string->symbol . symbol->string)
			     (list->string . string->list)
			     (string->list . list->string)
			     (number->string . string->number))))
	     (if (and (not (null? (cdr form)))
		      (pair? (cadr form))
		      (not (null? (cdadr form)))
		      (eq? (caadr form) (let ((p (assq head inverses))) (and (pair? p) (cdr p)))))
		 (lint-format "~A could be (copy ~A)" 
			      name line-number form (cadadr form)))))
	  
	  ((char->integer integer->char symbol->keyword keyword->symbol string->number)
	   (let ((inverses '((char->integer . integer->char)
			     (integer->char . char->integer)
			     (symbol->keyword . keyword->symbol)
			     (keyword->symbol . symbol->keyword)
			     (string->number . number->string))))
	     (if (and (not (null? (cdr form)))
		      (pair? (cadr form))
		      (not (null? (cdadr form)))
		      (eq? (caadr form) (let ((p (assq head inverses))) (and (pair? p) (cdr p)))))
		 (lint-format "~A could be ~A" 
			      name line-number form (cadadr form)))))
	  
	  ((append)
	   (if (= (length form) 2)
	       (lint-format "~A could be ~A" 
			    name line-number form (cadr form))))
	  
	  ((sort!)
	   (if (and (= (length form) 3)
		    (memq (caddr form) '(= <= >= eq? eqv? equal?
					   string=? string<=? string>=? char=? char<=? char>=?
					   string-ci=? string-ci<=? string-ci>=? char-ci=? char-ci<=? char-ci>=?)))
	       (lint-format "sort! with ~A may hang:~A"
			    name line-number head 
			    (truncated-list->string form))))))
      
      
      (define (check-call name line-number head form env)
	(let ((fdata (env-member head env)))
	  (if (pair? fdata)
	      ;; a local var
	      (let ()
		(if (and (>= (length fdata) 4)
			 (not (null? (list-ref fdata 3))))
		    (let ((type (car (list-ref fdata 3)))
			  (args (cadr (list-ref fdata 3))))
		      
		      (let ((rst (or (not (pair? args))
				     (negative? (length args))
				     (member ':rest args)))
			    (pargs (if (pair? args) (proper-list args) '())))
			
			(let ((call-args (length (cdr form)))
			      (decl-args (max 0 (- (length pargs) (keywords pargs) (if rst 1 0)))))
			  
			  (let ((req (if (memq type '(define lambda)) decl-args 0))
				(opt (if (memq type '(define lambda)) 0 decl-args)))
			    (if (< call-args req)
				(lint-format "~A needs ~D argument~A:~A" 
					     name line-number head 
					     req (if (> req 1) "s" "") 
					     (truncated-list->string form))
				(if (and (not rst)
					 (> (- call-args (keywords (cdr form))) (+ req opt)))
				    (lint-format "~A has too many arguments:~A" 
						 name line-number head 
						 (truncated-list->string form))))
			    (if (memq type '(define* lambda*))
				(if (not (member ':allow-other-keys pargs))
				    (for-each
				     (lambda (arg)
				       (if (and (keyword? arg)
						(not (member arg '(:rest :key :optional))))
					   (if (not (member (keyword->symbol arg) pargs 
								 (lambda (a b)
								   (if (pair? b) 
								       (eq? a (car b))
								       (eq? a b)))))
					       (lint-format "~A keyword argument ~A (in ~S) does not match any argument in ~S"
							    name line-number head arg form pargs))))
				     (cdr form))))))))))
	      ;; not local var
	      (if (and (symbol? head)
		       (procedure? (symbol->value head)))
		  ;; check arg number
		  (let ((arity (procedure-arity (symbol->value head)))
			(args (length (cdr form))))
		    
		    (if (pair? arity)
			(if (< args (car arity))
			    (lint-format "~A needs ~A~D argument~A:~A" 
					 name line-number head 
					 (if (and (= 0 (cadr arity)) (not (caddr arity))) "" "at least ")
					 (car arity) 
					 (if (> (car arity) 1) "s" "") 
					 (truncated-list->string form))
			    (if (and (not (caddr arity))
				     (> (- args (keywords (cdr form))) (+ (car arity) (cadr arity))))
				(lint-format "~A has too many arguments:~A" 
					     name line-number head 
					     (truncated-list->string form)))))
		    
		    (if (pair? (cdr form)) ; there are args
			(begin
			  ;; if keywords, check that they are acceptable
			  ;;    this only applies to lambda*'s that have been previously loaded (lint doesn't create them)
			  (let ((source (procedure-source (symbol->value head))))
			    (if (and (pair? source)
				     (eq? (car source) 'lambda*))
				(let ((decls (cadr source)))
				  (if (not (member ':allow-other-keys decls))
				      (for-each
				       (lambda (arg)
					 (if (and (keyword? arg)
						  (not (member arg '(:rest :key :optional))))
					     (if (not (member arg decls 
								   (lambda (a b) 
								     (if (pair? b) 
									 (eq? (keyword->symbol a) (car b))
									 (eq? (keyword->symbol a) b)))))
						 (lint-format "~A keyword argument ~A (in ~S) does not match any argument in ~S"
							      name line-number head arg form decls))))
				       (cdr form))))))
			  
			  (case head
			    ((eq?) 
			     (if (< (length form) 3)
				 (lint-format "eq? needs 2 arguments:~A"
					      name line-number 
					      (truncated-list->string form))
				 (if (or (and (number? (cadr form))
					      (rational? (cadr form)))
					 (char? (cadr form))
					 (and (number? (caddr form))
					      (rational? (caddr form)))
					 (char? (caddr form)))
				     (lint-format "eq? doesn't work reliably with args like ~S" 
						  name line-number form)
				     (if (or (and (number? (cadr form))
						  (not (rational? (cadr form))))
					     (string? (cadr form))
					     (vector? (cadr form))
					     (and (number? (caddr form))
						  (not (rational? (caddr form))))
					     (string? (caddr form))
					     (vector? (caddr form)))
					 (lint-format "~A is always #f"
						      name line-number form))))
			     
			     (check-for-repeated-args name line-number head form env)
			     (check-for-repeated-args-with-not name line-number form env))
			    
			    ((eqv?) 
			     (if (< (length form) 3)
				 (lint-format "eqv? needs 2 arguments:~A"
					      name line-number 
					      (truncated-list->string form))
				 (if (or (vector? (cadr form))
					 (string? (cadr form))
					 (vector? (caddr form))
					 (string? (caddr form)))
				     (lint-format "eqv? doesn't work reliably with args like ~S" 
						  name line-number form)))
			     
			     (check-for-repeated-args name line-number head form env)
			     (check-for-repeated-args-with-not name line-number form env))
			    
			    ((map for-each)
			     (let* ((len (length form))
				    (args (- len 2)))
			       (if (< len 3)
				   (lint-format "~A missing argument~A in:~A"
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
					      (memq (caadr form) '(lambda lambda*))
					      (pair? (cadr (cadr form))))
					 (let ((arglen (length (cadr (cadr form)))))
					   (if (eq? (cadr form) 'lambda)
					       (if (negative? arglen)
						   (set! arity (list (abs arglen) 0 #t))
						   (set! arity (list arglen 0 #f)))
					       (if (negative? arglen)
						   (set! arity (list 0 (abs arglen) #t))
						   (set! arity (list 0 arglen (member ':rest (cadr (cadr form))))))))))
				 
				 (if (pair? arity)
				     (if (< args (car arity))
					 (lint-format "~A has too few arguments in: ~A"
						      name line-number head 
						      (truncated-list->string form))
					 (if (and (not (caddr arity))
						  (> args (+ (car arity) (cadr arity))))
					     (lint-format "~A has too many arguments in: ~A"
							  name line-number head 
							  (truncated-list->string form))))))))
			    
			    ((catch)
			     (if (and (not (symbol? (cadr form)))
				      (not (boolean? (cadr form)))
				      (or (not (pair? (cadr form)))
					  (not (eq? (caadr form) 'quote))))
				 (lint-format "catch tag ~S is unreliable" 
					      name line-number
					      (cadr form))))
			    
			    (else
			     ;; we've already checked for head in env-member above
			     (check-for-repeated-args name line-number head form env)
			     
			     ;; now try to check arg types for egregious errors
			     (let ((arg-data (hash-table-ref argument-data head)))
			       (if arg-data
				   (check-args name line-number head form arg-data env)
				   )))))))))))
      
      
      (define (get-generator form env) ; defgenerator funcs
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
	    (set! generators (cons name generators))
	    
	    (for-each
	     (lambda (field)
	       (let ((fname (string->symbol 
			     (string-append 
			      (symbol->string name) "-" (symbol->string (if (pair? field)
									    (car field)
									    field))))))
		 (hash-table-set! globals fname (list fname #f #f (list 'lambda (list 'gen))))))
	     (cddr form))
	    (if (and env
		     (pair? (cadr form)))
		(lint-walk name (cdadr form) env)))
	  env))
      
      
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
	     (get-generator form #f))
	    
	    ((if)
	     (if (pair? (cddr form))
		 (if (pair? (cdddr form))
		     (begin
		       (load-walk (cadddr form))
		       (load-walk (caddr form)))
		     (load-walk (caddr form)))))
	    
	    ((load)
	     (if (>= (length form) 2)
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
		  (lint-format "~A binding is not a list? ~S" 
			       name line-number head binding))
	      #f)
	    (if (not (symbol? (car binding)))
		(begin 
		  (if (not second-pass)
		      (lint-format "~A variable is not a symbol? ~S" 
				   name line-number head binding))
		  #f)
		(begin
		  (if (constant? (car binding))
		      (lint-format "can't bind a constant: ~S"
				   name line-number binding))
		  (if (keyword? (car binding))
		      (begin 
			(if (not second-pass)
			    (lint-format "~A variable is a keyword? ~S" 
					 name line-number head binding))
			#f)
		      (if (null? (cdr binding))
			  (begin 
			    (if (not second-pass)
				(lint-format "~A variable value is missing? ~S" 
					     name line-number head binding))
			    #f)
			  (if (and (not (= (length binding) 2))
				   (not (eq? head 'do)))
			      (begin
				(if (not second-pass)
				    (lint-format "~A binding is messed up: ~S" 
						 name line-number head binding))
				#f)
			      (begin
				(if (and *report-shadowed-variables*
					 (not second-pass)
					 (env-member (car binding) env))
				    (lint-format "~A variable ~A in ~S shadows an earlier declaration" 
						 name line-number head (car binding) binding))
				#t))))))))
      
      
      (define (env-difference name e1 e2 lst)
	(if (or (null? e1)
		(null? e2)
		(eq? (car e1) (car e2)))
	    lst
	    (env-difference name (cdr e1) e2 
			    (if (eq? name (caar e1))
				lst
				(cons (car e1) lst)))))
      
      
      (define (report-usage name line-number type head vars)
	;; report unused or set-but-unreferenced variables
	(if (and (not (eq? head 'begin)) ; begin can redefine = set a variable
		 (list? vars)
		 (not (null? vars)))
	    (do ((cur vars (cdr cur))
		 (rst (cdr vars) (cdr rst)))
		((null? rst))
	      (let ((repeat (assq (caar cur) rst)))
		;; not env-member here because the same name might be used as a global
		(if repeat
		    (lint-format "~A ~A ~A is declared twice" name line-number head type (caar cur))))))
	
	(let ((set '())
	      (unused '()))
	  (for-each 
	   (lambda (arg)
	     (if (memq (car arg) '(quote if begin let let* letrec cond case or and do set! 
					 with-environment lambda lambda* define defvar define-envelope
					 define* defmacro defmacro* define-macro define-macro* 
					 define-bacro define-bacro* define-constant))
		 (lint-format "~A ~A named ~A is asking for trouble" name line-number head type (car arg))
		 (if (not (symbol? (car arg)))
		     (lint-format "bad ~A ~A name: ~S" name line-number head type (car arg))))
	     
	     (if *report-undefined-variables*
		 (set! undefined-identifiers (remove (car arg) undefined-identifiers)))
	     
	     (if (and (not (cadr arg))
		      (not (member (car arg) other-identifiers)))
		 (if (caddr arg)
		     (set! set (cons (car arg) set))
		     (set! unused (cons (car arg) unused)))))
	   vars)
	  
	  (if (not (null? set))
	      (lint-format "~A ~A~A ~{~A~^, ~} set, but not used" 
			   name line-number head type (if (> (length set) 1) "s" "") (reverse set)))
	  (if (not (null? unused))
	      (lint-format "~A ~A~A ~{~A~^, ~} not used" 
			   name line-number head type (if (> (length unused) 1) "s" "") (reverse unused)))))
      
      
      (define (lint-walk-body name line-number head body env)
	;; walk a body (a list of forms, the value of the last of which might be returned)
	
	(if (or (not (list? body))
		(negative? (length body)))
	    (lint-format "stray dot? ~A" 
			 name line-number (truncated-list->string body))
	    
	    (let ((ctr 0)
		  (len (length body)))
	      (for-each
	       (lambda (f)
		 (if (< ctr (- len 1)) ; not the last form, so its value is ignored
		     (begin
		       (if (and (pair? f)
				(eq? (car f) 'map))
			   (lint-format "map could be for-each:~A" 
					name line-number 
					(truncated-list->string f)))
		       
		       (if (not (side-effect? f env))
			   (lint-format "this could be omitted:~A" 
					name line-number 
					(truncated-list->string f)))))
		 
		 (if (and (pair? f)
			  (memq head '(defmacro defmacro* define-macro define-macro* define-bacro define-bacro*))
			  (tree-member 'unquote f))
		     (lint-format "~A possibly has too many unquotes:~A"
				  name line-number head
				  (truncated-list->string f)))
		 
		 (set! env (lint-walk name f env))
		 (set! ctr (+ ctr 1)))
	       body)))
	env)
      
      
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
			      (lint-format "docstring is messed up: ~S" name line-number doc)
			      (if (< end doclen)
				  (let* ((arglst (catch #t 
							(lambda ()
							  (eval-string (string-append "'" (substring doc 0 (+ 1 end)))))
							(lambda ignore-catch-error-args 
							  #f)))
					 (keys (if arglst (keywords arglst) 0))
					 (argn (if (or (pair? arglst) 
						       (null? arglst)) 
						   (- (length (proper-list arglst)) keys 1) 
						   0)))
				    (if (and arglst
					     (not (= (length arg-data) argn)))
					(lint-format "possible docstring mismatch:       ~S~%        ~S~%" 
						     name line-number (substring doc 0 (+ end 1)) (append (list name) args))))))))))
	      
	      ;; in any case, skip the docstring during the walk
	      (set! body (cdr body))))
	
	(lint-walk-body name line-number head body env))
      
      
      (define (lint-walk-function head name args val line-number env)
	;; check out function arguments (adding them to the current env), then walk its body, (name == function name, val == body)
	
	;; (format #t "walk function ~A ~A ~A ~A~%" head name args (if (pair? env) (car env) ""))
	(if (null? args)
	    (begin
	      (lint-walk-function-body name line-number head args '() val env)
	      (append (list (list name #f #f (list head args))) env))
	    
	    (if (or (symbol? args) 
		    (pair? args))
		(let ((arg-data (if (symbol? args)                            ; this is getting arg names to add to the environment
				    (list (list args #f #f))
				    (map
				     (lambda (arg)
				       (if (symbol? arg)
					   (if (member arg '(:optional :key :rest :allow-other-keys))
					       (values)                  ; map omits this entry 
					       (list arg #f #f))
					   (if (or (not (pair? arg))
						   (not (= (length arg) 2))
						   (not (memq head '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
					       (begin
						 (lint-format "strange parameter for ~A: ~S" 
							      name line-number head arg)
						 (values))
					       (list (car arg) #f #f))))
				     (proper-list args)))))
		  
		  (lint-walk-function-body name line-number head args arg-data val (append arg-data env))
		  (if *report-unused-parameters* 
		      (report-usage name line-number 'parameter head arg-data))
		  (append (list (list name #f #f (list head args))) env))
		
		(begin
		  (lint-format "strange ~A parameter list ~A" name line-number head args)
		  env))))
      
      
      (define (lint-walk name form env)
	;; walk a form 
	
	(if (symbol? form)
	    (begin
	      (set-ref? form env)
	      env)
	    
	    (if (pair? form)
		(let ((head (car form))
		      (line-number (pair-line-number form)))
		  (case head
		    
		    ;; ---------------- defmacro ----------------
		    ((defmacro defmacro*)
		     (if (or (< (length form) 4)
			     (not (symbol? (cadr form))))
			 (lint-format "~A declaration is messed up: ~S"
				      name line-number head form)
			 (let ((sym (cadr form))
			       (args (caddr form))
			       (body (cdddr form)))
			   (if (and (pair? args)
				    (repeated-member? args env))
			       (lint-format "~A parameter is repeated:~A"
					    name line-number head 
					    (truncated-list->string args)))
			   (lint-walk-function head sym args body line-number env))))
		    
		    ;; ---------------- define ----------------		  
		    ((define define* 
		       define-constant defvar define-envelope
		       define-expansion define-macro define-macro* define-bacro define-bacro*
		       definstrument)
		     
		     (if (< (length form) 2)
			 (begin
			   (lint-format "~S makes no sense" name line-number form)
			   env)
			 (let ((sym (cadr form))
			       (val (cddr form)))
			   
			   (if (symbol? sym)
			       (begin
				 (if (memq head '(define define-constant defvar define-envelope))
				     (let ((len (length form)))
				       (if (not (= len 3))
					   (lint-format "~S has ~A value~A?"
							name line-number form 
							(if (< len 3) "no" "too many") 
							(if (< len 3) "" "s"))))
				     (lint-format "~S is messed up" name line-number form))
				 
				 (if (equal? sym val)
				     (lint-format "this ~A is either not needed, or an error:~A" 
						  name line-number head 
						  (truncated-list->string form)))
				 
				 (if (not (null? (cddr form)))
				     (let ((e (lint-walk sym (caddr form) env)))
					;(format #t "define ~A: ~A~%" sym (car e))
				       (if (and (pair? e)
						(eq? (caar e) sym)) ; (define x (lambda ...)) but it misses closures
					   e
					   (append (list (list sym #f #f)) env)))
				     (append (list (list sym #f #f)) env)))
			       
			       (if (pair? sym)
				   (begin
				     (if (and (pair? (cdr sym))
					      (repeated-member? (proper-list (cdr sym)) env))
					 (lint-format "~A parameter is repeated:~A"
						      name line-number head 
						      (truncated-list->string sym)))
				     
				     (lint-walk-function head (car sym) (cdr sym) val line-number env))
				   
				   (begin
				     (lint-format "strange form: ~S" head line-number form)
				     env))))))
		    
		    ;; ---------------- defgenerator ----------------
		    ((defgenerator)
		     (get-generator form env)
		     env)
		    
		    ;; ---------------- lambda ----------------		  
		    ((lambda lambda*)
		     (if (< (length form) 3)
			 (begin
			   (lint-format "~A is messed up in ~A"
					name line-number head 
					(truncated-list->string form))
			   env)
			 (begin
			   (if (and (pair? (cadr form))
				    (repeated-member? (proper-list (cadr form)) env))
			       (lint-format "~A parameter is repeated:~A"
					    name line-number head 
					    (truncated-list->string (cadr form))))
			   (lint-walk-function head name (cadr form) (cddr form) line-number env))))
		    ;; the lambda case includes stuff like call/cc
		    
		    ;; ---------------- set! ----------------		  
		    ((set!)
		     (if (not (= (length form) 3))
			 (begin
			   (lint-format "set! has too ~A arguments: ~S" 
					name line-number 
					(if (> (length form) 3) "many" "few") 
					form)
			   env)
			 
			 (let ((settee (cadr form))
			       (setval (caddr form)))
			   (if (pair? settee)
			       (begin
				 (if (and *report-minor-stuff*
					  (memq (car settee) '(vector-ref list-ref string-ref hash-table-ref)))
				     (lint-format "~A as target of set!~A"
						  name line-number (car settee)
						  (truncated-list->string form)))
				 (lint-walk name settee env) ; this counts as a reference since it's by reference so to speak
				 (set! settee (do ((sym (car settee) (car sym)))
						  ((not (pair? sym)) sym)))))
			   (if (symbol? settee)
			       (begin
				 (if (constant? settee)
				     (lint-format "can't set! a constant:~A"
						  name line-number 
						  (truncated-list->string form)))
				 (set-set? settee env)))
			   
			   (if (and (symbol? (cadr form))
				    (equal? (cadr form) (caddr form))) ; not settee and setval here!
			       (lint-format "pointless set!~A" 
					    name line-number 
					    (truncated-list->string form)))
			   
			   (lint-walk name setval env))))
		    
		    ;; ---------------- quote ----------------		  
		    ((quote) 
		     (let ((len (length form)))
		       (if (negative? len)
			   (lint-format "stray dot in quote's arguments? ~S"
					name line-number form)
			   (if (not (= len 2))
			       (lint-format "quote has too ~A arguments: ~S" 
					    name line-number 
					    (if (> (length form) 2) "many" "few") 
					    form)
			       (if (and *report-minor-stuff*
					(or (number? (cadr form))
					    (boolean? (cadr form))
					    (string? (cadr form))))
				   (lint-format "quote is not needed here:~A"
						name line-number (truncated-list->string form))))))
		     env)
		    
		    ;; ---------------- cond ----------------
		    ((cond)
		     (let ((ctr 0)
			   (len (- (length form) 1)))
		       (if (negative? len)
			   (lint-format "cond is messed up:~A" 
					name line-number
					(truncated-list->string form))
			   (for-each
			    (lambda (clause)
			      (set! ctr (+ ctr 1))
			      (if (not (pair? clause))
				  (lint-format "cond clause is messed up: ~A"
					       name line-number
					       (truncated-list->string clause))
				  (let ((expr (simplify-boolean (car clause) '() '() env)))
				    (if (boolean? expr)
					(if (not expr)
					    (lint-format "cond clause will never be evaluated:~A"
							 name line-number 
							 (truncated-list->string clause))
					    (if (not (= ctr len))
						(lint-format "cond #t clause is not the last: ~A"
							     name line-number 
							     (truncated-list->string form))))
					(if (eq? (car clause) 'else)
					    (if (not (= ctr len))
						(lint-format "cond else clause is not the last: ~A"
							     name line-number 
							     (truncated-list->string form)))
					    (lint-walk name (car clause) env)))
				    (if (pair? (cdr clause))
					(if (eq? (cadr clause) '=>)
					    (if (not (pair? (cddr clause)))
						(lint-format "cond => target is messed up: ~A"
							     name line-number
							     (truncated-list->string clause))
						(lint-walk name (caddr clause) env))
					    (lint-walk-body name line-number head (cdr clause) env))
					(if (not (null? (cdr clause)))
					    (lint-format "cond clause is messed up: ~A"
							 name line-number
							 (truncated-list->string clause)))))))
			    (cdr form)))
		       env))
		    
		    
		    ;; ---------------- case ----------------		  
		    ((case)
		     ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		     (if (< (length form) 3)
			 (lint-format "case is messed up: ~A"
				      name line-number 
				      (truncated-list->string form))
			 (begin
			   (if (and (not (pair? (cadr form)))
				    (constant? (cadr form)))
			       (lint-format "case selector is a constant: ~A"
					    name line-number
					    (truncated-list->string form)))
			   (lint-walk name (cadr form) env) ; the selector
			   (let ((all-keys '())
				 (ctr 0)
				 (len (length (cddr form))))
			     (for-each
			      (lambda (clause)
				(set! ctr (+ ctr 1))
				(if (not (pair? clause))
				    (lint-format "case clause should be a list: ~A"
						 name line-number 
						 (truncated-list->string clause))
				    (let ((keys (car clause))
					  (exprs (cdr clause)))
				      (if (pair? keys)
					  (if (negative? (length keys))
					      (lint-format "stray dot in case case key list: ~A"
							   name line-number 
							   (truncated-list->string clause))
					      (for-each
					       (lambda (key)
						 (if (or (vector? key)
							 (string? key)
							 (list? key)
							 (hash-table? key)
							 (hook? key))
						     (lint-format "case key ~S in ~S is unlikely to work (case uses eqv?)" 
								  name line-number key clause))
						 (if (member key all-keys)
						     (lint-format "repeated case key ~S in ~S" 
								  name line-number key clause)))
					       keys))
					  (if (not (eq? keys 'else))
					      (lint-format "bad case key ~S in ~S" 
							   name line-number keys clause)
					      (if (not (= ctr len))
						  (lint-format "case else clause is not the last:~A"
							       name line-number 
							       (truncated-list->string (cddr form))))))
				      (set! all-keys (append (if (and (pair? keys)
								      (not (negative? (length keys))))
								 keys 
								 (list keys))
							     all-keys))
				      (lint-walk-body name line-number head exprs env))))
			      (cddr form)))))
		     env)
		    
		    ;; ---------------- do ----------------		  
		    ((do)
		     (let ((vars '()))
		       (if (or (< (length form) 3)
			       (not (list? (cadr form)))
			       (not (list? (caddr form))))
			   (lint-format "do is messed up: ~A" 
					name line-number 
					(truncated-list->string form))
			   
			   (let ((step-vars (cadr form)))
			     
			     ;; walk the init forms before adding the step vars to env
			     (do ((bindings step-vars (cdr bindings)))
				 ((null? bindings))
			       (if (binding-ok? name line-number head (car bindings) env #f)
				   (begin
				     (lint-walk name (cadar bindings) env)
				     (set! vars (append (list (list (caar bindings) #f #f () (->type (cadar bindings)))) vars)))))
			     
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
		     (if (< (length form) 3)
			 (lint-format "let is messed up: ~A" 
				      name line-number 
				      (truncated-list->string form))
			 (let ((named-let (if (symbol? (cadr form)) (cadr form) #f)))
			   (let ((vars (if named-let (list (list named-let #f #f)) '())))
			     (do ((bindings (if named-let (caddr form) (cadr form)) (cdr bindings)))
				 ((or (not (list? bindings))
				      (null? bindings)))
			       (if (binding-ok? name line-number head (car bindings) env #f)
				   (begin
				     (if (and (not (env-member (caar bindings) env))
					      (pair? (cadar bindings))
					      (eq? 'lambda (car (cadar bindings)))
					      (tree-car-member (caar bindings) (cadar bindings)))
					 (lint-format "let variable ~A is called in its binding?  perhaps let should be letrec:~A"
						      name line-number (caar bindings) 
						      (truncated-list->string bindings)))
				     (lint-walk name (cadar bindings) env)

				     ;; can we tell its type and (as long as not set) check for type errors?
				     ;; need a function that turns a constant into a type indication,
				     ;;   then append that as the 4th entry below (unused currently I think)
				     ;;   then use that in arg checks if arg is a known var

				     ; (format #t "~S: ~S~%" (car bindings) (->type (cadar bindings)))
					 
				     (set! vars (append (list (list (caar bindings) #f #f () (->type (cadar bindings)))) vars)))))
			     ;; each var is (sym ref set opt-func-data opt-type-data)
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name line-number head (if named-let (cdddr form) (cddr form)) cur-env))
				    (nvars (and (not (eq? e cur-env))
						(env-difference name e cur-env '()))))
			       (if (pair? nvars)
				   (set! vars (append nvars vars))))
			     (report-usage name line-number 'variable head vars))))
		     env)
		    
		    ;; ---------------- let* ----------------		  
		    ((let*)
		     (if (< (length form) 3)
			 (lint-format "let* is messed up: ~A" 
				      name line-number 
				      (truncated-list->string form))
			 (let ((vars '()))
			   (do ((bindings (cadr form) (cdr bindings)))
			       ((or (not (list? bindings))
				    (null? bindings)))
			     (if (binding-ok? name line-number head (car bindings) env #f)
				 (begin
				   (lint-walk name (cadar bindings) (append vars env))
				   (set! vars (append (list (list (caar bindings) #f #f () (->type (cadar bindings)))) vars)))))
			   
			   (if (and *report-minor-stuff* ; maybe we need *report-very-minor-stuff* !
				    (call-with-exit
				     (lambda (return)
				       (for-each
					(lambda (v)
					  (if (list-ref v 1) ; refd?
					      (return #f)))
					vars)
				       #t)))
			       (lint-format "let* could be let:~A" 
					    name line-number 
					    (truncated-list->string form)))
			   
			   (let* ((cur-env (append vars env))
				  (e (lint-walk-body name line-number head (cddr form) cur-env))
				  (nvars (and (not (eq? e cur-env))
					      (env-difference name e cur-env '()))))
			     (if (pair? nvars)
				 (set! vars (append nvars vars))))
			   
			   (report-usage name line-number 'variable head vars)))
		     env)
		    
		    ;; ---------------- letrec ----------------		  
		    ((letrec letrec*)
		     (if (< (length form) 3)
			 (lint-format "~A is messed up: ~A" 
				      name line-number head
				      (truncated-list->string form))
			 (let ((vars '()))
			   (if (null? (cadr form))
			       (lint-format "~A could be let:~A"
					    name line-number head 
					    (truncated-list->string form)))
			   (do ((bindings (cadr form) (cdr bindings)))
			       ((or (not (list? bindings))
				    (null? bindings)))
			     (if (binding-ok? name line-number head (car bindings) env #f)
				 (set! vars (append (list (list (caar bindings) #f #f () (->type (cadar bindings)))) vars))))
			   (let ((new-env (append vars env)))
			     (do ((bindings (cadr form) (cdr bindings)))
				 ((or (not (list? bindings))
				      (null? bindings)))
			       (if (binding-ok? name line-number head (car bindings) env #t)
				   (lint-walk name (cadar bindings) new-env)))
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name line-number head (cddr form) cur-env))
				    (nvars (and (not (eq? e cur-env))
						(env-difference name e cur-env '()))))
			       (if (pair? nvars)
				   (set! vars (append nvars vars)))))
			   
			   (report-usage name line-number 'variable head vars)))
		     env)
		    
		    ;; ---------------- begin ----------------
		    ((begin)
		     (if (negative? (length form))
			 (begin
			   (lint-format "stray dot in begin? ~A"
					name line-number
					(truncated-list->string form))
			   env)
			 (let* ((ctr 0)
				(body (cdr form))
				(len (length body))
				(vars env))
			   (for-each
			    (lambda (f)
			      (if (< ctr (- len 1))
				  (if (and (pair? f)
					   (eq? (car f) 'map))
				      (lint-format "map could be for-each:~A" 
						   name line-number 
						   (truncated-list->string f))
				      (if (not (side-effect? f env))
					  (lint-format "this could be omitted:~A"
						       name line-number
						       (truncated-list->string f)))))
			      
			      (if (and (pair? f)
				       (eq? (car f) 'begin))
				  (lint-format "redundant begin:~A"
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
		     (if (< (length form) 3)
			 (begin
			   (if (< (length form) 2)
			       (lint-format "~A has too few arguments:~A"
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
					     (if (and (not (memq c '(#\~ #\T #\t #\& #\% #\^ #\newline #\}))) ; ~* consumes an arg
						      (not (call-with-exit
							    (lambda (return)
							      (do ((k i (+ k 1)))
								  ((= k len) #f)
								;; this can be confused by pad chars in ~T
								(if (and (not (char-numeric? (string-ref str k)))
									 (not (char=? (string-ref str k) #\,)))
								    (return (char-ci=? (string-ref str k) #\t))))))))
						 (let ((dir (string-ref str i)))
						   ;; the possibilities are endless, so I'll stick to the simplest
						   (if (and (not (char-numeric? dir))
							    (not (memq dir '(#\A #\S #\C #\F #\E #\G #\O #\D #\B #\X #\, #\{ #\} #\@ #\P #\*
									     #\a #\s #\c #\f #\e #\g #\o #\d #\b #\x #\p))))
						       (lint-format "unrecognized format directive: ~C in ~S, ~S"
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
				   (lint-format "~A control string ends in tilde:~A"
						name line-number head
						(truncated-list->string form)))
			       
			       (if (not (= curlys 0))
				   (lint-format "~A has ~D unmatched ~A~A:~A"
						name line-number head 
						(abs curlys) 
						(if (positive? curlys) "{" "}") 
						(if (> curlys 1) "s" "") 
						(truncated-list->string form)))
			       dirs))
			   
			   (if (not (string? control-string))
			       (if (not (list? args))
				   (lint-format "~S looks suspicious" 
						name line-number form))
			       (let ((ndirs (count-directives control-string name line-number form))
				     (nargs (if (or (null? args) (pair? args)) (length args) 0)))
				 (if (not (= ndirs nargs))
				     (lint-format "~A has ~A arguments:~A" 
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
			    (if (not (env-member (car decl) env))
				(lint-format "run declare statement variable name ~A is unknown: ~S"
					     name line-number (car decl) form))))
		      (cdr form))
		     env)
		    
		    ((with-environment)
		     (if (< (length form) 3)
			 (lint-format "with-environment is messed up: ~A" 
				      name line-number 
				      (truncated-list->string form))
			 (let* ((e (lint-walk-body name line-number head (cddr form) env))
				(vars (if (not (eq? e env))
					  (env-difference name e env '())
					  '())))
			   (report-usage name line-number 'variable head vars)))
		     env)
		    
		    ;; ---------------- everything else ----------------		  
		    (else
		     
		     (if (negative? (length form))
			 (begin
			   (lint-format "stray dot? ~A" 
					name line-number 
					(truncated-list->string form))
			   env)
			 (begin
			   (check-call name line-number head form env)
			   (if (not (env-member head env))
			       (check-special-cases name line-number head form env))
			   
			   (if (and *report-minor-stuff*
				    (not (= line-number last-simplify-numeric-line-number))
				    (not (env-member head env))
				    (numeric? head)
				    (not (null? (cdr form))))
			       (let ((val (simplify-numerics form env)))
				 (set! last-simplify-numeric-line-number line-number)
				 (if (not (equal? form val))
				     (lint-format "possible simplification:~A"
						  name line-number 
						  (lists->string form val)))))
			   
			   ;; walk everything looking for undefined vars (saved until we finish the file).
			   ;;
			   ;;   if we loaded this file first, and f (head) is defined (e.g. scan above),
			   ;;   and it is used before it is defined, but not thereafter, the usage stuff 
			   ;;   can get confused, so other-identifiers is trying to track those.
			   
			   (if (and (symbol? head)
				    (not (member head other-identifiers))
				    start-up-environment ; not s7?
				    (not (defined? head start-up-environment)))
			       (set! other-identifiers (cons head other-identifiers)))
			   
			   (let ((vars env))
			     (for-each
			      (lambda (f)
				;; look for names we don't know about
				(if (and *report-undefined-variables*
					 (symbol? f)
					 (not (keyword? f))
					 (not (eq? f name))
					 (not (eq? f '=>))
					 (not (env-member f vars)))
				    (if (and (not (defined? f))
					     (not (assq f undefined-identifiers)))
					(set! undefined-identifiers (cons (list f name line-number 
										(truncated-list->string form)) 
									  undefined-identifiers))))
				(set! vars (lint-walk name f vars)))
			      form))
			   ))
		     env)))
		
		;; else form is not a symbol and not a pair
		env)))
      
    ;;; --------------------------------------------------------------------------------
      
      (lambda (file . args)
	"(lint file) looks for infelicities in file's scheme code"
	(set! *current-file* file)
	(set! undefined-identifiers '())
	(set! globals (make-hash-table))
	(set! loaded-files '())
	(set! other-identifiers '())
	
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
		
		(if *report-multiply-defined-top-level-functions*
		    (for-each
		     (lambda (var)
		       (let ((var-file (hash-table-ref *top-level-objects* (car var))))
			 (if (not var-file)
			     (hash-table-set! *top-level-objects* (car var) *current-file*)
			     (if (not (string=? var-file *current-file*))
				 (format #t ";~S is defined at the top level in ~S and ~S~%" (car var) var-file *current-file*)))))
		     vars))

		(if *report-unused-top-level-functions* 
		    (begin
		      (report-usage file 0 'top-level-object #f vars)
		      (if (not (null? generators))
			  (let ((descr `(lambda (gen)))
				(set '())
				(unused '()))
			    (for-each
			     (lambda (gen)
			       ;; look for unused fields
			       (let* ((giter (make-hash-table-iterator globals))
				      (field-prefix (string-append (symbol->string gen) "-"))
				      (prefix-len (string-length field-prefix)))
				 (do ((gfield (giter) (giter)))
				     ((null? gfield))
				   (if (not (memq (car gfield) other-identifiers)) 
				       (begin
					 (set! gfield (cdr gfield))
					 (if (and (= (length gfield) 4)
						  (not (cadr gfield))
						  (equal? (cadddr gfield) descr)
						  (let ((symstr (symbol->string (car gfield))))
						    (and (> (string-length symstr) prefix-len)
							 (string=? (substring symstr 0 prefix-len) field-prefix))))
					     (if (caddr gfield)
						 (set! set (cons (car gfield) set))
						 (set! unused (cons (car gfield) unused)))))))))
			     generators)
			    (if (not (null? set))
				(format #t "  generator fields set, but not used: ~{~A~^, ~}~%" set))
			    (if (not (null? unused))
				(format #t "  unused generator fields: ~{~A~^, ~}~%" unused))))))
		
		(if *report-undefined-variables*
		    (for-each
		     (lambda (var)
		       (if (not (env-member (car var) vars))
			   (lint-format "undefined identifier ~A in:~A"
					(list-ref var 1)
					(list-ref var 2)
					(list-ref var 0)
					(list-ref var 3))))
		     undefined-identifiers))
		
		(close-input-port fp))))))))



;;; TODO: nonce words that look like misspellings should be reported whatever the undefined-variables switch is
