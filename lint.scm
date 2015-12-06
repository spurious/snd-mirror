;;; lint for s7 scheme
;;;
;;; (lint "file.scm") checks file.scm for infelicities
;;; to control the kinds of checks, set the variables below.

(provide 'lint.scm)

(define *report-unused-parameters* #f)
(define *report-unused-top-level-functions* #f)
(define *report-multiply-defined-top-level-functions* #f) ; same name defined at top level in more than one file
(define *report-shadowed-variables* #f)
(define *report-minor-stuff* #t)                          ; now obsolete (#t)
(define *report-doc-strings* #f)                          ; report old-style (CL) doc strings
(define *load-file-first* #f)                             ; this will actually load the file, so errors will stop lint

(if (provided? 'pure-s7)
    (begin
      (define (make-polar mag ang) (complex (* mag (cos ang)) (* mag (sin ang))))

      (define (memq a b) (member a b eq?))
      (define (memv a b) (member a b eqv?))
      (define (assq a b) (assoc a b eq?))
      (define (assv a b) (assoc a b eqv?))

      (define (char-ci=? . chars) (apply char=? (map char-upcase chars)))
      (define (char-ci<=? . chars) (apply char<=? (map char-upcase chars)))
      (define (char-ci>=? . chars) (apply char>=? (map char-upcase chars)))
      (define (char-ci<? . chars) (apply char<? (map char-upcase chars)))
      (define (char-ci>? . chars) (apply char>? (map char-upcase chars)))

      (define (string-ci=? . strs) (apply string=? (map string-upcase strs)))
      (define (string-ci<=? . strs) (apply string<=? (map string-upcase strs)))
      (define (string-ci>=? . strs) (apply string>=? (map string-upcase strs)))
      (define (string-ci<? . strs) (apply string<? (map string-upcase strs)))
      (define (string-ci>? . strs) (apply string>? (map string-upcase strs)))

      (define (let->list e)
	(if (let? e)
	    (reverse! (map values e))
	    (error 'wrong-type-arg "let->list argument should be an environment: ~A" str)))
      ))

(define start-up-let (rootlet))
(define *current-file* "")
(define *top-level-objects* (make-hash-table))
(define *lint-output-port* *stderr*)

(format *stderr* "loading lint.scm~%")
(set! reader-cond #f)
(define-macro (reader-cond . clauses) `(values))          ; clobber reader-cond to avoid dumb unbound-variable errors


;;; --------------------------------------------------------------------------------
;;; for snd-test.scm


(set! *#readers* 
      (cons (cons #\_ (lambda (str)
			(and (string=? str "__line__")
			     (port-line-number))))
	    *#readers*))

(unless (provided? 'snd)
  (define defanimal define*)
  (define definstrument define*)
  (define defgenerator define*))


;;; --------------------------------------------------------------------------------


(define lint
  (let ()
    (define applicable? arity)

    (define every? 
      (let ((documentation "(every? func sequence) returns #t if func approves of every member of sequence"))
	(lambda (f sequence)
	  (not (member #f sequence (lambda (a b) (not (f b))))))))

    (define any? 
      (let ((documentation "(any? func sequence) returns #t if func approves of any member of sequence"))
	(lambda (f sequence)
	  (member #f sequence (lambda (a b) (f b))))))

    (define collect-if 
      (let ((documentation "(collect-if type func sequence) gathers the elements of sequence that satisfy func, and returns them via type:\n\
              (collect-if list integer? #(1.4 2/3 1 1+i 2)) -> '(1 2)"))
	(lambda (type f sequence)
	  (apply type (map (lambda (arg) (if (f arg) arg (values))) sequence)))))

    (define find-if 
      (let ((documentation "(find-if func sequence) applies func to each member of sequence.\n\
              If func approves of one, find-if returns that member of the sequence"))
	(lambda (f sequence)
	  (call-with-exit
	   (lambda (return)
	     (for-each (lambda (arg)
			 (if (f arg)
			     (return arg)))
		       sequence)
	     #f)))))

    (define copy-tree 
      (let ((documentation "(copy-tree lst) returns a full copy of lst"))
	(lambda (lis)
	  (if (pair? lis)
	      (cons (copy-tree (car lis))
		    (copy-tree (cdr lis)))
	      lis))))

    (require write.scm)

    (define (any-real? lst) ; ignore 0.0 and 1.0 in this since they normally work
      (and (pair? lst)
	   (or (and (number? (car lst))
		    (not (rational? (car lst)))
		    (not (= (car lst) 0.0))
		    (not (= (car lst) 1.0)))
	       (any-real? (cdr lst)))))
    
    (define (quoted-undotted-pair? x)
      (and (pair? x)
	   (eq? (car x) 'quote)
	   (pair? (cdr x))
	   (pair? (cadr x))
	   (positive? (length (cadr x)))))

    (define (quoted-null? x)
      (and (pair? x)
	   (eq? (car x) 'quote)
	   (pair? (cdr x))
	   (null? (cadr x))))

    (define (code-constant? x)
      (and (not (symbol? x))
	   (or (not (pair? x))
	       (eq? (car x) 'quote))))

    (define (eqv-code-constant? x)
      (or (number? x)
	  (char? x)
	  (boolean? x)
	  (and (pair? x)
	       (eq? (car x) 'quote)
	       (or (symbol? (cadr x))
		   (null? (cadr x))))
	  (memq x '(#<unspecified> #<undefined> #<eof>))))

    (let ((no-side-effect-functions 
	   ;; ideally we'd be able to add functions to this list, perhaps similar to the signatures
	   (let ((ht (make-hash-table)))
	     (for-each
	      (lambda (op) 
		(hash-table-set! ht op #t))
	      '(* + - / < <= = > >= 
		abs acos acosh and angle append aritable? arity ash asin asinh assoc assq assv atan atanh 
		begin boolean=? boolean? byte-vector byte-vector?
		caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
		call-with-input-from-string call-with-input-from-file
		c-pointer c-pointer? c-object? call-with-exit car case catch cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
		cddar cdddar cddddr cdddr cddr cdr ceiling char->integer char-alphabetic? char-ci<=?
		char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric? 
		char-position char-ready? char-upcase char-upper-case? char-whitespace? char<=? char<?
		char=? char>=? char>? char? complex complex? cond cons constant? continuation? cos
		cosh curlet current-error-port current-input-port current-output-port cyclic-sequences
		defined? denominator dilambda? do dynamic-wind
		eof-object? eq? equal? eqv? even? exact->inexact exact? exp expt
		float? float-vector float-vector-ref float-vector? floor for-each funclet 
		gcd gensym gensym?
		hash-table hash-table* hash-table-entries hash-table-ref hash-table? help hook-functions
		if imag-part inexact->exact inexact? infinite? inlet input-port? 
		int-vector int-vector-ref int-vector? iterator-at-end? iterator-sequence integer->char
		integer-decode-float integer-length integer? iterator?
		keyword->symbol keyword?
		let->list lcm length let let* let-ref let? letrec letrec* list list->string list->vector list-ref
		list-tail list? log logand logbit? logior lognot logxor
		macro? magnitude make-byte-vector make-float-vector make-int-vector make-hash-table make-hook make-iterator make-keyword make-list make-polar
		make-rectangular make-shared-vector make-string make-vector map max member memq memv min modulo morally-equal?
		nan? negative? not null? number->string number? numerator
		object->string odd? openlet? or outlet output-port? owlet
		pair-line-number pair? peek-char port-closed? port-filename port-line-number positive? procedure-documentation
		procedure-setter procedure-signature procedure-source procedure? proper-list? provided?
		quasiquote quote quotient
		random-state random-state->list random-state? rational? rationalize real-part real? remainder reverse rootlet round
		s7-version sequence? sin sinh sqrt stacktrace string string->list string->number string->symbol string-append 
		string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-downcase string-length
		string-position string-ref string-upcase string<=? string<? string=? string>=? string>? string?
		sublet substring symbol symbol->dynamic-value symbol->keyword symbol->string symbol->value symbol=? symbol?
		tan tanh truncate
		unless unlet
		vector vector-append vector->list vector-dimensions vector-length vector-ref vector?
		when with-baffle with-let with-input-from-file with-input-from-string with-output-to-string
		zero?))
	     ;; do not include file-exists? or directory?
	     ht))

	  (deprecated-ops '((global-environment . rootlet)
			    (current-environment . curlet)
			    (make-procedure-with-setter . dilambda)
			    (procedure-with-setter? . dilambda?)
			    (make-random-state . random-state)
			    ;(make-rectangular . complex)

			    (data-format . sample-type)
			    (mus-sound-data-format . mus-sound-sample-type)
			    (mus-data-format-name . mus-sample-type-name)
			    (mus-data-format->string . mus-sample-type->string)))

	  (numeric-ops (let ((h (make-hash-table)))
			 (for-each
			  (lambda (op)
			    (set! (h op) #t))
			  '(+ * - / 
			      sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh 
			      log exp expt sqrt make-polar complex
			      imag-part real-part abs magnitude angle max min exact->inexact
			      modulo remainder quotient lcm gcd
			      rationalize inexact->exact random
			      logior lognot logxor logand numerator denominator 
			      floor round truncate ceiling ash))
			 h))

	  (repeated-args-table (let ((h (make-hash-table)))
				 (for-each
				  (lambda (op)
				    (set! (h op) #t))
				  '(= / max min < > <= >= - quotient remainder modulo and or
				      string=? string<=? string>=? string<? string>?
				      char=? char<=? char>=? char<? char>?
				      boolean=? symbol=?))
				 h))
	  
	  (repeated-args-table-2 (let ((h (make-hash-table)))
				   (for-each
				    (lambda (op)
				      (set! (h op) #t))
				    '(= max min < > <= >= and or
					string=? string<=? string>=? string<? string>?
					char=? char<=? char>=? char<? char>?
					boolean=? symbol=?))
				   h))

	  (syntaces (let ((h (make-hash-table)))
		      (for-each
		       (lambda (op)
			 (set! (h op) #t))
		       '(quote if begin let let* letrec letrec* cond case or and do set! unless when
			 with-let with-baffle
			 lambda lambda* define define* 
			 define-macro define-macro* define-bacro define-bacro* 
			 define-constant define-expansion))
		      h))
	  
	  (format-control-char (let ((chars (make-vector 256 #f)))
				 (for-each
				  (lambda (c)
				    (vector-set! chars (char->integer c) #t))
				  '(#\A #\S #\C #\F #\E #\G #\O #\D #\B #\X #\, #\{ #\} #\@ #\P #\*
				    #\a #\s #\c #\f #\e #\g #\o #\d #\b #\x #\p #\N #\n #\W #\w #\v #\V
				    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
				 chars))

	  (selector-types '(#t symbol? char? boolean? integer? rational? real? complex? number?))
	  (outport #t)
	  (loaded-files #f)
	  (globals #f)
	  (*e* (curlet))
	  (other-identifiers #f)
	  (quote-warnings 0)
	  (last-simplify-boolean-line-number -1)
	  (last-simplify-numeric-line-number -1)
	  (last-if-line-number -1)
	  (last-checker-line-number -1)
	  (line-number -1))


      (define var-name car)
      (define var-ref (dilambda (lambda (v) (let-ref (cdr v) 'ref)) (lambda (v x) (let-set! (cdr v) 'ref x))))
      (define var-set (dilambda (lambda (v) (let-ref (cdr v) 'set)) (lambda (v x) (let-set! (cdr v) 'set x))))
      (define var-type (dilambda (lambda (v) (let-ref (cdr v) 'type)) (lambda (v x) (let-set! (cdr v) 'type x))))
      (define var-value (dilambda (lambda (v) (let-ref (cdr v) 'value)) (lambda (v x) (let-set! (cdr v) 'value x))))

      (define var-ftype (dilambda (lambda (v) (let-ref (cdr v) 'ftype)) (lambda (v x) (let-set! (cdr v) 'ftype x))))
      (define var-side-effect (dilambda (lambda (v) (let-ref (cdr v) 'side-effect)) (lambda (v x) (let-set! (cdr v) 'side-effect x))))
      (define var-arglist (dilambda (lambda (v) (let-ref (cdr v) 'arglist)) (lambda (v x) (let-set! (cdr v) 'arglist x))))
      (define var-signature (dilambda (lambda (v) (let-ref (cdr v) 'signature)) (lambda (v x) (let-set! (cdr v) 'signature x))))

      (define (var? v) (and (pair? v) (let? (cdr v))))
      (define var-member assq)

      (define* (make-var name type value)
	;(inlet 'name name 'set #f 'ref #f 'type type 'value value)
	(cons name (inlet 'value value 'type type 'set #f 'ref #f)))

      (define* (make-fvar name ftype arglist decl value location env)
	(cons name 
	      (inlet 'type 'closure
		     'signature (let ((body (and (memq ftype '(define define* lambda lambda*))
						 (cddr value))))
				  ;(format *stderr* "~A ~A -> ~A~%" name value body)
				  (if (and (pair? body)
					   (null? (cdr body)))
				      (if (not (pair? (car body)))
					  (and (not (symbol? (car body)))
					       (list (->type (car body)) #t))
					  (let ((sig (and (symbol? (caar body))
							  (let ((f (symbol->value (caar body) *e*)))
							    (if (procedure? f)
								(procedure-signature f)
								(let ((fd (or (var-member (caar body) env)
									      (hash-table-ref globals (caar body)))))
								  (and (var? fd)
								       (symbol? (var-ftype fd))
								       (list? (var-signature fd))
								       (var-signature fd))))))))
					    (when sig
					      ;; we know the new fvar's return type is (car sig)
					      (let ((new-sig (list (car sig))))
						;(format *stderr* "args: ~A, sig: ~A, body: ~A~%" arglist sig body)
						new-sig))))))
#|
						(for-each
						 (lambda (arg)

						  ;; for each arg, search body and correlate to arg type
|#

		     'side-effect (or (not (memq ftype '(define define* lambda lambda*)))
				      (any? (lambda (f) (side-effect? f env)) (cddr value)))
		     'allow-other-keys (and (pair? arglist)
					    (memq ftype '(define* define-macro* define-bacro* defmacro*))
					    (eq? (last-par arglist) 'allow-other-keys))
		     'env env
		     'value value
		     'location location
		     'decl decl
		     'arglist arglist
		     'ftype ftype
		     'set #f 
		     'ref #f)))
      
      (define (return-type sym e)
	(let ((f (if (symbol? sym) (symbol->value sym *e*) sym))) ; backwards -- should lookup locals first
	  (if (procedure? f)
	      (let ((sig (procedure-signature f)))
		(and (pair? sig)
		     (or (eq? (car sig) 'values) ; turn it into #t for now
			 (car sig))))        ; this might be undefined in the current context (eg oscil? outside clm)
	      (let ((fd (or (var-member sym e)
			    (hash-table-ref globals sym))))
		(and (var? fd)
		     (symbol? (var-ftype fd))
		     (list? (var-signature fd))
		     (or (eq? (car (var-signature fd)) 'values)
			 (car (var-signature fd))))))))
	      
      (define (->type c)
	(cond ((pair? c)
	       (if (symbol? (car c))
		   (if (eq? (car c) 'quote)
		       (->type (cadr c))
		       (return-type (car c) ()))
		   (or (pair? (car c)) 'pair?)))
	      ((integer? c)	  'integer?)
	      ((rational? c)	  'rational?)
	      ((real? c)	  'real?)
	      ((number? c)	  'number?)
	      ((keyword? c)	  'keyword?)
	      ((symbol? c)	  'symbol?)
	      ((byte-vector? c)   'byte-vector?)
	      ((string? c)	  'string?)
	      ((null? c)	  'null?)
	      ((char? c)	  'char?)
	      ((boolean? c)	  'boolean?)
	      ((float-vector? c)  'float-vector?)
	      ((int-vector? c)	  'int-vector?)
	      ((vector? c)	  'vector?)
	      ((let? c)		  'let?)
	      ((hash-table? c)	  'hash-table?)
	      ((input-port? c)	  'input-port?)
	      ((output-port? c)   'output-port?)
	      ((iterator? c)	  'iterator?)
	      ((continuation? c)  'continuation?)
	      ((dilambda? c)	  'dilambda?)
	      ((procedure? c)	  'procedure?)
	      ((macro? c)         'macro?)
	      ((random-state? c)  'random-state?)
	      ((eof-object? c)    'eof-object?)
	      ((c-pointer? c)     'c-pointer?)
	      (#t #t)))
      
      (define bools '(symbol? integer? rational? real? number? complex? float? keyword? gensym? byte-vector? string? list?
		      char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair?
		      output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?))

      (define (compatible? type1 type2) ; we want type1, we have type2 -- is type2 ok?
	;(format *stderr* "compatible ~S ~S~%" type1 type2)
	(or (eq? type1 type2)
	    (not (symbol? type1))
	    (not (symbol? type2))
	    (not (memq type1 bools))
	    (not (memq type2 bools))
	    (case type1
	      ((number? complex?)  (memq type2 '(float? real? rational? integer? number? complex?)))
	      ((real?)             (memq type2 '(float? rational? integer? complex? number?)))
	      ((float?)            (memq type2 '(real? complex? number?)))
	      ((rational?)         (memq type2 '(integer? real? complex? number?)))
	      ((integer?)          (memq type2 '(real? rational? complex? number?)))
	      ((vector?)           (memq type2 '(float-vector? int-vector?)))
	      ((float-vector? int-vector?) (eq? type2 'vector?))
	      ((symbol?)           (memq type2 '(gensym? keyword?)))
	      ((keyword? gensym?)  (eq? type2 'symbol?))
	      ((list?)             (memq type2 '(null? pair?)))
	      ((pair? null?)       (eq? type2 'list?))
	      ((dilambda?)         (memq type2 '(procedure? macro? iterator?)))
	      ((procedure? macro?) (memq type2 '(dilambda? iterator?)))
	      ((iterator?)         (memq type2 '(dilambda? procedure?)))
	      ((string?)           (eq? type2 'byte-vector?))
	      ((byte-vector?)      (eq? type2 'string?))
	      (else #f))))

      (define (any-compatible? type1 type2)
	;; type1 and type2 can be either a list of types or a type
	(if (symbol? type1)
	    (if (symbol? type2)
		(compatible? type1 type2)
		(and (pair? type2)
		     (or (compatible? type1 (car type2))
			 (any-compatible? type1 (cdr type2)))))
	    (and (pair? type1)
		 (or (compatible? (car type1) type2)
		     (any-compatible? (cdr type1) type2)))))

      (define (subsumes? type1 type2)
	(or (eq? type1 type2)
	    (case type1
	      ((rational?)        (eq? type2 'integer?))
	      ((real?)            (memq type2 '(integer? rational? float?)))
	      ((complex? number?) (memq type2 '(integer? rational? float? real? complex? number?)))
	      ((list?)            (memq type2 '(pair? null? proper-list?)))
	      ((pair?)            (eq? type2 'proper-list?))
	      ((vector?)          (memq type2 '(float-vector? int-vector?)))
	      ((symbol?)          (memq type2 '(keyword? gensym?)))
	      (else #f))))

      (define (any-checker? types arg)
	(if (and (symbol? types)
		 (not (eq? types 'values)))
	    ((symbol->value types *e*) arg)
	    (and (pair? types)
		 (or (any-checker? (car types) arg)
		     (any-checker? (cdr types) arg)))))

      (define (never-false expr)
	(or (eq? expr #t)
	    (let ((type (if (pair? expr)
			    (and (hash-table-ref no-side-effect-functions (car expr))
				 (return-type (car expr) ()))
			    (->type expr))))
	      (and (symbol? type)
		   (not (symbol? expr))
		   (not (memq type '(boolean? values)))))))
	
      (define (never-true expr)
	(or (not expr)
	    (and (pair? expr)
		 (eq? (car expr) 'not)
		 (never-false (cadr expr)))))

      (define (truncated-list->string form)
	;; return form -> string with limits on its length
	(let* ((str (object->string form))
	       (len (length str)))
	  (if (< len 80)
	      str
	      (do ((i 67 (- i 1)))
		  ((or (= i 40)
		       (char-whitespace? (str i)))
		   (string-append (substring str 0 (if (<= i 40) 67 i)) "..."))))))

      (define (lists->string f1 f2)
	;; same but 2 strings that may need to be lined up vertically
	(let* ((str1 (object->string f1))
	       (len1 (length str1))
	       (str2 (object->string f2))
	       (len2 (length str2))
	       (N 4))
	  (when (> len1 80)
	    (set! str1 (truncated-list->string f1))
	    (set! len1 (length str1)))
	  (when (> len2 80)
	    (set! ((funclet pretty-print) '*pretty-print-left-margin*) N)
	    (set! ((funclet pretty-print) '*pretty-print-length*) 110)
	    (set! str2 (pp f2))
	    (set! len2 (length str2)))
	  (if (< (+ len1 len2) 80)
	      (format #f "~A -> ~A" str1 str2)
	      (format #f "~%~NC~A ->~%~NC~A" N #\space str1 N #\space str2))))
      
      (define (lint-format str name . args)
	(let ((outstr (if (and (positive? line-number)
			       (< line-number 100000))
			  (apply format #f (string-append " ~A (line ~D): " str "~%") name line-number args)
			  (apply format #f (string-append " ~A: " str "~%") name args))))
	  (display outstr outport)
	  (if (> (length outstr) 120)
	      (newline outport))))
      
      (define (side-effect? form env)
	;; could evaluation of form have any side effects (like IO etc)

	(if (and (proper-list? form)                   ; we don't want dotted lists or () here
		 (not (null? form)))
	    ;; can't optimize ((...)...) because the car might eval to a function
	    (or (and (not (hash-table-ref no-side-effect-functions (car form)))
		     ;; if it's not in the no-side-effect table and ...

		     (let ((e (or (var-member (car form) env) (hash-table-ref globals (car form)))))
		       (or (not (var? e))
			   (not (symbol? (var-ftype e)))
			   (var-side-effect e)))
		     ;; it's either not known to be a local function, or it has side-effects, and...

		     (or (not (eq? (car form) 'format))                         ; (format #f ...)
			 (cadr form)))
		     ;; it's not the common (format #f ...) special case, then...(goto case below) 
		     ;; else return #t: side-effects are possible -- this is too hard to read

		(case (car form)
		  ((set! define define* define-macro define-macro* define-bacro define-bacro* define-constant define-expansion) #t)

		  ((quote) #f)

		  ((case)
		   (or (not (pair? (cdr form)))
		       (side-effect? (cadr form) env) ; the selector
		       (letrec ((case-effect? (lambda (f e)
						(and (pair? f)
						     (or (not (pair? (car f)))
							 (any? (lambda (ff) (side-effect? ff e)) (cdar f))
							 (case-effect? (cdr f) e))))))
			 (case-effect? (cddr form) env))))

		  ((cond)
		   (letrec ((cond-effect? (lambda (f e)
					    (and (pair? f)
						 (or (any? (lambda (ff) (side-effect? ff e)) (car f))
						     (cond-effect? (cdr f) e))))))
		     (or (not (pair? (cadr form)))
			 (cond-effect? (cdr form) env))))

		  ((let let* letrec letrec*)
		   (letrec ((let-effect? (lambda (f e)
					   (and (pair? f)
						(or (not (pair? (car f)))
						    (not (pair? (cdar f))) ; an error, reported elsewhere: (let ((x)) x)
						    (side-effect? (cadar f) e)
						    (let-effect? (cdr f) e))))))
		     (if (symbol? (cadr form))
			 (or (< (length form) 4)
			     (let-effect? (caddr form) env)
			     (any? (lambda (ff) (side-effect? ff env)) (cdddr form)))
			 (or (< (length form) 3) 
			     (let-effect? (cadr form) env)
			     (any? (lambda (ff) (side-effect? ff env)) (cddr form))))))
		   
		  ((do)
		   (letrec ((do-effect? (lambda (f e)
					  (and (pair? f)
						(or (not (pair? (car f)))
						    (not (pair? (cdar f)))
						    (side-effect? (cadar f) e)
						    (and (pair? (cddar f))
							 (side-effect? (caddar f) e))
						    (do-effect? (cdr f) e))))))
		     (or (< (length form) 3)
			 (not (list? (cadr form)))
			 (not (list? (caddr form)))
			 (do-effect? (cadr form) env)
			 (any? (lambda (ff) (side-effect? ff env)) (caddr form))
			 (any? (lambda (ff) (side-effect? ff env)) (cdddr form)))))

		  ;; ((lambda lambda*) (any? (lambda (ff) (side-effect? ff env)) (cddr form))) ; this is trickier than it looks

		  (else
		   (or (any? (lambda (f) (side-effect? f env)) (cdr form)) ; any subform has a side-effect
		       (let ((sig (procedure-signature (car form))))       ; sig has func arg and it is not known safe
			 (and sig
			      (memq 'procedure? (cdr sig))
			      (call-with-exit
			       (lambda (return)
				 (for-each
				  (lambda (sg arg)
				    (when (eq? sg 'procedure?)
				      (if (or (not (symbol? arg))
					      (not (hash-table-ref no-side-effect-functions arg)))
					  (return #t))))
				  (cdr sig) (cdr form))
				 #f))))))))

	    (and (symbol? form)
		 (not (hash-table-ref no-side-effect-functions form))
		 (let ((e (or (var-member form env) (hash-table-ref globals form))))
		   (and (var? e)
			(symbol? (var-ftype e)))))))


      (define (just-constants? form env)
	;; can we probably evaluate form given just built-in stuff?
	(or (and (constant? form)
		 (not (pair? form))
		 (not (vector? form)))
	    (and (pair? form)
		 (or (and (symbol? (car form))
			  (hash-table-ref no-side-effect-functions (car form))
			  (not (hash-table-ref globals (car form)))
			  (not (var-member (car form) env))) ; e.g. exp declared locally as a list
		     (and (constant? (car form))
			  (not (pair? (car form)))
			  (not (vector? (car form)))))
		 (just-constants? (cdr form) env))))

      
      (define (equal-ignoring-constants? a b)
	(or (morally-equal? a b)
	    (and (symbol? a)
		 (constant? a) 
		 (morally-equal? (symbol->value a) b))
	    (and (symbol? b)
		 (constant? b)
		 (morally-equal? (symbol->value b) a))
	    (and (pair? a)
		 (pair? b)
		 (equal-ignoring-constants? (car a) (car b))
		 (equal-ignoring-constants? (cdr a) (cdr b)))))

      
      (define (just-symbols? form)
	(or (null? form)
	    (symbol? form)
	    (and (pair? form)
		 (symbol? (car form))
		 (just-symbols? (cdr form)))))

      (define (list-any? f lst)
	(if (pair? lst)
	    (or (f (car lst))
		(list-any? f (cdr lst)))
	    (f lst)))
      
      (define (reversible? func)
	(memq func '(* + = char=? string=? eq? eqv? equal? morally-equal? logand logxor logior max min lcm gcd
		     < > <= >=
		     char<? char>? char<=? char>=?
		     string<? string>? string<=? string>=?
		     char-ci<? char-ci>? char-ci<=? char-ci>=?
		     string-ci<? string-ci>? string-ci<=? string-ci>=?)))

      (define (reversed func)
	(case func
	  ((<) '>) ((>) '<) ((<=) '>=) ((>=) '<=)
	  ((* + = char=? string=? eq? eqv? equal? morally-equal? logand logxor logior max min lcm gcd) func)
	  ((char<?) 'char>?) ((char>?) 'char<?) ((char<=?) 'char>=?) ((char>=?) 'char<=?) 
	  ((string<?) 'string>?) ((string>?) 'string<?) ((string<=?) 'string>=?) ((string>=?) 'string<=?) 
	  ((char-ci<?) 'char-ci>?) ((char-ci>?) 'char-ci<?) ((char-ci<=?) 'char-ci>=?) ((char-ci>=?) 'char-ci<=?)
	  ((string-ci<?) 'string-ci>?) ((string-ci>?) 'string-ci<?) ((string-ci<=?) 'string-ci>=?) ((string-ci>=?) 'string-ci<=?)
	  (else #f)))
      
      (define (repeated-member? lst env)
	(and (pair? lst)
	     (or (and (or (not (pair? (car lst)))
			  (and (not (side-effect? (car lst) env))
			       (not (eq? (caar lst) 'random))))
		      (pair? (cdr lst))
		      (member (car lst) (cdr lst)))
		 (repeated-member? (cdr lst) env))))
      
      (define (set-ref? name env)
	;; if name is in env, set its "I've been referenced" flag
	(let ((data (or (var-member name env) (hash-table-ref globals name))))
	  ;(format *stderr* "~A ~A -> ~A ~A~%" name env data (var? data))
	  (when (var? data)
	    (set! (var-ref data) #t)))
	env)
      
      (define (set-set? name new-val env)
	;; TODO: if (set! func val) need to either clear func info or fix it up

	(let ((data (or (var-member name env) (hash-table-ref globals name))))
	  (when (var? data)
	    (set! (var-value data) new-val)
	    (set! (var-type data) #t)  ; a stopgap
	    (set! (var-ftype data) #f) ; a stopgap
	    (set! (var-set data) #t))))
      
      (define (proper-list lst)
	;; return lst as a proper list
	(if (pair? lst)
	    (cons (car lst) 
		  (if (pair? (cdr lst)) 
		      (proper-list (cdr lst)) 
		      (if (null? (cdr lst)) 
			  () 
			  (list (cdr lst)))))
	    lst))
      
      (define (keywords lst)
	(let ((count 0))
	  (do ((p lst (cdr p)))
	      ((null? p) count)
	    (if (keyword? (car p))
		(set! count (+ count 1))))))
      ;(count-if keyword? lst))
      
      (define (check-star-parameters name args)
	(if (list-any? (lambda (k) (memq k '(:key :optional))) args)
	    (lint-format ":optional and key are no longer accepted: ~A" name args))
	(let ((r (memq :rest args)))
	  (when (pair? r)
	    (if (not (pair? (cdr r)))
		(lint-format ":rest parameter needs a name: ~A" name args)
		(if (pair? (cadr r))
		    (lint-format ":rest parameter can't specify a default value: ~A" name args)))))
	(let ((a (memq :allow-other-keys args)))
	  (if (and (pair? a)
		   (pair? (cdr a)))
	      (lint-format ":allow-other-keys should be at the end of the parameter list: ~A" name args))))

      (define (tree-member sym tree)
	(and (pair? tree)
	     (or (eq? (car tree) sym)
		 (and (pair? (car tree))
		      (tree-member sym (car tree)))
		 (tree-member sym (cdr tree)))))
      
      (define (tree-unquoted-member sym tree)
	(and (pair? tree)
	     (not (eq? (car tree) 'quote))
	     (or (eq? (car tree) sym)
		 (and (pair? (car tree))
		      (tree-unquoted-member sym (car tree)))
		 (tree-unquoted-member sym (cdr tree)))))
      
      (define (tree-car-member sym tree)
	(and (pair? tree)
	     (or (eq? (car tree) sym)
		 (and (pair? (car tree))
		      (tree-car-member sym (car tree)))
		 (and (pair? (cdr tree))
		      (member sym (cdr tree) tree-car-member)))))
      
      (define (tree-set-member sym set tree) ; sym as arg, set as car
	(and (pair? tree)
	     (or (memq (car tree) set)
		 (and (pair? (car tree))
		      (tree-set-member sym set (car tree)))
		 (and (pair? (cdr tree))
		      (or (member sym (cdr tree))
			  (member #f (cdr tree) (lambda (a b) (tree-set-member sym set b))))))))

      (define (tree-symbol-walk tree syms)
	(if (pair? tree)
	    (if (eq? (car tree) 'quote)
		(if (and (pair? (cdr tree))
			 (symbol? (cadr tree))
			 (not (memq (cadr tree) (car syms))))
		    (tree-symbol-walk (cddr tree) (begin (set-car! syms (cons (cadr tree) (car syms))) syms)))
		(if (eq? (car tree) {list})
		    (if (and (pair? (cdr tree))
			     (pair? (cadr tree))
			     (eq? (caadr tree) 'quote)
			     (symbol? (cadadr tree))
			     (not (memq (cadadr tree) (cadr syms))))
			(tree-symbol-walk (cddr tree) (begin (set-car! (cdr syms) (cons (cadadr tree) (cadr syms))) syms)))
		    (begin
		      (tree-symbol-walk (car tree) syms)
		      (tree-symbol-walk (cdr tree) syms))))))

      (define (remove item sequence)
	(let ((got-it #f))
	  (map (lambda (x)
		 (if (and (not got-it)
			  (equal? x item))
		     (begin
		       (set! got-it #t)
		       (values))
		     x))
	       sequence)))
      
      (define (remove-all item sequence)
	(map (lambda (x)
	       (if (equal? x item)
		   (values)
		   x))
	     sequence))
      
      (define (remove-if p l)
	(cond ((null? l) ())
	      ((p (car l)) (remove-if p (cdr l)))
	      (else (cons (car l) 
			  (remove-if p (cdr l))))))

      (define (checked-eval form)
	(catch #t
	  (lambda ()
	    (eval (copy-tree form)))
	  (lambda args
	    #t)))   ; just ignore errors in this context

      (define (return-type-ok? type ret)
	(or (eq? type ret)
	    (and (pair? ret)
		 (memq type ret))))


      (define (simplify-boolean in-form true false env)
	;; (or)->#f, (or x) -> x, (or x ... from here on we know x is #f), (or x #t...) -> (or x #t), any constant expr can be collapsed
	;;   (or ... (or ...) ...) -> or of all, (or ... #f ...) toss the #f
	;; similarly for and
	;; (or ... (not (and ...))) -> (or ... (not x) [from here we know x is true] (not y)...)
	;; (or ... (not (and x1 x2 ...))) -> (or ... (not x1) (not x2)...), but is that simpler?
	
	;;   (or x1 x2 x1) -> (or x1 x2) 
	;;   (and x1 x2 x1) -> (and x2 x1)

	(define (bsimp uform)
	  ;; find and remove any expressions that have no effect on the outcome
	  (if (or (not (pair? uform))
		  (not (memq (car uform) '(and or not)))
		  (side-effect? uform env))
	      uform
	      
	      (let ((vars ())
		    (associated-exprs ())
		    (ctr 0))
		
		(define (tree-remove-all x lst) 
		  (cond ((null? lst) ()) 
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
		
		(define (expand expr)
		  (define (car-rassoc val lst)
		    (and (pair? lst)
			 (if (equal? (cdar lst) val)
			     (car lst)
			     (car-rassoc val (cdr lst)))))
		  (let ((data (car-rassoc expr associated-exprs)))
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
		    (let ((len (length vars)))
		      (let ((vsize (expt 2 len))) ; 2^n possible cases
			(let ((v (make-vector vsize))
			      (vals ())
			      (nonf (make-vector len))
			      (cur 0)
			      (ctr 0)
			      (form (canonical-tree uform))) 
			  ;; (and (real? mag) (real? ang)) -> (and bool-0 bool-1)
			  ;; (not off)                     -> (not bool-0)
			  ;; (or din (sqrt 2.0))           -> (or bool-0 bool-1)
			  
			  (for-each
			   (lambda (var)
			     (do ((i cur (+ i 1)))
				 ((not (tree-member i form)) ; tree-member uses eq? (should use = for this?)
				  (set! cur (+ i 1))
				  (vector-set! nonf ctr i)
				  (set! ctr (+ ctr 1)))))
			   vars)
			  
			  (catch #t
			    (lambda ()
			      (let ((new-func (apply lambda vars form ())))
				(do ((ctr 0 (+ ctr 1)))
				    ((= ctr vsize))
				  (vector-set! v ctr (apply new-func (let ((args ()))
								       (do ((i 0 (+ i 1)))
									   ((= i len) (reverse args))
									 (set! args (cons (and (logbit? ctr i) 
											       (vector-ref nonf i))
											  args))))))
				  (if (not (member (vector-ref v ctr) vals))
				      (set! vals (cons (vector-ref v ctr) vals))))))
			    (lambda args 'error))
			  
			  (if (= (length vals) 1)
			      (car vals)
			      (let ((none-vars ())
				    (pos -1))
				(for-each
				 (lambda (var)
				   (set! pos (+ pos 1))
				   (call-with-exit
				    (lambda (return)
				      (do ((ctr 0 (+ ctr 1)))
					  ((= ctr vsize)
					   (set! none-vars (cons var none-vars)))
					(if (and (not (logbit? ctr pos))
						 (not (equal? (vector-ref v ctr) (vector-ref v (logior ctr (ash 1 pos))))))
					    (return #f))))))
				 vars)
				
				(if (pair? none-vars)
				    (begin
				      (for-each
				       (lambda (nv)
					 (set! form (tree-remove-all nv form)))
				       none-vars)
				      (expand form))
				    uform))))))))))
	
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
						 ((integer?)  (memq (car b) '(even? odd?)))
						 (else #f)))
					  (and (> (length b) 2)
					       (member (cadr a) (cdr b))
					       (case (car a)
						 ((complex? number?) (eq? (car b) '=))
						 ((real?)            (memq (car b) '(< > <= >=)))
						 (else #f)))))))
		       (and (pair? (cadr e))
			    (memq (car e) bools)
			    (memq (return-type (caadr e) env) bools)
			    (subsumes? (car e) (return-type (caadr e) env)))))))
	
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
	      ((boolean? procedure? symbol? continuation? let?)
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
		       (and (pair? (cadr e))
			    (memq (car e) bools)
			    (memq (return-type (caadr e) env) bools)
			    (not (any-compatible? (car e) (return-type (caadr e) env))))))))
	
	(define (contradictory? ands)
	  (let ((vars ()))
	    (call-with-exit
	     (lambda (return)
	       (do ((b ands (cdr b)))
		   ((null? b) #f)
		 (if (and (pair? b)
			  (pair? (car b))
			  (pair? (cdar b)))
		     (let* ((func (caar b))
			    (arg-type func)
			    (args (cdar b)))
		       (if (symbol? arg-type)
			   (for-each
			    (lambda (arg)
			      (if (symbol? arg)
				  (let ((type (assq arg vars)))
				    (if (not type)
					(set! vars (cons (cons arg arg-type) vars))
					(if (not (compatible? (cdr type) arg-type))
					    (return #t))))))
			    args)))))))))
	
	(define (and-redundant? type1 type2)
	  (if (or (not (symbol? type1))
		  (not (symbol? type2))
		  (not (memq type1 bools))
		  (not (memq type2 bools)))
	      #f ; return #f if not (obviously) redundant, else return which of the two to keep
	      (if (eq? type1 type2)
		  type1
		  (case type1
		    ((number? complex?) (or (and (memq type2 '(float? real? rational? integer?)) type2)
					    (and (memq type2 '(number? complex?)) type1)))
		    ((real?)            (or (and (memq type2 '(float? rational? integer?)) type2)
					    (and (memq type2 '(number? complex?)) type1)))
		    ((float?)           (and (memq type2 '(real? complex? number?)) type1))
		    ((rational?)        (or (and (eq? type2 'integer?) type2)
					    (and (memq type2 '(real? complex? number?)) type1)))
		    ((integer?)         (and (memq type2 '(real? rational? complex? number?)) type1))
		    ((vector?)          (and (memq type2 '(float-vector? int-vector?)) type2))
		    ((float-vector? int-vector?) (and (eq? type2 'vector?) type1))
		    ((symbol?)          (and (memq type2 '(keyword? gensym?)) type2))
		    ((keyword? gensym?) (and (eq? type2 'symbol?) type1))
		    ((list?)            (and (memq type2 '(null? pair?)) type2))
		    ((pair? null?)      (and (eq? type2 'list?) type1))
		    ((string?)          (and (eq? type2 'byte-vector?) type2))
		    ((byte-vector?)     (and (eq? type2 'string?) type1))
		    (else #f)))))

	(define (classify e)
	  ;; do we already know that e is true or false?
	  ;;   some cases subsume others: if we know (integer? x) is true, (complex? x) is also true
	  (if (true? e)
	      #t ; the simple boolean is passed back which will either be dropped or will stop the outer expr build
	      (if (false? e)
		  #f
		  ;; eval of a constant expression here is tricky -- for example, (sqrt 2) should not be turned into 1.414...
		  (if (just-constants? e env)
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
	  
	  (if (or (not (pair? e))
		  (not (side-effect? e env)))
	      (let ((its-true (if (eq? and/or 'or)
				  (eq? value #t)             ; or, so it's false if unknown
				  value)))
		(if its-true
		    (set! true (cons e true))
		    (set! false (cons e false))))))
	
	(let ((form (bsimp in-form)))
	  ;(format *stderr* "form: ~S~%" form)
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
			 ;(format *stderr* "val ~S, arg: ~S~%" val arg)
			 (if (boolean? val)
			     (not val)
			     (if (or (code-constant? arg)
				     (and (pair? arg)
					  (symbol? (car arg))
					  (hash-table-ref no-side-effect-functions (car arg))
					  (not (hash-table-ref globals (car arg)))
					  (let ((ret (return-type (car arg) env)))
					    (and (or (symbol? ret) (pair? ret))
						 (not (return-type-ok? 'boolean? ret))))
					  (not (var-member (car arg) env))))
				 #f
				 (if (and (pair? arg)               ; (not (not ...)) -> ...
					  (pair? (cdr arg))
					  (eq? (car arg) 'not))
				     (cadr arg)
				     (if (not (equal? val arg))
					 `(not ,val)
					 (if (and (pair? arg)
						  (<= (length arg) 3)) ; avoid (<= 0 i 12) and such
					     (case (car arg)
					       ((<)            `(>= ,@(cdr arg)))   ; (not (< ...)) -> (>= ...) 
					       ((>)            `(<= ,@(cdr arg)))
					       ((<=)           (if (morally-equal? (caddr arg) 0.0) `(positive? ,(cadr arg)) `(> ,@(cdr arg))))
					       ((>=)           (if (morally-equal? (caddr arg) 0.0) `(negative? ,(cadr arg)) `(< ,@(cdr arg))))
					       ((char<?)       `(char>=? ,@(cdr arg)))
					       ((char>?)       `(char<=? ,@(cdr arg)))
					       ((char<=?)      `(char>? ,@(cdr arg)))
					       ((char>=?)      `(char<? ,@(cdr arg)))
					       ((char-ci<?)    `(char-ci>=? ,@(cdr arg)))
					       ((char-ci>?)    `(char-ci<=? ,@(cdr arg)))
					       ((char-ci<=?)   `(char-ci>? ,@(cdr arg)))
					       ((char-ci>=?)   `(char-ci<? ,@(cdr arg)))
					       ((string<?)     `(string>=? ,@(cdr arg)))
					       ((string>?)     `(string<=? ,@(cdr arg)))
					       ((string<=?)    `(string>? ,@(cdr arg)))
					       ((string>=?)    `(string<? ,@(cdr arg)))
					       ((string-ci<?)  `(string-ci>=? ,@(cdr arg)))
					       ((string-ci>?)  `(string-ci<=? ,@(cdr arg)))
					       ((string-ci<=?) `(string-ci>? ,@(cdr arg)))
					       ((string-ci>=?) `(string-ci<? ,@(cdr arg)))
					       ((odd?)         `(even? ,@(cdr arg)))
					       ((even?)        `(odd? ,@(cdr arg)))
					       ((exact?)       `(inexact? ,@(cdr arg)))
					       ((inexact?)     `(exact? ,@(cdr arg)))
					       ;; ((null?)        `(pair? ,@(cdr arg)))
					       ;;      this is not quite right
					       ;; char-upper-case? and lower are not switchable here

					       ;; if stuff loaded, (not (every? ...)) => any? and so on

					       ((zero?)       ; (not (zero? (logand p (ash 1 i)))) -> (logbit? p i)
						(let ((zarg (cadr arg)))  ; (logand...)
						  (if (and (pair? zarg)
							   (eq? (car zarg) 'logand)
							   (pair? (cddr zarg))
							   (pair? (caddr zarg))
							   (eq? (caaddr zarg) 'ash)
							   (eqv? (cadr (caddr zarg)) 1))
						      `(logbit? ,(cadr zarg) ,(caddr (caddr zarg)))
						      form)))

					       (else form))
					     form))))))
		       form))
		  
		  ((or)
		   (and (> len 1)
		       (let ((arg1 (cadr form)))
			 (if (= len 2)
			     (if (code-constant? arg1)
				 arg1
				 (classify arg1))
			     (if (true? arg1)        ; no need to check anything else
				 #t                  ; side-effect? here is a nightmare
				 (let* ((arg2 (caddr form))
					(t1 (and (= len 3)
						 (pair? arg1)
						 (pair? arg2)
						 (pair? (cdr arg1))
						 (pair? (cdr arg2))
						 (equal? (cadr arg1) (cadr arg2))
						 (not (side-effect? arg1 env))
						 (and-redundant? (car arg1) (car arg2)))))
				   (if t1
				       (if (eq? t1 (car arg1))
					   arg2
					   arg1)
				       (let ((sf #f))
					 (if (and (every? (lambda (p)
							    (and (pair? p)
								 (pair? (cdr p))
								 (eq? (car p) 'not)))
							  (cdr form))
						  (let ()
						    (set! sf (simplify-boolean `(not (and ,@(map cadr (cdr form)))) true false env))
						    (or (not (pair? sf))
							(not (eq? (car sf) 'not))
							(not (pair? (cadr sf)))
							(not (eq? (caadr sf) 'and)))))
					     sf
					     ;; if all clauses are (eq-func x y) where one of x/y is a symbol|simple-expr repeated throughout
					     ;;   and the y is a code-constant, or -> memq and friends.  This could also handle cadr|caddr reversed.

					     ;; (or (pair? x) (null? x)) -> (list? x)

					     (let ((sym #f)
						   (eqf #f))
					       (if (every? (lambda (p)
							     (and (pair? p)
								  (if (not eqf)
								      (and (memq (car p) '(eq? eqv? equal? char=? string=? = char-ci=? string-ci=?))
									   (set! eqf (car p)))
								      (eq? eqf (car p)))
								  (if (not sym) 
								      (and (not (side-effect? (cadr p) env))
									   (set! sym (cadr p)))
								      (equal? sym (cadr p)))
								  (code-constant? (caddr p))))
							   (cdr form))
						   (let* ((vals (map caddr (cdr form)))
							  (func (case eqf 
								  ((eq?) 'memq) 
								  ((eqv? char=?) 'memv) 
								  ((=) (if (every? rational? vals) 'memv 'member))
								  (else 'member)))
							  (equals (if (and (eq? func 'member)
									   (not (eq? eqf 'equal?)))
								      (list eqf)
								      ()))
							  (elements (map (lambda (v) (if (pair? v) (cadr v) v)) vals)))
						     (if (and (eq? eqf 'char=?)
							      (= (length elements) 2)
							      (char-ci=? (car elements) (cadr elements)))
							 `(char-ci=? ,sym ,(car elements))
							 (if (and (eq? eqf 'string=?)
								  (= (length elements) 2)
								  (string-ci=? (car elements) (cadr elements)))
							     `(string-ci=? ,sym ,(car elements))
							     `(,func ,sym ',(map (lambda (v) (if (pair? v) (cadr v) v)) vals) ,@equals))))
						   
						   (let ((new-form ()))
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
							     (set! val (classify (set! e (simplify-boolean e true false env)))))
							 
							 (if val                                ; #f in or is ignored
							     (if (or (eq? val #t)               ; #t or any non-#f constant in or ends the expression
								     (code-constant? val))
								 (begin
								   (if (null? new-form)         ; (or x1 123) -> value of x1 first
								       (set! new-form (list val))           ;was `(,val))
								       (set! new-form (cons val new-form))) ;was (append `(,val) new-form))) ; reversed when returned
								   (set! exprs '(#t)))
								 
								 ;; (or x1 x2 x1) -> (or x1 x2) is ok because if we get to x2, x1 is #f, so trailing x1 would still be #f
								 
								 (if (and (pair? e)             ; (or ...) -> splice into current
									  (eq? (car e) 'or))
								     (set! exprs (append e (cdr exprs))) ; we'll skip the 'or in do step
								     (begin                     ; else add it to our new expression with value #f
								       (store e val 'or)
								       (if (not (memq val new-form))
									   (set! new-form (cons val new-form)))))))))))))))))))))
		  
		  ((and)
		   (or (= len 1)
		       (let ((arg1 (cadr form)))
			 (if (= len 2)
			     (classify arg1)
			     (and (not (contradictory? (cdr form)))
				 (let* ((arg2 (caddr form))
					(t1 (and (= len 3)
						 (pair? arg1)
						 (pair? arg2)
						 (pair? (cdr arg1))
						 (pair? (cdr arg2))
						 (equal? (cadr arg1) (cadr arg2))
						 (not (side-effect? arg1 env))
						 (and-redundant? (car arg1) (car arg2))))) ; (and (integer? x) (number? x)) -> (integer? x)
				   (if t1
				       (if (eq? t1 (car arg1))
					   arg1
					   arg2)
				       (let ((new-form ()))
					 
					 (if (and (= len 3)
						  (not (side-effect? arg1 env))
						  (equal? arg1 arg2))                  ; (and x x) -> x
					     arg1

					     ;; (and (= ...)...) for more than 2 args? 
					     ;;   (and (< x y z) (< z w)) -> (< x y z w) ?
					     ;;   (and (< x y) (< y z) (< z w)) -> (< x y z w)

					     (if (and (= len 3)
						      (pair? arg1)
						      (pair? arg2)
						      (reversible? (car arg1))
						      (null? (cdddr arg1))
						      (pair? (cdr arg2))
						      (pair? (cddr arg2))
						      (null? (cdddr arg2))
						      (not (side-effect? arg2 env)) ; arg1 is hit in any case
						      (or (eq? (car arg1) (car arg2))
							  (let ((rf (reversed (car arg2))))
							    (and (eq? (car arg1) rf)
								 (set! arg2 (append (list rf) (reverse (cdr arg2)))))))
						      (or (eq? (caddr arg1) (cadr arg2))
							  (eq? (cadr arg1) (caddr arg2))
							  (and (memq (car arg1) '(= char=? string=? char-ci=? string-ci=?))
							       (or (eq? (cadr arg1) (cadr arg2))
								   (eq? (caddr arg1) (caddr arg2))))))
						 (if (eq? (caddr arg1) (cadr arg2))
						     `(,(car arg1) ,(cadr arg1) ,(cadr arg2) ,(caddr arg2))
						     (if (eq? (cadr arg1) (caddr arg2))
							 `(,(car arg1) ,(cadr arg2) ,(cadr arg1) ,(caddr arg1))
							 (if (eq? (cadr arg1) (cadr arg2))
							     `(,(car arg1) ,(cadr arg1) ,(caddr arg1) ,(caddr arg2))
							     `(,(car arg1) ,(cadr arg1) ,(caddr arg1) ,(cadr arg2)))))

						 (do ((exprs (cdr form) (cdr exprs)))
						     ((null? exprs) 
						      (if (null? new-form)
							  #t                                ; (and) -> #t
							  (let* ((nform (reverse new-form)) 
								 (newer-form (map (lambda (x cdr-x)
										    (if (and x (code-constant? x))
											(values)
											x))
										  nform (cdr nform))))
							    (if (null? newer-form)
								(car new-form)
								`(and ,@newer-form ,(car new-form))))))
						   
						   (let* ((e (car exprs))
							  (val (classify e)))
						     
						     (if (and (pair? val)
							      (memq (car val) '(and or not)))
							 (set! val (classify (set! e (simplify-boolean e () false env)))))
						     
						     ;; (and x1 x2 x1) is not reducible
						     ;;   the final thing has to remain at the end, but can be deleted earlier if it can't short-circuit the evaluation,
						     ;;   but if there are expressions following the first x1, we can't be sure that it is not
						     ;;   protecting them:
						     ;;       (and false-or-0 (display (list-ref lst false-or-0)) false-or-0)
						     ;;   so I'll not try to optimize that case.  But (and x x) is optimizable.
						     
						     (if (eq? val #t)
							 (if (null? (cdr exprs))     ; (and x y #t) should not remove the #t
							     (if (or (and (pair? e)
									  (eq? (return-type (car e) env) 'boolean?))
								     (eq? e #t))
								 (set! new-form (cons val new-form))
								 (if (or (null? new-form)
									 (not (equal? e (car new-form))))
								     (set! new-form (cons e new-form))))
							     (if (and (not (eq? e #t))
								      (or (null? new-form)
									  (not (member e new-form))))
								 (set! new-form (cons e new-form))))
							 
							 (if (not val)             ; #f in 'and' ends the expression
							     (begin
							       (if (or (null? new-form)   
								       (just-symbols? new-form))
								   (set! new-form '(#f))
								   (set! new-form (cons #f new-form))) ;was (append '(#f) new-form)))
							       (set! exprs '(#f)))
							     (if (and (pair? e)       ; if (and ...) splice into current
								      (eq? (car e) 'and))
								 (set! exprs (append e (cdr exprs)))
								 (if (not (and (pair? e)                   ; (and ... (or ... 123) ...) -> splice out or
									       (pair? (cdr exprs))
									       (eq? (car e) 'or)
									       (> (length e) 2)
									       (let ((last (list-ref e (- (length e) 1))))
										 (and last ; (or ... #f)
										      (code-constant? last)))))
								     (begin                 ; else add it to our new expression with value #t
								       (store e val 'and)
								       (if (or (not (pair? new-form))
									       (not (eq? val (car new-form))))
									   (set! new-form (cons val new-form)))))))))))))))))))))))))
      
      (define (splice-if f lst)
	(cond ((null? lst) ())
	      ((pair? lst)
	       (if (and (pair? (car lst))
			(f (caar lst)))
		   (append (splice-if f (cdar lst)) (splice-if f (cdr lst)))
		   (cons (car lst) (splice-if f (cdr lst)))))
	      (#t lst)))
      
      
      (define (simplify-numerics form env)
	;; this returns a form, possibly the original simplified
	(let ((real-result? (lambda (op) (memq op '(imag-part real-part abs magnitude angle max min exact->inexact
							      modulo remainder quotient lcm gcd))))
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
			      ((and (member (car lst) nlst)
				    (or (not (pair? (car lst)))
					(not (eq? (caar lst) 'random)))) ; this problem applies to anything that calls random, mus-random etc
			       (rem-dup (cdr lst) nlst))
			      (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
	      (reverse (rem-dup lst ()))))
	  
	  (define (just-rationals? form)
	    (or (null? form)
		(rational? form)
		(and (pair? form)
		     (rational? (car form))
		     (just-rationals? (cdr form)))))
	  
	  (define (just-reals? form)
	    (or (null? form)
		(real? form)
		(and (pair? form)
		     (real? (car form))
		     (just-reals? (cdr form)))))
	  
	  (define (just-integers? form)
	    (or (null? form)
		(integer? form)
		(and (pair? form)
		     (integer? (car form))
		     (just-integers? (cdr form)))))
	  
	  (define (simplify-arg x)
	    (if (or (not (pair? x))                      ; constants and the like look dumb if simplified
		    (hash-table-ref globals (car x))
		    (not (hash-table-ref no-side-effect-functions (car x)))
		    (var-member (car x) env))
		x
		(let ((f (simplify-numerics x env)))
		  (if (and (pair? f)
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
		    (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			(let ((rats (collect-if list rational? val)))
			  (if (> (length rats) 1)
			      (let ((y (apply + rats)))
				(if (zero? y)
				  (set! val (collect-if list (lambda (x) (not (number? x))) val))
				  (set! val (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		    (case (length val)
		      ((0) 0)
		      ((1) (car val))                     ; (+ x) -> x
		      (else `(+ ,@val)))))))              ; other obvious simplifications never happen
	      
	      ((*)
	       (case len
		 ((0) 1)
		 ((1) (car args))
		 (else 
		  (let ((val (remove-all 1 (splice-if (lambda (x) (eq? x '*)) args))))
		    (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			(let ((rats (collect-if list rational? val)))
			  (if (> (length rats) 1)
			      (let ((y (apply * rats)))
				(if (= y 1)
				  (set! val (collect-if list (lambda (x) (not (number? x))) val))
				  (set! val (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		    (case (length val)
		      ((0) 1)
		      ((1) (car val))                     ; (* x) -> x
		      (else 
		       (if (just-rationals? val)
			   (apply * val)
			   (if (memv 0 val)               ; (* x 0 2) -> 0
			       0 
			       (if (= (length val) 2)
				   (if (memv -1 val)
				       `(- ,@(remove -1 val)) ; (* -1 x) -> (- x)
				       (if (and (pair? (car val))
						(pair? (cadr val))
						(= (length (car val)) 3)
						(equal? (cdar val) (cdadr val))
						(or (and (eq? (caar val) 'gcd) (eq? (caadr val) 'lcm))
						    (and (eq? (caar val) 'lcm) (eq? (caadr val) 'gcd))))
					   `(abs (* ,@(cdar val))) ; (* (gcd a b) (lcm a b)) -> (abs (* a b)) but only if 2 args?
					   `(* ,@val)))
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
				     `(- ,(caddar args) ,(cadar args)) ; (- (- x y)) -> (- y x)
				     `(- ,@args)))
			    (else `(- ,@args))))))
		 ((2) 
		  (if (just-rationals? args)
		      (apply - args)
		      (if (eqv? (car args) 0)
			  `(- ,(cadr args))                ; (- 0 x) -> (- x)
			  (if (eqv? (cadr args) 0)
			      (car args)                   ; (- x 0) -> x
			      (if (equal? (car args) (cadr args))
				  0                        ; (- x x)
				  (if (and (pair? (car args))
					   (eq? (caar args) '-)
					   (> (length (car args)) 2))
				      `(- ,@(cdar args) ,(cadr args)) ; (- (- x y) z) -> (- x y z) but leave (- (- x) ...)
				      `(- ,@args)))))))
		 (else 
		  (let ((val (remove-all 0 (splice-if (lambda (x) (eq? x '+)) (cdr args)))))
		    (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			(let ((rats (collect-if list rational? val)))
			  (if (> (length rats) 1)
			      (let ((y (apply + rats)))
				(if (zero? y)
				    (set! val (collect-if list (lambda (x) (not (number? x))) val))
				    (set! val (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		    (set! val (cons (car args) val))
		    (let ((first-arg (car args))
			  (nargs (cdr val)))
			(if (member first-arg nargs)
			    (begin
			      (set! nargs (remove first-arg nargs)) ; remove once
			      (set! first-arg 0)))
			(if (null? nargs)
			    first-arg                     ; (- x 0 0 0)?
			    (if (and (eqv? first-arg 0)
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
			  `(/ ,@args))))
		 ((2)
		  (if (and (just-rationals? args)
			   (not (zero? (cadr args))))
		      (apply / args)                          ; including (/ 0 12) -> 0
		      (let ((arg1 (car args))
			    (arg2 (cadr args)))
			(if (eqv? arg1 1)                 ; (/ 1 x) -> (/ x)
			    `(/ ,arg2)
			    (if (eqv? arg2 1)
				arg1                      ; (/ x 1) -> x
				(if (and (pair? arg1)     ; (/ (log x) (log y)) -> (log x y)
					 (= (length arg1) 2)
					 (pair? arg2)
					 (= (length arg2) 2)
					 (eq? (car arg1) 'log)
					 (eq? (car arg2) 'log))
				    `(log ,(cadr arg1) ,(cadr arg2))
				    (if (and (pair? arg1)
					     (eq? (car arg1) '/)
					     (pair? arg2)
					     (eq? '/ (car arg2)))
					(let ((a1 (if (null? (cddr arg1)) `(/ 1 ,(cadr arg1)) arg1))
					      (a2 (if (null? (cddr arg2)) `(/ 1 ,(cadr arg2)) arg2)))
					  (simplify-numerics `(/ (* ,(cadr a1) ,@(cddr a2)) (* ,@(cddr a1) ,(cadr a2))) env))
					`(/ ,@args))))))))

		 (else 
		  (if (and (just-rationals? args)
			   (not (memv 0 (cdr args)))
			   (not (memv 0.0 (cdr args))))
		      (apply / args)
		      (let ((nargs                      ; (/ x a (* b 1 c) d) -> (/ x a b c d) but not short cases
			     (remove-all 1 (splice-if (lambda (x) (eq? x '*)) (cdr args)))))
			(if (null? nargs) ; (/ x 1 1) -> x
			    (car args)
			    `(/ ,@(cons (car args) nargs))))))))
	      
	      ((sin cos asin acos sinh cosh tanh asinh acosh atanh exp)
	       (if (= len 1)
		   (if (and (pair? (car args))
			    (= (length (car args)) 2)
			    (eq? (caar args) (inverse-op (car form))))
		       (cadar args)
		       (if (eqv? (car args) 0)
			   (case (car form)
			     ((sin asin sinh asinh tanh atanh) 0)
			     ((exp cos cosh) 1)
			     (else `(,(car form) ,@args)))
			   (if (and (eq? (car form) 'cos)
				    (pair? (car args))
				    (eq? (caar args) '-)
				    (null? (cddar args)))
			       `(cos ,(cadar args))
			       (if (eq? (car args) 'pi)
				   (case (car form)
				     ((sin) 0.0)
				     ((cos) 1.0)
				     (else `(,(car form) ,@args)))
				   (if (eqv? (car args) 0.0)
				       (apply (symbol->value (car form)) '(0.0))
				       (if (and (eq? (car form) 'exp) ; (exp (* a (log b))) -> (expt b a)
						(pair? (car args))
						(eq? (caar args) '*))
					   (let ((targ (cdar args)))
					     (if (= (length targ) 2)
						 (if (and (pair? (car targ))
							  (eq? (caar targ) 'log)
							  (pair? (cdar targ))
							  (null? (cddar targ)))
						     `(expt ,(cadar targ) ,(cadr targ))
						     (if (and (pair? (cadr targ))
							      (eq? (caadr targ) 'log) 
							      (pair? (cdadr targ))
							      (null? (cddadr targ)))
							 `(expt ,(cadadr targ) ,(car targ))
							 `(,(car form) ,@args)))
						 `(,(car form) ,@args)))
				       `(,(car form) ,@args)))))))
		   `(,(car form) ,@args)))
	      
	      ((log)
	       (if (pair? args)
		   (if (eqv? (car args) 1)
		       0
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
		   form))
	      
	      ((sqrt)
	       (if (pair? args)
		   (if (and (rational? (car args))
			    (= (car args) (* (sqrt (car args)) (sqrt (car args)))))
		       (sqrt (car args)) ; don't collapse (sqrt (* a a)), a=-1 for example
		       `(sqrt ,@args))
		   form))
	      
	      ((floor round ceiling truncate)
	       (if (= len 1)
		   (if (number? (car args))
		       (catch #t (lambda () 
				   (apply (symbol->value (car form)) args)) 
			      (lambda any 
				`(,(car form) ,@args)))
		       (if (and (pair? (car args))
				(integer-result? (caar args)))
			   (car args)
			   `(,(car form) ,@args)))
		   form))
	      
	      ((abs magnitude)
	       (if (= len 1)
		   (if (and (pair? (car args))
			    (memq (caar args) '(abs magnitude denominator)))
		       (car args)
		       (if (rational? (car args))
			   (abs (car args))
			   (if (and (pair? (car args))       ; (abs (modulo x 2)) -> (modulo x 2)
				    (eq? (caar args) 'modulo)
				    (= (length (car args)) 3)
				    (number? (caddar args))
				    (positive? (caddar args)))
			       (car args)
			       (if (and (pair? (car args))   ; (abs (- x)) -> (abs x)
					(eq? (caar args) '-)
					(pair? (cdar args))
					(null? (cddar args)))
				   `(,(car form) ,(cadar args))
				   `(,(car form) ,@args)))))
		   form))
	      
	      ((imag-part)
	       (if (= len 1)
		   (if (or (real? (car args))
			   (and (pair? (car args))
				(real-result? (caar args))))
		       0.0
		       `(imag-part ,@args))
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
		       `(denominator ,(car args)))
		   form))

	      ((numerator)
	       (if (= len 1)
		   (if (or (integer? (car args))
			   (and (pair? (car args))
				(integer-result? (caar args))))
		       (car args)
		       (if (rational? (car args))
			   (numerator (car args))
			   `(numerator ,(car args))))
		   form))
	      
	      ((random)
	       (if (and (= len 1)
			(number? (car args)))
		   (if (and (integer? (car args))
			    (= (car args) 0))
		       0
		       (if (morally-equal? (car args) 0.0)
			   0.0
			   `(random ,@args)))
		   `(random ,@args)))
	      ;; what about (* 2.0 (random 1.0)) and the like?
	      ;;   this is trickier than it appears: (* 2.0 (random 3)) etc

	      ((complex)
	       (if (and (= len 2)
			(morally-equal? (cadr args) 0.0)) ; morally so that 0 matches
		   (car args)
		   `(complex ,@args)))
	       
	      ((rationalize lognot ash modulo remainder quotient)
	       (if (just-rationals? args)
		   (catch #t ; catch needed here for things like (ash 2 64)
		     (lambda ()
		       (apply (symbol->value (car form)) args))
		     (lambda ignore
		       `(,(car form) ,@args))) ; use this form to pick up possible arg changes
		   `(,(car form) ,@args)))
	      
	      ((expt) 
	       (if (= len 2)
		   (if (and (eqv? (car args) 0)
			    (not (eqv? (cadr args) 0)))
		       0
		       (if (and (eqv? (cadr args) 0)
				(not (eqv? (car args) 0)))
			   1
			   (if (eqv? (car args) 1)
			       1
			       (if (eqv? (cadr args) 1)
				   (car args)
				   (if (eqv? (cadr args) -1)
				       `(/ ,(car args))
				       (if (just-rationals? args)
					   (catch #t
					     (lambda ()
					       (let ((val (apply expt args)))
						 (if (integer? val)
						     val
						     `(expt ,@args))))
					     (lambda args
					       `(expt ,@args)))
					   `(expt ,@args)))))))
		   form))

	      ((angle)
	       (if (pair? args)
		   (if (eqv? (car args) -1)
		       'pi
		       (if (or (morally-equal? (car args) 0.0)
			       (eq? (car args) 'pi))
			   0.0
			   `(angle ,@args)))
		   form))

	      ((atan)
	       (if (and (= len 1)
			(pair? (car args))
			(= (length (car args)) 3)
			(eq? (caar args) '/))
		   `(atan ,@(cdar args))
		   `(atan ,@args)))
	      
	      ((inexact->exact)
	       (if (= len 1)
		   (if (or (rational? (car args))
			   (and (pair? (car args))
				(or (rational-result? (caar args))
				    (integer-result? (caar args)))))
		       (car args)
		       (if (number? (car args))
			   (catch #t (lambda () (inexact->exact (car args))) (lambda any `(inexact->exact ,@args)))
			   `(inexact->exact ,@args)))
		   form))
	      
	      ((logior)
	       (set! args (remove-duplicates (remove-all 0 (splice-if (lambda (x) (eq? x 'logior)) args))))
	       (if (every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		   (let ((rats (collect-if list integer? args)))
		     (if (> (length rats) 1)
			 (let ((y (apply logior rats)))
			   (if (zero? y)
			       (set! args (collect-if list (lambda (x) (not (number? x))) args))
			       (set! args (cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
	       (if (null? args)           ; (logior) -> 0
		   0
		   (if (null? (cdr args)) ; (logior x) -> x
		       (car args)
		       (if (memv -1 args)
			   -1
			   (if (just-integers? args)
			       (apply logior args)
			       `(logior ,@args))))))
	      
	      ((logand)
	       (set! args (remove-duplicates (remove-all -1 (splice-if (lambda (x) (eq? x 'logand)) args))))
	       (if (every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		   (let ((rats (collect-if list integer? args)))
		     (if (> (length rats) 1)
			 (let ((y (apply logand rats)))
			   (if (= y -1)
			       (set! args (collect-if list (lambda (x) (not (number? x))) args))
			       (set! args (cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
	       (if (null? args)
		   -1
		   (if (null? (cdr args)) ; (logand x) -> x
		       (car args)
		       (if (memv 0 args)
			   0
			   (if (just-integers? args)
			       (apply logand args)
			       `(logand ,@args))))))
	      ;; (logand 1 (logior 2 x)) -> (logand 1 x)? 
	      ;; (logand 1 (logior 1 x)) -> 1
	      ;; (logand 3 (logior 1 x))?
	      ;; similarly for (logior...(logand...))

	      ((logxor)
	       (set! args (splice-if (lambda (x) (eq? x 'logxor)) args)) ; is this correct??
	       (if (null? args)
		   0
		   (if (null? (cdr args)) ; (logxor x) -> x??
		       (car args)
		       (if (just-integers? args)
			   (apply logxor args)
			   (if (and (= len 2)
				    (equal? (car args) (cadr args)))
			       0
			       `(logxor ,@args))))))
		   
	      ((gcd)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'gcd)) args)))
	       (if (null? args)
		   0
		   ;; here and in lcm, if just 1 arg -> (abs arg) 
		   (if (memv 1 args)
		       1
		       (if (just-integers? args)
			   (catch #t  ; maybe (gcd -9223372036854775808 -9223372036854775808)
			     (lambda ()
			       (apply gcd args))
			     (lambda ignore
			       `(gcd ,@args)))
			   (if (null? (cdr args))
			       `(abs ,(car args))
			       (if (eqv? (car args) 0)
				   `(abs ,(cadr args))
				   (if (eqv? (cadr args) 0)
				       `(abs ,(car args))
				       `(gcd ,@args))))))))
	      
	      ((lcm)
	       (set! args (remove-duplicates (splice-if (lambda (x) (eq? x 'lcm)) args)))
	       (if (null? args)
		   1
		   (if (memv 0 args)
		       0
		       (if (just-integers? args)
			   (catch #t
			     (lambda ()
			       (apply lcm args))
			     (lambda ignore
			       `(lcm ,@args)))
			   (if (null? (cdr args))
			       `(abs ,(car args))
			       `(lcm ,@args))))))
	      
	      ((max min)
	       (if (pair? args)
		   (begin
		     (set! args (remove-duplicates (splice-if (lambda (x) (eq? x (car form))) args)))
		     (if (= len 1)
			 (car args)
			 (if (just-reals? args)
			     (apply (symbol->value (car form)) args)
			     (let ((nums (collect-if list number? args))
				   (other (if (eq? (car form) 'min) 'max 'min)))
			       (if (and (pair? nums)
					(just-reals? nums)) ; non-real case checked elsewhere (later)
				   (let ((relop (if (eq? (car form) 'min) >= <=)))
				     (if (> (length nums) 1)
					 (set! nums (list (apply (symbol->value (car form)) nums))))
				     (let ((new-args (append nums (collect-if list (lambda (x) (not (number? x))) args))))
				       (let ((c1 (car nums)))
					 (set! new-args (collect-if list (lambda (x)
									   (or (not (pair? x))
									       (<= (length x) 2)
									       (not (eq? (car x) other))
									       (let ((c2 (find-if number? (cdr x))))
										 (or (not c2)
										     (relop c1 c2)))))
								    new-args)))
				       (if (< (length new-args) (length args))
					   (set! args new-args)))))
				     ;; if (max c1 (min c2 . args1) . args2) where (>= c1 c2) -> (max c1 . args2)
				     ;; if (min c1 (max c2 . args1) . args2) where (<= c1 c2) -> (min c1 . args2)
			       
			       ;; there are more such cases: (max x (min x 3)) -> x and (min x (max x c)) -> x
			       ;; (max a b) is (- (min (- a) (- b))), but that doesn't help here -- the "-" gets in our way
			       (if (null? (cdr args)) ; (max (min x 3) (min x 3)) -> (max (min x 3)) -> (min x 3)
				   (car args)
				   (if (and (null? (cddr args))   ; (max|min x (min|max x ...) -> x
					    (or (and (pair? (car args))
						     (eq? (caar args) other)
						     (symbol? (cadr args))   ; actually this is probably not needed, but I want to avoid (random ...)
						     ;; perhaps instead use not min/max 
						     (member (cadr args) (car args)))
						(and (pair? (cadr args))
						     (eq? (caadr args) other)
						     (symbol? (car args))
						     (member (car args) (cadr args)))))
				       ((if (pair? (car args)) cadr car) args)
				       `(,(car form) ,@args)))))))
		   form))
	      
	      (else `(,(car form) ,@args))))))
      
      
      (define (->eqf x)
	(case x
	  ((char?) '(eqv? char=?))
	  ((integer? rational? real? number? complex?) '(eqv? =))
	  ((symbol? keyword? boolean?)'(eq? eq?))
	  ((string? byte-vector?) '(equal? string=?))
	  ((null?) '(eq? null?))
	  ((pair? vector? float-vector? int-vector? hash-table?) '(equal? equal))
	  (else '(#t #t))))
      
      (define (eqf selector)
	(if (symbol? selector)
	    '(#t #t)
	    (if (not (pair? selector))
		(->eqf (->type selector))
		(if (eq? (car selector) 'quote)
		    (if (symbol? (cadr selector))
			'(eq? eq?)
			(if (null? (cadr selector))
			    '(eq? null?)
			    (if (char? (cadr selector))
				'(eqv? char=?)
				'(equal? equal?))))
		    (if (symbol? (car selector))
			(->eqf (return-type (car selector) ()))
			'(#t #t))))))
	   
      (define (unquoted x)
	(if (and (pair? x)
		 (eq? (car x) 'quote))
	    (cadr x)
	    x))

      (define (check-char-cmp name op form)
	(if (and (any? (lambda (x) 
			 (and (pair? x) 
			      (eq? (car x) 'char->integer)))
		       (cdr form))
		 (every? (lambda (x) 
			   (or (and (integer? x)
				    (<= 0 x 255))
			       (and (pair? x) 
				    (eq? (car x) 'char->integer))))
			 (cdr form)))
	    (lint-format "perhaps ~A" name 
			 (lists->string form
                            `(,(case op ((=) 'char=?) ((>) 'char>?) ((<) 'char<?) ((>=) 'char>=?) (else 'char<=?))
			      ,@(map (lambda (arg)
				       (if (integer? arg)
					   (integer->char arg)
					   (cadr arg)))
				     (cdr form)))))))

      (define (check-special-cases name head form env)
	;; here curlet won't change (except for and/or/not, possibly apply, and map/for-each with a macro)

	;; this happens a lot!
	;;   (for-each (lambda (x) (write x) (newline)) more) 
	;;   (for-each (lambda (x) (display #\space cep) (write x cep)) args) [no 'do, only a dozen 'map cases]
	;; treat body as currently in lint-walk-body but expr lim == body len, then wrap in ~{~}?
	;;   will need to split out the formatter in lint-walk-body
	;; are the map cases for-each in disguise?
	;;   (map (lambda (x) (display x) (newline)) to-print)
#|
	(if (and (eq? head 'do) ;'for-each)
		 (tree-set-member 'not-a-symbol '(display write write-string write-line write-char newline) form))
	    (format *stderr* "~A~%" form))
|#
	;; what else? could we dive into funcs?

	;; TODO: extend define-macro stuff
	;; (eval (read (open-input-string expr))) -> eval-string?
	;; (eval (string->symbol "lcg:method:makeCodeVector"))=eval-string (eval 'p_lost_halls_r1)=sym

	;; (string-ref (substring...)) (string-set! (substring...))--an error
	;; (string-ref (symbol->string 't) 0) (string-ref (make-string 3 #\a) 2)

	;; better format checks, and accept %...[fsl] ? -> rewrite with ~
	;; (format (_ "Add to the sequence in row ~a.") (number->string (+ row 1))))
	;; (string-append (format...)) and (format ... (string-append))
	;;    in this case, if format port not #f/#t -> error
#|
	(if (member 'format form (lambda (a b) 
				    (and (pair? b)
					 (eq? (car b) a))))
	    (format *stderr* "~A~%" form))
|#
	(case head
	  ((memq assq memv assv member assoc)

	   (define (list-one? p)
	     (and (pair? p)
		  (pair? (cdr p))
		  (null? (cddr p))
		  (or (and (eq? (car p) 'list)
			   cadr)
		      (and (eq? (car p) 'quote)
			   (pair? (cadr p))
			   (null? (cdadr p))
			   (if (symbol? (caadr p))
			       (lambda (x) (list 'quote (caadr x)))
			       caadr)))))
			     
	   (if (= (length form) 4)
	       (let ((func (list-ref form 3)))
		 (if (symbol? func)
		     (let ((sig (procedure-signature (symbol->value func))))
		       (if (and sig
				(not (eq? (car sig) 'boolean?)))
			   (lint-format "~A is a questionable ~A function" name func head)))
		     (if (and (pair? func)
			      (= (length func) 3)
			      (eq? (car func) 'lambda)
			      (pair? (cadr func))
			      (pair? (caddr func)))
			 (if (not (member (length (cadr func)) '(2 -1)))
			     (lint-format "~A equality function (optional third arg) should take two arguments" name head)
			     (if (eq? head 'member)
				 (let ((eq (caddr func))
				       (args (cadr func)))
				   (if (and (memq (car eq) '(eq? eqv? equal?))
					    (eq? (car args) (cadr eq))
					    (pair? (caddr eq))
					    (eq? (car (caddr eq)) 'car)
					    (pair? (cdr (caddr eq)))
					    (pair? (cdr args))
					    (eq? (cadr args) (cadr (caddr eq))))
				       (lint-format "member might perhaps be ~A"
						    name
						    (if (or (eq? func 'eq?)
							    (eq? (car (caddr func)) 'eq?))
							'assq
							(if (eq? (car (caddr func)) 'eqv?) 
							    'assv 
							    'assoc))))))))))

	       (when (= (length form) 3)
		 (let ((selector (cadr form))
		       (items (caddr form)))
		   (let ((current-eqf (case head ((memq assq) 'eq?) ((memv assv) 'eqv?) (else 'equal?)))
			 (selector-eqf (eqf selector))
			 (one-item (and (memq head '(memq memv member)) (list-one? (caddr form)))))
		     ;; one-item assoc doesn't simplify cleanly
		     
		     (if one-item
			 (let* ((target (one-item items))
				(iter-eqf (eqf target)))
			   (lint-format "perhaps ~A" name 
					(if (or (symbol? target)
						(and (pair? target)
						     (not (eq? (car target) 'quote))))
					    (lists->string form `(,(cadr iter-eqf) ,selector ',target))
					    (lists->string form `(,(cadr iter-eqf) ,selector ,target)))))

			 (letrec ((duplicates? (lambda (lst fnc)
						 (and (pair? lst)
						      (or (fnc (car lst) (cdr lst))
							  (duplicates? (cdr lst) fnc)))))
				  (duplicate-constants? (lambda (lst fnc)
						 (and (pair? lst)
						      (or (and (constant? (car lst))
							       (fnc (car lst) (cdr lst)))
							  (duplicate-constants? (cdr lst) fnc))))))
			   (if (and (pair? items)
				    (or (eq? (car items) 'list)
					(and (eq? (car items) 'quote)
					     (pair? (cadr items)))))
			       (let ((baddy #f))
				 (catch #t 
				   (lambda () 
				     (if (eq? (car items) 'list) ; TODO: restrict to constants?
					 (set! baddy (duplicate-constants? (cdr items) (symbol->value head)))
					 (set! baddy (duplicates? (cadr items) (symbol->value head)))))
				   (lambda args #f))
				 (if (pair? baddy)
				     (lint-format "duplicated entry ~S in ~A" name (car baddy) items))))
					    
			   (if (and (symbol? (car selector-eqf))
				    (not (eq? (car selector-eqf) current-eqf)))
			       (lint-format "~A: perhaps ~A -> ~A" name form head 
					    (if (memq head '(memq memv member))
						(case (car selector-eqf) ((eq?) 'memq) ((eqv?) 'memv) ((equal?) 'member))
						(case (car selector-eqf) ((eq?) 'assq) ((eqv?) 'assv) ((equal?) 'assoc)))))

			   (if (and (pair? items)
				    (eq? (car items) 'list)
				    (every? code-constant? (cdr items)))
			       (lint-format "perhaps ~A -> '~A" name (truncated-list->string items) 
					    (truncated-list->string (map unquoted (cdr items))))))))

		   (when (and (memq head '(memq memv))
			      (pair? items)
			      (eq? (car items) 'quote)
			      (pair? (cadr items)))
		     (let ((bad (find-if (lambda (x)
					   (not (or (symbol? x)
						    (char? x)
						    (number? x)
						    (procedure? x) ; (memq abs '(1 #_abs 2)) !
						    (memq x '(#f #t () #<unspecified> #<undefined> #<eof>)))))
					 (cadr items))))
		       (if bad
			   (if (pair? bad)
			       (if (eq? (car bad) 'quote)
				   (lint-format "stray quote? ~A" name form)
				   (if (eq? (car bad) 'unquote)
				       (lint-format "stray comma? ~A" name form)
				       (lint-format "pointless list member: ~S in ~A" name bad form)))
			       (lint-format "pointless list member: ~S in ~A" name bad form)))))))))

	  ((car cdr 
	    caar cadr cddr cdar
	    caaar caadr caddr cdddr cdaar cddar cadar cdadr
            cadddr cddddr)

	   (let ((cxr? (lambda (s)
			 (and (pair? (cdr s))
			      (pair? (cadr s))
			      (memq (caadr s) '(car cdr cadr cddr cdar cdddr cddddr))))))
	     (if (and (not (= line-number last-simplify-boolean-line-number))
		      (cxr? form))
		 (let* ((arg1 (cadr form))
			(arg2 (and arg1 (cxr? arg1) (cadr arg1)))
			(arg3 (and arg2 (cxr? arg2) (cadr arg2))))
		   (set! last-simplify-boolean-line-number line-number)
		   (let* ((innards (lambda (c) (case c ((car) "a") ((cdr) "d") ((caar) "aa") ((cadr) "ad") ((cddr) "dd") ((cdar) "da")
						       ((caaar) "aaa") ((caadr) "aad") ((caddr) "add") ((cdddr) "ddd") 
						       ((cdaar) "daa") ((cddar) "dda") ((cdadr) "dad") ((cadar) "ada")
						       ((cddddr) "dddd") ((cadddr) "addd"))))
			  (cxr (string-append (innards (car form)) 
					      (innards (car arg1)) 
					      (if arg2 (innards (car arg2)) "")
					      (if arg3 (innards (car arg3)) ""))))
		     (if (< (length cxr) 5)
			 (lint-format "perhaps ~A" name (lists->string form `(,(string->symbol (string-append "c" cxr "r")) ,(cadr (or arg3 arg2 arg1)))))
			 ;; if it's car|cdr followed by cdr's, use list-ref|tail
			 (if (not (char-position #\a cxr))
			     (lint-format "perhaps ~A" name (lists->string form `(list-tail ,(cadr (or arg3 arg2 arg1)) ,(length cxr))))
			     (if (not (char-position #\a (substring cxr 1)))
				 (lint-format "perhaps ~A" name (lists->string form `(list-ref ,(cadr (or arg3 arg2 arg1)) ,(- (length cxr) 1))))
				 (set! last-simplify-boolean-line-number -1))))))))

	   (when (and (eq? head 'car)          ; (car (list-tail x y)) -> (list-ref x y)
		      (pair? (cadr form))
		      (eq? (caadr form) 'list-tail))
	     (lint-format "perhaps ~A" name (lists->string form `(list-ref ,(cadadr form) ,(caddr (cadr form))))))

	   (if (and (memq head '(car cdr))
		    (pair? (cadr form))
		    (eq? (caadr form) 'cons))
	       (lint-format "(~A~A) is the same as~A"
			    name head
			    (truncated-list->string (cadr form))
			    (if (eq? head 'car)
				(truncated-list->string (cadadr form))
				(truncated-list->string (caddr (cadr form))))))

	   (when (and (memq head '(car cadr caddr cadddr))
		    (pair? (cadr form)))
	     (if (memq (caadr form) '(string->list vector->list))    ; (car (string->list x)) -> (string-ref x 0)
		 (lint-format "perhaps ~A" name (lists->string form `(,(if (eq? (caadr form) 'string->list) 'string-ref 'vector-ref)
								      ,(cadadr form) 
								      ,(case head ((car) 0) ((cadr) 1) ((caddr) 2) (else 3)))))
		 (if (and (memq (caadr form) '(reverse reverse!))
			  (symbol? (cadadr form)))
		     (lint-format "perhaps ~A" name    ; (car (reverse x)) -> (list-ref x (- (length x) 1))
				  (lists->string form `(list-ref ,(cadadr form) 
							(- (length ,(cadadr form)) 
							   ,(case head ((car) 1) ((cadr) 2) ((caddr) 3) (else 4))))))))))

	  ((set-car!)
	   (when (and (= (length form) 3)
		      (pair? (cadr form)))
	     (let ((target (cadr form))
		   (cdr-count (lambda (c)
				(case c ((cdr) 1) ((cddr) 2) ((cdddr) 3) (else 4)))))
	       (cond ((eq? (car target) 'list-tail)        ; (set-car! (list-tail x y) z) -> (list-set! x y z)
		      (lint-format "perhaps ~A" name 
                        (lists->string form `(list-set! ,(cadr target) ,(caddr target) ,(caddr form)))))
		     ((memq (car target) '(cdr cddr cdddr cddddr))
		      (set! last-simplify-boolean-line-number line-number)
		      (if (and (pair? (cadr target))
			       (memq (caadr target) '(cdr cddr cdddr cddddr)))
			  (lint-format "perhaps ~A" name       ; (set-car! (cdr (cddr x)) y) -> (list-set! x 3 y)
				       (lists->string form `(list-set! ,(cadadr target)
								       ,(+ (cdr-count (car target)) (cdr-count (caadr target))) 
								       ,(caddr form))))
			  (lint-format "perhaps ~A" name       ; (set-car! (cdr x) y) -> (list-set! x 1 y)
				       (lists->string form `(list-set! ,(cadr target) 
								       ,(cdr-count (car target)) 
								       ,(caddr form))))))))))

	  ((and or not)
	   (if (not (= line-number last-simplify-boolean-line-number))
	       (let ((val (simplify-boolean form () () env)))
		 (set! last-simplify-boolean-line-number line-number)
		 (if (not (equal? form val))
		     (lint-format "perhaps ~A" name (lists->string form val))))))

	  ((=)
	   (if (and (> (length form) 2)
		    (any-real? (cdr form)))
	       (lint-format "= can be troublesome with floats: ~A" name (truncated-list->string form)))
	   (let ((cleared-form (cons = (remove-if (lambda (x) (not (number? x))) (cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true: ~A" name (truncated-list->string form))))
	   (when (and (= (length form) 3)
		      (eqv? (caddr form) 0))
	     (let ((arg (cadr form)))
	       (when (and (pair? arg)
			  (eq? (car arg) '-)
			  (= (length arg) 3))
		 (lint-format "perhaps ~A" name (lists->string form `(= ,(cadr arg) ,(caddr arg)))))))
	   (check-char-cmp name head form))

	  ((< > <= >=) ; '= handled above
	   (let ((cleared-form (cons (car form) ; keep operator
				     (remove-if (lambda (x) 
						  (not (number? x))) 
						(cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true: ~A" name (truncated-list->string form))))
	   (when (and (= (length form) 3)
		      (eqv? (caddr form) 0))
	     (let ((arg (cadr form)))
	       (if (and (pair? arg)
			(eq? (car arg) '-)
			(= (length arg) 3))
		   (lint-format "perhaps ~A" name (lists->string form `(,(car form) ,(cadr arg) ,(caddr arg)))))))

	   (check-char-cmp name head form))
	  ;; could change (> x 0) to (positive? x) and so on, but the former is clear and ubiquitous

	  ((char<? char>? char<=? char>=? char=? 
	    char-ci<? char-ci>? char-ci<=? char-ci>=? char-ci=?)
	   (let ((cleared-form (cons (car form) ; keep operator
				     (remove-if (lambda (x) 
						  (not (char? x))) 
						(cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true: ~A" name (truncated-list->string form)))))
	     
	  ((string<? string>? string<=? string>=? string=? 
	    string-ci<? string-ci>? string-ci<=? string-ci>=? string-ci=?)
	   (let ((cleared-form (cons (car form) ; keep operator
				     (remove-if (lambda (x) 
						  (not (string? x))) 
						(cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true: ~A" name (truncated-list->string form)))))

	  ((length)
	   (if (and (pair? (cdr form))
		    (pair? (cadr form))
		    (memq (caadr form) '(reverse reverse! list->vector vector->list list->string string->list let->list)))
	       (lint-format "perhaps ~A" name (lists->string form `(length ,(cadadr form))))))
	     
	  ((call-with-exit)
	   (let ((return (and (pair? (cdr form))
			      (pair? (cadr form))
			      (eq? (caadr form) 'lambda)
			      (pair? (cdadr form))
			      (pair? (cadadr form))
			      (car (cadadr form)))))
	     (if (symbol? return)
		 (let ((body (cddadr form)))
		   (if (not (tree-member return body))
		       (lint-format "exit-function appears to be unused: ~A" name (truncated-list->string form)))))))

	  ((call/cc call-with-current-continuation)
	   (let ((continuation (and (pair? (cdr form))
				    (pair? (cadr form))
				    (eq? (caadr form) 'lambda)
				    (pair? (cdadr form))
				    (pair? (cddadr form))
				    (pair? (cadadr form))
				    (car (cadadr form)))))
	     (if (symbol? continuation)
		 (let ((body (cddadr form)))
		   (if (and (not (eq? continuation (car body)))
			    (not (tree-set-member continuation '(lambda lambda* define define* curlet error apply) body)))
		       (lint-format "perhaps ~A could be call-with-exit: ~A" name head (truncated-list->string form)))))))

	  ((call-with-input-string call-with-input-file call-with-output-file)
	   ;; call-with-output-string func is the first arg, not second, but these checks get no hits
	   (let ((port (and (pair? (cdr form))
			    (pair? (cddr form))
			    (pair? (caddr form))
			    (eq? (caaddr form) 'lambda)
			    (pair? (cdaddr form))
			    (pair? (cadr (caddr form)))
			    (car (cadr (caddr form))))))
	     (if (symbol? port)
		 (let ((body (cddr (caddr form))))
		   (if (not (tree-member port body))
		       (lint-format "port appears to be unused: ~A" name (truncated-list->string form)))))))

	  ((/)
	   (if (pair? (cdr form))
	       (if (and (null? (cddr form))
			(number? (cadr form))
			(zero? (cadr form)))
		   (lint-format "attempt to invert zero: ~A" name (truncated-list->string form))
		   (if (and (pair? (cddr form))
			    (memv 0 (cddr form)))
		       (lint-format "attempt to divide by 0: ~A" name (truncated-list->string form))))))
	  
	  ((copy)
	   (if (and (pair? (cdr form))
		    (or (number? (cadr form))
			(boolean? (cadr form))
			(char? (cadr form))
			(and (pair? (cadr form))
			     (memq (caadr form) '(copy string-copy)))))
	       (lint-format "~A could be ~A" name form (cadr form))
	       (if (and (pair? (cdr form)) (equal? (cadr form) '(owlet)))
		   (lint-format "~A could be (owlet): owlet is copied internally" name form))))
	  
	  ((string-copy)
	   (if (and (pair? (cdr form))
		    (pair? (cadr form))
		    (memq (caadr form) '(copy string-copy)))
	       (lint-format "~A could be ~A" name form (cadr form))))

	  ((string)
	   (if (every? (lambda (x) (and (char? x) (not (member x '(#\null #\newline #\escape))))) (cdr form)) ;#\linefeed -> #\newline in reader
	       (lint-format "~A could be ~S" name form (apply string (cdr form)))))

	  ((string? number?)
	   (if (and (pair? (cdr form))
		    (pair? (cadr form))
		    (eq? (caadr form) (if (eq? head 'string?) 'number->string 'string->number)))
	       (lint-format "perhaps ~A" name (lists->string form (cadr form)))
	       (if (and (= (length form) 2)
			(not (symbol? (cadr form)))
			(not (= line-number last-simplify-boolean-line-number)))
		   (let ((expr (simplify-boolean form () () env)))
		     (if (not (equal? expr form))
			 (lint-format "perhaps ~A" name (lists->string form expr)))))))

	  ((symbol? integer? rational? real? complex? float? keyword? gensym? byte-vector? list?
		    char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair?
		    output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?)
	   (if (and (= (length form) 2)
		    (not (symbol? (cadr form)))
		    (not (= line-number last-simplify-boolean-line-number)))
	       (let ((expr (simplify-boolean form () () env)))
		 (if (not (equal? expr form))
		     (lint-format "perhaps ~A" name (lists->string form expr))))))

	  ((vector-ref list-ref hash-table-ref let-ref int-vector-ref float-vector-ref)
	   (unless (= line-number last-checker-line-number)
	     (if (= (length form) 3)
		 (let ((seq (cadr form)))
		   (if (and (pair? seq)
			    (eq? (car seq) head) ; perhaps instead: (memq (car seq) '(vector-ref list-ref hash-table-ref let-ref))
			    (= (length seq) 3))
		       (let ((seq1 (cadr seq)))
			 (if (and (pair? seq1)
				  (eq? (car seq1) head)
				  (= (length seq1) 3))
			     (lint-format "perhaps ~A" name (lists->string form `(,(cadr seq1) ,(caddr seq1) ,(caddr seq) ,(caddr form))))
			     (lint-format "perhaps ~A" name (lists->string form `(,seq1 ,(caddr seq) ,(caddr form)))))))))
	     (set! last-checker-line-number line-number)))

	  ((vector-set! list-set! hash-table-set! float-vector-set! int-vector-set!)
	   (if (= (length form) 4)
	       (let ((target (cadr form))
		     (index (caddr form))
		     (val (cadddr form)))
		 (if (and (pair? val)
			  (= (length val) 3)
			  (eq? target (cadr val))
			  (equal? index (caddr val))
			  (memq (car val) '(vector-ref list-ref hash-table-ref float-vector-ref int-vector-ref)))
		     (lint-format "redundant?: ~A" name (truncated-list->string form))
		     (if (and (pair? target) 
			      (memq (car target) '(vector-ref list-ref hash-table-ref float-vector-ref int-vector-ref)))
			 (lint-format "perhaps ~A" name (lists->string form `(set! (,@(cdr target) ,index) ,val)))
			 (if (or (code-constant? (cadr form))
				 (and (pair? (cadr form))
				      (memq (caadr form) '(make-vector vector make-string string make-list list append cons vector-append copy))))
			     (lint-format "perhaps ~A" name (lists->string form val))))))))
	  
	  ((object->string)
	   (if (pair? (cdr form))
	       (if (and (pair? (cadr form))
			(eq? (caadr form) 'object->string))
		   (lint-format "~A could be ~A" name form (cadr form))
		   (if (and (pair? (cddr form))
			    (not (pair? (caddr form)))
			    (or (not (symbol? (caddr form))) (keyword? (caddr form)))
			    (not (memq (caddr form) '(#f #t :readable))))
		       (lint-format "bad second argument: ~A" name (caddr form))))))

	  ((display)
	   (if (and (= (length form) 2)
		    (pair? (cadr form))
		    (eq? (caadr form) 'format)
		    (not (cadadr form)))
	       (lint-format "perhaps ~A" name (lists->string form `(format () ,@(cddadr form))))))

	  ((format)
	   (if (= (length form) 3)
	       (lint-format "perhaps ~A" name (lists->string form (cadr form)))))

	  ((make-vector)
	   (if (and (= (length form) 4)
		    (code-constant? (caddr form))
		    (not (real? (caddr form)))
		    (eq? (cadddr form) #t))
	       (lint-format "~A won't create an homogenous vector" name form)))

	  ((reverse reverse! list->vector vector->list list->string string->list symbol->string string->symbol number->string)
	   ;; not string->number -- no point in copying a number and it's caught below
	   (let ((inverses '((reverse . reverse) 
			     (reverse! . reverse!) 
			     (list->vector . vector->list)
			     (vector->list . list->vector)
			     (symbol->string . string->symbol)
			     (string->symbol . symbol->string)
			     (list->string . string->list)
			     (string->list . list->string)
			     (number->string . string->number))))
	     (when (and (pair? (cdr form))
			(pair? (cadr form))
			(pair? (cdadr form)))
	       (cond ((eq? (caadr form) (let ((p (assq head inverses))) (and (pair? p) (cdr p))))
		      (lint-format "~A could be (copy ~A)" name form (cadadr form)))
		     ((and (eq? head 'list->string)
			   (eq? (caadr form) 'vector->list))
		      (lint-format "perhaps ~A" name (lists->string form `(copy ,(cadadr form) (make-string (length ,(cadadr form)))))))
		     ((and (eq? head 'list->vector)
			   (eq? (caadr form) 'string->list))
		      (lint-format "perhaps ~A" name (lists->string form `(copy ,(cadadr form) (make-vector (length ,(cadadr form)))))))
		     ((and (eq? head 'vector->list)
			   (eq? (caadr form) 'make-vector))
		      (lint-format "perhaps ~A" name (lists->string form `(make-list ,@(cdadr form)))))
		     ((and (eq? head 'list->vector)
			   (eq? (caadr form) 'make-list))
		      (lint-format "perhaps ~A" name (lists->string form `(make-vector ,@(cdadr form)))))
		     ((and (eq? head 'list->string)
			   (memq (caadr form) '(reverse reverse!)))
		      (lint-format "perhaps ~A" name (lists->string form `(reverse (apply string ,(cadadr form))))))
		     )))

	   (when (and (eq? head 'reverse)
		      (pair? (cdr form))
		      (pair? (cadr form)))
	     (let ((arg (cadr form)))
	       (if (and (memq (car arg) '(cdr list-tail)) ; (reverse (cdr (reverse lst))) = all but last of lst -> copy to len-1
			(pair? (cadr arg))
			(eq? (caadr arg) 'reverse)
			(symbol? (cadadr arg)))
		   (lint-format "perhaps ~A" name 
                     (lists->string form `(copy ,(cadadr arg) (make-list (- (length ,(cadadr arg)) ,(if (eq? (car arg) 'cdr) 1 (caddr arg))))))))

	       (if (and (eq? (car arg) 'cons)      ; (reverse (cons x (reverse lst))) -- adds x to end -- (append lst (list x))
			(pair? (caddr arg))
			(eq? (car (caddr arg)) 'reverse))
		   (lint-format "perhaps ~A" name (lists->string form `(append ,(cadr (caddr arg)) (list ,(cadr arg)))))))))

	  ((char->integer integer->char symbol->keyword keyword->symbol string->number)
	   (let ((inverses '((char->integer . integer->char)
			     (integer->char . char->integer)
			     (symbol->keyword . keyword->symbol)
			     (keyword->symbol . symbol->keyword)
			     (string->number . number->string))))
	     (if (and (pair? (cdr form))
		      (pair? (cadr form))
		      (pair? (cdadr form))
		      (eq? (caadr form) (let ((p (assq head inverses))) (and (pair? p) (cdr p)))))
		 (lint-format "~A could be ~A" name form (cadadr form)))))
	  
	  ((string-append)
	   (if (not (= line-number last-checker-line-number))
	       (let ((args (remove-all "" (splice-if (lambda (x) (eq? x 'string-append)) (cdr form)))))
		 (if (null? args)
		     (lint-format "perhaps ~A" name (lists->string form ""))
		     (if (null? (cdr args))
			 (lint-format "perhaps ~A, or use copy" name (lists->string form (car args)))
			 (if (every? string? args)
			     (lint-format "perhaps ~A" name (lists->string form (apply string-append args)))
			     (if (not (equal? args (cdr form)))
				 (lint-format "perhaps ~A" name (lists->string form `(string-append ,@args)))))))
		 (set! last-checker-line-number line-number))))

	  ((vector-append)
	   (if (not (= line-number last-checker-line-number))
	       (let ((args (remove-all #() (splice-if (lambda (x) (eq? x 'vector-append)) (cdr form)))))
		 (if (null? args)
		     (lint-format "perhaps ~A" name (lists->string form #()))
		     (if (null? (cdr args))
			 (lint-format "perhaps ~A" name (lists->string form `(copy ,(car args))))
			 (if (every? vector? args)
			     (lint-format "perhaps ~A" name (lists->string form (apply vector-append args)))
			     (if (not (equal? args (cdr form)))
				 (lint-format "perhaps ~A" name (lists->string form `(vector-append ,@args)))))))
		 (set! last-checker-line-number line-number))))

	  ((cons)
	   (if (and (= (length form) 3)
		    (pair? (caddr form))
		    (eq? (caaddr form) 'list))
	       (lint-format "perhaps ~A" name (lists->string form `(list ,(cadr form) ,@(cdaddr form))))))

	  ((append)
	   (unless (= line-number last-checker-line-number)
	     (set! last-checker-line-number line-number)
	     (letrec ((splice-append (lambda (lst)
				       (cond ((null? lst) ())
					     ((pair? lst)
					      (if (and (pair? (car lst))
						       (eq? (caar lst) 'append))
						  (if (null? (cdar lst))
						      (cons () (splice-append (cdr lst)))
						      (append (splice-append (cdar lst)) (splice-append (cdr lst))))
						  (cons (car lst) (splice-append (cdr lst)))))
					     (#t lst)))))
	       (let ((new-args (splice-append (cdr form))))     ; (append '(1) (append '(2) '(3))) -> (append '(1) '(2) '(3))
		 (let ((len1 (length new-args)))

		   (define (distribute-quote x)
		     (map (lambda (item)
			    (if (or (symbol? item)
				    (pair? item))
				`(quote ,item)
				item))
			  x))
	   
		   (define (append->list . items)
		     (let ((lst (list 'list)))
		       (for-each (lambda (item)
				   (set! lst (append lst (if (eq? (car item) 'list)
							     (cdr item)
							     (distribute-quote (cadr item))))))
				 items)
		       lst))
				   
		   (case len1
		     ((0)					; (append) -> ()
		      (lint-format "perhaps ~A" name (lists->string form ())))
		     ((1)                                 ; (append x) -> x
		      (lint-format "perhaps ~A" name (lists->string form (car new-args))))
		     ((2)
		      (if (or (null? (cadr new-args))           ; (append (list x) ()) -> (list x)
			      (quoted-null? (cadr new-args))
			      (equal? (cadr new-args) '(list)))
			  (lint-format "perhaps clearer: ~A" name (lists->string form `(copy ,(car new-args))))
			  (if (null? (car new-args))            ; (append () x) -> x
			      (lint-format "perhaps ~A" name (lists->string form (cadr new-args)))
			      (if (and (pair? (car new-args))   ; (append (list x y) '(z)) -> (list x y 'z)
				       (or (eq? (caar new-args) 'list)
					   (quoted-undotted-pair? (car new-args)))
				       (pair? (cadr new-args))
				       (or (eq? (caadr new-args) 'list)
					   (quoted-undotted-pair? (cadr new-args))))
				  (lint-format "perhaps ~A" name (lists->string form (apply append->list new-args)))
				  (if (not (equal? (cdr form) new-args))
				      (lint-format "perhaps ~A" name (lists->string form `(append ,@new-args))))))))
		     (else
		      (if (every? (lambda (item)
				    (and (pair? item)
					 (or (eq? (car item) 'list)
					     (quoted-undotted-pair? item))))
				  new-args)
			  (lint-format "perhaps ~A" name (lists->string form (apply append->list new-args)))
			  (if (not (equal? (cdr form) new-args))
			      (lint-format "perhaps ~A" name (lists->string form `(append ,@new-args))))))))))))

	  ((apply)

	;; (apply func (cons port params)) -> (apply func port params)
	;; (apply string (reverse chars)) -> (reverse (aplpy string chars))
	;; (apply (spell-handler spell) (list kchar)) -> ((spell...) kchar)
	;; (apply (svc-proc svc) (list kchar knpc)) -> same
	;; (apply select (list utt)) -> (select utt)?!
	;; (apply proc) -> (proc)
	;; (apply (lambda (stone) stone) stone) -- this is (car stone)?!
	;; (apply (lambda (o) (make-vector size o)) rest) -> (make-vector size (car rest))?
	;; (apply cerr (append (list nl "XPointer parser error: ") text (list nl))) -> (apply cerr nl "X..." (append text (list nl)))?

	   (let ((function? (lambda (f)
			      (or (and (symbol? f)
				       (let ((func (symbol->value f *e*)))
					 (or (procedure? func)
					     (let ((e (or (var-member f env) (hash-table-ref globals f))))
					       (and (var? e)
						    (memq (var-ftype e) '(define define* lambda lambda*)))))))
				  (and (pair? f)
				       (memq (car f) '(lambda lambda*)))))))

	     (if (and (pair? (cdr form))
		      (not (symbol? (cadr form)))
		      (not (applicable? (cadr form))))
		 (lint-format "~S is not applicable: ~A" name (cadr form) (truncated-list->string form))
		 (let ((len (length form)))
		   (when (> len 2)
		     (if (and (not (list? (form (- len 1))))
			      (code-constant? (form (- len 1))))
			 (lint-format "last argument should be a list: ~A" name (truncated-list->string form))
			 (if (and (= len 3)
				  (pair? (caddr form))         ; (apply f (list a b)) -> (f a b)
				  (eq? (caaddr form) 'list)
				  ;; macros are different here
				  (function? (cadr form)))
			     (lint-format "perhaps ~A" name (lists->string form `(,(cadr form) ,@(cdaddr form))))
			     (if (and (or (not (every? code-constant? (cddr form)))
					  (catch #t
					    (lambda ()
					      (let ((val (eval form)))
						(lint-format "perhaps ~A -> ~S" name form val)
						#t))
					    (lambda args #f)))
				      (symbol? (cadr form)))
				 (let ((func (symbol->value (cadr form) *e*)))
				   (if (procedure? func)
				       (let ((ary (arity func)))
					 (if (and (pair? ary)
						  (> (- (length (cddr form)) 1) (cdr ary))) ; last apply arg might be var=()
					     (lint-format "too many arguments for ~A: ~A" name (cadr form) form)))))))))))))

	  ((format snd-display)
	   (if (< (length form) 3)
	       (begin
		 (if (< (length form) 2)
		     (lint-format "~A has too few arguments: ~A" name head (truncated-list->string form))
		     (if (and (pair? (cadr form))
			      (eq? (caadr form) 'format))
			 (lint-format "redundant format: ~A" name (truncated-list->string form))
			 (if (and (code-constant? (cadr form))
				  (not (string? (cadr form))))
			     (lint-format "format with one argument takes a string: ~A" name (truncated-list->string form)))))
		 env)
	       (let ((control-string (if (string? (cadr form)) (cadr form) (caddr form)))
		     (args (if (string? (cadr form)) (cddr form) (cdddr form))))
		 
		 (define (count-directives str name form)
		   (let ((curlys 0)
			 (dirs 0)
			 (pos (char-position #\~ str)))
		     (if pos
			 (let ((len (length str))
			       (tilde-time #t)) 
			   (do ((i (+ pos 1) (+ i 1)))
			       ((>= i len))
			     (let ((c (string-ref str i)))
			       (if tilde-time
				   (begin
				     (if (and (= curlys 0)
					      (not (memq c '(#\~ #\T #\t #\& #\% #\^ #\| #\newline #\}))) ; ~* consumes an arg
					      (not (call-with-exit
						    (lambda (return)
						      (do ((k i (+ k 1)))
							  ((= k len) #f)
							;; this can be confused by pad chars in ~T
							(if (and (not (char-numeric? (string-ref str k)))
								 (not (char=? (string-ref str k) #\,)))
							    (return (char-ci=? (string-ref str k) #\t))))))))
					 (begin
					   ;; the possibilities are endless, so I'll stick to the simplest
					   (if (not (vector-ref format-control-char (char->integer c)))
					       (lint-format "unrecognized format directive: ~C in ~S, ~S" name c str form))
					   (set! dirs (+ dirs 1))
					   
					   ;; ~n so try to figure out how many args are needed (this is not complete)
					   (when (char-ci=? c #\n)
					     (let ((j (+ i 1)))
					       (if (>= j len)
						   (lint-format "missing format directive: ~S" name str)
						   (begin
						     ;; if ,n -- add another, if then not T, add another
						     (if (char=? (string-ref str j) #\,)
							 (if (>= (+ j 1) len)
							     (lint-format "missing format directive: ~S" name str)
							     (if (char-ci=? (string-ref str (+ j 1)) #\n)
								 (begin
								   (set! dirs (+ dirs 1))
								   (set! j (+ j 2)))
								 (if (char-numeric? (string-ref str (+ j 1)))
								     (set! j (+ j 2))
								     (set! j (+ j 1))))))
						     (if (>= j len)
							 (lint-format "missing format directive: ~S" name str)
							 (if (not (char-ci=? (string-ref str j) #\t))
							     (set! dirs (+ dirs 1))))))))))
				     
				     (set! tilde-time #f)
				     (case c 
				       ((#\{) (set! curlys (+ curlys 1)))
				       ((#\}) (set! curlys (- curlys 1)))
				       ((#\^ #\|)
					(if (zero? curlys)
					    (lint-format "~A has ~C outside ~~{~~}?" name str c)))))
				   (begin
				     (set! pos (char-position #\~ str i))
				     (if pos 
					 (begin
					   (set! tilde-time #t)
					   (set! i pos))
					 (set! i len))))))
			   
			   (if tilde-time
			       (lint-format "~A control string ends in tilde: ~A" name head (truncated-list->string form)))))
		     
		     (if (not (= curlys 0))
			 (lint-format "~A has ~D unmatched ~A~A: ~A"
				      name head 
				      (abs curlys) 
				      (if (positive? curlys) "{" "}") 
				      (if (> curlys 1) "s" "") 
				      (truncated-list->string form)))
		     dirs))
		 
		 (if (not (string? control-string))
		     (if (not (proper-list? args))
			 (lint-format "~S looks suspicious" name form))
		     (let ((ndirs (count-directives control-string name form))
			   (nargs (if (or (null? args) (pair? args)) (length args) 0)))
		       (if (not (= ndirs nargs))
			   (lint-format "~A has ~A arguments: ~A" 
					name head 
					(if (> ndirs nargs) "too few" "too many")
					(truncated-list->string form))
			   (if (and (not (cadr form))
				    (zero? ndirs)
				    (not (char-position #\~ control-string)))
			       (lint-format "~A could be ~S, (format is a no-op here)" name form (caddr form)))))))))

	  ((sort!)
	   (if (= (length form) 3)
	       (let ((func (caddr form)))
		 (if (memq func '(= eq? eqv? equal? string=? char=? string-ci=? char-ci=?))
		     (lint-format "sort! with ~A may hang: ~A" name func (truncated-list->string form))
		     (if (symbol? func)
			 (let ((sig (procedure-signature (symbol->value func))))
			   (if (and sig
				    (not (eq? (car sig) 'boolean?)))
			       (lint-format "~A is a questionable sort! function" name func))))))))

	  ((substring) 
	   (if (every? code-constant? (cdr form))
	       (catch #t
		 (lambda ()
		   (let ((val (eval form)))
		     (lint-format "perhaps ~A -> ~S" name form val)))
		 (lambda (type info)
		   (lint-format "~A -> ~A~%" name form (apply format #f info))))
	       
	       (let ((str (cadr form)))
		 (if (and (pair? str)
			  (eq? (car str) 'substring)
			  (pair? (cddr form))
			  (null? (cdddr form))
			  (null? (cdddr str)))
		     (if (and (integer? (caddr form))
			      (integer? (caddr str)))
			 (lint-format "perhaps ~A" name 
				      (lists->string form `(substring ,(cadr str) ,(+ (caddr str) (caddr form)))))
			 (lint-format "perhaps ~A" name 
				      (lists->string form `(substring ,(cadr str) (+ ,(caddr str) ,(caddr form)))))))
		 ;; end indices are complicated -- since this rarely happens, not worth the trouble
		 (if (and (integer? (caddr form))
			  (zero? (caddr form))
			  (null? (cdddr form)))
		     (lint-format "perhaps clearer: ~A" name (lists->string form `(copy ,str)))))))

	  ((list-tail)
	   (if (= (length form) 3)
	       (if (and (integer? (caddr form))
			(zero? (caddr form)))
		   (lint-format "perhaps ~A" name (lists->string form (cadr form)))
		   (if (and (pair? (cadr form))
			    (eq? (caadr form) 'list-tail))
		       (if (and (integer? (caddr form))
				(integer? (caddr (cadr form))))
			   (lint-format "perhaps ~A" name 
					(lists->string form `(list-tail ,(cadadr form) ,(+ (caddr (cadr form)) (caddr form)))))
			   (lint-format "perhaps ~A" name 
					(lists->string form `(list-tail ,(cadadr form) (+ ,(caddr (cadr form)) ,(caddr form))))))))))

	  ((eq?) 
	   (if (< (length form) 3)
	       (lint-format "eq? needs 2 arguments: ~A" name (truncated-list->string form))
	       (let ((arg1 (cadr form))
		     (arg2 (caddr form)))
		 (let ((eq1 (eqf arg1))
		       (eq2 (eqf arg2)))
		   (if (or (eq? (car eq1) 'equal?)
			   (eq? (car eq2) 'equal?))
		       (lint-format "eq? should be equal? in ~S" name form)
		       (if (or (eq? (car eq1) 'eqv?)
			       (eq? (car eq2) 'eqv?))
			   (lint-format "eq? should be eqv? in ~S" name form))))
		 
		 (let ((expr 'unset))             ; (eq? e #f) or (eq? #f e) -> (not e)
		   (if (not arg1)
		       (set! expr (simplify-boolean `(not ,arg2) () () env))
		       (if (not arg2)
			   (set! expr (simplify-boolean `(not ,arg1) () () env))
			   (if (and (or (null? arg1)
					(quoted-null? arg1))
				    (not (code-constant? arg2)))
			       (set! expr `(null? ,arg2))
			       (if (and (or (null? arg2)
					    (quoted-null? arg2))
					(not (code-constant? arg1)))
				   (set! expr `(null? ,arg1))
				   (if (and (eq? arg1 #t)
					    (pair? arg2)
					    (eq? (return-type (car arg2) env) 'boolean?))
				       (set! expr arg2)
				       (if (and (eq? arg2 #t)
						(pair? arg1)
						(eq? (return-type (car arg1) env) 'boolean?))
					   (set! expr arg1)))))))
		   (if (not (eq? expr 'unset))
		       (lint-format "perhaps ~A" name (lists->string form expr)))))))
				  
	  ((eqv? equal? morally-equal?) 
	   (if (< (length form) 3)
	       (lint-format "~A needs 2 arguments: ~A" name head (truncated-list->string form))
	       (let ((arg1 (cadr form))
		     (arg2 (caddr form)))
		 (let ((eq1 (eqf arg1))
		       (eq2 (eqf arg2)))
		   (if (or (eq? (car eq1) 'equal?)
			   (eq? (car eq2) 'equal?))
		       (if (not (memq head '(equal? morally-equal?)))
			   (lint-format "~A should be equal? in ~S" name head form))
		       (if (or (eq? (car eq1) 'eqv?)
			       (eq? (car eq2) 'eqv?))
			   (if (and (not (eq? head 'eqv?))
				    (or (not (eq? head 'morally-equal?))
					(and (or (not (number? arg1))
						 (rational? arg1)) ; here we have float-equal-epsilon
					     (or (not (number? arg2))
						 (rational? arg2)))))
			       (lint-format "~A ~A be eqv? in ~S" name head (if (eq? head 'eq?) "should" "could") form))
			   (if (or (eq? (car eq1) 'eq?)
				   (eq? (car eq2) 'eq?))
			       (if (or (not arg1) (not arg2))
				   (lint-format "~A could be not: ~A" name head
						(lists->string form `(not ,(or arg1 arg2))))
				   (if (or (null? arg1) (null? arg2)
					   (quoted-null? arg1) (quoted-null? arg2))
				       (lint-format "~A could be null?: ~A" name head
						    (lists->string form 
								   (if (or (null? arg1) (quoted-null? arg1))
								       `(null? ,arg2)
								       `(null? ,arg1))))
				       (if (not (eq? head 'eq?))
					   (lint-format "~A could be eq? in ~S" name head form)))))))))))
				  
	  ((map for-each)
	   (let* ((len (length form))
		  (args (- len 2)))
	     (if (< len 3)
		 (lint-format "~A missing argument~A in: ~A"
			      name head 
			      (if (= len 2) "" "s") 
			      (truncated-list->string form))
		 (let ((func (cadr form))
		       (ary #f))
		   (if (and (symbol? func)
			    (defined? func)
			    (procedure? (symbol->value func *e*)))
		       (set! ary (arity (symbol->value func *e*)))
		       
		       (if (and (pair? func)
				(memq (car func) '(lambda lambda*))
				(pair? (cadr func)))
			   (let ((arglen (length (cadr func))))
			     (if (eq? (car func) 'lambda)
				 (if (negative? arglen)
				     (set! ary (cons (abs arglen) 512000))
				     (set! ary (cons arglen arglen)))
				 (if (or (negative? arglen)
					 (memq :rest (cadr func)))
				     (set! ary (cons 0 512000))
				     (set! ary (cons 0 arglen)))))))
		   
		   (if (pair? ary)
		       (if (< args (car ary))
			   (lint-format "~A has too few arguments in: ~A"
					name head 
					(truncated-list->string form))
			   (if (> args (cdr ary))
			       (lint-format "~A has too many arguments in: ~A"
					    name head 
					    (truncated-list->string form)))))
		   (for-each 
		    (lambda (obj)
		      (if (and (pair? obj)
			       (memq (car obj) '(vector->list string->list let->list)))
			  (lint-format "~A could be simplified to: ~A ; (~A accepts non-list sequences)" 
				       name
				       (truncated-list->string obj) 
				       (truncated-list->string (cadr obj))
				       head)))
		    (cddr form))

		   (when (and (eq? head 'map)
			      (memq func '(char-downcase char-upcase))
			      (pair? (caddr form))
			      (eq? (caaddr form) 'string->list))
		     (lint-format "perhaps ~A" name (lists->string form `(string->list (,(if (eq? func 'char-upcase) 'string-upcase 'string-downcase) 
											,(cadr (caddr form)))))))
		   ))))
	  
	  ((magnitude)
	   (if (and (= (length form) 2)
		    (memq (->type (cadr form)) '(integer? rational? real?)))
	       (lint-format "perhaps use abs here: ~A" name form)))

	  ((null eq eqv equal) ; (null (cdr...)) 
	   (if (not (var-member head env))
	       (lint-format "misspelled '~A? in ~A?" name head form)))

	  ((open-input-file open-output-file)
	   (if (and (pair? (cdr form))
		    (pair? (cddr form))
		    (string? (caddr form))
		    (not (memv (string-ref (caddr form) 0) '(#\r #\w #\a)))) ; b + then e m c x if gcc
	       (lint-format "unexpected mode: ~A" name form)))

	  ((catch)
	   ;; catch tag is tricky -- it is evaluated, then eq? matches at error time, so we need
	   ;;   to catch constants that can't be eq?
	   (if (= (length form) 4)
	       (let ((tag (cadr form)))
		 (if (or (and (not (pair? tag))
			      (or (number? tag) (char? tag) (length tag)))
			 (and (pair? tag)
			      (eq? (car tag) 'quote)
			      (or (not (pair? (cdr tag)))
				  (length (cadr tag)))))
		     (lint-format "catch tag ~S is unreliable (catch uses eq? to match tags)" name (cadr form))))))

	  ((load) ; pick up the top level declarations
	   (if (>= (length form) 2)
	       (scan form)))

	  ((values)
	   (if (= (length form) 2)
	       (lint-format "perhaps ~A" name (lists->string form (cadr form)))))

	  ((*s7*)
	   (if (= (length form) 2)
	       (let ((arg (cadr form)))
		 (if (and (pair? arg)
			  (eq? (car arg) 'quote)
			  (symbol? (cadr arg))
			  (not (memq (cadr arg) 
				     '(print-length safety cpu-time heap-size free-heap-size gc-freed max-string-length max-list-length 
				       max-vector-length max-vector-dimensions default-hash-table-length initial-string-port-length 
				       gc-protected-objects file-names rootlet-size c-types stack-top stack-size stacktrace-defaults
				       max-stack-size catches exits float-format-precision bignum-precision default-rationalize-error 
				       default-random-state morally-equal-float-epsilon hash-table-float-epsilon undefined-identifier-warnings 
				       gc-stats symbol-table-locked?))))
		     (lint-format "unknown *s7* field: ~A" name (cadr form))))))

	  )) ; end check-special-cases

      
      (define (check-args name head form checkers env max-arity)
	;; check for obvious argument type problems
	;; name = overall caller, head = current caller, checkers = proc or list of procs for checking args

	(define (prettify-arg-number argn)
	  (if (or (not (= argn 1))
		  (pair? (cddr form)))
	      (format #f "~D " argn)
	      ""))

	(define (check-checker checker at-end)
	  (if (eq? checker 'integer:real?)
	      (if at-end 'real? 'integer?)
	      (if (eq? checker 'integer:any?)
		  (or at-end 'integer?)
		  checker)))

	(define (prettify-checker-unq op)
	  (if (pair? op)
	      (string-append (prettify-checker-unq (car op)) " or " (prettify-checker-unq (cadr op)))
	      (case op
		((rational?) "rational")
		((real?) "real")
		((complex?) "complex")
		(else 
		 (let ((op-name (symbol->string op)))
		   (string-append (if (memv (op-name 0) '(#\a #\e #\i #\o #\u)) "an " "a ")
				  (substring op-name 0 (- (length op-name) 1))))))))

	(define (prettify-checker op)
	  (if (pair? op)
	      (string-append (prettify-checker-unq (car op)) " or " (prettify-checker (cadr op)))
	      (let ((op-name (symbol->string op)))
		(case op
		  ((rational? real? complex?) op-name)
		  (else (string-append (if (memv (op-name 0) '(#\a #\e #\i #\o #\u)) "an " "a ") op-name))))))

	(define (report-arg-trouble name form head arg-number checker arg uop env)
	  (let ((op (if (and (eq? checker 'real?)
			     (eq? uop 'number?))
			'complex?
			uop)))
	    (lint-format "in ~A, ~A's argument ~Ashould be ~A, but ~S is ~A"
			 name (truncated-list->string form) head 
			 (prettify-arg-number arg-number)
			 (prettify-checker-unq checker)
			 arg
			 (prettify-checker op))))

	(let ((arg-number 1))
	  (call-with-exit
	   (lambda (done)
	     (for-each 
	      (lambda (arg)
		(let ((checker (check-checker (if (list? checkers) (car checkers) checkers) (= arg-number (length (cdr form))))))
		  ;(format *stderr* "check-arg ~A check ~S via ~S~%" head arg checker)
		  (when (or (pair? checker)
			    (symbol? checker)) ; otherwise ignore type check on this argument (#t -> anything goes)
		    (if (pair? arg)                  ; arg is expr -- try to guess its type
			(if (eq? (car arg) 'quote)   ; '1 -> 1
			    (let ((op (if (pair? (cadr arg)) 'list? (->type (cadr arg)))))
			      ;; arg is quoted expression
			      (if (and (not (memq op '(#f #t values)))
				       (not (any-compatible? checker op)))
				  (report-arg-trouble name form head arg-number checker arg op env)))

			    ;; arg is an evaluated expression
			    (case (car arg)

			      ((begin let let* letrec letrec* with-let)
			       (let ((last-expr (and (pair? (cdr arg))
						     (list-ref arg (- (length arg) 1)))))
				 (unless (symbol? last-expr)
				   (let ((op (->type last-expr)))
				     (if (and (not (memq op '(#f #t values)))
					      (not (any-compatible? checker op)))
					 (report-arg-trouble name form head arg-number checker last-expr op env))))))

			      ((if)
			       (if (> (length arg) 2)
				   (let ((t (caddr arg))
					 (f (if (pair? (cdddr arg)) (cadddr arg) #<unspecified>)))
				     (unless (symbol? t)
				       (let ((op (->type t)))
					 (if (and (not (memq op '(#f #t values)))
						  (not (any-compatible? checker op)))
					     (report-arg-trouble name form head arg-number checker t op env))))
				     (when (and f (not (symbol? f)))
				       (let ((op (->type f)))
					 (if (and (not (memq op '(#f #t values)))
						  (not (any-compatible? checker op)))
					     (report-arg-trouble name form head arg-number checker f op env)))))))
					 
			      ((dynamic-wind)
			       (if (= (length arg) 4)
				   (let ((f (caddr arg)))
				     (if (and (pair? f)
					      (eq? (car f) 'lambda))
					 (let ((len (length f)))
					   (if (> len 2)
					       (let ((res (list-ref f (- len 1))))
						 (unless (symbol? res)
						   (let ((op (->type res)))
						     (if (and (not (memq op '(#f #t values)))
							      (not (any-compatible? checker op)))
							 (report-arg-trouble name form head arg-number checker res op env)))))))))))
			      
			      ((do)
			       (if (> (length arg) 2)
				   (let ((end+res (caddr arg)))
				     (let ((res (if (pair? (cdr end+res))
						    (list-ref (cdr end+res) (- (length end+res) 2))
						    ())))
				       (unless (symbol? res)
					 (let ((op (->type res)))
					   (if (and (not (memq op '(#f #t values)))
						    (not (any-compatible? checker op)))
					       (report-arg-trouble name form head arg-number checker res op env))))))))

			      ((case)
			       (for-each
				(lambda (clause)
				  (if (and (pair? clause)
					   (pair? (cdr clause)))
				      (let ((expr (list-ref clause (- (length clause) 1))))
					(unless (symbol? expr)
					  (let ((op (->type expr)))
					    (if (and (not (memq op '(#f #t values)))
						     (not (any-compatible? checker op)))
						(report-arg-trouble name form head arg-number checker expr op env)))))))
				(cddr arg)))

			      ((cond)
			       (for-each
				(lambda (clause)
				  (let ((expr (if (pair? clause)
						  (if (pair? (cdr clause))
						      (list-ref clause (- (length clause) 1))
						      (car clause))
						  clause)))
				    (unless (symbol? expr)
				      (let ((op (->type expr)))
					(if (and (not (memq op '(#f #t values)))
						 (not (any-compatible? checker op)))
					    (report-arg-trouble name form head arg-number checker expr op env))))))
				(cdr arg)))

			      ;; TODO: also if no else, check ()|#<unspecified> and report as default possibility
			      ;; TODO: values, call-with-exit, call/cc

			      (else 
			       (let ((op (return-type (car arg) env)))
				 ;; checker is arg-type, op is expression type (can also be a pair)
				 (if (and (not (memq op '(#f #t values)))
					  (or (not (any-compatible? checker op))
					      (and (just-constants? arg env) ; try to eval the arg
						   (catch #t 
						     (lambda ()
						       (not (any-checker? checker (eval arg))))
						     (lambda ignore-catch-error-args
						       #f)))))
				     (report-arg-trouble name form head arg-number checker arg op env))))))

			;; arg is not a pair
			(if (and (not (symbol? arg))
				 (not (any-checker? checker arg)))
			    (let ((op (->type arg)))
			      (unless (memq op '(#f #t values))
				(report-arg-trouble name form head arg-number checker arg op env))))))

		  (if (list? checkers)
		      (if (null? (cdr checkers))
			  (done)
			  (set! checkers (cdr checkers))))
		  (set! arg-number (+ arg-number 1))
		  (if (> arg-number max-arity) (done))))
	      (cdr form))))))
      
      (define (check-call name head form env)
	(let ((data (or (var-member head env) (hash-table-ref globals head))))
	  ;(format *stderr* "~A call: ~A~%" form fdata)
	  (if (var? data)
	      (let ((fdata (cdr data)))
		;; a local var
		(if (symbol? (fdata 'ftype))
		    (let ((args (fdata 'arglist))
			  (ary (and (not (eq? (fdata 'decl) 'error))
				    (arity (fdata 'decl)))))
		      (if (pair? ary)
			  (let ((req (car ary))
				(opt (cdr ary))
				(pargs (if (pair? args) 
					   (proper-list args)
					   (if (symbol? args)
					       (list args)
					       ()))))
			    (let ((call-args (length (cdr form))))
			      (if (< call-args req)
				  (lint-format "~A needs ~D argument~A: ~A" 
					       name head 
					       req (if (> req 1) "s" "") 
					       (truncated-list->string form))
				  (if (> (- call-args (keywords (cdr form))) opt)
				      (lint-format "~A has too many arguments: ~A" name head (truncated-list->string form)))))
			    (unless (fdata 'allow-other-keys)
			      (let ((last-was-key #f)
				    (have-keys 0)
				    (warned #f)
				    (rest (if (and (pair? form) (pair? (cdr form))) (cddr form) ())))
				(for-each
				 (lambda (arg)
				   (if (keyword? arg)
				       (begin
					 (set! have-keys (+ have-keys 1))
					 (if (not (member (keyword->symbol arg) pargs 
							  (lambda (a b)
							    (if (pair? b) 
								(eq? a (car b))
								(eq? a b)))))
					     (lint-format "~A keyword argument ~A (in ~S) does not match any argument in ~S" name head arg form pargs))
					 (if (memq arg rest)
					     (lint-format "~W is repeated in ~A" name arg (cdr form)))
					 (set! last-was-key #t))
				       (begin
					 (when (and (positive? have-keys)
						    (not last-was-key)
						    (not warned))
					   (set! warned #t)
					   (lint-format "non-keyword argument ~A follows previous keyword~P" name arg have-keys))
					 (set! last-was-key #f)))
				   (if (pair? rest)
				       (set! rest (cdr rest))))
				 (cdr form))))
			    
			    ;; look for problematic macro expansion
			    (when (memq (fdata 'ftype) '(define-macro define-macro* defmacro defmacro*))

			      (unless (list? (fdata 'macro-ops))
				(let ((syms (list () ())))
				  (if (memq (fdata 'ftype) '(define-macro define-macro*))
				      (tree-symbol-walk (cddr (fdata 'value)) syms)
				      (tree-symbol-walk (cdddr (fdata 'value)) syms))
				  (varlet fdata 'macro-locals (car syms) 'macro-ops (cadr syms))))

			      (when (or (pair? (fdata 'macro-locals))
					(pair? (fdata 'macro-ops)))
				(let ((bad-locals ())
				      (bad-quoted-locals ())
				      (bad-ops ()))
				  (for-each
				   (lambda (local)
				     (if (tree-unquoted-member local (cdr form))
					 (set! bad-locals (cons local bad-locals))))
				   (fdata 'macro-locals))
				  (when (null? bad-locals)
				    (for-each
				     (lambda (local)
				       (if (tree-member local (cdr form))
					   (set! bad-quoted-locals (cons local bad-quoted-locals))))
				     (fdata 'macro-locals)))
				  (for-each
				   (lambda (op)
				     (let ((curf (var-member op env))
					   (oldf (var-member op (fdata 'env))))
				       (if (and (not (eq? curf oldf))
						(or (pair? (fdata 'env))
						    (hash-table-ref globals op)
						    (defined? op (rootlet))))
					   (set! bad-ops (cons op bad-ops)))))
				   (fdata 'macro-ops))

				  (when (or (pair? bad-locals)
					    (pair? bad-quoted-locals) 
					    ;; (define-macro (mac8 b) `(let ((a 12)) (+ (symbol->value ,b) a)))
					    ;; (let ((a 1)) (mac8 'a))
					    ;; far-fetched!
					    (pair? bad-ops))
				    (lint-format "possible problematic macro expansion:~%  ~A ~A collide with subsequently defined ~A~A~A" 
						 name 
						 form
						 (if (or (pair? bad-locals)
							 (pair? bad-ops))
						     "may"
						     "could conceivably")
						 (if (pair? bad-locals)
						     (format #f "~{'~A~^, ~}" bad-locals)
						     (if (pair? bad-quoted-locals)
							 (format #f "~{'~A~^, ~}" bad-quoted-locals)
							 ""))
						 (if (and (pair? bad-locals) (pair? bad-ops)) ", " "")
						 (if (pair? bad-ops)
						     (format #f "~{~A~^, ~}" bad-ops)
						     ""))))))
			    )))))
	      ;; not local var
	      (when (symbol? head)
		(let ((head-value (symbol->value head *e*))) ; head might be "arity"!
		  (when (or (procedure? head-value)
			    (macro? head-value))
		    ;; check arg number
		    (let* ((args (length (cdr form)))
			   (ary (arity head-value))
			   (min-arity (car ary))
			   (max-arity (cdr ary)))
		      (if (< args min-arity)
			  (lint-format "~A needs ~A~D argument~A: ~A" 
				       name head 
				       (if (= min-arity max-arity) "" "at least ")
				       min-arity
				       (if (> min-arity 1) "s" "") 
				       (truncated-list->string form))
			  (if (and (not (procedure-setter head-value))
				   (> (- args (keywords (cdr form))) max-arity))
			      (lint-format "~A has too many arguments: ~A" name head (truncated-list->string form))))
		      (if (and (procedure? head-value)
			       (pair? (cdr form))) ; there are args (the not-enough-args case is checked above)
			  (if (zero? max-arity)
			      (lint-format "too many arguments: ~A" name (truncated-list->string form))    
			      (begin
				
				(for-each (lambda (arg)
					    (if (pair? arg)
						(if (negative? (length arg))
						    (lint-format "missing quote? ~A in ~A" name arg form)
						    (if (eq? (car arg) 'unquote)
							(lint-format "stray comma? ~A in ~A" name arg form)))))
					  (cdr form))
				
				;; if keywords, check that they are acceptable
				;;    this only applies to lambda*'s that have been previously loaded (lint doesn't create them)
				(let ((source (procedure-source head-value)))
				  (if (and (pair? source)
					   (eq? (car source) 'lambda*))
				      (let ((decls (cadr source)))
					(if (not (memq :allow-other-keys decls))
					    (for-each
					     (lambda (arg)
					       (if (and (keyword? arg)
							(not (eq? arg :rest))
							(not (member arg decls 
								     (lambda (a b) 
								       (if (pair? b) 
									   (eq? (keyword->symbol a) (car b))
									   (eq? (keyword->symbol a) b))))))
						   (lint-format "~A keyword argument ~A (in ~S) does not match any argument in ~S" name head arg form decls)))
					     (cdr form))))))
				
				;; we've already checked for head in the current env above
				(if (and (or (memq head '(eq? eqv?))
					     (and (= (length form) 3)
						  (hash-table-ref repeated-args-table head)))
					 (repeated-member? (cdr form) env))
				    (lint-format "this looks odd: ~A"
						 name
						 ;; sigh (= a a) could be used to check for non-finite numbers, I suppose,
						 ;;   and (/ 0 0) might be deliberate (as in gmp)
						 ;;   also (min (random x) (random x)) is not pointless
						 (truncated-list->string form))
				    (if (and (hash-table-ref repeated-args-table-2 head)
					     (repeated-member? (cdr form) env))
					(lint-format "it looks odd to have repeated arguments in~A" name (truncated-list->string form))))

				(when (memq head '(eq? eqv?))
				  (define (repeated-member-with-not? lst env)
				    (and (pair? lst)
					 (or (and (or (not (pair? (car lst)))
						      (not (side-effect? (car lst) env)))
						  (or (member (list 'not (car lst)) (cdr lst))
						      (and (pair? (car lst))
							   (eq? (caar lst) 'not)
							   (= (length (car lst)) 2)
							   (member (cadar lst) (cdr lst)))))
					     (repeated-member-with-not? (cdr lst) env))))
				  (if (repeated-member-with-not? (cdr form) env)
				      (lint-format "this looks odd: ~A" name (truncated-list->string form))))
      
      				;; now try to check arg types 
				(let ((func (symbol->value head *e*)))
				  (let ((arg-data (let ((sig (procedure-signature func)))
						    (and (pair? sig)
							 (cdr sig)))))
				    ;; (format *stderr* "arg-data: ~A~%" arg-data)
				    (if (and arg-data
					     (or (not (pair? arg-data))
						 (not (eq? (car arg-data) #t))
						 (not (infinite? (length arg-data)))))
					(check-args name head form arg-data env max-arity))))))))))))))
  
      (define (get-generator form name head) ; defgenerator funcs
	(let ((name (if (pair? (cadr form))
			(caadr form)
			(cadr form))))
	  ;; auto-define make-name, name?
	  (let ((make-name (string->symbol (string-append "make-" (symbol->string name))))
		(name? (string->symbol (string-append (symbol->string name) "?"))))
	    
	    (hash-table-set! globals make-name (make-var :name make-name))
	    (hash-table-set! globals name? (make-var :name name?)))))
      
      (define (last-par x)
	(let ((len (length x)))
	  (and (positive? len)
	       (x (- len 1)))))
      
      (define (load-walk form)
	;; check form for top-level declarations, if load seen, and we haven't seen that file, load it
	(let ((head (car form)))
	  (case head
	    ((begin)
	     (load-walk (cdr form)))
	    
	    ((define-constant define-envelope)
	     (hash-table-set! globals (cadr form) (make-var :name (cadr form) :value (and (pair? (cddr form)) (caddr form)))))
	    
	    ((defmacro defmacro*)
	     (hash-table-set! globals (cadr form) (make-fvar :name (cadr form) 
							     :ftype head
							     :decl (eval (list head '_ (caddr form) #f))
							     :arglist (caddr form)
							     :value form
							     :env ()
							     :location #__line__)))
	    ((define)
	     (let ((name (cadr form)))
	       (if (pair? name)
		   (let ((fname (car name)))
		     (if (symbol? fname)
			 (if (keyword? fname)
			     (lint-format "keywords are constants ~A" name form)
			     (hash-table-set! globals fname 
					      (make-fvar :name fname 
							 :ftype 'define
							 :decl (eval (list 'define (cons '_ (cdadr form)) #f))
							 :arglist (cdr name)
							 :value form
							 :env ()
							 :location #__line__)))
			 (lint-format "what is this? ~A" name form)))
		   (hash-table-set! globals name (make-var :name name 
							   :value (and (pair? (cddr form)) (caddr form)))))))

	    ((define*)
	     (hash-table-set! globals (caadr form) (make-fvar :name (caadr form) 
							      :ftype head
							      :decl (eval (list head (cons '_ (cdadr form)) #f))
							      :arglist (cdadr form)
							      :value form
							      :env ()
							      :location #__line__)))

	    ((define-expansion define-macro define-macro* define-bacro define-bacro* definstrument defanimal)
	     (hash-table-set! globals (caadr form) (make-fvar :name (caadr form) 
							      :ftype head
							      :decl (eval (list head (cons '_ (cdadr form)) #f))
							      :arglist (cdadr form)
							      :value form
							      :env ()
							      :location #__line__)))

	    ((defgenerator)
	     (get-generator form 'defgenerator head))
	    
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

	(define (find-file file paths)
	  (and (pair? paths)
	       (catch #t
		 (lambda ()
		   (open-input-file (string-append (car paths) "/" file)))
		 (lambda args
		   (find-file file (cdr paths))))))

	(let ((file (cadr form)))
	  (if (and (string? file)
		   (not (member file loaded-files)))
	      (let ((fp (catch #t
			  (lambda ()
			    (open-input-file file))
			  (lambda args
			    (or (find-file file *load-path*)
				(and (format outport " can't load ~S~%" file) #f))))))
		(if (input-port? fp)
		    (begin
		      (set! loaded-files (cons file loaded-files))
					;(format outport " (scanning ~S)~%" file)
		      (do ((form (read fp) (read fp)))
			  ((eof-object? form))
			(if (and (pair? form)
				 (pair? (cdr form)))
			    (load-walk form)))
		      (close-input-port fp)))))))
      
      
      (define (binding-ok? name head binding env second-pass)
	;; check let-style variable binding for various syntactic problems
	(if second-pass
	    (and (pair? binding)
		 (symbol? (car binding))
		 ;(not (keyword? (car binding)))
		 (not (constant? (car binding)))
		 (pair? (cdr binding))
		 (or (null? (cddr binding))
		     (and (eq? head 'do)
			  (pair? (cddr binding)) ; (do ((i 0 . 1))...)
			  (null? (cdddr binding)))))

	    (cond ((not (pair? binding)) 	   (lint-format "~A binding is not a list? ~S" name head binding) #f)
		  ((not (symbol? (car binding)))   (lint-format "~A variable is not a symbol? ~S" name head binding) #f)
		  ((keyword? (car binding))	   (lint-format "~A variable is a keyword? ~S" name head binding) #f)
		  ((constant? (car binding))	   (lint-format "can't bind a constant: ~S" name binding) #f)
		  ((not (pair? (cdr binding)))
		   (if (null? (cdr binding))
		       (lint-format "~A variable value is missing? ~S" name head binding)
		       (lint-format "~A binding is an improper list? ~S" name head binding))
		   #f)
		  ((or (not (pair? (cdr binding)))
		       (and (pair? (cddr binding))
			    (or (not (eq? head 'do))
				(pair? (cdddr binding)))))
		   (lint-format "~A binding is messed up: ~A" name head binding)
		   #f)
		  (else 
		   (if (and *report-shadowed-variables*
			    (or (hash-table-ref globals (car binding))
				(var-member (car binding) env)))
		       (lint-format "~A variable ~A in ~S shadows an earlier declaration" name head (car binding) binding))
		   #t))))
      
      
      (define (env-difference name e1 e2 lst)
	(if (or (null? e1)
		(null? e2)
		(eq? (car e1) (car e2)))
	    lst
	    (env-difference name (cdr e1) e2 
			    (if (eq? name (var-name (car e1)))
				lst
				(cons (car e1) lst)))))
      
      
      (define (report-usage name type head vars)
	;; report unused or set-but-unreferenced variables
	(if (and (not (eq? head 'begin)) ; begin can redefine = set a variable
		 (pair? vars)
		 (proper-list? vars))
	    (do ((cur vars (cdr cur))
		 (rst (cdr vars) (cdr rst)))
		((null? rst))
	      (let ((repeat (var-member (var-name (car cur)) rst)))
		;; not globals here because the same name might be used as a global
		(if repeat
		    (lint-format "~A ~A ~A is declared twice" name head type (var-name (car cur)))))))
	
	(let ((set ())
	      (unused ()))
	  ;(format *stderr* "report-usage vars: ~S~%" vars)
	  (for-each 
	   (lambda (arg)
	     (if (hash-table-ref syntaces (var-name arg))
		 (lint-format "~A ~A named ~A is asking for trouble" name head type (var-name arg))
		 (if (not (symbol? (var-name arg)))
		     (lint-format "bad ~A ~A name: ~S in ~S" name head type (var-name arg) arg)))
	     (if (and (not (var-ref arg))
		      (not (hash-table-ref other-identifiers (var-name arg))))
		 (if (var-set arg)
		     (set! set (cons (var-name arg) set))
		     (if (not (memq (var-name arg) '(documentation signature iterator?)))
			 (set! unused (cons (var-name arg) unused))))))
	   vars)
	  
	  (if (pair? set)
	      (lint-format "~A ~A~A ~{~A~^, ~} set, but not used" 
			   name head type (if (> (length set) 1) "s" "") (reverse set)))
	  (if (pair? unused)
	      (lint-format "~A ~A~A ~{~A~^, ~} not used" 
			   name head type (if (> (length unused) 1) "s" "") (reverse unused)))))
      
      
      (define (lint-walk-body name head body env)
	;; walk a body (a list of forms, the value of the last of which might be returned)

	(if (not (proper-list? body))
	    (lint-format "stray dot? ~A" name (truncated-list->string body))
	    
	    (let ((prev-f #f)
		  (prev-fs #f)
		  (prev-len 0)
		  (f-len 0)
		  (block-fs #f)
		  (dpy-f #f)
		  (dpy-start #f)
		  (len (length body)))
	      (if (eq? head 'do) (set! len (+ len 1))) ; last form in do body is not returned

	      (do ((fs body (cdr fs))
		   (ctr 0 (+ ctr 1)))
		  ((not (pair? fs)))
		(let ((f (car fs)))

		  (if (pair? f)
		      (begin
			(set! f-len (length f))
			(if (eq? (car f) 'begin)
			    (lint-format "redundant begin: ~A" name (truncated-list->string f))))
		      (set! f-len 0))

		  (if (and (= f-len prev-len 3)
			   (eq? (car f) 'set!)
			   (eq? (car prev-f) 'set!)
			   (eq? (cadr f) (cadr prev-f)))
		      (let ((arg1 (caddr prev-f))
			    (arg2 (caddr f)))
			(if (or (not (pair? arg2))
				(not (tree-member (cadr f) arg2)))
			    (if (and (not (side-effect? arg1 env))
				     (not (side-effect? arg2 env)))
				(lint-format "this could be omitted: ~A" name prev-f))
			    (if (and (pair? arg1)
				     (pair? arg2)
				     (eq? (car arg1) 'cons)
				     (eq? (car arg2) 'cons)
				     (eq? (cadr f) (caddr arg2))
				     (not (eq? (cadr f) (cadr arg2))))
				(lint-format "perhaps ~A ~A -> ~A" name
					     prev-f f
					     `(set! ,(cadr f) (cons ,(cadr arg2) (cons ,@(cdr arg1)))))))))

		  (let ((repeated-if (and (= f-len prev-len 3)
					  (eq? (car f) 'if)
					  (eq? (car prev-f) 'if)
					  (equal? (cadr f) (cadr prev-f))))
			(combine #f))
		    (if (not repeated-if)
			(if block-fs
			    (if (not (eq? (cdr block-fs) prev-fs))
				(set! combine prev-f)
				(set! block-fs #f)))
			(if block-fs
			    (set! combine (and (null? (cdr fs)) f))
			    (set! block-fs prev-fs)))
		    
		    (when combine
		      (if (not (side-effect? (caadr block-fs) env))
			  (lint-format "perhaps combine repeated if's: ~A ... ~A -> (when ~A ~A ... ~A)" name
				       (car block-fs) combine
				       (cadr combine) (caddar block-fs) (caddr combine)))
		      (set! block-fs #f)))

		  (if (< ctr (- len 1)) 
		      ;; f is not the last form, so its value is ignored
		      (begin
			(if (and (pair? f)
				 (eq? (car f) 'map))
			    (lint-format "map could be for-each: ~A" name (truncated-list->string f)))
			(if (not (side-effect? f env))
			    (lint-format "this could be omitted: ~A" name (truncated-list->string f))))
		      
		      ;; here f is the last form in the body
		      (when (and (pair? prev-f)
				 (pair? (cdr prev-f))
				 (pair? (cddr prev-f)))                ; (set! ((L 1) 2)) an error, but lint should keep going
			(if (and (eq? (car prev-f) 'set!)
				 (or (and (equal? (caddr prev-f) f)    ; (begin ... (set! x (...)) (...))
					  (not (side-effect? f env)))
				     (and (symbol? f)                  ; (begin ... (set! x ...) x)
					  (eq? f (cadr prev-f)))))
			    (lint-format "this could be omitted: ~A" name (truncated-list->string f)))
			(if (and (pair? f)
				 (pair? (cdr f))
				 (pair? (cddr f))
				 (eq? (cadr prev-f) (cadr f))
				 (or (and (eq? (car prev-f) 'vector-set!)
					  (eq? (car f) 'vector-ref))
				     (and (eq? (car prev-f) 'list-set!)
					  (eq? (car f) 'list-ref)))
				 (equal? (caddr f) (caddr prev-f))
				 (pair? (cdddr prev-f))
				 (not (pair? (cddddr prev-f)))
				 (not (pair? (cdddr f)))
				 (not (side-effect? (caddr f) env)))
			    (lint-format "this could be omitted: ~A" name (truncated-list->string f)))))
		  
		  ;; needs f fs prev-f dpy-f dpy-start ctr len
		  ;;   trap lint-format
		  (let ((dpy-case (and (pair? f)
				       (memq (car f) '(display write newline write-char write-string))))) ; flush-output-port?
		    (define (out-port expr) ; ()=not specified (*stdout*), #f=something is wrong (not enough args)
		      (if (eq? (car expr) 'newline)
			  (if (pair? (cdr expr))
			      (cadr expr)
			      ())
			  (and (pair? (cdr expr))
			       (if (pair? (cddr expr))
				   (caddr expr)
				   ()))))
		    (when (and dpy-case
			       (not dpy-start))
		      (set! dpy-f fs)
		      (set! dpy-start ctr))
		    ;(format *stderr* "~A ~A ~A ~A~%" f ctr dpy-start len)
		    (when (and dpy-start
			       (> (- ctr dpy-start) (if dpy-case 1 2))
			       (or (= ctr (- len 1))
				   (not dpy-case)))
		      ;; display sequence starts at dpy-start, goes to ctr (prev-f) unless not dpy-case
		      (let ((ctrl-string "")
			    (args ())
			    (dctr 0)
			    (dpy-last (if (not dpy-case) prev-f f))
			    (op (out-port (car dpy-f)))
			    (exprs (make-list (if dpy-case (- ctr dpy-start -1) (- ctr dpy-start)) ())))
			;(format *stderr* "~A: ~A ~A ~A ~A~%" body dpy-case dpy-start ctr dpy-last)
			(call-with-exit
			 (lambda (done)
			   (for-each
			    (lambda (d)
			      (if (not (equal? (out-port d) op)) 
				  (begin 
				    (lint-format "unexpected port change: ~A -> ~A in ~A~%" name op (out-port d) d) ; ??
				    (done)))
			      (list-set! exprs dctr d)
			      (set! dctr (+ dctr 1))
			      (case (car d)
				((display) 

				 ;; TODO: string-append as arg: (display (string-append...) p)
				 ;;   (display (number->string... [possible-radix])
				 ;;   (display (make-string ...)
				 ;;   (display (apply format ...)
				 ;; same happen with write
				 ;; (write-char #\a p)

				 (if (string? (cadr d))
				     (set! ctrl-string (string-append ctrl-string (cadr d)))
				     (begin
				       (set! ctrl-string (string-append ctrl-string "~A"))
				       (set! args (cons (cadr d) args)))))
				((write)
				 (if (string? (cadr d))
				     (set! ctrl-string (string-append ctrl-string "\"" (cadr d) "\""))
				     (begin
				       (set! ctrl-string (string-append ctrl-string "~S"))
				       (set! args (cons (cadr d) args)))))
				((write-char)
				 (if (char? (cadr d))
				     (set! ctrl-string (string-append ctrl-string (string (cadr d))))
				     (begin
				       (set! ctrl-string (string-append ctrl-string "~C"))
				       (set! args (cons (cadr d) args)))))
				((write-string)  ; same as display but with possible start|end indices
				 (let ((indices (and (pair? (cddr d)) ; port
						     (pair? (cdddr d))
						     (cdddr d))))
				   (if (string? (cadr d))
				       (if indices
					   (if (and (integer? (car indices))
						    (or (null? (cdr indices))
							(and (pair? indices)
							     (integer? (cadr indices)))))
					       (set! ctrl-string (string-append ctrl-string (apply substring (cadr d) indices)))
					       (begin
						 (set! ctrl-string (string-append ctrl-string "~A"))
						 (set! args (cons `(substring ,(cadr d) ,@indices) args))))
					   (set! ctrl-string (string-append ctrl-string (cadr d))))
				       (begin
					 (set! ctrl-string (string-append ctrl-string "~A"))
					 (if indices
					     (set! args (cons `(substring ,(cadr d) ,@indices) args))
					     (set! args (cons (cadr d) args)))))))
				((newline)
				 (set! ctrl-string (string-append ctrl-string "~%"))))
				
			      (when (eq? d dpy-last) ; op can be null => send to (current-output-port), return #f or #<unspecified>
				(lint-format "perhaps ~A" name (lists->string exprs `(format ,op ,ctrl-string ,@(reverse args))))
				(done)))
			    dpy-f))))
		      (set! dpy-start #f))
		    (unless dpy-case (set! dpy-start #f)))
		  
		  (if (and (pair? f)
			   (memq head '(defmacro defmacro* define-macro define-macro* define-bacro define-bacro*))
			   (tree-member 'unquote f))
		      (lint-format "~A probably has too many unquotes: ~A" name head (truncated-list->string f)))
		  
		  (set! prev-f f)
		  (set! prev-fs fs)
		  (set! prev-len f-len)
		  (set! env (lint-walk name f env))))))
	env)
      
      
      (define (lint-walk-function-body name head args arg-data body env)
	;; walk function body, with possible doc string at the start
	(when (and (pair? body)
		   (pair? (cdr body))
		   (string? (car body)))
	  (if *report-doc-strings*
	      (lint-format "old-style doc string: ~S~%" name (car body)))
	  (set! body (cdr body))) ; ignore old-style doc-string
	(lint-walk-body name head body env)
	env)
      
      
      (define (lint-walk-function head name args val form env)
	;(format *stderr* "function: ~A ~A ~A ~A~%" head name args val)

	;; check out function arguments (adding them to the current env), then walk its body, (name == function name, val == body)
	;; first check for (define (hi...) (ho...)) where ho has no opt args (and try to ignore possible string constant doc string)

	(if (eq? head 'define)
	    (let ((bval (if (and (pair? val)
				 (string? (car val)))
			    (cdr val)         ; strip away the (old-style) documentation string
			    val)))
	      (if (and (pair? bval)           ; not (define (hi a) . 1)!
		       (pair? (car bval))
		       (null? (cdr bval))
		       (symbol? (caar bval))) ; not (define (hi) ((if #f + abs) 0))
		  (if (equal? args (cdar bval))
		      (let* ((cval (caar bval))
			     (p (symbol->value cval *e*))
			     (ary (arity p)))
			(if (or (and (procedure? p)
				     (or (= (car ary) (cdr ary))
					 (= (length args) (cdr ary))))
				(let ((e (or (var-member cval env) 
					     (hash-table-ref globals cval))))
				  (and e
				       (var? e)
				       (symbol? (var-ftype e))
				       (let ((def (var-value e)) 
					     (e-args (var-arglist e)))
					 (and 
					  (pair? def)
					  (memq (var-ftype e) '(define lambda))
					  (or (and (null? args)
						   (null? e-args))
					      (and (symbol? args)
						   (symbol? e-args))
					      (and (pair? args)
						   (pair? e-args)
						   (= (length args) (length e-args)))))))))
			    (lint-format "~A could be (define ~A ~A)" name name name cval)))
		      (if (and (eq? (caar bval) 'list-ref)
			       (pair? (cdar bval))
			       (pair? (cddar bval))
			       (eq? (car args) (cadar bval))
			       (null? (cdr args)))
			  (case (caddar bval)
			    ((0) (lint-format "~A could be (define ~A car)" name name name))
			    ((1) (lint-format "~A could be (define ~A cadr)" name name name))
			    ((2) (lint-format "~A could be (define ~A caddr)" name name name))
			    ((3) (lint-format "~A could be (define ~A cadddr)" name name name))))))))
	    
	(let ((data (and (symbol? name)
			 (make-fvar :name (if (not (memq head '(lambda lambda*))) name '[anonymous])
				    :ftype head
				    :value form
				    :env env
				    :arglist (if (memq head '(lambda lambda*))
						 (cadr form)
						 (if (memq head '(defmacro defmacro*))
						     (caddr form)
						     (cdadr form)))
				    :location #__line__))))
	  (when data
	    (let ((ldata (cdr data)))
	    (set! (ldata 'decl)
		  (catch #t
		    (lambda ()
		      (case head
			((lambda)
			 (set! (ldata 'allow-other-keys) #t)
			 (eval (list head (cadr form) #f)))

			((lambda*)
			 (set! (ldata 'allow-other-keys) (eq? (last-par (cadr form)) :allow-other-keys))
			 (eval (list head (copy (cadr form)) #f)))         ; eval can remove :allow-other-keys!

			((define*)
			 (set! (ldata 'allow-other-keys) (eq? (last-par (cdadr form)) :allow-other-keys))
			 (eval (list head (cons '_ (copy (cdadr form))) #f)))

			((defmacro defmacro*)
			 (set! (ldata 'allow-other-keys) (or (not (eq? head 'defmacro*))
							     (eq? (last-par (caddr form)) :allow-other-keys)))
			 (eval (list head '_ (caddr form) #f)))

			((define-constant)
			 (set! (ldata 'allow-other-keys) #t)
			 (eval (list 'define (cons '_ (cdadr form)) #f)))

			(else
			 (set! (ldata 'allow-other-keys) (or (not (memq head '(define-macro* define-bacro*)))
							     (eq? (last-par (cdadr form)) :allow-other-keys)))
			 (eval (list head (cons '_ (cdadr form)) #f)))))
		    (lambda args
		      ;(format *stderr* "~A~%" args)
		      'error)))))
				    
	  (if (null? args)
	      (begin
		(if (memq head '(define* lambda* defmacro* define-macro* define-bacro*))
		    (lint-format "~A could be ~A" 
				 name head
				 (symbol (substring (symbol->string head) 0 (- (length (symbol->string head)) 1)))))
		(lint-walk-function-body name head args () val env)
		(if data
		    (append (list data) env)
		    env))
	    
	      (if (or (symbol? args) 
		      (pair? args))
		  (let ((arg-data (if (symbol? args)                            ; this is getting arg names to add to the environment
				      (list (make-var :name args))
				      (map
				       (lambda (arg)
					 (if (symbol? arg)
					     (if (memq arg '(:rest :allow-other-keys))
						 (values)                  ; omit :rest and :allow-other-keys
						 (make-var :name arg))
					     (if (or (not (pair? arg))
						     (not (= (length arg) 2))
						     (not (memq head '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
						 (begin
						   (lint-format "strange parameter for ~A: ~S" name head arg)
						   (values))
						 (begin
						   (if (not (cadr arg))
						       (lint-format "the default argument value is #f in ~A ~A" name head arg))
						   (make-var :name (car arg))))))
				       (proper-list args)))))
		  
		    (lint-walk-function-body name head args arg-data val (append arg-data (if data (append (list data) env) env)))
		    (if *report-unused-parameters* 
			(report-usage name 'parameter head arg-data))
		    (if data 
			(append (list data) env)
			env))
		
		  (begin
		    (lint-format "strange ~A parameter list ~A" name head args)
		    env)))))
      
      
      (define (lint-walk name form env)
	;; walk a form, here curlet can change
	;(format *stderr* "walk ~A, env: ~A~%~%" form env)

	(if (symbol? form)
	    (set-ref? form env) ; returns env
	    
	    (if (pair? form)
		(let ((head (car form)))
		  (set! line-number (pair-line-number form))
		  (case head
		    
		    ((define define* define-constant define-envelope define-expansion define-macro define-macro* define-bacro define-bacro* definstrument defanimal)
		     ;; ---------------- define ----------------		  
		     (if (< (length form) 2)
			 (begin
			   (lint-format "~S makes no sense" name form)
			   env)
			 (let ((sym (cadr form))
			       (val (cddr form)))
			   
			   (if (symbol? sym)
			       (begin
					;(set! env (cons (list (make-var :name sym :type (->type val))) env))
				 
				 (if (keyword? sym)
				     (lint-format "keywords are constants ~A" name sym))
				 
				 (if (memq head '(define define-constant define-envelope))
				     (let ((len (length form)))
				       (if (not (= len 3))
					   (lint-format "~S has ~A value~A?"
							name form 
							(if (< len 3) "no" "too many") 
							(if (< len 3) "" "s"))))
				     (lint-format "~A is messed up" name (truncated-list->string form)))
				 
				 (if (and (pair? val)
					  (null? (cdr val))
					  (equal? sym (car val)))
				     (lint-format "this ~A is either not needed, or is an error: ~A" name head (truncated-list->string form)))
				 
				 (if (pair? (cddr form))
				     (let ((e (lint-walk sym (caddr form) env)))
				       (if (and (pair? e)
						(eq? (var-name (car e)) '[anonymous])) ; (define x (lambda ...)) but it misses closures
					   (set! (var-name (car e)) sym)
					   (append (list (make-var :name sym)) env)))
				     (append (list (make-var :name sym)) env)))
			       
			       ;; not (symbol? sym)
			       (if (and (pair? sym)
					(pair? val)
					(not (pair? (car sym))))
				   (begin
				     (when (pair? (cdr sym))
				       (if (repeated-member? (proper-list (cdr sym)) env)
					   (lint-format "~A parameter is repeated: ~A" name head (truncated-list->string sym)))
				       (if (memq head '(define* define-macro* define-bacro*))
					   (check-star-parameters name (cdr sym))
					   (if (list-any? keyword? (cdr sym))
					       (lint-format "~A arglist can't handle keywords" name head))))

				     (when (and (eq? head 'define-macro)
						(null? (cdr sym))
						(null? (cdr val))
						(code-constant? (car val)))
				       (lint-format "perhaps ~A" name (lists->string form `(define ,(car sym) ,(car val)))))
				     
				     (if (and (eq? head 'definstrument)
					      (string? (car val)))
					 (set! val (cdr val)))
				     
				     (if (keyword? (car sym))
					 (begin
					   (lint-format "keywords are constants ~A" name (car sym))
					   env)
					 (lint-walk-function head (car sym) (cdr sym) val form env)))
				   
				   (begin
				     (lint-format "strange form: ~S" head form)
				     env))))))
		    
		    ((lambda lambda*)
		     ;; ---------------- lambda ----------------		  
		     ;; the lambda case includes stuff like call/cc?
		     (let ((len (length form)))
		       (if (< len 3)
			   (begin
			     (lint-format "~A is messed up in ~A" name head (truncated-list->string form))
			     env)
			   (let ((args (cadr form)))
			     
			     (if (list? args)
				 (let ((arglen (length args)))
				   (if (null? args)
				       (if (eq? head 'lambda*)             ; (lambda* ()...) -> (lambda () ...)
					   (lint-format "lambda* could be lambda: ~A" name form))
				       (begin ; args is a pair             ; (lambda (a a) ...)
					 (if (repeated-member? (proper-list args) env)
					     (lint-format "~A parameter is repeated: ~A" name head (truncated-list->string args)))
					 (if (eq? head 'lambda*)           ; (lambda* (a :b) ...)
					     (check-star-parameters name args)
					     (if (list-any? keyword? args) ; (lambda (:key) ...)
						 (lint-format "lambda arglist can't handle keywords (use lambda*)" name)))))
				   
				   (if (and (eq? head 'lambda)             ; (lambda () (f)) -> f, (lambda (a b) (f a b)) -> f
					    (= len 3)
					    (>= arglen 0)) ; not a dotted list
				       (let ((body (caddr form)))
					 (when (and (pair? body)
						    (symbol? (car body)))
					   (if (equal? args (cdr body))
					       (lint-format "perhaps ~A" name (lists->string form (car body)))
					       (if (equal? (reverse args) (cdr body))
						   (let ((rf (reversed (car body))))
						     (if rf (lint-format "perhaps ~A" name (lists->string form rf))))))))))
				 
				 (if (and (symbol? args)                   ; (lambda args (apply f args)) -> f
					  (eq? head 'lambda)
					  (= len 3))
				     (let ((body (caddr form)))
				       (if (and (pair? body)
						(= (length body) 3)
						(eq? (car body) 'apply)
						(symbol? (cadr body))
						(eq? args (caddr body)))
					   (lint-format "perhaps ~A" name (lists->string form (cadr body)))))))
			     
			     (lint-walk-function head name args (cddr form) form env)
			     env))))
		    
		    ((set!)
		     ;; ---------------- set! ----------------		  
		     (if (not (= (length form) 3))
			 (begin
			   (lint-format "set! has too ~A arguments: ~S" name (if (> (length form) 3) "many" "few") form)
			   env)
			 (let ((settee (cadr form))
			       (setval (caddr form)))
			   (let ((result (lint-walk name setval env)))
					;(format *stderr* "result: ~A, env: ~A~%" result env)
			     (if (symbol? settee)
				 (if (constant? settee)
				     (lint-format "can't set! ~A (it is a constant)" name (truncated-list->string form)))
				 (if (pair? settee)
				     (begin
				       (if (memq (car settee) '(vector-ref list-ref string-ref hash-table-ref))
					   (lint-format "~A as target of set!~A" name (car settee) (truncated-list->string form)))
				       (lint-walk name settee env) ; this counts as a reference since it's by reference so to speak
				       
				       ;; try type check (dilambda signatures)
				       (when (symbol? (car settee))
					 (let ((f (symbol->value (car settee) *e*)))
					   (when (dilambda? f)
					     (let ((sig (procedure-signature (procedure-setter f)))
						   (settee-len (length settee)))
					       (when (and (pair? sig)
							  (positive? settee-len)
							  (pair? (list-tail sig settee-len)))
						 (let ((checker (list-ref sig settee-len))
						       (arg-type (->type setval)))
						   (when (and (not (eq? 'symbol? arg-type))
							      (symbol? checker)
							      (not (compatible? checker arg-type)))
						     (lint-format "~A: new value should be a~A ~A: ~S: ~A" 
								  name (car settee)
								  (if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
								  checker arg-type
								  (truncated-list->string form)))))))))
				       
				       (set! settee (do ((sym (car settee) (car sym)))
							((not (pair? sym)) sym))))
				     (lint-format "can't set! ~A" name (truncated-list->string form))))
			     
			     (when (symbol? settee) ; see do above
			       (set-set? settee setval env))
			     (if (equal? (cadr form) (caddr form)) ; not settee and setval here!
				 (lint-format "pointless set!~A" name (truncated-list->string form)))
			     
			     result))))
		    
		    ((quote) 
		     ;; ---------------- quote ----------------		  
		     (let ((len (length form)))
		       (if (negative? len)
			   (lint-format "stray dot in quote's arguments? ~S" name form)
			   (if (not (= len 2))
			       (lint-format "quote has too ~A arguments: ~S" 
					    name 
					    (if (> (length form) 2) "many" "few") 
					    form)
			       (if (and (< quote-warnings 20)
					(or (number? (cadr form))
					    (boolean? (cadr form))
					    (string? (cadr form))
					    (vector? (cadr form))
					    (null? (cadr form))
					    (memq (cadr form) '(#<unspecified> #<undefined> #<eof>))))
				   (begin
				     (set! quote-warnings (+ quote-warnings 1))
				     (lint-format "quote is not needed here: ~A~A" 
						  name (truncated-list->string form)
						  (if (= quote-warnings 20) "; will ignore this error henceforth." "")))))))
		     env)

		    ((if)
		     (let ((len (length form)))
		       (if (> len 4)
			   (lint-format "if has too many clauses: ~A" name form)
			   (if (< len 3)
			       (lint-format "if has too few clauses: ~A" name form)
			       (let ((test (cadr form))
				     (true (caddr form))
				     (false (if (= len 4) (cadddr form) 'no-false)))

				 (unless (= last-if-line-number line-number)
				   (do ((iff form (cadddr iff))
					(iffs 0 (+ iffs 1)))
				       ((or (> iffs 3)
					    (not (pair? iff))
					    (not (= (length iff) 4))
					    (not (eq? (car iff) 'if)))
					(when (or (> iffs 2)
						  (and (= iffs 2)
						       (pair? iff)
						       (= (length iff) 3)
						       (eq? (car iff) 'if)))
					  (set! last-if-line-number line-number)
					  (lint-format "perhaps use cond: ~A" name
					    (lists->string form 
					      `(cond ,@(do ((iff form (cadddr iff))
							    (clauses ()))
							   ((or (not (pair? iff))
								(not (= (length iff) 4))
								(not (eq? (car iff) 'if)))
							    (append (reverse clauses)
								    (if (and (pair? iff)
									     (= (length iff) 3)
									     (eq? (car iff) 'if))
									`((,(cadr iff) ,(caddr iff)) (else #<unspecified>))
									`((else ,iff)))))
							 (set! clauses (cons (list (cadr iff) (caddr iff)) clauses))))))))))
				 
				 (if (never-false test)
				     (lint-format "if test is never false: ~A" name form)
				     (if (and (never-true test) true) ; complain about (if #f #f) later
					 (lint-format "if test is never true: ~A" name form)))
				 
				 (let ((expr (simplify-boolean test () () env)))
				   (if (not (side-effect? test env))
				       (if (or (equal? test true) (equal? expr true))
					   (lint-format "perhaps ~A" name 
							(lists->string form 
								       (if (eq? false 'no-false)
									   (simplify-boolean `(or ,expr #<unspecified>) () () env)
									   (simplify-boolean `(or ,expr ,false) () () env))))
					   (if (or (equal? test false) (equal? expr false))
					       (lint-format "perhaps ~A" name 
							    (lists->string form (simplify-boolean `(and ,expr ,true) () () env))))))
				   
				   (if (pair? false)
				       (begin
					 (if (and (eq? (car false) 'if)
						  (pair? (cdr false))
						  (pair? (cadr false))
						  (eq? (caadr false) 'not)
						  (or (equal? test (cadr (cadr false))) (equal? expr (cadr (cadr false))))
						  (not (side-effect? test env)))
					     (lint-format "pointless repetition of if test: ~A" name (lists->string form `(if ,expr ,true ,(caddr false)))))
					 
					 (if (and (eq? (car false) 'if) ; (if test0 expr (if test1 expr)) -> if (or test0 test1) expr) 
						  (equal? true (caddr false)))
					     (let ((test1 (simplify-boolean `(or ,expr ,(cadr false)) () () env)))
					       (lint-format "perhaps ~A" name (lists->string form `(if ,test1 ,true ,@(cdddr false))))))
					 
					 (if (and (pair? true) ; (if expr (set! var #t | #f) (set! var #f | #t)) -> (set! var expr|(not expr))??
						  (eq? (car true) 'set!)
						  (eq? (car false) 'set!)
						  (eq? (cadr true) (cadr false))
						  (boolean? (caddr true))
						  (boolean? (caddr false))
						  (not (eq? (caddr true) (caddr false))))
					     (lint-format "perhaps ~A"
							  name
							  (lists->string form 
									 (if (caddr true)
									     `(set! ,(cadr true) ,expr)
									     `(set! ,(cadr true) (not ,expr)))))))
				       (if (eq? false 'no-false)      ; no false branch
					   (begin
					     (if (and (pair? test)    ; (if (pair? lst) (for-each f lst)) -> (for-each f lst)
						      (eq? (car test) 'pair?)
						      (pair? true)
						      (memq (car true) '(map for-each))
						      (eq? (cadr test) (caddr true)))
						 (lint-format "perhaps ~A" name (lists->string form true)))
					     
					     (if (and (pair? true)     ; (if test0 (if test1 expr)) -> (if (and test0 test1) expr) (else #<unspecified>)
						      (eq? (car true) 'if)
						      (null? (cdddr true)))
						 (let ((test1 (simplify-boolean `(and ,expr ,(cadr true)) () () env)))
						   (lint-format "perhaps ~A" name (lists->string form `(if ,test1 ,(caddr true)))))))))
				   
				   (if (eq? expr #t)
				       (lint-format "perhaps ~A" name (lists->string form true))
				       (if (not expr)
					   (if (eq? false 'no-false)
					       (if true                             ; (if #f x) as a kludgey #<unspecified>
						   (lint-format "perhaps ~A" name (lists->string form #<unspecified>)))
					       (lint-format "perhaps ~A" name (lists->string form false)))
					   (if (not (equal? true false))
					       (if (boolean? true)
						   (if (boolean? false) ; !  (if expr #t #f) turned into something less verbose
						       (lint-format "perhaps ~A" name 
								    (lists->string form (if true 
											    expr 
											    (simplify-boolean `(not ,expr) () () env))))
						       (lint-format "perhaps ~A" name 
								    (lists->string form (if true
											    (if (eq? false 'no-false)
												expr
												(simplify-boolean `(or ,expr ,false) () () env))
											    (simplify-boolean 
											     (if (eq? false 'no-false)
												 `(not ,expr)
												 `(and (not ,expr) ,false))
											     () () env)))))
						   (if (boolean? false)
						       (lint-format "perhaps ~A" name 
								    (lists->string form (simplify-boolean
											 (if false 
											     (if (and (pair? expr) (eq? (car expr) 'not))
												 `(or ,(cadr expr) ,true) 
												 `(or (not ,expr) ,true))
											     `(and ,expr ,true))
											 () () env)))))
					       (if (= len 4)
						   (if (not (side-effect? test env))
						       (lint-format "if is not needed here: ~A" name (lists->string form true))
						       (lint-format "if is not needed here: ~A" name (lists->string form `(begin ,expr ,true)))))))))
				 (lint-walk name test env)
				 (set! env (lint-walk name true env))
				 (if (= len 4) (set! env (lint-walk name false env)))))))
		     env)

		    ((when unless)
		     ;; -------- when, unless --------
		     (if (< (length form) 3)
			 (begin
			   (lint-format "~A is messed up: ~A" name head (truncated-list->string form))
			   env)
			 (let ((test (cadr form)))
			   (if (and (pair? test)
				    (eq? (car test) 'not))
			       (lint-format "possible optimization: ~A -> ~A"
					    name 
					    (truncated-list->string form)
					    (truncated-list->string `(,(if (eq? head 'when) 'unless 'when)
								      ,(cadr test)
								      ,@(cddr form)))))
			   (if (never-false test)
			       (lint-format "~A test is never false: ~A" name head form)
			       (if (never-true test)
				   (lint-format "~A test is never true: ~A" name head form)))
			   (if (symbol? test)
			       (set-ref? test env)
			       (if (pair? test)
				   (lint-walk name test env)))
			   (lint-walk-body name head (cddr form) env))))
		    
		    ((cond)
		     ;; ---------------- cond ----------------
		     (let ((ctr 0)
			   (len (- (length form) 1)))
		       (if (negative? len)
			   (lint-format "cond is messed up: ~A" name (truncated-list->string form))
			   (let ((exprs ())
				 (result :unset)
				 (has-else #f)
				 (has-combinations #f)
				 (falses ())
				 (prev-clause #f)
				 (all-eqv #t)
				 (eqv-select #f))
			     (for-each
			      (lambda (clause)
				(set! ctr (+ ctr 1))
				(if all-eqv
				    (set! all-eqv (and (pair? clause)
						       (or (and (pair? (car clause))
								(memq (caar clause) '(eq? eqv? =))
								(= (length (car clause)) 3)
								(or (and (eqv-code-constant? (cadar clause))
									 (or (and (not eqv-select) (set! eqv-select (caddar clause)))
									     (equal? eqv-select (caddar clause))))
								    (and (eqv-code-constant? (caddar clause))
									 (or (and (not eqv-select) (set! eqv-select (cadar clause)))
									     (equal? eqv-select (cadar clause))))))
							   (memq (car clause) '(else #t))))))
				(if (and prev-clause
					 (not has-combinations)
					 (> len 2) 
					 (pair? clause)
					 (equal? (cdr clause) (cdr prev-clause)))
				    (if (memq (car clause) '(else #t))        ; (cond ... (x z) (else z)) -> (cond ... (else z))
					(if (not (side-effect? (car prev-clause) env))
					    (lint-format "this clause could be omitted: ~A" name prev-clause))
					(set! has-combinations #t)))          ; handle these later
				(set! prev-clause clause)
				
				(if (not (pair? clause))
				    (begin
				      (lint-format "cond clause is messed up: ~A" name (truncated-list->string clause))
				      (set! has-combinations #f))
				    
				    (let ((expr (simplify-boolean (car clause) () falses env))
					  (test (car clause))
					  (sequel (cdr clause)))
				      
				      (when (memq test '(else #t))
					(set! has-else #t)
					(if (and (pair? sequel)
						 (pair? (car sequel))
						 (null? (cdr sequel))
						 (eq? (caar sequel) 'cond))
					    (lint-format "else clause cond could be folded into the outer cond: ~A" name (truncated-list->string clause))))
				      (if (never-false expr)
					  (if (not (= ctr len))
					      (lint-format "cond test is never false: ~A" name form)
					      (if (and (not (memq expr '(#t else)))
						       (not (side-effect? test env)))
						  (lint-format "cond last test could be #t: ~A" name form)))
					  (if (never-true expr)
					      (lint-format "cond test is never true: ~A" name form)))
				      (if (not (side-effect? test env))
					  (begin
					    (if (and (not (memq test '(else #t)))
						     (pair? sequel)
						     (null? (cdr sequel)))
						(if (equal? test (car sequel))
						    (lint-format "no need to repeat the test: ~A" name (lists->string clause (list test)))
						    (if (and (pair? (car sequel))
							     (pair? (cdar sequel))
							     (null? (cddar sequel))
							     (equal? test (cadar sequel)))
							(lint-format "perhaps use => here: ~A" name 
								     (lists->string clause (list test '=> (caar sequel)))))))
					    (if (member test exprs)
						(lint-format "cond test repeated: ~A" name (truncated-list->string clause))
						(set! exprs (cons test exprs)))))
				      (if (boolean? expr)
					  (if (not expr)
					      (lint-format "cond test is always false: ~A" name (truncated-list->string clause))
					      (if (not (= ctr len))
						  (lint-format "cond #t clause is not the last: ~A" name (truncated-list->string form))))
					  (if (eq? test 'else)
					      (if (not (= ctr len))
						  (lint-format "cond else clause is not the last: ~A" name (truncated-list->string form)))
					      (lint-walk name test env)))
				      (if (eq? result :unset)
					  (set! result sequel)
					  (if (not (equal? result sequel))
					      (set! result :unequal)))
				      (if (pair? sequel)
					  (if (eq? (car sequel) '=>)
					      (if (or (not (pair? (cdr sequel)))
						      (pair? (cddr sequel)))
						  (lint-format "cond => target is messed up: ~A" name (truncated-list->string clause))
						  (let ((f (cadr sequel)))
						    (if (symbol? f)
							(let ((val (symbol->value f *e*)))
							  (if (procedure? val)
							      (if (not (aritable? val 1)) ; here values might be in test expr
								  (lint-format "=> target (~A) may be unhappy: ~A" name f clause))))
							(if (and (pair? f)
								 (eq? (car f) 'lambda)
								 (pair? (cdr f))
								 (pair? (cadr f))
								 (not (= (length (cadr f)) 1)))
							    (lint-format "=> target (~A) may be unhappy: ~A" name f clause)))
						    (lint-walk name f env)))
					      (lint-walk-body name head sequel env))
					  (if (not (null? sequel))  ; (not (null?...)) here is correct -- we're looking for stray dots (lint is confused)
					      (lint-format "cond clause is messed up: ~A" name (truncated-list->string clause))))
				      (if (not (side-effect? expr env))
					  (set! falses (cons expr falses))
					  (set! result :unequal)))))
			      (cdr form))
			     (if (and has-else (pair? result)) ; all result clauses are the same (and not implicit)
				 (if (null? (cdr result))
				     (lint-format "perhaps ~A" name (lists->string form (car result)))
				     (lint-format "perhaps ~A" name (lists->string form `(begin ,@result)))))
			     
			     (if (= len 2)
				 (let ((c1 (cadr form))
				       (c2 (caddr form)))
				   (if (and (pair? c1) (= (length c1) 2)
					    (pair? c2) (= (length c2) 2)
					    (boolean? (cadr c1))
					    (boolean? (cadr c2))
					    (memq (car c2) '(#t else)))
				       (if (equal? (cadr c1) (cadr c2))
					   (if (not (side-effect? (car c1) env))
					       (lint-format "perhaps ~A" name (lists->string form (cadr c1))))
					   (if (eq? (cadr c1) #t)
					       (lint-format "perhaps ~A" name (lists->string form (car c1)))
					       (lint-format "perhaps ~A" name (lists->string form `(not ,(car c1)))))))))
			     
			     (when has-combinations
			       (let ((new-clauses ())
				     (current-clauses ()))
				 (do ((clauses (cdr form) (cdr clauses)))
				     ((null? clauses)
				      (lint-format "perhaps ~A" name (lists->string form `(cond ,@(reverse new-clauses)))))
				   (let* ((clause (car clauses))
					  (result (cdr clause))) ; can be null in which case the test is the result
				     (if (and (pair? (cdr clauses))
					      (equal? result (cdar (cdr clauses))))
					 (set! current-clauses (cons clause current-clauses))
					 (if (pair? current-clauses)
					     (begin
					       (set! current-clauses (cons clause current-clauses))
					       (set! new-clauses (cons 
								  (cons (simplify-boolean `(or ,@(map car (reverse current-clauses))) () () env)
									result)
								  new-clauses))
					       (set! current-clauses ()))
					     (set! new-clauses (cons clause new-clauses))))))))

			     (when (and all-eqv
					(> len (if has-else 2 1))) ; (cond (x y)) -- kinda dumb, but (if x y ()) isn't much shorter
			       (lint-format "perhaps use case instead of cond: ~A" name
					    (lists->string 
					     form
					     `(case ,eqv-select 
						,@(map (lambda (clause)
							 (let ((test (car clause))
							       (exprs (cdr clause)))
							   (if (memq test '(else #t))
							       `(else ,@exprs)
							       (if (equal? eqv-select (cadr test))
								   `((,(unquoted (caddr test))) ,@exprs)
								   `((,(unquoted (cadr test))) ,@exprs)))))
						       (cdr form))
						,@(if has-else () `((else ()))))))) ))
		       env))
		    
		    ((case)
		     ;; ---------------- case ----------------		  
		     ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		     ;; also unlike cond, only 'else marks a default branch (not #t)
		     (if (< (length form) 3)
			 (lint-format "case is messed up: ~A" name (truncated-list->string form))
			 (let ((sel-type #t)
			       (selector (cadr form)))
			   (if (and (not (pair? selector))
				    (constant? selector))
			       (lint-format "case selector is a constant: ~A" name (truncated-list->string form)))
			   (lint-walk name selector env)
			   (if (and (pair? selector)
				    (symbol? (car selector)))
			       (begin
				 (set! sel-type (return-type (car selector) env))
				 (if (and (symbol? sel-type)
					  (not (memq sel-type selector-types)))
				     (lint-format "case selector may not work with eqv: ~A" name (truncated-list->string selector)))))
			   (let ((all-keys ())
				 (all-exprs ())
				 (ctr 0)
				 (result :unset)
				 (exprs-repeated #f)
				 (else-foldable #f)
				 (has-else #f)
				 (len (length (cddr form))))
			     (for-each
			      (lambda (clause)
				(set! ctr (+ ctr 1))
				(if (not (pair? clause))
				    (lint-format "case clause should be a list: ~A" name (truncated-list->string clause))
				    (let ((keys (car clause))
					  (exprs (cdr clause)))
				      (if (null? exprs)
					  (lint-format "clause result is missing: ~A" name clause))
				      (if (eq? result :unset)
					  (set! result exprs)
					  (if (not (equal? result exprs))
					      (set! result :unequal)))
				      (if (member exprs all-exprs)
					  (set! exprs-repeated exprs)
					  (set! all-exprs (cons exprs all-exprs)))
				      (if (and (pair? exprs)
					       (null? (cdr exprs))
					       (pair? (car exprs))
					       (pair? (cdar exprs))
					       (null? (cddar exprs))
					       (equal? selector (cadar exprs)))
					  (lint-format "perhaps use => here: ~A" name 
						       (lists->string clause (list keys '=> (caar exprs)))))
				      (if (pair? keys)
					  (if (not (proper-list? keys))
					      (if (null? keys)
						  (lint-format "null case key list: ~A" name (truncated-list->string clause))
						  (lint-format "stray dot in case case key list: ~A" name (truncated-list->string clause)))
					      (for-each
					       (lambda (key)
						 (if (or (vector? key)
							 (string? key)
							 (pair? key)
							 (hash-table? key))
						     (lint-format "case key ~S in ~S is unlikely to work (case uses eqv?)" name key clause))
						 (if (member key all-keys)
						     (lint-format "repeated case key ~S in ~S" name key clause)
						     (set! all-keys (cons key all-keys)))
						 ;; unintentional quote here, as in (case x ('a b)...) never happens and
						 ;;   is hard to distinguish from (case x ((quote a) b)...) which happens a lot
						 (if (not (compatible? sel-type (->type key)))
						     (lint-format "case key ~S in ~S is pointless" name key clause)))
					       keys))
					  (if (not (eq? keys 'else))
					      (lint-format "bad case key ~S in ~S" name keys clause)
					      (begin
						(set! has-else clause)
						;; exprs: (res) or if case, ((case ...)...)
						(if (not (= ctr len))
						    (lint-format "case else clause is not the last: ~A"
								 name 
								 (truncated-list->string (cddr form)))
						    (when (and (pair? exprs)
							       (pair? (car exprs))
							       (null? (cdr exprs))         ; just the case statement in the else clause
							       (eq? (caar exprs) 'case)
							       (equal? selector (cadar exprs))
							       (not (side-effect? selector env)))
						      (set! else-foldable (cddar exprs)))))))
				      (lint-walk-body name head exprs env))))
			      (cddr form))
			     (if (and has-else 
				      (pair? result)
				      (not else-foldable))
				 (begin
				   (if (null? (cdr result))
				       (lint-format "perhaps ~A" name (lists->string form (car result)))
				       (lint-format "perhaps ~A" name (lists->string form `(begin ,@result))))
				   (set! exprs-repeated #f)))
			     
			     (when (or exprs-repeated else-foldable)
			       (let* ((new-keys-and-exprs ())
				      (else-clause (if else-foldable
						       (call-with-exit
							(lambda (return)
							  (for-each (lambda (c) (if (eq? (car c) 'else) (return c))) else-foldable)
							  ()))
						       (or has-else ())))
				      (else-exprs (and (pair? else-clause) (cdr else-clause))))
				 
				 (define (merge-case-keys clause)
					;(format *stderr* "clause: ~S~%" clause)
				   (let ((keys (car clause))
					 (exprs (cdr clause)))
				     (when (and (pair? exprs)             ; ignore clauses that are messed up
						(not (eq? keys 'else))
						(not (equal? exprs else-exprs)))
				       (let ((prev (member exprs new-keys-and-exprs (lambda (a b) (equal? a (cdr b))))))
					 (if prev
					     (let* ((cur-clause (car prev))
						    (cur-keys (car cur-clause)))
					       (when (pair? cur-keys)
						 (set-car! cur-clause
							   (append cur-keys
								   (map (lambda (key)
									  (if (memv key cur-keys) (values) key))
									keys)))))
					     (set! new-keys-and-exprs (cons (cons (copy (car clause)) (cdr clause)) new-keys-and-exprs)))))))
				 
				 (for-each merge-case-keys (cddr form))
				 (if else-foldable
				     (for-each merge-case-keys else-foldable))
				 
					;(format *stderr* "~%~A -> new: ~A, else: ~A~%" form new-keys-and-exprs else-clause)
				 
				 (if (null? new-keys-and-exprs)
				     (if (or (null? else-clause)    ; can this happen? (it's caught above as an error)
					     (null? (cdr else-clause)))
					 (lint-format "perhaps ~A" name (lists->string form ()))
					 (if (null? (cddr else-clause))
					     (lint-format "perhaps ~A" name (lists->string form (cadr else-clause)))
					     (lint-format "perhaps ~A" name (lists->string form `(begin ,@(cdr else-clause))))))
				     (lint-format "perhaps ~A" name 
						  (lists->string form 
								 (if (pair? else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs) ,else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs)))))))))))
		     env)
		    
		    ((do)
		     ;; ---------------- do ----------------		  
		     (let ((vars ()))
		       (if (or (< (length form) 3)
			       (not (proper-list? (cadr form)))
			       (not (proper-list? (caddr form))))
			   (lint-format "do is messed up: ~A" name (truncated-list->string form))
			   
			   (let ((step-vars (cadr form)))
			     
			     (if (not (side-effect? form env))
				 (let ((end+result (caddr form)))
				   (if (or (not (pair? end+result))
					   (null? (cdr end+result)))
				       (lint-format "this do-loop could be replaced by (): ~A" name (truncated-list->string form))
				       (if (and (null? (cddr end+result))
						(code-constant? (cadr end+result)))
					   (lint-format "this do-loop could be replaced by ~A: ~A" name (cadr end+result) (truncated-list->string form))))))
			     
			     ;; walk the init forms before adding the step vars to env
			     (do ((bindings step-vars (cdr bindings)))
				 ((not (pair? bindings))
				  (if (pair? bindings)
				      (lint-format "do variable list is not a proper list? ~S" name step-vars)))
			       (if (binding-ok? name head (car bindings) env #f)
				   (begin
				     (lint-walk name (cadar bindings) env)
				     (set! vars (append (list (make-var :name (caar bindings) 
									:type (->type (cadar bindings)) 
									:value (and (pair? (cddar bindings)) (caddar bindings))))
							vars)))))
			     
			     ;; walk the step exprs
			     (do ((bindings step-vars (cdr bindings)))
				 ((not (pair? bindings)))
			       (let ((stepper (car bindings))) ; the entire binding: '(i 0 (+ i 1))
				 (when (and (binding-ok? name head stepper env #t)
					    (pair? (cddr stepper)))
				   (lint-walk name (caddr stepper) (append vars env))
				   (if (eq? (car stepper) (caddr stepper))  ; (i 0 i) -> (i 0)
				       (lint-format "perhaps ~A" name (lists->string stepper (list (car stepper) (cadr stepper)))))
				   (let ((data (var-member (car stepper) vars)))
				     (set! (var-ref data) #f))
				   (when (and (pair? (caddr stepper))
					      (not (eq? (car stepper) (cadr stepper))) ; (lst lst (cdr lst))
					      (eq? (car (caddr stepper)) 'cdr)
					      (eq? (cadr stepper) (cadr (caddr stepper))))
				     (lint-format "this looks suspicious: ~A" name stepper)))))
			     
			     ;; walk the body and end stuff (it's too tricky to find infinite do loops)
			     (if (pair? (caddr form))
				 (let ((end+result (caddr form)))
				   (lint-walk-body name head (cddr form) (append vars env))
				   (if (pair? end+result)
				       (let ((end (car end+result)))
					 (if (and (symbol? end) (memq end '(= > < >= <= null? not)))
					     (lint-format "perhaps missing parens: ~A" name end+result))
					 (if (never-false end)
					     (lint-format "end test is never false: ~A" name end)
					     (if end ; it's not #f
						 (if (never-true end)
						     (lint-format "end test is never true: ~A" name end)
						     (let ((v (and (pair? end)
								   (memq (car end) '(< > <= >=))
								   (pair? (cdr end))
								   (symbol? (cadr end))
								   (member (cadr end) vars (lambda (a b) (eq? a (var-name b)))))))
						       ;; if found, v is the var info
						       (when (pair? v)
							 (let ((step (var-value (car v))))
							   (when (pair? step)
							     (let ((inc (and (memq (car step) '(+ -))
									     (pair? (cdr step))
									     (pair? (cddr step))
									     (or (and (real? (cadr step)) (cadr step))
										 (and (real? (caddr step)) (caddr step))))))
							       (when (real? inc)
								 (if (or (and (eq? (car step) '+)
									      (positive? inc)
									      (memq (car end) '(< <=)))
									 (and (eq? (car step) '-)
									      (positive? inc)
									      (memq (car end) '(> >=))))
								     (lint-format "do step looks like it doesn't match end test: ~A" name 
										  (lists->string step end))))))))))
						 (if (pair? (cdr end+result))
						     (lint-format "result is unreachable: ~A" name end+result)))))))
				 (lint-walk-body name head (cdddr form) (append vars env)))
			     
			     ;; before report-usage, check for unused variables, and don't complain about them if
			     ;;   they are referenced in an earlier step expr?(!)
			     (do ((v vars (cdr v)))
				 ((null? v))
			       (let ((var (car v)))
				 (unless (var-ref var)
				   ;; var was not seen in the end+result/body or any subsequent step exprs
				   ;;   vars is reversed order, so we need only scan var-value of the rest
				   
				   (if (side-effect? (var-value var) env)
				       (set! (var-ref var) #t)
				       (for-each
					(lambda (nv)
					  (if (or (eq? (var-name var) (var-value nv))
						  (and (pair? (var-value nv))
						       (tree-member (var-name var) (var-value nv))))
					      (set! (var-ref var) #t)))
					(cdr v))))))
			     
			     (report-usage name 'variable head vars)
			     
			     ;; check for do-loop as copy/fill! stand-in
			     (let ((end-test (and (pair? (caddr form)) (caaddr form)))
				   (body (cdddr form))
				   (setv #f))
			       (when (and (pair? end-test)
					  (= (length vars) 1)
					  (= (length body) 1)
					  (pair? (car body)) 
					  (memq (caar body) '(vector-set! float-vector-set! int-vector-set! list-set! string-set!))
					  (eq? (var-type (car vars)) 'integer?)
					  (eq? (car end-test) '=)
					  (eq? (cadr end-test) (var-name (car vars)))
					  (eq? (caddar body) (var-name (car vars)))
					  (let ((val (car (cdddar body))))
					    (set! setv val)
					    (or (code-constant? val)
						(and (pair? val)
						     (memq (car val) '(vector-ref float-vector-ref int-vector-ref list-ref string-ref))
						     (eq? (caddr val) (var-name (car vars)))))))
				 (if (code-constant? setv)
				     (lint-format "perhaps ~A" name 
						  (lists->string form `(fill! ,(cadar body) ,(car (cdddar body)) 0 ,(caddr end-test))))
				     (lint-format "perhaps ~A" name 
						  (lists->string form `(copy ,(cadr setv) ,(cadar body) 0 ,(caddr end-test)))))))))
		       env))
		    
		    ((let)
		     ;; ---------------- let ----------------		  
		     (if (or (< (length form) 3)
			     (and (not (symbol? (cadr form)))
				  (not (list? (cadr form)))))
			 (lint-format "let is messed up: ~A" name (truncated-list->string form))
			 (let ((named-let (and (symbol? (cadr form)) (cadr form))))
			   (if (keyword? named-let)
			       (lint-format "bad let name: ~A" name named-let))
			   
			   (unless named-let
			     ;; this could be extended to other such cases
			     (or (any? (lambda (var)
					 (or (not (pair? var))
					     (not (pair? (cdr var)))
					     (not (code-constant? (cadr var)))))
				       (cadr form))
				 (any? (lambda (expr) 
					 (side-effect? expr env))
				       (cddr form))
				 (catch #t
				   (lambda ()
				     (let ((val (eval (copy-tree form) (rootlet))))
				       (lint-format "perhaps ~A" name (lists->string form val))))
				   (lambda args
				     'error))))
			   
			   (let ((vars (if (and named-let 
						(not (keyword? named-let))
						(or (null? (caddr form))
						    (and (proper-list? (caddr form))
							 (every? pair? (caddr form)))))
					   (list (make-fvar :name named-let 
							    :ftype head
							    :decl (eval (list 'define (cons '_ (map car (caddr form))) #f))
							    :arglist (map car (caddr form))
							    :value form
							    :env env
							    :location #__line__))  ; TODO: named-let*
					   ()))
				 (varlist (if named-let (caddr form) (cadr form)))
				 (body (if named-let (cdddr form) (cddr form))))
			     
			     (if (not (list? varlist))
				 (lint-format "let is messed up: ~A" name (truncated-list->string form))
				 (if (and (null? varlist)
					  (pair? body)
					  (null? (cdr body))
					  (not (side-effect? (car body) env)))
				     (lint-format "perhaps ~A" name (lists->string form (car body)))))
			     
			     (do ((bindings varlist (cdr bindings)))
				 ((not (pair? bindings))
				  (if (pair? bindings)
				      (lint-format "let variable list is not a proper list? ~S" name varlist)))
			       (if (binding-ok? name head (car bindings) env #f)
				   (let ((val (cadar bindings)))
				     (if (and (pair? val)
					      (eq? 'lambda (car val))
					      (not (hash-table-ref globals (caar bindings)))
					      (tree-car-member (caar bindings) val)
					      (not (var-member (caar bindings) env)))
					 (lint-format "let variable ~A is called in its binding?  Perhaps let should be letrec: ~A"
						      name (caar bindings) 
						      (truncated-list->string bindings)))
				     (lint-walk name val env)
				     
				     ;; can we tell its type and (as long as not set) check for type errors?
				     ;; need a function that turns a constant into a type indication,
				     ;;   then append that as the 4th entry below (used only in do?)
				     ;;   then use that in arg checks if arg is a known var
				     
				     (set! vars (append (list (make-var :name (caar bindings) :value val :type (->type val))) vars)))))
			     ;; each var is (sym ref set opt-type-data new)
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name head body cur-env))
				    (nvars (if (null? cur-env)
					       e
					       (and (not (eq? e cur-env))
						    (env-difference name e cur-env ())))))
					;(format *stderr* "nvars: ~A e: ~A~%" nvars e)
			       (if (pair? nvars)
				   (if (eq? (var-name (car nvars)) '[anonymous])
				       (begin
					 (set! env (cons (car nvars) env))
					 (set! nvars (cdr nvars)))
				       (set! vars (append nvars vars)))))
			     (report-usage name 'variable head vars))))
		     env)
		    
		    ((let*)
		     ;; ---------------- let* ----------------		  
		     (if (< (length form) 3)
			 (lint-format "let* is messed up: ~A" name (truncated-list->string form))
			 (let ((named-let (and (symbol? (cadr form)) (cadr form))))
			   (let ((vars (if named-let (list (make-var :name named-let)) ()))
				 (varlist (if named-let (caddr form) (cadr form)))
				 (side-effects #f))
			     (if (not (list? varlist))
				 (lint-format "let* is messed up: ~A" name (truncated-list->string form)))
			     (do ((bindings varlist (cdr bindings)))
				 ((not (pair? bindings))
				  (if (pair? bindings)
				      (lint-format "let* variable list is not a proper list? ~S" 
						   name (if named-let (caddr form) (cadr form)))))
			       (if (binding-ok? name head (car bindings) env #f)
				   (begin
				     (if (not side-effects)
					 (set! side-effects (side-effect? (cadar bindings) env)))
				     (lint-walk name (cadar bindings) (append vars env))
				     (set! vars (append (list (make-var :name (caar bindings) :value (cadar bindings) :type (->type (cadar bindings)))) vars)))))
			     
			     (if (and (not side-effects)
				      (not (any? var-ref vars)))
				 (lint-format "let* could be let: ~A" name (truncated-list->string form)))
			     
			     ;; in s7, let evaluates var values top down, so this message is correct
			     ;;   even in cases like (let ((ind (open-sound...)) (mx (maxamp))) ...)
			     ;; in r7rs, the order is not specified (section 4.2.2 of the spec), so
			     ;;   here we would restrict this message to cases where there is only
			     ;;   one variable, or where subsequent values are known to be independent.
			     ;; if each function could tell us what globals it depends on or affects,
			     ;;   we could make this work in all cases.
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name head (if named-let (cdddr form) (cddr form)) cur-env))
				    (nvars (and (not (eq? e cur-env))
						(env-difference name e cur-env ()))))
			       (if (pair? nvars)
				   (set! vars (append nvars vars))))
			     
			     (report-usage name 'variable head vars))))
		     env)
		    
		    ((letrec letrec*) 
		     ;; ---------------- letrec ----------------		  
		     (if (< (length form) 3)
			 (lint-format "~A is messed up: ~A" name head (truncated-list->string form))
			 (let ((vars ()))
			   (if (null? (cadr form))
			       (lint-format "~A could be let: ~A" name head (truncated-list->string form))
			       (if (not (pair? (cadr form)))
				   (lint-format "~A is messed up: ~A" name head (truncated-list->string form))
				   (if (and (null? (cdadr form))
					    (eq? head 'letrec*)) ; this happens all the time!
				       (lint-format "letrec* could be letrec? ~A" name (truncated-list->string form)))))
			   (do ((bindings (cadr form) (cdr bindings)))
			       ((not (pair? bindings))
				(if (pair? bindings)
				    (lint-format "letrec variable list is not a proper list? ~S" name (cadr form))))
			     (if (binding-ok? name head (car bindings) env #f)
				 (set! vars (append (list (make-var :name (caar bindings) :value (cadar bindings) :type (->type (cadar bindings)))) vars))))
			   (let ((new-env (append vars env)))
			     (do ((bindings (cadr form) (cdr bindings)))
				 ((not (pair? bindings)))
			       (if (binding-ok? name head (car bindings) env #t)
				   (lint-walk name (cadar bindings) new-env)))
			     
			     (let* ((cur-env (append vars env))
				    (e (lint-walk-body name head (cddr form) cur-env))
				    (nvars (and (not (eq? e cur-env))
						(env-difference name e cur-env ()))))
			       (if (pair? nvars)
				   (set! vars (append nvars vars)))))
			   
			   (report-usage name 'variable head vars)))
		     env)
		    
		    ((begin)
		     ;; ---------------- begin ----------------
		     (if (not (proper-list? form))
			 (begin
			   (lint-format "stray dot in begin? ~A" name (truncated-list->string form))
			   env)
			 (begin
			   (if (and (pair? (cdr form))
				    (null? (cddr form)))
			       (lint-format "begin could be omitted: ~A" name (truncated-list->string form)))
			   (lint-walk-body name head (cdr form) env))))
		    
		    ((with-let)
		     ;; -------- with-let --------
		     (if (< (length form) 3)
			 (lint-format "~A is messed up: ~A" head name (truncated-list->string form))
			 (let ((e (cadr form)))
			   (if (or (and (code-constant? e)
					(not (let? e)))
				   (and (symbol? e)
					(defined? e)
					(not (let? (symbol->value e))))
				   (and (pair? e)
					(let ((op (return-type (car e) env)))
					  (and op
					       (not (return-type-ok? 'let? op))))))
			       (lint-format "~A: first argument should be an environment: ~A" head name (truncated-list->string form)))
			   (if (symbol? e)
			       (set-ref? e env)
			       (if (pair? e)
				   (begin
				     (if (and (null? (cdr e))
					      (eq? (car e) 'curlet))
					 (lint-format "~A is not needed here: ~A" head name (truncated-list->string form)))
				     (lint-walk name e env))))
			   (let ((walked #f))
			     (if (or (and (symbol? e)
					  (memq e '(*gtk* *motif* *gl* *libc* *libm* *libgdbm* *libgsl*)))
				     (and (pair? e)
					  (eq? (car e) 'sublet)
					  (pair? (cdr e))
					  (memq (cadr e) '(*gtk* *motif* *gl* *libc* *libm* *libgdbm* *libgsl*))
					  (set! e (cadr e))))
				 (let ((lib (if (not (defined? e))
						(let ((file (*autoload* e)))
						  (and (string? file) 
						       (load file)))
						(symbol->value e))))
				   (when (let? lib)
				     (let ((old-e *e*))
				       (set! *e* lib)
				       (let* ((e (lint-walk-body name head (cddr form) env))
					      (vars (if (not (eq? e env))
							(env-difference name e env ())
							())))
					 (report-usage name 'variable head vars))
				       (set! *e* old-e)
				       (set! walked #t)))))

			     (unless walked
			       (lint-walk-body name head (cddr form) env)) )))
		     env)
		    
		    ((defmacro defmacro*) 
		     ;; ---------------- defmacro ----------------
		     (if (or (< (length form) 4)
			     (not (symbol? (cadr form))))
			 (lint-format "~A declaration is messed up: ~A" name head (truncated-list->string form))
			 (let ((sym (cadr form))
			       (args (caddr form))
			       (body (cdddr form)))
			   (if (and (pair? args)
				    (repeated-member? args env))
			       (lint-format "~A parameter is repeated: ~A" name head (truncated-list->string args)))
			   (lint-walk-function head sym args body form env))))
		    
		    ((defgenerator)
		     ;; ---------------- defgenerator ----------------
		     (get-generator form name head)
		     env)
		    
		    ((define-syntax let-syntax letrec-syntax define-module) ; all meaningless in s7
		     ;; actually the real problem with checking other schemes' code is that they use a large number
		     ;; of non-standard # and \ forms.  The # forms can mostly be kludged up via #*readers, but I'm
		     ;; not going to start building in all the crazy \ stuff.  
		     ;; Some schemes use the execrable [] substitutes for () -- Gauche in particular.
		     env)
		    
		    (else
		     ;; ---------------- everything else ----------------		  
		     (if (not (proper-list? form))
			 (begin
			   ;; these appear to be primarily macro arguments
			   (if (and (pair? form)
				    (symbol? (car form))
				    (procedure? (symbol->value (car form) *e*)))
			       (lint-format "unexpected dot: ~A" name (truncated-list->string form)))
			   env)
			 (begin
			   (when (symbol? head)
			     (check-call name head form env)

			     (if (not (or (hash-table-ref globals head)
					  (var-member head env)))
				 (check-special-cases name head form env))
			     (if (assq head deprecated-ops)
				 (lint-format "~A is deprecated; use ~A" name head (cdr (assq head deprecated-ops))))
			     
			     (if (and (not (= line-number last-simplify-numeric-line-number))
				      (not (hash-table-ref globals head))
				      (hash-table-ref numeric-ops head)
				      (not (var-member head env)))
				 (let ((val (simplify-numerics form env)))
				   (if (not (equal-ignoring-constants? form val))
				       (begin
					 (set! last-simplify-numeric-line-number line-number)
					 (lint-format "perhaps ~A" name (lists->string form val))))))
			     
			     ;; if we loaded this file first, and f (head) is defined (e.g. scan above),
			     ;; and it is used before it is defined, but not thereafter, the usage stuff 
			     ;; can get confused, so other-identifiers is trying to track those.
			     
			     (if (and (not (hash-table-ref other-identifiers head))
				      (not (defined? head start-up-let)))
				 (hash-table-set! other-identifiers head #t)))
			   
			   (when (and (pair? head)
				      (> (length head) 0)
				      (eq? (car head) 'lambda))
			     (if (and (proper-list? (cadr head))
				      (not (= (length (cadr head)) (length (cdr form)))))
				 (lint-format "~A has ~A arguments: ~A" 
					      head (car head) 
					      (if (> (length (cadr head)) (length (cdr form)))
						  "too few" "too many")
					      (truncated-list->string form))))
			   
			   (let ((vars env))
			     (for-each
			      (lambda (f)
				(set! vars (lint-walk name f vars)))
			      form))
			   ))
		     env)
		    ))
		
		;; else form is not a symbol and not a pair
		env)))
      
    ;;; --------------------------------------------------------------------------------
      (let ((documentation "(lint file port) looks for infelicities in file's scheme code"))
	(lambda* (file (outp *lint-output-port*) (report-input #t))
	  (set! outport outp)
	  (set! globals (make-hash-table))
	  (set! other-identifiers (make-hash-table))
	  (set! loaded-files ())
	  (set! last-simplify-boolean-line-number -1)
	  (set! last-simplify-numeric-line-number -1)
	  (set! last-checker-line-number -1)
	  (set! last-if-line-number -1)
	  (set! line-number -1)
	  (set! quote-warnings 0)
	  
	  ;(format *stderr* "lint ~S~%" file)
	  
	  (let ((fp (if (input-port? file)
			file
			(begin
			  (set! *current-file* file)
			  (if *load-file-first* ; this can improve the error checks
			      (load file))
			  (catch #t
			    (lambda ()
			      (let ((p (open-input-file file)))
				(if report-input 
				    (if (and (output-port? outport)
					     (not (member outport (list *stderr* *stdout*))))
					(format outport "~%~NC~%;~A~%" 16 #\- file)
					(format outport ";~A~%" file)))
				(set! loaded-files (cons file loaded-files))
				p))
			    (lambda args
			      (format outport " can't open ~S: ~A~%" file (apply format #f (cadr args)))
			      #f))))))
	    
	    (if (input-port? fp)
		(let ((vars ())
		      (line 0)
		      (last-form #f)
		      (last-line-number -1))
		  (do ((form (read fp) (read fp)))
#|
		  (do ((form 
			(catch #t
			  (lambda ()
			    (read fp))
			  (lambda args
			    (format *stderr* "~A: ~A~%" file args)))
			(catch #t
			  (lambda ()
			    (read fp))
			  (lambda args
			    (format *stderr* "~A: ~A~%" file args)))))
|#
		      ((eof-object? form))
		    (if (pair? form)
			(set! line (max line (pair-line-number form))))
		    
		    (if (and (not (= last-line-number -1))
			     (not (side-effect? last-form vars)))
			(format outport " top-level (line ~D): this has no effect: ~A~%" 
				last-line-number
				(truncated-list->string last-form)))
		    (set! last-form form)
		    (set! last-line-number line)
		    (set! vars (lint-walk (if (symbol? form) 
					      form 
					      (and (pair? form) 
						   (car form)))
					  form 
					  vars)))
		  
		  (if (and (pair? vars)
			   *report-multiply-defined-top-level-functions*)
		      (for-each
		       (lambda (var)
			 (let ((var-file (hash-table-ref *top-level-objects* (car var))))
			   (if (not var-file)
			       (hash-table-set! *top-level-objects* (car var) *current-file*)
			       (if (and (string? *current-file*)
					(not (string=? var-file *current-file*)))
				   (format outport ";~S is defined at the top level in ~S and ~S~%" (car var) var-file *current-file*)))))
		       vars))
		  
		  (if (and (string? file)
			   (pair? vars)
			   *report-unused-top-level-functions*)
		      (report-usage file 'top-level-var "" vars))
	      
		  (if (not (input-port? file))
		      (close-input-port fp))))))))))



;;; --------------------------------------------------------------------------------
;;; this reads an HTML file, finds likely-looking scheme code, and runs lint over it.
;;;    called on all snd files in hg.scm

(define (html-lint file)
  
  (define (remove-markups str)
    (let ((tpos (string-position "<b>" str)))
      (if tpos
	  (let ((epos (string-position "</b>" str)))
	    (remove-markups (string-append (substring str 0 tpos)
					   (substring str (+ tpos 3) epos)
					   (substring str (+ epos 4)))))
	  (let ((apos (string-position "<a " str))
		(epos (string-position "<em " str)))
	    (if (and (not apos)
		     (not epos))
		str
		(let* ((pos (if (and apos epos) (min apos epos) (or apos epos)))
		       (bpos (char-position #\> str (+ pos 1)))
		       (epos (if (and apos (= pos apos))
				 (string-position "</a>" str (+ bpos 1))
				 (string-position "</em>" str (+ bpos 1)))))
		  (string-append (substring str 0 pos)
				 (substring str (+ bpos 1) epos)
				 (remove-markups (substring str (+ epos (if (and apos (= apos pos)) 4 5)))))))))))
  
  (define (fixup-html str)
    (let ((pos (char-position #\& str)))
      (if (not pos)
	  str
	  (string-append (substring str 0 pos)
			 (let ((epos (char-position #\; str pos)))
			   (let ((substr (substring str (+ pos 1) epos)))
			     (let ((replacement (cond ((string=? substr "gt") ">")
						      ((string=? substr "lt") "<")
						      ((string=? substr "mdash") "-")
						      ((string=? substr "amp") "&")
						      (else (format #t "unknown: ~A~%" substr)))))
			       (string-append replacement
					      (fixup-html (substring str (+ epos 1)))))))))))
  
  (call-with-input-file file
    (lambda (f)
      (do ((line-num 0 (+ line-num 1))
	   (line (read-line f #t) (read-line f #t)))
	  ((eof-object? line))
	
	;; look for <pre , gather everything until </pre>
	;;   decide if it is scheme code (first char is #\()
	;;   if so, clean out html markup stuff, call lint on that
	
	(let ((pos (string-position "<pre" line)))
	  (if pos
	      (let ((code (substring line (+ (char-position #\> line) 1))))
		(do ((cline (read-line f #t) (read-line f #t))
		     (rline 1 (+ rline 1)))
		    ((string-position "</pre>" cline)
		     (set! line-num (+ line-num rline)))
		  (set! code (string-append code cline)))
		
		;; is first non-whitespace char #\(? ignoring comments
		(let ((len (length code)))
		  (do ((i 0 (+ i 1)))
		      ((>= i len))
		    (let ((c (string-ref code i)))
		      (if (not (char-whitespace? c))
			  (if (char=? c #\;)
			      (set! i (char-position #\newline code i))
			      (begin
				(set! i (+ len 1))
				(if (char=? c #\()
				    (catch #t
				      (lambda ()
					(let ((ncode (with-input-from-string 
							 (fixup-html (remove-markups code))
						       read)))
					  (call-with-output-file "t631-temp.scm"
					    (lambda (fout)
					      (format fout "~S~%" ncode)))
					  (let ((outstr (call-with-output-string
							 (lambda (p)
							   (let ((old-shadow *report-shadowed-variables*))
							     (set! *report-shadowed-variables* #t)
							     (lint "t631-temp.scm" p #f)
							     (set! *report-shadowed-variables* old-shadow))))))
					    (if (> (length outstr) 0)
						(format #t ";~A ~D: ~A~%" file line-num outstr)))))
				      (lambda args
					(format #t ";~A ~D, error in read: ~A ~A~%" file line-num args
						(fixup-html (remove-markups code)))))))))))))))))))
