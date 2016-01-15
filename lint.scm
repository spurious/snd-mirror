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

(set! *#readers* (list (cons #\_ (lambda (str)
				   (and (string=? str "__line__")
					(port-line-number))))))

(unless (provided? 'snd)
  (define defanimal define*)
  (define definstrument define*)
  (define defgenerator define*))


;;; --------------------------------------------------------------------------------


(define lint

  (let ((no-side-effect-functions 
	 ;; ideally we'd be able to add functions to this list, perhaps similar to the signatures
	 (let ((ht (make-hash-table)))
	   (for-each
	    (lambda (op) 
	      (hash-table-set! ht op #t))
	    '(* + - / < <= = > >= 
	      abs acos acosh and angle append aritable? arity ash asin asinh assoc assq assv atan atanh 
	      begin boolean? byte-vector byte-vector?
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
	      gcd gensym?
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
	      pair-line-number pair? port-closed? port-filename port-line-number positive? procedure-documentation
	      procedure-setter procedure-signature procedure-source procedure? proper-list? provided?
	      quasiquote quote quotient
	      random-state random-state->list random-state? rational? rationalize real-part real? remainder reverse rootlet round
	      s7-version sequence? sin sinh sqrt stacktrace string string->list string->number string->symbol string-append 
	      string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-downcase string-length
	      string-position string-ref string-upcase string<=? string<? string=? string>=? string>? string?
	      sublet substring symbol symbol->dynamic-value symbol->keyword symbol->string symbol->value symbol?
	      tan tanh truncate
	      unless
	      vector vector-append vector->list vector-dimensions vector-length vector-ref vector?
	      when with-baffle with-let with-input-from-file with-input-from-stringwith-output-to-string
	      zero?))
	   ;; do not include file-exists? or directory?
	   ;; should this include peek-char or unlet ?
	   ht))
	
	(deprecated-ops '((global-environment . rootlet)
			  (current-environment . curlet)
			  (make-procedure-with-setter . dilambda)
			  (procedure-with-setter? . dilambda?)
			  (make-random-state . random-state)
			  ;;(make-rectangular . complex)
			  
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
				  string=? string<=? string>=? string<? string>? string-ci=? string-ci<=? string-ci>=? string-ci<? string-ci>?
				  char=? char<=? char>=? char<? char>? char-ci=? char-ci<=? char-ci>=? char-ci<? char-ci>?
				  boolean=? symbol=?))
			       h))
	
	(repeated-args-table-2 (let ((h (make-hash-table)))
				 (for-each
				  (lambda (op)
				    (set! (h op) #t))
				  '(= max min < > <= >= and or
				    string=? string<=? string>=? string<? string>? string-ci=? string-ci<=? string-ci>=? string-ci<? string-ci>?
				    char=? char<=? char>=? char<? char>? char-ci=? char-ci<=? char-ci>=? char-ci<? char-ci>?
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
				'(#\A #\S #\C #\F #\E #\G #\O #\D #\B #\X #\P #\N #\W #\, #\{ #\} #\* #\@
				  #\a #\s #\c #\f #\e #\g #\o #\d #\b #\x #\p #\n #\w
				  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
			       chars))
	
	(selector-types '(#t symbol? char? boolean? integer? rational? real? complex? number? null? eof-object?))
	(outport #t)
	(loaded-files #f)
	(globals #f)
	(*e* #f)
	(other-identifiers #f)
	(quote-warnings 0)
	(last-simplify-boolean-line-number -1)
	(last-simplify-numeric-line-number -1)
	(last-if-line-number -1)
	(last-checker-line-number -1)
	(line-number -1)
	(lambda-marker '[lambda]))
    
    (set! *e* (curlet))

    (define var-name car)
    (define (var? v) (and (pair? v) (let? (cdr v))))
    (define var-member assq)
    
    (define var-ref (dilambda (lambda (v) (let-ref (cdr v) 'ref)) (lambda (v x) (let-set! (cdr v) 'ref x))))
    (define var-set (dilambda (lambda (v) (let-ref (cdr v) 'set)) (lambda (v x) (let-set! (cdr v) 'set x))))
    (define var-type (dilambda (lambda (v) (let-ref (cdr v) 'type)) (lambda (v x) (let-set! (cdr v) 'type x))))
    (define var-value (dilambda (lambda (v) (let-ref (cdr v) 'value)) (lambda (v x) (let-set! (cdr v) 'value x))))
    (define var-ftype (dilambda (lambda (v) (let-ref (cdr v) 'ftype)) (lambda (v x) (let-set! (cdr v) 'ftype x))))
    (define var-side-effect (dilambda (lambda (v) (let-ref (cdr v) 'side-effect)) (lambda (v x) (let-set! (cdr v) 'side-effect x))))
    (define var-arglist (dilambda (lambda (v) (let-ref (cdr v) 'arglist)) (lambda (v x) (let-set! (cdr v) 'arglist x))))
    (define var-signature (dilambda (lambda (v) (let-ref (cdr v) 'signature)) (lambda (v x) (let-set! (cdr v) 'signature x))))
    
    (define* (make-var name type value)
      (cons name (inlet 'value value 'type type 'set #f 'ref #f)))
    
    
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

    (define (tree-count1 x tree count)
      (if (and (< count 2)
	       (pair? tree)
	       (not (eq? (car tree) 'quote)))
	  (tree-count1 x (cdr tree)
		      (+ (tree-count1 x (car tree) (+ count (if (eq? x (car tree)) 1 0)))
			 (if (eq? x (cdr tree)) 1 0)))
	  count))

    (define (tree-length tree len)
      (if (pair? tree)
	  (tree-length (car tree) (tree-length (cdr tree) len))
	  (if (null? tree)
	      len
	      (+ len 1))))

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

    (define (quoted-not? x)
      (and (pair? x)
	   (eq? (car x) 'quote)
	   (pair? (cdr x))
	   (not (cadr x))))

    (define (code-constant? x)
      (and (not (symbol? x))
	   (or (not (pair? x))
	       (eq? (car x) 'quote))))

    (define (eqv-code-constant? x)
      (or (number? x)
	  (char? x)
	  (and (pair? x)
	       (eq? (car x) 'quote)
	       (or (symbol? (cadr x))
		   (and (not (pair? (cadr x)))
			(eqv-code-constant? (cadr x)))))
	  (memq x '(#t #f () #<unspecified> #<undefined> #<eof>))))


    (define (tree-arg-member sym tree)
      (and (proper-list? tree)
	   (or (and (memq sym (cdr tree))
		    tree)
	       (and (pair? (car tree))
		    (tree-arg-member sym (car tree)))
	       (and (pair? (cdr tree))
		    (call-with-exit
		     (lambda (return)
		       (for-each
			(lambda (b)
			  (cond ((and (pair? b)
				      (tree-arg-member sym b))
				 => return)))
			(cdr tree))
		       #f))))))
    
    (define (arg-signature fnc env)
      (and (symbol? fnc)
	   (let ((fd (or (var-member fnc env)
			 (hash-table-ref globals fnc))))
	     (if (var? fd)
		 (and (symbol? (var-ftype fd))
		      (var-signature fd))
		 (let ((f (symbol->value fnc *e*)))
		   (and (procedure? f)
			(procedure-signature f)))))))
    
    (define (dummy-func name form f)
      (catch #t 
	(lambda ()
	  (eval f))
	(lambda args
	  (lint-format "in ~A, ~A" name form (apply format #f (cadr args))))))
    
    (define* (make-fvar name ftype arglist decl value env)
      (cons name 
	    (inlet 'type 'closure
		   'signature (let ((body (and (memq ftype '(define define* lambda lambda* let))
					       (cddr value))))
				(and (pair? body)
				     (let ((sig #f))
				       (if (null? (cdr body))
					   (if (not (pair? (car body)))
					       (if (not (symbol? (car body)))
						   (set! sig (list (->type (car body)))))
					       (set! sig (cond ((arg-signature (caar body) env) => (lambda (a) (and (pair? a) (list (car a)))))
							       (else #f)))))
				       (if (not (pair? sig))
					   (set! sig (list #t)))
				       
				       (when (and (proper-list? arglist)
						  (not (any? keyword? arglist)))
					 (for-each
					  (lambda (arg)       ; new function's parameter
					    (set! sig (cons #t sig))
					    ;(if (pair? arg) (set! arg (car arg)))
					    ;; causes trouble when tree-count1 sees keyword args in s7test.scm
					    (if (= (tree-count1 arg body 0) 1)
						(let ((p (tree-arg-member arg body)))
						  (when (pair? p)
						    (let ((f (car p))
							  (m (memq arg (cdr p))))
						      (if (pair? m)
							  (let ((fsig (arg-signature f env)))
							    (if (pair? fsig)
								(let ((loc (- (length p) (length m))))
								  (let ((chk (catch #t (lambda () (fsig loc)) (lambda args #f))))
								    (if (and (symbol? chk) ; it defaults to #t
									     (not (memq chk '(integer:any? integer:real?))))
									(set-car! sig chk))))))))))))
					  arglist))
				       (and (any? (lambda (a) (not (eq? a #t))) sig)
					    (reverse sig)))))
		   
		   'side-effect (or (not (memq ftype '(define define* lambda lambda*)))
				    (any? (lambda (f) 
					    (side-effect? f env))
					  (cddr value)))
		   'allow-other-keys (and (pair? arglist)
					  (memq ftype '(define* define-macro* define-bacro* defmacro*))
					  (eq? (last-par arglist) 'allow-other-keys))
		   'env env
		   'value value
		   'decl decl
		   'arglist arglist
		   'ftype ftype
		   'set #f 
		   'ref #f)))
    
    (define (return-type sym e)
      (let ((sig (arg-signature sym e)))
	(and (pair? sig)
	     (or (eq? (car sig) 'values) ; turn it into #t for now
		 (car sig)))))           ; this might be undefined in the current context (eg oscil? outside clm)
    
    (define (any-macro? f env)
      (let ((fd (or (var-member f env)
		    (hash-table-ref globals f))))
	(or (and (var? fd)
		 (memq (var-ftype fd) '(define-macro define-macro* define-expansion define-bacro define-bacro* defmacro defmacro*)))
	    (and (symbol? f)
		 (macro? (symbol->value f *e*))))))
    
    (define (->simple-type c)
      (cond ((pair? c)         'pair?)
	    ((integer? c)      'integer?)
	    ((rational? c)     'rational?)
	    ((real? c)	       'real?)
	    ((number? c)       'number?)
	    ((keyword? c)      'keyword?)
	    ((symbol? c)       'symbol?)
	    ((byte-vector? c)  'byte-vector?)
	    ((string? c)       'string?)
	    ((null? c)	       'null?)
	    ((char? c)	       'char?)
	    ((boolean? c)      'boolean?)
	    ((float-vector? c) 'float-vector?)
	    ((int-vector? c)   'int-vector?)
	    ((vector? c)       'vector?)
	    ((let? c)	       'let?)
	    ((hash-table? c)   'hash-table?)
	    ((input-port? c)   'input-port?)
	    ((output-port? c)  'output-port?)
	    ((iterator? c)     'iterator?)
	    ((continuation? c) 'continuation?)
	    ((dilambda? c)     'dilambda?)
	    ((procedure? c)    'procedure?)
	    ((macro? c)        'macro?)
	    ((random-state? c) 'random-state?)
	    ((eof-object? c)   'eof-object?)
	    ((c-pointer? c)    'c-pointer?)
	    (#t #t)))
    
    (define (->type c)
      (if (pair? c)
	  (if (symbol? (car c))
	      (if (eq? (car c) 'quote)
		  (->simple-type (cadr c))   ; don't look for return type!
		  (return-type (car c) ()))
	      (or (pair? (car c)) 'pair?))
	  (->simple-type c)))
    
    (define bools '(symbol? integer? rational? real? number? complex? float? keyword? gensym? byte-vector? string? list?
			    char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair? proper-list?
			    output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?))
    
    (define (compatible? type1 type2) ; we want type1, we have type2 -- is type2 ok?
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
	    ((list?)             (memq type2 '(null? pair? proper-list?)))
	    ((proper-list?)      (memq type2 '(null? pair? list?)))
	    ((pair? null?)       (memq type2 '(list? proper-list?)))
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
	    ((proper-list?)     (eq? type2 'null?))
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
    
    (define lint-pp #f) ; avoid crosstalk with other schemes' definitions of pp and pretty-print (make-var also collides)
    (define lint-pretty-print #f)
    (let ()
      (require write.scm)
      (set! lint-pp pp);
      (set! lint-pretty-print pretty-print))
    
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
	  (set! ((funclet lint-pretty-print) '*pretty-print-left-margin*) N)
	  (set! ((funclet lint-pretty-print) '*pretty-print-length*) 110)
	  (set! str2 (lint-pp f2))
	  (set! len2 (length str2)))
	(if (< (+ len1 len2) 80)
	    (format #f "~A -> ~A" str1 str2)
	    (format #f "~%~NC~A ->~%~NC~A" N #\space str1 N #\space str2))))
    
    (define (truncated-lists->string f1 f2)
      ;; same but 2 strings that may need to be lined up vertically and both are truncated
      (let* ((str1 (object->string f1))
	     (len1 (length str1))
	     (str2 (object->string f2))
	     (len2 (length str2))
	     (N 4))
	(when (> len1 80)
	  (set! str1 (truncated-list->string f1))
	  (set! len1 (length str1)))
	(when (> len2 80)
	  (set! str2 (truncated-list->string f2))
	  (set! len2 (length str2)))
	(if (< (+ len1 len2) 80)
	    (format #f "~A -> ~A" str1 str2)
	    (format #f "~%~NC~A ->~%~NC~A" N #\space str1 N #\space str2))))
    
    (define made-suggestion 0)
    (define (lint-format str name . args)
      (let ((outstr (if (and (positive? line-number)
			     (< line-number 100000))
			(apply format #f (string-append " ~A (line ~D): " str "~%") name line-number args)
			(apply format #f (string-append " ~A: " str "~%") name args))))
	(set! made-suggestion (+ made-suggestion 1))
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
					       (or (and (pair? (car f))
							(any? (lambda (ff) (side-effect? ff e)) (car f)))
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
					;(format *stderr* "set-ref? ~A ~A -> ~A ~A~%" name env data (var? data))
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
    
    (define (check-star-parameters f args)
      (if (list-any? (lambda (k) (memq k '(:key :optional))) args)
	  (format outport " ~A: ~A is no longer accepted: ~A~%" f (if (memq :key args) :key :optional) args))
      (if (member 'pi args (lambda (a b) (or (eq? b 'pi) (and (pair? b) (eq? (car b) 'pi)))))
	  (format outport " ~A: parameter can't be a constant: ~A~%" f args))
      (let ((r (memq :rest args)))
	(when (pair? r)
	  (if (not (pair? (cdr r)))
	      (format outport " ~A: :rest parameter needs a name: ~A~%" f args)
	      (if (pair? (cadr r))
		  (format outport " ~A: :rest parameter can't specify a default value: ~A~%" f args)))))
      (let ((a (memq :allow-other-keys args)))
	(if (and (pair? a)
		 (pair? (cdr a)))
	    (format outport " ~A: :allow-other-keys should be at the end of the parameter list: ~A~%" f args))))
    
    (define (tree-member sym tree)
      (and (pair? tree)
	   (or (eq? (car tree) sym)
	       (tree-member sym (car tree))
	       (tree-member sym (cdr tree)))))
    
    (define (tree-list-member syms tree)
      (and (pair? tree)
	   (or (memq (car tree) syms)
	       (tree-list-member syms (car tree))
	       (tree-list-member syms (cdr tree)))))
    
    (define (tree-args tree)
      (let ((args ()))
	(define (tree-args-1 tree)
	  (if (symbol? tree)
	      (if (not (memq tree args))
		  (set! args (cons tree args)))
	      (when (and (pair? tree)
			 (not (eq? (car tree) 'quote)))
		(if (pair? (car tree))
		    (tree-args-1 (car tree)))
		(if (pair? (cdr tree))
		    (for-each tree-args-1 (cdr tree)))))) ; we don't care about dotted list members here
	(tree-args-1 tree)
	args))
    
    (define (tree-unquoted-member sym tree)
      (and (pair? tree)
	   (not (eq? (car tree) 'quote))
	   (or (eq? (car tree) sym)
	       (tree-unquoted-member sym (car tree))
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
		      (tree-symbol-walk (cddr tree) (begin (list-set! syms 1 (cons (cadadr tree) (cadr syms))) syms)))
		  (begin
		    (tree-symbol-walk (car tree) syms)
		    (tree-symbol-walk (cdr tree) syms))))))
    
    (define (remove item sequence)
      (cond ((null? sequence) ())
	    ((equal? item (car sequence)) (cdr sequence))
	    (else (cons (car sequence) (remove item (cdr sequence))))))
    
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
    
    (define (lint-remove-duplicates lst env)
      (letrec ((rem-dup
		(lambda (lst nlst)
		  (cond ((null? lst) nlst)
			((and (member (car lst) nlst)
			      (or (not (pair? (car lst)))
				  (not (side-effect? (car lst) env))))
			 (rem-dup (cdr lst) nlst))
			(else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
	(reverse (rem-dup lst ()))))
    
    (define (checked-eval form)
      (and (not (infinite? (length form)))
	   (catch #t
	     (lambda ()
	       (eval (copy form :readable)))
	     (lambda args
	       #t))))   ; just ignore errors in this context
    
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
		(cond ((assoc lst associated-exprs) => cdr)
		      (else
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
	;(format *stderr* "true? ~A~%" e)
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
					(and (pair? (cdr b))
					     (pair? (cddr b))
					     (member (cadr a) (cdr b))
					     (case (car a)
					       ((complex? number?) (eq? (car b) '=))
					       ((real?)            (memq (car b) '(< > <= >=)))
					       (else #f)))))))
		     (and (memq (car e) '(null? pair? proper-list?))
			  (pair? (cadr e))
			  (eq? (caadr e) 'list)
			  (or (eq? (car e) 'proper-list?)
			      (if (eq? (car e) 'null?)
				  (null? (cdadr e))
				  (not (null? (cdadr e))))))
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
		     (and (memq (car e) '(null? pair?))
			  (pair? (cadr e))
			  (eq? (caadr e) 'list)
			  (if (eq? (car e) 'null?)
			      (not (null? (cdadr e)))
			      (null? (cdadr e))))
		     (and (pair? (cadr e))
			  (memq (car e) bools)
			  (memq (return-type (caadr e) env) bools)
			  (not (any-compatible? (car e) (return-type (caadr e) env))))
		     (and (eq? (car e) 'negative?)
			  (pair? (cadr e))
			  (memq (caadr e) '(abs magnitude denominator gcd lcm)))))))
      
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
	(and (symbol? type1)
	     (symbol? type2)
	     (memq type1 bools)
	     (memq type2 bools)       ; return #f if not (obviously) redundant, else return which of the two to keep
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
		   ((list?)            (and (memq type2 '(null? pair? proper-list?)) type2))
		   ((null?)            (and (memq type2 '(list? proper-list?)) type1))
		   ((pair?)            (and (eq? type2 'list?) type1))
		   ((proper-list?)     (and (eq? type2 'null?) type2))
		   ((string?)          (and (eq? type2 'byte-vector?) type2))
		   ((byte-vector?)     (and (eq? type2 'string?) type1))
		   (else #f)))))
      
      (define (classify e)
	;; do we already know that e is true or false?
	;;   some cases subsume others: if we know (integer? x) is true, (complex? x) is also true
	(cond ((true? e)  #t) ; the simple boolean is passed back which will either be dropped or will stop the outer expr build
	      ((false? e) #f) ; eval of a constant expression here is tricky -- for example, (sqrt 2) should not be turned into 1.414...
	      ((just-constants? e env)
	       (catch #t
		 (lambda ()
		   (let ((val (eval e)))
		     (if (boolean? val)
			 val
			 e)))
		 (lambda ignore e)))
	      (else e)))
      
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
		       
		       (cond ((boolean? val) 
			      (not val))
			     
			     ((or (code-constant? arg)
				  (and (pair? arg)
				       (symbol? (car arg))
				       (hash-table-ref no-side-effect-functions (car arg))
				       (not (hash-table-ref globals (car arg)))
				       (let ((ret (return-type (car arg) env)))
					 (and (or (symbol? ret) (pair? ret))
					      (not (return-type-ok? 'boolean? ret))))
				       (not (var-member (car arg) env))))
			      #f)
			     
			     ((and (pair? arg)               ; (not (not ...)) -> ...
				   (pair? (cdr arg))
				   (eq? (car arg) 'not))
			      (cadr arg))
			     
			     ((and (pair? arg)               ; (not (or|and (not x) (not y)...)) -> (and|or x y ...)
				   (memq (car arg) '(and or))
				   (pair? (cdr arg))
				   (every? (lambda (p)
					     (and (pair? p)
						  (memq (car p) '(not < > <= >= odd? even? exact? inexact?
								  char<? char>? char<=? char>=? 
								  string<? string>? string<=? string>=?
								  char-ci<? char-ci>? char-ci<=? char-ci>=? 
								  string-ci<? string-ci>? string-ci<=? string-ci>=?))))
					   (cdr arg))
				   (any? (lambda (p) (eq? (car p) 'not)) (cdr arg)))
			      `(,(if (eq? (car arg) 'or) 'and 'or)
				,@(map (lambda (p)
					 (if (eq? (car p) 'not)
					     (cadr p)
					     (simplify-boolean `(not ,p) () () env)))
				       (cdr arg))))
			     
			     ((not (equal? val arg))
			      `(not ,val))
			     
			     ((and (pair? arg)
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
				;;    this is not quite right because (not (null? 3)) -> #t
				;; char-upper-case? and lower are not switchable here
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
				
				(else form)))
			     (else form)))
		     form))
		
		((or)
		 ;(format *stderr* "or: ~A~%" form)
		 (case len
		   ((1) #f)
		   ((2) (if (code-constant? (cadr form)) (cadr form) (classify (cadr form))))
		   (else
		    (let ((arg1 (cadr form)))
		      (or (true? arg1)        ; no need to check anything else, side-effect? here is a nightmare
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
				  (if (and (= len 3)
					   (pair? arg1)
					   (pair? arg2)
					   (or (and (eq? (car arg2) '=)
						    (memq (car arg1) '(< > <= >=)))
					       (and (eq? (car arg1) '=)
						    (memq (car arg2) '(< > <= >=))))
					   (= (length arg1) 3)
					   (equal? (cdr arg1) (cdr arg2)))
				      `(,(if (or (memq (car arg1) '(< <=))
						 (memq (car arg2) '(< <=)))
					     '<= '>=)
					,@(cdr arg1))
				      
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
						(eqf #f)
						(vals ()))

					    (if (every? (lambda (p)
							  (and (pair? p)
							       (if (not eqf)
								   (and (memq (car p) '(eq? eqv? equal? char=? string=? = char-ci=? string-ci=?))
									(set! eqf (car p)))
								   (or (eq? eqf (car p))
								       (and (memq eqf '(eq? eqv? equal? =)) 
									    (memq (car p) '(eq? eqv? equal? =))
									    (set! eqf (if (not (eq? (car p) 'eq?)) 'equal? eqf)))
								       (and (memq eqf '(char=? char-ci=?)) 
									    (memq (car p) '(char=? char-ci=?)))))
							       (or (and (code-constant? (caddr p))
									(set! vals (cons (caddr p) vals))
									(if (not sym) 
									    (and (not (side-effect? (cadr p) env))
										 (set! sym (cadr p)))
									    (equal? sym (cadr p))))
								   (and (code-constant? (cadr p))
									(set! vals (cons (cadr p) vals))
									(if (not sym) 
									    (and (not (side-effect? (caddr p) env))
										 (set! sym (caddr p)))
									    (equal? sym (caddr p)))))))
							(cdr form))
						(let* ((func (case eqf 
							       ((eq?) 'memq) 
							       ((eqv? char=?) 'memv) 
							       ((=) (if (every? rational? vals) 'memv 'member))
							       (else 'member)))
						       (equals (if (and (eq? func 'member)
									(not (eq? eqf 'equal?)))
								   (list eqf)
								   ()))
						       (elements (map (lambda (v) (if (pair? v) (cadr v) v)) vals)))

						  ;; (or (eq? 'quote (car expr)) (eq? 'QUOTE (car expr))) -> (memq (car expr) ''QUOTE)
						  ;; TODO: if '(quote ...) don't double it -- where did that happen?
						  
						  (cond ((and (eq? eqf 'char=?)
							      (= (length elements) 2)
							      (char-ci=? (car elements) (cadr elements)))
							 `(char-ci=? ,sym ,(car elements)))

							((and (eq? eqf 'string=?)
							      (= (length elements) 2)
							      (string-ci=? (car elements) (cadr elements)))
							 `(string-ci=? ,sym ,(car elements)))

							((member elements '((#t #f) (#f #t)))
							 `(boolean? ,sym))		; zero? doesn't happen

							(else 
							 `(,func ,sym ',(reverse elements) ,@equals))))
						
						;; not every? above
						(let ((new-form ()))
						  ;; (or (and A B) (and (not A) (not B))) -> (eq? (not A) (not B))
						  ;; more accurately (if A B (not B)), but every case I've seen is just boolean
						  ;; perhaps also (or (not (or A B)) (not (or (not A) (not B)))), but it never happens
						  (if (and (= len 3) 
							   (let ((a1 (cadr form))
								 (a2 (caddr form)))
							     (and (pair? a1)
								  (pair? a2)
								  (eq? (car a1) 'and)
								  (eq? (car a2) 'and)
								  (= (length a1) 3)
								  (= (length a2) 3)
								  (let ((A (if (and (pair? (cadr a1)) (eq? (caadr a1) 'not)) (cadr (cadr a1)) (cadr a1)))
									(B (if (and (pair? (caddr a1)) (eq? (caaddr a1) 'not)) (cadr (caddr a1)) (caddr a1))))
								    (or (and (or (equal? form `(or (and ,A ,B) (and (not ,A) (not ,B))))
										 (equal? form `(or (and (not ,A) (not ,B)) (and ,A ,B))))
									     (set! new-form `(eq? (not ,A) (not ,B))))
									(and (or (equal? form `(or (and ,A (not ,B)) (and (not ,A) ,B)))
										 (equal? form `(or (and (not ,A) ,B) (and ,A (not ,B)))))
									     (set! new-form `(not (eq? (not ,A) (not ,B))))))))))
						      new-form
						      
						      (do ((exprs (cdr form) (cdr exprs)))
							  ((null? exprs) 
							   ;(format *stderr* "new-form: ~A~%" new-form)
							   (and (pair? new-form)
								(if (null? (cdr new-form))
								    (car new-form)
								    `(or ,@(reverse new-form)))))
							(let* ((e (car exprs))
							       (val (classify e)))
							  ;(format *stderr* "e: ~A, val: ~A~%" e val)
							  (if (and (pair? val)
								   (memq (car val) '(and or not)))
							      (set! val (classify (set! e (simplify-boolean e true false env)))))
							  
							  (if val                                ; #f in or is ignored
							      (if (or (eq? val #t)               ; #t or any non-#f constant in or ends the expression
								      (code-constant? val))
								  (begin
								    (set! new-form (if (null? new-form)         ; (or x1 123) -> value of x1 first
										       (list val)
										       (cons val new-form)))
								    ;; reversed when returned
								    (set! exprs '(#t)))
								  
								  ;; (or x1 x2 x1) -> (or x1 x2) is ok because if we get to x2, x1 is #f, 
								  ;;   so trailing x1 would still be #f
								  
								  (if (and (pair? e)             ; (or ...) -> splice into current
									   (eq? (car e) 'or))
								      (set! exprs (append e (cdr exprs))) ; we'll skip the 'or in do step
								      (begin                     ; else add it to our new expression with value #f
									(store e val 'or)
									(if (not (memq val new-form))
									    (set! new-form (cons val new-form)))))))))))))))))))))))
		
		((and)
		 (case len
		   ((1) #t)
		   ((2) (classify (cadr form)))
		   (else
		    (let ((arg1 (cadr form)))
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
				       (if (and (= len 3)
						(pair? arg1)
						(pair? arg2)
						(reversible? (car arg1))
						(null? (cdddr arg1))
						(pair? (cdr arg2))
						(pair? (cddr arg2))
						(null? (cdddr arg2))
						(not (side-effect? arg2 env))          ; arg1 is hit in any case
						(or (eq? (car arg1) (car arg2))        ; either ops are equal or
						    (let ((rf (reversed (car arg2))))  ;    try reversed op for arg2
						      (and (eq? (car arg1) rf)
							   (set! arg2 (cons rf (reverse (cdr arg2)))))))
						(or (equal? (caddr arg1) (cadr arg2))     ; (and (op x y) (op y z))
						    (equal? (cadr arg1) (caddr arg2))     ; (and (op x y) (op z x))
						    (and (memq (car arg1) '(= char=? string=? char-ci=? string-ci=?))
							 (or (equal? (cadr arg1) (cadr arg2))
							     (equal? (caddr arg1) (caddr arg2))))))
					   (let ((op1 (car arg1))
						 (arg1-1 (cadr arg1))
						 (arg1-2 (caddr arg1))
						 (arg2-1 (cadr arg2))
						 (arg2-2 (caddr arg2)))
					     
					     (cond ((equal? arg1-2 arg2-1)       ; (and (op x y) (op y z)) -> (op x y z)
						    (if (equal? arg1-1 arg2-2)
							(and (memq op1 '(= char=? string=? char-ci=? string-ci=?))
							     arg1)
							(and (or (not (code-constant? arg1-1))
								 (not (code-constant? arg2-2))
								 ((symbol->value op1) arg1-1 arg2-2))
							     `(,op1 ,arg1-1 ,arg2-1 ,arg2-2))))
						   
						   ((equal? arg1-1 arg2-2)       ; (and (op x y) (op z x)) -> (op z x y)
						    (if (equal? arg1-2 arg2-1)
							(and (memq op1 '(= char=? string=? char-ci=? string-ci=?))
							     arg1)
							(and (or (not (code-constant? arg2-1))
								 (not (code-constant? arg1-2))
								 ((symbol->value op1) arg2-1 arg1-2))
							     `(,op1 ,arg2-1 ,arg1-1 ,arg1-2))))
						   
						   ;; here we're restricted to equalities and we know arg1 != arg2
						   ((equal? arg1-1 arg2-1)        ; (and (op x y) (op x z)) -> (op x y z)
						    (if (and (code-constant? arg1-2)
							     (code-constant? arg2-2))
							(and ((symbol->value op1) arg1-2 arg2-2)
							     arg1)
							`(,op1 ,arg1-1 ,arg1-2 ,arg2-2)))
						   
						   (else ; equalities again
						    (if (and (code-constant? arg1-1)
							     (code-constant? arg2-1))
							(and ((symbol->value op1) arg1-1 arg2-1)
							     arg1)
							`(,op1 ,arg1-1 ,arg1-2 ,arg2-1)))))
					   
					   (do ((exprs (cdr form) (cdr exprs)))
					       ((null? exprs) 
						(or (null? new-form)      ; (and) -> #t
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
					       
					       (cond ((eq? val #t)
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
							      (set! new-form (cons e new-form)))))
						     
						     ((not val)             ; #f in 'and' ends the expression
						      (if (or (null? new-form)   
							      (just-symbols? new-form))
							  (set! new-form '(#f))
							  (set! new-form (cons #f new-form))) ;was (append '(#f) new-form)))
						      (set! exprs '(#f)))
						     
						     ((and (pair? e)       ; if (and ...) splice into current
							   (eq? (car e) 'and))
						      (set! exprs (append e (cdr exprs))))
						     (else (unless (and (pair? e)                   ; (and ... (or ... 123) ...) -> splice out or
									(pair? (cdr exprs))
									(eq? (car e) 'or)
									(pair? (cdr e))
									(pair? (cddr e))
									(cond ((list-ref e (- (length e) 1)) => code-constant?) ; (or ... #f)
									      (else #f))) 
							     (store e val 'and)    ; else add it to our new expression with value #t
							     (if (or (not (pair? new-form))
								     (not (eq? val (car new-form))))
								 (set! new-form (cons val new-form)))))))))))))))))))))))
    
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
      (let ((real-result? (lambda (op) (memq op '(imag-part real-part abs magnitude angle max min exact->inexact inexact
						  modulo remainder quotient lcm gcd))))
	    (rational-result? (lambda (op) (memq op '(rationalize inexact->exact exact))))
	    (integer-result? (lambda (op) (memq op '(logior lognot logxor logand numerator denominator 
						     floor round truncate ceiling ash)))))
	
	(define (inverse-op op)
	  (case op 
	    ((sin) 'asin) ((cos) 'acos) ((tan) 'atan) ((asin) 'sin) ((acos) 'cos) ((atan) 'tan)
	    ((sinh) 'asinh) ((cosh) 'acosh) ((tanh) 'atanh) ((asinh) 'sinh) ((acosh) 'cosh) ((atanh) 'tanh)
	    ((log) exp) ((exp) log)))
	
	
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
			(if (and (pair? rats)
				 (pair? (cdr rats)))
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
			(if (and (pair? rats)
				 (pair? (cdr rats)))
			    (let ((y (apply * rats)))
			      (if (= y 1)
				  (set! val (collect-if list (lambda (x) (not (number? x))) val))
				  (set! val (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		  (case (length val)
		    ((0) 1)
		    ((1) (car val))                         ; (* x) -> x
		    ((2)
		     (cond ((just-rationals? val)
			    (let ((new-val (apply * val))) ; huge numbers here are less readable
			      (if (< (abs new-val) 1000000)
				  new-val
				  `(* ,@val))))
			   ((memv 0 val)                    ; (* x 0) -> 0
			    0) 
			   ((memv -1 val)
			    `(- ,@(remove -1 val)))         ; (* -1 x) -> (- x)
			   ((and (pair? (car val))
				 (pair? (cadr val)))
			    (if (and (eq? (caar val) '-)    ; (* (- x) (- y)) -> (* x y)
				     (null? (cddar val))
				     (eq? (caadr val) '-)
				     (null? (cddadr val)))
				`(* ,(cadar val) ,(cadadr val))
				(if (and (= (length (car val)) 3)
					 (equal? (cdar val) (cdadr val))
					 (or (and (eq? (caar val) 'gcd) (eq? (caadr val) 'lcm))
					     (and (eq? (caar val) 'lcm) (eq? (caadr val) 'gcd))))
				    `(abs (* ,@(cdar val))) ; (* (gcd a b) (lcm a b)) -> (abs (* a b)) but only if 2 args?
				    `(* ,@val))))
			   ((and (pair? (car val))          ; (* (inexact->exact x) 2.0) -> (* x 2.0)
				 (memq (caar val) '(exact->inexact inexact))
				 (number? (cadr val))
				 (not (rational? (cadr val))))
			    `(* ,(cadar val) ,(cadr val)))
			   ((and (pair? (cadr val))         ; (* 2.0 (inexact x)) -> (* 2.0 x)
				 (memq (caadr val) '(exact->inexact inexact))
				 (number? (car val))
				 (not (rational? (car val))))
			    `(* ,(car val) ,(cadadr val)))
			   
			   (else `(* ,@val))))
		    (else 
		     (cond ((just-rationals? val)
			    (let ((new-val (apply * val))) ; huge numbers here are less readable
			      (if (< (abs new-val) 1000000)
				  new-val
				  `(* ,@val))))
			   ((memv 0 val)                    ; (* x 0 2) -> 0
			    0) 
			   (else `(* ,@val)))))))))
	    
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
				   (cadar args)                      ; (- (- x)) -> x
				   `(- ,@args)))
			  ((3) (if (eq? (caar args) '-)
				   `(- ,(caddar args) ,(cadar args)) ; (- (- x y)) -> (- y x)
				   `(- ,@args)))
			  (else `(- ,@args))))))
	       ((2) 
		(cond ((just-rationals? args) (apply - args))     ; (- 3 2) -> 1
		      ((eqv? (car args) 0)    `(- ,(cadr args)))  ; (- 0 x) -> (- x)
		      ((eqv? (cadr args) 0)   (car args))         ; (- x 0) -> x
		      ((equal? (car args) (cadr args)) 0)         ; (- x x) -> 0
		      ((and (pair? (car args))                    ; (- (- x y) z) -> (- x y z) but leave (- (- x) ...)
			    (eq? (caar args) '-)
			    (> (length (car args)) 2))
		       `(- ,@(cdar args) ,(cadr args)))
		      (else `(- ,@args))))
	       (else 
		(let ((val (remove-all 0 (splice-if (lambda (x) (eq? x '+)) (cdr args)))))
		  (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
		      (let ((rats (collect-if list rational? val)))
			(if (and (pair? rats) 
				 (pair? (cdr rats)))
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
		    (if (and (pair? (car args))
			     (= (length (car args)) 2)
			     (eq? (caar args) '/))
			(cadar args)
			`(/ ,@args))))
	       ((2)
		(if (and (just-rationals? args)
			 (not (zero? (cadr args))))
		    (apply / args)                         ; including (/ 0 12) -> 0
		    (let ((arg1 (car args))
			  (arg2 (cadr args)))
		      (cond ((eqv? arg1 1)                 ; (/ 1 x) -> (/ x)
			     `(/ ,arg2))
			    ((eqv? arg2 1)                 ; (/ x 1) -> x
			     arg1)
			    ((and (pair? arg1)             ; (/ (/ a b) c) -> (/ a b c)
				  (eq? (car arg1) '/)
				  (pair? (cddr arg1))
				  (or (not (pair? arg2))
				      (not (eq? (car arg2) '/))))
			     `(/ ,(cadr arg1) ,@(cddr arg1) ,arg2))
			    ((and (pair? arg1)             ; (/ (log x) (log y)) -> (log x y)
				  (= (length arg1) 2)
				  (pair? arg2)
				  (= (length arg2) 2)
				  (eq? (car arg1) 'log)
				  (eq? (car arg2) 'log))
			     `(log ,(cadr arg1) ,(cadr arg2)))
			    ((and (pair? arg1)             ; (/ (/ a) (/ b)) -> (/ b a)??
				  (eq? (car arg1) '/)
				  (pair? arg2)
				  (eq? '/ (car arg2)))
			     (let ((a1 (if (null? (cddr arg1)) `(/ 1 ,(cadr arg1)) arg1))
				   (a2 (if (null? (cddr arg2)) `(/ 1 ,(cadr arg2)) arg2)))
			       (simplify-numerics `(/ (* ,(cadr a1) ,@(cddr a2)) (* ,@(cddr a1) ,(cadr a2))) env)))
			    ((and (pair? arg2)
				  (eq? (car arg2) '*)
				  (not (side-effect? arg1 env))
				  (member arg1 (cdr arg2)))
			     (let ((n (remove arg1 (cdr arg2))))
			       (if (= (length n) 1)
				   `(/ ,@n)               ; (/ x (* y x)) -> (/ y)
				   `(/ 1 ,@n))))          ; (/ x (* y x z)) -> (/ 1 y z)
			    ((and (pair? arg1)             ; (/ (inexact x) 2.0) -> (/ x 2.0)
				  (memq (car arg1) '(exact->inexact inexact))
				  (number? arg2)
				  (not (rational? arg2)))
			     `(/ ,(cadr arg1) ,arg2))
			    ((and (pair? arg2)            ; (/ 2.0 (inexact x)) -> (/ 2.0 x)
				  (memq (car arg2) '(exact->inexact inexact))
				  (number? arg1)
				  (not (rational? arg1)))
			     `(/ ,arg1 ,(cadr arg2)))
			    (else `(/ ,@args))))))
	       
	       (else 
		(if (and (just-rationals? args)
			 (not (memv 0 (cdr args)))
			 (not (memv 0.0 (cdr args))))
		    (apply / args)
		    (let ((nargs                      ; (/ x a (* b 1 c) d) -> (/ x a b c d) but not short cases
			   (remove-all 1 (splice-if (lambda (x) (eq? x '*)) (cdr args)))))
		      (if (null? nargs) ; (/ x 1 1) -> x
			  (car args)
			  (if (and (member (car args) (cdr args))
				   (not (side-effect? (car args) env)))
			      (let ((n (remove (car args) (cdr args))))
				(if (= (length n) 1)
				    `(/ ,@n)                    ; (/ x y x) -> (/ y)
				    `(/ 1 ,@n)))                ; (/ x y x z) -> (/ 1 y z)
			      `(/ ,@(cons (car args) nargs)))))))))
	    
	    ((sin cos tan asin acos sinh cosh tanh asinh acosh atanh exp)
	     (if (= len 1)
		 (cond ((and (pair? (car args))                 ; (sin (asin x)) -> x
			     (= (length (car args)) 2)
			     (eq? (caar args) (inverse-op (car form))))
			(cadar args))
		       ((eqv? (car args) 0)                     ; (sin 0) -> 0
			(case (car form)
			  ((sin asin sinh asinh tan tanh atanh) 0)
			  ((exp cos cosh) 1)
			  (else `(,(car form) ,@args))))
		       ((and (eq? (car form) 'cos)              ; (cos (- x)) -> (cos x)
			     (pair? (car args))
			     (eq? (caar args) '-)
			     (null? (cddar args)))
			`(cos ,(cadar args)))
		       ((eq? (car args) 'pi)                    ; (sin pi) -> 0.0
			(case (car form)
			  ((sin) 0.0)
			  ((cos) 1.0)
			  (else `(,(car form) ,@args))))
		       ((eqv? (car args) 0.0)                  ; (sin 0.0) -> 0.0
			((symbol->value (car form)) 0.0))
		       ((and (eq? (car form) 'exp)             ; (exp (* a (log b))) -> (expt b a)
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
			      `(,(car form) ,@args))))
		       (else `(,(car form) ,@args)))
		 `(,(car form) ,@args)))
	    
	    ((log)
	     (if (pair? args)
		 (cond ((eqv? (car args) 1) 0)    ; (log 1 ...) -> 0
		       ((and (= len 1)            ; (log (exp x)) -> x
			     (pair? (car args))
			     (= (length (car args)) 2)
			     (eq? (caar args) 'exp))
			(cadar args))
		       ((and (= len 2)            ; (log x x) -> 1.0
			     (equal? (car args) (cadr args)))
			(if (integer? (car args)) 1 1.0))
		       (else `(log ,@args)))
		 form))
	    
	    ((sqrt)
	     (if (pair? args)
		 (if (and (rational? (car args))
			  (rational? (sqrt (car args)))
			  (= (car args) (* (sqrt (car args)) (sqrt (car args)))))
		     (sqrt (car args)) ; don't collapse (sqrt (* a a)), a=-1 for example
		     `(sqrt ,@args))
		 form))
	    
	    ((floor round ceiling truncate)
	     (if (= len 1)
		 (cond ((number? (car args))
			(catch #t 
			  (lambda () (apply (symbol->value (car form)) args)) 
			  (lambda any `(,(car form) ,@args))))
		       ((pair? (car args))
			(cond ((or (integer-result? (caar args))
				   (and (eq? (caar args) 'random)
					(integer? (cadar args))))
			       (car args))
			      
			      ((memq (caar args) '(inexact->exact exact))
			       `(,(car form) ,(cadar args)))
			      
			      ((memq (caar args) '(* + / -)) ; maybe extend this list
			       `(,(car form) (,(caar args) ,@(map (lambda (p)
								    (if (and (pair? p)
									     (memq (car p) '(inexact->exact exact)))
									(cadr p)
									p))
								  (cdar args)))))
			      (else `(,(car form) ,@args))))
		       (else `(,(car form) ,@args)))
		 form))
	    
	    ((abs magnitude)
	     (if (= len 1)
		 (cond ((and (pair? (car args))        ; (abs (abs x)) -> (abs x)
			     (memq (caar args) '(abs magnitude denominator gcd lcm)))
			(car args))
		       ((rational? (car args))
			(abs (car args)))
		       ((and (pair? (car args))        ; (abs (modulo x 2)) -> (modulo x 2)
			     (eq? (caar args) 'modulo)
			     (= (length (car args)) 3)
			     (number? (caddar args))
			     (positive? (caddar args)))
			(car args))
		       ((and (pair? (car args))        ; (abs (- x)) -> (abs x)
			     (eq? (caar args) '-)
			     (pair? (cdar args))
			     (null? (cddar args)))
			`(,(car form) ,(cadar args)))
		       (else `(,(car form) ,@args)))
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
		 (cond ((and (eq? (car form) 'ash)          ; (ash x 0) -> x
			     (= len 2) ; length of args
			     (eqv? (cadr args) 0))
			(car args))
		       ((or (and (eq? (car form) 'quotient) ; (quotient (remainder x y) y) -> 0
				 (= len 2)
				 (pair? (car args))
				 (eq? (caar args) 'remainder)
				 (= (length (car args)) 3)
				 (eqv? (caddar args) (cadr args)))
			    (and (memq (car form) '(ash modulo))
				 (= len 2)                  ; (modulo 0 x) -> 0
				 (eqv? (car args) 0)))
			0)
		       ((and (eq? (car form) 'modulo)       ; (modulo (abs x) y) -> (modulo x y)
			     (= len 2)
			     (pair? (car args))
			     (eq? (caar args) 'abs))
			`(modulo ,(cadar args) ,(cadr args)))
		       (else `(,(car form) ,@args)))))
	    
	    ((expt) 
	     (if (= len 2)
		 (cond ((and (eqv? (car args) 0)            ; (expt 0 x) -> 0
			     (not (eqv? (cadr args) 0)))
			0)
		       ((or (and (eqv? (cadr args) 0)       ; (expt x 0) -> 1
				 (not (eqv? (car args) 0)))
			    (eqv? (car args) 1))            ; (expt 1 x) -> 1    
			1)
		       ((eqv? (cadr args) 1)                ; (expt x 1) -> x
			(car args))
		       ((eqv? (cadr args) -1)               ; (expt x -1) -> (/ x)
			`(/ ,(car args)))
		       ((just-rationals? args)              ; (expt 2 3) -> 8
			(catch #t
			  (lambda ()
			    (let ((val (apply expt args)))
			      (if (and (integer? val)
				       (< (abs val) 1000000))
				  val
				  `(expt ,@args))))
			  (lambda args
			    `(expt ,@args))))
		       (else `(expt ,@args)))
		 form))
	    
	    ((angle)
	     (if (pair? args)
		 (cond ((eqv? (car args) -1) 'pi)
		       ((or (morally-equal? (car args) 0.0)
			    (eq? (car args) 'pi))
			0.0)
		       (else `(angle ,@args)))
		 form))
	    
	    ((atan)
	     (if (and (= len 1)
		      (pair? (car args))
		      (= (length (car args)) 3)
		      (eq? (caar args) '/))
		 `(atan ,@(cdar args))
		 `(atan ,@args)))
	    
	    ((inexact->exact exact)
	     (if (= len 1)
		 (cond ((or (rational? (car args))
			    (and (pair? (car args))
				 (or (rational-result? (caar args))
				     (integer-result? (caar args))
				     (and (eq? (caar args) 'random)
					  (rational? (cadar args))))))
			(car args))
		       ((number? (car args))
			(catch #t (lambda () (inexact->exact (car args))) (lambda any `(,(car form) ,@args))))
		       (else `(,(car form) ,@args)))
		 form))
	    
	    ((exact->inexact inexact)
	     (if (= len 1)
		 (if (memv (car args) '(0 0.0))
		     0.0
		     `(,(car form) ,@args))
		 form))
	    
	    ((logior)
	     (set! args (lint-remove-duplicates (remove-all 0 (splice-if (lambda (x) (eq? x 'logior)) args)) env))
	     (if (every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		 (let ((rats (collect-if list integer? args)))
		   (if (and (pair? rats) 
			    (pair? (cdr rats)))
		       (let ((y (apply logior rats)))
			 (if (zero? y)
			     (set! args (collect-if list (lambda (x) (not (number? x))) args))
			     (set! args (cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
	     (cond ((null? args) 0)                ; (logior) -> 0
		   ((null? (cdr args)) (car args)) ; (logior x) -> x
		   ((memv -1 args) -1)             ; (logior ... -1 ...) -> -1
		   ((just-integers? args) (apply logior args))
		   (else `(logior ,@args))))
	    
	    ((logand)
	     (set! args (lint-remove-duplicates (remove-all -1 (splice-if (lambda (x) (eq? x 'logand)) args)) env))
	     (if (every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		 (let ((rats (collect-if list integer? args)))
		   (if (and (pair? rats) 
			    (pair? (cdr rats)))
		       (let ((y (apply logand rats)))
			 (if (= y -1)
			     (set! args (collect-if list (lambda (x) (not (number? x))) args))
			     (set! args (cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
	     (cond ((null? args) -1)
		   ((null? (cdr args)) (car args)) ; (logand x) -> x
		   ((memv 0 args) 0)
		   ((just-integers? args) (apply logand args))
		   (else `(logand ,@args))))
	    
	    ;; (logand 1 (logior 2 x)) -> (logand 1 x)? 
	    ;; (logand 1 (logior 1 x)) -> 1
	    ;; (logand 3 (logior 1 x))?
	    ;; similarly for (logior...(logand...))
	    
	    ((logxor)
	     (set! args (splice-if (lambda (x) (eq? x 'logxor)) args)) ; is this correct??
	     (cond ((null? args) 0)                                    ; (logxor) -> 0
		   ((null? (cdr args)) (car args))                     ; (logxor x) -> x??
		   ((just-integers? args) (apply logxor args))         ; (logxor 1 2) -> 3
		   ((and (= len 2) (equal? (car args) (cadr args))) 0) ; (logxor x x) -> 0
		   (else `(logxor ,@args))))                           ; (logxor x (logxor y z)) -> (logxor x y z)
	    
	    ((gcd)
	     (set! args (lint-remove-duplicates (splice-if (lambda (x) (eq? x 'gcd)) args) env))
	     (cond ((null? args) 0) 
		   ((memv 1 args) 1)
		   ((just-integers? args)
		    (catch #t  ; maybe (gcd -9223372036854775808 -9223372036854775808)
		      (lambda ()
			(apply gcd args))
		      (lambda ignore
			`(gcd ,@args))))
		   ((null? (cdr args))   `(abs ,(car args)))
		   ((eqv? (car args) 0)  `(abs ,(cadr args)))
		   ((eqv? (cadr args) 0) `(abs ,(car args)))
		   (else `(gcd ,@args))))
	    
	    ((lcm)
	     (set! args (lint-remove-duplicates (splice-if (lambda (x) (eq? x 'lcm)) args) env))
	     (cond ((null? args) 1)         ; (lcm) -> 1
		   ((memv 0 args) 0)        ; (lcm ... 0 ...) -> 0
		   ((just-integers? args)   ; (lcm 3 4) -> 12
		    (catch #t
		      (lambda ()
			(apply lcm args))
		      (lambda ignore
			`(lcm ,@args))))
		   ((null? (cdr args))      ; (lcm x) -> (abs x)
		    `(abs ,(car args)))
		   (else `(lcm ,@args))))
	    
	    ((max min)
	     (if (pair? args)
		 (begin
		   (set! args (lint-remove-duplicates (splice-if (lambda (x) (eq? x (car form))) args) env))
		   (if (= len 1)
		       (car args)
		       (if (just-reals? args)
			   (apply (symbol->value (car form)) args)
			   (let ((nums (collect-if list number? args))
				 (other (if (eq? (car form) 'min) 'max 'min)))
			     (if (and (pair? nums)
				      (just-reals? nums)) ; non-real case checked elsewhere (later)
				 (let ((relop (if (eq? (car form) 'min) >= <=)))
				   (if (pair? (cdr nums))
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
	((symbol? keyword? boolean? null?)'(eq? eq?))
	((string? byte-vector?) '(equal? string=?))
	((pair? vector? float-vector? int-vector? hash-table?) '(equal? equal?))
	(else '(#t #t))))
    
    (define (eqf selector)
      (cond ((symbol? selector)      '(#t #t))
	    ((not (pair? selector))  (->eqf (->type selector)))
	    ((eq? (car selector) 'quote)
	     (cond ((or (symbol? (cadr selector))
			(null? (cadr selector))
			(memq (cadr selector) '(#f #t #<unspecified> #<undefined> #<eof> ())))
		    '(eq? eq?))
		   ((char? (cadr selector))   '(eqv? char=?))
		   ((string? (cadr selector)) '(equal? string=?))
		   ((number? (cadr selector)) '(eqv? =))
		   (else                      '(equal? equal?))))
	    ((and (eq? (car selector) 'list)
		  (null? (cdr selector)))
	     '(eq? eq?))
	    ((symbol? (car selector)) (->eqf (return-type (car selector) ())))
	    (else                     '(#t #t))))
    
    (define (unquoted x)
      (if (and (pair? x)
	       (eq? (car x) 'quote))
	  (cadr x)
	  x))
    
    (define (distribute-quote x)
      (map (lambda (item)
	     (if (or (symbol? item)
		     (pair? item))
		 `(quote ,item)
		 item))
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
    
    (define (write-port expr) ; ()=not specified (*stdout*), #f=something is wrong (not enough args)
      (and (pair? expr)
	   (if (eq? (car expr) 'newline)
	       (if (pair? (cdr expr))
		   (cadr expr)
		   ())
	       (and (pair? (cdr expr))
		    (if (pair? (cddr expr))
			(caddr expr)
			())))))
    
    (define (display->format d)
      (case (car d)
	((newline) "~%")
	
	((display) 
	 (let ((arg (cadr d)))
	   (cond ((string? arg)
		  arg)
		 ((char? arg)
		  (string arg))
		 ((and (pair? arg)
		       (eq? (car arg) 'number->string)
		       (= (length arg) 3))
		  (case (caddr arg)
		    ((2) (values "~B" (cadr arg)))
		    ((8) (values "~O" (cadr arg)))
		    ((10) (values "~D" (cadr arg)))
		    ((16) (values "~X" (cadr arg)))
		    (else (values "~A" arg))))
		 (else (values "~A" arg)))))
	
	((write)
	 ;; very few special cases actually happen here, unlike display above
	 (if (string? (cadr d))
	     (string-append "\"" (cadr d) "\"")
	     (if (char? (cadr d))
		 (string (cadr d))
		 (values "~S" (cadr d)))))
	
	((write-char)
	 (if (char? (cadr d))
	     (string (cadr d))
	     (values "~C" (cadr d))))
	
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
		       (apply substring (cadr d) indices)
		       (values "~A" `(substring ,(cadr d) ,@indices)))
		   (cadr d))
	       (values "~A" (if indices `(substring ,(cadr d) ,@indices) (cadr d))))))))
    
    (define (identity? x) ; (lambda (x) x), or (define (x) x) -> procedure-source
      (and (pair? x)
	   (eq? (car x) 'lambda)
	   (pair? (cdr x))
	   (pair? (cadr x))
	   (null? (cdadr x))
	   (pair? (cddr x))
	   (null? (cdddr x))
	   (eq? (caddr x) (caadr x))))
    
    (define (cdr-count c)
      (case c ((cdr) 1) ((cddr) 2) ((cdddr) 3) (else 4)))
    
    (define (simple-lambda? x)
      (and (pair? x)
	   (eq? (car x) 'lambda)
	   (pair? (cdr x))
	   (pair? (cadr x))
	   (null? (cdadr x))
	   (pair? (cddr x))
	   (null? (cdddr x))
	   (= (tree-count1 (caadr x) (caddr x) 0) 1)))
    
    (define (less-simple-lambda? x)
      (and (pair? x)
	   (eq? (car x) 'lambda)
	   (pair? (cdr x))
	   (pair? (cadr x))
	   (null? (cdadr x))
	   (pair? (cddr x))
	   (= (tree-count1 (caadr x) (cddr x) 0) 1)))
    
    (define (tree-subst new old tree)
      (if (pair? tree)
	  (if (eq? (car tree) 'quote)
	      (copy-tree tree)
	      (cons (tree-subst new old (car tree))
		    (tree-subst new old (cdr tree))))
	  (if (equal? old tree)
	      new
	      tree)))
    
    (define* (find-unique-name f1 f2 (i 1))
      (let ((sym (string->symbol (format #f "_~D_" i))))
	(if (and (not (eq? sym f1))
		 (not (eq? sym f2))
		 (not (tree-member sym f1))
		 (not (tree-member sym f2)))
	    sym
	    (find-unique-name f1 f2 (+ i 1)))))
    
    (define (unrelop name head form)         ; assume len=3 
      (let ((arg1 (cadr form))
	    (arg2 (caddr form)))
	(if (memv arg2 '(0 0.0))             ; (< (- x y) 0) -> (< x y), need both 0 and 0.0 because (eqv? 0 0.0) is #f
	    (if (and (pair? arg1)
		     (eq? (car arg1) '-)
		     (= (length arg1) 3))
		(lint-format "perhaps ~A" name (lists->string form `(,head ,(cadr arg1) ,(caddr arg1)))))
	    (if (and (memv arg1 '(0 0.0))    ; (< 0 (- x y)) -> (> x y)
		     (pair? arg2)
		     (eq? (car arg2) '-)
		     (= (length arg2) 3))
		(lint-format "perhaps ~A" name (lists->string form `(,(reversed head) ,(cadr arg2) ,(caddr arg2))))))))
    
    (define (other-case c)
      (if (char-upper-case? c)
	  (char-downcase c)
	  (char-upcase c)))
    
    (define (check-special-cases name head form env)
      ;; here curlet won't change (except for and/or/not, possibly apply, and map/for-each with a macro)
      ;; keyword head here if args to func/macro that we don't know about
      
      ;; TODO:
      ;; macros that cause definitions are ignored (this also affects variable usage stats) and cload'ed identifiers are missed
      ;; bacro-shaker -- can we get set-member?
      ;; need values->func arg check escape (define*), or can these be correlated?
      ;;   display->format returns values, gather-format is recipient, takes 1 or 2 args -- check d->f
      ;;   can this info be saved in var? if (car sig) includes values, what are possibilities?
      ;;
      ;; pp (if...) as func par ->cr+indent if long?, and look-ahead for long pars and cr+indent all, also (lambda...)
      ;;   pp sketch mode would be useful, and truncate at right margin
      ;; unquasiquote innards pretty-printed and check quotes, doubled ,@
      ;;
      ;; bug: (if quoted ({list} 'not ({list} op selector ({list} 'quote... -> `(not ,(if quoted `(,op ,selector (quote ,keylist)) `(,op ,selector ,keylist)))
      ;;
      ;; (if (= 3 (length eq)) (caddr eq) #f) -> (and ...) with extra and omitted? [append test true]
      ;; (cond ((= x y) #f) (else ...)) ?
      ;;
      ;; 255/40 -> 228/38

      (case head

	;; ----------------
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
	 
	 (when (= (length form) 4)
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
							'assoc)))))))))))
	 
	 (when (= (length form) 3)
	   (let ((selector (cadr form))
		 (items (caddr form)))
	     
	     (let ((current-eqf (case head ((memq assq) 'eq?) ((memv assv) 'eqv?) (else 'equal?)))
		   (selector-eqf (eqf selector))
		   (one-item (and (memq head '(memq memv member)) (list-one? items))))
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
		   
		   ;; not one-item
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
			       (if (eq? (car items) 'list)
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
				      (truncated-list->string (map unquoted (cdr items)))))
		     
		     (when (pair? items)
		       (let ((memx (memq head '(memq memv member))))
			 (case (car items)
			   ((map)
			    (when (and memx (= (length items) 3))
			      (let ((mapf (cadr items))
				    (map-items (caddr items)))
				(cond ((eq? mapf 'car)
				       (lint-format "perhaps use assoc: ~A" name
						    (lists->string form `(,(case current-eqf ((eq?) 'assq) ((eqv?) 'assv) ((equal?) 'assoc)) 
									  ,selector ,map-items))))
				      ((eq? selector #t)
				       (if (eq? mapf 'null?)
					   (lint-format "perhaps ~A" name 
							(lists->string form `(memq () ,map-items)))
					   (let ((b (if (eq? mapf 'b) 'c 'b)))
					     (lint-format "perhaps avoid 'map: ~A" name 
							  (lists->string form `(member #t ,map-items (lambda (a ,b) (,mapf ,b))))))))
				      
				      ((and (pair? selector)
					    (eq? (car selector) 'string->symbol) ; this could be extended, but it doesn't happen
					    (eq? mapf 'string->symbol)
					    (or (not (pair? map-items))
						(not (eq? (car map-items) 'quote))))
				       (lint-format "perhaps ~A" name
						    (lists->string form `(member ,(cadr selector) ,map-items string=?))))
				      (else 
				       (let ((b (if (eq? mapf 'b) 'c 'b))) ; a below can't collide because eqf won't return 'a
					 (lint-format "perhaps avoid 'map: ~A" name 
						      (lists->string form `(member ,selector ,map-items 
										   (lambda (a ,b) (,current-eqf a (,mapf ,b))))))))))))
			   ((cons)
			    (if (not (pair? selector))
				(lint-format "perhaps avoid 'cons: ~A" name
					     (lists->string form `(or (,current-eqf ,selector ,(cadr items))
								      (,head ,selector ,(caddr items)))))))
			   ((append)
			    (if (and (not (pair? selector))
				     (= (length items) 3)
				     (pair? (cadr items))
				     (eq? (caadr items) 'list)
				     (null? (cddadr items)))
				(lint-format "perhaps ~A" name
					     (lists->string form `(or (,current-eqf ,selector ,(cadadr items))
								      (,head ,selector ,(caddr items)))))))))))))
	     
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
			 (lint-format "pointless list member: ~S in ~A" name bad form))))))))
	
	;; ----------------
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
			(cxr (string-append (innards head) 
					    (innards (car arg1)) 
					    (if arg2 (innards (car arg2)) "")
					    (if arg3 (innards (car arg3)) ""))))
		   (cond ((< (length cxr) 5)
			  (lint-format "perhaps ~A" name (lists->string form `(,(string->symbol (string-append "c" cxr "r")) 
									       ,(cadr (or arg3 arg2 arg1))))))
			 ;; if it's car|cdr followed by cdr's, use list-ref|tail
			 ((not (char-position #\a cxr))
			  (lint-format "perhaps ~A" name (lists->string form `(list-tail ,(cadr (or arg3 arg2 arg1)) 
											 ,(length cxr)))))
			 ((not (char-position #\a (substring cxr 1)))
			  (lint-format "perhaps ~A" name (lists->string form `(list-ref ,(cadr (or arg3 arg2 arg1)) 
											,(- (length cxr) 1)))))
			 (else (set! last-simplify-boolean-line-number -1)))))))
	 
	 (when (and (eq? head 'car)                             ; (car (list-tail x y)) -> (list-ref x y)
		    (pair? (cadr form))
		    (eq? (caadr form) 'list-tail))
	   (lint-format "perhaps ~A" name (lists->string form `(list-ref ,(cadadr form) ,(caddr (cadr form))))))
	 
	 (if (and (memq head '(car cdr))
		  (pair? (cadr form))
		  (eq? (caadr form) 'cons))
	     (lint-format "(~A~A) is the same as ~A"
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
		   (lint-format "perhaps ~A" name                  ; (car (reverse x)) -> (list-ref x (- (length x) 1))
				(lists->string form `(list-ref ,(cadadr form) 
							       (- (length ,(cadadr form)) 
								  ,(case head ((car) 1) ((cadr) 2) ((caddr) 3) (else 4))))))))))
	
	;; ----------------
	((set-car!)
	 (when (and (= (length form) 3)
		    (pair? (cadr form)))
	   (let ((target (cadr form)))
	     (case (car target)
	       
	       ((list-tail)                              ; (set-car! (list-tail x y) z) -> (list-set! x y z)
		(lint-format "perhaps ~A" name (lists->string form `(list-set! ,(cadr target) ,(caddr target) ,(caddr form)))))
	       
	       ((cdr cddr cdddr cddddr)
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
	
	;; ----------------
	((and or not)
	 (if (not (= line-number last-simplify-boolean-line-number))
	     (let ((val (simplify-boolean form () () env)))
	       (set! last-simplify-boolean-line-number line-number)
	       (if (not (equal? form val))
		   (lint-format "perhaps ~A" name (lists->string form val))))))
	
	;; ----------------
	((=)
	 (let ((len (length form)))
	   (if (and (> len 2)
		    (any-real? (cdr form)))
	       (lint-format "= can be troublesome with floats: ~A" name (truncated-list->string form)))
	   
	   (let ((cleared-form (cons = (remove-if (lambda (x) (not (number? x))) (cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true: ~A" name (truncated-list->string form))))
	   
	   (when (= len 3)
	     (let ((arg1 (cadr form))
		   (arg2 (caddr form)))
	       (let ((var (or (and (memv arg1 '(0 1))
				   (pair? arg2)
				   (eq? (car arg2) 'length)
				   (cadr arg2))
			      (and (memv arg2 '(0 1))
				   (pair? arg1)
				   (eq? (car arg1) 'length)
				   (cadr arg1)))))
		 ;; TODO: if it's (f y) -- check return type of f -- and we need Snd function signatures!
		 (if var
		     (if (or (eqv? arg1 0) 
			     (eqv? arg2 0))
			 (lint-format "perhaps (assuming ~A is a list), ~A" name var 
				      (lists->string form `(null? ,var)))
			 (if (symbol? var)
			     (lint-format "perhaps (assuming ~A is a list), ~A" name var 
					  (lists->string form `(and (pair? ,var) (null? (cdr ,var))))))))))
	     
	     (unrelop name '= form))
	   (check-char-cmp name head form)))
	
	;; ----------------
	((< > <= >=) ; '= handled above
	 (let ((cleared-form (cons head ; keep operator
				   (remove-if (lambda (x) 
						(not (number? x))) 
					      (cdr form)))))
	   (if (and (> (length cleared-form) 2)
		    (not (checked-eval cleared-form)))
	       (lint-format "this comparison can't be true: ~A" name (truncated-list->string form))))
	 
	 (if (= (length form) 3)
	     (unrelop name head form)
	     (when (> (length form) 3)
	       (if (and (memq head '(< >))
			(repeated-member? (cdr form) env))
		   (lint-format "perhaps ~A" name (truncated-lists->string form #f))
		   (if (and (memq head '(<= >=))
			    (repeated-member? (cdr form) env))
		       (let ((last-arg (cadr form))
			     (new-args (list (cadr form))))
			 (do ((lst (cddr form) (cdr lst)))
			     ((null? lst) 
			      (if (repeated-member? new-args env)
				  (lint-format "perhaps ~A" name (truncated-lists->string form `(= ,@(lint-remove-duplicates (reverse new-args) env))))
				  (if (< (length new-args) (length (cdr form)))
				      (if (null? (cdr new-args))
					  (lint-format "perhaps ~A" name (truncated-lists->string form #t))
					  (lint-format "perhaps ~A" name (truncated-lists->string form `(= ,@(reverse new-args))))))))
			   (unless (equal? (car lst) last-arg)
			     (set! last-arg (car lst))
			     (set! new-args (cons last-arg new-args)))))))))
	 
	 (check-char-cmp name head form))
	;; could change (> x 0) to (positive? x) and so on, but the former is clear and ubiquitous
	
	;; ----------------
	((char<? char>? char<=? char>=? char=? 
		 char-ci<? char-ci>? char-ci<=? char-ci>=? char-ci=?)
	 (let ((cleared-form (cons head ; keep operator
				   (remove-if (lambda (x) 
						(not (char? x))) 
					      (cdr form)))))
	   (if (and (> (length cleared-form) 2)
		    (not (checked-eval cleared-form)))
	       (lint-format "this comparison can't be true: ~A" name (truncated-list->string form))))
	 (if (and (eq? head 'char-ci=?) ; (char-ci=? x #\return)
		  (pair? (cdr form))
		  (pair? (cddr form))
		  (null? (cdddr form))
		  (or (and (char? (cadr form))
			   (char=? (cadr form) (other-case (cadr form))))
		      (and (char? (caddr form))
			   (char=? (caddr form) (other-case (caddr form))))))
	     (lint-format "char-ci=? could be char=? here: ~A" name form)))
	
	;; ----------------
	((string<? string>? string<=? string>=? string=? 
		   string-ci<? string-ci>? string-ci<=? string-ci>=? string-ci=?)
	 (let ((cleared-form (cons head ; keep operator
				   (remove-if (lambda (x) 
						(not (string? x))) 
					      (cdr form)))))
	   (if (and (> (length cleared-form) 2)
		    (not (checked-eval cleared-form)))
	       (lint-format "this comparison can't be true: ~A" name (truncated-list->string form)))))
	
	;; ----------------
	((length)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(reverse reverse! list->vector vector->list list->string string->list let->list)))
	     (lint-format "perhaps ~A" name (lists->string form `(length ,(cadadr form))))))
	
	;; ----------------
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
		     (lint-format "exit-function ~A appears to be unused: ~A" name return (truncated-list->string form)))))))
	
	;; ----------------
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
	
	;; ----------------
	((call-with-input-string call-with-input-file call-with-output-file call-with-output-string)
	 ;; call-with-output-string func is the first arg, not second
	 (let ((len (if (eq? head 'call-with-output-string) 2 3)))
	   (when (= (length form) len)
	     (let ((func (list-ref form (- len 1))))
	       (when (and (pair? func)
			  (eq? (car func) 'lambda))
		 (let* ((args (cadr func))
			(body (cddr func))
			(port (and (pair? args) (car args))))
		   (if (or (not port)
			   (pair? (cdr args)))
		       (lint-format "~A argument should be a function of one argument: ~A" name head func)
		       (if (and (symbol? port)
				(not (tree-member port body)))
			   (lint-format "port ~A appears to be unused: ~A" name port (truncated-list->string form))))))))))
	;; checking for port matches in the lambda body is very tricky and never gets hits

	;; ----------------
	((/)
	 (if (pair? (cdr form))
	     (if (and (null? (cddr form))
		      (number? (cadr form))
		      (zero? (cadr form)))
		 (lint-format "attempt to invert zero: ~A" name (truncated-list->string form))
		 (if (and (pair? (cddr form))
			  (memv 0 (cddr form)))
		     (lint-format "attempt to divide by 0: ~A" name (truncated-list->string form))))))
	
	;; ----------------
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
	
	;; ----------------
	((string-copy)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(copy string-copy)))
	     (lint-format "~A could be ~A" name form (cadr form))))
	
	;; ----------------
	((string)
	 (if (every? (lambda (x) 
		       (and (char? x)
			    (char<=? #\space x #\~))) ; #\0xx chars here look dumb
		     (cdr form))
	     (lint-format "~A could be ~S" name form (apply string (cdr form)))))
	
	;; ----------------
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
	
	;; ----------------
	((symbol? integer? rational? real? complex? float? keyword? gensym? byte-vector? list? proper-list?
		  char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair?
		  output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?)
	 (if (and (= (length form) 2)
		  (not (symbol? (cadr form)))
		  (not (= line-number last-simplify-boolean-line-number)))
	     (let ((expr (simplify-boolean form () () env)))
	       (if (not (equal? expr form))
		   (lint-format "perhaps ~A" name (lists->string form expr))))))
	
	;; ----------------
	((string-ref)
	 (when (and (= (length form) 3)
		    (pair? (cadr form)))
	   (let ((target (cadr form)))
	     (case (car target)
	       ((substring)
		(if (= (length target) 3)
		    (lint-format "perhaps ~A" name (lists->string form `(string-ref ,(cadr target) (+ ,(caddr form) ,(caddr target)))))))
	       ((symbol->string)
		(if (and (integer? (caddr form))
			 (pair? (cadr target))
			 (eq? (caadr target) 'quote)
			 (symbol? (cadadr target)))
		    (lint-format "perhaps ~A" name (lists->string form (string-ref (symbol->string (cadadr target)) (caddr form))))))
	       ((make-string)
		(if (and (integer? (cadr target))
			 (integer? (caddr form))
			 (> (cadr target) (caddr form)))
		    (lint-format "perhaps ~A" name (lists->string form (if (= (length target) 3) (caddr target) #\space)))))))))
	
	;; ----------------
	((vector-ref list-ref hash-table-ref let-ref int-vector-ref float-vector-ref)
	 (unless (= line-number last-checker-line-number)
	   (if (= (length form) 3)
	       (let ((seq (cadr form)))
		 (if (pair? seq)
		     (if (and (eq? (car seq) head) ; perhaps instead: (memq (car seq) '(vector-ref list-ref hash-table-ref let-ref))
			      (= (length seq) 3))
			 (let ((seq1 (cadr seq)))
			   (lint-format "perhaps ~A" name 
					(lists->string form 
						       (if (and (pair? seq1)
								(eq? (car seq1) head)
								(= (length seq1) 3))
							   `(,(cadr seq1) ,(caddr seq1) ,(caddr seq) ,(caddr form))
							   `(,seq1 ,(caddr seq) ,(caddr form))))))
			 (if (memq (car seq) '(make-vector make-list vector list
							   make-float-vector make-int-vector float-vector int-vector
							   make-hash-table hash-table hash-table*
							   inlet))
			     (lint-format "this doesn't make much sense: ~A~%" name form))))))
	   (set! last-checker-line-number line-number)))
	
	;; ----------------
	((vector-set! list-set! hash-table-set! float-vector-set! int-vector-set!)
	 (if (= (length form) 4)
	     (let ((target (cadr form))
		   (index (caddr form))
		   (val (cadddr form)))
	       (cond ((and (pair? val)
			   (= (length val) 3)
			   (eq? target (cadr val))
			   (equal? index (caddr val))
			   (memq (car val) '(vector-ref list-ref hash-table-ref float-vector-ref int-vector-ref)))
		      (lint-format "redundant?: ~A" name (truncated-list->string form)))
		     ((and (pair? target) 
			   (memq (car target) '(vector-ref list-ref hash-table-ref float-vector-ref int-vector-ref)))
		      (lint-format "perhaps ~A" name (lists->string form `(set! (,@(cdr target) ,index) ,val))))
		     ((or (code-constant? (cadr form))
			  (and (pair? (cadr form))
			       (memq (caadr form) '(make-vector vector make-string string make-list list append cons vector-append copy))))
		      (lint-format "perhaps ~A" name (lists->string form val)))))))
	
	;; ----------------
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
	
	;; ----------------
	((display)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form)))
	     (let ((arg (cadr form))
		   (port (if (pair? (cddr form))
			     (caddr form)
			     ())))
	       (if (and (eq? (car arg) 'format)
			(pair? (cdr arg))
			(not (cadr arg)))
		   (lint-format "perhaps ~A" name (lists->string form `(format ,port ,@(cddr arg))))
		   (if (and (eq? (car arg) 'apply)
			    (pair? (cdr arg))
			    (eq? (cadr arg) 'format)
			    (pair? (cddr arg))
			    (not (caddr arg)))
		       (lint-format "perhaps ~A" name (lists->string form `(apply format ,port ,@(cdddr arg)))))))))
	
	;; ----------------
	((make-vector)
	 (if (and (= (length form) 4)
		  (code-constant? (caddr form))
		  (not (real? (caddr form)))
		  (eq? (cadddr form) #t))
	     (lint-format "~A won't create an homogenous vector" name form)))
	
	;; ----------------
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
	     (let ((inv-op (assq head inverses))
		   (arg (cadr form))
		   (arg-of-arg (cadadr form)))
	       (if (pair? inv-op) (set! inv-op (cdr inv-op)))
	       
	       ;; TODO: some of these are currently ignoring the start/end args -- maybe move out the list->vector constant and check (null? (cddr arg))?
	       
	       (cond ((eq? (car arg) inv-op)                 ; (vector->list (list->vector x)) -> x
		      (if (eq? head 'string->symbol)
			  (lint-format "perhaps ~A" name (lists->string form arg-of-arg))
			  (lint-format "~A could be (copy ~S)" name form arg-of-arg)))
		     
		     ((and (eq? head 'list->string)          ; (list->string (vector->list x)) -> (copy x (make-string (length x)))
			   (eq? (car arg) 'vector->list))
		      (lint-format "perhaps ~A" name (lists->string form `(copy ,arg-of-arg (make-string (length ,arg-of-arg))))))
		     
		     ((and (eq? head 'list->vector)          ; (list->vector (string->list x)) -> (copy x (make-vector (length x)))
			   (eq? (car arg) 'string->list))
		      (lint-format "perhaps ~A" name (lists->string form `(copy ,arg-of-arg (make-vector (length ,arg-of-arg))))))
		     
		     ((and (eq? head 'vector->list)          ; (vector->list (make-vector ...)) -> (make-list ...)
			   (eq? (car arg) 'make-vector))
		      (lint-format "perhaps ~A" name (lists->string form `(make-list ,@(cdr arg)))))
		     
		     ((and (eq? head 'list->vector)          ; (list->vector (make-list ...)) -> (make-vector ...)
			   (eq? (car arg) 'make-list))
		      (lint-format "perhaps ~A" name (lists->string form `(make-vector ,@(cdr arg)))))
		     
		     ((and (memq (car arg) '(reverse reverse! copy))
			   (pair? (cadr arg))                ; (list->string (reverse (string->list x))) -> (reverse x)
			   (eq? (caadr arg) inv-op))
		      (lint-format "perhaps ~A" name (lists->string form `(,(if (eq? (car arg) 'reverse!) 'reverse (car arg)) ,(cadadr arg)))))
		     
		     ((and (pair? (cadr arg))
			   (memq (car arg) '(cdr cddr cdddr cddddr list-tail))
			   (or (and (eq? head 'list->string)
				    (eq? (caadr arg) 'string->list))
			       (and (eq? head 'list->vector)
				    (eq? (caadr arg) 'vector->list))))
		      (let ((len-diff (if (eq? (car arg) 'list-tail)
					  (caddr arg)
					  (cdr-count (car arg)))))
			(lint-format "perhaps ~A" name 
				     (lists->string form (if (eq? head 'list->string)
							     `(substring ,(cadadr arg) ,len-diff)
							     `(copy ,(cadadr arg) (make-vector (- (length ,(cadadr arg)) ,len-diff))))))))
		     
		     ((and (memq head '(list->vector list->string))
			   (eq? (car arg) 'sort!)
			   (pair? (cadr arg))
			   (eq? (caadr arg) (if (eq? head 'list->vector) 'vector->list 'string->list)))
		      (lint-format "perhaps ~A" name (lists->string form `(sort! ,(cadadr arg) ,(caddr arg)))))
		     
		     ((and (memq head '(list->vector list->string))
			   (or (memq (car arg) '(list cons))
			       (quoted-undotted-pair? arg)))
		      (let ((maker (if (eq? head 'list->vector) 'vector 'string)))
			(cond ((eq? (car arg) 'list)
			       (if (var-member maker env)
				   (lint-format "~A could be simplified, but you've shadowed '~A" name form maker)
				   (lint-format "perhaps ~A" name (lists->string form `(,maker ,@(cdr arg))))))
			      ((eq? (car arg) 'cons)
			       (if (or (null? (caddr arg))
				       (quoted-null? (caddr arg)))
				   (if (var-member maker env)
				       (lint-format "~A could be simplified, but you've shadowed '~A" name form maker)
				       (lint-format "perhaps ~A" name (lists->string form `(,maker ,(cadr arg)))))))
			      ((or (null? (cddr form))
				   (and (integer? (caddr form))
					(or (null? (cdddr form))
					    (integer? (cadddr form)))))
			       (lint-format "perhaps ~A" name 
					    (lists->string form (apply (if (eq? head 'list->vector) vector string) (cadr arg))))))))
		     
		     ((and (eq? head 'list->string)          ; (list->string (reverse x)) -> (reverse (apply string x))
			   (memq (car arg) '(reverse reverse!)))
		      (lint-format "perhaps ~A" name (lists->string form `(reverse (apply string ,arg-of-arg)))))
		     
		     ((and (pair? arg-of-arg)                ; (op (reverse (inv-op x))) -> (reverse x)
			   (eq? (car arg) 'reverse)
			   (eq? inv-op (car arg-of-arg)))
		      (lint-format "perhaps ~A" name (lists->string form `(reverse ,(cadr arg-of-arg)))))))))
	 
	 (when (and (pair? (cdr form))
		    (not (pair? (cadr form))))
	   (let ((arg (cadr form)))
	     (if (and (eq? head 'string->list)
		      (string? arg)
		      (or (null? (cddr form))
			  (and (integer? (caddr form))
			       (or (null? (cdddr form))
				   (integer? (cadddr form))))))
		 (lint-format "perhaps ~A -> ~A" name form (apply string->list (cdr form))))))
	 
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
	
	;; ----------------
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
	       (lint-format "~A could be ~A" name form (cadadr form))
	       (if (and (eq? head 'integer->char)
			(pair? (cdr form))
			(integer? (cadr form))
			(or (<= 32 (cadr form) 127)
			    (memv (cadr form) '(0 7 8 9 10 13 27))))
		   (lint-format "perhaps ~A -> ~W" name form (integer->char (cadr form)))))))
	
	;; ----------------
	((string-append)
	 (if (not (= line-number last-checker-line-number))
	     (let ((args (remove-all "" (splice-if (lambda (x) (eq? x 'string-append)) (cdr form)))))
	       (cond ((null? args)
		      (lint-format "perhaps ~A" name (lists->string form "")))
		     ((null? (cdr args))
		      (lint-format "perhaps ~A, or use copy" name (lists->string form (car args))))
		     ((every? string? args)
		      (lint-format "perhaps ~A" name (lists->string form (apply string-append args))))
		     ((not (equal? args (cdr form)))
		      (lint-format "perhaps ~A" name (lists->string form `(string-append ,@args)))))
	       (set! last-checker-line-number line-number))))
	
	;; ----------------
	((vector-append)
	 (if (not (= line-number last-checker-line-number))
	     (let ((args (remove-all #() (splice-if (lambda (x) (eq? x 'vector-append)) (cdr form)))))
	       (cond ((null? args)
		      (lint-format "perhaps ~A" name (lists->string form #())))
		     ((null? (cdr args))
		      (lint-format "perhaps ~A" name (lists->string form `(copy ,(car args)))))
		     ((every? vector? args)
		      (lint-format "perhaps ~A" name (lists->string form (apply vector-append args))))
		     ((not (equal? args (cdr form)))
		      (lint-format "perhaps ~A" name (lists->string form `(vector-append ,@args)))))
	       (set! last-checker-line-number line-number))))
	
	;; ----------------
	((cons)
	 (if (= (length form) 3)
	     (if (and (pair? (caddr form))
		      (eq? (caaddr form) 'list))   ; (cons x (list ...)) -> (list x ...)
		 (lint-format "perhaps ~A" name (lists->string form `(list ,(cadr form) ,@(cdaddr form))))
		 (if (or (null? (caddr form))      ; (cons x '()) -> (list x)
			 (quoted-null? (caddr form)))
		     (lint-format "perhaps ~A" name (lists->string form `(list ,(cadr form))))))))
	
	;; ----------------
	((append)
	 (unless (= line-number last-checker-line-number)
	   (set! last-checker-line-number line-number)
	   (letrec ((splice-append (lambda (lst)
				     (cond ((null? lst)
					    ())
					   ((pair? lst)
					    (if (and (pair? (car lst))
						     (eq? (caar lst) 'append))
						(if (null? (cdar lst))
						    (if (null? (cdr lst)) ; (append) at end -> () to keep copy intact?
							(list ())
							(splice-append (cdr lst)))
						    (append (splice-append (cdar lst)) (splice-append (cdr lst))))
						(if (or (null? (cdr lst))
							(not (or (null? (car lst))
								 (quoted-null? (car lst))
								 (and (pair? (car lst))
								      (eq? (caar lst) 'list)
								      (null? (cdar lst))))))
						    (cons (car lst) (splice-append (cdr lst)))
						    (splice-append (cdr lst)))))
					   (#t lst)))))
	     (let ((new-args (splice-append (cdr form))))     ; (append '(1) (append '(2) '(3))) -> (append '(1) '(2) '(3))
	       (let ((len1 (length new-args))
		     (suggestion made-suggestion))
		 (if (and (> len1 2)
			  (null? (list-ref new-args (- len1 1)))
			  (pair? (list-ref new-args (- len1 2)))
			  (memq (car (list-ref new-args (- len1 2))) '(list cons append map string->list vector->list make-list)))
		     (set-cdr! (list-tail new-args (- len1 2)) ()))
		 
		 (define (append->list . items)
		   (let ((lst (list 'list)))
		     (for-each 
		      (lambda (item)
			(set! lst (append lst (if (eq? (car item) 'list)
						  (cdr item)
						  (distribute-quote (cadr item))))))
		      items)
		     lst))
		 
		 (case len1
		   ((0)					        ; (append) -> ()
		    (lint-format "perhaps ~A" name (lists->string form ())))
		   ((1)                                               ; (append x) -> x
		    (lint-format "perhaps ~A" name (lists->string form (car new-args))))
		   ((2)                                               ; (append (list x) ()) -> (list x)
		    (let ((arg2 (cadr new-args))
			  (arg1 (car new-args)))
		      (cond ((or (null? arg2)           
				 (quoted-null? arg2)
				 (equal? arg2 '(list)))               ; (append x ()) -> (copy x)
			     (lint-format "perhaps clearer: ~A" name (lists->string form `(copy ,arg1))))
			    
			    ((null? arg1)                             ; (append () x) -> x
			     (lint-format "perhaps ~A" name (lists->string form arg2)))
			    
			    ((pair? arg1)            
			     (cond ((and (pair? arg2)                 ; (append (list x y) '(z)) -> (list x y 'z)
					 (or (eq? (car arg1) 'list)
					     (quoted-undotted-pair? arg1))
					 (or (eq? (car arg2) 'list)
					     (quoted-undotted-pair? arg2)))
				    (lint-format "perhaps ~A" name (lists->string form (apply append->list new-args))))
				   
				   ((and (eq? (car arg1) 'list)       ; (append (list x) y) -> (cons x y)
					 (pair? (cdr arg1))
					 (null? (cddr arg1)))
				    (lint-format "perhaps ~A" name (lists->string form `(cons ,(cadr arg1) ,arg2))))
				   
				   ((not (equal? (cdr form) new-args))
				    (lint-format "perhaps ~A" name (lists->string form `(append ,@new-args)))))))))
		   (else
		    (if (every? (lambda (item)
				  (and (pair? item)
				       (or (eq? (car item) 'list)
					   (quoted-undotted-pair? item))))
				new-args)
			(lint-format "perhaps ~A" name (lists->string form (apply append->list new-args))))))
		 
		 (if (and (= made-suggestion suggestion)
			  (not (equal? (cdr form) new-args)))
		     (lint-format "perhaps ~A" name (lists->string form `(append ,@new-args)))))))))
	
	;; ----------------
	((apply)
	 (when (pair? (cdr form))
	   (let ((len (length form))
		 (suggestion made-suggestion))
	     (if (= len 2)
		 (lint-format "perhaps ~A" name (lists->string form (list (cadr form))))
		 (if (and (> len 2) ; it might be (apply)...
			  (not (symbol? (cadr form)))
			  (not (applicable? (cadr form))))
		     (lint-format "~S is not applicable: ~A" name (cadr form) (truncated-list->string form))
		     (let ((happy #f)
			   (f (cadr form)))
		       (when (and (> len 2)
				  (not (any-macro? f env))
				  (not (eq? f 'macroexpand))) ; handled specially (syntactic, not a macro)
			 
			 (when (and (symbol? f)
				    (not (var-member f env))
				    (not (hash-table-ref globals f)))
			   (let ((func (symbol->value f *e*)))
			     (if (procedure? func)
				 (let ((ary (arity func)))
				   (if (and (pair? ary)
					    (> (- (length (cddr form)) 1) (cdr ary))) ; last apply arg might be var=()
				       (lint-format "too many arguments for ~A: ~A" name f form))))))
			 
			 (let ((last-arg (form (- len 1))))
			   (if (and (not (list? last-arg))
				    (code-constant? last-arg))
			       (lint-format "last argument should be a list: ~A" name (truncated-list->string form))
			       (if (= len 3)
				   (let ((args (caddr form)))
				     (if (identity? f)                                ; (apply (lambda (x) x) y) -> (car y)
					 (lint-format "perhaps (assuming ~A is a list of one element) ~A" name args 
						      (lists->string form `(car ,args)))
					 (if (simple-lambda? f)                       ; (apply (lambda (x) (f x)) y) -> (f (car y))
					     (lint-format "perhaps (assuming ~A is a list of one element) ~A" name args 
							  (lists->string form (tree-subst (list 'car (caddr form)) (caadr f) (caddr f))))))
				     
				     (cond ((eq? f 'list)                             ; (apply list x) -> x?
					    (lint-format "perhaps ~A" name (lists->string form args)))
					   ((or (null? args)                          ; (apply f ()) -> (f)
						(quoted-null? args))
					    (lint-format "perhaps ~A" name (lists->string form (list (cadr form)))))
					   ((pair? args)
					    (cond ((eq? (car args) 'list)             ; (apply f (list a b)) -> (f a b)
						   (lint-format "perhaps ~A" name (lists->string form `(,f ,@(cdr args)))))
						  
						  ((and (eq? (car args) 'quote)       ; (apply eq? '(a b)) -> (eq? 'a 'b)
							(= suggestion made-suggestion))
						   (lint-format "perhaps ~A" name (lists->string form `(,f ,@(distribute-quote (cadr args))))))
						  
						  ((eq? (car args) 'cons)             ; (apply f (cons a b)) -> (apply f a b)
						   (lint-format "perhaps ~A" name (lists->string form `(apply ,f ,@(cdr args)))))
						  
						  ((and (memq f '(string vector int-vector float-vector))
							(memq (car args) '(reverse reverse!))) ; (apply vector (reverse x)) -> (reverse (apply vector x))
						   (lint-format "perhaps ~A" name (lists->string form `(reverse (apply ,f ,(cadr args))))))
						  
						  ((and (eq? f 'string-append)        ; (apply string-append (map ...))
							(eq? (car args) 'map))
						   (if (eq? (cadr args) 'symbol->string)
						       (lint-format "perhaps ~A" name ; (apply string-append (map symbol->string ...))
								    (lists->string form `(format #f "~{~A~}" ,(caddr args))))
						       (if (simple-lambda? (cadr args))
							   (let ((body (caddr (cadr args))))
							     (if (and (pair? body)
								      (eq? (car body) 'string-append)
								      (= (length body) 3)
								      (or (and (string? (cadr body))
									       (eq? (caddr body) (caadr (cadr args))))
									  (and (string? (caddr body))
									       (eq? (cadr body) (caadr (cadr args))))))
								 (let ((str (string-append "~{" 
											   (if (string? (cadr body)) (cadr body) "~A")
											   (if (string? (caddr body)) (caddr body) "~A")
											   "~}")))
								   (lint-format "perhaps ~A" name
										(lists->string form `(format #f ,str ,(caddr args))))))))))
						  
						  ((and (eq? (car args) 'append)      ; (apply f (append (list ...)...)) -> (apply f ... ...)
							(pair? (cadr args))
							(eq? (caadr args) 'list))
						   (lint-format "perhaps ~A" name 
								(lists->string form `(apply ,f ,@(cdadr args)
											    ,(if (null? (cddr args)) ()
												 (if (null? (cdddr args)) (caddr args)
												     `(append ,@(cddr args))))))))))))
				   (begin ; len > 3
				     (when (and (= len 4)
						(memq f '(for-each map))             ; (apply map f (list x y)) -> (map f x y)
						(pair? (cadddr form))
						(eq? (car (cadddr form)) 'list))
				       (lint-format "perhaps ~A" name (lists->string form `(,(cadr form) ,(caddr form) ,@(cdr (cadddr form))))))
				     
				     (when (and (not happy)
						(not (memq f '(define define* define-macro define-macro* define-bacro define-bacro* lambda lambda*)))
						(or (null? last-arg)
						    (quoted-null? last-arg)))     ; (apply f ... ()) -> (f ...)
				       (lint-format "perhaps ~A" name (lists->string form `(,f ,@(copy (cddr form) (make-list (- len 3))))))))))))))))))
	
	;; ----------------
	((format snd-display)
	 (if (< (length form) 3)
	     (begin
	       (cond ((< (length form) 2)
		      (lint-format "~A has too few arguments: ~A" name head (truncated-list->string form)))
		     ((and (pair? (cadr form))
			   (eq? (caadr form) 'format))
		      (lint-format "redundant format: ~A" name (truncated-list->string form)))
		     ((and (code-constant? (cadr form))
			   (not (string? (cadr form))))
		      (lint-format "format with one argument takes a string: ~A" name (truncated-list->string form)))
		     ((and (not (cadr form))
			   (string? (caddr form)))
		      (lint-format "perhaps ~A" name (lists->string form (caddr form)))))
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
						       (cond ((>= (+ j 1) len)
							      (lint-format "missing format directive: ~S" name str))
							     ((char-ci=? (string-ref str (+ j 1)) #\n)
							      (set! dirs (+ dirs 1))
							      (set! j (+ j 2)))
							     ((char-numeric? (string-ref str (+ j 1)))
							      (set! j (+ j 2)))
							     (else (set! j (+ j 1)))))
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
					  (lint-format "~A has ~~~C outside ~~{~~}?" name str c))))
				   (if (and (< (+ i 2) len)
					    (member (substring str i (+ i 3)) '("%~&" "^~^" "|~|" "&~&" "\n~\n") string=?))
				       (lint-format "~A in ~A could be ~A" name
						    (substring str (- i 1) (+ i 3))
						    str
						    (substring str (- i 1) (+ i 1)))))
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
	       
	       (when (and (eq? head 'format)
			  (string? (cadr form)))
		 (lint-format "please include the port argument to format, perhaps ~A" name `(format () ,@(cdr form))))
	       (if (not (string? control-string))
		   (if (not (proper-list? args))
		       (lint-format "~S looks suspicious" name form))
		   (let ((ndirs (count-directives control-string name form))
			 (nargs (if (or (null? args) (pair? args)) (length args) 0)))
		     (let ((pos (char-position #\null control-string)))
		       (if (and pos (< pos (length control-string)))
			   (lint-format "#\\null in a format control string will confuse both lint and format: ~S in ~A" name control-string form)))
		     (if (not (= ndirs nargs))
			 (lint-format "~A has ~A arguments: ~A" 
				      name head 
				      (if (> ndirs nargs) "too few" "too many")
				      (truncated-list->string form))
			 (if (and (not (cadr form))
				  (zero? ndirs)
				  (not (char-position #\~ control-string)))
			     (lint-format "~A could be ~S, (format is a no-op here)" name form (caddr form)))))))))
	
	;; ----------------
	((sort!)
	 (if (= (length form) 3)
	     (let ((func (caddr form)))
	       (if (memq func '(= eq? eqv? equal? string=? char=? string-ci=? char-ci=?))
		   (lint-format "sort! with ~A may hang: ~A" name func (truncated-list->string form))
		   (if (symbol? func)
		       (let ((sig (procedure-signature (symbol->value func))))
			 (if (and (pair? sig)
				  (not (eq? (car sig) 'boolean?)))
			     (lint-format "~A is a questionable sort! function" name func))))))))
	
	;; ----------------
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
	
	;; ----------------
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
	
	;; ----------------
	((eq?) 
	 (if (< (length form) 3)
	     (lint-format "eq? needs 2 arguments: ~A" name (truncated-list->string form))
	     (let* ((arg1 (cadr form))
		    (arg2 (caddr form))
		    (eq1 (eqf arg1))
		    (eq2 (eqf arg2))
		    (specific-op (and (eq? (cadr eq1) (cadr eq2))
				      (not (memq (cadr eq1) '(eqv? equal?)))
				      (cadr eq1))))
	       (if (or (eq? (car eq1) 'equal?)
		       (eq? (car eq2) 'equal?))
		   (lint-format "eq? should be equal?~A in ~S" name (if specific-op (format #f " or ~A" specific-op) "") form)
		   (if (or (eq? (car eq1) 'eqv?)
			   (eq? (car eq2) 'eqv?))
		       (lint-format "eq? should be eqv?~A in ~S" name (if specific-op (format #f " or ~A" specific-op) "") form)))
	       
	       (let ((expr 'unset))
		 (cond ((or (not arg1)                  ; (eq? #f x) -> (not x)
			    (quoted-not? arg1))
			(set! expr (simplify-boolean `(not ,arg2) () () env)))
		       ((or (not arg2)                  ; (eq? x #f) -> (not x)
			    (quoted-not? arg2))
			(set! expr (simplify-boolean `(not ,arg1) () () env)))
		       ((and (or (null? arg1)           ; (eq? () x) -> (null? x)
				 (quoted-null? arg1))
			     (not (code-constant? arg2)))
			(set! expr `(null? ,arg2)))
		       ((and (or (null? arg2)           ; (eq? x ()) -> (null? x)
				 (quoted-null? arg2))
			     (not (code-constant? arg1)))
			(set! expr `(null? ,arg1)))
		       ((and (eq? arg1 #t)              ; (eq? #t <boolean-expr>) -> boolean-expr
			     (pair? arg2)
			     (eq? (return-type (car arg2) env) 'boolean?))
			(set! expr arg2))
		       ((and (eq? arg2 #t)              ; ; (eq? <boolean-expr> #t) -> boolean-expr
			     (pair? arg1)
			     (eq? (return-type (car arg1) env) 'boolean?))
			(set! expr arg1)))
		 (if (not (eq? expr 'unset))
		     (lint-format "perhaps ~A" name (lists->string form expr)))))))
	
	;; ----------------
	((eqv? equal? morally-equal?) 
	 (if (< (length form) 3)
	     (lint-format "~A needs 2 arguments: ~A" name head (truncated-list->string form))
	     (let* ((arg1 (cadr form))
		    (arg2 (caddr form))
		    (eq1 (eqf arg1))
		    (eq2 (eqf arg2))
		    (specific-op (and (eq? (cadr eq1) (cadr eq2))
				      (not (memq (cadr eq1) '(eq? eqv? equal?)))
				      (cadr eq1))))
	       
	       (cond ((or (eq? (car eq1) 'equal?)
			  (eq? (car eq2) 'equal?))
		      (if (not (memq head '(equal? morally-equal?)))
			  (lint-format "~A should be equal?~A in ~S" name head 
				       (if specific-op (format #f " or ~A" specific-op) "") 
				       form)
			  (if specific-op
			      (lint-format "~A could be ~A in ~S" name head specific-op form))))
		     
		     ((or (eq? (car eq1) 'eqv?)
			  (eq? (car eq2) 'eqv?))
		      (if (not (memq head '(eqv? morally-equal?)))
			  (lint-format "~A ~A be eqv?~A in ~S" name head 
				       (if (eq? head 'eq?) "should" "could") 
				       (if specific-op (format #f " or ~A" specific-op) "")
				       form)
			  (if specific-op
			      (lint-format "~A could be ~A in ~S" name head specific-op form))))
		     
		     ((or (eq? (car eq1) 'eq?)
			  (eq? (car eq2) 'eq?))
		      (cond ((or (not arg1) 
				 (not arg2))
			     (lint-format "~A could be not: ~A" name head (lists->string form `(not ,(or arg1 arg2)))))
			    ((or (null? arg1) 
				 (null? arg2)
				 (quoted-null? arg1) (quoted-null? arg2))
			     (lint-format "~A could be null?: ~A" name head
					  (lists->string form 
							 (if (or (null? arg1) (quoted-null? arg1))
							     `(null? ,arg2)
							     `(null? ,arg1)))))
			    ((not (eq? head 'eq?))
			     (lint-format "~A could be eq?~A in ~S" name head 
					  (if specific-op (format #f " or ~A" specific-op) "") 
					  form))))))))
	
	;; ----------------
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
		 (when (and (eq? head 'map)
			    (identity? func)) ; to check f here as var is more work
		   (lint-format "perhaps ~A" name (lists->string form (caddr form))))
		 
		 (let ((arg1 (caddr form)))
		   (when (and (pair? arg1)
			      (memq (car arg1) '(cdr cddr cdddr cddddr list-tail))
			      (pair? (cdr arg1))
			      (pair? (cadr arg1))
			      (memq (caadr arg1) '(string->list vector->list)))
		     (let ((string-case (eq? (caadr arg1) 'string->list))
			   (len-diff (if (eq? (car arg1) 'list-tail)
					 (caddr arg1)
					 (cdr-count (car arg1)))))
		       (lint-format "~A accepts ~A arguments, so perhaps ~A" name head 
				    (if string-case 'string 'vector)
				    (lists->string arg1 (if string-case
							    `(substring ,(cadadr arg1) ,len-diff)
							    `(make-shared-vector ,(cadadr arg1) (- (length ,(cadadr arg1)) ,len-diff) ,len-diff)))))))
		 
		 (when (and (eq? head 'for-each)
			    (pair? (cadr form))
			    (eq? (caadr form) 'lambda)
			    (pair? (cdadr form))
			    (not (any? (lambda (x) (side-effect? x env)) (cddadr form))))
		   (lint-format "pointless for-each: ~A" name (truncated-list->string form)))
		 
		 (when (= args 1)
		   (let ((seq (caddr form)))
		     (when (and (pair? seq)
				(eq? (car seq) 'map)
				(= (length seq) 3))
		       ;; a toss-up -- probably faster to combine funcs here, and easier to read?
		       ;;   but only if first arg is only used once in first func, and everything is simple (one-line or symbol)
		       (let* ((seq-func (cadr seq))
			      (arg-name (find-unique-name func seq-func)))
			 
			 (if (symbol? func)
			     (if (symbol? seq-func)              
				 ;; (map f (map g h)) -> (map (lambda (x) (f (g x))) h) -- dubious...
				 (lint-format "perhaps ~A" name 
					      (lists->string form `(,head (lambda (,arg-name) 
									    (,func (,seq-func ,arg-name))) 
									  ,(caddr seq))))
				 (if (simple-lambda? seq-func)   
				     ;; (map f (map (lambda (x) (g x)) h)) -> (map (lambda (x) (f (g x))) h)
				     (lint-format "perhaps ~A" name 
						  (lists->string form `(,head (lambda (,arg-name)
										(,func ,(tree-subst arg-name (caadr seq-func) (caddr seq-func))))
									      ,(caddr seq))))))
			     (if (less-simple-lambda? func)
				 (if (symbol? seq-func)          
				     ;; (map (lambda (x) (f x)) (map g h)) -> (map (lambda (x) (f (g x))) h)
				     (lint-format "perhaps ~A" name 
						  (lists->string form `(,head (lambda (,arg-name)
										,@(tree-subst (list seq-func arg-name) (caadr func) (cddr func)))
									      ,(caddr seq))))
				     (if (simple-lambda? seq-func) 
					 ;; (map (lambda (x) (f x)) (map (lambda (x) (g x)) h)) -> (map (lambda (x) (f (g x))) h)
					 (lint-format "perhaps ~A" name  
						      (lists->string form `(,head (lambda (,arg-name)
										    ,@(tree-subst (tree-subst arg-name (caadr seq-func) (caddr seq-func))
												  (caadr func) (cddr func)))
										  ,(caddr seq))))))))))))
		 ;; repetitive code...
		 (when (and (eq? head 'for-each)
			    (pair? (cadr form))
			    (eq? (caadr form) 'lambda))
		   (let* ((func (cadr form))
			  (body (cddr func))
			  (op (write-port (car body)))
			  (larg (and (pair? (cadr func))
				     (caadr func))))
		     (when (and (symbol? larg)
				(null? (cdadr func)) ; just one arg (one sequence to for-each) for now
				(every? (lambda (x)
					  (and (pair? x)
					       (memq (car x) '(display write newline write-char write-string))
					       (or (eq? (car x) 'newline)
						   (eq? (cadr x) larg)
						   (string? (cadr x))
						   (and (char? (cadr x)) 
							(char=? (cadr x) #\space))
						   (and (pair? (cadr x))
							(pair? (cdadr x))
							(eq? (caadr x) 'number->string)
							(eq? (cadadr x) larg)))
					       (eq? (write-port x) op)))
					body))
		       ;; (for-each (lambda (x) (display x) (write-char #\space)) msg)
		       ;; (for-each (lambda (elt) (display elt)) lst)
		       (let ((ctrl-string "")
			     (args ())
			     (arg-ctr 0))
			 
			 (define* (gather-format str (arg :unset))
			   (set! ctrl-string (string-append ctrl-string str))
			   (unless (eq? arg :unset) (set! args (cons arg args))))
			 
			 (for-each
			  (lambda (d)
			    (if (or (memq larg d) 
				    (and (pair? (cdr d))
					 (pair? (cadr d))
					 (memq larg (cadr d))))
				(set! arg-ctr (+ arg-ctr 1)))
			    (gather-format (display->format d)))
			  body)
			 
			 (when (= arg-ctr 1)
			   (lint-format "perhaps ~A" name 
					(lists->string form `(format ,op ,(string-append "~{" ctrl-string "~}") ,(caddr form)))))))))
		 ))))
	
	;; ----------------
	((magnitude)
	 (if (and (= (length form) 2)
		  (memq (->type (cadr form)) '(integer? rational? real?)))
	     (lint-format "perhaps use abs here: ~A" name form)))
	
	;; ----------------
	((null eq eqv equal) ; (null (cdr...)) 
	 (if (not (var-member head env))
	     (lint-format "misspelled '~A? in ~A?" name head form)))
	
	;; ----------------
	((open-input-file open-output-file)
	 (if (and (pair? (cdr form))
		  (pair? (cddr form))
		  (string? (caddr form))
		  (not (memv (string-ref (caddr form) 0) '(#\r #\w #\a)))) ; b + then e m c x if gcc
	     (lint-format "unexpected mode: ~A" name form)))
	
	;; ----------------
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
	
	;; ----------------
	((load) ; pick up the top level declarations
	 (if (>= (length form) 2)
	     (scan form)))
	
	;; ----------------
	((values)
	 (if (= (length form) 2)
	     (lint-format "perhaps ~A" name (lists->string form (cadr form)))))
	
	;; ----------------
	((call-with-values)  ; (call/values p c) -> (c (p))
	 (if (= (length form) 3)
	     (let ((producer (cadr form))
		   (consumer (caddr form)))
	       (if (not (pair? producer))
		   (if (and (symbol? producer)
			    (not (memq (return-type producer ()) '(#t #f values))))
		       (lint-format "~A does not return multiple values" name producer)
		       (lint-format "perhaps ~A" name (lists->string form `(,consumer (,producer)))))
		   (if (eq? (car producer) 'lambda)
		       (cond ((pair? (cadr producer))
			      (lint-format "~A requires too many arguments" name (truncated-list->string producer)))
			     ((symbol? (cadr producer))
			      (lint-format "~A's parameter ~A will always be ()" name (truncated-list->string producer) (cadr producer)))
			     ((and (pair? (cddr producer))
				   (null? (cdddr producer)))
			      (let ((body (caddr producer)))
				(if (or (code-constant? body)
					(and (pair? body)
					     (symbol? (car body))
					     (not (memq (return-type (car body) ()) '(#t #f values)))))
				    (lint-format "~A does not return multiple values" name body)
				    (if (and (pair? body)
					     (eq? (car body) 'values))
					(lint-format "perhaps ~A" name (lists->string form `(,consumer ,@(cdr body))))
					(lint-format "perhaps ~A" name (lists->string form `(,consumer ,(caddr producer))))))))
			     (else (lint-format "perhaps ~A" name (lists->string form `(,consumer (,producer))))))
		       (lint-format "perhaps ~A" name (lists->string form `(,consumer (,producer)))))))))
	
	;; ----------------
	((multiple-value-bind) ; apparently no-one uses multiple-value-set!
	 ;; mvbind: (mvbind (a b) (f) (cons a b)) -> (cons (f)) 
	 (if (= (length form) 4)
	     (let ((vars (cadr form))
		   (producer (caddr form))
		   (body (cdddr form)))
	       (if (and (symbol? producer)
			(not (memq (return-type producer ()) '(#t #f values))))
		   (lint-format "~A does not return multiple values" name producer))
	       (if (and (pair? body)
			(null? (cdr body)))
		   (if (not (pair? (car body)))
		       (lint-format "perhaps ~A" name (lists->string form `((lambda ,vars ,(car body)) ,producer)))
		       (if (equal? vars (cdar body))
			   (lint-format "perhaps ~A" name (lists->string form `(,(caar body) ,producer)))))))))
	
	;; ----------------
	((eval)
	 (when (= (length form) 2)
	   (let ((arg (cadr form)))
	     (cond ((not (pair? arg))
		    (if (not (symbol? arg))
			(lint-format "perhaps ~A" name (lists->string form (cadr form)))))
		   ((eq? (car arg) 'quote)
		    (lint-format "perhaps ~A" name (lists->string form (cadadr form))))
		   ((eq? (car arg) 'string->symbol)
		    (lint-format "perhaps ~A" name (lists->string form `(eval-string ,(cadadr form)))))
		   ((and (eq? (car arg) 'read)
			 (= (length arg) 2)
			 (pair? (cadr arg))
			 (eq? (caadr arg) 'open-input-string))
		    (lint-format "perhaps ~A" name (lists->string form `(eval-string ,(cadr (cadadr form))))))))))
	
	;;((fill!) (format *stderr* "~A~%" form))
	;;   (fill! (cdr x) y) -> (fill! x y 1)?
	;;   also fill! returns y, so (fill! (list 1 2 3) y) is y
	;;   (fill! (reverse! x) #f) is not what you want
	;; but fill! almost never happens yet...
	
	;; ----------------
	((string-length)
	 (if (and (= (length form) 2)
		  (string? (cadr form)))
	     (lint-format "perhaps ~A -> ~A" name form (string-length (cadr form)))))
	
	;; ----------------
	((vector-length)
	 (if (and (= (length form) 2)
		  (vector? (cadr form)))
	     (lint-format "perhaps ~A -> ~A" name form (vector-length (cadr form)))))

	;; ----------------
	((dynamic-wind)
	 (if (= (length form) 4)
	     (let ((init (cadr form))
		   (body (caddr form))
		   (end (cadddr form))
		   (empty 0))
	       (when (and (pair? init)
			  (eq? (car init) 'lambda))
		 (if (not (null? (cadr init)))
		     (lint-format "dynamic-wind init function should be a thunk: ~A" name init))
		 (if (pair? (cddr init))
		     (let ((last-expr (list-ref init (- (length init) 1))))
		       (if (not (pair? last-expr))
			   (if (null? (cdddr init))
			       (set! empty 1))
			   (unless (side-effect? last-expr env)
			     (if (null? (cdddr init))
				 (set! empty 1))
			     (lint-format "this could be omitted: ~A in ~A" name last-expr init))))))

	       (if (and (pair? body)
			(eq? (car body) 'lambda))
		   (if (not (null? (cadr body)))
		       (lint-format "dynamic-wind body function should be a thunk: ~A" name body))
		   (set! empty 3)) ; don't try to access body below

	       (when (and (pair? end)
			  (eq? (car end) 'lambda))
		 (if (not (null? (cadr end)))
		     (lint-format "dynamic-wind end function should be a thunk: ~A" name end))
		 (if (pair? (cddr end))
		     (let ((last-expr (list-ref end (- (length end) 1))))
		       (if (not (pair? last-expr))
			   (if (null? (cdddr end))
			       (set! empty (+ empty 1)))
			   (unless (side-effect? last-expr env) ; or if no side-effects in any (also in init)
			     (if (null? (cdddr end))
				 (set! empty (+ empty 1)))
			     (lint-format "this could be omitted: ~A in ~A" name last-expr end)))
		       (if (= empty 2)
			   (lint-format "this dynamic-wind is pointless, ~A" name 
					(lists->string form (if (null? (cdddr body)) (caddr body) `(begin ,@(cddr body))))))))))))

	;; ----------------
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
				     max-stack-size stack catches exits float-format-precision bignum-precision default-rationalize-error 
				     default-random-state morally-equal-float-epsilon hash-table-float-epsilon undefined-identifier-warnings 
				     gc-stats symbol-table-locked? c-objects history-size))))
		   (lint-format "unknown *s7* field: ~A" name (cadr form))))))

	)) ; end check-special-cases
    
    
    (define (unused-parameter? x) #t)
    (define (unused-set-parameter? x) #t)
    
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
	      ((null?) "null")
	      ((length) "a sequence")
	      (else 
	       (let ((op-name (symbol->string op)))
		 (string-append (if (memv (op-name 0) '(#\a #\e #\i #\o #\u)) "an " "a ")
				(substring op-name 0 (- (length op-name) 1))))))))
      
      (define (prettify-checker op)
	(if (pair? op)
	    (string-append (prettify-checker-unq (car op)) " or " (prettify-checker (cadr op)))
	    (let ((op-name (symbol->string op)))
	      (case op
		((rational? real? complex? null?) op-name)
		((length) "sequence?")
		(else (string-append (if (memv (op-name 0) '(#\a #\e #\i #\o #\u)) "an " "a ") op-name))))))
      
      (define (report-arg-trouble name form head arg-number checker arg uop)
	(let ((op (if (and (eq? checker 'real?)
			   (eq? uop 'number?))
		      'complex?
		      uop)))
	  (if (or arg (not (eq? checker 'output-port?)))
	      (lint-format "in ~A, ~A's argument ~Ashould be ~A, but ~S is ~A"
			   name (truncated-list->string form) head 
			   (prettify-arg-number arg-number)
			   (prettify-checker-unq checker)
			   arg
			   (prettify-checker op)))))
      
      (let ((arg-number 1)
	    (flen (length (cdr form))))
	(call-with-exit
	 (lambda (done)
	   (for-each 
	    (lambda (arg)
	      (let ((checker (check-checker (if (list? checkers) (car checkers) checkers) (= arg-number flen))))
					;(format *stderr* "check-arg ~A check ~S via ~S~%" head arg checker)
		
		(define (check-arg expr)
					;(format *stderr* "check-arg ~A~%" expr)
		  (unless (symbol? expr)
		    (let ((op (->type expr)))
					;(format *stderr* "type: ~A~%" op)
		      (if (and (not (memq op '(#f #t values)))
			       (not (any-compatible? checker op)))
			  (report-arg-trouble name form head arg-number checker expr op)))))
		
		(when (or (pair? checker)
			  (symbol? checker)) ; otherwise ignore type check on this argument (#t -> anything goes)
		  (if arg
		      (if (eq? checker 'unused-parameter?)
			  (lint-format "~A's parameter ~A is not used, but a value is passed: ~S" name head arg-number arg)
			  (if (eq? checker 'unused-set-parameter?)
			      (lint-format "~A's parameter ~A's value is not used, but a value is passed: ~S" name head arg-number arg))))
		  
		  (if (pair? arg)                  ; arg is expr -- try to guess its type
		      (if (eq? (car arg) 'quote)   ; '1 -> 1
			  (let ((op (if (pair? (cadr arg)) 'list? (->type (cadr arg)))))
			    ;; arg is quoted expression
			    (if (and (not (memq op '(#f #t values)))
				     (not (any-compatible? checker op)))
				(report-arg-trouble name form head arg-number checker arg op)))
			  
			  ;; arg is an expression
			  (case (car arg)
			    
			    ((begin let let* letrec letrec* with-let)
			     (check-arg (and (pair? (cdr arg))
					     (list-ref arg (- (length arg) 1)))))
			    
			    ((quote)
			     (check-arg (cadr arg)))
			    
			    ((if)
			     (if (and (pair? (cdr arg))
				      (pair? (cddr arg)))
				 (let ((t (caddr arg))
				       (f (if (pair? (cdddr arg)) (cadddr arg) #<unspecified>)))
				   (check-arg t)
				   (when (and f (not (symbol? f)))
				     (check-arg f)))))
			    
			    ((dynamic-wind catch)
			     (if (= (length arg) 4)
				 (let ((f (caddr arg)))
				   (if (and (pair? f)
					    (eq? (car f) 'lambda))
				       (let ((len (length f)))
					 (if (> len 2)
					     (check-arg (list-ref f (- len 1)))))))))
			    
			    ((do)
			     (if (and (pair? (cdr arg))
				      (pair? (cddr arg)))
				 (let ((end+res (caddr arg)))
				   (check-arg (if (pair? (cdr end+res))
						  (list-ref (cdr end+res) (- (length end+res) 2))
						  ())))))
			    
			    ((case)
			     (for-each
			      (lambda (clause)
				(if (and (pair? clause)
					 (pair? (cdr clause)))
				    (check-arg (list-ref clause (- (length clause) 1)))))
			      (cddr arg)))
			    
			    ((cond)
			     (for-each
			      (lambda (clause)
				(check-arg (if (pair? clause)
					       (if (pair? (cdr clause))
						   (list-ref clause (- (length clause) 1))
						   (car clause))
					       clause)))
			      (cdr arg)))
			    
			    ((call/cc call-with-exit call-with-current-continuation)
			     ;; find func in body (as car of list), check its arg as return value
			     (when (and (pair? (cdr arg))
					(pair? (cadr arg))
					(eq? (caadr arg) 'lambda))
			       (let ((f (cadr arg)))
				 (when (and (pair? (cdr f))
					    (pair? (cadr f))
					    (symbol? (caadr f))
					    (null? (cdadr f)))
				   (let ((rtn (caadr f)))
				     (define (c-walk tree)
				       (if (pair? tree)
					   (if (eq? (car tree) rtn)
					       (check-arg (if (null? (cdr tree)) () (cadr tree)))
					       (begin
						 (c-walk (car tree))
						 (for-each (lambda (x) (if (pair? x) (c-walk x))) (cdr tree))))))
				     (for-each c-walk (cddr f)))))))
			    
			    ((values) 
			     (when (positive? (length arg))
			       (cond ((null? (cdr arg)) ; #<unspecified>
				      (if (not (any-checker? checker #<unspecified>))
					  (report-arg-trouble name form head arg-number checker arg 'unspecified?)))
				     ((null? (cddr arg))
				      (check-arg (cadr arg)))
				     (else
				      (for-each
				       (lambda (expr rest)
					 (check-arg expr)
					 (set! arg-number (+ arg-number 1))
					 (if (> arg-number max-arity) (done))
					 (if (list? checkers)
					     (if (null? (cdr checkers))
						 (done)
						 (set! checkers (cdr checkers)))))
				       (cdr arg) (cddr arg))
				      (check-arg (list-ref arg (- (length arg) 1)))))))
			    
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
				   (report-arg-trouble name form head arg-number checker arg op))))))
		      
		      ;; arg is not a pair
		      (if (and (not (symbol? arg))
			       (not (any-checker? checker arg)))
			  (let ((op (->type arg)))
			    (unless (memq op '(#f #t values))
			      (report-arg-trouble name form head arg-number checker arg op))))))
		
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
				  (arity (fdata 'decl))))
			(sig (fdata 'signature)))
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
							  (eq? a (if (pair? b) (car b) b)))))
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
			  
			  (when (and (pair? sig)
				     (pair? (cdr sig)))
			    (check-args name head form (cdr sig) env opt))
			  
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
								     (eq? (keyword->symbol a) (if (pair? b) (car b) b))))))
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
				      (lint-format "it looks odd to have repeated arguments in ~A" name (truncated-list->string form))))
			      
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
				  (if (pair? arg-data)
				      (check-args name head form arg-data env max-arity))
				  ))))))))))))
    
    (define (get-generator form) ; defgenerator funcs
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
							   :decl (dummy-func head form (list head '_ (caddr form) #f))
							   :arglist (caddr form)
							   :value form
							   :env ())))
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
						       :decl (dummy-func head form (list 'define (cons '_ (cdadr form)) #f))
						       :arglist (cdr name)
						       :value form
						       :env ())))
		       (lint-format "what is this? ~A" name form)))
		 (hash-table-set! globals name (make-var :name name 
							 :value (and (pair? (cddr form)) (caddr form)))))))
	  
	  ((define* define-expansion define-macro define-macro* define-bacro define-bacro* definstrument defanimal)
	   (hash-table-set! globals (caadr form) (make-fvar :name (caadr form) 
							    :ftype head
							    :decl (dummy-func head form (list head (cons '_ (cdadr form)) #f))
							    :arglist (cdadr form)
							    :value form
							    :env ())))
	  
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
			 name head type (if (pair? (cdr set)) "s" "") (reverse set)))
	(if (pair? unused)
	    (lint-format "~A ~A~A ~{~A~^, ~} not used" 
			 name head type (if (pair? (cdr unused)) "s" "") (reverse unused)))))
    
    (define (lint-walk-body name head body env)
      ;; walk a body (a list of forms, the value of the last of which might be returned)
					;(format *stderr* "lint-walk-body ~A~%" body)
      
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
		      (cond ((or (not (pair? arg2))          ; (set! x 0) (set! x 1) -> "this could be omitted: (set! x 0)"
				 (not (tree-member (cadr f) arg2)))
			     (if (and (not (side-effect? arg1 env))
				      (not (side-effect? arg2 env)))
				 (lint-format "this could be omitted: ~A" name prev-f)))
			    
			    ((and (pair? arg1)               ; (set! x (cons 1 z)) (set! x (cons 2 x)) -> (set! x (cons 2 (cons 1 z)))
				  (pair? arg2)
				  (eq? (car arg1) 'cons)
				  (eq? (car arg2) 'cons)
				  (eq? (cadr f) (caddr arg2))
				  (not (eq? (cadr f) (cadr arg2))))
			     (lint-format "perhaps ~A ~A -> ~A" name
					  prev-f f
					  `(set! ,(cadr f) (cons ,(cadr arg2) (cons ,@(cdr arg1))))))
			    
			    ((and (= (tree-count1 (cadr f) arg2 0) 1) ; (set! x y) (set! x (+ x 1)) -> (set! x (+ y 1))
				  (or (not (pair? arg1))
				      (< (tree-length arg1 0) 5)))
			     (lint-format "perhaps ~A ~A ->~%~NC~A" name prev-f f 4 #\space
					  (object->string `(set! ,(cadr f) ,(tree-subst arg1 (cadr f) arg2))))))))
		
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
			  (lint-format "this could be omitted: ~A" name (truncated-list->string f))

			  (if (pair? f)
			      (if (eq? (car f) 'do) ; other syntactic forms almost never happen in this context
				  (let ((returned (if (and (pair? (cdr f))
							   (pair? (cddr f)))
						      (let ((end+res (caddr f)))
							(if (pair? (cdr end+res))
							    (list-ref (cdr end+res) (- (length end+res) 2)))))))
				    (if (and (not (eq? returned #<unspecified>))
					     (or (not (pair? returned))
						 (not (side-effect? returned env))))
					(lint-format "~A: result ~A is not used" name 
						     (truncated-list->string f) 
						     (truncated-list->string returned))))
				  (if (and (eq? (car f) 'format)
					   (pair? (cdr f))
					   (eq? (cadr f) #t))
				      (lint-format "perhaps use () with format since the string value is discarded:~%    ~A" name `(format () ,@(cddr f))))))))
		    
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
			  (op (write-port (car dpy-f)))
			  (exprs (make-list (if dpy-case (- ctr dpy-start -1) (- ctr dpy-start)) ())))
		      
		      (define* (gather-format str (arg :unset))
			(set! ctrl-string (string-append ctrl-string str))
			(unless (eq? arg :unset) (set! args (cons arg args))))
		      
		      (call-with-exit
		       (lambda (done)
			 (for-each
			  (lambda (d)
			    (if (not (equal? (write-port d) op)) 
				(begin 
				  (lint-format "unexpected port change: ~A -> ~A in ~A~%" name op (write-port d) d) ; ??
				  (done)))
			    (list-set! exprs dctr d)
			    (set! dctr (+ dctr 1))
			    (gather-format (display->format d))
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
    
    
    (define (lint-walk-function-body name head body env)
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
		      (if (or (procedure? p)
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
			  (lint-format "~A~A could be (define ~A ~A)" name 
				       (if (and (procedure? p)
						(not (= (car ary) (cdr ary)))
						(not (= (length args) (cdr ary))))
					   (format #f "leaving aside ~A's optional arg~P, " cval (- (cdr ary) (length args)))
					   "")
				       name name cval)))
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
		       (make-fvar :name (if (not (memq head '(lambda lambda*))) name lambda-marker)
				  :ftype head
				  :value form
				  :env env
				  :arglist (if (memq head '(lambda lambda*))
					       (cadr form)
					       (if (memq head '(defmacro defmacro*))
						   (caddr form)
						   (cdadr form)))))))
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
	      (lint-walk-function-body name head val env)
	      (if data
		  (cons data env)
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
		  
		  (lint-walk-function-body name head val (append arg-data (if data (cons data env) env)))
		  (if *report-unused-parameters* 
		      (report-usage name 'parameter head arg-data))
		  
		  (when (and (var? data)
			     (memq head '(define lambda define-macro))
			     (proper-list? args)) ; this could be extended
		    ;; look for unused parameters that are passed a value other than #f (ignore rest args either way)
		    (let ((set ())
			  (unused ()))
		      (for-each 
		       (lambda (arg)
			 (if (and (not (var-ref arg))
				  (not (hash-table-ref other-identifiers (var-name arg))))
			     (if (var-set arg)
				 (set! set (cons (var-name arg) set))
				 (if (not (memq (var-name arg) '(documentation signature iterator?)))
				     (set! unused (cons (var-name arg) unused))))))
		       arg-data)
		      (when (or (pair? set)
				(pair? unused))
			(let ((sig (var-signature data))
			      (len (+ (length args) 1)))
			  (if (not sig)
			      (set! sig (make-list len #t))
			      (if (< (length sig) len)
				  (set! sig (copy sig (make-list len #t)))))
			  (let ((siglist (cdr sig)))
			    (for-each
			     (lambda (arg)
			       (if (memq arg unused)
				   (set-car! siglist 'unused-parameter?)
				   (if (memq arg set)
				       (set-car! siglist 'unused-set-parameter?)))
			       (set! siglist (cdr siglist)))
			     args))
			  (set! (var-signature data) sig)))))
		  
		  (if data 
		      (cons data env)
		      env))
		
		(begin
		  (lint-format "strange ~A parameter list ~A" name head args)
		  env)))))
    
    
    (define (check-bool-cond name form c1 c2 env)
      ;; (cond (x #f) (#t #t)) -> (not x)
      ;; c1/c2 = possibly combined, so in (cond (x #t) (y #t) (else #f)), c1: ((or x y) #t), so -> (or x y)
      (and (pair? c1) 
	   (= (length c1) 2) 
	   (pair? c2) 
	   (pair? (cdr c2))
	   (memq (car c2) '(#t else))
	   (or (and (boolean? (cadr c1))
		    (or (and (null? (cddr c2))
			     (boolean? (cadr c2))
			     (not (equal? (cadr c1) (cadr c2))) ; handled elsewhere
			     (lint-format "perhaps ~A" name 
					  (lists->string form (if (eq? (cadr c1) #t) 
								  (car c1)
								  (simplify-boolean `(not ,(car c1)) () () env)))))
			(and (not (cadr c1))   ; (cond (x #f) (else y)) -> (and (not x) y)
			     (let ((cc1 (simplify-boolean `(not ,(car c1)) () () env)))
			       (lint-format "perhaps ~A" name 
					    (lists->string form 
							   (if (null? (cddr c2)) 
							       `(and ,cc1 ,(cadr c2))
							       `(and ,cc1 (begin ,@(cdr c2))))))))
			(and (pair? (car c1))  ; (cond ((null? x) #t) (else y)) -> (or (null? x) y)
			     (eq? (return-type (caar c1) env) 'boolean?)
			     (lint-format "perhaps ~A" name
					  (lists->string form 
							 (if (null? (cddr c2)) 
							     `(or ,(car c1) ,(cadr c2))
							     `(or ,(car c1) (begin ,@(cdr c2)))))))))
	       (and (boolean? (cadr c2))
		    (null? (cddr c2))
		    (not (equal? (cadr c1) (cadr c2)))
		    (lint-format "perhaps ~A" name
				 (lists->string form
						(if (not (cadr c2))
						    (if (and (pair? (car c1))
							     (eq? (caar c1) 'and))
							(append (car c1) (cdr c1))
							`(and ,@c1))
						    `(or (not ,(car c1)) ,(cadr c1)))))))))
    
    (define (cond->case eqv-select new-clauses)
      `(case ,eqv-select 
	 ,@(map (lambda (clause)
		  (let ((test (car clause))
			(exprs (cdr clause)))
		    
		    (if (null? exprs)                   ; cond returns the test result if no explicit results
			(set! exprs (list #t)))         ;   but all tests here return a boolean, and we win only if #t?? (memx is an exception)

		    (if (memq test '(else #t))
			`(else ,@exprs)
			
			(case (car test)
			  ((eq? eqv? = equal? char=?)
			   (if (equal? eqv-select (cadr test))
			       `((,(unquoted (caddr test))) ,@exprs)
			       `((,(unquoted (cadr test))) ,@exprs)))
			  
			  ((memq memv member)
			   `(,(unquoted (caddr test)) ,@exprs))
			  
			  ((not)
			   `((#f) ,@exprs))
			  
			  ((null?)
			   `((()) ,@exprs))
			  
			  ((eof-object?)
			   `((#<eof>) ,@exprs))
			  
			  ((zero?)
			   `((0 0.0) ,@exprs))
			  
			  ((boolean?)
			   `((#t #f) ,@exprs))
			  
			  ((char-ci=?)
			   (if (equal? eqv-select (cadr test))
			       `(,(list (caddr test) (other-case (caddr test))) ,@exprs)
			       `(,(list (cadr test) (other-case (cadr test))) ,@exprs)))
			  
			  (else 
			   `(,(map (lambda (p)
				     (case (car p)
				       ((eq? eqv? = equal? char=?)  
					(unquoted (if (equal? eqv-select (cadr p)) (caddr p) (cadr p))))
				       ((memq memv member) (apply values (caddr p)))
				       ((not)              #f)
				       ((null?)            ())
				       ((eof-object?)      #<eof>)
				       ((zero?)            (values 0 0.0))
				       ((boolean?)         (values #t #f))
				       ((char-ci=?)   
					(if (equal? eqv-select (cadr p)) 
					    (values (caddr p) (other-case (caddr p)))
					    (values (cadr p) (other-case (cadr p)))))
				       (else               (error "oops"))))
				   (cdr test))
			     ,@exprs))))))
		new-clauses)))
    
    (define (cond-eqv-select clause)
      (if (pair? clause)
	  (case (car clause)
	    ((memq memv member) 
	     (and (= (length clause) 3)
		  (cadr clause)))
	    ((eq? eqv? = equal? char=? char-ci=?)
	     (and (= (length clause) 3)
		  (if (code-constant? (cadr clause))
		      (caddr clause)
		      (cadr clause))))
	    ((or) 
	     (and (pair? (cdr clause))
		  (cond-eqv-select (cadr clause))))
	    ((not null? eof-object? zero? boolean?)
	     (and (pair? (cdr clause))
		  (cadr clause)))
	    (else #f))
	  (memq clause '(else #t))))
    
    (define (cond-eqv? clause eqv-select or-ok)
      (if (pair? clause)
	  ;; it's eqv-able either directly or via memq/memv, or via (or ... eqv-able clauses)
	  ;;   all clauses involve the same (eventual case) selector
	  (case (car clause)
	    ((eq? eqv? = equal? char=? char-ci=?)
	     (if (eqv-code-constant? (cadr clause))
		 (equal? eqv-select (caddr clause))
		 (and (eqv-code-constant? (caddr clause))
		      (equal? eqv-select (cadr clause)))))
	    
	    ((memq memv member)
	     (and (equal? eqv-select (cadr clause))
		  (pair? (caddr clause))
		  (eq? (caaddr clause) 'quote)
		  (or (not (eq? (car clause) 'member))
		      (every? (lambda (x)
				(or (number? x)
				    (char? x)
				    (symbol? x)
				    (memq x '(#t #f () #<unspecified> #<undefined> #<eof>))))
			      (cdr (caddr clause))))))
	    ((or)
	     (and or-ok
		  (every? (lambda (p)
			    (cond-eqv? p eqv-select #f))
			  (cdr clause))))

	    ((not null? eof-object? zero? boolean?)
	     (equal? eqv-select (cadr clause)))
	    
	    (else #f))
	  (memq clause '(else #t))))
    
    
    (define (lint-walk name form env)
      ;; walk a form, here curlet can change
      ;(format *stderr* "walk ~A~%" form)
      
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
					      (eq? (var-name (car e)) lambda-marker)) ; (define x (lambda ...)) but it misses closures
					 (set! (var-name (car e)) sym)
					 (cons (make-var :name sym) env)))
				   (cons (make-var :name sym) env)))
			     
			     ;; not (symbol? sym)
			     (if (and (pair? sym)
				      (pair? val)
				      (not (pair? (car sym))))
				 (begin
					;(format *stderr* "sym: ~A, val: ~A~%" sym val)
				   (when (pair? (cdr sym))
				     (if (repeated-member? (proper-list (cdr sym)) env)
					 (lint-format "~A parameter is repeated: ~A" name head (truncated-list->string sym)))
				     
				     (cond ((memq head '(define* define-macro* define-bacro*))
					    (check-star-parameters (car sym) (cdr sym)))
					   ((list-any? keyword? (cdr sym))
					    (lint-format "~A parameter can't be a keyword: ~A" name (car sym) sym))
					   ((memq 'pi (cdr sym))
					    (lint-format "~A parameter can't be a constant: ~A" name (car sym) sym))))
				   
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
					   (check-star-parameters head args)
					   (if (list-any? keyword? args) ; (lambda (:key) ...)
					       (lint-format "lambda arglist can't handle keywords (use lambda*)" name)))))
				 
				 (if (and (eq? head 'lambda)           ; (lambda () (f)) -> f, (lambda (a b) (f a b)) -> f
					  (not (eq? name 'case-lambda))    
					  (= len 3)
					  (>= arglen 0)) ; not a dotted list
				     (let ((body (caddr form)))
				       (when (and (pair? body)
						  (symbol? (car body))
						  (not (memq (car body) '(and or))))
					 (if (equal? args (cdr body))
					     (lint-format "perhaps ~A" name (lists->string form (car body)))
					     (if (equal? (reverse args) (cdr body))
						 (let ((rf (reversed (car body))))
						   (if rf (lint-format "perhaps ~A" name (lists->string form rf))))))))))
				 
			       (if (and (symbol? args)                 ; (lambda args (apply f args)) -> f
					(eq? head 'lambda)
					(not (eq? name 'case-lambda))    
					(= len 3))
				   (let ((body (caddr form)))
				     (if (and (pair? body)
					      (= (length body) 3)
					      (eq? (car body) 'apply)
					      (symbol? (cadr body))
					      (not (memq (cadr body) '(and or)))
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
					  (if (> len 2) "many" "few") 
					  form)
			     (let ((arg (cadr form)))
			       (if (and (< quote-warnings 20)
					(or (number? arg)
					    (boolean? arg)
					    (string? arg)
					    (char? arg)
					    (vector? arg)
					    (null? arg)
					    (memq arg '(#<unspecified> #<undefined> #<eof>))))
				   (begin
				     (set! quote-warnings (+ quote-warnings 1))
				     (lint-format "quote is not needed here: ~A~A" 
						  name (truncated-list->string form)
						  (if (= quote-warnings 20) "; will ignore this error henceforth." ""))))))))
		   env)
		  
		  ((if)
		   ;; ---------------- if ----------------
		   (let ((len (length form))
			 (suggestion made-suggestion))
		     (if (> len 4)
			 (lint-format "if has too many clauses: ~A" name form)
			 (if (< len 3)
			     (lint-format "if has too few clauses: ~A" name form)
			     (let ((test (cadr form))
				   (true (caddr form))
				   (false (if (= len 4) (cadddr form) 'no-false)))
			       
			       (define (differ-in-one p q)
				 (and (pair? p)
				      (pair? q)
				      (if (equal? (car p) (car q))
					  (differ-in-one (cdr p) (cdr q))
					  (and (not (pair? (car p)))
					       (not (pair? (car q)))
					       (equal? (cdr p) (cdr q))
					       (list p (list (car p) (car q)))))))
			       
			       (define (differ-in-one-seq p q c)
				 (and (pair? p)
				      (pair? q)
				      (if (equal? (car p) (car q))
					  (differ-in-one-seq (cdr p) (cdr q) (+ c 1))
					  (and (> c 1)
					       (equal? (cdr p) (cdr q))
					       (list p (list (car p) (car q)))))))
			       
			       (define (tree-subst-eq new old tree) 
				 ;; tree-subst above substitutes every occurence of 'old with 'new, so we check
				 ;;   in advance that 'old only occurs once in the tree (via tree-count1).  Here
				 ;;   'old may occur any number of times, but we only want to change it once,
				 ;;   so we keep the actual pointer to it and use eq?.
				 (if (eq? old tree)
				     (cons new (cdr tree))
				     (if (pair? tree)
					 (if (eq? (car tree) 'quote)
					     (copy-tree tree)
					     (cons (tree-subst-eq new old (car tree))
						   (tree-subst-eq new old (cdr tree))))
					 tree)))
			       
			       (when (and (pair? true)
					  (pair? false)
					  (not (eq? (car true) 'quote))
					  (not (any-macro? (car true) env))
					  (pair? (cdr true)))
				 (let ((diff (differ-in-one true false)))
					;(format *stderr* "diff: ~A, ~A ~A~%" diff true false)
				   (if (pair? diff)
				       (if (and (not (equal? (car true) (caadr diff))) ; (if z (+ x y) (- x y))? 
						(or (not (eq? (car true) 'set!))       ; (if x (set! y w) (set! z w))
						    (not (equal? (caar diff) (cadr true)))))
					   (lint-format "perhaps ~A" name 
							(lists->string form
								       (cond ((and (eq? (caadr diff) #t)
										   (not (cadadr diff)))
									      (tree-subst-eq test (car diff) true))
									     
									     ((and (not (caadr diff))
										   (eq? (cadadr diff) #t))
									      (tree-subst-eq `(not ,test) (car diff) true))
									     
									     ((equal? (caadr diff) test)
									      (tree-subst-eq `(or ,@(cadr diff)) (car diff) true))
									     
									     (else (tree-subst-eq `(if ,test ,@(cadr diff)) (car diff) true))))))
				       
				       ;; not sure about this -- in simple cases it looks good
				       ;;   some cases are trying to remove a test from a loop, so our suggestion will be unwelcome
				       (if (not (memq (car true) '(do lambda)))
					   (let ((seqdiff (differ-in-one-seq true false 0)))
					;(format *stderr* "seqdiff: ~A ~A ~A~%" seqdiff true false)
					     ;; cadr replacement is too messy, looks good about 1 in 10 times
					     (if (and (pair? seqdiff)
						      (< (tree-length (cadr seqdiff) 0) 20)) ; 100 is too big, 30 is ok perhaps
						 (lint-format "perhaps ~A" name
							      (lists->string form (tree-subst-eq `(if ,test ,@(cadr seqdiff)) (car seqdiff) true)))))))))
			       
			       (unless (= last-if-line-number line-number)
				 (do ((iff form (cadddr iff))
				      (iffs 0 (+ iffs 1)))
				     ((or (> iffs 2)
					  (not (pair? iff))
					  (not (= (length iff) 4))
					  (not (eq? (car iff) 'if)))
				      (when (or (> iffs 2)
						(and (= iffs 2)
						     (pair? iff)
						     (= (length iff) 3)
						     (eq? (car iff) 'if)))
					(define (unbegin x)
					  (if (and (pair? x)
						   (eq? (car x) 'begin))
					      (cdr x)
					      (list x)))
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
											      `((,(cadr iff) ,@(unbegin (caddr iff))))
											      `((else ,@(unbegin iff))))))
									       (set! clauses (cons (cons (cadr iff) (unbegin (caddr iff))) clauses))))))))))
			       
			       (if (never-false test)
				   (lint-format "if test is never false: ~A" name (truncated-list->string form))
				   (if (and (never-true test) true) ; complain about (if #f #f) later
				       (lint-format "if test is never true: ~A" name (truncated-list->string form))))
			       
			       (let ((expr (simplify-boolean test () () env)))
				 (if (not (side-effect? test env))
				     (cond ((or (equal? test true)               ; (if x x y) -> (or x y)
						(equal? expr true))
					    (lint-format "perhaps ~A" name 
							 (lists->string form 
									(if (eq? false 'no-false)
									    (simplify-boolean `(or ,expr #<unspecified>) () () env)
									    (simplify-boolean `(or ,expr ,false) () () env)))))
					   ((or (equal? test `(not ,true))       ; (if x (not x) y) -> (and (not x) y)
						(equal? `(not ,test) true))      ; (if (not x) x y) -> (and x y)
					    (lint-format "perhaps ~A" name 
							 (lists->string form 
									(if (eq? false 'no-false)
									    (simplify-boolean `(and ,true #<unspecified>) () () env)
									    (simplify-boolean `(and ,true ,false) () () env)))))
					   ((or (equal? test false)              ; (if x y x) -> (and x y)
						(equal? expr false))
					    (lint-format "perhaps ~A" name 
							 (lists->string form (simplify-boolean `(and ,expr ,true) () () env))))
					   ((or (equal? `(not ,test) false)      ; (if x y (not x)) -> (or (not x) y)
						(equal? test `(not ,false)))     ; (if (not x) y x) -> (or x y)
					    (lint-format "perhaps ~A" name 
							 (lists->string form (simplify-boolean `(or ,false ,true) () () env))))
					   
					   ((and (= len 4)
						 (pair? true)
						 (eq? (car true) 'if)
						 (= (length true) 4))
					    (if (equal? false (cadddr true))
						(if (not false)
						    (lint-format "perhaps ~A" name 
								 (lists->string form `(and ,expr ,(cadr true) ,(caddr true))))
						    (lint-format "perhaps ~A" name 
								 (lists->string form `(if (and ,expr ,(cadr true)) ,(caddr true) ,false))))
						(if (equal? false (caddr true))
						    (if (not false)
							(lint-format "perhaps ~A" name 
								     (lists->string form `(and ,expr (not ,(cadr true)) ,(cadddr true))))
							(lint-format "perhaps ~A" name 
								     (lists->string form `(if (and ,expr (not ,(cadr true))) ,(cadddr true) ,false)))))))
					   ((and (= len 4)
						 (pair? false)
						 (eq? (car false) 'if)
						 (= (length false) 4))
					    (if (equal? true (caddr false))
						(if (not true)
						    (lint-format "perhaps ~A" name 
								 (lists->string form `(and (not (or ,expr ,(cadr false))) ,(cadddr false))))
						    (lint-format "perhaps ~A" name 
								 (lists->string form `(if (or ,expr ,(cadr false)) ,true ,(cadddr false)))))
						(if (equal? true (cadddr false))
						    (if (not true)
							(lint-format "perhaps ~A" name 
								     (lists->string form `(and (not (or ,expr (not ,(cadr false)))) ,(caddr false))))
							(lint-format "perhaps ~A" name 
								     (lists->string form `(if (or ,expr (not ,(cadr false))) ,true ,(caddr false))))))))
					   ))
				 
				 (if (pair? false)
				     (begin
				       (if (and (eq? (car false) 'if)   ; (if x 3 (if (not x) 4)) -> (if x 3 4)
						(pair? (cdr false))
						(pair? (cadr false))
						(eq? (caadr false) 'not)
						(or (equal? test (cadr (cadr false))) (equal? expr (cadr (cadr false))))
						(not (side-effect? test env)))
					   (lint-format "pointless repetition of if test: ~A" name (lists->string form `(if ,expr ,true ,(caddr false)))))
				       
				       (if (and (eq? (car false) 'if)   ; (if test0 expr (if test1 expr)) -> if (or test0 test1) expr) 
						(null? (cdddr false))   ; other case is dealt with above
						(equal? true (caddr false)))
					   (let ((test1 (simplify-boolean `(or ,expr ,(cadr false)) () () env)))
					     (lint-format "perhaps ~A" name (lists->string form `(if ,test1 ,true ,@(cdddr false)))))))
				     
				     (if (and (eq? false 'no-false)                         ; no false branch
					      (pair? true))
					 (begin
					   (if (pair? test)  
					       (let ((test-op (car test))
						     (true-op (car true)))
						 (if (and (eq? test-op 'pair?)             ; (if (pair? lst) (for-each f lst)) -> (for-each f lst)
							  (memq true-op '(map for-each))
							  (eq? (cadr test) (caddr true)))
						     (lint-format "perhaps ~A" name (lists->string form true))
						     
						     ;; the min+max case is seldom hit, and takes about 50 lines
						     (if (and (memq test-op '(< > <= >=))  ; (if (< x y) (set! x y) -> (set! x (max x y))
							      (eq? true-op 'set!)
							      (null? (cdddr test))
							      (memq (cadr true) test)
							      (member (caddr true) test))
							 (let* ((target (cadr true))
								(f (if (memq test-op '(< <=))
								       (if (eq? target (cadr test)) 'max 'min)
								       (if (eq? target (caddr test)) 'max 'min))))
							   (lint-format "perhaps ~A" name
									(lists->string form `(set! ,target (,f ,@(cdr true))))))))))
					   
					   (if (and (eq? (car true) 'if) ; (if test0 (if test1 expr)) -> (if (and test0 test1) expr)
						    (null? (cdddr true)))
					       (let ((test1 (simplify-boolean `(and ,expr ,(cadr true)) () () env)))
						 (lint-format "perhaps ~A" name (lists->string form `(if ,test1 ,(caddr true))))))
					   )))
				 
				 (if (and (pair? test)
					  (memq (car test) '(< <= > >= =))       ; (if (< x y) x y) -> (min x y)
					  (null? (cdddr test))
					  (member false test)
					  (member true test))
				     (if (eq? (car test) '=)                     ; (if (= x y) y x) -> y [this never happens]
					 (lint-format "perhaps ~A" name (lists->string form false))
					 (let ((f (if (memq (car test) '(< <=))
						      (if (equal? (cadr test) true) 'min 'max)
						      (if (equal? (cadr test) false) 'min 'max))))
					   (lint-format "perhaps ~A" name (lists->string form `(,f ,true ,false))))))
				 
				 (cond ((eq? expr #t)
					(lint-format "perhaps ~A" name (lists->string form true)))
				       
				       ((not expr)
					(if (eq? false 'no-false)
					    (if true                             ; (if #f x) as a kludgey #<unspecified>
						(lint-format "perhaps ~A" name (lists->string form #<unspecified>)))
					    (lint-format "perhaps ~A" name (lists->string form false))))
				       
				       ((not (equal? true false))
					(if (boolean? true)
					    (if (boolean? false) ; !  (if expr #t #f) turned into something less verbose
						(lint-format "perhaps ~A" name 
							     (lists->string form (if true 
										     expr 
										     (simplify-boolean `(not ,expr) () () env))))
						(when (= suggestion made-suggestion)
						  (lint-format "perhaps ~A" name 
							       (lists->string form (if true
										       (if (eq? false 'no-false)
											   expr
											   (simplify-boolean `(or ,expr ,false) () () env))
										       (simplify-boolean 
											(if (eq? false 'no-false)
											    `(not ,expr)
											    `(and (not ,expr) ,false))
											() () env))))))
					    (if (and (boolean? false)
						     (= suggestion made-suggestion))
						(lint-format "perhaps ~A" name 
							     (lists->string form (simplify-boolean
										  (if false 
										      (if (and (pair? expr) (eq? (car expr) 'not))
											  `(or ,(cadr expr) ,true) 
											  `(or (not ,expr) ,true))
										      `(and ,expr ,true))
										  () () env))))))
				       ((= len 4)
					(if (not (side-effect? test env))
					    (lint-format "if is not needed here: ~A" name (lists->string form true))
					    (lint-format "if is not needed here: ~A" name (lists->string form `(begin ,expr ,true)))))))
			       
			       (if (and (pair? test)
					(pair? true)
					(pair? (cdr true))
					(null? (cddr true))
					(equal? test (cadr true)))
				   (lint-format "perhaps ~A" name
						(lists->string form 
							       (if (eq? false 'no-false)
								   `(cond (,test => ,(car true)))
								   `(cond (,test => ,(car true)) (else ,false))))))
			       (lint-walk name test env)
			       (set! env (lint-walk name true env))
			       (if (= len 4) (set! env (lint-walk name false env))))))
		     env))
		  
		  ((when unless)
		   ;; -------- when, unless --------
		   (if (< (length form) 3)
		       (begin
			 (lint-format "~A is messed up: ~A" name head (truncated-list->string form))
			 env)
		       (let ((test (cadr form)))
			 (if (and (pair? test)
				  (eq? (car test) 'not))
			     (lint-format "perhaps ~A -> ~A"
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
			      
			      (if (not (pair? clause))
				  (begin
				    (set! all-eqv #f)
				    (set! has-combinations #f)
				    (lint-format "cond clause ~A in ~A is not a pair?" name clause (truncated-list->string form)))
				  (begin
				    
				    (when all-eqv
				      (unless eqv-select
					(set! eqv-select (cond-eqv-select (car clause))))
				      (set! all-eqv (and eqv-select
							 (or (not (pair? (cdr clause)))
							     (not (eq? (cadr clause) '=>))) ; case sends selector, but cond sends test result
							 (cond-eqv? (car clause) eqv-select #t))))
				    
				    (if (and prev-clause
					     (not has-combinations)
					     (> len 2) 
					     (equal? (cdr clause) (cdr prev-clause)))
					(if (memq (car clause) '(else #t))        ; (cond ... (x z) (else z)) -> (cond ... (else z))
					    (unless (side-effect? (car prev-clause) env)
					      (lint-format "this clause could be omitted: ~A" name prev-clause))
					    (set! has-combinations #t)))          ; handle these later
				    (set! prev-clause clause)
				    
				    (let ((expr (simplify-boolean (car clause) () () env))
					  (test (car clause))
					  (sequel (cdr clause)))
				      
				      (if (memq test '(else #t))
					  (begin
					    (set! has-else #t)
					    (if (and (pair? sequel)
						     (pair? (car sequel))
						     (null? (cdr sequel))
						     (eq? (caar sequel) 'cond))
						(lint-format "else clause cond could be folded into the outer cond: ~A" name (truncated-list->string clause))))
					  (if (and (eq? test 't)
						   (= ctr len)
						   (not (var-member 't env))
						   (not (hash-table-ref globals 't)))
					      (lint-format "odd cond clause test: is 't supposed to be #t? ~A in ~A~%" name clause (truncated-list->string form))))
				      
				      (if (never-false expr)
					  (if (not (= ctr len))
					      (lint-format "cond test ~A is never false: ~A" name (car clause) (truncated-list->string form))
					      (if (and (not (memq expr '(#t else)))
						       (not (side-effect? test env)))
						  (lint-format "cond last test could be #t: ~A" name form)))
					  (if (never-true expr)
					      (lint-format "cond test ~A is never true: ~A" name (car clause) (truncated-list->string form))))
				      
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
				      
				      (if (and (symbol? expr)
					       (not (var-member expr env))
					       (not (hash-table-ref globals expr))
					       (procedure? (symbol->value expr *e*)))
					  (lint-format "strange cond test: ~A in ~A is a procedure" name expr clause))
				      
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
							  (if (and (procedure? val)
								   (not (aritable? val 1))) ; here values might be in test expr
							      (lint-format "=> target (~A) may be unhappy: ~A" name f clause)))
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
					  (set! result :unequal))))))
			    (cdr form))
			   
			   (if has-else 
			       (if (pair? result) ; all result clauses are the same (and not implicit)
				   (lint-format "perhaps ~A" name (lists->string form 
										 (if (null? (cdr result))
										     (car result)
										     `(begin ,@result)))))
			       (let* ((last-clause (and (> len 1)
							(list-ref form len)))
				      (clen (and (pair? last-clause)
						 (length last-clause)))
				      (last-res (and (number? clen)
						     (> clen 1) 
						     (list-ref last-clause (- clen 1)))))
				 (if (and (pair? last-res)
					  (memq (car last-res) '(#t else)))
				     (lint-format "perhaps cond else clause is misplaced: ~A in ~A" name last-res last-clause))))
			   
			   (if (= len 2)
			       (check-bool-cond name form (cadr form) (caddr form) env))
			   
			   (when has-combinations
			     (let ((new-clauses ())
				   (current-clauses ()))
			       (do ((clauses (cdr form) (cdr clauses)))
				   ((null? clauses)
				    (let ((len (length new-clauses)))
				      ;; len is very rarely 1
				      (if (or (not (= len 2))
					      (not (check-bool-cond name form (cadr new-clauses) (car new-clauses) env)))
					  (lint-format "perhaps ~A" name 
						       (lists->string form (if (not all-eqv)
									       `(cond ,@(reverse new-clauses))
									       (cond->case eqv-select (reverse new-clauses))))))
				      (set! all-eqv #f)))
				 
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
				      (> len (if has-else 2 1))) ; (cond (x y)) -- kinda dumb, but (if x y) isn't much shorter
			     (lint-format "perhaps use case instead of cond: ~A" name
					  (lists->string form (cond->case eqv-select (cdr form)))))))
		     env))
		  
		  ((case)
		   ;; ---------------- case ----------------		  
		   ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		   ;; also unlike cond, only 'else marks a default branch (not #t)
		   
		   (if (< (length form) 3)
		       (lint-format "case is messed up: ~A" name (truncated-list->string form))
		       (let ((sel-type #t)
			     (selector (cadr form)))
			 
			 (let ((clauses (cddr form)))            ; (case x ((a) #t) (else #f)) -> (eq? x 'a) -- this stuff actually happens!
			   (if (and (= (length clauses) 2)
				    (pair? (car clauses))
				    (pair? (cadr clauses))
				    (eq? (caadr clauses) 'else)
				    (pair? (cdar clauses))
				    (pair? (cdadr clauses))
				    (null? (cddar clauses))
				    (null? (cddadr clauses))
				    (memq (cadar clauses) '(#f #t))
				    (memq (cadadr clauses) '(#f #t))
				    (not (eq? (cadadr clauses) (cadar clauses))))
			       (let* ((keys (length (caar clauses)))
				      (keylist (if (= keys 1) (caaar clauses) (caar clauses)))
				      (quoted (or (> keys 1) (symbol? keylist)))
				      (op (if (every? symbol? (caar clauses))
					      (if (= keys 1) 'eq? 'memq)
					      (if (= keys 1) 'eqv? 'memv))))
				 ;; can't use '= or 'char=? here because the selector may return anything
				 (lint-format "perhaps ~A" name 
					      (lists->string form (if (cadadr clauses)
								      (if quoted
									  `(not (,op ,selector ',keylist))
									  `(not (,op ,selector ,keylist)))
								      (if quoted
									  `(,op ,selector ',keylist)
									  `(,op ,selector ,keylist))))))))

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
				 (lint-format "perhaps ~A" name (lists->string form 
									       (if (null? (cdr result))
										   (car result)
										   `(begin ,@result))))
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
			       
			       (if (null? new-keys-and-exprs)
				   (if (or (null? else-clause)    ; can this happen? (it's caught above as an error)
					   (null? (cdr else-clause)))
				       (lint-format "perhaps ~A" name (lists->string form ()))
				       (if (null? (cddr else-clause))
					   (lint-format "perhaps ~A" name (lists->string form (cadr else-clause)))
					   (lint-format "perhaps ~A" name (lists->string form `(begin ,@(cdr else-clause))))))
				   (begin
				     ;; (null? (cdr new-keys-and-exprs)) is rare and kinda dumb -- cases look like test suite entries
				     (for-each 
				      (lambda (clause)
					(if (and (pair? (car clause))
						 (pair? (cdar clause)))
					    (if (every? integer? (car clause))
						(set-car! clause (sort! (car clause) <))
						(if (every? char? (car clause))
						    (set-car! clause (sort! (car clause) char<?))))))
				      new-keys-and-exprs)
				     (lint-format "perhaps ~A" name 
						  (lists->string form 
								 (if (pair? else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs) ,else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs))))))))))))
		   env)
		  
		  ((do)
		   ;; ---------------- do ----------------
		   (let ((vars ()))
		     (if (or (< (length form) 3)
			     (not (proper-list? (cadr form)))
			     (not (proper-list? (caddr form))))
			 (lint-format "do is messed up: ~A" name (truncated-list->string form))
			 
			 (let ((step-vars (cadr form))
			       (inner-env #f))
			   
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
				   (set! vars (cons (make-var :name (caar bindings) 
							      :type (->type (cadar bindings)) 
							      :value (and (pair? (cddar bindings)) (caddar bindings)))
						    vars)))))

			   (set! inner-env (append vars env))

			   ;; walk the step exprs
			   (do ((bindings step-vars (cdr bindings)))
			       ((not (pair? bindings)))
			     (let ((stepper (car bindings))) ; the entire binding: '(i 0 (+ i 1))
			       (when (and (binding-ok? name head stepper env #t)
					  (pair? (cddr stepper)))
				 (lint-walk name (caddr stepper) inner-env)
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
				 (if (pair? end+result)
				     (let ((end (car end+result)))
				       (lint-walk name end inner-env)
				       (if (pair? (cdr end+result))
					   (if (null? (cddr end+result))
					       (lint-walk name (cadr end+result) inner-env)
					       (lint-walk-body name 'do-result (cdr end+result) inner-env)))
				       (if (and (symbol? end) (memq end '(= > < >= <= null? not)))
					   (lint-format "perhaps missing parens: ~A" name end+result))
				       
				       (cond ((never-false end)
					      (lint-format "end test is never false: ~A" name end))
					     
					     (end ; it's not #f
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
									       (lists->string step end)))))))))))
					     ((pair? (cdr end+result))
					      (lint-format "result is unreachable: ~A" name end+result)))
				       
				       (if (and (symbol? end)
						(not (var-member end env))
						(not (hash-table-ref globals end))
						(procedure? (symbol->value end *e*)))
					   (lint-format "strange do end-test: ~A in ~A is a procedure" name end end+result))))))
			   
			   (lint-walk-body name head (cdddr form) inner-env)
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
				 (varslen (length vars))
				 (setv #f))
			     (when (and (pair? end-test)
					(= varslen 1)
					(pair? body)
					(null? (cdr body))
					(pair? (car body)) 
					(memq (caar body) '(vector-set! float-vector-set! int-vector-set! list-set! string-set!))
					(eq? (var-type (car vars)) 'integer?)
					(memq (car end-test) '(>= =))
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
			   (let ((body (cddr form)))
			     (if (and (null? (cdr body))
				      (pair? (car body))
				      (memq (caar body) '(let let*)))
				 (if (null? (cadr form))
				     (lint-format "pointless let: ~A" name (truncated-lists->string form (car body)))
				     (if (null? (cadar body))
					 (lint-format "pointless let: ~A" name (truncated-lists->string form `(let ,(cadr form) ,@(cddar body))))))))
			   )
			 
			 (let ((vars (if (and named-let 
					      (not (keyword? named-let))
					      (or (null? (caddr form))
						  (and (proper-list? (caddr form))
						       (every? pair? (caddr form)))))
					 (list (make-fvar :name named-let 
							  :ftype head
							  :decl (dummy-func name form (list 'define (cons '_ (map car (caddr form))) #f))
							  :arglist (map car (caddr form))
							  :value form
							  :env env))
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
				   
				   (set! vars (cons (make-var :name (caar bindings) :value val :type (->type val)) vars)))))
			   
			   (when (and (pair? varlist)        ; (let ((x A)) (if x (f A) B)) -> (cond (A => f) (else B)
				      (pair? (car varlist))  ;   ^ this happens a lot, so it's worth this tedious search
				      (null? (cdr varlist))
				      (pair? body)
				      (null? (cdr body))
				      (pair? (cdar varlist))
				      (pair? (cadar varlist)))
			     (let ((p (car body))
				   (var (car varlist)))
			       (when (and (pair? p)
					  (pair? (cdr p)))
				 (if (and (or (and (memq (car p) '(if and)) ; (let ((x (f y))) (and x (g x))) -> (cond ((f y) => g) (else #f))
						   (eq? (cadr p) (car var)))
					      (and (eq? (car p) 'or)
						   (equal? (cadr p) `(not ,(car var)))))   
					  (pair? (cddr p))
					  (pair? (caddr p))
					  (or (eq? (car p) 'if)
					      (null? (cdddr p)))
					  (pair? (cdaddr p))
					  (not (eq? (caaddr p) (car var))) ; (let ((x A)) (if x (x x))) !?
					  (null? (cddr (caddr p)))
					  (eq? (car var) (cadr (caddr p))))
				     (let ((else-clause (cond ((pair? (cdddr p))
							       (if (not (eq? (cadddr p) (car var)))
								   (if (and (pair? (cadddr p))
									    (tree-member (car var) (cadddr p)))
								       :oops! ; if the let var appears in the else portion, we can't do anything with =>
								       `((else ,(cadddr p))))
								   `((else #f)))) ; this stands in for the local var
							      ((eq? (car p) 'and)
							       `((else #f)))
							      ((eq? (car p) 'or)
							       `((else #t)))
							      (else ()))))
				       (unless (eq? else-clause :oops!)
					 (lint-format "perhaps ~A" name (lists->string form `(cond (,(cadr var) => ,(caaddr p)) ,@else-clause)))))))))
			   
			   (let* ((cur-env (append vars env))
				  (e (lint-walk-body name head body cur-env))
				  (nvars (if (null? cur-env)
					     e
					     (and (not (eq? e cur-env))
						  (env-difference name e cur-env ())))))
			     (if (pair? nvars)
				 (if (eq? (var-name (car nvars)) lambda-marker)
				     (begin
				       (set! env (cons (car nvars) env))
				       (set! nvars (cdr nvars)))
				     (set! vars (append nvars vars)))))
			   (report-usage name 'variable head vars)

			   (if (and (pair? body)
				    (null? (cdr body))
				    (pair? varlist)             ; (let ()...)
				    (pair? (car varlist))       ; (let (x) ...)
				    (not (pair? (car body))))
			       (if (and (eq? (car body) (caar varlist))
					(null? (cdr varlist))
					(pair? (cdar varlist))) ; (let ((a))...)
				   (lint-format "perhaps ~A" name (lists->string form (cadar varlist)))
				   (let* ((len (length varlist))
					  (last-var (and (positive? len)
							 (list-ref varlist (- len 1)))))
				     (if (and (> len 1)
					      (pair? last-var)
					      (pair? (cdr last-var))
					      (null? (cddr last-var))
					      (eq? (car body) (car last-var)))
					 (lint-format "perhaps ~A" name 
						      (lists->string form `(let ,(copy varlist (make-list (- len 1))) ,(cadr last-var))))))))
			   )))
		   env)
		  
		  ((let*)
		   ;; ---------------- let* ----------------		  
		   (if (< (length form) 3)
		       (lint-format "let* is messed up: ~A" name (truncated-list->string form))
		       (let ((named-let (and (symbol? (cadr form)) (cadr form))))
			 (let ((vars (if named-let (list (make-var :name named-let)) ()))
			       (varlist (if named-let (caddr form) (cadr form)))
			       (body (if named-let (cdddr form) (cddr form)))
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
				   (set! vars (cons (make-var :name (caar bindings) :value (cadar bindings) :type (->type (cadar bindings))) vars)))))
			   
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
				  (e (lint-walk-body name head body cur-env))
				  (nvars (and (not (eq? e cur-env))
					      (env-difference name e cur-env ()))))
			     (if (pair? nvars)
				 (set! vars (append nvars vars))))
			   
			   (report-usage name 'variable head vars)
			   
			   (when (and (pair? varlist)        ; (let* (...(x A)) (if x (f A) B)) -> (let(*) (...) (cond (A => f) (else B)))
				      (pair? body)
				      (null? (cdr body)))
			     (let* ((varlen (length varlist))
				    (var (and (positive? varlen)
					      (varlist (- varlen 1)))))
			       (when (and (pair? var)
					  (positive? varlen)
					  (pair? (cdr var)))
				 (let ((p (car body)))
				   (when (and (pair? p)
					      (pair? (cdr p))
					      (or (and (memq (car p) '(if and)) 
						       (eq? (cadr p) (car var)))
						  (and (eq? (car p) 'or)
						       (equal? (cadr p) `(not ,(car var)))))
					      (pair? (cddr p))
					      (pair? (caddr p))
					      (or (eq? (car p) 'if)
						  (null? (cdddr p)))
					      (pair? (cdaddr p))
					      (not (eq? (caaddr p) (car var))) ; ! (let* (...(x A)) (if x (x x)))
					      (null? (cddr (caddr p)))
					      (eq? (car var) (cadr (caddr p))))

				     (let ((else-clause (cond ((pair? (cdddr p)) ; only if 'if (see above)
							       (if (not (eq? (cadddr p) (car var)))
								   (if (and (pair? (cadddr p))
									    (tree-member (car var) (cadddr p)))
								       :oops! ; if the let var appears in the else portion, we can't do anything with =>
								       `((else ,(cadddr p))))
								   `((else #f)))) ; this stands in for the local var
							      ((eq? (car p) 'and)
							       `((else #f)))
							      ((eq? (car p) 'or)
							       `((else #t)))
							      (else ()))))
				       (unless (eq? else-clause :oops!)
					 (case varlen
					   ((1) (lint-format "perhaps ~A" name 
							     (lists->string form `(cond (,(cadr var) => ,(caaddr p)) ,@else-clause))))
					   ((2) (lint-format "perhaps ~A" name 
							     (lists->string form `(let (,(car varlist))
										    (cond (,(cadr var) => ,(caaddr p)) ,@else-clause)))))
					   (else (lint-format "perhaps ~A" name 
							      (lists->string form `(let* (,@(copy varlist (make-list (- varlen 1))))
										     (cond (,(cadr var) => ,(caaddr p)) ,@else-clause)))))))))))))
			   
			   (if (and (pair? body)
				    (null? (cdr body))
				    (pair? varlist)             ; (let* ()...)
				    (pair? (car varlist))       ; (let* (x) ...)
				    (not (pair? (car body))))
			       (if (and (eq? (car body) (caar varlist))
					(null? (cdr varlist))
					(pair? (cdar varlist))) ; (let* ((a))...)
				   (lint-format "perhaps ~A" name (lists->string form (cadar varlist)))
				   (let* ((len (length varlist))
					  (last-var (and (positive? len)
							 (list-ref varlist (- len 1)))))
				     (if (and (> len 1)
					      (pair? last-var)
					      (pair? (cdr last-var))
					      (null? (cddr last-var))
					      (eq? (car body) (car last-var)))
					 (lint-format "perhaps ~A" name 
						      (lists->string form `(,(if (= len 2) 'let 'let*)
									    ,(copy varlist (make-list (- len 1)))
									    ,(cadr last-var))))))))
			   )))
		   env)
		  
		  ((letrec letrec*) 
		   ;; ---------------- letrec ----------------		  
		   (if (< (length form) 3)
		       (lint-format "~A is messed up: ~A" name head (truncated-list->string form))
		       (let ((vars ()))
			 (cond ((null? (cadr form))
				(lint-format "~A could be let: ~A" name head (truncated-list->string form)))
			       ((not (pair? (cadr form)))
				(lint-format "~A is messed up: ~A" name head (truncated-list->string form)))
			       ((and (null? (cdadr form))
				     (eq? head 'letrec*))
				(lint-format "letrec* could be letrec? ~A" name (truncated-list->string form))))
			 
			 (do ((bindings (cadr form) (cdr bindings)))
			     ((not (pair? bindings))
			      (if (pair? bindings)
				  (lint-format "~A variable list is not a proper list? ~S" name head (cadr form))))
			   (if (binding-ok? name head (car bindings) env #f)
			       (set! vars (cons (make-var :name (caar bindings) :value (cadar bindings) :type (->type (cadar bindings))) vars))))
			 
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
		  
		  ((defgenerator) ; this is the defining macro and the make-<gen> code
		   ;; ---------------- defgenerator ----------------
		   (get-generator form)
		   env)
		  
		  ((define-syntax define-module)
		   env)
		  
		  ((let-syntax letrec-syntax)
		   (lint-walk-body name head (cddr form) env))

		  ((case-lambda)
		   (if (pair? (cdr form))
		       (let ((lens ()))
			 (for-each 
			  (lambda (choice)
			    (if (pair? choice)
				(let ((len (length (car choice))))
				  (if (member len lens)
				      (lint-format "repeated parameter list? ~A in ~A" name (car choice) form))
				  (set! lens (cons len lens))
				  (lint-walk 'case-lambda `(lambda ,@choice) env))))
			  (if (not (pair? (cadr form))) (cddr form) (cdr form)))))
		   env)

		  (else
		   ;; ---------------- everything else ----------------		  
		   
		   (if (not (proper-list? form))
		       (begin
			 ;; these appear to be primarily macro/match arguments
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
				    (pair? (cdr head))
				    (eq? (car head) 'lambda))
			   (let ((len (length (cadr head))))
			     (cond ((and len (>= len 0)
					 (not (equal? len (length (cdr form)))))
				    (lint-format "~A has ~A arguments: ~A" 
						 head (car head) 
						 (if (> len (length (cdr form))) "too few" "too many")
						 (truncated-list->string form)))
				   ((and (null? (cadr head))
					 (pair? (cddr head)))
				    (lint-format "perhaps ~A" name 
						 (truncated-lists->string form 
						   (if (and (null? (cdddr head))
							    (or (not (pair? (caddr head)))
								(not (memq (caaddr head) '(define define* define-constant define-macro define-macro*)))))
						       (caddr head)
						       `(let () ,@(cddr head))))))
				   ((identity? head)
				    (lint-format "perhaps ~A" name (truncated-lists->string form (cadr form)))))))
			 
			 (let ((vars env))
			   (for-each
			    (lambda (f)
			      (set! vars (lint-walk name f vars)))
			    form))
			 ))
		   env)
		  ))
	      
	      ;; else form is not a symbol and not a pair
	      (begin
		(if (vector? form)
		    (let ((happy #t))
		      (for-each
		       (lambda (x)
			 (when (and (pair? x)
				    (eq? (car x) 'unquote))
			   (lint-walk name (cadr x) env) ; register refs
			   (set! happy #f)))
		       form)
		      (if (not happy)
			  (lint-format "quasiquoted vectors are a really bad idea: ~A" name form))))
		env))))
    
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
		
		(set! *#readers*
		      (list (cons #\e (lambda (str)
					(if (not (string=? str "e"))
					    (let ((num (string->number (substring str 1))))
					      (if num 
						  (cond ((rational? num)
							 (format outport " this #e is dumb, #~A -> ~A~%" str (substring str 1)))
							((not (real? num))
							 (format outport " #e can't handle complex numbers, #~A -> ~A~%" str num))
							((zero? (- num (floor num)))
							 (format outport "perhaps #~A -> ~A~%" str (floor num)))))))
					#f))
			    (cons #\i (lambda (str)
					(if (not (string=? str "i"))
					    (let ((num (string->number (substring str 1))))
					      (if num 
						  (if (not (rational? num))
						      (format outport " this #i is dumb, #~A -> ~A~%" str (substring str 1))
						      (format outport " perhaps #~A -> ~A~%" str (* 1.0 num))))))
					#f))
			    (cons #\d (lambda (str)
					(if (and (not (string=? str "d"))
						 (string->number (substring str 1)))
					    (format outport " #d is pointless, #~A -> ~A~%" str (substring str 1)))
					#f))
			    (cons #\_ (lambda (str)
					(and (string=? str "__line__")
					     (port-line-number))))))
		
		;; try to get past all the # and \ stuff in other Schemes
		;;   main remaining problem: [] used as parentheses (Gauche and Chicken for example), and #!optional (Chicken and MIT-scheme)
		(set! (hook-functions *read-error-hook*)  
		      (list (lambda (h)
			      (let ((data (h 'data))
				    (line (port-line-number)))
				(if (h 'type)
				    (begin
				      (format outport " reader[~A]: unknown # object: #~A~%" line data)
				      (set! (h 'result)
					    (case (data 0)
					      ((#\_) (if (string=? data "__line__")
							 (port-line-number)
							 (symbol->keyword (string->symbol data))))
					      ((#\;) (read) (values))
					      ((#\\) 
					       (if (and (member (substring data 1) 
								'("newline" "return" "space" "tab" "null" "linefeed" "alarm" "backspace" "escape" "delete")
								string-ci=?)
							(not (string=? data (string-downcase data))))
						   (format outport " reader: use #~A, not #~A~%" (string-downcase data) data))
					       (cond ((string-ci=? data "\\newline")   #\newline)
						     ((string-ci=? data "\\return")    #\return)
						     ((string-ci=? data "\\space")     #\space)
						     ((string-ci=? data "\\tab")       #\tab)
						     ((string-ci=? data "\\null")      #\null)
						     ((string-ci=? data "\\linefeed")  #\linefeed)
						     ((string-ci=? data "\\alarm")     #\alarm)
						     ((string-ci=? data "\\escape")    #\escape)
						     ((string-ci=? data "\\delete")    #\delete)
						     ((string-ci=? data "\\backspace") #\backspace)
						     ((string-ci=? data "\\page")
						      (format outport " reader: #~A is not predefined; use #\\xc~%" data)
						      #\xc)
						     ((string-ci=? data "\\rubout")    
						      (format outport " reader: #~A is not predefined; use #\\x7f~%" data)
						      #\x7f)
						     (else (symbol->keyword (string->symbol data)))))
					      (else (symbol->keyword (string->symbol data))))))
				    (begin
				      (format outport " reader[~A]: unknown \\ usage: \\~C~%" line data)
				      (set! (h 'result) data)))))))
		
		(do ((form (read fp) (read fp)))
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
		    (close-input-port fp)))))))))



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
		       (epos (string-position (if (and apos (= pos apos)) "</a>" "</em>") str (+ bpos 1))))
		  (string-append (substring str 0 pos)
				 (substring str (+ bpos 1) epos)
				 (remove-markups (substring str (+ epos (if (and apos (= apos pos)) 4 5)))))))))))
  
  (define (fixup-html str)
    (let ((pos (char-position #\& str)))
      (if (not pos)
	  str
	  (string-append (substring str 0 pos)
			 (let* ((epos (char-position #\; str pos))
				(substr (substring str (+ pos 1) epos)))
			   (string-append (cond ((string=? substr "gt") ">")
						((string=? substr "lt") "<")
						((string=? substr "mdash") "-")
						((string=? substr "amp") "&")
						(else (format () "unknown: ~A~%" substr)))
					  (fixup-html (substring str (+ epos 1)))))))))
  
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
						(format () ";~A ~D: ~A~%" file line-num outstr)))))
				      (lambda args
					(format () ";~A ~D, error in read: ~A ~A~%" file line-num args
						(fixup-html (remove-markups code)))))))))))))))))))
