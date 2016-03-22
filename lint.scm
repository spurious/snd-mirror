;;; lint for s7 scheme
;;;
;;; (lint "file.scm") checks file.scm for infelicities
;;; to control the kinds of checks, set the variables below.
;;; for tests and examples, see lint-test in s7test.scm 

(provide 'lint.scm)

(define *report-unused-parameters* #f)                    ; many of these are reported anyway if they are passed some non-#f value
(define *report-unused-top-level-functions* #f)           ; these are very common in Scheme, only questionable in self-contained code
(define *report-shadowed-variables* #f)                   ; shadowed parameters, etc
(define *report-undefined-identifiers* #f)                ; names we can't account for
(define *report-multiply-defined-top-level-functions* #f)
(define *report-nested-if* 4)                             ; 3 is lowest, this sets the nesting level that triggers an if->cond suggestion
(define *report-short-branch* 12)                         ; controls when a lop-sided if triggers a reordering suggestion
(define *report-one-armed-if* #f)                         ; if -> when/unless
(define *report-loaded-files* #f)                         ; if load is encountered, include that file in the lint process
(define *report-any-!-as-setter* #t)                      ; unknown funcs/macros ending in ! are treated as setters
(define *report-function-stuff* #t)                       ; checks for missed function uses etc
(define *report-doc-strings* #f)                          ; old-style (CL) doc strings
(define *report-func-as-arg-arity-mismatch* #f)           ; as it says... (kinda slow, and this error almost never happens)
(define *lint* #f)                                        ; the lint let
;; this gives other programs a way to extend or edit lint's tables: for example, the
;;   table of functions that are simple (no side effects) is (*lint* 'no-side-effect-functions)

(if (provided? 'pure-s7)
    (begin
      (define (make-polar mag ang) (complex (* mag (cos ang)) (* mag (sin ang))))

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

(define *current-file* "")
(define *lint-output-port* *stderr*)
(define *top-level-objects* (make-hash-table))

(format *stderr* "loading lint.scm~%")
(set! reader-cond #f)
(define-macro (reader-cond . clauses) `(values))          ; clobber reader-cond to avoid dumb unbound-variable errors


;;; --------------------------------------------------------------------------------

(set! *#readers* (list (cons #\_ (lambda (str)
				   (and (string=? str "__line__")
					(port-line-number))))))

(define defanimal define*)
(unless (provided? 'snd)
  (define definstrument define*)
  (define defgenerator define*))

;;; --------------------------------------------------------------------------------


(define lint

  (let ((no-side-effect-functions 
	 (let ((ht (make-hash-table)))
	   (for-each
	    (lambda (op) 
	      (hash-table-set! ht op #t))
	    '(* + - / < <= = > >= 
	      abs acos acosh and angle append aritable? arity ash asin asinh assoc assq assv atan atanh 
	      begin boolean? byte-vector byte-vector?
	      caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
	      call-with-input-string call-with-input-file
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
	      lambda lambda* let->list lcm length let let* let-ref let? letrec letrec* list list->string list->vector list-ref
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
	      tan tanh tree-length truncate
	      unless
	      values vector vector-append vector->list vector-dimensions vector-length vector-ref vector?
	      when with-baffle with-let with-input-from-file with-input-from-string with-output-to-string
	      zero?
	      #_{list} #_{apply_values} #_{append} unquote))
	   ;; do not include file-exists? or directory?
	   ;; should this include peek-char or unlet ?
	   ht))

	(built-in-functions (let ((ht (make-hash-table)))
			      (for-each
			       (lambda (op) 
				 (hash-table-set! ht op #t))
			       '(symbol? gensym? keyword? let? openlet? iterator? constant? macro? c-pointer? c-object? 
			         input-port? output-port? eof-object? integer? number? real? complex? rational? random-state? 
			         char? string? list? pair? vector? float-vector? int-vector? byte-vector? hash-table? 
			         continuation? procedure? dilambda? boolean? float? proper-list? sequence? null? gensym 
			         symbol->string string->symbol symbol symbol->value symbol->dynamic-value symbol-access 
			         make-keyword symbol->keyword keyword->symbol outlet rootlet curlet unlet sublet varlet 
			         cutlet inlet owlet coverlet openlet let-ref let-set! make-iterator iterate iterator-sequence
			         iterator-at-end? provided? provide defined? c-pointer port-line-number port-filename 
			         pair-line-number pair-filename port-closed? current-input-port current-output-port 
			         current-error-port let->list char-ready? close-input-port close-output-port flush-output-port 
			         open-input-file open-output-file open-input-string open-output-string get-output-string 
			         newline write display read-char peek-char write-char write-string read-byte write-byte 
			         read-line read-string read call-with-input-string call-with-input-file with-input-from-string 
			         with-input-from-file call-with-output-string call-with-output-file with-output-to-string 
			         with-output-to-file real-part imag-part numerator denominator even? odd? zero? positive? 
			         negative? infinite? nan? complex magnitude angle rationalize abs exp log sin cos tan asin 
			         acos atan sinh cosh tanh asinh acosh atanh sqrt expt floor ceiling truncate round lcm gcd
			         + - * / max min quotient remainder modulo = < > <= >= logior logxor logand lognot ash 
			         random-state random inexact->exact exact->inexact integer-length make-polar make-rectangular 
			         logbit? integer-decode-float exact? inexact? random-state->list number->string string->number 
			         char-upcase char-downcase char->integer integer->char char-upper-case? char-lower-case? 
			         char-alphabetic? char-numeric? char-whitespace? char=? char<? char>? char<=? char>=? 
			         char-position string-position make-string string-ref string-set! string=? string<? string>? 
			         string<=? string>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? string-ci=? string-ci<? 
			         string-ci>? string-ci<=? string-ci>=? string-copy string-fill! list->string string-length 
			         string->list string-downcase string-upcase string-append substring string object->string 
			         format cons car cdr set-car! set-cdr! caar cadr cdar cddr caaar caadr cadar cdaar caddr 
			         cdddr cdadr cddar caaaar caaadr caadar cadaar caaddr cadddr cadadr caddar cdaaar cdaadr 
			         cdadar cddaar cdaddr cddddr cddadr cdddar assoc member list list-ref list-set! list-tail 
			         make-list length copy fill! reverse reverse! sort! append assq assv memq memv vector-append 
			         list->vector vector-fill! vector-length vector->list vector-ref vector-set! vector-dimensions 
			         make-vector make-shared-vector vector float-vector make-float-vector float-vector-set! 
			         float-vector-ref int-vector make-int-vector int-vector-set! int-vector-ref ->byte-vector 
			         byte-vector make-byte-vector hash-table hash-table* make-hash-table hash-table-ref 
			         hash-table-set! hash-table-entries cyclic-sequences call/cc call-with-current-continuation 
			         call-with-exit load autoload eval eval-string apply for-each map dynamic-wind values 
			         catch throw error procedure-documentation procedure-signature help procedure-source funclet 
			         procedure-setter arity aritable? not eq? eqv? equal? morally-equal? gc s7-version emergency-exit 
			         exit dilambda hash-table-size make-hook hook-functions stacktrace tree-length
				 #_{list} #_{apply_values} #_{append} unquote))
			      ht))

	(makers '(gensym sublet inlet make-iterator let->list random-state random-state->list number->string
		  make-string string string-copy copy list->string string->list string-append substring object->string
		  format cons list make-list reverse append vector-append list->vector vector->list make-vector
		  make-shared-vector vector make-float-vector float-vector make-int-vector int-vector byte-vector
		  byte-vector hash-table hash-table* make-hash-table make-hook #_{list} #_{append}))

	(deprecated-ops '((global-environment . rootlet)
			  (current-environment . curlet)
			  (make-procedure-with-setter . dilambda)
			  (procedure-with-setter? . dilambda?)
			  (make-random-state . random-state)
			  ;;(make-rectangular . complex)
			  (data-format . sample-type)
			  (mus-sound-frames . mus-sound-framples)
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

	(non-negative-ops '(string-length vector-length abs magnitude denominator gcd lcm tree-length
			    char->integer byte-vector-ref byte-vector-set! hash-table-entries write-byte
			    char-position string-position pair-line-number port-line-number))
	
	(bools (let ((h (make-hash-table)))
		 (for-each
		  (lambda (op)
		    (set! (h op) #t))
		  '(symbol? integer? rational? real? number? complex? float? keyword? gensym? byte-vector? string? list? sequence?
		    char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair? proper-list?
		    output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?
		    unspecified? c-object? constant?))
		 h))

	(bools1 (let ((h (make-hash-table)))
		  (for-each
		   (lambda (op)
		     (set! (h op) #t))
		   '(symbol? integer? rational? real? number? complex? float? keyword? gensym? byte-vector? string? list? sequence?
		     char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair? proper-list?
		     output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer? c-object?
		     unspecified? exact? inexact? defined? provided? even? odd? char-whitespace? char-numeric? char-alphabetic?
		     negative? positive? zero? constant? infinite? nan? char-upper-case? char-lower-case? directory? file-exists?))
		  h))

	(notables (let ((h (make-hash-table)))
		    (for-each
		     (lambda (op)
		       (set! (h (car op)) (cadr op)))
		     '((< >=) (> <=) (<= >) (>= <)
		       (char<? char>=?) (char>? char<=?) (char<=? char>?) (char>=? char<?)
		       (string<? string>=?) (string>? string<=?) (string<=? string>?) (string>=? string<?)
		       (char-ci<? char-ci>=?) (char-ci>? char-ci<=?) (char-ci<=? char-ci>?) (char-ci>=? char-ci<?)
		       (string-ci<? string-ci>=?) (string-ci>? string-ci<=?) (string-ci<=? string-ci>?) (string-ci>=? string-ci<?)
		       (odd? even?) (even? odd?) (exact? inexact?) (inexact? exact?)))
		    h))

	(reversibles (let ((h (make-hash-table)))
		       (for-each
			(lambda (op)
			  (set! (h (car op)) (cadr op)))
			'((< >) (> <) (<= >=) (>= <=)
			  (* *) (+ +) (= =) (char=? char=?) (string=? string=?)
			  (eq? eq?) (eqv? eqv?) (equal? equal?) (morally-equal? morally-equal?)
			  (logand logand) (logxor logxor) (logior logior)
			  (max max) (min min) (lcm lcm) (gcd gcd)
			  (char<? char>?) (char>? char<?) (char<=? char>=?) (char>=? char<=?) 
			  (string<? string>?) (string>? string<?) (string<=? string>=?) (string>=? string<=?) 
			  (char-ci<? char-ci>?) (char-ci>? char-ci<?) (char-ci<=? char-ci>=?) (char-ci>=? char-ci<=?)
			  (string-ci<? string-ci>?) (string-ci>? string-ci<?) (string-ci<=? string-ci>=?) (string-ci>=? string-ci<=?)))
		       h))
    
	(repeated-args-table (let ((h (make-hash-table)))
			       (for-each
				(lambda (op)
				  (set! (h op) #t))
				'(= / max min < > <= >= - quotient remainder modulo rationalize and or
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
	
	(binders '(let let* letrec letrec* do
		   lambda lambda* define define* 
		   call/cc call-with-current-continuation 
		   define-macro define-macro* define-bacro define-bacro* define-constant define-expansion
		   load eval eval-string require))

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
	(linted-files ())
	(big-constants ())
	(equable-closures ())
	(*e* #f)
	(other-identifiers #f)
	(quote-warnings 0)
	(lint-let-reduction-factor 3) ; maybe make this a global switch -- the higher this number, the fewer let-reduction suggestions
	(last-simplify-boolean-line-number -1)
	(last-simplify-numeric-line-number -1)
	(last-simplify-cxr-line-number -1)
	(last-if-line-number -1)
	(last-checker-line-number -1)
	(line-number -1)
	(lambda-marker '[lambda])
	(goto-marker '[call/exit])
	(call/cc-marker '[call/cc])
	(catch-marker '[catch])
	(pp-left-margin 4)
	(lint-left-margin 1))
    
    (set! *e* (curlet))
    (set! *lint* *e*)
    ;; external access to (for example) the built-in-functions hash-table via (*lint* 'built-in-functions)


    ;; -------- lint-format --------
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
	     (len2 (length str2)))
	(when (> len1 80)
	  (set! str1 (truncated-list->string f1))
	  (set! len1 (length str1)))
	(when (> len2 80)
	  (set! ((funclet lint-pretty-print) '*pretty-print-left-margin*) pp-left-margin)
	  (set! ((funclet lint-pretty-print) '*pretty-print-length*) (- 114 pp-left-margin))
	  (set! str2 (lint-pp f2))
	  (set! len2 (length str2)))
	(if (< (+ len1 len2) 80)
	    (format #f "~A -> ~A" str1 str2)
	    (format #f "~%~NC~A ->~%~NC~A" pp-left-margin #\space str1 pp-left-margin #\space str2))))
    
    (define (truncated-lists->string f1 f2)
      ;; same but 2 strings that may need to be lined up vertically and both are truncated
      (let* ((str1 (object->string f1))
	     (len1 (length str1))
	     (str2 (object->string f2))
	     (len2 (length str2)))
	(when (> len1 80)
	  (set! str1 (truncated-list->string f1))
	  (set! len1 (length str1)))
	(when (> len2 80)
	  (set! str2 (truncated-list->string f2))
	  (set! len2 (length str2)))
	(if (< (+ len1 len2) 80)
	    (format #f "~A -> ~A" str1 str2)
	    (format #f "~%~NC~A ->~%~NC~A" pp-left-margin #\space str1 pp-left-margin #\space str2))))
    
    (define made-suggestion 0)
    
    (define (lint-format str caller . args)
      (let ((outstr (if (< 0 line-number 100000)
			(apply format #f (string-append "~NC~A (line ~D): " str "~%") 
			       lint-left-margin #\space
			       (truncated-list->string caller) 
			       line-number args)
			(apply format #f (string-append "~NC~A: " str "~%") 
			       lint-left-margin #\space
			       (truncated-list->string caller) args))))
	(set! made-suggestion (+ made-suggestion 1))
	(display outstr outport)
	(if (> (length outstr) 120)
	    (newline outport))))

    (define (local-line-number tree)
      (let ((tree-line (if (pair? tree) (pair-line-number tree) 0)))
	(if (and (< 0 tree-line 100000)
		 (not (= tree-line line-number)))
	    (format #f " (line ~D)" tree-line)
	    "")))

    
    ;; -------- vars -------- 
    (define var-name car)
    (define (var? v) (and (pair? v) (let? (cdr v))))
    (define var-member assq)
    
    (define var-ref     (dilambda (lambda (v) (let-ref (cdr v) 'ref))     (lambda (v x) (let-set! (cdr v) 'ref x))))
    (define var-set     (dilambda (lambda (v) (let-ref (cdr v) 'set))     (lambda (v x) (let-set! (cdr v) 'set x))))
    (define var-history (dilambda (lambda (v) (let-ref (cdr v) 'history)) (lambda (v x) (let-set! (cdr v) 'history x))))
    (define var-ftype   (dilambda (lambda (v) (let-ref (cdr v) 'ftype))   (lambda (v x) (let-set! (cdr v) 'ftype x))))
    (define var-arglist (dilambda (lambda (v) (let-ref (cdr v) 'arglist)) (lambda (v x) (let-set! (cdr v) 'arglist x))))
    (define var-definer (dilambda (lambda (v) (let-ref (cdr v) 'definer)) (lambda (v x) (let-set! (cdr v) 'definer x))))
    (define var-leaves  (dilambda (lambda (v) (let-ref (cdr v) 'leaves))  (lambda (v x) (let-set! (cdr v) 'leaves x))))
    (define var-scope   (dilambda (lambda (v) (let-ref (cdr v) 'scope))   (lambda (v x) (let-set! (cdr v) 'scope x))))
    (define var-env     (dilambda (lambda (v) (let-ref (cdr v) 'env))     (lambda (v x) (let-set! (cdr v) 'env x))))
    (define var-decl    (dilambda (lambda (v) (let-ref (cdr v) 'decl))    (lambda (v x) (let-set! (cdr v) 'decl x))))
    (define var-match-list (dilambda (lambda (v) (let-ref (cdr v) 'match-list)) (lambda (v x) (let-set! (cdr v) 'match-list x))))
    (define var-initial-value (lambda (v) (let-ref (cdr v) 'initial-value))) ; not settable

    (define var-side-effect (dilambda (lambda (v) 
					(if (null? (let-ref (cdr v) 'side-effect))
					    (let-set! (cdr v) 'side-effect (get-side-effect v))
					    (let-ref (cdr v) 'side-effect)))
				      (lambda (v x) 
					(let-set! (cdr v) 'side-effect x))))

    (define var-signature (dilambda (lambda (v) 
				      (if (null? (let-ref (cdr v) 'signature))
					  (let-set! (cdr v) 'signature (get-signature v))
					  (let-ref (cdr v) 'signature)))
				    (lambda (v x) 
				      (let-set! (cdr v) 'signature x))))
    
    (define* (make-var name initial-value definer)
      (let ((old (hash-table-ref other-identifiers name)))
	(cons name (inlet 'initial-value initial-value 
			  'definer definer 
			  'history (if old 
				       (begin
					 (hash-table-set! other-identifiers name #f)
					 (if initial-value (cons initial-value old) old))
				       (if initial-value (list initial-value) ()))
			  'set 0 
			  'ref (if old (length old) 0)))))
    
    
    ;; -------- the usual list functions --------
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
      (reverse (let rem-dup ((lst lst)
			     (nlst ()))
		 (cond ((null? lst) nlst)
		       ((and (member (car lst) nlst)
			     (not (and (pair? (car lst))
				       (side-effect? (car lst) env))))
			(rem-dup (cdr lst) nlst))
		       (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
    
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
    
    
    ;; -------- trees --------
    (define copy-tree 
      (let ((documentation "(copy-tree lst) returns a full copy of lst"))
	(lambda (lis)
	  (if (pair? lis)
	      (cons (copy-tree (car lis))
		    (copy-tree (cdr lis)))
	      lis))))
    
    (define (tree-count1 x tree count)
      (if (or (>= count 2)
	      (not (pair? tree))
	      (eq? (car tree) 'quote))
	  count
	  (tree-count1 x (cdr tree)
		       (+ (tree-count1 x (car tree) (+ count (if (eq? x (car tree)) 1 0)))
			  (if (eq? x (cdr tree)) 1 0)))))
    
    (define (tree-count2 x tree count)
      (if (or (>= count 3)
	      (not (pair? tree))
	      (eq? (car tree) 'quote))
	  count
	  (tree-count2 x (cdr tree)
		       (+ (tree-count2 x (car tree) (+ count (if (eq? x (car tree)) 1 0)))
			  (if (eq? x (cdr tree)) 1 0)))))
    
    (define (proper-tree? tree)
      (or (not (pair? tree))
	  (and (proper-list? tree)
	       (every? proper-tree? (cdr tree)))))

    (define (gather-symbols tree)
      (let ((syms ()))
	(define (walk p)
	  (if (pair? p)
	      (if (symbol? (car p))
		  (if (not (eq? (car p) 'quote))
		      (for-each (lambda (a)
				  (if (symbol? a)
				      (if (not (memq a syms))
					  (set! syms (cons a syms)))
				      (if (pair? a)
					  (walk a))))
				(cdr p)))
		  (if (pair? (car p))
		      (begin
			(walk (car p))
			(walk (cdr p)))))
	      (if (and (symbol? tree)
		       (not (memq tree syms)))
		  (set! syms (cons tree syms)))))
	(walk tree)
	syms))
    
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
    
    (define (tree-memq sym tree)
      (or (eq? sym tree)
	  (and (pair? tree)
	       (not (eq? (car tree) 'quote))
	       (or (eq? (car tree) sym)
		   (tree-memq sym (car tree))
		   (tree-memq sym (cdr tree))))))
    
    (define (tree-member sym tree)
      (and (pair? tree)
	   (or (eq? (car tree) sym)
	       (tree-member sym (car tree))
	       (tree-member sym (cdr tree)))))
    
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
    
    (define (tree-sym-set-member sym set tree) ; sym as arg, set as car
      (and (pair? tree)
	   (or (memq (car tree) set)
	       (and (pair? (car tree))
		    (tree-sym-set-member sym set (car tree)))
	       (and (pair? (cdr tree))
		    (or (member sym (cdr tree))
			(member #f (cdr tree) (lambda (a b) (tree-sym-set-member sym set b))))))))
    
    (define (tree-set-member set tree)
      (and (pair? tree)
	   (or (memq (car tree) set)
	       (tree-set-member set (car tree))
	       (tree-set-member set (cdr tree)))))
    
    (define (tree-set-car-member set tree) ; set as car
      (and (pair? tree)
	   (or (and (memq (car tree) set)
		    tree)
	       (and (pair? (car tree))
		    (tree-set-car-member set (car tree)))
	       (and (pair? (cdr tree))
		    (member #f (cdr tree) (lambda (a b) (tree-set-car-member set b)))))))
    
    (define (maker? tree)
      (tree-set-car-member makers tree))
    
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
    
    
    ;; -------- types --------
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
    
    (define (quoted-symbol? x)
      (and (pair? x)
	   (eq? (car x) 'quote)
	   (pair? (cdr x))
	   (symbol? (cadr x))))
    
    (define (code-constant? x)
      (and (or (not (symbol? x))
	       (keyword? x))
	   (or (not (pair? x))
	       (eq? (car x) 'quote))))
    
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
    
    
    ;; -------- func info --------
    (define (arg-signature fnc env)
      (and (symbol? fnc)
	   (let ((fd (var-member fnc env)))
	     (if (var? fd)
		 (and (symbol? (var-ftype fd))
		      (var-signature fd))
		 (let ((f (symbol->value fnc *e*)))
		   (and (procedure? f)
			(procedure-signature f)))))))
    
    (define (arg-arity fnc env)
      (and (symbol? fnc)
	   (let ((fd (var-member fnc env)))
	     (if (var? fd)
		 (and (not (eq? (var-decl fd) 'error))
		      (arity (var-decl fd)))
		 (let ((f (symbol->value fnc *e*)))
		   (and (procedure? f)
			(arity f)))))))
    
    (define (dummy-func caller form f)
      (catch #t 
	(lambda ()
	  (eval f))
	(lambda args
	  (lint-format "in ~A, ~A" caller form (apply format #f (cadr args))))))
    
    (define (count-values body)
      (let ((mn #f)
	    (mx #f))
	(define (counter ignored tree) ; 'ignored is for member's benefit
	  (if (pair? tree)
	      (if (eq? (car tree) 'values)
		  (let ((args (- (length tree) 1)))
		    (for-each (lambda (p)
				(if (and (pair? p)
					 (eq? (car p) 'values))
				    (set! args (+ args (- (length p) 2)))))
			      (cdr tree))
		    (set! mn (min (or mn args) args))
		    (set! mx (max (or mx args) args)))
		  (begin
		    (if (pair? (car tree))
			(counter 'values (car tree)))
		    (if (pair? (cdr tree))
			(member #f (cdr tree) counter)))))
	  #f) ; return #f so member doesn't quit early
	(if (pair? body) 
	    (counter #f (list-ref body (- (length body) 1))))
	(and mn (list mn mx))))
    

    (define (get-signature v)

      (define (signer endb env)
	(and (not (side-effect? endb env))
	     (cond ((not (pair? endb))
		    (and (not (symbol? endb))
			 (list (->type endb))))
		   ((arg-signature (car endb) env) 
		    => (lambda (a)
			 (and (pair? a) 
			      (list (car a)))))
		   ((and (eq? (car endb) 'if)
			 (pair? (cddr endb)))
		    (let ((a1 (signer (caddr endb) env))
			  (a2 (and (pair? (cdddr endb))
				   (signer (cadddr endb) env))))
		      (if (not a2)
			  a1
			  (and (equal? a1 a2) a1))))
		   (else #f))))
      
      (let ((ftype (var-ftype v))
	    (initial-value (var-initial-value v))
	    (arglist (var-arglist v))
	    (env (var-env v)))

	(let ((body (and (memq ftype '(define define* lambda lambda* let))
			 (cddr initial-value))))

	  (and (pair? body)
	       (let ((sig (signer (list-ref body (- (length body) 1)) env)))
		 (if (not (pair? sig))
		     (set! sig (list #t)))
		 
		 (when (and (proper-list? arglist)
			    (not (any? keyword? arglist)))
		   (for-each
		    (lambda (arg)       ; new function's parameter
		      (set! sig (cons #t sig))
		      ;; (if (pair? arg) (set! arg (car arg)))
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
		      (reverse sig)))))))

    (define (args->proper-list args)
      (cond ((symbol? args)	(list args))
	    ((not (pair? args))	args)
	    ((pair? (car args))	(cons (caar args) (args->proper-list (cdr args))))
	    (else               (cons (car args) (args->proper-list (cdr args))))))
    
    (define (out-vars func-name arglist body) ; t367 has tests
      (let ((ref ())
	    (set ()))
	
	(define (var-walk tree e)
	  
	  (define (var-walk-body tree e)
	    (when (pair? tree)
	      (for-each (lambda (p)
			  (set! e (var-walk p e)))
			tree)))

	  (define (shadowed v)
	    ;(format *stderr* "v: ~A, e: ~A~%" v e)
	    (if (and (or (memq v e)
			 (memq v ref))
		     (not (memq v set)))
		(set! set (cons v set)))
	    v)
	  
	  (if (symbol? tree)
	      (if (not (or (memq tree e)
			   (memq tree ref)
			   (defined? tree (rootlet))))
		  (set! ref (cons tree ref)))
	      
	      (if (pair? tree)
		  (if (not (pair? (cdr tree)))
		      (var-walk (car tree) e)
		      (case (car tree)
			((set! vector-set! list-set! hash-table-set! float-vector-set! int-vector-set! string-set! let-set! 
			       fill! string-fill! list-fill! vector-fill! reverse! sort! set-car! set-cdr!)
			 (let ((sym (if (symbol? (cadr tree))
					(cadr tree)
					(if (pair? (cadr tree))
					    (caadr tree)))))
			   (if (not (or (memq sym e)
					(memq sym set)))
			       (set! set (cons sym set)))
			   (var-walk (cddr tree) e)))
			
			((let letrec)
			 (if (and (pair? (cdr tree))
				  (pair? (cddr tree)))
			     (let* ((named (symbol? (cadr tree)))
				    (vars (if named (list (shadowed (cadr tree))) ())))
			       (for-each (lambda (v)
					   (when (and (pair? v)            ; protect against []
						      (pair? (cdr v)))     ; and other similar stupidity
					     (var-walk (cadr v) e)				   
					     (set! vars (cons (shadowed (car v)) vars))))
					 (if named (caddr tree) (cadr tree)))
			       (var-walk-body (if named (cdddr tree) (cddr tree)) (append vars e)))))
			
			((case)
			 (when (and (pair? (cdr tree))
				    (pair? (cddr tree)))
			   (for-each (lambda (c)
				       (when (pair? c)
					 (var-walk (cdr c) e)))
				     (cddr tree))))
			
			((quote) #f)
			
			((let* letrec*)
			 (let* ((named (symbol? (cadr tree)))
				(vars (if named (list (cadr tree)) ())))
			   (for-each (lambda (v)
				       (when (and (pair? v)
						  (pair? (cdr v)))
					 (var-walk (cadr v) (append vars e))				   
					 (set! vars (cons (shadowed (car v)) vars))))
				     (if named (caddr tree) (cadr tree)))
			   (var-walk-body (if named (cdddr tree) (cddr tree)) (append vars e))))
			
			((do)
			 ;(format *stderr* "inner do: ~A, e: ~A, refs: ~A ~A~%" tree e ref set)
			 (let ((vars ()))
			   (when (pair? (cadr tree))
			     (for-each (lambda (v)
					 (when (and (pair? v)
						    (pair? (cdr v)))
					   (var-walk (cadr v) e)
					   (set! vars (cons (shadowed (car v)) vars))))
				       (cadr tree))
			     (for-each (lambda (v)
					 (if (and (pair? v)
						  (pair? (cdr v))
						  (pair? (cddr v)))
					     (var-walk (caddr v) (append vars e))))
				       (cadr tree)))
			   (when (pair? (cddr tree))
			     (var-walk (caddr tree) (append vars e))
			     (var-walk-body (cdddr tree) (append vars e)))))
			
			((lambda lambda*)
			 (let ((vars (append (args->proper-list (cadr tree)) e)))
			   (var-walk-body (cddr tree) vars)))

			((define* define-macro define-macro* define-bacro define-bacro*)
			 (if (and (pair? (cdr tree))
				  (pair? (cddr tree)))
			     (begin
			       (set! e (cons (caadr tree) e))
			       (var-walk-body (cddr tree) (append (args->proper-list (cdadr tree)) e)))))

			((define define-constant) 
			 (if (and (pair? (cdr tree))
				  (pair? (cddr tree)))
			     (if (symbol? (cadr tree))
				 (begin
				   (var-walk (caddr tree) e)
				   (set! e (cons (cadr tree) e)))
				 (begin
				   (set! e (cons (caadr tree) e))
				   (var-walk-body (cddr tree) (append (args->proper-list (cdadr tree)) e))))))
			
			(else 
			 (var-walk (car tree) e)
			 (var-walk (cdr tree) e))))))
	  e)
	
	(var-walk body (cons func-name arglist))
	;(format *stderr* "~A: ~A ~A~%" func-name ref set)
	(list ref set)))

    (define (get-side-effect v)
      (let ((ftype (var-ftype v)))
	(or (not (memq ftype '(define define* lambda lambda*)))
	    (let ((body (cddr (var-initial-value v)))
		  (env (var-env v))
		  (args (cons (var-name v) (args->proper-list (var-arglist v)))))
	      (let ((outvars (append (cadr (out-vars (var-name v) args body)) args)))
		;(format *stderr* "get-side-effect: ~A ~A ~A~%" args outvars body)
		(any? (lambda (f) 
			(side-effect-with-vars? f env outvars))
		      body))))))

    (define* (make-fvar name ftype arglist decl initial-value env)
      (let ((old (hash-table-ref other-identifiers name)))
	(let ((new (cons name 
			 (inlet 'signature ()
				'side-effect ()
				'allow-other-keys (and (pair? arglist)
						       (memq ftype '(define* define-macro* define-bacro* defmacro*))
						       (eq? (last-par arglist) :allow-other-keys))
				'scope ()
				'env env
				'initial-value initial-value
				'values (count-values (cddr initial-value))
				'leaves #f
				'match-list #f
				'decl decl
				'arglist arglist
				'ftype ftype
				'history (if old 
					     (begin
					       (hash-table-set! other-identifiers name #f)
					       (if initial-value (cons initial-value old) old))
					     (if initial-value (list initial-value) ()))
				'set 0 
				'ref (if old (length old) 0)))))

	  (when (and *report-function-stuff*
		     (not (eq? name lambda-marker))
		     (memq ftype '(define lambda define* lambda*))
		     (pair? (caddr initial-value)))
	    (hash-table-set! equable-closures (caaddr initial-value)
			     (cons new (or (hash-table-ref equable-closures (caaddr initial-value)) ())))
	    ;(format *stderr* "make-fvar ~A: ~A -> ~A~%" name (caddr initial-value) (hash-table-ref equable-closures (caddr initial-value)))
	    )

	  new)))
    
    (define (return-type sym e)
      (let ((sig (arg-signature sym e)))
	(and (pair? sig)
	     (or (eq? (car sig) 'values) ; turn it into #t for now
		 (car sig)))))           ; this might be undefined in the current context (eg oscil? outside clm)
    
    (define (any-macro? f env)
      (let ((fd (var-member f env)))
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
	    ((c-pointer? c)    'c-pointer?)
	    ((c-object? c)     'c-object?)
	    ((eof-object? c)   'eof-object?)
	    ((eq? c #<unspecified>) 'unspecified?)
	    (#t #t))) ; this includes symbol? which at this level refers to a variable name (unknown type)
    
    (define (define->type c)
      (and (pair? c)
	   (case (car c)
	     ((define)
	      (if (and (pair? (cdr c))
		       (pair? (cadr c)))
		  'procedure?
		  (and (pair? (cddr c))
		       (->type (caddr c)))))
	     ((define* lambda lambda* case-lambda) 'procedure?)
	     ((dilambda) 'dilambda?)
	     ((define-macro define-macro* define-bacro define-bacro* defmacro defmacro* define-expansion) 'macro?)
	     (([call/cc] [call/exit]) 'continuation?)
	     (else #t))))
    
    (define (->type c)
      (cond ((not (pair? c))	        (->simple-type c))
	    ((not (symbol? (car c)))    (or (pair? (car c)) 'pair?))
	    ((not (eq? (car c) 'quote)) (or (return-type (car c) ()) (define->type c)))
	    ((symbol? (cadr c))         'symbol?)
	    (else                       (->simple-type (cadr c)))))   ; don't look for return type!
    
    (define (compatible? type1 type2) ; we want type1, we have type2 -- is type2 ok?
      (or (eq? type1 type2)
	  (not (symbol? type1))
	  (not (symbol? type2))
	  (not (hash-table-ref bools1 type1))
	  (not (hash-table-ref bools1 type2))
	  (case type1
	    ((number? complex?)  (memq type2 '(float? real? rational? integer? number? complex? exact? inexact? zero? negative? positive? even? odd? infinite? nan?)))
	    ((real?)             (memq type2 '(float? rational? integer? complex? number? exact? inexact? zero? negative? positive? even? odd? infinite? nan?)))
	    ((zero?)             (memq type2 '(float? real? rational? integer? number? complex? exact? inexact? even?)))
	    ((negative? positive?) (memq type2 '(float? real? rational? integer? complex? number? exact? inexact? even? odd? infinite? nan?)))
	    ((float?)            (memq type2 '(real? complex? number? inexact? zero? negative? positive? infinite? nan?)))
	    ((rational?)         (memq type2 '(integer? real? complex? number? exact? zero? negative? positive? even? odd?)))
	    ((integer?)          (memq type2 '(real? rational? complex? number? exact? even? odd? zero? negative? positive?)))
	    ((odd? even?)        (memq type2 '(real? rational? complex? number? exact? integer? zero? negative? positive?)))
	    ((exact?)            (memq type2 '(real? rational? complex? number? integer? zero? negative? positive?)))
	    ((inexact?)          (memq type2 '(real? number? complex? float? zero? negative? positive? infinite? nan?)))
	    ((infinite? nan?)    (memq type2 '(real? number? complex? positive? negative? inexact? float?)))
	    ((vector?)           (memq type2 '(float-vector? int-vector? sequence?)))
	    ((float-vector? int-vector?) (memq type2 '(vector? sequence?)))
	    ((sequence?)         (memq type2 '(list? pair? null? proper-list? vector? float-vector? int-vector? byte-vector? 
					       string? let? hash-table? c-object? iterator? procedure?))) ; procedure? for extended iterator
	    ((symbol? constant?) (memq type2 '(gensym? keyword? defined? provided? constant?)))
	    ((keyword? gensym? defined? provided?)  (eq? type2 'symbol?))
	    ((list?)             (memq type2 '(null? pair? proper-list? sequence?)))
	    ((proper-list?)      (memq type2 '(null? pair? list? sequence?)))
	    ((pair? null?)       (memq type2 '(list? proper-list? sequence?)))
	    ((dilambda?)         (memq type2 '(procedure? macro? iterator?)))
	    ((procedure?)        (memq type2 '(dilambda? iterator? macro? sequence?)))
	    ((macro?)            (memq type2 '(dilambda? iterator? procedure?)))
	    ((iterator?)         (memq type2 '(dilambda? procedure? sequence?)))
	    ((string?)           (memq type2 '(byte-vector? sequence? directory? file-exists?)))
	    ((hash-table? let? c-object?) 
	     (eq? type2 'sequence?))
	    ((byte-vector? directory? file-exists?) 
	     (memq type2 '(string? sequence?)))
	    ((input-port? output-port?) 
	     (eq? type2 'boolean?))
	    ((char? char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)
	     (memq type2 '(char? char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)))
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
	    ((integer?)         (memq type2 '(even? odd?)))
	    ((rational?)        (memq type2 '(integer? exact? odd? even?)))
	    ((exact?)           (memq type2 '(integer? rational?)))
	    ((real?)            (memq type2 '(integer? rational? float? negative? positive? zero? odd? even?)))
	    ((complex? number?) (memq type2 '(integer? rational? float? real? complex? number? negative? positive? zero? 
					      even? odd? exact? inexact? nan? infinite?)))
	    ((list?)            (memq type2 '(pair? null? proper-list?)))
	    ((proper-list?)     (eq? type2 'null?))
	    ((vector?)          (memq type2 '(float-vector? int-vector?)))
	    ((symbol?)          (memq type2 '(keyword? gensym? defined? provided? constant?)))
	    ((sequence?)        (memq type2 '(list? pair? null? proper-list? vector? float-vector? int-vector? byte-vector?
					      string? let? hash-table? c-object? directory? file-exists?)))
	    ((char?)            (memq type2 '(char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)))
	    (else #f))))
    
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
	       (pair? (cdr expr))
	       (never-false (cadr expr)))))
    
    (define (side-effect-with-vars? form env vars)
      ;(format *stderr* "sewv ~S ~S~%" form vars)
      ;; could evaluation of form have any side effects (like IO etc)
      
      (if (or (not (proper-list? form))                   ; we don't want dotted lists or () here
	      (null? form))

	  (and (symbol? form)
	       (let ((e (var-member form env)))
		 (if (var? e)
		     (and (symbol? (var-ftype e))
			  (var-side-effect e))
		     (and (procedure? (symbol->value form *e*))
			  (not (hash-table-ref no-side-effect-functions form))))))

	  ;; can't optimize ((...)...) because the car might eval to a function
	  (or (and (not (hash-table-ref no-side-effect-functions (car form)))
		   ;; if it's not in the no-side-effect table and ...
		   
		   (let ((e (var-member (car form) env)))
		     (or (not (var? e))
			 (not (symbol? (var-ftype e)))
			 (var-side-effect e)))
		   ;; it's either not known to be a local function, or it has side-effects, and...
		   
		   (or (not (eq? (car form) 'format))                         ; (format #f ...)
		       (not (pair? (cdr form)))                               ; (format)!
		       (cadr form))

		   (or (null? vars)
		       (not (memq (car form) '(set! 
					       ;vector-set! list-set! hash-table-set! float-vector-set! int-vector-set! string-set! let-set! 
					       ;fill! string-fill! list-fill! vector-fill! 
					       ;reverse! sort!
					       define define* define-macro define-macro* define-bacro define-bacro*)))))
	      ;; it's not the common (format #f ...) special case, then...(goto case below) 
	      ;; else return #t: side-effects are possible -- this is too hard to read
	      
	      (case (car form)

		((define-constant define-expansion) #t)

		((define define* define-macro define-macro* define-bacro define-bacro*)
		 (null? vars))

		((set! 
		  ;vector-set! list-set! hash-table-set! float-vector-set! int-vector-set! string-set! let-set! 
		  ;fill! string-fill! list-fill! vector-fill! 
		  ;reverse! sort!
		  )
		 (or (not (pair? (cdr form)))
		     (not (symbol? (cadr form)))
		     (memq (cadr form) vars)))
		
		((quote) #f)
		
		((case)
		 (or (not (pair? (cdr form)))
		     (side-effect-with-vars? (cadr form) env vars) ; the selector
		     (let case-effect? ((f (cddr form)))
		       (and (pair? f)
			    (or (not (pair? (car f)))
				(any? (lambda (ff) (side-effect-with-vars? ff env vars)) (cdar f))
				(case-effect? (cdr f)))))))
		
		((cond)
		 (or (not (pair? (cadr form)))
		     (let cond-effect? ((f (cdr form))
					(e env))
		       (and (pair? f)
			    (or (and (pair? (car f)) 
				     (any? (lambda (ff) (side-effect-with-vars? ff e vars)) (car f)))
				(cond-effect? (cdr f) e))))))
		
		((let let* letrec letrec*)
		 ;; here if the var value involves a member of vars, we have to add it to vars
		 (or (< (length form) 3)
		     (let ((syms (cadr form))
			   (body (cddr form)))
		       (when (symbol? (cadr form))
			 (set! syms (caddr form))
			 (set! body (cdddr form)))
		       (if (and (pair? vars)
				(pair? syms))
			   (for-each (lambda (sym)
				       (when (and (pair? sym)
						  (pair? (cdr sym))
						  (tree-set-member vars (cdr sym)))
					 (set! vars (cons (car sym) vars))))
				     syms))
		       (or (let let-effect? ((f syms) (e env) (v vars))
			     (and (pair? f)
				  (or (not (pair? (car f)))
				      (not (pair? (cdar f))) ; an error, reported elsewhere: (let ((x)) x)
				      (side-effect-with-vars? (cadar f) e v)
				      (let-effect? (cdr f) e v))))
			   (any? (lambda (ff) (side-effect-with-vars? ff env vars)) body)))))
		
		((do)
		 (or (< (length form) 3)
		     (not (list? (cadr form)))
		     (not (list? (caddr form)))
		     (let do-effect? ((f (cadr form))	(e env))
		       (and (pair? f)
			    (or (not (pair? (car f)))
				(not (pair? (cdar f)))
				(side-effect-with-vars? (cadar f) e vars)
				(and (pair? (cddar f))
				     (side-effect-with-vars? (caddar f) e vars))
				(do-effect? (cdr f) e))))
		     (any? (lambda (ff) (side-effect-with-vars? ff env vars)) (caddr form))
		     (any? (lambda (ff) (side-effect-with-vars? ff env vars)) (cdddr form))))
		
		;; ((lambda lambda*) (any? (lambda (ff) (side-effect-with-vars? ff env vars)) (cddr form))) ; this is trickier than it looks
		
		(else
		 (or (any? (lambda (f) (side-effect-with-vars? f env vars)) (cdr form)) ; any subform has a side-effect
		     (let ((sig (procedure-signature (car form))))       ; sig has func arg and it is not known safe
		       (and sig
			    (memq 'procedure? (cdr sig))
			    (call-with-exit
			     (lambda (return)
			       (for-each
				(lambda (sg arg)
				  (when (eq? sg 'procedure?)
				    (if (not (and (symbol? arg)
						  (hash-table-ref no-side-effect-functions arg)))
					(return #t))))
				(cdr sig) (cdr form))
			       #f))))))))))
	  
    
    (define (side-effect? form env)
      (side-effect-with-vars? form env ()))

    (define (just-constants? form env)
      ;; can we probably evaluate form given just built-in stuff?
      ;;   watch out here -- this is used later by 'if, so (defined 'hiho) should not be evalled to #f!
      (if (not (pair? form))
	  (constant? form)
	  (and (symbol? (car form))
	       (hash-table-ref no-side-effect-functions (car form))
	       (not (var-member (car form) env)) ; e.g. exp declared locally as a list
	       (every? (lambda (p) (just-constants? p env)) (cdr form)))))

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
    
    
    (define (repeated-member? lst env)
      (and (pair? lst)
	   (or (and (not (and (pair? (car lst))
			      (side-effect? (car lst) env)))
		    (pair? (cdr lst))
		    (member (car lst) (cdr lst)))
	       (repeated-member? (cdr lst) env))))
    
    (define (set-ref name form env)
      ;; if name is in env, set its "I've been referenced" flag
      (let ((data (var-member name env)))
	(if (var? data)
	    (begin
	      (set! (var-ref data) (+ (var-ref data) 1))
	      (if (and form (not (memq form (var-history data))))
		  (set! (var-history data) (cons form (var-history data)))))))
      env)
    
    (define (set-set name form env)
      (let ((data (var-member name env)))
	(when (var? data)
	  (set! (var-set data) (+ (var-set data) 1))
	  (if (not (memq form (var-history data)))
	      (set! (var-history data) (cons form (var-history data))))
	  (set! (var-signature data) #f)
	  (set! (var-ftype data) #f))))
    
    
    (define (proper-list lst)
      ;; return lst as a proper list
      (if (not (pair? lst))
	  lst
	  (cons (car lst) 
		(if (pair? (cdr lst)) 
		    (proper-list (cdr lst)) 
		    (if (null? (cdr lst)) 
			() 
			(list (cdr lst)))))))
    
    (define (keywords lst)
      (let ((count 0))
	(do ((p lst (cdr p)))
	    ((null? p) count)
	  (if (keyword? (car p))
	      (set! count (+ count 1))))))
					;(count-if keyword? lst))
    
    (define (eqv-selector clause)
      (if (pair? clause)
	  (case (car clause)
	    ((memq memv member) 
	     (and (= (length clause) 3)
		  (cadr clause)))
	    ((eq? eqv? = equal? char=? char-ci=? string=? string-ci=?)
	     (and (= (length clause) 3)
		  (if (code-constant? (cadr clause))
		      (caddr clause)
		      (cadr clause))))
	    ((or) 
	     (and (pair? (cdr clause))
		  (eqv-selector (cadr clause))))
	    ((not null? eof-object? zero? boolean?)
	     (and (pair? (cdr clause))
		  (cadr clause)))
 	    (else #f))
	  (memq clause '(else #t))))
    
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
    
    (define (focus-str str focus)
      (let ((len (length str)))
	(if (< len 40)
	    str
	    (let ((pos (string-position focus str))
		  (focus-len (length focus)))
	      (if (not pos)
		  str
		  (if (<= pos 20)
		      (string-append (substring str 0 (min 60 (- len 1) (+ focus-len pos 20))) " ...")
		      (string-append "... " (substring str (- pos 20) (min (- len 1) (+ focus-len pos 20))) " ...")))))))
    
    (define (check-star-parameters f args)
      (if (list-any? (lambda (k) (memq k '(:key :optional))) args)
	  (let ((kw (if (memq :key args) :key :optional)))
	    (format outport "~NC~A: ~A is no longer accepted: ~A~%" lint-left-margin #\space f kw 
		    (focus-str (object->string args) (symbol->string kw)))))
      
      (if (member 'pi args (lambda (a b) (or (eq? b 'pi) (and (pair? b) (eq? (car b) 'pi)))))
	  (format outport "~NC~A: parameter can't be a constant: ~A~%" lint-left-margin #\space f 
		  (focus-str (object->string args) "pi")))
      
      (let ((r (memq :rest args)))
	(when (pair? r)
	  (if (not (pair? (cdr r)))
	      (format outport "~NC~A: :rest parameter needs a name: ~A~%" lint-left-margin #\space f args)
	      (if (pair? (cadr r))
		  (format outport "~NC~A: :rest parameter can't specify a default value: ~A~%" lint-left-margin #\space f args)))))
      
      (let ((a (memq :allow-other-keys args)))
	(if (and (pair? a)
		 (pair? (cdr a)))
	    (format outport "~NC~A: :allow-other-keys should be at the end of the parameter list: ~A~%" lint-left-margin #\space f 
		    (focus-str (object->string args) ":allow-other-keys")))))
    
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
    
    
    (define relsub
      (let ((relops '((< <= > number?) (<= < >= number?) (> >= < number?) (>= > <= number?)
		      (char<? char<=? char>? char?) (char<=? char<? char>=? char?)  ; these never happen
		      (char>? char>=? char<? char?) (char>=? char>? char<=? char?)
		      (string<? string<=? string>? string?) (string<=? string<? string>=? string?)
		      (string>? string>=? string<? string?) (string>=? string>? string<=? string?))))
	(lambda (A B rel-op env)
	  (call-with-exit
	   (lambda (return)
	     (when (and (pair? A)
			(pair? B)
			(= (length A) (length B) 3))
	       (let ((Adata (assq (car A) relops))
		     (Bdata (assq (car B) relops)))
		 (when (and Adata Bdata)
		   (let ((op1 (car A))
			 (op2 (car B))
			 (A1 (cadr A))
			 (A2 (caddr A))
			 (B1 (cadr B))
			 (B2 (caddr B)))
		     (let ((x (if (and (not (number? A1))
				       (member A1 B))
				  A1 
				  (and (not (number? A2))
				       (member A2 B) 
				       A2))))
		       (when x
			 (let ((c1 (if (eq? x A1) A2 A1))
			       (c2 (if (eq? x B1) B2 B1))
			       (type (cadddr Adata)))
			   (if (or (side-effect? c1 env)
				   (side-effect? c2 env)
				   (side-effect? x env))
			       (return 'ok))
			   (if (eq? x A2) (set! op1 (caddr Adata)))
			   (if (eq? x B2) (set! op2 (caddr Bdata)))
			   
			   (let ((typer #f)
				 (gtes #f)
				 (gts #f)
				 (eqop #f))
			     (case type
			       ((number?)
				(set! typer number?)
				(set! gtes '(>= <=))
				(set! gts  '(< >))
				(set! eqop '=))
			       ((char?)
				(set! typer char?)
				(set! gtes '(char>=? char<=?))
				(set! gts  '(char<? char>?))
				(set! eqop 'char=?))
			       ((string?)
				(set! typer string?)
				(set! gtes '(string>=? string<=?))
				(set! gts  '(string<? string>?))
				(set! eqop 'string=?)))
			     
			     (case rel-op
			       ((and)
				(cond ((equal? c1 c2)
				       (cond ((eq? op1 op2)
					      (return `(,op1 ,x ,c1)))
					     
					     ((eq? op2 (cadr (assq op1 relops)))
					      (if (memq op2 gtes)
						  (return `(,op1 ,x ,c1))
						  (return `(,op2 ,x ,c1))))
					     
					     ((and (memq op1 gtes)
						   (memq op2 gtes))
					      (return `(,eqop ,x ,c1)))
					     
					     (else (return #f))))
				      
				      ((and (typer c1)
					    (typer c2))
				       (cond ((or (eq? op1 op2)
						  (eq? op2 (cadr (assq op1 relops))))
					      (if ((symbol->value op1) c1 c2)
						  (return `(,op1 ,x ,c1))
						  (return `(,op2 ,x ,c2))))
					     ((eq? op1 (caddr (assq op2 relops)))
					      (if ((symbol->value op1) c2 c1)
						  (return `(,op1 ,c2 ,x ,c1))
						  (if (memq op1 gts)
						      (return #f))))
					     ((and (eq? op2 (hash-table-ref reversibles (cadr (assq op1 relops))))
						   ((symbol->value op1) c1 c2))
					      (return #f))))))
			       
			       ((or)
				(cond ((equal? c1 c2)
				       (cond ((eq? op1 op2)
					      (return `(,op1 ,x ,c1)))
					     
					     ((eq? op2 (cadr (assq op1 relops)))
					      (if (memq op2 gtes)
						  (return `(,op2 ,x ,c1))
						  (return `(,op1 ,x ,c1))))
					     
					     ((and (memq op1 gts)
						   (memq op2 gts))
					      (return `(not (,eqop ,x ,c1))))
					     
					     (else (return #t))))
				      
				      ((and (typer c1)
					    (typer c2))
				       (cond ((or (eq? op1 op2)
						  (eq? op2 (cadr (assq op1 relops))))
					      (if ((symbol->value op1) c1 c2) 
						  (return `(,op2 ,x ,c2))
						  (return `(,op1 ,x ,c1))))
					     ((eq? op1 (caddr (assq op2 relops)))
					      (if ((symbol->value op1) c2 c1)
						  (return #t)))
					     ((and (eq? op2 (hash-table-ref reversibles (cadr (assq op1 relops))))
						   ((symbol->value op1) c2 c1))
					      (return #t)))))))))))))))
	     'ok)))))

    (define (simplify-boolean in-form true false env)
      ;; true and false are currently ignored!
      
      (define (classify e)
	(if (not (just-constants? e env))
	    e
	    (catch #t
	      (lambda ()
		(let ((val (eval e)))
		  (if (boolean? val)
		      val
		      e)))
	      (lambda ignore e))))
      
      (define (contradictory? ands)
	(let ((vars ()))
	  (call-with-exit
	   (lambda (return)
	     (do ((b ands (cdr b)))
		 ((null? b) #f)
	       (if (and (pair? b)
			(pair? (car b))
			(pair? (cdar b)))
		   (let ((func (caar b))
			 (args (cdar b)))
		     
		     (if (memq func '(eq? eqv? equal?))
			 (if (and (symbol? (car args))
				  (code-constant? (cadr args)))
			     (set! func (->type (cadr args)))
			     (if (and (symbol? (cadr args))
				      (code-constant? (car args)))
				 (set! func (->type (car args))))))
		     
		     (if (symbol? func)
			 (for-each
			  (lambda (arg)
			    (if (symbol? arg)
				(let ((type (assq arg vars)))
				  (if (not type)
				      (set! vars (cons (cons arg func) vars))
				      (if (not (compatible? (cdr type) func))
					  (return #t))))))
			  args)))))))))
      
      (define (and-redundant? arg1 arg2)
	(let ((type1 (car arg1))
	      (type2 (car arg2)))
	  (and (symbol? type1)
	       (symbol? type2)
	       (hash-table-ref bools1 type1)
	       (or (hash-table-ref bools1 type2)     ; return #f if not (obviously) redundant, else return which of the two to keep
		   (memq type2 '(= char=? string=? not eq?)))
	       (if (eq? type1 type2)
		   type1
		   (case type1
		     ((number? complex?) (or (and (memq type2 '(float? real? rational? integer?)) type2)
					     (and (memq type2 '(number? complex?)) type1)
					     (and (eq? type2 '=)
						  (let ((x (if (number? (caddr arg2)) (caddr arg2) (cadr arg2))))
						    (and (number? x)
							 (if (= x (floor x))
							     'memv
							     'eqv?))))))
		     ((real?)            (or (and (memq type2 '(float? rational? integer?)) type2)
					     (and (memq type2 '(number? complex?)) type1)
					     (and (eq? type2 '=)
						  (let ((x (if (real? (caddr arg2)) (caddr arg2) (cadr arg2))))
						    (and (real? x)
							 (if (= x (floor x))
							     'memv
							     'eqv?))))))
		     ((float?)           (and (memq type2 '(real? complex? number? inexact?)) type1))
		     ((rational?)        (or (and (eq? type2 'integer?) type2)
					     (and (memq type2 '(real? complex? number? exact?)) type1)
					     (and (eq? type2 '=)
						  (or (rational? (caddr arg2))
						      (rational? (cadr arg2)))
						  'eqv?)))
		     ((integer?)         (or (and (memq type2 '(real? rational? complex? number? exact?)) type1)
					     (and (eq? type2 '=)
						  (or (integer? (caddr arg2))
						      (integer? (cadr arg2)))
						  'eqv?)))
		     ((exact?)           (and (memq type2 '(rational? integer?)) type2))
		     ((even? odd?)       (and (memq type2 '(integer? rational? real? complex? number?)) type1)) ; not zero? -> 0.0
		     ((zero?)            (and (memq type2 '(complex? number? real?)) type1))
		     ((negative? positive?) (and (eq? type2 'real?) type1))
		     ((inexact?)         (and (eq? type2 'float?) type2))
		     ((infinite? nan?)   (and (memq type2 '(number? complex? inexact?)) type1))
		     ((vector?)          (and (memq type2 '(float-vector? int-vector?)) type2))
		     ((float-vector? int-vector?) (and (eq? type2 'vector?) type1))
		     ((symbol?)          (or (and (memq type2 '(keyword? gensym? constant?)) type2)
					     (and (eq? type2 'eq?)
						  (or (quoted-symbol? (cadr arg2))
						      (quoted-symbol? (caddr arg2)))
						  'eq?)))
		     ((keyword?)         (or (and (memq type2 '(symbol? constant?)) type1)
					     (and (eq? type2 'eq?)
						  (or (keyword? (cadr arg2))
						      (keyword? (caddr arg2)))
						  'eq?)))
		     ((gensym? defined? provided? constant?) (and (eq? type2 'symbol?) type1))
		     ((boolean?)         (and (or (eq? type2 'not) 
						  (and (eq? type2 'eq?)
						       (or (boolean? (cadr arg2))
							   (boolean? (caddr arg2)))))
					      type2))
		     ((list?)            (and (memq type2 '(null? pair? proper-list?)) type2))
		     ((null?)            (and (memq type2 '(list? proper-list?)) type1))
		     ((pair?)            (and (eq? type2 'list?) type1))
		     ((proper-list?)     (and (eq? type2 'null?) type2))
		     ((string?)          (or (and (eq? type2 'byte-vector?) type2)
					     (and (eq? type2 'string=?)
						  (or (eq? (->type (cadr arg2)) 'string?)
						      (eq? (->type (caddr arg2)) 'string?))
						  'equal?)))
		     ((char?)            (and (eq? type2 'char=?)
					      (or (eq? (->type (cadr arg2)) 'char?)
						  (eq? (->type (caddr arg2)) 'char?))
					      'eqv?))
		     ((char-numeric? char-whitespace? char-alphabetic? char-upper-case? char-lower-case?) (and (eq? type2 'char?) type1))
		     ((byte-vector? directory? file-exists?) (and (eq? type2 'string?) type1))
		     (else #f))))))

      (define (and-redundants env . args)
	(let ((locals ())
	      (diffs #f))
	  (do ((p args (cdr p)))
	      ((or (null? p)
		   (not (and (pair? (car p))
			     (pair? (cdar p))
			     (hash-table-ref bools1 (caar p)))))
	       (and (null? p)
		    (pair? locals)
		    (or diffs
			(any? (lambda (a) (pair? (cddr a))) locals))
		    (let ((keepers ()))
		      (for-each (lambda (a)
				  (cond ((null? (cddr a))
					 (set! keepers (cons (cadr a) keepers)))

					((null? (cdddr a)) 
					 (let ((res (apply and-redundant? (reverse (cdr a)))))
					   (if res
					       (begin
						 (set! keepers (cons (if (eq? res (caadr a)) (cadr a) (caddr a)) keepers))
						 (set! diffs #t))
					       (set! keepers (cons (cadr a) (cons (caddr a) keepers))))))

					(else
					 (let ((ar (reverse (cdr a))))
					   (let ((res1 (and-redundant? (car ar) (cadr ar)))   ; if res1 either 1 or 2 is out
						 (res2 (and-redundant? (cadr ar) (caddr ar))) ; if res2 either 2 or 3 is out
						 (res3 (and-redundant? (car ar) (caddr ar)))) ; if res3 either 1 or 3 is out
					     ;; (format *stderr* "ar: ~A, res1: ~A, res2: ~A, res3: ~A~%" ar res1 res2 res3)
					     ;; only in numbers can 3 actually be reducible
					     (if (not (or res1 res2 res3))
						 (set! keepers (append (cdr a) keepers))
						 (begin
						   (set! diffs #t)
						   (if (and (or (not res1)
								(eq? res1 (caar ar)))
							    (or (not res3)
								(eq? res3 (caar ar))))
						       (set! keepers (cons (car ar) keepers)))
						   (if (and (or (not res1)
								(eq? res1 (caadr ar)))
							    (or (not res2)
								(eq? res2 (caadr ar))))
						       (set! keepers (cons (cadr ar) keepers)))
						   (if (and (or (not res2)
								(eq? res2 (car (caddr ar))))
							    (or (not res3)
								(eq? res3 (car (caddr ar)))))
						       (set! keepers (cons (caddr ar) keepers)))
						   (if (pair? (cdddr ar))
						       (set! keepers (append (reverse (cdddr ar)) keepers))))))))))
				(reverse locals))
		      (and diffs (reverse keepers)))))
	    (let* ((bool (car p))
		   (local (assoc (cadr bool) locals)))
	      (if (pair? local)
		  (if (member bool (cdr local))
		      (set! diffs #t)
		      (set-cdr! local (cons bool (cdr local))))
		  (set! locals (cons (list (cadr bool) bool) locals)))))))
      
      
      (define (and-not-redundant arg1 arg2)
	(let ((type1 (car arg1))    ; (? ...)
	      (type2 (caadr arg2))) ; (not (? ...))
	  (and (symbol? type1)
	       (symbol? type2)
	       (or (hash-table-ref bools1 type1)
		   (memq type1 '(= char=? string=?)))
	       (hash-table-ref bools1 type2)
	       (if (eq? type1 type2)     ; (and (?) (not (?))) -> #f
		   'contradictory
		   (case type1
		     ((pair?) (if (eq? type2 'list?)
				  'contradictory
				  (and (not (eq? type2 'proper-list)) arg1)))   ; (and (pair?) (not (?)))
		     ((null?) (if (eq? type2 'list?)
				  'contradictory
				  arg1))
		     ((list?) (if (eq? type2 'pair?)
				  'null?
				  (if (eq? type2 'null?)
				      'pair?
				      (and (not (eq? type2 'proper-list?)) arg1))))
		     ((proper-list?) (if (memq type2 '(list? pair?))
					 'contradictory
					 (and (not (eq? type2 'null?)) arg1)))
		     ((symbol?) (and (not (memq type2 '(keyword? gensym? constant?))) arg1))
		     ((constant?) (and (eq? type2 'symbol?)
				       'contradictory))
		     ((char=?)  (if (eq? type2 'char?)
				    'contradictory
				    (and (or (char? (cadr arg1))
					     (char? (caddr arg1)))
					 `(eqv? ,@(cdr arg1))))) ; arg2 might be (not (eof-object?...))
		     ((real?) (case type2
				((rational? exact?)  `(float? ,@(cdr arg1)))
				((inexact?)          `(rational? ,@(cdr arg1)))
				((complex? number?)  'contradictory)
				(else (and (not (memq type2 '(negative? positive? even? odd? zero? integer?))) 
					   arg1))))
		     ((integer?) (if (memq type2 '(real? complex? number? rational? exact?))
				     'contradictory
				     (and (memq type2 '(float? inexact? infinite? nan?))
					  arg1)))
		     ((rational?) (if (memq type2 '(real? complex? number? exact?))
				      'contradictory
				      (and (memq type2 '(float? inexact? infinite? nan?))
					   arg1)))
		     ((complex? number?) (and (memq type2 '(complex? number?))
					      'contradictory))
		     ((float?) (if (memq type2 '(real? complex? number? inexact?))
				   'contradictory
				   (and (memq type2 '(rational? integer? exact?))
					arg1)))
		     ((exact?) (if (eq? type2 'rational?)
				   'contradictory
				   (and (memq type2 '(inexact? infinite? nan?))
					arg1)))
		     ((even? odd?) (if (memq type2 '(integer? exact? rational? real? number? complex?))
				       'contradictory
				       (and (memq type2 '(infinite? nan?))
					    arg1)))
		     ((zero? negative? positive?) (and (memq type2 '(complex? number? real?))
						       'contradictory))
		     ((infinite? nan?) (if (memq type2 '(number? complex? inexact?))
					   'contradictory
					   (and (memq type2 '(integer? rational? exact? even? odd?))
						arg1)))
		     ((char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)
		      (and (eq? type2 'char?)
			   'contradictory))
		     ((directory? file-exists?)
		      (and (memq type2 '(string? sequence?))
			   'contradictory))
		     (else 
		      ;; none of the rest happen
		      #f))))))
      
      (define (or-not-redundant arg1 arg2)
	(let ((type1 (car arg1))    ; (? ...)
	      (type2 (caadr arg2))) ; (not (? ...))
	  (and (symbol? type1)
	       (symbol? type2)
	       (or (hash-table-ref bools type1)
		   (memq type1 '(= char=? string=?)))
	       (hash-table-ref bools type2)
	       (if (eq? type1 type2)     ; (or (?) (not (?))) -> #t
		   'fatuous
		   (case type1
		     ((null?) (if (eq? type2 'list?) ; not proper-list? here
				  `(not (pair? ,(cadr arg1)))
				  (and (not (eq? type2 'proper-list?))
				       arg2)))
		     ((eof-object?) arg2) ; not keyword? here because (or (not (symbol? x)) (keyword? x)) is not reducible to (not (symbol? x))
		     ((string?) (and (not (eq? type2 'byte-vector?)) arg2))
		     (else #f))))))
      
      (define (bsimp x) ; quick check for common easy cases
	(set! last-simplify-boolean-line-number line-number)
	(if (not (and (pair? x)
		      (pair? (cdr x))))
	    x
	    (case (car x)
	      ((and) (and (cadr x)              ; (and #f ...) -> #f
			  x))
	      ((or) (if (and (cadr x)           ; (or #t ...) -> #t
			     (code-constant? (cadr x)))
			(cadr x)
			x))
	      (else 
	       (if (not (and (= (length x) 2)
			     (pair? (cadr x))
			     (symbol? (caadr x))))
		   x
		   (let ((rt (if (eq? (caadr x) 'quote)
				 (->simple-type (cadadr x))
				 (return-type (caadr x) env)))
			 (head (car x)))
		     (or (and (subsumes? head rt) #t) ; don't return the memq list!
			 (and (or (memq rt '(#t #f values))
				  (any-compatible? head rt))
			      (case head
				((null?) (if (eq? (caadr x) 'list)
					     (null? (cdadr x))
					     x))
				((pair?) (if (eq? (caadr x) 'list)
					     (pair? (cdadr x))
					     x))
				((negative?) (and (not (memq (caadr x) non-negative-ops))
						  x))
				(else x))))))))))
      
      (define (bcomp x) ; not so quick...
	;(format *stderr* "~A ~A ~A~%" x true false)
	(cond ((not (pair? x))
	       x)

	      ((eq? (car x) 'and)
	       (call-with-exit
		(lambda (return)
		  (let ((newx (list 'and)))
		    (do ((p (cdr x) (cdr p))
			 (sidex newx)
			 (endx newx))
			((null? p) newx)
		      (let ((next (car p)))
					;(format *stderr* "and ~A ~A ~A~%" next true false)
			(if (or (not next)        ; #f in and -> end of expr
				(member next false))
			    (if (eq? sidex newx)  ; no side-effects
				(return #f)       
				(begin
				  (set-cdr! endx (list #f))
				  (return newx)))
			    (if (or (code-constant? next)  ; (and ... true-expr ...)
				    (member next sidex)    ; if a member, and no side-effects since, it must be true
				    (member next true))
				(if (and (null? (cdr p))
					 (not (equal? next (car endx))))
				    (set-cdr! endx (list next)))
				(begin
				  (set-cdr! endx (list next))
				  (set! endx (cdr endx))
				  (if (side-effect? next env)
				      (set! sidex endx)))))))))))
		
	      ((not (eq? (car x) 'or))
	       x)

	      (else
	       (call-with-exit
		(lambda (return)
		  (let ((newx (list 'or)))
		    (do ((p (cdr x) (cdr p))
			 (sidex newx)
			 (endx newx))
			((null? p) newx)
		      (let ((next (car p)))
					;(format *stderr* "or ~A ~A ~A~%" next true false)
			(if (or (and next                 ; (or ... #t ...)
				     (code-constant? next))
				(member next true))
			    (begin
			      (set-cdr! endx (list next))
			      (return newx))          ; we're done since this is true
			    (if (or (not next)
				    (member next sidex) ; so its false in some way
				    (member next false))
				(if (and (null? (cdr p))
					 (not (equal? next (car endx))))
				    (set-cdr! endx (list next)))
				(begin
				  (set-cdr! endx (list next))
				  (set! endx (cdr endx))
				  (if (side-effect? next env)
				      (set! sidex endx)))))))))))))
      
      ;; start of simplify-boolean code
      ;;   this is not really simplify boolean as in boolean algebra because in scheme there are many unequal truths, but only one falsehood
      ;;   'and and 'or are not boolean operators in a sense
      
      (and (not (or (member in-form false)
		    (and (pair? in-form)
			 (eq? (car in-form) 'not)
			 (pair? (cdr in-form)) ; (not)!
			 (member (cadr in-form) true))))
	   (or (and (member in-form true) #t)
	       (and (pair? in-form)
		    (eq? (car in-form) 'not)
		    (pair? (cdr in-form))
		    (member (cadr in-form) false) 
		    #t)
      (if (not (pair? in-form))
	  in-form
	  (let ((form (bcomp (bsimp in-form))))
	    (if (not (and (pair? form)
			  (memq (car form) '(or and not))))
		(classify form)
		(let ((len (length form)))
		  (let ((op (if (eq? (car form) 'or) 'and 
				(and (eq? (car form) 'and) 'or))))
		    (if (and op
			     (>= len 3)
			     (every? (lambda (p) (and (pair? p) 
						      (eq? (car p) op) 
						      (= (length p) 3)))
				     (cdr form)))
			(let ((first (cadadr form))
			      (last (caddr (cadr form))))
			  (if (every? (lambda (p) (equal? (cadr p) first)) (cddr form))
			      (set! form `(,op ,first (,(car form) ,last ,(caddr (caddr form)))))
			      (if (every? (lambda (p) (equal? (caddr p) last)) (cddr form))
				  (set! form `(,op (,(car form) ,first ,(cadr (caddr form))) ,last)))))))
		  ;; (or (and A B) (and A C)) -> (and A (or B C))
		  ;; (or (and A B) (and C B)) -> (and (or A C) B)
		  ;; (and (or A B) (or A C)) -> (or A (and B C))
		  ;; (and (or A B) (or C B)) -> (or (and A C) B)
		  
		  (case (car form)
		    ;; --------------------------------
		    ((not)
		     (if (not (= len 2))
			 form
			 (let* ((arg (cadr form))
				(val (if (and (pair? arg)
					      (memq (car arg) '(and or not)))
					 (classify (simplify-boolean arg true false env))
					 (classify arg)))
				(arg-op (and (pair? arg) 
					     (car arg))))
			   
			   (cond ((boolean? val) 
				  (not val))
				 
				 ((or (code-constant? arg)
				      (and (pair? arg)
					   (symbol? arg-op)
					   (hash-table-ref no-side-effect-functions arg-op)
					   (let ((ret (return-type arg-op env)))
					     (and (or (symbol? ret) (pair? ret))
						  (not (return-type-ok? 'boolean? ret))))
					   (not (var-member arg-op env))))
				  #f)
				 
				 ((and (pair? arg)               ; (not (not ...)) -> ...
				       (pair? (cdr arg))         ; this is usually internally generated, so the message about (and x #t) is in special-cases below
				       (eq? arg-op 'not))
				  (cadr arg))

				 ((and (pair? arg)               ; (not (or|and x (not y)...)) -> (and|or (not x) y ...)
				       (memq arg-op '(and or))
				       (pair? (cdr arg))
				       (any? (lambda (p) 
					       (and (pair? p) 
						    (eq? (car p) 'not)))
					     (cdr arg)))
				  (let ((rel (if (eq? arg-op 'or) 'and 'or)))
				    `(,rel ,@(map (lambda (p)
						    (if (and (pair? p)
							     (eq? (car p) 'not))
							(cadr p)
							(simplify-boolean `(not ,p) () () env)))
						  (cdr arg)))))
				 
				 ((not (equal? val arg))
				  `(not ,val))
				 
				 ((and (pair? arg)
				       (<= (length arg) 3)) ; avoid (<= 0 i 12) and such
				  (case arg-op
				    ((< > <= >= odd? even? exact? inexact?char<? char>? char<=? char>=? string<? string>? string<=? string>=?
					char-ci<? char-ci>? char-ci<=? char-ci>=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
				     `(,(hash-table-ref notables arg-op) ,@(cdr arg)))
				    
				    ;; null? is not quite right because (not (null? 3)) -> #t
				    ;; char-upper-case? and lower are not switchable here
				    
				    ((zero?)       ; (not (zero? (logand p 2^n | (ash 1 i)))) -> (logbit? p i)
				     (let ((zarg (cadr arg)))  ; (logand...)
				       (if (and (pair? zarg)
						(eq? (car zarg) 'logand)
						(pair? (cdr zarg))
						(pair? (cddr zarg))
						(null? (cdddr zarg)))
					   (let ((arg1 (cadr zarg))
						 (arg2 (caddr zarg))) ; these are never reversed
					     (or (and (pair? arg2)
						      (pair? (cdr arg2))
						      (eq? (car arg2) 'ash)
						      (eqv? (cadr arg2) 1)
						      `(logbit? ,arg1 ,(caddr arg2)))
						 (and (integer? arg2)
						      (positive? arg2)
						      (zero? (logand arg2 (- arg2 1))) ; it's a power of 2
						      `(logbit? ,arg1 ,(floor (log arg2 2)))) ; floor for freeBSD?
						 form))
					   form)))

				    (else form)))
				 
				 (else form)))))
		    ;; --------------------------------
		    ((or)
		     (case len
		       ((1) #f)
		       ((2) (if (code-constant? (cadr form)) (cadr form) (classify (cadr form))))
		       (else
			(call-with-exit
			 (lambda (return)
			   (when (= len 3)
			     (let ((arg1 (cadr form))
				   (arg2 (caddr form)))

			       (if (and (pair? arg2)     ; (or A (and ... A ...)) -> A
					(eq? (car arg2) 'and)
					(member arg1 (cdr arg2))
					(not (side-effect? arg2 env)))
				   (return arg1))
			       (if (and (pair? arg1)     ; (or (and ... A) A) -> A
					(eq? (car arg1) 'and)
					(equal? arg2 (list-ref arg1 (- (length arg1) 1)))
					(not (side-effect? arg1 env)))
				   (return arg2))				   
			       
			       (when (and (pair? arg1)
					  (pair? arg2))
				 
				 (when (and (eq? (car arg1) 'and)
					    (eq? (car arg2) 'and)
					    (= 3 (length arg1) (length arg2))
					    ;; (not (side-effect? arg1 env)) ; maybe??
					    (or (equal? (cadr arg1) `(not ,(cadr arg2)))
						(equal? `(not ,(cadr arg1)) (cadr arg2)))
					    (not (equal? (caddr arg1) `(not ,(caddr arg2))))
					    (not (equal? `(not ,(caddr arg1)) (caddr arg2))))
				   ;; kinda dumb, but common: (or (and A B) (and (not A) C)) -> (if A B C)
				   ;;    the other side: (and (or A B) (or (not A) C)) -> (if A C (and B #t)), but it never happens
				   (lint-format "perhaps ~A" 'or 
						(lists->string form
							       (if (and (pair? (cadr arg1))
									(eq? (caadr arg1) 'not))
								   `(if ,(cadr arg2) ,(caddr arg2) ,(caddr arg1))
								   `(if ,(cadr arg1) ,(caddr arg1) ,(caddr arg2))))))
				 (let ((t1 (and (pair? (cdr arg1))
						(pair? (cdr arg2))
						(or (equal? (cadr arg1) (cadr arg2))
						    (and (pair? (cddr arg2))
							 (null? (cdddr arg2))
							 (equal? (cadr arg1) (caddr arg2))))
						(not (side-effect? arg1 env))
						(and-redundant? arg1 arg2))))
				   (if t1
				       (return (if (eq? t1 (car arg1)) arg2 arg1))))
				 
				 ;; if all clauses are (eq-func x y) where one of x/y is a symbol|simple-expr repeated throughout
				 ;;   and the y is a code-constant, or -> memq and friends.  
				 ;;   This could also handle cadr|caddr reversed, but it apparently never happens.
				 (if (and (or (and (eq? (car arg2) '=)
						   (memq (car arg1) '(< > <= >=)))
					      (and (eq? (car arg1) '=)
						   (memq (car arg2) '(< > <= >=))))
					  (= (length arg1) 3)
					  (equal? (cdr arg1) (cdr arg2)))
				     (return `(,(if (or (memq (car arg1) '(< <=))
							(memq (car arg2) '(< <=)))
						    '<= '>=)
					       ,@(cdr arg1))))
				 
				 ;; this makes some of the code above redundant
				 (let ((rel (relsub arg1 arg2 'or env)))
				   (if (or (boolean? rel)
					   (pair? rel))
				       (return rel)))

				 ;; (or (pair? x) (null? x)) -> (list? x)
				 (if (and (memq (car arg1) '(null? pair?))
					  (memq (car arg2) '(null? pair?))
					  (not (eq? (car arg1) (car arg2)))
					  (equal? (cadr arg1) (cadr arg2)))
				     (return `(list? ,(cadr arg1))))
				 
				 ;; (or (and A B) (and (not A) (not B))) -> (eq? (not A) (not B))
				 ;; more accurately (if A B (not B)), but every case I've seen is just boolean
				 ;; perhaps also (or (not (or A B)) (not (or (not A) (not B)))), but it never happens
				 (let ((a1 (cadr form))
				       (a2 (caddr form)))
				   (and (pair? a1)
					(pair? a2)
					(eq? (car a1) 'and)
					(eq? (car a2) 'and)
					(= (length a1) 3)
					(= (length a2) 3)
					(let ((A (if (and (pair? (cadr a1)) (eq? (caadr a1) 'not)) (cadadr a1) (cadr a1)))
					      (B (if (and (pair? (caddr a1)) (eq? (caaddr a1) 'not)) (cadr (caddr a1)) (caddr a1))))
					  (if (or (equal? form `(or (and ,A ,B) (and (not ,A) (not ,B))))
						  (equal? form `(or (and (not ,A) (not ,B)) (and ,A ,B))))
					      (return `(eq? (not ,A) (not ,B))))
					  (if (or (equal? form `(or (and ,A (not ,B)) (and (not ,A) ,B)))
						  (equal? form `(or (and (not ,A) ,B) (and ,A (not ,B)))))
					      (return `(not (eq? (not ,A) (not ,B))))))))
				 
				 (when (and (pair? (cdr arg1))
					    (pair? (cdr arg2))
					    (not (eq? (car arg1) (car arg2))))
				   (when (subsumes? (car arg1) (car arg2))
				     (return arg1))

				   (if (eq? (car arg1) 'not)
				       (let ((temp arg1))
					 (set! arg1 arg2)
					 (set! arg2 temp)))
				   (if (and (eq? (car arg2) 'not)
					    (pair? (cadr arg2))
					    (pair? (cdadr arg2))
					    (not (eq? (caadr arg2) 'let?))
					    (or (equal? (cadr arg1) (cadadr arg2))
						(and (pair? (cddr arg1))
						     (equal? (caddr arg1) (cadadr arg2))))
					    (eq? (return-type (car arg1) env) 'boolean?)
					    (eq? (return-type (caadr arg2) env) 'boolean?))
				       (let ((t2 (or-not-redundant arg1 arg2)))
					 (when t2 
					   (if (eq? t2 'fatuous)
					       (return #t)
					       (if (pair? t2)
						   (return t2))))))
				   ))))
			   
			   (let ((nots 0)
				 (revers 0))
			     (if (every? (lambda (a)                ; (and (not (pair? x)) (not (null? x))) -> (not (list? x))
					   (and (pair? a)
						(if (eq? (car a) 'not)
						    (set! nots (+ nots 1))
						    (and (hash-table-ref notables (car a))
							 (set! revers (+ revers 1))))))
					 (cdr form))
				 (if (zero? revers)
				     (let ((sf (simplify-boolean `(and ,@(map cadr (cdr form))) true false env)))
				       (return (simplify-boolean `(not ,sf) () () env)))
				     (if (> nots revers)
					 (let ((nf (simplify-boolean `(and ,@(map (lambda (p)
										    (if (eq? (car p) 'not)
											(cadr p)
											`(,(hash-table-ref notables (car p)) ,@(cdr p))))
										  (cdr form)))
								     true false env)))
					   (return (simplify-boolean `(not ,nf) () () env)))))))
			   
			   (let ((sym #f)
				 (eqf #f)
				 (vals ()))
			     
			     (define (constant-arg p)
			       (if (code-constant? (cadr p))
				   (set! vals (cons (cadr p) vals))
				   (and (code-constant? (caddr p))
					(set! vals (cons (caddr p) vals)))))
			     
			     (define (upgrade-eqf)
			       (set! eqf (if (memq eqf '(string=? string-ci=? = equal?))
					     'equal?
					     (if (memq eqf '(#f eq?)) 'eq? 'eqv?))))
			     
			     (if (every? (lambda (p)
					   (and (pair? p)
						(if (not sym)
						    (set! sym (eqv-selector p))
						    (equal? sym (eqv-selector p)))
						(or (not (memq eqf '(char-ci=? string-ci=? =)))
						    (memq (car p) '(char-ci=? string-ci=? =)))
						
						;; = can't share: (equal? 1 1.0) -> #f, so (or (not x) (= x 1)) can't be simplified
						;;   except via member+morally-equal? but that brings in float-epsilon and NaN differences.
						;;   We could add both: 1 1.0 as in cond?
						;;
						;; another problem: using memx below means the returned value of the expression
						;;   may not match the original (#t -> '(...)), so perhaps we should add a one-time
						;;   warning about this, and wrap it in (pair? (mem...)) as an example.
						;;
						;; and another thing... the original might be broken: (eq? x #(1)) where equal?
						;;   is more sensible, but that also changes the behavior of the expression:
						;;   (memq x '(#(1))) may be #f (or #t!) when (member x '(#(1))) is '(#(1)).
						;;
						;; I think I'll try to turn out a more-or-less working expression, but warn about it.
						
						(case (car p) 
						  ((string=? equal?)
						   (set! eqf (if (or (not eqf)
								     (eq? eqf (car p)))
								 (car p)
								 'equal?))
						   (and (= (length p) 3)
							(constant-arg p)))
						  
						  ((char=?)
						   (if (memq eqf '(#f char=?))
						       (set! eqf 'char=?)
						       (if (not (eq? eqf 'equal?))
							   (set! eqf 'eqv?)))
						   (and (= (length p) 3)
							(constant-arg p)))
						  
						  ((eq? eqv?)
						   (let ((leqf (car (->eqf (->type (if (code-constant? (cadr p)) (cadr p) (caddr p)))))))
						     (cond ((not eqf) 
							    (set! eqf leqf))
							   
							   ((or (memq leqf '(#t equal?))
								(not (eq? eqf leqf)))
							    (set! eqf 'equal?))
							   
							   ((memq eqf '(#f eq?))
							    (set! eqf leqf))))
						   (and (= (length p) 3)
							(constant-arg p)))
						  
						  ((char-ci=? string-ci=? =)
						   (and (or (not eqf)
							    (eq? eqf (car p)))
							(set! eqf (car p))
							(= (length p) 3)
							(constant-arg p)))
						  
						  ((eof-object?)
						   (upgrade-eqf)
						   (set! vals (cons #<eof> vals)))
						  
						  ((not)
						   (upgrade-eqf)
						   (set! vals (cons #f vals)))
						  
						  ((boolean?) 
						   (upgrade-eqf)
						   (set! vals (cons #f (cons #t vals))))
						  
						  ((zero?)
						   (if (memq eqf '(#f eq?)) (set! eqf 'eqv?))
						   (set! vals (cons 0 (cons 0.0 vals))))
						  
						  ((null?)
						   (upgrade-eqf)
						   (set! vals (cons () vals)))
						  
						  ((memq memv member)
						   (cond ((eq? (car p) 'member)
							  (set! eqf 'equal?))
							 
							 ((eq? (car p) 'memv)
							  (set! eqf (if (eq? eqf 'string=?) 'equal? 'eqv?)))
							 
							 ((not eqf)
							  (set! eqf 'eq?)))
						   (and (= (length p) 3)
							(pair? (caddr p))
							(eq? 'quote (caaddr p))
							(pair? (cadr (caddr p)))
							(set! vals (append (cadr (caddr p)) vals))))
						  
						  (else #f))))
					 (cdr form))
				 
				 (let* ((func (case eqf 
						((eq?) 'memq) 
						((eqv? char=?) 'memv) 
						(else 'member)))
					(equals (if (and (eq? func 'member)
							 (not (eq? eqf 'equal?)))
						    (list eqf)
						    ()))
					(elements (lint-remove-duplicates (map (lambda (v) 
										 (if (pair? v) ; quoted case
										     (cadr v)  ;   so unquote for quoted list below
										     v)) 
									       vals) 
									  env)))
				   (return (cond ((null? (cdr elements))
						  `(,eqf ,sym ,@elements))
						 
						 ((and (eq? eqf 'char=?)
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
						  `(,func ,sym ',(reverse elements) ,@equals))))))

			     (let ((new-form ())
				   (retry #f))
			       (do ((exprs (cdr form) (cdr exprs)))
				   ((null? exprs) 
				    (return (and (pair? new-form)
						 (if (null? (cdr new-form))
						     (car new-form)
						     (if retry
							 (simplify-boolean `(or ,@(reverse new-form)) () () env)
							 `(or ,@(reverse new-form)))))))
				 (let ((val (classify (car exprs))))
				   (if (and (pair? val)
					    (memq (car val) '(and or not)))
				       (set! val (classify (simplify-boolean val true false env))))
				   
				   (if (not (or retry
						(equal? val (car exprs))))
				       (set! retry #t))
				   
				   (if val                                   ; #f in or is ignored
				       (cond ((or (eq? val #t)               ; #t or any non-#f constant in or ends the expression
						  (code-constant? val))
					      (set! new-form (if (null? new-form) ; (or x1 123) -> value of x1 first
								 (list val)
								 (cons val new-form)))
					      ;; reversed when returned
					      (set! exprs '(#t)))
					     
					     ((and (pair? val)                       ; (or ...) -> splice into current
						   (eq? (car val) 'or))
					      (set! exprs (append val (cdr exprs)))) ; we'll skip the 'or in do step
					     
					     ((not (or (memq val new-form)
						       (and (pair? val)         ;   and redundant tests
							    (hash-table-ref bools1 (car val))
							    (any? (lambda (p)
								    (and (pair? p)
									 (subsumes? (car p) (car val))
									 (equal? (cadr val) (cadr p))))
								  new-form))))
					      (set! new-form (cons val new-form))))))))))))))
		    ;; --------------------------------
		    ((and)
		     (case len
		       ((1) #t)
		       ((2) (classify (cadr form)))
		       (else
			(and (not (contradictory? (cdr form)))
			     (call-with-exit
			      (lambda (return)
				(when (= len 3)
				  (let ((arg1 (cadr form))
					(arg2 (caddr form)))
				    
				    (if (and (pair? arg2)     ; (and A (or A ...)) -> A
					     (eq? (car arg2) 'or)
					     (equal? arg1 (cadr arg2))
					     (not (side-effect? arg2 env)))
					(return arg1))
				    (if (and (pair? arg1)     ; (and (or ... A ...) A) -> A
					     (eq? (car arg1) 'or)
					     (member arg2 (cdr arg1))
					     (not (side-effect? arg1 env)))
					(return arg2))
				    
				    (when (and (symbol? arg1)                       ; (and x (pair? x)) -> (pair? x)
					       (pair? arg2)
					       (pair? (cdr arg2))
					       (eq? arg1 (cadr arg2)))
				      (if (eq? (car arg2) 'not)
					  (return #f))
				      (if (not (or (memq (car arg2) '(and or not list cons vector))
						   (side-effect? arg2 env)))
					  (let ((v (var-member arg1 env)))
					    (if (not (and (var? v)
							  (pair? (var-history v))
							  (member #f (var-history v)
								  (lambda (a b)
								    (and (pair? b)
									 (memq (car b) '(char-position string-position format string->number
											 assoc assq assv memq memv member)))))))
						(let ((sig (arg-signature (car arg2) env)))
						  (let ((arg-type (and (pair? sig)
								       (pair? (cdr sig))
								       (symbol? (cadr sig)))))
						    (if arg-type
							(format outport "~NCin ~A, perhaps change ~S to ~S~%"
								lint-left-margin #\space 
								(truncated-list->string form) 
								`(and ,arg1 ...)
								`(and (,(cadr sig) ,arg1) ...))))))))
				      
				      (if (hash-table-ref bools1 (car arg2))
					  (return arg2)))
				    
				    (if (and (not (side-effect? arg1 env))
					     (equal? arg1 arg2))                  ; (and x x) -> x
					(return arg1))
				    
				    (when (and (pair? arg1)
					       (pair? arg2)
					       (pair? (cdr arg1))
					       (pair? (cdr arg2)))
				      
				      (let ((t1 (and (or (equal? (cadr arg1) (cadr arg2))
							 (and (pair? (cddr arg2))
							      (null? (cdddr arg2))
							      (equal? (cadr arg1) (caddr arg2))))
						     (not (side-effect? arg1 env))
						     (and-redundant? arg1 arg2)))) ; (and (integer? x) (number? x)) -> (integer? x)
					(if t1
					    (return (cond 
						     ((memq t1 '(eq? eqv? equal?))
						      `(,t1 ,@(cdr arg2)))
						     
						     ((eq? t1 'memv)
						      (let ((x (if (equal? (cadr arg1) (cadr arg2)) (caddr arg2) (cadr arg2))))
							(if (rational? x)
							    `(memv ,(cadr arg1) '(,x ,(* 1.0 x)))
							    `(memv ,(cadr arg1) '(,(floor x) ,x)))))
						     
						     ((eq? t1 (car arg1)) arg1)
						     (else arg2)))))
				      
				      ;(format *stderr* "~A ~A~%" arg1 arg2)
				      (if (and (hash-table-ref reversibles (car arg1))
					       (pair? (cddr arg1))
					       (null? (cdddr arg1))
					       (pair? (cddr arg2))
					       (null? (cdddr arg2))
					       (not (side-effect? arg2 env))          ; arg1 is hit in any case
					       (or (eq? (car arg1) (car arg2))        ; either ops are equal or
						   (let ((rf (hash-table-ref reversibles (car arg2))))  ;    try reversed op for arg2
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
					    
					    ;(format *stderr* "~A ~A ~A ~A ~A~%" op1 arg1-1 arg1-2 arg2-1 arg2-2)
					    (return
					     (cond ((equal? arg1-2 arg2-1)       ; (and (op x y) (op y z)) -> (op x y z)
						    (if (equal? arg1-1 arg2-2)
							(if (memq op1 '(= char=? string=? char-ci=? string-ci=?))
							    arg1 
							    (and (memq op1 '(<= >= char<=? char>=? string<=? string>=?
									    char-ci<=? char-ci>=? string-ci<=? string-ci>=?))
								`(,(case op1 
								     ((>= <=) '=)
								     ((char<= char>=) 'char=?)
								     ((char-ci<= char-ci>=) 'char-ci=?)
								     ((string<= string>=) 'string=?)
								     ((string-ci<= string-ci>=) 'string-ci=?))
								  ,@(cdr arg1))))
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
						   
						   ;; equalities again
						   ((and (code-constant? arg1-1)
							 (code-constant? arg2-1))
						    (and ((symbol->value op1) arg1-1 arg2-1)
							 arg1))
						   
						   (else `(,op1 ,arg1-1 ,arg1-2 ,arg2-1))))))
				      
				      ;; check some special cases 
				      (when (and (or (equal? (cadr arg1) (cadr arg2))
						     (and (pair? (cddr arg2))
							  (null? (cdddr arg2))
							  (equal? (cadr arg1) (caddr arg2))))
						 (hash-table-ref bools1 (car arg1)))
					
					(when (or (eq? (car arg1) 'zero?)  ; perhaps rational? and integer? here -- not many hits
						  (eq? (car arg2) 'zero?))
					  (if (or (memq (car arg1) '(integer? rational? exact?))
						  (memq (car arg2) '(integer? rational? exact?)))
					      (return `(eqv? ,(cadr arg1) 0)))
					  (if (or (eq? (car arg1) 'inexact?)
						  (eq? (car arg2) 'inexact?))
					      (return `(eqv? ,(cadr arg1) 0.0))))
					
					(when (memq (car arg2) '(< = > <= >= char-ci>=? char-ci<? char-ready? char<? char-ci=? char>? 
								   char<=? char-ci>? char-ci<=? char>=? char=? string-ci<=? string=? 
								   string-ci>=? string<? string-ci<? string-ci=? string-ci>? string>=? string<=? string>?
								   eqv? equal? eq? morally-equal?))
					  
					  (when (and (eq? (car arg1) 'symbol?)
						     (memq (car arg2) '(eq? eqv? equal?))
						     (or (quoted-symbol? (cadr arg2))
							 (quoted-symbol? (caddr arg2))))
					    (return `(eq? ,@(cdr arg2))))
					  
					  (when (and (eq? (car arg1) 'positive?)
						     (eq? (car arg2) '<)
						     (eq? (cadr arg1) (cadr arg2)))
					    (return `(< 0 ,(cadr arg1) ,(caddr arg2))))))

				      ;; this makes some of the code above redundant
				      (let ((rel (relsub arg1 arg2 'and env)))
					(if (or (boolean? rel)
						(pair? rel))
					    (return rel)))
				      
				      ;; (and ... (not...))
				      (unless (eq? (car arg1) (car arg2))
					(if (eq? (car arg1) 'not)
					    (let ((temp arg1))
					      (set! arg1 arg2)
					      (set! arg2 temp)))
					(if (and (eq? (car arg2) 'not)
						 (pair? (cadr arg2))
						 (pair? (cdadr arg2))
						 (not (eq? (caadr arg2) 'let?))
						 (or (equal? (cadr arg1) (cadadr arg2))
						     (and (pair? (cddr arg1))
							  (equal? (caddr arg1) (cadadr arg2))))
						 (eq? (return-type (car arg1) env) 'boolean?)
						 (eq? (return-type (caadr arg2) env) 'boolean?))
					    (let ((t2 (and-not-redundant arg1 arg2)))
					      (when t2 
						(cond ((eq? t2 'contradictory) (return #f))
						      ((symbol? t2)	       (return `(,t2 ,@(cdr arg1))))
						      ((pair? t2)	       (return t2)))))))
				      
				      (if (hash-table-ref bools (car arg1))
					  (let ((p (member (cadr arg1) (cdr arg2))))
					    (when p
					      (let ((sig (arg-signature (car arg2) env))
						    (pos (- (length arg2) (length p))))
						(when (pair? sig)
						  (let ((arg-type (and (> (length sig) pos)
								       (list-ref sig pos))))
						    (if (not (compatible? (car arg1) arg-type))
							(let ((ln (and (positive? line-number)
								       (< line-number 100000)
								       line-number)))
							  (format outport "~NCin ~A~A, ~A is ~A, but ~A wants ~A"
								  lint-left-margin #\space 
								  (truncated-list->string form) 
								  (if ln (format #f " (line ~D)" ln) "")
								  (cadr arg1) 
								  (prettify-checker-unq (car arg1))
								  (car arg2)
								  (prettify-checker arg-type))))))))))
				      )))
				
				;; len > 3 or nothing was caught above
				(let ((nots 0)
				      (revers 0))
				  (if (every? (lambda (a)                ; (and (not (pair? x)) (not (null? x))) -> (not (list? x))
						(and (pair? a)
						     (if (eq? (car a) 'not)
							 (set! nots (+ nots 1))
							 (and (hash-table-ref notables (car a))
							      (set! revers (+ revers 1))))))
					      (cdr form))
				      (if (zero? revers)
					  (let ((nf (simplify-boolean `(or ,@(map cadr (cdr form))) () () env)))
					    (return (simplify-boolean `(not ,nf) () () env)))
					  (if (> nots revers)
					      (let ((nf (simplify-boolean `(or ,@(map (lambda (p)
											(if (eq? (car p) 'not)
											    (cadr p)
											    `(,(hash-table-ref notables (car p)) ,@(cdr p))))
										      (cdr form)))
									  () () env)))
						(return (simplify-boolean `(not ,nf) () () env)))))))
				
				(if (every? (lambda (a)
					      (and (pair? a)
						   (eq? (car a) 'zero?)))
					    (cdr form))
				    (return `(= 0 ,@(map cadr (cdr form)))))
				
				(let ((diff (apply and-redundants env (cdr form))))
				  (when diff 
				    (if (null? (cdr diff))
					(return (car diff)))
				    (return `(and ,@diff))))
				;; now there are redundancies below (see subsumes?) but they assumed the tests were side-by-side

				(let ((new-form ())
				      (retry #f))
				  
				  (do ((exprs (cdr form) (cdr exprs)))
				      ((null? exprs) 
				       (or (null? new-form)      ; (and) -> #t
					   (let* ((nform (reverse new-form)) 
						  (newer-form (map (lambda (x cdr-x)
								     (if (and x (code-constant? x))
									 (values)
									 x))
								   nform (cdr nform))))
					     (return
					      (cond ((null? newer-form)
						     (car new-form))
						    
						    ((and (eq? (car new-form) #t) ; trailing #t is dumb if next-to-last is boolean func
							  (pair? (cdr new-form))
							  (pair? (cadr new-form))
							  (symbol? (caadr new-form))
							  (eq? (return-type (caadr new-form) env) 'boolean?))
						     (if (null? (cdr newer-form))
							 (car newer-form)
							 `(and ,@newer-form)))
						    
						    (retry
						     (simplify-boolean `(and ,@newer-form ,(car new-form)) () () env))
						    
						    (else `(and ,@newer-form ,(car new-form))))))))
				    
				    (let* ((e (car exprs))
					   (val (classify e)))
				      (if (and (pair? val)
					       (memq (car val) '(and or not)))
					  (set! val (classify (set! e (simplify-boolean val () false env)))))
				      
				      (if (not (or retry
						   (equal? e (car exprs))))
					  (set! retry #t))
				      
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
					     (set! new-form (if (or (null? new-form)   
								    (just-symbols? new-form))
								'(#f)
								(cons #f new-form))) ;was (append '(#f) new-form)))
					     (set! exprs '(#f)))
					    
					    ((and (pair? e)       ; if (and ...) splice into current
						  (eq? (car e) 'and))
					     (set! exprs (append e (cdr exprs))))
					    
					    ((not (and (pair? e)                   ; (and ... (or ... 123) ...) -> splice out or
						       (pair? (cdr exprs))
						       (eq? (car e) 'or)
						       (pair? (cdr e))
						       (pair? (cddr e))
						       (cond ((list-ref e (- (length e) 1)) => code-constant?) ; (or ... #f)
							     (else #f))))
					     (if (not (and (pair? new-form)
							   (or (eq? val (car new-form)) ; omit repeated tests
							       (and (pair? val)         ;   and redundant tests
								    (hash-table-ref bools1 (car val))
								    (any? (lambda (p)
									    (and (pair? p)
										 (subsumes? (car val) (car p))
										 (equal? (cadr val) (cadr p))))
									  new-form)))))
						 (set! new-form (cons val new-form))))))))))))))))))))))
    
    (define (splice-if f lst)
      (cond ((null? lst) ())
	    ((not (pair? lst)) lst)
	    ((and (pair? (car lst)) 
		  (f (caar lst)))
	     (append (splice-if f (cdar lst)) 
		     (splice-if f (cdr lst))))
	    (else (cons (car lst) 
			(splice-if f (cdr lst))))))
    
    (define (simplify-numerics form env)
      ;; this returns a form, possibly the original simplified
      (let ((real-result? (lambda (op) (memq op '(imag-part real-part abs magnitude angle max min exact->inexact inexact
						  modulo remainder quotient lcm gcd))))
	    (rational-result? (lambda (op) (memq op '(rationalize inexact->exact exact))))
	    (integer-result? (lambda (op) (memq op '(logior lognot logxor logand numerator denominator floor round truncate ceiling ash)))))
	
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
	  (if (or (null? x)                      ; constants and the like look dumb if simplified
		  (not (proper-list? x))
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
			      (set! val (if (zero? y) 
					    (collect-if list (lambda (x) (not (number? x))) val)
					    (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		  (case (length val)
		    ((0) 0)
		    ((1) (car val))                       ; (+ x) -> x
		    ((2)
		     (let ((arg1 (car val))
			   (arg2 (cadr val)))
		       (cond ((and (pair? arg1)           ; (+ (- x) y) -> (- y x)
				   (eq? (car arg1) '-)
				   (null? (cddr arg1)))
			      `(- ,arg2 ,(cadr arg1)))
			     ((and (pair? arg2)           ; (+ x (- y)) -> (- x y)
				   (eq? (car arg2) '-)
				   (null? (cddr arg2)))
			      `(- ,arg1 ,(cadr arg2)))
			     ((and (real? arg2)           ; (+ x -1) -> (- x 1)
				   (negative? arg2)
				   (not (number? arg1)))
			      `(- ,arg1 ,(abs arg2)))
			     ((and (real? arg1)           ; (+ -1 x) -> (- x 1)
				   (negative? arg1)
				   (not (number? arg2)))
			      `(- ,arg2 ,(abs arg1)))
			     (else `(+ ,@val)))))
		    (else 
		     (if (any? (lambda (p) 
				 (and (pair? p) 
				      (eq? (car p) '-) 
				      (null? (cddr p)))) 
			       val)
			 (let ((plus ())
			       (minus ()))
			   (for-each (lambda (p)
				       (if (not (and (pair? p)
						     (eq? (car p) '-)))
					   (set! plus (cons p plus))
					   (if (null? (cddr p))
					       (set! minus (cons (cadr p) minus))
					       (begin
						 (set! plus (cons (cadr p) plus))
						 (set! minus (append (cddr p) minus))))))
				     val)
			   `(- (+ ,@(reverse plus)) ,@(reverse minus)))
		     `(+ ,@val))))))))  ; other obvious simplifications never happen
					;   the most common by far is (+ i i) -- is (* 2 i) an improvement?
					;   in s7 the multiply appears to be marginally faster
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
			      (set! val (if (= y 1)
					    (collect-if list (lambda (x) (not (number? x))) val)
					    (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
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
			    (cond ((and (eq? (caar val) '-)      ; (* (- x) (- y)) -> (* x y)
					(null? (cddar val))
					(eq? (caadr val) '-)
					(null? (cddadr val)))
				   `(* ,(cadar val) ,(cadadr val)))
				  ((and (= (length (car val)) 3)
					(equal? (cdar val) (cdadr val))
					(or (and (eq? (caar val) 'gcd) (eq? (caadr val) 'lcm))
					    (and (eq? (caar val) 'lcm) (eq? (caadr val) 'gcd))))
				   `(abs (* ,@(cdar val))))      ; (* (gcd a b) (lcm a b)) -> (abs (* a b)) but only if 2 args?
				  ((and (eq? (caar val) 'exp)    ; (* (exp a) (exp b)) -> (exp (+ a b))
					(eq? (caadr val) 'exp))
				   `(exp (+ ,(cadar val) ,(cadadr val))))
				  (else `(* ,@val))))
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
			   ((and (number? (car val))        ; (* 2 (random 3.0)) -> (random 6.0)
				 (pair? (cadr val))
				 (eq? (caadr val) 'random)
				 (number? (cadadr val))
				 (not (rational? (cadadr val))))
			    `(random ,(* (car val) (cadadr val))))
			   (else `(* ,@val))))
		    (else 
		     (cond ((just-rationals? val)
			    (let ((new-val (apply * val))) ; huge numbers here are less readable
			      (if (< (abs new-val) 1000000)
				  new-val
				  `(* ,@val))))
			   ((memv 0 val)                   ; (* x 0 2) -> 0
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
				   (cadar args)                   ; (- (- x)) -> x
				   `(- ,@args)))
			  ((3) (if (eq? (caar args) '-)
				   `(- ,(caddar args) ,(cadar args)) ; (- (- x y)) -> (- y x)
				   `(- ,@args)))
			  (else `(- ,@args))))))
	       ((2) 
		(let ((arg1 (car args))
		      (arg2 (cadr args)))
		  (cond ((just-rationals? args) (apply - args)) ; (- 3 2) -> 1
			((eqv? arg1 0) `(- ,arg2))              ; (- 0 x) -> (- x)
			((eqv? arg2 0) arg1)                    ; (- x 0) -> x
			((equal? arg1 arg2) 0)                  ; (- x x) -> 0
			((and (pair? arg2)
			      (eq? (car arg2) '-)
			      (pair? (cdr arg2)))
			 (if (null? (cddr arg2)) 
			     `(+ ,arg1 ,(cadr arg2))            ; (- x (- y)) -> (+ x y)
			     (simplify-numerics `(- (+ ,arg1 ,@(cddr arg2)) ,(cadr arg2)) env))) ; (- x (- y z)) -> (- (+ x z) y)
			((and (pair? arg2)                      ; (- x (+ y z)) -> (- x y z)
			      (eq? (car arg2) '+))
			 (simplify-numerics `(- ,arg1 ,@(cdr arg2)) env))
			((and (pair? arg1)                      ; (- (- x y) z) -> (- x y z)
			      (eq? (car arg1) '-))
			 (if (> (length arg1) 2)
			     `(- ,@(cdr arg1) ,arg2)
			     (simplify-numerics `(- (+ ,(cadr arg1) ,arg2)) env)))  ; (- (- x) y) -> (- (+ x y))
			((and (pair? arg2)                      ; (- x (truncate x)) -> (remainder x 1)
			      (eq? (car arg2) 'truncate)
			      (equal? arg1 (cadr arg2)))
			 `(remainder ,arg1 1))
			((and (real? arg2)                      ; (- x -1) -> (+ x 1)
			      (negative? arg2)
			      (not (number? arg1)))
			 `(+ ,arg1 ,(abs arg2)))
			(else `(- ,@args)))))
	       (else 
		(if (just-rationals? args)
		    (apply - args)
		    (let ((val (remove-all 0 (splice-if (lambda (x) (eq? x '+)) (cdr args)))))
		      (if (every? (lambda (x) (or (not (number? x)) (rational? x))) val)
			  (let ((rats (collect-if list rational? val)))
			    (if (and (pair? rats) 
				     (pair? (cdr rats)))
				(let ((y (apply + rats)))
				  (set! val (if (zero? y)
						(collect-if list (lambda (x) (not (number? x))) val)
						(cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
		      (set! val (cons (car args) val))
		      (let ((first-arg (car args))
			    (nargs (cdr val)))
			(if (member first-arg nargs)
			    (begin
			      (set! nargs (remove first-arg nargs)) ; remove once
			      (set! first-arg 0)))
			(cond ((null? nargs) first-arg)       ; (- x 0 0 0)?
			      ((eqv? first-arg 0)
			       (if (= (length nargs) 1)
				   (if (number? (car nargs))
				       (- (car nargs))
				       `(- ,(car nargs)))     ; (- 0 0 0 x)?
				   `(- (+ ,@nargs))))         ; (- 0 z y) -> (- (+ x y))
			      ((not (and (pair? (car args))
					 (eq? (caar args) '-)))
			       `(- ,@(cons first-arg nargs)))
			      ((> (length (car args)) 2)
			       (simplify-numerics `(- ,@(cdar args) ,@(cdr args)) env))
			      (else (simplify-numerics `(- (+ ,(cadar args) ,@(cdr args))) env)))))))))

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
				  (not (and (pair? arg2)
					    (eq? (car arg2) '/))))
			     `(/ ,(cadr arg1) ,@(cddr arg1) ,arg2))
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
			       (if (and (pair? n) (null? (cdr n)))
				   `(/ ,@n)               ; (/ x (* y x)) -> (/ y)
				   `(/ 1 ,@n))))          ; (/ x (* y x z)) -> (/ 1 y z)
			    ((and (pair? arg2)             ; (/ c (/ a b)) -> (/ (* c b) a)
				  (eq? (car arg2) '/)
				  (pair? (cddr arg2)))
			     (if (and (rational? arg1)
				      (rational? (cadr arg2))
				      (null? (cdddr arg2)))
				 (let ((val (/ arg1 (cadr arg2))))
				   (if (= val 1)
				       (caddr arg2)
				       (if (= val -1)
					   `(- ,(caddr arg2))
					   `(* ,val ,(caddr arg2)))))
				 `(/ (* ,arg1 ,@(cddr arg2)) ,(cadr arg2))))
#|
			    ;; can't decide about this -- result usually looks cruddy
			    ((and (pair? arg2)             ; (/ x (* y z)) -> (/ x y z)
				  (eq? (car arg2) '*))
			     `(/ ,arg1 ,@(cdr arg2)))
|#
			    ((and (pair? arg1)             ; (/ (log x) (log y)) -> (log x y)
				  (pair? arg2)
				  (= (length arg1) (length arg2) 2)
				  (eq? (car arg1) 'log)
				  (eq? (car arg2) 'log))  ; other possibilities here don't happen
			     `(log ,(cadr arg1) ,(cadr arg2)))
			    ((and (pair? arg1)            ; (/ (inexact x) 2.0) -> (/ x 2.0)
				  (memq (car arg1) '(exact->inexact inexact))
				  (number? arg2)
				  (not (rational? arg2)))
			     `(/ ,(cadr arg1) ,arg2))
			    ((and (pair? arg2)            ; (/ 2.0 (inexact x)) -> (/ 2.0 x)
				  (memq (car arg2) '(exact->inexact inexact))
				  (number? arg1)
				  (not (rational? arg1)))
			     `(/ ,arg1 ,(cadr arg2)))
			    ((and (pair? arg1)            ; (/ (- x) (- y)) -> (/ x y)
				  (pair? arg2)
				  (eq? (car arg1) '-)
				  (eq? (car arg2) '-)
				  (= (length arg1) (length arg2) 2))
			     `(/ ,(cadr arg1) ,(cadr arg2)))
			    (else `(/ ,@args))))))
	       
	       (else 
		(if (and (just-rationals? args)
			 (not (memv 0 (cdr args)))
			 (not (memv 0.0 (cdr args))))
		    (apply / args)
		    (let ((nargs                            ; (/ x a (* b 1 c) d) -> (/ x a b c d)
			   (remove-all 1 (splice-if (lambda (x) (eq? x '*)) (cdr args)))))
		      (if (null? nargs) ; (/ x 1 1) -> x
			  (car args)
			  (if (and (member (car args) (cdr args))
				   (not (side-effect? (car args) env)))
			      (let ((n (remove (car args) (cdr args))))
				(if (= (length n) 1)
				    `(/ ,@n)                ; (/ x y x) -> (/ y)
				    `(/ 1 ,@n)))            ; (/ x y x z) -> (/ 1 y z)
			      `(/ ,@(cons (car args) nargs)))))))))
	    
	    ((sin cos tan asin acos sinh cosh tanh asinh acosh atanh exp)
	     (cond ((not (= len 1))
		    `(,(car form) ,@args))
		   ((and (pair? (car args))                 ; (sin (asin x)) -> x
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
		      ((sin tan) 0.0)
		      ((cos) 1.0)
		      (else `(,(car form) ,@args))))
		   ((eqv? (car args) 0.0)                  ; (sin 0.0) -> 0.0
		    ((symbol->value (car form)) 0.0))
		   ((and (eq? (car form) 'exp)             ; (exp (* a (log b))) -> (expt b a)
			 (pair? (car args))
			 (eq? (caar args) '*))
		    (let ((targ (cdar args)))
		      (cond ((not (= (length targ) 2))
			     `(,(car form) ,@args))
			    ((and (pair? (car targ))
				  (eq? (caar targ) 'log)
				  (pair? (cdar targ))
				  (null? (cddar targ)))
			     `(expt ,(cadar targ) ,(cadr targ)))
			    ((and (pair? (cadr targ))
				  (eq? (caadr targ) 'log) 
				  (pair? (cdadr targ))
				  (null? (cddadr targ)))
			     `(expt ,(cadadr targ) ,(car targ)))
			    (else `(,(car form) ,@args)))))
		   (else `(,(car form) ,@args))))
	    
	    ((log)
	     (cond ((not (pair? args)) form)
		   ((eqv? (car args) 1) 0)    ; (log 1 ...) -> 0
		   ((and (= len 1)            ; (log (exp x)) -> x
			 (pair? (car args))
			 (= (length (car args)) 2)
			 (eq? (caar args) 'exp))
		    (cadar args))
		   ((not (and (= len 2)            ; (log x x) -> 1.0
			      (equal? (car args) (cadr args))))
		    `(log ,@args))
		   ((integer? (car args)) 1)
		   (else 1.0)))
	    
	    ((sqrt)
	     (if (not (pair? args))
		 form
		 (if (and (rational? (car args))
			  (rational? (sqrt (car args)))
			  (= (car args) (* (sqrt (car args)) (sqrt (car args)))))
		     (sqrt (car args)) ; don't collapse (sqrt (* a a)), a=-1 for example
		     `(sqrt ,@args))))
	    
	    ((floor round ceiling truncate)
	     (cond ((not (= len 1))
		    form)

		   ((number? (car args))
		    (catch #t 
		      (lambda () (apply (symbol->value (car form)) args)) 
		      (lambda any `(,(car form) ,@args))))

		   ((not (pair? (car args)))
		    `(,(car form) ,@args))

		   ((or (integer-result? (caar args))
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
		   ((and (eq? (caar args) 'random)
			 (eq? (car form) 'floor)
			 (float? (cadar args))
			 (= (floor (cadar args)) (cadar args)))
		    `(random ,(floor (cadar args))))
			  
		   (else `(,(car form) ,@args))))
	    
	    ((abs magnitude)
	     (cond ((not (= len 1))
		    form)
		   ((and (pair? (car args))        ; (abs (abs x)) -> (abs x)
			 (memq (caar args) non-negative-ops))
		    (car args))
		   ((rational? (car args))
		    (abs (car args)))
		   ((and (pair? (car args))        ; (abs (modulo x 2)) -> (modulo x 2)
			 (memq (caar args) '(modulo random))
			 (= (length (car args)) 3)
			 (number? (caddar args))
			 (positive? (caddar args)))
		    (car args))
		   ((and (pair? (car args))        ; (abs (- x)) -> (abs x)
			 (eq? (caar args) '-)
			 (pair? (cdar args))
			 (null? (cddar args)))
		    `(,(car form) ,(cadar args)))
		   (else `(,(car form) ,@args))))
	
	    ((imag-part)
	     (if (not (= len 1))
		 form
		 (if (or (real? (car args))
			 (and (pair? (car args))
			      (real-result? (caar args))))
		     0.0
		     `(imag-part ,@args))))
	    
	    ((real-part)
	     (if (not (= len 1))
		 form
		 (if (or (real? (car args))
			 (and (pair? (car args))
			      (real-result? (caar args))))
		     (car args)
		     `(real-part ,@args))))
	    
	    ((denominator)
	     (if (not (= len 1))
		 form
		 (if (or (integer? (car args))
			 (and (pair? (car args))
			      (integer-result? (caar args))))
		     1
		     `(denominator ,(car args)))))
	    
	    ((numerator)
	     (cond ((not (= len 1))
		    form)
		   ((or (integer? (car args))
			(and (pair? (car args))
			     (integer-result? (caar args))))
		    (car args))
		   ((rational? (car args))
		    (numerator (car args)))
		   (else `(numerator ,(car args)))))
	    
	    ((random)
	     (cond ((not (and (= len 1)
			      (number? (car args))))
		    `(random ,@args))  
		   ((eqv? (car args) 0)
		    0)
		   ((morally-equal? (car args) 0.0)
		    0.0)
		   (else `(random ,@args))))
	    
	    ((complex make-rectangular)
	     (if (and (= len 2)
		      (morally-equal? (cadr args) 0.0)) ; morally so that 0 matches
		 (car args)
		 `(complex ,@args)))
	    
	    ((make-polar)
	     (if (and (= len 2)
		      (morally-equal? (cadr args) 0.0))
		 (car args)
		 `(make-polar ,@args)))

	    ((rationalize lognot ash modulo remainder quotient)
	     (cond ((just-rationals? args)
		    (catch #t ; catch needed here for things like (ash 2 64)
		      (lambda ()
			(apply (symbol->value (car form)) args))
		      (lambda ignore
			`(,(car form) ,@args)))) ; use this form to pick up possible arg changes
		   
		   ((and (eq? (car form) 'ash)          ; (ash x 0) -> x
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
		   
		   (else `(,(car form) ,@args))))
	    
	    ((expt) 
	     (cond ((not (= len 2))
		    form)
		   ((and (eqv? (car args) 0)            ; (expt 0 x) -> 0
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
		   (else `(expt ,@args))))

	    
	    ((angle)
	     (cond ((not (pair? args)) form)
		   ((eqv? (car args) -1) 'pi)
		   ((or (morally-equal? (car args) 0.0)
			(eq? (car args) 'pi))
		    0.0)
		   (else `(angle ,@args))))
	    
	    ((atan)
	     (if (and (= len 1)
		      (pair? (car args))
		      (= (length (car args)) 3)
		      (eq? (caar args) '/))
		 `(atan ,@(cdar args))
		 `(atan ,@args)))
	    
	    ((inexact->exact exact)
	     (cond ((not (= len 1)) 
		    form)
		   ((or (rational? (car args))
			(and (pair? (car args))
			     (or (rational-result? (caar args))
				 (integer-result? (caar args))
				 (and (eq? (caar args) 'random)
				      (rational? (cadar args))))))
		    (car args))
		   ((number? (car args))
		    (catch #t (lambda () (inexact->exact (car args))) (lambda any `(,(car form) ,@args))))
		   (else `(,(car form) ,@args))))
	    
	    ((exact->inexact inexact)
	     (if (not (= len 1))
		 form
		 (if (memv (car args) '(0 0.0))
		     0.0
		     ;; not (inexact (random 3)) -> (random 3.0) because results are different
		     `(,(car form) ,@args))))
	    
	    ((logior)
	     (set! args (lint-remove-duplicates (remove-all 0 (splice-if (lambda (x) (eq? x 'logior)) args)) env))
	     (if (every? (lambda (x) (or (not (number? x)) (integer? x))) args)
		 (let ((rats (collect-if list integer? args)))
		   (if (and (pair? rats) 
			    (pair? (cdr rats)))
		       (let ((y (apply logior rats)))
			 (set! args (if (zero? y)
					(collect-if list (lambda (x) (not (number? x))) args)
					(cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
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
			 (set! args (if (= y -1)
					(collect-if list (lambda (x) (not (number? x))) args)
					(cons y (collect-if list (lambda (x) (not (number? x))) args))))))))
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
	     (if (not (pair? args))
		 form
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

			     ;; if (max c1 (min c2 . args1) . args2) where (> c1 c2) -> (max c1 . args2), if = -> c1
			     ;; if (min c1 (max c2 . args1) . args2) where (< c1 c2) -> (min c1 . args2), if = -> c1
			     ;;   and if (max 4 x (min x 4)) -- is it (max x 4)?
			     ;; (max a b) is (- (min (- a) (- b))), but that doesn't help here -- the "-" gets in our way
			     ;;   (min (- a) (- b)) -> (- (max a b))?
			     ;; (+ a (max|min b c)) = (max|min (+ a b) (+ a c)))

			     (if (null? (cdr args)) ; (max (min x 3) (min x 3)) -> (max (min x 3)) -> (min x 3)
				 (car args)
				 (if (and (null? (cddr args))   ; (max|min x (min|max x ...) -> x
					  (or (and (pair? (car args))
						   (eq? (caar args) other)
						   (member (cadr args) (car args))
						   (not (side-effect? (cadr args) env)))
					      (and (pair? (cadr args))
						   (eq? (caadr args) other)
						   (member (car args) (cadr args))
						   (not (side-effect? (car args) env)))))
				     ((if (pair? (car args)) cadr car) args)
				     `(,(car form) ,@args)))))))))
	    (else 
	     `(,(car form) ,@args))))))
    
    
    (define (check-char-cmp caller op form)
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
	  (lint-format "perhaps ~A" caller
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
	 (let* ((arg (cadr d))
		(arg-arg (and (pair? arg)
			      (pair? (cdr arg))
			      (cadr arg))))
	   (cond ((string? arg)
		  arg)

		 ((char? arg)
		  (string arg))

		 ((and (pair? arg)
		       (eq? (car arg) 'number->string)
		       (= (length arg) 3))
		  (case (caddr arg)
		    ((2)  (values "~B" arg-arg))
		    ((8)  (values "~O" arg-arg))
		    ((10) (values "~D" arg-arg))
		    ((16) (values "~X" arg-arg))
		    (else (values "~A" arg))))

		 ((not (and (pair? arg)
			    (eq? (car arg) 'string-append)))
		  (values "~A" arg))

		 ((null? (cddr arg))
		  (if (string? arg-arg)
		      arg-arg
		      (values "~A" arg-arg)))

		 ((not (null? (cdddr arg)))
		  (values "~A" arg))

		 ((string? arg-arg)
		  (values (string-append arg-arg "~A") (caddr arg)))

		 ((string? (caddr arg))
		  (values (string-append "~A" (caddr arg)) arg-arg))

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
	       (if (not indices)
		   (cadr d)
		   (if (and (integer? (car indices))
			    (or (null? (cdr indices))
				(and (pair? indices)
				     (integer? (cadr indices)))))
		       (apply substring (cadr d) indices)
		       (values "~A" `(substring ,(cadr d) ,@indices))))
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
      (cond ((equal? old tree)
	     new)

	    ((not (pair? tree))
	     tree)

	    ((eq? (car tree) 'quote)
	     (copy-tree tree))
	    
	    (else (cons (tree-subst new old (car tree))
			(tree-subst new old (cdr tree))))))

    
    (define* (find-unique-name f1 f2 (i 1))
      (let ((sym (string->symbol (format #f "_~D_" i))))
	(if (not (or (eq? sym f1)
		     (eq? sym f2)
		     (tree-member sym f1)
		     (tree-member sym f2)))
	    sym
	    (find-unique-name f1 f2 (+ i 1)))))
    
    (define (unrelop caller head form)         ; assume len=3 
      (let ((arg1 (cadr form))
	    (arg2 (caddr form)))
	(if (memv arg2 '(0 0.0))             ; (< (- x y) 0) -> (< x y), need both 0 and 0.0 because (eqv? 0 0.0) is #f
	    (if (and (pair? arg1)
		     (eq? (car arg1) '-)
		     (= (length arg1) 3))
		(lint-format "perhaps ~A" caller (lists->string form `(,head ,(cadr arg1) ,(caddr arg1)))))
	    (if (and (memv arg1 '(0 0.0))    ; (< 0 (- x y)) -> (> x y)
		     (pair? arg2)
		     (eq? (car arg2) '-)
		     (= (length arg2) 3))
		(lint-format "perhaps ~A" caller (lists->string form `(,(hash-table-ref reversibles head) ,(cadr arg2) ,(caddr arg2))))))))
    
    (define (check-start-and-end caller head form ff env)
      (if (or (and (integer? (car form))
		   (integer? (cadr form))
		   (apply >= form))
	      (and (equal? (car form) (cadr form))
		   (not (side-effect? (car form) env))))
	  (lint-format "these ~A indices make no sense: ~A" caller head ff)))

    (define (other-case c)
      (if (char-upper-case? c)
	  (char-downcase c)
	  (char-upcase c)))
    
    (define (check-boolean-affinity caller form env)
      ;; does built-in boolean func's arg make sense
      (if (and (= (length form) 2)
	       (not (symbol? (cadr form)))
	       (not (= line-number last-simplify-boolean-line-number)))
	  (let ((expr (simplify-boolean form () () env)))
	    (if (not (equal? expr form))
		(lint-format "perhaps ~A" caller (lists->string form expr)))
	    (if (and (pair? (cadr form))
		     (symbol? (caadr form)))
		(let ((rt (if (eq? (caadr form) 'quote)
			      (->simple-type (cadadr form))
			      (return-type (caadr form) env)))
		      (head (car form)))
		  (if (subsumes? head rt)
		      (lint-format "~A is always #t" caller (truncated-list->string form))
		      (if (not (or (memq rt '(#t #f values))
				   (any-compatible? head rt)))
			  (lint-format "~A is always #f" caller (truncated-list->string form)))))))))
    
    (define (combine-cxrs form)
      (let ((cxr? (lambda (s)
		    (and (pair? (cdr s))
			 (pair? (cadr s))
			 (memq (caadr s) '(car cdr cadr cddr cdar cdddr cddddr))))))
	(and (cxr? form)
	     (let* ((arg1 (cadr form))
		    (arg2 (and arg1 (cxr? arg1) (cadr arg1)))
		    (arg3 (and arg2 (cxr? arg2) (cadr arg2)))
		    (innards (lambda (c)
			       (case c 
				 ((car) "a") ((cdr) "d") ((caar) "aa") ((cadr) "ad") ((cddr) "dd") ((cdar) "da")
				 ((caaar) "aaa") ((caadr) "aad") ((caddr) "add") ((cdddr) "ddd") 
				 ((cdaar) "daa") ((cddar) "dda") ((cdadr) "dad") ((cadar) "ada")
				 ((cddddr) "dddd") ((cadddr) "addd")))))
	       (values (string-append (innards (car form))
				      (innards (car arg1)) 
				      (if arg2 (innards (car arg2)) "")
				      (if arg3 (innards (car arg3)) ""))
		       (cadr (or arg3 arg2 arg1)))))))
    
    (define (mv-range producer env)
      (define (mv-range-1 p)
	(let ((v (var-member p env)))
	  (and (var? v)
	       (pair? ((cdr v) 'values)) ; not #<undefined> if v is a var, but not known to be a function (so possibly no 'values field)
	       ((cdr v) 'values))))
      
      (if (symbol? producer)
	  (mv-range-1 producer)
	  (and (pair? producer)
	       (if (memq (car producer) '(lambda lambda*))
		   (count-values (cddr producer))
		   (if (eq? (car producer) 'values)
		       (let ((len (length (cdr producer))))
			 (for-each (lambda (p)
				     (if (and (pair? p)
					      (eq? (car p) 'values))
					 (set! len (+ len (- (length (cdr p)) 1)))))
				   (cdr producer))
			 (list len len))
		       (mv-range (car producer) env))))))
    
    (define (eval-constant-expression caller form)
      (if (every? code-constant? (cdr form))
	  (catch #t
	    (lambda ()
	      (let ((val (eval (copy form :readable))))
		(lint-format "perhaps ~A" caller (lists->string form val))))
	    (lambda args
	      #t))))

	       
    (define (check-special-cases caller head form env)
      ;; here curlet won't change (leaving aside additions via define)
      ;; keyword head here if args to func/macro that we don't know about
      
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
		 (if (memq func '(eq? eqv? equal?))   ; (member x y eq?) -> (memq x y)
		     (let ((op (if (eq? head 'member)
				   (case func ((eq?) 'memq) ((eqv?) 'memv) (else 'member))
				   (case func ((eq?) 'assq) ((eqv?) 'assv) (else 'assoc)))))
		       (lint-format "perhaps ~A" caller (lists->string form `(,op ,(cadr form) ,(caddr form)))))
		     (let ((sig (procedure-signature (symbol->value func)))) ; arg-signature here is too cranky
		       (if (and sig
				(not (eq? (car sig) 'boolean?)))
			   (lint-format "~A is a questionable ~A function" caller func head))))
		 ;; func not a symbol
		 (if (and (pair? func)
			  (= (length func) 3)
			  (eq? (car func) 'lambda)
			  (pair? (cadr func))
			  (pair? (caddr func)))
		     (if (not (member (length (cadr func)) '(2 -1)))
			 (lint-format "~A equality function (optional third arg) should take two arguments" caller head)
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
						caller
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
		     (if (or (symbol? target)
			     (and (pair? target)
				  (not (eq? (car target) 'quote))))
			 (set! target (list 'quote target)))
		     (lint-format "perhaps ~A" caller (lists->string form `(,(cadr iter-eqf) ,selector ,target))))
		   
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
			       (set! baddy (if (eq? (car items) 'list)
					       (duplicate-constants? (cdr items) (symbol->value head))
					       (duplicates? (cadr items) (symbol->value head)))))
			     (lambda args #f))
			   (if (pair? baddy)
			       (lint-format "duplicated entry ~S in ~A" caller (car baddy) items))))
		     
		     (if (and (symbol? (car selector-eqf))
			      (not (eq? (car selector-eqf) current-eqf)))
			 (lint-format "~A: perhaps ~A -> ~A" caller (truncated-list->string form) head 
				      (if (memq head '(memq memv member))
					  (case (car selector-eqf) ((eq?) 'memq) ((eqv?) 'memv) ((equal?) 'member))
					  (case (car selector-eqf) ((eq?) 'assq) ((eqv?) 'assv) ((equal?) 'assoc)))))
		     
		     (if (and (pair? items)
			      (eq? (car items) 'list)
			      (every? code-constant? (cdr items)))
			 (lint-format "perhaps ~A -> '~A" caller (truncated-list->string items) 
				      (truncated-list->string (map unquoted (cdr items)))))
		     
		     (when (pair? items)
		       (let ((memx (memq head '(memq memv member))))
			 (case (car items)
			   ((map)
			    (when (and memx (= (length items) 3))
			      (let ((mapf (cadr items))
				    (map-items (caddr items)))
				(cond ((eq? mapf 'car)
				       (lint-format "perhaps use assoc: ~A" caller
						    (lists->string form `(,(case current-eqf ((eq?) 'assq) ((eqv?) 'assv) ((equal?) 'assoc)) 
									  ,selector ,map-items))))
				      ((eq? selector #t)
				       (if (eq? mapf 'null?)
					   (lint-format "perhaps ~A" caller 
							(lists->string form `(memq () ,map-items)))
					   (let ((b (if (eq? mapf 'b) 'c 'b)))
					     (lint-format "perhaps avoid 'map: ~A" caller 
							  (lists->string form `(member #t ,map-items (lambda (a ,b) (,mapf ,b))))))))
				      
				      ((and (pair? selector)
					    (eq? (car selector) 'string->symbol) ; this could be extended, but it doesn't happen
					    (eq? mapf 'string->symbol)
					    (not (and (pair? map-items)
						      (eq? (car map-items) 'quote))))
				       (lint-format "perhaps ~A" caller
						    (lists->string form `(member ,(cadr selector) ,map-items string=?))))
				      (else 
				       (let ((b (if (eq? mapf 'b) 'c 'b))) ; a below can't collide because eqf won't return 'a
					 (lint-format "perhaps avoid 'map: ~A" caller 
						      (lists->string form `(member ,selector ,map-items 
										   (lambda (a ,b) (,current-eqf a (,mapf ,b))))))))))))
			   ((cons)
			    (if (not (pair? selector))
				(lint-format "perhaps avoid 'cons: ~A" caller
					     (lists->string form `(or (,current-eqf ,selector ,(cadr items))
								      (,head ,selector ,(caddr items)))))))
			   ((append)
			    (if (and (not (pair? selector))
				     (= (length items) 3)
				     (pair? (cadr items))
				     (eq? (caadr items) 'list)
				     (null? (cddadr items)))
				(lint-format "perhaps ~A" caller
					     (lists->string form `(or (,current-eqf ,selector ,(cadadr items))
								      (,head ,selector ,(caddr items)))))))))))))
	     
	     (when (and (eq? (->type (cadr form)) 'char?)
			(pair? (caddr form))
			(eq? (caaddr form) 'string->list)
			(null? (cdddr form)))
	       (lint-format "perhaps ~A" caller
			    (lists->string form `(char-position ,(cadr form) ,@(cdaddr form)))))

	     (when (and (memq head '(memq memv))
			(pair? items)
			(eq? (car items) 'quote)
			(pair? (cadr items)))
	       (if (> (length items) 20)
		   (lint-format "perhaps use a hash-table here, rather than ~A" caller (truncated-list->string form)))

	       (let ((bad (find-if (lambda (x)
				     (not (or (symbol? x)
					      (char? x)
					      (number? x)
					      (procedure? x) ; (memq abs '(1 #_abs 2)) !
					      (memq x '(#f #t () #<unspecified> #<undefined> #<eof>)))))
				   (cadr items))))
		 (if bad
		     (cond ((not (pair? bad))
			    (lint-format "pointless list member: ~S in ~A" caller bad form))
			   ((eq? (car bad) 'quote)
			    (lint-format "stray quote? ~A" caller form))
			   ((eq? (car bad) 'unquote)
			    (lint-format "stray comma? ~A" caller form))
			   (else (lint-format "pointless list member: ~S in ~A" caller bad form)))))))))
			 
	
	;; ----------------
	((car cdr 
	  caar cadr cddr cdar
	  caaar caadr caddr cdddr cdaar cddar cadar cdadr
	  cadddr cddddr)
	 ;; caaaar caaadr caadar caaddr cadaar cadadr caddar cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar

	 (if (not (= line-number last-simplify-cxr-line-number))
	     ((lambda* (cxr arg)
		(when cxr
		  (set! last-simplify-cxr-line-number line-number)
		  (cond ((< (length cxr) 5)
			 (lint-format "perhaps ~A" caller 
				      (lists->string form `(,(string->symbol (string-append "c" cxr "r")) ,arg))))
			
			;; if it's car|cdr followed by cdr's, use list-ref|tail
			((not (char-position #\a cxr))
			 (lint-format "perhaps ~A" caller (lists->string form `(list-tail ,arg ,(length cxr)))))
			
			((not (char-position #\a (substring cxr 1)))
			 (lint-format "perhaps ~A" caller (lists->string form `(list-ref ,arg ,(- (length cxr) 1)))))
			
			(else (set! last-simplify-cxr-line-number -1)))))
	      (combine-cxrs form)))
	 
	 (when (pair? (cadr form))
	   (when (eq? head 'car)                             
	     (if (eq? (caadr form) 'list-tail)          ; (car (list-tail x y)) -> (list-ref x y)
		 (lint-format "perhaps ~A" caller (lists->string form `(list-ref ,(cadadr form) ,(caddr (cadr form)))))
		 (if (and (memq (caadr form) '(memq memv member assq assv assoc))
			  (pair? (cdadr form)))         ; (car (memq...))
		     (lint-format "~A is ~A, or an error" caller (truncated-list->string form) (cadadr form)))))

	   (if (and (memq head '(car cdr))
		    (eq? (caadr form) 'cons))
	       (lint-format "(~A~A) is the same as ~A"
			    caller head
			    (truncated-list->string (cadr form))
			    (if (eq? head 'car)
				(truncated-list->string (cadadr form))
				(truncated-list->string (caddr (cadr form))))))
	 
	   (when (memq head '(car cadr caddr cadddr))
	     (if (memq (caadr form) '(string->list vector->list))    ; (car (string->list x)) -> (string-ref x 0)
		 (lint-format "perhaps ~A" caller (lists->string form `(,(if (eq? (caadr form) 'string->list) 'string-ref 'vector-ref)
									,(cadadr form) 
									,(case head ((car) 0) ((cadr) 1) ((caddr) 2) (else 3)))))
		 (if (and (memq (caadr form) '(reverse reverse!))
			  (symbol? (cadadr form)))
		     (lint-format "perhaps ~A" caller                  ; (car (reverse x)) -> (list-ref x (- (length x) 1))
				  (lists->string form `(list-ref ,(cadadr form) 
								 (- (length ,(cadadr form)) 
								    ,(case head ((car) 1) ((cadr) 2) ((caddr) 3) (else 4)))))))))))
	
	;; ----------------
	((set-car!)
	 (when (= (length form) 3)
	   (let ((target (cadr form)))
	     (if (pair? target)
		 (case (car target)
		   
		   ((list-tail)                              ; (set-car! (list-tail x y) z) -> (list-set! x y z)
		    (lint-format "perhaps ~A" caller (lists->string form `(list-set! ,(cadr target) ,(caddr target) ,(caddr form)))))
		   
		   ((cdr cddr cdddr cddddr)
		    (set! last-simplify-cxr-line-number line-number)
		    (if (and (pair? (cadr target))
			     (memq (caadr target) '(cdr cddr cdddr cddddr)))
			(lint-format "perhaps ~A" caller       ; (set-car! (cdr (cddr x)) y) -> (list-set! x 3 y)
				     (lists->string form `(list-set! ,(cadadr target)
								     ,(+ (cdr-count (car target)) (cdr-count (caadr target))) 
								     ,(caddr form))))
			(lint-format "perhaps ~A" caller       ; (set-car! (cdr x) y) -> (list-set! x 1 y)
				     (lists->string form `(list-set! ,(cadr target) 
								     ,(cdr-count (car target)) 
								     ,(caddr form)))))))))))
	;; ----------------
	((not)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (eq? (caadr form) 'not))
	     (lint-format "if you want a boolean, (not (not ~A)) -> (and ~A #t)" 'paranoia 
			  (truncated-list->string (cadadr form))
			  (truncated-list->string (cadadr form))))
	 (if (not (= line-number last-simplify-boolean-line-number))
	     (let ((val (simplify-boolean form () () env)))
	       (set! last-simplify-boolean-line-number line-number)
	       (if (not (equal? form val))
		   (lint-format "perhaps ~A" caller (lists->string form val))))))
	
	((or)
	 (if (not (= line-number last-simplify-boolean-line-number))
	     (let ((val (simplify-boolean form () () env)))
	       (set! last-simplify-boolean-line-number line-number)
	       (if (not (equal? form val))
		   (lint-format "perhaps ~A" caller (lists->string form val))))))

	((and)
	 (if (not (= line-number last-simplify-boolean-line-number))
	     (let ((val (simplify-boolean form () () env)))
	       (set! last-simplify-boolean-line-number line-number)
	       (if (not (equal? form val))
		   (lint-format "perhaps ~A" caller (lists->string form val)))))

	 (if (not (tree-memq 'length form)) ; too many fussy messages! I may remove this check.
	     (let ((pairs())
		   (lists ()))
	       (let cxr-search ((tree (cdr form)))
		 (if (pair? tree)
		     (case (car tree)
		       ((pair?)
			(if (pair? (cdr tree))
			    (set! pairs (cons (cadr tree) pairs))))

		       ((list?)
			(if (pair? (cdr tree))
			    (set! lists (cons (cadr tree) lists))))

		       ((not)
			(if (and (pair? (cdr tree))
				 (pair? (cadr tree))
				 (eq? (caadr tree) 'null?)
				 (pair? (cdadr tree))
				 (member (cadadr tree) lists))
			    (set! lists (remove (cadadr tree) lists))))


		       ;; TODO: (if (list? x) (car|list-ref...) etc and proper-list? and maybe (not null?)?
		       ;; list followed by not null? or (or null?...) -- remove? or complain
			
		       ;; these apply to the others as well
		       ((car cdr)
			(if (and (pair? (cdr tree))
				 (member (cadr tree) lists)
				 (not (member (cadr tree) pairs)))
			    (lint-format "in ~A, we check ~A, but access ~A. But ~A might be null." caller
					 (truncated-list->string form)
					 `(list? ,(cadr tree)) ; TODO use the original
					 tree
					 (cadr tree))))

		       ((caar cadr cddr cdar 
			 caaar caadr caddr cdddr cdaar cddar cadar cdadr
			 cadddr cddddr caaaar caaadr caadar caaddr cadaar 
			 cadadr caddar cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar)
			(if (pair? (cdr tree))
			    (let ((ref (cadr tree)))
			      (if (and (tree-memq ref pairs)
				       (member ref pairs)
				       (not (member tree pairs)))
				  (let ((new-arg `(,(string->symbol (string-append "c" (substring (symbol->string (car tree)) 2))) ,(cadr tree))))
				    (if (not (member new-arg pairs))
					(lint-format "in ~A~%~NCwe check ~A, but then access ~A~A.~%~NCPerhaps add ~A" caller
						     (truncated-list->string form)
						     (+ lint-left-margin 4) #\space
						     `(pair? ,ref) tree
						     (local-line-number tree)
						     (+ lint-left-margin 4) #\space
						     `(pair? ,new-arg))))))))

			;; this can probably be confused by cxr-isms but they don't seem to happen
			(else (cxr-search (car tree))
			      (cxr-search (cdr tree)))))))))

	;; ----------------
	((=)
	 (let ((len (length form)))
	   (if (and (> len 2)
		    (any-real? (cdr form)))
	       (lint-format "= can be troublesome with floats: ~A" caller (truncated-list->string form)))
	   
	   (let ((cleared-form (cons = (remove-if (lambda (x) (not (number? x))) (cdr form)))))
	     (if (and (> (length cleared-form) 2)
		      (not (checked-eval cleared-form)))
		 (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form))))
	   
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
		 (if var
		     (if (or (eqv? arg1 0) 
			     (eqv? arg2 0))
			 (lint-format "perhaps (assuming ~A is a list), ~A" caller var 
				      (lists->string form `(null? ,var)))
			 (if (symbol? var)
			     (lint-format "perhaps (assuming ~A is a list), ~A" caller var 
					  (lists->string form `(and (pair? ,var) (null? (cdr ,var))))))))))
	     
	     (unrelop caller '= form))
	   (check-char-cmp caller head form)))
	
	;; ----------------
	((< > <= >=) ; '= handled above
	 (let ((cleared-form (cons head ; keep operator
				   (remove-if (lambda (x) 
						(not (number? x))) 
					      (cdr form)))))
	   (if (and (> (length cleared-form) 2)
		    (not (checked-eval cleared-form)))
	       (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form))))
	 
	 (if (= (length form) 3)
	     (unrelop caller head form)
	     (when (> (length form) 3)
	       (if (and (memq head '(< >))
			(repeated-member? (cdr form) env))
		   (lint-format "perhaps ~A" caller (truncated-lists->string form #f))
		   (if (and (memq head '(<= >=))
			    (repeated-member? (cdr form) env))
		       (let ((last-arg (cadr form))
			     (new-args (list (cadr form))))
			 (do ((lst (cddr form) (cdr lst)))
			     ((null? lst) 
			      (if (repeated-member? new-args env)
				  (lint-format "perhaps ~A" caller (truncated-lists->string form `(= ,@(lint-remove-duplicates (reverse new-args) env))))
				  (if (< (length new-args) (length (cdr form)))
				      (lint-format "perhaps ~A" caller 
						   (truncated-lists->string form (or (null? (cdr new-args))
										     `(= ,@(reverse new-args))))))))
			   (unless (equal? (car lst) last-arg)
			     (set! last-arg (car lst))
			     (set! new-args (cons last-arg new-args)))))))))

	 (when (= (length form) 3)
	   (cond ((and (eqv? (cadr form) 0)
		       (eq? head '>)
		       (pair? (caddr form))
		       (memq (caaddr form) non-negative-ops))
		  (lint-format "~A can't be negative: ~A" caller (caaddr form) (truncated-list->string form)))

		 ((and (eqv? (caddr form) 0)
		       (eq? head '<)
		       (pair? (cadr form))
		       (memq (caadr form) non-negative-ops))
		  (lint-format "~A can't be negative: ~A" caller (caadr form) (truncated-list->string form)))

		 ((and (pair? (cadr form))
		       (eq? (caadr form) 'length))
		  (let ((arg (cadadr form)))
		    (when (symbol? arg)
		      (if (eqv? (caddr form) 0)
			  (lint-format "perhaps~A ~A" caller
				       (if (eq? head '<) "" (format #f " (assuming ~A is a proper list)," arg))
				       (lists->string form
						      (case head
							((<)  `(and (pair? ,arg) (not (proper-list? ,arg))))
							((<=) `(null? ,arg))
							((>)  `(pair? ,arg))
							((>=) `(list? ,arg)))))
			  (if (eqv? (caddr form) 1)
			      (lint-format "perhaps (assuming ~A is a proper list), ~A" caller arg
					   (lists->string form
							  (case head
							    ((<)  `(null? ,arg))
							    ((<=) `(or (null? ,arg) (null? (cdr ,arg))))
							    ((>)  `(and (pair? ,arg) (pair? (cdr ,arg))))
							    ((>=) `(pair? ,arg))))))))))
		 ((and (pair? (caddr form))
		       (eq? (caaddr form) 'length))
		  (let ((arg (cadr (caddr form))))
		    (when (symbol? arg)
		      (if (eqv? (cadr form) 0)
			  (lint-format "perhaps~A ~A" caller
				       (if (eq? head '>) "" (format #f " (assuming ~A is a proper list)," arg))
				       (lists->string form
						      (case head
							((<)  `(pair? ,arg))
							((<=) `(list? ,arg))
							((>)  `(and (pair? ,arg) (not (proper-list? ,arg))))
							((>=) `(null? ,arg)))))
			  (if (eqv? (cadr form) 1)
			      (lint-format "perhaps (assuming ~A is a proper list), ~A" caller arg
					   (lists->string form
							  (case head
							    ((<)  `(and (pair? ,arg) (pair? (cdr ,arg))))
							    ((<=) `(pair? ,arg))
							    ((>)  `(null? ,arg))
							    ((>=) `(or (null? ,arg) (null? (cdr ,arg))))))))))))))
	 (check-char-cmp caller head form))
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
	       (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form))))
	 (if (and (eq? head 'char-ci=?) ; (char-ci=? x #\return)
		  (pair? (cdr form))
		  (pair? (cddr form))
		  (null? (cdddr form))
		  (or (and (char? (cadr form))
			   (char=? (cadr form) (other-case (cadr form))))
		      (and (char? (caddr form))
			   (char=? (caddr form) (other-case (caddr form))))))
	     (lint-format "char-ci=? could be char=? here: ~A" caller form)))
	
	;; ----------------
	((string<? string>? string<=? string>=? string=? 
		   string-ci<? string-ci>? string-ci<=? string-ci>=? string-ci=?)
	 (let ((cleared-form (cons head ; keep operator
				   (remove-if (lambda (x) 
						(not (string? x))) 
					      (cdr form)))))
	   (if (and (> (length cleared-form) 2)
		    (not (checked-eval cleared-form)))
	       (lint-format "this comparison can't be true: ~A" caller (truncated-list->string form)))))
	
	;; ----------------
	((length)
	 (if (pair? (cdr form))
	     (if (pair? (cadr form))
		 (let ((arg (cadr form)))
		   (case (car arg)
		     ((reverse reverse! list->vector vector->list list->string string->list let->list)
		      (lint-format "perhaps ~A" caller (lists->string form `(length ,(cadr arg)))))
		     ((cons)
		      (lint-format "perhaps ~A" caller (lists->string form `(+ (length ,(caddr arg)) 1))))
		     ((make-list)
		      (lint-format "perhaps ~A" caller (lists->string form (cadr arg))))
		     ((list)
		      (lint-format "perhaps ~A" caller (lists->string form (length (cdr arg)))))
		     ((cdr)
		      (lint-format "perhaps ~A" caller (lists->string form `(- (length ,(cadr arg)) 1))))
		     ((cddr)
		      (lint-format "perhaps ~A" caller (lists->string form `(- (length ,(cadr arg)) 2))))
		     ((append)
		      (if (= (length (cdr arg)) 2)
			  (lint-format "perhaps ~A" caller (lists->string form `(+ (length ,(cadr arg)) (length ,(caddr arg)))))))
		     ((quote)
		      (if (list? (cadr arg))
			  (lint-format "perhaps ~A" caller (lists->string form (length (cadr arg))))))))
		 ;; not pair cadr
		 (if (code-constant? (cadr form))
		     (lint-format "perhaps ~A -> ~A" caller 
				  (truncated-list->string form)
				  (length (if (and (pair? (cadr form))
						   (eq? (caadr form) 'quote))
					      (cadadr form)
					      (cadr form))))))))
	;; ----------------
	((zero? positive? negative?)
	 (if (pair? (cdr form))
	     (let ((arg (cadr form)))
	       (if (and (pair? arg)
			(eq? (car arg) '-))
		   (lint-format "perhaps ~A" caller 
				(lists->string form
					       (let ((op '((zero? = zero?) (positive? > negative?) (negative? < positive?))))
						 (if (null? (cddr arg))
						     `(,(caddr (assq head op)) ,(cadr arg))
						     (if (null? (cdddr arg))
							 `(,(cadr (assq head op)) ,(cadr arg) ,(caddr arg))
							 `(,(cadr (assq head op)) ,(cadr arg) (+ ,@(cddr arg))))))))))))
	;; (zero? (logand...)) is nearly always preceded by not and handled elsewhere
	
	;; ----------------
	((/)
	 (if (pair? (cdr form))
	     (if (and (null? (cddr form))
		      (number? (cadr form))
		      (zero? (cadr form)))
		 (lint-format "attempt to invert zero: ~A" caller (truncated-list->string form))
		 (if (and (pair? (cddr form))
			  (memv 0 (cddr form)))
		     (lint-format "attempt to divide by 0: ~A" caller (truncated-list->string form))))))
	
	;; ----------------
	((copy)
	 (cond ((and (pair? (cdr form))
		     (or (number? (cadr form))
			 (boolean? (cadr form))
			 (char? (cadr form))
			 (and (pair? (cadr form))
			      (memq (caadr form) '(copy string-copy)))
			 (and (pair? (cddr form))
			      (equal? (cadr form) (caddr form)))))
		(lint-format "~A could be ~A" caller (truncated-list->string form) (cadr form)))

	       ((and (pair? (cdr form)) 
		     (equal? (cadr form) '(owlet)))
		(lint-format "~A could be (owlet): owlet is copied internally" caller form))

	       ((= (length form) 5)
		(check-start-and-end caller head (cdddr form) form env))))
	
	;; ----------------
	((string-copy)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(copy string-copy)))
	     (lint-format "~A could be ~A" caller (truncated-list->string form) (cadr form))))
	
	;; ----------------
	((string)
	 (if (every? (lambda (x) 
		       (and (char? x)
			    (char<=? #\space x #\~))) ; #\0xx chars here look dumb
		     (cdr form))
	     (lint-format "~A could be ~S" caller (truncated-list->string form) (apply string (cdr form)))))
	
	;; ----------------
	((string?)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(format number->string)))
	     (if (eq? (caadr form) 'format)
		 (lint-format "format returns either #f or a string, so ~A" caller (lists->string form (cadr form)))
		 (lint-format "number->string always returns a string, so ~A" caller (lists->string form #t)))
	     (check-boolean-affinity caller form env)))
	
	((number?)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (eq? (caadr form) 'string->number))
	     (lint-format "string->number returns either #f or a number, so ~A" caller (lists->string form (cadr form)))
	     (check-boolean-affinity caller form env)))
	
	((symbol? rational? real? complex? float? keyword? gensym? byte-vector? proper-list?
	  char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? c-object?
	  output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?)
	 (check-boolean-affinity caller form env))

	((pair? list?)
	 (check-boolean-affinity caller form env)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(memq memv member assq assv assoc procedure-signature)))
	     (lint-format "~A returns either #f or a pair, so ~A" caller (caadr form)
			  (lists->string form (cadr form)))))

	((integer?)
	 (check-boolean-affinity caller form env)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(char-position string-position)))
	     (lint-format "~A returns either #f or an integer, so ~A" caller (caadr form)
			  (lists->string form (cadr form)))))

	((null?)
	 (check-boolean-affinity caller form env)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(vector->list string->list let->list)))
	     (lint-format "perhaps ~A" caller
			  (lists->string form `(zero? (length ,(cadadr form)))))))
	
	;; ----------------
	((string-ref)
	 (when (and (= (length form) 3)
		    (pair? (cadr form)))
	   (let ((target (cadr form)))
	     (case (car target)
	       ((substring)
		(if (= (length target) 3)
		    (lint-format "perhaps ~A" caller (lists->string form `(string-ref ,(cadr target) (+ ,(caddr form) ,(caddr target)))))))
	       ((symbol->string)
		(if (and (integer? (caddr form))
			 (pair? (cadr target))
			 (eq? (caadr target) 'quote)
			 (symbol? (cadadr target)))
		    (lint-format "perhaps ~A" caller (lists->string form (string-ref (symbol->string (cadadr target)) (caddr form))))))
	       ((make-string)
		(if (and (integer? (cadr target))
			 (integer? (caddr form))
			 (> (cadr target) (caddr form)))
		    (lint-format "perhaps ~A" caller (lists->string form (if (= (length target) 3) (caddr target) #\space)))))))))
	
	;; ----------------
	((vector-ref list-ref hash-table-ref let-ref int-vector-ref float-vector-ref)
	 (unless (= line-number last-checker-line-number)
	   (if (= (length form) 3)
	       (let ((seq (cadr form)))
		 (if (pair? seq)
		     (if (and (eq? (car seq) head) ; perhaps instead: (memq (car seq) '(vector-ref list-ref hash-table-ref let-ref))
			      (= (length seq) 3))
			 (let ((seq1 (cadr seq)))
			   (lint-format "perhaps ~A" caller 
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
			     (lint-format "this doesn't make much sense: ~A~%" caller form))))))
	   (set! last-checker-line-number line-number)))
	
	;; ----------------
	((vector-set! list-set! hash-table-set! float-vector-set! int-vector-set! string-set! let-set!) 
	 (if (= (length form) 4)
	     (let ((target (cadr form))
		   (index (caddr form))
		   (val (cadddr form)))
	       
	       (cond ((and (pair? val)                 ; (vector-set! x 0 (vector-ref x 0))
			   (= (length val) 3)
			   (eq? target (cadr val))
			   (equal? index (caddr val))
			   (memq (car val) '(vector-ref list-ref hash-table-ref string-ref let-ref float-vector-ref int-vector-ref)))
		      (lint-format "redundant ~A: ~A" caller head (truncated-list->string form)))

		     ((and (pair? target)              ; (vector-set! (vector-ref x 0) 1 2) -- vector within vector
			   (not (eq? head 'string-set!))
			   (memq (car target) '(vector-ref list-ref hash-table-ref let-ref float-vector-ref int-vector-ref)))
		      (lint-format "perhaps ~A" caller (lists->string form `(set! (,@(cdr target) ,index) ,val))))

		     ((and (pair? target)              ; (vector-set! (make-vector 3) 1 1) -- does this ever happen?
			   (memq (car target) '(make-vector vector make-string string make-list list append cons vector-append copy inlet sublet)))
		      (lint-format "~A is simply discarded; perhaps ~A" caller
				   (truncated-list->string target)
				   (lists->string form val)))

		     ((code-constant? target)     ; (vector-set! #(0 1 2) 1 3)??
		      (lint-format "~A is a constant that is discarded; perhaps ~A" caller target (lists->string form val)))
		     ))))
	
	;; ----------------
	((object->string)
	 (if (pair? (cdr form))
	     (if (and (pair? (cadr form))
		      (eq? (caadr form) 'object->string))
		 (lint-format "~A could be ~A" caller (truncated-list->string form) (cadr form))
		 (if (pair? (cddr form))
		     (let ((arg2 (caddr form)))
		       (if (or (and (keyword? arg2)
				    (not (eq? arg2 :readable)))
			       (and (code-constant? arg2)
				    (not (boolean? arg2))))
			   (lint-format "bad second argument: ~A" caller arg2)))))))
	
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
		   (lint-format "perhaps ~A" caller (lists->string form `(format ,port ,@(cddr arg))))
		   (if (and (eq? (car arg) 'apply)
			    (pair? (cdr arg))
			    (eq? (cadr arg) 'format)
			    (pair? (cddr arg))
			    (not (caddr arg)))
		       (lint-format "perhaps ~A" caller (lists->string form `(apply format ,port ,@(cdddr arg)))))))))
	
	;; ----------------
	((make-vector make-int-vector make-float-vector)
	 (if (and (= (length form) 4)
		  (eq? head 'make-vector)
		  (code-constant? (caddr form))
		  (not (real? (caddr form)))
		  (eq? (cadddr form) #t))
	     (lint-format "~A won't create an homogeneous vector" caller form))
	 (when (and (pair? (cdr form))
		    (integer? (cadr form))
		    (zero? (cadr form)))
	   (if (pair? (cddr form))
	       (lint-format "initial value is pointless here: ~A" caller form))
	   (lint-format "perhaps ~A" caller (lists->string form #()))))

	;; ----------------
	((make-string make-byte-vector)
	 (when (and (pair? (cdr form))
		    (integer? (cadr form))
		    (zero? (cadr form)))
	   (if (pair? (cddr form))
	       (lint-format "initial value is pointless here: ~A" caller form))
	   (lint-format "perhaps ~A" caller (lists->string form "")))) ; #u8() but (equal? #u8() "") -> #t so lint combines these clauses!
	
	;; ----------------
	((make-list)
	 (when (and (pair? (cdr form))
		    (integer? (cadr form))
		    (zero? (cadr form)))
	   (if (pair? (cddr form))
	       (lint-format "initial value is pointless here: ~A" caller form))
	   (lint-format "perhaps ~A" caller (lists->string form ()))))

	
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
		   (arg-of-arg (cadadr form))
		   (func-of-arg (caadr form)))
	       (if (pair? inv-op) (set! inv-op (cdr inv-op)))
	       
	       (cond ((eq? func-of-arg inv-op)               ; (vector->list (list->vector x)) -> x
		      (if (eq? head 'string->symbol)
			  (lint-format "perhaps ~A" caller (lists->string form arg-of-arg))
			  (lint-format "~A could be (copy ~S)" caller form arg-of-arg)))
		     
		     ((and (eq? head 'list->string)          ; (list->string (vector->list x)) -> (copy x (make-string (length x)))
			   (eq? func-of-arg 'vector->list))
		      (lint-format "perhaps ~A" caller (lists->string form `(copy ,arg-of-arg (make-string (length ,arg-of-arg))))))
		     
		     ((and (eq? head 'list->vector)          ; (list->vector (string->list x)) -> (copy x (make-vector (length x)))
			   (eq? func-of-arg 'string->list))
		      (lint-format "perhaps ~A" caller (lists->string form `(copy ,arg-of-arg (make-vector (length ,arg-of-arg))))))
		     
		     ((and (eq? head 'vector->list)          ; (vector->list (make-vector ...)) -> (make-list ...)
			   (eq? func-of-arg 'make-vector))
		      (lint-format "perhaps ~A" caller (lists->string form `(make-list ,@(cdr arg)))))
		     
		     ((and (eq? head 'list->vector)          ; (list->vector (make-list ...)) -> (make-vector ...)
			   (eq? func-of-arg 'make-list))
		      (lint-format "perhaps ~A" caller (lists->string form `(make-vector ,@(cdr arg)))))
		     
		     ((and (memq func-of-arg '(reverse reverse! copy))
			   (pair? (cadr arg))                ; (list->string (reverse (string->list x))) -> (reverse x)
			   (eq? (caadr arg) inv-op))
		      (lint-format "perhaps ~A" caller (lists->string form `(,(if (eq? func-of-arg 'reverse!) 'reverse func-of-arg) ,(cadadr arg)))))
		     
		     ((and (pair? (cadr arg))
			   (memq func-of-arg '(cdr cddr cdddr cddddr list-tail))
			   (or (and (eq? head 'list->string)
				    (eq? (caadr arg) 'string->list))
			       (and (eq? head 'list->vector)
				    (eq? (caadr arg) 'vector->list))))
		      (let ((len-diff (if (eq? func-of-arg 'list-tail)
					  (caddr arg)
					  (cdr-count func-of-arg))))
			(lint-format "perhaps ~A" caller 
				     (lists->string form (if (eq? head 'list->string)
							     `(substring ,(cadadr arg) ,len-diff)
							     `(copy ,(cadadr arg) (make-vector (- (length ,(cadadr arg)) ,len-diff))))))))
		     
		     ((and (memq head '(list->vector list->string))
			   (eq? func-of-arg 'sort!)
			   (pair? (cadr arg))
			   (eq? (caadr arg) (if (eq? head 'list->vector) 'vector->list 'string->list)))
		      (lint-format "perhaps ~A" caller (lists->string form `(sort! ,(cadadr arg) ,(caddr arg)))))
		     
		     ((and (memq head '(list->vector list->string))
			   (or (memq func-of-arg '(list cons))
			       (quoted-undotted-pair? arg)))
		      (let ((maker (if (eq? head 'list->vector) 'vector 'string)))
			(cond ((eq? func-of-arg 'list)
			       (if (var-member maker env)
				   (lint-format "~A could be simplified, but you've shadowed '~A" caller (truncated-list->string form) maker)
				   (lint-format "perhaps ~A" caller (lists->string form `(,maker ,@(cdr arg))))))
			      ((eq? func-of-arg 'cons)
			       (if (or (null? (caddr arg))
				       (quoted-null? (caddr arg)))
				   (if (var-member maker env)
				       (lint-format "~A could be simplified, but you've shadowed '~A" caller (truncated-list->string form) maker)
				       (lint-format "perhaps ~A" caller (lists->string form `(,maker ,(cadr arg)))))))
			      ((or (null? (cddr form))
				   (and (integer? (caddr form))
					(or (null? (cdddr form))
					    (integer? (cadddr form)))))
			       (lint-format "perhaps ~A" caller 
					    (lists->string form (apply (if (eq? head 'list->vector) vector string) (cadr arg))))))))
		     
		     ((and (eq? head 'list->string)          ; (list->string (reverse x)) -> (reverse (apply string x))
			   (memq func-of-arg '(reverse reverse!)))
		      (lint-format "perhaps ~A" caller (lists->string form `(reverse (apply string ,arg-of-arg)))))

		     ((and (memq head '(string->list vector->list))
			   (= (length form) 4))
		      (check-start-and-end caller head (cddr form) form env))

		     ((and (pair? arg-of-arg)                ; (op (reverse (inv-op x))) -> (reverse x)
			   (eq? func-of-arg 'reverse)
			   (eq? inv-op (car arg-of-arg)))
		      (lint-format "perhaps ~A" caller (lists->string form `(reverse ,(cadr arg-of-arg)))))))))
	 
	 (when (and (pair? (cdr form))
		    (not (pair? (cadr form))))
	   (let ((arg (cadr form)))
	     (if (and (eq? head 'string->list)
		      (string? arg)
		      (or (null? (cddr form))
			  (and (integer? (caddr form))
			       (or (null? (cdddr form))
				   (integer? (cadddr form))))))
		 (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) (apply string->list (cdr form))))))
	 
	 (when (and (memq head '(vector->list string->list))
		    (pair? (cddr form))
		    (pair? (cdddr form))
		    (equal? (caddr form) (cadddr form)))
	   (lint-format "leaving aside errors, ~A is ()~%" caller (truncated-list->string form)))

	 (when (and (memq head '(reverse reverse!))
		    (pair? (cdr form))
		    (pair? (cadr form)))
	   (let ((arg (cadr form)))
	     (if (and (memq (car arg) '(cdr list-tail)) ; (reverse (cdr (reverse lst))) = all but last of lst -> copy to len-1
		      (pair? (cadr arg))
		      (memq (caadr arg) '(reverse reverse!))
		      (symbol? (cadadr arg)))
		 (lint-format "perhaps ~A" caller 
			      (lists->string form `(copy ,(cadadr arg) (make-list (- (length ,(cadadr arg)) ,(if (eq? (car arg) 'cdr) 1 (caddr arg))))))))

	     (if (and (eq? (car arg) 'append) ; (reverse (append (reverse b) res)) = (append (reverse res) b)
		      (pair? (cadr arg))
		      (eq? (caadr arg) 'reverse)
		      (pair? (cddr arg))
		      (null? (cdddr arg)))
		 (lint-format "perhaps ~A" caller (lists->string form `(append (reverse ,(caddr arg)) ,(cadadr arg)))))

	     (if (and (eq? (car arg) 'cons)      ; (reverse (cons x (reverse lst))) -- adds x to end -- (append lst (list x))
		      (pair? (caddr arg))
		      (memq (car (caddr arg)) '(reverse reverse!)))
		 (lint-format "perhaps ~A" caller (lists->string form `(append ,(cadr (caddr arg)) (list ,(cadr arg)))))))))
	
	;; ----------------
	((char->integer integer->char symbol->keyword keyword->symbol string->number)
	 (let ((inverses '((char->integer . integer->char)
			   (integer->char . char->integer)
			   (symbol->keyword . keyword->symbol)
			   (keyword->symbol . symbol->keyword)
			   (string->number . number->string))))
	   (cond ((and (pair? (cdr form))
		       (pair? (cadr form))
		       (pair? (cdadr form))
		       (eq? (caadr form) (cond ((assq head inverses) => cdr))))
		  (lint-format "~A could be ~A" caller (truncated-list->string form) (cadadr form)))

		 ((and (eq? head 'integer->char)
		       (pair? (cdr form))
		       (integer? (cadr form))
		       (or (<= 32 (cadr form) 127)
			   (memv (cadr form) '(0 7 8 9 10 13 27))))
		  (lint-format "perhaps ~A -> ~W" caller (truncated-list->string form) (integer->char (cadr form))))

		 ((and (eq? head 'symbol->keyword)
		       (pair? (cdr form))
		       (pair? (cadr form))
		       (eq? (caadr form) 'string->symbol))
		  (lint-format "perhaps ~A" caller (lists->string form `(make-keyword ,(cadadr form))))))))
	
	;; ----------------
	((string-append)
	 (if (not (= line-number last-checker-line-number))
	     (let ((args (remove-all "" (splice-if (lambda (x) (eq? x 'string-append)) (cdr form)))))
	       (cond ((null? args)
		      (lint-format "perhaps ~A" caller (lists->string form "")))
		     ((null? (cdr args))
		      (lint-format "perhaps ~A, or use copy" caller (lists->string form (car args))))
		     ((every? string? args)
		      (lint-format "perhaps ~A" caller (lists->string form (apply string-append args))))
		     ((not (equal? args (cdr form)))
		      (lint-format "perhaps ~A" caller (lists->string form `(string-append ,@args)))))
	       (set! last-checker-line-number line-number))))
	
	;; ----------------
	((vector-append)
	 (if (not (= line-number last-checker-line-number))
	     (let ((args (remove-all #() (splice-if (lambda (x) (eq? x 'vector-append)) (cdr form)))))
	       (cond ((null? args)
		      (lint-format "perhaps ~A" caller (lists->string form #())))
		     ((null? (cdr args))
		      (lint-format "perhaps ~A" caller (lists->string form `(copy ,(car args)))))
		     ((every? vector? args)
		      (lint-format "perhaps ~A" caller (lists->string form (apply vector-append args))))
		     ((not (equal? args (cdr form)))
		      (lint-format "perhaps ~A" caller (lists->string form `(vector-append ,@args)))))
	       (set! last-checker-line-number line-number))))
	
	;; ----------------
	((cons)
	 (if (= (length form) 3)
	     (if (and (pair? (caddr form))
		      (eq? (caaddr form) 'list))   ; (cons x (list ...)) -> (list x ...)
		 (lint-format "perhaps ~A" caller (lists->string form `(list ,(cadr form) ,@(cdaddr form))))
		 (if (or (null? (caddr form))      ; (cons x '()) -> (list x)
			 (quoted-null? (caddr form)))
		     (lint-format "perhaps ~A" caller (lists->string form `(list ,(cadr form))))))))
	
	;; ----------------
	((append)
	 (unless (= line-number last-checker-line-number)
	   (set! last-checker-line-number line-number)
	   (letrec ((splice-append (lambda (lst)
				     (cond ((null? lst)
					    ())
					   ((not (pair? lst))
					    lst)
					   ((and (pair? (car lst))
						 (eq? (caar lst) 'append))
					    (if (null? (cdar lst))
						(if (null? (cdr lst)) ; (append) at end -> () to keep copy intact?
						    (list ())
						    (splice-append (cdr lst)))
						(append (splice-append (cdar lst)) (splice-append (cdr lst)))))
					   ((or (null? (cdr lst))
						(not (or (null? (car lst))
							 (quoted-null? (car lst))
							 (and (pair? (car lst))
							      (eq? (caar lst) 'list)
							      (null? (cdar lst))))))
					    (cons (car lst) (splice-append (cdr lst))))
					   (else (splice-append (cdr lst)))))))

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
		   ((0)					              ; (append) -> ()
		    (lint-format "perhaps ~A" caller (lists->string form ())))
		   ((1)                                               ; (append x) -> x
		    (lint-format "perhaps ~A" caller (lists->string form (car new-args))))
		   ((2)                                               ; (append (list x) ()) -> (list x)
		    (let ((arg2 (cadr new-args))
			  (arg1 (car new-args)))
		      (cond ((or (null? arg2)           
				 (quoted-null? arg2)
				 (equal? arg2 '(list)))               ; (append x ()) -> (copy x)
			     (lint-format "perhaps clearer: ~A" caller (lists->string form `(copy ,arg1))))
			    
			    ((null? arg1)                             ; (append () x) -> x
			     (lint-format "perhaps ~A" caller (lists->string form arg2)))
			    
			    ((not (pair? arg1)))

			    ((and (pair? arg2)                        ; (append (list x y) '(z)) -> (list x y 'z)
				  (or (eq? (car arg1) 'list)
				      (quoted-undotted-pair? arg1))
				  (or (eq? (car arg2) 'list)
				      (quoted-undotted-pair? arg2)))
			     (lint-format "perhaps ~A" caller (lists->string form (apply append->list new-args))))
				   
			    ((and (eq? (car arg1) 'list)              ; (append (list x) y) -> (cons x y)
				  (pair? (cdr arg1))
				  (null? (cddr arg1)))
			     (lint-format "perhaps ~A" caller (lists->string form `(cons ,(cadr arg1) ,arg2))))
				   
			    ((not (equal? (cdr form) new-args))
			     (lint-format "perhaps ~A" caller (lists->string form `(append ,@new-args)))))))
		   (else
		    (if (every? (lambda (item)
				  (and (pair? item)
				       (or (eq? (car item) 'list)
					   (quoted-undotted-pair? item))))
				new-args)
			(lint-format "perhaps ~A" caller (lists->string form (apply append->list new-args))))))
		 
		 (if (and (= made-suggestion suggestion)
			  (not (equal? (cdr form) new-args)))
		     (lint-format "perhaps ~A" caller (lists->string form `(append ,@new-args)))))))))
	
	;; ----------------
	((apply)
	 (when (pair? (cdr form))
	   (let ((len (length form))
		 (suggestion made-suggestion))
	     (if (= len 2)
		 (lint-format "perhaps ~A" caller (lists->string form (list (cadr form))))
		 (if (not (or (<= len 2) ; it might be (apply)...
			      (symbol? (cadr form))
			      (applicable? (cadr form))))
		     (lint-format "~S is not applicable: ~A" caller (cadr form) (truncated-list->string form))
		     (let ((happy #f)
			   (f (cadr form)))
		       (unless (or (<= len 2)
				   (any-macro? f env)
				   (eq? f 'macroexpand)) ; handled specially (syntactic, not a macro)
			 
			 (when (and (symbol? f)
				    (not (var-member f env)))
			   (let ((func (symbol->value f *e*)))
			     (if (procedure? func)
				 (let ((ary (arity func)))
				   (if (and (pair? ary)
					    (> (- (length (cddr form)) 1) (cdr ary))) ; last apply arg might be var=()
				       (lint-format "too many arguments for ~A: ~A" caller f form))))))
			 
			 (let ((last-arg (form (- len 1))))
			   (if (and (not (list? last-arg))
				    (code-constant? last-arg))
			       (lint-format "last argument should be a list: ~A" caller (truncated-list->string form))
			       (if (= len 3)
				   (let ((args (caddr form)))
				     (if (identity? f)                         ; (apply (lambda (x) x) y) -> (car y)
					 (lint-format "perhaps (assuming ~A is a list of one element) ~A" caller args 
						      (lists->string form `(car ,args)))
					 (if (simple-lambda? f)                ; (apply (lambda (x) (f x)) y) -> (f (car y))
					     (lint-format "perhaps (assuming ~A is a list of one element) ~A" caller args 
							  (lists->string form (tree-subst (list 'car args) (caadr f) (caddr f))))))
				     
				     (cond ((eq? f 'list)                      ; (apply list x) -> x?
					    (lint-format "perhaps ~A" caller (lists->string form args)))

					   ((or (null? args)                   ; (apply f ()) -> (f)
						(quoted-null? args))
					    (lint-format "perhaps ~A" caller (lists->string form (list (cadr form)))))

					   ((not (pair? args)))

					   ((eq? (car args) 'list)             ; (apply f (list a b)) -> (f a b)
					    (lint-format "perhaps ~A" caller (lists->string form `(,f ,@(cdr args)))))
					   
					   ((and (eq? (car args) 'quote)       ; (apply eq? '(a b)) -> (eq? 'a 'b)
						 (= suggestion made-suggestion))
					    (lint-format "perhaps ~A" caller (lists->string form `(,f ,@(distribute-quote (cadr args))))))
					   
					   ((eq? (car args) 'cons)             ; (apply f (cons a b)) -> (apply f a b)
					    (lint-format "perhaps ~A" caller (lists->string form `(apply ,f ,@(cdr args)))))
					   
					   ((and (memq f '(string vector int-vector float-vector))
						 (memq (car args) '(reverse reverse!))) ; (apply vector (reverse x)) -> (reverse (apply vector x))
					    (lint-format "perhaps ~A" caller (lists->string form `(reverse (apply ,f ,(cadr args))))))
					   
					   ((and (eq? f 'string-append)        ; (apply string-append (map ...))
						 (eq? (car args) 'map))
					    (if (eq? (cadr args) 'symbol->string)
						(lint-format "perhaps ~A" caller ; (apply string-append (map symbol->string ...))
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
							    (lint-format "perhaps ~A" caller
									 (lists->string form `(format #f ,str ,(caddr args))))))))))
					   
					   ((and (eq? (car args) 'append)      ; (apply f (append (list ...)...)) -> (apply f ... ...)
						 (pair? (cadr args))
						 (eq? (caadr args) 'list))
					    (lint-format "perhaps ~A" caller 
							 (lists->string form `(apply ,f ,@(cdadr args)
										     ,(if (null? (cddr args)) ()
											  (if (null? (cdddr args)) (caddr args)
											      `(append ,@(cddr args))))))))))
				   (begin ; len > 3
				     (when (and (pair? last-arg)
						(eq? (car last-arg) 'list)            ; (apply f y z (list a b)) -> (f y z a b)
						(not (hash-table-ref syntaces (cadr form)))) ; also not any-macro I presume
				       (lint-format "perhaps ~A" caller 
						    (lists->string form 
								   `(,@(copy (cdr form) (make-list (- len 2))) 
								     ,@(cdr last-arg)))))
				     
				     ;; can't cleanly go from (apply write o p) to (write o (car p)) since p can be ()

				     (when (and (not happy)
						(not (memq f '(define define* define-macro define-macro* define-bacro define-bacro* lambda lambda*)))
						(or (null? last-arg)
						    (quoted-null? last-arg)))     ; (apply f ... ()) -> (f ...)
				       (lint-format "perhaps ~A" caller (lists->string form `(,f ,@(copy (cddr form) (make-list (- len 3))))))))))))))))))
	
	;; ----------------
	((format snd-display)
	 (if (< (length form) 3)
	     (begin
	       (cond ((< (length form) 2)
		      (lint-format "~A has too few arguments: ~A" caller head (truncated-list->string form)))
		     ((and (pair? (cadr form))
			   (eq? (caadr form) 'format))
		      (lint-format "redundant format: ~A" caller (truncated-list->string form)))
		     ((and (code-constant? (cadr form))
			   (not (string? (cadr form))))
		      (lint-format "format with one argument takes a string: ~A" caller (truncated-list->string form)))
		     ((and (not (cadr form))
			   (string? (caddr form)))
		      (lint-format "perhaps ~A" caller (lists->string form (caddr form)))))
	       env)
	     (let ((control-string (if (string? (cadr form)) (cadr form) (caddr form)))
		   (args (if (string? (cadr form)) (cddr form) (cdddr form))))
	       
	       (define (count-directives str caller form)
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
						      (if (not (or (char-numeric? (string-ref str k))
								   (char=? (string-ref str k) #\,)))
							  (return (char-ci=? (string-ref str k) #\t))))))))
				       (begin
					 ;; the possibilities are endless, so I'll stick to the simplest
					 (if (not (vector-ref format-control-char (char->integer c)))
					     (lint-format "unrecognized format directive: ~C in ~S, ~S" caller c str form))
					 (set! dirs (+ dirs 1))
					 
					 ;; ~n so try to figure out how many args are needed (this is not complete)
					 (when (char-ci=? c #\n)
					   (let ((j (+ i 1)))
					     (if (>= j len)
						 (lint-format "missing format directive: ~S" caller str)
						 (begin
						   ;; if ,n -- add another, if then not T, add another
						   (if (char=? (string-ref str j) #\,)
						       (cond ((>= (+ j 1) len)
							      (lint-format "missing format directive: ~S" caller str))
							     ((char-ci=? (string-ref str (+ j 1)) #\n)
							      (set! dirs (+ dirs 1))
							      (set! j (+ j 2)))
							     ((char-numeric? (string-ref str (+ j 1)))
							      (set! j (+ j 2)))
							     (else (set! j (+ j 1)))))
						   (if (>= j len)
						       (lint-format "missing format directive: ~S" caller str)
						       (if (not (char-ci=? (string-ref str j) #\t))
							   (set! dirs (+ dirs 1))))))))))
				   
				   (set! tilde-time #f)
				   (case c 
				     ((#\{) (set! curlys (+ curlys 1)))
				     ((#\}) (set! curlys (- curlys 1)))
				     ((#\^ #\|)
				      (if (zero? curlys)
					  (lint-format "~A has ~~~C outside ~~{~~}?" caller str c))))
				   (if (and (< (+ i 2) len)
					    (member (substring str i (+ i 3)) '("%~&" "^~^" "|~|" "&~&" "\n~\n") string=?))
				       (lint-format "~A in ~A could be ~A" caller
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
			     (lint-format "~A control string ends in tilde: ~A" caller head (truncated-list->string form)))))
		   
		   (if (not (= curlys 0))
		       (lint-format "~A has ~D unmatched ~A~A: ~A"
				    caller head 
				    (abs curlys) 
				    (if (positive? curlys) "{" "}") 
				    (if (> curlys 1) "s" "") 
				    (truncated-list->string form)))
		   dirs))
	       
	       (when (pair? args)
		 (for-each
		  (lambda (a)
		    (if (and (pair? a)
			     (memq (car a) '(number->string symbol->string)))
			(if (null? (cddr a))
			    (lint-format "format arg ~A could be ~A" caller a (cadr a))
			    (if (and (pair? (cddr a))
				     (integer? (caddr a))
				     (memv (caddr a) '(2 8 10 16)))
				(if (= (caddr a) 10)
				    (lint-format "format arg ~A could be ~A" caller a (cadr a))
				    (lint-format "format arg ~A could use the format directive ~~~A and change the argument to ~A" caller a
						 (case (caddr a) ((2) "B") ((8) "O") (else "X"))
						 (cadr a)))))))
		  args))
	       
	       (when (and (eq? head 'format)
			  (string? (cadr form)))
		 (lint-format "please include the port argument to format, perhaps ~A" caller `(format () ,@(cdr form))))
	       
	       (if (not (string? control-string))
		   (if (not (proper-list? args))
		       (lint-format "~S looks suspicious" caller form))
		   (let ((ndirs (count-directives control-string caller form))
			 (nargs (if (list? args) (length args) 0)))
		     (let ((pos (char-position #\null control-string)))
		       (if (and pos (< pos (length control-string)))
			   (lint-format "#\\null in a format control string will confuse both lint and format: ~S in ~A" caller control-string form)))
		     (if (not (= ndirs nargs))
			 (lint-format "~A has ~A arguments: ~A" 
				      caller head 
				      (if (> ndirs nargs) "too few" "too many")
				      (truncated-list->string form))
			 (if (and (not (cadr form))
				  (zero? ndirs)
				  (not (char-position #\~ control-string)))
			     (lint-format "~A could be ~S, (format is a no-op here)" caller (truncated-list->string form) (caddr form)))))))))
	
	;; ----------------
	((sort!)
	 (if (= (length form) 3)
	     (let ((func (caddr form)))
	       (if (memq func '(= eq? eqv? equal? string=? char=? string-ci=? char-ci=?))
		   (lint-format "sort! with ~A may hang: ~A" caller func (truncated-list->string form))
		   (if (symbol? func)
		       (let ((sig (procedure-signature (symbol->value func))))
			 (if (and (pair? sig)
				  (not (eq? (car sig) 'boolean?)))
			     (lint-format "~A is a questionable sort! function" caller func))))))))
	
	;; ----------------
	((substring) 
	 (if (every? code-constant? (cdr form))
	     (catch #t
	       (lambda ()
		 (let ((val (eval form)))
		   (lint-format "perhaps ~A -> ~S" caller (truncated-list->string form) val)))
	       (lambda (type info)
		 (lint-format "~A -> ~A~%" caller (truncated-list->string form) (apply format #f info))))
	     
	     (let ((str (cadr form)))
	       (if (and (pair? str)
			(eq? (car str) 'substring)
			(pair? (cddr form))
			(null? (cdddr form))
			(null? (cdddr str)))
		   (if (and (integer? (caddr form))
			    (integer? (caddr str)))
		       (lint-format "perhaps ~A" caller 
				    (lists->string form `(substring ,(cadr str) ,(+ (caddr str) (caddr form)))))
		       (lint-format "perhaps ~A" caller 
				    (lists->string form `(substring ,(cadr str) (+ ,(caddr str) ,(caddr form)))))))
	       ;; end indices are complicated -- since this rarely happens, not worth the trouble
	       (if (and (integer? (caddr form))
			(zero? (caddr form))
			(null? (cdddr form)))
		   (lint-format "perhaps clearer: ~A" caller (lists->string form `(copy ,str))))
	       (if (and (pair? (cdddr form))
			(equal? (caddr form) (cadddr form)))
		   (lint-format "leaving aside errors, ~A is \"\"" caller form)))))
	
	;; ----------------
	((list-tail)
	 (if (= (length form) 3)
	     (if (eqv? (caddr form) 0)
		 (lint-format "perhaps ~A" caller (lists->string form (cadr form)))
		 (if (and (pair? (cadr form))
			  (eq? (caadr form) 'list-tail))
		     (if (and (integer? (caddr form))
			      (integer? (caddr (cadr form))))
			 (lint-format "perhaps ~A" caller 
				      (lists->string form `(list-tail ,(cadadr form) ,(+ (caddr (cadr form)) (caddr form)))))
			 (lint-format "perhaps ~A" caller 
				      (lists->string form `(list-tail ,(cadadr form) (+ ,(caddr (cadr form)) ,(caddr form))))))))))
	
	;; ----------------
	((eq?) 
	 (if (< (length form) 3)
	     (lint-format "eq? needs 2 arguments: ~A" caller (truncated-list->string form))
	     (let* ((arg1 (cadr form))
		    (arg2 (caddr form))
		    (eq1 (eqf arg1))
		    (eq2 (eqf arg2))
		    (specific-op (and (eq? (cadr eq1) (cadr eq2))
				      (not (memq (cadr eq1) '(eqv? equal?)))
				      (cadr eq1))))

	       (eval-constant-expression caller form)

	       (if (or (eq? (car eq1) 'equal?)
		       (eq? (car eq2) 'equal?))
		   (lint-format "eq? should be equal?~A in ~S" caller (if specific-op (format #f " or ~A" specific-op) "") form)
		   (if (or (eq? (car eq1) 'eqv?)
			   (eq? (car eq2) 'eqv?))
		       (lint-format "eq? should be eqv?~A in ~S" caller (if specific-op (format #f " or ~A" specific-op) "") form)))
	       
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
		     (lint-format "perhaps ~A" caller (lists->string form expr)))))))
	
	;; ----------------
	((eqv? equal? morally-equal?) 
	 (if (< (length form) 3)
	     (lint-format "~A needs 2 arguments: ~A" caller head (truncated-list->string form))
	     (let* ((arg1 (cadr form))
		    (arg2 (caddr form))
		    (eq1 (eqf arg1))
		    (eq2 (eqf arg2))
		    (specific-op (and (eq? (cadr eq1) (cadr eq2))
				      (not (memq (cadr eq1) '(eq? eqv? equal?)))
				      (cadr eq1))))

	       (eval-constant-expression caller form)

	       (cond ((or (eq? (car eq1) 'equal?)
			  (eq? (car eq2) 'equal?))
		      (if (memq head '(equal? morally-equal?))
			  (if specific-op
			      (lint-format "~A could be ~A in ~S" caller head specific-op form))
			  (lint-format "~A should be equal?~A in ~S" caller head 
				       (if specific-op (format #f " or ~A" specific-op) "") 
				       form)))
		     ((or (eq? (car eq1) 'eqv?)
			  (eq? (car eq2) 'eqv?))
		      (if (memq head '(eqv? morally-equal?))
			  (if specific-op
			      (lint-format "~A could be ~A in ~S" caller head specific-op form))
			  (lint-format "~A ~A be eqv?~A in ~S" caller head 
				       (if (eq? head 'eq?) "should" "could") 
				       (if specific-op (format #f " or ~A" specific-op) "")
				       form)))
		     ((not (or (eq? (car eq1) 'eq?)
			       (eq? (car eq2) 'eq?))))

		     ((not (and arg1 arg2))
		      (lint-format "~A could be not: ~A" caller head (lists->string form `(not ,(or arg1 arg2)))))

		     ((or (null? arg1) 
			  (null? arg2)
			  (quoted-null? arg1) (quoted-null? arg2))
		      (lint-format "~A could be null?: ~A" caller head
				   (lists->string form 
						  (if (or (null? arg1) (quoted-null? arg1))
						      `(null? ,arg2)
						      `(null? ,arg1)))))
		     ((not (eq? head 'eq?))
		      (lint-format "~A could be eq?~A in ~S" caller head 
				   (if specific-op (format #f " or ~A" specific-op) "") 
				   form))))))
	
	;; ----------------
	((map for-each)
	 (let* ((len (length form))
		(args (- len 2)))
	   (if (< len 3)
	       (lint-format "~A missing argument~A in: ~A"
			    caller head 
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
			   (set! ary (if (eq? (car func) 'lambda)
					 (if (negative? arglen) 
					     (cons (abs arglen) 512000)
					     (cons arglen arglen))
					 (cons 0 (if (or (negative? arglen)
							 (memq :rest (cadr func)))
						     512000 arglen)))))))
		 (if (pair? ary)
		     (if (< args (car ary))
			 (lint-format "~A has too few arguments in: ~A"
				      caller head 
				      (truncated-list->string form))
			 (if (> args (cdr ary))
			     (lint-format "~A has too many arguments in: ~A"
					  caller head 
					  (truncated-list->string form)))))
		 (for-each 
		  (lambda (obj)
		    (if (and (pair? obj)
			     (memq (car obj) '(vector->list string->list let->list)))
			(lint-format "~A could be simplified to: ~A ; (~A accepts non-list sequences)" 
				     caller
				     (truncated-list->string obj) 
				     (truncated-list->string (cadr obj))
				     head)))
		  (cddr form))
		 
		 (when (eq? head 'map)
		   (when (and (memq func '(char-downcase char-upcase))
			      (pair? (caddr form))
			      (eq? (caaddr form) 'string->list))
		     (lint-format "perhaps ~A" caller (lists->string form `(string->list (,(if (eq? func 'char-upcase) 'string-upcase 'string-downcase) 
											,(cadr (caddr form)))))))
		   (when (identity? func) ; to check f here as var is more work
		     (lint-format "perhaps ~A" caller (lists->string form (caddr form)))))
		 
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
		       (lint-format "~A accepts ~A arguments, so perhaps ~A" caller head 
				    (if string-case 'string 'vector)
				    (lists->string arg1 (if string-case
							    `(substring ,(cadadr arg1) ,len-diff)
							    `(make-shared-vector ,(cadadr arg1) (- (length ,(cadadr arg1)) ,len-diff) ,len-diff)))))))
		 (when (and (eq? head 'for-each)
			    (pair? (cadr form))
			    (eq? (caadr form) 'lambda)
			    (pair? (cdadr form))
			    (not (any? (lambda (x) (side-effect? x env)) (cddadr form))))
		   (lint-format "pointless for-each: ~A" caller (truncated-list->string form)))
		 
		 (when (= args 1)
		   (let ((seq (caddr form)))
		     
		     (when (pair? seq)
		       (case (car seq)
			 ((cons)
			  (if (and (pair? (cdr seq))
				   (pair? (cddr seq))
				   (code-constant? (caddr seq)))
			      (lint-format "~A will ignore ~S in ~A" caller head (caddr seq) seq)))
			 
			 ((list)
			  (if (and (pair? (cdr seq))
				   (null? (cddr seq)))
			      (let* ((list-arg (cadr seq))
				     (sig (and (pair? list-arg)
					       (arg-signature seq env))))
				(if (not (and (pair? sig)
					      (pair? (car sig))
					      (memq 'values (car sig))))
				    (lint-format "~Aperhaps ~A" caller
						 (if (or sig
							 (code-constant? list-arg))
						     ""
						     (format #f "assuming ~A does not return multiple values, " (cadr seq)))
						 (lists->string form `(list (,(cadr form) ,list-arg))))))))
			 
			 ((map)
			  (when (= (length seq) 3)
			  ;; a toss-up -- probably faster to combine funcs here, and easier to read?
			  ;;   but only if first arg is only used once in first func, and everything is simple (one-line or symbol)
			  (let* ((seq-func (cadr seq))
				 (arg-name (find-unique-name func seq-func)))
			    
			    (if (symbol? func)
				(if (symbol? seq-func)              
				    ;; (map f (map g h)) -> (map (lambda (x) (f (g x))) h) -- dubious...
				    (lint-format "perhaps ~A" caller 
						 (lists->string form `(,head (lambda (,arg-name) 
									       (,func (,seq-func ,arg-name))) 
									     ,(caddr seq))))
				    (if (simple-lambda? seq-func)   
					;; (map f (map (lambda (x) (g x)) h)) -> (map (lambda (x) (f (g x))) h)
					(lint-format "perhaps ~A" caller 
						     (lists->string form `(,head (lambda (,arg-name)
										   (,func ,(tree-subst arg-name (caadr seq-func) (caddr seq-func))))
										 ,(caddr seq))))))
				(if (less-simple-lambda? func)
				    (if (symbol? seq-func)          
					;; (map (lambda (x) (f x)) (map g h)) -> (map (lambda (x) (f (g x))) h)
					(lint-format "perhaps ~A" caller 
						     (lists->string form `(,head (lambda (,arg-name)
										   ,@(tree-subst (list seq-func arg-name) (caadr func) (cddr func)))
										 ,(caddr seq))))
					(if (simple-lambda? seq-func) 
					    ;; (map (lambda (x) (f x)) (map (lambda (x) (g x)) h)) -> (map (lambda (x) (f (g x))) h)
					    (lint-format "perhaps ~A" caller  
							 (lists->string form `(,head (lambda (,arg-name)
										       ,@(tree-subst (tree-subst arg-name (caadr seq-func) (caddr seq-func))
												     (caadr func) (cddr func)))
										     ,(caddr seq)))))))))))))
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
						       (eqv? (cadr x) #\space)
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
			       (lint-format "perhaps ~A" caller 
					    (lists->string form `(format ,op ,(string-append "~{" ctrl-string "~}") ,seq))))))))
		     ))))))
      
	;; ----------------
	((magnitude)
	 (if (and (= (length form) 2)
		  (memq (->type (cadr form)) '(integer? rational? real?)))
	     (lint-format "perhaps use abs here: ~A" caller form)))
	
	;; ----------------
	((null eq eqv equal) ; (null (cdr...)) 
	 (if (not (var-member head env))
	     (lint-format "misspelled '~A? in ~A?" caller head form)))
	
	((interaction-environment)
	 (lint-format "interaction-environment is probably curlet in s7" caller))
	
	;; ----------------
	((open-input-file open-output-file)
	 (if (and (pair? (cdr form))
		  (pair? (cddr form))
		  (string? (caddr form))
		  (not (memv (string-ref (caddr form) 0) '(#\r #\w #\a)))) ; b + then e m c x if gcc
	     (lint-format "unexpected mode: ~A" caller form)))
	
	;; ----------------
	((values)
	 (if (member 'values (cdr form) (lambda (a b)
					  (and (pair? b)
					       (eq? (car b) 'values))))
	     (lint-format "perhaps ~A" caller (lists->string form `(values ,@(splice-if (lambda (x) (eq? x 'values)) (cdr form)))))
	     (if (= (length form) 2)
		 (lint-format "perhaps ~A" caller (lists->string form (cadr form))))))
	
	;; ----------------
	((call-with-values)  ; (call/values p c) -> (c (p))
	 (if (= (length form) 3)
	     (let ((producer (cadr form))
		   (consumer (caddr form)))
	       
	       (let* ((produced-values (mv-range producer env))
		      (consumed-values (and produced-values
					    (or (and (symbol? consumer)
						     (arg-arity consumer env)) ; TODO: use elsewhere
						(and (pair? consumer)
						     (eq? (car consumer) 'lambda)
						     (pair? (cadr consumer))
						     (let ((len (length (cadr consumer))))
						       (if (negative? len)
							   (cons (abs len) 536870912)
							   (cons len len))))))))
		 
					;(format *stderr* "~A ~A ~A~%~%" form produced-values consumed-values)
		 (if (and consumed-values
			  (or (> (car consumed-values) (car produced-values))
			      (< (cdr consumed-values) (cadr produced-values))))
		     (let ((clen ((if (> (car consumed-values) (car produced-values)) car cdr) consumed-values)))
		       (lint-format "call-with-values consumer ~A wants ~D value~P, but producer ~A returns ~A" 
				    caller
				    (truncated-list->string consumer)
				    clen clen
				    (truncated-list->string producer)
				    ((if (> (car consumed-values) (car produced-values)) car cadr) produced-values)))))
	       
	       (cond ((not (pair? producer))
		      (if (and (symbol? producer)
			       (not (memq (return-type producer ()) '(#t #f values))))
			  (lint-format "~A does not return multiple values" caller producer)
			  (lint-format "perhaps ~A" caller (lists->string form `(,consumer (,producer))))))

		     ((not (eq? (car producer) 'lambda))
		      (lint-format "perhaps ~A" caller (lists->string form `(,consumer (,producer)))))

		     ((pair? (cadr producer))
		      (lint-format "~A requires too many arguments" caller (truncated-list->string producer)))
		     
		     ((symbol? (cadr producer))
		      (lint-format "~A's parameter ~A will always be ()" caller (truncated-list->string producer) (cadr producer)))

		     ((and (pair? (cddr producer))
			   (null? (cdddr producer)))
		      (let ((body (caddr producer)))
			(if (or (code-constant? body)
				(and (pair? body)
				     (symbol? (car body))
				     (not (memq (return-type (car body) ()) '(#t #f values)))))
			    (lint-format "~A does not return multiple values" caller body)
			    (lint-format "perhaps ~A" caller 
					 (lists->string form 
							(if (and (pair? body)
								 (eq? (car body) 'values))
							    `(,consumer ,@(cdr body))
							    `(,consumer ,body)))))))

		     (else (lint-format "perhaps ~A" caller (lists->string form `(,consumer (,producer)))))))))
		       
	
	;; ----------------
	((multiple-value-bind) 
	 (if (= (length form) 4)
	     (let ((vars (cadr form))
		   (producer (caddr form))
		   (body (cdddr form)))

	       (if (null? vars)
		   (lint-format "this multiple-value-bind is pointless; perhaps ~A" caller
				(lists->string form 
					       (if (side-effect? producer env)
						   `(begin ,producer ,@body)
						   (if (null? (cdr body))
						       (car body)
						       `(begin ,@body)))))

		   (if (not (symbol? vars)) ; else any number of values is ok
		       (let ((vals (mv-range producer env))
			     (args (length vars)))
			 (if (and vals
				  (or (< args (car vals))
				      (> args (cadr vals))))
			     (lint-format "multiple-value-bind wants ~D values, but ~A returns ~A" 
					  caller args 
					  (truncated-list->string producer)
					  (if (< args (car vals)) (car vals) (cadr vals))))

			 (if (and (pair? producer)
				  (symbol? (car producer))
				  (not (memq (return-type (car producer) ()) '(#t #f values))))
			     (lint-format "~A does not return multiple values" caller (car producer))
			     (if (and (null? (cdr body))
				      (pair? (car body))
				      (equal? vars (cdar body))
				      (defined? (caar body))
				      (equal? (arity (symbol->value (caar body))) (cons args args)))
				 (lint-format "perhaps ~A" caller (lists->string form `(,(caar body) ,producer)))))))))))
	
	;; ----------------
	((eval)
	 (when (= (length form) 2)
	   (let ((arg (cadr form)))
	     (cond ((not (pair? arg))
		    (if (not (symbol? arg))
			(lint-format "this eval is pointless; perhaps ~A" caller (lists->string form arg))))

		   ((eq? (car arg) 'quote)
		    (lint-format "perhaps ~A" caller (lists->string form (cadr arg))))

		   ((eq? (car arg) 'string->symbol)
		    (lint-format "perhaps ~A" caller (lists->string form (string->symbol (cadr arg)))))

		   ((and (eq? (car arg) 'read)
			 (= (length arg) 2)
			 (pair? (cadr arg))
			 (eq? (caadr arg) 'open-input-string))
		    (lint-format "perhaps ~A" caller (lists->string form `(eval-string ,(cadadr arg)))))))))
	
	;; ----------------
	((fill! string-fill! list-fill! vector-fill!)
	 (if (= (length form) 5)
	     (check-start-and-end caller head (cdddr form) form env)))
	;;   (fill! (cdr x) y) -> (fill! x y 1)?
	;;   also fill! returns y, so (fill! (list 1 2 3) y) is y
	;;   (fill! (reverse! x) #f) is not what you want

	;; ----------------
	((write-string)
	 (if (= (length form) 4)
	     (check-start-and-end caller head (cddr form) form env)))
	
	;; ----------------
	((string-length)
	 (if (and (= (length form) 2)
		  (string? (cadr form)))
	     (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) (string-length (cadr form)))))
	
	;; ----------------
	((vector-length)
	 (if (and (= (length form) 2)
		  (vector? (cadr form)))
	     (lint-format "perhaps ~A -> ~A" caller (truncated-list->string form) (vector-length (cadr form)))))

	;; ----------------
	((dynamic-wind)
	 (if (= (length form) 4)
	     (let ((init (cadr form))
		   (body (caddr form))
		   (end (cadddr form))
		   (empty 0))
	       ;; (equal? init end) as a mistake doesn't seem to happen

	       (when (and (pair? init)
			  (eq? (car init) 'lambda))
		 (if (not (null? (cadr init)))
		     (lint-format "dynamic-wind init function should be a thunk: ~A" caller init))
		 (if (pair? (cddr init))
		     (let ((last-expr (list-ref init (- (length init) 1))))
		       (if (not (pair? last-expr))
			   (if (null? (cdddr init))
			       (set! empty 1))
			   (unless (side-effect? last-expr env)
			     (if (null? (cdddr init))
				 (set! empty 1))
			     (lint-format "this could be omitted: ~A in ~A" caller last-expr init))))))

	       (if (and (pair? body)
			(eq? (car body) 'lambda))
		   (if (not (null? (cadr body)))
		       (lint-format "dynamic-wind body function should be a thunk: ~A" caller body))
		   (set! empty 3)) ; don't try to access body below

	       (when (and (pair? end)
			  (eq? (car end) 'lambda))
		 (if (not (null? (cadr end)))
		     (lint-format "dynamic-wind end function should be a thunk: ~A" caller end))
		 (if (pair? (cddr end))
		     (let ((last-expr (list-ref end (- (length end) 1))))
		       (if (not (pair? last-expr))
			   (if (null? (cdddr end))
			       (set! empty (+ empty 1)))
			   (unless (side-effect? last-expr env) ; or if no side-effects in any (also in init)
			     (if (null? (cdddr end))
				 (set! empty (+ empty 1)))
			     (lint-format "this could be omitted: ~A in ~A" caller last-expr end)))
		       (if (= empty 2)
			   (lint-format "this dynamic-wind is pointless, ~A" caller 
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
				     gc-stats symbol-table-locked? c-objects history-size profile-info))))
		   (lint-format "unknown *s7* field: ~A" caller arg)))))


	((push!) ; not predefined
	 (if (= (length form) 3)
	     (set-set (caddr form) form env)))
	((pop!)  ; also not predefined
	 (if (= (length form) 2)
	     (set-set (cadr form) form env)))
	)) ; end check-special-cases
    
    
    (define (prettify-checker-unq op)
      (if (pair? op)
	  (string-append (prettify-checker-unq (car op)) " or " (prettify-checker-unq (cadr op)))
	  (case op
	    ((rational?) "rational")
	    ((real?) "real")
	    ((complex?) "complex")
	    ((null?) "null")
	    ((length) "a sequence")
	    ((unspecified?) "untyped")
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
	      ((unspecified?) "untyped")
	      (else (string-append (if (memv (op-name 0) '(#\a #\e #\i #\o #\u)) "an " "a ") op-name))))))
    
    (define (unused-parameter? x) #t)
    (define (unused-set-parameter? x) #t)

    
    (define (check-args caller head form checkers env max-arity)
      ;; check for obvious argument type problems
      ;; caller = overall caller, head = current caller, checkers = proc or list of procs for checking args
      
      (define (check-checker checker at-end)
	(if (eq? checker 'integer:real?)
	    (if at-end 'real? 'integer?)
	    (if (eq? checker 'integer:any?)
		(or at-end 'integer?)
		checker)))
    
      (define (any-checker? types arg)
	(if (and (symbol? types)
		 (not (eq? types 'values)))
	    ((symbol->value types *e*) arg)
	    (and (pair? types)
		 (or (any-checker? (car types) arg)
		     (any-checker? (cdr types) arg)))))
    
      (define (every-compatible? type1 type2)
	(if (symbol? type1)
	    (if (symbol? type2)
		(compatible? type1 type2)
		(and (pair? type2)                   ; here everything has to match
		     (compatible? type1 (car type2))
		     (every-compatible? type1 (cdr type2))))
	    (and (pair? type1)                       ; here any match is good
		 (or (compatible? (car type1) type2)
		     (any-compatible? (cdr type1) type2)))))
    
      (define (report-arg-trouble caller form head arg-number checker arg uop)
	(define (prettify-arg-number argn)
	  (if (or (not (= argn 1))
		  (pair? (cddr form)))
	      (format #f "~D " argn)
	      ""))
    	(let ((op (if (and (eq? checker 'real?)
			   (eq? uop 'number?))
		      'complex?
		      uop)))
	  (if (and (or arg (not (eq? checker 'output-port?)))
		   (not (and (eq? checker 'string?)
			     (pair? arg)
			     (eq? (car arg) 'format)))) ; don't try to untangle the format non-string case
	      (if (and (pair? op)
		       (member checker op any-compatible?))
		  (if (not (var-member catch-marker env))
		      (lint-format "in ~A, ~A's argument ~Ashould be ~A, but ~A might also be ~A" caller
				   (truncated-list->string form) head 
				   (prettify-arg-number arg-number)
				   (prettify-checker-unq checker)
				   (truncated-list->string arg)
				   (car (remove-if (lambda (o) (any-compatible? checker o)) op))))
		  (lint-format "in ~A, ~A's argument ~Ashould be ~A, but ~A is ~A" caller
			       (truncated-list->string form) head 
			       (prettify-arg-number arg-number)
			       (prettify-checker-unq checker)
			       (truncated-list->string arg)
			       (prettify-checker op))))))


      (if *report-func-as-arg-arity-mismatch*
	  (let ((v (var-member head env)))
	    (if (and (var? v)
		     (memq (var-ftype v) '(define define* lambda lambda*))
		     (zero? (var-set v)) ; perhaps this needs to wait for report-usage?
		     (pair? (var-arglist v)))
		(let ((source (var-initial-value v)))
		  (when (and (pair? source)
			     (pair? (cdr source))
			     (pair? (cddr source)))
		    (let ((vhead (cddr source))
			  (head-arglist (var-arglist v))
			  (arg-number 1))
		      
		      (if (pair? vhead)
			  (for-each 
			   (lambda (arg)
			     ;; (format *stderr* "~A: ~A ~A --------~%" form vhead arg)
			     
			     ;; only check func if head is var-member and has procedure-source (var-[initial-]value?)
			     ;;   and arg has known arity, and check only if arg(par) is car, not (for example) cadr of apply
			     
			     (let ((ari (if (symbol? arg)
					    (arg-arity arg env)
					    (and (pair? arg)
						 (eq? (car arg) 'lambda)
						 (let ((len (length (cadr arg))))
						   (and (integer? len)
							(cons (abs len)
							      (if (negative? len) 500000 len)))))))
				   (par (and (> (length head-arglist) (- arg-number 1))
					     (list-ref head-arglist (- arg-number 1)))))
			       ;; (format *stderr* "  ~A ari: ~A ~A~%" arg ari par)
			       (if (and (symbol? par)
					(pair? ari)
					(or (> (car ari) 0)
					    (< (cdr ari) 20)))
				   
				   ;; fwalk below needs to be smart about tree walking so that
				   ;;   it does not confuse (c) in (lambda (c)...) with a call on the function c.
				   ;; check only if current parameter name is not shadowed

				   (let fwalk ((sym par) (tree vhead))
				     ;; (format *stderr* "    ~A: ~A~%" tree sym)
				     (if (pair? tree)
					 (if (eq? (car tree) sym)
					     (let ((args (length (cdr tree))))
					       ;; (format *stderr* "  ~A in ~A: ~A~%" args ari tree)
					       (if (> (car ari) args)
						   (lint-format "~A's parameter ~A is passed ~A and called ~A, but ~A needs ~A argument~P" caller
								head par 
								(truncated-list->string arg)
								(truncated-list->string tree)
								(truncated-list->string arg)
								(car ari) (car ari))
						   (if (> args (cdr ari))
						       (lint-format "~A's parameter ~A is passed ~A and called ~A, but ~A takes only ~A argument~P" caller
								    head par 
								    (truncated-list->string arg)
								    (truncated-list->string tree)
								    (truncated-list->string arg)
								    (cdr ari) (cdr ari)))))
					     (case (car tree)
					       ((let let*)
						(if (and (pair? (cdr tree))
							 (pair? (cddr tree)))
						    (let ((vs (if (symbol? (cadr tree)) (caddr tree) (cadr tree))))
						      (if (not (any? (lambda (a) (or (not (pair? a)) (eq? sym (car a)))) vs))
							  (fwalk sym (if (symbol? (cadr tree)) (cdddr tree) (cddr tree)))))))
					       
					       ((do letrec letrec*)
						(if (and (pair? (cdr tree))
							 (pair? (cddr tree))
							 (not (any? (lambda (a) (or (not (pair? a)) (eq? sym (car a)))) (cadr tree))))
						    (fwalk sym (cddr tree))))
					       
					       ((lambda lambda*)
						(if (and (pair? (cdr tree))
							 (pair? (cddr tree))
							 (not (any? (lambda (a) (eq? sym a)) (args->proper-list (cadr tree)))))
						    (fwalk sym (cddr tree))))
					       
					       ((define define-constant)
						(if (and (not (eq? sym (cadr tree)))
							 (pair? (cadr tree))
							 (not (any? (lambda (a) (eq? sym a)) (args->proper-list (cdadr tree)))))
						    (fwalk sym (cddr tree))))
					       
					       ((define* define-macro define-macro* define-expansion define-bacro define-bacro*)
						(if (and (pair? (cdr tree))
							 (pair? (cddr tree))
							 (not (any? (lambda (a) (eq? sym a)) (args->proper-list (cdadr tree)))))
						    (fwalk sym (cddr tree))))
					       
					       ((quote) #f)
					       
					       ((case)
						(if (and (pair? (cdr tree))
							 (pair? (cddr tree)))
						    (for-each (lambda (c) (fwalk sym (cdr c))) (cddr tree))))
					       
					       (else 
						(if (pair? (car tree))
						    (fwalk sym (car tree)))
						(if (pair? (cdr tree))
						    (for-each (lambda (p) (fwalk sym p)) (cdr tree))))))))))
			     
			     (set! arg-number (+ arg-number 1)))
			   (cdr form)))))))))
      
      (when (pair? checkers)
      (let ((arg-number 1)
	    (flen (length (cdr form))))

	(call-with-exit
	 (lambda (done)
	   (for-each 
	    (lambda (arg)
	      (let ((checker (check-checker (if (pair? checkers) (car checkers) checkers) (= arg-number flen))))

		(define (check-arg expr)
		  (unless (symbol? expr)
		    (let ((op (->type expr)))
		      (if (not (or (memq op '(#f #t values))
				   (every-compatible? checker op)))
			  (report-arg-trouble caller form head arg-number checker expr op)))))

		;; special case checker?
		(if (and (symbol? checker)
			 (not (memq checker '(unused-parameter? unused-set-parameter?)))
			 (not (hash-table-ref built-in-functions checker)))
		    (let ((chk (symbol->value checker)))
		      (if (and (procedure? chk)
			       (equal? (arity chk) '(2 . 2)))
			  (catch #t
			    (lambda ()
			      (let ((res (chk form arg-number)))
				(set! checker #t)
				(if (symbol? res)
				    (set! checker res)
				    (if (string? res)
					(lint-format "~A's argument, ~A, should be ~A" caller head arg res)))))
			    (lambda (type info)
			      (set! checker #t))))))

		(if (and (pair? arg)
			 (pair? (car arg)))
		    (let ((rtn (return-type (caar arg) env)))
		      (if (memq rtn '(boolean? real? integer? rational? number? complex? float? pair? keyword? symbol? null? char?))
			  (lint-format "~A's argument ~A looks odd: ~A returns ~A which is not applicable"
				       caller head arg (caar arg) rtn))))

		(when (or (pair? checker)
			  (symbol? checker)) ; otherwise ignore type check on this argument (#t -> anything goes)
		  (if arg
		      (if (eq? checker 'unused-parameter?)
			  (lint-format "~A's parameter ~A is not used, but a value is passed: ~S" caller head arg-number arg)
			  (if (eq? checker 'unused-set-parameter?)
			      (lint-format "~A's parameter ~A's value is not used, but a value is passed: ~S" caller head arg-number arg))))
		  
		  (if (not (pair? arg))                  ; arg is expr -- try to guess its type
		      (if (not (or (symbol? arg)
				   (any-checker? checker arg)))
			      (let ((op (->type arg)))
				(unless (memq op '(#f #t values))
				  (report-arg-trouble caller form head arg-number checker arg op))))
		      ;; arg is a pair
		      (case (car arg) 
			((quote)   ; '1 -> 1
			 (let ((op (if (pair? (cadr arg)) 'list? (->type (cadr arg)))))
			   ;; arg is quoted expression
			   (if (not (or (memq op '(#f #t values))
					(every-compatible? checker op)))
			       (report-arg-trouble caller form head arg-number checker arg op))))
			  
			;; arg is an expression
			((begin let let* letrec letrec* with-let)
			 (check-arg (and (pair? (cdr arg))
					 (list-ref arg (- (length arg) 1)))))
			
			((if)
			 (if (and (pair? (cdr arg))
				  (pair? (cddr arg)))
			     (let ((t (caddr arg))
				   (f (if (pair? (cdddr arg)) (cadddr arg))))
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
				     (pair? (cdr clause))
				     (not (eq? (cadr clause) '=>)))
				(check-arg (list-ref clause (- (length clause) 1)))))
			  (cddr arg)))
			
			((cond)
			 (for-each
			  (lambda (clause)
			    (if (pair? clause)
				(if (pair? (cdr clause))
				    (if (not (eq? (cadr clause) '=>))
					(check-arg (list-ref clause (- (length clause) 1))))
				    (check-arg (car clause)))))
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
				      (report-arg-trouble caller form head arg-number checker arg 'unspecified?)))
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
			   (let ((v (var-member (car arg) env)))
			     (if (and (var? v)
				      (not (memq form (var-history v))))
				 (set! (var-history v) (cons form (var-history v)))))

			   ;; checker is arg-type, op is expression type (can also be a pair)
			   (if (and (not (memq op '(#f #t values)))
				    (not (memq checker '(unused-parameter? unused-set-parameter?)))
				    (or (not (every-compatible? checker op))
					(and (just-constants? arg env) ; try to eval the arg
					     (catch #t 
					       (lambda ()
						 (not (any-checker? checker (eval arg))))
					       (lambda ignore-catch-error-args
						 #f)))))
			       (report-arg-trouble caller form head arg-number checker arg op)))))))
		
		(if (list? checkers)
		    (if (null? (cdr checkers))
			(done)
			(set! checkers (cdr checkers))))
		(set! arg-number (+ arg-number 1))
		(if (> arg-number max-arity) (done))))
	    (cdr form)))))))

    
    (define (check-call caller head form env)
      (let ((data (var-member head env)))
	;; (format *stderr* "~A call: ~A~%" form fdata)
	(if (var? data)
	    (let ((fdata (cdr data)))
	      ;; a local var
	      (if (symbol? (fdata 'ftype))
		  (let ((args (fdata 'arglist))
			(ary (and (not (eq? (fdata 'decl) 'error))
				  (arity (fdata 'decl))))
			(sig (var-signature data)))
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
				(begin
				  (for-each (lambda (p)
					      (if (pair? p)
						  (let ((v (var-member (car p) env)))
						    (if (var? v)
							(let ((vals (let-ref (cdr v) 'values)))
							  (if (pair? vals)
							      (set! call-args (+ call-args -1 (cadr vals)))))))))
					    (cdr form))
				  (if (< call-args req)
				      (lint-format "~A needs ~D argument~A: ~A" 
						   caller head 
						   req (if (> req 1) "s" "") 
						   (truncated-list->string form))))
				(if (> (- call-args (keywords (cdr form))) opt) ; multiple-values can make this worse, (values)=nothing doesn't apply here
				    (lint-format "~A has too many arguments: ~A" caller head (truncated-list->string form)))))
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
					   (lint-format "~A keyword argument ~A (in ~A) does not match any argument in ~S" caller 
							head arg (truncated-list->string form) pargs))
				       (if (memq arg rest)
					   (lint-format "~W is repeated in ~A" caller arg (cdr form)))
				       (set! last-was-key #t))
				     (begin
				       (when (and (positive? have-keys)
						  (not last-was-key)
						  (not warned))
					 (set! warned #t)
					 (lint-format "non-keyword argument ~A follows previous keyword~P" caller arg have-keys))
				       (set! last-was-key #f)))
				 (if (pair? rest)
				     (set! rest (cdr rest))))
			       (cdr form))))
			  
			  (check-args caller head form (if (pair? sig) (cdr sig) ()) env opt)

			  ;; for a complete var-history, we could run through the args here even if no type info
			  ;; also if var passed to macro -- what to do?
			  
			  ;; look for problematic macro expansion
			  (when (memq (fdata 'ftype) '(define-macro define-macro* defmacro defmacro*))
			    
			    (unless (list? (fdata 'macro-ops))
			      (let ((syms (list () ())))
				(if (memq (fdata 'ftype) '(define-macro define-macro*))
				    (tree-symbol-walk (cddr (fdata 'initial-value)) syms)
				    (tree-symbol-walk (cdddr (fdata 'initial-value)) syms))
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
					       caller 
					       (truncated-list->string form)
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
				     caller head 
				     (if (= min-arity max-arity) "" "at least ")
				     min-arity
				     (if (> min-arity 1) "s" "") 
				     (truncated-list->string form))
			(if (and (not (procedure-setter head-value))
				 (> (- args (keywords (cdr form))) max-arity))
			    (lint-format "~A has too many arguments: ~A" caller head (truncated-list->string form))))
		    (if (and (procedure? head-value)
			     (pair? (cdr form))) ; there are args (the not-enough-args case is checked above)
			(if (zero? max-arity)
			    (lint-format "too many arguments: ~A" caller (truncated-list->string form))    
			    (begin
			      
			      (for-each (lambda (arg)
					  (if (pair? arg)
					      (if (negative? (length arg))
						  (lint-format "missing quote? ~A in ~A" caller arg form)
						  (if (eq? (car arg) 'unquote)
						      (lint-format "stray comma? ~A in ~A" caller arg form)))))
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
						 (lint-format "~A keyword argument ~A (in ~A) does not match any argument in ~S" caller 
							      head arg (truncated-list->string form) decls)))
					   (cdr form))))))
			      
			      ;; we've already checked for head in the current env above
			      (if (and (or (memq head '(eq? eqv?))
					   (and (= (length form) 3)
						(hash-table-ref repeated-args-table head)))
				       (repeated-member? (cdr form) env))
				  (lint-format "this looks odd: ~A"
					       caller
					       ;; sigh (= a a) could be used to check for non-finite numbers, I suppose,
					       ;;   and (/ 0 0) might be deliberate (as in gmp)
					       ;;   also (min (random x) (random x)) is not pointless
					       (truncated-list->string form))
				  (if (and (hash-table-ref repeated-args-table-2 head)
					   (repeated-member? (cdr form) env))
				      (lint-format "it looks odd to have repeated arguments in ~A" caller (truncated-list->string form))))
			      
			      (when (memq head '(eq? eqv?))
				(define (repeated-member-with-not? lst env)
				  (and (pair? lst)
				       (or (and (not (and (pair? (car lst))
							  (side-effect? (car lst) env)))
						(or (member (list 'not (car lst)) (cdr lst))
						    (and (pair? (car lst))
							 (eq? (caar lst) 'not)
							 (= (length (car lst)) 2)
							 (member (cadar lst) (cdr lst)))))
					   (repeated-member-with-not? (cdr lst) env))))
				(if (repeated-member-with-not? (cdr form) env)
				    (lint-format "this looks odd: ~A" caller (truncated-list->string form))))
			      
			      ;; now try to check arg types 
			      (let ((func (symbol->value head *e*)))
				(let ((arg-data (let ((sig (procedure-signature func)))
						  (and (pair? sig)
						       (cdr sig)))))
				  (if (pair? arg-data)
				      (check-args caller head form arg-data env max-arity))
				  ))))))))))))
    
    (define (get-generator caller form env)
      (let ((name (if (pair? (cadr form))
		      (caadr form)
		      (cadr form))))

	(if (and (pair? (cadr form))
		 (pair? (cdadr form)))
	    (lint-walk caller (cdadr form) env))

	(let ((gen? (string->symbol (string-append (symbol->string name) "?")))
	      (gen-make (string->symbol (string-append "make-" (symbol->string name)))))
	  (list (make-fvar :name gen?
			   :ftype 'define
			   :decl (dummy-func 'define `(define (,gen? x) (let? x)) '(define (_ x) #f))
			   :initial-value `(define (,gen? x) (let? x))
			   :arglist (list 'x)
			   :env env)
		(make-fvar :name gen-make
			   :ftype 'define*
			   :decl (dummy-func 'define* `(define* (,gen-make :rest x :allow-other-keys) (apply inlet x)) '(define (_ . x) #f))
			   :initial-value `(define* (,gen-make :rest x :allow-other-keys) (apply inlet x))
			   :arglist (list :rest 'x :allow-other-keys)
			   :env env)))))
    
    (define (last-par x)
      (let ((len (length x)))
	(and (positive? len)
	     (x (- len 1)))))
    
    (define (binding-ok? caller head binding env second-pass)
      ;; check let-style variable binding for various syntactic problems
      (cond (second-pass
	     (and (pair? binding)
		  (symbol? (car binding))
					;(not (keyword? (car binding)))
		  (not (constant? (car binding)))
		  (pair? (cdr binding))
		  (or (null? (cddr binding))
		      (and (eq? head 'do)
			   (pair? (cddr binding)) ; (do ((i 0 . 1))...)
			   (null? (cdddr binding))))))
	  
	    ((not (pair? binding)) 	   (lint-format "~A binding is not a list? ~S" caller head binding) #f)
	    ((not (symbol? (car binding))) (lint-format "~A variable is not a symbol? ~S" caller head binding) #f)
	    ((keyword? (car binding))	   (lint-format "~A variable is a keyword? ~S" caller head binding) #f)
	    ((constant? (car binding))	   (lint-format "can't bind a constant: ~S" caller binding) #f)
	    ((not (pair? (cdr binding)))
	     (lint-format (if (null? (cdr binding))
			      "~A variable value is missing? ~S" 
			      "~A binding is an improper list? ~S")
			  caller head binding)
	     #f)
	    ((and (pair? (cddr binding))
		  (or (not (eq? head 'do))
		      (pair? (cdddr binding))))
	     (lint-format "~A binding is messed up: ~A" caller head binding)
	     #f)
	    (else 
	     (if (and *report-shadowed-variables*
		      (var-member (car binding) env))
		 (lint-format "~A variable ~A in ~S shadows an earlier declaration" caller head (car binding) binding))
	     #t)))

    (define (env-difference name e1 e2 lst)
      (if (or (null? e1)
	      (null? e2)
	      (eq? (car e1) (car e2)))
	  (reverse lst)
	  (env-difference name (cdr e1) e2 
			  (if (eq? name (var-name (car e1)))
			      lst
			      (cons (car e1) lst)))))
    
    (define (report-usage caller head vars env)
      ;; report unused or set-but-unreferenced variables, then look at the overall history
    
      (define (all-types-agree v)
	(let ((base-type (->type (var-initial-value v)))
	      (vname (var-name v)))
	  (and (every? (lambda (p)
			 (or (not (and (pair? p)
				       (eq? (car p) 'set!)
				       (eq? vname (cadr p))))
			      (let ((nt (->type (caddr p))))
				(or (subsumes? base-type nt)
				    (and (subsumes? nt base-type)
					 (set! base-type nt))
				    (and (memq nt '(pair? null? proper-list?))
					 (memq base-type '(pair? null? proper-list?))
					 (set! base-type 'list?))))))
		       (var-history v))
	       base-type)))

      (define (indirect-set? vname func arg1)
	(or (and (memq func '(set-car! set-cdr! vector-set! list-set! string-set!))
		 (eq? arg1 vname))
	    (and (eq? func 'set!) ; implicit index presumably
		 (pair? arg1)
		 (eq? (car arg1) vname))))

      (if (and (not (eq? head 'begin)) ; begin can redefine = set a variable
	       (pair? vars)
	       (proper-list? vars))
	  (do ((cur vars (cdr cur))
	       (rst (cdr vars) (cdr rst)))
	      ((null? rst))
	    (let ((vn (var-name (car cur))))
	      (if (not (eq? vn lambda-marker))
		  (let ((repeat (var-member vn rst)))
		    (when repeat
		      (let ((type (if (eq? (var-definer repeat) 'parameter) 'parameter 'variable)))
			(if (eq? (var-definer (car cur)) 'define)
			    (lint-format "~A ~A ~A is redefined in the ~A body.  Perhaps use set! instead: ~A" caller
					 head type vn head
					 (truncated-list->string `(set! ,vn ,(var-initial-value (car cur)))))
			    (lint-format "~A ~A ~A is declared twice" caller 
					 head type vn)))))))))
      (for-each 
       (lambda (arg)
	 (let ((vname (var-name arg))
	       (type (if (eq? (var-definer arg) 'parameter) 'parameter 'variable)))
	   
	   (if (hash-table-ref syntaces vname)
	       (lint-format "~A ~A named ~A is asking for trouble" caller head type vname)
	       (if (not (symbol? vname))
		   (lint-format "bad ~A ~A name: ~S in ~S" caller head type vname arg)))
	   
	   (unless (eq? vname lambda-marker)

	     (when (and *report-function-stuff*
			(memq (var-ftype arg) '(define lambda define* lambda*)))
	       (let ((scope (var-scope arg))) ; might be #<undefined>?
		 (if (pair? scope) (set! scope (remove vname scope)))
		 (when (and (pair? scope)
			    (null? (cdr scope))
			    (symbol? (car scope))
			    (not (var-member (car scope) env)))
		   (format outport "~NC~A is called only in ~A~%" lint-left-margin #\space vname (car scope))))
	       (if (pair? (caddr (var-initial-value arg)))
	       (let ((cur (hash-table-ref equable-closures (caaddr (var-initial-value arg)))))
		 (if (pair? cur)
		     (hash-table-set! equable-closures (caaddr (var-initial-value arg)) (remove arg cur)))
	       )))

	     ;; redundant vars are hard to find -- tons of false positives
	     
	     (if (zero? (var-ref arg))
		 (if (or *report-unused-parameters* 
			 (not (eq? type 'parameter)))
		     (if (positive? (var-set arg))
			 (let ((sets (map (lambda (call)
					    (if (and (pair? call)
						     (not (eq? (var-definer arg) 'do))
						     (eq? (car call) 'set!)
						     (eq? (cadr call) vname))
						call
						(values)))
					  (var-history arg))))
			   (if (pair? sets)
			       (if (null? (cdr sets))
				   (lint-format "~A set, but not used: ~A" caller 
						vname (truncated-list->string (car sets)))
				   (lint-format "~A set, but not used: ~{~S~^ ~}" caller 
						vname sets))
			       (lint-format "~A set, but not used: ~A from ~A" caller 
					    vname (truncated-list->string (var-initial-value arg)) (var-definer arg))))
			 
			 ;; not ref'd or set
			 (if (not (memq vname '(documentation signature iterator?)))
			     (let ((val (if (pair? (var-history arg)) (car (var-history arg)) (var-initial-value arg)))
				   (def (var-definer arg)))
			       (if (symbol? def)
				   (if (eq? type 'parameter)
				       (lint-format "~A not used" caller vname)
				       (lint-format "~A not used, initially: ~A from ~A" caller 
						    vname (truncated-list->string val) def))
				   (lint-format "~A not used, value: ~A" caller 
						vname (truncated-list->string val)))))))
		 
		 ;; not zero var-ref
		 (let ((arg-type #f))
		   (if (and (not (memq (var-definer arg) '(parameter named-let named-let*)))
			    (pair? (var-history arg))
			    (or (zero? (var-set arg))
				(set! arg-type (all-types-agree arg))))
		       (let ((type (or arg-type (->type (var-initial-value arg))))
			     (lit? (code-constant? (var-initial-value arg))))
			 
			 (do ((clause (var-history arg) (cdr clause)))
			     ((null? (cdr clause))) ; ignore the initial value which depends on a different env
			   (let ((call (car clause)))
			     (when (pair? call)
			       (let ((func (car call))
				     (arg1 (and (pair? (cdr call)) (cadr call))))
				 
				 ;; check for assignments into constants
				 (if (and lit?
					  (indirect-set? vname func arg1))
				     (lint-format "~A's value, ~A, is a literal constant, so this set! is trouble: ~A" caller 
						  vname (var-initial-value arg) (truncated-list->string call)))
				 
				 ;; check for incorrect types in function calls
				 (when (and (symbol? type)
					    (not (memq type '(boolean? null?)))) ; null? here avoids problems with macros that call set!
				   (let ((p (memq vname (cdr call))))                    
				     (when (pair? p)
				       (let ((sig (arg-signature func env))
					     (pos (- (length call) (length p))))
					 (when (and (pair? sig)
						    (< pos (length sig)))
					   (let ((desired-type (list-ref sig pos)))
					     (if (not (compatible? type desired-type))
						 (lint-format "~A is ~A, but ~A in ~A wants ~A" caller
							      vname (prettify-checker-unq type)
							      func (truncated-list->string call) 
							      (prettify-checker desired-type))))))))
				   
				   (let ((suggest made-suggestion))
				     ;; check for pointless type checks
				     (when (and (hash-table-ref bools func)
						(not (eq? vname func)))
				       
				       (when (or (eq? type func)
						 (and (compatible? type func)
						      (not (subsumes? type func))))
					 (lint-format "~A is ~A, so ~A is #t" caller vname (prettify-checker-unq type) call))
				       
				       (unless (compatible? type func)
					 (lint-format "~A is ~A, so ~A is #f" caller vname (prettify-checker-unq type) call)))
				     
				     ;; (and (pair? x) (eq? x #\a)) etc
				     (when (and (memq func '(eq? eqv? equal?))
						(or (and (code-constant? arg1)
							 (not (compatible? type (->type arg1))))
						    (and (code-constant? (caddr call))
							 (not (compatible? type (->type (caddr call)))))))
				       (lint-format "~A is ~A, so ~A is #f" caller vname (prettify-checker-unq type) call))
				     
				     ;; the usual eqx confusion
				     (when (and (= suggest made-suggestion)
						(memq type '(char? number? integer? real? float? rational? complex?)))
				       (if (memq func '(eq? equal?))
					   (lint-format "~A is ~A, so ~A ~A be eqv? in ~A" caller 
							vname (prettify-checker-unq type) func
							(if (eq? func 'eq?) "should" "could")
							call))
				       ;; check other boolean exprs
				       (when (and (zero? (var-set arg))
						  (number? (var-initial-value arg))
						  (eq? vname arg1)
						  (null? (cddr call))
						  (hash-table-ref bools1 func))
					 (let ((val (catch #t 
						      (lambda ()
							((symbol->value func (rootlet)) (var-initial-value arg)))
						      (lambda args 
							'error))))
					   (if (boolean? val)
					       (lint-format "~A is ~A, so ~A is ~A~%" caller vname (var-initial-value arg) call val))))))
				   
				   ;; implicit index checks -- these are easily fooled by macros
				   (when (and (memq type '(vector? float-vector? int-vector? string? list? byte-vector?))
					      (pair? (cdr call)))
				     (when (eq? func vname)
				       (let ((init (var-initial-value arg)))
					 (if (not (compatible? 'integer? (->type arg1)))
					     (lint-format "~A is ~A, but the index ~A is ~A" caller
							  vname (prettify-checker-unq type)
							  arg1 (prettify-checker (->type arg1))))
					 
					 (if (integer? arg1)
					     (if (negative? arg1)
						 (lint-format "~A's index ~A is negative" caller vname arg1)
						 (if (zero? (var-set arg))
						     (let ((lim (cond ((code-constant? init)
								       (length init))

								      ((memq (car init) '(vector float-vector int-vector string list byte-vector))
								       (length (cdr init)))

								      (else
								       (and (pair? (cdr init))
									    (integer? (cadr init))
									    (memq (car init) '(make-vector make-float-vector make-int-vector 
											       make-string make-list make-byte-vector))
									    (cadr init))))))
						       (if (and lim (>= arg1 lim))
							   (lint-format "~A has length ~A, but index is ~A" caller vname lim arg1))))))))
				     
				     (when (eq? func 'implicit-set)
				       ;; ref is already checked in other history entries
				       (let ((ref-type (case type
							 ((float-vector?) 'real?) ; not 'float? because ints are ok here
							 ((int-vector? byte-vector?) 'integer)
							 ((string?) 'char?)
							 (else #f))))
					 (if ref-type
					     (let ((val-type (->type (caddr call))))
					       (if (not (compatible? val-type ref-type))
						   (lint-format "~A wants ~A, but the value in ~A is ~A" caller
								vname (prettify-checker-unq ref-type)
								`(set! ,@(cdr call)) 
								(prettify-checker val-type)))))
					 )))))
			       ))) ; do loop through clauses
			 
			 ;; check for duplicated calls involving arg
			 (if (and (> (var-ref arg) 8)
				  (zero? (var-set arg))
				  (eq? (var-ftype arg) #<undefined>))
			     (let ((h (make-hash-table)))
			       (for-each (lambda (call)
					   (if (and (pair? call)
						    (not (eq? (car call) vname)) ; ignore functions for now
						    (not (side-effect? call env)))
					       (hash-table-set! h call (+ 1 (or (hash-table-ref h call) 0)))))
					 (var-history arg))
			       (let ((repeats ()))
				 (for-each (lambda (call)
					     (if (and (> (cdr call) 5)
						      (not (memq (caar call) '(make-vector make-float-vector)))
						      (or (null? (cddar call))
							  (every? (lambda (p)
								    (or (not (symbol? p))
									(eq? p vname)))
								  (cdar call))))
						 (set! repeats (cons (truncated-list->string (car call)) (cons (cdr call) repeats)))))
					   h)
				 (if (pair? repeats)
				     (lint-format "~A is not set, but ~{~A occurs ~A times~^, ~}" caller
						  vname repeats)))))
			 
			 ;; check for function parameters whose values never change and are not just symbols
			 (if (and (> (var-ref arg) 3)
				  (zero? (var-set arg))
				  (memq (var-ftype arg) '(define lambda))
				  (pair? (var-arglist arg)))
			     (let ((pars (map list (proper-list (var-arglist arg)))))
			       (do ((clauses (var-history arg) (cdr clauses)))
				   ((null? (cdr clauses))) ; ignore the initial value
				 (if (and (pair? (car clauses))
					  (eq? (caar clauses) (var-name arg)))
				     (for-each (lambda (arg par)
						 (if (not (member arg (cdr par)))
						     (set-cdr! par (cons arg (cdr par)))))
					       (cdar clauses)
					       pars)))
			       (for-each (lambda (p)
					   (if (and (pair? (cdr p))
						    (null? (cddr p))
						    (not (symbol? (cadr p))))
					       (lint-format "~A's '~A parameter is always ~S (~D calls)~%" caller
							    (var-name arg) (car p) (cadr p) (var-ref arg))))
					 pars)))
			 )))) ; end (if zero var-ref)

	     ;; vars with multiple incompatible ascertainable types don't happen much and obvious type errors are extremely rare
	     )))
       vars))
    
    ;; preloading built-in definitions, and looking for them here found less than a dozen (list-ref, list-tail, and boolean?)

    (define (code-equal? l1 l2 matches e1 e2)
      ;; (format *stderr* "equal: ~A ~A~%" l1 l2)

      (define (match-vars r1 r2 mat)
	(and (pair? r1)
	     (pair? r2)
	     (pair? (cdr r1))
	     (pair? (cdr r2))
	     (if (and (pair? (cadr r1))
		      (pair? (cadr r2))
		      (memq (caadr r1) '(let let* letrec letrec* do lambda lambda* 
					 define define-constant define-macro define-bacro define-expansion
					 define* define-macro* define-bacro*)))
		 (code-equal? (cadr r1) (cadr r2) mat e1 e2)
		 (structures-equal? (cadr r1) (cadr r2) mat e1 e2))
	     (cons (car r1) (car r2))))
	
      (let ((f1 (car l1))
	    (f2 (car l2)))
	(and (eq? f1 f2)
	     (let ((rest1 (cdr l1))
		   (rest2 (cdr l2)))
	       (and (pair? rest1)
		    (pair? rest2)
		    (call-with-exit
		     (lambda (return)
		       (case f1
			 ((let)
			  (let ((name ()))
			    (if (symbol? (car rest1))                      ; named let -- match funcs too
				(if (symbol? (car rest2))
				    (begin
				      (set! name (list (cons (car rest1) (car rest2))))
				      (set! rest1 (cdr rest1))
				      (set! rest2 (cdr rest2)))
				    (return #f))
				(if (symbol? (car rest2))
				    (return #f)))
			    (and (= (length (car rest1)) (length (car rest2)))
				 (structures-equal? (cdr rest1) (cdr rest2)     ; refs in values are to outer matches
						    (append (map (lambda (var1 var2)
								   (or (match-vars var1 var2 matches)
								       (return #f)))
								 (car rest1)
								 (car rest2))
							    name           ; append will splice out nil
							    matches)
						    e1 e2))))
			 ((let*)                                           ; refs move with the vars 
			  (and (= (length (car rest1)) (length (car rest2)))
			       (let ((new-matches matches))
				 (for-each (lambda (var1 var2)
					     (cond ((match-vars var1 var2 new-matches) =>
						    (lambda (v)
						      (set! new-matches (cons v new-matches))))
						   (else (return #f))))
					   (car rest1)
					   (car rest2))
				 (structures-equal? (cdr rest1) (cdr rest2) new-matches e1 e2))))
			 
			 ((do)                                             ; matches at init are outer, but at step are inner
			  (and (= (length (car rest1)) (length (car rest2)))
			       (let ((new-matches matches))
				 (for-each (lambda (var1 var2)
					     (cond ((match-vars var1 var2 matches) =>
						    (lambda (v)
						      (set! new-matches (cons v new-matches))))
						   (else (return #f))))
					   (car rest1)
					   (car rest2))
				 (for-each (lambda (var1 var2)
					     (unless (structures-equal? (cddr var1) (cddr var2) new-matches e1 e2)
					       (return #f)))
					   (car rest1)
					   (car rest2))
				 (structures-equal? (cdr rest1) (cdr rest2) new-matches e1 e2))))
			 
			 ((letrec letrec*)                                ; ??? refs are local I think
			  (and (= (length (car rest1)) (length (car rest2)))
			       (let ((new-matches (append (map (lambda (var1 var2)
								 (cons (car var1) (car var2)))
							       (car rest1)
							       (car rest2))
							  matches)))
				 (for-each (lambda (var1 var2)
					     (unless (structures-equal? (cadr var1) (cadr var2) new-matches e1 e2)
					       (return #f)))
					   (car rest1)
					   (car rest2))
				 (structures-equal? (cdr rest1) (cdr rest2) new-matches e1 e2))))
			 
			 ((lambda) 
			  (if (symbol? (car rest1))
			      (and (symbol? (car rest2))
				   (structures-equal? (cdr rest1) (cdr rest2)
						      (cons (cons (car rest1) (car rest2)) matches) e1 e2))
			      (and (equal? (length (car rest1)) (length (car rest2))) ; (car rest2) might be a symbol, dotted lists ok here
				   (structures-equal? (cdr rest1) (cdr rest2)
						      (append (map cons (proper-list (car rest1)) (proper-list (car rest2)))
							      matches)
						      e1 e2))))
			 ((define define-constant define-macro define-bacro define-expansion)
			  (if (symbol? (car rest1))
			      (and (symbol? (car rest2))
				   (let ((new-matches (cons (cons (car rest1) (car rest2)) matches)))
				     (and (structures-equal? (cdr rest1) (cdr rest2) new-matches e1 e2)
					  new-matches)))
			      (and (equal? (length (car rest1)) (length (car rest2))) ; (car rest2) might be a symbol, dotted lists ok here
				   (structures-equal? (cdr rest1) (cdr rest2)
						      (append (map cons (proper-list (car rest1)) (proper-list (car rest2)))
							      matches)
						      e1 e2)
				   (cons (cons (caar rest1) (caar rest2)) matches))))
			 ;; for define we add the new name to matches before walking the body (shadow is immediate),
			 ;;   but then the new name is added to matches and returned (see below)
			 
			 ((lambda*)
			  (if (symbol? (car rest1))
			      (and (symbol? (car rest2))
				   (structures-equal? (cdr rest1) (cdr rest2)
						      (cons (cons (car rest1) (car rest2)) matches)
						      e1 e2))
			      (and (equal? (length (car rest1)) (length (car rest2))) ; (car rest2) might be a symbol, dotted lists ok here
				   (structures-equal? (cdr rest1) (cdr rest2)
						      (append (map (lambda (a b)
								     (if (or (pair? a)  ; if default, both must have the same value
									     (pair? b))
									 (if (not (and (pair? a)
										       (pair? b)
										       (equal? (cadr a) (cadr b))))
									     (return #f)
									     (cons (car a) (car b)))
									 (cons a b)))
								   (proper-list (car rest1)) (proper-list (car rest2)))
							      matches)
						      e1 e2))))
			 
			 ((define* define-macro* define-bacro*)
			  ;; (format *stderr* "~A ~A~%" rest1 rest2)
			  (if (symbol? (car rest1))
			      (and (symbol? (car rest2))
				   (let ((new-matches (cons (cons (car rest1) (car rest2)) matches)))
				     (and (structures-equal? (cdr rest1) (cdr rest2) new-matches e1 e2)
					  new-matches)))
			      (and (equal? (length (car rest1)) (length (car rest2))) ; (car rest2) might be a symbol, dotted lists ok here
				   (structures-equal? (cdr rest1) (cdr rest2)
						      (append (map (lambda (a b)
								     (if (or (pair? a)  ; if default, both must have the same value
									     (pair? b))
									 (if (not (and (pair? a)
										       (pair? b)
										       (equal? (cadr a) (cadr b))))
									     (return #f)
									     (cons (car a) (car b)))
									 (cons a b)))
								   (proper-list (car rest1)) (proper-list (car rest2)))
							      matches)
						      e1 e2)
				   (cons (cons (caar rest1) (caar rest2)) matches))))
			 
			 (else #f))))))))) ; can't happen I hope

    (define (structures-equal? l1 l2 matches e1 e2)
      ;; (format *stderr* "    equal ~A ~A~%" l1 l2)
      (if (pair? l1)
	  (if (eq? (car l1) 'quote)
	      (and (pair? l2)
		   (eq? (car l2) 'quote)
		   (equal? (cdr l1) (cdr l2)))
	      (and (pair? l2)
		   (cond ((not (and (pair? (car l1))
				    (pair? (car l2))))
			  (structures-equal? (car l1) (car l2) matches e1 e2))

			 ((memq (caar l1) '(let let* letrec letrec* do lambda lambda*))
			  (code-equal? (car l1) (car l2) matches e1 e2))

			 ((memq (caar l1) '(define define-constant define-macro define-bacro define-expansion define* define-macro* define-bacro*))
			  (let ((mat (code-equal? (car l1) (car l2) matches e1 e2)))
			    (and (pair? mat)
				 (set! matches mat))))

			 ;; this ignores possible reversible equivalence (i.e. (< x 0) is the same as (> 0 x)
			 ;;   (structures-equal? (car l1) (car l2) matches e1 e2)))

			 ;; check for reversible equivalence
			 ;;   half-humorous problem: infinite loop here switching back and forth!
			 ;;   so I guess we have to check cdar by hand
			 ;; we could also check for not+notable here, but lint will complain
			 ;;   about that elsewhere, causing this check to be ignored.
			 (else (or (structures-equal? (car l1) (car l2) matches e1 e2)
				   (and (eq? (hash-table-ref reversibles (caar l1)) (caar l2))
					(do ((a (cdar l1) (cdr a))
					     (b (reverse (cdar l2)) (cdr b)))
					    ((or (null? a)
						 (null? b)
						 (not (structures-equal? a b matches e1 e2)))
					     (and (null? a)
						  (null? b))))))))
		   (structures-equal? (cdr l1) (cdr l2) matches e1 e2)))
	  (let ((match (assq l1 matches)))
	    (if match
		(or (and (eq? (cdr match) :unset)
			 (set-cdr! match l2))
		    (equal? (cdr match) l2))
		(if (symbol? l1)
		    (and (eq? l1 l2)
			 (or (eq? e1 e2)
			     (eq? (assq l1 e1) (assq l2 e2))))
		    (equal? l1 l2))))))


    ;; code-equal? and structures-equal? called on through function-match and only in each other

    (define (function-match caller form env)
      ;; (format *stderr* "match ~A ~A~%" caller form)

      (define func-min-cutoff 6)
      (define func-max-cutoff 120)

      (define (proper-list* lst)
	;; return lst as a proper list (might have defaults, keywords etc)
	(if (or (not (pair? lst))
		(eq? (car lst) :allow-other-keys))
	    ()
	    (if (eq? (car lst) :rest)
		(cdr lst)
		(cons (if (pair? (car lst)) (caar lst) (car lst))
		      (if (pair? (cdr lst)) 
			  (proper-list* (cdr lst)) 
			  (if (null? (cdr lst)) 
			      () 
			      (list (cdr lst))))))))
      
      (let* ((leaves (tree-length form))
	     (cutoff (max func-min-cutoff (- leaves 12))))

	(when (<= func-min-cutoff leaves func-max-cutoff)
	  (let ((new-form (if (pair? (car form)) form (list form)))
		(name-args #f)
		(name-args-len :unset)
		(e1 ())
		(e2 ()))
	    
	    (let ((v (var-member caller env)))
	      (when (and (var? v)
			 (memq (var-ftype v) '(define lambda define* lambda*))
			 (or (eq? form (cddr (var-initial-value v)))    ; only check args if this is the complete body
			     (and (null? (cdddr (var-initial-value v))) 
				  (eq? form (caddr (var-initial-value v))))))
		(set! e2 (var-env v))
		(if (symbol? (var-arglist v))
		    (begin
		      (set! name-args-len #f)
		      (set! name-args (list (var-arglist v))))
		    (begin
		      (set! name-args-len (length (var-arglist v)))
		      (set! name-args (map (lambda (arg)
					     (if (symbol? arg)
						 arg
						 (values)))
					   (proper-list* (var-arglist v))))))))
	    
	    (define (find-code-match v)
	      (and (not (eq? (var-name v) lambda-marker))
		   (memq (var-ftype v) '(define lambda define* lambda*))
		   (not (eq? caller (var-name v)))
		   (let ((body (cddr (var-initial-value v)))
			 (args (var-arglist v)))
		     (set! e1 (var-env v))
		     
		     (let ((args-len (length args)))
		       (when (or (eq? name-args-len :unset)
				 (equal? args-len name-args-len)
				 (and (integer? args-len)
				      (integer? name-args-len)
				      (not (negative? (* args-len name-args-len)))))
			 
			 (unless (var-leaves v)
			   (set! (var-leaves v) (tree-length body))
			   (set! (var-match-list v) (if (symbol? args)
							(list (cons args :unset))
							(map (lambda (arg)
							       (if (symbol? arg)
								   (cons arg :unset)
								   (values)))
							     (proper-list* args)))))
			 
			 ;; var-leaves is size of func (v) body
			 ;; leaves is size of form which we want to match with func
			 ;; func-min-cutoff avoids millions of uninteresting matches
			 
			 (and (<= cutoff (var-leaves v) leaves)
			      (let ((match-list (do ((p (var-match-list v) (cdr p))) 
						    ((null? p) 
						     (var-match-list v))
						  (set-cdr! (car p) :unset))))
				(and (structures-equal? body new-form
							(cons (cons (var-name v) caller) match-list) e1 e2)
				     ;; if the functions are recursive, we also need those names matched, hence the extra entry
				     ;;   but we treat match-list below as just the args, so add the func names at the call,
				     ;;   but this can be fooled if we're playing games with eq? basically -- the function
				     ;;   names should only match if used as functions.
				     
				     (not (member :unset match-list (lambda (a b) (eq? (cdr b) :unset))))
				     (let ((new-args (map cdr match-list)))
				       (if (and (equal? new-args name-args)
						(equal? args-len name-args-len))
					   (lint-format "~A could be ~A" caller caller `(define ,caller ,(var-name v)))
					   (lint-format "perhaps ~A" caller (lists->string form `(,(var-name v) ,@new-args))))
				       #t)))))))))
	    
	    (do ((vs (or (hash-table-ref equable-closures (caar new-form)) ()) (cdr vs)))
		((or (null? vs)
		     (find-code-match (car vs)))))))))
    

    (define (unbegin x)
      (if (and (pair? x)
	       (eq? (car x) 'begin))
	  (cdr x)
	  (list x)))

    (define (check-returns caller f env)
      (if (not (side-effect? f env))
	  (lint-format "this could be omitted: ~A" caller (truncated-list->string f))
	  (when (pair? f)
	    (case (car f)
	      ((if)
	       (when (and (pair? (cdr f))
			  (pair? (cddr f)))
		 (let ((true (caddr f))
		       (false (if (pair? (cdddr f)) (cadddr f) 'no-false)))
		   (let ((true-ok (side-effect? true env))
			 (false-ok (or (eq? false 'no-false)
				       (side-effect? false env))))
		     (if true-ok
			 (if (pair? true)
			     (check-returns caller true env))
			 (lint-format "this branch is pointless~A: ~A in ~A" caller
				      (local-line-number true)
				      (truncated-list->string true)
				      (truncated-list->string f)))
		     (if false-ok
			 (if (pair? false)
			     (check-returns caller false env))
			 (lint-format "this branch is pointless~A: ~A in ~A" caller
				      (local-line-number false)
				      (truncated-list->string false)
				      (truncated-list->string f)))))))
	      ((cond case)
	       ;; here all but last result exprs are already checked
	       ;;   redundant begin can confuse this, but presumably we'll complain about that elsewhere
	       (for-each (lambda (c)
			   (if (and (pair? c)
				    (pair? (cdr c))
				    (not (memq '=> (cdr c))))
			       (let ((last-expr (list-ref (cdr c) (- (length (cdr c)) 1))))
				 (if (side-effect? last-expr env)
				     (if (pair? last-expr)
					 (check-returns caller last-expr env))
				     (if (eq? (car f) 'case)  ; here some sort of return is required (sigh)
					 (if (null? (cddr c)) ; just the return value
					     (if (not (memq last-expr '(#f #t ())))
						 (lint-format "this could be simply #f: ~A in ~A" caller
							      (truncated-list->string last-expr)
							      (truncated-list->string c)))
					     (lint-format "this could be omitted: ~A in ~A" caller
							  (truncated-list->string last-expr)
							  (truncated-list->string c)))
					 (lint-format "this is pointless: ~A in ~A" caller
						      (truncated-list->string last-expr)
						      (truncated-list->string c)))))))
			 (if (eq? (car f) 'cond) (cdr f) (cddr f))))

	      ((let let*)
	       (if (and (pair? (cdr f))
			(not (symbol? (cadr f)))
			(pair? (cddr f)))
		   (let ((last-expr (list-ref (cddr f) (- (length (cddr f)) 1))))
		     (if (side-effect? last-expr env)
			 (if (pair? last-expr)
			     (check-returns caller last-expr env))		 
			 (lint-format "this is pointless~A: ~A in ~A" caller
				      (local-line-number last-expr)
				      (truncated-list->string last-expr)
				      (truncated-list->string f))))))

	      ((letrec letrec* with-let unless when begin with-baffle)
	       (if (and (pair? (cdr f))
			(pair? (cddr f)))
		   (let ((last-expr (list-ref (cddr f) (- (length (cddr f)) 1))))
		     (if (side-effect? last-expr env)
			 (if (pair? last-expr)
			     (check-returns caller last-expr env))		 
			 (lint-format "this is pointless~A: ~A in ~A" caller
				      (local-line-number last-expr)
				      (truncated-list->string last-expr)
				      (truncated-list->string f))))))
	      ((do)
	       (let ((returned (if (and (pair? (cdr f))
					(pair? (cddr f)))
				   (let ((end+res (caddr f)))
				     (if (pair? (cdr end+res))
					 (list-ref (cdr end+res) (- (length end+res) 2)))))))
		 (if (or (eq? returned #<unspecified>)
			 (and (pair? returned)
			      (side-effect? returned env)))
		     (if (pair? returned)
			 (check-returns caller returned env))
		     (lint-format "~A: result ~A~A is not used" caller 
				  (truncated-list->string f) 
				  (truncated-list->string returned)
				  (local-line-number returned)))))
	      ((call-with-exit)
	       (if (and (pair? (cdr f))
			(pair? (cadr f))
			(eq? (caadr f) 'lambda)
			(pair? (cdadr f))
			(pair? (cadadr f)))
		   (let ((return (car (cadadr f))))
		     (let walk ((tree (cddadr f)))
		       (if (pair? tree)
			   (if (eq? (car tree) return)
			       (if (and (pair? (cdr tree))
					(or (not (boolean? (cadr tree)))
					    (pair? (cddr tree))))
				   (lint-format "th~A call-with-exit return value~A will be ignored: ~A" caller
						(if (pair? (cddr tree)) "ese" "is")
						(if (pair? (cddr tree)) "s" "")
						tree))
			       (for-each walk tree)))))))

	      ((map)
	       (if (pair? (cdr f))
		   (lint-format "map could be for-each: ~A" caller (truncated-list->string `(for-each ,@(cdr f))))))

	      ((reverse!)
	       (if (pair? (cdr f))
		   (lint-format "~A might leave ~A in an undefined state; perhaps ~A" caller (car f) (cadr f)
				`(set! ,(cadr f) ,f))))

	      ((format)
	       (if (and (pair? (cdr f))
			(eq? (cadr f) #t))
		   (lint-format "perhaps use () with format since the string value is discarded:~%    ~A" 
				caller `(format () ,@(cddr f)))))))))

    (define lint-current-form #f)
    (define lint-mid-form #f)

    (define (lint-walk-body caller head body env)
      ;; walk a body (a list of forms, the value of the last of which might be returned)
      ;; (format *stderr* "lint-walk-body ~A ~A ~A~%" caller head body)
      
      (if (not (proper-list? body))
	  (lint-format "stray dot? ~A" caller (truncated-list->string body))
	  
	  (let ((prev-f #f)
		(old-current-form lint-current-form)
		(old-mid-form lint-mid-form)
		(prev-len 0)
		(f-len 0)
		(repeats 0)
		(start-repeats body)
		(repeat-arg 0)
		(dpy-f #f)
		(dpy-start #f)
		(len (length body)))
	    (if (eq? head 'do) (set! len (+ len 1))) ; last form in do body is not returned

	    (when (and (pair? body)
		       *report-function-stuff*
		       (not (null? (cdr body))))
	      (function-match caller body env))
	    
	    (do ((fs body (cdr fs))
		 (ctr 0 (+ ctr 1)))
		((not (pair? fs)))
	      (let ((f (car fs)))

		(if (and (pair? prev-f)
			 (pair? f)
			 (eq? (car f) 'if)
			 (eq? (car prev-f) 'if)
			 (pair? (cdr f))
			 (pair? (cdr prev-f))
			 (equal? (cadr f) (cadr prev-f))
			 (not (side-effect? (cadr f) env))
			 (not (tree-set-member (gather-symbols (cadr prev-f)) (cddr prev-f)))) ; perhaps outvar here?
		    (lint-format "perhaps ~A" caller
				 (lists->string `(... ,prev-f ,f ...)
						(if (and (null? (cdddr prev-f))
							 (null? (cdddr f)))
						    (if (and (pair? (cadr f))
							     (eq? (caadr f) 'not))
							`(... (unless ,(cadadr f)
								,@(unbegin (caddr prev-f))
								,@(unbegin (caddr f))) ...)
							`(... (when ,(cadr f)
								,@(unbegin (caddr prev-f))
								,@(unbegin (caddr f))) ...))
						    `(... (if ,(cadr f)
							      (begin
								,@(unbegin (caddr prev-f))
								,@(unbegin (caddr f)))
							      (begin
								,@(if (pair? (cdddr prev-f)) (unbegin (cadddr prev-f)) ())
								,@(if (pair? (cdddr f)) (unbegin (cadddr f)) ())))
							  ...)))))
		;; cond occasionally is repeated, but almost never in a way we can combine

		;; --------
		;; check for repeated calls, but only one arg currently can change (more args = confusing separation in code)
		(let ((feq (and (pair? prev-f)
				(pair? f)
				(eq? (car f) (car  prev-f))
				(or (equal? (cdr f) (cdr prev-f))
				    (do ((fp (cdr f) (cdr fp))
					 (pp (cdr prev-f) (cdr pp))
					 (i 1 (+ i 1)))
					((or (and (null? pp) 
						  (null? fp))
					     (not (pair? pp))
					     (not (pair? fp))
					     (if (= i repeat-arg)                   ; ignore the arg that's known to be changing
						 (side-effect? (car pp) env)
						 (and (not (equal? (car pp) (car fp)))
						      (or (positive? repeat-arg)
							  (and (set! repeat-arg i)  ; call this one the changer
							       #f)))))
					 (and (null? pp)
					      (null? fp))))))))
		  (if feq
		      (set! repeats (+ repeats 1)))
		  (when (or (not feq)
			    (= ctr (- len 1))) ; this assumes we're not returning the last value?
		    (when (and (> repeats 2)
			       (not (hash-table-ref syntaces (car prev-f)))) ; macros should be ok here if args are constants
		      (let ((fs-end (if (not feq) fs (cdr fs))))

			(if (zero? repeat-arg)		    ; simple case -- all exprs are identical
			    (let ((step 'i))
			      (if (tree-member step prev-f)
				  (set! step (find-unique-name prev-f #f)))
			      (lint-format "perhaps ~A... ->~%~NC(do ((~A 0 (+ ~A 1))) ((= ~A ~D)) ~A)" caller 
					   (truncated-list->string prev-f)
					   pp-left-margin #\space
					   step step step (+ repeats 1)
					   prev-f))

			    (let ((args ())
				  (constants? #t)
				  (func-name (car prev-f))
				  (new-arg (if (tree-member 'arg prev-f)
					       (find-unique-name prev-f #f)
					       'arg)))
			      (do ((p start-repeats (cdr p)))
				  ((eq? p fs-end))
				(set! args (cons (list-ref (car p) repeat-arg) args))
				(set! constants? (and constants? (code-constant? (car args)))))
			      
			      (let ((func (if (and (= repeat-arg 1)
						   (null? (cddar start-repeats)))
					      func-name
					      `(lambda (,new-arg)
						 ,(let ((call (copy prev-f)))
						    (list-set! call repeat-arg new-arg)
						    call)))))
				(if constants?
				    (lint-format "perhaps ~A... ->~%~NC(for-each ~S '(~{~S~^ ~}))" caller
						 (truncated-list->string (car start-repeats))
						 pp-left-margin #\space
						 func
						 (map unquoted (reverse args)))
				    (let ((v (var-member func-name env)))
				      (if (or (and (var? v)
						   (memq (var-ftype v) '(define define* lambda lambda*)))
					      (procedure? (symbol->value func-name *e*)))
					  (lint-format "perhaps ~A... ->~%~NC(for-each ~S (vector ~{~S~^ ~}))" caller 
						       ;; vector rather than list because it is easier on the GC (list copies in s7)
						       (truncated-list->string (car start-repeats))
						       pp-left-margin #\space
						       func
						       (reverse args))
					  (if (not (or (var? v)
						       (macro? (symbol->value func-name *e*))))
					      (lint-format "assuming ~A is not a macro, perhaps ~A" caller
							   func-name
							   (lists->string (list '... (car start-repeats) '...) 
									  `(for-each ,func (vector ,@(reverse args))))))))))))))
		    (set! repeats 0)
		    (set! repeat-arg 0)
		    (set! start-repeats fs)))
		;; --------

		(if (pair? f)
		    (begin
		      (set! f-len (length f))
		      (if (eq? (car f) 'begin)
			  (lint-format "redundant begin: ~A" caller (truncated-list->string f))))
		    (set! f-len 0))
		
		(if (and (= f-len prev-len 3)
			 (eq? (car f) 'set!)
			 (eq? (car prev-f) 'set!))
		    (let ((arg1 (caddr prev-f))
			  (arg2 (caddr f)))
		      (if (eq? (cadr f) (cadr prev-f))
			  (cond ((not (and (pair? arg2)          ; (set! x 0) (set! x 1) -> "this could be omitted: (set! x 0)"
					   (tree-member (cadr f) arg2)))
				 (if (not (or (side-effect? arg1 env)
					      (side-effect? arg2 env)))
				     (lint-format "this could be omitted: ~A" caller prev-f)))
				
				((and (pair? arg1)               ; (set! x (cons 1 z)) (set! x (cons 2 x)) -> (set! x (cons 2 (cons 1 z)))
				      (pair? arg2)
				      (eq? (car arg1) 'cons)
				      (eq? (car arg2) 'cons)
				      (eq? (cadr f) (caddr arg2))
				      (not (eq? (cadr f) (cadr arg2))))
				 (lint-format "perhaps ~A ~A -> ~A" caller
					      prev-f f
					      `(set! ,(cadr f) (cons ,(cadr arg2) (cons ,@(cdr arg1))))))
				
				((and (= (tree-count1 (cadr f) arg2 0) 1) ; (set! x y) (set! x (+ x 1)) -> (set! x (+ y 1))
				      (or (not (pair? arg1))
					  (< (tree-length arg1) 5)))
				 (lint-format "perhaps ~A ~A ->~%~NC~A" caller 
					      prev-f f pp-left-margin #\space
					      (object->string `(set! ,(cadr f) ,(tree-subst arg1 (cadr f) arg2))))))

			  (if (and (symbol? (cadr prev-f)) ; (set! x (A)) (set! y (A)) -> (set! x (A)) (set! y x)
				   (pair? arg1)            ;   maybe more trouble than it's worth
				   (equal? arg1 arg2)
				   (not (eq? (car arg1) 'quote))
				   (hash-table-ref no-side-effect-functions (car arg1))
				   (not (tree-member (cadr prev-f) arg1))
				   (not (side-effect? arg1 env))
				   (not (maker? arg1)))
			      (lint-format "perhaps ~A" caller (lists->string f `(set! ,(cadr f) ,(cadr prev-f))))))))

		(if (< ctr (- len 1)) 
		    (begin		             ; f is not the last form, so its value is ignored
		      (if (and (pair? f)
			       (eq? (car f) 'error)
			       (not (var-member 'error env))
			       (pair? (cdr fs)) ; do special case
			       (every? (lambda (arg)
					 (not (and (symbol? arg)
						   (let ((v (var-member arg env)))
						     (and (var? v)
							  (eq? (var-initial-value v) call/cc-marker))))))
				       (cdr f)))
			  (if (= ctr (- len 2))
			      (lint-format "~A make this pointless: ~A" caller
					   (truncated-list->string f)
					   (truncated-list->string (cadr fs)))
			      (lint-format "~A makes the rest of the body unreachable: ~A" caller
					   (truncated-list->string f)
					   (truncated-list->string (list '... (cadr fs) '...)))))

		      (check-returns caller f env))

		    (begin                          ; here f is the last form in the body
		      (when (and (pair? prev-f)
				 (pair? (cdr prev-f)))
			
			(if (and (memq (car prev-f) '(display write write-char write-byte))
				 (equal? f (cadr prev-f))
				 (not (side-effect? f env)))
			    (lint-format "~A returns its first argument, so this could be omitted: ~A" caller 
					 (car prev-f) (truncated-list->string f)))
			
			(if (and (memq (car prev-f) '(vector-set! float-vector-set! int-vector-set! byte-vector-set!
								  string-set! list-set! hash-table-set! let-set!
								  set-car! set-cdr!))
				 (equal? f (list-ref prev-f (- (length prev-f) 1))))
			    (lint-format "~A returns the new value, so this could be omitted: ~A" caller 
					 (car prev-f) (truncated-list->string f)))
			
			(when (pair? (cddr prev-f))                ; (set! ((L 1) 2)) an error, but lint should keep going
			  (if (and (memq (car prev-f) '(set! define define* define-macro define-constant define-macro*
							     defmacro defmacro* define-expansion define-bacro define-bacro*))
				   (or (and (equal? (caddr prev-f) f)    ; (begin ... (set! x (...)) (...))
					    (not (side-effect? f env)))
				       (and (symbol? f)                  ; (begin ... (set! x ...) x)
					    (eq? f (cadr prev-f)))       ; also (begin ... (define x ...) x)
				       (and (not (eq? (car prev-f) 'set!))
					    (pair? (cadr prev-f))        ; (begin ... (define (x...)...) x)
					    (eq? f (caadr prev-f)))))
			      (lint-format "~A returns the new value, so this could be omitted: ~A" caller
					   (car prev-f) (truncated-list->string f)))
			  
			  (if (and (pair? f)
				   (pair? (cdr f))
				   (eq? (cadr prev-f) (cadr f))
				   (not (code-constant? (cadr f)))
				   (or (and (memq (car prev-f) '(vector-set! float-vector-set! int-vector-set!))
					    (memq (car f) '(vector-ref float-vector-ref int-vector-ref)))
				       (and (eq? (car prev-f) 'list-set!)
					    (eq? (car f) 'list-ref))
				       (and (eq? (car prev-f) 'string-set!)
					    (eq? (car f) 'string-ref))
				       (and (eq? (car prev-f) 'set-car!)
					    (eq? (car f) 'car))
				       (and (eq? (car prev-f) 'set-cdr!)
					    (eq? (car f) 'cdr)))
				   (or (memq (car f) '(car cdr)) ; no indices
				       (and (pair? (cddr f))     ; for the others check that indices match
					    (equal? (caddr f) (caddr prev-f))
					    (pair? (cdddr prev-f))
					    (not (pair? (cddddr prev-f)))
					    (not (pair? (cdddr f)))
					    (not (side-effect? (caddr f) env)))))
			      (lint-format "~A returns the new value, so this could be omitted: ~A" caller
					   (car prev-f) (truncated-list->string f)))))))
	      
		;; needs f fs prev-f dpy-f dpy-start ctr len
		;;   trap lint-format
		(let ((dpy-case (and (pair? f)
				     (memq (car f) '(display write newline write-char write-string))))) ; flush-output-port?
		  (when (and dpy-case
			     (not dpy-start))
		    (set! dpy-f fs)
		    (set! dpy-start ctr))
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
				  (lint-format "unexpected port change: ~A -> ~A in ~A~%" caller op (write-port d) d) ; ??
				  (done)))
			    (list-set! exprs dctr d)
			    (set! dctr (+ dctr 1))
			    (gather-format (display->format d))
			    (when (eq? d dpy-last) ; op can be null => send to (current-output-port), return #f or #<unspecified>
			      (lint-format "perhaps ~A" caller (lists->string exprs `(format ,op ,ctrl-string ,@(reverse args))))
			      (done)))
			  dpy-f))))
		    (set! dpy-start #f))
		  (unless dpy-case (set! dpy-start #f)))
		
		(if (and (pair? f)
			 (memq head '(defmacro defmacro* define-macro define-macro* define-bacro define-bacro*))
			 (tree-member 'unquote f))
		    (lint-format "~A probably has too many unquotes: ~A" caller head (truncated-list->string f)))

		(set! prev-f f)
		(set! prev-len f-len)

		(set! lint-current-form f)
		(if (= ctr (- len 1))
		    (set! env (lint-walk caller f env))
		    (begin
		      (set! lint-mid-form f)
		      (let ((e (lint-walk caller f env)))
			(if (and (pair? e)
				 (not (eq? (var-name (car e)) lambda-marker)))
			    (set! env e)))))
		(set! lint-current-form #f)
		(set! lint-mid-form #f)

		;; need to put off this ref tick until we have a var for it (lint-walk above)
		(when (and (= ctr (- len 1))
			   (pair? f)
			   (pair? (cdr f)))
		  (if (and (pair? (cadr f))
			   (memq (car f) '(define define* define-macro define-constant define-macro* define-expansion define-bacro define-bacro*)))
		      (set-ref (caadr f) #f env)
		      (if (memq (car f) '(defmacro defmacro*))
			  (set-ref (cadr f) #f env))))
		))
	    (set! lint-mid-form old-mid-form)
	    (set! lint-current-form old-current-form)))
      env)
    
    
    (define (lint-walk-function-body definer function-name args body env)
      ;; walk function body, with possible doc string at the start
      (when (and (pair? body)
		 (pair? (cdr body))
		 (string? (car body)))
	(if *report-doc-strings*
	    (lint-format "old-style doc string: ~S, in s7 use 'documentation:~%~NC~A" function-name
			 (car body) (+ lint-left-margin 4) #\space
			 (lint-pp `(define ,function-name
				     (let ((documentation ,(car body)))
				       (,(if (eq? definer 'define) 'lambda
					     (if (eq? definer 'define*) 'lambda*
						 definer))
					,args
					,@(cdr body)))))))
	(set! body (cdr body))) ; ignore old-style doc-string
      (lint-walk-body function-name definer body env))
    

    (define (lint-walk-function definer function-name args body form env)
      ;; check out function arguments (adding them to the current env), then walk its body
      ;; first check for (define (hi...) (ho...)) where ho has no opt args (and try to ignore possible string constant doc string)
      
      (when (eq? definer 'define)
	(let ((bval (if (and (pair? body)
			     (string? (car body)))
			(cdr body)          ; strip away the (old-style) documentation string
			body)))
	  
	  (when (and (pair? bval)           ; not (define (hi a) . 1)!
		     (pair? (car bval))
		     (null? (cdr bval))
		     (symbol? (caar bval))) ; not (define (hi) ((if #f + abs) 0))
	    
	    (cond ((equal? args (cdar bval))
		   (let* ((cval (caar bval))
			  (p (symbol->value cval *e*))
			  (ary (arity p)))
		     (if (or (procedure? p)
			     (let ((e (var-member cval env) ))
			       (and e
				    (var? e)
				    (symbol? (var-ftype e))
				    (let ((def (var-initial-value e)) 
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
			 (lint-format "~A~A could be (define ~A ~A)" function-name 
				      (if (and (procedure? p)
					       (not (= (car ary) (cdr ary)))
					       (not (= (length args) (cdr ary))))
					  (format #f "leaving aside ~A's optional arg~P, " cval (- (cdr ary) (length args)))
					  "")
				      function-name function-name cval))))

		  ;; (equal? args (reverse (cdar bval))) rarely happens, and never with a reversible op
		  
		  ((and (or (symbol? args)
			    (and (pair? args)
				 (negative? (length args))))
			(eq? (caar bval) 'apply)
			(pair? (cdar bval))
			(symbol? (cadar bval))
			(not (memq (cadar bval) '(and or)))
			(pair? (cddar bval))
			(or (and (eq? args (caddar bval))
				 (null? (cdddar bval)))
			    (and (pair? args)
				 (equal? (cddar bval) (proper-list args)))))
		   (lint-format "~A could be (define ~A ~A)" function-name function-name function-name (cadar bval)))
		  
		  ((and (memq (caar bval) '(car cdr caar cadr cddr cdar caaar caadr caddr cdddr cdaar cddar cadar cdadr cadddr cddddr))
			(pair? (cadar bval)))
		   ((lambda* (cr arg)
		      (and cr
			   (< (length cr) 5)
			   (= (length args) 1)
			   (eq? (car args) arg)
			   (let ((f (string->symbol (string-append "c" cr "r"))))
			     (if (eq? f function-name)
				 (lint-format "this redefinition of ~A is pointless (use (with-let (unlet)...) or #_~A)" definer function-name function-name)
				 (lint-format "~A could be (define ~A ~A)" function-name function-name function-name f)))))
		    (combine-cxrs (car bval))))

		  ((not (and (memq (caar bval) '(list-ref list-tail))
			     (pair? (cdar bval))
			     (pair? (cddar bval))
			     (pair? args)
			     (eq? (car args) (cadar bval))
			     (null? (cdr args)))))

		  ((eq? (caar bval) 'list-ref)
		   (case (caddar bval)
		     ((0) (lint-format "~A could be (define ~A car)" function-name function-name function-name))
		     ((1) (lint-format "~A could be (define ~A cadr)" function-name function-name function-name))
		     ((2) (lint-format "~A could be (define ~A caddr)" function-name function-name function-name))
		     ((3) (lint-format "~A could be (define ~A cadddr)" function-name function-name function-name))))

		  (else
		   (case (caddar bval)
		     ((1) (lint-format "~A could be (define ~A cdr)" function-name function-name function-name))
		     ((2) (lint-format "~A could be (define ~A cddr)" function-name function-name function-name))
		     ((3) (lint-format "~A could be (define ~A cdddr)" function-name function-name function-name))
		     ((4) (lint-format "~A could be (define ~A cddddr)" function-name function-name function-name))))))))
      
      (let ((data (and (symbol? function-name)
		       (make-fvar :name (if (not (memq definer '(lambda lambda*))) function-name lambda-marker)
				  :ftype definer
				  :initial-value form
				  :env env
				  :arglist (if (memq definer '(lambda lambda*))
					       (cadr form)
					       (if (memq definer '(defmacro defmacro*))
						   (caddr form)
						   (cdadr form)))))))
	(when data
	  (let ((ldata (cdr data)))
	    (set! (ldata 'decl)
		  (catch #t
		    (lambda ()
		      (case definer
			((lambda)
			 (set! (ldata 'allow-other-keys) #t)
			 (eval (list definer (cadr form) #f)))
			
			((lambda*)
			 (set! (ldata 'allow-other-keys) (eq? (last-par (cadr form)) :allow-other-keys))
			 (eval (list definer (copy (cadr form)) #f)))         ; eval can remove :allow-other-keys!
			
			((define*)
			 (set! (ldata 'allow-other-keys) (eq? (last-par (cdadr form)) :allow-other-keys))
			 (eval (list definer (cons '_ (copy (cdadr form))) #f)))
			
			((defmacro defmacro*)
			 (set! (ldata 'allow-other-keys) (or (not (eq? definer 'defmacro*))
							     (eq? (last-par (caddr form)) :allow-other-keys)))
			 (eval (list definer '_ (caddr form) #f)))
			
			((define-constant)
			 (set! (ldata 'allow-other-keys) #t)
			 (eval (list 'define (cons '_ (cdadr form)) #f)))
			
			(else
			 (set! (ldata 'allow-other-keys) (or (not (memq definer '(define-macro* define-bacro*)))
							     (eq? (last-par (cdadr form)) :allow-other-keys)))
			 (eval (list definer (cons '_ (cdadr form)) #f)))))
		    (lambda args
		      'error)))))
	
	(if (null? args)
	    (begin
	      (if (memq definer '(define* lambda* defmacro* define-macro* define-bacro*))
		  (lint-format "~A could be ~A" 
			       function-name definer
			       (symbol (substring (symbol->string definer) 0 (- (length (symbol->string definer)) 1)))))
	      (let ((cur-env (if data (cons data env) env)))
		(let ((e (lint-walk-function-body definer function-name args body cur-env)))
		  (let ((nvars (and (not (eq? e cur-env))
				    (env-difference function-name e cur-env ()))))
		    (if (pair? nvars)
			(report-usage function-name definer nvars cur-env))))
		cur-env))
	    
	    (if (not (or (symbol? args) 
			 (pair? args)))
		(begin
		  (lint-format "strange ~A parameter list ~A" function-name definer args)
		  env)
		(let ((arg-data (if (symbol? args)                            ; this is getting arg names to add to the environment
				    (list (make-var :name args :definer 'parameter))
				    (map
				     (lambda (arg)
				       (if (symbol? arg)
					   (if (memq arg '(:rest :allow-other-keys))
					       (values)                  ; omit :rest and :allow-other-keys
					       (make-var :name arg :definer 'parameter))
					   (if (not (and (pair? arg)
							 (= (length arg) 2)
							 (memq definer '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
					       (begin
						 (lint-format "strange parameter for ~A: ~S" function-name definer arg)
						 (values))
					       (begin
						 (if (not (cadr arg))
						     (lint-format "the default argument value is #f in ~A ~A" function-name definer arg))
						 (make-var :name (car arg) :definer 'parameter)))))
				     (proper-list args)))))
		  
		  (let ((cur-env (append arg-data (if data (cons data env) env))))
		    (let ((e (lint-walk-function-body definer function-name args body cur-env)))
		      (let ((nvars (and (not (eq? e cur-env))
					(env-difference function-name e cur-env ()))))
			(report-usage function-name definer (append (or nvars ()) arg-data) cur-env))))
		  
		  (when (and (var? data)
			     (memq definer '(define lambda define-macro))
			     (proper-list? args)) ; this could be extended
		    ;; look for unused parameters that are passed a value other than #f (ignore rest args either way)
		    (let ((set ())
			  (unused ()))
		      (for-each 
		       (lambda (arg)
			 (if (zero? (var-ref arg))
			     (if (positive? (var-set arg))
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
		      env))))))
    
    
    (define (check-bool-cond caller form c1 c2 env)
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
			     (lint-format "perhaps ~A" caller 
					  (lists->string form (if (eq? (cadr c1) #t) 
								  (car c1)
								  (simplify-boolean `(not ,(car c1)) () () env)))))
			(and (not (cadr c1))   ; (cond (x #f) (else y)) -> (and (not x) y)
			     (let ((cc1 (simplify-boolean `(not ,(car c1)) () () env)))
			       (lint-format "perhaps ~A" caller 
					    (lists->string form 
							   (if (null? (cddr c2)) 
							       `(and ,cc1 ,(cadr c2))
							       `(and ,cc1 (begin ,@(cdr c2))))))))
			(and (pair? (car c1))  ; (cond ((null? x) #t) (else y)) -> (or (null? x) y)
			     (eq? (return-type (caar c1) env) 'boolean?)
			     (lint-format "perhaps ~A" caller
					  (lists->string form 
							 (if (null? (cddr c2)) 
							     `(or ,(car c1) ,(cadr c2))
							     `(or ,(car c1) (begin ,@(cdr c2)))))))))
	       (and (boolean? (cadr c2))
		    (null? (cddr c2))
		    (not (equal? (cadr c1) (cadr c2)))
		    (lint-format "perhaps ~A" caller
				 (lists->string form
						(if (cadr c2)
						    `(or (not ,(car c1)) ,(cadr c1))
						    (if (and (pair? (car c1))
							     (eq? (caar c1) 'and))
							(append (car c1) (cdr c1))
							`(and ,@c1)))))))))
    
    (define (case-branch test eqv-select exprs)
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
	   ,@exprs))))

    (define (cond->case eqv-select new-clauses)
      `(case ,eqv-select 
	 ,@(map (lambda (clause)
		  (let ((test (car clause))
			(exprs (cdr clause)))
		    (if (null? exprs)                   ; cond returns the test result if no explicit results
			(set! exprs (list #t)))         ;   but all tests here return a boolean, and we win only if #t?? (memx is an exception)
		    (if (memq test '(else #t))
			`(else ,@exprs)
			(case-branch test eqv-select exprs))))
		new-clauses)))
    
    (define (eqv-code-constant? x)
      (or (number? x)
	  (char? x)
	  (and (pair? x)
	       (eq? (car x) 'quote)
	       (or (symbol? (cadr x))
		   (and (not (pair? (cadr x)))
			(eqv-code-constant? (cadr x)))))
	  (memq x '(#t #f () #<unspecified> #<undefined> #<eof>))))

    (define (cond-eqv? clause eqv-select or-ok)
      (if (not (pair? clause))
	  (memq clause '(else #t))
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
	    
	    (else #f))))
    
    (define (find-constant-exprs caller vars body)
      (if (tree-set-member '(call/cc call-with-current-continuation lambda lambda* define define* 
			     define-macro define-macro* define-bacro define-bacro* define-constant define-expansion)
			    body)
	  ()
	  (let* ((vs (out-vars caller vars body))
		 (refs (remove-if (lambda (v)
				    (or (assq v vars)        ; vars = do-loop steppers
					(memq v (cadr vs)))) ; (cadr vs) = sets
				  (car vs)))
	    ;; refs are the external variables accessed in the do-loop body
	    ;;   that are not set or shadowed or changed (vector-set! etc)
		 (constant-exprs ()))
	      
	    (define (all-ok? tree)
	      (if (symbol? tree)
		  (memq tree refs)
		  (or (not (pair? tree))
		      (eq? (car tree) 'quote)
		      (and (hash-table-ref no-side-effect-functions (car tree)) ; super-careful...
			   (or (not (hash-table-ref syntaces (car tree))) ; someday??
			       (memq (car tree) '(if begin cond or and unless when)))
			   (not (memq (car tree) makers))
			   (list? (cdr tree))
			   (every? all-ok? (cdr tree))))))
	    
	    (define (expr-walk tree)
	      (if (pair? tree)
		  (if (all-ok? tree)
		      (if (not (or (eq? (car tree) 'quote)
				   (member tree constant-exprs)))
			  (set! constant-exprs (cons tree constant-exprs)))
		      (begin
			(if (pair? (car tree))
			    (expr-walk (car tree)))
			(if (pair? (cdr tree))
			    (let ((f (cdr tree)))
			      (case (car f)
				((case) 
				 (when (and (pair? (cdr f))
					    (pair? (cddr f)))
				   (expr-walk (cadr f))
				   (for-each (lambda (c) (expr-walk (cdr c))) (cddr f))))

				((letrec letrec*) 
				 (when (pair? (cddr f))
				   (for-each (lambda (c) 
					       (if (and (pair? c)
							(pair? (cdr c)))
						   (expr-walk (cadr c))))
					     (cadr f))
				   (expr-walk (cddr f))))

				((let let*)
				 (when (pair? (cddr f))
				   (if (symbol? (cadr f)) (set! f (cdr f)))
				   (for-each (lambda (c) 
					       (if (and (pair? c)
							(pair? (cdr c)))
						   (expr-walk (cadr c))))
					     (cadr f))
				   (expr-walk (cddr f))))

				((do) 
				 (when (and (list? (cadr f))
					    (list? (cddr f))
					    (pair? (cdddr f)))
				   (for-each (lambda (c) 
					       (if (pair? (cddr c)) 
						   (expr-walk (caddr c)))) 
					     (cadr f))
				   (expr-walk (cdddr f))))

				(else 
				 (for-each expr-walk f)))))))))
	    (expr-walk body)

	    (when (pair? constant-exprs)
	      (set! constant-exprs (remove-if (lambda (p)
						(or (null? (cdr p)) ; omit (x) (not x) (- x)
						    (and (null? (cddr p))
							 (memq (car p) '(not -))
							 (symbol? (cadr p)))
						    (tree-member 'port-line-number p)))
					      constant-exprs)))
	    constant-exprs)))

    (define (find-let-constant-exprs caller form vars body)
      (let ((zv (map (lambda (v) 
		       (if (and (zero? (var-set v))
				(not (tree-member (var-name v) (var-initial-value v))))
			   v
			   (values)))
		     vars)))
	(when (pair? zv)
	  (let ((constant-exprs (find-constant-exprs 'let (map (lambda (v)
								 (if (positive? (var-set v))
								     (var-name v)
								     (values)))
							       vars)
						     body)))
	    (when (pair? constant-exprs)
	      (let ((vals (map (lambda (v)
				 (cons (var-initial-value v) (var-name v)))
			       zv)))
		(for-each (lambda (expr)
			    (cond ((or (assoc expr vals)
				       (and (pair? expr)
					    (hash-table-ref reversibles (car expr))
					    (= 3 (length expr))
					    (assoc (list (hash-table-ref reversibles (car expr)) (caddr expr) (cadr expr)) vals)))
				   => (lambda (ev)
					(lint-format "~A is ~A in ~A~%" caller 
						     (car ev) (cdr ev) (truncated-list->string form))))))
			  constant-exprs)))))))
    
    (define (find-call sym body)
      (call-with-exit
       (lambda (return)
	 (let tree-call ((tree body))
	   (if (and (pair? tree)
		    (not (eq? (car tree) 'quote)))
	       (begin
		 (if (eq? (car tree) sym)
		     (return tree))
		 (if (memq (car tree) '(let let* letrec letrec* do lambda lambda* define))
		     (return #f)) ; possible shadowing -- not worth the infinite effort to corroborate
		 (if (pair? (car tree))
		     (tree-call (car tree)))
		 (if (pair? (cdr tree))
		     (do ((p (cdr tree) (cdr p)))
			 ((not (pair? p)) #f)
		       (tree-call (car p))))))))))

    (define (lint-walk caller form env)
      ;; walk a form, here curlet can change
      ;(format *stderr* "lint-walk ~A ~A~%" form env)

      (if (symbol? form)
	  (set-ref form #f env) ; returns env
	  
	  (if (pair? form)
	      (let ((head (car form)))

		(set! line-number (pair-line-number form))

		(when *report-function-stuff* 
		  (function-match caller form env))
		
		(case head
		  
		  ;; ---------------- define ----------------		  
		  ((define define* define-constant define-envelope define-expansion 
		    define-macro define-macro* define-bacro define-bacro* definstrument defanimal
		    define-public) ; this gives more informative names in Guile

		   (if (< (length form) 2)
		       (begin
			 (lint-format "~S makes no sense" caller form)
			 env)
		       (let ((sym (cadr form))
			     (val (cddr form)))
			 ;(format *stderr* "define ~S ~S~%" sym val)
			 
			 (if (symbol? sym)
			     (begin

			       (cond ((keyword? sym)               ; (define :x 1)
				      (lint-format "keywords are constants ~A" caller sym))

				     ((and (eq? sym 'pi)           ; (define pi (atan 0 -1))
					   (member (car val) '((atan 0 -1)
							       (acos -1)
							       (* 2 (acos 0))
							       (* 4 (atan 1)))))
				      (lint-format "~A is one of its many names, but pi a predefined constant in s7" caller (car val)))

				     ((constant? sym)              ; (define most-positive-fixnum 432)
				      (lint-format "~A is a constant in s7: ~A" caller sym form))

				     ((let ((v (var-member sym env)))
					(and (var? v)
					     (eq? (var-definer v) 'define-constant)
					     (not (equal? (caddr form) (var-initial-value v)))))
				      (lint-format "~A in ~A is already a constant, defined ~A~A" caller sym
						   (truncated-list->string form)
						   (if (and (pair? (var-initial-value v))
							    (positive? (pair-line-number (var-initial-value v))))
						       (format #f "(line ~D): " (pair-line-number (var-initial-value v)))
						       "")
						   (truncated-list->string (var-initial-value v)))))
			       
			       (if (memq head '(define define-constant define-envelope 
						define-public))
				   (let ((len (length form)))
				     (if (not (= len 3))
					 (lint-format "~A has ~A value~A?"
						      caller (truncated-list->string form)
						      (if (< len 3) "no" "too many") 
						      (if (< len 3) "" "s"))))
				   (lint-format "~A is messed up" caller (truncated-list->string form)))
			       
			       (if (and (pair? val)
					(null? (cdr val))
					(equal? sym (car val)))
				   (lint-format "this ~A is either not needed, or is an error: ~A" caller head (truncated-list->string form)))
			       
			       (if (pair? val)
				   (let ((e (lint-walk (if (and (pair? (car val))
								(eq? (caar val) 'letrec))
							   'define sym)
						       (car val) env)))
				     (if (and (pair? e)
					      (not (eq? e env))
					      (eq? (var-name (car e)) lambda-marker)) ; (define x (lambda ...))
					 (begin
					   (set! (var-name (car e)) sym)

					   ;; (define x (letrec ((y (lambda...))) (lambda (...) (y...)))) -> (define (x...)...)
					   (let* ((let-form (caddr form))
						  (var (and (pair? (cadr let-form))
							    (null? (cdadr let-form)) ; just one var in let/rec
							    (caadr let-form))))
					     (when (and (pair? var)
							(symbol? (car var))
							(pair? (cddr let-form))
							(pair? (caddr let-form))
							(null? (cdddr let-form))     ; just one form in the let/rec
							(pair? (cdr var))
							(pair? (cadr var))
							(eq? (caadr var) 'lambda)    ; var is lambda
							(proper-list? (cadadr var))) ; it has no rest arg
					       (let ((body (caddr let-form)))
						 (when (and (eq? (car body) 'lambda)      ; let/rec body is lambda calling var
							    (proper-list? (cadr body)))   ; rest args are a headache
						   (when (pair? (caddr body))   ; (lambda (...) (...) where car is letrec func name
						     (if (eq? (caaddr body) (car var))
							 (lint-format "perhaps ~A" caller
								  (lists->string form
										 `(define (,sym ,@(cadr body))
										    (let ,(car var)
										      ,(map list (cadadr var) (cdaddr body))
										      ,@(cddadr var)))))
							 (let ((call (find-call (car var) (caddr body))))
							   (when (pair? call)       ; inner lambda body is (...some-expr...(sym...) ...)
							     (if (= (tree-count1 (car var) (caddr body) 0) 1)
								 (let ((new-call `(let ,(car var)
										    ,(map list (cadadr var) (cdr call))
										    ,@(cddadr var))))
								   (lint-format "perhaps ~A" caller
										(lists->string form
											       `(define (,sym ,@(cadr body))
												  ,(tree-subst new-call call
													       (caddr body)))))))))))))))
					   (when (and *report-function-stuff*
						      (pair? (caddr (var-initial-value (car e)))))
					     (hash-table-set! equable-closures (caaddr (var-initial-value (car e)))
							      (cons (car e) (or (hash-table-ref equable-closures (caaddr (var-initial-value (car e)))) ()))))
					   e)
					 (cons (make-var :name sym :initial-value (car val) :definer head) env)))
				   (cons (make-var :name sym :initial-value val :definer head) env)))
			     
			     ;; not (symbol? sym)
			     (if (and (pair? sym) ; cadr form
				      (pair? val) ; cddr form
				      (not (pair? (car sym)))) ; curried func or something equally stupid
				 (begin
				   
				   ;; perhaps this block should be on a *report-* switch --
				   ;;   it translates some internal defines into named lets
				   ;;   (or just normal lets, etc)
				   (when (and (pair? (car val))
					      (eq? (caar val) 'define)
					      (pair? (cdr val))
					      (pair? (cadar val))) ; inner define (name ...)
				     (let ((inner-name (caadar val))
					   (inner-args (cdadar val))
					   (inner-body (cddar val))
					   (outer-name (car sym))
					   (outer-args (cdr sym))
					   (outer-body (cdddr form)))
				       (if (and (symbol? inner-name)
						(proper-list? inner-args)
						(pair? (car outer-body))
						;(null? (cdr outer-body))
						(= (tree-count1 inner-name outer-body 0) 1))
					   (let ((call (find-call inner-name outer-body)))
					     (when (pair? call)
					       (let ((new-call (if (tree-memq inner-name inner-body)
								   (if (and (null? inner-args)
									    (null? outer-args))
								       (if (null? (cdr inner-body))
									   (car (tree-subst outer-name inner-name inner-body))
									   `(begin ,@(tree-subst outer-name inner-name inner-body)))
								       `(let ,inner-name
									  ,(if (null? inner-args) () (map list inner-args (cdr call)))
									  ,@inner-body))
								   (if (or (null? inner-args)
									   (and (equal? inner-args outer-args)
										(equal? inner-args (cdr call))))
								       (if (null? (cdr inner-body))
									   (car (tree-subst outer-name inner-name inner-body))
									   `(begin ,@(tree-subst outer-name inner-name inner-body)))
								       `(let ,(map list inner-args (cdr call))
									  ,@inner-body)))))
						 (lint-format "perhaps ~A" caller
							      (lists->string form 
									     `(,head ,sym 
										     ,@(let ((p (tree-subst new-call call outer-body)))
											 ;(format *stderr* "p: ~A~%" p)
											 (if (and (pair? p)
												  (pair? (car p))
												  (eq? (caar p) 'begin))
											     (cdar p)
											     p)))))))))))
				   (when (pair? (cdr sym))
				     (if (repeated-member? (proper-list (cdr sym)) env)
					 (lint-format "~A parameter is repeated: ~A" caller head (truncated-list->string sym)))
				     
				     (cond ((memq head '(define* define-macro* define-bacro*))
					    (check-star-parameters (car sym) (cdr sym)))
					   ((list-any? keyword? (cdr sym))
					    (lint-format "~A parameter can't be a keyword: ~A" caller (car sym) sym))
					   ((memq 'pi (cdr sym))
					    (lint-format "~A parameter can't be a constant: ~A" caller (car sym) sym))))
				   
				   (when (and (eq? head 'define-macro)
					      (null? (cdr sym))
					      (null? (cdr val))
					      (code-constant? (car val)))
				     (lint-format "perhaps ~A" caller (lists->string form `(define ,(car sym) ,(car val)))))
				   
				   (if (and (eq? head 'definstrument)
					    (string? (car val)))
				       (set! val (cdr val)))
				   
				   (if (keyword? (car sym))
				       (begin
					 (lint-format "keywords are constants ~A" caller (car sym))
					 env)
				       (lint-walk-function head (car sym) (cdr sym) val form env)))
				 
				 (begin
				   (lint-format "strange form: ~A" head (truncated-list->string form))
				   env))))))
		  
		  ;; ---------------- lambda ----------------		  
		  ((lambda lambda*)
		   (let ((len (length form)))
		     (if (< len 3)
			 (begin
			   (lint-format "~A is messed up in ~A" caller head (truncated-list->string form))
			   env)
			 (let ((args (cadr form)))
			   (if (list? args)
			       (let ((arglen (length args)))
				 (if (null? args)
				     (if (eq? head 'lambda*)             ; (lambda* ()...) -> (lambda () ...)
					 (lint-format "lambda* could be lambda: ~A" caller form))
				     (begin ; args is a pair             ; (lambda (a a) ...)
				       (if (repeated-member? (proper-list args) env)
					   (lint-format "~A parameter is repeated: ~A" caller head (truncated-list->string args)))
				       (if (eq? head 'lambda*)           ; (lambda* (a :b) ...)
					   (check-star-parameters head args)
					   (if (list-any? keyword? args) ; (lambda (:key) ...)
					       (lint-format "lambda arglist can't handle keywords (use lambda*)" caller)))))
				 
				 (if (and (eq? head 'lambda)           ; (lambda () (f)) -> f, (lambda (a b) (f a b)) -> f
					  (not (eq? caller 'case-lambda))    
					  (= len 3)
					  (>= arglen 0)) ; not a dotted list
				     (let ((body (caddr form)))
				       (when (and (pair? body)
						  (symbol? (car body))
						  (not (memq (car body) '(and or))))
					 (cond ((equal? args (cdr body))
						(lint-format "perhaps ~A" caller (lists->string form (car body))))

					       ((equal? (reverse args) (cdr body))
						(let ((rf (hash-table-ref reversibles (car body))))
						  (if rf (lint-format "perhaps ~A" caller (lists->string form rf)))))

					       ((and (= arglen 1)
						     (memq (car body) '(car cdr caar cadr cddr cdar caaar caadr caddr 
									cdddr cdaar cddar cadar cdadr cadddr cddddr)))
						((lambda* (cr arg) ; lambda* not lambda because combine-cxrs might return just #f
						   (and cr
							(< (length cr) 5) 
							(eq? (car args) arg)
							(lint-format "perhaps ~A" caller 
								     (lists->string form (string->symbol (string-append "c" cr "r"))))))
						 (combine-cxrs body)))))))))
				 
			   (if (and (or (symbol? args)                 ; (lambda args (apply f args)) -> f
					(and (pair? args)              ; (lambda #\a ...) !
					     (negative? (length args))))
				    (eq? head 'lambda)
				    (not (eq? caller 'case-lambda))    
				    (= len 3))
			       (let ((body (caddr form)))
				 (if (and (pair? body)
					  (eq? (car body) 'apply)
					  (pair? (cdr body))
					  (symbol? (cadr body))
					  (not (memq (cadr body) '(and or)))
					  (pair? (cddr body))
					  (or (eq? args (caddr body))
					      (and (pair? args)
						   (equal? (cddr body) (proper-list args)))))
				     (lint-format "perhaps ~A" caller (lists->string form (cadr body))))))
			   
			   (lint-walk-function head caller args (cddr form) form env)
			   ;env -- not this -- return the lambda-marker+old env via lint-walk-function
			   ))))
		  
		  ;; ---------------- set! ----------------		  
		  ((set!)
		   (if (not (= (length form) 3))
		       (begin
			 (lint-format "set! has too ~A arguments: ~S" caller (if (> (length form) 3) "many" "few") form)
			 env)
		       (let ((settee (cadr form))
			     (setval (caddr form)))
			 (let ((result (lint-walk caller setval env)))
			   (if (symbol? settee)
			       (if (constant? settee)
				   (lint-format "can't set! ~A (it is a constant)" caller (truncated-list->string form))
				   (let ((v (var-member settee env)))
				     (if (and (var? v)
					      (eq? (var-definer v) 'define-constant))
					 (lint-format "can't set! ~A in ~A (it is a constant: ~A~A)" caller settee
						      (truncated-list->string form)
						      (if (and (pair? (var-initial-value v))
							       (positive? (pair-line-number (var-initial-value v))))
							  (format #f "(line ~D): " (pair-line-number (var-initial-value v)))
							  "")
						      (truncated-list->string (var-initial-value v))))))
			       (if (not (pair? settee))
				   (lint-format "can't set! ~A" caller (truncated-list->string form))
				   (begin
				     (if (memq (car settee) '(vector-ref list-ref string-ref hash-table-ref))
					 (lint-format "~A as target of set!~A" caller (car settee) (truncated-list->string form)))
				     (lint-walk caller settee env) ; this counts as a reference since it's by reference so to speak
				     
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
						 (when (and (symbol? checker)
							    (not (compatible? checker arg-type)))
						   (lint-format "~A: new value should be a~A ~A: ~S: ~A" 
								caller (car settee)
								(if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
								checker arg-type
								(truncated-list->string form)))))))))
				     (set! settee (do ((sym (car settee) (car sym)))
						      ((not (pair? sym)) sym))))))
			   
			   (if (symbol? (cadr form)) ; see do directly above -- sets settee so we have to go back to (cadr form)
			       (set-set (cadr form) form env)
			       (if (and (pair? (cadr form))
					(symbol? settee))
				   (set-ref settee `(implicit-set ,@(cdr form)) env)))
			   
			   (if (equal? (cadr form) setval) ; not settee here!
			       (lint-format "pointless set! ~A" caller (truncated-list->string form)))
			   
			   result))))
		  
		  ;; ---------------- quote ----------------		  
		  ((quote) 
		   (let ((len (length form)))
		     (if (negative? len)
			 (lint-format "stray dot in quote's arguments? ~S" caller form)
			 (if (not (= len 2))
			     (lint-format "quote has too ~A arguments: ~S" caller (if (> len 2) "many" "few") form)
			     (let ((arg (cadr form)))
			       (if (pair? arg)
				   (if (> (length arg) 8)
				       (hash-table-set! big-constants arg (+ 1 (or (hash-table-ref big-constants arg) 0))))
				   (when (not (or (>= quote-warnings 20)
						  (and (symbol? arg) 
						       (not (keyword? arg)))))
				     (set! quote-warnings (+ quote-warnings 1))
				     (lint-format "quote is not needed here: ~A~A" caller
						  (truncated-list->string form)
						  (if (= quote-warnings 20) "; will ignore this error henceforth." ""))))))))
		   env)
		  
		  ;; ---------------- if ----------------
		  ((if)
		   (let ((len (length form)))
		     (if (> len 4)
			 (lint-format "if has too many clauses: ~A" caller (truncated-list->string form))
			 (if (< len 3)
			     (lint-format "if has too few clauses: ~A" caller (truncated-list->string form))
			     (let ((test (cadr form))
				   (true (caddr form))
				   (false (if (= len 4) (cadddr form) 'no-false))
				   (suggestion made-suggestion))
			       
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

				 (cond ((eq? old tree)
					(cons new (cdr tree)))
					((not (pair? tree)) 
					 tree)
					((eq? (car tree) 'quote)
					 (copy-tree tree))
					(else (cons (tree-subst-eq new old (car tree))
						    (tree-subst-eq new old (cdr tree))))))

			       (if (eq? false #<unspecified>)
				   (lint-format "this #<unspecified> is redundant: ~A" caller form))

			       (when (and (pair? true)
					  (pair? false)
					  (not (memq (car true) (list 'quote {list})))
					  (not (any-macro? (car true) env))
					  (pair? (cdr true)))
				 (let ((diff (differ-in-one true false)))
				   (if (pair? diff)
				       (if (not (or (equal? (car true) (caadr diff))  ; (if z (+ x y) (- x y))? 
						    (and (eq? (car true) 'set!)       ; (if x (set! y w) (set! z w))
							 (equal? (caar diff) (cadr true)))))
					   (lint-format "perhaps ~A" caller 
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
				       (if (not (memq (car true) '(let do lambda)))
					   (let ((seqdiff (differ-in-one-seq true false 0)))
					     ;; (format *stderr* "seqdiff: ~A ~A ~A~%" seqdiff true false)
					     ;; cadr replacement is too messy, looks good about 1 in 10 times
					     (if (and (pair? seqdiff)
						      (< (tree-length (cadr seqdiff)) 25)) ; 100 is too big, 30 is ok perhaps
						 (lint-format "perhaps ~A" caller
							      (lists->string form (tree-subst-eq `(if ,test ,@(cadr seqdiff)) (car seqdiff) true)))))))))
			       
			       (unless (= last-if-line-number line-number)
				 (do ((iff form (cadddr iff))
				      (iffs 0 (+ iffs 1)))
				     ((not (and (<= iffs 2)
						(pair? iff)
						(= (length iff) 4)
						(eq? (car iff) 'if)))
				      (when (or (> iffs 2)
						(and (= iffs 2)
						     (pair? iff)
						     (= (length iff) 3)
						     (eq? (car iff) 'if)))
					(set! last-if-line-number line-number)
					(lint-format "perhaps use cond: ~A" caller
						     (lists->string form 
								    `(cond ,@(do ((iff form (cadddr iff))
										  (clauses ()))
										 ((not (and (pair? iff)
											    (= (length iff) 4)
											    (eq? (car iff) 'if)))
										  (append (reverse clauses)
											  (if (and (pair? iff)
												   (= (length iff) 3)
												   (eq? (car iff) 'if))
											      `((,(cadr iff) ,@(unbegin (caddr iff))))
											      `((else ,@(unbegin iff))))))
									       (set! clauses (cons (cons (cadr iff) (unbegin (caddr iff))) clauses))))))))))
			       
			       (if (never-false test)
				   (lint-format "if test is never false: ~A" caller (truncated-list->string form))
				   (if (and (never-true test) true) ; complain about (if #f #f) later
				       (lint-format "if test is never true: ~A" caller (truncated-list->string form))))
			       
			       (let ((expr (simplify-boolean test () () env)))

				 (if (not (side-effect? test env))
				     (cond ((or (equal? test true)               ; (if x x y) -> (or x y)
						(equal? expr true))
					    (lint-format "perhaps ~A" caller 
							 (lists->string form 
									(if (eq? false 'no-false)
									    (simplify-boolean `(or ,expr #<unspecified>) () () env)
									    (simplify-boolean `(or ,expr ,false) () () env)))))
					   ((or (equal? test `(not ,true))       ; (if x (not x) y) -> (and (not x) y)
						(equal? `(not ,test) true))      ; (if (not x) x y) -> (and x y)
					    (lint-format "perhaps ~A" caller 
							 (lists->string form 
									(if (eq? false 'no-false)
									    (simplify-boolean `(and ,true #<unspecified>) () () env)
									    (simplify-boolean `(and ,true ,false) () () env)))))
					   ((or (equal? test false)              ; (if x y x) -> (and x y)
						(equal? expr false))
					    (lint-format "perhaps ~A" caller 
							 (lists->string form (simplify-boolean `(and ,expr ,true) () () env))))
					   ((or (equal? `(not ,test) false)      ; (if x y (not x)) -> (or (not x) y)
						(equal? test `(not ,false)))     ; (if (not x) y x) -> (or x y)
					    (lint-format "perhaps ~A" caller 
							 (lists->string form (simplify-boolean `(or ,false ,true) () () env))))))
				 (when (= len 4)
				   (if (and (pair? true)
					    (eq? (car true) 'if))
				       (if (= (length true) 4)
					   (begin
					     (if (equal? expr (simplify-boolean `(not ,(cadr true)) () () env))
						 (lint-format "pointless repetition of if test: ~A" caller
							      (lists->string form `(if ,expr ,(cadddr true) ,false))))
					     (if (equal? false (cadddr true))
						 (lint-format "perhaps ~A" caller 
							      (lists->string form (if (not false)
										      `(and ,expr ,(cadr true) ,(caddr true))
										      `(if (and ,expr ,(cadr true)) ,(caddr true) ,false))))
						 (if (equal? false (caddr true))
						     (lint-format "perhaps ~A" caller 
								  (lists->string form (if (not false)
											  `(and ,expr (not ,(cadr true)) ,(cadddr true))
											  `(if (and ,expr (not ,(cadr true))) ,(cadddr true) ,false)))))))
					   (begin
					     (if (equal? expr (simplify-boolean `(not ,(cadr true)) () () env))
						 (lint-format "pointless repetition of if test: ~A" caller 
							      (lists->string form `(if (not ,expr) ,false))))
					     (if (equal? false (caddr true)) ; (if a (if b A) A)
						 (lint-format "perhaps ~A" caller
							      (lists->string form `(if (or (not ,expr) ,(cadr true)) ,false)))))))
				   
				   (if (pair? false)
				       (case (car false)
					 ((cond)                 ; (if a A (cond...)) -> (cond (a  A) ...)
					  (lint-format "perhaps ~A" caller (lists->string form `(cond (,expr ,true) ,@(cdr false)))))

					 ((if)
					  (if (= (length false) 4)
					      (if (equal? true (caddr false))
						  (if true
						      (lint-format "perhaps ~A" caller 
								   (lists->string form `(if (or ,expr ,(cadr false)) ,true ,(cadddr false))))
						      (lint-format "perhaps ~A" caller 
								   (lists->string form `(and (not (or ,expr ,(cadr false))) ,(cadddr false)))))
						  (if (equal? true (cadddr false))
						      (if true
							  (lint-format "perhaps ~A" caller 
								       (lists->string form `(if (or ,expr (not ,(cadr false))) ,true ,(caddr false))))
							  (lint-format "perhaps ~A" caller 
								       (lists->string form `(and (not (or ,expr (not ,(cadr false)))) ,(caddr false)))))))))

					 ((case)
					  (if (and (pair? expr)
						   (cond-eqv? expr (cadr false) #t))
					      (lint-format "perhaps ~A" caller
							   (lists->string form `(case ,(cadr false)
										  ,(case-branch expr (cadr false) (list true))
										  ,@(cddr false))))))))
				   ) ; (= len 4)
				 
				 (if (pair? false)
				     (begin
				       (if (and (eq? (car false) 'if)   ; (if x 3 (if (not x) 4)) -> (if x 3 4)
						(pair? (cdr false))
						(pair? (cadr false))
						(eq? (caadr false) 'not)
						(or (equal? test (cadadr false)) (equal? expr (cadadr false)))
						(not (side-effect? test env)))
					   (lint-format "pointless repetition of if test: ~A" caller (lists->string form `(if ,expr ,true ,(caddr false)))))
				       
				       (if (and (eq? (car false) 'if)   ; (if test0 expr (if test1 expr)) -> if (or test0 test1) expr) 
						(null? (cdddr false))   ; other case is dealt with above
						(equal? true (caddr false)))
					   (let ((test1 (simplify-boolean `(or ,expr ,(cadr false)) () () env)))
					     (lint-format "perhaps ~A" caller (lists->string form `(if ,test1 ,true ,@(cdddr false))))))
				       )
				       
				     
				     (if (and (eq? false 'no-false)                         ; no false branch
					      (pair? true))
					 (begin
					   (if (pair? test)  
					       (let ((test-op (car test))
						     (true-op (car true)))
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
						       (lint-format "perhaps ~A" caller
								    (lists->string form `(set! ,target (,f ,@(cdr true)))))))))
					   
					   (if (and (eq? (car true) 'if) ; (if test0 (if test1 expr)) -> (if (and test0 test1) expr)
						    (null? (cdddr true)))
					       (let ((test1 (simplify-boolean `(and ,expr ,(cadr true)) () () env)))
						 (lint-format "perhaps ~A" caller (lists->string form `(if ,test1 ,(caddr true))))))
					   )))
				 
				 (if (and (pair? test)
					  (memq (car test) '(< <= > >= =))       ; (if (< x y) x y) -> (min x y)
					  (null? (cdddr test))
					  (member false test)
					  (member true test))
				     (if (eq? (car test) '=)                     ; (if (= x y) y x) -> y [this never happens]
					 (lint-format "perhaps ~A" caller (lists->string form false))
					 (let ((f (if (memq (car test) '(< <=))
						      (if (equal? (cadr test) true) 'min 'max)
						      (if (equal? (cadr test) false) 'min 'max))))
					   (lint-format "perhaps ~A" caller (lists->string form `(,f ,true ,false))))))
				 
				 (cond ((eq? expr #t)
					(lint-format "perhaps ~A" caller (lists->string form true)))
				       
				       ((not expr)
					(if (eq? false 'no-false)
					    (if true                             ; (if #f x) as a kludgey #<unspecified>
						(lint-format "perhaps ~A" caller (lists->string form #<unspecified>)))
					    (lint-format "perhaps ~A" caller (lists->string form false))))
				       
				       ((not (equal? true false))
					(if (boolean? true)
					    (if (boolean? false) ; !  (if expr #t #f) turned into something less verbose
						(lint-format "perhaps ~A" caller 
							     (lists->string form (if true 
										     expr 
										     (simplify-boolean `(not ,expr) () () env))))
						(when (= suggestion made-suggestion)
						  (lint-format "perhaps ~A" caller 
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
						(lint-format "perhaps ~A" caller 
							     (lists->string form (simplify-boolean
										  (if false 
										      (if (and (pair? expr) (eq? (car expr) 'not))
											  `(or ,(cadr expr) ,true) 
											  `(or (not ,expr) ,true))
										      `(and ,expr ,true))
										  () () env))))))
				       ((= len 4)
					(lint-format "if is not needed here: ~A" caller 
						     (lists->string form (if (not (side-effect? test env))
									     true
									     `(begin ,expr ,true))))))

				 (when (and (= suggestion made-suggestion)
					    (not (equal? expr test))) ; make sure the boolean simplification gets reported
				   (lint-format "perhaps ~A" caller (lists->string test expr)))

				 (if (and (pair? test)
					  (pair? true)
					  (pair? (cdr true))
					  (null? (cddr true))
					  (or (equal? test (cadr true))
					      (equal? expr (cadr true))))
				     (lint-format "perhaps ~A" caller
						  (lists->string form 
								 (if (eq? false 'no-false)
								     `(cond (,expr => ,(car true)))
								     `(cond (,expr => ,(car true)) (else ,false))))))
				 
				 (if (and (pair? true)   ; (if a (if b A B) (if b B A))
					  (pair? false)
					  (eq? (car true) 'if)
					  (eq? (car false) 'if)
					  (= (length true) (length false) 4)
					  (equal? (cadr true) (cadr false))
					  (equal? (caddr true) (cadddr false))
					  (equal? (cadddr true) (caddr false)))
				     (let* ((switch #f)
					    (a (if (and (pair? expr)
							(eq? (car expr) 'not))
						   (begin (set! switch #t) expr)
						   (simplify-boolean `(not ,expr) () () env)))
					    (b (if (and (pair? (cadr true))
							(eq? (caadr true) 'not))
						   (begin (set! switch (not switch)) (cadr true))
						   (simplify-boolean `(not ,(cadr true)) () () env))))
				       (if switch
					   (lint-format "perhaps ~A" caller
							(lists->string form `(if (eq? ,a ,b) ,(caddr false) ,(caddr true))))
					   (lint-format "perhaps ~A" caller
							(lists->string form `(if (eq? ,a ,b) ,(caddr true) ,(caddr false)))))))
				 
				 ;; --------
				 (when (and (= suggestion made-suggestion)
					    (not (= line-number last-if-line-number)))
				   ;; unravel complicated if-then-else nestings into a single cond, if possible.
				   ;;
				   ;; The (> new-len *report-nested-if*) below can mean (nearly) all nested ifs are turned into conds.
				   ;;   For a long time I thought the if form was easier to read, but now
				   ;;   I like cond better.  But cond also has serious readability issues:
				   ;;   it needs to be clearer where the test separates from the result,
				   ;;   and in a stack of these clauses, it's hard to see anything at all.
				   ;;   Maybe a different color for the test than the result?
				   ;;
				   ;; Also, the check for tree-lengths being hugely different is taken
				   ;;   from C -- I think it is easier to read a large if statement if
				   ;;   the shortest clause is at the start -- especially in a nested if where
				   ;;   it can be nearly impossible to see which dangling one-liner matches
				   ;;   which if (this even in emacs because it unmarks or doesn't remark the matching
				   ;;   paren as you're trying to scroll up to it). 

				   (define (swap-clauses form)
				     (if (not (pair? (cdddr form)))
					 form
					 (let ((expr (cadr form))
					       (true (caddr form))
					       (false (cadddr form)))
					   
					   (let ((true-n (tree-length true))
						 (false-n (if (not (pair? false)) 
							      1
							      (tree-length false))))
					     
					     (if (< false-n (/ true-n 4))
						 (let ((new-expr (simplify-boolean `(not ,expr) () () env)))
						   (if (and (pair? true)
							    (eq? (car true) 'if))
						       (set! true (swap-clauses true)))
						   (if (and (pair? true)
							    (eq? (car true) 'cond))
						       `(cond (,new-expr ,@(unbegin false))
							      ,@(cdr true))
						       `(cond (,new-expr ,@(unbegin false))
							      (else ,@(unbegin true)))))
						 (begin
						   (if (and (pair? false)
							    (eq? (car false) 'if))
						       (set! false (swap-clauses false)))
						   
						   (if (and (pair? false)
							    (eq? (car false) 'cond))
						       `(cond (,expr ,@(unbegin true))
							      ,@(cdr false))
						       `(cond (,expr ,@(unbegin true))
							      (else ,@(unbegin false))))))))))
				   
				   (let ((new-if (swap-clauses form)))
				     (if (eq? (car new-if) 'cond)
					 (if (> (length new-if) *report-nested-if*)
					     (begin
					       (set! last-if-line-number line-number)
					       (lint-format "perhaps ~A" caller (lists->string form new-if)))
					     
					     (if (= len 4) ; unneccessary?
						 (let ((true-len (tree-length (caddr form))))
						   (if (and (> true-len *report-short-branch*)
							    (< (tree-length (cadddr form)) (/ true-len *report-short-branch*)))
						       (let ((new-expr (simplify-boolean `(not ,(cadr form)) () () env)))
							 (lint-format "perhaps place the much shorter branch first~A: ~A" caller
								      (local-line-number (cadr form))
								      (truncated-lists->string form `(if ,new-expr ,false ,true))))))))))
				   ;; --------

				   (if (and (= len 4)            ; move repeated test to top, if no inner false branches
					    (pair? true)         ; (if A (if B C) (if B D)) -> (if B (if A C D))
					    (pair? false)
					    (eq? (car true) 'if)
					    (eq? (car false) 'if)
					    (equal? (cadr true) (cadr false))
					    (null? (cdddr true))
					    (null? (cdddr false)))
				       (lint-format "perhaps ~A" caller
						    (lists->string form `(if ,(cadr (caddr form))
									     (if ,expr
										 ,(caddr (caddr form))
										 ,(caddr (cadddr form)))))))

				   (if (and (= len 4)            ; move repeated start/end statements out of the if
					    (pair? true)
					    (pair? false)
					    (eq? (car true) 'begin)
					    (eq? (car false) 'begin))
				       (let ((true-len (length true))
					     (false-len (length false)))
					 (if (and (> true-len 2)
						  (> false-len 2))
					     (let ((start (if (and (equal? (cadr true) (cadr false))
								   (not (side-effect? expr env))) ; expr might affect start, so we can't pull it ahead
							      (list (cadr true))
							      ()))
						   (end (if (equal? (list-ref true (- true-len 1))
								    (list-ref false (- false-len 1)))
							    (list (list-ref true (- true-len 1)))
							    ())))
					       (when (or (pair? start)
							 (pair? end))
						 (let ((new-true (cdr true))
						       (new-false (cdr false)))
						   (when (pair? end)
						     (set! new-true (copy new-true (make-list (- true-len 2))))
						     (set! new-false (copy new-false (make-list (- false-len 2)))))
						   (when (pair? start)
						     (set! new-true (cdr new-true))
						     (set! new-false (cdr new-false)))
						   (set! new-true (if (null? (cdr new-true)) 
								      (car new-true)
								      (cons 'begin new-true)))
						   (set! new-false (if (null? (cdr new-false)) 
								       (car new-false)
								       (cons 'begin new-false)))
						   (lint-format "perhaps ~A" caller
								(lists->string form `(begin ,@start
											    (if ,expr ,new-true	,new-false)
											    ,@end)))))))))

				   (if (and (= suggestion made-suggestion) ; (if (not a) A B) -> (if a B A)
					    (not (= line-number last-if-line-number))
					    (= len 4)
					    (pair? expr)
					    (eq? (car expr) 'not)
					    (> (tree-length true) (tree-length false)))
				       (lint-format "perhaps ~A" caller
						    (lists->string form `(if ,(cadr expr) ,false ,true))))

				   ;; this happens occasionally -- scarcely worth this much code! (gather copied vars outside the if)
				   ;;   TODO: this should check that the gathered code does not affect the original test
				   (if (and (= len 4)
					    (pair? true)
					    (pair? false)
					    (eq? (car true) 'let)
					    (eq? (car false) 'let)
					    (pair? (cadr true))
					    (pair? (cadr false)))
				       (let ((true-vars (map car (cadr true)))
					     (false-vars (map car (cadr false)))
					     (shared-vars ()))
					 (for-each (lambda (v)
						     (if (and (memq v false-vars)
							      (let ((tv (assq v (cadr true)))
								    (fv (assq v (cadr false))))
								(equal? (cadr tv) (cadr fv))))
							 (set! shared-vars (cons v shared-vars))))
						   true-vars)
					 (if (pair? shared-vars)
					     ;; now remake true/false lets (maybe nil) without shared-vars
					     (let ((ntv ())
						   (nfv ())
						   (sv ()))
					       (for-each (lambda (v)
							   (if (memq (car v) shared-vars)
							       (set! sv (cons v sv))
							       (set! ntv (cons v ntv))))
							 (cadr true))
					       (set! ntv (if (or (pair? ntv)
								 (pair? (cdddr true))) ; even define is safe here because outer let blocks just as inner let used to
							     `(let (,@(reverse ntv)) ,@(cddr true))
							     (caddr true)))
					       (for-each (lambda (v)
							   (if (not (memq (car v) shared-vars))
							       (set! nfv (cons v nfv))))
							 (cadr false))
					       (set! nfv (if (or (pair? nfv)
								 (pair? (cdddr false)))
							     `(let (,@(reverse nfv)) ,@(cddr false))
							     (caddr false)))
					       (lint-format "perhaps ~A" caller
							    (lists->string form `(let (,@(reverse sv)) (if ,expr ,ntv ,nfv)))))))))

				 (if (and *report-one-armed-if*
					  (eq? false 'no-false))
				     (lint-format "perhaps ~A" caller
						  (lists->string form (if (and (pair? expr)
									       (eq? (car expr) 'not))
									  `(unless ,(cadr expr) ,@(unbegin true))
									  `(when ,expr ,@(unbegin true))))))

				 (lint-walk caller expr env)
				 (set! env (lint-walk caller true env))
				 (if (= len 4) (set! env (lint-walk caller false env)))))))
		     env))
		  
		  ;; -------- when, unless --------
		  ((when unless)
		   (if (< (length form) 3)
		       (begin
			 (lint-format "~A is messed up: ~A" caller head (truncated-list->string form))
			 env)
		       (let ((test (cadr form)))
			 (if (and (pair? test)
				  (eq? (car test) 'not))
			     (lint-format "perhaps ~A -> ~A"
					  caller 
					  (truncated-list->string form)
					  (truncated-list->string `(,(if (eq? head 'when) 'unless 'when)
								    ,(cadr test)
								    ,@(cddr form)))))
			 (if (never-false test)
			     (lint-format "~A test is never false: ~A" caller head form)
			     (if (never-true test)
				 (lint-format "~A test is never true: ~A" caller head form)))
			 (if (symbol? test)
			     (set-ref test form env)
			     (if (pair? test)
				 (lint-walk caller test env)))
			 (lint-walk-body caller head (cddr form) env))))
		  
		  ;; ---------------- cond ----------------
		  ((cond)
		   (let ((ctr 0)
			 (suggest made-suggestion)
			 (len (- (length form) 1)))
		     (if (negative? len)
			 (lint-format "cond is messed up: ~A" caller (truncated-list->string form))
			 (let ((exprs ())
			       (result :unset)
			       (has-else #f)
			       (has-combinations #f)
			       (simplifications ())
			       (prev-clause #f)
			       (all-eqv #t)
			       (eqv-select #f))

			   (let ((falses ())
				 (trues ()))
			     (for-each
			      (lambda (clause)
				(set! ctr (+ ctr 1))
				(if (not (pair? clause))
				    (begin
				      (set! all-eqv #f)
				      (set! has-combinations #f)
				      (lint-format "cond clause ~A in ~A is not a pair?" caller clause (truncated-list->string form)))
				    (begin
				      
				      (when all-eqv
					(unless eqv-select
					  (set! eqv-select (eqv-selector (car clause))))
					(set! all-eqv (and eqv-select
							   (not (and (pair? (cdr clause))
								     (eq? (cadr clause) '=>))) ; case sends selector, but cond sends test result
							   (cond-eqv? (car clause) eqv-select #t))))
				      
				      (if (and prev-clause
					       (not has-combinations)
					       (> len 2) 
					       (equal? (cdr clause) (cdr prev-clause)))
					  (if (memq (car clause) '(else #t))        ; (cond ... (x z) (else z)) -> (cond ... (else z))
					      (unless (side-effect? (car prev-clause) env)
						(lint-format "this clause could be omitted: ~A" caller prev-clause))
					      (set! has-combinations #t)))          ; handle these later
				      (set! prev-clause clause)
				      
				      (let ((expr (simplify-boolean (car clause) trues falses env))
					    (test (car clause))
					    (sequel (cdr clause)))
					(if (not (equal? expr test))
					    (set! simplifications (cons (cons clause expr) simplifications)))
					
					(cond ((memq test '(else #t))
					       (set! has-else #t)
					       
					       (if (and (pair? sequel)
							(eq? (car sequel) #<unspecified>))
						   (lint-format "this #<unspecified> is redundant: ~A" caller clause))
					       
					       (if (and (pair? sequel)        ; (cond (a A) (else (cond ...))) -> (cond (a A) ...)
							(pair? (car sequel))  ;    similarly for if, when, and unless
							(null? (cdr sequel)))
						   (case (caar sequel)
						     ((cond)
						      (lint-format "else clause could be folded into the outer cond: ~A" caller 
								   (lists->string form (append (copy form (make-list ctr)) 
											       (cdar sequel)))))
						     ((if)
						      (let ((if-expr (car sequel)))
							(lint-format "else clause could be folded into the outer cond: ~A" caller 
								     (lists->string form 
										    (append (copy form (make-list ctr)) 
											    (if (= (length if-expr) 3)
												(list (cdr if-expr))
												`((,(cadr if-expr) ,@(unbegin (caddr if-expr)))
												  (else ,@(unbegin (cadddr if-expr))))))))))
						     ((when unless)
						      (lint-format "else clause could be folded into the outer cond: ~A" caller 
								   (lists->string form 
										  (append (copy form (make-list ctr))
											  (if (eq? (caar sequel) 'when)
											      `((,(cadar sequel) ,@(cddar sequel)))
											      `(((not ,(cadar sequel)) ,@(cddar sequel)))))))))))
					      ((and (equal? test ''else)
						    (= ctr len))
					       (lint-format "odd cond clause test: is 'else supposed to be else? ~A" caller
							    (truncated-list->string clause)))
					      
					      ((and (eq? test 't)
						    (= ctr len)
						    (not (var-member 't env)))
					       (lint-format "odd cond clause test: is t supposed to be #t? ~A" caller
							    (truncated-list->string clause))))
					
					(if (never-false expr)
					    (if (not (= ctr len))
						(lint-format "cond test ~A is never false: ~A" caller (car clause) (truncated-list->string form))
						(if (not (or (memq expr '(#t else))
							     (side-effect? test env)))
						    (lint-format "cond last test could be #t: ~A" caller form)))
					    (if (never-true expr)
						(lint-format "cond test ~A is never true: ~A" caller (car clause) (truncated-list->string form))))
					
					(if (not (side-effect? test env))
					    (begin
					      (if (and (not (memq test '(else #t)))
						       (pair? sequel)
						       (null? (cdr sequel)))
						  (cond ((equal? test (car sequel))
							 (lint-format "no need to repeat the test: ~A" caller (lists->string clause (list test))))
							
							((and (pair? (car sequel))
							      (pair? (cdar sequel))
							      (null? (cddar sequel))
							      (equal? test (cadar sequel)))
							 (lint-format "perhaps use => here: ~A" caller 
								      (lists->string clause (list test '=> (caar sequel)))))
							
							((and (eq? (car sequel) #t)
							      (pair? test)
							      (not (memq (car test) '(or and)))
							      (eq? (return-type (car test) env) 'boolean?))
							 (lint-format "this #t could be omitted: ~A" caller (truncated-list->string clause)))))
					      (if (member test exprs)
						  (lint-format "cond test repeated: ~A" caller (truncated-list->string clause))
						  (set! exprs (cons test exprs)))))
					
					(if (boolean? expr)
					    (if (not expr)
						(lint-format "cond test is always false: ~A" caller (truncated-list->string clause))
						(if (not (= ctr len))
						    (lint-format "cond #t clause is not the last: ~A" caller (truncated-list->string form))))
					    (if (eq? test 'else)
						(if (not (= ctr len))
						    (lint-format "cond else clause is not the last: ~A" caller (truncated-list->string form)))
						(lint-walk caller test env)))
					
					(if (and (symbol? expr)
						 (not (var-member expr env))
						 (procedure? (symbol->value expr *e*)))
					    (lint-format "strange cond test: ~A in ~A is a procedure" caller expr clause))
					
					(if (eq? result :unset)
					    (set! result sequel)
					    (if (not (equal? result sequel))
						(set! result :unequal)))
					
					(cond ((not (pair? sequel))
					       (if (not (null? sequel))  ; (not (null?...)) here is correct -- we're looking for stray dots
						   (lint-format "cond clause is messed up: ~A" caller (truncated-list->string clause))))
					      
					      ((not (eq? (car sequel) '=>))
					       (lint-walk-body caller head sequel env))
					      
					      ((or (not (pair? (cdr sequel)))
						   (pair? (cddr sequel)))
					       (lint-format "cond => target is messed up: ~A" caller (truncated-list->string clause)))
					      
					      (else (let ((f (cadr sequel)))
						      (if (symbol? f)
							  (let ((val (symbol->value f *e*)))
							    (if (and (procedure? val)
								     (not (aritable? val 1))) ; here values might be in test expr
								(lint-format "=> target (~A) may be unhappy: ~A" caller f clause)))
							  (if (and (pair? f)
								   (eq? (car f) 'lambda)
								   (pair? (cdr f))
								   (pair? (cadr f))
								   (not (= (length (cadr f)) 1)))
							      (lint-format "=> target (~A) may be unhappy: ~A" caller f clause)))
						      (lint-walk caller f env))))
					
					(if (side-effect? expr env)
					    (begin
					      (set! falses ())
					      (set! trues ())
					      (set! result :unequal))
					    (begin
					      (if (not (member expr falses))
						  (set! falses (cons expr falses)))
					      (if (and (pair? expr)
						       (eq? (car expr) 'not)
						       (not (member (cadr expr) trues)))
						  (set! trues (cons (cadr expr) trues)))
					      (if (and (pair? expr)
						       (eq? (car expr) 'or))
						  (for-each (lambda (p) 
							      (if (not (member p falses))
								  (set! falses (cons p falses))))
							    (cdr expr)))))))))
			      (cdr form))) ; for-each clause
			   
			   (if has-else 
			       (if (pair? result) ; all result clauses are the same (and not implicit)
				   (lint-format "perhaps ~A" caller (lists->string form 
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
				     (lint-format "perhaps cond else clause is misplaced: ~A in ~A" caller last-res last-clause))))
			   
			   (if (and (= len 2)
				    (not (check-bool-cond caller form (cadr form) (caddr form) env))
				    (pair? (cadr form))   ; (cond 1 2)!
				    (pair? (caddr form))
				    (equal? (simplify-boolean (caadr form) () () env)
					    (simplify-boolean `(not ,(caaddr form)) () () env)))
			       (lint-format "perhaps ~A" caller  ; (cond ((x) y) ((not (x)) z)) -> (cond ((x) y) (else z))
					    (lists->string form `(cond ,(cadr form) (else ,@(cdaddr form))))))
			   
			   (when has-combinations
			     (let ((new-clauses ())
				   (current-clauses ()))
			       (do ((clauses (cdr form) (cdr clauses)))
				   ((null? clauses)
				    (let ((len (length new-clauses)))
				      (if (not (and (= len 2) ; i.e. don't go to check-bool-cond
						    (check-bool-cond caller form (cadr new-clauses) (car new-clauses) env)))
					  (lint-format "perhaps ~A" caller 
						       (lists->string 
							form
							(if all-eqv
							    (cond->case eqv-select (reverse new-clauses))
							    (if (and (= len 2) ; (cond (A) (B) (else C)) -> (or A B C)
								     (pair? (car new-clauses))
								     (memq (caar new-clauses) '(else #t))
								     (pair? (cadr new-clauses))
								     (pair? (caadr new-clauses))
								     (eq? (caaadr new-clauses) 'or)
								     (null? (cdadr new-clauses)))
								(if (null? (cddar new-clauses))
								    `(or ,@(cdaadr new-clauses) ,(cadar new-clauses))
								    `(or ,@(cdaadr new-clauses) (begin ,@(cdar new-clauses))))
								`(cond ,@(reverse new-clauses)))))))
				      (set! simplifications ())
				      (set! all-eqv #f)))
				 
				 (let* ((clause (car clauses))
					(result (cdr clause))) ; can be null in which case the test is the result
				   (cond ((and (pair? simplifications)
					       (assq clause simplifications))
					  => (lambda (e)
					       (set! clause (cons (cdr e) result)))))
				   (if (and (pair? (cdr clauses))
					    (equal? result (cdadr clauses)))
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
			     (lint-format "perhaps use case instead of cond: ~A" caller
					  (lists->string form (cond->case eqv-select (cdr form)))))

			   (if (and (= len 2)
				    has-else
				    (null? (cdadr form)))
			       (let ((else-clause (if (null? (cddr (caddr form)))
						      (cadr (caddr form))
						      `(begin ,@(cdr (caddr form))))))
				 (lint-format "perhaps ~A" caller (lists->string form `(or ,(caadr form) ,else-clause)))))

			   ;; --------
			   (unless (or has-combinations all-eqv)
			     ;; look for repeated ((op x c1) c2) -> ((assoc x '((c1 . c2)...)) => cdr) anywhere in the clause list
			     (let ((nc ())
				   (op #f)
				   (c #f)
				   (start #f)
				   (changed #f))

			       (define (car-with-expr cls)
				 (cond ((and (pair? simplifications)
					     (assq cls simplifications))
					=> (lambda (e)
					     (set! changed #t)
					     (cons (cdr e) (cdr cls))))
				       (else cls)))
			       
			       (define (start-search clauses test)
				 (if (code-constant? (cadr test))
				     (if (memq (car test) '(= string=? string-ci=? eq? eqv? equal? char=? char-ci=?))
					 (set! c caddr))
				     (if (code-constant? (caddr test))
					 (set! c cadr)))
				 (if c 
				     (begin
				       (set! start clauses)
				       (set! op (car test)))
				     (set! nc (cons (car-with-expr (car clauses)) nc))))
			       
			       (do ((clauses (cdr form) (cdr clauses)))
				   ((or (null? clauses)
					(not (pair? (car clauses))))
				    (if (and changed 
					     (null? clauses))
					(lint-format "perhaps ~A" caller
						     (lists->string form `(cond ,@(reverse nc))))))
				 (let* ((test (caar clauses))
					(result (cdar clauses))
					(ok-but-at-end #f)
					(looks-ok (and (pair? test)
						       (pair? (cdr test))
						       (pair? (cddr test))
						       (null? (cdddr test))
						       (pair? result)
						       (null? (cdr result))
						       (not (symbol? (car result)))
						       (or (not (pair? (car result))) ; quoted lists look bad in this context
							   (and (eq? (caar result) 'quote)
								(not (pair? (cadar result))))))))
				   (if (not start)
				       (if (and looks-ok
						(not (null? (cdr clauses))))
					   (start-search clauses test)
					   (set! nc (cons (car-with-expr (car clauses)) nc)))
				       
				       (if (or (not looks-ok)
					       (not (eq? (car test) op))
					       (not (equal? (c test) (c (caar start))))
					       (not (code-constant? ((if (eq? c cadr) caddr cadr) test)))
					       (set! ok-but-at-end (null? (cdr clauses))))
					   
					   (if (eq? (cdr start) clauses) ; only one item in the block, or two but it's just two at the end
					       (begin
						 (set! nc (cons (car start) nc))
						 (if (and looks-ok
							  (not (null? (cdr clauses))))
						     (start-search clauses test)
						     (begin
						       (set! start #f)
						       (set! nc (cons (car-with-expr (car clauses)) nc)))))
					       
					       ;; multiple hits -- can we combine them?
					       (let ((alist ())
						     (cc (if (eq? c cadr) caddr cadr)))
						 (set! changed #t)
						 (do ((sc start (cdr sc)))
						     ((if ok-but-at-end
							  (null? sc)
							  (eq? sc clauses))
						      (case op
							((eq?)         (set! nc (cons `((assq ,(c (caar start)) ',(reverse alist)) => cdr) nc)))
							((eqv? char=?) (set! nc (cons `((assv ,(c (caar start)) ',(reverse alist)) => cdr) nc)))
							((equal?)      (set! nc (cons `((assoc ,(c (caar start)) ',(reverse alist)) => cdr) nc)))
							(else          (set! nc (cons `((assoc ,(c (caar start)) ',(reverse alist) ,op) => cdr) nc)))))
						   (set! alist (cons (cons (unquoted (cc (caar sc))) (unquoted (cadar sc))) alist)))

						 (if (and looks-ok
							  (not (null? (cdr clauses))))
						     (start-search clauses test)
						     (begin
						       (set! start #f)
						       (set! nc (cons (car-with-expr (car clauses)) nc))))))))))))
			   ;; --------

			   (when (pair? (cadr form)) 
			     (if (= len 1)
				 (let ((clause (cadr form)))       ; (cond (a)) -> a, (cond (a b)) -> (if a b) etc
				   (if (null? (cdr clause))
				       (lint-format "perhaps ~A" caller (lists->string form (car clause)))
				       (if (and (not (eq? (cadr clause) '=>))
						(or (pair? (cddr clause))
						    (= suggest made-suggestion)))
					       (lint-format "perhaps ~A" caller 
							    (lists->string form 
									   (if (null? (cddr clause))
									       `(if ,(car clause) ,(cadr clause))
									       (if (and (pair? (car clause))
											(eq? (caar clause) 'not))
										   `(unless ,@(cdar clause) ,@(cdr clause))						
										   `(when ,(car clause) ,@(cdr clause)))))))))
				 (if has-else ; len > 1 here
				     (let ((last-clause (list-ref form (- len 1)))) ; not the else branch! -- just before it.
				       (if (and (= len 3)
						(equal? (cdadr form) (cdr (list-ref form len)))
						(pair? (cdaddr form))
						(not (eq? (cadr (caddr form)) '=>)))
					   (lint-format "perhaps ~A" caller
							(lists->string form
								       (let ((A (caadr form))
									     (a (cdadr form))
									     (B (caaddr form))
									     (b (cdaddr form)))
									 (let ((nexpr (simplify-boolean `(or ,A (not ,B)) () () env)))
									   (cond ((not (and (null? (cdr a))
											    (null? (cdr b))))
										  `(cond (,nexpr ,@a) (else ,@b)))

										 ((eq? (car a) #t)
										  (if (not (car b))
										      nexpr
										      (simplify-boolean `(or ,nexpr ,(car b)) () () env)))

										 ((car a)
										  `(if ,nexpr ,(car a) ,(car b)))

										 ((eq? (car b) #t)
										  (simplify-boolean `(not ,nexpr) () () env))

										 (else (simplify-boolean `(and (not ,nexpr) ,(car b)) () () env))))))))
				       (let ((arg1 (cadr form))
					     (arg2 (caddr form)))
					 (if (and (pair? arg1)
						  (pair? arg2)
						  (pair? (car arg1))
						  ;(pair? (car arg2))
						  (eq? (caar arg1) 'and)
						  (pair? (cdr arg1))
						  (null? (cddr arg1))
						  (pair? (cdr arg2))
						  (null? (cddr arg2))
						  (member (car arg2) (cdar arg1))
						  (= (length (cdar arg1)) 2))
					     (lint-format "perhaps ~A" caller
							  (lists->string form
									 `(cond (,(car arg2)
										 (if ,(if (equal? (car arg2) (cadar arg1)) (caddar arg1) (cadar arg1))
										     ,(cadr arg1)
										     ,(cadr arg2)))
										,@(cdddr form)))))

					 (if (and (= len 2)          ; (cond ((not A) B) (else C)) -> (if A C B)
						  (pair? arg1)       
						  (pair? (car arg1))
						  (eq? (caar arg1) 'not)
						  (pair? (cdr arg2))
						  (> (tree-length (cdr arg1)) (tree-length (cdr arg2))))
					     (lint-format "perhaps ~A" caller
							  (lists->string form 
									 (if (and (null? (cddr arg1))
										  (null? (cddr arg2)))
									     `(if ,(cadar arg1) ,(cadr arg2) ,(cadr arg1))
									     `(cond (,(cadar arg1) ,@(cdr arg2)) (else ,@(cdr arg1))))))))


				       (if (and (pair? last-clause)   ; (cond ... ((or ...)) (else ...)) -> (cond ... (else (or ... ...)))
						(pair? (car last-clause))
						(null? (cdr last-clause))
						(eq? (caar last-clause) 'or))
					   (let* ((e (list-ref form len))
						  (else-clause (if (null? (cddr e))
								   (cadr e)
								   `(begin ,@(cdr e)))))
					     (lint-format "perhaps ~A" caller
							  (lists->string form
									 `(cond ,@(copy (cdr form) (make-list (- len 2)))

										(else (or ,@(cdar last-clause) ,else-clause))))))))))

			     (let ((last-clause (list-ref form (if has-else (- len 1) len)))) ; not the else branch! -- just before it.
			       (if (and (pair? last-clause)          ; (cond ... (A (cond ...)) (else B)) -> (cond ... ((not A) B) ...)
					(pair? (cdr last-clause))
					(null? (cddr last-clause))
					(pair? (cadr last-clause))
					(memq (caadr last-clause) '(if cond)))
				   (let ((new-test (simplify-boolean `(not ,(car last-clause)) () () env))
					 (new-result (if has-else 
							 (cdr (list-ref form len))
							 (if (eq? form lint-mid-form) 
							     () 
							     (list #<unspecified>)))))
				     (if (eq? (caadr last-clause) 'cond)
					 (lint-format "perhaps ~A" caller
						      (lists->string form
								     `(cond ,@(copy (cdr form) (make-list (- len (if has-else 2 1))))
									    (,new-test ,@new-result)
									    ,@(cdadr last-clause))))
					 (if (= (length (cadr last-clause)) 4)
					     (let ((if-form (cadr last-clause)))
					       (lint-format "perhaps ~A" caller
							    (lists->string form
									   `(cond ,@(copy (cdr form) (make-list (- len (if has-else 2 1))))
										  (,new-test ,@new-result)
										  (,(cadr if-form) ,(caddr if-form))
										  (else ,(cadddr if-form)))))))))
			     (when (> len 2)
			       (let ((lim (if has-else (- len 2) len))
				     (tlen (tree-length form)))
				 (when (< tlen 200)
				   (set! tlen (/ tlen 4))
				   (do ((i 0 (+ i 1))
					(k (+ lim 1) (- k 1))
					(p (cdr form) (cdr p)))
				       ((or (not (pair? p))
					    (= i lim)))
				     (let ((nc (car p)))
				       (if (and (pair? nc)        
						(pair? (cdr nc))
						(null? (cddr nc))
						(pair? (cadr nc))
						(eq? (caadr nc) 'cond)
						(>= (length (cdadr nc)) (* 2 k))
						(> (tree-length nc) tlen))

					   (let ((new-test (simplify-boolean `(not ,(car nc)) () () env))
						 (new-result (if (and has-else
								      (= i (- lim 1))
								      (let ((nc1 (cadr p))
									    (nc-else (caddr p)))
									(and (null? (cddr nc1))
									     (null? (cddr nc-else)))))
								 `(if ,(caadr p) ,(cadadr p) ,(cadr (caddr p)))
								 `(cond ,@(cdr p)))))
					     (lint-format "perhaps ~A" caller
							  (lists->string form
									 `(cond ,@(copy (cdr form) (make-list i))
										(,new-test ,new-result)
										,@(cdadr nc))))))))))))))))
		     env))
		  
		  ;; ---------------- case ----------------		  
		  ((case)
		   ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		   ;; also unlike cond, only 'else marks a default branch (not #t)
		   
		   (if (< (length form) 3)
		       (lint-format "case is messed up: ~A" caller (truncated-list->string form))
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
				 (lint-format "perhaps ~A" caller 
					      (lists->string form (if (cadadr clauses)
								      (if quoted
									  `(not (,op ,selector ',keylist))
									  `(not (,op ,selector ,keylist)))
								      (if quoted
									  `(,op ,selector ',keylist)
									  `(,op ,selector ,keylist))))))))

			 (if (and (not (pair? selector))
				  (constant? selector))
			     (lint-format "case selector is a constant: ~A" caller (truncated-list->string form)))
			 (lint-walk caller selector env)
			 (if (and (pair? selector)
				  (symbol? (car selector)))
			     (begin
			       (set! sel-type (return-type (car selector) env))
			       (if (and (symbol? sel-type)
					(not (memq sel-type selector-types)))
				   (lint-format "case selector may not work with eqv: ~A" caller (truncated-list->string selector)))))
			 
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
				  (lint-format "case clause should be a list: ~A" caller (truncated-list->string clause))
				  (let ((keys (car clause))
					(exprs (cdr clause)))
				    (if (null? exprs)
					(lint-format "clause result is missing: ~A" caller clause))
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
					(lint-format "perhaps use => here: ~A" caller 
						     (lists->string clause (list keys '=> (caar exprs)))))
				    (if (pair? keys)
					(if (not (proper-list? keys))
					    (lint-format (if (null? keys) 
							     "null case key list: ~A" 
							     "stray dot in case case key list: ~A")
							 caller (truncated-list->string clause))
					    (for-each
					     (lambda (key)
					       (if (or (vector? key)
						       (string? key)
						       (pair? key)
						       (hash-table? key))
						   (lint-format "case key ~S in ~S is unlikely to work (case uses eqv?)" caller key clause))
					       (if (member key all-keys)
						   (lint-format "repeated case key ~S in ~S" caller key clause)
						   (set! all-keys (cons key all-keys)))
					       ;; unintentional quote here, as in (case x ('a b)...) never happens and
					       ;;   is hard to distinguish from (case x ((quote a) b)...) which happens a lot
					       (if (not (compatible? sel-type (->type key)))
						   (lint-format "case key ~S in ~S is pointless" caller key clause)))
					     keys))
					(if (not (eq? keys 'else))
					    (lint-format "bad case key ~S in ~S" caller keys clause)
					    (begin
					      (set! has-else clause)
					      ;; exprs: (res) or if case, ((case ...)...)
					      (if (not (= ctr len))
						  (lint-format "case else clause is not the last: ~A"
							       caller 
							       (truncated-list->string (cddr form)))
						  (and (pair? exprs)
						       (pair? (car exprs))
						       (null? (cdr exprs))         
						       (or (and (eq? (caar exprs) 'case) ; just the case statement in the else clause
								(equal? selector (cadar exprs))
								(not (side-effect? selector env))
								(set! else-foldable (cddar exprs)))
							   (and (eq? (caar exprs) 'if)   ; just if -- if foldable, make it look like it came from case
								(equal? selector (eqv-selector (cadar exprs)))
								(cond-eqv? (cadar exprs) selector #t)
								(not (side-effect? selector env))
								;; else-foldable as (((keys-from-test) true-branch) (else false-branch))
								(set! else-foldable 
								      (if (pair? (cdddar exprs))
									  `(,(case-branch (cadar exprs) selector (list (caddar exprs)))
									    (else ,(car (cdddar exprs))))
									  (list (case-branch (cadar exprs) selector (cddar exprs))))))))))))
				    (lint-walk-body caller head exprs env))))
			    (cddr form))
			   
			   (if (and has-else 
				    (pair? result)
				    (not else-foldable))
			       (begin
				 (lint-format "perhaps ~A" caller (lists->string form 
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
				   (lint-format "perhaps ~A" caller 
						(lists->string form 
							       (if (or (null? else-clause)    ; can this happen? (it's caught above as an error)
								       (null? (cdr else-clause)))
								   ()
								   (if (null? (cddr else-clause))
								       (cadr else-clause)
								       `(begin ,@(cdr else-clause))))))
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
				     (lint-format "perhaps ~A" caller 
						  (lists->string form 
								 (if (pair? else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs) ,else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs))))))))))))
		   env)
		  
		  ;; ---------------- do ----------------
		  ((do)
		   (let ((vars ()))
		     (if (not (and (>= (length form) 3)
				   (proper-list? (cadr form))
				   (proper-list? (caddr form))))
			 (lint-format "do is messed up: ~A" caller (truncated-list->string form))
			 
			 (let ((step-vars (cadr form))
			       (inner-env #f))

			   (define (var-step v) ((cdr v) 'step))

			   (if (not (side-effect? form env))
			       (let ((end+result (caddr form)))
				 (if (or (not (pair? end+result))
					 (null? (cdr end+result)))
				     (lint-format "this do-loop could be replaced by (): ~A" caller (truncated-list->string form))
				     (if (and (null? (cddr end+result))
					      (code-constant? (cadr end+result)))
					 (lint-format "this do-loop could be replaced by ~A: ~A" caller (cadr end+result) (truncated-list->string form))))))
			   
			   ;; walk the init forms before adding the step vars to env
			   (do ((bindings step-vars (cdr bindings)))
			       ((not (pair? bindings))
				(if (not (null? bindings))
				    (lint-format "do variable list is not a proper list? ~S" caller step-vars)))
			     (if (binding-ok? caller head (car bindings) env #f)
				 (begin
				   
				   (for-each (lambda (v)
					       (if (and (not (eq? (var-initial-value v) (var-name v)))
							(tree-memq (var-name v) (cadar bindings))
							(not (hash-table-ref built-in-functions (var-name v)))
							(not (tree-set-member binders (cadar bindings))))
						   (if (not (var-member (var-name v) env))
						       (lint-format "~A in ~A does not appear to be defined in the calling environment" caller
								    (var-name v) (car bindings))
						       (lint-format "~A in ~A refers to the caller's ~A, not the do-loop variable" caller
								    (var-name v) (car bindings) (var-name v)))))
					     vars)

				   (lint-walk caller (cadar bindings) env)
				   (set! vars (cons (let ((v (make-var :name (caar bindings) 
								       :definer 'do
								       :initial-value (cadar bindings))))
						      (let ((stepper (and (pair? (cddar bindings)) (caddar bindings))))
							(varlet (cdr v) :step stepper)
							(if stepper (set! (var-history v) (cons (list 'set! (caar bindings) stepper) (var-history v)))))
						      v)
						    vars)))))

			   (set! inner-env (append vars env))

			   ;; walk the step exprs
			   (let ((baddies ())) ; these are step vars used within other step vars step expressions			   
			     (do ((bindings step-vars (cdr bindings)))
				 ((not (pair? bindings)))
			       (let ((stepper (car bindings))) ; the entire binding: '(i 0 (+ i 1))
				 (when (and (binding-ok? caller head stepper env #t)
					    (pair? (cddr stepper)))
				   (let ((data (var-member (car stepper) vars)))
				     (let ((old-ref (var-ref data)))
				       (lint-walk caller (caddr stepper) inner-env)
				       (set! (var-ref data) old-ref))
				     (if (eq? (car stepper) (caddr stepper))  ; (i 0 i) -> (i 0)
					 (lint-format "perhaps ~A" caller (lists->string stepper (list (car stepper) (cadr stepper)))))
				     ;; pointless caddr here happens very rarely
				     (set! (var-set data) (+ (var-set data) 1))) ; (pair? cddr) above
				   (when (and (pair? (caddr stepper))
					      (not (eq? (car stepper) (cadr stepper))) ; (lst lst (cdr lst))
					      (eq? (car (caddr stepper)) 'cdr)
					      (eq? (cadr stepper) (cadr (caddr stepper))))
				     (lint-format "this looks suspicious: ~A" caller stepper))
				   (for-each (lambda (v)
					       (if (and (not (eq? (var-name v) (car stepper)))
							(or (eq? (var-name v) (caddr stepper))
							    (and (pair? (caddr stepper))
								 (tree-member (var-name v) (caddr stepper)))))
						   (set! baddies (cons (car stepper) baddies))))
					     vars))))

			     (when (pair? baddies)
			       ;(format *stderr* "~A: ~A~%" baddies form)
			       ;; (do ((i 0 j) (j ...))...) is unreadable -- which (binding of) j is i set to?
			       ;; but this is tricky if there is more than one such variable -- if cross links, we'll need named let
			       ;; (do ((i 0 j) (j 1 i) (k 0 (+ k 1))) ((= k 4)) (format *stderr* "~A ~A~%" i j))
			       ;; (let __1__ ((i 0) (j 1) (k 0)) (if (= k 4) () (begin (format *stderr* "~A ~A~%" i j) (__1__ j i (+ k 1)))))
			       (let ((new-steppers (map (lambda (stepper)
							  (if (memq (car stepper) baddies)
							      `(,(car stepper) ,(cadr stepper))
							      stepper))
							step-vars))
				     (new-sets (map (lambda (stepper)
						      (if (memq (car stepper) baddies)
							  `(set! ,(car stepper) ,(caddr stepper))
							  (values)))
						    step-vars)))
				 (if (or (null? (cdr baddies))
					 (let ((trails new-sets))
					   (not (any? (lambda (v)     ; for each baddy, is it used in any following set!?
							(and (pair? (cdr trails))
							     (set! trails (cdr trails))
							     (tree-member v trails)))
						      (reverse baddies)))))
				     (lint-format "perhaps ~A" caller
						  (lists->string form
								 `(do ,new-steppers
								      ,(caddr form)
								    ,@(cdddr form)
								    ,@new-sets)))
				     (let* ((loop (find-unique-name form #f))
					    (test (if (pair? (caddr form))
						      (caaddr form)
						      ()))
					    (result (if (not (and (pair? (caddr form))
								  (pair? (cdaddr form))))
							()
							(if (null? (cdr (cdaddr form)))
							    (car (cdaddr form))
							    `(begin ,@(cdaddr form)))))
					    (let-loop `(,loop ,@(map (lambda (s)
								       (if (pair? (cddr s))
									   (caddr s)
									   (car s)))
								     step-vars)))
					    (new-body (if (pair? (cdddr form))
							  `(begin ,@(cdddr form) ,let-loop)
							  let-loop)))
				       (lint-format "this do loop is unreadable; perhaps ~A" caller
						  (lists->string form
								 `(let ,loop ,(map (lambda (s)
										     (list (car s) (cadr s)))
										   step-vars)
								    (if ,test ,result ,new-body)))))))))

			   ;; walk the body and end stuff (it's too tricky to find infinite do loops)
			   (if (pair? (caddr form))
			       (let ((end+result (caddr form)))
				 (if (pair? end+result)
				     (let ((end (car end+result)))
				       (lint-walk caller end inner-env) ; this will call simplify-boolean
				       (if (pair? (cdr end+result))
					   (if (null? (cddr end+result))
					       (lint-walk caller (cadr end+result) inner-env)
					       (lint-walk-body caller 'do-result (cdr end+result) inner-env)))
				       (if (and (symbol? end) (memq end '(= > < >= <= null? not)))
					   (lint-format "perhaps missing parens: ~A" caller end+result))
				       
				       (cond ((never-false end)
					      (lint-format "end test is never false: ~A" caller end))

					     (end ; it's not #f
					      (if (never-true end)
						  (lint-format "end test is never true: ~A" caller end)
						  (let ((v (and (pair? end)
								(memq (car end) '(< > <= >=))
								(pair? (cdr end))
								(symbol? (cadr end))
								(var-member (cadr end) vars))))
						    ;; if found, v is the var info
						    (when (pair? v)
						      (let ((step (var-step v)))
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
								  (lint-format "do step looks like it doesn't match end test: ~A" caller 
									       (lists->string step end)))))))))))
					     ((pair? (cdr end+result))
					      (lint-format "result is unreachable: ~A" caller end+result)))

				       (if (and (symbol? end)
						(not (var-member end env))
						(procedure? (symbol->value end *e*)))
					   (lint-format "strange do end-test: ~A in ~A is a procedure" caller end end+result))))))

			   (lint-walk-body caller head (cdddr form) inner-env)

			   ;; before report-usage, check for unused variables, and don't complain about them if
			   ;;   they are referenced in an earlier step expr?(!)
			   (do ((v vars (cdr v)))
			       ((null? v))
			     (let ((var (car v)))
			       (when (zero? (var-ref var))
				 ;; var was not seen in the end+result/body or any subsequent step exprs
				 ;;   vars is reversed order, so we need only scan var-step of the rest
				 
				 (if (side-effect? (var-step var) env)
				     (set! (var-ref var) (+ (var-ref var) 1))
				     (for-each
				      (lambda (nv)
					(if (or (eq? (var-name var) (var-step nv))
						(and (pair? (var-step nv))
						     (tree-member (var-name var) (var-step nv))))
					    (set! (var-ref var) (+ (var-ref var) 1))))
				      (cdr v))))))
			   (report-usage caller head vars inner-env)
			   
			   ;; look for constant expressions in the do body
			   (let ((constant-exprs (find-constant-exprs 'do (map var-name vars) (cdddr form))))
			     (if (pair? constant-exprs)
				 (if (null? (cdr constant-exprs))
				     (lint-format "in ~A, ~A appears to be constant" caller
						  (truncated-list->string form)
						  (car constant-exprs))
				     (lint-format "in ~A, the following expressions appear to be constant:~%~NC~A" caller
						  (truncated-list->string form)
						  (+ lint-left-margin 4) #\space
						  (format #f "~{~A~^, ~}" constant-exprs)))))

			   ;; check for do-loop as copy/fill! stand-in and other similar cases
			   (when (and (pair? vars)
				      (null? (cdr vars)))
			     (let ((end-test (and (pair? (caddr form)) (caaddr form)))
				   (first-var (caadr form))
				   (body (cdddr form))
				   (setv #f))
			       (when (and (pair? end-test)
					  (pair? body)
					  (null? (cdr body))
					  (pair? (car body)) 
					  (memq (car end-test) '(>= =)))
				 (let ((vname (car first-var))
				       (start (cadr first-var))
				       (step (and (pair? (cddr first-var))
						  (caddr first-var)))
				       (end (caddr end-test)))
				   (when (and step
					      (pair? step)
					      (eq? (car step) '+)
					      (memq vname step)
					      (memv 1 step)
					      (null? (cdddr step))
					      (or (eq? (cadr end-test) vname)
						  (and (eq? (car end-test) '=)
						       (eq? (caddr end-test) vname)
						       (set! end (cadr end-test)))))
				     ;; we have (do ((v start (+ v 1)|(+ 1 v))) ((= v end)|(= end v)|(>= v end)) one-statement)
				     (set! body (car body))
				     ;; write-char is the only other common case here -> write-string in a few cases
				     (when (and (memq (car body) '(vector-set! float-vector-set! int-vector-set! list-set! string-set! byte-vector-set!))
						;; integer type check here isn't needed because we're using this as an index below
						;;   the type error will be seen in report-usage if not earlier
						(eq? (caddr body) vname)
						(let ((val (cadddr body)))
						  (set! setv val)
						  (or (code-constant? val)
						      (and (pair? val)
							   (memq (car val) '(vector-ref float-vector-ref int-vector-ref list-ref string-ref byte-vector-ref))
							   (eq? (caddr val) vname)))))
				       (lint-format "perhaps ~A" caller 
						    (lists->string form 
								   (if (code-constant? setv)
								       `(fill! ,(cadr body) ,(cadddr body) ,start ,end)
								       `(copy ,(cadr setv) ,(cadr body) ,start ,end))))))))))))
		     env))
		  
		  ;; ---------------- let ----------------
		  ((let)
		   (if (or (< (length form) 3)
			   (not (or (symbol? (cadr form))
				    (list? (cadr form)))))
		       (lint-format "let is messed up: ~A" caller (truncated-list->string form))
		       (let ((named-let (and (symbol? (cadr form)) (cadr form))))
			 (if (keyword? named-let)
			     (lint-format "bad let name: ~A" caller named-let))
			 (unless named-let
			   (if (and (null? (cadr form)) ; this can be fooled by macros that define things
				    (eq? form lint-current-form)
				    (not (tree-set-member '(call/cc call-with-current-continuation lambda lambda* define define* 
							    define-macro define-macro* define-bacro define-bacro* define-constant define-expansion
							    load eval eval-string require)
							  (cddr form))))
			       (lint-format "otiose let: ~A" caller (truncated-list->string form))

			       (let ((body (cddr form)))
				 (if (and (null? (cdr body))
					  (pair? (car body))
					  (memq (caar body) '(let let*)))
				     (if (null? (cadr form))
					 (lint-format "pointless let: ~A" caller (lists->string form (car body)))
					 (if (null? (cadar body))
					     (lint-format "pointless let: ~A" caller (lists->string form `(let ,(cadr form) ,@(cddar body))))))))))

			 (let ((vars (if (and named-let 
					      (not (keyword? named-let))
					      (or (null? (caddr form))
						  (and (proper-list? (caddr form))
						       (every? pair? (caddr form)))))
					 (list (make-fvar :name named-let 
							  :ftype head
							  :decl (dummy-func caller form (list 'define (cons '_ (map car (caddr form))) #f))
							  :arglist (map car (caddr form))
							  :initial-value form
							  :env env))
					 ()))
			       (varlist (if named-let (caddr form) (cadr form)))
			       (body (if named-let (cdddr form) (cddr form))))
			   
			   (if (not (list? varlist))
			       (lint-format "let is messed up: ~A" caller (truncated-list->string form))
			       (if (and (null? varlist)
					(pair? body)
					(null? (cdr body))
					(not (side-effect? (car body) env)))
				   (lint-format "perhaps ~A" caller (lists->string form (car body)))))
			   
			   (do ((bindings varlist (cdr bindings)))
			       ((not (pair? bindings))
				(if (not (null? bindings))
				    (lint-format "let variable list is not a proper list? ~S" caller varlist)))
			     (if (binding-ok? caller head (car bindings) env #f)
				 (let ((val (cadar bindings)))
				   (if (and (pair? val)
					    (eq? 'lambda (car val))
					    (tree-car-member (caar bindings) val)
					    (not (var-member (caar bindings) env)))
				       (lint-format "let variable ~A is called in its binding?  Perhaps let should be letrec: ~A"
						    caller (caar bindings) 
						    (truncated-list->string bindings))
				       (unless named-let
					 (for-each (lambda (v)
						     (if (and (tree-memq (var-name v) (cadar bindings))
							      (not (hash-table-ref built-in-functions (var-name v)))
							      (not (tree-set-member binders (cadar bindings))))
							 (if (not (var-member (var-name v) env))
							     (lint-format "~A in ~A does not appear to be defined in the calling environment" caller
									  (var-name v) (car bindings))
							     (lint-format "~A in ~A refers to the caller's ~A, not the let variable" caller
									  (var-name v) (car bindings) (var-name v)))))
						   vars)))
				   (lint-walk caller val env)
				   (set! vars (cons (make-var :name (caar bindings) 
							      :initial-value val 
							      :definer (if named-let 'named-let 'let))
						    vars)))))

			   (when (and (pair? varlist)        ; (let ((x (A))) (if x (f x) B)) -> (cond ((A) => f) (else B)
				      (pair? (car varlist))  ;   ^ this happens a lot, so it's worth this tedious search
				      (null? (cdr varlist))  ;   also (let ((x (A))) (cond (x (f x))...)
				      (pair? body)
				      (null? (cdr body))
				      (pair? (cdar varlist))
				      (pair? (cadar varlist)))
			     (let ((p (car body))
				   (vname (caar varlist))
				   (vvalue (cadar varlist)))

			       (when (and (not named-let)   ; (let ((x (assq a y))) (set! z (if x (cadr x) 0))) -> (set! z (cond ((assq a y) => cadr) (else 0)))
					  (pair? p)
					  (not (memq (car p) '(if cond))))
				 (if (= (tree-count2 vname p 0) 2)
				     (do ((i 0 (+ i 1))
					  (bp (cdr p) (cdr bp)))
					 ((or (null? bp)
					      (let ((b (car bp)))
						(and (pair? b)
						     (eq? (car b) 'if)
						     (= (tree-count2 vname b 0) 2)
						     (eq? vname (cadr b))
						     (pair? (caddr b))
						     (pair? (cdaddr b))
						     (null? (cddr (caddr b)))
						     (eq? vname (cadr (caddr b))))))
					  (if (pair? bp)
					      (let ((else-clause (if (pair? (cdddar bp)) `((else ,@(cdddar bp))) ())))
						(lint-format "perhaps ~A" caller
							     (lists->string form `(,@(copy p (make-list (+ i 1)))
										   (cond (,vvalue => ,(caaddr (car bp))) ,@else-clause)
										 ,@(cdr bp))))))))))

			       (when (and (pair? p)
					  (pair? (cdr p)))
				 (when (and (eq? (car p) 'cond) ; (let ((x (f y))) (cond (x (g x)) ...)) -> (cond ((f y) => g) ...)
					    (pair? (cadr p))
					    (eq? (caadr p) vname)
					    (pair? (cdadr p))
					    (null? (cddadr p))
					    (or (and (pair? (cadadr p))
						     (pair? (cdr (cadadr p)))
						     (null? (cddr (cadadr p))) ; one arg to func
						     (eq? vname (cadr (cadadr p))))
						(eq? vname (cadadr p)))
					    (or (null? (cddr p))
						(not (tree-member vname (cddr p)))))
				   (if (eq? vname (cadadr p))
				       (if (and (pair? (cddr p))
						(pair? (caddr p))
						(memq (caaddr p) '(else #t t)))
					   (lint-format "perhaps ~A" caller 
							(lists->string form (if (null? (cddr (caddr p)))
										`(or ,vvalue ,(cadr (caddr p)))
										`(or ,vvalue (begin ,@(cdaddr p))))))
					   (lint-format "perhaps ~A" caller (lists->string form `(or ,vvalue 
												   (cond ,@(cddr p))))))
				       (lint-format "perhaps ~A" caller (lists->string form `(cond (,vvalue => ,(caadr (cadr p))) 
												   ,@(cddr p))))))
				 
				 (when (and (null? (cddr p))    ; (let ((x (+ y 1))) (abs x)) -> (abs (+ y 1))
					    (eq? vname (cadr p))
					    (let ((v (var-member (car p) env)))
					      (if (var? v)
						  (memq (var-definer v) '(define define* lambda lambda*))
						  (hash-table-ref built-in-functions (car p)))))
				   ;; we'd get many more hits here if we used (not (any-macro? (car p) env))
				   ;;   because undefined cases would assume they were not macros (I think)
				   (lint-format "perhaps ~A" caller
						(lists->string form `(,(car p) ,vvalue))))

				 (when (pair? (cddr p))
				   (when (and (pair? (cdddr p))
					      (eq? (car p) 'if))

				     (when (and (eq? (cadr p) vname) ; (let ((x (g y))) (if x #t #f)) -> (g y)
						(boolean? (caddr p))
						(boolean? (cadddr p))
						(not (eq? (caddr p) (cadddr p))))
				       (lint-format "perhaps ~A" caller
						    (lists->string form (if (caddr p) vvalue `(not ,vvalue)))))
					
				     (when (and (pair? (cadr p)) ; (let ((x (f y))) (if (not x) B (g x))) -> (cond ((f y) => g) (else B))
						(eq? (caadr p) 'not)
						(eq? (cadadr p) vname)
						(pair? (cadddr p))
						(pair? (cdr (cadddr p)))
						(null? (cddr (cadddr p)))
						(eq? vname (cadr (cadddr p))))
				       (let ((else-clause (if (eq? (caddr p) vname)
							      `((else #f))
							      (if (and (pair? (caddr p))
								       (tree-member vname (caddr p)))
								  :oops! ; if the let var appears in the else portion, we can't do anything with =>
								  `((else ,(caddr p)))))))
					 (unless (eq? else-clause :oops!)
					   (lint-format "perhaps ~A" caller (lists->string form `(cond (,vvalue => ,(car (cadddr p))) ,@else-clause)))))))

				   (let ((crf #f))
				     ;; all this stuff still misses (cond ((not x)...)) and (set! y (if x (cdr x)...)) i.e. need embedding in this case
				     (when (and (or (and (memq (car p) '(if and)) ; (let ((x (f y))) (and x (g x))) -> (cond ((f y) => g) (else #f))
							 (eq? (cadr p) vname))
						    (and (eq? (car p) 'or)
							 (equal? (cadr p) `(not ,vname)))
						    (and (pair? vvalue)
							 (memq (car vvalue) '(assoc assv assq member memv memq))
							 (pair? (cadr p))
							 (or (eq? (caadr p) 'pair?)
							     (and (eq? (caadr p) 'null?)
								  (lint-format "in ~A, ~A can't be null because ~A in ~A only returns #f or a pair" 
									       caller p vname (car vvalue) (truncated-list->string (car varlist)))
								  #f))
							 (eq? (cadadr p) vname)))
						(or (and (pair? (caddr p))
							 (pair? (cdaddr p))
							 (null? (cddr (caddr p))) ; one func arg
							 (or (eq? vname (cadr (caddr p)))
							     (and (memq (caaddr p) '(car cdr caar cadr cddr cdar caaar caadr caddr 
										     cdddr cdaar cddar cadar cdadr cadddr cddddr))
								  ((lambda* (cr arg) ; lambda* not lambda because combine-cxrs might return just #f
								     (and cr
									  (< (length cr) 5) 
									  (eq? vname arg)
									  (set! crf (string->symbol (string-append "c" cr "r")))))
								   (combine-cxrs (caddr p))))))
						    (and (eq? (car p) 'if)
							 (eq? (caddr p) vname)
							 (not (tree-member vname (cdddr p)))
							 (lint-format "perhaps ~A" caller 
								      (lists->string form 
										     (if (null? (cdddr p))
											 vvalue
											 `(or ,vvalue ,(cadddr p)))))
							 #f))
						(pair? (caddr p))
						(or (eq? (car p) 'if)
						    (null? (cdddr p))))
				       (let ((else-clause (cond ((pair? (cdddr p))
								 (if (eq? (cadddr p) vname)
								     `((else #f)) ; this stands in for the local var
								     (if (and (pair? (cadddr p))
									      (tree-member vname (cadddr p)))
									 :oops! ; if the let var appears in the else portion, we can't do anything with =>
									 `((else ,(cadddr p))))))
								((eq? (car p) 'and)
								 `((else #f)))
								((eq? (car p) 'or)
								 `((else #t)))
								(else ()))))
					 (unless (eq? else-clause :oops!)
					   (lint-format "perhaps ~A" caller 
							(lists->string form `(cond (,vvalue => ,(or crf (caaddr p))) ,@else-clause))))))))
				 )))
			   
			   (let* ((cur-env (append vars env))
				  (e (lint-walk-body (or named-let caller) head body cur-env))
				  (nvars (if (null? cur-env)
					     e
					     (and (not (eq? e cur-env))
						  (env-difference caller e cur-env ())))))
			     (if (pair? nvars)
				 (if (eq? (var-name (car nvars)) lambda-marker)
				     (begin
				       (set! env (cons (car nvars) env))
				       (set! nvars (cdr nvars)))
				     (set! vars (append nvars vars))))
			     (report-usage caller head vars cur-env))

			   ;; look for exprs replaceable with vars
			   (unless named-let
			     (find-let-constant-exprs caller form vars body))

			   (if (and (pair? body)                ; (let ((x y)) x) -> y
				    (null? (cdr body))
				    (pair? varlist)             ; (let ()...)
				    (pair? (car varlist))       ; (let (x) ...)
				    (not (pair? (car body)))
				    (eq? (car body) (caar varlist))
				    (null? (cdr varlist))
				    (pair? (cdar varlist))) ; (let ((a))...)
			       (lint-format "perhaps ~A" caller (lists->string form (cadar varlist))))

			   (when (and (not named-let)           ; (let ((x 0)...) (set! x 1)...) -> (let ((x 1)...)...)
				      (pair? body)              ; this almost never happens in let*
				      (pair? (car body))
				      (pair? (cdar body))
				      (eq? (caar body) 'set!)
				      (pair? (cddar body))
				      (not (tree-memq 'curlet (caddar body)))
				      (cond ((assq (cadar body) vars)
					     => (lambda (v)
						  (or (and (code-constant? (var-initial-value v))
							   (code-constant? (caddar body)))
						      (not (any? (lambda (v1)
								   (or (tree-memq (car v1) (caddar body))
								       (side-effect? (cadr v1) env)))
								 varlist)))))
					    (else #f)))
			     (if (null? (cdr body)) ; this only happens in test suites...
				 (lint-format "perhaps ~A" caller
					      (lists->string form (if (null? (cdr varlist))
								      (caddar body)
								      `(let ,(map (lambda (v) (if (eq? (car v) (cadar body)) (values) v)) varlist)
									 ,(caddar body)))))
				 (lint-format "perhaps ~A" caller
					      (lists->string form
							     `(let ,(map (lambda (v)
									   (if (eq? (car v) (cadar body))
									       (list (car v) (caddar body))
									       v))
									 varlist)
								,@(if (null? (cddr body))
								      (cdr body)
								      `(,(cadr body) ...)))))))			   
			   (when (and (not named-let)
				      (pair? varlist)
				      (> (length body) 3)
				      (every? pair? varlist)
				      (not (tree-set-car-member '(define define* define-macro define-macro* 
								  define-bacro define-bacro* define-constant define-expansion) 
							    body)))
			     ;; define et al are like a continuation of the let bindings, so we can't restrict them by accident
			     ;;   (let ((x 1)) (define y x) ...)
			     (let ((last-refs (map (lambda (v) (vector (var-name v) #f 0 v)) vars))
				   (got-lambdas (tree-set-car-member '(lambda lambda*) body)))
			           ;; (let ((x #f) (y #t)) (set! x (lambda () y)) (set! y 5) (x))
			       (do ((p body (cdr p))
				    (i 0 (+ i 1)))
				   ((null? p)
				    (let ((end 0)
					  (len i))
				      (for-each (lambda (v)
						  (set! end (max end (v 2))))
						last-refs)
				      (if (and (< end (/ len lint-let-reduction-factor)) ; maybe we need tree-length here
					       (eq? form lint-current-form))

					  (lint-format "this let could be tightened:~%~NC~A ->~%~NC~A~%~NC~A ..." caller
						       (+ lint-left-margin 4) #\space
						       (truncated-list->string form)
						       (+ lint-left-margin 4) #\space
						       (let ((old-pp ((funclet 'lint-pretty-print) '*pretty-print-left-margin*)))
							 (set! ((funclet lint-pretty-print) '*pretty-print-left-margin*) (+ lint-left-margin 4))
							 (let ((res (lint-pp `(let ,(cadr form) 
										,@(copy body (make-list (+ end 1)))))))
							   (set! ((funclet lint-pretty-print) '*pretty-print-left-margin*) old-pp)
							   res))
						       (+ lint-left-margin 4) #\space
						       (lint-pp (list-ref body (+ end 1))))
					  
					  (let ((mnv ())
						(cur-end len))
					    (for-each (lambda (v)
							(when (and (or (null? mnv)
								       (<= (v 2) cur-end))
								   (positive? (var-ref (v 3)))
								   (let ((expr (var-initial-value (v 3))))
								     (not (any? (lambda (ov) ; watch out for shadowed vars
										  (tree-memq (car ov) expr))
										varlist))))
							  (set! mnv (if (= (v 2) cur-end)
									(cons v mnv)
									(list v)))
							  (set! cur-end (v 2))))
						      last-refs)
					    (when (and (pair? mnv)
						       (< cur-end (/ len lint-let-reduction-factor))
						       (> (- len cur-end) 3))
					      ;; mnv is in the right order because last-refs is reversed
					      (lint-format "the scope of ~{~A~^, ~} could be reduced: ~A" caller 
							   (map (lambda (v) (v 0)) mnv)
							   (lists->string form
									  `(let ,(map (lambda (v)
											(if (member (car v) mnv (lambda (a b) (eq? a (b 0))))
											    (values)
											    v))
										      varlist)
									     (let ,(map (lambda (v)
											  (list (v 0) (var-initial-value (v 3))))
											mnv)
									       ,@(copy body (make-list (+ cur-end 1))))
									     ,(list-ref body (+ cur-end 1))
									     ...))))))))
				 (if (and (not got-lambdas)
					  (pair? (car p))
					  (pair? (cdr p))
					  (eq? (caar p) 'set!)
					  (var-member (cadar p) vars)
					  (not (tree-memq (cadar p) (cdr p))))
				     (if (not (side-effect? (caddar p) env))
					 (lint-format "~A in ~A could be omitted" caller (car p) (truncated-list->string form))
					 (lint-format "perhaps ~A" caller (lists->string (car p) (caddar p)))))

				 (for-each (lambda (v)
					     (when (tree-memq (v 0) (car p))
					       (set! (v 2) i)
					       (if (not (v 1)) (set! (v 1) i))))
					   last-refs))))
			   
			   ;; maybe more code than this is worth -- combine lets
			   (if (and (pair? (cadr form))
				    (pair? (cddr form))
				    (null? (cdddr form))
				    (pair? (caddr form))
				    (eq? (caaddr form) 'let)
				    (pair? (cdr (caddr form)))
				    (pair? (cadr (caddr form))))
			       (let ((inner (caddr form)))
				 
				 (define (letstar . lets)
				   (let loop ((vars (list 'curlet)) (forms lets))
				     (and (pair? forms)
					  (or (and (pair? (car forms))
						   (or (tree-set-member vars (car forms))
						       (any? (lambda (a) 
							       (or (not (pair? a))
								   (not (pair? (cdr a))) 
								   (side-effect? (cadr a) env)))
							     (car forms))))
					      (loop (append (map car (car forms)) vars) 
						    (cdr forms))))))
				 
				 (if (and (pair? (cddr inner))
					  (pair? (caddr inner))
					  (null? (cdddr inner))
					  (eq? (caaddr inner) 'let)
					  (pair? (cdr (caddr inner)))
					  (pair? (cadr (caddr inner))))
				     (let ((inner1 (caddr inner)))
				       (if (and (pair? (cddr inner1))
						(null? (cdddr inner1))
						(pair? (caddr inner1))
						(eq? (caaddr inner1) 'let)
						(pair? (cdr (caddr inner1)))
						(pair? (cadr (caddr inner1))))
					   (let ((inner2 (caddr inner1)))
					     (if (not (letstar (cadr form)
							       (cadr inner)
							       (cadr inner1)
							       (cadr inner2)))
						 (lint-format "perhaps ~A" caller
							      (lists->string form
									     `(let (,@(cadr form)
										    ,@(cadr inner)
										    ,@(cadr inner1)
										    ,@(cadr inner2))
										,@(cddr inner2))))))
					   (if (not (letstar (cadr form)
							     (cadr inner)
							     (cadr inner1)))
					       (lint-format "perhaps ~A" caller
							    (lists->string form
									   `(let (,@(cadr form)
										  ,@(cadr inner)
										  ,@(cadr inner1))
									      ,@(cddr inner1)))))))
				     (if (not (letstar (cadr form)
						       (cadr inner)))
					 (lint-format "perhaps ~A" caller
						      (lists->string form
								     `(let (,@(cadr form)
									    ,@(cadr inner))
									,@(cddr inner))))))))

			   ))) ; messed up let
		   env)
		  
		  ;; ---------------- let* ----------------		  	
		  ((let*)
		   (if (< (length form) 3)
		       (lint-format "let* is messed up: ~A" caller (truncated-list->string form))
		       (let ((named-let (and (symbol? (cadr form)) (cadr form))))

			 (let ((vars (if named-let (list (make-var :name named-let 
								   :definer 'let*)) ())) ; TODO: fvar
			       (varlist (if named-let (caddr form) (cadr form)))
			       (body (if named-let (cdddr form) (cddr form))))
			   (if (not (list? varlist))
			       (lint-format "let* is messed up: ~A" caller (truncated-list->string form)))

			   (let ((side-effects #f))
			     (do ((bindings varlist (cdr bindings)))
				 ((not (pair? bindings))
				  (if (not (null? bindings))
				      (lint-format "let* variable list is not a proper list? ~S" 
						   caller (if named-let (caddr form) (cadr form)))))
			       (if (binding-ok? caller head (car bindings) env #f)
				   (begin
				     (if (not (or (eq? bindings varlist)
						  ;; first var side-effect is innocuous (especially if it's the only one!)
						  ;;    does this need to protect against a side-effect that the next var accesses?
						  ;;    I think we're ok -- the accessed var must be exterior, and we go down in order
						  side-effects))
					 (set! side-effects (side-effect? (cadar bindings) env)))
				     (lint-walk caller (cadar bindings) (append vars env))
				     (set! vars (cons (make-var :name (caar bindings) 
								:initial-value (cadar bindings) 
								:definer (if named-let 'named-let* 'let*))
						      vars)))))
			     (if (not (or side-effects
					  (any? (lambda (v) (positive? (var-ref v))) vars)))
				 (lint-format "let* could be let: ~A" caller (truncated-list->string form))))
			   
			   ;; in s7, let evaluates var values top down, so this message is correct
			   ;;   even in cases like (let ((ind (open-sound...)) (mx (maxamp))) ...)
			   ;; in r7rs, the order is not specified (section 4.2.2 of the spec), so
			   ;;   here we would restrict this message to cases where there is only
			   ;;   one variable, or where subsequent values are known to be independent.
			   ;; if each function could tell us what globals it depends on or affects,
			   ;;   we could make this work in all cases.
			   
			   (let* ((cur-env (append vars env))
				  (e (lint-walk-body caller head body cur-env))
				  (nvars (and (not (eq? e cur-env))
					      (env-difference caller e cur-env ()))))
			     (if (pair? nvars)
				 (if (eq? (var-name (car nvars)) lambda-marker)
				     (begin
				       (set! env (cons (car nvars) env))
				       (set! nvars (cdr nvars)))
				     (set! vars (append nvars vars))))
			   
			     (report-usage caller head vars cur-env))

			   
			   ;; look for exprs replaceable with vars
			   (unless named-let
			     (find-let-constant-exprs caller form vars body))

			   (when (and (pair? varlist)        ; (let* (...(x A)) (if x (f A) B)) -> (let(*) (...) (cond (A => f) (else B)))
				      (pair? body)
				      (null? (cdr body)))
			     (let* ((varlen (length varlist))
				    (var (and (positive? varlen)
					      (varlist (- varlen 1)))))
			       
			       (when (and (pair? var)
					  (pair? (cdr var)))
				 (let ((p (car body)))
				   (if (and (pair? p)
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
								 (if (eq? (cadddr p) (car var))
								     `((else #f)) ; this stands in for the local var
								     (if (and (pair? (cadddr p))
									      (tree-member (car var) (cadddr p)))
									 :oops! ; if the let var appears in the else portion, we can't do anything with =>
									 `((else ,(cadddr p))))))
								((eq? (car p) 'and)
								 `((else #f)))
								((eq? (car p) 'or)
								 `((else #t)))
								(else ()))))
					 (if (not (eq? else-clause :oops!))
					     (case varlen
					       ((1) (lint-format "perhaps ~A" caller 
								 (lists->string form `(cond (,(cadr var) => ,(caaddr p)) 
											    ,@else-clause))))
					       ((2) (lint-format "perhaps ~A" caller 
								 (lists->string form `(let (,(car varlist))
											(cond (,(cadr var) => ,(caaddr p)) 
											      ,@else-clause)))))
					       (else (lint-format "perhaps ~A" caller 
								  (lists->string form `(let* (,@(copy varlist (make-list (- varlen 1))))
											 (cond (,(cadr var) => ,(caaddr p)) 
											       ,@else-clause)))))))))))))
			   (if (and (pair? body)                ; same as let: (let* ((x y)) x) -> y
				    (null? (cdr body))
				    (pair? varlist)             ; (let* ()...)
				    (pair? (car varlist))       ; (let* (x) ...)
				    (not (pair? (car body))))
			       (if (and (eq? (car body) (caar varlist))
					(null? (cdr varlist))
					(pair? (cdar varlist))) ; (let* ((a...)) a)
				   (lint-format "perhaps ~A" caller (lists->string form (cadar varlist)))
				   (let* ((len (length varlist))
					  (last-var (and (positive? len)
							 (list-ref varlist (- len 1)))))
				     (if (and (> len 1)         ; (let* (... (x y)) x) -> (let(*)(...) y)
					      (pair? last-var)
					      (pair? (cdr last-var))
					      (null? (cddr last-var))
					      (eq? (car body) (car last-var)))
					 (lint-format "perhaps ~A" caller 
						      (lists->string form `(,(if (= len 2) 'let 'let*)
									    ,(copy varlist (make-list (- len 1)))
									    ,(cadr last-var))))))))
			   (when (and (not named-let)
				      (> (length body) 3)
				      (> (length vars) 1)
				      (every? pair? varlist)
				      (not (tree-set-car-member '(define define* define-macro define-macro* 
								  define-bacro define-bacro* define-constant define-expansion)
							    body)))
			     (let ((last-ref (vector (var-name (car vars)) #f 0 (car vars))))
			       (do ((p body (cdr p))
				    (i 0 (+ i 1)))
				   ((null? p)
				    (let ((len i))
				      (if (and (< (last-ref 2) (/ len lint-let-reduction-factor))
					       (> (- len (last-ref 2)) 3))
					  (lint-format "the scope of ~A could be reduced: ~A" caller 
						       (last-ref 0)
						       (lists->string form
								      `(,(if (> (length vars) 2) 'let* 'let) ,(copy varlist (make-list (- (length vars) 1)))
									 (let (,(list (last-ref 0) (var-initial-value (last-ref 3))))
									   ,@(copy body (make-list (+ (last-ref 2) 1))))
									 ,(list-ref body (+ (last-ref 2) 1))
									 ...))))))
				 (when (tree-memq (last-ref 0) (car p))
				   (set! (last-ref 2) i)
				   (if (not (last-ref 1)) (set! (last-ref 1) i))))))
			   )))
		   env)
		  
		  ;; ---------------- letrec ----------------		  
		  ((letrec letrec*) 
		   (if (< (length form) 3)
		       (lint-format "~A is messed up: ~A" caller head (truncated-list->string form))
		       (let ((vars ()))
			 (cond ((null? (cadr form))
				(lint-format "~A could be let: ~A" caller head (truncated-list->string form)))
			       ((not (pair? (cadr form)))
				(lint-format "~A is messed up: ~A" caller head (truncated-list->string form)))
			       ((and (null? (cdadr form))
				     (eq? head 'letrec*))
				(lint-format "letrec* could be letrec: ~A" caller (truncated-list->string form))))

			 (do ((bindings (cadr form) (cdr bindings)))
			     ((not (pair? bindings))
			      (if (not (null? bindings))
				  (lint-format "~A variable list is not a proper list? ~S" caller head (cadr form))))
			   (when (binding-ok? caller head (car bindings) env #f)
			     (set! vars (cons (make-var :name (caar bindings) 
							:initial-value (if (and (eq? (caar bindings) (cadar bindings))
										(or (eq? head 'letrec)
										    (not (var-member (caar bindings) vars))))
									   (begin
									     (lint-format "~A is the same as (~A #<undefined>) in ~A" caller
											  (car bindings) (caar bindings) head)
									     ;; in letrec* ((x 12) (x x)) is an error
									     #<undefined>)
									   (cadar bindings))
							:definer head)
					      vars))))

			 (if (pair? vars)  ; if none of the local vars occurs in any of the values, no need for the "rec"
			     (do ((bindings (cadr form) (cdr bindings))
				  (vs (map var-name vars)))
				 ((or (not (pair? bindings))
				      (not (pair? (car bindings)))
				      (not (pair? (cdar bindings)))
				      (memq (cadar bindings) vs)
				      (tree-set-member vs (cadar bindings)))
				  (if (null? bindings)
				      (lint-format "~A could be ~A: ~A" caller
						   head (if (eq? head 'letrec) 'let 'let*)
						   (truncated-list->string form))))))

			 (when (and (pair? vars)
				    (null? (cdr vars))
				    (pair? (cddr form))
				    (pair? (caddr form))
				    (null? (cdddr form)))
			   (let ((body (caddr form))
				 (sym (var-name (car vars)))
				 (lform (cadar (cadr form))))           ; the letrec var's lambda
			     (if (eq? sym (car body))                   ; (letrec ((x (lambda ...))) (x...)) -> (let x (...)...)
				 (if (and (pair? lform)
					  (pair? (cdr lform))
					  (eq? (car lform) 'lambda)
					  (proper-list? (cadr lform)) ; includes ()
					  (< (tree-length body) 100))
				     ;; the limit on tree-length is for cases where the args are long lists of data --
				     ;;   more like for-each than let, and easier to read if the code is first, I think.
				     (lint-format "perhaps ~A" caller
						  (lists->string form `(let ,sym
									 ,(if (null? (cadr lform)) () (map list (cadr lform) (cdr body)))
									 ,@(cddr lform)))))
				 (if (and (not (eq? caller 'define))
					  (pair? lform)
					  (pair? (cdr lform))
					  (proper-list? (cadr lform)))
				     (let ((call (find-call sym body)))
				       (when (pair? call)
					 (if (= (tree-count1 sym body 0) 1)
					     (let ((new-call `(let ,sym
								,(map list (cadr lform) (cdr call))
								,@(cddr lform))))
					       (lint-format "perhaps ~A" caller
							    (lists->string form (tree-subst new-call call body)))))))))))
			 ;; lambda here is handled under define

			 (let ((new-env (append vars env)))
			   (do ((bindings (cadr form) (cdr bindings)))
			       ((not (pair? bindings)))
			     (if (binding-ok? caller head (car bindings) env #t)
				 (lint-walk caller (cadar bindings) new-env)))

			   (let* ((cur-env (append vars env))
				  (e (lint-walk-body caller head (cddr form) cur-env))
				  (nvars (and (not (eq? e cur-env))
					      (env-difference caller e cur-env ()))))
			     (if (pair? nvars)
				 (if (eq? (var-name (car nvars)) lambda-marker)
				     (begin
				       (set! env (cons (car nvars) env))
				       (set! nvars (cdr nvars)))
				     (set! vars (append nvars vars))))

			     (report-usage caller head vars cur-env))))) ; constant exprs never happen here
		   env)
		  
		  ;; ---------------- begin ----------------
		  ((begin)
		   (if (not (proper-list? form))
		       (begin
			 (lint-format "stray dot in begin? ~A" caller (truncated-list->string form))
			 env)
		       (begin
			 (if (and (pair? (cdr form))
				  (null? (cddr form)))
			     (lint-format "begin could be omitted: ~A" caller (truncated-list->string form)))
			 (lint-walk-body caller head (cdr form) env))))
		  
		  ;; -------- with-let --------
		  ((with-let)
		   (if (< (length form) 3)
		       (lint-format "~A is messed up: ~A" head caller (truncated-list->string form))
		       (let ((e (cadr form)))
			 (if (or (and (code-constant? e)
				      (not (let? e)))
				 (and (pair? e)
				      (let ((op (return-type (car e) env)))
					(and op
					     (not (return-type-ok? 'let? op))))))
			     (lint-format "~A: first argument should be an environment: ~A" head caller (truncated-list->string form)))
			 (if (symbol? e)
			     (set-ref e form env)
			     (if (pair? e)
				 (begin
				   (if (and (null? (cdr e))
					    (eq? (car e) 'curlet))
				       (lint-format "~A is not needed here: ~A" head caller (truncated-list->string form)))
				   (lint-walk caller e env))))
			 (let ((walked #f))
			   (if (or (and (symbol? e)
					(memq e '(*gtk* *motif* *gl* *libc* *libm* *libgdbm* *libgsl*)))
				   (and (pair? e)
					(eq? (car e) 'sublet)
					(pair? (cdr e))
					(memq (cadr e) '(*gtk* *motif* *gl* *libc* *libm* *libgdbm* *libgsl*))
					(set! e (cadr e))))
			       (let ((lib (if (defined? e)
					      (symbol->value e)
					      (let ((file (*autoload* e)))
						(and (string? file) 
						     (load file))))))
				 (when (let? lib)
				   (let ((old-e *e*))
				     (set! *e* lib)
				     (let* ((e (lint-walk-body caller head (cddr form) env))
					    (vars (if (eq? e env) () (env-difference caller e env ()))))
				       (report-usage caller head vars env))
				     (set! *e* old-e)
				     (set! walked #t)))))
			   
			   (unless walked
			     (lint-walk-body caller head (cddr form) env)))))
		   env)
		  
		  ;; ---------------- defmacro ----------------
		  ((defmacro defmacro*) 
		   (if (or (< (length form) 4)
			   (not (symbol? (cadr form))))
		       (lint-format "~A declaration is messed up: ~A" caller head (truncated-list->string form))
		       (let ((sym (cadr form))
			     (args (caddr form))
			     (body (cdddr form)))
			 (if (and (pair? args)
				  (repeated-member? args env))
			     (lint-format "~A parameter is repeated: ~A" caller head (truncated-list->string args))
			     (lint-format "~A is deprecated; perhaps ~A" caller head
					  (truncated-lists->string form 
								   `(,(if (eq? head 'defmacro) 'define-macro 'define-macro*) 
								     ,(cons sym args) 
								     ,@body))))
			 (lint-walk-function head sym args body form env)))
		   env)
		  
		  ;; ---------------- defgenerator ----------------
		  ((defgenerator)
		   (append (get-generator caller form env) env))
		  

		  ;; ---------------- load ----------------
		  ((load)
		   (lint-walk caller (cdr form) env)
		   (if (and *report-loaded-files*
			    (string? (cadr form)))
		       (catch #t
			 (lambda ()
			   (lint-file (cadr form) env))
			 (lambda args
			   env))
		       env))

		  ;; ---------------- require ----------------
		  ((require)
		   (if (not *report-loaded-files*)
		       env
		       (let ((vars env))
			 (for-each 
			  (lambda (f)
			    (let ((file (*autoload* f)))
			      (if (string? file)
				  (catch #t
				    (lambda ()
				      (set! vars (lint-file file vars)))
				    (lambda args
				      #f)))))
			  (cdr form))
			 vars)))

		  ;; ----------------
		  ((call-with-input-string call-with-input-file call-with-output-file call-with-output-string)
		   (let ((len (if (eq? head 'call-with-output-string) 2 3))) ; call-with-output-string func is the first arg, not second
		     (when (= (length form) len)
		       (let ((func (list-ref form (- len 1))))
			 (if (= len 3)
			     (lint-walk caller (cadr form) env))
			 (if (not (and (pair? func)
				       (eq? (car func) 'lambda)))
			     (lint-walk caller func env)
			     (let* ((args (cadr func))
				    (body (cddr func))
				    (port (and (pair? args) (car args))))
			       (if (or (not port)
				       (pair? (cdr args)))
				   (lint-format "~A argument should be a function of one argument: ~A" caller head func)
				   (if (and (null? (cdr body))
					    (pair? (car body))
					    (pair? (cdar body))
					    (eq? (cadar body) port)
					    (null? (cddar body)))
				       (lint-format "perhaps ~A" caller 
						    (lists->string form 
								   (if (= len 2)
								       `(,head ,(caar body))
								       `(,head ,(cadr form) ,(caar body)))))
				       (let ((cc (make-var :name port
							   :initial-value (list (case head 
										  ((call-with-input-string)  'open-input-string)
										  ((call-with-output-string) 'open-output-string)
										  ((call-with-input-file)    'open-input-file)
										  ((call-with-output-file)   'open-output-file)))
							   :definer head)))
					 (lint-walk-body caller head body (cons cc env))
					 (report-usage caller head (list cc) env)))))))))
		   env)
		  
		  ;; ----------------
		  ((catch)
		   ;; catch tag is tricky -- it is evaluated, then eq? matches at error time, so we need
		   ;;   to catch constants that can't be eq?
		   (if (not (= (length form) 4))
		       (begin
			 (lint-format "catch takes 3 arguments (tag body error-handler): ~A" caller (truncated-list->string form))
			 (lint-walk caller (cdr form) env))
		       (let ((tag (cadr form)))
			 (if (or (and (not (pair? tag))
				      (or (number? tag) (char? tag) (length tag)))
				 (and (pair? tag)
				      (eq? (car tag) 'quote)
				      (or (not (pair? (cdr tag)))
					  (length (cadr tag)))))
			     (lint-format "catch tag ~S is unreliable (catch uses eq? to match tags)" caller tag))
			 (let ((body (caddr form))
			       (error-handler (cadddr form)))
			   (let ((catcher (make-var :name catch-marker
						    :initial-value form
						    :definer head)))
			     (lint-walk caller body (cons catcher env)))
			   (lint-walk caller error-handler env))))
		   env)
		  
		  ;; ----------------
		  ((call/cc call-with-current-continuation call-with-exit)
		   (let ((continuation (and (pair? (cdr form))
					    (pair? (cadr form))
					    (eq? (caadr form) 'lambda)
					    (pair? (cdadr form))
					    (pair? (cddadr form))
					    (pair? (cadadr form))
					    (car (cadadr form)))))
		     (if (not (symbol? continuation))
			 (lint-walk caller (cdr form) env)
			 (let ((body (cddadr form)))

			   (if (not (or (eq? head 'call-with-exit)
					(eq? continuation (car body))
					(tree-sym-set-member continuation '(lambda lambda* define define* curlet error apply) body)))
					;; this checks for continuation as arg (of anything), and any of set as car
			       (lint-format "perhaps ~A could be call-with-exit: ~A" caller head (truncated-list->string form)))

			   (if (not (tree-member continuation body))
			       (lint-format "~A ~A ~A appears to be unused: ~A" caller head 
					    (if (eq? head 'call-with-exit) "exit function" "continuation")
					    continuation
					    (truncated-list->string form))
			       (let ((last (and (proper-list? body)
						(list-ref body (- (length body) 1)))))
				 (if (and (pair? last)
					  (eq? (car last) continuation))
				     (lint-format "~A is redundant here: ~A" caller continuation (truncated-list->string last)))))

			   (let ((cc (make-var :name continuation 
					       :initial-value (list (if (eq? head 'call-with-exit) goto-marker call/cc-marker))
					       :definer head)))
			     (lint-walk-body caller head body (cons cc env))
			     (report-usage caller head (list cc) env)))))
		   env)

		  ((define-syntax define-module)
		   env)
		  
		  ((let-syntax letrec-syntax)
		   (lint-walk-body caller head (cddr form) env))

		  ((case-lambda)
		   (if (pair? (cdr form))
		       (let ((lens ())
			     (body (if (string? (cadr form)) (cddr form) (cdr form))) ; might have a doc string before the clauses
			     (doc-string (and (string? (cadr form)) (cadr form))))

			 (define (arg->defaults arg b1 b2 defaults)
			   (and defaults
				(cond ((null? b1) (and (null? b2) defaults))
				      ((null? b2) (and (null? b1) defaults))
				      ((eq? arg b1) (cons b2 defaults))
				      ((eq? arg b2) (cons b1 defaults))
				      ((pair? b1)
				       (and (pair? b2)
					    (arg->defaults arg (car b1) (car b2) (arg->defaults arg (cdr b1) (cdr b2) defaults))))
				      (else (and (equal? b1 b2) defaults)))))
			 (for-each 
			  (lambda (choice)
			    (if (pair? choice)
				(let ((len (length (car choice))))
				  (if (member len lens)
				      (lint-format "repeated parameter list? ~A in ~A" caller (car choice) form))
				  (set! lens (cons len lens))
				  (lint-walk 'case-lambda `(lambda ,@choice) env))))
			  body)

			 (define (arglists-equal? args1 args2)
			   (if (null? args1)
			       (and (pair? args2)
				    (null? (cdr args2)))
			       (if (null? args2)
				   (and (pair? args1)
					(null? (cdr args1)))
				   (and (pair? args1) 
					(pair? args2)
					(eq? (car args1) (car args2))
					(arglists-equal? (cdr args1) (cdr args2))))))

			 (case (length lens)
			   ((1) 
			    (lint-format "perhaps ~A" caller 
					 (lists->string form 
							(if doc-string
							    `(let ((documentation ,doc-string))
							       (lambda ,(caar body) ,@(cdar body)))
							    `(lambda ,(caar body) ,@(cdar body))))))
			   ((2) 
			    (if (arglists-equal? (caar body) (caadr body))
				(let* ((clause1 (car body))
				       (arg1 (car clause1))
				       (body1 (cdr clause1))
				       (clause2 (cadr body))
				       (arg2 (car clause2))
				       (body2 (cdr clause2))
				       (arglist (if (> (car lens) (cadr lens)) arg2 arg1)) ; lens is reversed
				       (arg-name (list-ref arglist (- (length arglist) 1)))
				       (diffs (arg->defaults arg-name body1 body2 ())))
				  (if (and (pair? diffs)
					   (= (length diffs) 1)
					   (code-constant? (car diffs)))
				      (let ((new-body (if (> (car lens) (cadr lens)) body2 body1))
					    (new-arglist (if (not (car diffs))
							     arglist
							     (if (null? (cdr arglist))
								 `((,arg-name ,(car diffs)))
								 `(,(car arglist) (,arg-name ,(car diffs)))))))
					(lint-format "perhaps ~A" caller
						     (lists->string form
								    (if doc-string
									`(let ((documentation ,doc-string))
									   (lambda* ,new-arglist ,@new-body))
									`(lambda* ,new-arglist ,@new-body))))))))))))
		   ;; for 3 up, the defaults need to be consistent:
		   ;; (case-lambda (() (f 0 0)) ((a) (f a 0)) ((a b) (f a b))) -> (lambda* ((a 0) (b 0)) (f a b))
		   env)

		  ;; ---------------- everything else ----------------	
		  (else
		   (if (not (proper-list? form))
		       ;; these appear to be primarily macro/match arguments
		       (if (and (pair? form)
				(symbol? head)
				(procedure? (symbol->value head *e*)))
			   (lint-format "unexpected dot: ~A" caller (truncated-list->string form)))
		       (begin
			 (when (symbol? head)
			   (let ((v (var-member head env)))
			     (if (and (var? v) 
				      (not (memq form (var-history v))))
				 (set! (var-history v) (cons form (var-history v))))
			     (check-call caller head form env)

			     (when (pair? form)
			       ;; save any references to vars in their var-history (type checked later)
			       ;;   this can be fooled by macros, as everywhere else
			       (for-each (lambda (arg)
					   (if (symbol? arg)
					       (let ((v (var-member arg env)))
						 (if (and (var? v)
							  (not (memq form (var-history v))))
						     (set! (var-history v) (cons form (var-history v)))))))
					   form)

			       (if (and *report-any-!-as-setter* ; (inc! x) when inc! is unknown, assume it sets x
					(symbol? (car form))
					(pair? (cdr form))
					(symbol? (cadr form))
					(not (var-member (car form) env))
					(not (hash-table-ref built-in-functions (car form)))
					(let ((str (symbol->string (car form))))
					  (char=? (string-ref str (- (length str) 1)) #\!)))
				   (set-set (cadr form) form env)))

			     (if (not (var? v))
				 (check-special-cases caller head form env)
				 (if (and (memq (var-ftype v) '(define lambda define* lambda*))
					  (not (memq caller (var-scope v))))
				     (let ((cv (var-member caller env)))
				       (set! (var-scope v) 
					     (if (and (var? cv)
							    (memq (var-ftype cv) '(define lambda define* lambda*))) ; named-let does not define ftype
						 (cons caller (var-scope v))
						 (cons (cons caller #f) (var-scope v)))))))

			     (if (assq head deprecated-ops)
				 (lint-format "~A is deprecated; use ~A" caller head (cdr (assq head deprecated-ops))))

			     (if (and (not (= line-number last-simplify-numeric-line-number))
				      (not (var? v))
				      (hash-table-ref numeric-ops head)
				      (proper-tree? form))
				 (let ((val (simplify-numerics form env)))
				   (if (not (equal-ignoring-constants? form val))
				       (begin
					 (set! last-simplify-numeric-line-number line-number)
					 (lint-format "perhaps ~A" caller (lists->string form val))))))
			   
			     ;; if a var is used before it is defined, the var history and ref/set
			     ;;   info needs to be saved until the definition, so other-identifiers collects it
			     (if (not (or (var? v)
					  (defined? head (rootlet))))
				 (hash-table-set! other-identifiers head 
						  (if (not (hash-table-ref other-identifiers head))
						      (list form)
						      (cons form (hash-table-ref other-identifiers head)))))))

			 (when (and (pair? head)
				    (pair? (cdr head))
				    (memq (car head) '(lambda lambda*))
				    (not (any? (lambda (arg)
						 (and (pair? arg)
						      (mv-range (car arg) env))) ; ignore ((lambda...) (multiple-value...))
					       (cdr form))))
			   (let ((len (length (cadr head))))
			     (cond ((and len (>= len 0)
					 (eq? (car head) 'lambda)
					 (not (equal? len (length (cdr form)))))
				    (lint-format "~A has ~A arguments: ~A" 
						 head (car head) 
						 (if (> len (length (cdr form))) "too few" "too many")
						 (truncated-list->string form)))

				   ((identity? head)
				    (lint-format "perhaps ~A" caller (truncated-lists->string form (cadr form))))

				   ((and (null? (cadr head))
					 (pair? (cddr head)))
				    (lint-format "perhaps ~A" caller 
						 (truncated-lists->string form 
						   (if (and (null? (cdddr head))
							    (not (and (pair? (caddr head))
								      (memq (caaddr head) '(define define* define-constant define-macro define-macro*)))))
						       (caddr head)
						       `(let () ,@(cddr head))))))

				   ((and (pair? (cddr head)) ; ((lambda (...) ...) ...) -> (let ...) -- lambda here is ugly and slow
					 (proper-list? (cddr head)))
				    (call-with-exit
				     (lambda (quit)          ; uncountably many things can go wrong with the lambda form
				       (let ((vars ())
					     (vals ()))
					 (do ((v (cadr head) (cdr v))
					      (a (cdr form) (cdr a)))
					     ((not (and (pair? a)
							(pair? v)))
					      (if (symbol? v)
						  (begin
						    (set! vars (cons v vars))
						    (set! vals (cons `(list ,@a) vals)))
						  (do ((v v (cdr v)))
						      ((not (pair? v)))
						    (if (not (pair? v))
							(quit))
						    (if (pair? (car v)) 
							(begin
							  (if (not (pair? (cdar v)))
							      (quit))
							  (set! vars (cons (caar v) vars))
							  (set! vals (cons (cadar v) vals)))
							(begin
							  (set! vars (cons (car v) vars))
							  (set! vals (cons #f vals)))))))
					   (set! vars (cons (if (pair? (car v)) (caar v) (car v)) vars))
					   (set! vals (cons (car a) vals)))
					 
					 (lint-format "perhaps ~A" caller
						      (lists->string form
								     `(,(if (or (eq? (car head) 'lambda)
										(not (pair? (cadr head)))
										(null? (cdadr head)))
									    'let 'let*)
								       ,(map list (reverse vars) (reverse vals))
								       ,@(cddr head)))))))))))
			 (let ((vars env))
			   (for-each
			    (lambda (f)
			      (set! vars (lint-walk caller f vars)))
			    form))))
		   env)))
	      
	      ;; else form is not a symbol and not a pair
	      (begin
		(if (vector? form)
		    (let ((happy #t))
		      (for-each
		       (lambda (x)
			 (when (and (pair? x)
				    (eq? (car x) 'unquote))
			   (lint-walk caller (cadr x) env) ; register refs
			   (set! happy #f)))
		       form)
		      (if (not happy)   ; these are used exactly 4 times in 2.5 million lines of randomly gathered open source scheme code
			  (lint-format "quasiquoted vectors are not supported: ~A" caller form))))
		env))))


    ;; -------- lint-file --------
    (define *report-input* #t)

    (define (lint-file-1 file env)
      (set! linted-files (cons file linted-files))
      (let ((fp (if (input-port? file)
		    file
		    (begin
		      (set! *current-file* file)
		      (catch #t
			(lambda ()
			  (let ((p (open-input-file file)))
			    (if *report-input*
				(if (and (output-port? outport)
					 (not (member outport (list *stderr* *stdout*))))
				    (format outport "~%~NC~%;~A~%" (+ lint-left-margin 16) #\- file)
				    (format outport ";~A~%" file)))
			    p))
			(lambda args
			  (format outport "~NCcan't open ~S: ~A~%" lint-left-margin #\space file (apply format #f (cadr args)))
			  #f))))))
	
	(if (input-port? fp)
	    (let ((vars env)
		  (line 0)
		  (last-form #f)
		  (last-line-number -1))
	      
	      (do ((form (read fp) (read fp)))
		  ((eof-object? form))
		(if (pair? form)
		    (set! line (max line (pair-line-number form))))
		
		(if (not (or (= last-line-number -1)
			     (side-effect? last-form vars)))
		    (format outport "~NCtop-level (line ~D): this has no effect: ~A~%" 
			    lint-left-margin #\space last-line-number
			    (truncated-list->string last-form)))
		(set! last-form form)
		(set! last-line-number line)
		
		(if (and (pair? form)
			 (memq (car form) '(define define-macro))
			 (pair? (cdr form))
			 (pair? (cadr form)))
		    (let ((f (caadr form)))
		      (if (and (symbol? f)
			       (hash-table-ref built-in-functions f))
			  (format outport "~NCtop-level ~Aredefinition of built-in function ~A: ~A~%" 
				  lint-left-margin #\space 
				  (if (> (pair-line-number form) 0)
				      (format #f "(line ~D) " (pair-line-number form))
				      "")
				  f (truncated-list->string form)))))

		(set! vars (lint-walk (if (symbol? form) 
					  form 
					  (and (pair? form) 
					       (car form)))
				      form 
				      vars)))
	      
	      (if (not (input-port? file))
		  (close-input-port fp))
	      
	      vars))))
    
    (define (lint-file file env)
      ;; (format *stderr* "lint ~S~%" file)
      
      (if (member file linted-files)
	  env
	  (let ((old-current-file *current-file*)
		(old-pp-left-margin pp-left-margin)
		(old-lint-left-margin lint-left-margin)
		(old-load-path *load-path*))
	    
	    (dynamic-wind
		(lambda ()
		  (set! pp-left-margin (+ pp-left-margin 4))
		  (set! lint-left-margin (+ lint-left-margin 4))
		  (when (and (string? file)
			     (char=? (file 0) #\/))
		    (let ((last-pos 0))
		      (do ((pos (char-position #\/ file (+ last-pos 1)) (char-position #\/ file (+ last-pos 1))))
			  ((not pos)
			   (if (> last-pos 0)
			       (set! *load-path* (cons (substring file 0 last-pos) *load-path*))))
			(set! last-pos pos)))))
		
		(lambda ()
		  (lint-file-1 file env))
		
		(lambda ()
		  (set! pp-left-margin old-pp-left-margin)
		  (set! lint-left-margin old-lint-left-margin)
		  (set! *current-file* old-current-file)
		  (set! *load-path* old-load-path)
		  (if (positive? (length *current-file*))
		      (newline outport)))))))
    

    
    ;;; --------------------------------------------------------------------------------'
    ;;; lint itself
    ;;;
    (let ((documentation "(lint file port) looks for infelicities in file's scheme code")
	  (signature (list #t string? output-port? boolean?)))
      (lambda* (file (outp *lint-output-port*) (report-input #t))
	(set! outport outp)
	(set! other-identifiers (make-hash-table))
	(set! linted-files ())
	(set! last-simplify-boolean-line-number -1)
	(set! last-simplify-numeric-line-number -1)
	(set! last-simplify-cxr-line-number -1)
	(set! last-checker-line-number -1)
	(set! last-if-line-number -1)
	(set! line-number -1)
	(set! quote-warnings 0)
	(set! pp-left-margin 0)
	(set! lint-left-margin -3) ; lint-file above adds 4
	
	(set! big-constants (make-hash-table))
	(set! equable-closures (make-hash-table))

	(set! *report-input* report-input)
	(set! *report-nested-if* (if (integer? *report-nested-if*) (max 3 *report-nested-if*) 4))
	(set! *report-short-branch* (if (integer? *report-short-branch*) (max 0 *report-short-branch*) 12))

	(set! *#readers*
	      (list (cons #\e (lambda (str)
				(if (not (string=? str "e"))
				    (let ((num (string->number (substring str 1))))
				      (if num 
					  (cond ((rational? num)
						 (format outport "~NCthis #e is dumb, #~A -> ~A~%" lint-left-margin #\space str (substring str 1)))
						((not (real? num))
						 (format outport "~NC#e can't handle complex numbers, #~A -> ~A~%" lint-left-margin #\space str num))
						((= num (floor num))
						 (format outport "~NCperhaps #~A -> ~A~%" lint-left-margin #\space str (floor num)))))))
				#f))
		    (cons #\i (lambda (str)
				(if (not (string=? str "i"))
				    (let ((num (string->number (substring str 1))))
				      (if num 
					  (if (not (rational? num))
					      (format outport "~NCthis #i is dumb, #~A -> ~A~%" lint-left-margin #\space str (substring str 1))
					      (format outport "~NCperhaps #~A -> ~A~%" lint-left-margin #\space str (* 1.0 num))))))
				#f))
		    (cons #\d (lambda (str)
				(if (and (not (string=? str "d"))
					 (string->number (substring str 1)))
				    (format outport "~NC#d is pointless, #~A -> ~A~%" lint-left-margin #\space str (substring str 1)))
				#f))

		    (cons #\! (lambda (str)
				(if (member str '("!optional" "!default" "!rest" "!key" "!aux" "!false" "!true") string-ci=?) ; for MIT-scheme
				    (make-keyword (substring str 1))
				    (let ((lc (str 0))) ; s7 should handle this, but...
				      (do ((c (read-char) (read-char)))
					  ((or (and (eof-object? c)
						    (or (format outport "unclosed block comment~%")
							#t))
					       (and (char=? lc #\!)
						    (char=? c #\#)))
					   #f)
					(set! lc c))))))

		    (cons #\_ (lambda (str)
				(and (string=? str "__line__")
				     (port-line-number))))))
	
	;; try to get past all the # and \ stuff in other Schemes
	;;   main remaining problem: [] used as parentheses (Gauche and Chicken for example)
	(set! (hook-functions *read-error-hook*)  
	      (list (lambda (h)
		      (let ((data (h 'data))
			    (line (port-line-number)))
			(if (not (h 'type))
			    (begin
			      (format outport "~NCreader[~A]: unknown \\ usage: \\~C~%" lint-left-margin #\space line data)
			      (set! (h 'result) data))
			    (begin
			      (format outport "~NCreader[~A]: unknown # object: #~A~%" lint-left-margin #\space line data)
			      (set! (h 'result)
				    (case (data 0)
				      ((#\_) (if (string=? data "__line__")
						 (port-line-number)
						 (make-keyword data)))
				      
				      ((#\;) (read) (values))
				      
				      ((#\T) (string=? data "T"))
				      ((#\F) (and (string=? data "F") (list 'not #t)))

				      ((#\l #\z)
				       (let ((num (string->number (substring data 1)))) ; Bigloo (also has #ex #lx #z and on and on)
					 (if (number? num)
					     (begin
					       (format outport "~NCjust omit this silly #~C!~%" lint-left-margin #\space (data 0))
					       num)
					     (make-keyword data))))
				      
				      ((#\u) ; for Bigloo
				       (if (string=? data "unspecified")
					   (format outport "~NCuse #<unspecified>, not #unspecified~%" lint-left-margin #\space))
				       ;; #<unspecified> seems to hit the no-values check?
				       (make-keyword data))

				      ((#\v) ; r6rs byte-vectors?
				       (if (string=? data "vu8")
					   (format outport "~NCuse #u8 in s7, not #vu8~%" lint-left-margin #\space))
				       (make-keyword data))
				      
				      ((#\>) ; for Chicken, apparently #>...<# encloses in-place C code
				       (do ((last #\#)
					    (c (read-char) (read-char))) 
					   ((and (char=? last #\<) 
						 (char=? c #\#)) 
					    (values))
					 (if (char=? c #\newline)
					     (set! (port-line-number ()) (+ (port-line-number) 1)))
					 (set! last c)))
				      
				      ((#\<) ; Chicken also, #<<EOF -> EOF
				       (if (and (char=? (data 1) #\<)
						(> (length data) 2))
					   (let ((end (substring data 2)))
					     (do ((c (read-line) (read-line)))
						 ((string-position end c)
						  (values))))
					   (make-keyword data)))
				      
				      ((#\\) 
				       (cond ((assoc data '(("\\newline"   . #\newline)
							    ("\\return"    . #\return)
							    ("\\space"     . #\space)
							    ("\\tab"       . #\tab)
							    ("\\null"      . #\null)
							    ("\\linefeed"  . #\linefeed)
							    ("\\alarm"     . #\alarm)
							    ("\\esc"       . #\escape)
							    ("\\escape"    . #\escape)
							    ("\\rubout"    . #\delete)
							    ("\\delete"    . #\delete)
							    ("\\backspace" . #\backspace)
							    ("\\page"      . #\xc)
							    
							    ;; rest are for Guile
							    ("\\vt"        . #\xb)
							    ("\\bs"        . #\backspace)
							    ("\\cr"        . #\newline)
							    ("\\sp"        . #\space)
							    ("\\lf"        . #\linefeed)
							    ("\\nl"        . #\null)
							    ("\\ht"        . #\tab)
							    ("\\ff"        . #\xc)
							    ("\\np"        . #\xc))
						     string-ci=?)
					      => (lambda (c)
						   (format outport "~NCperhaps use ~W instead~%" lint-left-margin #\space (cdr c))
						   (cdr c)))
					     (else 
					      (make-keyword (substring data 1)))))
				      (else 
				       (make-keyword data))))))))))
	
	(let ((vars (lint-file file ())))

	  (if (and (pair? vars)
		   *report-multiply-defined-top-level-functions*)
	      (for-each
	       (lambda (var)
		 (let ((var-file (hash-table-ref *top-level-objects* (car var))))
		   (if (not var-file)
		       (hash-table-set! *top-level-objects* (car var) *current-file*)
		       (if (and (string? *current-file*)
				(not (string=? var-file *current-file*)))
			   (format outport "~NC~S is defined at the top level in ~S and ~S~%" 
				   lint-left-margin #\space 
				   (car var) var-file *current-file*)))))
	       vars))
	  
	  (if (and (string? file)
		   (pair? vars)
		   *report-unused-top-level-functions*)
	      (report-usage file "" vars vars)))

	(for-each 
	 (lambda (p)
	   (if (or (> (cdr p) 5)
		   (and (> (cdr p) 3) 
			(> (length (car p)) 12)))
	       (format outport "~A~A occurs ~D times~%"
		       (if (pair? (car p)) "'" "")
		       (truncated-list->string (car p)) (cdr p))))
	 big-constants)
	
	(if (and *report-undefined-identifiers*
		 (positive? (hash-table-entries other-identifiers)))
	    (let ((lst (sort! (map car other-identifiers) (lambda (a b)
							    (string<? (symbol->string a) (symbol->string b))))))
	      (format outport "~NCth~A identifier~A not defined~A: ~{~S~^ ~}~%"
		      (max lint-left-margin 1) #\space 
		      (if (= (hash-table-entries other-identifiers) 1) "is" "e following")
		      (if (= (hash-table-entries other-identifiers) 1) " was" "s were")
		      (if (string? file) (format #f " in ~S" file) "")
		      lst)
	      (fill! other-identifiers #f)))))))
	      



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
	    (if (not (or apos epos))
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

			   (string-append (cond ((assoc substr '(("gt"    . ">") 
								 ("lt"    . "<") 
								 ("mdash" . "-") 
								 ("amp"   . "&"))
							string=?) => cdr)
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
					  (let ((outstr (call-with-output-string
							 (lambda (op)
							   (call-with-input-string (format #f "~S" ncode)
							     (lambda (ip)
							       (let ((old-shadow *report-shadowed-variables*))
								 (set! *report-shadowed-variables* #t)
								 (lint ip op #f)
								 (set! *report-shadowed-variables* old-shadow))))))))
					    (if (> (length outstr) 1) ; possible newline at end
						(format () ";~A ~D: ~A~%" file line-num outstr)))))
				      (lambda args
					(format () ";~A ~D, error in read: ~A ~A~%" file line-num args
						(fixup-html (remove-markups code)))))))))))))))))))


;;; --------------------------------------------------------------------------------
;;; TODO:
;;; perhaps report an overall order to definitions in a block that maximizes locality
;;; find the rest of the macro cases and (s7)test out-vars somehow
;;; second pass after report-usage: check multi-type var, collect blocks, check seq bounds as passed to func?
;;; code-equal if/when/unless/cond, case: any order of clauses, let: any order of vars, etc
;;; lint-suggest with forms as pars to get better line numbers -- at least collect the offenders [check-returns]
;;;   could we search the form for the lowest positive line num?
;;; tree-set-member -> tree-hash-member or at least binder? [match-vars]
;;; snd-lint: load lint, add to various hash-tables via *lint* 
;;;
;;; (list? x) -> (car x) but might be () [at least if] also needs proper-list? somehow
;;; (number? x) -> (vector|list-ref...)?
;;;
;;; look for other mid-form uses [or->#f and->#t trailers?]
;;; let*-values pprint is messed up?
;;; (and (not (...)) (or (not (...))...)) -> in the or, the repeated thing (if no side-effect) is redundant
;;;
;;; 495/100
