;;; lint for s7 scheme
;;;
;;; (lint "file.scm") checks file.scm for infelicities
;;; to control the kinds of checks, set the variables below.

(provide 'lint.scm)

(define *report-unused-parameters* #f)                    ; many of these are reported anyway if they are passed some non-#f value
(define *report-unused-top-level-functions* #f)           
(define *report-multiply-defined-top-level-functions* #f) ; same name defined at top level in more than one file
(define *report-shadowed-variables* #f)                   ; shadowed parameters, etc
(define *report-undefined-identifiers* #f)                ; names we can't account for
(define *report-function-stuff* #t)                       ; still very much work-in-progress (and slow!)
(define *report-doc-strings* #f)                          ; old-style (CL) doc strings

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

(define defanimal define*)
(unless (provided? 'snd)
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
	      values vector vector-append vector->list vector-dimensions vector-length vector-ref vector?
	      when with-baffle with-let with-input-from-file with-input-from-string with-output-to-string
	      zero?))
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
			         exit dilambda hash-table-size make-hook hook-functions))
			      ht))

	(makers '(gensym sublet inlet make-iterator let->list random-state random-state->list number->string
		  make-string string string-copy copy list->string string->list string-append substring object->string
		  format cons list make-list reverse append vector-append list->vector vector->list make-vector
		  make-shared-vector vector make-float-vector float-vector make-int-vector int-vector byte-vector
		  byte-vector hash-table hash-table* make-hash-table make-hook))
	
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
	(last-simplify-cxr-line-number -1)
	(last-if-line-number -1)
	(last-checker-line-number -1)
	(line-number -1)
	(lambda-marker '[lambda]))
    
    (set! *e* (curlet))
;    (define calls 0)


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
      (let ((outstr (if (< 0 line-number 100000)
			(apply format #f (string-append " ~A (line ~D): " str "~%") (truncated-list->string name) line-number args)
			(apply format #f (string-append " ~A: " str "~%") (truncated-list->string name) args))))
	(set! made-suggestion (+ made-suggestion 1))
	(display outstr outport)
	(if (> (length outstr) 120)
	    (newline outport))))


    ;; -------- vars -------- 
    (define var-name car)
    (define (var? v) (and (pair? v) (let? (cdr v))))
    (define var-member assq)
    
    (define var-ref (dilambda (lambda (v) (let-ref (cdr v) 'ref)) (lambda (v x) (let-set! (cdr v) 'ref x))))
    (define var-set (dilambda (lambda (v) (let-ref (cdr v) 'set)) (lambda (v x) (let-set! (cdr v) 'set x))))
    (define var-initial-value (lambda (v) (let-ref (cdr v) 'initial-value))) ; not settable
    (define var-history (dilambda (lambda (v) (let-ref (cdr v) 'history)) (lambda (v x) (let-set! (cdr v) 'history x))))
    (define var-ftype (dilambda (lambda (v) (let-ref (cdr v) 'ftype)) (lambda (v x) (let-set! (cdr v) 'ftype x))))
    (define var-side-effect (dilambda (lambda (v) (let-ref (cdr v) 'side-effect)) (lambda (v x) (let-set! (cdr v) 'side-effect x))))
    (define var-arglist (dilambda (lambda (v) (let-ref (cdr v) 'arglist)) (lambda (v x) (let-set! (cdr v) 'arglist x))))
    (define var-signature (dilambda (lambda (v) (let-ref (cdr v) 'signature)) (lambda (v x) (let-set! (cdr v) 'signature x))))
    (define var-definer (dilambda (lambda (v) (let-ref (cdr v) 'definer)) (lambda (v x) (let-set! (cdr v) 'definer x))))
    (define var-leaves (dilambda (lambda (v) (let-ref (cdr v) 'leaves)) (lambda (v x) (let-set! (cdr v) 'leaves x))))
    (define var-scope (dilambda (lambda (v) (let-ref (cdr v) 'scope)) (lambda (v x) (let-set! (cdr v) 'scope x))))
    (define var-match-list (dilambda (lambda (v) (let-ref (cdr v) 'match-list)) (lambda (v x) (let-set! (cdr v) 'match-list x))))
    
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
      (letrec ((rem-dup
		(lambda (lst nlst)
		  (cond ((null? lst) nlst)
			((and (member (car lst) nlst)
			      (not (and (pair? (car lst))
					(side-effect? (car lst) env))))
			 (rem-dup (cdr lst) nlst))
			(else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
	(reverse (rem-dup lst ()))))

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
      (if (and (< count 2)
	       (pair? tree)
	       (not (eq? (car tree) 'quote)))
	  (tree-count1 x (cdr tree)
		      (+ (tree-count1 x (car tree) (+ count (if (eq? x (car tree)) 1 0)))
			 (if (eq? x (cdr tree)) 1 0)))
	  count))

    (define (tree-count2 x tree count)
      (if (and (< count 3)
	       (pair? tree)
	       (not (eq? (car tree) 'quote)))
	  (tree-count2 x (cdr tree)
		      (+ (tree-count2 x (car tree) (+ count (if (eq? x (car tree)) 1 0)))
			 (if (eq? x (cdr tree)) 1 0)))
	  count))

    (define (tree-length tree len)
      (if (pair? tree)
	  (tree-length (car tree) (tree-length (cdr tree) len))
	  (if (null? tree)
	      len
	      (+ len 1))))

    (define (tree-length-to tree len)
      (let loop ((x tree) (i 0))
	(if (pair? x)
	    (loop (car x) (loop (cdr x) i))
	    (if (or (null? x)
		    (>= i len))
		i
		(+ i 1)))))

    (define (proper-tree? tree)
      (or (not (pair? tree))
	  (and (proper-list? tree)
	       (every? proper-tree? (cdr tree)))))

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

    (define (maker? tree)
      (tree-set-member #f makers tree))
    
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
      (and (not (symbol? x))
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
	   (let ((fd (or (var-member fnc env)
			 (hash-table-ref globals fnc))))
	     (if (var? fd)
		 (and (symbol? (var-ftype fd))
		      (var-signature fd))
		 (let ((f (symbol->value fnc *e*)))
		   (and (procedure? f)
			(procedure-signature f)))))))

    (define (arg-arity fnc env)
      (and (symbol? fnc)
	   (let ((fd (or (var-member fnc env)
			 (hash-table-ref globals fnc))))
	     (if (var? fd)
		 (and (not (eq? (fdata 'decl) 'error))
		      (arity (fdata 'decl)))
		 (let ((f (symbol->value fnc *e*)))
		   (and (procedure? f)
			(arity f)))))))

    (define (dummy-func name form f)
      (catch #t 
	(lambda ()
	  (eval f))
	(lambda args
	  (lint-format "in ~A, ~A" name form (apply format #f (cadr args))))))

    (define (count-values body)
      (let ((mn #f)
	    (mx #f))
	(define (counter ignored tree) ; 'ignored is for member's benefit
	  (if (pair? tree)
	      (if (eq? (car tree) 'values)
		  (let ((args (length (cdr tree))))
		    (for-each (lambda (p)
				(if (and (pair? p)
					 (eq? (car p) 'values))
				    (set! args (+ args (- (length (cdr p)) 1)))))
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

    (define* (make-fvar name ftype arglist decl initial-value env)
      (let ((old (hash-table-ref other-identifiers name)))
      (cons name 
	    (inlet 'signature (let ((body (and (memq ftype '(define define* lambda lambda* let))
					       (cddr initial-value))))
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
					  (cddr initial-value)))

		   'allow-other-keys (and (pair? arglist)
					  (memq ftype '(define* define-macro* define-bacro* defmacro*))
					  (eq? (last-par arglist) 'allow-other-keys))
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
	     (else #t))))

    (define (->type c)
      (if (pair? c)
	  (if (symbol? (car c))
	      (if (eq? (car c) 'quote)
		  (if (symbol? (cadr c))
		      'symbol?
		      (->simple-type (cadr c)))   ; don't look for return type!
		  (or (return-type (car c) ())
		      (define->type c)))
	      (or (pair? (car c)) 'pair?))
	  (->simple-type c)))
    
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
	    ((procedure?)        (memq type2 '(dilambda? iterator? macro?)))
	    ((macro?)            (memq type2 '(dilambda? iterator? procedure?)))
	    ((iterator?)         (memq type2 '(dilambda? procedure? sequence?)))
	    ((string?)           (memq type2 '(byte-vector? sequence? directory? file-exists?)))
	    ((hash-table? let?)  (eq? type2 'sequence?))
	    ((byte-vector? directory? file-exists?) (memq type2 '(string? sequence?)))
	    ((input-port? output-port?) (eq? type2 'boolean?))
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
	    ((complex? number?) (memq type2 '(integer? rational? float? real? complex? number? negative? positive? zero? even? odd? exact? inexact? nan? infinite?)))
	    ((list?)            (memq type2 '(pair? null? proper-list?)))
	    ((proper-list?)     (eq? type2 'null?))
	    ((vector?)          (memq type2 '(float-vector? int-vector?)))
	    ((symbol?)          (memq type2 '(keyword? gensym? defined? provided? constant?)))
	    ((sequence?)        (memq type2 '(list? pair? null? proper-list? vector? float-vector? int-vector? byte-vector? 
					      string? let? hash-table? c-object? directory? file-exists?)))
	    ((char?)            (memq type2 '(char-whitespace? char-numeric? char-alphabetic? char-upper-case? char-lower-case?)))
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
    
    (define (side-effect? form env)
      ;; could evaluation of form have any side effects (like IO etc)
      
      (if (and (proper-list? form)                   ; we don't want dotted lists or () here
	       (not (null? form)))
	  ;; can't optimize ((...)...) because the car might eval to a function
	  (or (and (not (hash-table-ref no-side-effect-functions (car form)))
		   ;; if it's not in the no-side-effect table and ...
		   
		   (let ((e (or (var-member (car form) env) 
				(hash-table-ref globals (car form)))))
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
				    (if (not (and (symbol? arg)
						  (hash-table-ref no-side-effect-functions arg)))
					(return #t))))
				(cdr sig) (cdr form))
			       #f))))))))
	  
	  (and (symbol? form)
	       (let ((e (or (var-member form env) 
			    (hash-table-ref globals form))))
		 (if (var? e)
		     (symbol? (var-ftype e))
		     (and (procedure? (symbol->value form *e*))
			  (not (hash-table-ref no-side-effect-functions form))))))))
    
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
	   (or (and (not (and (pair? (car lst))
			      (side-effect? (car lst) env)))
		    (pair? (cdr lst))
		    (member (car lst) (cdr lst)))
	       (repeated-member? (cdr lst) env))))

    (define (set-ref name form env)
      ;; if name is in env, set its "I've been referenced" flag
      (let ((data (or (var-member name env) 
		      (hash-table-ref globals name))))
	(when (var? data)
	  ;; (format *stderr* "ref: ~A: ~A~%" data form)
	  (set! (var-ref data) (+ (var-ref data) 1))
	  (if (and form (not (memq form (var-history data))))
	      (set! (var-history data) (cons form (var-history data))))))
      env)
    
    (define (set-set name form env)
      (let ((data (or (var-member name env) 
		      (hash-table-ref globals name))))
	(when (var? data)
	  ;; (format *stderr* "set: ~A: ~A~%" data form)
	  (set! (var-set data) (+ (var-set data) 1))
	  (if (not (memq form (var-history data)))
	      (set! (var-history data) (cons form (var-history data))))
	  (set! (var-signature data) #f)
	  (set! (var-ftype data) #f))))

    
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
	    (format outport " ~A: ~A is no longer accepted: ~A~%" f kw 
		    (focus-str (object->string args) (symbol->string kw)))))

      (if (member 'pi args (lambda (a b) (or (eq? b 'pi) (and (pair? b) (eq? (car b) 'pi)))))
	  (format outport " ~A: parameter can't be a constant: ~A~%" f 
		  (focus-str (object->string args) "pi")))

      (let ((r (memq :rest args)))
	(when (pair? r)
	  (if (not (pair? (cdr r)))
	      (format outport " ~A: :rest parameter needs a name: ~A~%" f args)
	      (if (pair? (cadr r))
		  (format outport " ~A: :rest parameter can't specify a default value: ~A~%" f args)))))

      (let ((a (memq :allow-other-keys args)))
	(if (and (pair? a)
		 (pair? (cdr a)))
	    (format outport " ~A: :allow-other-keys should be at the end of the parameter list: ~A~%" f 
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
    
    
    (define (simplify-boolean in-form true false env)

      (define (classify e)
	(if (just-constants? e env)
	    (catch #t
	      (lambda ()
		(let ((val (eval e)))
		  (if (boolean? val)
		      val
		      e)))
	      (lambda ignore e))
	    e))
      
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
		   (memq type2 '(= char=? string=?)))
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
		      ;(format *stderr* "not: ~A ~A~%" arg1 arg2)
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
		     ((null?) (if (memq type2 '(list? proper-list?))
				  `(not (pair? ,(cadr arg1)))
				  arg2))
		     ((eof-object? keyword?) arg2)
		     ((string?) (and (not (eq? type2 'byte-vector?)) arg2))
		     (else 
		      ;(format *stderr* "not: ~A ~A~%" arg1 arg2)
		      #f))))))

      (define (bsimp x) ; quick check for common easy cases
	(set! last-simplify-boolean-line-number line-number)
	(if (and (pair? x)
		 (pair? (cdr x)))
	    (case (car x)
	      ((and) (and (cadr x)              ; (and #f ...) -> #f
			  x))
	      ((or) (if (and (cadr x)           ; (or #t ...) -> #t
			     (code-constant? (cadr x)))
			(cadr x)
			x))
	      (else 
	       (if (and (= (length x) 2)
			(pair? (cadr x))
			(symbol? (caadr x)))
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
				((negative?) (and (not (memq (caadr x) '(abs magnitude denominator gcd lcm char->integer byte-vector-ref byte-vector-set!)))
						  x))
				(else x)))))
		   x)))
	    x))

      (define (bcomp x) ; not so quick...
	(if (pair? x)
	    (if (eq? (car x) 'and)
		(call-with-exit
		 (lambda (return)
		   (let ((newx (list 'and)))
		     (do ((p (cdr x) (cdr p))
			  (sidex newx)
			  (endx newx))
			 ((null? p) newx)
		       (let ((next (car p)))
			 (if (not next)            ; #f in and -> end of expr
			     (if (eq? sidex newx)  ; no side-effects
				 (return #f)       
				 (begin
				   (set-cdr! endx (list #f))
				   (return newx)))
			     (if (or (code-constant? next)  ; (and ... true-expr ...)
				     (member next sidex))   ; if a member, and no side-effects since, it must be true
				 (if (and (null? (cdr p))
					  (not (equal? next (car endx))))
				     ;; here and below we're being careless because next might have a side-effect so it should be repeated,
				     ;;   but the other side appears to be much more common (inadvertent repetition)
				     (set-cdr! endx (list next))
				     (if (side-effect? next env) ; perhaps look for IO funcs?
					 (format outport " I'm just guessing here that this repetition is a typo...~%")))
				 (begin
				   (set-cdr! endx (list next))
				   (set! endx (cdr endx))
				   (if (side-effect? next env)
				       (set! sidex endx))))))))))

		(if (eq? (car x) 'or)
		    (call-with-exit
		     (lambda (return)
		       (let ((newx (list 'or)))
			 (do ((p (cdr x) (cdr p))
			      (sidex newx)
			      (endx newx))
			     ((null? p) newx)
			   (let ((next (car p)))
			     (if (and next                 ; (or ... #t ...)
				      (code-constant? next))
				 (begin
				   (set-cdr! endx (list next))
				   (return newx))          ; we're done since this is true
				 (if (or (not next)
					 (member next sidex)) ; so its false in some way
				     (if (and (null? (cdr p))
					      (not (equal? next (car endx))))
					 (set-cdr! endx (list next))
					 (if (side-effect? next env)
					     (format outport " I'm just guessing here that this repetition is a typo...~%")))
				     (begin
				       (set-cdr! endx (list next))
				       (set! endx (cdr endx))
				       (if (side-effect? next env)
					   (set! sidex endx))))))))))
		    x))
	    x))
				 
      ;; start of simplify-boolean code
      ;;   this is not really simplify boolean as in boolean algebra because in scheme there are many unequal truths, but only one falsehood
      ;;   'and and 'or are not boolean operators in a sense

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
				       (pair? (cdr arg))         ; this is usually internally generated, so the message about (and x #t) is in special-cases below
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
				  (let ((rest (cdr arg)))
				    (case (car arg)
				      ((<)            `(>= ,@rest))   ; (not (< ...)) -> (>= ...) 
				      ((>)            `(<= ,@rest))
				      ((<=)           (if (morally-equal? (cadr rest) 0.0) `(positive? ,(car rest)) `(> ,@rest)))
				      ((>=)           (if (morally-equal? (cadr rest) 0.0) `(negative? ,(car rest)) `(< ,@rest)))
				      ((char<?)       `(char>=? ,@rest))
				      ((char>?)       `(char<=? ,@rest))
				      ((char<=?)      `(char>? ,@rest))
				      ((char>=?)      `(char<? ,@rest))
				      ((char-ci<?)    `(char-ci>=? ,@rest))
				      ((char-ci>?)    `(char-ci<=? ,@rest))
				      ((char-ci<=?)   `(char-ci>? ,@rest))
				      ((char-ci>=?)   `(char-ci<? ,@rest))
				      ((string<?)     `(string>=? ,@rest))
				      ((string>?)     `(string<=? ,@rest))
				      ((string<=?)    `(string>? ,@rest))
				      ((string>=?)    `(string<? ,@rest))
				      ((string-ci<?)  `(string-ci>=? ,@rest))
				      ((string-ci>?)  `(string-ci<=? ,@rest))
				      ((string-ci<=?) `(string-ci>? ,@rest))
				      ((string-ci>=?) `(string-ci<? ,@rest))
				      ((odd?)         `(even? ,@rest))
				      ((even?)        `(odd? ,@rest))
				      ((exact?)       `(inexact? ,@rest))
				      ((inexact?)     `(exact? ,@rest))
				      ;; ((null?)     `(pair? ,@rest))
				      ;;    this is not quite right because (not (null? 3)) -> #t
				      ;; char-upper-case? and lower are not switchable here
				      ((zero?)       ; (not (zero? (logand p (ash 1 i)))) -> (logbit? p i)
				       (let ((zarg (car rest)))  ; (logand...)
					 (if (and (pair? zarg)
						  (eq? (car zarg) 'logand)
						  (pair? (cddr zarg))
						  (pair? (caddr zarg))
						  (eq? (caaddr zarg) 'ash)
						  (eqv? (cadr (caddr zarg)) 1))
					     `(logbit? ,(cadr zarg) ,(caddr (caddr zarg)))
					     form)))
				      (else form))))
				 
				 (else form)))
			 form))
		    
		    ((or)
					;(format *stderr* "or: ~A~%" form)
		     (case len
		       ((1) #f)
		       ((2) (if (code-constant? (cadr form)) (cadr form) (classify (cadr form))))
		       (else
			(call-with-exit
			 (lambda (return)
			   (when (= len 3)
			     (let ((arg1 (cadr form))
				   (arg2 (caddr form)))
			       
			       (when (and (pair? arg1)
					  (pair? arg2))
				 
				 (when (and ;(not (= line-number last-simplify-boolean-line-number))
					(eq? (car arg1) 'and)
					(eq? (car arg2) 'and)
					(= 3 (length arg1) (length arg2))
					;; (not (side-effect? arg1 env)) ; maybe??
					(or (equal? (cadr arg1) `(not ,(cadr arg2)))
					    (equal? `(not ,(cadr arg1)) (cadr arg2)))
					(not (equal? (caddr arg1) `(not ,(caddr arg2))))
					(not (equal? `(not ,(caddr arg1)) (caddr arg2))))
				   ;; kinda dumb, but common: (or (and A B) (and (not A) C)) -> (if A B C)
				   ;;    the other side: (and (or A B) (or (not A) C)) -> (if A C (and B #t)), but it never happens
					;(set! last-simplify-boolean-line-number line-number)
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
					;(format *stderr* "t1: ~A~%" t1)
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
					;(format *stderr* "subsumes: ~A~%" arg1)
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
					;(format *stderr* "t2: ~A~%" t2)
					 (when t2 
					   (if (eq? t2 'fatuous)
					       (return #t)
					       (if (pair? t2)
						   (return t2)))
					   )))))))
			   
			   ;; len > 3 or not caught above
			   ;; there are a few cases where the 'not-infested form is actually clearer
			   (if (every? (lambda (p)                ; (or (not x) (not y)) -> (not (and x y))
					 (and (pair? p)
					      (pair? (cdr p))
					      (eq? (car p) 'not)))
				       (cdr form))
			       (let ((sf (simplify-boolean `(and ,@(map cadr (cdr form))) true false env)))
				 (return (simplify-boolean `(not ,sf) () () env))))
			   
			   (let ((sym #f)
				 (eqf #f)
				 (vals ()))

			     (define (constant-arg p)
			       (if (code-constant? (cadr p))
				   (set! vals (cons (cadr p) vals))
				   (and (code-constant? (caddr p))
					(set! vals (cons (caddr p) vals)))))

			     (define (upgrade-eqf)
			       (if (memq eqf '(string=? string-ci=? = equal?))
				   (set! eqf 'equal?)
				   (set! eqf (if (memq eqf '(#f eq?)) 'eq? 'eqv?))))

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
						   (if (or (not eqf)
							   (eq? eqf (car p)))
						       (set! eqf (car p)) 
						       (set! eqf 'equal?))
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

							    ((or (memq leqf '(#t 'equal?))
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
					;(format *stderr* "end: ~A~%" new-form)
				    (return (and (pair? new-form)
						 (if (null? (cdr new-form))
						     (car new-form)
						     (if retry
							 (simplify-boolean `(or ,@(reverse new-form)) () () env)
							 `(or ,@(reverse new-form)))))))
				 (let* ((e (car exprs))
					(val (classify e)))
				   (if (and (pair? val)
					    (memq (car val) '(and or not)))
				       (set! val (classify (set! e (simplify-boolean e true false env)))))
				   
				   (if (not (or retry
						(equal? e (car exprs))))
				       (set! retry #t))
				   
				   (if val                                   ; #f in or is ignored
				       (cond ((or (eq? val #t)               ; #t or any non-#f constant in or ends the expression
						  (code-constant? val))
					      (set! new-form (if (null? new-form) ; (or x1 123) -> value of x1 first
								 (list val)
								 (cons val new-form)))
					      ;; reversed when returned
					      (set! exprs '(#t)))
					     
					     ((and (pair? e)                       ; (or ...) -> splice into current
						   (eq? (car e) 'or))
					      (set! exprs (append e (cdr exprs)))) ; we'll skip the 'or in do step
					     
					     ((not (memq val new-form))
					      (set! new-form (cons val new-form))))))))))))))
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
				    
				    (when (and (symbol? arg1)                       ; (and x (pair? x)) -> (pair? x)
					       (pair? arg2)
					       (pair? (cdr arg2))
					       (eq? arg1 (cadr arg2)))
				      (if (eq? (car arg2) 'not)
					  (return #f))
				      
				      (if (not (or (memq (car arg2) '(and or not list cons vector))
						   (side-effect? arg2 env)))
					  (let ((v (or (var-member arg1 env)
						       (hash-table-ref globals arg1))))
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
							(format outport " in ~A, perhaps change ~S to ~S~%"
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
				      
				      (if (and (reversible? (car arg1))
					       (pair? (cddr arg1))
					       (null? (cdddr arg1))
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
					    
					    (return
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
							  (format outport "in ~A~A, ~A is ~A, but ~A wants ~A"
								  (truncated-list->string form) 
								  (if ln (format #f " (line ~D)" ln) "")
								  (cadr arg1) 
								  (prettify-checker-unq (car arg1))
								  (car arg2)
								  (prettify-checker arg-type))))))))))
				      )))
				
				;; len > 3 or nothing was caught above
				(if (every? (lambda (a)                ; (and (not (pair? x)) (not (null? x))) -> (not (list? x))
					      (and (pair? a)
						   (eq? (car a) 'not)))
					    (cdr form))
				    (let ((nf (simplify-boolean `(or ,@(map cadr (cdr form))) () () env)))
				      (return (simplify-boolean `(not ,nf) () () env))))
				
				(if (every? (lambda (a)
					      (and (pair? a)
						   (eq? (car a) 'zero?)))
					    (cdr form))
				    (return `(= 0 ,@(map cadr (cdr form)))))
				
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
					  (set! val (classify (set! e (simplify-boolean e () false env)))))
				      
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
							   (eq? val (car new-form))))
						 (set! new-form (cons val new-form))))))))))))))))))))
    
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
			      (set! val (if (zero? y) 
					    (collect-if list (lambda (x) (not (number? x))) val)
					    (cons y (collect-if list (lambda (x) (not (number? x))) val))))))))
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
				  (not (and (pair? arg2)
					    (eq? (car arg2) '/))))
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
			       (if (and (pair? n) (null? (cdr n)))
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

			      ((and (eq? (caar args) 'random)
				    (eq? (car form) 'floor)
				    (float? (cadar args))
				    (= (floor (cadar args)) (cadar args)))
			       `(random ,(floor (cadar args))))

			      (else `(,(car form) ,@args))))
		       (else `(,(car form) ,@args)))
		 form))
	    
	    ((abs magnitude)
	     (if (= len 1)
		 (cond ((and (pair? (car args))        ; (abs (abs x)) -> (abs x)
			     (memq (caar args) '(abs magnitude denominator gcd lcm char->integer byte-vector-ref byte-vector-set!)))
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
		 (if (eqv? (car args) 0)
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
		     ;; not (inexact (random 3)) -> (random 3.0) because results are different
		     `(,(car form) ,@args))
		 form))
	    
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
		 ((and (pair? arg)
		       (eq? (car arg) 'string-append))
		  (if (null? (cddr arg))
		      (if (string? (cadr arg))
			  (cadr arg)
			  (values "~A" (cadr arg)))
		      (if (null? (cdddr arg))
			  (if (string? (cadr arg))
			      (values (string-append (cadr arg) "~A") (caddr arg))
			      (if (string? (caddr arg))
				  (values (string-append "~A" (caddr arg)) (cadr arg))
				  (values "~A" arg)))
			  (values "~A" arg))))
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
	(if (not (or (eq? sym f1)
		     (eq? sym f2)
		     (tree-member sym f1)
		     (tree-member sym f2)))
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
    
    (define (check-boolean-affinity name form env)
      ;; does built-in boolean func's arg make sense
      (if (not (or (not (= (length form) 2))
		   (symbol? (cadr form))
		   (= line-number last-simplify-boolean-line-number)))
	  (let ((expr (simplify-boolean form () () env)))
	    (if (not (equal? expr form))
		(lint-format "perhaps ~A" name (lists->string form expr)))
	    (if (and (pair? (cadr form))
		     (symbol? (caadr form)))
		(let ((rt (if (eq? (caadr form) 'quote)
			      (->simple-type (cadadr form))
			      (return-type (caadr form) env)))
		      (head (car form)))
		  (if (subsumes? head rt)
		      (lint-format "~A is always #t" name (truncated-list->string form))
		      (if (not (or (memq rt '(#t #f values))
				   (any-compatible? head rt)))
			  (lint-format "~A is always #f" name (truncated-list->string form)))))))))
	
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
	(let ((v (or (var-member p env)
		     (hash-table-ref globals p))))
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


    (define (check-special-cases name head form env)
      ;; here curlet won't change (leaving aside additions via define)
      ;; keyword head here if args to func/macro that we don't know about
      
      ;; TODO:
      ;; pp (if...) as func par ->cr+indent if long?, and look-ahead for long pars and cr+indent all, also (lambda...)
      ;;   pp sketch mode would be useful, and truncate at right margin
      ;; unquasiquote innards pretty-printed and check quotes, doubled ,@
      ;; #_{list} to check quasiquote (unquote=extra comma, quote where bad for op = missing comma)
      ;; nvals to values as a range, checkable if called -- needed in check-args
      ;;
      ;; for macros (or unknown ids?) ending in !, perhaps scan body for 'set! or just assume cadr is the target?
      ;; call/exit if all (any?) returns are at exits
      ;; hash-table cons -- this is an arg to make-iterator
      ;; define-class and define-record-type for the function names
      ;; other-idents needs to take require into account
      ;; what about (let () (define...))? or define*/lambda*
      ;; or|and nots with reversible ops -- if more than (2?) nots, reverse and rewrite?
      ;; perhaps keep syntax junk as var with definer 'syntax so we don't make any assumptions about it
      ;; why aren't definer and ftype the same field? -- using ftype to see that var is a function apparently
      ;;  if (define f (lambda*...)) -- ftype is define?
      ;; func 1 expr reversible -- add to reversible ops?
      ;;
      ;; *lint-hook* could pass the current form to each function and let it do special analysis
      ;;    maybe specialize on the name?
      ;;    *linters* -> a list of car/function
      ;;    or a linter local (like signature) that gets run whenever we check a call to that function -- could check bounds etc
      ;;
      ;; t354/t359 lint for-each tests [extend...]
      ;;
      ;; ---- from structures-equal:
      ;; to find possible function perhaps save lets (via hash+count?), look for structure equality in that set? [save lets that aren't already functions?]
      ;;   or if function found later, note possible change of scope?
      ;;   count above=var num in bindings -> args in func
      ;;   then if match, perhaps some of these match even in constants, so can be relet/unpard
      ;;   the match would happen in report-usage?
      ;; for possible, hash on car, keep tree-len + pointers to matches
      ;; here can we assume car is not a par and check it before tree count or anything?
      ;; could also hash the built-ins in this way:
      ;;   use code-equal? to catch built-in procedure-source possibilities, maybe use rather than explicit matching (as in fill! etc)
      ;;   also code-equal? needs named-let* support
      ;;   should code-equal? try to match internal defines? (symbol as well as func)
      ;;   what about simple syntactic equalities: (if a b c) <-> (cond (a b) (else c)), (if a (begin ...)) <-> (when a ...)
      ;;   or not so simple: named-let <-> define/1
      ;;   also need code-equal? support for all the varieties of define and lambda*
      ;;   also the type of the arg matters: symbol does not match pair so negative/#f len = don't match code, only complete forms
      ;;
      ;; ---- from report-usage:
      ;; parameter initial value is currently #f which should be ignored in typing
      ;; parameter vals from func calls? and fixup the initial-value from first history setting?
      ;;
      ;; reverse! missing type checks
      ;; ref count can be confused by with-let if it's a reference to the current env (as in Display in stuff.scm)
      ;; set subsumes type -> new ntype
      ;;
      ;; catch direct access when accessor exists? -- (*-x z) (accessor z n) -> watch for (accessor zz n) where zz value is make-*?
      ;;   if vector-ref|set v -- var? v -- value = make-* and *-ref|set! equivalent exists
      ;; accessor: all refs involve (sym-*) or (make-sym...) or (sym?) but then there's *-ref or implicit index
      ;; on setters, types of values can be compared, then used with readers
      ;;
      ;; 310/52 (no func), 1159/151
      ;;     53            1267/167

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
		     (if (or (symbol? target)
			     (and (pair? target)
				  (not (eq? (car target) 'quote))))
			 (set! target (list 'quote target)))
		     (lint-format "perhaps ~A" name (lists->string form `(,(cadr iter-eqf) ,selector ,target))))
		   
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
			       (lint-format "duplicated entry ~S in ~A" name (car baddy) items))))
		     
		     (if (and (symbol? (car selector-eqf))
			      (not (eq? (car selector-eqf) current-eqf)))
			 (lint-format "~A: perhaps ~A -> ~A" name (truncated-list->string form) head 
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
					    (not (and (pair? map-items)
						      (eq? (car map-items) 'quote))))
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
	 
	 (if (not (= line-number last-simplify-cxr-line-number))
	     ((lambda* (cxr arg)
		(when cxr
		  (set! last-simplify-cxr-line-number line-number)
		  (cond ((< (length cxr) 5)
			 (lint-format "perhaps ~A" name 
				      (lists->string form `(,(string->symbol (string-append "c" cxr "r")) ,arg))))
			
			;; if it's car|cdr followed by cdr's, use list-ref|tail
			((not (char-position #\a cxr))
			 (lint-format "perhaps ~A" name (lists->string form `(list-tail ,arg ,(length cxr)))))
			
			((not (char-position #\a (substring cxr 1)))
			 (lint-format "perhaps ~A" name (lists->string form `(list-ref ,arg ,(- (length cxr) 1)))))
			
			(else (set! last-simplify-cxr-line-number -1)))))
	      (combine-cxrs form)))

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
	 (when (= (length form) 3)
	   (let ((target (cadr form)))
	     (if (pair? target)
		 (case (car target)
		   
		   ((list-tail)                              ; (set-car! (list-tail x y) z) -> (list-set! x y z)
		    (lint-format "perhaps ~A" name (lists->string form `(list-set! ,(cadr target) ,(caddr target) ,(caddr form)))))
		   
		   ((cdr cddr cdddr cddddr)
		    (set! last-simplify-cxr-line-number line-number)
		    (if (and (pair? (cadr target))
			     (memq (caadr target) '(cdr cddr cdddr cddddr)))
			(lint-format "perhaps ~A" name       ; (set-car! (cdr (cddr x)) y) -> (list-set! x 3 y)
				     (lists->string form `(list-set! ,(cadadr target)
								     ,(+ (cdr-count (car target)) (cdr-count (caadr target))) 
								     ,(caddr form))))
			(lint-format "perhaps ~A" name       ; (set-car! (cdr x) y) -> (list-set! x 1 y)
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
		   (lint-format "perhaps ~A" name (lists->string form val))))))
	 
	((and or)
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
				      (lint-format "perhaps ~A" name 
						   (truncated-lists->string form (or (null? (cdr new-args))
										     `(= ,@(reverse new-args))))))))
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
	 (if (pair? (cdr form))
	     (if (and (pair? (cadr form))
		      (memq (caadr form) '(reverse reverse! list->vector vector->list list->string string->list let->list)))
		 (lint-format "perhaps ~A" name (lists->string form `(length ,(cadadr form))))
		 (if (code-constant? (cadr form))
		     (lint-format "perhaps ~A -> ~A" name 
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
		   (lint-format "perhaps ~A" name 
				(lists->string form
					       (let ((op '((zero? = zero?) (positive? > negative?) (negative? < positive?))))
						 (if (null? (cddr arg))
						     `(,(caddr (assq head op)) ,(cadr arg))
						     (if (null? (cdddr arg))
						       `(,(cadr (assq head op)) ,(cadr arg) ,(caddr arg))
						       `(,(cadr (assq head op)) ,(cadr arg) (+ ,@(cddr arg))))))))))))

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
		     (lint-format "exit-function ~A appears to be unused: ~A" name return (truncated-list->string form))
		     (let ((last (and (proper-list? body)
				      (list-ref body (- (length body) 1)))))
		       (if (and (pair? last)
				(eq? (car last) return))
			   (lint-format "~A is redundant here: ~A" name return (truncated-list->string last)))))))))
	
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
		 (if (not (or (eq? continuation (car body))
			      (tree-set-member continuation '(lambda lambda* define define* curlet error apply) body)))
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
	     (lint-format "~A could be ~A" name (truncated-list->string form) (cadr form))
	     (if (and (pair? (cdr form)) (equal? (cadr form) '(owlet)))
		 (lint-format "~A could be (owlet): owlet is copied internally" name form))))
	
	;; ----------------
	((string-copy)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (memq (caadr form) '(copy string-copy)))
	     (lint-format "~A could be ~A" name (truncated-list->string form) (cadr form))))
	
	;; ----------------
	((string)
	 (if (every? (lambda (x) 
		       (and (char? x)
			    (char<=? #\space x #\~))) ; #\0xx chars here look dumb
		     (cdr form))
	     (lint-format "~A could be ~S" name (truncated-list->string form) (apply string (cdr form)))))
	
	;; ----------------
	((string? number?)
	 (if (and (pair? (cdr form))
		  (pair? (cadr form))
		  (eq? (caadr form) (if (eq? head 'string?) 'number->string 'string->number)))
	     (lint-format "perhaps ~A" name (lists->string form (cadr form)))
	     (check-boolean-affinity name form env)))
	
	;; ----------------
	((symbol? integer? rational? real? complex? float? keyword? gensym? byte-vector? list? proper-list?
		  char? boolean? float-vector? int-vector? vector? let? hash-table? input-port? null? pair? c-object?
		  output-port? iterator? continuation? dilambda? procedure? macro? random-state? eof-object? c-pointer?)
	 (check-boolean-affinity name form env))

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
		      (lint-format "redundant ~A: ~A" name head (truncated-list->string form)))

		     ((and (pair? target)              ; (vector-set! (vector-ref x 0) 1 2) -- vector within vector
			   (not (eq? head 'string-set!))
			   (memq (car target) '(vector-ref list-ref hash-table-ref let-ref float-vector-ref int-vector-ref)))
		      (lint-format "perhaps ~A" name (lists->string form `(set! (,@(cdr target) ,index) ,val))))

		     ((and (pair? (cadr form))         ; (vector-set! (make-vector 3) 1 1) -- does this ever happen?
			   (memq (caadr form) '(make-vector vector make-string string make-list list append cons vector-append copy inlet sublet)))
		      (lint-format "~A is simply discarded; perhaps ~A" name
				   (truncated-list->string (cadr form))
				   (lists->string form val)))

		     ((code-constant? (cadr form))     ; (vector-set! #(0 1 2) 1 3)??
		      (lint-format "~A is a constant that is discarded; perhaps ~A" name (cadr form) (lists->string form val)))
		     ))))
	
	;; ----------------
	((object->string)
	 (if (pair? (cdr form))
	     (if (and (pair? (cadr form))
		      (eq? (caadr form) 'object->string))
		 (lint-format "~A could be ~A" name (truncated-list->string form) (cadr form))
		 (if (pair? (cddr form))
		     (let ((arg2 (caddr form)))
		       (if (or (and (keyword? arg2)
				    (not (eq? arg2 :readable)))
			       (and (code-constant? arg2)
				    (not (boolean? arg2))))
			   (lint-format "bad second argument: ~A" name arg2)))))))
	
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
	     (lint-format "~A won't create an homogeneous vector" name form)))
	
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
	       
	       ;; TODO: some of these are currently ignoring the start/end args -- maybe move out the list->vector constant and check (null? (cddr arg))?
	       
	       (cond ((eq? func-of-arg inv-op)               ; (vector->list (list->vector x)) -> x
		      (if (eq? head 'string->symbol)
			  (lint-format "perhaps ~A" name (lists->string form arg-of-arg))
			  (lint-format "~A could be (copy ~S)" name form arg-of-arg)))
		     
		     ((and (eq? head 'list->string)          ; (list->string (vector->list x)) -> (copy x (make-string (length x)))
			   (eq? func-of-arg 'vector->list))
		      (lint-format "perhaps ~A" name (lists->string form `(copy ,arg-of-arg (make-string (length ,arg-of-arg))))))
		     
		     ((and (eq? head 'list->vector)          ; (list->vector (string->list x)) -> (copy x (make-vector (length x)))
			   (eq? func-of-arg 'string->list))
		      (lint-format "perhaps ~A" name (lists->string form `(copy ,arg-of-arg (make-vector (length ,arg-of-arg))))))
		     
		     ((and (eq? head 'vector->list)          ; (vector->list (make-vector ...)) -> (make-list ...)
			   (eq? func-of-arg 'make-vector))
		      (lint-format "perhaps ~A" name (lists->string form `(make-list ,@(cdr arg)))))
		     
		     ((and (eq? head 'list->vector)          ; (list->vector (make-list ...)) -> (make-vector ...)
			   (eq? func-of-arg 'make-list))
		      (lint-format "perhaps ~A" name (lists->string form `(make-vector ,@(cdr arg)))))
		     
		     ((and (memq func-of-arg '(reverse reverse! copy))
			   (pair? (cadr arg))                ; (list->string (reverse (string->list x))) -> (reverse x)
			   (eq? (caadr arg) inv-op))
		      (lint-format "perhaps ~A" name (lists->string form `(,(if (eq? func-of-arg 'reverse!) 'reverse func-of-arg) ,(cadadr arg)))))
		     
		     ((and (pair? (cadr arg))
			   (memq func-of-arg '(cdr cddr cdddr cddddr list-tail))
			   (or (and (eq? head 'list->string)
				    (eq? (caadr arg) 'string->list))
			       (and (eq? head 'list->vector)
				    (eq? (caadr arg) 'vector->list))))
		      (let ((len-diff (if (eq? func-of-arg 'list-tail)
					  (caddr arg)
					  (cdr-count func-of-arg))))
			(lint-format "perhaps ~A" name 
				     (lists->string form (if (eq? head 'list->string)
							     `(substring ,(cadadr arg) ,len-diff)
							     `(copy ,(cadadr arg) (make-vector (- (length ,(cadadr arg)) ,len-diff))))))))
		     
		     ((and (memq head '(list->vector list->string))
			   (eq? func-of-arg 'sort!)
			   (pair? (cadr arg))
			   (eq? (caadr arg) (if (eq? head 'list->vector) 'vector->list 'string->list)))
		      (lint-format "perhaps ~A" name (lists->string form `(sort! ,(cadadr arg) ,(caddr arg)))))
		     
		     ((and (memq head '(list->vector list->string))
			   (or (memq func-of-arg '(list cons))
			       (quoted-undotted-pair? arg)))
		      (let ((maker (if (eq? head 'list->vector) 'vector 'string)))
			(cond ((eq? func-of-arg 'list)
			       (if (var-member maker env)
				   (lint-format "~A could be simplified, but you've shadowed '~A" name (truncated-list->string form) maker)
				   (lint-format "perhaps ~A" name (lists->string form `(,maker ,@(cdr arg))))))
			      ((eq? func-of-arg 'cons)
			       (if (or (null? (caddr arg))
				       (quoted-null? (caddr arg)))
				   (if (var-member maker env)
				       (lint-format "~A could be simplified, but you've shadowed '~A" name (truncated-list->string form) maker)
				       (lint-format "perhaps ~A" name (lists->string form `(,maker ,(cadr arg)))))))
			      ((or (null? (cddr form))
				   (and (integer? (caddr form))
					(or (null? (cdddr form))
					    (integer? (cadddr form)))))
			       (lint-format "perhaps ~A" name 
					    (lists->string form (apply (if (eq? head 'list->vector) vector string) (cadr arg))))))))
		     
		     ((and (eq? head 'list->string)          ; (list->string (reverse x)) -> (reverse (apply string x))
			   (memq func-of-arg '(reverse reverse!)))
		      (lint-format "perhaps ~A" name (lists->string form `(reverse (apply string ,arg-of-arg)))))

		     ((and (pair? arg-of-arg)                ; (op (reverse (inv-op x))) -> (reverse x)
			   (eq? func-of-arg 'reverse)
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
		 (lint-format "perhaps ~A -> ~A" name (truncated-list->string form) (apply string->list (cdr form))))))
	 
	 (when (and (memq head '(vector->list string->list))
		    (pair? (cddr form))
		    (pair? (cdddr form))
		    (equal? (caddr form) (cadddr form)))
	   (lint-format "leaving aside errors, ~A is ()~%" name (truncated-list->string form)))

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
		    (eq? (caadr form) (cond ((assq head inverses) => cdr))))
	       (lint-format "~A could be ~A" name (truncated-list->string form) (cadadr form))
	       (if (and (eq? head 'integer->char)
			(pair? (cdr form))
			(integer? (cadr form))
			(or (<= 32 (cadr form) 127)
			    (memv (cadr form) '(0 7 8 9 10 13 27))))
		   (lint-format "perhaps ~A -> ~W" name (truncated-list->string form) (integer->char (cadr form)))))))
	
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
		   ((0)					              ; (append) -> ()
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
						      (if (not (or (char-numeric? (string-ref str k))
								   (char=? (string-ref str k) #\,)))
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

	       (when (pair? args)
		 (for-each
		  (lambda (a)
		    (if (and (pair? a)
			     (memq (car a) '(number->string symbol->string)))
			(if (null? (cddr a))
			    (lint-format "format arg ~A could be ~A" name a (cadr a))
			    (if (and (pair? (cddr a))
				     (integer? (caddr a))
				     (memv (caddr a) '(2 8 10 16)))
				(if (= (caddr a) 10)
				    (lint-format "format arg ~A could be ~A" name a (cadr a))
				    (lint-format "format arg ~A could use the format directive ~~~A and change the argument to ~A" name a
						 (case (caddr a) ((2) "B") ((8) "O") (else "X"))
						 (cadr a)))))))
		  args))

	       (when (and (eq? head 'format)
			  (string? (cadr form)))
		 (lint-format "please include the port argument to format, perhaps ~A" name `(format () ,@(cdr form))))

	       (if (not (string? control-string))
		   (if (not (proper-list? args))
		       (lint-format "~S looks suspicious" name form))
		   (let ((ndirs (count-directives control-string name form))
			 (nargs (if (list? args) (length args) 0)))
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
			     (lint-format "~A could be ~S, (format is a no-op here)" name (truncated-list->string form) (caddr form)))))))))
	
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
		   (lint-format "perhaps ~A -> ~S" name (truncated-list->string form) val)))
	       (lambda (type info)
		 (lint-format "~A -> ~A~%" name (truncated-list->string form) (apply format #f info))))
	     
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
		   (lint-format "perhaps clearer: ~A" name (lists->string form `(copy ,str))))
	       (if (and (pair? (cdddr form))
			(equal? (caddr form) (cadddr form)))
		   (lint-format "leaving aside errors, ~A is \"\"" name form)))))
	
	;; ----------------
	((list-tail)
	 (if (= (length form) 3)
	     (if (eqv? (caddr form) 0)
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
		      (cond ((not (and arg1 arg2))
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
		 
		 (when (eq? head 'map)
		   (when (and (memq func '(char-downcase char-upcase))
			      (pair? (caddr form))
			      (eq? (caaddr form) 'string->list))
		     (lint-format "perhaps ~A" name (lists->string form `(string->list (,(if (eq? func 'char-upcase) 'string-upcase 'string-downcase) 
										      ,(cadr (caddr form)))))))
		   (when (identity? func) ; to check f here as var is more work
		     (lint-format "perhaps ~A" name (lists->string form (caddr form)))))
		 
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
			   (lint-format "perhaps ~A" name 
					(lists->string form `(format ,op ,(string-append "~{" ctrl-string "~}") ,(caddr form)))))))))
		 ))))
	
	;; ----------------
	((magnitude)
	 (if (and (= (length form) 2)
		  (memq (->type (cadr form)) '(integer? rational? real?)))
	     (lint-format "perhaps use abs here: ~A" name form)))
	
	;; ----------------
	((null eq eqv equal character?) ; (null (cdr...)) 
	 (if (not (or (var-member head env)
		      (hash-table-ref globals head)))
	     (lint-format "misspelled '~A? in ~A?" name head form)))

	((interaction-environment)
	 (lint-format "interaction-environment is probably curlet in s7" name))

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
	 (if (member 'values (cdr form) (lambda (a b)
					  (and (pair? b)
					       (eq? (car b) 'values))))
	     (lint-format "perhaps ~A" name (lists->string form `(values ,@(splice-if (lambda (x) (eq? x 'values)) (cdr form)))))
	     (if (= (length form) 2)
		 (lint-format "perhaps ~A" name (lists->string form (cadr form))))))
	
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
				    name
				    (truncated-list->string consumer)
				    clen clen
				    (truncated-list->string producer)
				    ((if (> (car consumed-values) (car produced-values)) car cadr) produced-values)))))

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
				    (lint-format "perhaps ~A" name 
						 (lists->string form 
								(if (and (pair? body)
									 (eq? (car body) 'values))
								    `(,consumer ,@(cdr body))
								    `(,consumer ,(caddr producer))))))))
			     (else (lint-format "perhaps ~A" name (lists->string form `(,consumer (,producer))))))
		       (lint-format "perhaps ~A" name (lists->string form `(,consumer (,producer)))))))))
	
	;; ----------------
	((multiple-value-bind) 
	 (if (= (length form) 4)
	     (let ((vars (cadr form))
		   (producer (caddr form))
		   (body (cdddr form)))

	       (if (null? vars)
		   (lint-format "this multiple-value-bind is pointless; perhaps ~A" name
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
					  name args 
					  (truncated-list->string producer)
					  (if (< args (car vals)) (car vals) (cadr vals))))

			 (if (and (pair? producer)
				  (symbol? (car producer))
				  (not (memq (return-type (car producer) ()) '(#t #f values))))
			     (lint-format "~A does not return multiple values" name (car producer))
			     (if (and (null? (cdr body))
				      (pair? (car body))
				      (equal? vars (cdar body))
				      (defined? (caar body))
				      (equal? (arity (symbol->value (caar body))) (cons args args)))
				 (lint-format "perhaps ~A" name (lists->string form `(,(caar body) ,producer)))))))))))
	
	;; ----------------
	((eval)
	 (when (= (length form) 2)
	   (let ((arg (cadr form)))
	     (cond ((not (pair? arg))
		    (if (not (symbol? arg))
			(lint-format "this eval is pointless; perhaps ~A" name (lists->string form (cadr form)))))

		   ((eq? (car arg) 'quote)
		    (lint-format "perhaps ~A" name (lists->string form (cadadr form))))

		   ((eq? (car arg) 'string->symbol)
		    (lint-format "perhaps ~A" name (lists->string form (string->symbol (cadadr form)))))

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
	     (lint-format "perhaps ~A -> ~A" name (truncated-list->string form) (string-length (cadr form)))))
	
	;; ----------------
	((vector-length)
	 (if (and (= (length form) 2)
		  (vector? (cadr form)))
	     (lint-format "perhaps ~A -> ~A" name (truncated-list->string form) (vector-length (cadr form)))))

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
				     gc-stats symbol-table-locked? c-objects history-size profile-info))))
		   (lint-format "unknown *s7* field: ~A" name (cadr form))))))


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

		(define (check-arg expr)
		  (unless (symbol? expr)
		    (let ((op (->type expr)))
		      (if (not (or (memq op '(#f #t values))
				   (any-compatible? checker op)))
			  (report-arg-trouble name form head arg-number checker expr op)))))
		
		(if (and (pair? arg)
			 (pair? (car arg)))
		    (let ((rtn (return-type (caar arg) env)))
		      (if (memq rtn '(boolean? real? integer? rational? number? complex? float? pair? keyword? symbol? null? char?))
			  (lint-format "~A's argument ~A looks odd: ~A returns ~A which is not applicable"
				       name head arg (caar arg) rtn))))

		(when (or (pair? checker)
			  (symbol? checker)) ; otherwise ignore type check on this argument (#t -> anything goes)
		  (if arg
		      (if (eq? checker 'unused-parameter?)
			  (lint-format "~A's parameter ~A is not used, but a value is passed: ~S" name head arg-number arg)
			  (if (eq? checker 'unused-set-parameter?)
			      (lint-format "~A's parameter ~A's value is not used, but a value is passed: ~S" name head arg-number arg))))
		  
		  (if (pair? arg)                  ; arg is expr -- try to guess its type
		      (case (car arg) 
			((quote)   ; '1 -> 1
			 (let ((op (if (pair? (cadr arg)) 'list? (->type (cadr arg)))))
			   ;; arg is quoted expression
			   (if (not (or (memq op '(#f #t values))
					(any-compatible? checker op)))
			       (report-arg-trouble name form head arg-number checker arg op))))
			  
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
			   (let ((v (or (var-member (car arg) env)
					(hash-table-ref globals (car arg)))))
			     (if (and (var? v)
				      (not (memq form (var-history v))))
				 (set! (var-history v) (cons form (var-history v)))))

			   ;; checker is arg-type, op is expression type (can also be a pair)
			   (if (and (not (memq op '(#f #t values)))
				    (or (not (any-compatible? checker op))
					(and (just-constants? arg env) ; try to eval the arg
					     (catch #t 
					       (lambda ()
						 (not (any-checker? checker (eval arg))))
					       (lambda ignore-catch-error-args
						 #f)))))
			       (report-arg-trouble name form head arg-number checker arg op)))))
		      
		      ;; arg is not a pair
		      (if (not (or (symbol? arg)
				   (any-checker? checker arg)))
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
	;; (format *stderr* "~A call: ~A~%" form fdata)
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
					   (lint-format "~A keyword argument ~A (in ~A) does not match any argument in ~S" name 
							head arg (truncated-list->string form) pargs))
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
						 (lint-format "~A keyword argument ~A (in ~A) does not match any argument in ~S" name 
							      head arg (truncated-list->string form) decls)))
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
				       (or (and (not (and (pair? (car lst))
							  (side-effect? (car lst) env)))
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
    
    (define (get-generator form)
      (let ((name (if (pair? (cadr form))
		      (caadr form)
		      (cadr form))))
	(let ((gen? (string->symbol (string-append (symbol->string name) "?")))
	      (gen-make (string->symbol (string-append "make-" (symbol->string name)))))
	  (hash-table-set! globals gen? (make-fvar :name gen?
						    :ftype 'define
						    :decl (dummy-func 'define `(define (,gen? x) (let? x)) '(define (_ x) #f))
						    :initial-value `(define (,gen? x) (let? x))
						    :arglist (list 'x)
						    :env ()))
	  (hash-table-set! globals gen? (make-fvar :name gen-make
						    :ftype 'define
						    :decl (dummy-func 'define* `(define* (,gen-make :rest x :allow-other-keys) (apply inlet x)) '(define (_ . x) #f))
						    :initial-value `(define* (,gen-make :rest x :allow-other-keys) (apply inlet x))
						    :arglist (list 'x)
						    :env ())))))
    
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
	   (hash-table-set! globals (cadr form) (make-var :name (cadr form) 
							  :definer head
							  :initial-value (and (pair? (cddr form)) (caddr form)))))
	  
	  ((defmacro defmacro*)
	   (hash-table-set! globals (cadr form) (make-fvar :name (cadr form) 
							   :ftype head
							   :decl (dummy-func head form (list head '_ (caddr form) #f))
							   :arglist (caddr form)
							   :initial-value form
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
						       :initial-value form
						       :env ())))
		       (lint-format "what is this? ~A" name form)))
		 (hash-table-set! globals name (make-var :name name 
							 :definer 'define
							 :initial-value (and (pair? (cddr form)) (caddr form)))))))
	  
	  ((define* define-expansion define-macro define-macro* define-bacro define-bacro* definstrument defanimal)
	   (hash-table-set! globals (caadr form) (make-fvar :name (caadr form) 
							    :ftype head
							    :decl (dummy-func head form (list head (cons '_ (cdadr form)) #f))
							    :arglist (cdadr form)
							    :initial-value form
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
	  
	    ((not (pair? binding)) 	   (lint-format "~A binding is not a list? ~S" name head binding) #f)
	    ((not (symbol? (car binding))) (lint-format "~A variable is not a symbol? ~S" name head binding) #f)
	    ((keyword? (car binding))	   (lint-format "~A variable is a keyword? ~S" name head binding) #f)
	    ((constant? (car binding))	   (lint-format "can't bind a constant: ~S" name binding) #f)
	    ((not (pair? (cdr binding)))
	     (lint-format (if (null? (cdr binding))
			      "~A variable value is missing? ~S" 
			      "~A binding is an improper list? ~S")
			  name head binding)
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
    
    (define (report-usage name type head vars env)
      ;; report unused or set-but-unreferenced variables, then look at the overall history
    
      (define (all-types-agree v)
	(let ((base-type (->type (var-initial-value v)))
	      (vname (var-name v)))
	  (every? (lambda (p)
		    (not (and (pair? p)
			      (eq? (car p) 'set!)
			      (eq? vname (cadr p))
			      (not (eq? base-type (->type (caddr p)))))))
		  (var-history v))))

      (if (and (not (eq? head 'begin)) ; begin can redefine = set a variable
	       (pair? vars)
	       (proper-list? vars))
	  (do ((cur vars (cdr cur))
	       (rst (cdr vars) (cdr rst)))
	      ((null? rst))
	    (let ((vn (var-name (car cur))))
	      (if (not (eq? vn lambda-marker))
		  (let ((repeat (var-member vn rst)))
		    ;; not globals here because the same name might be used as a global
		    (if repeat
			(lint-format "~A ~A ~A is declared twice" name head type (var-name (car cur)))))))))
      
      (for-each 
       (lambda (arg)
	 (let ((vname (var-name arg)))
	   
	   (if (hash-table-ref syntaces vname)
	       (lint-format "~A ~A named ~A is asking for trouble" name head type vname)
	       (if (not (symbol? vname))
		   (lint-format "bad ~A ~A name: ~S in ~S" name head type vname arg)))
#|
	   (format *stderr* "~A: ~A (~A/~A, ~S from ~S): ~{~^~%    ~S~}~%~%" 
		   type vname
		   (var-ref arg)
		   (var-set arg)
		   (->type (var-initial-value arg))
		   (var-initial-value arg)
		   (reverse (var-history arg)))
|#
	   (unless (eq? vname lambda-marker)
	     
	     (when *report-function-stuff*
	       (let ((scope (var-scope arg)))
		 (when (and (pair? scope)
			    (null? (cdr scope))
			    (memq (var-ftype arg) '(define lambda))
			    (not (var-member (car scope) env))
			    (var-member name env) ; not 'let etc
			    (not (eq? vname (car scope))))
		   (format outport " ~A is called only in ~A~%" vname (car (var-scope arg))))))

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
				   (lint-format "~A set, but not used: ~A" name 
						vname (truncated-list->string (car sets)))
				   (lint-format "~A set, but not used: ~{~S~^ ~}" name 
						vname sets))
			       (lint-format "~A set, but not used: ~A from ~A" name 
					    vname (truncated-list->string (var-initial-value arg)) (var-definer arg))))
			 
			 (if (not (memq vname '(documentation signature iterator?)))
			     (let ((val (if (pair? (var-history arg)) (car (var-history arg)) (var-initial-value arg)))
				   (def (var-definer arg)))
			       (if (symbol? def)
				   (if (eq? type 'parameter)
				       (lint-format "~A not used" name vname)
				       (lint-format "~A not used, initially: ~A from ~A" name 
						    vname (truncated-list->string val) def))
				   (lint-format "~A not used, value: ~A" name 
						vname (truncated-list->string val)))))))
		 
		 ;; not zero var-ref
		 (if (and (not (memq (var-definer arg) '(parameter named-let named-let*)))
			  (pair? (var-history arg))
			  (or (zero? (var-set arg))
			      (all-types-agree arg)))
		     (let ((type (->type (var-initial-value arg)))
			   (lit? (code-constant? (var-initial-value arg))))
		       
					;(format *stderr* "type: ~A, lit? ~A~%" type lit?)
		       
		       (do ((clause (var-history arg) (cdr clause)))
			   ((null? (cdr clause))) ; ignore the initial value which depends on a different env
			 (let ((call (car clause)))
			   (when (pair? call)
			     (let ((func (car call))
				   (arg1 (and (pair? (cdr call)) (cadr call))))
			       
			       ;; check for assignments into constants
			       (if (and lit?
					(or (and (memq func '(set-car! set-cdr! vector-set! list-set! string-set!))
						 (eq? arg1 vname))
					    (and (eq? func 'set!) ; implicit index presumably
						 (pair? arg1)
						 (eq? (car arg1) vname))))
				   (lint-format "~A's value, ~A, is a literal constant, so this set! is trouble: ~A" name 
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
					       (lint-format "~A is ~A, but ~A in ~A wants ~A" name
							    vname (prettify-checker-unq type)
							    func (truncated-list->string call) 
							    (prettify-checker desired-type))))))))
				 
				 ;; check for pointless type checks
				 (when (and (hash-table-ref bools func)
					    (not (eq? vname func)))
				   
				   (when (or (eq? type func)
					     (and (compatible? type func)
						  (not (subsumes? type func))))
				     (lint-format "~A is ~A, so ~A is #t" name vname (prettify-checker-unq type) call))
				   
				   (unless (compatible? type func)
				     (lint-format "~A is ~A, so ~A is #f" name vname (prettify-checker-unq type) call)))
				 
				 ;; (and (pair? x) (eq? x #\a)) etc
				 (when (and (memq func '(eq? eqv? equal?))
					    (or (and (code-constant? arg1)
						     (not (compatible? type (->type arg1))))
						(and (code-constant? (caddr call))
						     (not (compatible? type (->type (caddr call)))))))
				   (lint-format "~A is ~A, so ~A is #f" name vname (prettify-checker-unq type) call))
				 
				 ;; the usual eqx confusion
				 (when (memq type '(char? number? integer? real? float? rational? complex?))
				   (if (memq func '(eq? equal?))
				       (lint-format "~A is ~A, so ~A ~A be eqv? in ~A" name 
						    vname (prettify-checker-unq type) func
						    (if (eq? func 'eq?) "should" "could")
						    call)))
				 
				 ;; implicit index checks -- these are easily fooled by macros
				 (when (and (memq type '(vector? float-vector? int-vector? string? list? byte-vector?))
					    (pair? (cdr call)))
				   (when (eq? func vname)
				     (let ((init (var-initial-value arg)))
				       (if (not (compatible? 'integer? (->type arg1)))
					   (lint-format "~A is ~A, but the index ~A is ~A" name
							vname (prettify-checker-unq type)
							arg1 (prettify-checker (->type arg1))))
				       
				       (if (integer? arg1)
					   (if (negative? arg1)
					       (lint-format "~A's index ~A is negative" name vname arg1)
					       (let ((lim (if (code-constant? init)
							      (length init)
							      (if (memq (car init) '(vector float-vector int-vector string list byte-vector))
								  (length (cdr init))
								  (and (pair? (cdr init))
								       (integer? (cadr init))
								       (memq (car init) '(make-vector make-float-vector make-int-vector 
												      make-string make-list make-byte-vector))
								       (cadr init))))))
						 (if (and lim (>= arg1 lim))
						     (lint-format "~A has length ~A, but index is ~A" name vname lim arg1)))))
				       ))
				   
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
						 (lint-format "~A wants ~A, but the value in ~A is ~A" name
							      vname (prettify-checker-unq ref-type)
							      `(set! ,@(cdr call)) 
							      (prettify-checker val-type)))))
				       ))))
			       )
			     )))
		       
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
				   (lint-format "~A is not set, but ~{~A occurs ~A times~^, ~}" name
						vname repeats)))))
		       ))))))
       vars))
    
    
    (define (match-vars r1 r2 mat)
      (and (pair? r1)
	   (pair? r2)
	   (pair? (cdr r1))
	   (pair? (cdr r2))
	   (if (and (pair? (cadr r1))
		    (pair? (cadr r2))
		    (memq (caadr r1) '(let let* letrec letrec* do lambda define)))
	       (code-equal? (cadr r1) (cadr r2) mat)
	       (structures-equal? (cadr r1) (cadr r2) mat))
	   (cons (car r1) (car r2))))
	
    (define (code-equal? l1 l2 matches)
      (and (pair? l1)
	   (pair? l2)
	   ;(memq (car l1) '(let let* letrec letrec* do lambda define))
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
								 name                  ; append will splice out nil
								 matches)))))
			      ((let*)                                           ; refs move with the vars TODO: named let*
			       (and (= (length (car rest1)) (length (car rest2)))
				    (let ((new-matches matches))
				      (for-each (lambda (var1 var2)
						  (cond ((match-vars var1 var2 new-matches) =>
							 (lambda (v)
							   (set! new-matches (cons v new-matches))))
							(else (return #f))))
						(car rest1)
						(car rest2))
				      (structures-equal? (cdr rest1) (cdr rest2) new-matches))))
			      
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
						  (unless (structures-equal? (cddr var1) (cddr var2) new-matches)
						    (return #f)))
						(car rest1)
						(car rest2))
				      (structures-equal? (cdr rest1) (cdr rest2) new-matches))))
			      
			      ((letrec letrec*)                                ; ??? refs are local I think
			       (and (= (length (car rest1)) (length (car rest2)))
				    (let ((new-matches (append (map (lambda (var1 var2)
								      (cons (car var1) (car var2)))
								    (car rest1)
								    (car rest2))
							       matches)))
				      (for-each (lambda (var1 var2)
						  (unless (structures-equal? (cadr var1) (cadr var2) new-matches)
						    (return #f)))
						(car rest1)
						(car rest2))
				      (structures-equal? (cdr rest1) (cdr rest2) new-matches))))
			       
			      ((lambda define) ; for define we add the new name to matches before walking the body (shadow is immediate)
			       ;; (format *stderr* "~A ~A~%" rest1 rest2)
			       (if (symbol? (car rest1))
				   (and (symbol? (car rest2))
					(structures-equal? (cdr rest1) (cdr rest2)
							   (cons (cons (car rest1) (car rest2)) matches)))
				   (and (equal? (length (car rest1)) (length (car rest2))) ; (car rest2) might be a symbol, dotted lists ok here
					(structures-equal? (cdr rest1) (cdr rest2)
							   (append (map cons (proper-list (car rest1)) (proper-list (car rest2)))
								   matches)))))

			      (else #f)))))))))) ; can't happen I hope

    (define (structures-equal? l1 l2 matches)
      (if (pair? l1)
	  (and (pair? l2)
	       (if (and (pair? (car l1))
			(memq (caar l1) '(let let* letrec letrec* do lambda define)))
		   (code-equal? (car l1) (car l2) matches)        ; (if or above) need this first to make sure matches are correct (if shadowing)
		   (structures-equal? (car l1) (car l2) matches))
	       (structures-equal? (cdr l1) (cdr l2) matches))
	  (let ((match (assq l1 matches)))
	    (if match
		(or (and (eq? (cdr match) :unset)
			 (set-cdr! match l2))
		    (equal? (cdr match) l2))
		(equal? l1 l2)))))

    (define func-min-cutoff 6)
    (define func-max-cutoff 120)

    (define (function-match name form env)
      (if (>= (tree-length-to form func-min-cutoff) func-min-cutoff)
	  (let* ((leaves (tree-length-to form (+ func-max-cutoff 8)))
		 (cutoff (max func-min-cutoff (- leaves 12))))
	    (when (< leaves func-max-cutoff)

	      (let ((new-form (if (not (pair? (car form)))
				  (list form)
				  form))
		    (name-args #f)
		    (name-args-len :unset))

		(let ((v (var-member name env)))
		  (if (and (var? v)
			   (memq (var-ftype v) '(define lambda))
			   (or (eq? form (cddr (var-initial-value v)))    ; only check args if this is the complete body
			       (and (null? (cdddr (var-initial-value v))) 
				    (eq? form (caddr (var-initial-value v))))))
		       (if (symbol? (var-arglist v))
			   (begin
			     (set! name-args-len #f)
			     (set! name-args (list (var-arglist v))))
			   (begin
			     (set! name-args-len (length (var-arglist v)))
			     (set! name-args (proper-list (var-arglist v)))))))

		(do ((vs env (cdr vs)))
		    ((or (null? vs)
			 (let ((v (car vs)))
			   (and (not (eq? (var-name v) lambda-marker))
				(memq (var-ftype v) '(define lambda))
				(not (equal? name (var-name v)))
				(let ((body (cddr (var-initial-value v)))
				      (args (var-arglist v)))
				  (unless (var-leaves v)
				    (set! (var-leaves v) (tree-length body 0))
				    (set! (var-match-list v) (if (symbol? args)
								 (list (cons args :unset))
								 (map (lambda (arg)
									(cons arg :unset))
								      (proper-list args)))))
				  
				  ;; var-leaves is size of func (v) body
				  ;; leaves is size of form which we want to match with func
				  ;; func-min-cutoff avoids millions of uninteresting matches
				  (and (<= cutoff (var-leaves v) leaves)
				       (let ((match-list (do ((p (var-match-list v) (cdr p))) 
							     ((null? p) 
							      (var-match-list v))
							   (set-cdr! (car p) :unset))))
					 (and (structures-equal? body new-form match-list)
					      (not (member :unset match-list (lambda (a b) 
									       (eq? (cdr b) :unset))))
					      
					      (let ((new-args (map cdr match-list)))
						(if (and (equal? new-args name-args)
							 (equal? (length (var-arglist v)) name-args-len))
						    (lint-format "~A could be ~A" name name `(define ,name ,(var-name v)))
						    (lint-format "perhaps ~A" name (lists->string form `(,(var-name v) ,@new-args))))
						#t)))))))))))))))

    (define (lint-walk-body name head body env)
      ;; walk a body (a list of forms, the value of the last of which might be returned)
      ;; (format *stderr* "lint-walk-body ~A ~A ~A~%" name head body)
      
      (if (not (proper-list? body))
	  (lint-format "stray dot? ~A" name (truncated-list->string body))
	  
	  (let ((prev-f #f)
		(prev-fs #f)
		(prev-len 0)
		(f-len 0)
		(repeats 0)
		(start-repeats body)
		(repeat-arg 0)
		(block-fs #f)
		(dpy-f #f)
		(dpy-start #f)
		(len (length body)))
	    (if (eq? head 'do) (set! len (+ len 1))) ; last form in do body is not returned

	    ;; 2-step repeats are mainly display+newline and equivalents, which should be handled below as format calls

	    (when (and (pair? body)
		       *report-function-stuff*
		       (not (null? (cdr body))))
	      (function-match name body env))
	    
	    (do ((fs body (cdr fs))
		 (ctr 0 (+ ctr 1)))
		((not (pair? fs)))
	      (let ((f (car fs)))

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
			      (lint-format "perhaps ~A... ->~%    (do ((~A 0 (+ ~A 1))) ((= ~A ~D)) ~A)" name 
					   (truncated-list->string prev-f)
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
				    (lint-format "perhaps ~A... ->~%    (for-each ~S '(~{~S~^ ~}))" name
						 (truncated-list->string (car start-repeats))
						 func
						 (map unquoted (reverse args)))
				    (let ((v (or (var-member func-name env)
						 (hash-table-ref globals func-name))))
				      (if (or (and (var? v)
						   (memq (var-ftype v) '(define define* lambda lambda*)))
					      (procedure? (symbol->value func-name *e*)))
					  (lint-format "perhaps ~A... ->~%    (for-each ~S (vector ~{~S~^ ~}))" name 
						       ;; vector rather than list because it is easier on the GC (list copies in s7)
						       (truncated-list->string (car start-repeats))
						       func
						       (reverse args))
					  (if (and (not (var? v))
						   (not (macro? (symbol->value func-name *e*))))
					      (lint-format "assuming ~A is not a macro, perhaps ~A... ->~%    (for-each ~S (vector ~{~S~^ ~}))" name 
							   func-name
							   (truncated-list->string (car start-repeats))
							   func
							   (reverse args)))))))))))
		    (set! repeats 0)
		    (set! repeat-arg 0)
		    (set! start-repeats fs)))
		;; --------

		(if (pair? f)
		    (begin
		      (set! f-len (length f))
		      (if (eq? (car f) 'begin)
			  (lint-format "redundant begin: ~A" name (truncated-list->string f))))
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
					  (< (tree-length-to arg1 5) 5)))
				 (lint-format "perhaps ~A ~A ->~%~NC~A" name prev-f f 4 #\space
					      (object->string `(set! ,(cadr f) ,(tree-subst arg1 (cadr f) arg2))))))

			  (if (and (symbol? (cadr prev-f)) ; (set! x (A)) (set! y (A)) -> (set! x (A)) (set! y x)
				   (pair? arg1)            ;   maybe more trouble than it's worth
				   (equal? arg1 arg2)
				   (not (eq? (car arg1) 'quote))
				   (hash-table-ref no-side-effect-functions (car arg1))
				   (not (tree-member (cadr prev-f) arg1))
				   (not (side-effect? arg1 env))
				   (not (maker? arg1)))
			      (lint-format "perhaps ~A" name (lists->string f `(set! ,(cadr f) ,(cadr prev-f))))))))
		
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
		      (when (pair? f)
			(if (eq? (car f) 'map)
			    (lint-format "map could be for-each: ~A" name (truncated-list->string `(for-each ,@(cdr f))))
			    (if (eq? (car f) 'reverse!)
				(lint-format "~A might leave ~A in an undefined state; perhaps ~A" name (car f) (cadr f)
					     `(set! ,(cadr f) ,f)))))
		      
		      (if (not (side-effect? f env))
			  (lint-format "this could be omitted: ~A" name (truncated-list->string f))
			  (if (pair? f)
			      (if (eq? (car f) 'do) ; other syntactic forms almost never happen in this context
				  (let ((returned (if (and (pair? (cdr f))
							   (pair? (cddr f)))
						      (let ((end+res (caddr f)))
							(if (pair? (cdr end+res))
							    (list-ref (cdr end+res) (- (length end+res) 2)))))))
				    (if (not (or (eq? returned #<unspecified>)
						 (and (pair? returned)
						      (side-effect? returned env))))
					(lint-format "~A: result ~A is not used" name 
						     (truncated-list->string f) 
						     (truncated-list->string returned))))
				  (if (and (eq? (car f) 'format)
					   (pair? (cdr f))
					   (eq? (cadr f) #t))
				      (lint-format "perhaps use () with format since the string value is discarded:~%    ~A" 
						   name `(format () ,@(cddr f))))))))

		    ;; here f is the last form in the body
		    (when (and (pair? prev-f)
			       (pair? (cdr prev-f)))
		      
		      (if (and (memq (car prev-f) '(display write write-char write-byte))
			       (equal? f (cadr prev-f))
			       (not (side-effect? f env)))
			  (lint-format "this could be omitted: ~A" name (truncated-list->string f)))
		      
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
			    (lint-format "this could be omitted: ~A (~A returns its value)" name
					 (truncated-list->string f) (car prev-f)))
			
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
				 (or (memq (car f) '(car cdr))
				     (and (pair? (cddr f))
					  (equal? (caddr f) (caddr prev-f))
					  (pair? (cdddr prev-f))
					  (not (pair? (cddddr prev-f)))
					  (not (pair? (cdddr f)))
					  (not (side-effect? (caddr f) env)))))
			    (lint-format "this could be omitted: ~A (~A returns its value)" name 
					 (truncated-list->string f) (car prev-f))))))
	      
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
		(set! env (lint-walk name f env))

		;; need to put off this ref tick until we have a var for it (lint-walk above)
		(when (and (= ctr (- len 1))
			   (pair? f)
			   (pair? (cdr f)))
		  (if (and (pair? (cadr f))
			   (memq (car f) '(define define* define-macro define-constant define-macro* define-expansion define-bacro define-bacro*)))
		      (set-ref (caadr f) #f env)
		      (if (memq (car f) '(defmacro defmacro*))
			  (set-ref (cadr f) #f env))))
		))))
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
      ;; check out function arguments (adding them to the current env), then walk its body, (name == function name, val == body)
      ;; first check for (define (hi...) (ho...)) where ho has no opt args (and try to ignore possible string constant doc string)
      
      (when (eq? head 'define)
	(let ((bval (if (and (pair? val)
			     (string? (car val)))
			(cdr val)         ; strip away the (old-style) documentation string
			val)))
	  
	  (when (and (pair? bval)           ; not (define (hi a) . 1)!
		     (pair? (car bval))
		     (null? (cdr bval))
		     (symbol? (caar bval))) ; not (define (hi) ((if #f + abs) 0))
	    
	    (cond ((equal? args (cdar bval))
		   (let* ((cval (caar bval))
			  (p (symbol->value cval *e*))
			  (ary (arity p)))
		     (if (or (procedure? p)
			     (let ((e (or (var-member cval env) 
					  (hash-table-ref globals cval))))
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
			 (lint-format "~A~A could be (define ~A ~A)" name 
				      (if (and (procedure? p)
					       (not (= (car ary) (cdr ary)))
					       (not (= (length args) (cdr ary))))
					  (format #f "leaving aside ~A's optional arg~P, " cval (- (cdr ary) (length args)))
					  "")
				      name name cval))))
		  
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
		   (lint-format "~A could be (define ~A ~A)" name name name (cadar bval)))
		  
		  ((and (memq (caar bval) '(car cdr caar cadr cddr cdar caaar caadr caddr cdddr cdaar cddar cadar cdadr cadddr cddddr))
			(pair? (cadar bval)))
		   ((lambda* (cr arg)
		      (and cr
			   (< (length cr) 5)
			   (= (length args) 1)
			   (eq? (car args) arg)
			   (let ((f (string->symbol (string-append "c" cr "r"))))
			     (if (eq? f name)
				 (lint-format "this redefinition of ~A is pointless (use (with-let (unlet)...) or #_~A)" head name name)
				 (lint-format "~A could be (define ~A ~A)" name name name f)))))
		    (combine-cxrs (car bval))))

		  ((and (memq (caar bval) '(list-ref list-tail))
			(pair? (cdar bval))
			(pair? (cddar bval))
			(eq? (car args) (cadar bval))
			(null? (cdr args)))
		   (if (eq? (caar bval) 'list-ref)
		       (case (caddar bval)
			 ((0) (lint-format "~A could be (define ~A car)" name name name))
			 ((1) (lint-format "~A could be (define ~A cadr)" name name name))
			 ((2) (lint-format "~A could be (define ~A caddr)" name name name))
			 ((3) (lint-format "~A could be (define ~A cadddr)" name name name)))
		       (case (caddar bval)
			 ((1) (lint-format "~A could be (define ~A cdr)" name name name))
			 ((2) (lint-format "~A could be (define ~A cddr)" name name name))
			 ((3) (lint-format "~A could be (define ~A cdddr)" name name name))
			 ((4) (lint-format "~A could be (define ~A cddddr)" name name name)))))))))
      
      (let ((data (and (symbol? name)
		       (make-fvar :name (if (not (memq head '(lambda lambda*))) name lambda-marker)
				  :ftype head
				  :initial-value form
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
				    (list (make-var :name args :definer 'parameter))
				    (map
				     (lambda (arg)
				       (if (symbol? arg)
					   (if (memq arg '(:rest :allow-other-keys))
					       (values)                  ; omit :rest and :allow-other-keys
					       (make-var :name arg :definer 'parameter))
					   (if (not (and (pair? arg)
							 (= (length arg) 2)
							 (memq head '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
					       (begin
						 (lint-format "strange parameter for ~A: ~S" name head arg)
						 (values))
					       (begin
						 (if (not (cadr arg))
						     (lint-format "the default argument value is #f in ~A ~A" name head arg))
						 (make-var :name (car arg) :definer 'parameter)))))
				     (proper-list args)))))
		  
		  (let ((cur-env (append arg-data (if data (cons data env) env))))
		    (lint-walk-function-body name head val cur-env)
		    (report-usage name 'parameter head arg-data cur-env))
		  
		  (when (and (var? data)
			     (memq head '(define lambda define-macro))
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
    
    (define (unbegin x)
      (if (and (pair? x)
	       (eq? (car x) 'begin))
	  (cdr x)
	  (list x)))

    (define (lint-walk name form env)
      ;; walk a form, here curlet can change
      ;; (format *stderr* "lint-walk ~A~%" form)

      (if (symbol? form)
	  (set-ref form #f env) ; returns env
	  
	  (if (pair? form)
	      (let ((head (car form)))
		(set! line-number (pair-line-number form))

		(when *report-function-stuff* 
		  (function-match name form env))
		
		(case head
		  
		  ((define define* define-constant define-envelope define-expansion 
		     define-macro define-macro* define-bacro define-bacro* definstrument defanimal
		     define-public) ; this gives more informative names in Guile
		   ;; ---------------- define ----------------		  
		   (if (< (length form) 2)
		       (begin
			 (lint-format "~S makes no sense" name form)
			 env)
		       (let ((sym (cadr form))
			     (val (cddr form)))
			 
			 (if (symbol? sym)
			     (begin

			       (cond ((keyword? sym)               ; (define :x 1)
				      (lint-format "keywords are constants ~A" name sym))

				     ((and (eq? sym 'pi)           ; (define pi (atan 0 -1))
					   (member (car val) '((atan 0 -1)
							       (acos -1)
							       (* 2 (acos 0))
							       (* 4 (atan 1)))))
				      (lint-format "~A is one of its many names, but pi a predefined constant in s7" name (car val)))

				     ((constant? sym)              ; (define most-positive-fixnum 432)
				      (lint-format "~A is a constant in s7: ~A" name sym form))

				     ((let ((v (or (var-member sym env)
						   (hash-table-ref globals sym))))
					(and (var? v)
					     (eq? (var-definer v) 'define-constant)
					     (not (equal? (caddr form) (var-initial-value v)))))
				      (lint-format "~A in ~A is already a constant, defined ~A~A" name sym
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
						      name (truncated-list->string form)
						      (if (< len 3) "no" "too many") 
						      (if (< len 3) "" "s"))))
				   (lint-format "~A is messed up" name (truncated-list->string form)))
			       
			       (if (and (pair? val)
					(null? (cdr val))
					(equal? sym (car val)))
				   (lint-format "this ~A is either not needed, or is an error: ~A" name head (truncated-list->string form)))
			       
			       (if (pair? val)
				   (let ((e (lint-walk sym (car val) env)))
				     ;(format *stderr* "e: ~S, sym: ~S, val: ~S~%" e sym val)
				     (if (and (pair? e)
					      (eq? (var-name (car e)) lambda-marker)) ; (define x (lambda ...)) but it misses closures
					 (begin
					   ;(format *stderr* "set to ~S\n" sym)
					   (set! (var-name (car e)) sym)
					   e)
					 (cons (make-var :name sym :initial-value (car val) :definer head) env)))
				   (cons (make-var :name sym :initial-value val :definer head) env)))
			     
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
					 (cond ((equal? args (cdr body))
						(lint-format "perhaps ~A" name (lists->string form (car body))))

					       ((equal? (reverse args) (cdr body))
						(let ((rf (reversed (car body))))
						  (if rf (lint-format "perhaps ~A" name (lists->string form rf)))))

					       ((and (= arglen 1)
						     (memq (car body) '(car cdr caar cadr cddr cdar caaar caadr caddr 
									cdddr cdaar cddar cadar cdadr cadddr cddddr)))
						((lambda* (cr arg) ; lambda* not lambda because combine-cxrs might return just #f
						   (and cr
							(< (length cr) 5) 
							(eq? (car args) arg)
							(lint-format "perhaps ~A" name 
								     (lists->string form (string->symbol (string-append "c" cr "r"))))))
						 (combine-cxrs body)))))))))
				 
			   (if (and (or (symbol? args)                 ; (lambda args (apply f args)) -> f
					(and (pair? args)              ; (lambda #\a ...) !
					     (negative? (length args))))
				    (eq? head 'lambda)
				    (not (eq? name 'case-lambda))    
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
				     (lint-format "perhaps ~A" name (lists->string form (cadr body))))))
			   
			   (lint-walk-function head name args (cddr form) form env)
			   ;env -- not this -- return the lambda-marker+old env via lint-walk-function
			   ))))
		  
		  ((set!)
		   ;; ---------------- set! ----------------		  
		   (if (not (= (length form) 3))
		       (begin
			 (lint-format "set! has too ~A arguments: ~S" name (if (> (length form) 3) "many" "few") form)
			 env)
		       (let ((settee (cadr form))
			     (setval (caddr form)))
			 (let ((result (lint-walk name setval env)))
			   (if (symbol? settee)
			       (if (constant? settee)
				   (lint-format "can't set! ~A (it is a constant)" name (truncated-list->string form))
				   (let ((v (or (var-member settee env)
						(hash-table-ref globals settee))))
				     (if (and (var? v)
					      (eq? (var-definer v) 'define-constant))
					 (lint-format "can't set! ~A in ~A (it is a constant: ~A~A)" name settee
						      (truncated-list->string form)
						      (if (and (pair? (var-initial-value v))
							       (positive? (pair-line-number (var-initial-value v))))
							  (format #f "(line ~D): " (pair-line-number (var-initial-value v)))
							  "")
						      (truncated-list->string (var-initial-value v))))))
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
						 (when (and (symbol? checker)
							    (not (compatible? checker arg-type)))
						   (lint-format "~A: new value should be a~A ~A: ~S: ~A" 
								name (car settee)
								(if (char=? (string-ref (format #f "~A" checker) 0) #\i) "n" "")
								checker arg-type
								(truncated-list->string form)))))))))
				     
				     (set! settee (do ((sym (car settee) (car sym)))
						      ((not (pair? sym)) sym))))
				   (lint-format "can't set! ~A" name (truncated-list->string form))))
			   
			   (if (symbol? (cadr form)) ; see do above
			       (set-set (cadr form) form env)
			       (if (and (pair? (cadr form))
					(symbol? settee))
				   (set-ref settee `(implicit-set ,@(cdr form)) env)))
			   
			   (if (equal? (cadr form) setval) ; not settee here!
			       (lint-format "pointless set! ~A" name (truncated-list->string form)))
			   
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
		   (let ((len (length form)))
		     (if (> len 4)
			 (lint-format "if has too many clauses: ~A" name (truncated-list->string form))
			 (if (< len 3)
			     (lint-format "if has too few clauses: ~A" name (truncated-list->string form))
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
				 (if (eq? old tree)
				     (cons new (cdr tree))
				     (if (pair? tree)
					 (if (eq? (car tree) 'quote)
					     (copy-tree tree)
					     (cons (tree-subst-eq new old (car tree))
						   (tree-subst-eq new old (cdr tree))))
					 tree)))

			       (if (eq? false #<unspecified>)
				   (lint-format "this #<unspecified> is redundant: ~A" name form))

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
				       (if (not (memq (car true) '(let do lambda)))
					   (let ((seqdiff (differ-in-one-seq true false 0)))
					     ;; (format *stderr* "seqdiff: ~A ~A ~A~%" seqdiff true false)
					     ;; cadr replacement is too messy, looks good about 1 in 10 times
					     (if (and (pair? seqdiff)
						      (< (tree-length-to (cadr seqdiff) 25) 25)) ; 100 is too big, 30 is ok perhaps
						 (lint-format "perhaps ~A" name
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
					(lint-format "perhaps use cond: ~A" name
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
							 (lists->string form (simplify-boolean `(or ,false ,true) () () env))))))
				 
				 (when (= len 4)
				   (if (and (pair? true)
					    (eq? (car true) 'if))
				       (if (= (length true) 4)
					   (begin
					     (if (equal? expr (simplify-boolean `(not ,(cadr true)) () () env))
						 (lint-format "pointless repetition of if test: ~A" name
							      (lists->string form `(if ,expr ,(cadddr true) ,false))))
					     (if (equal? false (cadddr true))
						 (lint-format "perhaps ~A" name 
							      (lists->string form (if (not false)
										      `(and ,expr ,(cadr true) ,(caddr true))
										      `(if (and ,expr ,(cadr true)) ,(caddr true) ,false))))
						 (if (equal? false (caddr true))
						     (lint-format "perhaps ~A" name 
								  (lists->string form (if (not false)
											  `(and ,expr (not ,(cadr true)) ,(cadddr true))
											  `(if (and ,expr (not ,(cadr true))) ,(cadddr true) ,false)))))))
					   (begin
					     (if (equal? expr (simplify-boolean `(not ,(cadr true)) () () env))
						 (lint-format "pointless repetition of if test: ~A" name 
							      (lists->string form `(if (not ,expr) ,false))))
					     (if (equal? false (caddr true)) ; (if a (if b A) A)
						 (lint-format "perhaps ~A" name
							      (lists->string form `(if (or (not ,expr) ,(cadr true)) ,false)))))))
				   
				   (if (pair? false)
				       (case (car false)
					 ((cond)                 ; (if a A (cond...)) -> (cond (a  A) ...)
					  (lint-format "perhaps ~A" name (lists->string form `(cond (,expr ,true) ,@(cdr false)))))

					 ((if)
					  (if (= (length false) 4)
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
								       (lists->string form `(if (or ,expr (not ,(cadr false))) ,true ,(caddr false)))))))))

					 ((case)
					  (if (and (pair? expr)
						   (cond-eqv? expr (cadr false) #t))
					      (lint-format "perhaps ~A" name
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
					   (lint-format "pointless repetition of if test: ~A" name (lists->string form `(if ,expr ,true ,(caddr false)))))
				       
				       (if (and (eq? (car false) 'if)   ; (if test0 expr (if test1 expr)) -> if (or test0 test1) expr) 
						(null? (cdddr false))   ; other case is dealt with above
						(equal? true (caddr false)))
					   (let ((test1 (simplify-boolean `(or ,expr ,(cadr false)) () () env)))
					     (lint-format "perhaps ~A" name (lists->string form `(if ,test1 ,true ,@(cdddr false))))))
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
						       (lint-format "perhaps ~A" name
								    (lists->string form `(set! ,target (,f ,@(cdr true)))))))))
					   
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
					(lint-format "if is not needed here: ~A" name 
						     (lists->string form (if (not (side-effect? test env))
									     true
									     `(begin ,expr ,true))))))

				 (when (and (= suggestion made-suggestion)
					    (not (equal? expr test)))
				   (lint-format "perhaps ~A" name (lists->string test expr)))

				 ;; why not use expr here?
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
				 
				 (if (and (pair? true)   ; (if a (if b A B) (if b B A))
					  (pair? false)
					  (eq? (car true) 'if)
					  (eq? (car false) 'if)
					  (= (length true) (length false) 4)
					  (equal? (cadr true) (cadr false))
					  (equal? (caddr true) (cadddr false))
					  (equal? (cadddr true) (caddr false)))
				     (let* ((switch #f)
					    (a (if (and (pair? test)
							(eq? (car test) 'not))
						   (begin (set! switch #t) test)
						   (simplify-boolean `(not ,test) () () env)))
					    (b (if (and (pair? (cadr true))
							(eq? (caadr true) 'not))
						   (begin (set! switch (not switch)) (cadr true))
						   (simplify-boolean `(not ,(cadr true)) () () env))))
				       (if switch
					   (lint-format "perhaps ~A" name
							(lists->string form `(if (eq? ,a ,b) ,(caddr false) ,(caddr true))))
					   (lint-format "perhaps ~A" name
							(lists->string form `(if (eq? ,a ,b) ,(caddr true) ,(caddr false)))))))
				 
				 (lint-walk name expr env)
				 (set! env (lint-walk name true env))
				 (if (= len 4) (set! env (lint-walk name false env)))))))
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
			     (set-ref test form env)
			     (if (pair? test)
				 (lint-walk name test env)))
			 (lint-walk-body name head (cddr form) env))))
		  
		  ((cond)
		   ;; ---------------- cond ----------------
		   (let ((ctr 0)
			 (suggest made-suggestion)
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
					      (lint-format "this clause could be omitted: ~A" name prev-clause))
					    (set! has-combinations #t)))          ; handle these later
				    (set! prev-clause clause)
				    
				    (let ((expr (simplify-boolean (car clause) () () env))
					  (test (car clause))
					  (sequel (cdr clause)))
				      
				      (cond ((memq test '(else #t))
					     (set! has-else #t)
					     
					     (if (and (pair? sequel)
						      (eq? (car sequel) #<unspecified>))
						 (lint-format "this #<unspecified> is redundant: ~A" name clause))
					     
					     (if (and (pair? sequel)        ; (cond (a A) (else (cond ...))) -> (cond (a A) ...)
						      (pair? (car sequel))  ;    similarly for if, when, and unless
						      (null? (cdr sequel)))
						 (case (caar sequel)
						   ((cond)
						    (lint-format "else clause could be folded into the outer cond: ~A" name 
								 (lists->string form (append (copy form (make-list ctr)) 
											     (cdar sequel)))))
						   ((if)
						    (let ((if-expr (car sequel)))
						      (lint-format "else clause could be folded into the outer cond: ~A" name 
								   (lists->string form 
										  (append (copy form (make-list ctr)) 
											  (if (= (length if-expr) 3)
											      (list (cdr if-expr))
											      `((,(cadr if-expr) ,@(unbegin (caddr if-expr)))
												(else ,@(unbegin (cadddr if-expr))))))))))
						   ((when unless)
						    (lint-format "else clause could be folded into the outer cond: ~A" name 
								 (lists->string form 
										(append (copy form (make-list ctr))
											(if (eq? (caar sequel) 'when)
											    `((,(cadar sequel) ,@(cddar sequel)))
											    `(((not ,(cadar sequel)) ,@(cddar sequel)))))))))))
					  ((and (equal? test ''else)
						(= ctr len))
					   (lint-format "odd cond clause test: is 'else supposed to be else? ~A in ~A~%" 
							name clause (truncated-list->string form)))
					      
					  ((and (eq? test 't)
						(= ctr len)
						(not (var-member 't env))
						(not (hash-table-ref globals 't)))
					   (lint-format "odd cond clause test: is t supposed to be #t? ~A in ~A~%" 
							name clause (truncated-list->string form))))
				      
				      (if (never-false expr)
					  (if (not (= ctr len))
					      (lint-format "cond test ~A is never false: ~A" name (car clause) (truncated-list->string form))
					      (if (not (or (memq expr '(#t else))
							   (side-effect? test env)))
						  (lint-format "cond last test could be #t: ~A" name form)))
					  (if (never-true expr)
					      (lint-format "cond test ~A is never true: ~A" name (car clause) (truncated-list->string form))))
				      
				      (if (not (side-effect? test env))
					  (begin
					    (if (and (not (memq test '(else #t)))
						     (pair? sequel)
						     (null? (cdr sequel)))
						(cond ((equal? test (car sequel))
						       (lint-format "no need to repeat the test: ~A" name (lists->string clause (list test))))

						      ((and (pair? (car sequel))
							    (pair? (cdar sequel))
							    (null? (cddar sequel))
							    (equal? test (cadar sequel)))
						       (lint-format "perhaps use => here: ~A" name 
								    (lists->string clause (list test '=> (caar sequel)))))

						      ((and (eq? (car sequel) #t)
							    (pair? test)
							    (not (memq (car test) '(or and)))
							    (eq? (return-type (car test) env) 'boolean?))
						       (lint-format "this #t could be omitted: ~A" name (truncated-list->string clause)))))
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
			    (cdr form)) ; for-each clause
			   
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
			   
			   (if (and (= len 2)
				    (not (check-bool-cond name form (cadr form) (caddr form) env))
				    (pair? (cadr form))   ; (cond 1 2)!
				    (pair? (caddr form))
				    (equal? (simplify-boolean (caadr form) () () env)
					    (simplify-boolean `(not ,(caaddr form)) () () env)))
			       (lint-format "perhaps ~A" name  ; (cond ((x) y) ((not (x)) z)) -> (cond ((x) y) (else z))
					    (lists->string form `(cond ,(cadr form) (else ,@(cdaddr form))))))
			   
			   (when has-combinations
			     (let ((new-clauses ())
				   (current-clauses ()))
			       (do ((clauses (cdr form) (cdr clauses)))
				   ((null? clauses)
				    (let ((len (length new-clauses)))
				      ;; len is very rarely 1
				      (if (not (and (= len 2) ; i.e. don't go to check-bool-cond
						    (check-bool-cond name form (cadr new-clauses) (car new-clauses) env)))
					  (lint-format "perhaps ~A" name 
						       (lists->string 
							form
							(if (not all-eqv)
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
								`(cond ,@(reverse new-clauses)))
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
					  (lists->string form (cond->case eqv-select (cdr form)))))

			   (if (and (= len 2)
				    has-else
				    (null? (cdadr form)))
			       (let ((else-clause (if (null? (cddr (caddr form)))
						      (cadr (caddr form))
						      `(begin ,@(cdr (caddr form))))))
				 (lint-format "perhaps ~A" name (lists->string form `(or ,(caadr form) ,else-clause)))))

			   (if (and (= suggest made-suggestion) ; this rarely happens
				    (>= len 2)                  ; (cond ((A) B) ((or C D)) (else E)) -> (cond ((A) B) (else (or C D E)))
				    has-else)
			       (let ((last-clause (list-ref form (- len 1))))
				 (if (and (pair? last-clause)
					  (null? (cdr last-clause))
					  (pair? (car last-clause))
					  (eq? (caar last-clause) 'or))
				     (let ((e (list-ref form len)))
				       (let ((else-clause (if (null? (cddr e))
							      (cadr e)
							      `(begin ,@(cdr e)))))
					 (lint-format "perhaps ~A" name
						      (lists->string form
								     `(cond ,@(copy (cdr form) (make-list (- len 2)))
									    (else (or ,@(cdar last-clause) ,else-clause))))))))))))
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
					    (lint-format (if (null? keys) 
							     "null case key list: ~A" 
							     "stray dot in case case key list: ~A")
							 name (truncated-list->string clause))
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
				   (lint-format "perhaps ~A" name 
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
				     (lint-format "perhaps ~A" name 
						  (lists->string form 
								 (if (pair? else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs) ,else-clause)
								     `(case ,(cadr form) ,@(reverse new-keys-and-exprs))))))))))
			 ))
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

			   (define (var-step v) ((cdr v) 'step))
			   
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
				(if (not (null? bindings))
				    (lint-format "do variable list is not a proper list? ~S" name step-vars)))
			     (if (binding-ok? name head (car bindings) env #f)
				 (begin
				   (lint-walk name (cadar bindings) env)
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
			   (do ((bindings step-vars (cdr bindings)))
			       ((not (pair? bindings)))
			     (let ((stepper (car bindings))) ; the entire binding: '(i 0 (+ i 1))
			       (when (and (binding-ok? name head stepper env #t)
					  (pair? (cddr stepper)))
				 (let ((data (var-member (car stepper) vars)))
				   (let ((old-ref (var-ref data)))
				     (lint-walk name (caddr stepper) inner-env)
				     (set! (var-ref data) old-ref))
				   (if (eq? (car stepper) (caddr stepper))  ; (i 0 i) -> (i 0)
				       (lint-format "perhaps ~A" name (lists->string stepper (list (car stepper) (cadr stepper)))))
				   (set! (var-set data) (+ (var-set data) 1))) ; (pair? cddr) above
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
			   (report-usage name 'variable head vars inner-env)
			   
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
					;; integer type check here isn't needed because we're using this as an index below
					;;   the type error will be seen in report-usage if not earlier
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
			   (not (or (symbol? (cadr form))
				    (list? (cadr form)))))
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
				     (lint-format "pointless let: ~A" name (lists->string form (car body)))
				     (if (null? (cadar body))
					 (lint-format "pointless let: ~A" name (lists->string form `(let ,(cadr form) ,@(cddar body))))))))
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
							  :initial-value form
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
				(if (not (null? bindings))
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
						(lint-format "perhaps ~A" name
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
						     (null? (cddr (cadadr p))) ; one arg to func
						     (eq? vname (cadr (cadadr p))))
						(eq? vname (cadadr p)))
					    (or (null? (cddr p))
						(not (tree-member vname (cddr p)))))
				   (if (eq? vname (cadadr p))
				       (if (and (pair? (cddr p))
						(pair? (caddr p))
						(memq (caaddr p) '(else #t t)))
					   (lint-format "perhaps ~A" name 
							(lists->string form (if (null? (cddr (caddr p)))
										`(or ,vvalue ,(cadr (caddr p)))
										`(or ,vvalue (begin ,@(cdaddr p))))))
					   (lint-format "perhaps ~A" name (lists->string form `(or ,vvalue 
												   (cond ,@(cddr p))))))
				       (lint-format "perhaps ~A" name (lists->string form `(cond (,vvalue => ,(caadr (cadr p))) 
												 ,@(cddr p))))))

				 (when (pair? (cddr p))
				   (when (and (pair? (cdddr p))
					      (eq? (car p) 'if))

				     (when (and (eq? (cadr p) vname) ; (let ((x (g y))) (if x #t #f)) -> (g y)
						(boolean? (caddr p))
						(boolean? (cadddr p))
						(not (eq? (caddr p) (cadddr p))))
				       (lint-format "perhaps ~A" name
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
					   (lint-format "perhaps ~A" name (lists->string form `(cond (,vvalue => ,(car (cadddr p))) ,@else-clause)))))))

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
									       name p vname (car vvalue) (truncated-list->string (car varlist)))
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
							 (lint-format "perhaps ~A" name 
								      (lists->string form 
										     (if (null? (cdddr p))
											 vvalue
											 `(or ,vvalue ,(cadddr p)))))
							 #f))
						(pair? (caddr p))
						(or (eq? (car p) 'if)
						    (null? (cdddr p))))
				       (let ((else-clause (cond ((pair? (cdddr p))
								 (if (not (eq? (cadddr p) vname))
								     (if (and (pair? (cadddr p))
									      (tree-member vname (cadddr p)))
									 :oops! ; if the let var appears in the else portion, we can't do anything with =>
									 `((else ,(cadddr p))))
								     `((else #f)))) ; this stands in for the local var
								((eq? (car p) 'and)
								 `((else #f)))
								((eq? (car p) 'or)
								 `((else #t)))
								(else ()))))
					 (unless (eq? else-clause :oops!)
					   (lint-format "perhaps ~A" name 
							(lists->string form `(cond (,vvalue => ,(or crf (caaddr p))) ,@else-clause))))))))
				 )))
			   
			   (let* ((cur-env (append vars env))
				  (e (lint-walk-body name head body cur-env))
				  (nvars (if (null? cur-env)
					     e
					     (and (not (eq? e cur-env))
						  (env-difference name e cur-env ())))))
			     (if (pair? nvars)
				 (if (eq? (var-name (car nvars)) lambda-marker)
				     (begin
				       ;(format *stderr* "saving ~S~%" (car nvars))
				       (set! env (cons (car nvars) env))
				       (set! nvars (cdr nvars)))
				     (set! vars (append nvars vars))))
			     (report-usage name 'variable head vars cur-env))

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
			 (let ((vars (if named-let (list (make-var :name named-let 
								   :definer 'let*)) ())) ; TODO: fvar
			       (varlist (if named-let (caddr form) (cadr form)))
			       (body (if named-let (cdddr form) (cddr form)))
			       (side-effects #f))
			   (if (not (list? varlist))
			       (lint-format "let* is messed up: ~A" name (truncated-list->string form)))
			   (do ((bindings varlist (cdr bindings)))
			       ((not (pair? bindings))
				(if (not (null? bindings))
				    (lint-format "let* variable list is not a proper list? ~S" 
						 name (if named-let (caddr form) (cadr form)))))
			     (if (binding-ok? name head (car bindings) env #f)
				 (begin
				   (if (not side-effects)
				       (set! side-effects (side-effect? (cadar bindings) env)))
				   (lint-walk name (cadar bindings) (append vars env))
				   (set! vars (cons (make-var :name (caar bindings) 
							      :initial-value (cadar bindings) 
							      :definer (if named-let 'named-let* 'let*))
						    vars)))))
			   
			   (if (not (or side-effects
					(any? (lambda (v) (positive? (var-ref v))) vars)))
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
				 (if (eq? (var-name (car nvars)) lambda-marker)
				     (begin
				       (set! env (cons (car nvars) env))
				       (set! nvars (cdr nvars)))
				     (set! vars (append nvars vars))))
			   
			     (report-usage name 'variable head vars cur-env))
			   
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
					 (if (not (eq? else-clause :oops!))
					     (case varlen
					       ((1) (lint-format "perhaps ~A" name 
								 (lists->string form `(cond (,(cadr var) => ,(caaddr p)) 
											    ,@else-clause))))
					       ((2) (lint-format "perhaps ~A" name 
								 (lists->string form `(let (,(car varlist))
											(cond (,(cadr var) => ,(caaddr p)) 
											      ,@else-clause)))))
					       (else (lint-format "perhaps ~A" name 
								  (lists->string form `(let* (,@(copy varlist (make-list (- varlen 1))))
											 (cond (,(cadr var) => ,(caaddr p)) 
											       ,@else-clause)))))))))))))
			   (if (and (pair? body)
				    (null? (cdr body))
				    (pair? varlist)             ; (let* ()...)
				    (pair? (car varlist))       ; (let* (x) ...)
				    (not (pair? (car body))))
			       (if (and (eq? (car body) (caar varlist))
					(null? (cdr varlist))
					(pair? (cdar varlist))) ; (let* ((a...)) a)
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
			      (if (not (null? bindings))
				  (lint-format "~A variable list is not a proper list? ~S" name head (cadr form))))
			   (when (binding-ok? name head (car bindings) env #f)
			     (set! vars (cons (make-var :name (caar bindings) 
							:initial-value (if (and (eq? (caar bindings) (cadar bindings))
										(or (eq? head 'letrec)
										    (not (var-member (caar bindings) vars))))
									   (begin
									     (lint-format "~A is the same as (~A #<undefined>) in ~A" name
											  (car bindings) (caar bindings) head)
									     ;; in letrec* ((x 12) (x x)) is an error
									     #<undefined>)
									   (cadar bindings))
							:definer head)
					      vars))))
			 
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
				 (if (eq? (var-name (car nvars)) lambda-marker)
				     (begin
				       (set! env (cons (car nvars) env))
				       (set! nvars (cdr nvars)))
				     (set! vars (append nvars vars))))

			     (report-usage name 'variable head vars cur-env)))))
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
				 (and (pair? e)
				      (let ((op (return-type (car e) env)))
					(and op
					     (not (return-type-ok? 'let? op))))))
			     (lint-format "~A: first argument should be an environment: ~A" head name (truncated-list->string form)))
			 (if (symbol? e)
			     (set-ref e form env)
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
				       (report-usage name 'variable head vars env))
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
			     (lint-format "~A parameter is repeated: ~A" name head (truncated-list->string args))
			     (lint-format "~A is deprecated; perhaps ~A" name head
					  (truncated-lists->string form `(,(if (eq? head 'defmacro) 'define-macro 'define-macro*) ,(cons sym args) ,@body))))
			 (lint-walk-function head sym args body form env)))
		   env)
		  
		  ((defgenerator)
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
				  (symbol? head)
				  (procedure? (symbol->value head *e*)))
			     (lint-format "unexpected dot: ~A" name (truncated-list->string form)))
			 env)
		       (begin

			 (when (symbol? head)
			   (let ((v (or (var-member head env)
					(hash-table-ref globals head))))
			     (if (and (var? v) 
				      (not (memq form (var-history v))))
				 (set! (var-history v) (cons form (var-history v))))
			     (check-call name head form env)
			   
			     (when (pair? form)
			       ;; save any references to vars in their var-history (type checked later)
			       ;;   this can be fooled by macros, as everywhere else
			       (for-each (lambda (arg)
					   (if (symbol? arg)
					       (let ((v (or (var-member arg env)
							    (hash-table-ref globals arg))))
						 (if (and (var? v)
							  (not (memq form (var-history v))))
						     (set! (var-history v) (cons form (var-history v)))))))
					   form))

			     (if (not (var? v))
				 (check-special-cases name head form env)
				 (if (and (memq (var-ftype v) '(define lambda))
					  (not (memq name (var-scope v))))
				     (set! (var-scope v) (cons name (var-scope v)))))

			     (if (assq head deprecated-ops)
				 (lint-format "~A is deprecated; use ~A" name head (cdr (assq head deprecated-ops))))

			     (if (and (not (= line-number last-simplify-numeric-line-number))
				      (not (var? v))
				      (hash-table-ref numeric-ops head)
				      (proper-tree? form))
				 (let ((val (simplify-numerics form env)))
				   (if (not (equal-ignoring-constants? form val))
				       (begin
					 (set! last-simplify-numeric-line-number line-number)
					 (lint-format "perhaps ~A" name (lists->string form val))))))
			   
			     ;; if we loaded this file first, and f (head) is defined (e.g. scan above),
			     ;; and it is used before it is defined, but not thereafter, the usage stuff 
			     ;; can get confused, so other-identifiers is trying to track those.
			     
			     (if (not (or (var? v)
					  (defined? head (rootlet))))
				 (hash-table-set! other-identifiers head 
						  (if (not (hash-table-ref other-identifiers head))
						      (list form)
						      (cons form (hash-table-ref other-identifiers head)))))))
			 
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
							    (not (and (pair? (caddr head))
								      (memq (caaddr head) '(define define* define-constant define-macro define-macro*)))))
						       (caddr head)
						       `(let () ,@(cddr head))))))
				   ((identity? head)
				    (lint-format "perhaps ~A" name (truncated-lists->string form (cadr form)))))))
#|
			 (when (and (pair? form)
				    (memq head (list {list} {apply_values} {append})))
			   (define (tree-qq tree sig)
			     (if (pair? tree)
				 (if (eq? (car tree) 'unquote)
				     (format *stderr* "unmatched comma in quasiquote: ~A in ~A~%" tree (lint-pp form))
				     (if (and (eq? (car tree) 'quote)
					      (pair? sig)
					      (not (memq 'pair? sig)))
					 (format *stderr* "missing comma in quasiquote: ~A in ~A~%" tree (lint-pp form))
					 (let ((sig (arg-signature (car tree) env)))
					   (for-each (lambda (p)
						       (tree-qq p sig))
						     (cdr tree)))))))
			   (tree-qq form #f))
|#
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
    (let ((documentation "(lint file port) looks for infelicities in file's scheme code")
	  (signature (list #t string? output-port? boolean?)))
      (lambda* (file (outp *lint-output-port*) (report-input #t))
	(set! outport outp)
	(set! globals (make-hash-table))
	(set! other-identifiers (make-hash-table))
	(set! loaded-files ())
	(set! last-simplify-boolean-line-number -1)
	(set! last-simplify-numeric-line-number -1)
	(set! last-simplify-cxr-line-number -1)
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
	      (let ((vars ()) ; initial "env"
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
							((= num (floor num))
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

					      ((#\T) (string=? data "T"))
					      ((#\F) (and (string=? data "F") (list 'not #t)))

					      ((#\u) ; for Bigloo
					       (if (string=? data "unspecified")
						   (format outport "  use #<unspecified>, not #unspecified~%"))
					       ;; #<unspecified> seems to hit the no-values check?
					       (symbol->keyword (string->symbol data)))

					      ((#\>) ; for Chicken, apparently #>...<# encloses in-place C code
					       (do ((last #\# c) 
						    (c (read-char) (read-char))) 
						   ((and (char=? last #\<) 
							 (char=? c #\#)) 
						    (values))
						 (if (char=? c #\newline)
						     (set! (port-line-number ()) (+ (port-line-number) 1)))))					      

					      ((#\<) ; Chicken also, #<<EOF -> EOF
					       (if (and (char=? (data 1) #\<)
							(> (length data) 2))
						   (let ((end (substring data 2)))
						     (do ((c (read-line) (read-line)))
							 ((string-position end c)
							  (values))))
						   (symbol->keyword (string->symbol data))))

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
							   (format outport "  perhaps use ~W instead~%" (cdr c))
							   (cdr c)))
						     (else 
						      (symbol->keyword (string->symbol (substring data 1))))))
					      (else 
					       (symbol->keyword (string->symbol data))))))
				    (begin
				      (format outport " reader[~A]: unknown \\ usage: \\~C~%" line data)
				      (set! (h 'result) data)))))))
		
		(do ((form (read fp) (read fp)))
		    ((eof-object? form))
		  (if (pair? form)
		      (set! line (max line (pair-line-number form))))

		  (if (not (or (= last-line-number -1)
			       (side-effect? last-form vars)))
		      (format outport " top-level (line ~D): this has no effect: ~A~%" 
			      last-line-number
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
			    (format outport " top-level ~Aredefinition of built-in function ~A: ~A~%" 
				    (if (> (pair-line-number form) 0)
					(format #f "(line ~D) " (pair-line-number form))
					"")
				    f 
				    (truncated-list->string form)))))
		  
		  (set! vars (lint-walk (if (symbol? form) 
					    form 
					    (and (pair? form) 
						 (car form)))
					form 
					vars))
		  ;(set! calls (+ calls 1))
		  )
		
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
		    (report-usage file 'top-level-var "" vars vars))
		
		(if (and *report-undefined-identifiers*
			 (positive? (hash-table-entries other-identifiers)))
		    (begin ; TODO: show uses etc
		      (format outport "the following identifiers were undefined: ~{~S~^ ~}" (map car other-identifiers))
		      (fill! other-identifiers #f)))

		;(if (and (string? file) (string=? file "profile.scm")) (format *stderr* "calls: ~A ~A~%" calls made-suggestion))

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
