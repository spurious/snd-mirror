;;; lint for s7 scheme

(provide 'lint.scm)

(define *report-unused-parameters* #t)


;;; --------------------------------------------------------------------------------
;;; for snd-test.scm
(set! *#readers* 
      (cons (cons #\_ (lambda (str)
			(if (string=? str "__line__")
			    (port-line-number)
			    #f)))
            *#readers*))
;;; --------------------------------------------------------------------------------


(define (lint file)

  (define (ref-var name env)
    (call-with-exit                             ; same as call/cc
     (lambda (ok)
       (for-each
	(lambda (var)
	  (if (eq? name (car var))
	      (begin 
		(set! (var 1) #t)               ; (list-set! var 1 #t)
		(ok))))
	env)))
    env)
  
  (define (proper-list lst)
    (let ((len (length lst)))
      (if (infinite? len)                       ; (and (not (finite? len)) (not (nan? len)))
	  (begin
	    (format #t "  circular list: ~A?~%" lst)
	    lst)
	  (if (>= len 0)
	      lst
	      (let* ((nlen (+ (abs len) 1))
		     (res (make-list nlen)))    ; make-list returns a list of n elements
		(do ((i 0 (+ i 1))
		     (x lst))
		    ((= i nlen))
		  (if (pair? x)
		      (begin
			(set! (res i) (car x))  ; (list-set! res i (car x))
			(set! x (cdr x)))
		      (set! (res i) x)))        ; (list-set! res i x)
		res)))))

  (define (keywords lst)
    (let ((keys 0))
      (for-each 
       (lambda (arg)
	 (if (keyword? arg)
	     (set! keys (+ keys 1))))
       lst)
      keys))

  (define (report-usage name line-number type head vars)
    (let ((set '())
	  (unused '()))
      (for-each 
       (lambda (arg)
	 (if (member (car arg) '(quote if begin let let* letrec cond case or and do set! with-environment lambda lambda* define
				 define* defmacro defmacro* define-macro define-macro* define-bacro define-bacro* define-constant))
	     (format #t "  ~A (line ~D): ~A ~A named ~A is asking for trouble~%" name line-number head type (car arg)))

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

  
  (define (walk-function head name args val line-number env)
    (if (null? args)
	(begin
	  (walk name val env)
	  (append (list (list name #f #f)) env))
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
					       (not (member head '(define* lambda* defmacro* define-macro* define-bacro* definstrument))))
					   (format #t "  ~A (line ~D): strange parameter ~A~%" name line-number arg)
					   (list (car arg) #f #f))))
				 (proper-list args)))))
	      (let ((new-env (append arg-data env)))
		(for-each
		 (lambda (form)
		   (walk name form new-env))
		 val))
	      (if *report-unused-parameters* (report-usage name line-number 'parameter head arg-data))
	      (append (list (list name #f #f)) env))
	    (begin
	      (format #t "  ~A (line ~D): strange ~A parameter list ~A~%" name line-number head args)
	      env))))
  
  (define (walk name form env)
    
    ;(format #t "walk ~S ~S ~S~%" name form env)
    (if (symbol? form)
	(ref-var form env)
	
	(if (pair? form)
	    (let ((head (car form))
		  (line-number (pair-line-number form)))
	      (case head
		
		((define define* 
		  define-constant 
		  define-expansion define-macro define-macro* define-bacro define-bacro* defmacro defmacro* 
		  definstrument)

		 (let ((sym (cadr form))
		       (val (cddr form)))
		   (if (symbol? sym)
		       (begin
			 (walk sym (caddr form) env)
			 (append (list (list sym #f #f)) env))
		       (if (pair? sym)
			   (walk-function head (car sym) (cdr sym) val line-number env)
			   (begin
			     (format #t "  ~A (line ~D): strange form: ~A~%" head line-number form)
			     env)))))
		
		((lambda lambda*)
		 (walk-function head name (cadr form) (cddr form) line-number env))
		;; the lambda case includes stuff like call/cc
		
		((set!)
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
			   env))))
		   (walk name setval env)))
		
		((quote) env)

		((case)
		 ;; here the keys are not evaluated, so we might have a list like (letrec define ...)
		 (walk name (cadr form) env) ; the selector
		 (for-each
		  (lambda (clause)
		    (walk name (cdr clause) env))
		  (cddr form))
		 env)
		
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
		   (walk name (cddr form) (append vars env))
		   (report-usage name line-number 'variable head vars)
		   env))
		
		((let)
		 (let* ((named-let (if (symbol? (cadr form)) (cadr form) #f)))
		   (let ((vars (if named-let (list (list named-let #f #f)) '())))
		     (do ((bindings (if named-let (caddr form) (cadr form)) (cdr bindings)))
			 ((null? bindings))
		       (walk name (cadar bindings) env)
		       (set! vars (append (list (list (caar bindings) #f #f)) vars)))
		     (walk name (if named-let (cdddr form) (cddr form)) (append vars env))
		     (report-usage name line-number 'variable head vars)
		     env)))
		
		((let*)
		 (let ((vars '()))
		   (do ((bindings (cadr form) (cdr bindings)))
		       ((null? bindings))
		     (walk name (cadar bindings) (append vars env))
		     (set! vars (append (list (list (caar bindings) #f #f)) vars)))
		   (walk name (cddr form) (append vars env))
		   (report-usage name line-number 'variable head vars)
		   env))
		
		((letrec letrec*)
		 (let ((vars '()))
		   (do ((bindings (cadr form) (cdr bindings)))
		       ((null? bindings))
		     (set! vars (append (list (list (caar bindings) #f #f)) vars))
		     (walk name (cadar bindings) (append vars env)))
		   (walk name (cddr form) (append vars env))
		   (report-usage name line-number 'variable head vars)
		   env))

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

		 (if (and (symbol? head)
			  (defined? head)
			  (procedure? (symbol->value head))
			  (not (procedure-with-setter? (symbol->value head))) ; set! case is confusing here
			  (not (member head env)))                            ; not shadowed locally (confusable if the file was loaded first)
		     ;; check arg number
		     (let ((arity (procedure-arity (symbol->value head)))
			   (args (length (cdr form))))
		       (if (pair? arity)
			   (begin
			     (if (< args (car arity))
				 (format #t "  ~A (line ~D): ~A needs at least ~D argument~A:~%        ~S~%" 
					 name line-number head (car arity) (if (> (car arity) 1) "s" "") form)
				 (if (and (not (caddr arity))
					  (> (- args (keywords (cdr form))) (+ (car arity) (cadr arity))))
				     (format #t "  ~A (line ~D): ~A has too many arguments:~%        ~S~%" 
					     name line-number head form)))))

		       ;; now try to check arg types for egregious errors
		       (if (pair? (cdr form)) ; there are args
			   (let ((arg1 (cadr form)))
			     (if (and (not (symbol? arg1))
				      (not (pair? arg1))) ; no check if it's a variable or some expression
				 (let ((number-ops '(= > < random abs * imag-part real-part / magnitude max nan? negative? 
						     positive? >= expt number->string zero?
						     floor denominator integer->char min <= cos rationalize sin 
						     log round ceiling truncate atan numerator make-rectangular
						     cosh even? sqrt odd? tanh sinh tan exp acos asin acosh asinh 
						     atanh modulo make-polar gcd angle remainder quotient
						     lcm inexact->exact ash exact->inexact integer-decode-float 
						     logand lognot logior logxor exact? integer-length
						     inexact? infinite?))
				       (list-ops '(cdr car list-ref cadr list-set! caddr cddr set-cdr! 
						   caar set-car! list-tail cadar cdddr cadddr
						   cddddr cdar caadr cadadr cdadr caaaar caaddr caddar cdaaar 
						   cdaadr cdaddr cddar list->vector list->string
						   caaadr caaar caadar cadaar cdadar cdddar cdaar cddaar cddadr))
				       (vector-ops '(vector-ref vector-set! vector-length vector-fill! vector-dimensions))
				       (string-ops '(string->number string-set! string-ref string=? string-append 
						     string-ci=? string->symbol string-copy with-input-from-string
						     substring string->list string<? string>=? string-ci<? string>? 
						     string-ci>=? string-ci>? string<=? string-ci<=? string-fill!
						     string-length ))
				       (char-ops '(char->integer char=? char-upcase char-alphabetic? write-char char-ci=? 
						   char-numeric? char-downcase char-whitespace?
						   char<? char>? char-upper-case? char-ci=? char<=? char-ci<=? char-ci>? 
						   char-lower-case? char-ci<? char>=?)))

				   (if (member head list-ops)
				       (if (not (null? arg1))
					   (format #t "  ~A (line ~D): ~A's 1st argument should be a list: ~S:~%        ~S~%" 
						   name line-number head arg1 form))
				       (if (member head number-ops)
					   (if (not (number? arg1))
					       (format #t "  ~A (line ~D): ~A's 1st argument should be a number: ~S:~%        ~S~%" 
						       name line-number head arg1 form))
					   (if (member head string-ops)
					       (if (not (string? arg1))
						   (format #t "  ~A (line ~D): ~A's 1st argument should be a string: ~S:~%        ~S~%" 
							   name line-number head arg1 form))
					       (if (member head char-ops)
						   (if (not (char? arg1))
						       (format #t "  ~A (line ~D): ~A's 1st argument should be a character: ~S:~%        ~S~%" 
							       name line-number head arg1 form))
						   (if (member head vector-ops)
						       (if (not (vector? arg1))
							   (format #t "  ~A (line ~D): ~A's 1st argument should be a vector: ~S:~%        ~S~%" 
								   name line-number head arg1 form)))))))))))))
		 (let ((vars env))
		   (for-each
		    (lambda (f)
		      (set! vars (walk name f vars)))
		    form)
		   (if (and (not (eq? vars env))
			    (eq? head 'begin))
		       (let ((nvars '()))
			 (do ((v vars (cdr v)))
			     ((or (null? v)
				  (eq? v env)))
			   (set! nvars (cons (car vars) nvars)))
			 (report-usage name line-number 'local-variable head nvars))))
		 env)))

	    ;; else form is a constant or something
	    env)))
  
  (let ((fp (catch #t
		   (lambda ()
		     (open-input-file file))
		   (lambda args
		     (format #t "  can't open ~S: ~A~%" file (apply format #f (cdr args)))
		     #f))))
    (if (input-port? fp)
	(begin
	  (format #t ";~A~%" file)
	  (do ((form (read fp) (read fp)))
	      ((eof-object? form))
	    (walk (if (symbol? form) form (car form)) form '()))
	  (close-input-port fp)))))


;;; check for obvious cases like (list-ref '(1 2 3) 12)
;;; eq? with something that will always be #f (and eqv? catch)
;;; (not list) -> (not (null?...))
;;; use this stuff in va.scm/s7test.scm/sndscm.html lint section


