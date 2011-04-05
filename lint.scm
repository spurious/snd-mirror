;;; lint for s7 scheme



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

  (define (report-usage name line-number type head vars)
    (let ((set '())
	  (unused '()))
      (for-each 
       (lambda (arg)
	 (if (member arg '(quote if begin let let* letrec cond case or and do set! with-environment lambda lambda* define
			   define* defmacro defmacro* define-macro define-macro* define-bacro define-bacro* define-constant))
	     (format #t "  ~A (line ~D): ~A ~A named ~A is asking for trouble~%" name line-number head type arg))

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
					       (not (member head '(define* lambda* defmacro* define-macro* define-bacro*))))
					   (format #t "  ~A (line ~D): strange parameter ~A~%" name line-number arg)
					   (list (car arg) #f #f))))
				 (proper-list args)))))
	      (walk name val (append arg-data env))
	      (report-usage name line-number 'parameter head arg-data)
	      (append (list (list name #f #f)) env))
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
		
		((define define* define-constant define-expansion define-macro define-macro* define-bacro define-bacro* defmacro defmacro*)
		 (let ((sym (cadr form))
		       (val (cddr form)))
		   (if (symbol? sym)
		       (begin
			 (walk sym val env)
			 (append (list (list sym #f #f)) env))
		       (if (pair? sym)
			   (walk-function head (car sym) (cdr sym) val line-number env)
			   (begin
			     (format #t "  ~A (line ~D): strange form: ~A~%" head line-number form)
			     env)))))
		
		((lambda lambda*)
		 (walk-function head name (cadr form) (cddr form) line-number env))
		
		((set!)
		 (let* ((settee (cadr form))
			(setval (cddr form)))
		   (if (pair? settee)
		       (begin
			 (walk name settee env) ; this counts as a reference since it's by reference so to speak
			 (set! settee (do ((sym (car settee) (car sym)))
					  ((symbol? sym) sym)))))
		   (call-with-exit
		    (lambda (ok)
		      (for-each
		       (lambda (var)
			 (if (eq? settee (car var))
			     (begin 
			       (set! (var 2) #t)  ; (list-set! var 2 #t)
			       (ok))))
		       env)))
		   (walk name setval env)))
		
		((quote) env)
		
		((let do)
		 (let* ((named-let (if (symbol? (cadr form)) (cadr form) #f)))
		   (let ((vars (if named-let (list (list named-let #f #f)) '())))
		     (do ((bindings (if named-let (caddr form) (cadr form)) (cdr bindings)))
			 ((null? bindings))
		       (walk name (cdar bindings) env)
		       (set! vars (append (list (list (caar bindings) #f #f)) vars)))
		     (walk name (if named-let (cdddr form) (cddr form)) (append vars env))
		     (report-usage name line-number 'variable head vars)
		     env)))
		
		((let*)
		 (let ((vars '()))
		   (do ((bindings (cadr form) (cdr bindings)))
		       ((null? bindings))
		     (walk name (cdar bindings) (append vars env))
		     (set! vars (append (list (list (caar bindings) #f #f)) vars)))
		   (walk name (cddr form) (append vars env))
		   (report-usage name line-number 'variable head vars)
		   env))
		
		((letrec letrec*)
		 (let ((vars '()))
		   (do ((bindings (cadr form) (cdr bindings)))
		       ((null? bindings))
		     (set! vars (append (list (list (caar bindings) #f #f)) vars))
		     (walk name (cdar bindings) (append vars env)))
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
			   (format #t "  ~A (line ~D): ~A has ~D unmatched ~A~A~%"
				   name line-number head (abs curlys) (if (positive? curlys) "{" "}") (if (> curlys 1) "s" "")))
		       dirs))
		       
		   ;(format #t "~A -> ~A ~A~%" form control-string args)
		   (if (and (not (string? control-string))
			    (not (pair? args)))
		       (format #t "  ~A (line ~D): ~A looks suspicious~%" name line-number form)
		       (let ((ndirs (count-directives control-string))
			     (nargs (if (or (null? args) (pair? args)) (length args) 0)))
			 (if (not (= ndirs nargs))
			     (format #t "  ~A (line ~D): ~A has ~A arguments~%" 
				     name line-number head 
				     (if (> ndirs nargs) "too few" "too many")))))
		   (walk name (cdr form) env)))
		
		(else  ; if begin cond and or case with-environment

		 ;; TODO: if car is known func we could check arg num/type [and :names for define*]
		 ;; we can't expand macros so free variables can confuse the usage checks
		 
		 (for-each
		  (lambda (f)
		    (walk name f env))
		  form)))))))
  
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
	    (walk form form '()))
	  (close-input-port fp)))))


;;; check for bad args if func is known (and save as we go?)
;;; check for call/cc where continuation is not saved and suggest call-with-exit
;;; check for obvious cases like (list-ref '(1 2 3) 12)
;;; call/cc with no use of the continuation
;;; eq? with something that will always be #f (and eqv? catch)
;;; (not list) -> (not (null?...))
;;; doc/test pair-line-number, this file and use in va.scm
