(gc-verbose #f)
(tracing #f)


(define (error . args) 
  ;(display "error: ") 
  (display args) 
  ;(newline) 
  (if (port-filename (current-input-port))
      (begin
	(display "    ")
	(display (port-filename (current-input-port)))
	(display ", line ")
	(display (port-line-number (current-input-port)))
;	(stacktrace)
	(newline)
	))
  (quit)
  'error)

(define (error . args) 'error)

(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list-tail x k)
  (if (zero? k)
      x
      (list-tail (cdr x) (- k 1))))

(define (list-head l n)
  (reverse (list-tail (reverse l) (- (length l) n))))

(define (list-copy list)
  (if (null? list)
      '()
      (cons (car list)
            (list-copy (cdr list)))))

(define (list-ref x k)
  (car (list-tail x k)))

(define (last-pair x)
  (if (pair? (cdr x))
      (last-pair (cdr x))
      x))



(macro (unless form)
  `(if (not ,(cadr form)) (begin ,@(cddr form))))

(macro (when form)
  `(if ,(cadr form) (begin ,@(cddr form))))

;(define (log10 n) (/ (log n) (log 10)))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (string . charlist)
  (list->string charlist))

(define (list->string charlist)
  (let* ((len (length charlist))
	 (newstr (make-string len))
	 (fill-string!
	  (lambda (str i len charlist)
	    (if (= i len)
		str
		(begin (string-set! str i (car charlist))
		       (fill-string! str (+ i 1) len (cdr charlist)))))))
    (fill-string! newstr 0 len charlist)))

(define (string-fill! s e)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (if (= i n)
	  s
	  (begin (string-set! s i e) (loop (1+ i)))))))

(define (string->list s)
  (let loop ((n (1- (string-length s))) (l '()))
    (if (= n -1)
	l
	(loop (1- n) (cons (string-ref s n) l)))))

(define (list . x) x)

(define (foldr f x lst)
  (if (null? lst)
      x
      (foldr f (f x (car lst)) (cdr lst))))

(define (unzip1-with-cdr . lists)
  (unzip1-with-cdr-iterative lists '() '()))

(define (unzip1-with-cdr-iterative lists cars cdrs)
  (if (null? lists)
      (cons cars cdrs)
      (let ((car1 (caar lists))
	    (cdr1 (cdar lists)))
	(unzip1-with-cdr-iterative 
	 (cdr lists) 
	 (append cars (list car1))
	 (append cdrs (list cdr1))))))

(define (map proc . lists)
  (if (null? lists)
      (apply proc)
      (if (null? (car lists))
	  '()
	  (let* ((unz (apply unzip1-with-cdr lists))
		 (cars (car unz))
		 (cdrs (cdr unz)))
	    (cons (apply proc cars) (apply map (cons proc cdrs)))))))


(define (for-each proc . lists)
  (if (null? lists)
      (apply proc)
      (if (null? (car lists))
	  #t
	  (let* ((unz (apply unzip1-with-cdr lists))
		 (cars (car unz))
		 (cdrs (cdr unz)))
	    (apply proc cars) (apply map (cons proc cdrs))))))



(define (head stream) (car stream))

(define (tail stream) (force (cdr stream)))


;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;;
;; Subsequently modified to handle vectors: D. Souflis

(macro
    quasiquote
  (lambda (l)

    (define (mcons f l r)
      (if (and (pair? r)
	       (eq? (car r) 'quote)
	       (eq? (car (cdr r)) (cdr f))
	       (pair? l)
	       (eq? (car l) 'quote)
	       (eq? (car (cdr l)) (car f)))
	  (if (or (procedure? f) (number? f) (string? f))
	      f
	      (list 'quote f))
	  (if (eqv? l vector)
	      (apply l (eval r))
	      (list 'cons l r)
	      )))

    (define (mappend f l r)
      (if (or (null? (cdr f))
	      (and (pair? r)
		   (eq? (car r) 'quote)
		   (eq? (car (cdr r)) '())))
	  l
	  (list 'append l r)))

    (define (foo level form)
      (cond ((not (pair? form))
	     (if (or (procedure? form) (number? form) (string? form))
		 form
		 (list 'quote form))
	     )
	    ((eq? 'quasiquote (car form))
	     (mcons form ''quasiquote (foo (+ level 1) (cdr form))))

	    (#t (if (zero? level)
		    (cond ((eq? (car form) 'unquote)
			   (car (cdr form)))

			  ((eq? (car form) 'unquote-splicing)
			   form)

			  ((and (pair? (car form))
				(eq? (car (car form)) 'unquote-splicing))

			   (mappend form 
				    (car (cdr (car form)))
				    (foo level (cdr form))))

			  (#t (mcons form (foo level (car form))
				     (foo level (cdr form)))))

		    (cond ((eq? (car form) 'unquote)
			   (mcons form ''unquote (foo (- level 1)
						      (cdr form))))
			  ((eq? (car form) 'unquote-splicing)
			   (mcons form ''unquote-splicing
				  (foo (- level 1) (cdr form))))
			  (#t (mcons form (foo level (car form))
				     (foo level (cdr form)))))))))
    (foo 0 (car (cdr l)))))

#|
(define (expand-quasiquote x)
  (if (pair? x)
      (if (eq? (car x) 'unquote)
          (cadr x)
      (if (eq? (car x) 'unquote-splicing)
          (error "unquote-splicing not inside list")
      (if (and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
          (list 'append
                (cadar x)
                (expand-quasiquote (cdr x)))
          (list 'cons
                (expand-quasiquote (car x))
                (expand-quasiquote (cdr x))))))
      (list 'quote x)))

(defmacro quasiquote (x) (expand-quasiquote x))

(defmacro unquote (x)
  (error "unquote not inside quasiquote"))

(defmacro unquote-splicing (x)
  (error "unquote-splicing not inside quasiquote"))
|#


;;;; (do ((var init inc) ...) (endtest result ...) body ...)
;;
(macro do
  (lambda (do-macro)
    (apply (lambda (do vars endtest . body)
             (let ((do-loop (gensym)))
               `(letrec ((,do-loop
			  (lambda ,(map (lambda (x)
					  (if (pair? x) (car x) x))
					`,vars)
			    (if ,(car endtest)
				(begin ,@(cdr endtest))
				(begin
				  ,@body
				  (,do-loop
                                   ,@(map (lambda (x)
                                            (cond
					     ((not (pair? x)) x)
					     ((< (length x) 3) (car x))
					     (else (car (cdr (cdr x))))))
					  `,vars)))))))
                  (,do-loop
		   ,@(map (lambda (x)
			    (if (and (pair? x) (cdr x))
				(car (cdr x))
				'()))
			  `,vars)))))
	   do-macro)))

;;;; generic-member
(define (generic-member cmp obj lst)
  (cond
   ((null? lst) #f)
   ((cmp obj (car lst)) lst)
   (else (generic-member cmp obj (cdr lst)))))

(define (memq obj lst)
  (generic-member eq? obj lst))

(define (memv obj lst)
  (generic-member eqv? obj lst))

(define (member obj lst)
  (generic-member equal? obj lst))

(define (call-with-input-file filename func)
  (let ((inport (open-input-file filename)))
    (and (input-port? inport)
	 (let ((res (func inport)))
	   (close-input-port inport)
	   res))))

(define (call-with-output-file filename func)
  (let ((outport (open-output-file filename)))
    (and (output-port? outport)
	 (let ((res (func outport)))
	   (close-output-port outport)
	   res))))

(define (with-input-from-file s p)
  (let ((inport (open-input-file s)))
    (if (eq? inport #f)
	#f
	(let ((prev-inport (current-input-port)))
	  (set-input-port inport)
	  (let ((res (p)))
	    (close-input-port inport)
	    (set-input-port prev-inport)
	    res)))))

(define (with-output-to-file s p)
  (let ((outport (open-output-file s)))
    (if (eq? outport #f)
	#f
	(let ((prev-outport (current-output-port)))
	  (set-output-port outport)
	  (let ((res (p)))
	    (close-output-port outport)
	    (set-output-port prev-outport)
	    res)))))


(define (provide sym)
  (set! *features* (cons sym *features*)))

(define (provided? sym)
  (member sym *features*))

(define (call-with-output-string t)
  (let* ((p (open-output-string))
	 (r (t p))
	 (s (get-output-string p)))
    (close-output-port p)
    s))

(define (call-with-input-string s t)
  (let* ((p (open-input-string s))
	 (r (t p)))
    (close-input-port p)
    r))

(define (with-output-to-string thunk)
  (let* ((output-port (current-output-port))
	 (string-port (open-output-string)))
    (thunk)
    (let ((result (get-output-string string-port)))
      (close-output-port string-port)
      result)))

(define (catch tag body tag-handler)
  (call-with-exit
   (lambda (exiter)
     (let ((error (lambda args
		    (exiter 
		     (apply tag-handler args)))))
	(body)))))

; (let ((tag (catch #t (lambda () (display "before") (error 'oops) (display "after")) (lambda args (display "in handler") 'error)))) tag)

(defmacro define+ (name value)
  `(define ,name ,value))
