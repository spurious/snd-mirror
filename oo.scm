
#!

;;OO is a message based system to cause less typing doing object oriented programming with guile.
;;-Kjetil S. Matheussen, 9.2.2005


;;What makes this system different from all the other systems I know about for scheme
;;(found at www.schemers.org), is that the methods are placed inside the classes, like
;;this:

(def-class (<aclass>)
  (def-method (a) 0)
  (def-method (b) 1))

;;The system is used in various other files in SND made by me.
;;The code was previously placed in gui.scm.


;;"def-class" and "def-method" is used instead of "define-class" and "define-method" to
;;not interfere with clos/etc. I have the following lines in my .emacs file:

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(def-method\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 (cond ((match-beginning 1) font-lock-function-name-face)
	     ((match-beginning 2) font-lock-variable-name-face)
	     (t font-lock-type-face))
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(def-class\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(def-constructor\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(c-load-from-path\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-type-face
       nil t))))

(font-lock-add-keywords
 'scheme-mode
 '(("(\\(var\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face
       nil t))))

!#


(provide 'snd-oo.scm)

(use-modules (ice-9 optargs)
	     (ice-9 format)
	     (srfi srfi-1))


(define-macro (c-load-from-path filename)
  `(if (not (provided? (symbol-append 'snd- ',filename '.scm)))
       (load-from-path (symbol->string (symbol-append ',filename '.scm)))))

(define-macro (define-toplevel symbol val)
  `(primitive-eval `(define ,,symbol ,,val)))


(define-macro (def-class def . body)
  
  (for-each (lambda (a) (if (eq? (car a) 'def-constructor)
			    (let* ((name (caadr a))
				   (constructor-name (symbol-append 'constructor- name))
				   (classname (symbol->string (car def)))
				   (reversedclassnameaslist (reverse (string->list classname)))
				   (funcname (if (member (car reversedclassnameaslist) '(#\> #\) #\] #\}))
						 (symbol-append (apply symbol (reverse (cdr reversedclassnameaslist)))
								'/
								name
								(symbol (car reversedclassnameaslist)))
						 (symbol-append (car def) '/ name))))
			      (define-toplevel funcname
				(lambda args
				  (let ((classfunc (eval-string classname)))
				    (define-toplevel funcname
				      (lambda args
					(apply (-> (classfunc) get-method constructor-name) args)))
				    (apply (-> (classfunc) get-method constructor-name) args)))))))
	    body)
  
  `(define* ,def
     (let* ((methods (make-hash-table 256))
	    (supers '())
	    (super (lambda args (c-display "\n\nError! \"super\" is not a method. Perhaps you ment \"Super\"?\n\n")))
	    (dispatch-preds #f)
	    (dispatch-funcs #f)
	    (add-dispatcher (lambda (pred func)
			      (cond ((not dispatch-preds)
				     (set! dispatch-preds pred)
				     (set! dispatch-funcs func))
				    ((procedure? dispatch-preds)
				     (set! dispatch-preds (list dispatch-preds pred))
				     (set! dispatch-funcs (list dispatch-funcs func)))
				    (else
				     (set! dispatch-preds (append dispatch-preds (list pred)))
				     (set! dispatch-funcs (append dispatch-funcs (list func)))))))
	    (add-method-do (lambda (name func)
			     (hashq-set! methods name func))))
       (var class-name ',(car def))
       (def-method (dir)
	 (append (cons this->class-name
		       (hash-fold (lambda (key value s) (cons key s)) '() 
				  methods))
		 (map (lambda (super) (-> super dir))
		      supers)))
       (def-method (get-method name)
	 (or (hashq-ref methods name)
	     (any (lambda (super) (-> super get-method name))
		  supers)))
       (def-method (instance? class-name)
	 (or (eq? class-name this->class-name)
	     (any (lambda (super) (-> super instance? class-name))
		  supers)))
       
       (define (this name . rest)
	 (apply (or (hashq-ref methods name)
		    (any (lambda (super) (-> super get-method name))
			 supers)
		    (lambda x (format #t "No such method: \"~A\" in class \"~A\".~%" name this->class-name)))
		rest))
       
       (define (this-with-custom-dispatchers m . rest)
	 (call-with-current-continuation
	  (lambda (return)
	    (for-each (lambda (pred func)
			(if (pred m rest)
			    (return (func m rest))))
		      dispatch-preds
		      dispatch-funcs)
	    (apply (or (hashq-ref methods m)
		       (any (lambda (super) (-> super get-method m))
			    supers)
		       (lambda x (format #t "No such method: \"~A\" in class \"~A\".~%" m this->class-name)))
		   rest))))
       
       (define (this-with-custom-dispatcher m . rest)
	 (if (dispatch-preds m rest)
	     (dispatch-funcs m rest)
	     (apply (or (hashq-ref methods m)
			(any (lambda (super) (-> super get-method m))
			     supers)
			(lambda x (format #t "No such method: \"~A\" in class \"~A\".~%" m this->class-name)))
		    rest)))
       
       ,@body
       
       (if (and this dispatch-preds)
	   (if (procedure? dispatch-preds)
	       (set! this this-with-custom-dispatcher)
	       (set! this this-with-custom-dispatchers)))
       
       this)))


(define-macro (add-method nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda ,(cdr nameandvars) ,@body)))

(define-macro (add-method* nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda* ,(cdr nameandvars) ,@body)))

(define-macro (def-method nameandvars . body)
  (if (and (list? nameandvars)
	   (or (member #:optional nameandvars)
	       (member #:rest nameandvars)
	       (member #:key nameandvars)))
      `(define ,(symbol-append 'this-> (car nameandvars))
	 (add-method* ,nameandvars ,@body))
      `(define ,(symbol-append 'this-> (car nameandvars))
	 (add-method ,nameandvars ,@body))))

(define-macro (var name initial)
  (let ((thisname (symbol-append 'this-> name)))
    `(define ,thisname
       (begin
	 (add-method (,name . rest) (if (null? rest) ,thisname (set! ,thisname (car rest))))
	 ,initial))))

(define-macro (def-constructor nameandvars . body)
  (let* ((name (car nameandvars))
	(args (cdr nameandvars))
	(name2 (symbol-append 'constructor- name)))
    `(add-method* ,(cons name2 args) ,@body)))

(define (object? o)
  (and (procedure? o)
       (catch #t
	      (lambda ()
		(-> o instance? (-> o class-name)))
	      (lambda (key . args)
		#f))))

(define-macro (instance? object class)
  `(-> ,object instance? ',class))

(define-macro (Super . rest)
  `(define dassupers
     (begin
       (set! supers (list ,@rest))
       (set! super (car supers)))))


(define (void . rest)
  #f)

;; The -> macro caches the function pointer. Generally a little bit faster than ->2.
(define-macro (-> object method . args)
  (if (number? object)
      `(list-set! ,method ,object ,(car args))
      (let ((funcname (gensym (string-append "->___" (symbol->string method)))))
	(define-toplevel funcname
	  (let ((func #f)
		(lastobj #f))
	    (lambda (object . args)
	      (if (not (eq? lastobj object))
		  (begin
		    (set! lastobj object)
		    (set! func (object 'get-method method))
		    (if (not func)
			(throw 'no-such-method method "in class" (object 'class-name)))))
	      (apply func args))))
	`(,funcname ,object ,@args))))


;; This one works just the same as ->, but doesn't cache the function pointer. Could be a tiny tiny little bit faster than -> in some situations.
(define-macro (->2 object method . args)
  (if (number? object)
      `(list-set! ,method ,object ,(car args))
      `(,object ',method ,@args)))

(define-macro (<- object method)
  (if (number? object)
      `(list-ref ,method ,object)
      `(-> ,object get-method ',method)))


#!
(class (<wefwe> wrg)
  (def-method (ai val)
    5))

(class (<test>)
  (def-method (ai val)
    val))
(define a (<test>))
(-> a ai 5)

(def-class (<super1> sum)
  (var avar 2)
  (def-method (super1)
    (display "super1 sum: ")(display sum)
    (newline)))

(def-class (<super2> sum)
  (def-method (super2)
    (display "super2 sum: ")(display sum)
    (newline)))

(def-class (<bank> sum) (Super (<super1> (+ 1000 sum)) (<super2> (+ 2000 sum)))
  (def-method (print-sum)
    (display sum)(newline))
  (def-method (deposit x)
    (set! sum (+ sum x))
    (this->print-sum))
  (def-method (withdraw x)
    (set! sum (- sum x))
    (this->print-sum)))

(define b (<bank> 5))
(begin b)
(-> b deposit 3)
(-> b withdraw 6)
(define b->withdraw (<- b withdraw))
(begin b->withdraw)
(b->withdraw 7)
(-> b class-name)
(-> b super1)
(-> b super2)
(-> b avar)
(-> b avar 5)
(-> b avar)
(instance? b <bank>)
(instance? b <super1>)
(instance? b <super2>)
(instance? b <someother-class>)
(-> b dir)
(-> b not-a-method)
!#






;;##############################################################
;; Array 
;;##############################################################

(def-class (<array> . rest)
  (define dasarray (list->vector rest))

  (def-method (get-vector)
    dasarray)
  (def-method (set-vector! v)
    (set! dasarray v))
  (def-method (get-list)
    (vector->list dasarray))
  (def-method (set-list! l)
    (set! dasarray (list->vector l)))
  (def-method (reset!)
    (this->set-list! rest))
  (def-method (set!! . rest)
    (this->set-list! rest))
  (def-method (set! . rest)
    (c-for-each (lambda (i val)
		  (vector-set! dasarray i val))
		rest))
  (def-method (for-each func)
    (c-for 0 < (this->length) 1
	   (lambda (n)
	     (func n (vector-ref dasarray n)))))
  (def-method (map! func)
    (this->for-each (lambda (n el)
		      (vector-set! dasarray n (func n el)))))
  (def-method (map func)
    (let* ((ret '(0))
	   (tail ret))
      (this->for-each (lambda (n el)
			(let ((new (list (func n el))))
			  (set-cdr! tail new)
			  (set! tail new))))
      (cdr ret)))
  (def-method (length)
    (vector-length dasarray))

  ;; Python-like list-selector (not complete, or optimized, or very useful in the current form.)
  (def-method (p sel)
    (let* ((split (string-split sel #\:))
	   (intsplit (apply <array> (map string->number split))))
      (cond ((= 1 (length split)) (vector-ref dasarray (intsplit 0)))
	    ((= 2 (length split)) (sublist (this->get-list) (intsplit 0) (intsplit 1)))
	    (else split))))

  (add-dispatcher (lambda (n rest)
		    (integer? n))
		  (lambda (n rest)
		    (if (null? rest)
			(vector-ref dasarray n)
			(vector-set! dasarray n (car rest)))))

  (add-dispatcher (lambda (s rest)
		    (string? s))
		  (lambda (s rest)
		    (this->p s)))

  (def-constructor (length len #:optional default)
    (this->set-vector! (make-vector len default))
    this)
  
  (def-constructor (map len func)
    (this->set-vector! (make-vector len #f))
    (this->map! (lambda (n el) (func n)))
    this)

  (def-constructor (multidimensional dimensions #:optional default)
    (if (null? dimensions)
	default
	(-> this constructor-map (car dimensions) (lambda (n)
						    (<array/multidimensional> (cdr dimensions) default)))))
  )



#!
(define a (<array> 0 1 2 3 4 5 6 7 8))
(begin a)
(-> a get-list)
(a 0 10)
(a 1 11)
(a 0)
(a 1)
(-> a get-list)
(a "2:6")
(-> a set! 9 8 7 6 5)
(-> a get-list)
(-> a set!! 9 8 7 6 5)
(-> a get-list)
(-> a map list)
(-> a reset!)
(-> a get-list)
(-> a dir)

(define a (<array/multidimensional> '(5 4)))
(-> a for-each (lambda (n1 el1) (-> el1 map! (lambda (n2 el2) (exact->inexact (+ n1 (/ n2 10)))))))
(-> a map (lambda (n el) (-> el get-list)))
((a 0) 3)
((a 3) 2)
!#






;;##############################################################
;; A hook class.
;;##############################################################
(def-class (<hook>)
  (define funcs '())
  (define system-funcs '())
  (define steelfunc #f)
  (def-method (add! func)
    (set! funcs (cons func funcs)))
  (def-method (add-system! func)
    (set! system-funcs (cons func system-funcs)))
  (def-method (only! func)
    (set! steelfunc func))
  (def-method (not-only!)
    (set! steelfunc #f))
  (def-method (remove! func)
    (set! funcs (remove! (lambda (f) (eq? f func))
			 funcs)))
  (def-method (run . args)
    (if steelfunc
	(apply steelfunc args)
	(call-with-current-continuation
	 (lambda (return)
	   (for-each (lambda (func)
		       (if (eq? 'stop! (apply func args))
			   (return 'stop!)))
		     (append system-funcs funcs)))))))



