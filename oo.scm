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
;;not interfere with goops.

;;Theres a lot of special macros specified in this file, and I have the following lines in my .emacs file
;;to make them look better:

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
 '(("(\\(def-var\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face
       nil t))))

(put 'letrec* 'scheme-indent-function 1)
(font-lock-add-keywords
 'scheme-mode
 '(("(\\(letrec[*]\\)\\>\\s-*(?\\(\\sw+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face
       nil t))))


!#


(provide 'snd-oo.scm)

(use-modules (ice-9 optargs)
	     (ice-9 format))
;	     (srfi srfi-1))
;;	     (srfi srfi-26))

(if (not srfi-loaded)
    (use-modules (srfi srfi-1)))
(set! srfi-loaded #t)


(define-macro (c-load-from-path filename)
  `(if (not (provided? (symbol-append 'snd- ',filename '.scm)))
       (load-from-path (symbol->string (symbol-append ',filename '.scm)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Various functions ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (define-toplevel symbol val)
  `(primitive-eval `(define ,,symbol ,,val)))


;; Snd has its own filter function (a clm function) overriding the guile filter function. This affects
;; remove, because remove is based on filter. Redefine remove:
(define (remove pred list)
  (if (null? list)
      '()
      (if (pred (car list))
	  (remove pred (cdr list))
	  (cons (car list) (remove pred (cdr list))))))

;; Snd has its own filter function (a clm function) overriding the guile filter function.
(define (filter-org pred list)
  (remove (lambda (e) (not (pred e)))
	  list))

(define <-> string-append)
(define <_> symbol-append)

(define (nth n list)
  (list-ref list n))

(define (nth-cdr n list)
  (cond ((= 0 n)
	 list)
	(else
	 (nth-cdr (1- n) (cdr list)))))

(define (sublist l start end)
  (take (drop l start) (- end start)))

;;(sublist '(0 1 2 3 4 5 6 7 8 9) 2 5)

(define (flatten tree)
  (cond ((null? tree) '())
	((pair? (car tree))
	 (append (flatten (car tree))
		 (flatten (cdr tree))))
	(else
	 (cons (car tree) (flatten (cdr tree))))))

(define-macro (push! val where)
  (let ((ret (gensym)))
    `(let ((,ret ,val))
       (set! ,where (cons ,ret ,where))
       ,ret)))

(define-macro (push-back! val where)
  (let ((ret (gensym)))
    `(let ((,ret ,val))
       (set! ,where (append! ,where (list ,ret)))
       ,ret)))

(define-macro (inc! var how-much)
  `(begin
     (set! ,var (+ ,how-much ,var))
     ,var))

(define-macro (when cond . rest)
  `(cond (,cond (let ()
                  ,@rest))
	 (else '())))

(define (c-atleast1.7?)
  (or (>= (string->number (major-version)) 2)
      (and (string=? "1" (major-version))
	   (>= (string->number (minor-version)) 7))))


(define (c-before1.8?)
  (and (= 1 (string->number (major-version)))
       (< (string->number (minor-version)) 8)))


(define (c-butlast l)
  (if (null? l)
      l
      (reverse! (cdr (reverse l)))))



(define (hash-table->alist table)
  (hash-fold (lambda (key value s) (cons (cons key value) s)) '() 
	     table))
(define hash->list hash-table->alist)


(define (deep-list-copy list)
  (cond ((not (pair? list))
	 list)
	(else
	 (cons (deep-list-copy (car list))
	       (deep-list-copy (cdr list))))))



(define (c-integer somekindofnumberorsomething)
;;    somekindofnumberorsomething)
  (inexact->exact (floor somekindofnumberorsomething)))

(define (c-integer2 somekindofnumberorsomething)
  (inexact->exact (floor somekindofnumberorsomething)))


;; C-like for-iterator
(define (c-for init pred least add proc)
  (do ((n init (+ n add)))
      ((not (pred n least)))
    (proc n)))


#!
(c-for 2 < 7 1
       (lambda (n) (display n)(newline)))
!#


(define (c-for-each func . lists)
  (let ((n 0))
    (apply for-each (cons (lambda els
			    (apply func (cons n els))
			    (set! n (1+ n)))
			  lists))))

(define (append-various . rest)
  (apply symbol-append (map (lambda (r)
			      (cond ((keyword? r) (keyword->symbol r))
				    ((string? r) (string->symbol r))
				    ((not (symbol? r))
				     'unknown)
				    (else r)))
			    rest)))


;; !!!!!!!
(define (c-display . args)
  ;(if (not (show-listener))
  ;    (set! (show-listener) #t))
  ;(gtk_paned_set_position (GTK_PANED (list-ref (main-widgets) 3)) (c-integer (* (window-height) 0.75)))
  (set! (show-listener) #t)
  (let ((printfunc (if (show-listener #f) snd-print display)))
    (c-for-each (lambda (n arg)
		  (if (> n 0)
		      (printfunc " "))
		  (printfunc arg))
		args)
    (printfunc #\newline))
  (if (defined? 'gtk_events_pending)
      (while (= 1 (gtk_events_pending))
	(gtk_main_iteration))))

;  (gtk_main_iteration_do #f))

;(define (print . args)
;  (display (car args))
;  (for-each (lambda (x) (display " ")(display x)) (cdr args))
;  (newline))

(define (print . args)
  (define result #f)
  (if (pair? args)
      (begin
	(write (car args))
	(for-each (lambda (x) (display #\ )(write x)) (cdr args))
	(set! result (car (reverse args)))))
  (newline)
  result)


(define-macro (define*2 def . code)    
  (cond ((or (not (proper-list? def))
	     (not (memv :rest def)))
	 `(define* ,def ,@code))
	(else
	 (let* ((keyargs '())
		(rest-name #f)
		(defargs (let loop ((arg (cdr def))
				   (inkeys #f))
			  (cond ((null? arg)
				 '())
				((eqv? :key (car arg))
				 (cons :key
				       (loop (cdr arg) #t)))
				((eqv? :rest (car arg))
				 (set! rest-name (cadr arg))
				 arg)
				((eqv? :allow-other-keys (car arg))
				 (set! rest-name (caddr arg))
				 arg)
				;;(cdr arg))
				(inkeys
				 (let ((keyarg (list (if (pair? (car arg))
							 (caar arg)
							 (car arg))
						     (if (pair? (car arg))
							 (cadr (car arg))
							 #f))))
				   (push-back! keyarg keyargs)
				   (cons (list (car keyarg)
					       ''undefined)
					 (loop (cdr arg)
					       inkeys))))
				(else
				 (cons (car arg)
				       (loop (cdr arg)
					     inkeys)))))))
	   ;;(c-display "keyargs/defarg" keyargs "\n" defargs)
	   `(define* (,(car def) ,@defargs)
	      ,@(map (lambda (keyarg)
		       `(cond ((eq? 'undefined ,(car keyarg))
			       (set! ,(car keyarg) ,(cadr keyarg)))
			      (else
			       (set! ,rest-name (cddr ,rest-name)))))
		     keyargs)
	      (let ()
		,@code))))))

#!
(pretty-print (macroexpand '(define*2 (ai :key 
					  (b 2)
					  c
					  :rest rest)
			      (c-display b c rest)
			      )))
(define*2 (ai :key 
	      (b 2)
	      :rest rest)
  (c-display b rest)
  )

(ai :b 1 :c 2 3 4 5)
(ai :b 1 3 4 5)
(ai 3 4 5)
!#

(define-macro (letrec* vardecls . body)
  (let* ((sets '())
	 (newvardecls (map (lambda (vardecl)
			     (if (not (number? (cadr vardecl)))
				 (begin
				   (set! sets (cons `(set! ,(car vardecl) ,(cadr vardecl)) sets))
				   `(,(car vardecl) #f))
				 vardecl))
			   vardecls)))
    `(let* ,newvardecls
       ,@(reverse! sets)
       (let ()
	 ,@body))))

#!
(letrec* ((a (+ d 2))
	  (b (lambda () (+ (c) a d)))
	  (c (lambda () 7))
	  (d 6))
  (+ a (b)))
->
(let* ((a #f)
       (b #f)
       (c #f)
       (d 6))
  (set! a (+ d 2))
  (set! b (lambda () (+ (c) a d)))
  (set! c (lambda () 7))
  (+ a (b)))
!#
	       

(define-macro (c-time form)
     `(let* ((t1 (rte-time))
	     (r ,form)
	     (t2 (rte-time)))
	(display "Time: ")
	(display (- t2 t1))
	(newline)
	r))


(define (make-container)
  (let ((container (make-hash-table 219)))
    (hashq-set! container 'hash-table container)
    (hashq-set! container 'reset (lambda ()
				   (set! container (make-hash-table 219))))
    (lambda (which . rest)
      (if (null? rest)
          (hashq-ref container which)
          (hashq-set! container which (car rest))))))


#!
(define c (make-container))
(c 'gakk 45)
(c 'gakk)

((c 'reset))

(-> c gakk)
(-> c hash-table)

(-> c hash-table (make-hash-table 219))
!#


(define (supereval func)
  (let* ((filename (tmpnam))
         (fd (open-file filename "w")))
    (delete-at-exit filename)
    (func (lambda something
            (for-each (lambda (s)
                        (display s fd))
                      something)))
    (close fd)
    (load filename)
    (delete-file filename)))

#!
(supereval (lambda (out)
             (out "(c-display \"hello\")")))
(supereval (lambda (out)
             (out '(c-display 'hello))
             (out '(newline))))
!#


(define schemecodeparser-varlist '())

(define (schemecodeparser-get-varlist)
  schemecodeparser-varlist)



(define* (schemecodeparser expr :key elsefunc symbolfunc keywordfunc atomfunc nullfunc pairfunc use-customsymbolhandler? customsymbolhandler blockhandler symbolhandler (varlist '()))

  (let parse ((varlist varlist)
	      (expr expr))

    (define (blockhandlerfunc varlist expr)
      ;;(c-display "blockhandlerfunc" varlist expr)
      (if (not blockhandler)
	  (map (lambda (expr)
		 (parse varlist expr))
	       expr)
	  (begin
	    (set! schemecodeparser-varlist varlist)
	    (blockhandler expr))))

    ;; Like append, but varlist1 might not be a valid list (in case of optional arguments)
    (define (append-varlists varlist1 varlist2)
      (append (let append ((varlist varlist1))
		(cond ((null? varlist) varlist)
		      ((not (pair? varlist)) (list varlist))
		      (else (cons (car varlist)
				  (append (cdr varlist))))))
	      varlist2))
		       
    (set! schemecodeparser-varlist varlist)

    ;;(c-display "scp/expr:" expr)

    (cond ((and (symbol? expr)
		symbolfunc)
	   (symbolfunc expr))
          ((and (keyword? expr)
                keywordfunc)
           (keywordfunc expr))
	  ((not (pair? expr)) 
	   (if atomfunc
	       (atomfunc expr)
	       expr))
	  ((null? expr) 
	   (if nullfunc
	       (nullfunc expr)
	       expr))
	  ((pair? (car expr))
	   (if pairfunc
	       (pairfunc expr)
	       (blockhandlerfunc varlist expr)))
	  ((and use-customsymbolhandler?
		(use-customsymbolhandler? expr))
	   (customsymbolhandler expr))
	  ((eq? 'lambda (car expr))
	   `(lambda ,(cadr expr)
	      ,@(blockhandlerfunc (append-varlists (cadr expr) varlist) (cddr expr))))
	  ((eq? 'define (car expr))
	   `(define ,(cadr expr)
	      ,@(blockhandlerfunc (append-varlists (cadr expr) varlist) (cddr expr))))
	  ((eq? 'delay (car expr))
	   `(delay ,@(blockhandlerfunc varlist (cdr expr))))
	  ((eq? 'force (car expr))
	   `(force ,@(blockhandlerfunc varlist (cdr expr))))
	  ((eq? 'do (car expr))
	   (let* ((newvars (append (map car (cadr expr)) varlist))
		  (first (map (lambda (a)
				(let ((second (parse varlist (cadr a))))
				  `(,(car a) ,second ,@(blockhandlerfunc newvars (cddr a)))))
			      (cadr expr))))
	     `(do ,first
		  ,@(blockhandlerfunc newvars (cddr expr)))))
	  ;; named let
	  ((and (eq? 'let (car expr))
		(symbol? (cadr expr)))
	   (let* ((newvars (append (cons (cadr expr) (map car (caddr expr)))
				   varlist))
		  (vars (map (lambda (a)
			       `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
			     (caddr expr))))
	     `(let ,(cadr expr) ,vars
		   ,@(blockhandlerfunc newvars (cdddr expr)))))
	  ((eq? 'let (car expr))
	   (let ((vars (map (lambda (a)
			 `(,(car a) ,@(blockhandlerfunc varlist (cdr a))))
		       (cadr expr))))
	     `(let ,vars
		,@(blockhandlerfunc (append (map car (cadr expr))
					    varlist)
				    (cddr expr)))))
	  ((eq? 'let* (car expr))
	   (let* ((newvars varlist)
		  ;; This needs to be generated outside quasiquote, because quasiqote elements does not need to be generated in order. (@#$@#$@#$@!!!#$@)
		  (let*vars (map (lambda (a)
				   (let ((ret `(,(car a) ,@(blockhandlerfunc newvars (cdr a)))))
				     (push! (car a) newvars)
				     ret))
				 (cadr expr))))
	     `(let* ,let*vars
		,@(blockhandlerfunc newvars (cddr expr)))))
	  ((eq? 'letrec (car expr))
	   (let* ((newvars (append (map car (cadr expr))
				   varlist))
		  (vars (map (lambda (a)
			      `(,(car a) ,@(blockhandlerfunc newvars (cdr a)))) ;; Not entily correct...
			     (cadr expr)))) 
	     `(letrec ,vars
		,@(blockhandlerfunc newvars (cddr expr)))))
	  ((or (eq? 'quote (car expr))
	       (eq? 'QUOTE (car expr)))
	   expr)
	  ((or (eq? 'quasiquote (car expr))
	       (eq? 'QUASIQUOTE (car expr)))
	   ;;(c-display "Warning in macros.scm/schemecodeparser: quasiquote not handled")
	   expr)
	  ((eq? 'cond (car expr))
	   `(cond ,@(map (lambda (exprs)
			   (let ((test (parse varlist (car exprs))))
			     `(,test ,@(blockhandlerfunc varlist (cdr exprs)))))
			 (cdr expr))))
	  ((eq? 'case (car expr))
	   (let ((first (parse varlist (cadr expr))))
	     `(case ,first
		,@(map (lambda (expr)
			 `(,(car expr) ,@(blockhandlerfunc varlist (cdr expr))))
		       (cddr expr)))))
	  ((and symbolhandler
		(eq? (car expr) (car symbolhandler)))
	   ((cadr symbolhandler) expr))
	  (else
	   (if elsefunc
	       (elsefunc expr)
	       `(,(parse varlist (car expr)) ,@(map (lambda (expr)
                                                      (parse varlist expr))
                                                    (cdr expr))))))))

(define (fix-defines-do terms)
  ;;(c-display terms)
  (schemecodeparser terms
		    :blockhandler
		    (lambda (terms)
		      ;(if (not (eq? 'define (car terms)))
		;	  (let ((temp (c-macroexpand-1 terms)))
		;	    (if (eq? 'define (car temp))
		;		(set! terms temp))))
		      ;;(c-display "terms fdd" terms)
		      (let* ((defines '())
			     (newterms (map (lambda (terms)
						       (if (and (pair? terms)
								(eq? 'define (car terms)))
							   (if (pair? (cadr terms))
							       (begin
								 (push! (car (cadr terms)) defines)
								 `(set! ,(car (cadr terms)) (lambda ,(cdr (cadr terms)) ,@(cddr terms))))
							       (begin
								 (push! (cadr terms) defines)
								 `(set! ,(cadr terms) ,@(cddr terms))))
							   terms))
						     terms)))
			(if (null? defines)
			    (map fix-defines-do terms)
			    `((let* ,(delete-duplicates (map (lambda (name)
							       `(,name #f))
							     (reverse! defines)))
				,@(map fix-defines-do newterms))))))))


(define-macro (fix-defines . terms)
  `(let ()
     ,@(fix-defines-do terms)))


#!
(fix-defines-do '(let ()
		   (set! a 5)
		   (define b 6)
		   (define (c) 7)))


(macroexpand '(fix-defines 
	       (set! a 5)
	       (define b 6)
	       (define (c) 7)))


!#


(define-macro (c-loop var min-val check-func max-val inc-val . body)
  (define das-min (gensym))
  (define das-max (gensym))
  (define loop (gensym))
  `(let ((,das-min ,min-val)
	 (,das-max ,max-val))
     (let ,loop ((,var ,das-min))
	  (if (,check-func ,var ,das-max)
	      (begin
		(let ((,(symbol-append var '.scale) (lambda (das-matches . das-rest)
						      (if (not (null? das-rest))
							  (c-scale ,var ,das-min ,das-max das-matches (car das-rest))
							  (fix-defines
							   (define get-time car)
							    (define get-val cadr)
							    (define das-first (car das-matches))
							    (define das-last (last das-matches))
							    (define matches (map (lambda (match)
										   (cons (c-scale (get-time match)
												  (get-time das-first) (get-time das-last)
												  ,das-min ,das-max)
											 (cdr match)))
										 das-matches))
							    (define (find-matchpointer matches)
							      (if (or (null? (cddr matches))
								      (and (>= ,var (get-time (car matches)))
									   (<= ,var (get-time (cadr matches)))))
								  matches
								  (find-matchpointer (cdr matches))))
							    (define matchpointer (find-matchpointer matches))
							    (define match (car matchpointer))
							    (define time (get-time match))
							    (define val (get-val match))
							    (define next-match (cadr matchpointer))
							    (define next-time (get-time next-match))
							    (define next-val (get-val next-match))
							    (c-scale ,var time next-time val next-val))))))
		  ,@body
		  (,loop (+ ,var ,inc-val))))))))

#!
(c-loop i 0 < 20 (i.scale `((1 1) (2 0.5) (3 1)))
	(c-display i))
!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Structs (redefined by rt-compiler) ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (=> object das-method . rest)
  (define method (keyword->symbol das-method))
  (define object-decomposed (map string->symbol (string-split (symbol->string object) #\:)))
  (let ()
    (define struct-name (car object-decomposed))
    (define object-name (if (null? (cdr object-decomposed))
			    (car object-decomposed)
			    (cadr object-decomposed)))
    `(,(append-various 'access- struct-name ":" method) ,object-name ,@rest)))


(define-macro (define-rt-something-struct something name . das-slots)
  (define name-name (gensym))
  (define val-name (gensym))
  (define slots '())
  
  (for-each (lambda (slot)
	      (if (keyword? slot)
		  (push-back! (list (append-various slot) 0) slots)
		  (set-cdr! (last slots) (list slot))))
	    das-slots)

  (let ((slot-names (map car slots)))
    `(begin

       ;; guile
       (define* (,(symbol-append 'make- name) :key ,@slots)
	 (,something ,@slot-names))
       ,@(let ((i -1))
	   (map-in-order (lambda (slot)
			   (set! i (1+ i))
			   `(define ,(append-various 'access- name ":" slot)
			      (make-procedure-with-setter
			       (lambda (,name-name)
				 (,(symbol-append something '-ref) ,name-name ,i))
			       (lambda (,name-name ,val-name)
				 (,(symbol-append something '-set!) ,name-name ,i ,val-name)))))
			 slot-names))
       )))


(define-macro (define-rt-vct-struct name . das-slots)
  `(define-rt-something-struct vct ,name ,@das-slots))

(define-macro (define-rt-vector-struct name . das-slots)
  `(define-rt-something-struct vector ,name ,@das-slots))


#!
(define-rt-vct-struct str
  :a 1
  :b)
(set! (=> str:str) 200)
(set! (=> str :b) 200)
(=> str:str)
(=> str :b)

(macroexpand '(=> str:str :b))
(macroexpand '(=> str :b))
!#





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Finalizer     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-rt-vector-struct das-guardian
  :finalizer
  :guardian
  :num-visitors 1)

(define das-guardians '())

;; Make sure cleanup-func doesn't refer to object somehow.
;; This, for example:
;;  (let ((obj (list 2 3))) (add-guardian-object obj (lambda x x)))
;; won't work.

(define (add-finalizer object finalizer)
  (let ((das-guardian (member finalizer das-guardians (lambda (finalizer das-guardian)
							(eqv? finalizer (=> das-guardian :finalizer))))))
    ;;(c-display "hmm" das-guardian)
    (if das-guardian
	(begin
	  (set! das-guardian (car das-guardian))
	  ((=> das-guardian :guardian) object)
	  (set! (=> das-guardian :num-visitors) (1+ (=> das-guardian :num-visitors))))
	(push! (make-das-guardian :finalizer finalizer
				  :guardian (let ((guardian (make-guardian)))
					      (guardian object)
					      guardian))
	       das-guardians)))
  (set! das-guardians
	(remove (lambda (das-guardian)
		  (let loop ((object ((=> das-guardian :guardian))))
		    (and object
			 (begin
			   ((=> das-guardian :finalizer) object)
			   (set! (=> das-guardian :num-visitors) (1- (=> das-guardian :num-visitors)))
			   (if (= 0 (=> das-guardian :num-visitors))
			       #t
			       (loop ((=> das-guardian :guardian))))))))
		das-guardians)))

#!
(define alist (list 'unique-name 2 3 4))
(define cleanfunc (lambda (object) (c-display "freeing" object)))
(add-finalizer alist cleanfunc)
(add-finalizer alist (lambda (object)
		       (c-display object "was garbage collected")))
(gc)

(map cadr das-guardians)
(map caddr das-guardians)
(length das-guardians)
(set! alist #f)

(begin das-guardians)

!#
		      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Class definition, etc. ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (def-class def . body)

  (define newvars '())
  (define newbody '())
  
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

  (set! newbody (map-in-order (lambda (t)
				(cond ((or (eq? (car t) 'define)
					   (eq? (car t) 'define*))
				       (if (list? (cadr t))
					   (set! newvars (cons (car (cadr t)) newvars))
					   (set! newvars (cons (cadr t) newvars)))
				       (if (list? (cadr t))
					   (if (eq? (car t) 'define*)
					       `(set! ,(car (cadr t)) (lambda* ,(cdr (cadr t))
									,@(cddr t)))
					       `(set! ,(car (cadr t)) (lambda ,(cdr (cadr t))
									,@(cddr t))))
					   `(set! ,(cadr t) ,(caddr t))))
				      ((eq? (car t) 'def-method)
				       (let* ((nameandvars (cadr t))
					      (body (cddr t))
					      (defname (symbol-append 'this-> (car nameandvars))))
					 (set! newvars (cons defname newvars))
					 (if (and (list? nameandvars)
						  (or (member #:optional nameandvars)
						      (member #:rest nameandvars)
						      (member #:key nameandvars)))
					     `(set! ,defname (add-method2* ,nameandvars ,@body))
					     `(set! ,defname (add-method2 ,nameandvars ,@body)))))
				      ((eq? (car t) 'def-var)
				       (if (= 2 (length t))
					   `(add-method2* ',(cadr t) (lambda rest
								       (if (null? rest)
									   ,(cadr t)
									   (set! ,(cadr t) (car rest)))))
					   (let* ((name (cadr t))
						  (initial (caddr t))
						  (thisname name));;(symbol-append 'this-> name)))
					     (set! newvars (cons thisname newvars))
					     `(begin
						(add-method2 (,name . rest) (if (null? rest) ,thisname (set! ,thisname (car rest))))
						(set! ,thisname ,initial)))))
				      ((eq? (car t) 'def-constructor)
				       (let* ((nameandvars (cadr t))
					      (body (cddr t))
					      (name (car nameandvars))
					      (args (cdr nameandvars))
					      (name2 (symbol-append 'constructor- name)))
					 `(add-method2* ,(cons name2 args) ,@body)))
				      (else
				       t)))
			      body))


  ;;(c-display "newvars" newvars)
  ;;(c-display "newbody" newbody)


  `(define* ,def
     (let* ((methods (make-hash-table 251))
	    (supers '())
	    (super (lambda args (c-display "\n\nError! \"super\" is not a method. Perhaps you ment \"Super\"?\n\n")))
	    (add-super! (lambda (asuper)
			  (if (null? supers)
			      (set! super asuper))
			  (set! supers (cons asuper supers))))
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
			     (hashq-set! methods name func)
			     func))
	    ,@(map (lambda (var)
		     (list var #f))
		   (reverse! newvars)))
       
       (def-var class-name ',(car def))

       (def-method (add-method name func)
	 (add-method-do name func))
       
       (def-method (dir)
	 (append (cons class-name
		       (hash-fold (lambda (key value s) (cons key s)) '() 
				  methods))
		 (map (lambda (super) (-> super dir))
		      supers)))
       (def-method (get-method name)
	 (or (hashq-ref methods name)
	     (any (lambda (super) (-> super get-method name))
		  supers)))
       (def-method (instance? class-name*)
	 (or (eq? class-name* class-name)
	     (any (lambda (super) (-> super instance? class-name*))
		  supers)))
       
       (define (this name . rest)
	 (apply (or (hashq-ref methods name)
		    (any (lambda (super) (-> super get-method name))
			 supers)
		    (lambda x (format #t "No such method: \"~A\" in class \"~A\".~%" name class-name)))
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
		       (lambda x (format #t "No such method: \"~A\" in class \"~A\".~%" m class-name)))
		   rest))))
       
       (define (this-with-custom-dispatcher m . rest)
	 (if (dispatch-preds m rest)
	     (dispatch-funcs m rest)
	     (apply (or (hashq-ref methods m)
			(any (lambda (super) (-> super get-method m))
			     supers)
			(lambda x (format #t "No such method: \"~A\" in class \"~A\".~%" m class-name)))
		    rest)))
       
       ,@newbody
       
       (if (and this dispatch-preds)
	   (if (procedure? dispatch-preds)
	       (set! this this-with-custom-dispatcher)
	       (set! this this-with-custom-dispatchers)))
       
       this)))


(define-macro (add-method2 nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda ,(cdr nameandvars) ,@body)))

(define-macro (add-method2* nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda* ,(cdr nameandvars) ,@body)))

#!
(def-class (gakk)
  (define g 2)
  (define h (+ g 2))
  (c-display "ai")
  (def-var a 5)
  (c-display "h:" h))

(begin gakk)
(define g (gakk))
(-> g a)
(-> g add-method 'tja (lambda (c)
			 90))
(-> g dir)
(-> g tja 2)
!#

(define-macro (def-method nameandvars . body)
  (if (and (list? nameandvars)
	   (or (member #:optional nameandvars)
	       (member #:rest nameandvars)
	       (member #:key nameandvars)))
      `(define ,(symbol-append 'this-> (car nameandvars))
	 (add-method2* ,nameandvars ,@body))
      `(define ,(symbol-append 'this-> (car nameandvars))
	 (add-method2 ,nameandvars ,@body))))

(define-macro (def-var name . initial)
  (if (null? initial)
      `(add-method ',name (lambda rest
			    (if (null? rest)
				,name
				(set! ,name (car rest)))))
      (let ((thisname name ));;(symbol-append 'this-> name)))
	`(define ,thisname
	   (begin
	     (add-method2 (,name . rest) (if (null? rest) ,thisname (set! ,thisname (car rest))))
	     ,(car initial))))))


(define-macro (def-constructor nameandvars . body)
  (let* ((name (car nameandvars))
	 (args (cdr nameandvars))
	 (name2 (symbol-append 'constructor- name)))
    `(add-method2* ,(cons name2 args) ,@body)))

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
     (for-each add-super! (list ,@rest))))



;; The -> macro caches the function pointer. Generally a little bit faster than ->2.
;;
;; Warning! When dynamically generating "->"-calls, its easy to make memory-leaking code.
;; If you thing that is whats happening, rename "->3" to "->" and "->3" to "->", and see if that helps.
;; If it did, change the "->"-calls thats causing the leak into "->2"-calls.
;;
;; Nah, this wasn't such a good idea... ->-bad-idea is the old ->
(define-macro (->-bad-idea object method . args)
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

(define-macro (-> object method . args)
  (if (number? object)
      `(list-set! ,method ,object ,(car args))
      `(,object ',method ,@args)))

(define-macro (<- object method)
  (if (number? object)
      `(list-ref ,method ,object)
      `(-> ,object get-method ',method)))


#!
(def-class (<gakk>)
  (define a #f)
  (define b #f)
  (set! a 5)
  (set! b a))
  (define a 5)
  (define b a)
  )
(define (<gakk>)
  (define a 5)
  (define b a)
  b)

(define gakk (<gakk>))

(class (<wefwe> wrg)
  (def-method (ai val)
    5))

(class (<test>)
  (def-method (ai val)
    val))
(define a (<test>))
(-> a ai 5)

(def-class (<super1> sum)
  (def-var avar 2)
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



