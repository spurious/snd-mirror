#!


This is code to evaluate prefix-notated C-code on the fly with guile.
See gui.scm and ladspa.scm for examples of use.

Kjetil Matheussen, 20.12.2004





EVAL-C
------

eval-c takes as arguments lisp- and C-like blocks, generates c-code from it, and runs it.

Some reasons to use eval-c:

* Easy integration of c-code from within lisp.
  Mix C and Lisp in the same source without making it look strange. (hopefully)
* Use lisp-macros to generate c-code. (There is a special macro function
  called "define-c-macro" that works with eval-c)
* Generate/compile/link/run c-code on the fly.
* Some people think prefix notation is nice.
* Speed. C is faster than guile.
* Hides guile-semantic to access C-code from guile. Less need to read the guile manual.
* Global functions does not need to be defined at the top-level. (that is a good thing,
  right?)



Examples.
--------

The simplest fibonacci function:

(define-c <int> fib ((<int> n))
  (if (< n 2)
      (return n)
      (return (+ (fib (- n 1))
		 (fib (- n 2)))))))

(define-c fib <int> ((<int> n))
  (if (< n 2)
      (return n)
      (return (+ (fib (- n 1))
		 (fib (- n 2)))))))


The define-c macro will produce the following code:

(eval-c ""
	(public
	 (<int> fib (lambda ((<int> n))
		      (if (< n 2)
			  (return n)
			  (return (+ (fib (- n 1))
				     (fib (- n 2)))))))))


The "public" macro will change the code so that it looks something like this:

(eval-c ""
	(<int> fib (lambda ((<int> n))
		     (if (< n 2)
			 (return n)
			 (return (+ (fib (- n 1))
				    (fib (- n 2)))))))
	(<SCM> fib_eval_c_helper (lambda ((<SCM> n))
					  (return (MAKE_INTEGER (fib (GET_INTEGER n))))))
	(run-now
	 (scm_c_define_gsubr (string "fib") 1 0 0 fib_eval_c_helper)))


And after running the "lambda", "if", "<", "+", "-", "string" and "run-now" macros (most of eval-c is defined
as macros), eval-c will produce and run the following c-code:

static int fib (int n){
  if ((n < 2))
    return (n);
  else
    return ((fib ((n - 1)) + fib ((n - 2))));
}
static SCM fib_eval_c_helper (SCM n){
  return (MAKE_INTEGER (fib (GET_INTEGER (n))));
}
static void run_now_1 (){
  scm_c_define_gsubr ("fib", 1, 0, 0, fib_eval_c_helper);
}


The first function is the fibonacci generator, and the
second function is the guile-wrapper. (GET_INTEGER and
MAKE_INTEGER are just simple C macros.)
"run_now"-functions are run once when the file is loaded.

From guile you have now a function called "fib" which takes
one argument.



Hello world looks like this:

(eval-c ""
	(run-now
	 (printf (string "Hello world!\\n"))))




First argument to eval c is a string with compiling/linking options.
Usually just "", but can be "-lsnd" or something if needed.


!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 optargs)
	     (srfi srfi-1)
	     (srfi srfi-13)
	     (ice-9 rdelim))


(provide 'snd-eval-c.scm)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Public variables ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-verbose #f)
(define eval-c-very-verbose #f)
(define eval-c-cleanup #t)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Various functions ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <-> string-append)

(define (c-atleast1.7?)
  (or (>= (string->number (major-version)) 2)
      (and (string=? "1" (major-version))
	   (>= (string->number (minor-version)) 7))))

(define (c-butlast l)
  (reverse! (cdr (reverse l))))

(define (c-for-each func . lists)
  (let ((n 0))
    (apply for-each (cons (lambda els
			    (apply func (cons n els))
			    (set! n (1+ n)))
			  lists))))

(define (c-display . args)
  (c-for-each (lambda (n arg)
		(if (> n 0)
		    (display " "))
		(display arg))
	      args)
  (newline))


(define (c-tosymbol something)
  (cond ((string? something) (string->symbol something))
	((symbol? something) something)
	(else
	 (c-display "Error in eval-c.scm/c-tosymbol. Not able to handle " something ".")
	 (thisisnotafunctionhopefully))))

(define (c-tostring something)
  (cond ((string? something) something)
	((number? something) (number->string something))
	((symbol? something) (symbol->string something))
	((char? something) (string something))
	(else
	 (c-display "Error in eval-c.scm/c-tostring. Not able to handle " something ".")
	 (thisisnotafunctionhopefully))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-macro-prefix 'eval-c-macro-)

(define eval-c-run-nows '())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Type Handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define eval-c-void-types (list "void"))
(define eval-c-string-types (list "char *" "char*" "gchar*" "char *"))
(define eval-c-int-types (list "int" "long" "short" "char" "gint" "size_t"))
(define eval-c-float-types (list "float" "double"))

;; etype: '<static-int>
;; ctype: "static int"

(define (eval-c-isetype? type)
  (symbol? type))

(define (eval-c-isctype? type)
  (string? type))

(define (eval-c-ctype->etype das-type)
  (string->symbol (<-> "<" 
		       (list->string (map (lambda (c)
					    (if (eq? #\space c)
						#\-
						c))
					  (string->list das-type)))
		       ">")))


(define (eval-c-etype->ctype das-type)
  (let ((type (string->list (symbol->string das-type)))
	(ret '()))
    (if (eq? #\< (car type))
	(set! type (cdr type)))
    (if (eq? #\> (last type))
	(set! type (c-butlast type)))
    (list->string (map (lambda (c)
			 (if (eq? #\- c)
			     #\space
			     c))
		       type))))

(define (eval-c-etype type)
  (if (eval-c-isetype? type)
      type
      (eval-c-ctype->etype type)))

(define (eval-c-ctype type)
  (if (eval-c-isctype? type)
      type
      (eval-c-etype->ctype type)))

(define (eval-c-returnsametype orgtype das-ret)
  (if (and (eval-c-isetype? orgtype)
	   (eval-c-isctype? das-ret))
      (eval-c-ctype->etype das-ret)
      (if (and (eval-c-isctype? orgtype)
	       (eval-c-isetype? das-ret))
	  (eval-c-etype->ctype das-ret)
	  das-ret)))


(define (eval-c-strip-qualifiers das-type)
  (let* ((type-split (remove (lambda (s) (or (string=? "" s))) (string-split (eval-c-ctype das-type) #\space)))
	 (type (string-trim-right (apply <-> (map (lambda (t) (<-> t " ")) type-split))))
	 (type-first (car type-split)))
    (while (or (string=? type-first "unsigned")
	       (string=? type-first "signed")
	       (string=? type-first "static")
	       (string=? type-first "volatile")
	       (string=? type-first "const"))
	   (set! type-split (cdr type-split))
	   (set! type (string-trim-right (apply <-> (map (lambda (t) (<-> t " ")) type-split))))
	   (set! type-first (car type-split)))
    (eval-c-returnsametype das-type type)))

#!
(eval-c-strip-qualifiers "unsigned static   int")
(eval-c-strip-qualifiers '<unsigned-static-int>)
!#

(define (eval-c-get-propertype das-type)
  (<-> (eval-c-etype->ctype das-type) " "))

#!
(eval-c-get-propertype '<int-wefwe>)
!#

(define (eval-c-to-scm das-type)
  (let ((type (eval-c-strip-qualifiers das-type)))
    (cond ((member type eval-c-void-types) "UNSPECIFIED")
	  ((member type eval-c-string-types) "STRING")
	  ((member type eval-c-int-types) "INTEGER")
	  ((member type eval-c-float-types) "DOUBLE")
	  (else
	   "POINTER"))))

(define (eval-c-add-void-type type)
  (set! eval-c-void-types (cons type eval-c-void-types)))
(define (eval-c-add-string-type type)
  (set! eval-c-string-types (cons type eval-c-string-types)))
(define (eval-c-add-int-type type)
  (set! eval-c-int-types (cons type eval-c-int-types)))
(define (eval-c-add-float-type type)
  (set! eval-c-float-types (cons type eval-c-float-types)))


#!
(eval-c-to-scm "        static  const unsigned char   *")
!#

(define (eval-c-cify-var das-var)
  (if (and (not (string? das-var))
	   (not (symbol? das-var)))
      das-var
      (if (or (and (string? das-var)
		   (string=? "-" (string-trim-both das-var)))
	      (and (symbol? das-var)
		   (string=? "-" (string-trim-both (symbol->string das-var)))))
	  (if (symbol? das-var)
	      '-
	      "-")
	  (let* ((var (if (string? das-var)
			  das-var
			  (symbol->string das-var)))
		 (varlist (string->list var)))
	    (if (member #\- varlist)
		(let ((ret (apply <-> (map (lambda (c) (if (equal? #\- c)
							   "_minus_"
							   (string c)))
					   varlist))))
		  (if (symbol? das-var)
		      (string->symbol ret)
		      ret))
		das-var)))))
      
#!
(eval-c-cify-var '(aiai))
!#

(define (eval-c-symbol-is-type s)
  (and (not (or (eq? s '<->')
		(eq? s '<)
		(eq? s '>)))
       (let ((aslist (string->list (symbol->string s))))
	 (and (eq? #\<
		   (car aslist))
	      (eq? #\>
		   (last aslist))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-structlist '())

(define (eval-c-addstruct name varlist)
  (set! eval-c-structlist (cons (cons (c-tosymbol name) varlist)
				eval-c-structlist)))

(define (eval-c-getstruct name)
  (let ((struct (assq (c-tosymbol name) eval-c-structlist)))
    (if struct
	(cdr struct)
	#f)))

(define (eval-c-get-structlisttype struct name)
  (let ((elem (assq (c-tosymbol name) struct)))
    (if elem
	(cadr elem)
	(c-display "Eval-c.scm/eval-c-get-structlisttype. Error. " name " not found in structure " struct "."))))

(define (eval-c-get-structlistelem struct name)
  (let ((elem (assq (c-tosymbol name) struct)))
    (if elem
	(cdr elem)
	(c-display "Eval-c.scm/eval-c-get-structlistelem. Error. " name " not found in structure " struct "."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-classlist '())

(define eval-c-privatevars  cadr)
(define eval-c-publicvars   caddr)
(define eval-c-privatefuncs cadddr)
(define eval-c-publicfuncs  (lambda (x) (cadddr (cdr x))))
(define eval-c-destructors  (lambda (x) (cadddr (cddr x))))
(define eval-c-constructors (lambda (x) (cadddr (cdddr x))))
(define eval-c-privatevars  (lambda (x) (cadddr (cdddr (cdr x)))))

(define (eval-c-addclass name
			 privatevars
			 publicvars
			 privatefuncs
			 publicfuncs
			 destructors
			 constructors)
  (set! eval-c-classlist (cons (list name privatevars privatefuncs publicfuncs destructors constructors)
			       eval-c-classlist)))


(define (eval-c-get-cmethodname methodname classname publicorprivate)
  (c-tosymbol (<-> (c-tostring classname)
		   "_split_"
		   (c-tostring methodname)
		   "_split_"
		   publicorprivate)))

#!
(eval-c-get-cmethodname 'aiai 'Gakk "private")
!#

#!
;; To complicated. Perhaps later.
(define (eval-c-get-cmethodname methodname classtype rettype args)
  (define (gettype das-type)
    (let ((type (string->list (symbol->string das-type)))
	  (ret '()))
      (if (eq? #\< (car type))
	  (set! type (cdr type)))
      (if (eq? #\> (last type))
	  (set! type (c-butlast type)))
      (list->string type)))

  (<-> (gettype classtype)
       "_split_"
       (gettype rettype)
       "_split_"
       methodname
       (apply <-> (map (lambda (x) (<-> "_split_" (gettype (car x))))
		       args))))
!#

#!
(eval-c-get-cmethodname "c-scale" '<Gakk_class> '<int> '((<struct-int> a)(<int> b)))
->
"Gakk_class" "_split_" "int" "_split_" "c-scale" "_split_" "struct-int" "_split_" "int"
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Getting the type of a variable ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-c-typelistlist '())
(define eval-c-typelist '())

(define (eval-c-puttype name type)
  ;;(c-display "puttype, name: " name ", " type)
  (set! eval-c-typelist (cons (cons (c-tosymbol name) type) eval-c-typelist)))


(define (eval-c-get-structlist name)
  (let ((ret '())
	(temp '())
	(alist (string->list (c-tostring name))))
    (define (checkit)
      (define (get-tempstring)
	(set! ret (cons (string->symbol (list->string (reverse! temp))) ret))
	(set! temp '()))
      (if (eq? #\. (car alist))
	  (get-tempstring)
	  (if (eq? #\- (car alist))
	      (begin
		(set! alist (cdr alist))
		(get-tempstring))
	      (set! temp (cons (car alist) temp))))
      (set! alist (cdr alist))
      (if (not (null? alist))
	  (checkit)
	  (get-tempstring)))
    (checkit)
    (reverse! ret)))



#!
(eval-c-get-structlist 'gakk1.gakk2->gakk3.gakk5)
!#

(define (eval-c-getfulltype name)
  (let ((varnamelist (eval-c-get-structlist name)))

    (define (rec struct varnamelist)
      ;;(c-display "rec: " struct " " varnamelist)
      (let ((firstelem (eval-c-get-structlisttype struct (car varnamelist))))
	;;(c-display "firstelem: " firstelem)
	(if (= (length varnamelist) 1)
	    firstelem
	    (rec (eval-c-getstruct firstelem) (cdr varnamelist)))))
    
    ;;(c-display varnamelist)
    (let ((start (assq (c-tosymbol (car varnamelist)) eval-c-typelist)))
      (if start
	  (if (> (length varnamelist) 1)
	      (rec (eval-c-getstruct (eval-c-getfulltype (car varnamelist))) (cdr varnamelist))
	      (cdr start))
	  '<undefined_t>))))

(define (eval-c-gettype name)
  (eval-c-strip-qualifiers (eval-c-getfulltype name)))


#!
(eval-c-puttype 'a '<const-static-int>)
(eval-c-gettype 'a)
!#



#!
(eval-c-addstruct '<struct-struct1> '((a <int>)
				      (b <int>)))
(eval-c-addstruct '<struct-struct2> '((c <struct-struct1> *)
				      (d <int>)))
(define a (eval-c-getstruct '<struct-struct2>))
(eval-c-get-structlistelem a 'c)
(eval-c-get-structlisttype a 'c)
(eval-c-puttype 'a '<struct-struct2>)
(eval-c-gettype 'a)
(eval-c-gettype 'a.c)
(eval-c-gettype 'a->c->a)
;; (eval-c-gettype could/should be more intelligent regarding pointers and the use of "." or "->")
!#


(define eval-c-level 0)

(define (eval-c-uplevel)
  (set! eval-c-level (1+ eval-c-level))
  (set! eval-c-typelistlist (cons eval-c-typelist eval-c-typelistlist)))

(define (eval-c-downlevel)
  (set! eval-c-level (1- eval-c-level))
  (set! eval-c-typelist (car eval-c-typelistlist))
  (set! eval-c-typelistlist (cdr eval-c-typelistlist)))


#!
(begin eval-c-typelist)
(eval-c-puttype 'name3 'atype3)
(eval-c-gettype 'name2)
(eval-c-uplevel)
(eval-c-downlevel)
(assq 'name eval-c-typelist)
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-c-eval-funccall term)
  (if (null? (cdr term))
      (<-> (eval-c-cify-var (if (symbol? (car term))
				(symbol->string (car term))
				(car term)))
	   " ()")
      (apply <-> (append (list (<-> (eval-c-cify-var (if (symbol? (car term))
							 (symbol->string (car term))
							 (car term)))
				    " ("))
			 (map (lambda (x) (<-> (eval-c-parse x) ", ")) (c-butlast (cdr term)))
			 (list (eval-c-parse (last term)))
			 (list ")")))))


(define (eval-c-macro-result term)
  (let* ((a (cons (symbol-append eval-c-macro-prefix (car term)) (cdr term)))
	 (b (macroexpand-1 a )))
    (if (not (equal? a b))
	(eval-c-parse b)
	(eval-c-eval-funccall term))))


(define (eval-c-parse term)
  (if eval-c-very-verbose
      (c-display "top: " term))
  (cond 
   ((string? term) (<-> term " "))
   ((number? term) (<-> (number->string term) " "))
   ((symbol? term) (<-> (symbol->string term) " "))
   ((char? term) (<-> (string term) " "))

   ((list? term)
    (if (string? (car term))
	(eval-c-eval-funccall term)
	(let ((type (car term)))
	  (if (not (eval-c-symbol-is-type type))

	      (eval-c-macro-result term)

	      ;; (<type> ....)
	      (let* ((type (if (= 0 eval-c-level)
			       (eval-c-ctype->etype (<-> "static " (eval-c-etype->ctype type)))
			       type))
		     (varname (cadr term))
		     (isvarname? (or (string? varname) (symbol? varname))))
		
		(if (not isvarname?)

		    ;; (<type> () .... )
		    (eval-c-parse `(,type ,@(map eval-c-parse (cdr term))))

		    ;; (<type> varname ...)
		    (begin
		      (eval-c-puttype (if (string? varname)
					  (car (string-split varname #\ ))
					  varname)
				      type)

		      (if (and (= (length term) 3)
			       (or (not (list? (caddr term)))
				   (not (eq? 'lambda (caaddr term)))))
			  
			  ;; (<int> a 5)
			  (<-> (eval-c-get-propertype type)
			       (eval-c-parse (eval-c-cify-var varname))
			       " = "
			       (eval-c-parse (caddr term)))
			  
			  ;; (<int> a (lambda ...)) or (<int> a) (or?)
			  (apply <-> (map eval-c-parse (cons (eval-c-get-propertype type)
							     (cons (eval-c-cify-var varname)
								   (cddr term)))))))))))))))

#!
(eval-c-parse '(<int> (define (ai (<int> avar)) (return 2))))
(eval-c-parse '(<int> ai (lambda () (return 2))))
(eval-c-parse '(define (ai (<int> avar)) (return 2)))

!#

(define (eval-c-parse-line term)
  (if (and (string? term)
	   (eq? #\# (string-ref term 0)))
      (<-> term (string #\newline))
      (<-> (eval-c-parse term) ";" (string #\newline))))

(define (eval-c-parse-lines terms)
  (apply <-> (map (lambda (x) (<-> (eval-c-parse-line x) " ")) terms)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; C-macros;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (define-c-macro def . body)
  `(define-macro ,(cons (symbol-append eval-c-macro-prefix (car def)) (cdr def)) ,@body))



(define-c-macro (if a b . c)
  (if (null? c)
      `(<if> "(" ,a ")" #\newline
	     ,b)
      `(<if> "(" ,a ")" #\newline
	     ,b ";" #\newline
	     else #\newline
	     ,(car c))))



(define (eval-c-gen-inbetweens op rest)
  (let ((opstring (symbol->string op)))
    (apply <-> (append (list "(")
		       (list (eval-c-parse (car rest)))
		       (map (lambda (x) (<-> opstring " " (eval-c-parse x) " ")) (cdr rest))
		       (list ")")))))

(define-c-macro (+ . rest)
  (eval-c-gen-inbetweens '+ rest))
(define-c-macro (- . rest)
  (eval-c-gen-inbetweens '- rest))
(define-c-macro (* . rest)
  (eval-c-gen-inbetweens '* rest))
(define-c-macro (/ . rest)
  (eval-c-gen-inbetweens '/ rest))
(define-c-macro (| . rest)
  (eval-c-gen-inbetweens '| rest))
(define-c-macro (% . rest)
  (eval-c-gen-inbetweens '% rest))
(define-c-macro (& . rest)
  (eval-c-gen-inbetweens '& rest))
(define-c-macro (~ . rest)
  (eval-c-gen-inbetweens '~ rest))
(define-c-macro (== . rest)
  (eval-c-gen-inbetweens '== rest))
(define-c-macro (!= . rest)
  (eval-c-gen-inbetweens '!= rest))
(define-c-macro (< . rest)
  (eval-c-gen-inbetweens '< rest))
(define-c-macro (> . rest)
  (eval-c-gen-inbetweens '> rest))
(define-c-macro (<= . rest)
  (eval-c-gen-inbetweens '<= rest))
(define-c-macro (>= . rest)
  (eval-c-gen-inbetweens '>= rest))
(define-c-macro (|| . rest)
  (eval-c-gen-inbetweens '|| rest))
(define-c-macro (&& . rest)
  (eval-c-gen-inbetweens '&& rest))

(define-c-macro (begin . body)
  (eval-c-uplevel)
  (let ((ret (<-> "{" (string #\newline)
		  (eval-c-parse-lines body)
		  "}")))
    (eval-c-downlevel)
    ret))


(define (eval-c-cond->if terms)
  (let ((term (car terms)))
    (if (and (symbol? (car term))
	     (eq? 'else (car term)))
	(cons 'begin (cdr term))
	(list 'if (car term)
	      (append (list 'begin) (cdr term))
	      (eval-c-cond->if (cdr terms))))))
	      
	    

(define-c-macro (cond . terms)
  (eval-c-cond->if terms))


(define (eval-c-structure structname cname rest)
  ;;(c-display rest)
  (let ((das-map '())
	(even #f))
    (define (nest rest)
      (if (not (null? rest))
	  (let ((a (car rest)))
	    (if even
		(begin
		  (if (eq? '* a)
		      (begin
			(set! das-map (cons (list even a (cadr rest)) das-map))
			(set! rest (cdr rest)))
		      (set! das-map (cons (list even a) das-map)))
		  (set! even #f))
		(set! even a))
	    (nest (cdr rest)))))
    (nest rest)
    (set! das-map (reverse! das-map))
    (eval-c-addstruct structname
		      (map (lambda (a)
			     (if (= 3 (length a))
				 (list (caddr a) (car a) (cadr a))
				 (list (cadr a) (car a))))
			   das-map))
    (<-> "struct " cname
	 " {\n"
	 (apply <->
		(map (lambda (a)
		       (<-> "  "
			    (eval-c-get-propertype (car a))
			    (eval-c-parse (eval-c-cify-var (cadr a)))
			    (if (= 3 (length a))
				(eval-c-parse (caddr a))
				"")
			    ";\n"))
		     das-map))
	 "}")))

(define-c-macro (define-struct name . rest)
  (eval-c-structure (<-> "<struct-" (string-trim-right (eval-c-get-propertype name)) ">")
		    (eval-c-parse (eval-c-get-propertype name))
		    rest))

#!
(eval-c-macro-result
 '(define-struct <gakk>
    <int> a
    <int> b
    <char-*> c
    <struct-gakk> * d))
(set! eval-c-typelist '())
(begin eval-c-typelist)
(eval-c-getstruct '<struct-gakk>)
(set! eval-c-structlist '())
(begin eval-c-structlist)
(eval-c-puttype 'a '<struct-gakk>)
(eval-c-gettype 'a.d.d.d.a)
!#

#!
(eval-c ""
	(define-class <Gakk>
	  (<int> ai 5)
	  ;;(virtual <int> (define (gakk2)
	  ;;		   (return (+ this->ai 2))))

	  (<int> (define (gakk)
		   (return (-> this ai))))

	  (public
	   (<int> (define (c-scale (<float> x)
				   (<float> x1)
				   (<float> x2)
				   (<float> y1)
				   (<float> y2))
		    (return (+ (-> this gakk)
			       ;;(this->gakk2)
			       y1
			       (/ (* (- x x1) (- y2 y1))
				  (- x2 x1)))))))

	  (define (destructor)
	    (c-display "killed me"))
	  
	  (define (constructor (<int> a) (<int> b))
	    (set! this->ai (+ (-> this ai) a b)))))
	
->
!#

(define-c-macro (define-class name . rest)
  (let* ((privatevars '())
	 (publics '())
	 (publicvars '())
	 (privatefuncs '())
	 (publicfuncs '())
	 (destructors '())
	 (constructors '())
	 (cname (eval-c-etype->ctype name))
	 (structname (<-> cname "_class"))
	 (cclassname (<-> "class_" cname))
	 (classname (c-tosymbol (<-> "<" cclassname ">"))))
    
    (for-each (lambda (term)
		(cond ((eq? 'public (car term)) (set! publics (append (cdr term) publics)))
		      ((eq? 'define (car term)) (if (eq? 'destructor (caadr term))
						    (set! destructors (cons (cons (cdr (cadr term)) (cddr term)) destructors))
						    (set! constructors (cons (cons (cdr (cadr term)) (cddr term)) constructors))))
		      ((list? (cadr term)) (set! privatefuncs (cons term privatefuncs)))
		      (else (set! privatevars (cons term privatevars)))))
	      rest)
    (for-each (lambda (term)
		(if (list? (cadr term))
		    (set! publicfuncs (cons term publicfuncs))
		    (set! publicvars (cons term publicvars))))
	      publics)

    (set! privatevars (reverse! privatevars))
    (set! publicvars (reverse! publicvars))
    (set! privatefuncs (reverse! privatefuncs))
    (set! publicfuncs (reverse! publicfuncs))
    (set! destructors (reverse! destructors))
    (set! constructors(reverse! constructors))
    
    (let ((func-org-func (lambda (funcs publicorprivate)
			   (map (lambda (func)
				  (let* ((rettype (car func))
					 (rest (cadr func))
					 (def (cadr rest))
					 (name (car def))
					 (args (cdr def))
					 (body (cddr rest)))
				    (list rettype
					  (eval-c-get-cmethodname name cclassname publicorprivate)
					  (cons (list classname 'this) args)
					  body)))
				funcs))))

      (set! privatefuncs (func-org-func privatefuncs "private"))
      (set! publicfuncs (func-org-func publicfuncs "public"))

      (set! publicfuncs (cons `(,classname ,(eval-c-get-cmethodname 'constructor-internal cclassname "public") ()
					   ((let* ((this ,classname
							 (calloc 1 (sizeof ,classname))))
					      ,@(map (lambda (var)
						       `(set! ,(symbol-append 'this-> (cadr var)) ,(caddr var)))
						     (remove (lambda (var) (= (length var) 2))
							     (append privatevars publicvars)))
					      (return this))))
			      publicfuncs))

      (set! publicfuncs (cons `(<void> ,(eval-c-get-cmethodname 'destructor-internal cclassname "public") ((,classname this))
				       ((free this)))
			      publicfuncs))

      (if (null? destructors)
	  (set! destructors '((()))))
      (if (null? constructors)
	  (set! constructors '((() (return this)))))

      (c-display "privatevars" privatevars "\n"
		 "publicvars" publicvars "\n"
		 "privatefuncs" privatefuncs "\n"
		 "publicfuncs" publicfuncs "\n"
		 "destructors" destructors "\n"
		 "constructors" constructors "\n")

    
      (<-> (eval-c-structure classname
			     structname
			     (apply append (map (lambda (var) (list (car var) (cadr var))) (append privatevars publicvars))))
	   
	   ";\n"
	   "typedef struct " structname "* " cclassname ";\n"

	   (let ((methods (append
			   (map (lambda (func)
				  (eval-c-parse `(,(car func) ,(cadr func) (lambda ,(caddr func) ,@(cadddr func)))))
				(append privatefuncs publicfuncs))
			   (map (lambda (constructor)
				  (eval-c-parse `(,classname ,(eval-c-get-cmethodname 'constructor cclassname "public")
							     (lambda ,(car constructor)
							       (let* ((this ,classname
									    (,(eval-c-get-cmethodname 'constructor-internal cclassname "public"))))
								 ,@(cdr constructor))))))
				constructors)
			   (map (lambda (destructor)
				  (eval-c-parse `(<void> ,(eval-c-get-cmethodname 'destructor cclassname "public")
							 (lambda ,(cons  (list classname 'this) (car destructor))
							   ,@(cdr destructor)
							   (,(eval-c-get-cmethodname 'destructor-internal cclassname "public") this)))))
				destructors))))
	     (apply <-> (append (map (lambda (method)
				       (<-> (car (string-split method #\{)) ";\n"))
				     methods)
				methods))
	     )
	   )
      )
  
    )
  )


(define-c-macro (new classname . args)
  `(,(eval-c-get-cmethodname 'constructor (eval-c-etype->ctype classname) "public") ,@args))

(define-c-macro (-> object methodname . args)
  `(,(eval-c-get-cmethodname methodname (eval-c-etype->ctype (eval-c-gettype object)) "public") ,object ,@args))

#!
(eval-c ""
	(<int> ais 78)
	(define-class <Bank>
	  (<int> sum (+ 2 3))
	  (<int> sum2)
	  (<int> sum3 (+ this->sum 1500))
	  (public
	   (<int> (define (deposit (<int> das_sum))
		    (<int> ai2 5)
		    (set! this->sum (+ this->sum3 das_sum 13451345))
		    (return (+ 2 this->sum))))))
	(run-now
	 (let* ((bank <class_Bank> (new <class_Bank>)))
	   (printf (string "aisann: %d %d\\n") ais (-> bank deposit 17)))))
	   
    
(c-dynamic-call "das_init" "/tmp/file6JoOzy.c.so")

(eval-c-macro-result
 '(define-class <Gakk>
    (<int> ai 5)
    (<int> (define (amethod (<int> a))
	     (return (-> this ai))))
    
    (public
     (<int> ai2)
     (<int> (define (c-scale (<float> x)
			     (<float> x1)
				   (<float> x2)
				   (<float> y1)
				   (<float> y2))
		    (return (+ (-> this gakk)
			       ;;(this->gakk2)
			       y1
			       (/ (* (- x x1) (- y2 y1))
				  (- x2 x1)))))))
    
    (define (destructor)
      (c-display "killed me"))
    
    (define (constructor (<int> a) (<int> b))
      (set! this->ai (+ (-> this ai) a b))
      (return this))))


(eval-c ""
	(define-struct <Gakk_class>
	  <int> ai
	  ;;(<int> gakk())
	  )
	(typedef <struct-Gakk_class*> <Gakk_class>)
	(typedef <struct-Gakk_class*> <class_Gakk>)

	;;(<int> gakk_class_gakk2 (lambda ((<struct-gakk_class> this))
	;;			  (return (+ this->ai 2))))
	(<int> gakk_class_gakk_private (lambda ((<Gakk_class> this))
					 (return (-> this ai))))
	(<int> gakk_class_c-scale_public_float_split_float_split_float_split_float_splitfloat (lambda ((<Gakk_class> this)
												       (<float> x)
												       (<float> x1)
												       (<float> x2)
												       (<float> y1)
												       (<float> y2))
												(return (+ (-> this gakk)
													   ;;(this->gakk2)
													   y1
													   (/ (* (- x x1) (- y2 y1))
													      (- x2 x1))))))
	(<void> delete-Gakk (lambda (<Gakk_class> this)
			      (c-display "killed me")
			      (free this)))

	(<Gakk_class> new-Gakk-int_split_int (lambda ((<int> a)
						      (<int> b))
					       (let* ((this <Gakk_class> (calloc 1 (sizeof <struct-Gakk_class>))))
						 (set! this->ai 5)
						 (set! this->ai (+ this->ai a b))
						 (return this)))))


---

(let* (gakk <class_Gakk> 
	    (new 5 6))
  (-> gakk c-scale 0 1 2 3))

->
(let* (gakk <class_Gakk>
	    (new 5 6))
  (gakk->c-scale gakk 0 1 2 3))	    

!#


#!

This definition of lambda doesn't work because I want the following to work:

  ("func" a b c)  -> func(a,b,c)

But, perhaps that last one shouldn't be allowed to work.

(define-c-macro (lambda def . body)
  `( ,(if (null? def) 
	  "()"
	  (apply <-> (append (list "(")
			     (map (lambda (x) (<-> (eval-c-parse x) ", ")) (c-butlast def))
			     (list (<-> (eval-c-parse (last def))))
			     (list ")"))))
     (begin ,@body)))
!#
      


;; Workaround:
(define-c-macro (lambda def . body)
  (eval-c-uplevel)
  (let ((ret (<-> (if (null? def) 
		      "()"
		      (apply <-> (append (list "(")
					 (map (lambda (x) (<-> (eval-c-parse x) ", ")) (c-butlast def))
					 (list (<-> (eval-c-parse (last def))))
					 (list ")"))))
		  "{" (string #\newline)
		  (eval-c-parse-lines body)
		  "}" (string #\newline))))
    (eval-c-downlevel)
    ret))



(define-c-macro (define def . body)
  (if (list? def)
      (<-> (eval-c-parse (eval-c-cify-var (car def)))
	   (eval-c-parse (append (list 'lambda (cdr def))
				 body)))
      `(,(car body) ,def ,(cadr body))))


(define eval-c-get-run-now-procname
  (let ((num 0))
    (lambda ()
      (set! num (1+ num))
      (<-> "run_now_" (number->string num)))))

(define-c-macro (run-now . body)
  (let ((procname (eval-c-get-run-now-procname)))
    (set! eval-c-run-nows (cons procname eval-c-run-nows))
    `(<void> ,procname (lambda ()
				,@body))))


(define* (eval-c-gen-public-func term #:optional publicname)
  (let* ((funcname_org (cadr term))
	 (funcname (eval-c-cify-var funcname_org))
	 (helperfuncname (<-> (symbol->string funcname) "_eval_c_helper"))
	 (parameters (cadr (caddr term)))
	 (rettype (car term)))
    `((<SCM> ,helperfuncname (lambda ,(map (lambda (var) (list '<SCM> (cadr var)))
						  parameters)
				      (,(let ((ret-scm (eval-c-to-scm (string-trim-right (eval-c-get-propertype rettype)))))
					  (cond ((string=? "UNSPECIFIED" ret-scm) 'RETURN_UNSPECIFIED)
						((string=? "POINTER" ret-scm) 'RETURN_POINTER)
						((string=? "STRING" ret-scm) 'RETURN_STRING)
						(else
						 (<-> "return MAKE_" ret-scm))))
				       (,funcname ,@(map (lambda (var)
							   (list (string->symbol (<-> "GET_" 
										      (eval-c-to-scm
										       (string-trim-right
											(eval-c-get-propertype
											 (car var))))))
								 (cadr var)))
							 parameters)))))
      (run-now
       (scm_c_define_gsubr (string ,(if publicname publicname funcname_org)) ,(length parameters) 0 0 ,helperfuncname)))))

(define-c-macro (public . body)
  (let* ((newbody '()))
    (for-each (lambda (term)
		(if (and (eval-c-symbol-is-type (car term))
			 (= 3 (length term))
			 (list? (caddr term))
			 (eq? 'lambda (caaddr term)))
		    (set! newbody (append newbody (list term) (eval-c-gen-public-func term)))
		    (set! newbody (append newbody (list term)))))
	      body)
    (eval-c-parse-lines newbody)))



(define eval-c-get-unique-name
  (let ((num 0))
    (lambda ()
      (set! num (1+ num))
      (<-> "unique_name_" (number->string num)))))

(define (eval-c-proto->public funcdef)
  ;;(c-display "funcdef: -" funcdef "-")
  (let* ((temp (map string-trim-both (string-split funcdef #\()))
	 (retname (string-split (car temp) #\space))
	 (rettype (string-trim-both (apply <-> (map (lambda (x) (<-> x " ")) (c-butlast retname)))))
	 (name (last retname))
	 (args (let* ((temp1 (string-trim-right funcdef))
		      (temp2 (string-trim-both (substring temp1
							  (1+ (string-index temp1 #\())
							  (1- (string-length temp1))))))
		 ;;(c-display "temp2: -" temp2 "-")
		 (if (or (= (string-length temp2) 0)
			 (string=? temp2 "void"))
		     '()
		     (map (lambda (x)
			    (if (string-index x #\()
				(list "void*" (eval-c-get-unique-name))
				(let ((dassplit (map string-trim-both (string-split (string-trim-both x) #\space))))
				  ;;(c-display "dassplit: " dassplit (length dassplit))
				  (if (= 1 (length dassplit))
				      (list (car dassplit) (eval-c-get-unique-name))
				      (list (string-trim-both (apply <-> (map (lambda (x)
										(<-> x " "))
									      (c-butlast dassplit))))
					    (string-trim-both (last dassplit)))))))
			  (string-split temp2 #\,)))))
	 )
    ;;(c-display "args: " args)

    (while (char=? #\* (car (string->list name)))
	   (begin
	     (set! name (list->string (cdr (string->list name))))
	     (set! rettype (<-> rettype "*"))))

    (eval-c-gen-public-func
     `( ,(string->symbol rettype) ,(string->symbol name) (lambda ,(map (lambda (x)
									 (let* ((type (car x))
										(name (cadr x)))
									   ;;(c-display "name: -" name "-")
									   (if (char=? #\* (car (string->list name)))
									       (begin
										 (set! name (list->string (cdr (string->list name))))
										 (if (string=? "" name)
										     (set! name (eval-c-get-unique-name)))
										 (set! type (<-> type "*"))))
									   (list (string->symbol type)
										 (string->symbol name))))
								       args))))))
#!
(eval-c-proto->public "const char *jack_port_name (const jack_port_t *port)")
(eval-c-proto->public "const char **jack_port_get_connections (const jack_port_t *port)")

(eval-c-proto->public "void *jack_port_get_buffer (jack_port_t *, jack_nframes_t)")
(eval-c-proto->public "void jack_on_shutdown (jack_client_t *client, void (*function)(void *arg), void *arg)")
(eval-c-proto->public "void jack_on_shutdown (jack_client_t *client, void* arg)")
!#

(define-c-macro (proto->public . body)
  (eval-c-parse-lines (apply append! (map eval-c-proto->public
					  (apply append! (map (lambda (lines) (remove! (lambda (line) (string=? "" (string-trim-both line)))
										       lines))
							      (map (lambda (bod) (string-split bod #\;))
								   body)))))))


#!
(eval-c ""
	"#include <jack/jack.h>"
	(proto->public
	 "int getchar(void);
int fgetc (FILE
 *stream)"
	 "int getc(FILE *stream)"
	 "void jack_on_shutdown (jack_client_t *client, void (*function)(void *arg), void *arg)"
	 "void *jack_port_get_buffer (jack_port_t *, jack_nframes_t)"
	 "const char *jack_port_type (const jack_port_t *port)"
	 ))

(string-split "int ai();" #\;)
!#


(define-c-macro (variables->public . terms)
  (apply <-> (map (lambda (term)
		    (let ((type (car term))
			  (vars (cdr term)))
		      (apply <-> (map (lambda (var)
					(let* ((funcname (string->symbol (eval-c-get-unique-name)))
					       (funcdef `(,type ,funcname (lambda () (return ,var)))))
					  (<-> (eval-c-parse funcdef)
					       (eval-c-parse-lines (eval-c-gen-public-func funcdef var)))))
				      vars))))
		  terms)))

(define-c-macro (functions->public . terms)
  (apply <-> (map (lambda (term)
		    (<-> (eval-c-parse term)
			 (eval-c-parse `(variables->public (<void*> ,(cadr term))))))
		  terms)))

#!  

(eval-c-parse
 '(variables->public
   (<int> JackPortIsInput
	  JackPortIsOutput 
	  JackPortIsPhysical
	  JackPortCanMonitor
	  JackPortIsTerminal)
   (<char*> JACK_DEFAULT_TYPE)))
!#


(define-c-macro (set! name val)
  (<-> (eval-c-parse name) " = " (eval-c-parse val)))

(define-c-macro (or . tests)
  `(|| ,@tests))

(define-c-macro (and . tests)
  `(&& ,@tests))

(define-c-macro (quote val)
  (<-> (string #\') (string-trim-right (eval-c-parse val)) (string #\')))

(define-c-macro (let* defs . body)
  `(begin
     ,@(map (lambda (def)
	      (list (cadr def) (car def) (caddr def)))
	    defs)
     ,@body))

(define-c-macro (not atest)
  `("!" ,atest))

(define-c-macro (1+ something)
  `(+ 1 ,something))

(define-c-macro (1- something)
  `(- ,something 1))

(define-c-macro (sizeof type)
  `("sizeof" ,(eval-c-get-propertype type)))

(define-c-macro (while test . body)
  `(<while> "(" ,test ")" ,@body))

(define-c-macro (string astring)
  (<-> "\"" (if (string? astring)
		astring
		(string-trim-right (eval-c-parse astring)))
       "\""))

(define-c-macro (for init testing lastaction .  body)
  `(begin ,init
	  (while 1 (begin
		     (if (not ,testing) break)
		     ,@body
		     ,lastaction))))


(define-c-macro (for-each startval . rest)
  (let* ((restlen (length rest))
	 (itername (caadr (last rest)))
	 (body (cddr (last rest)))
	 (endval #f)
	 (testfunc #f)
	 (addval #f))
    (cond ((= 2 restlen)
	   ;;(for-each 0 n
	   (set! endval (car rest))
	   (set! testfunc (if (and (number? endval)
				   (number? startval))
			      (if (> startval endval)
				  '>=
				  '<)
			      '<))
	   (set! addval (if (eq? testfunc '<)
			    1
			    -1)))

	  ((= 4 restlen)
	   ;;(for-each 0 < n 1
	   (set! testfunc (car rest))
	   (set! endval (cadr rest))
	   (set! addval (caddr rest)))
	  
	  (else
	   (if (member (car rest) '(< > <= >= == != <> & | ~))
	       (begin
		 ;;(for-each 0 < n
		 (set! testfunc (car rest))
		 (set! endval (cadr rest))
		 (set! addval (if (member testfunc '(> =>))
				  -1
				  1)))
	       (begin
		 ;;(for-each 0 n 1
		 (set! endval (car rest))
		 (set! testfunc (if (and (number? endval)
					 (number? startval))
				    (if (> startval endval)
					'>=
					'<)
				    '<))
		 (set! addval (cadr rest))))))
    `(for (<int> ,itername ,startval)
	  (,testfunc ,itername ,endval)
	  (set! ,itername (+ ,itername ,addval))
	  ,@body)))
    
	      

  

#!

(eval-c ""
	(run-now
	 (for-each 10 != 5 1 
		   (lambda (i)
		     (printf (string "%d\\n") i)))))


(eval-c-parse '(if (not (== a 2)) b))

(eval-c-parse '(for-each 0 < 5 1
			 (lambda (i)
			   (set! out[i] 0.0f))))

(eval-c-parse '(for (set! i 0) (< i 5) i++
		    (printf i)))

(eval-c-parse '(if (not (a)) b))

(eval-c-parse '(while (a) (begin (ai))))
(eval-c-parse '(<static-int> a 5))
(eval-c-parse '(define a <static-int> 5))
(eval-c-parse '(<int> (define (gakk (<int> a)) (ai))))
(eval-c-parse '(define (gakk (<int> a))))

(eval-c-parse (list 'lambda (list (list '<int> 'a))))

(define-c-macro (printf . rt)
  `("printf" 4 5 ,@rt))
(macroexpand (cons (symbol-append eval-c-macro-prefix 'printf) (list 2)))

(eval-c-parse '(<int> gakk (lambda ()
			     au
			     (au2))))

(eval-c-parse '(<static-int> gakk 452 3 "314" (printf "asdfasdf")))

(eval-c-parse '(if (not (== a b))
		   (begin
		     (printf (+ 2 5 2))
		     (ai 5))
		   (printf 3)))

(eval-c-parse '(cond (test1 todo1 todo1.2 (todo1.3))
		     (test2 todo2)
		     (else todo3)))

(eval-c-parse '(if (== 2 test1)
		   (begin
		     (todo1)
		     (todo1.2)
		     (todo1.3))
		   (if (== 2 test2)
		       (begin
			 (todo2))
		       (begin
			 (todo3)
			 (todo3.2)))))

(eval-c-parse '(<void> parse_arguments 
		       (lambda ((<int> argc) (<char**> argv))
			 (if (< argc 2)
			     (begin
			       (fprintf stderr (string "usage: %s y|n\n") package)
			       (exit 9))))))

(eval-c-parse '(<void> (define (parse_arguments (<int> argc)
						(<char**> argv))
			 (if (< argc 2)
			     (begin
			       (fprintf stderr (string "usage: %s y|n\n") package)
			       (exit 9)))
			 (if (or (== argv[1][0] 'y)
				 (== argv[1][0] 'Y)
				 (== argv[1][0] '1))
			     (set! onoff 1)
			     (set! onoff 0)))))

(eval-c-parse '(<int> process (lambda ((<jack_nframes_t> nframes)
				       (<void*> arg))
				(let* ((out <jack_default_audio_sample_t*>
					    (jack_port_get_buffer output_port nframes))
				       (in <jack_default_audio_sample_t*>
					   (jack_port_get_buffer input_port nframes)))
				  (memcpy out in
					  (* (sizeof <jack_default_audio_sample_t>) nframes)))
				
				(return 0))))


!#



(define* (eval-c-eval #:key (compile-options "") . codestrings)
  (let* ((evalstring "")
	(sourcefile (<-> (tmpnam) ".c"))
	(libfile (<-> sourcefile ".so"))
	(fd (open-file sourcefile "w"))
	(guile-config (<-> (cdr (assoc 'bindir %guile-build-info)) "/guile-config")))
    (if (not (access? guile-config X_OK))
	(begin
	  (c-display "Error. " guile-config " not found, or is not an executable.")
	  (c-display "        Perhaps you need the guile-devel pacage?")
	  (newline)
	  (exit)))
    (for-each (lambda (s)
		(write-line s fd))
	      (if (eq? (car codestrings) '#:compile-options)
		  (cddr codestrings)
		  codestrings))
    (close fd)
    (if eval-c-verbose
	(c-display sourcefile))
    (system (<-> "gcc -Wall -O2 -shared -o " libfile " " sourcefile " "
			   (if (getenv "CFLAGS") (getenv "CFLAGS") "") " " (if (getenv "LDFLAGS") (getenv "LDFLAGS") "") " "
			   (string #\`) guile-config " compile" (string #\`) " "
			   compile-options))
    (dynamic-call "das_init" (dynamic-link libfile))
    (if eval-c-cleanup
	(system (<-> "rm " libfile " " sourcefile)))
    ))


#!
(eval-c ""
	"#include <dlfcn.h>"
	"#include <unistd.h>"

	"typedef void (*functype)()"

	(public
	 (<void> c-dynamic-call (lambda ((<char-*> funcname)
					 (<char-*> filename))
				  (let* ((handle <void-*> (dlopen filename RTLD_NOW)))
				    (if (== handle NULL)
					(printf (string "Handle null\\n"))
					(let* ((func <functype> (dlsym handle funcname)))
					  (if (!= func NULL)
					      (func)))))))))
!#

(define (eval-c-parse-file terms)
  (set! eval-c-run-nows '())
  (set! eval-c-typelistlist '())
  (set! eval-c-typelist '())
  (set! eval-c-structlist '())
  (set! eval-c-classlist '())
  (set! eval-c-level 0)
  `("#include <stdio.h>"
    "#include <libguile.h>"
    "#include <string.h>"
    "#include <stdlib.h>"
    "#define MAKE_STRING(a) scm_mem2string(a,strlen(a))"
    "#define RETURN_STRING(a) {char *ret=(a); return ret?MAKE_STRING(ret):SCM_BOOL_F;}"
    "#define GET_STRING(a) (scm_is_false(a)?NULL:(char*)SCM_STRING_CHARS(a))"
    "#define GET_POINTER3(a) (scm_is_false(a)?NULL:(void *)scm_num2ulong(a,0,\"GET_POINTER3()\"))"
    "#define GET_POINTER(a) (scm_is_false(a)?NULL:GET_POINTER3(SCM_CAR(SCM_CDR(a))))"
    "#define GET_POINTER2(a) GET_POINTER(a)"
    "#define MAKE_POINTER(a) scm_cons(MAKE_STRING(\"A_POINTER\"),scm_cons(scm_ulong2num((unsigned long)a),SCM_EOL))"
    "#define RETURN_POINTER(a) {unsigned long ret=(unsigned long)(a); return ret?MAKE_POINTER(ret):SCM_BOOL_F;}"
    "#define RETURN_UNSPECIFIED(a) {(a);return SCM_UNSPECIFIED;}"
    ,(<-> "#define GET_INTEGER(a) SCM_INUM(scm_inexact_to_exact(" (if (c-atleast1.7?) "scm_floor(a)" "a") "))")
    "#define MAKE_INTEGER SCM_MAKINUM"
    "#define GET_DOUBLE(a) scm_num2dbl(a,\"GET_DOUBLE\")"
    "#define MAKE_DOUBLE(a) scm_make_real((double)a)"
    
    ,@(map eval-c-parse-line terms)
    
    "void das_init(){"
    ,@(map (lambda (x)
	     (<-> x "();"))
	   (reverse eval-c-run-nows))
    "}"))


(define-macro (eval-c compile-options . terms)
  `(eval-c-eval #:compile-options ,compile-options
		,@(eval-c-parse-file terms)))


;(define-macro (define-c ret-type def . body)
;  `(eval-c ""
;	   (public (,ret-type ,(car def) (lambda ,(cdr def)
;					   ,@body)))))

(define-macro (define-c ret-type body)
  `(eval-c ""
	   (public (,ret-type ,(car body) (lambda ,(cadr body)
					    ,@(cddr body))))))

(eval-c ""
	(public
	 (<int> ec-get-ints-element (lambda ((<int*> array) (<int> n))
				      (return array[n])))
	 (<double> ec-get-doubles-element (lambda ((<double*> array) (<int> n))
					(return array[n])))
	 (<float> ec-get-floats-element (lambda ((<float*> array) (<int> n))
					  (return array[n])))
	 (<char*> ec-get-strings-element (lambda ((<char**> array) (<int> n))
					   (return array[n])))
	 (<void*> ec-get-pointers-element (lambda ((<void**> array) (<int> n))
					    (return array[n])))
	 
	 (<void> ec-put-ints-element (lambda ((<int*> array) (<int> val) (<int> n))
				       (set! array[n] val)))
	 (<void> ec-put-doubles-element (lambda ((<double*> array) (<double> val) (<int> n))
					  (set! array[n] val)))
	 (<void> ec-put-floats-element (lambda ((<float*> array) (<float> val) (<int> n))
					 (set! array[n] val)))
	 (<void> ec-put-strings-element (lambda ((<char**> array) (<char*> val) (<int> n))
					  (set! array[n] val)))
	 (<void> ec-put-pointers-element (lambda ((<void**> array) (<void*> val) (<int> n))
					   (set! array[n] val)))
	 
	 (<int*> ec-make-ints (lambda ((<int> n))
				(return (calloc n (sizeof <int>)))))
	 (<double*> ec-make-doubles (lambda ((<int> n))
				      (return (calloc n (sizeof <double>)))))
	 (<float*> ec-make-floats (lambda ((<int> n))
				    (return (calloc n (sizeof <float>)))))
	 (<char**> ec-make-strings (lambda ((<int> n))
				     (return (calloc n (sizeof <char*>)))))
	 (<void**> ec-make-pointers (lambda ((<int> n))
				      (return (calloc n (sizeof <void*>)))))
	 
	;; No. This is dangerous, ugly and unnecesarry. #f is the same as NULL from the guile-side.
	;;(<SCM> c_NULL (lambda ()
	;;		(return (MAKE_POINTER NULL))))
	;;(run-now
	;; (scm_c_define_gsubr (string "NULL") 0 0 0 c_NULL)
	 ))


(define (ec-pointer? something)
  (and (pair? something)
       (string? (car something))
       (string=? "A_POINTER" (car something))))

(define (ec-get-nullterminated-somethings array get-func)
  (let ((ret '()))
    (let loop ((n 0))
      (let ((res (get-func array n)))
	(if res
	    (begin
	      (set! ret (cons res ret))
	      (loop (1+ n))))))
    (reverse! ret)))

(define (ec-get-somethings array num-elements get-func)
  (if num-elements
      (let ((ret '()))
	(c-for 0 < num_elements 1
	       (lambda (n)
		 (set! ret (cons (get-func array n)
				 ret))))
	(reverse! ret))
      (ec-get-nullterminated-somethings array get-func)))
  
(define* (ec-get-ints array #:optional num)
  (array num ec-get-ints-element))
(define* (ec-get-doubles array #:optional  num)
  (ec-get-somethings array num ec-get-double-element))
(define* (ec-get-floatss array  #:optional num)
  (ec-get-somethings array num ec-get-floats-element))
(define* (ec-get-strings array  #:optional num)
  (ec-get-somethings array num ec-get-strings-element))
(define* (ec-get-pointers array  #:optional num)
  (ec-get-somethings array num ec-get-pointers-element))


;; This one will never return an integer array, unless das-make-func is set to ec-make-ints. Same
;; goes for creating a double-array; das-make-func must be set to ec-make-doubles.
(define* (ec-make-array array #:key das-make-func das-ret)
  (let* ((make-func (if das-make-func
			das-make-func
		       (cond ((string? (car array)) ec-make-strings)
			     ((ec-pointer? (car array)) ec-make-pointers)
			     (else ec-make-floats))))
	 (ret (if das-ret
		  das-ret
		  (make-func (length array))))
	 (put-func (cond ((eq? ec-make-strings make-func) ec-put-strings-element)
			 ((eq? ec-make-pointers make-func) ec-put-pointers-element)
			 ((eq? ec-make-floats make-func) ec-put-floats-element)
			 ((eq? ec-make-doubles make-func) ec-put-doubles-element)
			 ((eq? ec-make-ints make-func) ec-put-ints-element)
			 (else
			  (c-display "Illegal das-make-func argument for ec-make-array: " das-make-func)))))

    (c-for-each (lambda (n val)
		  (put-func ret val n))
		array)
    ret))

;(define (ec-make-struct something)
;  ())


#!


(eval-c ""
	(run-now
	 (for-each 0 5
		   (lambda (i)
		     (printf (string "%d\\n") i)))))

(eval-c ""
	"#include <lrdf.h>"
	(proto->public
	 "int lrdf_export_by_source(const char *src, const char *file);"
	 "lrdf_uris *lrdf_match_multi(lrdf_statement *patterns);"
	 "lrdf_statement *lrdf_matches(lrdf_statement *pattern);"))

 
(define-c <float> (c-scale ((<float> x)
			    (<float> x1)
			    (<float> x2)
			    (<float> y1)
			    (<float> y2))
			   (return (+ y1
				      (/ (* (- x x1)
					    (- y2 y1))
					 (- x2 x1))))))

(define-c <int> (fib ((<int> n))
		     (if (< n 2)
			 (return n)
			 (return (+ (fib (- n 1))
				    (fib (- n 2)))))))


(eval-c ""
	(public (<int> f-ib (lambda ((<int> n))
			       (if (< n 2)
				   (return n)
				   (return (+ (f-ib (- n 1))
					      (f-ib (- n 2)))))))))

	 

(eval-c-to-scm "  static   volatile    int  ")
(eval-c ""
	(public
	 (<int> jepp (lambda ((<int> a)(<int> b))
			      (printf (string "jepp\\n"))
			      (return (+ a b))))))

(eval-c ""
	(public
	 (<int> ratt 81)
	 (<int> jepp5 (lambda ()
		       (return ratt)))))

(eval-c ""
	(run-now (printf (string "jepp1\\n"))
		 (printf (string "jepp2\\n")))

	(run-now (printf (string "jepp3\\n"))
		 (printf (string "jepp4\\n"))))

	 
(eval-c ""
	(run-now
	 (printf (string "hello world!\\n"))))

(eval-c ""
	(<int> jepp (lambda ()
		      (printf (string "jepp\\n"))
		      (return 0))))

(begin "jepp\\n")

(eval-c "-ljack"
	
	"#include <jack/jack.h>"

	(public

	 (<int> a 5)
	 (<int> b 6)

	 (<void> parse_arguments 
		 (lambda ((<int> argc) (<char**> argv))
		   (if (< argc 2)
		       (begin
			 (exit 9)))))
	 
	 (<jack_port_t*> input_port)
	 (<jack_port_t*> output_port)
	 
	 
	 (<int> process (lambda ((<jack_nframes_t> nframes)
				 (<void*> arg))
			  (let* ((out <jack_default_audio_sample_t*> (jack_port_get_buffer output_port nframes))
				 (in <jack_default_audio_sample_t*> (jack_port_get_buffer input_port nframes)))
			    (memcpy out in
				    (* (sizeof <jack_default_audio_sample_t>) nframes)))
			  (return 0)))
	 )
	)


!#


