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

(define-c <int> (fib ((<int> n))
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
	(<static-SCM> fib_eval_c_helper (lambda ((<SCM> n))
					  (return (MAKE_INTEGER (fib (GET_INTEGER n))))))
	(run-now
	 (scm_c_define_gsubr "fib" 1 0 0 fib_eval_c_helper)))


And after running the "lambda", "if" and "run-now" macros, eval-c will produce and run the following c-code:

int fib (int n){
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

From guile you now have a function called "fib" which takes
one argument.



Hello world looks like this:

(eval-c ""
	(run-now
	 (printf (string "Hello world!\\n"))))




First argument to eval-c is a string with compiling/linking options.
Usually just "", but can be "-lsnd" or something if needed.


!#


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

(define (eval-c-get-propertype type)
  (let ((ret '()))
    (for-each (lambda (c)
		(if (not (or (eq? #\< c)
			     (eq? #\> c)))
		    (if (eq? #\- c)
			(set! ret (cons #\space ret))
			(set! ret (cons c ret)))))
	      (string->list (symbol->string type)))
    (list->string (reverse! (cons #\space ret)))))

(define (eval-c-to-scm das-type)
  (let* ((type-split (remove (lambda (s) (or (string=? "" s))) (string-split das-type #\space)))
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
(eval-c-cify-var 'ga-kk)
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
	(let ((s (car term)))
	  (if (eval-c-symbol-is-type s)
	      (if (and (= (length term) 3)
		       (or (not (list? (caddr term)))
			   (not (eq? 'lambda (caaddr term)))))
		  (<-> (eval-c-get-propertype s)
		       (eval-c-parse (eval-c-cify-var (cadr term)))
		       " = "
		       (eval-c-parse (caddr term)))
		  (apply <-> (map eval-c-parse (cons (eval-c-get-propertype s)
						     (cons (eval-c-cify-var (cadr term))
							   (cddr term))))))
	      (let* ((a (cons (symbol-append eval-c-macro-prefix (car term)) (cdr term)))
		     (b (macroexpand-1 a )))
		(if (not (equal? a b))
		    (begin
		      (eval-c-parse b))
		    (eval-c-eval-funccall term)))))))))

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
  (apply <-> (append (list "(")
		     (map (lambda (x) (<-> (eval-c-parse x) " " (symbol->string op)" ")) (c-butlast rest))
		     (list (eval-c-parse (last rest)))
		     (list ")"))))

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
  (<-> "{" (string #\newline)
       (eval-c-parse-lines body)
       "}"))


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
  (<-> (if (null? def) 
	   "()"
	   (apply <-> (append (list "(")
			      (map (lambda (x) (<-> (eval-c-parse x) ", ")) (c-butlast def))
			      (list (<-> (eval-c-parse (last def))))
			      (list ")"))))
       "{" (string #\newline)
       (eval-c-parse-lines body)
       "}" (string #\newline)))



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
    `(<static-void> ,procname (lambda ()
				,@body))))


(define (eval-c-gen-public-func term)
  (let* ((funcname_org (cadr term))
	 (funcname (eval-c-cify-var funcname_org))
	 (helperfuncname (<-> (symbol->string funcname) "_eval_c_helper"))
	 (parameters (cadr (caddr term)))
	 (rettype (car term)))
    `((<static-SCM> ,helperfuncname (lambda ,(map (lambda (var) (list '<SCM> (cadr var)))
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
       (scm_c_define_gsubr (string ,funcname_org) ,(length parameters) 0 0 ,helperfuncname)))))

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




(define (eval-c-proto->public funcdef)
  (let* ((temp (map string-trim-both (string-split funcdef #\()))
	 (retname (string-split (car temp) #\space))
	 (rettype (string-trim-both (apply <-> (map (lambda (x) (<-> x " ")) (c-butlast retname)))))
	 (name (last retname))
	 (args (let ((temp2 (string-trim-both (car (string-split (cadr temp) #\))))))
		 (if (= (string-length temp2) 0)
		     '()
		     (map (lambda (x)
			    (let ((dassplit (map string-trim-both (string-split x #\space))))
			      (list (string-trim-both (apply <-> (map (lambda (x)
									(<-> x " "))
								      (c-butlast dassplit))))
				    (string-trim-both (last dassplit)))))
			  (string-split temp2 #\,)))))
	 )
    (if (char=? #\* (car (string->list name)))
	(begin
	  (set! name (list->string (cdr (string->list name))))
	  (set! rettype (<-> rettype "*"))))
    (eval-c-gen-public-func
     `( ,(string->symbol rettype) ,(string->symbol name) (lambda ,(map (lambda (x)
									 (let* ((type (car x))
										(name (cadr x)))
									   (if (char=? #\* (car (string->list name)))
									       (begin
										 (set! name (list->string (cdr (string->list name))))
										 (set! type (<-> type "*"))))
									   (list (string->symbol type)
										 (string->symbol name))))
								       args))))))


(define-c-macro (proto->public . body)
  (eval-c-parse-lines (apply append (map eval-c-proto->public body))))


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


(define (eval-c-parse-file terms)
  (set! eval-c-run-nows '())
  (apply <-> (map (lambda (x) (<-> x (string #\newline)))
		  (append (list "#include <stdio.h>"
				"#include <libguile.h>"
				"#include <string.h>"
				"#include <stdlib.h>"
				"#define MAKE_STRING(a) scm_mem2string(a,strlen(a))"
				"#define RETURN_STRING(a) {char *ret=(a); return ret?MAKE_STRING(ret):SCM_BOOL_F;}"
				"#define GET_STRING(a) ((char*)SCM_STRING_CHARS(a))"
				"#define GET_POINTER3(a) (void *)scm_num2ulong(a,0,\"GET_POINTER3()\")"
				"#define GET_POINTER(a) GET_POINTER3(SCM_CAR(SCM_CDR(a)))"
				"#define GET_POINTER2(a) GET_POINTER(a)"
				"#define MAKE_POINTER(a) scm_cons(MAKE_STRING(\"A_POINTER\"),scm_cons(scm_ulong2num((unsigned long)a),SCM_EOL))"
				"#define RETURN_POINTER(a) {unsigned long ret=(unsigned long)(a); return ret?MAKE_POINTER(ret):SCM_BOOL_F;}"
				"#define RETURN_UNSPECIFIED(a) {(a);return SCM_UNSPECIFIED;}"
				(<-> "#define GET_INTEGER(a) SCM_INUM(scm_inexact_to_exact(" (if (c-atleast1.7?) "scm_floor(a)" "a") "))")
				"#define MAKE_INTEGER SCM_MAKINUM"
				"#define GET_DOUBLE(a) scm_num2dbl(a,\"GET_DOUBLE\")"
				"#define MAKE_DOUBLE(a) scm_make_real((double)a)")
			  
			  (list (eval-c-parse-lines terms))

			  (cons "void das_init(){"
				(map (lambda (x)
				       (<-> x "();"))
				     (reverse eval-c-run-nows)))
			  (list "}")))))


(define-macro (eval-c compile-options . terms)
  `(eval-c-eval #:compile-options ,compile-options
		,(eval-c-parse-file terms)))


(define-macro (define-c ret-type body)
  `(eval-c ""
	   (public (,ret-type ,(car body) (lambda ,(cadr body)
					    ,@(cddr body))))))


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
	 (<static-int> jepp (lambda ((<int> a)(<int> b))
			      (printf (string "jepp\\n"))
			      (return (+ a b))))))

(eval-c ""
	(public
	 (<static-int> ratt 81)
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


