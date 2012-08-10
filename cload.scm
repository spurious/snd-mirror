(provide 'cload.scm)

;;; to place the new function in the caller's current environment, we need to pass it in explicitly:
(define-macro (define-c-function . args) 
  `(define-c-function-1 (current-environment) ,@args))


(define* (define-c-function-1 cur-env return-type name arg-types (prefix "g") (headers ()) cflags ldflags)
  ;; write a C shared library module that links in "name" (or eventually a list thereof)
  ;; the new functions are placed in cur-env
  ;;     (define-c-function 'double 'j0 '(double) "m" "math.h")


  (define handlers (list '(integer s7_is_integer s7_integer s7_make_integer s7_Int)
			 '(boolean s7_is_boolean s7_boolean s7_make_boolean bool)
			 '(real s7_is_real s7_real s7_make_real s7_Double)
			 '(string s7_is_string s7_string s7_make_string char*)
			 (list 'character 's7_is_character 's7_character 's7_make_character (symbol "unsigned char"))
			 ))

  (define (find-handler handle choice)
    (let ((found (assq handle handlers)))
      (if (pair? found)
	  (choice found)
	  #t)))
  

  (define (C-type->s7-type type)

    (define (substring? subs s)
      (let ((start 0)
	    (ls (length s))
	    (lu (length subs)))
	(let ((limit (- ls lu)))
	  (let loop ((i start))
	    (if (> i limit)
		#f
		(if (do ((j i (+ j 1))
			 (k 0 (+ k 1)))
			((or (= k lu)
			     (not (char-ci=? (subs k) (s j))))
			 (= k lu)))
		    i
		    (loop (+ i 1))))))))

    (if (pair? type)                                ; '(gpointer c_pointer), (XtPointer|void* -> c_pointer) ?
	(symbol->string (cadr type))
	(let ((type-name (symbol->string type)))
	  (cond ((substring? "char*" type-name) 
		 'string)

		((substring? "char" type-name)
		 'character)

		((substring? "bool" type-name) 
		 'boolean)

		((or (substring? "float" type-name) 
		     (substring? "double" type-name)) 
		 'real)

		((or (substring? "int" type-name) 
		     (substring? "long" type-name) ; assuming not "long double" here so we need to look for it first (above)
		     (substring? "short" type-name) 
		     (substring? "byte" type-name)) 
		 'integer)

		(#t #t)))))

  (define (C->s7-cast type)
    (find-handler (C-type->s7-type type) (lambda (p) (car (cddddr p)))))
    
  (define (C->s7 type)
    (find-handler (C-type->s7-type type) cadddr))
    
  (define (s7->C type)
    (find-handler (C-type->s7-type type) caddr))

  (define (checker type)
    (find-handler (C-type->s7-type type) cadr))


  (let* ((func-name (symbol->string name))
	 (num-args (length arg-types))
	 (base-name (string-append prefix "_" func-name))
	 (scheme-name (string-append prefix ":" func-name))
	 (c-name (string-append base-name ".c"))
	 (o-name (string-append base-name ".o"))
	 (so-name (string-append base-name ".so")))
    (call-with-output-file
	c-name
      (lambda (p)

	;; C header stuff
	(format p "#include <stdlib.h>~%")
	(format p "#include <stdio.h>~%")
	(format p "#include <string.h>~%")
	(if (string? headers)
	    (format p "#include <~A>~%" headers)
	    (for-each
	     (lambda (header)
	       (format p "#include <~A>~%" header))
	     headers))
	(format p "#include \"s7.h\"~%~%")

	;; our C->scheme function
	(format p "static s7_pointer ~A(s7_scheme *sc, s7_pointer args)~%" base-name)
	(format p "{~%")
	
	;; get the Scheme args, check their types, assign to local C variables
	(if (positive? num-args)
	    (begin
	      (format p "  s7_pointer arg;~%")
	      (do ((i 0 (+ i 1))
		   (type arg-types (cdr type)))
		  ((= i num-args))
		(format p "  ~A ~A_~D;~%" (car type) base-name i))
	      (format p "  arg = args;~%")
	      (do ((i 0 (+ i 1))
		   (type arg-types (cdr type)))
		  ((= i num-args))
		(format p "  if (~A(s7_car(arg)))~%" (checker (car type)))
		(format p "    ~A_~D = (~A)~A(s7_car(arg));~%"
			base-name i
			(car type)
			(s7->C (car type)))
		(format p "  else return(s7_wrong_type_arg_error(sc, ~S, ~D, s7_car(arg), ~S));~%"
			func-name 
			(if (= num-args 1) 0 (+ i 1))
			(symbol->string (C-type->s7-type (car type))))
		(if (< i (- num-args 1))
		    (format p "  arg = s7_cdr(arg);~%")))))
	
	;; return C value to Scheme
	(let ((return-translator (C->s7 return-type)))
	  (if (not (eq? return-translator #t))
	      (format p "  return("))
	  (if (symbol? return-translator)
	      (format p "~A(sc, (~A)" return-translator (C->s7-cast return-type)))
	  (format p "~A(" func-name)
	  (do ((i 0 (+ i 0)))
	      ((= i (- num-args 1)))
	    (format p "~A_~D, " base-name i))
	  (if (positive? num-args)
	      (format p "~A_~D)" base-name (- num-args 1)))
	  (if (symbol? return-translator)
	      (format p ")"))
	  (if (not (eq? return-translator #t))
	      (format p ");~%")
	      (format p "return(s7_unspecified(sc));~%"))
	  (format p "}~%~%"))
	
	;; now the init function
	;;   the new function is placed in the current (not necessarily global) environment

	(format p "void init_~A(s7_scheme *sc);~%" base-name)
	(format p "void init_~A(s7_scheme *sc)~%" base-name)
	(format p "{~%")
	(format p "  s7_pointer cur_env;~%")
	(format p "  cur_env = s7_outer_environment(s7_current_environment(sc));~%") ; this must exist because we pass load the env ourselves
	(format p "  s7_define(sc, cur_env,~%")
	(format p "            s7_make_symbol(sc, ~S),~%" scheme-name)
	(format p "            s7_make_function(sc, ~S, ~A, ~D, 0, false, \"lib~A ~A\"));~%"
		scheme-name
		base-name
		num-args
		prefix
		func-name)
	(format p "}~%")))

    ;; now we have name.c -- make it into a shared object, load it, delete the temp files

    ;; TODO: use cflags/ldflags
    ;; TODO: check the system stuff below, maybe use better temp file names
    ;; TODO: expand the types so gtk/xm might be done this way (via autoload)
    ;;    structs -> environments, double*->vectors? va-args? c-null? 
    ;;    c-complex->complex 
    ;; TODO: add a way to collect many functions in one module, loading an entire library at once
    ;; (define-c-functions ...)
    ;; TODO: add cload.scm to the Snd tarballs/doc/test
    ;; SOMEDAY: take a set of these functions (possibly in the current program via a C header like s7 and NULL as lib name)
    ;;   and run the torture tester on the current program (going below the scheme level in a sense)
    ;; PERHAPS: xgdata->a long (define-c-functions ...) call and see if xg.c can be dispensed with!

    ;; we also need in general the header (math.h) and the library (-lm) --
    ;;   ideally perhaps from pkg-config?

    (system (format #f "gcc -c -fPIC ~A" c-name))
    (system (format #f "gcc ~A -shared -o ~A" o-name so-name))
    (let ((new-env (augment-environment
		       cur-env
		       (cons 'init_func (string->symbol (string-append "init_" base-name))))))
      (load so-name new-env))

    ;;(delete-file c-name)
    (delete-file o-name)
    (delete-file so-name)

    name
    ))

	    
	



