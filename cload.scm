(provide 'cload.scm)

;;; automatically link a C function into s7 (there are a bunch of examples below)
;;;     (define-c-function '(double j0 (double)) "m" "math.h")
;;; means link the name m:j0 to the math library function j0 passing a double arg and getting a double result (reals in s7)

(define define-c-function-output-file-counter 0)

;;; to place the new function(s) in the caller's current environment, we need to pass it in explicitly:
(define-macro (define-c-function . args) 
  `(define-c-function-1 (current-environment) ,@args))


(define* (define-c-function-1 cur-env function-info (prefix "g") (headers ()) (cflags "") (ldflags ""))
  ;; write a C shared library module that links in the functions in function-info
  ;;    function info is either a list: (return-type c-name arg-type) or a list thereof
  ;;    the new functions are placed in cur-env


  (define handlers (list '(integer s7_is_integer s7_integer s7_make_integer s7_Int)
			 '(boolean s7_is_boolean s7_boolean s7_make_boolean bool)
			 '(real s7_is_real s7_real s7_make_real s7_Double)

			 ;; '(complex s7_is_complex #f s7_make_complex s7_Complex)
			 ;; the typedef is around line 6116 in s7.c, but we also need s7_complex in the gmp case, I think

			 '(string s7_is_string s7_string s7_make_string char*)
			 (list 'character 's7_is_character 's7_character 's7_make_character (symbol "unsigned char"))
			 '(c_pointer s7_is_c_pointer s7_c_pointer s7_make_c_pointer void*)
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

    (if (pair? type)
	(symbol->string (cadr type))
	(let ((type-name (symbol->string type)))
	  (cond ((substring? "**" type-name)     ; any C pointer is uninterpreted
		 'c_pointer)
		
		((substring? "char*" type-name)  ; but not char** (caught above)
		 'string)

		((substring? "*" type-name)      ; float* etc
		 'c_pointer)

		((substring? "char" type-name)
		 'character)

		((substring? "bool" type-name) 
		 'boolean)
		
;		((substring? "complex")
;		 'complex)

		((or (substring? "float" type-name) 
		     (substring? "double" type-name)) 
		 'real)

		((or (substring? "int" type-name) 
		     (substring? "long" type-name) ; assuming not "long double" here so we need to look for it first (above)
		     (substring? "short" type-name) 
		     (substring? "size" type-name)
		     (substring? "byte" type-name)) 
		 'integer)

		((substring? "pointer" type-name)
		 'c_pointer)

		(#t #t)))))

  (define (C->s7-cast type)
    (find-handler (C-type->s7-type type) (lambda (p) (car (cddddr p)))))
    
  (define (C->s7 type)
    (find-handler (C-type->s7-type type) cadddr))
    
  (define (s7->C type)
    (find-handler (C-type->s7-type type) caddr))

  (define (checker type)
    (find-handler (C-type->s7-type type) cadr))

  (set! define-c-function-output-file-counter (+ define-c-function-output-file-counter 1))

  (let* ((file-name (format "temp-s7-output-~D" define-c-function-output-file-counter))
	 (c-file-name (string-append file-name ".c"))
	 (scheme-data ())
	 (p #f))

    (define (initialize-c-file)
      ;; C header stuff
      (set! p (open-output-file c-file-name))
      (format p "#include <stdlib.h>~%")
      (format p "#include <stdio.h>~%")
      (format p "#include <string.h>~%")
      (if (string? headers)
	  (format p "#include <~A>~%" headers)
	  (for-each
	   (lambda (header)
	     (format p "#include <~A>~%" header))
	   headers))
      (format p "#include \"s7.h\"~%~%"))
  

    (define (add-one-function return-type name arg-types)
      ;; (format *stderr* "add ~A ~A ~A~%" return-type name arg-types)
      (let* ((func-name (symbol->string name))
	     (num-args (length arg-types))
	     (base-name (string-append (if (> (length prefix) 0) prefix "g") "_" func-name))
	     (scheme-name (string-append prefix (if (> (length prefix) 0) ":" "") func-name)))
	
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
	  (format p "  ")
	  (if (not (eq? return-translator #t))
	      (format p "return("))
	  (if (symbol? return-translator)
	      (format p "~A(sc, (~A)" return-translator (C->s7-cast return-type)))
	  (format p "~A(" func-name)
	  (do ((i 0 (+ i 1)))
	      ((>= i (- num-args 1)))
	    (format p "~A_~D, " base-name i))
	  (if (positive? num-args)
	      (format p "~A_~D" base-name (- num-args 1)))
	  (format p ")")
	  (if (symbol? return-translator)
	      (format p ")"))
	  (if (not (eq? return-translator #t))
	      (format p ");~%")
	      (format p ";~%  return(s7_unspecified(sc));~%"))
	  (format p "}~%~%"))
	(set! scheme-data (cons (list scheme-name base-name func-name num-args) scheme-data))))

  
    (define (end-functions)
      (let ((o-file-name (string-append file-name ".o"))
	    (so-file-name (string-append file-name ".so"))
	    (base-name (string-append "_" (number->string define-c-function-output-file-counter) "_init")))

	;; now the init function
	;;   the new function is placed in the current (not necessarily global) environment
	
	(format p "void init_~A(s7_scheme *sc);~%" base-name)
	(format p "void init_~A(s7_scheme *sc)~%" base-name)
	(format p "{~%")
	(format p "  s7_pointer cur_env;~%")
	(format p "  cur_env = s7_outer_environment(s7_current_environment(sc));~%") ; this must exist because we pass load the env ourselves
	(for-each
	 (lambda (sfunc)
	   (let ((scheme-name (sfunc 0))
		 (base-name   (sfunc 1))
		 (func-name   (sfunc 2))
		 (num-args    (sfunc 3)))
	     (format p "~%  s7_define(sc, cur_env,~%")
	     (format p "            s7_make_symbol(sc, ~S),~%" scheme-name)
	     (format p "            s7_make_function(sc, ~S, ~A, ~D, 0, false, \"lib~A ~A\"));~%"
		     scheme-name
		     base-name
		     num-args
		     prefix
		     func-name)))
	 scheme-data)
	(format p "}~%")
	(close-output-port p)
  
	;; now we have name.c -- make it into a shared object, load it, delete the temp files
	
	;; TODO: expand the types so gtk/xm might be done this way (via autoload)
	;;    structs -> environments, va-args? c-null=0?
	;;    c-complex->complex
	;;    can't we handle float* (etc) as c_pointer, then have a way to decode->vector?
	;;    (vct->vector (xen_make_vct_wrapper len c_ptr)) but why no vct_length?
	;; SOMEDAY: take a set of these functions (possibly in the current program via a C header like s7 and NULL as lib name)
	;;   and run the tester on the current program (going below the scheme level in a sense)
	;; PERHAPS: xgdata->a long (define-c-functions ...) call and see if xg.c can be dispensed with!
	
	;; we also need in general the header (math.h) and the library (-lm) --
	;;   ideally perhaps from pkg-config?
	
	;; TODO: make an OSX case -- do we need a *feature* for the current OS?
	;;    there's OSTYPE="linux" or "darwin", HOST="fatty"
	
	(system (format #f "gcc -c -fPIC ~A ~A" c-file-name cflags))
	(system (format #f "gcc ~A -shared -o ~A ~A" o-file-name so-file-name ldflags))

	(let ((new-env (augment-environment
			   cur-env
			 (cons 'init_func (string->symbol (string-append "init_" base-name))))))
	  (load so-file-name new-env))

	;;(delete-file c-file-name)
	(delete-file o-file-name)
	(= (delete-file so-file-name) 0)
	))


    (initialize-c-file)

    (if (and (list? (car function-info))
	     (= (length (car function-info)) 3))
	(for-each
	 (lambda (func)
	   (apply add-one-function func))
	 function-info)
	(apply add-one-function function-info))

    (end-functions)))





	    
	
;;;  (define-c-function '(double j0 (double)) "m" "math.h")
;;;  (define-c-function '((double j0 (double)) (double j1 (double)) (double erf (double)) (double erfc (double)) (double lgamma (double))) "m" "math.h")
;;; 
;;;  (define-c-function '(char* getcwd (char* size_t)) "" "unistd.h")
;;;    here we need to pass the char* by ref? how to get the actual result back?
;;;    :(let ((str (make-string 32))) (getcwd str 32) str)
;;;    "/home/bil/cl\x00                   "
;;;    so it works in a sense -- there is a memory leak here
;;; 
;;; DIR *opendir (__const char *__name)
;;; extern int closedir (DIR *__dirp)
;;; struct dirent *readdir (DIR *__dirp) dirp->d_name is the filename if dirp not null -- we need NULL
;;;
;;; for localtime curtime we need to take an address, and maybe allocate some struct
;;; 
;;; (define-c-function '(char* getenv (char*)) "")
;;; (define-c-function '(int setenv (char* char* int)) "")

;;; (define get-environment-variable (let () (define-c-function '(char* getenv (char*)) "") getenv))
;;; environ returns a char** of all env-vars 
;;;
;;; F_OK=0, R_OK=4, W_OK=2, X_OK=1 (/usr/include/unistd.h)
;;; (define file-exists? (let ((F_OK 0)) (define-c-function '(int access (char* int)) "" "unistd.h") (lambda (arg) (= (access arg F_OK) 0))))
;;;
;;; (define delete-file (let () (define-c-function '(int unlink (char*)) "" "unistd.h") (lambda (file) (= (unlink file) 0)))) ; 0=success, -1=failure
;;;
;;; to allocate/free C memory, can we link in malloc and free? -- how to get sizeof? 
;;;
;;; this picks up a Snd function:
;;; (define-c-function '(char* version_info ()) "" "snd.h" "-I.")
;;; (define-c-function '(mus_float_t mus_degrees_to_radians (mus_float_t)) "" "snd.h" "-I.")
;;;
;;; (define-c-function '(snd_info* any_selected_sound ()) "" "snd.h" "-I.")
;;; (define-c-function '(void select_channel (snd_info* int)) "" "snd.h" "-I.")
;;;   -> (select_channel (any_selected_sound) 1)
;;; if no sound: (any_selected_sound) -> #<c_pointer (nil)>!
