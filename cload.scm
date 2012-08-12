(provide 'cload.scm)


;;; automatically link a C function into s7 (there are a bunch of examples below)
;;;     (define-c-function '(double j0 (double)) "m" "math.h")
;;; means link the name m:j0 to the math library function j0 passing a double arg and getting a double result (reals in s7)
;;;
;;; (define-c-function c-info prefix headers cflags ldflags)
;;;    prefix is some arbitrary prefix (it can be "") that you want prepended to various names.
;;;    headers is a list of headers (as strings) that the c-info relies on, (("math.h") for example).
;;;    cflags are any special C compiler flags that are needed ("-I." in particular).
;;;    ldflags is the similar case for the loader.
;;;    c-info is a list that describes the C entities that you want to tie into s7.
;;;       it can be either one list describing one entity, or a list of such lists.
;;;       Each description has the form: (return-type entity-name-in-C (argument-type...))
;;;       where each entry is a symbol, and C names are used throughout.  So, in the j0
;;;       example above, (double j0 (double)) says we want access to j0, it returns
;;;       a C double, and takes one argument, also a C double.  s7 tries to figure out 
;;;       what the corresponding s7 type is, but in tricky cases, you should tell it
;;;       by replacing the bare type name with a list: (C-type underlying-C-type).  For example,
;;;       the Snd function set_graph_style takes an (enum) argument of type graph_style_t.
;;;       This is actually an int, so we use (graph_style_t int) as the type:
;;;         (void set_graph_style ((graph_style_t int)))
;;;       If the C entity is a constant, then the descriptor list has just two entries,
;;;       the C-type and the entity name: (int F_OK) for example. The entity name can also be a list 
;;;       (an enum listing for example).
;;;       If the C type has a space ("struct tm*" for example), use (symbol "struct tm*") 
;;;       to construct the corresponding symbol.
;;;    The entity is placed in the current s7 environment under the name (string-append prefix ":" name)
;;;    where the ":" is omitted if the prefix is null.  So in the j0 example, we get in s7 the function m:j0.
;;;
;;; this function really ought to be named something like define-c-stuff
;;;
;;; more examples:
;;;
;;;  (define-c-function '((double j0 (double)) 
;;;                       (double j1 (double)) 
;;;                       (double erf (double)) 
;;;                       (double erfc (double))
;;;                       (double lgamma (double)))
;;;                      "m" "math.h")
;;; 
;;;
;;; (define-c-function '(char* getenv (char*)))
;;; (define-c-function '(int setenv (char* char* int)))
;;; (define get-environment-variable (let () (define-c-function '(char* getenv (char*))) getenv))
;;;
;;; (define file-exists? (let () (define-c-function '((int F_OK) (int access (char* int))) "" "unistd.h") (lambda (arg) (= (access arg F_OK) 0))))
;;; (define delete-file (let () (define-c-function '(int unlink (char*)) "" "unistd.h") (lambda (file) (= (unlink file) 0)))) ; 0=success, -1=failure
;;;
;;;
;;; these pick up Snd stuff:
;;;   (define-c-function '(char* version_info ()) "" "snd.h" "-I.")
;;;   (define-c-function '(mus_float_t mus_degrees_to_radians (mus_float_t)) "" "snd.h" "-I.")
;;;
;;;   (define-c-function '(snd_info* any_selected_sound ()) "" "snd.h" "-I.")
;;;   (define-c-function '(void select_channel (snd_info* int)) "" "snd.h" "-I.")
;;;   -> (select_channel (any_selected_sound) 1)
;;;
;;;   (define-c-function '(((graph_style_t int) (GRAPH_LINES GRAPH_DOTS GRAPH_FILLED GRAPH_DOTS_AND_LINES GRAPH_LOLLIPOPS)) 
;;;                        (void set_graph_style ((graph_style_t int)))) 
;;;                      "" "snd.h" "-I.")
;;;   
;;;
;;;  (define-c-function '(char* getcwd (char* size_t)) "" "unistd.h")
;;;    :(let ((str (make-string 32))) (getcwd str 32) str)
;;;    "/home/bil/cl\x00                   "
;;;    so it works in a sense -- there is a memory leak here
;;; 
;;;
;;; (define-c-function (list '(void* calloc (size_t size_t))
;;;		             '(void* malloc (size_t))
;;;		             '(void free (void*))
;;;		             '(void* realloc(void* size_t))
;;;		             '(void time (time_t*)) ; ignore returned value
;;;		             (list (symbol "struct tm*") 'localtime '(time_t*))
;;;                          (list 'size_t 'strftime (list 'char* 'size_t 'char* (symbol "struct tm*"))))
;;;                    "" "time.h")
;;;   > (let ((p (calloc 1 8)) (str (make-string 32))) (time p) (strftime str 32 "%a %d-%b-%Y %H:%M %Z" (localtime p)) (free p) str)
;;;   "Sat 11-Aug-2012 08:55 PDT\x00      "
;;;


(define-macro (defvar name value) 
  `(if (not (defined? ',name)) 
       (define ,name ,value)))

(defvar define-c-function-output-file-counter 0)   ; ugly, but I can't find a way around this (dlopen/dlsym stupidity)


;;; to place the new function in the caller's current environment, we need to pass it in explicitly:
(define-macro (define-c-function . args) 
  `(define-c-function-1 (current-environment) ,@args))


(define* (define-c-function-1 cur-env function-info (prefix "") (headers ()) (cflags "") (ldflags ""))
  ;; write a C shared library module that links in the functions in function-info
  ;;    function info is either a list: (return-type c-name arg-type) or a list thereof
  ;;    the new functions are placed in cur-env


  (define handlers (list '(integer s7_is_integer s7_integer s7_make_integer s7_Int)
			 '(boolean s7_is_boolean s7_boolean s7_make_boolean bool)
			 '(real s7_is_real s7_number_to_real s7_make_real s7_Double)

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

    (if (pair? type)                             ; in case the type name does not make its C type obvious: (graph_style_t int)
	(symbol->string (cadr type))
	(let ((type-name (symbol->string type)))
	  (cond ((substring? "**" type-name)     ; any complicated C pointer is uninterpreted
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
	 (functions ())
	 (constants ())
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
      (let* ((func-name (symbol->string name))
	     (num-args (length arg-types))
	     (base-name (string-append (if (> (length prefix) 0) prefix "g") "_" func-name))
	     (scheme-name (string-append prefix (if (> (length prefix) 0) ":" "") func-name)))
	
	;; scheme->C->scheme function
	(format p "static s7_pointer ~A(s7_scheme *sc, s7_pointer args)~%" base-name)
	(format p "{~%")
	
	;; get the Scheme args, check their types, assign to local C variables
	(if (positive? num-args)
	    (begin
	      (format p "  s7_pointer arg;~%")
	      (do ((i 0 (+ i 1))
		   (type arg-types (cdr type)))
		  ((= i num-args))
		(format p "  ~A ~A_~D;~%" (if (pair? (car type)) (caar type) (car type)) base-name i))
	      (format p "  arg = args;~%")
	      (do ((i 0 (+ i 1))
		   (type arg-types (cdr type)))
		  ((= i num-args))
		(let ((nominal-type (if (pair? (car type)) (caar type) (car type)))
		      (true-type (if (pair? (car type)) (cadar type) (car type))))
		  (format p "  if (~A(s7_car(arg)))~%" (checker true-type))
		  (format p "    ~A_~D = (~A)~A(~As7_car(arg));~%"
			  base-name i
			  nominal-type
			  (s7->C true-type)
			  (if (eq? (C-type->s7-type true-type) 'real)
			      "sc, " ""))
		  (format p "  else return(s7_wrong_type_arg_error(sc, ~S, ~D, s7_car(arg), ~S));~%"
			  func-name 
			  (if (= num-args 1) 0 (+ i 1))
			  (symbol->string (C-type->s7-type true-type)))
		  (if (< i (- num-args 1))
		      (format p "  arg = s7_cdr(arg);~%"))))))
	
	;; return C value to Scheme
	(if (pair? return-type) 
	    (set! return-type (cadr return-type)))
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
	(set! functions (cons (list scheme-name base-name func-name num-args) functions))))

    
    (define (add-one-constant type name)
      (let ((c-type (if (pair? type) (cadr type) type)))
	(if (symbol? name)
	    (set! constants (cons (list c-type (symbol->string name)) constants))
	    (for-each 
	     (lambda (c)
	       (set! constants (cons (list c-type (symbol->string c)) constants)))
	     name))))

  
    (define (end-c-file)
	;; now the init function
	;;   the new function is placed in the current (not necessarily global) environment
	
      (let ((o-file-name (string-append file-name ".o"))
	    (so-file-name (string-append file-name ".so"))
	    (base-name (string-append "_" (number->string define-c-function-output-file-counter) "_init")))
	(format p "void init_~A(s7_scheme *sc);~%" base-name)
	(format p "void init_~A(s7_scheme *sc)~%" base-name)
	(format p "{~%")
	(format p "  s7_pointer cur_env;~%")
	(format p "  cur_env = s7_outer_environment(s7_current_environment(sc));~%") ; this must exist because we pass load the env ourselves
	
	;; "constants" -- actually variables in s7 because we want them to be local to the current environment
	(if (pair? constants)
	    (begin
	      (format p "~%")
	      (for-each
	       (lambda (c)
		 (let* ((type (c 0))
			(c-name (c 1))
			(scheme-name (string-append prefix (if (> (length prefix) 0) ":" "") c-name)))
		   (format p "  s7_define(sc, cur_env, s7_make_symbol(sc, ~S), ~A(sc, (~A)~A));~%" 
			   scheme-name
			   (C->s7 type)
			   (C->s7-cast type)
			   c-name)))
	       constants)))

	;; functions
	(for-each
	 (lambda (f)
	   (let ((scheme-name (f 0))
		 (base-name   (f 1))
		 (func-name   (f 2))
		 (num-args    (f 3)))
	     (format p "~%  s7_define(sc, cur_env,~%")
	     (format p "            s7_make_symbol(sc, ~S),~%" scheme-name)
	     (format p "            s7_make_function(sc, ~S, ~A, ~D, 0, false, \"lib~A ~A\"));~%"
		     scheme-name
		     base-name
		     num-args
		     prefix
		     func-name)))
	 functions)

	(format p "}~%")
	(close-output-port p)
  
	;; now we have the module .c file -- make it into a shared object, load it, delete the temp files
	(system (format #f "gcc -c -fPIC ~A ~A" c-file-name cflags))
	(system (format #f "gcc ~A -shared -o ~A ~A" o-file-name so-file-name ldflags))

	(let ((new-env (augment-environment
			   cur-env
			 (cons 'init_func (string->symbol (string-append "init_" base-name))))))
	  (load so-file-name new-env))

	;;(delete-file c-file-name)
	(delete-file o-file-name)
	(zero? (delete-file so-file-name))
	))


    (initialize-c-file)

    (if (symbol? (cadr function-info))
	(if (= (length function-info) 3)
	    (apply add-one-function function-info)
	    (apply add-one-constant function-info))
	(for-each
	 (lambda (func)
	   (if (= (length func) 3)
	       (apply add-one-function func)
	       (apply add-one-constant func)))
	 function-info))

    (end-c-file)))





	    
	
;;;
;;;
;;; DIR *opendir (__const char *__name)
;;; extern int closedir (DIR *__dirp)
;;; struct dirent *readdir (DIR *__dirp) dirp->d_name is the filename if dirp not null -- we need NULL
;;; (-> dirp d_name) -> dirp->d_name
;;; (. dirp d_name) for dirp.d_name
;;; (define-macro (-> (symbol "struct dirent*") (d_name char*)) -> write c file with this access?
;;; similar to define-c-function, but code generated is return(s7_make_string(sc, ((struct dirent*)p)->d_name));
;;; but we don't want to do this on every (de)reference!?


;;; TODO: expand the types so gtk/xm might be done this way (via autoload)
;;;    structs -> environments, va-args? c-null=0?
;;;    c-complex->complex
;;;    can't we handle float* (etc) as c_pointer, then have a way to decode->vector?
;;;    (vct->vector (xen_make_vct_wrapper len c_ptr)) but why no vct_length?
;;; SOMEDAY: take a set of these functions (possibly in the current program via a C header like s7 and NULL as lib name)
;;;   and run the tester on the current program (going below the scheme level in a sense)
;;; PERHAPS: xgdata->a long (define-c-functions ...) call and see if xg.c can be dispensed with!
;;;   (cast var) i.e. (G_OBJECT shell) in current code is (GObject*)val = GObject* f(arg) return(G_OBJECT(arg)) I guess
;;;   seems pointless -- we can build in the cast in g_signal_etc
;;;   var args here also: gtk_text_buffer_create_tag for example
;;;   (even better: turn a C header into a define-c-function call -- for simple cases this is not out of the question)
;;; it would also be possible to insert arbitrary C code -- maybe that's the way to handle dirp->d_name?
;;;    too ugly...
;;; TODO: make an OSX case -- do we need a *feature* for the current OS?
;;;    there's OSTYPE="linux" or "darwin", but can we depend on these?
;;; PERHAPS: let name (cadr) be a list too for enums: ((sync_style_t int) (SYNC_NONE SYNC_ALL SYNC_BY_SOUND))
;;; TODO: doc (sndscm? perhaps lint and cload in s7.html?) and s7test for cload