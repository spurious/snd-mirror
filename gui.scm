
;;; Functions to help making various gui-things more convenient and without
;;; worrying about whether we use gtk or motif. (see ladspa.scm and snd_conffile.scm for examples of use)
;;; -Kjetil S. Matheussen.


(if (provided? 'snd-gui.scm)
    (begin
      (display "Very warning: gui.scm has already been loaded. (This is not good.)")
      (newline)))


(provide 'snd-gui.scm)

(use-modules (ice-9 optargs)
	     (ice-9 format)
	     (srfi srfi-1)
	     (srfi srfi-13))


;;(use-modules (oop goops))






;;##############################################################
;; Various functions
;;##############################################################

(define use-gtk (if (provided? 'snd-gtk)
		    #t
		    #f))


(if (not use-gtk)
    (begin
      (display "Warning, gui.scm might not work very well with Motif anymore.")(newline)
      (display "You should compile up Snd using the --with-gtk and --with-static-xm configure options.")(newline)))


(if use-gtk
    (if (not (provided? 'xg))
	(let ((hxm (dlopen "xm.so")))
	  (if (string? hxm)
	      (snd-error (format #f "gui.scm needs the xg module: ~A" hxm))
	      (dlinit hxm "init_xm"))))
    (if (not (provided? 'xm))
	(let ((hxm (dlopen "xm.so")))
	  (if (string? hxm)
	      (snd-error (format #f "gui.scm needs the xm module: ~A" hxm))
	      (dlinit hxm "init_xm")))))
    



(define-macro (c-load-from-path filename)
  `(if (not (provided? (symbol-append 'snd- ',filename '.scm)))
       (load-from-path (symbol->string (symbol-append ',filename '.scm)))))


(define (c-atleast1.7?)
  (or (>= (string->number (major-version)) 2)
      (and (string=? "1" (major-version))
	   (>= (string->number (minor-version)) 7))))

(define (c-integer somekindofnumberorsomething)
;;    somekindofnumberorsomething)
  (inexact->exact (floor somekindofnumberorsomething)))

(define (c-integer2 somekindofnumberorsomething)
  (inexact->exact (floor somekindofnumberorsomething)))

(define (c-editor-widget snd)
  (list-ref (channel-widgets snd 0) 0))


;; Copied from gtk-popup.scm
(define* (c-g_signal_connect obj name func #:optional data)
  (g_signal_connect_closure_by_id (GPOINTER obj)
				  (g_signal_lookup name (G_OBJECT_TYPE (GTK_OBJECT obj)))
				  0
				  (g_cclosure_new func data #f)
				  #f))


;; C-like for-iterator
(define (c-for init pred least add proc)
  (do ((n init (+ n add)))
      ((not (pred n least)))
    (proc n)))


#!
(c-for 2 < 7 1
       (lambda (n) (display n)(newline)))
!#



(define (c-sync? snd)
  (> (sync snd) 0))


(define (c-for-each-channel snd func)
  (if (c-sync? snd)
      (c-for 0 < (chans snd) 1 func)
      (func (selected-channel snd))))

(define (c-for-each-channel2 snd func)
  (c-for 0 < (chans snd) 1 func))


(define (c-get-channel snd y)
  (define (get-channel ch)
    (if (>= ch (chans snd))
	#f
	(let ((axinfo (axis-info snd ch)))
	  (if (and (>= y (list-ref axinfo 13))
		   (< y (list-ref axinfo 11)))
	      ch
	      (get-channel (1+ ch))))))
  (get-channel 0))


(define (c-get-bounds snd ch func)
  (let ((axinfo (axis-info snd ch)))
    (func (list-ref axinfo 10)
	  (list-ref axinfo 13)
	  (list-ref axinfo 12)
	  (list-ref axinfo 11))))

(define (c-get-mouse-info2 snd x y mustcall func)
  (let ((ch (if (number? mustcall) mustcall (c-get-channel snd y))))
    (if (and mustcall
	     (not ch))
	(set! ch 0))
    (if ch
	(let* ((axinfo (axis-info snd ch)))
	  (if (or mustcall
		  (and (>= x (list-ref axinfo 10))
		       (< x (list-ref axinfo 12))))
	      (func ch
		    (c-scale (* (srate snd) (position->x x snd ch)) (list-ref axinfo 0) (list-ref axinfo 1) 0 1)
		    (c-scale (position->y y snd ch) 1 -1 0 1)))))))


(define (c-get-mouse-info snd x y mustcall func)
  (let ((axinfo (axis-info snd 0))
	(ch 0)
	(xmax 0)
	(ymax 0)
	(newx 0)
	(newy 0))
    (define (set-channel!)
      (if (>= ch (chans snd))
	  #f
	  (begin
	    (set! axinfo (axis-info snd ch))
	    (if (and (>= y (list-ref axinfo 13))
		     (< y (list-ref axinfo 11)))
		#t
		(begin
		  (set! ch (1+ ch))
		  (set-channel!))))))
    (let ((legalchannel (set-channel!)))
      (if (or mustcall
	      (and legalchannel
		   (>= x (list-ref axinfo 10))
		   (< x (list-ref axinfo 12))))
	  (begin
	    (set! ch (min (- (chans snd) 1) ch))
	    (set! axinfo (axis-info snd ch))
	    (set! xmax (- (list-ref axinfo 12) (list-ref axinfo 10)))
	    (set! ymax (- (list-ref axinfo 11) (list-ref axinfo 13)))
	    (set! newx (min xmax (max 0 (- x (list-ref axinfo 10)))))
	    (set! newy (min ymax (max 0 (- y (list-ref axinfo 13)))))
	    (func ch x newy))))))

;; Taken from new-effects.scm
(define yellow-pixel
  (let ((pix #f))
    (lambda ()
      (if (not pix)
	  (let* ((shell (cadr (main-widgets)))
		 (dpy (XtDisplay shell))
		 (scr (DefaultScreen dpy))
		 (cmap (DefaultColormap dpy scr))
		 (col (XColor)))
	       (if (= (XAllocNamedColor dpy cmap "yellow" col col) 0)
		   (snd-error "can't allocate yellow!")
		   (set! pix (.pixel col)))))
      pix)))


(define (c-report text)
  (if (defined? 'change-window-property)
      (change-window-property "SND_VERSION" "WM_NAME"
			      (if (string=? " " text)
				  (string-append "snd: "
						 (apply string-append (map (lambda (snd) (string-append (short-file-name snd) ", "))
									   (reverse (cdr (sounds)))))
						 (short-file-name (car (sounds))))
				  text))
      (set! (window-property "SND_VERSION" "WM_NAME")
	    (if (string=? " " text)
		(string-append "snd: "
			       (apply string-append (map (lambda (snd) (string-append (short-file-name snd) ", "))
							 (reverse (cdr (sounds)))))
			       (short-file-name (car (sounds))))
		text))))
  

;; Set cursor-style. Copied from the manual.
(define (c-set-sound-cursor snd shape)
  (do ((j 0 (1+ j)))
      ((= j (channels snd)) #f)
    (set! (cursor-style snd j) shape)))


(define (c-for-each func . lists)
  (let ((n 0))
    (apply for-each (cons (lambda els
			    (apply func (cons n els))
			    (set! n (1+ n)))
			  lists))))

(define (c-scale-do x x1 x2 y1 y2)
  (+ y1
     (/ (* (- x x1)
	   (- y2 y1))
	(- x2 x1))))

(define (c-scale-do2 x y1 y2)
  (+ y1 (* x (- y2 y1))))

(define (c-scale-do3 x x1 x2)
  (/ (- x x1)
     (- x2 x1)))

(define-macro (c-scale2 x x1 x2 y1 y2)
  (if (and (number? x1) (number? x2) (= 0 x1) (= 1 x2))
      `(c-scale-do2 ,x ,y1 ,y2)
      (if (and (number? y1) (number? y2) (= 0 y1) (= 1 y2))
	  `(c-scale-do3 ,x ,x1 ,x2)
	  `(c-scale-do ,x ,x1 ,x2 ,y1 ,y2))))

;; Snd has its own filter function (a clm function) overriding the guile filter function.
(define (filter-org pred list)
  (remove (lambda (e) (not (pred e)))
	  list))

(define (insert! list pos element)
  (if (= 0 pos)
      (cons element list)
      (let ((f (drop list (- pos 1))))
	(set-cdr! f (cons element (cdr f)))
	list)))

(define (sublist l start end)
  (take (drop l start) (- end start)))

;;(sublist '(0 1 2 3 4 5 6 7 8 9) 2 5)


(define (c-display . args)
  (c-for-each (lambda (n arg)
		(if (> n 0)
		    (display " "))
		(display arg))
	      args)
  (newline))


;; Eval c-code on the fly. Code must have a void()-function called "das_init".
(define* (c-eval-c #:key (compile-options "") . codestrings)
  (let* ((evalstring "")
	(sourcefile (string-append (tmpnam) ".c"))
	(libfile (string-append sourcefile ".so"))
	(fd (open-file sourcefile "w"))
	(guile-config (string-append (cdr (assoc 'bindir %guile-build-info)) "/guile-config")))
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
    ;;(c-display sourcefile)
    (system (string-append "gcc -Wall -O2 -shared -o " libfile " " sourcefile " "
			   (if (getenv "CFLAGS") (getenv "CFLAGS") "") " " (if (getenv "LDFLAGS") (getenv "LDFLAGS") "") " "
			   (string #\`) guile-config " compile" (string #\`) " "
			   compile-options))
    (dynamic-call "das_init" (dynamic-link libfile))
    (system (string-append "rm " libfile " " sourcefile))
    ))


(define (c-eval-c2 compile-options top . codestrings)
  (c-eval-c #:compile-options compile-options
	    (let ((n 0)
		  (funcs '())
		  (<-> string-append))
	      (apply <-> (map (lambda (x) (<-> x (string #\newline)))
			      (append (list "#include <stdio.h>"
					    "#include <libguile.h>"
					    top
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
				      (map (lambda (x)
					     (let ((name (caar x))
						   (args (cadar x))
						   (code (cdr x))
						   (funcname (format #f "func~A" n)))
					       (set! n (1+ n))
					       (set! funcs (cons (list name (length args) funcname) funcs))
					       (<-> "static SCM " funcname "("
						    (apply <-> (map (lambda (arg)
								      (<-> "SCM " (symbol->string arg)
									   (if (not (eq? arg (car (reverse args))))
									       ","
									       "")))
								    args))
						    "){" (apply <-> (map (lambda (x) (<-> x " ")) code)) "}")))
					   (map (lambda (funcdef)
						  (if (list? funcdef)
						      funcdef
						      (let* ((temp (apply <array> (map string-trim-both (string-split funcdef #\())))
							     (retname (string-split (temp 0) #\space))
							     (rettype (string-trim-both (apply <-> (map (lambda (x) (<-> x " ")) (reverse (cdr (reverse retname)))))))
							     (name (car (reverse retname)))
							     (args (let ((temp2 (string-trim-both (car (string-split (temp 1) #\))))))
								     (if (= (string-length temp2) 0)
									 '()
									 (map (lambda (x)
										(let ((dassplit (map string-trim-both (string-split x #\space))))
										  (list (string-trim-both (apply <-> (map (lambda (x)
															    (<-> x " "))
															  (reverse (cdr (reverse dassplit))))))
											(string-trim-both (car (reverse dassplit))))))
									      (string-split temp2 #\,)))))
							     (to-scm (lambda (type)
								       (cond ((string=? type "void") "UNSPECIFIED")
									     ((member type (list "char *" "const char*" "const char *" "char*" "const gchar*") string=?) "STRING")
									     ((member type (list "int" "unsigned int" "long" "unsigned long" "short" "unsigned short"
												 "char" "unsigned char" "gint") string=?) "INTEGER")
									     ((member type (list "float" "double") string=?) "DOUBLE")
									     (else "POINTER"))))
							     )
							(if (char=? #\* (car (string->list name)))
							    (begin
							      (set! name (list->string (cdr (string->list name))))
							      (set! rettype (<-> rettype "*"))))
							(list  (list (string->symbol name) (map (lambda (x) (string->symbol (if (char=? #\* (car (string->list (cadr x))))
																(list->string (cdr (string->list (cadr x))))
																(cadr x))))
												args))
							       (apply <->
								      (append (list (let ((ret-scm (to-scm rettype)))
										      (cond((string=? "UNSPECIFIED" ret-scm) "RETURN_UNSPECIFIED(")
											   ((string=? "POINTER" ret-scm) "RETURN_POINTER(")
											   ((string=? "STRING" ret-scm) "RETURN_STRING(")
											   (else
											    (<-> "return MAKE_" ret-scm "(")))))
									      (list name "(")
									      (map (lambda (x)
										     (let* ((type (car x))
											    (name (cadr x)))
										       (if (char=? #\* (car (string->list name)))
											   (begin
											     (set! name (list->string (cdr (string->list name))))
											     (set! type (<-> type "*"))))
										       (<-> "GET_" (to-scm type) "(" name ")"
											    (if (not (eq? x (car (reverse args)))) "," ""))))
										   args)
									      (list "));")))))))
						
						codestrings))
				      (cons "void das_init(){"
					    (map (lambda (x)
						   (let ((name (car x))
							 (numargs (cadr x))
							 (funcname (caddr x)))
						     (<-> "scm_c_define_gsubr(\"" (symbol->string name) "\"," (format #f "~A" numargs) ",0,0," funcname ");")))
						 funcs))
				      (list "}")))))))




;; define-toplevel is like define, but at the toplevel.
(c-eval-c
"
#include <libguile.h>
void das_init(){
  scm_c_define_gsubr(\"define-toplevel\",2,0,0,scm_define);
}
")


(c-eval-c2 ""
	   ""
	   '((c-scale (x x1 x2 y1 y2))
	     "  double x1_=GET_DOUBLE(x1);"
	     "  double y1_=GET_DOUBLE(y1);"
	     "  return MAKE_DOUBLE(y1_ + ( ( (GET_DOUBLE(x)-x1_) * (GET_DOUBLE(y2)-y1_)) / (GET_DOUBLE(x2)-x1_)));"))
	     


;; A desperate way to minimize consing. (Guile is definitely not a realtime friendly language...)

(c-for 0 < 50 1
       (lambda (n)
	 (define-toplevel (string->symbol (format #f "c-arg~A" n)) 0)))


(define-macro (lambda-non-cons args . body)
  (let ((lets '())
	(sets '()))
    (c-for-each (lambda (n val)
		  (set! lets (cons (list  val #f) lets))
		  (set! sets (cons (list 'set!
					 val
					 (string->symbol (format #f "c-arg~A" n)))
				   sets)))
		args)
    `(let (,@lets)
       (lambda ()
	 ,@sets
	 ,@body))))


(define-macro (call-non-cons func . args)
  (let ((sets '()))
    (c-for-each (lambda (n val)
		  (set! sets (cons (list 'set!
					 (string->symbol (format #f "c-arg~A" n))
					 val)
				   sets)))
		args)
    `(begin
       ,@(reverse sets)
       (,func))))



(define-macro (add-call-hook! funcname func)
  `(define ,funcname
     (let ((func-old ,funcname))
       (lambda args
	 (apply ,func args)
	 (apply func-old args)))))

(define-macro (add-called-hook! funcname func)
  `(define ,funcname
     (let ((func-old ,funcname))
       (lambda args
	 (apply func-old args)
	 (apply ,func args)))))



(define c-gc-isoff #f)

;; The following functions ensure that gc-off is not called more times than gc-on.
;; It takes an optional argument which is the number of milliseconds before turning
;; the garbage collector on again, no matter what. Set this arguement to #f to avoid
;; automaticly turning the garbage collector on. But that is dangerous.
;;(define* (c-gc-off #:optional (timeout 2000))
(define* (c-gc-off #:optional (timeout 2000))
  (if (not c-gc-isoff)
      (begin
	(gc-off)
	(set! c-gc-isoff #t)
	(if timeout
	    (in timeout
		(lambda ()
		  (c-gc-on)))))))


;; Ouch, might actually freeze the machine if using jack. Better turn the function off.
;; (re-mlocking all memory in audio.c without using MCL_FUTURE seems to fix the freezing problem)
;(define (c-gc-off)
;  #t)

(define (c-gc-on)
  (if c-gc-isoff
      (begin
	(gc-on)
	(set! c-gc-isoff #f))))




;;##############################################################
;; OO  (Goops/cloos syntax is so ugly.)
;;##############################################################

(define instance?-old #f)
(define define-class-old #f)
(define define-method-old #f)

(if (defined? 'instance?)
    (set! instance?-old instance?))
(if (defined? 'define-class)
    (set! define-class-old define-class))
(if (defined? 'define-method)
    (set! define-method-old define-method))


(define-macro (define-class def . body)
  (if (and #f (symbol? def))
      `(define-class-old ,def ,@body)
      (begin
	(for-each (lambda (a) (if (eq? (car a) 'define-constructor)
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
		  (super #f)
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
	     (define-method (dir)
	       (append (cons this->class-name
			     (hash-fold (lambda (key value s) (cons key s)) '() 
					methods))
		       (map (lambda (super) (-> super dir))
			    supers)))
	     (define-method (get-method name)
	       (or (hashq-ref methods name)
		   (any (lambda (super) (-> super get-method name))
			supers)))
	     (define-method (instance? class-name)
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
	     
	     this)))))
  
  
(define-macro (add-method nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda ,(cdr nameandvars) ,@body)))

(define-macro (add-method* nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda* ,(cdr nameandvars) ,@body)))

(define-macro (define-method nameandvars . body)
  `(define ,(symbol-append 'this-> (car nameandvars))
     (add-method ,nameandvars ,@body)))

(define-macro (define-method* nameandvars . body)
  `(define ,(symbol-append 'this-> (car nameandvars))
     (add-method* ,nameandvars ,@body)))

(define-macro (var name initial)
  (let ((thisname (symbol-append 'this-> name)))
    `(define ,thisname
       (begin
	 (add-method (,name . rest) (if (null? rest) ,thisname (set! ,thisname (car rest))))
	 ,initial))))

(define-macro (define-constructor nameandvars . body)
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
		    (set! func (object 'get-method method))))
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

(define-class (<super1> sum)
  (var avar 2)
  (define-method (super1)
    (display "super1 sum: ")(display sum)
    (newline)))

(define-class (<super2> sum)
  (define-method (super2)
    (display "super2 sum: ")(display sum)
    (newline)))

(define-class (<bank> sum) (Super (<super1> (+ 1000 sum)) (<super2> (+ 2000 sum)))
  (define-method (print-sum)
    (display sum)(newline))
  (define-method (deposit x)
    (set! sum (+ sum x))
    (this->print-sum))
  (define-method (withdraw x)
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
(-> b nosuchmethod)
!#






;;##############################################################
;; Array 
;;##############################################################

(define-class (<array> . rest)
  (define dasarray (list->vector rest))

  (define-method (get-vector)
    dasarray)
  (define-method (set-vector! v)
    (set! dasarray v))
  (define-method (get-list)
    (vector->list dasarray))
  (define-method (set-list! l)
    (set! dasarray (list->vector l)))
  (define-method (reset!)
    (this->set-list! rest))
  (define-method (set!! . rest)
    (this->set-list! rest))
  (define-method (set! . rest)
    (c-for-each (lambda (i val)
		  (vector-set! dasarray i val))
		rest))
  (define-method (for-each func)
    (c-for 0 < (this->length) 1
	   (lambda (n)
	     (func n (vector-ref dasarray n)))))
  (define-method (map! func)
    (this->for-each (lambda (n el)
		      (vector-set! dasarray n (func n el)))))
  (define-method (map func)
    (let* ((ret '(0))
	   (tail ret))
      (this->for-each (lambda (n el)
			(let ((new (list (func n el))))
			  (set-cdr! tail new)
			  (set! tail new))))
      (cdr ret)))
  (define-method (length)
    (vector-length dasarray))

  ;; Python-like list-selector (not complete, or optimized, or very useful in the current form.)
  (define-method (p sel)
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

  (define-constructor (length len #:optional default)
    (this->set-vector! (make-vector len default))
    this)

  (define-constructor (map len func)
    (this->set-vector! (make-vector len #f))
    (this->map! (lambda (n el) (func n)))
    this)

  (define-constructor (multidimensional dimensions #:optional default)
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
(-> a for-each (lambda (n1 el1) (-> el1 map! (lambda (n2 el2) (+ n1 (/ n2 10))))))
(-> a map (lambda (n el) (-> el get-list)))
((a 0) 3)
((a 3) 2)
!#






;;##############################################################
;; A hook class.
;;##############################################################
(define-class (<hook>)
  (define funcs '())
  (define system-funcs '())
  (define steelfunc #f)
  (define-method (add! func)
    (set! funcs (cons func funcs)))
  (define-method (add-system! func)
    (set! system-funcs (cons func system-funcs)))
  (define-method (only! func)
    (set! steelfunc func))
  (define-method (not-only!)
    (set! steelfunc #f))
  (define-method (remove! func)
    (set! funcs (remove! (lambda (f) (eq? f func))
			 funcs)))
  (define-method (run . args)
    (if steelfunc
	(apply steelfunc args)
	(call-with-current-continuation
	 (lambda (return)
	   (for-each (lambda (func)
		       (if (eq? 'stop! (apply func args))
			   (return 'stop!)))
		     (append system-funcs funcs)))))))




;;##############################################################
;; Mouse stuff. All c-code mouse handling in snd is disabled,
;; and replaced with scheme-code. Selection handling is
;; handled in snd_conffile.scm.
;;##############################################################


(define mouse-button-press-hook (<hook>))
(define mouse-move-hook (<hook>))
(define mouse-drag2-hook (<hook>))
(define mouse-button-release-hook (<hook>))
(define mouse-scroll-hook (<hook>))
(define selection-changed2-hook (<hook>))


(if (not use-gtk)
    (c-display "c-remove-motionhandler not implemented for motif")
    (c-eval-c2 (string-append (string #\`) "pkg-config --cflags gtk+-2.0" (string #\`))
	       "#include <gtk/gtk.h>"
	    
	       ;; A function to remove all the gtk mousehandlers in snd for a widget.
	       '((c-remove-mousehandlers (w))
		 "  gpointer g=(gpointer)GET_POINTER2(w);"
		 "  g_signal_handler_disconnect(g,g_signal_handler_find(g,"
		 "						      G_SIGNAL_MATCH_ID,"
		 " 						      g_signal_lookup(\"motion_notify_event\",G_OBJECT_TYPE(g)),"
		 "						      0,0,0,0));"
		 "  g_signal_handler_disconnect(g,g_signal_handler_find(g,"
		 "						      G_SIGNAL_MATCH_ID,"
		 "						      g_signal_lookup(\"button_press_event\",G_OBJECT_TYPE(g)),"
		 "						      0,0,0,0));"
		 "  g_signal_handler_disconnect(g,g_signal_handler_find(g,"
		 "						      G_SIGNAL_MATCH_ID,"
		 "						      g_signal_lookup(\"button_release_event\",G_OBJECT_TYPE(g)),"
		 "						      0,0,0,0));"
		 "  g_signal_handler_disconnect(g,g_signal_handler_find(g,"
		 "						      G_SIGNAL_MATCH_ID,"
		 "						      g_signal_lookup(\"scroll_event\",G_OBJECT_TYPE(g)),"
		 "						      0,0,0,0));"
		 "  return SCM_UNSPECIFIED;")

	       ;; Wrapper for gdk_draw_string
	       `((c-draw-string (s_widget s_gc s_x s_y s_text))
		 "  GtkWidget *widget=(GtkWidget*)GET_POINTER2(s_widget);"
		 "  GdkGC *gc=(GdkGC*)GET_POINTER2(s_gc);"
		 "  GdkFont *font=gtk_style_get_font(widget->style);"
		 "  gdk_draw_string(widget->window,font,gc,GET_INTEGER(s_x),GET_INTEGER(s_y),GET_STRING(s_text));"
		 "  return SCM_UNSPECIFIED;")
	       ))



(add-hook! after-open-hook
	   (lambda (snd)
	     (let ((w (c-editor-widget snd)))
	       (if (not use-gtk)
		   (begin
		     (XtAddEventHandler w ButtonPressMask #f 
					(lambda (w c e f)
					  (-> mouse-button-press-hook run 
					      snd (.x e) (.y e) (.state e))))
		     (XtAddEventHandler w ButtonMotionMask #f 
					(lambda (w c e f)
					  (-> mouse-drag2-hook run 
					      snd (.x e) (.y e) (.state e))))
		     (XtAddEventHandler w ButtonReleaseMask #f 
					(lambda (w c e f)
					  (-> mouse-button-release-hook run 
					      snd (.x e) (.y e) 1 (.state e)))))
		   (let ((ispressed #f)
			 (ismoved #f))
		     
		     (c-remove-mousehandlers w)

		     (c-g_signal_connect w "button_press_event"
					 (lambda (w e i)
					   (focus-widget w)
					   (set! ispressed #t)
					   (set! ismoved #f)
					   (if (and (not (eq? 'stop!
							      (-> mouse-button-press-hook run
								  snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e))
								  (.button (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))))
						    (= (.button (GDK_EVENT_BUTTON e)) 3))
					       (run-hook gtk-popup-hook w e i snd 0))))
		     (c-g_signal_connect w "motion_notify_event"
				       (lambda (w e i)
					 (set! ismoved #t)
					 (let ((args (if (.is_hint (GDK_EVENT_MOTION e))
							 (let ((s (cdr (gdk_window_get_pointer (.window e)))))
							   (list snd (car s) (cadr s) (.button (GDK_EVENT_BUTTON e)) (caddr s)))
							 (list snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e))
							       (.button (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e))))))
					   (if (and (not (eq? 'stop! (apply (<- mouse-move-hook run) args)))
						    ispressed)
					       (apply (<- mouse-drag2-hook run) args)))))
		     (c-g_signal_connect w "button_release_event"
					 (lambda (w e i)
					   (set! ispressed #f)
					   (-> mouse-button-release-hook run
					       snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e)) (.button (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))))
		     (c-g_signal_connect w "scroll_event"
					 (lambda (w e i)
					   (set! ispressed #f)
					   (-> mouse-scroll-hook run
					       snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))
					   ))
		     )))))


(define (c-rightbutton? button)
  (= 3 button))

(define (c-leftbutton? button)
  (= 1 button))

(define (c-ctrl? stat)
  (= (logand stat 4) 4))

(define (c-shift? stat)
  (= (logand stat 1) 1))

(define (c-altGr? stat)
  (= (logand stat 8192) 8192))

;; Common way to treat mouse
(define-class (<mouse-cycle> clickfunc movefunc releasefunc #:key (scaled #f) (add-type 'add!))

  (define (press-hook snd pix-x pix-y button stat)
    (let ((isdragged #f)
	  (mousefunc (if scaled c-get-mouse-info2 c-get-mouse-info)))
      (mousefunc snd pix-x pix-y #t
		 (lambda (ch x y)
		   (let ((res (clickfunc snd ch x y button stat)))
		     (if (or (eq? 'stop! res)
			     (eq? 'allways-stop! res))
			 (begin
			   (-> mouse-move-hook only!
			       (lambda (snd pix-x pix-y button stat)
				 (set! isdragged #t)
				 (mousefunc snd pix-x pix-y ch
					    (lambda (dasch x y)
					      (movefunc snd ch x y button stat)))))
			   (-> mouse-button-release-hook only!
			       (lambda (snd pix-x pix-y button stat)
				 (-> mouse-move-hook not-only!)
				 (-> mouse-button-release-hook not-only!)
				 (mousefunc snd pix-x pix-y ch
					    (lambda (dasch x y)
					      (if (and (eq? 'allways-stop! res) (not isdragged))
						  (begin
						    ;; Small hack needed to get the mouse-click-hook to run.
						    (focus-widget (c-editor-widget snd))
						    (select-channel ch)
						    (run-hook mouse-click-hook
							      snd ch button stat pix-x pix-y time-graph))
						  (if (= 7 (car (procedure-property releasefunc 'arity)))
						      (releasefunc snd ch x y button stat isdragged)
						      (releasefunc snd ch x y button stat)))))))
			   'stop!)))))))

  (define-method (delete!)
    (-> mouse-button-press-hook remove! press-hook))

  (mouse-button-press-hook add-type press-hook)

  )



;; Allways select current channel
(-> mouse-button-press-hook add-system!
    (lambda (snd x y button stat)
      (let ((ch (c-get-channel snd y)))
	(if ch 
	    (select-channel ch)))))


;; Run the mouse-click-hook
(let ((isdragged #f))
  (-> mouse-button-press-hook add!
      (lambda (snd x y button stat)
	(set! isdragged #f)))
  (-> mouse-drag2-hook add!
      (lambda (snd x y button stat)
	(set! isdragged #t)))
  (-> mouse-scroll-hook add!
      (lambda (snd orgx y stat)
	(c-get-mouse-info snd orgx y #t
			  (lambda (ch x y)
			    (focus-widget (c-editor-widget snd))
			    (run-hook mouse-click-hook
				      snd ch (+ stat 4) 0 orgx y time-graph)))))
  (-> mouse-button-release-hook add!
      (lambda (snd orgx y button stat)
	(if (not isdragged)
	    (c-get-mouse-info snd orgx y #t
			      (lambda (ch x y)
				(focus-widget (c-editor-widget snd))
				(select-channel ch)
				(run-hook mouse-click-hook
					  snd ch button stat orgx y time-graph)))))))


;;  Moving marks with the mouse
(let ((currmark #f))
  (<mouse-cycle> (lambda (snd ch x y button stat)
		 (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch)))))
		       (pointrange (- (* (srate snd) (position->x 15 snd ch))
				      (* (srate snd) (position->x 0 snd ch)))))
		   (if (and (> y 7)
			    (< y 15))
		       (call-with-current-continuation
			(lambda (return)
			  (for-each
			   (lambda (mark)
			     (if (< (abs (- (mark-sample mark) pointpos)) pointrange)
				 (begin
				   (set! currmark mark)
				   (return 'stop!))))
			   (list-ref (list-ref (marks) snd) ch)))))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     (if (> (mark-sync currmark) 0)
			 (move-syncd-marks (mark-sync currmark)
					   (- pointpos (mark-sample currmark)))
			 (set! (mark-sample currmark) pointpos))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     (if (> (mark-sync currmark) 0)
			 (move-syncd-marks (mark-sync currmark)
					   (- pointpos (mark-sample currmark)))
			 (set! (mark-sample currmark) pointpos))))

		 #:add-type 'add-system!
		 ))




;;  Moving mixes with the mouse
(let ((currmixes #f)
      (offset 0))

  (<mouse-cycle> (lambda (snd ch x y button stat)
		 (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch)))))
		       (pointrange (- (* (srate snd) (position->x (mix-tag-width) snd ch))
				      (* (srate snd) (position->x 0 snd ch)))))
		   (call-with-current-continuation
		    (lambda (return)
		      (c-for-each
		       (lambda (n mix)
			 (if (< (abs (- (mix-position mix) pointpos)) pointrange)
			     (let ((ypos (if (> (mix-tag-y mix) 0) 
					     (mix-tag-y mix)
					     (- (* (mix-tag-height) n) 5))))
			       (if (and (>= y ypos)
					(<= y (+ ypos (+ (mix-tag-height) 3))))
				   (begin
				     (set! offset (- (mix-position mix) pointpos))
				     (set! currmixes '())
				     (if (c-sync? snd)
					 (for-each (lambda (dasmix)
						     (if (= (mix-position mix) (mix-position dasmix))
							 (set! currmixes (cons dasmix currmixes))))
						   (apply append (mixes snd)))
					 (set! currmixes (list mix)))
				     (return 'stop!))))))
		       (mixes snd ch))))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     ;;(set! (mix-position currmix) (+ offset pointpos))
		     (draw-line x 0 x (list-ref (axis-info snd ch) 11))))
		 
		 (lambda (snd ch x y button stat)
		   (let ((pointpos (max 0 (c-integer (* (srate snd) (position->x x snd ch))))))
		     (for-each (lambda (mix)
				 (set! (mix-position mix) (+ offset pointpos)))
			       currmixes)
		     (set! currmixes #f)))
		 
		 #:add-type 'add-system!
		 ))




;; To avoid irritating non-smoothness, we turn off the garbage collector for 2 seconds. (Does not seem to be necessary for guile 1.7 and newer)
(if (not (c-atleast1.7?))
    (-> mouse-button-press-hook add-system!
	(lambda (snd orgx orgy button stat)
	  (c-gc-off))))







;;##############################################################
;; Double buffer
;;##############################################################

(define-class (<doublebuffer> parent)
  
  (var pixmap #f)
  (var drawable #f)

  (define width 0)
  (define height 0)

  (define (make-pixmap!)
    (set! width (car (widget-size parent)))
    (set! height (cadr (widget-size parent)))
    (set! this->pixmap (gdk_pixmap_new (.window parent) width height -1))
    (set! this->drawable (GDK_DRAWABLE this->pixmap)))

  (define-method (pixmap->parent)
    (gdk_draw_drawable (GDK_DRAWABLE (.window parent))
		       (.black_gc (.style parent))
		       this->drawable
		       0 0
		       0 0
		       width height))

  (define-method (parent->pixmap)
    (gdk_draw_drawable this->drawable
		       (.black_gc (.style parent))
		       (GDK_DRAWABLE (.window parent))
		       0 0
		       0 0
		       width height))

  (make-pixmap!)
  (this->parent->pixmap)

  (c-g_signal_connect parent "expose_event" 
		      (lambda (w e d) 
			(c-display "ai2")
			(this->parent->pixmap)))

)



;;##############################################################
;; Paint (not usable yet)
;;##############################################################

(define-class (<paint> parent width height)

  (define pixmap #f)
  (define colors '())
  (define gc #f)
  (define font #f)

  (define xmin width)
  (define ymin height)
  (define xmax 0)
  (define ymax 0)

  (define (update-minmax x1 y1 x2 y2)
    (set! xmin (min xmin x1 x2))
    (set! ymin (min ymin y1 y2))
    (set! xmax (max xmax x1 x2))
    (set! ymax (max ymax y1 y2)))

  (define (reset-minmax)
    (set! xmin width)
    (set! ymin height)
    (set! xmax 0)
    (set! ymax 0))

  (define-method (line color x1 y1 x2 y2)
    (update-minmax x1 y1 x2 y2)
    (gdk_draw_line pixmap color x1 y1 (- x2 x1 -1) (- y2 y1 -1)))

  (define (update-do x1 y1 x2 y2)
    (gdk_draw_pixmap (.window parent)
		     gc
		     pixmap
		     x1 y1
		     x1 y1
		     (- x2 x1 -1) (- y2 y1 -1)))

  (define-method (update)
    (if (and (>= xmax xmin)
	     (>= ymax ymin))
	(update-do xmin ymix xmax ymax))
    (reset-minmax))

  (define-method (update-all)
    (reset-minmax)
    (update-do 0 0 width height))

  (set! pixmap (gdk_pixmap_new (.window parent)
			       width
			       height
			       -1))
  
  (gtk_signal_connect (GTK_OBJECT parent) "expose_event"
		      (lambda (w e)
			(this->update-all))
		      #f)

  )




;;##############################################################
;; Pool (this class is pointless, right?)
;; The point is to avoid garbage collection, but just
;; calling a function does consing of its arguments, or?
;;##############################################################

(define-class (<pool>)
  (define all 0)
  (define pool '())
  (define (get)
    (set! all (+ all 8))
    (if (or #t (null? pool))
	(cons #f #f)
	(let ((ret pool))
	  (set! pool (cdr pool))
	  ret)))
  (define (put cell)
    (if #t
	(c-free cell)
	(begin
	  (set-cdr! cell pool)
	  (set! pool cell))))
  (define-method (cons a b)
    (let ((cell (get)))
      (set-car! cell a)
      (set-cdr! cell b)
      cell))
  (define-method (list . rest)
    (if (null? rest)
	rest
	(this->cons (car rest) (apply this->list (cdr rest)))))
  (define-method (list-copy daslist)
    (apply this->list daslist))
  (define-method (map func daslist)
    (let* ((ret '(0))
	   (tail ret))
      (for-each (lambda (el)
		  (let ((new (this->cons (func el) '())))
		    (set-cdr! tail new)
		    (set! tail new)))
		daslist)
      (cdr ret)))
  (define-method (insert! list pos element)
    (if (= 0 pos)
	(this->cons element list)
	(let ((f (drop list (- pos 1))))
	  (set-cdr! f (this->cons element (cdr f)))
	  list)))
  (define (returnalot alot)
    (if (and (not (null? alot)) (pair? alot))
	(let ((temp #f))
	  (for-each (lambda (a) (returnalot a)) alot)
	  (while (not (null? alot))
		 (set! temp alot)
		 (set! alot (cdr alot))
		 (put temp)))))
  
  (define-method (returnalot alot)
    (c-display "pool-length:" (length pool))
    ;(c-display "inserting: " alot)
    (returnalot alot)
    (c-display "pool-length:" (length pool))
    ;(newline)
    )

)


;;##############################################################
;; Nodeline
;;##############################################################


(define-class (<nodeline> dasnodes linefunc textfunc changefunc)

  (define nodes (map list-copy dasnodes))

  (define lines '())
  (define boxes '())

  (define minx 0)
  (define maxx 1)
  (define miny 0)
  (define maxy 0)
  (define proportion 1)

  (var boxsize 0.04)

  (define (gfx-> x)
    (c-scale x 0 1 minx maxx))
  (define (gfy-> x)
    (c-scale x 0 1 miny maxy))

  (define (<-gfx x)
    (c-scale x minx maxx 0 1))
  (define (<-gfy x)
    (c-scale x miny maxy 0 1))

  (define (for-each-node func)
    (let ((prev #f)
	  (next #f)
	  (i 0))
      (for-each (lambda (n)
		  (if prev
		      (func i
			    (car prev) (cadr prev)
			    (car n) (cadr n)))
		  (set! i (1+ i))
		  (set! prev n))
		nodes)))

  (define (for-each-box func)
    (for-each (lambda (n)
		(apply func n))
	      boxes))

  (define (for-each-line func)
    (for-each (lambda (n)
		(apply func n))
	      lines))

  (define (get-last-line)
    (car lines))


  (define (nodes-partly minx maxx func)
    (for-each-node (lambda (i x1 y1 x2 y2)
		     (if (and (< x1 maxx) (> x2 minx))
			 (let ((nx1 x1)
			       (nx2 x2)
			       (ny1 y1)
			       (ny2 y2))
			   (if (< nx1 minx)
			       (begin
				 (set! nx1 minx)
				 (set! ny1 (c-scale nx1 x1 x2 y1 y2))))
			   (if (< ny1 miny)
			       (begin
				 (set! ny1 miny)
				 (set! nx1 (c-scale ny1 y1 y2 x1 x2))))
			   (if (> nx2 maxx)
			       (begin
				 (set! nx2 maxx)
				 (set! ny2 (c-scale nx2 x1 x2 y1 y2))))
			   (if (> ny2 maxy)
			       (begin
				 (set! ny2 maxy)
				 (set! nx2 (c-scale ny2 y1 y2 x1 x2))))
			   (func i nx1 ny1 nx2 ny2))))))



  ;; (define-method* (get-graph ...)
  (define-method* (get-graph #:optional start end)
    (if (not start)
	nodes
	(let ((ret '()))	
	  (nodes-partly start end
			(lambda (i x1 y1 x2 y2)
			  (if (null? ret)
			      (set! ret (list (list x2 y2) (list x1 y1)))
			      (set! ret (cons (list x2 y2) ret)))))
	  (reverse! ret))))


  (define-method (get-val x)
    (let ((first nodes)
	  (next #f))
      (while (< (caadr first) x)
	     (set! first (cdr first)))
      (set! next (cadr first))
      (set! first (car first))
      (c-scale x (car first) (car next) (cadr first) (cadr next))))
      

  (define-method (set-graph! graph)
    (if (not (= (length graph) (length nodes)))
	(begin
	  (changefunc this)
	  (set! nodes (map list-copy graph))
	  (make-lines-and-boxes)
	  (this->paint))
	(let ((start-not-set #t)
	      (start 0)
	      (end 0))
	  (c-for-each (lambda (n a b)
			(let ((eq (equal? a b)))
			  (if start-not-set
			      (begin
				(set! end n)
				(if (not eq)
				    (begin
				      (set! start n)
				      (set! start-not-set #f))))
			      (if (not eq)
				  (set! end n)))))
		      graph nodes)
	  (paint-some start (1+ end))
	  (set! nodes (map list-copy graph))
	  (make-lines-and-boxes)
	  (paint-some start (1+ end)))))

				  
				  
  (define (get-node x y)
    (call-with-current-continuation
     (lambda (return)
       (for-each-box (lambda (i x1 y1 x2 y2)
		       (if (and (>= x x1)
				(< x x2)
				(>= y y1)
				(< y y2))
			   (return i))))
       #f)))


  (define (perhaps-make-node x y)
    (define (square x)
      (* x x))
    (define (distance2 x1 y1 x2 y2)
      (+ (square (- x1 x2)) (square (- y1 y2))))
    (call-with-current-continuation
     (lambda (return)
       (for-each-node (lambda (i x1 y1 x2 y2)
			(if (and (>= x x1)
				 (<= x x2))
			    (let* ((gx (<-gfx x))
				   (gx1 (<-gfx x1))
				   (gx2 (<-gfx x2))
				   (a2 (distance2 gx1 y1 gx y))
				   (b2 (distance2 gx y gx2 y2))
				   (c2 (distance2 gx1 y1 gx2 y2))
				   (F2 (/ (- (* 4 a2 b2)
					     (square (- (+ a2 b2) c2)))
					  16))
				   (hc2 (* 4 (/ F2 c2))))
			      (return (if (< hc2 (square this->boxsize))
					  (let ((node (list x y)))
					    (changefunc this)
					    (set! nodes (insert! nodes i node))
					    (make-lines-and-boxes)
					    (this->paint)
					    node)
					  #f)))))))))


  (define (make-lines-and-boxes)
    (set! lines '())
    (set! boxes '())
    (nodes-partly minx maxx
		  (lambda (i x1 y1 x2 y2)
		    (set! lines (cons (list i
					    (<-gfx x1)
					    (<-gfy y1)
					    (<-gfx x2)
					    (<-gfy y2))
				      lines))))
    (for-each-node (lambda (i x1 y1 x2 y2)
		     (let ((makebox (lambda (i x y)
				      (if (and (<= x maxx) (>= x minx))
					  (let ((nx (<-gfx x))
						(ny (<-gfy y))
						(ax (* this->boxsize proportion)))
					    (set! boxes (cons (list i
								    (- nx ax)
								    (- ny this->boxsize)
								    (+ nx ax)
								    (+ ny this->boxsize))
							      boxes)))))))
		       (if (= i 1)
			   (makebox 0 x1 y1))
		       (makebox i x2 y2)))))
  


  (define (paint-some start end)
    (for-each-box (lambda (n x1 y1 x2 y2)
		    (if (and (>= n start)
			     (<= n end))
			(begin
			  (linefunc x1 y1 x2 y1)
			  (linefunc x2 y1 x2 y2)
			  (linefunc x2 y2 x1 y2)
			  (linefunc x1 y2 x1 y1)))))
    (for-each-line (lambda (n x1 y1 x2 y2)
		     (if (and (>= n start)
			      (<= n end))
			 (begin
			   (linefunc x1 y1 x2 y2)
			   (textfunc y1 (+ x1 (/ this->boxsize 2)) (- y1 this->boxsize))
			   (if (= n (car (get-last-line)))
			       (textfunc y2 (+ x2 (/ this->boxsize 2)) (- y2 this->boxsize))))))))
  
  
  (define-method (paint)
    (paint-some 0 (length nodes)))


  ;; The dasm(in|ax)(x|y) variables defines the range of the whole graph that is showed
  ;; in the current display.

  (define-method (set-bounds! dasminx dasmaxx dasminy dasmaxy dasproportion)
    (set! minx dasminx)
    (set! maxx dasmaxx)
    (set! miny dasminy)
    (set! maxy dasmaxy)
    (set! proportion dasproportion)
    (make-lines-and-boxes))



  ;; The x values for the mousefunctions are between 0 and 1 in the current display.

  (define pressednode #f)
  (define prevnode #f)
  (define nextnode #f)
  (define pressednodenum 0)
  (define x_press 0)
  (define y_press 0)
  (define x_press_offset 0)
  (define y_press_offset 0)
  (define (maixy x)
    (max 0 (min 1 x)))
  (define direction #f)


  (define (delete-node nodenum)
    (changefunc this)
    (set! nodes (delete! (list-ref nodes nodenum) nodes eq?))
    (make-lines-and-boxes)
    (this->paint)
    'stop!)
  
  (define-method (mouse-clicked x y button stat)
    (if (and pressednode
	     (c-rightbutton? button)
	     (> pressednodenum 0)
	     (< pressednodenum (1- (length nodes))))
	(delete-node pressednodenum)))

  (define-method (mouse-press x y button stat)
    (if (and (c-shift? stat)
	     (c-rightbutton? button))
	(let ((nodenum (get-node (maixy x) (maixy y))))
	  (if (and nodenum
		   (> nodenum 0)
		   (< nodenum (1- (length nodes))))
	      (delete-node nodenum)))
	(let ((func (lambda ()
		      (let ((nodenum (get-node (maixy x) (maixy y))))
			(if nodenum
			    (begin
			      (set! pressednodenum nodenum)
			      (if (> nodenum 0)
				  (set! prevnode (list-ref nodes (1- nodenum))))
			      (set! pressednode (list-ref nodes nodenum))
			      (if (< nodenum (1- (length nodes)))
				  (set! nextnode (list-ref nodes (1+ nodenum))))
			      (set! x_press x)
			      (set! y_press y)
			      (set! x_press_offset (- (<-gfx (car pressednode)) x))
			      (set! y_press_offset (- (cadr pressednode) y))
			      (set! direction (if (c-rightbutton? button) 'not-set #f))
			      'stop!)
			    #f)))))
	  (if (not (func))
	      (if (perhaps-make-node (gfx-> x) y)
		  (func))
	      'stop!))))

  (define-method (mouse-move x_org y_org button stat)
    (if pressednode
	(let ((y (+ y_press_offset (if (c-ctrl? stat) (+ y_press (/ (- y_org y_press) 12)) y_org)))
	      (x (+ x_press_offset (if (c-ctrl? stat) (+ x_press (/ (- x_org x_press) 12)) x_org)))
	      (minx2 (if prevnode (if nextnode (car prevnode) 1) 0))
	      (maxx2 (if nextnode (if prevnode (car nextnode) 0) 1)))
	  (if direction
	      (begin
		(if (eq? direction 'not-set)
		    (set! direction (if (> (abs (- x_org x_press)) (abs (- y_org y_press)))
					'x
					'y)))
		(if (eq? direction 'y)
		    (set! x (+ x_press_offset x_press))
		    (set! y (+ y_press_offset y_press)))))
	  (paint-some  pressednodenum (1+ pressednodenum))
	  (set-car! pressednode (max minx2 (min maxx2 (gfx-> x))))
	  (set-car! (cdr pressednode) (if (c-shift? stat) 0.5 (maixy y)))
	  (make-lines-and-boxes)
	  (paint-some pressednodenum (1+ pressednodenum))
	  (c-gc-on) ;; To avoid crashing the machine, actually.
	  'stop!)))

  (define-method (mouse-release x y button stat)
    (if pressednode
	(begin
	  (this->mouse-move x y button stat)
	  (set! prevnode #f)
	  (set! pressednode #f)
	  (set! nextnode #f)
	  'stop!)))


  )
		     
		  


(define-class (<editor-nodeline> snd ch orgval #:optional string-func moused-func (context mark-context))
  
  (Super (<nodeline> (begin
		       (if (or (< orgval 0) (> orgval 1))
			   (begin
			     (c-display "Warning! gui.scm/<editor-nodeline>: orgval=" orgval)
			     (set! orgval (max 0 (min orgval 1)))))
		       (list (list 0 orgval) (list 1 orgval)))
		     (lambda (x1 y1 x2 y2)
		       (c-get-bounds snd ch
				     (lambda (minx miny maxx maxy)
				       (draw-line (c-scale x1 0 1 minx maxx)
						  (c-scale y1 0 1 miny maxy)
						  (c-scale x2 0 1 minx maxx)
						  (c-scale y2 0 1 miny maxy)
						  snd
						  ch
						  context))))
		     (lambda (val x y)
		       (if string-func
			   (c-get-bounds snd ch
					 (lambda (minx miny maxx maxy)
					   (c-draw-string (c-editor-widget snd)
							  (list-ref (snd-gcs) 3)
							  (min (- maxx 20) (c-scale x 0 1 minx maxx))
							  (max 20 (c-scale y 0 1 miny maxy))
							  (string-func val))))))
		     (lambda (this)
		       (-> this paint))))

  (define active #t)

  (define-method (set-inactive)
    (-> this paint)
    (set! visible #f))

  (define-method (set-active)
    (-> this paint)
    (set! visible #t))

  (define-method (is-active?)
    active)

  (define (das-after-graph-hook dassnd dasch)
    (if (and active (= snd dassnd) (= ch dasch))
	(let ((length (/ (frames snd ch) (srate snd)))
	      (minx (car (x-bounds snd ch)))
	      (maxx (cadr (x-bounds snd ch)))
	      (miny (car (y-bounds snd ch)))
	      (maxy (cadr (y-bounds snd ch))))
	  (-> this set-bounds!
	      (c-scale minx 0 length 0 1)
	      (c-scale maxx 0 length 0 1)
	      (c-scale miny -1 1 0 1)
	      (c-scale maxy -1 1 0 1)
	      (/ (/ (window-height) (chans snd)) (window-width)))
	  (-> this paint)))
    #f)


  (define mouse-cycle (<mouse-cycle> (lambda (dassnd dasch x y button stat)
				       (if (and active (= snd dassnd) (= ch dasch))
					   (if (eq? 'stop! (-> this mouse-press x y button stat))
					       (begin
						 (if moused-func (moused-func this))
						 'stop!))))
				     (lambda (snd ch x y button stat)
				       (-> this mouse-move x y button stat)
				       (if moused-func (moused-func this)))
				     (lambda (snd ch x y button stat isdragged)
				       (if isdragged
					   (-> this mouse-release x y button stat)
					   (-> this mouse-clicked x y button stat))
				       (if moused-func (moused-func this)))
				     
				     #:scaled #t))

  (define (das-close-hook dassnd)
    (if (= dassnd snd)
	(this->delete!)))


  (define-method (delete!)
    (remove-hook! after-graph-hook das-after-graph-hook)
    (-> mouse-cycle delete!)
    (remove-hook! close-hook das-close-hook))


  (add-hook! after-graph-hook das-after-graph-hook)
  (add-hook! close-hook das-close-hook)

  (das-after-graph-hook snd ch)

  )


#!
(c-for 0 < 0.9 0.1
       (lambda (n)
	 (<editor-nodeline> (selected-sound) 0 n)))
!#




;;##############################################################
;; Menues
;;##############################################################

(define* (menu-sub-add menu menu-label #:optional callback)
  (let ((dasmenu (if (integer? menu) (main-menu menu) menu)))
    (if use-gtk
	(let ((menuitem (gtk_menu_item_new_with_label menu-label))
	      (submenu (gtk_menu_new)))
	  (gtk_menu_shell_append (GTK_MENU_SHELL dasmenu) menuitem)
	  (gtk_widget_show menuitem)
	  (gtk_menu_item_set_submenu (GTK_MENU_ITEM menuitem) submenu)
	  (if callback
	      (g_signal_connect_closure_by_id 
	       (GPOINTER menuitem)
	       (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT menuitem))) 0
	       (g_cclosure_new (lambda (w d) 
				 (callback))
			       #f #f)
	       #f))
	  submenu)
	(let* ((submenu (XmCreatePulldownMenu dasmenu menu-label
					      (list XmNbackground (basic-color))))
	       (menuitem (XtCreateManagedWidget menu-label
						xmCascadeButtonWidgetClass dasmenu
						(list XmNsubMenuId submenu
						      XmNbackground (basic-color)))))
	  (if callback
	      (XtAddCallback menuitem XmNcascadingCallback (lambda (w c i) (callback))))
	  submenu))))



 (define* (menu-add top-menu menu-label callback #:optional position)
  (if (integer? top-menu)
      (if position
	  (add-to-menu top-menu menu-label callback position)
	  (add-to-menu top-menu menu-label callback))
      (if use-gtk
	  (let ((child (gtk_menu_item_new_with_label menu-label)))
	    (gtk_menu_shell_append (GTK_MENU_SHELL top-menu) child)
	    (gtk_widget_show child)
	    ;(set-procedure-property! das 'arity '(2 0 #f))
	    ;;(set-procedure-properties! das '((arity 2 0 #f)))
	    ;;(c-display "prop: " (procedure-properties das))
	    (g_signal_connect_closure_by_id 
	     (GPOINTER child)
	     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
	     (g_cclosure_new (lambda (w d)
			       (callback))
					;das
					; (callback))
			     #f #f)
	     #f)
	    child)
	  (let ((child (XtCreateManagedWidget menu-label xmPushButtonWidgetClass top-menu
					      (list XmNbackground (basic-color)))))
	    (XtAddCallback child XmNactivateCallback
			   (lambda (w c i)
			     (callback)))
	    child))))


(define (menu-set-label menu label)
  (if use-gtk
      (gtk_label_set_text (GTK_LABEL (gtk_bin_get_child (GTK_BIN menu))) label)
      (let ((str (XmStringCreateLocalized label)))
	(XtSetValues menu (list XmNlabelString str))
	(XmStringFree str))))



#!
;; Menu-test
(let ((test-menu (add-to-main-menu "testing")))
  (define (adding menu n)
    (if (> n 0)
	(let ((submenu (menu-sub-add menu (format #f "Submenu ~D" n))))
	  (menu-add menu "MenuItem" (lambda () (display n)))
	  (adding submenu (- n 1)))))
  (adding test-menu 10))
!#






;;##############################################################
;; Checkbuttons
;;##############################################################

(define-class (<checkbutton> parent name callback #:optional onoff (extraopts '()))

  (var button #f)

  (define-method (set to)
    (if use-gtk
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON this->button) to)
	(XtSetValues this->button (list XmNset to))))

  (define-method (remove)
    (if use-gtk
	(hide-widget (GTK_WIDGET this->button))
	;;(gtk_widget_destroy (GTK_WIDGET button))
	(XtUnmanageChild this->button)))
    
  (if use-gtk
      (let ((dasparent (if (isdialog? parent) (-> parent getbox2) (GTK_BOX parent))))
	(set! this->button (if name (gtk_check_button_new_with_label name) (gtk_check_button_new)))
	(gtk_box_pack_end (GTK_BOX dasparent) this->button #f #f 0)
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON this->button) onoff)
	(gtk_widget_show this->button)
	(g_signal_connect_closure_by_id 
	 (GPOINTER this->button)
	 (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT this->button))) 0
	 (g_cclosure_new (lambda (w d) 
			   (callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w))))
			 #f #f)
	 #f))
      (let* ((dasparent (if (isdialog? parent) (-> parent getbox2) parent)))
	(set! this->button (XtCreateManagedWidget (if name name "") xmToggleButtonWidgetClass dasparent
					    (append (list XmNbackground       (basic-color)
							  ;;XmNlabelString      name
							  XmNset              onoff
							  XmNselectColor      (yellow-pixel))
						    extraopts)))
	(XtAddCallback this->button XmNvalueChangedCallback (lambda (w c i) (callback (.set i)))))))



(define (checkbutton-get button)
  (if use-gtk
      (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON button))
      (c-display "checkbutton-get not implemented for motif.")))

(define (checkbutton-set button to)
  (if use-gtk
      (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) to)
      (XtSetValues button (list XmNset to))))

(define (checkbutton-remove button)
  (if use-gtk
      (hide-widget (GTK_WIDGET button))
      ;;(gtk_widget_destroy (GTK_WIDGET button))
      (XtUnmanageChild button)))





;;##############################################################
;; Buttons
;;##############################################################

(define-class (<button> parent name callback)

  (var button #f)

  (if use-gtk
      (begin
	(set! this->button (gtk_button_new_with_label name))
	(gtk_box_pack_start (GTK_BOX parent) this->button #t #t 20)
	(g_signal_connect_closure_by_id (GPOINTER this->button)
					(g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT this->button)))
					0 (g_cclosure_new (lambda (w data) 
							    (callback))
							  #f #f) #f)
	(gtk_widget_show this->button))
      (begin
	(set! this->button (XtCreateManagedWidget name xmPushButtonWidgetClass parent
					     (list XmNbackground (basic-color)
						   XmNarmColor   (pushed-button-color))))
	(XtAddCallback this->button XmNactivateCallback (lambda (w c i)
							    (callback))))))





;;##############################################################
;; Togglebuttons
;;##############################################################

(define-class (<togglebutton> parent name callback #:optional onoff (extraopts '()))

  (var button #f)

  (define-method (set to)
    (if use-gtk
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON this->button) to)
	(XtSetValues this->button (list XmNset to))))

  (define-method (remove)
    (if use-gtk
	(hide-widget (GTK_WIDGET this->button))
	;;(gtk_widget_destroy (GTK_WIDGET button))
	(XtUnmanageChild this->button)))
    
  (if use-gtk
      (let ((dasparent (if (isdialog? parent) (-> parent getbox2) (GTK_BOX parent))))
	(set! this->button (gtk_toggle_button_new_with_label name))
	(gtk_box_pack_end (GTK_BOX dasparent) this->button #f #f 0)
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON this->button) onoff)
	(gtk_widget_show this->button)
	(g_signal_connect_closure_by_id 
	 (GPOINTER this->button)
	 (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT this->button))) 0
	 (g_cclosure_new (lambda (w d) 
			   (if (= 1 (car (procedure-property callback 'arity)))
			       (callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w)))
			       (callback this (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w)))))
			 #f #f)
	 #f))
      (let* ((dasparent (if (isdialog? parent) (-> parent getbox2) parent)))
	(set! this->button (XtCreateManagedWidget (if name name "") xmToggleButtonWidgetClass dasparent
					    (append (list XmNbackground       (basic-color)
							  ;;XmNlabelString      name
							  XmNset              onoff
							  XmNselectColor      (yellow-pixel))
						    extraopts)))
	(XtAddCallback this->button XmNvalueChangedCallback (lambda (w c i) (callback (.set i)))))))



;;##############################################################
;; Sliders
;;##############################################################

(define-class (<slider> parent
			 title
			 low initial high
			 func
			 scaler
			 #:optional autofunc use-log)

  (define hbox #f)
  (define label #f)
  (define scale #f)
  (define slider #f)

  (define-method (delete!)
    (gtk_widget_destroy (GTK_WIDGET scale))
    (gtk_widget_destroy (GTK_WIDGET label))
    (gtk_widget_destroy (GTK_WIDGET hbox)))

  (define-method (set! val)
    (gtk_adjustment_set_value (GTK_ADJUSTMENT slider) val))
			     
  (if use-gtk
      (let* ((vbox (if (isdialog? parent) (-> parent getbox1) parent))
	     (adj (if use-log 
		      (gtk_adjustment_new (scale-log->linear low initial high) 0 log-scale-ticks 1 10 1)
		      (gtk_adjustment_new initial low high 0.0 0.0 0.0))))

	(set! label (gtk_label_new (if use-log
				       (format #f "~A (~,2F)" title initial)
				       (format #f "~A" title))))

	(set! hbox (gtk_hbox_new #f 0))

	(set! scale (gtk_hscale_new (GTK_ADJUSTMENT adj)))

	(if autofunc
	    (<checkbutton> hbox #f autofunc))

	(gtk_box_pack_start (GTK_BOX vbox) hbox #f #f 2)
	(gtk_widget_show hbox)
	(gtk_box_pack_start (GTK_BOX hbox) label #f #f 6)
	(gtk_widget_show label)
	(gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE scale)) GTK_UPDATE_CONTINUOUS)
	(gtk_scale_set_digits (GTK_SCALE scale)
			      (if use-log
				  0
				  (if (= scaler 1000) 3 (if (= scaler 100) 2 (if (= scaler 10) 1 0)))))
	(gtk_scale_set_draw_value (GTK_SCALE scale) (not use-log))
	(gtk_widget_show scale)
	
	(gtk_box_pack_start (GTK_BOX hbox) scale #t #t 0)
	(if use-log
	    (g_signal_connect_closure_by_id (GPOINTER adj)
					    (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj))) 0
					    (g_cclosure_new (lambda (w d) 
							  (func (.value (GTK_ADJUSTMENT adj)))
							  (change-label label 
									(format #f "~A: ~,2F" 
										title 
										(scale-log-label low (.value (GTK_ADJUSTMENT adj)) high))))
							    #f #f)
					    #f)
	    (g_signal_connect_closure_by_id (GPOINTER adj)
					    (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj))) 0
					    (g_cclosure_new (lambda (w d) (func (.value (GTK_ADJUSTMENT adj)))) #f #f)
					    #f))
	(set! slider adj))
      (let* ((mainform (if (isdialog? parent) (-> parent getbox1) parent))
	     (dastitle (XmStringCreate title XmFONTLIST_DEFAULT_TAG))
	     (new-slider (XtCreateManagedWidget title xmScaleWidgetClass mainform
						(list XmNorientation   XmHORIZONTAL
						      XmNshowValue     #t
						      XmNminimum       (c-integer (* low scaler))
						      XmNmaximum       (c-integer (* high scaler))
						      XmNvalue         (c-integer (* initial scaler))
						      XmNdecimalPoints (if (= scaler 1000) 3 (if (= scaler 100) 2 (if (= scaler 10) 1 0)))
						      XmNtitleString   dastitle
						      XmNleftAttachment XmATTACH_FORM
						      XmNrightAttachment XmATTACH_FORM
						      XmNbackground    (basic-color)
						      ))))
	
	(XmStringFree dastitle)
	(XtAddCallback new-slider XmNvalueChangedCallback (lambda (w c info) (func (/ (.value info) scaler))))
	(XtAddCallback new-slider XmNdragCallback (lambda (w c info) (func (/ (.value info) scaler))))
	(set! slider new-slider))))







;;##############################################################
;; Dialogs
;;##############################################################

(define (isdialog? dialog)
  (and (object? dialog)
       (instance? dialog <dialog>)))

(define-class (<dialog> label deletefunc . buttons)

  (define box1 #f)
  (define box2 #f)

  (define wassoc (list (list 'Close "quit_button" (quit-button-color))
		       (list 'Help "help_button" (help-button-color))
		       (list 'Apply "doit_button" (doit-button-color))
		       (list 'Dismiss "quit_button" (quit-button-color))
		       (list 'Ok "doit_button" (doit-button-color))))


  (var dialog #f)
  (var sliders #f)

  (define-method (getbox2)
    (if (not box2)
	(let ((hbox #f))
	  (if use-gtk
	      (begin
		(set! hbox (gtk_hbox_new #f 0))
		(gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG this->dialog))) hbox #f #f 4)
		(gtk_widget_show hbox))
	      (let* ((mainform box1)
		     (sep (XtCreateManagedWidget "sep" xmSeparatorWidgetClass mainform
						 (list XmNorientation      XmHORIZONTAL
						       XmNseparatorType    XmSHADOW_ETCHED_OUT
						       XmNbackground       (basic-color))))
		     (rc (XtCreateManagedWidget "rc"  xmRowColumnWidgetClass mainform
						(list XmNorientation      XmHORIZONTAL
						      XmNbackground       (basic-color)
						      XmNradioBehavior    #f
						      XmNradioAlwaysOne   #t
						      XmNbottomAttachment XmATTACH_FORM
						      XmNleftAttachment   XmATTACH_FORM
						      XmNrightAttachment  XmATTACH_FORM
						      XmNentryClass       xmToggleButtonWidgetClass
						      XmNisHomogeneous    #t))))
		(set! hbox rc)))
	  (setbox2! hbox)))
    box2)

  (define (setbox2! dashbox)
    (set! box2 dashbox))

  (define-method (getbox1)
    (if (not box1)
	(let ((vbox #f))
	  (if use-gtk
	      (begin
		(set! vbox (gtk_vbox_new #f 2))
		(gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG this->dialog))) vbox #f #f 4)
		(gtk_widget_show vbox))
	      (set! vbox (XtCreateManagedWidget "formd" xmRowColumnWidgetClass this->dialog
						(list XmNleftAttachment      XmATTACH_FORM
						      XmNrightAttachment     XmATTACH_FORM
						      XmNtopAttachment       XmATTACH_FORM
						      XmNbottomAttachment    XmATTACH_WIDGET
						      XmNbottomWidget        (XmMessageBoxGetChild this->dialog XmDIALOG_SEPARATOR)
						      XmNbackground          (highlight-color)
						      XmNorientation         XmVERTICAL))))
	  (setbox1! vbox)))
    box1)

  (define (setbox1! dasvbox)
    (set! box1 dasvbox))

  (define-method (hide)
    (if use-gtk
	(gtk_widget_hide this->dialog)
	(XtUnmanageChild this->dialog))
    (focus-widget (c-editor-widget (selected-sound))))

  (define-method (show)
    (if use-gtk
	(begin
	  (gtk_widget_show this->dialog)
	  (gdk_window_raise (.window this->dialog)))
	(if (not (XtIsManaged this->dialog))
	    (XtManageChild this->dialog)
	    (raise-dialog this->dialog))))


  ;; Replacement for add-sliders in new-effects.scm/gtk-effects.scm
  (define-method (add-sliders dassliders)
    (set! this->sliders (map
			 (lambda (slider-data)
			   (apply <slider> (cons (this->getbox1) slider-data)))
			 dassliders))
    
    (if (not use-gtk)
	(let ((num_inputs (+ 1 (length this->sliders))))
	  (set! (widget-size this->dialog) (list (min 800 (max 400 (* num_inputs 20)))
						   (min 800 (max 120 (* num_inputs 70)))))))
    
    this->sliders)


  (let ((names '())
	(funcs '())
	(wnames '())
	(new-dialog #f))

    (if use-gtk
	(begin
	  (set! new-dialog (gtk_dialog_new))
	  (gtk_window_set_title (GTK_WINDOW new-dialog) label)
	  (gtk_container_set_border_width (GTK_CONTAINER new-dialog) 10)
	  (gtk_window_set_default_size (GTK_WINDOW new-dialog) -1 -1)
	  (gtk_window_set_resizable (GTK_WINDOW new-dialog) #t)
	  (gtk_widget_realize new-dialog)
	  
	  (g_signal_connect_closure_by_id (GPOINTER new-dialog)
					  (g_signal_lookup "delete_event" (G_OBJECT_TYPE (GTK_OBJECT new-dialog)))
					  0 (g_cclosure_new (lambda (w ev data)
							      (if deletefunc (deletefunc))
							      (gtk_widget_hide new-dialog)
							      (focus-widget (c-editor-widget (selected-sound))))
							    #f #f) #f))
	(let ((titlestr (XmStringCreate label XmFONTLIST_DEFAULT_TAG)))
	  (set! new-dialog
		(XmCreateTemplateDialog (cadr (main-widgets)) label
					(list XmNautoUnmanage        #f
					      XmNdialogTitle         titlestr
					;XmNresizePolicy        XmRESIZE_GROW
					      XmNnoResize            #f
					      XmNbackground          (basic-color)
					      XmNtransient           #f)))
	  (XtAddCallback new-dialog XmNcancelCallback (lambda (w c i)
							(if deletefunc (deletefunc))
							(XtUnmanageChild new-dialog)
							(focus-widget (c-editor-widget (selected-sound)))))
	  (XmStringFree titlestr)))
    
    
    (for-each
     (lambda (e)
       (if (procedure? e)
	   (set! funcs (cons e funcs))
	   (begin
	     (set! names (cons e names))
	     (set! wnames (cons (if (assoc (string->symbol e) wassoc) (assoc (string->symbol e) wassoc) (list #f "noname")) wnames)))))
     buttons)
    
    (for-each
     (lambda (name func wname)
       (let ((button ((if (> (car (procedure-property func 'arity)) 0)
			  <togglebutton>
			  <button>)
		      (if use-gtk
			  (.action_area (GTK_DIALOG new-dialog))
			  new-dialog)
		      name 
		      func)))
	 (if use-gtk
	     (gtk_widget_set_name (-> button button) (cadr wname))
	     (if (car wname)
		 (XtVaSetValues
		  (-> button button)
		  (list XmNarmColor   (pushed-button-color)
			XmNbackground (caddr wname)))))))
     
     (reverse names) (reverse funcs) (reverse wnames))
    
    ;; build rest in (.vbox (GTK_DIALOG new-dialog))
    (set! this->dialog new-dialog)))
    






;;##############################################################
;; GUI test
;;##############################################################

#!
(define d (<dialog> "gakk"  #f
		    "Close" (lambda () (-> d hide))
		    "Apply" (lambda () (display "apply"))
		    "Play" (lambda (onoroff) (c-display "play" onoroff))
		    "Help" (lambda () (display "help"))))
  
(<slider> d "slider1" 0 1 2 (lambda (val) (display val)(newline)) 100)
(<slider> d "slider2" 0 0.2 1 (lambda (val) (display val)(newline)) 1000)
(<slider> d "slider3" 0 1 20 (lambda (val) (display val)(newline)) 1)
(<checkbutton> d "checkbutton1" (lambda (a) (display a)(newline)))
(<checkbutton> d "checkbutton2" (lambda (a) (display a)(newline)))
(<checkbutton> d "checkbutton3" (lambda (a) (display a)(newline)))
(<togglebutton> d "togglebutton1" (lambda (a) (display a)(newline)))
(-> d show)
(-> d hide)
!#
