
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
	     (srfi srfi-1))


;;(use-modules (oop goops))





;;##############################################################
;; Various functions
;;##############################################################

(define use-gtk (if (provided? 'snd-gtk)
		    #t
		    #f))

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


;; C-like for-iterator
(define (c-for init pred least add proc)
  (do ((n init (+ n add)))
      ((not (pred n least)))
    (proc n)))

#!
(define (c-for init pred least add proc)
  (let ((n init))
    (while (pred n least)
	   (proc n)
	   (set! n (+ add n)))))
!#

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

(define (c-scale x x1 x2 y1 y2)
  (+ y1
     (/ (* (- x x1)
	   (- y2 y1))
	(- x2 x1))))

;; Snd has its own filter function (a clm function) overriding the guile filter function.
(define (filter-org pred list)
  (remove (lambda (e) (not (pred e)))
	  list))

(define (insert! list pos element)
  (call-with-values (lambda () (split-at! list pos))
    (lambda (f s)
      (append! f (cons element s)))))

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
	(fd (open-file sourcefile "w")))
    (for-each (lambda (s)
		(write-line s fd))
	      (if (eq? (car codestrings) '#:compile-options)
		  (cddr codestrings)
		  codestrings))
    (close fd)
    (system (string-append "gcc -Wall -O2 -shared -o " libfile " " sourcefile " " compile-options))
    (dynamic-call "das_init" (dynamic-link libfile))
    (system (string-append "rm " libfile " " sourcefile))))


;; define-toplevel is like define, but at the toplevel.
(c-eval-c
"
#include <libguile.h>
void das_init(){
  scm_c_define_gsubr(\"define-toplevel\",2,0,0,scm_define);
}
")


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



;; A function to remove the default motion_notify_event handler for gtk.

(define (c-remove-motionhandler widget)
(if (not use-gtk)
    (c-display "c-remove-motionhandler not implemented for motif")
    (begin
      (c-eval-c #:compile-options "\`pkg-config --cflags glib-2.0\`"
"
#include <libguile.h>
#include <glib.h>
#include <glib-object.h>
SCM das_func(SCM w){
  gpointer g=(gpointer)scm_num2ulong(SCM_CAR(SCM_CDR(w)),0,\"remove-motionhanders\");
  g_signal_handler_disconnect(g,g_signal_handler_find(g,
						      G_SIGNAL_MATCH_ID,
						      g_signal_lookup(\"motion_notify_event\",G_OBJECT_TYPE(g)),
						      0,0,0,0));
  return SCM_UNSPECIFIED;
}
void das_init(){
  scm_c_define_gsubr(\"c-remove-motionhandler\",1,0,0,das_func);
}
")
      (c-remove-motionhandler widget))))



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
      (let ((funcname (symbol-append '-> method (gensym))))
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
;; Mouse-hooks for the channel widgets.
;;##############################################################


(define mouse-button-press-hook (make-hook 4))
(define mouse-move-hook (make-hook 4))
(define mouse-drag2-hook (make-hook 4))
(define mouse-button-release-hook (make-hook 4))


(add-hook! after-open-hook
	   (lambda (snd)
	     (let ((w (list-ref (channel-widgets snd 0) 0)))
	       (if (not use-gtk)	     
		   (begin
		     (XtAddEventHandler w ButtonPressMask #f 
					(lambda (w c e f)
					  (run-hook mouse-button-press-hook snd (.x e) (.y e) (.state e))))
		     (XtAddEventHandler w ButtonMotionMask #f 
					(lambda (w c e f)
					  (run-hook mouse-drag2-hook snd (.x e) (.y e) (.state e))))
		     (XtAddEventHandler w ButtonReleaseMask #f 
					(lambda (w c e f)
					  (run-hook mouse-button-release-hook snd (.x e) (.y e) (.state e)))))
		   (let ((ispressed #f))
		     (g_signal_connect w "button_press_event"
				       (lambda (w e i)
					 (set! ispressed #t)
					 (run-hook mouse-button-press-hook snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))))
		     (g_signal_connect w "motion_notify_event"
				       (lambda (w e i)
					 (let ((args (if (.is_hint (GDK_EVENT_MOTION e))
							 (cons snd (cdr (gdk_window_get_pointer (.window e))))
							 (list snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e))))))
					   (apply run-hook (cons mouse-move-hook args))
					   (if ispressed
					       (apply run-hook (cons mouse-drag2-hook args))))
					 #f))
		     (g_signal_connect w "button_release_event"
				       (lambda (w e i)
					 (set! ispressed #f)
					 (run-hook mouse-button-release-hook snd (.x (GDK_EVENT_BUTTON e)) (.y (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e))))))))))



(define mouse-click2-hook (make-hook 4))


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
;; Nodeline
;;##############################################################


(define-class (<nodeline> dasnodes linefunc boxfunc clearfunc)

  (define nodes (map list-copy dasnodes))

  (define lines '())
  (define boxes '())

  (define minx 0)
  (define maxx 1)
  (define miny 0)
  (define maxy 0)
  (define proportion 1)

  (var boxsize 0.02)

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
		(func (n 0) (n 1) (n 2) (n 3)))
	      boxes))

  (define-method (find-node x y xrange yrange)
    (let* ((x05 (- x (/ xrange 2)))
	   (y05 (- y (/ yrange 2)))
	   (ret (find (lambda (n)
			(let ((nx (car n))
			      (ny (cadr n)))
			  (and (>= (+ x05 xrange) nx) (<= x05 nx)
			       (>= (+ y05 yrange) ny) (<= y05 ny))))
		      nodes)))
      (if ret
	  (car ret)
	  #f)))

  (define (make-lines-and-boxes)
    (set! lines '())
    (set! boxes '())
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
			   (set! lines (cons (<array> (c-scale nx1 minx maxx 0 1)
						      (c-scale ny1 miny maxy 0 1)
						      (c-scale nx2 minx maxx 0 1)
						      (c-scale ny2 miny maxy 0 1))
					     lines))))))

    (for-each-node (lambda (i x1 y1 x2 y2)
		     (let ((makebox (lambda (x y)
				      (if (and (<= x maxx) (>= x minx))
					  (let ((nx (c-scale x minx maxx 0 1))
						(ny (c-scale y miny maxy 0 1))
						(ax (* this->boxsize proportion)))
					    (set! boxes (cons (<array> (- nx ax)
								       (- ny this->boxsize)
								       (+ nx ax)
								       (+ ny this->boxsize))
							      boxes)))))))
		       (if (= i 1)
			   (makebox x1 y1))
		       (makebox x2 y2)))))


  (define-method (set-bounds! dasminx dasmaxx dasminy dasmaxy dasproportion)
    (set! minx dasminx)
    (set! maxx dasmaxx)
    (set! miny dasminy)
    (set! maxy dasmaxy)
    (set! proportion dasproportion)
    (make-lines-and-boxes))


  (define-method (paint)
    (for-each (lambda (line)
		(linefunc (line 0) (line 1) (line 2) (line 3)))
	      lines)
    (for-each-box (lambda (x1 y1 x2 y2)
		    (linefunc x1 y1 x2 y1)
		    (linefunc x2 y1 x2 y2)
		    (linefunc x2 y2 x1 y2)
		    (linefunc x1 y2 x1 y1))))
  
  (define-method (add-node! x y)
    (let ((newnodes '())
	  (foundit #f))
      (for-each-node (lambda (i x1 y1 x2 y2)
		       (if (and (not foundit) (>= x x1) (<= x x2))
			   (begin
			     (set! newnodes (cons (list x1 x2) (list x y) newnodes))
			     (set! foundit #t))
			   (set! newnodes (cons (list x1 x2) newnodes)))
		       (if (= i (- (length nodes 1)))
			   (set! newnodes (cons (list y1 y2))))))
      (set! nodes (reverse newnodes))))

  (define ispressed #f)

  (define-method (mouse-press x y)
    (c-display x y)
    (set! ispressed (<array> x y)))

  (define-method (mouse-move x y)
    #t)

  (define-method (mouse-release x y)
    (if ispressed
	(begin
	  (set! ispressed #f))))

  )
		     
		  




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
	    (g_signal_connect_closure_by_id 
	     (GPOINTER child)
	     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
	     (g_cclosure_new 
	      (lambda (w d)
		(callback))
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

  (define button #f)

  (define-method (set to)
    (if use-gtk
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) to)
	(XtSetValues button (list XmNset to))))

  (define-method (remove)
    (if use-gtk
	(hide-widget (GTK_WIDGET button))
	;;(gtk_widget_destroy (GTK_WIDGET button))
	(XtUnmanageChild button)))
    
  (if use-gtk
      (let ((dasparent (if (isdialog? parent) (-> parent getbox2) (GTK_BOX parent))))
	(set! button (gtk_check_button_new_with_label name))
	(gtk_box_pack_end (GTK_BOX dasparent) button #f #f 0)
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) onoff)
	(gtk_widget_show button)
	(g_signal_connect_closure_by_id 
	 (GPOINTER button)
	 (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button))) 0
	 (g_cclosure_new (lambda (w d) 
			   (callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w))))
			 #f #f)
	 #f))
      (let* ((dasparent (if (isdialog? parent) (-> parent getbox2) parent)))
	(set! button (XtCreateManagedWidget name xmToggleButtonWidgetClass dasparent
					    (append (list XmNbackground       (basic-color)
							  ;;XmNlabelString      name
							  XmNset              onoff
							  XmNselectColor      (yellow-pixel))
						    extraopts)))
	(XtAddCallback button XmNvalueChangedCallback (lambda (w c i) (callback (.set i)))))))



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
;; Sliders
;;##############################################################

(define-class (<slider> parent
			 title
			 low initial high
			 func
			 scaler
			 #:optional use-log)

  (define slider #f)

  (if use-gtk
      (let* ((vbox (if (isdialog? parent) (-> parent getbox1) parent))
	     (label (gtk_label_new (if use-log
				       (format #f "~A (~,2F)" title initial)
				       (format #f "~A" title))))
	     (adj (if use-log 
		      (gtk_adjustment_new (scale-log->linear low initial high) 0 log-scale-ticks 1 10 1)
		      (gtk_adjustment_new initial low high 0.0 0.0 0.0)))
	     (hbox (gtk_hbox_new #f 0))
	     (scale (gtk_hscale_new (GTK_ADJUSTMENT adj))))

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
						      XmNminimum       (inexact->exact (floor (* low scaler)))
						      XmNmaximum       (inexact->exact (floor (* high scaler)))
						      XmNvalue         (inexact->exact (floor (* initial scaler)))
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
    (focus-widget (list-ref (channel-widgets (selected-sound) 0) 0)))

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
							      (if deletefunc (deletefunc new-dialog))
							      (gtk_widget_hide new-dialog)
							      (focus-widget (list-ref (channel-widgets (selected-sound) 0) 0)))
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
							(if deletefunc (deletefunc new-dialog))
							(XtUnmanageChild new-dialog)
							(focus-widget (list-ref (channel-widgets (selected-sound) 0) 0))))
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
       (let ((button (<button> (if use-gtk
					(.action_area (GTK_DIALOG new-dialog))
					new-dialog)
				    name (lambda () (func)))))
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
		    "Play" (lambda () (display "play"))
		    "Stop" (lambda () (display "stop"))
		    "Help" (lambda () (display "help"))))
  
(<slider> d "slider1" 0 1 2 (lambda (val) (display val)(newline)) 100)
(<slider> d "slider2" 0 0.2 1 (lambda (val) (display val)(newline)) 1000)
(<slider> d "slider3" 0 1 20 (lambda (val) (display val)(newline)) 1)
(<checkbutton> d "checkbutton1" (lambda (a) (display a)(newline)))
(<checkbutton> d "checkbutton2" (lambda (a) (display a)(newline)))
(<checkbutton> d "checkbutton3" (lambda (a) (display a)(newline)))
(-> d show)
(-> d hide)
!#




    
