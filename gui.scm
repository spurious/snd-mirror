
;;; Functions to help making various gui-things more convenient and without
;;; worrying about whether we use gtk or motif. (see ladspa.scm and snd_conffile.scm for examples of use)
;;; -KSM.


(use-modules (ice-9 optargs)
	     (ice-9 format)
	     (srfi srfi-1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OO  (Goops/cloos syntax is so ugly.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if #f
    (begin
      (use-modules (oop goops))
      (define instance?-old instance?)
      (define define-class-old define-class)
      (define define-method-old define-method)))



(define-macro (define-class def . body)
  (if (and #f (symbol? def))
      `(define-class-old ,def ,@body)
      `(define* ,def
	 (letrec ((methods (make-hash-table 256))
		  (supers '())
		  (super #f)
		  (add-method-do (lambda (name func)
				   (hashq-set! methods name func))))
	   (var class-name ',(car def))
	   (define-method (dir)
	     (append (cons class-name
			   (hash-fold (lambda (key value s) (cons key s)) '() 
				      methods))
		     (map (lambda (super) (-> super dir))
			  supers)))
	   (define-method (get-method name)
	     (or (hashq-ref methods name)
		 (any (lambda (super) (-> super get-method name))
		      supers)))
	   (define-method (instance? dasclass-name)
	     (or (eq? dasclass-name class-name)
		 (any (lambda (super) (-> super instance? dasclass-name))
		      supers)))
	   (define (this m . rest)
	     (let ((func (get-method m)))
	       (if func
		   (apply func rest)
		   (format #t "No such method: \"~A\" in class \"~A\".\n" m ',(car def)))))
	   ,@body
	   this))))

(define-macro (add-method nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda ,(cdr nameandvars) ,@body)))

(define-macro (add-method* nameandvars . body)
  `(add-method-do ',(car nameandvars) (lambda* ,(cdr nameandvars) ,@body)))

(define-macro (define-method nameandvars . body)
  `(define ,(car nameandvars)
     (add-method ,nameandvars ,@body)))

(define-macro (define-method* nameandvars . body)
  `(define ,(car nameandvars)
     (add-method* ,nameandvars ,@body)))

(define-macro (var name initial)
  `(define ,name
     (let ((inited #f))
       (if (not inited)
	   (begin
	     (add-method* (,name #:optional newval) (if newval (set! ,name newval) ,name))
	     (set! inited #t)))
       ,initial)))

(define (object? o)
  (and (procedure? o)
       (catch #t
	      (lambda ()
		(eq? (-> o instance? (-> o class-name))))
	      (lambda (key . args)
		#f))))

(define-macro (instance? object class)
  `(-> ,object instance? ',class))

(define-macro (Super . rest)
  `(define dassupers
     (begin
       (set! supers (list ,@rest))
       (set! super (car supers)))))

(define-macro (-> object method . args)
  (if (number? object)
      `(list-set! ,method ,object ,(car args))
      `(,object ',method ,@args)))

(define-macro (<- object method)
  (if (number? object)
      `(list-ref ,method ,object)
      `(-> ,object get-method ',method)))


#!
(define-class (<initial> sum)
  (var avar 2)
  (define-method (initial)
    (display "Initial sum: ")(display sum)
    (newline)))

(define-class (<initial2> sum)
  (define-method (initial2)
    (display "Initial sum2: ")(display sum)
    (newline)))

(define-class (<bank> sum) (Super (<initial> sum) (<initial2> sum))
  (define (print-sum)
    (display sum)(newline))
  (define-method (deposit x)
    (set! sum (+ sum x))
    (print-sum))
  (define-method (withdraw x)
    (set! sum (- sum x))
    (print-sum)))

(define b (<bank> 5))
(-> b deposit 3)
(-> b withdraw 6)
(define b->withdraw (<- b withdraw))
(define b->withdraw (-> b get-method 'withdraw))
(begin b->withdraw)
(b->withdraw 7)
(-> b class-name)
(-> b initial)
(-> b initial2)
(-> b avar)
(-> b avar 5)
(-> b avar)
(instance? b <bank>)
(instance? b <initial>)
(instance? b <initial2>)
(instance? b <someother-class>)
(-> b dir)
(-> b nosuchmethod)
!#






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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



;; Snd has its own filter-function overriding the guile filter-function.
(define (filter-org pred list)
  (remove (lambda (e) (not (pred e)))
	  list))

(define (insert! list pos element)
  (call-with-values (lambda () (split-at! list pos))
    (lambda (f s)
      (append! f (cons element s)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paint (just started)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (<paint> parent width height)

  (define pixmap #f)
  (define colors '())
  (define gc #f)
  (define font #f)

  (define-method (line color x1 y1 x2 y2)
    (gdk_draw_line pixmap color x1 y1 (- x2 x1 -1) (- y2 y1 -1)))

  (define-method (update x1 y1 x2 y2)
    (gdk_draw_pixmap (.window parent)
		     gc
		     pixmap
		     x1 y1
		     x1 y1
		     (- x2 x1 -1) (- y2 y1 -1)))

  (define-method (update-all)
    (update 0 0 width height))

  (set! pixmap (gdk_pixmap_new (.window parent)
			       width
			       height
			       -1))
  
  (gtk_signal_connect (GTK_OBJECT parent) "expose_event"
		      (lambda (w e)
			(update-all))
		      #f)

  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checkbuttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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



(define (checkbutton-set button to)
  (if use-gtk
      (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) to)
      (XtSetValues button (list XmNset to))))

(define (checkbutton-remove button)
  (if use-gtk
      (hide-widget (GTK_WIDGET button))
      ;;(gtk_widget_destroy (GTK_WIDGET button))
      (XtUnmanageChild button)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class (<button> parent name callback)

  (var button #f)

  (if use-gtk
      (begin
	(set! button (gtk_button_new_with_label name))
	(gtk_box_pack_start (GTK_BOX parent) button #t #t 20)
	(g_signal_connect_closure_by_id (GPOINTER button)
					(g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button)))
					0 (g_cclosure_new (lambda (w data) 
							    (callback))
							  #f #f) #f)
	(gtk_widget_show button))
      (begin
	(set! button (XtCreateManagedWidget name xmPushButtonWidgetClass parent
					     (list XmNbackground (basic-color)
						   XmNarmColor   (pushed-button-color))))
	(XtAddCallback button XmNactivateCallback (lambda (w c i)
							    (callback))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sliders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
		(gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG dialog))) hbox #f #f 4)
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
		(gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG dialog))) vbox #f #f 4)
		(gtk_widget_show vbox))
	      (set! vbox (XtCreateManagedWidget "formd" xmRowColumnWidgetClass dialog
						(list XmNleftAttachment      XmATTACH_FORM
						      XmNrightAttachment     XmATTACH_FORM
						      XmNtopAttachment       XmATTACH_FORM
						      XmNbottomAttachment    XmATTACH_WIDGET
						      XmNbottomWidget        (XmMessageBoxGetChild dialog XmDIALOG_SEPARATOR)
						      XmNbackground          (highlight-color)
						      XmNorientation         XmVERTICAL))))
	  (setbox1! vbox)))
    box1)

  (define (setbox1! dasvbox)
    (set! box1 dasvbox))

  (define-method (hide)
    (if use-gtk
	(gtk_widget_hide dialog)
	(XtUnmanageChild dialog))
    (focus-widget (list-ref (channel-widgets (selected-sound) 0) 0)))

  (define-method (show)
    (if use-gtk
	(begin
	  (gtk_widget_show dialog)
	  (gdk_window_raise (.window dialog)))
	(if (not (XtIsManaged dialog))
	    (XtManageChild dialog)
	    (raise-dialog dialog))))


  ;; Replacement for add-sliders in new-effects.scm/gtk-effects.scm
  (define-method (add-sliders dassliders)
    (set! sliders (map
		   (lambda (slider-data)
		     (apply <slider> (cons (getbox1) slider-data)))
		   dassliders))
    
    (if (not use-gtk)
	(let ((num_inputs (+ 1 (length sliders))))
	  (set! (widget-size dialog) (list (min 800 (max 400 (* num_inputs 20)))
						   (min 800 (max 120 (* num_inputs 70)))))))
    
    sliders)


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
    (set! dialog new-dialog)))
    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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




    
