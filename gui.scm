
;;; Functions to help making various gui-things more convenient and without
;;; worrying about whether we use gtk or motif. (see ladspa.scm and snd_conffile.scm for examples of use)
;;; -KSM.



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
    


;; This should take care of some of the functions we need.
;(if (not (defined? 'add-sliders))
;    (load-from-path (if use-gtk
;			"gtk-effects.scm"
;			"new-effects.scm")))



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
      (gtk_label_set_text (GTK_LABEL (.child (GTK_BIN menu))) label)
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

(define* (checkbutton-create parent name callback #:optional onoff (extraopts '()))
  (if use-gtk
      (let ((button (gtk_check_button_new_with_label name))
	    (dasparent (if (isdialog? parent) (dialog-getbox2 parent) (GTK_BOX parent))))
	(gtk_box_pack_end (GTK_BOX dasparent) button #f #f 0)
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) onoff)
	(gtk_widget_show button)
	(g_signal_connect_closure_by_id 
	 (GPOINTER button)
	 (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button))) 0
	 (g_cclosure_new (lambda (w d) 
			   (callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w))))
			 #f #f)
	 #f)
	button)
      (let* ((dasparent (if (isdialog? parent) (dialog-getbox2 parent) parent))
	     (button (XtCreateManagedWidget name xmToggleButtonWidgetClass dasparent
					    (append (list XmNbackground       (basic-color)
							  ;;XmNlabelString      name
							  XmNset              onoff
							  XmNselectColor      (yellow-pixel))
						    extraopts))))
	(XtAddCallback button XmNvalueChangedCallback (lambda (w c i) (callback (.set i))))
	button)))

(define (checkbutton-remove button)
  (if use-gtk
      (hide-widget (GTK_WIDGET button))
      ;;(gtk_widget_destroy (GTK_WIDGET button))
      (XtUnmanageChild button)))

(define (checkbutton-set button to)
  (if use-gtk
      (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) to)
      (XtSetValues button (list XmNset to))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-create parent name callback)
  (if use-gtk
      (let ((button (gtk_button_new_with_label name)))
	(gtk_box_pack_start (GTK_BOX parent) button #t #t 20)
	(g_signal_connect_closure_by_id (GPOINTER button)
					(g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button)))
					0 (g_cclosure_new (lambda (w data) 
							    (callback))
							  #f #f) #f)
	(gtk_widget_show button)
	button)
      (let ((button (XtCreateManagedWidget name xmPushButtonWidgetClass parent
					   (list XmNbackground (basic-color)
						 XmNarmColor   (pushed-button-color)))))
	(XtAddCallback button XmNactivateCallback (lambda (w c i)
						    (callback)))
	button)))

  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sliders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (slider-create parent
			title
			low initial high
			func
			scaler
			#:optional use-log)
  (if use-gtk
      (let* ((vbox (if (isdialog? parent) (dialog-getbox1 parent) parent))
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
	adj)
      (let* ((mainform (if (isdialog? parent) (dialog-getbox1 parent) parent))
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
	new-slider)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (isdialog? dialog)
  (and (list? dialog) (= (length dialog) 5) (eq? (list-ref dialog 4) 'dialog)))
(define* (dialog-makedialog dialog #:optional sliders box2 box1)
  (list dialog sliders box2 box1 'dialog))
(define (dialog-getdialog dialog)
  (car dialog))
(define (dialog-getsliders dialog)
  (cadr dialog))
(define (dialog-setsliders! dialog sliders)
  (set-car! (cdr dialog) sliders))

(define (dialog-getbox2 dialog)
  (if (not (caddr dialog))
      (let ((hbox #f))
	(if use-gtk
	    (begin
	      (set! hbox (gtk_hbox_new #f 0))
	      (gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG (dialog-getdialog dialog)))) hbox #f #f 4)
	      (gtk_widget_show hbox))
	    (let* ((mainform (dialog-getbox1 dialog))
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
	(dialog-setbox2! dialog hbox)))
  (caddr dialog))
(define (dialog-setbox2! dialog hbox)
  (set-car! (cddr dialog) hbox))
(define (dialog-getbox1 dialog)
  (if (not (cadddr dialog))
      (let ((vbox #f))
	(if use-gtk
	    (begin
	      (set! vbox (gtk_vbox_new #f 2))
	      (gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG (dialog-getdialog dialog)))) vbox #f #f 4)
	      (gtk_widget_show vbox))
	    (set! vbox (XtCreateManagedWidget "formd" xmRowColumnWidgetClass (dialog-getdialog dialog)
					      (list XmNleftAttachment      XmATTACH_FORM
						    XmNrightAttachment     XmATTACH_FORM
						    XmNtopAttachment       XmATTACH_FORM
						    XmNbottomAttachment    XmATTACH_WIDGET
						    XmNbottomWidget        (XmMessageBoxGetChild (dialog-getdialog dialog) XmDIALOG_SEPARATOR)
						    XmNbackground          (highlight-color)
						    XmNorientation         XmVERTICAL))))
	(dialog-setbox1! dialog vbox)))
  (cadddr dialog))
(define (dialog-setbox1! dialog vbox)
  (set-car! (cdddr dialog) vbox))

(define (dialog-create label deletefunc . buttons)

  (define wassoc (list (list 'Close "quit_button" (quit-button-color))
		       (list 'Help "help_button" (help-button-color))
		       (list 'Apply "doit_button" (doit-button-color))
		       (list 'Dismiss "quit_button" (quit-button-color))
		       (list 'Ok "doit_button" (doit-button-color))))


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
	 (let ((button (button-create (if use-gtk
					  (.action_area (GTK_DIALOG new-dialog))
					  new-dialog)
				      name (lambda () (func new-dialog)))))
	   ;;(display (caddr wname))
	   (if use-gtk
	       (gtk_widget_set_name button (cadr wname))
	       (if (car wname)
		   (XtVaSetValues
		    button
		    (list XmNarmColor   (pushed-button-color)
			  XmNbackground (caddr wname)))))))
       
       (reverse names) (reverse funcs) (reverse wnames))
      
      ;; build rest in (.vbox (GTK_DIALOG new-dialog))
      (dialog-makedialog new-dialog #f #f)))
    

(define (dialog-hide dialog)
  (if use-gtk
      (gtk_widget_hide (dialog-getdialog dialog))
      (XtUnmanageChild (dialog-getdialog dialog)))
  (focus-widget (list-ref (channel-widgets (selected-sound) 0) 0)))

(define (dialog-show dialog)
  (if use-gtk
      (begin
	(gtk_widget_show (dialog-getdialog dialog))
	(gdk_window_raise (.window (dialog-getdialog dialog))))
      (if (not (XtIsManaged (dialog-getdialog dialog)))
	  (XtManageChild (dialog-getdialog dialog))
	  (raise-dialog (dialog-getdialog dialog)))))


;; Replacement for add-sliders in new-effects.scm/gtk-effects.scm
(define (dialog-add-sliders dialog sliders)
  (dialog-setsliders! dialog (map
			      (lambda (slider-data)
				(apply slider-create (cons dialog slider-data)))
			      sliders))

  (if (not use-gtk)
      (let ((num_inputs (+ 1 (length (dialog-getsliders dialog)))))
	(set! (widget-size (dialog-getdialog dialog)) (list (min 800 (max 400 (* num_inputs 20)))
							    (min 800 (max 120 (* num_inputs 70)))))))
    
  (dialog-getsliders dialog))



#!
(let ((d (dialog-create "gakk"  #f
			"Close" (lambda (d2) (dialog-hide d2))
			"Apply" (lambda (d2) (dialog-hide d2))
			"Play" (lambda (d2) (dialog-hide d2))
			"Stop" (lambda (d2) (dialog-hide d2))
			"Help" (lambda (d2) (dialog-hide d2)))))

  (slider-create d "slider1" 0 1 2 (lambda (val) (display val)(newline)) 100)
  (slider-create d "slider2" 0 0.2 1 (lambda (val) (display val)(newline)) 100)
  (slider-create d "slider3" 0 1 20 (lambda (val) (display val)(newline)) 1)
  (checkbutton-create d "checkbutton1" (lambda (a) (display a)(newline)))
  (checkbutton-create d "checkbutton2" (lambda (a) (display a)(newline)))
  (checkbutton-create d "checkbutton3" (lambda (a) (display a)(newline)))
  (dialog-show d))  
!#




    
