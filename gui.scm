
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

(define* (checkbutton-create parent name callback #:optional on (extraopts '()))
  (if use-gtk
      (let ((button (gtk_check_button_new_with_label name)))
	(gtk_box_pack_end (GTK_BOX parent) button #f #f 0)
	(gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) on)
	(gtk_widget_show button)
	(g_signal_connect_closure_by_id 
	 (GPOINTER button)
	 (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT button))) 0
	 (g_cclosure_new (lambda (w d) 
			   (callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w))))
			 #f #f)
	 #f)
	button)
      (let ((button (XtCreateManagedWidget name xmToggleButtonWidgetClass parent
					   (append (list XmNbackground       (basic-color)
							 XmNset              on
							 XmNselectColor      (yellow-pixel))
						   extraopts))))
	(XtAddCallback button XmNvalueChangedCallback (lambda (w c i) (callback (.set i))))
	button)))

(define (checkbutton-remove button)
  (if use-gtk
      (hide-widget (GTK_WIDGET button))
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
;;; Dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dialog-makedialog dialog sliders hbox)
  (list dialog sliders hbox))
(define (dialog-getdialog dialog)
  (car dialog))
(define (dialog-getsliders dialog)
  (cadr dialog))
(define (dialog-setsliders! dialog sliders)
  (set-car! (cdr dialog) sliders))
(define (dialog-gethbox dialog)
  (caddr dialog))
(define (dialog-sethbox! dialog hbox)
  (set-car! (cddr dialog) hbox))

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
								(deletefunc)
								(gtk_widget_hide new-dialog)
								(focus-widget (list-ref (channel-widgets (selected-sound) 0) 0)))
							      #f #f) #f))
	  (let ((titlestr (XmStringCreate label XmFONTLIST_DEFAULT_TAG)))
	    (set! new-dialog
		  (XmCreateTemplateDialog (cadr (main-widgets)) label
					  (list XmNautoUnmanage        #f
						XmNdialogTitle         titlestr
						XmNresizePolicy        XmRESIZE_GROW
						XmNnoResize            #f
						XmNbackground          (basic-color)
						XmNtransient           #f)))
	    (XtAddCallback new-dialog XmNcancelCallback (lambda (w c i)
							  (deletefunc)
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
				      name func)))
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
      (XtUnmanageChild (dialog-getdialog dialog))))

(define (dialog-show dialog)
  (activate-dialog (dialog-getdialog dialog)))

(define (dialog-add-sliders dialog sliders)
  (define slider-funcs '())
;  (display dialog)(newline)
;  (display sliders)(newline)

  (let ((newsliders (add-sliders (dialog-getdialog dialog)
				 (map (lambda (slider)
					(let* ((name (car slider))
					       (lo (cadr slider))
					       (init (caddr slider))
					       (hi (cadddr slider))
					       (orgfunc (list-ref slider 4))
					       (scale (list-ref slider 5))
					       (func (if use-gtk
							 (lambda (w data)
							   (orgfunc (.value (GTK_ADJUSTMENT w))))
							 (lambda (w c info)
							   (orgfunc (/ (.value info) scale))))))
					  (if (not use-gtk)
					      (set! slider-funcs (cons func slider-funcs)))
					  (list name
						lo
						init
						hi
						func
						scale)))
				      sliders))))

    ;; Fix the sliders (only necesarry for motif)
    (if (not use-gtk)
	(for-each (lambda (slider slider-func)
		    (XtAddCallback slider XmNdragCallback slider-func))
		  newsliders
		  (reverse slider-funcs)))
    (dialog-setsliders! dialog newsliders)

    (if (not use-gtk)
	(let ((num_inputs (+ 1 (length sliders))))
	  (set! (widget-size (dialog-getdialog dialog)) (list (min 800 (max 400 (* num_inputs 20)))
							      (min 800 (max 120 (* num_inputs 70)))))))
    
    newsliders))


(define (dialog-add-checkbutton dialog name func onoff)
  (define hbox (dialog-gethbox dialog))

  (if (not hbox)
      (begin
	(if use-gtk
	    (begin
	      (set! hbox (gtk_hbox_new #f 0))
	      (gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG (dialog-getdialog dialog)))) hbox #f #f 4)
	      (gtk_widget_show hbox))
	    (set! hbox (if (not (dialog-getsliders dialog)) (dialog-getdialog dialog) (XtParent (car (dialog-getsliders dialog))))))
	(dialog-sethbox! dialog hbox)))

  (checkbutton-create hbox
		      name
		      func
		      onoff))




    
