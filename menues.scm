
;;; Functions to help making (sub)menues more convenient. (see ladspa.scm)
;;; -KSM.


(define use-gtk (if (provided? 'snd-gtk)
		    #t
		    #f))


(define* (add-submenu menu menu-label #:optional callback)
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
	(let ((submenu (XmCreatePulldownMenu dasmenu menu-label
					     (list XmNbackground (basic-color)))))
	  (XtCreateManagedWidget menu-label
				 xmCascadeButtonWidgetClass dasmenu
				 (list XmNsubMenuId submenu
				       XmNbackground (basic-color)))
	  submenu))))

(define add-to-menu-org add-to-menu)

(define* (add-to-menu top-menu menu-label callback #:optional position)
  (if (integer? top-menu)
      (if position
	  (add-to-menu-org top-menu menu-label callback position)
	  (add-to-menu-org top-menu menu-label callback))
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
	     #f))
	  (let ((child (XtCreateManagedWidget menu-label xmPushButtonWidgetClass top-menu
					      (list XmNbackground (basic-color)))))
	    (XtAddCallback child XmNactivateCallback
			   (lambda (w c i)
			     (callback)))))))



#!
(let ((test-menu (add-to-main-menu "testing")))
  (define (adding menu n)
    (if (> n 0)
	(let ((submenu (add-submenu menu (format #f "Submenu ~D" n))))
	  (add-to-menu menu "MenuItem" (lambda () (display n)))
	  (adding submenu (- n 1)))))
  (adding test-menu 10))
!#



  
