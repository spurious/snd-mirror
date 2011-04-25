;;; envelope editor based on xm module (cf enved.scm)
;;;
;;; (xe-create-enved name parent args axis-bounds) -> new envelope editor (returned value is list)
;;; (xe-envelope editor) -> current envelope (settable)

(provide 'snd-xm-enved.scm)
(define with-gtk2 (provided? 'gtk2))
(define with-gtk3 (provided? 'gtk3))

(if (provided? 'snd-motif)
    (if (not (provided? 'snd-snd-motif.scm))
	(load "snd-motif.scm")))

(if (provided? 'snd-motif)
    (if (not (provided? 'xm))
	(let ((hxm (dlopen "xm.so")))
	  (if (string? hxm)
	      (snd-error (format #f "xm-enved.scm needs the xm module (either 'make xm' or build Snd with --with-static-xm): ~A" hxm))
	      (dlinit hxm "Init_libxm"))))
    (if (provided? 'snd-gtk)
	(if (not (provided? 'xg))
	    (let ((hxm (dlopen "xg.so")))
	      (if (string? hxm)
		  (snd-error (format #f "xm-enved.scm needs the xg module (either 'make xg' or build Snd with --with-static-xg): ~A" hxm))
		  (dlinit hxm "Init_libxg"))))))

(define xe-envelope
  (make-procedure-with-setter
   (lambda (drawer)
     "accessor for the current xm-enved envelope"
     (or (car drawer) 
	 (let ((ax-inf (drawer 3)))
	   (list (ax-inf 0)
		 (ax-inf 1)
		 (ax-inf 2)
		 (ax-inf 3))))) ; was 1?
   (lambda (drawer new-env)
     (set! (drawer 0) new-env)
     (xe-redraw drawer))))

(define (xe-create-enved name parent args axis-bounds)

  (define (xe-add-envelope-point x y cur-env)
    (let ((new-env '()))
      (define (search-point e)
	(if (null? e)
	    (append new-env (list x y))
	    (if (= (car e) x)
		(append new-env (list x y) (cddr e))
		(if (> (car e) x)
		    (append new-env (list x y) e)
		    (begin
		      (set! new-env (append new-env (list (car e) (cadr e))))
		      (search-point (cddr e)))))))
      (search-point cur-env)))

  (define (xe-edit-envelope-point pos x y cur-env)
    (let ((new-env '()))
      (define (search-point e npos)
	(if (= npos pos)
	    (append new-env (list x y) (cddr e))
	    (begin
	      (set! new-env (append new-env (list (car e) (cadr e))))
	      (search-point (cddr e) (+ npos 2)))))
      (search-point cur-env 0)))

  (define (xe-remove-envelope-point pos cur-env)
    (let ((new-env '()))
      (define (search-point e npos)
	(if (null? e)
	    new-env
	    (if (= pos npos)
		(append new-env (cddr e))
		(begin
		  (set! new-env (append new-env (list (car e) (cadr e))))
		  (search-point (cddr e) (+ npos 2))))))
      (search-point cur-env 0)))

  (define (xe-envelope-position x cur-env)
    (define (search-point e pos)
      (if (= (car e) x)
	  pos
	  (search-point (cddr e) (+ pos 2))))
    (search-point cur-env 0))

  (define (xe-on-dot? x y cur-env pos)
    (define xe-mouse-radius .03)
    (and (not (null? cur-env))
	 (or (and (< (abs (- (car cur-env) x)) xe-mouse-radius)
		  (< (abs (- (cadr cur-env) y)) xe-mouse-radius)
		  pos)
	     (xe-on-dot? x y (cddr cur-env) (+ pos 2)))))

  (define (xe-ungrfy drawer y)
    (let* ((bounds (drawer 3))
	   (locs (drawer 2))
	   (ay0 (bounds 1))
	   (ay1 (bounds 3))
	   (py0 (locs 1))
	   (py1 (locs 3)))
      (if (= py0 py1)
	  ay1
	  (min ay1
	       (max ay0
		    (+ ay0 (* (- ay1 ay0)
			      (/ (- py0 y)
				 (- py0 py1)))))))))
  
  (define (xe-ungrfx drawer x)
    (let* ((bounds (drawer 3))
	   (locs (drawer 2))
	   (ax0 (bounds 0))
	   (ax1 (bounds 2))
	   (px0 (locs 0))
	   (px1 (locs 2)))
      (if (= px0 px1)
	  ax0
	  (min ax1
	       (max ax0
		    (+ ax0 (* (- ax1 ax0)
			      (/ (- x px0)
				 (- px1 px0)))))))))

  (define xe-mouse-down 0)
  (define xe-mouse-up 0)
  (define xe-click-time .1)
  (define xe-mouse-pos 0)
  (define xe-mouse-new #f)

  (define (xe-mouse-press drawer xx yy)
    (let* ((cur-env (xe-envelope drawer))
	   (x (xe-ungrfx drawer xx))
	   (y (xe-ungrfy drawer yy))
	   (pos (xe-on-dot? x y cur-env 0)))
      (set! xe-mouse-new (not pos))
      (set! xe-mouse-down (get-internal-real-time))
      (if (not pos)
	  (begin
	    (set! (xe-envelope drawer) 
		  (xe-add-envelope-point x y cur-env))
	    (set! xe-mouse-pos (xe-envelope-position x (xe-envelope drawer))))
	  (set! xe-mouse-pos pos))))

  (define (xe-mouse-drag drawer xx yy)
    ;; point exists, needs to be edited with check for various bounds
    (let* ((cur-env (xe-envelope drawer))
	   (x (xe-ungrfx drawer xx))
	   (y (xe-ungrfy drawer yy))
	   (lx (if (= xe-mouse-pos 0)
		   (car cur-env)
		   (if (>= xe-mouse-pos (- (length cur-env) 2))
		       (cur-env (- (length cur-env) 2))
		       (max (cur-env (- xe-mouse-pos 2))
			    (min x
				 (cur-env (+ xe-mouse-pos 2))))))))
      (set! (xe-envelope drawer) 
	    (xe-edit-envelope-point xe-mouse-pos lx y cur-env))
      (xe-redraw drawer)))

  (define (xe-mouse-release drawer xx yy)
    (let ((cur-env (xe-envelope drawer)))
      (set! xe-mouse-up (get-internal-real-time))
      (if (and (not xe-mouse-new)
	       (<= (- xe-mouse-up xe-mouse-down) xe-click-time)
	       (not (= xe-mouse-pos 0))
	       (not (>= xe-mouse-pos (- (length cur-env) 2))))
	  (set! (xe-envelope drawer)
		(xe-remove-envelope-point xe-mouse-pos cur-env)))
      (xe-redraw drawer)
      (set! xe-mouse-new #f)))

  (if (provided? 'snd-motif)
      (begin
	(if (not (member XmNbackground args))
	    (set! args (append args (list XmNbackground (graph-color)))))
	(if (not (member XmNforeground args))
	    (set! args (append args (list XmNforeground (data-color)))))
	(let* ((drawer (XtCreateManagedWidget name xmDrawingAreaWidgetClass parent args))
	       (gc (car (snd-gcs)))
	       (x0 (car axis-bounds))
	       (x1 (cadr axis-bounds)) ; too confusing! -- change internally below
	       (y0 (caddr axis-bounds))
	       (y1 (cadddr axis-bounds))
	       (arrow-cursor (XCreateFontCursor (XtDisplay (cadr (main-widgets))) XC_crosshair))
	       (editor (list (list x0 y0 x1 y0) ; needs to be in user-coordinates (graph size can change)
			     drawer 
			     #f  ; axis pixel locs filled in when drawn
			     (list x0 y0 x1 y1)
			     (list gc #f)
			     name)))
	  (XtAddCallback drawer XmNresizeCallback 
			 (lambda (w context info) 
			   (set! (editor 2) (apply draw-axes drawer gc name axis-bounds))
			   (xe-redraw editor)))
	  (XtAddCallback drawer XmNexposeCallback 
			 (lambda (w context info) 
			   (set! (editor 2) (apply draw-axes drawer gc name axis-bounds))
			   (xe-redraw editor)))
	  (XtAddEventHandler drawer ButtonPressMask #f 
			     (lambda (w context ev flag) 
			       (xe-mouse-press editor (.x ev) (.y ev))))
	  (XtAddEventHandler drawer ButtonMotionMask #f 
			     (lambda (w context ev flag)
			       (xe-mouse-drag editor (.x ev) (.y ev))))
	  (XtAddEventHandler drawer ButtonReleaseMask #f 
			     (lambda (w context ev flag)
			       (xe-mouse-release editor (.x ev) (.y ev))))
	  (XtAddEventHandler drawer EnterWindowMask #f
			     (lambda (w context ev flag)
			       (XDefineCursor (XtDisplay w) (XtWindow w) arrow-cursor)))
	  (XtAddEventHandler drawer LeaveWindowMask #f
			     (lambda (w context ev flag)
			       (XUndefineCursor (XtDisplay w) (XtWindow w))))
	  editor))

      (let* ((drawer (gtk_drawing_area_new))
	     (gc (car (snd-gcs)))
	     (x0 (car axis-bounds))
	     (x1 (cadr axis-bounds))
	     (y0 (caddr axis-bounds))
	     (y1 (cadddr axis-bounds))
	     (arrow-cursor (gdk_cursor_new GDK_CROSSHAIR))
	     (old-cursor (gdk_cursor_new GDK_LEFT_PTR))
	     (editor (list (list x0 y0 x1 y0) ; needs to be in user-coordinates (graph size can change)
			   drawer 
			   #f  ; axis pixel locs filled in when drawn
			   (list x0 y0 x1 y1)
			   (list gc #f)
			   name))
	     (dragging #f))

	;; (xe-create-enved "hi" ((sound-widgets 0) 9) '() '(0.0 1.0 0.0 1.0))

	(define (local-draw-axes wid gc label x0 x1 y0 y1)
	  (let ((cr (make-cairo wid)))
	    (let ((val (draw-axes wid gc label x0 x1 y0 y1 x-axis-in-seconds show-all-axes cr)))
	      (free-cairo cr)
	      val)))

	(gtk_widget_set_events drawer GDK_ALL_EVENTS_MASK)
	(gtk_box_pack_start (GTK_BOX parent) drawer #t #t 10)
	(gtk_widget_show drawer)
	(gtk_widget_set_size_request drawer -1 200)

	(g_signal_connect_closure_by_id (GPOINTER drawer)
					(g_signal_lookup (if with-gtk3 "draw" "expose_event")
							 (G_OBJECT_TYPE (G_OBJECT drawer)))
					0 (g_cclosure_new (lambda (w e d)
							    (set! (editor 2) (apply local-draw-axes drawer gc name axis-bounds))
							    (xe-redraw editor)
							    #f)
							  #f #f)
					#f)
	(g_signal_connect_closure_by_id (GPOINTER drawer)
					(g_signal_lookup "configure_event" (G_OBJECT_TYPE (G_OBJECT drawer)))
					0 (g_cclosure_new (lambda (w e d)
							    (set! (editor 2) (apply local-draw-axes drawer gc name axis-bounds))
							    (xe-redraw editor)
							    #f)
							  #f #f) 
					#f)
	(g_signal_connect_closure_by_id (GPOINTER drawer)
					(g_signal_lookup "button_press_event" (G_OBJECT_TYPE (G_OBJECT drawer)))
					0 (g_cclosure_new (lambda (w e d) 
							    (let* ((ev (GDK_EVENT e))
								   (coords (gdk_event_get_coords ev))
								   (x (cadr coords))
								   (y (caddr coords)))
							      (set! dragging #t)
							      (xe-mouse-press editor x y))
							    #f)
							  #f #f) 
					#f)
	(g_signal_connect_closure_by_id (GPOINTER drawer)
					(g_signal_lookup "button_release_event" (G_OBJECT_TYPE (G_OBJECT drawer)))
					0 (g_cclosure_new (lambda (w e d) 
							    (let* ((ev (GDK_EVENT e))
								   (coords (gdk_event_get_coords ev))
								   (x (cadr coords))
								   (y (caddr coords)))
							      (set! dragging #f)
							      (xe-mouse-release editor x y))
							    #f)
							  #f #f)
					#f)
	(g_signal_connect_closure_by_id (GPOINTER drawer)
					(g_signal_lookup "motion_notify_event" (G_OBJECT_TYPE (G_OBJECT drawer)))
					0 (g_cclosure_new (lambda (w e d) 
							    (if dragging
								(let* ((ev (GDK_EVENT e))
								       (coords (gdk_event_get_coords ev))
								       (x (cadr coords))
								       (y (caddr coords)))
								  (xe-mouse-drag editor x y)))
							    #f)
							  #f #f)
					#f)

	(g_signal_connect_closure_by_id (GPOINTER drawer)
					(g_signal_lookup "enter_notify_event" (G_OBJECT_TYPE (G_OBJECT drawer)))
					0 (g_cclosure_new (lambda (w e d)
							    (gdk_window_set_cursor (gtk_widget_get_window w) arrow-cursor)
							    #f)
							  #f #f)
					#f)
	(g_signal_connect_closure_by_id (GPOINTER drawer)
					(g_signal_lookup "leave_notify_event" (G_OBJECT_TYPE (G_OBJECT drawer)))
					0 (g_cclosure_new (lambda (w e d)
							    (gdk_window_set_cursor (gtk_widget_get_window w) old-cursor)
							    #f)
							  #f #f)
					#f)
	  editor)))

(define (xe-redraw drawer)
  (let* ((cur-env (xe-envelope drawer))
	 (widget (drawer 1))
	 (dpy (and (provided? 'snd-motif) (XtDisplay widget)))
	 (wn (if (provided? 'snd-motif) (XtWindow widget) (gtk_widget_get_window widget)))
	 (ax-pix (drawer 2))
	 (ax-inf (drawer 3))
	 (gc (car (drawer 4)))
	 (name (drawer 5))
	 (len (and (list? cur-env) (length cur-env)))
	 (get_realized (if with-gtk3 gtk_widget_get_realized (lambda (w) #t))))
    (if (and (list? ax-pix)
	     (list? cur-env)
	     (if (provided? 'snd-motif)
		 (XtIsManaged widget)
		 (get_realized widget)))
	(let ((px0 (ax-pix 0))
	      (px1 (ax-pix 2))
	      (py0 (ax-pix 1))
	      (py1 (ax-pix 3))
	      (ix0 (ax-inf 0))
	      (ix1 (ax-inf 2))
	      (iy0 (ax-inf 1))
	      (iy1 (ax-inf 3))
	      (mouse-d 10)
	      (mouse-r 5))

	  (define (xe-grfx drawer x)
	    (if (= px0 px1)
		px0
		(min px1
		     (max px0
			  (floor (+ px0 (* (- px1 px0)
					   (/ (- x ix0)
					      (- ix1 ix0)))))))))
	  
	  (define (xe-grfy drawer y)
	    (if (= py0 py1)
		py0
		(min py0 ; grows downward so y1 < y0
		     (max py1
			  (floor (+ py1 (* (- py0 py1)
					   (/ (- y iy1)
					      (- iy0 iy1)))))))))

	  (if (> py0 py1)
	      (begin
		(if (provided? 'snd-motif)
		    (begin
		      (XClearWindow dpy wn)
		      (draw-axes widget gc name ix0 ix1 iy0 iy1)
		      (let ((lx #f)
			    (ly #f))
			(do ((i 0 (+ i 2)))
			    ((= i len))
			  (let ((cx (xe-grfx drawer (cur-env i)))
				(cy (xe-grfy drawer (cur-env (+ i 1)))))
			    (XFillArc dpy wn gc 
				      (- cx mouse-r)
				      (- cy mouse-r)
				      mouse-d mouse-d
				      0 (* 360 64))
			    (if lx
				(XDrawLine dpy wn gc lx ly cx cy))
			    (set! lx cx)
			    (set! ly cy)))))
		    (let ((lx #f)
			  (ly #f)
			  (cr (gdk_cairo_create ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) wn)))
			  (size (widget-size (GTK_WIDGET widget))))
		      
		      (cairo_push_group cr)
		      (cairo_set_source_rgb cr 1.0 1.0 1.0)
		      (cairo_rectangle cr 0 0 (car size) (cadr size))
		      (cairo_fill cr)

		      (draw-axes widget gc name ix0 ix1 iy0 iy1 x-axis-in-seconds show-all-axes cr)

		      (cairo_set_line_width cr 1.0)
		      (cairo_set_source_rgb cr 0.0 0.0 0.0)
		      (do ((i 0 (+ i 2)))
			  ((= i len))
			(let ((cx (xe-grfx drawer (cur-env i)))
			      (cy (xe-grfy drawer (cur-env (+ i 1)))))
			  (cairo_arc cr cx cy mouse-r 0.0 (* 2 pi))
			  (cairo_fill cr)
			  (if lx
			      (begin
				(cairo_move_to cr lx ly)
				(cairo_line_to cr cx cy)
				(cairo_stroke cr)))
			  (set! lx cx)
			  (set! ly cy)))
		      (cairo_pop_group_to_source cr)
		      (cairo_paint cr)
		      (cairo_destroy cr)))))))))
  
#|
(define outer (add-main-pane "hiho" xmFormWidgetClass '()))

(define editor (xe-create-enved "a name" outer 
			     (list XmNleftAttachment   XmATTACH_FORM
				   XmNtopAttachment    XmATTACH_FORM
				   XmNbottomAttachment XmATTACH_FORM
				   XmNrightAttachment  XmATTACH_FORM)
			     '(0.0 1.0 0.0 1.0)))
|#
