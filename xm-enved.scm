;;; envelope editor based on xm module (cf enved.scm)
;;;
;;; (xe-create-enved name parent args axis-bounds) -> new envelope editor (returned value is list)
;;; (xe-envelope editor) -> current envelope (settable)

(use-modules (ice-9 common-list) (ice-9 format))

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "xe-enved.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "init_xm"))))

(define xe-envelope
  (make-procedure-with-setter
   (lambda (drawer)
     (or (car drawer) 
	 (let ((ax-inf (list-ref drawer 3)))
	   (list (list-ref ax-inf 0)
		 (list-ref ax-inf 1)
		 (list-ref ax-inf 2)
		 (list-ref ax-inf 1)))))
   (lambda (drawer new-env)
     (list-set! drawer 0 new-env)
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
    (let* ((bounds (list-ref drawer 3))
	   (locs (list-ref drawer 2))
	   (ay0 (list-ref bounds 1))
	   (ay1 (list-ref bounds 3))
	   (py0 (list-ref locs 1))
	   (py1 (list-ref locs 3)))
      (if (= py0 py1)
	  ay1
	  (min ay1
	       (max ay0
		    (+ ay0 (* (- ay1 ay0)
			      (/ (- py0 y)
				 (- py0 py1)))))))))
  
  (define (xe-ungrfx drawer x)
    (let* ((bounds (list-ref drawer 3))
	   (locs (list-ref drawer 2))
	   (ax0 (list-ref bounds 0))
	   (ax1 (list-ref bounds 2))
	   (px0 (list-ref locs 0))
	   (px1 (list-ref locs 2)))
      (if (= px0 px1)
	  ax0
	  (min ax1
	       (max ax0
		    (+ ax0 (* (- ax1 ax0)
			      (/ (- x px0)
				 (- px1 px0)))))))))

  (define xe-mouse-down 0)
  (define xe-mouse-up 0)
  (define xe-click-time 10) ; .1 sec?
  (define xe-mouse-pos 0)
  (define xe-mouse-new #f)

  (define (xe-mouse-press drawer xx yy)
    (let* ((cur-env (xe-envelope drawer))
	   (x (xe-ungrfx editor xx))
	   (y (xe-ungrfy editor yy))
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
	   (x (xe-ungrfx editor xx))
	   (y (xe-ungrfy editor yy))
	   (ax-pix (list-ref drawer 2))
	   (lx (if (= xe-mouse-pos 0)
		   (car cur-env)
		   (if (>= xe-mouse-pos (- (length cur-env) 2))
		       (list-ref cur-env (- (length cur-env) 2))
		       (max (list-ref cur-env (- xe-mouse-pos 2))
			    (min x
				 (list-ref cur-env (+ xe-mouse-pos 2))))))))
      (set! (xe-envelope drawer) 
	    (xe-edit-envelope-point xe-mouse-pos lx y cur-env))
      (xe-redraw drawer)))

  (define (xe-mouse-release drawer xx yy)
    (let* ((cur-env (xe-envelope drawer))
	   (x (xe-ungrfx editor xx))
	   (y (xe-ungrfy editor yy))
	   (ax-pix (list-ref drawer 2)))
      (set! xe-mouse-up (get-internal-real-time))
      (if (and (not xe-mouse-new)
	       (<= (- xe-mouse-up xe-mouse-down) xe-click-time)
	       (not (= xe-mouse-pos 0))
	       (not (>= xe-mouse-pos (- (length cur-env) 2))))
	  (set! (xe-envelope drawer)
		(xe-remove-envelope-point xe-mouse-pos cur-env)))
      (xe-redraw drawer)
      (set! xe-mouse-new #f)))


  (if (not (member |XmNbackground args))
      (set! args (append args (list |XmNbackground (|Pixel (snd-pixel (graph-color)))))))
  (if (not (member |XmNforeground args))
      (set! args (append args (list |XmNforeground (|Pixel (snd-pixel (data-color)))))))
  (let* ((drawer (|XtCreateManagedWidget name |xmDrawingAreaWidgetClass parent args))
	 (gc (|GC (car (snd-gcs))))
	 (egc (|GC (list-ref (snd-gcs) 7)))
	 (x0 (car axis-bounds))
	 (x1 (cadr axis-bounds)) ; too confusing! -- change internally below
	 (y0 (caddr axis-bounds))
	 (y1 (cadddr axis-bounds))
	 (editor (list (list x0 y0 x1 y0) ; needs to be in user-coordinates (graph size can change)
		       drawer 
		       #f  ; axis pixel locs filled in when drawn
		       (list x0 y0 x1 y1)
		       (list gc egc) 
		       name)))
    (|XtAddCallback drawer |XmNresizeCallback 
		    (lambda (w context info) 
		      (list-set! editor 2 (apply draw-axes drawer gc name axis-bounds))
		      (xe-redraw editor)))
    (|XtAddCallback drawer |XmNexposeCallback 
		    (lambda (w context info) 
		      (list-set! editor 2 (apply draw-axes drawer gc name axis-bounds))
		      (xe-redraw editor)))
    (|XtAddEventHandler drawer |ButtonPressMask #f 
			(lambda (w context ev flag) 
			  (xe-mouse-press editor (|x ev) (|y ev))))
    (|XtAddEventHandler drawer |ButtonMotionMask #f 
			(lambda (w context ev flag)
			  (xe-mouse-drag editor (|x ev) (|y ev))))
    (|XtAddEventHandler drawer |ButtonReleaseMask #f 
			(lambda (w context ev flag)
			  (xe-mouse-release editor (|x ev) (|y ev))))
    editor))

(define (xe-redraw drawer)
  (let* ((cur-env (xe-envelope drawer))
	 (widget (list-ref drawer 1))
	 (dpy (|XtDisplay widget))
	 (wn (|XtWindow widget))
	 (ax-pix (list-ref drawer 2))
	 (ax-inf (list-ref drawer 3))
	 (gc (car (list-ref drawer 4)))
	 (egc (cadr (list-ref drawer 4)))
	 (name (list-ref drawer 5))
	 (len (and (list? cur-env) (length cur-env)))
	 (px0 (list-ref ax-pix 0))
	 (px1 (list-ref ax-pix 2))
	 (py0 (list-ref ax-pix 1))
	 (py1 (list-ref ax-pix 3))
	 (ix0 (list-ref ax-inf 0))
	 (ix1 (list-ref ax-inf 2))
	 (iy0 (list-ref ax-inf 1))
	 (iy1 (list-ref ax-inf 3))
	 (mouse-d 10)
	 (mouse-r 5))

    (define (xe-grfx drawer x)
      (if (= px0 px1)
	  px0
	  (min px1
	       (max px0
		    (inexact->exact
		     (+ px0 (* (- px1 px0)
			       (/ (- x ix0)
				  (- ix1 ix0)))))))))

    (define (xe-grfy drawer y)
      (if (= py0 py1)
	  py0
	  (min py0 ; grows downward so y1 < y0
	       (max py1
		    (inexact->exact
		     (+ py1 (* (- py0 py1)
			       (/ (- y iy1)
				  (- iy0 iy1)))))))))

    (if (and (> py0 py1)
	     (list? cur-env)
	     (|XtIsManaged widget))
	(begin
	  (|XClearWindow dpy wn)
	  (draw-axes widget gc name ix0 ix1 iy0 iy1)

	  (let ((lx #f)
		(ly #f))
	    (do ((i 0 (+ i 2)))
		((= i len))
	      (let ((cx (xe-grfx drawer (list-ref cur-env i)))
		    (cy (xe-grfy drawer (list-ref cur-env (+ i 1)))))
		(|XFillArc dpy wn gc 
			   (- cx mouse-r)
			   (- cy mouse-r)
			   mouse-d mouse-d
			   0 (* 360 64))
		(if lx
		    (|XDrawLine dpy wn gc lx ly cx cy))
		(set! lx cx)
		(set! ly cy))))))))


(if #f (begin
(define outer (add-main-pane "hiho" |xmFormWidgetClass '()))

(define editor (xe-create-enved "a name" outer 
			     (list |XmNleftAttachment   |XmATTACH_FORM
				   |XmNtopAttachment    |XmATTACH_FORM
				   |XmNbottomAttachment |XmATTACH_FORM
				   |XmNrightAttachment  |XmATTACH_FORM)
			     '(0.0 1.0 0.0 1.0)))
))

