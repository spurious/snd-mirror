;;; event.scm: user-interface auto-test functions (see snd-test.scm)

(if (not (provided? 'snd-motif)) (snd-error "event.scm is Motif-specific"))

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "event.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "Init_libxm"))))

(provide 'snd-event.scm)


(define key-event
  (let ((e (XEvent KeyPress))
	(cast-current-time (list 'Time CurrentTime)))
    (lambda (widget key state)
      "(key-event widget key state) sends the key event 'key' with 'state' control bits to 'widget'"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.type e) KeyPress)
	(set! (.window e) window)
	(set! (.display e) dpy)
	(set! (.root e) (RootWindow dpy (DefaultScreen dpy)))
	(set! (.x e) 0)
	(set! (.y e) 0)
	(set! (.x_root e) 0)
	(set! (.y_root e) 0)
	(set! (.keycode e) (XKeysymToKeycode dpy (list 'KeySym key)))
	(set! (.state e) state)
	(set! (.time e) cast-current-time)
	(set! (.same_screen e) #t)
	(set! (.subwindow e) (list 'Window None))
	(let ((err (XSendEvent dpy window #f KeyPressMask e)))
	  (if (not (= err 0))
	      (begin
		(set! (.time e) cast-current-time)
		(set! (.type e) KeyRelease)
		(set! err (XSendEvent dpy window #f KeyReleaseMask e))))
	  (if (= err 0)
	      (display (format #f "[key-event error] " err)))
	  err)))))
    
;;; (key-event (car (channel-widgets)) (char->integer #\a) 4)

(define key-event-with-mouse
  (let ((e (XEvent KeyPress))
	(cast-current-time (list 'Time CurrentTime)))
    (lambda (widget key state x y)
      "(key-event-with-mouse widget key state x y) sends the key event 'key' with 'state' control bits and mouse at 'x' and 'y' to 'widget'"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.type e) KeyPress)
	(set! (.window e) window)
	(set! (.display e) dpy)
	(set! (.root e) (RootWindow dpy (DefaultScreen dpy)))
	(set! (.x e) x)
	(set! (.y e) y)
	(set! (.x_root e) x)
	(set! (.y_root e) y)
	(set! (.keycode e) (XKeysymToKeycode dpy (list 'KeySym key)))
	(set! (.state e) state)
	(set! (.time e) cast-current-time)
	(set! (.same_screen e) #t)
	(set! (.subwindow e) (list 'Window None))
	(let ((err (XSendEvent dpy window #f KeyPressMask e)))
	  (if (not (= err 0))
	      (begin
		(set! (.time e) cast-current-time)
		(set! (.type e) KeyRelease)
		(set! err (XSendEvent dpy window #f KeyReleaseMask e))))
	  (if (= err 0)
	      (display (format #f "[key-event error] " err)))
	  err)))))

(define resize-event
  (let ((e (XEvent ResizeRequest)))
    (lambda (widget width height)
      "(resize-event widget width height) sends a resize event to the widget"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.window e) window)
	(set! (.display e) dpy)
	(set! (.width e) width)
	(set! (.height e) height)
	(XSendEvent dpy window #f ResizeRedirectMask e)))))

(define enter-event
  (let ((e (XEvent EnterNotify)))
    (lambda (widget)
      "(enter-event widget) sends an enter window event to the widget"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.window e) window)
	(set! (.display e) dpy)
	(XSendEvent dpy window #f EnterWindowMask e)))))

(define leave-event
  (let ((e (XEvent LeaveNotify)))
    (lambda (widget)
      "(leave-event widget) sends a leave window event to the widget"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.window e) window)
	(set! (.display e) dpy)
	(XSendEvent dpy window #f LeaveWindowMask e)))))

(define expose-event
  (let ((e (XEvent Expose)))
    (lambda (widget x y width height)
      "(expose-event widget x y width height) sends an exposure event to the widget"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.window e) window)
	(set! (.display e) dpy)
	(set! (.width e) width)
	(set! (.height e) height)
	(set! (.x e) x)
	(set! (.y e) y)
	(set! (.count e) 0)
	(XSendEvent dpy window #f ExposureMask e)))))

(define click-event
  (let ((e (XEvent ButtonPress)))
    (lambda (widget button state x y)
      "(click-event widget button state x y) sends a button click event to the widget"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.type e) ButtonPress)
	(set! (.window e) window)
	(set! (.display e) dpy)
	(set! (.root e) (RootWindow dpy (DefaultScreen dpy)))
	(set! (.x e) x)
	(set! (.y e) y)
	(set! (.x_root e) 0)
	(set! (.y_root e) 0)
	(set! (.state e) state)
	(set! (.button e) button)
	(set! (.time e) (list 'Time CurrentTime))
	(set! (.same_screen e) #t)
	(set! (.subwindow e) (list 'Window None))
	(let ((err (XSendEvent dpy window #f ButtonPressMask e)))
	  (if (not (= err 0))
	      (begin
		(set! (.time e) (list 'Time CurrentTime))
		(set! (.type e) ButtonRelease)
		(set! err (XSendEvent dpy window #f ButtonReleaseMask e))))
	  (if (= err 0)
	      (display (format #f "[click-event error] " err)))
	  err)))))


(define drag-event
  (let ((e (XEvent ButtonPress))
	(e1 (XEvent MotionNotify)))
    (lambda (widget button state x0 y0 x1 y1)
      "(drag-event widget button state x0 y0 x1 y1) sends a button drag event to the widget"
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.type e) ButtonPress)
	(set! (.window e) window)
	(set! (.display e) dpy)
	(set! (.root e) (RootWindow dpy (DefaultScreen dpy)))
	(set! (.x e) x0)
	(set! (.y e) y0)
	(set! (.x_root e) 0)
	(set! (.y_root e) 0)
	(set! (.state e) state)
	(set! (.button e) button)
	(set! (.time e) (list 'Time CurrentTime))
	(set! (.same_screen e) #t)
	(set! (.subwindow e) (list 'Window None))
	(let ((err (XSendEvent dpy window #f ButtonPressMask e)))
	  (if (not (= err 0))
	      (begin
		(set! (.window e1) window)
		(set! (.display e1) dpy)
		(set! (.root e1) (RootWindow dpy (DefaultScreen dpy)))
		;(set! (.x e1) x1)
		;(set! (.y e1) y1)
		(set! (.x_root e1) x0)
		(set! (.y_root e1) y0)
		(set! (.state e1) state)
		(set! (.time e1) (list 'Time (+ 300 CurrentTime)))
		(set! (.same_screen e1) #t)
		(set! (.subwindow e1) (list 'Window None))
		(set! (.is_hint e1) NotifyNormal)
		(let* ((den (if (or (> (abs (- x1 x0)) 10)
				    (> (abs (- y1 y0)) 10))
				10
				2))
		       (xdiff (inexact->exact (floor (/ (- x1 x0) den))))
		       (ydiff (inexact->exact (floor (/ (- y1 y0) den)))))
		  (do ((xn (+ x0 xdiff) (+ xn xdiff))
		       (yn (+ y0 ydiff) (+ yn ydiff))
		       (i 0 (1+ i)))
		      ((= i den))
		    (set! (.x e1) xn)
		    (set! (.y e1) yn)
		    (XSendEvent dpy window #f ButtonMotionMask e1)))
		(set! (.type e) ButtonRelease)
		(set! (.time e) (list 'Time (+ 500 CurrentTime)))
		(set! (.x e) x1)
		(set! (.y e) y1)
		(XSendEvent dpy window #f ButtonReleaseMask e))))))))

(define (select-item wid pos)
  "(select-item wid pos) selects the item in the list widget 'wid' at position 'pos' (0-based)"
  (if (not (XmIsList wid))
      (display (format #f "~A is not a list" (XtName wid)))
      (XmListSelectPos wid (+ pos 1) #t)))

(define* (click-button button :optional value bits)
  "(click-button button :optional value bits) tries to click the given button"
  (if (Widget? button)
      (if (XtIsSensitive button)
	  (if (or (XmIsPushButton button)
		  (XmIsPushButtonGadget button))
	      (if (= (XtHasCallbacks button XmNactivateCallback) XtCallbackHasSome)
		  (XtCallCallbacks button XmNactivateCallback 
				   (let ((but (XmPushButtonCallbackStruct)))
				     (set! (.click_count but) 0)
				     (set! (.event but) 
					   (let ((e (XEvent ButtonPress)))
					     (set! (.state e) (or bits 0))
					     e))
				     but))
		  (display (format #f 
				   ";pushbutton ~A has no activate callbacks~%" 
				   (XtName button))))
	      (if (or (XmIsToggleButton button)
		      (XmIsToggleButtonGadget button))
		  (if (= (XtHasCallbacks button XmNvalueChangedCallback) XtCallbackHasSome)
		      (XtCallCallbacks button XmNvalueChangedCallback 
				       (let ((tgl (XmToggleButtonCallbackStruct)))
					 (set! (.set tgl) value)
					 (set! (.event tgl) 
					       (let ((e (XEvent ButtonPress)))
						 (set! (.state e) (or bits 0))
						 e))
					 tgl))
		      (display (format #f 
				       ";togglebutton ~A has no valueChanged callbacks~%" 
				       (XtName button))))
		  (if (XmIsArrowButton button)
		      (if (= (XtHasCallbacks button XmNactivateCallback) XtCallbackHasSome)
			  (XtCallCallbacks button XmNactivateCallback
					   (let ((arr (XmArrowButtonCallbackStruct)))
					     (set! (.click_count arr) 0)
					     (set! (.event arr) 
						   (let ((e (XEvent ButtonPress)))
						     (set! (.state e) (or bits 0))
						     e))
					     arr))
			  (display (format #f 
					   ";arrowbutton ~A has no activate callbacks~%" 
					   (XtName button))))
		      (display (format #f 
				       ";~A (~A) is not a push or toggle button~%" 
				       (XtName button) (XtName (XtParent button)))))))
	  (display (format #f 
			   ";~A is not sensitive~%" 
			   (XtName button))))
      (display (format #f 
		       ";~A is not a widget~%" 
		       button))))

(define (resize-pane wid height)
  "(resize-pane wid height) tries to force a paned widget window resize"
  (XtUnmanageChild wid)
  (XtVaSetValues wid (list XmNpaneMinimum (if (> height 5) (- height 5) 0)
			   XmNpaneMaximum (+ height 5)))
  (XtManageChild wid)
  (XtVaSetValues wid (list XmNpaneMinimum 5
			   XmNpaneMaximum 1000)))

(define (force-event)
  "(force-event) tries to force X to deal with an event"
  (let ((app (car (main-widgets)))
	(done #f))
    (do ()
	(done)
      (let ((msk (XtAppPending app)))
	(if (= (logand msk (logior XtIMXEvent XtIMAlternateInput)) 0)
	    (set! done #t)
	    (XtDispatchEvent (XtAppNextEvent app)))))))
	      
(define (take-keyboard-focus wid)
  "(take-keyboard-focus wid) gives the widget 'wid' keyboard focus"
  (if (and (XmIsTraversable wid)
	   (not (= (XmGetVisibility wid) XmVISIBILITY_FULLY_OBSCURED)))
      (XmProcessTraversal wid XmTRAVERSE_CURRENT)))

(define (move-scale scl val)
  "(move-scale scl val) moves the scale widget 'scl' to 'val'"
  (XmScaleSetValue scl val)
  (XtCallCallbacks scl XmNvalueChangedCallback
    (let ((cb (XmScaleCallbackStruct)))
      (set! (.value cb) val)
      (set! (.event cb) (XEvent))
      cb)))


(define (change-prop winat name command)
  (define (find-window dpy top natom)
    (let ((res (XGetWindowProperty dpy top natom 0 1024 #f XA_STRING)))
      (if (and (= (car res) Success) 
	       (not (equal? (cadr res) (list 'Atom None))))
	  top
	  (let ((vals (XQueryTree dpy top)))
	    (if (= (car vals) 0)
		#f
		(call-with-current-continuation
		 (lambda (return)
		   (for-each 
		    (lambda (win)
		      (let ((val (find-window dpy win natom)))
			(if (Window? val)
			    (return val))))
		    (list-ref vals 3)))))))))
  (let* ((dpy (XtDisplay (cadr (main-widgets))))
	 (natom (XInternAtom dpy winat #f))
	 (window (find-window dpy (DefaultRootWindow dpy) natom)))
    (if (Window? window)
	(begin
	  (XChangeProperty dpy window (XInternAtom dpy name #f) XA_STRING 8 PropModeReplace command)
	  (XFlush dpy)
	  command)
	#f)))

(define beep-state
  (make-procedure-with-setter
   (lambda ()
     ;; returns amp pitch duration
     (let ((vals (XGetKeyboardControl (XtDisplay (cadr (main-widgets))))))
       (list (list-ref vals 1) (list-ref vals 2) (list-ref vals 3))))
   (lambda (lst) 
     ; amp pitch dur
     ; (set! (beep-state) (list 100 200 100))
     (XChangeKeyboardControl 
       (XtDisplay (cadr (main-widgets)))
       (logior KBBellPercent KBBellPitch KBBellDuration)
       (cons 0 lst)))))

(define (beep)
  "(beep) beeps"
  (XBell (XtDisplay (cadr (main-widgets))) 100))

#|
(define wm-delete-event
  ;; this works on the main window, but I can't see how to get it to work on dialogs
  (let ((e (XEvent ClientMessage)))
    (lambda (widget)
      (let ((dpy (XtDisplay widget))
	    (window (XtWindow widget)))
	(set! (.type e) ClientMessage)
	(set! (.window e) window)
	(set! (.display e) dpy)
	(set! (.data e) (list (cadr (XmInternAtom dpy "WM_DELETE_WINDOW" #f)) CurrentTime)) ; longs
	(set! (.format e) 32)
	(set! (.message_type e) (XmInternAtom dpy "WM_PROTOCOLS" #f))
	(let ((err (XSendEvent dpy window #f NoEventMask e)))
	  (if (= err 0)
	      (display (format #f "[client-event error] " err)))
	  (XFlush dpy))))))
|#
