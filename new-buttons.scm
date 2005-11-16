(provide 'snd-new-buttons.scm)

(if (not (provided? 'snd-snd7.scm)) (load-from-path "snd7.scm"))           ; backward-mix
(if (not (provided? 'snd-play.scm)) (load-from-path "play.scm"))           ; play-until-c-g
(if (not (provided? 'snd-snd-motif.scm)) (load-from-path "snd-motif.scm")) ; add-main-pane, add-tooltip (etc)
(if (not (provided? 'snd-new-icons.scm)) (load-from-path "new-icons.scm"))

(define (add-listener-pane name type args)
  (let* ((listener (find-child (cadr (main-widgets)) "lisp-listener"))
         ;; this is the listener text widget, hopefully
         ;;   its parent is the scrolled window
         (listener-scroll (XtParent listener))
         ;; its parent is the form widget filling the listener pane
         (listener-form (XtParent listener-scroll)))
    ;; to insert the new widget at the top of the listener pane we need to detach the
    ;;   listener scrolled window etc -- assume here that the "args" list does not
    ;;   include any ATTACH_* arguments
    (XtUnmanageChild listener-scroll)
    (let ((top-widget (XtCreateManagedWidget name type listener-form
                                              (append
                                               (list XmNleftAttachment   XmATTACH_FORM
                                                     XmNrightAttachment  XmATTACH_FORM
                                                     XmNtopAttachment    XmATTACH_FORM)
                                               args))))
      (XtVaSetValues listener-scroll (list XmNtopAttachment XmATTACH_WIDGET
                                           XmNtopWidget     top-widget))
      (XtManageChild listener-scroll)
      top-widget)))

;;;
;;; code for the zoom buttons
;;;

(define (zoom-in)
  (let ((midpoint (* 0.5 (apply + (x-bounds))))
        (range (* -0.25 (apply - (x-bounds))))
        (dur (/ (frames) (srate))))
    (set! (x-bounds) (list (max 0.0 (- midpoint range))
                           (min dur (+ midpoint range))))))

(define (zoom-out)
  (let ((midpoint (* 0.5 (apply + (x-bounds))))
        (range (abs (apply - (x-bounds))))
        (dur (/ (frames) (srate))))
    (set! (x-bounds) (list (max 0.0 (- midpoint range))
                           (min dur (+ midpoint range))))))

;;;
;;; the button bar
;;;

(define (add-useful-icons)
  (let* ((toolscroll (add-main-pane "toolscroll" xmScrolledWindowWidgetClass
                       (list XmNscrollingPolicy XmAUTOMATIC
                             XmNscrollBarDisplayPolicy XmSTATIC
                             XmNpaneMinimum (+ 48 26) ; leave room for scrollers
                             XmNpaneMaximum (+ 48 26)
                             XmNbackground (basic-color))))
         (tools (XtCreateManagedWidget "tools" xmRowColumnWidgetClass toolscroll
                  (list XmNbackground (black-pixel)
                        XmNorientation XmHORIZONTAL))))
    (let ((play-pixmap (make-pixmap tools icon-full-go))
          (stop-pixmap (make-pixmap tools icon-full-stop))
          (play-forward-pixmap (make-pixmap tools icon-play-direction-forward))
          (play-backward-pixmap (make-pixmap tools icon-play-direction-backward))
          (loop-pixmap (make-pixmap tools icon-loop-play))
          (loop-stop-pixmap (make-pixmap tools icon-loop-stop)))
    (for-each
     (lambda (icon callback-and-tooltip)
       (let ((button
              (XtCreateManagedWidget "button" xmPushButtonWidgetClass tools
                (list XmNlabelPixmap (make-pixmap tools icon)
                      XmNlabelType   XmPIXMAP
                      XmNwidth       32
                      XmNheight      32))))
         (XtAddCallback button XmNactivateCallback (car callback-and-tooltip))
         (if (string? (cadr callback-and-tooltip))
             (add-tooltip button (cadr callback-and-tooltip)))))

     (list icon-open-file icon-close-file icon-save-as icon-open-mix-file icon-rec-pane icon-env-edit 
	   icon-regions-browser icon-mix-pane icon-undo-it icon-redo-it icon-full-go icon-play-direction-forward 
	   icon-loop-play icon-start-of-file icon-start-of-window icon-back-one-window icon-back-one-sample 
	   icon-mid-window icon-forward-one-sample icon-forward-one-window icon-end-of-window icon-end-of-file 
	   icon-last-mix-point icon-next-mix-point icon-zooming-in icon-zooming-out icon-exit-it)

     (list
           (list (lambda (w c i) 
		   (open-file-dialog))
		 "Open file")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (close-sound)))
		 "Close file")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (save-sound-dialog)))
		 "Save file")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (mix-file-dialog)))
		 "Mix file")

           (list (lambda (w c i) 
		   (recorder-dialog))
		 "Recorder")

	   (list (lambda (w c i) 
		   (enved-dialog))
		 "Envelope editor")

           (list (lambda (w c i) 
		   (if (not (null? (regions))) 
		       (view-regions-dialog)))
		 "Region editor")

           (list (lambda (w c i) 
		   (mix-dialog))
		 "Mix editor")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (undo)))
		 "Undo edit")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (redo)))
		 "Redo edit")

	   (list (let ((playing #f)
		       (already-hooked #f))
		   (lambda (w c i)
		     (if (not (null? (sounds)))
			 (begin
			   (if (not already-hooked)
			       (begin
				 (add-hook! stop-dac-hook ; play either ended normally or was interrupted in some way
					    (lambda ()
					      (if playing 
						  (XtVaSetValues w (list XmNlabelPixmap play-pixmap)))
					      (set! playing #f)))
				 (set! already-hooked #t)))
			   (if playing
			       (stop-playing) ; hook takes care of the rest
			       (begin
				 (set! playing #t)
				 (play)
				 (XtVaSetValues w (list XmNlabelPixmap stop-pixmap))))))))
		 "Play")

           (list (lambda (w c i) 
		   (if (not (null? (sounds)))
		       (begin
			 (set! (speed-control) (- (speed-control)))
			 (XtVaSetValues w (list XmNlabelPixmap (if (>= (speed-control) 0.0) play-forward-pixmap play-backward-pixmap))))))
		 "Reverse")

           (list (let ((looping #f)
		       (already-hooked #f))
		   (lambda (w c i)
		     (if (not (null? (sounds)))
			 (begin
			   (if (not already-hooked)
			       (begin
				 (add-hook! stop-dac-hook ; play either ended normally or was interrupted in some way
					    (lambda ()
					      (if looping
						  (XtVaSetValues w (list XmNlabelPixmap loop-pixmap)))
					      (set! looping #f)))
				 (set! already-hooked #t)))
			   (if looping 
			       (c-g!)
			       (begin
				 (set! looping #t)
				 (XtVaSetValues w (list XmNlabelPixmap loop-stop-pixmap))
				 (play-until-c-g)))))))
		 "Loop play")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (set! (cursor) 0))) 
		 "Move to start of file")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (set! (cursor) (left-sample)))) 
		 "Move to start of window")

	   (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (if (> (left-sample) 0) 
			   (set! (left-sample) (max 0 (- (* 2 (left-sample)) (right-sample)))))))
		 "Move back one window")

	   (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (set! (cursor) (max 0 (1- (cursor))))))
		 "Move back one sample")

	   (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (set! (cursor) (inexact->exact (round (/ (+ (left-sample) (right-sample)) 2))))))
		 "Move to mid-window")

	   (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (set! (cursor) (min (1- (frames)) (1+ (cursor))))))
		 "Move forward one sample")

	   (list (lambda (w c i) 
		   (if (not (null? (sounds)))
		       (if (< (right-sample) (frames)) (set! (left-sample) (right-sample)))))
		 "Move forward one window")

	   (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (set! (cursor) (right-sample))))
		 "Move to end of window")

	   (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (set! (cursor) (1- (frames)))))
		 "Move to end of file")

           (list (lambda (w c i) 
		   (backward-mix)) 
		 "Previous mix")

           (list (lambda (w c i) 
		   (forward-mix))
		 "Next mix")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (zoom-in)))
		 "Zoom in")

           (list (lambda (w c i) 
		   (if (not (null? (sounds))) 
		       (zoom-out)))
		 "Zoom out")

           (list (lambda (w c i) 
		   (exit)) 
		 "Quit Snd")

           )))))

(add-useful-icons)

