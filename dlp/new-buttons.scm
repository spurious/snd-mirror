(provide 'snd-new-buttons.scm)

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
    (load-from-path "new-icons.scm")
    (let ((play-pixmap (make-pixmap tools full-go))
          (stop-pixmap (make-pixmap tools full-stop))
          (play-forward-pixmap (make-pixmap tools play-direction-forward))
          (play-backward-pixmap (make-pixmap tools play-direction-backward))
          (loop-pixmap (make-pixmap tools loop-play))
          (loop-stop-pixmap (make-pixmap tools loop-stop)))
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
     (list open-file close-file save-as open-mix-file rec-pane env-edit regions-browser mix-pane undo-it redo-it full-go play-direction-forward loop-play start-of-file start-of-window back-one-window back-one-sample mid-window forward-one-sample forward-one-window end-of-window end-of-file last-mix-point next-mix-point zooming-in zooming-out exit-it)
     (list
           (list (lambda (w c i) (open-file-dialog)) "Open file")
           (list (lambda (w c i) (close-sound)) "Close file")
           (list (lambda (w c i) (file-save-as-dialog)) "Save file")
           (list (lambda (w c i) (mix-file-dialog)) "Mix file")
           (list (lambda (w c i) (recorder-dialog)) "Recorder")
	   (list (lambda (w c i) (enved-dialog)) "Envelope editor")
           (list (lambda (w c i) (region-dialog)) "Region editor")
           (list (lambda (w c i) (mix-dialog)) "Mix editor")
           (list (lambda (w c i) (undo)) #f)
           (list (lambda (w c i) (redo)) #f)
	   (list (let ((playing #f)
	           (play-button #f))
	       (add-hook! stop-dac-hook ; play either ended normally or was interrupted in some way
	         (lambda ()
	           (set! playing #f)
	           (if play-button (XtVaSetValues play-button (list XmNlabelPixmap play-pixmap)))))
	   (lambda (w c i)
	        (set! play-button w)
	        (if playing (stop-playing) (play))
	        (set! playing (not playing))
	        (XtVaSetValues w (list XmNlabelPixmap (if playing stop-pixmap play-pixmap))))) #f)
           (list (lambda (w c i) 
		(set! (speed-control) (- (speed-control)))
		(XtVaSetValues w (list XmNlabelPixmap (if (>= (speed-control) 0.0) play-forward-pixmap play-backward-pixmap)))) "Reverse")
           (list (let ((looping #f)
                   (loop-button #f))
               (add-hook! stop-dac-hook ; play either ended normally or was interrupted in some way
                 (lambda ()
                   (set! looping #f)
                   (if loop-button (XtVaSetValues loop-button (list XmNlabelPixmap loop-pixmap)))))
               (lambda (w c i)
                (set! loop-button w)
                (if looping (c-g!) (play-until-c-g))
                (set! looping (not looping))
                (XtVaSetValues w (list XmNlabelPixmap (if looping loop-stop-pixmap loop-pixmap))))) "Loop play")
           (list (lambda (w c i) (set! (cursor) 0)) "Move to start of file")
           (list (lambda (w c i) (set! (cursor) (left-sample))) "Move to start of window")
	   (list (lambda (w c i) (if (> (left-sample) 0) (set! (left-sample) (max 0 (- (* 2 (left-sample)) (right-sample)))))) "Move back one window")
	   (list (lambda (w c i) (set! (cursor) (max 0 (1- (cursor))))) "Move back one sample")
	   (list (lambda (w c i) (set! (cursor) (inexact->exact (/ (+ (left-sample) (right-sample)) 2)))) "Move to mid-window")
	   (list (lambda (w c i) (set! (cursor) (min (1- (frames)) (1+ (cursor))))) "Move forward one sample")
	   (list (lambda (w c i) (if (< (right-sample) (frames)) (set! (left-sample) (right-sample)))) "Move forward one window")
	   (list (lambda (w c i) (set! (cursor) (right-sample))) "Move to end of window")
	   (list (lambda (w c i) (set! (cursor) (1- (frames)))) "Move to end of file")
           (list (lambda (w c i) (backward-mix)) "Previous mix")
           (list (lambda (w c i) (forward-mix)) "Next mix")
           (list (lambda (w c i) (zoom-in)) "Zoom in")
           (list (lambda (w c i) (zoom-out)) "Zoom out")
           (list (lambda (w c i) (exit)) "Quit Snd")
           )))))
(add-useful-icons)

