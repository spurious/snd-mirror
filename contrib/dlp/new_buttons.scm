(define (add-useful-icons)
  (let ((tools (add-main-pane "tools" |xmRowColumnWidgetClass
                  (list |XmNbackground (black-pixel)
                        |XmNpaneMinimum 48
                        |XmNpaneMaximum 48
                        |XmNorientation |XmHORIZONTAL))))
    (load "/home/dlphilp/my_scm/new-icons.scm")
    (for-each
     (lambda (icon callback)
       (let ((button
              (|XtCreateManagedWidget "button" |xmPushButtonWidgetClass tools
                (list |XmNlabelPixmap (make-pixmap tools icon)
                      |XmNlabelType   |XmPIXMAP
                      |XmNwidth       32
                      |XmNheight      32))))
         (|XtAddCallback button |XmNactivateCallback callback)))
     (list open-file close-file save-as rec-pane env-edit regions-browser mix-pane full-go full-stop start-of-file start-of-window back-one-window back-one-sample mid-window forward-one-sample forward-one-window end-of-window end-of-file undo-it exit-it)
     (list 
           (lambda (w c i) (open-file-dialog))
           (lambda (w c i) (close-sound))
           (lambda (w c i) (file-save-as-dialog))
           (lambda (w c i) (recorder-dialog))
	   (lambda (w c i) (enved-dialog))
           (lambda (w c i) (region-dialog))
           (lambda (w c i) (mix-panel))
           (lambda (w c i) (play))
           (lambda (w c i) (stop-playing))
           (lambda (w c i) (set! (cursor) 0)) ; to start of file
           (lambda (w c i) (set! (cursor) (left-sample))) ; to window start
	   (lambda (w c i) (if (> (left-sample) 0) (set! (left-sample) (max 0 (- (* 2 (left-sample)) (right-sample)))))) ; back one window
	   (lambda (w c i) (set! (cursor) (max 0 (1- (cursor))))) ; back one sample
	   (lambda (w c i) (set! (cursor) (inexact->exact (/ (+ (left-sample) (right-sample)) 2)))) ; to mid-window
	   (lambda (w c i) (set! (cursor) (min (1- (frames)) (1+ (cursor))))) ; ahead one sample
	   (lambda (w c i) (if (< (right-sample) (frames)) (set! (left-sample) (right-sample)))) ; ahead one window
	   (lambda (w c i) (set! (cursor) (right-sample))) ; to window end
	   (lambda (w c i) (set! (cursor) (1- (frames)))) ; to end of file
           (lambda (w c i) (undo))
           (lambda (w c i) (exit))
           ))))
(add-useful-icons)

