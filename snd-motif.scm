(init-xm)


;;; -------- install-searcher
;;;
;;; replaces the current file search procedure in the File Selection Box
;;;
;;;    (install-searcher (lambda (file) (= (mus-sound-srate file) 44100)))
;;;    (install-searcher (lambda (file) (= (mus-sound-chans file) 4)))

(define match-sound-files
  (lambda args
    "(match-sound-files func &optional dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
    (let* ((func (car args))
	   (matches '()))
      (for-each
       (lambda (file)
	 (if (func file)
	     (set! matches (cons file matches))))
       (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
      matches)))

(define (install-searcher proc)
  (define (XmString->string str)
    (cadr (|XmStringGetLtoR str |XmFONTLIST_DEFAULT_TAG)))
  (define (XmStringTable->list st len)
    (|XmStringTableUnparse st len #f |XmCHARSET_TEXT |XmCHARSET_TEXT #f 0 |XmOUTPUT_ALL))
  (define (list->XmStringTable strs)
    (|XmStringTableParseStringArray strs (length strs) #f |XmCHARSET_TEXT #f 0 #f))
  (|XtSetValues (let ((m (open-file-dialog #f)))
		  (|Widget (list-ref (dialog-widgets) 6)))
		(list |XmNfileSearchProc
		       (lambda (widget info)
			 (let* ((dir (XmString->string (|dir info)))
				(files (match-sound-files proc dir))
				(fileTable (list->XmStringTable (map (lambda (n) (string-append dir n)) files))))
			   (|XtSetValues widget
					 (list |XmNfileListItems fileTable
					       |XmNfileListItemCount (length files)
					       |XmNlistUpdated #t))
			   (|XmStringTableFree fileTable (length files)))))))



;;; -------- zync and unzync: start or stop y-zoom slider sync 
;;; 
;;; (i.e. when one y-zoom-slider changes position, all other channels in the sound change in parallel)
;;; (zync) to start and (unzync) to stop

(define (sync-y-zooms snd)
  (let ((calls '()))
    (do ((chn 0 (1+ chn)))
	((= chn (chans snd)))
      (let* ((zy (|Widget (list-ref (channel-widgets snd chn) 6)))
	     (zy-div (- 100.0 (cadr (|XtGetValues zy (list |XmNsliderSize 0))))))
	(set! calls
	      (cons (|XtAddCallback zy
		       |XmNdragCallback 
		         (lambda (w data info)
			   (let ((v (/ (|value info) zy-div)))
			     (do ((i 0 (1+ i)))
				 ((= i (chans snd)))
			       (if (not (= i chn))
				   (begin
				     (set! (y-zoom-slider snd i) (* v v))
				     (set! (y-position-slider snd i) (y-position-slider snd chn))))))))
			  calls))))
    (reverse calls)))

(define (unsync-y-zooms snd calls)
  (do ((chn 0 (1+ chn)))
      ((= chn (chans snd)))
    (let ((zy (|Widget (list-ref (channel-widgets snd chn) 6))))
      (|XtRemoveCallback zy |XmNdragCallback (car calls))
      (set! calls (cdr calls)))))

(define draggers '())

(define (dragger snd)
  (find-if (lambda (n) (= (car n) snd)) draggers))

(define (remove-dragger snd)
  (if (dragger snd) (apply unsync-y-zooms (dragger snd)))
  (set! draggers (remove-if (lambda (n) (= (car n) snd)) draggers)))

(define (add-dragger snd)
  (set! draggers (cons (list snd (sync-y-zooms snd)) draggers)))

(add-hook! close-hook remove-dragger)

(define (zync)
  (add-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (not (dragger n))
	 (add-dragger n)))
   (sounds)))

(define (unzync)
  (remove-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (dragger n)
	 (remove-dragger n)))
   (sounds)))



;;; -------- add our own pane to the channel section

(define (add-channel-pane snd chn name type args)
  (|XtCreateManagedWidget name type (|XtParent (|XtParent (|Widget (list-ref (channel-widgets snd chn) 7)))) args))

;;; -------- add our own pane to the sound section (underneath the controls in this case)

(define (add-sound-pane snd name type args)
  (|XtCreateManagedWidget name type (|Widget (car (sound-widgets snd))) args))

;;; -------- add our own pane to the overall Snd window (underneath the listener in this case)

(define (add-main-pane name type args)
  (|XtCreateManagedWidget name type (|Widget (list-ref (main-widgets) 3)) args))


;(add-channel-pane "new-pane" 
;		  |xmDrawingAreaWidgetClass 
;		  (list |XmNbackground (snd-pixel (graph-color))
;			|XmNforeground (snd-pixel (data-color))))


;;; draw in new pane(s) -- scanned-synthesis in own sound pane
;;; pixmaps (and animation)
;;; hidden control dialog (with new ggctrls.png)
;;; fmv dialog
;;; disable control panel
;;; spectral editing in new window
;;; running spectrogram in recorder
;;; flashier rtio.scm graphics
;;; panel of effects-buttons[icons] on right? (normalize, reverse, etc)
;;; panel of icons at top (cut/paste/undo/redo/save/play/play-selection
;;; emacs running in lower pane
