(use-modules (ice-9 format))

(define marks-list '()) ; menu labels are updated to show current default settings

(define marks-menu (add-to-main-menu "Marks" (lambda ()
						   (define (update-label marks)
						     (if (not (null? marks))
							 (begin
							   ((car marks))
							   (update-label (cdr marks)))))
						   (update-label marks-list))))
(define (all-chans)
  (let ((sndlist '())
	(chnlist '()))
    (for-each (lambda (snd)
		(do ((i (1- (channels snd)) (1- i)))
		    ((< i 0))
		  (set! sndlist (cons snd sndlist))
		  (set! chnlist (cons i chnlist))))
	      (sounds))
    (list sndlist chnlist)))

(define map-chan-with-sync
  (lambda (func origin)
    (let ((snc (sync)))
      (if (> snc 0)
	  (apply map
		 (lambda (snd chn)
		   (if (= (sync snd) snc)
		       (map-chan (func) #f #f origin snd chn)))
		 (all-chans))
	  (map-chan (func) #f #f origin)))))

(define find-plausible-marks
  (lambda args
    (let* ((snd (selected-sound))
           (chn (selected-channel))
           (m1 (if (> (length args) 0)
                   (car args)
                   (let find-mark ((ms (marks snd chn)))
                     (if (null? ms)
                         (begin
                           (snd-print ";no marks in current window?")
                           #f)
                         (if (>= (mark-sample (car ms)) (left-sample snd chn))
                             (car ms)
                             (find-mark (cdr ms)))))))
           (m2 (and (mark? m1)
                    (if (> (length args) 1)
                        (cadr args)
                        (let find-another-mark ((ms (marks snd chn)))
                          (if (null? ms)
                              (begin
                                (snd-print ";no second mark?")
                                #f)
                              (if (> (mark-sample (car ms)) (mark-sample m1))
                                  (car ms)
                                  (find-another-mark (cdr ms)))))))))
      (if (and (mark? m1)
               (mark? m2))
          (list (mark-sample m1)
                (car (mark-home m1))
                (cadr (mark-home m1))
                #f
                (mark-sample m2))
          #f))))



;;; -------- Play between by marks
;;;

(define play-between-marks-m1 0)
(define play-between-marks-m2 1)
(define play-between-marks-label "Play between marks")
(define play-between-marks-dialog #f)

(define (cp-play-between-marks)
 (play-between-marks play-between-marks-m1 play-between-marks-m2))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-play-between-marks-dialog)
        (if (not (Widget? play-between-marks-dialog))
            ;; if play-between-marks-dialog doesn't exist, create it
            (let ((initial-play-between-marks-m1 0)
                  (initial-play-between-marks-m2 1)
                  (sliders '()))
              (set! play-between-marks-dialog 
		    (make-effect-dialog play-between-marks-label
					(lambda (w context info)
					  (cp-play-between-marks))
					(lambda (w context info)
					  (XtUnmanageChild play-between-marks-dialog))
					(lambda (w context info)
					  (help-dialog "Define selection by marks Help"
						"Plays area between specified marks.\n\ Use the sliders to select the boundary marks.\n\ Negative play not supported yet: coming soon !"))
					(lambda (w c i)
					  (set! play-between-marks-m1 initial-play-between-marks-m1)
					  (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* play-between-marks-m1 1))))
					  (set! play-between-marks-m2 initial-play-between-marks-m2)
					  (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* play-between-marks-m2 1)))))))
	      (set! sliders
		    (add-sliders play-between-marks-dialog
				 (list (list "mark one" 0 initial-play-between-marks-m1 25
					     (lambda (w context info)
					       (set! play-between-marks-m1 (/ (.value info) 1)))
					     1)
				       (list "mark two" 0 initial-play-between-marks-m2 25
					     (lambda (w context info)
					       (set! play-between-marks-m2 (/ (.value info) 1)))
					     1))))))
	(activate-dialog play-between-marks-dialog))

      (add-to-menu marks-menu "Play between marks" (lambda () (post-play-between-marks-dialog))))

    (add-to-menu marks-menu play-between-marks-label cp-play-between-marks))

(set! marks-list (cons (lambda ()
                           (let ((new-label (format #f "Play between marks (~1,2D ~1,2D)" play-between-marks-m1 play-between-marks-m2)))
                             (change-menu-label marks-menu play-between-marks-label new-label)
                             (set! play-between-marks-label new-label)))
                         marks-list))


;;; -------- Loop play between marks
;;;

(define loop-between-marks-m1 0)
(define loop-between-marks-m2 1)
(define loop-between-marks-buffer-size 512)
(define loop-between-marks-label "Loop play between marks")
(define loop-between-marks-dialog #f)
(define loop-between-marks-default-buffer-widget #f)

(define use-combo-box-for-buffer-size #f) ; radio-buttons or combo-box choice


(define (cp-loop-between-marks)
  (loopit loop-between-marks-m1 loop-between-marks-m2 loop-between-marks-buffer-size))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-loop-between-marks-dialog)
        (if (not (Widget? loop-between-marks-dialog))
            ;; if loop-between-marks-dialog doesn't exist, create it
            (let ((initial-loop-between-marks-m1 0)
                  (initial-loop-between-marks-m2 1)
                  (initial-loop-between-marks-buffer-size 512)
                  (sliders '()))
              (set! loop-between-marks-dialog
                    (make-effect-dialog loop-between-marks-label
                                        (lambda (w context info) (cp-loop-between-marks))
                                        (lambda (w context info) (XtUnmanageChild loop-between-marks-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Loop play between marks"
                                                       "Move the sliders to set the mark numbers. Check a radio button to set the buffer size."))
                                        (lambda (w c i)
                                          (set! loop-between-marks-m1 initial-loop-between-marks-m1)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* loop-between-marks-m1 1))))
                                          (set! loop-between-marks-m2 initial-loop-between-marks-m2)
                                          (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* loop-between-marks-m2 1))))
                                          (set! loop-between-marks-buffer-size initial-loop-between-marks-buffer-size)
                                          (if use-combo-box-for-buffer-size
                                              (XtSetValues loop-between-marks-default-buffer-widget (list XmNselectedPosition 1))
                                              (XmToggleButtonSetState loop-between-marks-default-buffer-widget #t #t)))))
              (set! sliders
                    (add-sliders loop-between-marks-dialog
                                 (list (list "mark one" 0 initial-loop-between-marks-m1 25
                                             (lambda (w context info)
                                               (set! loop-between-marks-m1 (/ (.value info) 1)))
                                             1)
                                       (list "mark two" 0 initial-loop-between-marks-m2 25
                                             (lambda (w context info)
                                               (set! loop-between-marks-m2 (/ (.value info) 1)))
                                             1))))

              ;; now add either a radio-button box or a combo-box for the buffer size
              ;;   need to use XtParent here since "mainform" isn't returned by add-sliders

              (if use-combo-box-for-buffer-size
                  ;; this block creates a "combo box" to handle the buffer size
                  (let* ((s1 (XmStringCreateLocalized "Buffer size"))
                         (frame (XtCreateManagedWidget "frame" xmFrameWidgetClass (XtParent (car sliders))
                                   (list XmNborderWidth 1
                                         XmNshadowType XmSHADOW_ETCHED_IN
                                         XmNpositionIndex 2)))
                         (frm (XtCreateManagedWidget "frm" xmFormWidgetClass frame
                                (list XmNleftAttachment      XmATTACH_FORM
                                      XmNrightAttachment     XmATTACH_FORM
  XmNtopAttachment       XmATTACH_FORM
                                      XmNbottomAttachment    XmATTACH_FORM
                                      XmNbackground          (basic-color))))
                         (lab (XtCreateManagedWidget "Buffer size" xmLabelWidgetClass frm
                                   (list XmNleftAttachment      XmATTACH_FORM
                                         XmNrightAttachment     XmATTACH_NONE
                                         XmNtopAttachment       XmATTACH_FORM
                                         XmNbottomAttachment    XmATTACH_FORM
                                         XmNlabelString         s1
                                         XmNbackground          (basic-color))))
                         (buffer-labels (map (lambda (n) (XmStringCreateLocalized n)) (list "64" "128" "256" "512" "1024" "2048" "4096")))
                         (combo (XtCreateManagedWidget "buffersize" xmComboBoxWidgetClass frm
                                   (list XmNleftAttachment      XmATTACH_WIDGET
                                         XmNleftWidget          lab
                                         XmNrightAttachment     XmATTACH_FORM
                                         XmNtopAttachment       XmATTACH_FORM
                                         XmNbottomAttachment    XmATTACH_FORM
                                         XmNitems               buffer-labels
                                         XmNitemCount           (length buffer-labels)
                                         XmNcomboBoxType        XmDROP_DOWN_COMBO_BOX
                                         XmNbackground          (basic-color)))))
                    (set! loop-between-marks-default-buffer-widget combo)
                    (for-each (lambda (n) (XmStringFree n)) buffer-labels)
                    (XmStringFree s1)
                    (XtSetValues combo (list XmNselectedPosition 1))
                    (XtAddCallback combo XmNselectionCallback
                       (lambda (w c i)
                         (let* ((selected (.item_or_text i))
                                (size-as-string (cadr (XmStringGetLtoR selected XmFONTLIST_DEFAULT_TAG))))
                           (set! loop-between-marks-buffer-size (string->number size-as-string))))))

                  ;; this block creates a "radio button box"
                  (let* ((s1 (XmStringCreateLocalized "Buffer size"))
                         (frame (XtCreateManagedWidget "frame" xmFrameWidgetClass (XtParent (car sliders))
                                   (list XmNborderWidth 1
                                         XmNshadowType XmSHADOW_ETCHED_IN
                                         XmNpositionIndex 2)))
                         (frm (XtCreateManagedWidget "frm" xmFormWidgetClass frame
                                (list XmNleftAttachment      XmATTACH_FORM
                                      XmNrightAttachment     XmATTACH_FORM
                                      XmNtopAttachment       XmATTACH_FORM
                                      XmNbottomAttachment    XmATTACH_FORM
                                      XmNbackground          (basic-color))))
                         (rc (XtCreateManagedWidget "rc" xmRowColumnWidgetClass frm
                                   (list XmNorientation XmHORIZONTAL
                                         XmNradioBehavior #t
                                         XmNradioAlwaysOne #t
                                         XmNentryClass xmToggleButtonWidgetClass
                                         XmNisHomogeneous #t
                                         XmNleftAttachment      XmATTACH_FORM
                                         XmNrightAttachment     XmATTACH_FORM
                                         XmNtopAttachment       XmATTACH_FORM
                                         XmNbottomAttachment    XmATTACH_NONE
                                         XmNbackground          (basic-color))))
                         (lab (XtCreateManagedWidget "Buffer size" xmLabelWidgetClass frm
                                   (list XmNleftAttachment      XmATTACH_FORM
                                         XmNrightAttachment     XmATTACH_FORM
                                         XmNtopAttachment       XmATTACH_WIDGET
                                         XmNtopWidget           rc
                                         XmNbottomAttachment    XmATTACH_FORM
                                         XmNlabelString         s1
                                         XmNalignment           XmALIGNMENT_BEGINNING
                                         XmNbackground          (basic-color)))))
                    (for-each

                    (lambda (size)
                       (let ((button (XtCreateManagedWidget (format #f "~D" size) xmToggleButtonWidgetClass rc
                                        (list XmNbackground           (basic-color)
                                              XmNvalueChangedCallback (list (lambda (w c i) (if (.set i) (set! loop-between-marks-buffer-size c))) size)
                                              XmNset                  (= size loop-between-marks-buffer-size)))))
                         (if (= size loop-between-marks-buffer-size)
                             (set! loop-between-marks-default-buffer-widget button))))
                     (list 64 128 256 512 1024 2048 4096))
                    (XmStringFree s1)))))
        (activate-dialog loop-between-marks-dialog))

      (add-to-menu marks-menu "Loop play between marks" (lambda () (post-loop-between-marks-dialog))))

    (add-to-menu marks-menu loop-between-marks-label cp-loop-between-marks))

(set! marks-list (cons (lambda ()
                           (let ((new-label (format #f "Loop play between marks (~1,2D ~1,2D ~1,2D)"
                                                loop-between-marks-m1 loop-between-marks-m2 loop-between-marks-buffer-size)))
                             (change-menu-label marks-menu loop-between-marks-label new-label)
                             (set! loop-between-marks-label new-label)))
                         marks-list))

(add-to-menu marks-menu #f #f)

;;; -------- trim from and back (goes by first or last mark)
(define (trim-front)
  "trim-front finds the first mark in each of the syncd channels and removes all samples before it"
  (let ((snc (sync)))
    (define (trim-front-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
          (report-in-minibuffer "trim-front needs a mark" snd)
          (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)))
    (if (> snc 0)
        (apply map
               (lambda (snd chn)
                 (if (= (sync snd) snc)
                     (trim-front-one-channel snd chn)))
               (all-chans))
        (trim-front-one-channel (selected-sound) (selected-channel)))))

(add-to-menu marks-menu "Trim before mark" trim-front)

(define (trim-back)
  "trim-back finds the last mark in each of the syncd channels and removes all samples after it"
  (let ((snc (sync)))
    (define (trim-back-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
          (report-in-minibuffer "trim-back needs a mark" snd)
          (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
            (delete-samples (+ endpt 1) (- (frames snd chn) endpt)))))
    (if (> snc 0)
        (apply map
               (lambda (snd chn)
                 (if (= (sync snd) snc)
                     (trim-back-one-channel snd chn)))
               (all-chans))
        (trim-back-one-channel (selected-sound) (selected-channel)))))

(add-to-menu marks-menu "Trim behind mark" trim-back)


;;; -------- crop (trims front and back)
(define (crop)
  "crop finds the first and last marks in each of the syncd channels and removes all samples outside them"
  (let ((snc (sync)))
    (define (crop-one-channel snd chn)
      (if (< (length (marks snd chn)) 2)
          (report-in-minibuffer "crop needs start and end marks" snd)
          (as-one-edit
           (lambda ()
             (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)
             (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
               (delete-samples (+ endpt 1) (- (frames snd chn) endpt))))
           "crop")))
    (if (> snc 0)
        (apply map
               (lambda (snd chn)
                 (if (= (sync snd) snc)
                     (crop-one-channel snd chn)))
               (all-chans))
        (crop-one-channel (selected-sound) (selected-channel)))))

(add-to-menu marks-menu "Crop around marks" crop)

(add-to-menu marks-menu #f #f)

;;; -------- Fit selection to marks
;;;

(define fit-to-mark-one 0)
(define fit-to-mark-two 1)
(define fit-to-mark-label "Fit selection to marks")
(define fit-to-mark-dialog #f)

(define (cp-fit-to-marks)
 (fit-selection-between-marks fit-to-mark-one fit-to-mark-two))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-fit-to-mark-dialog)
        (if (not (Widget? fit-to-mark-dialog))
            ;; if fit-to-mark-dialog doesn't exist, create it
            (let ((initial-fit-to-mark-one 0)
                  (initial-fit-to-mark-two 1)
                  (sliders '()))
              (set! fit-to-mark-dialog 
		    (make-effect-dialog fit-to-mark-label
					(lambda (w context info)
					  (cp-fit-to-marks))
					(lambda (w context info)
					  (XtUnmanageChild fit-to-mark-dialog))
					(lambda (w context info)
					  (help-dialog "Fit selection to marks Help"
"Fit-selection-between-marks tries to squeeze the current selection between two marks,\n\
 using the granulate generator to fix up the selection duration (this still is not perfect).\n\
 Move the sliders to set the mark numbers."))
					(lambda (w c i)
					  (set! fit-to-mark-one initial-fit-to-mark-one)
					  (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* fit-to-mark-one 1))))
					  (set! fit-to-mark-two initial-fit-to-mark-two)
					  (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* fit-to-mark-two 1)))))))
	      (set! sliders
		    (add-sliders fit-to-mark-dialog
				 (list (list "mark one" 0 initial-fit-to-mark-one 20
					     (lambda (w context info)
					       (set! fit-to-mark-one (/ (.value info) 1)))
					     1)
				       (list "mark two" 0 initial-fit-to-mark-two 20
					     (lambda (w context info)
					       (set! fit-to-mark-two (/ (.value info) 1)))
					     1))))))
	(activate-dialog fit-to-mark-dialog))

      (add-to-menu marks-menu "Fit selection to marks" (lambda () (post-fit-to-mark-dialog))))

    (add-to-menu marks-menu fit-to-mark-label cp-fit-to-marks))

(set! marks-list (cons (lambda ()
                           (let ((new-label (format #f "Fit selection to marks (~1,2D ~1,2D)" fit-to-mark-one fit-to-mark-two)))
                             (change-menu-label marks-menu fit-to-mark-label new-label)
                             (set! fit-to-mark-label new-label)))
                         marks-list))

;;; -------- Define selection by marks
;;;

(define define-by-mark-one 0)
(define define-by-mark-two 1)
(define define-by-mark-label "Define selection by marks")
(define define-by-mark-dialog #f)

(define (define-selection-via-marks m1 m2)
  (let ((m1sc (mark-home m1))
        (m2sc (mark-home m2)))
    (if (not (equal? m1sc m2sc))
        (snd-error "define-selection-via-marks assumes the marks are in the same channel")
        (let ((beg (min (mark-sample m1) (mark-sample m2)))
              (end (max (mark-sample m1) (mark-sample m2)))
              (snd (car m1sc))
              (chn (cadr m1sc)))
;          (if (selection?)
;              (set! (selection-member #t) #f)) ; clear entire current selection, if any
          (set! (selection-member? snd chn) #t)
          (set! (selection-position snd chn) beg)
          (set! (selection-length snd chn) (1+ (- end beg)))))))

(define (cp-define-by-marks)
 (define-selection-via-marks define-by-mark-one define-by-mark-two))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-define-by-mark-dialog)
        (if (not (Widget? define-by-mark-dialog))
            ;; if define-by-mark-dialog doesn't exist, create it
            (let ((initial-define-by-mark-one 0)
                  (initial-define-by-mark-two 1)
                  (sliders '()))
              (set! define-by-mark-dialog 
		    (make-effect-dialog define-by-mark-label
					(lambda (w context info)
					  (cp-define-by-marks))
					(lambda (w context info)
					  (XtUnmanageChild define-by-mark-dialog))
					(lambda (w context info)
					  (help-dialog "Define selection by marks Help"
						"Selects and highlights area between marks.\n\ Use the sliders to choose the boundary marks."))
					(lambda (w c i)
					  (set! define-by-mark-one initial-define-by-mark-one)
					  (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* define-by-mark-one 1))))
					  (set! define-by-mark-two initial-define-by-mark-two)
					  (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* define-by-mark-two 1)))))))
	      (set! sliders
		    (add-sliders define-by-mark-dialog
				 (list (list "mark one" 0 initial-define-by-mark-one 25
					     (lambda (w context info)
					       (set! define-by-mark-one (/ (.value info) 1)))
					     1)
				       (list "mark two" 0 initial-define-by-mark-two 25
					     (lambda (w context info)
					       (set! define-by-mark-two (/ (.value info) 1)))
					     1))))))
	(activate-dialog define-by-mark-dialog))

      (add-to-menu marks-menu "Define selection by marks" (lambda () (post-define-by-mark-dialog))))

    (add-to-menu marks-menu define-by-mark-label cp-define-by-marks))

(set! marks-list (cons (lambda ()
                           (let ((new-label (format #f "Define selection by marks (~1,2D ~1,2D)" define-by-mark-one define-by-mark-two)))
                             (change-menu-label marks-menu define-by-mark-label new-label)
                             (set! define-by-mark-label new-label)))
                         marks-list))

(add-to-menu marks-menu #f #f)

;;; ------- Start/stop mark sync
;;;

(define mark-sync-number 0)
(define (start-sync) (set! mark-sync-number (+ (mark-sync-max) 1)))
(define (stop-sync) (set! mark-sync-number 0))
(define (click-to-sync id) (set! (mark-sync id) mark-sync-number) #f)
(add-hook! mark-click-hook click-to-sync)

(define m-sync #f)
(define m-sync-label "Mark sync (On)")
(define no-m-sync-label "Mark sync (Off)")

(define (msync!)
  (set! m-sync #t)
  (change-menu-label marks-menu no-m-sync-label m-sync-label)
  (start-sync)
  (mark-sync-color "yellow"))


(define (unmsync!)
  (set! m-sync #f)
  (change-menu-label marks-menu m-sync-label no-m-sync-label)
  (stop-sync))

(add-to-menu marks-menu no-m-sync-label
  (lambda ()
    (if m-sync
        (unmsync!)
        (msync!))))

;(add-to-menu marks-menu "Start mark sync" start-sync)
;(add-to-menu marks-menu "Stop mark sync" stop-sync)

(add-to-menu marks-menu #f #f)

;;; -------- Places marks at loop points specified in the file header
;;;


(add-to-menu marks-menu "Mark sample loop points" mark-loops)


;;; Here is a first stab at the loop dialog (I guessed a lot as to what these buttons
;;; are supposed to do -- have never used these loop points).

(define (change-label widget new-label)
  (let ((str (XmStringCreateLocalized new-label)))
    (XtSetValues widget (list XmNlabelString str))
    (XmStringFree str)))


(define loop-dialog #f)
(define loop-data '(0 0 0 0 0 0 1 1))

(define (update-labels start range end sus-rel range-in-secs)
  (if range-in-secs
      (begin
	(change-label start (format #f "~,3F" (/ (list-ref loop-data (* sus-rel 2)) (srate))))
	(change-label range (format #f "~,3F" (/ (- (list-ref loop-data (+ 1 (* sus-rel 2))) (list-ref loop-data (*  
sus-rel 2))) (srate))))
	(change-label end (format #f "~,3F" (/ (list-ref loop-data (+ 1 (* sus-rel 2))) (srate)))))
      (begin
	(change-label start (format #f "~D" (list-ref loop-data (* sus-rel 2))))
	(change-label range (format #f "~D" (- (list-ref loop-data (+ 1 (* sus-rel 2))) (list-ref loop-data (* sus-rel 2)))))
	(change-label end (format #f "~D" (list-ref loop-data (+ 1 (* sus-rel 2))))))))

(define (create-loop-dialog)
  (if (not (Widget? loop-dialog))
      (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	    (xsave (XmStringCreate "Save" XmFONTLIST_DEFAULT_TAG))
	    (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	    (titlestr (XmStringCreate "Loop Points" XmFONTLIST_DEFAULT_TAG)))
	(set! loop-dialog
	      (XmCreateTemplateDialog (cadr (main-widgets)) "loop-points"
                (list XmNcancelLabelString   xdismiss
		      XmNhelpLabelString     xhelp
		      XmNokLabelString       xsave
		      XmNautoUnmanage        #f
		      XmNdialogTitle         titlestr
		      XmNresizePolicy        XmRESIZE_GROW
	              XmNnoResize            #f
		      XmNbackground          (basic-color)
		      XmNtransient           #f)))
	(XtAddCallback loop-dialog
			XmNcancelCallback (lambda (w context info)
					     (XtUnmanageChild loop-dialog)))
	(XtAddCallback loop-dialog
			XmNhelpCallback (lambda (w context info)
					   (snd-print "set loop points")))
	(XtAddCallback loop-dialog
			XmNokCallback (lambda (w context info)
					 (set! (sound-loop-info) loop-data)))
	(XmStringFree xhelp)
	(XmStringFree xdismiss)
	(XmStringFree titlestr)
	(XmStringFree xsave)
	(let* ((mainform
		(XtCreateManagedWidget "form" xmFormWidgetClass loop-dialog
                  (list XmNleftAttachment      XmATTACH_FORM
		        XmNrightAttachment     XmATTACH_FORM
		        XmNtopAttachment       XmATTACH_FORM
		        XmNbottomAttachment    XmATTACH_WIDGET
		        XmNbottomWidget        (XmMessageBoxGetChild loop-dialog XmDIALOG_SEPARATOR)
			XmNbackground          (basic-color))))
	       (leftform
		(XtCreateManagedWidget "lform" xmFormWidgetClass mainform
                  (list XmNleftAttachment      XmATTACH_FORM
		        XmNrightAttachment     XmATTACH_POSITION
			XmNrightPosition       50
		        XmNtopAttachment       XmATTACH_FORM
		        XmNbottomAttachment    XmATTACH_FORM
			XmNbackground          (basic-color))))
	       (rightform
		(XtCreateManagedWidget "rform" xmFormWidgetClass mainform
                  (list XmNleftAttachment      XmATTACH_WIDGET
			XmNleftWidget          leftform
		        XmNrightAttachment     XmATTACH_FORM
		        XmNtopAttachment       XmATTACH_FORM
		        XmNbottomAttachment    XmATTACH_FORM
			XmNbackground          (basic-color)))))
	  (for-each
	   (lambda (parent top-label offset)
	     (let* ((main-label (XtCreateManagedWidget top-label xmLabelWidgetClass parent
							(list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_NONE)))
		    (main-frame (XtCreateManagedWidget "fr"  xmFrameWidgetClass parent
							(list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_WIDGET
							      XmNtopWidget           main-label
							      XmNbottomAttachment    XmATTACH_FORM
							      XmNshadowThickness     6
							      XmNshadowType          XmSHADOW_ETCHED_OUT)))
		    (frame-form (XtCreateManagedWidget "fform" xmFormWidgetClass main-frame '()))
		    (top-frame (XtCreateManagedWidget "topf" xmFrameWidgetClass frame-form
						        (list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_NONE)))
		    (top-form (XtCreateManagedWidget "tform" xmFormWidgetClass top-frame '()))
		    (left-column (XtCreateManagedWidget "lcol" xmRowColumnWidgetClass top-form
						        (list XmNorientation         XmVERTICAL
							      XmNbackground          (position-color)
							      XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_POSITION
							      XmNrightPosition       40
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_FORM)))
		    (mid-column (XtCreateManagedWidget "lcol" xmFormWidgetClass top-form
						        (list XmNleftAttachment      XmATTACH_WIDGET
							      XmNleftWidget          left-column
							      XmNrightAttachment     XmATTACH_POSITION
							      XmNrightPosition       60
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_FORM)))
		    (right-column (XtCreateManagedWidget "lcol" xmRowColumnWidgetClass top-form
						        (list XmNorientation         XmVERTICAL
							      XmNbackground          (position-color)
							      XmNleftAttachment      XmATTACH_WIDGET
							      XmNleftWidget          mid-column
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_FORM)))
		    (rowlefttop (XtCreateManagedWidget "r1"  xmRowColumnWidgetClass left-column
							(list XmNorientation         XmHORIZONTAL
							      XmNbackground          (position-color)
							      XmNspacing             0)))
		    (leftrange (XtCreateManagedWidget "range" xmPushButtonWidgetClass left-column '()))
		    (rowleftbottom (XtCreateManagedWidget "r1" xmRowColumnWidgetClass left-column
							(list XmNorientation         XmHORIZONTAL
							      XmNbackground          (position-color)
							      XmNspacing             0)))
		    (rowrighttop (XtCreateManagedWidget "r1" xmRowColumnWidgetClass right-column
							(list XmNorientation         XmHORIZONTAL
							      XmNbackground          (position-color)
							      XmNspacing             0)))
		    (rowrightmid (XtCreateManagedWidget "r1" xmRowColumnWidgetClass right-column
							(list XmNorientation         XmHORIZONTAL
							      XmNbackground          (position-color))))
		    (rightsep (XtCreateManagedWidget "rsep"  xmSeparatorWidgetClass rowrightmid
						        (list XmNseparatorType       XmNO_LINE
							      XmNorientation         XmVERTICAL
							      XmNbackground          (position-color)
							      XmNwidth               20)))
		    (rightlock (XtCreateManagedWidget "lock" xmToggleButtonWidgetClass rowrightmid '()))
	;; TODO: implement the lock button
		    (rowrightbottom (XtCreateManagedWidget "r1" xmRowColumnWidgetClass right-column
							(list XmNorientation         XmHORIZONTAL
							      XmNbackground          (position-color)
							      XmNspacing             0)))
		    (midlab1 (XtCreateManagedWidget "0.000" xmLabelWidgetClass mid-column
						     (list    XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_POSITION
							      XmNtopPosition         10
							      XmNbottomAttachment    XmATTACH_NONE)))
		    (midlab2 (XtCreateManagedWidget "0.000" xmLabelWidgetClass mid-column
						     (list    XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_POSITION
							      XmNtopPosition         40
							      XmNbottomAttachment    XmATTACH_NONE)))
		    (midlab3 (XtCreateManagedWidget "0.000" xmLabelWidgetClass mid-column
						     (list    XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_NONE
							      XmNbottomAttachment    XmATTACH_POSITION
							      XmNbottomPosition      90)))
		    (bottom-form (XtCreateManagedWidget "bform" xmFormWidgetClass frame-form
							(list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_WIDGET
							      XmNtopWidget           top-frame
							      XmNbottomAttachment    XmATTACH_FORM)))
		    (bottom-left (XtCreateManagedWidget "bleft" xmFormWidgetClass bottom-form
							(list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_NONE
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_FORM)))
		    (bottom-right (XtCreateManagedWidget "bright" xmFrameWidgetClass bottom-form
							(list XmNleftAttachment      XmATTACH_WIDGET
							      XmNleftWidget          bottom-left
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_FORM)))
		    (bottom-left-label (XtCreateManagedWidget "Loop Mode" xmLabelWidgetClass bottom-left
							(list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_FORM
							      XmNbottomAttachment    XmATTACH_NONE)))
		    (bottom-left-button (XtCreateManagedWidget "forwards" xmPushButtonWidgetClass bottom-left
							(list XmNleftAttachment      XmATTACH_FORM
							      XmNrightAttachment     XmATTACH_FORM
							      XmNtopAttachment       XmATTACH_WIDGET
							      XmNtopWidget           bottom-left-label
							      XmNbottomAttachment    XmATTACH_FORM)))
		    (range-in-secs #t))
	       (let ((mode 1))
		 (XtAddCallback bottom-left-button
				 XmNactivateCallback
				  (lambda (w context info)
				    (if (= mode 1)
					(set! mode 2)
					(set! mode 1))
				    (list-set! loop-data (+ offset 6) mode)
				    (change-label w (if (= mode 1) "forward" "forw/back")))))
	       (XtAddCallback leftrange XmNactivateCallback
			       (lambda (w c i)
				 (set! range-in-secs (not range-in-secs))
				 (update-labels midlab1 midlab2 midlab3 offset range-in-secs)))
	       (for-each
		(lambda (rparent loc)
		  (let* ((farleft (XtCreateManagedWidget "<<" xmPushButtonWidgetClass rparent '()))
			 (stopleft (XtCreateManagedWidget " O " xmPushButtonWidgetClass rparent '()))
			 (lotsleft (XtCreateManagedWidget "<< " xmPushButtonWidgetClass rparent '()))
			 (someleft (XtCreateManagedWidget " < " xmPushButtonWidgetClass rparent '()))
			 (sus-rel-start (* offset 2)))

		    (XtAddCallback farleft XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) 0 (list-ref loop-data sus-rel-start))))
					(list-set! loop-data (+ loc (* offset 2)) ml)
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))
		    (XtAddCallback stopleft XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) 0 (list-ref loop-data sus-rel-start))))
					(list-set! loop-data (+ loc (* offset 2)) ml)
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))
		    (XtAddCallback lotsleft XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) 0 (list-ref loop-data sus-rel-start))))
					(list-set! loop-data (+ loc (* offset 2)) (max ml (- (list-ref loop-data (+  
loc (* offset 2))) 10)))
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))
		    (XtAddCallback someleft XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) 0 (list-ref loop-data sus-rel-start))))
					(list-set! loop-data (+ loc (* offset 2)) (max ml (- (list-ref loop-data (+  
loc (* offset 2))) 1)))
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))))
		(list rowlefttop rowleftbottom)
		(list 0 1))
	       (for-each
		(lambda (rparent loc)
		  (let* ((someright (XtCreateManagedWidget " > " xmPushButtonWidgetClass rparent '()))
			 (lotsright (XtCreateManagedWidget " >>" xmPushButtonWidgetClass rparent '()))
			 (stopright (XtCreateManagedWidget " O " xmPushButtonWidgetClass rparent '()))
			 (farright (XtCreateManagedWidget ">>" xmPushButtonWidgetClass rparent '()))
			 (sus-rel-start (* offset 2)))

		    (XtAddCallback farright XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) (list-ref loop-data (+ sus-rel-start 1)) (frames))))
					(list-set! loop-data (+ loc (* offset 2)) ml)
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))
		    (XtAddCallback stopright XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) (list-ref loop-data (+ sus-rel-start 1)) (frames))))
					(list-set! loop-data (+ loc (* offset 2)) ml)
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))
		    (XtAddCallback lotsright XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) (list-ref loop-data (+ sus-rel-start 1)) (frames))))
					(list-set! loop-data (+ loc (* offset 2)) (min ml (+ (list-ref loop-data (+  
loc (* offset 2))) 10)))
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))
		    (XtAddCallback someright XmNactivateCallback
				    (lambda (w c i)
				      (let ((ml (if (= loc 0) (list-ref loop-data (+ sus-rel-start 1)) (frames))))
					(list-set! loop-data (+ loc (* offset 2)) (min ml (+ (list-ref loop-data (+  
loc (* offset 2))) 1)))
					(update-labels midlab1 midlab2 midlab3 offset range-in-secs))))))
		(list rowrighttop rowrightbottom)
		(list 0 1))))
	   (list leftform rightform)
	   (list "Sustain" "Release")
	   (list 0 1)))
	(for-each-child
	 loop-dialog
	 (lambda (n)
	   (if (and (XtIsWidget n)
		    (not (XmIsRowColumn n))
		    (not (XmIsSeparator n)))
	       (begin
		 (XmChangeColor n (basic-color))
		 (if (XmIsToggleButton n)
		     (XtVaSetValues n (list XmNselectColor
					      (let* ((col (XColor))
						     (dpy (XtDisplay (cadr (main-widgets))))
						     (scr (DefaultScreen dpy))
						     (cmap (DefaultColormap dpy scr)))
						(XAllocNamedColor dpy cmap "yellow" col col)
						(.pixel col)))))))))
	))
  (XtManageChild loop-dialog))


;(create-loop-dialog)
(add-to-menu marks-menu "Show loop editor" create-loop-dialog)


(add-to-menu marks-menu #f #f)

;;; -------- Delete all marks 
;;;

(add-to-menu marks-menu "Delete all marks" delete-marks)

