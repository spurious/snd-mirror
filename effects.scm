(use-modules (ice-9 format) (ice-9 common-list))

(define pi 3.141592653589793)
(define effects-list '()) ; menu labels are updated to show current settings

(define effects-menu (add-to-main-menu "Effects" (lambda ()
						   (define (update-label effects)
						     (if (not (null? effects))
							 (begin
							   ((car effects))
							   (update-label (cdr effects)))))
						   (update-label effects-list))))
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
	  (apply for-each
		 (lambda (snd chn)
		   (if (= (sync snd) snc)
		       (map-chan (func) #f #f origin snd chn)))
		 (all-chans))
	  (map-chan (func) #f #f origin)))))

(define (make-effect-dialog label ok-callback dismiss-callback help-callback reset-callback)
  ;; make a standard dialog
  (let* ((xdismiss (|XmStringCreate "Dismiss" |XmFONTLIST_DEFAULT_TAG))
	 (xhelp (|XmStringCreate "Help" |XmFONTLIST_DEFAULT_TAG))
	 (xok (|XmStringCreate "DoIt" |XmFONTLIST_DEFAULT_TAG))
	 (titlestr (|XmStringCreate label |XmFONTLIST_DEFAULT_TAG))
	 (new-dialog (|XmCreateTemplateDialog
		       (|Widget (cadr (main-widgets))) label
		       (list |XmNcancelLabelString   xdismiss
			     |XmNhelpLabelString     xhelp
			     |XmNokLabelString       xok
			     |XmNautoUnmanage        #f
			     |XmNdialogTitle         titlestr
			     |XmNresizePolicy        |XmRESIZE_GROW
			     |XmNnoResize            #f
			     |XmNbackground          (|Pixel (snd-pixel (basic-color)))
			     |XmNtransient           #f))))
    (for-each
     (lambda (button)
       (|XtVaSetValues
	 (|XmMessageBoxGetChild new-dialog button)
	 (list |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color)))
		|XmNbackground (|Pixel (snd-pixel (basic-color))))))
     (list |XmDIALOG_HELP_BUTTON |XmDIALOG_CANCEL_BUTTON |XmDIALOG_OK_BUTTON))
    
    (|XtAddCallback new-dialog |XmNcancelCallback dismiss-callback) ; "Dismiss"
    (|XtAddCallback new-dialog |XmNhelpCallback help-callback)  ; "Help"
    (|XtAddCallback new-dialog |XmNokCallback ok-callback)    ; "DoIt"

    (if reset-callback
	;; add a Reset button
	(let ((reset-button (|XtCreateManagedWidget "Reset" |xmPushButtonWidgetClass new-dialog
			      (list |XmNbackground (|Pixel (snd-pixel (basic-color)))
				    |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color)))))))
	  (|XtAddCallback reset-button |XmNactivateCallback reset-callback)))

    (|XmStringFree xhelp)
    (|XmStringFree xok)
    (|XmStringFree xdismiss)
    (|XmStringFree titlestr)
    new-dialog))

(define (add-sliders dialog sliders)
  ;; sliders is a list of lists, each inner list being (title low initial high callback scale)
  ;; returns list of widgets (for reset callbacks)
  (let ((mainform 
	 (|XtCreateManagedWidget "formd" |xmRowColumnWidgetClass dialog
	   (list |XmNleftAttachment      |XmATTACH_FORM
		 |XmNrightAttachment     |XmATTACH_FORM
		 |XmNtopAttachment       |XmATTACH_FORM
		 |XmNbottomAttachment    |XmATTACH_WIDGET
		 |XmNbottomWidget        (|XmMessageBoxGetChild dialog |XmDIALOG_SEPARATOR)
		 |XmNbackground          (|Pixel (snd-pixel (highlight-color)))
		 |XmNorientation         |XmVERTICAL))))
    (map 
     (lambda (slider-data)
       (let* ((title (|XmStringCreate (list-ref slider-data 0) |XmFONTLIST_DEFAULT_TAG))
	      (low (list-ref slider-data 1))
	      (initial (list-ref slider-data 2))
	      (high (list-ref slider-data 3))
	      (func (list-ref slider-data 4))
	      (scale (list-ref slider-data 5))
	      (new-slider 
	       (|XtCreateManagedWidget (car slider-data) |xmScaleWidgetClass mainform
                  (list |XmNorientation   |XmHORIZONTAL
			|XmNshowValue     #t
			|XmNminimum       (inexact->exact (* low scale))
			|XmNmaximum       (inexact->exact (* high scale))
			|XmNvalue         (inexact->exact (* initial scale))
			|XmNdecimalPoints (if (= scale 1000) 3 (if (= scale 100) 2 (if (= scale 10) 1 0)))
			|XmNtitleString   title
			;|XmNborderWidth   1
			|XmNbackground    (|Pixel (snd-pixel (basic-color)))))))
	 (|XmStringFree title)
	 (|XtAddCallback new-slider |XmNvalueChangedCallback func)
	 new-slider))
     sliders)))

(define yellow-pixel
  (let ((pix #f))
    (lambda ()
      (if (not pix)
	  (let* ((shell (|Widget (cadr (main-widgets))))
		 (dpy (|XtDisplay shell))
		 (scr (|DefaultScreen dpy))
		 (cmap (|DefaultColormap dpy scr))
		 (col (|XColor)))
	       (if (= (|XAllocNamedColor dpy cmap "yellow" col col) 0)
		   (snd-error "can't allocate yellow!")
		   (set! pix (|pixel col)))))
      pix)))

(define (add-target mainform target-callback)
  ;; add a set of 3 radio buttons at the bottom of the main section for choice between sound, selection, between-marks
  ;;   target-callback should take one arg, a symbol: 'sound, 'selection, 'marks, and apply the effect accordingly (upon "DoIt")
  (let* ((sep (|XtCreateManagedWidget "sep" |xmSeparatorWidgetClass mainform
		(list |XmNorientation      |XmHORIZONTAL
		      |XmNseparatorType    |XmSHADOW_ETCHED_OUT
		      ;|XmNheight          4
		      |XmNbackground       (|Pixel (snd-pixel (basic-color))))))
	 (rc (|XtCreateManagedWidget "rc"  |xmRowColumnWidgetClass mainform
                (list |XmNtopAttachment    |XmATTACH_WIDGET
		      |XmNtopWidget        sep
		      |XmNbottomAttachment |XmATTACH_FORM
		      |XmNleftAttachment   |XmATTACH_FORM
		      |XmNrightAttachment  |XmATTACH_FORM
		      |XmNorientation      |XmHORIZONTAL
		      |XmNbackground       (|Pixel (snd-pixel (basic-color)))
		      |XmNradioBehavior    #t
		      |XmNradioAlwaysOne   #t
		      |XmNentryClass       |xmToggleButtonWidgetClass
		      |XmNisHomogeneous    #t))))
    (map 
     (lambda (name type on)
       (|XtCreateManagedWidget name |xmToggleButtonWidgetClass rc
                (list |XmNbackground       (|Pixel (snd-pixel (basic-color)))
		      |XmNset              on
		      |XmNselectColor      (yellow-pixel)
		      |XmNarmCallback      (list (lambda (w c i) (target-callback type)) #f))))
     (list "entire sound" "selection" "between marks")
     (list 'sound 'selection 'marks)
     (list #t #f #f))))

(define (activate-dialog dialog)
  (if (not (|XtIsManaged dialog))
      (|XtManageChild dialog)
      (raise-dialog dialog)))


;;; -------- insert silence (at cursor, silence-amount in secs)
(define silence-amount .1)
(define silence-label "Add silence")
(define silence-dialog #f)

(define (cp-silence)
"Add silence adds the requested amount of silence at the cursor"
    (insert-silence (cursor)
       (inexact->exact (* (srate) silence-amount))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-silence-dialog)
        (if (not (|Widget? silence-dialog))
            ;; if silence-dialog doesn't exist, create it
	    (let ((initial-silence-amount 0.1)
		  (sliders '()))
	      (set! silence-dialog 
		    (make-effect-dialog silence-label
					(lambda (w context info) (cp-silence))
					(lambda (w context info) (|XtUnmanageChild silence-dialog))
					(lambda (w context info)
					  (help-dialog "Add silence Help"
						       "move the slider to change the silence amount (in secs)"))
					(lambda (w c i)
					  (set! silence-amount initial-silence-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* silence-amount 100)))))))
	      (set! sliders 
		    (add-sliders silence-dialog
				 (list (list "silence" 0.0 initial-silence-amount 5.0 
					     (lambda (w context info)
					       (set! silence-amount (/ (|value info) 100.0)))
					     100))))))
	(activate-dialog silence-dialog))

      (add-to-menu effects-menu "Add silence" (lambda () (post-silence-dialog))))

    (add-to-menu effects-menu silence-label cp-silence))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Add silence (~1,2F)"  silence-amount)))
                             (change-menu-label effects-menu silence-label new-label)
                             (set! silence-label new-label)))
                         effects-list))


;;; -------- gain (gain set by gain-amount)
(define gain-amount 1.0)
(define gain-label "Gain")
(define gain-dialog #f)
(define gain-target 'sound)

(define (cp-gain)
  "gain scales amplitude by gain amount"
  (if (eq? gain-target 'sound)
      (scale-by gain-amount)
      (if (eq? gain-target 'selection)
	  (if (selection?)
	      (scale-selection-by gain-amount)
	      (snd-print "no selection"))
	  (snd-print "can't apply gain between marks yet"))))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-gain-dialog)
        (if (not (|Widget? gain-dialog))
            ;; if gain-dialog doesn't exist, create it
            (let ((initial-gain-amount 1.0)
		  (sliders '()))
              (set! gain-dialog 
		    (make-effect-dialog gain-label
					(lambda (w context info) (cp-gain))
					(lambda (w context info) (|XtUnmanageChild gain-dialog))
					(lambda (w context info)
					  (help-dialog "Gain Help"
						       "move the slider to change the gain scaling amount"))
					(lambda (w c i)
					  (set! gain-amount initial-gain-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gain-amount 100)))))))
	      (set! sliders 
		    (add-sliders gain-dialog
				 (list (list "gain" 0.0 initial-gain-amount 5.0 
					     (lambda (w context info)
					       (set! gain-amount (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) 
			  (lambda (target)
			    (set! gain-target target)))))
        (activate-dialog gain-dialog))

      (add-to-menu effects-menu "Gain" (lambda () (post-gain-dialog))))

    (add-to-menu effects-menu gain-label cp-gain))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Gain (~1,2F)"  gain-amount)))
                             (change-menu-label effects-menu gain-label new-label)
                             (set! gain-label new-label)))
                         effects-list))



;;; -------- normalize (normalize set by normalization)
(define normalization 1.0)
(define normalize-label "Normalize")
(define normalize-dialog #f)

(define (cp-normalize)
"normalize scales amplitude by normalization amount"
 (scale-to normalization))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-normalize-dialog)
        (if (not (|Widget? normalize-dialog))
            ;; if normalize-dialog doesn't exist, create it
            (let ((initial-normalization 1.0)
                  (sliders '()))
              (set! normalize-dialog 
		    (make-effect-dialog normalize-label
					(lambda (w context info) (cp-normalize))
					(lambda (w context info) (|XtUnmanageChild normalize-dialog))
					(lambda (w context info)
					  (help-dialog "Normalize Help"
						       "move the slider to change the normalization scaling amount"))
					(lambda (w c i)
					  (set! normalization initial-normalization)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* normalization 100)))))))
	      (set! sliders
		    (add-sliders normalize-dialog
				 (list (list "normalize" 0.0 initial-normalization 1.0 
					     (lambda (w context info)
					       (set! gain-amount (/ (|value info) 100.0)))
					     100))))))

	(activate-dialog normalize-dialog))

      (add-to-menu effects-menu "Normalize" (lambda () (post-normalize-dialog))))

    (add-to-menu effects-menu normalize-label cp-normalize))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Normalize (~1,2F)"  normalization)))
                             (change-menu-label effects-menu normalize-label new-label)
                             (set! normalize-label new-label)))
                         effects-list))


;;; -------- gate (gate set by gate-amount)
(define gate-amount 1.0)
(define gate-label "Gate")
(define gate-dialog #f)

(define omit-silence #f)

(define (squelch-one-channel silence snd chn)
  (let* ((buffer-size 128)
         (buffer0 #f)
	 (tmp #f)
	 (sum0 0.0)
	 (buffer1 (make-vct buffer-size))
         (chan-samples (frames snd chn))
         (pad-samples (+ chan-samples buffer-size))
         (tempfilename (snd-tempnam))
         (new-file (open-sound-file tempfilename 1 (srate snd)))
         (reader (make-sample-reader 0 snd chn))
	 (buffers-per-progress-report (inexact->exact (/ chan-samples (* buffer-size 20)))))

    (start-progress-report snd)

    (do ((i 0 (+ i buffer-size))
	 (j 0))
        ((>= i pad-samples))
      (let ((sum 0.0))
        (do ((j 0 (+ j 1)))
            ((= j buffer-size))
          (let ((val (next-sample reader)))
            (vct-set! buffer1 j val)
            (set! sum (+ sum (* val val)))))
        (if buffer0
            (let ((all-zeros #f))
              (if (> sum silence)
                  (if (<= sum0 silence)
                      (do ((j 0 (+ j 1))
                           (incr 0.0 (+ incr (/ 1.0 buffer-size))))
                          ((= j buffer-size))
                        (vct-set! buffer0 j (* (vct-ref buffer0 j) incr))))
                  (if (<= sum0 silence)
                      (begin
                        (vct-fill! buffer0 0.0)
                        (set! all-zeros #t))
                      (do ((j 0 (+ j 1))
                           (incr 1.0 (- incr (/ 1.0 buffer-size))))
                          ((= j buffer-size))
                        (vct-set! buffer0 j (* (vct-ref buffer0 j) incr)))))
              (if (not (and omit-silence all-zeros))
                  (vct->sound-file new-file buffer0 buffer-size)))
            (set! buffer0 (make-vct buffer-size)))
	(set! j (+ j 1))
	(if (>= j buffers-per-progress-report)
	    (begin
	      (set! j 0)
	      (progress-report (/ i pad-samples) "squelch-one-channel" chn 1 snd)))
        (set! tmp buffer0)
        (set! buffer0 buffer1)
        (set! buffer1 tmp)
        (set! sum0 sum)))

    (finish-progress-report snd)
    (free-sample-reader reader)
    (close-sound-file new-file (* chan-samples 4))
    (set! (samples 0 chan-samples snd chn) tempfilename)))


(define (cp-gate)
"Gate: higher values remove more of the sound"
  (let ((snc (sync)))
    (if (> snc 0)
       (apply map
              (lambda (snd chn)
                (if (= (sync snd) snc)
                    (squelch-one-channel gate-amount snd chn)))
              (all-chans))
       (squelch-one-channel gate-amount (selected-sound) (selected-channel)))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-gate-dialog)
        (if (not (|Widget? gate-dialog))
            ;; if gate-dialog doesn't exist, create it
            (let ((initial-gate-amount 1.0)
                  (sliders '()))
              (set! gate-dialog 
		    (make-effect-dialog gate-label
					(lambda (w context info) (cp-gate))
					(lambda (w context info) (|XtUnmanageChild gate-dialog))
					(lambda (w context info)
					  (help-dialog "Gate Help"
						       "move the slider to change the gate intensity"))
					(lambda (w c i)
					  (set! gate-amount initial-gate-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gate-amount 100)))))))
	      (set! sliders
		    (add-sliders gate-dialog
				 (list (list "gate" 0.0 initial-gate-amount 5.0 
					     (lambda (w context info)
					       (set! gate-amount (/ (|value info) 100.0)))
					     100))))
	      ;; now add a toggle button setting omit-silence 
	      ;;  (need to use XtParent here because the containing RowColumn widget is
	      ;;  hidden in add-sliders -- prehaps it should be returned in the slider list)
	      
	      (let* ((s1 (|XmStringCreateLocalized "Omit silence"))
		     (toggle
		      (|XtCreateManagedWidget "Omit silence" |xmToggleButtonWidgetClass (|XtParent (car sliders))
		        (list |XmNselectColor  (|Pixel (snd-pixel (pushed-button-color)))
			      |XmNbackground   (|Pixel (snd-pixel (basic-color)))
			      |XmNvalue        omit-silence
			      ;|XmNborderWidth  1
			      |XmNlabelString  s1))))
		(|XmStringFree s1)
		(|XtAddCallback toggle |XmNvalueChangedCallback (lambda (w c i)
								  (set! omit-silence (|set i)))))))
        (activate-dialog gate-dialog))

      (add-to-menu effects-menu "Gate" (lambda () (post-gate-dialog))))

    (add-to-menu effects-menu gate-label cp-gate))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Gate (~1,2F)"  gate-amount)))
                             (change-menu-label effects-menu gate-label new-label)
                             (set! gate-label new-label)))
                         effects-list))


;;; -------- freqdiv (freqdiv set by freqdiv-amount)
(define freqdiv-amount 5.0)
(define freqdiv-label "Frequency division")
(define freqdiv-dialog #f)

(define (cp-freqdiv)
"freqdiv does weird things by freqdiv amount"
 (freqdiv freqdiv-amount))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-freqdiv-dialog)
        (if (not (|Widget? freqdiv-dialog))
            ;; if freqdiv-dialog doesn't exist, create it
            (let ((initial-freqdiv-amount 5.0)
                  (sliders '()))
              (set! freqdiv-dialog 
		    (make-effect-dialog freqdiv-label
					(lambda (w context info) (cp-freqdiv))
					(lambda (w context info) (|XtUnmanageChild freqdiv-dialog))
					(lambda (w context info)
					  (help-dialog "Frequency division Help"
						       "move the slider to change the frequency division factor"))
					(lambda (w c i)
					  (set! freqdiv-amount initial-freqdiv-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact freqdiv-amount))))))
	      (set! sliders
		    (add-sliders freqdiv-dialog
				 (list (list "frequency division" 0.0 initial-freqdiv-amount 100.0 
					     (lambda (w context info)
					       (set! freqdiv-amount (|value info)))
					     1))))))
	(activate-dialog freqdiv-dialog))

      (add-to-menu effects-menu "Frequency division" (lambda () (post-freqdiv-dialog))))

    (add-to-menu effects-menu freqdiv-label cp-freqdiv))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Frequency division (~1,2F)"  freqdiv-amount)))
                             (change-menu-label effects-menu freqdiv-label new-label)
                             (set! freqdiv-label new-label)))
                         effects-list))


;;; -------- adsat (adsat set by adsat-size)
(define adsat-size 4.0)
(define adsat-label "Adaptive saturation")
(define adsat-dialog #f)

(define (cp-adsat)
"adsat does weird stuff by adsat size"
 (adsat adsat-size))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-adsat-dialog)
        (if (not (|Widget? adsat-dialog))
            ;; if adsat-dialog doesn't exist, create it
            (let ((initial-adsat-size 4.0)
                  (sliders '()))
              (set! adsat-dialog 
		    (make-effect-dialog adsat-label
					(lambda (w context info) (cp-adsat))
					(lambda (w context info) (|XtUnmanageChild adsat-dialog))
					(lambda (w context info)
					  (help-dialog "Adaptive saturation Help"
						       "move the slider to change the adsat scaling size"))
					(lambda (w c i)
					  (set! adsat-size initial-adsat-size)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* adsat-size 100)))))))
	      (set! sliders
		    (add-sliders adsat-dialog
				 (list (list "adaptive saturation" 0.0 initial-adsat-size 10.0 
					     (lambda (w context info)
					       (set! adsat-size (/ (|value info) 100.0)))
					     100))))))
	(activate-dialog adsat-dialog))

      (add-to-menu effects-menu "Adaptive saturation" (lambda () (post-adsat-dialog))))

    (add-to-menu effects-menu adsat-label cp-adsat))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Adaptive saturation (~1,2F)"  adsat-size)))
                             (change-menu-label effects-menu adsat-label new-label)
                             (set! adsat-label new-label)))
                         effects-list))



;;; -------- AM (am set by am-amount)
(define am-amount 100.0)
(define am-label "Amplitude modulation")
(define am-dialog #f)


(define (cp-am)
"amplitude modulation"
 (map-chan (am am-amount)))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-am-dialog)
        (if (not (|Widget? am-dialog))
            ;; if am-dialog doesn't exist, create it
            (let ((initial-am-amount 100.0)
                  (sliders '()))
              (set! am-dialog 
		    (make-effect-dialog am-label
					(lambda (w context info) (cp-am))
					(lambda (w context info) (|XtUnmanageChild am-dialog))
					(lambda (w context info)
					  (help-dialog "Amplitude modulation Help"
						       "move the slider to change the amplitude modulation factor"))
					(lambda (w c i)
					  (set! am-amount initial-am-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact am-amount))))))
	      (set! sliders
		    (add-sliders am-dialog
				 (list (list "amplitude modulation" 0.0 initial-am-amount 1000.0 
					     (lambda (w context info)
					       (set! am-amount (|value info)))
					     1))))))
	(activate-dialog am-dialog))

      (add-to-menu effects-menu "Amplitude modulation" (lambda () (post-am-dialog))))

    (add-to-menu effects-menu am-label cp-am))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Amplitude modulation (~1,2F)"  am-amount)))
                             (change-menu-label effects-menu am-label new-label)
                             (set! am-label new-label)))
                         effects-list))


;;; -------- pitch and time scaling by granular synthesis and sampling rate conversion

(define pitch-scale 1.0)
(define time-scale 1.0)
(define expsrc-label "Time/Pitch scaling")
(define expsrc-dialog #f)

(define (cp-expsrc)
  (save-controls)
  (reset-controls)
  (set! (speed-control) pitch-scale)
  (let ((new-time (* pitch-scale time-scale)))
    (if (not (= new-time 1.0))
        (begin
          (set! (expand-control?) #t)
          (set! (expand-control) new-time))))
  (apply-controls)
  (restore-controls))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-expsrc-dialog)
        (if (not (|Widget? expsrc-dialog))
            ;; if expsrc-dialog doesn't exist, create it
            (let ((initial-time-scale 1.0)
                  (initial-pitch-scale 1.0)
                  (sliders '()))
              (set! expsrc-dialog 
		    (make-effect-dialog expsrc-label
					(lambda (w context info) (cp-expsrc))
					(lambda (w context info) (|XtUnmanageChild expsrc-dialog))
					(lambda (w context info)
					  (help-dialog "Time/Pitch scaling Help"
						       "move the sliders to change the tims/pitch scaling amounts"))
					(lambda (w c i)
					  (set! time-scale initial-time-scale)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* time-scale 1000))))
					  (set! pitch-scale initial-pitch-scale)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* pitch-scale 1000)))))))
	      (set! sliders
		    (add-sliders expsrc-dialog
				 (list (list "time scale" 0.0 initial-time-scale 5.0
					     (lambda (w context info)
					       (set! time-scale (/ (|value info) 100.00)))
					     1000)
				       (list "pitch scale" 0.0 initial-pitch-scale 5.0
					     (lambda (w context info)
					       (set! pitch-scale (/ (|value info) 100.00)))
					     1000))))))
	(activate-dialog expsrc-dialog))

      (add-to-menu effects-menu "Time/Pitch scaling" (lambda () (post-expsrc-dialog))))

    (add-to-menu effects-menu expsrc-label cp-expsrc))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Time/Pitch scaling (~1,2F ~1,2F)" time-scale pitch-scale)))
                             (change-menu-label effects-menu expsrc-label new-label)
                             (set! expsrc-label new-label)))
                         effects-list))



;;; -------- Reverb 
;;; -------- very nice reverb actually
;;;

(define reverb-amount 0.1)
(define reverb-filter 0.5)
(define reverb-feedback 1.0)
(define reverb-label "Reverb")
(define reverb-dialog #f)

(define (cp-reverb)
  "Reverb adds reverberation scaled by reverb amount, lowpass filtering, and feedback"
  (save-controls)
  (reset-controls)
  (set! (reverb-control?) #t)
  (set! (reverb-control-scale) reverb-amount)
  (set! (reverb-control-lowpass) reverb-filter)
  (set! (reverb-control-feedback) reverb-feedback)
  (apply-controls)
  (restore-controls))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-reverb-dialog)
        (if (not (|Widget? reverb-dialog))
            ;; if reverb-dialog doesn't exist, create it
            (let ((initial-reverb-amount 0.1)
                  (initial-reverb-filter 0.5)
                  (initial-reverb-feedback 1.09)
                  (sliders '()))
              (set! reverb-dialog 
		    (make-effect-dialog reverb-label
					(lambda (w context info)
					  (cp-reverb))
					(lambda (w context info)
					  (|XtUnmanageChild reverb-dialog))
					(lambda (w context info)
					  (help-dialog "Reverb Help"
						       "move the sliders to change the reverb amount, lowpass filter, and feedback"))
					(lambda (w c i)
					  (set! reverb-amount initial-reverb-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* reverb-amount 100))))
					  (set! reverb-filter initial-reverb-filter)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* reverb-filter 100))))
					  (set! reverb-feedback initial-reverb-feedback)
					  (|XtSetValues (caddr sliders) (list |XmNvalue (inexact->exact (* reverb-feedback 100)))))))
	      (set! sliders
		    (add-sliders reverb-dialog
				 (list (list "reverb amount" 0.0 initial-reverb-amount 1.0
					     (lambda (w context info)
					       (set! reverb-amount (/ (|value info) 100.0)))
					     100)
				       (list "reverb filter" 0.0 initial-reverb-filter 1.0
					     (lambda (w context info)
					       (set! reverb-filter (/ (|value info) 100.0)))
					     100)
				       (list "reverb feedback" 0.0 initial-reverb-feedback 1.25
					     (lambda (w context info)
					       (set! reverb-feedback (/ (|value info) 100.0)))
					     100))))))
	(activate-dialog reverb-dialog))

      (add-to-menu effects-menu "Reverb" (lambda () (post-reverb-dialog))))

    (add-to-menu effects-menu reverb-label cp-reverb))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Reverb (~1,2F ~1,2F ~1,2F)" reverb-amount reverb-filter reverb-feedback)))
                             (change-menu-label effects-menu reverb-label new-label)
                             (set! reverb-label new-label)))
                         effects-list))

;;; -------- Echo (controlled by delay-time and echo-amount)
;;;
;;;

(define delay-time .5) ; i.e. delay between echoes
(define echo-amount .2)
(define echo-label "Echo")
(define echo-dialog #f)

(define (cp-echo)
  "echo adds echos spaced by delay-time seconds and scaled by echo-amount"
  (let ((del (make-delay (round (* delay-time (srate))))))
    (lambda (inval)
      (+ inval
         (delay del
                (* echo-amount (+ (tap del) inval)))))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin  

      (define (post-echo-dialog)
        (if (not (|Widget? echo-dialog))
            ;; if echo-dialog doesn't exist, create it
            (let ((initial-delay-time 0.5)
                  (initial-echo-amount 0.2)
                  (sliders '()))
              (set! echo-dialog 
		    (make-effect-dialog echo-label
					(lambda (w context info)
					  (map-chan-with-sync (lambda () (cp-echo)) "echo" ))
					(lambda (w context info) (|XtUnmanageChild echo-dialog))
					(lambda (w context info)
					  (help-dialog "Echo Help"
						       "move the sliders to change the delay time and echo amount"))
					(lambda (w c i)   
					  (set! delay-time initial-delay-time)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* delay-time 100))))
					  (set! echo-amount initial-echo-amount)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* echo-amount 100)))))))
	      (set! sliders
		    (add-sliders echo-dialog
				 (list (list "delay time" 0.0 initial-delay-time 2.0
					     (lambda (w context info)
					       (set! delay-time (/ (|value info) 100.0)))
					     100)
				       (list "echo amount" 0.0 initial-echo-amount 1.0
					     (lambda (w context info)
					       (set! echo-amount (/ (|value info) 100.0)))
					     100))))))
	(activate-dialog echo-dialog))

      (add-to-menu effects-menu "Echo"
                   (lambda ()
                     (post-echo-dialog))))

    (add-to-menu effects-menu echo-label cp-echo))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Delay time/amount (~1,2F ~1,2F)" delay-time echo-amount)))
                             (change-menu-label effects-menu echo-label new-label)
                             (set! echo-label new-label)))
                         effects-list))


;;; -------- flange (and phasing)
(define flange-speed 2.0)
(define flange-amount 5.0)
(define flange-time 0.001)
(define flange-label "Flange")
(define flange-dialog #f)

(define (cp-flange) ; increase speed and amount to get phaser
  (let* ((ri (make-rand-interp :frequency flange-speed :amplitude flange-amount))
        (len (round (* flange-time (srate))))
        (del (make-delay len :max-size (+ len flange-amount 1))))
    (lambda (inval)
      (* .75 (+ inval 
              (delay del 
                     inval
                     (rand-interp ri)))))))

;(add-to-menu effects-menu "Flange" (lambda () (map-chan-with-sync (lambda () (cp-flange)) "flange")))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-flange-dialog)
        (if (not (|Widget? flange-dialog))
            ;; if flange-dialog doesn't exist, create it
            (let ((initial-flange-speed 2.0)
                  (initial-flange-amount 5.0)
                  (initial-flange-time 0.001)
                  (sliders '()))
              (set! flange-dialog 
		    (make-effect-dialog flange-label
					(lambda (w context info)
					  (map-chan-with-sync (lambda () (cp-flange)) "flange"))
					(lambda (w context info)
					  (|XtUnmanageChild flange-dialog))
					(lambda (w context info)
					  (help-dialog "Flange Help"
						       "move the sliders to change the flange speed, amount, and time"))
					(lambda (w c i)
					  (set! flange-speed initial-flange-speed)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* flange-speed 10))))
					  (set! flange-amount initial-flange-amount)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* flange-amount 10))))
					  (set! flange-time initial-flange-time)
					  (|XtSetValues (caddr sliders) (list |XmNvalue (inexact->exact (* flange-time 100)))))))
	      (set! sliders
		    (add-sliders flange-dialog
				 (list (list "flange speed" 0.0 initial-flange-speed 100.0
					     (lambda (w context info)
					       (set! flange-speed (/ (|value info) 10.0)))
					     10)
				       (list "flange amount" 0.0 initial-flange-amount 100.0
					     (lambda (w context info)
					       (set! flange-amount (/ (|value info) 10.0)))
					     10)
				       ;; flange time ought to use a non-linear scale (similar to amp in control panel)
				       (list "flange time" 0.0 initial-flange-time 1.0
					     (lambda (w context info)
					       (set! flange-time (/ (|value info) 100.0)))
					     100))))))
	(activate-dialog flange-dialog))

      (add-to-menu effects-menu "Flange" (lambda () (post-flange-dialog))))

    (add-to-menu effects-menu flange-label cp-flange))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Flange (~1,2F ~1,2F ~1,3F)" flange-speed flange-amount flange-time)))
                             (change-menu-label effects-menu flange-label new-label)
                             (set! flange-label new-label)))
                         effects-list))



;;; -------- contrast (contrast set by contrast-amount)
(define contrast-amount 1.0)
(define contrast-label "Contrast enhancement")
(define contrast-dialog #f)

(define (cp-contrast)
  (let ((peak (maxamp)))
    (save-controls)
    (reset-controls)
    (set! (contrast-control?) #t)
    (set! (contrast-control) contrast-amount)
    (set! (contrast-control-amp) (/ 1.0 peak))
    (set! (amp-control) peak)
    (apply-controls)
    (restore-controls)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-contrast-dialog)
        (if (not (|Widget? contrast-dialog))
            ;; if contrast-dialog doesn't exist, create it
            (let ((initial-contrast-amount 1.0)
                  (sliders '()))
              (set! contrast-dialog 
		    (make-effect-dialog contrast-label
					(lambda (w context info) (cp-contrast))
					(lambda (w context info) (|XtUnmanageChild contrast-dialog))
					(lambda (w context info)
					  (help-dialog "Contrast enhancement Help"
						       "move the slider to change the contrast amount"))
					(lambda (w c i)
					  (set! contrast-amount initial-contrast-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* contrast-amount 100)))))))
	      (set! sliders
		    (add-sliders contrast-dialog
				 (list (list "contrast enhancement" 0.0 initial-contrast-amount 10.0
					     (lambda (w context info)
					       (set! contrast-maount (/ (|value info) 100.0)))
					     100))))))
	(activate-dialog contrast-dialog))

      (add-to-menu effects-menu "Contrast enhancement" (lambda () (post-contrast-dialog))))

    (add-to-menu effects-menu contrast-label cp-contrast))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Contrast enhancement (~1,2F)"  contrast-amount)))
                             (change-menu-label effects-menu contrast-label new-label)
                             (set! contrast-label new-label)))
                         effects-list))

;;; -------- Butterworth filters
;;;
;; translated from CLM butterworth.cl:
;;
;;   Sam Heisz, January 1998
;;   inspired by some unit generators written for Csound by Paris Smaragdis
;;   who based his work on formulas from 
;;   Charles Dodge, Computer music: synthesis, composition, and performance.

(define root-2 (sqrt 2.0))

(define butter filter)

;;; -------- Butterworth band-pass filter

(define (make-butter-band-pass fq bw)
  (let* ((d (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
         (c (/ 1.0 (tan (/ (* pi bw) (srate)))))
         (c1 (/ 1.0 (+ 1.0 c)))
         (c2 0.0)
         (c3 (- c1))
         (c4 (* (- c) d c1))
         (c5 (* (- c 1.0) c1)))
    (make-filter 3
                 (list->vct (list c1 c2 c3))
                 (list->vct (list 0.0 c4 c5)))))

(define band-pass-freq 1000.0)
(define band-pass-bw 100.0)
(define band-pass-label "Band-pass filter")
(define band-pass-dialog #f)

(define (cp-band-pass)
"Butterworth band-pass filter"
  (filter-sound (make-butter-band-pass band-pass-freq band-pass-bw)))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-band-pass-dialog)
        (if (not (|Widget? band-pass-dialog))
            ;; if band-pass-dialog doesn't exist, create it
            (let ((initial-band-pass-freq 1000.0)
                  (initial-band-pass-bw 100.0)
                  (sliders '()))
              (set! band-pass-dialog 
		    (make-effect-dialog band-pass-label
					(lambda (w context info) (cp-band-pass))
					(lambda (w context info) (|XtUnmanageChild band-pass-dialog))
					(lambda (w context info)
					  (help-dialog "Band-pass filter Help"
						       "move the sliders to change the center frequency and bandwidth"))
					(lambda (w c i)
					  (set! band-pass-freq initial-band-pass-freq)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact band-pass-freq)))
					  (set! band-pass-bw initial-band-pass-bw)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact band-pass-bw))))))
	      (set! sliders
		    (add-sliders band-pass-dialog
				 (list (list "band-pass frequency" 0.0 initial-band-pass-freq 10000.0
					     (lambda (w context info)
					       (set! band-pass-freq (|value info)))
					     1)
				       (list "band-pass bandwidth" 0.0 initial-band-pass-bw 1000.0
					     (lambda (w context info)
					       (set! band-pass-bw (|value info)))
					     1))))))
	(activate-dialog band-pass-dialog))

      (add-to-menu effects-menu "Band-pass filter" (lambda () (post-band-pass-dialog))))

    (add-to-menu effects-menu band-pass-label cp-band-pass))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Band-pass filter (~1,2F ~1,2F)" band-pass-freq band-pass-bw)))
                             (change-menu-label effects-menu band-pass-label new-label)
                             (set! band-pass-label new-label)))
                         effects-list))


;;; -------- Butterworth band-reject (notch) filter

(define (make-butter-band-reject fq bw)
  (let* ((d  (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
         (c (tan (/ (* pi bw) (srate))))
         (c1 (/ 1.0 (+ 1.0 c)))
         (c2 (* (- d) c1))
         (c3 c1)
         (c4 c2)
         (c5 (* (- 1.0 c) c1)))
    (make-filter 3
                 (list->vct (list c1 c2 c3))
                 (list->vct (list 0.0 c4 c5)))))

(define notch-freq 100.0)
(define notch-bw 100.0)
(define notch-label "Band-reject filter")
(define notch-dialog #f)

(define (cp-notch)
"Butterworth band-reject filter"
  (filter-sound (make-butter-band-reject notch-freq notch-bw)))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-notch-dialog)
        (if (not (|Widget? notch-dialog))
            ;; if notch-dialog doesn't exist, create it
            (let ((initial-notch-freq 100.0)
                  (initial-notch-bw 100.0)
                  (sliders '()))
              (set! notch-dialog 
		    (make-effect-dialog notch-label
					(lambda (w context info) (cp-notch))
					(lambda (w context info) (|XtUnmanageChild notch-dialog))
					(lambda (w context info)
					  (help-dialog "Band-reject filter Help"
						       "move the sliders to change the center frequency and bandwidth"))
					(lambda (w c i)
					  (set! notch-freq initial-notch-freq)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact notch-freq)))
					  (set! notch-bw initial-notch-bw)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact notch-bw))))))
	      (set! sliders
		    (add-sliders notch-dialog
				 (list (list "band-reject frequency" 0.0 initial-notch-freq 10000.0
					     (lambda (w context info)
					       (set! notch-freq (|value info)))
					     1)
				       (list "band-reject bandwidth" 0.0 initial-notch-bw 1000.0
					     (lambda (w context info)
					       (set! notch-bw (|value info)))
					     1))))))
	(activate-dialog notch-dialog))

      (add-to-menu effects-menu "Band-reject filter" (lambda () (post-notch-dialog))))

    (add-to-menu effects-menu notch-label cp-notch))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Band-reject filter (~1,2F ~1,2F)" notch-freq notch-bw)))
                             (change-menu-label effects-menu notch-label new-label)
                             (set! notch-label new-label)))
                         effects-list))



;;; -------- Butterworth high-pass filter

(define (make-butter-high-pass fq)
  (let* ((r (tan (/ (* pi fq) (srate))))
         (r2 (* r r))
         (c1 (/ 1.0 (+ 1.0 (* r root-2) r2)))
         (c2  (* -2.0 c1))
         (c3 c1)
         (c4 (* 2.0 (- r2 1.0) c1))
         (c5 (* (+ (- 1.0 (* r root-2)) r2) c1)))
    (make-filter 3
                 (list->vct (list c1 c2 c3))
                 (list->vct (list 0.0 c4 c5)))))


(define high-pass-freq 100.0)
(define high-pass-label "High-pass filter")
(define high-pass-dialog #f)

(define (cp-high-pass)
"Butterworth high-pass filter"
  (filter-sound (make-butter-high-pass high-pass-freq)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-high-pass-dialog)
        (if (not (|Widget? high-pass-dialog))
            ;; if high-pass-dialog doesn't exist, create it
            (let ((initial-high-pass-freq 100.0)
                  (sliders '()))
              (set! high-pass-dialog 
		    (make-effect-dialog high-pass-label
					(lambda (w context info) (cp-high-pass))
					(lambda (w context info) (|XtUnmanageChild high-pass-dialog))
					(lambda (w context info)
					  (help-dialog "High-pass Help"
						       "move the slider to change the high-pass frequency"))
					(lambda (w c i)
					  (set! high-pass-freq initial-high-pass-freq)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact high-pass-freq))))))
	      (set! sliders
		    (add-sliders high-pass-dialog
				 (list (list "high-pass frequency" 0.0 initial-high-pass-freq 10000.0
					     (lambda (w context info)
					       (set! high-pass-freq (|value info)))
					     1))))))
	(activate-dialog high-pass-dialog))

      (add-to-menu effects-menu "High-pass filter" (lambda () (post-high-pass-dialog))))

    (add-to-menu effects-menu high-pass-label cp-high-pass))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "High-pass filter (~1,2F)"  high-pass-freq)))
                             (change-menu-label effects-menu high-pass-label new-label)
                             (set! high-pass-label new-label)))
                         effects-list))


;;; -------- Butterworth low-pass filter

(define (make-butter-low-pass fq)
  (let* ((r (/ 1.0 (tan (/ (* pi fq) (srate)))))
         (r2 (* r r))
         (c1 (/ 1.0 (+ 1.0 (* r root-2) r2)))
         (c2 (* 2.0 c1))
         (c3 c1)
         (c4 (* 2.0 (- 1.0 r2) c1))
         (c5  (* (+ (- 1.0 (* r root-2)) r2) c1)))
    (make-filter 3
                 (list->vct (list c1 c2 c3))
                 (list->vct (list 0.0 c4 c5)))))

(define low-pass-freq 1000.0)
(define low-pass-label "Low-pass filter")
(define low-pass-dialog #f)

(define (cp-low-pass)
"Butterworth low-pass filter"
  (filter-sound (make-butter-low-pass low-pass-freq)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-low-pass-dialog)
        (if (not (|Widget? low-pass-dialog))
            ;; if low-pass-dialog doesn't exist, create it
            (let ((initial-low-pass-freq 1000.0)
                  (sliders '()))
              (set! low-pass-dialog 
		    (make-effect-dialog low-pass-label
					(lambda (w context info) (cp-low-pass))
					(lambda (w context info) (|XtUnmanageChild low-pass-dialog))
					(lambda (w context info)
					  (help-dialog "Low-pass Help"
						       "move the slider to change the low-pass frequency"))
					(lambda (w c i)
					  (set! low-pass-freq initial-low-pass-freq)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact low-pass-freq))))))
	      (set! sliders
		    (add-sliders low-pass-dialog
				 (list (list "low-pass frequency" 0.0 initial-low-pass-freq 10000.0
					     (lambda (w context info)
					       (set! low-pass-freq (|value info)))
					     1))))))
	(activate-dialog low-pass-dialog))
      (add-to-menu effects-menu "Low-pass filter" (lambda () (post-low-pass-dialog))))

    (add-to-menu effects-menu low-pass-label cp-low-pass))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Low-pass filter (~1,2F)"  low-pass-freq)))
                             (change-menu-label effects-menu low-pass-label new-label)
                             (set! low-pass-label new-label)))
                         effects-list))



;;; -------- Comb filter chord
;;;

(define comb-chord-scaler 0.95)
(define comb-chord-size 60)
(define comb-chord-amp 0.3)
(define comb-chord-interval-one 0.75)
(define comb-chord-interval-two 1.20)
(define comb-chord-label "Comb filter chord")
(define comb-chord-dialog #f)

(define comb-chord
  (lambda (scaler size amp interval-one interval-two)
    "Comb filter chord: create chords by using filters at harmonically related sizes."
    (let ((c1 (make-comb scaler size))
          (c2 (make-comb scaler (* size interval-one)))
          (c3 (make-comb scaler (* size interval-two))))
      (lambda (x)
        (* amp (+ (comb c1 x) (comb c2 x) (comb c3 x)))))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-comb-chord-dialog)
        (if (not (|Widget? comb-chord-dialog))
            ;; if comb-chord-dialog doesn't exist, create it
            (let ((initial-comb-chord-scaler 0.95)
                  (initial-comb-chord-size 60)
                  (initial-comb-chord-amp 0.3)
                  (initial-comb-chord-interval-one 0.75)
                  (initial-comb-chord-interval-two 1.20)
                  (sliders '()))
              (set! comb-chord-dialog
                    (make-effect-dialog comb-chord-label
                                        (lambda (w context info)
					  (map-chan-with-sync 
					   (lambda ()
					     (comb-chord comb-chord-scaler comb-chord-size comb-chord-amp
							 comb-chord-interval-one comb-chord-interval-two))
					   "comb-chord"))
                                        (lambda (w context info)
                                          (|XtUnmanageChild comb-chord-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Comb filter chord Help"
                                                       "Move the sliders to set the comb-chord parameters."))
                                        (lambda (w c i)
                                          (set! comb-chord-scaler initial-comb-chord-scaler)
                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* comb-chord-scaler 100))))
                                          (set! comb-chord-size initial-comb-chord-size)
                                          (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* comb-chord-size 100))))
                                          (set! comb-chord-amp initial-comb-chord-amp)
                                          (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* comb-chord-amp 100))))
                                          (set! comb-chord-interval-one initial-comb-chord-interval-one)
                                          (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* comb-chord-interval-one 100))))
                                          (set! comb-chord-interval-two initial-comb-chord-interval-two)
                                          (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* comb-chord-interval-two 100)))))))
              (set! sliders
                    (add-sliders comb-chord-dialog
                                 (list (list "comb-chord scaler" 0.0 initial-comb-chord-scaler 1.0
                                             (lambda (w context info)
                                               (set! comb-chord-scaler (/ (|value info) 100.0)))
                                             100)
                                       (list "comb-chord size" 0.0 initial-comb-chord-size 100.0
                                             (lambda (w context info)
                                               (set! comb-chord-size (/ (|value info) 100.0)))
                                             100)
                                       (list "comb-chord amp" 0.0 initial-comb-chord-amp 1.0
                                             (lambda (w context info)
                                               (set! comb-chord-amp (/ (|value info) 100.0)))
                                             100)
                                       (list "comb-chord interval 1" 0.0 initial-comb-chord-interval-one 2.0
                                             (lambda (w context info)
                                               (set! comb-chord-interval-one (/ (|value info) 100.0)))
                                             100)
                                       (list "comb-chord interval 2" 0.0 initial-comb-chord-interval-two 2.0
                                             (lambda (w context info)
                                               (set! comb-chord-interval-two (/ (|value info) 100.0)))
                                             100))))))
        (activate-dialog comb-chord-dialog))

      (add-to-menu effects-menu "Comb filter chord" (lambda () (post-comb-chord-dialog))))

    (add-to-menu effects-menu comb-chord-label 
		 (lambda ()
		   (map-chan-with-sync 
		    (lambda ()
		      (comb-chord comb-chord-scaler comb-chord-size comb-chord-amp
				  comb-chord-interval-one comb-chord-interval-two))
		    "comb-chord"))))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Comb filter chord (~1,2F ~1,2F ~1,2F ~1,2F ~1,2F)" 
						comb-chord-scaler comb-chord-size
 						comb-chord-amp comb-chord-interval-one comb-chord-interval-two)))
                             (change-menu-label effects-menu comb-chord-label new-label)
                             (set! comb-chord-label new-label)))
                         effects-list))

#!
;;; -------- comb filter

(define comb-scaler 0.1)
(define comb-size 50)
(define comb-label "Comb filter")
(define comb-dialog #f)

(define comb-filter
  (lambda (scaler size)
    (let ((delay-line (make-vector size 0.0))
          (delay-loc 0))
      (lambda (x)
        (let ((result (vector-ref delay-line delay-loc)))
          (vector-set! delay-line delay-loc (+ x (* scaler result)))
          (set! delay-loc (1+ delay-loc))
          (if (= delay-loc size) (set! delay-loc 0))
          result)))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-comb-dialog)
        (if (not (|Widget? comb-dialog))
            ;; if comb-dialog doesn't exist, create it
            (let ((initial-comb-scaler 0.1)
                  (initial-comb-size 50)
                  (sliders '()))
              (set! comb-dialog 
		    (make-effect-dialog comb-label
					(lambda (w context info) (map-chan (comb-filter comb-scaler comb-size)))
					(lambda (w context info) (|XtUnmanageChild comb-dialog))
					(lambda (w context info)
					  (help-dialog "Comb filter Help"
						       "move the sliders to change the comb scaler and size"))
					(lambda (w c i)
					  (set! comb-scaler initial-comb-scaler)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* comb-scaler 100))))
					  (set! comb-size initial-comb-size)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact comb-size))))))
	      (set! sliders
		    (add-sliders comb-dialog
				 (list (list "comb scaler" 0.0 initial-comb-scaler 1.0
					     (lambda (w context info)
					       (set! comb-scaler (/ (|value info) 100.0)))
					     100)
				       (list "comb size" 0 initial-comb-size 100
					     (lambda (w context info)
					       (set! comb-size (|value info)))
					     1))))))
	(activate-dialog comb-dialog))
      (add-to-menu effects-menu comb-label (lambda () (post-comb-dialog))))

    (add-to-menu effects-menu comb-label 
		 (lambda () 
		   (map-chan-with-sync 
		    (lambda () 
		      (comb-filter comb-scaler comb-size))
		    "comb"))))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Comb filter (~1,2F ~1D)" comb-scaler comb-size)))
                             (change-menu-label effects-menu comb-label new-label)
                             (set! comb-label new-label)))
                         effects-list))
!#



;;; -------- compand
(define vct (lambda args (list->vct args)))

(define (compand)
  "compand distorts a sound"
  (let* ((tbl (vct -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250
                   0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000)))
    ;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")
    (lambda (inval)
      (let ((index (+ 8.0 (* 8.0 inval))))
        (array-interp tbl index 17)))))

;;; -------- remove DC (from Perry Cook's physical modeling toolkit)

(define (block-dc)
  (let ((lastx 0.0)
        (lasty 0.0))
    (lambda (inval)
      (set! lasty (+ inval (- (* 0.999 lasty) lastx)))
      (set! lastx inval)
      lasty)))





(define (down-oct)
  "(down-oct) tries to move a sound down an octave"
  (let* ((len (frames))
	 (pow2 (ceiling (/ (log len) (log 2))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl1 (samples->vct 0 fftlen))
	 (im1 (make-vct fftlen)))
    (fft rl1 im1 1)
    (vct-scale! rl1 fftscale)
    (vct-scale! im1 fftscale)
    (let ((rl2 (make-vct (* 2 fftlen)))
	  (im2 (make-vct (* 2 fftlen))))
      (do ((i 0 (+ i 1))
	   (k (/ fftlen 2) (+ k 1))
	   (j (+ fftlen (/ fftlen 2)) (+ j 1)))
	  ((= i (/ fftlen 2)))
	(vct-set! rl2 i (vct-ref rl1 i))
	(vct-set! rl2 j (vct-ref rl1 k))
	(vct-set! im2 i (vct-ref im1 i))
	(vct-set! im2 j (vct-ref im1 k)))
      (fft rl2 im2 -1)
      (vct->samples 0 (* 2 fftlen) rl2))))

;;; -------- remove-clicks 

(define (find-click loc)
  (let ((reader (make-sample-reader loc))
	(samp0 0.0)
	(samp1 0.0)
	(samp2 0.0)
	(samps (make-vct 10))
	(samps-ctr 0)
	(diff 1.0)
	(len (frames)))
    (call-with-current-continuation
     (lambda (return)
       (do ((ctr loc (1+ ctr)))
	   ((or (c-g?) (= ctr len)) #f)
	 (set! samp0 samp1)
	 (set! samp1 samp2)
	 (set! samp2 (next-sample reader))
	 (vct-set! samps samps-ctr samp0)
	 (if (< samps-ctr 9)
	     (set! samps-ctr (+ samps-ctr 1))
	     (set! samps-ctr 0))
	 (let ((local-max (max .1 (vct-peak samps))))
	   (if (and (> (abs (- samp0 samp1)) local-max)
		    (> (abs (- samp1 samp2)) local-max)
		    (< (abs (- samp0 samp2)) (/ local-max 2)))
	       (return (1- ctr)))))))))

(define (remove-clicks)
  ;; this is very conservative -- the click detection limits above could be set much tighter in many cases
  (define (remove-click loc)
    (let ((click (find-click loc)))
      (if (and click (not (c-g?)))
	  (begin
	    (smooth-sound (- click 2) 4)
	    (remove-click (+ click 2))))))
  (remove-click 0))


;;; -------- spike
;;;
;;; makes sound more spikey -- sometimes a nice effect

(define (spike)
  (map-chan (let ((x1 0.0) 
		  (x2 0.0) 
		  (amp (maxamp))) ; keep resultant peak at maxamp
	      (lambda (x0) 
		(let ((res (* (/ x0 (* amp amp)) 
			      (abs x2) 
			      (abs x1)))) 
		  (set! x2 x1) 
		  (set! x1 x0) 
		  res)))))

;;; the more successive samples we include in the product, the more we
;;;   limit the output to pulses placed at (just after) wave peaks

#!
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

(add-to-menu effects-menu 
	     "Mark play"
	     (let ((old-cfp #f)
		   (old-cursor #f)
		   (stopping #f))
	       (lambda () 
		 (if stopping
		     (let ((snd (selected-sound)))
		       (set! (cursor-follows-play snd) old-cfp)
		       (set! (cursor snd (selected-channel snd)) old-cursor)
		       (set! stopping #f)
		       (change-menu-label effects-menu "Stop" "Mark Play")
		       (reset-hook! stop-playing-channel-hook)
		       (stop-playing (selected-sound)))
		   (let ((snd (selected-sound)))
		     (set! old-cfp (cursor-follows-play snd))
		     (set! old-cursor (cursor snd (selected-channel snd)))
		     (set! stopping #t)
		     (change-menu-label effects-menu "Mark play" "Stop")
		     (let ((args (find-plausible-marks)))
		       (if args
			   (begin
			     (set! (cursor-follows-play snd) #t)
			     (set! (cursor snd (selected-channel snd)) (car args))
			     (apply play args)
			     (add-hook! stop-playing-channel-hook 
					(lambda (snd chn)
					  (apply play args)))))))))))
				  
!#

(add-to-menu effects-menu #f #f)
(add-to-menu effects-menu "Octave-down" down-oct)
(add-to-menu effects-menu "Remove clicks" remove-clicks)
(add-to-menu effects-menu "Remove DC" (lambda () (map-chan-with-sync (lambda () (block-dc)) "block-dc")))
(add-to-menu effects-menu "Spiker" spike)
(add-to-menu effects-menu "Compand" (lambda () (map-chan-with-sync (lambda () (compand)) "compand")))
(add-to-menu effects-menu "Invert" (lambda () (scale-by -1)))
(add-to-menu effects-menu "Reverse" (lambda () (reverse-sound)))



#!
;;; -------- Cross synthesis
;;;

(define cross-synth-sound 1)
(define cross-synth-amp .5)
(define cross-synth-fft-size 128)
(define cross-synth-radius 6.0)
(define cross-synth-label "Cross synthesis")
(define cross-synth-dialog #f)
(define cross-synth-default-fft-widget #f)

(define use-combo-box-for-fft-size #t) ; radio-buttons or combo-box choice


(define (cp-cross-synth)
  (map-chan (cross-synthesis cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-cross-synth-dialog)
        (if (not (|Widget? cross-synth-dialog))
            ;; if cross-synth-dialog doesn't exist, create it
            (let ((initial-cross-synth-sound 1)
                  (initial-cross-synth-amp .5)
                  (initial-cross-synth-fft-size 128)
                  (initial-cross-synth-radius 6.0)
                  (sliders '()))
              (set! cross-synth-dialog
                    (make-effect-dialog cross-synth-label
                                        (lambda (w context info) (cp-cross-synth))
                                        (lambda (w context info) (|XtUnmanageChild cross-synth-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Cross synthesis Help"
    					               "Move the sliders to set the number of the soundfile to be cross-synthesized, \
the synthesis amplitude, the FFT size, and the radius value. The FFT size must be a power of 2."))
                                        (lambda (w c i)
                                          (set! cross-synth-sound initial-cross-synth-sound)
                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* cross-synth-sound 1))))
                                          (set! cross-synth-amp initial-cross-synth-amp)
                                          (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* cross-synth-amp 100))))
                                          (set! cross-synth-fft-size initial-cross-synth-fft-size)
					  (if use-combo-box-for-fft-size
					      (|XtSetValues cross-synth-default-fft-widget (list |XmNselectedPosition 1))
					      (|XmToggleButtonSetState cross-synth-default-fft-widget #t #t))
                                          (set! cross-synth-radius initial-cross-synth-radius)
                                          (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* cross-synth-radius 100)))))))
              (set! sliders
                    (add-sliders cross-synth-dialog
                                 (list (list "input sound" 0 initial-cross-synth-sound 20
                                             (lambda (w context info)
                                               (set! cross-synth-sound (/ (|value info) 1)))
                                             1)
                                       (list "amplitude" 0.0 initial-cross-synth-amp 1.0
                                             (lambda (w context info)
                                               (set! cross-synth-amp (/ (|value info) 100)))
                                             100)
				       (list "radius" 0.0 initial-cross-synth-radius 360.0
                                             (lambda (w context info)
                                               (set! cross-synth-radius (/ (|value info) 100)))
                                             100))))

	      ;; now add either a radio-button box or a combo-box for the fft size
	      ;;   need to use XtParent here since "mainform" isn't returned by add-sliders

	      (if use-combo-box-for-fft-size
		  ;; this block creates a "combo box" to handle the fft size
		  (let* ((s1 (|XmStringCreateLocalized "FFT size"))
			 (frame (|XtCreateManagedWidget "frame" |xmFrameWidgetClass (|XtParent (car sliders))
				   (list ;|XmNborderWidth 1
					 |XmNshadowType |XmSHADOW_ETCHED_IN
					 |XmNpositionIndex 2)))
			 (frm (|XtCreateManagedWidget "frm" |xmFormWidgetClass frame
				(list |XmNleftAttachment      |XmATTACH_FORM
				      |XmNrightAttachment     |XmATTACH_FORM
				      |XmNtopAttachment       |XmATTACH_FORM
				      |XmNbottomAttachment    |XmATTACH_FORM
				      |XmNbackground          (|Pixel (snd-pixel (basic-color))))))
			 (lab (|XtCreateManagedWidget "FFT size" |xmLabelWidgetClass frm
				   (list |XmNleftAttachment      |XmATTACH_FORM
					 |XmNrightAttachment     |XmATTACH_NONE
					 |XmNtopAttachment       |XmATTACH_FORM
					 |XmNbottomAttachment    |XmATTACH_FORM
					 |XmNlabelString         s1
					 |XmNbackground          (|Pixel (snd-pixel (basic-color))))))
			 (fft-labels (map (lambda (n) (|XmStringCreateLocalized n)) (list "64" "128" "256" "512" "1024" "4096")))
			 (combo (|XtCreateManagedWidget "fftsize" |xmComboBoxWidgetClass frm
				   (list |XmNleftAttachment      |XmATTACH_WIDGET
					 |XmNleftWidget          lab
					 |XmNrightAttachment     |XmATTACH_FORM
					 |XmNtopAttachment       |XmATTACH_FORM
					 |XmNbottomAttachment    |XmATTACH_FORM
					 |XmNitems               fft-labels
					 |XmNitemCount           (length fft-labels)
					 |XmNcomboBoxType        |XmDROP_DOWN_COMBO_BOX
					 |XmNbackground          (|Pixel (snd-pixel (basic-color)))))))
		    (set! cross-synth-default-fft-widget combo)
		    (for-each (lambda (n) (|XmStringFree n)) fft-labels)
		    (|XmStringFree s1)
		    (|XtSetValues combo (list |XmNselectedPosition 1))
		    (|XtAddCallback combo |XmNselectionCallback
		       (lambda (w c i)
			 (let* ((selected (|item_or_text i))
				(size-as-string (cadr (|XmStringGetLtoR selected |XmFONTLIST_DEFAULT_TAG))))
			   (set! cross-synth-fft-size (string->number size-as-string))))))

		  ;; this block creates a "radio button box"
		  (let* ((s1 (|XmStringCreateLocalized "FFT size"))
			 (frame (|XtCreateManagedWidget "frame" |xmFrameWidgetClass (|XtParent (car sliders))
				   (list ;|XmNborderWidth 1
					 |XmNshadowType |XmSHADOW_ETCHED_IN
					 |XmNpositionIndex 2)))
			 (frm (|XtCreateManagedWidget "frm" |xmFormWidgetClass frame
				(list |XmNleftAttachment      |XmATTACH_FORM
				      |XmNrightAttachment     |XmATTACH_FORM
				      |XmNtopAttachment       |XmATTACH_FORM
				      |XmNbottomAttachment    |XmATTACH_FORM
				      |XmNbackground          (|Pixel (snd-pixel (basic-color))))))
			 (rc (|XtCreateManagedWidget "rc" |xmRowColumnWidgetClass frm
				   (list |XmNorientation |XmHORIZONTAL
					 |XmNradioBehavior #t
					 |XmNradioAlwaysOne #t
					 |XmNentryClass |xmToggleButtonWidgetClass
					 |XmNisHomogeneous #t
					 |XmNleftAttachment      |XmATTACH_FORM
					 |XmNrightAttachment     |XmATTACH_FORM
					 |XmNtopAttachment       |XmATTACH_FORM
					 |XmNbottomAttachment    |XmATTACH_NONE
					 |XmNbackground          (|Pixel (snd-pixel (basic-color))))))
			 (lab (|XtCreateManagedWidget "FFT size" |xmLabelWidgetClass frm
				   (list |XmNleftAttachment      |XmATTACH_FORM
					 |XmNrightAttachment     |XmATTACH_FORM
					 |XmNtopAttachment       |XmATTACH_WIDGET
					 |XmNtopWidget           rc
					 |XmNbottomAttachment    |XmATTACH_FORM
					 |XmNlabelString         s1
					 |XmNalignment           |XmALIGNMENT_BEGINNING
					 |XmNbackground          (|Pixel (snd-pixel (basic-color)))))))
		    (for-each 
		     (lambda (size)
		       (let ((button (|XtCreateManagedWidget (format #f "~D" size) |xmToggleButtonWidgetClass rc
				        (list |XmNbackground           (|Pixel (snd-pixel (basic-color)))
					      |XmNvalueChangedCallback (list (lambda (w c i) (if (|set i) (set! cross-synth-fft-size c))) size)
					      |XmNset                  (= size cross-synth-fft-size)))))
			 (if (= size cross-synth-fft-size)
			     (set! cross-synth-default-fft-widget button))))
		     (list 64 128 256 512 1024 4096))
		    (|XmStringFree s1)))))
        (activate-dialog cross-synth-dialog))

      (add-to-menu effects-menu "Cross synthesis" (lambda () (post-cross-synth-dialog))))

    (add-to-menu effects-menu cross-synth-label cp-cross-synth))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Cross synthesis (~1,2D ~1,2F ~1,2D ~1,2F)"
                                                cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius)))
                             (change-menu-label effects-menu cross-synth-label new-label)
                             (set! cross-synth-label new-label)))
                         effects-list))

!#
