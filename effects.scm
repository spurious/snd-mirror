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

(define (plausible-mark-samples)
  ;; find two marks in the current channel (in or nearest to current window)
  (let* ((snd (selected-sound))
	 (chn (selected-channel))
	 (ms (sort (map mark-sample (marks snd chn)) <)))
    (if (< (length ms) 2)
	(throw 'no-such-mark (list "mark-related action requires two marks"))
	(if (= (length ms) 2)
	    ms
	    (let* ((lw (left-sample snd chn))
		   (rw (right-sample snd chn))
		   (cw (cursor snd chn))
		   (favor (if (and (>= cw lw)
				   (<= cw rw))
			      cw
			      (* .5 (+ lw rw)))))
	      ;; favor is the point we center the search on
	      (define (centered-points points)
		(if (= (length points) 2)
		    points
		    (let ((p1 (car points))
			  (p2 (cadr points))
			  (p3 (caddr points)))
		      (if (< (abs (- p1 favor)) (abs (- p3 favor)))
			  (list p1 p2)
			  (centered-points (cdr points))))))
	      (centered-points ms))))))

(define (map-chan-over-target func target decay)
  (let* ((ms (and (eq? target 'marks)
		 (plausible-mark-samples)))
	 (beg (if (eq? target 'sound)
		  0
		  (if (eq? target 'selection)
		      (selection-position)
		      (car ms))))
	 (dur (if (eq? target 'sound)
		  (1- (frames))
		  (if (eq? target 'selection)
		      (+ (selection-position) (selection-length))
		      (cadr ms))))
	 (overlap (if decay
		      (inexact->exact (* (srate) decay))
		      0)))
    (map-chan (func dur)
	      beg 
	      (+ dur overlap))))

(define map-chan-over-target-with-sync
  (lambda (func target origin decay)
    (let* ((snc (sync))
	   (ms (and (eq? target 'marks)
		    (plausible-mark-samples)))
	   (beg (if (eq? target 'sound)
		    0
		    (if (eq? target 'selection)
			(selection-position)
			(car ms))))
	   (overlap (if decay
			(inexact->exact (* (srate) decay))
			0)))
      (apply for-each
	     (lambda (snd chn)
	       (let ((dur (if (eq? target 'sound)
			      (1- (frames snd chn))
			      (if (eq? target 'selection)
				  (+ (selection-position) (selection-length))
				  (cadr ms)))))
		 (if (= (sync snd) snc)
		     (map-chan (func dur) 
			       beg 
			       (+ dur overlap)
			       origin snd chn))))
	     (if (> snc 0) 
		 (all-chans) 
		 (list (list (selected-sound)) 
		       (list (selected-channel))))))))

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

(define (add-target mainform target-callback truncate-callback)
  ;; add a set of 3 radio buttons at the bottom of the main section for choice between sound, selection, between-marks
  ;;   target-callback should take one arg, a symbol: 'sound, 'selection, 'marks, and apply the effect accordingly (upon "DoIt")
  ;;   truncate-callback (if any) takes one arg: boolean representing toggle state (#t = on)
  (let* ((sep (|XtCreateManagedWidget "sep" |xmSeparatorWidgetClass mainform
		(list |XmNorientation      |XmHORIZONTAL
		      |XmNseparatorType    |XmSHADOW_ETCHED_OUT
		      |XmNbackground       (|Pixel (snd-pixel (basic-color))))))
	 (rc (|XtCreateManagedWidget "rc"  |xmRowColumnWidgetClass mainform
                (list |XmNorientation      |XmHORIZONTAL
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
		      |XmNindicatorType    |XmONE_OF_MANY_ROUND
		      |XmNarmCallback      (list (lambda (w c i) (target-callback type)) #f))))
     (list "entire sound" "selection" "between marks")
     (list 'sound 'selection 'marks)
     (list #t #f #f))
    (if truncate-callback
	(let* ((trsep (|XtCreateManagedWidget "trsep" |xmSeparatorWidgetClass mainform
		(list |XmNorientation      |XmHORIZONTAL)))
	       (trbutton (|XtCreateManagedWidget "truncate at end" |xmToggleButtonWidgetClass mainform
                (list |XmNbackground       (|Pixel (snd-pixel (basic-color)))
		      |XmNset              #t
		      |XmNselectColor      (yellow-pixel)))))
	  (|XtAddCallback trbutton |XmNvalueChangedCallback (lambda (w c i) (truncate-callback (|set i)))) ))))


(define (activate-dialog dialog)
  (if (not (|XtIsManaged dialog))
      (|XtManageChild dialog)
      (raise-dialog dialog)))


;;; -------- insert silence (at cursor, silence-amount in secs)
(define silence-amount 1.0)
(define silence-label "Add silence")
(define silence-dialog #f)

(define (cp-silence)
  "Add silence adds the requested seconds of silence at the cursor"
  (insert-silence (cursor)
		  (inexact->exact (* (srate) silence-amount))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-silence-dialog)
        (if (not (|Widget? silence-dialog))
            ;; if silence-dialog doesn't exist, create it
	    (let ((initial-silence-amount 1.0)
		  (sliders '()))
	      (set! silence-dialog 
		    (make-effect-dialog silence-label
					(lambda (w context info) (cp-silence))
					(lambda (w context info) (|XtUnmanageChild silence-dialog))
					(lambda (w context info)
					  (help-dialog "Add silence Help"
						       "Move the slider to change the number of seconds of silence added at the cursor."))
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
  "Gain scales amplitude by gain amount."
  (if (eq? gain-target 'sound)
      (scale-by gain-amount)
      (if (eq? gain-target 'selection)
	  (if (selection?)
	      (scale-selection-by gain-amount)
	      (snd-print "no selection"))
	  (let ((pts (plausible-mark-samples)))
	    (if pts
		(scale-sound-by gain-amount (car pts) (- (cadr pts) (car pts))))))))

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
						       "Move the slider to change the gain scaling amount."))
					(lambda (w c i)
					  (set! gain-amount initial-gain-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gain-amount 100)))))))
	      (set! sliders 
		    (add-sliders gain-dialog
				 (list (list "gain" 0.0 initial-gain-amount 5.0 
					     (lambda (w context info)
					       (set! gain-amount (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! gain-target target)) #f)))
        (activate-dialog gain-dialog))

      (add-to-menu effects-menu "Gain" (lambda () (post-gain-dialog))))

    (add-to-menu effects-menu gain-label cp-gain))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Gain (~1,2F)"  gain-amount)))
                             (change-menu-label effects-menu gain-label new-label)
                             (set! gain-label new-label)))
                         effects-list))



;;; -------- Normalize
;;;

(define normalize-amount 1.0)
(define normalize-label "Normalize")
(define normalize-dialog #f)
(define normalize-target 'sound)

(define (cp-normalize)
  (if (eq? normalize-target 'sound)
      (scale-to normalize-amount)
      (if (eq? normalize-target 'selection)
	  (if (selection?)
	      (scale-selection-to normalize-amount)
	      (snd-print "no selection"))
	  (let ((pts (plausible-mark-samples)))
	    (if pts
		(scale-sound-to normalize-amount (car pts) (- (cadr pts) (car pts))))))))
	  
(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-normalize-dialog)
        (if (not (|Widget? normalize-dialog))
            ;; if normalize-dialog doesn't exist, create it
            (let ((initial-normalize-amount 1.0)
                  (sliders '()))
              (set! normalize-dialog 
		    (make-effect-dialog normalize-label
					(lambda (w context info) (cp-normalize))
					(lambda (w context info) (|XtUnmanageChild normalize-dialog))
					(lambda (w context info)
					  (help-dialog "Normalize Help"
						       "Normalize scales amplitude to the normalize amount.\n\ Move the slider to change the scaling amount."))
					(lambda (w c i)
					  (set! normalize-amount initial-normalize-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* normalize-amount 100)))))))
	      (set! sliders
		    (add-sliders normalize-dialog
				 (list (list "normalize" 0.0 initial-normalize-amount 1.0 
					     (lambda (w context info)
					       (set! normalize-amount (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! normalize-target target)) #f)))

	(activate-dialog normalize-dialog))

      (add-to-menu effects-menu "Normalize" (lambda () (post-normalize-dialog))))

    (add-to-menu effects-menu normalize-label cp-normalize))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Normalize (~1,2F)"  normalize-amount)))
                             (change-menu-label effects-menu normalize-label new-label)
                             (set! normalize-label new-label)))
                         effects-list))


;;; -------- Gate (gate set by gate-amount)
;;;

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
						       "Move the slider to change the gate intensity. Higher values gate more of the sound."))
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


;;; -------- Adaptive saturation
(define adsat-size 4.0)
(define adsat-label "Adaptive saturation")
(define adsat-dialog #f)
(define adsat-target 'sound)

(define (cp-adsat)
  "adsat does weird stuff by adsat size"
  (map-chan-over-target 
   (lambda (ignored)
     (let ((mn 0.0)
	   (mx 0.0)
	   (n 0)
	   (vals (make-vct adsat-size)))
       (lambda (val)
	 (if (= n adsat-size)
	     (begin
	       (do ((i 0 (1+ i)))
		   ((= i adsat-size))
		 (if (>= (vct-ref vals i) 0.0)
		     (vct-set! vals i mx)
		     (vct-set! vals i mn)))
	       (set! n 0)
	       (set! mx 0.0)
	       (set! mn 0.0)
	       vals)
	     (begin
	       (vct-set! vals n val)
	       (if (> val mx) (set! mx val))
	       (if (< val mn) (set! mn val))
	       (set! n (1+ n))
	       #f)))))
   adsat-target #f))



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
						       "Move the slider to change the adsat scaling factor."))
					(lambda (w c i)
					  (set! adsat-size initial-adsat-size)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* adsat-size 100)))))))
	      (set! sliders
		    (add-sliders adsat-dialog
				 (list (list "adaptive saturation" 0.0 initial-adsat-size 10.0 
					     (lambda (w context info)
					       (set! adsat-size (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! adsat-target target)) #f)))

	(activate-dialog adsat-dialog))

      (add-to-menu effects-menu "Adaptive saturation" (lambda () (post-adsat-dialog))))

    (add-to-menu effects-menu adsat-label cp-adsat))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Adaptive saturation (~1,2F)"  adsat-size)))
                             (change-menu-label effects-menu adsat-label new-label)
                             (set! adsat-label new-label)))
                         effects-list))



;;; -------- Amplitude modulation
(define am-amount 100.0)
(define am-label "Amplitude modulation")
(define am-dialog #f)
(define am-target 'sound)

(if (not (defined? 'am))
    (define am 
      (lambda (freq) 
	(let ((os (make-oscil freq))) 
	  (lambda (inval) 
	    (amplitude-modulate 1.0 inval (oscil os)))))))

(define (cp-am)
  "amplitude modulation"
  (map-chan-over-target (lambda (ignored) (am am-amount)) am-target #f))

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
						       "Move the slider to change the amplitude modulation amount."))
					(lambda (w c i)
					  (set! am-amount initial-am-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact am-amount))))))
	      (set! sliders
		    (add-sliders am-dialog
				 (list (list "amplitude modulation" 0.0 initial-am-amount 1000.0 
					     (lambda (w context info)
					       (set! am-amount (|value info)))
					     1))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! am-target target)) #f)))

	(activate-dialog am-dialog))

      (add-to-menu effects-menu "Amplitude modulation" (lambda () (post-am-dialog))))

    (add-to-menu effects-menu am-label cp-am))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Amplitude modulation (~1,2F)"  am-amount)))
                             (change-menu-label effects-menu am-label new-label)
                             (set! am-label new-label)))
                         effects-list))

;;; -------- Ring modulation
;;;

(define ring-mod-frequency 100)
(define ring-mod-radians 100)
(define ring-mod-label "Ring modulation")
(define ring-mod-dialog #f)
(define ring-mod-target 'sound)

(if (not (defined? 'ring-mod))
    (define ring-mod
      (lambda (freq gliss-env)
	(let* ((os (make-oscil :frequency freq))
	       (len (frames))
	       (genv (make-env :envelope gliss-env :end len)))
	  (lambda (inval)
	    (* (oscil os (env genv)) inval))))))

(define (cp-ring-mod)
  (map-chan-over-target 
   (lambda (ignored) 
     (ring-mod ring-mod-frequency (list 0 0 1 (hz->radians ring-mod-radians))))
   ring-mod-target #f))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-ring-mod-dialog)
        (if (not (|Widget? ring-mod-dialog))
            ;; if ring-mod-dialog doesn't exist, create it
            (let ((initial-ring-mod-frequency 100)
                  (initial-ring-mod-radians 100)
                  (sliders '()))
              (set! ring-mod-dialog
                    (make-effect-dialog ring-mod-label
                                        (lambda (w context info) (cp-ring-mod))
                                        (lambda (w context info) (|XtUnmanageChild ring-mod-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Ring modulation"
                                                       "Move the sliders to set the modulation frequency and envelope radians."))
                                        (lambda (w c i)
                                          (set! ring-mod-frequency initial-ring-mod-frequency)
                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* ring-mod-frequency 1))))
                                          (set! ring-mod-radians initial-ring-mod-radians)
                                          (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* ring-mod-radians 1)))))))
              (set! sliders
                    (add-sliders ring-mod-dialog
                                 (list (list "modulation frequency" 0 initial-ring-mod-frequency 1000
                                             (lambda (w context info)
                                               (set! ring-mod-frequency (/ (|value info) 1)))
                                             1)
                                       (list "envelope radians" 0 initial-ring-mod-radians 360
                                             (lambda (w context info)
                                               (set! ring-mod-radians (/ (|value info) 1)))
                                             1))))

	      (add-target (|XtParent (car sliders)) (lambda (target) (set! ring-mod-target target)) #f)))

        (activate-dialog ring-mod-dialog))

      (add-to-menu effects-menu "Ring modulation" (lambda () (post-ring-mod-dialog))))
    (add-to-menu effects-menu ring-mod-label cp-ring-mod))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Ring modulation (~1,2D ~1,2D)"  ring-mod-frequency ring-mod-radians)))
                             (change-menu-label effects-menu ring-mod-label new-label)
                             (set! ring-mod-label new-label)))
                         effects-list))


;;; -------- Sample rate conversion (resample)
;;;

(define src-amount 0.0)
(define src-label "Sample rate conversion")
(define src-dialog #f)
(define src-target 'sound)

(define (cp-src)
  (if (eq? src-target 'sound)
      (src-sound src-amount)
      (if (eq? src-target 'selection)
          (if (selection?)
              (src-selection src-amount)
              (snd-print "no selection"))
          (snd-print "can't apply src between marks yet"))))


(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-src-dialog)
        (if (not (|Widget? src-dialog))
            ;; if src-dialog doesn't exist, create it
            (let ((initial-src-amount 0.0)
                  (sliders '()))
              (set! src-dialog
                    (make-effect-dialog src-label
                                        (lambda (w context info) (cp-src))
                                        (lambda (w context info) (|XtUnmanageChild src-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Sample rate conversion"
                                                       "Move the slider to change the sample rate.\n\ Values greater than 1.0 speed up file play,\n\ negative values reverse it."))
                                        (lambda (w c i)
                                          (set! src-amount initial-src-amount)
                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* src-amount 100)))))))
              (set! sliders
                    (add-sliders src-dialog
                                 (list (list "sample rate" -2.0 initial-src-amount 2.0
                                             (lambda (w context info)
                                               (set! src-amount (/ (|value info) 100.0)))
                                             100))))
              (add-target (|XtParent (car sliders)) (lambda (target) (set! src-target target)) #f)))
        (activate-dialog src-dialog))

      (add-to-menu effects-menu "Sample rate conversion" (lambda () (post-src-dialog))))

    (add-to-menu effects-menu src-label cp-src))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Sample rate conversion (~1,2F)"  src-amount)))
                             (change-menu-label effects-menu src-label new-label)
                             (set! src-label new-label)))
                         effects-list))

;;; -------- Time and pitch scaling by granular synthesis and sampling rate conversion
;;;

(define time-scale 1.0)
(define hop-size 0.05)
(define segment-length 0.15)
(define ramp-scale 0.5)
(define pitch-scale 1.0)
(define expsrc-label "Time/Pitch scaling")
(define expsrc-dialog #f)
(define expsrc-target 'sound)

(define (cp-expsrc)
  (save-controls)
  (reset-controls)
  (set! (speed-control) pitch-scale)
  (let ((new-time (* pitch-scale time-scale)))
    (if (not (= new-time 1.0))
        (begin
          (set! (expand-control?) #t)
          (set! (expand-control) new-time)
          (set! (expand-control-hop) hop-size)
          (set! (expand-control-length) segment-length)
          (set! (expand-control-ramp) ramp-scale))))
    (if (eq? expsrc-target 'marks)
	(let ((ms (plausible-mark-samples)))
	  (apply-controls (selected-sound) 0 (car ms) (1+ (- (cadr ms) (car ms)))))
	(apply-controls (selected-sound) (if (eq? contrast-target 'sound) 0 2)))
  (restore-controls))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-expsrc-dialog)
        (if (not (|Widget? expsrc-dialog))
            ;; if expsrc-dialog doesn't exist, create it
            (let ((initial-time-scale 1.0)
                  (initial-hop-size 0.05)
                  (initial-segment-length 0.15)
                  (initial-ramp-scale 0.5)
                  (initial-pitch-scale 1.0)
                  (sliders '()))
              (set! expsrc-dialog 
		    (make-effect-dialog expsrc-label
					(lambda (w context info) (cp-expsrc))
					(lambda (w context info) (|XtUnmanageChild expsrc-dialog))
					(lambda (w context info)
					  (help-dialog "Time/Pitch scaling Help"
						       "Move the sliders to change the time/pitch scaling parameters."))
					(lambda (w c i)
					  (set! time-scale initial-time-scale)
					  (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* time-scale 100))))
 					  (set! hop-size initial-hop-size)
                                          (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* hop-size 100))))
                                  	  (set! segment-length initial-segment-length)
                                  	  (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* segment-length 100))))
                                  	  (set! ramp-scale initial-ramp-scale)
                                  	  (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* ramp-scale 100))))
					  (set! pitch-scale initial-pitch-scale)
					  (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* pitch-scale 100)))))))
	      (set! sliders
		    (add-sliders expsrc-dialog
				 (list (list "time scale" 0.0 initial-time-scale 5.0
					     (lambda (w context info)
					       (set! time-scale (/ (|value info) 100.0)))
					     100)
 				       (list "hop size" 0.0 initial-hop-size 1.0
                                             (lambda (w context info)
                                               (set! hop-size (/ (|value info) 100.0)))
                                             100)
                                       (list "segment length" 0.0 initial-segment-length 0.5
                                             (lambda (w context info)
                                               (set! segment-length (/ (|value info) 100.0)))
                                             100)
				       (list "ramp scale" 0.0 initial-ramp-scale 0.5
                                             (lambda (w context info)
                                               (set! ramp-scale (/ (|value info) 100.0)))
                                             1000)
				       (list "pitch scale" 0.0 initial-pitch-scale 5.0
					     (lambda (w context info)
					       (set! pitch-scale (/ (|value info) 100.0)))
					     100))))
              (add-target (|XtParent (car sliders)) (lambda (target) (set! expsrc-target target)) #f)))

	(activate-dialog expsrc-dialog))

      (add-to-menu effects-menu "Time/Pitch scaling" (lambda () (post-expsrc-dialog))))

    (add-to-menu effects-menu expsrc-label cp-expsrc))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Time/Pitch scaling (~1,2F ~1,2F)" time-scale pitch-scale)))
                             (change-menu-label effects-menu expsrc-label new-label)
                             (set! expsrc-label new-label)))
                         effects-list))


;;; -------- Reverb from Michael McNabb's Nrev 
;;; -------- very nice reverb actually
;;;
;;; (truncate)

(define reverb-amount 0.1)
(define reverb-filter 0.5)
(define reverb-feedback 1.09)
(define reverb-label "McNabb reverb")
(define reverb-dialog #f)
(define reverb-target 'sound)

;;; add reverb-control-decay (with ramp?) and reverb-truncate

(define (cp-reverb)
  (save-controls)
  (reset-controls)
  (set! (reverb-control?) #t)
  (set! (reverb-control-scale) reverb-amount)
  (set! (reverb-control-lowpass) reverb-filter)
  (set! (reverb-control-feedback) reverb-feedback)
  (if (eq? reverb-target 'marks)
      (let ((ms (plausible-mark-samples)))
	(apply-controls (selected-sound) 0 (car ms) (1+ (- (cadr ms) (car ms)))))
      (apply-controls (selected-sound) (if (eq? reverb-target 'sound) 0 2)))
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
					  (help-dialog "McNabb reverb Help"
						       "Reverberator from Michael McNabb. \
Adds reverberation scaled by reverb amount, lowpass filtering, and feedback. Move the sliders to change the reverb parameters."))
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
					     100))))
              (add-target (|XtParent (car sliders)) (lambda (target) (set! reverb-target target)) #f)))

	(activate-dialog reverb-dialog))

      (add-to-menu effects-menu "McNabb reverb" (lambda () (post-reverb-dialog))))

    (add-to-menu effects-menu reverb-label cp-reverb))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "McNabb reverb (~1,2F ~1,2F ~1,2F)" reverb-amount reverb-filter reverb-feedback)))
                             (change-menu-label effects-menu reverb-label new-label)
                             (set! reverb-label new-label)))
                         effects-list))


;;; -------- Chowning reverb
;;;

(define jc-reverb-decay 2.0)
(define jc-reverb-volume 0.1)
(define jc-reverb-label "Chowning reverb")
(define jc-reverb-dialog #f)
(define jc-reverb-target 'sound)
(define jc-reverb-truncate #t)

(define jc-reverb-1 ; changed from examp.scm for target/truncate (and omits low-pass and amp-env)
  (lambda (input-samps)
    (let* ((allpass1 (make-all-pass -0.700 0.700 1051))
	   (allpass2 (make-all-pass -0.700 0.700  337))
	   (allpass3 (make-all-pass -0.700 0.700  113))
	   (comb1 (make-comb 0.742 4799))
	   (comb2 (make-comb 0.733 4999))
	   (comb3 (make-comb 0.715 5399))
	   (comb4 (make-comb 0.697 5801))
	   (outdel1 (make-delay (round (* .013 (srate)))))
	   (comb-sum 0.0)
	   (comb-sum-1 0.0)
	   (comb-sum-2 0.0)
	   (delA 0.0)
	   (delB 0.0)
	   (samp 0))
      (lambda (inval)
	(let ((allpass-sum (all-pass allpass3 
				     (all-pass allpass2 
					       (all-pass allpass1 
							 (if (< samp input-samps) inval 0.0))))))
	  (set! samp (1+ samp))
	  (set! comb-sum-2 comb-sum-1)
	  (set! comb-sum-1 comb-sum)
	  (set! comb-sum 
		(+ (comb comb1 allpass-sum)
		   (comb comb2 allpass-sum)
		   (comb comb3 allpass-sum)
		   (comb comb4 allpass-sum)))
	  (+ inval
	     (* jc-reverb-volume (delay outdel1 comb-sum))))))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-jc-reverb-dialog)
        (if (not (|Widget? jc-reverb-dialog))
            ;; if jc-reverb-dialog doesn't exist, create it
            (let ((initial-jc-reverb-decay 2.0)
                  (initial-jc-reverb-volume 0.1)
                  (sliders '()))
              (set! jc-reverb-dialog
                    (make-effect-dialog jc-reverb-label
                                        (lambda (w context info) 
					  (map-chan-over-target jc-reverb-1 jc-reverb-target (and (not jc-reverb-truncate) jc-reverb-decay)))
                                        (lambda (w context info) (|XtUnmanageChild jc-reverb-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Chowning reverb Help"
    					               "Nice reverb from John Chowning. Move the sliders to set the jc-reverb parameters."))
                                        (lambda (w c i)
                                          (set! jc-reverb-decay initial-jc-reverb-decay)
                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* jc-reverb-decay 100))))
                                          (set! jc-reverb-volume initial-jc-reverb-volume)
                                          (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* jc-reverb-volume 100))))
					  )))
              (set! sliders
                    (add-sliders jc-reverb-dialog
                                 (list (list "decay duration" 0.0 initial-jc-reverb-decay 10.0
                                             (lambda (w context info)
                                               (set! jc-reverb-decay (/ (|value info) 100)))
                                             100)
                                       (list "reverb volume" 0.0 initial-jc-reverb-volume 1.0
                                             (lambda (w context info)
                                               (set! jc-reverb-volume (/ (|value info) 100)))
                                             100))))
              (add-target (|XtParent (car sliders)) 
			  (lambda (target) (set! jc-reverb-target target))
			  (lambda (truncate) (set! jc-reverb-truncate truncate)))))
        (activate-dialog jc-reverb-dialog))

      (add-to-menu effects-menu "Chowning reverb" (lambda () (post-jc-reverb-dialog))))

    (add-to-menu effects-menu jc-reverb-label cp-jc-reverb))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Chowning reverb (~1,2F ~1,2F)"
                                                jc-reverb-decay jc-reverb-volume)))
                             (change-menu-label effects-menu jc-reverb-label new-label)
                             (set! jc-reverb-label new-label)))
                         effects-list))


;;; -------- Convolution
;;;
;;; (progress report? truncate?)

(define convolve-sound-one 0)
(define convolve-sound-two 1)
(define convolve-amp 0.01)
(define convolve-label "Convolution")
(define convolve-dialog #f)

(if (not (defined? 'cnvtest))
    (define cnvtest
      ;; returns new max sample
      (lambda (snd0 snd1 amp)
	(let* ((flt-len (frames snd0))
	       (total-len (+ flt-len (frames snd1)))
	       (cnv (make-convolve :filter (samples->vct 0 flt-len snd0)))
	       (sf (make-sample-reader 0 snd1))
	       (out-data (make-vct total-len)))
	  (vct-map! out-data (lambda () (convolve cnv (lambda (dir) (next-sample sf)))))
	  (free-sample-reader sf)
	  (vct-scale! out-data amp)
	  (let ((max-samp (vct-peak out-data)))
	    (vct->samples 0 total-len out-data snd1)
	    (if (> max-samp 1.0) (set! (y-bounds snd1) (list (- max-samp) max-samp)))
	    max-samp)))))

(define (cp-convolve)
  (cnvtest convolve-sound-one convolve-sound-two convolve-amp)) 

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-convolve-dialog)
        (if (not (|Widget? convolve-dialog))
            ;; if convolve-dialog doesn't exist, create it
            (let ((initial-convolve-sound-one 0)
                  (initial-convolve-sound-two 1)
                  (initial-convolve-amp 0.01)
                  (sliders '()))
              (set! convolve-dialog
                    (make-effect-dialog convolve-label
                                        (lambda (w context info) (cp-convolve))
                                        (lambda (w context info) (|XtUnmanageChild convolve-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Convolution Help"
    					               "Very simple convolution. \
Move the sliders to set the numbers of the soundfiles to be convolved and the amount for the amplitude scaler."))
                                        (lambda (w c i)
                                          (set! convolve-sound-one initial-convolve-sound-one)
                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* convolve-sound-one 1))))
                                          (set! convolve-sound-two initial-convolve-sound-two)
                                          (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* convolve-sound-two 1))))
                                          (set! convolve-amp initial-convolve-amp)
                                          (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* convolve-amp 100)))))))
              (set! sliders
                    (add-sliders convolve-dialog
                                 (list (list "impulse response file" 0 initial-convolve-sound-one 24
                                             (lambda (w context info)
                                               (set! convolve-sound-one (/ (|value info) 1)))
                                             1)
                                       (list "sound file" 0 initial-convolve-sound-two 24
                                             (lambda (w context info)
                                               (set! convolve-sound-two (/ (|value info) 1)))
                                             1)
                                       (list "amplitude" 0.0 initial-convolve-amp 0.10
                                             (lambda (w context info)
                                               (set! convolve-amp (/ (|value info) 100.0)))
                                             1000))))))
        (activate-dialog convolve-dialog))

      (add-to-menu effects-menu "Convolution" (lambda () (post-convolve-dialog))))

    (add-to-menu effects-menu convolve-label cp-convolve))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Convolution (~1,2D ~1,2D ~1,2F)"
                                                convolve-sound-one convolve-sound-two convolve-amp)))
                             (change-menu-label effects-menu convolve-label new-label)
                             (set! convolve-label new-label)))
                         effects-list))



;;; -------- Echo (controlled by delay-time and echo-amount)

(define delay-time .5) ; i.e. delay between echoes
(define echo-amount .2)
(define echo-label "Echo")
(define echo-dialog #f)
(define echo-target 'sound)
(define echo-truncate #t)
;;; echo-decay? -- using (* 4 delay-time) currently


(define (cp-echo input-samps)
  "echo adds echos spaced by delay-time seconds and scaled by echo-amount"
  (let ((del (make-delay (round (* delay-time (srate)))))
	(samp 0))
    (lambda (inval)
      (set! samp (1+ samp))
      (+ inval
         (delay del
                (* echo-amount (+ (tap del) (if (<= samp input-samps) inval 0.0))))))))

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
					  (map-chan-over-target-with-sync 
					   (lambda (input-samps) 
					     (cp-echo input-samps))
					   echo-target "echo"
					   (and (not echo-truncate) 
						(* 4 delay-time))))
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
					     100))))
              (add-target (|XtParent (car sliders)) 
			  (lambda (target) (set! echo-target target))
			  (lambda (truncate) (set! echo-truncate truncate)))))

	(activate-dialog echo-dialog))

      (add-to-menu effects-menu "Echo"
                   (lambda ()
                     (post-echo-dialog))))

    (add-to-menu effects-menu echo-label cp-echo))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Echo (~1,2F ~1,2F)" delay-time echo-amount)))
                             (change-menu-label effects-menu echo-label new-label)
                             (set! echo-label new-label)))
                         effects-list))


;;; -------- Filtered echo

(define flecho-scaler 0.5)
(define flecho-delay 0.9)
(define flecho-label "Filtered echo")
(define flecho-dialog #f)
(define flecho-target 'sound)
(define flecho-truncate #t)

(define flecho-1
  (lambda (scaler secs input-samps)
    (let* ((flt (make-fir-filter :order 4 :xcoeffs (list->vct '(.125 .25 .25 .125))))
	   (del (make-delay  (round (* secs (srate)))))
	   (samp 0))
      (lambda (inval)
	(set! samp (1+ samp))
	(+ inval 
	   (delay del 
		  (fir-filter flt (* scaler (+ (tap del) (if (<= samp input-samps) inval 0.0))))))))))

(define (cp-flecho)
 (map-chan-over-target 
  (lambda (input-samps) 
    (flecho-1 flecho-scaler flecho-delay input-samps))
  flecho-target 
  (and (not flecho-truncate) 
       (* 4 flecho-delay))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-flecho-dialog)
        (if (not (|Widget? flecho-dialog))
            ;; if flecho-dialog doesn't exist, create it
            (let ((initial-flecho-scaler 0.5)
                  (initial-flecho-delay 0.9)
                  (sliders '()))
              (set! flecho-dialog 
		    (make-effect-dialog flecho-label
					(lambda (w context info)
					  (cp-flecho))
					(lambda (w context info)
					  (|XtUnmanageChild flecho-dialog))
					(lambda (w context info)
					  (help-dialog "Filtered echo Help"
						       "Move the sliders to set the filter scaler and the delay time in seconds."))
					(lambda (w c i)
					  (set! flecho-scaler initial-flecho-scaler)
					  (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* flecho-scaler 100))))
					  (set! flecho-delay initial-flecho-delay)
					  (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* flecho-delay 100)))))))
	      (set! sliders
		    (add-sliders flecho-dialog
				 (list (list "filter scaler" 0.0 initial-flecho-scaler 1.0
					     (lambda (w context info)
					       (set! flecho-scaler (/ (|value info) 100.0)))
					     100)
				       (list "delay time (secs)" 0.0 initial-flecho-delay 3.0
					     (lambda (w context info)
					       (set! flecho-delay (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) 
			  (lambda (target) (set! flecho-target target))
			  (lambda (truncate) (set! flecho-truncate truncate)))))

	(activate-dialog flecho-dialog))

      (add-to-menu effects-menu "Filtered echo" (lambda () (post-flecho-dialog))))

    (add-to-menu effects-menu flecho-label cp-flecho))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Filtered echo (~1,2F ~1,2F)" flecho-scaler flecho-delay)))
                             (change-menu-label effects-menu flecho-label new-label)
                             (set! flecho-label new-label)))
                         effects-list))


;;; -------- Modulated echo
;;; -------- very slick

(define zecho-scaler 0.5)
(define zecho-delay 0.75)
(define zecho-freq 6)
(define zecho-amp 10.0)
(define zecho-label "Modulated echo")
(define zecho-dialog #f)
(define zecho-target 'sound)
(define zecho-truncate #t)

(define zecho-1
  (lambda (scaler secs frq amp input-samps)
    (let* ((os (make-oscil frq))
	   (len (round (* secs (srate))))
	   (del (make-delay len :max-size (+ len amp 1)))
	   (samp 0))
      (lambda (inval)
	(set! samp (1+ samp))
	(+ inval 
	   (delay del 
		  (* scaler (+ (tap del) (if (<= samp input-samps) inval 0.0)))
		  (* amp (oscil os))))))))

(define (cp-zecho)
 (map-chan-over-target 
  (lambda (input-samps) (zecho-1 zecho-scaler zecho-delay zecho-freq zecho-amp input-samps)) 
  zecho-target
  (and (not zecho-truncate)
       (* 4 zecho-delay))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-zecho-dialog)
        (if (not (|Widget? zecho-dialog))
            ;; if zecho-dialog doesn't exist, create it
            (let ((initial-zecho-scaler 0.5)
                  (initial-zecho-delay 0.75)
                  (initial-zecho-freq 6)
                  (initial-zecho-amp 10.0)
                  (sliders '()))
              (set! zecho-dialog 
		    (make-effect-dialog zecho-label
					(lambda (w context info)
					  (cp-zecho))
					(lambda (w context info)
					  (|XtUnmanageChild zecho-dialog))
					(lambda (w context info)
					  (help-dialog "Modulated echo Help"
						       "Move the sliders to set the echo scaler, \
the delay time in seconds, the modulation frequency, and the echo amplitude."))
					(lambda (w c i)
					  (set! zecho-scaler initial-zecho-scaler)
					  (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* zecho-scaler 100))))
					  (set! zecho-delay initial-zecho-delay)
					  (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* zecho-delay 100))))
					  (set! zecho-freq initial-zecho-freq)
					  (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* zecho-freq 100))))
					  (set! zecho-amp initial-zecho-amp)
					  (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* zecho-amp 100)))))))
	      (set! sliders
		    (add-sliders zecho-dialog
				 (list (list "echo scaler" 0.0 initial-zecho-scaler 1.0
					     (lambda (w context info)
					       (set! zecho-scaler (/ (|value info) 100.0)))
					     100)
				       (list "delay time (secs)" 0.0 initial-zecho-delay 3.0
					     (lambda (w context info)
					       (set! zecho-delay (/ (|value info) 100.0)))
					     100)
				       (list "modulation frequency" 0.0 initial-zecho-freq 100.0
					     (lambda (w context info)
					       (set! zecho-freq (/ (|value info) 100.0)))
					     100)
                                      (list "modulation amplitude" 0.0 initial-zecho-amp 100.0
                                             (lambda (w context info)
                                               (set! zecho-amp (/ (|value info) 100.0)))
                                             100))))
	      (add-target (|XtParent (car sliders)) 
			  (lambda (target) (set! zecho-target target))
			  (lambda (truncate) (set! zecho-truncate truncate)))))
	(activate-dialog zecho-dialog))

      (add-to-menu effects-menu "Modulated echo" (lambda () (post-zecho-dialog))))

    (add-to-menu effects-menu zecho-label cp-zecho))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Modulated echo (~1,2F ~1,2F ~1,2F ~1,2F)" zecho-scaler zecho-delay zecho-freq zecho-amp)))
                             (change-menu-label effects-menu zecho-label new-label)
                             (set! zecho-label new-label)))
                         effects-list))



;;; -------- Flange and phasing
;;;

(define flange-speed 2.0)
(define flange-amount 5.0)
(define flange-time 0.001)
(define flange-label "Flange")
(define flange-dialog #f)
(define flange-target 'sound)

(define (cp-flange) ; increase speed and amount to get phaser
  (let* ((ri (make-rand-interp :frequency flange-speed :amplitude flange-amount))
        (len (round (* flange-time (srate))))
        (del (make-delay len :max-size (+ len flange-amount 1))))
    (lambda (inval)
      (* .75 (+ inval 
              (delay del 
                     inval
                     (rand-interp ri)))))))

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
					  (map-chan-over-target-with-sync (lambda (ignored) (cp-flange)) flange-target "flange" #f))
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
					     100))))
              (add-target (|XtParent (car sliders)) (lambda (target) (set! flange-target target)) #f)))

	(activate-dialog flange-dialog))

      (add-to-menu effects-menu "Flange" (lambda () (post-flange-dialog))))

    (add-to-menu effects-menu flange-label cp-flange))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Flange (~1,2F ~1,2F ~1,3F)" flange-speed flange-amount flange-time)))
                             (change-menu-label effects-menu flange-label new-label)
                             (set! flange-label new-label)))
                         effects-list))


;;; -------- Contrast (brightness control)
;;;

(define contrast-amount 1.0)
(define contrast-label "Contrast enhancement")
(define contrast-dialog #f)
(define contrast-target 'sound)

(define (cp-contrast)
  (let ((peak (maxamp)))
    (save-controls)
    (reset-controls)
    (set! (contrast-control?) #t)
    (set! (contrast-control) contrast-amount)
    (set! (contrast-control-amp) (/ 1.0 peak))
    (set! (amp-control) peak)
    (if (eq? contrast-target 'marks)
	(let ((ms (plausible-mark-samples)))
	  (apply-controls (selected-sound) 0 (car ms) (1+ (- (cadr ms) (car ms)))))
	(apply-controls (selected-sound) (if (eq? contrast-target 'sound) 0 2)))
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
						       "Move the slider to change the contrast intensity."))
					(lambda (w c i)
					  (set! contrast-amount initial-contrast-amount)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* contrast-amount 100)))))))
	      (set! sliders
		    (add-sliders contrast-dialog
				 (list (list "contrast enhancement" 0.0 initial-contrast-amount 10.0
					     (lambda (w context info)
					       (set! contrast-maount (/ (|value info) 100.0)))
					     100))))
              (add-target (|XtParent (car sliders)) (lambda (target) (set! contrast-target target)) #f)))

	(activate-dialog contrast-dialog))

      (add-to-menu effects-menu "Contrast enhancement" (lambda () (post-contrast-dialog))))

    (add-to-menu effects-menu contrast-label cp-contrast))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Contrast enhancement (~1,2F)"  contrast-amount)))
                             (change-menu-label effects-menu contrast-label new-label)
                             (set! contrast-label new-label)))
                         effects-list))


;;; TODO: these filters don't notice the 'marks target yet

;;; -------- Butterworth filters
;;;
;; translated from CLM butterworth.cl:
;;
;;   Sam Heisz, January 1998
;;   inspired by some unit generators written for Csound by Paris Smaragdis
;;   who based his work on formulas from 
;;   Charles Dodge, Computer music: synthesis, composition, and performance.

(define root-2 (sqrt 2.0))

(define (butter b sig) (filter b sig))

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
(define band-pass-target 'sound)

(define (cp-band-pass)
  "Butterworth band-pass filter"
  (if (eq? band-pass-target 'sound)
      (filter-sound (make-butter-band-pass band-pass-freq band-pass-bw))
      (if (eq? band-pass-target 'selection)
	  (filter-selection (make-butter-band-pass band-pass-freq band-pass-bw)))))

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
					     1))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! band-pass-target target)) #f)))

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
(define notch-target 'sound)

(define (cp-notch)
  "Butterworth band-reject filter"
  (if (eq? notch-target 'sound)
      (filter-sound (make-butter-band-reject notch-freq notch-bw))
      (if (eq? notch-target 'selection)
	  (filter-selection (make-butter-band-reject notch-freq notch-bw)))))

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
					     1))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! notch-target target)) #f)))

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
(define high-pass-target 'sound)

(define (cp-high-pass)
  "Butterworth high-pass filter"
  (if (eq? high-pass-target 'sound)
      (filter-sound (make-butter-high-pass high-pass-freq))
      (if (eq? high-pass-target 'selection)
	  (filter-selection (make-butter-high-pass high-pass-freq)))))

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
					     1))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! high-pass-target target)) #f)))

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
(define low-pass-target 'sound)

(define (cp-low-pass)
  "Butterworth low-pass filter"
  (if (eq? low-pass-target 'sound)
      (filter-sound (make-butter-low-pass low-pass-freq))
      (if (eq? low-pass-target 'selection)
	  (filter-selection (make-butter-low-pass low-pass-freq)))))

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
					     1))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! low-pass-target target)) #f)))

	(activate-dialog low-pass-dialog))
      (add-to-menu effects-menu "Low-pass filter" (lambda () (post-low-pass-dialog))))

    (add-to-menu effects-menu low-pass-label cp-low-pass))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Low-pass filter (~1,2F)"  low-pass-freq)))
                             (change-menu-label effects-menu low-pass-label new-label)
                             (set! low-pass-label new-label)))
                         effects-list))

;;; more filters

;;; -------- Comb filter
;;;
;;; (truncate)

(define comb-scaler 0.1)
(define comb-size 50)
(define comb-label "Comb filter")
(define comb-dialog #f)
(define comb-target 'sound)

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
					(lambda (w context info) 
					  (map-chan-over-target (lambda (ignored) (comb-filter comb-scaler comb-size)) comb-target #f))
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
					     1))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! comb-target target)) #f)))

	(activate-dialog comb-dialog))
      (add-to-menu effects-menu "Comb filter" (lambda () (post-comb-dialog))))

    (add-to-menu effects-menu comb-label (lambda () (comb-filter comb-scaler comb-size))))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Comb filter (~1,2F ~1D)" comb-scaler comb-size)))
                             (change-menu-label effects-menu comb-label new-label)
                             (set! comb-label new-label)))
                         effects-list))



;;; -------- Comb-chord filter
;;;
;;; (truncate)

(define comb-chord-scaler 0.95)
(define comb-chord-size 60)
(define comb-chord-amp 0.3)
(define comb-chord-interval-one 0.75)
(define comb-chord-interval-two 1.20)
(define comb-chord-label "Comb filter chord")
(define comb-chord-dialog #f)
(define comb-chord-target 'sound)

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
                                          (map-chan-over-target-with-sync
                                           (lambda (ignored)
                                             (comb-chord comb-chord-scaler comb-chord-size comb-chord-amp
                                                         comb-chord-interval-one comb-chord-interval-two))
					   comb-chord-target
                                           "comb-chord" #f))
                                        (lambda (w context info)
                                          (|XtUnmanageChild comb-chord-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Comb filter chord Help"
    					               "Comb filter chord: create chords by using filters at harmonically related sizes. \
Move the sliders to set the comb-chord parameters."))
                                        (lambda (w c i)
                                          (set! comb-chord-scaler initial-comb-chord-scaler)
                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* comb-chord-scaler 100))))
                                          (set! comb-chord-size initial-comb-chord-size)
                                          (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* comb-chord-size 1))))
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
                                       (list "comb-chord size" 0 initial-comb-chord-size 100
                                             (lambda (w context info)
                                               (set! comb-chord-size (/ (|value info) 1)))
                                             1)
                                       (list "comb-chord amp" 0.0 initial-comb-chord-amp 1.0
                                             (lambda (w context info)
                                               (set! comb-chord-amp (/ (|value info) 100.0)))
                                             100)
                                       (list "comb-chord interval one" 0.0 initial-comb-chord-interval-one 2.0
                                             (lambda (w context info)
                                               (set! comb-chord-interval-one (/ (|value info) 100.0)))
                                             100)
                                       (list "comb-chord interval two" 0.0 initial-comb-chord-interval-two 2.0
                                             (lambda (w context info)
                                               (set! comb-chord-interval-two (/ (|value info) 100.0)))
                                             100))))
              (add-target (|XtParent (car sliders)) (lambda (target) (set! comb-chord-target target)) #f)))

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



;;; -------- Moog filter
;;;

(define cutoff-frequency 10000)
(define resonance 0.5)
(define moog-label "Moog filter")
(define moog-dialog #f)
(define moog-target 'sound)

(if (not (defined? 'moog-filter))
    (load "moog.scm"))

(define (moog freq Q)
    (let ((gen (make-moog-filter freq Q)))
      (lambda (inval)
        (moog-filter gen inval))))

(define (cp-moog)
  (map-chan-over-target (lambda (ignored) (moog cutoff-frequency resonance)) moog-target #f))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-moog-dialog)
        (if (not (|Widget? moog-dialog))
            ;; if moog-dialog doesn't exist, create it
            (let ((initial-cutoff-frequency 10000)
                  (initial-resonance 0.5)
                  (sliders '()))
              (set! moog-dialog 
		    (make-effect-dialog moog-label
					(lambda (w context info)
					  (cp-moog))
					(lambda (w context info)
					  (|XtUnmanageChild moog-dialog))
					(lambda (w context info)
					  (help-dialog "Moog filter Help"
 						"Moog-style 4-pole lowpass filter with 24db/oct rolloff and variable resonance.\n\ Move the sliders to set the filter cutoff frequency and resonance."))
					(lambda (w c i)
					  (set! cutoff-frequency initial-cutoff-frequency)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* cutoff-frequency 100))))
					  (set! resonance initial-resonance)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* resonance 100)))))))
	      (set! sliders
		    (add-sliders moog-dialog
				 (list (list "cutoff frequency" 0 initial-cutoff-frequency 96000
					     (lambda (w context info)
					       (set! cutoff-frequency (/ (|value info) 1)))
					     1)
				       (list "resonance" 0.0 initial-resonance 1.0
					     (lambda (w context info)
					       (set! resonance (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! moog-target target)) #f)))

	(activate-dialog moog-dialog))

      (add-to-menu effects-menu "Moog filter" (lambda () (post-moog-dialog))))

    (add-to-menu effects-menu moog-label cp-moog))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Moog filter (~1,2D ~1,2F)" cutoff-frequency resonance)))
                             (change-menu-label effects-menu moog-label new-label)
                             (set! moog-label new-label)))
                         effects-list))

;;; -------- Robotize
;;;
;;; (progress report?)

(define samp-rate 1.0)
(define osc-amp 0.3)
(define osc-freq 20)
(define robotize-label "Robotize")
(define robotize-dialog #f)
(define robotize-target 'sound)

(define fp-1 ; fp from examp.scm with added beg end args
  (lambda (sr osamp osfrq beg end)
    (let* ((os (make-oscil osfrq))
	       (sr (make-src :srate sr))
	       (len (1+ (- end beg)))
	       (sf (make-sample-reader beg))
	       (out-data (make-vct len)))
	  (vct-map! out-data
		    (lambda () 
		      (src sr (* osamp (oscil os))
			   (lambda (dir)
			     (if (> dir 0)
				 (next-sample sf)
				 (previous-sample sf))))))
	  (free-sample-reader sf)
	  (vct->samples beg len out-data))))

(define (cp-robotize)
  "Robotize is similar to an effect used by the Barrons in the movie Forbidden Planet"
  (let ((ms (and (eq? robotize-target 'marks)
		 (plausible-mark-samples))))
    (fp-1 samp-rate osc-amp osc-freq
	  (if (eq? robotize-target 'sound)
	      0
	      (if (eq? robotize-target 'selection)
		  (selection-position)
		  (car ms)))
	  (if (eq? robotize-target 'sound)
	      (1- (frames))
	      (if (eq? robotize-target 'selection)
		  (+ (selection-position) (selection-length))
		  (cadr ms))))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-robotize-dialog)
        (if (not (|Widget? robotize-dialog))
            ;; if robotize-dialog doesn't exist, create it
            (let ((initial-samp-rate 1.0)
                  (initial-osc-amp 0.3)
                  (initial-osc-freq 20)
                  (sliders '()))
              (set! robotize-dialog 
		    (make-effect-dialog robotize-label
					(lambda (w context info)
					  (cp-robotize))
					(lambda (w context info)
					  (|XtUnmanageChild robotize-dialog))
					(lambda (w context info)
					  (help-dialog "Robotize Help"
						       "Move the sliders to set the sample rate, oscillator amplitude, and oscillator frequency."))
					(lambda (w c i)
					  (set! samp-rate initial-samp-rate)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* samp-rate 100))))
					  (set! osc-amp initial-osc-amp)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* osc-amp 100))))
					  (set! osc-freq initial-osc-freq)
					  (|XtSetValues (caddr sliders) (list |XmNvalue (inexact->exact (* osc-freq 100)))))))
	      (set! sliders
		    (add-sliders robotize-dialog
				 (list (list "sample rate" 0.0 initial-samp-rate 2.0
					     (lambda (w context info)
					       (set! samp-rate (/ (|value info) 100.0)))
					     100)
				       (list "oscillator amplitude" 0.0 initial-osc-amp 1.0
					     (lambda (w context info)
					       (set! osc-amp (/ (|value info) 100.0)))
					     100)
				       (list "oscillator frequency" 0.0 initial-osc-freq 60
					     (lambda (w context info)
					       (set! osc-freq (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! robotize-target target)) #f)))

	(activate-dialog robotize-dialog))

      (add-to-menu effects-menu "Robotize" (lambda () (post-robotize-dialog))))

    (add-to-menu effects-menu robotize-label cp-robotize))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Robotize (~1,2F ~1,2F ~1,2F)" samp-rate osc-amp osc-freq)))
                             (change-menu-label effects-menu robotize-label new-label)
                             (set! robotize-label new-label)))
                         effects-list))


;;; -------- Wobble
;;;
;;; (progress report)

(define wobble-frequency 50)
(define wobble-amplitude 0.5)
(define wobble-label "Wobble")
(define wobble-dialog #f)
(define wobble-target 'sound)

(define hello-dentist-1 ; hello-dentist with added beg and end args
  (lambda (frq amp beg end)
    (let* ((rn (make-rand-interp :frequency frq :amplitude amp))
	   (i 0)
	   (j 0)
	   (len (1+ (- end beg)))
	   (in-data (samples->vct beg len))
	   (out-len (inexact->exact (round (* len (+ 1.0 (* 2 amp))))))
	   (out-data (make-vct out-len))
	   (rd (make-src :srate 1.0 
			 :input (lambda (dir) 
				  (let ((val (if (and (>= i 0) (< i len)) 
						 (vct-ref in-data i) 
						 0.0)))
				    (set! i (+ i dir)) 
				    val)))))
      (do ()
	  ((or (= i len) (= j out-len)))
	(vct-set! out-data j (src rd (rand-interp rn)))
	(set! j (+ j 1)))
      (vct->samples beg j out-data))))

(define (cp-wobble)
  "Wobble: randomly interpolates frequency and amplitude for wobbling effect."
  (let ((ms (and (eq? wobble-target 'marks)
		 (plausible-mark-samples))))
    (hello-dentist-1
     wobble-frequency wobble-amplitude
     (if (eq? wobble-target 'sound)
	 0
	 (if (eq? wobble-target 'selection)
	     (selection-position)
	     (car ms)))
     (if (eq? wobble-target 'sound)
	 (1- (frames))
	 (if (eq? wobble-target 'selection)
	     (+ (selection-position) (selection-length))
	     (cadr ms))))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-wobble-dialog)
        (if (not (|Widget? wobble-dialog))
            ;; if wobble-dialog doesn't exist, create it
            (let ((initial-wobble-frequency 50)
                  (initial-wobble-amplitude 0.5)
                  (sliders '()))
              (set! wobble-dialog 
		    (make-effect-dialog wobble-label
					(lambda (w context info)
					  (cp-wobble))
					(lambda (w context info)
					  (|XtUnmanageChild wobble-dialog))
					(lambda (w context info)
					  (help-dialog "Wobble Help"
						       "Move the sliders to set the wobble frequency and amplitude."))
					(lambda (w c i)
					  (set! wobble-frequency initial-wobble-frequency)
					  (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* wobble-frequency 100))))
					  (set! wobble-amplitude initial-wobble-amplitude)
					  (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* wobble-amplitude 100)))))))
	      (set! sliders
		    (add-sliders wobble-dialog
				 (list (list "wobble frequency" 0 initial-wobble-frequency 100
					     (lambda (w context info)
					       (set! wobble-frequency (/ (|value info) 100.0)))
					     100)
				       (list "wobble-amplitude" 0.0 initial-wobble-amplitude 1.0
					     (lambda (w context info)
					       (set! wobble-amplitude (/ (|value info) 100.0)))
					     100))))
	      (add-target (|XtParent (car sliders)) (lambda (target) (set! wobble-target target)) #f)))

	(activate-dialog wobble-dialog))

      (add-to-menu effects-menu "Wobble" (lambda () (post-wobble-dialog))))

    (add-to-menu effects-menu wobble-label cp-wobble))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Wobble (~1,2F ~1,2F)" wobble-frequency wobble-amplitude)))
                             (change-menu-label effects-menu wobble-label new-label)
                             (set! wobble-label new-label)))
                         effects-list))



;;; -------- Cross synthesis
;;;

(define cross-synth-sound 1)
(define cross-synth-amp .5)
(define cross-synth-fft-size 128)
(define cross-synth-radius 6.0)
(define cross-synth-label "Cross synthesis")
(define cross-synth-dialog #f)
(define cross-synth-default-fft-widget #f)
(define cross-synth-target 'sound)

(define use-combo-box-for-fft-size #f) ; radio-buttons or combo-box choice

(if (not (defined? 'cross-synthesis))
    (define cross-synthesis
      (lambda (cross-snd amp fftsize r)
	;; cross-snd is the index of the other sound (as opposed to the map-chan sound)
	(let* ((freq-inc (/ fftsize 2))
	       (fdr (make-vct fftsize))
	       (fdi (make-vct fftsize))
	       (spectr (make-vct freq-inc))
	       (inctr 0)
	       (ctr freq-inc)
	       (radius (- 1.0 (/ r fftsize)))
	       (bin (/ (srate) fftsize))
	       (formants (make-vector freq-inc)))
	  (do ((i 0 (1+ i)))
	      ((= i freq-inc))
	    (vector-set! formants i (make-formant radius (* i bin))))
	  (lambda (inval)
	    (let ((outval 0.0))
	      (if (= ctr freq-inc)
		  (begin
		    (samples->vct inctr fftsize cross-snd 0 fdr)
		    (set! inctr (+ inctr freq-inc))
		    (spectrum fdr fdi #f fftsize 2)
		    (vct-subtract! fdr spectr)
		    (vct-scale! fdr (/ 1.0 freq-inc))
		    (set! ctr 0)))
	      (set! ctr (+ ctr 1))
	      (vct-add! spectr fdr)
	      (* amp (formant-bank spectr formants inval))))))))

(define (cp-cross-synth)
  (map-chan-over-target
   (lambda (ignored) 
     (cross-synthesis cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius))
   cross-synth-target #f))

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
    					               "Move the sliders to set the number of the soundfile \
to be cross-synthesized, the synthesis amplitude, the FFT size, and the radius value."))
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
				   (list |XmNborderWidth 1
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
				   (list |XmNborderWidth 1
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
		    (|XmStringFree s1)))

	      (add-target (|XtParent (car sliders)) (lambda (target) (set! cross-synth-target target)) #f)))

        (activate-dialog cross-synth-dialog))

      (add-to-menu effects-menu "Cross synthesis" (lambda () (post-cross-synth-dialog))))

    (add-to-menu effects-menu cross-synth-label cp-cross-synth))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Cross synthesis (~1,2D ~1,2F ~1,2D ~1,2F)"
                                                cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius)))
                             (change-menu-label effects-menu cross-synth-label new-label)
                             (set! cross-synth-label new-label)))
                         effects-list))


;;; -------- Rubber sound
;;;

(define rubber-factor 1.0)
(define rubber-label "Rubber sound")
(define rubber-dialog #f)
(define rubber-target 'sound)

(if (not (defined? 'rubber-sound))
    (load "rubber.scm"))

(define (cp-rubber)
  (rubber-sound rubber-factor))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-rubber-dialog)
        (if (not (|Widget? rubber-dialog))
            ;; if rubber-dialog doesn't exist, create it
            (let ((initial-rubber-factor 1.0)
                  (sliders '()))
              (set! rubber-dialog
                    (make-effect-dialog rubber-label
                                        (lambda (w context info) (cp-rubber))
                                        (lambda (w context info) (|XtUnmanageChild rubber-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Rubber sound Help"
                                                       "Stretches or contracts the time of a sound. Move the slider to change the stretch factor."))
                                        (lambda (w c i)
                                          (set! rubber-factor initial-rubber-factor)
                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* rubber-factor 100)))))))
              (set! sliders
                    (add-sliders rubber-dialog
                                 (list (list "stretch factor" 0.0 initial-rubber-factor 5.0
                                             (lambda (w context info)
                                               (set! rubber-factor (/ (|value info) 100.0)))
                                             100))))
              (add-target (|XtParent (car sliders)) (lambda (target) (set! rubber-target target)) #f)))
        (activate-dialog rubber-dialog))

      (add-to-menu effects-menu "Rubber sound" (lambda () (post-rubber-dialog))))

    (add-to-menu effects-menu rubber-label cp-rubber))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Rubber sound (~1,2F)"  rubber-factor)))
                             (change-menu-label effects-menu rubber-label new-label)
                             (set! rubber-label new-label)))
                         effects-list))


;;; -------- Randomize phase
;;;
;;; (source, progress, target)

(define random-phase-amp-scaler 3.14)
(define random-phase-label "Randomize phase")
(define random-phase-dialog #f)

(define (cp-random-phase)
 (rotate-phase (lambda (x) (random random-phase-amp-scaler))))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-random-phase-dialog)
        (if (not (|Widget? random-phase-dialog))
            ;; if random-phase-dialog doesn't exist, create it
            (let ((initial-random-phase-amp-scaler 3.14)
                  (sliders '()))
              (set! random-phase-dialog
                    (make-effect-dialog random-phase-label
                                        (lambda (w context info) (cp-random-phase))
                                        (lambda (w context info) (|XtUnmanageChild random-phase-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Randomize phase"
                                                       "Move the slider to change the randomization amp-scaler."))
                                        (lambda (w c i)
                                          (set! random-phase-amp-scaler initial-random-phase-amp-scaler)
                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* random-phase-amp-scaler 100)))))))
              (set! sliders
                    (add-sliders random-phase-dialog
                                 (list (list "amplitude scaler" 0.0 initial-random-phase-amp-scaler 100.0
                                             (lambda (w context info)
                                               (set! random-phase-amp-scaler (/ (|value info) 100.0)))
                                             100))))))
        (activate-dialog random-phase-dialog))

      (add-to-menu effects-menu "Randomize phase" (lambda () (post-random-phase-dialog))))

    (add-to-menu effects-menu random-phase-label cp-random-phase))

(set! effects-list (cons (lambda ()
                           (let ((new-label (format #f "Randomize phase (~1,2F)"  random-phase-amp-scaler)))
                             (change-menu-label effects-menu random-phase-label new-label)
                             (set! random-phase-label new-label)))
                         effects-list))


;;; -------- Compander
;;;

(define vct (lambda args (list->vct args)))

(define (compand)
  "Compand distorts a sound"
  (let* ((tbl (vct -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250
                   0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000)))
    ;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")
    (lambda (inval)
      (let ((index (+ 8.0 (* 8.0 inval))))
        (array-interp tbl index 17)))))


;;; -------- Remove DC (from Perry Cook's physical modeling toolkit)
;;;

(define (block-dc)
  (let ((lastx 0.0)
        (lasty 0.0))
    (lambda (inval)
      (set! lasty (+ inval (- (* 0.999 lasty) lastx)))
      (set! lastx inval)
      lasty)))

;;; -------- Down-oct (pitch shift downwards one octave)
;;;

(define (down-oct)
  "Down-oct tries to move a sound down an octave"
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

;;; -------- Remove clicks 
;;;

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
;;; limit the output to pulses placed at (just after) wave peaks

(add-to-menu effects-menu #f #f)
(add-to-menu effects-menu "Octave-down" down-oct)
(add-to-menu effects-menu "Remove clicks" remove-clicks)
(add-to-menu effects-menu "Remove DC" (lambda () (map-chan-with-sync (lambda () (block-dc)) "block-dc")))
(add-to-menu effects-menu "Spiker" spike)
(add-to-menu effects-menu "Compand" (lambda () (map-chan-with-sync (lambda () (compand)) "compand")))
(add-to-menu effects-menu "Invert" (lambda () (scale-by -1)))
(add-to-menu effects-menu "Reverse" (lambda () (reverse-sound)))
(add-to-menu effects-menu "Null phase" (lambda () (zero-phase)))
;;;(add-to-menu effects-menu "Randomize phase" (lambda () (rotate-phase (lambda (x) (random 3.1415)))))




