(use-modules (ice-9 format))

(define plugins-list '()) ; menu labels are updated to show current default settings

(define plugins-menu (add-to-main-menu "LADSPA Plugins" (lambda ()
						   (define (update-label plugins)
						     (if (not (null? plugins))
							 (begin
							   ((car plugins))
							   (update-label (cdr plugins)))))
						   (update-label plugins-list))))
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




;;; redefine the main menu

(define plug-menu (main-menu plugins-menu))

(define ladspa-delay-menu-list '()) ; just for testing
(define ladspa-mod-menu-list '()) ; just for testing
(define ladspa-freq-menu-list '()) ; just for testing
(define ladspa-distort-menu-list '()) ; just for testing
(define ladspa-reverb-menu-list '()) ; just for testing


;; this definition only needs to occur once in the entire file
(define (change-label widget new-label)
  (let ((str (XmStringCreateLocalized new-label)))
    (XtSetValues widget (list XmNlabelString str))
    (XmStringFree str)))


(define (apply-ladspa-over-target-with-sync ladspa-description target origin decay)
  ;;
  ;; ladspa-description is a list of ladspa data passed as the 2nd arg to apply-ladspa
  ;;   for example: (list "delay" "delay_5s" delay5s-time delay5s-balance)
  ;;
  ;; target: 'marks -> beg=closest marked sample, dur=samples to next mark
  ;;         'sound -> beg=0, dur=all samples in sound
  ;;         'selection -> beg=selection-position, dur=selection-frames
  ;;         'cursor -> beg=cursor, dur=samples to end of sound
  ;;
  ;; origin is a string naming the edit for use in the edit history list
  ;;
  ;; decay is how long to run the effect past the end of the sound
  ;; first portion borrowed from map-chan-over-target-with-sync with added 'cursor target
  (let* ((snc (sync))
         (ms (and (eq? target 'marks)
                  (plausible-mark-samples)))
         (beg (if (eq? target 'sound)
                  0
                  (if (eq? target 'selection)
                      (selection-position)
                      (if (eq? target 'cursor)
                          (cursor (selected-sound) (selected-channel))
                          (car ms)))))
         (overlap (if decay
                      (inexact->exact (* (srate) decay))
                      0)))
    (apply for-each
           (lambda (snd chn)
             (let ((dur (if (eq? target 'sound)
                            (1- (frames snd chn))
                            (if (eq? target 'selection)
                                (+ (selection-position) (selection-frames))
                                (if (eq? target 'cursor)
                                    (- (frames snd chn) beg)
                                    (cadr ms))))))
               (if (= (sync snd) snc)
                   ;; replace map-chan with apply-ladspa
                   (apply-ladspa

	(let* ((snd (selected-sound))
	       	(chns (channels snd)))
       		(if (= chns 1)
           	(make-sample-reader beg snd chn)
	           ;; assume stereo -- this could collect an arbitrary list
        	   (list (make-sample-reader beg snd chn)
                	 (make-sample-reader beg snd chn))))

;                    (make-sample-reader beg snd chn)
                    ladspa-description
                    dur
                    origin))))
           (if (> snc 0)
               (all-chans)
               (list (list (selected-sound))
                     (list (selected-channel)))))))


;;; DELAY EFFECTS
;;;

(define ladspa-delay-menu (XmCreatePulldownMenu plug-menu "Delay Effects"
                                        (list XmNbackground (basic-color))))
(define ladspa-delay-cascade (XtCreateManagedWidget "Delay Effects" xmCascadeButtonWidgetClass plug-menu
                                            (list XmNsubMenuId ladspa-delay-menu
                                                  XmNbackground (basic-color))))



;;; LADSPA Canyon delay
;;;


(define canyon-delay-left-to-right-time .10)
(define canyon-delay-left-to-right-feedback 0)
(define canyon-delay-right-to-left-time .10)
(define canyon-delay-right-to-left-feedback 0)
(define canyon-delay-filter-cutoff 1000)
(define canyon-delay-dialog #f)
(define canyon-delay-label "Canyon-delay (stereo)")
(define canyon-delay-target 'sound)
(define canyon-delay-truncate #t)
(define canyon-delay-menu-widget #f)


(define (post-canyon-delay-dialog)
  (if (not canyon-delay-dialog)
      ;; if canyon-delay-dialog doesn't exist, create it
      (let ((initial-canyon-delay-left-to-right-time .10)
            (initial-canyon-delay-left-to-right-feedback 0)
            (initial-canyon-delay-right-to-left-time .10)
            (initial-canyon-delay-right-to-left-feedback 0)
            (initial-canyon-delay-filter-cutoff 1000)
            (sliders '()))
        (set! canyon-delay-dialog
              (make-effect-dialog
               canyon-delay-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "cmt" "canyon_delay" canyon-delay-left-to-right-time canyon-delay-left-to-right-feedback canyon-delay-right-to-left-time canyon-delay-right-to-left-feedback canyon-delay-filter-cutoff)
                  canyon-delay-target
                  "canyon delay"
                  (and canyon-delay-truncate
                       (* 4 canyon-delay-left-to-right-time))))
               (lambda (w context info) (XtUnmanageChild canyon-delay-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Canyon delay"
 "A deep stereo cross-delay with built-in low pass filters. Note: This effect works only with stereo soundfiles !"))

               (lambda (w c i)
                 (set! canyon-delay-left-to-right-time initial-canyon-delay-left-to-right-time)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* canyon-delay-left-to-right-time 100))))
                 (set! canyon-delay-left-to-right-feedback initial-canyon-delay-left-to-right-feedback)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* canyon-delay-left-to-right-feedback 100))))
                 (set! canyon-delay-right-to-left-time initial-canyon-delay-right-to-left-time)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* canyon-delay-right-to-left-time 100))))
                 (set! canyon-delay-right-to-left-feedback initial-canyon-delay-right-to-left-feedback)
                 (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* canyon-delay-right-to-left-feedback 100))))
                 (set! canyon-delay-filter-cutoff initial-canyon-delay-filter-cutoff)
                 (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* canyon-delay-filter-cutoff 1)))))))
        (set! sliders
              (add-sliders canyon-delay-dialog
                           (list (list "left to right time (s)" 0.01 initial-canyon-delay-left-to-right-time .99
                                       (lambda (w context info)
                                         (set! canyon-delay-left-to-right-time (/ (.value info) 100)))
                                       100)
                                 (list "left to right feedback (%)" -1 initial-canyon-delay-left-to-right-feedback 1
                                       (lambda (w context info)
                                         (set! canyon-delay-left-to-right-feedback (/ (.value info) 100)))
                                       100)
                                 (list "right to left time (s)" 0.01 initial-canyon-delay-right-to-left-time .99
                                       (lambda (w context info)
                                         (set! canyon-delay-right-to-left-time (/ (.value info) 100)))
                                       100)
                                 (list "right to left feedback (%)" -1 initial-canyon-delay-right-to-left-feedback 1
                                       (lambda (w context info)
                                         (set! canyon-delay-right-to-left-feedback (/ (.value info) 100)))
                                       100)
                                 (list "low-pass filter cutoff (Hz)" 1 initial-canyon-delay-filter-cutoff 44100 
                                       (lambda (w context info)
                                         (set! canyon-delay-filter-cutoff (/ (.value info) 1)))
                                       1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! canyon-delay-target target))
                    (lambda (truncate) (set! canyon-delay-truncate truncate)))))

  (activate-dialog canyon-delay-dialog))

(let ((child (XtCreateManagedWidget "Canyon delay" xmPushButtonWidgetClass ladspa-delay-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-canyon-delay-dialog)))
  (set! ladspa-delay-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Canyon delay (~1,2F ~1,2D ~1,2F ~1,2D ~1,2D)" canyon-delay-left-to-right-time canyon-delay-left-to-right-feedback canyon-delay-right-to-left-time canyon-delay-right-to-left-feedback canyon-delay-filter-cutoff)))
                  (change-label child new-label)))
              ladspa-delay-menu-list)))






;;; LADSPA Delay (5 sec)
;;;

(define delay5s-time .3)
(define delay5s-balance .5)
(define delay5s-dialog #f)
(define delay5s-label "Delay (5 sec)")
(define delay5s-target 'sound)
(define delay5s-truncate #t)
(define delay5s-menu-widget #f)


(define (post-delay5s-dialog)
  (if (not delay5s-dialog)
      ;; if delay5s-dialog doesn't exist, create it
      (let ((initial-delay5s-time 0.3)
	    (initial-delay5s-balance 0.5)
	    (sliders '()))
	(set! delay5s-dialog
	      (make-effect-dialog
	       delay5s-label
	       (lambda (w context info)
		 (apply-ladspa-over-target-with-sync
		  (list "delay" "delay_5s" delay5s-time delay5s-balance)
		  delay5s-target
		  "delay5s"
		  (and delay5s-truncate
		       (* 4 delay5s-time))))
	       (lambda (w context info) (XtUnmanageChild delay5s-dialog)) ; not needed in new-effects.scm version
	
	       (lambda (w context info)
		 (help-dialog "Delay 5s" "The delay time unit is in seconds. The balance slider controls the strength of the delays."))
	
	       (lambda (w c i)
		 (set! delay5s-time initial-delay5s-time)
		 (XtSetValues (car sliders) (list XmNvalue (inexact->exact (* delay5s-time 100))))
		 (set! delay5s-balance initial-delay5s-balance)
		 (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (* delay5s-balance 100)))))))
	(set! sliders
	      (add-sliders delay5s-dialog
			   (list (list "delay time" 0.0 initial-delay5s-time 5.0
				       (lambda (w context info)
					 (set! delay5s-time (/ (.value info) 100.0)))
				       100)
				 (list "balance" 0.0 initial-delay5s-balance 1.0
				       (lambda (w context info)
					 (set! delay5s-balance (/ (.value info) 100.0)))
				       100))))
	(add-target (XtParent (car sliders))
		    (lambda (target) (set! delay5s-target target))
		    (lambda (truncate) (set! delay5s-truncate truncate)))))

  (activate-dialog delay5s-dialog))

(let ((child (XtCreateManagedWidget "Delay 5s" xmPushButtonWidgetClass ladspa-delay-menu
				    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
		 (lambda (w c i)
		   (post-delay5s-dialog)))
  (set! ladspa-delay-menu-list
	(cons (lambda ()
		(let ((new-label (format #f "Delay 5s (~1,2F ~1,2F)" delay5s-time delay5s-balance)))
		  (change-label child new-label)))
	      ladspa-delay-menu-list)))


;;; LADSPA Delayorama
;;;

(define delayorama-random-seed 100)
(define delayorama-input-gain 0)
(define delayorama-feedback 0)
(define delayorama-taps 2)
(define delayorama-first-delay 0.1)
(define delayorama-delay-range 0.1)
(define delayorama-delay-change 1.0)
(define delayorama-delay-random 10)
(define delayorama-amp-change 1.0)
(define delayorama-amp-random 10)
(define delayorama-mix 0.5)
(define delayorama-dialog #f)
(define delayorama-label "Delayorama")
(define delayorama-target 'sound)
(define delayorama-truncate #t)
(define delayorama-menu-widget #f)


(define (post-delayorama-dialog)
   (if (not delayorama-dialog)
       (let ((sliders '()))
         (set! delayorama-dialog
               (make-effect-dialog 
               delayorama-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "delayorama_1402" "delayorama" delayorama-random-seed delayorama-input-gain delayorama-feedback delayorama-taps delayorama-first-delay delayorama-delay-range delayorama-delay-change delayorama-delay-random delayorama-amp-change delayorama-amp-random delayorama-mix)
                  delayorama-target
                  "delayorama"
                  (and delayorama-truncate
                       (* 4 delayorama-first-delay))))
               (lambda (w context info) (XtUnmanageChild delayorama-dialog))
               (lambda (w context info)
                       (help-dialog "Delayorama" "Random seed: Controls the random numbers that will be used to stagger the delays and amplitudes if random is turned up on them. Changing this forces the random values to be recalulated.\n\ Input gain (dB): Controls the gain of the input signal in decibels.\n\ Feedback (%): Controls the amount of output signal fed back into the input.\n\ Number of taps: Controls the number of taps in the delay.\n\ First delay (s): The time (in seconds) of the first delay.\n\ Delay range (s): The time difference (in seconds) between the first and last delay.\n\ Delay change: The scaling factor between one delay and the next.\n\ Delay random (%): The random factor applied to the delay.\n\ Amplitude change: The scaling factor between one amplitude and the next.\n\ Amplitude random (%): The random factor applied to the amplitude.\n\ Dry/wet mix: The level of delayed sound mixed into the output."))
                                       (lambda (w c i)
                                         (set! delayorama-random-seed 100)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* delayorama-random-seed 1))))
                                         (set! delayorama-input-gain 0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* delayorama-input-gain 1))))
                                         (set! delayorama-feedback 0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* delayorama-feedback 1))))
                                         (set! delayorama-taps 2)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* delayorama-taps 1))))
                                         (set! delayorama-first-delay 0.1)
                                         (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* delayorama-first-delay 100))))
                                         (set! delayorama-delay-range 0.1)
                                         (XtSetValues (list-ref sliders 5) (list XmNvalue (inexact->exact (* delayorama-delay-range 100))))
                                         (set! delayorama-delay-change 1.0)
                                         (XtSetValues (list-ref sliders 6) (list XmNvalue (inexact->exact (* delayorama-delay-change 100))))
                                         (set! delayorama-delay-random 10)
                                         (XtSetValues (list-ref sliders 7) (list XmNvalue (inexact->exact (* delayorama-delay-random 1))))
                                         (set! delayorama-amp-change 1.0)
                                         (XtSetValues (list-ref sliders 8) (list XmNvalue (inexact->exact (* delayorama-amp-change 100))))
                                         (set! delayorama-amp-random 0)
                                         (XtSetValues (list-ref sliders 9) (list XmNvalue (inexact->exact (* delayorama-amp-random 1))))
                                         (set! delayorama-mix 0.5)
                                         (XtSetValues (list-ref sliders 10) (list XmNvalue (inexact->exact (* delayorama-mix 100)))))))
             (set! sliders
                   (add-sliders delayorama-dialog
                                (list (list "random seed" 0 100 1000
                                            (lambda (w context info)
                                              (set! delayorama-random-seed (/ (.value info) 1)))
                                            1)
                                      (list "input gain (dB)" -96 0 24
                                            (lambda (w context info)
                                              (set! delayorama-input-gain (/ (.value info) 1)))
                                            1)
                                      (list "feedback (%)" 0 0 100
                                            (lambda (w context info)
                                              (set! delayorama-feedback (/ (.value info) 1)))
                                            1)
                                      (list "number of taps" 2 2 128
                                           (lambda (w context info)
                                              (set! delayorama-taps (/ (.value info) 1)))
                                            1)
                                      (list "first delay (s)" 0.0 0.1 5.0
                                            (lambda (w context info)
                                              (set! delayorama-first-delay (/ (.value info) 100)))
                                            100)
                                      (list "delay range (s)" 0.0001 1.0 6.0
                                            (lambda (w context info)
                                              (set! delayorama-delay-range (/ (.value info) 100)))
                                            100)
                                      (list "delay change" 0.2 1.0 5.0
                                            (lambda (w context info)
                                              (set! delayorama-delay-change (/ (.value info) 100)))
                                            100)
                                      (list "delay random (%)" 0 10 100
                                            (lambda (w context info)
                                              (set! delayorama-delay-random (/ (.value info) 1)))
                                            1)
                                      (list "amplitude change" 0.2 1.0 5.0
                                            (lambda (w context info)
                                              (set! delayorama-amp-change (/ (.value info) 100)))
                                            100)
                                      (list "amplitude random (%)" 0 10 100
                                            (lambda (w context info)
                                              (set! delayorama-amp-random (/ (.value info) 1)))
                                            1)
                                     (list "dry/wet mix" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! delayorama-amp-random (/ (.value info) 100)))
                                            100))))

        (add-target (XtParent (car sliders))
                    (lambda (target) (set! delayorama-target target))
                    (lambda (truncate) (set! delayorama-truncate truncate)))))

  (activate-dialog delayorama-dialog))

(let ((child (XtCreateManagedWidget "Delayorama" xmPushButtonWidgetClass ladspa-delay-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-delayorama-dialog)))
  (set! ladspa-delay-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Delayorama (~1,2D ~1,2D ~1,2D ~1,2D ~1,2F ~1,2F ~1,2F ~1,2D ~1,2F ~1,2D ~1,2F)" delayorama-random-seed delayorama-input-gain delayorama-feedback delayorama-taps delayorama-first-delay delayorama-delay-range delayorama-delay-change delayorama-delay-random delayorama-amp-change delayorama-amp-random delayorama-mix)))
                  (change-label child new-label)))
              ladspa-delay-menu-list)))






;;; LADSPA Feedback Delay (5s)
;;;



(define fbdelay5s-time .3)
(define fbdelay5s-balance .5)
(define fbdelay5s-feedback 0)
(define fbdelay5s-dialog #f)
(define fbdelay5s-label "Feedback delay (5 sec)")
(define fbdelay5s-target 'sound)
(define fbdelay5s-truncate #t)
(define fbdelay5s-menu-widget #f)


(define (post-fbdelay5s-dialog)
  (if (not fbdelay5s-dialog)
      ;; if fbdelay5s-dialog doesn't exist, create it
      (let ((initial-fbdelay5s-time 0.3)
            (initial-fbdelay5s-balance 0.5)
            (initial-fbdelay5s-feedback 0)
            (sliders '()))
        (set! fbdelay5s-dialog
              (make-effect-dialog
               fbdelay5s-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "cmt" "fbdelay_5s" fbdelay5s-time fbdelay5s-balance fbdelay5s-feedback)
                  fbdelay5s-target
                  "fbdelay5s"
                  (and fbdelay5s-truncate
                       (* 4 fbdelay5s-time))))
               (lambda (w context info) (XtUnmanageChild fbdelay5s-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Feedback delay (5 sec)" "Feedback delay with maximum delay time of 5 seconds."))

               (lambda (w c i)
                 (set! fbdelay5s-time initial-fbdelay5s-time)
                 (XtSetValues (car sliders) (list XmNvalue (inexact->exact (* fbdelay5s-time 100))))
                 (set! fbdelay5s-balance initial-fbdelay5s-balance)
                 (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (* fbdelay5s-balance 100))))
                 (set! fbdelay5s-feedback initial-fbdelay5s-feedback)
                 (XtSetValues (caddr sliders) (list XmNvalue (inexact->exact (* fbdelay5s-feedback 100)))))))
        (set! sliders
              (add-sliders fbdelay5s-dialog
                           (list (list "fbdelay time" 0.0 initial-fbdelay5s-time 5.0
                                       (lambda (w context info)
                                         (set! fbdelay5s-time (/ (.value info) 100.0)))
                                       100)
                                 (list "balance" 0.0 initial-fbdelay5s-balance 1.0
                                       (lambda (w context info)
                                         (set! fbdelay5s-balance (/ (.value info) 100.0)))
                                       100)
                                 (list "feedback" -1 initial-fbdelay5s-feedback 1.0
                                       (lambda (w context info)
                                         (set! fbdelay5s-feedback (/ (.value info) 100.0)))
                                       100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! fbdelay5s-target target))
                    (lambda (truncate) (set! fbdelay5s-truncate truncate)))))

  (activate-dialog fbdelay5s-dialog))

(let ((child (XtCreateManagedWidget "Feedback delay 5s" xmPushButtonWidgetClass ladspa-delay-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-fbdelay5s-dialog)))
  (set! ladspa-delay-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Feedback delay 5s (~1,2F ~1,2F ~1,2D)" fbdelay5s-time fbdelay5s-balance fbdelay5s-feedback)))
                  (change-label child new-label)))
              ladspa-delay-menu-list)))


;;; LADSPA Fractional Delay Line
;;;


(define fractional-delay-time 1.0)
(define fractional-delay-feedback 0)
(define fractional-delay-dialog #f)
(define fractional-delay-label "Fractionally addressed delay")
(define fractional-delay-target 'sound)
(define fractional-delay-truncate #t)
(define fractional-delay-menu-widget #f)


(define (post-fractional-delay-dialog)
  (if (not fractional-delay-dialog)
      ;; if fractional-delay-dialog doesn't exist, create it
      (let ((initial-fractional-delay-time 1.0)
            (initial-fractional-delay-feedback 0)
            (sliders '()))
        (set! fractional-delay-dialog
              (make-effect-dialog
               fractional-delay-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "fad_delay_1192" "fadDelay" fractional-delay-time fractional-delay-feedback)
                  fractional-delay-target
                  "fad delay"
                  (and fractional-delay-truncate
                       (* 4 fractional-delay-time))))
               (lambda (w context info) (XtUnmanageChild fractional-delay-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Fractionally addressed delay line"
 "A fixed ring buffer delay implementation. Has different dynamics than a normal delay, more suitable for certain things.\n\ Changes in delay length are generally more pleasing, but delays >2s long have reduced sound quality."))

               (lambda (w c i)
                 (set! fractional-delay-time initial-fractional-delay-time)
                 (XtSetValues (car sliders) (list XmNvalue (inexact->exact (* fractional-delay-time 100))))
                 (set! fractional-delay-feedback initial-fractional-delay-feedback)
                 (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (* fractional-delay-feedback 1)))))))
        (set! sliders
              (add-sliders fractional-delay-dialog
                           (list (list "delay time (s)" 0.0 initial-fractional-delay-time 10.0
                                       (lambda (w context info)
                                         (set! fractional-delay-time (/ (.value info) 100.0)))
                                       100)
                                 (list "feedback (dB)" -70 initial-fractional-delay-feedback 0
                                       (lambda (w context info)
                                         (set! fractional-delay-feedback (/ (.value info) 1)))
                                       1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! fractional-delay-target target))
                    (lambda (truncate) (set! fractional-delay-truncate truncate)))))

  (activate-dialog fractional-delay-dialog))

(let ((child (XtCreateManagedWidget "Fractional Delay" xmPushButtonWidgetClass delay-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-fractional-delay-dialog)))
  (set! ladspa-delay-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Fractional Delay (~1,2F ~1,2D)" fractional-delay-time fractional-delay-feedback)))
                  (change-label child new-label)))
              ladspa-delay-menu-list)))



;;; LADSPA Granular scatter processor
;;;


(define gs-density 5)
(define gs-scatter 1)
(define gs-grain-length 1)
(define gs-grain-attack 0)
(define gs-dialog #f)
(define gs-label "Granular scatter processor")
(define gs-target 'sound)
(define gs-truncate #t)
(define gs-menu-widget #f)


(define (post-gs-dialog)
  (if (not gs-dialog)
      ;; if gs-dialog doesn't exist, create it
      (let ((initial-gs-density 5)
            (initial-gs-scatter 1)
            (initial-gs-grain-length 1)
            (initial-gs-grain-attack 1)
            (sliders '()))
        (set! gs-dialog
              (make-effect-dialog
               gs-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "cmt" "grain_scatter" gs-density gs-scatter gs-grain-length gs-grain-attack)
                  gs-target
                  "granular scatter"
                  (and gs-truncate
                       (* 4 gs-grain-length))))
               (lambda (w context info) (XtUnmanageChild gs-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Granular scatter processor"
 "This plugin generates an output audio stream by scattering short `grains' of sound from an input stream. It is possible to control the length and envelope of these grains, how far away from their source time grains may be scattered and the density (grains/sec) of the texture produced."))

               (lambda (w c i)
                 (set! gs-density initial-gs-density)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* gs-density 1))))
                 (set! gs-scatter initial-gs-scatter)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* gs-scatter 100))))
                 (set! gs-grain-length initial-gs-grain-length)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* gs-grain-length 100))))
                 (set! gs-grain-attack initial-gs-grain-attack)
                 (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* gs-grain-attack 100)))))))
        (set! sliders
              (add-sliders gs-dialog
                           (list (list "density (grains/s)" 0 initial-gs-density 100
                                       (lambda (w context info)
                                         (set! gs-density (/ (.value info) 1)))
                                       1)
                                 (list "scatter" 0 initial-gs-scatter 5
                                       (lambda (w context info)
                                         (set! gs-scatter (/ (.value info) 100)))
                                       100)
                                 (list "grain length (s)" 0 initial-gs-grain-length 5
                                       (lambda (w context info)
                                         (set! gs-grain-length (/ (.value info) 100)))
                                       100)
                                 (list "grain attack (s)" 0 initial-gs-grain-attack 5
                                       (lambda (w context info)
                                         (set! gs-grain-attack (/ (.value info) 100)))
                                       100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! gs-target target))
                    (lambda (truncate) (set! gs-truncate truncate)))))

  (activate-dialog gs-dialog))

(let ((child (XtCreateManagedWidget "Granular scatter processor" xmPushButtonWidgetClass ladspa-delay-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-gs-dialog)))
  (set! ladspa-delay-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Granular scatter processor (~1,2D ~1,2D ~1,2D ~1,2D)" gs-density gs-scatter gs-grain-length gs-grain-attack)))
                  (change-label child new-label)))
              ladspa-delay-menu-list)))



;;; LADSPA Tape delay
;;;

(define tape-delay-tape-speed 1)
(define tape-delay-dry-level 0)
(define tape-delay-tap-1-distance 0)
(define tape-delay-tap-1-level 0)
(define tape-delay-tap-2-distance 0)
(define tape-delay-tap-2-level 0)
(define tape-delay-tap-3-distance 0)
(define tape-delay-tap-3-level 0)
(define tape-delay-tap-4-distance 0)
(define tape-delay-tap-4-level 0)
(define tape-delay-dialog #f)
(define tape-delay-label "Tape delay")
(define tape-delay-target 'sound)
(define tape-delay-truncate #t)
(define tape-delay-menu-widget #f)


(define (post-tape-delay-dialog)
  (if (not tape-delay-dialog)
      ;; if tape-delay-dialog doesn't exist, create it
      (let ((sliders '()))

        (set! tape-delay-dialog
              (make-effect-dialog
               tape-delay-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "tape_delay_1211" "tapeDelay" tape-delay-tape-speed tape-delay-dry-level tape-delay-tap-1-distance tape-delay-tap-1-level tape-delay-tap-2-distance tape-delay-tap-2-level tape-delay-tap-3-distance tape-delay-tap-3-level tape-delay-tap-4-distance tape-delay-tap-4-level)
                  tape-delay-target
                  "tape delay"
                  (and tape-delay-truncate
                       (* 4 tape-delay-tap-1-distance))))
               (lambda (w context info) (XtUnmanageChild tape-delay-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Tape delay" "Correctly models the tape motion and some of the smear effect, but there is no simulation for the head saturation yet. The way the tape accelerates and decelerates gives a nicer delay effect for many purposes."))

                                       (lambda (w c i)
                                         (set! tape-delay-tape-speed 1)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* tape-delay-tape-speed 100))))
                                         (set! tape-delay-dry-level 0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* tape-delay-dry-level 1))))
                                         (set! tape-delay-tap-1-distance 0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* tape-delay-tap-1-distance 100))))
                                         (set! tape-delay-tap-1-level 0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* tape-delay-tap-1-level 1))))
                                         (set! tape-delay-tap-2-distance 0)
                                         (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* tape-delay-tap-2-distance 100))))
                                         (set! tape-delay-tap-2-level 0)
                                         (XtSetValues (list-ref sliders 5) (list XmNvalue (inexact->exact (* tape-delay-tap-2-level 1))))
                                         (set! tape-delay-tap-3-distance 0)
                                         (XtSetValues (list-ref sliders 6) (list XmNvalue (inexact->exact (* tape-delay-tap-3-distance 100))))
                                         (set! tape-delay-tap-3-level 0)
                                         (XtSetValues (list-ref sliders 7) (list XmNvalue (inexact->exact (* tape-delay-tap-3-level 1))))
                                         (set! tape-delay-tap-4-distance 0)
                                         (XtSetValues (list-ref sliders 8) (list XmNvalue (inexact->exact (* tape-delay-tap-4-distance 100))))
                                         (set! tape-delay-tap-4-level 0)
                                         (XtSetValues (list-ref sliders 9) (list XmNvalue (inexact->exact (* tape-delay-tap-4-level 1)))))))
             (set! sliders
                   (add-sliders tape-delay-dialog
                                (list (list "tape speed (inches/sec, 1=normal)" 0 1 10
                                            (lambda (w context info)
                                              (set! tape-delay-tape-speed (/ (.value info) 100)))
                                            100)
                                      (list "dry level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-dry-level (/ (.value info) 1)))
                                            1)
                                      (list "tap 1 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-1-distance (/ (.value info) 100)))
                                            100)
                                      (list "tap 1 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-1-level (/ (.value info) 1)))
                                            1)
                                      (list "tap 2 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-2-distance (/ (.value info) 100)))
                                            100)
                                      (list "tap 2 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-2-level (/ (.value info) 1)))
                                            1)
                                      (list "tap 3 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-3-distance (/ (.value info) 100)))
                                            100)
                                      (list "tap 3 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-3-level (/ (.value info) 1)))
                                            1)
                                      (list "tap 4 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-4-distance (/ (.value info) 100)))
                                            100)
                                      (list "tap 4 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-4-level (/ (.value info) 1)))
                                            1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! tape-delay-target target))
                    (lambda (truncate) (set! tape-delay-truncate truncate)))))

 (activate-dialog tape-delay-dialog))

(let ((child (XtCreateManagedWidget "Tape delay" xmPushButtonWidgetClass ladspa-delay-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-tape-delay-dialog)))
  (set! ladspa-delay-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Tape delay (~1,2D ~1,2D ~1,2D ~1,2D ~1,2D ~1,2D ~1,2D ~1,2D ~1,2D ~1,2D)" tape-delay-tape-speed tape-delay-dry-level tape-delay-tap-1-distance tape-delay-tap-1-level tape-delay-tap-2-distance tape-delay-tap-2-level tape-delay-tap-3-distance tape-delay-tap-3-level tape-delay-tap-4-distance tape-delay-tap-4-level)))
                  (change-label child new-label)))
              ladspa-delay-menu-list)))



;;; DISTORTION EFFECTS
;;;

(define ladspa-distort-menu (XmCreatePulldownMenu plug-menu "Distortion Effects"
                                        (list XmNbackground (basic-color))))
(define ladspa-distort-cascade (XtCreateManagedWidget "Distortion Effects" xmCascadeButtonWidgetClass plug-menu
                                            (list XmNsubMenuId ladspa-distort-menu
                                                  XmNbackground (basic-color))))



;;; LADSPA Decimator
;;;

(define decimator-bit-depth 24)
(define decimator-sample-rate 1.0)
(define decimator-dialog #f)
(define decimator-label "Decimator")
(define decimator-target 'sound)
(define decimator-menu-widget #f)

(define (post-decimator-dialog)
  (if (not decimator-dialog)
      ;; if decimator-dialog doesn't exist, create it
      (let ((initial-decimator-bit-depth 24)
            (initial-decimator-sample-rate 1.0)
            (sliders '()))
        (set! decimator-dialog
              (make-effect-dialog
               decimator-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "decimator_1202" "decimator" decimator-bit-depth (* (srate) decimator-sample-rate))
                  decimator-target
                  "Decimator" #f))
               (lambda (w context info) (XtUnmanageChild decimator-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Decimator" "Decimates by reducing the effective sample rate and reducing the bit depth of the input signal. Allows non-integer values for smooth transitions between clean and low-fidelity signals.\n\ Bit depth: The bit depth that the signal will be reduced to.\n\ Sample rate scalar: Multiplies (scales) the signal sample rate by this value."))

               (lambda (w c i)
                 (set! decimator-bit-depth initial-decimator-bit-depth)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* decimator-bit-depth 1))))
                 (set! decimator-sample-rate initial-decimator-sample-rate)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* decimator-sample-rate 1000)))))))
        (set! sliders
              (add-sliders decimator-dialog
                           (list (list "bit depth" 1 initial-decimator-bit-depth 24 
                                       (lambda (w context info)
                                         (set! decimator-bit-depth (/ (.value info) 1)))
                                       1)
				 (list "sample rate scalar" 0.001 initial-decimator-sample-rate 1.0
                                       (lambda (w context info)
                                         (set! decimator-sample-rate (/ (.value info) 1000)))
                                       1000))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! decimator-target target)) #f)))

  (activate-dialog decimator-dialog))

(let ((child (XtCreateManagedWidget "Decimator" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-decimator-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Decimator (~1,2D ~1,2F)" decimator-bit-depth decimator-sample-rate)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))



;;; LADSPA Diode
;;;

(define diode-mode 0.0)
(define diode-dialog #f)
(define diode-label "Diode processor")
(define diode-target 'sound)
(define diode-menu-widget #f)

(define (post-diode-dialog)
  (if (not diode-dialog)
      ;; if diode-dialog doesn't exist, create it
      (let ((initial-diode-mode 0.0)
            (sliders '()))
        (set! diode-dialog
              (make-effect-dialog
               diode-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "diode_1185" "diode" diode-mode)
                  diode-target
                  "Diode processor" #f))
               (lambda (w context info) (XtUnmanageChild diode-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Diode" "Mangles the signal as if it had been passed through a diode rectifier network. You should probably follow this with a DC offset remover, unless you want the offset.\n\ Mode: 0 for none, 1 for half wave, 2 for full wave, 3 for silence. The mode parameter is continuously variable from through to half-wave rectification to full-wave to silence."))

               (lambda (w c i)
                 (set! diode-mode initial-diode-mode)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* diode-mode 100)))))))
        (set! sliders
              (add-sliders diode-dialog
                           (list (list "bit depth" 0.0 initial-diode-mode 3.0
                                       (lambda (w context info)
                                         (set! diode-mode (/ (.value info) 100)))
                                       100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! diode-target target)) #f)))

  (activate-dialog diode-dialog))

(let ((child (XtCreateManagedWidget "Diode processor" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-diode-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Diode processor (~1,2F)" diode-mode)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))




;;; LADSPA Fast overdrive
;;;

(define foverdrive-level 1.0)
(define foverdrive-dialog #f)
(define foverdrive-label "Fast overdrive")
(define foverdrive-target 'sound)
(define foverdrive-menu-widget #f)

(define (post-foverdrive-dialog)
  (if (not foverdrive-dialog)
      ;; if foverdrive-dialog doesn't exist, create it
      (let ((initial-foverdrive-level 1.0)
            (sliders '()))
        (set! foverdrive-dialog
              (make-effect-dialog
               foverdrive-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "foverdrive_1196" "foverdrive" foverdrive-level)
                  foverdrive-target
                  "Fast overdrive" #f))
               (lambda (w context info) (XtUnmanageChild foverdrive-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Fast overdrive" "A simple overdrive. Compresses the extreme peaks to make a sound similar to an overdriven amplifier.\n\ Level: Controls the point at which the signal starts to distort and the degree of distortion."))

               (lambda (w c i)
                 (set! foverdrive-level initial-foverdrive-level)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* foverdrive-level 100)))))))
        (set! sliders
              (add-sliders foverdrive-dialog
                           (list (list "level" 1.0 initial-foverdrive-level 3.0
                                       (lambda (w context info)
                                         (set! foverdrive-level (/ (.value info) 100)))
                                       100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! foverdrive-target target)) #f )))

  (activate-dialog foverdrive-dialog))

(let ((child (XtCreateManagedWidget "Fast overdrive" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-foverdrive-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Fast overdrive (~1,2F)" foverdrive-level)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))



;;; LADSPA Foldover distortion
;;;

(define foldover-drive 0.0)
(define foldover-skew 0.0)
(define foldover-dialog #f)
(define foldover-label "Foldover distortion")
(define foldover-target 'sound)
(define foldover-menu-widget #f)

(define (post-foldover-dialog)
  (if (not foldover-dialog)
      ;; if foldover-dialog doesn't exist, create it
      (let ((initial-foldover-drive 0.0)
            (initial-foldover-skew 0.0)
            (sliders '()))
        (set! foldover-dialog
              (make-effect-dialog
               foldover-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "foldover_1213" "foldover" foldover-drive foldover-skew)
                  foldover-target
                  "Foldover distortion" #f ))
               (lambda (w context info) (XtUnmanageChild foldover-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Foldover distortion" "Uses a sine wave approximation to simulate valve-style foldover distortion. Probably should have a DC offset remover on the output, but it's not always necessary.\n\ Drive: Controls the degree of distortion.\n\ Skew: Controls the asymmetry of the waveform."))

               (lambda (w c i)
                 (set! foldover-drive initial-foldover-drive)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* foldover-drive 100))))
                 (set! foldover-skew initial-foldover-skew)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* foldover-skew 100)))))))
        (set! sliders
              (add-sliders foldover-dialog
                           (list (list "drive" 0.0 initial-foldover-drive 1.0
                                       (lambda (w context info)
                                         (set! foldover-drive (/ (.value info) 100)))
                                       100)
				 (list "skew" 0.0 initial-foldover-skew 1.0
                                       (lambda (w context info)
                                         (set! foldover-skew (/ (.value info) 100)))
                                       100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! foldover-target target)) #f )))

  (activate-dialog foldover-dialog))

(let ((child (XtCreateManagedWidget "Foldover distortion" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-foldover-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Foldover distortion (~1,2F ~1,2F)" foldover-drive foldover-skew)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))




;;; LADSPA GSM simulator
;;;

(define gsm-simulator-mix .50)
(define gsm-simulator-passes 5)
(define gsm-simulator-error-rate 10)
(define gsm-simulator-dialog #f)
(define gsm-simulator-label "GSM simulator")
(define gsm-simulator-target 'sound)
(define gsm-simulator-menu-widget #f)

(define (post-gsm-simulator-dialog)
  (if (not gsm-simulator-dialog)
      ;; if gsm-simulator-dialog doesn't exist, create it
      (let ((initial-gsm-simulator-mix .50)
	    (initial-gsm-simulator-passes 5)
	    (initial-gsm-simulator-error-rate 10)
            (sliders '()))
        (set! gsm-simulator-dialog
              (make-effect-dialog
               gsm-simulator-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "gsm_1215" "gsm" gsm-simulator-mix gsm-simulator-passes gsm-simulator-error-rate)
                  gsm-simulator-target
                  "GSM simulator" #f))
               (lambda (w context info) (XtUnmanageChild gsm-simulator-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "GSM simulator" "Encodes and decodes a signal using the GSM voice compression system. Has the effect of making the signal sound like it is being sent over a European mobile phone network.\n\ Dry/wet mix: 0 will give you the dry signal (but with the appropriate amount of delay), 1 will give you a totally wet signal.\n\  Number of passes: The number of times the signal is sent through the encode/decode process. Increases the CPU consumption almost linearly, and it will become more peaky so less friendly to realtime processing.\n\  Error rate (bits/block): The number of simulated bits that get changed during the transmission process."))

               (lambda (w c i)
                 (set! gsm-simulator-mix initial-gsm-simulator-mix)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* gsm-simulator-mix 100))))
                 (set! gsm-simulator-passes initial-gsm-simulator-passes)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* gsm-simulator-passes 1))))
                 (set! gsm-simulator-error-rate initial-gsm-simulator-error-rate)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* gsm-simulator-error-rate 1)))))))
        (set! sliders
              (add-sliders gsm-simulator-dialog
                           (list 
                                 (list "wet/dry mix" 0.0 initial-gsm-simulator-mix 1.0
                                        (lambda (w context info)
                                          (set! gsm-simulator-mix (/ (.value info) 100)))
                                          100)
                                 (list "number of passes" 0 initial-gsm-simulator-passes 10
                                         (lambda (w context info)
                                          (set! gsm-simulator-passes (/ (.value info) 1)))
                                          1)
                                 (list "error rate (bits/block)" 0 initial-gsm-simulator-error-rate 30
                                          (lambda (w context info)
                                           (set! gsm-simulator-error-rate (/ (.value info) 1)))
                                           1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! gsm-simulator-target target)) #f)))

  (activate-dialog gsm-simulator-dialog))

(let ((child (XtCreateManagedWidget "GSM simulator" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-gsm-simulator-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "GSM simulator (~1,2F ~1,2D ~1,2D)" gsm-simulator-mix gsm-simulator-passes gsm-simulator-error-rate)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))




;;; LADSPA Mono overdrive
;;;

(define overdrive-amp-limit 0)
(define overdrive-level 0.0)
(define overdrive-low-density-color 0)
(define overdrive-high-density-color 0)
(define overdrive-dialog #f)
(define overdrive-label "Mono overdrive")
(define overdrive-target 'sound)
(define overdrive-menu-widget #f)

(define (post-overdrive-dialog)
  (if (not overdrive-dialog)
      ;; if overdrive-dialog doesn't exist, create it
      (let ((initial-overdrive-amp-limit 0)
	    (initial-overdrive-level 0.0)
	    (initial-overdrive-low-density-color 0)
	    (initial-overdrive-high-density-color 0)
            (sliders '()))
        (set! overdrive-dialog
              (make-effect-dialog
               overdrive-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "overdrive_1182" "overdrive" overdrive-amp-limit overdrive-level overdrive-low-density-color overdrive-high-density-color)
                  overdrive-target
                  "Mono overdrive" #f))
               (lambda (w context info) (XtUnmanageChild overdrive-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Mono overdrive" "A basic overdrive effect, with controls for the degree of compression and for the distortion.\n\ Amps limit (in dB relative to nominal 0): Mostly this parameter is used to artificially lower the headroom of the amp, but it can also be used to counteract the effects of hosts that ignore the audio input range hints. If the host is using 16-bit int WAV files then a good guess for limit is +80dB.\n\ Drive level: This controls the degree of amplifier compression. Values above 1.0 will work, but they produce unpredictable output levels. Lowering the limit is a better way of increasing the distortion.\n\ Low-density coloration: Controls the amplitude of some low (input space) frequency amplitude distortion.\n\ High-density coloration: Controls the amplitude of some high (input space) frequency amplitude distortion."))

               (lambda (w c i)
                 (set! overdrive-amp-limit initial-overdrive-amp-limit)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* overdrive-amp-limit 100))))
                 (set! overdrive-level initial-overdrive-level)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* overdrive-level 1))))
                 (set! overdrive-low-density-color initial-overdrive-low-density-color)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* overdrive-low-density-color 1))))
                 (set! overdrive-high-density-color initial-overdrive-high-density-color)
                 (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* overdrive-high-density-color 1)))))))
        (set! sliders
              (add-sliders overdrive-dialog
                           (list 
                                 (list "amp limit (dB relative to nominal 0)" -100 initial-overdrive-amp-limit 100
                                        (lambda (w context info)
                                          (set! overdrive-amp-limit (/ (.value info) 100)))
                                          1)
                                 (list "drive level" 0.0 initial-overdrive-level 1.0
                                         (lambda (w context info)
                                          (set! overdrive-level (/ (.value info) 1)))
                                          100)
                                 (list "low-density coloration" 0 initial-overdrive-low-density-color 100
                                          (lambda (w context info)
                                           (set! overdrive-low-density-color (/ (.value info) 1)))
                                           1)
                                 (list "high-density coloration" 0 initial-overdrive-high-density-color 100
                                          (lambda (w context info)
                                           (set! overdrive-high-density-color (/ (.value info) 1)))
                                           1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! overdrive-target target)) #f )))

  (activate-dialog overdrive-dialog))

(let ((child (XtCreateManagedWidget "Mono overdrive" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-overdrive-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Mono overdrive (~1,2D ~1,2F ~1,2D ~1,2D)" overdrive-amp-limit overdrive-level overdrive-low-density-color overdrive-level overdrive-hig-density-color)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))





;;; LADSPA Barry's Satan maximizer
;;;

(define satan-decay-time 10)
(define satan-knee-point 0)
(define satan-dialog #f)
(define satan-label "Barry's Satan maximizer")
(define satan-target 'sound)
(define satan-menu-widget #f)

(define (post-satan-dialog)
  (if (not satan-dialog)
      ;; if satan-dialog doesn't exist, create it
      (let ((initial-satan-decay-time 10)
	    (initial-satan-knee-point 0)
            (sliders '()))
        (set! satan-dialog
              (make-effect-dialog
               satan-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "satan_maximiser_1408" "satanMaximiser" satan-decay-time satan-knee-point)
                  satan-target
                  "Satan maximizer" #f))
               (lambda (w context info) (XtUnmanageChild satan-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Barry's Satan maximizer" "Compresses signals with a stupidly short attack and decay, infinite ratio and hard knee. Not really a compressor, but good harsh distortion.\n\ Decay time (samples): Controls the envelope decay time.\n\ Knee point (dB): Controls the knee roll-off point, i.e. the point above which the compression kicks in. 0 will have no effect, -90 will remove virtually the entire dynamic range."))

               (lambda (w c i)
                 (set! satan-decay-time initial-satan-decay-time)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* satan-decay-time 1))))
                 (set! satan-knee-point initial-satan-knee-point)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* satan-knee-point 1)))))))
        (set! sliders
              (add-sliders satan-dialog
                           (list 
                                 (list "decay time (samples)" 2 initial-satan-decay-time 30
                                        (lambda (w context info)
                                          (set! satan-decay-time (/ (.value info) 1)))
                                          1)
                                 (list "knee point (dB)" -90 initial-satan-knee-point 0
                                         (lambda (w context info)
                                          (set! satan-knee-point (/ (.value info) 1)))
                                          1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! satan-target target)) #f)))

  (activate-dialog satan-dialog))

(let ((child (XtCreateManagedWidget "Barry's Satan maximizer" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-satan-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Barry's Satan maximizer (~1,2D ~1,2D )" satan-decay-time satan-knee-point)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))





;;; LADSPA Signal sifter
;;;

(define sifter-size 100)
(define sifter-dialog #f)
(define sifter-label "Signal sifter")
(define sifter-target 'sound)
(define sifter-menu-widget #f)

(define (post-sifter-dialog)
  (if (not sifter-dialog)
      ;; if sifter-dialog doesn't exist, create it
      (let ((initial-sifter-size 100)
            (sliders '()))
        (set! sifter-dialog
              (make-effect-dialog
               sifter-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "sifter_1210" "sifter" sifter-size)
                  sifter-target
                  "signal sifter" #f))
               (lambda (w context info) (XtUnmanageChild sifter-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Signal sifter" "Sorts and mixes blocks of the input signal to give a 'bumpy ramp' effect.\n\ Certain types of input will produce silence on the output (mostly ones with only low frequency components).\n\ This is a very odd effect, and doesn't really have any music applications, but can produce some interesting noises."))

               (lambda (w c i)
                 (set! sifter-size initial-sifter-size)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* sifter-size 1)))))))
        (set! sliders
              (add-sliders sifter-dialog
                           (list 
                                 (list "block size"  1 initial-sifter-size 2000
                                        (lambda (w context info)
                                          (set! sifter-size (/ (.value info) 1)))
                                          1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! sifter-target target)) #f)))

  (activate-dialog sifter-dialog))

(let ((child (XtCreateManagedWidget "Signal sifter" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-sifter-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Signal sifter (~1,2D)" sifter-size)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))



;;; LADSPA Stereo overdrive
;;;

(define overdrive_s-amp-limit 0)
(define overdrive_s-level 0.0)
(define overdrive_s-low-density-color 0)
(define overdrive_s-high-density-color 0)
(define overdrive_s-dialog #f)
(define overdrive_s-label "Stereo overdrive")
(define overdrive_s-target 'sound)
(define overdrive_s-menu-widget #f)

(define (post-overdrive_s-dialog)
  (if (not overdrive_s-dialog)
      ;; if overdrive_s-dialog doesn't exist, create it
      (let ((initial-overdrive_s-amp-limit 0)
	    (initial-overdrive_s-level 0.0)
	    (initial-overdrive_s-low-density-color 0)
	    (initial-overdrive_s-high-density-color 0)
            (sliders '()))
        (set! overdrive_s-dialog
              (make-effect-dialog
               overdrive_s-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "overdrive_s_1183" "overdrive_s" overdrive_s-amp-limit overdrive_s-level overdrive_s-low-density-color overdrive_s-high-density-color)
                  overdrive_s-target
                  "Stereo overdrive" #f))
               (lambda (w context info) (XtUnmanageChild overdrive_s-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Stereo overdrive" "A basic overdrive_s effect, with controls for the degree of compression and for the distortion.\n\ Amps limit (in dB relative to nominal 0): Mostly this parameter is used to artificially lower the headroom of the amp, but it can also be used to counteract the effects of hosts that ignore the audio input range hints. If the host is using 16-bit int WAV files then a good guess for limit is +80dB.\n\ Drive level: This controls the degree of amplifier compression. Values above 1.0 will work, but they produce unpredictable output levels. Lowering the limit is a better way of increasing the distortion.\n\ Low-density coloration: Controls the amplitude of some low (input space) frequency amplitude distortion.\n\ High-density coloration: Controls the amplitude of some high (input space) frequency amplitude distortion."))

               (lambda (w c i)
                 (set! overdrive_s-amp-limit initial-overdrive_s-amp-limit)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* overdrive_s-amp-limit 100))))
                 (set! overdrive_s-level initial-overdrive_s-level)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* overdrive_s-level 1))))
                 (set! overdrive_s-low-density-color initial-overdrive_s-low-density-color)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* overdrive_s-low-density-color 1))))
                 (set! overdrive_s-high-density-color initial-overdrive_s-high-density-color)
                 (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* overdrive_s-high-density-color 1)))))))
        (set! sliders
              (add-sliders overdrive_s-dialog
                           (list 
                                 (list "amp limit (dB relative to nominal 0)" -100 initial-overdrive_s-amp-limit 100
                                        (lambda (w context info)
                                          (set! overdrive_s-amp-limit (/ (.value info) 100)))
                                          1)
                                 (list "drive level" 0.0 initial-overdrive_s-level 1.0
                                         (lambda (w context info)
                                          (set! overdrive_s-level (/ (.value info) 1)))
                                          100)
                                 (list "low-density coloration" 0 initial-overdrive_s-low-density-color 100
                                          (lambda (w context info)
                                           (set! overdrive_s-low-density-color (/ (.value info) 1)))
                                           1)
                                 (list "high-density coloration" 0 initial-overdrive_s-high-density-color 100
                                          (lambda (w context info)
                                           (set! overdrive_s-high-density-color (/ (.value info) 1)))
                                           1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! overdrive_s-target target)) #f)))

  (activate-dialog overdrive_s-dialog))

(let ((child (XtCreateManagedWidget "Stereo overdrive" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-overdrive_s-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Stereo overdrive (~1,2D ~1,2F ~1,2D ~1,2D)" overdrive_s-amp-limit overdrive_s-level overdrive_s-low-density-color overdrive_s-level overdrive_s-hig-density-color)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))





;;; LADSPA Transient mangler
;;;

(define transient-attack-speed 1)
(define transient-knee-point 1)
(define transient-dialog #f)
(define transient-label "Transient mangler")
(define transient-target 'sound)
(define transient-menu-widget #f)

(define (post-transient-dialog)
  (if (not transient-dialog)
      ;; if transient-dialog doesn't exist, create it
      (let ((initial-transient-attack-speed 1)
	    (initial-transient-knee-point 1)
            (sliders '()))
        (set! transient-dialog
              (make-effect-dialog
               transient-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "transient_1206" "transient" transient-attack-speed transient-knee-point)
                  transient-target
                  "transient mangler" #f))
               (lambda (w context info) (XtUnmanageChild transient-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Transient mangler" "No help yet, sorry"))

               (lambda (w c i)
                 (set! transient-attack-speed initial-transient-attack-speed)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* transient-attack-speed 100))))
                 (set! transient-knee-point initial-transient-knee-point)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* transient-knee-point 100)))))))
        (set! sliders
              (add-sliders transient-dialog
                           (list 
                                 (list "attack speed" -1 initial-transient-attack-speed 1
                                        (lambda (w context info)
                                          (set! transient-attack-speed (/ (.value info) 100)))
                                          100)
                                 (list "sustain time" -1 initial-transient-knee-point 1
                                         (lambda (w context info)
                                          (set! transient-knee-point (/ (.value info) 100)))
                                          100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! transient-target target)) #f)))

  (activate-dialog transient-dialog))

(let ((child (XtCreateManagedWidget "Transient mangler" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-transient-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Transient mangler (~1,2F ~1,2F )" transient-attack-speed transient-knee-point)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))





;;; LADSPA Valve saturation
;;;

(define valve-distortion-level .5)
(define valve-distortion-character .5)
(define valve-distortion-dialog #f)
(define valve-distortion-label "Valve saturation")
(define valve-distortion-target 'sound)
(define valve-distortion-menu-widget #f)

(define (post-valve-distortion-dialog)
  (if (not valve-distortion-dialog)
      ;; if valve-distortion-dialog doesn't exist, create it
      (let ((initial-valve-distortion-level .5)
	    (initial-valve-distortion-character .5)
            (sliders '()))
        (set! valve-distortion-dialog
              (make-effect-dialog
               valve-distortion-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "valve_1209" "valve" valve-distortion-level valve-distortion-character)
                  valve-distortion-target
                  "valve saturation" #f))
               (lambda (w context info) (XtUnmanageChild valve-distortion-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Valve saturation" "A model of valve (tube) distortion, lacking some of the harmonics you would get in a real tube amp, but sounds good nonetheless.\n\ Distortion level: How hard the signal is driven against the limit of the amplifier.\n\ Distortion character: The hardness of the sound, low for softer, high for harder."))

               (lambda (w c i)
                 (set! valve-distortion-level initial-valve-distortion-level)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* valve-distortion-level 100))))
                 (set! valve-distortion-character initial-valve-distortion-character)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* valve-distortion-character 100)))))))
        (set! sliders
              (add-sliders valve-distortion-dialog
                           (list 
                                 (list "distortion level"  0.0 initial-valve-distortion-level 1.0
                                        (lambda (w context info)
                                          (set! valve-distortion-level (/ (.value info) 100.0)))
                                          100)
                                 (list "distortion character" 0.0 initial-valve-distortion-character 1.0
                                         (lambda (w context info)
                                          (set! valve-distortion-character (/ (.value info) 100.0)))
                                          100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! valve-distortion-target target)) #f)))

  (activate-dialog valve-distortion-dialog))

(let ((child (XtCreateManagedWidget "Valve saturation" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-valve-distortion-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Valve saturation (~1,2F ~1,2F )" valve-distortion-level valve-distortion-character)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))




;;; LADSPA Wave shaper
;;;

(define waveshaper-waveshape 0.0)
(define waveshaper-dialog #f)
(define waveshaper-label "Wave shaper")
(define waveshaper-target 'sound)
(define waveshaper-menu-widget #f)

(define (post-waveshaper-dialog)
  (if (not waveshaper-dialog)
      ;; if waveshaper-dialog doesn't exist, create it
      (let ((initial-waveshaper-waveshape 0.0)
            (sliders '()))
        (set! waveshaper-dialog
              (make-effect-dialog
               waveshaper-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "shaper_1187" "shaper" waveshaper-waveshape)
                  waveshaper-target
                  "wave shaper" #f))
               (lambda (w context info) (XtUnmanageChild waveshaper-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Wave shaper" "Reshapes the wave by an exponential function. Inspiration was taken from the Nord module of the same name.\n\ If you are getting rubbish out then it's probably because the host isn't using the input/output range hints, which are very important for this plugin.\n\ Waveshape: Positive values have an expanding effect, negative values have a compressing effect."))

               (lambda (w c i)
                 (set! waveshaper-waveshape initial-waveshaper-waveshape)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* waveshaper-waveshape 100)))))))
        (set! sliders
              (add-sliders waveshaper-dialog
                           (list 
                                 (list "waveshape" -10.0 initial-waveshaper-waveshape 10.0
                                        (lambda (w context info)
                                          (set! waveshaper-waveshape (/ (.value info) 100)))
                                          100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! waveshaper-target target)) #f)))

  (activate-dialog waveshaper-dialog))

(let ((child (XtCreateManagedWidget "Wave shaper" xmPushButtonWidgetClass ladspa-distort-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-waveshaper-dialog)))
  (set! ladspa-distort-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Wave shaper (~1,2F)" waveshaper-waveshape)))
                  (change-label child new-label)))
              ladspa-distort-menu-list)))




;;; FILTERS AND EQUALIZERS
;;;

(define ladspa-filter-menu (XmCreatePulldownMenu plug-menu "Filters/EQs"
                                        (list XmNbackground (basic-color))))
(define ladspa-filter-cascade (XtCreateManagedWidget "Filters/EQs" xmCascadeButtonWidgetClass plug-menu
                                            (list XmNsubMenuId ladspa-filter-menu
                                                  XmNbackground (basic-color))))


;;; Comb filter
;;;

(define comb-filter-band-separation 100)
(define comb-filter-feedback 0.0)
(define comb-filter-dialog #f)
(define comb-filter-label "Comb filter")

(define (cp-comb-filter)
  (apply-ladspa (make-sample-reader (cursor))
                (list "comb_1190" "comb" comb-filter-band-separation comb-filter-feedback)
                (- (frames) (cursor))
                "comb filter"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-comb-filter-dialog)
       (if (not (Widget? comb-filter-dialog))
           (let ((sliders '()))
             (set! comb-filter-dialog
                   (make-effect-dialog "comb filter ladspa plugin"
                                       (lambda (w context info) (cp-comb-filter))
                                       (lambda (w context info) (XtUnmanageChild comb-filter-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Comb filter"
                                                      "Band separation controls the distance between the filter's peaks.\n\ Feedback level increases the distinctive wooshy phaser sound."))
                                       (lambda (w c i)
                                         (set! comb-filter-band-separation 100)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* comb-filter-band-separation 1))))
                                         (set! comb-filter-feedback 0.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* comb-filter-feedback 100)))))))
             (set! sliders
                   (add-sliders comb-filter-dialog
                                (list 
                                      (list "band-separation (Hz)" 16 100 640
                                            (lambda (w context info)
                                              (set! comb-filter-band-separation (/ (.value info) 1)))
                                            1)
 				      (list "feedback" -0.99 0.0 0.99
                                            (lambda (w context info)
                                              (set! comb-filter-feedback (/ (.value info) 100)))
                                            100))))))
       (activate-dialog comb-filter-dialog))))

      (let ((child (XtCreateManagedWidget "Comb filter" xmPushButtonWidgetClass ladspa-filter-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-comb-filter-dialog))))


;;; Karaoke
;;;

(define karaoke-vocal-volume 0)
(define karaoke-dialog #f)
(define karaoke-label "Karaoke")

(define (cp-karaoke)
   (apply-ladspa (make-sample-reader (cursor))
      (list "karaoke_1409" "amp" karaoke-vocal-volume)
         (- (frames) (cursor))
         "Karaoke"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-karaoke-dialog)
       (if (not (Widget? karaoke-dialog))
           (let ((sliders '()))
             (set! karaoke-dialog
                   (make-effect-dialog "karaoke ladspa plugin"
                                       (lambda (w context info) (cp-karaoke))
                                       (lambda (w context info) (XtUnmanageChild karaoke-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Karaoke"
                                                      "Attempts to strip the vocals from a stereo signal.\n\ Vocal volume (dB): Controls the attenuation of the vocal (center channel) in decibels. The greater the attenuation the greater the loss of the stereo field."))
                                       (lambda (w c i)
                                         (set! karaoke-vocal-volume 0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* karaoke-vocal-volume 1)))))))
             (set! sliders
                   (add-sliders karaoke-dialog
                                (list (list "vocal volume (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! karaoke-vocal-volume (/ (.value info) 1)))
                                            1))))))
       (activate-dialog karaoke-dialog))))

      (let ((child (XtCreateManagedWidget "Karaoke" xmPushButtonWidgetClass ladspa-filter-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-karaoke-dialog))))



;;; State variable filter
;;;

(define svf-type 0)
(define svf-frequency 100)
(define svf-q 0.0)
(define svf-resonance 0.0)
(define svf-dialog #f)
(define svf-label "State variable filter")

(define (cp-svf)
 (apply-ladspa (make-sample-reader (cursor))
   (list "svf_1214" "svf" svf-type svf-frequency svf-q svf-resonance)
     (- (frames) (cursor))
     "state variable filter"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-svf-dialog)
       (if (not (Widget? svf-dialog))
           (let ((sliders '()))
             (set! svf-dialog
                   (make-effect-dialog "state variable filter ladspa plugin"
                                       (lambda (w context info) (cp-svf))
                                       (lambda (w context info) (XtUnmanageChild svf-dialog))
                                       (lambda (w context info)
                                         (help-dialog "State variable filter"
                                                      "An oversampled state variable filter with a few tweaks. Quite nice, tends to be unstable with high resonance and Q values, but good when kept under control.\n\ Filter type: (0=none, 1=LP, 2=HP, 3=BP, 4=BR, 5=AP) Select between no filtering, low-pass, high-pass, band-pass, band-reject and all-pass.\n\ Filter frequency: Cutoff frequency, beware of high values with low sample rates.\n\ Filter Q: The filter's cutoff slope.\n\ Filter resonance: The filter's resonance is sort of separate from Q but very related (implemented with feedback). Do not use with the bandpass mode."))
                                       (lambda (w c i)
                                         (set! svf-type 0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* svf-type 1))))
                                         (set! svf-frequency 100)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* svf-frequency 1))))
                                         (set! svf-q 0.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* svf-q 100))))
                                         (set! svf-resonance 0.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* svf-resonance 100)))))))
             (set! sliders
                   (add-sliders svf-dialog
                                (list
                                      (list "filter type" 0 0 5
                                            (lambda (w context info)
                                              (set! svf-type (/ (.value info) 1)))
                                            1)
                                      (list "filter frequency" 0 100 6000
                                            (lambda (w context info)
                                              (set! svf-frequency (/ (.value info) 1)))
                                            1)
                                      (list "filter Q" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! svf-q (/ (.value info) 100)))
                                            100)
                                      (list "filter resonance" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! svf-resonance (/ (.value info) 100)))
                                            100))))))
       (activate-dialog svf-dialog))))

      (let ((child (XtCreateManagedWidget "State variable filter" xmPushButtonWidgetClass ladspa-filter-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-svf-dialog))))


;;; VCF303 filter
;;;

(define vcf-303-trigger 0)
(define vcf-303-cutoff .5)
(define vcf-303-resonance .5)
(define vcf-303-env-mod .5)
(define vcf-303-decay .5)
(define vcf-303-dialog #f)
(define vcf-303-label "VCF303 filter")

(define (cp-vcf-303)
  (apply-ladspa (make-sample-reader (cursor))
                (list "cmt" "vcf303" vcf-303-trigger vcf-303-cutoff vcf-303-resonance vcf-303-env-mod vcf-303-decay)
                (- (frames) (cursor))
                "vcf-303"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-vcf-303-dialog)
       (if (not (Widget? vcf-303-dialog))
           (let ((sliders '()))
             (set! vcf-303-dialog
                   (make-effect-dialog "vcf-303 ladspa plugin"
                                       (lambda (w context info) (cp-vcf-303))
                                       (lambda (w context info) (XtUnmanageChild vcf-303-dialog))
                                       (lambda (w context info)
                                         (help-dialog "VCF-303"
                                                      "Emulates the voltage controlled resonant filter of the bass synthesizer of Roland's TB-303.\n\ Move the sliders to set the filter parameters."))
                                       (lambda (w c i)
                                         (set! vcf-303-cutoff .5)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* vcf-303-cutoff 100))))
                                         (set! vcf-303-resonance .5)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* vcf-303-resonance 100))))
                                         (set! vcf-303-env-mod .5)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* vcf-303-env-mod 100))))
                                         (set! vcf-303-decay .5)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* vcf-303-decay 100)))))))
             (set! sliders
                   (add-sliders vcf-303-dialog
                                (list (list "cutoff" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-cutoff (/ (.value info) 100)))
                                            100)
                                      (list "resonance" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-resonance (/ (.value info) 100)))
                                            100)
 				      (list "envelope modulation" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-env-mod (/ (.value info) 100)))
                                            100)
                                      (list "decay" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-decay (/ (.value info) 100)))
                                            100))))
             (let* ((s1 (XmStringCreateLocalized "Trigger"))
                     (toggle
                      (XtCreateManagedWidget "Trigger" xmToggleButtonWidgetClass (XtParent (car sliders))
                        (list XmNselectColor  (pushed-button-color)
                              XmNbackground   (basic-color)
                              XmNvalue        vcf-303-trigger
                              ;XmNborderWidth  1
                              XmNlabelString  s1))))
                (XmStringFree s1)
                (XtAddCallback toggle XmNvalueChangedCallback (lambda (w c i) (set! vcf-303-trigger (if (.set i) 1 0)))))))

       (activate-dialog vcf-303-dialog))))

      (let ((child (XtCreateManagedWidget "VCF303" xmPushButtonWidgetClass ladspa-filter-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-vcf-303-dialog))))

;(add-to-menu ladspa-filter-menu #f #f)

;;; Multiband equalizer 
;;;

(define multiband-eq-50hz-gain 0)
(define multiband-eq-100hz-gain 0)
(define multiband-eq-156hz-gain 0)
(define multiband-eq-220hz-gain 0)
(define multiband-eq-311hz-gain 0)
(define multiband-eq-440hz-gain 0)
(define multiband-eq-622hz-gain 0)
(define multiband-eq-880hz-gain 0)
(define multiband-eq-1250hz-gain 0)
(define multiband-eq-1750hz-gain 0)
(define multiband-eq-2500hz-gain 0)
(define multiband-eq-3500hz-gain 0)
(define multiband-eq-5000hz-gain 0)
(define multiband-eq-10000hz-gain 0)
(define multiband-eq-20000hz-gain 0)
(define multiband-eq-dialog #f)
(define multiband-eq-label "Multiband equalizer")

(define (cp-mbeq)
   (apply-ladspa (make-sample-reader (cursor))
      (list "mbeq_1197" "mbeq" multiband-eq-50hz-gain multiband-eq-100hz-gain multiband-eq-156hz-gain multiband-eq-220hz-gain multiband-eq-311hz-gain multiband-eq-440hz-gain multiband-eq-622hz-gain multiband-eq-880hz-gain multiband-eq-1250hz-gain multiband-eq-1750hz-gain multiband-eq-2500hz-gain multiband-eq-3500hz-gain multiband-eq-5000hz-gain multiband-eq-10000hz-gain multiband-eq-20000hz-gain)
         (- (frames) (cursor))
         "multiband eq"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-multiband-eq-dialog)
       (if (not (Widget? multiband-eq-dialog))
           (let ((sliders '()))
             (set! multiband-eq-dialog
                   (make-effect-dialog "multiband eq ladspa plugin"
                                       (lambda (w context info) (cp-mbeq))
                                       (lambda (w context info) (XtUnmanageChild multiband-eq-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Multiband equalizer"
                                                      "This is a fairly typical multiband graphic equalizer. It's implemented using a FFT, so it takes quite a lot of CPU power, but it should have less phase effect than an equivalent filter implementation. If the input signal is at too low a sample rate then the top bands will be ignored, i.e., the highest useful band will always be a high shelf."))
                                       (lambda (w c i)
                                         (set! multiband-eq-50hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-50hz-gain 1))))
                                         (set! multiband-eq-100hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-100hz-gain 1))))
                                         (set! multiband-eq-156hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-156hz-gain 1))))
                                         (set! multiband-eq-220hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-220hz-gain 1))))
                                         (set! multiband-eq-311hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-311hz-gain 1))))
                                         (set! multiband-eq-440hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-440hz-gain 1))))
                                         (set! multiband-eq-622hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-622hz-gain 1))))
                                         (set! multiband-eq-880hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-880hz-gain 1))))
                                         (set! multiband-eq-1250hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-1250hz-gain 1))))
                                         (set! multiband-eq-1750hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-1750hz-gain 1))))
                                         (set! multiband-eq-2500hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-2500hz-gain 1))))
                                         (set! multiband-eq-3500hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-3500hz-gain 1))))
                                         (set! multiband-eq-5000hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-5000hz-gain 1))))
                                         (set! multiband-eq-10000hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-10000hz-gain 1))))
                                         (set! multiband-eq-20000hz-gain 0)
                                         (XtSetValues (list-ref sliders ) (list XmNvalue (inexact->exact (* multiband-eq-20000hz-gain 1)))))))
             (set! sliders
                   (add-sliders multiband-eq-dialog
                                (list 
				      (list "50 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-50hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "100 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-100hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "156 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-156hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "220 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-220hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "311 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-311hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "440 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-440hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "622 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-622hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "880 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-880hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "1250 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-1250hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "1750 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-1750hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "2500 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-2500hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "3500 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-3500hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "5000 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-5000hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "10000 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-10000hz-gain (/ (.value info) 1)))
                                            1)
                                      (list "20000 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-20000hz-gain (/ (.value info) 1)))
                                            1))))))
       (activate-dialog multiband-eq-dialog))))

      (let ((child (XtCreateManagedWidget "Multiband equalizer" xmPushButtonWidgetClass ladspa-filter-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-multiband-eq-dialog))))


;;; DYNAMICS PROCESSORS
;;;

(define ladspa-amp-menu (XmCreatePulldownMenu plug-menu "Dynamics Processors"
                                        (list XmNbackground (basic-color))))
(define ladspa-amp-cascade (XtCreateManagedWidget "Dynamics Processors" xmCascadeButtonWidgetClass plug-menu
                                            (list XmNsubMenuId ladspa-amp-menu
                                                  XmNbackground (basic-color))))

;;; Compressor (peak tracking)
;;;

(define compress-peak-threshold 0.0)
(define compress-peak-ratio 1.0)
(define compress-peak-envelope-attack 1.0)
(define compress-peak-envelope-decay 1.0)
(define compress-peak-dialog #f)
(define compress-peak-label "Compressor (peak tracking)")

(define (cp-compress-peak)
 (apply-ladspa (make-sample-reader (cursor))
   (list "cmt" "compress_peak" compress-peak-threshold compress-peak-ratio compress-peak-envelope-attack compress-peak-envelope-decay)
     (- (frames) (cursor))
     "compressor (peak)"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-compress-peak-dialog)
       (if (not (Widget? compress-peak-dialog))
           (let ((sliders '()))
             (set! compress-peak-dialog
                   (make-effect-dialog "compressor (peak) ladspa plugin"
                                       (lambda (w context info) (cp-compress-peak))
                                       (lambda (w context info) (XtUnmanageChild compress-peak-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Compressor (peak tracking)"
                                                      "Move the sliders to set the compressor parameters."))
                                       (lambda (w c i)
                                         (set! compress-peak-threshold 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* compress-peak-threshold 100))))
                                         (set! compress-peak-ratio 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* compress-peak-ratio 100))))
                                         (set! compress-peak-envelope-attack 1.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* compress-peak-envelope-attack 100))))
                                         (set! compress-peak-envelope-decay 1.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* compress-peak-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders compress-peak-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! compress-peak-threshold (/ (.value info) 100)))
                                            100)
 				      (list "compression ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! compress-peak-ratio (/ (.value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-peak-envelope-attack (/ (.value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-peak-envelope-decay (/ (.value info) 100)))
                                            100))))))
       (activate-dialog compress-peak-dialog))))

      (let ((child (XtCreateManagedWidget "Compressor (peak tracking)" xmPushButtonWidgetClass ladspa-amp-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-compress-peak-dialog))))


;;; Compressor (rms tracking)
;;;

(define compress-rms-threshold 0.0)
(define compress-rms-ratio 1.0)
(define compress-rms-envelope-attack 1.0)
(define compress-rms-envelope-decay 1.0)
(define compress-rms-dialog #f)
(define compress-rms-label "Compressor (rms tracking)")

(define (cp-compress-rms)
 (apply-ladspa (make-sample-reader (cursor))
   (list "cmt" "compress_rms" compress-rms-threshold compress-rms-ratio compress-rms-envelope-attack compress-rms-envelope-decay)
     (- (frames) (cursor))
     "compressor (rms)"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-compress-rms-dialog)
       (if (not (Widget? compress-rms-dialog))
           (let ((sliders '()))
             (set! compress-rms-dialog
                   (make-effect-dialog "compressor (rms) ladspa plugin"
                                       (lambda (w context info) (cp-compress-rms))
                                       (lambda (w context info) (XtUnmanageChild compress-rms-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Compressor (rms tracking)"
                                                      "Move the sliders to set the compressor parameters."))
                                       (lambda (w c i)
                                         (set! compress-rms-threshold 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* compress-rms-threshold 100))))
                                         (set! compress-rms-ratio 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* compress-rms-ratio 100))))
                                         (set! compress-rms-envelope-attack 1.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* compress-rms-envelope-attack 100))))
                                         (set! compress-rms-envelope-decay 1.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* compress-rms-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders compress-rms-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! compress-rms-threshold (/ (.value info) 100)))
                                            100)
 				      (list "compression ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! compress-rms-ratio (/ (.value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-rms-envelope-attack (/ (.value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-rms-envelope-decay (/ (.value info) 100)))
                                            100))))))
       (activate-dialog compress-rms-dialog))))

      (let ((child (XtCreateManagedWidget "Compressor (rms tracking)" xmPushButtonWidgetClass ladspa-amp-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-compress-rms-dialog))))


;;; Dyson compressor 
;;;

(define dyson-compress-peak-limit 0)
(define dyson-compress-release-time 1.0)
(define dyson-compress-fast-compression-ratio 0.0)
(define dyson-compress-compression-ratio 0.0)
(define dyson-compress-dialog #f)
(define dyson-compress-label "Dyson compressor")

(define (cp-dyson-compress)
 (apply-ladspa (make-sample-reader (cursor))
   (list "dyson_compress_1403" "dysonCompress" dyson-compress-peak-limit dyson-compress-release-time dyson-compress-fast-compression-ratio dyson-compress-compression-ratio)
     (- (frames) (cursor))
     "dyson compressor"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-dyson-compress-dialog)
       (if (not (Widget? dyson-compress-dialog))
           (let ((sliders '()))
             (set! dyson-compress-dialog
                   (make-effect-dialog "dyson compressor ladspa plugin"
                                       (lambda (w context info) (cp-dyson-compress))
                                       (lambda (w context info) (XtUnmanageChild dyson-compress-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Dyson compressor"
                                                      "Peak limit (dB) controls the desired limit of the output signal in decibels.\n\ Release time (s) controls the time in seconds taken for the compressor to relax its gain control over the input signal.\n\ (No information is available regarding the significance of the fast and normal compression ratio settings)."))
                                       (lambda (w c i)
                                         (set! dyson-compress-peak-limit 0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* dyson-compress-peak-limit 1))))
                                         (set! dyson-compress-release-time 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* dyson-compress-release-time 100))))
                                         (set! dyson-compress-fast-compression-ratio 0.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* dyson-compress-fast-compression-ratio 100))))
                                         (set! dyson-compress-compression-ratio 0.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* dyson-compress-compression-ratio 100)))))))
             (set! sliders
                   (add-sliders dyson-compress-dialog
                                (list
                                      (list "peak limit (dB)" -30 0 0
                                            (lambda (w context info)
                                              (set! dyson-compress-peak-limit (/ (.value info) 1)))
                                            1)
                                      (list "release time (s)" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! dyson-compress-release-time (/ (.value info) 100)))
                                            100)
                                      (list "fast compression ratio" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! dyson-compress-fast-compression-ratio (/ (.value info) 100)))
                                            100)
                                      (list "compression ratio" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! dyson-compress-compression-ratio (/ (.value info) 100)))
                                            100))))))
       (activate-dialog dyson-compress-dialog))))

      (let ((child (XtCreateManagedWidget "Dyson compressor" xmPushButtonWidgetClass ladspa-amp-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-dyson-compress-dialog))))






;;; Expander (peak tracking)
;;;

(define expand-peak-threshold 0.0)
(define expand-peak-ratio 1.0)
(define expand-peak-envelope-attack 1.0)
(define expand-peak-envelope-decay 1.0)
(define expand-peak-dialog #f)
(define expand-peak-label "Expander (peak tracking)")

(define (cp-expand-peak)
 (apply-ladspa (make-sample-reader (cursor))
   (list "cmt" "expand_peak" expand-peak-threshold expand-peak-ratio expand-peak-envelope-attack expand-peak-envelope-decay)
     (- (frames) (cursor))
     "expander (peak)"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-expand-peak-dialog)
       (if (not (Widget? expand-peak-dialog))
           (let ((sliders '()))
             (set! expand-peak-dialog
                   (make-effect-dialog "expander (peak) ladspa plugin"
                                       (lambda (w context info) (cp-expand-peak))
                                       (lambda (w context info) (XtUnmanageChild expand-peak-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Expander (peak tracking)"
                                                      "Move the sliders to set the expander parameters."))
                                       (lambda (w c i)
                                         (set! expand-peak-threshold 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* expand-peak-threshold 100))))
                                         (set! expand-peak-ratio 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* expand-peak-ratio 100))))
                                         (set! expand-peak-envelope-attack 1.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* expand-peak-envelope-attack 100))))
                                         (set! expand-peak-envelope-decay 1.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* expand-peak-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders expand-peak-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! expand-peak-threshold (/ (.value info) 100)))
                                            100)
 				      (list "expansion ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! expand-peak-ratio (/ (.value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-peak-envelope-attack (/ (.value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-peak-envelope-decay (/ (.value info) 100)))
                                            100))))))
       (activate-dialog expand-peak-dialog))))

      (let ((child (XtCreateManagedWidget "Expander (peak tracking)" xmPushButtonWidgetClass ladspa-amp-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-expand-peak-dialog))))


;;; Expander (rms tracking)
;;;

(define expand-rms-threshold 0.0)
(define expand-rms-ratio 1.0)
(define expand-rms-envelope-attack 1.0)
(define expand-rms-envelope-decay 1.0)
(define expand-rms-dialog #f)
(define expand-rms-label "Expander (rms tracking)")

(define (cp-expand-rms)
 (apply-ladspa (make-sample-reader (cursor))
   (list "cmt" "expand_rms" expand-rms-threshold expand-rms-ratio expand-rms-envelope-attack expand-rms-envelope-decay)
     (- (frames) (cursor))
     "expander (rms)"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-expand-rms-dialog)
       (if (not (Widget? expand-rms-dialog))
           (let ((sliders '()))
             (set! expand-rms-dialog
                   (make-effect-dialog "expander (rms) ladspa plugin"
                                       (lambda (w context info) (cp-expand-rms))
                                       (lambda (w context info) (XtUnmanageChild expand-rms-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Expander (rms tracking)"
                                                      "Move the sliders to set the expander parameters."))
                                       (lambda (w c i)
                                         (set! expand-rms-threshold 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* expand-rms-threshold 100))))
                                         (set! expand-rms-ratio 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* expand-rms-ratio 100))))
                                         (set! expand-rms-envelope-attack 1.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* expand-rms-envelope-attack 100))))
                                         (set! expand-rms-envelope-decay 1.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* expand-rms-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders expand-rms-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! expand-rms-threshold (/ (.value info) 100)))
                                            100)
 				      (list "expansion ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! expand-rms-ratio (/ (.value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-rms-envelope-attack (/ (.value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-rms-envelope-decay (/ (.value info) 100)))
                                            100))))))
       (activate-dialog expand-rms-dialog))))

      (let ((child (XtCreateManagedWidget "Expander (rms tracking)" xmPushButtonWidgetClass ladspa-amp-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-expand-rms-dialog))))


;;; Limiter (peak tracking)
;;;

(define limit-peak-threshold 0.0)
(define limit-peak-envelope-attack 1.0)
(define limit-peak-envelope-decay 1.0)
(define limit-peak-dialog #f)
(define limit-peak-label "Limiter (peak tracking)")

(define (cp-limit-peak)
 (apply-ladspa (make-sample-reader (cursor))
   (list "cmt" "limit_peak" limit-peak-threshold limit-peak-envelope-attack limit-peak-envelope-decay)
     (- (frames) (cursor))
     "limiter (peak)"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-limit-peak-dialog)
       (if (not (Widget? limit-peak-dialog))
           (let ((sliders '()))
             (set! limit-peak-dialog
                   (make-effect-dialog "limiter (peak) ladspa plugin"
                                       (lambda (w context info) (cp-limit-peak))
                                       (lambda (w context info) (XtUnmanageChild limit-peak-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Limiter (peak tracking)"
                                                      "Move the sliders to set the limiter parameters."))
                                       (lambda (w c i)
                                         (set! limit-peak-threshold 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* limit-peak-threshold 100))))
                                         (set! limit-peak-envelope-attack 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* limit-peak-envelope-attack 100))))
                                         (set! limit-peak-envelope-decay 1.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* limit-peak-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders limit-peak-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! limit-peak-threshold (/ (.value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-peak-envelope-attack (/ (.value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-peak-envelope-decay (/ (.value info) 100)))
                                            100))))))
       (activate-dialog limit-peak-dialog))))

      (let ((child (XtCreateManagedWidget "Limiter (peak tracking)" xmPushButtonWidgetClass ladspa-amp-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-limit-peak-dialog))))


;;; Limiter (rms tracking)
;;;

(define limit-rms-threshold 0.0)
(define limit-rms-envelope-attack 1.0)
(define limit-rms-envelope-decay 1.0)
(define limit-rms-dialog #f)
(define limit-rms-label "Limiter (rms tracking)")

(define (cp-limit-rms)
 (apply-ladspa (make-sample-reader (cursor))
   (list "cmt" "limit_rms" limit-rms-threshold limit-rms-envelope-attack limit-rms-envelope-decay)
     (- (frames) (cursor))
     "limiter (rms)"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

      (define (post-limit-rms-dialog)
       (if (not (Widget? limit-rms-dialog))
           (let ((sliders '()))
             (set! limit-rms-dialog
                   (make-effect-dialog "limiter (rms) ladspa plugin"
                                       (lambda (w context info) (cp-limit-rms))
                                       (lambda (w context info) (XtUnmanageChild limit-rms-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Limiter (rms tracking)"
                                                      "Move the sliders to set the limiter parameters."))
                                       (lambda (w c i)
                                         (set! limit-rms-threshold 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* limit-rms-threshold 100))))
                                         (set! limit-rms-envelope-attack 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* limit-rms-envelope-attack 100))))
                                         (set! limit-rms-envelope-decay 1.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* limit-rms-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders limit-rms-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! limit-rms-threshold (/ (.value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-rms-envelope-attack (/ (.value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-rms-envelope-decay (/ (.value info) 100)))
                                            100))))))
       (activate-dialog limit-rms-dialog))))

      (let ((child (XtCreateManagedWidget "Limiter (rms tracking)" xmPushButtonWidgetClass ladspa-amp-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-limit-rms-dialog))))





;;; FREQUENCY EFFECTS
;;;

(define ladspa-freq-menu (XmCreatePulldownMenu plug-menu "Frequency Effects"
                                        (list XmNbackground (basic-color))))
(define ladspa-freq-cascade (XtCreateManagedWidget "Frequency Effects" xmCascadeButtonWidgetClass plug-menu
                                            (list XmNsubMenuId ladspa-freq-menu
                                                  XmNbackground (basic-color))))


;;; LADSPA AM Pitchshifter 
;;;

(define am-pitchshifter-pitch-shift 1.0)
(define am-pitchshifter-buffer-size 4)
(define am-pitchshifter-dialog #f)
(define am-pitchshifter-label "AM Pitchshifter")
(define am-pitchshifter-target 'sound)
(define am-pitchshifter-menu-widget #f)

(define (post-am-pitchshifter-dialog)
  (if (not am-pitchshifter-dialog)
      ;; if am-pitchshifter-dialog doesn't exist, create it
      (let ((initial-am-pitchshifter-pitch-shift 1.0)
            (initial-am-pitchshifter-buffer-size 4)
            (sliders '()))
        (set! am-pitchshifter-dialog
              (make-effect-dialog
               am-pitchshifter-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "am_pitchshift_1433" "amPitchshift" am-pitchshifter-pitch-shift am-pitchshifter-buffer-size)
                  am-pitchshifter-target
                  "AM Pitchshifter" #f))
               (lambda (w context info) (XtUnmanageChild am-pitchshifter-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "AM Pitchshifter" "This plugin works by running a single write pointer (monotonic) and two read pointers (pitchscaled) over a ringbuffer. The output is faded between the two read pointers according to the sine of the distance from the write pointer. The design is based on the mechanism of a mechanical pitchshifter I saw in the Gemeentemuseum in Den Haag, though I'm sure it is a common enough algorithm.\n\ Pitch shift: The multiple of the output pitch. Thus, a value of 2.0 will increase the pitch by one octave.\n\ Buffer size: The order of magnitude of the buffer size. Small buffers will sound fluttery, large buffers will have flangey sounding echoes. I recommend a buffer size of 3 for a reasonable compromise, with wideband material at around 48KHz. For drums you might have to lower it, and for voiced background noises it can go higher."))

               (lambda (w c i)
                 (set! am-pitchshifter-pitch-shift initial-am-pitchshifter-pitch-shift)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* am-pitchshifter-pitch-shift 100))))
                 (set! am-pitchshifter-buffer-size initial-am-pitchshifter-buffer-size)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* am-pitchshifter-buffer-size 1)))))))
        (set! sliders
              (add-sliders am-pitchshifter-dialog
                           (list (list "Pitch shift" 0.25 initial-am-pitchshifter-pitch-shift 4.0
                                       (lambda (w context info)
                                         (set! am-pitchshifter-pitch-shift (/ (.value info) 100)))
                                       100)
                                 (list "Buffer size" 1 initial-am-pitchshifter-buffer-size 7
                                       (lambda (w context info)
                                         (set! am-pitchshifter-buffer-size (/ (.value info) 1)))
                                       1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! am-pitchshifter-target target)) #f)))

  (activate-dialog am-pitchshifter-dialog))

(let ((child (XtCreateManagedWidget "AM Pitchshifter" xmPushButtonWidgetClass ladspa-freq-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-am-pitchshifter-dialog)))
  (set! ladspa-freq-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "AM Pitchshifter (~1,2F ~1,2D)" am-pitchshifter-pitch-shift am-pitchshifter-buffer-size)))
                  (change-label child new-label)))
              ladspa-freq-menu-list)))




;;; LADSPA Harmonic generator
;;;

(define hg-fundamental-magnitude 0.0)
(define hg-harmonic-magnitude-2 0.0)
(define hg-harmonic-magnitude-3 0.0)
(define hg-harmonic-magnitude-4 0.0)
(define hg-harmonic-magnitude-5 0.0)
(define hg-harmonic-magnitude-6 0.0)
(define hg-harmonic-magnitude-7 0.0)
(define hg-harmonic-magnitude-8 0.0)
(define hg-harmonic-magnitude-9 0.0)
(define hg-harmonic-magnitude-10 0.0)
(define hg-dialog #f)
(define hg-label "Harmonic generator")
(define hg-target 'sound)
(define hg-menu-widget #f)

(define (post-hg-dialog)
  (if (not hg-dialog)
      ;; if hg-dialog doesn't exist, create it
      (let ((sliders '()))
        (set! hg-dialog
              (make-effect-dialog
               hg-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "harmonic_gen_1220" "harmonicGen"  hg-fundamental-magnitude hg-harmonic-magnitude-2 hg-harmonic-magnitude-3 hg-harmonic-magnitude-4 hg-harmonic-magnitude-5 hg-harmonic-magnitude-6 hg-harmonic-magnitude-7 hg-harmonic-magnitude-8 hg-harmonic-magnitude-9 hg-harmonic-magnitude-10)
                  hg-target
                  "harmonic generator" #f))
               (lambda (w context info) (XtUnmanageChild hg-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Harmonic generator" "Allows you to add harmonics and remove the fundamental from any audio signal.\n\ Known bugs: There is no bandwidth limiting filter on the output, so it is easy to create excessive high-frequency harmonics that could cause aliasing problems. However, in practice this doesn't seem to be a serious problem.\n\ Examples: There are many interesting effects you can achieve with sine waves. One example is producing band-limited square waves from sine waves. To do this, set the parameters to 1, 0, -0.3333, 0, 0.2, 0, -0.14285, 0, 0.11111. To get a triangle-like signal use 1, 0, -0.3333, 0, -0.2, 0, -0.14285, 0, -0.11111.\n\ Fundamental magnitude: The amplitude of the fundamental of the signal. Reduce it to 0 to remove the base signal altogether, or -1 to phase-invert it.\n\ 2nd harmonic magnitude: The 2nd harmonic, its frequency is twice the frequency of the fundamental.\n\ 3rd harmonic magnitude: The 3rd harmonic, its frequency is three times the frequency of the fundamental.\n\ Even harmonics add a distorted feel to the sound. Valve (tube) amplifiers introduce distortions at all the harmonics, transistor amplifiers only introduce distortion into the odd harmonics."))

                (lambda (w c i)
                                         (set! hg-fundamental-magnitude 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* hg-fundamental-magnitude 100))))
                                         (set! hg-harmonic-magnitude-2 0.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-2 100))))
                                         (set! hg-harmonic-magnitude-3 0.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-3 100))))
                                         (set! hg-harmonic-magnitude-4 0.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-4 100))))
                                         (set! hg-harmonic-magnitude-5 0.0)
                                         (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-5 100))))
                                         (set! hg-harmonic-magnitude-6 0.0)
                                         (XtSetValues (list-ref sliders 5) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-6 100))))
                                         (set! hg-harmonic-magnitude-7 0.0)
                                         (XtSetValues (list-ref sliders 6) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-7 100))))
                                         (set! hg-harmonic-magnitude-8 0.0)
                                         (XtSetValues (list-ref sliders 7) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-8 100))))
                                         (set! hg-harmonic-magnitude-9 0.0)
                                         (XtSetValues (list-ref sliders 8) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-9 100))))
                                         (set! hg-harmonic-magnitude-10 0.0)
                                         (XtSetValues (list-ref sliders 9) (list XmNvalue (inexact->exact (* hg-harmonic-magnitude-10 100)))))))
             
             (set! sliders
              (add-sliders hg-dialog
                                (list 
                                      (list "fundamental magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-fundamental-magnitude (/ (.value info) 100)))
                                            100)
                                      (list "2nd harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-2 (/ (.value info) 100)))
                                            100)
                                      (list "3rd harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-3 (/ (.value info) 100)))
                                            100)
                                      (list "4th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-4 (/ (.value info) 100)))
                                            100)
                                      (list "5th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-5 (/ (.value info) 100)))
                                            100)
                                      (list "6th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-6 (/ (.value info) 100)))
                                            100)
                                      (list "7th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-7 (/ (.value info) 100)))
                                            100)
                                      (list "8th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-8 (/ (.value info) 100)))
                                            100)
                                      (list "9th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-9 (/ (.value info) 100)))
                                            100)
                                      (list "10th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-10 (/ (.value info) 100)))
                                            100))))

        (add-target (XtParent (car sliders))
                    (lambda (target) (set! hg-target target)) #f)))

  (activate-dialog hg-dialog))

(let ((child (XtCreateManagedWidget "Harmonic generator" xmPushButtonWidgetClass ladspa-freq-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-hg-dialog)))
  (set! ladspa-freq-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Harmonic generator (~1,2F ~1,2F ~1,2F ~1,2F ~1,2F ~1,2F ~1,2F ~1,2F ~1,2F ~1,2F)" hg-fundamental-magnitude hg-harmonic-magnitude-2 hg-harmonic-magnitude-3 hg-harmonic-magnitude-4 hg-harmonic-magnitude-5 hg-harmonic-magnitude-6 hg-harmonic-magnitude-7 hg-harmonic-magnitude-8 hg-harmonic-magnitude-9 hg-harmonic-magnitude-10)))
                  (change-label child new-label)))
              ladspa-freq-menu-list)))



;;; LADSPA Pitch scaler 
;;;

(define pitch-scaler-coefficient 1.0)
(define pitch-scaler-dialog #f)
(define pitch-scaler-label "Pitch scaler")
(define pitch-scaler-target 'sound)
(define pitch-scaler-menu-widget #f)

(define (post-pitch-scaler-dialog)
  (if (not pitch-scaler-dialog)
      ;; if pitch-scaler-dialog doesn't exist, create it
      (let ((initial-pitch-scaler-coefficient 1.0)
            (sliders '()))
        (set! pitch-scaler-dialog
              (make-effect-dialog
               pitch-scaler-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "pitch_scale_1194" "pitchScaleHQ" pitch-scaler-coefficient)
                  pitch-scaler-target
                  "Pitch scaler"#f))
               (lambda (w context info) (XtUnmanageChild pitch-scaler-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Pitch scaler" "A pitch shifter implementation that scales the harmonics appropriately with the base frequencies.\n\ It is an implementation of Stephen M. Sprenger's pitch scaler design. It gives reasonable, general purpose results for small changes, but won't give Antares or Eventide anything to worry about.\n\ The FFT block size and oversampling has been kept at reasonable levels to keep the CPU usage low.\n\ Pitch coefficient: The pitch scaling factor. A value of 2.0 will increase the pitch by one octave, .50 will lower it by one octave, etc."))

               (lambda (w c i)
                 (set! pitch-scaler-coefficient initial-pitch-scaler-coefficient)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* pitch-scaler-coefficient 100)))))))
        (set! sliders
              (add-sliders pitch-scaler-dialog
                           (list (list "coefficient" 0.5 initial-pitch-scaler-coefficient 2.0
                                       (lambda (w context info)
                                         (set! pitch-scaler-coefficient (/ (.value info) 100)))
                                       100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! pitch-scaler-target target)) #f)))

  (activate-dialog pitch-scaler-dialog))

(let ((child (XtCreateManagedWidget "Pitch scaler" xmPushButtonWidgetClass ladspa-freq-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-pitch-scaler-dialog)))
  (set! ladspa-freq-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Pitch scaler (~1,2F)" pitch-scaler-coefficient)))
                  (change-label child new-label)))
              ladspa-freq-menu-list)))


;;; Pitch scaler
;;; by semitones

;(define pitch-coefficient 1.0)
;(define pitch-scale-dialog #f)
;(define pitch-scale-label "Pitch scaler")
;
;(define (cp-pitch-scale)
;  (apply-ladspa (make-sample-reader (cursor))
;                (list "pitch_scale_1194" "pitchScaleHQ" pitch-coefficient)
;                (- (frames) (cursor))
;                "pitch-scale"))
;
;(if (and (provided? 'snd-ladspa)
;         (provided? 'xm))
;  (begin
;
;     (define (post-pitch-scale-dialog)
;       (if (not (Widget? pitch-scale-dialog))
;           ;; if pitch-scale-dialog doesn't exist, create it
;           (let ((sliders '()))
;             (set! pitch-scale-dialog
;                   (make-effect-dialog "pitch-scale (high-quality) ladspa plugin"
;                                       (lambda (w context info) (cp-pitch-scale))
;                                       (lambda (w context info) (XtUnmanageChild pitch-scale-dialog))
;                                       (lambda (w context info)
;                                         (help-dialog "Pitch scaler"
;                                                      "A pitch shifter implementation that scales the harmonics appropriately with the base frequencies.\n\ It is an implementation of Stephen M. Sprengler's pitch scaler design. It gives reasonable, general purpose results for small changes, but won't give Antares or Eventide anything to worry about.\n\ The FFT block size and oversampling has been kept at reasonable levels to keep the CPU usage low.\n\ Pitch coefficient: The pitch scaling factor, a value of 2.0 will increase the pitch by one octave, etc."))
;                                       (lambda (w c i)
;                                         (set! pitch-coefficient 1.0)
;                                         (XtSetValues (car sliders)
;                                            (list XmNvalue semi-range)))))
;             (set! sliders
;                   (add-sliders pitch-scale-dialog
;                                (list (list "pitch coefficient" 0.25 1.0 4.0
;                                            (lambda (w context info)
;                                              (set! pitch-coefficient (semitones->ratio (- (.value info) semi-range))))
;                                            100 'semi))))))
;       (activate-dialog pitch-scale-dialog))))
;
;      (let ((child (XtCreateManagedWidget "Pitch scaler" xmPushButtonWidgetClass ladspa-freq-menu
;                                           (list XmNbackground (basic-color)))))
;        (XtAddCallback child XmNactivateCallback
;                        (lambda (w c i)
;                          (post-pitch-scale-dialog))))

;;; MODULATION EFFECTS
;;;

(define ladspa-mod-menu (XmCreatePulldownMenu plug-menu "Modulation Effects"
                                        (list XmNbackground (basic-color))))
(define ladspa-mod-cascade (XtCreateManagedWidget "Modulation Effects" xmCascadeButtonWidgetClass plug-menu
                                            (list XmNsubMenuId ladspa-mod-menu
                                                  XmNbackground (basic-color))))


;;; Chorus
;;;

(define mvChorus-voices 3)
(define mvChorus-delay 10)
(define mvChorus-separation 1)
(define mvChorus-detune 0)
(define mvChorus-lfo 10)
(define mvChorus-attenuation 0)
(define mvChorus-dialog #f)
(define mvChorus-label "Chorus (multivoice)")

(define (cp-mvChorus)
  (apply-ladspa (make-sample-reader (cursor))
                (list "multivoice_chorus_1201" "multivoiceChorus" mvChorus-voices mvChorus-delay mvChorus-separation mvChorus-detune mvChorus-lfo mvChorus-attenuation)
                (- (frames) (cursor))
                "multivoice chorus"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-mvChorus-dialog)
       (if (not (Widget? mvChorus-dialog))
           ;; if mvChorus-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! mvChorus-dialog
                   (make-effect-dialog "multivoice chorus ladspa plugin"
                                       (lambda (w context info) (cp-mvChorus))
                                       (lambda (w context info) (XtUnmanageChild mvChorus-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Multivoice chorus"
                                                      "This is an implementation of a Multivoice (as opposed to Multiscale) chorus algorithm. It uses a novel sinc-based noise interpolation method to produce a subtle modulation law, which makes it possible to get away with larger numbers of voices without the metallic, artificial sound common in chorus effects.\n\ Voice separation (ms): The individual voices can either be running at the same base delay (set this to zero) or staggered. Setting this to non-zero values can make the output sound richer, but will make it sound grainy with some type of signals.\n\ Detune (%): The maximum amount that a voice will be detuned by. A value of 1 is recommended, but you may be able to get away with higher values if the signal is less harmonic.\n\ LFO frequency (Hz): The frequency that the detune effect will be modulated at. A matter of taste, for most types of input a lower value will be more subtle.\n\ Output attenuation (dB): With large numbers of voices the output can become too high, so use this to trim the amplitude to a more helpful level."))
                                       (lambda (w c i)
                                         (set! mvChorus-voices 3)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* mvChorus-voices 1))))
                                         (set! mvChorus-delay 10)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* mvChorus-delay 1))))
                                         (set! mvChorus-separation 1)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* mvChorus-separation 10))))
                                         (set! mvChorus-detune 0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* mvChorus-detune 1))))
                                         (set! mvChorus-lfo 10)
                                         (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* mvChorus-lfo 1))))
         				 (set! mvChorus-attenuation 0)
				         (XtSetValues (list-ref sliders 5) (list XmNvalue (inexact->exact (* mvChorus-attenuation 1)))))))+
             (set! sliders
                   (add-sliders mvChorus-dialog
                                (list (list "voices" 1 3 8
                                            (lambda (w context info)
                                              (set! mvChorus-voices (/ (.value info) 1)))
                                            1)
                                      (list "delay time (ms)" 10 10 40
                                            (lambda (w context info)
                                              (set! mvChorus-delay (/ (.value info) 1)))
                                            1)
 				      (list "voice separation (ms)" 0 1 2
                                            (lambda (w context info)
                                              (set! mvChorus-separation (/ (.value info) 10)))
                                            10)
                                      (list "detune (%)" 0 0 5
                                            (lambda (w context info)
                                              (set! mvChorus-detune (/ (.value info) 1)))
                                            1)
                                      (list "LFO frequency (Hz)" 2 10 30
                                            (lambda (w context info)
                                              (set! mvChorus-lfo (/ (.value info) 1)))
                                            1)
                                      (list "output attenuation (db)" -20 0 0
                                            (lambda (w context info)
                                              (set! mvChorus-attenuation (/ (.value info) 1)))
                                            1))))))
       (activate-dialog mvChorus-dialog))))

      (let ((child (XtCreateManagedWidget "Chorus (multivoice)" xmPushButtonWidgetClass ladspa-mod-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-mvChorus-dialog))))


;;; LADSPA DJ Flanger
;;;

(define dj-flanger-lfo-sync 0)
(define dj-flanger-lfo-period 1.0)
(define dj-flanger-lfo-depth 4.0)
(define dj-flanger-feedback 0)
(define dj-flanger-dialog #f)
(define dj-flanger-label "DJ Flanger")
(define dj-flanger-target 'sound)
(define dj-flanger-truncate #t)
(define dj-flanger-menu-widget #f)

(define (post-dj-flanger-dialog)
  (if (not dj-flanger-dialog)
      ;; if dj-flanger-dialog doesn't exist, create it
      (let ((initial-dj-flanger-lfo-sync 0)
            (initial-dj-flanger-lfo-period 1.0)
            (initial-dj-flanger-lfo-depth 4.0)
            (initial-dj-flanger-feedback 0)
            (sliders '()))
        (set! dj-flanger-dialog
              (make-effect-dialog
               dj-flanger-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "dj_flanger_1438" "djFlanger" dj-flanger-lfo-sync dj-flanger-lfo-period dj-flanger-lfo-depth dj-flanger-feedback)
                  dj-flanger-target
                  "DJ Flanger"
                  (and dj-flanger-truncate
                       (* 4 dj-flanger-lfo-sync))))
               (lambda (w context info) (XtUnmanageChild dj-flanger-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "DJ Flanger" "This is a flanger which is more or less typical of DJ mixing desks. Requested by Patrick Shirkey.\n\ LFO Sync: When turned from off to on it resets the phase of the LFO back to the start of the cycle. Used to sync the LFO to the track.\n\ LFO Period: The cycle period of the LFO in seconds.\n\ LFO Depth: The maximum delay the LFO will use to flange, in milliseconds.\n\ Feedback: The amount of the delayed output that is mixed back into the delay."))

               (lambda (w c i)
                 (set! dj-flanger-lfo-sync initial-dj-flanger-lfo-sync)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* dj-flanger-lfo-sync 1))))
                 (set! dj-flanger-lfo-period initial-dj-flanger-lfo-period)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* dj-flanger-lfo-period 100))))
                 (set! dj-flanger-lfo-depth initial-dj-flanger-lfo-depth)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* dj-flanger-lfo-depth 100))))   
                 (set! dj-flanger-feedback initial-dj-flanger-feedback)
                 (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* dj-flanger-feedback 1)))))))
        (set! sliders
              (add-sliders dj-flanger-dialog
                           (list (list "LFO sync" -1 initial-dj-flanger-lfo-sync 1
                                       (lambda (w context info)
                                         (set! dj-flanger-lfo-sync (/ (.value info) 1)))
                                       1)
                                 (list "LFO period (s)" 0.1 initial-dj-flanger-lfo-period 32.0
                                       (lambda (w context info)
                                         (set! dj-flanger-lfo-period (/ (.value info) 100.0)))
                                       100)
                                 (list "LFO depth (ms)" 1.0 initial-dj-flanger-lfo-depth 5.0
                                       (lambda (w context info)
                                         (set! dj-flanger-lfo-depth (/ (.value info) 100.0)))
                                       100)   
                                 (list "Feedback (%)" -100 initial-dj-flanger-feedback 100
                                       (lambda (w context info)
                                         (set! dj-flanger-feedback (/ (.value info) 1)))
                                       1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! dj-flanger-target target))
                    (lambda (truncate) (set! dj-flanger-truncate truncate)))))

  (activate-dialog dj-flanger-dialog))

(let ((child (XtCreateManagedWidget "DJ Flanger" xmPushButtonWidgetClass ladspa-mod-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-dj-flanger-dialog)))
  (set! ladspa-mod-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "DJ Flanger (~1,2D ~1,2F ~1,2F ~1,2D)" dj-flanger-lfo-sync dj-flanger-lfo-period dj-flanger-lfo-depth dj-flanger-feedback)))
                  (change-label child new-label)))
              ladspa-mod-menu-list)))


;;; Flanger
;;;

(define sh-flanger-delay-base 1.0)
(define sh-flanger-max-slowdown 1.0)
(define sh-flanger-lfo-frequency 10.0)
(define sh-flanger-feedback 0.0)
(define sh-flanger-dialog #f)
(define sh-flanger-label "Flanger")

(define (cp-flanger)
  (apply-ladspa (make-sample-reader (cursor))
                (list "flanger_1191" "flanger" sh-flanger-delay-base sh-flanger-max-slowdown sh-flanger-lfo-frequency sh-flanger-feedback)
                (- (frames) (cursor))
                "flanger"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-sh-flanger-dialog)
       (if (not (Widget? sh-flanger-dialog))
           (let ((sliders '()))
             (set! sh-flanger-dialog
                   (make-effect-dialog "flanger ladspa plugin"
                                       (lambda (w context info) (cp-flanger))
                                       (lambda (w context info) (XtUnmanageChild sh-flanger-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Flanger"
                                                      "A digital flanger implementation. Uses a novel zero excursion and a controlled bandwidth modulation function that should make the modulation less repetitive and noticeable. This effect is similar in character to a phaser, the main difference is that a phaser sounds more regular and stable.\n\ Delay base (ms): This is the offset from the input time that the detuned delay moves around. 10 is probably a good starting value.\n\ Max slowdown (ms): This is the maximum delay that will be applied to the delayed signal, relative to the dry signal.\n\ LFO frequency (Hz): This is the core frequency that the 'LFO' will move at. The LFO isn't actually an oscillator, but it does change periodically.\n\ Feedback: Feedback applied from the output to the input. Increases the depth of the effect, but makes it sound less like a real flanger."))
                                       (lambda (w c i)
                                         (set! sh-flanger-delay-base 1.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* sh-flanger-delay-base 100))))
                                         (set! sh-flanger-max-slowdown 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* sh-flanger-max-slowdown 100))))
                                         (set! sh-flanger-lfo-frequency 10.0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* sh-flanger-lfo-frequency 100))))
                                         (set! sh-flanger-feedback 0.0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* sh-flanger-feedback 100)))))))
             (set! sliders
                   (add-sliders sh-flanger-dialog
                                (list (list "delay base (ms)" 0.1 1.0 25.0
                                            (lambda (w context info)
                                              (set! sh-flanger-delay-base (/ (.value info) 100)))
                                            100)
                                      (list "max slowdown (ms)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! sh-flanger-max-slowdown (/ (.value info) 100)))
                                            100)
 				      (list "LFO frequency (Hz)" 0.5 10.0 100.0
                                            (lambda (w context info)
                                              (set! sh-flanger-lfo-frequency (/ (.value info) 100)))
                                            100)
                                      (list "feedback" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! sh-flanger-feedback (/ (.value info) 100)))
                                            100))))))
       (activate-dialog sh-flanger-dialog))))

      (let ((child (XtCreateManagedWidget "Flanger" xmPushButtonWidgetClass ladspa-mod-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-sh-flanger-dialog))))

;;; Phaser (LFO)
;;;

(define lfo-phaser-rate 10)
(define lfo-phaser-depth .5)
(define lfo-phaser-feedback 0)
(define lfo-phaser-spread 1)
(define lfo-phaser-dialog #f)
(define lfo-phaser-label "Phaser (LFO)")

(define (cp-lfo-phaser)
 (apply-ladspa (make-sample-reader (cursor))
               (list "phasers_1217" "lfoPhaser" lfo-phaser-rate lfo-phaser-depth lfo-phaser-feedback lfo-phaser-spread)
               (- (frames) (cursor))
               "lfo-phaser"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-lfo-phaser-dialog)
       (if (not (Widget? lfo-phaser-dialog))
           ;; if lfo-phaser-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! lfo-phaser-dialog
                   (make-effect-dialog "lfo-phaser ladspa plugin"
                                       (lambda (w context info) (cp-lfo-phaser))
                                       (lambda (w context info) (XtUnmanageChild lfo-phaser-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Phaser (LFO)"
                                                      "Move sliders to adjust phaser parameters."))
                                       (lambda (w c i)
				         (set! lfo-phaser-rate 10) 
					 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* lfo-phaser-rate 1))))
                                         (set! lfo-phaser-depth .5) 
					 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* lfo-phaser-depth 100))))
                                         (set! lfo-phaser-feedback .5)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* lfo-phaser-feedback 100))))
                                         (set! lfo-phaser-spread 1)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* lfo-phaser-spread 1)))))))
             (set! sliders
                   (add-sliders lfo-phaser-dialog
                                (list 
                                      (list "LFO rate (Hz)" 1 10 100
                                            (lambda (w context info)
                                              (set! lfo-phaser-rate (/ (.value info) 1)))
                                            1)
				      (list "LFO depth" 0.0 .5 1.0
                                            (lambda (w context info)
                                              (set! lfo-phaser-depth (/ (.value info) 100)))
                                            100)
                                      (list "feedback" -1.0 .5 1.0
                                            (lambda (w context info)
                                              (set! lfo-phaser-feedback (/ (.value info) 100)))
                                            100)
                                      (list "spread" 0 1 2
                                            (lambda (w context info)
                                              (set! lfo-phaser-spread (/ (.value info) 1)))
                                            1))))))
       (activate-dialog lfo-phaser-dialog))))

      (let ((child (XtCreateManagedWidget "Phaser (LFO)" xmPushButtonWidgetClass ladspa-mod-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-lfo-phaser-dialog))))


;;; Retro flange
;;;

(define retro-flange-stall 0.0)
(define retro-flange-frequency 1.0)
(define retro-flange-dialog #f)
(define retro-flange-label "Retro flange")

(define (cp-retro-flange)
  (apply-ladspa (make-sample-reader (cursor))
                (list "retro_flange_1208" "retroFlange" retro-flange-stall retro-flange-frequency)
                (- (frames) (cursor))
                "retro flanger"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-retro-flange-dialog)
       (if (not (Widget? retro-flange-dialog))
           (let ((sliders '()))
             (set! retro-flange-dialog
                   (make-effect-dialog "retro flanger ladspa plugin"
                                       (lambda (w context info) (cp-retro-flange))
                                       (lambda (w context info) (XtUnmanageChild retro-flange-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Retro flanger"
                                                      "A model of someone flanging the input. Models the tape saturation effects and frequency smear. The smear could probably be done better.\n\ Average stall (ms): The average time difference between the two tapes, per stall.\n\ Flange frequency (Hz): The rate the tape is stalled at."))
                                       (lambda (w c i)
                                         (set! retro-flange-stall 0.0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* retro-flange-stall 100))))
                                         (set! retro-flange-frequency 1.0)
                                         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* retro-flange-frequency 100)))))))
             (set! sliders
                   (add-sliders retro-flange-dialog
                                (list 
                                      (list "average stall (ms)" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! retro-flange-stall (/ (.value info) 100)))
                                            100)
 				      (list "flange frequency (Hz)" 0.5 1.0 8.0
                                            (lambda (w context info)
                                              (set! retro-flange-frequency (/ (.value info) 100)))
                                            100))))))
       (activate-dialog retro-flange-dialog))))

      (let ((child (XtCreateManagedWidget "Retro flange" xmPushButtonWidgetClass ladspa-mod-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-retro-flange-dialog))))

;;; Ring modulation
;;;

(define ringmod-modulation-depth 0)
(define ringmod-frequency 100)
(define ringmod-sine-level 0)
(define ringmod-triangle-level 0)
(define ringmod-sawtooth-level 0)
(define ringmod-square-level 0)
(define ringmod-dialog #f)
(define ringmod-label "Ring modulation")

(define (cp-ringmod)
  (apply-ladspa (make-sample-reader (cursor))
                (list "ringmod_1188" "ringmod_1i1o1l" ringmod-modulation-depth ringmod-frequency ringmod-sine-level ringmod-triangle-level ringmod-sawtooth-level ringmod-square-level)
                (- (frames) (cursor))
                "ring modulation"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-ringmod-dialog)
       (if (not (Widget? ringmod-dialog))
           ;; if ringmod-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! ringmod-dialog
                   (make-effect-dialog "ringmod ladspa plugin"
                                       (lambda (w context info) (cp-ringmod))
                                       (lambda (w context info) (XtUnmanageChild ringmod-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Ring modulation Help"
                                                      "Ring or amplitude modulation with LFO.\n\ Move the sliders to set the ring modulation parameters.\n\ Modulation depth key: 0=none 1=AM 2=RM"))
                                       (lambda (w c i)
                                         (set! ringmod-modulation-depth 0)
                                         (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* ringmod-modulation-depth 1))))
				         (set! ringmod-frequency 100)
				         (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* ringmod-frequency 1))))
                                         (set! ringmod-sine-level 0)
                                         (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* ringmod-sine-level 100))))
                                         (set! ringmod-triangle-level 0)
                                         (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* ringmod-triangle-level 100))))
                                         (set! ringmod-sawtooth-level 0)
                                         (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* ringmod-sawtooth-level 100))))
                                         (set! ringmod-square-level 0)
                                         (XtSetValues (list-ref sliders 5) (list XmNvalue (inexact->exact (* ringmod-square-level 100)))))))
             (set! sliders
                   (add-sliders ringmod-dialog
                                (list (list "modulation depth" 0 0 2
                                            (lambda (w context info)
                                              (set! ringmod-modulation-depth (/ (.value info) 1)))
                                            1)
                                      (list "frequency (Hz)" 1 100 1000
                                            (lambda (w context info)
                                              (set! ringmod-frequency (/ (.value info) 1)))
                                            1)
                                      (list "sine level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-sine-level (/ (.value info) 100)))
                                            100)
                                      (list "triangle level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-triangle-level (/ (.value info) 100)))
                                            100)
                                      (list "sawtooth level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-sawtooth-level (/ (.value info) 100)))
                                            100)
                                      (list "square level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-square-level (/ (.value info) 100)))
                                            100))))))
       (activate-dialog ringmod-dialog))))


      (let ((child (XtCreateManagedWidget "Ring modulation" xmPushButtonWidgetClass ladspa-mod-menu
                                           (list XmNbackground (basic-color)))))
        (XtAddCallback child XmNactivateCallback
                        (lambda (w c i)
                          (post-ringmod-dialog))))

;;; REVERBS
;;;

(define ladspa-reverb-menu (XmCreatePulldownMenu plug-menu "Reverbs"
                                        (list XmNbackground (basic-color))))
(define ladspa-reverb-cascade (XtCreateManagedWidget "Reverbs" xmCascadeButtonWidgetClass plug-menu
                                            (list XmNsubMenuId ladspa-reverb-menu
                                                  XmNbackground (basic-color))))



;;; LADSPA Freeverb3 
;;;

(define freeverb-roomsize .5)
(define freeverb-damping .5)
(define freeverb-wet-level .5)
(define freeverb-dry-level .5)
(define freeverb-width .5)
(define freeverb-freeze-mode 0)
(define freeverb-dialog #f)
(define freeverb-label "Freeverb3")
(define freeverb-target 'sound)
(define freeverb-truncate #t)
(define freeverb-menu-widget #f)

(define (post-freeverb-dialog)
  (if (not freeverb-dialog)
      ;; if freeverb-dialog doesn't exist, create it
      (let ((initial-freeverb-roomsize .5)
            (initial-freeverb-damping .5)
            (initial-freeverb-wet-level .5)
            (initial-freeverb-dry-level .5)
            (initial-freeverb-width 0)
            (initial-freeverb-freeze-mode 0)
            (sliders '()))
        (set! freeverb-dialog
              (make-effect-dialog
               freeverb-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "cmt" "freeverb3" freeverb-freeze-mode freeverb-roomsize freeverb-damping freeverb-wet-level freeverb-dry-level freeverb-width)
                  freeverb-target
                  "Freeverb3" 
		  (and freeverb-truncate
                       (* 4 freeverb-roomsize))))
               (lambda (w context info) (XtUnmanageChild freeverb-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Freeverb3" "Jezar's famous reverb. Move the sliders to set the reverb parameters.\n\ This effect works only with stereo soundfiles!\n\ See the Freeverb Web page at http://www.jw015a0732.pwp.blueyonder.co.uk/freeverb.htm for more information."))

               (lambda (w c i)
                 (set! freeverb-roomsize .5)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* freeverb-roomsize 100))))
                 (set! freeverb-damping .5)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* freeverb-damping 100))))
                 (set! freeverb-wet-level .5)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* freeverb-wet-level 100))))
                 (set! freeverb-dry-level .5)
                 (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* freeverb-dry-level 100))))
                 (set! freeverb-width .5)
                 (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* freeverb-width 100)))))))

             (set! sliders
                   (add-sliders freeverb-dialog
                                (list (list "room size" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-roomsize (/ (.value info) 100)))
                                            100)
                                      (list "damping" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-damping (/ (.value info) 100)))
                                            100)
                                      (list "wet level" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-wet-level (/ (.value info) 100)))
                                            100)
                                      (list "dry level" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-dry-level (/ (.value info) 100)))
                                            100)
                                      (list "width" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-width (/ (.value info) 100)))
                                            100))))

             (let* ((s1 (XmStringCreateLocalized "Freeze mode"))
                     (toggle

                      (XtCreateManagedWidget "Freeze mode" xmToggleButtonWidgetClass (XtParent (car sliders))

                        (list XmNselectColor  (pushed-button-color)
                              XmNbackground   (basic-color)
                              XmNvalue        freeverb-freeze-mode
                              ;XmNborderWidth  1
                              XmNlabelString  s1))))
                (XmStringFree s1)
                (XtAddCallback toggle XmNvalueChangedCallback (lambda (w c i) (set! freeverb-freeze-mode (if (.set i) 1 0)))))

        (add-target (XtParent (car sliders))
                    (lambda (target) (set! freeverb-target target)) 
		    (lambda (truncate) (set! freeverb-truncate truncate)))))

  (activate-dialog freeverb-dialog))

(let ((child (XtCreateManagedWidget "Freeverb3" xmPushButtonWidgetClass ladspa-reverb-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-freeverb-dialog)))
  (set! ladspa-reverb-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Freeverb3 (~1,2F ~1,2F ~1,2F ~1,2F ~1,2F)" freeverb-roomsize freeverb-damping freeverb-wet-level freeverb-dry-level freeverb-width)))
                  (change-label child new-label)))
              ladspa-reverb-menu-list)))



;;; LADSPA Gverb 
;;;

(define gverb-roomsize 30)
(define gverb-reverb-time 7.0)
(define gverb-damping 0.5)
(define gverb-bandwidth 0.5)
(define gverb-dry-level 0)
(define gverb-early-reflect-level 0)
(define gverb-tail-level 0)
(define gverb-dialog #f)
(define gverb-label "Gverb")
(define gverb-target 'sound)
(define gverb-truncate #t)
(define gverb-menu-widget #f)

(define (post-gverb-dialog)
  (if (not gverb-dialog)
      ;; if gverb-dialog doesn't exist, create it
      (let ((initial-gverb-roomsize 30)
            (initial-gverb-reverb-time 7.0)
            (initial-gverb-damping 0.5)
            (initial-gverb-bandwidth 0.5)
            (initial-gverb-dry-level 0)
            (initial-gverb-early-reflect-level 0)
            (initial-gverb-tail-level 0)
            (sliders '()))
        (set! gverb-dialog
              (make-effect-dialog
               gverb-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "gverb_1216" "gverb" gverb-roomsize gverb-reverb-time gverb-damping gverb-bandwidth gverb-dry-level gverb-early-reflect-level gverb-tail-level)
                  gverb-target
                  "Gverb" 
		  (and gverb-truncate
                       (* 4 gverb-roomsize))))
               (lambda (w context info) (XtUnmanageChild gverb-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Gverb" "A mono in, stereo out reverb implementation by Juhana Sadeharju (kouhia at nic.funet.fi). Steve Harris ported it to LADSPA and did some testing. Please contact Juhana directly regarding any bugs you find.\n\ Roomsize (m): The size of the room, in meters. Excessivly large, and excessivly small values will make it sound a bit unrealistic. Values of around 30 sound good.\n\ Reverb time (s): Reverb decay time, in seconds. 7 is a good place to start.\n\ Damping: This controls the high-frequency damping (a lowpass filter). Values near 1 will make it sound very bright, values near 0 will make it sound very dark.\n\ Input bandwidth: This is like a damping control for the input. It has a similar effect to the damping control, but is subtly different.\n\ Dry signal level (dB): The amount of dry signal to be mixed with the reverbed signal.\n\ Early reflection level (dB): The quantity of early reflections (scatter reflections directly from the source). Think of Lexicon's ambience patches.\n\ Tail level (dB): The level of the classic reverb tail reflections."))

               (lambda (w c i)
                 (set! gverb-roomsize 30)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* gverb-roomsize 1))))
                 (set! gverb-reverb-time 7.0)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* gverb-reverb-time 100))))
                 (set! gverb-damping 0.5)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* gverb-damping 100))))
                 (set! gverb-bandwidth 0.5)
                 (XtSetValues (list-ref sliders 3) (list XmNvalue (inexact->exact (* gverb-bandwidth 100))))
                 (set! gverb-dry-level 0)
                 (XtSetValues (list-ref sliders 4) (list XmNvalue (inexact->exact (* gverb-dry-level 1))))
                 (set! gverb-early-reflect-level 0)
                 (XtSetValues (list-ref sliders 5) (list XmNvalue (inexact->exact (* gverb-early-reflect-level 1))))
                 (set! gverb-tail-level 0)
                 (XtSetValues (list-ref sliders 6) (list XmNvalue (inexact->exact (* gverb-tail-level 1)))))))
        (set! sliders
              (add-sliders gverb-dialog
                                (list (list "room size (m)" 1 30 300
                                            (lambda (w context info)
                                              (set! gverb-roomsize (/ (.value info) 1)))
                                            1)
                                      (list "reverb time (s)" 0.1 7.0 360.0
                                            (lambda (w context info)
                                              (set! gverb-reverb-time (/ (.value info) 100)))
                                            100)
                                      (list "damping" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! gverb-damping (/ (.value info) 100)))
                                            100)
                                      (list "bandwidth" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! gverb-bandwidth (/ (.value info) 100)))
                                            100)
                                      (list "dry level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-dry-level (/ (.value info) 1)))
                                            1)
                                      (list "early reflections level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-early-reflect-level (/ (.value info) 1)))
                                            1)
                                      (list "tail level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-tail-level (/ (.value info) 1)))
                                            1))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! gverb-target target)) 
		    (lambda (truncate) (set! gverb-truncate truncate)))))

  (activate-dialog gverb-dialog))

(let ((child (XtCreateManagedWidget "Gverb" xmPushButtonWidgetClass ladspa-reverb-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-gverb-dialog)))
  (set! ladspa-reverb-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Gverb (~1,2D ~1,2F ~1,2F ~1,2F ~1,2D ~1,2D ~1,2D)" gverb-roomsize gverb-reverb-time gverb-damping gverb-bandwidth gverb-dry-level gverb-early-reflect-level gverb-tail-level)))
                  (change-label child new-label)))
              ladspa-reverb-menu-list)))




;;; LADSPA Plate reverb
;;;

(define plate-reverb-time 1.0)
(define plate-reverb-damping 0.5)
(define plate-reverb-mix 0.5)
(define plate-reverb-dialog #f)
(define plate-reverb-label "Plate reverb")
(define plate-reverb-target 'sound)
(define plate-reverb-truncate #t)
(define plate-reverb-menu-widget #f)

(define (post-plate-reverb-dialog)
  (if (not plate-reverb-dialog)
      ;; if plate-reverb-dialog doesn't exist, create it
      (let ((initial-plate-reverb-time 1.0)
            (initial-plate-reverb-damping 0.5)
            (initial-plate-reverb-mix 0.5)
            (sliders '()))
        (set! plate-reverb-dialog
              (make-effect-dialog
               plate-reverb-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "plate_1423" "plate" plate-reverb-time plate-reverb-damping plate-reverb-mix)
                  plate-reverb-target
                  "Plate reverb" 
		  (and plate-reverb-truncate
                       (* 4 plate-reverb-time))))
               (lambda (w context info) (XtUnmanageChild plate-reverb-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Plate reverb" "A physical model of a steel plate reverb. Based on Josep Comajuncosas' gong model, it uses 8 linear waveguides to model the plate.\n\ Reverb time: Controls the RT60 time of the reverb. Actually controls the size of the plate. The mapping betwwen plate size and RT60 time is just a heuristic, so it's not very accurate.\n\ Damping: Controls the degree that the surface of the plate is damped.\n\ Dry/wet mix: Controls the balance between the dry and wet signals."))

               (lambda (w c i)
                 (set! plate-reverb-time initial-plate-reverb-time)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* plate-reverb-time 100))))
                 (set! plate-reverb-damping initial-plate-reverb-damping)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* plate-reverb-damping 100))))
                 (set! plate-reverb-mix initial-plate-reverb-mix)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* plate-reverb-mix 100)))))))
        (set! sliders
              (add-sliders plate-reverb-dialog
                           (list
                                 (list "reverb time" 0.01 initial-plate-reverb-time 8.5
                                        (lambda (w context info)
                                          (set! plate-reverb-time (/ (.value info) 100)))
                                          100)
                                 (list "damping" 0.0 initial-plate-reverb-damping 1.0
                                         (lambda (w context info)
                                          (set! plate-reverb-damping (/ (.value info) 100)))
                                          100)
                                 (list "dry/wet mix" 0.0 initial-plate-reverb-mix 1.0
                                          (lambda (w context info)
                                           (set! plate-reverb-mix (/ (.value info) 100)))
                                           100))))
        (add-target (XtParent (car sliders))
                    (lambda (target) (set! plate-reverb-target target)) 
		    (lambda (truncate) (set! plate-reverb-truncate truncate)))))

  (activate-dialog plate-reverb-dialog))

(let ((child (XtCreateManagedWidget "Plate reverb" xmPushButtonWidgetClass ladspa-reverb-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-plate-reverb-dialog)))
  (set! ladspa-reverb-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Plate reverb (~1,2F ~1,2F ~1,2F)" plate-reverb-time plate-reverb-damping plate-reverb-mix)))
                  (change-label child new-label)))
              ladspa-reverb-menu-list)))




;;; LADSPA Impulse convolver
;;;

(define impulse-convolver-id 1)
(define impulse-convolver-high-latency-mode 0)
(define impulse-convolver-gain 0)
(define impulse-convolver-dialog #f)
(define impulse-convolver-label "Impulse convolver")
(define impulse-convolver-target 'sound)
(define impulse-convolver-truncate #t)
(define impulse-convolver-menu-widget #f)

(define (post-impulse-convolver-dialog)
  (if (not impulse-convolver-dialog)
      ;; if impulse-convolver-dialog doesn't exist, create it
      (let ((initial-impulse-convolver-id 1.0)
            (initial-impulse-convolver-high-latency-mode 0.5)
            (initial-impulse-convolver-gain 0.5)
            (sliders '()))
        (set! impulse-convolver-dialog
              (make-effect-dialog
               impulse-convolver-label
               (lambda (w context info)
                 (apply-ladspa-over-target-with-sync
                  (list "imp_1199" "imp" impulse-convolver-id impulse-convolver-high-latency-mode impulse-convolver-gain)
                  impulse-convolver-target
                  "impulse convolver"
                  (and impulse-convolver-truncate
                       (* 4 impulse-convolver-id))))
               (lambda (w context info) (XtUnmanageChild impulse-convolver-dialog)) ; not needed in new-effects.scm version

               (lambda (w context info)
                 (help-dialog "Impulse convolver" "This is a convolver for a set of fairly short impulses. The set of impulses has to be compiled in; currently they are:\n\
ID   Impulse source\n\
\n\
1 Unit impulse (identity)\n\
2 My flat (light natural reverb)\n\
3 Yamaha Marshall stack simulator\n\
4 Fender 68 Vibrolux (SM57 on axis)\n\
5 Fender 68 Vibrolux (SM57 off axis)\n\
6 Fender 68 Vibrolux (Audio-technica AT4050)\n\
7 Fender 68 Vibrolux (Neumann U87)\n\
8 Fender Bassman (SM57 on axis)\n\
9 Fender Bassman (SM57 off axis)\n\
10 Fender Bassman (Audio-technica AT4050)\n\
11 Fender Bassman (Neumann U87)\n\
12 Fender Superchamp (SM57 on axis)\n\
13 Fender Superchamp (SM57 off axis)\n\
14 Fender Superchamp (Audio-technica AT4050)\n\
15 Fender Superchamp (Neumann U87)\n\
16 Marshall JCM2000 (SM57 on axis)\n\
17 Marshall JCM2000 (SM57 off axis)\n\
18 Marshall Plexi (SM57 on axis)\n\
19 Marshall Plexi (SM57 off axis)\n\
20 Matchless Chieftain (SM57 on axis)\n\
21 Matchless Chieftain (SM57 off axis)\n\
\n\
The impulse ID selects the impulse to convolve with.\n\ High latency mode: If you are running with blocks that are not whole powers of two long, or you are hearing distortion, try changing this to 1.\n\ Gain (dB): Controls the gain of the output signal in decibels." ))

                (lambda (w c i)
                 (set! impulse-convolver-id 1)
                 (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* impulse-convolver-id 1))))
                 (set! impulse-convolver-high-latency-mode 0)
                 (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* impulse-convolver-high-latency-mode 1))))
                 (set! impulse-convolver-gain 0)
                 (XtSetValues (list-ref sliders 2) (list XmNvalue (inexact->exact (* impulse-convolver-gain 1)))))))
             (set! sliders

              (add-sliders impulse-convolver-dialog
                                (list (list "impulse ID" 1 initial-impulse-convolver-id 21
                                            (lambda (w context info)
                                              (set! impulse-convolver-id (/ (.value info) 1)))
                                            1)
                                      (list "high latency mode" 0 initial-impulse-convolver-high-latency-mode 1
                                            (lambda (w context info)
                                              (set! impulse-convolver-high-latency-mode (/ (.value info) 1)))
                                            1)
                                      (list "gain" -90 initial-impulse-convolver-gain 24
                                            (lambda (w context info)
                                              (set! impulse-convolver-gain (/ (.value info) 1)))
                                            1))))

        (add-target (XtParent (car sliders))
                    (lambda (target) (set! impulse-convolver-target target))
                    (lambda (truncate) (set! impulse-convolver-truncate truncate)))))

  (activate-dialog impulse-convolver-dialog))

(let ((child (XtCreateManagedWidget "Impulse convolver" xmPushButtonWidgetClass ladspa-reverb-menu
                                    (list XmNbackground (basic-color)))))
  (XtAddCallback child XmNactivateCallback
                 (lambda (w c i)
                   (post-impulse-convolver-dialog)))
  (set! ladspa-reverb-menu-list
        (cons (lambda ()
                (let ((new-label (format #f "Impulse convolver (~1,2D ~1,2D ~1,2D)" impulse-convolver-id impulse-convolver-high-latency-mode impulse-convolver-gain)))
                  (change-label child new-label)))
              ladspa-reverb-menu-list)))

