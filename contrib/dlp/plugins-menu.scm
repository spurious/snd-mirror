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

(define plug-menu (|Widget (main-menu plugins-menu)))

;;; create the submenu


;;; DELAY EFFECTS
;;;

(define ladspa-delay-menu (|XmCreatePulldownMenu plug-menu "Delay Effects"
                                        (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))
(define ladspa-delay-cascade (|XtCreateManagedWidget "Delay Effects" |xmCascadeButtonWidgetClass plug-menu
                                            (list |XmNsubMenuId ladspa-delay-menu
                                                  |XmNbackground (|Pixel (snd-pixel (basic-color))))))


;;; Canyon delay
;;;

(define canyon-delay-left-to-right-time .10)
(define canyon-delay-left-to-right-feedback 0)
(define canyon-delay-right-to-left-time .10)
(define canyon-delay-right-to-left-feedback 0)
(define canyon-delay-filter-cutoff 1000)
(define canyon-delay-dialog #f)
(define canyon-delay-label "Canyon delay (stereo)")

(define (cp-canyon-delay)
  (let* ((snd (selected-sound)) 
         (chns (channels snd))
         (readers (if (= chns 1)
         (make-sample-reader 0 snd 0)
         ;; assume stereo -- this could collect an arbitrary list
         (list (make-sample-reader 0 snd 0)
               (make-sample-reader 0 snd 1)))))
         (apply-ladspa readers
           (list "cmt" "canyon_delay" canyon-delay-left-to-right-time canyon-delay-left-to-right-feedback canyon-delay-right-to-left-time canyon-delay-right-to-left-feedback canyon-delay-filter-cutoff)
             (- (frames) (cursor))
             "canyon delay")))


(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-canyon-delay-dialog)
       (if (not (|Widget? canyon-delay-dialog))
           (let ((sliders '()))
             (set! canyon-delay-dialog
                   (make-effect-dialog "canyon stereo delay ladspa plugin"
                                       (lambda (w context info) (cp-canyon-delay))
                                       (lambda (w context info) (|XtUnmanageChild canyon-delay-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Canyon delay"
                                                      "A deep stereo cross-delay with built-in low pass filters. Note: This effect works only with stereo soundfiles !"))
                                       (lambda (w c i)
                                         (set! canyon-delay-left-to-right-time .10)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* canyon-delay-left-to-right-time 100))))
                                         (set! canyon-delay-left-to-right-feedback 0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* canyon-delay-left-to-right-feedback 100))))
                                         (set! canyon-delay-right-to-left-time .10)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* canyon-delay-right-to-left-time 100))))
                                         (set! canyon-delay-right-to-left-feedback 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* canyon-delay-right-to-left-feedback 100))))
                                         (set! canyon-delay-filter-cutoff 1000)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* canyon-delay-filter-cutoff 1)))))))
             (set! sliders
                   (add-sliders canyon-delay-dialog
                                (list (list "left to right time (s)" 0.01 .10 .99
                                            (lambda (w context info)
                                              (set! canyon-delay-left-to-right-time (/ (|value info) 100)))
                                            100)
                                      (list "left to right feedback (%)" -1 0 1
                                            (lambda (w context info)
                                              (set! canyon-delay-left-to-right-feedback (/ (|value info) 100)))
                                            100)
 				      (list "right to left time (s)" 0.01 .10 .99
                                            (lambda (w context info)
                                              (set! canyon-delay-right-to-left-time (/ (|value info) 100)))
                                            100)
                                      (list "right to left feedback (%)" -1 0 1
                                            (lambda (w context info)
                                              (set! canyon-delay-right-to-left-feedback (/ (|value info) 100)))
                                            100)
                                      (list "low-pass filter cutoff (Hz)" 1 1000 44100
                                            (lambda (w context info)
                                              (set! canyon-delay-filter-cutoff (/ (|value info) 1)))
                                            1))))))
       (activate-dialog canyon-delay-dialog))))

      (let ((child (|XtCreateManagedWidget "Canyon delay (stereo)" |xmPushButtonWidgetClass ladspa-delay-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-canyon-delay-dialog))))


;;; Delay (5 sec)
;;;

(define delay5s-time .3)
(define delay5s-balance .5)
(define delay5s-dialog #f)
(define delay5s-label "Delay (5 sec)")

(define (cp-delay5s)
  (apply-ladspa (make-sample-reader (cursor))
      (list "delay" "delay_5s" delay5s-time delay5s-balance)
      (- (frames) (cursor))
      "delay5s"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-delay5s-dialog)
       (if (not (|Widget? delay5s-dialog))
           ;; if delay5s-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! delay5s-dialog
                   (make-effect-dialog "delay_5s ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "delay" "delay_5s" delay5s-time delay5s-balance)
                                                       (- (frames) (cursor))
                                                       "delay5s"))
                                       (lambda (w context info) (|XtUnmanageChild delay5s-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Delay (5 sec)"
                                                      "Move the slider to set delay parameters."))
                                       (lambda (w c i)
                                         (set! delay5s-time .3)
                                         (|XtSetValues (list-ref sliders 0)
                                            (list |XmNvalue (inexact->exact (* delay5s-time 100))))
				         (set! delay5s-balance .5)
				         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* delay5s-balance 100)))))))
             (set! sliders
                   (add-sliders delay5s-dialog
                                (list (list "delay time" 0.0 0.3 5.0
                                            (lambda (w context info)
                                              (set! delay5s-time (/ (|value info) 100.0)))
                                            100)
                                      (list "balance" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! delay5s-balance (/ (|value info) 100.0)))
                                            100))))))
       (activate-dialog delay5s-dialog))))

      (let ((child (|XtCreateManagedWidget "Delay (5 sec)" |xmPushButtonWidgetClass ladspa-delay-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-delay5s-dialog))))


;;; Feedback delay
;;;

(define fbdelay5s-time 1.0)
(define fbdelay5s-balance .5)
(define fbdelay5s-feedback 0)
(define fbdelay5s-dialog #f)
(define fbdelay5s-label "Feedback delay (5 sec)")

(define (cp-fbdelay5s)
   (apply-ladspa (make-sample-reader (cursor))
      (list "cmt" "fbdelay_5s" fbdelay5s-time fbdelay5s-balance fbdelay5s-feedback)
            (- (frames) (cursor))
            "fbdelay5s"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-fbdelay5s-dialog)
       (if (not (|Widget? fbdelay5s-dialog))
           ;; if fbdelay5s-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! fbdelay5s-dialog
                   (make-effect-dialog "feedback delay ladspa plugin"
                                       (lambda (w context info) (cp-fbdelay5s))
                                       (lambda (w context info) (|XtUnmanageChild fbdelay5s-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Feedback delay (5 sec)"
                                                      "Feedback delay with maximum delay time of 5 seconds."))
                                       (lambda (w c i)
                                         (set! fbdelay5s-time 1.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* fbdelay5s-time 100))))
				         (set! fbdelay5s-balance .5)
				         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* fbdelay5s-balance 100))))
                                         (set! fbdelay5s-feedback 0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* fbdelay5s-feedback 100)))))))
             (set! sliders
                   (add-sliders fbdelay5s-dialog
                                (list (list "delay time (s)" 0.0 1.0 5.0
                                            (lambda (w context info)
                                              (set! fbdelay5s-time (/ (|value info) 100)))
                                            100)
                                      (list "balance" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! fbdelay5s-balance (/ (|value info) 100)))
                                            100)
                                      (list "feedback" -1 0 1
                                            (lambda (w context info)
                                              (set! fbdelay5s-feedback (/ (|value info) 100)))
                                            100))))))
       (activate-dialog fbdelay5s-dialog))))

      (let ((child (|XtCreateManagedWidget "Feedback delay (5 sec)" |xmPushButtonWidgetClass ladspa-delay-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-fbdelay5s-dialog))))

;;; Fractionally addressed delay line
;;;

(define fractional-delay-time 1.0)
(define fractional-delay-feedback 0)
(define fractional-delay-dialog #f)
(define fractional-delay-label "Fractionally addressed delay")

(define (cp-fractional-delay)
   (apply-ladspa (make-sample-reader (cursor))
      (list "fad_delay_1192" "fadDelay" fractional-delay-time fractional-delay-feedback)
         (- (frames) (cursor))
         "fad delay"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-fractional-delay-dialog)
       (if (not (|Widget? fractional-delay-dialog))
           (let ((sliders '()))
             (set! fractional-delay-dialog
                   (make-effect-dialog "fractionally addressed delay line ladspa plugin"
                                       (lambda (w context info) (cp-fractional-delay))
                                       (lambda (w context info) (|XtUnmanageChild fractional-delay-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Fractionally addressed delay line"
                                                      "A fixed ring buffer delay implementation. Has different dynamics than a normal delay, more suitable for certain things.\n\ Changes in delay length are generally more pleasing, but delays >2s long have reduced sound quality."))
                                       (lambda (w c i)
                                         (set! fractional-delay-time 1.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* fractional-delay-time 100))))
                                         (set! fractional-delay-feedback 0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* fractional-delay-feedback 1)))))))
             (set! sliders
                   (add-sliders fractional-delay-dialog
                                (list (list "delay time (s)" 0.1 1.0 10.0
                                            (lambda (w context info)
                                              (set! fractional-delay-time (/ (|value info) 100)))
                                            100)
                                      (list "feedback (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! fractional-delay-feedback (/ (|value info) 1)))
                                            1))))))
       (activate-dialog fractional-delay-dialog))))

      (let ((child (|XtCreateManagedWidget "Fractionally addressed delay" |xmPushButtonWidgetClass ladspa-delay-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-fractional-delay-dialog))))



;;; Granular scatter processor
;;;

(define gs-density 5)
(define gs-scatter 1)
(define gs-grain-length 1)
(define gs-grain-attack 1)
(define gs-dialog #f)
(define gs-label "Granular scatter processor")

(define (cp-gs)
  (apply-ladspa (make-sample-reader (cursor))
    (list "cmt" "grain_scatter" gs-density gs-scatter gs-grain-length gs-grain-attack)
          (- (frames) (cursor))
          "grain_scatter"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-gs-dialog)
       (if (not (|Widget? gs-dialog))
           (let ((sliders '()))
             (set! gs-dialog
                   (make-effect-dialog "granular scatter processor ladspa plugin"
                                       (lambda (w context info) (cp-gs))
                                       (lambda (w context info) (|XtUnmanageChild gs-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Granular scatter processor"
                                                      "This plugin generates an output audio stream by scattering short `grains' of sound from an input stream. It is possible to control the length and envelope of these grains, how far away from their source time grains may be scattered and the density (grains/sec) of the texture produced."))
                                       (lambda (w c i)
                                         (set! gs-density 5)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* gs-density 1))))
                                         (set! gs-scatter 1)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* gs-scatter 100))))
                                         (set! gs-grain-length 1)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* gs-grain-length 100))))
                                         (set! gs-grain-attack 1)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* gs-grain-attack 100)))))))
             (set! sliders
                   (add-sliders gs-dialog
                                (list (list "density (grains/s)" 0 5 100
                                            (lambda (w context info)
                                              (set! gs-density (/ (|value info) 1)))
                                            1)
                                      (list "scatter" 0 1 5
                                            (lambda (w context info)
                                              (set! gs-scatter (/ (|value info) 100)))
                                            100)
 				      (list "grain length (s)" 0 1 5
                                            (lambda (w context info)
                                              (set! gs-grain-length (/ (|value info) 100)))
                                            100)
                                      (list "grain attack (s)" 0 1 5
                                            (lambda (w context info)
                                              (set! gs-grain-attack (/ (|value info) 100)))
                                            100))))))
       (activate-dialog gs-dialog))))

      (let ((child (|XtCreateManagedWidget "Granular scatter processor" |xmPushButtonWidgetClass ladspa-delay-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-gs-dialog))))


;;; Tape delay
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

(define (cp-tape-delay)
   (apply-ladspa (make-sample-reader (cursor))
      (list "tape_delay_1211" "tapeDelay" tape-delay-tape-speed tape-delay-dry-level tape-delay-tap-1-distance tape-delay-tap-1-level tape-delay-tap-2-distance tape-delay-tap-2-level tape-delay-tap-3-distance tape-delay-tap-3-level tape-delay-tap-4-distance tape-delay-tap-4-level)
         (- (frames) (cursor))
         "tape delay"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-tape-delay-dialog)
       (if (not (|Widget? tape-delay-dialog))
           (let ((sliders '()))
             (set! tape-delay-dialog
                   (make-effect-dialog "tape delay ladspa plugin"
                                       (lambda (w context info) (cp-tape-delay))
                                       (lambda (w context info) (|XtUnmanageChild tape-delay-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Tape delay"
                                                      "Correctly models the tape motion and some of the smear effect, but there is no simulation for the head saturation yet. The way the tape accelerates and decelerates gives a nicer delay effect for many purposes."))
                                       (lambda (w c i)
                                         (set! tape-delay-tape-speed 1)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* tape-delay-tape-speed 100))))
                                         (set! tape-delay-dry-level 0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* tape-delay-dry-level 1))))
                                         (set! tape-delay-tap-1-distance 0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* tape-delay-tap-1-distance 100))))
                                         (set! tape-delay-tap-1-level 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* tape-delay-tap-1-level 1))))
                                         (set! tape-delay-tap-2-distance 0)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* tape-delay-tap-2-distance 100))))
                                         (set! tape-delay-tap-2-level 0)
                                         (|XtSetValues (list-ref sliders 5) (list |XmNvalue (inexact->exact (* tape-delay-tap-2-level 1))))
                                         (set! tape-delay-tap-3-distance 0)
                                         (|XtSetValues (list-ref sliders 6) (list |XmNvalue (inexact->exact (* tape-delay-tap-3-distance 100))))
                                         (set! tape-delay-tap-3-level 0)
                                         (|XtSetValues (list-ref sliders 7) (list |XmNvalue (inexact->exact (* tape-delay-tap-3-level 1))))
                                         (set! tape-delay-tap-4-distance 0)
                                         (|XtSetValues (list-ref sliders 8) (list |XmNvalue (inexact->exact (* tape-delay-tap-4-distance 100))))
                                         (set! tape-delay-tap-4-level 0)
                                         (|XtSetValues (list-ref sliders 9) (list |XmNvalue (inexact->exact (* tape-delay-tap-4-level 1)))))))
             (set! sliders
                   (add-sliders tape-delay-dialog
                                (list (list "tape speed (inches/sec, 1=normal)" 0 1 10
                                            (lambda (w context info)
                                              (set! tape-delay-tape-speed (/ (|value info) 100)))
                                            100)
                                      (list "dry level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-dry-level (/ (|value info) 1)))
                                            1)
 				      (list "tap 1 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-1-distance (/ (|value info) 100)))
                                            100)
                                      (list "tap 1 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-1-level (/ (|value info) 1)))
                                            1)
                                      (list "tap 2 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-2-distance (/ (|value info) 100)))
                                            100)
                                      (list "tap 2 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-2-level (/ (|value info) 1)))
                                            1)
                                      (list "tap 3 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-3-distance (/ (|value info) 100)))
                                            100)
                                      (list "tap 3 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-3-level (/ (|value info) 1)))
                                            1)
                                      (list "tap 4 distance (inches)" 0 0 4
                                            (lambda (w context info)
                                              (set! tape-delay-tap-4-distance (/ (|value info) 100)))
                                            100)
                                      (list "tap 4 level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! tape-delay-tap-4-level (/ (|value info) 1)))
                                            1))))))
       (activate-dialog tape-delay-dialog))))

      (let ((child (|XtCreateManagedWidget "Tape delay" |xmPushButtonWidgetClass ladspa-delay-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-tape-delay-dialog))))

;;; DISTORTION EFFECTS
;;;

(define ladspa-distort-menu (|XmCreatePulldownMenu plug-menu "Distortion Effects"
                                        (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))
(define ladspa-distort-cascade (|XtCreateManagedWidget "Distortion Effects" |xmCascadeButtonWidgetClass plug-menu
                                            (list |XmNsubMenuId ladspa-distort-menu
                                                  |XmNbackground (|Pixel (snd-pixel (basic-color))))))


;;; Decimator
;;;

(define decimator-bit-depth 5)
(define decimator-sample-rate .5)
(define decimator-dialog #f)
(define decimator-label "Decimator")

(define (cp-decimator)
 (let* ((snd (selected-sound))
        (sr (srate snd))
        (dsr (* decimator-sample-rate sr)))
 (apply-ladspa (make-sample-reader (cursor))
               (list "decimator_1202" "decimator" decimator-bit-depth dsr)
               (- (frames) (cursor))
               "decimator")))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-decimator-dialog)
       (if (not (|Widget? decimator-dialog))
           ;; if decimator-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! decimator-dialog
                   (make-effect-dialog "decimator ladspa plugin"
                                       (lambda (w context info) (cp-decimator))
                                       (lambda (w context info) (|XtUnmanageChild decimator-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Decimator"
                                                      "Reduces the effective sample rate, reduces the input signal, and allows non-integer values for smooth transitions between clean and low-fidelity signals.\n\ Bit-depth: The bit-depth that the signal will be reduced to.\n\ Sample rate (Hz): The sample rate that the signal will be resampled at."))
                                       (lambda (w c i)
                                         (set! decimator-bit-depth 5)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* decimator-bit-depth 1))))
				         (set! decimator-sample-rate .5)
				         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* decimator-sample-rate 1000)))))))
             (set! sliders
                   (add-sliders decimator-dialog
                                (list (list "bit depth" 1 5 24
                                            (lambda (w context info)
                                              (set! decimator-bit-depth (/ (|value info) 1)))
                                            1)
                                      (list "sample rate (Hz)" 0.001 0.5 1.0
                                            (lambda (w context info)
                                              (set! decimator-sample-rate (/ (|value info) 1000)))
                                            1000))))))
       (activate-dialog decimator-dialog))))

      (let ((child (|XtCreateManagedWidget "Decimator" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-decimator-dialog))))



;;; Diode processor
;;;

(define diode-mode 0.0)
(define diode-dialog #f)
(define diode-label "Diode processor")

(define (cp-diode)
 (apply-ladspa (make-sample-reader (cursor))
               (list "diode_1185" "diode" diode-mode)
               (- (frames) (cursor))
               "diode processor"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-diode-dialog)
       (if (not (|Widget? diode-dialog))
           (let ((sliders '()))
             (set! diode-dialog
                   (make-effect-dialog "diode processor ladspa plugin"
                                       (lambda (w context info) (cp-diode))
                                       (lambda (w context info) (|XtUnmanageChild diode-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Diode processor"
                                                      "Mangles the signal as if it had been passed through a diode rectifier network. You should probably follow this with a DC offset remover, unless you want the offset.\n\ Mode: 0 for none, 1 for half wave, 2 for full wave, 3 for silence. The mode parameter is continuously variable from thru to half-wave rectification to full-wave to silence."))
                                       (lambda (w c i)
                                         (set! diode-mode 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* diode-mode 100)))))))
             (set! sliders
                   (add-sliders diode-dialog
                                (list
                                      (list "mode" 0.0 0.0 3.0
                                            (lambda (w context info)
                                              (set! diode-mode (/ (|value info) 100)))
                                            100))))))
       (activate-dialog diode-dialog))))

      (let ((child (|XtCreateManagedWidget "Diode processor" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-diode-dialog))))

;;; Fast overdrive
;;;

(define foverdrive-level 0.0)
(define foverdrive-dialog #f)
(define foverdrive-label "Fast overdrive")

(define (cp-foverdrive)
   (apply-ladspa (make-sample-reader (cursor))
      (list "foverdrive_1196" "overdrive" foverdrive-level)
         (- (frames) (cursor))
         "fast overdrive"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-foverdrive-dialog)
       (if (not (|Widget? foverdrive-dialog))
           (let ((sliders '()))
             (set! foverdrive-dialog
                   (make-effect-dialog "fast overdrive ladspa plugin"
                                       (lambda (w context info) (cp-foverdrive))
                                       (lambda (w context info) (|XtUnmanageChild foverdrive-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Fast overdrive" 
						"A simple overdrive. Compresses the extreme peaks to make a sound similar to an overdriven amplifier.\n\ Drive level: Controls the point at which the signal starts to compress, and the degree of compression."))
                                       (lambda (w c i)
                                         (set! foverdrive-level 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* foverdrive-level 100)))))))
             (set! sliders
                   (add-sliders foverdrive-dialog
                                (list 
				      (list "drive level" 1.0 0.0 3.0
                                            (lambda (w context info)
                                              (set! foverdrive-level (/ (|value info) 100)))
                                            100))))))
       (activate-dialog foverdrive-dialog))))

      (let ((child (|XtCreateManagedWidget "Fast overdrive" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-foverdrive-dialog))))


;;; Foldover distortion
;;;


(define foldover-drive 0.0)
(define foldover-skew 0.0)
(define foldover-dialog #f)
(define foldover-label "Foldover distortion")

(define (cp-foldover)
 (apply-ladspa (make-sample-reader (cursor))
               (list "foldover_1213" "foldover" foldover-drive foldover-skew)
               (- (frames) (cursor))
               "foldover distortion"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin 

    (define (post-foldover-dialog)
       (if (not (|Widget? foldover-dialog))
           (let ((sliders '()))
             (set! foldover-dialog
                   (make-effect-dialog "foldover distortion ladspa plugin"
                                       (lambda (w context info) (cp-foldover))
                                       (lambda (w context info) (|XtUnmanageChild foldover-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Foldover distortion"
                                                      "Uses a sine wave approximation to simulate valve-style foldover distortion. Probably should have a DC offset remover on the output, but it's not always necessary.\n\ Drive: Controls the degree of distortion.\n\ Skew: Controls the asymmetry of the waveform."))
                                       (lambda (w c i)
                                         (set! foldover-drive 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* foldover-drive 100))))
                                         (set! foldover-skew 0.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* foldover-skew 100)))))))
             (set! sliders
                   (add-sliders foldover-dialog
                                (list 
                                      (list "drive" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! foldover-drive (/ (|value info) 100)))
                                            100)
 				      (list "skew" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! foldover-skew (/ (|value info) 100)))
                                            100))))))
       (activate-dialog foldover-dialog))))

      (let ((child (|XtCreateManagedWidget "Foldover distortion" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-foldover-dialog))))


;;; GSM simulator
;;;

(define gsm-simulator-mix .50)
(define gsm-simulator-passes 5)
(define gsm-simulator-error-rate 10)
(define gsm-simulator-dialog #f)
(define gsm-simulator-label "GSM simulator")

(define (cp-gsm-simulator)
  (apply-ladspa (make-sample-reader (cursor))
                (list "gsm_1215" "gsm" gsm-simulator-mix gsm-simulator-passes gsm-simulator-error-rate)
                (- (frames) (cursor))
                "gsm-simulator"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-gsm-simulator-dialog)
       (if (not (|Widget? gsm-simulator-dialog))
           ;; if gsm-simulator-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! gsm-simulator-dialog
                   (make-effect-dialog "gsm-simulator ladspa plugin"
                                       (lambda (w context info) (cp-gsm-simulator))
                                       (lambda (w context info) (|XtUnmanageChild gsm-simulator-dialog))
                                       (lambda (w context info)
                                         (help-dialog "GSM simulator"
                                                      "Encodes and decodes a signal using the GSM voice compression system. Has the effect of making the signal sound like it is being sent over a European mobile phone network.\n\ Dry/wet mix: 0 will give you the dry signal (but with the appropriate amount of delay), 1 will give you a totally wet signal.\n\  Number of passes: The number of times the signal is sent through the encode/decode process. Increases the CPU consumption almost linearly, and it will become more peaky so less friendly to realtime processing.\n\  Error rate (bits/block): The number of simulated bits that get changed during the transmission process."))
                                       (lambda (w c i)
				         (set! gsm-simulator-mix .50) 
					 (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* gsm-simulator-mix 100))))
                                         (set! gsm-simulator-passes 5) 
					 (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* gsm-simulator-passes 1))))
                                         (set! gsm-simulator-error-rate 10)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* gsm-simulator-error-rate 1)))))))
             (set! sliders
                   (add-sliders gsm-simulator-dialog
                                (list 
                                      (list "wet/dry mix" 0.0 .50 1.0
                                            (lambda (w context info)
                                              (set! gsm-simulator-mix (/ (|value info) 100)))
                                            100)
				      (list "number of passes" 0 5 10
                                            (lambda (w context info)
                                              (set! gsm-simulator-passes (/ (|value info) 1)))
                                            1)
                                      (list "error rate (bits/block)" 0 10 30
                                            (lambda (w context info)
                                              (set! gsm-simulator-error-rate (/ (|value info) 1)))
                                            1))))))
       (activate-dialog gsm-simulator-dialog))))

      (let ((child (|XtCreateManagedWidget "GSM simulator" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-gsm-simulator-dialog))))

;;; Mono overdrive
;;;

(define overdrive-amp-limit 0)
(define overdrive-level 0.0)
(define overdrive-low-density-color 0)
(define overdrive-high-density-color 0)
(define overdrive-dialog #f)
(define overdrive-label "Mono overdrive")

(define (cp-overdrive)
   (apply-ladspa (make-sample-reader (cursor))
      (list "overdrive_1182" "overdrive" overdrive-amp-limit overdrive-level overdrive-low-density-color overdrive-high-density-color)
         (- (frames) (cursor))
         "mono overdrive"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-overdrive-dialog)
       (if (not (|Widget? overdrive-dialog))
           (let ((sliders '()))
             (set! overdrive-dialog
                   (make-effect-dialog "mono overdrive ladspa plugin"
                                       (lambda (w context info) (cp-overdrive))
                                       (lambda (w context info) (|XtUnmanageChild overdrive-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Mono overdrive" 
						"A basic overdrive effect, with controls for the degree of compression and for the distortion.\n\ Amps limit (in dB relative to nominal 0): Mostly this parameter is used to artificially lower the headroom of the amp, but it can also be used to counteract the effects of hosts that ignore the audio input range hints. If the host is using 16-bit int WAV files then a good guess for limit is +80dB.\n\ Drive level: This controls the degree of amplifier compression. Values above 1.0 will work, but they produce unpredictable output levels. Lowering the limit is a better way of increasing the distortion.\n\ Low-density coloration: Controls the amplitude of some low (input space) frequency amplitude distortion.\n\ High-density coloration: Controls the amplitude of some high (input space) frequency amplitude distortion."))
                                       (lambda (w c i)
                                         (set! overdrive-amp-limit 0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* overdrive-amp-limit 1))))
                                         (set! overdrive-level 0.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* overdrive-level 100))))
                                         (set! overdrive-low-density-color 0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* overdrive-low-density-color 1))))
                                         (set! overdrive-high-density-color 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* overdrive-high-density-color 1)))))))
             (set! sliders
                   (add-sliders overdrive-dialog
                                (list 
                                      (list "amp limit (dB relative to nominal 0)" -100 0 100
                                            (lambda (w context info)
                                              (set! overdrive-amp-limit (/ (|value info) 1)))
                                            1)
                                      (list "drive level" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! overdrive-level (/ (|value info) 100)))
                                            100)
                                      (list "low density coloration" 0 0 100
                                            (lambda (w context info)
                                              (set! overdrive-low-density-color (/ (|value info) 1)))
                                            1)
                                      (list "high density coloration" 0 0 100
                                            (lambda (w context info)
                                              (set! overdrive-high-density-color (/ (|value info) 1)))
                                            1))))))
       (activate-dialog overdrive-dialog))))

      (let ((child (|XtCreateManagedWidget "Mono overdrive" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-overdrive-dialog))))


;;; Signal sifter
;;;

(define sifter-size 100)
(define sifter-dialog #f)
(define sifter-label "Signal sifter")

(define (cp-sifter)
 (apply-ladspa (make-sample-reader (cursor))
               (list "sifter_1210.so" "sifter" sifter-size)
               (- (frames) (cursor))
               "sifter"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-sifter-dialog)
       (if (not (|Widget? sifter-dialog))
           (let ((sliders '()))
             (set! sifter-dialog
                   (make-effect-dialog "sifter ladspa plugin"
                                       (lambda (w context info) (cp-sifter))
                                       (lambda (w context info) (|XtUnmanageChild sifter-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Signal sifter"
                                                      "Sorts and mixes blocks of the input signal to give a 'bumpy ramp' effect.\n\ Certain types of input will produce silence on the output (mostly ones with only low frequency components).\n\ This is a very odd effect, and doesn't really have any music applications, but can produce some interesting noises."))
                                       (lambda (w c i)
                                         (set! sifter-size 100)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* sifter-size 1)))))))
             (set! sliders
                   (add-sliders sifter-dialog
                                (list (list "sifter size" 1 100 2000 
                                            (lambda (w context info)
                                              (set! sifter-size (/ (|value info) 1)))
                                            1))))))
       (activate-dialog sifter-dialog))))

      (let ((child (|XtCreateManagedWidget "Signal sifter" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-sifter-dialog))))


;;; Stereo overdrive
;;;

(define overdrive_s-amp-limit 0)
(define overdrive_s-level 0.0)
(define overdrive_s-low-density-color 0)
(define overdrive_s-high-density-color 0)
(define overdrive_s-dialog #f)
(define overdrive_s-label "Stereo overdrive")

(define (cp-overdrive_s)
   (apply-ladspa (make-sample-reader (cursor))
      (list "overdrive_1183" "overdrive_s" overdrive_s-amp-limit overdrive_s-level overdrive_s-low-density-color overdrive_s-high-density-color)
         (- (frames) (cursor))
         "stereo overdrive"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-overdrive_s-dialog)
       (if (not (|Widget? overdrive_s-dialog))
           (let ((sliders '()))
             (set! overdrive_s-dialog
                   (make-effect-dialog "stereo overdrive ladspa plugin"
                                       (lambda (w context info) (cp-overdrive_s))
                                       (lambda (w context info) (|XtUnmanageChild overdrive_s-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Stereo overdrive" 
						"A basic overdrive effect, with controls for the degree of compression and for the distortion.\n\ Amps limit (in dB relative to nominal 0): Mostly this parameter is used to artificially lower the headroom of the amp, but it can also be used to counteract the effects of hosts that ignore the audio input range hints. If the host is using 16-bit int WAV files then a good guess for limit is +80dB.\n\ Drive level: This controls the degree of amplifier compression. Values above 1.0 will work, but they produce unpredictable output levels. Lowering the limit is a better way of increasing the distortion.\n\ Low-density coloration: Controls the amplitude of some low (input space) frequency amplitude distortion.\n\ High-density coloration: Controls the amplitude of some high (input space) frequency amplitude distortion."))
                                       (lambda (w c i)
                                         (set! overdrive_s-amp-limit 0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* overdrive_s-amp-limit 1))))
                                         (set! overdrive_s-level 0.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* overdrive_s-level 100))))
                                         (set! overdrive_s-low-density-color 0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* overdrive_s-low-density-color 1))))
                                         (set! overdrive_s-high-density-color 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* overdrive_s-high-density-color 1)))))))
             (set! sliders
                   (add-sliders overdrive_s-dialog
                                (list 
                                      (list "amp limit (dB relative to nominal 0)" -100 0 100
                                            (lambda (w context info)
                                              (set! overdrive_s-amp-limit (/ (|value info) 1)))
                                            1)
                                      (list "drive level" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! overdrive_s-level (/ (|value info) 100)))
                                            100)
                                      (list "low density coloration" 0 0 100
                                            (lambda (w context info)
                                              (set! overdrive_s-low-density-color (/ (|value info) 1)))
                                            1)
                                      (list "high density coloration" 0 0 100
                                            (lambda (w context info)
                                              (set! overdrive_s-high-density-color (/ (|value info) 1)))
                                            1))))))
       (activate-dialog overdrive_s-dialog))))

      (let ((child (|XtCreateManagedWidget "Stereo overdrive" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-overdrive_s-dialog))))

;;; Transient mangler
;;;

(define transient-attack-speed 1)
(define transient-sustain-time 1)
(define transient-dialog #f)
(define transient-label "Transient mangler")

(define (cp-transient)
   (apply-ladspa (make-sample-reader (cursor))
                 (list "transient_1206" "transient" transient-attack-speed transient-sustain-time)
                 (- (frames) (cursor))
                 "transient"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-transient-dialog)
       (if (not (|Widget? transient-dialog))
           ;; if transient-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! transient-dialog
                   (make-effect-dialog "transient mangler ladspa plugin"
                                       (lambda (w context info) (cp-transient))
                                       (lambda (w context info) (|XtUnmanageChild transient-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Transient mangler"
                                                      "Move the sliders to set the mangler parameters."))
                                       (lambda (w c i)
                                         (set! transient-attack-speed 1)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* transient-attack-speed 100))))
                                         (set! transient-sustain-time 1)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* transient-sustain-time 100)))))))
             (set! sliders
                   (add-sliders transient-dialog
                                (list (list "attack speed" -1 1 1
                                            (lambda (w context info)
                                              (set! transient-attack-speed (/ (|value info) 100)))
                                            100)
                                      (list "sustain time" -1 1 1
                                            (lambda (w context info)
                                              (set! transient-sustain-time (/ (|value info) 100)))
                                            100))))))
       (activate-dialog transient-dialog))))

      (let ((child (|XtCreateManagedWidget "Transient mangler" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-transient-dialog))))

;;; Valve (tube) saturation
;;;

(define valve-distortion-level .5)
(define valve-distortion-character .5)
(define valve-saturation-dialog #f)
(define valve-saturation-label "Valve (tube) saturation")

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-valve-saturation-dialog)
       (if (not (|Widget? valve-saturation-dialog))
           ;; if valve-saturation-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! valve-saturation-dialog
                   (make-effect-dialog "valve saturation ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "valve_1209" "valve" valve-distortion-level valve-distortion-character)
                                                       (- (frames) (cursor))
                                                       "valve saturation"))
                                       (lambda (w context info) (|XtUnmanageChild valve-saturation-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Valve (tube) saturation"
                                                      "A model of valve (tube) distortion, lacking some of the harmonics you would get in a real tube amp, but sounds good nonetheless.\n\ Distortion level: How hard the signal is driven against the limit of the amplifier.\n\ Distortion character: The hardness of the sound, low for softer, high for harder."))
                                       (lambda (w c i)
                                         (set! valve-distortion-level .5)
                                         (|XtSetValues (list-ref sliders 0)
                                            (list |XmNvalue (inexact->exact (* valve-distortion-level 100))))
				         (set! valve-distortion-character .5)
				         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* valve-distortion-character 100)))))))
             (set! sliders
                   (add-sliders valve-saturation-dialog
                                (list (list "distortion level" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! valve-distortion-level (/ (|value info) 100.0)))
                                            100)
                                      (list "distortion character" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! valve-distortion-character (/ (|value info) 100.0)))
                                            100))))))
       (activate-dialog valve-saturation-dialog))))

      (let ((child (|XtCreateManagedWidget "Valve saturation" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-valve-saturation-dialog))))

;;; Wave shaper
;;;

(define waveshaper-waveshape 0.0)
(define waveshaper-dialog #f)
(define waveshaper-label "Wave shaper")

(define (cp-waveshaper)
  (apply-ladspa (make-sample-reader (cursor))
                (list "shaper_1187" "shaper" waveshaper-waveshape)
                (- (frames) (cursor))
                "wave shaper"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-waveshaper-dialog)
       (if (not (|Widget? waveshaper-dialog))
           ;; if waveshaper-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! waveshaper-dialog
                   (make-effect-dialog "wave shaper ladspa plugin"
                                       (lambda (w context info) (cp-waveshaper))
                                       (lambda (w context info) (|XtUnmanageChild waveshaper-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Wave shaper"
                                                      "Reshapes the wave by an exponential function. Inspiration was taken from the Nord module of the same name.\n\ If you are getting rubbish out then it's probably because the host isn't using the input/output range hints, which are very important for this plugin.\n\ Waveshape: Positive values have an expanding effect, negative values have a compressing effect."))
                                       (lambda (w c i)
                                         (set! waveshaper-waveshape 0.0)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* waveshaper-waveshape 100)))))))
             (set! sliders
                   (add-sliders waveshaper-dialog
                                (list (list "waveshape" -10.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! waveshaper-waveshape (/ (|value info) 100)))
                                            100))))))
       (activate-dialog waveshaper-dialog))))

      (let ((child (|XtCreateManagedWidget "Wave shaper" |xmPushButtonWidgetClass ladspa-distort-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-waveshaper-dialog))))



;;; FILTERS AND EQUALIZERS
;;;

(define ladspa-filter-menu (|XmCreatePulldownMenu plug-menu "Filters/EQs"
                                        (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))
(define ladspa-filter-cascade (|XtCreateManagedWidget "Filters/EQs" |xmCascadeButtonWidgetClass plug-menu
                                            (list |XmNsubMenuId ladspa-filter-menu
                                                  |XmNbackground (|Pixel (snd-pixel (basic-color))))))


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
       (if (not (|Widget? comb-filter-dialog))
           (let ((sliders '()))
             (set! comb-filter-dialog
                   (make-effect-dialog "comb filter ladspa plugin"
                                       (lambda (w context info) (cp-comb-filter))
                                       (lambda (w context info) (|XtUnmanageChild comb-filter-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Comb filter"
                                                      "Band separation controls the distance between the filter's peaks.\n\ Feedback level increases the distinctive wooshy phaser sound."))
                                       (lambda (w c i)
                                         (set! comb-filter-band-separation 100)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* comb-filter-band-separation 1))))
                                         (set! comb-filter-feedback 0.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* comb-filter-feedback 100)))))))
             (set! sliders
                   (add-sliders comb-filter-dialog
                                (list 
                                      (list "band-separation (Hz)" 16 100 640
                                            (lambda (w context info)
                                              (set! comb-filter-band-separation (/ (|value info) 1)))
                                            1)
 				      (list "feedback" -0.99 0.0 0.99
                                            (lambda (w context info)
                                              (set! comb-filter-feedback (/ (|value info) 100)))
                                            100))))))
       (activate-dialog comb-filter-dialog))))

      (let ((child (|XtCreateManagedWidget "Comb filter" |xmPushButtonWidgetClass ladspa-filter-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-comb-filter-dialog))))

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
       (if (not (|Widget? svf-dialog))
           (let ((sliders '()))
             (set! svf-dialog
                   (make-effect-dialog "state variable filter ladspa plugin"
                                       (lambda (w context info) (cp-svf))
                                       (lambda (w context info) (|XtUnmanageChild svf-dialog))
                                       (lambda (w context info)
                                         (help-dialog "State variable filter"
                                                      "An oversampled state variable filter with a few tweaks. Quite nice, tends to be unstable with high resonance and Q values, but good when kept under control.\n\ Filter type: (0=none, 1=LP, 2=HP, 3=BP, 4=BR, 5=AP) Select between no filtering, low-pass, high-pass, band-pass, band-reject and all-pass.\n\ Filter frequency: Cutoff frequency, beware of high values with low sample rates.\n\ Filter Q: The filter's cutoff slope.\n\ Filter resonance: The filter's resonance is sort of separate from Q but very related (implemented with feedback). Do not use with the bandpass mode."))
                                       (lambda (w c i)
                                         (set! svf-type 0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* svf-type 1))))
                                         (set! svf-frequency 100)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* svf-frequency 1))))
                                         (set! svf-q 0.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* svf-q 100))))
                                         (set! svf-resonance 0.0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* svf-resonance 100)))))))
             (set! sliders
                   (add-sliders svf-dialog
                                (list
                                      (list "filter type" 0 0 5
                                            (lambda (w context info)
                                              (set! svf-type (/ (|value info) 1)))
                                            1)
                                      (list "filter frequency" 0 100 6000
                                            (lambda (w context info)
                                              (set! svf-frequency (/ (|value info) 1)))
                                            1)
                                      (list "filter Q" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! svf-q (/ (|value info) 100)))
                                            100)
                                      (list "filter resonance" 0.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! svf-resonance (/ (|value info) 100)))
                                            100))))))
       (activate-dialog svf-dialog))))

      (let ((child (|XtCreateManagedWidget "State variable filter" |xmPushButtonWidgetClass ladspa-filter-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? vcf-303-dialog))
           (let ((sliders '()))
             (set! vcf-303-dialog
                   (make-effect-dialog "vcf-303 ladspa plugin"
                                       (lambda (w context info) (cp-vcf-303))
                                       (lambda (w context info) (|XtUnmanageChild vcf-303-dialog))
                                       (lambda (w context info)
                                         (help-dialog "VCF-303 Help"
                                                      "Move the sliders to set the filter parameters."))
                                       (lambda (w c i)
                                         (set! vcf-303-cutoff .5)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* vcf-303-cutoff 100))))
                                         (set! vcf-303-resonance .5)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* vcf-303-resonance 100))))
                                         (set! vcf-303-env-mod .5)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* vcf-303-env-mod 100))))
                                         (set! vcf-303-decay .5)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* vcf-303-decay 100)))))))
             (set! sliders
                   (add-sliders vcf-303-dialog
                                (list (list "cutoff" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-cutoff (/ (|value info) 100)))
                                            100)
                                      (list "resonance" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-resonance (/ (|value info) 100)))
                                            100)
 				      (list "envelope modulation" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-env-mod (/ (|value info) 100)))
                                            100)
                                      (list "decay" 0 .5 1
                                            (lambda (w context info)
                                              (set! vcf-303-decay (/ (|value info) 100)))
                                            100))))
             (let* ((s1 (|XmStringCreateLocalized "Trigger"))
                     (toggle
                      (|XtCreateManagedWidget "Trigger" |xmToggleButtonWidgetClass (|XtParent (car sliders))
                        (list |XmNselectColor  (|Pixel (snd-pixel (pushed-button-color)))
                              |XmNbackground   (|Pixel (snd-pixel (basic-color)))
                              |XmNvalue        vcf-303-trigger
                              ;|XmNborderWidth  1
                              |XmNlabelString  s1))))
                (|XmStringFree s1)
                (|XtAddCallback toggle |XmNvalueChangedCallback (lambda (w c i) (set! vcf-303-trigger (if (|set i) 1 0))))))

       (activate-dialog vcf-303-dialog)))))

      (let ((child (|XtCreateManagedWidget "VCF303" |xmPushButtonWidgetClass ladspa-filter-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? multiband-eq-dialog))
           (let ((sliders '()))
             (set! multiband-eq-dialog
                   (make-effect-dialog "multiband eq ladspa plugin"
                                       (lambda (w context info) (cp-mbeq))
                                       (lambda (w context info) (|XtUnmanageChild multiband-eq-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Multiband equalizer"
                                                      "This is a fairly typical multiband graphic equalizer. It's implemented using a FFT, so it takes quite a lot of CPU power, but it should have less phase effect than an equivalent filter implementation. If the input signal is at too low a sample rate then the top bands will be ignored, i.e., the highest useful band will always be a high shelf."))
                                       (lambda (w c i)
                                         (set! multiband-eq-50hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-50hz-gain 1))))
                                         (set! multiband-eq-100hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-100hz-gain 1))))
                                         (set! multiband-eq-156hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-156hz-gain 1))))
                                         (set! multiband-eq-220hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-220hz-gain 1))))
                                         (set! multiband-eq-311hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-311hz-gain 1))))
                                         (set! multiband-eq-440hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-440hz-gain 1))))
                                         (set! multiband-eq-622hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-622hz-gain 1))))
                                         (set! multiband-eq-880hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-880hz-gain 1))))
                                         (set! multiband-eq-1250hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-1250hz-gain 1))))
                                         (set! multiband-eq-1750hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-1750hz-gain 1))))
                                         (set! multiband-eq-2500hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-2500hz-gain 1))))
                                         (set! multiband-eq-3500hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-3500hz-gain 1))))
                                         (set! multiband-eq-5000hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-5000hz-gain 1))))
                                         (set! multiband-eq-10000hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-10000hz-gain 1))))
                                         (set! multiband-eq-20000hz-gain 0)
                                         (|XtSetValues (list-ref sliders ) (list |XmNvalue (inexact->exact (* multiband-eq-20000hz-gain 1)))))))
             (set! sliders
                   (add-sliders multiband-eq-dialog
                                (list 
				      (list "50 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-50hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "100 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-100hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "156 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-156hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "220 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-220hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "311 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-311hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "440 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-440hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "622 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-622hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "880 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-880hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "1250 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-1250hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "1750 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-1750hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "2500 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-2500hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "3500 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-3500hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "5000 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-5000hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "10000 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-10000hz-gain (/ (|value info) 1)))
                                            1)
                                      (list "20000 Hz gain" -70 0 30
                                            (lambda (w context info)
                                              (set! multiband-eq-20000hz-gain (/ (|value info) 1)))
                                            1))))))
       (activate-dialog multiband-eq-dialog))))

      (let ((child (|XtCreateManagedWidget "Multiband equalizer" |xmPushButtonWidgetClass ladspa-filter-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-multiband-eq-dialog))))


;;; DYNAMICS PROCESSORS
;;;

(define ladspa-amp-menu (|XmCreatePulldownMenu plug-menu "Dynamics Processors"
                                        (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))
(define ladspa-amp-cascade (|XtCreateManagedWidget "Dynamics Processors" |xmCascadeButtonWidgetClass plug-menu
                                            (list |XmNsubMenuId ladspa-amp-menu
                                                  |XmNbackground (|Pixel (snd-pixel (basic-color))))))

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
       (if (not (|Widget? compress-peak-dialog))
           (let ((sliders '()))
             (set! compress-peak-dialog
                   (make-effect-dialog "compressor (peak) ladspa plugin"
                                       (lambda (w context info) (cp-compress-peak))
                                       (lambda (w context info) (|XtUnmanageChild compress-peak-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Compressor (peak tracking)"
                                                      "Move the sliders to set the compressor parameters."))
                                       (lambda (w c i)
                                         (set! compress-peak-threshold 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* compress-peak-threshold 100))))
                                         (set! compress-peak-ratio 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* compress-peak-ratio 100))))
                                         (set! compress-peak-envelope-attack 1.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* compress-peak-envelope-attack 100))))
                                         (set! compress-peak-envelope-decay 1.0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* compress-peak-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders compress-peak-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! compress-peak-threshold (/ (|value info) 100)))
                                            100)
 				      (list "compression ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! compress-peak-ratio (/ (|value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-peak-envelope-attack (/ (|value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-peak-envelope-decay (/ (|value info) 100)))
                                            100))))))
       (activate-dialog compress-peak-dialog))))

      (let ((child (|XtCreateManagedWidget "Compressor (peak tracking)" |xmPushButtonWidgetClass ladspa-amp-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? compress-rms-dialog))
           (let ((sliders '()))
             (set! compress-rms-dialog
                   (make-effect-dialog "compressor (rms) ladspa plugin"
                                       (lambda (w context info) (cp-compress-rms))
                                       (lambda (w context info) (|XtUnmanageChild compress-rms-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Compressor (rms tracking)"
                                                      "Move the sliders to set the compressor parameters."))
                                       (lambda (w c i)
                                         (set! compress-rms-threshold 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* compress-rms-threshold 100))))
                                         (set! compress-rms-ratio 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* compress-rms-ratio 100))))
                                         (set! compress-rms-envelope-attack 1.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* compress-rms-envelope-attack 100))))
                                         (set! compress-rms-envelope-decay 1.0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* compress-rms-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders compress-rms-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! compress-rms-threshold (/ (|value info) 100)))
                                            100)
 				      (list "compression ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! compress-rms-ratio (/ (|value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-rms-envelope-attack (/ (|value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! compress-rms-envelope-decay (/ (|value info) 100)))
                                            100))))))
       (activate-dialog compress-rms-dialog))))

      (let ((child (|XtCreateManagedWidget "Compressor (rms tracking)" |xmPushButtonWidgetClass ladspa-amp-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-compress-rms-dialog))))


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
       (if (not (|Widget? expand-peak-dialog))
           (let ((sliders '()))
             (set! expand-peak-dialog
                   (make-effect-dialog "expander (peak) ladspa plugin"
                                       (lambda (w context info) (cp-expand-peak))
                                       (lambda (w context info) (|XtUnmanageChild expand-peak-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Expander (peak tracking)"
                                                      "Move the sliders to set the expander parameters."))
                                       (lambda (w c i)
                                         (set! expand-peak-threshold 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* expand-peak-threshold 100))))
                                         (set! expand-peak-ratio 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* expand-peak-ratio 100))))
                                         (set! expand-peak-envelope-attack 1.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* expand-peak-envelope-attack 100))))
                                         (set! expand-peak-envelope-decay 1.0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* expand-peak-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders expand-peak-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! expand-peak-threshold (/ (|value info) 100)))
                                            100)
 				      (list "expansion ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! expand-peak-ratio (/ (|value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-peak-envelope-attack (/ (|value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-peak-envelope-decay (/ (|value info) 100)))
                                            100))))))
       (activate-dialog expand-peak-dialog))))

      (let ((child (|XtCreateManagedWidget "Expander (peak tracking)" |xmPushButtonWidgetClass ladspa-amp-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? expand-rms-dialog))
           (let ((sliders '()))
             (set! expand-rms-dialog
                   (make-effect-dialog "expander (rms) ladspa plugin"
                                       (lambda (w context info) (cp-expand-rms))
                                       (lambda (w context info) (|XtUnmanageChild expand-rms-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Expander (rms tracking)"
                                                      "Move the sliders to set the expander parameters."))
                                       (lambda (w c i)
                                         (set! expand-rms-threshold 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* expand-rms-threshold 100))))
                                         (set! expand-rms-ratio 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* expand-rms-ratio 100))))
                                         (set! expand-rms-envelope-attack 1.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* expand-rms-envelope-attack 100))))
                                         (set! expand-rms-envelope-decay 1.0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* expand-rms-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders expand-rms-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! expand-rms-threshold (/ (|value info) 100)))
                                            100)
 				      (list "expansion ratio" 0.0 1.0 1.0
                                            (lambda (w context info)
                                              (set! expand-rms-ratio (/ (|value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-rms-envelope-attack (/ (|value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! expand-rms-envelope-decay (/ (|value info) 100)))
                                            100))))))
       (activate-dialog expand-rms-dialog))))

      (let ((child (|XtCreateManagedWidget "Expander (rms tracking)" |xmPushButtonWidgetClass ladspa-amp-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? limit-peak-dialog))
           (let ((sliders '()))
             (set! limit-peak-dialog
                   (make-effect-dialog "limiter (peak) ladspa plugin"
                                       (lambda (w context info) (cp-limit-peak))
                                       (lambda (w context info) (|XtUnmanageChild limit-peak-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Limiter (peak tracking)"
                                                      "Move the sliders to set the limiter parameters."))
                                       (lambda (w c i)
                                         (set! limit-peak-threshold 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* limit-peak-threshold 100))))
                                         (set! limit-peak-envelope-attack 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* limit-peak-envelope-attack 100))))
                                         (set! limit-peak-envelope-decay 1.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* limit-peak-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders limit-peak-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! limit-peak-threshold (/ (|value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-peak-envelope-attack (/ (|value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-peak-envelope-decay (/ (|value info) 100)))
                                            100))))))
       (activate-dialog limit-peak-dialog))))

      (let ((child (|XtCreateManagedWidget "Limiter (peak tracking)" |xmPushButtonWidgetClass ladspa-amp-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? limit-rms-dialog))
           (let ((sliders '()))
             (set! limit-rms-dialog
                   (make-effect-dialog "limiter (rms) ladspa plugin"
                                       (lambda (w context info) (cp-limit-rms))
                                       (lambda (w context info) (|XtUnmanageChild limit-rms-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Limiter (rms tracking)"
                                                      "Move the sliders to set the limiter parameters."))
                                       (lambda (w c i)
                                         (set! limit-rms-threshold 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* limit-rms-threshold 100))))
                                         (set! limit-rms-envelope-attack 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* limit-rms-envelope-attack 100))))
                                         (set! limit-rms-envelope-decay 1.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* limit-rms-envelope-decay 100)))))))
             (set! sliders
                   (add-sliders limit-rms-dialog
                                (list 
                                      (list "threshold" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! limit-rms-threshold (/ (|value info) 100)))
                                            100)
				      (list "output envelope attack (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-rms-envelope-attack (/ (|value info) 100)))
                                            100)
                                      (list "output envelope decay (s)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! limit-rms-envelope-decay (/ (|value info) 100)))
                                            100))))))
       (activate-dialog limit-rms-dialog))))

      (let ((child (|XtCreateManagedWidget "Limiter (rms tracking)" |xmPushButtonWidgetClass ladspa-amp-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-limit-rms-dialog))))





;;; FREQUENCY EFFECTS
;;;

(define ladspa-freq-menu (|XmCreatePulldownMenu plug-menu "Frequency Effects"
                                        (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))
(define ladspa-freq-cascade (|XtCreateManagedWidget "Frequency Effects" |xmCascadeButtonWidgetClass plug-menu
                                            (list |XmNsubMenuId ladspa-freq-menu
                                                  |XmNbackground (|Pixel (snd-pixel (basic-color))))))

;;; Harmonic generator
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

(define (cp-hg)
   (apply-ladspa (make-sample-reader (cursor))
      (list "harmonic_gen_1220" "harmonicGen" hg-fundamental-magnitude hg-harmonic-magnitude-2 hg-harmonic-magnitude-3 hg-harmonic-magnitude-4 hg-harmonic-magnitude-5 hg-harmonic-magnitude-6 hg-harmonic-magnitude-7 hg-harmonic-magnitude-8 hg-harmonic-magnitude-9 hg-harmonic-magnitude-10)
         (- (frames) (cursor))
         "harmonic generator"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-hg-dialog)
       (if (not (|Widget? hg-dialog))
           (let ((sliders '()))
             (set! hg-dialog
                   (make-effect-dialog "harmonic generator ladspa plugin"
                                       (lambda (w context info) (cp-hg))
                                       (lambda (w context info) (|XtUnmanageChild hg-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Harmonic generator" 
						"Allows you to add harmonics and remove the fundamental from any audio signal.\n\ Known bugs: There is no bandwidth limiting filter on the output, so it is easy to create excessive high-frequency harmonics that could cause aliasing problems. However, in practice this doesn't seem to be a serious problem.\n\ Examples: There are many interesting effects you can achieve with sine waves. One example is producing band-limited square waves from sine waves. To do this, set the parameters to 1, 0, -0.3333, 0, 0.2, 0, -0.14285, 0, 0.11111. To get a triangle-like signal use 1, 0, -0.3333, 0, -0.2, 0, -0.14285, 0, -0.11111.\n\ Fundamental magnitude: The amplitude of the fundamental of the signal. Reduce it to 0 to remove the base signal altogether, or -1 to phase-invert it.\n\ 2nd harmonic magnitude: The 2nd harmonic, its frequency is twice the frequency of the fundamental.\n\ 3rd harmonic magnitude: The 3rd harmonic, its frequency is three times the frequency of the fundamental.\n\ Even harmonics add a distorted feel to the sound. Valve (tube) amplifiers introduce distortions at all the harmonics, transistor amplifiers only introduce distortion into the odd harmonics."))
                                       (lambda (w c i)
                                         (set! hg-fundamental-magnitude 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* hg-fundamental-magnitude 100))))
                                         (set! hg-harmonic-magnitude-2 0.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-2 100))))
                                         (set! hg-harmonic-magnitude-3 0.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-3 100))))
                                         (set! hg-harmonic-magnitude-4 0.0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-4 100))))
                                         (set! hg-harmonic-magnitude-5 0.0)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-5 100))))
                                         (set! hg-harmonic-magnitude-6 0.0)
                                         (|XtSetValues (list-ref sliders 5) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-6 100))))
                                         (set! hg-harmonic-magnitude-7 0.0)
                                         (|XtSetValues (list-ref sliders 6) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-7 100))))
                                         (set! hg-harmonic-magnitude-8 0.0)
                                         (|XtSetValues (list-ref sliders 7) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-8 100))))
                                         (set! hg-harmonic-magnitude-9 0.0)
                                         (|XtSetValues (list-ref sliders 8) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-9 100))))
                                         (set! hg-harmonic-magnitude-10 0.0)
                                         (|XtSetValues (list-ref sliders 9) (list |XmNvalue (inexact->exact (* hg-harmonic-magnitude-10 100)))))))
             (set! sliders
                   (add-sliders hg-dialog
                                (list 
				      (list "fundamental magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-fundamental-magnitude (/ (|value info) 100)))
                                            100)
                                      (list "2nd harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-2 (/ (|value info) 100)))
                                            100)
                                      (list "3rd harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-3 (/ (|value info) 100)))
                                            100)
                                      (list "4th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-4 (/ (|value info) 100)))
                                            100)
                                      (list "5th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-5 (/ (|value info) 100)))
                                            100)
                                      (list "6th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-6 (/ (|value info) 100)))
                                            100)
                                      (list "7th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-7 (/ (|value info) 100)))
                                            100)
                                      (list "8th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-8 (/ (|value info) 100)))
                                            100)
                                      (list "9th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-9 (/ (|value info) 100)))
                                            100)
                                      (list "10th harmonic magnitude" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! hg-harmonic-magnitude-10 (/ (|value info) 100)))
                                            100))))))
       (activate-dialog hg-dialog))))

      (let ((child (|XtCreateManagedWidget "Harmonic generator" |xmPushButtonWidgetClass ladspa-freq-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-hg-dialog))))


;;; Pitch scaler
;;;

(define pitch-coefficient 1.0)
(define pitch-scale-dialog #f)
(define pitch-scale-label "Pitch scaler")

(define (cp-pitch-scale)
  (apply-ladspa (make-sample-reader (cursor))
                (list "pitch_scale_1194" "pitchScaleHQ" pitch-coefficient)
                (- (frames) (cursor))
                "pitch-scale"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-pitch-scale-dialog)
       (if (not (|Widget? pitch-scale-dialog))
           ;; if pitch-scale-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! pitch-scale-dialog
                   (make-effect-dialog "pitch-scale (high-quality) ladspa plugin"
                                       (lambda (w context info) (cp-pitch-scale))
                                       (lambda (w context info) (|XtUnmanageChild pitch-scale-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Pitch scaler"
                                                      "A pitch shifter implementation that scales the harmonics appropriately with the base frequencies.\n\ It is an implementation of Stephen M. Sprengler's pitch scaler design. It gives reasonable, general purpose results for small changes, but won't give Antares or Eventide anything to worry about.\n\ The FFT block size and oversampling has been kept at reasonable levels to keep the CPU usage low.\n\ Pitch coefficient: The pitch scaling factor, a value of 2.0 will increase the pitch by one octave, etc."))
                                       (lambda (w c i)
                                         (set! pitch-coefficient 1.0)
                                         (|XtSetValues (car sliders)
                                            (list |XmNvalue semi-range)))))
             (set! sliders
                   (add-sliders pitch-scale-dialog
                                (list (list "pitch coefficient" 0.25 1.0 4.0
                                            (lambda (w context info)
                                              (set! pitch-coefficient (semitones->ratio (- (|value info) semi-range))))
                                            100 'semi))))))
       (activate-dialog pitch-scale-dialog))))

      (let ((child (|XtCreateManagedWidget "Pitch scaler" |xmPushButtonWidgetClass ladspa-freq-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-pitch-scale-dialog))))

;;; Modulation effects
;;;

(define ladspa-mod-menu (|XmCreatePulldownMenu plug-menu "Modulation Effects"
                                        (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))
(define ladspa-mod-cascade (|XtCreateManagedWidget "Modulation Effects" |xmCascadeButtonWidgetClass plug-menu
                                            (list |XmNsubMenuId ladspa-mod-menu
                                                  |XmNbackground (|Pixel (snd-pixel (basic-color))))))


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
       (if (not (|Widget? mvChorus-dialog))
           ;; if mvChorus-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! mvChorus-dialog
                   (make-effect-dialog "multivoice chorus ladspa plugin"
                                       (lambda (w context info) (cp-mvChorus))
                                       (lambda (w context info) (|XtUnmanageChild mvChorus-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Multivoice chorus"
                                                      "This is an implementation of a Multivoice (as opposed to Multiscale) chorus algorithm. It uses a novel sinc-based noise interpolation method to produce a subtle modulation law, which makes it possible to get away with larger numbers of voices without the metallic, artificial sound common in chorus effects.\n\ Voice separation (ms): The individual voices can either be running at the same base delay (set this to zero) or staggered. Setting this to non-zero values can make the output sound richer, but will make it sound grainy with some type of signals.\n\ Detune (%): The maximum amount that a voice will be detuned by. A value of 1 is recommended, but you may be able to get away with higher values if the signal is less harmonic.\n\ LFO frequency (Hz): The frequency that the detune effect will be modulated at. A matter of taste, for most types of input a lower value will be more subtle.\n\ Output attenuation (dB): With large numbers of voices the output can become too high, so use this to trim the amplitude to a more helpful level."))
                                       (lambda (w c i)
                                         (set! mvChorus-voices 3)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* mvChorus-voices 1))))
                                         (set! mvChorus-delay 10)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* mvChorus-delay 1))))
                                         (set! mvChorus-separation 1)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* mvChorus-separation 10))))
                                         (set! mvChorus-detune 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* mvChorus-detune 1))))
                                         (set! mvChorus-lfo 10)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* mvChorus-lfo 1))))
         				 (set! mvChorus-attenuation 0)
				         (|XtSetValues (list-ref sliders 5) (list |XmNvalue (inexact->exact (* mvChorus-attenuation 1)))))))+
             (set! sliders
                   (add-sliders mvChorus-dialog
                                (list (list "voices" 1 3 8
                                            (lambda (w context info)
                                              (set! mvChorus-voices (/ (|value info) 1)))
                                            1)
                                      (list "delay time (ms)" 10 10 40
                                            (lambda (w context info)
                                              (set! mvChorus-delay (/ (|value info) 1)))
                                            1)
 				      (list "voice separation (ms)" 0 1 2
                                            (lambda (w context info)
                                              (set! mvChorus-separation (/ (|value info) 10)))
                                            10)
                                      (list "detune (%)" 0 0 5
                                            (lambda (w context info)
                                              (set! mvChorus-detune (/ (|value info) 1)))
                                            1)
                                      (list "LFO frequency (Hz)" 2 10 30
                                            (lambda (w context info)
                                              (set! mvChorus-lfo (/ (|value info) 1)))
                                            1)
                                      (list "output attenuation (db)" -20 0 0
                                            (lambda (w context info)
                                              (set! mvChorus-attenuation (/ (|value info) 1)))
                                            1))))))
       (activate-dialog mvChorus-dialog))))

      (let ((child (|XtCreateManagedWidget "Chorus (multivoice)" |xmPushButtonWidgetClass ladspa-mod-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-mvChorus-dialog))))


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
       (if (not (|Widget? sh-flanger-dialog))
           (let ((sliders '()))
             (set! sh-flanger-dialog
                   (make-effect-dialog "flanger ladspa plugin"
                                       (lambda (w context info) (cp-flanger))
                                       (lambda (w context info) (|XtUnmanageChild sh-flanger-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Flanger"
                                                      "A digital flanger implementation. Uses a novel zero excursion and a controlled bandwidth modulation function that should make the modulation less repetitive and noticeable. This effect is similar in character to a phaser, the main difference is that a phaser sounds more regular and stable.\n\ Delay base (ms): This is the offset from the input time that the detuned delay moves around. 10 is probably a good starting value.\n\ Max slowdown (ms): This is the maximum delay that will be applied to the delayed signal, relative to the dry signal.\n\ LFO frequency (Hz): This is the core frequency that the 'LFO' will move at. The LFO isn't actually an oscillator, but it does change periodically.\n\ Feedback: Feedback applied from the output to the input. Increases the depth of the effect, but makes it sound less like a real flanger."))
                                       (lambda (w c i)
                                         (set! sh-flanger-delay-base 1.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* sh-flanger-delay-base 100))))
                                         (set! sh-flanger-max-slowdown 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* sh-flanger-max-slowdown 100))))
                                         (set! sh-flanger-lfo-frequency 10.0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* sh-flanger-lfo-frequency 100))))
                                         (set! sh-flanger-feedback 0.0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* sh-flanger-feedback 100)))))))
             (set! sliders
                   (add-sliders sh-flanger-dialog
                                (list (list "delay base (ms)" 0.1 1.0 25.0
                                            (lambda (w context info)
                                              (set! sh-flanger-delay-base (/ (|value info) 100)))
                                            100)
                                      (list "max slowdown (ms)" 0.0 1.0 10.0
                                            (lambda (w context info)
                                              (set! sh-flanger-max-slowdown (/ (|value info) 100)))
                                            100)
 				      (list "LFO frequency (Hz)" 0.5 10.0 100.0
                                            (lambda (w context info)
                                              (set! sh-flanger-lfo-frequency (/ (|value info) 100)))
                                            100)
                                      (list "feedback" -1.0 0.0 1.0
                                            (lambda (w context info)
                                              (set! sh-flanger-feedback (/ (|value info) 100)))
                                            100))))))
       (activate-dialog sh-flanger-dialog))))

      (let ((child (|XtCreateManagedWidget "Flanger" |xmPushButtonWidgetClass ladspa-mod-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? lfo-phaser-dialog))
           ;; if lfo-phaser-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! lfo-phaser-dialog
                   (make-effect-dialog "lfo-phaser ladspa plugin"
                                       (lambda (w context info) (cp-lfo-phaser))
                                       (lambda (w context info) (|XtUnmanageChild lfo-phaser-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Phaser (LFO)"
                                                      "Move sliders to adjust phaser parameters."))
                                       (lambda (w c i)
				         (set! lfo-phaser-rate 10) 
					 (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* lfo-phaser-rate 1))))
                                         (set! lfo-phaser-depth .5) 
					 (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* lfo-phaser-depth 100))))
                                         (set! lfo-phaser-feedback .5)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* lfo-phaser-feedback 100))))
                                         (set! lfo-phaser-spread 1)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* lfo-phaser-spread 1)))))))
             (set! sliders
                   (add-sliders lfo-phaser-dialog
                                (list 
                                      (list "LFO rate (Hz)" 1 10 100
                                            (lambda (w context info)
                                              (set! lfo-phaser-rate (/ (|value info) 1)))
                                            1)
				      (list "LFO depth" 0.0 .5 1.0
                                            (lambda (w context info)
                                              (set! lfo-phaser-depth (/ (|value info) 100)))
                                            100)
                                      (list "feedback" -1.0 .5 1.0
                                            (lambda (w context info)
                                              (set! lfo-phaser-feedback (/ (|value info) 100)))
                                            100)
                                      (list "spread" 0 1 2
                                            (lambda (w context info)
                                              (set! lfo-phaser-spread (/ (|value info) 1)))
                                            1))))))
       (activate-dialog lfo-phaser-dialog))))

      (let ((child (|XtCreateManagedWidget "Phaser (LFO)" |xmPushButtonWidgetClass ladspa-mod-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? retro-flange-dialog))
           (let ((sliders '()))
             (set! retro-flange-dialog
                   (make-effect-dialog "retro flanger ladspa plugin"
                                       (lambda (w context info) (cp-retro-flange))
                                       (lambda (w context info) (|XtUnmanageChild retro-flange-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Retro flanger"
                                                      "A model of someone flanging the input. Models the tape saturation effects and frequency smear. The smear could probably be done better.\n\ Average stall (ms): The average time difference between the two tapes, per stall.\n\ Flange frequency (Hz): The rate the tape is stalled at."))
                                       (lambda (w c i)
                                         (set! retro-flange-stall 0.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* retro-flange-stall 100))))
                                         (set! retro-flange-frequency 1.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* retro-flange-frequency 100)))))))
             (set! sliders
                   (add-sliders retro-flange-dialog
                                (list 
                                      (list "average stall (ms)" 0.0 0.0 10.0
                                            (lambda (w context info)
                                              (set! retro-flange-stall (/ (|value info) 100)))
                                            100)
 				      (list "flange frequency (Hz)" 0.5 1.0 8.0
                                            (lambda (w context info)
                                              (set! retro-flange-frequency (/ (|value info) 100)))
                                            100))))))
       (activate-dialog retro-flange-dialog))))

      (let ((child (|XtCreateManagedWidget "Retro flange" |xmPushButtonWidgetClass ladspa-mod-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
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
       (if (not (|Widget? ringmod-dialog))
           ;; if ringmod-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! ringmod-dialog
                   (make-effect-dialog "ringmod ladspa plugin"
                                       (lambda (w context info) (cp-ringmod))
                                       (lambda (w context info) (|XtUnmanageChild ringmod-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Ring modulation Help"
                                                      "Ring or amplitude modulation with LFO.\n\ Move the sliders to set the ring modulation parameters.\n\ Modulation depth key: 0=none 1=AM 2=RM"))
                                       (lambda (w c i)
                                         (set! ringmod-modulation-depth 0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* ringmod-modulation-depth 1))))
				         (set! ringmod-frequency 100)
				         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* ringmod-frequency 1))))
                                         (set! ringmod-sine-level 0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* ringmod-sine-level 100))))
                                         (set! ringmod-triangle-level 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* ringmod-triangle-level 100))))
                                         (set! ringmod-sawtooth-level 0)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* ringmod-sawtooth-level 100))))
                                         (set! ringmod-square-level 0)
                                         (|XtSetValues (list-ref sliders 5) (list |XmNvalue (inexact->exact (* ringmod-square-level 100)))))))
             (set! sliders
                   (add-sliders ringmod-dialog
                                (list (list "modulation depth" 0 0 2
                                            (lambda (w context info)
                                              (set! ringmod-modulation-depth (/ (|value info) 1)))
                                            1)
                                      (list "frequency (Hz)" 1 100 1000
                                            (lambda (w context info)
                                              (set! ringmod-frequency (/ (|value info) 1)))
                                            1)
                                      (list "sine level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-sine-level (/ (|value info) 100)))
                                            100)
                                      (list "triangle level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-triangle-level (/ (|value info) 100)))
                                            100)
                                      (list "sawtooth level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-sawtooth-level (/ (|value info) 100)))
                                            100)
                                      (list "square level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-square-level (/ (|value info) 100)))
                                            100))))))
       (activate-dialog ringmod-dialog))))


      (let ((child (|XtCreateManagedWidget "Ring modulation" |xmPushButtonWidgetClass ladspa-mod-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-ringmod-dialog))))

;;; REVERBS
;;;

(define ladspa-reverb-menu (|XmCreatePulldownMenu plug-menu "Reverbs"
                                        (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))
(define ladspa-reverb-cascade (|XtCreateManagedWidget "Reverbs" |xmCascadeButtonWidgetClass plug-menu
                                            (list |XmNsubMenuId ladspa-reverb-menu
                                                  |XmNbackground (|Pixel (snd-pixel (basic-color))))))

;;; Freeverb3
;;;

(define freeverb-room-size .5)
(define freeverb-damping .5)
(define freeverb-wet-level .5)
(define freeverb-dry-level .5)
(define freeverb-width .5)
(define freeverb-freeze-mode 0)
(define freeverb-dialog #f)
(define freeverb-label "Freeverb3 (stereo)")

(define (cp-freeverb3)
  (let* ((snd (selected-sound))
         (chns (channels snd))
         (readers (if (= chns 1)
                  (make-sample-reader 0 snd 0)
                  ;; assume stereo -- this could collect an arbitrary list
                  (list (make-sample-reader 0 snd 0)
                        (make-sample-reader 0 snd 1)))))
  (apply-ladspa readers
    (list "cmt" "freeverb3" freeverb-freeze-mode freeverb-room-size freeverb-damping freeverb-wet-level freeverb-dry-level freeverb-width)
    (- (frames) (cursor))
    "freeverb")))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

     (define (post-freeverb3-dialog)
       (if (not (|Widget? freeverb-dialog))
           (let ((sliders '()))
             (set! freeverb-dialog
                   (make-effect-dialog "freeverb ladspa plugin"
                                       (lambda (w context info) (cp-freeverb3))
                                       (lambda (w context info) (|XtUnmanageChild freeverb-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Freeverb3 Help"
                                                      "Jezar's famous reverb. Move the sliders to set the reverb parameters.\n\ This effect works only with stereo soundfiles!"))
                                       (lambda (w c i)
                                         (set! freeverb-room-size .5)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* freeverb-room-size 100))))
                                         (set! freeverb-damping .5)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* freeverb-damping 100))))
                                         (set! freeverb-wet-level .5)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* freeverb-wet-level 100))))
                                         (set! freeverb-dry-level .5)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* freeverb-dry-level 100))))
                                         (set! freeverb-width .5)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* freeverb-width 100)))))))
             (set! sliders
                   (add-sliders freeverb-dialog
                                (list (list "room size" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-room-size (/ (|value info) 100)))
                                            100)
                                      (list "damping" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-damping (/ (|value info) 100)))
                                            100)
 				      (list "wet level" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-wet-level (/ (|value info) 100)))
                                            100)
                                      (list "dry level" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-dry-level (/ (|value info) 100)))
                                            100)
                                      (list "width" 0 .5 1
                                            (lambda (w context info)
                                              (set! freeverb-width (/ (|value info) 100)))
                                            100))))
             (let* ((s1 (|XmStringCreateLocalized "Freeze mode"))
                     (toggle
                      (|XtCreateManagedWidget "Freeze mode" |xmToggleButtonWidgetClass (|XtParent (car sliders))
                        (list |XmNselectColor  (|Pixel (snd-pixel (pushed-button-color)))
                              |XmNbackground   (|Pixel (snd-pixel (basic-color)))
                              |XmNvalue        freeverb-freeze-mode
                              ;|XmNborderWidth  1
                              |XmNlabelString  s1))))
                (|XmStringFree s1)
                (|XtAddCallback toggle |XmNvalueChangedCallback (lambda (w c i) (set! freeverb-freeze-mode (if (|set i) 1 0))))))

       (activate-dialog freeverb-dialog)))))

      (let ((child (|XtCreateManagedWidget "Freeverb3 (stereo)" |xmPushButtonWidgetClass ladspa-reverb-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-freeverb3-dialog))))


;;; Gverb
;;; plugin by Juhana Sadeharju
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

(define (cp-gverb)
   (apply-ladspa (make-sample-reader (cursor))
      (list "gverb_1216" "gverb" gverb-roomsize gverb-reverb-time gverb-damping gverb-bandwidth gverb-dry-level gverb-early-reflect-level gverb-tail-level)
         (- (frames) (cursor))
         "gverb"))

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))
  (begin

    (define (post-gverb-dialog)
       (if (not (|Widget? gverb-dialog))
           (let ((sliders '()))
             (set! gverb-dialog
                   (make-effect-dialog "gverb ladspa plugin"
                                       (lambda (w context info) (cp-gverb))
                                       (lambda (w context info) (|XtUnmanageChild gverb-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Gverb"
                                                      "A mono in, stereo out reverb implementation by Juhana Sadeharju (kouhia at nic.funet.fi). Steve Harris ported it to LADSPA and did some testing. Please contact Juhana directly regarding any bugs you find.\n\ Roomsize (m): The size of the room, in meters. Excessivly large, and excessivly small values will make it sound a bit unrealistic. Values of around 30 sound good.\n\ Reverb time (s): Reverb decay time, in seconds. 7 is a good place to start.\n\ Damping: This controls the high-frequency damping (a lowpass filter). Values near 1 will make it sound very bright, values near 0 will make it sound very dark.\n\ Input bandwidth: This is like a damping control for the input. It has a similar effect to the damping control, but is subtly different.\n\ Dry signal level (dB): The amount of dry signal to be mixed with the reverbed signal.\n\ Early reflection level (dB): The quantity of early reflections (scatter reflections directly from the source). Think of Lexicon's ambience patches.\n\ Tail level (dB): The level of the classic reverb tail reflections."))
                                       (lambda (w c i)
                                         (set! gverb-roomsize 30)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* gverb-roomsize 1))))
                                         (set! gverb-reverb-time 7.0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* gverb-reverb-time 100))))
                                         (set! gverb-damping 0.5)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* gverb-damping 100))))
                                         (set! gverb-bandwidth 0.5)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* gverb-bandwidth 100))))
                                         (set! gverb-dry-level 0)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* gverb-dry-level 1))))
                                         (set! gverb-early-reflect-level 0)
                                         (|XtSetValues (list-ref sliders 5) (list |XmNvalue (inexact->exact (* gverb-early-reflect-level 1))))
                                         (set! gverb-tail-level 0)
                                         (|XtSetValues (list-ref sliders 6) (list |XmNvalue (inexact->exact (* gverb-tail-level 1)))))))
             (set! sliders
                   (add-sliders gverb-dialog
                                (list (list "room size (m)" 1 30 300
                                            (lambda (w context info)
                                              (set! gverb-roomsize (/ (|value info) 1)))
                                            1)
                                      (list "reverb time (s)" 0.1 7.0 360.0
                                            (lambda (w context info)
                                              (set! gverb-reverb-time (/ (|value info) 100)))
                                            100)
                                      (list "damping" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! gverb-damping (/ (|value info) 100)))
                                            100)
                                      (list "bandwidth" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! gverb-bandwidth (/ (|value info) 100)))
                                            100)
                                      (list "dry level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-dry-level (/ (|value info) 1)))
                                            1)
                                      (list "early reflections level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-early-reflect-level (/ (|value info) 1)))
                                            1)
                                      (list "tail level (dB)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-tail-level (/ (|value info) 1)))
                                            1))))))
       (activate-dialog gverb-dialog))))

      (let ((child (|XtCreateManagedWidget "Gverb" |xmPushButtonWidgetClass ladspa-reverb-menu
                                           (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
        (|XtAddCallback child |XmNactivateCallback
                        (lambda (w c i)
                          (post-gverb-dialog))))


