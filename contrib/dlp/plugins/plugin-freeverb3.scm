
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define freeverb-room-size .5)
(define freeverb-damping .5)
(define freeverb-wet-level .5)
(define freeverb-dry-level .5)
(define freeverb-width .5)
(define freeverb-freeze-mode 0)
(define freeverb-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Freeverb3"
     (lambda ()
       (if (not (|Widget? freeverb-dialog))
           (let ((sliders '()))
             (set! freeverb-dialog
                   (make-effect-dialog "freeverb ladspa plugin"
                                       (lambda (w context info)
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
                                       (lambda (w context info) (|XtUnmanageChild freeverb-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Freeverb3 Help"
                                                      "Jezar's famous reverb. Move the sliders to set the reverb parameters.\n\ This effect works only with stereo soundfiles!"))
                                       (lambda (w c i)
                                         (set! freeverb-room-size .5)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* freeverb-room-size 100))))
                                         (set! freeverb-damping .5)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* freeverb-damping 100))))
                                         (set! freeverb-wet-level .5)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* freeverb-wet-level 100))))
                                         (set! freeverb-dry-level .5)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* freeverb-dry-level 100))))
                                         (set! freeverb-width .5)
                                         (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* freeverb-width 100)))))))
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
