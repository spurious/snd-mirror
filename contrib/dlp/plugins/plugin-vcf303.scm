
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define vcf-303-trigger 0)
(define vcf-303-cutoff .5)
(define vcf-303-resonance .5)
(define vcf-303-env-mod .5)
(define vcf-303-decay .5)
(define vcf-303-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "VCF-303"
     (lambda ()
       (if (not (|Widget? vcf-303-dialog))
           (let ((sliders '()))
             (set! vcf-303-dialog
                   (make-effect-dialog "vcf-303 ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "cmt" "vcf303" vcf-303-trigger vcf-303-cutoff vcf-303-resonance vcf-303-env-mod vcf-303-decay)
                                                       (- (frames) (cursor))
                                                       "vcf-303"))
                                       (lambda (w context info) (|XtUnmanageChild vcf-303-dialog))
                                       (lambda (w context info)
                                         (help-dialog "VCF-303 Help"
                                                      "Move the sliders to set the parameters."))
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
