
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define delay5s-time .3)
(define delay5s-balance .5)
(define delay5s-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Delay (5s max)"
     (lambda ()
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
                                         (help-dialog "delay5s Help"
                                                      "Move the slider"))
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
