;;; backwards compatibility for snd 7

(define free-mix-sample-reader free-sample-reader)
(define free-track-sample-reader free-sample-reader)
(define (inspect-sample-reader rd) (format #f "~A" rd))

(define enved-exp?
  (make-procedure-with-setter
   (lambda ()
     (= (enved-style envelope-exponential)))
   (lambda (val)
     (set! (enved-style) (if val envelope-exponential envelope-linear)))))

(define enved-active-env enved-envelope)
(define enved-selected-env enved-envelope)
(define filter-control-env filter-control-envelope)
(define filter-waveform-color filter-control-waveform-color)

(define (change-window-property w a v) (set! (window-property w a) v))

(define (recolor-widget w col)
  (if (and (provided? 'xm)
	   (provided? 'snd-motif))
      (XmChangeColor w col)
      (if (and (provided? 'xg)
	       (provided? 'snd-gtk))
	  (gtk_widget_modify_bg w GTK_STATE_NORMAL col))))

(define region-dialog view-regions-dialog)
(define edit-save-as-dialog save-selection-dialog)
