;;; examples of the Snd guile-gtk connection
;;;
;;; make-amp-dialog: create a dialog with an amplitude control on playback and a play button
;;; make-control-dialog: create a dialog that controls all the "hidden" control-panel variables
;;; control the fm-violin's amplitude from a slider


(use-modules (gtk gtk) (gtk gdk))


;;; --------------------------------
;;; make a dialog connected to a menu item that controls playback amp

(define make-amp-dialog
  (lambda ()
    (define amplitude 1.0)
    (define play-dialog-menu (gtk-menu-item-new-with-label "play"))
    (gtk-menu-append (sg-options-menu-widget) play-dialog-menu)
    (gtk-widget-show play-dialog-menu)
    (gtk-signal-connect play-dialog-menu "activate"
      (lambda ()
	(let* ((window (gtk-dialog-new))
	       (adj (gtk-adjustment-new 1.0 0.0 1.01 .01 .01 .01))
	       (scale (gtk-hscale-new adj))
	       (button (gtk-button-new-with-label "play")))
	  (gtk-box-pack-start (gtk-dialog-action-area window) scale #t #t 2)
	  (gtk-range-set-update-policy scale 'continuous)
	  (gtk-scale-set-digits scale 2)
	  (gtk-scale-set-draw-value scale #t)
	  (gtk-widget-show scale)
	  (gtk-signal-connect adj "value_changed"
            (lambda ()
	      (set! amplitude (gtk-adjustment-value adj))))
	  (gtk-box-pack-start (gtk-dialog-action-area window) button #f #f 2)
	  (gtk-signal-connect button "clicked"
            (lambda ()
	      (let* ((size 256)
		     (data (make-sound-data 1 size))
		     (bytes (* size 2))
		     (len (frames))
		     (beg 0)
		     (audio-fd (mus-audio-open-output mus-audio-default 22050 1 mus-lshort bytes)))
		(if (not (= audio-fd -1))
		    (do ()
			((or (abort?) (>= beg len))
			 (mus-audio-close audio-fd))
		      (vct->sound-data (vct-scale! (samples->vct beg size) amplitude) data 0)
		      (mus-audio-write audio-fd data size)
		      (set! beg (+ beg size)))))))
	  (gtk-widget-show button)
	  (gtk-widget-show window))))))


;;; --------------------------------
;;; make a dialog that exposes the various hidden parameters in the control-panel functions

(define make-control-dialog
  (lambda () 
    (define control-dialog-menu (gtk-menu-item-new-with-label "controls"))
    (define add-control
      (lambda (container name adjs)
	(let* ((adj (apply gtk-adjustment-new adjs))
	       (lab (gtk-label-new name))
	       (scl (gtk-hscale-new adj))
	       (hb (gtk-hbox-new #f 0)))
	  (gtk-box-pack-start container hb #t #t 2)
	  (gtk-widget-show hb)
	  (gtk-box-pack-start hb lab #f #f 2)
	  (gtk-widget-show lab)
	  (gtk-box-pack-start hb scl #t #t 2)
	  (gtk-widget-show scl)
	  (gtk-range-set-update-policy scl 'continuous)
	  (gtk-scale-set-digits scl 2)
	  (gtk-scale-set-draw-value scl #t)
	  adj)))
    (gtk-menu-append (sg-options-menu-widget) control-dialog-menu)
    (gtk-widget-show control-dialog-menu)
    (gtk-signal-connect control-dialog-menu "activate"
      (lambda ()
	(let* ((window (gtk-dialog-new))
	       (outer-vbox (gtk-vbox-new #f 0)))
	  (gtk-box-pack-start (gtk-dialog-action-area window) outer-vbox #t #t 0)
	  (gtk-widget-show outer-vbox)
	  (let ((adj (add-control outer-vbox "expand-hop" '(0.05 0.0 1.01 .01 .01 .01))))
	    (gtk-signal-connect adj "value_changed"
	      (lambda ()
		(set-expand-hop (gtk-adjustment-value adj) #t))))
	  (let ((adj (add-control outer-vbox "expand-length" '(0.15 0.0 .51 .01 .01 .01))))
	    (gtk-signal-connect adj "value_changed"
	      (lambda ()
		(set-expand-length (gtk-adjustment-value adj) #t))))
	  (let ((adj (add-control outer-vbox "expand-ramp" '(0.4 0.0 .51 .01 .01 .01))))
	    (gtk-signal-connect adj "value_changed"
	      (lambda ()
		(set-expand-ramp (gtk-adjustment-value adj) #t))))
	  (let ((adj (add-control outer-vbox "contrast-amp" '(1.0 0.0 2.01 .1 .1 .1))))
	    (gtk-signal-connect adj "value_changed"
	      (lambda ()
		(set-contrast-amp (gtk-adjustment-value adj) #t))))
	  (let ((adj (add-control outer-vbox "reverb-feedback" '(1.09 0.0 1.22 .01 .01 .01))))
	    (gtk-signal-connect adj "value_changed"
	      (lambda ()
		(set-reverb-feedback (gtk-adjustment-value adj) #t))))
	  (let ((adj (add-control outer-vbox "reverb-lowpass" '(0.7 0.0 1.01 .01 .01 .01))))
	    (gtk-signal-connect adj "value_changed"
	      (lambda ()
		(set-reverb-lowpass (gtk-adjustment-value adj) #t))))
	  (gtk-widget-show window))))))


;;; --------------------------------
;;; control the fm-violin's amplitude from a slider
;;; this needs the fm-violin in fmv.scm and a fast machine

(define make-fmv-dialog
  (lambda ()
    (let ((amplitude 1.0))
      (define play-dialog-menu (gtk-menu-item-new-with-label "play"))
      (gtk-menu-append (sg-options-menu-widget) play-dialog-menu)
      (gtk-widget-show play-dialog-menu)
      (gtk-signal-connect play-dialog-menu "activate"
        (lambda ()
	  (let* ((window (gtk-dialog-new))
		 (adj (gtk-adjustment-new 1.0 0.0 1.01 .01 .01 .01))
		 (scale (gtk-hscale-new adj))
		 (button (gtk-button-new-with-label "play")))
	    (gtk-box-pack-start (gtk-dialog-action-area window) scale #t #t 2)
	    (gtk-range-set-update-policy scale 'continuous)
	    (gtk-scale-set-digits scale 2)
	    (gtk-scale-set-draw-value scale #t)
	    (gtk-widget-show scale)
	    (gtk-signal-connect adj "value_changed"
              (lambda ()
	        (set! amplitude (gtk-adjustment-value adj))))
	    (gtk-box-pack-start (gtk-dialog-action-area window) button #f #f 2)
	    (let ((running #f)
		  (audio-fd #f))
	      (gtk-signal-connect button "clicked"
                (lambda ()
		  (if running
		      (set! running #f)
		      (let* ((size 64)
			     (data (make-sound-data 1 size))
			     (v (make-fm-violin 440 amplitude :amp-env (lambda () amplitude)))
			     (bytes (* size 2))
			     (audio-fd (mus-audio-open-output mus-audio-default 22050 1 mus-lshort bytes)))
			(set! running #t)
			(if (not (= audio-fd -1))
			    (do ()
				((or (abort?) (not running))
				 (begin
				   (set! running #f)
				   (mus-audio-close audio-fd)))
			      (do ((k 0 (1+ k)))
				  ((= k size))
				(sound-data-set! data 0 k (fm-violin v)))
			      (mus-audio-write audio-fd data size))
			    (set! running #f)))))))
	    (gtk-widget-show button)
	    (gtk-widget-show window)))))))
