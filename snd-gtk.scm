;;; examples of the Snd guile-gtk connection

(use-modules (gtk gtk) (gtk gdk))


;;; --------------------------------
;;; make a dialog connected to a menu item that controls playback amp

(define amp 1.0)
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
         (set! amp (gtk-adjustment-value adj))))
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
                (vct->sound-data (vct-scale! (samples->vct beg size) amp) data 0)
                (mus-audio-write audio-fd data size)
                (set! beg (+ beg size)))))))
     (gtk-widget-show button)
     (gtk-widget-show window))))

