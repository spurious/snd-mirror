
;; My config file for snd.
;; -Kjetil S. Matheussen.


;; Set this one to #t if you use gtk. This config-file is primarly made for use with motif, so by
;; using gtk you will get less functionality out of this config-file.
(define use-gtk #f)


(set! (ladspa-dir) "/usr/lib/ladspa")


;; Set various variables. See Snd documentation.

#!
(set! %load-path (cons
		  (if use-gtk
		      "/home/kjetil/snd-6-gtk2"
		      "/home/kjetil/snd-6")
		  %load-path))
!#


;(set! snd-remember-paths #t)

(set! (just-sounds) #t)

;;(set! (temp-dir) "/lyd/local/tmp")
;;(set! (save-dir) "/lyd/local/tmp")

(set! (default-output-srate) 44100)
(set! (default-output-type) mus-riff)
(set! (default-output-chans) 2)
(set! (default-output-format) mus-lfloat)

(set! (show-backtrace) #t)
(set! (show-indices) #t)

;(set! (auto-resize) #f)

; Regions are created when needed when using the ctrl+y and ctrl+w keybindings.
(set! (selection-creates-region) #f)



;; This is the value for the loop-button.
(define islooping #t)



(load-from-path "rgb.scm")

;; Set different color on the cursor for playing and not playing.
(define set-sound-cursor
  (lambda (snd shape)
    (do ((j 0 (1+ j)))
        ((= j (channels snd)) #f)
      (set! (cursor-style snd j) shape))))

(add-hook! start-playing-hook 
	   (lambda (snd) 
	     (set! (cursor-color) yellow)
	     #f))

(set! (cursor-color) blue)
(add-hook! stop-playing-hook
	   (lambda (snd) 
	     (set! (cursor-color) blue)
	     #f))


;; Set different color on the cursor when playing selection.
(add-hook! start-playing-selection-hook
	   (lambda ()
;;	     (display "Start playing selection")(newline)
	     (set! (cursor-color) green)
	     #f))


(add-hook! stop-playing-selection-hook
	   (lambda ()
;;	     (display "Stop playing selection")(newline)
	     (set! (cursor-color) blue)
	     #f))


;; Let the cursor move when playing.
(add-hook! after-open-hook 
	   (lambda (sp)
	     (set! (cursor-follows-play sp) #t)
	     (set-sound-cursor sp cursor-line)
	     #f))


;; Set graph-style to filled.
(set! (graph-style) graph-filled)



;; Removes default mouse-click-hook handling. The possibility to paste with
;; mouse-button 2 is really annoying when using a wheel mouse.
(add-hook! mouse-click-hook (lambda (snd chn button state x y axis)
			      #t))




;; Moves the playing position when clicking in the editor if clicking button 1 or 4.
;; Moves cursor and stops playing if clicking buttton 5. (wheel down)
;; Moves cursor and starts playing if clicking button 4. (wheel up)
; -Kjetil.


(if #t

    (add-hook! mouse-click-hook
	       (lambda (snd chn button state x y axis)
		 (if (= axis time-graph)
		     (let ((samp (inexact->exact (* (srate snd) (position->x x snd chn))))
			   (dasspeed (speed-control)))
		       (if (< samp 0) (set! samp 0))
		       (stop-playing)
		       (set! (cursor) samp)
		       (cond ((= button 4)
			      (if (< dasspeed 0)
				  (set! (speed-control) (* -1 dasspeed)))
			      (play samp))
			     ((= button 5)
			      (if (> dasspeed 0)
				  (set! (speed-control) (* -1 dasspeed)))
			      (play samp)))))
		 #f))

    (add-hook! mouse-click-hook
	       (lambda (snd chn button state x y axis)
		 (if (= axis time-graph)
		     (let ((samp (inexact->exact (* (srate snd) (position->x x snd chn)))))
		       (if (< samp 0) (set! samp 0))
		       (if initfile-isplaying
			   (if (or (= button 1) (= button 4) (= button 5))
			       (begin
				 (stop-playing)
				 (if (not (= button 5))
				     (play samp))))
			   (if (= button 4)
			       (begin
				 (play samp))))))
		 #f)))





;(define (play-selection-with-play)
;  (play (selection-position) #f #f #f (+ (selection-position) (selection-frames))))

(define (my-play-selection2)
  (if islooping
      (play-selection)
      (remove-hook! stop-playing-selection-hook my-play-selection2))
  #f)

(define (my-play-selection)
  (if islooping
      (add-hook! stop-playing-selection-hook my-play-selection2))
  (play-selection))

(define (my-play2 snd)
  (if islooping
      (play)
      (remove-hook! stop-playing-hook my-play2))
  #f)

(define (my-play pos)
  (if islooping
      (add-hook! stop-playing-hook my-play2))
  (play pos))

(define (my-stop-playing)
  (remove-hook! stop-playing-selection-hook my-play-selection2))


;; Replace the old space binding with one that starts playing from the current cursor position,
;; all channels. And stops playing if allready playing. -Kjetil.
(bind-key (char->integer #\ ) 0 
	  (lambda ()
	    (if (dac-is-running)
		(begin
		  (remove-hook! stop-playing-selection-hook my-play-selection2)
		  (remove-hook! stop-playing-hook my-play2)
		  (stop-playing))
		(if (selection-member? (selected-sound))
		    (my-play-selection)
		    (my-play (cursor))))))
;		    (play-selection-with-play)
;		    (play (cursor))))))

;; Makes the P key a pause button. If playing, stops playing, but doesnt reset the cursor pos.
;; If not playing, starts playing from cursor. -Kjetil.
(bind-key (char->integer #\p) 0 
	  (lambda ()
	    (let ((cursor-pos (cursor)))
	      (if (dac-is-running)
		  (begin
		    (stop-playing)
		    (set! (cursor) cursor-pos)
		    (let ((syncnum (sync)))
		      (set! (sync) 0)
		      (set! (sync) syncnum))
		    (update-time-graph))
		  (play (cursor))))))


;; Let the "a" key be a "show-all" key.
(bind-key (char->integer #\a) 0
	  (lambda ()
	    (set! (x-bounds) (list 0.0 (/ (frames) (srate))))))


;; Let the "s" key be a "show-selection" key.
(bind-key (char->integer #\s) 0
	  (lambda ()
	    (set! (x-bounds) (list (/ (selection-position) (srate))
				   (/ (+ (selection-position) (selection-frames)) (srate))))))


;; Let "c" turn on/off controls
(bind-key (char->integer #\c) 0
	  (lambda ()
	    (set! (show-controls)
		  (if (show-controls)
		      #f
		      #t))))

;; Replace the default up-key handler. This one does nothing when all data is shown.
(bind-key #xFF52 0
	  (lambda ()
	    (let* ((x (car (x-bounds)))
		   (y (cadr (x-bounds)))
		   (i (* 0.2 (- y x)))
		   (newx (max 0 (- x i)))
		   (newy (min (/ (frames) (srate))
			      (+ y i))))
	      (if (or (> x 0)
		      (< y (/ (frames) (srate))))
		  (set! (x-bounds) (list newx newy))))))


;(bind-key #xFF54 0
;	  (lambda ()
;	    (display "down")(newline)))



;; Make snd quit when pressing C-x-c, just like emacs.
(bind-key (char->integer #\c) 4
	  (lambda ()
	    (exit))
	  #t)
	    

(load-from-path "draw.scm")


;; ;;; Set some colors

;; Bacground
(define backgroundcolor  (make-color 0.8 0.8 0.78))
(set! (selected-graph-color) backgroundcolor)
(set! (graph-color) backgroundcolor)

;; Selection
;;(set! (selection-color) (make-color 0.5 0.6 0.4))
;;(set! (selection-color) (make-color 0.78 0.87 0.85))
(set! (selection-color) white)

;(set! (cursor-color) green)

;(set! (basic-color) (make-color 0.95 0.95 0.92))






;; Fix mouse-selection handling.

(define region-generation 0)


(begin
  (define selection-starting-point #f)
  (define stop-handling #f)

  (define (mouse-press-callback w context ev flag)
    (if (not stop-handling)
	(let* ((x (.x ev))
	       (snd (selected-sound))
	       (chn (selected-channel))
	       (samp (inexact->exact (* (srate snd) (position->x x snd chn)))))
	  (if (< samp 0) (set! samp 0))
	  (show-times (cursor) #t)
	  (set! selection-starting-point samp))))
  
  (define (mouse-release-callback w context ev flag)
    (if (and (not stop-handling)
	     selection-starting-point)
	(let* ((x (.x ev))
	       (snd (selected-sound))
	       (chn (selected-channel))
	       (samp (max 0 (inexact->exact (* (srate snd) (position->x x snd chn))))))
	  (if (not (= samp selection-starting-point))
	      (if (< samp selection-starting-point)
		  (begin
		    (set! (selection-position) samp)
		    (set! (selection-frames) (- selection-starting-point samp)))
		  (begin
		    (set! (selection-position) selection-starting-point)
		    (set! (selection-frames) (- samp selection-starting-point)))))
	  (show-times (cursor) #t)
	  (set! selection-starting-point #f))))
  
  (define (mouse-motion-callback w context ev flag)
    (if (and (not stop-handling)
	     (selection?))
	(let* ((x (.x ev))
	       (snd (selected-sound))
	       (chn (selected-channel))
	       (samp (max 0 (inexact->exact (* (srate snd) (position->x x snd chn)))))
	       (selstart (selection-position))
	       (selframes (selection-frames))
	       (newselstart (if (< samp selection-starting-point)
				samp
				selection-starting-point))
	       (newselframes (if (< samp selection-starting-point)
				 (- selection-starting-point samp)
				 (1+ (- samp selection-starting-point)))))
	  (if (and (or (not (= selstart newselstart))
		       (not (= selframes newselframes)))
		   (not (= samp selection-starting-point)))
	      (begin
		;;(display selstart)(display "->")(display newselstart)(newline)
		;;(display selframes)(display "->")(display newselframes)(newline)
		(set! region-generation (+ region-generation 1))
		(set! (selection-position) newselstart)
		(set! (selection-frames) newselframes)))
	  (show-times (cursor) #t))))

  (add-hook! mix-drag-hook
	     (lambda (n) 
	       (set! selection-starting-point #f)
	       (set! stop-handling #t)
	       #f))
  
  (add-hook! mix-dragged-hook
	     (lambda (id samps)
	       (set! stop-handling #f)
	       #f))
  (if (not use-gtk)	     
      (add-hook! after-open-hook
		 (lambda (snd)
		   (XtAddEventHandler (list-ref (channel-widgets snd 0) 0) ButtonPressMask #f 
				      mouse-press-callback)
		   (XtAddEventHandler (list-ref (channel-widgets snd 0) 0) ButtonMotionMask #f 
				      mouse-motion-callback)
		   (XtAddEventHandler (list-ref (channel-widgets snd 0) 0) ButtonReleaseMask #f 
				      mouse-release-callback)
		   #f))))
  





;; Replace the old C-y and C-w keybindings. These are equal, but writes
;; "Please wait" to the minibuffer, and colorize the inserted region.


(define iscolorized #f)

(define (color-samples-allchans chans color start end)
  (if (> chans 0)
      (begin
	(if (eq? color 'black)
	    (uncolor-samples (selected-sound) (- chans 1))
	    (color-samples color start end (selected-sound) (- chans 1)))
	(color-samples-allchans (- chans 1) color start end))))

(define (my-make-region)
  (if (not (selection-creates-region))
      (if (> region-generation 0)
	  (begin
	    (set! region-generation 0)
	    (make-region)))))
;;	    (make-region (* 8 (srate (selected-sound))) (* 10 (srate (selected-sound))) (selected-sound) #t)))))

(bind-key (char->integer #\r) 0
	  my-make-region)

(bind-key (char->integer #\y) 4
	  (lambda ()
	    (if (selection-member? (selected-sound))
		(begin
		  (my-make-region)))
	    (if (> (length (regions)) 0)
		(begin
		  (report-in-minibuffer "Please wait...")
		  (color-samples-allchans (channels) 'black 0 0)
		  (let ((length (region-frames))
			(curspos (cursor)))
		    (if (> length 5000000)
			(begin
			  (set! (with-background-processes) #f) ;; To prevent using horrible long time, and the little picture not to update properly.
			  (insert-region curspos)
			  (set! (with-background-processes) #t))
			(insert-region curspos)) ;; The little picture might not update properly. But turning off background-processes slows down pasting of small regions a lot.
		    (if (> (sync) 0)
			(color-samples-allchans (channels) green curspos (region-frames))
			(color-samples green curspos (region-frames)))
		    (set! iscolorized #t)
		    (update-time-graph)
		    (report-in-minibuffer " "))))))


(bind-key (char->integer #\w) 4
	  (lambda ()
	    (report-in-minibuffer "Please wait...")
	    (let ((curspos (selection-position)))
	      (color-samples-allchans (channels) 'black 0 0)
	      (set! (cursor #t #t) curspos)
	      (my-make-region)
	      (delete-selection)
	      (set! iscolorized #f)
	      (report-in-minibuffer " "))))


#!
(add-hook! mouse-click-hook
	   (lambda (snd chn button state x y axis)
	     (if iscolorized
		 (begin
		   (color-samples-allchans (channels) 'black 0 0)
		   (set! iscolorized #f)))
	     #f))
		 
!#

(define last-report-value 0.0)

(define (progress-report pct . various) ;name current-channel channels snd)
  (let* ((sound-widget (list-ref (channel-widgets (selected-sound) 0) 0))
	 (widget-width (car (widget-size sound-widget))))
    (if (> (abs (- pct last-report-value)) (/ 1 widget-width))
	(let* ((old-color (foreground-color))
	       (x0 56)
	       (y0 0)
	       (width (* pct (- widget-width x0)))
	       (height 10)
	       (new-report-value (/ (floor (* 100 pct)) 100)))
	  (set! (foreground-color) blue)
	  (fill-rectangle x0 y0 width height)
	  (set! (foreground-color) old-color)
	  (report-in-minibuffer (string-append (number->string new-report-value)
					       (if (member new-report-value '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
						   "0"
						   "")))
	  (set! last-report-value pct)))))

(define  (start-progress-report . snd)
  (set! last-report-value 0.0)
  (report-in-minibuffer "0.00"))
(define (finish-progress-report . snd)
  (report-in-minibuffer ""))




(define lastpainted '(#f #f #f))

(define (dodasprint string level color framepos)
  (let* ((old-color (foreground-color))
	 (sound-widget (list-ref (channel-widgets (selected-sound) 0) 0))
	 (fontlen 9)
	 (width (- (car (widget-size sound-widget)) fontlen))
	 (height (cadr (widget-size sound-widget)))
	 (stringlen (* fontlen 7))
	 (fontheight 12)
	 (x (min (- width stringlen) (x->position (/ framepos (srate)))))
;	 (x (/ width 5))
	 (y (+ (* fontheight level)
	       (- (/ (- height fontheight) 2) (* fontheight 2)))))
;	       (- (/ (- height fontheight) 2) 5))))
    (let ((olddim (list-ref lastpainted level)))
      (if olddim
	  (begin
	    (set! (foreground-color) backgroundcolor)
	    (fill-rectangle (car olddim) (cadr olddim) (caddr olddim) fontheight))))
    (set! (foreground-color) color)
    (draw-string string x (+ y fontheight))
    (let ((i -1))
      (set! lastpainted
	    (map (lambda (dasx)
		   (set! i (1+ i))
		   (if (= i level)
		       (list x y stringlen)
		       dasx))
		 lastpainted)))
    (set! (foreground-color) old-color)))



;; Show cursor and selection position in minutes and seconds and 1/10th seconds. -Kjetil.
(define last-time-showed 0)
(define* (show-times dastime #:optional (force #f))

  (define (get-time-string dastime)
    (let* ((time (/ (floor (* 10 (/ dastime (srate (selected-sound))))) 10))
	   (minutes (inexact->exact (floor (/ time 60))))
	   (seconds (/ (floor (* 10 (- time (* minutes 60)))) 10)))
      (string-append (if (< minutes 10)
			 "0"
			 "")
		     (number->string minutes)
		     (if (< seconds 10)
			 ":0"
			 ":")
		     (number->string seconds))))

  (if (and (= (channels) 2) (= (channel-style (selected-sound)) channels-combined))
      (if (selection?)
	  (begin
	    (dodasprint (get-time-string dastime) 0 red dastime)
	    (dodasprint (get-time-string (selection-position)) 1 blue (selection-position))
	    (dodasprint (get-time-string (+ (selection-position) (selection-frames))) 2 blue (+ (selection-position) (selection-frames))))
	  (dodasprint (get-time-string dastime) 0 red dastime))
      (if (or force
	      (>= (abs (- dastime last-time-showed)) (/ (srate (selected-sound)) 10)))
	  (begin
	    (report-in-minibuffer (string-append (get-time-string dastime)
						 (if (selection?)
						     (begin
						       (string-append " "
								      (get-time-string (selection-position))
								      " "
								      (get-time-string (+ (selection-position) (selection-frames)))))
						     "")))
	    (set! last-time-showed dastime)))))
   

;; Show the time in the minibuffer when playing
(add-hook! play-hook
	   (lambda (samples)
	     (show-times (cursor))
	     #f))

(add-hook! after-graph-hook
	   (lambda (snd chn)
	     (show-times (cursor) #t)
	     #f))

; Show the time in the minibuffer when clicking.
;(add-hook! mouse-click-hook
;	   (lambda snd 
;	     (show-times (cursor))
;	     #f))

; This one don't work.
;(add-hook! mouse-drag-hook
;	   (lambda (snd chn button state x y)
;	     (let ((samp (inexact->exact (* (srate snd) (position->x x snd chn)))))
;	       (display "asdf")(newline)
;	       (show-times samp)) #f))




;; Shows the full sound after opening.
(add-hook! after-open-hook
	   (lambda (n) (set! (x-bounds) (list 0.0 (/ (frames) (srate)))) #f))


;; Equalize panes if more than 3 channels. I think. Hmm, can't
;; remember where I got this one from. Nah, doesn't hurt. -Kjetil.
;;(add-hook! after-open-hook 
;;  (lambda (n) (if (> (channels n) 3) (equalize-panes)) #f))





;; Doing several things after opening a file.
;; -Sync and Unite newly loaded files by default.
;;  The sync value should be unique for each sound.
;; -Replace the sync button with a new one that uses get-unique-sync-num
;;  as the sync-number. Simply using (+ 1 snd) doesn't work when a sound is closed
;;  in certain situations.
;; -Remove the play button. Its useless now with the way p and space is configured.
;; -Added a loop button where the play button was.

(define unique-sync-num 0)
(define (get-unique-sync-num)
  (set! unique-sync-num (+ unique-sync-num 1))
  unique-sync-num)

(add-hook! after-open-hook
	   (lambda (snd)
	     (if (not use-gtk)
		 (begin
		   (let* ((oldplay (find-child (list-ref (sound-widgets snd) 2) "play"))
			  (playpos (widget-position oldplay)))
		     
		     (let ((loop (XtCreateManagedWidget "loop" xmToggleButtonWidgetClass (XtParent oldplay)
							(list XmNbackground       (basic-color)
							      XmNset              islooping
							      XmNx                (car playpos)
							      XmNselectColor      (yellow-pixel)))))
		       (XtAddCallback loop
				      XmNvalueChangedCallback
				      (lambda (w c i)
					(if (.set i)
					    (begin
					      (set! islooping #t))
					    (begin 
					      (set! islooping #f)))
					(for-each (lambda (s)
						    (if (not (= s snd))
							(XtSetValues (find-child (list-ref (sound-widgets s) 2) "loop")
								     (list XmNset islooping))))
						  (sounds))
					(focus-widget (list-ref (channel-widgets snd 0) 0)))))
		     (XtUnmanageChild oldplay))))
	     
	     (if (> (channels snd) 1)
		 (set! (channel-style snd) channels-combined))
	     (set! (sync snd) (get-unique-sync-num))
	     

	     (if (not use-gtk)
		 (begin
		   (let* ((oldsync (find-child (list-ref (sound-widgets snd) 2) "sync"))
			  (syncpos (widget-position oldsync)))
		     
		     (let ((newsync (XtCreateManagedWidget "sync" xmToggleButtonWidgetClass (XtParent oldsync)
							   (list XmNbackground       (basic-color)
								 XmNset              #t
								 XmNx                (car syncpos)
								 XmNselectColor      (yellow-pixel)))))
		       (XtAddCallback newsync
				      XmNvalueChangedCallback
				      (lambda (w c i)
					(if (.set i)
					    (begin
					      (set! (sync snd) (get-unique-sync-num)))
					    (begin 
					      (set! (sync snd) 0)))
					(focus-widget (list-ref (channel-widgets snd 0) 0)))))
		     (XtUnmanageChild oldsync))))
	     #f))






;;"various generally useful Snd extensions". See source.
(load-from-path "extensions.scm")

;; When exiting.
(check-for-unsaved-edits #t)



;; "Easy edit". See source. -Kjetil.
;;(load-from-path "edit123.scm")



;;(load-from-path "dsp.scm")


;;(load-from-path "snd-motif.scm")

;(define (Widget wid) (if (list? wid) wid (list 'Widget wid)))
(load-from-path "edit-menu.scm")

;; Shows an envelope editor for each channel. Didn't understand how to use it, and it segfaulted. -Kjetil.
;;(load-from-path "enved.scm")
;;(start-enveloping)



(load-from-path "examp.scm")

;;;;;;;;;;;;;;;;;
;; Makes the buffer-menu a bit more pleasent to use. -Kjetil.
;;;;;;;;;;;;;;;;
;(set! last-height 800)
;(set! last-width 800)

(define (my-switch-to-buf filename)
  (let* ((width (car (widget-size (car (sound-widgets (car current-buffer))))))
	 (height (cadr (widget-size (car (sound-widgets (car current-buffer)))))))
    (call-with-current-continuation
     (lambda (give-up)
       (if (or (not (string? filename))
	       (= (string-length filename) 0))
	   (let ((temp current-buffer))
	     (if last-buffer
		 (set! current-buffer last-buffer)
		 (let ((index (new-sound)))
		   (set! current-buffer (list index 0))))
	     (set! last-buffer temp))
	   (let ((index (find-sound filename)))
	     (if index
		 (begin
		   (set! last-buffer current-buffer)
		   (set! current-buffer (list index 0)))
		 (give-up (report-in-minibuffer (format #f "can't find ~A" response))))))
       (close-all-buffers)
       (report-in-minibuffer "")
       (open-current-buffer width height)))))


(define (my-open-buffer filename)
  "(open-buffer filename) adds a menu item that will select filename (use with open-hook)"
  (if (member filename (map (lambda (snd) (file-name snd))
			    (sounds)))
      #t
      (begin
	(add-to-menu buffer-menu 
		     filename 
		     (lambda () (my-switch-to-buf filename)))
	#f)))


(define buffer-menu (add-to-main-menu "Buffers"))
(add-hook! open-hook my-open-buffer)
(add-hook! close-hook close-buffer)
(bind-key (char->integer #\b) 0 (lambda (x) (switch-to-buf)))
(add-hook! close-hook xb-close)
(add-hook! after-open-hook xb-open)	    




(define (first-time-open-soundfile . args)
  (set! (window-width) 20)
  (set! (window-height) 20)
  (remove-hook! open-hook first-time-open-soundfile)
  #f)

;(add-hook! open-hook first-time-open-soundfile)



;;Show the little picture of the whole sound in the upper right corner.
(load-from-path "draw.scm")
;;(load "/home/kjetil/snd/draw.scm")
(make-current-window-display)

;; The background-process slows things down when the little picture is active and loading large files.
;; Better turn off the background-process when loading. -Kjetil.
(add-hook! open-hook (lambda args (set! (with-background-processes) #f) #f))
(add-hook! after-open-hook (lambda args (set! (with-background-processes) #t) #f))




; Would be nice, perhaps, but prints out a lot of error messages. -Kjetil.
;;(if-cursor-follows-play-it-stays-where-play-stopped #t)





;; Adds a lot of things when pressing the right mouse button. Very nice. -Kjetil.
(load-from-path (if use-gtk
		    "gtk-popup.scm"
		    "popup.scm"))



;; Adds the "Effects" menu. Very very Nice. -Kjetil.
;;(load-from-path "new-effects.scm")


;; My ladspa menu stuff. -Kjetil.
;;(load "/hom/kjetism/snd/ladspa2.scm")
;(load "/home/kjetil/snd/ladspa2.scm")

(load-from-path  (if use-gtk
		     "gtk-effects.scm"
		     "ladspa.scm"))
;;(load-from-path "/home/kjetil/snd-6/ladspa.scm")

; Lots of functions. But I Don't understand what they do. -Kjetil.
;;(load-from-path "marks.scm")


; "provide pop-up help in the Files viewer". Didn't see any pop-up help. -Kjetil.
;;(load-from-path "nb.scm")



;; Stores the peak information for sounds in the ~/peaks/ directory. Seems to work correctly. I have tried
;; to fool it in many ways, but it seems to be very intelligent. Extremely nice. -Kjetil.
;; The point is to decrease the loading time.
; First make sure the peaks directory is present
(system (string-append "mkdir " (getenv "HOME") "/peaks >/dev/null 2>/dev/null"))
; Then load
(load-from-path "peak-env.scm")




(load-from-path (if use-gtk
		    "snd-gtk.scm"
		    "snd-motif.scm"))

;; Shows diskspace for the partition the opened sound was placed on. -Kjetil.
;(add-hook! after-open-hook show-disk-space)

;; Add extra control for the controls dialog. -Kjetil.
(make-hidden-controls-dialog)

;; Add rename option to the file menu. -Kjetil.
(if (not use-gtk)
    (add-rename-option))


;; Dave Phillips fft-menu. (Loads an incompatible make-effect-dialog function)
;(load-from-path "dlp/fft-menu.scm")

;; Dave Phillips panic-menu
(load-from-path "dlp/panic.scm")

;; Dave Phillips special-menu
(load-from-path "dlp/special-menu.scm")

;;(load-from-path "dlp/plugins-menu.scm")


;; All of Dave Phillips nice things.
;; Uncomment to try. (Most of it is allready present)
;;(load-from-path "dlp/misc.scm")



(define (open-sounds-filename)
  (string-append (getenv "HOME") "/.snd_soundfilelist"))


;; Save all filenames when exiting.
(define (save-all-filenames filename)
  (let ((fd (open-file filename "w")))
    (for-each (lambda (snd)
		(write-line (file-name snd) fd))
	      (reverse (sounds)))
    (close fd)))

(add-hook! exit-hook (lambda args
		       (save-all-filenames (open-sounds-filename))
		       #f))
				   


;; Load files from previous session.

(system (string-append "touch " (open-sounds-filename) " >/dev/null 2>/dev/null"))
(let ((fd (open-file (open-sounds-filename) "r")))
  (define (myread)
    (let ((line (read-line fd)))
      (if (not (eof-object? line))
	  (begin
	    (open-sound line)
	    (myread))
	  (begin
	    (set! (auto-resize) #f)
	    (in 2000
		(lambda ()
		  (set! (window-width) 800)
		  (set! (window-height) 600)))))))
  (set! (auto-resize) #t)
  (myread))



;; Need to update the loop button when switching buffer. (Done in the callback-function instead)
;(add-hook! after-graph-hook
;	   (lambda (snd chn)
;	     (XtSetValues (find-child (list-ref (sound-widgets snd) 2) "loop")
;			  (list XmNset islooping))))









