
;; My config file for snd.
;; -Kjetil S. Matheussen.

;; Should work for both gtk and motif.




(provide 'snd-snd_conffile.scm)



;;##############################################################
;; c-define is used instead of define when theres a variable the
;; user might want to define before loading this file.
;;##############################################################

(define-macro (c-define name val)
  (if (not (defined? name))
      `(define ,name ,val)))





;;##############################################################
;; Various settings.
;; (ladspa-dir), %load-path, (temp-dir) and (save-dir) are
;; variables you might want to set in your .snd file.
;;##############################################################

(if (defined? 'notam-settings)
    ;; This is the settings used at Notam/Oslo. May not suite your setup.
    (begin
      (set! (ladspa-dir) "/usr/lib/ladspa")
      (set! %load-path (cons "/hom/kjetism/snd/snd-run" %load-path))
      (set! (temp-dir) "/lyd/local/tmp")
      (set! (save-dir) "/lyd/local/tmp")))

(if (defined? 'kjetil-settings)
    ;; This is for my personal computers settings. May not suite your setup.
    (begin
      (set! %load-path (cons "/hom/kjetil/snd-7" %load-path))
      (set! (temp-dir) "/lyd/local/tmp")
      (set! (save-dir) "/lyd/local/tmp")))






;;##############################################################
;; Load various files
;;##############################################################

(if (not (provided? 'snd-gui.scm))
    (load-from-path "gui.scm"))
(if (not (defined? 'green))
    (load-from-path "rgb.scm"))




;;##############################################################
;; Set various variables
;;##############################################################

;;(set! snd-remember-paths #t)

(set! (just-sounds) #t)

(c-define c-default-output-srate 44100)
(set! (default-output-srate) c-default-output-srate)

(c-define c-default-output-type mus-riff)
(set! (default-output-type) c-default-output-type)

(c-define c-default-output-chans 2)
(set! (default-output-chans) c-default-output-chans)

(c-define c-default-output-format mus-lfloat)
(set! (default-output-format) c-default-output-format)

(c-define c-show-backtrace #t)
(set! (show-backtrace) c-show-backtrace)

(c-define c-show-indices #t)
(set! (show-indices) c-show-indices)

;;(set! (auto-resize) #f)

; Regions are created when needed when using the ctrl+y and ctrl+w keybindings.
(set! (selection-creates-region) #f)

;; Less than 64 seems to be very unreliable.
(c-define c-minimum-dac-size 1024)
(if (< (dac-size) c-minimum-dac-size)
    (set! (dac-size) c-minimum-dac-size))


;; Set graph-style to filled.
(set! (graph-style) graph-filled)

;; This is the value for the loop-button.
(c-define c-islooping #t)

(c-define c-backgroundcolor (make-color 0.8 0.8 0.78))
(c-define c-selectioncolor white)

(c-define c-restore-previous-session #t)

(c-define c-cursor-color blue)

(c-define c-zoomfactor 1.2)

;; Time forward/backward to move the cursor using which keys, in seconds.
(c-define c-cursormovetime-rightleft 2)
(c-define c-cursormovetime-updown 10)
(c-define c-cursormovetime-pageupdown 30)

;; Hopefully this will change.
(define c-setting-cursor-color-cause-redraw #t)

(define c-is-stop-hooks-here #f)
;;(defined? 'stop-playing-hook))



;;##############################################################
;; Documentation for this configuration file.
;;##############################################################

(add-to-menu 4 "--------------------------------" (lambda ()(newline)) 0)
(add-to-menu 4 "But! How do I..."
	     (lambda ()
	       (help-dialog  "How do I..."
			     (apply string-append (map (lambda (s) (string-append s (string #\newline)))
						       '(
							 "Play:"
							 "     <Space>"
							 ""
							 "Play selection:"
							 "     <Space>"
							 ""
							 "Stop playing:"
							 "     <Space>"
							 ""
							 "Play from cursor:"
							 "     p"
							 ""
							 "Pause playing:"
							 "     p"
							 ""
							 "Play starting from mousepointer position:"
							 "     <Mouse Scroll Up/Down>"
							 ""
							 "Cut:"
							 "      <Ctrl>+w"
							 ""
							 "Paste:"
							 "      <Ctrl>+y"
							 ""
							 "Zoom in:"
							 "      +"
							 ""
							 "Zoom out:"
							 "      -"
							 ""
							 "Show all:"
							 "      a"
							 ""
							 "Show selection:"
							 "      s"
							 ""
							 "Undo:"
							 "      <Ctrl>+_"
							 ""
							 "Make mark"
							 "      m"
							 ""
							 "Move cursor 2 seconds forward"
							 "      <Arrow right>"
							 ""
							 "Move cursor 2 seconds backward"
							 "      <Arrow left>"
							 ""
							 "Move cursor 10 seconds forward"
							 "      <Arrow up>"
							 ""
							 "Move cursor 10 seconds backward"
							 "      <Arrow down>"
							 ""
							 "Move cursor 30 seconds forward"
							 "      <Page Up>"
							 ""
							 "Move cursor 30 seconds backward"
							 "      <Page Down>"
							 ""
							 "Set cursor at beginning:"
							 "      <"
							 ""
							 "Set cursor at end:"
							 "      >"
							 ""
							 "Change selection without destroying the old one:"
							 "      <Ctrl> + <Mouse>"
							 ""
							 "Move selected area:"
							 "      <Shift> + <Mouse>"
							 ))))
	       )
	     0)








;;##############################################################
;; Various more or less general functions
;;##############################################################


(define (c-get-nameform snd)
  (if use-gtk
      (GTK_BOX (list-ref (sound-widgets snd) 10))
      (find-child (list-ref (sound-widgets snd) 2) "snd-name-form")))

(define (c-for-each-nameform-button snd name func)
  (for-each-child (c-get-nameform snd)
		  (lambda (w)
		    (if use-gtk
			(if (and (GTK_IS_BUTTON w)
				 (string=? name (gtk_button_get_label (GTK_BUTTON w))))
			    (func (GTK_BUTTON w)))
			(if (string=? (XtName w) name)
			    (func w))))))

(define (c-get-nameform-button snd name)
  (call-with-current-continuation
   (lambda (return)
     (c-for-each-nameform-button snd name return)
     #f)))


;; Selection-position and selection-frames doesn't allways work properly.
(define (c-selection-position)
  (selection-position (selected-sound)))
(define (c-selection-frames)
  (selection-frames (selected-sound)))

;; Like (selection?) but only for the selected sound.
(define (c-selection?)
  (selection-member? (selected-sound)))

;; Returns true if file is allready loaded. Necesarry to unscrew up open-hook handling.
(define (c-isloaded? filename)
  (member filename (map (lambda (snd) (file-name snd))
			(sounds))))

;; Like (set! (cursor) pos), but legalize pos and
;; calls c-show-times as well. Also works when playing.
(define (c-set-cursor-pos pos)
  (let ((isplaying (dac-is-running))
	(legalpos (min (frames) (max 0 pos))))
    (if isplaying 
	(stop-playing)
	(c-show-times legalpos))
    (set! (cursor) legalpos)
    (if isplaying
	(c-play legalpos))))




;;##############################################################
;; Customize the cursor look
;;##############################################################

(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! start-playing-hook 
	       (lambda (snd) 
		 (set! (cursor-color) yellow)
		 #f)))

(set! (cursor-color) c-cursor-color)

(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! stop-playing-hook
	       (lambda (snd) 
		 (set! (cursor-color) blue)
		 ;;(gc)
		 #f)))


;; Set different color on the cursor when playing selection.
(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! start-playing-selection-hook
	       (lambda ()
		 ;;(display "Start playing selection")(newline)
		 (set! (cursor-color) green)
		 #f)))


(if (not c-setting-cursor-color-cause-redraw)
    (add-hook! stop-playing-selection-hook
	       (lambda ()
		 ;;(display "Stop playing selection")(newline)
		 (set! (cursor-color) blue)
		 #f)))



;; Let the cursor move when playing. (copied from the manual)
(add-hook! after-open-hook 
	   (lambda (snd)
	     (c-for 0 < (channels snd) 1
		    (lambda (i)
		      (set! (cursor snd i) 0)))
	     (set! (cursor-follows-play snd) #t)
	     (c-set-sound-cursor snd cursor-line)
	     #f))

;(set! (cursor-follows-play) #t)







;;##############################################################
;; Various mouse handling
;;##############################################################


;; Removes default mouse-click-hook handling. The possibility to paste with
;; mouse-button 2 is really annoying when using a wheel mouse.
(add-hook! mouse-click-hook (lambda (snd chn button state x y axis)
			      #t))


;; Moves the playing position when clicking in the editor if clicking button 1 or 4.
;; Moves cursor and stops playing if clicking buttton 5. (wheel down)
;; Moves cursor and starts playing if clicking button 4. (wheel up)


(if #t
    (if #t
	
	(add-hook! mouse-click-hook
		   (lambda (snd chn button state x y axis)
		     (define myplay c-play)
#!
		     (define (myplay samp)
		       (let ((chans (chans snd)))
			 (do ((chan 0 (1+ chan)))
			     ((= chan chans))
			   (let ((player (make-player snd chan)))
			     (add-player player samp)))
			 (start-playing chans (srate snd))))
!#
		     (if (= axis time-graph)
			 (let ((samp (inexact->exact (* (srate snd) (position->x x snd chn))))
			       (dasspeed (speed-control)))
			   (if (< samp 0) (set! samp 0))
			   (stop-playing)
			   (set! (cursor) samp)
			   (cond ((= button 4)
				  (if (< dasspeed 0)
				      (set! (speed-control) (* -1 dasspeed)))
				  (myplay samp))
				 ((= button 5)
				  (if (> dasspeed 0)
				      (set! (speed-control) (* -1 dasspeed)))
				  (c-play samp)))))
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
					 (c-play samp))))
			   (if (= button 4)
			       (begin
				 (c-play samp))))))
		     #f))))







;;##############################################################
;; Playing
;;##############################################################

;(define (play-selection-with-play)
;  (play (c-selection-position) #f #f #f (+ (c-selection-position) (c-selection-frames))))

(define (c-play-selection2)
  ;;(c-display "gakk")
  (if c-islooping
      (play-selection)
      (remove-hook! stop-playing-selection-hook c-play-selection2))
  #f)

(define (c-play-selection)
  (if c-is-stop-hooks-here
      (begin
	(if c-islooping
	    (add-hook! stop-playing-selection-hook c-play-selection2))
	(play-selection))
      (play-selection #f #f
		      (lambda x
;			(c-display "c-play-selection lambda")
			(if c-islooping
			    (c-play-selection))))))

(define (c-play2 snd)
  (if c-islooping
      (play)
      (remove-hook! stop-playing-hook c-play2))
  #f)

(define (c-play pos)
  (if c-is-stop-hooks-here
      (begin
	(if c-islooping
	    (add-hook! stop-playing-hook c-play2))
	(play pos))
      (play pos #f #f #f #f #f
	    (lambda x
	      (if c-islooping
		  (c-play pos))))))



;; Replace the old space binding with one that starts playing from the current cursor position,
;; all channels. And stops playing if allready playing.
(bind-key (char->integer #\ ) 0 
	  (lambda ()
	    (if (dac-is-running)
		(begin
		  (if c-is-stop-hooks-here
		      (begin
			(remove-hook! stop-playing-selection-hook c-play-selection2)
			(remove-hook! stop-playing-hook c-play2)))
		  (stop-playing))
		(if (c-selection?)
		    (c-play-selection)
		    (c-play (cursor))))))
;		    (play-selection-with-play)
;		    (play (cursor))))))

;; Makes the P key a pause button. If playing, stops playing, but doesn't reset the cursor pos.
;; If not playing, starts playing from cursor.
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
		  (c-play (cursor))))))






;;##############################################################
;; View
;;##############################################################

;; Let the "a" key be a "show-all" key.
(bind-key (char->integer #\a) 0
	  (lambda ()
	    (set! (x-bounds) (list 0.0 (/ (frames) (srate))))))


;; Let the "s" key be a "show-selection" key.
(bind-key (char->integer #\s) 0
	  (lambda ()
	    (set! (x-bounds) (list (/ (c-selection-position) (srate))
				   (/ (+ (c-selection-position) (c-selection-frames)) (srate))))))


(define (c-set-x-bounds x1 x2)
  (let* ((all (/ (frames) (srate)))
	 (n-x1 (max 0 x1))
	 (n-x2 (min all (+ (- n-x1 x1) x2))))
    (if (> x2 all)
	(set! n-x1 (max 0 (- n-x1 (- x2 all)))))
    (if (and (or (>= (* (abs (- n-x1 (car (x-bounds)))) (srate)) 1)
		 (>= (* (abs (- n-x2 (cadr (x-bounds)))) (srate)) 1))
	     (>= (* (srate) (- n-x2 n-x1)) (if (< (- (frames) (cursor)) 10) 10 1)))
	(set! (x-bounds) (list n-x1 n-x2)))))

(define (c-zoom zoomfactor)
  (let* ((cursor (/ (cursor) (srate)))
	 (x1 (car (x-bounds)))
	 (x2 (cadr (x-bounds)))
	 (length (- x2 x1))
	 (n-length (* zoomfactor length))
	 (n-x2 (/ (+ (* 2 cursor) n-length) 2))
	 (n-x1 (- n-x2 n-length)))
    (c-set-x-bounds n-x1 n-x2)))

;; -: Zoom out
(bind-key (char->integer #\-) 0
	  (lambda ()
	    (c-zoom c-zoomfactor)))

;; +: Zoom in
(bind-key (char->integer #\+) 0
	  (lambda ()
	    (c-zoom (/ 1 c-zoomfactor))))

;; Shows the full sound after opening.
(add-hook! initial-graph-hook
	   (lambda (snd chn dur)
	     (list 0.0 dur)))






;;##############################################################
;; Cursor position
;;##############################################################


;; Page Up/Down: +/- 30 seconds
;; Up/down:      +/- 10 seconds
;; Right/left:   +/-  2 seconds


(bind-key #xFF56 0
	  (lambda ()
	    (c-set-cursor-pos (- (cursor) (* (srate) c-cursormovetime-pageupdown)))))

(bind-key #xFF55 0
	  (lambda ()
	    (c-set-cursor-pos (+ (cursor) (* (srate) c-cursormovetime-pageupdown)))))

(bind-key #xFF52 0
	  (lambda ()
	    (c-set-cursor-pos (+ (cursor) (* (srate) c-cursormovetime-updown)))))

(bind-key #xFF54 0
	  (lambda ()
	    (c-set-cursor-pos (- (cursor) (* (srate) c-cursormovetime-updown)))))

(bind-key #xFF51 0
	  (lambda ()
	    (c-set-cursor-pos (- (cursor) (* (srate) c-cursormovetime-rightleft)))))

(bind-key #xFF53 0
	  (lambda ()
	    (c-set-cursor-pos (+ (cursor) (* (srate) c-cursormovetime-rightleft)))))

(bind-key #xFF57 0
	  (lambda ()
	    (c-set-cursor-pos 0)))

(bind-key (char->integer #\<) 0
	  (lambda ()
	    (c-set-cursor-pos 0)))

#!
Does not work.
(bind-key (char->integer #\>) 0
	  (lambda ()
	    (c-set-cursor-pos (frames))))
!#






;;##############################################################
;; Various key-bindings
;;##############################################################

;; Make snd quit when pressing C-x-c, just like emacs.
(bind-key (char->integer #\c) 4
	  (lambda ()
	    (exit))
	  #t)


;; Let "c" turn on/off controls
(bind-key (char->integer #\c) 0
	  (lambda ()
	    (set! (show-controls)
		  (if (show-controls)
		      #f
		      #t))))

#!
(add-hook! key-press-hook 
	   (lambda (snd chn key state)
	     (c-display "key: " key)))
!#




;; Key-bindings to have fun.

(bind-key (char->integer #\z) 0
	  (lambda ()
	    (set! (speed-control) (* -1 (speed-control)))))


(bind-key (char->integer #\q) 0
	  (lambda ()
	    (set! (speed-control) (* 0.75 (speed-control)))))
(bind-key (char->integer #\w) 0
          (lambda ()
            (set! (speed-control) (* (if (< (speed-control) 0)
					 (if (= 1 (random 10)) 1 -1)
					 (if (= 1 (random 10)) -1 1))
				     (/ (random 1000) 500)))))
(bind-key (char->integer #\e) 0
	  (lambda ()
	    (set! (speed-control) (* 1.25 (speed-control)))))



					

;;##############################################################
;; Set some colors
;;##############################################################

;; Bacground
(set! (selected-graph-color) c-backgroundcolor)
(set! (graph-color) c-backgroundcolor)

;; Selection
(set! (selection-color) c-selectioncolor)





;;##############################################################
;; Marks
;;##############################################################

; Let m make a named mark, and sync it if the current sound is synced.
(bind-key (char->integer #\m) 0
	  (lambda ()
	    (define (my-add-mark sample snd ch syncnum name)
	      (if (> ch -1)
		  (let ((newmark (add-mark sample snd ch)))
		    (set! (mark-sync newmark) syncnum)
		    (set! (mark-name newmark) name)
		    (my-add-mark sample snd (1- ch) syncnum name))))
	    (define my-mark-sync-max
	      (let ((max-mark-sync 1000))
		(lambda ()
		  (set! max-mark-sync (max (1+ (mark-sync-max)) (1+ max-mark-sync)))
		  max-mark-sync)))
	    (report-in-minibuffer "")
	    (prompt-in-minibuffer "mark: "
				  (lambda (ret)
				    (if (> (sync) 0)
					(my-add-mark (cursor) (selected-sound) (1- (channels)) (my-mark-sync-max) ret)
					(set! (mark-name (add-mark (cursor))) ret)))
				  (selected-sound)
				  #t)))
					      

(if (not (defined? 'mark-name->id))
    (load-from-path "marks.scm"))


;; The following mark-related lines are copied from marks.scm. For some reason the functions was commented away.

(define (eval-header sndf)
  (and (string? (comment sndf))
       (catch #t
	      (lambda ()
		(eval-string (comment sndf)))
	      (lambda args #f))))

(define (marks->string sndf)
  (let ((str (format #f "(if (not (defined? 'mark-property)) (load \"marks.scm\"))~%(let ((m #f))~%"))
	(chan 0))
    (for-each
     (lambda (chan-marks)
       (for-each 
	(lambda (m)
	  (set! str 
		(string-append str 
			       (format #f
				       "  (set! m (add-mark ~A #f ~D))~%" 
				       (mark-sample m)
				       chan)))
	  (if (and (string? (mark-name m))
		   (> (string-length (mark-name m)) 0))
	      (set! str 
		    (string-append str 
				   (format #f
					   "  (set! (mark-name m) ~S)~%"
					   (mark-name m)))))
	  (if (not (null? (mark-properties m)))
	      (set! str
		    (string-append str 
				   (format #f
					   "  (set! (mark-properties m) '~A)~%"
					   (mark-properties m))))))
	  chan-marks)
       (set! chan (1+ chan)))
     (marks sndf))
    (string-append str (format #f "  m)~%"))))
		   
(add-hook! output-comment-hook (lambda (str) (marks->string (selected-sound))))
(add-hook! after-open-hook (lambda (snd) (eval-header snd)))





;;##############################################################
;; Fix mouse-selection handling.
;;##############################################################

(define c-region-generation 0)

(if #t (begin
  (define about-to-move #f)
  (define selection-starting-point #f)
  (define stop-handling #f)

  (define (mouse-press-callback x state)
    (if (not stop-handling)
	(let* ((snd (selected-sound))
	       (chn (selected-channel))
	       (samp (inexact->exact (* (srate snd) (position->x x snd chn)))))
	  (if (< samp 0) (set! samp 0))
	  (c-show-times (cursor) #t)
	  (if (and (= state 4) (c-selection?))
	      (begin
		(if (< samp (+ (/ (c-selection-frames) 2) (c-selection-position)))
		    (set! selection-starting-point (+ (c-selection-position) (c-selection-frames)))
		    (set! selection-starting-point (c-selection-position)))
		(mouse-motion-callback x state))
	      (if (and (= state 1) (c-selection?))
		  (begin
		    (color-samples-allchans (channels) green (c-selection-position) (c-selection-frames))
		    (set! about-to-move (<array> (c-selection-position) (c-selection-frames))))
		  (set! selection-starting-point samp))))))

  (define (mouse-motion-callback x state)
    (let* ((snd (selected-sound))
	   (chn (selected-channel))
	   (samp (max 0 (inexact->exact (* (srate snd) (position->x x snd chn))))))
      (if about-to-move
	  (begin
	    (set! (selection-position) samp)
	    (set! (selection-frames) (about-to-move 1)))
	  (if (and (not stop-handling)
		   (c-selection?))
	      (let* ((selstart (c-selection-position))
		     (selframes (c-selection-frames))
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
		      (set! c-region-generation (+ c-region-generation 1))
		      (set! (selection-position) newselstart)
		      (set! (selection-frames) newselframes)))
		(c-show-times (cursor) #t))))))
    
  (define (mouse-release-callback x state)
    (define (nofunc . x)
      #t)
    (let* ((snd (selected-sound))
	   (chn (selected-channel))
	   (samp (max 0 (inexact->exact (* (srate snd) (position->x x snd chn))))))
      (if about-to-move
	  (begin
	    (add-hook! graph-hook nofunc)
	    (set! (selection-position) (about-to-move 0))
	    (set! (selection-frames) (about-to-move 1))
	    (c-cut)
	    (set! (cursor) samp)
	    (remove-hook! graph-hook nofunc)
	    (c-paste)
	    (set! (selection-position) samp)
	    (set! (selection-frames) (about-to-move 1))
	    (set! about-to-move #f))
	  (begin
	    (if (and (not stop-handling)
		     selection-starting-point)
		(if (not (= samp selection-starting-point))
		    (if (< samp selection-starting-point)
			(begin
			  (set! (selection-position) samp)
			  (set! (selection-frames) (- selection-starting-point samp)))
			(begin
			  (set! (selection-position) selection-starting-point)
			  (set! (selection-frames) (- samp selection-starting-point))))))
	    (c-show-times (cursor) #t)
	    (set! selection-starting-point #f)))))
  

  (add-hook! mix-drag-hook
	     (lambda (n) 
	       (set! selection-starting-point #f)
	       (set! stop-handling #t)
	       #f))

  (add-hook! mix-release-hook
	     (lambda (id samps)
	       (set! stop-handling #f)
	       #f))

  (add-hook! mark-hook
	     (lambda (id snd chn reason)
	       (set! stop-handling  #f)
	       #f))

  (add-hook! mark-drag-hook
	     (lambda (id)
	       (set! selection-starting-point #f)
	       (set! stop-handling #t)
	       #f))

  (add-hook! after-open-hook
	     (lambda (snd)
	       (let ((w (list-ref (channel-widgets snd 0) 0)))
		 (if (not use-gtk)	     
		     (begin
		       (XtAddEventHandler w ButtonPressMask #f 
					  (lambda (w c e f)
					    (mouse-press-callback (.x e) (.state e))))
		       (XtAddEventHandler w ButtonMotionMask #f 
					  (lambda (w c e f)
					    (mouse-motion-callback (.x e) (.state e))))
		       (XtAddEventHandler w ButtonReleaseMask #f 
					  (lambda (w c e f)
					    (mouse-release-callback (.x e) (.state e)))))
		     (let ((ispressed #f))
		       (g_signal_connect w "button_press_event"
					 (lambda (w e i)
					   (set! ispressed #t)
					   (mouse-press-callback (.x (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))))
		       (g_signal_connect w "motion_notify_event"
					 (lambda (w e i)
					   (if ispressed
					       (mouse-motion-callback (.x (GDK_EVENT_MOTION e)) (.state (GDK_EVENT_BUTTON e))))))
		       (g_signal_connect w "button_release_event"
					 (lambda (w e i)
					   (set! ispressed #f)
					   (mouse-release-callback (.x (GDK_EVENT_BUTTON e)) (.state (GDK_EVENT_BUTTON e)))
					   )))))
	       #f))))





;;##############################################################
;; Cut and Paste
;;##############################################################


;; Replace the C-y and C-w keybindings. These are equal, but writes
;; "Please wait" to the window bar, colorize the inserted region and
;; automaticly creates regions when needed if (selection-creates-region) is false.

(define c-iscolorized #f)

(define (color-samples-allchans chans color start end)
  (if (> chans 0)
      (begin
	(if (eq? color 'black)
	    (uncolor-samples (selected-sound) (- chans 1))
	    (color-samples color start end (selected-sound) (- chans 1)))
	(color-samples-allchans (- chans 1) color start end))))


(define (c-clear-report-hook snd chn)
  (remove-hook! after-graph-hook c-clear-report-hook)
  (c-report " "))



#!
;;This one could have been very nice, but the graph-hook is called very often,
;;and then the window bar changes title too often too, which is disturbing.
(add-hook! graph-hook
  (lambda (snd chn y0 y1)
    (c-report "Please wait, updating time graph...")
    (add-hook! after-graph-hook c-clear-report-hook)
    #f))
!#


(define (c-make-region)
  (if (not (selection-creates-region))
      (if (> c-region-generation 0)
	  (begin
	    (set! c-region-generation 0)
	    (c-report "Please wait, making region...")
	    (add-hook! after-graph-hook c-clear-report-hook)
	    (make-region)))))

(bind-key (char->integer #\r) 0
	  c-make-region)


(define (c-paste)
  (if (c-selection?)
      (c-make-region))
  (if (> (length (regions)) 0)
      (begin
	(c-report "Please wait, inserting region...")
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
	  (set! c-iscolorized #t)
	  (c-report "Please wait, updating time graph...")
	  (add-hook! after-graph-hook c-clear-report-hook)
	  (update-time-graph)))))

(bind-key (char->integer #\y) 4
	  c-paste)


(define (c-cut)
  (if (c-selection?)
      (let ((curspos (c-selection-position)))
	(color-samples-allchans (channels) 'black 0 0)
	(set! (cursor #t #t) curspos)
	(c-make-region)
	(c-report "Please wait, deleting selection...")
	(add-hook! after-graph-hook c-clear-report-hook)
	(delete-selection)
	(set! c-iscolorized #f))))

(bind-key (char->integer #\w) 4
	  c-cut)




;;##############################################################
;; Progress reporting (graphic)
;;##############################################################

(define c-last-report-value 0.0)

(define (progress-report pct . various) ;name current-channel channels snd)
  (let* ((sound-widget (list-ref (channel-widgets (selected-sound) 0) 0))
	 (widget-width (car (widget-size sound-widget))))
    (if (> (abs (- pct c-last-report-value)) (/ 1 widget-width))
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
	  (set! c-last-report-value pct)))))

(define  (start-progress-report . snd)
  (set! c-last-report-value 0.0)
  (report-in-minibuffer "0.00"))
(define (finish-progress-report . snd)
  (report-in-minibuffer ""))









;;##############################################################
;; Show times
;;##############################################################

;; Show cursor and selection position in minutes, seconds and 1/10th seconds.
;; This function is often called when playing, so I have tried as much as possible to avoid triggering a garbage collection.
;; (without very much (if any) success by the way). Would be extremely nice if Guile could collect garbage in a seperate thread.

(define c-show-times 

  (let (
	 
	(c-dodasprint
	 (let* ((lastpainted (<array/multidimensional> '(3 4)))
		(fontwidth 9)	
		(fontheight 12)
		(stringlen (* fontwidth 7))

		(sound-widget #f)
		(width #f)
		(x #f)
		(dim #f)

		(old-color #f)
		(height #f)
		(y #f))
	   
	   (lambda (string level color framepos . force)
	     (set! sound-widget (list-ref (channel-widgets (selected-sound) 0) 0))
	     (set! width (- (car (widget-size sound-widget)) fontwidth))
	     (set! x (min (- width stringlen) (x->position (/ framepos (srate)))))
	     (set! dim (lastpainted level))
	     
	     (if (or (not (null? force))
		     (not (= x (dim 0))))
		 (begin
		   (set! old-color (foreground-color))
		   (set! height (cadr (widget-size sound-widget)))
		   (set! y (+ (- (* fontheight level) fontheight)
			      (- (/ (- height fontheight) 2) fontheight)))

		   (if (dim 0)
		       (begin
			 (set! (foreground-color) c-backgroundcolor)
			 ;;(set! (foreground-color) green)
			 (if #f
			     (draw-string (dim 3) (dim 0) (dim 1))
			     (fill-rectangle (dim 0) (dim 1) (dim 2) (1+ fontheight)))))
		   ;;(display string)(newline)
		   (set! (foreground-color) color)
		   (draw-string string x y)

		   (-> dim set! x y stringlen string)
		   (set! (foreground-color) old-color))))))

	(get-time-string
	 (let* ((ret-string "        ")
		(intdiv (lambda (a b)
			  (inexact->exact (floor (/ a b)))))
		(int->char (lambda (i)
			     (integer->char (+ (char->integer #\0) i))))
		
		(time #f)
		(minutes #f)
		(minutes100 #f)
		(minutes10 #f)
		(minutes1 #f)
		(seconds #f)
		(seconds10 #f)
		(seconds1 #f)
		(seconds-1 #f)

		(p (lambda (n)
		     (if (< minutes 10)
			 (max 0 (- n 2))
			 (if (< minutes 100)
			     (max 0 (- n 1))
			     n)))))

	   
	   (lambda (dastime)
	     (set! time (/ (floor (* 10 (/ dastime (srate (selected-sound))))) 10))
	     (set! minutes (intdiv time 60))
	     (set! minutes100 (intdiv minutes 100))
	     (set! minutes10 (intdiv (- minutes (* minutes100 100)) 10))
	     (set! minutes1 (- minutes (* minutes100 100) (* minutes10 10)))
	     (set! seconds (/ (floor (* 10 (- time (* minutes 60)))) 10))
	     (set! seconds10 (intdiv seconds 10))
	     (set! seconds1 (inexact->exact (floor (- seconds (* seconds10 10)))))
	     (set! seconds-1 (inexact->exact (floor (* 10 (- seconds (* 10 seconds10) seconds1)))))

	     (if (< minutes 100)
		 (string-set! ret-string 7 #\ )
		 (string-set! ret-string 0 (int->char minutes100)))
	     (if (< minutes 10)
		 (string-set! ret-string 6 #\ )
		 (string-set! ret-string (p 1) (int->char minutes10)))
	     
	     (string-set! ret-string (p 2) (int->char minutes1))
	     (string-set! ret-string (p 3) #\:)
	     (string-set! ret-string (p 4) (int->char seconds10))
	     (string-set! ret-string (p 5) (int->char seconds1))
	     (string-set! ret-string (p 6) #\.)
	     (string-set! ret-string (p 7) (int->char seconds-1))
	     
	     ret-string)))
	
	
	(last-time-showed 0)
	(largestring "                                        ")

	(largetimechange #f)
	(stereocombined #f)
	(wanttoupdate #f))
    

    (lambda (dastime . force)
	     
      (set! largetimechange (>= (abs (- dastime last-time-showed))
				(/ (srate (selected-sound)) 5)))
      (set! stereocombined (and (= (channels) 2) 
				(= (channel-style (selected-sound)) channels-combined)))
      (set! wanttoupdate (or (not (null? force)) largetimechange))
      
      (if wanttoupdate
	  (set! last-time-showed dastime))
      (if stereocombined
	  (begin
	    ;;(c-dodasprint "ai ai" 0 red dastime wanttoupdate)
	    (c-dodasprint (get-time-string dastime) 0 red dastime wanttoupdate)
	    (if (and (not (null? force)) (c-selection?))
		(begin
		  (c-dodasprint (get-time-string (c-selection-position)) 1 blue (c-selection-position) #t)
		  (c-dodasprint (get-time-string (+ (c-selection-position) (c-selection-frames))) 2 blue (+ (c-selection-position) (c-selection-frames)) #t))))
	  (if wanttoupdate
	      (report-in-minibuffer (if (c-selection?)
					(begin
					  (substring-move! (get-time-string dastime) 0 7 largestring 0)
					  (substring-move! (get-time-string (c-selection-position)) 0 7 largestring 9)
					  (substring-move! (get-time-string (+ (c-selection-position) (c-selection-frames))) 0 7 largestring 18)
					  largestring)
					(get-time-string dastime))))))))



;; Show the time in the minibuffer when playing

(let ((samplecount 0))
  (add-hook! play-hook
	     (lambda (samples)
	       (set! samplecount (+ samplecount (* 40 samples)))
	       (if (> samplecount (srate (selected-sound)))
		   (begin
		     (set! samplecount 0)
		     (c-show-times (cursor))))
	       #f)))

;(define te 0)
(add-hook! after-graph-hook
	   (lambda (snd chn)
	     ;(set! te (1+ te))
	     ;(c-display "after-graph-hook " te)
	     (c-show-times (cursor) #t)
	     #f))






;;##############################################################
;; The Sync and Loop buttons
;;##############################################################

;; Doing several things after opening a file.
;; -Sync and Unite newly loaded files by default.
;;  The sync value should be unique for each sound.
;; -Replace the sync button with a new one that uses get-unique-sync-num
;;  as the sync-number. Simply using (+ 1 snd) doesn't work when a sound is closed
;;  in certain situations.
;; -Remove the play button. Its useless now with the way p and space is configured.
;; -Added a loop button where the play button was.

(define c-get-unique-sync-num
  (let ((unique-sync-num 0))
    (lambda ()
      (set! unique-sync-num (+ unique-sync-num 1))
      unique-sync-num)))

#!
(display-widget-tree (list-ref (sound-widgets (selected-sound)) 2))
(find-child (list-ref (sound-widgets (selected-sound)) 2) "snd-name-form")
!#

(add-hook! close-hook
	   (lambda (snd)
	     (c-for-each-nameform-button snd "loop" checkbutton-remove)
	     (c-for-each-nameform-button snd "sync" checkbutton-remove)
	     #f))

#!
(checkbutton-remove (c-get-nameform-button (selected-sound) "sync"))
!#

(let ((not-now #f))
  (add-hook! after-open-hook
	     (lambda (snd)
	       (let ((oldplay (c-get-nameform-button snd "play")))
		 (<checkbutton> (c-get-nameform snd)
				"loop"
				(lambda (on)
				  (if (not not-now) ;; checkbutton-set or focus-widget triggers a button-get-active callback, which again
				      ;;;;;;;;;;;;;;;; cause checkbutton-set or focus-widget to be called for all other sounds, which again... etc.: and stack runs out.
				      (begin
					(set! not-now #t)
					(set! c-islooping on)
					(for-each (lambda (s)
						    (if (not (= s snd)) (c-for-each-nameform-button s "loop"
												    (lambda (b) 
												      (checkbutton-set b c-islooping)))))
						  (sounds))
					(focus-widget (list-ref (channel-widgets snd 0) 0))
					(set! not-now #f))))
				c-islooping
				(if use-gtk '() (list XmNx (car (widget-position oldplay)))))
		 (checkbutton-remove oldplay))
	       
	       (if (> (channels snd) 1)
		   (set! (channel-style snd) channels-combined))
	       (set! (sync snd) (c-get-unique-sync-num))
	       
	       (let ((oldsync (c-get-nameform-button snd "sync")))
		 (<checkbutton> (c-get-nameform snd)
				"sync"
				(lambda (on)
				  (if on
				      (set! (sync snd) (c-get-unique-sync-num))
				      (set! (sync snd) 0))
				  (focus-widget (list-ref (channel-widgets snd 0) 0)))
				#t
				(if use-gtk '() (list XmNx (car (widget-position oldsync)))))
		 (checkbutton-remove oldsync))
	       
	       #f)))





;;##############################################################
;; Buffer-menu
;;##############################################################

;; Makes the buffer-menu a bit more pleasent to use. This is code
;; copied from examp.scm and modified. (Something is wrong here.)

(if (not (defined? 'close-all-buffers))
    (load-from-path "examp.scm"))


;;;;;;;;;;;;;;;;
;;(set! last-height 500)
;;(set! last-width 700)


;; For gtk, the -notebook switch doesn't seem to work very well.
;; For motif, the -notebook switch doesn't seem to work very well either.
(if (or (not use-gtk)
	(not (string=? "GtkNotebook" (gtk_widget_get_name (list-ref (main-widgets) 5)))))
    (begin
      (define (c-switch-to-buf filename)
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
      
      ;;(set! (widget-size (car (sound-widgets (car current-buffer)))) (list 1000 1000) )
      
      (define (c-open-buffer filename)
	"(c-open-buffer filename) adds a menu item that will select filename (use with open-hook)"
	(if (c-isloaded? filename)
	    (begin
	      (c-switch-to-buf filename)
	      #t)
	    (begin
	      (add-to-menu buffer-menu 
			   filename 
			   (lambda () (c-switch-to-buf filename)))
	      #f)))
      
      
      (define buffer-menu (add-to-main-menu "Buffers"))
      (add-hook! open-hook c-open-buffer)
      (add-hook! close-hook close-buffer)
      (bind-key (char->integer #\b) 0 (lambda (x) (switch-to-buf)))
      (add-hook! close-hook xb-close)
      (add-hook! after-open-hook xb-open)	    
      
      

      
      (define (first-time-open-soundfile . args)
	(set! (window-width) 20)
	(set! (window-height) 20)
	(remove-hook! open-hook first-time-open-soundfile)
	#f)))
      





;;##############################################################
;; Various
;;##############################################################

;;"various generally useful Snd extensions". See source.
(if (not (defined? 'check-from-unsaved-edits))
    (load-from-path "extensions.scm"))

;; When exiting.
(check-for-unsaved-edits #t)


(if (not (defined? 'selection->new))
    (load-from-path "edit-menu.scm"))


;;Show the little picture of the whole sound in the upper right corner.
(if (not (defined? 'make-current-window-display))
    (load-from-path "draw.scm"))
(make-current-window-display)


;; The background-process slows things down when the little picture is active and loading large files.
;; Better turn off the background-process when loading.
(add-hook! open-hook (lambda (filename) (if (c-isloaded? filename)
					    #t
					    (begin
					      (c-report (string-append "Please wait, making waveform-data for \"" filename "\"."))
					      (set! (with-background-processes) #f)
					      #f))))
(add-hook! after-open-hook (lambda (filename)
			     (set! (with-background-processes) #t)
			     (c-report " ")
			     #f))



;; Adds a lot of things when pressing the right mouse button. Very nice.
(if (not (defined? 'make-popup-menu))
    (load-from-path (if use-gtk
			"gtk-popup.scm"
			"popup.scm")))

(if (not (defined? 'make-effect-dialog))
    (load-from-path  (if use-gtk
			 "gtk-effects.scm"
			 "new-effects.scm")))



(if (provided? 'snd-ladspa)
    (if (not (provided? 'snd-ladspa.scm))
	(load-from-path "ladspa.scm")))


;; Stores the peak information for sounds in the ~/peaks/ directory. Seems to work correctly. I have tried
;; to fool it in many ways, but it seems to be very intelligent. Extremely nice.
;; The point is to decrease the loading time.
; First make sure the peaks directory is present
(system (string-append "mkdir " (getenv "HOME") "/peaks >/dev/null 2>/dev/null"))
(if (not (defined? 'save-peak-env-info))
    ;; Then load
    (load-from-path "peak-env.scm"))


(if (not (defined? 'for-each-child))
    (load-from-path (if use-gtk
			"snd-gtk.scm"
			"snd-motif.scm")))

;; Shows diskspace for the partition the opened sound was placed on.
;(add-hook! after-open-hook show-disk-space)

;; Add extra control for the controls dialog.
(make-hidden-controls-dialog)

;; Add rename option to the file menu.
(if (not use-gtk)
    (add-rename-option))



;; Dave Phillips fft-menu.
(if (not (defined? 'fft-list))
    (load-from-path "dlp/fft-menu.scm"))

;; Dave Phillips panic-menu
(if (not (defined? 'panic-menu))
    (load-from-path "dlp/panic.scm"))

;; Dave Phillips special-menu
(if (not (defined? 'special-menu))
    (load-from-path "dlp/special-menu.scm"))


;; (load-from-path "dlp/plugins-menu.scm")


;; All of Dave Phillips nice things.
;; Uncomment to try. (Most of it is already present)
;;(if (not (defined? 'change-selection-popup-color))
;;  (load-from-path "dlp/misc.scm"))






;;##############################################################
;; Store and restore list of used sound-files
;;##############################################################

(define (c-open-sounds-filename)
  (string-append (getenv "HOME") "/.snd_soundfilelist"))


;; Save all filenames when exiting.
(define (c-save-all-filenames filename)
  (let ((fd (open-file filename "w")))
    (if (selected-sound)
	(for-each (lambda (filename)
		    (write-line filename fd))
		  (reverse (delete-duplicates (map (lambda (snd) (file-name snd))
						   (cons (selected-sound) (sounds)))))))
    (close fd)))
  
(add-hook! exit-hook (lambda args
		       (c-save-all-filenames (c-open-sounds-filename))
		       #f))
				   


;; Load files from previous session.

(if c-restore-previous-session
    (begin
      (system (string-append "touch " (c-open-sounds-filename) " >/dev/null 2>/dev/null"))
      (let ((fd (open-file (c-open-sounds-filename) "r")))
	(define (myread)
	  (let ((line (read-line fd)))
	    (if (not (eof-object? line))
		(begin
		  (catch #t
			 (lambda ()
			   (open-sound line))
			 (lambda (key . args)
			   (c-display "File \"" line  "\" not found.")
			   #f))
		  (myread))
		(begin
		  ;;(set! (auto-resize) #f)
		  (close fd)
		  (in (if use-gtk 0 2000)
		      (lambda ()
			(set! (window-width) 800)
			(set! (window-height) 600)))))))
	;;(set! (auto-resize) #t)
	(myread))))



