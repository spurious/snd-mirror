;;; specialize popup menus
;;;
;;; currently there are five special popup menus:
;;;   selection fft time-domain lisp-listener edit-history
;;;
;;; (add-popups) creates popup menus specialized for the fft, selection, and time-domain sections of the graph
;;;    (change-selection-popup-color new-color) to change selection memu's color
;;;    (change-fft-popup-color new-color) to change fft menu's color
;;;    (change-graph-popup-color new-color) to change time-domain menu's color
;;; (add-listener-popup) posts a special popup menu if the pointer is in the listener
;;;    (change-listener-popup-color new-color) to change its color
;;; a popup menu is also added to each edit history pane to display the "spreadsheet" edit-list->function menu

(use-modules (ice-9 common-list) (ice-9 format))
(provide 'snd-popup.scm)

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "popup.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "Init_libxm"))))

(if (not (defined? 'for-each-child))
    (define (for-each-child w func)
      "(for-each-child widget func) applies func to widget and each of its children"
      (func w)
      (if (XtIsComposite w)
	  (for-each 
	   (lambda (n)
	     (for-each-child n func))
	   (cadr (XtGetValues w (list XmNchildren 0) 1))))))

(define (change-label w new-label)
  "(change-label widget new-label) changes widget's label to be new-label"
  (let ((str (XmStringCreateLocalized new-label)))
    (XtSetValues w (list XmNlabelString str))
    (XmStringFree str)))
      
(define (current-label w)
  "(current-label widget) returns widget's label"
  (let ((xmstr (cadr (XtGetValues w (list XmNlabelString 0)))))
    (cadr (XmStringGetLtoR xmstr XmFONTLIST_DEFAULT_TAG))))



(define (make-popup-menu name parent top-args entries)
  "(make-popup-menu name parent top-args entries) creates a popup menu"
  (let ((menu (XmCreatePopupMenu parent name top-args)))
    (for-each
     (lambda (entry)
       ;; entry is list: name type args optional-callback optional-func
       (let ((widget (XtCreateManagedWidget (car entry)
					    (cadr entry)
					    menu
					    (caddr entry))))
	 (if (> (length entry) 3)
	     (begin
	       (XtAddCallback widget XmNactivateCallback (list-ref entry 3))
	       (if (> (length entry) 4)
		   ((list-ref entry 4) widget))))))
     entries)
    menu))

;;; -------- selection popup

(define selection-popup-menu 
  ;; used in graph if pointer is inside selected portion
  (let ((every-menu (list XmNbackground (highlight-color)))
	(stopping #f)
	(stopping1 #f)
	(stop-widget #f)
	(stop-widget1 #f))
    (make-popup-menu 
     "selection-popup"
     (caddr (main-widgets))
     (list XmNpopupEnabled XmPOPUP_AUTOMATIC
	   XmNbackground (highlight-color))
     (list
      (list "Selection" xmLabelWidgetClass      every-menu)
      (list "sep"       xmSeparatorWidgetClass  every-menu)
      (list "Play"      xmPushButtonWidgetClass every-menu 
	    (lambda (w c i) 
	      (if stopping
		  (begin
		    (set! stopping #f)
		    (change-label w "Play")
		    (if stopping1
			(begin
			  (set! stopping1 #f)
			  (change-label stop-widget1 "Loop play")))
		    (stop-playing)) ; stops all including possible looping play
		  (begin
		    (change-label w "Stop")
		    (set! stop-widget w)
		    (set! stopping #t)
		    (play-selection)))))
      (list "Loop play"      xmPushButtonWidgetClass every-menu ; play over and over
	    (lambda (w c i) 
	      (define (stop-playing-selection)
		(set! stopping1 #f)
		(change-label w "Loop play")
		(if stopping
		    (begin
		      (set! stopping #f)
		      (change-label stop-widget "Play"))))
	      (define (play-selection-again reason)
		(if (and (not (c-g?))
			 (= reason 0)
			 stopping1)
		    (play-selection #f play-selection-again)
		    (stop-playing-selection)))
	      (if stopping1
		  (begin
		    (stop-playing-selection)
		    (stop-playing))
		  (begin
		    (change-label w "Stop!")
		    (set! stop-widget1 w) ; needs to be separate from Play case since we're stopping/restarting deliberately
		    (set! stopping1 #t)
		    (play-selection #f play-selection-again)))))
      (list "Delete"    xmPushButtonWidgetClass every-menu (lambda (w c i) (delete-selection)))
      (list "Zero"      xmPushButtonWidgetClass every-menu (lambda (w c i) (scale-selection-by 0.0)))
      (list "Crop"      xmPushButtonWidgetClass every-menu
	    (lambda (w c i)
	      ;; delete everything except selection
	      (for-each
	       (lambda (selection)
		 (as-one-edit
		  (lambda ()
		    (let* ((snd (car selection))
			   (chn (cadr selection))
			   (beg (selection-position snd chn))
			   (len (selection-frames snd chn)))
		      (if (> beg 0) 
			  (delete-samples 0 beg snd chn))
		      (if (< len (frames snd chn))
			  (delete-samples (+ len 1) (- (frames snd chn) len) snd chn))))))
	       (let ((sndlist '()))
		 (for-each (lambda (snd)
			     (do ((i (1- (channels snd)) (1- i)))
				 ((< i 0))
			       (if (selection-member? snd i)
				   (set! sndlist (cons (list snd i) sndlist)))))
			   (sounds))
		 sndlist))))
      (list "Save as"   xmPushButtonWidgetClass every-menu (lambda (w c i) (save-selection-dialog)))
      (list "Copy->New" xmPushButtonWidgetClass every-menu 
	    (lambda (w c i) 
	      (let ((new-file-name (snd-tempnam)))
		(save-selection new-file-name)
		(open-sound new-file-name))))
      (list "Cut->New"   xmPushButtonWidgetClass every-menu 
	    (lambda (w c i) 
	      (let ((new-file-name (snd-tempnam)))
		(save-selection new-file-name)
		(delete-selection)
		(open-sound new-file-name))))
      (list "Snap marks" xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (for-each 
	       (lambda (select)
		 (let ((pos  (apply selection-position select))
		       (len  (apply selection-frames select)))
		   (apply add-mark pos select)
		   (apply add-mark (+ pos len) select)))
	       (selection-members))))
      (list "Selection Info"      xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (let ((beg (selection-position))
		    (len (selection-frames)))
		(info-dialog 
		 "Selection info"
		 (format #f "start ~A, ~,3F~%end: ~A, ~,3F~%duration: ~A, ~,3F~%chans: ~D~%maxamp: ~,3F"
			 beg (exact->inexact (/ beg (srate)))
			 (+ beg len) (exact->inexact (/ (+ beg len) (srate)))
			 len (exact->inexact (/ len (srate)))
			 (selection-chans)
			 (selection-maxamp))))))
      (list "Apply controls" xmPushButtonWidgetClass every-menu (lambda (w c i) (apply-controls (selected-sound) 2))) ; 2=selection
      (list "Reset controls" xmPushButtonWidgetClass every-menu (lambda (w c i) (reset-controls)))
      (list "Unselect"       xmPushButtonWidgetClass every-menu (lambda (w c i) (set! (selection-member? #t) #f)))
      (list "Reverse"        xmPushButtonWidgetClass every-menu (lambda (w c i) (reverse-selection)))
      (list "Mix"            xmPushButtonWidgetClass every-menu (lambda (w c i) (mix-selection (cursor))))
      (list "Invert"         xmPushButtonWidgetClass every-menu (lambda (w c i) (scale-selection-by -1)))))))


;;; -------- time domain popup

(define graph-popup-snd #f)
(define graph-popup-chn #f)

(define graph-popup-menu 
  ;; used within graph if pointer is not inside selected portion
  (let ((every-menu (list XmNbackground (highlight-color)))
	(stopping #f)
	(stop-widget #f))
    (define (vector-print v)
      (if (< (vector-length v) 3)
	  (object->string v)
	  (let ((str (format #f "'#(~A" (vector-ref v 0))))
	    (do ((i 1 (1+ i)))
		((= i 3))
	      (set! str (string-append str " " (object->string (vector-ref v i)))))
	    (string-append str " ...)"))))
    (define (display-properties props)
      ;; there's no way to tell Guile's format that enormous vectors should not be printed in full
      ;; so we search for them here and handle them ourselves
      (let ((str ""))
	(for-each
	 (lambda (pr)
	   (let ((property (car pr))
		 (value (cdr pr)))
	     (set! str (string-append str (string #\newline) "    " (object->string property) ": "))
	     (if (vector? value)
		 (set! str (string-append str (vector-print value)))
		 (if (list? value)
		     (let ((prev #f))
		       (set! str (string-append str "'("))
		       (for-each
			(lambda (v)
			  (if prev 
			      (set! str (string-append str " "))
			      (set! prev #t))
			  (if (vector? v)
			      (set! str (string-append str (vector-print v)))
			      (set! str (string-append str (object->string v)))))
			value)
		       (set! str (string-append str ")")))
		     (set! str (string-append str (object->string value)))))))
	 props)
	str))

    (add-hook! stop-playing-hook
	       (lambda (snd) 
		 (if stopping
		     (begin
		       (set! stopping #f)
		       (if (Widget? stop-widget)
			   (change-label stop-widget "Play"))))))

    (make-popup-menu 
     "graph-popup"
     (caddr (main-widgets))
     (list XmNpopupEnabled XmPOPUP_AUTOMATIC
	   XmNbackground (highlight-color))
     (list

      (list "Snd"                xmLabelWidgetClass      every-menu) 
      (list "sep"                xmSeparatorWidgetClass  every-menu)
      (list "Play"               xmPushButtonWidgetClass every-menu 
	    (lambda (w c i) 
	      (if stopping
		  (begin
		    (set! stopping #f)
		    (change-label w "Play")
		    (stop-playing))
		  (begin
		    (change-label w "Stop")
		    (set! stopping #t)
		    (play 0 graph-popup-snd))))
	    (lambda (wid)
	      (set! stop-widget wid)))
      (list "Play channel"       xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (set! stopping #t)
	      (change-label stop-widget "Stop")
	      (play 0 graph-popup-snd graph-popup-chn)))
      (list "Play from cursor"   xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (set! stopping #t)
	      (change-label stop-widget "Stop")
	      (play (cursor graph-popup-snd graph-popup-chn) graph-popup-snd)))
      (list "Play previous"      xmPushButtonWidgetClass every-menu
	    (lambda (w c i)
	      (set! stopping #t)
	      (change-label stop-widget "Stop")
	      (play 0 graph-popup-snd graph-popup-chn #f #f (1- (edit-position)))))  ; play version before most-recent edit
      (list "Play original"      xmPushButtonWidgetClass every-menu
	    (lambda (w c i)
	      (set! stopping #t)
	      (change-label stop-widget "Stop")
	      (play 0 graph-popup-snd graph-popup-chn #f #f 0)))                     ; play unedited version
      (list "Undo"               xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (undo 1 graph-popup-snd graph-popup-chn)))
      (list "Redo"               xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (redo 1 graph-popup-snd graph-popup-chn)))
      (list "Revert"             xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (revert-sound graph-popup-snd)))
      (list "Save"               xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (save-sound graph-popup-snd)))
      (list "Save as"            xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (select-sound graph-popup-snd)
	      (save-sound-dialog)))
      (list "Close"              xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (close-sound graph-popup-snd)))
      (list "Mix selection"      xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (mix-selection (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
      (list "Insert selection"   xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (insert-selection (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
      (list "Replace with selection"   xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (let* ((snd graph-popup-snd)
		     (chn graph-popup-chn)
		     (beg (cursor snd chn))
		     (len (selection-frames))
		     (sbeg (selection-position)))
		(if (or (not (selection-member? snd chn))
			(< (+ beg len) sbeg)
			(> beg (+ sbeg len)))
		    (begin
		      (delete-samples beg len snd chn)
		      (insert-selection beg snd chn))
		    (if (< beg sbeg)
			(delete-samples beg (- sbeg beg) snd chn)
			;(snd-warning "replace at ~D would collide with selected portion")
			)))))
      (list "Select all"         xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (select-all graph-popup-snd graph-popup-chn)))
      (list "Unselect"           xmPushButtonWidgetClass every-menu 
	    (lambda (w c i) 
	      (set! (selection-member? #t) #f)))
      (list "Equalize panes"     xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (equalize-panes)))
      (list "Apply controls"     xmPushButtonWidgetClass every-menu
	    (lambda (w c i)
	      (apply-controls)))
      (list "Reset controls"     xmPushButtonWidgetClass every-menu
	    (lambda (w c i)
	      (reset-controls)))
      (list "Info"               xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (let ((snd graph-popup-snd))
		(info-dialog 
		 (format #f "~A info" (file-name snd))
		 (format #f "~A:~%  chans: ~D~%  srate: ~D~%  header: ~A~%  data format: ~A~%  length: ~1,3F~%  maxamp: ~A~%~A~A~A~A~A"
			(short-file-name snd)
			(chans snd)
			(srate snd)
			(mus-header-type-name (header-type snd))
			(mus-data-format-name (data-format snd))
			(exact->inexact (/ (frames snd graph-popup-chn) (srate snd)))
			(maxamp snd #t)
			(if (comment snd)
			    (format #f "  comment: \"~A\"~%" (comment snd))
			    "")
			(let ((loops (mus-sound-loop-info (file-name snd))))
			  (if (not (null? loops))
			      (format #f "  loop: ~A~%" loops)
			      ""))
			(if (= (header-type snd) mus-soundfont)
			    (format #f "  sounds: ~:{~%     ~S start: ~A, loop: ~A ~A~}" (soundfont-info))
			    "")
			(if (not (null? (sound-properties snd)))
			    (format #f "  properties: ~A"
				    (display-properties (sound-properties snd)))
			    "")
			(let ((chan-str ""))
			  (do ((i 0 (1+ i)))
			      ((= i (chans snd)))
			    (if (not (null? (channel-properties snd i)))
				(set! chan-str 
				      (string-append chan-str
						     (format #f "~%  chan ~D properties: ~A" 
							     i (display-properties (channel-properties snd i)))))))
			  chan-str)
			)))))
      (list "Add mark"           xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (add-mark (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
      (list "Delete mark"        xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      ;; find mark closest to cursor and delete it
	      (let* ((ms (marks graph-popup-snd graph-popup-chn))
		     (id (if (null? ms)
			     #f
			     (if (= (length ms) 1)
				 (car ms)
				 (let ((loc (cursor)))
				   (define (find-closest-mark lst cur-min cur-id)
				     (if (null? lst)
					 cur-id
					 (let* ((this-id (car lst))
						(this-min (abs (- loc (mark-sample this-id)))))
					   (if (< this-min cur-min)
					       (find-closest-mark (cdr lst) this-min this-id)
					       (find-closest-mark (cdr lst) cur-min cur-id)))))
				   (find-closest-mark (cdr ms) 
						      (abs (- loc (mark-sample (car ms)))) 
						      (car ms)))))))
		(if id
		    (delete-mark id)))))
      (list "To next mark"       xmPushButtonWidgetClass every-menu
            (lambda (w c i)
              (key (char->integer #\j) 4 graph-popup-snd graph-popup-chn)))
      (list "To last mark"       xmPushButtonWidgetClass every-menu
            (lambda (w c i)
	      (key (char->integer #\-) 4 graph-popup-snd graph-popup-chn)
	      (key (char->integer #\j) 4 graph-popup-snd graph-popup-chn)))
      (list "sep"                xmSeparatorWidgetClass  every-menu)
      (list "Exit"               xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (exit)))
      ))))

(define (edit-graph-popup-menu snd chn)
  "(edit-graph-popup-menu snd chn) hides otiose entries, relabel others to reflect current state of snd and chn"
  (let ((eds (edits snd chn)))
    (for-each-child
     graph-popup-menu
     (lambda (w)
       (let ((name (XtName w)))
	 (if (string=? name "Snd")
	     (if (> (chans snd) 1)
		 (change-label w (format #f "~A[~D]" (short-file-name snd) chn))
		 (change-label w (short-file-name snd)))
	     (if (or (string=? name "Save")
		     (string=? name "Undo")
		     (string=? name "Revert")
		     (string=? name "Play previous"))
		 ((if (> (car eds) 0) XtManageChild XtUnmanageChild) w)
		 (if (string=? name "Play channel")
		     ((if (> (chans snd) 1) XtManageChild XtUnmanageChild) w)
		     (if (string=? name "Equalize panes")
			 ((if (or (> (chans snd) 1) 
				  (> (length (sounds)) 1))
			      XtManageChild XtUnmanageChild) w)
			 (if (string=? name "Redo")
			     ((if (> (cadr eds) 0) XtManageChild XtUnmanageChild) w)
			     (if (or (string=? name "Mix selection")
				     (string=? name "Insert selection")
				     (string=? name "Unselect")
				     (string=? name "Replace with selection"))
				 ((if (selection?) XtManageChild XtUnmanageChild) w)
				 (if (string=? name "Play from cursor")
				     ((if (> (cursor snd chn) 0) XtManageChild XtUnmanageChild) w)
				     (if (string=? name "Play original")
					 ((if (> (car eds) 1) XtManageChild XtUnmanageChild) w)
					 (if (or (string=? name "Delete mark")
						 (string=? name "To next mark")
						 (string=? name "To last mark"))
					     ((if (null? (marks snd chn)) XtUnmanageChild XtManageChild) w)))))))))))))))


;;; -------- fft popup (easier to access than Options:Transform)

(define (make-simple-popdown-menu label popdown-labels parent cascade-func args)
  "(make-simple-popdown-menu label popdown-labels parent cascade-func args)"
  (let* ((top (XmCreatePulldownMenu parent label args))
	 (top-cascade (XtCreateManagedWidget label xmCascadeButtonWidgetClass parent
			(append (list XmNsubMenuId top)
				args)))
	 (children (map (lambda (poplab)
			  (let ((child (XtCreateManagedWidget (car poplab) xmPushButtonWidgetClass top args)))
			    (XtAddCallback child XmNactivateCallback (cadr poplab))
			    child))
			popdown-labels)))
      (if cascade-func 
	  (XtAddCallback top-cascade XmNcascadingCallback 
            (lambda (w c i)
              (cascade-func children))))))

(define (edit-fft-popup-menu snd chn)
  "(edit-fft-popup-menu snd chn) changes the fft-related popup menu to reflect the state of snd and chn"
  (for-each-child
   fft-popup-menu
   (lambda (w)
     (let ((name (XtName w)))
       (if (string=? name "Peaks")
	   (change-label w (if (show-transform-peaks snd chn) "No peaks" "Peaks"))
	   (if (string=? name "dB")
	       (change-label w (if (fft-log-magnitude snd chn) "Linear" "dB"))
	       (if (string=? name "Log freq")
		   (change-label w (if (fft-log-frequency snd chn) "Linear freq" "Log freq")))))))))

(define fft-popup-menu 
  ;; used within graph if pointer is in the fft graph
  (let* ((every-menu (list XmNbackground (highlight-color)))
	 (fft-popup (XmCreatePopupMenu (caddr (main-widgets)) "fft-popup"
		       (append (list XmNpopupEnabled XmPOPUP_AUTOMATIC) every-menu))))

    (define (choose-chan)
      (if (= (channel-style graph-popup-snd) channels-separate) graph-popup-chn #t))
    
    (XtCreateManagedWidget "Transform" xmLabelWidgetClass fft-popup every-menu)
    (XtCreateManagedWidget "sep" xmSeparatorWidgetClass fft-popup every-menu)

    (let ((peaks (XtCreateManagedWidget "Peaks" xmPushButtonWidgetClass fft-popup every-menu)))
      (XtAddCallback peaks XmNactivateCallback
         (lambda (w c i)
	   (set! (show-transform-peaks graph-popup-snd (choose-chan)) (not (show-transform-peaks graph-popup-snd graph-popup-chn))))))

    (let ((db (XtCreateManagedWidget "dB" xmPushButtonWidgetClass fft-popup every-menu)))
      (XtAddCallback db XmNactivateCallback
         (lambda (w c i)
	   (set! (fft-log-magnitude graph-popup-snd (choose-chan)) (not (fft-log-magnitude graph-popup-snd graph-popup-chn))))))

    (let ((logfreq (XtCreateManagedWidget "Log freq" xmPushButtonWidgetClass fft-popup every-menu)))
      (XtAddCallback logfreq XmNactivateCallback
         (lambda (w c i)
	   (set! (fft-log-frequency graph-popup-snd (choose-chan)) (not (fft-log-frequency graph-popup-snd graph-popup-chn))))))

    (let ((norm (XtCreateManagedWidget "Normalize" xmPushButtonWidgetClass fft-popup every-menu)))
      (XtAddCallback norm XmNactivateCallback
         (lambda (w c i)
	   (if (= (transform-normalization graph-popup-snd graph-popup-chn) dont-normalize)
	       (set! (transform-normalization graph-popup-snd (choose-chan)) normalize-by-channel)
	       (set! (transform-normalization graph-popup-snd (choose-chan)) dont-normalize)))))

    (make-simple-popdown-menu 
     "Graph type" 
     (map (lambda (name val)
		  (list name
			(lambda (w c i) 
			  (set! (transform-graph-type graph-popup-snd (choose-chan)) val))))
		(list "once" "sonogram" "spectrogram")
		(list graph-once graph-as-sonogram graph-as-spectrogram))
     fft-popup 
     (lambda (lst)
       (let ((ctr 0))
	 (for-each 
	  (lambda (child)
	    (XtSetSensitive child (not (= (transform-graph-type graph-popup-snd graph-popup-chn) ctr)))
	    (set! ctr (+ ctr 1)))
	  lst)))
     every-menu)

    (let ((sizes (list 16 64 256 1024 4096 16384 65536 262144 1048576)))
      (make-simple-popdown-menu
       "Size"
       (map (lambda (name val)
	      (list name
		    (lambda (w c i) 
		      (set! (transform-size graph-popup-snd (choose-chan)) val))))
	    (map (lambda (n) (number->string n)) sizes)
	    sizes)
     fft-popup
     (lambda (lst)
       (for-each 
	(lambda (child size)
	  (XtSetSensitive child (not (= (transform-size graph-popup-snd graph-popup-chn) size))))
	lst sizes))
     every-menu))

    (let ((windows (list rectangular-window hann-window welch-window parzen-window bartlett-window hamming-window blackman2-window 
			 blackman3-window blackman4-window exponential-window riemann-window kaiser-window cauchy-window 
			 poisson-window gaussian-window tukey-window dolph-chebyshev-window hann-poisson-window connes-window
			 samaraki-window ultraspherical-window)))
      (make-simple-popdown-menu
       "Window"
       (map (lambda (name val)
	      (list name
		    (lambda (w c i) 
		      (set! (fft-window graph-popup-snd (choose-chan)) val))))
	    (list "Rectangular" "Hann" "Welch" "Parzen" "Bartlett" "Hamming" "Blackman2" "Blackman3" "Blackman4"
		  "Exponential" "Riemann" "Kaiser" "Cauchy" "Poisson" "Gaussian" "Tukey" "Dolph-Chebyshev" "Hann-Poisson" 
		  "Connes" "Samaraki" "Ultraspherical")
	    windows)
       fft-popup
       (lambda (lst)
	 (for-each 
	  (lambda (child window)
	    (XtSetSensitive child (not (= (fft-window graph-popup-snd graph-popup-chn) window))))
	  lst windows))
       every-menu))

    (let ((types (list fourier-transform wavelet-transform autocorrelation cepstrum walsh-transform haar-transform)))
      (make-simple-popdown-menu 
       "Transform type"
       (map (lambda (name val)
	      (list name 
		    (lambda (w c i)
		      (set! (transform-type graph-popup-snd (choose-chan)) val))))
	    (list "Fourier" "Wavelet" "Autocorrelate" "Cepstrum" "Walsh" "Haar")
	    types)
       fft-popup 
       (lambda (lst)
	 (for-each 
	  (lambda (child type)
	    (XtSetSensitive child (not (= (transform-type graph-popup-snd graph-popup-chn) type))))
	  lst types))
       every-menu))

    (let ((types (list "daub4" "daub6" "daub8" "daub10" "daub12" "daub14" "daub16" "daub18" "daub20" "battle_lemarie" 
		       "burt_adelson" "beylkin" "coif2" "coif4" "coif6" "sym2" "sym3" "sym4" "sym5" "sym6"))
	  (vals (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
      (make-simple-popdown-menu 
       "Wavelet type"
       (map (lambda (name val)
	      (list name 
		    (lambda (w c i)
		      (set! (wavelet-type graph-popup-snd (choose-chan)) val))))
	    types
	    vals)
       fft-popup 
       (lambda (lst)
	 (for-each 
	  (lambda (child val)
	    (XtSetSensitive child (not (= (wavelet-type graph-popup-snd graph-popup-chn) val))))
	  lst vals))
       every-menu))

    (let ((color (XtCreateManagedWidget "Color" xmPushButtonWidgetClass fft-popup every-menu)))
      (XtAddCallback color XmNactivateCallback (lambda (w c i) (color-dialog))))

    (let ((orient (XtCreateManagedWidget "Orientation" xmPushButtonWidgetClass fft-popup every-menu)))
      (XtAddCallback orient XmNactivateCallback (lambda (w c i) (orientation-dialog))))

    fft-popup))


;;; -------- edit history popup

(define edhist-funcs '())
(define edhist-widgets '()) ; the apply popdown list
(define edhist-snd #f)
(define edhist-chn #f)

(define (edhist-clear-edits w c i)
  (set! edhist-funcs '())
  #f)

(define (edhist-save-edits w c i)
  (let* ((old-val (assoc (cons edhist-snd edhist-chn) edhist-funcs))
	 (cur-edits (edits edhist-snd edhist-chn))
	 (new-func (edit-list->function edhist-snd edhist-chn (1+ (car cur-edits)) (apply + cur-edits))))
    (if old-val
	(set-cdr! old-val new-func)
	;; perhaps this should save the previous function under the current file name?
	(set! edhist-funcs (cons (cons (cons edhist-snd edhist-chn) new-func) edhist-funcs)))
    #f))

(define (edhist-reapply-edits w c i)
  (let* ((old-val (assoc (cons edhist-snd edhist-chn) edhist-funcs)))
    (if old-val	((cdr old-val) edhist-snd edhist-chn))))

(define (edhist-apply-edits w c i)
  ;; the current popdown widgets are in edhist-widgets, current funcs are in edhist-funcs
  ;; edhist-apply (for new widgets) is car of edhist-widgets

  (define (edhist-apply w c i)
    (let ((index (cadr (XtVaGetValues w (list XmNuserData 0)))))
      (if (and (> index 0)
	       (<= index (length edhist-funcs)))
	  ((cdr (list-ref edhist-funcs (1- index))) edhist-snd edhist-chn))))
	  
  (let ((wids (cdr edhist-widgets))
	(funcs edhist-funcs)
	(parent (car edhist-widgets))
	(index 1))
    (if (not (null? funcs))
	(for-each
	 (lambda (func)
	   (let* ((label (car func))
		  (button (if (not (null? wids))
			      (let ((wid (car wids)))
				(set! wids (cdr wids))
				(XtManageChild wid)
				wid)
			      (let ((wid (XtCreateManagedWidget "wid" xmPushButtonWidgetClass parent (list XmNbackground (highlight-color)))))
				(set! edhist-widgets (append edhist-widgets (list wid)))
				(XtAddCallback wid XmNactivateCallback edhist-apply)
				wid))))
	     (if (pair? label)
		 (change-label button (format #f "~A[~A]" (short-file-name (car label)) (cdr label)))
		 (change-label button label))
	     (XtVaSetValues button (list XmNuserData index))
	     (set! index (1+ index))))
	 edhist-funcs))
    (if (not (null? wids))
	(for-each
	 (lambda (w)
	   (XtUnmanageChild w))
	 wids)))
  #f)

(define (edhist-help-edits w c i)
  (help-dialog "Edit History Functions"
	       "This popup menu gives access to the edit-list function handlers in Snd. \
At any time you can backup in the edit list, 'save' the current trailing edits, make some \
new set of edits, then 'reapply' the saved edits.  The 'apply' choice gives access to all \
currently saved edit lists -- any such list can be applied to any channel.  'Clear' deletes \
all saved edit lists."
	       (list "{edit lists}" "{edit-list->function}")
	       (list "extsnd.html#editlists" "extsnd.html#editlist_to_function")
	       ))

(define edit-history-menu
  (let* ((every-menu (list XmNbackground (highlight-color)))
	 (edhist-popup (XmCreatePopupMenu (caddr (main-widgets)) "edhist-popup"
					  (append (list XmNpopupEnabled XmPOPUP_AUTOMATIC) every-menu))))
    (XtCreateManagedWidget "Edits" xmLabelWidgetClass edhist-popup every-menu)
    (XtCreateManagedWidget "sep" xmSeparatorWidgetClass edhist-popup every-menu)
    (let ((edhist-save (XtCreateManagedWidget "Save" xmPushButtonWidgetClass edhist-popup every-menu)))
      (XtAddCallback edhist-save XmNactivateCallback edhist-save-edits))
    (let ((edhist-reapply (XtCreateManagedWidget "Reapply" xmPushButtonWidgetClass edhist-popup every-menu)))
      (XtAddCallback edhist-reapply XmNactivateCallback edhist-reapply-edits))
    (let* ((edhist-apply (XmCreatePulldownMenu edhist-popup "Apply" every-menu))
	   (apply-cascade (XtCreateManagedWidget "Apply" xmCascadeButtonWidgetClass edhist-popup
						 (append (list XmNsubMenuId edhist-apply)
							 every-menu))))
      (XtAddCallback apply-cascade XmNcascadingCallback edhist-apply-edits)
      (set! edhist-widgets (list edhist-apply)))
    (let ((edhist-clear (XtCreateManagedWidget "Clear" xmPushButtonWidgetClass edhist-popup every-menu)))
      (XtAddCallback edhist-clear XmNactivateCallback edhist-clear-edits))
    (let ((edhist-help (XtCreateManagedWidget "Help" xmPushButtonWidgetClass edhist-popup every-menu)))
      (XtAddCallback edhist-help XmNactivateCallback edhist-help-edits))
    (add-hook! close-hook (lambda (snd)
			    (let ((chns (chans snd))
				  (name (short-file-name snd)))
			      (do ((i 0 (1+ i)))
				  ((= i chns))
				(let* ((old-val (assoc (cons snd i) edhist-funcs)))
				  (if old-val
				      (set-car! old-val (format #f "~A[~A]" name i))))))))
    edhist-popup))

(define (edit-history-popup-menu snd chn)
  (set! edhist-snd snd)
  (set! edhist-chn chn)
  edit-history-menu)



;;; -------- activate the above menus

(define (add-popups)
  "(add-popups) adds context-sensitive popup menus to various parts of the interface"
  (let ((popups '()))

    (define (find-popup snd chn dats)
      (if (not (null? dats))
	  (let ((cur (car dats)))
	    (if (and (= (car cur) snd)
		     (= (cadr cur) chn))
		cur
		(find-popup snd chn (cdr dats))))
	  #f))

    (define (add-popup snd)
      (do ((chn 0 (1+ chn)))
	  ((= chn (chans snd)))
	(if (not (find-popup snd chn popups))
	    (let ((chn-grf (car (channel-widgets snd chn))))
	      (set! popups (cons (list snd chn) popups))

	      ;; edit history list spreadsheet support
	      (XtAddCallback  (list-ref (channel-widgets snd chn) 7) XmNpopupHandlerCallback 
		 (lambda (w data info)
		   (let ((e (.event info)))
		     (if (= ButtonPress (.type e))
			 (begin
			   (if edit-history-menu
			       (for-each-child
				edit-history-menu
				(lambda (w)
				  (let ((name (XtName w)))
				    (if (string=? name "Clear")
					(XtSetSensitive w (not (null? edhist-funcs)))
					(if (string=? name "Save")
					    (XtSetSensitive w (> (apply + (edits snd chn)) 0))
					    (if (string=? name "Apply")
						(XtSetSensitive w (not (null? edhist-funcs)))
						(if (string=? name "Reapply")
						    (XtSetSensitive w (not (eq? #f (assoc (cons snd chn) edhist-funcs))))))))))))
			   (set! (.menuToPost info) (edit-history-popup-menu snd chn)))))))

	      (XtAddCallback chn-grf XmNpopupHandlerCallback 
		 (lambda (w data info)
		   (let* ((e (.event info))
			  (xe (- (.x_root e) (car (XtTranslateCoords w 0 0)))))
		     (if (= ButtonPress (.type e))
			 (begin
			   ;; xe is where the mouse-click occurred in the graph window's (local) coordinates
			   ;;   not the same as (.x e), which appears to be in the outer shell's coordinates
			   (set! graph-popup-snd snd)
			   (set! graph-popup-chn chn)
			   (if (= (channel-style snd) channels-combined)
			       (let ((ye (.y e))) ; y axis location of mouse-down
				 (call-with-current-continuation
				  (lambda (break)
				    (do ((i 0 (1+ i)))
					((= i (chans snd)))
				      (let ((off (list-ref (axis-info snd i) 14)))
					(if (< ye off)
					    (begin
					      (set! graph-popup-chn (- i 1))
					      (break)))))
				    (set! graph-popup-chn (- (chans snd) 1))))))
			   
			   (let ((fax (if (transform-graph? snd chn) (axis-info snd chn transform-graph) #f))
				 (lax (if (lisp-graph? snd chn) (axis-info snd chn lisp-graph) #f)))
			     (if (and fax
				      (>= xe (list-ref fax 10))
				      (<= xe (list-ref fax 12)))
				 ;; in fft
				 (begin
				   (edit-fft-popup-menu snd chn)
				   (set! (.menuToPost info) fft-popup-menu))
				 
				 (if (and lax
					  (>= xe (list-ref lax 10))
					  (<= xe (list-ref lax 12)))
				     ;; in lisp
				     ;;   nothing special implemented yet
				     ;; (set! (.menuToPost info) graph-popup-menu)
				     #f ; just a place-holder
				     
				     (if (and (selection?)
					      (let* ((beg (/ (selection-position snd graph-popup-chn) (srate snd)))
						     (end (/ (+ (selection-position snd graph-popup-chn) 
								(selection-frames snd graph-popup-chn)) 
							     (srate snd))))
						(and (>= xe (x->position beg snd chn))
						     (<= xe (x->position end snd chn)))))
					 (set! (.menuToPost info) selection-popup-menu)
					 
					 (begin
					   (edit-graph-popup-menu graph-popup-snd graph-popup-chn)
					   (set! (.menuToPost info) graph-popup-menu)))))))))))))))

    (add-hook! after-open-hook add-popup)
    (for-each add-popup (sounds))))

(define (change-menu-color menu new-color)
  "(change-menu-color menu new-color) changes the color of menu to new-color. new-color can be the \
color name, an xm Pixel, a snd color, or a list of rgb values (as in Snd's make-color)"
  (let ((color-pixel
	 (if (string? new-color) ; assuming X11 color names here
	     (let* ((shell (cadr (main-widgets)))
		    (dpy (XtDisplay shell))
		    (scr (DefaultScreen dpy))
		    (cmap (DefaultColormap dpy scr))
		    (col (XColor)))
	       (if (= (XAllocNamedColor dpy cmap new-color col col) 0)
		   (snd-error "can't allocate ~S" new-color)
		   (.pixel col)))
	     (if (color? new-color)
		 new-color
		 ;; assume a list of rgb vals?
		 (apply make-color new-color)))))
    (for-each-child
     menu
     (lambda (n)
       (XmChangeColor n color-pixel)))))

(define (change-selection-popup-color new-color)
  "(change-selection-popup-color new-color) changes the selection popup menu's color: (change-selection-popup-color \"red\")"
  (change-menu-color selection-popup-menu new-color))

(define (change-fft-popup-color new-color)
  "(change-fft-popup-color new-color) changes the fft popup menu's color: (change-fft-popup-color (list .5 .5 .5))"
  (change-menu-color fft-popup-menu new-color))

(define (change-graph-popup-color new-color)
  "(change-graph-popup-color new-color) changes the time-domain popup menu's color: (change-graph-popup-color (basic-color))"
  (change-menu-color graph-popup-menu new-color))



;;; -------- listener popup

(define (make-popdown-entry label parent func args collector with-one)
  "(make-popdown-entry label parent func args collector with-one) makes a new listener popup menu entry"
  ;; make two entries for the popup menu, only one of which (if any) is active at a time
  ;;   if there are no relevant choices, no option is displayed
  ;;   if there's one such choice "top-one" (i.e. just a simple menu option) is displayed
  ;;   if there are several possibilities, "top-two" (a drop-down menu selection) is displayed
  ;;   "label" is the basic option label
  ;;   "parent" is the overall popup menu
  ;;   "func" is the activation function, applied to the chosen sound (index)
  ;;       that is, (func snd) is called upon activation
  ;;   "args" is an optional list of addition resource settings (pass empty list if none)
  ;;  "collector" is a function, its argument is the list of currently active sounds
  ;;    it should return those sounds relevant to the current operation.
  ;;    The length of the returned list determines which menu (if any) is actually displayed,
  ;;      and in the multiple case, what the drop-down labels are
  ;;  "with-one" is normally #t, but if there's never a simple case (i.e. it's either
  ;;    a drop-down selection or nothing), set "with-one" to #f.
  (let ((top-one (if with-one (XtCreateManagedWidget label xmPushButtonWidgetClass parent args) #f))
	(children '()))
    (if with-one
	(XtAddCallback top-one XmNactivateCallback (lambda (w c i) (func (car (collector (sounds)))))))
    (let* ((top-two (XmCreatePulldownMenu parent label args))
	   (top-two-cascade (XtCreateManagedWidget label xmCascadeButtonWidgetClass parent
			       (append (list XmNsubMenuId top-two)
				       args))))
      (XtAddCallback top-two-cascade XmNcascadingCallback
	(lambda (w c i)
	  (for-each
	   (lambda (n)
	     (XtUnmanageChild n))
	   children)
	  (let ((current-sounds (collector (sounds))))
	    (if (< (length children) (length current-sounds)) ; only active if len (collector (sounds)) > 1
		(do ((i (length children) (1+ i)))
		    ((= i (length current-sounds)))
		  (let ((child (XtCreateManagedWidget "" xmPushButtonWidgetClass top-two args)))
		    (XtAddCallback child XmNactivateCallback
		      (lambda (w c i)
			(func (or (string=? (current-label w) "all")
				  (find-sound (current-label w))))))
		    (set! children (cons child children)))))
	    (let setup
		((cs children)
		 (snds current-sounds))
	      (if (not (or (null? cs)
			   (null? snds)))
		  (let ((child (car cs))
			(snd (car snds)))
		    (change-label child (short-file-name snd))
		    (XtManageChild child)
		    (setup (cdr cs) (cdr snds))))))))
      (list 'Popdown top-one top-two top-two-cascade collector))))

(define (add-listener-popup)
  (let* ((listener (or (list-ref (main-widgets) 4)
		       (begin
			 (show-listener)
			 (set! (show-listener) #f)
			 (list-ref (main-widgets) 4))))
	 (every-menu (list XmNbackground (highlight-color)))
	 (listener-popup (XmCreatePopupMenu listener "listener-popup"
			   (append (list XmNpopupEnabled XmPOPUP_AUTOMATIC) every-menu))))

    (define (edited snds)
      (remove-if (lambda (n) 
		   (call-with-current-continuation
		    (lambda (return)
		      (do ((i 0 (1+ i)))
			  ((= i (chans n)) #t)
			(if (not (= (car (edits n i)) 0))
			    (return #f))))))
		 snds))

    (define (focused snds)
      (if (> (length snds) 1)
	  snds
	  '()))

    (XtCreateManagedWidget "Listener" xmLabelWidgetClass listener-popup every-menu)
    (XtCreateManagedWidget "sep" xmSeparatorWidgetClass listener-popup every-menu)

    (let ((listener-popup-menu
	   (list (make-popdown-entry "Play" listener-popup (lambda (snd) (play 0 snd)) every-menu identity #t)

		 (let ((help-widget (XtCreateManagedWidget "Help" xmPushButtonWidgetClass listener-popup every-menu)))
		   (XtAddCallback help-widget XmNactivateCallback
		     (lambda (w c i)
		       (let* ((selected (listener-selection))
			      (help (and selected (snd-help selected))))
			 (if help (help-dialog selected help)))))
		   help-widget)
		 
		 (let ((clear-widget (XtCreateManagedWidget "Clear listener" xmPushButtonWidgetClass listener-popup every-menu)))
		   (XtAddCallback clear-widget XmNactivateCallback (lambda (w c i) (clear-listener)))
		   clear-widget)

		 (let ((open-widget (XtCreateManagedWidget "Open" xmPushButtonWidgetClass listener-popup every-menu)))
		   (XtAddCallback open-widget XmNactivateCallback (lambda (w c i) (open-file-dialog)))
		   open-widget)
		 
		 (make-popdown-entry "Close" listener-popup close-sound every-menu #f #t)
		 (make-popdown-entry "Save" listener-popup save-sound every-menu edited #t)
		 (make-popdown-entry "Revert" listener-popup revert-sound every-menu edited #t)

		 (let ((panes-widget (XtCreateManagedWidget "Equalize panes" xmPushButtonWidgetClass listener-popup every-menu)))
		   (XtAddCallback panes-widget XmNactivateCallback (lambda (w c i) (equalize-panes)))
		   panes-widget)

		 (make-popdown-entry "Focus" listener-popup 
				     (lambda (us)
				       (let* ((pane (car (sound-widgets us)))
					      (old-resize (auto-resize)))
					 (XtSetValues (cadr (main-widgets)) (list XmNallowShellResize #f))
					 (for-each 
					  (lambda (them)
					    (XtUnmanageChild (car (sound-widgets them))))
					  (sounds))
					 (XtManageChild pane)
					 (XtSetValues (cadr (main-widgets)) (list XmNallowShellResize (auto-resize)))))
				     every-menu focused #f)

		 (XtCreateManagedWidget "sep" xmSeparatorWidgetClass listener-popup every-menu)
		 (let ((exit-widget (XtCreateManagedWidget "Exit" xmPushButtonWidgetClass listener-popup every-menu)))
		   (XtAddCallback exit-widget XmNactivateCallback (lambda (w c i) (exit)))
		   exit-widget))))

      (XtAddCallback listener XmNpopupHandlerCallback 
        (lambda (w data info)
	  (if (= ButtonPress (.type (.event info))) ; otherwise it's probably Meta-F or whatever 
	      (begin
		(for-each
		 (lambda (n)
		   (if (and (list? n)
			    (equal? (list-ref n 0) 'Popdown))
		       (let ((top-one (list-ref n 1))
			     (top-two (list-ref n 2))
			     (top-two-cascade (list-ref n 3))
			     (len (if (list-ref n 4)
				      (length ((list-ref n 4) (sounds)))
				      0)))
			 (XtUnmanageChild top-two)
			 (XtUnmanageChild top-two-cascade)
			 (if top-one (XtUnmanageChild top-one))
			 (if (> len 1) 
			     (begin
			       (XtManageChild top-two-cascade)
			       (XtManageChild top-two)))
			 (if (and top-one
				  (= len 1))
			     (XtManageChild top-one)))
		       (if (Widget? n)
			   (if (string=? (XtName n) "Equalize panes")
			       ((if (> (length (sounds)) 1) XtManageChild XtUnmanageChild) n)
			       (if (string=? (XtName n) "Help")
				   ((if (listener-selection) XtManageChild XtUnmanageChild) n))))))
		 listener-popup-menu)
		(set! (.menuToPost info) listener-popup)))))
      listener-popup)))

(add-popups)
(define listener-menu (add-listener-popup))

(define (change-listener-popup-color new-color)
  "(change-listener-popup-color new-color) changes the listener popup menu's color"
  ;; slightly different from the earlier cases because the menu parent is not explicitly in the list
  (change-menu-color listener-menu new-color))
