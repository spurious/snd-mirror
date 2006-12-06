;;; translated from popup.scm
;;;
;;; no attempt here to set widget colors, no listener popup menu (gtk predefines
;;;   a popup for each entry/textview widget and there's no way to get rid of it!
;;;   perhaps we could find a pointer to its children and hide all of them?)

(use-modules (ice-9 format))
(define popup-already-loaded (provided? 'snd-gtk-popup.scm))
(provide 'snd-gtk-popup.scm)

(if (not (provided? 'xg))
    (let ((hxm (dlopen "xg.so")))
      (if (string? hxm)
	  (snd-error (format #f "gtk-popup.scm needs the xg module: ~A" hxm))
	  (dlinit hxm "Init_libxg"))))

(define (change-label w new-label)
  "(change-label widget new-label) changes widget's label to be new-label"
  (if w
      (if (GTK_IS_LABEL w)
	  (gtk_label_set_text (GTK_LABEL w) new-label)
	  (gtk_label_set_text (GTK_LABEL (gtk_bin_get_child (GTK_BIN w))) new-label))))

(define (make-popup-menu top-field-func entries)
  "(make-popup-menu top-field-func entries) creates a popup menu"
  (let ((menu (gtk_menu_new)))
    (if top-field-func
	(top-field-func menu))
    (for-each
     (lambda (entry)
       ;; entry is list: name field-func optional-callback
       (let ((widget (if (car entry)
			 (gtk_menu_item_new_with_label (car entry))
			 (gtk_menu_item_new)))) ; separator
	 (gtk_menu_shell_append (GTK_MENU_SHELL menu) widget)
	 (if (and (cadr entry) 
		  (procedure? (cadr entry)))
	     ((cadr entry) widget))
	 (gtk_widget_show widget)
	 (if (and (not (null? (cddr entry)))
		  (procedure? (caddr entry)))
	     (g_signal_connect widget "activate" (lambda (w data) 
						   ((caddr entry) w data) 
						   (gtk_widget_hide menu)) #f))))
     entries)
    menu))


(define (display-properties props)
  ;; there's no way to tell Guile's format that enormous vectors should not be printed in full
  ;; so we search for them here and handle them ourselves

  (define (vector-print v)
    (if (< (vector-length v) 3)
	(object->string v)
	(let ((str (format #f "'#(~A" (vector-ref v 0))))
	  (do ((i 1 (1+ i)))
	      ((= i 3))
	    (set! str (string-append str " " (object->string (vector-ref v i)))))
	  (string-append str " ...)"))))

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


(if (not popup-already-loaded)
    (let* ((selection-popup-menu 
	    ;; used in graph if pointer is inside selected portion
	    (let ((every-menu (lambda (w) #f)) ; local settings
		  (stopping #f)
		  (stopping1 #f)
		  (stop-widget #f)
		  (stop-widget1 #f))
	      ;; -------- selection popup
	      (make-popup-menu 
	       (lambda (w) ; top level settings
		 #f)
	       (list
		;; name field-func callback
		(list "Selection" #f)
		(list #f #f) ; separator
		(list "Play" every-menu
		      (lambda (w data)
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
			      (play-selection #f (lambda (reason)
						   (set! stopping #f)
						   (change-label w "Play")))))))
		(list "Loop play" every-menu
		      (lambda (w data) 
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
		(list "Delete"    every-menu (lambda (w data) (delete-selection)))
		(list "Zero"      every-menu (lambda (w data) (scale-selection-by 0.0)))
		(list "Crop"      every-menu
		      (lambda (w data)
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
		(list "Save as"   every-menu (lambda (w data) (save-selection-dialog)))
		(list "Copy->New" every-menu 
		      (lambda (w data) 
			(let ((new-file-name (snd-tempnam)))
			  (save-selection new-file-name)
			  (open-sound new-file-name))))
		(list "Cut->New"   every-menu 
		      (lambda (w data) 
			(let ((new-file-name (snd-tempnam)))
			  (save-selection new-file-name)
			  (delete-selection)
			  (open-sound new-file-name))))
		(list "Snap marks" every-menu 
		      (lambda (w data)
			(for-each 
			 (lambda (select)
			   (let ((pos  (apply selection-position select))
				 (len  (apply selection-frames select)))
			     (apply add-mark pos select)
			     (apply add-mark (+ pos len) select)))
			 (selection-members))))
		(list "Apply controls" every-menu (lambda (w data) (apply-controls (selected-sound) 2))) ; 2=selection
		(list "Reset controls" every-menu (lambda (w data) (reset-controls)))
		(list "Unselect"       every-menu (lambda (w data) (set! (selection-member? #t) #f)))
		(list "Reverse"        every-menu (lambda (w data) (reverse-selection)))
		(list "Mix"            every-menu (lambda (w data) (mix-selection (cursor))))
		(list "Invert"         every-menu (lambda (w data) (scale-selection-by -1)))))))
	   
	   ;; -------- time domain popup
	   
	   (graph-popup-snd #f)
	   (graph-popup-chn #f)
	   
	   (save-popup-menu #f)
	   (undo-popup-menu #f)
	   (revert-popup-menu #f)
	   (play-previous-popup-menu #f)
	   (play-channel-popup-menu #f)
	   (redo-popup-menu #f)
	   (mix-selection-popup-menu #f)
	   (play-cursor-popup-menu #f)
	   (unselect-popup-menu #f)
	   (insert-selection-popup-menu #f)
	   (replace-with-selection-popup-menu #f)
	   (delete-mark-popup-menu #f)
	   (next-mark-popup-menu #f)
	   (last-mark-popup-menu #f)
	   (play-original-popup-menu #f)
	   
	   (graph-popup-menu 
	    ;; used within graph if pointer is not inside selected portion
	    (let ((every-menu (lambda (w) #f))
		  (stopping #f)
		  (stop-widget #f))
	      
	      (add-hook! stop-playing-hook
			 (lambda (snd) 
			   (if stopping
			       (begin
				 (set! stopping #f)
				 (if (GTK_IS_WIDGET stop-widget)
				     (change-label stop-widget "Play"))))))
	      
	      (make-popup-menu 
	       (lambda (menu) #f)
	       (list
		(list "Play" every-menu 
		      (lambda (w data) 
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
		(list "Play channel" 
		      (lambda (w) (set! play-channel-popup-menu w))
		      (lambda (w data)
			(set! stopping #t)
			(change-label stop-widget "Stop")
			(play 0 graph-popup-snd graph-popup-chn)))
		(list "Play from cursor"
		      (lambda (w) (set! play-cursor-popup-menu w))
		      (lambda (w data)
			(set! stopping #t)
			(change-label stop-widget "Stop")
			(play (cursor graph-popup-snd graph-popup-chn) graph-popup-snd)))
		(list "Play previous"
		      (lambda (w) (set! play-previous-popup-menu w))
		      (lambda (w data)
			(set! stopping #t)
			(change-label stop-widget "Stop")
			(play 0 graph-popup-snd graph-popup-chn #f #f (1- (edit-position)))))  ; play version before most-recent edit
		(list "Play original"
		      (lambda (w) (set! play-original-popup-menu w))
		      (lambda (w data)
			(set! stopping #t)
			(change-label stop-widget "Stop")
			(play 0 graph-popup-snd graph-popup-chn #f #f 0)))                     ; play unedited version
		(list "Undo"
		      (lambda (w) (set! undo-popup-menu w))
		      (lambda (w data)
			(undo 1 graph-popup-snd graph-popup-chn)))
		(list "Redo"
		      (lambda (w) (set! redo-popup-menu w))
		      (lambda (w data)
			(redo 1 graph-popup-snd graph-popup-chn)))
		(list "Revert"
		      (lambda (w) (set! revert-popup-menu w))
		      (lambda (w data)
			(revert-sound graph-popup-snd)))
		(list "Save"
		      (lambda (w) (set! save-popup-menu w))
		      (lambda (w data)
			(save-sound graph-popup-snd)))
		(list "Save as"             every-menu 
		      (lambda (w data)
			(select-sound graph-popup-snd)
			(save-sound-dialog)))
		(list "Close"               every-menu 
		      (lambda (w data)
			(close-sound graph-popup-snd)))
		(list "Mix selection"
		      (lambda (w) (set! mix-selection-popup-menu w))
		      (lambda (w data)
			(mix-selection (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
		(list "Insert selection"
		      (lambda (w) (set! insert-selection-popup-menu w))
		      (lambda (w data)
			(insert-selection (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
		(list "Replace with selection"
		      (lambda (w) (set! replace-with-selection-popup-menu w))
		      (lambda (w data)
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
		(list "Select all"          every-menu 
		      (lambda (w data)
			(select-all graph-popup-snd graph-popup-chn)))
		(list "Unselect"
		      (lambda (w) (set! unselect-popup-menu w))
		      (lambda (w data) 
			(set! (selection-member? #t) #f)))
		(list "Apply controls"          every-menu 
		      (lambda (w data) 
			(apply-controls)))
		(list "Reset controls"          every-menu 
		      (lambda (w data) 
			(reset-controls)))
		(list "Info"                every-menu 
		      (lambda (w data)
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
		(list "Add mark"            every-menu 
		      (lambda (w data)
			(add-mark (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
		(list "Delete mark"
		      (lambda (w) (set! delete-mark-popup-menu w))
		      (lambda (w data)
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
		(list "To next mark"
		      (lambda (w) (set! next-mark-popup-menu w))
		      (lambda (w data)
			(key (char->integer #\j) 4 graph-popup-snd graph-popup-chn)))
		(list "To last mark"
		      (lambda (w) (set! last-mark-popup-menu w))
		      (lambda (w data)
			(key (char->integer #\-) 4 graph-popup-snd graph-popup-chn)
			(key (char->integer #\j) 4 graph-popup-snd graph-popup-chn)))
		(list #f #f) ; separator
		(list "Exit"                every-menu 
		      (lambda (w data)
			(exit)))
		))))
	   
	   ;; -------- fft popup (easier to access than Options:Transform)
	   
	   (peaks-popup-menu #f)
	   (db-popup-menu #f)
	   (log-freq-popup-menu #f)
	   (fft-sizes '())
	   (fft-types '())
	   (wavelet-types '())
	   (fft-norms '())
	   (fft-windows '())
	   (norms (list graph-once graph-as-sonogram graph-as-spectrogram))
	   (sizes (list 16 64 256 1024 4096 16384 65536 262144 1048576))
	   (windows (list rectangular-window hann-window welch-window parzen-window bartlett-window hamming-window blackman2-window 
			  blackman3-window blackman4-window exponential-window riemann-window kaiser-window cauchy-window 
			  poisson-window gaussian-window tukey-window dolph-chebyshev-window hann-poisson-window connes-window
			  samaraki-window ultraspherical-window))
	   (types (list fourier-transform wavelet-transform autocorrelation cepstrum walsh-transform haar-transform))
	   
	   (fft-popup-menu #f)
	   ;; used within graph if pointer is in the fft graph
	   (fft-popup-menu-1 ; needed since fft-popup-menu is accessed in this context
	    (let* ((every-menu (lambda (w) #f)))
	      (define (choose-chan)
		(if (= (channel-style graph-popup-snd) channels-separate) graph-popup-chn #t))
	      
	      (define (make-simple-popdown-menu top popdown-labels label-list)
		(let* ((top-cascade (gtk_menu_new)))
		  (gtk_menu_item_set_submenu (GTK_MENU_ITEM top) top-cascade)
		  (for-each (lambda (poplab)
			      (let ((child (gtk_menu_item_new_with_label (car poplab))))
				(gtk_menu_shell_append (GTK_MENU_SHELL top-cascade) child)
				(gtk_widget_show child)
				(if (eq? label-list 'fft-sizes)
				    (set! fft-sizes (cons child fft-sizes))
				    (if (eq? label-list 'fft-types)
					(set! fft-types (cons child fft-types))
					(if (eq? label-list 'fft-windows)
					    (set! fft-windows (cons child fft-windows))
					    (if (eq? label-list 'fft-norms)
						(set! fft-norms (cons child fft-norms))
						(set! wavelet-types (cons child wavelet-types))))))
				(g_signal_connect child "activate" (lambda (w data) ((cadr poplab) w data) (gtk_widget_hide fft-popup-menu)))))
			    popdown-labels)))
	      
	      (make-popup-menu 
	       (lambda (menu) #f)
	       (list
		(list "Peaks" 
		      (lambda (w)
			(set! peaks-popup-menu w))
		      (lambda (w data)
			(set! (show-transform-peaks graph-popup-snd (choose-chan)) (not (show-transform-peaks graph-popup-snd graph-popup-chn)))))
		(list "dB" 
		      (lambda (w)
			(set! db-popup-menu w))
		      (lambda (w data)
			(set! (fft-log-magnitude graph-popup-snd (choose-chan)) (not (fft-log-magnitude graph-popup-snd graph-popup-chn)))))
		(list "Log freq"
		      (lambda (w)
			(set! log-freq-popup-menu w))
		      (lambda (w data)
			(set! (fft-log-frequency graph-popup-snd (choose-chan)) (not (fft-log-frequency graph-popup-snd graph-popup-chn)))))
		(list "Normalize" every-menu
		      (lambda (w data)
			(if (= (transform-normalization graph-popup-snd graph-popup-chn) dont-normalize)
			    (set! (transform-normalization graph-popup-snd (choose-chan)) normalize-by-channel)
			    (set! (transform-normalization graph-popup-snd (choose-chan)) dont-normalize))))
		(list "Graph type"
		      (lambda (w)
			(make-simple-popdown-menu 
			 w
			 (map (lambda (name val)
				(list name
				      (lambda (w data) 
					(set! (transform-graph-type graph-popup-snd (choose-chan)) val))))
			      (list "once" "sonogram" "spectrogram")
			      norms)
			 'fft-norms)))
		(list "Size"
		      (lambda (w)
			(make-simple-popdown-menu 
			 w
			 (map (lambda (name val)
				(list name
				      (lambda (w data) 
					(set! (transform-size graph-popup-snd (choose-chan)) val))))
			      (map (lambda (n) (number->string n)) sizes)
			      sizes)
			 'fft-sizes)))
		(list "Window"
		      (lambda (w)
			(make-simple-popdown-menu
			 w
			 (map (lambda (name val)
				(list name
				      (lambda (w data) 
					(set! (fft-window graph-popup-snd (choose-chan)) val))))
			      (list "Rectangular" "Hann" "Welch" "Parzen" "Bartlett" "Hamming" "Blackman2" "Blackman3" "Blackman4"
				    "Exponential" "Riemann" "Kaiser" "Cauchy" "Poisson" "Gaussian" "Tukey" "Dolph-Chebyshev" "Hann-Poisson" 
				    "Connes" "Samaraki" "Ultraspherical")
			      windows)
			 'fft-windows)))
		(list "Transform type"
		      (lambda (w)
			(make-simple-popdown-menu 
			 w
			 (map (lambda (name val)
				(list name 
				      (lambda (w data)
					(set! (transform-type graph-popup-snd (choose-chan)) val))))
			      (list "Fourier" "Wavelet" "Autocorrelate" "Cepstrum" "Walsh" "Haar")
			      types)
			 'fft-types)))
		(list "Wavelet type"
		      (lambda (w)
			(make-simple-popdown-menu 
			 w
			 (let ((ctr -1))
			   (map (lambda (name)
				  (set! ctr (+ ctr 1))
				  (list name 
					(lambda (w data)
					  (set! (wavelet-type graph-popup-snd (choose-chan)) ctr))))
				(list "daub4" "daub6" "daub8" "daub10" "daub12" "daub14" "daub16" "daub18" "daub20" "battle_lemarie" 
				      "burt_adelson" "beylkin" "coif2" "coif4" "coif6" "sym2" "sym3" "sym4" "sym5" "sym6")))
			 'wavelet-types)))
		(list "Color" every-menu
		      (lambda (w data) (color-dialog)))
		(list "Orientation" every-menu
		      (lambda (w data) (orientation-dialog))))))))
      (set! fft-types (reverse fft-types))
      (set! fft-windows (reverse fft-windows))
      (set! fft-norms (reverse fft-norms))
      (set! fft-sizes (reverse fft-sizes))
      (set! wavelet-types (reverse wavelet-types))
      (set! fft-popup-menu fft-popup-menu-1)
      (add-hook! gtk-popup-hook
		 (lambda (widget event data snd chn)
		   (let* ((e (GDK_EVENT_BUTTON event))
			  (x (.x e))
			  (y (.y e))
			  (menu graph-popup-menu))
		     (if snd
			 (begin
			   (set! graph-popup-snd snd)
			   (set! graph-popup-chn chn)
			   (if (= (channel-style snd) channels-combined)
			       (call-with-current-continuation
				(lambda (break)
				  (do ((i 0 (1+ i)))
				      ((= i (chans snd)))
				    (let ((off (list-ref (axis-info snd i) 14)))
				      (if (< y off)
					  (begin
					    (set! graph-popup-chn (- i 1))
					    (break)))))
				  (set! graph-popup-chn (- (chans snd) 1)))))
			   (let ((fax (if (transform-graph? snd chn) (axis-info snd chn transform-graph) #f))
				 (lax (if (lisp-graph? snd chn) (axis-info snd chn lisp-graph) #f)))
			     (if (and fax
				      (>= x (list-ref fax 10))
				      (<= x (list-ref fax 12)))
				 ;; in fft
				 (begin
				   (change-label peaks-popup-menu (if (show-transform-peaks snd chn) "No peaks" "Peaks"))
				   (change-label db-popup-menu (if (fft-log-magnitude snd chn) "Linear" "dB"))
				   (change-label log-freq-popup-menu (if (fft-log-frequency snd chn) "Linear freq" "Log freq"))
				   (for-each (lambda (child size)
					       (gtk_widget_set_sensitive child (not (= (transform-size graph-popup-snd graph-popup-chn) size))))
					     fft-sizes sizes)
				   (for-each (lambda (child norm)
					       (gtk_widget_set_sensitive child (not (= (transform-graph-type graph-popup-snd graph-popup-chn) norm))))
					     fft-norms norms)
				   (for-each (lambda (child window)
					       (gtk_widget_set_sensitive child (not (= (fft-window graph-popup-snd graph-popup-chn) window))))
					     fft-windows windows)
				   (for-each (lambda (child type)
					       (gtk_widget_set_sensitive child (not (= (transform-type graph-popup-snd graph-popup-chn) type))))
					     fft-types types)
				   (let ((ctr 0))
				     (for-each (lambda (child)
						 (gtk_widget_set_sensitive child (not (= (wavelet-type graph-popup-snd graph-popup-chn) ctr)))
						 (set! ctr (1+ ctr)))
					       wavelet-types))
				   (set! menu fft-popup-menu))
				 (if (and lax
					  (>= x (list-ref lax 10))
					  (<= x (list-ref lax 12)))
				     ;; in lisp
				     ;;   nothing special implemented yet
				     ;; (set! menu graph-popup-menu)
				     #f ; just a place-holder
				     
				     (if (and (selection?)
					      (let* ((beg (/ (selection-position snd graph-popup-chn) (srate snd)))
						     (end (/ (+ (selection-position snd graph-popup-chn) 
								(selection-frames snd graph-popup-chn)) 
							     (srate snd))))
						(and (>= x (x->position beg snd chn))
						     (<= x (x->position end snd chn)))))
					 (set! menu selection-popup-menu)
					 (let* ((eds (edits graph-popup-snd graph-popup-chn))
						(with-edit (if (> (car eds) 0) gtk_widget_show gtk_widget_hide))
						(nchans (chans graph-popup-snd))
						(with-selection (if (selection?) gtk_widget_show gtk_widget_hide))
						(with-marks (if (not (null? (marks graph-popup-snd graph-popup-chn))) gtk_widget_show gtk_widget_hide)))
					   (set! menu graph-popup-menu)
					   (with-edit save-popup-menu)
					   (with-edit undo-popup-menu)
					   (with-edit revert-popup-menu)
					   (with-edit play-previous-popup-menu)
					   ((if (> (cadr eds) 0) gtk_widget_show gtk_widget_hide) redo-popup-menu)
					   ((if (> nchans 1) gtk_widget_show gtk_widget_hide) play-channel-popup-menu)
					   (with-selection mix-selection-popup-menu)
					   (with-selection replace-with-selection-popup-menu)
					   (with-selection insert-selection-popup-menu)
					   (with-selection unselect-popup-menu)
					   ((if (> (cursor graph-popup-snd graph-popup-chn) 0) gtk_widget_show gtk_widget_hide) play-cursor-popup-menu)
					   ((if (> (car eds) 1) gtk_widget_show gtk_widget_hide) play-original-popup-menu)
					   (with-marks delete-mark-popup-menu)
					   (with-marks next-mark-popup-menu)
					   (with-marks last-mark-popup-menu))))))
			   (gtk_widget_show menu)
			   (gtk_menu_popup (GTK_MENU menu) #f #f #f #f (.button e) (.time e))
			   #t)
			 #f))))))


;;; -------- edit history popup

(define edhist-funcs '())
(define edhist-widgets '())
(define edhist-channel-widgets '())
(define edhist-snd #f)
(define edhist-chn #f)
(define edhist-time 0)

(define (edhist-clear-edits)
  (set! edhist-funcs '())
  #f)

(define (edhist-save-edits)
  (let* ((old-val (assoc (cons edhist-snd edhist-chn) edhist-funcs))
	 (cur-edits (edits edhist-snd edhist-chn))
	 (new-func (edit-list->function edhist-snd edhist-chn (1+ (car cur-edits)) (apply + cur-edits))))
    (if old-val
	(set-cdr! old-val new-func)
	;; perhaps this should save the previous function under the current file name?
	(set! edhist-funcs (cons (cons (cons edhist-snd edhist-chn) new-func) edhist-funcs)))
    #f))

(define (edhist-reapply-edits)
  (let* ((old-val (assoc (cons edhist-snd edhist-chn) edhist-funcs)))
    (if old-val	((cdr old-val) edhist-snd edhist-chn))))

(define (edhist-apply-edits)
  ;; the current popdown widgets are in edhist-widgets, current funcs are in edhist-funcs
  ;; edhist-apply (for new widgets) is car of edhist-widgets

  (define (edhist-apply w d)
    (let ((index (cadr (g_object_get_data (G_OBJECT w) "popup-index"))))
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
				wid)
			      (let ((wid (gtk_menu_item_new_with_label "label")))
				(gtk_menu_shell_append (GTK_MENU_SHELL parent) wid)
				(set! edhist-widgets (append edhist-widgets (list wid)))
				(g_signal_connect wid "activate" edhist-apply)
				wid))))
	     (if (pair? label)
		 (change-label button (format #f "~A[~A]" (short-file-name (car label)) (cdr label)))
		 (change-label button label))
	     (g_object_set_data (G_OBJECT button) "popup-index" (GPOINTER index))
	     (gtk_widget_show button)
	     (set! index (1+ index))))
	 edhist-funcs))
    (if (not (null? wids))
	(for-each
	 (lambda (w)
	   (gtk_widget_hide w))
	 wids))
    (gtk_menu_popup (GTK_MENU parent) #f #f #f #f 3 edhist-time)
    #f))

(define (edhist-help-edits)
  (help-dialog "Edit History Functions"
	       "This popup menu gives access to the edit-list function handlers in Snd. \
At any time you can backup in the edit list, 'save' the current trailing edits, make some \
new set of edits, then 'reapply' the saved edits.  The 'apply' choice gives access to all \
currently saved edit lists -- any such list can be applied to any channel.  'Clear' deletes \
all saved edit lists."
	       (list "{edit lists}" "{edit-list->function}")
	       (list "extsnd.html#editlists" "extsnd.html#editlist_to_function")
	       ))

(define edhist-save-menu #f)
(define edhist-apply-menu #f)
(define edhist-reapply-menu #f)
(define edhist-clear-menu #f)

;(gc) -- trouble here?
(define edit-history-menu
  (let ((every-menu (lambda (w) #f)))
    (make-popup-menu 
     (lambda (w) #f)
     (list
      (list "Edits"    #f)
      (list #f #f)
      (list "Save"
	    (lambda (w) (set! edhist-save-menu w))
	    (lambda (w d) (edhist-save-edits)))
      (list "Reapply"
	    (lambda (w) (set! edhist-reapply-menu w))
	    (lambda (w d) (edhist-reapply-edits)))
      (list "Apply"
	    (lambda (w)
	      (set! edhist-apply-menu w)
	      (let* ((top-cascade (gtk_menu_new)))
		(gtk_menu_item_set_submenu (GTK_MENU_ITEM w) top-cascade)
		(set! edhist-widgets (list top-cascade))))
	    (lambda (w d) (edhist-apply-edits)))
      (list "Clear"
	    (lambda (w) (set! edhist-clear-menu w))
	    (lambda (w d) (edhist-clear-edits)))
      (list "Help"     #f 
	    (lambda (w d) (edhist-help-edits)))))))

(define (add-edhist-popup snd)
  (let ((chns (chans snd)))
    (do ((i 0 (1+ i)))
	((= i chns))
      (let ((edhist (list-ref (channel-widgets snd i) 7)))
	(if (not (member edhist edhist-channel-widgets))
	    (begin
	      (set! edhist-channel-widgets (cons edhist edhist-channel-widgets))
	      (g_signal_connect edhist "button_press_event" 
		(lambda (w e d) 
		  (let ((button (.button (GDK_EVENT_BUTTON e))))
		    (if (= button 3)
			(begin
			  (set! edhist-snd snd)
			  (set! edhist-chn i)
			  (gtk_widget_set_sensitive edhist-clear-menu (not (null? edhist-funcs)))
			  (gtk_widget_set_sensitive edhist-save-menu (> (apply + (edits snd i)) 0))
			  (gtk_widget_set_sensitive edhist-apply-menu (not (null? edhist-funcs)))
			  (gtk_widget_set_sensitive edhist-reapply-menu (not (eq? #f (assoc (cons snd i) edhist-funcs))))
			  (gtk_widget_show edit-history-menu)
			  (set! edhist-time (.time (GDK_EVENT_BUTTON e)))
			  (gtk_menu_popup (GTK_MENU edit-history-menu) #f #f #f #f (.button (GDK_EVENT_BUTTON e)) (.time (GDK_EVENT_BUTTON e)))
			  #t) ; don't select anything in the list (i.e. don't pass event to widget)
			#f)))
		#f)
	      ))))))

(add-hook! after-open-hook add-edhist-popup)
(add-hook! close-hook (lambda (snd)
			(let ((chns (chans snd))
			      (name (short-file-name snd)))
			  (do ((i 0 (1+ i)))
			      ((= i chns))
			    (let* ((old-val (assoc (cons snd i) edhist-funcs)))
			      (if old-val
				  (set-car! old-val (format #f "~A[~A]" name i))))))))
(for-each add-edhist-popup (sounds))


