;;; specialize popup menus
;;;
;;; currently there are four special popup menus:
;;;   selection fft time-domain lisp-listener
;;;
;;; (add-selection-popup) creates popup menus specialized for the fft, selection, and time-domain sections of the graph
;;;    (change-selection-popup-color new-color) to change selection memu's color
;;;    (change-fft-popup-color new-color) to change fft menu's color
;;;    (change-graph-popup-color new-color) to change time-domain menu's color
;;; (add-listener-popup) posts a special popup menu if the pointer is in the listener
;;;    (change-listener-popup-color new-color) to change its color

(use-modules (ice-9 common-list))

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-motif.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "init_xm"))))

(define (for-each-child w func)
  (func w)
  (if (|XtIsComposite w)
      (for-each 
       (lambda (n)
	 (for-each-child n func))
       (cadr (|XtGetValues w (list |XmNchildren 0) 1)))))

(define (change-label w new-label)
  (let ((str (|XmStringCreateLocalized new-label)))
    (|XtSetValues w (list |XmNlabelString str))
    (|XmStringFree str)))
      
(define (current-label w)
  (let ((xmstr (cadr (|XtGetValues w (list |XmNlabelString 0)))))
    (cadr (|XmStringGetLtoR xmstr |XmFONTLIST_DEFAULT_TAG))))



(define (make-popup-menu name parent top-args entries)
  (let ((menu (|XmCreatePopupMenu parent name top-args)))
    (for-each
     (lambda (entry)
       ;; entry is list: name type args callback
       (let ((widget (|XtCreateManagedWidget (car entry)
					     (cadr entry)
					     menu
					     (caddr entry))))
	 (if (not (null? (cdddr entry)))
	     (|XtAddCallback widget |XmNactivateCallback (cadddr entry)))))
     entries)
    menu))

;;; -------- selection popup

(define selection-popup-menu 
  ;; used in graph if pointer is inside selected portion
  (let ((every-menu (list |XmNbackground (|Pixel (snd-pixel (highlight-color)))))
	(stopping #f)
	(stopping1 #f)
	(stop-widget #f)
	(stop-widget1 #f))

    (add-hook! stop-playing-selection-hook
	       (lambda () 
		 (if stopping
		     (begin
		       (set! stopping #f)
		       (if (|Widget? stop-widget)
			   (change-label stop-widget "Play"))))))

    (make-popup-menu 
     "selection-popup"
     (|Widget (caddr (main-widgets)))
     (list |XmNpopupEnabled #t
	   |XmNbackground (|Pixel (snd-pixel (highlight-color))))
     (list
      (list "Selection" |xmLabelWidgetClass      every-menu)
      (list "sep"       |xmSeparatorWidgetClass  every-menu)
      (list "Play"      |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i) 
	      (if stopping
		  (begin
		    (set! stopping #f)
		    (change-label w "Play")
		    (if stopping1
			(begin
			  (set! stopping1 #f)
			  (change-label stop-widget1 "Loop play")
			  (remove-hook! stop-playing-selection-hook play-selection)))
		    (stop-playing)) ; stops all including possible looping play
		  (begin
		    (change-label w "Stop")
		    (set! stop-widget w)
		    (set! stopping #t)
		    (play-selection)))))
      (list "Loop play"      |xmPushButtonWidgetClass every-menu ; play over and over
	    (lambda (w c i) 
	      (if stopping1
		  (begin
		    (set! stopping1 #f)
		    (change-label w "Loop play")
		    (remove-hook! stop-playing-selection-hook play-selection)
		    (if stopping
			(begin
			  (set! stopping #f)
			  (change-label stop-widget "Play")))
		    (stop-playing))
		  (begin
		    (change-label w "Stop!")
		    (set! stop-widget1 w) ; needs to be separate from Play case since we're stopping/restarting deliberately
		    (set! stopping1 #t)
		    (add-hook! stop-playing-selection-hook play-selection) ; when one rendition ends, we immediately start another
		    (play-selection)))))
      (list "Delete"    |xmPushButtonWidgetClass every-menu (lambda (w c i) (delete-selection)))
      (list "Zero"      |xmPushButtonWidgetClass every-menu (lambda (w c i) (scale-selection-by 0.0)))
      (list "Crop"      |xmPushButtonWidgetClass every-menu
	    (lambda (w c i)
	      ;; delete everything except selection
	      (for-each
	       (lambda (selection)
		 (as-one-edit
		  (lambda ()
		    (let* ((snd (car selection))
			   (chn (cadr selection))
			   (beg (selection-position snd chn))
			   (len (selection-length snd chn)))
		      (if (> beg 0) 
			  (delete-samples 0 beg snd chn))
		      (if (< len (frames snd chn))
			  (delete-samples (+ len 1) (- (frames snd chn) len) snd chn))))))
	       (let ((sndlist '()))
		 (map (lambda (snd)
			(do ((i (1- (channels snd)) (1- i)))
			    ((< i 0))
			  (if (selection-member? snd i)
			      (set! sndlist (cons (list snd i) sndlist)))))
		      (sounds))
		 sndlist))))
      (list "Save as"   |xmPushButtonWidgetClass every-menu (lambda (w c i) (edit-save-as-dialog)))
      (list "Copy->New" |xmPushButtonWidgetClass every-menu 
	    (let ((selctr 0)) 
	      (lambda (w c i) 
		(let ((new-file-name (format #f "newf-~D.snd" selctr)))
		  (set! selctr (+ selctr 1))
		  (save-selection new-file-name)
		  (open-sound new-file-name)))))
      (list "Snap marks" |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (for-each 
	       (lambda (select)
		 (let ((pos  (apply selection-position select))
		       (len  (apply selection-length select)))
		   (apply add-mark pos select)
		   (apply add-mark (+ pos len) select)))
	       (selection-members))))
      (list "Reverse"   |xmPushButtonWidgetClass every-menu (lambda (w c i) (reverse-selection)))
      (list "Invert"    |xmPushButtonWidgetClass every-menu (lambda (w c i) (scale-selection-by -1)))))))


;;; -------- time domain popup

(define graph-popup-snd #f)
(define graph-popup-chn #f)

(define graph-popup-menu 
  ;; used within graph if pointer is not inside selected portion
  (let ((every-menu (list |XmNbackground (|Pixel (snd-pixel (highlight-color)))))
	(stopping #f)
	(stop-widget #f))

    (add-hook! stop-playing-hook
	       (lambda (snd) 
		 (if stopping
		     (begin
		       (set! stopping #f)
		       (if (|Widget? stop-widget)
			   (change-label stop-widget "Play"))))))

    (make-popup-menu 
     "graph-popup"
     (|Widget (caddr (main-widgets)))
     (list |XmNpopupEnabled #t
	   |XmNbackground (|Pixel (snd-pixel (highlight-color))))
     (list

      (list "Snd"                |xmLabelWidgetClass      every-menu) 
      (list "sep"                |xmSeparatorWidgetClass  every-menu)
      (list "Play"               |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i) 
	      (if stopping
		  (begin
		    (set! stopping #f)
		    (change-label w "Play")
		    (stop-playing))
		  (begin
		    (change-label w "Stop")
		    (set! stop-widget w)
		    (set! stopping #t)
		    (play 0 graph-popup-snd)))))
      (list "Play channel"       |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (play 0 graph-popup-snd graph-popup-chn)))
      (list "Play from cursor"   |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (play (cursor graph-popup-snd graph-popup-chn) graph-popup-snd)))
      (list "Undo"               |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (undo 1 graph-popup-snd graph-popup-chn)))
      (list "Redo"               |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (redo 1 graph-popup-snd graph-popup-chn)))
      (list "Revert"             |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (revert-sound graph-popup-snd)))
      (list "Save"               |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (save-sound graph-popup-snd)))
      (list "Save as"            |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (select-sound graph-popup-snd)
	      (file-save-as-dialog)))
      (list "Close"              |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (close-sound graph-popup-snd)))
      (list "Mix selection"      |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (mix-selection (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
      (list "Insert selection"   |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (insert-selection (cursor graph-popup-snd graph-popup-chn) graph-popup-snd graph-popup-chn)))
      (list "Equalize panes"     |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (equalize-panes)))
      (list "Info"               |xmPushButtonWidgetClass every-menu 
	    (lambda (w c i)
	      (let ((snd graph-popup-snd))
		(help-dialog 
		 (format #f "~A info" (file-name snd))
		 (format #f "~A:~%  chans: ~D~%  srate: ~D~%  header: ~A~%  data format: ~A~%  length: ~1,3F~%  maxamp: ~A~%~A"
			(short-file-name snd)
			(chans snd)
			(srate snd)
			(mus-header-type-name (header-type snd))
			(mus-data-format-name (data-format snd))
			(/ (frames snd graph-popup-chn) (srate snd))
			(maxamp snd #t)
			(if (comment snd)
			    (format #f "  comment: ~A~%" (comment snd))))))))
      ))))

(define (edit-graph-popup-menu snd chn)
  ;; hide otiose entries, relabel others
  (let ((eds (edits snd chn)))
    (for-each-child
     graph-popup-menu
     (lambda (w)
       (let ((name (|XtName w)))
	 (if (string=? name "Snd")
	     (if (> (chans snd) 1)
		 (change-label w (format #f "~A[~D]" (short-file-name snd) chn))
		 (change-label w (short-file-name snd)))
	     (if (or (string=? name "Save")
		     (string=? name "Undo")
		     (string=? name "Revert"))
		 ((if (> (car eds) 0) |XtManageChild |XtUnmanageChild) w)
		 (if (string=? name "Play channel")
		     ((if (> (chans snd) 1) |XtManageChild |XtUnmanageChild) w)
		     (if (string=? name "Equalize panes")
			 ((if (or (> (chans snd) 1) 
				  (> (length (sounds)) 1))
			      |XtManageChild |XtUnmanageChild) w)
			 (if (string=? name "Redo")
			     ((if (> (cadr eds) 0) |XtManageChild |XtUnmanageChild) w)
			     (if (or (string=? name "Mix selection")
				     (string=? name "Insert selection"))
				 ((if (selection?) |XtManageChild |XtUnmanageChild) w)
				 (if (string=? name "Play from cursor")
				     ((if (> (cursor snd chn) 0) |XtManageChild |XtUnmanageChild) w)))))))))))))


;;; -------- fft popup (easier to access than Options:Transform)

(define (make-simple-popdown-menu label popdown-labels parent cascade-func args)
  (let* ((top (|XmCreatePulldownMenu parent label args))
	 (top-cascade (|XtCreateManagedWidget label |xmCascadeButtonWidgetClass parent
			(append (list |XmNsubMenuId top)
				args)))
	 (children (map (lambda (poplab)
			  (let ((child (|XtCreateManagedWidget (car poplab) |xmPushButtonWidgetClass top args)))
			    (|XtAddCallback child |XmNactivateCallback (cadr poplab))
			    child))
			popdown-labels)))
      (if cascade-func 
	  (|XtAddCallback top-cascade |XmNcascadingCallback 
            (lambda (w c i)
              (cascade-func children))))))

(define fft-popup-menu 
  ;; used within graph if pointer is in the fft graph
  (let* ((every-menu (list |XmNbackground (|Pixel (snd-pixel (highlight-color)))))
	 (fft-popup (|XmCreatePopupMenu (|Widget (caddr (main-widgets))) "fft-popup"
		       (append (list |XmNpopupEnabled #t) every-menu))))
    
    (|XtCreateManagedWidget "Transform" |xmLabelWidgetClass fft-popup every-menu)
    (|XtCreateManagedWidget "sep" |xmSeparatorWidgetClass fft-popup every-menu)

    (let ((peaks (|XtCreateManagedWidget "Peaks" |xmPushButtonWidgetClass fft-popup every-menu)))
      (|XtAddCallback peaks |XmNactivateCallback
         (lambda (w c i)
	   (set! (show-transform-peaks graph-popup-snd graph-popup-chn) (not (show-transform-peaks graph-popup-snd graph-popup-chn))))))

    (let ((db (|XtCreateManagedWidget "dB" |xmPushButtonWidgetClass fft-popup every-menu)))
      (|XtAddCallback db |XmNactivateCallback
         (lambda (w c i)
	   (set! (fft-log-magnitude graph-popup-snd graph-popup-chn) (not (fft-log-magnitude graph-popup-snd graph-popup-chn))))))

    (let ((logfreq (|XtCreateManagedWidget "Log freq" |xmPushButtonWidgetClass fft-popup every-menu)))
      (|XtAddCallback logfreq |XmNactivateCallback
         (lambda (w c i)
	   (set! (fft-log-frequency graph-popup-snd graph-popup-chn) (not (fft-log-frequency graph-popup-snd graph-popup-chn))))))

    (let ((norm (|XtCreateManagedWidget "Normalize" |xmPushButtonWidgetClass fft-popup every-menu)))
      (|XtAddCallback norm |XmNactivateCallback
         (lambda (w c i)
	   (if (= (transform-normalization graph-popup-snd graph-popup-chn) dont-normalize-transform)
	       (set! (transform-normalization graph-popup-snd graph-popup-chn) normalize-transform-by-channel)
	       (set! (transform-normalization graph-popup-snd graph-popup-chn) dont-normalize-transform)))))

    (make-simple-popdown-menu 
     "Graph type" 
     (map (lambda (name val)
		  (list name
			(lambda (w c i) 
			  (set! (transform-graph-type graph-popup-snd graph-popup-chn) val))))
		(list "once" "sonogram" "spectrogram")
		(list graph-transform-once graph-transform-as-sonogram graph-transform-as-spectrogram))
     fft-popup 
     (lambda (lst)
       (let ((ctr 0))
	 (for-each 
	  (lambda (child)
	    (|XtSetSensitive child (not (= (transform-graph-type graph-popup-snd graph-popup-chn) ctr)))
	    (set! ctr (+ ctr 1)))
	  lst)))
     every-menu)

    (let ((sizes (list 16 64 256 1024 4096 16384 65536 262144 1048576)))
      (make-simple-popdown-menu
       "Size"
       (map (lambda (name val)
	      (list name
		    (lambda (w c i) 
		      (set! (transform-size graph-popup-snd graph-popup-chn) val))))
	    (map (lambda (n) (number->string n)) sizes)
	    sizes)
     fft-popup
     (lambda (lst)
       (for-each 
	(lambda (child size)
	  (|XtSetSensitive child (not (= (transform-size graph-popup-snd graph-popup-chn) size))))
	lst sizes))
     every-menu))

    (let ((windows (list rectangular-window hann-window welch-window parzen-window bartlett-window hamming-window blackman2-window 
			 blackman3-window blackman4-window exponential-window riemann-window kaiser-window cauchy-window 
			 poisson-window gaussian-window tukey-window dolph-chebyshev-window)))
      (make-simple-popdown-menu
       "Window"
       (map (lambda (name val)
	      (list name
		    (lambda (w c i) 
		      (set! (fft-window graph-popup-snd graph-popup-chn) val))))
	    (list "Rectangular" "Hann" "Welch" "Parzen" "Bartlett" "Hamming" "Blackman2" "Blackman3" "Blackman4"
		  "Exponential" "Riemann" "Kaiser" "Cauchy" "Poisson" "Gaussian" "Tukey" "Dolph-Chebyshev")
	    windows)
       fft-popup
       (lambda (lst)
	 (for-each 
	  (lambda (child window)
	    (|XtSetSensitive child (not (= (fft-window graph-popup-snd graph-popup-chn) window))))
	  lst windows))
       every-menu))

    (let ((types (list fourier-transform wavelet-transform autocorrelation cepstrum hankel-transform
		       walsh-transform chebyshev-transform hadamard-transform haar-transform)))
      (make-simple-popdown-menu 
       "Transform type"
       (map (lambda (name val)
	      (list name 
		    (lambda (w c i)
		      (set! (transform-type graph-popup-snd graph-popup-chn) val))))
	    (list "Fourier" "Wavelet" "Autocorrelate" "Cepstrum" "Hankel" "Walsh" "Chebychev" "Hadamard" "Haar")
	    types)
       fft-popup 
       (lambda (lst)
	 (for-each 
	  (lambda (child type)
	    (|XtSetSensitive child (not (= (transform-type graph-popup-snd graph-popup-chn) type))))
	  lst types))
       every-menu))

    (make-simple-popdown-menu 
     "Wavelet type"
     (let ((ctr -1))
       (map (lambda (name)
	      (set! ctr (+ ctr 1))
	      (list name 
		    (lambda (w c i)
		      (set! (wavelet-type graph-popup-snd graph-popup-chn) ctr))))
	    (list "daub4" "daub6" "daub8" "daub10" "daub12" "daub14" "daub16" "daub18" "daub20" "battle_lemarie" 
		  "burt_adelson" "beylkin" "coif2" "coif4" "coif6" "sym2" "sym3" "sym4" "sym5" "sym6")))
     fft-popup 
     (lambda (lst)
       (let ((ctr 0))
	 (for-each 
	  (lambda (child)
	    (|XtSetSensitive child (not (= (wavelet-type graph-popup-snd graph-popup-chn) ctr)))
	    (set! ctr (+ ctr 1)))
	  lst)))
     every-menu)

    (let ((color (|XtCreateManagedWidget "Color" |xmPushButtonWidgetClass fft-popup every-menu)))
      (|XtAddCallback color |XmNactivateCallback (lambda (w c i) (color-dialog))))

    (let ((orient (|XtCreateManagedWidget "Orientation" |xmPushButtonWidgetClass fft-popup every-menu)))
      (|XtAddCallback orient |XmNactivateCallback (lambda (w c i) (orientation-dialog))))

    fft-popup))

(define (add-selection-popup)
  ;; TODO: add new popups to existing chans as well
  (let ((popups '()))
    (define (find-popup snd chn dats)
      (if (not (null? dats))
	  (let ((cur (car dats)))
	    (if (and (= (car cur) snd)
		     (= (cadr cur) chn))
		cur
		(find-popup snd chn (cdr dats))))
	  #f))
    (add-hook! after-open-hook
      (lambda (snd)
	(do ((chn 0 (1+ chn)))
	    ((= chn (chans snd)))
	  (if (not (find-popup snd chn popups))
	      (let ((chn-grf (|Widget (car (channel-widgets snd chn)))))
		(set! popups (cons (list snd chn) popups))
		(|XtAddCallback chn-grf |XmNpopupHandlerCallback 
		  (lambda (w data info)
		    (let* ((e (|event info))
			   (xe (- (|x_root e) (car (|XtTranslateCoords w 0 0)))))
		      ;; xe is where the mouse-click occurred in the graph window's (local) coordinates
		      (set! graph-popup-snd snd)
		      (set! graph-popup-chn chn)

		      (let ((fax (if (graph-transform? snd chn) (axis-info snd chn transform-graph) #f))
			    (lax (if (graph-lisp? snd chn) (axis-info snd chn lisp-graph) #f)))
			(if (and fax
				 (>= xe (list-ref fax 10))
				 (<= xe (list-ref fax 12)))
			    ;; in fft
			    (set! (|menuToPost info) fft-popup-menu)

			    (if (and lax
				     (>= xe (list-ref lax 10))
				     (<= xe (list-ref lax 12)))
				;; in lisp
				;;   nothing special implemented yet
				;; (set! (|menuToPost info) graph-popup-menu)
				#f ; just a place-holder

				(if (and (selection?)
					 (let* ((beg (/ (selection-position snd chn) (srate snd)))
						(end (/ (+ (selection-position snd chn) (selection-length snd chn)) (srate snd))))
					   (and (>= xe (x->position beg snd chn))
						(<= xe (x->position end snd chn)))))
				    (set! (|menuToPost info) selection-popup-menu)

				    (begin
				      (edit-graph-popup-menu snd chn)
				      (set! (|menuToPost info) graph-popup-menu))))))))))))))))

(define (change-menu-color menu new-color)
  ;; new-color can be the color name, an xm Pixel, a snd color, or a list of rgb values (as in Snd's make-color)
  (let ((color-pixel
	 (if (string? new-color) ; assuming X11 color names here
	     (let* ((shell (|Widget (cadr (main-widgets))))
		    (dpy (|XtDisplay shell))
		    (scr (|DefaultScreen dpy))
		    (cmap (|DefaultColormap dpy scr))
		    (col (|XColor)))
	       (if (= (|XAllocNamedColor dpy cmap new-color col col) 0)
		   (snd-error "can't allocate ~S" new-color)
		   (|pixel col)))
	     (if (color? new-color)
		 (|Pixel (snd-pixel new-color))
		 (if (|Pixel? new-color)
		     new-color
		     ;; assume a list of rgb vals?
		     (|Pixel (snd-pixel (apply make-color new-color))))))))
    (for-each-child
     menu
     (lambda (n)
       (|XmChangeColor n color-pixel)))))

(define (change-selection-popup-color new-color)
  (change-menu-color selection-popup-menu new-color))

(define (change-fft-popup-color new-color)
  (change-menu-color fft-popup-menu new-color))

(define (change-graph-popup-color new-color)
  (change-menu-color graph-popup-menu new-color))

; (change-selection-popup-color "red") 
; (change-selection-popup-color (list .5 .5 .5))
; (change-selection-popup-color (basic-color))


;;; -------- listener popup

(define (make-popdown-entry label parent func args collector with-one)
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
  (let ((top-one (if with-one (|XtCreateManagedWidget label |xmPushButtonWidgetClass parent args) #f))
	(children '()))
    (if with-one
	(|XtAddCallback top-one |XmNactivateCallback (lambda (w c i) (func (car (collector (sounds)))))))
    (let* ((top-two (|XmCreatePulldownMenu parent label args))
	   (top-two-cascade (|XtCreateManagedWidget label |xmCascadeButtonWidgetClass parent
			       (append (list |XmNsubMenuId top-two)
				       args))))
      (|XtAddCallback top-two-cascade |XmNcascadingCallback
	(lambda (w c i)
	  (for-each
	   (lambda (n)
	     (|XtUnmanageChild n))
	   children)
	  (let ((current-sounds (collector (sounds))))
	    (if (< (length children) (length current-sounds)) ; only active if len (collector (sounds)) > 1
		(do ((i (length children) (1+ i)))
		    ((= i (length current-sounds)))
		  (let ((child (|XtCreateManagedWidget "" |xmPushButtonWidgetClass top-two args)))
		    (|XtAddCallback child |XmNactivateCallback
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
		    (|XtManageChild child)
		    (setup (cdr cs) (cdr snds))))))))
      (list 'Popdown top-one top-two top-two-cascade collector))))

(define (add-listener-popup)
  (let* ((listener (|Widget (or (list-ref (main-widgets) 4)
				(begin
				  (show-listener)
				  (set! (show-listener) #f)
				  (list-ref (main-widgets) 4)))))
	 (every-menu (list |XmNbackground (|Pixel (snd-pixel (highlight-color)))))
	 (listener-popup (|XmCreatePopupMenu listener "listener-popup"
			   (append (list |XmNpopupEnabled #t) every-menu))))

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

    (|XtCreateManagedWidget "Listener" |xmLabelWidgetClass listener-popup every-menu)
    (|XtCreateManagedWidget "sep" |xmSeparatorWidgetClass listener-popup every-menu)

    (let ((listener-popup-menu
	   (list (make-popdown-entry "Play" listener-popup (lambda (snd) (play 0 snd)) every-menu identity #t)

		 (let ((help-widget (|XtCreateManagedWidget "Help" |xmPushButtonWidgetClass listener-popup every-menu)))
		   (|XtAddCallback help-widget |XmNactivateCallback
		     (lambda (w c i)
		       (let* ((selected (listener-selection))
			      (help (and selected (snd-help selected))))
			 (if help (help-dialog selected help)))))
		   help-widget)

		 (let ((open-widget (|XtCreateManagedWidget "Open" |xmPushButtonWidgetClass listener-popup every-menu)))
		   (|XtAddCallback open-widget |XmNactivateCallback (lambda (w c i) (open-file-dialog)))
		   open-widget)
		 
		 (make-popdown-entry "Close" listener-popup close-sound every-menu identity #t)
		 (make-popdown-entry "Save" listener-popup save-sound every-menu edited #t)
		 (make-popdown-entry "Revert" listener-popup revert-sound every-menu edited #t)

		 (let ((panes-widget (|XtCreateManagedWidget "Equalize panes" |xmPushButtonWidgetClass listener-popup every-menu)))
		   (|XtAddCallback panes-widget |XmNactivateCallback (lambda (w c i) (equalize-panes)))
		   panes-widget)

		 (make-popdown-entry "Focus" listener-popup 
				     (lambda (us)
				       (let* ((pane (|Widget (car (sound-widgets us))))
					      (old-resize (auto-resize)))
					 (|XtSetValues (|Widget (cadr (main-widgets))) (list |XmNallowShellResize #f))
					 (for-each 
					  (lambda (them)
					    (|XtUnmanageChild (|Widget (car (sound-widgets them)))))
					  (sounds))
					 (|XtManageChild pane)
					 (|XtSetValues (|Widget (cadr (main-widgets))) (list |XmNallowShellResize (auto-resize)))))
				     every-menu focused #f)

		 (|XtCreateManagedWidget "sep" |xmSeparatorWidgetClass listener-popup every-menu)
		 (let ((exit-widget (|XtCreateManagedWidget "Exit" |xmPushButtonWidgetClass listener-popup every-menu)))
		   (|XtAddCallback exit-widget |XmNactivateCallback (lambda (w c i) (exit)))
		   exit-widget))))

      (|XtAddCallback listener |XmNpopupHandlerCallback 
        (lambda (w data info)
	  (for-each
	   (lambda (n)
	     (if (and (list? n)
		      (equal? (list-ref n 0) 'Popdown))
		 (let ((top-one (list-ref n 1))
		       (top-two (list-ref n 2))
		       (top-two-cascade (list-ref n 3))
		       (len (length ((list-ref n 4) (sounds)))))
		   (|XtUnmanageChild top-two)
		   (|XtUnmanageChild top-two-cascade)
		   (if top-one (|XtUnmanageChild top-one))
		   (if (> len 1) 
		       (begin
			 (|XtManageChild top-two-cascade)
			 (|XtManageChild top-two)))
		   (if (and top-one
			    (= len 1))
		       (|XtManageChild top-one)))
		 (if (|Widget? n)
		     (if (string=? (|XtName n) "Equalize panes")
			 ((if (> (length (sounds)) 1) |XtManageChild |XtUnmanageChild) n)
			 (if (string=? (|XtName n) "Help")
			     ((if (listener-selection) |XtManageChild |XtUnmanageChild) n))))))
	   listener-popup-menu)
	  (set! (|menuToPost info) listener-popup)))
      listener-popup)))

(add-selection-popup)
(define listener-menu (add-listener-popup))

(define (change-listener-popup-color new-color)
  ;; slightly different from the earlier cases because the menu parent is not explicitly in the list
    (change-menu-color listener-menu new-color))



;;; TODO: lisp graph, possibly some of the dialogs


