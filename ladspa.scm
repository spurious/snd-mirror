

;; Use ladspa plugins with the help of a mouse. -Kjetil S. Matheussen.

;; If there is a selection, all buttons apply to the selection, if not
;; they apply to the whole currently selected sound.

;; The realtime preview function does not work very well.
;; The problems are currently:
;; 1. Only channel 0
;; 2. For each chunk processed, call (undo). (the reason for only using channel 0)
;; 3. Graphics may not update.
;; 4. The last edit chunk is not automaticly undone.
;; 
;; Any ideas how to improve the function?


;; This should take care of the functions we need.
;(load "new-effects.scm")




;; Need to keep track of the dac to find out whether to play or stop
;; when pressing the play/stop button.
(define ladspa-isplaying #f)
(add-hook! stop-dac-hook (lambda x (set! ladspa-isplaying #f)))
(add-hook! dac-hook (lambda x (set! ladspa-isplaying #t)))




;; Try as good as possible to apply ladspa. Unfortunately, I can't
;; find a convenient way to get the number of audio inputs/outputs a plug-in provides.
;; Freeverb3 and others will not work because of that.
(define (ladspa-apply-higher-level ladspa-description)
  (define (find-start)
    (if (selection?)
	(selection-position)
	0))
  (define (find-length)
    (if (selection?)
	(selection-frames)
	(frames (selected-sound) 0)))
  (define (do-the-apply reader)
    (apply-ladspa reader ladspa-description (find-length) (car ladspa-description))
    (free-sample-reader reader))
  (define (make-a-sample-reader num_channels start snd)
    (define (recur new_num_channels)
      (if (= -1 new_num_channels)
	  '()
	  (cons (make-sample-reader start snd new_num_channels)
		(recur (- new_num_channels 1)))))
    (let ((gakk (reverse (recur (- num_channels 1)))))
      gakk))
  (define (do-multiple-applies num_channels start snd)
    (define (recur new_num_channels)
      (if (>= new_num_channels 0)
	  (begin
	    (do-the-apply (make-sample-reader (find-start) (selected-sound) new_num_channels))
	    (recur (- new_num_channels 1)))))
    (recur (- num_channels 1)))

  (if (or (= 1 (channels (selected-sound)))
	  (= 0 (sync)))
      (do-the-apply (make-sample-reader (find-start) (selected-sound) (selected-channel)))
      (if #f ;; No easy way to find out how many inputs a plug-in has.
	  (do-the-apply (make-a-sample-reader (channels (selected-sound)) (find-start) (selected-sound)))
	  (do-multiple-applies (channels (selected-sound)) (find-start) (selected-sound)))))
  



(define (make-ladspa-dialog-motifstuff label ok-callback preview-callback preview-stop-callback help-callback)
  ;; Based on make-effect-dialog from new-effects.scm
  (let* ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	 (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	 (xok (XmStringCreate "Apply&Play" XmFONTLIST_DEFAULT_TAG))
	 (titlestr (XmStringCreate label XmFONTLIST_DEFAULT_TAG))
	 (new-dialog (XmCreateTemplateDialog
		       (cadr (main-widgets)) label
		       (list XmNcancelLabelString   xdismiss
			     XmNhelpLabelString     xhelp
			     XmNokLabelString       xok
			     XmNautoUnmanage        #f
			     XmNdialogTitle         titlestr
			     XmNresizePolicy        XmRESIZE_GROW
			     XmNnoResize            #f
			     XmNbackground          (basic-color)
			     XmNtransient           #f))))
    (for-each
     (lambda (button)
       (XtVaSetValues
	 (XmMessageBoxGetChild new-dialog button)
	 (list XmNarmColor   (pushed-button-color)
		XmNbackground (basic-color))))
     (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON))
    
    (XtAddCallback new-dialog XmNcancelCallback (lambda (w c i) (XtUnmanageChild new-dialog)))
    (XtAddCallback new-dialog XmNhelpCallback help-callback)  ; "Help"
    (XtAddCallback new-dialog XmNokCallback (lambda (w c i)
					      (if ladspa-isplaying
						  (begin
						    (stop-playing)))
					      ;; Turn of background processing. Else the player starts before
					      ;; the graphics is shown, and for some reason blocks the graphics update.
					      (set! (with-background-processes) #f)
					      (ok-callback w c i)
					      (set! (with-background-processes) #t)
					      (if (selection?)
						  (play-selection)
						  (play))))


    ;; add a Undo/Apply button
    (let ((undoapply-button (XtCreateManagedWidget "Undo&Apply&Play" xmPushButtonWidgetClass new-dialog
						   (list XmNbackground (basic-color)
							 XmNarmColor   (pushed-button-color)))))
      (XtAddCallback undoapply-button XmNactivateCallback (lambda (w c i)
							    (undo)
							    (if ladspa-isplaying
								(begin
								  (stop-playing)))
							    ;; Turn of background processing. Else the player starts before
							    ;; the graphics is shown, and for some reason blocks the graphics update.
							    (set! (with-background-processes) #f)
							    (ok-callback w c i)
							    (set! (with-background-processes) #t)
							    (if (selection?)
								(play-selection)
								(play)))))

    ;; add a Play button
    (let ((play-button (XtCreateManagedWidget "Play/Stop" xmPushButtonWidgetClass new-dialog
					       (list XmNbackground (basic-color)
						     XmNarmColor   (pushed-button-color)))))
      (XtAddCallback play-button XmNactivateCallback (lambda (w c i)
						       (if ladspa-isplaying
							   (stop-playing)
							   (if (selection?)
							       (play-selection)
							       (play))))))

    ;; add an Undo button
    (let ((undo-button (XtCreateManagedWidget "Stop&Undo" xmPushButtonWidgetClass new-dialog
					       (list XmNbackground (basic-color)
						     XmNarmColor   (pushed-button-color)))))
      (XtAddCallback undo-button XmNactivateCallback (lambda (w c i)
						       (stop-playing)
						       (undo))))

    ;; add a Redo button
    (let ((redo-button (XtCreateManagedWidget "Redo" xmPushButtonWidgetClass new-dialog
					       (list XmNbackground (basic-color)
						     XmNarmColor   (pushed-button-color)))))
      (XtAddCallback redo-button XmNactivateCallback (lambda (w c i)
						       (redo))))



    ;; add a Preview button (experimental deluxe)
    (let ((preview-button (XtCreateManagedWidget "Preview" xmPushButtonWidgetClass new-dialog
						   (list XmNbackground (basic-color)
							 XmNarmColor   (pushed-button-color)))))
      (XtAddCallback preview-button XmNactivateCallback (lambda (w c i)
							    (preview-callback w c i))))

    ;; add a Preview stop button (experimental deluxe)
    (let ((preview-stop-button (XtCreateManagedWidget "Stop preview" xmPushButtonWidgetClass new-dialog
						   (list XmNbackground (basic-color)
							 XmNarmColor   (pushed-button-color)))))
      (XtAddCallback preview-stop-button XmNactivateCallback (lambda (w c i)
							    (preview-stop-callback w c i))))


    (XmStringFree xhelp)
    (XmStringFree xok)
    (XmStringFree xdismiss)
    (XmStringFree titlestr)
    new-dialog))




(define (install-ladspa-menues)

  (let* ((ladspa-effects-menu (add-to-main-menu "Ladspa" (lambda () (update-label '()))))
	 (num-effects-per-submenu 12)
	 (ladspa-effect-num num-effects-per-submenu)
	 (curr-submenu #f))

    (define (ladspa-add-effect-menuitem name proc)
      (if (= num-effects-per-submenu ladspa-effect-num)
	  (begin
	    (set! curr-submenu (XmCreatePulldownMenu (main-menu ladspa-effects-menu) (substring name 0 2)
						     (list XmNbackground (basic-color))))
	    (XtCreateManagedWidget (string-append (substring name 0 (min (string-length name) 20)) " ... ")
				   xmCascadeButtonWidgetClass (main-menu ladspa-effects-menu)
				   (list XmNsubMenuId curr-submenu
					 XmNbackground (basic-color)))
	    (set! ladspa-effect-num -1)))
      
      (let ((child (XtCreateManagedWidget name xmPushButtonWidgetClass curr-submenu
					  (list XmNbackground (basic-color)))))
	(XtAddCallback child XmNactivateCallback
		       (lambda (w c i)
			 (proc))))
      (set! ladspa-effect-num (+ 1 ladspa-effect-num)))
  

    (define (make-ladspadialog ladspa-analysed libraryname effectname)
      (define ladspa-dialog #f)
      (define (param-name param)
	(car param))
      (define (get-param param param-name)
	(let ((mem (member param-name param)))
	  (if mem
	      (cadr mem)
	      0)))
      (define (param-min param)
	(get-param param "minimum"))
      (define (param-max param)
	(get-param param "maximum"))
      (define (param-val param)
	(car (reverse param)))
      (define (param-val! param val)
	(let ((mem (member "dasvalue" param)))
	  (set-car! (cdr mem) val)))
      
      
      (let ((name (car ladspa-analysed))
	    (author (cadr ladspa-analysed))
	    (lisense (caddr ladspa-analysed))
	    (paramlist (cadddr ladspa-analysed))
	    (stopthemadness #f)
	    (ispreviewing #f)
	    (sliders '()))
	
	(define (make-ladspa-description)
	  (append (list libraryname
			effectname)
		  (map (lambda (x) (param-val x))
		       paramlist))) 


	;; Start of the realtime preview code.
	(define (ladspa-preview-sound-chunk start end)
	  (define (do-the-apply reader)
	    (let ((ladspa-description (make-ladspa-description)))
	      (apply-ladspa reader ladspa-description (- end start) (car ladspa-description))
	      (free-sample-reader reader)))
	  (do-the-apply (make-sample-reader start (selected-sound) 0))
	  (play start (selected-sound) 0 #f end))
	(define (ladspa-preview-sound start end chunksize)
	  (define (sound-stop-hook snd)
	    (remove-hook! stop-playing-hook sound-stop-hook)
	    (undo)
	    (if (not stopthemadness)
		(ladspa-preview-sound (+ start chunksize) end chunksize)
		(set! ispreviewing #f)))
	  (if (< start (+ end chunksize))
	      (begin
		(set! ispreviewing #t)
		(add-hook! stop-playing-hook sound-stop-hook)
		(ladspa-preview-sound-chunk start (+ start chunksize)))
	      (set! ispreviewing #f)))
	(define (ladspa-preview-sound-start)	
	  (if (not ispreviewing)
	      (begin 
		(set! stopthemadness #f)
		(ladspa-preview-sound (if (selection?) (selection-position) 0)
				      (if (selection?) (+ (selection-frames) (selection-position)) (frames (selected-sound) 0))
				      (* 8 (dac-size))))))
	(define (ladspa-preview-sound-stop)
	  (set! stopthemadness #t))
	;; End of the realtime preview code.

	
	(define (params-reset!)
	  (for-each (lambda (x) (param-val! x (/ (+ (param-max x) (param-min x) ) 2)))
		    paramlist))
	
	;; Add values for the parameters
	(for-each (lambda (x) (append! x (list "dasvalue" 0)))
		  paramlist)
	
	(params-reset!)
	
	;; Make dialog
	(set! ladspa-dialog 
	      (make-ladspa-dialog-motifstuff
	       name
	       (lambda (w c i)
		 (ladspa-apply-higher-level (make-ladspa-description)))
	       (lambda (w context info)
		 (ladspa-preview-sound-start))
	       (lambda (w c i)
		 (ladspa-preview-sound-stop))
	       (lambda (w context info)
		 (help-dialog author
			      lisense))))
	
	;; Add sliders.
	(set! sliders
	      (add-sliders ladspa-dialog
			   (map (lambda (x)
				  (list (param-name x)
					(param-min x)
					(param-val x)
					(param-max x)
					(lambda (w context info)
					  (param-val! x (/ (.value info) 100.0)))
					100))
				paramlist)))
	
	(ladspa-add-effect-menuitem name (lambda () (activate-dialog ladspa-dialog)))
	#t))
    
    (for-each (lambda (x)
		(make-ladspadialog (analyse-ladspa (car x) (cadr x)) (car x) (cadr x)))
	      (sort (list-ladspa)
		    (lambda (x y)
		      (string<? (car (analyse-ladspa (car x) (cadr x)))
				(car (analyse-ladspa (car y) (cadr y)))))))))




(install-ladspa-menues)



