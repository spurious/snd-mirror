
;; Ladspa2.scm
;; Use ladspa plugins with the help of a mouse. -Kjetil S. Matheussen.


;; Changes 8.8.2003 -> 9.8.2003
;; -Added some workaround code for vst plugins. Previously, applying didnt work.
;; -Possible to use plugins without input audio ports.
;; -Channels were reversed. Fixed
;; -Applying to a single channel allways made the script apply to channel 0. Fixed.
;; -Try to fix dialog-proportions after opening.


;; Changes 5.8.2003 -> 8.8.2003
;; -Fixed segfaults when exiting.
;; -Improved the apply function.
;; -Use the help text from new-plugins.scm written by Dave Phillips.
;; -Fixed the sliders so that sound is updated also when changing
;;  position. Not only after releasing.
;; 

;; ladspa.scm -> ladspa2.scm:
;; -Making heavy use of the new ladspa-functions in snd 6.11.
;; -Realtime preview works perfectly. Seems to at least.
;; -Many plugins can be previewed at the same time.
;; -Previewing is automaticly looped.
;; -The plugins with more than one input or output audio port work.
;; -The default values are based on ladspa hints.
;; -Toggle ports uses toggle buttons instead of sliders.
;; -Integer ports uses integer sliders instead of float sliders.
;; -Initialisation is probably faster.
;; -Added the hour-glass progress reporter when applying.
;;
;; Problems:
;; -Theres a lot of (if (not (string=? "vst" libname)) tests. This is a workaround
;;  to make a vst plugin remember parametersettings even when changed inside a native gui.
;;  However, this can lead to problems when the number of channels for the
;;  selection is changed from what was the situation when
;;  the dialog was opened. Doing a workaround for that situation too is messy as well.
;; -The Model-E vst plugin have no less than 513 input control ports,
;;  and makes the script stop because snd is out of stack. There might
;;  be something wrong with the script. Don't know scheme too well.
;; -In some resize-positions, some widgets becomes invisible.
;; -Realtime preview wont work if number of frames sent to the
;;  dac-hook is more than 8192. (minor problem probably, and is solved
;;  by increasing "maxbuf" in the script to something larger than 8192).



;; This should take care of all the functions we need.
(load-from-path "new-effects.scm")


;(load "/usr/lib/snd/scheme/new-effects.scm")


;; Organize help texts.
(load "ladspa-help.scm")


(define ladspa-help-assoclist '())

(define (insert-ladspa-help alist)
  (if (not (null? alist))
      (begin
	(set! ladspa-help-assoclist 
	      (cons (list (string-append (car alist) (cadr alist))
			  (caddr alist) (cadddr alist))
		    ladspa-help-assoclist))
	(insert-ladspa-help (cddddr alist)))))

(insert-ladspa-help ladspa-help-texts)



;; A for function, a simple version of the one in C. I use this one because "do" is evil. ;)
;; Works like this:
;; for(n=init;n pred least;n+=add)
;;   proc(n);
(define (c-for init pred least add proc)
  (if (pred init least)
      (begin
	(proc init)
	(c-for (+ add init) pred least add proc))))

;;(c-for 0 < 5 1
;;       (lambda (n) (display n)(newline)))


(define (my-filter proc list)
  (if (null? list)
      '()
      (if (proc (car list))
	  (cons (car list) (my-filter proc (cdr list)))
	  (my-filter proc (cdr list)))))



(define (ladspa-class libname plugname maxbuf)
  (define descriptor #f)
  
  ;; List of ladspa plugin handles.
  (define handles '())

  ;; These are lists of numbers.
  (define input-controls '())
  (define output-controls '())
  (define input-audios '())
  (define output-audios '())

  ;; Is #t if ladspa plugin has no audio inputs.
  (define no_audio_inputs #f)

  ;; A ladspa plugin does not need to have equal number of audio
  ;; inputs and outputs. This variable holds the mininum of the two.
  ;; Except when no_audio_inputs is #t. Then it holds the number
  ;; of audio-outputs instead.
  (define min_num_audios 0)


  ;; List of vcts. All handles use the same vcts.
  (define ports '())


  (define isopened #f)


  (define* (open #:optional (num_handles 1))
    (set! ports (map (lambda (port) (if port
					port
					(make-vct maxbuf)))
		     ports))
    (if (= 0 num_handles)
	#t
	(let ((handle (ladspa-instantiate descriptor (srate) )))
	  ;;(display "open called ")(display num_handles)(newline)
	  (if (not handle)
	      (begin
		(close)
		#f)
	      (begin
		(let ((n 0))
		  (for-each (lambda (x)
			      (ladspa-connect-port descriptor handle n x)
			      (set! n (+ n 1)))
			    ports))
		(ladspa-activate descriptor handle)
		(set! handles (cons handle handles))
		(open (- num_handles 1)))))))
  

  ;; Close all handles.
  (define (close)
    ;;(display "close called ")(display (length handles))(newline)
    (if (not (null? handles))
	(let ((handle (car handles)))
	  (ladspa-deactivate descriptor handle)
	  (ladspa-cleanup descriptor handle)
	  (set! handles (cdr handles))
	  (close))))
	  
  
  
  
  (define (input-control-ports)
    input-controls)
  (define (output-control-ports)
    output-controls)
  (define (input-audio-ports)
    input-audios)
  (define (output-audio-ports)
    output-audios)

  (define (get-port portnum)
    (list-ref ports portnum))

  (define (set-port portnum vct)
    (let ((n 0))
      (set! ports (map (lambda (x)
			 (if (= n portnum) vct x)
			 (set! n (+ n 1)))
		       ports)))
    (ladspa-connect-port descriptor handle portnum vct))

  (define (get-num-input-audio-ports)
    (length input-audios))
  (define (get-input-audio-port num)
    (get-port (list-ref input-audios num)))
  (define (set-input-audio-port num vct)
    (set-port (list-ref input-audios num) vct))

  (define (get-num-output-audio-ports)
    (length output-audios))
  (define (get-output-audio-port num)
    (get-port (list-ref output-audios num)))
  (define (set-output-audio-port num vct)
    (set-port (list-ref output-audios num) vct))


  (define (get-descriptor)
    descriptor)


  ;; Warning, len must not be larger than maxbuf.
  (define (run len)
    (for-each (lambda (handle) (ladspa-run descriptor handle len))
	      handles))


  (define (input-control-set! num val)
    (vct-set! (list-ref ports num) 0 val))

  (define (get-input-control num)
    (vct-ref (list-ref ports num) 0))

;(((x) & LADSPA_HINT_DEFAULT_MASK)   \
;                                           == LADSPA_HINT_DEFAULT_0)



  (define (set-default-input-controls)
    (for-each (lambda (x)
		(let ((hint (car (cadr x)))
		      (lo (cadr (cadr x)))
		      (hi (caddr (cadr x))))
		  (define (ishint dashint)
		    (= (logand hint LADSPA_HINT_DEFAULT_MASK) dashint))
		  (define (ishint_notdefault dashint)
		    (not (= (logand hint dashint ) 0)))
		  (input-control-set! (car x) 
				      (cond ((ishint LADSPA_HINT_DEFAULT_0) 0)
					    ((ishint LADSPA_HINT_DEFAULT_MINIMUM) lo)
					    ((ishint LADSPA_HINT_DEFAULT_LOW) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
										  (exp (+ (* 0.75 (log lo)) (* 0.25 (log hi))))
										  (+ (* 0.75 lo) (* 0.25 hi))))
					    ((ishint LADSPA_HINT_DEFAULT_1) 1)
					    ((ishint LADSPA_HINT_DEFAULT_MAXIMUM) hi)
					    ((ishint LADSPA_HINT_DEFAULT_HIGH) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
										  (exp (+ (* 0.75 (log hi)) (* 0.25 (log lo))))
										  (+ (* 0.75 hi) (* 0.25 lo))))
					    ((ishint LADSPA_HINT_DEFAULT_MIDDLE) (if (ishint_notdefault LADSPA_HINT_LOGARITHMIC)
										     (exp (+ (* 0.5 (log hi)) (* 0.5 (log lo))))
										     (+ (* 0.5 hi) (* 0.5 lo))))
					    ((ishint LADSPA_HINT_DEFAULT_100) 100)
					    ((ishint LADSPA_HINT_DEFAULT_440) 440)
					    (else
					     (/ (+ lo hi) 2))))))
		
	      (map (lambda (x) (list x (list-ref (.PortRangeHints descriptor) x)))
		   input-controls)))
  
  
  
  (define (minimum-num-handles sndchannels pluginchannels)
    (+ (if (> (modulo sndchannels pluginchannels) 0) 1 0)
       (inexact->exact (floor (/ sndchannels pluginchannels)))))



  ;; This one always return #f because it can be used as a hook.
  (define (apply-soundobject sdobj)
    (let* ((len (sound-data-length sdobj))
	   (num_chans (sound-data-chans sdobj)))
      (if (> len maxbuf)
	  (begin 
	    (display "Ladspa buffer to small. Can't use ladspa plugin in realtime.")
	    (display "You can try to reduce the dac-size to fix the problem.")
	    (newline))
	  (begin 
	    (let ((chan 0))
	      (for-each (lambda (handle)
			  (if (not no_audio_inputs)
			      (c-for 0 < min_num_audios 1
				     (lambda (n)
				       (if (< (+ chan n) num_chans)
					   (sound-data->vct sdobj
							    (+ chan n)
							    (get-input-audio-port n))))))
			  (ladspa-run descriptor handle len)
			  (c-for 0 < min_num_audios 1
				 (lambda (n)
				   (if (< (+ chan n) num_chans)
				       (vct->sound-data (get-output-audio-port n)
							sdobj
							(+ chan n)))))
			  (set! chan (+ chan min_num_audios)))
			handles)))))
    #f)



    

  (define (apply!)
    (define (get-startchan snd chan)
      (if (selection-member? snd chan)
	  chan
	  (get-startchan snd (+ chan 1))))
    (if (not (selection-member? (selected-sound)))
	(select-all graph-popup-snd graph-popup-chn))
    (let* ((snd (selected-sound))
	   (start (selection-position))
	   (length (selection-frames))
	   (end (+ (selection-position) (selection-frames)))
	   (chans (selection-chans))
	   (startchan (get-startchan snd 0))
	   (tempfilenames '())
	   (new-files '())
	   (vct-out (make-vct maxbuf))
	   (sdobj (make-sound-data chans maxbuf))
	   (readers '()))

      (define (apply-open)
	(if (not (string=? "vst" libname))
	    (open (minimum-num-handles chans min_num_audios))
	    #t))

      (if (not (apply-open))
	  (begin
	    (display "Could not start plugin.")
	    (newline))
	  (begin

	    ;; Set up sample readers and tempfilenames for each channel.
	    (c-for 0 < chans 1
		   (lambda (ch)
		     (set! readers (cons (make-sample-reader start
							     snd
							     (+ startchan ch))
					 readers))
		     (set! tempfilenames (cons (snd-tempnam)
					       tempfilenames))))

	    ;; Create temporary files. One for each channel.
	    (set! new-files (map (lambda (filename) (open-sound-file filename 1 (srate)))
				 tempfilenames))


	    ;; Start the hour-glass
	    (start-progress-report)

	    ;; Do the ladspa.
	    (c-for 0 < (+ maxbuf length) maxbuf
		   (lambda (n)
		     (if (< n length)
			 (let ((len (min (- length n)
					 maxbuf)))
			   
			   ;;(display "N: ")(display n)
			   ;;(display " len: ")(display len)
			   ;;(display " length: ")(display length)(newline)
			   
			   ;; Update hour-glass
			   (progress-report (/ n length) "doing the ladspa" chans chans snd)


			   ;; Only happens at last iteration.
			   (if (< len maxbuf)
			       (set! sdobj (make-sound-data chans len))
			       (set! vct-out (make-vct len)))


			   ;; Reading data into soundobject from soundfile.
			   (if (not no_audio_inputs)
			       (let ((ch 0))
				 (for-each (lambda (reader)
					     (vct-map! vct-out
						       (lambda () (next-sample reader)))
					     (vct->sound-data vct-out sdobj ch)
					     (set! ch (+ ch 1)))
					   readers)))

			   ;; Process soundobject
			   (apply-soundobject sdobj)


			   ;; Writing data from soundobject into temporary file.
			   (c-for 0 < chans 1
				  (lambda (ch)
				    (sound-data->vct sdobj ch vct-out)
				    (vct->sound-file (list-ref new-files ch)
						     vct-out
						     len)))))))
	    

	    ;; Close temporary files.
	    (for-each (lambda (file) (close-sound-file file (* 4 length)))
		      new-files)

	    ;; Let snd know about the new files.
	    (let ((ch startchan))
	      (for-each (lambda (filename)
			  (set! (samples start
					 length
					 snd
					 ch)
				filename)
			  (set! ch (+ ch 1)))
			(reverse tempfilenames)))
	    
	    ;; Close hour-glass
	    (finish-progress-report)

	    ;; Close plugin.
	    (if (not (string=? "vst" libname))
		(close))))))


      


  (define (add-dac-hook!)
    (let* ((num_channels (channels (selected-sound)))
	   (num_ins (length input-audios))
	   (num_outs (length output-audios)))
      (define (init-dac-hook-stuff)
	(if (not (open (minimum-num-handles num_channels min_num_audios)))
	    #f
	    (begin
	      (add-hook! dac-hook apply-soundobject)
	      #t)))
      (if (= 0 min_num_audios)
	  (begin
	    (display "Ladspa plugin have no output audio ports.")(newline)
	    #f)
	  (if (not (string=? "vst" libname))
	      (init-dac-hook-stuff)
	      (if (null? handles)
		  (init-dac-hook-stuff)
		  (begin
		    (add-hook! dac-hook apply-soundobject)
		    #t))))))


  (define (remove-dac-hook!)
    (remove-hook! dac-hook apply-soundobject)
    (if (not (string=? "vst" libname))    
	(close)))


  (define (constructor)
    (set! descriptor (ladspa-descriptor libname plugname))
    (if (not descriptor)
	#f
	(begin
	  (let ((n 0))
	    (for-each (lambda (x)
			(if (> (logand x LADSPA_PORT_CONTROL) 0)
			    (if (> (logand x LADSPA_PORT_INPUT) 0)
				(set! input-controls (cons n input-controls))
				(set! output-controls (cons n output-controls)))
			    (if (> (logand x LADSPA_PORT_INPUT) 0)
				(set! input-audios (cons n input-audios))
				(set! output-audios (cons n output-audios))))
			(set! n (+ n 1)))
		      (.PortDescriptors descriptor)))
	  (set! input-controls (reverse input-controls))
	  (set! output-controls (reverse output-controls))
	  (set! input-audios (reverse input-audios))
	  (set! output-audios (reverse output-audios))
	  (if (= (length input-audios) 0)
	      (begin
		(set! min_num_audios (length output-audios))
		(set! no_audio_inputs #t))
	      (set! min_num_audios (min (length input-audios) (length output-audios))))
	  (set! ports (map (lambda (x) (if (> (logand x LADSPA_PORT_CONTROL) 0)
					   (make-vct 1)
					   #f))
			   (.PortDescriptors descriptor)))
	  (set-default-input-controls)
	  #t)))

    

  (define (dispatcher m)
    (cond ((eq? m 'open) open)
	  ((eq? m 'close) close)
	  ((eq? m 'run) run)

	  ((eq? m 'input-control-ports) input-control-ports)
	  ((eq? m 'output-control-ports) output-control-ports)
	  ((eq? m 'input-audio-ports) input-audio-ports)
	  ((eq? m 'output-audio-ports) output-audio-ports)

	  ((eq? m 'input-control-set!) input-control-set!)
	  ((eq? m 'get-input-control) get-input-control)

	  ((eq? m 'get-port) get-port)
	  ((eq? m 'set-port) set-port)

	  ((eq? m 'get-num-input-audio-ports)  get-num-input-audio-ports)
	  ((eq? m 'get-input-audio-port)   get-input-audio-port)
	  ((eq? m 'set-input-audio-port)  set-input-audio-port)
	  ((eq? m 'get-num-output-audio-ports)  get-num-output-audio-ports)
	  ((eq? m 'get-output-audio-port)  get-output-audio-port)
	  ((eq? m 'set-output-audio-port)  set-output-audio-port)

	  ((eq? m 'set-default-input-controls) set-default-input-controls)

	  ((eq? m 'constructor) constructor)

	  ((eq? m 'descriptor) get-descriptor)

	  ((eq? m 'add-dac-hook!) add-dac-hook!)
	  ((eq? m 'remove-dac-hook!) remove-dac-hook!)

	  ((eq? m 'apply!) apply!)

	  (else
	   (display "Unknown message to ladspa-class: ")
	   (display m)
	   (newline))))

  dispatcher)



(define (make-ladspa-dialog-motifstuff label play-callback stop-callback ok-callback cancel-callback help-callback)
  ;; Based on make-effect-dialog from new-effects.scm
  (let* ((xdismiss (XmStringCreate "Close" XmFONTLIST_DEFAULT_TAG))
	 (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	 (xok (XmStringCreate "Apply" XmFONTLIST_DEFAULT_TAG))
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
     (lambda (button color)
       (XtVaSetValues
	 (XmMessageBoxGetChild new-dialog button)
	 (list XmNarmColor   (pushed-button-color)
		XmNbackground color)))
     (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
     (list (help-button-color) (quit-button-color) (doit-button-color)))
    

    (XtAddCallback new-dialog XmNcancelCallback (lambda (w c i)
						  (cancel-callback)
						  (XtUnmanageChild new-dialog)
						  (focus-widget (list-ref (channel-widgets (selected-sound) 0) 0))))

    (XtAddCallback new-dialog XmNhelpCallback (lambda (w c i) (help-callback)))
    (XtAddCallback new-dialog XmNokCallback (lambda (w c i) (ok-callback)))


    ;; add a Play button
    (let ((play-button (XtCreateManagedWidget "Play" xmPushButtonWidgetClass new-dialog
					      (list XmNbackground (basic-color)
						    XmNarmColor   (pushed-button-color)))))
      (XtAddCallback play-button XmNactivateCallback (lambda (w c i)
						       (play-callback))))

    ;; add a Stop button
    (let ((stop-button (XtCreateManagedWidget "Stop" xmPushButtonWidgetClass new-dialog
						 (list XmNbackground (basic-color)
						       XmNarmColor   (pushed-button-color)))))
      (XtAddCallback stop-button XmNactivateCallback (lambda (w c i)
							  (stop-callback))))



    (XmStringFree xok)
    (XmStringFree xdismiss)
    (XmStringFree titlestr)
    (XmStringFree xhelp)
    new-dialog))



(define (install-ladspa-menues)  

  ;; Increase this number if you can't preview sound because of large latency in the system.
  ;; Note, this is not the latency, just the maximum buffer size. The only bad consequence about
  ;; increasing this number, is the increased use of memory.

  ;; Note, because of a bug in the vstserver,
  ;; 1024 is used as maximimum buffer size for vst plugins, no matter what maxbuf is.

  (define maxbuf 8192)


  
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
      (define ladspa-object #f)
      (define ladspa-dialog #f)
      (define slider-funcs '())
      
      (let ((name (car ladspa-analysed))
	    (author (cadr ladspa-analysed))
	    (lisense (caddr ladspa-analysed))
	    (isplaying #f)
	    (onoffbutton #f)
	    (plugin_enable #f)
	    (islooping #f)
	    (isplayingselection #f)
	    (sliders '()))

	(define (ShowDialog)
	  (activate-dialog ladspa-dialog)
	  (XtSetValues onoffbutton (list XmNset #t))
	  (let ((num_inputs (+ 1 (length ((ladspa-object 'input-control-ports))))))
	    (set! (widget-size ladspa-dialog) (list (min 800 (max 400 (* num_inputs 20)))
						    (min 800 (max 120 (* num_inputs 70))))))
	  (enableplugin))

	(define (Help)
	  (let ((dashelp (assoc (string-append libraryname effectname) ladspa-help-assoclist)))
	    (help-dialog author
			 (if dashelp
			     (caddr dashelp)
			     lisense))))
	
	(define (OK)
	  (MyStop)
	  (disableplugin)
	  (XtSetValues onoffbutton (list XmNset #f))
	  ((ladspa-object 'apply!)))

	(define (Cancel)
	  (disableplugin)
	  (if (string=? "vst" libraryname)
	      ((ladspa-object 'close)))
	  (XtSetValues onoffbutton (list XmNset #f)))


	(define (MyPlay)
	  (if (not (selection-member? (selected-sound)))
	      (select-all graph-popup-snd graph-popup-chn))
	  (add-hook! stop-playing-selection-hook play-selection)
	  (play-selection))

	(define (MyStop)
	  (remove-hook! stop-playing-selection-hook play-selection)
	  (stop-playing))

	(define (enableplugin)
	  (if (not ((ladspa-object 'add-dac-hook!)))
	      (begin
		(display "Unable use ladspa plugin")
		(newline))))

	(define (disableplugin)
	  ((ladspa-object 'remove-dac-hook!)))

	(define (onoff onoroff)
	  (set! plugin_enable onoroff)
	  (if onoroff
	      (enableplugin)
	      (disableplugin)))

	(define (ishint dashint dashint2)
	  (not (= (logand dashint dashint2 ) 0)))

	(set! ladspa-object (ladspa-class libraryname effectname (if (string=? "vst" libraryname)
								     1024
								     maxbuf)))
	(if ((ladspa-object 'constructor))
	    (let ((descriptor ((ladspa-object 'descriptor))))


	      ;; Make sure the plugin is disabled before quitting.
	      (add-hook! exit-hook (lambda args
				     (disableplugin)
				     (if (string=? "vst" libraryname)
					 ((ladspa-object 'close)))
				     #f))

	      
	      ;; Make dialog
	      (set! ladspa-dialog 
		    (make-ladspa-dialog-motifstuff name
						   MyPlay
						   MyStop
						   OK
						   Cancel
						   Help))

	      ;; Add sliders.
	      (if (not (null? ((ladspa-object 'input-control-ports))))
		  (set! sliders
			(add-sliders ladspa-dialog
				     (map (lambda (portnum)
					    (let* ((lo (cadr (list-ref (.PortRangeHints descriptor) portnum)))
						   (init ((ladspa-object 'get-input-control) portnum))
						   (hi (caddr (list-ref (.PortRangeHints descriptor) portnum)))
						   (hint (car (list-ref (.PortRangeHints descriptor) portnum)))
						   (scale (if (ishint hint LADSPA_HINT_INTEGER)
							      1
							      100.0))
						   (islog (ishint hint LADSPA_HINT_LOGARITHMIC)))
					      (if (and #f islog (>= hi 0) (> hi lo) (not (= lo 0)) (not (= (log (max lo 1.0)) (log hi))))
						  ;; Doesnt work very well for this purpose.
						  (list (list-ref (.PortNames descriptor) portnum)
							lo
							(if (= 0 (inexact->exact init))
							    0.1
							    init)
							hi
							(lambda (w context info)
							  (display (.value info))(newline)
							  ((ladspa-object 'input-control-set!) portnum (/ (.value info) scale)))
							scale
							'log)
						  (begin
						    (set! slider-funcs (cons (lambda (w c info)
									       ((ladspa-object 'input-control-set!) portnum (/ (.value info)
															       scale)))
									     slider-funcs))
						    (list (list-ref (.PortNames descriptor) portnum)
							  lo
							  init
							  hi
							  (car slider-funcs)
							  scale)))))
					  (my-filter (lambda (portnum) (not (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED)))
						     ((ladspa-object 'input-control-ports))))
				     #f)))
	      

	      ;; Fix the sliders
	      (set! slider-funcs (reverse slider-funcs))
	      
	      (for-each (lambda (slider slider-func)
			  (XtAddCallback slider XmNdragCallback slider-func))
			sliders
			slider-funcs)

	      
	      ;; Add toggle buttons.
	      (for-each (lambda (portnum)
			  (let* ((hint (car (list-ref (.PortRangeHints descriptor) portnum)))
				 (hi (caddr (list-ref (.PortRangeHints descriptor) portnum)))
				 (lo (cadr (list-ref (.PortRangeHints descriptor) portnum)))
				 (toggle (XtCreateManagedWidget (list-ref (.PortNames descriptor) portnum)
								xmToggleButtonWidgetClass
								(if (null? sliders) ladspa-dialog (XtParent (car sliders)))
								(list XmNbackground       (basic-color)
								      XmNset              (> ((ladspa-object 'get-input-control) portnum) 0)
								      XmNselectColor      (yellow-pixel)))))
			    (XtAddCallback toggle XmNvalueChangedCallback (lambda (w c i)
									    ((ladspa-object 'input-control-set!) portnum (if (.set i) hi lo))))))

			(my-filter (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
				   ((ladspa-object 'input-control-ports))))



	      ;; Add on/off button.
	      (set! onoffbutton (XtCreateManagedWidget "On/off" xmToggleButtonWidgetClass (if (null? sliders) ladspa-dialog (XtParent (car sliders)))
						       (list XmNbackground       (basic-color)
							     XmNset              #f
							     XmNselectColor      (yellow-pixel))))
	      (XtAddCallback onoffbutton XmNvalueChangedCallback (lambda (w c i) (onoff (.set i))))



	      (ladspa-add-effect-menuitem name (lambda () (ShowDialog)))
	      #t))))
      
    (for-each (lambda (x)
		(make-ladspadialog (caddr x) (car x) (cadr x)))
	      (sort (map (lambda (listpart) (list (car listpart)
						  (cadr listpart)
						  (analyse-ladspa (car listpart) (cadr listpart))))
			 (list-ladspa))
		    (lambda (x y)
		      (string<? (car (caddr x))
				(car (caddr y))))))))
				     


(install-ladspa-menues)


