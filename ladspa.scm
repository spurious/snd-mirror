
;; ladspa.scm
;; Use ladspa plugins with the help of a mouse. -Kjetil S. Matheussen.

;; Changes 9.8.2003 ->
;; Many changes.
;;
;; Changes 8.8.2003 -> 9.8.2003
;; -Added some workaround code for vst plugins. Previously, applying didn't work.
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
;;  by increasing "ladspa-maxbuf" to something larger than 8192).




(provide 'snd-ladspa.scm)



;; Increase this number if you can't preview sound because of large latency in the system.
;; Note, this is not the latency, just the maximum buffer size. The only bad consequence about
;; increasing this number, is the increased use of memory.

(define ladspa-maxbuf 8192)



;; This should take care of the functions we need.
(if (not (provided? 'snd-gui.scm))
    (load-from-path "gui.scm"))



;; Organize help texts.
(if (not (provided? 'snd-ladspa-help.scm))
    (load-from-path "ladspa-help.scm"))


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






(define-class (<ladspa> libname plugname)

  (var descriptor (ladspa-descriptor libname plugname))
 
  ;; <array> of ladspa plugin handles.
  (define handles #f)

  ;; These are lists of numbers.
  (var input-controls '())
  (var output-controls '())

  (define input-audios '())
  (define output-audios '())

  ;; Is #t if ladspa plugin has no audio inputs.
  (define no_audio_inputs #f)

  ;; A ladspa plugin does not need to have equal number of audio
  ;; inputs and outputs. This variable holds the mininum of the two.
  ;; Except when no_audio_inputs is #t. Then it holds the number
  ;; of audio-outputs instead.
  (define min_num_audios 0)


  ;; <array> of vcts. All handles use the same vcts.
  (define ports #f)


  (define isopened #f)


  (define* (open #:optional (num_handles 1))
    (-> ports map! (lambda (n port) (or port 
					(make-vct ladspa-maxbuf))))
    (call-with-current-continuation
     (lambda (return)
       (set! handles (<array/map> num_handles
				  (lambda (n)
				    (let ((handle (ladspa-instantiate this->descriptor (srate) )))
				      (if (not handle)
					  (begin
					    (c-display "Error: Could not make ladspa handle.")
					    (this->close)
					    (return #f))
					  (begin
					    (-> ports for-each (lambda (n port)
								 (ladspa-connect-port this->descriptor handle n port)))
					    (ladspa-activate this->descriptor handle)))
				      handle))))))
    #t)


  ;; Close all handles.
  (define-method (close)
    ;;(display "close called ")(display (-> handles length))(newline)
    (if handles
	(-> handles for-each (lambda (n handle)
			       (if handle
				   (begin
				     (ladspa-deactivate this->descriptor handle)
				     (ladspa-cleanup this->descriptor handle))))))
    (set! handles #f))


  (define (get-port portnum)
    (ports portnum))

  (define (set-port! portnum vct)
    (display "ai: ")(display handle)(newline)
    (ports portnum vct)
    (ladspa-connect-port this->descriptor handle portnum vct)) ;; Which handle???

  (define (get-num-input-audio-ports)
    (length input-audios))
  (define (get-input-audio-port num)
    (get-port (list-ref input-audios num)))
  (define (set-input-audio-port num vct)
    (set-port! (list-ref input-audios num) vct))

  (define (get-num-output-audio-ports)
    (length output-audios))
  (define (get-output-audio-port num)
    (get-port (list-ref output-audios num)))
  (define (set-output-audio-port num vct)
    (set-port! (list-ref output-audios num) vct))


  ;; Warning, len must not be larger than ladspa-maxbuf.
  (define (run len)
    (-> handles for-each (lambda (n handle)
			   (ladspa-run this->descriptor handle len))))


  (define (set-default-input-controls)
    (for-each (lambda (x)
		(let ((hint (car (x 1)))
		      (lo (cadr (x 1)))
		      (hi (caddr (x 1))))
		  (define (ishint dashint)
		    (= (logand hint LADSPA_HINT_DEFAULT_MASK) dashint))
		  (define (ishint_notdefault dashint)
		    (not (= (logand hint dashint ) 0)))
		  (this 'input-control-set! (x 0) 
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
	      
	      (map (lambda (x) (<array> x (list-ref (.PortRangeHints this->descriptor) x)))
		   this->input-controls)))
  
  
  
  (define (minimum-num-handles sndchannels pluginchannels)
    (+ (if (> (modulo sndchannels pluginchannels) 0) 1 0)
       (inexact->exact (floor (/ sndchannels pluginchannels)))))



  ;; This one always return #f because it can be used as a hook.
  (define apply-soundobject
    (let ((len #f)
	  (num_chans #f)
	  (chan #f))
      (lambda (sdobj)
	(set! len (sound-data-length sdobj))
	(set! num_chans (sound-data-chans sdobj))
	(if (> len ladspa-maxbuf)
	    (begin 
	      (display "Ladspa buffer to small. Can't use ladspa plugin in realtime.")
	      (display "You can try to reduce the dac-size to fix the problem.")
	      (newline))
	    (begin
	      (set! chan 0)
	      (-> handles for-each (lambda (handlenum handle)
				     (if (not no_audio_inputs)
					 (c-for 0 < min_num_audios 1
						(lambda (n)
						  (if (< (+ chan n) num_chans)
						      (sound-data->vct sdobj
								       (+ chan n)
								       (get-input-audio-port n))))))
				     (ladspa-run this->descriptor handle len)
				     (c-for 0 < min_num_audios 1
					    (lambda (n)
					      (if (< (+ chan n) num_chans)
						  (vct->sound-data (get-output-audio-port n)
								   sdobj
								   (+ chan n)))))
				     (set! chan (+ chan min_num_audios))))))
	#f)))


  (define-method (apply!)
    (define (get-startchan snd chan)
      (if (selection-member? snd chan)
	  chan
	  (get-startchan snd (+ chan 1))))
    (if (not (selection-member? (selected-sound)))
	(select-all (selected-sound)))
    (let* ((snd (selected-sound))
	   (start (selection-position snd))
	   (length (selection-frames snd))
	   (end (+ start length))
	   (chans (selection-chans snd))
	   (startchan (get-startchan snd 0))
	   (tempfilenames '())
	   (new-files '())
	   (vct-out (make-vct ladspa-maxbuf))
	   (sdobj (make-sound-data chans ladspa-maxbuf))
	   (isbreaked #f)
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
	    (c-for 0 < (+ ladspa-maxbuf length) ladspa-maxbuf
		   (lambda (n)
		     (if (< n length)
			 (let ((len (min (- length n)
					 ladspa-maxbuf)))
			   
			   ;;(display "N: ")(display n)
			   ;;(display " len: ")(display len)
			   ;;(display " length: ")(display length)(newline)

			   (if (or isbreaked (c-g?))
			       (if (not isbreaked) (set! isbreaked #t))
			       (begin
			       
				 ;; Update hour-glass
				 (progress-report (/ n length) "doing the ladspa" chans chans snd)
				 
				 ;; Only happens at last iteration.
				 (if (< len ladspa-maxbuf)
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
				 
				 ;; Writing data from soundobject into temporary files.
				 (c-for 0 < chans 1
					(lambda (ch)
					  (mus-sound-write (list-ref new-files ch)
							   0
							   len
							   1
							   sdobj)))))))))
	    
	    ;; Close temporary files.
	    (for-each (lambda (file) (close-sound-file file (* 4 length)))
		      new-files)

	    ;; Let snd know about the new files.
	    (if (not isbreaked)
		(let ((ch startchan))
		  (for-each (lambda (filename)
			      (set! (samples start
					     length
					     snd
					     ch)
				    filename)
			      (set! ch (+ ch 1)))
			    (reverse tempfilenames))))
	    
	    ;; Close hour-glass
	    (finish-progress-report)

	    ;; Close plugin.
	    (if (not (string=? "vst" libname))
		(this->close))))))


      


  (define-method (add-dac-hook!)
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
	    (c-display "Ladspa plugin has no output audio ports.")
	    #f)
	  (if (not (string=? "vst" libname))
	      (init-dac-hook-stuff)
	      (if (not handles)
		  (init-dac-hook-stuff)
		  (begin
		    (add-hook! dac-hook apply-soundobject)
		    #t))))))


  (define-method (remove-dac-hook!)
    (remove-hook! dac-hook apply-soundobject)
    (if (not (string=? "vst" libname))    
	(this->close)))

  (define-method (input-control-set! num val)
    (vct-set! (ports num) 0 val))

  (define-method (get-input-control num)
    (vct-ref (ports num) 0))


  ;; Constructor:
  (if (not this->descriptor)
      (set! this #f)
      (begin
	(let ((n 0))
	  (for-each (lambda (x)
		      (if (> (logand x LADSPA_PORT_CONTROL) 0)
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! this->input-controls (append this->input-controls (list n)))
			      (set! this->output-controls (append this->output-controls (list n))))
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! input-audios (append input-audios (list n)))
			      (set! output-audios (append output-audios (list n)))))
		      (set! n (+ n 1)))
		    (.PortDescriptors this->descriptor)))
	(if (= (length input-audios) 0)
	    (begin
	      (set! min_num_audios (length output-audios))
	      (set! no_audio_inputs #t))
	    (set! min_num_audios (min (length input-audios) (length output-audios))))
	(set! ports (apply <array> (map (lambda (x) (if (> (logand x LADSPA_PORT_CONTROL) 0)
							(make-vct 1)
							#f))
					(.PortDescriptors this->descriptor))))
	(set-default-input-controls))))



(define (install-ladspa-menues)  

  (let* ((ladspa-effects-menu (add-to-main-menu "Ladspa" (lambda x #f)))
	 (num-effects-per-submenu 12)
	 (ladspa-effect-num num-effects-per-submenu)
	 (curr-submenu #f))
    
    (define (ladspa-add-effect-menuitem name proc)
      (if (= num-effects-per-submenu ladspa-effect-num)
	  (begin
	    (set! curr-submenu (menu-sub-add ladspa-effects-menu (string-append (substring name 0 (min (string-length name) 20)) " ... ")))
	    (set! ladspa-effect-num -1)))
      (menu-add curr-submenu name proc)
      (set! ladspa-effect-num (+ 1 ladspa-effect-num)))
    
    
    (define (make-ladspadialog ladspa-analysed libraryname effectname)
      (define ladspa #f)
      (define dialog #f)
      
      (let ((name (car ladspa-analysed))
	    (author (cadr ladspa-analysed))
	    (lisense (caddr ladspa-analysed))
	    (isplaying #f)
	    (onoffbutton #f)
	    (islooping #f)
	    (isplayingselection #f))

	(define (ShowDialog)
	  (MakeDialogIfNotMade)
	  (-> dialog show)
	  (-> onoffbutton set #t)
	  (enableplugin)
	  )

	(define (Help)
	  (let ((dashelp (assoc (string-append libraryname effectname) ladspa-help-assoclist)))
	    (help-dialog author
			 (string-append (if dashelp
					    (caddr dashelp)
					    lisense)
					(string #\newline #\newline)
					"Processing can be stopped by pressing C-g"))))
	  
	(define (OK)
	  (MyStop)
	  (disableplugin)
	  (-> onoffbutton set #f)
	  (-> ladspa apply!))

	(define (Cancel)
	  (-> onoffbutton set #f)
	  (-> dialog hide)
	  (disableplugin)
	  (if (string=? "vst" libraryname)
	      (-> ladspa close)))

	(define (MyPlay)
	  (if (not (selection-member? (selected-sound)))
	      (select-all (selected-sound)))
	  (letrec ((das-play (lambda ()
			       (play-selection #f #f
					       (lambda (x)
						 (if (and (= x 0) c-islooping)
						     (das-play)))))))
	    (das-play)))

	(define (MyStop)
	  (remove-hook! stop-playing-selection-hook play-selection)
	  (stop-playing))

	(define (enableplugin)
	  (if (not (-> ladspa add-dac-hook!))
	      (begin
		(display "Unable to use ladspa plugin.")
		(newline))))

	(define (disableplugin)
	  (-> ladspa remove-dac-hook!))

	(define (onoff onoroff)
	  (if onoroff
	      (enableplugin)
	      (disableplugin)))

	(define (ishint dashint dashint2)
	  (not (= (logand dashint dashint2 ) 0)))

	(define (MakeDialogIfNotMade)
	  (if (not dialog)
	      (let ((descriptor (-> ladspa descriptor)))

	    
		;; Make sure the plugin is disabled before quitting.
		(add-hook! exit-hook (lambda args
				       (disableplugin)
				       (if (string=? "vst" libraryname)
					   (-> ladspa close))
				       #f))
		
	    
		;; Make dialog
		(set! dialog 
		      (<dialog> name Cancel
				"Close" Cancel
				"Apply" OK
				"Play" MyPlay
				"Stop" MyStop
				(if (assoc (string-append libraryname effectname) ladspa-help-assoclist)
				    "Help"
				    "Not much help")
				Help))
		
		;; Add sliders.
		(if (not (null? (-> ladspa input-controls)))
		    (dialog 'add-sliders
			    (map (lambda (portnum)
				   (let* ((lo (cadr (list-ref (.PortRangeHints descriptor) portnum)))
					  (init (-> ladspa get-input-control portnum))
					  (hi (caddr (list-ref (.PortRangeHints descriptor) portnum)))
					  (hint (car (list-ref (.PortRangeHints descriptor) portnum)))
					  (scale (if (ishint hint LADSPA_HINT_INTEGER)
						     1
						     (if use-gtk 1000.0 100.0)))
					  (islog (ishint hint LADSPA_HINT_LOGARITHMIC)))
				     
				     (if #f
					 (begin
					   (if (ishint hint LADSPA_HINT_SAMPLE_RATE)
					       (display "HINT_SAMPLE_RATE "))
					   (if (ishint hint LADSPA_HINT_BOUNDED_ABOVE)
					       (display "HINT_BOUNDED_ABOVE "))
					   (if (ishint hint LADSPA_HINT_BOUNDED_BELOW)
					       (display "HINT_BOUNDED_BELOW "))
					   (display (list-ref (.PortNames descriptor) portnum))(display " ")(display hint)(display " ")
					   (display lo)(display " ")(display init)(display " ")(display hi)(newline)))
				     
				     (if (not (ishint hint LADSPA_HINT_BOUNDED_BELOW))
					 (set! lo 0))                                   ;The value Ardour use.
				     (if (not (ishint hint LADSPA_HINT_BOUNDED_ABOVE))
					 (set! hi 4))                                   ;The value Ardour use.
				     (if (ishint hint LADSPA_HINT_SAMPLE_RATE)
					 (begin
					   (set! lo (* lo (srate)))
					   (set! hi (* hi (srate)))))
				     
				     (list (list-ref (.PortNames descriptor) portnum)
					   lo
					   init
					   hi
					   (lambda (val) (-> ladspa input-control-set! portnum val))
					   scale)))
				 (remove (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
					 (-> ladspa input-controls)))
			    ))
		
		
		;; Add toggle buttons.
		(for-each (lambda (portnum)
			    (let* ((hint (car (list-ref (.PortRangeHints descriptor) portnum)))
				   (hi (caddr (list-ref (.PortRangeHints descriptor) portnum)))
				   (lo (cadr (list-ref (.PortRangeHints descriptor) portnum)))
				   (portname (list-ref (.PortNames descriptor) portnum))
				   (ison (> (-> ladspa get-input-control portnum) 0)))
			      (<checkbutton> dialog
					     portname
					     (lambda (on)
					       (-> ladspa input-control-set! portnum (if on hi lo)))
					     ison)))
			  (filter-org (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
				      (-> ladspa input-controls)))
		
	    
		;; Add on/off button.
		(set! onoffbutton (<checkbutton> dialog
						 "On/off"
						 onoff
						 #t)))))
	
	  
	(set! ladspa (<ladspa> libraryname effectname))
	
	(if ladspa
	    (ladspa-add-effect-menuitem name ShowDialog)
	    #t)))

    (for-each (lambda (x)
		(make-ladspadialog (x 2) (x 0) (x 1)))
	      (sort (map (lambda (listpart) (<array> (car listpart)
						     (cadr listpart)
						     (analyse-ladspa (car listpart) (cadr listpart))))
			 (list-ladspa))
		    (lambda (x y)
		      (string<? (car (x 2))
				(car (y 2))))))))
				     


(install-ladspa-menues)



