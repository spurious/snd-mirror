
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



(if (not (provided? 'snd-gui.scm))
    (load-from-path "gui.scm"))





;; Organize help texts.
(c-load-from-path ladspa-help)

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
       (c-integer (/ sndchannels pluginchannels))))



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


  ;;(define-method* (apply! ...)
  (define-method* (apply! #:optional parset-func)
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
	   (chans (if (= 0 (sync snd))
		      1
		      (channels snd)))
	   (startchan (if (= 0 (sync snd))
			  (selected-channel snd)
			  (get-startchan snd 0)))
	   (tempfilename #f)
	   (new-file #f)
	   (buflen (if parset-func 32 ladspa-maxbuf))
	   (vct-out (make-vct buflen))
	   (sdobj (make-sound-data chans buflen))
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
	    
	    ;; Set up sample readers for each channel.
	    (c-for 0 < chans 1
		   (lambda (ch)
		     (set! readers (cons (make-sample-reader start
							     snd
							     (+ startchan ch))
					 readers))))
	    (set! readers (reverse readers))


	    ;; Create a temporary file.
	    (set! tempfilename (snd-tempnam))
	    (set! new-file (open-sound-file tempfilename chans :srate (srate snd)))


	    ;; Start the hour-glass
	    (start-progress-report)


	    ;; Do the ladspa.
	    (c-for 0 < (+ buflen length) buflen
		   (lambda (n)
		     (if (< n length)
			 (let ((len (min (- length n)
					 buflen)))
			   
			   ;;(display "N: ")(display n)
			   ;;(display " len: ")(display len)
			   ;;(display " length: ")(display length)(newline)

			   (if (or isbreaked (c-g?))
			       (if (not isbreaked) (set! isbreaked #t))
			       (begin
			       
				 ;; Update hour-glass
				 (progress-report (/ n length) "doing the ladspa" chans chans snd)
				 
				 ;; The length of sdobj must be the length of the data. Can only happen at last iteration.
				 (if (< len buflen)
				     (begin
				       (set! sdobj (make-sound-data chans len))
				       (set! vct-out (make-vct len))))

				 ;; Reading data into soundobject from soundfile.
				 (if (not no_audio_inputs)
				     (c-for-each (lambda (ch reader)
						   (vct-map! vct-out
							     (lambda () (next-sample reader)))
						   (vct->sound-data vct-out sdobj ch))
						 readers))

				 ;; Automation
				 (if parset-func
				     (parset-func snd (+ start n)))

				 ;; Process soundobject
				 (apply-soundobject sdobj)
				 
				 ;; Writing data from soundobject into temporary file.
				 (mus-sound-write new-file
						  0
						  (1- len)
						  chans
						  sdobj)))))))
	   

	    ;; Close temporary file.
	    (close-sound-file new-file (* chans (* 4 length)))


	    ;; Let snd know about the new file.
	    (if (not isbreaked)
		(c-for 0 < chans 1
		       (lambda (ch)
			 (set-samples start length tempfilename snd (+ ch startchan) #f (string-append "ladspa: " libname "/" plugname) ch))))
	    

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


  (define-method (get-hint portnum)
    (car (list-ref (.PortRangeHints this->descriptor) portnum)))

  (define (ishint portnum dashint)
    (not (= (logand (this->get-hint portnum) dashint) 0)))

  (define-method (get-lo portnum)
    (* (if (ishint portnum LADSPA_HINT_SAMPLE_RATE)
	   (srate)
	   1)
       (if (not (ishint portnum LADSPA_HINT_BOUNDED_BELOW))
	   0                                   ;The value Ardour use.
	   (cadr (list-ref (.PortRangeHints this->descriptor) portnum)))))

  (define-method (get-hi portnum)
    (* (if (ishint portnum LADSPA_HINT_SAMPLE_RATE)
	   (srate)
	   1)
       (if (not (ishint portnum LADSPA_HINT_BOUNDED_ABOVE))
	   4                                   ;The value Ardour use.
	   (caddr (list-ref (.PortRangeHints this->descriptor) portnum)))))


  ;; Constructor:
  (if (not this->descriptor)
      (set! this #f)
      (begin
	(c-for-each (lambda (n x)
		      (if (> (logand x LADSPA_PORT_CONTROL) 0)
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! this->input-controls (append this->input-controls (list n)))
			      (set! this->output-controls (append this->output-controls (list n))))
			  (if (> (logand x LADSPA_PORT_INPUT) 0)
			      (set! input-audios (append input-audios (list n)))
			      (set! output-audios (append output-audios (list n))))))
		    (.PortDescriptors this->descriptor))
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

  (define das-ladspa-list #f)

  (define (get-ladspa-list)
    (if (not das-ladspa-list)
	(set! das-ladspa-list (sort (map (lambda (listpart) (list (ladspa-descriptor (car listpart) (cadr listpart))
								  (car listpart)
								  (cadr listpart)))
					 (list-ladspa))
				    (lambda (x y)
				      (string-ci<? (.Name (car x))
						   (.Name (car y)))))))
    das-ladspa-list)
  
  (define (get-ladspa-with-id id)
    (if (null? (get-ladspa-list))
	#f
	(if (= (.UniqueID (caar das-ladspa-list)) id)
	    (let ((ret (car das-ladspa-list)))
	      (set! das-ladspa-list (cdr das-ladspa-list))
	      ret)
	    (let loop ((part das-ladspa-list))
	      (if (or (null? part)
		      (null? (cdr part)))
		  #f
		  (if (= (.UniqueID (caadr part)) id)
		      (let ((ret (cadr part)))
			(set-cdr! part (cddr part))
			ret)
		      (loop (cdr part))))))))


  (define ladspa-effects-menu (add-to-main-menu "Ladspa" (lambda x #f)))

  (define ladspa-add-effect-menuitem
    (let* ((num-effects-per-submenu 12)
	   (ladspa-effect-num num-effects-per-submenu)
	   (uncat-submenu #f)
	   (curr-submenu #f))
      (lambda* (name proc #:optional menu)
	       (if menu
		   (menu-add menu name proc)
		   (begin
		     (if (not uncat-submenu)
			 (set! uncat-submenu 
			       (if (provided? 'snd-lrdf)
				   (menu-sub-add ladspa-effects-menu "Uncategorised")
				   ladspa-effects-menu)))
		     (if (= num-effects-per-submenu ladspa-effect-num)
			 (begin
			   (set! curr-submenu (menu-sub-add uncat-submenu (string-append (substring name 0 (min (string-length name) 20)) " ... ")))
			   (set! ladspa-effect-num -1)))
		     (menu-add curr-submenu name proc)
		     (set! ladspa-effect-num (+ 1 ladspa-effect-num)))))))
  
    
  (define* (make-ladspadialog descriptor libraryname effectname #:optional menu)
    (define ladspa #f)
    (define dialog #f)
    
    (let ((name (.Name descriptor))
	  (author (.Maker descriptor))
	  (lisense (.Copyright descriptor))
	  (isplaying #f)
	  (onoffbutton #f)
	  (islooping #f)
	  (isplayingselection #f)
	  (nodelines (make-hash-table 4))
	  (play-hooks (make-hash-table 4))
	  (node-graphs (make-hash-table 4))
	  (open-portnums '()))
      
      (define (ShowDialog)
	(MakeDialogIfNotMade)
	(-> dialog show)
	(-> onoffbutton set #t)
	(enableplugin)
	(for-each (lambda (portnum)
		    (automation-onoff #t portnum
				      (-> ladspa get-lo portnum)
				      (-> ladspa get-hi portnum)
				      (list-ref (.PortNames descriptor) portnum)))
		  open-portnums)
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
	(-> ladspa apply!
	    (lambda (snd pos)
	      (hash-fold (lambda (portnum nodeline s)
			   (let ((-> ladspa get-lo portnum)
				 (-> ladspa get-hi portnum))
			     (-> ladspa input-control-set!
				 portnum
				 (c-scale (-> nodeline get-val (c-scale pos 0 (frames snd 0) 0 1))
					  0 1
					  hi lo))))
			 '() nodelines))))
      
      (define (Cancel)
	(set! open-portnums (hash-fold (lambda (portnum nodeline s) (nodeline-off portnum) (cons portnum s))
				       '() nodelines))
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
					       (if (= x 0)
						   (das-play)))))))
	  (das-play)))
      
      (define (MyStop)
	;;(remove-hook! stop-playing-selection-hook play-selection)
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
      
      (define (nodeline-off portnum)
	(remove-hook! play-hook (hash-ref play-hooks portnum))
	(hash-set! node-graphs portnum (-> (hash-ref nodelines portnum) get-graph))
	(hash-remove! play-hooks portnum)
	(-> (hash-ref nodelines portnum) paint)
	(-> (hash-ref nodelines portnum) delete!)
	(hash-remove! nodelines portnum))
      
      (define (automation-onoff onoroff portnum lo hi name)
	(if onoroff
	    (let* ((nodeline (<editor-nodeline> (selected-sound) 0
						(c-scale (-> ladspa get-input-control portnum) hi lo 0 1)
						(lambda (val)
						  (format #f "~1,3f(~A)" (c-scale val 0 1 hi lo) name))
						#f
						(list-ref (list cursor-context selection-context) (random 2))))
		   (das-play-hook (let ((lastcursor (- (dac-size))))
				    (lambda (samples)
				      (let* ((snd (selected-sound))
					     (ch 0)
					     (newcursor (cursor snd ch)))
					(if (or (= newcursor (- lastcursor (dac-size)) )
						(= newcursor lastcursor))
					    (set! newcursor (min (frames snd ch) (+ (dac-size) lastcursor))))
					(set! lastcursor newcursor)
					(-> ladspa input-control-set!
					    portnum
					    (c-scale (-> nodeline get-val (c-scale newcursor 0 (frames snd ch) 0 1))
						     0 1
						     hi lo)))))))
	      (if (hash-ref node-graphs portnum)
		  (-> nodeline set-graph!
			(hash-ref node-graphs portnum)))
	      (hash-set! nodelines portnum nodeline)
	      (add-hook! play-hook das-play-hook)
	      (hash-set! play-hooks portnum das-play-hook))
	    (nodeline-off portnum)))
      
      (define (slider-moved val portnum lo hi name)
	(if (not (hash-ref nodelines portnum))
	      (-> ladspa input-control-set! portnum val)))
      
      (define (ishint dashint dashint2)
	(not (= (logand dashint dashint2 ) 0)))

      (define (MakeDialogIfNotMade)
	(if (not dialog)
	    (begin
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
				 (let* ((lo (-> ladspa get-lo portnum))
					(init (-> ladspa get-input-control portnum))
					(hi (-> ladspa get-hi portnum))
					(hint (-> ladspa get-hint portnum))
					(scale (if (ishint hint LADSPA_HINT_INTEGER)
						   1
						   (if use-gtk 1000.0 100.0)))
					(name (list-ref (.PortNames descriptor) portnum)))
				   (list name
					 lo
					 init
					 hi
					 (lambda (val) (slider-moved val portnum lo hi name))
					 scale
					 (lambda (onoff) (automation-onoff onoff portnum lo hi name)))))
			       (remove (lambda (portnum) (ishint (car (list-ref (.PortRangeHints descriptor) portnum))  LADSPA_HINT_TOGGLED))
				       (-> ladspa input-controls)))
			  ))
	      
	      
	      ;; Add toggle buttons.
	      (for-each (lambda (portnum)
			  (let* ((hint (-> ladspa get-hint portnum))
				 (hi (-> ladspa get-hi portnum))
				 (lo (-> ladspa get-lo portnum))
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
	  (ladspa-add-effect-menuitem name ShowDialog menu)
	  #t)))

  ;; Following function made while looking at the source for jack-rack written by Bob Ham.
  (define (menu-descend menu uri base)
    (let ((uris (lrdf-get-subclasses uri)))
      (if uris
	  (let ((n 0))
	    (while (< n (lrdf-uris-count uris))
		   (let* ((item (lrdf-uris-get-item uris n))
			  (label (lrdf-get-label item))
			  (newmenu (menu-sub-add menu label)))
		     (menu-descend newmenu item (string-append base "/" label))
		     (set! n (1+ n))))
	    (lrdf-free-uris uris))))
    (let ((uris (lrdf-get-instances uri)))
      (if uris
	  (let ((n 0))
	    (while (< n (lrdf-uris-count uris))
		   (let* ((item (lrdf-uris-get-item uris n))
			  (das-ladspa (get-ladspa-with-id (lrdf-get-uid item))))
		     (if das-ladspa
			 (apply make-ladspadialog (append das-ladspa (list menu))))
		     (set! n (1+ n))))
	    (lrdf-free-uris uris)))))
  
  (if (provided? 'snd-lrdf)
      (menu-descend ladspa-effects-menu (string-append (LADSPA-BASE) "Plugin") ""))
  
  (for-each (lambda (x)
	      (apply make-ladspadialog x))
	    (get-ladspa-list)))




;; Remove the following line to take away lrdf support.
(provide 'snd-lrdf)


(define lrdf-is-inited #f)

(if (and (provided? 'snd-lrdf)
	 (not lrdf-is-inited))
    (begin
      (c-eval-c2 "-llrdf"
		 "#include <lrdf.h>"

		 '((lrdf-get-subclasses (uri))
		   "  lrdf_uris *ret=lrdf_get_subclasses(GET_STRING(uri));"
		   "  if(ret)"
		   "    return MAKE_POINTER(ret);"
		   "  else"
		   "    return SCM_BOOL_F;")

		 '((lrdf-get-label (uri))
		   "  return MAKE_STRING(lrdf_get_label(GET_STRING(uri)));")
		 
		 '((lrdf-uris-count (uris))
		   "  return MAKE_INTEGER(((lrdf_uris*)GET_POINTER(uris))->count);")
		 
		 '((lrdf-free-uris (uris))
		   "  lrdf_free_uris((lrdf_uris*)GET_POINTER(uris));return SCM_UNSPECIFIED;")

		 '((lrdf-get-instances (uri))
		   "  lrdf_uris *ret=lrdf_get_instances(GET_STRING(uri));"
		   "  if(ret)"
		   "    return MAKE_POINTER(ret);"
		   "  else"
		   "    return SCM_BOOL_F;")

		 '((lrdf-get-uid (item))
		   "  return MAKE_INTEGER(lrdf_get_uid(GET_STRING(item)));")

		 '((lrdf-uris-get-item (uri n))
		   "  return MAKE_STRING((((lrdf_uris*)GET_POINTER(uri))->items[GET_INTEGER(n)]));")

		 '((lrdf-init ())
		   "  lrdf_init();return SCM_UNSPECIFIED;")

		 '((lrdf-read-file (uri))
		   "  return MAKE_INTEGER(lrdf_read_file(GET_STRING(uri)));")

		 '((LADSPA-BASE () )
		   "  return MAKE_STRING(LADSPA_BASE);"))

      (lrdf-init)
      (for-each (lambda (path)
		  (catch #t
			 (lambda ()
			   (let* ((dir (opendir path))
				  (entry (readdir dir)))
			     (while (not (eof-object? entry))
				    (lrdf-read-file (string-append "file://" path "/" entry))
				    (set! entry (readdir dir)))
			     (closedir dir)))
			 (lambda (key . args)
			   #f)))
		(string-split	(if (getenv "LADSPA_RDF_PATH")
				    (getenv "LADSPA_RDF_PATH")
				    "/usr/local/share/ladspa/rdf:/usr/share/ladspa/rdf")
				#\:))

      (set! lrdf-is-inited #t)))



(install-ladspa-menues)


