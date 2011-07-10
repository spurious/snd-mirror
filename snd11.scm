;;; backwards compatibility for snd 11

;;; -------- with-mix --------
;;;
;;; weird syntax = with-mix (with-sound-args) file-name start-in-output &body body
;;;
;;; (with-sound () 
;;;   (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)
;;;                              (fm-violin 1 2 660 .1))
;;; ...)

(define (with-mix-find-file-with-extensions file extensions)
  "(with-mix-find-file-with-extensions file extensions) helps the with-mix macro find checkpoint files"
  (if (file-exists? file)
      file
      (call-with-exit
       (lambda (found-one)
	 (for-each
	  (lambda (ext)
	    (let ((new-file (string-append file "." ext)))
	      (if (file-exists? new-file)
		  (found-one new-file))))
	  extensions)
	 #f))))

(define (with-mix-file-extension file default)
  "(with-mix-file-extension file default) is a helper function for the with-mix macro"
  (let ((len (string-length file)))
    (call-with-exit
     (lambda (ok)
       (do ((i (- len 1) (- i 1)))
	   ((= i 0))
	 (if (char=? (file i) #\.)
	     (ok (substring file (+ 1 i) len))))
       default))))

(defmacro with-mix (options ur-chkpt-file ur-beg . body)
  `(let ((chkpt-file ,ur-chkpt-file)
	 (beg-1 ,ur-beg))
     (if (not (list? ',options))
	 (throw 'with-sound-interrupt (format #f "with-mix options list (arg 1) is ~A?~%;" ',options))
	 (if (not (string? chkpt-file))
	     (throw 'with-sound-interrupt (format #f "with-mix file (arg 2) is ~A?~%;" ,ur-chkpt-file))
	     (if (not (number? beg-1))
		 (throw 'with-sound-interrupt (format #f "with-mix begin time (arg 3) for ~S = ~A?~%;" chkpt-file beg-1))
		 (let ((beg (round (* (mus-srate) beg-1))))
		   (if (null? ',body)
		       (mus-mix *output* chkpt-file beg)
		       (let* ((call-str (object->string ',body))
			      (option-str (object->string ',options))
			      (sndf (with-mix-find-file-with-extensions chkpt-file (list (with-mix-file-extension *clm-file-name* "snd") "snd")))
			      (revf (and sndf *reverb* (with-mix-find-file-with-extensions chkpt-file (list "rev"))))
			      (mix-values (and sndf
					       (or (not *reverb*)
						   revf)
					       (let ((comment (mus-sound-comment sndf)))
						 (and (string? comment)
						      (catch #t
							     (lambda ()
							       (eval-string comment))
							     (lambda args #f))))))) ; any error means we lost
			 (if (and sndf
				  (or (not *reverb*)
				      revf)
				  (list? mix-values)
				  (= (length mix-values) 2)
				  (string? (car mix-values))
				  (string? (cadr mix-values))
				  (string=? (car mix-values) option-str)
				  (string=? (cadr mix-values) call-str))
			     (begin
			       (if *clm-verbose* (snd-print (format #f "mix ~S at ~,3F~%" sndf beg)))
			       (mus-mix *output* sndf beg)
			       (if revf (mus-mix *reverb* revf beg)))
			     ;; else recompute
			     (let ((old-to-snd *to-snd*))
			       (set! *to-snd* #f)
			       (if *clm-verbose* (snd-print (format #f "remake ~S at ~,3F~%" chkpt-file beg)))
			       (let ((new-sound 
				      (apply with-sound-helper 
					     (lambda () ,@body) 
					     (append (list :output 
							   (string-append chkpt-file "." (with-mix-file-extension *clm-file-name* "snd")))
						     (list :comment 
							   (format #f "(begin~%;; written ~A (Snd: ~A)~%(list ~S ~S))~%"
								   (strftime "%a %d-%b-%Y %H:%M %Z" (localtime (current-time)))
								   (snd-version)
								   option-str
								   call-str))
						     (if (and (> (channels *output*) 1)
							      (not (member :channels ',options)))
							 (list :channels (channels *output*))
							 '())
						     ',options))))
				 (set! *to-snd* old-to-snd)
				 (mus-mix *output* new-sound beg)
				 (if revf (mus-mix *reverb* revf beg)))))))))))))


#|
;;; this is the with-mix documentation:

<br>
<table border=0 bordercolor="lightgreen" width=50% cellpadding=1 cellspacing=0><tr><td bgcolor="lightgreen">
<table width=100% border=0><tr><td bgcolor="#EEFDEE" valign="middle"><h4>with-mix</h4></td></tr></table>
</td></tr></table>

<p><a name="with-mix">with-mix</a> is a "checkpointing" version of with-sound, more useful in the bad old days when computers
were incredibly slow, but lingering on...
It is a macro, callable within <a href="#wsdoc">with-sound</a> or <a href="#clmload">clm-load</a>,
which saves the computation in its body in a separate file, and
then upon a subsequent recomputation, tries to tell (via a string comparison) when that file's data is up to date
and does not need to be recomputed.
</p>

<table border=0 hspace=40 cellpadding=8 cellspacing=3><tr><td>
<pre>
(<a class=quiet href="#wsdoc" onmouseout="UnTip()" onmouseover="Tip(sndscm_wsdoc_tip)">with-sound</a> () 
  (fm-violin 0 .1 440 .1)
  (<em class=red>with-mix</em> () "sec1" .5 
    (fm-violin 0 .1 550 .1)
    (fm-violin .1 .1 660 .1))
  (<em class=red>with-mix</em> (:reverb jc-reverb) "sec2" 1.0
    (fm-violin 0 .1 880 .1 :reverb-amount .2)
    (fm-violin .1 .1 1320 .1 :reverb-amount .2))
  (fm-violin 2 .1 220 .1)
  (<em class=red>mix</em> "/zap/slow.snd"))
</pre>
</td></tr></table>

<p>Now, if we change just the first note in the with-mix call, the
second with-mix section will not be recomputed, but will be mixed in from the
saved file "sec2.snd".
In the old days, when notes took hours to compute, this was a big deal,
but not anymore.
</p>


;;; and these are the regression tests

  (define (check-with-mix num dur total-dur amp opts calls old-date chkmx)
    (let ((ind (find-sound "test.snd")))
      (if (not (sound? ind)) (snd-display #__line__ ";with-mix (~A) init: no test.snd?" num))
      (if (and chkmx (fneq (maxamp ind) amp)) (snd-display #__line__ ";with-mix (~A) maxamp: ~A (~A)" num (maxamp ind) amp))
      (if (not (file-exists? "with-mix.snd")) (snd-display #__line__ ";with-mix (~A) output doesn't exist" num))
      (let ((mx (mus-sound-maxamp "with-mix.snd"))
	    (date (mus-sound-write-date "with-mix.snd"))
	    (duration (mus-sound-duration "with-mix.snd")))
	(if (fneq duration dur) (snd-display #__line__ ";with-mix (~A) dur: ~A ~A" num dur duration))
	(if (fneq total-dur (/ (frames ind) (srate ind))) 
	    (snd-display #__line__ ";with-mix (~A) total dur: ~A ~A" num total-dur (/ (frames ind) (srate ind))))
	(if (and old-date
		 (> (- date old-date) 1)) ; these can be off by some amount in Linux
	    (snd-display #__line__ ";with-mix (~A) rewrote output?: ~A ~A ~A" num (- date old-date)
			 (strftime "%d-%b-%g %H:%M:%S" (localtime old-date))
			 (strftime "%d-%b-%g %H:%M:%S" (localtime date))))
	(if (and chkmx (or (not mx) (fneq (cadr mx) amp))) (snd-display #__line__ ";with-mix sndf (~A) maxamp: ~A (~A)" num mx amp))
	(let ((header-str (mus-sound-comment "with-mix.snd")))
	  (if (not (string? header-str)) (snd-display #__line__ ";with-mix (~A) comment unwritten?: ~A" num (mus-sound-comment "with-mix.snd")))
	  (let ((header (eval-string header-str)))
	    (if (not (list? header)) (snd-display #__line__ ";with-mix (~A) comment: ~A -> ~A" num header-str header))
	    (if (or (not (string=? (car header) opts))
		    (not (string=? (cadr header) calls)))
		(snd-display #__line__ ";with-mix (~A) header values: ~A" num header))))
	(close-sound ind)
	date)))
  
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound () (with-mix () "with-mix" 0 (fm-violin 0 .1 440 .1)))
    (let ((old-date (check-with-mix 1 .1 .1 .1 "()" "((fm-violin 0 0.1 440 0.1))" #f #t)))
      (with-sound () (with-mix () "with-mix" 0 (fm-violin 0 .1 440 .1)))
      (check-with-mix 1 .1 .1 .1 "()" "((fm-violin 0 0.1 440 0.1))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1)))
    (let ((old-date (check-with-mix 2 .1 .2 .1 "()" "((fm-violin 0 0.1 440 0.1))" #f #t)))
      (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1)))
      (check-with-mix 2 .1 .2 .1 "()" "((fm-violin 0 0.1 440 0.1))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1) (fm-violin .1 .1 660 .2)))
    (let ((old-date (check-with-mix 3 .2 .3 .2 "()" "((fm-violin 0 0.1 440 0.1) (fm-violin 0.1 0.1 660 0.2))" #f #t)))
      (with-sound () (fm-violin 0 .1 660 .1) (with-mix () "with-mix" .1 (fm-violin 0 .1 440 .1) (fm-violin .1 .1 660 .2)))
      (check-with-mix 3 .2 .3 .2 "()" "((fm-violin 0 0.1 440 0.1) (fm-violin 0.1 0.1 660 0.2))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound ()
		(with-mix () "with-mix" 0
			  (sound-let ((tmp () (fm-violin 0 1 440 .1))) (mus-mix *output* tmp 0))))
    (let ((old-date (check-with-mix 4 1 1 .1 "()" "((sound-let ((tmp () (fm-violin 0 1 440 0.1))) (mus-mix *output* tmp 0)))" #f #t)))
      (with-sound ()
		  (with-mix () "with-mix" 0
			    (sound-let ((tmp () (fm-violin 0 1 440 .1))) (mus-mix *output* tmp 0))))
      (check-with-mix 4 1 1 .1 "()" "((sound-let ((tmp () (fm-violin 0 1 440 0.1))) (mus-mix *output* tmp 0)))" old-date #t))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound (:channels 2) (fm-violin 0 .1 440 .1 :degree 0) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3 :degree 90)))
    (let ((ind (find-sound "test.snd")))
      (if (or (fneq (maxamp ind 0) .1)
	      (fneq (maxamp ind 1) .3))
	  (snd-display #__line__ ";with-mix stereo: ~A" (maxamp ind #t)))
      (if (not (= (mus-sound-chans "with-mix.snd") 2)) (snd-display #__line__ ";with-mix stereo out: ~A" (mus-sound-chans "with-mix.snd"))))
    (let ((old-date (mus-sound-write-date "with-mix.snd")))
      (with-sound (:channels 2) (fm-violin 0 .1 440 .1 :degree 0) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3 :degree 90)))
      (if (not (= (mus-sound-write-date "with-mix.snd") old-date)) 
	  (snd-display #__line__ ";stereo with-mix dates: ~A ~A" old-date (mus-sound-write-date "with-mix.snd"))))
    (let ((ind (find-sound "test.snd")))
      (close-sound ind))
    
    (if (file-exists? "with-mix.snd") (delete-file "with-mix.snd"))
    (with-sound (:reverb jc-reverb) (fm-violin 0 .1 440 .1) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3)))
    (let ((old-date (check-with-mix 6 .1 1.1 .398 "()" "((fm-violin 0 0.1 550 0.3))" #f #f)))
      (with-sound (:reverb jc-reverb) (fm-violin 0 .1 440 .1) (with-mix () "with-mix" 0 (fm-violin 0 .1 550 .3)))
      (check-with-mix 6 .1 1.1 .398 "()" "((fm-violin 0 0.1 550 0.3))" old-date #f))

|#

;;; --------------------------------------------------------------------------------


(define (focus-follows-mouse) (set! (with-pointer-focus) #t))
(define (make-current-window-display) (set! (with-inset-graph) #t))
(define load-from-path load)
;(define def-optkey-fun define*)
;(define def-optkey-instrument definstrument)

(define spectro-cutoff spectrum-start)
(define spectro-end spectrum-end)



(define* (play-region reg wait stop-func)
  (play (if (integer? reg) (integer->region reg) reg) :wait wait :stop stop-func))

(define* (play-selection wait stop-func)
  (play (selection) :wait wait :stop stop-func))

(define* (play-mix id (beg 0))
  (play (if (integer? id) (integer->mix id) id) beg))

(define* (play-and-wait (start 0) snd chn syncd end (pos -1) stop-proc)
  (if (string? start)
      (play start (or snd 0) :end (or chn -1) :wait #t) 
      (play (if (integer? snd) (integer->sound snd)
		(if (sound? snd) snd
		    (or (selected-sound) (car (sounds)))))
	    :channel (or chn -1) :wait #t :with-sync syncd :start start :end (or end -1) 
	    :stop stop-proc :edit-position pos)))

(define* (old-play (start 0) snd chn syncd end (pos -1) stop-proc (out-chan -1))
  (play (if (integer? snd) (integer->sound snd)
	    (if (sound? snd) snd
		(or (selected-sound) (car (sounds)))))
	:channel (or chn -1) :with-sync syncd :start start :end (or end -1) 
	:stop stop-proc :out-channel out-chan :edit-position pos))

(define* (play-channel (beg 0) dur snd chn (pos -1) stop-proc (out-chan -1))
  (play (if (integer? snd) (integer->sound snd)
	    (if (sound? snd) snd
		(or (selected-sound) (car (sounds)))))
	:channel (or chn -1) :with-sync #f :start beg :end (if dur (+ beg dur) -1) 
	:stop stop-proc :out-channel out-chan :edit-position pos))


(define quit-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define reset-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define help-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define do-it-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define do-it-again-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))
(define pushed-button-button-color (make-procedure-with-setter (lambda () 'obsolete) (lambda (val) 'obsolete)))


;;; --------------------------------------------------------------------------------

(define (add-watcher func)
  (hook-push effects-hook func))

(define (delete-watcher func)
  (hook-remove effects-hook func))


#|
;;; -------- reopen menu

(define including-reopen-menu #f)

(define (with-reopen-menu)
  (if (not including-reopen-menu)
      (let ((reopen-menu (add-to-main-menu "Reopen"))
	    (reopen-names '()))

	(define (add-to-reopen-menu snd)
	  (let ((brief-name (short-file-name snd))
		(long-name (file-name snd))
		(reopen-max-length 16)) ; sets max length of menu
	    (if (not (member brief-name reopen-names))
		(begin
		  (add-to-menu reopen-menu 
			       brief-name
			       (lambda () 
				 (remove-from-menu reopen-menu brief-name)
				 (open-sound long-name))
			       0) ; add to top
		  (set! reopen-names (append reopen-names (list brief-name)))
		  (if (> (length reopen-names) reopen-max-length)
		      (let ((goner (car reopen-names)))
			(set! reopen-names (cdr reopen-names))
			(remove-from-menu reopen-menu goner)))))))
	
	(define (check-reopen-menu filename)
	  (define (just-filename name)
	    (let ((last-slash -1)
		  (len (string-length name)))
	      (do ((i 0 (+ 1 i)))
		  ((= i len) (substring name (+ 1 last-slash)))
		(if (char=? (name i) #\/)
		    (set! last-slash i)))))
	  (let ((brief-name (just-filename filename)))
	    (if (member brief-name reopen-names)
		(set! reopen-names (remove-if (lambda (n) 
						(let ((val (string=? n brief-name)))
						  (if val (remove-from-menu reopen-menu brief-name))
						  val))
					      reopen-names))))
	  #f)
	
	(set! including-reopen-menu #t)
	(hook-push close-hook add-to-reopen-menu)
	(hook-push open-hook check-reopen-menu))))




;;; -------- hidden controls panel --------

(define hidden-controls-dialog #f)
(define hidden-controls '())

(define hidden-controls-help 
"make-hidden-controls-dialog adds an item \"Hidden controls\" to the Option
menu that creates a dialog that controls all the otherwise hidden control-panel variables.
Expand-hop sets the time in seconds between successive grains.
Expand-length sets the length of each grain.
Expand-ramp sets the ramp-time in the grain envelope.
Expand-jitter sets the grain timing jitter.
Contrast-amp sets the prescaler for contrast-enhancement.
Reverb-lowpass sets the feedback lowpass filter coeficient.
Reverb-feedback sets the scaler on the feedback.
")

(define (make-hidden-controls-dialog)
  (if (provided? 'snd-motif)
      (begin
	(define (reset-all-sliders)
	  (for-each
	   (lambda (ctl)
	     (set! ((caddr ctl) #t) (cadr ctl))
	     (XtSetValues (car ctl) 
			  (list XmNvalue (floor (* (cadr ctl) 100)))))
	   hidden-controls))
	
	(if (not (Widget? hidden-controls-dialog))
	    (let ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
		  (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
		  (titlestr (XmStringCreate "More Controls" XmFONTLIST_DEFAULT_TAG))
		  (xreset (XmStringCreate "Reset" XmFONTLIST_DEFAULT_TAG)))
	      (set! hidden-controls-dialog 
		    (XmCreateTemplateDialog (cadr (main-widgets)) "More Controls"
					    (list XmNcancelLabelString   xreset
						  XmNokLabelString       xdismiss
						  XmNhelpLabelString     xhelp
						  XmNautoUnmanage        #f
						  XmNdialogTitle         titlestr
						  XmNresizePolicy        XmRESIZE_GROW
						  XmNnoResize            #f
						  XmNtransient           #f
						  XmNbackground          (basic-color))))
	      
	      (for-each
	       (lambda (button color)
		 (XtVaSetValues (XmMessageBoxGetChild hidden-controls-dialog button)
				(list XmNarmColor   (selection-color)
				      XmNbackground color)))
	       (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
	       (list (highlight-color) (highlight-color) (highlight-color)))
	      
	      (XtAddCallback hidden-controls-dialog 
			     XmNokCallback (lambda (w context info)
					     (XtUnmanageChild hidden-controls-dialog)))
	      (XtAddCallback hidden-controls-dialog 
			     XmNhelpCallback (lambda (w context info)
					       (help-dialog "More Controls" hidden-controls-help)))
	      (XtAddCallback hidden-controls-dialog
			     XmNcancelCallback (lambda (w context info)
						 (reset-all-sliders)))
	      (XmStringFree xhelp)
	      (XmStringFree xdismiss)
	      (XmStringFree titlestr)
	      (XmStringFree xreset)
	      
	      (let* ((mainform 
		      (XtCreateManagedWidget "formd" xmRowColumnWidgetClass hidden-controls-dialog
					     (list XmNleftAttachment      XmATTACH_FORM
						   XmNrightAttachment     XmATTACH_FORM
						   XmNtopAttachment       XmATTACH_FORM
						   XmNbottomAttachment    XmATTACH_WIDGET
						   XmNbottomWidget        (XmMessageBoxGetChild hidden-controls-dialog XmDIALOG_SEPARATOR)
						   XmNorientation         XmVERTICAL))))
		(for-each
		 (lambda (lst)
		   (let* ((name (car lst))
			  (low (cadr lst))
			  (high (caddr lst))
			  (initial (list-ref lst 3))
			  (func (list-ref lst 4))
			  (title (XmStringCreate name XmFONTLIST_DEFAULT_TAG))
			  (slider (XtCreateManagedWidget name xmScaleWidgetClass mainform
							 (list XmNorientation   XmHORIZONTAL
							       XmNshowValue     #t
							       XmNminimum       (floor (* low 1000))
							       XmNmaximum       (floor (* high 1000))
							       XmNvalue         (floor (* initial 1000))
							       XmNdecimalPoints 3
							       XmNtitleString   title
							       XmNborderWidth   1
							       XmNbackground    (basic-color)))))
		     (XmStringFree title)
		     (set! hidden-controls (cons (list slider initial func) hidden-controls))
		     (XtAddCallback slider
				    XmNvalueChangedCallback 
				    (lambda (w context info)
				      (set! (func) (/ (.value info) 1000.0))))
		     (XtAddCallback slider
				    XmNdragCallback 
				    (lambda (w context info)
				      (set! (func) (/ (.value info) 1000.0))))))
		 (list (list "expand-hop" 0.001 0.3 0.05  expand-control-hop)
		       (list "expand-length" 0.01 .5 0.15 expand-control-length)
		       (list "expand-ramp" 0.01 .5 0.4 expand-control-ramp)
		       (list "expand-jitter" 0.0 2.0 1.0 expand-control-jitter)
		       (list "contrast-amp" 0.0 2.0 1.0 contrast-control-amp)
		       (list "reverb-lowpass" 0.0 1.0 0.7 reverb-control-lowpass)
		       (list "reverb-feedback" 0.0 1.25 1.09 reverb-control-feedback))))
	      (add-to-menu 3 "Hidden controls"
			   (lambda ()
			     (if (not (XtIsManaged hidden-controls-dialog))
				 (XtManageChild hidden-controls-dialog)
				 (raise-dialog hidden-controls-dialog))))))
	) ; motif case
      (begin
	
	(define (reset-all-sliders)
	  (for-each
	   (lambda (ctl)
	     (set! ((caddr ctl)) (cadr ctl))
	     (gtk_adjustment_set_value (GTK_ADJUSTMENT (car ctl)) (cadr ctl))
       ;;; (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car ctl)))
	     )
	   hidden-controls))
	
	(if (not hidden-controls-dialog)
	    (let ((dismiss-button (gtk_button_new_with_label "Go Away"))
		  (help-button (gtk_button_new_with_label "Help"))
		  (reset-button (gtk_button_new_with_label "Reset")))
	      (gtk_widget_set_name dismiss-button "quit_button")
	      (gtk_widget_set_name help-button "help_button")
	      (gtk_widget_set_name reset-button "reset_button")
	      (set! hidden-controls-dialog (gtk_dialog_new))
	      (gtk_window_set_title (GTK_WINDOW hidden-controls-dialog) "More Controls")
	      (gtk_container_set_border_width (GTK_CONTAINER hidden-controls-dialog) 10)
	      (gtk_window_set_default_size (GTK_WINDOW hidden-controls-dialog) -1 -1)
	      (gtk_window_set_resizable (GTK_WINDOW hidden-controls-dialog) #t)
	      (gtk_widget_realize hidden-controls-dialog)
	      (g_signal_connect hidden-controls-dialog "delete_event" (lambda (w ev data) (gtk_widget_hide hidden-controls-dialog) #t) #f)
	      
	      (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG hidden-controls-dialog))) dismiss-button #t #t 20)
	      (g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide hidden-controls-dialog)) #f)
	      (gtk_widget_show dismiss-button)
	      (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG hidden-controls-dialog))) reset-button #t #t 20)
	      (g_signal_connect reset-button "clicked" (lambda (w data) (reset-all-sliders)) #f)
	      (gtk_widget_show reset-button)
	      (gtk_box_pack_end (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG hidden-controls-dialog))) help-button #t #t 20)
	      (g_signal_connect help-button "clicked" (lambda (w data) (help-dialog "More Controls" hidden-controls-help)) #f)
	      (gtk_widget_show help-button)
	      
	      (let ((mainform (gtk_dialog_get_content_area (GTK_DIALOG hidden-controls-dialog))))
		(for-each
		 (lambda (lst)
		   (let* ((title (car lst))
			  (low (cadr lst))
			  (high (caddr lst))
			  (initial (list-ref lst 3))
			  (func (list-ref lst 4))
			  (adj (gtk_adjustment_new initial low high .001 .001 .1))
			  (slider (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT adj)))
			  (label (gtk_label_new title)))
		     (if (not (provided? 'gtk3)) (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE slider)) GTK_UPDATE_CONTINUOUS))
		     (gtk_scale_set_digits (GTK_SCALE slider) 3)
		     (gtk_scale_set_value_pos (GTK_SCALE slider) GTK_POS_TOP)
		     (gtk_scale_set_draw_value (GTK_SCALE slider) #t)
		     (gtk_box_pack_start (GTK_BOX mainform) slider #t #t 4)
		     (gtk_box_pack_start (GTK_BOX mainform) label #t #t 4)
		     (set! hidden-controls (cons (list adj initial func) hidden-controls))	       
		     (g_signal_connect adj "value_changed" (lambda (adj data) (set! (func) (gtk_adjustment_get_value (GTK_ADJUSTMENT adj)))) #f)
		     (gtk_widget_show slider)
		     (gtk_widget_show label)))
		 
		 (list (list "expand-hop" 0.001 0.3 0.05  expand-control-hop)
		       (list "expand-length" 0.01 .5 0.15 expand-control-length)
		       (list "expand-ramp" 0.01 .5 0.4 expand-control-ramp)
		       (list "expand-jitter" 0.0 2.0 1.0 expand-control-jitter)
		       (list "contrast-amp" 0.0 2.0 1.0 contrast-control-amp)
		       (list "reverb-lowpass" 0.0 1.0 0.7 reverb-control-lowpass)
		       (list "reverb-feedback" 0.0 1.25 1.09 reverb-control-feedback))))
	      
	      (add-to-menu 3 "Hidden controls" 
			   (lambda () 
			     (gtk_widget_show hidden-controls-dialog)))
	      )))
      ))
|#

;;; --------------------------------------------------------------------------------

(define clear-selection unselect-all)



#|
;;; --------------------------------------------------------------------------------
;;; this is now built-in

(define remembering-sound-state 0) ; for prefs
(define remember-sound-filename ".snd-remember-sound") ; should this be in the home directory?

(define* (remember-sound-state (choice 3))
  "(remember-sound-state) remembers the state of a sound when it is closed, and if it is subsquently re-opened, restores that state"

  (let ((states '())
	(sound-funcs (list sync with-tracking-cursor selected-channel show-controls read-only
			   contrast-control? expand-control? reverb-control? filter-control?
			   amp-control amp-control-bounds
			   contrast-control contrast-control-amp contrast-control-bounds
			   expand-control expand-control-bounds expand-control-hop expand-control-jitter expand-control-length expand-control-ramp 
			   filter-control-envelope filter-control-in-dB filter-control-in-hz filter-control-order 
			   reverb-control-decay reverb-control-feedback reverb-control-length reverb-control-length-bounds
			   reverb-control-lowpass reverb-control-scale reverb-control-scale-bounds
			   speed-control speed-control-bounds speed-control-style speed-control-tones))

	(channel-funcs (list time-graph? transform-graph? lisp-graph? x-bounds y-bounds cursor cursor-size
			     cursor-style show-marks show-y-zero show-grid wavo-hop wavo-trace max-transform-peaks
			     show-transform-peaks fft-log-frequency fft-log-magnitude with-verbose-cursor zero-pad
			     wavelet-type min-dB transform-size transform-graph-type time-graph-type fft-window
			     transform-type transform-normalization time-graph-style show-mix-waveforms dot-size
			     x-axis-style show-axes graphs-horizontal lisp-graph-style transform-graph-style
			     grid-density tracking-cursor-style
			     )))

    (define (print-readably fd field depth first)
      (if (not first) (format fd " "))
      (if (string? field)
	  (if (= (string-length (format #f "~S" "1")) 3)
	      (format fd "~S" field)
	      (format fd "\"~S\"" field)) ; sometimes format omits the double quotes!
	  (if (number? field)
	      (if (rational? field) ; get these out of our way before float stuff
		  (format fd "~A" field)
		  (format fd "~,4F" field))
	      (if (procedure? field)
		  (format fd "~A" (procedure-source field))
		  (if (list? field)
		      (begin
			(if (or (= depth 1)
				(> (length field) 12))
			    (begin
			      (format fd "~%")
			      (do ((i 0 (+ 1 i)))
				  ((= i depth))
				(format fd "  "))))
			(format fd "(")
			(let ((fst #t))
			  (for-each 
			   (lambda (val)
			     (print-readably fd val (+ 1 depth) fst)
			     (set! fst #f))
			   field))
			(format fd ")"))
		      (format fd "~A" field))))))

    (define (find-if pred l)
      "(find-if func lst) scans 'lst' for any element that 'func' likes"
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l)))))

    (define saved-state
      (make-procedure-with-setter
       (lambda (snd)
	 (find-if (lambda (n) 
		    (string=? (car n) (file-name snd))) 
		  states))
       (lambda (snd new-state)
	 (set! states (cons new-state
			    (remove-if
			     (lambda (n)
			       (string=? (car n) (file-name snd)))
			     states))))))

    (define (remember-sound-at-close snd)
      ;; save current state in list (name write-date (snd props) (chan props))
      (set! (saved-state snd)
	    (list (file-name snd)
		  (file-write-date (file-name snd))
		  (map (lambda (f) 
			 (f snd)) 
		       sound-funcs)
		  (map (lambda (sc)
			 (map (lambda (f)
				(if (equal? f transform-type) 
				    (transform->integer (f (car sc) (cadr sc))) 
				    (f (car sc) (cadr sc))))
			      channel-funcs))
		       (let ((scs '()))
			 (do ((i 0 (+ 1 i)))
			     ((= i (channels snd)))
			   (set! scs (cons (list snd i) scs)))
			 (reverse scs)))))
      #f)

    (define (remember-sound-at-open snd)
      ;; restore previous state, if any
      (let ((state (saved-state snd))) ; removes old state from current list
	(if (and state
		 (= (file-write-date (file-name snd)) (cadr state))
		 (= (channels snd) (length (cadddr state)))
		 (not (= choice 2)))
	    ;; we need the chans check because auto-test files seem to have confused write dates
	    (begin
	      (for-each (lambda (f val)
			  (set! (f snd) val))
			sound-funcs
			(caddr state))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn (channels snd)))
		(dynamic-wind
		    (lambda () (set! (squelch-update snd chn) #t))
		    (lambda ()
		      (for-each (lambda (f val)
				  (if (and (list? val)
					   (not (null? val))
					   (eq? (car val) 'lambda))
				      (set! (f snd chn) (eval val (current-environment)))
				      (if (equal? f transform-type) 
					  (set! (f snd chn) (integer->transform val)) 
					  (set! (f snd chn) val))))
				channel-funcs
				(list-ref (cadddr state) chn)))
		    (lambda () (set! (squelch-update snd chn) #f)))
		(if (time-graph? snd chn) (update-time-graph snd chn))
		(if (transform-graph? snd chn) (update-transform-graph snd chn)))))))

    (define (remember-sound-at-start filename)
      (if (and (null? states)
	       (file-exists? remember-sound-filename))
	  (begin
	    (load remember-sound-filename)
	    (set! states -saved-remember-sound-states-states-)))
      #f)

    (define (remember-sound-at-exit)
      (if (not (null? states))
	  (call-with-output-file remember-sound-filename
	    (lambda (fd)
	      (format fd "~%~%;;; from remember-sound-state in extensions.scm~%")
	      (format fd "(define -saved-remember-sound-states-states-~%  '")
	      (print-readably fd states 0 #t)
	      (format fd ")~%"))))
      #f)
      
    (if (or (= choice 0)  ; no remembering
	    (= choice 1)) ; just within-run remembering
	(begin
	  (if (= choice 0)
	      (begin
		(hook-remove close-hook remember-sound-at-close)
		(hook-remove after-open-hook remember-sound-at-open)))
	  (hook-remove open-hook remember-sound-at-start)
	  (hook-remove before-exit-hook remember-sound-at-exit)
	  (if (file-exists? remember-sound-filename)
	      (delete-file remember-sound-filename))))

    (if (not (= choice 0))
	(begin
	  (hook-push close-hook remember-sound-at-close)
	  (hook-push after-open-hook remember-sound-at-open)
	  (if (not (= choice 1))
	      (begin
		(hook-push open-hook remember-sound-at-start)
		(hook-push before-exit-hook remember-sound-at-exit)))))

    (set! remembering-sound-state choice)
    'remembering!))
|#


(define cursor-follows-play (make-procedure-with-setter
			     (lambda (snd)
			       (with-tracking-cursor))
			     (lambda (snd val)
			       (set! (with-tracking-cursor) val))))
