;;; translation of new-effects.scm to gtk/xg

(use-modules (ice-9 format) (ice-9 common-list))
(provide 'snd-gtk-effects.scm)
 
(if (not (provided? 'xg))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "gtk-effects.scm needs the xg module: ~A" hxm))
	  (dlinit hxm "init_xm"))))

(define pi 3.141592653589793)

(if (not (provided? 'snd-xm-enved.scm)) (load-from-path "xm-enved.scm"))
(if (not (provided? 'snd-moog.scm)) (load-from-path "moog.scm"))
(if (not (provided? 'snd-rubber.scm)) (load-from-path "rubber.scm"))
(if (not (provided? 'snd-dsp.scm)) (load-from-path "dsp.scm"))

(if (not (defined? 'g_signal_connect))
    (define* (g_signal_connect obj name func #:optional data)
      (g_signal_connect_closure_by_id (GPOINTER obj)
				      (g_signal_lookup name (G_OBJECT_TYPE (GTK_OBJECT obj)))
				      0
				      (g_cclosure_new func data #f)
				      #f)))
;;; SOMEDAY: finish the g_signal_closure cleanup

(define effects-list '()) ; menu labels are updated to show current settings

(define (update-label effects)
  (if (not (null? effects))
      (begin
	((car effects))
	(update-label (cdr effects)))))

(define effects-menu (add-to-main-menu "Effects" (lambda () (update-label effects-list))))

(define (all-chans)
  (let ((sndlist '())
	(chnlist '()))
    (for-each (lambda (snd)
		(do ((i (1- (channels snd)) (1- i)))
		    ((< i 0))
		  (set! sndlist (cons snd sndlist))
		  (set! chnlist (cons i chnlist))))
	      (sounds))
    (list sndlist chnlist)))

(define (plausible-mark-samples)
  ;; find two marks in the current channel (in or nearest to current window)
  (let* ((snd (selected-sound))
	 (chn (selected-channel))
	 (ms (sort (map mark-sample (marks snd chn)) <)))
    (if (< (length ms) 2)
	(throw 'no-such-mark (list "mark-related action requires two marks"))
	(if (= (length ms) 2)
	    ms
	    (let* ((lw (left-sample snd chn))
		   (rw (right-sample snd chn))
		   (cw (cursor snd chn))
		   (favor (if (and (>= cw lw)
				   (<= cw rw))
			      cw
			      (* .5 (+ lw rw)))))
	      ;; favor is the point we center the search on
	      (define (centered-points points)
		(if (= (length points) 2)
		    points
		    (let ((p1 (car points))
			  (p2 (cadr points))
			  (p3 (caddr points)))
		      (if (< (abs (- p1 favor)) (abs (- p3 favor)))
			  (list p1 p2)
			  (centered-points (cdr points))))))
	      (centered-points ms))))))

(define map-chan-over-target-with-sync
  (lambda (func target origin decay)
    (let* ((snc (sync))
	   (ms (and (eq? target 'marks)
		    (plausible-mark-samples)))
	   (beg (if (eq? target 'sound)
		    0
		    (if (eq? target 'selection)
			(selection-position)
			(car ms))))
	   (overlap (if decay
			(inexact->exact (floor (* (srate) decay)))
			0)))
      (apply for-each
	     (lambda (snd chn)
	       (let ((end (if (or (eq? target 'sound)
				  (eq? target 'cursor))
			      (1- (frames snd chn))
			      (if (eq? target 'selection)
				  (+ (selection-position) (selection-frames))
				  (cadr ms)))))
		 (if (= (sync snd) snc)
		     (map-chan (func (- end beg))
			       beg 
			       (+ end overlap)
			       origin snd chn))))
	     (if (> snc 0) 
		 (all-chans) 
		 (list (list (selected-sound)) 
		       (list (selected-channel))))))))


(define (make-effect-dialog label ok-callback help-callback reset-callback)
  ;; make a standard dialog
  ;; callbacks take 2 args: widget data
  (let* ((dismiss-button (gtk_button_new_with_label "Dismiss"))
	 (help-button (gtk_button_new_with_label "Help"))
	 (ok-button (gtk_button_new_with_label "DoIt"))
	 (reset-button (if reset-callback (gtk_button_new_with_label "Reset") #f))
	 (new-dialog (gtk_dialog_new)))
    (gtk_widget_set_name dismiss-button "quit_button")
    (gtk_widget_set_name help-button "help_button")
    (gtk_widget_set_name ok-button "doit_button")
    (gtk_widget_set_name reset-button "reset_button")
    (gtk_window_set_title (GTK_WINDOW new-dialog) label)
    (gtk_container_set_border_width (GTK_CONTAINER new-dialog) 10)
    (gtk_window_set_default_size (GTK_WINDOW new-dialog) -1 -1)
    (gtk_window_set_resizable (GTK_WINDOW new-dialog) #t)
    (gtk_widget_realize new-dialog)
    (g_signal_connect new-dialog "delete_event" 
		      (lambda (w ev data) 
			(gtk_widget_hide new-dialog)
			#t) ; this is crucial -- thanks to Kjetil for catching it!
		      #f)
    (gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG new-dialog))) dismiss-button #t #t 20)
    (g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide new-dialog)) #f)
    (gtk_widget_show dismiss-button)
    (gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG new-dialog))) ok-button #t #t 20)
    (g_signal_connect ok-button "clicked" ok-callback #f)
    (gtk_widget_show ok-button)
    (if reset-button
	(begin
	  (gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG new-dialog))) reset-button #t #t 20)
	  (g_signal_connect reset-button "clicked" reset-callback #f)
	  (gtk_widget_show reset-button)))
    (gtk_box_pack_end (GTK_BOX (.action_area (GTK_DIALOG new-dialog))) help-button #t #t 20)
    (g_signal_connect help-button "clicked" help-callback #f)
    (gtk_widget_show help-button)
    ;; build rest in (.vbox (GTK_DIALOG new-dialog))
    new-dialog))

(define (change-label w new-label)
  (if w
      (if (GTK_IS_LABEL w)
	  (gtk_label_set_text (GTK_LABEL w) new-label)
	  (gtk_label_set_text (GTK_LABEL (gtk_bin_get_child (GTK_BIN w))) new-label))))


;;; -------- log scaler widget

(define log-scale-ticks 500) ; sets precision (to some extent) of slider 

(define (scale-log->linear lo val hi)
  ;; given user-relative low..val..hi return val as scale-relative (0..log-scale-ticks)
  (let* ((log2 (log 2.0)) ; using log 2 here to get equally spaced octaves
	 (log-lo (/ (log (max lo 1.0)) log2))
	 (log-hi (/ (log hi) log2))
	 (log-val (/ (log val) log2)))
    (inexact->exact (floor (* log-scale-ticks (/ (- log-val log-lo) (- log-hi log-lo)))))))
  
(define (scale-linear->log lo val hi)
  ;; given user-relative lo..hi and scale-relative val, return user-relative val
  ;; since log-scale widget assumes 0..log-scale-ticks, val can be used as ratio (log-wise) between lo and hi
  (let* ((log2 (log 2.0))
	 (log-lo (/ (log (max lo 1.0)) log2))
	 (log-hi (/ (log hi) log2))
	 (log-val (+ log-lo (* (/ val log-scale-ticks) (- log-hi log-lo)))))
    (expt 2.0 log-val)))

(define (scale-log-label lo val hi)
  (format #f "~,2F" (scale-linear->log lo val hi)))
	  
(define (add-sliders dialog sliders)
  ;; sliders is a list of lists, each inner list being (title low initial high callback scale ['log])
  ;; returns list of widgets (for reset callbacks)
  (let* ((mainform (gtk_vbox_new #f 2))
	 (use-hbox (= (length sliders) 1))
	 (table (if (not use-hbox) (gtk_table_new 2 (length sliders) #f)))
	 (slider 0))
    (gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG dialog))) mainform #f #f 4)
    (gtk_widget_show mainform)
    (if (not use-hbox)
	(begin
	  (gtk_box_pack_start (GTK_BOX mainform) table #f #f 4)
	  (gtk_table_set_row_spacings (GTK_TABLE table) 4)
	  (gtk_table_set_col_spacings (GTK_TABLE table) 4)
	  (gtk_widget_show table)))
    (map 
     (lambda (slider-data)
       (let* ((title (list-ref slider-data 0))
	      (low (list-ref slider-data 1))
	      (initial (list-ref slider-data 2))
	      (high (list-ref slider-data 3))
	      (func (list-ref slider-data 4))
	      (scaler (list-ref slider-data 5))
	      (use-log (and (= (length slider-data) 7)
			    (eq? (list-ref slider-data 6) 'log)))
	      (hbox (and use-hbox (gtk_hbox_new #f 0)))
	      (label (gtk_label_new 
		      (if use-hbox
			  (if use-log 
			      (format #f "~A: ~,2F" title initial)
			      (format #f "~A:" title))
			  (if use-log
			      (format #f "~A (~,2F)" title initial)
			      (format #f "~A" title)))))
	      (adj (if use-log 
		       (gtk_adjustment_new (scale-log->linear low initial high) 0 log-scale-ticks 1 10 1)
		       (gtk_adjustment_new initial low high 0.0 0.0 0.0)))
	      (scale (gtk_hscale_new (GTK_ADJUSTMENT adj))))
	 (if use-hbox
	     (begin
	       (gtk_box_pack_start (GTK_BOX mainform) hbox #f #f 2)
	       (gtk_widget_show hbox)
	       (gtk_box_pack_start (GTK_BOX hbox) label #f #f 6))
	     (gtk_table_attach (GTK_TABLE table) label 0 1 slider (1+ slider)
			       (logior GTK_FILL GTK_SHRINK) 
			       (logior GTK_FILL GTK_SHRINK)
			       0 0))
	 (gtk_widget_show label)
	 (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE scale)) GTK_UPDATE_CONTINUOUS)
	 (gtk_scale_set_digits (GTK_SCALE scale)
			       (if use-log
				   0
				   (if (= scaler 1000) 3 (if (= scaler 100) 2 (if (= scaler 10) 1 0)))))
	 (gtk_scale_set_draw_value (GTK_SCALE scale) (not use-log))
	 (if use-hbox
	     (gtk_box_pack_start (GTK_BOX hbox) scale #t #t 0)
	     (begin
	       (gtk_table_attach (GTK_TABLE table) scale 1 2 slider (1+ slider)
				 (logior GTK_FILL GTK_EXPAND GTK_SHRINK) 
				 (logior GTK_FILL GTK_EXPAND GTK_SHRINK)
				 0 0)
	       (set! slider (1+ slider))))
	 (gtk_widget_show scale)
	 (if use-log
	     (g_signal_connect adj "value_changed"
			       (lambda (w d) 
				 (func w d)
				 (change-label label 
					       (format #f "~A: ~,2F" 
						       title 
						       (scale-log-label low (.value (GTK_ADJUSTMENT adj)) high))))
			       #f)
	     (g_signal_connect adj "value_changed" func #f))
	 adj))
     sliders)))

(define (add-target mainform target-callback truncate-callback)
  ;; add a set of 3 radio buttons at the bottom of the main section for choice between sound, selection, between-marks
  ;;   target-callback should take one arg, a symbol: 'sound, 'selection, 'marks, and apply the effect accordingly (upon "DoIt")
  ;;   truncate-callback (if any) takes one arg: boolean representing toggle state (#t = on)
  (let (;(sep (gtk_hseparator_new))
	(rc (gtk_hbox_new #f 0)))
    (gtk_box_pack_start (GTK_BOX mainform) rc #f #f 4)
    ;(gtk_box_pack_start (GTK_BOX rc) sep #t #t 4)
    ;(gtk_widget_show sep)
    (gtk_widget_show rc)
    
    (let ((group #f))
      (for-each
       (lambda (name type on)
	 (let ((button (gtk_radio_button_new_with_label group name)))
	   (set! group (gtk_radio_button_get_group (GTK_RADIO_BUTTON button)))
	   (gtk_box_pack_start (GTK_BOX rc) button #f #f 4)
	   (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) on)
	   (gtk_widget_show button)
	   (g_signal_connect button "clicked" (lambda (w d) (target-callback type)) #f)))
       (list "entire sound" "selection" "between marks")
       (list 'sound 'selection 'marks)
       (list #t #f #f)))

    (if truncate-callback
	(let ((sep (gtk_hseparator_new))
	      (button (gtk_check_button_new_with_label "truncate at end")))
	  (gtk_box_pack_start (GTK_BOX rc) sep #t #t 4)
	  (gtk_widget_show sep)
	  (gtk_box_pack_start (GTK_BOX rc) button #t #t 4)
	  (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON button) #t)
	  (gtk_widget_show button)
	  (g_signal_connect button "clicked" (lambda (w d) (truncate-callback (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON w)))) #f)))))

(define (activate-dialog w)
  (gtk_widget_show w)
  (gtk_window_present (GTK_WINDOW w)))

(define (effect-frames target)
  (if (eq? target 'sound)
      (1- (frames))
      (if (eq? target 'selection)
          (selection-frames)
          (+ 1 (abs (apply - (plausible-mark-samples)))))))


;;; *******************************
;;;                              **
;;; BEGIN PARAMETRIZED EFFECTS   **
;;;                              **
;;; *******************************

;;; AMPLITUDE EFFECTS
;;;

(let ((amp-menu-list '())
      (amp-menu (gtk_menu_item_new_with_label "Amplitude Effects"))
      (amp-cascade (gtk_menu_new)))
  (gtk_menu_shell_append (GTK_MENU_SHELL (main-menu effects-menu)) amp-menu)
  (gtk_widget_show amp-menu)
  (gtk_menu_item_set_submenu (GTK_MENU_ITEM amp-menu) amp-cascade)
  (g_signal_connect amp-menu "activate" (lambda (w d) (update-label amp-menu-list)) #f)

  ;; -------- Gain (gain set by gain-amount)

  (let ((child (gtk_menu_item_new_with_label "Gain"))
	(gain-amount 1.0)
	(gain-dialog #f)
	(gain-target 'sound)
	(gain-envelope #f))
    
    (define (scale-envelope e scl)
      (if (null? e)
	  '()
	  (append (list (car e) (* scl (cadr e)))
		  (scale-envelope (cddr e) scl))))
    
    (gtk_menu_shell_append (GTK_MENU_SHELL amp-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect child "activate" 
		      (lambda (w d) 
			(if (not gain-dialog)
			    (let ((initial-gain-amount 1.0)
				  (sliders '()))
			      (set! gain-dialog
				    (make-effect-dialog 
				     "Gain"
				     (lambda (w data) 
				       "Gain scales amplitude by gain amount."
				       (let ((with-env (and (not (equal? (xe-envelope gain-envelope) (list 0.0 1.0 1.0 1.0)))
							    (scale-envelope (xe-envelope gain-envelope) gain-amount))))
					 (if (eq? gain-target 'sound)
					     (if with-env
						 (env-sound with-env)
						 (scale-by gain-amount))
					     (if (eq? gain-target 'selection)
						 (if (selection?)
						     (if with-env
							 (env-selection with-env)
							 (scale-selection-by gain-amount))
						     (snd-print "no selection"))
						 (let ((pts (plausible-mark-samples)))
						   (if pts
						       (if with-env
							   (env-sound with-env (car pts) (- (cadr pts) (car pts)))
							   (scale-sound-by gain-amount (car pts) (- (cadr pts) (car pts))))))))))
				     (lambda (w data)
				       (help-dialog 
					"Gain"
					"Move the slider to change the gain scaling amount."))
				     (lambda (w data)
				       (set! gain-amount initial-gain-amount)
				       (set! (xe-envelope gain-envelope) (list 0.0 1.0 1.0 1.0))
				       (set! (.value (GTK_ADJUSTMENT (car sliders))) gain-amount)
				       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))))
			      (set! sliders
				    (add-sliders gain-dialog
						 (list (list "gain" 0.0 initial-gain-amount 5.0
							     (lambda (w data)
							       (set! gain-amount (.value (GTK_ADJUSTMENT w))))
							     100))))
			      (gtk_widget_show gain-dialog)
			      (set! gain-envelope (xe-create-enved "gain" 
								   (.vbox (GTK_DIALOG gain-dialog))
								   #f
								   '(0.0 1.0 0.0 1.0)))
			      (set! (xe-envelope gain-envelope) (list 0.0 1.0 1.0 1.0))
			      (add-target (.vbox (GTK_DIALOG gain-dialog)) 
					  (lambda (target) 
					    (set! gain-target target))
					  #f)))
			(activate-dialog gain-dialog))
		      #f)

    (set! amp-menu-list (cons (lambda ()
				(let ((new-label (format #f "Gain (~1,2F)"  gain-amount)))
				  (change-label child new-label)))
			      amp-menu-list)))
  
  ;; -------- Normalize
  
  (let ((child (gtk_menu_item_new_with_label "Normalize"))
	(normalize-amount 1.0)
	(normalize-dialog #f)
	(normalize-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL amp-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect child "activate"
		      (lambda (w d) 
			(if (not normalize-dialog)
			    (let ((initial-normalize-amount 1.0)
				  (sliders '()))
			      (set! normalize-dialog 
				    (make-effect-dialog 
				     "Normalize"
				     (lambda (w data) 
				       (if (eq? normalize-target 'sound)
					   (scale-to normalize-amount)
					   (if (eq? normalize-target 'selection)
					       (if (selection?)
						   (scale-selection-to normalize-amount)
						   (snd-print "no selection"))
					       (let ((pts (plausible-mark-samples)))
						 (if pts
						     (scale-sound-to normalize-amount (car pts) (- (cadr pts) (car pts))))))))
				     (lambda (w data)
				       (help-dialog 
					"Normalize"
					"Normalize scales amplitude to the normalize amount. Move the slider to change the scaling amount."))
				     (lambda (w data)
				       (set! normalize-amount initial-normalize-amount)
				       (set! (.value (GTK_ADJUSTMENT (car sliders))) normalize-amount)
				       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))))
			      (set! sliders
				    (add-sliders normalize-dialog
						 (list (list "normalize" 0.0 initial-normalize-amount 1.1
							     (lambda (w data)
							       (set! normalize-amount (.value (GTK_ADJUSTMENT w))))
					     100))))
			      (add-target (.vbox (GTK_DIALOG normalize-dialog)) 
					  (lambda (target) 
					    (set! normalize-target target))
					  #f)))
			(activate-dialog normalize-dialog))
		      #f)
    (set! amp-menu-list (cons (lambda ()
				(let ((new-label (format #f "Normalize (~1,2F)"  normalize-amount)))
				  (change-label child new-label)))
			      amp-menu-list)))
  
  ;; -------- Gate (gate set by gate-amount)
  
  (let ((gate-amount 0.01)
	(gate-dialog #f)
	(gate-size 128)
	(omit-silence #f))

    (define (squelch-channel amount snd chn)
      (let ((f0 (make-average gate-size))
	    (f1 (make-average gate-size :initial-element 1.0)))
	(map-channel (lambda (y) (* y (average f1 (if (< (average f0 (* y y)) amount) 0.0 1.0))))
		     0 #f snd chn)))

    (let ((child (gtk_menu_item_new_with_label "Gate")))
      (gtk_menu_shell_append (GTK_MENU_SHELL amp-cascade) child)
      (gtk_widget_show child)
      (g_signal_connect child "activate"
			(lambda (w d) 
			  (if (not gate-dialog)
			      ;; if gate-dialog doesn't exist, create it
			      (let ((initial-gate-amount 0.01)
				    (sliders '()))
				(set! gate-dialog
				      (make-effect-dialog 
				       "Gate"
				       (lambda (w data)
					 (let ((snc (sync)))
					   (if (> snc 0)
					       (apply map
						      (lambda (snd chn)
							(if (= (sync snd) snc)
							    (squelch-channel (* gate-amount gate-amount) snd chn)))
						      (all-chans))
					       (squelch-channel (* gate-amount gate-amount) (selected-sound) (selected-channel)))))
				       (lambda (w data)
					 (help-dialog "Gate"
						      "Move the slider to change the gate intensity. Higher values gate more of the sound."))
				       (lambda (w data)
					 (set! gate-amount initial-gate-amount)
					 (set! (.value (GTK_ADJUSTMENT (car sliders))) gate-amount)
					 (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))))
				(set! sliders
				      (add-sliders gate-dialog
						   (list (list "gate" 0.0 initial-gate-amount 0.1
							       (lambda (w data)
								 (set! gate-amount (.value (GTK_ADJUSTMENT w))))
							       1000))))
				;; now add a toggle button setting omit-silence 
				(let ((toggle (gtk_check_button_new_with_label "Omit silence")))
				  (gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG gate-dialog))) toggle #f #f 4)
				  (gtk_widget_show toggle)
				  (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON toggle) omit-silence)
				  (g_signal_connect toggle "clicked" (lambda (w d) (set! omit-silence (.active (GTK_TOGGLE_BUTTON toggle)))) #f))))
			  (activate-dialog gate-dialog))
			#f)
      (set! amp-menu-list (cons (lambda ()
				  (let ((new-label (format #f "Gate (~1,3F)"  gate-amount)))
				    (change-label child new-label)))
				amp-menu-list)))))

;;; DELAY EFFECTS
;;;

(let ((delay-menu-list '())
      (delay-menu (gtk_menu_item_new_with_label "Delay Effects"))
      (delay-cascade (gtk_menu_new)))
  (gtk_menu_shell_append (GTK_MENU_SHELL (main-menu effects-menu)) delay-menu)
  (gtk_widget_show delay-menu)
  (gtk_menu_item_set_submenu (GTK_MENU_ITEM delay-menu) delay-cascade)
  (g_signal_connect delay-menu "activate" (lambda (w d) (update-label delay-menu-list)) #f)

  ;; -------- Echo (controlled by delay-time and echo-amount)

  (let ((child (gtk_menu_item_new_with_label "Echo"))
	(delay-time .5) ; i.e. delay between echoes
	(echo-amount .2)
	(echo-dialog #f)
	(echo-target 'sound)
	(echo-truncate #t))
	;; echo-decay? -- using (* 4 delay-time) currently
    (gtk_menu_shell_append (GTK_MENU_SHELL delay-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not echo-dialog)
	    (let ((initial-delay-time 0.5)
		  (initial-echo-amount 0.2)
		  (sliders '()))
	      (set! echo-dialog 
		    (make-effect-dialog 
		     "Echo"
		     (lambda (w data)
		       (map-chan-over-target-with-sync 
			(lambda (input-samps) 
			  (let ((del (make-delay (inexact->exact (round (* delay-time (srate))))))
				(samp 0))
			    (lambda (inval)
			      (set! samp (1+ samp))
			      (+ inval
				 (delay del
					(* echo-amount (+ (tap del) (if (<= samp input-samps) inval 0.0))))))))
			echo-target "echo"
			(and (not echo-truncate) 
			     (* 4 delay-time))))
		     (lambda (w data)
		       (help-dialog "Echo"
				    "The sliders change the delay time and echo amount."))
		     (lambda (w data)   
		       (set! delay-time initial-delay-time)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) delay-time)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
		       (set! echo-amount initial-echo-amount)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) echo-amount)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (cadr sliders))))))
	      (set! sliders
		    (add-sliders echo-dialog
				 (list (list "delay time" 0.0 initial-delay-time 2.0
					     (lambda (w data)
					       (set! delay-time (.value (GTK_ADJUSTMENT (car sliders)))))
					     100)
				       (list "echo amount" 0.0 initial-echo-amount 1.0
					     (lambda (w data)
					       (set! echo-amount (.value (GTK_ADJUSTMENT (cadr sliders)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG echo-dialog)) 
			  (lambda (target) (set! echo-target target))
			  (lambda (truncate) (set! echo-truncate truncate)))))
	(activate-dialog echo-dialog))
      #f #f) #f)
    (set! delay-menu-list (cons (lambda ()
				  (let ((new-label (format #f "Echo (~1,2F ~1,2F)" 
							   delay-time echo-amount)))
				    (change-label child new-label)))
				delay-menu-list)))

  ;; -------- Filtered echo

  (let ((child (gtk_menu_item_new_with_label "Filtered echo"))
	(flecho-scaler 0.5)
	(flecho-delay 0.9)
	(flecho-dialog #f)
	(flecho-target 'sound)
	(flecho-truncate #t))
    (gtk_menu_shell_append (GTK_MENU_SHELL delay-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not flecho-dialog)
	    (let ((initial-flecho-scaler 0.5)
		  (initial-flecho-delay 0.9)
		  (sliders '()))
	      (set! flecho-dialog 
		    (make-effect-dialog 
		     "Filtered echo"
		     (lambda (w data)
		       (map-chan-over-target-with-sync
			(lambda (input-samps) 
			  (let* ((flt (make-fir-filter :order 4 :xcoeffs (vct .125 .25 .25 .125)))
				 (del (make-delay  (inexact->exact (round (* flecho-delay (srate))))))
				 (samp 0))
			    (lambda (inval)
			      (set! samp (1+ samp))
			      (+ inval 
				 (delay del 
					(fir-filter flt (* flecho-scaler (+ (tap del) (if (<= samp input-samps) inval 0.0)))))))))
			flecho-target 
			"flecho"
			(and (not flecho-truncate) 
			     (* 4 flecho-delay))))
		     (lambda (w data)
		       (help-dialog "Filtered echo"
				    "Move the sliders to set the filter scaler and the delay time in seconds."))
		     (lambda (w data)
		       (set! flecho-scaler initial-flecho-scaler)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) flecho-scaler)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
		       (set! flecho-delay initial-flecho-delay)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) flecho-delay)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (cadr sliders))))))
	      (set! sliders
		    (add-sliders flecho-dialog
				 (list (list "filter scaler" 0.0 initial-flecho-scaler 1.0
					     (lambda (w data)
					       (set! flecho-scaler (.value (GTK_ADJUSTMENT (car sliders)))))
					     100)
				       (list "delay time (secs)" 0.0 initial-flecho-delay 3.0
					     (lambda (w data)
					       (set! flecho-delay (.value (GTK_ADJUSTMENT (cadr sliders)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG flecho-dialog)) 
			  (lambda (target) (set! flecho-target target))
			  (lambda (truncate) (set! flecho-truncate truncate)))))
	(activate-dialog flecho-dialog))
      #f #f) #f)
    (set! delay-menu-list (cons (lambda ()
				  (let ((new-label (format #f "Filtered echo (~1,2F ~1,2F)"
							   flecho-scaler flecho-delay)))
				    (change-label child new-label)))
				delay-menu-list)))

  ;; -------- Modulated echo

  (let ((child (gtk_menu_item_new_with_label "Modulated echo"))
	(zecho-scaler 0.5)
	(zecho-delay 0.75)
	(zecho-freq 6)
	(zecho-amp 10.0)
	(zecho-dialog #f)
	(zecho-target 'sound)
	(zecho-truncate #t))
    (gtk_menu_shell_append (GTK_MENU_SHELL delay-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not zecho-dialog)
	    (let ((initial-zecho-scaler 0.5)
		  (initial-zecho-delay 0.75)
		  (initial-zecho-freq 6)
		  (initial-zecho-amp 10.0)
		  (sliders '()))
	      (set! zecho-dialog 
		    (make-effect-dialog 
		     "Modulated echo"
		     (lambda (w data)
		       (map-chan-over-target-with-sync
			(lambda (input-samps)
			  (let* ((os (make-oscil zecho-freq))
				 (len (inexact->exact (round (* zecho-delay (srate)))))
				 (del (make-delay len :max-size (+ len zecho-amp 1)))
				 (samp 0))
			    (lambda (inval)
			      (set! samp (1+ samp))
			      (+ inval 
				 (delay del 
					(* zecho-scaler (+ (tap del) (if (<= samp input-samps) inval 0.0)))
					(* zecho-amp (oscil os)))))))
			zecho-target
			"zecho"
			(and (not zecho-truncate)
			     (* 4 zecho-delay))))
		     (lambda (w data)
		       (help-dialog "Modulated echo"
				    "Move the sliders to set the echo scaler, the delay time in seconds, 
the modulation frequency, and the echo amplitude."))
		     (lambda (w data)
		       (set! zecho-scaler initial-zecho-scaler)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) zecho-scaler)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! zecho-delay initial-zecho-delay)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) zecho-delay)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! zecho-freq initial-zecho-freq)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 2))) zecho-freq)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2)))
		       (set! zecho-amp initial-zecho-amp)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 3))) zecho-amp)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 3))))))
	      (set! sliders
		    (add-sliders zecho-dialog
				 (list (list "echo scaler" 0.0 initial-zecho-scaler 1.0
					     (lambda (w data)
					       (set! zecho-scaler (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "delay time (secs)" 0.0 initial-zecho-delay 3.0
					     (lambda (w data)
					       (set! zecho-delay (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100)
				       (list "modulation frequency" 0.0 initial-zecho-freq 100.0
					     (lambda (w data)
					       (set! zecho-freq (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     100)
				       (list "modulation amplitude" 0.0 initial-zecho-amp 100.0
					     (lambda (w data)
					       (set! zecho-amp (.value (GTK_ADJUSTMENT (list-ref sliders 3)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG zecho-dialog)) 
			  (lambda (target) (set! zecho-target target))
			  (lambda (truncate) (set! zecho-truncate truncate)))))
	(activate-dialog zecho-dialog))
      #f #f) #f)

    (set! delay-menu-list (cons (lambda ()
				  (let ((new-label (format #f "Modulated echo (~1,2F ~1,2F ~1,2F ~1,2F)" 
							   zecho-scaler zecho-delay zecho-freq zecho-amp)))
				    (change-label child new-label)))
				delay-menu-list)))
    )


;;; FILTERS
;;;

(let ((filter-menu-list '())
      (filter-menu (gtk_menu_item_new_with_label "Filter Effects"))
      (filter-cascade (gtk_menu_new))
      (root-2 (sqrt 2.0)))
  (gtk_menu_shell_append (GTK_MENU_SHELL (main-menu effects-menu)) filter-menu)
  (gtk_widget_show filter-menu)
  (gtk_menu_item_set_submenu (GTK_MENU_ITEM filter-menu) filter-cascade)
  (g_signal_connect_closure_by_id 
   (GPOINTER filter-menu)
   (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT filter-menu))) 0
   (g_cclosure_new (lambda (w d) 
		     (update-label filter-menu-list))
		   #f #f)
   #f)

  ;; -------- Butterworth band-pass filter

  (let ((child (gtk_menu_item_new_with_label "Band-pass filter"))
	(band-pass-freq 1000)
	(band-pass-bw 100)
	(band-pass-dialog #f)
	(band-pass-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL filter-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not band-pass-dialog)
	    (let ((initial-band-pass-freq 1000)
		  (initial-band-pass-bw 100)
		  (sliders '()))
	      (set! band-pass-dialog 
		    (make-effect-dialog 
		     "Band-pass filter"
		     (lambda (w data) 
		       (if (eq? band-pass-target 'sound)
			   (filter-sound (make-butter-band-pass band-pass-freq band-pass-bw))
			   (if (eq? band-pass-target 'selection)
			       (filter-selection (make-butter-band-pass band-pass-freq band-pass-bw)))))
		     (lambda (w data)
		       (help-dialog "Band-pass filter"
				    "Butterworth band-pass filter. Move the sliders to change the center frequency and bandwidth."))
		     (lambda (w data)
		       (set! band-pass-freq initial-band-pass-freq)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) (scale-log->linear 20 band-pass-freq 22050))
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! band-pass-bw initial-band-pass-bw)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) band-pass-bw)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1))))))
	      (set! sliders
		    (add-sliders band-pass-dialog
				 (list (list "center frequency" 20 initial-band-pass-freq 22050
					     (lambda (w data)
					       (set! band-pass-freq (scale-linear->log 20 (.value (GTK_ADJUSTMENT (car sliders))) 22050)))
					     1 'log)
				       (list "bandwidth" 0 initial-band-pass-bw 1000
					     (lambda (w data)
					       (set! band-pass-bw (.value (GTK_ADJUSTMENT (cadr sliders)))))
					     1))))
	      (add-target (.vbox (GTK_DIALOG band-pass-dialog)) 
			  (lambda (target) (set! band-pass-target target)) 
			  #f)))
	(activate-dialog band-pass-dialog))
      #f #f)
     #f)
    (set! filter-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Band-pass filter (~,2F ~1,2D)" 
							    band-pass-freq band-pass-bw)))
				     (change-label child new-label)))
				 filter-menu-list)))

  ;; -------- Butterworth band-reject (notch) filter

  (let ((child (gtk_menu_item_new_with_label "Band-reject filter"))
	(notch-freq 100)
	(notch-bw 100)
	(notch-dialog #f)
	(notch-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL filter-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not notch-dialog)
	    (let ((initial-notch-freq 100)
		  (initial-notch-bw 100)
		  (sliders '()))
	      (set! notch-dialog 
		    (make-effect-dialog 
		     "Band-reject filter"
		     (lambda (w data) 
		       (if (eq? notch-target 'sound)
			   (filter-sound (make-butter-band-reject notch-freq notch-bw))
			   (if (eq? notch-target 'selection)
			       (filter-selection (make-butter-band-reject notch-freq notch-bw)))))
		     (lambda (w data)
		       (help-dialog "Band-reject filter"
				    "Butterworth band-reject filter. Move the sliders to change the center frequency and bandwidth."))
		     (lambda (w data)
		       (set! notch-freq initial-notch-freq)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) (scale-log->linear 20 notch-freq 22050))
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! notch-bw initial-notch-bw)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) notch-bw)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1))))))
	      (set! sliders
		    (add-sliders notch-dialog
				 (list (list "center frequency" 20 initial-notch-freq 22050
					     (lambda (w data)
					       (set! notch-freq (scale-linear->log 20 (.value (GTK_ADJUSTMENT (car sliders))) 22050)))
					     1 'log)
				       (list "bandwidth" 0 initial-notch-bw 1000
					     (lambda (w data)
					       (set! notch-bw (.value (GTK_ADJUSTMENT (cadr sliders)))))
					     1))))
	      (add-target (.vbox (GTK_DIALOG notch-dialog)) 
			  (lambda (target) (set! notch-target target)) 
			  #f)))
	(activate-dialog notch-dialog))
      #f #f)
     #f)
    (set! filter-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Band-reject filter (~,2F ~1,2D)" notch-freq notch-bw)))
				     (change-label child new-label)))
				 filter-menu-list)))

  ;; -------- Butterworth high-pass filter

  (let ((child (gtk_menu_item_new_with_label "High-pass filter"))
	(high-pass-freq 100)
	(high-pass-dialog #f)
	(high-pass-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL filter-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not high-pass-dialog)
	    (let ((initial-high-pass-freq 100)
		  (sliders '()))
	      (set! high-pass-dialog 
		    (make-effect-dialog 
		     "High-pass filter"
		     (lambda (w data) 
		       (if (eq? high-pass-target 'sound)
			   (filter-sound (make-butter-high-pass high-pass-freq))
			   (if (eq? high-pass-target 'selection)
			       (filter-selection (make-butter-high-pass high-pass-freq)))))
		     (lambda (w data)
		       (help-dialog "High-pass filter"
				    "Butterworth high-pass filter. Move the slider to change the high-pass cutoff frequency."))
		     (lambda (w data)
		       (set! high-pass-freq initial-high-pass-freq)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) (scale-log->linear 20 high-pass-freq 22050))
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders high-pass-dialog
				 (list (list "high-pass cutoff frequency" 20 initial-high-pass-freq 22050
					     (lambda (w data)
					       (set! high-pass-freq (scale-linear->log 20 (.value (GTK_ADJUSTMENT (list-ref sliders 0))) 22050)))
					     1 'log))))
	      (add-target (.vbox (GTK_DIALOG high-pass-dialog)) 
			  (lambda (target) (set! high-pass-target target)) 
			  #f)))
	(activate-dialog high-pass-dialog))
      #f #f)
     #f)
    (set! filter-menu-list (cons (lambda ()
				   (let ((new-label (format #f "High-pass filter (~,2F)" high-pass-freq)))
				     (change-label child new-label)))
				 filter-menu-list)))

  ;; -------- Butterworth low-pass filter

  (let ((child (gtk_menu_item_new_with_label "Low-pass filter"))
	(low-pass-freq 1000)
	(low-pass-dialog #f)
	(low-pass-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL filter-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not low-pass-dialog)
	    (let ((initial-low-pass-freq 1000)
		  (sliders '()))
	      (set! low-pass-dialog 
		    (make-effect-dialog 
		     "Low-pass filter"
		     (lambda (w data) 
		       (if (eq? low-pass-target 'sound)
			   (filter-sound (make-butter-low-pass low-pass-freq))
			   (if (eq? low-pass-target 'selection)
			       (filter-selection (make-butter-low-pass low-pass-freq)))))
		     (lambda (w data)
		       (help-dialog "Low-pass filter"
				    "Butterworth low-pass filter. Move the slider to change the low-pass cutoff frequency."))
		     (lambda (w data)
		       (set! low-pass-freq initial-low-pass-freq)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) (scale-log->linear 20 low-pass-freq 22050))
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders low-pass-dialog
				 (list (list "low-pass cutoff frequency" 20 initial-low-pass-freq 22050
					     (lambda (w data)
					       (set! low-pass-freq (scale-linear->log 20 (.value (GTK_ADJUSTMENT (list-ref sliders 0))) 22050)))
					     1 'log))))
	      (add-target (.vbox (GTK_DIALOG low-pass-dialog)) 
			  (lambda (target) (set! low-pass-target target)) 
			  #f)))
	(activate-dialog low-pass-dialog))
      #f #f)
     #f)
    (set! filter-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Low-pass filter (~,2F)" low-pass-freq)))
				     (change-label child new-label)))
				 filter-menu-list)))

  ;; -------- Comb filter

  (let ((child (gtk_menu_item_new_with_label "Comb filter"))
	(comb-scaler 0.1)
	(comb-size 50)
	(comb-dialog #f)
	(comb-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL filter-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not comb-dialog)
	    (let ((initial-comb-scaler 0.1)
		  (initial-comb-size 50)
		  (sliders '()))
	      (set! comb-dialog 
		    (make-effect-dialog 
		     "Comb filter"
		     (lambda (w data) 
		       (map-chan-over-target-with-sync
			(lambda (ignored) 
			  (let ((delay-line (make-vct comb-size 0.0))
				(delay-loc 0))
			    (lambda (x)
			      (let ((result (vct-ref delay-line delay-loc)))
				(vct-set! delay-line delay-loc (+ x (* comb-scaler result)))
				(set! delay-loc (1+ delay-loc))
				(if (= delay-loc comb-size) (set! delay-loc 0))
				result))))
			comb-target "comb-filter" #f))
		    (lambda (w data)
		      (help-dialog "Comb filter"
				   "Move the sliders to change the comb scaler and size."))
		    (lambda (w data)
		      (set! comb-scaler initial-comb-scaler)
		      (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) comb-scaler)
		      (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		      (set! comb-size initial-comb-size)
		      (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) comb-size)
		      (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1))))))
	      (set! sliders
		    (add-sliders comb-dialog
				 (list (list "scaler" 0.0 initial-comb-scaler 1.0
					     (lambda (w data)
					       (set! comb-scaler (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "size" 0 initial-comb-size 100
					     (lambda (w data)
					       (set! comb-size (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     1))))
	      (add-target (.vbox (GTK_DIALOG comb-dialog)) 
			  (lambda (target) (set! comb-target target)) 
			  #f)))
	(activate-dialog comb-dialog))
      #f #f)
     #f)
    (set! filter-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Comb filter (~1,2F ~1,2D)" comb-scaler comb-size)))
				     (change-label child new-label)))
				 filter-menu-list)))

  ;; -------- Comb-chord filter

  (let ((child (gtk_menu_item_new_with_label "Comb chord filter"))
	(new-comb-chord-scaler 0.95)
	(new-comb-chord-size 60)
	(new-comb-chord-amp 0.3)
	(new-comb-chord-interval-one 0.75)
	(new-comb-chord-interval-two 1.20)
	(new-comb-chord-dialog #f)
	(new-comb-chord-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL filter-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not new-comb-chord-dialog)
	    (let ((initial-new-comb-chord-scaler 0.95)
		  (initial-new-comb-chord-size 60)
		  (initial-new-comb-chord-amp 0.3)
		  (initial-new-comb-chord-interval-one 0.75)
		  (initial-new-comb-chord-interval-two 1.20)
		  (sliders '()))
	      (set! new-comb-chord-dialog
		    (make-effect-dialog 
		     "Comb chord filter"
		     (lambda (w data)
		       (map-chan-over-target-with-sync
			(lambda (ignored)
			  (let ((c1 (make-comb new-comb-chord-scaler new-comb-chord-size))
				(c2 (make-comb new-comb-chord-scaler (* new-comb-chord-size new-comb-chord-interval-one)))
				(c3 (make-comb new-comb-chord-scaler (* new-comb-chord-size new-comb-chord-interval-two))))
			    (lambda (x)
			      (* new-comb-chord-amp (+ (comb c1 x) (comb c2 x) (comb c3 x))))))
			new-comb-chord-target
			"new-comb-chord" #f))
		     (lambda (w data)
		       (help-dialog "Comb chord filter"
				    "Creates chords by using filters at harmonically related sizes.
Move the sliders to set the comb chord parameters."))
		     (lambda (w data)
		       (set! new-comb-chord-scaler initial-new-comb-chord-scaler)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) new-comb-chord-scaler)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! new-comb-chord-size initial-new-comb-chord-size)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) new-comb-chord-size)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! new-comb-chord-amp initial-new-comb-chord-amp)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 2))) new-comb-chord-amp)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2)))
		       (set! new-comb-chord-interval-one initial-new-comb-chord-interval-one)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 3))) new-comb-chord-interval-one)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 3)))
		       (set! new-comb-chord-interval-two initial-new-comb-chord-interval-two)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 4))) new-comb-chord-interval-two)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 4))))))
	      (set! sliders
		    (add-sliders new-comb-chord-dialog
				 (list (list "chord scaler" 0.0 initial-new-comb-chord-scaler 1.0
					     (lambda (w data)
					       (set! new-comb-chord-scaler (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "chord size" 0 initial-new-comb-chord-size 100
					     (lambda (w data)
					       (set! new-comb-chord-size (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     1)
				       (list "amplitude" 0.0 initial-new-comb-chord-amp 1.0
					     (lambda (w data)
					       (set! new-comb-chord-amp (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     100)
				       (list "interval one" 0.0 initial-new-comb-chord-interval-one 2.0
					     (lambda (w data)
					       (set! new-comb-chord-interval-one (.value (GTK_ADJUSTMENT (list-ref sliders 4)))))
					     100)
				       (list "interval two" 0.0 initial-new-comb-chord-interval-two 2.0
					     (lambda (w data)
					       (set! new-comb-chord-interval-two (.value (GTK_ADJUSTMENT (list-ref sliders 4)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG new-comb-chord-dialog))
			  (lambda (target) (set! new-comb-chord-target target))
			  #f)))
	(activate-dialog new-comb-chord-dialog))
      #f #f)
     #f)
    (set! filter-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Comb chord filter (~1,2F ~1,2D ~1,2F ~1,2F ~1,2F)"  
							    new-comb-chord-scaler new-comb-chord-size new-comb-chord-amp 
							    new-comb-chord-interval-one new-comb-chord-interval-two)))           
				     (change-label child new-label)))
				 filter-menu-list)))

  ;; -------- Moog filter

  (let ((child (gtk_menu_item_new_with_label "Moog filter"))
	(moog-cutoff-frequency 10000)
	(moog-resonance 0.5)
	(moog-dialog #f)
	(moog-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL filter-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not moog-dialog)
	    (let ((initial-moog-cutoff-frequency 10000)
		  (initial-moog-resonance 0.5)
		  (sliders '()))
	      (set! moog-dialog 
		    (make-effect-dialog
		     "Moog filter"
		     (lambda (w data)
		       (map-chan-over-target-with-sync
			(lambda (ignored) 
			  (let ((gen (make-moog-filter moog-cutoff-frequency moog-resonance)))
			    (lambda (inval)
			      (moog-filter gen inval))))
			moog-target "moog-filter" #f))
		     (lambda (w data)
		       (help-dialog "Moog filter"
				    "Moog-style 4-pole lowpass filter with 24db/oct rolloff and variable resonance.
Move the sliders to set the filter cutoff frequency and resonance."))
		     (lambda (w data)
		       (set! moog-cutoff-frequency initial-moog-cutoff-frequency)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) (scale-log->linear 20 moog-cutoff-frequency 22050))
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! moog-resonance initial-moog-resonance)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) moog-resonance)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1))))))
	      (set! sliders
		    (add-sliders moog-dialog
				 (list (list "cutoff frequency" 20 initial-moog-cutoff-frequency 22050
					     (lambda (w data)
					       (set! moog-cutoff-frequency (scale-linear->log 20 (.value (GTK_ADJUSTMENT (list-ref sliders 0))) 22050)))
					     1 'log)
				       (list "resonance" 0.0 initial-moog-resonance 1.0
					     (lambda (w data)
					       (set! moog-resonance (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG moog-dialog))
			  (lambda (target) (set! moog-target target)) 
			  #f)))
	(activate-dialog moog-dialog))
      #f #f)
     #f)
    (set! filter-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Moog filter (~,2F ~1,2F)" moog-cutoff-frequency moog-resonance)))
				     (change-label child new-label)))
				 filter-menu-list)))
  )


;;; FREQUENCY EFFECTS

(let ((freq-menu-list '())
      (freq-menu (gtk_menu_item_new_with_label "Frequency Effects"))
      (freq-cascade (gtk_menu_new)))
  (gtk_menu_shell_append (GTK_MENU_SHELL (main-menu effects-menu)) freq-menu)
  (gtk_widget_show freq-menu)
  (gtk_menu_item_set_submenu (GTK_MENU_ITEM freq-menu) freq-cascade)
  (g_signal_connect_closure_by_id 
   (GPOINTER freq-menu)
   (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT freq-menu))) 0
   (g_cclosure_new (lambda (w d) 
		     (update-label freq-menu-list))
		   #f #f)
   #f)

  ;; -------- Adaptive saturation

  (let ((child (gtk_menu_item_new_with_label "Adaptive saturation"))
	(adsat-size 4)
	(adsat-dialog #f)
	(adsat-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL freq-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not adsat-dialog)
	    (let ((initial-adsat-size 4)
		  (sliders '()))
	      (set! adsat-dialog
		    (make-effect-dialog
		     "Adaptive saturation"
		     (lambda (w data)
		       (map-chan-over-target-with-sync
			(lambda (ignored)
			  (let ((mn 0.0)
				(mx 0.0)
				(n 0)
				(vals (make-vct adsat-size)))
			    (lambda (val)
			      (if (= n adsat-size)
				  (begin
				    (do ((i 0 (1+ i)))
					((= i adsat-size))
				      (if (>= (vct-ref vals i) 0.0)
					  (vct-set! vals i mx)
					  (vct-set! vals i mn)))
				    (set! n 0)
				    (set! mx 0.0)
				    (set! mn 0.0)
				    vals)
				  (begin
				    (vct-set! vals n val)
				    (if (> val mx) (set! mx val))
				    (if (< val mn) (set! mn val))
				    (set! n (1+ n))
				    #f)))))
			adsat-target "adsat" #f))
		     (lambda (w data)
		       (help-dialog "Adaptive saturation"
				    "Move the slider to change the saturation scaling factor."))
		     (lambda (w data)
		       (set! adsat-size initial-adsat-size)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) adsat-size)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders adsat-dialog
				 (list (list "adaptive saturation size" 0 initial-adsat-size 10
					     (lambda (w data)
					       (set! adsat-size (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     1))))
	      (add-target (.vbox (GTK_DIALOG adsat-dialog)) 
			  (lambda (target) (set! adsat-target target)) 
			  #f)))
	(activate-dialog adsat-dialog))
      #f #f)
     #f)
    (set! freq-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Adaptive saturation (~1,2D)" adsat-size)))
				   (change-label child new-label)))
			       freq-menu-list)))

  ;; -------- Sample rate conversion (resample)

  (let ((child (gtk_menu_item_new_with_label "Sample rate conversion"))
	(src-amount 0.0)
	(src-dialog #f)
	(src-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL freq-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not src-dialog)
	    (let ((initial-src-amount 0.0)
		  (sliders '()))
	      (set! src-dialog
		    (make-effect-dialog
		     "Sample rate conversion"
		     (lambda (w data) 
		       (if (eq? src-target 'sound)
			   (src-sound src-amount)
			   (if (eq? src-target 'selection)
			       (if (selection?)
				   (src-selection src-amount)
				   (snd-print "no selection"))
			       (snd-print "can't apply src between marks yet"))))
		     (lambda (w data)
		       (help-dialog "Sample rate conversion"
				    "Move the slider to change the sample rate.
Values greater than 1.0 speed up file play, negative values reverse it."))
		     (lambda (w data)
		       (set! src-amount initial-src-amount)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) src-amount)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders src-dialog
				 (list (list "sample rate" -2.0 initial-src-amount 2.0
					     (lambda (w data)
					       (set! src-amount (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG src-dialog)) 
			  (lambda (target) (set! src-target target)) 
			  #f)))
	(activate-dialog src-dialog))
      #f #f)
     #f)
    (set! freq-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Sample rate scaling (~1,2F)" src-amount)))
				   (change-label child new-label)))
			       freq-menu-list)))

  ;; -------- Time and pitch scaling by granular synthesis and sampling rate conversion

  (let ((child (gtk_menu_item_new_with_label "Time/pitch scaling"))
	(time-scale 1.0)
	(hop-size 0.05)
	(segment-length 0.15)
	(ramp-scale 0.5)
	(pitch-scale 1.0)
	(expsrc-dialog #f)
	(expsrc-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL freq-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not expsrc-dialog)
	    (let ((initial-time-scale 1.0)
		  (initial-hop-size 0.05)
		  (initial-segment-length 0.15)
		  (initial-ramp-scale 0.5)
		  (initial-pitch-scale 1.0)
		  (sliders '()))
	      (set! expsrc-dialog 
		    (make-effect-dialog
		     "Time/pitch scaling"
		     (lambda (w data) 
		       (let ((snd (selected-sound)))
			 (save-controls snd)
			 (reset-controls snd)
			 (set! (speed-control snd) pitch-scale)
			 (let ((new-time (* pitch-scale time-scale)))
			   (if (not (= new-time 1.0))
			       (begin
				 (set! (expand-control? snd) #t)
				 (set! (expand-control snd) new-time)
				 (set! (expand-control-hop snd) hop-size)
				 (set! (expand-control-length snd) segment-length)
				 (set! (expand-control-ramp snd) ramp-scale))))
			 (if (eq? expsrc-target 'marks)
			     (let ((ms (plausible-mark-samples)))
			       (apply-controls snd 0 (car ms) (1+ (- (cadr ms) (car ms)))))
			     (apply-controls snd (if (eq? expsrc-target 'sound) 0 2)))
			 (restore-controls snd)))
		     (lambda (w data)
		       (help-dialog "Time/pitch scaling"
				    "Move the sliders to change the time/pitch scaling parameters."))
		     (lambda (w data)
		       (set! time-scale initial-time-scale)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) time-scale)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! hop-size initial-hop-size)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) hop-size)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! segment-length initial-segment-length)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 2))) segment-length)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2)))
		       (set! ramp-scale initial-ramp-scale)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 3))) ramp-scale)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 3)))
		       (set! pitch-scale initial-pitch-scale)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 4))) pitch-scale)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 4))))))
	      (set! sliders
		    (add-sliders expsrc-dialog
				 (list (list "time scale" 0.0 initial-time-scale 5.0
					     (lambda (w data)
					       (set! time-scale (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "hop size" 0.0 initial-hop-size 1.0
					     (lambda (w data)
					       (set! hop-size (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100)
				       (list "segment length" 0.0 initial-segment-length 0.5
					     (lambda (w data)
					       (set! segment-length (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     100)
				       (list "ramp scale" 0.0 initial-ramp-scale 0.5
					     (lambda (w data)
					       (set! ramp-scale (.value (GTK_ADJUSTMENT (list-ref sliders 3)))))
					     1000)
				       (list "pitch scale" 0.0 initial-pitch-scale 5.0
					     (lambda (w data)
					       (set! pitch-scale (.value (GTK_ADJUSTMENT (list-ref sliders 4)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG expsrc-dialog)) 
			  (lambda (target) (set! expsrc-target target)) 
			  #f)))
	(activate-dialog expsrc-dialog))
      #f #f)
     #f)
    (set! freq-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Time/pitch scaling (~1,2F ~1,2F)" time-scale pitch-scale)))
				   (change-label child new-label)))
			       freq-menu-list)))

;;; -------- Time-varying sample rate conversion (resample)
;;; (KSM)

  (let ((child (gtk_menu_item_new_with_label "Src-timevar"))
	(src-timevar-scale 1.0)
	(src-timevar-dialog #f)
	(src-timevar-target 'sound)
	(src-timevar-envelope #f))

    (define (scale-envelope e scl)
      (if (null? e)
	  '()
	  (append (list (car e) (* scl (cadr e)))
		  (scale-envelope (cddr e) scl))))

    (gtk_menu_shell_append (GTK_MENU_SHELL freq-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not src-timevar-dialog)
	    (let ((initial-src-timevar-scale 1.0)
		  (sliders '()))
	      (set! src-timevar-dialog
		    (make-effect-dialog 
		     "Src-Timevar"
		     (lambda (w data) 
		       (let ((env (scale-envelope (xe-envelope src-timevar-envelope)
						  src-timevar-scale)))
			 (if (eq? src-timevar-target 'sound)
			     (src-sound env)
			     (if (eq? src-timevar-target 'selection)
				 (if (selection-member? (selected-sound))
				     (src-selection env)
				     (display "no selection"))
				 (let ((pts (plausible-mark-samples)))
				   (if pts
				       (let* ((beg (car pts))
					      (end (cadr pts))
					      (len (- end beg)))
					 (src-channel (make-env env :end len) beg len (selected-sound)))))))))
		     (lambda (w data)
		       (help-dialog 
			"Src-Timevar"
			"Move the slider to change the src-timevar scaling amount."))
		     (lambda (w data)
		       (set! src-timevar-amount initial-src-timevar-scale)
		       (set! (xe-envelope src-timevar-envelope) (list 0.0 1.0 1.0 1.0))
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) src-timevar-scale)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))))
	      (set! sliders
		    (add-sliders src-timevar-dialog
				 (list (list "Resample factor" 0.0 initial-src-timevar-scale 10.0
					     (lambda (w data)
					       (set! src-timevar-scale (.value (GTK_ADJUSTMENT w))))
					     100))))
	      (gtk_widget_show src-timevar-dialog)
	      (set! src-timevar-envelope (xe-create-enved "src-timevar" 
							  (.vbox (GTK_DIALOG src-timevar-dialog))
							  #f
						   '(0.0 1.0 0.0 1.0)))
	      (set! (xe-envelope src-timevar-envelope) (list 0.0 1.0 1.0 1.0))
	      (add-target (.vbox (GTK_DIALOG src-timevar-dialog)) 
			  (lambda (target) 
			    (set! src-timevar-target target))
			  #f)))
	(activate-dialog src-timevar-dialog))
      #f #f)
     #f)
    (set! freq-menu-list (cons (lambda ()
				(let ((new-label (format #f "Src-Timevar")))
				  (change-label child new-label)))
			       freq-menu-list)))
    )

;;; MODULATION EFFECTS

(let ((mod-menu-list '())
      (mod-menu (gtk_menu_item_new_with_label "Modulation Effects"))
      (mod-cascade (gtk_menu_new)))
  (gtk_menu_shell_append (GTK_MENU_SHELL (main-menu effects-menu)) mod-menu)
  (gtk_widget_show mod-menu)
  (gtk_menu_item_set_submenu (GTK_MENU_ITEM mod-menu) mod-cascade)
  (g_signal_connect_closure_by_id 
   (GPOINTER mod-menu)
   (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT mod-menu))) 0
   (g_cclosure_new (lambda (w d) 
		     (update-label mod-menu-list))
		   #f #f)
   #f)

  ;; -------- Amplitude modulation

  (let ((child (gtk_menu_item_new_with_label "Amplitude modulation"))
	(am-effect-amount 100.0)
	(am-effect-dialog #f)
	(am-effect-target 'sound)
	(am-effect-envelope #f))
    (gtk_menu_shell_append (GTK_MENU_SHELL mod-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not am-effect-dialog)
	    (let ((initial-am-effect-amount 100.0)
		  (sliders '()))
	      (set! am-effect-dialog
		    (make-effect-dialog
		     "Amplitude modulation"
		     (lambda (w data) 
		       (map-chan-over-target-with-sync
			(lambda (ignored) 
			  (let* ((os (make-oscil am-effect-amount))
				 (need-env (not (equal? (xe-envelope am-effect-envelope) (list 0.0 1.0 1.0 1.0))))
				 (e (and need-env (make-env (xe-envelope am-effect-envelope) :end (1- (effect-frames am-effect-target))))))
			    (if need-env
				(lambda (inval)
				  (amplitude-modulate 1.0 inval (* (env e) (oscil os))))
				(lambda (inval)
				  (amplitude-modulate 1.0 inval (oscil os))))))
			am-effect-target "am" #f))
		     (lambda (w data)
		       (help-dialog "Amplitude modulation"
				    "Move the slider to change the modulation amount."))
		     (lambda (w data)
		       (set! am-effect-amount initial-am-effect-amount)
		       (set! (xe-envelope am-effect-envelope) (list 0.0 1.0 1.0 1.0))
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) am-effect-amount)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders am-effect-dialog
				 (list (list "amplitude modulation" 0.0 initial-am-effect-amount 1000.0
					     (lambda (w data)
					       (set! am-effect-amount (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     1))))
	      (gtk_widget_show am-effect-dialog)
	      (set! am-effect-envelope (xe-create-enved "am" 
						   (.vbox (GTK_DIALOG am-effect-dialog))
						   #f
						   '(0.0 1.0 0.0 1.0)))
	      (set! (xe-envelope am-effect-envelope) (list 0.0 1.0 1.0 1.0))
	      (add-target (.vbox (GTK_DIALOG am-effect-dialog))
			  (lambda (target) (set! am-effect-target target))
			  #f)))
	(activate-dialog am-effect-dialog))
      #f #f)
     #f)
    (set! mod-menu-list (cons (lambda ()
				(let ((new-label (format #f "Amplitude modulation (~1,2F)"  am-effect-amount)))
				  (change-label child new-label)))
			      mod-menu-list)))

  ;; -------- Ring modulation
    
  (let ((child (gtk_menu_item_new_with_label "Ring modulation"))
	(rm-frequency 100)
	(rm-radians 100)
	(rm-dialog #f)
	(rm-target 'sound)
	(rm-envelope #f))
    (gtk_menu_shell_append (GTK_MENU_SHELL mod-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not rm-dialog)
	    (let ((initial-rm-frequency 100)
		  (initial-rm-radians 100)
		  (sliders '()))
	      (set! rm-dialog
		    (make-effect-dialog
		     "Ring modulation"
		     (lambda (w data)
		       (map-chan-over-target-with-sync
			(lambda (ignored) 
			  (let* ((gliss-env (list 0 0 1 (hz->radians rm-radians)))
				 (os (make-oscil rm-frequency))
				 (need-env (and rm-envelope (not (equal? (xe-envelope rm-envelope) (list 0.0 1.0 1.0 1.0)))))
				 (e (and need-env (make-env (xe-envelope rm-envelope) :end (1- (effect-frames rm-target)))))
				 (len (frames))
				 (genv (make-env :envelope gliss-env :end len)))
			    (if need-env
				(lambda (inval)
				  (* inval (* (env e) (oscil os))))
				(lambda (inval)
				  (* inval (oscil os))))))
			rm-target "ring-modulation" #f))
		     (lambda (w data)
		       (help-dialog "Ring modulation"
				    "Move the slider to change the ring modulation parameters."))
		     (lambda (w data)
		       (set! rm-frequency initial-rm-frequency)
		       (set! (xe-envelope rm-envelope) (list 0.0 1.0 1.0 1.0))
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) rm-frequency)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! rm-radians initial-rm-radians)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) rm-radians)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1))))))
	      (set! sliders
		    (add-sliders rm-dialog
				 (list 
				  (list "modulation frequency" 0 initial-rm-frequency 1000
					(lambda (w data)
					  (set! rm-frequency (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					1)
				  (list "modulation radians" 0 initial-rm-radians 360
					(lambda (w data)
					  (set! rm-radians (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					1))))
	      (gtk_widget_show rm-dialog)
	      (set! rm-envelope (xe-create-enved "rm frequency" 
						 (.vbox (GTK_DIALOG rm-dialog))
						 #f
						 '(0.0 1.0 0.0 1.0)))
	      (set! (xe-envelope rm-envelope) (list 0.0 1.0 1.0 1.0))
	      (add-target (.vbox (GTK_DIALOG rm-dialog))
			  (lambda (target) (set! rm-target target)) 
			  #f)))
	(activate-dialog rm-dialog))
      #f #f)
     #f)
    (set! mod-menu-list (cons (lambda ()
				(let ((new-label (format #f "Ring modulation (~1,2D ~1,2D)"
							 rm-frequency rm-radians)))
				  (change-label child new-label)))
			      mod-menu-list)))
  )


;;; REVERBS

(let ((reverb-menu-list '())
      (reverb-menu (gtk_menu_item_new_with_label "Reverberation"))
      (reverb-cascade (gtk_menu_new)))
  (gtk_menu_shell_append (GTK_MENU_SHELL (main-menu effects-menu)) reverb-menu)
  (gtk_widget_show reverb-menu)
  (gtk_menu_item_set_submenu (GTK_MENU_ITEM reverb-menu) reverb-cascade)
  (g_signal_connect_closure_by_id 
   (GPOINTER reverb-menu)
   (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT reverb-menu))) 0
   (g_cclosure_new (lambda (w d) 
		     (update-label reverb-menu-list))
		   #f #f)
   #f)

  ;; -------- Reverb from Michael McNabb's Nrev 

  (let ((child (gtk_menu_item_new_with_label "McNabb reverb"))
	(reverb-amount 0.1)
	(reverb-filter 0.5)
	(reverb-feedback 1.09)
	(reverb-dialog #f)
	(reverb-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL reverb-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	;; add reverb-control-decay (with ramp?) and reverb-truncate
	(if (not reverb-dialog)
	    (let ((initial-reverb-amount 0.1)
		  (initial-reverb-filter 0.5)
		  (initial-reverb-feedback 1.09)
		  (sliders '()))
	      (set! reverb-dialog 
		    (make-effect-dialog 
		     "McNabb reverb"
		     (lambda (w data)
		       (let ((snd (selected-sound)))
			 (save-controls snd)
			 (reset-controls snd)
			 (set! (reverb-control? snd) #t)
			 (set! (reverb-control-scale snd) reverb-amount)
			 (set! (reverb-control-lowpass snd) reverb-filter)
			 (set! (reverb-control-feedback snd) reverb-feedback)
			 (if (eq? reverb-target 'marks)
			     (let ((ms (plausible-mark-samples)))
			       (apply-controls snd 0 (car ms) (1+ (- (cadr ms) (car ms)))))
			     (apply-controls snd (if (eq? reverb-target 'sound) 0 2)))
			 (restore-controls snd)))
		     (lambda (w data)
		       (help-dialog "McNabb reverb"
				    "Reverberator from Michael McNabb.
Adds reverberation scaled by reverb amount, lowpass filtering, and feedback. Move the sliders to change the reverb parameters."))
		     (lambda (w data)
		       (set! reverb-amount initial-reverb-amount)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) reverb-amount)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! reverb-filter initial-reverb-filter)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) reverb-filter)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! reverb-feedback initial-reverb-feedback)
		       (set! (.value (GTK_ADJUSTMENT (caddr sliders))) reverb-feedback)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2))))))
	      (set! sliders
		    (add-sliders reverb-dialog
				 (list (list "reverb amount" 0.0 initial-reverb-amount 1.0
					     (lambda (w data)
					       (set! reverb-amount (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "reverb filter" 0.0 initial-reverb-filter 1.0
					     (lambda (w data)
					       (set! reverb-filter (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100)
				       (list "reverb feedback" 0.0 initial-reverb-feedback 1.25
					     (lambda (w data)
					       (set! reverb-feedback (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG reverb-dialog)) 
			  (lambda (target) (set! reverb-target target)) 
			  #f)))
	(activate-dialog reverb-dialog))
      #f #f)
     #f)
    (set! reverb-menu-list (cons (lambda ()
				   (let ((new-label (format #f "McNabb reverb (~1,2F ~1,2F ~1,2F)" reverb-amount reverb-filter reverb-feedback)))
				     (change-label child new-label)))
				 reverb-menu-list)))

  ;; -------- Chowning reverb

  (let ((child (gtk_menu_item_new_with_label "Chowning reverb"))
	(jc-reverb-decay 2.0)
	(jc-reverb-volume 0.1)
	(jc-reverb-dialog #f)
	(jc-reverb-target 'sound)
	(jc-reverb-truncate #t))

    (define jc-reverb-1 ; changed from examp.scm for target/truncate (and omits low-pass and amp-env)
      (lambda (input-samps)
	(let* ((allpass1 (make-all-pass -0.700 0.700 1051))
	       (allpass2 (make-all-pass -0.700 0.700  337))
	       (allpass3 (make-all-pass -0.700 0.700  113))
	       (comb1 (make-comb 0.742 4799))
	       (comb2 (make-comb 0.733 4999))
	       (comb3 (make-comb 0.715 5399))
	       (comb4 (make-comb 0.697 5801))
	       (outdel1 (make-delay (inexact->exact (round (* .013 (srate))))))
	       (comb-sum 0.0)
	       (comb-sum-1 0.0)
	       (comb-sum-2 0.0)
	       (delA 0.0)
	       (delB 0.0)
	       (samp 0))
	  (lambda (inval)
	    (let ((allpass-sum (all-pass allpass3 
					 (all-pass allpass2 
						   (all-pass allpass1 
							     (if (< samp input-samps) inval 0.0))))))
	      (set! samp (1+ samp))
	      (set! comb-sum-2 comb-sum-1)
	      (set! comb-sum-1 comb-sum)
	      (set! comb-sum 
		    (+ (comb comb1 allpass-sum)
		       (comb comb2 allpass-sum)
		       (comb comb3 allpass-sum)
		       (comb comb4 allpass-sum)))
	      (+ inval
		 (* jc-reverb-volume (delay outdel1 comb-sum))))))))
    
    (gtk_menu_shell_append (GTK_MENU_SHELL reverb-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not jc-reverb-dialog)
	    (let ((initial-jc-reverb-decay 2.0)
		  (initial-jc-reverb-volume 0.1)
		  (sliders '()))
	      (set! jc-reverb-dialog
		    (make-effect-dialog
		     "Chowning reverb"
		     (lambda (w data) 
		       (map-chan-over-target-with-sync
			jc-reverb-1 jc-reverb-target "jc-reverb" (and (not jc-reverb-truncate) jc-reverb-decay)))
		     (lambda (w data)
		       (help-dialog "Chowning reverb"
				    "Nice reverb from John Chowning. Move the sliders to set the reverb parameters."))
		     (lambda (w data)
		       (set! jc-reverb-decay initial-jc-reverb-decay)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) jc-reverb-decay)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! jc-reverb-volume initial-jc-reverb-volume)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) jc-reverb-volume)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1))))))
	      (set! sliders
		    (add-sliders jc-reverb-dialog
				 (list (list "decay duration" 0.0 initial-jc-reverb-decay 10.0
					     (lambda (w data)
					       (set! jc-reverb-decay (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "reverb volume" 0.0 initial-jc-reverb-volume 1.0
					     (lambda (w data)
					       (set! jc-reverb-volume (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG jc-reverb-dialog)) 
			  (lambda (target) (set! jc-reverb-target target))
			  (lambda (truncate) (set! jc-reverb-truncate truncate)))))
	(activate-dialog jc-reverb-dialog))
      #f #f)
     #f)
    (set! reverb-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Chowning reverb (~1,2F ~1,2F)" jc-reverb-decay jc-reverb-volume)))
				     (change-label child new-label)))
				 reverb-menu-list)))

  ;; -------- Convolution

  (let ((child (gtk_menu_item_new_with_label "Convolution"))
	(convolve-sound-one 0)
	(convolve-sound-two 1)
	(convolve-amp 0.01)
	(convolve-dialog #f))
    (gtk_menu_shell_append (GTK_MENU_SHELL reverb-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not convolve-dialog)
	    (let ((initial-convolve-sound-one 0)
		  (initial-convolve-sound-two 1)
		  (initial-convolve-amp 0.01)
		  (sliders '()))
	      (set! convolve-dialog
		    (make-effect-dialog
		     "Convolution"
		     (lambda (w data)
		       (let* ((flt-len (frames convolve-sound-one))
			      (total-len (+ flt-len (frames convolve-sound-two)))
			      (cnv (make-convolve :filter (channel->vct 0 flt-len convolve-sound-one)))
			      (sf (make-sample-reader 0 convolve-sound-two))
			      (out-data (make-vct total-len)))
			 (vct-map! out-data (lambda () (convolve cnv (lambda (dir) (next-sample sf)))))
			 (free-sample-reader sf)
			 (vct-scale! out-data convolve-amp)
			 (let ((max-samp (vct-peak out-data)))
			   (vct->channel out-data 0 total-len convolve-sound-two)
			   (if (> max-samp 1.0) (set! (y-bounds snd1) (list (- max-samp) max-samp)))
			   max-samp)))
		     (lambda (w data)
		       (help-dialog "Convolution"
				    "Very simple convolution. Move the sliders to set the numbers of the soundfiles
to be convolved and the amount for the amplitude scaler.  Output will be scaled to floating-point values, resulting
in very large (but not clipped) amplitudes. Use the Normalize amplitude effect to rescale the output.
The convolution data file typically defines a natural reverberation source, and the output from this effect
can provide very striking reverb effects. You can find convolution data files on sites listed at
http://www.bright.net/~dlphilp/linux_csound.html under Impulse Response Data."))
		     (lambda (w data)
		       (set! convolve-sound-one initial-convolve-sound-one)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) convolve-sound-one)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! convolve-sound-two initial-convolve-sound-two)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) convolve-sound-two)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! convolve-amp initial-convolve-amp)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 2))) convolve-amp)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2))))))
	      (set! sliders
		    (add-sliders convolve-dialog
				 (list (list "impulse response file" 0 initial-convolve-sound-one 24
					     (lambda (w data)
					       (set! convolve-sound-one (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     1)
				       (list "sound file" 0 initial-convolve-sound-two 24
					     (lambda (w data)
					       (set! convolve-sound-two (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     1)
				       (list "amplitude" 0.0 initial-convolve-amp 0.10
					     (lambda (w data)
					       (set! convolve-amp (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     1000))))))
	(activate-dialog convolve-dialog))
      #f #f)
     #f)
    (set! reverb-menu-list (cons (lambda ()
				   (let ((new-label (format #f "Convolution (~1,2D ~1,2D ~1,2F)" convolve-sound-one convolve-sound-two convolve-amp)))
				     (change-label child new-label)))
				 reverb-menu-list)))
  )


;;; VARIOUS AND MISCELLANEOUS

(let ((misc-menu-list '())
      (misc-menu (gtk_menu_item_new_with_label "Various"))
      (misc-cascade (gtk_menu_new)))
  (gtk_menu_shell_append (GTK_MENU_SHELL (main-menu effects-menu)) misc-menu)
  (gtk_widget_show misc-menu)
  (gtk_menu_item_set_submenu (GTK_MENU_ITEM misc-menu) misc-cascade)
  (g_signal_connect_closure_by_id 
   (GPOINTER misc-menu)
   (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT misc-menu))) 0
   (g_cclosure_new (lambda (w d) 
		     (update-label misc-menu-list))
		   #f #f)
   #f)

  ;; -------- Place sound

  (let ((child (gtk_menu_item_new_with_label "Place sound"))
	(mono-snd 0)
	(stereo-snd 1)
	(pan-pos 45)
	(place-sound-dialog #f)
	(place-sound-target 'sound)
	(place-sound-envelope #f))

    (define (place-sound mono-snd stereo-snd pan-env)
      "(place-sound mono-snd stereo-snd pan-env) mixes a mono sound into a stereo sound, splitting 
it into two copies whose amplitudes depend on the envelope 'pan-env'.  If 'pan-env' is 
a number, the sound is split such that 0 is all in channel 0 and 90 is all in channel 1."
      (let ((len (frames mono-snd)))
	(if (number? pan-env)
	    (let* ((pos (/ pan-env 90.0))
		   (reader0 (make-sample-reader 0 mono-snd))
		   (reader1 (make-sample-reader 0 mono-snd)))
	      (map-channel (lambda (y)
			     (+ y (* pos (read-sample reader1))))
			   0 len stereo-snd 1)
	      (map-channel (lambda (y)
			     (+ y (* (- 1.0 pos) (read-sample reader0))))
			   0 len stereo-snd 0))
	    (let ((e0 (make-env pan-env :end (1- len)))
		  (e1 (make-env pan-env :end (1- len)))
		  (reader0 (make-sample-reader 0 mono-snd))
		  (reader1 (make-sample-reader 0 mono-snd)))
	      (map-channel (lambda (y)
			     (+ y (* (env e1) (read-sample reader1))))
			   0 len stereo-snd 1)
	      (map-channel (lambda (y)
			     (+ y (* (- 1.0 (env e0)) (read-sample reader0))))
			   0 len stereo-snd 0)))))

    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not place-sound-dialog)
	    (let ((initial-mono-snd 0)
		  (initial-stereo-snd 1)
		  (initial-pan-pos 45)
		  (sliders '()))
	      (set! place-sound-dialog 
		    (make-effect-dialog
		     "Place sound"
		     (lambda (w data)
		       (let ((e (xe-envelope place-sound-envelope)))
			 (if (not (equal? e (list 0.0 1.0 1.0 1.0)))
			     (place-sound mono-snd stereo-snd e)
			     (place-sound mono-snd stereo-snd pan-pos))))
		     (lambda (w data)
		       (help-dialog "Place sound"
				    "Mixes mono sound into stereo sound field."))
		     (lambda (w data)
		       (set! mono-snd initial-mono-snd)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) mono-snd)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! stereo-snd initial-stereo-snd)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) stereo-snd)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! pan-pos initial-pan-pos)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 2))) pan-pos)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2))))))
	      (set! sliders
		    (add-sliders place-sound-dialog
				 (list (list "mono sound" 0 initial-mono-snd 50
					     (lambda (w data)
					       (set! mono-snd (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     1)
				       (list "stereo sound" 0 initial-stereo-snd 50
					     (lambda (w data)
					       (set! stereo-snd (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     1)
				       (list "pan position" 0 initial-pan-pos 90
					     (lambda (w data)
					       (set! pan-pos (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     1))))
	      (gtk_widget_show place-sound-dialog)
	      (set! place-sound-envelope (xe-create-enved "panning" 
							  (.vbox (GTK_DIALOG place-sound-dialog))
							  #f
							  '(0.0 1.0 0.0 1.0)))
	      (set! (xe-envelope place-sound-envelope) (list 0.0 1.0 1.0 1.0))))
	(activate-dialog place-sound-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Place sound (~1,2D ~1,2D ~1,2D)" mono-snd stereo-snd pan-pos)))
				   (change-label child new-label)))
			       misc-menu-list)))

  ;; -------- Insert silence (at cursor, silence-amount in secs)

  (let ((child (gtk_menu_item_new_with_label "Add silence"))
	(silence-amount 1.0)
	(silence-dialog #f))
    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not silence-dialog)
	    (let ((initial-silence-amount 1.0)
		  (sliders '()))
	      (set! silence-dialog
		    (make-effect-dialog
		     "Add silence"
		     (lambda (w data)
		       (insert-silence (cursor) (inexact->exact (floor (* (srate) silence-amount)))))
		     (lambda (w data)
		       (help-dialog "Add silence"
				    "Move the slider to change the number of seconds of silence added at the cursor position."))
		     (lambda (w data)
		       (set! silence-amount initial-silence-amount)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) silence-amount)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders silence-dialog
				 (list (list "silence" 0.0 initial-silence-amount 5.0
					     (lambda (w data)
					       (set! silence-amount (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100))))))
	(activate-dialog silence-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Add silence (~1,2F)" silence-amount)))
				   (change-label child new-label)))
			       misc-menu-list)))

;;; -------- Contrast (brightness control)
;;;

  (let ((child (gtk_menu_item_new_with_label "Contrast enhancement"))
	(contrast-amount 1.0)
	(contrast-dialog #f)
	(contrast-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not contrast-dialog)
	    (let ((initial-contrast-amount 1.0)
		  (sliders '()))
	      (set! contrast-dialog
		    (make-effect-dialog
		     "Contrast enhancement"
		     (lambda (w data) 
		       (let ((peak (maxamp))
			     (snd (selected-sound)))
			 (save-controls snd)
			 (reset-controls snd)
			 (set! (contrast-control? snd) #t)
			 (set! (contrast-control snd) contrast-amount)
			 (set! (contrast-control-amp snd) (/ 1.0 peak))
			 (set! (amp-control snd) peak)
			 (if (eq? contrast-target 'marks)
			     (let ((ms (plausible-mark-samples)))
			       (apply-controls snd 0 (car ms) (1+ (- (cadr ms) (car ms)))))
			     (apply-controls snd (if (eq? contrast-target 'sound) 0 2)))
			 (restore-controls snd)))
		     (lambda (w data)
		       (help-dialog "Contrast enhancement"
				    "Move the slider to change the contrast intensity."))
		     (lambda (w data)
		       (set! contrast-amount initial-contrast-amount)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) contrast-amount)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders contrast-dialog
				 (list (list "contrast enhancement" 0.0 initial-contrast-amount 10.0
					     (lambda (w data)
					       (set! contrast-amount (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG contrast-dialog))
			  (lambda (target) (set! contrast-target target))
			  #f)))
	(activate-dialog contrast-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Contrast enhancement (~1,2F)" contrast-amount)))
				   (change-label child new-label)))
			       misc-menu-list)))

  ;; -------- Cross synthesis

  (let ((child (gtk_menu_item_new_with_label "Cross synthesis"))
	(cross-synth-sound 1)
	(cross-synth-amp .5)
	(cross-synth-fft-size 128)
	(cross-synth-radius 6.0)
	(cross-synth-dialog #f)
	(cross-synth-default-fft-widget #f)
	(cross-synth-target 'sound))

    (define (cross-synthesis cross-snd amp fftsize r)
      ;; cross-snd is the index of the other sound (as opposed to the map-chan sound)
      (let* ((freq-inc (/ fftsize 2))
	     (fdr (make-vct fftsize))
	     (fdi (make-vct fftsize))
	     (spectr (make-vct freq-inc))
	     (inctr 0)
	     (ctr freq-inc)
	     (radius (- 1.0 (/ r fftsize)))
	     (bin (/ (srate) fftsize))
	     (formants (make-vector freq-inc)))
	(do ((i 0 (1+ i)))
	    ((= i freq-inc))
	  (vector-set! formants i (make-formant radius (* i bin))))
	(lambda (inval)
	  (let ((outval 0.0))
	    (if (= ctr freq-inc)
		(begin
		  (set! fdr (channel->vct inctr fftsize cross-snd 0))
		  (set! inctr (+ inctr freq-inc))
		  (spectrum fdr fdi #f 2)
		  (vct-subtract! fdr spectr)
		  (vct-scale! fdr (/ 1.0 freq-inc))
		  (set! ctr 0)))
	    (set! ctr (+ ctr 1))
	    (vct-add! spectr fdr)
	    (* amp (formant-bank spectr formants inval))))))

    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not cross-synth-dialog)
	    (let ((initial-cross-synth-sound 1)
		  (initial-cross-synth-amp .5)
		  (initial-cross-synth-fft-size 128)
		  (initial-cross-synth-radius 6.0)
		  (sliders '()))
	      (set! cross-synth-dialog
		    (make-effect-dialog
		     "Cross synthesis"
		     (lambda (w data)
		       (map-chan-over-target-with-sync
			(lambda (ignored) 
			  (cross-synthesis cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius))
			cross-synth-target "Cross synthesis" #f))
		     (lambda (w data)
		       (help-dialog "Cross synthesis"
				    "The sliders set the number of the soundfile to be cross-synthesized, 
the synthesis amplitude, the FFT size, and the radius value."))
		     (lambda (w data)
		       (set! cross-synth-sound initial-cross-synth-sound)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 0))) cross-synth-sound)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! cross-synth-amp initial-cross-synth-amp)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 1))) cross-synth-amp)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! cross-synth-fft-size initial-cross-synth-fft-size)
		       (set! cross-synth-radius initial-cross-synth-radius)
		       (set! (.value (GTK_ADJUSTMENT (list-ref sliders 2))) cross-synth-radius)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2))))))
	      (set! sliders
		    (add-sliders cross-synth-dialog
				 (list (list "input sound" 0 initial-cross-synth-sound 20
					     (lambda (w data)
					       (set! cross-synth-sound (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     1)
				       (list "amplitude" 0.0 initial-cross-synth-amp 1.0
					     (lambda (w data)
					       (set! cross-synth-amp (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100)
				       (list "radius" 0.0 initial-cross-synth-radius 360.0
					     (lambda (w data)
					       (set! cross-synth-radius (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG cross-synth-dialog)) 
			  (lambda (target) (set! cross-synth-target target)) 
			  #f)))
	(activate-dialog cross-synth-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Cross synthesis (~1,2D ~1,2F ~1,2D ~1,2F)" 
							  cross-synth-sound cross-synth-amp cross-synth-fft-size cross-synth-radius)))
				   (change-label child new-label)))
			       misc-menu-list)))

  ;; -------- Flange and phasing

  (let ((child (gtk_menu_item_new_with_label "Flange"))
	(flange-speed 2.0)
	(flange-amount 5.0)
	(flange-time 0.001)
	(flange-dialog #f)
	(flange-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not flange-dialog)
	    (let ((initial-flange-speed 2.0)
		  (initial-flange-amount 5.0)
		  (initial-flange-time 0.001)
		  (sliders '()))
	      (set! flange-dialog
		    (make-effect-dialog
		     "Flange"
		     (lambda (w data)
		       (map-chan-over-target-with-sync 
			(lambda (ignored)
			  (let* ((ri (make-rand-interp :frequency flange-speed :amplitude flange-amount))
				 (len (inexact->exact (round (* flange-time (srate)))))
				 (del (make-delay len :max-size (+ len flange-amount 1))))
			    (lambda (inval)
			      (* .75 (+ inval
					(delay del
					       inval
					       (rand-interp ri)))))))
			flange-target "flange" #f))
		     (lambda (w data)
		       (help-dialog "Flange"
				    "Move the sliders to change the flange speed, amount, and time"))
		     (lambda (w data)
		       (set! flange-speed initial-flange-speed)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) flange-speed)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! flange-amount initial-flange-amount)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) flange-amount)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! flange-time initial-flange-time)
		       (set! (.value (GTK_ADJUSTMENT (caddr sliders))) flange-time)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2))))))
	      (set! sliders
		    (add-sliders flange-dialog
				 (list (list "flange speed" 0.0 initial-flange-speed 100.0
					     (lambda (w data)
					       (set! flange-speed (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     10)
				       (list "flange amount" 0.0 initial-flange-amount 100.0
					     (lambda (w data)
					       (set! flange-amount (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     10)
				       ;; flange time ought to use a non-linear scale (similar to amp in control panel)
				       (list "flange time" 0.0 initial-flange-time 1.0
					     (lambda (w data)
					       (set! flange-time (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG flange-dialog)) 
			  (lambda (target) (set! flange-target target)) 
			  #f)))
	(activate-dialog flange-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Flange (~1,2F ~1,2F ~1,3F)" flange-speed flange-amount flange-time)))
				   (change-label child new-label)))
			       misc-menu-list)))

  ;; -------- Randomize phase

  (let ((child (gtk_menu_item_new_with_label "Randomize phase"))
	(random-phase-amp-scaler 3.14)
	(random-phase-dialog #f))
    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not random-phase-dialog)
	    (let ((initial-random-phase-amp-scaler 3.14)
		  (sliders '()))
	      (set! random-phase-dialog
		    (make-effect-dialog
		     "Randomize phase"
		     (lambda (w data)
		       (rotate-phase (lambda (x) (random random-phase-amp-scaler))))
		     (lambda (w data)
		       (help-dialog "Randomize phase"
				    "Move the slider to change the randomization amplitude scaler."))
		     (lambda (w data)
		       (set! random-phase-amp-scaler initial-random-phase-amp-scaler)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) random-phase-amp-scaler)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders random-phase-dialog
				 (list (list "amplitude scaler" 0.0 initial-random-phase-amp-scaler 100.0
					     (lambda (w data)
					       (set! random-phase-amp-scaler (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100))))))
	(activate-dialog random-phase-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Randomize phase (~1,2F)"  random-phase-amp-scaler)))
				   (change-label child new-label)))
			       misc-menu-list)))

  ;; -------- Robotize

  (let ((child (gtk_menu_item_new_with_label "Robotize"))
	(samp-rate 1.0)
	(osc-amp 0.3)
	(osc-freq 20)
	(robotize-dialog #f)
	(robotize-target 'sound))

    (define fp-1 ; fp from examp.scm with added beg end args
      (lambda (sr osamp osfrq beg end)
	(let* ((os (make-oscil osfrq))
               (sr (make-src :srate sr))
               (len (1+ (- end beg)))
               (sf (make-sample-reader beg))
               (out-data (make-vct len)))
          (vct-map! out-data
                    (lambda ()
                      (src sr (* osamp (oscil os))
                           (lambda (dir)
                             (if (> dir 0)
                                 (next-sample sf)
                                 (previous-sample sf))))))
          (free-sample-reader sf)
          (vct->channel out-data beg len))))

    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not robotize-dialog)
	    (let ((initial-samp-rate 1.0)
		  (initial-osc-amp 0.3)
		  (initial-osc-freq 20)
		  (sliders '()))
	      (set! robotize-dialog
		    (make-effect-dialog
		     "Robotize"
		     (lambda (w data)
		       (let ((ms (and (eq? robotize-target 'marks)
				      (plausible-mark-samples))))
			 (fp-1 samp-rate osc-amp osc-freq
			       (if (eq? robotize-target 'sound)
				   0
				   (if (eq? robotize-target 'selection)
				       (selection-position)
				       (car ms)))
			       (if (eq? robotize-target 'sound)
				   (1- (frames))
				   (if (eq? robotize-target 'selection)
				       (+ (selection-position) (selection-frames))
				       (cadr ms))))))
		     (lambda (w data)
		       (help-dialog "Robotize"
				    "Move the sliders to set the sample rate, oscillator amplitude, and oscillator frequency."))
		     (lambda (w data)
		       (set! samp-rate initial-samp-rate)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) samp-rate)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! osc-amp initial-osc-amp)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) osc-amp)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1)))
		       (set! osc-freq initial-osc-freq)
		       (set! (.value (GTK_ADJUSTMENT (caddr sliders))) osc-freq)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 2))))))
	      (set! sliders
		    (add-sliders robotize-dialog
				 (list (list "sample rate" 0.0 initial-samp-rate 2.0
					     (lambda (w data)
					       (set! samp-rate (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "oscillator amplitude" 0.0 initial-osc-amp 1.0
					     (lambda (w data)
					       (set! osc-amp (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100)
				       (list "oscillator frequency" 0.0 initial-osc-freq 60
					     (lambda (w data)
					       (set! osc-freq (.value (GTK_ADJUSTMENT (list-ref sliders 2)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG robotize-dialog)) 
			  (lambda (target) (set! robotize-target target)) 
			  #f)))
	(activate-dialog robotize-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Robotize (~1,2F ~1,2F ~1,2F)" samp-rate osc-amp osc-freq)))
				   (change-label child new-label)))
			       misc-menu-list)))

  ;; -------- Rubber sound

  (let ((child (gtk_menu_item_new_with_label "Rubber sound"))
	(rubber-factor 1.0)
	(rubber-dialog #f)
	(rubber-target 'sound))
    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not rubber-dialog)
	    (let ((initial-rubber-factor 1.0)
		  (sliders '()))
	      (set! rubber-dialog
		    (make-effect-dialog
		     "Rubber sound"
		     (lambda (w data) 
		       (rubber-sound rubber-factor))
		     (lambda (w data)
		       (help-dialog "Rubber sound"
				    "Stretches or contracts the time of a sound. Move the slider to change the stretch factor."))
		     (lambda (w data)
		       (set! rubber-factor initial-rubber-factor)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) rubber-factor)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0))))))
	      (set! sliders
		    (add-sliders rubber-dialog
				 (list (list "stretch factor" 0.0 initial-rubber-factor 5.0
					     (lambda (w data)
					       (set! rubber-factor (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG rubber-dialog)) 
			  (lambda (target) (set! rubber-target target)) 
			  #f)))
	(activate-dialog rubber-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Rubber sound (~1,2F)"  rubber-factor)))
				   (change-label child new-label)))
			       misc-menu-list)))

  ;; -------- Wobble

  (let ((child (gtk_menu_item_new_with_label "Wobble"))
	(wobble-frequency 50)
	(wobble-amplitude 0.5)
	(wobble-dialog #f)
	(wobble-target 'sound))

    (define hello-dentist-1 ; hello-dentist with added beg and end args
      (lambda (frq amp beg end)
	(let* ((rn (make-rand-interp :frequency frq :amplitude amp))
	       (i 0)
	       (j 0)
	       (len (1+ (- end beg)))
	       (in-data (channel->vct beg len))
	       (out-len (inexact->exact (round (* len (+ 1.0 (* 2 amp))))))
	       (out-data (make-vct out-len))
	       (rd (make-src :srate 1.0
			     :input (lambda (dir)
				      (let ((val (if (and (>= i 0) (< i len))
						     (vct-ref in-data i)
						     0.0)))
					(set! i (+ i dir))
					val)))))
	  (do ()
	      ((or (= i len) (= j out-len)))
	    (vct-set! out-data j (src rd (rand-interp rn)))
	    (set! j (+ j 1)))
	  (vct->channel out-data beg j))))

    (gtk_menu_shell_append (GTK_MENU_SHELL misc-cascade) child)
    (gtk_widget_show child)
    (g_signal_connect_closure_by_id 
     (GPOINTER child)
     (g_signal_lookup "activate" (G_OBJECT_TYPE (GTK_OBJECT child))) 0
     (g_cclosure_new 
      (lambda (w d) 
	(if (not wobble-dialog)
	    (let ((initial-wobble-frequency 50)
		  (initial-wobble-amplitude 0.5)
		  (sliders '()))
	      (set! wobble-dialog
		    (make-effect-dialog
		     "Wobble"
		     (lambda (w data)
		       (let ((ms (and (eq? wobble-target 'marks)
				      (plausible-mark-samples))))
			 (hello-dentist-1
			  wobble-frequency wobble-amplitude
			  (if (eq? wobble-target 'sound)
			      0
			      (if (eq? wobble-target 'selection)
				  (selection-position)
				  (car ms)))
			  (if (eq? wobble-target 'sound)
			      (1- (frames))
			      (if (eq? wobble-target 'selection)
				  (+ (selection-position) (selection-frames))
				  (cadr ms))))))
		     (lambda (w data)
		       (help-dialog "Wobble"
				    "Move the sliders to set the wobble frequency and amplitude."))
		     (lambda (w data)
		       (set! wobble-frequency initial-wobble-frequency)
		       (set! (.value (GTK_ADJUSTMENT (car sliders))) wobble-frequency)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 0)))
		       (set! wobble-amplitude initial-wobble-amplitude)
		       (set! (.value (GTK_ADJUSTMENT (cadr sliders))) wobble-amplitude)
		       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (list-ref sliders 1))))))
	      (set! sliders
		    (add-sliders wobble-dialog
				 (list (list "wobble frequency" 0 initial-wobble-frequency 100
					     (lambda (w data)
					       (set! wobble-frequency (.value (GTK_ADJUSTMENT (list-ref sliders 0)))))
					     100)
				       (list "wobble amplitude" 0.0 initial-wobble-amplitude 1.0
					     (lambda (w data)
					       (set! wobble-amplitude (.value (GTK_ADJUSTMENT (list-ref sliders 1)))))
					     100))))
	      (add-target (.vbox (GTK_DIALOG wobble-dialog)) 
			  (lambda (target) (set! wobble-target target)) 
			  #f)))
	(activate-dialog wobble-dialog))
      #f #f)
     #f)
    (set! misc-menu-list (cons (lambda ()
				 (let ((new-label (format #f "Wobble (~1,2F ~1,2F)" wobble-frequency wobble-amplitude)))
				   (change-label child new-label)))
			       misc-menu-list)))
  )

;;;
;;; END PARAMETRIZED EFFECTS
;;;

(add-to-menu effects-menu #f #f)
(add-to-menu effects-menu "Octave-down" (lambda () (down-oct 2)))
(add-to-menu effects-menu "Remove clicks"
	     (lambda ()
	       (define (find-click loc)
		 (let ((reader (make-sample-reader loc))
		       (samp0 0.0)
		       (samp1 0.0)
		       (samp2 0.0)
		       (samps (make-vct 10))
		       (samps-ctr 0)
		       (diff 1.0)
		       (len (frames)))
		   (call-with-current-continuation
		    (lambda (return)
		      (do ((ctr loc (1+ ctr)))
			  ((or (c-g?) (= ctr len)) #f)
			(set! samp0 samp1)
			(set! samp1 samp2)
			(set! samp2 (next-sample reader))
			(vct-set! samps samps-ctr samp0)
			(if (< samps-ctr 9)
			    (set! samps-ctr (+ samps-ctr 1))
			    (set! samps-ctr 0))
			(let ((local-max (max .1 (vct-peak samps))))
			  (if (and (> (abs (- samp0 samp1)) local-max)
				   (> (abs (- samp1 samp2)) local-max)
				   (< (abs (- samp0 samp2)) (/ local-max 2)))
			      (return (1- ctr)))))))))

	       (define (remove-click loc)
		 (let ((click (find-click loc)))
		   (if (and click (not (c-g?)))
		       (begin
			 (smooth-sound (- click 2) 4)
			 (remove-click (+ click 2))))))
	       (remove-click 0)))
(add-to-menu effects-menu "Remove DC" (lambda () 
					(map-chan
					 (let ((lastx 0.0)
					       (lasty 0.0))
					   (lambda (inval)
					     (set! lasty (+ inval (- (* 0.999 lasty) lastx)))
					     (set! lastx inval)
					     lasty)))))
(add-to-menu effects-menu "Spiker" spike)
(add-to-menu effects-menu "Compand" (lambda () (map-chan 
						(let* ((tbl (vct -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250
								 0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000)))
						  (lambda (inval)
						    (let ((index (+ 8.0 (* 8.0 inval))))
						      (array-interp tbl index 17)))))))
(add-to-menu effects-menu "Invert" (lambda () (scale-by -1)))
(add-to-menu effects-menu "Reverse" (lambda () (reverse-sound)))
(add-to-menu effects-menu "Null phase" (lambda () (zero-phase)))
