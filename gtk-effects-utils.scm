(if (not (provided? 'snd-gtk)) (snd-error "gtk-effects-utils.scm is Gtk-specific"))

(provide 'snd-gtk-effects-utils.scm)

(if (not (defined? 'all-chans))
    (define (all-chans)
      (let ((sndlist ())
	    (chnlist ()))
	(for-each (lambda (snd)
		    (do ((i (- (channels snd) 1) (- i 1)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))

(define (update-label effects)
  (if (pair? effects)
      (begin
	((car effects))
	(update-label (cdr effects)))))

(define (effect-target-ok target)
  (if (eq? target 'sound) 
      (pair? (sounds))
      (if (eq? target 'selection) 
	  (selection?)
	  (and (selected-sound)
	       (>= (length (marks (selected-sound) (selected-channel))) 2)))))

(define* (make-effect-dialog label ok-callback help-callback reset-callback target-ok-callback)
  ;; make a standard dialog
  ;; callbacks take 2 args: widget data
  (let ((dismiss-button (gtk_button_new_with_label "Go Away"))
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
    (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG new-dialog))) dismiss-button #t #t 20)
    (g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide new-dialog)) #f)
    (gtk_widget_show dismiss-button)
    (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG new-dialog))) ok-button #t #t 20)
    (g_signal_connect ok-button "clicked" ok-callback #f)
    (gtk_widget_show ok-button)
    (if reset-button
	(begin
	  (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG new-dialog))) reset-button #t #t 20)
	  (g_signal_connect reset-button "clicked" reset-callback #f)
	  (gtk_widget_show reset-button)))
    (gtk_box_pack_end (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG new-dialog))) help-button #t #t 20)
    (g_signal_connect help-button "clicked" help-callback #f)
    (gtk_widget_show help-button)
    ;; build rest in (gtk_dialog_get_content_area (GTK_DIALOG new-dialog))

    (if target-ok-callback
	(begin
	  (gtk_widget_set_sensitive ok-button (target-ok-callback))
	  (hook-push effects-hook
		     (lambda (hook) (gtk_widget_set_sensitive ok-button (target-ok-callback)))))
	(begin
	  (gtk_widget_set_sensitive ok-button (pair? (sounds)))
	  (hook-push effects-hook
		     (lambda (hook) (gtk_widget_set_sensitive ok-button (pair? (sounds)))))))

    (g_object_set_data (G_OBJECT new-dialog) "ok-button" (GPOINTER ok-button))
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
  (let (;; using log 2 here to get equally spaced octaves
	(log-lo (log (max lo 1.0) 2))
	(log-hi (log hi 2))
	(log-val (log val 2)))
    (floor (* log-scale-ticks (/ (- log-val log-lo) (- log-hi log-lo))))))
  
(define (scale-linear->log lo val hi)
  ;; given user-relative lo..hi and scale-relative val, return user-relative val
  ;; since log-scale widget assumes 0..log-scale-ticks, val can be used as ratio (log-wise) between lo and hi
  (let* ((log-lo (log (max lo 1.0) 2))
	 (log-hi (log hi 2))
	 (log-val (+ log-lo (* (/ val log-scale-ticks) (- log-hi log-lo)))))
    (expt 2.0 log-val)))

(define (scale-log-label lo val hi)
  (format #f "~,2F" (scale-linear->log lo val hi)))
	  
(define (add-sliders dialog sliders)
  ;; sliders is a list of lists, each inner list being (title low initial high callback scale ['log])
  ;; returns list of widgets (for reset callbacks)
  (let* ((mainform (gtk_box_new GTK_ORIENTATION_VERTICAL 2))
	 (use-hbox (= (length sliders) 1))
	 (table (if (not use-hbox) (gtk_grid_new)))
	 (slider 0))
    (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG dialog))) mainform #f #f 4)
    (gtk_widget_show mainform)
    (if (not use-hbox)
	(begin
	  (gtk_box_pack_start (GTK_BOX mainform) table #f #f 4)
	  (gtk_grid_set_row_spacing (GTK_GRID table) 4)
	  (gtk_grid_set_column_spacing (GTK_GRID table) 4)
	  (gtk_widget_show table)))
    (map 
     (lambda (slider-data)
       (let* ((title (slider-data 0))
	      (low (slider-data 1))
	      (initial (slider-data 2))
	      (high (slider-data 3))
	      (func (slider-data 4))
	      (scaler (slider-data 5))
	      (use-log (and (= (length slider-data) 7)
			    (eq? (slider-data 6) 'log)))
	      (hbox (and use-hbox (gtk_box_new GTK_ORIENTATION_HORIZONTAL 0)))
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
	      (scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT adj))))
	 (if use-hbox
	     (begin
	       (gtk_box_pack_start (GTK_BOX mainform) hbox #f #f 2)
	       (gtk_widget_show hbox)
	       (gtk_box_pack_start (GTK_BOX hbox) label #f #f 6))
	     (gtk_grid_attach (GTK_GRID table) label 0 slider 1 1)
	     ;(gtk_table_attach (GTK_TABLE table) label 0 1 slider (+ 1 slider) (logior GTK_FILL GTK_SHRINK) (logior GTK_FILL GTK_SHRINK) 0 0)
	     )
	 (gtk_widget_show label)
	 (if (not (provided? 'gtk3)) (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE scale)) GTK_UPDATE_CONTINUOUS))
	 (gtk_scale_set_digits (GTK_SCALE scale)
			       (if use-log
				   0
				   (if (= scaler 1000) 3 (if (= scaler 100) 2 (if (= scaler 10) 1 0)))))
	 (gtk_scale_set_draw_value (GTK_SCALE scale) (not use-log))
	 (if use-hbox
	     (gtk_box_pack_start (GTK_BOX hbox) scale #t #t 0)
	     (begin
	       (gtk_widget_set_hexpand (GTK_WIDGET scale) #t)
	       (gtk_grid_attach (GTK_GRID table) scale 1 slider 1 1)
	       ;(gtk_table_attach (GTK_TABLE table) scale 1 2 slider (+ 1 slider) (logior GTK_FILL GTK_EXPAND GTK_SHRINK) (logior GTK_FILL GTK_EXPAND GTK_SHRINK) 0 0)
	       (set! slider (+ 1 slider))))
	 (gtk_widget_show scale)
	 (if use-log
	     (g_signal_connect adj "value_changed"
			       (lambda (w d) 
				 (func w d)
				 (change-label label 
					       (format #f "~A: ~,2F" 
						       title 
						       (scale-linear->log low (gtk_adjustment_get_value (GTK_ADJUSTMENT adj)) high))))
			       #f)
	     (g_signal_connect adj "value_changed" func #f))
	 adj))
     sliders)))

(define (activate-dialog w)
  (gtk_widget_show w)
  (gtk_window_present (GTK_WINDOW w)))

