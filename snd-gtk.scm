;;; translations from snd-motif.scm
;;;
;;; display-scanned-synthesis
;;; show-smpte-label
;;; mark-sync-color
;;; zync and unzync
;;; disable control panel
;;; hidden controls panel
;;; show-disk-space
;;; remove top level menu
;;; keep-file-dialog-open-upon-ok
;;; [snd-clock-icon]
;;; add "tooltip" to a widget


(use-modules (ice-9 format))

(if (not (provided? 'xg))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-gtk.scm needs the xg module: ~A" hxm))
	  (dlinit hxm "init_xm"))))


(define (host-name)
  "(host-name) -> name of current machine"
  (let ((val (gdk_property_get (car (main-widgets))
			       (gdk_atom_intern "WM_CLIENT_MACHINE" #f)
			       GDK_TARGET_STRING 0 1024 0)))
    ;; val is list: (success atom element-size length unterminated-string)
    (and (car val)
	 (substring (list-ref val 4) 0 (list-ref val 3)))))



;;; -------- display-scanned-synthesis --------
;;;
;;; open a new main pane below the listener, with two sections
;;;  on the left various controls, on the right a graph
;;;  push 'start' to start the scanned synthesis display
;;;  if spring > mass, you'll get overflows
;;;
;;; TODO: add audio output

(define scanned-synthesis-pane #f)

(define (display-scanned-synthesis)

  (define compute-uniform-circular-string
    ;; copied from dsp.scm to simplify life
    (lambda (size x0 x1 x2 mass xspring damp)
      (define circle-vct-ref 
	(lambda (v i)
	  (if (< i 0)
	      (vct-ref v (+ size i))
	      (if (>= i size)
		  (vct-ref v (- i size))
		  (vct-ref v i)))))
      (let* ((dm (/ damp mass))
	     (km (/ xspring mass))
	     (denom (+ 1.0 dm))
	     (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	     (p2 (/ km denom))
	     (p3 (/ -1.0 denom)))
	(do ((i 0 (1+ i)))
	    ((= i size))
	  (vct-set! x0 i (min (+ (* p1 (vct-ref x1 i))
				 (* p2 (+ (circle-vct-ref x1 (- i 1)) (circle-vct-ref x1 (+ i 1))))
				 (* p3 (vct-ref x2 i)))
			      1000.0)))
	(vct-fill! x2 0.0)
	(vct-add! x2 x1)
	(vct-fill! x1 0.0)
	(vct-add! x1 x0))))

  (let* ((pi 3.141592653589793) ; just for now?
	 (mass 1.0)
	 (xspring 0.1)
	 (damp 0.0)
	 (bounds '())
	 (pts0 #f)
	 (pts1 #f)
	 (ax0 0) (ax1 0) (ay0 0) (ay1 0)
	 (gc (car (snd-gcs)))
	 (egc (list-ref (snd-gcs) 7))

	 ;; now set up a paned window in the main Snd window with controllers on the left and the graph on the right
	 (scan-outer (let ((pane (gtk_hbox_new #f 0)))
		       (gtk_box_pack_start (GTK_BOX (list-ref (main-widgets) 5)) pane #f #f 4)
		       (gtk_widget_show pane)
		       pane))
	 (scan-row (let ((box (gtk_vbox_new #f 0)))
		     (gtk_box_pack_start (GTK_BOX scan-outer) box #f #f 0)
		     (gtk_widget_show box)
		     box))
	 ;; the graph
	 (scan-pane (let ((grf (gtk_drawing_area_new)))
		      (gtk_widget_set_events grf GDK_ALL_EVENTS_MASK)
		      (gtk_box_pack_start (GTK_BOX scan-outer) grf #t #t 0)
		      (gtk_widget_show grf)
		      (gdk_window_set_background (.window grf) (graph-color))
		      grf))
	 ;; the controllers
	 (scan-start (let ((label (gtk_button_new_with_label "Start")))
		       (gtk_box_pack_start (GTK_BOX scan-row) label #t #t 0)
		       (gtk_widget_show label)
		       label))
	 (scan-continue (let ((label (gtk_button_new_with_label "Continue")))
			  (gtk_box_pack_start (GTK_BOX scan-row) label #t #t 0)
			  (gtk_widget_show label)
			  label))
	 (scan-stop (let ((label (gtk_button_new_with_label "Stop")))
		      (gtk_box_pack_start (GTK_BOX scan-row) label #t #t 0)
		      (gtk_widget_show label)
		      label))
	 (size 128)
	 (gx0 (make-vct size))	   
	 (gx1 (make-vct size))	   
	 (gx2 (make-vct size))
	 (vect (make-vector (* 2 size)))
	 (work-proc #f))

    (define (y->grfy y range)
      (min ay1
	   (max ay0
		(inexact->exact
		 (+ ay0
		    (* range (- 10.0 y)))))))

    (define (draw-graph)
      (if (and (> ax1 ax0)
	       (> ay1 ay0))
	  (let ((diff (* 0.05 (- ay1 ay0))) ; assuming -10 to 10 
		(wn (list 'GdkDrawable_ (cadr (.window scan-pane))))
		(xincr (/ (- ax1 ax0) size)))
	    (if pts1
		(gdk_draw_lines wn egc (list 'GdkPoint_ pts1) size)
		(gdk_draw_rectangle wn egc #t
				    (+ ax0 2)
				    ay0
				    (- ax1 ax0 2)
				    (- ay1 ay0)))
	    (do ((i 0 (1+ i))
		 (j 0 (+ j 2))
		 (xi ax0 (+ xi xincr)))
		((= i size))
	      (vector-set! vect j (inexact->exact xi))
	      (vector-set! vect (+ j 1) (y->grfy (vct-ref gx0 i) diff)))
	    (if pts1 (freeGdkPoints pts1))
	    (set! pts0 (vector->GdkPoints vect))
	    (set! pts1 pts0)
	    (gdk_draw_lines wn gc (list 'GdkPoint_ pts0) size))))

    (define (redraw-graph)
      (set! bounds (draw-axes scan-pane gc "scanned synthesis" 0.0 1.0 -10.0 10.0))
      (set! ax0 (+ (car bounds) 2))
      (set! ax1 (caddr bounds))
      (set! ay1 (cadr bounds))
      (set! ay0 (cadddr bounds))
      (draw-graph))

    (define (tick-synthesis n)
      ;; background process
      (compute-uniform-circular-string size gx0 gx1 gx2 mass xspring damp)
      (draw-graph)
      #t)

    (define (stop-synthesis)
      (if work-proc
	  (gtk_idle_remove work-proc))
      (set! work-proc #f))

    (define (start-synthesis)
      (stop-synthesis)
      (vct-fill! gx0 0.0)
      (vct-fill! gx1 0.0)
      (vct-fill! gx2 0.0)
      (do ((i 0 (1+ i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (vct-set! gx1 (+ i (- (/ size 4) 6)) val)))
      (set! work-proc (gtk_idle_add tick-synthesis #f)))

    (define (continue-synthesis)
      (stop-synthesis)
      (set! work-proc (gtk_idle_add tick-synthesis #f)))

    ;; controller callbacks
    (for-each 
     (lambda (data)
       (let* ((title (list-ref data 0))
	      (minval (list-ref data 1))
	      (maxval (list-ref data 2))
	      (curval (list-ref data 3))
	      (decimals (list-ref data 4))
	      (func (list-ref data 5))
	      (adj (gtk_adjustment_new curval minval maxval 1.0 10.0 1.0))
	      (scale (gtk_hscale_new (GTK_ADJUSTMENT adj)))
	      (label (gtk_label_new title)))
	 (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE scale)) GTK_UPDATE_CONTINUOUS)
	 (gtk_scale_set_digits (GTK_SCALE scale) decimals)
	 (gtk_scale_set_value_pos (GTK_SCALE scale) GTK_POS_TOP)
	 (gtk_scale_set_draw_value (GTK_SCALE scale) #t)
      	 (gtk_box_pack_start (GTK_BOX scan-row) scale #t #t 0)
	 (gtk_widget_show scale)
	 (gtk_box_pack_start (GTK_BOX scan-row) label #t #t 0)
	 (gtk_widget_show label)
	 (g_signal_connect_closure_by_id (list 'gpointer (cadr adj))
					 (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj)))
					 0
					 (g_cclosure_new (lambda (w d) (func (.value (GTK_ADJUSTMENT adj)))) #f #f) #f)))
     (list (list "mass" 1 200 100 2 (lambda (val) (set! mass (/ val 100.0))))
	   (list "spring" 1 100 10 2 (lambda (val) (set! xspring (/ val 100.0))))
	   (list "damping" 0 100 0 4 (lambda (val) (set! damp (/ val 10000.0))))))

    (let* ((scan-size (gtk_hbox_new #f 4)))
      (gtk_box_pack_start (GTK_BOX scan-row) scan-size #t #t 6)
      (gtk_widget_show scan-size)
      (let ((scan-label (gtk_label_new "Size:")))
	(gtk_box_pack_start (GTK_BOX scan-size) scan-label #f #f 10)
	(gtk_widget_show scan-label)
	(let ((scan-text (gtk_entry_new)))
	  (gtk_box_pack_start (GTK_BOX scan-size) scan-text #t #t 0)
	  (gtk_widget_show scan-text)
	  (gtk_entry_set_text (GTK_ENTRY scan-text) (number->string size))
	  (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-text))
					  (g_signal_lookup "activate" 
							   (G_OBJECT_TYPE (GTK_OBJECT scan-text)))
					  0
					  (g_cclosure_new (lambda (w d) 
							    (stop-synthesis)
							    (set! size (string->number (gtk_entry_get_text (GTK_ENTRY scan-text))))
							    (set! gx0 (make-vct size))	   
							    (set! gx1 (make-vct size))	   
							    (set! gx2 (make-vct size))
							    (set! vect (make-vector (* size 2))))
							  #f #f) #f))))

    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-pane))
				    (g_signal_lookup "expose_event" (G_OBJECT_TYPE (GTK_OBJECT scan-pane)))
				    0 (g_cclosure_new (lambda (w e d) (redraw-graph)) #f #f) #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-pane))
				    (g_signal_lookup "configure_event" (G_OBJECT_TYPE (GTK_OBJECT scan-pane)))
				    0 (g_cclosure_new (lambda (w e d) (redraw-graph)) #f #f) #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-pane))
				    (g_signal_lookup "button_press_event" (G_OBJECT_TYPE (GTK_OBJECT scan-pane)))
				    0
				    (g_cclosure_new (lambda (w e d) 
						      (let ((button (.button (list 'GdkEventButton_ (cadr e)))))
							(if (not work-proc)
							    (if (= button 2)
								(continue-synthesis)
								(start-synthesis))
							    (stop-synthesis))))
						    #f #f) #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-start))
				    (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT scan-start))) 0
				    (g_cclosure_new (lambda (w d) (start-synthesis)) #f #f) #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-continue))
				    (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT scan-continue))) 0
				    (g_cclosure_new (lambda (w d) (continue-synthesis)) #f #f) #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-stop))
				    (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT scan-stop))) 0
				    (g_cclosure_new (lambda (w d) (stop-synthesis)) #f #f) #f)
    #t))


(define (close-scanned-synthesis-pane)
  (gtk_widget_hide scanned-synthesis-pane))



;;; -------- show-smpte-label
;;;
;;; (show-smpte-label #:optional on-or-off)
;;;   turns on/off a label in the time-domain graph showing the current smpte frame of the leftmost sample

(define smpte-frames-per-second 24.0)

(define get-text-width-and-height
  (let ((smpte-font-wh #f)
	(smpte-font-name ""))
    (lambda (text fs)
      (if (or (not smpte-font-wh)
	      (not (string=? smpte-font-name (axis-numbers-font))))
	  (let ((layout (pango_layout_new (gdk_pango_context_get))))
	    (set! smpte-font-name (axis-numbers-font))
	    (if layout
		(begin
		  (pango_layout_set_font_description layout fs)
		  (pango_layout_set_text layout text -1)
		  (let ((wid (pango_layout_get_pixel_size layout #f)))
		    (g_object_unref (list 'gpointer (cadr layout)))
		    (set! smpte-font-wh wid)
		    wid))
		#f))
	  smpte-font-wh))))

(define draw-smpte-label

  (let* ((fs (pango_font_description_from_string (axis-numbers-font)))
	 (wh (get-text-width-and-height "00:00:00:00" fs))
	 (width (+ 8 (car wh)))
	 (height (+ 8 (cadr wh))))

    (define (smpte-label samp sr)
      (define (round-down val) (inexact->exact (truncate val)))
      (let* ((seconds (/ samp sr))
	     (frames (* seconds smpte-frames-per-second))
	     (minutes (round-down (/ seconds 60)))
	     (hours (round-down (/ minutes 60))))
	(format #f "~2,'0D:~2,'0D:~2,'0D:~2,'0D"
		hours
		(- minutes (* hours 60))
		(round-down (- seconds (* minutes 60)))
		(round-down (- frames (* (round-down seconds) smpte-frames-per-second))))))
	    
    (lambda (snd chn)
      (let* ((axinf (axis-info snd chn))
	     (x (list-ref axinf 10))
	     (y (list-ref axinf 13))
	     (grf-width (- (list-ref axinf 12) x))
	     (grf-height (- (list-ref axinf 11) y)))
	(if (and (> grf-height (* 2 height))
		 (> grf-width (* 1.5 width))
		 (time-graph? snd chn))
	    (let* ((smpte (smpte-label (car axinf) (srate snd)))
		   (samp (car axinf)))
	      (fill-rectangle x y width 2 snd chn)
	      (fill-rectangle x (+ y height) width 2 snd chn)
	      (fill-rectangle x y 2 height snd chn)
	      (fill-rectangle (+ x width -2) y 2 height snd chn)
	      (set! (current-font snd chn) (cadr fs))
	      (draw-string smpte (+ x 4) (+ y 4) snd chn)))))))

(define show-smpte-label
  (lambda arg
    (if (or (null? arg)
	    (car arg))
      (if (not (member draw-smpte-label (hook->list after-graph-hook)))
	  (begin
	    (add-hook! after-graph-hook draw-smpte-label)
	    (update-time-graph #t #t)))
      (begin
	(remove-hook! after-graph-hook draw-smpte-label)
	(update-time-graph #t #t)))))


;;; -------- mark-sync-color
;;;
;;; (mark-sync-color "blue")

(define mark-sync-color
  (let ((orig-g-color #f)
	(orig-sg-color #f)
	(gm-color #f)
	(sgm-color #f)
	(ogm-color #f)
	(osgm-color #f))

    (define get-color
      (let ((tmp (GdkColor)))
	(lambda (color-name)
	  (if (not (gdk_color_parse color-name tmp))
	      (snd-error "can't find: ~A" color-name)
	      (let ((col (gdk_color_copy tmp)))
		(gdk_rgb_find_color (gdk_colormap_get_system) col)
		col)))))
    
    (define (xor-color col1 col2)
      (GdkColor (logxor (.pixel col1) (.pixel col2))
		(logxor (.red col1) (.red col2))
		(logxor (.green col1) (.green col2))
		(logxor (.blue col1) (.blue col2))))
    
    (lambda (new-color)
      (let* ((mark-gc (list-ref (snd-gcs) 9))
	     (selected-mark-gc (list-ref (snd-gcs) 10))
	     (color (get-color new-color))
	     (gmc (if (or (not gm-color)
			  (not (eq? orig-g-color (graph-color))))
		      (let ((new-color (xor-color (graph-color) color)))
			(set! orig-g-color (graph-color))
			(set! gm-color new-color)
			new-color)
		      gm-color))
	     (sgmc (if (or (not sgm-color)
			   (not (eq? orig-sg-color (selected-graph-color))))
		       (let ((new-color (xor-color (selected-graph-color) color)))
			 (set! orig-sg-color (selected-graph-color))
			 (set! sgm-color new-color)
			 new-color)
		       sgm-color))
	     (ogmc (if (or (not ogm-color)
			   (not (eq? orig-g-color (graph-color))))
		       (let ((new-color (xor-color (graph-color) (mark-color))))
			 (set! ogm-color new-color)
			 new-color)
		       ogm-color))
	     (osgmc (if (or (not osgm-color)
			    (not (eq? orig-sg-color (selected-graph-color))))
			(let ((new-color (xor-color (selected-graph-color) (mark-color))))
			  (set! osgm-color new-color)
			  new-color)
			osgm-color)))
	
	(if (not (hook-empty? draw-mark-hook)) 
	    (reset-hook! draw-mark-hook))
	(add-hook! draw-mark-hook
		   (lambda (id)
		     (if (> (mark-sync id) 0)
			 (begin
			   (gdk_gc_set_foreground mark-gc gmc)
			   (gdk_gc_set_foreground selected-mark-gc sgmc))
			 (begin
			   (gdk_gc_set_foreground mark-gc ogmc)
			   (gdk_gc_set_foreground selected-mark-gc osgmc)))
		     #f))))))
  
  

;;; -------- zync and unzync: start or stop y-zoom slider sync --------
;;; 
;;; (i.e. when one y-zoom-slider changes position, all other channels in the sound change in parallel)
;;; (zync) to start and (unzync) to stop


(define (add-dragger snd)

  (define (dragger-callback adj context)
    (let ((val (- 1.0 (.value (GTK_ADJUSTMENT adj))))
	  (snd (car context))
	  (chn (cadr context)))
      (if (sound-property 'dragger snd)
	  (begin
	    (do ((i 0 (1+ i)))
		((= i (chans snd)))
	      (if (not (= i chn))
		  (begin
		    (set! (y-zoom-slider snd i) (* val val))
		    (set! (y-position-slider snd i) (y-position-slider snd chn)))))
	    (g_signal_stop_emission (list 'gpointer (cadr adj))
				    (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj)))
				    0)))))
      
  (set! (sound-property 'dragger snd) #t)
  (do ((chn 0 (1+ chn)))
      ((= chn (chans snd)))
    (let* ((zy (list-ref (channel-widgets snd chn) 14)))
      (g_signal_connect_closure_by_id (list 'gpointer (cadr zy))
				      (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT zy)))
				      0
				      (g_cclosure_new dragger-callback (list snd chn) (list 'GClosureNotify 0))
				      #f))))

(define (zync)
  "(zync) ties each sound's y-zoom sliders together so that all change in paralle if one changes"
  (add-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (not (sound-property 'dragger n))
	 (add-dragger n)))
   (sounds)))

(define (unzync)
  "(unzync) undoes a previous (zync) -- subsequently each sound's y-zoom sliders are independent"
  (remove-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (sound-property 'dragger n)
	 (set! (sound-property 'dragger n) #f)))
   (sounds)))



;;; -------- disable control panel --------

(define (disable-control-panel snd)
  (gtk_widget_hide (caddr (sound-widgets snd)))
  (remove-from-menu 2 "Show controls"))



;;; -------- hidden controls panel --------

(define hidden-controls-dialog #f)
(define hidden-controls '())

(define hidden-controls-help 
"Expand-hop sets the time in seconds between successive grains.\n\
Expand-length sets the length of each grain.\n\
Expand-ramp sets the ramp-time in the grain envelope.\n\
Contrast-amp sets the prescaler for contrast-enhancement.\n\
Reverb-lowpass sets the feedback lowpass filter coeficient.\n\
Reverb-feedback sets the scaler on the feedback.\n\
")

(define (make-hidden-controls-dialog)

  (define (reset-all-sliders)
    (for-each
     (lambda (ctl)
       (set! ((caddr ctl) #t) (cadr ctl))
       (set! (.value (GTK_ADJUSTMENT (car ctl))) (cadr ctl))
       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car ctl))))
     hidden-controls))

  (if (not hidden-controls-dialog)
      (let ((dismiss-button (gtk_button_new_with_label "Dismiss"))
	    (help-button (gtk_button_new_with_label "Help"))
	    (reset-button (gtk_button_new_with_label "Reset")))
	(set! hidden-controls-dialog (gtk_dialog_new))
	(gtk_window_set_title (GTK_WINDOW hidden-controls-dialog) "More Controls")
	(gtk_container_set_border_width (GTK_CONTAINER hidden-controls-dialog) 10)
	(gtk_window_set_default_size (GTK_WINDOW hidden-controls-dialog) -1 -1)
	(gtk_window_set_resizable (GTK_WINDOW hidden-controls-dialog) #t)
	(gtk_widget_realize hidden-controls-dialog)
	(g_signal_connect_closure_by_id (list 'gpointer (cadr hidden-controls-dialog))
					(g_signal_lookup "delete_event" (G_OBJECT_TYPE (GTK_OBJECT hidden-controls-dialog)))
					0 (g_cclosure_new (lambda (w ev data) (gtk_widget_hide hidden-controls-dialog)) #f #f) #f)

	(gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG hidden-controls-dialog))) dismiss-button #t #t 20)
	(g_signal_connect_closure_by_id (list 'gpointer (cadr dismiss-button))
					(g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT dismiss-button)))
					0 (g_cclosure_new (lambda (w data) (gtk_widget_hide hidden-controls-dialog)) #f #f) #f)
	(gtk_widget_show dismiss-button)
	(gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG hidden-controls-dialog))) reset-button #t #t 20)
	(g_signal_connect_closure_by_id (list 'gpointer (cadr reset-button))
					(g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT reset-button)))
					0 (g_cclosure_new (lambda (w data) (reset-all-sliders)) #f #f) #f)
	(gtk_widget_show reset-button)
	(gtk_box_pack_end (GTK_BOX (.action_area (GTK_DIALOG hidden-controls-dialog))) help-button #t #t 20)
	(g_signal_connect_closure_by_id (list 'gpointer (cadr help-button))
					(g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT help-button)))
					0 (g_cclosure_new (lambda (w data) (help-dialog "More Controls" hidden-controls-help)) #f #f) #f)
	(gtk_widget_show help-button)
	
	(let ((mainform (.vbox (GTK_DIALOG hidden-controls-dialog))))
	  (for-each
	   (lambda (lst)
	     (let* ((title (car lst))
		    (low (cadr lst))
		    (high (caddr lst))
		    (initial (list-ref lst 3))
		    (func (list-ref lst 4))
		    (adj (gtk_adjustment_new initial low high .001 .001 .1))
		    (slider (gtk_hscale_new (GTK_ADJUSTMENT adj)))
		    (label (gtk_label_new title)))
	       (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE slider)) GTK_UPDATE_CONTINUOUS)
	       (gtk_scale_set_digits (GTK_SCALE slider) 3)
	       (gtk_scale_set_value_pos (GTK_SCALE slider) GTK_POS_TOP)
	       (gtk_scale_set_draw_value (GTK_SCALE slider) #t)
	       (gtk_box_pack_start (GTK_BOX mainform) slider #t #t 4)
	       (gtk_box_pack_start (GTK_BOX mainform) label #t #t 4)
	       (set! hidden-controls (cons (list adj initial func) hidden-controls))	       
	       (g_signal_connect_closure_by_id (list 'gpointer (cadr adj))
					       (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj)))
					       0
					       (g_cclosure_new (lambda (adj data)
								 (set! (func #t) (.value (GTK_ADJUSTMENT adj))))
							       #f #f)
					       #f)
	       (gtk_widget_show slider)
	       (gtk_widget_show label)))
	   (list (list "expand-hop" 0.01 1.0 0.05  expand-control-hop)
		 (list "expand-length" 0.01 .5 0.15 expand-control-length)
		 (list "expand-ramp" 0.01 .5 0.4 expand-control-ramp)
		 (list "contrast-amp" 0.0 2.0 1.0 contrast-control-amp)
		 (list "reverb-lowpass" 0.0 1.0 0.7 reverb-control-lowpass)
		 (list "reverb-feedback" 0.0 1.25 1.09 reverb-control-feedback))))

	(add-to-menu 3 "Hidden controls" 
		     (lambda () 
		       (gtk_widget_show hidden-controls-dialog)))
	))
  (gtk_widget_show hidden-controls-dialog))



;;; -------- show-disk-space
;;;
;;; adds a label to the minibuffer area showing the current free space 

(define show-disk-space
  (let ((labelled-snds '()))

    (define (kmg num)
      (if (<= num 0)
	  "disk full!"
	  (if (> num 1024)
	      (if (> num (* 1024 1024))
		  (format #f "space: ~6,3FG" (/ num (* 1024 1024)))
		  (format #f "space: ~6,3FM" (/ num 1024.0)))
	      (format #f "space: ~10DK" num))))

    (define (show-label data)
      (if (sound? (car data))
	  (let ((space (kmg (disk-kspace (file-name (car data))))))
	    (gtk_label_set_text (GTK_LABEL (cadr data)) space)
	    (gtk_timeout_add 10000 show-label data) ; every 10 seconds recheck space
	    0)))

    (lambda (snd)
      "(show-disk-space) adds a label to the minibuffer area showing the current free space (for use with after-open-hook)"
      (let ((previous-label (find-if (lambda (n) (= (car n) snd)) labelled-snds)))
	(if (not previous-label)
	    (let* ((name-form (list-ref (sound-widgets) 10))
		   (space (kmg (disk-kspace (file-name snd))))
		   (new-label (gtk_label_new space)))
	      (gtk_box_pack_start (GTK_BOX name-form) new-label #f #f 6)
	      (gtk_widget_show new-label)
	      (set! previous-label (list snd new-label))
	      (set! labelled-snds (cons previous-label labelled-snds))))
	(gtk_timeout_add 10000 show-label previous-label)))))

;(add-hook! after-open-hook show-disk-space)


;;; -------- remove top level menu
;;;
;;; (remove-main-menu 5) removes the Help menu

(define (remove-main-menu menu)
  "(remove-main-menu menu) removes the specified top-level menu: (remove-main-menu 5) removes the Help menu"
  (gtk_widget_hide (list-ref (menu-widgets) menu)))


;;; -------- keep-file-dialog-open-upon-ok
;;;
;;; this seems to work, but it's a kludge that depends on the fact that the Snd ok-callback
;;;   is called before our callback -- I don't know who determines the order of calls.
;;;   (This order means we can't use stop_emission to disable the built-in callback)

(define (keep-file-dialog-open-upon-ok)
  (let* ((dialog (let ((m (open-file-dialog #f)))
		   (list-ref (dialog-widgets) 6)))
	 (ok-button (.ok_button (GTK_FILE_SELECTION dialog))))
    (g_signal_connect_closure_by_id 
     (list 'gpointer (cadr ok-button))
     (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT ok-button)))
     0
     (g_cclosure_new (lambda (w d) (gtk_widget_show dialog)) #f #f) #f)
    'ok))


;;; -------- snd-clock-icon --------
;;;
;;; a clock icon to replace Snd's hourglass
;;;   call from a work proc or whatever with hour going from 0 to 12 then #f
;;; doesn't work yet because I can't bring myself to translate sg_set_pixmap

(define snd-clock-icon
  (let* ((shell (cadr (main-widgets)))
	 (win (car (main-widgets)))
	 (clock-pixmaps (make-vector 12))
	 (dgc (car (snd-gcs)))
	 (white (let ((tmp (GdkColor)))
		  (gdk_color_parse "white" tmp)
		  (let ((col (gdk_color_copy tmp)))
		    (gdk_rgb_find_color (gdk_colormap_get_system) col)
		    col)))
	 (black (let ((tmp (GdkColor)))
		  (gdk_color_parse "black" tmp)
		  (let ((col (gdk_color_copy tmp)))
		    (gdk_rgb_find_color (gdk_colormap_get_system) col)
		    col))))
    (do ((i 0 (1+ i)))
	((= i 12))
      (let* ((pix (gdk_pixmap_new win 16 16 -1))
	     (pixwin (GDK_DRAWABLE pix)))
	(vector-set! clock-pixmaps i pix)
	(gdk_gc_set_foreground dgc (basic-color))
	(gdk_draw_rectangle pixwin dgc #t 0 0 16 16)
	(gdk_gc_set_foreground dgc white)
	(gdk_draw_arc pixwin dgc #t 1 1 14 14 0 (* 64 360))
	(gdk_gc_set_foreground dgc black)
	(gdk_draw_arc pixwin dgc #f 1 1 14 14 0 (* 64 360))
	(gdk_draw_line pixwin dgc 8 8 
		       (+ 8 (inexact->exact (* 7 (sin (* i (/ 3.1416 6.0))))))
		       (- 8 (inexact->exact (* 7 (cos (* i (/ 3.1416 6.0)))))))))
    (gdk_gc_set_foreground dgc (data-color))
    (lambda (snd hour)
      ;; TODO: figure out some not-completely-idiotic way to set the widget's pixmap
      #f)))



;;; -------- add "tooltip" to a widget
;;;
;;; (add-tooltip (cadr (channel-widgets)) "the w button")

(define (add-tooltip widget tip)
  (gtk_tooltips_set_tip (gtk_tooltips_new) widget tip ""))

