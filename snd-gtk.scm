;;; translations from snd-motif.scm
;;;
;;; display-scanned-synthesis
;;; show-smpte-label
;;; mark-sync-color

(use-modules (ice-9 common-list) (ice-9 format))

(if (not (provided? 'xg))
    (let ((hxm (dlopen "xg.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-gtk.scm needs the xg module: ~A" hxm))
	  (dlinit hxm "init_xm"))))


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
					 (g_cclosure_new (lambda (w d) (func (.value (GTK_ADJUSTMENT adj)))) #f (list 'GClosureNotify 0))
					 #f)))
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
							  #f 
							  (list 'GClosureNotify 0))
					  #f))))

    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-pane))
				    (g_signal_lookup "expose_event" (G_OBJECT_TYPE (GTK_OBJECT scan-pane)))
				    0
				    (g_cclosure_new (lambda (w e d) (redraw-graph)) #f (list 'GClosureNotify 0))
				    #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-pane))
				    (g_signal_lookup "configure_event" (G_OBJECT_TYPE (GTK_OBJECT scan-pane)))
				    0
				    (g_cclosure_new (lambda (w e d) (redraw-graph)) #f (list 'GClosureNotify 0))
				    #f)
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
						    #f (list 'GClosureNotify 0))
				    #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-start))
				    (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT scan-start))) 0
				    (g_cclosure_new (lambda (w d) (start-synthesis)) #f (list 'GClosureNotify 0)) #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-continue))
				    (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT scan-continue))) 0
				    (g_cclosure_new (lambda (w d) (continue-synthesis)) #f (list 'GClosureNotify 0)) #f)
    (g_signal_connect_closure_by_id (list 'gpointer (cadr scan-stop))
				    (g_signal_lookup "clicked" (G_OBJECT_TYPE (GTK_OBJECT scan-stop))) 0
				    (g_cclosure_new (lambda (w d) (stop-synthesis)) #f (list 'GClosureNotify 0)) #f)
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
  
  