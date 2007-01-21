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
;;; snd-clock-icon
;;; add "tooltip" to a widget
;;; bring possibly-obscured dialog to top
;;; select-file
;;; with-level-meters
;;; add delete and rename options to the file menu
;;; make-pixmap
;;; notebook-with-top-tabs
;;; make-font-selector-dialog
;;; make-color-selector-dialog
;;; add-main-menu-mnemonics

(use-modules (ice-9 format) (ice-9 common-list))

(if (not (provided? 'snd-gtk)) (snd-error "snd-gtk.scm is Gtk-specific"))

(if (not (defined? 'find-if))
    (define (find-if pred l)
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l))))))

(if (not (provided? 'xg))
    (let ((hxm (dlopen "xg.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-gtk.scm needs the xg module: ~A" hxm))
	  (dlinit hxm "Init_libxg"))))

(provide 'snd-snd-gtk.scm)

(if (not (provided? 'snd-extensions.scm)) (load-from-path "extensions.scm"))
(if (not (provided? 'snd-play.scm)) (load-from-path "play.scm"))

(define (load-font name)
  (pango_font_description_from_string name))

(define (g-list-foreach glist func)
  (let ((len (g_list_length glist)))
    (do ((i 0 (1+ i)))
	((= i len))
      (func (g_list_nth_data glist i)))))

(define (for-each-child w func)
  "(for-each-child w func) applies func to w and each of its children"
  (func w)
  (g-list-foreach (gtk_container_get_children (GTK_CONTAINER w))
		  (lambda (w)
		    (func (GTK_WIDGET w)))))


(define (host-name)
  "(host-name) -> name of current machine"
  (let ((val (gdk_property_get (car (main-widgets))
			       (gdk_atom_intern "WM_CLIENT_MACHINE" #f)
			       GDK_TARGET_STRING 0 1024 0)))
    ;; val is list: (success atom element-size length unterminated-string)
    (and (car val)
	 (substring (list-ref val 4) 0 (list-ref val 3)))))


(define red-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "red" tmp)
    (let ((col (gdk_color_copy tmp)))
      (gdk_rgb_find_color (gdk_colormap_get_system) col)
      col)))

(define white-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "white" tmp)
    (let ((col (gdk_color_copy tmp)))
      (gdk_rgb_find_color (gdk_colormap_get_system) col)
      col)))

(define black-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "black" tmp)
    (let ((col (gdk_color_copy tmp)))
      (gdk_rgb_find_color (gdk_colormap_get_system) col)
      col)))


;;; -------- display-scanned-synthesis --------
;;;
;;; open a new main pane below the listener, with two sections
;;;  on the left various controls, on the right a graph
;;;  push 'start' to start the scanned synthesis display
;;;  if spring > mass, you'll get overflows
;;;

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
  
  (set! (mus-srate) 22050.0)
  
  (let* ((mass 1.0)
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
	 (tbl (make-table-lookup :size size))
	 (frequency 440.0)
	 (amplitude 0.02)
	 (gx0 (mus-data tbl))
	 (gx1 (make-vct size))	   
	 (gx2 (make-vct size))
	 (vect (make-vector (* 2 size)))
	 (work-proc #f)
	 (play-button #f)) ; fixed up later -- needed in stop-synthesis
    
    (define (y->grfy y range)
      (min ay1
	   (max ay0
		(inexact->exact
		 (round (+ ay0
			   (* range (- 10.0 y))))))))
    
    (define (draw-graph)
      (if (and (> ax1 ax0)
	       (> ay1 ay0))
	  (let ((diff (* 0.05 (- ay1 ay0))) ; assuming -10 to 10 
		(wn (GDK_DRAWABLE (.window scan-pane)))
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
	      (vector-set! vect j (inexact->exact (floor xi)))
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
	  (g_source_remove work-proc))
      (set! work-proc #f)
      (gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON play-button) #f))
    
    (define (start-synthesis)
      (stop-synthesis)
      (vct-fill! gx0 0.0)
      (vct-fill! gx1 0.0)
      (vct-fill! gx2 0.0)
      (do ((i 0 (1+ i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (vct-set! gx1 (+ i (- (/ size 4) 6)) val)))
      (set! work-proc (g_idle_add tick-synthesis #f)))
    
    (define (continue-synthesis)
      (stop-synthesis)
      (set! work-proc (g_idle_add tick-synthesis #f)))
    
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
	 (g_signal_connect adj "value_changed" (lambda (w d) (func (.value (GTK_ADJUSTMENT adj)))) #f)))
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
	  (g_signal_connect scan-text "activate"
			    (lambda (w d) 
			      (stop-synthesis)
			      (set! size (string->number (gtk_entry_get_text (GTK_ENTRY scan-text))))
			      (set! tbl (make-table-lookup :size size))
			      (set! gx0 (mus-data tbl))
			      (set! gx1 (make-vct size))	   
			      (set! gx2 (make-vct size))
			      (set! vect (make-vector (* size 2))))
			    #f))))
    (set! play-button (gtk_check_button_new_with_label "play"))
    (gtk_box_pack_start (GTK_BOX scan-row) play-button #f #f 4)
    (gtk_widget_show play-button)
    (g_signal_connect play-button "toggled"
		      (lambda (w d) 
			(if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON play-button))
			    (let* ((audio-info (open-play-output 1 22050 #f 128))
				   (audio-fd (car audio-info))
				   (outchans (cadr audio-info))
				   (frames (caddr audio-info))
				   (data (make-sound-data outchans frames)))
			      (if (not (= audio-fd -1))
				  (do ()
				      ((or (c-g?) (not (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON play-button))))
				       (mus-audio-close audio-fd))
				    (tick-synthesis work-proc)
				    (do ((k 0 (1+ k)))
					((= k frames))
				      (sound-data-set! data 0 k (* amplitude (table-lookup tbl))))
				    (mus-audio-write audio-fd data frames))))))
		      #f)
    (let* ((freq-adj (gtk_adjustment_new 440.0 20.0 1000.0 1.0 10.0 1.0)) ; step incr, page incr page size
	   (amp-adj (gtk_adjustment_new 0.02 0.0 0.1 .001 .01 .001))
	   (freq-scale (gtk_hscale_new (GTK_ADJUSTMENT freq-adj)))
	   (amp-scale (gtk_hscale_new (GTK_ADJUSTMENT amp-adj)))
	   (freq-label (gtk_label_new "frequency"))
	   (amp-label (gtk_label_new "amplitude")))
      (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE freq-scale)) GTK_UPDATE_CONTINUOUS)
      (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE amp-scale)) GTK_UPDATE_CONTINUOUS)
      (gtk_scale_set_digits (GTK_SCALE freq-scale) 1)
      (gtk_scale_set_digits (GTK_SCALE amp-scale) 3)
      (gtk_scale_set_value_pos (GTK_SCALE freq-scale) GTK_POS_TOP)
      (gtk_scale_set_value_pos (GTK_SCALE amp-scale) GTK_POS_TOP)
      (gtk_scale_set_draw_value (GTK_SCALE freq-scale) #t)
      (gtk_scale_set_draw_value (GTK_SCALE amp-scale) #t)
      (gtk_box_pack_start (GTK_BOX scan-row) freq-scale #t #t 0)
      (gtk_box_pack_start (GTK_BOX scan-row) freq-label #t #t 0)
      (gtk_box_pack_start (GTK_BOX scan-row) amp-scale #t #t 0)
      (gtk_box_pack_start (GTK_BOX scan-row) amp-label #t #t 0)
      (gtk_widget_show freq-scale)
      (gtk_widget_show amp-scale)
      (gtk_widget_show freq-label)
      (gtk_widget_show amp-label)
      (g_signal_connect freq-adj "value_changed" (lambda (w d) (set! (mus-frequency tbl) (.value (GTK_ADJUSTMENT freq-adj)))) #f)
      (g_signal_connect amp-adj "value_changed" (lambda (w d) (set! amplitude (.value (GTK_ADJUSTMENT amp-adj)))) #f)
      )
    
    (g_signal_connect scan-pane "expose_event" (lambda (w e d) (redraw-graph)) #f)
    (g_signal_connect scan-pane "configure_event" (lambda (w e d) (redraw-graph)) #f)
    (g_signal_connect scan-pane "button_press_event" 
		      (lambda (w e d) 
			(let ((button (.button (GDK_EVENT_BUTTON e))))
			  (if (not work-proc)
			      (if (= button 2)
				  (continue-synthesis)
				  (start-synthesis))
			      (stop-synthesis))))
		      #f)
    (g_signal_connect scan-start "clicked" (lambda (w d) (start-synthesis)) #f)
    (g_signal_connect scan-continue "clicked" (lambda (w d) (continue-synthesis)) #f)
    (g_signal_connect scan-stop "clicked" (lambda (w d) (stop-synthesis)) #f)
    #t))


(define (close-scanned-synthesis-pane)
  (gtk_widget_hide scanned-synthesis-pane))



;;; -------- show-smpte-label
;;;
;;; (show-smpte-label :optional on-or-off)
;;;   turns on/off a label in the time-domain graph showing the current smpte frame of the leftmost sample

(define smpte-frames-per-second 24.0)

(define get-text-width-and-height
  (let ((smpte-font-wh #f)
	(smpte-font-name ""))
    (lambda (text fs)
      (if (or (not smpte-font-wh)
	      (not (string=? smpte-font-name (axis-numbers-font))))
	  (let* ((ctx (gdk_pango_context_get))
		 (layout (pango_layout_new ctx)))
	    (set! smpte-font-name (axis-numbers-font))
	    (if layout
		(begin
		  ;; should we call g_utf8_validate first here?
		  (pango_layout_set_font_description layout fs)
		  (pango_layout_set_text layout text -1)
		  (let ((wid (pango_layout_get_pixel_size layout #f)))
		    (g_object_unref (GPOINTER layout))
		    (set! smpte-font-wh wid)
		    (g_object_unref (GPOINTER ctx))
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
	    (let* ((smpte (smpte-label (car axinf) (srate snd))))
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
	    (g_signal_stop_emission (GPOINTER adj)
				    (g_signal_lookup "value_changed" (G_OBJECT_TYPE (GTK_OBJECT adj)))
				    0)))))
      
  (set! (sound-property 'dragger snd) #t)
  (set! (sound-property 'save-state-ignore snd)
	(cons 'dragger
	      (or (sound-property 'save-state-ignore snd)
		  (list 'save-state-ignore))))
  (do ((chn 0 (1+ chn)))
      ((= chn (chans snd)))
    (let* ((zy (list-ref (channel-widgets snd chn) 14)))
      (g_signal_connect_closure_by_id (GPOINTER zy)
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
"Expand-hop sets the time in seconds between successive grains.
Expand-length sets the length of each grain.
Expand-ramp sets the ramp-time in the grain envelope.
Expand-jitter sets the grain timing jitter.
Contrast-amp sets the prescaler for contrast-enhancement.
Reverb-lowpass sets the feedback lowpass filter coeficient.
Reverb-feedback sets the scaler on the feedback.
")

(define (make-hidden-controls-dialog)

  (define (reset-all-sliders)
    (for-each
     (lambda (ctl)
       (set! ((caddr ctl)) (cadr ctl))
       (set! (.value (GTK_ADJUSTMENT (car ctl))) (cadr ctl))
       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car ctl))))
     hidden-controls))

  (if (not hidden-controls-dialog)
      (let ((dismiss-button (gtk_button_new_with_label "Dismiss"))
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

	(gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG hidden-controls-dialog))) dismiss-button #t #t 20)
	(g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide hidden-controls-dialog)) #f)
	(gtk_widget_show dismiss-button)
	(gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG hidden-controls-dialog))) reset-button #t #t 20)
	(g_signal_connect reset-button "clicked" (lambda (w data) (reset-all-sliders)) #f)
	(gtk_widget_show reset-button)
	(gtk_box_pack_end (GTK_BOX (.action_area (GTK_DIALOG hidden-controls-dialog))) help-button #t #t 20)
	(g_signal_connect help-button "clicked" (lambda (w data) (help-dialog "More Controls" hidden-controls-help)) #f)
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
	       (g_signal_connect adj "value_changed" (lambda (adj data) (set! (func) (.value (GTK_ADJUSTMENT adj)))) #f)
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
		  (format #f "space: ~6,3FG" (/ num (* 1024.0 1024.0)))
		  (format #f "space: ~6,3FM" (/ num 1024.0)))
	      (format #f "space: ~10DK" num))))

    (define (show-label data)
      (if (sound? (car data))
	  (let ((space (kmg (disk-kspace (file-name (car data))))))
	    (gtk_label_set_text (GTK_LABEL (cadr data)) space)
	    (g_timeout_add 10000 show-label data) ; every 10 seconds recheck space
	    0)))

    (lambda* (:optional snd-arg)
      "(show-disk-space) adds a label to the minibuffer area showing the current free space (for use with after-open-hook)"
      (let* ((snd (or snd-arg (selected-sound)))
	     (previous-label (find-if (lambda (n) (= (car n) snd)) labelled-snds)))
	(if (not previous-label)
	    (if (not snd)
		(snd-error "no sound found for disk space label")
		(let* ((name-form (list-ref (sound-widgets) 10))
		       (space (kmg (disk-kspace (file-name snd))))
		       (new-label (gtk_label_new space)))
		  (gtk_box_pack_start (GTK_BOX name-form) new-label #f #f 6)
		  (gtk_widget_show new-label)
		  (set! previous-label (list snd new-label))
		  (set! labelled-snds (cons previous-label labelled-snds))
		  (g_timeout_add 10000 show-label previous-label))))))))



;;; -------- remove top level menu
;;;
;;; (remove-main-menu 5) removes the Help menu

(define (remove-main-menu menu)
  "(remove-main-menu menu) removes the specified top-level menu: (remove-main-menu 5) removes the Help menu"
  (gtk_widget_hide (list-ref (menu-widgets) menu)))


;;; -------- keep-file-dialog-open-upon-ok
;;;
;;; this seems to work, but it's a kludge

(define (keep-file-dialog-open-upon-ok)
  (let ((dialog (open-file-dialog #f)))
    (g_object_set_data (G_OBJECT dialog) "hide-me" (GPOINTER 1)))) ; anything not 0 means don't hide (this is a stupid kludge forced on me by goddamn gtk)
	


;;; -------- snd-clock-icon --------
;;;
;;; a clock icon to replace Snd's hourglass
;;;   call from a work proc or whatever with hour going from 0 to 12 then #f

(if (defined? 'gdk_pixmap_new)
(define snd-clock-icon
  (let* ((shell (cadr (main-widgets)))
	 (win (car (main-widgets)))
	 (clock-pixmaps (make-vector 12))
	 (dgc (car (snd-gcs))))
    (do ((i 0 (1+ i)))
	((= i 12))
      (let* ((pix (gdk_pixmap_new (GDK_DRAWABLE win) 16 16 -1))
	     (pixwin (GDK_DRAWABLE pix)))
	(vector-set! clock-pixmaps i pix)
	(gdk_gc_set_foreground dgc (basic-color))
	(gdk_draw_rectangle pixwin dgc #t 0 0 16 16)
	(gdk_gc_set_foreground dgc white-pixel)
	(gdk_draw_arc pixwin dgc #t 1 1 14 14 0 (* 64 360))
	(gdk_gc_set_foreground dgc black-pixel)
	(gdk_draw_arc pixwin dgc #f 1 1 14 14 0 (* 64 360))
	(gdk_draw_line pixwin dgc 8 8 
		       (+ 8 (inexact->exact (round (* 7 (sin (* i (/ 3.1416 6.0)))))))
		       (- 8 (inexact->exact (round (* 7 (cos (* i (/ 3.1416 6.0))))))))))
    (gdk_gc_set_foreground dgc (data-color))
    (lambda (snd hour)
      (gdk_draw_drawable (GDK_DRAWABLE (.window (list-ref (sound-widgets snd) 8))) dgc 
			 (GDK_DRAWABLE (vector-ref clock-pixmaps hour)) 0 0 0 4 16 16)
      #f))))



;;; -------- add "tooltip" to a widget
;;;
;;; (add-tooltip (cadr (channel-widgets)) "the w button")

(define (add-tooltip widget tip)
  (gtk_tooltips_set_tip (gtk_tooltips_new) widget tip ""))


;;; -------- bring possibly-obscured dialog to top

(define (raise-dialog w)
  (gtk_widget_show w)
  (gtk_window_present (GTK_WINDOW w)))


;;; -------- select-file --------
;;;
;;; (select-file func :optional title dir filter help)
;;;   starts a File Selection Dialog, runs func if a file is selected
;;;
;;; (add-to-menu 0 "Insert File" 
;;;   (lambda () 
;;;     (select-file 
;;;       (lambda (filename)
;;;         (insert-sound filename))
;;;       "Insert File" "." "*" "file will be inserted at cursor")))

(define select-file

  (let ((file-selector-dialogs '()))
    ;; (list (list widget inuse func title help) ...)
    (define (find-free-dialog ds)
      (if (null? ds)
	  #f
	  (if (not (cadr (car ds)))
	      (begin
		(list-set! (car ds) 1 #t)
		(caar ds))
	      (find-free-dialog (cdr ds)))))
    (define (find-dialog-widget wid ds)
      (if (null? ds)
	  #f
	  (if (equal? wid (caar ds))
	      (car ds)
	      (find-dialog-widget wid (cdr ds)))))
    (lambda args
      ;; (file-select func title dir filter help)
      (let* ((func (if (> (length args) 0) (list-ref args 0) #f))
	     (title (if (> (length args) 1) (list-ref args 1) "select file"))
	     ;; (dir (if (> (length args) 2) (list-ref args 2) "."))
	     ;; (filter (if (> (length args) 3) (list-ref args 3) "*"))
	     (dialog (or (find-free-dialog file-selector-dialogs)
		 	 (let ((new-dialog (gtk_file_selection_new title)))
			   (g_signal_connect new-dialog "delete_event" 
					     (lambda (w e d) 
					       (let ((lst (find-dialog-widget new-dialog file-selector-dialogs)))
						 (list-set! lst 1 #f)
						 (gtk_widget_hide new-dialog)
						 #t))
					     #f)
			   (g_signal_connect  (.ok_button (GTK_FILE_SELECTION new-dialog)) "clicked"
					      (lambda (w d)
						(let ((lst (find-dialog-widget new-dialog file-selector-dialogs)))
						  ((list-ref lst 2) (gtk_file_selection_get_filename (GTK_FILE_SELECTION new-dialog)))
						  (list-set! lst 1 #f)
						  (gtk_widget_hide new-dialog)))
					      #f)
			   (g_signal_connect (.cancel_button (GTK_FILE_SELECTION new-dialog)) "clicked"
					     (lambda (w d) 
					       (let ((lst (find-dialog-widget new-dialog file-selector-dialogs)))
						 (list-set! lst 1 #f)
						 (gtk_widget_hide new-dialog)))
					     #f)
			   (set! file-selector-dialogs (cons (list new-dialog #t func title) file-selector-dialogs))
			   new-dialog))))
	(gtk_widget_show dialog)))))

; (select-file (lambda (n) (snd-print n)))


;;; -------- with-level-meters, make-level-meter, display-level

(define (make-level-meter parent width height)
  (let ((frame (gtk_frame_new #f)))
    (gtk_widget_set_size_request frame width height)
    (gtk_box_pack_start (GTK_BOX parent) frame #t #t 4)
    (gtk_widget_show frame)
    (let ((meter (gtk_drawing_area_new)))
      (gtk_widget_set_events meter (logior GDK_EXPOSURE_MASK GDK_STRUCTURE_MASK))
      (gtk_container_add (GTK_CONTAINER frame) meter)
      (gtk_widget_show meter)
      (let ((context (list meter 0.0 1.0 0.0 0.0 width height)))
	(g_signal_connect meter "expose_event" (lambda (w e d) (display-level d)) context)
	(g_signal_connect meter "configure_event" 
			  (lambda (w e d)
			    (let ((xy (gdk_drawable_get_size (GDK_DRAWABLE (.window w)))))
			      (list-set! d 5 (car xy))
			      (list-set! d 6 (cadr xy))
			      (display-level d)))
			  context)
	context))))

(define (display-level meter-data)
  (let* ((meter (car meter-data))
	 (level (list-ref meter-data 1))
	 (last-level (list-ref meter-data 3))
	 (red-deg (list-ref meter-data 4))
	 (width (list-ref meter-data 5))
	 (height (list-ref meter-data 6))
	 ;; (size (list-ref meter-data 2))
	 (win (GDK_DRAWABLE (.window meter)))
	 (major-tick (inexact->exact (round (/ width 24))))
	 (minor-tick (inexact->exact (round (* major-tick .6))))
	 (ang0 (* 45 64))
	 (ang1 (* 90 64))
	 (wid2 (inexact->exact (floor (/ width 2))))
	 (gc (car (snd-gcs)))
	 (top (inexact->exact (round (/ height 3.2))))) ; distance of label from top of meter
    (gdk_gc_set_foreground gc white-pixel)
    (gdk_draw_rectangle win gc #t 0 0 width height)
    (gdk_gc_set_foreground gc black-pixel)
    (gdk_draw_arc win gc #f 0 top width width ang0 ang1)
    (gdk_draw_arc win gc #f 0 (1- top) width width ang0 ang1)
    (if (> width 100)
	(gdk_draw_arc win gc #f 0 (- top 2) width width ang0 ang1))
    (gdk_draw_arc win gc #f 4 (+ top 4) (- width 8) (- width 8) ang0 ang1)
    (do ((i 0 (1+ i)))
	((= i 5))
      (let* ((rdeg (degrees->radians (- 45 (* i 22.5))))
	     (sinr (sin rdeg))
	     (cosr (cos rdeg))
	     (x0 (inexact->exact (round (+ wid2 (* wid2 sinr)))))
	     (y0 (inexact->exact (round (- (+ wid2 top) (* wid2 cosr)))))
	     (x1 (inexact->exact (round (+ wid2 (* (+ wid2 major-tick) sinr)))))
	     (y1 (inexact->exact (round (- (+ wid2 top) (* (+ wid2 major-tick) cosr))))))
	(gdk_draw_line win gc x0 y0 x1 y1)
	(gdk_draw_line win gc (+ x0 1) y0 (+ x1 1) y1)
	(if (< i 4)
	    (do ((j 1 (1+ j)))
		((= j 6))
	      (let* ((rdeg (degrees->radians (- 45 (* i 22.5) (* j (/ 90.0 20.0)))))
		     (sinr (sin rdeg))
		     (cosr (cos rdeg))
		     (x0 (inexact->exact (round (* wid2 (+ 1.0 sinr)))))
		     (y0 (inexact->exact (round (- (+ wid2 top) (* wid2 cosr)))))
		     (x1 (inexact->exact (round (+ wid2 (* (+ wid2 minor-tick) sinr)))))
		     (y1 (inexact->exact (round (- (+ wid2 top) (* (+ wid2 minor-tick) cosr))))))
		(gdk_draw_line win gc x0 y0 x1 y1))))))
    (let* ((needle-speed 0.25)
	   (bubble-speed 0.025)
	   (bubble-size (* 15 64))
	   (val (+ (* level needle-speed) (* last-level (- 1.0 needle-speed))))
	   (deg (- (* val 90.0) 45.0))
	   (rdeg (degrees->radians deg))
	   (nx1 (inexact->exact (round (+ wid2 (* (+ wid2 major-tick) (sin rdeg))))))
	   (ny1 (inexact->exact (round (- (+ wid2 top) (* (+ wid2 major-tick) (cos rdeg)))))))
      (gdk_draw_line win gc wid2 (+ top wid2) nx1 ny1)
      (list-set! meter-data 3 val)
      (if (> val red-deg)
	  (list-set! meter-data 4 val)
	  (list-set! meter-data 4 (+ (* val bubble-speed) (* red-deg (- 1.0 bubble-speed)))))
      (if (> (list-ref meter-data 4) .01)
	  (begin
	    (gdk_gc_set_foreground gc red-pixel)
	    (let* ((redx (inexact->exact (floor (* (list-ref meter-data 4) 90 64))))
		   (redy (min redx bubble-size)))
	      (do ((i 0 (1+ i)))
		  ((= i 4))
		(gdk_draw_arc win gc #f i (+ top i) (- width (* i 2)) (- width (* i 2)) (- (* 135 64) redx) redy)))
	    (gdk_gc_set_foreground gc black-pixel)))
      )))

(define (with-level-meters n)
  ;; add n level meters to a pane at the top of the Snd window
  (let* ((parent (list-ref (main-widgets) 5))
	 (height (if (> n 2) 70 85))
	 (parent-width (cadr (gdk_drawable_get_size (GDK_DRAWABLE (.window parent)))))
	 (width (inexact->exact (floor (/ parent-width n))))
	 (meters (gtk_hbox_new #t 4))
	 (meter-list '()))
    (gtk_box_pack_start (GTK_BOX parent) meters #f #f 4)
    (gtk_widget_set_size_request meters width height)
    (gtk_widget_show meters)
    (do ((i 0 (1+ i)))
	((= i n))
      (set! meter-list (cons (make-level-meter meters width height) meter-list)))
    (add-hook! dac-hook 
	       (lambda (sdobj)
		 (let* ((maxes (sound-data-maxamp sdobj)))
		   (for-each
		    (lambda (meter)
		      (if (null? maxes)
			  (list-set! meter 1 0.0)
			  (begin
			    (list-set! meter 1 (car maxes))
			    (display-level meter)
			    (set! maxes (cdr maxes)))))
		    (reverse meter-list)))))
    (add-hook! stop-dac-hook
	       (lambda () ; drain away the bubble
		 (gtk_idle_add 
		  (let ((ctr 0))
		    (lambda (ignored)
		      (for-each 
		       (lambda (meter)
			 (list-set! meter 1 0.0)
			 (display-level meter))
		       meter-list)
		      (set! ctr (+ ctr 1))
		      (< ctr 200)))
		  #f)))
    meter-list))


;;; -------- add delete and rename options to the file menu

(define (add-delete-and-rename-options)
  (let ((dialog (open-file-dialog #f)))
    (if (GTK_IS_FILE_SELECTION dialog) ; in newer gtk's this is a file_chooser, not a file_selection widget
	(gtk_file_selection_show_fileop_buttons (GTK_FILE_SELECTION dialog)))
    (set! dialog (mix-file-dialog #f))
    (if (GTK_IS_FILE_SELECTION dialog)
	(gtk_file_selection_show_fileop_buttons (GTK_FILE_SELECTION dialog)))))

  

;;; -------- make-pixmap --------

(define arrow-strs (list
"16 12 6 1"
" 	c None s None"
".	c gray50"
"X	c black"
"o	c white"
"O	c yellow"
"-      c ivory2 s basiccolor"
"--------X---------"
"---------X--------"
"----------X-------"
"-----------X------"
"------------X-----"
"XXXXXXXXXXXXXX----"
"------------X-----"
"-----------X------"
"----------X-------"
"---------X--------"
"--------X---------"
"-------X----------"))

(define (make-pixmap strs) ; strs is list of strings as in arrow-strs above
  (let ((win (GDK_DRAWABLE (car (main-widgets)))))
    (gdk_pixmap_create_from_xpm_d win #f (basic-color) (list->c-array strs "gchar**"))))


;;; -------- state display panel --------

(define variables-dialog #f)
(define variables-notebook #f)
(define variables-pages '())

(define (make-variables-dialog)
  (let ((dismiss-button (gtk_button_new_with_label "Dismiss")))
    (gtk_widget_set_name dismiss-button "quit_button")
    (set! variables-dialog (gtk_dialog_new))
    (gtk_window_set_title (GTK_WINDOW variables-dialog) "Variables")
    (gtk_container_set_border_width (GTK_CONTAINER variables-dialog) 10)
    (gtk_window_set_default_size (GTK_WINDOW variables-dialog) -1 -1)
    (gtk_window_set_resizable (GTK_WINDOW variables-dialog) #t)
    (gtk_widget_realize variables-dialog)
    (g_signal_connect variables-dialog "delete_event" (lambda (w ev data) (gtk_widget_hide variables-dialog) #t) #f)
    (gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG variables-dialog))) dismiss-button #t #t 20)
    (g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide variables-dialog)) #f)
    (gtk_widget_show dismiss-button)
    (set! variables-notebook (gtk_notebook_new))
    (gtk_box_pack_start (GTK_BOX (.vbox (GTK_DIALOG variables-dialog))) variables-notebook #t #t 4)
    (gtk_notebook_set_tab_pos (GTK_NOTEBOOK variables-notebook) GTK_POS_RIGHT)
    (gtk_widget_show variables-notebook)
    (gtk_widget_show variables-dialog)
    variables-dialog))

(define* (make-variable-display page-name variable-name :optional (type 'text) (range (list 0.0 1.0)))
  ;; type = 'text, 'meter, 'graph, 'spectrum, 'scale
  (if (not variables-dialog) (make-variables-dialog))
  (let ((page-info (assoc page-name variables-pages)))
    (if (not page-info)
	(let* ((vbox (gtk_vbox_new #f 0))
	       (tab (gtk_label_new page-name)))
	  (gtk_widget_show tab)
	  (gtk_widget_show vbox)
	  (gtk_notebook_append_page (GTK_NOTEBOOK variables-notebook) vbox tab)
	  (set! page-info (cons page-name vbox))
	  (set! variables-pages (cons page-info variables-pages))))
    (let* ((pane (cdr page-info))
	   (var-label (string-append variable-name ":")))
      (case type
	((text)
	 ;; add a horizontal pair: label text
	 (let* ((label (gtk_label_new var-label))
		(hbox (gtk_hbox_new #f 0))
		(text (gtk_label_new "")))
	   (gtk_box_pack_start (GTK_BOX pane) hbox #f #f 2)
	   (gtk_widget_show hbox)
	   (gtk_box_pack_start (GTK_BOX hbox) label #f #f 6)
	   (gtk_misc_set_alignment (GTK_MISC (GTK_LABEL label)) 0.05 0.0)
	   (gtk_widget_show label)
	   (gtk_box_pack_start (GTK_BOX hbox) text #t #t 6)
	   (gtk_misc_set_alignment (GTK_MISC (GTK_LABEL text)) 0.05 0.0)
	   (gtk_widget_show text)
	   text))
	((scale)
	 (let* ((label (gtk_label_new var-label))
		(hbox (gtk_hbox_new #f 0))
		(scale (gtk_progress_bar_new)))
	   (gtk_box_pack_start (GTK_BOX pane) hbox #f #f 2)
	   (gtk_widget_show hbox)
	   (gtk_box_pack_start (GTK_BOX hbox) label #f #f 6)
	   (gtk_misc_set_alignment (GTK_MISC (GTK_LABEL label)) 0.05 0.0)
	   (gtk_widget_show label)
	   (gtk_box_pack_start (GTK_BOX hbox) scale #f #f 6)
	   (gtk_progress_bar_set_orientation (GTK_PROGRESS_BAR scale) GTK_PROGRESS_LEFT_TO_RIGHT)
	   ;(gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR scale) GTK_PROGRESS_CONTINUOUS)
	   (gtk_widget_show scale)
	   (list scale (car range) (cadr range))))
	((meter)
	 ;; using the level meters in snd-gtk.scm
	 (let* ((height 70)
		(width 210)
		(label (gtk_label_new var-label)))
	   (gtk_box_pack_start (GTK_BOX pane) label #f #f 2)
	   (make-level-meter pane width height)))
	((graph)
	 (let* ((snd (make-variable-graph pane (string-append variable-name ": time") 2048 (mus-srate))))
	   (list snd (channel-data snd 0))))
	((spectrum)
	 (let* ((snd (make-variable-graph pane variable-name 2048 (mus-srate))))
	   (set! (time-graph? snd 0) #f)
	   (set! (transform-graph? snd 0) #t)
	   (set! (x-axis-label snd 0 transform-graph) (string-append variable-name ": frequency"))
	   (list snd (channel-data snd 0))))
	(else #f)))))

(define (variable-display var widget)

  (define (force-update wid)
    (gdk_window_invalidate_rect (GDK_WINDOW (.window (GTK_WIDGET wid))) (list 'GdkRectangle_ 0) #t)
    (gdk_window_process_updates (GDK_WINDOW (.window (GTK_WIDGET wid))) #t))

  (define (widget? w) (and (list? w) (= (length w) 2) (eq? (car w) 'GtkWidget_)))

  (if (widget? widget)
      (if (GTK_IS_LABEL widget)
	  (begin
	    (gtk_label_set_text (GTK_LABEL widget) (object->string var))
	    (force-update widget)))

      (if (list? widget)
	  (if (number? (car widget))
	      ;; graph/spectrum -- does this need an explicit update?
	      (let* ((snd (car widget))
		     (data (cadr widget))
		     (frames (sound-data-length data))
		     (loc (cursor snd 0)))
		(sound-data-set! data 0 loc var)
		(if (time-graph? snd) (update-time-graph snd))
		(if (transform-graph? snd) (update-transform-graph snd))
		(if (= (+ loc 1) frames)
		    (set! (cursor snd 0) 0)
		    (set! (cursor snd 0) (+ loc 1))))
	      (if (GTK_IS_PROGRESS_BAR (car widget))
		  ;; "thermometer"
		  (let ((y0 (cadr widget))
			(y1 (caddr widget)))
;		    (display (max 0.0 (min 1.0 (/ (- var y0) (- y1 y0)))))
		    ;; (define wid (make-variable-display "do-loop" "i*2" 'scale))
		    (gtk_progress_bar_set_fraction 
		     (GTK_PROGRESS_BAR (car widget))
		     (max 0.0 (min 1.0 (/ (- var y0) (- y1 y0))))))
		  ;; level meter
		  (begin
		    (list-set! widget 1 var)
		    (display-level widget)
		    (force-update (car widget)))))))
  var)

(define (notebook-with-top-tabs)
  (gtk_notebook_set_tab_pos (GTK_NOTEBOOK (list-ref (main-widgets) 5)) GTK_POS_TOP))



;;; -------- font selector --------

(define font-selector-dialog #f)
(define font-selectors '())

(define (make-font-selector-dialog)
  (if (not font-selector-dialog)
      (let ((dismiss-button (gtk_button_new_with_label "Dismiss"))
	    (help-button (gtk_button_new_with_label "Help")))
	(gtk_widget_set_name dismiss-button "quit_button")
	(gtk_widget_set_name help-button "help_button")
	(set! font-selector-dialog (gtk_font_selection_dialog_new "Choose a Font"))
	(gtk_window_set_default_size (GTK_WINDOW font-selector-dialog) -1 -1)
	(gtk_window_set_resizable (GTK_WINDOW font-selector-dialog) #t)
	(gtk_widget_realize font-selector-dialog)
	(g_signal_connect font-selector-dialog "delete_event" (lambda (w ev data) (gtk_widget_hide font-selector-dialog) #t) #f)
	(gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG font-selector-dialog))) dismiss-button #t #t 20)
	(g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide font-selector-dialog)) #f)
	(gtk_widget_show dismiss-button)
	(gtk_box_pack_end (GTK_BOX (.action_area (GTK_DIALOG font-selector-dialog))) help-button #t #t 20)
	(g_signal_connect help-button "clicked" 
			  (lambda (w data) 
			    (help-dialog "Choose a Font" 
					 "choose a font, set which fields you want to use that font, and click 'ok'"))
			  #f)
	(gtk_widget_show help-button)
	(let ((ok-button (.ok_button (GTK_FONT_SELECTION_DIALOG font-selector-dialog))))
	  (gtk_widget_set_name ok-button "doit_button")
	  (g_signal_connect ok-button "clicked"
			    (lambda (w d) 
			      (let ((new-font (gtk_font_selection_dialog_get_font_name (GTK_FONT_SELECTION_DIALOG font-selector-dialog))))
				(for-each 
				 (lambda (fd)
				   (let ((button (car fd))
					 (func (cadr fd)))
				     (if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON button))
					 (set! (func) new-font))))
				 font-selectors)
				(if (> (length (sounds)) 0)
				    (begin
				      (if (time-graph?) (update-time-graph))
				      (if (transform-graph?) (update-transform-graph))
				      (if (lisp-graph?) (update-lisp-graph))))))
			    #f))
	(let ((cancel (.cancel_button (GTK_FONT_SELECTION_DIALOG font-selector-dialog))))
	  (gtk_widget_set_name cancel "reset_button")
	  (g_signal_connect cancel "clicked"
			    (lambda (w d) 
			      (for-each 
			       (lambda (fd)
				 (let ((func (cadr fd))
				       (old-value (caddr fd)))
				   (set! (func) old-value)))
			       font-selectors)
			      (if (> (length (sounds)) 0)
				  (begin
				    (if (time-graph?) (update-time-graph))
				    (if (transform-graph?) (update-transform-graph))
				    (if (lisp-graph?) (update-lisp-graph)))))
			    #f))
	(let ((mainform (.vbox (GTK_DIALOG font-selector-dialog))))
	  (let ((label (gtk_label_new "Apply font to:")))
	    (gtk_box_pack_start (GTK_BOX mainform) label #f #f 4)
	    (gtk_misc_set_alignment (GTK_MISC label) 0.1 0.0)
	    (gtk_widget_show label))
	  (for-each
	   (lambda (title func)
	     (let* ((button (gtk_toggle_button_new_with_label (symbol->string title))))
	       (gtk_box_pack_start (GTK_BOX mainform) button #f #f 4)
	       (set! font-selectors (cons (list button func (func)) font-selectors))
	       (gtk_widget_show button)))
	   (list 'axis-label-font 'axis-numbers-font 'bold-peaks-font 'peaks-font 'listener-font 'tiny-font)
	   (list axis-label-font axis-numbers-font bold-peaks-font peaks-font listener-font tiny-font)))
	(add-to-menu 3 "Choose Font" 
		     (lambda () 
		       (gtk_widget_show font-selector-dialog)))
	)))


;;; -------- color selector --------

(define color-selector-dialog #f)
(define color-selectors '())

(define (make-color-selector-dialog)
  (if (not color-selector-dialog)
      (let ((dismiss-button (gtk_button_new_with_label "Dismiss"))
	    (help-button (gtk_button_new_with_label "Help")))
	(gtk_widget_set_name dismiss-button "quit_button")
	(gtk_widget_set_name help-button "help_button")
	(set! color-selector-dialog (gtk_color_selection_dialog_new "Choose a Color"))
	(gtk_window_set_default_size (GTK_WINDOW color-selector-dialog) -1 -1)
	(gtk_color_selection_set_has_palette (GTK_COLOR_SELECTION (.colorsel (GTK_COLOR_SELECTION_DIALOG color-selector-dialog))) #t)
	(gtk_window_set_resizable (GTK_WINDOW color-selector-dialog) #t)
	(gtk_widget_realize color-selector-dialog)
	(g_signal_connect color-selector-dialog "delete_event" (lambda (w ev data) (gtk_widget_hide color-selector-dialog) #t) #f)
	(gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG color-selector-dialog))) dismiss-button #t #t 20)
	(g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide color-selector-dialog)) #f)
	(gtk_widget_show dismiss-button)
	(gtk_box_pack_end (GTK_BOX (.action_area (GTK_DIALOG color-selector-dialog))) help-button #t #t 20)
	(g_signal_connect help-button "clicked" 
			  (lambda (w data) 
			    (help-dialog "Choose a Color" 
					 "choose a color, set which fields you want to use that color, and click 'ok'"))
			  #f)
	(gtk_widget_show help-button)
	(let ((ok-button (.ok_button (GTK_COLOR_SELECTION_DIALOG color-selector-dialog))))
	  (gtk_widget_set_name ok-button "doit_button")
	  (g_signal_connect ok-button "clicked"
			    (lambda (w d) 
			      (let ((color (GdkColor))
				    (new-color #f))
				(gtk_color_selection_get_current_color 
				 (GTK_COLOR_SELECTION (.colorsel (GTK_COLOR_SELECTION_DIALOG color-selector-dialog)))
				 color)
				(set! new-color (gdk_color_copy color))
				(gdk_rgb_find_color (gdk_colormap_get_system) new-color)
				(for-each 
				 (lambda (fd)
				   (let ((button (car fd))
					 (func (cadr fd)))
				     (if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON button))
					 (set! (func) new-color))))
				 color-selectors)
				(if (> (length (sounds)) 0)
				    (begin
				      (if (time-graph?) (update-time-graph))
				      (if (transform-graph?) (update-transform-graph))
				      (if (lisp-graph?) (update-lisp-graph))))))
			    #f))
	(let ((cancel (.cancel_button (GTK_COLOR_SELECTION_DIALOG color-selector-dialog))))
	  (gtk_widget_set_name cancel "reset_button")
	  (g_signal_connect cancel "clicked"
			    (lambda (w d) 
			      (for-each 
			       (lambda (fd)
				 (let ((func (cadr fd))
				       (old-value (caddr fd)))
				   (set! (func) old-value)))
			       color-selectors)
			      (if (> (length (sounds)) 0)
				  (begin
				    (if (time-graph?) (update-time-graph))
				    (if (transform-graph?) (update-transform-graph))
				    (if (lisp-graph?) (update-lisp-graph)))))
			    #f))
	(let ((mainform (.vbox (GTK_DIALOG color-selector-dialog))))
	  (let ((label (gtk_label_new "Apply color to:")))
	    (gtk_box_pack_start (GTK_BOX mainform) label #f #f 4)
	    (gtk_misc_set_alignment (GTK_MISC label) 0.1 0.0)
	    (gtk_widget_show label))
	  (let ((table (gtk_table_new 12 2 #f))
		(row 0)
		(column 0))
	    (gtk_box_pack_start (GTK_BOX mainform) table #t #t 4)
	    (gtk_widget_show table)
	    (for-each
	     (lambda (title func)
	       (let* ((button (gtk_toggle_button_new_with_label (symbol->string title))))
		 (gtk_table_attach_defaults (GTK_TABLE table) button column (1+ column) row (1+ row))
		 (set! row (1+ row))
		 (if (= row 12)
		     (begin
		       (set! row 0)
		       (set! column 1)))
		 (set! color-selectors (cons (list button func (func)) color-selectors))
		 (gtk_widget_show button)))
	     (list 'basic-color 'cursor-color 'data-color 'doit-button-color 'doit-again-button-color 'enved-waveform-color
		   'filter-control-waveform-color 'graph-color 'help-button-color 'highlight-color 'listener-color
		   'listener-text-color 'mark-color 'mix-color 'position-color 'pushed-button-color 'quit-button-color
		   'reset-button-color 'sash-color 'selected-data-color 'selected-graph-color 'selection-color
		   'text-focus-color 'zoom-color)
	     (list basic-color cursor-color data-color doit-button-color doit-again-button-color enved-waveform-color
		   filter-control-waveform-color graph-color help-button-color highlight-color listener-color
		   listener-text-color mark-color mix-color position-color pushed-button-color quit-button-color
		   reset-button-color sash-color selected-data-color selected-graph-color selection-color
		   text-focus-color zoom-color))))
	(add-to-menu 3 "Choose Color" 
		     (lambda () 
		       (gtk_widget_show color-selector-dialog)))
	)))


(define (add-main-menu-mnemonics)
  ;; thanks to Maxim Krikun
  (gtk_label_set_text_with_mnemonic (GTK_LABEL (gtk_bin_get_child (GTK_BIN (main-menu 0)))) "_File") 
  (gtk_label_set_text_with_mnemonic (GTK_LABEL (gtk_bin_get_child (GTK_BIN (main-menu 1)))) "_Edit") 
  (gtk_label_set_text_with_mnemonic (GTK_LABEL (gtk_bin_get_child (GTK_BIN (main-menu 2)))) "_View") 
  (gtk_label_set_text_with_mnemonic (GTK_LABEL (gtk_bin_get_child (GTK_BIN (main-menu 3)))) "_Options") 
  (gtk_label_set_text_with_mnemonic (GTK_LABEL (gtk_bin_get_child (GTK_BIN (main-menu 4)))) "_Help"))



#|
;;; this code changes the main window's cursor, based on some obsolete code in the gdk documentation
;;;   unfortunately, it has no effect on the text widget's cursor

(define* (istring :rest ints)
  (apply string (map integer->char ints)))

(define cursor-bits (istring
  #x80 #x01 #x40 #x02 #x20 #x04 #x10 #x08 #x08 #x10 #x04 #x20
  #x82 #x41 #x41 #x82 #x41 #x82 #x82 #x41 #x04 #x20 #x08 #x10
  #x10 #x08 #x20 #x04 #x40 #x02 #x80 #x01))

(define cursor-mask (istring
  #x80 #x01 #xc0 #x03 #x60 #x06 #x30 #x0c #x18 #x18 #x8c #x31
  #xc6 #x63 #x63 #xc6 #x63 #xc6 #xc6 #x63 #x8c #x31 #x18 #x18
  #x30 #x0c #x60 #x06 #xc0 #x03 #x80 #x01))

(define blue-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "blue" tmp)
    (let ((col (gdk_color_copy tmp)))
      (gdk_rgb_find_color (gdk_colormap_get_system) col)
      col)))

(define black-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "black" tmp)
    (let ((col (gdk_color_copy tmp)))
      (gdk_rgb_find_color (gdk_colormap_get_system) col)
      col)))

(let* ((listener-text (list-ref (main-widgets) 4))
       (source (gdk_bitmap_create_from_data (GDK_DRAWABLE (car (main-widgets))) cursor-bits 16 16))
       (mask (gdk_bitmap_create_from_data (GDK_DRAWABLE (car (main-widgets))) cursor-mask 16 16))
       (new-cursor (gdk_cursor_new_from_pixmap (GDK_PIXMAP source) (GDK_PIXMAP mask) blue-pixel black-pixel 8 8)))
  (gdk_window_set_cursor (car (main-widgets)) new-cursor))
|#
