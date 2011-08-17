;;; translations from snd-motif.scm
;;;
;;; display-scanned-synthesis
;;; zync and unzync
;;; disable control panel
;;; show-disk-space
;;; remove top level menu
;;; keep-file-dialog-open-upon-ok
;;; snd-clock-icon
;;; bring possibly-obscured dialog to top
;;; select-file
;;; with-level-meters
;;; add delete and rename options to the file menu
;;; notebook-with-top-tabs
;;; make-font-selector-dialog
;;; add-main-menu-mnemonics

(if (not (provided? 'snd-gtk)) (snd-error "snd-gtk.scm is Gtk-specific"))


(if (not (provided? 'xg))
    (let ((hxm (dlopen "xg.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-gtk.scm needs the xg module (either 'make xg' or build Snd with --with-static-xg): ~A" hxm))
	  (dlinit hxm "Init_libxg"))))

(provide 'snd-snd-gtk.scm)

(if (not (provided? 'snd-extensions.scm)) (load "extensions.scm"))
(if (not (provided? 'snd-play.scm)) (load "play.scm"))

(define (load-font name)
  (pango_font_description_from_string name))

(define (g-list-foreach glist func)
  (let ((len (g_list_length glist)))
    (do ((i 0 (+ 1 i)))
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
	 (substring (val 4) 0 (val 3)))))


(define red-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "red" tmp)
    (gdk_color_copy tmp)))

(define white-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "white" tmp)
    (gdk_color_copy tmp)))

(define black-pixel
  (let ((tmp (GdkColor)))
    (gdk_color_parse "black" tmp)
    (gdk_color_copy tmp)))


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
	      (v (+ size i))
	      (if (>= i size)
		  (v (- i size))
		  (v i)))))
      (let* ((dm (/ damp mass))
	     (km (/ xspring mass))
	     (denom (+ 1.0 dm))
	     (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	     (p2 (/ km denom))
	     (p3 (/ -1.0 denom)))
	(do ((i 0 (+ 1 i)))
	    ((= i size))
	  (set! (x0 i) (min (+ (* p1 (x1 i))
			       (* p2 (+ (circle-vct-ref x1 (- i 1)) (circle-vct-ref x1 (+ i 1))))
			       (* p3 (x2 i)))
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
	 (pts1 #f)
	 (ax0 0) (ax1 0) (ay0 0) (ay1 0)
	 (gc (car (snd-gcs)))
	 
	 ;; now set up a paned window in the main Snd window with controllers on the left and the graph on the right
	 (scan-outer (let ((pane (gtk_box_new GTK_ORIENTATION_HORIZONTAL 0)))
		       (gtk_box_pack_start (GTK_BOX ((main-widgets) 5)) pane #f #f 4)
		       (gtk_widget_show pane)
		       pane))
	 (scan-row (let ((box (gtk_box_new GTK_ORIENTATION_VERTICAL 0)))
		     (gtk_box_pack_start (GTK_BOX scan-outer) box #f #f 0)
		     (gtk_widget_show box)
		     box))
	 ;; the graph
	 (scan-pane (let ((grf (gtk_drawing_area_new)))
		      (gtk_widget_set_events grf GDK_ALL_EVENTS_MASK)
		      (gtk_box_pack_start (GTK_BOX scan-outer) grf #t #t 0)
		      (gtk_widget_show grf)
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
		(round (+ ay0
			  (* range (- 10.0 y)))))))

    (define (cairo-draw-lines cr data size)
      (cairo_set_line_width cr 4.0)
      (cairo_move_to cr (data 0) (data 1))
      (do ((i 1 (+ 1 i))
	   (j 2 (+ j 2)))
	  ((= i size))
	(cairo_line_to cr (data j) (data (+ j 1))))
      (cairo_stroke cr))

    (define (draw-graph cr)
      (if (and (> ax1 ax0)
	       (> ay1 ay0))
	  (let ((diff (* 0.05 (- ay1 ay0))) ; assuming -10 to 10 
		(xincr (/ (- ax1 ax0) size))
		(bg-color (color->list (basic-color))))
	    (cairo_set_source_rgb cr (car bg-color) (cadr bg-color) (caddr bg-color))
	    (if pts1
		(cairo-draw-lines cr pts1 size)
		(begin
		  (cairo_rectangle cr (+ ax0 2) ay0 (- ax1 ax0 2) (- ay1 ay0))
		  (cairo_fill cr)))
	    (cairo_set_source_rgb cr 0.0 0.0 0.0)
	    (cairo_set_line_width cr 1.0)
	    (let ((x (floor ax0))
		  (y (y->grfy (gx0 0) diff)))
	      (cairo_move_to cr x y)
	      (set! (vect 0) x)
	      (set! (vect 1) y))
	    (do ((i 1 (+ 1 i))
		 (j 2 (+ j 2))
		 (xi (+ ax0 xincr) (+ xi xincr)))
		((= i size))
	      (let ((x (floor xi))
		    (y (y->grfy (gx0 i) diff)))
		(set! (vect j) x)
		(set! (vect (+ j 1)) y)
		(cairo_line_to cr x y)))
	    (cairo_stroke cr)
	    (set! pts1 vect))))
    
    (define (redraw-graph)
      (let* ((wn ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) (gtk_widget_get_window scan-pane)))
	     (cr (gdk_cairo_create wn)))
	(set! bounds (draw-axes scan-pane gc "scanned synthesis" 0.0 1.0 -10.0 10.0 x-axis-in-seconds show-all-axes cr))
	(set! ax0 (+ (car bounds) 4))
	(set! ax1 (caddr bounds))
	(set! ay1 (cadr bounds))
	(set! ay0 (cadddr bounds))
	(draw-graph cr)
	(cairo_destroy cr)))
    
    (define (tick-synthesis n)
      ;; background process
      (compute-uniform-circular-string size gx0 gx1 gx2 mass xspring damp)
      (let* ((wn ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) (gtk_widget_get_window scan-pane)))
	     (cr (gdk_cairo_create wn)))
	(draw-graph cr)
	(cairo_destroy cr))
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
      (do ((i 0 (+ 1 i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (set! (gx1 (+ i (- (/ size 4) 6))) val)))
      (set! work-proc (g_idle_add tick-synthesis #f)))
    
    (define (continue-synthesis)
      (stop-synthesis)
      (set! work-proc (g_idle_add tick-synthesis #f)))
    
    ;; controller callbacks
    (for-each 
     (lambda (data)
       (let* ((title (data 0))
	      (minval (data 1))
	      (maxval (data 2))
	      (curval (data 3))
	      (decimals (data 4))
	      (func (data 5))
	      (adj (gtk_adjustment_new curval minval maxval 1.0 10.0 1.0))
	      (scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT adj)))
	      (label (gtk_label_new title)))
	 (if (not (provided? 'gtk3)) (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE scale)) GTK_UPDATE_CONTINUOUS))
	 (gtk_scale_set_digits (GTK_SCALE scale) decimals)
	 (gtk_scale_set_value_pos (GTK_SCALE scale) GTK_POS_TOP)
	 (gtk_scale_set_draw_value (GTK_SCALE scale) #t)
      	 (gtk_box_pack_start (GTK_BOX scan-row) scale #t #t 0)
	 (gtk_widget_show scale)
	 (gtk_box_pack_start (GTK_BOX scan-row) label #t #t 0)
	 (gtk_widget_show label)
	 (g_signal_connect adj "value_changed" (lambda (w d) (func (gtk_adjustment_get_value (GTK_ADJUSTMENT adj)))) #f)))
     (list (list "mass" 1 200 100 2 (lambda (val) (set! mass (/ val 100.0))))
	   (list "spring" 1 100 10 2 (lambda (val) (set! xspring (/ val 100.0))))
	   (list "damping" 0 100 0 4 (lambda (val) (set! damp (/ val 10000.0))))))
    
    (let ((scan-size (gtk_box_new GTK_ORIENTATION_HORIZONTAL 4)))
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
				      ((not (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON play-button)))
				       (mus-audio-close audio-fd))
				    (tick-synthesis work-proc)
				    (do ((k 0 (+ 1 k)))
					((= k frames))
				      (sound-data-set! data 0 k (* amplitude (table-lookup tbl))))
				    (mus-audio-write audio-fd data frames))))))
		      #f)
    (let* ((freq-adj (gtk_adjustment_new 440.0 20.0 1000.0 1.0 10.0 1.0)) ; step incr, page incr page size
	   (amp-adj (gtk_adjustment_new 0.02 0.0 0.1 .001 .01 .001))
	   (freq-scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT freq-adj)))
	   (amp-scale (gtk_scale_new GTK_ORIENTATION_HORIZONTAL (GTK_ADJUSTMENT amp-adj)))
	   (freq-label (gtk_label_new "frequency"))
	   (amp-label (gtk_label_new "amplitude")))
      (if (not (provided? 'gtk3)) (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE freq-scale)) GTK_UPDATE_CONTINUOUS))
      (if (not (provided? 'gtk3)) (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE amp-scale)) GTK_UPDATE_CONTINUOUS))
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
      (g_signal_connect freq-adj "value_changed" (lambda (w d) (set! (mus-frequency tbl) (gtk_adjustment_get_value (GTK_ADJUSTMENT freq-adj)))) #f)
      (g_signal_connect amp-adj "value_changed" (lambda (w d) (set! amplitude (gtk_adjustment_get_value (GTK_ADJUSTMENT amp-adj)))) #f)
      )
    
    (g_signal_connect scan-pane 
		      (if (provided? 'gtk3) "draw" "expose_event")
		      (lambda (w e d) 
			(redraw-graph)) 
		      #f)
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

  
  

;;; -------- zync and unzync: start or stop y-zoom slider sync --------
;;; 
;;; (i.e. when one y-zoom-slider changes position, all other channels in the sound change in parallel)
;;; (zync) to start and (unzync) to stop


(define (add-dragger snd)

  (define (dragger-callback adj context)
    (let ((val (- 1.0 (gtk_adjustment_get_value (GTK_ADJUSTMENT adj))))
	  (snd (car context))
	  (chn (cadr context)))
      (if (sound-property 'dragger snd)
	  (begin
	    (do ((i 0 (+ 1 i)))
		((= i (channels snd)))
	      (if (not (= i chn))
		  (begin
		    (set! (y-zoom-slider snd i) (* val val))
		    (set! (y-position-slider snd i) (y-position-slider snd chn)))))
	    (g_signal_stop_emission (GPOINTER adj)
				    (g_signal_lookup "value_changed" (G_OBJECT_TYPE (G_OBJECT adj)))
				    0)))))
      
  (set! (sound-property 'dragger snd) #t)
  (set! (sound-property 'save-state-ignore snd)
	(cons 'dragger
	      (or (sound-property 'save-state-ignore snd)
		  (list 'save-state-ignore))))
  (do ((chn 0 (+ 1 chn)))
      ((= chn (channels snd)))
    (let ((zy ((channel-widgets snd chn) 14)))
      (g_signal_connect_closure_by_id (GPOINTER zy)
				      (g_signal_lookup "value_changed" (G_OBJECT_TYPE (G_OBJECT zy)))
				      0
				      (g_cclosure_new dragger-callback (list snd chn) (list 'GClosureNotify 0))
				      #f))))

(define (zync)
  "(zync) ties each sound's y-zoom sliders together so that all change in paralle if one changes"
  (hook-push after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (not (sound-property 'dragger n))
	 (add-dragger n)))
   (sounds)))

(define (unzync)
  "(unzync) undoes a previous (zync) -- subsequently each sound's y-zoom sliders are independent"
  (hook-remove after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (sound-property 'dragger n)
	 (set! (sound-property 'dragger n) #f)))
   (sounds)))



;;; -------- disable control panel --------

(define (disable-control-panel snd)
  (gtk_widget_hide (caddr (sound-widgets snd)))
  (remove-from-menu 2 "Show controls"))



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

    (define (find-if pred l)
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l)))))

    (lambda* (snd-arg)
      "(show-disk-space snd) adds a label to snd's minibuffer area showing the current free space (for use with after-open-hook)"

      (let* ((snd (or snd-arg (selected-sound)))
	     (previous-label (find-if (lambda (n) (equal? (car n) snd)) labelled-snds)))
	(if (not previous-label)
	    (if (not snd)
		(snd-error "no sound found for disk space label")
		(let* ((name-form ((sound-widgets) 10))
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
  (gtk_widget_hide ((menu-widgets) menu)))


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

(define snd-clock-icon
  (lambda (snd hour)
    (let* ((window ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) (gtk_widget_get_window ((sound-widgets snd) 8))))
	   (cr (gdk_cairo_create window))
	   (bg (color->list (basic-color))))
      (cairo_set_source_rgb cr (car bg) (cadr bg) (caddr bg))
      (cairo_rectangle cr 0 0 16 16) ; icon bg
      (cairo_fill cr)
      (cairo_set_source_rgb cr 1.0 1.0 1.0)
      (cairo_arc cr 8 8 7 0 (* 2 pi))  ; clock face
      (cairo_fill cr)
      (cairo_set_line_width cr 2.0)
      (cairo_set_source_rgb cr 0.0 0.0 0.0)
      (cairo_move_to cr 8 8)         ; clock hour hand
      (cairo_line_to cr (+ 8 (* 7 (sin (* hour (/ 3.1416 6.0)))))
		        (- 8 (* 7 (cos (* hour (/ 3.1416 6.0))))))
      (cairo_stroke cr)
      (cairo_destroy cr))))


#|
;;; this is the happy face progress bar

(define (snd-happy-face snd progress)
  (let* ((window ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) (gtk_widget_get_window ((sound-widgets snd) 8))))
	 (cr (gdk_cairo_create window))
	 (bg (color->list (basic-color)))
	 (fc (list 1.0 progress 0.0)))

    ;; overall background
    (cairo_set_source_rgb cr (car bg) (cadr bg) (caddr bg))
    (cairo_rectangle cr 0 0 16 16)
    (cairo_fill cr)

    ;; round face
    (cairo_set_source_rgb cr (car fc) (cadr fc) (caddr fc))
    (cairo_arc cr 8 8 8 0.0 (* 2 pi))
    (cairo_fill cr)

    ;; eyes
    (cairo_set_source_rgb cr 0.0 0.0 0.0)
    (cairo_arc cr 5 6 1.5 0 (* 2 pi))
    (cairo_fill cr)

    (cairo_arc cr 11 6 1.5 0 (* 2 pi))
    (cairo_fill cr)

    ;; mouth
    (cairo_set_line_width cr 1.0)
    (if (< progress 0.4)
	(cairo_arc cr 8 14 4 (* 17/16 pi) (* -1/16 pi))
	(if (< progress 0.7)
	    (begin
	      (cairo_move_to cr 4 12)
	      (cairo_rel_line_to cr 8 0))
	    (cairo_arc cr 8 8 5 (* 1/16 pi) (* 15/16 pi))))
    (cairo_stroke cr)

    (cairo_destroy cr)))

|#


;;; -------- bring possibly-obscured dialog to top

(define (raise-dialog w)
  (gtk_widget_show w)
  (gtk_window_present (GTK_WINDOW w)))


;;; -------- select-file --------
;;;
;;; (select-file func title dir filter help)
;;;   starts a File Chooser Dialog, runs func if a file is selected
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
		(set! ((car ds) 1) #t)
		(caar ds))
	      (find-free-dialog (cdr ds)))))
#|
    (define (find-dialog-widget wid ds)
      (if (null? ds)
	  #f
	  (if (equal? wid (caar ds))
	      (car ds)
	      (find-dialog-widget wid (cdr ds)))))
|#
    (lambda args
      ;; (file-select func title dir filter help)
      (let* ((func (if (> (length args) 0) (args 0) #f))
	     (title (if (> (length args) 1) (args 1) "select file"))
	     (dir (if (> (length args) 2) (args 2) "."))
	     ;; (filter (if (> (length args) 3) (args 3) "*"))
	     (dialog (or (find-free-dialog file-selector-dialogs)
			 (GTK_FILE_CHOOSER_DIALOG (gtk_file_chooser_dialog_new
						   title
						   #f
						   ;(GTK_WINDOW (cadr (main-widgets)))
						   GTK_FILE_CHOOSER_ACTION_OPEN
						   (list GTK_STOCK_CANCEL GTK_RESPONSE_REJECT
							 GTK_STOCK_OK GTK_RESPONSE_ACCEPT))))))	
	(gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER dialog) dir)
	(if (and (= GTK_RESPONSE_ACCEPT (gtk_dialog_run (GTK_DIALOG dialog)))
		 func)
	    (func (gtk_file_chooser_get_filename (GTK_FILE_CHOOSER dialog))))
	(gtk_widget_hide (GTK_WIDGET dialog))))))

;;(select-file (lambda (n) (snd-print n)))


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
	(g_signal_connect meter 
			  (if (provided? 'gtk3) "draw" "expose_event")
			  (lambda (w e d) 
			    (display-level d)) 
			  context)
	(g_signal_connect meter "configure_event" 
			  (lambda (w e d)
			    (let ((xy (if (provided? 'gtk2)
					  (gdk_drawable_get_size ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) (gtk_widget_get_window w)))
					  (list (gtk_widget_get_allocated_width w)
						(gtk_widget_get_allocated_height w)))))
			      (set! (d 5) (car xy))
			      (set! (d 6) (cadr xy))
			      (display-level d)))
			  context)
	context))))

(define (display-level meter-data)
  (let* ((meter (car meter-data))
	 (level (meter-data 1))
	 (last-level (meter-data 3))
	 (red-deg (meter-data 4))
	 (width (meter-data 5))
	 (height (meter-data 6))
	 ;; (size (meter-data 2))
	 (win ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) (gtk_widget_get_window meter))))

    ;; this is too slow -- can we save the plate? (also if just 1 meter, put pivot higher?)
    (let ((cr (gdk_cairo_create win)))
      
      ;; put our origin at the meter pivot point scaled (as a square so the dial remains circular) to 0..1
      (cairo_translate cr (* 0.5 width) (+ (* 0.5 width) (* 0.2 height)))
      (cairo_scale cr width width)
      
      ;; background
      (let ((pat (cairo_pattern_create_radial 0 0 .1 0 0 0.75)))
	(cairo_pattern_add_color_stop_rgb pat 0.0 1.0 0.9 0.0) 
	(cairo_pattern_add_color_stop_rgb pat 1.0 1.0 1.0 1.0)
	(cairo_rectangle cr -1 -1 2 2)
	(cairo_set_source cr pat)
	(cairo_fill cr)
	(cairo_pattern_destroy pat))
      
      ;; dial markings
      (cairo_set_source_rgb cr 0.0 0.0 0.0)
      
      ;; outer arc
      (cairo_set_line_width cr (/ 2.0 width))
      (cairo_arc cr 0 0 0.5 (* -0.75 pi) (* -0.25 pi))
      (cairo_stroke cr)
      
      ;; inner arc
      (cairo_set_line_width cr (/ 0.5 width))
      (cairo_arc cr 0 0 (- 0.5 (/ 6.0 width)) (* -0.75 pi) (* -0.25 pi))
      (cairo_stroke cr)
      
      ;; save unrotated coords
      (cairo_save cr)
      
      ;; ticks
      (cairo_rotate cr (* 5 (/ pi 4)))
      (do ((i 0 (+ 1 i)))
	  ((= i 5))
	(cairo_set_line_width cr (/ 1.5 width))
	(if (or (= i 0) (= i 4))
	    (begin
	      (cairo_move_to cr (- 0.5 (/ 6.0 width)) 0.0)
	      (cairo_rel_line_to cr (/ 15.0 width) 0))
	    (begin
	      (cairo_move_to cr 0.5 0.0)
	      (cairo_rel_line_to cr (/ 9.0 width) 0)))
	(cairo_stroke cr)
	(if (< i 4)
	    (begin
	      (cairo_set_line_width cr (/ 0.5 width))
	      (do ((j 0 (+ 1 j)))
		  ((= j 5))
		(cairo_move_to cr 0.5 0.0)
		(cairo_rel_line_to cr (/ 6.0 width) 0)
		(cairo_rotate cr (/ pi (* 8 5)))
		(cairo_stroke cr)))))
      (cairo_restore cr)
      
      ;; needle and bubble
      (let* ((needle-speed 0.25)
	     (bubble-speed 0.025)
	     (bubble-size (/ pi 12))
	     (val (+ (* level needle-speed) (* last-level (- 1.0 needle-speed)))))
	(cairo_save cr)
	(cairo_set_line_width cr (/ 2.0 width))
	(cairo_rotate cr (+ (* 5 (/ pi 4)) (* val pi 0.5)))
	(cairo_move_to cr 0 0)
	(cairo_rel_line_to cr 0.55 0.0)
	(cairo_stroke cr)
	(cairo_restore cr)
	
	(set! (meter-data 3) val)
	(if (<= val red-deg)
	    (set! val (+ (* val bubble-speed) (* red-deg (- 1.0 bubble-speed)))))
	(set! (meter-data 4) val)
	
	;; now the red bubble...
	(if (> val .01)
	    (begin
	      (cairo_set_source_rgb cr 1.0 0.0 0.0)
	      (cairo_set_line_width cr (/ 5.0 width))
	      (let ((redx (* val 0.5 pi)))
		(cairo_arc cr 0 0 (- 0.5 (/ 3.0 width))  (+ (* 5 (/ pi 4)) (max 0.0 (- redx bubble-size))) (+ (* 5 (/ pi 4)) redx))
		(cairo_stroke cr)))))
      
      (cairo_destroy cr))))


(define (with-level-meters n)
  ;; add n level meters to a pane at the top of the Snd window
  (let* ((parent ((main-widgets) 5))
	 (height (if (> n 2) 70 85))
	 (parent-width (cadr (gdk_drawable_get_size ((if (provided? 'gtk3) GDK_WINDOW GDK_DRAWABLE) (gtk_widget_get_window parent)))))
	 (width (floor (/ parent-width n)))
	 (meters (gtk_box_new GTK_ORIENTATION_HORIZONTAL 4))
	 (meter-list '()))
    (gtk_box_pack_start (GTK_BOX parent) meters #f #f 4)
    (gtk_widget_set_size_request meters width height)
    (gtk_widget_show meters)
    (do ((i 0 (+ 1 i)))
	((= i n))
      (set! meter-list (cons (make-level-meter meters width height) meter-list)))
    (hook-push dac-hook 
	       (lambda (sdobj)
		 (let ((maxes (sound-data-maxamp sdobj)))
		   (for-each
		    (lambda (meter)
		      (if (null? maxes)
			  (set! (meter 1) 0.0)
			  (begin
			    (set! (meter 1) (car maxes))
			    (display-level meter)
			    (set! maxes (cdr maxes)))))
		    (reverse meter-list)))))
    (hook-push stop-dac-hook
	       (lambda () ; drain away the bubble
		 (g_idle_add 
		  (let ((ctr 0))
		    (lambda (ignored)
		      (for-each 
		       (lambda (meter)
			 (set! (meter 1) 0.0)
			 (display-level meter))
		       meter-list)
		      (set! ctr (+ ctr 1))
		      (< ctr 200)))
		  #f)))
    meter-list))




;;; -------- state display panel --------

(define variables-dialog #f)
(define variables-notebook #f)
(define variables-pages '())

(define (make-variables-dialog)
  (let ((dismiss-button (gtk_button_new_with_label "Go Away")))
    (gtk_widget_set_name dismiss-button "quit_button")
    (set! variables-dialog (gtk_dialog_new))
    (gtk_window_set_title (GTK_WINDOW variables-dialog) "Variables")
    (gtk_container_set_border_width (GTK_CONTAINER variables-dialog) 10)
    (gtk_window_set_default_size (GTK_WINDOW variables-dialog) -1 -1)
    (gtk_window_set_resizable (GTK_WINDOW variables-dialog) #t)
    (gtk_widget_realize variables-dialog)
    (g_signal_connect variables-dialog "delete_event" (lambda (w ev data) (gtk_widget_hide variables-dialog) #t) #f)
    (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG variables-dialog))) dismiss-button #t #t 20)
    (g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide variables-dialog)) #f)
    (gtk_widget_show dismiss-button)
    (set! variables-notebook (gtk_notebook_new))
    (gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG variables-dialog))) variables-notebook #t #t 4)
    (gtk_notebook_set_tab_pos (GTK_NOTEBOOK variables-notebook) GTK_POS_RIGHT)
    (gtk_widget_show variables-notebook)
    (gtk_widget_show variables-dialog)
    variables-dialog))

(define* (make-variable-display page-name variable-name (type 'text) (range (list 0.0 1.0)))
  ;; type = 'text, 'meter, 'graph, 'spectrum, 'scale
  (if (not variables-dialog) (make-variables-dialog))
  (let ((page-info (assoc page-name variables-pages)))
    (if (not page-info)
	(let ((vbox (gtk_box_new GTK_ORIENTATION_VERTICAL 0))
	      (tab (gtk_label_new page-name)))
	  (gtk_widget_show tab)
	  (gtk_widget_show vbox)
	  (gtk_notebook_append_page (GTK_NOTEBOOK variables-notebook) vbox tab)
	  (set! page-info (cons page-name vbox))
	  (set! variables-pages (cons page-info variables-pages))))
    (let ((pane (cdr page-info))
	  (var-label (string-append variable-name ":")))
      (case type
	((text)
	 ;; add a horizontal pair: label text
	 (let ((label (gtk_label_new var-label))
	       (hbox (gtk_box_new GTK_ORIENTATION_HORIZONTAL 0))
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
	 (let ((label (gtk_label_new var-label))
	       (hbox (gtk_box_new GTK_ORIENTATION_HORIZONTAL 0))
	       (scale (gtk_progress_bar_new)))
	   (gtk_box_pack_start (GTK_BOX pane) hbox #f #f 2)
	   (gtk_widget_show hbox)
	   (gtk_box_pack_start (GTK_BOX hbox) label #f #f 6)
	   (gtk_misc_set_alignment (GTK_MISC (GTK_LABEL label)) 0.05 0.0)
	   (gtk_widget_show label)
	   (gtk_box_pack_start (GTK_BOX hbox) scale #f #f 6)
	   (gtk_widget_show scale)
	   (list scale (car range) (cadr range))))
	((meter)
	 ;; using the level meters in snd-gtk.scm
	 (let ((height 70)
	       (width 210)
	       (label (gtk_label_new var-label)))
	   (gtk_box_pack_start (GTK_BOX pane) label #f #f 2)
	   (make-level-meter pane width height)))
	((graph)
	 (let ((snd (make-variable-graph pane (string-append variable-name ": time") 2048 (mus-srate))))
	   (list (sound->integer snd) (channel-data snd 0))))
	((spectrum)
	 (let ((snd (make-variable-graph pane variable-name 2048 (mus-srate))))
	   (set! (time-graph? snd 0) #f)
	   (set! (transform-graph? snd 0) #t)
	   (set! (x-axis-label snd 0 transform-graph) (string-append variable-name ": frequency"))
	   (list (sound->integer snd) (channel-data snd 0))))
	(else #f)))))

(define (variable-display var widget)

  (define (force-update wid)
    (gdk_window_invalidate_rect (GDK_WINDOW (gtk_widget_get_window (GTK_WIDGET wid))) (list 'GdkRectangle_ 0) #t)
    (gdk_window_process_updates (GDK_WINDOW (gtk_widget_get_window (GTK_WIDGET wid))) #t))

  (define (widget? w) (and (list? w) (= (length w) 2) (eq? (car w) 'GtkWidget_)))

  (if (widget? widget)
      (if (GTK_IS_LABEL widget)
	  (begin
	    (gtk_label_set_text (GTK_LABEL widget) (object->string var))
	    (force-update widget)))

      (if (list? widget)
	  (if (or (number? (car widget))
		  (sound? (car widget)))
	      ;; graph/spectrum -- does this need an explicit update?
	      (let* ((snd (car widget))
		     (data (cadr widget))
		     (frames (length data))
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
		    ;; (define wid (make-variable-display "do-loop" "i*2" 'scale))
		    (gtk_progress_bar_set_fraction 
		     (GTK_PROGRESS_BAR (car widget))
		     (max 0.0 (min 1.0 (/ (- var y0) (- y1 y0))))))
		  ;; level meter
		  (begin
		    (set! (widget 1) var)
		    (display-level widget)
		    (force-update (car widget)))))))
  var)

(define (notebook-with-top-tabs)
  (gtk_notebook_set_tab_pos (GTK_NOTEBOOK ((main-widgets) 5)) GTK_POS_TOP))


#|
;;; -------- font selector --------
;;; now (gtk 3.1.12) obsolete.  The new version is font_chooser_dialog.

(define font-selector-dialog #f)
(define font-selectors '())

(define (make-font-selector-dialog)
  (if (not font-selector-dialog)
      (let ((dismiss-button (gtk_button_new_with_label "Go Away"))
	    (help-button (gtk_button_new_with_label "Help")))
	(gtk_widget_set_name dismiss-button "quit_button")
	(gtk_widget_set_name help-button "help_button")
	(set! font-selector-dialog (gtk_font_selection_dialog_new "Choose a Font"))
	(gtk_window_set_default_size (GTK_WINDOW font-selector-dialog) -1 -1)
	(gtk_window_set_resizable (GTK_WINDOW font-selector-dialog) #t)
	(gtk_widget_realize font-selector-dialog)
	(g_signal_connect font-selector-dialog "delete_event" (lambda (w ev data) (gtk_widget_hide font-selector-dialog) #t) #f)
	(gtk_box_pack_start (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG font-selector-dialog))) dismiss-button #t #t 20)
	(g_signal_connect dismiss-button "clicked" (lambda (w data) (gtk_widget_hide font-selector-dialog)) #f)
	(gtk_widget_show dismiss-button)
	(gtk_box_pack_end (GTK_BOX (gtk_dialog_get_action_area (GTK_DIALOG font-selector-dialog))) help-button #t #t 20)
	(g_signal_connect help-button "clicked" 
			  (lambda (w data) 
			    (help-dialog "Choose a Font" 
					 "choose a font, set which fields you want to use that font, and click 'ok'"))
			  #f)
	(gtk_widget_show help-button)
	(let ((ok-button (gtk_font_selection_dialog_get_ok_button (GTK_FONT_SELECTION_DIALOG font-selector-dialog))))
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
	(let ((cancel (gtk_font_selection_dialog_get_cancel_button (GTK_FONT_SELECTION_DIALOG font-selector-dialog))))
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
	(let ((mainform (gtk_dialog_get_content_area (GTK_DIALOG font-selector-dialog))))
	  (let ((label (gtk_label_new "Apply font to:")))
	    (gtk_box_pack_start (GTK_BOX mainform) label #f #f 4)
	    (gtk_misc_set_alignment (GTK_MISC label) 0.1 0.0)
	    (gtk_widget_show label))
	  (for-each
	   (lambda (title func)
	     (let ((button (gtk_toggle_button_new_with_label (symbol->string title))))
	       (gtk_box_pack_start (GTK_BOX mainform) button #f #f 4)
	       (set! font-selectors (cons (list button func (func)) font-selectors))
	       (gtk_widget_show button)))
	   (list 'axis-label-font 'axis-numbers-font 'bold-peaks-font 'peaks-font 'listener-font 'tiny-font)
	   (list axis-label-font axis-numbers-font bold-peaks-font peaks-font listener-font tiny-font)))
	(add-to-menu 3 "Choose Font" 
		     (lambda () 
		       (gtk_widget_show font-selector-dialog)))
	)))
|#

#|
;;; this doesn't actually work yet, but the simpler C case below almost works
(define (emacs)
  (let ((emacs-pane (gtk_box_new GTK_ORIENTATION_VERTICAL 0)))
    (gtk_box_pack_start (GTK_BOX ((main-widgets) 5)) emacs-pane #f #f 4)
    (gtk_widget_show emacs-pane)
    (let ((socket (gtk_socket_new)))
      (gtk_container_add (GTK_CONTAINER emacs-pane) socket)
      (gtk_widget_show socket)
      (gtk_widget_realize socket)
      (let ((id (gtk_socket_get_id (GTK_SOCKET socket))))
	(system (format #f "/home/bil/test/emacs-23.1/src/emacs --parent-id=~D" id))))))

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <gtk/gtk.h>
int main(int argc, char *argv)
{
  GtkWidget *parent, *window;
  gtk_init (&argc, &argv);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  parent = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_container_add(GTK_CONTAINER (window), parent);
  {
    GtkWidget *socket = gtk_socket_new ();
    gtk_widget_show (socket);
    gtk_container_add(GTK_CONTAINER (parent), socket);
    gtk_widget_realize (socket);
    g_print ("The ID of the sockets window is %ld\n", gtk_socket_get_id (socket));
  }
  gtk_widget_show_all (window);
  gtk_main ();
  return(0);
}
|#
