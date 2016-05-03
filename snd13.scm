(provide 'snd-snd13.scm)

(define (clm-print . args) 
  "(clm-print . args) applies format to args and prints the result"
  (snd-print (apply format #f args)))

#|
;;; this is now moved to C 
;;; -------- envelope-interp

(define* (envelope-interp x e (base 1.0))   ;e is list of x y breakpoint pairs, interpolate at x returning y
;;  "(envelope-interp x e (base 1.0)) -> value of e at x; base controls connecting segment type: (envelope-interp .3 '(0 0 .5 1 1 0) -> .6"
  (cond ((null? e) 0.0)		        ;no data -- return 0.0
	((or (<= x (car e))	        ;we're sitting on x val (or if < we blew it)
	     (null? (cddr e)))	        ;or we're at the end of the list
	 (cadr e))		        ;so return current y value
	((> (caddr e) x)		;x <= next env x axis value
	 (if (or (= (cadr e) (cadddr e))
		 (= base 0.0))
	     (cadr e)		        ;y1=y0, so just return y0 (avoid endless calculations below)
	     (if (= base 1.0)
		 (+ (cadr e)	        ;y0+(x-x0)*(y1-y0)/(x1-x0)
		    (* (- x (car e))
		       (/ (- (cadddr e) (cadr e))
			  (- (caddr e) (car e)))))
		 (+ (cadr e) ; this does not exactly match xramp-channel
		    (* (/ (- (cadddr e) (cadr e))
			  (- base 1.0))
		       (- (expt base (/ (- x (car e))
					(- (caddr e) (car e))))
			  1.0))))))
	(else (envelope-interp x (cddr e) base)))) ;go on looking for x segment
|#


#|

;;; ---------------- waterfall spectrum ----------------
;;; this is obsolete

(define waterfall
  (let* ((drawer #f)
	 (input-port #f)
	 (input-proc 0)
	 (gl-list #f)
	 (slices 256) ; number of traces displayed
	 (slice 0)
	 (data (make-vector slices))
	 (bins 512) ; fft size
	 (input-data #f)
	 (scaler 1.0)  ; data scaler before GL turns it into colors
	 (cutoff 0.2)) ; 0.5 is full spectrum
    
    (define (redraw-graph)
      (let ((win (XtWindow drawer))
	    (dpy (XtDisplay drawer))
	    (cx (snd-gl-context)))
	(glXMakeCurrent dpy win cx)
	(if gl-list (glDeleteLists gl-list 1))
	(set! gl-list (glGenLists 1))
	(glEnable GL_DEPTH_TEST)
	(glShadeModel GL_SMOOTH)
	(glClearDepth 1.0)
	(glClearColor 1.0 1.0 1.0 1.0) ; todo: bg color here
	(glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	;;gl_spectrogram(Xen data, Xen gl_list, Xen cutoff, Xen use_dB, Xen min_dB, Xen scale, Xen br, Xen bg, Xen bb)
	(glSpectrogram data gl-list cutoff #f -60.0 scaler 65535 65535 65535)
	(let ((vals (XtVaGetValues drawer (list XmNwidth 0 XmNheight 0))))
	  (glViewport 0 0 (list-ref vals 1) (list-ref vals 3)))
	(glMatrixMode GL_PROJECTION)
	(glLoadIdentity)
	(glRotatef (spectro-x-angle) 1.0 0.0 0.0)
	(glRotatef (spectro-y-angle) 0.0 1.0 0.0)
	(glRotatef (spectro-z-angle) 0.0 0.0 1.0)
	(glScalef (spectro-x-scale) (spectro-y-scale) (spectro-z-scale))
	(glCallList gl-list)
	;; todo: make axis
	(glXSwapBuffers dpy win)
	(glDrawBuffer GL_BACK)))
    
    (define (tick-audio id)
      ;; background process reads incoming audio data, creates spectrum, displays next trace
      (mus-audio-read input-port input-data (* bins 2))
      (let ((rl-data (copy (input-data 0) (data slice))))
	(snd-spectrum rl-data blackman2-window bins #t 0.0 #t #f)
	(redraw-graph))
      (set! slice (+ 1 slice))
      (if (>= slice slices)
	  (set! slice 0))
      #f)
    
    (define (stop-it)
      ;; turn off the waterfall display
      (if input-port
	  (begin
	    (mus-audio-close input-port)
	    (set! input-port #f)))
      (if (XtWorkProcId? input-proc)
	  (begin
	    (XtRemoveWorkProc input-proc)
	    (set! input-proc 0)))
      (do ((i 0 (+ 1 i)))
	  ((= i slices))
	(float-vector-scale! (data i) 0.0)))
    
    (define (start-it)
      (define (add-main-pane name type args)
	(XtCreateManagedWidget name type (list-ref (main-widgets) 3) args))
      (if (not drawer)
	  (let ((outer (add-main-pane "Waterfall" xmFormWidgetClass
				      (list XmNbackground (basic-color)
					    XmNpaneMinimum 320))))
	    (set! drawer (XtCreateManagedWidget "draw" xmDrawingAreaWidgetClass outer
						(list XmNbackground       (graph-color)
						      XmNforeground       (data-color)
						      XmNleftAttachment   XmATTACH_FORM
						      XmNtopAttachment    XmATTACH_FORM
						      XmNbottomAttachment XmATTACH_FORM
						      XmNrightAttachment  XmATTACH_FORM)))
	    (XtAddCallback drawer XmNresizeCallback (lambda (w context info) (redraw-graph)))
	    (XtAddCallback drawer XmNexposeCallback (lambda (w context info) (redraw-graph)))
	    (hook-push orientation-hook (lambda (hook) (redraw-graph)))
	    (hook-push color-hook (lambda (hook) (redraw-graph)))))
      ;; start the waterfall display
      (if (not (or input-port (XtWorkProcId? input-proc)))
	  (begin
	    (set! input-port (mus-audio-open-input mus-audio-default 22050 1 mus-lshort 512))
	    (set! input-proc (XtAppAddWorkProc (car (main-widgets)) tick-audio)))))
    
    ;; turn display with orientation dialog
    ;;  for example: x-angle 290, y angle: 60
    
    (lambda* (start scl pc-spectrum fft-size)
	     (if start
		 (begin
		   (set! cutoff pc-spectrum)
		   (set! scaler scl)
		   (set! bins fft-size)
		   (set! input-data (make-float-vector (list 1 (* bins 2)) 0.0))
		   (do ((i 0 (+ 1 i)))
		       ((= i slices))
		     (set! (data i) (make-float-vector bins)))
		   (start-it))
		 (stop-it)))))

  
(define* (start-waterfall (scl 1.0) (pc-spectrum 0.2) (fft-size 512))
  "(start-waterfall (scl 1.0) (pc-spectrum 0.2) (fft-size 512)) starts a 'waterfall' spectrum display of the incoming audio data"
  (waterfall #t scl pc-spectrum fft-size))

(define (stop-waterfall)
  "(stop-waterfall) stops a waterfall display"
  (waterfall #f))

|#



#|
;;; -------- "vector synthesis"
;;; also obsolete
;;; this idea (and the weird name) from linux-audio-development mailing list discussion
;;;   apparently some commercial synths (or software?) provide this

(define (vector-synthesis driver files read-even-when-not-playing)

  "(vector-synthesis driver files read-even-when-not-playing) uses 'driver', a 
function of two args (the number of files, and the number of samples between calls) to decide which file to play.  If 
'read-even-when-not-playing' is #t (default is #f), the input files are constantly 
read, even if not playing.  'files' is a list of files to be played."
  
  (let ((files-len (length files)))
    (if (> files-len 0)
	(let* ((bufsize 256)
	       (srate (srate (car files)))
	       (chans (apply max (map channels files)))
	       (data (make-float-vector (list chans bufsize) 0.0))
	       (readers (map make-file->frame files))
	       (locs (make-vector files-len 0))
	       (pframes (make-vector files-len 0))
	       (current-file 0)
	       (reading #t)
	       (out-port (mus-audio-open-output 0 srate chans mus-lshort (* bufsize 2))))
	  (if (< out-port 0)
	      (format #t "can't open audio port! ~A" out-port)
	      (begin
		(do ((i 0 (+ i 1)))
		    ((= i files-len))
		  (set! (pframes i) (framples (files i))))
		(catch #t
		       (lambda ()
			 (while reading
				(let ((next-file (driver files-len bufsize)))
				  (if (not (= next-file current-file))
				      (let ((ramp-down 1.0)
					    (ramp (/ 1.0 bufsize))
					    (current (readers current-file))
					    (current-loc (locs current-file))
					    (next (readers next-file))
					    (next-loc (locs next-file))
					    (up (make-frame chans))
					    (down (make-frame chans)))
					(do ((i 0 (+ i 1)))
					    ((= i bufsize))
					  (file->frame next (+ next-loc i) up)
					  (file->frame current (+ current-loc i) down)
					  (do ((j 0 (+ 1 j)))
					      ((= j chans))
					    (vector-set! data j i 
							     (+ (* ramp-down (frame-ref down j))
								(* (- 1.0 ramp-down) (frame-ref up j)))))
					  (set! ramp-down (- ramp-down ramp)))
					(if read-even-when-not-playing
					    (do ((i 0 (+ i 1)))
						((= i files-len))
					      (set! (locs i) (+ (locs i) bufsize)))
					    (begin
					      (set! (locs current-file) (+ (locs current-file) bufsize))
					      (set! (locs next-file) (+ (locs next-file) bufsize))))
					(set! current-file next-file))
				      (let ((current (readers current-file))
					    (current-loc (locs current-file))
					    (on (make-frame chans)))
					(do ((i 0 (+ i 1)))
					    ((= i bufsize))
					  (file->frame current (+ current-loc i) on)
					  (do ((k 0 (+ 1 k)))
					      ((= k chans))
					    (vector-set! data k i (frame-ref on k))))
					(if read-even-when-not-playing
					    (do ((i 0 (+ i 1)))
						((= i files-len))
					      (set! (locs i) (+ (locs i) bufsize)))
					    (set! (locs current-file) (+ (locs current-file) bufsize)))))
				  (mus-audio-write out-port data bufsize)
				  (set! reading (letrec ((any-data-left 
							  (lambda (f)
							    (if (= f files-len)
								#f
								(or (< (locs f) (pframes f))
								    (any-data-left (+ 1 f)))))))
						  (any-data-left 0))))))
		       (lambda args (begin (snd-print (format #f "error ~A" args)) (car args))))
		(mus-audio-close out-port)))))))
|#
#|
(vector-synthesis (let ((ctr 0) (file 0)) 
		    (lambda (files bufsize)
		      (if (> ctr 4)
			  (begin
			    (set! file (+ 1 file))
			    (set! ctr 0)
			    (if (>= file files)
				(set! file 0)))
			  (set! ctr (+ ctr 1)))
		      file))
		  (list "oboe.snd" "pistol.snd") #t)
|#


;;; --------------------------------------------------------------------------------
;;; old frame.scm
;;; various frame-related extensions

;;; frame-reverse! frame-copy (from mixer.scm)
;;; sound->frame frame->sound 
;;;   region->frame
;;;
;;; make-frame-reader frame-reader? frame-reader-at-end frame-reader-position frame-reader-home free-frame-reader copy-frame-reader frame-reader-chans
;;;   next-frame previous-frame read-frame
;;;   make-region-frame-reader make-selection-frame-reader
;;;   make-sync-frame-reader
;;;
;;; file->frample frample->file
;;; frame->float-vector float-vector->frame
;;;
;;; insert-frame insert-float-vector
;;; mix-frame
;;; scan-sound map-sound
;;;
;;; simultaneous-zero-crossing

(provide 'snd-frame.scm)
(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))


(define frame-reverse! reverse)
(define frame-copy copy)

#|
(define (frame-cross m1 m2)
  "(frame-cross fr1 fr2) returns the cross product (a frame) of frames fr1 and fr2"
  (if (or (not (= (channels m1) 3))
	  (not (= (channels m2) 3)))
      (snd-print "cross product only in 3 dimensions")
      (make-frame 3 
		  (- (* (m1 1) (m2 2)) 
		     (* (m1 2) (m2 1)))
		  (- (* (m1 2) (m2 0)) 
		     (* (m1 0) (m2 2)))
		  (- (* (m1 0) (m2 1)) 
		     (* (m1 1) (m2 0))))))

;;; (frame-cross (make-frame 3 0 0 1) (make-frame 3 0 -1 0))
;;; <frame[3]: [1.000 0.000 0.000]>

(define (frame-normalize f)
  "(frame-normalize fr) scales the contents of frame fr so that its euclidean length is 1.0"
  (let ((mag (sqrt (dot-product (mus-data f) (mus-data f)))))
    (if (> mag 0.0)
	(frame* f (/ 1.0 mag))
	f)))

;;; (frame-normalize (make-frame 3 4 3 0))
;;; <frame[3]: [0.800 0.600 0.000]>
|#

(define* (frame->float-vector fr v)
  "(frame->float-vector fr v) copies frame fr into either float-vector v or a new float-vector, returning the float-vector"
  (copy fr (or v (make-float-vector (length fr)))))

(define* (float-vector->frame v fr)
  "(float-vector->frame v fr) copies float-vector v into either frame fr or a new frame, returning the frame"
  (copy v (or fr (make-frame (length v)))))

(define frame->vct frame->float-vector)
(define vct->frame float-vector->frame)


(define* (sound->frame (pos 0) snd)
  "(sound->frame pos snd) returns a frame containing the contents of the sound snd at position pos"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound "sound->frame: ~A" snd)
	(let ((fr (make-frame (channels index))))
	  (do ((i 0 (+ i 1)))
	      ((= i (channels index))
	       fr)
	    (set! (fr i) (sample pos index i)))))))

(define* (frame->sound fr (pos 0) snd)
  "(frame->sound fr pos snd) places the contents of frame fr into sound snd at position pos"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound "frame->sound: ~A" snd)
	(do ((i 0 (+ i 1)))
	    ((= i (channels index))
	     fr)
	  (set! (sample pos index i) (fr i))))))

	
(define (region->frame reg pos)
  "(region->frame pos reg) returns a frame with the contents of region reg at position pos"
  (if (not (region? reg))
      (error 'no-such-region "region->frame: ~A" reg)
      (let ((fr (make-frame (channels reg))))
	(do ((i 0 (+ i 1)))
	    ((= i (channels reg))
	     fr)
	  (set! (fr i) (region-sample reg pos i))))))



;;; --------------------------------------------------------------------------------
;;; frame-readers
;;;

(defgenerator frame-sampler snd chns frm samplers)

(define* (make-frame-reader (beg 0) snd dir edpos)
  "(make-frame-reader beg snd dir edpos) returns a frame reader, basically a sampler that reads all channels on each call"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (and (not (sound? index))
	     (not (string? index))) ; filename is a possibility here
	(error 'no-such-sound "make-frame-reader: ~A" snd)
	(let ((chns (channels index))) ; this works in both cases
	  (make-frame-sampler index chns 
			      (make-frame chns)
			      (let ((v (make-vector chns)))
				(do ((i 0 (+ i 1)))
				    ((= i chns) v)
				  (set! (v i) (make-sampler beg index i dir edpos)))))))))

(define frame-reader? frame-sampler?)

(define (frame-reader-at-end? fr)
  "(frame-reader-at-end? fr) -> #t if the samplers in frame-reader fr have reached the end of their respective channels"
  (with-let fr
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i chns) #t)
	 (if (not (sampler-at-end? (samplers i)))
	     (return #f)))))))

(define (frame-reader-position fr)
  "(frame-reader-position fr) -> current read position of frame-reader fr"
  (with-let fr (sampler-position (samplers 0))))

(define (frame-reader-home fr)
  "(frame-reader-home fr) -> sound object associated with frame-reader fr"
  (with-let fr snd))

(define (frame-reader-chans fr)
  "(frame-reader-chans fr) -> number of channels read by frame-reader fr"
  (with-let fr chns))

(define (free-frame-reader fr)
  "(free-frame-reader fr) frees all samplers associated with frame-reader fr"
  (with-let fr
    (do ((i 0 (+ i 1)))
	((= i chns))
      (free-sampler (samplers i)))))

(define (copy-frame-reader fr)
  "(copy-frame-reader fr) returns a copy of frame-reader fr"
  (with-let fr
    (make-frame-sampler snd chns
			(make-frame chns)
			(let ((v (make-vector chns)))
			  (do ((i 0 (+ i 1)))
			      ((= i chns) v)
			    (set! (v i) (copy-sampler (samplers i))))))))

(define (next-frame fr)
;;  "(next-frame fr) returns the next frame as read by frame-reader fr"
  (with-let fr
    (do ((i 0 (+ i 1)))
	((= i chns) frm)
      (set! (frm i) (next-sample (samplers i))))))

(define (previous-frame fr)
  "(previous-frame fr) returns the previous frame as read by frame-reader fr"
  (with-let fr
    (do ((i 0 (+ i 1)))
	((= i chns) frm)
      (set! (frm i) (previous-sample (samplers i))))))

(define (read-frame fr)
;;  "(read-frame fr) returns the next frame read by frame-reader fr taking its current read direction into account"
  (with-let fr
    (do ((i 0 (+ i 1)))
	((= i chns) frm)
      (set! (frm i) (read-sample (samplers i))))))


(define* (make-region-frame-reader reg beg dir)
  "(make-region-frame-reader reg beg dir) returns a frame-reader reading the contents of region reg"
  (if (not (region? reg))
      (error 'no-such-region "make-region-frame-reader: ~A" reg)
      (let ((chns (channels reg)))
	(make-frame-sampler reg chns 
			    (make-frame chns)
			    (let ((v (make-vector chns)))
			      (do ((i 0 (+ i 1)))
				  ((= i chns) v)
				(set! (v i) (make-region-sampler reg beg i dir))))))))

(define* (make-sync-frame-reader (beg 0) snd dir edpos)
  "(make-sync-frame-reader beg snd dir edpos) returns a frame-reader that reads all channels sync'd to 'snd'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound "make-sync-frame-reader: ~A" snd)
	(let ((snc (sync index)))
	  (if (= snc 0)
	      (make-frame-reader beg index dir edpos)
	      (let ((chns 0))
		(for-each
		 (lambda (s)
		   (if (= (sync s) snc) ; sync field is always an int (0 = none)
		       (set! chns (+ chns (channels s)))))
		 (sounds))
		(make-frame-sampler index chns
				    (make-frame chns)
				    (let ((v (make-vector chns))
					  (ctr 0))
				      (for-each 
				       (lambda (s)
					 (if (= (sync s) snc)
					     (begin
					       (do ((i 0 (+ i 1)))
						   ((= i (channels s)))
						 (set! (v (+ i ctr)) (make-sampler beg s i dir edpos)))
					       (set! ctr (+ ctr (channels s))))))
				       (sounds))
				      v))))))))

(define* (make-selection-frame-reader (beg 0))
  "(make-selection-frame-reader (beg 0)) returns a frame reader that reads all channels of the current selection"
  (if (not (selection?))
      (error 'no-active-selection "make-selection-frame-reader: ~A" beg)
      (let ((chns (selection-chans)))
	(make-frame-sampler -1 chns
			    (make-frame chns)
			    (let ((ctr 0)
				  (v (make-vector chns)))
			      (for-each
			       (lambda (snd)
				 (do ((chn 0 (+ 1 chn)))
				     ((= chn (channels snd)))
				   (if (selection-member? snd chn)
				       (begin
					 (set! (v ctr) (make-sampler (+ beg (selection-position snd chn)) snd chn))
					 (set! ctr (+ ctr 1))))))
			       (sounds))
			      v)))))


(define (old-file->frample file)
  (samples 0 (framples file) file))

(define file->vct old-file->frample)


(define* (old-frample->file v file (srate 22050) (comment ""))
  "(frample->file v file srate comment) writes the data in float-vector v to the specified sound file"
  (if (float-vector? v)
      (let ((fd (mus-sound-open-output file srate 1 #f mus-riff comment)))
	(mus-sound-write fd 0 (- (length v) 1) 1 (make-shared-vector v (list 1 (length v))))
	(mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) (length v)))
	file)
      (error 'wrong-type-arg "file->frample: ~A" v)))

(define vct->file old-frample->file)


(define* (insert-float-vector v (beg 0) dur snd chn edpos)
  "(insert-float-vector v beg dur snd chn edpos) inserts float-vector v's data into sound snd at beg"
  (if (not (float-vector? v))
      (error 'wrong-type-arg "insert-float-vector: ~A" v)
      (let ((len (or dur (length v))))
	(insert-samples beg len v snd chn edpos #f (format #f "insert-float-vector ~A ~A ~A" (float-vector->string v) beg dur)))))

(define insert-vct insert-float-vector)


(define* (insert-frame fr (beg 0) snd edpos)
  "(insert-frame fr beg snd edpos) inserts frame fr's data into sound snd (one sample in each channel) at beg"
  (if (not (frame? fr))
      (error 'wrong-type-arg "insert-frame: ~A" fr)
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound "insert-frame: ~A" snd)
	    (let ((chns (min (channels fr) (channels index))))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn chns))
		(insert-sample beg (fr chn) index chn edpos)))))))


(define* (mix-frame fr (beg 0) snd)
  "(mix-frame fr beg snd) mixes frame fr's data into sound snd (one sample in each channel) at beg"
  (if (not (frame? fr))
      (error 'wrong-type-arg "mix-frame: ~A" fr)
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound "mix-frame: ~A" snd)
	    (let ((chns (min (channels fr) (channels index))))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn chns))
		(set! (sample beg index chn) (+ (fr chn) (sample beg index chn)))))))))

  
(define* (scan-sound func (beg 0) dur snd with-sync)
  "(scan-sound func beg dur snd with-sync) is like scan-channel; it passes func a frame on each call, and stops when func returns true"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((reader (if with-sync
			   (make-sync-frame-reader beg index)
			   (make-frame-reader beg index)))
	       (result #f)
	       (len (framples index))
	       (end (if dur (min len (+ beg dur)) len)))
	  (do ((i beg (+ i 1)))
	      ((or result (= i end))
	       (and result
		    (list result (- i 1))))
	    (set! result (func (read-frame reader)))))
	(error 'no-such-sound "scan-sound: ~A" snd))))

(define +read-forward+ 1)

(define* (map-sound func (beg 0) dur snd edpos)
  "(map-sound func beg dur snd edpos) is a version of map-channel that passes func a frame on each call, rather than a sample"
  ;; not sure map-sound with sync is a good idea -- even scale-by following sync seems bad
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (channels index))
	       (reader (make-frame-reader beg index +read-forward+ edpos))
	       (filename (snd-tempnam))
	       (writer (make-frame->file filename out-chans))
	       (len (framples index))
	       (end (if dur (min len (+ beg dur)) len))
	       (loc 0))
	  (do ((i beg (+ i 1))) 
	      ((= i end))
	    (let ((result (func (next-frame reader))))
	      (if result 
		  (begin
		    (frame->file writer loc result)
		    (set! loc (+ loc 1))))))
	  (mus-close writer)
	  (free-frame-reader reader)
	  (do ((i 0 (+ i 1)))
	      ((= i (channels index)))
	    (set! (samples beg loc index i #f "map-sound" i #f (= i 0)) filename))) ; edpos = #f, auto-delete = chan=0
	(error 'no-such-sound "map-sound: ~A" snd))))


(define* (simultaneous-zero-crossing (beg 0) dur snd)
  "(simultaneous-zero-crossing :option beg dur snd) looks through all channels of 'snd' for a simultaneous zero crossing."
  (let ((last-fr (make-frame (channels snd))))
    (scan-sound (lambda (fr)
		  (let ((result #t))
		    (do ((chn 0 (+ 1 chn)))
			((= chn (channels fr)))
		      (set! result (and result (< (* (fr chn) (last-fr chn)) 0.0)))
		      (set! (last-fr chn) (fr chn)))
		    result))
		beg dur snd)))




;;; --------------------------------------------------------------------------------
;;; old mixer.scm

;;; mixer and frame stuff, mostly oriented toward linear algebra (see also snd-test)
;;;
;;; make-zero-mixer, mixer-diagonal?, mixer-transpose, mixer-determinant,
;;; mixer-solve, mixer-inverse, invert-matrix, mixer-trace, mixer-poly, mixer-copy

(provide 'snd-mixer.scm)

(define make-zero-mixer make-mixer)

(define (mixer-copy umx)
  "(mixer-copy umx) returns a copy of its argument (a mixer)"
  (let* ((size (length umx))
	 (mx (make-mixer size)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (do ((j 0 (+ j 1)))
	  ((= j size))
	(set! (mx i j) (umx i j))))
    mx))


(define (mixer-diagonal? m)
  "(mixer-diagonal? m) returns #t if 'm' is a diagonal mixer"
  (let ((n (length m)))
    (or (= n 1)
	(call-with-exit
	 (lambda (return)
	   (do ((i 0 (+ i 1)))
	       ((= i n) #t)
	     (do ((j 0 (+ j 1)))
		 ((= j n))
	       (if (and (not (= i j))
			(not (= (m i j) 0.0)))
		   (return #f)))))))))
	   
(define (mixer-transpose mx)
  "(mixer-transpose mx) returns a new mixer of 'mx' transposed"
  (let* ((n (length mx))
	 (nmx (make-zero-mixer n)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (do ((j 0 (+ j 1)))
	  ((= j n))
	(set! (nmx j i) (mx i j))))
    nmx))

(define (sub-matrix mx row col)
  "(sub-matrix mx row col) returns a portion of the matrix 'mx'"
  (let* ((old-n (length mx))
	 (new-n (- old-n 1))
	 (nmx (make-zero-mixer new-n)))
    (do ((i 0 (+ i 1))
	 (ni 0))
	((= i old-n))
      (if (not (= i row))
	  (begin
	    (do ((j 0 (+ j 1))
		 (nj 0))
		((= j old-n))
	      (if (not (= j col))
		  (begin
		    (set! (nmx ni nj) (mx i j))
		    (set! nj (+ nj 1)))))
	    (set! ni (+ 1 ni)))))
    nmx))

(define (mixer-determinant mx)
  "(mixer-determinant mx) returns the determinant of 'mx'"
  (if (not (mixer? mx))
      (error 'wrong-type-arg "mixer-determinant argument should be a mixer")
      (let ((n (length mx)))
	(if (= n 1) 
	    (mx 0 0)
	    (if (= n 2)
		(- (* (mx 0 0) (mx 1 1))
		   (* (mx 0 1) (mx 1 0)))
		(if (= n 3)
		    (- (+ (* (mx 0 0) (mx 1 1) (mx 2 2))
			  (* (mx 0 1) (mx 1 2) (mx 2 0))
			  (* (mx 0 2) (mx 1 0) (mx 2 1)))
		       (+ (* (mx 0 0) (mx 1 2) (mx 2 1))
			  (* (mx 0 1) (mx 1 0) (mx 2 2))
			  (* (mx 0 2) (mx 1 1) (mx 2 0))))
		    (let ((sum 0.0)
			  (sign 1))
		      (do ((i 0 (+ i 1)))
			  ((= i n))
			(let ((mult (mx 0 i)))
			  (if (not (= mult 0.0))
			      (set! sum (+ sum (* sign mult (mixer-determinant (sub-matrix mx 0 i))))))
			  (set! sign (- sign))))
		      sum)))))))

(define* (mixer-poly mx :rest coeffs)
  "(mixer-poly mx :rest coeffs) returns a new mixer, the result of treating 'mx' as the argument to the polynomial defined by the 'coeffs' list"
  (let* ((n (length coeffs))
	 (nmx (make-scalar-mixer (length mx) (coeffs (- n 1))))
	 (x (mixer* mx 1.0)))
    (do ((i (- n 2) (- i 1)))
	((< i 0))
      (set! nmx (mixer+ nmx (mixer* x (coeffs i))))
      (set! x (mixer* mx x)))
    nmx))

;;; (define (float-vector-norm v1) (sqrt (dot-product v1 v1)))

(define (mixer-trace mx)
  "(mixer-trace mx) returns the trace of 'mx'"
  (let ((sum 0.0)
	(n (length mx)))
    (do ((i 0 (+ i 1)))
	((= i n) sum)
      (set! sum (+ sum (mx i i))))))


;;; invert-matrix is in dsp.scm
;;; it would be faster to use invert-matrix to calculate the determinant

(define (mixer-solve A b)
  "(mixer-solve A b) returns the solution of Ax=b"
  (let ((val (invert-matrix A b)))
    (and val (cadr val))))

(define (mixer-inverse A)
  "(mixer-inverse A) returns the inverse of 'A'"
  (let ((val (invert-matrix A)))
    (and val (car val))))

#|
(define (plane p1 p2 p3) ; each p a list of 3 coords, returns list (a b c d) of ax + by + cz = 1 (d = -1)
  (let ((m (make-mixer 3))
	(f (make-frame 3 1 1 1)))
    (do ((i 0 (+ i 1)))
	((= i 3))
      (set! (m 0 i) (p1 i))
      (set! (m 1 i) (p2 i))
      (set! (m 2 i) (p3 i)))
    (let ((b (mixer-solve m f)))
      (list (b 0) (b 1) (b 2) -1))))

;;; (plane '(0 0 1) '(1 0 0) '(0 1 0))
;;; (1.0 1.0 1.0 -1)
|#




;;; backwards compatibility for snd 12

(define (mus-audio-describe) "no description available anymore")

(define verbose-cursor with-verbose-cursor)

(define (recorder-dialog) "recorder-dialog is obsolete")



;;; backwards compatibility for snd 11 and earlier

;;; backwards compatibility for snd 11


;;; old Guile-style hook functions
(define (hook-empty? hook) 
  (null? (hook-functions hook)))

(define (reset-hook! hook) 
  (set! (hook-functions hook) ()))

;(define (run-hook . args) 
;  (hook-apply (car args) (cdr args)))

(define hook->list hook-functions)

(define* (add-hook! hook func at-end)
  (set! (hook-functions hook)
	(if (not at-end)
	    (cons func (hook-functions hook))
	    (append (hook-functions hook) (list func)))))

(define (remove-hook! hook func)
  (set! (hook-functions hook)
	(let loop ((l (hook-functions hook))
		   (result ()))
	  (cond ((null? l) (reverse! result))
		((eq? func (car l)) (loop (cdr l) result))
		(else (loop (cdr l) (cons (car l) result)))))))


;;; these are carried forward from snd10.scm

(define sine-summation nrxysin)
(define sine-summation? nrxysin?)

(define sum-of-sines nsin)
(define sum-of-sines? nsin?)

(define sum-of-cosines ncos)
(define sum-of-cosines? ncos?)

(define* (make-sum-of-sines (sines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-nsin :frequency frequency :n sines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sum-of-cosines (cosines 1) (frequency 0.0) (initial-phase 0.0))
  (let ((gen (make-ncos :frequency frequency :n cosines)))
    (set! (mus-phase gen) initial-phase)
    gen))

(define* (make-sine-summation (frequency 0.0) (initial-phase 0.0) (n 1) (a 0.5) (ratio 1.0))
  (let ((gen (make-nrxysin :frequency frequency :ratio ratio :n n :r a)))
    (set! (mus-phase gen) initial-phase)
    gen))

(if (not (defined? 'in-hz)) (define in-hz hz->radians))

(define copy-sample-reader copy-sampler)
(define free-sample-reader free-sampler)
(define make-mix-sample-reader make-mix-sampler)
(define make-region-sample-reader make-region-sampler)
(define make-sample-reader make-sampler)
(define mix-sample-reader? mix-sampler?)
(define region-sample-reader? region-sampler?)
(define sample-reader-at-end? sampler-at-end?)
(define sample-reader-home sampler-home)
(define sample-reader? sampler?)
(define sample-reader-position sampler-position)



;;; -------- with-mix --------
;;;
;;; weird syntax = with-mix (with-sound-args) file-name start-in-output &body body
;;;
;;; (with-sound () 
;;;   (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)
;;;                              (fm-violin 1 2 660 .1))
;;; ...)

(define with-mix-find-file-with-extensions
  (let ((documentation "(with-mix-find-file-with-extensions file extensions) helps the with-mix macro find checkpoint files"))
    (lambda (file extensions)
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
	     #f))))))

(define with-mix-file-extension 
  (let ((documentation "(with-mix-file-extension file default) is a helper function for the with-mix macro"))
    (lambda (file default)
      (let ((len (length file)))
	(call-with-exit
	 (lambda (ok)
	   (do ((i (- len 1) (- i 1)))
	       ((= i 0))
	     (if (char=? (file i) #\.)
		 (ok (substring file (+ 1 i) len))))
	   default))))))

(define-macro (with-mix options ur-chkpt-file ur-beg . body)
  `(let ((chkpt-file ,ur-chkpt-file)
	 (beg-1 ,ur-beg))
     (if (not (list? ',options))
	 (throw 'with-sound-interrupt (format #f "with-mix options list (arg 1) is ~A?~%;" ',options))
	 (if (not (string? chkpt-file))
	     (throw 'with-sound-interrupt (format #f "with-mix file (arg 2) is ~A?~%;" ,ur-chkpt-file))
	     (if (not (number? beg-1))
		 (throw 'with-sound-interrupt (format #f "with-mix begin time (arg 3) for ~S = ~A?~%;" chkpt-file beg-1))
		 (let ((beg (round (* *clm-srate* beg-1))))
		   (if (null? ',body)
		       (mus-file-mix *output* chkpt-file beg)
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
			       (mus-file-mix *output* sndf beg)
			       (if revf (mus-file-mix *reverb* revf beg)))
			     ;; else recompute
			     (let ((old-to-snd *to-snd*))
			       (set! *to-snd* #f)
			       (if *clm-verbose* (snd-print (format #f "remake ~S at ~,3F~%" chkpt-file beg)))
			       (let ((new-snd 
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
							 ())
						     ',options))))
				 (set! *to-snd* old-to-snd)
				 (mus-file-mix *output* new-snd beg)
				 (if revf (mus-file-mix *reverb* revf beg)))))))))))))


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
|#

;;; --------------------------------------------------------------------------------


(define (focus-follows-mouse) (set! *with-pointer-focus* #t))
(define (make-current-window-display) (set! *with-inset-graph* #t))
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


;;; --------------------------------------------------------------------------------

(define (add-watcher func)
  (hook-push effects-hook func))

(define (delete-watcher func)
  (hook-remove effects-hook func))


(define clear-selection unselect-all)

(define cursor-follows-play with-tracking-cursor)

