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
;;; file->float-vector float-vector->file
;;; frame->float-vector float-vector->frame
;;;
;;; insert-frame insert-float-vector
;;; mix-frame
;;; scan-sound map-sound
;;;
;;; simultaneous-zero-crossing

(provide 'snd-frame.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm")) ; for defgenerator
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))


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

(defgenerator (frame-sampler 
	       :methods (list
			 (cons 'object->string
			       (lambda* (g readable)
				 (string-append "#<frame-reader "
						(object->string (g 'snd)) " "
						(object->string (g 'frm)) " "
						(object->string (g 'samplers)) ">")))))
  snd chns frm samplers)

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
  (with-environment fr
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i chns) #t)
	 (if (not (sampler-at-end? (samplers i)))
	     (return #f)))))))

(define (frame-reader-position fr)
  "(frame-reader-position fr) -> current read position of frame-reader fr"
  (with-environment fr (sampler-position (samplers 0))))

(define (frame-reader-home fr)
  "(frame-reader-home fr) -> sound object associated with frame-reader fr"
  (with-environment fr snd))

(define (frame-reader-chans fr)
  "(frame-reader-chans fr) -> number of channels read by frame-reader fr"
  (with-environment fr chns))

(define (free-frame-reader fr)
  "(free-frame-reader fr) frees all samplers associated with frame-reader fr"
  (with-environment fr
    (do ((i 0 (+ i 1)))
	((= i chns))
      (free-sampler (samplers i)))))

(define (copy-frame-reader fr)
  "(copy-frame-reader fr) returns a copy of frame-reader fr"
  (with-environment fr
    (make-frame-sampler snd chns
			(make-frame chns)
			(let ((v (make-vector chns)))
			  (do ((i 0 (+ i 1)))
			      ((= i chns) v)
			    (set! (v i) (copy-sampler (samplers i))))))))

(define (next-frame fr)
;;  "(next-frame fr) returns the next frame as read by frame-reader fr"
  (with-environment fr
    (do ((i 0 (+ i 1)))
	((= i chns) frm)
      (set! (frm i) (next-sample (samplers i))))))

(define (previous-frame fr)
  "(previous-frame fr) returns the previous frame as read by frame-reader fr"
  (with-environment fr
    (do ((i 0 (+ i 1)))
	((= i chns) frm)
      (set! (frm i) (previous-sample (samplers i))))))

(define (read-frame fr)
;;  "(read-frame fr) returns the next frame read by frame-reader fr taking its current read direction into account"
  (with-environment fr
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


(define (file->float-vector file)
  "(file->float-vector file) returns a float-vector with file's data (channel 0)"
  (let* ((len (frames file))
	 (reader (make-sampler 0 file))
	 (data (make-float-vector len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (set! (data i) (next-sample reader)))
    (free-sampler reader)
    data))

(define file->vct file->float-vector)


(define* (float-vector->file v file (srate 22050) (comment ""))
  "(float-vector->file v file srate comment) writes the data in float-vector v to the specified sound file"
  (if (float-vector? v)
      (let ((fd (mus-sound-open-output file srate 1 #f mus-riff comment)))
	(mus-sound-write fd 0 (- (length v) 1) 1 (make-shared-vector v (list 1 (length v))))
	(mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) (length v)))
	file)
      (error 'wrong-type-arg "file->float-vector: ~A" v)))

(define vct->file float-vector->file)


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
	       (len (frames index))
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
	       (len (frames index))
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

