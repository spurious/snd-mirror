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
;;; frame->sound-data, sound-data->frame
;;; sound->sound-data sound-data->sound
;;;   region->sound-data selection->sound-data
;;; file->vct vct->file
;;; frame->vct vct->frame
;;; file->sound-data sound-data->file
;;;
;;; insert-sound-data insert-frame insert-vct
;;; mix-sound-data mix-frame
;;; scan-sound map-sound
;;;
;;; simultaneous-zero-crossing

(provide 'snd-frame.scm)
(if (not (provided? 'snd-ws.scm)) (load "ws.scm")) ; for defgenerator


(define (frame-reverse! fr)
  "(frame-reverse fr) reverses the contents of frame fr"
  (if (not (frame? fr))
      (error 'wrong-type-arg "frame-reverse: ~A" fr)
      (let ((len (channels fr)))
	(do ((i 0 (+ i 1))
	     (j (- len 1) (- j 1)))
	    ((>= i (/ len 2)))
	  (let ((temp (fr i)))
	    (set! (fr i) (fr j))
	    (set! (fr j) temp)))
	fr)))

(define (frame-copy fr)
  "(frame-copy fr) returns a copy of frame fr"
  (if (not (frame? fr))
      (error 'wrong-type-arg "frame-copy: ~A" fr)
      (copy fr)))
#|
      (let* ((len (channels fr))
	     (nfr (make-frame len)))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (set! (nfr i) (fr i)))
	nfr)))
|#


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

(define* (frame->vct fr v)
  "(frame->vct fr v) copies frame fr into either vct v or a new vct, returning the vct"
  (if (not (frame? fr))
      (error 'wrong-type-arg "frame->vct: ~A" fr)
      (let* ((flen (channels fr))
	     (nv (or (and (vct? v) v)
		     (make-vct flen)))
	     (len (min flen (length nv))))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (set! (nv i) (fr i)))
	nv)))

(define* (vct->frame v fr)
  "(vct->frame v fr) copies vct v into either frame fr or a new frame, returning the frame"
  (if (not (vct? v))
      (error 'wrong-type-arg "vct->frame: ~A" v)
      (let* ((vlen (length v))
	     (nfr (or (and (frame? fr) fr)
		      (make-frame vlen)))
	     (len (min vlen (channels nfr))))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (set! (nfr i) (v i)))
	nfr)))


(define* (frame->sound-data fr sd (pos 0))
;;  "(frame->sound-data fr sd pos) copies the contents of frame fr into the sound-data sd at position pos"
  (if (not (frame? fr))
      (error 'wrong-type-arg "frame->sound-data: ~A" fr)
      (if (not (sound-data? sd))
	  (error 'wrong-type-arg "frame->sound-data: ~A" sd)
	  (if (>= pos (length sd))
	      (error 'out-of-range "frame->sound-data: ~A" pos)
	      (let ((len (min (channels fr) 
			      (channels sd))))
		(do ((i 0 (+ i 1)))
		    ((= i len))
		  (sound-data-set! sd i pos (frame-ref fr i)))
		sd)))))

(define (sound-data->frame sd pos fr)
  "(sound-data->frame sd pos fr) copies sound-data sd's contents at position pos into frame fr"
  (if (not (frame? fr))
      (error 'wrong-type-arg "sound-data->frame: ~A" fr)
      (if (not (sound-data? sd))
	  (error 'wrong-type-arg "sound-data->frame: ~A" sd)
	  (if (>= pos (length sd))
	      (error 'out-of-range "sound-data->frame: ~A" pos)
	      (let ((len (min (channels fr) 
			      (channels sd))))
		(do ((i 0 (+ i 1)))
		    ((= i len))
		  (set! (fr i) (sound-data-ref sd i pos)))
		fr)))))


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


(define* (sound->sound-data (beg 0) dur snd)
  "(sound->sound-data beg dur snd) returns a sound-data object containing the samples in sound snd starting at position beg for dur frames"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound "sound->sound-data: ~A" snd)
	(let* ((chns (channels index))
	       (len (or dur (max 1 (- (frames index) beg))))
	       (sd (make-sound-data chns len)))
	  (do ((i 0 (+ i 1)))
	      ((= i chns) 
	       sd)
	    (vct->sound-data (channel->vct beg len index i) sd i))))))

(define* (sound-data->sound sd (beg 0) dur snd)
  "(sound-data->sound sd beg dur snd) places the contents of sound-data sd into sound snd starting at position beg for dur frames"
  (if (not (sound-data? sd))
      (error 'wrong-type-arg "sound-data->sound: ~A" sd)
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound "sound->sound-data: ~A" snd)
	    (let ((ndur (or dur (length sd))))
	      (do ((i 0 (+ i 1)))
		  ((= i (channels index)) 
		   sd)
		(vct->channel (sound-data->vct sd i) beg ndur index i)))))))



;;; --------------------------------------------------------------------------------
;;; frame-readers
;;;

(defgenerator (frame-sampler 
	       :methods (list
			 (cons 'object->string
			       (lambda (g)
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


(define (file->vct file)
  "(file->vct file) returns a vct with file's data (channel 0)"
  (let* ((len (frames file))
	 (reader (make-sampler 0 file))
	 (data (make-vct len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (set! (data i) (next-sample reader)))
    (free-sampler reader)
    data))

(define* (vct->file v file (srate 22050) (comment ""))
  "(vct->file v file srate comment) writes the data in vct v to the specified sound file"
  (if (vct? v)
      (let ((fd (mus-sound-open-output file srate 1 #f mus-riff comment)))
	(mus-sound-write fd 0 (- (length v) 1) 1 (vct->sound-data v))
	(mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) (length v)))
	file)
      (error 'wrong-type-arg "file->vct: ~A" v)))


(define (file->sound-data file)
  "(file->sound-data file) returns a sound-data object with the contents of file"
  (let* ((len (frames file))
	 (chns (channels file))
	 (reader (make-frame-reader 0 file))
	 (data (make-sound-data chns len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (frame->sound-data (read-frame reader) data i))
    (free-frame-reader reader)
    data))

(define* (sound-data->file sd file (srate 22050) (comment ""))
  "(sound-data->file sd file srate comment) writes the contents of sound-data sd to file"
  (if (sound-data? sd)
      (let ((fd (mus-sound-open-output file srate (channels sd) #f mus-riff comment)))
	(mus-sound-write fd 0 (- (length sd) 1) (channels sd) sd)
	(mus-sound-close-output fd (* (mus-bytes-per-sample mus-out-format) (length sd) (channels sd)))
	file)
      (error 'wrong-type-arg "sound-data->file: ~A" sd)))


(define (region->sound-data reg)
  "(region->sound-data reg) returns a sound-data object with the contents of region reg"
  (if (region? reg)
      (let* ((reader (make-region-frame-reader reg 0))
	     (len (region-frames reg))
	     (chns (channels reg))
	     (data (make-sound-data chns len)))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (frame->sound-data (read-frame reader) data i))
	(free-frame-reader reader)
	data)
      (error 'no-such-region "region->sound-data: ~A" reg)))

(define* (selection->sound-data (beg 0))
  "(selection->sound-data beg) returns a sound-data object with the contents of current selection"
  (if (selection?)
      (let* ((reader (make-selection-frame-reader beg))
	     (len (- (selection-frames) beg))
	     (chns (selection-chans))
	     (sd (make-sound-data chns len)))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (frame->sound-data (read-frame reader) sd i))
	(free-frame-reader reader)
	sd)
      (error 'no-active-selection "selection->sound-data: ~A")))




(define* (insert-vct v (beg 0) dur snd chn edpos)
  "(insert-vct v beg dur snd chn edpos) inserts vct v's data into sound snd at beg"
  (if (not (vct? v))
      (error 'wrong-type-arg "insert-vct: ~A" v)
      (let ((len (or dur (length v))))
	(insert-samples beg len v snd chn edpos #f (format #f "insert-vct ~A ~A ~A" (vct->string v) beg dur)))))

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

(define* (insert-sound-data sd (beg 0) dur snd edpos)
  "(insert-sound-data sd beg dur snd edpos) inserts sound-data sd's contents into sound snd at beg"
  ;; this should be built-into insert-samples
  (if (not (sound-data? sd))
      (error 'wrong-type-arg "insert-sound-data: ~A" sd)
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound "insert-sound-data: ~A" snd)
	    (let* ((chns (min (channels sd) (channels index)))
		   (len (or dur (length sd)))
		   (v (make-vct len)))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn chns))
		(insert-samples beg len (sound-data->vct sd chn v) index chn edpos #f "insert-sound-data")))))))


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

(define* (mix-sound-data sd (beg 0) dur snd tagged)
  "(mix-sound-data sd beg dur snd tagged) mixes the contents of sound-data sd into sound snd at beg"
  (if (not (sound-data? sd))
      (error 'wrong-type-arg "mix-sound-data: ~A" sd)
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound "mix-sound-data: ~A" snd)
	    (let* ((chns (min (channels sd) (channels index)))
		   (len (or dur (length sd)))
		   (v (make-vct len))
		   (mix-id #f))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn chns))
		(let ((id (mix-vct (sound-data->vct sd chn v) beg index chn tagged "mix-sound-data")))
		  (if (not mix-id) (set! mix-id id))))
	      mix-id)))))

  
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

