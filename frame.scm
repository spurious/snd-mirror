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
      (error 'wrong-type-arg (list "frame-reverse" fr))
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
      (error 'wrong-type-arg (list "frame-copy" fr))
      (let* ((len (channels fr))
	     (nfr (make-frame len)))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (set! (nfr i) (fr i)))
	nfr)))

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
      (error 'wrong-type-arg (list "frame->vct" fr))
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
      (error 'wrong-type-arg (list "vct->frame" v))
      (let* ((vlen (length v))
	     (nfr (or (and (frame? fr) fr)
		      (make-frame vlen)))
	     (len (min vlen (channels nfr))))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (set! (nfr i) (v i)))
	nfr)))


(define* (frame->sound-data fr sd (pos 0))
  "(frame->sound-data fr sd pos) copies the contents of frame fr into the sound-data sd at position pos"
  (if (not (frame? fr))
      (error 'wrong-type-arg (list "frame->sound-data" fr))
      (if (not (sound-data? sd))
	  (error 'wrong-type-arg (list "frame->sound-data" sd))
	  (if (>= pos (length sd))
	      (error 'out-of-range (list "frame->sound-data" pos))
	      (let ((len (min (channels fr) 
			      (channels sd))))
		(do ((i 0 (+ i 1)))
		    ((= i len))
		  (sound-data-set! sd i pos (frame-ref fr i)))
		sd)))))

(define (sound-data->frame sd pos fr)
  "(sound-data->frame sd pos fr) copies sound-data sd's contents at position pos into frame fr"
  (if (not (frame? fr))
      (error 'wrong-type-arg (list "sound-data->frame" fr))
      (if (not (sound-data? sd))
	  (error 'wrong-type-arg (list "sound-data->frame" sd))
	  (if (>= pos (length sd))
	      (error 'out-of-range (list "sound-data->frame" pos))
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
	(error 'no-such-sound (list "sound->frame" snd))
	(let ((fr (make-frame (channels index))))
	  (do ((i 0 (+ i 1)))
	      ((= i (channels index))
	       fr)
	    (set! (fr i) (sample pos index i)))))))

(define* (frame->sound fr (pos 0) snd)
  "(frame->sound fr pos snd) places the contents of frame fr into sound snd at position pos"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound (list "frame->sound" snd))
	(do ((i 0 (+ i 1)))
	    ((= i (channels index))
	     fr)
	  (set! (sample pos index i) (fr i))))))

	
(define (region->frame reg pos)
  "(region->frame pos reg) returns a frame with the contents of region reg at position pos"
  (if (not (region? reg))
      (error 'no-such-region (list "region->frame" reg))
      (let ((fr (make-frame (channels reg))))
	(do ((i 0 (+ i 1)))
	    ((= i (channels reg))
	     fr)
	  (set! (fr i) (region-sample reg pos i))))))


(define* (sound->sound-data (beg 0) dur snd)
  "(sound->sound-data beg dur snd) returns a sound-data object containing the samples in sound snd starting at position beg for dur frames"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound (list "sound->sound-data" snd))
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
      (error 'wrong-type-arg (list "sound-data->sound" sd))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound (list "sound->sound-data" snd))
	    (let ((ndur (or dur (length sd))))
	      (do ((i 0 (+ i 1)))
		  ((= i (channels index)) 
		   sd)
		(vct->channel (sound-data->vct sd i) beg ndur index i)))))))



;;; TODO: frame-reader should be an environment, not a vector

(define +frame-reader-tag+ 0)
(define +frame-reader-snd+ 1)
(define +frame-reader-channels+ 2)
(define +frame-reader-frame+ 3)
(define +frame-reader0+ 4)

(define* (make-frame-reader (beg 0) snd dir edpos)
  "(make-frame-reader beg snd dir edpos) returns a frame reader, basically a sampler that reads all channels on each call"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (and (not (sound? index))
	     (not (string? index))) ; filename is a possibility here
	(error 'no-such-sound (list "make-frame-reader" snd))
	(let* ((chns (channels index)) ; this works in both cases
	       (fr (make-vector (+ chns +frame-reader0+))))
	  (set! (fr +frame-reader-tag+) 'frame-reader)
	  (set! (fr +frame-reader-snd+) index)
	  (set! (fr +frame-reader-channels+) chns)
	  (set! (fr +frame-reader-frame+) (make-frame chns))
	  (do ((i 0 (+ i 1)))
	      ((= i chns))
	    (set! (fr (+ i +frame-reader0+)) (make-sampler beg index i dir edpos)))
	  fr))))

(define (frame-reader? obj)
  "(frame-reader? obj) -> #t if obj is a frame-reader"
  (and (vector? obj)
       (eq? (obj +frame-reader-tag+) 'frame-reader)))

(define (frame-reader-at-end? fr)
  "(frame-reader-at-end? fr) -> #t if the samplers in frame-reader fr have reached the end of their respective channels"
  (if (frame-reader? fr)
      (let ((at-end #t))
	(do ((i 0 (+ i 1)))
	    ((or (not at-end) 
		 (= i (fr +frame-reader-channels+)))
	     at-end)
	  (set! at-end (sampler-at-end? (fr (+ i +frame-reader0+))))))
      (error 'wrong-type-arg (list "frame-reader-at-end" fr))))

(define (frame-reader-position fr)
  "(frame-reader-position fr) -> current read position of frame-reader fr"
  (if (frame-reader? fr)
      (sampler-position (fr +frame-reader0+))
      (error 'wrong-type-arg (list "frame-reader-position" fr))))

(define (frame-reader-home fr)
  "(frame-reader-home fr) -> sound object associated with frame-reader fr"
  (if (frame-reader? fr)
      (fr +frame-reader-snd+)
      (error 'wrong-type-arg (list "frame-reader-home" fr))))

(define (frame-reader-chans fr)
  "(frame-reader-chans fr) -> number of channels read by frame-reader fr"
  (if (frame-reader? fr)
      (fr +frame-reader-channels+)
      (error 'wrong-type-arg (list "frame-reader-chans" fr))))

(define (free-frame-reader fr)
  "(free-frame-reader fr) frees all readers associated with frame-reader fr"
  (if (frame-reader? fr)
      (do ((i 0 (+ i 1)))
	  ((= i (fr +frame-reader-channels+)))
	(free-sampler (fr (+ i +frame-reader0+))))
      (error 'wrong-type-arg (list "free-frame-reader" fr))))

(define (copy-frame-reader fr)
  "(copy-frame-reader fr) returns a copy of frame-reader fr"
  (if (frame-reader? fr)
      (let* ((chns (fr +frame-reader-channels+))
	     (nfr (make-vector (+ chns +frame-reader0+))))
	(set! (nfr +frame-reader-tag+) 'frame-reader)
	(set! (nfr +frame-reader-snd+) (fr 1))
	(set! (nfr +frame-reader-channels+) chns)
	(do ((i 0 (+ i 1)))
	    ((= i chns))
	  (set! (nfr (+ i +frame-reader0+)) (copy-sampler (fr (+ i +frame-reader0+)))))
	nfr)
      (error 'wrong-type-arg (list "copy-frame-reader" fr))))

(define (next-frame fr)
  "(next-frame fr) returns the next frame as read by frame-reader fr"
  (let ((vals (fr +frame-reader-frame+)))
    (do ((i 0 (+ i 1)))
	((= i (fr +frame-reader-channels+)))
      (set! (vals i) (next-sample (fr (+ i +frame-reader0+)))))
    vals))

(define (previous-frame fr)
  "(previous-frame fr) returns the previous frame as read by frame-reader fr"
  (let ((vals (fr +frame-reader-frame+)))
    (do ((i 0 (+ i 1)))
	((= i (fr +frame-reader-channels+)))
      (set! (vals i) (previous-sample (fr (+ i +frame-reader0+)))))
    vals))

(define (read-frame fr)
  "(read-frame fr) returns the next frame read by frame-reader fr taking its current read direction into account"
  (let ((vals (fr +frame-reader-frame+)))
    (do ((i 0 (+ i 1)))
	((= i (fr +frame-reader-channels+)))
      (set! (vals i) (read-sample (fr (+ i +frame-reader0+)))))
    vals))


(define* (make-region-frame-reader reg beg dir)
  "(make-region-frame-reader reg beg dir) returns a frame-reader reading the contents of region reg"
  (if (not (region? reg))
      (error 'no-such-region (list "make-region-frame-reader" reg))
      (let* ((chns (channels reg))
	     (fr (make-vector (+ chns +frame-reader0+))))
	(set! (fr +frame-reader-tag+) 'frame-reader)
	(set! (fr +frame-reader-snd+) reg)
	(set! (fr +frame-reader-channels+) chns)
	(set! (fr +frame-reader-frame+) (make-frame chns))
	(do ((i 0 (+ i 1)))
	    ((= i chns))
	  (set! (fr (+ i +frame-reader0+)) (make-region-sampler reg beg i dir)))
	fr)))

(define* (make-sync-frame-reader (beg 0) snd dir edpos)
  "(make-sync-frame-reader beg snd dir edpos) returns a frame reader that reads all channels sync'd to 'snd'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound (list "make-sync-frame-reader" snd))
	(let ((snc (sync index)))
	  (if (= snc 0)
	      (make-frame-reader beg snd dir edpos)
	      (let ((chns 0))
		(for-each
		 (lambda (s)
		   (if (= (sync s) snc) ; sync field is always an int (0 = none)
		       (set! chns (+ chns (channels s)))))
		 (sounds))
		(let ((fr (make-vector (+ chns +frame-reader0+))))
		  (set! (fr +frame-reader-tag+) 'frame-reader)
		  (set! (fr +frame-reader-snd+) index)
		  (set! (fr +frame-reader-channels+) chns)
		  (set! (fr +frame-reader-frame+) (make-frame chns))
		  (let ((ctr 0))
		    (for-each 
		     (lambda (s)
		       (if (= (sync s) snc)
			   (begin
			     (do ((i 0 (+ i 1)))
				 ((= i (channels s)))
			       (set! (fr (+ i ctr +frame-reader0+)) (make-sampler beg s i dir edpos)))
			     (set! ctr (+ ctr (channels s))))))
		     (sounds)))
		  fr)))))))

(define* (make-selection-frame-reader (beg 0))
  "(make-selection-frame-reader (beg 0)) returns a frame reader that reads all channels of the current selection"
  (if (not (selection?))
      (error 'no-active-selection (list "make-selection-frame-reader" beg))
      (let* ((chns (selection-chans))
	     (fr (make-vector (+ chns +frame-reader0+))))
	(set! (fr +frame-reader-tag+) 'frame-reader)
	(set! (fr +frame-reader-snd+) -1)
	(set! (fr +frame-reader-channels+) chns)
	(set! (fr +frame-reader-frame+) (make-frame chns))
	(let ((ctr 0))
	  (for-each
	   (lambda (snd)
	     (do ((chn 0 (+ 1 chn)))
		 ((= chn (channels snd)))
	       (if (selection-member? snd chn)
		   (begin
		     (set! (fr (+ ctr +frame-reader0+)) (make-sampler (+ beg (selection-position snd chn)) snd chn))
		     (set! ctr (+ 1 ctr))))))
	   (sounds)))
	fr)))


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
      (error 'wrong-type-arg (list "file->vct" v))))


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
      (error 'wrong-type-arg (list "sound-data->file" sd))))


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
      (error 'no-such-region (list "region->sound-data" reg))))

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
      (error 'no-active-selection (list "selection->sound-data"))))


(define* (insert-vct v (beg 0) dur snd chn edpos)
  "(insert-vct v beg dur snd chn edpos) inserts vct v's data into sound snd at beg"
  (if (not (vct? v))
      (error 'wrong-type-arg (list "insert-vct" v))
      (let ((len (or dur (length v))))
	(insert-samples beg len v snd chn edpos #f (format #f "insert-vct ~A ~A ~A" (vct->string v) beg dur)))))

(define* (insert-frame fr (beg 0) snd edpos)
  "(insert-frame fr beg snd edpos) inserts frame fr's data into sound snd (one sample in each channel) at beg"
  (if (not (frame? fr))
      (error 'wrong-type-arg (list "insert-frame" fr))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound (list "insert-frame" snd))
	    (let ((chns (min (channels fr) (channels index))))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn chns))
		(insert-sample beg (fr chn) index chn edpos)))))))

(define* (insert-sound-data sd (beg 0) dur snd edpos)
  "(insert-sound-data sd beg dur snd edpos) inserts sound-data sd's contents into sound snd at beg"
  ;; this should be built-into insert-samples
  (if (not (sound-data? sd))
      (error 'wrong-type-arg (list "insert-sound-data" sd))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound (list "insert-sound-data" snd))
	    (let* ((chns (min (channels sd) (channels index)))
		   (len (or dur (length sd)))
		   (v (make-vct len)))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn chns))
		(insert-samples beg len (sound-data->vct sd chn v) index chn edpos #f "insert-sound-data")))))))


(define* (mix-frame fr (beg 0) snd)
  "(mix-frame fr beg snd) mixes frame fr's data into sound snd (one sample in each channel) at beg"
  (if (not (frame? fr))
      (error 'wrong-type-arg (list "mix-frame" fr))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound (list "mix-frame" snd))
	    (let ((chns (min (channels fr) (channels index))))
	      (do ((chn 0 (+ 1 chn)))
		  ((= chn chns))
		(set! (sample beg index chn) (+ (fr chn) (sample beg index chn)))))))))

(define* (mix-sound-data sd (beg 0) dur snd tagged)
  "(mix-sound-data sd beg dur snd tagged) mixes the contents of sound-data sd into sound snd at beg"
  (if (not (sound-data? sd))
      (error 'wrong-type-arg (list "mix-sound-data" sd))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound (list "mix-sound-data" snd))
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
	(error 'no-such-sound (list "scan-sound" snd)))))

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
		    (set! loc (+ 1 loc))))))
	  (mus-close writer)
	  (free-frame-reader reader)
	  (do ((i 0 (+ i 1)))
	      ((= i (channels index)))
	    (set! (samples beg loc index i #f "map-sound" i #f (= i 0)) filename))) ; edpos = #f, auto-delete = chan=0
	(error 'no-such-sound (list "map-sound" snd)))))


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

