;;; various frame-related extensions

;;; frame-reverse! frame-copy (from mixer.scm)
;;; sound->frame frame->sound 
;;;   region->frame
;;;
;;; make-frame-reader frame-reader? frame-reader-at-end frame-reader-position frame-reader-home free-frame-reader copy-frame-reader frame-reader-chans
;;;   next-frame previous-frame read-frame
;;;   make-region-frame-reader make-selection-frame-reader make-track-frame-reader read-track-frame make-sync-frame-reader
;;;
;;; frame->sound-data, sound-data->frame
;;; sound->sound-data sound-data->sound
;;;   region->sound-data track->sound-data selection->sound-data
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


(define (frame-reverse! fr)
  "(frame-reverse fr) reverses the contents of frame fr"
  (if (not (frame? fr))
      (throw 'wrong-type-arg (list "frame-reverse" fr))
      (let ((len (mus-length fr)))
	(do ((i 0 (1+ i))
	     (j (1- len) (1- j)))
	    ((>= i (/ len 2)))
	  (let ((temp (frame-ref fr i)))
	    (frame-set! fr i (frame-ref fr j))
	    (frame-set! fr j temp)))
	fr)))

(define (frame-copy fr)
  "(frame-copy fr) returns a copy of frame fr"
  (if (not (frame? fr))
      (throw 'wrong-type-arg (list "frame-copy" fr))
      (let* ((len (mus-length fr))
	     (nfr (make-frame len)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (frame-set! nfr i (frame-ref fr i)))
	nfr)))

#|
(define (frame-cross m1 m2)
  "(frame-cross fr1 fr2) returns the cross product (a frame) of frames fr1 and fr2"
  (if (or (not (= (mus-length m1) 3))
	  (not (= (mus-length m2) 3)))
      (snd-print "cross product only in 3 dimensions")
      (make-frame 3 
		  (- (* (frame-ref m1 1) (frame-ref m2 2)) 
		     (* (frame-ref m1 2) (frame-ref m2 1)))
		  (- (* (frame-ref m1 2) (frame-ref m2 0)) 
		     (* (frame-ref m1 0) (frame-ref m2 2)))
		  (- (* (frame-ref m1 0) (frame-ref m2 1)) 
		     (* (frame-ref m1 1) (frame-ref m2 0))))))

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

(define* (frame->vct fr :optional v)
  "(frame->vct fr :optional v) copies frame fr into either vct v or a new vct, returning the vct"
  (if (not (frame? fr))
      (throw 'wrong-type-arg (list "frame->vct" fr))
      (let* ((flen (mus-length fr))
	     (nv (or (and (vct? v) v)
		     (make-vct flen)))
	     (len (min flen (vct-length nv))))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (vct-set! nv i (frame-ref fr i)))
	nv)))

(define* (vct->frame v :optional fr)
  "(vct->frame v :optional fr) copies vct v into either frame fr or a new frame, returning the frame"
  (if (not (vct? v))
      (throw 'wrong-type-arg (list "vct->frame" v))
      (let* ((vlen (vct-length v))
	     (nfr (or (and (frame? fr) fr)
		      (make-frame vlen)))
	     (len (min vlen (mus-length nfr))))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (frame-set! nfr i (vct-ref v i)))
	nfr)))


(define* (frame->sound-data fr sd :optional (pos 0))
  "(frame->sound-data fr sd pos) copies the contents of frame fr into the sound-data sd at position pos"
  (if (not (frame? fr))
      (throw 'wrong-type-arg (list "frame->sound-data" fr))
      (if (not (sound-data? sd))
	  (throw 'wrong-type-arg (list "frame->sound-data" sd))
	  (if (>= pos (sound-data-length sd))
	      (throw 'out-of-range (list "frame->sound-data" pos))
	      (let ((len (min (mus-length fr) 
			      (sound-data-chans sd))))
		(do ((i 0 (1+ i)))
		    ((= i len))
		  (sound-data-set! sd i pos (frame-ref fr i)))
		sd)))))

(define (sound-data->frame sd pos fr)
  "(sound-data->frame sd pos fr) copies sound-data sd's contents at position pos into frame fr"
  (if (not (frame? fr))
      (throw 'wrong-type-arg (list "sound-data->frame" fr))
      (if (not (sound-data? sd))
	  (throw 'wrong-type-arg (list "sound-data->frame" sd))
	  (if (>= pos (sound-data-length sd))
	      (throw 'out-of-range (list "sound-data->frame" pos))
	      (let ((len (min (mus-length fr) 
			      (sound-data-chans sd))))
		(do ((i 0 (1+ i)))
		    ((= i len))
		  (frame-set! fr i (sound-data-ref sd i pos)))
		fr)))))


(define* (sound->frame :optional (pos 0) snd)
  "(sound->frame pos :optional snd) returns a frame containing the contents of the sound snd at position pos"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "sound->frame" snd))
	(let ((fr (make-frame (chans index))))
	  (do ((i 0 (1+ i)))
	      ((= i (chans index))
	       fr)
	    (frame-set! fr i (sample pos index i)))))))

(define* (frame->sound fr :optional (pos 0) snd)
  "(frame->sound fr pos :optional snd) places the contents of frame fr into sound snd at position pos"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "frame->sound" snd))
	(do ((i 0 (1+ i)))
	    ((= i (chans index))
	     fr)
	  (set! (sample pos index i) (frame-ref fr i))))))

	
(define (region->frame pos reg) ; arg order is like region-sample
  "(region->frame pos reg) returns a frame with the contents of region reg at position pos"
  (if (not (region? reg))
      (throw 'no-such-region (list "region->frame" reg))
      (let ((fr (make-frame (region-chans reg))))
	(do ((i 0 (1+ i)))
	    ((= i (region-chans reg))
	     fr)
	  (frame-set! fr i (region-sample pos reg i))))))


(define* (sound->sound-data :optional (beg 0) dur snd)
  "(sound->sound-data beg dur :optional snd) returns a sound-data object containing the samples in sound snd starting at position beg for dur frames"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "sound->sound-data" snd))
	(let* ((chns (chans index))
	       (len (or dur (max 1 (- (frames index) beg))))
	       (sd (make-sound-data chns len)))
	  (do ((i 0 (1+ i)))
	      ((= i chns) 
	       sd)
	    (vct->sound-data (channel->vct beg len index i) sd i))))))

(define* (sound-data->sound sd :optional (beg 0) dur snd)
  "(sound-data->sound sd beg :optional dur snd) places the contents of sound-data sd into sound snd starting at position beg for dur frames"
  (if (not (sound-data? sd))
      (throw 'wrong-type-arg (list "sound-data->sound" sd))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (throw 'no-such-sound (list "sound->sound-data" snd))
	    (let ((ndur (or dur (sound-data-length sd))))
	      (do ((i 0 (1+ i)))
		  ((= i (chans index)) 
		   sd)
		(vct->channel (sound-data->vct sd i) beg ndur index i)))))))



(define +frame-reader-tag+ 0)
(define +frame-reader-snd+ 1)
(define +frame-reader-channels+ 2)
(define +frame-reader-frame+ 3)
(define +frame-reader0+ 4)

(define* (make-frame-reader :optional (beg 0) snd dir edpos)
  "(make-frame-reader :optional beg snd dir edpos) returns a frame reader, basically a sample reader that reads all channels on each call"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (and (not (sound? index))
	     (not (string? index))) ; filename is a possibility here
	(throw 'no-such-sound (list "make-frame-reader" snd))
	(let* ((chns (if (sound? index) (chans index) (mus-sound-chans index)))
	       (fr (make-vector (+ chns +frame-reader0+))))
	  (vector-set! fr +frame-reader-tag+ 'frame-reader)
	  (vector-set! fr +frame-reader-snd+ index)
	  (vector-set! fr +frame-reader-channels+ chns)
	  (vector-set! fr +frame-reader-frame+ (make-frame chns))
	  (do ((i 0 (1+ i)))
	      ((= i chns))
	    (vector-set! fr (+ i +frame-reader0+) (make-sample-reader beg index i dir edpos)))
	  fr))))

(define (frame-reader? obj)
  "(frame-reader? obj) -> #t if obj is a frame-reader"
  (and (vector? obj)
       (eq? (vector-ref obj +frame-reader-tag+) 'frame-reader)))

(define (frame-reader-at-end? fr)
  "(frame-reader-at-end? fr) -> #t if the sample-readers in frame-reader fr have reached the end of their respective channels"
  (if (frame-reader? fr)
      (let ((at-end #t))
	(do ((i 0 (1+ i)))
	    ((or (not at-end) 
		 (= i (vector-ref fr +frame-reader-channels+)))
	     at-end)
	  (set! at-end (sample-reader-at-end? (vector-ref fr (+ i +frame-reader0+))))))
      (throw 'wrong-type-arg (list "frame-reader-at-end" fr))))

(define (frame-reader-position fr)
  "(frame-reader-position fr) -> current read position of frame-reader fr"
  (if (frame-reader? fr)
      (sample-reader-position (vector-ref fr +frame-reader0+))
      (throw 'wrong-type-arg (list "frame-reader-position" fr))))

(define (frame-reader-home fr)
  "(frame-reader-home fr) -> sound index associated with frame-reader fr"
  (if (frame-reader? fr)
      (vector-ref fr +frame-reader-snd+)
      (throw 'wrong-type-arg (list "frame-reader-home" fr))))

(define (frame-reader-chans fr)
  "(frame-reader-chans fr) -> number of channels read by frame-reader fr"
  (if (frame-reader? fr)
      (vector-ref fr +frame-reader-channels+)
      (throw 'wrong-type-arg (list "frame-reader-chans" fr))))

(define (free-frame-reader fr)
  "(free-frame-reader fr) frees all readers associated with frame-reader fr"
  (if (frame-reader? fr)
      (do ((i 0 (1+ i)))
	  ((= i (vector-ref fr +frame-reader-channels+)))
	(free-sample-reader (vector-ref fr (+ i +frame-reader0+))))
      (throw 'wrong-type-arg (list "free-frame-reader" fr))))

(define (copy-frame-reader fr)
  "(copy-frame-reader fr) returns a copy of frame-reader fr"
  (if (frame-reader? fr)
      (let* ((chns (vector-ref fr +frame-reader-channels+))
	     (nfr (make-vector (+ chns +frame-reader0+))))
	(vector-set! nfr +frame-reader-tag+ 'frame-reader)
	(vector-set! nfr +frame-reader-snd+ (vector-ref fr 1))
	(vector-set! nfr +frame-reader-channels+ chns)
	(do ((i 0 (1+ i)))
	    ((= i chns))
	  (vector-set! nfr (+ i +frame-reader0+) (copy-sample-reader (vector-ref fr (+ i +frame-reader0+)))))
	nfr)
      (throw 'wrong-type-arg (list "copy-frame-reader" fr))))

(define (next-frame fr)
  "(next-frame fr) returns the next frame as read by frame-reader fr"
  (let ((vals (vector-ref fr +frame-reader-frame+)))
    (do ((i 0 (1+ i)))
	((= i (vector-ref fr +frame-reader-channels+)))
      (frame-set! vals i (next-sample (vector-ref fr (+ i +frame-reader0+)))))
    vals))

(define (previous-frame fr)
  "(previous-frame fr) returns the previous frame as read by frame-reader fr"
  (let ((vals (vector-ref fr +frame-reader-frame+)))
    (do ((i 0 (1+ i)))
	((= i (vector-ref fr +frame-reader-channels+)))
      (frame-set! vals i (previous-sample (vector-ref fr (+ i +frame-reader0+)))))
    vals))

(define (read-frame fr)
  "(read-frame fr) returns the next frame read by frame-reader fr taking its current read direction into account"
  (let ((vals (vector-ref fr +frame-reader-frame+)))
    (do ((i 0 (1+ i)))
	((= i (vector-ref fr +frame-reader-channels+)))
      (frame-set! vals i (read-sample (vector-ref fr (+ i +frame-reader0+)))))
    vals))


(define* (make-region-frame-reader beg reg :optional dir)
  "(make-region-frame-reader beg reg :optional dir) returns a frame-reader reading the contents of region reg"
  (if (not (region? reg))
      (throw 'no-such-region (list "make-region-frame-reader" reg))
      (let* ((chns (region-chans reg))
	     (fr (make-vector (+ chns +frame-reader0+))))
	(vector-set! fr +frame-reader-tag+ 'frame-reader)
	(vector-set! fr +frame-reader-snd+ reg)
	(vector-set! fr +frame-reader-channels+ chns)
	(vector-set! fr +frame-reader-frame+ (make-frame chns))
	(do ((i 0 (1+ i)))
	    ((= i chns))
	  (vector-set! fr (+ i +frame-reader0+) (make-region-sample-reader beg reg i dir)))
	fr)))

(define* (make-sync-frame-reader :optional (beg 0) snd dir edpos)
  "(make-sync-frame-reader :optional beg snd dir edpos) returns a frame reader that reads all channels sync'd to 'snd'"
  (let* ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "make-sync-frame-reader" snd))
	(let ((snc (sync index)))
	  (if (= snc 0)
	      (make-frame-reader beg snd dir edpos)
	      (let ((chns 0))
		(for-each
		 (lambda (s)
		   (if (= (sync s) snc) ; sync field is always an int (0 = none)
		       (set! chns (+ chns (chans s)))))
		 (sounds))
		(let ((fr (make-vector (+ chns +frame-reader0+))))
		  (vector-set! fr +frame-reader-tag+ 'frame-reader)
		  (vector-set! fr +frame-reader-snd+ index)
		  (vector-set! fr +frame-reader-channels+ chns)
		  (vector-set! fr +frame-reader-frame+ (make-frame chns))
		  (let ((ctr 0))
		    (for-each 
		     (lambda (s)
		       (if (= (sync s) snc)
			   (begin
			     (do ((i 0 (1+ i)))
				 ((= i (chans s)))
			       (vector-set! fr (+ (+ i ctr) +frame-reader0+) (make-sample-reader beg s i dir edpos)))
			     (set! ctr (+ ctr (chans s))))))
		     (sounds)))
		  fr)))))))

(define* (make-selection-frame-reader :optional (beg 0))
  "(make-selection-frame-reader :optional (beg 0)) returns a frame reader that reads all channels of the current selection"
  (if (not (selection?))
      (throw 'no-active-selection (list "make-selection-frame-reader" beg))
      (let* ((chns (selection-chans))
	     (fr (make-vector (+ chns +frame-reader0+))))
	(vector-set! fr +frame-reader-tag+ 'frame-reader)
	(vector-set! fr +frame-reader-snd+ -1)
	(vector-set! fr +frame-reader-channels+ chns)
	(vector-set! fr +frame-reader-frame+ (make-frame chns))
	(let ((ctr 0))
	  (for-each
	   (lambda (snd)
	     (do ((chn 0 (1+ chn)))
		 ((= chn (chans snd)))
	       (if (selection-member? snd chn)
		   (begin
		     (vector-set! fr (+ ctr +frame-reader0+) (make-sample-reader (+ beg (selection-position snd chn)) snd chn))
		     (set! ctr (1+ ctr))))))
	   (sounds)))
	fr)))

(define (make-track-frame-reader beg trk) ; following region reader in arg order
  "(make-track-frame-reader beg trk) returns a frame reader that reads all the channels in the track trk"
  (if (not (track? trk))
      (throw 'no-such-track (list "make-track-frame-reader" trk))
      (let* ((chns (track-chans trk))
	     (fr (make-vector (+ chns +frame-reader0+)))
	     (pos (track-position trk)))
	(vector-set! fr +frame-reader-tag+ 'frame-reader)
	(vector-set! fr +frame-reader-snd+ trk)
	(vector-set! fr +frame-reader-channels+ chns)
	(vector-set! fr +frame-reader-frame+ (make-frame chns))
	(do ((i 0 (1+ i)))
	    ((= i chns))
	  (let* ((chan-pos (- (track-position trk i) pos))
		 (chan-beg (- (or beg 0) chan-pos))) ; track chans can start at different places, but track-sample-reader is chan-specific
	    (vector-set! fr (+ i +frame-reader0+) 
			 (if (>= chan-beg 0)
			     (make-track-sample-reader trk i chan-beg)
			     chan-beg))))
	fr)))

(define (read-track-frame fr)
  "(read-track-frame fr) returns the next frame read by the track-frame-reader 'fr'"
  (let ((vals (vector-ref fr +frame-reader-frame+)))
    (do ((i 0 (1+ i)))
	((= i (vector-ref fr +frame-reader-channels+)))
      (let ((rd (vector-ref fr (+ i +frame-reader0+))))
	(if (number? rd)
	    (begin
	      (frame-set! vals i 0.0)
	      (if (>= rd -1)
		  (begin
		    (set! rd (make-track-sample-reader (vector-ref fr +frame-reader-snd+) i 0))
		    (vector-set! fr (+ i +frame-reader0+) rd))
		  (vector-set! fr (+ i +frame-reader0+) (1+ rd))))
	    (frame-set! vals i (read-track-sample rd)))))
    vals))


(define (file->vct file)
  "(file->vct file) returns a vct with file's data (channel 0)"
  (let* ((len (mus-sound-frames file))
	 (reader (make-sample-reader 0 file))
	 (data (make-vct len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (vct-set! data i (next-sample reader)))
    (free-sample-reader reader)
    data))

(define* (vct->file v file :optional (srate 22050) (comment ""))
  "(vct->file v file :optional srate comment) writes the data in vct v to the specified sound file"
  (if (vct? v)
      (let ((fd (mus-sound-open-output file srate 1 mus-lfloat mus-riff comment)))
	(mus-sound-write fd 0 (1- (vct-length v)) 1 (vct->sound-data v))
	(mus-sound-close-output fd (* 4 (vct-length v)))
	file)
      (throw 'wrong-type-arg (list "file->vct" v))))


(define (file->sound-data file)
  "(file->sound-data file) returns a sound-data object with the contents of file"
  (let* ((len (mus-sound-frames file))
	 (chns (mus-sound-chans file))
	 (reader (make-frame-reader 0 file))
	 (data (make-sound-data chns len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (frame->sound-data (read-frame reader) data i))
    (free-frame-reader reader)
    data))

(define* (sound-data->file sd file :optional (srate 22050) (comment ""))
  "(sound-data->file sd file :optional srate comment) writes the contents of sound-data sd to file"
  (if (sound-data? sd)
      (let ((fd (mus-sound-open-output file srate (sound-data-chans sd) mus-lfloat mus-riff comment)))
	(mus-sound-write fd 0 (1- (sound-data-length sd)) (sound-data-chans sd) sd)
	(mus-sound-close-output fd (* 4 (sound-data-length sd) (sound-data-chans sd)))
	file)
      (throw 'wrong-type-arg (list "sound-data->file" sd))))


(define (region->sound-data reg)
  "(region->sound-data reg) returns a sound-data object with the contents of region reg"
  (if (region? reg)
      (let* ((reader (make-region-frame-reader 0 reg))
	     (len (region-frames reg))
	     (chns (region-chans reg))
	     (data (make-sound-data chns len)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (frame->sound-data (read-frame reader) data i))
	(free-frame-reader reader)
	data)
      (throw 'no-such-region (list "region->sound-data" reg))))

(define* (selection->sound-data :optional (beg 0))
  "(selection->sound-data :optional beg) returns a sound-data object with the contents of current selection"
  (if (selection?)
      (let* ((reader (make-selection-frame-reader beg))
	     (len (- (selection-frames) beg))
	     (chns (selection-chans))
	     (sd (make-sound-data chns len)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (frame->sound-data (read-frame reader) sd i))
	(free-frame-reader reader)
	sd)
      (throw 'no-active-selection (list "selection->sound-data"))))

(define* (track->sound-data trk :optional (beg 0))
  "(track->sound-data trk) returns a sound-data object with the contents of track trk"
  (if (not (track? trk))
      (throw 'no-such-track (list "track->sound-data" trk))
      (let* ((chns (track-chans trk))
	     (len (track-frames trk))
	     (sd (make-sound-data chns len))
	     (reader (make-track-frame-reader beg trk)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (frame->sound-data (read-track-frame reader) sd i))
	(free-frame-reader reader)
	sd)))


(define* (insert-vct v :optional (beg 0) dur snd chn edpos)
  "(insert-vct v :optional beg dur snd chn edpos) inserts vct v's data into sound snd at beg"
  (if (not (vct? v))
      (throw 'wrong-type-arg (list "insert-vct" v))
      (let ((len (or dur (vct-length v))))
	(insert-samples beg len v snd chn edpos #f (format #f "insert-vct ~A ~A ~A" (vct->string v) beg dur)))))

(define* (insert-frame fr :optional (beg 0) snd edpos)
  "(insert-frame fr :optional beg snd edpos) inserts frame fr's data into sound snd (one sample in each channel) at beg"
  (if (not (frame? fr))
      (throw 'wrong-type-arg (list "insert-frame" fr))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (throw 'no-such-sound (list "insert-frame" snd))
	    (let ((chns (min (mus-length fr) (chans index))))
	      (do ((chn 0 (1+ chn)))
		  ((= chn chns))
		(insert-sample beg (frame-ref fr chn) index chn edpos)))))))

(define* (insert-sound-data sd :optional (beg 0) dur snd edpos)
  "(insert-sound-data sd :optional beg dur snd edpos) inserts sound-data sd's contents into sound snd at beg"
  ;; this should be built-into insert-samples
  (if (not (sound-data? sd))
      (throw 'wrong-type-arg (list "insert-sound-data" sd))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (throw 'no-such-sound (list "insert-sound-data" snd))
	    (let* ((chns (min (sound-data-chans sd) (chans index)))
		   (len (or dur (sound-data-length sd)))
		   (v (make-vct len)))
	      (do ((chn 0 (1+ chn)))
		  ((= chn chns))
		(insert-samples beg len (sound-data->vct sd chn v) index chn edpos #f "insert-sound-data")))))))


(define* (mix-frame fr :optional (beg 0) snd)
  "(mix-frame fr :optional beg snd) mixes frame fr's data into sound snd (one sample in each channel) at beg"
  (if (not (frame? fr))
      (throw 'wrong-type-arg (list "mix-frame" fr))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (throw 'no-such-sound (list "mix-frame" snd))
	    (let ((chns (min (mus-length fr) (chans index))))
	      (do ((chn 0 (1+ chn)))
		  ((= chn chns))
		(set! (sample beg index chn) (+ (frame-ref fr chn) (sample beg index chn)))))))))

(define* (mix-sound-data sd :optional (beg 0) dur snd tagged trk)
  "(mix-sound-data sd :optional beg dur snd tagged trk) mixes the contents of sound-data sd into sound snd at beg"
  (if (not (sound-data? sd))
      (throw 'wrong-type-arg (list "mix-sound-data" sd))
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (throw 'no-such-sound (list "mix-sound-data" snd))
	    (let* ((chns (min (sound-data-chans sd) (chans index)))
		   (len (or dur (sound-data-length sd)))
		   (v (make-vct len))
		   (mix-id #f))
	      (do ((chn 0 (1+ chn)))
		  ((= chn chns))
		(let ((id (mix-vct (sound-data->vct sd chn v) beg index chn tagged "mix-sound-data" trk)))
		  (if (not mix-id) (set! mix-id id))))
	      mix-id)))))

  
(define* (scan-sound func :optional (beg 0) dur snd with-sync)
  "(scan-sound func :optional beg dur snd with-sync) is a version of scan-channel that passes func a frame on each call, rather than a sample"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((reader (if with-sync
			   (make-sync-frame-reader beg index)
			   (make-frame-reader beg index)))
	       (result #f)
	       (len (frames index))
	       (end (if dur (min len (+ beg dur)) len)))
	  (do ((i beg (1+ i)))
	      ((or result (= i end))
	       (and result
		    (list result (1- i))))
	    (set! result (func (read-frame reader)))))
	(throw 'no-such-sound (list "scan-sound" snd)))))

(define +read-forward+ 1)

(define* (map-sound func :optional (beg 0) dur snd edpos)
  "(map-sound func :optional beg dur snd edpos) is a version of map-channel that passes func a frame on each call, rather than a sample"
  ;; not sure map-sound with sync is a good idea -- even scale-by following sync seems bad
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (chans index))
	       (reader (make-frame-reader beg index +read-forward+ edpos))
	       (filename (snd-tempnam))
	       (writer (make-frame->file filename out-chans))
	       (len (frames index))
	       (end (if dur (min len (+ beg dur)) len))
	       (loc 0))
	  (do ((i beg (1+ i))) 
	      ((= i end))
	    (let ((result (func (next-frame reader))))
	      (if result 
		  (begin
		    (frame->file writer loc result)
		    (set! loc (1+ loc))))))
	  (mus-close writer)
	  (free-frame-reader reader)
	  (do ((i 0 (1+ i)))
	      ((= i (chans index)))
	    (set! (samples beg loc index i #f "map-sound" i #f (= i 0)) filename))) ; edpos = #f, auto-delete = chan=0
	(throw 'no-such-sound (list "map-sound" snd)))))


;;; SOMEDAY: channel slicer: grn but output locs permuted, reversed etc

(define* (simultaneous-zero-crossing :optional (beg 0) dur snd)
  "(simultaneous-zero-crossing :option beg dur snd) looks through all channels of 'snd' for a simultaneous zero crossing."
  (let ((last-fr (make-frame (chans snd))))
    (scan-sound (lambda (fr)
		  (let ((result #t))
		    (do ((chn 0 (1+ chn)))
			((= chn (mus-length fr)))
		      (set! result (and result (< (* (frame-ref fr chn) (frame-ref last-fr chn)) 0.0)))
		      (frame-set! last-fr chn (frame-ref fr chn)))
		    result))
		beg dur snd)))

