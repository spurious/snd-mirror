;;; various frame-related extensions
;;;
;;; I might move these into C 

;;; TODO: doc test frame.scm funcs
;;;            if sound-data cases, = block of frames essentially

(define* (sound->frame pos :optional snd)
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "sound->frame" snd))
	(let ((fr (make-frame (chans index))))
	  (do ((i 0 (1+ i)))
	      ((= i (chans index))
	       fr)
	    (frame-set! fr i (sample pos index i)))))))

(define* (frame->sound fr pos :optional snd)
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "frame->sound" snd))
	(do ((i 0 (1+ i)))
	    ((= i (chans index))
	     fr)
	  (set! (sample pos index i) (frame-ref fr i))))))
	
(define (region->frame pos reg) ; arg order is like region-sample
  (if (not (region? reg))
      (throw 'no-such-region (list "region->frame" reg))
      (let ((fr (make-frame (region-chans reg))))
	(do ((i 0 (1+ i)))
	    ((= i (region-chans reg))
	     fr)
	  (frame-set! fr i (region-sample pos reg i))))))


;;; track->frame ?


(define +frame-reader-tag+ 0)
(define +frame-snd+ 1)
(define +frame-channels+ 2)
(define +frame-frame+ 3)
(define +frame-reader0+ 4)

(define* (make-frame-reader :optional (beg 0) snd dir edpos)
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "make-frame-reader" snd))
	(let* ((chns (chans index))
	       (fr (make-vector (+ chns +frame-reader0+))))
	  (vector-set! fr +frame-reader-tag+ 'frame-reader)
	  (vector-set! fr +frame-reader-snd+ snd)
	  (vector-set! fr +frame-reader-channels+ chns)
	  (vector-set! fr +frame-frame+ (make-frame chns))
	  (do ((i 0 (1+ i)))
	      ((= i chns))
	    (vector-set! fr (+ i +frame-reader0+) (make-sample-reader beg snd i dir edpos)))
	  fr))))

(define (frame-reader? obj)
  (and (vector? obj)
       (eq? (vector-ref obj +frame-reader-tag+) 'frame-reader)))

(define (frame-reader-at-end fr)
  (if (frame-reader? fr)
      (let ((at-end #t))
	(do ((i 0 (1+ i)))
	    ((or (not at-end) 
		 (= i (vector-ref fr +frame-reader-channels+)))
	     at-end)
	  (set! at-end (sample-reader-at-end (vector-ref fr (+ i +frame-reader0+))))))
      (throw 'wrong-type-arg (list "frame-reader-at-end" fr))))

(define (frame-reader-position fr)
  (if (frame-reader? fr)
      (sample-reader-position (vector-ref fr +frame-reader0+))
      (throw 'wrong-type-arg (list "frame-reader-position" fr))))

(define (frame-reader-home fr)
  (if (frame-reader? fr)
      (vector-ref fr +frame-reader-snd+)
      (throw 'wrong-type-arg (list "frame-reader-home" fr))))

(define (free-frame-reader fr)
  (if (frame-reader? fr)
      (do ((i 0 (1+ i)))
	  ((= i (vector-ref fr +frame-reader-channels+)))
	(free-sample-reader (vector-ref fr (+ i +frame-reader0+))))
      (throw 'wrong-type-arg (list "free-frame-reader" fr))))

(define (copy-frame-reader fr)
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
  (let ((vals (vector-ref fr +frame-frame+)))
    (do ((i 0 (1+ i)))
	((= i (vector-ref fr +frame-channels+)))
      (frame-set! vals i (next-sample (vector-ref fr (+ i +frame-reader0+)))))
    vals))

(define (previous-frame fr)
  (let ((vals (vector-ref fr +frame-frame+)))
    (do ((i 0 (1+ i)))
	((= i (vector-ref fr +frame-channels+)))
      (frame-set! vals i (previous-sample (vector-ref fr (+ i +frame-reader0+)))))
    vals))

(define (read-frame fr)
  (let ((vals (vector-ref fr +frame-frame+)))
    (do ((i 0 (1+ i)))
	((= i (vector-ref fr +frame-channels+)))
      (frame-set! vals i (read-sample (vector-ref fr (+ i +frame-reader0+)))))
    vals))



;;; make-track-frame-reader?


(define* (make-region-frame-reader beg reg :optional dir)
  (if (not (region? reg))
      (throw 'no-such-region (list "make-region-frame-reader" reg))
      (let* ((chns (region-chans index))
	     (fr (make-vector (+ chns +frame-reader0+))))
	(vector-set! fr +frame-reader-tag+ 'frame-reader)
	(vector-set! fr +frame-reader-snd+ snd)
	(vector-set! fr +frame-reader-channels+ chns)
	(vector-set! fr +frame-frame+ (make-frame chns))
	(do ((i 0 (1+ i)))
	    ((= i chns))
	  (vector-set! fr (+ i +frame-reader0+) (make-region-sample-reader beg reg i dir)))
	fr)))



(define* (scan-sound-frames func :optional (beg 0) dur snd)
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let ((reader (make-frame-reader beg index))
	      (result #f)
	      (len (frames index))
	      (end (if dur (min len (+ beg dur)) len)))
	  (do ((i beg (1+ i)))
	      ((or result (= i end)) 
	       result)
	    (set! result (func (read-frame reader)))))
	(throw 'no-such-sound (list "scan-sound-frames" snd)))))

(define* (map-sound-frames func beg dur snd)
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((reader (make-frame-reader beg index))
	       (filename (snd-tempnam))
	       (writer (make-frame->file filename))
	       (len (frames index))
	       (end (if dur (min len (+ beg dur)) len))
	       (loc 0))
	  (do ((i beg (1+ i))) 
	      ((= i end))
	    (let ((result (func (reader))))
	      (if result 
		  (begin
		    (frame->file writer loc result)
		    (set! loc (1+ loc))))))
	  (mus-close writer)
	  (free-frame-reader reader)
	  (do ((i 0 (1+ i)))
	      ((= i (chans index)))
	    (set! (samples beg loc index i #f "map-sound" i #f #t) filename))) ; edpos = #f, auto-delete = #t
	(throw 'no-such-sound (list "map-sound-frames" snd)))))
