;;; backwards compatibility for Snd-6

(define graph-lisp?                    lisp-graph?)
(define graph-transform?               transform-graph?)
(define graph-time?                    time-graph?)

(define graph-time-once                graph-once)
(define graph-transform-once           graph-once)
(define graph-time-as-wavogram         graph-as-wavogram)
(define graph-transform-as-sonogram    graph-as-sonogram)
(define graph-transform-as-spectrogram graph-as-spectrogram)

(define dont-normalize-transform       dont-normalize)
(define normalize-transform-by-channel normalize-by-channel)
(define normalize-transform-by-sound   normalize-by-sound)
(define normalize-transform-globally   normalize-globally)

(define set-oss-buffers mus-audio-set-oss-buffers)

(define (max-sounds) (apply max (sounds)))

(define (transform-samples snd chn) 
  (vct->vector (transform-samples->vct snd chn)))

(define (region-samples samp samps reg chn) 
  (vct->vector (region-samples->vct samp samps reg chn)))

(define (convolve-arrays v0 v1)
  (let ((vc0 (if (vector? v0 (vector->vct v0)) v0))
	(vc1 (if (vector? v1 (vector->vct v1)) v1)))
    (vct-convolve! vc0 vc1)
    (if (vector? v0)
	(vct->vector vc0)
	vc0)))

;;; 'impossible-bounds changed to 'out-of-range

(define (clear-audio-inputs)
  "OSS-specific: clears input lines, maximizes output gains; intent is to reduce noise"
  (let ((vals (make-vector 32)))
    (vector-set! vals 0 0.0)
    (vector-set! vals 1 0.0)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-microphone 0 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-igain 0 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-line 0 vals)
    (vector-set! vals 0 1.0)
    (vector-set! vals 1 1.0)
    (mus-audio-mixer-write mus-audio-default mus-audio-amp 2 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-ogain 0 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-pcm 2 vals)))

(define* (append-to-minibuffer msg #:optional snd)
  (if (and (sound? snd)
	   (not (provided? 'snd-nogui)))
      (let* ((minibuffer (and (sound-widgets snd) 
			      (list-ref (sound-widgets snd) 3)))
	     (text (and minibuffer 
			(widget-text minibuffer))))
	(if (string? text)
	    (set! (widget-text minibuffer) (string-append text msg))
	    (report-in-minibuffer msg snd)))))

(define (select-mix id) (set! (selected-mix) id))

(define use-sinc-interp
  (make-procedure-with-setter
   (lambda () #t)
   (lambda (val) val)))

(define (mus-set-srate val) (set! (mus-srate) val))
(define (mus-set-rand-seed val) (set! (mus-rand-seed) val))
(define (mus-file-set-prescaler fd val) (set! (mus-file-prescaler fd) val))
(define (mus-file-set-data-clipped fd val) (set! (mus-file-data-clipped fd) val))
(define (mus-sound-set-maxamp file vals) (set! (mus-sound-maxamp file) vals))

(define (dismiss-all-dialogs)
  (define (is-active? dialog)
    (if (provided? 'xm)
	(XtIsManaged dialog)
	#t))
  (define (deactivate dialog)
    (if (provided? 'xm)
	(XtUnmanageChild dialog)
	(if (provided? 'xg)
	    (gtk_widget_hide dialog))))
  (for-each
   (lambda (dialog)
     (if dialog
	 (if (is-active? dialog)
	     (deactivate dialog))))
   (dialog-widgets)))

(define change-property change-window-property)

;;; to get a vector from samples, use vct->vector

(define (mus-sound-seek fd loc seek format chans)
  ;; mus-sound-seek-frame = lseek fd data-location+frames*chans*sample-size-in-bytes seek-set
  ;; old mus-sound-seek = lseek fd data-location+loc-as-16-in-bytes seek-set
  ;; but there's no direct way to get the data-format and chans from the file descriptor
  ;; so in this replacement, there are two added args
  (let* ((datum-bytes (mus-data-format-bytes-per-sample format))
	 (fixup-loc (inexact->exact (/ (* loc datum-bytes) (* 2 chans)))))
    (mus-sound-seek-frame fd loc)))

(define (with-html)
  (add-hook! menu-hook
	     (lambda (m o)
	       (define (make-url n)
		 (if (and (html-dir)
			  (not (string=? (html-dir) ".")))
		     (string-append (html-dir) "/" n)
		     (string-append (getcwd) "/" n)))
	       (if (string=? m "Help")
		   (let ((fallback #f))
		     (cond ((string=? o "Find") (send-netscape (make-url "snd.html#find")))
			   ((string=? o "Undo") (send-netscape (make-url "snd.html#undoredo")))
			   ((string=? o "Sync") (send-netscape (make-url "snd.html#multichannel")))
			   ((string=? o "Contrast") (send-netscape (make-url "snd.html#contrast")))
			   ((string=? o "Envelope") (send-netscape (make-url "snd.html#editenvelope")))
			   ((string=? o "About") (send-netscape (make-url Snd "snd.html#gettingstarted")))
			   ((string=? o "FFT") (send-netscape (make-url "snd.html#viewfft")))
			   ((string=? o "Speed") (send-netscape (make-url "snd.html#speed")))
			   ((string=? o "Expand") (send-netscape (make-url "snd.html#expand")))
			   ((string=? o "Reverb") (send-netscape (make-url "snd.html#reverb")))
			   ((string=? o "Marks") (send-netscape (make-url "snd.html#marks")))
			   ((string=? o "Mixing") (send-netscape (make-url "snd.html#mixingfiles")))
			   ((string=? o "Format") (send-netscape (make-url "snd.html#formats")))
			   ((string=? o "Recording") (send-netscape (make-url "snd.html#recordfile")))
			   ((string=? o "Customization") (send-netscape (make-url "extsnd.html#extsndcontents")))
			   (else (set! fallback #t)))
		     fallback)
		   #t))))


