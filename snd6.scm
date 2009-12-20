;;; backwards compatibility for Snd-7

(provide 'snd-snd6.scm)

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

(define set-oss-buffers mus-oss-set-buffers)
(define hanning-window hann-window)

(define (max-sounds) 
  "(max-sounds) -> current max sound index" 
  (apply max (sounds)))

(define (transform-samples snd chn)
  "(transform-samples snd chn) returns a vector with the given channel's transform samples"
  (vct->vector (transform-samples->vct snd chn)))

(define (region-samples samp samps reg chn)
  "(region-samples samp samps reg chn) returns a vector with the given samples in region 'reg'"
  (vct->vector (region-samples->vct samp samps reg chn)))

(define (convolve-arrays v0 v1)
  "(convolve-arrays v0 v1) is a wrapper for vct-convolve! -- v0 and v1 can be vcts or vectors"
  (let ((vc0 (if (vector? v0) (vector->vct v0) v0))
	(vc1 (if (vector? v1) (vector->vct v1) v1)))
    (vct-convolve! vc0 vc1)
    (if (vector? v0)
	(vct->vector vc0)
	vc0)))

(define* (append-to-minibuffer msg :optional snd)
  "(append-to-minibuffer msg :optional snd) appends 'msg' to whatever is in snd's minibuffer"
  (if (and (sound? snd)
	   (not (provided? 'snd-nogui)))
      (let* ((minibuffer (and (sound-widgets snd) 
			      (list-ref (sound-widgets snd) 3)))
	     (text (and minibuffer 
			(widget-text minibuffer))))
	(if (string? text)
	    (set! (widget-text minibuffer) (string-append text msg))
	    (report-in-minibuffer msg snd)))))

(define use-sinc-interp
  (make-procedure-with-setter
   (lambda () 
     "dummy accessor for obsolete use-sinc-interp"
     #t)
   (lambda (val) val)))

(define (mus-set-srate val)
  "obsolete way to set mus-srate"
  (set! (mus-srate) val))

(define (mus-set-rand-seed val) 
  "obsolete way to set the random number seed, mus-rand-seed"
  (set! (mus-rand-seed) val))

(define (mus-file-set-prescaler fd val)
  "obsolete way to set mus-file-prescaler"
  (set! (mus-file-prescaler fd) val))

(define (mus-file-set-data-clipped fd val)
  "obsolete way to set mus-file-clipping"
  (set! (mus-file-clipping fd) val))

(define (mus-sound-set-maxamp file vals) 
  "obsolete way to set mus-sound-maxamp"
  (set! (mus-sound-maxamp file) vals))

(define (change-property w a v) 
  "obsolete way to change a window-property"
  (set! (window-property w a) v))

;;; to get a vector from samples, use vct->vector

(define (mus-sound-seek fd loc seek format chans)
  "obsolete way to go to a position in a sound file -- use mus-sound-seek-frame instead"
  ;; mus-sound-seek-frame = lseek fd data-location+frames*chans*sample-size-in-bytes seek-set
  ;; old mus-sound-seek = lseek fd data-location+loc-as-16-in-bytes seek-set
  ;; but there's no direct way to get the data-format and chans from the file descriptor
  ;; so in this replacement, there are two added args
  (mus-sound-seek-frame fd loc))

(if (not (provided? 'snd-mix.scm)) (load "mix.scm"))

(define (protect-region n) 
  "obsolete -- simply produces an error"
  (snd-error "protect-region has been removed"))

(define button-font
  (make-procedure-with-setter
   (lambda ()
     "dummy accessor for obsolete button-font"
     #f)
   (lambda (val)
     #f)))

(define bold-button-font
  (make-procedure-with-setter
   (lambda ()
     "dummy accessor for obsolete bold-button-font"
     #f)
   (lambda (val)
     #f)))

(define help-text-font
  (make-procedure-with-setter
   (lambda ()
     "dummy accessor for obsolete help-text-font"
     #f)
   (lambda (val)
     #f)))
