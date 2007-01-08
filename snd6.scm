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

;;; 'impossible-bounds changed to 'out-of-range

(define (clear-audio-inputs)
  "OSS-specific: clears input lines, maximizes output gains; intent is to reduce noise"
  (let ((vals (make-vct 32)))
    (vct-set! vals 0 0.0)
    (vct-set! vals 1 0.0)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-microphone 0 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-igain 0 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-line 0 vals)
    (vct-set! vals 0 1.0)
    (vct-set! vals 1 1.0)
    (mus-audio-mixer-write mus-audio-default mus-audio-amp 2 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-ogain 0 vals)
    (mus-audio-mixer-write mus-audio-mixer mus-audio-pcm 2 vals)))

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
  (let* ((datum-bytes (mus-bytes-per-sample format))
	 ;; (fixup-loc (inexact->exact (floor (/ (* loc datum-bytes) (* 2 chans)))))
	 )
    (mus-sound-seek-frame fd loc)))

(if (not (provided? 'snd-mix.scm)) (load-from-path "mix.scm"))

(if (defined? 'sync-multichannel-mixes)
    (define mix-panel-applies-to-track sync-multichannel-mixes))
(if (defined? 'track-position)
    (define (set-track-position track pos)
      "obsolete way to set track-position"
      (set! (track-position track) pos)))
(if (defined? 'track-amp)
    (define (set-track-amp track new-amp) 
      "obsolete way to set track amp"
      (set! (track-amp track) new-amp)))
(if (defined? 'track-speed)
    (define (set-track-speed track new-speed) 
      "obsolete way to set track-speed"
      (set! (track-speed track) new-speed)))
(if (defined? 'track-color)
    (define (set-track-color track new-color) 
      "obsolete way to set track-color"
      (set! (track-color track) new-color)))
(if (defined? 'retempo-track)
    (define set-track-tempo retempo-track))
(if (defined? 'env-track)
    (define set-track-amp-env env-track))

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

(define mix-sync mix-track)

;;; unused-track/new-track -> make-track
;;; env-track -> track-amp-env
