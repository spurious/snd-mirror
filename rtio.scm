(define (show-input . arg)
  ;; show (as time domain graph) whatever is being seen from input port
  ;;    this needs a dummy file open to get a place to put the lisp graph
  (let* ((in-sys (if (not (null? arg)) (car arg) 0))
	 (in-port (mus-audio-open-input (+ (ash in-sys 16) mus-audio-default) 22050 1 mus-lshort 512))
	 (data (make-sound-data 1 256))
    	 (vobj (make-vct 256)))
    (do ()
	((c-g?)) ; run until C-g stops us
      (mus-audio-read in-port data 256)
      (sound-data->vct data 0 vobj)
      (graph vobj))
    (mus-audio-close in-port)))

(define (show-input-fft . arg)
  ;; show (as spectrum) whatever is being seen from input port
  (let* ((in-sys (if (not (null? arg)) (car arg) 0))
	 (in-port (mus-audio-open-input (+ (ash in-sys 16) mus-audio-default) 22050 1 mus-lshort 512))
	 (data (make-sound-data 1 256))
    	 (vobj (make-vct 256)))
    (do ()
	((c-g?))
      (mus-audio-read in-port data 256)
      (sound-data->vct data 0 vobj)
      (graph (snd-spectrum vobj blackman2-window 256 #t))) ;probably better to have bounds here
    (mus-audio-close in-port)))

(define (in-out func in-sys out-sys)
  ;; read audio data from in-sys (a sound card), pass it to func for processing, write it to out-sys
  (let ((out-port (mus-audio-open-output 
		   (logior (ash out-sys 16) mus-audio-default) 
		   22050 1 mus-lshort 512))
	(in-port (mus-audio-open-input 
		  (logior (ash in-sys 16) mus-audio-default) 
		  22050 1 mus-lshort 512))
	(data (make-sound-data 1 256))
	(vobj (make-vct 256)))
    (do ()
	((c-g?))
      (mus-audio-read in-port data 256)
      ;; now process the sound...
      (func data)
      (mus-audio-write out-port data 256))
    (mus-audio-close in-port)
    (mus-audio-close out-port)))

;;; now some "func"s for in-out above
(define (amplify amp)
  (lambda (data)
    (do ((i 0 (1+ i)))
	((= i 256))
      (sound-data-set! data 0 i (* (sound-data-ref data 0 i) amp)))))

;;; (in-out (amplify 2.0) 1 0)

(define (lp)
  (let ((val 0.0))
    (lambda (data)
      (do ((i 0 (1+ i)))
	  ((= i 256))
	(let ((curval (sound-data-ref data 0 i)))
	  (sound-data-set! data 0 i (* .5 (+ curval val)))
	  (set! val curval))))))

;;; (in-out (lp) 1 0)

(define (hp frq)
  (let ((flt (make-formant .99 frq))
	(vobj (make-vct 256)))
    (lambda (data)
      (do ((i 0 (1+ i)))
	  ((= i 256))
	(let ((curval (sound-data-ref data 0 i)))
	  (sound-data-set! data 0 i (formant flt curval))))
      (graph (snd-spectrum (sound-data->vct data 0 vobj) blackman2-window 256 #t)))))

;;; (in-out (hp 1200) 1 0)
