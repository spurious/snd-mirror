;;; various low-level IO and "real-time" functions/examples


(define (card+device card device)
  ;; sndlib (for dubious historical reasons) packs the sound card number into the same parameter as the device number
  (logior (ash card 16) device))

;; for simplicity the C->Scheme code in sndlib2scm.c for mus-audio-read|write
;;   assumes you're asking the sound device to deal with shorts (16-bit quantities).
;;   This could be changed, but see the Snd recorder for a cautionary tale...

(define our-short (if (little-endian?) mus-lshort mus-bshort))
(define our-srate 22050)
(define our-dac-buffer-size-in-bytes 512)
(define our-dac-buffer-size-in-shorts 256)
(define our-chans 1)
(define our-chan 0)
(define our-default-card-number 0)


(define (show-input . arg)
  ;; show (as time domain graph) whatever is being seen from input port
  ;;    this needs a dummy file open to get a place to put the lisp graph
  (let* ((in-sys (if (not (null? arg)) 
		     (car arg) 
		     our-default-card-number))
	 (in-port (mus-audio-open-input 
		   (card+device in-sys mus-audio-default) 
		   our-srate our-chans our-short our-dac-buffer-size-in-bytes))
	 (data (make-sound-data our-chans our-dac-buffer-size-in-shorts))
    	 (vobj (make-vct our-dac-buffer-size-in-shorts)))
    (do ()
	((c-g?)) ; run until C-g stops us
      (mus-audio-read in-port data our-dac-buffer-size-in-shorts)
      (graph (sound-data->vct data our-chan vobj)))
    (mus-audio-close in-port)))


(define (show-input-fft . arg)
  ;; show (as spectrum) whatever is being seen from input port
  (let* ((in-sys (if (not (null? arg)) 
		     (car arg) 
		     our-default-card-number))
	 (in-port (mus-audio-open-input 
		   (card+device in-sys mus-audio-default) 
		   our-srate our-chans our-short our-dac-buffer-size-in-bytes))
	 (data (make-sound-data our-chans our-dac-buffer-size-in-shorts))
    	 (vobj (make-vct our-dac-buffer-size-in-shorts)))
    (do ()
	((c-g?))
      (mus-audio-read in-port data our-dac-buffer-size-in-shorts)
      (sound-data->vct data our-chan vobj)
      (graph (snd-spectrum vobj blackman2-window our-dac-buffer-size-in-shorts #t))) ;probably better to have bounds here
    (mus-audio-close in-port)))


(define (in-out func in-sys out-sys)
  ;; read audio data from in-sys (a sound card), pass it to func for processing, write it to out-sys
  (let ((out-port (mus-audio-open-output 
		   (card+device out-sys mus-audio-default) 
		   our-srate our-chans our-short our-dac-buffer-size-in-bytes))
	(in-port (mus-audio-open-input 
		  (card+device in-sys mus-audio-default) 
		  our-srate our-chans our-short our-dac-buffer-size-in-bytes))
	(data (make-sound-data our-chans our-dac-buffer-size-in-shorts))
	(vobj (make-vct our-dac-buffer-size-in-shorts)))
    (do ()
	((c-g?))
      (mus-audio-read in-port data our-dac-buffer-size-in-shorts)
      ;; now process the sound...
      (func data)
      (mus-audio-write out-port data our-dac-buffer-size-in-shorts))
    (mus-audio-close in-port)
    (mus-audio-close out-port)))

;;; now some "func"s for in-out above
(define (amplify amp)
  (lambda (data)
    (do ((i 0 (1+ i)))
	((= i our-dac-buffer-size-in-shorts))
      (sound-data-set! data our-chan i (* (sound-data-ref data our-chan i) amp)))))

;;; (in-out (amplify 2.0) 1 0)

(define (lp)
  (let ((val 0.0))
    (lambda (data)
      (do ((i 0 (1+ i)))
	  ((= i our-dac-buffer-size-in-shorts))
	(let ((curval (sound-data-ref data our-chan i)))
	  (sound-data-set! data our-chan i (* .5 (+ curval val)))
	  (set! val curval))))))

;;; (in-out (lp) 1 0)

(define (hp frq)
  (let ((flt (make-formant .99 frq))
	(vobj (make-vct our-dac-buffer-size-in-shorts)))
    (lambda (data)
      (do ((i 0 (1+ i)))
	  ((= i our-dac-buffer-size-in-shorts))
	(let ((curval (sound-data-ref data our-chan i)))
	  (sound-data-set! data our-chan i (formant flt curval))))
      (graph (snd-spectrum (sound-data->vct data our-chan vobj) blackman2-window our-dac-buffer-size-in-shorts #t)))))

;;; (in-out (hp 1200) 1 0)


(define (show-draggable-input-fft . arg)
  ;; show (as draggable spectrum) whatever is being seen from input port
  (let ((mouse-down 0)
	(mouse-pos 0.0)
	( x1 1.0))

    (define (mouse-press chn snd button state x y)
      (set! mouse-pos (/ x x1))
      (set! mouse-down x1))

    (define (mouse-drag snd chn button state x y)
      (let* ((xnew (/ x x1))
	     (lim (min 1.0 (max 0.1 (+ mouse-down (- mouse-pos xnew))))))
	(set! x1 lim)))

    (let* ((in-sys (if (not (null? arg)) 
		       (car arg) 
		       our-default-card-number))
	   (in-port (mus-audio-open-input 
		     (card+device in-sys mus-audio-default) 
		     our-srate our-chans our-short our-dac-buffer-size-in-bytes))
	   (data (make-sound-data our-chans our-dac-buffer-size-in-shorts))
	   (vobj (make-vct our-dac-buffer-size-in-shorts)))
      (add-hook! mouse-drag-hook mouse-drag)
      (add-hook! mouse-press-hook mouse-press)
      (do ()
	  ((c-g?))
	(mus-audio-read in-port data our-dac-buffer-size-in-shorts)
	(if (= x1 1.0)
	    (graph 
	     (snd-spectrum 
	      (sound-data->vct data our-chan vobj)
	      blackman2-window our-dac-buffer-size-in-shorts #t)
	     "spectrum"
	     0.0 x1)
	    (let ((maxpt (inexact->exact (* x1 our-dac-buffer-size-in-shorts))))
	      (graph
	       (snd-spectrum
		(vct-subseq (sound-data->vct data our-chan vobj) 0 maxpt)
		blackman2-window maxpt #t)
	       "spectrum"
	       0.0 x1))))
      (remove-hook! mouse-drag-hook mouse-drag)
      (remove-hook! mouse-press-hook mouse-press)
      (mus-audio-close in-port))))




(define (show-input-spectrogram traces)
  ;; this shows an on-going spectrogram of the input data, but it needs work!
  ;;   TODO: reduce flashing by combining graphs
  ;;   TODO: make the offset business built-in, rather than use this vct-offset/move kludge
  ;;   TODO: how to turn off the colors?
  (let ((ffts (make-vector traces))
	(fft-size 512)
	(our-sys 1)
	(g-size 64)
	(vf (make-vct 512)))
    (define (collect-ffts flist ind last)
      (if (= ind last)
	  flist
	  (collect-ffts 
	   (append flist (list (vector-ref ffts ind)))
	   (if (= ind 0) (1- traces) (1- ind))
	   last)))
    (do ((i 0 (1+ i)))
	((= i traces))
      (vector-set! ffts i (make-vct g-size)))
    (let* ((in-sys 0)
	   (in-port (mus-audio-open-input (logior (ash our-sys 16) mus-audio-default) 22050 1 our-short (* 2 fft-size)))
	   (data (make-sound-data 1 fft-size))
	   (ind 0))
      (do ()
	  ((c-g?))
	(let ((cur-v (vector-ref ffts ind)))
	  (mus-audio-read in-port data fft-size)
	  (sound-data->vct data 0 vf)
	  (snd-spectrum vf blackman2-window fft-size #t)
	  (vct-subseq vf 0 (1- g-size) cur-v)
	  (let ((last (if (= ind (1- traces)) 0 (1+ ind))))
	    (do ((i ind))
		((= i last))
	      (let ((v (vector-ref ffts i)))
		;(vct-move! v (- g-size 1) (- g-size 2) #t)
		(vct-offset! v (/ .5 traces)))
	      (set! i (- i 1))
	      (if (< i 0) (set! i (- traces 1)))))
	  (graph (collect-ffts '() ind (if (= ind (1- traces)) 0 (1+ ind)))
		 "spectrum"
		 0.0
		 (/ (* 11025 g-size) fft-size))
	  (set! ind (+ ind 1))
	  (if (>= ind traces) (set! ind 0))))
      (mus-audio-close in-port))))
