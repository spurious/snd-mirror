;;; playing-related examples previously scattered around at random
;;;
;;; play-sound #:optional func -- play current sound, calling (func data) on each buffer if func is passed
;;; play-often, play-until-c-g -- play sound n times or until c-g
;;; play-region-forever -- play region over and over until C-g typed
;;; play samples created on-the-fly
;;; loopit -- play while looping continuously between two movable marks
;;; start-dac -- hold DAC open and play sounds via keyboard
;;; "vector synthesis"
;;; play-with-amps -- play channels with individually settable amps


;;; see also play-syncd-marks and play-between-marks in marks.scm


(use-modules (ice-9 format) (ice-9 optargs))

(define* (play-sound #:optional func)
  ;; play the currently selected sound, calling func on each data buffer, if func exists
  ;;   this is essentially what the built-in play function is doing
  ;;
  ;; first try the simple case(s) -- most sound systems can handle short (16-bit) data and 1 or 2 channels
  (let* ((filechans (chans))       ; initial settings are picked up from the current sound
	 (outchans filechans)
	 (size 256)
	 (outbytes (* size 2))     ; 2 here since we'll first try to send short (16-bit) data to the DAC
	 (audio-fd ;; ALSA throws an error where the rest of the audio cases simply report failure
	           ;;   so we turn off the "error" printout, catch the error itself, and toss it
	  (let ((no-error (hook-empty? mus-error-hook)))
	    (if no-error
		(add-hook! mus-error-hook (lambda (typ msg) #t)))
	    (let ((val (catch #t
			      (lambda ()
				(mus-audio-open-output mus-audio-default (srate) filechans 
						       (if (little-endian?) mus-lshort mus-bshort) 
						       outbytes))
			      (lambda args -1)))) ; -1 returned in case of error
	      (if no-error 
		  (reset-hook! mus-error-hook))
	      val))))
    (if (= audio-fd -1)
	;; ask card what it wants -- ALSA with some cards, for example, insists on 10 (virtual) channels and mus-lintn data!
	(let ((vals (make-vector 32)))
	  (mus-audio-mixer-read mus-audio-default mus-audio-format 32 vals)
	  (let ((fmt (inexact->exact (vector-ref vals 1))))
	    (mus-audio-mixer-read mus-audio-default mus-audio-channel 32 vals)
	    (set! outchans (inexact->exact (vector-ref vals 0)))
	    (let ((err (mus-audio-mixer-read mus-audio-default mus-audio-samples-per-channel 2 vals)))
	      (if (not (= err -1))
		  (set! size (inexact->exact (vector-ref vals 0))))
	      (let* ((bps (mus-data-format-bytes-per-sample fmt)))
		(set! outbytes (* bps size outchans))
		(set! audio-fd (mus-audio-open-output mus-audio-default (srate) outchans fmt outbytes)))))))
    ;; it's still possible the srate is unplayable -- Mac OSX only accepts stereo 44100!
    ;; (snd-print (format #f "fd: ~A, size: ~A, outchans: ~A, filechans: ~A~%" audio-fd size outchans filechans))
    (if (not (= audio-fd -1))
	(let ((len (frames))
	      (data (make-sound-data outchans size)))  ; the data buffer passed to the function (func above), then to mus-audio-write
	  (set! (dac-size) outbytes)
	  (do ((beg 0 (+ beg size)))
	      ((or (c-g?)                   ; C-g to stop in mid-stream
		   (> beg len)))
	    (if (> filechans 1)
		(do ((k 0 (1+ k)))
		    ((= k filechans))
		  (samples->sound-data beg size #f k data current-edit-position k))
		(samples->sound-data beg size #f 0 data))
	    (if func
		(func data))
	    (mus-audio-write audio-fd data size))
	  (mus-audio-close audio-fd))
	(snd-print "could not open dac"))))


;;; -------- play sound n times -- (play-often 3) for example.

(define (play-often n) 
  (let ((plays (- n 1)))
    (define (play-once snd)
      (if (or (= plays 0)
	      (c-g?))
	  (remove-hook! stop-playing-hook play-once)
	  (begin
	    (set! plays (- plays 1))
	    (play 0 snd))))
    (add-hook! stop-playing-hook play-once)
    (play)))

(bind-key (char->integer #\p) 0 (lambda (n) (play-often (max 1 n))))


;;; -------- play sound until c-g

(define (play-until-c-g)
  (define (play-once snd)
    (if (c-g?)
	(remove-hook! stop-playing-hook play-once)
	(play 0 snd)))
  (add-hook! stop-playing-hook play-once)
  (play))


;;; -------- play region over and over until C-g typed

(define (play-region-forever reg)
  (define (play-region-again reg)
    (if (c-g?)
	(remove-hook! stop-playing-region-hook play-region-again)
	(play-region reg)))
  (reset-hook! stop-playing-region-hook)
  (add-hook! stop-playing-region-hook play-region-again)
  (play-region reg))

(bind-key (char->integer #\p) 0 (lambda (n) (play-region-forever (list-ref (regions) (max 0 n)))))



;;; -------- play samples created on-the-fly
;;;
;;; play-fun sends the output of its function argument to the dac
;;;   function should return #t to indicate completion

(define play-fun
  (lambda (proc bufsize)
    (let* ((data (make-sound-data 1 bufsize))
           (bytes (* bufsize 2))
	   (audio-fd (mus-audio-open-output mus-audio-default 22050 1 mus-lshort bytes)))
      (if (not (= audio-fd -1))
	  (begin
	    (do ((result #f (proc audio-fd data bufsize))) (result))
	    (mus-audio-close audio-fd))))))

(define ampit
  ;; read current state of 0th sound and send it to the dac scaled by scaler
  (lambda (len scaler)
    (let ((beg 0))
      (lambda (fd data size) 
	(vct->sound-data (vct-scale! (samples->vct beg size) scaler) data 0)
	(mus-audio-write fd data size)
	(set! beg (+ beg size))
	(>= beg len)))))

;(play-fun (ampit (frames) 2.0) 256)

(define amprt
  ;; this one can be interrupted by C-g and uses the control-panel amp slider to set the amp 
  (lambda (len)
    (let ((beg 0))
      (lambda (fd data size) 
	(vct->sound-data (vct-scale! (samples->vct beg size) (amp)) data 0)
	(mus-audio-write fd data size)
	(set! beg (+ beg size))
	(or (c-g?) (>= beg len))))))

;(play-fun (amprt (frames)) 256)


;;; -------- play while looping continuously between two movable marks

(define loopit
  (lambda (m1 m2 bufsize)
    "(loopit mark1 mark2 buffersize) plays while looping between two marks"
    (let* ((pos1 (mark-sample m1))
	   (pos2 (mark-sample m2))
	   (beg (min pos1 pos2))
	   (end (max pos1 pos2))
	   (all-data (samples->sound-data)) ; for simplicity, just grab all the data
	   (audio-data (make-sound-data 1 bufsize))
	   (bytes (* bufsize 2))
	   (audio-fd (mus-audio-open-output mus-audio-default (srate) 1 mus-lshort bytes)))
      (if (not (= audio-fd -1))
	  (do ()
	      ((c-g?) 
	       (mus-audio-close audio-fd))
	    (do ((i 0 (1+ i)))
		((= i bufsize) 
		 (begin 
		   (set! i 0) 
		   (mus-audio-write audio-fd audio-data bufsize)))
	      (sound-data-set! audio-data 0 i (sound-data-ref all-data 0 beg))
	      (set! beg (1+ beg))
	      (if (= beg end)
		  (begin
		    (set! pos1 (mark-sample m1)) ; get current mark positions (can change while looping)
		    (set! pos2 (mark-sample m2))
		    (set! beg (min pos1 pos2))
		    (set! end (max pos1 pos2))))))))))

;;; m1 and m2 are mark (id) numbers
;;; (loopit 0 1 512)



;;; -------- hold DAC open and play sounds via keyboard
;;; 
;;; if for some reason you want the DAC to run continuously in the background,
;;;   use the "end" argument to the first player seen upon starting the dac:

(define now-playing #f)
(define hidden-player #f)

(define (start-dac)
  (if (not now-playing)
      (begin
	(set! now-playing #t)
	(set! hidden-player (make-player))
	(set! (amp-control hidden-player) 0.0)
	(add-player hidden-player 0 123456789)
	(start-playing 1 22050))))

(define (stop-dac)
  (stop-playing)
  (set! now-playing #f))

;(bind-key (char->integer #\o) 0 (lambda () (play "oboe.snd")))
;(bind-key (char->integer #\p) 0 (lambda () (play "pistol.snd")))

;;; in this particular case, there's no need to hold the DAC open
;;;   but maybe this will come in handy someday


;;; -------- "vector synthesis"
;;;
;;; this idea (and the weird name) from linux-audio-development mailing list discussion
;;;   apparently some commercial synths (or software?) provide this

(define (vector-synthesis driver files read-even-when-not-playing)

  "(vector-synthesis driver files read-even-when-not-playing) uses 'driver', a \
function of two args (the number of files, and the number of samples between calls) to decide which file to play.  If \
'read-even-when-not-playing' is #t (default is #f), the input files are constantly \
read, even if not playing.  'files' is a list of files to be played."
  
  (let ((files-len (length files)))
    (if (> files-len 0)
	(let* ((bufsize 256)
	       (srate (mus-sound-srate (car files)))
	       (chans (apply max (map mus-sound-chans files)))
	       (data (make-sound-data chans bufsize))
	       (readers (map make-file->frame files))
	       (locs (make-vector files-len 0))
	       (frames (make-vector files-len 0))
	       (current-file 0)
	       (reading #t)
	       (out-port (mus-audio-open-output mus-audio-default srate chans mus-lshort (* bufsize 2))))
	  (if (< out-port 0)
	      (snd-error "can't open audio port! ~A" out-port)
	      (begin
		(do ((i 0 (1+ i)))
		    ((= i files-len))
		  (vector-set! frames i (mus-sound-frames (list-ref files i))))
		(catch #t
		       (lambda ()
			 (while reading
				(let ((next-file (driver files-len bufsize)))
				  (if (not (= next-file current-file))
				      (let* ((ramp-down 1.0)
					     (ramp (/ 1.0 bufsize))
					     (current (list-ref readers current-file))
					     (current-loc (vector-ref locs current-file))
					     (next (list-ref readers next-file))
					     (next-loc (vector-ref locs next-file))
					     (downs (mus-channels current))
					     (ups (mus-channels next))
					     (up (make-frame ups))
					     (down (make-frame downs)))
					(do ((i 0 (1+ i)))
					    ((= i bufsize))
					  (file->frame next (+ next-loc i) up)
					  (file->frame current (+ current-loc i) down)
					  (do ((j 0 (1+ j)))
					      ((= j chans))
					    (sound-data-set! data j i 
							     (+ (if (< j downs) 
								    (* ramp-down (frame-ref down j))
								    0.0)
								(if (< j ups) 
								    (* (- 1.0 ramp-down) (frame-ref up j))
								    0.0))))
					  (set! ramp-down (- ramp-down ramp)))
					(if read-even-when-not-playing
					    (do ((i 0 (1+ i)))
						((= i files-len))
					      (vector-set! locs i (+ (vector-ref locs i) bufsize)))
					    (begin
					      (vector-set! locs current-file (+ (vector-ref locs current-file) bufsize))
					      (vector-set! locs next-file (+ (vector-ref locs next-file) bufsize))))
					(set! current-file next-file))
				      (let* ((current (list-ref readers current-file))
					     (current-loc (vector-ref locs current-file))
					     (ons (mus-channels current))
					     (on (make-frame ons)))
					(do ((i 0 (1+ i)))
					    ((= i bufsize))
					  (file->frame current (+ current-loc i) on)
					  (do ((k 0 (1+ k)))
					      ((= k chans))
					    (sound-data-set! data k i (if (< k ons) (frame-ref on k) 0.0))))
					(if read-even-when-not-playing
					    (do ((i 0 (1+ i)))
						((= i files-len))
					      (vector-set! locs i (+ (vector-ref locs i) bufsize)))
					    (vector-set! locs current-file (+ (vector-ref locs current-file) bufsize)))))
				  (mus-audio-write out-port data bufsize)
				  (set! reading (and (not (c-g?))
						     (letrec ((any-data-left 
							       (lambda (f)
								 (if (= f files-len)
								     #f
								     (or (< (vector-ref locs f) (vector-ref frames f))
									 (any-data-left (1+ f)))))))
						       (any-data-left 0)))))))
		       (lambda args (begin (snd-print (format #f "error ~A" args)) (car args))))
		(mus-audio-close out-port)))))))

#!
(vector-synthesis (let ((ctr 0) (file 0)) 
		    (lambda (files bufsize)
		      (if (> ctr 4)
			  (begin
			    (set! file (1+ file))
			    (set! ctr 0)
			    (if (>= file files)
				(set! file 0)))
			  (set! ctr (1+ ctr)))
		      file))
		  (list "oboe.snd" "pistol.snd") #t)
!#


;;; play-with-amps -- play channels with individually settable amps

(define play-with-amps
  (lambda (sound . amps)
    (let ((chans (chans sound)))
      (do ((chan 0 (1+ chan)))
          ((= chan chans))
        (let ((player (make-player sound chan)))
          (set! (amp-control player) (list-ref amps chan))
          (add-player player)))
      (start-playing chans (srate sound)))))

;(play-with-amps 0 1.0 0.5) ;plays channel 2 of stereo sound at half amplitude
