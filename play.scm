;;; playing-related examples previously scattered around at random
;;;
;;; play-sound :optional func -- play current sound, calling (func data) on each buffer if func is passed
;;; play-often, play-until-c-g -- play sound n times or until c-g
;;; play-region-forever -- play region over and over until C-g typed
;;; loop-between-marks -- play while looping continuously between two movable marks
;;; start-dac -- hold DAC open and play sounds via keyboard
;;; "vector synthesis"
;;; play-with-amps -- play channels with individually settable amps
;;; play-sine and play-sines -- produce tones direct to DAC

;;; see also play-syncd-marks and play-between-marks in marks.scm


(use-modules (ice-9 format) (ice-9 optargs))
(provide 'snd-play.scm)

(if (not (provided? 'snd-snd8.scm)) (load-from-path "snd8.scm")) ; samples->sound-data

(define* (open-play-output :optional out-chans out-srate out-format out-bufsize)
  ;; returns (list audio-fd chans frames)
  (let* ((outchans (or out-chans 1))
	 (cur-srate (or out-srate (and (not (null? (sounds))) (srate)) 22050))
	 (pframes (or out-bufsize 256))
	 (frm (or out-format (if (little-endian?) mus-lshort mus-bshort)))
	 (outbytes (* pframes 2))     ; 2 here since we'll first try to send short (16-bit) data to the DAC
	 (audio-fd ;; ALSA throws an error where the rest of the audio cases simply report failure
	  ;;   so we turn off the "error" printout, catch the error itself, and toss it
	  (let ((no-error (hook-empty? mus-error-hook)))
	    (if no-error
		(add-hook! mus-error-hook (lambda (typ msg) #t)))
	    (let ((val (catch #t
			      (lambda ()
				(mus-audio-open-output mus-audio-default cur-srate outchans frm outbytes))
			      (lambda args -1)))) ; -1 returned in case of error
	      (if no-error 
		  (reset-hook! mus-error-hook))
	      val))))
    (if (= audio-fd -1)
	;; ask card what it wants -- ALSA with some cards, for example, insists on 10 (virtual) channels and mus-lintn data!
	(let ((vals (make-vct 32)))
	  (mus-audio-mixer-read mus-audio-default mus-audio-format 32 vals)
	  (let ((fmt (inexact->exact (vct-ref vals 1))))
	    (mus-audio-mixer-read mus-audio-default mus-audio-channel 32 vals)
	    (set! outchans (inexact->exact (vct-ref vals 0)))
	    (let ((err (mus-audio-mixer-read mus-audio-default mus-audio-samples-per-channel 2 vals)))
	      (if (not (= err -1))
		  (set! pframes (inexact->exact (vct-ref vals 0))))
	      (let* ((bps (mus-bytes-per-sample fmt)))
		(set! outbytes (* bps pframes outchans))
		(set! audio-fd (catch #t
				      (lambda ()
					(mus-audio-open-output mus-audio-default cur-srate outchans fmt outbytes))
				      (lambda args -1))))))))
    (if (not (= audio-fd -1))
	(set! (dac-size) outbytes))
    (list audio-fd outchans pframes)))


(define* (play-sound :optional func)
  "(play-sound :optional func) plays the currently selected sound, calling func on each data buffer, if func exists"
  (if (not (null? (sounds)))
      (let* ((filechans (chans))
	     (audio-info (open-play-output filechans (srate) #f 256))
	     (audio-fd (car audio-info))
	     (outchans (cadr audio-info))
	     (pframes (caddr audio-info)))
	(if (not (= audio-fd -1))
	    (let ((len (frames))
		  (data (make-sound-data outchans pframes)))  ; the data buffer passed to the function (func above), then to mus-audio-write
	      (do ((beg 0 (+ beg pframes)))
		  ((or (c-g?)                   ; C-g to stop in mid-stream
		       (> beg len)))
		(if (and (> outchans 1) (> filechans 1))
		    (do ((k 0 (1+ k)))
			((= k (min outchans filechans)))
		      (samples->sound-data beg pframes #f k data current-edit-position k))
		    (samples->sound-data beg pframes #f 0 data))
		(if func
		    (func data))
		(mus-audio-write audio-fd data pframes))
	      (mus-audio-close audio-fd))
	    (snd-print ";could not open dac")))
      (snd-print ";no sounds open")))

#|
(play-sound 
 (lambda (data)
   (let ((len (sound-data-length data)))
     (do ((i 0 (1+ i)))
	 ((= i len))
       (sound-data-set! data 0 i (* 2.0 (sound-data-ref data 0 i)))))))
|#


;;; -------- play sound n times -- (play-often 3) for example.

(define (play-often n) 
  "(play-often n) plays the selected sound 'n' times (interruptible via C-g)"
  (let ((plays (- n 1)))
    (define (play-once reason)
      (if (and (> plays 0)
	       (not (c-g?))
	       (= reason 0))
	  (begin
	    (set! plays (- plays 1))
	    (play 0 #f #f #f #f #f play-once))))
    (play 0 #f #f #f #f #f play-once)))

;(bind-key #\p 0 (lambda (n) "play often" (play-often (max 1 n))))


;;; -------- play sound until c-g

(define+ (play-until-c-g)
  "(play-until-c-g) plays the selected sound until you interrupt it via C-g"
  (define (play-once reason)
    (if (and (not (c-g?))
	     (= reason 0))
	(play 0 #f #f #f #f #f play-once)))
  (play 0 #f #f #f #f #f play-once))


;;; -------- play region over and over until C-g typed

(define+ (play-region-forever reg)
  "(play-region-forever reg) plays region 'reg' until you interrupt it via C-g"
  (define (play-region-again reason)
    (if (and (not (c-g?))  ; be extra careful (probably superfluous)
	     (= reason 0)) ; 0=play completed normally
	(play-region reg #f play-region-again)))
  (play-region reg #f play-region-again))


;(bind-key #\p 0 (lambda (n) "play region forever" (play-region-forever (list-ref (regions) (max 0 n)))))


;;; -------- play while looping continuously between two movable marks

(define (loop-between-marks m1 m2 bufsize)
  "(loop-between-marks mark1 mark2 buffersize) plays while looping between two marks"
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
		  (set! end (max pos1 pos2)))))))))

;;; m1 and m2 are mark (id) numbers
;;; (loop-between-marks 0 1 512)



;;; -------- hold DAC open and play sounds via keyboard
;;; 
;;; if for some reason you want the DAC to run continuously in the background,
;;;   use the "end" argument to the first player seen upon starting the dac:

(define (start-dac)
  "(start-dac) starts the DAC running continuously in the background"
  (let ((hidden-player (make-player)))
    (set! (amp-control hidden-player) 0.0)
    (add-player hidden-player 0 123456789)
    (start-playing 1 22050)))

(define stop-dac stop-playing)



;;; -------- "vector synthesis"
;;;
;;; this idea (and the weird name) from linux-audio-development mailing list discussion
;;;   apparently some commercial synths (or software?) provide this

(define (vector-synthesis driver files read-even-when-not-playing)

  "(vector-synthesis driver files read-even-when-not-playing) uses 'driver', a 
function of two args (the number of files, and the number of samples between calls) to decide which file to play.  If 
'read-even-when-not-playing' is #t (default is #f), the input files are constantly 
read, even if not playing.  'files' is a list of files to be played."
  
  (let ((files-len (length files)))
    (if (> files-len 0)
	(let* ((bufsize 256)
	       (srate (mus-sound-srate (car files)))
	       (chans (apply max (map mus-sound-chans files)))
	       (data (make-sound-data chans bufsize))
	       (readers (map make-file->frame files))
	       (locs (make-vector files-len 0))
	       (pframes (make-vector files-len 0))
	       (current-file 0)
	       (reading #t)
	       (out-port (mus-audio-open-output mus-audio-default srate chans mus-lshort (* bufsize 2))))
	  (if (< out-port 0)
	      (snd-error "can't open audio port! ~A" out-port)
	      (begin
		(do ((i 0 (1+ i)))
		    ((= i files-len))
		  (vector-set! pframes i (mus-sound-frames (list-ref files i))))
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
								     (or (< (vector-ref locs f) (vector-ref pframes f))
									 (any-data-left (1+ f)))))))
						       (any-data-left 0)))))))
		       (lambda args (begin (snd-print (format #f "error ~A" args)) (car args))))
		(mus-audio-close out-port)))))))

#|
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
|#


;;; play-with-amps -- play channels with individually settable amps

(define play-with-amps
  (lambda (sound . amps)
    "(play-with-amps snd :rest amps) plays snd with each channel scaled by the corresponding 
amp: (play-with-amps 0 1.0 0.5) plays channel 2 of stereo sound at half amplitude"
    (let ((chans (chans sound)))
      (do ((chan 0 (1+ chan)))
          ((= chan chans))
        (let ((player (make-player sound chan)))
          (set! (amp-control player) (list-ref amps chan))
          (add-player player)))
      (start-playing chans (srate sound)))))


;;; play-sine and play-sines

(define (play-sine freq amp)
  "(play-sine freq amp) plays a 1 second sinewave at freq and amp"
  (let* ((audio-info (open-play-output 1 22050 #f 256))
	 (audio-fd (car audio-info))
	 (outchans (cadr audio-info))
	 (pframes (caddr audio-info)))
    (if (not (= audio-fd -1))
	(let ((len 22050)
	      (osc (make-oscil freq))
	      (data (make-sound-data outchans pframes)))
	  (do ((beg 0 (+ beg pframes)))
	      ((or (c-g?)                   ; C-g to stop in mid-stream
		   (> beg len)))
	    (do ((i 0 (1+ i)))
		((= i pframes))
	      (sound-data-set! data 0 i (* amp (oscil osc))))
	    (mus-audio-write audio-fd data pframes))
	  (mus-audio-close audio-fd))
	#f)))

(define (play-sines freqs-and-amps)
  "(play-sines freqs-and-amps) produces a tone given its spectrum: (play-sines '((440 .4) (660 .3)))"
  (let* ((audio-info (open-play-output 1 22050 #f 256))
	 (audio-fd (car audio-info))
	 (outchans (cadr audio-info))
	 (pframes (caddr audio-info)))
    (if (not (= audio-fd -1))
	(let ((len 22050)
	      (oscs (map make-oscil (map car freqs-and-amps)))
	      (amps (map cadr freqs-and-amps))
	      (data (make-sound-data outchans pframes)))
	  (do ((beg 0 (+ beg pframes)))
	      ((or (c-g?)                   ; C-g to stop in mid-stream
		   (> beg len)))
	    (do ((i 0 (1+ i)))
		((= i pframes))
	      (sound-data-set! data 0 i (apply + (map (lambda (o a) (* a (oscil o))) oscs amps))))
	    (mus-audio-write audio-fd data pframes))
	  (mus-audio-close audio-fd))
	#f)))

;(play-sines '((425 .05) (450 .01) (470 .01) (546 .02) (667 .01) (789 .034) (910 .032)))
