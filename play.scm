;;; most of this file is obsolete.
;;;
;;; playing-related examples previously scattered around at random
;;;
;;; play-sound func -- play current sound, calling (func data) on each buffer if func is passed
;;; play-often, play-until-c-g -- play sound n times or until C-g is typed
;;; play-region-forever -- play region over and over until C-g typed
;;; loop-between-marks -- play while looping continuously between two movable marks
;;; start-dac -- hold DAC open and play sounds via keyboard
;;; "vector synthesis"
;;; play-with-amps -- play channels with individually settable amps
;;; play-sine and play-sines -- produce tones direct to DAC

;;; see also play-syncd-marks and play-between-marks in marks.scm

(provide 'snd-play.scm)


(define* (samples->sound-data (beg 0) num snd chn obj pos (sd-chan 0))
  (vct->sound-data 
   (channel->vct beg num snd chn pos) 
   (or obj (make-sound-data 1 (or num (frames snd chn))))
   sd-chan))


(define* (open-play-output out-chans out-srate out-format out-bufsize)
  ;; returns (list audio-fd chans frames)
  (let* ((outchans (or out-chans 1))
	 (cur-srate (or out-srate (and (not (null? (sounds))) (srate)) 22050))
	 (pframes (or out-bufsize 256))
	 (frm (or out-format (if (little-endian?) mus-lshort mus-bshort)))
	 (outbytes (* pframes 2))     ; 2 here since we'll first try to send short (16-bit) data to the DAC
	 (audio-fd 
	  ;; ALSA throws an error where the rest of the audio cases simply report failure
	  ;;   so we turn off the "error" printout, catch the error itself, and toss it
	  (let ((no-error (null? (hook-functions mus-error-hook))))
	    (if no-error
		(hook-push mus-error-hook (lambda (hook) (set! (hook 'result) #t))))
	    (let ((val (catch #t
			      (lambda ()
				(mus-audio-open-output 0 cur-srate outchans frm outbytes))
			      (lambda args -1)))) ; -1 returned in case of error
	      (if no-error 
		  (set! (hook-functions mus-error-hook) ()))
	      val))))
    (if (not (= audio-fd -1))
	(set! (dac-size) outbytes))
    (list audio-fd outchans pframes)))


(define* (play-sound func)
  "(play-sound func) plays the currently selected sound, calling func on each data buffer, if func exists"
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
		  ((> beg len))
		(if (and (> outchans 1) (> filechans 1))
		    (do ((k 0 (+ 1 k)))
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
   (let ((len (length data)))
     (do ((i 0 (+ i 1)))
	 ((= i len))
       (sound-data-set! data 0 i (* 2.0 (sound-data-ref data 0 i)))))))
|#

;;; this could also be done with a function argument to the play function -- get a
;;;   sampler for the sound, call it on each invocation of the argument function etc


;;; -------- play sound n times -- (play-often 3) for example.

(define (play-often n) 
  "(play-often n) plays the selected sound 'n' times (interruptible via C-g)"
  (let ((plays (- n 1)))
    (define (play-once reason)
      (if (and (> plays 0)
	       (= reason 0))
	  (begin
	    (set! plays (- plays 1))
	    (play (selected-sound) :start 0 :stop play-once))))
    (play (selected-sound) :start 0 :stop play-once)))

;(bind-key #\p 0 (lambda (n) "play often" (play-often (max 1 n))))


;;; -------- play sound until c-g

(define (play-until-c-g)
  "(play-until-c-g) plays the selected sound until you interrupt it via C-g"
  (define (play-once reason)
    (if (= reason 0)
	(play (selected-sound) :start 0 :stop play-once)))
  (play (selected-sound) :start 0 :stop play-once))


;;; -------- play region over and over until C-g typed

(define (play-region-forever reg1)
  "(play-region-forever reg) plays region 'reg' until you interrupt it via C-g"

  (let ((reg (if (integer? reg1) (integer->region reg1) reg1)))

    (define (play-region-again reason)
      (if (= reason 0) ; 0=play completed normally
	  (play reg :wait #f :stop play-region-again)))

    (play reg :wait #f :stop play-region-again)))

;(bind-key #\p 0 (lambda (n) "play region forever" (play-region-forever ((regions) (max 0 n)))))


;;; -------- play while looping continuously between two movable marks

(define (loop-between-marks m1 m2 bufsize)
  "(loop-between-marks mark1 mark2 buffersize) plays while looping between two marks.  x typed in the graph, or C-g in the listener exits the loop."
  (let* ((pos1 (mark-sample m1))
	 (pos2 (mark-sample m2))
	 (beg (min pos1 pos2))
	 (end (max pos1 pos2))
	 (all-data (samples->sound-data)) ; for simplicity, just grab all the data
	 (audio-data (make-sound-data 1 bufsize))
	 (bytes (* bufsize 2)) ; mus-audio-write handles the translation to short (and takes frames, not bytes as 3rd arg)
	 (audio-fd (mus-audio-open-output 0 (srate) 1 mus-lshort bytes))
	 (stop-looping #f))
    (if (not (= audio-fd -1))
	(begin
	  (bind-key #\space 0 (lambda () (set! stop-looping #t))) ; type space in the graph to stop this loop
	  (do ()
	      (stop-looping
	       (mus-audio-close audio-fd)
	       (unbind-key #\space 0))
	    (do ((i 0 (+ i 1)))
		((= i bufsize) 
		 (begin 
		   (set! i 0) 
		   (mus-audio-write audio-fd audio-data bufsize)))
	      (sound-data-set! audio-data 0 i (sound-data-ref all-data 0 beg))
	      (set! beg (+ 1 beg))
	      (if (= beg end)
		  (begin
		    (set! pos1 (mark-sample m1)) ; get current mark positions (can change while looping)
		    (set! pos2 (mark-sample m2))
		    (set! beg (min pos1 pos2))
		    (set! end (max pos1 pos2))))))))))

;;; m1 and m2 are marks
;;; (loop-between-marks (caaar (marks)) (cadaar (marks)) 512)



;;; -------- hold DAC open and play sounds via keyboard

(define* (start-dac (sr 44100) (chans 1))
  "(start-dac (srate 44100) (chans 1)) starts the DAC running continuously in the background"
  (play #f :srate sr :channels chans))

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
	       (srate (srate (car files)))
	       (chans (apply max (map channels files)))
	       (data (make-sound-data chans bufsize))
	       (readers (map make-file->frame files))
	       (locs (make-vector files-len 0))
	       (pframes (make-vector files-len 0))
	       (current-file 0)
	       (reading #t)
	       (out-port (mus-audio-open-output 0 srate chans mus-lshort (* bufsize 2))))
	  (if (< out-port 0)
	      (format #t "can't open audio port! ~A" out-port)
	      (begin
		(do ((i 0 (+ i 1)))
		    ((= i files-len))
		  (set! (pframes i) (frames (files i))))
		(catch #t
		       (lambda ()
			 (while reading
				(let ((next-file (driver files-len bufsize)))
				  (if (not (= next-file current-file))
				      (let* ((ramp-down 1.0)
					     (ramp (/ 1.0 bufsize))
					     (current (readers current-file))
					     (current-loc (locs current-file))
					     (next (readers next-file))
					     (next-loc (locs next-file))
					     (up (make-frame chans))
					     (down (make-frame chans)))
					(do ((i 0 (+ i 1)))
					    ((= i bufsize))
					  (file->frame next (+ next-loc i) up)
					  (file->frame current (+ current-loc i) down)
					  (do ((j 0 (+ 1 j)))
					      ((= j chans))
					    (sound-data-set! data j i 
							     (+ (* ramp-down (frame-ref down j))
								(* (- 1.0 ramp-down) (frame-ref up j)))))
					  (set! ramp-down (- ramp-down ramp)))
					(if read-even-when-not-playing
					    (do ((i 0 (+ i 1)))
						((= i files-len))
					      (set! (locs i) (+ (locs i) bufsize)))
					    (begin
					      (set! (locs current-file) (+ (locs current-file) bufsize))
					      (set! (locs next-file) (+ (locs next-file) bufsize))))
					(set! current-file next-file))
				      (let* ((current (readers current-file))
					     (current-loc (locs current-file))
					     (on (make-frame chans)))
					(do ((i 0 (+ i 1)))
					    ((= i bufsize))
					  (file->frame current (+ current-loc i) on)
					  (do ((k 0 (+ 1 k)))
					      ((= k chans))
					    (sound-data-set! data k i (frame-ref on k))))
					(if read-even-when-not-playing
					    (do ((i 0 (+ i 1)))
						((= i files-len))
					      (set! (locs i) (+ (locs i) bufsize)))
					    (set! (locs current-file) (+ (locs current-file) bufsize)))))
				  (mus-audio-write out-port data bufsize)
				  (set! reading (letrec ((any-data-left 
							  (lambda (f)
							    (if (= f files-len)
								#f
								(or (< (locs f) (pframes f))
								    (any-data-left (+ 1 f)))))))
						  (any-data-left 0))))))
		       (lambda args (begin (snd-print (format #f "error ~A" args)) (car args))))
		(mus-audio-close out-port)))))))

#|
(vector-synthesis (let ((ctr 0) (file 0)) 
		    (lambda (files bufsize)
		      (if (> ctr 4)
			  (begin
			    (set! file (+ 1 file))
			    (set! ctr 0)
			    (if (>= file files)
				(set! file 0)))
			  (set! ctr (+ ctr 1)))
		      file))
		  (list "oboe.snd" "pistol.snd") #t)
|#


;;; play-with-amps -- play channels with individually settable amps

(define play-with-amps
  (lambda (sound . amps)
    "(play-with-amps snd :rest amps) plays snd with each channel scaled by the corresponding 
amp: (play-with-amps 0 1.0 0.5) plays channel 2 of stereo sound at half amplitude"
    (let ((chans (channels sound)))
      (do ((chan 0 (+ 1 chan)))
          ((= chan chans))
        (let ((player (make-player sound chan)))
          (set! (amp-control player) (amps chan))
          (add-player player)))
      (start-playing chans (srate sound)))))


;;; play-sine and play-sines

(define (play-sine freq amp)
  "(play-sine freq amp) plays a 1 second sinewave at freq and amp"
  (let ((len 44100)
	(osc (make-oscil freq)))
    (play (lambda ()
	    (set! len (- len 1))
	    (if (<= len 0)
		#f
		(* amp (oscil osc)))))))


(define (play-sines freqs-and-amps)
  "(play-sines freqs-and-amps) produces a tone given its spectrum: (play-sines '((440 .4) (660 .3)))"
  (let* ((len 44100)
	 (num-oscs (length freqs-and-amps))
	 (oscs (make-vector num-oscs))
	 (amps (make-vector num-oscs)))
    (do ((i 0 (+ i 1)))
	((= i num-oscs))
      (set! (oscs i) (make-oscil (car (freqs-and-amps i))))
      (set! (amps i) (cadr (freqs-and-amps i))))
    (play (lambda ()
	    (set! len (- len 1))
	    (if (<= len 0)
		#f
		(let ((sum 0.0))
		  (do ((i 0 (+ i 1)))
		      ((= i num-oscs))
		    (set! sum (+ sum (* (amps i) (oscil (oscs i))))))
		  sum))))))

;(play-sines '((425 .05) (450 .01) (470 .01) (546 .02) (667 .01) (789 .034) (910 .032)))
