;;; examples of Guile extensions to Snd
;;;
;;;        contents
;;;
;;; documentation examples made harder to break
;;; 'info' from extsnd.html using format
;;; correlation
;;; XEmacs-like "Buffers" menu
;;; set fft-size based on current time domain window size
;;; superimpose spectra of sycn'd sounds
;;; example of abort?
;;; play sound n times
;;; play region over and over until C-g typed
;;; play samples created on-the-fly
;;; play while looping continuously between two movable marks
;;; play-section, play-selection
;;; make a system call
;;; translate mpeg input to 16-bit linear and read into Snd
;;; make dot size dependent on number of samples being displayed
;;; auto-save
;;; move window left edge to mark upon 'm' key
;;; flash selected data red and green
;;; show bomb icon
;;; use loop info (if any) to set marks at loop points
;;; delete selected portion and smooth the splice
;;; eval over selection or between marks replacing current samples, mapped to "x" or "m" key using prompt-in-minibuffer
;;; mix with result at original peak amp
;;; mapping extensions (map arbitrary single-channel function over various channel collections)
;;;     do-chans, do-all-chans, do-sound-chans
;;;     every-sample?
;;;     sort-samples
;;;     swap-channels
;;; envelope-interp, window-envelope, map-envelopes, multiply-envelopes
;;; mix mono sound into stereo sound panning according to env, also simple sound placement
;;; fft-edit -- FFT based editing
;;; comb-filter, notch-filter, formant-filter
;;; echo (delays)
;;; ring-modulation, am
;;; src-related sound effects (src, rand-interp, etc)
;;; compand (array-interp)
;;; shift pitch keeping duration constant (src+granulate)
;;; tempo change via envelope (granulate)
;;; cross-synthesis (using a formant bank)
;;; voiced->unvoiced (formants)
;;; convolution (convolve)
;;; reverb (all-pass etc)
;;; scissor-tailed flycatcher (waveshaping)
;;; fm-violin (FM and various other generators, #&key args)
;;; digital zipper "crossfade" (file->sample)
;;; FOF voice synthesis (wave-train, #&optional args)
;;; phase vocoder
;;; mix with envelope
;;; time varying FIR filter
;;; map-sound-files, match-sound-files


(use-modules (ice-9 debug)) ;comment this out in guile versions before 1.3.2
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))

(read-set! keywords 'prefix) ;this so we can use ":" as the keyword prefix

;;; -------- snd.html examples made harder to break --------
;;;
;;; this mainly involves keeping track of the current sound/channel

(define region-rms
  (lambda (n)
    "(region-rms n) -> rms of region n's data (chan 0)"
    (let* ((data (region-samples 0 0 n))
	   (len (vector-length data))
	   (sum 0.0))
      (do ((i 0 (1+ i))) ((= i len) (sqrt (/ sum len)))
	(set! sum (+ sum (* (vector-ref data i) (vector-ref data i))))))))

(define window-samples
  (lambda (snd chn)
    "(window-samples snd chn) -> sample in snd channel chn in current graph window"
    (let ((wl (left-sample snd chn))
	  (wr (right-sample snd chn)))
      (samples wl (+ 1 (- wr wl)) snd chn))))

(define display-energy
  ;; in this version, the y-zoom-slider controls the graph amp
  (lambda (snd chn y0 y1)
    "(vct-display-energy snd chn y0 y1) is a graph-hook function to display the time domain data\n\
    as energy (squared); y0 and y1 set the resultant graph y axis bounds; vct-display-energy is faster.\n\
    use (add-hook! graph-hook vct-display-energy)"
    (let* ((ls (left-sample))
           (rs (right-sample))
           (data (samples->vct ls (+ 1 (- rs ls)) snd chn))
           (len (vct-length data))
           (sr (srate snd)))
      (vct-multiply! data data)
      (let ((peak (vct-peak data)))
	(graph data "energy" (/ ls sr) (/ rs sr) 0.0 (* peak (y-zoom-slider snd chn)) snd chn)))))

;(add-hook! graph-hook display-energy)

(define window-rms
  (lambda ()
    "(window-rms n) -> rms of data in currently selected graph window"
    (let* ((ls (left-sample))
	   (rs (right-sample))
	   (data (samples->vct ls (+ 1 (- rs ls))))
	   (len (vct-length data)))
      (sqrt (/ (dot-product data data) len)))))


(define unsaved-edits?
  (lambda (ind)
    "(unsaved-edits? ind) -> #t if there are unsaved edits in sound ind"
    (letrec ((unsaved-edits-in-chan? 
	      (lambda (chan)
		(if (>= chan (channels ind))
		    #f
		    (let ((eds (edits ind chan)))
		      (if (> (vector-ref eds 0) 0)
			  (not (yes-or-no-p ;that is, "yes" => exit
				(format #f "~A has ~D unsaved edit~P in channel ~D, exit anyway? " 
					(short-file-name ind) 
					(vector-ref eds 0)
					(vector-ref eds 0)
					chan)))
			  (unsaved-edits-in-chan? (1+ chan))))))))
      (if (< ind (max-sounds))
	  (or (and (ok? ind) 
		   (unsaved-edits-in-chan? 0))
	      (unsaved-edits? (+ ind 1)))
	  #f))))

;(add-hook! exit-hook (lambda () (report-in-minibuffer "") (unsaved-edits? 0)))


(define no-startup-file?
  (lambda (ind file)
    (if (= ind (max-sounds))
	(begin
	  (write (string-append "can't open " file) (current-error-port))
	  (newline (current-error-port))
	  #t)
	(if (ok? ind)
	    #f
	    (no-startup-file? (+ ind 1) file)))))

;(add-hook! start-hook (lambda (file) (if (> (string-length file) 0) (no-startup-file? 0 file) #f)))


(define fft-peak
  (lambda (snd chn scale)
    (if (and (ffting) (= (fft-style) normal-fft))
	(report-in-minibuffer 
	 (number->string (/ (* 2.0 (vct-peak (transform-samples->vct snd chn))) (fft-size)))
	 snd)
      #f)))

;(add-hook! fft-hook fft-peak)


;;; -------- 'info' from extsnd.html using format --------

(define finfo
  (lambda (file)
    "(finfo file) -> description (as a string) of file"
    (format #f "~A: chans: ~D, srate: ~D, ~A, ~A, len: ~1,3F"
	    file
	    (mus-sound-chans file)
	    (mus-sound-srate file)
	    (mus-header-type-name (mus-sound-header-type file))
	    (mus-data-format-name (mus-sound-data-format file))
	    (/ (mus-sound-samples file)
	       (* (mus-sound-chans file) (mus-sound-srate file))))))


;;; -------- Correlation --------
;;;
;;; correlation of channels in a stereo sound (uses window-samples given above)

(define correlate
 (lambda (snd chn y0 y1)
   (if (and (= (channels snd) 2)
	    (> (frames snd 0) 1)
	    (> (frames snd 1) 1))
       (let* ((ls (left-sample snd 0))
	      (rs (right-sample snd 0))
	      (ilen (+ 1 (- rs ls)))
	      (pow2 (ceiling (/ (log ilen) (log 2))))
	      (fftlen (inexact->exact (expt 2 pow2)))
	      (fftlen2 (/ fftlen 2))
	      (fftscale (/ 1.0 fftlen))
	      (rl1 (samples->vct ls fftlen snd 0))
	      (rl2 (samples->vct ls fftlen snd 1))
	      (im1 (make-vct fftlen))
	      (im2 (make-vct fftlen)))
	 (fft rl1 im1 1)
	 (fft rl2 im2 1)
	 (let* ((tmprl (vct-copy rl1))
		(tmpim (vct-copy im1))
		(data3 (make-vct fftlen2)))
	   (vct-multiply! tmprl rl2)     ; (* tempr1 tempr2)
	   (vct-multiply! tmpim im2)     ; (* tempi1 tempi2)
	   (vct-multiply! im2 rl1)       ; (* tempr1 tempi2)
	   (vct-multiply! rl2 im1)       ; (* tempr2 tempi1)
	   (vct-add! tmprl tmpim)        ; add the first two
	   (vct-subtract! im2 rl2)       ; subtract the 4th from the 3rd
	   (fft tmprl im2 -1)
	   (vct-add! data3 tmprl)        ; copy into data3 (which is half the size of tmprl)
	   (vct-scale! data3 fftscale)   ; scale by fftscale
	   (graph data3 "lag time" 0 fftlen2)))
       (report-in-minibuffer "vct-correlate wants stereo input"))))

;(add-hook! graph-hook correlate)

;;; The inner let body could also be:
;;;
;;;	   (graph
;;;	    (vct-scale! 
;;;	     (vct-add! data3
;;;		       (fft (vct-add! (vct-multiply! tmprl rl2) (vct-multiply! tmpim im2))
;;;			    (vct-subtract! (vct-multiply! im2 rl1) (vct-multiply! rl2 im1))
;;;			    -1))
;;;	     fftscale) "lag time" 0 fftlen2)))



;;; -------- Buffers Menu --------
;;; patterned after the XEmacs Buffers menu

;(define buffer-menu (add-to-main-menu "Buffers"))

(define open-buffer
  (lambda (filename)
    (add-to-menu buffer-menu 
		 filename 
		 (lambda () (select-sound (find-sound filename))))
    #f))

(define close-buffer 
  (lambda (snd)
    (remove-from-menu buffer-menu (file-name snd))
    #f))

;;; here we're adding this menu handling code to whatever is already happening at open/close-hook time

;(add-hook! open-hook open-buffer)
;(add-hook! close-hook close-buffer)


;;; -------- set fft-size based on current time domain window size
;;;
;;; also zoom spectrum based on y-axis zoom slider

(add-hook! graph-hook 
	   (lambda (snd chn y0 y1)
	     (if (and (ffting) (= (fft-style) normal-fft))
		 (begin
		   (set-fft-size
		    (expt 2 (ceiling 
			     (/ (log (- (right-sample) (left-sample))) 
				(log 2.0)))))
		   (set-spectro-cutoff (y-zoom-slider snd chn))))))

;;; this version only messes with the fft settings if the time domain is not displayed
;;;   it also sets the spectrum display start point based on the x position slider
;;;   this can be confusing if fft normalization is on (the default)

(add-hook! graph-hook 
	   (lambda (snd chn y0 y1)
	     (if (and (ffting)
		      (not (waving))
		      (= (fft-style) normal-fft))
		 (begin
		   (set-fft-size
		    (expt 2 (ceiling 
			     (/ (log (- (right-sample) (left-sample))) 
				(log 2.0)))))
		   (set-spectro-start (x-position-slider snd chn))
		   (set-spectro-cutoff (y-zoom-slider snd chn))))))



;;; -------- superimpose spectra of sycn'd sounds

(define make-one-fft
  (lambda (samp size snd chn)
    (let* ((fdr (samples->vct samp size snd chn))
	   (fdi (make-vct size))
	   (spectr (make-vct (/ size 2))))
      (vct-add! spectr (spectrum fdr fdi #f size 2)))))

(define min-at-sync 
  (lambda (snd ind)
    (if (>= ind (max-sounds)) 
	#t
	(if (or (= snd ind)
		(not (ok? ind))
		(and (= (syncing snd) (syncing ind))
		     (> ind snd)))
	    (min-at-sync snd (1+ ind))
	    #f))))

(define collect-ffts 
  (lambda (samp size snd chn ffts ind)
    (if (>= ind (max-sounds)) 
	ffts
	(if (and (ok? ind)
		 (= (syncing snd) (syncing ind))
		 (> (chans ind) chn))
	    (collect-ffts samp size snd chn (append ffts (list (make-one-fft samp size ind chn))) (1+ ind))
	    (collect-ffts samp size snd chn ffts (1+ ind))))))
	
(define superimpose-ffts
  (lambda (snd chn y0 y1)
    (if (and (> (syncing snd) 0)
	     (min-at-sync snd 0))
	;; we are syncing, and we are the top sound in this sync group
	(let* ((ls (left-sample snd chn))
	       (rs (right-sample snd chn))
	       (pow2 (floor (/ (log (- rs ls)) (log 2))))
	       (fftlen (inexact->exact (expt 2 pow2))))
	  (graph (collect-ffts ls fftlen snd chn '() snd)
		 "spectra" 0.0 0.5 #f #f snd chn))
	(set-graphing #f snd chn))
    #f))

;(add-hook! graph-hook superimpose-ffts)


;;; -------- abort? example (Anders Vinjar)

(define (locate-zero limit)
  (let* ((start (cursor))
	 (sf (make-sample-reader start)))
    (do ((n start (1+ n))
	 (val0 (abs (next-sample sf)) val1)
	 (val1 (abs (next-sample sf)) (abs (next-sample sf))))
	((or (sample-reader-at-end? sf)
	     (abort?)
	     (< (+ val0 val1) limit))
	 (begin
	   (free-sample-reader sf)
	   (set-cursor n)
	   n)))))


;;; -------- play sound n times -- (pl 3) for example.

(define plays 0)

(define pl1
  (lambda (snd)
    (if (= plays 0)
	(remove-hook! stop-playing-hook pl1)
      (begin
	(set! plays (- plays 1))
	(play 0 snd)))))

(define (pl n) 
  (set! plays (- n 1))
  (add-hook! stop-playing-hook pl1)
  (play))

(bind-key (char->integer #\p) 0 (lambda () (pl (max 1 (prefix-arg)))) #t)



;;; -------- play region over and over until C-g typed

(define play-region-again
  (lambda (reg)
    (if (abort?)
	(remove-hook! stop-playing-region-hook play-region-again)
	(play-region reg))))

(define play-region-forever 
  (lambda (reg) 
    (reset-hook! stop-playing-region-hook)
    (add-hook! stop-playing-region-hook play-region-again)
    (play-region reg)))

(bind-key (char->integer #\p) 0 (lambda () (play-region-forever (max 0 (prefix-arg)))) #t)



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
	(or (abort?) (>= beg len))))))

;(play-fun (amprt (frames)) 256)


;;; -------- play while looping continuously between two movable marks

(define loopit
  (lambda (m1 m2 bufsize)
    (let* ((pos1 (mark-sample m1))
	   (pos2 (mark-sample m2))
	   (beg (min pos1 pos2))
	   (end (max pos1 pos2))
	   (all-data (samples->sound-data)) ; for simplicity, just grab all the data
	   (audio-data (make-sound-data 1 bufsize))
	   (bytes (* bufsize 2))
	   (audio-fd (mus-audio-open-output mus-audio-default 22050 1 mus-lshort bytes)))
      (if (not (= audio-fd -1))
	  (do ()
	      ((abort?) 
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

;;; m1 and m2 are mark numbers
;;; (loopit 0 1 512)


;;; -------- play portion of sound, play current selection (as opposed to C-x p which plays region 0)

(define play-section
  (lambda* (beg end #&optional (snd 0))
    (if (and (ok? snd) (> end beg))
	(let* ((chans (channels snd))
	       (edited (make-vector chans)))
	  (do ((chn 0 (1+ chn)))
	      ((= chn chans))
	    (let ((len (frames snd chn)))
	      (if (< end len) 
		  (begin
		    (vector-set! edited chn #t)
		    (set-squelch-update #t snd chn)
		    (delete-samples end (- len end) snd chn))
		(vector-set! edited chn #f))))
	  (play-and-wait beg snd)
	  (do ((chn 0 (1+ chn)))
	      ((= chn chans))
	    (if (vector-ref edited chn) 
		(begin
		  (undo 1 snd chn)
		  (set-squelch-update #f snd chn))))))))

(define play-selection
  (lambda ()
    ;; this is kinda brute force
    (save-selection "tmp.snd" mus-next mus-bshort (region-srate 0) "")
    (play-and-wait "tmp.snd")
    (delete-file "tmp.snd")))



;;; -------- make a system call from the listener
;;;
;;;   (shell "df") for example -- there's probably a more elegant way to do this is in Scheme
;;; or to play a sound whenever a file is closed:
;;;   (set! close-hook "(shell \"sndplay wood16.wav\")")

(use-modules (ice-9 popen))  

(define shell 
  (lambda (cmd)
    (let* ((str "")
	   (fil (open-pipe cmd "r")))
      (do ((val (read-char fil) (read-char fil))) 
	  ((eof-object? val))
	(set! str (string-append str (string val))))
      (close-pipe fil)
      str)))


;;; -------- translate mpeg input to 16-bit linear and read into Snd
;;;
;;; mpg123 with the -s switch sends the 16-bit (mono or stereo) representation of
;;;   an mpeg file to stdout.  There's also apparently a switch to write 'wave' output.

(define mpg
  (lambda (mpgfile rawfile chans)
    ;; there's probably a way to get the channels automatically
    (system (format #f "mpg123 -s ~A > ~A" mpgfile rawfile))
    (set-raw-srate 44100)
    (set-raw-chans chans)
    (if (little-endian?) (set-raw-format mus-lshort) (set-raw-format mus-bshort))
    (set-use-raw-defaults #t)
    (open-sound rawfile)))


;;; -------- make dot size dependent on number of samples being displayed
;;; 
;;; this could be extended to set graph-style to graph-lines if many samples are displayed, etc

(define auto-dot
  (lambda (snd chn y0 y1)
    (let ((dots (- (right-sample snd chn)
		   (left-sample snd chn))))
      (if (> dots 100) 
	  (set-dot-size 1)
	(if (> dots 50)
	    (set-dot-size 2)
	  (if (> dots 25)
	      (set-dot-size 3)
	    (set-dot-size 5)))))))
    
;(add-hook! graph-hook auto-dot)


;;; -- current time 
;(strftime "%H:%M" (localtime (current-time)))
;(add-hook! output-comment-hook (lambda (str) (string-append "written " (strftime "%a %d-%b-%Y %H:%M %Z" (localtime (current-time))))))

;;; -------- auto-save 
;;;
;;; we use the "in" function to glance at the state of edits every "interval" seconds
;;; a simple example of "in" is:
;;;    (in 5000 "(report-in-minibuffer \"boo!\")")
;;; which will be print "boo" 5 seconds after it is evaluated.
;;;
;;; in this somewhat minimal implementation, the user calls (auto-save secs)
;;; for example, (auto-save 100)
;;; which causes the function auto-save to run every "secs" seconds
;;; If a sound is found with a channel with more than 10 unsaved edits,
;;;  it is saved on the /tmp directory with the name #<filename># -- that
;;;  is, test.snd's current state is saved as /tmp/#test.snd#.
;;;
;;; useful extensions: delete the temp file when associated file is closed (close-hook addition)
;;;                    check that auto-saved version is actually out-of-date
;;;                    check that there's enough disk space
;;;                    check at open for more recent auto-save version

(define auto-save
  (lambda (interval)
    (let ((lim (max-sounds)))
      (do ((i 0 (1+ i)))
	  ((= i lim))
	(if (ok? i)
	    (let ((max-edits 0)
		  (chans (channels i)))
	      (do ((chn 0 (1+ chn)))
		  ((= chn chans))
		(let ((eds (vector-ref (edits i chn) 0)))
		  (if (> eds max-edits) (set! max-edits eds))))
	      (if (> max-edits 10)
		  (begin
		   (report-in-minibuffer "auto-saving..." i)
		   (save-sound-as (string-append "/tmp/#" (short-file-name i) "#") i))))))
      (in (* 1000 interval) (string-append "(auto-save " (number->string interval) ")")))))



;;; -------- move window left edge to mark upon 'm'
;;;
;;; in large sounds, it can be pain to get the left edge of the window
;;; aligned with a specific spot in the sound.  In this code, we assume
;;; the desired left edge has a mark, and the 'm' key (without control)
;;; will move the window left edge to that mark.

(define first-mark-in-window-at-left
  (lambda ()
    (let* ((keysnd (or (selected-sound) 0))
	   (keychn (or (selected-channel keysnd) 0))
	   (current-left-sample (left-sample keysnd keychn))
	   (num-marks (marks keysnd keychn)))
      (if (= num-marks 0)
	  (report-in-minibuffer "no marks!")
	(if (= num-marks 1)
	    (begin
	      (set-left-sample (mark-sample 0) keysnd keychn)
	      cursor-update-display)
          (do ((i 0 (1+ i)))
	      ((or (= i num-marks) 
		   (<= current-left-sample (mark-sample i)))
	       (if (or (= i num-marks) 
		       (> (mark-sample i) (right-sample keysnd keychn)))
		   (report-in-minibuffer "no mark in window")
		 (begin
		   (set-left-sample (mark-sample i) keysnd keychn)
		   cursor-update-display)))))))))

(bind-key (char->integer #\m) 0 (lambda () (first-mark-in-window-at-left)))



;;; -------- flash selected data red and green
;;;

(define red (make-color 1 0 0))
(define green (make-color 0 1 0))
(define data-red? #t)
(set-selected-data-color red)

(define flash-selected-data
  (lambda (interval)
    (if (selected-sound)
	(begin
	  (set-selected-data-color (if data-red? green red))
	  (set! data-red? (not data-red?))
	  (in interval (lambda () (flash-selected-data interval)))))))


;;; -------- show bomb icon

(define show-bomb 
  (lambda (n speed) 
    (if (> n 0) 
	(begin 
	  (bomb) 
	  (in speed (lambda () (show-bomb (- n 1) speed))))
	(bomb 0 #f))))

; (show-bomb 20 200)


;;; --------  use loop info (if any) to set marks at loop points

(define mark-loops
  (lambda ()
    (let ((loops (mus-sound-loop-info (file-name))))
      (if (not (null? loops))
	  (begin
	    (if (not (and (= (car loops) 0) (= (cadr loops) 0)))
		(begin
		  (add-mark (car loops))
		  (add-mark (cadr loops))
		  (if (not (and (= (caddr loops) 0) (= (cadddr loops) 0)))
		      (begin
			(add-mark (caddr loops))
			(add-mark (cadddr loops)))))))
	  (snd-print "no loop info")))))
		
	    

;;; -------- delete selected portion and smooth the splice

(define delete-selection-and-smooth
  (lambda ()
    (if (selection-length)
	(let ((beg (selection-beg))
	      (len (selection-length)))
	  (do ((i 0 (1+ i)))
	      ((= i (max-sounds)) #f)
	    (if (ok? i)
		(do ((j 0 (1+ j)))
		    ((= j (channels i)) #f)
		  (if (selection-member i j)
		      (let ((smooth-beg (max 0 (- beg 16))))
			(delete-samples beg len i j)
			(smooth smooth-beg 32 i j))))))))))


;;; -------- eval over selection, replacing current samples, mapped to "x" key using prompt-in-minibuffer
;;;
;;; when the user types x (without modifiers) and there is a current selection,
;;;   the minibuffer prompts "selection eval:".  Eventually the user responds,
;;;   hopefully with a function of one argument, the current selection sample
;;;   the value returned by the function becomes the new selection value.

(bind-key (char->integer #\x) 0 
	  (lambda ()
	    (if (selection-length)
		(prompt-in-minibuffer "selection eval:" eval-over-selection)
		(report-in-minibuffer "no selection"))))

(define eval-over-selection 
  (lambda (func snd)
    (if (procedure? func) 
	(let* ((beg (selection-beg))
	       (len (selection-length)))
	  (do ((i 0 (1+ i))) ;here we're treating each sound in the current selection
	      ((= i (max-sounds)) #f)
	    (if (ok? i)
		(do ((j 0 (1+ j))) ;here we're treating each channel of the current sound in the current selection
		    ((= j (channels i)) #f)
		  (if (selection-member i j)
		      (let ((new-data (make-vct len))
			    (old-data (samples->vct beg len i j)))
			(do ((k 0 (1+ k))) ;here we're applying our function to each sample in the currently selected portion
			    ((= k len) (set-samples beg len new-data i j))
			  (vct-set! new-data k (func (vct-ref old-data k)))))))))))))

;;; the same idea can be used to apply a function between two marks:

(bind-key (char->integer #\m) 0 
	  (lambda ()
	    (if (> (marks) 1)
		(prompt-in-minibuffer "mark eval:" eval-between-marks)
		(report-in-minibuffer "no marks"))))

(define eval-between-marks
  (lambda (func snd)
    (if (procedure? func)
	(let* ((beg (mark-sample 0))
	       (len (- (mark-sample 1) beg))
	       (new-data (make-vct len))
	       (old-data (samples->vct beg len snd)))
	  (do ((k 0 (1+ k)))
	      ((= k len) (set-samples beg len new-data snd))
	    (vct-set! new-data k (func (vct-ref old-data k))))))))



;;; -------- mix with result at original peak amp

(define normalized-mix 
  (lambda (filename beg in_chan snd chn)
    ;; like mix but returns result with same peak amp as before (return scaler)
    (let ((original-max-amp (maxamp snd chn)))
      (mix filename beg in_chan snd chn)
      (let ((new-max-amp (maxamp snd chn)))
	(if (not (= original-max-amp new-max-amp))
	    (let ((scaler (/ original-max-amp new-max-amp))
		  (old-sync (syncing snd)))
	      (set-syncing 0 snd)
	      (scale-by scaler snd chn)
	      (set-syncing old-sync snd)
	      scaler)
	    1.0)))))


;;; -------- mapping extensions (map arbitrary single-channel function over various channel collections)
;;;

(define do-all-chans
  (lambda (proc args origin)
    (do ((i 0 (1+ i)))
	((= i (max-sounds)) #f)
      (if (ok? i)
	  (do ((j 0 (1+ j)))
	      ((= j (channels i)) #f)
	    (map-chan (apply proc args) #f #f origin i j))))))

(define update-graphs
  (lambda ()
    (do ((i 0 (1+ i)))
	((= i (max-sounds)) #f)
      (if (ok? i)
	  (do ((j 0 (1+ j)))
	      ((= j (channels i)) #f)
	    (update-graph i j))))))

(define do-chans
  (lambda (proc args origin)
    (if (> (syncing) 0)
	(do ((snc (syncing))
	     (i 0 (1+ i)))
	    ((= i (max-sounds)) #f)
	  (if (and (ok? i) (= (syncing i) snc))
	      (do ((j 0 (1+ j)))
		  ((= j (channels i)) #f)
		(map-chan (apply proc args) #f #f origin i j))))
	(map-chan (apply proc args)))))

(define do-sound-chans
  (lambda (proc args origin)
    (let ((ind (selected-sound)))
      (do ((j 0 (1+ j)))
	  ((= j (channels ind)) #f)
      (map-chan (apply proc args) #f #f origin ind j)))))


(define every-sample?
  (lambda (proc)
    (let ((baddy (scan-chan (lambda (y) (if y (not (proc y)) #f)))))
      (if baddy (set-cursor (cadr baddy)))
      (not baddy))))

(define sort-samples
  (lambda (nbins)
    (let ((bins (make-vector nbins 0)))
      (lambda (y)
	(if y
	    (let ((bin (inexact->exact (floor (* (abs y) nbins)))))
	      (vector-set! bins bin (+ (vector-ref bins bin) 1))
	      #f)
	    bins)))))

(define swap-channels
  (lambda ()
    (if (= (channels) 2)
	(map-across-sound-chans
	 (lambda (data chans)
	   (if data 
	       (let ((chan0-sample (vector-ref data 0)))
		 (vector-set! data 0 (vector-ref data 1))
		 (vector-set! data 1 chan0-sample)
		 data)
	       #f))
	 #f #f "swap-channels")
	(string-append (short-file-name) " is not stereo!"))))


;;; -------- envelope-interp (named envelope-interp in clm's env.lisp)
;;;
;;; (envelope-interp .3 '(0 0 .5 1 1 0) -> .6

(define envelope-interp                      ;env is list of x y breakpoint pairs, interpolate at x returning y
  (lambda args                          ;  (x env &optional (base 1.0)
    "(envelope-interp x env &optional (base 1.0)) -> value of env at x (base controls connecting segment type)"
    (let ((x (car args))
	  (env (cadr args))
	  (base (if (null? (cddr args)) #f (caddr args))))
      (cond ((null? env) 0.0)		;no data -- return 0.0
	    ((or (<= x (car env))	;we're sitting on x val (or if < we blew it)
		 (null? (cddr env)))	;or we're at the end of the list
	     (cadr env))		;so return current y value
	    ((> (caddr env) x)		;x <= next env x axis value
	     (if (or (= (cadr env) (cadddr env))
		     (and base (= base 0.0)))
		 (cadr env)		;y1=y0, so just return y0 (avoid endless calculations below)
		 (if (or (not base) (= base 1.0))
		     (+ (cadr env)	;y0+(x-x0)*(y1-y0)/(x1-x0)
			(* (- x (car env))
			   (/ (- (cadddr env) (cadr env))
			      (- (caddr env) (car env)))))
		     (+ (cadr env)
			(* (/ (- (cadddr env) (cadr env))
			      (- base 1.0))
			   (- (expt base (/ (- x (car env))
					    (- (caddr env) (car env))))
			      1.0))))))
	    (else (envelope-interp x (cddr env))))))) ;go on looking for x segment

;;; -------- window-envelope (a kinda brute-force translation from the CL version in env.lisp)
;;;
;;; (window-envelope 1.0 3.0 '(0.0 0.0 5.0 1.0)) -> '(1.0 0.2 3.0 0.6)

(define window-envelope 
  (lambda (beg end env)
    "(window-envelope beg end env) -> portion of env lying between x axis values beg and end"
    (let ((nenv '())
	  (lasty (if env (cadr env) 0.0))
	  (len (length env)))
      (call-with-current-continuation
       (lambda (return-early)               
	 (do ((i 0 (+ i 2)))
	     ((>= i len))
	   (let ((x (list-ref env i))
		 (y (list-ref env (+ i 1))))
	     (set! lasty y)
	     (if (null? nenv)
		 (if (>= x beg)
		     (begin
		       (set! nenv (append nenv (list beg (envelope-interp beg env))))
		       (if (not (= x beg))
			   (if (>= x end)
			       (return-early (append nenv (list end (envelope-interp end env))))
			       (set! nenv (append nenv (list x y)))))))
		 (if (<= x end)
		     (begin
		       (set! nenv (append nenv (list x y)))
		       (if (= x end)
			   (return-early nenv)))
		     (if (> x end)
			 (return-early (append nenv (list end (envelope-interp end env)))))))))
	 (append nenv (list end lasty)))))))

;;; map-envelopes like map-across-envelopes in env.lisp

(define map-envelopes 
  (lambda (op e1 e2)
    "(map-envelopes func env1 env2) maps func over the breakpoints in env1 and env2 returning a new envelope"
    (let ((xs '()))
      (letrec ((at0 
		(lambda (e)
		  (let ((diff (car e))
			(len (length e)))
		    (do ((i 0 (+ i 2)))
			((>= i len) e)
		      (let ((x (- (list-ref e i) diff)))
			(set! xs (cons x xs))
			(list-set! e i x))))))
	       (remove-duplicates
		(lambda (lst)
		  (letrec ((rem-dup
			    (lambda (lst nlst)
			      (cond ((null? lst) nlst)
				    ((member (car lst) nlst) (rem-dup (cdr lst) nlst))
				    (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
		    (rem-dup lst '())))))
	(let ((ee1 (at0 e1))
	      (ee2 (at0 e2))
	      (newe '()))
	  (set! xs (sort! (remove-duplicates xs) <))
	  (let ((len (length xs)))
	    (do ((i 0 (1+ i)))
		((= i len))
	      (let ((x (list-ref xs i)))
		(set! newe (append newe (list x (op (envelope-interp x ee1) (envelope-interp x ee2)))))))
	    newe))))))

(define multiply-envelopes
  (lambda (e1 e2)
    "(multiply-envelopes env1 env2) multiplies break-points of env1 and env2 returning a new envelope"
    (map-envelopes * e1 e2)))

; (multiply-envelopes '(0 0 2 .5) '(0 0 1 2 2 1)) -> '(0 0 1 0.5 2 0.5)


;;; -------- mix mono sound into stereo sound panning according to env

(define pan-mono-to-stereo
  ;; this function assumes we've currently syncd together one mono and one stereo sound,
  ;; then mixes the mono into the stereo following the panning envelope;
  ;; the latter is 0 when we want all the mono in chan 0, 1 for all in chan 1.
  ;; We'll assume for simplicity that the envelope goes from 0 to 1 along the x axis.
  (lambda (panning-envelope)
    (letrec ((find-nchannel-sound (lambda (ind chans) 
				      (if (< ind (max-sounds))
					  (if (and (ok? ind) (> (syncing ind) 0) (= (channels ind) chans)) 
					      ind 
					      (find-nchannel-sound (1+ ind) chans))
					  (begin
					    (report-in-minibuffer 
					     (string-append "can't find any syncd " 
							    (number->string chans) 
							    " channel sound!"))
					    #f)))))
      (let* ((mono-sound (find-nchannel-sound 0 1))
	     (stereo-sound (and mono-sound (find-nchannel-sound 0 2))))
	(if stereo-sound
	    (let ((samps (max (frames stereo-sound) (frames mono-sound)))
		  (current-samp 0)
		  (mono (if (> stereo-sound mono-sound) 0 2))
		  (stereo (if (> stereo-sound mono-sound) 1 0)))
	      (map-across-chans
	       (lambda (data chans)
		 (if data
		     (let ((samp (vector-ref data mono))
			   (y (envelope-interp (/ current-samp samps) panning-envelope)))
		       (set! current-samp (1+ current-samp))
		       (vector-set! data mono #f) ;don't edit the mono file
		       (vector-set! data stereo (+ (vector-ref data stereo) (* (- 1.0 y) samp)))
		       (vector-set! data (1+ stereo) (+ (vector-ref data (1+ stereo)) (* y samp)))
		       data)
		   #f)))))))))

;;; a similar function using the CLM module:
(define place
  (lambda (mono-snd stereo-snd deg)
    (let* ((len (frames mono-snd))
	   (msf (make-sample-reader 0 mono-snd 0))
	   (loc (make-locsig :degree deg :channels 2))
	   (out-data0 (make-vct len))
	   (out-data1 (make-vct len)))
      (vcts-do! out-data0 out-data1
		 (lambda (num i)
		   (frame->list (locsig loc i (next-sample msf)))))
      (free-sample-reader msf)
      (vct-add! out-data0 (samples->vct 0 len stereo-snd 0))
      (vct-add! out-data1 (samples->vct 0 len stereo-snd 1))
      (vct->samples 0 len out-data0 stereo-snd 0)
      (vct->samples 0 len out-data1 stereo-snd 1))))

;;; of course, in a simple situation like this, it's much faster to do it directly:
(define place1
  (lambda (mono-snd stereo-snd deg)
    (let* ((len (frames mono-snd))
	   (pos (/ deg 90.0))
	   (in-data0 (samples->vct 0 len mono-snd))
	   (in-data1 (samples->vct 0 len mono-snd))
	   (out-data0 (samples->vct 0 len stereo-snd 0))
	   (out-data1 (samples->vct 0 len stereo-snd 1)))
      (vct-scale! in-data0 pos)
      (vct-scale! in-data1 (- 1.0 pos))
      (vct-add! out-data0 in-data0)
      (vct-add! out-data1 in-data1)
      (vct->samples 0 len out-data0 stereo-snd 0)
      (vct->samples 0 len out-data1 stereo-snd 1))))


;;; -------- FFT-based editing
;;;

(define fft-edit
  ;; fft entire sound, remove all energy below 'bottom' Hz and all above 'top' Hz,
  ;; then unfft.  We're assuming we can fit the entire fft into memory (we could
  ;; easily reduce this by a factor of 2 by repacking the data in and out).
  (lambda (bottom top)
    "(fft-edit lo-bin hi-bin) removes all energy below lo-bin and above hi-bin"
    (let* ((sr (srate))
	   (len (frames))
	   (fsize (expt 2 (ceiling (/ (log len) (log 2.0)))))
	   (rdata (samples->vct 0 fsize))
	   (idata (make-vct fsize))
	   (lo (round (/ bottom (/ sr fsize))))
	   (hi (round (/ top (/ sr fsize)))))
      (fft rdata idata 1)
      (do ((i 0 (1+ i))
	   (j (- fsize 1) (1- j)))
	  ((= i lo))
	(vct-set! rdata i 0.0)
	(vct-set! rdata j 0.0)
	(vct-set! idata i 0.0)
	(vct-set! idata j 0.0))
      (do ((i hi (1+ i))
	   (j (- fsize hi) (1- j)))
	  ((= i (/ fsize 2)))
	(vct-set! rdata i 0.0)
	(vct-set! rdata j 0.0)
	(vct-set! idata i 0.0)
	(vct-set! idata j 0.0))
      (fft rdata idata -1)
      (vct-scale! rdata (/ 1.0 fsize))
      (set-samples 0 (1- len) rdata))))


;;; -------- comb-filter

(define comb-filter
  (lambda (scaler size)
    (let ((delay-line (make-vector size 0.0))
	  (delay-loc 0))
      (lambda (x)
	(if x
	    (let ((result (vector-ref delay-line delay-loc)))
	      (vector-set! delay-line delay-loc (+ x (* scaler result)))
	      (set! delay-loc (1+ delay-loc))
	      (if (= delay-loc size) (set! delay-loc 0))
	      result)
	    #f)))))

; (map-chan (comb-filter .8 32))

;;; the same thing using the CLM module is:

(define comb-filter 
  (lambda (scaler size)
    (let ((cmb (make-comb scaler size)))
      (lambda (x) (if x (comb cmb x) #f)))))

;;; by using filters at harmonically related sizes, we can get chords:

(define comb-chord
  (lambda (scaler size amp)
    (let ((c1 (make-comb scaler size))
	  (c2 (make-comb scaler (* size .75)))
	  (c3 (make-comb scaler (* size 1.2))))
      (lambda (x) (if x (* amp (+ (comb c1 x) (comb c2 x) (comb c3 x))) #f)))))

; (map-chan (comb-chord .95 100 .3))
; (map-chan (comb-chord .95 60 .3))

;;; or change the comb length via an envelope:

(define max-envelope
  (lambda (e mx)
    (if (null? e)
	mx
      (max-envelope (cddr e) (max mx (abs (cadr e)))))))

(define zcomb
  (lambda (scaler size pm)
    (let ((cmb (make-comb scaler size :max-size (+ size 1 (max-envelope pm 0))))
	  (penv (make-env :envelope pm :end (frames))))
      (lambda (x) (if x (comb cmb x (env penv)) #f)))))

; (map-chan (zcomb .8 32 '(0 0 1 10)))

;;; or notch: 

(define notch-filter 
  (lambda (scaler size)
    (let ((cmb (make-notch scaler size)))
      (lambda (x) (if x (notch cmb x) #f)))))

; (map-chan (notch-filter .8 32))

;;; or formant:

(define formant-filter
  (lambda (radius frequency)
    (let ((frm (make-formant radius frequency)))
      (lambda (x) (if x (formant frm x) #f)))))

; (map-chan (formant-filter .99 2400))

;;; to impose several formants, just add them in parallel:

(define formants
  (lambda (r1 f1 r2 f2 r3 f3)
    (let ((fr1 (make-formant r1 f1))
	  (fr2 (make-formant r2 f2))
	  (fr3 (make-formant r3 f3)))
      (lambda (x)
	(if x (+ (formant fr1 x)
		 (formant fr2 x)
		 (formant fr3 x))
	    #f)))))

; (map-chan (formants .99 900 .98 1800 .99 2700))

;;; to get a moving formant:

(define moving-formant
  (lambda (radius move)
    (let ((frm (make-formant radius (cadr move)))
	  (menv (make-env :envelope move :end (frames))))
      (lambda (x)
	(if x
	    (let ((val (formant frm x)))
	      (mus-set-frequency frm (env menv))
	      val)
	    #f)))))

; (map-chan (moving-formant .99 '(0 1200 1 2400)))


;;; -------- echo

(define echo 
  (lambda (scaler secs)
    (let ((del (make-delay (round (* secs (srate))))))
      (lambda (inval)
	(if inval (+ inval (delay del (* scaler (+ (tap del) inval)))) #f)))))

; (map-chan (echo .5 .5) 0 44100)

;;; here is a multi-channel version:

(define echoes
  ;; since map-across-sound-chans is expected here as the caller, I'll embed it
  (lambda (scaler secs decay-time)
    (let* ((len (channels))
	   (dels (make-vector len))
	   (total-len (+ (frames) (round (* (srate) decay-time)))))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(vector-set! dels i (make-delay (round (* secs (srate))))))
      (map-across-sound-chans
       (lambda (invals chans)
	 (if invals
	     (do ((i 0 (1+ i)))
		 ((= i chans) invals)
	       (let ((inval (vector-ref invals i))
		     (del (vector-ref dels i)))
		 (vector-set! invals i (+ inval (delay del (* scaler (+ (tap del) inval)))))))
	     #f))
       0 total-len
       (string-append "(echoes "
		      (number->string scaler) " "
		      (number->string secs) " "
		      (number->string decay-time) ")")))))

; (echoes .5 .75 2.0)

;;; here is a version that modulates the echos:

(define zecho 
  (lambda (scaler secs frq amp)
    (let* ((os (make-oscil frq))
	   (len (round (* secs (srate))))
	   (del (make-delay len :max-size (+ len amp 1))))
      (lambda (inval)
	(if inval 
	    (+ inval 
	       (delay del 
		      (* scaler (+ (tap del) inval))
		      (* amp (oscil os))))
	    #f)))))

; (map-chan (zecho .5 .75 6 10.0) 0 65000)

;;; or low-pass filter the echoes:

(define flecho 
  (lambda (scaler secs)
    (let* ((flt (make-fir-filter :order 4 :xcoeffs (list->vct '(.125 .25 .25 .125))))
	   (del (make-delay  (round (* secs (srate))))))
      (lambda (inval)
	(if inval 
	    (+ inval 
	       (delay del 
		      (fir-filter flt (* scaler (+ (tap del) inval)))))
	    #f)))))

; (map-chan (flecho .5 .9) 0 75000)


;;; -------- ring-mod and am
;;;
;;; CLM instrument is ring-modulate.ins

(define ring-mod
  (lambda (freq gliss-env)
    (let* ((os (make-oscil :frequency freq))
	   (len (frames))
	   (genv (make-env :envelope gliss-env :end len)))
      (lambda (inval)
	(if inval (* (oscil os (env genv)) inval) #f)))))

; (map-chan (ring-mod 100 '(0 0 1 0)))
; (map-chan (ring-mod 10 (list 0 0 1 (hz->radians 100))))

(define am 
  (lambda (freq) 
    (let ((os (make-oscil freq))) 
      (lambda (inval) 
	(if inval 
	    (amplitude-modulate 1.0 inval (oscil os)) #f)))))

; (map-chan (am 440))


;;; -------- hello-dentist
;;;
;;; CLM instrument version is in clm.html

(define hello-dentist 
  (lambda (frq amp)
    (let* ((rn (make-rand-interp :frequency frq :amplitude amp))
	   (i 0)
	   (j 0)
	   (len (frames))
	   (in-data (samples->vct 0 len))
	   (out-len (inexact->exact (round (* len (+ 1.0 (* 2 amp))))))
	   (out-data (make-vct out-len))
	   (rd (make-src :srate 1.0 
			 :input (lambda (dir) 
				  (let ((val (if (and (>= i 0) (< i len)) 
						 (vct-ref in-data i) 
						 0.0)))
				    (set! i (+ i dir)) 
				    val)))))
      (do ()
	  ((or (= i len) (= j out-len)))
	(vct-set! out-data j (src rd (rand-interp rn)))
	(set! j (+ j 1)))
      (vct->samples 0 j out-data))))

; (hello-dentist 40.0 .1)

;;; a very similar function uses oscil instead of rand-interp, giving
;;; various "Forbidden Planet" sound effects:

(define fp
  (lambda (sr osamp osfrq)
    (let* ((os (make-oscil osfrq))
	   (sr (make-src :srate sr))
	   (len (frames))
	   (sf (make-sample-reader))
	   (out-data (make-vct len)))
      (vct-map! out-data
		  (lambda () 
		    (src sr (* osamp (oscil os))
			 (lambda (dir)
			   (if (> dir 0)
			       (next-sample sf)
			       (previous-sample sf))))))
      (free-sample-reader sf)
      (vct->samples 0 len out-data))))

; (fp 1.0 .3 20)
	    

;;; -------- compand

(define compand
  (lambda ()
    (let* ((tbl (make-vct 17)))
      ;; we'll fill this by hand with some likely-looking companding curve
      ;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")
      (vct-set! tbl 8 0.0)
      (vct-set! tbl 7 -.25) (vct-set! tbl 9  .25) 
      (vct-set! tbl 6 -.45) (vct-set! tbl 10 .45) 
      (vct-set! tbl 5 -.6)  (vct-set! tbl 11 .6)
      (vct-set! tbl 4 -.72) (vct-set! tbl 12 .72)
      (vct-set! tbl 3 -.82) (vct-set! tbl 13 .82)
      (vct-set! tbl 2 -.90) (vct-set! tbl 14 .90)
      (vct-set! tbl 1 -.96) (vct-set! tbl 15 .96)
      (vct-set! tbl 0 -1.0) (vct-set! tbl 16 1.0)
      (lambda (inval)
	(if inval
	    (let ((index (+ 8.0 (* 8.0 inval))))
	      (array-interp tbl index 17))
	    #f)))))

; (map-chan (compand))

;;; since there's no state in this function, it can be used without change
;;; in any of the mapping functions (unlike echo, for example)

;;; it is easy to test functions like this in the listener:
;;;
;;;    >((compand) 0.0)
;;;    0.0
;;;    >((compand) 1.0)
;;;    1.0
;;;    >((compand) .1)
;;;    0.200000047683716
;;;    >((compand) .99)
;;;    0.996800001335146
;;;    >((compand) .95)
;;;    0.984000006675728


;;; -------- shift pitch keeping duration constant
;;;
;;; both src and granulate take a function argument to get input whenever it is needed.
;;; in this case, src calls granulate which reads the currently selected file.
;;; CLM version is in expsrc.ins

(define expsrc 
  (lambda (rate)
    (let* ((gr (make-granulate :expansion rate))
	   ;; this can be improved by messing with make-granulate's hop and length args
	   (sr (make-src :srate rate))
	   (vsize 1024)
	   (vbeg 0)
	   (v (samples->vct 0 vsize))
	   (inctr 0))
      (lambda (inval)
	(if inval
	    (src sr 0.0
		 (lambda (dir)
		   (granulate gr
			      (lambda (dir)
				(let ((val (vct-ref v inctr)))
				  (set! inctr (+ inctr dir))
				  (if (>= inctr vsize)
				      (begin
					(set! vbeg (+ vbeg inctr))
					(set! inctr 0)
					(samples->vct vbeg vsize 0 0 v)))
				  val)))))
	    #f)))))

;;; the next (expsnd) changes the tempo according to an envelope; the new duration
;;; will depend on the expansion envelope -- we integrate it to get
;;; the overall expansion, then use that to decide the new length.

(define integrate-envelope
  (lambda (e sum)
    (if (or (null? e) (null? (cddr e)))
	sum
      (integrate-envelope (cddr e) (+ sum (* (+ (cadr e) (cadddr e)) .5 (- (caddr e) (car e))))))))

(define max-x
  (lambda (e)
    (if (null? (cddr e))
	(car e)
      (max-x (cddr e)))))

(define expsnd
  (lambda (gr-env)
    (let* ((dur (/ (* (/ (frames) (srate)) (integrate-envelope gr-env 0.0)) (max-x gr-env)))
	   (gr (make-granulate :expansion (cadr gr-env) :jitter 0))
	   (ge (make-env :envelope gr-env :duration dur))
	   (sound-len (round (* (srate) dur)))
	   (len (max sound-len (frames)))
	   (out-data (make-vct len))
	   (sf (make-sample-reader)))
      (vct-map! out-data (lambda ()
			   (let ((val (granulate gr (lambda (dir) (next-sample sf)))))
			     (mus-set-increment gr (env ge))
			     val)))
      (free-sample-reader sf)
      (vct->samples 0 len out-data))))

; (expsnd '(0 1 2 .4))
; (expsnd '(0 .5 2 2.0))


;;; -------- cross-synthesis
;;;
;;; CLM version is in clm.html

(define cross-synthesis
  (lambda (cross-snd amp fftsize r)
    ;; cross-snd is the index of the other sound (as opposed to the map-chan sound)
    (let* ((freq-inc (/ fftsize 2))
	   (fdr (make-vct fftsize))
	   (fdi (make-vct fftsize))
	   (spectr (make-vct freq-inc))
	   (inctr 0)
	   (ctr freq-inc)
	   (radius (- 1.0 (/ r fftsize)))
	   (bin (/ (srate) fftsize))
	   (formants (make-vector freq-inc)))
      (do ((i 0 (1+ i)))
	  ((= i freq-inc))
	(vector-set! formants i (make-formant radius (* i bin))))
      (lambda (inval)
	(if inval
	    (let ((outval 0.0))
	      (if (= ctr freq-inc)
		  (begin
		    (samples->vct inctr fftsize cross-snd 0 fdr)
		    (set! inctr (+ inctr freq-inc))
		    (spectrum fdr fdi #f fftsize 2)
		    (vct-subtract! fdr spectr)
		    (vct-scale! fdr (/ 1.0 freq-inc))
		    (set! ctr 0)))
	      (set! ctr (+ ctr 1))
	      (vct-add! spectr fdr)
	      (* amp (formant-bank spectr formants inval)))
	    #f)))))

; (map-chan (cross-synthesis 1 .5 128 6.0))

;;; similar ideas can be used for spectral cross-fades, etc -- for example:

(define voiced->unvoiced
  (lambda (amp fftsize r tempo)
    (let* ((freq-inc (/ fftsize 2))
	   (fdr (make-vct fftsize))
	   (fdi (make-vct fftsize))
	   (spectr (make-vct freq-inc))
	   (noi (make-rand (/ (srate) 3)))
	   (inctr 0)
	   (ctr freq-inc)
	   (radius (- 1.0 (/ r fftsize)))
	   (bin (/ (srate) fftsize))
	   (len (frames))
	   (outlen (inexact->exact (/ len tempo)))
	   (hop (inexact->exact (* freq-inc tempo)))
	   (out-data (make-vct (max len outlen)))
	   (formants (make-vector freq-inc))
	   (old-peak-amp 0.0)
	   (new-peak-amp 0.0))
      (do ((i 0 (1+ i)))
	  ((= i freq-inc))
	(vector-set! formants i (make-formant radius (* i bin))))
      (call-with-current-continuation ; setup non-local exit (for C-g interrupt)
       (lambda (break)                ;   now (break value) will exit the call/cc returning value
	 (do ((k 0 (1+ k)))           ; this is our actual loop 
	     ((= k outlen))
	   (let ((outval 0.0))
	     (if (= ctr freq-inc)
		 (begin
		  (if (abort?)               ; let interface run
		      (break "interrupted")) ;   if C-g exit the loop returning the string "interrupted"
		  (samples->vct inctr fftsize 0 0 fdr)
		  (let ((pk (vct-peak fdr)))
		    (if (> pk old-peak-amp) (set! old-peak-amp pk)))
		  (spectrum fdr fdi #f fftsize 2)
		  (set! inctr (+ hop inctr))
		  (vct-subtract! fdr spectr)
		  (vct-scale! fdr (/ 1.0 freq-inc))
		  (set! ctr 0)))
	     (set! ctr (+ ctr 1))
	     (vct-add! spectr fdr)
	     (set! outval (formant-bank spectr formants (rand noi)))
	     (if (> (abs outval) new-peak-amp) (set! new-peak-amp (abs outval)))
	     (vct-set! out-data k outval)))
	 (vct-scale! out-data (* amp (/ old-peak-amp new-peak-amp)))
	 (vct->samples 0 (max len outlen) out-data)
	 (play-and-wait))))))

;;; this example also shows how to let the rest of Snd run during a long computation,
;;;  and a simple way to jump out of a loop if C-g is typed.
; (voiced->unvoiced 1.0 256 2.0 2.0)



;;; -------- convolution example

(define cnvtest
  ;; returns new max sample
  (lambda (snd0 snd1 amp)
    (let* ((flt-len (frames snd0))
	   (total-len (+ flt-len (frames snd1)))
	   (cnv (make-convolve :filter (samples->vct 0 flt-len snd0)))
	   (sf (make-sample-reader 0 snd1))
	   (out-data (make-vct total-len)))
      (vct-map! out-data (lambda () (convolve cnv (lambda (dir) (next-sample sf)))))
      (free-sample-reader sf)
      (vct-scale! out-data amp)
      (let ((max-samp (vct-peak out-data)))
	(vct->samples 0 total-len out-data snd1)
	(if (> max-samp 1.0) (set-y-bounds (- max-samp) max-samp snd1))
	max-samp))))

; (cnvtest 0 1 .1)


;;; -------- reverb (1-channel in this example)
;;;
;;; CLM version is jcrev.ins

(define jc-reverb
  (lambda (decay-dur low-pass volume amp-env)
    (let* ((allpass1 (make-all-pass -0.700 0.700 1051))
	   (allpass2 (make-all-pass -0.700 0.700  337))
	   (allpass3 (make-all-pass -0.700 0.700  113))
	   (comb1 (make-comb 0.742 4799))
	   (comb2 (make-comb 0.733 4999))
	   (comb3 (make-comb 0.715 5399))
	   (comb4 (make-comb 0.697 5801))
	   (outdel1 (make-delay (round (* .013 (srate)))))
	   (comb-sum 0.0)
	   (comb-sum-1 0.0)
	   (comb-sum-2 0.0)
	   (all-sums 0.0)
	   (delA 0.0)
	   (delB 0.0)
	   (dur (+ decay-dur (/ (frames) (srate))))
	   (envA (if amp-env (make-env :envelope amp-env :scaler volume :duration dur) #f))
	   (len (round (* dur (srate)))))
      (map-chan
       (lambda (inval)
	 (if inval
	     (let ((allpass-sum (all-pass allpass3 (all-pass allpass2 (all-pass allpass1 inval)))))
	       (set! comb-sum-2 comb-sum-1)
	       (set! comb-sum-1 comb-sum)
	       (set! comb-sum 
		     (+ (comb comb1 allpass-sum)
			(comb comb2 allpass-sum)
			(comb comb3 allpass-sum)
			(comb comb4 allpass-sum)))
	       (if low-pass
		   (set! all-sums (+ (* .25 (+ comb-sum comb-sum-2)) (* .5 comb-sum-1)))
		 (set! all-sums comb-sum))
	       (+ inval
		  (if envA
		      (* (env envA) (delay outdel1 all-sums))
		    (* volume (delay outdel1 all-sums)))))
	     #f))
       0 (round (* dur (srate)))))))

; (jc-reverb 2.0 #f .1 #f)


;;; -------- scissor-tailed flycatcher
;;;
;;; mix a scissor-tailed flycatcher call into the current sound
;;; CLM version is bigbird.ins (see bird.ins and bird.clm for lots more)

(define sum-partials
  (lambda (lst sum)
    (if (null? lst)
	sum
      (sum-partials (cddr lst) (+ sum (cadr lst))))))

(define scale-partials
  (lambda (lst scl newlst)
    (if (null? lst)
	newlst
      (scale-partials (cddr lst) scl (append newlst (list (car lst) (* scl (cadr lst))))))))

(define normalize-partials
  (lambda (lst)
    (scale-partials lst (/ 1.0 (sum-partials lst 0.0)) '())))

(define bigbird
  (lambda (start dur frequency freqskew amplitude
		 freq-envelope amp-envelope partials
		 lpcoeff)
    (let* ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	   (os (make-oscil :frequency frequency))
	   (fil (make-one-pole lpcoeff (- 1.0 lpcoeff)))
	   (coeffs (partials->polynomial (normalize-partials partials)))
	   (amp-env (make-env amp-envelope amplitude dur))
	   (len (round (* (srate) dur)))
	   (beg (round (* (srate) start)))
	   (sf (make-sample-reader beg))
	   (out-data (make-vct len)))
      (vct-map! out-data
		(lambda ()
		  (+ (next-sample sf)
		     (one-pole fil (* (env amp-env)
				      (polynomial coeffs
						  (oscil os (env gls-env))))))))
      (free-sample-reader sf)
      (vct->samples beg len out-data))))

(define scissor
  (lambda (begin-time)
    (let ((scissorf '(0 0  40 1  60 1  100 0)))
      (bigbird begin-time 0.05 1800 1800 .2 
	       scissorf 
	       '(0 0  25 1  75 1  100 0) 
	       '(1 .5  2 1  3 .5  4 .1  5 .01)
	       1.0))))

; (scissor 2.0)


;;; -------- fm-violin
;;;
;;; here we're using the keyword stuff in guile/ice-9/optargs.scm
;;; CLM version is v.ins, C version is in sndlib.html

(define (keyword->symbol kw)
  (let ((sym (symbol->string (keyword-dash-symbol kw))))
    (string->symbol (substring sym 1 (string-length sym)))))
;;; this is needed to get around a bug in optargs


(define pi 3.141592653589793)

(define fm-violin 
  (lambda* (startime dur frequency amplitude #&key
	    (fm-index 1.0)
	    (amp-env '(0 0  25 1  75 1  100 0))
	    (periodic-vibrato-rate 5.0) 
	    (random-vibrato-rate 16.0)
	    (periodic-vibrato-amplitude 0.0025) 
	    (random-vibrato-amplitude 0.005)
	    (noise-amount 0.0) 
	    (noise-freq 1000.0)
	    (ind-noise-freq 10.0) 
	    (ind-noise-amount 0.0)
	    (amp-noise-freq 20.0) 
	    (amp-noise-amount 0.0)
	    (gliss-env '(0 0  100 0)) 
	    (glissando-amount 0.0) 
	    (fm1-env '(0 1  25 .4  75 .6  100 0))  
	    (fm2-env '(0 1  25 .4  75 .6  100 0)) 
	    (fm3-env '(0 1  25 .4  75 .6  100 0))
	    (fm1-rat 1.0) 
	    (fm2-rat 3.0)	 
	    (fm3-rat 4.0)                    
	    (fm1-index #f) 
	    (fm2-index #f) 
	    (fm3-index #f)
	    (base 1.0)
	    (reverb-amount 0.01)
	    (degree #f) (distance 1.0) (degrees #f)
	    #&allow-other-keys)
    (let* ((beg (floor (* startime (srate))))
	   (len (floor (* dur (srate))))
	   (end (+ beg len))
	   (frq-scl (hz->radians frequency))
	   (modulate (not (zero? fm-index)))
	   (maxdev (* frq-scl fm-index))
	   (logfreq (log frequency))
	   (sqrtfreq (sqrt frequency))
	   (index1 (or fm1-index (min pi (* maxdev (/ 5.0 logfreq)))))
	   (index2 (or fm2-index (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001)))))))
	   (index3 (or fm3-index (min pi (* maxdev (/ 4.0 sqrtfreq)))))
	   (easy-case (and (zero? noise-amount)
			   (equal? fm1-env fm2-env)
			   (equal? fm1-env fm3-env)
			   (= fm1-rat (floor fm1-rat))
			   (= fm2-rat (floor fm2-rat))
			   (= fm3-rat (floor fm3-rat))))
	   (coeffs (and easy-case modulate
			(partials->polynomial
			 (list fm1-rat index1
			       (floor (/ fm2-rat fm1-rat)) index2
			       (floor (/ fm3-rat fm1-rat)) index3))))
	   (norm (or (and easy-case modulate 1.0) index1))
	   (carrier (make-oscil frequency))
	   (fmosc1  (and modulate (make-oscil (* fm1-rat frequency))))
	   (fmosc2  (and modulate (or easy-case (make-oscil (* fm2-rat frequency)))))
	   (fmosc3  (and modulate (or easy-case (make-oscil (* fm3-rat frequency)))))
	   (ampf  (make-env amp-env :scaler amplitude :base base :duration dur))
	   (indf1 (and modulate (make-env fm1-env norm :duration dur)))
	   (indf2 (and modulate (or easy-case (make-env fm2-env index2 :duration dur))))
	   (indf3 (and modulate (or easy-case (make-env fm3-env index3 :duration dur))))
	   (frqf (make-env gliss-env (* glissando-amount frq-scl) :duration dur))
	   (pervib (make-triangle-wave periodic-vibrato-rate (* periodic-vibrato-amplitude frq-scl)))
	   (ranvib (make-rand-interp random-vibrato-rate (* random-vibrato-amplitude frq-scl)))
	   (fm-noi (if (not (= 0.0 noise-amount))
		       (make-rand noise-freq (* pi noise-amount))
		       #f))
	   (ind-noi (if (and (not (= 0.0 ind-noise-amount)) (not (= 0.0 ind-noise-freq)))
			(make-rand-interp ind-noise-freq ind-noise-amount)
			#f))
	   (amp-noi (if (and (not (= 0.0 amp-noise-amount)) (not (= 0.0 amp-noise-freq)))
			(make-rand-interp amp-noise-freq amp-noise-amount)
			#f))
	   (vib 0.0) 
	   (modulation 0.0)
	   (loc (make-locsig :channels (channels) :degree (or degree degrees (random 90.0)) :reverb reverb-amount :distance distance))
	   (fuzz 0.0)
	   (ind-fuzz 1.0)
	   (amp-fuzz 1.0)
	   (out-data (make-vct len)))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(if (not (= 0.0 noise-amount))
	    (set! fuzz (rand fm-noi)))
	(set! vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib)))
	(if ind-noi (set! ind-fuzz (+ 1.0 (rand-interp ind-noi))))
	(if amp-noi (set! amp-fuzz (+ 1.0 (rand-interp amp-noi))))
	(if modulate
	    (if easy-case
		(set! modulation
		      (* (env indf1) 
			 (polynomial coeffs (oscil fmosc1 vib)))) ;(* vib fm1-rat)??
		(set! modulation
		      (+ (* (env indf1) (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
			 (* (env indf2) (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
			 (* (env indf3) (oscil fmosc3 (+ (* fm3-rat vib) fuzz)))))))
	(vct-set! out-data i 
		  (* (env ampf) amp-fuzz
		     (oscil carrier (+ vib (* ind-fuzz modulation))))))
      (if (= (channels) 2)
	  (let ((bsamps (vct-copy out-data)))
	    (mix-vct (vct-scale! bsamps (locsig-ref loc 1)) beg 1 #f 1 #f)
	    (mix-vct (vct-scale! out-data (locsig-ref loc 0)) beg 1 #f 0 #f))
	  (mix-vct out-data beg 1 #f 0 #f)))))

; (fm-violin 0 1 440 .1 :fm-index 2.0)

;;;   remeber to (read-set! keywords 'prefix) before using a keyword like :fm-index


;;; -------- zipper "crossfade"
;;;
;;; create the 'digital zipper' effect
;;; a not-very-debonair way to fade out file1 and fade in file2
;;; CLM version in zipper.ins

;(define max-envelope
;  (lambda (e mx)
;    (if (null? e)
;	mx
;      (max-envelope (cddr e) (max mx (abs (cadr e)))))))

(define zipper 
  (lambda* (beg dur file1 file2 ramp-envelope frame-size #&optional (ramp-envelope-base 1.0) (frame-envelope #f))
  ;; pan between file1 and file2 using a "zipper-like" effect
  ;; ramp-env at 0 => all file1, at 1 => all file2, in between a mixture
  ;; frame-size is the basic speed at which the mixture takes place (dependent also on frame-env)
  ;; ramp-env-base is the base of the panning envelope (for exponential pans)
  ;; frame-env affects the frame size -- don't let it get to 0!!
  (let* ((f1 (make-file->sample file1))
	 (f2 (make-file->sample file2))
	 (start (floor (* (srate) beg)))
	 (len (floor (* (srate) dur)))
	 (end (+ start len))
	 (tframe-envelope (or frame-envelope '(0 1 1 1)))
	 (fe (make-env tframe-envelope :scaler (* (srate) frame-size) :duration dur))
	 (maxframe (max-envelope tframe-envelope 0.0))
	 (ctr1 0)
	 (ctr2 0)
	 (frame-loc 0)
	 (ramp-loc 0.0)
	 (rampe (make-env ramp-envelope :duration dur :base ramp-envelope-base))
	 (trigger 0)
	 (frame-samples (1+ (ceiling (* (srate) maxframe frame-size))))
	 (frame (make-vct frame-samples))
	 (frame1 (make-vct frame-samples))
	 (frame2 (make-vct frame-samples))
	 (cursamples 0)
	 (sf (make-sample-reader start))
	 (low-start (/ 20.0 (srate)))
	 (high-start (- 1.0 low-start))
	 (out-data (make-vct len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (let ((insamp (next-sample sf)))
	(set! ramp-loc (env rampe))
	(set! frame-samples (floor (env fe)))
	;; fe's duration assumes it's being called on every sample, but we only need this value every once in a while
	(if (<= ramp-loc low-start)
	    (begin
	      (vct-set! out-data i (+ insamp (file->sample f1 ctr1)))
	      (set! ctr1 (+ ctr1 1)))
	    (if (>= ramp-loc high-start)
		(begin
		  (vct-set! out-data i (+ insamp (file->sample f2 ctr2)))
		  (set! ctr2 (+ ctr2 1)))
		;; else we're in the ramp phase
		;;  read frame if we're within its bounds
		(if (= trigger 0)
		    (vct-set! out-data i (+ insamp (vct-ref frame frame-loc)))
		    ;; now get next portion of the ramp
		    (begin
		      (set! cursamples frame-samples)
		      (let* ((changept (floor (* cursamples ramp-loc)))
			     (samp1 (/ 1.0 (- 1.0 ramp-loc)))
			     (samp2 (/ 1.0 ramp-loc)))
			(do ((k 0 (1+ k)))
			    ((= k cursamples))
			 (vct-set! frame1 k (file->sample f1 ctr1))
			 (set! ctr1 (+ ctr1 1))
			 (vct-set! frame2 k (file->sample f2 ctr2))
			 (set! ctr2 (+ ctr2 1)))
			;; now resample each dependent on location in ramp (samp1 and samp2 are increments)
			(clear-array frame)
			(let ((start-ctr 0.0))
			  (do ((k 0 (1+ k)))
			      ((= k changept))
			    (let* ((ictr (floor start-ctr))
				   (y0 (vct-ref frame2 ictr))
				   (y1 (vct-ref frame2 (+ ictr 1))))
			      (vct-set! frame k (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			      (set! start-ctr (+ start-ctr samp2)))))
			(let ((start-ctr 0.0)
			      (m changept))
			  (do ((k 0 (1+ k)))
			      ((= k (- cursamples changept)))
			    (let* ((ictr (floor start-ctr))
				   (y0 (vct-ref frame1 ictr))
				   (y1 (vct-ref frame1 (+ ictr 1))))
			      (vct-set! frame m (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			      (set! start-ctr (+ start-ctr samp1))
			      (set! m (+ m 1)))))
			(vct-set! out-data i (+ insamp (vct-ref frame 0))))))))
	(set! frame-loc (+ frame-loc 1))
	(set! trigger 0)
	(if (>= frame-loc cursamples)
	    (begin
	      (set! frame-loc 0)
	      (set! trigger 1)))))
    (free-sample-reader sf)
    (vct->samples start len out-data)
    )))

;;; this is also good if the same file is used twice -- sort of like a CD player gone berserk

; (zipper 0 1 "fyow.snd" "now.snd" '(0 0 1 1) .05)
; (zipper 0 3 "mb.snd" "fyow.snd" '(0 0 1.0 0 1.5 1.0 3.0 1.0) .025)


;;; -------- FOF example

(define two-pi (* 2 3.141592653589793))

(define fofins 
  (lambda* (beg dur frq amp vib f0 a0 f1 a1 f2 a2 #&optional (ae '(0 0 25 1 75 1 100 0)))
    (let* ((start (floor (* beg (srate))))
	   (len (floor (* dur (srate))))
	   (ampf (make-env :envelope ae :scaler amp :duration dur))
	   (frq0 (hz->radians f0))
	   (frq1 (hz->radians f1))
	   (frq2 (hz->radians f2))
	   (foflen (if (= (srate) 22050) 100 200))
	   (vibr (make-oscil :frequency 6))
	   (win-freq (/ two-pi foflen))
	   (foftab (make-vct foflen))
	   (sf (make-sample-reader start))
	   (wt0 (make-wave-train :wave foftab :frequency frq))
	   (out-data (make-vct len)))
      (do ((i 0 (1+ i)))
	  ((= i foflen))
	(vct-set! foftab i (* (+ (* a0 (sin (* i frq0)))
				 (* a1 (sin (* i frq1)))
				 (* a2 (sin (* i frq2))))
			      .5 (- 1.0 (cos (* i win-freq))))))
      (vct-map! out-data 
		(lambda ()
		  (+ (next-sample sf)
		     (* (env ampf) 
			(wave-train wt0 (* vib (oscil vibr)))))))
      (free-sample-reader sf)
      (vct->samples start len out-data))))

; (fofins 0 1 270 .2 .001 730 .6 1090 .3 2440 .1)


;;; -------- phase vocoder --------
;;;
;;; this is a translation of Michael Klingbeil's pvoc.ins in CLM

(define ifloor (lambda (n) (inexact->exact (floor n))))
(define pi 3.141592653589793)

(define pvoc
  (lambda* (#&key
	   (fftsize 512) (overlap 4) (time 1.0)
	   (pitch 1.0) (gate 0.0) (hoffset 0.0))
    (let* ((len (frames))
	   (filptr 0)           ; index into the file
	   (pi2 (* 2 pi))       ; handy constant
	   (sr (srate))
	   (N fftsize)          ; fft size
	   (N2 (ifloor (/ N 2)))  ;; half the fft size
	   (Nw fftsize) ;; window size -- currently restricted to the fftsize
	   (D (ifloor (/ fftsize overlap))) ; decimation factor (how often do we take an fft)
	   (interp (* (ifloor (/ fftsize overlap)) time)) ; interpolation factor how often do we synthesize
	   (windowsum 0.0)        ; for window normalization
	   ;; take a resynthesis gate specificed in dB, convert to linear amplitude
	   (syngate (if (= 0.0 gate) 0.0 (expt 10 (/ (- (abs gate)) 20))))
	   (poffset (hz->radians hoffset))
	   (window (make-vct Nw)) ; array for the window
	   (fdr (make-vct N))     ; buffer for real fft data
	   (fdi (make-vct N))     ; buffer for imaginary fft data
	   (lastphase (make-vct N2)) ;; last phase change
	   (lastamp (make-vct N2)) ;; last sampled amplitude
	   (lastfreq (make-vct N2)) ;; last sampled frequency
	   (ampinc (make-vct N2)) ;; amplitude interpolation increment
	   (freqinc (make-vct N2)) ;; frequency interpolation increments
	   ;; expresses the fundamental in terms of radians per OUTPUT sample
	   (fundamental (/ pi2 N))
	   (output interp)      ; count of samples that have been output
	   (resynth-oscils (make-vector N2))  ; synthesis oscillators
	   (nextpct 10.0)       ; how often to print out the percentage complete message
	   (outlen (ifloor (* time len)))
	   (out-data (make-vct (max len outlen)))
	   (in-data (samples->vct (* N 2)))
	   (in-data-beg 0))
      ;; setup oscillators
      (do ((i 0 (1+ i)))
	  ((= i N2))
	(vector-set! resynth-oscils i (make-oscil :frequency 0)))
      ;; set-up the analysis window here
      (set! windowsum 0.0)
      ;; create a Hamming window (Moore p. 251)
      (do ((k 0 (1+ k)))
	  ((= k Nw))
	(let ((val (- 0.54 (* 0.46 (cos (* 2 pi (/ k (- Nw 1))))))))
	  (vct-set! window k val)
	  (set! windowsum (+ windowsum val))))
      ;; normalize window
      (set! windowsum (/ 2.0 windowsum))
      ;; loop over normalizing the window
      (vct-scale! window windowsum)
      (call-with-current-continuation
       (lambda (break)
	 (do ((i 0 (1+ i)))
	     ((>= i outlen))
	   ;; begin the master run loop
	   (if (>= output interp) ;; if all the samples have been output then do the next frame
	       (let ((buffix (modulo filptr N)))
					; buffix is the index into the input buffer
					; it wraps around circularly as time increases in the input
		 (if (abort?) (break "interrupted"))
		 (set! output 0)       ; reset the output sample counter
		 ;; save the old amplitudes and frequencies
		 (vct-fill! lastamp 0.0)
		 (vct-fill! lastfreq 0.0)
		 (vct-add! lastamp fdr)
		 (vct-add! lastfreq fdi)
		 (do ((k 0 (1+ k)))
		     ((= k N))
		   ;; apply the window and then stuff into the input array
		   (vct-set! fdr buffix (* (vct-ref window k) (vct-ref in-data (- filptr in-data-beg))))
		   (set! filptr (1+ filptr))
		   ;; increment the buffer index with wrap around
		   (set! buffix (1+ buffix))
		   (if (>= buffix N) (set! buffix 0)))
		 ;; rewind the file for the next hop
		 (set! filptr (- filptr (- N D)))
		 (if (> filptr (+ in-data-beg N))
		     (begin
		       (set! in-data-beg filptr)
		       (samples->vct in-data-beg (* N 2) 0 0 in-data)))
		 ;; no imaginary component input so zero out fdi
		 (vct-fill! fdi 0.0)
		 ;; compute the fft
		 (mus-fft fdr fdi N 1)
		 ;; now convert into magnitude and interpolated frequency
		 (do ((k 0 (1+ k)))
		     ((= k N2))
		   (let* ((a (vct-ref fdr k))
			  (b (vct-ref fdi k))
			  (mag (* (sqrt (+ (* a a) (* b b)))))
			  (phase 0)
			  (phasediff 0))
		     (vct-set! fdr k mag)    ;; current amp stored in fdr
		     ;; mag is always positive
		     ;; if it is zero then the phase difference is zero
		     (if (> mag 0)
			 (begin
			  (set! phase (- (atan b a)))
			  (set! phasediff (- phase (vct-ref lastphase k)))
			  (vct-set! lastphase k phase)
			  ;; frequency wrapping from Moore p. 254
			  (if (> phasediff pi) (do () ((<= phasediff pi)) (set! phasediff (- phasediff pi2))))
			  (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (set! phasediff (+ phasediff pi2))))))
		     ;; current frequency stored in fdi
		     ;; scale by the pitch transposition
		     (vct-set! fdi k 
			       (* pitch (+ (/ (* phasediff sr) (* D sr))
					   (* k fundamental)
					   poffset)))
		     ;; resynthesis gating
		     (if (< (vct-ref fdr k) syngate) (vct-set! fdr k 0.0))
		     ;; take (vct-ref lastamp k) and count up to (vct-ref fdr k)
		     ;; interpolating by ampinc
		     (vct-set! ampinc k (/ (- (vct-ref fdr k) (vct-ref lastamp k)) interp))
		     ;; take (vct-ref lastfreq k) and count up to (vct-ref fdi k)
		     ;; interpolating by freqinc
		     (vct-set! freqinc k (/ (- (vct-ref fdi k) (vct-ref lastfreq k)) interp))))))
	   ;; loop over the partials interpolate frequency and amplitude
	   (vct-add! lastamp ampinc)
	   (vct-add! lastfreq freqinc)
	   (vct-set! out-data i (oscil-bank lastamp resynth-oscils lastfreq))
	   (set! output (1+ output)))
	 (vct->samples 0 (max len outlen) out-data))))))


;;;-------- mix with envelope on mixed-in file
;;;
;;; there are lots of ways to do this; this version uses functions from Snd, CLM, and Sndlib.

(define enveloped-mix
  (lambda (filename beg env)
    (let ((len (mus-sound-frames filename))
	  (tmpfil (mus-sound-open-output "/tmp/tmp.snd" 22050 1 mus-bshort mus-next ""))
	  (mx (make-mixer 1 1.0))
	  (envs (make-vector 1))
	  (inenvs (make-vector 1)))
      (mus-sound-close-output tmpfil 0)
      (vector-set! inenvs 0 (make-env env :end len))
      (vector-set! envs 0 inenvs)
      (mus-mix "/tmp/tmp.snd" filename 0 len 0 mx envs)
      (mix "/tmp/tmp.snd" beg)
      (delete-file "/tmp/tmp.snd"))))

;(enveloped-mix "pistol.snd" 0 '(0 0 1 1 2 0))

;;; another way:

(define enveloped-mix-1
  (lambda (filename beg env)
    (as-one-edit
     (lambda ()
       (let* ((mix-id (mix filename beg))
	      (inchans (mix-chans mix-id)))
	 (do ((i 0 (1+ i)))
	     ((= i inchans))
	   (set-mix-amp-env mix-id i env)))))))



;;; -------- time varying FIR filter

(define fltit
  (lambda () 
    (let* ((coeffs (list .1 .2 .3 .4 .4 .3 .2 .1))
	   (flt (make-fir-filter 8 (list->vct coeffs)))
	   (es (make-vector 8)))
      (do ((i 0 (1+ i)))
	  ((= i 8))
	(vector-set! es i (make-env (list 0 (list-ref coeffs i) 1 0) :end 100)))
      (vector-set! es 5 (make-env '(0 .4 1 1) :duration 1.0))
      (lambda (x)
	(if x
	    (let ((val (fir-filter flt x))
		  (xcof (mus-data flt)))
	      (do ((i 0 (1+ i)))
		  ((= i 8))
		(vct-set! xcof i (env (vector-ref es i))))
	      val)
	    #f)))))

;(map-chan (fltit))


;;; -------- map-sound-files, match-sound-files
;;;
;;; apply a function to each sound in dir
;;;
;;;   (map-sound-files (lambda (n) (if (> (mus-sound-duration n) 10.0) (snd-print n))))

(define map-sound-files
  (lambda args
    "(map-sound-files func &optional dir) applies func to each sound file in dir"
    (let* ((func (car args))
	   (files (sound-files-in-directory (if (null? (cdr args)) "." (cadr args)))))
      (do ((i 0 (1+ i)))
	  ((= i (vector-length files)))
	(func (vector-ref files i))))))

(define match-sound-files
  (lambda args
    "(match-sound-files func &optional dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
    (let* ((func (car args))
	   (files (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
	   (matches '()))
      (do ((i 0 (1+ i)))
	  ((= i (vector-length files)))
	(let ((filename (vector-ref files i)))
	  (if (func filename)
	      (set! matches (cons filename matches)))))
      matches)))
  
;;; we can use Guile's regexp support here to search for all .snd and .wav files:
;
;(let ((reg (make-regexp ".wav|.snd$")))
;  (match-sound-files (lambda (file) (regexp-exec reg file))))
;
;;; this argument to make-regexp is looking for *.wav and *.snd
;;; in fact, we could use regexp's in place of Snd's sound-files-in-directory.


