;;; examples of Guile extensions to Snd
;;;
;;; (this file requires guile 1.5 -- it uses the generalized set! and newer optargs support)
;;;
;;;        contents
;;;
;;; documentation examples made harder to break
;;; 'info' from extsnd.html using format
;;; correlation
;;; XEmacs-like "Buffers" menu
;;; set transform-size based on current time domain window size
;;; superimpose spectra of sycn'd sounds
;;; example of c-g?
;;; play sound n times or until c-g
;;; play region over and over until C-g typed
;;; play samples created on-the-fly
;;; play while looping continuously between two movable marks
;;; make a system call
;;; translate mpeg input to 16-bit linear and read into Snd
;;; make dot size dependent on number of samples being displayed
;;; move window left edge to mark upon 'm' key
;;; flash selected data red and green
;;; use loop info (if any) to set marks at loop points
;;; mapping extensions (map arbitrary single-channel function over various channel collections)
;;;     do-chans, do-all-chans, do-sound-chans
;;;     every-sample?
;;;     sort-samples
;;; mix mono sound into stereo sound panning according to env, also simple sound placement
;;; fft-edit, fft-squelch, fft-env-interp, fft-smoother -- FFT based editing, fft-smoothing
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
;;; fm-violin (FM and various other generators, #:key args)
;;; FOF voice synthesis (wave-train, #:optional args)
;;; phase vocoder
;;; time varying FIR filter, notch filter
;;; swap selection chans
;;; sound-interp, env-sound-interp
;;; add date and time to title bar
;;; with-sound for Snd
;;; how to get 'display' to write to Snd's listener
;;; hold DAC open and play sounds via keyboard
;;; pluck instrument (physical modelling)
;;; voice instrument (formants via FM)
;;; filtered-env (low-pass and amplitude follow envelope)
;;; multi-colored rxvt printout
;;; locsig using fancier placement choice (Michael Edwards)
;;; lisp graph with draggable x axis
;;; pointer focus within Snd
;;; View: Files dialog chooses which sound is displayed
;;;    C-x b support along the same lines 
;;; "vector synthesis"
;;; remove-clicks
;;; searching examples (zero+, next-peak, find-pitch)
;;; sound-data->list
;;; file->vct and a sort of cue-list, I think, and region-play-list, region-play-sequence

;;; TODO: decide how to handle the CLM examples
;;; TODO: robust pitch tracker
;;; TODO: adaptive notch filter
;;; TODO: ins: singer piano flute fade
;;; TODO: data-file rw case for pvoc.scm
;;; TODO: C-s to click (using the int rtn inc case)
;;; TODO: C-s wrap-around (as in Emacs)
;;; TODO: triggered record
;;; TODO: notation following location (as in display-current-window-location)
;;;       but this requires some way to converse with cmn that does not require sleep

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(debug-enable 'debug 'backtrace)
(read-enable 'positions)

(if (not (defined? 'all-chans))
    (define (all-chans)
      (let ((sndlist '())
	    (chnlist '()))
	(for-each (lambda (snd)
		    (do ((i (1- (channels snd)) (1- i)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))


;;; -------- (ext)snd.html examples made harder to break --------
;;;
;;; this mainly involves keeping track of the current sound/channel

(define region-rms-1
  (lambda (n)
    "(region-rms-1 n) -> rms of region n's data (chan 0) the slow way"
    (if (region? n)
	(let* ((data (region-samples 0 0 n))
	       (len (vector-length data))
	       (sum 0.0))
	  (do ((i 0 (1+ i))) 
	      ((= i len) (sqrt (/ sum len)))
	    (set! sum (+ sum (* (vector-ref data i) (vector-ref data i))))))
	(throw 'no-such-region (list "region-rms-1" n)))))

(define selection-rms-1
  (lambda ()
    "(selection-rms-1) -> rms of selection data using sample readers"
    (if (selection?)
	(let* ((reader (make-sample-reader (selection-position)))
	       (len (selection-length))
	       (sum 0.0))
	  (do ((i 0 (1+ i))) 
	      ((= i len) 
	       (begin 
		 (free-sample-reader reader) 
		 (sqrt (/ sum len))))
	    (let ((val (next-sample reader)))
	      (set! sum (+ sum (* val val))))))
	(throw 'no-active-selection (list "selection-rms-1")))))

;;; if you'd rather use recursion:
(define selection-rms
  (lambda ()
    "(selection-rms) -> rms of selection data using sample readers and recursion"
    ;; this is actually slightly faster than selection-rms-1
    ;; all the DO loops in this file could be re-written in this form, but I find loops easier to read
    (if (selection?)
	(let* ((reader (make-sample-reader (selection-position)))
	       (len (selection-length)))
	  (define rsum 
	    (lambda (leng sum)
	      (if (= leng 0)
		  (sqrt (/ sum len))
		  (let ((val (next-sample reader)))
		    (rsum (1- leng) (+ sum (* val val)))))))
	  (let ((val (rsum len 0.0)))
	    (free-sample-reader reader)
	    val))
	(throw 'no-active-selection(list "selection-rms")))))

(define region-rms
  (lambda (n)
    "(region-rms n) -> rms of region n's data (chan 0)"
    (if (region? n)
	(let* ((data (region-samples->vct 0 0 n)))
	  (sqrt (/ (dot-product data data) (vct-length data))))
	(throw 'no-such-region (list "region-rms" n)))))


(define window-samples
  (lambda (snd chn)
    "(window-samples snd chn) -> sample in snd channel chn in current graph window"
    (let ((wl (left-sample snd chn))
	  (wr (right-sample snd chn)))
      (samples wl (+ 1 (- wr wl)) snd chn))))

(define display-energy
  ;; in this version, the y-zoom-slider controls the graph amp
  (lambda (snd chn)
    "(display-energy snd chn) is a lisp-graph-hook function to display the time domain data\n\
    as energy (squared)"
    (let* ((ls (left-sample))
           (rs (right-sample))
	   (datal (make-graph-data snd chn))
	   (data (if (vct? datal) datal (cadr datal)))
           (len (vct-length data))
           (sr (srate snd))
	   (y-max (y-zoom-slider snd chn)))
      (vct-multiply! data data)
      (graph data "energy" (/ ls sr) (/ rs sr) 0.0 (* y-max y-max) snd chn #f))))

;(add-hook! lisp-graph-hook display-energy)

(define display-db
  (lambda (snd chn)
    "(display-db snd chn) is a lisp-graph-hook function to display the time domain data in dB"
    (let* ((datal (make-graph-data snd chn))
	   (data (if (vct? datal) datal (cadr datal)))
           (sr (srate snd)))
      (define (dB val)
	(if (< val .001)
	    -60.0
	    (* 20.0 (log10 val))))
      (vct-do! data (lambda (i)
		      (vct-set! data i (+ 60.0 (dB (abs (vct-ref data i)))))))
      (graph data "dB" 
	     (/ (left-sample snd chn) sr) (/ (right-sample snd chn) sr)  
	     0.0 60.0
	     snd chn #f))))

;(add-hook! lisp-graph-hook display-db)

(define window-rms
  (lambda ()
    "(window-rms n) -> rms of data in currently selected graph window"
    (let* ((ls (left-sample))
	   (rs (right-sample))
	   (data (samples->vct ls (+ 1 (- rs ls))))
	   (len (vct-length data)))
      (sqrt (/ (dot-product data data) len)))))


(define no-startup-file?
  (lambda (ind file)
    "(no-startup-file?) is intended as a start-hook procedure; if a file is specified in the
  Snd invocation, but the file doesn't exist, Snd exits back to the shell"
    (if (= ind (max-sounds))
	(begin
	  (write (string-append "can't open " file) (current-error-port))
	  (newline (current-error-port))
	  #t)
	(if (sound? ind)
	    #f
	    (no-startup-file? (+ ind 1) file)))))

;(add-hook! start-hook (lambda (file) (if (> (string-length file) 0) (no-startup-file? 0 file) #f)))


(define fft-peak
  (lambda (snd chn scale)
    "(fft-peak) returns the peak spectral magnitude"
    (if (and (graph-transform?) (= (transform-graph-type) graph-transform-once))
	(report-in-minibuffer 
	 (number->string (/ (* 2.0 (vct-peak (transform-samples->vct snd chn))) (transform-size)))
	 snd)
      #f)))

;(add-hook! transform-hook fft-peak)


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
   "(correlate snd chn y0 y1) returns the correlation of snd's 2 channels"
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
;;; see effects.scm for a much fancier example

;(define buffer-menu (add-to-main-menu "Buffers"))

(define open-buffer
  (lambda (filename)
    "(open-buffer filename) adds a menu item that will select filename"
    (add-to-menu buffer-menu 
		 filename 
		 (lambda () (select-sound (find-sound filename))))
    #f))

(define close-buffer 
  (lambda (snd)
    "(close-buffer snd) removes the menu item associated with snd"
    (remove-from-menu buffer-menu (file-name snd))
    #f))

;;; here we're adding this menu handling code to whatever is already happening at open/close-hook time

;(add-hook! open-hook open-buffer)
;(add-hook! close-hook close-buffer)


;;; -------- set transform-size based on current time domain window size
;;;
;;; also zoom spectrum based on y-axis zoom slider

(define (zoom-spectrum snd chn y0 y1)
  (if (and (graph-transform? snd chn) (= (transform-graph-type snd chn) graph-transform-once))
      (begin
	(set! (transform-size snd chn)
	      (expt 2 (ceiling 
		       (/ (log (- (right-sample snd chn) (left-sample snd chn))) 
			  (log 2.0)))))
	(set! (spectro-cutoff snd chn) (y-zoom-slider snd chn)))))

;(add-hook! graph-hook zoom-spectrum)

;;; this version only messes with the fft settings if the time domain is not displayed
;;;   it also sets the spectrum display start point based on the x position slider
;;;   this can be confusing if fft normalization is on (the default)

(define (zoom-fft snd chn y0 y1)
  (if (and (graph-transform? snd chn)
	   (not (graph-time? snd chn))
	   (= (transform-graph-type snd chn) graph-transform-once))
      (begin
	(set! (transform-size snd chn)
	      (expt 2 (ceiling 
		       (/ (log (- (right-sample snd chn) (left-sample snd chn))) 
			  (log 2.0)))))
	(set! (spectro-start snd chn) (x-position-slider snd chn))
	(set! (spectro-cutoff snd chn) (y-zoom-slider snd chn)))))

;(add-hook! graph-hook zoom-fft)


;;; -------- superimpose spectra of sycn'd sounds

(define superimpose-ffts
  (lambda (snd chn y0 y1)
    "(superimpose-ffts snd chn y0 y1) superimposes ffts of multiple (syncd) sounds"
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
		    (not (sound? ind))
		    (and (= (sync snd) (sync ind))
			 (> ind snd)))
		(min-at-sync snd (1+ ind))
		#f))))
    (define collect-ffts 
      (lambda (samp size snd chn ffts ind)
	(if (>= ind (max-sounds)) 
	    ffts
	    (if (and (sound? ind)
		     (= (sync snd) (sync ind))
		     (> (chans ind) chn))
		(collect-ffts samp size snd chn (append ffts (list (make-one-fft samp size ind chn))) (1+ ind))
		(collect-ffts samp size snd chn ffts (1+ ind))))))
    (if (and (> (sync snd) 0)
	     (min-at-sync snd 0))
	;; we are sync, and we are the top sound in this sync group
	(let* ((ls (left-sample snd chn))
	       (rs (right-sample snd chn))
	       (pow2 (inexact->exact (ceiling (/ (log (- rs ls)) (log 2)))))
	       (fftlen (inexact->exact (expt 2 pow2))))
	  (if (> pow2 2)
	      (graph (collect-ffts ls fftlen snd chn '() snd)
		     "spectra" 0.0 0.5 #f #f snd chn)))
	(set! (graph-lisp? snd chn) #f))
    #f))

;(add-hook! graph-hook superimpose-ffts)


;;; -------- c-g? example (Anders Vinjar)

(define (locate-zero limit)
  (let* ((start (cursor))
	 (sf (make-sample-reader start)))
    (do ((n start (1+ n))
	 (val0 (abs (next-sample sf)) val1)
	 (val1 (abs (next-sample sf)) (abs (next-sample sf))))
	((or (sample-reader-at-end? sf)
	     (c-g?)
	     (< (+ val0 val1) limit))
	 (begin
	   (free-sample-reader sf)
	   (set! (cursor) n)
	   n)))))


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



;;; -------- make a system call from the listener
;;;
;;;   (shell "df") for example -- there's probably a more elegant way to do this is in Scheme
;;; or to play a sound whenever a file is closed:
;;;   (add-hook! close-hook (lambda (snd) (shell \"sndplay wood16.wav\")))

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

;;; to simply make a system call, you can use the system function
;;;   (system "ls *.snd")
;;; but output goes to stdout.

;;; see also Thien-Thi Nguyen's shell-command-to-string in his package of useful Scheme code at
;;;   http://www.glug.org/people/ttn/software/ttn-pers-scheme/



;;; -------- translate mpeg input to 16-bit linear and read into Snd
;;;
;;; mpg123 with the -s switch sends the 16-bit (mono or stereo) representation of
;;;   an mpeg file to stdout.  There's also apparently a switch to write 'wave' output.

(define mpg
  (lambda (mpgfile rawfile)
    "(mpg file tmpname) converts file from MPEG to raw 16-bit samples using mpg123"
    (let* ((fd (open-file mpgfile "r"))
	   (b0 (char->integer (read-char fd)))
	   (b1 (char->integer (read-char fd)))
	   (b2 (char->integer (read-char fd)))
	   (b3 (char->integer (read-char fd))))
      (close fd)
      (if (or (not (= b0 255))
	      (not (= (logand b1 #b11100000) #b11100000)))
	  (snd-print (format #f "~S is not an MPEG file (first 11 bytes: #b~B #b~B)" mpgfile b0 (logand b1 #b11100000)))
	  (let ((id (ash (logand b1 #b11000) -3))
		(layer (ash (logand b1 #b110) -1))
		(protection (logand b1 1))
		(bitrate-index (ash (logand b2 #b11110000) -4))
		(srate-index (ash (logand b2 #b1100) -2))
		(padding (ash (logand b2 #b10) -1))
		(channel-mode (ash (logand b3 #b11000000) -6))
		(mode-extension (ash (logand b3 #b110000) -4))
		(copyright (ash (logand b3 #b1000) -3))
		(original (ash (logand b3 #b100) -2))
		(emphasis (logand b3 #b11)))
	    (if (= id 1)
		(snd-print (format #f "odd: ~S is using a reserved Version ID" mpgfile)))
	    (if (= layer 0)
		(snd-print (format #f "odd: ~S is using a reserved layer decription" mpgfile)))
	    (let* ((chans (if (= channel-mode 3) 1 2))
		   (mpegnum (if (= id 0) 4 (if (= id 2) 2 1)))
		   (mpeg-layer (if (= layer 3) 1 (if (= layer 2) 2 3)))
		   (srate (/ (list-ref (list 44100 48000 32000 0) srate-index) mpegnum)))
	      (snd-print (format #f "~S: ~A Hz, ~A, MPEG-~A~%" 
				 mpgfile srate (if (= chans 1) "mono" "stereo") mpeg-layer))
	      (system (format #f "mpg123 -s ~A > ~A" mpgfile rawfile))
	      (open-raw-sound rawfile chans srate (if (little-endian?) mus-lshort mus-bshort))))))))

;;; (mpg "mpeg.mpg" "mpeg.raw")


;;; -------- make dot size dependent on number of samples being displayed
;;; 
;;; this could be extended to set graph-style to graph-lines if many samples are displayed, etc

(define auto-dot
  (lambda (snd chn y0 y1)
    "(auto-dot snd chn y0 y1) sets the dot size depending on the number of samples being displayed"
    (let ((dots (- (right-sample snd chn)
		   (left-sample snd chn))))
      (if (> dots 100) 
	  (set! (dot-size snd chn) 1)
	(if (> dots 50)
	    (set! (dot-size snd chn) 2)
	  (if (> dots 25)
	      (set! (dot-size snd chn) 3)
	    (set! (dot-size snd chn) 5))))
      #f)))
    
;(add-hook! graph-hook auto-dot)



;;; -------- move window left edge to mark upon 'm'
;;;
;;; in large sounds, it can be pain to get the left edge of the window
;;; aligned with a specific spot in the sound.  In this code, we assume
;;; the desired left edge has a mark, and the 'm' key (without control)
;;; will move the window left edge to that mark.

(define first-mark-in-window-at-left
  (lambda ()
    "(first-mark-in-window-at-left) moves the graph so that the leftmost visible mark is at the left edge"
    (let* ((keysnd (or (selected-sound) 0))
	   (keychn (or (selected-channel keysnd) 0))
	   (current-left-sample (left-sample keysnd keychn))
	   (chan-marks (marks keysnd keychn)))
      (define (find-leftmost-mark samples)
	(if (null? samples)
	    #f
	    (if (> (car samples) current-left-sample)
		(car samples)
		(find-leftmost-mark (cdr samples)))))
      (if (= (length chan-marks) 0)
	  (report-in-minibuffer "no marks!")
	  (let ((leftmost (find-leftmost-mark (map mark-sample chan-marks))))
	    (if (number? leftmost)
		(begin
		  (set! (left-sample keysnd keychn) leftmost)
		  cursor-update-display)
		(report-in-minibuffer "no mark in window")))))))

(bind-key (char->integer #\m) 0 (lambda () (first-mark-in-window-at-left)))


;;; -------- flash selected data red and green
;;;

(define red (make-color 1 0 0))
(define green (make-color 0 1 0))
(define data-red? #t)
;(set! (selected-data-color) red)

(define flash-selected-data
  (lambda (interval)
    "(flash-selected-data millisecs) causes the selected data to flash red and green"
    (if (selected-sound)
	(begin
	  (set! (selected-data-color) (if data-red? green red))
	  (set! data-red? (not data-red?))
	  (in interval (lambda () (flash-selected-data interval)))))))


;;; --------  use loop info (if any) to set marks at loop points

(define mark-loops
  (lambda ()
    "(mark-loops) places marks at loop points found in the selected sound's header"
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
		
	    

;;; -------- mapping extensions (map arbitrary single-channel function over various channel collections)
;;;

(define do-all-chans
  (lambda (func origin)
    "(do-all-chans func edhist) applies func to all active channels, using edhist as the edit history indication"
    (apply map (lambda (snd chn)
		 (map-chan func #f #f origin snd chn))
	   (all-chans))))

;; (do-all-chans (lambda (val) (* 2.0 val)) "double all samples") 

(define update-graphs
  (lambda ()
    "(update-graphs) updates (redraws) all graphs"
    (apply map (lambda (snd chn)
		 (update-time-graph snd chn))
	   (all-chans))))

(define do-chans
  (lambda (func origin)
    "(do-chans func edhist) applies func to all sync'd channels using edhist as the edit history indication"
    (let ((snc (sync)))
      (if (> snc 0)
	  (apply map
		 (lambda (snd chn)
		   (if (= (sync snd) snc)
		       (map-chan func #f #f origin snd chn)))
		 (all-chans))
	  (snd-warning "sync not set")))))

(define do-sound-chans
  (lambda (proc origin)
    "(do-sound-chans func args edhist) applies func to all selected channels using edhist as the edit history indication"
    (let ((snd (selected-sound)))
      (if (sound? snd)
	  (begin
	    (do ((chn 0 (1+ chn)))
		((= chn (channels snd)) #f)
	      (map-chan proc #f #f origin snd chn)))
	  (snd-warning "no selected sound")))))

(define every-sample?
  (lambda (proc)
    "(every-sample func) -> #t if func is not #f for all samples in the current channel,
  otherwise it moves the cursor to the first offending sample"
    (let ((baddy (scan-chan 
		  (lambda (y) 
		    (not (proc y))))))
      (if baddy (set! (cursor) (cadr baddy)))
      (not baddy))))

(define sort-samples
  (lambda (nbins)
    "(sort-samples bins) provides a histogram in bins bins"
    (let ((bins (make-vector nbins 0)))
      (scan-chan
       (lambda (y)
	 (let ((bin (inexact->exact (floor (* (abs y) nbins)))))
	   (vector-set! bins bin (+ (vector-ref bins bin) 1))
	   #f)))
      bins)))



;;; -------- mix mono sound into stereo sound panning according to env

(define place
  (lambda (mono-snd stereo-snd pan-env)
    (let* ((len (frames mono-snd))
	   (msf (make-sample-reader 0 mono-snd 0))
	   (out-data0 (make-vct len))
	   (out-data1 (make-vct len))
	   (e (make-env pan-env :end (1- len))))
      (vcts-do! out-data0 out-data1
		 (lambda (num i)
		   (let ((panval (env e))
			 (val (next-sample msf)))
		     (list (* val (- 1.0 panval)) (* val panval)))))
      (free-sample-reader msf)
      (vct-add! out-data0 (samples->vct 0 len stereo-snd 0))
      (vct-add! out-data1 (samples->vct 0 len stereo-snd 1))
      (vct->samples 0 len out-data0 stereo-snd 0)
      (vct->samples 0 len out-data1 stereo-snd 1))))

;;; if there's no envelope (just a position):
(define place1
  (lambda (mono-snd stereo-snd deg)
    (let* ((len (frames mono-snd))
	   (pos (/ deg 90.0))
	   (in-data0 (samples->vct 0 len mono-snd))
	   (in-data1 (samples->vct 0 len mono-snd))
	   (out-data0 (samples->vct 0 len stereo-snd 0))
	   (out-data1 (samples->vct 0 len stereo-snd 1)))
      (vct-scale! in-data0 (- 1.0 pos))
      (vct-scale! in-data1 pos)
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

(define (fft-squelch squelch)
  ;; fft entire sound, remove all energy below squelch, unfft
  (let* ((sr (srate))
	 (len (frames))
	 (fsize (expt 2 (ceiling (/ (log len) (log 2.0)))))
	 (rdata (samples->vct 0 fsize))
	 (idata (make-vct fsize))
	 (fsize2 (/ fsize 2))
	 (scaler 1.0))
    (fft rdata idata 1)
    (let ((vr (vct-copy rdata))
	  (vi (vct-copy idata)))
      (rectangular->polar vr vi)
      (set! scaler (vct-peak vr)))
    (set! squelch (* squelch scaler))
    (do ((i 0 (1+ i))
	 (j (- fsize 1) (1- j)))
	((= i fsize2))
      (let ((magnitude (sqrt (+ (* (vct-ref rdata i) (vct-ref rdata i)) (* (vct-ref idata i) (vct-ref idata i))))))
	(if (< magnitude squelch)
	    (begin
	      (vct-set! rdata i 0.0)
	      (vct-set! rdata j 0.0)
	      (vct-set! idata i 0.0)
	      (vct-set! idata j 0.0)))))
    (fft rdata idata -1)
    (vct-scale! rdata (/ 1.0 fsize))
    (set-samples 0 (1- len) rdata)
    scaler))
    
(define (fft-env-data fft-env)
  ;; apply fft-env as spectral env to current sound, returning vct of new data
  (let* ((sr (srate))
	 (len (frames))
	 (fsize (expt 2 (ceiling (/ (log len) (log 2.0)))))
	 (rdata (samples->vct 0 fsize))
	 (idata (make-vct fsize))
	 (fsize2 (/ fsize 2))
	 (e (make-env fft-env :end (1- fsize2))))
    (fft rdata idata 1)
    (do ((i 0 (1+ i))
	 (j (1- fsize) (1- j)))
	((= i fsize2))
      (let ((val (env e)))
	(vct-set! rdata i (* val (vct-ref rdata i)))
	(vct-set! idata i (* val (vct-ref idata i)))
	(vct-set! rdata j (* val (vct-ref rdata j)))
	(vct-set! idata j (* val (vct-ref idata j)))))
    (fft rdata idata -1)
    (vct-scale! rdata (/ 1.0 fsize))))

(define (fft-env-edit fft-env)
  ;; edit current chan using fft-env
  (set-samples 0 (1- (frames)) (fft-env-data fft-env)))

(define (fft-env-interp env1 env2 interp)
  ;; interpolate between two fft-filtered versions (env1 and env2 are the spectral envelopes) following interp (an env between 0 and 1)
  (let* ((data1 (fft-env-data env1))
	 (data2 (fft-env-data env2))
	 (len (frames))
	 (new-data (make-vct len))
	 (e (make-env interp :end (1- len))))
    (do ((i 0 (1+ i)))
	((= i len))
      (let ((pan (env e)))
	(vct-set! new-data i 
		  (+ (* (- 1.0 pan) (vct-ref data1 i))
		     (* pan (vct-ref data2 i))))))
    (set-samples 0 (1- len) new-data)))

(define (fft-smoother cutoff start samps snd chn)
  "use fft-filtering to smooth a section"
  (let* ((fftpts (inexact->exact (expt 2 (ceiling (/ (log (1+ samps)) (log 2.0))))))
	 (rl (make-vct fftpts))
	 (im (make-vct fftpts))
	 (top (inexact->exact (floor (* fftpts cutoff)))))
    (samples->vct start samps snd chn rl)
    (let* ((old0 (vct-ref rl 0))
	   (old1 (vct-ref rl (1- samps)))
	   (oldmax (vct-peak rl)))
      (fft rl im 1)
      (do ((i top (1+ i)))
	  ((= i fftpts))
	(vct-set! rl i 0.0)
	(vct-set! im i 0.0))
      (fft rl im -1)
      (vct-scale! rl (/ 1.0 fftpts))
      (let ((newmax (vct-peak rl)))
	(if (= newmax 0.0)
	    rl
	    (begin
	      (if (> (/ oldmax newmax) 1.5)
		  (vct-scale! rl (/ oldmax newmax)))
	      (let* ((new0 (vct-ref rl 0))
		     (new1 (vct-ref rl (1- samps)))
		     (offset0 (- old0 new0))
		     (offset1 (- old1 new1))
		     (incr (if (= offset1 offset0) 0.0 (/ (- offset1 offset0) samps))))
		(do ((i 0 (1+ i))
		     (trend offset0 (+ trend incr)))
		    ((= i samps))
		  (vct-set! rl i (+ (vct-ref rl i) trend)))
		rl)))))))

; (vct->samples (cursor) 400 (fft-smoother .1 (cursor) 400 0 0))


;;; -------- comb-filter

(define comb-filter
  (lambda (scaler size)
    (let ((delay-line (make-vector size 0.0))
	  (delay-loc 0))
      (lambda (x)
	(let ((result (vector-ref delay-line delay-loc)))
	  (vector-set! delay-line delay-loc (+ x (* scaler result)))
	  (set! delay-loc (1+ delay-loc))
	  (if (= delay-loc size) (set! delay-loc 0))
	  result)))))

; (map-chan (comb-filter .8 32))

;;; the same thing using the CLM module is:

(define comb-filter 
  (lambda (scaler size)
    (let ((cmb (make-comb scaler size)))
      (lambda (x) 
	(comb cmb x)))))

;;; by using filters at harmonically related sizes, we can get chords:

(define comb-chord
  (lambda (scaler size amp)
    (let ((c1 (make-comb scaler size))
	  (c2 (make-comb scaler (* size .75)))
	  (c3 (make-comb scaler (* size 1.2))))
      (lambda (x) 
	(* amp (+ (comb c1 x) (comb c2 x) (comb c3 x)))))))

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
      (lambda (x)
	(comb cmb x (env penv))))))

; (map-chan (zcomb .8 32 '(0 0 1 10)))

;;; or notch: 

(define notch-filter 
  (lambda (scaler size)
    (let ((cmb (make-notch scaler size)))
      (lambda (x) 
	(notch cmb x)))))

; (map-chan (notch-filter .8 32))

;;; or formant:

(define formant-filter
  (lambda (radius frequency)
    (let ((frm (make-formant radius frequency)))
      (lambda (x) 
	(formant frm x)))))

; (map-chan (formant-filter .99 2400))

;;; in cases this simple (involving just a straight call on a single filtering generator),
;;;   it is much faster to use filter-sound:
;;;   (filter-sound (make-formant .99 2400))

;;; to impose several formants, just add them in parallel:

(define formants
  (lambda (r1 f1 r2 f2 r3 f3)
    (let ((fr1 (make-formant r1 f1))
	  (fr2 (make-formant r2 f2))
	  (fr3 (make-formant r3 f3)))
      (lambda (x)
	(+ (formant fr1 x)
	   (formant fr2 x)
	   (formant fr3 x))))))

; (map-chan (formants .99 900 .98 1800 .99 2700))

;;; to get a moving formant:

(define moving-formant
  (lambda (radius move)
    (let ((frm (make-formant radius (cadr move)))
	  (menv (make-env :envelope move :end (frames))))
      (lambda (x)
	(let ((val (formant frm x)))
	  (set! (mus-frequency frm) (env menv))
	  val)))))

; (map-chan (moving-formant .99 '(0 1200 1 2400)))

(define osc-formants
  ;; set up any number of independently oscillating formants
  (lambda (radius bases amounts freqs)
    (let* ((len (length bases))
	   (frms (make-vector len))
	   (oscs (make-vector len)))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(vector-set! frms i (make-formant radius (list-ref bases i)))
	(vector-set! oscs i (make-oscil (list-ref freqs i))))
      (lambda (x)
	(let ((val 0.0))
	  (do ((i 0 (1+ i)))
	      ((= i len))
	    (let ((frm (vector-ref frms i)))
	      (set! val (+ val (formant frm x)))
	      (set! (mus-frequency frm) 
		    (+ (list-ref bases i) ;(this is not optimized for speed!)
		       (* (list-ref amounts i) 
			  (oscil (vector-ref oscs i)))))))
	  val)))))

;(map-chan (osc-formants .99 '(400 800 1200) '(400 800 1200) '(4 2 3)))


;;; -------- echo

(define echo 
  (lambda (scaler secs)
    (let ((del (make-delay (round (* secs (srate))))))
      (lambda (inval)
	(+ inval (delay del (* scaler (+ (tap del) inval))))))))

; (map-chan (echo .5 .5) 0 44100)

;;; here is a version that modulates the echos:

(define zecho 
  (lambda (scaler secs frq amp)
    (let* ((os (make-oscil frq))
	   (len (round (* secs (srate))))
	   (del (make-delay len :max-size (+ len amp 1))))
      (lambda (inval)
	(+ inval 
	   (delay del 
		  (* scaler (+ (tap del) inval))
		  (* amp (oscil os))))))))

; (map-chan (zecho .5 .75 6 10.0) 0 65000)

;;; or low-pass filter the echoes:

(define flecho 
  (lambda (scaler secs)
    (let* ((flt (make-fir-filter :order 4 :xcoeffs (list->vct '(.125 .25 .25 .125))))
	   (del (make-delay  (round (* secs (srate))))))
      (lambda (inval)
	(+ inval 
	   (delay del 
		  (fir-filter flt (* scaler (+ (tap del) inval)))))))))

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
	(* (oscil os (env genv)) inval)))))

; (map-chan (ring-mod 100 '(0 0 1 0)))
; (map-chan (ring-mod 10 (list 0 0 1 (hz->radians 100))))

(define am 
  (lambda (freq) 
    (let ((os (make-oscil freq))) 
      (lambda (inval) 
	(amplitude-modulate 1.0 inval (oscil os))))))

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
    (let* ((tbl (vct -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250 
		     0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000)))
      ;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")
      (lambda (inval)
	(let ((index (+ 8.0 (* 8.0 inval))))
	  (array-interp tbl index 17))))))

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
  (lambda (rate . rest)
    (let* ((gr (make-granulate :expansion rate))
	   ;; this can be improved by messing with make-granulate's hop and length args
	   (sr (make-src :srate rate))
	   (vsize 1024)
	   (vbeg 0)
	   (v (samples->vct 0 vsize))
	   (snd (if (> (length rest) 0) (car rest) 0))
	   (chn (if (> (length rest) 1) (cadr rest) 0))
	   (inctr 0))
      (lambda (inval)
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
				    (samples->vct vbeg vsize snd chn v)))
			      val)))))))))

;;; the next (expsnd) changes the tempo according to an envelope; the new duration
;;; will depend on the expansion envelope -- we integrate it to get
;;; the overall expansion, then use that to decide the new length.

(define expsnd
  (lambda (gr-env)
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
    (let* ((dur (/ (* (/ (frames) (srate)) (integrate-envelope gr-env 0.0)) (max-x gr-env)))
	   (gr (make-granulate :expansion (cadr gr-env) :jitter 0))
	   (ge (make-env :envelope gr-env :duration dur))
	   (sound-len (round (* (srate) dur)))
	   (len (max sound-len (frames)))
	   (out-data (make-vct len))
	   (sf (make-sample-reader)))
      (vct-map! out-data (lambda ()
			   (let ((val (granulate gr (lambda (dir) (next-sample sf)))))
			     (set! (mus-increment gr) (env ge))
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
	  (* amp (formant-bank spectr formants inval)))))))

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
		  (if (c-g?)               ; let interface run
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
	(if (> max-samp 1.0) (set! (y-bounds snd1) (list (- max-samp) max-samp)))
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
		  (* volume (delay outdel1 all-sums))))))
       0 (round (* dur (srate)))))))

; (jc-reverb 2.0 #f .1 #f)


;;; -------- scissor-tailed flycatcher
;;;
;;; mix a scissor-tailed flycatcher call into the current sound
;;; see bird.scm for lots more birds

(define bigbird
  (lambda (start dur frequency freqskew amplitude
		 freq-envelope amp-envelope partials
		 lpcoeff)
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
;;; a version treating the entire violin as a generator is in fmv.scm.

(define pi 3.141592653589793)

(define fm-violin 
  (lambda* (startime dur frequency amplitude #:key
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
	    #:allow-other-keys)
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
	    (mix-vct (vct-scale! bsamps (locsig-ref loc 1)) beg #f 1 #f)
	    (mix-vct (vct-scale! out-data (locsig-ref loc 0)) beg #f 0 #f))
	  (mix-vct out-data beg #f 0 #f)))))

; (fm-violin 0 1 440 .1 :fm-index 2.0)


;;; -------- FOF example

(define two-pi (* 2 3.141592653589793))

(define fofins 
  (lambda* (beg dur frq amp vib f0 a0 f1 a1 f2 a2 #:optional (ae '(0 0 25 1 75 1 100 0)))
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
;;;   see pvoc.scm for a generator-oriented version

(define ifloor (lambda (n) (inexact->exact (floor n))))
(define pi 3.141592653589793)

(define pvoc
  (lambda* (#:key
	   (fftsize 512) (overlap 4) (time 1.0)
	   (pitch 1.0) (gate 0.0) (hoffset 0.0)
	   (snd 0) (chn 0))
    "(pvoc &key fftsize overlap time pitch gate hoffset) applies the phase vocoder
  algorithm to the current sound (i.e. fft analysis, oscil bank resynthesis). 'pitch'
  specifies the pitch transposition ratio, 'time' - specifies the time dilation ratio,
  'gate' specifies a resynthesis gate in dB (partials with amplitudes lower than
  the gate value will not be synthesized), 'hoffset is a pitch offset in Hz."
    (let* ((len (frames))
	   (filptr 0)           ; index into the file
	   (pi2 (* 2 pi))       ; handy constant
	   (sr (srate))
	   (N fftsize)          ; fft size
	   (N2 (ifloor (/ N 2)))  ;; half the fft size
	   (Nw fftsize) ;; window size -- currently restricted to the fftsize
	   (D (ifloor (/ fftsize overlap))) ; decimation factor (how often do we take an fft)
	   (interp (* (ifloor (/ fftsize overlap)) time)) ; interpolation factor how often do we synthesize
	   ;; take a resynthesis gate specificed in dB, convert to linear amplitude
	   (syngate (if (= 0.0 gate) 0.0 (expt 10 (/ (- (abs gate)) 20))))
	   (poffset (hz->radians hoffset))
	   (window (make-fft-window hamming-window fftsize))
	   (fdr (make-vct N))     ; buffer for real fft data
	   (fdi (make-vct N))     ; buffer for imaginary fft data
	   (lastphase (make-vct N2)) ;; last phase change
	   (lastamp (make-vct N2)) ;; last sampled amplitude
	   (lastfreq (make-vct N2)) ;; last sampled frequency
	   (ampinc (make-vct N2)) ;; amplitude interpolation increment
	   (freqinc (make-vct N2)) ;; frequency interpolation increments
	   ;; expresses the fundamental in terms of radians per output sample
	   (fundamental (/ pi2 N))
	   (output interp)      ; count of samples that have been output
	   (resynth-oscils (make-vector N2))  ; synthesis oscillators
	   (nextpct 10.0)       ; how often to print out the percentage complete message
	   (outlen (ifloor (* time len)))
	   (out-data (make-vct (max len outlen)))
	   (in-data (samples->vct 0 (* N 2) snd chn))
	   (in-data-beg 0))
      ;; setup oscillators
      (do ((i 0 (1+ i)))
	  ((= i N2))
	(vector-set! resynth-oscils i (make-oscil :frequency 0)))
      (vct-scale! window (/ 2.0 (* 0.54 fftsize))) ;den = hamming window integrated
      (call-with-current-continuation
       (lambda (break)
	 (do ((i 0 (1+ i)))
	     ((>= i outlen))
	   ;; begin the master run loop
	   (if (>= output interp) ;; if all the samples have been output then do the next frame
	       (let ((buffix (modulo filptr N)))
					; buffix is the index into the input buffer
					; it wraps around circularly as time increases in the input
		 (if (c-g?) (break "interrupted"))
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
		       (samples->vct in-data-beg (* N 2) snd chn in-data)))
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
	(let ((val (fir-filter flt x))
	      (xcof (mus-data flt)))
	  (do ((i 0 (1+ i)))
	      ((= i 8))
	    (vct-set! xcof i (env (vector-ref es i))))
	  val)))))

;(map-chan (fltit))

;;; for something this simple (like a notch filter), we can use a two-zero filter:
;
;(define flt (make-zpolar .99 550.0))
;
;;; this is a strong notch filter centered at 550 Hz
;
;(map-chan (lambda (x) (two-zero flt x)))
;
;;; similarly make-ppolar/two-pole (or better, make-formant)
;;; can be used for resonances.





;;; -------- swap selection chans

(define swap-selection-channels 
  (lambda ()
    "(swap-selection-channels) swaps the currently selected data's channels"
    (define find-selection-sound 
      (lambda (not-this)
	(catch 'return ; could also use call-with-current-continuation
	  (lambda ()
	    (apply map (lambda (snd chn)
			 (if (and (selection-member? snd chn)
				  (or (null? not-this)
				      (not (= snd (car not-this)))
				      (not (= chn (cadr not-this)))))
			     (throw 'return (list snd chn))))
		  (all-chans)))
	  (lambda (tag val) val))))
    (if (selection?)
	(if (= (selection-chans) 2)
	    (let* ((beg (selection-position))
		   (len (selection-length))
		   (snd-chn0 (find-selection-sound '()))
		   (snd-chn1 (find-selection-sound snd-chn0)))
	      (if snd-chn1
		  (swap-channels (car snd-chn0) (cadr snd-chn0) (car snd-chn1) (cadr snd-chn1) beg len)
		  (report-in-minubuffer "swap-selection-channels needs two channels two swap")))
	    (report-in-minibuffer "swap-selection-channels needs a stereo selection"))
	(report-in-minibuffer "no active selection"))))


;;; -------- sound interp
;;;
;;; make-sound-interp sets up a sound reader that reads a channel at an arbitary location,
;;;   interpolating between samples if necessary, the corresponding "generator" is sound-interp

(define make-sound-interp 
  (lambda (start . rest)
    "(make-sound-interp start &optional snd chn) -> an interpolating reader for snd's channel chn"
    (let* ((snd (if (> (length rest) 0) (car rest) #f))
	   (chn (if (> (length rest) 1) (cadr rest) #f))
	   (bufsize 2048)
	   (buf4size 128)
	   (data (samples->vct start bufsize snd chn))
	   (curbeg start)
	   (curend (+ start bufsize)))
      (lambda (loc)
	(if (< loc curbeg)
	    (begin
	      ;; get previous buffer
	      (set! curbeg (+ buf4size (- loc bufsize)))
	      (set! curend (+ curbeg bufsize))
	      (samples->vct curbeg bufsize snd chn data))
	    (if (> loc curend)
		(begin
		  ;; get next buffer
		  (set! curbeg (- loc buf4size))
		  (set! curend (+ curbeg bufsize))
		  (samples->vct curbeg bufsize snd chn data))))
	(array-interp data (- loc curbeg) bufsize)))))

(define sound-interp ;make it look like a clm generator
  (lambda (func loc) 
    "(sound-interp func loc) -> sample at loc (interpolated if necessary) from func created by make-sound-interp"
    (func loc)))

(define test-interp
  (lambda (freq)
    ;; use a sine wave to lookup the current sound
    (let ((osc (make-oscil :frequency freq :initial-phase (+ pi (/ pi 2))))
	  (reader (make-sound-interp 0 0 0)) 
	  (len (frames 0 0)))
      (map-chan (lambda (val) 
		  (sound-interp reader (* len (+ 0.5 (* 0.5 (oscil osc))))))))))

;;; (test-interp 0.5)

;; env-sound-interp takes an envelope that goes between 0 and 1 (y-axis), and a time-scaler
;;   (1.0 = original length) and returns a new version of the data in the specified channel
;;   that follows that envelope (that is, when the envelope is 0 we get sample 0, when the
;;   envelope is 1 we get the last sample, envelope = .5 we get the middle sample of the 
;;   sound and so on. (env-sound-interp '(0 0 1 1)) will return a copy of the
;;   current sound; (env-sound-interp '(0 0 1 1 2 0) 2.0) will return a new sound 
;;   with the sound copied first in normal order, then reversed.  src-sound with an
;;   envelope could be used for this effect, but it is much more direct to apply the
;;   envelope to sound sample positions.

(define env-sound-interp
  (lambda (envelope . rest)
    "(env-sound-interp env &optional (time-scale 1.0) snd chn) reads snd's channel chn according to env and time-scale"
    ;; since the old/new sounds can be any length, we'll write a temp file rather than trying to use map-chan or vct-map!
    (let* ((time-scale (if (not (null? rest)) (car rest) 1.0))
	   (snd (if (> (length rest) 1) (cadr rest) #f))
	   (chn (if (> (length rest) 2) (caddr rest) #f))
	   (len (frames snd chn))
	   (newlen (inexact->exact (* time-scale len)))
	   (reader (make-sound-interp 0 snd chn))
	   (read-env (make-env envelope :end newlen :scaler len))
	   (tempfilename (snd-tempnam))
	   (fil (mus-sound-open-output tempfilename (srate snd) 1 #f mus-next ""))
	   ;; #f as data-format -> format compatible with sndlib (so no data translation is needed)
	   (bufsize 8192)
	   (data (make-sound-data 1 bufsize))
	   (data-ctr 0))
      (do ((i 0 (1+ i)))
	  ((= i newlen))
	(sound-data-set! data 0 data-ctr (reader (env read-env)))
	(set! data-ctr (1+ data-ctr))
	(if (= bufsize data-ctr)
	    (begin
	      (mus-sound-write fil 0 (1- bufsize) 1 data)
	      (set! data-ctr 0))))
      (if (> data-ctr 0)
	  (mus-sound-write fil 0 (1- data-ctr) 1 data))
      (mus-sound-close-output fil (* 4 newlen))
      ;; #t trunc arg to set-samples shortens the sound as needed
      (set-samples 0 newlen tempfilename snd chn #t))))



;;; -------- add date and time to title bar
;;;
;;; The window manager's property that holds the Snd window's title is WM_NAME,
;;;   we can use the change-property function (used normally for CLM/Snd communication)
;;;   to reset this value.  The Snd window's identifier is SND_VERSION.
;;;   Here we're also using the #t argument to short-file-name to get a list of all current sounds.

(define retitle-time (* 60 1000)) ;once a minute

(define title-with-date
  (lambda ()
    (let ((names (short-file-name #t)))
      (change-property "SND_VERSION" "WM_NAME"
		       (format #f "snd (~A)~A"
			       (strftime "%d-%b %H:%M %Z" (localtime (current-time)))
			       (if (null? names)
				   ""
				   (format #f ":~{~A~^, ~}" names))))
      (if (> retitle-time 0)
	  (in retitle-time title-with-date)))))

;(title-with-date)
;  -- this line starts the new window title handler which runs until Snd is exited or retitle-time is set to 0


;;; -------- with-sound for Snd!
;;;
;;; this is just a bare beginning, but it's the basic idea...
;;;
;;; in Common Lisp this is essentially
;;;    (defmacro with-sound ((&key (srate 22050) ...) &body body) (let (...) ,.body))
;;; so that a call looks like 
;;;    (with-sound (:srate 44100) (fm-violin 0 1 440 .1))

(defmacro with-sound (args . body) 
  `((lambda* (#:key (srate 22050)
		    (output "test.snd")
		    (channels 1)
		    (explode #f))
      (let ((old-srate (mus-srate)))
	(dynamic-wind
	 (lambda ()
	   (set! (mus-srate) srate))
	 (lambda () 
	   (if (find-sound output) (close-sound (find-sound output)))
	   (new-sound output (default-output-type) (default-output-format) srate channels)
	   ,@body)
	 (lambda ()
	   (set! (mus-srate) old-srate)))))
    ,@args))

;;; now instrument calls (outa etc) need to write (mix) to the currently selected sounds,
;;;   or to a newly opened sound

;;; here's a better version courtesy of Kalle Olavi Niemitalo
;;; but it doesn't seem to work in Guile 1.4 (it needs 1.4.1)
;;;
;;;(define* (with-sound-helper thunk #:key (srate 22050) (explode #f))
;;;  (let ((old-srate (mus-srate)))
;;;    (dynamic-wind (lambda () (set! (mus-srate) srate))
;;;                  thunk
;;;                  (lambda () (set! (mus-srate) old-srate)))))
;;;
;;;(defmacro with-sound (args . body)
;;;  `(with-sound-helper (lambda () ,@body)
;;;                      ,@args))
;;;
;;; see ws.scm for an elaboration of this version.
;;;
;;; this could save (current-load-port) somewhere, and if it's not #f, report
;;;   port-filename and port-line on it in case of error (since backtrace sometimes
;;;   seems to get confused)


;;; -------- how to get 'display' to write to Snd's listener
;;;
;;; scheme's display function writes to current-output-port which defaults to stdout.
;;; This is a bit annoying since we'd like everything to go to the listener in many cases
;;; so, to bring that about we need to define our own "soft-port" as follows:

(define stdout (current-output-port)) ;save it in case we want to go back to it
(define snd-out
  (make-soft-port
   (vector                      ;soft port is a vector of procedures:
    (lambda (c) (snd-print c))  ;  procedure accepting one character for output 
    (lambda (s) (snd-print s))  ;  procedure accepting a string for output 
    (lambda () #f)              ;  thunk for flushing output (not needed here)
    #f                          ;  thunk for getting one character (also not needed)
    (lambda () #f))             ;  thunk for closing port -- hmm should this go back to the previous?
   "w"))

;(set-current-output-port snd-out)

;;; Perhaps this should be built-in, allowing us to merge snd-help and help etc.



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


;;; -------- pluck
;;;
;;; The Karplus-Strong algorithm as extended by David Jaffe and Julius Smith -- see 
;;;  Jaffe and Smith, "Extensions of the Karplus-Strong Plucked-String Algorithm"
;;;  CMJ vol 7 no 2 Summer 1983, reprinted in "The Music Machine".
;;;  translated from CLM's pluck.ins

(define (pluck start dur freq amp weighting lossfact)

  ;; DAJ explains weighting and lossfact as follows:
  ;; weighting is the ratio of the once-delayed to the twice-delayed samples.  It defaults to .5=shortest decay.
  ;;     anything other than .5 = longer decay.  Must be between 0 and less than 1.0.
  ;; lossfact can be used to shorten decays.  Most useful values are between .8 and 1.0.

  (define (getOptimumC S o p)
    (let* ((pa (* (/ 1.0 o) (atan (* S (sin o)) (+ (- 1.0 S) (* S (cos o))))))
	   (tmpInt (inexact->exact (floor (- p pa))))
	   (pc (- p pa tmpInt)))
      (if (< pc .1)
	  (do ()
	      ((>= pc .1))
	    (set! tmpInt (- tmpInt 1))
	    (set! pc (+ pc 1.0))))
      (list tmpInt (/ (- (sin o) (sin (* o pc))) (sin (+ o (* o pc)))))))

  (define (tuneIt f s1)
    (let* ((p (/ (srate) f))	;period as float
	   (s (if (= s1 0.0) 0.5 s1))
	   (o (hz->radians f))
	   (vals (getOptimumC s o p))
	   (T1 (car vals))
	   (C1 (cadr vals))
	   (vals1 (getOptimumC (- 1.0 s) o p))
	   (T2 (car vals1))
	   (C2 (cadr vals1)))
      (if (and (not (= s .5))
	       (< (abs C1) (abs C2)))
	  (list (- 1.0 s) C1 T1)
	(list s C2 T2))))

  (let* ((vals (tuneIt freq weighting))
	 (wt0 (car vals))
	 (c (cadr vals))
	 (dlen (caddr vals))
	 (beg (inexact->exact (floor (* start (srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (srate))))))
	 (lf (if (= lossfact 0.0) 1.0 (min 1.0 lossfact)))
	 (wt (if (= wt0 0.0) 0.5 (min 1.0 wt0)))
	 (tab (make-vct dlen))
	 ;; get initial waveform in "tab" -- here we can introduce 0's to simulate different pick
	 ;; positions, and so on -- see the CMJ article for numerous extensions.  The normal case
	 ;; is to load it with white noise (between -1 and 1).
	 (val 0.0)
	 (allp (make-one-zero (* lf (- 1.0 wt)) (* lf wt)))
	 (feedb (make-one-zero c 1.0)) ;or (feedb (make-one-zero 1.0 c))
	 (ctr 0)
	 (out-data (make-vct (+ 1 (- end beg)))))
    (do ((i 0 (1+ i)))
	((= i dlen))
      (vct-set! tab i (- 1.0 (random 2.0))))
    (vct-map! out-data (lambda ()
			 (let ((val (vct-ref tab ctr)))	;current output value
			   (vct-set! tab ctr (* (- 1.0 c) 
						(one-zero feedb 
							  (one-zero allp val))))
			   (set! ctr (+ ctr 1))
			   (if (>= ctr dlen) (set! ctr 0))
			   (* amp val))))
    (mix-vct out-data beg #f 0 #f)
    (update-time-graph)))

;(pluck .01 1 330 .3 .96 0 0 0)


;;; -------- mlbvoi
;;;
;;; translation from MUS10 of Marc LeBrun's waveshaping voice instrument (using FM here)
;;; this version translated (and simplified slightly) from CLM's mlbvoi.ins

(define (vox beg dur freq amp ampfun freqfun freqscl voxfun index vibscl)
  (let ((formants
	 '((I 390 1990 2550)  (E 530 1840 2480)  (AE 660 1720 2410)
	   (UH 520 1190 2390) (A 730 1090 2440)  (OW 570 840 2410)
	   (U 440 1020 2240)  (OO 300 870 2240)  (ER 490 1350 1690)
	   (W 300 610 2200)   (LL 380 880 2575)  (R 420 1300 1600)
	   (Y 300 2200 3065)  (EE 260 3500 3800) (LH 280 1450 1600)
	   (L 300 1300 3000)  (I2 350 2300 3340) (B 200 800 1750)
	   (D 300 1700 2600)  (G 250 1350 2000)  (M 280 900 2200)
	   (N 280 1700 2600)  (NG 280 2300 2750) (P 300 800 1750)
	   (T 200 1700 2600)  (K 350 1350 2000)  (F 175 900 4400)
	   (TH 200 1400 2200) (S 200 1300 2500)  (SH 200 1800 2000)
	   (V 175 1100 2400)  (THE 200 1600 2200)(Z 200 1300 2500)
	   (ZH 175 1800 2000) (ZZ 900 2400 3800) (VV 565 1045 2400))))
	;;formant center frequencies for a male speaker

    (define (find-phoneme phoneme forms)
      (if (eq? phoneme (car (car forms)))
	  (cdr (car forms))
	(find-phoneme phoneme (cdr forms))))
    
    (let ((f1 '())
	  (f2 '())
	  (f3 '())
	  (len (length voxfun)))
      (do ((i (- len 1) (- i 2)))
	  ((<= i 0))
	(let ((phon (find-phoneme (list-ref voxfun i) formants))
	      (x (list-ref voxfun (- i 1))))
	  (set! f1 (cons (car phon) f1))
	  (set! f1 (cons x f1))
	  (set! f2 (cons (cadr phon) f2))
	  (set! f2 (cons x f2))
	  (set! f3 (cons (caddr phon) f3))
	  (set! f3 (cons x f3))))
      
      (let* ((start (inexact->exact (floor (* (srate) beg))))
	     (end (+ start (inexact->exact (floor (* (srate) dur)))))
	     (car-os (make-oscil :frequency 0))
	     (of0 (make-oscil :frequency 0))
	     (of1 (make-oscil :frequency 0))
	     (of2 (make-oscil :frequency 0))
	     (of3 (make-oscil :frequency 0))
	     (of4 (make-oscil :frequency 0))
	     (of5 (make-oscil :frequency 0))
	     (ampf (make-env :envelope ampfun :scaler amp :duration dur))
	     (frmf1 (make-env :envelope f1 :duration dur))
	     (frmf2 (make-env :envelope f2 :duration dur))
	     (frmf3 (make-env :envelope f3 :duration dur))
	     (freqf (make-env :envelope freqfun :duration dur
			      :scaler (* freqscl freq)
			      :offset freq))
	     (per-vib (make-triangle-wave :frequency 6 :amplitude (* freq vibscl)))
	     (ran-vib (make-rand-interp :frequency 20 :amplitude (* freq .01)))
	     (car 0.0)
	     (frq 0.0)
	     (frm-int 0)
	     (frm0 0.0)
	     (frm 0.0)
	     (frq0 0.0) (frq1 0.0) (frq2 0.0) (frq3 0.0) (frq4 0.0) (frq5 0.0)
	     (amp0 0.0) (amp1 0.0) (amp2 0.0) (amp3 0.0) (amp4 0.0) (amp5 0.0)
	     (out-data (make-vct (+ 1 (- end start)))))
	(vct-map! out-data
		  (lambda ()
		    (set! frq (+ (env freqf) (triangle-wave per-vib) (rand-interp ran-vib)))
		    (set! car (* index (oscil car-os (hz->radians frq))))
		    (set! frm (env frmf1))
		    (set! frm0 (/ frm frq))
		    (set! frm-int (inexact->exact (floor frm0)))
		    (if (even? frm-int)
			(begin
			 (set! frq0 (hz->radians (* frm-int frq)))
			 (set! frq1 (hz->radians (* (+ frm-int 1) frq)))
			 (set! amp1 (- frm0 frm-int))
			 (set! amp0 (- 1.0 amp1)))
		      (begin
		       (set! frq1 (hz->radians (* frm-int frq)))
		       (set! frq0 (hz->radians (* (+ frm-int 1) frq)))
		       (set! amp0 (- frm0 frm-int))
		       (set! amp1 (- 1.0 amp0))))
		    (set! frm (env frmf2))
		    (set! frm0 (/ frm frq))
		    (set! frm-int (inexact->exact (floor frm0)))
		    (if (even? frm-int)
			(begin
			 (set! frq2 (hz->radians (* frm-int frq)))
			 (set! frq3 (hz->radians (* (+ frm-int 1) frq)))
			 (set! amp3 (- frm0 frm-int))
			 (set! amp2 (- 1.0 amp3)))
		      (begin
		       (set! frq3 (hz->radians (* frm-int frq)))
		       (set! frq2 (hz->radians (* (+ frm-int 1) frq)))
		       (set! amp2 (- frm0 frm-int))
		       (set! amp3 (- 1.0 amp2))))
		    (set! frm (env frmf3))
		    (set! frm0 (/ frm frq))
		    (set! frm-int (inexact->exact (floor frm0)))
		    (if (even? frm-int)
			(begin
			 (set! frq4 (hz->radians (* frm-int frq)))
			 (set! frq5 (hz->radians (* (+ frm-int 1) frq)))
			 (set! amp5 (- frm0 frm-int))
			 (set! amp4 (- 1.0 amp5)))
		      (begin
		       (set! frq5 (hz->radians (* frm-int frq)))
		       (set! frq4 (hz->radians (* (+ frm-int 1) frq)))
		       (set! amp4 (- frm0 frm-int))
		       (set! amp5 (- 1.0 amp4))))
		    (* (env ampf)
		       (+ (* .8 (+ (* amp0 (oscil of0 (+ frq0 (* .2 car))))
				   (* amp1 (oscil of1 (+ frq1 (* .2 car))))))
			  (* .15 (+ (* amp2 (oscil of2 (+ frq2 (* .5 car))))
				    (* amp3 (oscil of3 (+ frq3 (* .5 car))))))
			  (* .05 (+ (* amp4 (oscil of4 (+ frq4 (* 1.0 car))))
				    (* amp5 (oscil of5 (+ frq5 (* 1.0 car))))))))))
	(mix-vct out-data beg #f 0 #f)
	(update-time-graph)))))
  
;;; (vox 0 2 110 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 UH 25 UH 35 ER 65 ER 75 UH 100 UH) .025 .1)
;;; (vox 0 2 170 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 E 25 AE 35 ER 65 ER 75 I 100 UH) .05 .1)
;;; (vox 0 2 300 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 I 5 OW 10 I 50 AE 100 OO) .02 .1)
;;; (vox 0 5 600 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 I 5 OW 10 I 50 AE 100 OO) .01 .1)
  

;;; -------- filtered-env 

(define (filtered-env e)
  ;; amplitude and low-pass amount move together
  ;; when env is at 1.0, no filtering, as env moves to 0.0, low-pass gets more severe
  (let* ((samps (frames))
	 (flt (make-one-pole 1.0 0.0))
	 (amp-env (make-env e :end (1- samps))))
    (map-chan
     (lambda (val)
       (let ((env-val (env amp-env)))
	 (set! (mus-a0 flt) env-val)
	 (set! (mus-b1 flt) (- env-val 1.0))
	 (one-pole flt (* env-val val)))))))


;;; -------- multi-colored rxvt printout
;;;
;;; if you're using display to write to rxvt, you can use the latter's escape sequences
;;;   for things like multi-colored text:

(define red-text (format #f "~C[31m" #\esc))
(define normal-text (format #f "~C[0m" #\esc))
;(display (format #f "~Athis is red!~Abut this is not" red-text normal-text))

;;; there are a bunch of these:

(define black-on-red-text (format #f "~C[30m~C[41m" #\esc #\esc))

;;; or perhaps more convenient:

(define black-fg (format #f "~C[30m" #\esc))  (define black-bg (format #f "~C[40m" #\esc))
(define red-fg (format #f "~C[31m" #\esc))    (define red-bg (format #f "~C[41m" #\esc))
(define green-fg (format #f "~C[32m" #\esc))  (define green-bg (format #f "~C[42m" #\esc))
(define yellow-fg (format #f "~C[33m" #\esc)) (define yellow-bg (format #f "~C[43m" #\esc))
(define blue-fg (format #f "~C[34m" #\esc))   (define blue-bg (format #f "~C[44m" #\esc))
;;; etc (magenta: 35 cyan: 36 white: 37 default: 39)

(define bold-text (format #f "~C[1m" #\esc))       (define unbold-text (format #f "~C[22m" #\esc))  
(define underline-text (format #f "~C[4m" #\esc))  (define ununderline-text (format #f "~C[24m" #\esc))  
(define blink-text (format #f "~C[5m" #\esc))      (define unblink-text (format #f "~C[25m" #\esc))  

;(display (format #f "~A~Ahiho~Ahiho" yellow-bg red-fg normal-text))



;;; -------- locsig using fancier placement choice
;;;

(define (make-cpp-locsig . args)
  (define (get-cpp-scalers degree)
    (let* ((magic (/ (sqrt 2) 2))
	   ;; although we (in clm) specify the degree from 0 - 90, for the sake
	   ;; of the calculation it's from -45 to 45 with 0, not 45, being the
	   ;; centre. 
	   ;; (this taken from panning.lsp by Michael Edwards which is based on Curt Roads' suggestions)
	   (radians (degrees->radians (- 45.0 degree)))
	   (cos (cos radians))
	   (sin (sin radians)))
      (list (* magic (+ cos sin))
	    (* magic (- cos sin)))))
  (define* (make-cpp-locsig-internal curloc #:key (degree 0.0) (distance 1.0) #:allow-other-keys)
    ;; here we're picking the two original arguments that interest us (degree and distance)
    (let ((new-vals (get-cpp-scalers degree))
	  (dist (/ 1.0 (max distance 1.0))))
      (locsig-set! curloc 0 (* dist (car new-vals)))
      (locsig-set! curloc 1 (* dist (cadr new-vals)))))
  (let ((locgen (apply make-locsig args)))
    (if (= (mus-channels locgen) 2)
	(apply make-cpp-locsig-internal locgen args))
    locgen))

(define cpp-locsig locsig)


;;; -------- lisp graph with draggable x axis

(define mouse-down 0)
(define mouse-pos 0.0)
(define x1 1.0)

(define (show-draggable-graph)
  (let* ((pts (inexact->exact (* 100 x1)))
	 (data (make-vct pts)))
    (do ((i 0 (1+ i)))
	((= i pts))
      (vct-set! data i (* i .01)))
    (graph data "ramp" 0.0 x1)))

(define (mouse-press chn snd button state x y)
  (set! mouse-pos (/ x x1))
  (set! mouse-down x1))

(define (mouse-drag snd chn button state x y)
  (let* ((xnew (/ x x1))
	 (lim (min 1.0 (max 0.1 (+ mouse-down (- mouse-pos xnew))))))
    (set! x1 lim)
    (show-draggable-graph)))

;(add-hook! mouse-drag-hook mouse-drag)
;(add-hook! mouse-press-hook mouse-press)


;;; -------- pointer focus within Snd

;(add-hook! mouse-enter-graph-hook (lambda (snd chn) (focus-widget (car (channel-widgets snd chn)))))
;(add-hook! mouse-enter-listener-hook (lambda (widget) (focus-widget widget)))
;(add-hook! mouse-enter-text-hook (lambda (w) (focus-widget w)))


;;; -------- View: Files dialog chooses which sound is displayed
;;;
;;; by Anders Vinjar
;;;
;;; this hides all sounds but the one the mouse touched in the current files list

(define (files-popup-buffer type position name)
  (let ((snd (find-sound name)))
    (if snd
	(let* ((curr-buffer (max 0 (selected-sound)))
	       (width (car (widget-size (car (sound-widgets curr-buffer)))))
	       (height (cadr (widget-size (car (sound-widgets curr-buffer))))))
	  (for-each 
	   (lambda (n)
	     (hide-widget (car (sound-widgets n))))
	   (sounds))
	  (show-widget (car (sound-widgets snd)))
	  (set! (widget-size (car (sound-widgets snd)))
		(list width height))
	  (select-sound snd)))))

;(add-hook! mouse-enter-label-hook files-popup-buffer)


;;; -------- C-x b support along the same lines 
;;;
;;;  this could also hide all but the current channel, but I can't decide if that's a good idea

(define last-buffer #f)
(define current-buffer #f)
(define last-width 0)
(define last-height 0)

(define (open-current-buffer width height)
  (set! last-width width)
  (set! last-height height)
  (let ((sound-pane (car (sound-widgets (car current-buffer)))))
    (if sound-pane
	(begin
	  (show-widget sound-pane)
	  (set! (widget-size sound-pane) (list width height))
	  (select-sound (car current-buffer))
	  (select-channel (cadr current-buffer))))))

(define (close-all-buffers)
  (for-each 
   (lambda (n)
     (hide-widget (car (sound-widgets n))))
   (sounds)))

(define (switch-to-buf)
  (prompt-in-minibuffer 
   (if last-buffer
       (if (> (cadr last-buffer) 0)
	   (format #f "switch to buf: (default \"~A\" chan ~A) " 
		   (short-file-name (car last-buffer))
		   (cadr last-buffer))
	   (format #f "switch to buf: (default \"~A\") " 
		   (short-file-name (car last-buffer))))
       "switch to buf: (default: make new sound)")
   (lambda (response)
     (let* ((width (car (widget-size (car (sound-widgets (car current-buffer))))))
	    (height (cadr (widget-size (car (sound-widgets (car current-buffer)))))))
       (call-with-current-continuation
	(lambda (give-up)
	  (if (or (not (string? response))
		  (= (string-length response) 0))
	      (let ((temp current-buffer))
		(if last-buffer
		    (set! current-buffer last-buffer)
		    (let ((index (new-sound)))
		      (set! current-buffer (list index 0))))
		(set! last-buffer temp))
	      (let ((index (find-sound response)))
		(if index
		    (begin
		      (set! last-buffer current-buffer)
		      (set! current-buffer (list index 0)))
		    (give-up (report-in-minibuffer (format #f "can't find ~A" response))))))
	  (close-all-buffers)
	  (report-in-minibuffer "")
	  (open-current-buffer width height)))))))

(define xb-close
  (lambda (snd) 
    (if (and current-buffer
	     (= snd (car current-buffer)))
	(let ((closer (car current-buffer)))
	  (close-all-buffers)
	  (if last-buffer
	      (set! current-buffer last-buffer)
	      (if (sounds)
		  (set! current-buffer (list (car (sounds)) 0))
		  (set! current-buffer #f)))
	  (set! last-buffer
		(call-with-current-continuation
		 (lambda (return)
		   (for-each
		    (lambda (n)
		      (if (and (not (= n closer))
			       (or (not current-buffer)
				   (not (= n (car current-buffer)))))
			  (return (list n 0))))
		    (sounds))
		   #f)))
	  (if current-buffer
	      (open-current-buffer last-width last-height))))
    #f))

(define xb-open
  (lambda (snd)
    (close-all-buffers)
    (set! last-buffer current-buffer)
    (set! current-buffer (list snd 0))
    (open-current-buffer (if (= last-width 0) (window-width) last-width)
			 (if (= last-height 0) (- (window-height) 10) last-height))
    #f))

;(bind-key (char->integer #\b) 0 switch-to-buf #t)
;(add-hook! close-hook xb-close)
;(add-hook! after-open-hook xb-open)	    


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


;;; -------- remove-clicks 

(define (find-click loc)
  (let ((reader (make-sample-reader loc))
	(samp0 0.0)
	(samp1 0.0)
	(samp2 0.0)
	(samps (make-vct 10))
	(samps-ctr 0)
	(diff 1.0)
	(len (frames)))
    (call-with-current-continuation
     (lambda (return)
       (do ((ctr loc (1+ ctr)))
	   ((or (c-g?) (= ctr len)) #f)
	 (set! samp0 samp1)
	 (set! samp1 samp2)
	 (set! samp2 (next-sample reader))
	 (vct-set! samps samps-ctr samp0)
	 (if (< samps-ctr 9)
	     (set! samps-ctr (+ samps-ctr 1))
	     (set! samps-ctr 0))
	 (let ((local-max (max .1 (vct-peak samps))))
	   (if (and (> (abs (- samp0 samp1)) local-max)
		    (> (abs (- samp1 samp2)) local-max)
		    (< (abs (- samp0 samp2)) (/ local-max 2)))
	       (return (1- ctr)))))))))

(define (remove-clicks)
  ;; this is very conservative -- the click detection limits above could be set much tighter in many cases
  (define (remove-click loc)
    (let ((click (find-click loc)))
      (if (and click (not (c-g?)))
	  (begin
	    (smooth-sound (- click 2) 4)
	    (remove-click (+ click 2))))))
  (remove-click 0))


;;; -------- searching examples (zero+, next-peak)

(define (zero+)
  ;; find next positive-going zero crossing (if searching forward)
  (let ((lastn 0.0))
    (lambda (n)
      (let ((rtn (and (< lastn 0.0)
		      (>= n 0.0)
		      -1)))
	(set! lastn n)
	rtn))))

(define (next-peak)
  ;; find next max or min
  (let ((last0 #f)
	(last1 #f))
    (lambda (n)
      (let ((rtn (and (number? last0)
		      (or (and (< last0 last1) (> last1 n))
			  (and (> last0 last1) (< last1 n)))
		      -1)))
	(set! last0 last1)
	(set! last1 n)
	rtn))))

(define (find-pitch pitch)
  ;; find point in sound where pitch (in Hz) predominates -- C-s (find-pitch 300)
  ;;   in most cases, this will be slightly offset from the true beginning of the note
  (define (interpolated-peak-offset la ca ra)
    (let* ((pk (+ .001 (max la ca ra)))
	   (logla (/ (log (/ (max la .0000001) pk)) (log 10)))
	   (logca (/ (log (/ (max ca .0000001) pk)) (log 10)))
	   (logra (/ (log (/ (max ra .0000001) pk)) (log 10))))
      (/ (* 0.5 (- logla logra))
	 (- (+ logla logra)
	    (* 2 logca)))))
  (let ((data (make-vct (transform-size)))
	(data-loc 0))
    (lambda (n)
      (vct-set! data data-loc n)
      (set! data-loc (1+ data-loc))
      (let ((rtn #f))
	(if (= data-loc (transform-size))
	    (begin
	      (set! data-loc 0)
	      (if (> (vct-peak data) .001) ;ignore noise sections??
		  (let ((spectr (snd-spectrum data rectangular-window (transform-size)))
			(pk 0.0)
			(pkloc 0))
		    (let ((pit 
			   (do ((i 0 (1+ i)))
			       ((= i (/ (transform-size) 2)) 
				(/ (* (+ pkloc
					 (if (> pkloc 0)
					     (interpolated-peak-offset (vct-ref spectr (1- pkloc))
								       (vct-ref spectr pkloc)
								       (vct-ref spectr (1+ pkloc)))
					     0.0))
				      (srate)) 
				   (transform-size)))
			     (if (> (vct-ref spectr i) pk)
				 (begin
				   (set! pk (vct-ref spectr i))
				   (set! pkloc i))))))
		      (if (< (abs (- pitch pit)) (/ (srate) (* 2 (transform-size))))
			  (set! rtn (- (/ (transform-size) 2)))))))
	       (vct-fill! data 0.0)))
	 rtn))))


;;; -------- sound-data->list

(define (sound-data-channel->list sd chan)
  (let ((ls '()))
    (do ((i (1- (sound-data-length sd)) (1- i)))
	((< i 0) ls)
      (set! ls (cons (sound-data-ref sd chan i) ls)))))

(define (sound-data->list sd)
  (let ((lst '()))
    (do ((i (1- (sound-data-chans sd)) (1- i)))
	((< i 0) lst)
      (set! lst (cons (sound-data-channel->list sd i) lst)))))



;;; -------- file->vct and a sort of cue-list, I think

(define (file->vct file)
  ;; should probably save these somewhere
  (let* ((len (mus-sound-frames file))
	 (reader (make-sample-reader 0 file))
	 (data (make-vct len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (vct-set! data i (next-sample reader)))
    (free-sample-reader reader)
    data))

(define (add-notes notes)
  ;; adds (mixes) notes which is a list of lists of the form
  ;;   file &optional (offset 0.0) (amp 1.0)
  ;; starting at the cursor in the currently selected channel
  (let* ((snd (selected-sound))
	 (chn (selected-channel))
	 (start (cursor snd chn)))
    (as-one-edit
     (lambda ()
       (for-each 
	(lambda (note)
	  (let* ((file (car note))
		 (offset (if (> (length note) 1) (cadr note) 0.0))
		 (amp (if (> (length note) 2) (caddr note) #f))
		 (beg (+ start (inexact->exact (* (srate snd) offset)))))
	    (if (and (number? amp)
		     (not (= amp 1.0)))
		(mix-vct (vct-scale! (file->vct file) amp) beg snd chn #f "add-notes")
		(mix file beg 0 snd chn #f))))
	notes)))))

;(add-notes '(("oboe.snd") ("pistol.snd" 1.0 2.0)))

;;; or maybe "cue-list" means something like this:

(define (region-play-list data)
  ;; data is list of lists (list (list time reg)...), time in secs
  (for-each
   (lambda (tone)
     (let ((time (inexact->exact (* 1000 (car tone))))
	   (region (cadr tone)))
       (if (region? region)
	   (in time (lambda () (play-region region))))))
   data))

;(region-play-list (list (list 0.0 0) (list 0.5 1) (list 1.0 2) (list 1.0 0)))

(define (region-play-sequence data)
  ;; data is list of region ids which will be played one after the other
  ;; (region-play-sequence '(0 2 1))
  (region-play-list
   (let ((time 0.0))
     (map 
      (lambda (id)
	(let ((cur time))
	  (set! time (+ time (/ (region-length id) (region-srate id))))
	  (list cur id)))
      data))))
