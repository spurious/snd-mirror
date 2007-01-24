;;; examples of Scheme extensions to Snd
;;;
;;; documentation examples made harder to break
;;; 'info' from extsnd.html using format
;;; correlation
;;; XEmacs-like "Buffers" menu
;;; Reopen menu
;;; set transform-size based on current time domain window size
;;; superimpose spectra of sycn'd sounds
;;; example of c-g?
;;; make a system call
;;; translate mpeg input to 16-bit linear and read into Snd
;;; read and write OGG files
;;; make dot size dependent on number of samples being displayed
;;; move window left edge to mark upon 'm' key
;;; flash selected data red and green
;;; use loop info (if any) to set marks at loop points
;;; mapping extensions (map arbitrary single-channel function over various channel collections)
;;;     do-chans, do-all-chans, do-sound-chans
;;;     every-sample?
;;;     sort-samples
;;; mix mono sound into stereo sound panning according to env, also simple sound placement
;;; fft-edit, fft-squelch, squelch-vowels, fft-env-interp, fft-smoother -- FFT based editing, fft-smoothing
;;; comb-filter, notch-filter, formant-filter
;;; echo (delays)
;;; ring-modulation, am, vibro
;;; src-related sound effects (src, rand-interp, etc)
;;; compand (array-interp)
;;; shift pitch keeping duration constant (src+granulate)
;;; tempo change via envelope (granulate)
;;; cross-synthesis (using a formant bank)
;;; voiced->unvoiced (formants)
;;; convolution (convolve)
;;; time varying FIR filter, notch filter
;;; swap selection chans
;;; sound-interp, env-sound-interp
;;; add date and time to title bar
;;; how to get 'display' to write to Snd's listener
;;; filtered-env (low-pass and amplitude follow envelope)
;;; multi-colored rxvt printout
;;; lisp graph with draggable x axis
;;; pointer focus within Snd
;;; View: Files dialog chooses which sound is displayed
;;;    C-x b support along the same lines 
;;; remove-clicks
;;; searching examples (zero+, next-peak, find-pitch)
;;; file->vct and a sort of cue-list, I think, and region-play-list, region-play-sequence
;;; replace-with-selection
;;; explode-sf2 -- turn soundfont file into a bunch of files of the form sample-name.aif
;;; open-next-file-in-directory -- middle button click closes current file and opens next
;;; chain-dsps
;;; cursor-follows-play and stays where it was when the play ended
;;; smooth-channel as virtual op
;;; ring-modulate-channel (ring-mod as virtual op)
;;; scramble-channels -- reorder chans
;;; scramble-channel -- randomly reorder segments within a sound
;;; reverse-by-blocks and reverse-within-blocks -- reorder or reverse blocks within a channel
;;; sound segmentation
;;; sync-all

(use-modules (ice-9 debug) (ice-9 format) (ice-9 optargs) (ice-9 common-list))
(if (provided? 'snd-gauche) (use srfi-13)) ; string-downcase

(provide 'snd-examp.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))

(if (not (defined? 'all-chans))
    (define (all-chans)
      "(all-chans) -> two parallel lists, the first snd indices, the second channel numbers.  If we have 
two sounds open (indices 0 and 1 for example), and the second has two channels, (all-chans) returns '((0 1 1) (0 0 1))"
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

(define (selection-rms-1)
  "(selection-rms-1) -> rms of selection data using sample readers"
  (if (selection?)
      (let* ((reader (make-sample-reader (selection-position)))
	     (len (selection-frames))
	     (sum 0.0))
	(do ((i 0 (1+ i))) 
	    ((= i len) 
	     (begin 
	       (free-sample-reader reader) 
	       (sqrt (/ sum len))))
	  (let ((val (next-sample reader)))
	    (set! sum (+ sum (* val val))))))
      (throw 'no-active-selection (list "selection-rms-1"))))

;;; if you'd rather use recursion:
(define (selection-rms)
  "(selection-rms) -> rms of selection data using sample readers and recursion"
  ;; this is actually slightly faster than selection-rms-1
  ;; all the DO loops in this file could be re-written in this form, but I find loops easier to read
  (if (selection?)
      (let* ((reader (make-sample-reader (selection-position)))
	     (len (selection-frames)))
	(define rsum 
	  (lambda (leng sum)
	    (if (= leng 0)
		(sqrt (/ sum len))
		(let ((val (next-sample reader)))
		  (rsum (1- leng) (+ sum (* val val)))))))
	(let ((val (rsum len 0.0)))
	  (free-sample-reader reader)
	  val))
      (throw 'no-active-selection (list "selection-rms"))))

(define* (region-rms :optional (n 0))
  "(region-rms :optional (n 0)) -> rms of region n's data (chan 0)"
  (if (region? n)
      (let* ((data (region->vct 0 0 n)))
	(sqrt (/ (dot-product data data) (vct-length data))))
      (throw 'no-such-region (list "region-rms" n))))


(define* (window-samples :optional snd chn)
  "(window-samples :optional snd chn) -> samples in snd channel chn in current graph window"
  (let ((wl (left-sample snd chn))
	(wr (right-sample snd chn)))
    (channel->vct wl (+ 1 (- wr wl)) snd chn)))

(define (display-energy snd chn)
  ;; in this version, the y-zoom-slider controls the graph amp
  "(display-energy snd chn) is a lisp-graph-hook function to display the time domain data as energy (squared)"
  (let* ((ls (left-sample snd chn))
	 (rs (right-sample snd chn))
	 (datal (make-graph-data snd chn))
	 (data (if (vct? datal) datal (cadr datal)))
	 (sr (srate snd))
	 (y-max (y-zoom-slider snd chn)))
    (if (and data ls rs)
	(begin
	  (vct-multiply! data data)
	  (graph data "energy" (/ ls sr) (/ rs sr) 0.0 (* y-max y-max) snd chn #f)))))

;(add-hook! lisp-graph-hook display-energy)

(if (not (defined? 'log10))
    (define (log10 a) 
      "(log10 a) returns the log bas 10 of 'a'"
      (/ (log a) (log 10))))

(define display-db
  (lambda (snd chn)
    "(display-db snd chn) is a lisp-graph-hook function to display the time domain data in dB"
    (let ((datal (make-graph-data snd chn)))
      (if datal
	  (let* ((data (if (vct? datal) datal (cadr datal)))
		 (len (vct-length data))
		 (sr (srate snd)))
	    (define (dB val)
	      (if (< val .001)
		  -60.0
		  (* 20.0 (log10 val))))
	    (do ((i 0 (1+ i)))
		((= i len))
	      (vct-set! data i (+ 60.0 (dB (abs (vct-ref data i))))))
	    (graph data "dB" 
		   (/ (left-sample snd chn) sr) (/ (right-sample snd chn) sr)  
		   0.0 60.0
		   snd chn))))))

;(add-hook! lisp-graph-hook display-db)

(define (window-rms)
  "(window-rms) -> rms of data in currently selected graph window"
  (let* ((ls (left-sample))
	 (rs (right-sample))
	 (data (channel->vct ls (+ 1 (- rs ls))))
	 (len (vct-length data)))
    (sqrt (/ (dot-product data data) len))))


(define (fft-peak snd chn scale)
  "(fft-peak) returns the peak spectral magnitude"
  (if (and (transform-graph?) 
	   (= (transform-graph-type) graph-once))
      (report-in-minibuffer 
       (number->string (/ (* 2.0 (vct-peak (transform->vct snd chn))) 
			  (transform-size)))
       snd)
      #f))

;(add-hook! after-transform-hook fft-peak)


;;; -------- 'info' from extsnd.html using format --------

(define (finfo file)
  "(finfo file) -> description (as a string) of file"
  (format #f "~A: chans: ~D, srate: ~D, ~A, ~A, len: ~1,3F"
	  file
	  (mus-sound-chans file)
	  (mus-sound-srate file)
	  (mus-header-type-name (mus-sound-header-type file))
	  (mus-data-format-name (mus-sound-data-format file))
	  (exact->inexact (/ (mus-sound-samples file)
			     (* (mus-sound-chans file) (mus-sound-srate file))))))


;;; -------- Correlation --------
;;;
;;; correlation of channels in a stereo sound

(define (correlate snd chn y0 y1)
  "(correlate snd chn y0 y1) returns the correlation of snd's 2 channels (intended for use with graph-hook).  y0 and y1 are ignored."
  (if (and (= (channels snd) 2)
	   (> (frames snd 0) 1)
	   (> (frames snd 1) 1))
      (let* ((ls (left-sample snd 0))
	     (rs (right-sample snd 0))
	     (ilen (+ 1 (- rs ls)))
	     (pow2 (inexact->exact (ceiling (/ (log ilen) (log 2)))))
	     (fftlen (inexact->exact (expt 2 pow2)))
	     (fftlen2 (/ fftlen 2))
	     (fftscale (/ 1.0 fftlen))
	     (rl1 (channel->vct ls fftlen snd 0))
	     (rl2 (channel->vct ls fftlen snd 1))
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
      (report-in-minibuffer "vct-correlate wants stereo input")))

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
;;; see new-effects.scm for a much fancier example

;(define buffer-menu (add-to-main-menu "Buffers"))

(define (open-buffer filename)
  "(open-buffer filename) adds a menu item that will select filename (use with open-hook)"
  (add-to-menu buffer-menu 
	       filename 
	       (lambda () 
		 (let ((ind (find-sound filename)))
		   (if (sound? ind) 
		       (select-sound ind)))))
  #f)

(define (close-buffer snd)
  "(close-buffer snd) removes the menu item associated with snd (use with close-hook)"
  (remove-from-menu buffer-menu (file-name snd)))

;;; here we're adding this menu handling code to whatever is already happening at open/close-hook time

;(add-hook! open-hook open-buffer)
;(add-hook! close-hook close-buffer)



;;; -------- set transform-size based on current time domain window size
;;;
;;; also zoom spectrum based on y-axis zoom slider

(define (zoom-spectrum snd chn y0 y1)
  "(zoom-spectrum snd chn y0 y1) sets the transform size to correspond to the time-domain window size (use with graph-hook)"
  (if (and (transform-graph? snd chn) 
	   (= (transform-graph-type snd chn) graph-once))
      (begin
	(set! (transform-size snd chn)
	      (expt 2 (inexact->exact 
		       (ceiling 
			(/ (log (- (right-sample snd chn) (left-sample snd chn))) 
			   (log 2.0))))))
	(set! (spectro-cutoff snd chn) (y-zoom-slider snd chn))))
  #f)

;(add-hook! graph-hook zoom-spectrum)



;;; -------- superimpose spectra of sycn'd sounds

(define (superimpose-ffts snd chn y0 y1)
  "(superimpose-ffts snd chn y0 y1) superimposes ffts of multiple (syncd) sounds (use with graph-hook)"
  (let ((maxsync (apply max (map sync (sounds)))))
    (if (and (> (sync snd) 0)
	     (= snd (apply min (map (lambda (n) 
				      (if (= (sync snd) (sync n))
					  n
					  (1+ maxsync)))
				    (sounds)))))
	(let* ((ls (left-sample snd chn))
	       (rs (right-sample snd chn))
	       (pow2 (inexact->exact (ceiling (/ (log (- rs ls)) (log 2)))))
	       (fftlen (inexact->exact (expt 2 pow2))))
	  (if (> pow2 2)
	      (let ((ffts '()))
		(for-each
		 (lambda (n)
		   (if (and (= (sync n) (sync snd))
			    (> (chans n) chn))
		       (set! ffts (append ffts (let* ((fdr (channel->vct ls fftlen n chn))
						      (fdi (make-vct fftlen))
						      (spectr (make-vct (/ fftlen 2))))
						 (list (vct-add! spectr (spectrum fdr fdi #f 2))))))))
		 (sounds))
		(graph ffts "spectra" 0.0 0.5 y0 y1 snd chn)))))
    #f))

;(add-hook! graph-hook superimpose-ffts)


;;; -------- c-g? example (Anders Vinjar)

(define (locate-zero limit)
  "(locate-zero limit) looks for successive samples that sum to less than 'limit', moving the cursor if successful"
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


;;; -------- do?
;;; a version of "do" that is interruptible and continuable
;;;   to continue an interrupted "do?", (do-go-on)

(define do-go-on-continuation #f)

(define (do-go-on)
  "(do-go-on) tries to continue an interrupted do loop"
  (if (continuation? do-go-on-continuation)
      (do-go-on-continuation #f)
      ";sorry! can't continue"))

(defmacro do? (inits ends . body)
  `(do ,inits
       ((or (and ,(car ends)
		 (begin 
		   (set! do-go-on-continuation #f)
		   #t))
	    (and (c-g?)
		 (call-with-current-continuation
		  (lambda (go-on)
		    (set! do-go-on-continuation go-on)))))
	,(and (not (null? (cdr ends)))
	      (cadr ends)))
     ,@body))


;;; -------- make a system call from the listener
;;;
;;;   (shell "df") for example -- there's probably a more elegant way to do this is in Scheme
;;; or to play a sound whenever a file is closed:
;;;   (add-hook! close-hook (lambda (snd) (shell \"sndplay wood16.wav\")))

(use-modules (ice-9 popen))  

(if (provided? 'snd-guile)
    (define (shell cmd)
      "(shell cmd) sends 'cmd' to a shell (executes it as a shell command) and returns the result."
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



;;; -------- translate mpeg input to 16-bit linear and read into Snd
;;;
;;; mpg123 with the -s switch sends the 16-bit (mono or stereo) representation of
;;;   an mpeg file to stdout.  There's also apparently a switch to write 'wave' output.

(define (mpg mpgfile rawfile)
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
	      ;; (protection (logand b1 1))
	      ;; (bitrate-index (ash (logand b2 #b11110000) -4))
	      (srate-index (ash (logand b2 #b1100) -2))
	      ;; (padding (ash (logand b2 #b10) -1))
	      (channel-mode (ash (logand b3 #b11000000) -6))
	      ;; (mode-extension (ash (logand b3 #b110000) -4))
	      ;; (copyright (ash (logand b3 #b1000) -3))
	      ;; (original (ash (logand b3 #b100) -2))
	      ;; (emphasis (logand b3 #b11))
	      )
	  (if (= id 1)
	      (snd-print (format #f "odd: ~S is using a reserved Version ID" mpgfile)))
	  (if (= layer 0)
	      (snd-print (format #f "odd: ~S is using a reserved layer description" mpgfile)))
	  (let* ((chans (if (= channel-mode 3) 1 2))
		 (mpegnum (if (= id 0) 4 (if (= id 2) 2 1)))
		 (mpeg-layer (if (= layer 3) 1 (if (= layer 2) 2 3)))
		 (srate (/ (list-ref (list 44100 48000 32000 0) srate-index) mpegnum)))
	    (snd-print (format #f "~S: ~A Hz, ~A, MPEG-~A~%" 
			       mpgfile srate (if (= chans 1) "mono" "stereo") mpeg-layer))
	    (system (format #f "mpg123 -s ~A > ~A" mpgfile rawfile))
	    (open-raw-sound rawfile chans srate (if (little-endian?) mus-lshort mus-bshort)))))))

;;; (mpg "mpeg.mpg" "mpeg.raw")


;;; -------- read and write OGG files

(define (read-ogg filename)
  "(read-ogg filename) tries to open an OGG file"
  ;; check for "OggS" first word, if found, translate to something Snd can read
  ;; (open-sound (read-ogg "/home/bil/sf1/oboe.ogg"))
  (if (call-with-input-file filename 
	(lambda (fd)
	  (and (char=? (read-char fd) #\O)
	       (char=? (read-char fd) #\g)
	       (char=? (read-char fd) #\g)
	       (char=? (read-char fd) #\S))))
      (let ((aufile (string-append filename ".au")))
	(if (file-exists? aufile) (delete-file aufile))
	(system (format #f "ogg123 -d au -f ~A ~A" aufile filename))
	aufile)
      #f))

#|  
(add-hook! open-hook
           (lambda (filename)
             (if (= (mus-sound-header-type filename) mus-raw)
                 (read-ogg filename)
		 #f)))
|#

(define (write-ogg snd)
  "(write-ogg snd) writes 'snd' in OGG format"
  (if (or (> (car (edits snd)) 0)
	  (not (= (header-type snd) mus-riff)))
      (let ((file (string-append (file-name snd) ".tmp")))
	(save-sound-as file snd mus-riff)
	(system (format #f "oggenc ~A" file))
	(delete-file file))
      (system (format #f "oggenc ~A" (file-name snd)))))


;;; -------- read and write Speex files

(define (read-speex filename)
  "(read-speex filename) tries to open a SPEEX file"
  (let ((wavfile (string-append filename ".wav")))
    (if (file-exists? wavfile) (delete-file wavfile))
    (system (format #f "speexdec ~A ~A" filename wavfile))
    wavfile))

(define (write-speex snd)
  "(write-speex snd) writes 'snd' in Speex format"
  ;; write snd data in Speex format
  (if (or (> (car (edits snd)) 0)
	  (not (= (header-type snd) mus-riff)))
      (let ((file (string-append (file-name snd) ".wav"))
	    (spxfile (string-append (file-name snd) ".spx")))
	(save-sound-as file snd mus-riff)
	(system (format #f "speexenc ~A ~A" file spxfile))
	(delete-file file))
      (system (format #f "speexenc ~A ~A" (file-name snd) spxfile))))


;;; -------- read and write FLAC files

(define (read-flac filename)
  "(read-flac filename) tries to read a FLAC file"
  (system (format #f "flac -d ~A" filename)))

(define (write-flac snd)
  "(write-flac snd) writes 'snd' in a FLAC file"
  ;; write snd data in FLAC format
  (if (or (> (car (edits snd)) 0)
	  (not (= (header-type snd) mus-riff)))
      (let ((file (string-append (file-name snd) ".wav")))
	(save-sound-as file snd mus-riff)
	(system (format #f "flac ~A" file))
	(delete-file file))
      (system (format #f "flac ~A" (file-name snd)))))


;;; -------- play AC3 via a52dec

(define (play-ac3 name)
  "(play-ac3 name) uses a52dec to play an AC3 sound"
  ;; ideally we'd implement C-g to interrupt this, but you can type C-c to the shell once it has started
  ;;   to turn an AC3 file into something Snd can edit, /usr/local/bin/a52dec test.ac3 -o wav > test.wav
  (system (format #f "a52dec ~A" name)))


;;; -------- read ASCII files
;;;
;;; these are used by Octave (WaveLab) -- each line has one integer, apparently a signed short.

(define* (read-ascii in-filename :optional (out-filename "test.snd") (out-type mus-next) (out-format mus-bshort) (out-srate 44100))
  "(read-ascii in-filename :optional (out-filename \"test.snd\") (out-type mus-next) (out-format mus-bshort) (out-srate 44100)) tries to \
read an ASCII sound file"
  (let* ((in-fd (open-input-file in-filename))
	 (out-fd (new-sound out-filename out-type out-format out-srate 1 (format #f "created by read-ascii: ~A" in-filename)))
	 (bufsize 512)
	 (data (make-vct bufsize))
	 (loc 0)
	 (frame 0)
	 (short->float (/ 1.0 32768.0)))
    (as-one-edit
     (lambda ()
       (let loop ((val (read in-fd)))
	 (or (eof-object? val)
	     (begin
	       (vct-set! data loc (* (exact->inexact val) short->float))
	       (set! loc (1+ loc))
	       (if (= loc bufsize)
		   (begin
		     (vct->channel data frame bufsize out-fd 0)
		     (set! frame (+ frame bufsize))
		     (set! loc 0)))
	       (loop (read in-fd)))))
       (if (> loc 0)
	   (vct->channel data frame loc out-fd 0))))
    (close-input-port in-fd)
    out-fd))



;;; -------- make dot size dependent on number of samples being displayed
;;; 
;;; this could be extended to set time-graph-style to graph-lines if many samples are displayed, etc

(define (auto-dot snd chn y0 y1)
  "(auto-dot snd chn y0 y1) sets the dot size depending on the number of samples being displayed (use with graph-hook)"
  (let ((dots (- (right-sample snd chn)
		 (left-sample snd chn))))
    (if (> dots 100) 
	(set! (dot-size snd chn) 1)
	(if (> dots 50)
	    (set! (dot-size snd chn) 2)
	    (if (> dots 25)
		(set! (dot-size snd chn) 3)
		(set! (dot-size snd chn) 5))))
    #f))
    
;(add-hook! graph-hook auto-dot)



;;; -------- move window left edge to mark upon 'm'
;;;
;;; in large sounds, it can be pain to get the left edge of the window
;;; aligned with a specific spot in the sound.  In this code, we assume
;;; the desired left edge has a mark, and the 'm' key (without control)
;;; will move the window left edge to that mark.

(define (first-mark-in-window-at-left)
  "(first-mark-in-window-at-left) moves the graph so that the leftmost visible mark is at the left edge"
  (let* ((keysnd (or (selected-sound) (car (sounds))))
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
		keyboard-no-action)
	      (report-in-minibuffer "no mark in window"))))))

;(bind-key #\m 0 (lambda () "align window left edge with mark" (first-mark-in-window-at-left)))


;;; -------- flash selected data red and green

(define flash-selected-data
  (let ((data-red? #t)
	(red (make-color 1 0 0))
	(green (make-color 0 1 0)))
    (lambda (interval)
      "(flash-selected-data millisecs) causes the selected data to flash red and green"
      (if (selected-sound)
	  (begin
	    (set! (selected-data-color) (if data-red? green red))
	    (set! data-red? (not data-red?))
	    (in interval (lambda () (flash-selected-data interval))))))))


;;; --------  use loop info (if any) to set marks at loop points

(define (mark-loops)
  "(mark-loops) places marks at loop points found in the selected sound's header"
  (let ((loops (or (sound-loop-info)
		   (mus-sound-loop-info (file-name)))))
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
	(snd-print (format #f "~S has no loop info" (short-file-name))))))
		
	    

;;; -------- mapping extensions (map arbitrary single-channel function over various channel collections)
;;;

(define* (do-all-chans func :optional origin)
  "(do-all-chans func edhist) applies func to all active channels, using edhist as the edit history 
indication: (do-all-chans (lambda (val) (* 2.0 val)) \"double all samples\")"
  (apply map (lambda (snd chn)
	       (map-channel func 0 #f snd chn #f origin))
	 (all-chans)))

(define (update-graphs)
  "(update-graphs) updates (redraws) all graphs"
  (apply map (lambda (snd chn)
	       (update-time-graph snd chn))
	 (all-chans)))

(define* (do-chans func :optional origin)
  "(do-chans func edhist) applies func to all sync'd channels using edhist as the edit history indication"
  (let ((snc (sync)))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (map-channel func 0 #f snd chn #f origin)))
	       (all-chans))
	(snd-warning "sync not set"))))

(define* (do-sound-chans proc :optional origin)
  "(do-sound-chans func args edhist) applies func to all selected channels using edhist as the edit history indication"
  (let ((snd (selected-sound)))
    (if snd
	(begin
	  (do ((chn 0 (1+ chn)))
	      ((= chn (channels snd)) #f)
	    (map-channel proc 0 #f snd chn #f origin)))
	(snd-warning "no selected sound"))))

(define (every-sample? proc)
  "(every-sample func) -> #t if func is not #f for all samples in the current channel, 
otherwise it moves the cursor to the first offending sample"
  (let ((baddy (scan-channel 
		(lambda (y) 
		  (not (proc y))))))
    (if baddy (set! (cursor) (cadr baddy)))
    (not baddy)))

(define (sort-samples nbins)
  "(sort-samples bins) provides a histogram in 'bins' bins"
  (let ((bins (make-vector nbins 0)))
    (scan-channel
     (lambda (y)
       (let ((bin (inexact->exact (floor (* (abs y) nbins)))))
	 (vector-set! bins bin (+ (vector-ref bins bin) 1))
	 #f)))
    bins))



;;; -------- mix mono sound into stereo sound panning according to env

(define (place-sound mono-snd stereo-snd pan-env)
  "(place-sound mono-snd stereo-snd pan-env) mixes a mono sound into a stereo sound, splitting 
it into two copies whose amplitudes depend on the envelope 'pan-env'.  If 'pan-env' is 
a number, the sound is split such that 0 is all in channel 0 and 90 is all in channel 1."
  (let ((len (frames mono-snd)))
    (if (number? pan-env)
	(let* ((pos (/ pan-env 90.0))
	       (reader0 (make-sample-reader 0 mono-snd))
	       (reader1 (make-sample-reader 0 mono-snd)))
	  (map-channel (lambda (y)
			 (+ y (* pos (read-sample reader1))))
		       0 len stereo-snd 1)
	  (map-channel (lambda (y)
			 (+ y (* (- 1.0 pos) (read-sample reader0))))
		       0 len stereo-snd 0))
	(let ((e0 (make-env pan-env :end (1- len)))
	      (e1 (make-env pan-env :end (1- len)))
	      (reader0 (make-sample-reader 0 mono-snd))
	      (reader1 (make-sample-reader 0 mono-snd)))
	  (map-channel (lambda (y)
			 (+ y (* (env e1) (read-sample reader1))))
		       0 len stereo-snd 1)
	  (map-channel (lambda (y)
			 (+ y (* (- 1.0 (env e0)) (read-sample reader0))))
		       0 len stereo-snd 0)))))



;;; -------- FFT-based editing
;;;

(define* (fft-edit bottom top :optional snd chn)
  "(fft-edit low-Hz high-Hz) ffts an entire sound, removes all energy below low-Hz and all above high-Hz, 
then inverse ffts."
  (let* ((sr (srate snd))
	 (len (frames snd chn))
	 (fsize (expt 2 (inexact->exact (ceiling (/ (log len) (log 2.0))))))
	 (rdata (channel->vct 0 fsize snd chn))
	 (idata (make-vct fsize))
	 (lo (inexact->exact (round (/ bottom (/ sr fsize)))))
	 (hi (inexact->exact (round (/ top (/ sr fsize))))))
    (fft rdata idata 1)
    (if (> lo 0)
	(begin
	  (vct-set! rdata 0 0.0)
	  (vct-set! idata 0 0.0)
	  (do ((i 1 (1+ i))
	       (j (- fsize 1) (1- j)))
	      ((= i lo))
	    (vct-set! rdata i 0.0)
	    (vct-set! rdata j 0.0)
	    (vct-set! idata i 0.0)
	    (vct-set! idata j 0.0))))
    (if (< hi (/ fsize 2))
	(do ((i hi (1+ i))
	     (j (- fsize hi) (1- j)))
	    ((= i (/ fsize 2)))
	  (vct-set! rdata i 0.0)
	  (vct-set! rdata j 0.0)
	  (vct-set! idata i 0.0)
	  (vct-set! idata j 0.0)))
    (fft rdata idata -1)
    (vct-scale! rdata (/ 1.0 fsize))
    (vct->channel rdata 0 (1- len) snd chn #f (format #f "fft-edit ~A ~A" bottom top))))

(define* (fft-squelch squelch :optional snd chn)
  "(fft-squelch squelch) ffts an entire sound, sets all bins to 0.0 whose energy is below squelch, then inverse ffts"
  (let* ((len (frames snd chn))
	 (fsize (expt 2 (inexact->exact (ceiling (/ (log len) (log 2.0))))))
	 (rdata (channel->vct 0 fsize snd chn))
	 (idata (make-vct fsize))
	 (fsize2 (/ fsize 2))
	 (scaler 1.0))
    (fft rdata idata 1)
    (let ((vr (vct-copy rdata))
	  (vi (vct-copy idata)))
      (rectangular->polar vr vi)
      (set! scaler (vct-peak vr)))
    (let ((scl-squelch (* squelch scaler)))
      (if (< (sqrt (+ (* (vct-ref rdata 0) (vct-ref rdata 0)) (* (vct-ref idata 0) (vct-ref idata 0)))) scl-squelch)
	  (begin
	    (vct-set! rdata 0 0.0)
	    (vct-set! idata 0 0.0)))
      (do ((i 1 (1+ i))
	   (j (- fsize 1) (1- j)))
	  ((= i fsize2))
	(let ((magnitude (sqrt (+ (* (vct-ref rdata i) (vct-ref rdata i)) (* (vct-ref idata i) (vct-ref idata i))))))
	  (if (< magnitude scl-squelch)
	      (begin
		(vct-set! rdata i 0.0)
		(vct-set! rdata j 0.0)
		(vct-set! idata i 0.0)
		(vct-set! idata j 0.0)))))
      (fft rdata idata -1)
      (vct-scale! rdata (/ 1.0 fsize)))
    (vct->channel rdata 0 (1- len) snd chn #f (format #f "fft-squelch ~A" squelch))
    scaler))

(define* (fft-cancel lo-freq hi-freq :optional snd chn)
  "(fft-cancel lo-freq hi-freq) ffts an entire sound, sets the bin(s) representing lo-freq to hi-freq to 0.0, then inverse ffts"
  (let* ((sr (srate snd))
	 (len (frames snd chn))
	 (fsize (expt 2 (inexact->exact (ceiling (/ (log len) (log 2.0))))))
	 (rdata (channel->vct 0 fsize snd chn))
	 (idata (make-vct fsize)))
    (fft rdata idata 1)
    (let* ((hz-bin (/ sr fsize))
	   (lo-bin (inexact->exact (round (/ lo-freq hz-bin))))
	   (hi-bin (inexact->exact (round (/ hi-freq hz-bin)))))
      (do ((i lo-bin (1+ i))
	   (j (- fsize lo-bin) (1- j)))
	  ((> i hi-bin))
	(vct-set! rdata i 0.0) ; ignoring window side lobes for now
	(vct-set! idata i 0.0)
	(vct-set! rdata j 0.0)
	(vct-set! idata j 0.0)))
    (fft rdata idata -1)
    (vct-scale! rdata (/ 1.0 fsize))
    (vct->channel rdata 0 (1- len) snd chn #f (format #f "fft-cancel ~A ~A" lo-freq hi-freq))))
    

;;; same idea but used to distinguish vowels (steady-state) from consonants

(define (ramp gen up)
  "(ramp gen up) is a kind of CLM generator that produces a ramp of a given length, then sticks at 0.0 or 1.0 until the 'up' argument changes"
  ;; gen is list: ctr size
  ;;  the idea here is that we want to ramp in or out a portion of a sound based on some
  ;;  factor of the sound data -- the ramp gen produces a ramp up when 'up' is #t, sticking
  ;;  at 1.0, and a ramp down when 'up' is #f, sticking at 0.0
  ;;
  ;; this could use the moving-average generator, though the resultant envelopes would be slightly less bumpy
  (let* ((ctr (vct-ref gen 0))
	 (size (vct-ref gen 1))
	 (val (/ ctr size)))
    (vct-set! gen 0 (min size (max 0 (+ ctr (if up 1 -1)))))
    val))

(define* (make-ramp :optional (size 128))
  "(make-ramp :optional (size 128)) returns a ramp generator"
  (vct 0.0 size))

(define* (squelch-vowels :optional snd chn)
  "(squelch-vowels) suppresses portions of a sound that look like steady-state"
  (let* ((fft-size 32)
	 (fft-mid (inexact->exact (floor (/ fft-size 2))))
	 (rl (make-vct fft-size))
	 (im (make-vct fft-size))
	 (ramper (make-ramp 256)) ; 512 ok too
	 (peak (/ (maxamp) fft-mid))
	 (read-ahead (make-sample-reader 0 snd chn))
	 (ctr 0)
	 (in-vowel #f))
    (do ((i 0 (1+ i)))
	((= i (1- fft-size)))
      (vct-set! rl i (read-ahead)))
    (set! ctr (1- fft-size))
    (map-channel (lambda (y)
		   (vct-set! rl ctr (read-ahead))
		   (set! ctr (1+ ctr))
		   (if (= ctr fft-size)
		       (begin
			 (fft rl im 1)
			 (vct-multiply! rl rl)
			 (vct-multiply! im im)
			 (vct-add! rl im)
			 (set! in-vowel (> (+ (vct-ref rl 0) (vct-ref rl 1) (vct-ref rl 2) (vct-ref rl 3)) peak))
			 ;; fancier version checked here ratio of this sum and
			 ;;   sum of all rl vals, returned vowel if > 0.5
			 (set! ctr 0)
			 (vct-fill! im 0.0)))
		   (let ((rval (- 1.0 (ramp ramper in-vowel))))
		     ; squelch consonants if just ramp value (not 1.0-val)
		     ;(and (> rval 0.0) ; if this is included, the vowel-portions are omitted
		     (* y rval) ; squelch vowels 
		     ;(* y (+ (* 2 rval) .1)) ;accentuate consonants
		     ))
		 0 #f snd chn #f "squelch-vowels")))

(define* (fft-env-data fft-env :optional snd chn)
  "(fft-env-data fft-env) applies fft-env as spectral env to current sound, returning vct of new data"
  (let* ((len (frames snd chn))
	 (fsize (expt 2 (inexact->exact (ceiling (/ (log len) (log 2.0))))))
	 (rdata (channel->vct 0 fsize snd chn))
	 (idata (make-vct fsize))
	 (fsize2 (/ fsize 2))
	 (e (make-env fft-env :end (1- fsize2))))
    (fft rdata idata 1)
    (let ((val (env e)))
      (vct-set! rdata 0 (* val (vct-ref rdata 0)))
      (vct-set! idata 0 (* val (vct-ref idata 0))))
    (do ((i 1 (1+ i))
	 (j (1- fsize) (1- j)))
	((= i fsize2))
      (let ((val (env e)))
	(vct-set! rdata i (* val (vct-ref rdata i)))
	(vct-set! idata i (* val (vct-ref idata i)))
	(vct-set! rdata j (* val (vct-ref rdata j)))
	(vct-set! idata j (* val (vct-ref idata j)))))
    (fft rdata idata -1)
    (vct-scale! rdata (/ 1.0 fsize))))

(define* (fft-env-edit fft-env :optional snd chn)
  "(fft-env-edit fft-env) edits (filters) current chan using fft-env"
  (vct->channel (fft-env-data fft-env snd chn) 0 (1- (frames)) snd chn #f (format #f "fft-env-edit '~A" fft-env)))

(define* (fft-env-interp env1 env2 interp :optional snd chn)
  "(fft-env-interp env1 env2 interp) interpolates between two fft-filtered versions (env1 and env2 are the 
spectral envelopes) following interp (an env between 0 and 1)"
  (let* ((data1 (fft-env-data env1 snd chn))
	 (data2 (fft-env-data env2 snd chn))
	 (len (frames snd chn))
	 (new-data (make-vct len))
	 (e (make-env interp :end (1- len))))
    (do ((i 0 (1+ i)))
	((= i len))
      (let ((pan (env e)))
	(vct-set! new-data i 
		  (+ (* (- 1.0 pan) (vct-ref data1 i))
		     (* pan (vct-ref data2 i))))))
    (vct->channel new-data 0 (1- len) snd chn #f (format #f "fft-env-interp '~A '~A '~A" env1 env2 interp))))


(define* (filter-fft flt :optional (normalize #t) snd chn)
  "(filter-fft flt normalize snd chn) gets the spectrum of all the data in the given channel, \
applies the function 'flt' to it, then inverse ffts.  'flt' should take one argument, the \
current spectrum value.  (filter-fft (lambda (y) (if (< y .01) 0.0 else y))) is like fft-squelch."
  (let* ((len (frames snd chn))
	 (mx (maxamp snd chn))
	 (fsize (expt 2 (inexact->exact (ceiling (/ (log len) (log 2.0))))))
	 (fsize2 (/ fsize 2))
	 (rdata (channel->vct 0 fsize snd chn))
	 (idata (make-vct fsize))
	 (spect (snd-spectrum rdata rectangular-window fsize #t 1.0 #f normalize))) ; not in-place!
    (fft rdata idata 1)
    (flt (vct-ref spect 0))
    (do ((i 1 (1+ i))
	 (j (- fsize 1) (1- j)))
	((= i fsize2))
      (let* ((orig (vct-ref spect i))
	     (cur (flt orig)))
	(if (>  (abs orig) .000001)
	    (let ((scl (/ cur orig)))
	      (vct-set! rdata i (* scl (vct-ref rdata i)))
	      (vct-set! idata i (* scl (vct-ref idata i)))
	      (vct-set! rdata j (* scl (vct-ref rdata j)))
	      (vct-set! idata j (* scl (vct-ref idata j))))
	    (if (> (abs cur) .000001)
		(let ((scl (/ cur (sqrt 2.0))))
		  (vct-set! rdata i scl)
		  (vct-set! idata i scl)
		  (vct-set! rdata j scl)
		  (vct-set! idata j (- scl)))))))
    (fft rdata idata -1)
    (if (not (= mx 0.0))
	(let ((pk (vct-peak rdata)))
	  (vct->channel (vct-scale! rdata (/ mx pk)) 0 (1- len) snd chn #f (format #f "filter-fft ~A" flt)))
	(vct->channel rdata 0 (1- len) snd chn #f (format #f "filter-fft ~A" flt)))))

;; (let ((op (make-one-zero .5 .5))) (filter-fft op))
;; (let ((op (make-one-pole .05 .95))) (filter-fft op))
;; (filter-fft (lambda (y) (if (< y .1) 0.0 y)))
;; (let ((rd (make-sample-reader 0 0 0 1 0))) (scale-by 0) (filter-fft (lambda (y) (rd)))) ; treat original sound as spectrum
;; (filter-fft contrast-enhancement)
;; (filter-fft (lambda (y) (* y y y))) ; extreme low pass

#|
;;; save this example -- find a better use for it someday and add to docs
(let* ((ind (or (find-sound "now.snd")
		(open-sound "now.snd")))
       (mx (maxamp ind 0)))
  (do ((i 1 (1+ i))
       (lo 0.0 (+ lo .1)))
      ((= i 8))
    (filter-fft (lambda (y) (contrast-enhancement y (+ 1.0 (* lo 30.0)))) #t ind 0 0))
  (let ((mixers (make-vector 8)))
    (do ((i 0 (1+ i))
	 (lo 0.0 (+ lo .12)))
	((= i 8))
      (env-sound (list 0 0 lo 1 1 0) 0 #f 32.0 ind 0 (1+ i))
      (vector-set! mixers i (make-sample-reader 0 ind 0 1 (edit-position ind 0))))
    (scale-by 0.0)
    (map-channel
     (lambda (y)
       (let ((sum 0.0))
	 (do ((i 0 (1+ i)))
	     ((= i 8) sum)
	   (set! sum (+ sum (read-sample (vector-ref mixers i))))))))
    (do ((i 0 (1+ i)))
	((= i 8))
      (free-sample-reader (vector-ref mixers i))))
  (scale-to mx))
|#



(define* (fft-smoother cutoff start samps :optional snd chn)
  "(fft-smoother cutoff start samps snd chn) uses fft-filtering to smooth a 
section: (vct->channel (fft-smoother .1 (cursor) 400) (cursor) 400)"
  (let* ((fftpts (inexact->exact (expt 2 (inexact->exact (ceiling (/ (log (1+ samps)) (log 2.0)))))))
	 (rl (channel->vct start fftpts snd chn))
	 (im (make-vct fftpts))
	 (top (inexact->exact (floor (* fftpts cutoff)))))
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



;;; -------- comb-filter

(define (comb-filter scaler size)
  "(comb-filter scaler size) returns a comb-filter ready for map-channel etc: (map-channel (comb-filter .8 32)).  If you're 
in a hurry use: (clm-channel (make-comb .8 32)) instead"
  (let ((cmb (make-comb scaler size)))
    (lambda (x) 
      (comb cmb x))))

;;; by using filters at harmonically related sizes, we can get chords:

(define (comb-chord scaler size amp)
  "(comb-chord scaler size amp) returns a set of harmonically-related comb filters: (map-channel (comb-chord .95 100 .3))"
  (let ((c1 (make-comb scaler (inexact->exact size)))
	(c2 (make-comb scaler (inexact->exact (* size .75))))
	(c3 (make-comb scaler (inexact->exact (* size 1.2)))))
    (lambda (x) 
      (* amp (+ (comb c1 x) (comb c2 x) (comb c3 x))))))

;;; or change the comb length via an envelope:

(define+ (zcomb scaler size pm)
  "(zcomb scaler size pm) returns a comb filter whose length varies according to an 
envelope: (map-channel (zcomb .8 32 '(0 0 1 10)))"
  (define (max-envelope-1 e mx)
    (if (null? e)
	mx
	(max-envelope-1 (cddr e) (max mx (abs (cadr e))))))

  (let ((cmb (make-comb scaler size :max-size (inexact->exact (+ size 1 (max-envelope-1 pm 0.0)))))
	(penv (make-env :envelope pm :end (frames))))
    (lambda (x)
      (comb cmb x (env penv)))))

(define (notch-filter scaler size)
  "(notch-filter scaler size) returns a notch-filter: (map-channel (notch-filter .8 32))"
  (let ((cmb (make-notch scaler size)))
    (lambda (x) 
      (notch cmb x))))

(define (formant-filter radius frequency)
  "(formant-filter radius frequency) returns a formant generator: (map-channel (formant-filter .99 2400)). Faster 
is: (filter-sound (make-formant .99 2400))"
  (let ((frm (make-formant radius frequency)))
    (lambda (x) 
      (formant frm x))))

;;; to impose several formants, just add them in parallel:

(define (formants r1 f1 r2 f2 r3 f3)
  "(formants r1 f1 r2 f2 r3 f3) returns 3 formant filters in parallel: (map-channel (formants .99 900 .98 1800 .99 2700))"
  (let ((fr1 (make-formant r1 f1))
	(fr2 (make-formant r2 f2))
	(fr3 (make-formant r3 f3)))
    (lambda (x)
      (+ (formant fr1 x)
	 (formant fr2 x)
	 (formant fr3 x)))))

(define (moving-formant radius move)
  "(moving-formant radius move) returns a time-varying (in frequency) formant filter: (map-channel (moving-formant .99 '(0 1200 1 2400)))"
  (let ((frm (make-formant radius (cadr move)))
	(menv (make-env :envelope move :end (frames))))
    (lambda (x)
      (let ((val (formant frm x)))
	(set! (mus-frequency frm) (env menv))
	val))))

(define (osc-formants radius bases amounts freqs) ; changed to call map-channel itself, 21-Apr-05
  "(osc-formants radius bases amounts freqs) set up any number of independently oscillating 
formants, then calls map-channel: (osc-formants .99 (vct 400.0 800.0 1200.0) (vct 400.0 800.0 1200.0) (vct 4.0 2.0 3.0)))"
  (let* ((len (vct-length bases))
	 (frms (make-vector len))
	 (oscs (make-vector len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (vector-set! frms i (make-formant radius (vct-ref bases i)))
      (vector-set! oscs i (make-oscil (vct-ref freqs i))))
    (map-channel
     (lambda (x)
       (let ((val 0.0))
	 (do ((i 0 (1+ i)))
	     ((= i len))
	   (let ((frm (vector-ref frms i)))
	     (set! val (+ val (formant frm x)))
	     (set! (mus-frequency frm) 
		   (+ (vct-ref bases i)
		      (* (vct-ref amounts i) 
			 (oscil (vector-ref oscs i)))))))
	 val)))))



;;; -------- echo

(define (echo scaler secs)
  "(echo scaler secs) returns an echo maker: (map-channel (echo .5 .5) 0 44100)"
  (let ((del (make-delay (inexact->exact (round (* secs (srate)))))))
    (lambda (inval)
      (+ inval (delay del (* scaler (+ (tap del) inval)))))))

(define (zecho scaler secs frq amp)
  "(zecho scaler secs freq amp) returns a modulated echo maker: (map-channel (zecho .5 .75 6 10.0) 0 65000)"
  (let* ((os (make-oscil frq))
	 (len (inexact->exact (round (* secs (srate)))))
	 (del (make-delay len :max-size (inexact->exact (+ len amp 1)))))
    (lambda (inval)
      (+ inval 
	 (delay del 
		(* scaler (+ (tap del) inval))
		(* amp (oscil os)))))))

(define (flecho scaler secs)
  "(flecho scaler secs) returns a low-pass filtered echo maker: (map-channel (flecho .5 .9) 0 75000)"
  (let* ((flt (make-fir-filter :order 4 :xcoeffs (vct .125 .25 .25 .125)))
	 (del (make-delay  (inexact->exact (round (* secs (srate)))))))
    (lambda (inval)
      (+ inval 
	 (delay del 
		(fir-filter flt (* scaler (+ (tap del) inval))))))))


;;; -------- ring-mod and am
;;;
;;; CLM instrument is ring-modulate.ins

(define (ring-mod freq gliss-env)
  "(ring-mod freq gliss-env) returns a time-varying ring-modulation filter: (map-channel (ring-mod 10 (list 0 0 1 (hz->radians 100))))"
  (let* ((os (make-oscil :frequency freq))
	 (len (frames))
	 (genv (make-env :envelope gliss-env :end len)))
    (lambda (inval)
      (* (oscil os (env genv)) inval))))

(define (am freq)
  "(am freq)returns an amplitude-modulator: (map-channel (am 440))"
  (let ((os (make-oscil freq))) 
    (lambda (inval) 
      (amplitude-modulate 1.0 inval (oscil os)))))

;;; this taken from sox (vibro.c)
(define (vibro speed depth)
  "(vibro speed depth) adds vibrato or tremolo"
  (let* ((sine (make-oscil speed))
	 (scl (* 0.5 depth))
	 (offset (- 1.0 scl)))
    (lambda (y)
      (* y (+ offset (* scl (oscil sine)))))))


;;; -------- hello-dentist
;;;
;;; CLM instrument version is in clm.html

(define* (hello-dentist frq amp :optional snd chn)
  "(hello-dentist frq amp) varies the sampling rate randomly, making a voice sound quavery: (hello-dentist 40.0 .1)"
  (let* ((rn (make-rand-interp :frequency frq :amplitude amp))
	 (i 0)
	 (len (frames))
	 (in-data (channel->vct 0 len snd chn))
	 (out-len (inexact->exact (round (* len (+ 1.0 (* 2 amp))))))
	 (out-data (make-vct out-len))
	 (rd (make-src :srate 1.0 
		       :input (lambda (dir) 
				(let ((val (if (and (>= i 0) (< i len)) 
					       (vct-ref in-data i) 
					       0.0)))
				  (set! i (+ i dir)) 
				  val)))))
    (vct->channel
     (vct-map! out-data
	       (lambda ()
		 (src rd (rand-interp rn))))
     0 len snd chn #f (format #f "hello-dentist ~A ~A" frq amp))))

;;; a very similar function uses oscil instead of rand-interp, giving
;;; various "Forbidden Planet" sound effects:

(define* (fp sr osamp osfrq :optional snd chn)
  "(fp sr osamp osfrq) varies the sampling rate via an oscil: (fp 1.0 .3 20)"
  (let* ((os (make-oscil osfrq))
	 (s (make-src :srate sr))
	 (len (frames snd chn))
	 (sf (make-sample-reader 0 snd chn))
	 (out-data (make-vct len))
	 (amp osamp))
    (vct-map! out-data
	      (lambda () 
		(src s (* amp (oscil os))
		     (lambda (dir)
		       (if (> dir 0)
			   (next-sample sf)
			   (previous-sample sf))))))
    (free-sample-reader sf)
    (vct->channel out-data 0 len snd chn #f (format #f "fp ~A ~A ~A" sr osamp osfrq))))
	    

;;; -------- compand, compand-channel

(define compand-table (vct -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250 
			   0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000))
;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")

(define (compand)
  "(compand) returns a compander: (map-channel (compand))"
  (lambda (inval)
    (let ((index (+ 8.0 (* 8.0 inval))))
      (array-interp compand-table index 17))))

;;; here's the virtual op version:

(define* (compand-channel :optional (beg 0) dur snd chn edpos)
  "(compand-channel :optional (beg 0) dur snd chn edpos) applies a standard compander to sound"
  ;; this is the "regularized version of the compander using ptree-channel
  (ptree-channel (lambda (inval)
		   (let ((index (+ 8.0 (* 8.0 inval))))
		     (array-interp compand-table index 17)))
		 beg dur snd chn edpos #t #f
		 (format #f "compand-channel ~A ~A" beg dur)))

(define* (compand-sound :optional (beg 0) dur snd)
  "(compand-sound :optional beg dur snd) applies companding to every channel of 'snd'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let* ((out-chans (chans index)))
	  (do ((chn 0 (1+ chn)))
	      ((= chn out-chans))
	    (compand-channel beg dur index chn)))
	(throw 'no-such-sound (list "compand-sound" snd)))))



;;; -------- shift pitch keeping duration constant
;;;
;;; both src and granulate take a function argument to get input whenever it is needed.
;;; in this case, src calls granulate which reads the currently selected file.
;;; CLM version is in expsrc.ins

(define* (expsrc rate :optional snd chn)
  "(expsrc rate :optional snd chn) uses sampling-rate conversion and granular synthesis 
to produce a sound at a new pitch but at the original tempo.  It returns a function for map-channel."
  (let* ((gr (make-granulate :expansion rate))
	 ;; this can be improved by messing with make-granulate's hop and length args
	 (sr (make-src :srate rate))
	 (vsize 1024)
	 (vbeg 0)
	 (v (channel->vct 0 vsize))
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
				  (set! v (channel->vct vbeg vsize snd chn))))
			    val))))))))

;;; the next (expsnd) changes the tempo according to an envelope; the new duration
;;; will depend on the expansion envelope -- we integrate it to get
;;; the overall expansion, then use that to decide the new length.

(define* (expsnd gr-env :optional snd chn)
  "(expsnd gr-env) uses the granulate generator to change tempo according to an envelope: (expsnd '(0 .5 2 2.0))"
  (let* ((dur (/ (* (/ (frames snd chn) (srate snd)) 
		    (integrate-envelope gr-env)) ; in env.scm
		 (envelope-last-x gr-env)))
	 (gr (make-granulate :expansion (cadr gr-env) :jitter 0))
	 (ge (make-env :envelope gr-env :duration dur))
	 (sound-len (inexact->exact (round (* (srate snd) dur))))
	 (len (max sound-len (frames snd chn)))
	 (out-data (make-vct len))
	 (sf (make-sample-reader 0 snd chn)))
    (vct-map! out-data (lambda ()
			 (let ((val (granulate gr (lambda (dir) (next-sample sf)))))
			   (set! (mus-increment gr) (env ge))
			   val)))
    (free-sample-reader sf)
    (vct->channel out-data 0 len snd chn #f (format #f "expsnd '~A" gr-env))))


;;; -------- cross-synthesis
;;;
;;; CLM version is in clm.html

(define (cross-synthesis cross-snd amp fftsize r)
  "(cross-synthesis cross-snd amp fftsize r) does cross-synthesis between 'cross-snd' (a sound index) and the currently 
selected sound: (map-channel (cross-synthesis 1 .5 128 6.0))"
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
      (if (= ctr freq-inc)
	  (begin
	    (set! fdr (channel->vct inctr fftsize cross-snd 0))
	    (set! inctr (+ inctr freq-inc))
	    (spectrum fdr fdi #f 2)
	    (vct-subtract! fdr spectr)
	    (vct-scale! fdr (/ 1.0 freq-inc))
	    (set! ctr 0)))
      (set! ctr (+ ctr 1))
      (vct-add! spectr fdr)
      (* amp (formant-bank spectr formants inval)))))

;;; similar ideas can be used for spectral cross-fades, etc -- for example:

(define* (voiced->unvoiced amp fftsize r tempo :optional snd chn)
  "(voiced->unvoiced amp fftsize r tempo) turns a vocal sound into whispering: (voiced->unvoiced 1.0 256 2.0 2.0)"
  (let* ((freq-inc (/ fftsize 2))
	 (fdr (make-vct fftsize))
	 (fdi (make-vct fftsize))
	 (spectr (make-vct freq-inc))
	 (noi (make-rand (/ (srate snd) 3)))
	 (inctr 0)
	 (ctr freq-inc)
	 (radius (- 1.0 (/ r fftsize)))
	 (bin (/ (srate snd) fftsize))
	 (len (frames snd chn))
	 (outlen (inexact->exact (floor (/ len tempo))))
	 (hop (inexact->exact (floor (* freq-inc tempo))))
	 (out-data (make-vct (max len outlen)))
	 (formants (make-vector freq-inc))
	 (old-peak-amp 0.0)
	 (new-peak-amp 0.0))
    (do ((i 0 (1+ i)))
	((= i freq-inc))
      (vector-set! formants i (make-formant radius (* i bin))))
    (call-with-current-continuation ; setup non-local exit (for C-g interrupt)
     (lambda (break)                ;   now (break value) will exit the call/cc returning value
       (do ((k 0 (1+ k)))
	   ((= k outlen))
	 (let ((outval 0.0))
	   (if (= ctr freq-inc)
	       (begin
		 (if (c-g?)               ; let interface run
		     (break "interrupted")) ;   if C-g exit the loop returning the string "interrupted"
		 (set! fdr (channel->vct inctr fftsize snd chn))
		 (let ((pk (vct-peak fdr)))
		   (if (> pk old-peak-amp) (set! old-peak-amp pk)))
		 (spectrum fdr fdi #f 2)
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
       (vct->channel out-data 0 (max len outlen) snd chn #f 
		     (format #f "voiced->unvoiced ~A ~A ~A ~A" amp fftsize r tempo))))))

;;; very similar but use sum-of-cosines (glottal pulse train?) instead of white noise
(define* (pulse-voice cosines :optional (freq 440.0) (amp 1.0) (fftsize 256) (r 2.0) snd chn)
  "(pulse-voice cosines :optional (freq 440) (amp 1.0) (fftsize 256) (r 2.0) snd chn) uses sum-of-cosines to manipulate speech sounds"
  (let* ((freq-inc (/ fftsize 2))
	 (fdr (make-vct fftsize))
	 (fdi (make-vct fftsize))
	 (spectr (make-vct freq-inc))
	 (pulse (make-sum-of-cosines cosines freq))
	 (inctr 0)
	 (ctr freq-inc)
	 (radius (- 1.0 (/ r fftsize)))
	 (bin (/ (srate snd) fftsize))
	 (len (frames snd chn))
	 (out-data (make-vct len))
	 (formants (make-vector freq-inc))
	 (old-peak-amp 0.0)
	 (new-peak-amp 0.0))
    (do ((i 0 (1+ i)))
	((= i freq-inc))
      (vector-set! formants i (make-formant radius (* i bin))))
    (call-with-current-continuation ; setup non-local exit (for C-g interrupt)
     (lambda (break)
       (do ((k 0 (1+ k)))
	   ((= k len))
	 (let ((outval 0.0))
	   (if (= ctr freq-inc)
	       (begin
		 (if (c-g?) (break "interrupted"))
		 (set! fdr (channel->vct inctr fftsize snd chn))
		 (let ((pk (vct-peak fdr)))
		   (if (> pk old-peak-amp) (set! old-peak-amp pk)))
		 (spectrum fdr fdi #f 2)
		 (set! inctr (+ freq-inc inctr))
		 (vct-subtract! fdr spectr)
		 (vct-scale! fdr (/ 1.0 freq-inc))
		 (set! ctr 0)))
	   (set! ctr (+ ctr 1))
	   (vct-add! spectr fdr)
	   (set! outval (formant-bank spectr formants (sum-of-cosines pulse)))
	   (if (> (abs outval) new-peak-amp) (set! new-peak-amp (abs outval)))
	   (vct-set! out-data k outval)))
       (vct-scale! out-data (* amp (/ old-peak-amp new-peak-amp)))
       (vct->channel out-data 0 (max len len) snd chn)))))

;;; (pulse-voice 80 20.0 1.0 1024 0.01)
;;; (pulse-voice 80 120.0 1.0 1024 0.2)
;;; (pulse-voice 30 240.0 1.0 1024 0.1)
;;; (pulse-voice 30 240.0 1.0 2048)
;;; (pulse-voice 6 1000.0 1.0 512)


;;; -------- convolution example

(define (cnvtest snd0 snd1 amp)
  "(cnvtest snd0 snd1 amp) convolves snd0 and snd1, scaling by amp, returns new max amp: (cnvtest 0 1 .1)"
  (let* ((flt-len (frames snd0))
	 (total-len (+ flt-len (frames snd1)))
	 (cnv (make-convolve :filter (channel->vct 0 flt-len snd0)))
	 (sf (make-sample-reader 0 snd1))
	 (out-data (make-vct total-len)))
    (vct-map! out-data (lambda () (convolve cnv (lambda (dir) (next-sample sf)))))
    (free-sample-reader sf)
    (vct-scale! out-data amp)
    (let ((max-samp (vct-peak out-data)))
      (vct->channel out-data 0 total-len snd1)
      (if (> max-samp 1.0) (set! (y-bounds snd1) (list (- max-samp) max-samp)))
      max-samp)))

#|
;;; -------- time varying FIR filter (not very interesting...)

(define (fltit)
  "(fltit) returns a time-varying filter: (map-channel (fltit))"
  (let* ((coeffs (list .1 .2 .3 .4 .4 .3 .2 .1))
	 (flt (make-fir-filter 8 (list->vct coeffs)))
	 (es (make-vector 8)))
    (do ((i 0 (1+ i)))
	((= i 8))
      (vector-set! es i (make-env (list 0 (list-ref coeffs i) 1 0) :end 100)))
    (vector-set! es 5 (make-env '(0 .4 1 1) :duration 1.0))
    (lambda (x)
      (let ((val (fir-filter flt x))
	    (xcof (mus-xcoeffs flt)))
	(do ((i 0 (1+ i)))
	    ((= i 8))
	  (vct-set! xcof i (env (vector-ref es i))))
	val))))

;;; for something this simple (like a notch filter), we can use a two-zero filter:
;
;(define flt (make-zpolar .99 550.0))
;
;;; this is a strong notch filter centered at 550 Hz
;
;(map-channel (lambda (x) (two-zero flt x)))
;
;;; similarly make-two-pole (or better, make-formant)
;;; can be used for resonances.
|#



;;; -------- swap selection chans

(define+ (swap-selection-channels)
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
		 (len (selection-frames))
		 (snd-chn0 (find-selection-sound '()))
		 (snd-chn1 (find-selection-sound snd-chn0)))
	    (if snd-chn1
		(swap-channels (car snd-chn0) (cadr snd-chn0) (car snd-chn1) (cadr snd-chn1) beg len)
		(throw 'wrong-number-of-channels (list "swap-selection-channels" "needs two channels to swap"))))
	  (throw 'wrong-number-of-channels (list "swap-selection-channels" "needs a stereo selection")))
      (throw 'no-active-selection (list "swap-selection-channels"))))


;;; -------- sound interp
;;;
;;; make-sound-interp sets up a sound reader that reads a channel at an arbitary location,
;;;   interpolating between samples if necessary, the corresponding "generator" is sound-interp

(define* (make-sound-interp start :optional snd chn)
  "(make-sound-interp start :optional snd chn) -> an interpolating reader for snd's channel chn"
  (let* ((bufsize 2048)
	 (buf4size 128)
	 (data (channel->vct start bufsize snd chn))
	 (curbeg start)
	 (curend (+ start bufsize)))
    (lambda (loc)
      (if (< loc curbeg)
	  (begin
	    ;; get previous buffer
	    (set! curbeg (max 0 (+ buf4size (- loc bufsize))))
	    (set! curend (+ curbeg bufsize))
	    (set! data (channel->vct curbeg bufsize snd chn)))
	  (if (> loc curend)
	      (begin
		;; get next buffer
		(set! curbeg (max 0 (- loc buf4size)))
		(set! curend (+ curbeg bufsize))
		(set! data (channel->vct curbeg bufsize snd chn)))))
      (array-interp data (- loc curbeg) bufsize))))

(define sound-interp ;make it look like a clm generator
  (lambda (func loc) 
    "(sound-interp func loc) -> sample at loc (interpolated if necessary) from func created by make-sound-interp"
    (func loc)))

#|
(define test-interp
  (lambda (freq)
    ;; use a sine wave to lookup the current sound
    (let ((osc (make-oscil :frequency freq :initial-phase (+ pi (/ pi 2))))
	  (reader (make-sound-interp 0 0 0)) 
	  (len (1- (frames 0 0))))
      (map-channel (lambda (val) 
		     (sound-interp reader (* len (+ 0.5 (* 0.5 (oscil osc))))))))))

;;; (test-interp 0.5)

(define (sound-via-sound snd1 snd2) ; "sound composition"??
  (let* ((intrp (make-sound-interp 0 snd1 0))
	 (len (1- (frames snd1 0)))
	 (rd (make-sample-reader 0 snd2 0))
	 (mx (maxamp snd2 0)))
      (map-channel (lambda (val) 
		     (sound-interp intrp (inexact->exact (floor (* len (* 0.5 (+ 1.0 (/ (read-sample rd) mx)))))))))))
|#

;; env-sound-interp takes an envelope that goes between 0 and 1 (y-axis), and a time-scaler
;;   (1.0 = original length) and returns a new version of the data in the specified channel
;;   that follows that envelope (that is, when the envelope is 0 we get sample 0, when the
;;   envelope is 1 we get the last sample, envelope = .5 we get the middle sample of the 
;;   sound and so on. (env-sound-interp '(0 0 1 1)) will return a copy of the
;;   current sound; (env-sound-interp '(0 0 1 1 2 0) 2.0) will return a new sound 
;;   with the sound copied first in normal order, then reversed.  src-sound with an
;;   envelope could be used for this effect, but it is much more direct to apply the
;;   envelope to sound sample positions.

(define* (env-sound-interp envelope :optional (time-scale 1.0) snd chn)
  "(env-sound-interp env (time-scale 1.0) snd chn) reads snd's channel chn according to env and time-scale"
  ;; since the old/new sounds can be any length, we'll write a temp file rather than trying to use map-channel or vct-map!
  (let* ((len (frames snd chn))
	 (newlen (inexact->exact (floor (* time-scale len))))
	 (reader (make-sound-interp 0 snd chn))
	 (read-env (make-env envelope :end newlen :scaler len))
	 (tempfilename (snd-tempnam))
	 (fil (mus-sound-open-output tempfilename (srate snd) 1 #f mus-next "env-sound-interp temp file"))
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
    ;; #t trunc arg to set samples shortens the sound as needed
    (set-samples 0 newlen tempfilename snd chn #t
		 (format #f "env-sound-interp '~A ~A" envelope time-scale))
    (delete-file tempfilename)))

;;; here's a very similar function that uses granular synthesis to move at a varying tempo through a sound

(define* (granulated-sound-interp envelope :optional (time-scale 1.0) (grain-length 0.10) (grain-envelope '(0 0 1 1 2 1 3 0)) (output-hop 0.05) snd chn)
  "(granulated-sound-interp envelope :optional (time-scale 1.0) (grain-length 0.10) (grain-envelope '(0 0 1 1 2 1 3 0)) (output-hop 0.05) snd chn) reads \
the given channel following 'envelope' (as in env-sound-interp), using grains to create the re-tempo'd read"
  (let* ((len (frames snd chn))
	 (newlen (inexact->exact (floor (* time-scale len))))
	 (read-env (make-env envelope :end newlen :scaler len))
	 (tempfilename (snd-tempnam))
	 (fil (mus-sound-open-output tempfilename (srate snd) 1 #f mus-next "env-sound-interp temp file"))
	 ;; #f as data-format -> format compatible with sndlib (so no data translation is needed)
	 (grain-frames (inexact->exact (round (* grain-length (srate snd)))))
	 (hop-frames (inexact->exact (round (* output-hop (srate snd)))))
	 (num-readers (1+ (inexact->exact (round (/ grain-length output-hop)))))
	 (readers (make-vector num-readers #f))
	 (grain-envs (make-vector num-readers #f))
	 (next-reader-starts-at 0)
	 (next-reader 0)
	 (bufsize 8192)
	 (data (make-sound-data 1 bufsize))
	 (data-ctr 0)
	 (jitter (* (srate snd) .005)))

    (do ((i 0 (1+ i)))
	((= i num-readers))
      (vector-set! grain-envs i (make-env grain-envelope :end grain-frames)))

    (do ((i 0 (1+ i)))
	((= i newlen))
      (let ((position-in-original (env read-env)))     

	(if (>= i next-reader-starts-at)
	    (begin
	      (vector-set! readers next-reader 
			   (make-sample-reader (max 0 (inexact->exact (round (+ position-in-original (mus-random jitter))))) 
					       snd chn))
	      (mus-reset (vector-ref grain-envs next-reader)) ; restart grain env
	      (set! next-reader (1+ next-reader))
	      (if (>= next-reader num-readers) (set! next-reader 0))
	      (set! next-reader-starts-at (+ next-reader-starts-at hop-frames))))

	(let ((sum 0.0))
	  (do ((i 0 (1+ i)))
	      ((= i num-readers))
	    (if (sample-reader? (vector-ref readers i))
		(set! sum (+ sum (* (env (vector-ref grain-envs i)) (next-sample (vector-ref readers i)))))))
	  (sound-data-set! data 0 data-ctr sum))

	(set! data-ctr (1+ data-ctr))
	(if (= bufsize data-ctr)
	    (begin
	      (mus-sound-write fil 0 (1- bufsize) 1 data)
	      (set! data-ctr 0)))))

    (if (> data-ctr 0)
	(mus-sound-write fil 0 (1- data-ctr) 1 data))
    (mus-sound-close-output fil (* 4 newlen))
    ;; #t trunc arg to set samples shortens the sound as needed
    (set-samples 0 newlen tempfilename snd chn #t
		 (format #f "granulated-sound-interp '~A ~A ~A ~A ~A" envelope time-scale grain-length grain-envelope output-hop))
    (delete-file tempfilename)))


;;; (granulated-sound-interp '(0 0 1 .1 2 1) 1.0 0.2 '(0 0 1 1 2 0))
;;; (granulated-sound-interp '(0 0 1 1) 2.0)
;;; (granulated-sound-interp '(0 0 1 .1 2 1) 1.0 0.2 '(0 0 1 1 2 0) 0.02)



;;; -------- add date and time to title bar
;;;
;;; The window manager's property that holds the Snd window's title is WM_NAME,
;;;   we can use the window-property function (used normally for CLM/Snd communication)
;;;   to reset this value.  The Snd window's identifier is SND_VERSION.
;;;   Here we're also using the #t argument to short-file-name to get a list of all current sounds.

(define retitle-time (* 60 1000)) ;once a minute

(define (title-with-date)
  "(title-with-date) causes Snd's main window to display the time of day.  To turn off 
this clock, set retitle-time to 0"
  (let ((names (short-file-name #t)))
    (set! (window-property "SND_VERSION" "WM_NAME")
	  (format #f "snd (~A)~A"
		  (strftime "%d-%b %H:%M %Z" (localtime (current-time)))
		  (if (null? names)
		      ""
		      (format #f ":~{~A~^, ~}" names))))
    (if (> retitle-time 0)
	(in retitle-time title-with-date))))



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



;;; -------- filtered-env 

(define* (filtered-env e :optional snd chn)
  "(filtered-env env) is a time-varying one-pole filter: when env is at 1.0, no filtering, 
as env moves to 0.0, low-pass gets more intense; amplitude and low-pass amount move together"
  (let* ((samps (frames))
	 (flt (make-one-pole 1.0 0.0))
	 (amp-env (make-env e :end (1- samps))))
    (map-channel
     (lambda (val)
       (let ((env-val (env amp-env)))
	 (set! (mus-xcoeff flt 0) env-val)
	 (set! (mus-ycoeff flt 1) (- env-val 1.0))
	 (one-pole flt (* env-val val))))
     0 #f snd chn #f (format #f "filtered-env '~A" e))))


;;; -------- multi-colored rxvt printout
;;;
;;; if you're using display to write to rxvt, you can use the latter's escape sequences
;;;   for things like multi-colored text:

#|
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
|#


;;; -------- C-x b support: hide all but one of the current sounds (more like Emacs)
;;;
;;;  this could also hide all but the current channel, but I can't decide if that's a good idea

(define last-buffer #f)
(define current-buffer #f)
(define last-width 0)
(define last-height 0)

(define (open-current-buffer width height)
  "(open-current-buffer width height) makes sure the current buffer is displayed (part of the C-x b support)"
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
  "(close-all-buffers) closes all buffers (for C-x b)"
  (for-each 
   (lambda (n)
     (hide-widget (car (sound-widgets n))))
   (sounds)))

(define (switch-to-buf)
  "(switch-to-buf) handles C-x b"
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

(define (xb-close snd)
  "(xb-close snd) is part of the C-x b support"
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
	    (open-current-buffer last-width last-height)))))

(define (xb-open snd)
  "(xb-open snd) is part of the C-x b support"
  (close-all-buffers)
  (set! last-buffer current-buffer)
  (set! current-buffer (list snd 0))
  (open-current-buffer (if (= last-width 0) (window-width) last-width)
		       (if (= last-height 0) (- (window-height) 10) last-height))
  #f)

;(bind-key #\b 0 switch-to-buf #t)
;(add-hook! close-hook xb-close)
;(add-hook! after-open-hook xb-open)	    


;;; -------- remove-clicks 

(define (find-click loc)
  "(find-click loc) finds the next click starting at 'loc'"
  (let ((reader (make-sample-reader loc))
	(samp0 0.0)
	(samp1 0.0)
	(samp2 0.0)
	(samps (make-vct 10))
	(samps-ctr 0)
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

(define+ (remove-clicks)
  "(remove-clicks) tries to find and smooth-over clicks"
  ;; this is very conservative -- the click detection limits above could be set much tighter in many cases
  (define (remove-click loc)
    (let ((click (find-click loc)))
      (if (and click (not (c-g?)))
	  (begin
	    (smooth-sound (- click 2) 4)
	    (remove-click (+ click 2))))))
  (remove-click 0))


;;; -------- searching examples (zero+, next-peak)

(define (search-for-click)
  "(search-for-click) looks for the next click (for use with C-s)"
  (let ((samp0 0.0)
	(samp1 0.0)
	(samp2 0.0)
	(samps (make-vct 10))
	(sctr 0))
    (lambda (val)
      (declare (val float)) ; for run
      (set! samp0 samp1)
      (set! samp1 samp2)
      (set! samp2 val)
      (vct-set! samps sctr val)
      (set! sctr (+ sctr 1))
      (if (>= sctr 10) (set! sctr 0))
      (let ((local-max (max .1 (vct-peak samps))))
	(if (and (>= (abs (- samp0 samp1)) local-max)
		 (>= (abs (- samp1 samp2)) local-max)
		 (<= (abs (- samp0 samp2)) (/ local-max 2)))
	    -1
	    #f)))))

(define (zero+)
  "(zero+) finds the next positive-going zero crossing (if searching forward) (for use with C-s)"
  (let ((lastn 0.0))
    (lambda (n)
      (declare (n float))
      (let ((rtn (and (< lastn 0.0)
		      (>= n 0.0)
		      -1)))
	(set! lastn n)
	rtn))))

(define (next-peak)
  "(next-peak) finds the next max or min point in the time-domain waveform (for use with C-s)"
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

(define+ (find-pitch pitch)
  "(find-pitch pitch) finds the point in the current sound where 'pitch' (in Hz) predominates -- C-s (find-pitch 300) 
In most cases, this will be slightly offset from the true beginning of the note"
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


;;; -------- file->vct and a sort of cue-list, I think

(if (not (defined? 'file->vct))
    (define (file->vct file)
      "(file->vct file) returns a vct with file's data"
      (let* ((len (mus-sound-frames file))
	     (reader (make-sample-reader 0 file))
	     (data (make-vct len)))
	(do ((i 0 (1+ i)))
	    ((= i len))
	  (vct-set! data i (next-sample reader)))
	(free-sample-reader reader)
	data)))

(define* (add-notes notes :optional snd chn)
  "(add-notes notes) adds (mixes) 'notes' which is a list of lists of the form: file :optional (offset 0.0) (amp 1.0) 
starting at the cursor in the currently selected channel: (add-notes '((\"oboe.snd\") (\"pistol.snd\" 1.0 2.0)))"
  (let* ((start (cursor snd chn)))
    (as-one-edit
     (lambda ()
       (for-each 
	(lambda (note)
	  (let* ((file (car note))
		 (offset (if (> (length note) 1) (cadr note) 0.0))
		 (amp (if (> (length note) 2) (caddr note) #f))
		 (beg (+ start (inexact->exact (floor (* (srate snd) offset))))))
	    (if (and (number? amp)
		     (not (= amp 1.0)))
		(mix-vct (vct-scale! (file->vct file) amp) beg snd chn #f "add-notes")
		(mix file beg 0 snd chn #f))))
	notes))
     (format #f "add-notes '~S" notes))))

(define (region-play-list data)
  "(region-play-list data): 'data' is list of lists (list (list time reg)...), time in secs, setting up 
a sort of play list: (region-play-list (list (list 0.0 0) (list 0.5 1) (list 1.0 2) (list 1.0 0)))"
  (for-each
   (lambda (tone)
     (let ((time (inexact->exact (floor (* 1000 (car tone)))))
	   (region (cadr tone)))
       (if (region? region)
	   (in time (lambda () (play-region region))))))
   data))

(define (region-play-sequence data)
  "(region-play-sequence data): 'data' is list of region ids which will be played one after the other: (region-play-sequence '(0 2 1))"
  (region-play-list
   (let ((time 0.0))
     (map 
      (lambda (id)
	(let ((cur time))
	  (set! time (+ time (/ (region-frames id) (region-srate id))))
	  (list cur id)))
      data))))


;;; -------- replace-with-selection

(define (replace-with-selection)
  "(replace-with-selection) replaces the samples from the cursor with the current selection"
  (let ((beg (cursor))
	(len (selection-frames)))
    (insert-selection beg) ; put in the selection before deletion, since delete-samples can deactivate the selection
    (delete-samples (+ beg len) len)))



;;; -------- explode-sf2

(define (explode-sf2)
  "(explode-sf2) turns the currently selected soundfont file into a bunch of files of the form sample-name.aif"
  (letrec ((sf2it 
	    (lambda (lst)
	      (if (not (null? lst))
		  (let* ((vals (car lst))
			 ;; each inner list is: '(name start loop-start loop-end)
			 (name (car vals))
			 (start (cadr vals))
			 (end (if (null? (cdr lst))
				  (frames)
				  (cadr (cadr lst))))
			 (loop-start (- (caddr vals) start))
			 (loop-end (- (cadddr vals) start))
			 (filename (string-append name ".aif")))
		    (if (selection?)
			(set! (selection-member? #t) #f)) ; clear entire current selection, if any
		    (set! (selection-member?) #t)
		    (set! (selection-position) start)
		    (set! (selection-frames) (- end start))
		    (save-selection filename mus-aifc)
		    (let ((temp (open-sound filename)))
		      (set! (sound-loop-info temp) (list loop-start loop-end))
		      (close-sound temp))
		    (sf2it (cdr lst)))))))
    (sf2it (soundfont-info))))


;;; -------- open-next-file-in-directory

(define open-next-file-in-directory
  (let ((last-file-opened #f)
	(current-directory #f)
	(current-sorted-files #f))

    (define (file-from-path curfile)
      (let ((last-slash 0))
	(do ((i 0 (1+ i)))
	    ((= i (string-length curfile)))
	  (if (char=? (string-ref curfile i) #\/)
	      (set! last-slash i)))
	(substring curfile (1+ last-slash))))

    (define (directory-from-path curfile)
      (let ((last-slash 0))
	(do ((i 0 (1+ i)))
	    ((= i (string-length curfile)))
	  (if (char=? (string-ref curfile i) #\/)
	      (set! last-slash i)))
	(substring curfile 0 last-slash)))

    (define (find-next-file)
      ;; find the next file in the sorted list, with wrap-around
      (let ((choose-next (not (string? last-file-opened)))
	    (just-filename (file-from-path last-file-opened)))
	(call-with-current-continuation
	 (lambda (return)
	   (for-each 
	    (lambda (file)
	      (if choose-next
		  (return file)
		  (if (string=? file just-filename)
		      (set! choose-next #t))))
	    current-sorted-files)
	   ;; if we get here we wrapped around
	   (car current-sorted-files)))))

    (define (get-current-files dir)
      (set! current-directory dir)
      (set! current-sorted-files (sort (sound-files-in-directory dir) string<?)))
      
    (define (get-current-directory filename)
      (set! last-file-opened filename)
      (display last-file-opened)
      (let ((new-path (directory-from-path (mus-expand-filename filename))))
	(if (or (not (string? current-directory))
		(not (string=? current-directory new-path)))
	    (get-current-files new-path)))
      #f)

    (lambda ()
      (if (not (member get-current-files (hook->list open-hook)))
	  (add-hook! open-hook get-current-directory))
      (if (and (not (string? last-file-opened))
	       (not (null? (sounds))))
	  (set! last-file-opened (file-name (or (selected-sound)
						(car (sounds))))))
      (if (not current-directory)
	  (if (null? (sounds))
	      (get-current-files (getcwd))
	      (get-current-files (directory-from-path last-file-opened))))
      (if (null? current-sorted-files)
	  (throw 'no-such-file (list "open-next-file-in-directory" current-directory))
	  (let ((next-file (find-next-file)))
	    (if (find-sound next-file)
		(throw 'file-already-open (list "open-next-file-in-directory" next-file))
		(begin
		  (if (not (null? (sounds)))
		      (close-sound (or (selected-sound)
				       (car (sounds)))))
		  (open-sound next-file)))))
      #t)))

(define (click-middle-button-to-open-next-file-in-directory)
  "(click-middle-button-to-open-next-file-in-directory) adds open-next-file-in-directory to the mouse-click-hook"
  (add-hook! mouse-click-hook
	     (lambda (snd chn button state x y axis)
	       (if (= button 2)
		   (open-next-file-in-directory)
		   #f)))) ; else handle it normally


;;; -------- chain-dsps

(define* (chain-dsps beg dur :rest dsps)
  "(chain-dsps beg dur :rest dsps) sets up a generator patch from its arguments"
  ;; I assume the dsps are already made, 
  ;;          the envs are present as break-point lists
  ;;          the calls are ordered out->in (or last first)
  ;; this should use definstrument, not define*, but it's defined in ws.scm which I don't want to require here
  (let* ((dsp-chain (list->vector (reverse (map (lambda (gen)
						  (if (list? gen)
						      (make-env gen :duration dur)
						      gen))
						dsps))))
	 (start (inexact->exact (floor (* (mus-srate) beg))))
	 (samps (inexact->exact (floor (* (mus-srate) dur))))
	 (end (+ start samps))
	 (len (vector-length dsp-chain)))
    (ws-interrupt?)
    (run
     (lambda ()
       (do ((k start (1+ k)))
	   ((= k end))
	 (let ((val 0.0))
	   ;; using do and vector here for the run macro's benefit
	   (do ((i 0 (1+ i)))
	       ((= i len))
	     (let ((gen (vector-ref dsp-chain i)))
	       (if (env? gen)
		   (set! val (* (gen) val))
		   (if (readin? gen)
		       (set! val (+ val (gen)))
		       (set! val (gen val))))))
	   (outa k val *output*)))))))

#|
(with-sound ()
  (chain-dsps 0 1.0 '(0 0 1 1 2 0) (make-oscil 440))
  (chain-dsps 0 1.0 '(0 0 1 1 2 0) (make-one-zero .5) (make-readin "oboe.snd"))
  ;; next call not currently optimizable
  (chain-dsps 0 1.0 '(0 0 1 1 2 0) (let ((osc1 (make-oscil 220)) 
					 (osc2 (make-oscil 440))) 
				     (lambda (val) (+ (osc1 val) 
						      (osc2 (* 2 val)))))))
|#


;;; -------- cursor-follows-play and stays where it was when the play ended

(if (not (provided? 'snd-extensions.scm)) (load-from-path "extensions.scm"))
(if (not (provided? 'snd-hooks.scm)) (load-from-path "hooks.scm"))

(define* (if-cursor-follows-play-it-stays-where-play-stopped :optional (enable #t))
  ;; call with #t or no args to enable this, with #f to disable

  (let ()
    (define current-cursor
      (make-procedure-with-setter
       (lambda (snd chn) (channel-property 'cursor snd chn))
       (lambda (snd chn val) (set! (channel-property 'cursor snd chn) val))))
    
    (define original-cursor
      (make-procedure-with-setter
       (lambda (snd chn) (channel-property 'original-cursor snd chn))
       (lambda (snd chn val) (set! (channel-property 'original-cursor snd chn) val))))
    
    (define (local-dac-func data)
      (for-each
       (lambda (snd)
	 (do ((i 0 (1+ i)))
	     ((= i (channels snd)))
	   (if (not (= (cursor snd i) (original-cursor snd i)))
	       (set! (current-cursor snd i) (cursor snd i)))))
       (sounds)))
    
    (define (local-start-playing-func snd)
      (do ((i 0 (1+ i)))
	  ((= i (channels snd)))
	(set! (original-cursor snd i) (cursor snd i))
	(set! (current-cursor snd i) (cursor snd i))))
    
    (define (local-stop-playing-func snd)
      (set! (cursor snd #t) (current-cursor snd 0)))
    
    (if enable
	(begin
	  (add-hook! dac-hook local-dac-func)
	  (add-hook! start-playing-hook local-start-playing-func)
	  (add-hook! stop-playing-hook local-stop-playing-func))
	(begin
	  (remove-local-hook! dac-hook local-dac-func)
	  (remove-local-hook! start-playing-hook local-start-playing-func)
	  (remove-local-hook! stop-playing-hook local-stop-playing-func)))))


;;; -------- smooth-channel as virtual op

(define* (smooth-channel-via-ptree :optional (beg 0) dur snd chn edpos)
  "(smooth-channel-via-ptree :optional (beg 0) dur snd chn edpos) is smooth-channel implemented as a virtual edit"
  (let* ((y0 (sample beg snd chn edpos))
	 (y1 (sample (+ beg (or dur (1- (frames)))) snd chn edpos))
	 (init-angle (if (> y1 y0) pi 0.0)) 
	 (off (* .5 (+ y0 y1))) 
	 (scale (* 0.5 (abs (- y1 y0))))
	 (data (vct 0.0 0.0 init-angle off scale)))
    (ptree-channel
     (lambda (y data forward)
       (declare (y real) (data vct) (forward boolean))
       (let* ((angle (vct-ref data 0))
	      (incr (vct-ref data 1))
	      (val (+ (vct-ref data 3) 
		      (* (vct-ref data 4) 
			 (cos (+ (vct-ref data 2) angle))))))
       (if forward
	   (vct-set! data 0 (+ angle incr))
	   (vct-set! data 0 (- angle incr)))
       val))
     beg dur snd chn edpos #t
     (lambda (frag-beg frag-dur)
       (let ((incr (/ pi frag-dur)))
	 (vct-set! data 1 incr)
	 (vct-set! data 0 (* frag-beg incr))
	 data))
     (format #f "smooth-channel-via-ptree ~A ~A" beg dur))))


;;; -------- ring-modulate-channel (ring-mod as virtual op)

(define* (ring-modulate-channel freq :optional (beg 0) dur snd chn edpos)
  "(ring-modulate-channel freq :optional (beg 0) dur snd chn edpos) ring-modulates the given channel"
  (ptree-channel
   (lambda (y data forward)
     (declare (y real) (data vct) (forward boolean))
     (let* ((angle (vct-ref data 0))
	    (incr (vct-ref data 1))
	    (val (* y (sin angle))))
       (if forward
	   (vct-set! data 0 (+ angle incr))
	   (vct-set! data 0 (- angle incr)))
       val))
   beg dur snd chn edpos #f
   (lambda (frag-beg frag-dur)
     (let ((incr (/ (* 2 pi freq) (srate snd))))
       (vct (fmod (* frag-beg incr) (* 2 pi)) incr)))
   (format #f "ring-modulate-channel ~A ~A ~A" freq beg dur)))

;;; amplitude-modulate-channel could be (lambda (y data forward) (* y 0.5 (+ 1.0 (sin angle))) etc ...)


;;; -------- re-order channels 

(define (scramble-channels . new-order)
  ;; (scramble-channels 3 2 0 1) means chan 3 goes to 0, etc
  
  (define (find-chan chans chan len)
    (let ((pos #f))
      (do ((i 0 (1+ i)))
	  ((or pos (= i len)) pos)
	(if (= (vector-ref chans i) chan)
	    (set! pos i)))))

  (define (scramble-channels-1 cur-chans end-chans chans loc)
    (if (> chans loc)
	(let* ((end-chan (vector-ref end-chans loc)) ; we want this channel at loc
	       (cur-chan (vector-ref cur-chans loc)) ; this (original) channel is currently at loc
	       (end-loc (find-chan cur-chans end-chan chans))) ; where is end-chan currently?
	  ;; end-chan goes in cur-chan's slot
	  (if (not (= cur-chan end-chan))
	      (begin
		(swap-channels #f end-loc #f loc)
		(vector-set! cur-chans end-loc cur-chan)
		(vector-set! cur-chans loc end-chan)))
	  (scramble-channels-1 cur-chans end-chans chans (1+ loc)))))

  (let ((len (length new-order)))
    (if (> len 1)
	(let ((end-chans (list->vector new-order))
	      (cur-chans (make-vector len)))
	  (do ((i 0 (1+ i)))
	      ((= i len))
	    (vector-set! cur-chans i i))
	  (scramble-channels-1 cur-chans end-chans len 0)))))

(define (scramble-channel silence-1)
  ;; (scramble-channel .01)
  (let ((buffer (make-moving-average 128))
	(silence (/ silence-1 128))
	(edges '())
	(samp 0)
	(in-silence #t)
	(old-max (max-regions))
	(old-tags (with-mix-tags)))
    (dynamic-wind
     (lambda ()
       (set! (max-regions) 1024)
       (set! (with-mix-tags) #f))
     (lambda ()
       (scan-channel
	(lambda (y)
	  (let* ((sum-of-squares (moving-average buffer (* y y)))
		 (now-silent (< sum-of-squares silence)))
	    (if (not (eq? in-silence now-silent))
		(set! edges (cons samp edges)))
	    (set! in-silence now-silent)
	    (set! samp (1+ samp))
	    #f)))
       (set! edges (append (reverse edges) (list (frames))))
       (let* ((len (length edges))
	      (pieces (make-vector len #f))
	      (start 0)
	      (ctr 0))
	 (for-each
	  (lambda (end)
	    (vector-set! pieces ctr (make-region start end))
	    (set! ctr (1+ ctr))
	    (set! start end))
	  edges)
	 (set! start 0)
	 (as-one-edit
	  (lambda()
	    (scale-by 0.0)
	    (do ((i 0 (1+ i)))
		((= i len))
	      (let* ((this (random len))
		     (reg (vector-ref pieces this)))
		(vector-set! pieces this #f)
		(if (not reg)
		    (begin
		      (do ((j (1+ this) (1+ j)))
			  ((or (= j len)
			       reg))
			(set! reg (vector-ref pieces j))
			(if reg (vector-set! pieces j #f)))
		      (if (not reg)
			  (do ((j (1- this) (1- j)))
			      ((or (< j 0)
				   reg))
			    (set! reg (vector-ref pieces j))
			    (if reg (vector-set! pieces j #f))))))
		(mix-region start reg)
		(set! start (+ start (region-frames reg)))
		(forget-region reg)))))))
     (lambda ()
       (set! (with-mix-tags) old-tags)
       (set! (max-regions) old-max)))))
    
    
;; -------- reorder blocks within channel

(define* (reverse-by-blocks block-len :optional snd chn)
  "(reverse-by-blocks block-len :optional snd chn): divide sound into block-len blocks, recombine blocks in reverse order"
  (let* ((len (frames snd chn))
	 (num-blocks (inexact->exact (floor (/ len (* (srate snd) block-len))))))
    (if (> num-blocks 1)
	(let* ((actual-block-len (inexact->exact (ceiling (/ len num-blocks))))
	       (rd (make-sample-reader (- len actual-block-len) snd chn))
	       (beg 0)
	       (ctr 1))
	  (map-channel
            (lambda (y)
	      (let ((val (read-sample rd)))
		(if (< beg 10) ; ramp start and end to avoid clicks (might want to mix with next section)
		    (set! val (* val beg .1))
		    (if (> beg (- actual-block-len 10))
			(set! val (* val (- actual-block-len beg) .1))))
		(set! beg (1+ beg))
		(if (= beg actual-block-len)
		    (begin
		      (set! ctr (1+ ctr))
		      (set! beg 0)
		      (set! rd (make-sample-reader (max 0 (- len (* ctr actual-block-len))) snd chn))))
		val))
	    0 #f snd chn #f (format #f "reverse-by-blocks ~A" block-len))))))

(define* (reverse-within-blocks block-len :optional snd chn)
  "(reverse-within-blocks block-len :optional snd chn): divide sound into blocks, recombine in order, but each block internally reversed"
  (let* ((len (frames snd chn))
	 (num-blocks (inexact->exact (floor (/ len (* (srate snd) block-len))))))
    (if (> num-blocks 1)
	(let* ((actual-block-len (inexact->exact (ceiling (/ len num-blocks))))
	       (no-clicks-env (list 0.0 0.0  .01 1.0  .99 1.0  1.0 0.0)))
	  (as-one-edit
	   (lambda ()
	     (do ((beg 0 (+ beg actual-block-len)))
		 ((>= beg len))
	       (reverse-channel beg actual-block-len snd chn)
	       (env-channel no-clicks-env beg actual-block-len snd chn)))
	   (format #f "reverse-within-blocks ~A" block-len)))
	(reverse-channel 0 #f snd chn))))

  
;;; -------- sound segmentation
;;;
;;; this code was used to return note on and off points for the Iowa Musical Instrument Sound library
;;;   the main function takes the top level directory of the sounds, and returns (eventually) a text
;;;   file containing the start times (in samples) and durations of all the notes (each sound file in
;;;   this library can have about 12 notes).  The results of this function need to be fixed up by hand
;;;   in some cases (violin notes in particular).

(define* (sounds->segment-data main-dir :optional (output-file "sounds.data"))

  (define (lower-case-and-no-spaces name)
    (let* ((new-name (string-downcase name))
	   (len (string-length new-name)))
      (do ((i 0 (1+ i)))
	  ((= i len) new-name)
	(if (char=? (string-ref new-name i) #\space)
	    (string-set! new-name i #\-)))))

  (define (directory->list dir)
    (let ((dport (opendir dir)))                ; no opendir in Gauche -- not sure how to handle this
      (let loop ((entry (readdir dport))
		 (files '()))
	(if (not (eof-object? entry))
	    (loop (readdir dport) (cons entry files))
	    (begin
	      (closedir dport)
	      (reverse! files))))))

  (define (segment-maxamp name beg dur)
    (let ((mx 0.0)
	  (rd (make-sample-reader beg name)))
      (run
       (lambda ()
	 (do ((i 0 (1+ i)))
	     ((= i dur))
	   (set! mx (max mx (abs (next-sample rd)))))))
      (free-sample-reader rd)
      mx))

  (define (segment-sound name high low)
    (let* ((end (mus-sound-frames name))
	   (reader (make-sample-reader 0 name))    ; no need to open the sound and display it
	   (avg (make-moving-average :size 128))
	   (lavg (make-moving-average :size 2048)) ; to distinguish between slow pulse train (low horn) and actual silence
	   (segments (make-vct 100))
	   (segctr 0)
	   (possible-end 0)
	   (in-sound #f))
      (run 
       (lambda ()                                  ; this block is where 99% of the time goes
	 (do ((i 0 (1+ i)))
	     ((= i end))
	   (let* ((samp (abs (next-sample reader)))
		  (val (moving-average avg samp))
		  (lval (moving-average lavg samp)))
	     (if in-sound
		 (if (< val low)
		     (begin
		       (set! possible-end i)
		       (if (< lval low)
			   (begin
			     (vct-set! segments segctr (+ possible-end 128))
			     (set! segctr (1+ segctr))
			     (set! in-sound #f)))))
		 (if (> val high)
		     (begin
		       (vct-set! segments segctr (- i 128))
		       (set! segctr (1+ segctr))
		       (set! in-sound #t))))))))
      (free-sample-reader reader)
      (if in-sound
	  (begin
	    (vct-set! segments segctr end)
	    (list (1+ segctr) segments))
	  (list segctr segments))))

  (define* (do-one-directory fd dir-name ins-name :optional (high .01) (low .001))
    (snd-print (format #f ";~A~%" dir-name))
    (for-each
     (lambda (sound)
       (let* ((sound-name (string-append dir-name "/" sound))
	      (boundary-data (segment-sound sound-name high low))
	      (boundaries (cadr boundary-data))
	      (segments (car boundary-data)))
	 (format fd "~%~%;;;    ~A" sound)
	 (format fd "~%(~A ~S" ins-name (string-append dir-name "/" sound))
	 (do ((bnd 0 (+ bnd 2)))
	     ((>= bnd segments))
	   (let* ((segbeg (inexact->exact (vct-ref boundaries bnd)))
		  (segdur (inexact->exact (- (vct-ref boundaries (1+ bnd)) segbeg))))
	     (format fd " (~A ~A ~A)" segbeg segdur (segment-maxamp sound-name segbeg segdur))))
	 (format fd ")")
	 (mus-sound-forget (string-append dir-name "/" sound))))
     (sound-files-in-directory dir-name)))

  (call-with-output-file
      output-file
    (lambda (fd)
      (let ((old-fam (with-file-monitor))) 
	(set! (with-file-monitor) #f) ; no need to monitor these guys
	(format fd ";;; sound data from ~S" main-dir)
	(if (not (char=? (string-ref main-dir (1- (string-length main-dir))) #\/))
	    (set! main-dir (string-append main-dir "/")))
	(for-each
	 (lambda (dir)
	   (if (not (char=? (string-ref dir 0) #\.))
	       (let ((ins-name (lower-case-and-no-spaces dir)))
		 (format fd "~%~%;;; ---------------- ~A ----------------" dir)
		 (if (string=? dir "Piano")
		     (for-each
		      (lambda (inner-dir)
			(if (not (char=? (string-ref inner-dir 0) #\.))
			    (do-one-directory fd (string-append main-dir dir "/" inner-dir) ins-name .001 .0001))) ; pp piano notes are .01 maxamp and short
		      (directory->list (string-append main-dir dir)))
		     (do-one-directory fd (string-append main-dir dir) ins-name)))))
	 (directory->list main-dir))
	(set! (with-file-monitor) old-fam)))))

;;; (sounds->segment-data "/home/bil/test/iowa/sounds/" "iowa.data")


;;; -------- channel-clipped?

(define* (channel-clipped? :optional snd chn)
  "(channel-clipped? snd chn) returns #t and a sample number if it finds clipping"
  (let ((last-y 0.0))
    (scan-channel 
     (lambda (y)
       (let ((result (and (>= (abs y) 0.9999)
			  (>= (abs last-y) 0.9999))))
	 (set! last-y y)
	 result))
     0 #f snd chn)))

#|
(define* (mark-clipping)
  "(mark-clipping) adds a mark at a clipped portion"
  (let ((snd (or (selected-sound) 
		 (and (not (null? (sounds))) 
		      (car (sounds))))))
    (if (sound? snd)
	(do ((chn 0 (1+ chn)))
	    ((= chn (chans snd)))
	  (let ((last-y 0.0)
		(marked #f)
		(samp 0))
	    (scan-channel 
	     (lambda (y)
	       (let ((clipping (and (>= (abs y) 0.9999)
				    (>= last-y 0.9999))))
		 (set! last-y (abs y))
		 (if (and clipping (not marked))
		     (begin
		       (add-mark (1- samp) snd chn)
		       (set! marked #t))
		     (if marked
			 (set! marked (or (> (abs y) 0.9999)
					  (> last-y 0.9999)))))
		 (set! samp (1+ samp))
		 #f)))
	    (update-time-graph snd chn))))))

(add-to-menu 2 "Show Clipping" (lambda () (mark-clipping)))
|#


;;; -------- sync-all

(define (sync-all)
  "(sync-all) sets the sync fields of all currently open sounds to the same, unique value"
  (let ((new-sync (1+ (sync-max))))
    (for-each
     (lambda (snd)
       (set! (sync snd) new-sync))
     (sounds))))
