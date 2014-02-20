;;; examples of Scheme extensions to Snd
;;;
;;; documentation examples made harder to break
;;; 'info' from extsnd.html using format
;;; correlation
;;; XEmacs-like "Buffers" menu
;;; Reopen menu
;;; set transform-size based on current time domain window size
;;; superimpose spectra of sycn'd sounds
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
;;; sound-interp, env-sound-interp
;;; filtered-env (low-pass and amplitude follow envelope)
;;; multi-colored rxvt printout
;;; lisp graph with draggable x axis
;;; pointer focus within Snd
;;; View: Files dialog chooses which sound is displayed
;;; remove-clicks
;;; searching examples (zero+, next-peak, find-pitch)
;;; file->floats and a sort of cue-list, I think, and region-play-list, region-play-sequence
;;; explode-sf2 -- turn soundfont file into a bunch of files of the form sample-name.aif
;;; open-next-file-in-directory -- middle button click closes current file and opens next
;;; chain-dsps
;;; scramble-channels -- reorder chans
;;; scramble-channel -- randomly reorder segments within a sound
;;; reverse-by-blocks and reverse-within-blocks -- reorder or reverse blocks within a channel
;;; sound segmentation
;;; sync-everything

(provide 'snd-examp.scm)
(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))
(if (not (provided? 'snd-env.scm)) (load "env.scm")) ; integrate-envelope, reverse-envelopes, etc


;;; -------- (ext)snd.html examples made harder to break --------
;;;
;;; this mainly involves keeping track of the current sound/channel

(define (selection-rms)
  "(selection-rms) -> rms of selection data using samplers"
  (if (selection?)
      (let ((data (samples (selection-position) (selection-frames))))
	(sqrt (/ (dot-product data data) (selection-frames))))
      (error 'no-active-selection (list "selection-rms-1"))))


(define (region-rms reg)
  "(region-rms n) -> rms of region n's data (chan 0)"
  (if (region? reg)
      (let ((data (region->float-vector reg 0 0)))
	(sqrt (/ (dot-product data data) (length data))))
      (error 'no-such-region (list "region-rms" reg))))


(define* (window-samples snd chn)
  "(window-samples snd chn) -> samples in snd channel chn in current graph window"
  (let ((wl (left-sample snd chn))
	(wr (right-sample snd chn)))
    (channel->float-vector wl (+ 1 (- wr wl)) snd chn)))


(define (display-energy hook)
  ;; in this version, the y-zoom-slider controls the graph amp
  "(display-energy hook) is a lisp-graph-hook function to display the time domain data as energy (squared)"
  (let* ((snd (hook 'snd))
	 (chn (hook 'chn))
	 (ls (left-sample snd chn))
	 (rs (right-sample snd chn))
	 (datal (make-graph-data snd chn))
	 (data (if (float-vector? datal) datal (cadr datal)))
	 (sr (srate snd))
	 (y-max (y-zoom-slider snd chn)))
    (if (and data ls rs)
	(begin
	  (float-vector-multiply! data data)
	  (graph data "energy" (/ ls sr) (/ rs sr) 0.0 (* y-max y-max) snd chn #f)))))

;(hook-push lisp-graph-hook display-energy)


(define (display-db hook)
  "(display-db hook) is a lisp-graph-hook function to display the time domain data in dB"
  (let* ((snd (hook 'snd))
	 (chn (hook 'chn))
	 (datal (make-graph-data snd chn)))

      (if datal
	  (let* ((data (if (float-vector? datal) datal (cadr datal)))
		 (len (length data))
		 (sr (srate snd)))
	    (define (dB val)
	      (if (< val .001)
		  -60.0
		  (* 20.0 (log val 10))))
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (set! (data i) (+ 60.0 (dB (abs (data i))))))
	    (graph data "dB" 
		   (/ (left-sample snd chn) sr) (/ (right-sample snd chn) sr)  
		   0.0 60.0
		   snd chn)))))

;(hook-push lisp-graph-hook display-db)


(define (window-rms)
  "(window-rms) -> rms of data in currently selected graph window"
  (let* ((ls (left-sample))
	 (rs (right-sample))
	 (data (channel->float-vector ls (+ 1 (- rs ls))))
	 (len (length data)))
    (sqrt (/ (dot-product data data) len))))


(define (fft-peak hook)
  "(fft-peak hook) returns the peak spectral magnitude.  It is intended for use with after-transform-hook."
  (let ((snd (hook 'snd))
	(chn (hook 'chn)))
    (if (and (transform-graph?) 
	     (= (transform-graph-type) graph-once))
	(status-report 
	 (number->string (/ (* 2.0 (float-vector-peak (transform->float-vector snd chn))) 
			    (transform-size)))
	 snd))))

;(hook-push after-transform-hook fft-peak)


;;; -------- 'info' from extsnd.html using format --------

(define (finfo file)
  "(finfo file) -> description (as a string) of file"
  (format #f "~A: chans: ~D, srate: ~D, ~A, ~A, len: ~1,3F"
	  file
	  (channels file)
	  (srate file)
	  (mus-header-type-name (mus-sound-header-type file))
	  (mus-data-format-name (mus-sound-data-format file))
	  (/ (mus-sound-samples file)
	     (* 1.0 (channels file) (srate file)))))


;;; -------- Correlation --------
;;;
;;; correlation of channels in a stereo sound

(define (display-correlation hook)
  "(display-correlation hook) returns the correlation of snd's 2 channels (intended for use with graph-hook).  y0 and y1 are ignored."
  (let ((snd (hook 'snd)))
    (if (and (= (channels snd) 2)
	     (> (frames snd 0) 1)
	     (> (frames snd 1) 1))
	(let* ((ls (left-sample snd 0))
	       (rs (right-sample snd 0))
	       (ilen (+ 1 (- rs ls)))
	       (pow2 (ceiling (log ilen 2)))
	       (fftlen (floor (expt 2 pow2)))
	       (fftscale (/ 1.0 fftlen))
	       (rl1 (channel->float-vector ls fftlen snd 0))
	       (rl2 (channel->float-vector ls fftlen snd 1))
	       (im1 (make-float-vector fftlen))
	       (im2 (make-float-vector fftlen)))
	  (fft rl1 im1 1)
	  (fft rl2 im2 1)
	  (let ((tmprl (copy rl1))
		(tmpim (copy im1))
		(data3 (make-float-vector fftlen)))
	    (float-vector-multiply! tmprl rl2)     ; (* tempr1 tempr2)
	    (float-vector-multiply! tmpim im2)     ; (* tempi1 tempi2)
	    (float-vector-multiply! im2 rl1)       ; (* tempr1 tempi2)
	    (float-vector-multiply! rl2 im1)       ; (* tempr2 tempi1)
	    (float-vector-add! tmprl tmpim)        ; add the first two
	    (float-vector-subtract! im2 rl2)       ; subtract the 4th from the 3rd
	    (fft tmprl im2 -1)
	    (float-vector-add! data3 tmprl)        ; copy into data3
	    (float-vector-scale! data3 fftscale)   ; scale by fftscale
	    (graph data3 "lag time" 0 fftlen)))
	(status-report "display-correlation wants stereo input"))))

;(hook-push graph-hook display-correlation)

;;; The inner let body could also be:
;;;
;;;	   (graph
;;;	    (float-vector-scale! 
;;;	     (float-vector-add! data3
;;;		       (fft (float-vector-add! (float-vector-multiply! tmprl rl2) (float-vector-multiply! tmpim im2))
;;;			    (float-vector-subtract! (float-vector-multiply! im2 rl1) (float-vector-multiply! rl2 im1))
;;;			    -1))
;;;	     fftscale) "lag time" 0 fftlen2)))



;;; -------- set transform-size based on current time domain window size
;;;
;;; also zoom spectrum based on y-axis zoom slider

(define (zoom-spectrum hook)
  "(zoom-spectrum hook) sets the transform size to correspond to the time-domain window size (use with graph-hook)"
  (let ((snd (hook 'snd))
	(chn (hook 'chn)))
    (if (and (transform-graph? snd chn) 
	     (= (transform-graph-type snd chn) graph-once))
	(begin
	  (set! (transform-size snd chn)
		(expt 2 (ceiling (log (- (right-sample snd chn) (left-sample snd chn)) 2.0))))
	  (set! (spectrum-end snd chn) (y-zoom-slider snd chn))))))


;(hook-push graph-hook zoom-spectrum)



;;; -------- superimpose spectra of sycn'd sounds

(define (superimpose-ffts hook)
  "(superimpose-ffts hook) superimposes ffts of multiple (syncd) sounds (use with graph-hook)"
  (let ((maxsync (apply max (map sync (sounds))))
	(snd (hook 'snd))
	(chn (hook 'chn))
	(y0 (hook 'y0))
	(y1 (hook 'y1)))
    (if (and (> (sync snd) 0)
	     (> (right-sample snd chn) (left-sample snd chn))
	     (equal? snd (integer->sound (apply min (map (lambda (n) 
							   (if (= (sync snd) (sync n))
							       (sound->integer n)
							       (+ 1 maxsync)))
							 (sounds))))))
	(let* ((ls (left-sample snd chn))
	       (rs (right-sample snd chn))
	       (pow2 (ceiling (log (max 1 (- rs ls)) 2)))
	       (fftlen (floor (expt 2 pow2))))
	  (if (> pow2 2)
	      (let ((ffts ()))
		(for-each
		 (lambda (n)
		   (if (and (= (sync n) (sync snd))
			    (> (channels n) chn))
		       (set! ffts (append ffts (let ((fdr (channel->float-vector ls fftlen n chn))
						     (fdi (make-float-vector fftlen))
						     (spectr (make-float-vector (/ fftlen 2))))
						 (list (float-vector-add! spectr (spectrum fdr fdi #f 2))))))))
		 (sounds))
		(graph ffts "spectra" 0.0 0.5 y0 y1 snd chn)))))))

;(hook-push graph-hook superimpose-ffts)


;;; -------- translate mpeg input to 16-bit linear and read into Snd
;;;
;;; mpg123 with the -s switch sends the 16-bit (mono or stereo) representation of
;;;   an mpeg file to stdout.  There's also apparently a switch to write 'wave' output.

(define (mpg mpgfile rawfile)
  "(mpg file tmpname) converts file from MPEG to raw 16-bit samples using mpg123"
  (let* ((fd (open-input-file mpgfile "r"))
	 (b0 (char->integer (read-char fd)))
	 (b1 (char->integer (read-char fd)))
	 (b2 (char->integer (read-char fd)))
	 (b3 (char->integer (read-char fd))))
    (close-input-port fd)
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
(hook-push open-hook
           (lambda (hook)
	     (let ((filename (hook 'name)))
	       (if (= (mus-sound-header-type filename) mus-raw)
		   (read-ogg filename)))))
;; was returning #f?
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
      (system (format #f "speexenc ~A ~A" (file-name snd) (string-append (file-name snd) ".spx")))))


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
  ;;   to turn an AC3 file into something Snd can edit, /usr/local/bin/a52dec test.ac3 -o wav > test.wav
  (system (format #f "a52dec ~A" name)))


;;; -------- read ASCII files
;;;
;;; these are used by Octave (WaveLab) -- each line has one integer, apparently a signed short.

(define* (read-ascii in-filename (out-filename "test.snd") (out-type mus-next) (out-format mus-bshort) (out-srate 44100))
  "(read-ascii in-filename (out-filename \"test.snd\") (out-type mus-next) (out-format mus-bshort) (out-srate 44100)) tries to \
read an ASCII sound file"
  (let ((in-fd (open-input-file in-filename))
	(out-fd (new-sound out-filename out-type out-format out-srate 1 (format #f "created by read-ascii: ~A" in-filename)))
	(bufsize 8192))
    (as-one-edit
     (lambda ()
       (let ((data (make-float-vector bufsize))
	     (frame 0)
	     (short->float (/ 1.0 32768.0)))
	 (do ((loc 0 (+ loc 1))
	      (val (read-line in-fd) (read-line in-fd)))
	     ((eof-object? val)
	      (if (> loc 0)
		  (float-vector->channel data frame loc out-fd 0)))
	   (if (= loc bufsize)
	       (begin
		 (float-vector->channel data frame bufsize out-fd 0)
		 (set! frame (+ frame bufsize))
		 (set! loc 0)))
	   (float-vector-set! data loc (* (string->number val) short->float))))))
    (close-input-port in-fd)
    out-fd))



;;; -------- make dot size dependent on number of samples being displayed
;;; 
;;; this could be extended to set time-graph-style to graph-lines if many samples are displayed, etc

(define (auto-dot hook)
  "(auto-dot hook) sets the dot size depending on the number of samples being displayed (use with graph-hook)"
  (let* ((snd (hook 'snd))
	 (chn (hook 'chn))
	 (dots (- (right-sample snd chn)
		 (left-sample snd chn))))
    (if (> dots 100) 
	(set! (dot-size snd chn) 1)
	(if (> dots 50)
	    (set! (dot-size snd chn) 2)
	    (if (> dots 25)
		(set! (dot-size snd chn) 3)
		(set! (dot-size snd chn) 5))))))
    
;(hook-push graph-hook auto-dot)



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
    (define (find-leftmost-mark samps)
      (if (null? samps)
	  #f
	  (if (> (car samps) current-left-sample)
	      (car samps)
	      (find-leftmost-mark (cdr samps)))))
    (if (= (length chan-marks) 0)
	(status-report "no marks!")
	(let ((leftmost (find-leftmost-mark (map mark-sample chan-marks))))
	  (if (number? leftmost)
	      (begin
		(set! (left-sample keysnd keychn) leftmost)
		keyboard-no-action)
	      (status-report "no mark in window"))))))

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
	    (set! *selected-data-color* (if data-red? green red))
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

(define (all-chans)
  "(all-chans) -> two parallel lists, the first sound objects, the second channel numbers.  If we have 
two sounds open (indices 0 and 1 for example), and the second has two channels, (all-chans) returns '((#<sound 0> #<sound 1> #<sound 1>) (0 0 1))"
  (let ((sndlist ())
	(chnlist ()))
    (for-each (lambda (snd)
		(let ((chntop (- (channels snd) 1)))
		  (do ((i chntop (- i 1)))
		    ((< i 0))
		    (set! sndlist (cons snd sndlist))
		    (set! chnlist (cons i chnlist)))))
	      (sounds))
    (list sndlist chnlist)))

(define* (do-all-chans func origin)
  "(do-all-chans func edhist) applies func to all active channels, using edhist as the edit history 
indication: (do-all-chans (lambda (val) (* 2.0 val)) \"double all samples\")"
  (apply for-each (lambda (snd chn)
		    (map-channel func 0 #f snd chn #f origin))
	 (all-chans)))

(define (update-graphs)
  "(update-graphs) updates (redraws) all graphs"
  (apply for-each (lambda (snd chn)
		    (update-time-graph snd chn))
	 (all-chans)))

(define* (do-chans func origin)
  "(do-chans func edhist) applies func to all sync'd channels using edhist as the edit history indication"
  (let ((snc (sync)))
    (if (> snc 0)
	(apply for-each
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (map-channel func 0 #f snd chn #f origin)))
	       (all-chans))
	(snd-warning "sync not set"))))

(define* (do-sound-chans proc origin)
  "(do-sound-chans func edhist) applies func to all selected channels using edhist as the edit history indication"
  (let ((snd (selected-sound)))
    (if snd
	(begin
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn (channels snd)) #f)
	    (map-channel proc 0 #f snd chn #f origin)))
	(snd-warning "no selected sound"))))

(define (every-sample? proc)
  "(every-sample func) -> #t if func is not #f for all samples in the current channel, 
otherwise it moves the cursor to the first offending sample"
  (let ((reader (make-sampler))
	(len (frames)))
    (call-with-exit
     (lambda (quit)
       (do ((i 0 (+ i 1)))
	   ((= i len) #t)
	 (if (not (proc (next-sample reader)))
	     (begin
	       (set! (cursor) i)
	       (quit #f))))))))

(define (sort-samples nbins)
  "(sort-samples bins) provides a histogram in 'bins' bins"
  (let ((bins (make-vector nbins 0))
	(reader (make-sampler))
	(len (frames)))
    (do ((i 0 (+ i 1)))
	((= i len) bins)
      (let ((bin (floor (* (abs (next-sample reader)) nbins))))
	(set! (bins bin) (+ (bins bin) 1))))))



;;; -------- mix mono sound into stereo sound panning according to env

(define (place-sound mono-snd stereo-snd pan-env)
  "(place-sound mono-snd stereo-snd pan-env) mixes a mono sound into a stereo sound, splitting 
it into two copies whose amplitudes depend on the envelope 'pan-env'.  If 'pan-env' is 
a number, the sound is split such that 0 is all in channel 0 and 90 is all in channel 1."
  (let ((len (frames mono-snd)))
    (if (number? pan-env)
	(let ((pos (/ pan-env 90.0))
	      (reader0 (make-sampler 0 mono-snd))
	      (reader1 (make-sampler 0 mono-snd)))
	  (map-channel (lambda (y)
			 (+ y (* pos (read-sample reader1))))
		       0 len stereo-snd 1)
	  (map-channel (lambda (y)
			 (+ y (* (- 1.0 pos) (read-sample reader0))))
		       0 len stereo-snd 0))
	(let ((e0 (make-env pan-env :length len))
	      (e1 (make-env pan-env :length len))
	      (reader0 (make-sampler 0 mono-snd))
	      (reader1 (make-sampler 0 mono-snd)))
	  (map-channel (lambda (y)
			 (+ y (* (env e1) (read-sample reader1))))
		       0 len stereo-snd 1)
	  (map-channel (lambda (y)
			 (+ y (* (- 1.0 (env e0)) (read-sample reader0))))
		       0 len stereo-snd 0)))))



;;; -------- FFT-based editing
;;;

(define* (fft-edit bottom top snd chn)
  "(fft-edit low-Hz high-Hz snd chn) ffts an entire sound, removes all energy below low-Hz and all above high-Hz, 
then inverse ffts."
  (let* ((sr (srate snd))
	 (len (frames snd chn))
	 (fsize (expt 2 (ceiling (log len 2))))
	 (fsize2 (/ fsize 2))
	 (rdata (channel->float-vector 0 fsize snd chn))
	 (idata (make-float-vector fsize))
	 (lo (round (/ bottom (/ sr fsize))))
	 (hi (round (/ top (/ sr fsize)))))
    (fft rdata idata 1)
    (if (> lo 0)
	(begin
	  (vector-fill! rdata 0.0 0 lo)
	  (vector-fill! idata 0.0 0 lo)
	  (vector-fill! rdata (- fsize lo) fsize)
	  (vector-fill! idata (- fsize lo) fsize)))
    (if (< hi fsize2)
	(begin 
	  (vector-fill! rdata 0.0 hi (- fsize hi))
	  (vector-fill! idata 0.0 hi (- fsize hi))))
    (fft rdata idata -1)
    (float-vector-scale! rdata (/ 1.0 fsize))
    (float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "fft-edit ~A ~A" bottom top))))


(define* (fft-squelch squelch snd chn)
  "(fft-squelch squelch snd chn) ffts an entire sound, sets all bins to 0.0 whose energy is below squelch, then inverse ffts"
  (let* ((len (frames snd chn))
	 (fsize (expt 2 (ceiling (log len 2))))
	 (rdata (channel->float-vector 0 fsize snd chn))
	 (idata (make-float-vector fsize))
	 (fsize2 (/ fsize 2))
	 (scaler 1.0))
    (fft rdata idata 1)
    (let ((vr (copy rdata))
	  (vi (copy idata)))
      (rectangular->polar vr vi)
      (set! scaler (float-vector-peak vr)))
    (let ((scl-squelch (* squelch scaler)))
      (do ((i 0 (+ i 1)))
	  ((= i fsize))
	(if (< (magnitude (make-rectangular (rdata i) (idata i))) scl-squelch)
	    (begin
	      (set! (rdata i) 0.0)
	      (set! (idata i) 0.0))))
      (fft rdata idata -1)
      (float-vector-scale! rdata (/ 1.0 fsize)))
    (float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "fft-squelch ~A" squelch))
    scaler))


(define* (fft-cancel lo-freq hi-freq snd chn)
  "(fft-cancel lo-freq hi-freq snd chn) ffts an entire sound, sets the bin(s) representing lo-freq to hi-freq to 0.0, then inverse ffts"
  (let* ((sr (srate snd))
	 (len (frames snd chn))
	 (fsize (expt 2 (ceiling (log len 2))))
	 (rdata (channel->float-vector 0 fsize snd chn))
	 (idata (make-float-vector fsize)))
    (fft rdata idata 1)
    (let* ((hz-bin (/ sr fsize))
	   (lo-bin (round (/ lo-freq hz-bin)))
	   (hi-bin (round (/ hi-freq hz-bin))))
      (vector-fill! rdata 0.0 lo-bin hi-bin)
      (vector-fill! idata 0.0 lo-bin hi-bin)
      (vector-fill! rdata 0.0 (- fsize hi-bin) (- fsize lo-bin)))
    (fft rdata idata -1)
    (float-vector-scale! rdata (/ 1.0 fsize))
    (float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "fft-cancel ~A ~A" lo-freq hi-freq))))
    

;;; same idea but used to distinguish vowels (steady-state) from consonants

(define (ramp gen up)
  ;;  "(ramp gen up) is a kind of CLM generator that produces a ramp of a given length, then sticks at 0.0 or 1.0 until the 'up' argument changes"
  ;; gen is list: ctr size
  ;;  the idea here is that we want to ramp in or out a portion of a sound based on some
  ;;  factor of the sound data -- the ramp gen produces a ramp up when 'up' is #t, sticking
  ;;  at 1.0, and a ramp down when 'up' is #f, sticking at 0.0
  ;;
  ;; this could use the moving-average generator, though the resultant envelopes would be slightly less bumpy

  (environment-set! gen 'up up)
  (with-environment gen
    (set! val (min 1.0 (max 0.0 (+ val (if up incr (- incr))))))))

(define* (make-ramp (size 128))
  "(make-ramp (size 128)) returns a ramp generator"
  (environment (cons 'val 0.0) (cons 'incr (/ 1.0 size)) (cons 'up 1)))

;;; (let ((r (make-ramp))) (map-channel (lambda (y) (* y (ramp r (> (random 1.0) 0.5))))))

(define* (squelch-vowels snd chn)
  "(squelch-vowels snd chn) suppresses portions of a sound that look like steady-state"
  (let* ((fft-size 32)
	 (fft-mid (floor (/ fft-size 2)))
	 (rl (make-float-vector fft-size))
	 (im (make-float-vector fft-size))
	 (ramper (make-ramp 256)) ; 512 ok too
	 (peak (/ (maxamp) fft-mid))
	 (read-ahead (make-sampler 0 snd chn))
	 (ctr 0)
	 (in-vowel #f))
    (do ((i 0 (+ i 1)))
	((= i fft-size))
      (float-vector-set! rl i (read-sample read-ahead)))
    (set! ctr (- fft-size 1))
    (map-channel (lambda (y)
		   (set! ctr (+ ctr 1))
		   (if (= ctr fft-size)
		       (begin
			 (fft rl im 1)
			 (float-vector-multiply! rl rl)
			 (float-vector-multiply! im im)
			 (float-vector-add! rl im)
			 (set! in-vowel (> (+ (rl 0) (rl 1) (rl 2) (rl 3)) peak))
			 ;; fancier version checked here ratio of this sum and
			 ;;   sum of all rl vals, returned vowel if > 0.5
			 (set! ctr 0)
			 (do ((i 0 (+ i 1)))
			     ((= i fft-size))
			   (float-vector-set! rl i (read-sample read-ahead)))
			 (fill! im 0.0)))
		   (let ((rval (- 1.0 (ramp ramper in-vowel))))
		     ; squelch consonants if just ramp value (not 1.0-val)
		     ;(and (> rval 0.0) ; if this is included, the vowel-portions are omitted
		     (* y rval) ; squelch vowels 
		     ;(* y (+ (* 2 rval) .1)) ;accentuate consonants
		     ))
		 0 #f snd chn #f "squelch-vowels")))


(define* (fft-env-data fft-env snd chn)
  "(fft-env-data fft-env snd chn) applies fft-env as spectral env to current sound, returning float-vector of new data"
  (let* ((len (frames snd chn))
	 (fsize (expt 2 (ceiling (log len 2))))
	 (rdata (channel->float-vector 0 fsize snd chn))
	 (idata (make-float-vector fsize))
	 (fsize2 (/ fsize 2))
	 (e (make-env (concatenate-envelopes fft-env (reverse-envelope fft-env)) :length fsize))
	 (ve (make-float-vector fsize)))
    (fft rdata idata 1)
    (do ((i 0 (+ i 1)))
	((= i fsize))
      (float-vector-set! ve i (env e)))
    (float-vector-multiply! rdata ve)
    (float-vector-multiply! idata ve)
    (fft rdata idata -1)
    (float-vector-scale! rdata (/ 1.0 fsize))))


(define* (fft-env-edit fft-env snd chn)
  "(fft-env-edit fft-env snd chn) edits (filters) current chan using fft-env"
  (float-vector->channel (fft-env-data fft-env snd chn) 0 (- (frames) 1) snd chn #f (format #f "fft-env-edit '~A" fft-env)))


(define* (fft-env-interp env1 env2 interp snd chn)
  "(fft-env-interp env1 env2 interp snd chn) interpolates between two fft-filtered versions (env1 and env2 are the 
spectral envelopes) following interp (an env between 0 and 1)"
  (let* ((data1 (fft-env-data env1 snd chn))
	 (data2 (fft-env-data env2 snd chn))
	 (len (frames snd chn))
	 (new-data (make-float-vector len))
	 (e (make-env interp :length len))
	 (erev (make-env (scale-envelope interp -1.0 1.0) :length len))) ; 1.0 - e
    (do ((i 0 (+ i 1)))
	((= i len))
      (float-vector-set! new-data i
			 (+ (* (env erev) (float-vector-ref data1 i))
			    (* (env e) (float-vector-ref data2 i)))))
    (float-vector->channel new-data 0 (- len 1) snd chn #f (format #f "fft-env-interp '~A '~A '~A" env1 env2 interp))))


(define* (filter-fft flt (normalize #t) snd chn)
  "(filter-fft flt normalize snd chn) gets the spectrum of all the data in the given channel, \
applies the function 'flt' to it, then inverse ffts.  'flt' should take one argument, the \
current spectrum value.  (filter-fft (lambda (y) (if (< y .01) 0.0 y))) is like fft-squelch."
  (let* ((len (frames snd chn))
	 (mx (maxamp snd chn))
	 (fsize (expt 2 (ceiling (log len 2))))
	 (fsize2 (/ fsize 2))
	 ;(orig 0.0) (cur 0.0)
	 (rdata (channel->float-vector 0 fsize snd chn))
	 (idata (make-float-vector fsize))
	 (spect (snd-spectrum rdata rectangular-window fsize #t 1.0 #f normalize)) ; not in-place!
	 (vf (make-float-vector fsize)))

    (fft rdata idata 1)
    (flt (spect 0))
    (do ((i 1 (+ i 1))
	 (j (- fsize 1) (- j 1)))
	((= i fsize2))
      (float-vector-set! vf j (float-vector-set! vf i (/ (flt (spect i)) (max (spect i) 1e-5)))))
    (float-vector-multiply! rdata vf)
    (float-vector-multiply! idata vf)
    (fft rdata idata -1)
    (if (not (= mx 0.0))
	(let ((pk (float-vector-peak rdata)))
	  (float-vector->channel (float-vector-scale! rdata (/ mx pk)) 0 (- len 1) snd chn #f (format #f "filter-fft ~A" flt)))
	(float-vector->channel rdata 0 (- len 1) snd chn #f (format #f "filter-fft ~A" flt)))))

;; (let ((op (make-one-zero .5 .5))) (filter-fft op))
;; (let ((op (make-one-pole .05 .95))) (filter-fft op))
;; (filter-fft (lambda (y) (if (< y .1) 0.0 y)))
;; (let ((rd (make-sampler 0 0 0 1 0))) (scale-by 0) (filter-fft (lambda (y) (rd)))) ; treat original sound as spectrum
;; (filter-fft contrast-enhancement)
;; (filter-fft (lambda (y) (* y y y))) ; extreme low pass

#|
(let* ((ind (or (find-sound "now.snd")
		(open-sound "now.snd")))
       (mx (maxamp ind 0)))
  (do ((i 1 (+ i 1))
       (lo 0.0 (+ lo .1)))
      ((= i 8))
    (filter-fft (lambda (y) (contrast-enhancement y (+ 1.0 (* lo 30.0)))) #t ind 0))
  (let ((mixers (make-vector 8)))
    (do ((i 0 (+ i 1))
	 (lo 0.001 (+ lo .12)))
	((= i 8))
      (env-sound (list 0 0 lo 1 1 0) 0 #f 32.0 ind 0 (+ i 1))
      (set! (mixers i) (make-sampler 0 ind 0 1 (edit-position ind 0))))
    (scale-by 0.0)
    (map-channel
     (lambda (y)
       (let ((sum 0.0))
	 (do ((i 0 (+ i 1)))
	     ((= i 8) sum)
	   (set! sum (+ sum (read-sample (mixers i)))))))))
  (scale-to mx))
|#



(define* (fft-smoother cutoff start samps snd chn)
  "(fft-smoother cutoff start samps snd chn) uses fft-filtering to smooth a 
section: (float-vector->channel (fft-smoother .1 (cursor) 400) (cursor) 400)"
  (let* ((fftpts (floor (expt 2 (ceiling (log (+ 1 samps) 2)))))
	 (rl (channel->float-vector start fftpts snd chn))
	 (im (make-float-vector fftpts))
	 (top (floor (* fftpts cutoff))))
    (let ((old0 (rl 0))
	  (old1 (rl (- samps 1)))
	  (oldmax (float-vector-peak rl)))
      (fft rl im 1)
      (do ((i top (+ i 1)))
	  ((= i fftpts))
	(set! (rl i) 0.0)
	(set! (im i) 0.0))
      (fft rl im -1)
      (float-vector-scale! rl (/ 1.0 fftpts))
      (let ((newmax (float-vector-peak rl)))
	(if (= newmax 0.0)
	    rl
	    (begin
	      (if (> (/ oldmax newmax) 1.5)
		  (float-vector-scale! rl (/ oldmax newmax)))
	      (let* ((new0 (rl 0))
		     (new1 (rl (- samps 1)))
		     (offset0 (- old0 new0))
		     (offset1 (- old1 new1))
		     (incr (if (= offset1 offset0) 0.0 (/ (- offset1 offset0) samps))))
		(do ((i 0 (+ i 1))
		     (trend offset0 (+ trend incr)))
		    ((= i samps))
		  (set! (rl i) (+ (rl i) trend)))
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
  (let ((cs (make-comb-bank (vector (make-comb scaler (floor size))
				    (make-comb scaler (floor (* size .75)))
				    (make-comb scaler (floor (* size 1.2)))))))
    (lambda (x) 
      (* amp (comb-bank cs x)))))


;;; or change the comb length via an envelope:

(define (zcomb scaler size pm)
  "(zcomb scaler size pm) returns a comb filter whose length varies according to an 
envelope: (map-channel (zcomb .8 32 '(0 0 1 10)))"
  (define (max-envelope-1 e mx)
    (if (null? e)
	mx
	(max-envelope-1 (cddr e) (max mx (abs (cadr e))))))

  (let ((cmb (make-comb scaler size :max-size (floor (+ size 1 (max-envelope-1 pm 0.0)))))
	(penv (make-env pm :length (frames))))
    (lambda (x)
      (comb cmb x (env penv)))))


(define (notch-filter scaler size)
  "(notch-filter scaler size) returns a notch-filter: (map-channel (notch-filter .8 32))"
  (let ((cmb (make-notch scaler size)))
    (lambda (x) 
      (notch cmb x))))


(define (formant-filter radius frequency)
  "(formant-filter radius frequency) returns a formant generator: (map-channel (formant-filter .99 2400)). Faster 
is: (filter-sound (make-formant 2400 .99))"
  (let ((frm (make-formant frequency radius)))
    (lambda (x) 
      (formant frm x))))


;;; to impose several formants, just add them in parallel:

(define (formants r1 f1 r2 f2 r3 f3)
  "(formants r1 f1 r2 f2 r3 f3) returns 3 formant filters in parallel: (map-channel (formants .99 900 .98 1800 .99 2700))"
  (let ((fr1 (make-formant f1 r1))
	(fr2 (make-formant f2 r2))
	(fr3 (make-formant f3 r3)))
    (lambda (x)
      (+ (formant fr1 x)
	 (formant fr2 x)
	 (formant fr3 x)))))


(define (moving-formant radius move)
  "(moving-formant radius move) returns a time-varying (in frequency) formant filter: (map-channel (moving-formant .99 '(0 1200 1 2400)))"
  (let ((frm (make-formant (cadr move) radius))
	(menv (make-env move :length (frames))))
    (lambda (x)
      (let ((val (formant frm x)))
	(mus-set-formant-frequency frm (env menv))
	val))))


(define (osc-formants radius bases amounts freqs) ; changed to call map-channel itself, 21-Apr-05
  "(osc-formants radius bases amounts freqs) set up any number of independently oscillating 
formants, then calls map-channel: (osc-formants .99 (float-vector 400.0 800.0 1200.0) (float-vector 400.0 800.0 1200.0) (float-vector 4.0 2.0 3.0))"
  (let ((len (length bases)))
    (if (= len 3)
	;; this way is faster but verbose
	(let ((fa1 (amounts 0))
	      (fa2 (amounts 1))
	      (fa3 (amounts 2))
	      (frq1 (bases 0))
	      (frq2 (bases 1))
	      (frq3 (bases 2))
	      (fr1 (make-formant (bases 0) radius))
	      (fr2 (make-formant (bases 1) radius))
	      (fr3 (make-formant (bases 2) radius))
	      (o1 (make-oscil (freqs 0)))
	      (o2 (make-oscil (freqs 1)))
	      (o3 (make-oscil (freqs 2))))
	  (map-channel
	   (lambda (y)
	     (+ (formant fr1 y (hz->radians (+ frq1 (* fa1 (oscil o1)))))
		(formant fr2 y (hz->radians (+ frq2 (* fa2 (oscil o2)))))
		(formant fr3 y (hz->radians (+ frq3 (* fa3 (oscil o3)))))))))

	(let ((frms (make-vector len))
	      (oscs (make-vector len))
	      (amps (make-float-vector len 1.0)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (set! (frms i) (make-formant (bases i) radius))
	    (set! (oscs i) (make-oscil (freqs i))))
	  (let ((frms1 (make-formant-bank frms amps)))
	    (map-channel
	     (lambda (x)
	       (let ((val (formant-bank frms1 x)))
		 (do ((i 0 (+ i 1)))
		     ((= i len))
		   (mus-set-formant-frequency (vector-ref frms i)
			 (+ (bases i)
			    (* (amounts i) 
			       (oscil (oscs i))))))
		 val))))))))



;;; -------- echo

(define (echo scaler secs)
  "(echo scaler secs) returns an echo maker: (map-channel (echo .5 .5) 0 44100)"
  (let ((del (make-delay (round (* secs (srate))))))
    (lambda (inval)
      (+ inval (delay del (* scaler (+ (tap del) inval)))))))


(define (zecho scaler secs frq amp)
  "(zecho scaler secs freq amp) returns a modulated echo maker: (map-channel (zecho .5 .75 6 10.0) 0 65000)"
  (let* ((os (make-oscil frq))
	 (len (round (* secs (srate))))
	 (del (make-delay len :max-size (floor (+ len amp 1)))))
    (lambda (inval)
      (+ inval 
	 (delay del 
		(* scaler (+ (tap del) inval))
		(* amp (oscil os)))))))


(define (flecho scaler secs)
  "(flecho scaler secs) returns a low-pass filtered echo maker: (map-channel (flecho .5 .9) 0 75000)"
  (let ((flt (make-fir-filter :order 4 :xcoeffs (float-vector .125 .25 .25 .125)))
	(del (make-delay  (round (* secs (srate))))))
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
	 (genv (make-env gliss-env :length len)))
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

(define* (hello-dentist frq amp snd chn)
  "(hello-dentist frq amp snd chn) varies the sampling rate randomly, making a voice sound quavery: (hello-dentist 40.0 .1)"
  (let* ((rn (make-rand-interp :frequency frq :amplitude amp))
	 (i 0)
	 (len (frames))
	 (len1 (- len 1))
	 (in-data (channel->float-vector 0 len snd chn))
	 (rd (make-src :srate 1.0 
		       :input (lambda (dir) 
				(float-vector-ref in-data (min (max 0 (set! i (+ i dir))) len1))))))
    (map-channel
     (lambda (y)
       (src rd (rand-interp rn)))
     0 len snd chn #f (format #f "hello-dentist ~A ~A" frq amp))))


;;; a very similar function uses oscil instead of rand-interp, giving
;;; various "Forbidden Planet" sound effects:

(define* (fp sr osamp osfrq snd chn)
  "(fp sr osamp osfrq snd chn) varies the sampling rate via an oscil: (fp 1.0 .3 20)"
  (let* ((os (make-oscil osfrq))
	 (len (frames snd chn))
	 (sf (make-sampler 0 snd chn))
	 (s (make-src :srate sr :input (lambda (dir) (read-sample-with-direction sf dir)))))
    (map-channel
     (lambda (y)
       (src s (* osamp (oscil os))))
      0 #f snd chn #f (format #f "fp ~A ~A ~A" sr osamp osfrq))))
     
	    

;;; -------- compand, compand-channel

(define compand-table (float-vector -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250 
			   0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000))
;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")

(define (compand)
  "(compand) returns a compander: (map-channel (compand))"
  (lambda (inval)
    (array-interp compand-table (+ 8.0 (* 8.0 inval)) 17)))


;;; -------- shift pitch keeping duration constant
;;;
;;; both src and granulate take a function argument to get input whenever it is needed.
;;; in this case, src calls granulate which reads the currently selected file.
;;; CLM version is in expsrc.ins

(define* (expsrc rate snd chn)
  "(expsrc rate snd chn) uses sampling-rate conversion and granular synthesis 
to produce a sound at a new pitch but at the original tempo.  It returns a function for map-channel."
  (let* ((gr (make-granulate :expansion rate))
	 ;; this can be improved by messing with make-granulate's hop and length args
	 (sr (make-src :srate rate))
	 (vsize 1024)
	 (vbeg 0)
	 (v (channel->float-vector 0 vsize))
	 (inctr 0)
	 (f1 (lambda (dir)
	       (let ((val (v inctr)))
		 (set! inctr (+ inctr dir))
		 (if (>= inctr vsize)
		     (begin
		       (set! vbeg (+ vbeg inctr))
		       (set! inctr 0)
		       (set! v (channel->float-vector vbeg vsize snd chn))))
		 val)))
	 (f2 (lambda (dir)
	       (granulate gr f1))))
    (lambda (inval)
      (src sr 0.0 f2))))


;;; the next (expsnd) changes the tempo according to an envelope; the new duration
;;; will depend on the expansion envelope -- we integrate it to get
;;; the overall expansion, then use that to decide the new length.

(define* (expsnd gr-env snd chn)
  "(expsnd gr-env snd chn) uses the granulate generator to change tempo according to an envelope: (expsnd '(0 .5 2 2.0))"
  (let* ((dur (/ (* (/ (frames snd chn) (srate snd)) 
		    (integrate-envelope gr-env)) ; in env.scm
		 (envelope-last-x gr-env)))
	 (gr (make-granulate :expansion (cadr gr-env) 
			     :jitter 0
			     :input (make-sampler 0 snd chn)))
	 (ge (make-env gr-env :duration dur))
	 (sound-len (round (* (srate snd) dur)))
	 (len (max sound-len (frames snd chn)))
	 (out-data (make-float-vector len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (float-vector-set! out-data i (granulate gr))
      (set! (mus-increment gr) (env ge)))
    (float-vector->channel out-data 0 len snd chn #f (format #f "expsnd '~A" gr-env))))


;;; -------- cross-synthesis
;;;
;;; CLM version is in clm.html

(define (cross-synthesis cross-snd amp fftsize r)
  "(cross-synthesis cross-snd amp fftsize r) does cross-synthesis between 'cross-snd' (a sound object) and the currently 
selected sound: (map-channel (cross-synthesis (integer->sound 0) .5 128 6.0))"
  (let* ((freq-inc (/ fftsize 2))
	 (fdr #f)
	 (fdi (make-float-vector fftsize))
	 (spectr (make-float-vector freq-inc))
	 (inctr 0)
	 (ctr freq-inc)
	 (radius (- 1.0 (/ r fftsize)))
	 (bin (/ (srate) fftsize))
	 (formants (make-vector freq-inc))
	 (old-srate *clm-srate*))
    (set! *clm-srate* (srate))
    ;; if mus-srate is 44.1k and srate is 48k, make-formant thinks we're trying to go past srate/2
    ;;    and in any case it's setting its formants incorrectly for the actual output srate

    (do ((i 0 (+ i 1)))
	((= i freq-inc))
      (set! (formants i) (make-formant (* i bin) radius)))
    (set! formants (make-formant-bank formants spectr))
    (set! *clm-srate* old-srate)

    (lambda (inval)
      (if (= ctr freq-inc)
	  (begin
	    (set! fdr (channel->float-vector inctr fftsize cross-snd 0))
	    (set! inctr (+ inctr freq-inc))
	    (spectrum fdr fdi #f 2)
	    (float-vector-subtract! fdr spectr)
	    (float-vector-scale! fdr (/ 1.0 freq-inc))
	    (set! ctr 0)))
      (set! ctr (+ ctr 1))
      (float-vector-add! spectr fdr)
      (* amp (formant-bank formants inval)))))



;;; similar ideas can be used for spectral cross-fades, etc -- for example:

(define* (voiced->unvoiced amp fftsize r tempo snd chn)
  "(voiced->unvoiced amp fftsize r tempo snd chn) turns a vocal sound into whispering: (voiced->unvoiced 1.0 256 2.0 2.0)"
  (let* ((freq-inc (/ fftsize 2))
	 (fdr #f)
	 (fdi (make-float-vector fftsize))
	 (spectr (make-float-vector freq-inc))
	 (noi (make-rand (/ (srate snd) 3)))
	 (inctr 0)
	 (ctr 0)
	 (radius (- 1.0 (/ r fftsize)))
	 (bin (/ (srate snd) fftsize))
	 (len (frames snd chn))
	 (outlen (floor (/ len tempo)))
	 (hop (floor (* freq-inc tempo)))
	 (out-data (make-float-vector (max len outlen)))
	 (formants (make-vector freq-inc))
	 (old-peak-amp 0.0))

    (do ((i 0 (+ i 1)))
	((= i freq-inc))
      (set! (formants i) (make-formant (* i bin) radius)))
    (set! formants (make-formant-bank formants spectr))

    (do ((i 0 (+ i freq-inc)))
	((>= i outlen))
      (set! ctr (min (- outlen i) freq-inc))

      (set! fdr (channel->float-vector inctr fftsize snd chn))
      (let ((pk (float-vector-peak fdr)))
	(if (> pk old-peak-amp) (set! old-peak-amp pk)))
      (spectrum fdr fdi #f 2)
      (float-vector-subtract! fdr spectr)
      (float-vector-scale! fdr (/ 2.0 freq-inc))
      (set! inctr (+ hop inctr))

      (do ((k 0 (+ k 2))
	   (j i (+ j 2)))
	  ((>= k ctr))
	(float-vector-add! spectr fdr)
	(float-vector-set! out-data j (formant-bank formants (rand noi)))
	(float-vector-set! out-data (+ j 1) (formant-bank formants (rand noi)))))

    (float-vector-scale! out-data (* amp (/ old-peak-amp (float-vector-peak out-data))))
    (float-vector->channel out-data 0 (max len outlen) snd chn)))


;;; very similar but use ncos (glottal pulse train?) instead of white noise

(define* (pulse-voice cosines (freq 440.0) (amp 1.0) (fftsize 256) (r 2.0) snd chn)
  "(pulse-voice cosines (freq 440) (amp 1.0) (fftsize 256) (r 2.0) snd chn) uses ncos to manipulate speech sounds"
  (let* ((freq-inc (/ fftsize 2))
	 (fdr #f)
	 (fdi (make-float-vector fftsize))
	 (spectr (make-float-vector freq-inc))
	 (pulse (make-ncos freq cosines))
	 (inctr 0)
	 (ctr 0)
	 (radius (- 1.0 (/ r fftsize)))
	 (bin (/ (srate snd) fftsize))
	 (len (frames snd chn))
	 (out-data (make-float-vector len))
	 (formants (make-vector freq-inc))
	 (old-peak-amp 0.0))

    (do ((i 0 (+ i 1)))
	((= i freq-inc))
      (set! (formants i) (make-formant (* i bin) radius)))
    (set! formants (make-formant-bank formants spectr))

    (do ((i 0 (+ i freq-inc)))
	((>= i len))
      (set! ctr (min (- len i) freq-inc))

      (set! fdr (channel->float-vector inctr fftsize snd chn))
      (let ((pk (float-vector-peak fdr)))
	(if (> pk old-peak-amp) (set! old-peak-amp pk)))
      (spectrum fdr fdi #f 2)
      (float-vector-subtract! fdr spectr)
      (float-vector-scale! fdr (/ 1.0 freq-inc))
      (set! inctr (+ freq-inc inctr))

      (do ((k 0 (+ k 1))
	   (j i (+ j 1)))
	  ((= k ctr))
	(float-vector-add! spectr fdr)
	(float-vector-set! out-data j (formant-bank formants (ncos pulse)))))

    (float-vector-scale! out-data (* amp (/ old-peak-amp (float-vector-peak out-data))))
    (float-vector->channel out-data 0 len snd chn)))

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
	 (cnv (make-convolve :filter (channel->float-vector 0 flt-len snd0)
			     :input (make-sampler 0 snd1)))
	 (out-data (make-float-vector total-len)))
    (do ((i 0 (+ i 1)))
	((= i total-len))
      (float-vector-set! out-data i (convolve cnv)))
    (float-vector-scale! out-data amp)
    (let ((max-samp (float-vector-peak out-data)))
      (float-vector->channel out-data 0 total-len snd1)
      (if (> max-samp 1.0) (set! (y-bounds snd1) (list (- max-samp) max-samp)))
      max-samp)))

#|
;;; -------- time varying FIR filter (not very interesting...)

(define (fltit)
  "(fltit) returns a time-varying filter: (map-channel (fltit))"
  (let* ((coeffs (float-vector .1 .2 .3 .4 .4 .3 .2 .1))
	 (flt (make-fir-filter 8 coeffs))
	 (es (make-vector 8)))
    (do ((i 0 (+ i 1)))
	((= i 8))
      (set! (es i) (make-env (list 0 (coeffs i) 1 0) :length 100)))
    (set! (es 5) (make-env '(0 .4 1 1) :duration 1.0))
    (lambda (x)
      (let ((val (fir-filter flt x))
	    (xcof (mus-xcoeffs flt)))
	(do ((i 0 (+ i 1)))
	    ((= i 8))
	  (float-vector-set! xcof i (env (vector-ref es i))))
	val))))

;;; for something this simple (like a notch filter), we can use a two-zero filter:
;
;(define flt (make-two-zero 550.0 .99))
;
;;; this is a strong notch filter centered at 550 Hz
;
;(map-channel (lambda (x) (two-zero flt x)))
;
;;; similarly make-two-pole (or better, make-formant)
;;; can be used for resonances.
|#


;;; -------- locate-zero (Anders Vinjar)

(define (locate-zero limit)
  "(locate-zero limit) looks for successive samples that sum to less than 'limit', moving the cursor if successful"
  (let* ((start (cursor))
	 (sf (make-sampler start)))
    (do ((n start (+ 1 n))
	 (val0 (abs (next-sample sf)) val1)
	 (val1 (abs (next-sample sf)) (abs (next-sample sf))))
	((or (sampler-at-end? sf)
	     (< (+ val0 val1) limit))
	 (set! (cursor) n)
	 n))))


;;; -------- sound interp
;;;
;;; make-sound-interp sets up a sound reader that reads a channel at an arbitary location,
;;;   interpolating between samples if necessary, the corresponding "generator" is sound-interp

(define* (make-sound-interp start snd chn)
  "(make-sound-interp start snd chn) -> an interpolating reader for snd's channel chn"
  (let* ((data (channel->float-vector 0 #f snd chn))
	 (size (length data)))
    (lambda (loc)
      (array-interp data loc size))))

(define (sound-interp func loc) ;make it look like a clm generator
  "(sound-interp func loc) -> sample at loc (interpolated if necessary) from func created by make-sound-interp"
  (func loc))

#|
(define test-interp
  (lambda (freq)
    ;; use a sine wave to lookup the current sound
    (let ((osc (make-oscil :frequency freq :initial-phase (+ pi (/ pi 2))))
	  (reader (make-sound-interp 0 0 0)) 
	  (len (- (frames 0 0) 1)))
      (map-channel (lambda (val) 
		     (sound-interp reader (* len (+ 0.5 (* 0.5 (oscil osc))))))))))

;;; (test-interp 0.5)

;;; our FM index is len * 0.5 * (hz->radians freq)

(define (sound-via-sound snd1 snd2) ; "sound composition"??
  (let* ((intrp (make-sound-interp 0 snd1 0))
	 (len (- (frames snd1 0) 1))
	 (rd (make-sampler 0 snd2 0))
	 (mx (maxamp snd2 0)))
      (map-channel (lambda (val) 
		     (sound-interp intrp (floor (* len (* 0.5 (+ 1.0 (/ (read-sample rd) mx))))))))))
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

(define* (env-sound-interp envelope (time-scale 1.0) snd chn)
  "(env-sound-interp env (time-scale 1.0) snd chn) reads snd's channel chn according to env and time-scale"
  ;; since the old/new sounds can be any length, we'll write a temp file rather than trying to use map-channel

  (let* ((len (frames snd chn))
	 (newlen (floor (* time-scale len))))
    (let ((new-snd (with-sound (:output (snd-tempnam) :to-snd #f :srate (srate snd))
		     (let ((data (channel->float-vector 0 #f snd chn))
			   (read-env (make-env envelope :length (+ 1 newlen) :scaler len)))
		       (do ((i 0 (+ i 1)))
			   ((= i newlen))
			 (outa i (array-interp data (env read-env) len)))))))
	(set-samples 0 newlen new-snd snd chn #t
		     (format #f "env-sound-interp '~A ~A" envelope time-scale)
		     0 current-edit-position #t))))


;;; (env-sound-interp '(0 0 1 1 2 0) 2.0)



;;; here's a very similar function that uses granular synthesis to move at a varying tempo through a sound

(define* (granulated-sound-interp envelope (time-scale 1.0) (grain-length 0.10) (grain-envelope '(0 0 1 1 2 1 3 0)) (output-hop 0.05) snd chn)

  "(granulated-sound-interp envelope (time-scale 1.0) (grain-length 0.10) (grain-envelope '(0 0 1 1 2 1 3 0)) (output-hop 0.05) snd chn) reads \
the given channel following 'envelope' (as in env-sound-interp), using grains to create the re-tempo'd read"

  (let* ((len (frames snd chn))
	 (newlen (floor (* time-scale len))))
    (let ((read-env (make-env envelope :length newlen :scaler len))
	  (grain-frames (round (* grain-length (srate snd))))
	  (hop-frames (round (* output-hop (srate snd))))
	  (num-readers (ceiling (/ grain-length output-hop)))
	  (cur-readers 0)
	  (next-reader 0)
	  (jitter (* (srate snd) .005)))

      (let ((readers (make-vector num-readers #f))
	    (grain-envs (make-vector num-readers #f)))
	(do ((i 0 (+ i 1)))
	    ((= i num-readers))
	  (set! (grain-envs i) (make-env grain-envelope :length grain-frames)))

	(let ((new-snd (with-sound (:output (snd-tempnam) :to-snd #f :srate (srate snd))

			 (do ((i 0 (+ i hop-frames)))
			     ((>= i newlen))
			   (let ((start i)
				 (stop (min newlen (+ i hop-frames)))
				 (e #f)
				 (r #t))
	
			     (set! (mus-location read-env) i)
			     (let ((position-in-original (env read-env)))
			       (set! (readers next-reader)
				     (make-sampler (max 0 (round (+ position-in-original (mus-random jitter)))) snd chn))
			       (mus-reset (grain-envs next-reader)) ; restart grain env
			       (set! next-reader (modulo (+ next-reader 1) num-readers))
			       (if (< cur-readers next-reader) (set! cur-readers next-reader)))

			     (do ((k 0 (+ k 1)))
				 ((= k cur-readers))
			       (set! e (grain-envs k))
			       (set! r (readers k))
			       (do ((j start (+ j 1)))
				   ((= j stop))
				 (outa j (* (env e) (next-sample r))))))))))

	  (set-samples 0 newlen new-snd snd chn #t
		       (format #f "granulated-sound-interp '~A ~A ~A ~A ~A" envelope time-scale grain-length grain-envelope output-hop)
		       0 current-edit-position #t))))))

;;; (granulated-sound-interp '(0 0 1 .1 2 1) 1.0 0.2 '(0 0 1 1 2 0))
;;; (granulated-sound-interp '(0 0 1 1) 2.0)
;;; (granulated-sound-interp '(0 0 1 .1 2 1) 1.0 0.2 '(0 0 1 1 2 0) 0.02)




;;; -------- filtered-env 

(define* (filtered-env e snd chn)
  "(filtered-env env snd chn) is a time-varying one-pole filter: when env is at 1.0, no filtering, 
as env moves to 0.0, low-pass gets more intense; amplitude and low-pass amount move together"
  (let* ((samps (frames))
	 (flt (make-one-pole 1.0 0.0))
	 (amp-env (make-env e :length samps)))
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
(define red-text (format #f "~C[31m" #\escape))
(define normal-text (format #f "~C[0m" #\escape))

;;; there are a bunch of these:

(define black-on-red-text (format #f "~C[30m~C[41m" #\escape #\escape))

;;; or perhaps more convenient:

(define black-fg (format #f "~C[30m" #\escape))  (define black-bg (format #f "~C[40m" #\escape))
(define red-fg (format #f "~C[31m" #\escape))    (define red-bg (format #f "~C[41m" #\escape))
(define green-fg (format #f "~C[32m" #\escape))  (define green-bg (format #f "~C[42m" #\escape))
(define yellow-fg (format #f "~C[33m" #\escape)) (define yellow-bg (format #f "~C[43m" #\escape))
(define blue-fg (format #f "~C[34m" #\escape))   (define blue-bg (format #f "~C[44m" #\escape))
;;; etc (magenta: 35 cyan: 36 white: 37 default: 39)

(define bold-text (format #f "~C[1m" #\escape))       (define unbold-text (format #f "~C[22m" #\escape))  
(define underline-text (format #f "~C[4m" #\escape))  (define ununderline-text (format #f "~C[24m" #\escape))  
(define blink-text (format #f "~C[5m" #\escape))      (define unblink-text (format #f "~C[25m" #\escape))  
|#


;;; -------- remove-clicks 

(define (find-click loc)
  "(find-click loc) finds the next click starting at 'loc'"
  (let ((reader (make-sampler loc))
	(mmax (make-moving-max 10))
	(samp0 0.0)
	(samp1 0.0)
	(samp2 0.0)
	(len (frames)))
    (call-with-exit
     (lambda (return)
       (do ((ctr loc (+ ctr 1)))
	   ((= ctr len) #f)
	 (set! samp0 samp1)
	 (set! samp1 samp2)
	 (set! samp2 (next-sample reader))
	 (let ((local-max (max .1 (moving-max mmax samp0))))
	   (if (and (> (abs (- samp0 samp1)) local-max)
		    (> (abs (- samp1 samp2)) local-max)
		    (< (abs (- samp0 samp2)) (/ local-max 2)))
	       (return (- ctr 1)))))))))


(define (remove-clicks)
  "(remove-clicks) tries to find and smooth-over clicks"
  ;; this is very conservative -- the click detection limits above could be set much tighter in many cases
  (define (remove-click loc)
    (let ((click (find-click loc)))
      (if click
	  (begin
	    (smooth-sound (- click 2) 4)
	    (remove-click (+ click 2))))))
  (remove-click 0))


;;; -------- searching examples (zero+, next-peak)

(define (search-for-click)
  "(search-for-click) looks for the next click (use with C-s)"
  (let ((samp0 0.0)
	(samp1 0.0)
	(samp2 0.0)
	(mmax (make-moving-max 10)))
    (lambda (val)
      (set! samp0 samp1)
      (set! samp1 samp2)
      (set! samp2 val)
      (let ((local-max (max .1 (moving-max mmax samp0))))
	(and (>= (abs (- samp0 samp1)) local-max)
	     (>= (abs (- samp1 samp2)) local-max)
	     (<= (abs (- samp0 samp2)) (/ local-max 2)))))))


(define (zero+)
  "(zero+) finds the next positive-going zero crossing (if searching forward) (for use with C-s)"
  (let ((lastn 0.0))
    (lambda (n)
      (let ((rtn (and (< lastn 0.0)
		      (>= n 0.0))))
	(set! lastn n)
	rtn))))


(define (next-peak)
  "(next-peak) finds the next max or min point in the time-domain waveform (for use with C-s)"
  (let ((last0 #f)
	(last1 #f))
    (lambda (n)
      (let ((rtn (and (number? last0)
		      (or (and (< last0 last1) (> last1 n))
			  (and (> last0 last1) (< last1 n))))))
	(set! last0 last1)
	(set! last1 n)
	rtn))))


(define (find-pitch pitch)
  "(find-pitch pitch) finds the point in the current sound where 'pitch' (in Hz) predominates -- C-s (find-pitch 300) 
In most cases, this will be slightly offset from the true beginning of the note"

  (define (interpolated-peak-offset la pk ra)
    (let ((logla (log (/ (max la .0000001) pk) 10))
	  (logra (log (/ (max ra .0000001) pk) 10)))
      (/ (* 0.5 (- logla logra))
	 (+ logla logra))))

  (let ((data (make-float-vector (transform-size)))
	(data-loc 0))
    (lambda (n)
      (set! (data data-loc) n)
      (set! data-loc (+ data-loc 1))
      (let ((rtn #f))
	(if (= data-loc (transform-size))
	    (begin
	      (set! data-loc 0)
	      (if (> (float-vector-peak data) .001) ;ignore noise sections??
		  (let ((spectr (snd-spectrum data rectangular-window (transform-size)))
			(pk 0.0)
			(pkloc 0))
		    (let ((pit 
			   (do ((i 0 (+ i 1)))
			       ((= i (/ (transform-size) 2)) 
				(if (or (= pk 0.0)
					(= pkloc 0))
				    0.0
				    (/ (* (+ pkloc
					     (interpolated-peak-offset (spectr (- pkloc 1))
								       pk
								       (spectr (+ 1 pkloc))))
					  (srate))
				       (transform-size))))
			     (if (> (spectr i) pk)
				 (begin
				   (set! pk (spectr i))
				   (set! pkloc i))))))
		      (if (< (abs (- pitch pit)) (/ (srate) (* 2 (transform-size)))) ; uh... why not do it direct?
			  (set! rtn #t)))))
	       (fill! data 0.0)))
	 rtn))))


;;; -------- file->floats and a sort of cue-list, I think

(define (file->floats file) (samples 0 (frames file) file))


(define* (add-notes notes snd chn)
  "(add-notes notes snd chn) adds (mixes) 'notes' which is a list of lists of the form: file (offset 0.0) (amp 1.0) 
starting at the cursor in the currently selected channel: (add-notes '((\"oboe.snd\") (\"pistol.snd\" 1.0 2.0)))"
  (let ((start (cursor snd chn)))
    (as-one-edit
     (lambda ()
       (for-each 
	(lambda (note)
	  (let* ((file (car note))
		 (offset (if (> (length note) 1) (cadr note) 0.0))
		 (amp (if (> (length note) 2) (caddr note) #f))
		 (beg (+ start (floor (* (srate snd) offset)))))
	    (if (and (number? amp)
		     (not (= amp 1.0)))
		(mix-float-vector (float-vector-scale! (file->floats file) amp) beg snd chn #f "add-notes")
		(mix file beg 0 snd chn #f))))
	notes))
     (format #f "add-notes '~S" notes))))


(define (region-play-list data)
  "(region-play-list data): 'data' is list of lists (list (list reg time)...), time in secs, setting up 
a sort of play list: (region-play-list (list (list reg0 0.0) (list reg1 0.5) (list reg2 1.0) (list reg0 1.0)))"
  (for-each
   (lambda (tone)
     (let ((time (floor (* 1000 (cadr tone))))
	   (region (car tone)))
       (if (region? region)
	   (in time (lambda () (play region))))))
   data))


(define (region-play-sequence data)
  "(region-play-sequence data): 'data' is list of regions which will be played one after the other: (region-play-sequence (list reg0 reg2 reg1))"
  (region-play-list
   (let ((time 0.0))
     (map 
      (lambda (id)
	(let ((cur time))
	  (set! time (+ time (/ (frames id) (srate id))))
	  (list cur id)))
      data))))


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
	(do ((i 0 (+ i 1)))
	    ((= i (string-length curfile)))
	  (if (char=? (curfile i) #\/)
	      (set! last-slash i)))
	(substring curfile (+ 1 last-slash))))

    (define (directory-from-path curfile)
      (let ((last-slash 0))
	(do ((i 0 (+ i 1)))
	    ((= i (string-length curfile)))
	  (if (char=? (curfile i) #\/)
	      (set! last-slash i)))
	(substring curfile 0 last-slash)))

    (define (find-next-file)
      ;; find the next file in the sorted list, with wrap-around
      (let ((choose-next (not (string? last-file-opened)))
	    (just-filename (file-from-path last-file-opened)))
	(call-with-exit
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
      (set! current-sorted-files (sort! (sound-files-in-directory dir) string<?)))
      
    (define (get-current-directory filename)
      (set! last-file-opened filename)
      (display last-file-opened)
      (let ((new-path (directory-from-path (file-name filename))))
	(if (or (not (string? current-directory))
		(not (string=? current-directory new-path)))
	    (get-current-files new-path)))
      #f)

    (lambda ()
      (if (not (member get-current-files (hook-functions open-hook)))
	  (hook-push open-hook (lambda (hook) (get-current-directory (hook 'name)))))
      (if (and (not (string? last-file-opened))
	       (not (null? (sounds))))
	  (set! last-file-opened (file-name (or (selected-sound)
						(car (sounds))))))
      (if (not current-directory)
	  (if (null? (sounds))
	      (get-current-files (getcwd))
	      (get-current-files (directory-from-path last-file-opened))))
      (if (null? current-sorted-files)
	  (error 'no-such-file (list "open-next-file-in-directory" current-directory))
	  (let ((next-file (find-next-file)))
	    (if (find-sound next-file)
		(error 'file-already-open (list "open-next-file-in-directory" next-file))
		(begin
		  (if (not (null? (sounds)))
		      (close-sound (or (selected-sound)
				       (car (sounds)))))
		  (open-sound next-file)))))
      #t)))


(define (click-middle-button-to-open-next-file-in-directory)
  "(click-middle-button-to-open-next-file-in-directory) adds open-next-file-in-directory to the mouse-click-hook"
  (hook-push mouse-click-hook
	     (lambda (hook)
	       (if (= (hook 'button) 2)
		   (set! (hook 'result) (open-next-file-in-directory))))))


;;; -------- chain-dsps

(define* (chain-dsps beg dur :rest dsps)
  "(chain-dsps beg dur :rest dsps) sets up a generator patch from its arguments"

  ;; assume the dsps are already made, 
  ;;        the envs are present as break-point lists
  ;;        the calls are ordered out->in (or last first)
  ;; we take this list and create and evaluate a new function

  (let ((dsp-chain (list->vector (reverse (map (lambda (gen)
						 (if (list? gen)
						     (make-env gen :duration dur)
						     gen))
					       dsps))))
	 (start (seconds->samples beg))
	 (samps (seconds->samples dur))
	 (body 0.0)
	 (closure ()))
    (let ((end (+ start samps))
	  (len (length dsp-chain)))

      ;; create the let variable list and lambda body of our new function
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(let ((g (dsp-chain i))
	      (gname (string->symbol (format #f "g~D" i))))
	  (set! closure (cons `(,gname (dsp-chain ,i)) closure))
	  (if (env? g)
	      (set! body `(* ,body (env ,gname)))
	      (if (readin? g)
		  (set! body `(+ ,body (readin ,gname)))
		  (if (mus-generator? g)
		      (set! body (list (string->symbol (mus-name g)) gname body))
		      (set! body (list gname body)))))))

      ;; now patch the two together (the apply let below) and evaluate the resultant thunk
      ((apply let closure 
	      `((lambda ()
		  (do ((k ,start (+ k 1)))
		      ((= k ,end))
		    (outa k ,body)))))))))
#|
(with-sound ()
  (chain-dsps 0 1.0 '(0 0 1 .5 2 0) (make-oscil 440))
  (chain-dsps 1 1.0 '(0 0 1 4 2 0) (make-one-zero .5) (make-readin "oboe.snd"))
  ;; next call not currently optimizable
  (chain-dsps 2 1.0 '(0 0 1 .5 2 0) (let ((osc1 (make-oscil 220)) 
					  (osc2 (make-oscil 440))) 
				      (lambda (val) (+ (osc1 val) 
						       (osc2 (* 2 val)))))))
|#



;;; amplitude-modulate-channel could be (lambda (y data forward) (* y 0.5 (+ 1.0 (sin angle))) etc ...)


;;; -------- re-order channels 

(define (scramble-channels . new-order)

  "scramble-channels can arbitrarily re-order a sound's channels. The new channel order is \
passed as the arguments so to end with channel 3 in channel 0, 2 in 1, 0 in 2, and 1 in 3, (scramble-channels 3 2 0 1)"
  
  (define (find-chan chans chan len)
    (let ((pos #f))
      (do ((i 0 (+ i 1)))
	  ((or pos (= i len)) pos)
	(if (= (chans i) chan)
	    (set! pos i)))))

  (define (scramble-channels-1 cur-chans end-chans chans loc)
    (if (> chans loc)
	(let* ((end-chan (end-chans loc)) ; we want this channel at loc
	       (cur-chan (cur-chans loc)) ; this (original) channel is currently at loc
	       (end-loc (find-chan cur-chans end-chan chans))) ; where is end-chan currently?
	  ;; end-chan goes in cur-chan's slot
	  (if (not (= cur-chan end-chan))
	      (begin
		(swap-channels #f end-loc #f loc)
		(set! (cur-chans end-loc) cur-chan)
		(set! (cur-chans loc) end-chan)))
	  (scramble-channels-1 cur-chans end-chans chans (+ 1 loc)))))

  (let ((len (length new-order)))
    (if (> len 1)
	(let ((end-chans (list->vector new-order))
	      (cur-chans (make-vector len)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (set! (cur-chans i) i))
	  (scramble-channels-1 cur-chans end-chans len 0)))))


(define (scramble-channel silence-1)
  ;; (scramble-channel .01)
  (let ((buffer (make-moving-average 128))
	(silence (/ silence-1 128))
	(edges ())
	(in-silence #t)
	(old-max (max-regions))
	(old-tags (with-mix-tags)))
    (dynamic-wind
     (lambda ()
       (set! (max-regions) 1024)
       (set! (with-mix-tags) #f))
     (lambda ()
       (let ((len (frames))
	     (reader (make-sampler)))
	 (do ((i 0 (+ i 1)))
	     ((= i len))
	   (let ((y (next-sample reader)))
	     (let* ((sum-of-squares (moving-average buffer (* y y)))
		    (now-silent (< sum-of-squares silence)))
	       (if (not (eq? in-silence now-silent))
		   (set! edges (cons i edges)))
	       (set! in-silence now-silent)))))
       (set! edges (append (reverse edges) (list (frames))))
       (let* ((len (length edges))
	      (pieces (make-vector len #f))
	      (start 0)
	      (ctr 0))
	 (for-each
	  (lambda (end)
	    (set! (pieces ctr) (make-region start end))
	    (set! ctr (+ ctr 1))
	    (set! start end))
	  edges)
	 (set! start 0)
	 (as-one-edit
	  (lambda()
	    (scale-by 0.0)
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (let* ((this (random len))
		     (reg (pieces this)))
		(set! (pieces this) #f)
		(if (not reg)
		    (begin
		      (do ((j (+ 1 this) (+ j 1)))
			  ((or (= j len)
			       reg))
			(set! reg (pieces j))
			(if reg (set! (pieces j) #f)))
		      (if (not reg)
			  (do ((j (- this 1) (- j 1)))
			      ((or (< j 0)
				   reg))
			    (set! reg (pieces j))
			    (if reg (set! (pieces j) #f))))))
		(mix-region reg start)
		(set! start (+ start (frames reg)))
		(forget-region reg)))))))
     (lambda ()
       (set! (with-mix-tags) old-tags)
       (set! (max-regions) old-max)))))
    
    
;; -------- reorder blocks within channel

(define* (reverse-by-blocks block-len snd chn)
  "(reverse-by-blocks block-len snd chn): divide sound into block-len blocks, recombine blocks in reverse order"
  (let* ((len (frames snd chn))
	 (num-blocks (floor (/ len (* (srate snd) block-len)))))
    (if (> num-blocks 1)
	(let* ((actual-block-len (ceiling (/ len num-blocks)))
	       (rd (make-sampler (- len actual-block-len) snd chn))
	       (beg 0)
	       (ctr 1))
	  (map-channel
            (lambda (y)
	      (let ((val (read-sample rd)))
		(if (< beg 10) ; ramp start and end to avoid clicks (might want to mix with next section)
		    (set! val (* val beg .1))
		    (if (> beg (- actual-block-len 10))
			(set! val (* val (- actual-block-len beg) .1))))
		(set! beg (+ beg 1))
		(if (= beg actual-block-len)
		    (begin
		      (set! ctr (+ ctr 1))
		      (set! beg 0)
		      (set! rd (make-sampler (max 0 (- len (* ctr actual-block-len))) snd chn))))
		val))
	    0 #f snd chn #f (format #f "reverse-by-blocks ~A" block-len))))))


(define* (reverse-within-blocks block-len snd chn)
  "(reverse-within-blocks block-len snd chn): divide sound into blocks, recombine in order, but each block internally reversed"
  (let* ((len (frames snd chn))
	 (num-blocks (floor (/ len (* (srate snd) block-len)))))
    (if (> num-blocks 1)
	(let ((actual-block-len (ceiling (/ len num-blocks)))
	      (no-clicks-env (list 0.0 0.0  .01 1.0  .99 1.0  1.0 0.0)))
	  (as-one-edit
	   (lambda ()
	     (do ((beg 0 (+ beg actual-block-len)))
		 ((>= beg len))
	       (reverse-channel beg actual-block-len snd chn)
	       (env-channel no-clicks-env beg actual-block-len snd chn)))
	   (format #f "reverse-within-blocks ~A" block-len)))
	(reverse-channel 0 #f snd chn))))

  
;;; -------- channel-clipped?

#|
(define* (channel-clipped? snd chn)
  "(channel-clipped? snd chn) returns the sample number if it finds clipping"
  (let ((last-y 0.0)
	(len (frames snd chn))
	(reader (make-sampler 0 snd chn)))
    (call-with-exit
     (lambda (quit)
       (do ((i 0 (+ i 1)))
	   ((= i len) #f)
	 (let ((y (next-sample reader)))
	   (if (and (>= (abs y) 0.9999)
		    (>= (abs last-y) 0.9999))
	       (quit i)
	       (set! last-y y))))))))
|#
;;; not pretty but faster:

(define* (channel-clipped? snd chn)
  "(channel-clipped? snd chn) returns the sample number if it finds clipping"
  (do ((pos (scan-channel (lambda (y) (>= (abs y) 0.9999)) 0 #f snd chn) 
	    (scan-channel (lambda (y) (>= (abs y) 0.9999)) (+ pos 1) #f snd chn)))
      ((or (not pos)
	   (>= (abs (sample (+ pos 1) snd chn)) 0.9999))
       pos))) ; or (and pos (+ pos 1)) to mimic the old version


;;; -------- sync-everything

(define (sync-everything)
  "(sync-everything) sets the sync fields of all currently open sounds to the same, unique value"
  (let ((new-sync (+ 1 (sync-max))))
    (for-each
     (lambda (snd)
       (set! (sync snd) new-sync))
     (sounds))))
