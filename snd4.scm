;;; Snd-4 compatibility stuff

(use-modules (ice-9 format) (ice-9 optargs))

(if #f
    ;; these are the Snd-4 names 
    (begin
      (define smooth            smooth-sound)
      (define cut               delete-selection)
      (define call-apply        apply-controls)
      (define (open-alternate-sound file) (close-sound) (open-sound file))
      (define normalize-view    equalize-panes)
      (define save-control-panel save-controls)
      (define restore-control-panel restore-controls)
      (define reset-control-panel reset-controls)
      (define mark->sound       mark-home)
      (define (mix-sound-index m) (car (mix-home m)))
      (define (mix-sound-channel m) (cadr (mix-home m)))
      (define amp               amp-control)
      (define contrast          contrast-control)
      (define contrast-amp      contrast-control-amp)
      (define contrast-func     #f)
      (define contrasting       contrast-control?)
      (define expand            expand-control)
      (define expand-hop        expand-control-hop)
      (define expand-length     expand-control-length)
      (define expand-ramp       expand-control-ramp)
      (define expanding         expand-control?)
      (define filtering         filter-control?)
      (define filter-order      filter-control-order)
      (define filter-env        filter-control-env)
      (define filter-dBing      filter-control-in-dB)
      (define reverb-decay      reverb-control-decay)
      (define reverb-feedback   reverb-control-feedback)
      (define reverb-funcs      #f)
      (define reverb-length     reverb-control-length)
      (define reverb-lowpass    reverb-control-lowpass)
      (define reverb-scale      reverb-control-scale)
      (define reverbing         reverb-control?)
      (define speed             speed-control)
      (define speed-as-float    speed-control-as-float)
      (define speed-as-ratio    speed-control-as-ratio)
      (define speed-as-semitone speed-control-as-semitone)
      (define speed-style       speed-control-style)
      (define speed-tones       speed-control-tones)
      (define filter-env-order  enved-filter-order)
      (define enved-dBing       enved-in-dB)
      (define enved-exping      enved-exp?)
      (define enved-waving      enved-wave?)
      (define enved-clipping    enved-clip?)
      (define amplitude-env     enved-amplitude)
      (define srate-env         enved-srate)
      (define spectrum-env      enved-spectrum)
      (define (hide-listener) (set! (show-listener) #f))
      (define activate-listener show-listener)
      (define dac-folding       dac-combines-channels)
      (define focus-left        zoom-focus-left)
      (define focus-right       zoom-focus-right)
      (define focus-middle      zoom-focus-middle)
      (define focus-active      zoom-focus-active)
      (define x-to-one          x-axis-as-percentage)
      (define x-in-seconds      x-axis-in-seconds)
      (define x-in-samples      x-axis-in-samples)

      (define graphing          graph-lisp?)
      ;; old transform-size is shadowed by new version = transform-samples-size
      (define waving            graph-time?)
      (define ffting            graph-transform?)
      (define fft-graph         transform-graph)
      (define fft-beta          fft-window-beta)
      (define fft-hook          transform-hook)
      (define normalize-by-channel normalize-transform-by-channel)
      (define normalize-by-sound   normalize-transform-by-sound)
      (define normalize-globally   normalize-transform-globally)
      (define normalize-transform  transform-normalization)
      (define dont-normalize    dont-normalize-transform)
      (define max-fft-peaks     max-transform-peaks)
      (define show-fft-peaks    show-transform-peaks)
      (define before-fft-hook   before-transform-hook)
      (define fft-style         transform-graph-type)
      (define normal-fft        graph-transform-once)
      (define sonogram          graph-transform-as-sonogram)
      (define spectrogram       graph-transform-as-spectrogram)
      (define update-fft        update-transform)
      (define update-graph      update-time-graph)
      (define fft-size          transform-size)
      (define wavo              time-graph-type) ;not quite right since wavo used booleans
      (define yes-or-no-p       yes-or-no?)
      (define corruption-time   auto-update-interval)

      (define uniting 
	(make-procedure-with-setter 
	 (lambda arg 
	   (apply channel-style arg)) 
	 (lambda args 
	   (if (= (length args) 1)
	       (set! (channel-style) (car args))
	       (set! (channel-style (car args)) (cadr args))))))

      ))


;;; Snd-4 map/scan/temp functions

(define* (scan-sound-chans proc #:optional (beg 0) end snd edpos)
  "(scan-sound-chans proc #:optional (beg 0) end snd edpos) applies scan-chan with proc to each channel in a sound"
  (let ((result #f))
    (do ((i 0 (1+ i)))
	((or (= i (chans snd))
	     result)
	 result)
      (let ((val (scan-chan proc beg end snd i edpos)))
	(if val
	    (set! result (append val (list (or snd (selected-sound)) i))))))))

(define* (map-sound-chans proc #:optional (beg 0) end edname snd edpos)
  "(map-sound-chans proc #:optional (beg 0) end edname snd edpos) applies map-chan with proc to each channel in a sound"
  (do ((i 0 (1+ i)))
      ((= i (chans snd)))
    (map-chan proc beg end edname snd i edpos)))


(define* (scan-all-chans proc #:optional (beg 0) end edpos)
  "(scan-all-chans proc #:optional (beg 0) end snd edpos) applies scan-chan with proc to all channels (all sounds)"
  (catch 'done
	 (lambda ()
	   (apply for-each 
		  (lambda (snd chn)
		    (let ((result (scan-chan proc beg end snd chn edpos)))
		      (if result (throw 'done (append result (list snd chn))))))
		  (all-chans)))
	 (lambda args (cadr args))))

(define* (map-all-chans proc #:optional (beg 0) end edname edpos)
  "(map-all-chans proc #:optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels (all sounds)"
  (apply for-each 
	 (lambda (snd chn)
	   (map-chan proc beg end edname snd chn edpos))
	 (all-chans)))


(define* (scan-chans proc #:optional (beg 0) end edpos)
  "(scan-chans proc #:optional (beg 0) end snd edpos) applies scan-chan with proc to all channels sharing current sound's sync"
  (let ((current-sync (sync (selected-sound))))
    (define (check-one-chan proc beg end snd chn edpos)
      (let ((val (scan-chan proc beg end snd chn edpos)))
	(if val
	    (append val (list snd chn))
	    #f)))
    (call-with-current-continuation
     (lambda (return)
       (for-each 
	(lambda (snd)
	  (if (= (sync snd) current-sync)
	      (do ((i 0 (1+ i)))
		  ((= i (chans snd)))
		(let ((val (check-one-chan proc beg end snd i edpos)))
		  (if val
		      (return val))))))
	(sounds))
       #f))))

(define* (map-chans proc #:optional (beg 0) end edname edpos)
  "(map-chans proc #:optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels sharing current sound's sync"
  (let ((current-sync (sync (selected-sound))))
    (for-each 
     (lambda (snd)
       (if (= (sync snd) current-sync)
	   (do ((i 0 (1+ i)))
	       ((= i (chans snd)))
	     (map-chan proc beg end edname snd i edpos))))
     (sounds))))


(define* (map-across-all-chans proc #:optional (beg 0) end edname snd edpos)
  "(map-across-all-chans proc #:optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels in parallel"
  (let* ((chans (all-chans))
	 (chan-num (length (car chans)))
	 (maxlen (apply max (apply map frames chans)))
	 (len (if (number? end)
		  (- (min end maxlen) beg)
		  (- maxlen beg)))
	 (data (make-vector chan-num))
	 (fds (make-vector chan-num))
	 (filenames (make-vector chan-num))
	 (outsamp 0)
	 (outgs (make-vector chan-num)))
    (do ((j 0 (1+ j))
	 (s (car chans) (cdr s))
	 (c (cadr chans) (cdr c)))
	((= j chan-num))
      (vector-set! fds j (make-sample-reader beg (car s) (car c) 1 edpos))
      (vector-set! filenames j (snd-tempnam))
      (vector-set! outgs j (make-sample->file (vector-ref filenames j) 1 mus-out-format mus-next)))
    (do ((i 0 (1+ i)))
	((= i len))
      (do ((j 0 (1+ j)))
	  ((= j chan-num))
	(vector-set! data j (next-sample (vector-ref fds j))))
      (let ((newdata (proc data chan-num)))
	(if newdata
	    (begin
	      (do ((j 0 (1+ j)))
		  ((= j chan-num))
		(out-any outsamp (vector-ref newdata j) 0 (vector-ref outgs j)))
	      (set! outsamp (1+ outsamp))))))
    (do ((j 0 (1+ j))
	 (s (car chans) (cdr s))
	 (c (cadr chans) (cdr c)))
	((= j chan-num))
      (mus-close (vector-ref outgs j))
      (free-sample-reader (vector-ref fds j))
      (if (> outsamp 0)
	  (begin
	    (if (not (= outsamp len))
		(delete-samples beg len (car s) (car c)))
	    (set! (samples beg outsamp (car s) (car c) #t edname) (vector-ref filenames j))
	    (delete-file (vector-ref filenames j)))))))

(define* (scan-across-all-chans proc #:optional (beg 0) end snd edpos)
  "(scan-across-all-chans proc #:optional (beg 0) end edname snd edpos) applies scan-chan with proc to all channels in parallel"
  (let* ((chans (all-chans))
	 (chan-num (length (car chans)))
	 (maxlen (apply max (apply map frames chans)))
	 (len (if (number? end)
		  (- (min end maxlen) beg)
		  (- maxlen beg)))
	 (data (make-vector chan-num))
	 (fds (make-vector chan-num)))
    (do ((j 0 (1+ j))
	 (s (car chans) (cdr s))
	 (c (cadr chans) (cdr c)))
	((= j chan-num))
      (vector-set! fds j (make-sample-reader beg (car s) (car c) 1 edpos)))
    (catch 'done
	   (lambda ()
	     (do ((i 0 (1+ i)))
		 ((= i len))
	       (do ((j 0 (1+ j)))
		   ((= j chan-num))
		 (vector-set! data j (next-sample (vector-ref fds j))))
	       (let ((newdata (proc data chan-num)))
		 (if newdata
		     (throw 'done (list newdata (+ i beg)))))))
	   (lambda args (cadr args)))))

(define* (map-across-sound-chans proc #:optional (beg 0) end edname snd edpos)
  "(map-across-sound-chans proc #:optional (beg 0) end edname snd edpos) applies map-chan with proc to all channels or sound in parallel"
  (let* ((chan-num (chans snd))
	 (len (- (min end (frames snd 0)) beg))
	 (data (make-vector chan-num))
	 (fds (make-vector chan-num))
	 (filenames (make-vector chan-num))
	 (outsamp 0)
	 (outgs (make-vector chan-num)))
    (do ((j 0 (1+ j)))
	((= j chan-num))
      (vector-set! fds j (make-sample-reader beg snd j 1 edpos))
      (vector-set! filenames j (snd-tempnam))
      (vector-set! outgs j (make-sample->file (vector-ref filenames j) 1 mus-out-format mus-next)))
    (do ((i 0 (1+ i)))
	((= i len))
      (do ((j 0 (1+ j)))
	  ((= j chan-num))
	(vector-set! data j (next-sample (vector-ref fds j))))
      (let ((newdata (proc data chan-num)))
	(if newdata
	    (begin
	      (do ((j 0 (1+ j)))
		  ((= j chan-num))
		(out-any outsamp (vector-ref newdata j) 0 (vector-ref outgs j)))
	      (set! outsamp (1+ outsamp))))))
    (do ((j 0 (1+ j)))
	((= j chan-num))
      (mus-close (vector-ref outgs j))
      (free-sample-reader (vector-ref fds j))
      (if (> outsamp 0)
	  (begin
	    (if (not (= outsamp len))
		(delete-samples beg len snd j))
	    (set! (samples beg outsamp snd j #t edname) (vector-ref filenames j))
	    (delete-file (vector-ref filenames j)))))))


;;; Snd-4 external program support stuff

(define* (selection-to-temp #:optional (type mus-next) (format mus-out-format))
  "(selection-to-temp #:optional (type mus-next) (format mus-out-format)) writes selection data as (multichannel) file (for external program)"
  (let ((data (make-vector 1)))
    (vector-set! data 0 (snd-tempnam))
    (save-selection (vector-ref data 0) type format)
    data))

(define (syncd-sounds val)
  (let ((ctr 0))
    (for-each 
     (lambda (n)
       (if (= (sync n) val)
	   (set! ctr (+ ctr 1))))
     (sounds))
    ctr))

(define* (sound-to-temp #:optional (type mus-next) (format mus-out-format) edpos)
  "(sound-to-temp #:optional (type mus-next) (format mus-out-format) edpos) writes sound data as (multichannel) file (for external program)"
  (let* ((cursnd (selected-sound))
	 (cursync (sync cursnd)))
    (if (or (= cursync 0)
	    (= (syncd-sounds cursync) 1))
	(let ((data (make-vector 1)))
	  (vector-set! data 0 (snd-tempnam))
	  (save-sound-as (vector-ref data 0) (selected-sound) type format #f #f edpos)
	  data)
	(snd-error "re-implemented sound-to-temp doesn't handle sync bit correctly yet."))))

(define* (selection-to-temps #:optional (type mus-next) (format mus-out-format))
  "(selection-to-temps #:optional (type mus-next) (format mus-out-format)) writes selection data as mono files (for external program)"
  (let* ((chns (selection-chans))
	 (data (make-vector chns)))
    (do ((i 0 (1+ i))) 
	((= i chns)) 
      (vector-set! data i (snd-tempnam))
      (save-selection (vector-ref data i) type format #f #f i))
    data))

  
(define* (sound-to-temps #:optional (type mus-next) (format mus-out-format) edpos)
  "(sound-to-temps #:optional (type mus-next) (format mus-out-format) edpos) writes sound data as mono files (for external program)"
  (let* ((cursnd (selected-sound))
	 (cursync (sync cursnd)))
    (if (or (= cursync 0)
	    (= (syncd-sounds cursync) 1))
	(let* ((chns (chans cursnd))
	       (data (make-vector chns)))
	  (do ((i 0 (1+ i)))
	      ((= i chns))
	    (vector-set! data i (snd-tempnam))
	    (save-sound-as (vector-ref data i) (selected-sound) type format #f i edpos))
	  data)
	(snd-error "re-implemented sound-to-temps doesn't handle sync bit correctly yet."))))

(define (temp-filenames data) data)

(define* (temp-to-sound data filename #:optional origin)
  "(temp-to-sound data filename #:optional origin) reads (multichannel) file as new sound data (from external program)"
  (let ((cursnd (selected-sound)))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file filename)))
	    (delete-file temp-file))))
    (do ((i 0 (1+ i)))
	((= i (chans cursnd)))
      (set! (samples 0 (mus-sound-frames filename)  cursnd i #t origin i) filename))))

(define* (temps-to-sound data filenames #:optional origin)
  "(temps-to-sound data filenames #:optional origin) reads mono files as new sound data (from external program)"
  (let ((cursnd (selected-sound)))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file (vector-ref filenames i))))
	    (delete-file temp-file))))
    (do ((i 0 (1+ i)))
	((= i (chans cursnd)))
      (set! (samples 0 (mus-sound-frames (vector-ref filenames i)) cursnd i #t origin) (vector-ref filenames i)))))

(define* (temp-to-selection data filename #:optional origin)
  "(temp-to-selection data filename #:optional origin) sets selection from (multichannel) file (from external program)"
  (let ((chan 0)
	(len (mus-sound-frames filename)))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file filename)))
	    (delete-file temp-file))))
    (for-each
     (lambda (n)
       (do ((i 0 (1+ i)))
	   ((= i (chans n)))
	 (if (selection-member? n i)
	     (begin
	       (set! (samples (selection-position n i) len n i #t origin chan) filename)
	       (set! chan (+ chan 1))))))
     (sounds))))

(define* (temps-to-selection data filenames #:optional origin)
  "(temps-to-selection data filenames #:optional origin) sets selection from mono files (from external program)"
  (let ((chan 0))
    (do ((i 0 (1+ i)))
	((= i (vector-length data)))
      (let ((temp-file (vector-ref data i)))
	(if (and (file-exists? temp-file)
		 (not (string=? temp-file (vector-ref filenames i))))
	    (delete-file temp-file))))
    (for-each
     (lambda (n)
       (do ((i 0 (1+ i)))
	   ((= i (chans n)))
	 (if (selection-member? n i)
	     (let ((len (mus-sound-frames (vector-ref filenames chan))))
	       (set! (samples (selection-position n i) len n i #t origin) (vector-ref filenames chan))
	       (set! chan (+ chan 1))))))
     (sounds))))
