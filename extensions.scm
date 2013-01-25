;;; various generally useful Snd extensions

;;; mix then scale result to original peak amp
;;; mix with envelope
;;; map-sound-files, for-each-sound-file, match-sound-files, directory->list
;;; mix-channel, insert-channel
;;; redo-channel, undo-channel
;;; sine-ramp, sine-env-channel, blackman4-ramp, blackman4-env-channel
;;; ramp-squared, env-squared-channel
;;; ramp-expt, env-expt-channel
;;; offset-channel
;;; channels-equal
;;; mono->stereo, mono-files->stereo, stereo->mono


(provide 'snd-extensions.scm)

(define (remove-if pred l)
  "(remove-if func lst) removes any element from 'lst' that 'func' likes"
  (map (lambda (x) (if (pred x) (values) x)) l))


(if (not (defined? 'all-chans))
    (define (all-chans)
      "(all-chans) -> two parallel lists, the first sound objects, the second channel numbers.  If we have
two sounds open (indices 0 and 1 for example), and the second has two channels, (all-chans) returns '((#<sound 0> #<sound 1> #<sound 1>) (0 0 1))"
      (let ((sndlist ())
	    (chnlist ()))
	(for-each (lambda (snd)
		    (do ((i (- (channels snd) 1) (- i 1)))
			((< i 0))
		      (set! sndlist (cons snd sndlist))
		      (set! chnlist (cons i chnlist))))
		  (sounds))
	(list sndlist chnlist))))


(define channel-sync
  (make-procedure-with-setter
   (lambda (snd chn) 
     "(channel-sync snd chn) returns the sync property of that channel (it is not actually used anywhere)"
     (channel-property 'sync snd chn))
   (lambda (snd chn val) 
     (set! (channel-property 'sync snd chn) val))))



;;; -------- mix with result at original peak amp

(define (normalized-mix filename beg in-chan snd chn)
  "(normalized-mix filename beg in-chan snd chn) is like mix but the mix result has same peak amp as unmixed snd/chn (returns scaler)"
  (let ((original-maxamp (maxamp snd chn)))
    (mix filename beg in-chan snd chn)
    (let ((new-maxamp (maxamp snd chn)))
      (if (not (= original-maxamp new-maxamp))
	  (let ((scaler (/ original-maxamp new-maxamp))
		(old-sync (sync snd)))
	    (set! (sync snd) 0)
	    (scale-by scaler snd chn)
	    (set! (sync snd) old-sync)
	    scaler)
	  1.0))))


;;;-------- mix with envelope on mixed-in file
;;;
;;; there are lots of ways to do this; this version uses functions from Snd, CLM, and Sndlib.

(define (enveloped-mix filename beg e)
  "(enveloped-mix filename beg e) mixes filename starting at beg with amplitude envelope e. (enveloped-mix \"pistol.snd\" 0 '(0 0 1 1 2 0))"
  (let* ((len (frames filename))
	 (tmp-name (string-append (if (and (string? (temp-dir))
					   (> (string-length (temp-dir)) 0))
				      (string-append (temp-dir) "/")
				      "")
				  "tmp.snd"))
	 (tmpfil (mus-sound-open-output tmp-name 22050 1 mus-bshort mus-next ""))
	 (mx (make-mixer 1 1.0))
	 (envs (make-vector 1))
	 (inenvs (make-vector 1)))
    (mus-sound-close-output tmpfil 0)
    (set! (inenvs 0) (make-env e :length len))
    (set! (envs 0) inenvs)
    (mus-mix tmp-name filename 0 len 0 mx envs)
    (mix tmp-name beg)
    (delete-file tmp-name)))


;;; -------- map-sound-files, match-sound-files
;;;
;;; apply a function to each sound in dir
;;;
;;;   (map-sound-files (lambda (n) (if (> (mus-sound-duration n) 10.0) (snd-print n))))

(define* (map-sound-files func dir)
  "(map-sound-files func dir) applies func to each sound file in dir"
  (map func (sound-files-in-directory (or dir "."))))


(define* (for-each-sound-file func dir)
  "(for-each-sound-file func dir) applies func to each sound file in dir"
  (for-each func (sound-files-in-directory (or dir "."))))

#|
 (for-each-sound-file
  (lambda (n) 
    (catch #t
           (lambda ()
 	      (if (not (null? (mus-sound-loop-info (string-append "/home/bil/sf/" n)))) 
 		  (snd-print (format #f "~%~A" n))))
           (lambda args #f)))
  "/home/bil/sf")
|#


(define* (match-sound-files func dir)
  "(match-sound-files func dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
  (let ((matches ()))
    (for-each
     (lambda (file)
       (if (func file)
	   (set! matches (cons file matches))))
     (sound-files-in-directory (or dir ".")))
    matches))
  

    
;;; -------- mix-channel, insert-channel, c-channel

(define* (mix-channel input-data (beg 0) dur snd (chn 0) edpos with-tag)

  "(mix-channel file beg dur snd chn edpos with-tag) mixes in file. file can be the file name, a sound object, or \
a list (file-name-or-sound-object [beg [channel]])."

  (define (channel->mix input-snd input-chn input-beg input-len output-snd output-chn output-beg)
    (if (< input-len 1000000)
	(mix-vct (channel->vct input-beg input-len input-snd input-chn) output-beg output-snd output-chn #t)
	(let* ((output-name (snd-tempnam))
	       (output (new-sound output-name :size input-len))
	       (reader (make-sampler input-beg input-snd input-chn))
	       (data (make-vct input-len)))
	  (do ((i 0 (+ i 1)))
	      ((= i input-len))
	    (vct-set! data i (next-sample reader)))
	  (vct->channel data 0 input-len output 0)
	  (save-sound output)
	  (close-sound output)
	  (mix output-name output-beg 0 output-snd output-chn #t #t))))

  (let* ((input (if (not (list? input-data)) 
		    input-data 
		    (car input-data)))
	 (input-beg (if (or (not (list? input-data))
			   (< (length input-data) 2)) 
		       0 
		       (cadr input-data)))
	 (input-channel (if (or (not (list? input-data))
			       (< (length input-data) 3))
			   0 
			   (caddr input-data)))
	 (len (or dur (- (if (string? input)
			     (frames input) 
			     (frames input input-channel))
			 input-beg)))
	 (start (or beg 0)))
    (if (< start 0) 
	(error 'no-such-sample (list "mix-channel" beg))
	(if (> len 0)
	    (if (not with-tag)

		;; not a virtual mix
		(let ((reader (make-sampler input-beg input input-channel))
		      (read2 (make-sampler start snd chn 1 edpos))
		      (data (make-vct len)))
		  (do ((i 0 (+ i 1)))
		      ((= i len))
		    (vct-set! data i (+ (next-sample reader) (next-sample read2))))
		  (vct->channel data start len snd chn current-edit-position
				(if (string? input-data)
				    (format #f "mix-channel ~S ~A ~A" input-data beg dur)
				    (format #f "mix-channel '~A ~A ~A" input-data beg dur))))

		;; a virtual mix -- use simplest method available
		(if (sound? input)

		    ;; sound object case
		    (channel->mix input input-channel input-beg len snd chn start)

		    ;; file input
		    (if (and (= start 0) 
			     (= len (frames input)))

			;; mixing entire file
			(mix input start 0 snd chn #t #f) ; don't delete it!

			;; mixing part of file
			(let* ((output-name (snd-tempnam))
			       (output (new-sound output-name :size len))
			       (reader (make-sampler input-beg input input-channel))
			       (data (make-vct len)))
			  (do ((i 0 (+ i 1)))
			      ((= i len))
			    (vct-set! data i (next-sample reader)))
			  (vct->channel data 0 len output 0)
			  (save-sound output)
			  (close-sound output)
			  (mix output-name start 0 snd chn #t #t)))))))))


(define* (insert-channel file-data beg dur snd chn edpos)
  "(insert-channel file beg dur snd chn edpos) inserts the file. file can be the file name or a list (file-name [beg [channel]])"
  (let* ((file-name (if (string? file-data) file-data (car file-data)))
	 (file-beg (if (or (string? file-data) 
			   (< (length file-data) 2)) 
		       0 
		       (cadr file-data)))
	 (file-channel (if (or (string? file-data) 
			       (< (length file-data) 3))
			   0 
			   (caddr file-data)))
	 (len (or dur (- (frames file-name) file-beg)))
	 (start (or beg 0)))
    (if (< start 0) (error 'no-such-sample (list "insert-channel" beg)))
    (if (> len 0)
	(let ((reader (make-sampler file-beg file-name file-channel))
	      (data (make-vct len)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (set! (data i) (next-sample reader)))
	  (insert-samples start len data snd chn edpos #f 
			  (if (string? file-data)
			      (format #f "insert-channel ~S ~A ~A" file-data beg dur)
			      (format #f "insert-channel '~A ~A ~A" file-data beg dur)))))))


;;; -------- redo-channel, undo-channel

(define* (redo-channel (edits 1) snd chn)
  "(redo-channel (edits 1) snd chn) is the regularized version of redo"
  (if (and snd (not (= (sync snd) 0)) chn)
      (set! (edit-position snd chn) (+ (edit-position snd chn) edits))
      (redo edits snd)))


(define* (undo-channel (edits 1) snd chn)
  "(undo-channel (edits 1) snd chn) is the regularized version of undo"
  (if (and snd (not (= (sync snd) 0)) chn)
      (set! (edit-position snd chn) (max 0 (- (edit-position snd chn) edits)))
      (undo edits snd)))


;;; -------- any-env-channel

(define* (any-env-channel e func (beg 0) dur snd chn edpos origin)
  "(any-env-channel e func (beg 0) dur snd chn edpos origin) takes breakpoints in 'e', \
connects them with 'func', and applies the result as an amplitude envelope to the given channel"
  ;; handled as a sequence of funcs and scales
  (if (not (null? e))
      (let ((pts (/ (length e) 2)))
	(if (= pts 1)
	    (scale-channel (car e) beg dur snd chn edpos)
	    (let ((x0 0)
		  (y0 0)
		  (x1 (car e))
		  (y1 (cadr e))
		  (xrange (- (e (- (length e) 2)) (car e)))
		  (ramp-beg beg)
		  (ramp-dur 0))
	      (if (not (number? dur)) (set! dur (frames snd chn)))
	      (as-one-edit 
	       (lambda ()
		 (do ((i 1 (+ 1 i))
		      (j 2 (+ j 2)))
		     ((= i pts))
		   (set! x0 x1)
		   (set! y0 y1)
		   (set! x1 (e j))
		   (set! y1 (e (+ 1 j)))
		   (set! ramp-dur (round (* dur (/ (- x1 x0) xrange))))
		   (if (= y0 y1)
		       (scale-channel y0 ramp-beg ramp-dur snd chn edpos)
		       (func y0 y1 ramp-beg ramp-dur snd chn edpos))
		   (set! ramp-beg (+ ramp-beg ramp-dur))))
	       origin))))))

;;; -------- sine-ramp sine-env-channel 

(define* (sine-ramp rmp0 rmp1 (beg 0) dur snd chn edpos)
  "(sine-ramp rmp0 rmp1 (beg 0) dur snd chn edpos) produces a sinsusoidal connection from rmp0 to rmp1"
  (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	 (incr (/ pi len))
	 (data (make-vct len))
	 (reader (make-sampler beg snd chn 1 edpos)))
    (do ((i 0 (+ i 1))
	 (angle (- pi) (+ angle incr)))
	((= i len))
      (vct-set! data i (* (next-sample reader) 
			  (+ rmp0 (* (- rmp1 rmp0) 
				     (+ 0.5 (* 0.5 (cos angle))))))))
     (vct->channel data beg len snd chn current-edit-position
		   (format #f "sine-ramp ~A ~A ~A ~A" rmp0 rmp1 beg dur))))


(define* (sine-env-channel e (beg 0) dur snd chn edpos)
  "(sine-env-channel e (beg 0) dur snd chn edpos) connects e's dots with sinusoids"
  (any-env-channel e sine-ramp beg dur snd chn edpos (format #f "sine-env-channel '~A ~A ~A" e beg dur)))

;;; (sine-env-channel '(0 0 1 1 2 -.5 3 1))

;;; an obvious extension of this idea is to use the blackman fft window formulas
;;;   to get sharper sinusoids (i.e. use the sum of n cosines, rather than just 1)


;;; -------- blackman4-ramp, blackman4-env-channel

(define* (blackman4-ramp rmp0 rmp1 (beg 0) dur snd chn edpos)
  "(blackman4-ramp rmp0 rmp1 (beg 0) dur snd chn edpos) produces a blackman4-shaped envelope"
  ;; vct: angle incr off scl
  (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	 (incr (/ pi len))
	 (data (make-vct len))
	 (reader (make-sampler beg snd chn 1 edpos)))

    (do ((i 0 (+ i 1))
	 (angle 0.0 (+ angle incr)))
	((= i len))
      (let ((cx (cos angle)))
	(vct-set! data i (* (next-sample reader) 
			    (+ rmp0
			       (* (- rmp1 rmp0)
				  (+ .084037 (* cx (+ -.29145 (* cx (+ .375696 (* cx (+ -.20762 (* cx .041194))))))))))))))

     (vct->channel data beg len snd chn current-edit-position
		   (format #f "blackman4-ramp ~A ~A ~A ~A" rmp0 rmp1 beg dur))))


(define* (blackman4-env-channel e (beg 0) dur snd chn edpos)
  "(blackman4-env-channel e (beg 0) dur snd chn edpos) uses the blackman4 window to connect the dots in 'e'"
  (any-env-channel e blackman4-ramp beg dur snd chn edpos (format #f "blackman4-env-channel '~A ~A ~A" e beg dur)))



;;; -------- ramp-squared, env-squared-channel

(define* (ramp-squared rmp0 rmp1 (symmetric #t) (beg 0) dur snd chn edpos)
  "(ramp-squared rmp0 rmp1 (symmetric #t) (beg 0) dur snd chn edpos) connects rmp0 and rmp1 with an x^2 curve"
  ;; vct: start incr off scl
  (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	 (incr (/ 1.0 len))
	 (data (make-vct len))
	 (reader (make-sampler beg snd chn 1 edpos)))

    (if (and symmetric
	     (< rmp1 rmp0))

	 (do ((i 0 (+ i 1))
	      (angle 1.0 (- angle incr)))
	     ((= i len))
	   (vct-set! data i (* (next-sample reader) 
			       (+ rmp1 (* angle angle (- rmp0 rmp1))))))
	 (do ((i 0 (+ i 1))
	      (angle 0.0 (+ angle incr)))
	     ((= i len))
	   (vct-set! data i (* (next-sample reader) 
			       (+ rmp0 (* angle angle (- rmp1 rmp0)))))))

     (vct->channel data beg len snd chn current-edit-position
		   (format #f "ramp-squared ~A ~A ~A ~A ~A" rmp0 rmp1 symmetric beg dur))))


(define* (env-squared-channel e (symmetric #t) (beg 0) dur snd chn edpos)
  "(env-squared-channel e (symmetric #t) (beg 0) dur snd chn edpos) connects e's dots with x^2 curves"
  (any-env-channel e 
		   (lambda (r0 r1 b d s c e)
		     (ramp-squared r0 r1 symmetric b d s c e))
		   beg dur snd chn edpos
		   (format #f "env-squared-channel '~A ~A ~A ~A" e symmetric beg dur)))

;;; (env-squared-channel '(0 0 1 1 2 -.5 3 1))


;;; -------- ramp-expt, env-expt-channel

(define* (ramp-expt rmp0 rmp1 exponent (symmetric #t) (beg 0) dur snd chn edpos)
  "(ramp-expt rmp0 rmp1 exponent (symmetric #t) (beg 0) dur snd chn edpos) connects rmp0 and rmp1 with an x^exponent curve"
  ;; vct: start incr off scl exponent
  ;; a^x = exp(x * log(a))
  (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	 (incr (/ 1.0 len))
	 (data (make-vct len))
	 (reader (make-sampler beg snd chn 1 edpos)))

     (if (and symmetric
	     (< rmp1 rmp0))
	 (do ((i 0 (+ i 1))
	      (angle 1.0 (- angle incr)))
	     ((= i len))
	   (vct-set! data i (* (next-sample reader) 
			       (+ rmp1 (* (expt angle exponent) (- rmp0 rmp1))))))
	 (do ((i 0 (+ i 1))
	      (angle 0.0 (+ angle incr)))
	     ((= i len))
	   (vct-set! data i (* (next-sample reader) 
			       (+ rmp0 (* (expt angle exponent) (- rmp1 rmp0)))))))

     (vct->channel data beg len snd chn current-edit-position
		   (format #f "ramp-expt ~A ~A ~A ~A ~A ~A" rmp0 rmp1 exponent symmetric beg dur))))


(define* (env-expt-channel e exponent (symmetric #t) (beg 0) dur snd chn edpos)
  "(env-expt-channel e exponent (symmetric #t) (beg 0) dur snd chn edpos) connects e's dots with x^exponent curves"
  (if (= exponent 1.0)
      (env-channel e beg dur snd chn edpos)
      (any-env-channel e 
		       (lambda (r0 r1 b d s c e)
			 (ramp-expt r0 r1 exponent symmetric b d s c e))
		       beg dur snd chn edpos
		       (format #f "env-expt-channel '~A ~A ~A ~A ~A" e exponent symmetric beg dur))))


;;; -------- offset-channel 

(define* (offset-channel dc (beg 0) dur snd chn edpos)
  "(offset-channel amount (beg 0) dur snd chn edpos) adds amount to each sample"
  (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	 (data (make-vct len))
	 (reader (make-sampler beg snd chn 1 edpos)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vct-set! data i (+ (next-sample reader) dc)))
    (vct->channel data beg len snd chn current-edit-position (format #f "offset-channel ~A ~A ~A" dc beg dur))))


(define* (offset-sound off (beg 0) dur snd)
  "(offset-sound off beg dur snd) adds 'off' to every sample in 'snd'"
  ;; the pretty but slow way:
  ;; (map-sound (lambda (fr) (frame+ fr off)) beg dur snd)
  ;;
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (offset-channel off beg dur index chn)))
	(error 'no-such-sound (list "offset-sound" snd)))))


;;; -------- pad-sound

(define* (pad-sound beg dur snd) 
  "(pad-sound beg dur snd) places a block of 'dur' zeros in every channel of 'snd' starting at 'beg'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (pad-channel beg dur index chn)))
	(error 'no-such-sound (list "pad-sound" snd)))))


;;; -------- dither-channel

(define* (dither-channel (amount .00006) (beg 0) dur snd chn edpos)
  "(dither-channel (amount .00006) (beg 0) dur snd chn edpos) adds amount dither to each sample"
  (let ((dither (* .5 amount)))
    (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	   (data (make-vct len))
	   (reader (make-sampler beg snd chn 1 edpos)))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(vct-set! data i (+ (next-sample reader) (mus-random dither) (mus-random dither))))
      (vct->channel data beg len snd chn current-edit-position
		    (format #f "dither-channel ~,8F ~A ~A" amount beg dur)))))


(define* (dither-sound (amount .00006) (beg 0) dur snd)
  "(dither-sound (amount .00006) beg dur snd) adds dithering to every channel of 'snd'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (dither-channel amount beg dur index chn)))
	(error 'no-such-sound (list "dither-sound" snd)))))


;;; -------- contrast-channel

(define* (contrast-channel index (beg 0) dur snd chn edpos)
  "(contrast-channel index (beg 0) dur snd chn edpos) applies contrast enhancement to the sound"
  (let* ((len (if (number? dur) dur (- (frames snd chn) beg)))
	 (data (make-vct len))
	 (reader (make-sampler beg snd chn 1 edpos)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (let ((y (next-sample reader)))
	(vct-set! data i (sin (+ (* y 0.5 pi) (* index (sin (* y 2.0 pi))))))))
    (vct->channel data beg len snd chn current-edit-position
		  (format #f "contrast-channel ~A ~A ~A" index beg dur))))


(define* (contrast-sound index (beg 0) dur snd)
  "(contrast-sound index beg dur snd) applies contrast-enhancement to every channel of 'snd'"
  (let ((ind (or snd (selected-sound) (car (sounds)))))
    (if (sound? ind)
	(let ((out-chans (channels ind)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (contrast-channel index beg dur ind chn)))
	(error 'no-such-sound (list "contrast-sound" snd)))))


;;; -------- scale-sound

(define* (scale-sound scl (beg 0) dur snd)
  "(scale-sound scl beg dur snd) multiplies every sample in 'snd' by 'scl'"
  ;; the slow way:
  ;; (map-sound (lambda (fr) (frame* fr scl)) beg dur snd))
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let ((out-chans (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (scale-channel scl beg dur index chn)))
	(error 'no-such-sound (list "scale-sound" snd)))))


;;; -------- normalize-sound

(define* (normalize-sound amp (beg 0) dur snd)
  "(normalize-sound amp beg dur snd) scales 'snd' to peak amplitude 'amp'"
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (sound? index)
	(let ((out-chans (channels index))
	      (mx (apply max (maxamp index #t))))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn out-chans))
	    (scale-channel (/ amp mx) beg dur index chn)))
	(error 'no-such-sound (list "normalize-sound" snd)))))



;;; -------- channels-equal

(define* (channels=? snd1 chn1 snd2 chn2 (allowable-difference 0.0))
  "(channels=? s1 c1 s2 c2 (diff 0.0)) -> #t if the two channels are the same (within diff) modulo trailing 0's"
  (or (and (equal? snd1 snd2)
	   (= chn1 chn2))
      (let ((mx1 (maxamp snd1 chn1))
	    (mx2 (maxamp snd1 chn1)))
	(and (<= (abs (- mx1 mx2)) allowable-difference)
	     (let* ((len1 (frames snd1 chn1))
		    (len2 (frames snd2 chn2))
		    (first-longer (>= len1 len2)))
	       (let ((len (if first-longer len1 len2))
		     (s1 (if first-longer snd1 snd2))
		     (s2 (if first-longer snd2 snd1))
		     (c1 (if first-longer chn1 chn2))
		     (c2 (if first-longer chn2 chn1)))
		 (let ((read2 (make-sampler 0 s2 c2))
		       (read1 (make-sampler 0 s1 c1)))
		   (do ((i 0 (+ i 1))
			(y (read-sample read1) (read-sample read1))
			(val (read-sample read2) (read-sample read2)))
		       ((or (= i len)
			    (> (abs (- val y)) allowable-difference))
			(= i len))))))))))


(define* (channels-equal? snd1 chn1 snd2 chn2 (allowable-difference 0.0))
  "(channels-equal? s1 c1 s2 c2 (diff 0.0)) -> #t if the two channels are the same (within diff)"
  (let ((len1 (frames snd1 chn1))
	(len2 (frames snd2 chn2)))
    (if (not (= len1 len2))
	#f
	(channels=? snd1 chn1 snd2 chn2 allowable-difference))))


;;; -------- mono->stereo, mono-files->stereo

(define (mono->stereo new-name snd1 chn1 snd2 chn2)
  "(mono->stereo new-name snd1 chn1 snd2 chn2) takes the two channels and combines them into a stereo sound 'new-name'"
  ;; (mono->stereo "test.snd" 0 0 1 0)
  (let ((old-ed1 (edit-position snd1 chn1))
	(old-ed2 (edit-position snd2 chn2))
	(ind (new-sound new-name :channels 2 :srate (srate snd1))))
    (swap-channels ind 0 snd1 chn1)
    (swap-channels ind 1 snd2 chn2)
    (set! (edit-position snd1 chn1) old-ed1)
    (set! (edit-position snd2 chn2) old-ed2)
    ind))


(define (mono-files->stereo new-name chan1-name chan2-name)
  "(mono-files->stereo new-name file1 file2) combines two mono files into the stereo file 'new-name'"
  ;; (mono-files->stereo "test.snd" "oboe.snd" "pistol.snd")
  (let* ((ind1 (open-sound chan1-name))
	 (ind2 (open-sound chan2-name))
	 (ind3 (mono->stereo new-name ind1 0 ind2 0)))
    (close-sound ind1)
    (close-sound ind2)
    ind3))


(define (stereo->mono orig-snd chan1-name chan2-name)
  "(stereo->mono stereo-sound new-chan1 new-chan2) splits a stereo sound into two mono sounds named 'new-chan1' and 'new-chan2'"
  ;; (stereo->mono 0 "hi1.snd" "hi2.snd")
  (let ((old-ed0 (edit-position orig-snd 0))
	(old-ed1 (edit-position orig-snd 1))
	(chan1 (new-sound chan1-name :srate (srate orig-snd)))	
	(chan2 (new-sound chan2-name :srate (srate orig-snd))))
    (swap-channels orig-snd 0 chan1 0)
    (swap-channels orig-snd 1 chan2 0)
    (set! (edit-position orig-snd 0) old-ed0)
    (set! (edit-position orig-snd 1) old-ed1)
    (list chan1 chan2)))

