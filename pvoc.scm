;;; versions of the Moore-Klingbeil-Trevisani-Edwards phase-vocoder

(provide 'snd-pvoc.scm)

(define* (make-pvocoder fftsize overlap interp analyze edit synthesize)
  "(make-pvocoder fftsize overlap interp analyze edit synthesize) makes a new (Scheme-based, not CLM) phase-vocoder generator"

  (let* ((N (or fftsize 512))
	 (N2 (floor (/ N 2)))
	 (hop (or overlap 4))
	 (D (floor (/ N hop))))

    ;; basic: fftsize overlap
    ;;  everything else via closures (interp in particular)
    ;;  pv: counter ("output" here)
    ;;      interp
    ;;      fftsize ("N"), hop ("D")
    ;;      in-counter ("filptr")
    ;;      hamming window scaled
    ;;      slot for in-coming data ("in-data") (created on first call)
    ;;      vcts: ampinc amp freqinc phaseinc phase lastphase
    ;;      funcs: analysize, edit, resynthesize

    (list 
     interp                        ;output
     interp                        ;interp
     0                             ;filptr
     N                             ;N
     (let ((window (make-fft-window hamming-window fftsize)))
       (vct-scale! window (/ 2.0 (* 0.54 fftsize))) ;den = hamming window integrated
       window)                     ; window
     D                             ;D
     #f                            ;in-data (created in pvocoder gen)
     (make-vct fftsize)            ;ampinc
     (make-vct fftsize)            ;freqs
     (make-vct N2)                 ;amps
     (make-vct N2)                 ;phaseinc
     (make-vct N2)                 ;phases
     (make-vct N2)                 ;lastphaseinc
     analyze
     edit
     synthesize)))

;;; pvocoder generator: 
;;     input data func
;;     analysis func with fallback
;;     editing func with fallback 
;;     resynthesis func with fallback


(define* (sine-bank amps phases size)
  (let ((len (or size (length amps)))
	(sum 0.0))
    (do ((i 0 (+ i 1)))
	((= i len))
      (set! sum (+ sum (* (amps i)
			  (sin (phases i))))))
    sum))


(define (pvocoder pv input)
  "(pvocoder pv input) is the phase-vocoder generator associated with make-pvocoder"

  ;; pvocoder list accessors
  (define (pvoc-output pv) (pv 0))
  (define (set-pvoc-output pv val) (set! (pv 0) val))
  (define (pvoc-interp pv) (pv 1))
  (define (set-pvoc-interp pv val) (set! (pv 1) val))
  (define (pvoc-filptr pv) (pv 2))
  (define (set-pvoc-filptr pv val) (set! (pv 2) val))
  (define (pvoc-N pv) (pv 3))
  (define (pvoc-window pv) (pv 4))
  (define (pvoc-D pv) (pv 5))
  (define (pvoc-in-data pv) (pv 6))
  (define (set-pvoc-in-data pv val) (set! (pv 6) val))
  (define (pvoc-ampinc pv) (pv 7))
  (define (pvoc-freqs pv) (pv 8))
  (define (pvoc-amps pv) (pv 9))
  (define (pvoc-phaseinc pv) (pv 10))
  (define (pvoc-phases pv) (pv 11))
  (define (pvoc-lastphase pv) (pv 12))
  (define (pvoc-analyze pv) (pv 13))
  (define (pvoc-edit pv) (pv 14))
  (define (pvoc-synthesize pv) (pv 15))

  (let ((pi2 (* 2.0 pi)))

    (if (>= (pvoc-output pv) (pvoc-interp pv))
	;; get next block of amp/phase info
	(let ((N (pvoc-N pv))
	      (D (pvoc-D pv))
	      (amps (pvoc-ampinc pv))
	      (freqs (pvoc-freqs pv))
	      (filptr (pvoc-filptr pv)))

	  (if (pvoc-analyze pv)
	      ((pvoc-analyze pv) pv input)
	      ;; if no analysis func:
	      (begin
		(fill! freqs 0.0)
		(set-pvoc-output pv 0)
		(if (not (pvoc-in-data pv))
		    (begin
		      (set-pvoc-in-data pv (make-vct N))
		      (do ((i 0 (+ i 1)))
			  ((= i N))
			(set! ((pvoc-in-data pv) i) (input))))
		    (let ((indat (pvoc-in-data pv)))
		      ;; extra loop here since I find the optimized case confusing (we could dispense with the data move)
		      (vct-move! indat 0 D)
		      (do ((i (- N D) (+ i 1)))
			  ((= i N))
			(set! (indat i) (input)))))
		(let ((buf (modulo filptr N)))
		  (if (= buf 0)
		      (begin
			(fill! amps 0.0)
			(vct-add! amps (pvoc-in-data pv))
			(vct-multiply! amps (pvoc-window pv)))
		      (begin
			(do ((k 0 (+ k 1)))
			    ((= k N))
			  (set! (amps buf) (* ((pvoc-window pv) k) ((pvoc-in-data pv) k)))
			  (set! buf (+ 1 buf))
			  (if (= buf N) (set! buf 0))))))
		(set-pvoc-filptr pv (+ filptr D))
		(mus-fft amps freqs N 1)
		(rectangular->polar amps freqs)))

	  (if (pvoc-edit pv)
	      ((pvoc-edit pv) pv)
	      (let ((lp (pvoc-lastphase pv))
		    (pscl (/ 1.0 D))
		    (kscl (/ pi2 N))
		    (lim (floor (/ N 2))))
		;; if no editing func:
		(do ((k 0 (+ k 1)))
		    ((= k lim))
		  (let ((phasediff (remainder (- (freqs k) (lp k)) pi2)))
		    (vct-set! lp k (freqs k))
		    (if (> phasediff pi) (set! phasediff (- phasediff pi2))
			(if (< phasediff (- pi)) (set! phasediff (+ phasediff pi2))))
		    (set! (freqs k) (+ (* pscl phasediff) (* k kscl)))))))

	  (let ((scl (/ 1.0 (pvoc-interp pv))))
	    (vct-subtract! amps (pvoc-amps pv))
	    (vct-subtract! freqs (pvoc-phaseinc pv))
	    (vct-scale! amps scl)
	    (vct-scale! freqs scl)
	    )))

    (set-pvoc-output pv (+ 1 (pvoc-output pv)))

    (if (pvoc-synthesize pv)
	((pvoc-synthesize pv) pv)
        ;; if no synthesis func:
	;; synthesize next sample
	(begin
	  (vct-add! (pvoc-amps pv) (pvoc-ampinc pv))
	  (vct-add! (pvoc-phaseinc pv) (pvoc-freqs pv))
	  (vct-add! (pvoc-phases pv) (pvoc-phaseinc pv))
	  (sine-bank (pvoc-amps pv) (pvoc-phases pv))))
    ))

#|
(let* ((ind (open-sound "oboe.snd"))
       (pv (make-pvocoder 256 4 64))
       (rd (make-sampler 0)))
  (map-channel (lambda (y) (pvocoder pv (lambda () (rd))))))
|#

#|
;;; ---------------- same thing using phase-vocoder gen

(define test-pv-1
  (lambda (freq)
    (let ((pv (make-phase-vocoder #f
				  512 4 128 1.0
				  #f ;no change to analysis
				  #f ;no change to edits
				  #f ;no change to synthesis
				  ))
	  (reader (make-sampler 0)))
      (map-channel (lambda (val)
		     (phase-vocoder pv (lambda (dir) 
					 (next-sample reader)))))
      (free-sampler reader))))

(define test-pv-2
  (lambda (freq)
    (let ((pv (make-phase-vocoder #f
				  512 4 128 freq
				  #f ;no change to analysis
				  #f
				  #f ; no change to synthesis
				  ))
	  (reader (make-sampler 0)))
      (map-channel (lambda (val)
		     (phase-vocoder pv (lambda (dir) 
					 (next-sample reader)))))
      (free-sampler reader))))

(define test-pv-3
  (lambda (time)
    (let* ((pv (make-phase-vocoder #f
				   512 4 (floor (* 128 time)) 1.0
				   #f ;no change to analysis
				   #f ;no change to edits
				   #f ;no change to synthesis
				   ))
	   (reader (make-sampler 0))
	   (len (floor (* time (frames))))
	   (data (make-vct len))
	   )
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(set! (data i) (phase-vocoder pv (lambda (dir) (next-sample reader)))))
      (free-sampler reader)
      (vct->channel data 0 len))))

(define test-pv-4
  (lambda (gate)
    (let ((pv (make-phase-vocoder #f
				  512 4 128 1.0
				  #f ;no change to analysis
				  (lambda (v)
				    (let ((N (mus-length v)))
				      (do ((i 0 (+ i 1)))
					  ((= i N))
					(if (< ((phase-vocoder-amp-increments v) i) gate)
					    (vct-set! (phase-vocoder-amp-increments v) i 0.0)))
				      #t))
				  #f ;no change to synthesis
				  ))
	  (reader (make-sampler 0))
	  )
      (map-channel (lambda (val)
		     (phase-vocoder pv (lambda (dir) 
					 (next-sample reader)))))
      (free-sampler reader))))
|#


;;; -------- another version of the phase vocoder --------

(define pvoc
  (lambda* ((fftsize 512) (overlap 4) (time 1.0)
	    (pitch 1.0) (gate 0.0) (hoffset 0.0)
	    (snd 0) (chn 0))
    "(pvoc fftsize overlap time pitch gate hoffset snd chn) applies the phase vocoder
  algorithm to the current sound (i.e. fft analysis, oscil bank resynthesis). 'pitch'
  specifies the pitch transposition ratio, 'time' - specifies the time dilation ratio,
  'gate' specifies a resynthesis gate in dB (partials with amplitudes lower than
  the gate value will not be synthesized), 'hoffset is a pitch offset in Hz."

    (let* ((len (frames))
	   (filptr 0)           ; index into the file
	   (pi2 (* 2 pi))       ; handy constant
	   (sr (srate))
	   (N fftsize)          ; fft size
	   (N2 (floor (/ N 2)))
	   ;; (Nw fftsize) ;; window size -- currently restricted to the fftsize
	   (D (floor (/ fftsize overlap))) ; decimation factor (how often do we take an fft)
	   (interp (* (floor (/ fftsize overlap)) time)) ; interpolation factor how often do we synthesize
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
	   ;; (nextpct 10.0)       ; how often to print out the percentage complete message
	   (outlen (floor (* time len)))
	   (out-data (make-vct (max len outlen)))
	   (in-data (channel->vct 0 (* N 2) snd chn))
	   (in-data-beg 0)
	   (obank (make-oscil-bank lastfreq (make-vct N2 0.0) lastamp)))

      (set! window (vct->vector (vct-scale! window (/ 2.0 (* 0.54 fftsize))))) ;den = hamming window integrated

      (do ((i 0 (+ i 1)))
	  ((>= i outlen))
	(if (>= output interp) ;; if all the samples have been output then do the next frame
	    (let ((buffix (modulo filptr N)))
					; buffix is the index into the input buffer
					; it wraps around circularly as time increases in the input
	      (set! output 0)       ; reset the output sample counter
	      ;; save the old amplitudes and frequencies
	      (fill! lastamp 0.0)
	      (fill! lastfreq 0.0)
	      (vct-add! lastamp fdr)
	      (vct-add! lastfreq fdi)
	      (do ((k 0 (+ k 1)))
		  ((= k N))
		;; apply the window and then stuff into the input array
		(set! (fdr buffix) (* (window k) (in-data (- filptr in-data-beg))))
		(set! filptr (+ 1 filptr))
		;; increment the buffer index with wrap around
		(set! buffix (+ 1 buffix))
		(if (>= buffix N) (set! buffix 0)))
	      ;; rewind the file for the next hop
	      (set! filptr (- filptr (- N D)))
	      (if (> filptr (+ in-data-beg N))
		  (begin
		    (set! in-data-beg filptr)
		    (set! in-data (channel->vct in-data-beg (* N 2) snd chn))))
	      ;; no imaginary component input so zero out fdi
	      (fill! fdi 0.0)
	      ;; compute the fft
	      (mus-fft fdr fdi N 1)
	      ;; now convert into magnitude and interpolated frequency
	      (do ((k 0 (+ k 1)))
		  ((= k N2))
		(let* ((a (fdr k))
		       (b (fdi k))
		       (mag (sqrt (+ (* a a) (* b b))))
		       (phase 0)
		       (phasediff 0))
		  (set! (fdr k) mag)    ;; current amp stored in fdr
		  ;; mag is always positive
		  ;; if it is zero then the phase difference is zero
		  (if (> mag 0)
		      (begin
			(set! phase (- (atan b a)))
			(set! phasediff (- phase (lastphase k)))
			(set! (lastphase k) phase)
			;; frequency wrapping from Moore p. 254
			(if (> phasediff pi) (do () ((<= phasediff pi)) (set! phasediff (- phasediff pi2))))
			(if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (set! phasediff (+ phasediff pi2))))))
		  ;; current frequency stored in fdi
		  ;; scale by the pitch transposition
		  (set! (fdi k) 
			(* pitch (+ (/ (* phasediff sr) (* D sr))
				    (* k fundamental)
				    poffset)))
		  ;; resynthesis gating
		  (if (< (fdr k) syngate) (set! (fdr k) 0.0))
		  ;; take (lastamp k) and count up to (fdr k)
		  ;; interpolating by ampinc
		  (set! (ampinc k) (/ (- (fdr k) (lastamp k)) interp))
		  ;; take (lastfreq k) and count up to (fdi k)
		  ;; interpolating by freqinc
		  (set! (freqinc k) (/ (- (fdi k) (lastfreq k)) interp))))))
	;; loop over the partials interpolate frequency and amplitude
	(vct-add! lastamp ampinc)
	(vct-add! lastfreq freqinc)
	(set! (out-data i) (oscil-bank obank))
	(set! output (+ 1 output)))
      (vct->channel out-data 0 (max len outlen)))))


