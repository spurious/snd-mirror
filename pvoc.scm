;;; a version of the Moore-Klingbeil-Trevisani-Edwards phase-vocoder
;;;
;;; here we split it into three pieces:
;;;   oscil-bank-interp (a generator) that does the resynthesis
;;;   make-phase-vocoder
;;;   phase-vocoder (callable like a generator)
;;;
;;; the vocoder input comes from a function called as-needed (as in src, etc)
;;;   but it could easily read a previous analysis instead (just replace the analysis block with a data reader)


;;; guile junk....
(use-modules (ice-9 optargs))
(read-set! keywords 'prefix) ;this so we can use ":" as the keyword prefix
(define (keyword->symbol kw)
  (let ((sym (symbol->string (keyword-dash-symbol kw))))
    (string->symbol (substring sym 1 (string-length sym)))))
;;; this is needed to get around a bug in optargs
;;; end guile junk


(define make-oscil-bank 
  (lambda (len)
    (let ((oscils (make-vector len)))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(vector-set! oscils i (make-oscil :frequency 0.0)))
      oscils)))

(define make-oscil-bank-interp
  (lambda (len)
    (list (make-oscil-bank len)
	  (make-vct len)
	  (make-vct len))))

(define oscil-bank-interp 
  (lambda (ampincs bank freqincs)
    (let* ((oscils (car bank))
	   (amps (cadr bank))
	   (freqs (caddr bank)))
      (vct-add! amps ampincs)
      (vct-add! freqs freqincs)
      (oscil-bank amps oscils freqs))))

(define mus-amps 
  (lambda (bank)
    (cadr bank)))

(define mus-freqs
  (lambda (bank)
    (caddr bank)))


(define ifloor (lambda (n) (inexact->exact (floor n))))
(define pi 3.141592653589793)
(define pi2 (* 2 pi))

;;; make-phase-vocoder as in examp.scm
(define make-phase-vocoder
  (lambda* (#&key
	   (fftsize 512) (overlap 4) (time 1.0)
	   (pitch 1.0) (gate 0.0) (hoffset 0.0))
    (let ((N2 (ifloor (/ fftsize 2)))
	  (interp (* (ifloor (/ fftsize overlap)) time)))
      (list 
       interp                        ;output
       interp                        ;interp
       0                             ;filptr
       fftsize                       ;N
       (srate)                       ;sr
       (let ((window (make-fft-window hamming-window fftsize)))
	 (vct-scale! window (/ 2.0 (* 0.54 fftsize))) ;den = hamming window integrated
	 window)                    ; window
       (ifloor (/ fftsize overlap)) ;D
       #f                           ;in-data (created in phase-vocoder gen)
       (if (procedure? hoffset)     ;poffset
	   hoffset 
	   (lambda () (hz->radians hoffset)))  
       (if (= 0.0 gate) 0.0 (expt 10 (/ (- (abs gate)) 20))) ;syngate
       (make-vct N2)                ;ampinc
       (make-vct N2)                ;freqinc
       (make-oscil-bank-interp N2)  ;bank
       (make-vct N2)                ;lastphase
       (if (procedure? pitch) pitch (lambda () pitch))
       ))))

;;; phase-vocoder list accessors
(define pv-output (lambda (pv) (list-ref pv 0)))
(define set-pv-output (lambda (pv val) (list-set! pv 0 val)))
(define pv-interp (lambda (pv) (list-ref pv 1)))
(define pv-filptr (lambda (pv) (list-ref pv 2)))
(define set-pv-filptr (lambda (pv val) (list-set! pv 2 val)))
(define pv-N (lambda (pv) (list-ref pv 3)))
(define pv-sr (lambda (pv) (list-ref pv 4)))
(define pv-window (lambda (pv) (list-ref pv 5)))
(define pv-D (lambda (pv) (list-ref pv 6)))
(define pv-in-data (lambda (pv) (list-ref pv 7)))
(define set-pv-in-data (lambda (pv val) (list-set! pv 7 val)))
(define pv-poffset (lambda (pv) (list-ref pv 8)))
(define pv-syngate (lambda (pv) (list-ref pv 9)))
(define pv-ampinc (lambda (pv) (list-ref pv 10)))
(define pv-freqinc (lambda (pv) (list-ref pv 11)))
(define pv-bank (lambda (pv) (list-ref pv 12)))
(define pv-lastphase (lambda (pv) (list-ref pv 13)))
(define pv-pitch (lambda (pv) (list-ref pv 14)))

;;; phase-vocoder generator
(define phase-vocoder
  (lambda (pv input)
    (if (>= (pv-output pv) (pv-interp pv))
	(let* ((N (pv-N pv))
	       (D (pv-D pv))
	       (fdr (make-vct N))
	       (fdi (make-vct N))
	       (filptr (pv-filptr pv)))
	  (set-pv-output pv 0)
	  (if (not (pv-in-data pv))
	      (begin
		(set-pv-in-data pv (make-vct N))
		(do ((i 0 (1+ i)))
		    ((= i N))
		  (vct-set! (pv-in-data pv) i (input))))
	      (let ((indat (pv-in-data pv)))
		;; extra loop here since I find the optimized case confusing (we could dispense with the data move)
		(do ((i D (1+ i))
		     (j 0 (1+ j)))
		    ((= i N))
		  (vct-set! indat j (vct-ref indat i)))
		(do ((i (- N D) (1+ i))
		     (j 0 (1+ j)))
		    ((= i N))
		  (vct-set! indat i (input)))))
	  (do ((k 0 (1+ k))
	       (buf (modulo filptr N)))
	      ((= k N))
	    (vct-set! fdr buf
		      (* (vct-ref (pv-window pv) k) 
			 (vct-ref (pv-in-data pv) k)))
	    (set! buf (1+ buf))
	    (if (= buf N) (set! buf 0)))
	  (set-pv-filptr pv (+ filptr D))
	  (mus-fft fdr fdi N 1)
	  (do ((k 0 (1+ k)))
	      ((= k (ifloor (/ N 2))))
	    (let* ((a (vct-ref fdr k))
		   (b (vct-ref fdi k))
		   (mag (* (sqrt (+ (* a a) (* b b)))))
		   (phase 0)
		   (phasediff 0))
	      (if (< mag (pv-syngate pv)) 
		  (vct-set! fdr k 0.0)
		  (vct-set! fdr k mag))
	      (if (> mag 0)
		  (begin
		    (set! phase (- (atan b a)))
		    (set! phasediff (- phase (vct-ref (pv-lastphase pv) k)))
		    (vct-set! (pv-lastphase pv) k phase)
		    (if (> phasediff pi) (do () ((<= phasediff pi)) (set! phasediff (- phasediff pi2))))
		    (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (set! phasediff (+ phasediff pi2))))))
	      (vct-set! fdi k 
			(* ((pv-pitch pv))
			   (+ (/ (* phasediff (pv-sr pv)) (* D (pv-sr pv)))
			      (* k 2 (/ pi N))
			      ((pv-poffset pv)))))
	      (vct-set! (pv-ampinc pv) k (/ (- (vct-ref fdr k) (vct-ref (mus-amps (pv-bank pv)) k)) (pv-interp pv)))
	      (vct-set! (pv-freqinc pv) k (/ (- (vct-ref fdi k) (vct-ref (mus-freqs (pv-bank pv)) k)) (pv-interp pv)))))))
    (set-pv-output pv (1+ (pv-output pv)))
    (oscil-bank-interp (pv-ampinc pv) (pv-bank pv) (pv-freqinc pv))
    ))


(define test-pv 
  (lambda (freq)
    (let ((pv (make-phase-vocoder :pitch freq))
	  (reader (make-sample-reader 0)))
      (map-chan (lambda (val)
	          (if val
		      (phase-vocoder pv (lambda () 
				          (next-sample reader)))
		      #f))))))

