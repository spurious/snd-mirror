;;; a version of the Moore-Klingbeil-Trevisani-Edwards phase-vocoder
;;;

(use-modules (ice-9 optargs))
(read-set! keywords 'prefix)

(define ifloor (lambda (n) (inexact->exact (floor n))))
(define pi 3.141592653589793)
(define pi2 (* 2 pi))

(define make-phase-vocoder
  (lambda (fftsize overlap interp analyze edit synthesize)
    (let* ((N (or fftsize 512))
	   (N2 (ifloor (/ N 2)))
	   (hop (or overlap 4))
	   (D (ifloor (/ N hop))))

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
       D                             ;output
       D                             ;interp
       0                             ;filptr
       N                             ;N
       (let ((window (make-fft-window hamming-window fftsize)))
	 (vct-scale! window (/ 2.0 (* 0.54 fftsize))) ;den = hamming window integrated
	 window)                     ; window
       D                             ;D
       #f                            ;in-data (created in phase-vocoder gen)
       (make-vct fftsize)            ;ampinc
       (make-vct fftsize)            ;freqs
       (make-vct N2)                 ;amps
       (make-vct N2)                 ;phaseinc
       (make-vct N2)                 ;phases
       (make-vct N2)                 ;lastphaseinc
       analyze
       edit
       synthesize
       ))))

;;; phase-vocoder list accessors
(define pv-output (lambda (pv) (list-ref pv 0)))
(define set-pv-output (lambda (pv val) (list-set! pv 0 val)))
(define pv-interp (lambda (pv) (list-ref pv 1)))
(define set-pv-interp (lambda (pv val) (list-set! pv 1 val)))
(define pv-filptr (lambda (pv) (list-ref pv 2)))
(define set-pv-filptr (lambda (pv val) (list-set! pv 2 val)))
(define pv-N (lambda (pv) (list-ref pv 3)))
(define pv-window (lambda (pv) (list-ref pv 4)))
(define pv-D (lambda (pv) (list-ref pv 5)))
(define pv-in-data (lambda (pv) (list-ref pv 6)))
(define set-pv-in-data (lambda (pv val) (list-set! pv 6 val)))
(define pv-ampinc (lambda (pv) (list-ref pv 7)))
(define pv-freqs (lambda (pv) (list-ref pv 8)))
(define pv-amps (lambda (pv) (list-ref pv 9)))
(define pv-phaseinc (lambda (pv) (list-ref pv 10)))
(define pv-phases (lambda (pv) (list-ref pv 11)))
(define pv-lastphase (lambda (pv) (list-ref pv 12)))
(define pv-analyze (lambda (pv) (list-ref pv 13)))
(define pv-edit (lambda (pv) (list-ref pv 14)))
(define pv-synthesize (lambda (pv) (list-ref pv 15)))


;;; phase-vocoder generator: 
;;     input data func
;;     analysis func with fallback
;;     editing func with fallback 
;;     resynthesis func with fallback

(define phase-vocoder
  (lambda (pv input)

    (if (>= (pv-output pv) (pv-interp pv))
	;; get next block of amp/phase info
	(let* ((N (pv-N pv))
	       (D (pv-D pv))
	       (amps (pv-ampinc pv))
	       (freqs (pv-freqs pv))
	       (filptr (pv-filptr pv)))

	  (if (pv-analyze pv)
	      ((pv-analyze pv) pv)
	      ;; if no analysis func, do:
	      (begin
		(vct-fill! freqs 0.0)
		(set-pv-output pv 0)
		(if (not (pv-in-data pv))
		    (begin
		      (set-pv-in-data pv (make-vct N))
		      (vct-map! (pv-in-data pv) input))
		    (let ((indat (pv-in-data pv)))
		      ;; extra loop here since I find the optimized case confusing (we could dispense with the data move)
		      (vct-move! indat 0 D)
		      (do ((i (- N D) (1+ i)))
			  ((= i N))
			(vct-set! indat i (input)))))
		(let ((buf (modulo filptr N)))
		  (if (= buf 0)
		      (begin
			(vct-fill! amps 0.0)
			(vct-add! amps (pv-in-data pv))
			(vct-multiply! amps (pv-window pv)))
		      (begin
			(do ((k 0 (1+ k)))
			    ((= k N))
			  (vct-set! amps buf (* (vct-ref (pv-window pv) k) (vct-ref (pv-in-data pv) k)))
			  (set! buf (1+ buf))
			  (if (= buf N) (set! buf 0))))))
		(set-pv-filptr pv (+ filptr D))
		(mus-fft amps freqs N 1)
		(rectangular->polar amps freqs)))

	  (if (pv-edit pv)
	      ((pv-edit pv) pv)
	      (begin
		;; if no editing func:
		(do ((k 0 (1+ k))
		     (pscl (/ 1.0 D))
		     (kscl (/ pi2 N)))
		    ((= k (ifloor (/ N 2))))
		  (let ((phasediff (- (vct-ref freqs k) (vct-ref (pv-lastphase pv) k))))
		    (vct-set! (pv-lastphase pv) k (vct-ref freqs k))
		    (if (> phasediff pi) (do () ((<= phasediff pi)) (set! phasediff (- phasediff pi2))))
		    (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (set! phasediff (+ phasediff pi2))))
		    (vct-set! freqs k (+ (* pscl phasediff) (* k kscl)))))))

	  (let ((scl (/ 1.0 (pv-interp pv))))
	    (vct-subtract! amps (pv-amps pv))
	    (vct-subtract! freqs (pv-phaseinc pv))
	    (vct-scale! amps scl)
	    (vct-scale! freqs scl)
	    )))

    (set-pv-output pv (1+ (pv-output pv)))

    (if (pv-synthesize pv)
	((pv-synthesize pv) pv)
        ;; if no synthesis func:
	;; synthesize next sample
	(begin
	  (vct-add! (pv-amps pv) (pv-ampinc pv))
	  (vct-add! (pv-phaseinc pv) (pv-freqs pv))
	  (vct-add! (pv-phases pv) (pv-phaseinc pv))
	  (sum-of-sines (pv-amps pv) (pv-phases pv))))
    ))


(define test-pv 
  (lambda (freq)
    (let ((pv (make-phase-vocoder 512 4 128 
				  #f ;no change to analysis
				  (lambda (v)
				    ; new editing func changes pitch
				    (let* ((N (pv-N v))
					   (D (pv-D v))
					   (amps (pv-ampinc v))
					   (freqs (pv-freqs v)))
				      (do ((k 0 (1+ k))
					   (pscl (/ 1.0 D))
					   (kscl (/ pi2 N)))
					  ((= k (ifloor (/ N 2))))
					(let ((phasediff (- (vct-ref freqs k) (vct-ref (pv-lastphase v) k))))
					  (vct-set! (pv-lastphase v) k (vct-ref freqs k))
					  (if (> phasediff pi) (do () ((<= phasediff pi)) (set! phasediff (- phasediff pi2))))
					  (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (set! phasediff (+ phasediff pi2))))
					  (vct-set! freqs k 
						    (* freq
						       (+ (* pscl phasediff)
							  (* k kscl))))))))
				  #f ; no change to synthesis
				  ))
	  (reader (make-sample-reader 0))
	  )
      (map-chan (lambda (val)
	          (if val
		      (phase-vocoder pv (lambda () 
				          (next-sample reader)))
		      #f))))))

