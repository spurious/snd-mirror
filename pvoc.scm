;;; a version of the Moore-Klingbeil-Trevisani-Edwards pvocoder
;;;

(use-modules (ice-9 optargs))
(read-set! keywords 'prefix)

(define ifloor (lambda (n) (inexact->exact (floor n))))
(define pi 3.141592653589793)
(define pi2 (* 2 pi))

(define make-pvocoder
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
       #f                            ;in-data (created in pvocoder gen)
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

;;; pvocoder list accessors
(define pvoc-output (lambda (pv) (list-ref pv 0)))
(define set-pvoc-output (lambda (pv val) (list-set! pv 0 val)))
(define pvoc-interp (lambda (pv) (list-ref pv 1)))
(define set-pvoc-interp (lambda (pv val) (list-set! pv 1 val)))
(define pvoc-filptr (lambda (pv) (list-ref pv 2)))
(define set-pvoc-filptr (lambda (pv val) (list-set! pv 2 val)))
(define pvoc-N (lambda (pv) (list-ref pv 3)))
(define pvoc-window (lambda (pv) (list-ref pv 4)))
(define pvoc-D (lambda (pv) (list-ref pv 5)))
(define pvoc-in-data (lambda (pv) (list-ref pv 6)))
(define set-pvoc-in-data (lambda (pv val) (list-set! pv 6 val)))
(define pvoc-ampinc (lambda (pv) (list-ref pv 7)))
(define pvoc-freqs (lambda (pv) (list-ref pv 8)))
(define pvoc-amps (lambda (pv) (list-ref pv 9)))
(define pvoc-phaseinc (lambda (pv) (list-ref pv 10)))
(define pvoc-phases (lambda (pv) (list-ref pv 11)))
(define pvoc-lastphase (lambda (pv) (list-ref pv 12)))
(define pvoc-analyze (lambda (pv) (list-ref pv 13)))
(define pvoc-edit (lambda (pv) (list-ref pv 14)))
(define pvoc-synthesize (lambda (pv) (list-ref pv 15)))


;;; pvocoder generator: 
;;     input data func
;;     analysis func with fallback
;;     editing func with fallback 
;;     resynthesis func with fallback

(define pvocoder
  (lambda (pv input)

    (if (>= (pvoc-output pv) (pvoc-interp pv))
	;; get next block of amp/phase info
	(let* ((N (pvoc-N pv))
	       (D (pvoc-D pv))
	       (amps (pvoc-ampinc pv))
	       (freqs (pvoc-freqs pv))
	       (filptr (pvoc-filptr pv)))

	  (if (pvoc-analyze pv)
	      ((pvoc-analyze pv) pv input)
	      ;; if no analysis func, do:
	      (begin
		(vct-fill! freqs 0.0)
		(set-pvoc-output pv 0)
		(if (not (pvoc-in-data pv))
		    (begin
		      (set-pvoc-in-data pv (make-vct N))
		      (vct-map! (pvoc-in-data pv) input))
		    (let ((indat (pvoc-in-data pv)))
		      ;; extra loop here since I find the optimized case confusing (we could dispense with the data move)
		      (vct-move! indat 0 D)
		      (do ((i (- N D) (1+ i)))
			  ((= i N))
			(vct-set! indat i (input)))))
		(let ((buf (modulo filptr N)))
		  (if (= buf 0)
		      (begin
			(vct-fill! amps 0.0)
			(vct-add! amps (pvoc-in-data pv))
			(vct-multiply! amps (pvoc-window pv)))
		      (begin
			(do ((k 0 (1+ k)))
			    ((= k N))
			  (vct-set! amps buf (* (vct-ref (pvoc-window pv) k) (vct-ref (pvoc-in-data pv) k)))
			  (set! buf (1+ buf))
			  (if (= buf N) (set! buf 0))))))
		(set-pvoc-filptr pv (+ filptr D))
		(mus-fft amps freqs N 1)
		(rectangular->polar amps freqs)))

	  (if (pvoc-edit pv)
	      ((pvoc-edit pv) pv)
	      (begin
		;; if no editing func:
		(do ((k 0 (1+ k))
		     (pscl (/ 1.0 D))
		     (kscl (/ pi2 N)))
		    ((= k (ifloor (/ N 2))))
		  (let ((phasediff (- (vct-ref freqs k) (vct-ref (pvoc-lastphase pv) k))))
		    (vct-set! (pvoc-lastphase pv) k (vct-ref freqs k))
		    (if (> phasediff pi) (do () ((<= phasediff pi)) (set! phasediff (- phasediff pi2))))
		    (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (set! phasediff (+ phasediff pi2))))
		    (vct-set! freqs k (+ (* pscl phasediff) (* k kscl)))))))

	  (let ((scl (/ 1.0 (pvoc-interp pv))))
	    (vct-subtract! amps (pvoc-amps pv))
	    (vct-subtract! freqs (pvoc-phaseinc pv))
	    (vct-scale! amps scl)
	    (vct-scale! freqs scl)
	    )))

    (set-pvoc-output pv (1+ (pvoc-output pv)))

    (if (pvoc-synthesize pv)
	((pvoc-synthesize pv) pv)
        ;; if no synthesis func:
	;; synthesize next sample
	(begin
	  (vct-add! (pvoc-amps pv) (pvoc-ampinc pv))
	  (vct-add! (pvoc-phaseinc pv) (pvoc-freqs pv))
	  (vct-add! (pvoc-phases pv) (pvoc-phaseinc pv))
	  (sum-of-sines (pvoc-amps pv) (pvoc-phases pv))))
    ))


(define test-pvoc-1
  (lambda (freq)
    (let ((pv (make-pvocoder 512 4 128 
				  #f ;no change to analysis
				  #f ;no change to edits
				  #f ;no change to synthesis
				  ))
	  (reader (make-sample-reader 0))
	  )
      (map-chan (lambda (val)
	          (if val
		      (pvocoder pv (lambda () 
				          (next-sample reader)))
		      #f))))))

(define test-pvoc-2
  (lambda (freq)
    (let ((pv (make-pvocoder 512 4 128 
				  #f ;no change to analysis
				  (lambda (v)
				    ; new editing func changes pitch
				    (let* ((N (pvoc-N v))
					   (D (pvoc-D v))
					   (amps (pvoc-ampinc v))
					   (freqs (pvoc-freqs v)))
				      (do ((k 0 (1+ k))
					   (pscl (/ 1.0 D))
					   (kscl (/ pi2 N)))
					  ((= k (ifloor (/ N 2))))
					(let ((phasediff (- (vct-ref freqs k) (vct-ref (pvoc-lastphase v) k))))
					  (vct-set! (pvoc-lastphase v) k (vct-ref freqs k))
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
		      (pvocoder pv (lambda () 
				          (next-sample reader)))
		      #f))))))

