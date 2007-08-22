;;; these are CLM test instruments

(provide 'snd-clm23.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))
(if (not (provided? 'snd-dsp.scm)) (load-from-path "dsp.scm"))

;;; definstrument -> define (+ change open paren placement)
;;; *srate* -> (mus-srate)
;;; run loop ... -> run (lambda () (do... + extra end close paren
;;; floor, [round, ceiling] wrapped in inexact->exact
;;; aref -> vct-ref
;;; setf -> set!
;;; remove declare (or change order of args and remove ":")
;;;   however in granulate run-time edit-func, the "(declare (g clm))" is necessary
;;; double not needed
;;; array of gen -> vector (and setf aref to vector-set! in this case)
;;; nil -> #f, t -> #t
;;; outa needs *output*
;;; incf, decf 
;;; length sometimes vector-length, vct-length etc
;;; make-filter in scm requires coeffs arrays
;;; &optional -> :optional, &key too (using define*)
;;; two-pi -> (* 2 pi)
;;; make-empty-frame is make-frame essentially
;;; open-input and close-input -> make-readin or use name directly (in make-readin)
;;; make-locsig needs :output *output* and similarly for *reverb* if it's active
;;; progn -> begin, when -> if+begin (prog1 prog2), dotimes
;;; string= -> string=? (also string-equal)
;;; integerp -> integer? and others like it (null -> null?)
;;; sound-duration -> mus-sound-duration and similarly for others
;;; various array info procs like array-dimension
;;; no length arg to sine-bank
;;; #'(lambda ...) to just (lambda...)
;;; nth -> list-ref
;;; loop -> do

(define* (make-double-array len :key initial-contents initial-element)
  "(make-double-array len :key initial-contents initial-element) is for CL/Scheme compatibility; it makes a vct"
  (let ((v (make-vct len (or initial-element 0.0))))
    (if initial-contents
	(let ((clen (min len (length initial-contents))))
	  (do ((i 0 (1+ i)))
	      ((= i clen))
	    (vct-set! v i (list-ref initial-contents i)))))
    v))

(define make-double-float-array make-double-array)
(define make-integer-array make-double-array) ; could use a vector here I suppose

(define (double a) 
  "(double a) is for compatibility with CL instruments; it returns its argument"
  a)	

(define open-input make-file->sample)

(define two-pi (* 2 pi))

(define (simple-out beg dur freq amp)
  "(simple-out beg dur freq amp) test instrument for outa"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (oscil os)) 0 *output*))))))

(definstrument (simple-fm beg dur freq amp mc-ratio index :optional amp-env index-env)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq))                     ; our carrier
         (md (make-oscil (* freq mc-ratio)))        ; our modulator
         (fm-index (hz->radians (* index mc-ratio freq)))
         (ampf (make-env (or amp-env '(0 0 .5 1 1 0)) :scaler amp :end end))
         (indf (make-env (or index-env '(0 0 .5 1 1 0)) :scaler fm-index :end end)))
    (run
      (lambda ()
        (do ((i start (1+ i)))
            ((= i end))
          (outa i (* (env ampf) (oscil cr (* (env indf) (oscil md)))) *output*))))))

(define (simple-outn beg dur freq ampa ampb ampc ampd reva revb)
  "(simple-outn beg dur freq ampa ampb ampc ampd reva revb) test instrument for outn"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (let ((val (oscil os)))
	   (if (> ampa 0.0) (outa i (* ampa val) *output*))
	   (if (> ampb 0.0) (outb i (* ampb val) *output*))
	   (if (> ampc 0.0) (outc i (* ampc val) *output*))
	   (if (> ampd 0.0) (outd i (* ampd val) *output*))
	   (if (> reva 0.0) (outa i (* reva val) *reverb*))
	   (if (> revb 0.0) (outb i (* revb val) *reverb*))))))))

(define (simple-ssb beg dur freq amp)
  "(simple-ssb beg dur freq amp) test instrument for ssb-am"
  (let* ((os (make-ssb-am freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 3)))
    (vector-set! arr 0 os)
    (vector-set! arr 1 #f)
    (vector-set! arr 2 (make-ssb-am 660 40))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (let* ((sum 0.0))
	   (do ((i 0 (1+ i)))
	       ((= i (vector-length arr)))
	     (if (ssb-am? (vector-ref arr i))
		 (set! sum (+ sum (ssb-am (vector-ref arr i) 1.0)))))
	   (out-any i (* amp sum) 0 *output*)))))))

(define (simple-multiarr beg dur freq amp)
  "(simple-multiarr beg dur freq amp) test instrument for array of gen"
  ;; this won't work in CL because that version of CLM assumes all aref gens are the same type
  (let* ((start (seconds->samples beg))
	 (len (seconds->samples dur))
	 (end (+ start len))
	 (arr (make-vector 3)))
    (vector-set! arr 0 (make-oscil freq))
    (vector-set! arr 1 (make-env '(0 0 1 1) :scaler amp :end len))
    (vector-set! arr 2 (make-oscil (* freq 2)))
    (run
     (lambda ()
       (do ((i start (1+ i))) 
	   ((= i end))
	 (out-any i (* (env (vector-ref arr 1))
		       (oscil (vector-ref arr 0)
			      (* .1 (oscil (vector-ref arr 2)))))
		  0 *output*))))))

(define (simple-sos beg dur amp)
  "(simple-sos beg dur amp) test instrument for sum-of-sines"
  (let* ((os (make-sum-of-sines 10 440 0.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (sum-of-sines os)) 0 *output*))))))

(define (simple-soc beg dur freq amp)
  "(simple-soc beg dur freq amp) test instrument for sum-of-cosines"
  (let* ((os (make-sum-of-cosines 10 freq 0.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (sum-of-cosines os)) 0 *output*))))))

(define (simple-osc beg dur freq amp)
  "(simple-osc beg dur freq amp) test instrument for oscil"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 20)))
    (do ((i 0 (1+ i)))
	((= i 20))
      (vector-set! arr i (make-oscil (* (1+ i) 100))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (let ((sum 0.0))
	   (do ((i 0 (1+ i)))
	       ((= i (vector-length arr)))
	     (if (oscil? (vector-ref arr i))
		 (set! sum (+ sum (oscil (vector-ref arr i))))))
	   (out-any i (* amp .05 sum) 0 *output*)))))))

(define (simple-sss beg dur amp)
  "(simple-sss beg dur amp) test instrument for sine-summation"
  (let* ((os (make-sine-summation 440 0.0 7 .5 1.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (sine-summation os)) 0 *output*))))))

(define (simple-asy beg dur amp)
  "(simple-asy beg dur amp) test instrument for asymmetric-fm"
  (let* ((os (make-asymmetric-fm 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (asymmetric-fm os 1.0)) 0 *output*))))))

(define (simple-saw beg dur amp)
  "(simple-saw beg dur amp) test instrument for sawtooth-wave"
  (let* ((os (make-sawtooth-wave 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (sawtooth-wave os)) 0 *output*))))))

(define (simple-sqr beg dur amp)
  "(simple-sqr beg dur amp) test instrument for square-wave"
  (let* ((os (make-square-wave 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (square-wave os)) 0 *output*))))))

(define (simple-tri beg dur amp)
  "(simple-tri beg dur amp) test instrument for triangle-wave"
  (let* ((os (make-triangle-wave 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (triangle-wave os)) 0 *output*))))))

(define (simple-pul beg dur amp)
  "(simple-pul beg dur amp) test instrument for pusle-train"
  (let* ((os (make-pulse-train 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (pulse-train os)) 0 *output*))))))

(define (simple-sib beg dur freq amp)
  "(simple-sib beg dur freq amp) test instrument for sine-bank"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (amps (make-double-array 3 :initial-element 0.0))
	 (phases (make-double-array 3 :initial-element 0.0))
	 (freqs (make-double-array 3 :initial-element 0.0)))
    (do ((i 0 (1+ i)))
	((= i 3))
      (set! (vct-ref freqs i) (double (* freq (1+ i))))
      (set! (vct-ref amps i) (double (/ amp (+ i 2)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (do ((i 0 (1+ i)))
	     ((= i (vct-length phases)))
	   (set! (vct-ref phases i) (+ (vct-ref phases i) (hz->radians (vct-ref freqs i)))))
	 (out-any i (sine-bank amps phases) 0 *output*))))))

(define (simple-oz beg dur freq amp)
  "(simple-oz beg dur freq amp) test instrument for one-zero"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-one-zero 0.4 0.6)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (one-zero oz (oscil os))) 0 *output*))))))

(define (simple-op beg dur freq amp)
  "(simple-op beg dur freq amp) test instrument for one-pole"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-one-pole 0.4 -0.6)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (one-pole oz (oscil os))) 0 *output*))))))

(define (simple-tz beg dur freq amp)
  "(simple-tz beg dur freq amp) test instrument for two-zero"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-two-zero 0.4 0.3 0.3)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (two-zero oz (oscil os))) 0 *output*))))))

(define (simple-tp beg dur freq amp)
  "(simple-tp beg dur freq amp) test instrument for two-pole"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-two-pole 0.3 -0.6 0.1)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (two-pole oz (oscil os))) 0 *output*))))))

(define (simple-frm beg dur freq amp)
  "(simple-frm beg dur freq amp) test instrument for formant"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-formant .95 1200.0 1.0)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (formant oz (oscil os))) 0 *output*))))))

(define (simple-wav beg dur freq amp)
  "(simple-wav beg dur freq amp) test instrument for waveshape"
  (let* ((w1 (make-waveshape :frequency freq :partials '(1 1 2 1 3 1)))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (waveshape w1 1.0)) 0 *output*))))))

;(define w1 (make-polyshape :frequency 100.0 
;			   :partials (let ((frqs '()))
;				       (do ((i 1 (1+ i)))
;					   ((= i 10) (begin (snd-display frqs) (reverse frqs)))
;					 (set! frqs (cons (/ 1.0 (* i i)) (cons i frqs)))))))

(define (simple-poly beg dur freq amp)
  "(simple-poly beg dur freq amp) test instrument for polyshape"
  (let* ((w1 (make-polyshape :frequency freq :partials '(1 1 2 1 3 1)))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (polyshape w1 1.0)) 0 *output*))))))

(define (simple-dly beg dur freq amp)
  "(simple-dly beg dur freq amp) test instrument for delay"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-delay 100)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (delay buf (* amp (oscil os))) 0 *output*))))))

(define (simple-cmb beg dur freq amp)
  "(simple-cmb beg dur freq amp) test instrument for comb"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-comb .1 100)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (comb buf (* amp (oscil os))) 0 *output*))))))

(define (simple-filtered-cmb beg dur freq amp)
  "(simple-filtered-cmb beg dur freq amp) test instrument for filtered-comb"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-filtered-comb .1 100 :filter (make-one-zero .5 .5))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (filtered-comb buf (* amp (oscil os))) 0 *output*))))))

(define (simple-not beg dur freq amp)
  "(simple-not beg dur freq amp) test instrument for notch"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-notch .1 100)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (notch buf (* amp (oscil os))) 0 *output*))))))

(define (simple-alp beg dur freq amp)
  "(simple-alp beg dur freq amp) test instrument for all-pass"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-all-pass .2 .8 100)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (all-pass buf (* amp (oscil os))) 0 *output*))))))

(define (simple-ave beg dur freq amp)
  "(simple-ave beg dur freq amp) test instrument for moving-average"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-moving-average 10)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (moving-average buf (* amp (oscil os))) 0 *output*))))))

(define (simple-tab beg dur freq amp)
  "(simple-tab beg dur freq amp) test instrument for table-lookup"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (table-size 256)
	 (buf (make-table-lookup freq 0.0 :size table-size))
	 (table (mus-data buf)))
    (do ((i 0 (1+ i)))
	((= i table-size))
      (set! (vct-ref table i) (double (/ i table-size))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (table-lookup buf)) 0 *output*))))))

(define (simple-flt beg dur freq amp)
  "(simple-flt beg dur freq amp) test instrument for filter"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (1+ i)))
	((= i 8))
      (set! (vct-ref (mus-xcoeffs flt) i) (double (/ i 16)))
      (set! (vct-ref (mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (filter flt (oscil os))) 0 *output*))))))

(define (simple-fir beg dur freq amp)
  "(simple-fir beg dur freq amp) test instrument for fir-filter"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-fir-filter 8 :xcoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (1+ i)))
	((= i 8))
      (set! (vct-ref (mus-xcoeffs flt) i) (double (/ i 16))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (fir-filter flt (oscil os))) 0 *output*))))))

(define (simple-iir beg dur freq amp)
  "(simple-iir beg dur freq amp) test instrument for iir-filter"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-iir-filter 8 :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (1+ i)))
	((= i 8))
      (set! (vct-ref (mus-ycoeffs flt) i) (double (/ i 16))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (iir-filter flt (oscil os))) 0 *output*))))))

(define (simple-f beg dur freq amp)
  "(simple-f beg dur freq amp) test instrument for frame->sample"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (frm (make-frame 2 .7 .3))
	 (smp (make-frame 2))
	 (os (make-oscil freq)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (let ((val (oscil os)))
	   (frame-set! smp 0 val)
	   (set! (frame-ref smp 1) val)
	   (out-any i (* amp (frame->sample frm smp)) 0 *output*)))))))

(define (simple-ran beg dur freq amp)
  "(simple-ran beg dur freq amp) test instrument for rand"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand freq)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (rand os)) 0 *output*))))))

(define (simple-ri beg dur freq amp)
  "(simple-ri beg dur freq amp) test instrument for rand-interp"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand-interp freq)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (rand-interp os)) 0 *output*))))))

(define (simple-rndist beg dur freq amp)
  "(simple-rndist beg dur freq amp) test instrument for rand dist"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand freq :distribution (inverse-integrate '(0 0 1 1)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (rand os)) 0 *output*))))))

(define (simple-ridist beg dur freq amp)
  "(simple-ridist beg dur freq amp) test instrument for rand-interp dist"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand-interp freq :distribution (inverse-integrate '(0 1 1 0)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (rand-interp os)) 0 *output*))))))

(define (simple-env beg dur freq amp)
  "(simple-env beg dur freq amp) test instrument for env"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (e (make-env '(0 0 1 1 2 1 3 0) :scaler amp :offset .1 :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* (env e) (oscil os)) 0 *output*))))))

(define* (simple-fof beg dur frq amp vib f0 a0 f1 a1 f2 a2 :optional ve ae)
  " (simple-fof beg dur frq amp vib f0 a0 f1 a1 f2 a2 :optional ve ae) test instrument for FOF"
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (ampf (make-env :envelope (or ae (list 0 0 25 1 75 1 100 0)) :scaler amp :duration dur))
         (frq0 (hz->radians f0))
         (frq1 (hz->radians f1))
         (frq2 (hz->radians f2))
         (foflen (if (= (mus-srate) 22050) 100 200))
         (vibr (make-oscil :frequency 6))
	 (vibenv (make-env :envelope (or ve (list 0 1 100 1)) :scaler vib :duration dur))
         (win-freq (/ two-pi foflen))
         (wt0 (make-wave-train :size foflen :frequency frq))
         (foftab (mus-data wt0)))
    (do ((i 0 (1+ i))) ((= i foflen))
      (set! (vct-ref foftab i) (double
				;; this is not the pulse shape used by B&R
				(* (+ (* a0 (sin (* i frq0))) 
				      (* a1 (sin (* i frq1))) 
				      (* a2 (sin (* i frq2)))) 
				   .5 (- 1.0 (cos (* i win-freq)))))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* (env ampf) (wave-train wt0 (* (env vibenv) (oscil vibr)))) 0 *output*))))))

(define (simple-amb beg dur freq amp)
  "(simple-amb beg dur freq amp) test instrument for osc?+rand"
  (let* ((os (if (> freq 1) (make-oscil freq) (make-rand freq)))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (if (oscil? os) (oscil os) (rand os))) 0 *output*))))))

(define (simple-rd beg dur amp file)
  "(simple-rd beg dur amp file) test instrument for readin"
  (let* ((rd (make-readin file))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (readin rd)) 0 *output*))))))

(define (simple-rd-start beg dur amp file channel start)
  "(simple-rd-start beg dur amp file channel start) test instrument for readin"
  (let* ((rd (make-readin file :channel channel :start start))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (readin rd)) 0 *output*))))))

(define (simple-cnv beg dur amp file)
  "(simple-cnv beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (filt (make-double-array 8)))
    (do ((i 0 (1+ i))) ((= i 8)) (set! (vct-ref filt i) (double 0.0)))
    (set! (vct-ref filt 4) (double 1.0))
    (let ((ff (make-convolve :input (make-readin file) :filter filt)))
      (run
       (lambda ()
	 (do ((i start (1+ i))) ((= i end))
	   (out-any i (* amp (convolve ff)) 0 *output*)))))))

(define (simple-cnf beg dur amp file)
  "(simple-cnf beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (filt (make-double-array 8)))
    (do ((i 0 (1+ i))) ((= i 8)) (set! (vct-ref filt i) (double 0.0)))
    (set! (vct-ref filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (lambda ()
	 (do ((i start (1+ i))) ((= i end))
	   (out-any i (* amp (convolve ff (lambda (dir) (readin rd)))) 
		    0 *output*)))))))

(define (simple-lrg beg dur amp file)
  "(simple-lrg beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (filt (make-double-array 8)))
    (do ((i 0 (1+ i))) ((= i 8)) (set! (vct-ref filt i) (double 0.0)))
    (set! (vct-ref filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (lambda ()
	 (do ((i start (1+ i))) ((= i end))
	   (out-any i (* amp (convolve ff (lambda (dir)
					       (if (= dir 1)
						   (readin rd)
						   0.0)))) 
		    0 *output*)))))))

(define (simple-cn2 beg dur amp file)
  "(simple-cn2 beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (filt (make-double-array 8)))
    (do ((i 0 (1+ i))) ((= i 8)) (set! (vct-ref filt i) (double 0.0)))
    (set! (vct-ref filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt))
	  (ff1 (make-convolve :filter filt :input (make-readin file))))
      (run
       (lambda ()
	 (do ((i start (1+ i))) ((= i end))
	   (out-any i (* amp (+ (convolve ff (lambda (dir)
						  (if (= dir 1)
						      (readin rd)
						      0.0)))
				 (convolve ff1))) 
		    0 *output*)))))))

(define (simple-src beg dur amp speed file)
  "(simple-src beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-src :input (make-readin file) :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (src sr)) 0 *output*))))))

(define (simple-src-f beg dur amp speed file)
  "(simple-src-f beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-src :input (make-readin file) :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (src sr 0.0 #f)) 0 *output*))))))

(define (simple-sr2 beg dur amp speed file)
  "(simple-sr2 beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (src sr 0.0 (lambda (dir) (if (= dir 1) (readin rd))))) 0 *output*))))))

(define (simple-sr2a beg dur amp speed file)
  "(simple-sr2a beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :input rd :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (src sr)) 0 *output*))))))

(define (simple-sro beg dur amp speed freq)
  "(simple-sro beg dur amp speed freq) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-src :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (src sr 0.0 (lambda (dir)
					    (oscil os)))) 
		  0 *output*))))))

(define (simple-grn beg dur amp speed freq)
  "(simple-grn beg dur amp speed freq) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-granulate :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate sr (lambda (dir) (oscil os)))) 0 *output*))))))

(define (simple-pvoc beg dur amp size file)
  "(simple-pvoc beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-phase-vocoder :input (make-readin file) :fft-size size)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (phase-vocoder sr)) 0 *output*))))))

(define (simple-ina beg dur amp file)
  "(simple-ina beg dur amp file) test instrument for ina"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (open-input file))
	 (ctr 0))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (in-any ctr 0 fil)) 0 *output*)
	 (set! ctr (+ ctr 1)))))))

(define (simple-in-rev beg dur ampa ampb)
  "(simple-in-rev beg dur ampa ampb) test instrument for in reverb"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (if (> ampa 0.0) (outa i (* ampa (ina i *reverb*)) *output*))
	 (if (> ampb 0.0) (outb i (* ampb (inb i *reverb*)) *output*)))))))

(define (simple-f2s beg dur amp file)
  "(simple-f2s beg dur amp file) test instrument for file->sample"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-file->sample file))
	 (ctr 0))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (file->sample fil ctr 0)) 0 *output*)
	 (set! ctr (1+ ctr)))))))

(define (simple-rdf beg dur amp file)
  "(simple-rdf beg dur amp file) test instrument for readin"
  (let* ((rd (make-readin file))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (readin rd)) 0 *output*))))))

(define (simple-loc beg dur freq amp)
  "(simple-loc beg dur freq amp) test instrument for locsig"
  (let* ((os (make-oscil freq))
	 (loc (make-locsig :degree 0.0 :channels 1 :output *output*))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (locsig loc i (* amp (oscil os))))))))

(define (simple-dloc beg dur freq amp)
  "(simple-dloc beg dur freq amp) test instrument for move-sound"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (loc (make-move-sound (list start end 1 0
				     (make-delay 32) (make-env '(0 0 1 1) :end 1000) (make-env '(0 0 1 1) :end 1000)
				     (vector (make-delay 32)) (vector (make-env '(0 0 1 1) :end 1000)) 
				     (vector (make-delay 32)) (vector 0 1))
			       *output*)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (move-sound loc i (* amp (oscil os))))))))

(define (simple-dloc-4 beg dur freq amp)
  "(simple-dloc-4 beg dur freq amp) test instrument for dlocsig"
  (let* ((os (make-oscil freq))
	 (start (floor (* beg (mus-srate))))
	 (end (+ start (floor (* dur (mus-srate)))))
	 (loc (make-move-sound (list start end 4 0
				     (make-delay 12) 
				     (make-env '(0 0 10 1) :duration dur)
				     #f
				     (make-vector 4 #f)
				     (vector (make-env '(0 0 1 1 2 0 3 0 4 0) :duration dur)
					     (make-env '(0 0 1 0 2 1 3 0 4 0) :duration dur)
					     (make-env '(0 0 1 0 2 0 3 1 4 0) :duration dur)
					     (make-env '(0 0 1 0 2 0 3 0 4 1) :duration dur))
				     #f
				     (vector 0 1 2 3))
			       *output*)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (move-sound loc i (* amp (oscil os))))))))

;(with-sound (:channels 4 :output "temp.snd") (simple-dloc-4 0 2 440 .5))

(define (simple-dup beg dur freq amp)
  "(simple-dup beg dur freq amp) test instrument for arith"
  (let* ((os (make-oscil freq))
	 (j 2)
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (let ((amp .3)
	       (j 4))
	   (if (not (= j 4)) (clm-print "local j is ~D\n" j))
	   (if (> (abs (- amp .3)) .001) (clm-print "local amp is ~F\n" amp)))
	 (if (= j 2)
	     (out-any i (* amp (oscil os)) 0 *output*)))))))

(define (simple-du1 beg dur freq amp)
  "(simple-du1 beg dur freq amp) test instrument for arith"
  (let* ((os (make-oscil freq))
	 (j (+ (expt 2 41) 1234)) ; 2199023256786
	 (mj -3)
	 (jj (- (+ (expt 2 40) 4321))) ; -1099511632097
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (if (not (= j 2199023256786)) (clm-print "local j is ~A" j))
	 (if (not (= jj -1099511632097)) (clm-print "local jj is ~A" jj))
	 (if (= mj -3)
	     (out-any i (* amp (oscil os)) 0 *output*)
	     (clm-print "minus 3: ~D" mj)))))))

(define (sample-desc beg dur freq amp)
  "(sample-desc beg dur freq amp) test instrument for generics"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (printed #f)
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (if (not printed)
	     (begin
	       (if (not (string=? (mus-describe os) "oscil freq: 440.000Hz, phase: 0.000"))
		   (clm-print "describe oscil: ~A~%" (mus-describe os)))
	       (if (> (abs (- (mus-frequency os) freq)) .001)
		   (clm-print "osc freq: ~A (~A)~%" (mus-frequency os) freq))
	       (set! printed #t)))
	 (out-any i (* amp (oscil os)) 0 *output*))))))

(define (sample-mdat beg dur freq amp)
  "(sample-mdat beg dur freq amp) test instrument for coeffs"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (table-size 256)
	 (j 0)
	 (buf (make-table-lookup freq 0.0 :size table-size))
	 (table (mus-data buf)))
    (do ((i 0 (1+ i)))
	((= i table-size))
      (set! (vct-ref table i) (double (/ i table-size))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (vct-ref (mus-data buf) j) (table-lookup buf)) 0 *output*)
	 (set! j (1+ j))
	 (if (>= j table-size) (set! j 0)))))))

(define (sample-xtab beg dur freq amp)
  "(sample-xtab beg dur freq amp) test instrument for generics"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (1+ i)))
	((= i 8))
      (set! (vct-ref (mus-xcoeffs flt) i) (double (/ i 16)))
      (set! (vct-ref (mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp
			(+ (vct-ref (mus-xcoeffs flt) 4)
			   (vct-ref (mus-ycoeffs flt) 4))
			(filter flt (oscil os))) 
		  0 *output*))))))

(define (sample-xts beg dur freq amp)
  "(sample-xts beg dur freq amp) test instrument for generics"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (1+ i)))
	((= i 8))
      (set! (vct-ref (mus-xcoeffs flt) i) (double (/ i 16)))
      (set! (vct-ref (mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (set! (vct-ref (mus-xcoeffs flt) 0) .5)
	 (set! (vct-ref (mus-ycoeffs flt) 0) .5)       
	 (out-any i (* amp
			(+ (vct-ref (mus-xcoeffs flt) 0)
			   (mus-ycoeff flt 0))
			(filter flt (oscil os))) 
		  0 *output*))))))

(define (sample-srl2 beg dur amp speed freq)
  "(sample-srl2 beg dur amp speed freq) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os1 (make-oscil freq))
	 (os2 (make-oscil (* freq 2)))
	 (sr1 (make-src :srate speed))
	 (sr2 (make-src :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (+ (src sr1 0.0 (lambda (dir) (oscil os1)))
			      (src sr2 0.0 (lambda (dir) (oscil os2))))) 
		  0 *output*))))))

(define (sample-srll beg dur amp speed freq)
  "(sample-srll beg dur amp speed freq) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr1 (make-src :srate speed))
	 (sr2 (make-src :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (src sr1 0.0 (lambda (dir)
					     (src sr2 0.0
						  (lambda (dir)
						      (oscil os)))))) 
		  0 *output*))))))

(define (sample-srl3 beg dur amp speed freq)
  "(sample-srl3 beg dur amp speed freq) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os1 (make-oscil freq))
	 (os2 (make-oscil freq))
	 (sr1 (make-src :srate speed))
	 (sr2 (make-src :srate speed))
	 (sr3 (make-src :srate speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (+ (src sr1 0.0 (lambda (dir)
						(src sr2 0.0
						     (lambda (dir)
							 (oscil os1)))))
			       (src sr3 0.0 (lambda (dir)
						(oscil os2))))) 
		  0 *output*))))))

(define (sample-grn2 beg dur amp speed freq)
  "(sample-grn2 beg dur amp speed freq) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-granulate :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate sr
				       (lambda (dir) 
					   (oscil os))
				       (lambda (g) 
					   (declare (g clm))
					   0))) 
		  0 *output*))))))

(define (sample-grn3 beg dur amp speed file)
  "(sample-grn3 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed))
	 (gr (make-granulate :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate gr (lambda (dir)
					      (src sr 0.0 (lambda (dir)
							      (readin rd))))))
		  0 *output*))))))

(define (sample-cnv beg dur amp speed file)
  "(sample-cnv beg dur amp speed file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed))	 
	 (filt (make-double-array 8)))
    (do ((i 0 (1+ i))) ((= i 8)) (set! (vct-ref filt i) (double 0.0)))
    (set! (vct-ref filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (lambda ()
	 (do ((i start (1+ i))) ((= i end))
	   (out-any i (* amp (convolve ff (lambda (dir)
					       (src sr 0.0 (lambda (dir)
							       (readin rd)))))) 
		    0 *output*)))))))

(define (sample-cnv1 beg dur amp speed file)
  "(sample-cnv1 beg dur amp speed file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed :input rd))	 
	 (filt (make-double-array 8)))
    (do ((i 0 (1+ i))) ((= i 8)) (set! (vct-ref filt i) (double 0.0)))
    (set! (vct-ref filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (lambda ()
	 (do ((i start (1+ i))) ((= i end))
	   (out-any i (* amp (convolve ff (lambda (dir)
					       (src sr)))) 
		    0 *output*)))))))

(define (sample-pvoc1 beg dur amp size file)
  "(sample-pvoc1 beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (phase-vocoder sr
					   (lambda (dir)
					       (readin fil)))) 
		  0 *output*))))))

(define (sample-pvoc2 beg dur amp size file)
  "(sample-pvoc2 beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (phase-vocoder sr
					   (lambda (dir)
					       (readin fil))
					   #f
					   (lambda (closure)
					       (declare (closure clm))
					       (if (not (= (phase-vocoder-outctr sr) 0))
						   (clm-print "outctr: ~A" (phase-vocoder-outctr sr)))
					       #t)
					   #f)) 
		  0 *output*))))))

(define (sample-pvoc3 beg dur amp size file)
  "(sample-pvoc3 beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (k 0)
	 (N2 (/ size 2))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i 
           (* amp (phase-vocoder sr
	            (lambda (dir)
		      (readin fil))
		    #f
		    #f
		    (lambda (closure)
		      (declare (closure clm))
		      (set! k 0)
		      (do ()
			  ((= k N2))
			(set! (vct-ref (phase-vocoder-amps sr) k) 
			      (+ (vct-ref (phase-vocoder-amps sr) k) 
				 (vct-ref (phase-vocoder-amp-increments sr) k)))
			(set! (vct-ref (phase-vocoder-phase-increments sr) k) 
			      (+ (vct-ref (phase-vocoder-phase-increments sr) k) 
				 (vct-ref (phase-vocoder-freqs sr) k)))
			(set! (vct-ref (phase-vocoder-phases sr) k) 
			      (+ (vct-ref (phase-vocoder-phases sr) k)
				 (vct-ref (phase-vocoder-phase-increments sr) k)))
			(set! k (1+ k)))
		      (sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2)))) 
	   0 *output*))))))

(define (sample-mxf beg dur freq amp)
  "(sample-mxf beg dur freq amp) test instrument for frames"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (frm (make-frame 2 .7 .3))
	 (smp (make-frame 2))
	 (os (make-oscil freq))
	 (int 0)
	 (gen (make-mixer 2 .5 .25 .125 1.0))
	 (fr0 (make-frame 2 1.0 1.0))
	 (fr1 (make-frame 2 0.0 0.0))
	 (fr3 (make-frame 2))
	 (fr4 (make-frame 4))
	 (fr5 (make-frame 4))
	 (mx1 (make-scalar-mixer 2 2.0))
	 (mx2 (make-mixer 2 .1 .2 .3 .4))
	 (nmx (make-mixer 2)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (let ((val (* (oscil os) amp)))
	   (frame-set! smp 0 val)
	   (set! (frame-ref smp (+ int 1)) val)
	   (out-any i (frame->sample frm smp) 0 *output*)
	   (if (not (frame? fr0)) (clm-print ";~S not a frame?" (mus-describe fr0)))
	   (if (not (mixer? gen)) (clm-print ";~S not a mixer?" (mus-describe gen)))
	   (if (not (= (mus-channels fr0) 2)) (clm-print ";frame channels: ~D?" (mus-channels fr0)))
	   (if (not (= (mus-length fr1) 2)) (clm-print ";frame length: ~D?" (mus-length fr0)))
	   (if (not (= (mus-channels gen) 2)) (clm-print ";mixer channels: ~D?" (mus-channels gen)))
	   (frame->frame fr0 gen fr1)
	   (if (or (> (abs (- (frame-ref fr0 0) 1.0)) .001)
		   (> (abs (- (frame-ref fr1 1) 1.25)) .001)
		   (> (abs (- (mixer-ref gen 0 0) .5)) .001))
	       (clm-print ";fr1: ~A" (mus-describe fr1)))
	   (frame-set! fr1 0 1.0)
	   (set! (frame-ref fr1 1) 1.0)
	   (frame+ fr0 fr1 fr3)
	   (frame* fr0 fr1 fr4)
	   (sample->frame fr1 .5 fr5)
	   (if (or (> (abs (- (frame-ref fr3 0) 2.0)) .001)
		   (> (abs (-(frame-ref fr4 0) 1.0)) .001))
	       (clm-print ";fr+*: ~A ~A" (mus-describe fr3) (mus-describe fr4)))
	   (if (> (abs (- (frame-ref fr5 0) .5)) .001)
	       (clm-print ";sample->frame: ~A?" (frame-ref fr5 0)))
	   (mixer+ mx1 mx2 nmx)
	   (if (or (> (abs (- (mixer-ref mx1 0 0) 2.0)) .001)
		   (> (abs (- (mixer-ref mx1 0 1) 0.0)) .001)
		   (> (abs (- (mixer-ref mx1 1 0) 0.0)) .001)
		   (> (abs (- (mixer-ref mx1 1 1) 2.0)) .001))
	       (clm-print ";make-scalar-mixer 2: ~A" (mus-describe mx1)))
	   (if (or (> (abs (- (mixer-ref mx2 0 0) .1)) .001)
		   (> (abs (- (mixer-ref mx2 0 1) .2)) .001)
		   (> (abs (- (mixer-ref mx2 1 0) .3)) .001)
		   (> (abs (- (mixer-ref mx2 1 1) .4)) .001))
	       (clm-print ";make-mixer .1 .2 .3 .4: ~A" (mus-describe mx2)))
	   (if (or (> (abs (- (mixer-ref nmx 0 0) 2.1)) .001)
		   (> (abs (- (mixer-ref nmx 0 1) 0.2)) .001)
		   (> (abs (- (mixer-ref nmx 1 0) 0.3)) .001)
		   (> (abs (- (mixer-ref nmx 1 1) 2.4)) .001))
	       (clm-print ";mixer add ~A ~A: ~A" (mus-describe mx1) (mus-describe mx2) (mus-describe nmx)))
	   (mixer* mx1 .5 mx1)
	   (if (or (> (abs (- (mixer-ref mx1 0 0) 1.0)) .001)
		   (> (abs (- (mixer-ref mx1 0 1) 0.0)) .001)
		   (> (abs (- (mixer-ref mx1 1 0) 0.0)) .001)
		   (> (abs (- (mixer-ref mx1 1 1) 1.0)) .001))
	       (clm-print ";make-scale (identity): ~A" (mus-describe mx1)))
	   (do ((j 0 (1+ j)))
	       ((= j 2))
	     (do ((k 0 (1+ k)))
		 ((= k 2))
	       (mixer-set! nmx j k 0.0)
	       (set! (mixer-ref mx1 j k) (exact->inexact (+ j k)))
	       (set! (mixer-ref mx2 j k) (exact->inexact (* j k)))))
	   (mixer* mx1 mx2 nmx)
	   (if (or (> (abs (- (mixer-ref nmx 0 0) 0.0)) .001)
		   (> (abs (- (mixer-ref nmx 0 1) 1.0)) .001)
		   (> (abs (- (mixer-ref nmx 1 0) 0.0)) .001)
		   (> (abs (- (mixer-ref nmx 1 1) 2.0)) .001))
	       (clm-print ";mixer*: ~A" (mus-describe nmx)))
	   (do ((j 0 (1+ j)))
	       ((= j 2))
	     (do ((k 0 (1+ k)))
		 ((= k 2))
	       (set! (mixer-ref mx1 j k) 0.0)
	       (set! (mixer-ref nmx j k) 0.0)))
	   (do ((j 0 (1+ j)))
	       ((= j 2))
	     (mixer-set! mx1 j j 2.0))
	   (mixer-set! mx2 0 0 .1)
	   (mixer-set! mx2 0 1 .2)
	   (mixer-set! mx2 1 0 .3)
	   (mixer-set! mx2 1 1 .4)))))))

(define (sample-osc beg dur freq amp)
  "(sample-osc beg dur freq amp) test instrument for oscil"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 20))
	 (arrfrq (make-double-array 20 :initial-element (double 0.0))))
    (do ((i 0 (1+ i)))
	((= i 20))
      (set! (vct-ref arrfrq i) (double (* (1+ i) 100.0)))
      (vector-set! arr i (make-oscil (* (1+ i) 100))))
    (run
     (lambda ()
       (do ((k start (1+ k))) ((= k end))
	 (let ((sum 0.0))
	   (do ((i 0 (1+ i)))
	       ((= i (vector-length arr)))
	     (if (oscil? (vector-ref arr i))
		 (begin
		   (set! (mus-frequency (vector-ref arr i)) (vct-ref arrfrq i))
		   (if (> (abs (- (mus-frequency (vector-ref arr i)) (vct-ref arrfrq i))) .001)
		       (clm-print "oops ~A ~A" (mus-frequency (vector-ref arr i)) (vct-ref arrfrq i)))
		   (set! sum (+ sum (oscil (vector-ref arr i)))))))
	   (out-any k (* amp .05 sum) 0 *output*)))))))

(define (sample-ardcl beg dur freq amp)
  "(sample-ardcl beg dur freq amp) test instrument for arith"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (amps (make-double-array 3 :initial-element 0.0))
	 (phases (make-double-array 3 :initial-element 0.0))
	 (freqs (make-double-array 3 :initial-element 0.0))
	 (ints (make-vector 3 32)))
    (do ((i 0 (1+ i)))
	((= i 3))
      (set! (vct-ref freqs i) (double (* freq (1+ i))))
      (set! (vct-ref amps i) (double (/ amp (+ i 2)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (do ((i 0 (1+ i)))
	     ((= i (vct-length phases)))
	   (set! (vct-ref phases i) (+ (vct-ref phases i) (hz->radians (vct-ref freqs i)))))
	 (if (not (= (vector-ref ints 0) 32)) (clm-print "int array trouble"))
	 (vector-set! ints 1 3)
	 (if (not (= (vector-ref ints 1) 3)) (clm-print "set int array trouble"))
	 (if (not (= (vct-length amps) 3)) (clm-print "amps len: ~A" (vct-length amps)))
;	 (if (not (= (array-rank amps) 1)) (clm-print "amps rank: ~A" (array-rank amps)))
;	 (if (not (= (array-dimension amps 0) 3)) (clm-print "amps dim 0: ~A" (array-dimension amps 0)))
;	 (if (not (= (array-total-size amps) 3)) (clm-print "amps total size: ~A" (array-total-size amps)))
;	 (if (not (array-in-bounds-p amps 2)) (clm-print "amps in bounds 2"))
;	 (if (array-in-bounds-p amps 21) (clm-print "amps in bounds 21"))
	 (out-any i (sine-bank amps phases) 0 *output*))))))

(define (sample-strs beg dur freq amp)
  "(sample-strs beg dur freq amp) test instrument for strings"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (filename "oboe.snd"))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (if (not (string=? "oscil" (mus-name os))) (clm-print "oscil name: ~A" (mus-name os)))
	 (if (> (abs (- (mus-sound-duration filename) 2.305)) .001) (clm-print "sound-duration: ~A" (mus-sound-duration filename)))
	 (if (and (not (string=? (mus-header-type-name (mus-sound-header-type "oboe.snd")) "Sun/Next"))
		  (not (string=? (mus-header-type-name (mus-sound-header-type "oboe.snd")) "Sun"))
		  (not (string=? (mus-header-type-name (mus-sound-header-type "oboe.snd")) "Next")))
	     (clm-print "header type: ~A" (mus-header-type-name (mus-sound-header-type "oboe.snd"))))
	 (if (not (string=? (mus-data-format-name (mus-sound-data-format "oboe.snd")) "big endian short (16 bits)"))
	     (clm-print "data format: ~A" (mus-data-format-name (mus-sound-data-format "oboe.snd"))))
	 (if (not (= (mus-sound-datum-size "oboe.snd") 2)) (clm-print ";datum size: ~A" (mus-sound-datum-size filename)))
	 (if (not (= (mus-sound-chans "oboe.snd") 1)) (clm-print ";chans: ~A" (mus-sound-chans filename)))
	 (if (not (= (mus-sound-data-location filename) 28)) (clm-print ";data-location: ~A" (mus-sound-data-location filename)))
	 (if (not (= (mus-sound-length filename) 101684)) (clm-print ";length: ~A" (mus-sound-length filename)))
	 (if (not (= (mus-sound-samples filename) 50828)) (clm-print ";samples: ~A" (mus-sound-samples filename)))              
	 (if (not (= (mus-sound-frames filename) 50828)) (clm-print ";frames: ~A" (mus-sound-samples filename)))
	 (if (not (= (mus-sound-srate filename) 22050)) (clm-print ";srate: ~A" (mus-sound-srate filename)))       
	 (out-any i (oscil os) 0 *output*))))))

(define (sample-flt beg dur freq amp)
  "(sample-flt beg dur freq amp) test instrument for arith"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fltdat (make-double-array 3 :initial-element (double 3.14)))
	 (intdat (make-integer-array 3 :initial-element 3))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (1+ i)))
	((= i 8))
      (set! (vct-ref (mus-xcoeffs flt) i) (double (/ i 16)))
      (set! (vct-ref (mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (let ((xs (mus-xcoeffs flt)))
	   (if (or (> (abs (- (vct-ref xs 1) (mus-xcoeff flt 1))) .001)
		   (> (abs (- (vct-ref xs 1) 0.0625)) .001))
	       (clm-print "~A ~A~%" (vct-ref xs 1) (mus-xcoeff flt 1))))
	 (let ((data (mus-data flt)))
	   (if (> (vct-ref data 0) 1.0) (clm-print "data overflow? ~A~%" (vct-ref data 0))))
	 (let ((is intdat)
	       (fs fltdat))
	   (if (not (= (vct-ref is 1) 3)) (clm-print "intdat let: ~A~%" (vct-ref is 1)))
	   (if (> (abs (- (vct-ref fs 1) 3.14)) .001) (clm-print "fltdat let: ~A~%" (vct-ref fs 1))))
	 (out-any i (* amp (filter flt (oscil os))) 0 *output*))))))

(define (sample-arrintp beg dur freq amp)
  "(sample-arrintp beg dur freq amp) test instrument for array-interp"
  (let* ((os (make-oscil freq))
	 (arr (make-double-array 101))
	 (start (seconds->samples beg))
	 (len (seconds->samples dur))
	 (end (+ start len))
	 (loc 0.0)
	 (loc-incr (/ 100.0 len)))
    (do ((i 0 (1+ i))
	 (x 0.0 (+ x .01)))
	((= i 100))
      (set! (vct-ref arr i) (double x)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (array-interp arr loc) (oscil os)) 0 *output*)
	 (set! loc (+ loc loc-incr)))))))

(define (sample-if beg dur freq amp)
  "(sample-if beg dur freq amp) test instrument for ifs"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (k -123)
	 (j 0)
	 (bool #t))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (if (and (= i start) (not (= k -123))) (clm-print "init k: ~A~%" k))
	 (if (not bool) (clm-print "bool: ~A~%" bool))
	 (set! j (if bool 1 2))
	 (if (not (= j 1)) (clm-print "if expr: ~A~%" j))
	 (if bool (set! j 3) (set! j 4))
	 (if (not (= j 3)) (clm-print "if statement: ~A~%" j))
	 (if (integer? k) (set! j 5))
	 (if (not (= j 5)) (clm-print "int k? ~A ~A~%" (integer? k) j))
	 (if (= j k) (set! j 6))
	 (if (= j 6) (clm-print "j if false: ~A~%" j))
	 (set! j (if (= j k) (+ k 7) (+ k 8)))
	 (if (not (= j (+ k 8))) (clm-print "if false expr: ~A ~A~%" j k))
	 (set! j (if (> j -1234) (if (> k -1234) 9 10) 11))
	 (if (not (= j 9)) (clm-print "if 2 expr: ~A~%" j))
	 (set! j (if (> j -1234) (begin (set! k 0) 12) 13))
	 (if (not (= j 12)) (clm-print "if begin expr: ~A~%" j))
	 (if (> j -1234) (begin (set! j 1234) (set! j 14)) (set! j 15))
	 (if (not (= j 14)) (clm-print "if begin: ~A~%" j))
;	 (if (> j -1234) (set! j (prog1 16 (set! k 0))))
;	 (if (not (= j 16)) (clm-print "if prog1: ~A~%" j))
;	 (if (> j -1234) (set! j (prog2 (set! k 0) 17 (set! k 0))))
;	 (if (not (= j 17)) (clm-print "if prog2: ~A~%" j))
;       (set! k (loop for j from 1 to 4 sum j))
;       (if (not (= k 10)) (clm-print "loop sum: ~A~%" k))
;	 (if (> j -1234) (set! j (prog2 (set! k 0) (if (> j -1234) (begin (set! k 123) 18) 19) (set! k 0))))
;	 (if (not (= j 18)) (clm-print "if nested prog2: ~A~%" j))
	 (set! j 123)
	 (cond ((= j 0) (set! k -1))
	       ((= j 12) (set! k -2))
	       ((= j 123) (set! k -3))
	       (#t (set! k -4)))
	 (if (not (= k -3)) (clm-print "cond: ~A ~A~%" j k))
	 (set! k (cond ((= j 0) -4)
		       ((= j 12) -5)
		       (#t -6)))
	 (if (not (= k -6)) (clm-print "cond expr: ~A ~A~%" j k))
	 (set! k (let ((a 123))
		   (if (> a 0)
		       20
		       32)))
	 (if (not (= k 20)) (clm-print "let expr: ~A ~A~%" j k))
	 (let ((a 123))
	   (set! k a))
	 (if (not (= k 123)) (clm-print "let: ~A ~A~%" j k))
	 (set! k 123)
	 (set! bool (= k 123))
	 (if (not bool) (clm-print "bool expr: ~A~%" bool))
	 (set! bool (if (= k 123) (> k 0) (< k 0)))
	 (if (not bool) (clm-print "if bool expr: ~A~%" bool))
	 (set! j 0)
	 (set! k (do ((m 0 (1+ m)))
		     ((= m 3) j)
		   (set! j (+ j 1))))
	 (if (not (= k 3)) (clm-print "do expr: ~A~%" k))
;	 (dotimes (m 2)
;		  (set! k (- k 1)))
;	 (if (not (= k 1)) (clm-print "dotimes: ~A~%" k))
	 (out-any i (* amp (oscil os)) 0 *output*))))))

(define (sample-arrfile beg dur freq amp)
  "(sample-arrfile beg dur freq amp) test instrument for arrays"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-double-array 100 :initial-element (double 0.0)))
	 (ctr 0)
	 (dir 1))
    (do ((i 0 (1+ i)))
	((= i 100))
      (set! (vct-ref arr i) (double (* amp (+ -.5 (* i .01))))))
    (array->file "test.data" arr 100 22050 1)
    (do ((i 0 (1+ i)))
	((= i 100))
      (set! (vct-ref arr i) (double 0.0)))
    (file->array "test.data" 0 0 100 arr)
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* (vct-ref arr ctr) (oscil os)) 0 *output*)
	 (set! ctr (+ ctr dir))
	 (if (>= ctr 99) (set! dir -1)
	     (if (<= ctr 0) (set! dir 1))))))))

(define (simple-grn-f1 beg dur amp speed freq)
  "(simple-grn-f1 beg dur amp speed freq) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-granulate :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate sr (lambda (dir) (oscil os)) #f)) 0 *output*))))))

;(with-sound () (simple-grn-f1 0 1 .1 2 440))

(define (simple-grn-f2 beg dur amp speed file)
  "(simple-grn-f2 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate sr)) 0 *output*))))))

;(with-sound () (simple-grn-f2 0 1 1 2 "oboe.snd"))

(define (simple-grn-f3 beg dur amp speed file)
  "(simple-grn-f3 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate sr #f #f)) 0 *output*))))))

;(with-sound () (simple-grn-f3 0 1 1 2 "oboe.snd"))

(define (simple-grn-f4 beg dur amp speed file)
  "(simple-grn-f4 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate sr #f)) 0 *output*))))))

;(with-sound () (simple-grn-f4 0 1 1 2 "oboe.snd"))

(define (simple-grn-f5 beg dur amp speed file)
  "(simple-grn-f5 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (granulate sr #f
				      (lambda (g)
					(declare (g clm))
					(let ((grain (mus-data g))  ; current grain
					      (len (mus-length g))) ; current grain length
					  (do ((i 0 (1+ i)))
					      ((= i len) len)       ; grain length unchanged in this case
					    (vct-set! grain i (* 2 (vct-ref grain i)))))
					0)))
		  0 *output*))))))

;(with-sound () (simple-grn-f5 0 1 1 2 "oboe.snd"))

(define (sample-pvoc5 beg dur amp size file freq)
  "(sample-pvoc5 beg dur amp size file freq) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size))
	 (os (make-oscil freq)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (phase-vocoder sr
					   (lambda (dir)
					       (readin fil))
					   #f
					   #f
					   (lambda (closure)
					       (declare (closure clm))
					       (oscil os))))
		  0 *output*))))))

;(with-sound () (sample-pvoc5 0 1 .1 256 "oboe.snd" 440.0))


#|
(with-sound (:statistics #t)
	    (simple-ssb 0 .2 440 .1)
	    (simple-sos .25 .2 .1)
	    (simple-soc 0.5 .2 440 .1)
	    (simple-osc 0.75 .2 440 .1)
	    (simple-sss 1.0 .2 .1)
	    (simple-asy 1.25 .2 .1)
	    (simple-saw 1.5 .2 .1)
	    (simple-tri 1.75 .2 .1)
	    (simple-pul 2.0 .2 .1)
	    (simple-sqr 2.25 .2 .1)
	    (simple-sib 2.5 .2 440.0 .1)
	    (simple-oz 2.75 .2 440.0 .1)
	    (simple-op 3.0 .2 440.0 .1)
	    (simple-tz 3.25 .2 440.0 .1)
	    (simple-tp 3.5 .2 440.0 .1)
	    (simple-frm 3.75 .2 440.0 .1)
	    (simple-wav 4.0 .2 440.0 .1)
	    (simple-buf 4.5 .2 440.0 .1)
	    (simple-dly 4.75 .2 440.0 .1)
	    (simple-cmb 5.0 .2 440.0 .1)
	    (simple-not 5.25 .2 440.0 .1)
	    (simple-alp 5.5 .2 440.0 .1)
	    (simple-ave 5.75 .2 440.0 .1)
	    (simple-tab 6.0 .2 440.0 .1)
	    (simple-flt 6.25 .2 440.0 .1)
	    (simple-fir 6.5 .2 440.0 .1)
	    (simple-iir 6.5 .2 440.0 .3)
	    (simple-f 6.75 .2 440.0 .1)
	    (simple-ran 7.0 .2 440.0 .1)
	    (simple-ri 7.25 .2 440.0 .1)
	    (simple-env 7.5 .2 440.0 .1)
	    (simple-amb 7.75 .2 440.0 .1)
	    (simple-fof 8 1 270 .1 .001 730 .6 1090 .3 2440 .1) ;"Ahh"
	    (simple-fof 9 4 270 .1 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
			'(0 0 .5 1 3 .5 10 .2 20 .1 50 .1 60 .2 85 1 100 0))
	    (simple-fof 9 4 (* 6/5 540) .1 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
			'(0 0 .5 .5 3 .25 6 .1 10 .1 50 .1 60 .2 85 1 100 0))
	    (simple-fof 9 4 135 .1 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
			'(0 0 1 3 3 1 6 .2 10 .1 50 .1 60 .2 85 1 100 0))
	    (simple-rd 13.5 .45 .75 "oboe.snd")
	    (simple-cnv 14.0 .45 .75 "oboe.snd")
	    (simple-cnf 14.5 .45 .75 "oboe.snd")
	    (simple-lrg 15.0 .45 .75 "oboe.snd")
	    (simple-cn2 15.5 .45 .4 "oboe.snd")
	    (simple-src 16  .45 1.0 2.0 "oboe.snd")
	    (simple-sr2 16.5 .45 1.0 2.0 "oboe.snd")
	    (simple-sr2a 16.75 .45 1.0 2.0 "oboe.snd")
	    (simple-rndist 17.0 .2 440.0 .1)
	    (simple-ridist 17.25 .2 440.0 .1)
	    (simple-sro 17.5 .45 .1 .5 440)
	    (simple-grn 18 .2 .1 1.0 440)
	    (simple-pvoc 18.25 .2 .4 256 "oboe.snd")
	    (simple-ina 18.5 .45 1 "oboe.snd")
	    (simple-rdf 19 .45 1 "oboe.snd")
	    (simple-f2s 19.5 .45 1 "oboe.snd")
	    (simple-loc 20 .2 440 .1)
	    (simple-out 20.25 .2 440 .1)		  
	    (simple-dup 20.5 .2 440 .1)
	    (simple-du1 20.75 .2 440 .1)))

(with-sound (:statistics #t)
	    (sample-desc 0 .2 440 .1)
	    (sample-mdat .25 .2 440 .1)
	    (sample-xtab .5 .2 440 .1)
	    (sample-xts .75 .2 440 .1)
	    (sample-srl2 1 .2 .2 .5 (* 440 2))
	    (sample-srll 1.25 .2 .1 .5 (* 440 4))
	    (sample-srl3 1.5 .2 .1 .5 880)
	    (sample-grn2 1.75 .2 .1 .5 880)
	    (sample-grn3 2 .45 1 1 "oboe.snd")
	    (sample-cnv 2.5 .45 1 1 "oboe.snd")
	    (sample-cnv1 3.0 .45 1 1 "oboe.snd")
	    (sample-pvoc1 3.5 .45 1 512 "oboe.snd")
	    (sample-pvoc2 4.0 .45 1 512 "oboe.snd")
	    (sample-pvoc3 4.5 .001 1 512 "oboe.snd")
	    (sample-mxf 5 .2 440 .1)
	    (sample-osc 5.25 .2 440 .1)
	    (sample-ardcl 5.5 .2 440 .1)
	    (sample-strs 5.75 .2 440 .1)
	    (sample-flt 6 .2 440 .1)
	    (sample-arrintp 6.25 .2 440 .1)
	    (sample-if 6.5 .2 440 .1)
	    (sample-arrfile 6.75 .2 440 .15)
	    (sample-pvoc5 7 .2 .1 256 "oboe.snd" 440.0)
	    )
|#

(define (pvoc-a beg dur amp size file)
  "(pvoc-a beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-phase-vocoder :input (make-readin file) :fft-size size :interp (/ size 4) :overlap 4)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (phase-vocoder sr)) 0 *output*))))))

(define (pvoc-b beg dur amp size file)
  "(pvoc-b beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (phase-vocoder sr (lambda (dir) (readin rd)))) 0 *output*))))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-b 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (snd-display ";pvoc a-b: ~A" mx)))
|#

(define (pvoc-c beg dur amp size file)
  "(pvoc-c beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4)))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i 
	   (* amp
	      (phase-vocoder sr 
		(lambda (dir) (readin rd))
		#f
		#f
		(lambda (closure)
		  (declare (closure clm))
		  (let ((N2 (inexact->exact (/ size 2))))
		    (do ((k 0 (1+ k)))
			((= k N2))
		      (set! (vct-ref (phase-vocoder-amps sr) k) (+ (vct-ref (phase-vocoder-amps sr) k) 
								   (vct-ref (phase-vocoder-amp-increments sr) k)))
		      (set! (vct-ref (phase-vocoder-phase-increments sr) k) (+ (vct-ref (phase-vocoder-phase-increments sr) k) 
									       (vct-ref (phase-vocoder-freqs sr) k)))
		      (set! (vct-ref (phase-vocoder-phases sr) k) (+ (vct-ref (phase-vocoder-phases sr) k)
								     (vct-ref (phase-vocoder-phase-increments sr) k))))
		    (sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2)))
		))
	   0 *output*))))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-c 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (snd-display ";pvoc a-c: ~A" mx)))
|#


(define (pvoc-d beg dur amp size file)
  "(pvoc-d beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4))
	 (N2 (inexact->exact (/ size 2)))
	 (lastphases (make-vct N2 0.0))
	 (two-pi (* 2 pi)))
    (run
     (lambda ()
       (declare (n2 integer))
       (do ((i start (1+ i))) ((= i end))
	 (out-any i 
	   (* amp
	      (phase-vocoder sr 
		(lambda (dir) (readin rd))
		#f
		(lambda (closure)
		  (declare (closure clm))
		  (let* ((D (inexact->exact (/ size 4))) ; overlap = 4
			 (pscl (/ 1.0 D))
			 (kscl (/ two-pi size)))
		    (do ((k 0 (1+ k))
			 (ks 0.0 (+ ks kscl)))
			((= k N2))
		      (let* ((freq (vct-ref (phase-vocoder-freqs sr) k))
			     (diff (- freq (vct-ref lastphases k))))
			(vct-set! lastphases k freq)
			(if (> diff pi) (set! diff (- diff two-pi)))
			(if (< diff (- pi)) (set! diff (+ diff two-pi)))
			(vct-set! (phase-vocoder-freqs sr) k (+ (* diff  pscl) ks))))
		    #f))
		(lambda (closure)
		  (declare (closure clm))
		  (do ((k 0 (1+ k)))
		      ((= k N2))
		    (set! (vct-ref (phase-vocoder-amps sr) k) (+ (vct-ref (phase-vocoder-amps sr) k) 
								 (vct-ref (phase-vocoder-amp-increments sr) k)))
		    (set! (vct-ref (phase-vocoder-phase-increments sr) k) (+ (vct-ref (phase-vocoder-phase-increments sr) k) 
									     (vct-ref (phase-vocoder-freqs sr) k)))
		    (set! (vct-ref (phase-vocoder-phases sr) k) (+ (vct-ref (phase-vocoder-phases sr) k)
								   (vct-ref (phase-vocoder-phase-increments sr) k))))
		  (sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2))
		))
	   0 *output*))))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-d 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (snd-display ";pvoc a-d: ~A" mx)))
|#

(define (pvoc-e beg dur amp size file)
  "(pvoc-e beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4))
	 (N2 (inexact->exact (/ size 2)))
	 (lastphases (make-vct N2 0.0))
	 (in-data (make-vct size 0.0))
	 (two-pi (* 2 pi))
	 (filptr 0)
	 (window (make-fft-window hamming-window size 0.0))
	 (D (inexact->exact (/ size 4)))) ; overlap = 4
    (vct-scale! window (/ 2.0 (* 0.54 size)))
    (run
     (lambda ()
       (declare (n2 integer) (D integer))
       (do ((i start (1+ i))) ((= i end))
	 (out-any i 
	   (* amp
	      (phase-vocoder sr 
		(lambda (dir) (readin rd))

		(lambda (closure input)
		  (declare (closure clm))
		  (let ((buf (modulo filptr size)))
		    (clear-array (phase-vocoder-freqs sr))
		    (if (= filptr 0)
			(do ((k 0 (1+ k)))
			    ((= k size))
			  (vct-set! in-data k (readin rd)))
			(begin
			  (do ((k 0 (1+ k))
			       (j D (1+ j)))
			      ((= j size))
			    (vct-set! in-data k (vct-ref in-data j)))
			  (do ((k (- size D) (1+ k)))
			      ((= k size))
			    (vct-set! in-data k (readin rd)))))
		    (do ((k 0 (1+ k)))
			((= k size))
		      (vct-set! (phase-vocoder-amp-increments sr) buf (* (vct-ref in-data k) (vct-ref window k)))
		      (set! buf (1+ buf))
		      (if (>= buf size) (set! buf 0)))
		    (set! filptr (+ filptr D))
		    (mus-fft (phase-vocoder-amp-increments sr) (phase-vocoder-freqs sr) size 1)
		    (rectangular->polar (phase-vocoder-amp-increments sr) (phase-vocoder-freqs sr))
		    #f))

		(lambda (closure)
		  (declare (closure clm))
		  (let* ((pscl (/ 1.0 D))
			 (kscl (/ two-pi size)))
		    (do ((k 0 (1+ k))
			 (ks 0.0 (+ ks kscl)))
			((= k N2))
		      (let* ((freq (vct-ref (phase-vocoder-freqs sr) k))
			     (diff (- freq (vct-ref lastphases k))))
			(vct-set! lastphases k freq)
			(if (> diff pi) (set! diff (- diff two-pi)))
			(if (< diff (- pi)) (set! diff (+ diff two-pi)))
			(vct-set! (phase-vocoder-freqs sr) k (+ (* diff  pscl) ks))))
		    #f))

		(lambda (closure)
		  (declare (closure clm))
		  (do ((k 0 (1+ k)))
		      ((= k N2))
		    (set! (vct-ref (phase-vocoder-amps sr) k) (+ (vct-ref (phase-vocoder-amps sr) k) 
								 (vct-ref (phase-vocoder-amp-increments sr) k)))
		    (set! (vct-ref (phase-vocoder-phase-increments sr) k) (+ (vct-ref (phase-vocoder-phase-increments sr) k) 
									     (vct-ref (phase-vocoder-freqs sr) k)))
		    (set! (vct-ref (phase-vocoder-phases sr) k) (+ (vct-ref (phase-vocoder-phases sr) k)
								   (vct-ref (phase-vocoder-phase-increments sr) k))))
		  (sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2))
		))
	   0 *output*))))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-e 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (snd-display ";pvoc a-e: ~A" mx)))
|#

(define (or1)
  "(or1) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :end 10))
	(e2 (make-env '(0 1 1 0) :end 10))
	(e3 #f)
	(ok1 0.0))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 1))
	 (set! ok1 0.0)
	 (if (or e1 e2)
	     (set! ok1 (+ ok1 (env e1)))
	     (clm-print ";or1 a~%"))
	 (if (not (or e1 e2))
	     (clm-print ";or1 1~%"))
	 (if (and e1 e2)
	     (set! ok1 (+ ok1 (env e2)))
	     (clm-print ";or1 b~%"))
	 (if (not (and e1 e2))
	     (clm-print ";or1 2~%"))
	 (if (or e3 e1 e2)
	     (mus-reset e2) ; resets e2 -> 1.0
	     (clm-print ";or1 c~%"))
	 (if (and e1 e3 e2)
	     (clm-print ";or1 3~%"))
	 (if (not e1)
	     (clm-print ";or1 4~%"))
	 (if (< (abs ok1) .001)
	     (clm-print ";or1 ok1: ~A~%" ok1)))))))

(define (or2)
  "(or2) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :end 10))
	(e2 (make-env '(0 1 1 0) :end 10))
	(e3 #f)
	(ok1 0.0)
	(oki 0)
	(okb #f))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 1))
	 (declare (e1 clm) (e2 clm) (e3 clm)
		  (ok1 float)
		  (oki int)
		  (okb boolean))
	 (set! ok1 0.0)
	 (set! oki (1+ oki))
	 (set! okb #t)
	 (if (or e1 e2)
	     (set! ok1 (+ ok1 (env e1)))
	     (clm-print ";or2 a~%"))
	 (if (not (or e1 e2))
	     (clm-print ";or2 1~%"))
	 (if (and e1 e2)
	     (set! ok1 (+ ok1 (env e2)))
	     (clm-print ";or2 b~%"))
	 (if (not (and e1 e2))
	     (clm-print ";or2 2~%"))
	 (if (or e3 e1 e2)
	     (mus-reset e2) ; resets e2 -> 1.0
	     (clm-print ";or2 c~%"))
	 (if (and e1 e3 e2)
	     (clm-print ";or2 3~%"))
	 (if (not e1)
	     (clm-print ";or2 4~%"))
	 (if (< (abs ok1) .001)
	     (clm-print ";or1 ok1: ~A~%" ok1)))))))

(define (or3)
  "(or3) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :end 10))
	(i1 (make-vector 3 32))
	(f1 (make-vct 3 3.14))
	(i2 (make-vector 3 3))
	(f2 (make-vector 3 1.5))
	(ok1 0.0)
	(oki 0))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 1))
	 (cond (e1 (set! ok1 (+ ok1 (env e1))))
	       (#t (clm-print ";or3 1~%")))
	 (if (or f1 f2)
	     (set! ok1 (+ ok1 (vector-ref f2 0)))
	     (clm-print ";or3 a~%"))
	 (if (not (or f2 f1))
	     (clm-print ";or3 2~%"))
	 (if (and f2 f1)
	     (set! ok1 (+ ok1 (vct-ref f1 1)))
	     (clm-print ";or3 b~%"))
	 (if (or i1 i2)
	     (set! oki (+ oki (vector-ref i2 0)))
	     (clm-print ";or3 d~%"))
	 (if (not (or i2 i1))
	     (clm-print ";or3 3~%"))
	 (if (and i2 i1)
	     (set! oki (+ oki (vector-ref i1 1)))
	     (clm-print ";or3 e~%")))))))

(define (or4)
  "(or4) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :end 10))
	(e2 (make-env '(0 1 1 0) :end 10))
  	(i1 (make-vector 3 32))
	(f1 (make-vct 3 3.14))
	(i2 (make-vector 3 3))
	(f2 (make-vector 3 1.5))
	(oki 0))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 1))
	 (if (or (and e1 e2)
		 (and f1 f2)
		 (and i1 i2))
	     (set! oki (1+ oki))
	     (clm-print ";or4 a~%"))
	 (if (and (or f1 f2)
		  (not (or i1 i2))
		  (or e1 e2))
	     (clm-print ";or4 1~%"))
	 (if f1
	     (if e1
		 (if (not e2)
		     (clm-print ";or4 2~%")
		     (set! oki (1+ oki)))
		 (clm-print ";or4 3~%"))
	     (clm-print ";or4 4~%")))))))


;;; --------------------------------------------------------------------------------
;;;
;;; instruments and note lists from the documentation


(if (not (provided? 'snd-dsp.scm)) (load "dsp.scm"))
(if (not (provided? 'snd-jcrev.scm)) (load "jcrev.scm"))

;;; ins in docs + note lists

;;; fm.html

(define (fmdoc-pm beg end freq amp mc-ratio index)
  (let ((carrier-phase 0.0)
        (carrier-phase-incr (hz->radians freq))
        (modulator-phase 0.0)
        (modulator-phase-incr (hz->radians (* freq mc-ratio))))
   (run
    (lambda ()
      (do ((i beg (1+ i)))
	  ((= i end))
	(let* ((modulation (* index (sin modulator-phase)))
	       (pm-val (* amp (sin (+ carrier-phase modulation))))) 
	       ; no integration in phase modulation
	  (set! carrier-phase (+ carrier-phase carrier-phase-incr))
	  (set! modulator-phase (+ modulator-phase modulator-phase-incr))
	  (outa i pm-val *output*)))))))

(define (fmdoc-fm beg end freq amp mc-ratio index)
  (let ((carrier-phase 0.0)
        (carrier-phase-incr (hz->radians freq))
        (modulator-phase (* .5 pi))
        (modulator-phase-incr (hz->radians (* freq mc-ratio)))
        (fm-index (hz->radians (* freq mc-ratio index)))) 
	; fix up fm index to take integration into account
  (run
   (lambda ()
     (do ((i beg (1+ i)))
	 ((= i end))
       (let ((modulation (* fm-index (sin modulator-phase))))
	 (set! carrier-phase (+ carrier-phase modulation)) 
	 ; here is the fm integration
	 (let ((fm-val (* amp (sin carrier-phase))))
	   (set! carrier-phase (+ carrier-phase carrier-phase-incr))
	   (set! modulator-phase (+ modulator-phase modulator-phase-incr))
	   (outb i fm-val *output*))))))))

(define* (fmdoc-fm-1 beg dur freq amp mc-ratio index :optional (index-env '(0 1 100 1)))
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (cr (make-oscil freq))
         (md (make-oscil (* freq mc-ratio)))
         (fm-index (hz->radians (* index mc-ratio freq)))
         (ampf (make-env index-env :scaler amp :duration dur)) 
         (indf (make-env index-env :scaler fm-index :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (outa i (* (env ampf)                  ; amplitude env
                    (oscil cr (* (env indf)     ; carrier + modulation env
                                 (oscil md))))  ; modulation
               *output*))))))

(define (fmdoc-fm-2 beg dur freq amp mc-ratio index carrier-phase mod-phase)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq carrier-phase))
	 (md (make-oscil (* freq mc-ratio) mod-phase))
	 (fm-index (hz->radians (* index mc-ratio freq))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (outa i (* amp (oscil cr (* fm-index (oscil md)))) *output*))))))

(define (fmdoc-fm-3 beg dur freq amp mc-ratio index car-phase mod-phase skew-func skew)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq car-phase))
	 (md (make-oscil (* freq mc-ratio) mod-phase))
	 (skewf (make-env skew-func :scaler (hz->radians (* skew mc-ratio freq)) :duration dur))
	 (fm-index (hz->radians (* index mc-ratio freq))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
        (outa i (* amp (oscil cr (* fm-index (oscil md (env skewf))))) *output*))))))

(define (fmdoc-fm-4 beg dur freq amp mc-ratio index cr0p cr1p md0p md1p)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr0 (make-oscil 0 cr0p))
	 (cr1 (make-oscil 0 cr1p))
	 (md0 (make-oscil (* freq mc-ratio) md0p))
	 (md1 (make-oscil (* freq mc-ratio) md1p))
	 (am0 (make-oscil freq 0))
	 (am1 (make-oscil freq (* .5 pi)))
	 (fm-index (hz->radians (* index mc-ratio freq))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (outa i (* amp (+ (* (oscil am0) (oscil cr0 (* fm-index (oscil md0))))
			   (* (oscil am1) (oscil cr1 (* fm-index (oscil md1))))))
	       *output*))))))

(define (fmdoc-fm-5 beg dur freq amp mc-ratios indexes carrier-phase mod-phases)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq carrier-phase))
         (n (length mc-ratios))
         (modulators (make-vector n))
         (fm-indices (make-vct n)))
    (do ((i 0 (1+ i)))
	((= i n))
      (vector-set! modulators i (make-oscil (* freq (list-ref mc-ratios i)) (list-ref mod-phases i)))
      (vct-set! fm-indices i (hz->radians (* freq (list-ref indexes i) (list-ref mc-ratios i)))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (let ((sum 0.0))
	   (do ((k 0 (1+ k)))
	       ((= k n))
	     (set! sum (+ sum (* (vct-ref fm-indices k) (oscil (vector-ref modulators k))))))
	   (outa i (* amp (oscil cr sum)) *output*)))))))

(define (fmdoc-violin beg dur frequency amplitude fm-index)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (frq-scl (hz->radians frequency))
         (maxdev (* frq-scl fm-index))
         (index1 (* maxdev (/ 5.0 (log frequency))))
         (index2 (* maxdev 3.0 (/ (- 8.5 (log frequency)) (+ 3.0 (/ frequency 1000)))))
         (index3 (* maxdev (/ 4.0 (sqrt frequency))))
         (carrier (make-oscil frequency))
         (fmosc1 (make-oscil frequency))
         (fmosc2 (make-oscil (* 3 frequency)))
         (fmosc3 (make-oscil (* 4 frequency)))
         (ampf  (make-env '(0 0 25 1 75 1 100 0) :scaler amplitude :duration dur))
         (indf1 (make-env '(0 1 25 .4 75 .6 100 0) :scaler index1 :duration dur))
         (indf2 (make-env '(0 1 25 .4 75 .6 100 0) :scaler index2 :duration dur))
         (indf3 (make-env '(0 1 25 .4 75 .6 100 0) :scaler index3 :duration dur))
         (pervib (make-triangle-wave 5 :amplitude (* .0025 frq-scl)))
         (ranvib (make-rand-interp 16 :amplitude (* .005 frq-scl))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (let ((vib (+ (triangle-wave pervib) (rand-interp ranvib))))
	   (outa i (* (env ampf)
		      (oscil carrier
			     (+ vib 
				(* (env indf1) (oscil fmosc1 vib))
				(* (env indf2) (oscil fmosc2 (* 3.0 vib)))
				(* (env indf3) (oscil fmosc3 (* 4.0 vib))))))
		 *output*)))))))

(define (fmdoc-cascade beg dur freq amp modrat modind casrat casind caspha)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq))
	 (md (make-oscil (* freq modrat)))
	 (ca (make-oscil (* freq casrat) caspha))
	 (fm-ind0 (hz->radians (* modind modrat freq)))
	 (fm-ind1 (hz->radians (* casind (/ casrat modrat) freq))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (outa i (* amp 
                    (oscil cr (* fm-ind0 
                                 (oscil md (* fm-ind1 
                                              (oscil ca)))))) 
               *output*))))))

(define (fmdoc-feedbk beg dur freq amp index)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (y 0.0)
	 (x-incr (hz->radians freq)))
    (run 
     (lambda ()
       (do ((i start (1+ i))
	    (x 0.0 (+ x x-incr)))
	   ((= i end))
	 (set! y (+ x (* index (sin y))))
	 (outa i (* amp (sin y)) *output*))))))

(define* (fmdoc-vox beg dur freq1 amp :optional (indexes '(.005 .01 .02)) (formant-amps '(.86 .13 .01)))
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (freq freq1)
	 (car-os (make-oscil 0))
         (evens (make-vector 3))
         (odds (make-vector 3))
         (amps (apply vct formant-amps))
         (ampf (make-env '(0 0 25 1 75 1 100 0) :scaler amp :duration dur))
         (frmfs (make-vector 3))
         (indices (apply vct indexes))
         (per-vib (make-triangle-wave 6 :amplitude (* freq .03)))
         (ran-vib (make-rand-interp 20 :amplitude (* freq .5 .02))))
    (do ((i 0 (1+ i)))
	((= i 3))
      (vector-set! evens i (make-oscil 0))
      (vector-set! odds i (make-oscil 0)))

    (vector-set! frmfs 0 (make-env '(0 520 100 490) :duration dur)) 
    (vector-set! frmfs 1 (make-env '(0 1190 100 1350) :duration dur)) 
    (vector-set! frmfs 2 (make-env '(0 2390 100 1690) :duration dur))

    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (let* ((frq (+ freq (triangle-wave per-vib) (rand-interp ran-vib)))
		(car (oscil car-os (hz->radians frq)))
		(sum 0.0))
	   (do ((k 0 (1+ k)))
	       ((= k 3))
	     (let* ((frm (env (vector-ref frmfs k)))
		    (frm0 (/ frm frq))
		    (frm-int (inexact->exact (floor frm0)))
		    (even-amp 0.0) (odd-amp 0.0) 
		    (even-freq 0.0) (odd-freq 0.0))
	       (if (even? frm-int)
		   (begin
		     (set! even-freq (hz->radians (* frm-int frq)))
		     (set! odd-freq (hz->radians (* (+ frm-int 1) frq)))
		     (set! odd-amp (- frm0 frm-int))
		     (set! even-amp (- 1.0 odd-amp)))
		   (begin
		     (set! odd-freq (hz->radians (* frm-int frq)))
		     (set! even-freq (hz->radians (* (+ frm-int 1) frq)))
		     (set! even-amp (- frm0 frm-int))
		     (set! odd-amp (- 1.0 even-amp))))
	       (set! sum (+ sum (+ (* (vct-ref amps k) 
				      (+ (* even-amp 
					    (oscil (vector-ref evens k) 
						   (+ even-freq (* (vct-ref indices k) car))))
					 (* odd-amp 
					    (oscil (vector-ref odds k) 
						   (+ odd-freq (* (vct-ref indices k) car)))))))))))
	   (outa i (* (env ampf) sum) *output*)))))))

;;; --------------------------------------------------------------------------------

;;; sndclm.html


(define (sndclmdoc-simp start end freq amp)
  (let ((os (make-oscil freq)))
    (do ((i start (1+ i))) 
        ((= i end))
      (outa i (* amp (oscil os)) *output*))))

(define (sndclmdoc-simp-1 beg dur freq amp)
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (do ((i start (1+ i))) 
        ((= i end))
      (outa i (* amp (oscil os)) *output*))))

(define (sndclmdoc-simp-2 beg dur freq amp)
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
      (lambda ()
        (do ((i start (1+ i))) 
            ((= i end))
          (outa i (* amp (oscil os)) *output*))))))

(definstrument (sndclmdoc-simp-3 beg dur freq amp)
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
      (lambda ()
        (do ((i start (1+ i))) 
            ((= i end))
          (outa i (* amp (oscil os)) *output*))))))

(define (sndclmdoc-telephone start telephone-number)
  (let ((touch-tab-1 '(0 697 697 697 770 770 770 852 852 852 941 941 941))
	(touch-tab-2 '(0 1209 1336 1477 1209 1336 1477 1209 1336 1477 1209 1336 1477)))
    (do ((i 0 (1+ i)))
	((= i (length telephone-number)))
      (let* ((num (list-ref telephone-number i))
	     (frq1 (list-ref touch-tab-1 num))
	     (frq2 (list-ref touch-tab-2 num)))
        (sndclmdoc-simp-3 (+ start (* i .4)) .3 frq1 .1)
        (sndclmdoc-simp-3 (+ start (* i .4)) .3 frq2 .1)))))

(definstrument (sndclmdoc-simp-4 beg dur freq amp envelope)
  (let* ((os (make-oscil freq))
         (amp-env (make-env envelope :duration dur :scaler amp))
	 (start (seconds->samples beg))
         (end (+ start (seconds->samples dur))))
    (run
      (lambda ()
        (do ((i start (1+ i))) 
            ((= i end))
          (outa i (* (env amp-env) (oscil os)) *output*))))))

(define (make-my-oscil frequency)       ; we want our own oscil!
  (vct 0.0 (hz->radians frequency)))    ; current phase and frequency-based phase increment

(define (my-oscil gen fm)               ; the corresponding generator
  (let ((result (sin (vct-ref gen 0)))) ; return sin(current-phase)
    (vct-set! gen 0 (+ (vct-ref gen 0)  ; increment current phase
                       (vct-ref gen 1)  ;    by frequency
                       fm))             ;    and FM
    result))                            ; return sine wave

(define (sndclmdoc-simp-5 start end freq amp frq-env)
  (let ((os (make-oscil freq)) 
        (frqe (make-env frq-env :dur (- end start) :scaler (hz->radians freq))))
    (do ((i start (1+ i))) 
        ((= i end))
      (outa i (* amp (oscil os (env frqe))) *output*))))

(definstrument (sndclmdoc-simple-fm beg dur freq amp mc-ratio index :optional amp-env index-env)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq))                     ; carrier
         (md (make-oscil (* freq mc-ratio)))        ; modulator
         (fm-index (hz->radians (* index mc-ratio freq)))
         (ampf (make-env (or amp-env '(0 0  .5 1  1 0)) :scaler amp :duration dur))
         (indf (make-env (or index-env '(0 0  .5 1  1 0)) :scaler fm-index :duration dur)))
    (run
      (lambda ()
        (do ((i start (1+ i)))
            ((= i end))
          (outa i (* (env ampf) (oscil cr (* (env indf) (oscil md)))) *output*))))))

(define (sndclmdoc-simple-add beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 20)))     ; we'll create a tone with 20 equal amplitude harmonics
    (do ((i 0 (1+ i)))               ;   use the 'f' button to check out the spectrum
	((= i 20))
      (vector-set! arr i (make-oscil (* (1+ i) freq))))
    (run
     (lambda ()
       (do ((i start (1+ i))) 
           ((= i end))
	 (let ((sum 0.0))
	   (do ((k 0 (1+ k)))
	       ((= k 20))
	     (set! sum (+ sum (oscil (vector-ref arr k)))))
	   (out-any i (* amp .05 sum) 0 *output*)))))))

(definstrument (sndclmdoc-mapenv beg dur frq amp en)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (osc (make-oscil frq))
	 (zv (make-env en 1.0 dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
           ((= i end))
         (let ((zval (env zv))) 
	   (outa i 
             (* amp 
                (sin (* 0.5 pi zval zval zval)) 
                (oscil osc)) 
             *output*)))))))

(definstrument (sndclmdoc-simple-table dur)
  (let ((tab (make-table-lookup :wave (partials->wave '(1 .5  2 .5)))))
    (do ((i 0 (1+ i))) ((= i dur))
      (outa i (* .3 (table-lookup tab)) *output*))))

(define (sndclmdoc-looper start dur sound freq amp)
  (let* ((beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur)))
	 (loop-data (mus-sound-loop-info sound)))
    (if (or (null? loop-data)
	    (<= (cadr loop-data) (car loop-data)))
	(throw 'no-loop-positions)
	(let* ((loop-start (car loop-data))
	       (loop-end (cadr loop-data))
	       (loop-length (1+ (- loop-end loop-start)))
	       (sound-section (file->array sound 0 loop-start loop-length (make-vct loop-length)))
	       (original-loop-duration (/ loop-length (mus-sound-srate sound)))
	       (tbl (make-table-lookup :frequency (/ freq original-loop-duration) :wave sound-section)))
	       ;; "freq" here is how fast we read (transpose) the sound -- 1.0 returns the original
	  (run
	   (lambda ()
	     (do ((i beg (1+ i)))
		 ((= i end))
	       (outa i (* amp (table-lookup tbl)) *output*))))))))

(definstrument (sndclmdoc-fm-table file start dur amp read-speed modulator-freq index-in-samples)
  (let* ((beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur)))
	 (table-length (mus-sound-frames file))
	 (tab (make-table-lookup :frequency (/ read-speed (mus-sound-duration file)) 
				 :wave (file->array file 0 0 table-length (make-vct table-length))))
	 (osc (make-oscil modulator-freq))
	 (index (/ (* (hz->radians modulator-freq) 2 pi index-in-samples) table-length)))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* amp (table-lookup tab (* index (oscil osc))))
	       *output*))))))

(definstrument (sndclmdoc-simp-6)
  (let ((wav (make-waveshape 
               :frequency 440 
               :partials '(1 .5  2 .3  3 .2))))
    (do ((i 0 (1+ i))) ((= i 10000))
      (outa i (waveshape wav) *output*))))

(definstrument (sndclmdoc-bigbird start duration frequency freqskew amplitude freq-env amp-env partials)
  (let* ((beg (seconds->samples start))
         (end (+ beg (seconds->samples duration)))
         (gls-env (make-env freq-env (hz->radians freqskew) duration))
         (polyos (make-polyshape frequency :coeffs (partials->polynomial partials)))
         (fil (make-one-pole .1 .9))
         (amp-env (make-env amp-env amplitude duration)))
    (run
      (lambda ()
        (do ((i beg (1+ i)))
            ((= i end))
          (outa i 
            (one-pole fil   ; for distance effects
              (* (env amp-env) 
                 (polyshape polyos 1.0 (env gls-env))))
            *output*))))))

(definstrument (sndclmdoc-pqw start dur spacing carrier partials)
  (let* ((spacing-cos (make-oscil spacing (/ pi 2.0)))
	 (spacing-sin (make-oscil spacing))
	 (carrier-cos (make-oscil carrier (/ pi 2.0)))
	 (carrier-sin (make-oscil carrier))
	 (sin-coeffs (partials->polynomial
                       partials mus-chebyshev-second-kind))
	 (cos-coeffs (partials->polynomial
                       partials mus-chebyshev-first-kind))
	 (beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur))))
    (run (lambda ()
     (do ((i beg (1+ i))) ((= i end))
       (let ((ax (oscil spacing-cos)))
	 (outa i (- (* (oscil carrier-sin) 
                       (oscil spacing-sin) 
		       (polynomial sin-coeffs ax))
		    (* (oscil carrier-cos) 
		       (polynomial cos-coeffs ax)))
	       *output*)))))))

(def-optkey-fun (sndclmdoc-make-band-limited-triangle-wave (frequency 440.0) (order 1))
  (let ((freqs '()))
    (do ((i 1 (1+ i))
	 (j 1 (+ j 2)))
	((> i order))
      (set! freqs (cons (/ 1.0 (* j j)) (cons j freqs))))
    (make-waveshape frequency :wave (partials->waveshape (reverse freqs)))))

(define* (sndclmdoc-band-limited-triangle-wave gen :optional (fm 0.0))
  (waveshape gen 1.0 fm))

(definstrument (sndclmdoc-bl-saw start dur frequency order)
  (let* ((norm (if (= order 1) 1.0     ; these peak amps were determined empirically
                 (if (= order 2) 1.3   ;   actual limit is supposed to be pi/2 (G&R 1.441)
                   (if (< order 9) 1.7 ;   but Gibbs phenomenon pushes it to 1.851
                     1.9))))           ;   if order>25, numerical troubles -- use table-lookup
         (freqs '()))
    (do ((i 1 (1+ i)))
	((> i order))
      (set! freqs (cons (/ 1.0 (* norm i)) (cons i freqs))))
    (let* ((ccos (make-oscil frequency (/ pi 2.0)))
	   (csin (make-oscil frequency))
	   (coeffs (partials->polynomial (reverse freqs) mus-chebyshev-second-kind))
	   (beg (seconds->samples start))
	   (end (+ beg (seconds->samples dur))))
      (run 
       (lambda ()
	 (do ((i beg (1+ i))) 
	     ((= i end))
	   (outa i (* (oscil csin) 
                      (polynomial coeffs (oscil ccos))) 
                 *output*)))))))

(define (sndclmdoc-tritri start dur freq amp index mcr)
  (let* ((beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur)))
	 (carrier (make-triangle-wave freq))
	 (modulator (make-triangle-wave (* mcr freq))))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* amp (triangle-wave carrier 
                          (* index (triangle-wave modulator)))) 
               *output*))))))

(define (sndclmdoc-simple-soc beg dur freq amp)
  (let* ((os (make-sum-of-cosines 10 freq 0.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (outa i (* amp (sum-of-cosines os)) 
               *output*))))))

(define* (sndclmdoc-make-sinc-train :optional (frequency 440.0) (width #f))
  (let ((range (or width (* pi (- (* 2 (inexact->exact (floor (/ (mus-srate) (* 2.2 frequency))))) 1)))))
    ;; 2.2 leaves a bit of space before srate/2, (* 3 pi) is the minimum width, normally
    (list (- (* range 0.5))
	  range
	  (/ (* range frequency) (mus-srate)))))
	
(define* (sndclmdoc-sinc-train gen :optional (fm 0.0))
  (let* ((ang (car gen))
	 (range (cadr gen))
	 (top (* 0.5 range))
	 (frq (caddr gen))
	 (val (if (= ang 0.0) 1.0 (/ (sin ang) ang)))
	 (new-ang (+ ang frq fm)))
    (if (> new-ang top)
	(list-set! gen 0 (- new-ang range))
	(list-set! gen 0 new-ang))
    val))

(definstrument (sndclmdoc-ss beg dur freq amp :optional (N 1) (a .5) (B-ratio 1.0) (frqf #f))
  (let* ((st (seconds->samples beg))
         (nd (+ st (seconds->samples dur)))
         (sgen (make-sine-summation :n N :a a :ratio B-ratio :frequency freq))
	 (frq-env (if frqf (make-env frqf :scaler (hz->radians freq) :duration dur) #f))
	 (spectr-env (if frqf (make-env frqf :duration dur) #f))
	 (amp-env (make-env '(0 0 1 1 2 1 3 0) :scaler amp :duration dur)))
    (run
     (lambda ()
       (do ((i st (1+ i))) 
	   ((= i nd))
	 (if spectr-env
	     (set! (mus-scaler sgen) (* a (exp (- (env spectr-env))))))
         (outa i (* (env amp-env)
		    (sine-summation sgen (if frq-env (env frq-env) 0.0)))
	       *output*))))))

(define (sndclmdoc-make-sum-of-odd-sines frequency n)
  (vct 0.0 (hz->radians frequency) (exact->inexact n)))

(define (sndclmdoc-sum-of-odd-sines gen fm)
  (let* ((angle (vct-ref gen 0))
	 (a2 (* angle 0.5))
	 (n (vct-ref gen 2))
	 (den (* n (sin a2)))
	 (result (if (< (abs den) 1.0e-9)
		     0.0
		     (/ (* (sin (* n a2)) 
			   (sin (* (1+ n) a2)))
			den))))
    (vct-set! gen 0 (+ (vct-ref gen 0) (vct-ref gen 1) fm))
    result))

(definstrument (sndclmdoc-shift-pitch beg dur file freq :optional (order 40))
  (let* ((st (seconds->samples beg))
         (nd (+ st (seconds->samples dur)))
	 (gen (make-ssb-am freq order))
	 (rd (make-readin file)))
    (run
     (lambda ()
       (do ((i st (1+ i))) 
	   ((= i nd))
	 (outa i (ssb-am gen (readin rd)) *output*))))))

(definstrument (sndclmdoc-repitch beg dur sound old-freq new-freq 
	         :optional (amp 1.0) (bands 10) (order 40) (bw 50.0))
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (pairs bands)
	 (ssbs (make-vector pairs))
	 (bands (make-vector pairs))
	 (factor (/ (- new-freq old-freq) old-freq))
	 (rd (make-readin sound)))
    (do ((i 1 (1+ i)))
	((> i pairs))
      (let* ((aff (* i old-freq))
	     (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(vector-set! ssbs (1- i) (make-ssb-am (* i factor old-freq)))
	(vector-set! bands (1- i) (make-bandpass (hz->radians (- aff bwf)) 
						 (hz->radians (+ aff bwf)) 
						 order))))
    (run
     (lambda ()
       (do ((i start (1+ i))) 
           ((= i end))
	 (let ((sum 0.0)
	       (y (readin rd)))
	   (do ((band 0 (1+ band)))
	       ((= band pairs))
	     (set! sum (+ sum (ssb-am (vector-ref ssbs band) 
				      (bandpass (vector-ref bands band) y)))))
	   (outa i (* amp sum) *output*)))))))

(definstrument (sndclmdoc-fofins beg dur frq amp vib f0 a0 f1 a1 f2 a2 :optional ve ae)
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (ampf (make-env :envelope (or ae (list 0 0 25 1 75 1 100 0)) :scaler amp :duration dur))
         (frq0 (hz->radians f0))
         (frq1 (hz->radians f1))
         (frq2 (hz->radians f2))
         (foflen (if (= (mus-srate) 22050) 100 200))
         (vibr (make-oscil :frequency 6))
	 (vibenv (make-env :envelope (or ve (list 0 1 100 1)) :scaler vib :duration dur))
         (win-freq (/ (* 2 pi) foflen))
         (foftab (make-vct foflen))
         (wt0 (make-wave-train :wave foftab :frequency frq)))
    (do ((i 0 (1+ i)))
        ((= i foflen))
      (set! (vct-ref foftab i) ;; this is not the pulse shape used by B&R
            (* (+ (* a0 (sin (* i frq0))) 
                  (* a1 (sin (* i frq1))) 
                  (* a2 (sin (* i frq2)))) 
               .5 (- 1.0 (cos (* i win-freq))))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
           ((= i end))
         (outa i (* (env ampf) (wave-train wt0 (* (env vibenv) (oscil vibr)))) *output*))))))

(definstrument (sndclmdoc-echo beg dur scaler secs file)
  (let ((del (make-delay (seconds->samples secs)))
	(rd (make-sample-reader 0 file)))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
           ((= i (+ beg dur)))
         (let ((inval (rd)))
  	   (outa i (+ inval (delay del (* scaler (+ (tap del) inval)))) *output*)))))))

(define* (sndclmdoc-make-moving-max :optional (size 128))
  (let ((gen (make-delay size)))
    (set! (mus-scaler gen) 0.0)
    gen))

(define (sndclmdoc-moving-max gen y)
  (let* ((absy (abs y))
         (mx (delay gen absy)))
    (if (>= absy (mus-scaler gen))
	(set! (mus-scaler gen) absy)
	(if (>= mx (mus-scaler gen))
	    (set! (mus-scaler gen) (vct-peak (mus-data gen)))))
    (mus-scaler gen)))

(definstrument (sndclmdoc-zc time dur freq amp length1 length2 feedback)
  (let* ((beg (seconds->samples time))
         (end (+ beg (seconds->samples dur)))
         (s (make-pulse-train :frequency freq))  ; some raspy input so we can hear the effect easily
         (d0 (make-comb :size length1 :max-size (max length1 length2) :scaler feedback))
         (aenv (make-env '(0 0 .1 1 .9 1 1 0) :scaler amp :duration dur))
         (zenv (make-env '(0 0 1 1) :scaler (- length2 length1) :base 12.0 :duration dur)))
    (run (lambda ()
      (do ((i beg (1+ i))) ((= i end))
        (outa i (* (env aenv) (comb d0 (pulse-train s) (env zenv))) *output*))))))

(define (sndclmdoc-fir+comb beg dur freq amp size)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (dly (make-comb :scaler .9 :size size)) 
	 (flt (make-fir-filter :order size :xcoeffs (mus-data dly))) ; comb delay line as FIR coeffs
	 (r (make-rand freq)))                                       ; feed comb with white noise
    (run 
     (lambda () 
       (do ((i start (1+ i))) 
	   ((= i end)) 
	 (outa i (* amp (fir-filter flt (comb dly (rand r)))) *output*))))))

(definstrument (sndclmdoc-simple-src start-time duration amp srt srt-env filename)
  (let* ((senv (make-env :envelope srt-env :duration duration))
         (beg (seconds->samples start-time))
         (end (+ beg (seconds->samples duration)))
         (src-gen (make-src :input (make-readin filename) :srate srt)))
    (run
      (lambda ()
        (do ((i beg (1+ i)))
            ((= i end))
          (outa i (* amp (src src-gen (env senv))) *output*))))))

(definstrument (sndclmdoc-srcer start-time duration amp srt fmamp fmfreq filename)
  (let* ((os (make-oscil :frequency fmfreq))
         (beg (seconds->samples start-time))
         (end (+ beg (seconds->samples duration)))
         (src-gen (make-src :input (make-readin filename) :srate srt)))
    (run
      (lambda ()
        (do ((i beg (1+ i)))
            ((= i end))
          (outa i (* amp (src src-gen (* fmamp (oscil os)))) *output*))))))

(definstrument (sndclmdoc-convins beg dur filter file :optional (size 128))
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (ff (make-convolve :input (make-readin file) :fft-size size :filter filter)))
    (run
      (lambda ()
        (do ((i start (1+ i)))
            ((= i end))
          (outa i (convolve ff) *output*))))))

(definstrument (sndclmdoc-granulate-sound file beg :optional dur (orig-beg 0.0) (exp-amt 1.0))
  (let* ((f-srate (mus-sound-srate file))
	 (f-start (inexact->exact (round (* f-srate orig-beg))))
         (f (make-readin file :start f-start))
	 (st (seconds->samples beg))
	 (new-dur (or dur (- (mus-sound-duration file) orig-beg)))
	 (exA (make-granulate :input f :expansion exp-amt))
	 (nd (+ st (seconds->samples new-dur))))
    (run
     (lambda ()
       (do ((i st (1+ i)))
           ((= i nd))
         (outa i (granulate exA) *output*))))))

(definstrument (sndclmdoc-grev beg dur exp-amt file file-beg)
  (let* ((exA (make-granulate :expansion exp-amt))
	 (fil (make-file->sample file))
	 (ctr file-beg))
    (run
      (lambda ()
       (do ((i beg (1+ i)))
           ((= i (+ beg dur)))
         (outa i (granulate exA
		   (lambda (dir)
		     (let ((inval (file->sample fil ctr 0)))
		       (if (> ctr 0) (set! ctr (1- ctr)))
		       inval)))
	        *output*))))))

(definstrument (sndclmdoc-simple-pvoc beg dur amp size file)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-phase-vocoder (make-readin file) :fft-size size)))
    (run
      (lambda ()
        (do ((i start (1+ i)))
            ((= i end))
          (outa i (* amp (phase-vocoder sr)) *output*))))))

(definstrument (sndclmdoc-asy beg dur freq amp index :optional (r 1.0) (ratio 1.0))
  (let* ((st (seconds->samples beg))
         (nd (+ st (seconds->samples dur)))
         (asyf (make-asymmetric-fm :r r :ratio ratio :frequency freq)))
    (do ((i st (1+ i))) 
        ((= i nd))
      (outa i (* amp (asymmetric-fm asyf index 0.0)) *output*))))

(define (sndclmdoc-simple-f2s beg dur amp file)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-file->sample file))
	 (ctr 0))
    (run
     (lambda ()
       (do ((i start (1+ i))) ((= i end))
	 (out-any i (* amp (file->sample fil ctr 0)) 0 *output*)
	 (set! ctr (1+ ctr)))))))

(definstrument (sndclmdoc-simple-ina beg dur amp file)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-file->sample file)))
    (run
      (lambda ()
        (do ((i start (1+ i)))
            ((= i end))
          (outa i 
             (* amp (in-any i 0 fil)) ; same as (ina i fil)
             *output*))))))

(definstrument (sndclmdoc-env-sound file beg :optional (amp 1.0) (amp-env '(0 1 100 1)))
  (let* ((st (seconds->samples beg))
         (dur (mus-sound-duration file))
         (rev-amount .01)
         (rdA (make-readin file))
         (ampf (make-env amp-env amp dur))
         (nd (+ st (seconds->samples dur))))
    (run
      (lambda ()
        (do ((i st (1+ i)))
	    ((= i nd))
          (let ((outval (* (env ampf) (readin rdA))))
  	    (outa i outval *output*)
	    (if *reverb* 
              (outa i (* outval rev-amount) *reverb*))))))))

(definstrument (sndclmdoc-space file onset duration :key (distance-env '(0 1 100 10)) (amplitude-env '(0 1 100 1))
		     (degree-env '(0 45 50 0 100 90)) (reverb-amount .05))
  (let* ((beg (seconds->samples onset))
	 (end (+ beg (seconds->samples duration)))
         (loc (make-locsig :degree 0 :distance 1 :reverb reverb-amount :output *output* :revout *reverb*))
         (rdA (make-readin :file file))
         (dist-env (make-env distance-env :duration duration))
         (amp-env (make-env amplitude-env :duration duration))
         (deg-env (make-env degree-env :scaler (/ 1.0 90.0) :duration duration))
         (dist-scaler 0.0))
    (run
      (lambda ()
        (do ((i beg (1+ i)))
            ((= i end))
          (let ((rdval (* (readin rdA) (env amp-env)))
	        (degval (env deg-env))
	        (distval (env dist-env)))
            (set! dist-scaler (/ 1.0 distval))
            (set! (locsig-ref loc 0) (* (- 1.0 degval) dist-scaler))
            (if (> (mus-channels *output*) 1)
                (set! (locsig-ref loc 1) (* degval dist-scaler)))
            (if *reverb* 
                (set! (locsig-reverb-ref loc 0) (* reverb-amount (sqrt dist-scaler))))
            (locsig loc i rdval)))))))

(define (sndclmdoc-simple-dloc beg dur freq amp)
  "(simple-dloc-4 beg dur freq amp) test instrument for dlocsig"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (loc (make-move-sound (list start end 4 0
	      	   		     (make-delay 12) 
				     (make-env '(0 0 10 1) :end dur)
				     #f
				     (make-vector 4 #f)
				     (vector (make-env '(0 0 1 1 2 0 3 0 4 0) :duration dur)
					     (make-env '(0 0 1 0 2 1 3 0 4 0) :duration dur)
					     (make-env '(0 0 1 0 2 0 3 1 4 0) :duration dur)
					     (make-env '(0 0 1 0 2 0 3 0 4 1) :duration dur))
				     #f
				     (vector 0 1 2 3))
			       *output*)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
         (move-sound loc i (* amp (oscil os))))))))

(definstrument (when? start-time duration start-freq end-freq grain-file)
  (let* ((beg (seconds->samples start-time))
	 (len (seconds->samples duration))
	 (end (+ beg len))
	 (grain-dur (mus-sound-duration grain-file))
	 (frqf (make-env '(0 0 1 1) :scaler (hz->radians (- end-freq start-freq)) :duration duration))
	 (click-track (make-pulse-train start-freq))
	 (grain-size (seconds->samples grain-dur))
	 (grains (make-wave-train :size grain-size :frequency start-freq))
	 (ampf (make-env '(0 1 1 0) :scaler .7 :offset .3 :duration duration :base 3.0))
	 (grain (mus-data grains)))
    (file->array grain-file 0 0 grain-size grain)
    (let ((original-grain (vct-copy grain)))
      (run
       (lambda ()
	 (do ((i beg (1+ i)))
	     ((= i end))
	   (let* ((gliss (env frqf)))
	     (outa i (* (env ampf) (wave-train grains gliss)) *output*)
	     (let ((click (pulse-train click-track gliss)))
	       (if (> click 0.0)
		   (let* ((scaler (max 0.1 (exact->inexact (/ (- i beg) len))))
			  (comb-len 32)
			  (c1 (make-comb scaler comb-len))
			  (c2 (make-comb scaler (inexact->exact (floor (* comb-len .75)))))
			  (c3 (make-comb scaler (inexact->exact (floor (* comb-len 1.25))))))
		     (do ((k 0 (1+ k)))
			 ((= k grain-size))
		       (let ((x (vct-ref original-grain k)))
			 (vct-set! grain k (+ (comb c1 x) (comb c2 x) (comb c3 x)))))))))))))))

(definstrument (move-formants start file amp radius move-env num-formants)
  (let* ((frms (make-vector num-formants))
	 (beg (seconds->samples start))
	 (dur (mus-sound-frames file))
	 (end (+ beg dur))
	 (rd (make-readin file))
	 (menv (make-env move-env :end dur)))
    (let ((start-frq (env menv)))
      (do ((i 0 (1+ i)))
	  ((= i num-formants))
	(vector-set! frms i (make-formant radius (* (+ i 1) start-frq)))))
    (run
     (lambda ()
       (do ((k beg (1+ k)))
	   ((= k end))
	 (let ((sum 0.0)
	       (x (readin rd))
	       (frq (env menv)))
	   (do ((i 0 (1+ i)))
	       ((= i num-formants))
	     (set! sum (+ sum (formant (vector-ref frms i) x)))
	     (let ((curfrq (* (+ i 1) frq)))
	       (if (< (* 2 curfrq) (mus-srate))
		   (set! (mus-frequency (vector-ref frms i)) curfrq))))
	   (outa k (* amp sum) *output*)))))))

(define (test-filter flt)
  (let* ((osc (make-oscil 0.0))
	 (samps (seconds->samples 0.5))
	 (ramp (make-env '(0 0 1 1) :scaler (hz->radians samps) :end samps)))
    (with-sound ()
      (do ((i 0 (1+ i)))
	  ((= i samps))
        (outa i (flt (oscil osc (env ramp))) *output*)))))

(definstrument (flux start-time file frequency combs0 combs1 :optional (scaler 0.99) (comb-len 32))
  (let* ((beg (seconds->samples start-time))
	 (len (mus-sound-frames file))
	 (end (+ beg len))
	 (num-combs0 (length combs0))
	 (num-combs1 (length combs1))
	 (cmbs0 (make-vector num-combs0))
	 (cmbs1 (make-vector num-combs1))
	 (osc (make-oscil frequency))
	 (rd (make-readin file)))
    (do ((k 0 (1+ k)))
	((= k num-combs0))
      (vector-set! cmbs0 k 
		   (make-comb scaler 
			      (inexact->exact (floor (* comb-len (list-ref combs0 k)))))))
    (do ((k 0 (1+ k)))
	((= k num-combs1))
      (vector-set! cmbs1 k 
		   (make-comb scaler 
			      (inexact->exact (floor (* comb-len (list-ref combs1 k)))))))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (let* ((interp (oscil osc))
		(sum0 0.0)
		(sum1 0.0)
		(x (readin rd)))
	   (do ((k 0 (1+ k)))
	       ((= k num-combs0))
	     (set! sum0 (+ sum0 (comb (vector-ref cmbs0 k) x))))
	   (do ((k 0 (1+ k)))
	       ((= k num-combs1))
	     (set! sum1 (+ sum1 (comb (vector-ref cmbs1 k) x))))
	   (outa i (+ (* interp sum0) (* (- 1.0 interp) sum1)) *output*)))))))




;;; --------------------------------------------------------------------------------


(define (test-documentation-instruments)

  (with-sound (:channels 2) 
	      (fmdoc-pm 0 10000 1000 .25 0.5 4)
	      (fmdoc-fm 0 10000 1000 .25 0.5 4))
  (with-sound () (fmdoc-fm-1 0 1.0 100 .5 1.0 4.0))
  (with-sound () (fmdoc-fm-1 0 1.0 400 .5 0.25 4.0))
  (with-sound () (fmdoc-fm-1 0 1.0 400 .5 1.1414 4.0))
  (with-sound () (fmdoc-fm-1 0 0.5 400 .5 1.0 5.0 '(0 0 20 1 40 .6 90 .5 100 0)))
  (with-sound () (fmdoc-fm-1 0 1.0 900 .5 1/3 2.0 '(0 0 6 .5 10 1 90 1 100 0)))
  (with-sound () (fmdoc-fm-1 0 1.0 500 .5 .2 1.5 '(0 0 6 .5 10 1 90 1 100 0)))
  (with-sound () (fmdoc-fm-1 0 1.0 900 .5 2/3 2 '(0 0 25 1 75 1 100 0)))
  (with-sound () (fmdoc-fm-2 0 1.0 100 .25 1.0 4 0 (* .5 pi)))
  (with-sound () (fmdoc-fm-2 0 1.0 100 .25 1.0 4.0 (* .5 pi) (* .5 pi)))
  (with-sound () (fmdoc-fm-3 0 2.0 100 .25 1.0 4.0 0 0 '(0 0 50 1 100 0) .02))
  (with-sound () (fmdoc-fm-4 0 1.0 1000 .25 .1 1.0 0 (* .5 pi) (* .5 pi) 0))
  (with-sound () (fmdoc-fm-5 0 2.0 440 .3 '(1 3 4) '(1.0 0.5 0.1) 0.0 '(0.0 0.0 0.0)))
  (with-sound () (fmdoc-violin 0 1.0 440 .1 2.5))
  (with-sound () 
	      (fmdoc-cascade 0 1.0 400 .25 1.0 1.0 1.0 1.0 0)
	      (fmdoc-cascade 1.5 1.0 400 .25 1.0 1.0 1.0 1.0 (* .5 pi)))
  (with-sound () (fmdoc-feedbk 0 1 100.0 1.0 1.0))
  (with-sound () 
	      (fmdoc-vox 0 1.0 220.0 0.5)
	      (fmdoc-vox 1.5 1.0 110 .5 '(0.02 0.01 0.02) '(.9 .09 .01)))
  (with-sound (:play #t) (sndclmdoc-simp 0 22050 330 .1))
  (with-sound (:srate 44100) (sndclmdoc-simp-1 0 1.0 440.0 0.1))
  (with-sound () (sndclmdoc-telephone 0.0 '(7 2 3 4 9 7 1)))
  (with-sound () (sndclmdoc-simp-4 0 2 440 .1 '(0 0  0.1 1.0  1.0 0.0)))
  (with-sound () 
	      (let ((sqr (make-square-wave 100))) ; test a square-wave generator
		(do ((i 0 (1+ i))) 
		    ((= i 10000)) 
		  (outa i (square-wave sqr) *output*))))
  (with-sound () 
	      (run 
	       (lambda () 
		 (let ((osc (make-my-oscil 440.0)))
		   (do ((i 0 (1+ i))) 
		       ((= i 22050))
		     (outa i (my-oscil osc 0.0) *output*))))))
  (with-sound () (sndclmdoc-simp-5 0 10000 440 .1 '(0 0 1 1))) ; sweep up an octave
  (with-sound () (sndclmdoc-simple-fm 0 1 440 .1 2 1.0))
  (with-sound () (sndclmdoc-simple-add 0 1 220 .3))
  (with-sound () 
	      (sndclmdoc-mapenv 0 1 440 .4 '(0 0 50 1 75 0 86 .5 100 0)))
  (with-sound (:srate 44100) (sndclmdoc-looper 0 10 "/home/bil/sf1/forest.aiff" 1.0 0.5))
  (with-sound ()
	      (sndclmdoc-bigbird 0 .05 1800 1800 .2
				 '(.00 .00 .40 1.00 .60 1.00 1.00 .0)         ; freq env
				 '(.00 .00 .25 1.00 .60 .70 .75 1.00 1.00 .0) ; amp env
				 '(1 .5 2 1 3 .5 4 .1 5 .01)))                ; bird song spectrum
  (with-sound () (sndclmdoc-pqw 0 1 200.0 1000.0 '(2 .2  3 .3  6 .5)))
  (with-sound (:srate 44100) (sndclmdoc-tritri 0 1 1000.0 0.5 0.1 0.01)) ; sci-fi laser gun
  (with-sound (:srate 44100) (sndclmdoc-tritri 0 1 4000.0 0.7 0.1 0.01)) ; a sparrow?
  (with-sound () (sndclmdoc-simple-soc 0 1 100 1.0))
  (with-sound () (sndclmdoc-ss 0 1 400.0 1.0 5 0.5 1.0 '(0 0 1 2)))
  (with-sound () (sndclmdoc-shift-pitch 0 3 "oboe.snd" 1108.0))
  (let* ((sound "oboe.snd")
	 (mx (cadr (mus-sound-maxamp sound)))
	 (dur (mus-sound-duration sound)))
    (with-sound (:scaled-to mx) 
		(sndclmdoc-repitch 0 dur sound 554 1000)))
  (with-sound () (sndclmdoc-fofins 0 1 270 .2 .001 730 .6 1090 .3 2440 .1)) ; "Ahh"
  (with-sound () ; one of JC's favorite demos
	      (sndclmdoc-fofins 0 4 270 .2 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
				'(0 0 .5 1 3 .5 10 .2 20 .1 50 .1 60 .2 85 1 100 0))
	      (sndclmdoc-fofins 0 4 (* 6/5 540) .2 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
				'(0 0 .5 .5 3 .25 6 .1 10 .1 50 .1 60 .2 85 1 100 0))
	      (sndclmdoc-fofins 0 4 135 .2 0.005 730 .6 1090 .3 2440 .1 '(0 0 40 0 75 .2 100 1) 
				'(0 0 1 3 3 1 6 .2 10 .1 50 .1 60 .2 85 1 100 0)))
  (with-sound () (sndclmdoc-echo 0 60000 .5 1.0 "pistol.snd"))
  (with-sound () 
	      (sndclmdoc-zc 0 3 100 .1 20 100 .5) 
	      (sndclmdoc-zc 3.5 3 100 .1 90 100 .95))
  (with-sound () 
	      (sndclmdoc-fir+comb 0 2 10000 .001 200)
	      (sndclmdoc-fir+comb 2 2 1000 .0005 400)
	      (sndclmdoc-fir+comb 4 2 3000 .001 300)
	      (sndclmdoc-fir+comb 6 2 3000 .0005 1000))
  (with-sound () (sndclmdoc-simple-src 0 4 1.0 0.5 '(0 1 1 2) "oboe.snd"))
  (with-sound () (sndclmdoc-srcer 0 2 1.0   1 .3 20 "fyow.snd"))   
  (with-sound () (sndclmdoc-srcer 0 25 10.0   .01 1 10 "fyow.snd"))
  (with-sound () (sndclmdoc-srcer 0 2 1.0   .9 .05 60 "oboe.snd")) 
  (with-sound () (sndclmdoc-srcer 0 2 1.0   1.0 .5 124 "oboe.snd"))
  (with-sound () (sndclmdoc-srcer 0 10 10.0   .01 .2 8 "oboe.snd"))
  (with-sound () (sndclmdoc-srcer 0 2 1.0   1 3 20 "oboe.snd"))    
  (with-sound () 
	      (sndclmdoc-convins 0 2 (vct 1.0 0.5 0.25 0.125) "oboe.snd")) ; same as fir-filter with those coeffs
  (with-sound () (sndclmdoc-granulate-sound "now.snd" 0 3.0 0 2.0))
  (with-sound () (sndclmdoc-grev 0 100000 2.0 "pistol.snd" 40000))
  (with-sound () (sndclmdoc-simple-pvoc 0 2.0 1.0 512 "oboe.snd"))
  (with-sound () (sndclmdoc-simple-ina 0 1 .5 "oboe.snd"))
  (with-sound () (sndclmdoc-env-sound "oboe.snd" 0 1.0 '(0 0 1 1 2 1 3 0)))
  (with-sound (:reverb jc-reverb :channels 2) 
	      (sndclmdoc-space "pistol.snd" 0 3 :distance-env '(0 1 1 2) :degree-env '(0 0 1 90)))
  (with-sound (:channels 4)
	      (let ((loc (make-locsig :channels 4 :output *output*))
		    (osc (make-oscil 440.0))
		    (j 0))
		(run  ; 360 notes one at each degree in a circle
		 (lambda ()
		   (do ((i 0 (1+ i)))
		       ((= i 360))
		     (do ((k 0 (1+ k)))
			 ((= k 1000))
		       (let ((sig (* .5 (oscil osc))))
			 (locsig loc j sig)
			 (set! j (1+ j))))
		     (move-locsig loc (exact->inexact i) 1.0))))))
  (with-sound (:channels 4) (sndclmdoc-simple-dloc 0 2 440 .5))
  (with-sound () (when? 0 4 2.0 8.0 "1a.snd"))
  (with-sound () 
	      (move-formants 0 "oboe.snd" 2.0 0.99 '(0 1200 1.6 2400 2 1400) 4))
  (test-filter (make-one-zero 0.5 0.5))
  (test-filter (make-one-pole 0.1 -0.9))
  (test-filter (make-two-pole 0.1 0.1 0.9))
  (test-filter (make-two-zero 0.5 0.2 0.3))

  (with-sound (:scaled-to .5) 
	      (flux 0 "oboe.snd" 10.0 '(1.0 1.25 1.5) '(1.0 1.333 1.6))
	      (flux 2 "now.snd" 4.0 '(1.0 1.25 1.5) '(1.0 1.333 1.6 2.0 3.0))
	      (flux 4 "now.snd" 1.0 '(1.0 1.25 1.5) '(1.0 1.333 1.6 2.0 3.0) 0.995 20)
	      (flux 6 "now.snd" 10.0 '(1.0 1.25 1.5) '(1.0 1.333 1.6 2.0 3.0) 0.99 10)
	      (flux 8 "now.snd" 10.0 '(2.0) '(1.0 1.333 1.6 2.0 3.0) 0.99 120)
	      (flux 10 "fyow.snd" .50 '(1.0 2.0 1.5) '(1.0 1.333 1.6 2.0 3.0) 0.99 120))

  (for-each close-sound (sounds))
  )



