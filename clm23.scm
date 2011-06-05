;;; these are CLM test instruments

(provide 'snd-clm23.scm)
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
(if (not (provided? 'snd-dsp.scm)) (load "dsp.scm"))


;;; definstrument -> define (+ change open paren placement)
;;; *srate* -> (mus-srate)
;;; run loop ... -> run (do... + extra end close paren
;;; aref -> 
;;; setf -> set!
;;; remove declare (or change order of args and remove ":")
;;;   however in granulate run-time edit-func, the "(declare (g clm))" is necessary
;;; double not needed
;;; array of gen -> vector (and setf aref to vector-set! in this case)
;;; nil -> #f, t -> #t
;;; incf, decf 
;;; length sometimes length, vct-length etc
;;; make-filter in scm requires coeffs arrays
;;; &optional ->, &key too (using define*)
;;; two-pi -> (* 2 pi)
;;; make-empty-frame is make-frame essentially
;;; open-input and close-input -> make-readin or use name directly (in make-readin)
;;; make-locsig channel arg is in a different place
;;; progn -> begin, when -> if+begin (prog1 prog2), dotimes
;;; string= -> string=? (also string-equal)
;;; integerp -> integer? and others like it (null -> null?)
;;; sound-duration -> mus-sound-duration and similarly for others
;;; various array info procs like array-dimension
;;; #'(lambda ...) to just (lambda...)
;;; nth -> list-ref
;;; loop -> do

(define (clm23-sine-bank amps phases len)
  (let ((sum 0.0))
    (do ((i 0 (+ 1 i)))
	((= i len))
      (set! sum (+ sum (* (amps i)
			  (sin (phases i))))))
    sum))


(define* (make-double-array len initial-contents initial-element)
  "(make-double-array len initial-contents initial-element) is for CL/Scheme compatibility; it makes a vct"
  (let ((v (make-vct len (or initial-element 0.0))))
    (if initial-contents
	(let ((clen (min len (length initial-contents))))
	  (do ((i 0 (+ i 1)))
	      ((= i clen))
	    (set! (v i) (initial-contents i)))))
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
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (oscil os)) 0)))))

(definstrument (simple-fm beg dur freq amp mc-ratio index amp-env index-env)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq))                     ; our carrier
         (md (make-oscil (* freq mc-ratio)))        ; our modulator
         (fm-index (hz->radians (* index mc-ratio freq)))
         (ampf (make-env (or amp-env '(0 0 .5 1 1 0)) :scaler amp :duration dur))
         (indf (make-env (or index-env '(0 0 .5 1 1 0)) :scaler fm-index :duration dur)))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* (env ampf) (oscil cr (* (env indf) (oscil md)))))))))

(define (simple-outn beg dur freq ampa ampb ampc ampd reva revb)
  "(simple-outn beg dur freq ampa ampb ampc ampd reva revb) test instrument for outn"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (let ((val (oscil os)))
	 (if (> ampa 0.0) (outa i (* ampa val)))
	 (if (> ampb 0.0) (outb i (* ampb val)))
	 (if (> ampc 0.0) (outc i (* ampc val)))
	 (if (> ampd 0.0) (outd i (* ampd val)))
	 (if (> reva 0.0) (outa i (* reva val) *reverb*))
	 (if (> revb 0.0) (outb i (* revb val) *reverb*)))))))

(define (simple-ssb beg dur freq amp)
  "(simple-ssb beg dur freq amp) test instrument for ssb-am"
  (let* ((os (make-ssb-am freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 3)))
    (set! (arr 0) os)
    (set! (arr 1) #f)
    (set! (arr 2) (make-ssb-am 660 40))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (let ((sum 0.0))
	 (do ((i 0 (+ i 1)))
	     ((= i (length arr)))
	   (if (ssb-am? (arr i))
	       (set! sum (+ sum (ssb-am (arr i) 1.0)))))
	 (out-any i (* amp sum) 0))))))

(define (simple-multiarr beg dur freq amp)
  "(simple-multiarr beg dur freq amp) test instrument for array of gen"
  ;; this won't work in CL because that version of CLM assumes all aref gens are the same type
  (let* ((start (seconds->samples beg))
	 (len (seconds->samples dur))
	 (end (+ start len))
	 (arr (make-vector 3)))
    (set! (arr 0) (make-oscil freq))
    (set! (arr 1) (make-env '(0 0 1 1) :scaler amp :duration dur))
    (set! (arr 2) (make-oscil (* freq 2)))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (out-any i (* (env (arr 1))
		     (oscil (arr 0)
			    (* .1 (oscil (arr 2)))))
		0)))))

(define (simple-nsin beg dur amp)
  "(simple-nsin beg dur amp) test instrument for nsin"
  (let* ((os (make-nsin 440 10))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (nsin os)) 0)))))

(define (simple-ncos beg dur freq amp)
  "(simple-ncos beg dur freq amp) test instrument for ncos"
  (let* ((os (make-ncos freq 10))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (ncos os)) 0)))))

(define (simple-nrxysin beg dur amp)
  "(simple-nrxysin beg dur amp) test instrument for nrxysin"
  (let* ((os (make-nrxysin 440 1.0 10))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (nrxysin os)) 0)))))

(define (simple-nrxycos beg dur freq amp)
  "(simple-nrxycos beg dur freq amp) test instrument for nrxycos"
  (let* ((os (make-nrxycos freq 1.0 10))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (nrxycos os)) 0)))))

(define (simple-osc beg dur freq amp)
  "(simple-osc beg dur freq amp) test instrument for oscil"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 20)))
    (do ((i 0 (+ i 1)))
	((= i 20))
      (set! (arr i) (make-oscil (* (+ i 1) 100))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (let ((sum 0.0))
	 (do ((i 0 (+ i 1)))
	     ((= i (length arr)))
	   (if (oscil? (arr i))
	       (set! sum (+ sum (oscil (arr i))))))
	 (out-any i (* amp .05 sum) 0))))))

(define (simple-asy beg dur amp)
  "(simple-asy beg dur amp) test instrument for asymmetric-fm"
  (let* ((os (make-asymmetric-fm 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (asymmetric-fm os 1.0)) 0)))))

(define (simple-saw beg dur amp)
  "(simple-saw beg dur amp) test instrument for sawtooth-wave"
  (let* ((os (make-sawtooth-wave 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (sawtooth-wave os)) 0)))))

(define (simple-sqr beg dur amp)
  "(simple-sqr beg dur amp) test instrument for square-wave"
  (let* ((os (make-square-wave 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (square-wave os)) 0)))))

(define (simple-tri beg dur amp)
  "(simple-tri beg dur amp) test instrument for triangle-wave"
  (let* ((os (make-triangle-wave 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (triangle-wave os)) 0)))))

(define (simple-pul beg dur amp)
  "(simple-pul beg dur amp) test instrument for pusle-train"
  (let* ((os (make-pulse-train 440.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (pulse-train os)) 0)))))

(define (simple-sib beg dur freq amp)
  "(simple-sib beg dur freq amp) test instrument for sine-bank"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (amps (make-double-array 3 :initial-element 0.0))
	 (phases (make-double-array 3 :initial-element 0.0))
	 (freqs (make-double-array 3 :initial-element 0.0)))
    (do ((i 0 (+ i 1)))
	((= i 3))
      (set! (freqs i) (double (* freq (+ i 1))))
      (set! (amps i) (double (/ amp (+ i 2)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (do ((i 0 (+ i 1)))
	   ((= i (vct-length phases)))
	 (set! (phases i) (+ (phases i) (hz->radians (freqs i)))))
       (out-any i (clm23-sine-bank amps phases 3) 0)))))

(define (simple-oz beg dur freq amp)
  "(simple-oz beg dur freq amp) test instrument for one-zero"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-one-zero 0.4 0.6)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (one-zero oz (oscil os))) 0)))))

(define (simple-op beg dur freq amp)
  "(simple-op beg dur freq amp) test instrument for one-pole"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-one-pole 0.4 -0.6)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (one-pole oz (oscil os))) 0)))))

(define (simple-tz beg dur freq amp)
  "(simple-tz beg dur freq amp) test instrument for two-zero"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-two-zero 0.4 0.3 0.3)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (two-zero oz (oscil os))) 0)))))

(define (simple-tp beg dur freq amp)
  "(simple-tp beg dur freq amp) test instrument for two-pole"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-two-pole 0.3 -0.6 0.1)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (two-pole oz (oscil os))) 0)))))

(define (simple-frm beg dur freq amp)
  "(simple-frm beg dur freq amp) test instrument for formant"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-formant 1200.0 0.95)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (formant oz (oscil os))) 0)))))

(define (simple-firm beg dur freq amp)
  "(simple-frm beg dur freq amp) test instrument for firmant"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (oz (make-firmant 1200.0 0.95)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (firmant oz (oscil os))) 0)))))

(define (simple-firm2 beg dur freq amp)
  "(simple-frm beg dur freq amp) test instrument for firmant"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (frqf (make-env '(0 1200 1 2400) :scaler (hz->radians 1.0) :duration dur))
	 (oz (make-firmant 1200.0 0.95)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (firmant oz (oscil os) (env frqf))) 0)))))

;(define w1 (make-polyshape :frequency 100.0 
;			   :partials (let ((frqs '()))
;				       (do ((i 1 (+ i 1)))
;					   ((= i 10) (begin (format #t frqs) (reverse frqs)))
;					 (set! frqs (cons (/ 1.0 (* i i)) (cons i frqs)))))))

(define (simple-poly beg dur freq amp)
  "(simple-poly beg dur freq amp) test instrument for polyshape"
  (let* ((w1 (make-polyshape :frequency freq :partials '(1 1 2 1 3 1)))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (polyshape w1 1.0)) 0)))))

(define (simple-polyw beg dur freq amp)
  "(simple-poly beg dur freq amp) test instrument for polywave"
  (let* ((w1 (make-polywave :frequency freq :partials '(1 1 2 1 3 1)))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (polywave w1)) 0)))))

(define (simple-dly beg dur freq amp)
  "(simple-dly beg dur freq amp) test instrument for delay"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-delay 100)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (delay buf (* amp (oscil os))) 0)))))

(define (simple-cmb beg dur freq amp)
  "(simple-cmb beg dur freq amp) test instrument for comb"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-comb .1 100)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (comb buf (* amp (oscil os))) 0)))))

(define (simple-filtered-cmb beg dur freq amp)
  "(simple-filtered-cmb beg dur freq amp) test instrument for filtered-comb"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-filtered-comb .1 100 :filter (make-one-zero .5 .5))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (filtered-comb buf (* amp (oscil os))) 0)))))

(define (simple-not beg dur freq amp)
  "(simple-not beg dur freq amp) test instrument for notch"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-notch .1 100)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (notch buf (* amp (oscil os))) 0)))))

(define (simple-alp beg dur freq amp)
  "(simple-alp beg dur freq amp) test instrument for all-pass"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-all-pass .2 .8 100)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (all-pass buf (* amp (oscil os))) 0)))))

(define (simple-ave beg dur freq amp)
  "(simple-ave beg dur freq amp) test instrument for moving-average"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (buf (make-moving-average 10)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (moving-average buf (* amp (oscil os))) 0)))))

(define (simple-tab beg dur freq amp)
  "(simple-tab beg dur freq amp) test instrument for table-lookup"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (table-size 256)
	 (buf (make-table-lookup freq 0.0 :size table-size))
	 (table (mus-data buf)))
    (do ((i 0 (+ i 1)))
	((= i table-size))
      (set! (table i) (double (/ i table-size))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (table-lookup buf)) 0)))))

(define (simple-flt beg dur freq amp)
  "(simple-flt beg dur freq amp) test instrument for filter"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (+ i 1)))
	((= i 8))
      (set! ((mus-xcoeffs flt) i) (double (/ i 16)))
      (set! ((mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (filter flt (oscil os))) 0)))))

(define (simple-fir beg dur freq amp)
  "(simple-fir beg dur freq amp) test instrument for fir-filter"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-fir-filter 8 :xcoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (+ i 1)))
	((= i 8))
      (set! ((mus-xcoeffs flt) i) (double (/ i 16))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (fir-filter flt (oscil os))) 0)))))

(define (simple-iir beg dur freq amp)
  "(simple-iir beg dur freq amp) test instrument for iir-filter"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-iir-filter 8 :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (+ i 1)))
	((= i 8))
      (set! ((mus-ycoeffs flt) i) (double (/ i 16))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (iir-filter flt (oscil os))) 0)))))

(define (simple-f beg dur freq amp)
  "(simple-f beg dur freq amp) test instrument for frame->sample"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (frm (make-frame 2 .7 .3))
	 (smp (make-frame 2))
	 (os (make-oscil freq)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (let ((val (oscil os)))
	 (frame-set! smp 0 val)
	 (set! (smp 1) val)
	 (out-any i (* amp (frame->sample frm smp)) 0))))))

(define (simple-ran beg dur freq amp)
  "(simple-ran beg dur freq amp) test instrument for rand"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand freq)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (rand os)) 0)))))

(define (simple-ri beg dur freq amp)
  "(simple-ri beg dur freq amp) test instrument for rand-interp"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand-interp freq)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (rand-interp os)) 0)))))

(define (simple-rndist beg dur freq amp)
  "(simple-rndist beg dur freq amp) test instrument for rand dist"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand freq :distribution (inverse-integrate '(0 0 1 1)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (rand os)) 0)))))

(define (simple-ridist beg dur freq amp)
  "(simple-ridist beg dur freq amp) test instrument for rand-interp dist"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-rand-interp freq :distribution (inverse-integrate '(0 1 1 0)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (rand-interp os)) 0)))))

(define (simple-env beg dur freq amp)
  "(simple-env beg dur freq amp) test instrument for env"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (e (make-env '(0 0 1 1 2 1 3 0) :scaler amp :offset .1 :duration dur)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* (env e) (oscil os)) 0)))))

(define* (simple-fof beg dur frq amp vib f0 a0 f1 a1 f2 a2 ve ae)
  " (simple-fof beg dur frq amp vib f0 a0 f1 a1 f2 a2 ve ae) test instrument for FOF"
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
    (do ((i 0 (+ i 1))) ((= i foflen))
      (set! (foftab i) (double
				;; this is not the pulse shape used by B&R
				(* (+ (* a0 (sin (* i frq0))) 
				      (* a1 (sin (* i frq1))) 
				      (* a2 (sin (* i frq2)))) 
				   .5 (- 1.0 (cos (* i win-freq)))))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* (env ampf) (wave-train wt0 (* (env vibenv) (oscil vibr)))) 0)))))

(define (simple-amb beg dur freq amp)
  "(simple-amb beg dur freq amp) test instrument for osc?+rand"
  (let* ((os (if (> freq 1) (make-oscil freq) (make-rand freq)))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (if (oscil? os) (oscil os) (rand os))) 0)))))

(define (simple-rd beg dur amp file)
  "(simple-rd beg dur amp file) test instrument for readin"
  (let* ((rd (make-readin file))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (readin rd)) 0)))))

(define (simple-rd-start beg dur amp file channel start)
  "(simple-rd-start beg dur amp file channel start) test instrument for readin"
  (let* ((rd (make-readin file :channel channel :start start))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (readin rd)) 0)))))

(define (simple-cnv beg dur amp file)
  "(simple-cnv beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (filt (make-double-array 8)))
    (do ((i 0 (+ i 1))) ((= i 8)) (set! (filt i) (double 0.0)))
    (set! (filt 4) (double 1.0))
    (let ((ff (make-convolve :input (make-readin file) :filter filt)))
      (run
       (do ((i start (+ i 1))) ((= i end))
	 (out-any i (* amp (convolve ff)) 0))))))

(define (simple-cnf beg dur amp file)
  "(simple-cnf beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (filt (make-double-array 8)))
    (do ((i 0 (+ i 1))) ((= i 8)) (set! (filt i) (double 0.0)))
    (set! (filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (do ((i start (+ i 1))) ((= i end))
	 (out-any i (* amp (convolve ff (lambda (dir) (readin rd)))) 
		  0))))))

(define (simple-lrg beg dur amp file)
  "(simple-lrg beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (filt (make-double-array 8)))
    (do ((i 0 (+ i 1))) ((= i 8)) (set! (filt i) (double 0.0)))
    (set! (filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (do ((i start (+ i 1))) ((= i end))
	 (out-any i (* amp (convolve ff (lambda (dir)
					  (if (= dir 1)
					      (readin rd)
					      0.0)))) 
		  0))))))

(define (simple-cn2 beg dur amp file)
  "(simple-cn2 beg dur amp file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (filt (make-double-array 8)))
    (do ((i 0 (+ i 1))) ((= i 8)) (set! (filt i) (double 0.0)))
    (set! (filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt))
	  (ff1 (make-convolve :filter filt :input (make-readin file))))
      (run
       (do ((i start (+ i 1))) ((= i end))
	 (out-any i (* amp (+ (convolve ff (lambda (dir)
					     (if (= dir 1)
						 (readin rd)
						 0.0)))
			      (convolve ff1))) 
		  0))))))

(define (simple-src beg dur amp speed file)
  "(simple-src beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-src :input (make-readin file) :srate speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (src sr)) 0)))))

(define (simple-src-f beg dur amp speed file)
  "(simple-src-f beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-src :input (make-readin file) :srate speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (src sr 0.0 #f)) 0)))))

(define (simple-sr2 beg dur amp speed file)
  "(simple-sr2 beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (src sr 0.0 (lambda (dir) (if (= dir 1) (readin rd))))) 0)))))

(define (simple-sr2a beg dur amp speed file)
  "(simple-sr2a beg dur amp speed file) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :input rd :srate speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (src sr)) 0)))))

(define (simple-sro beg dur amp speed freq)
  "(simple-sro beg dur amp speed freq) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-src :srate speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (src sr 0.0 (lambda (dir)
				       (oscil os)))) 
		0)))))

(define (simple-grn beg dur amp speed freq)
  "(simple-grn beg dur amp speed freq) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-granulate :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate sr (lambda (dir) (oscil os)))) 0)))))

(define (simple-pvoc beg dur amp size file)
  "(simple-pvoc beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-phase-vocoder :input (make-readin file) :fft-size size)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (phase-vocoder sr)) 0)))))

(define (simple-ina beg dur amp file)
  "(simple-ina beg dur amp file) test instrument for ina"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (open-input file))
	 (ctr 0))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (in-any ctr 0 fil)) 0)
       (set! ctr (+ ctr 1))))))

(define (simple-in-rev beg dur ampa ampb)
  "(simple-in-rev beg dur ampa ampb) test instrument for in reverb"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (if (> ampa 0.0) (outa i (* ampa (ina i *reverb*))))
       (if (> ampb 0.0) (outb i (* ampb (inb i *reverb*))))))))

(define (simple-f2s beg dur amp file)
  "(simple-f2s beg dur amp file) test instrument for file->sample"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-file->sample file))
	 (ctr 0))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (file->sample fil ctr 0)) 0)
       (set! ctr (+ 1 ctr))))))

(define (simple-rdf beg dur amp file)
  "(simple-rdf beg dur amp file) test instrument for readin"
  (let* ((rd (make-readin file))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (readin rd)) 0)))))

(define (simple-loc beg dur freq amp)
  "(simple-loc beg dur freq amp) test instrument for locsig"
  (let* ((os (make-oscil freq))
	 (loc (make-locsig :degree 0.0))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (locsig loc i (* amp (oscil os)))))))

(define (simple-dloc beg dur freq amp)
  "(simple-dloc beg dur freq amp) test instrument for move-sound"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (loc (make-move-sound (list start end 1 0
				     (make-delay 32) (make-env '(0 0 1 1) :length 1000) (make-env '(0 0 1 1) :length 1000)
				     (vector (make-delay 32)) (vector (make-env '(0 0 1 1) :length 1000)) 
				     (vector (make-delay 32)) (vector 0 1)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (move-sound loc i (* amp (oscil os)))))))

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
				     (vector 0 1 2 3)))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (move-sound loc i (* amp (oscil os)))))))

;(with-sound (:channels 4 :output "temp.snd") (simple-dloc-4 0 2 440 .5))

(define (simple-dup beg dur freq amp)
  "(simple-dup beg dur freq amp) test instrument for arith"
  (let* ((os (make-oscil freq))
	 (j 2)
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (let ((amp .3)
	     (j 4))
	 (if (not (= j 4)) (clm-print "local j is ~D\n" j))
	 (if (> (abs (- amp .3)) .001) (clm-print "local amp is ~F\n" amp)))
       (if (= j 2)
	   (out-any i (* amp (oscil os)) 0))))))

(define (simple-du1 beg dur freq amp)
  "(simple-du1 beg dur freq amp) test instrument for arith"
  (let* ((os (make-oscil freq))
	 (j (+ (expt 2 41) 1234)) ; 2199023256786
	 (mj -3)
	 (jj (- (+ (expt 2 40) 4321))) ; -1099511632097
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (if (not (= j 2199023256786)) (clm-print "local j is ~A" j))
       (if (not (= jj -1099511632097)) (clm-print "local jj is ~A" jj))
       (if (= mj -3)
	   (out-any i (* amp (oscil os)) 0)
	   (clm-print "minus 3: ~D" mj))))))

(define (sample-desc beg dur freq amp)
  "(sample-desc beg dur freq amp) test instrument for generics"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (printed #f)
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (if (not printed)
	   (begin
	     (if (not (string=? (mus-describe os) "oscil freq: 440.000Hz, phase: 0.000"))
		 (clm-print "describe oscil: ~A~%" (mus-describe os)))
	     (if (> (abs (- (mus-frequency os) freq)) .001)
		 (clm-print "osc freq: ~A (~A)~%" (mus-frequency os) freq))
	     (set! printed #t)))
       (out-any i (* amp (oscil os)) 0)))))

(define (sample-mdat beg dur freq amp)
  "(sample-mdat beg dur freq amp) test instrument for coeffs"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (table-size 256)
	 (j 0)
	 (buf (make-table-lookup freq 0.0 :size table-size))
	 (table (mus-data buf)))
    (do ((i 0 (+ i 1)))
	((= i table-size))
      (set! (table i) (double (/ i table-size))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp ((mus-data buf) j) (table-lookup buf)) 0)
       (set! j (+ 1 j))
       (if (>= j table-size) (set! j 0))))))

(define (sample-xtab beg dur freq amp)
  "(sample-xtab beg dur freq amp) test instrument for generics"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (+ i 1)))
	((= i 8))
      (set! ((mus-xcoeffs flt) i) (double (/ i 16)))
      (set! ((mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp
		     (+ ((mus-xcoeffs flt) 4)
			((mus-ycoeffs flt) 4))
		     (filter flt (oscil os))) 
		0)))))

(define (sample-xts beg dur freq amp)
  "(sample-xts beg dur freq amp) test instrument for generics"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (+ i 1)))
	((= i 8))
      (set! ((mus-xcoeffs flt) i) (double (/ i 16)))
      (set! ((mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (vct-set! (mus-xcoeffs flt) 0 .5)
       (vct-set! (mus-ycoeffs flt) 0 .5)       
       (out-any i (* amp
		     (+ ((mus-xcoeffs flt) 0)
			(mus-ycoeff flt 0))
		     (filter flt (oscil os))) 
		0)))))

(define (sample-srl2 beg dur amp speed freq)
  "(sample-srl2 beg dur amp speed freq) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os1 (make-oscil freq))
	 (os2 (make-oscil (* freq 2)))
	 (sr1 (make-src :srate speed))
	 (sr2 (make-src :srate speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (+ (src sr1 0.0 (lambda (dir) (oscil os1)))
			    (src sr2 0.0 (lambda (dir) (oscil os2))))) 
		0)))))

(define (sample-srll beg dur amp speed freq)
  "(sample-srll beg dur amp speed freq) test instrument for src"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr1 (make-src :srate speed))
	 (sr2 (make-src :srate speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (src sr1 0.0 (lambda (dir)
					(src sr2 0.0
					     (lambda (dir)
					       (oscil os)))))) 
		0)))))

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
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (+ (src sr1 0.0 (lambda (dir)
					   (src sr2 0.0
						(lambda (dir)
						  (oscil os1)))))
			    (src sr3 0.0 (lambda (dir)
					   (oscil os2))))) 
		0)))))

(define (sample-grn2 beg dur amp speed freq)
  "(sample-grn2 beg dur amp speed freq) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-granulate :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate sr
				    (lambda (dir) 
				      (oscil os))
				    (lambda (g) 
				      0))) 
		0)))))

(define (sample-grn3 beg dur amp speed file)
  "(sample-grn3 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed))
	 (gr (make-granulate :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate gr (lambda (dir)
					 (src sr 0.0 (lambda (dir)
						       (readin rd))))))
		0)))))

(define (sample-cnv beg dur amp speed file)
  "(sample-cnv beg dur amp speed file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed))	 
	 (filt (make-double-array 8)))
    (do ((i 0 (+ i 1))) ((= i 8)) (set! (filt i) (double 0.0)))
    (set! (filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (do ((i start (+ i 1))) ((= i end))
	 (out-any i (* amp (convolve ff (lambda (dir)
					  (src sr 0.0 (lambda (dir)
							(readin rd)))))) 
		  0))))))

(define (sample-cnv1 beg dur amp speed file)
  "(sample-cnv1 beg dur amp speed file) test instrument for convolve"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-src :srate speed :input rd))	 
	 (filt (make-double-array 8)))
    (do ((i 0 (+ i 1))) ((= i 8)) (set! (filt i) (double 0.0)))
    (set! (filt 4) (double 1.0))
    (let ((ff (make-convolve :filter filt)))
      (run
       (do ((i start (+ i 1))) ((= i end))
	 (out-any i (* amp (convolve ff (lambda (dir)
					  (src sr)))) 
		  0))))))

(define (sample-pvoc1 beg dur amp size file)
  "(sample-pvoc1 beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (phase-vocoder sr
					(lambda (dir)
					  (readin fil)))) 
		0)))))

(define (sample-pvoc2 beg dur amp size file)
  "(sample-pvoc2 beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (phase-vocoder sr
					(lambda (dir)
					  (readin fil))
					#f
					(lambda (closure)
					  (if (not (= (mus-location sr) 0))
					      (clm-print "outctr: ~A" (mus-location sr)))
					  #t)
					#f)) 
		0)))))

(define (sample-pvoc3 beg dur amp size file)
  "(sample-pvoc3 beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (k 0)
	 (N2 (/ size 2))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i 
		(* amp (phase-vocoder sr
				      (lambda (dir)
					(readin fil))
				      #f
				      #f
				      (lambda (closure)
					(set! k 0)
					(do ()
					    ((= k N2))
					  (vct-set! (phase-vocoder-amps sr) k
						(+ ((phase-vocoder-amps sr) k) 
						   ((phase-vocoder-amp-increments sr) k)))
					  (vct-set! (phase-vocoder-phase-increments sr) k
						(+ ((phase-vocoder-phase-increments sr) k) 
						   ((phase-vocoder-freqs sr) k)))
					  (vct-set! (phase-vocoder-phases sr) k
						(+ ((phase-vocoder-phases sr) k)
						   ((phase-vocoder-phase-increments sr) k)))
					  (set! k (+ 1 k)))
					(clm23-sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2)))) 
		0)))))

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
     (do ((i start (+ i 1))) ((= i end))
       (let ((val (* (oscil os) amp)))
	 (frame-set! smp 0 val)
	 (set! (smp (+ int 1)) val)
	 (out-any i (frame->sample frm smp) 0)
	 (if (not (frame? fr0)) (clm-print ";~S not a frame?" (mus-describe fr0)))
	 (if (not (mixer? gen)) (clm-print ";~S not a mixer?" (mus-describe gen)))
	 (if (not (= (channels fr0) 2)) (clm-print ";frame channels: ~D?" (channels fr0)))
	 (if (not (= (length fr1) 2)) (clm-print ";frame length: ~D?" (length fr0)))
	 (if (not (= (channels gen) 2)) (clm-print ";mixer channels: ~D?" (channels gen)))
	 (frame->frame fr0 gen fr1)
	 (if (or (> (abs (- (fr0 0) 1.0)) .001)
		 (> (abs (- (fr1 1) 1.25)) .001)
		 (> (abs (- (gen 0 0) .5)) .001))
	     (clm-print ";fr1: ~A" (mus-describe fr1)))
	 (frame-set! fr1 0 1.0)
	 (set! (fr1 1) 1.0)
	 (frame+ fr0 fr1 fr3)
	 (frame* fr0 fr1 fr4)
	 (sample->frame fr1 .5 fr5)
	 (if (or (> (abs (- (fr3 0) 2.0)) .001)
		 (> (abs (-(fr4 0) 1.0)) .001))
	     (clm-print ";fr+*: ~A ~A" (mus-describe fr3) (mus-describe fr4)))
	 (if (> (abs (- (fr5 0) .5)) .001)
	     (clm-print ";sample->frame: ~A?" (fr5 0)))
	 (mixer+ mx1 mx2 nmx)
	 (if (or (> (abs (- (mx1 0 0) 2.0)) .001)
		 (> (abs (- (mx1 0 1) 0.0)) .001)
		 (> (abs (- (mx1 1 0) 0.0)) .001)
		 (> (abs (- (mx1 1 1) 2.0)) .001))
	     (clm-print ";make-scalar-mixer 2: ~A" (mus-describe mx1)))
	 (if (or (> (abs (- (mx2 0 0) .1)) .001)
		 (> (abs (- (mx2 0 1) .2)) .001)
		 (> (abs (- (mx2 1 0) .3)) .001)
		 (> (abs (- (mx2 1 1) .4)) .001))
	     (clm-print ";make-mixer .1 .2 .3 .4: ~A" (mus-describe mx2)))
	 (if (or (> (abs (- (nmx 0 0) 2.1)) .001)
		 (> (abs (- (nmx 0 1) 0.2)) .001)
		 (> (abs (- (nmx 1 0) 0.3)) .001)
		 (> (abs (- (nmx 1 1) 2.4)) .001))
	     (clm-print ";mixer add ~A ~A: ~A" (mus-describe mx1) (mus-describe mx2) (mus-describe nmx)))
	 (mixer* mx1 .5 mx1)
	 (if (or (> (abs (- (mx1 0 0) 1.0)) .001)
		 (> (abs (- (mx1 0 1) 0.0)) .001)
		 (> (abs (- (mx1 1 0) 0.0)) .001)
		 (> (abs (- (mx1 1 1) 1.0)) .001))
	     (clm-print ";make-scale (identity): ~A" (mus-describe mx1)))
	 (do ((j 0 (+ 1 j)))
	     ((= j 2))
	   (do ((k 0 (+ 1 k)))
	       ((= k 2))
	     (set! (nmx j k) 0.0)
	     (set! (mx1 j k) (* 1.0 (+ j k)))
	     (set! (mx2 j k) (* 1.0 j k))))
	 (mixer* mx1 mx2 nmx)
	 (if (or (> (abs (- (nmx 0 0) 0.0)) .001)
		 (> (abs (- (nmx 0 1) 1.0)) .001)
		 (> (abs (- (nmx 1 0) 0.0)) .001)
		 (> (abs (- (nmx 1 1) 2.0)) .001))
	     (clm-print ";mixer*: ~A" (mus-describe nmx)))
	 (do ((j 0 (+ 1 j)))
	     ((= j 2))
	   (do ((k 0 (+ 1 k)))
	       ((= k 2))
	     (set! (mx1 j k) 0.0)
	     (set! (nmx j k) 0.0)))
	 (do ((j 0 (+ 1 j)))
	     ((= j 2))
	   (set! (mx1 j j) 2.0))
	 (set! (mx2 0 0) .1)
	 (set! (mx2 0 1) .2)
	 (set! (mx2 1 0) .3)
	 (set! (mx2 1 1) .4))))))

(define (sample-osc beg dur freq amp)
  "(sample-osc beg dur freq amp) test instrument for oscil"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 20))
	 (arrfrq (make-double-array 20 :initial-element (double 0.0))))
    (do ((i 0 (+ i 1)))
	((= i 20))
      (set! (arrfrq i) (double (* (+ i 1) 100.0)))
      (set! (arr i) (make-oscil (* (+ i 1) 100))))
    (run
     (do ((k start (+ 1 k))) ((= k end))
       (let ((sum 0.0))
	 (do ((i 0 (+ i 1)))
	     ((= i (length arr)))
	   (if (oscil? (arr i))
	       (begin
		 (set! (mus-frequency (arr i)) (arrfrq i))
		 (if (> (abs (- (mus-frequency (arr i)) (arrfrq i))) .001)
		     (clm-print "oops ~A ~A" (mus-frequency (arr i)) (arrfrq i)))
		 (set! sum (+ sum (oscil (arr i)))))))
	 (out-any k (* amp .05 sum) 0))))))

(define (sample-ardcl beg dur freq amp)
  "(sample-ardcl beg dur freq amp) test instrument for arith"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (amps (make-double-array 3 :initial-element 0.0))
	 (phases (make-double-array 3 :initial-element 0.0))
	 (freqs (make-double-array 3 :initial-element 0.0))
	 (ints (make-vector 3 32)))
    (do ((i 0 (+ i 1)))
	((= i 3))
      (set! (freqs i) (double (* freq (+ i 1))))
      (set! (amps i) (double (/ amp (+ i 2)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (do ((i 0 (+ i 1)))
	   ((= i (vct-length phases)))
	 (set! (phases i) (+ (phases i) (hz->radians (freqs i)))))
       (if (not (= (ints 0) 32)) (clm-print "int array trouble"))
       (set! (ints 1) 3)
       (if (not (= (ints 1) 3)) (clm-print "set int array trouble"))
       (if (not (= (length amps) 3)) (clm-print "amps len: ~A" (length amps)))
       (out-any i (clm23-sine-bank amps phases 3) 0)))))

(define (sample-strs beg dur freq)
  "(sample-strs beg dur freq) test instrument for strings"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (filename "oboe.snd"))
    (run
     (do ((i start (+ i 1))) ((= i end))
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
       (if (not (= (frames filename) 50828)) (clm-print ";frames: ~A" (mus-sound-samples filename)))
       (if (not (= (mus-sound-srate filename) 22050)) (clm-print ";srate: ~A" (mus-sound-srate filename)))       
       (out-any i (oscil os) 0)))))

(define (sample-flt beg dur freq amp)
  "(sample-flt beg dur freq amp) test instrument for arith"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fltdat (make-double-array 3 :initial-element (double 3.14)))
	 (intdat (make-integer-array 3 :initial-element 3))
	 (flt (make-filter 8 :xcoeffs (make-vct 8) :ycoeffs (make-vct 8)))
	 (os (make-oscil freq)))
    (do ((i 0 (+ i 1)))
	((= i 8))
      (set! ((mus-xcoeffs flt) i) (double (/ i 16)))
      (set! ((mus-ycoeffs flt) i) (- 0.5 (double (/ i 16)))))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (let ((xs (mus-xcoeffs flt)))
	 (if (or (> (abs (- (xs 1) (mus-xcoeff flt 1))) .001)
		 (> (abs (- (xs 1) 0.0625)) .001))
	     (clm-print "~A ~A~%" (xs 1) (mus-xcoeff flt 1))))
       (let ((data (mus-data flt)))
	 (if (> (data 0) 1.0) (clm-print "data overflow? ~A~%" (data 0))))
       (let ((is intdat)
	     (fs fltdat))
	 (if (not (= (is 1) 3)) (clm-print "intdat let: ~A~%" (is 1)))
	 (if (> (abs (- (fs 1) 3.14)) .001) (clm-print "fltdat let: ~A~%" (fs 1))))
       (out-any i (* amp (filter flt (oscil os))) 0)))))

(define (sample-arrintp beg dur freq amp)
  "(sample-arrintp beg dur freq amp) test instrument for array-interp"
  (let* ((os (make-oscil freq))
	 (arr (make-double-array 101))
	 (start (seconds->samples beg))
	 (len (seconds->samples dur))
	 (end (+ start len))
	 (loc 0.0)
	 (loc-incr (/ 100.0 len)))
    (do ((i 0 (+ i 1))
	 (x 0.0 (+ x .01)))
	((= i 100))
      (set! (arr i) (double x)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (array-interp arr loc) (oscil os)) 0)
       (set! loc (+ loc loc-incr))))))

(define (sample-if beg dur freq amp)
  "(sample-if beg dur freq amp) test instrument for ifs"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (k -123)
	 (j 0)
	 (bool #t))
    (run
     (do ((i start (+ i 1))) ((= i end))
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
       (set! k (do ((m 0 (+ 1 m)))
		   ((= m 3) j)
		 (set! j (+ j 1))))
       (if (not (= k 3)) (clm-print "do expr: ~A~%" k))
					;	 (dotimes (m 2)
					;		  (set! k (- k 1)))
					;	 (if (not (= k 1)) (clm-print "dotimes: ~A~%" k))
       (out-any i (* amp (oscil os)) 0)))))

(define (sample-arrfile beg dur freq amp)
  "(sample-arrfile beg dur freq amp) test instrument for arrays"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-double-array 100 :initial-element (double 0.0)))
	 (ctr 0)
	 (dir 1))
    (do ((i 0 (+ i 1)))
	((= i 100))
      (set! (arr i) (double (* amp (+ -.5 (* i .01))))))
    (array->file "testx.data" arr 100 22050 1)
    (do ((i 0 (+ i 1)))
	((= i 100))
      (set! (arr i) (double 0.0)))
    (file->array "testx.data" 0 0 100 arr)
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* (arr ctr) (oscil os)) 0)
       (set! ctr (+ ctr dir))
       (if (>= ctr 99) (set! dir -1)
	   (if (<= ctr 0) (set! dir 1)))))))

(define (simple-grn-f1 beg dur amp speed freq)
  "(simple-grn-f1 beg dur amp speed freq) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (os (make-oscil freq))
	 (sr (make-granulate :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate sr (lambda (dir) (oscil os)) #f)) 0)))))

;(with-sound () (simple-grn-f1 0 1 .1 2 440))

(define (simple-grn-f2 beg dur amp speed file)
  "(simple-grn-f2 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate sr)) 0)))))

;(with-sound () (simple-grn-f2 0 1 1 2 "oboe.snd"))

(define (simple-grn-f3 beg dur amp speed file)
  "(simple-grn-f3 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate sr #f #f)) 0)))))

;(with-sound () (simple-grn-f3 0 1 1 2 "oboe.snd"))

(define (simple-grn-f4 beg dur amp speed file)
  "(simple-grn-f4 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate sr #f)) 0)))))

;(with-sound () (simple-grn-f4 0 1 1 2 "oboe.snd"))

(define (simple-grn-f5 beg dur amp speed file)
  "(simple-grn-f5 beg dur amp speed file) test instrument for granulate"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-granulate :input rd :expansion speed)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (granulate sr #f
				    (lambda (g)
				      (let ((grain (mus-data g))  ; current grain
					    (len (length g))) ; current grain length
					(do ((i 0 (+ i 1)))
					    ((= i len) len)       ; grain length unchanged in this case
					  (set! (grain i) (* 2 (grain i)))))
				      0)))
		0)))))

;(with-sound () (simple-grn-f5 0 1 1 2 "oboe.snd"))

(define (sample-pvoc5 beg dur amp size file freq)
  "(sample-pvoc5 beg dur amp size file freq) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-readin file))
	 (sr (make-phase-vocoder :fft-size size))
	 (os (make-oscil freq)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (phase-vocoder sr
					(lambda (dir)
					  (readin fil))
					#f
					#f
					(lambda (closure)
					  (oscil os))))
		  0)))))

;(with-sound () (sample-pvoc5 0 1 .1 256 "oboe.snd" 440.0))


#|
(with-sound (:statistics #t)
	    (simple-ssb 0 .2 440 .1)
	    (simple-nsin .6 .2 .1)
	    (simple-ncos 0.7 .2 440 .1)
	    (simple-nrxysin .6 .2 .1)
	    (simple-nrxycos 0.7 .2 440 .1)
	    (simple-osc 0.75 .2 440 .1)
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
	    (sample-strs 5.75 .2 440)
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
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (phase-vocoder sr)) 0)))))

(define (pvoc-b beg dur amp size file)
  "(pvoc-b beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (phase-vocoder sr (lambda (dir) (readin rd)))) 0)))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-b 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (format #t ";pvoc a-b: ~A" mx)))
|#

(define (pvoc-c beg dur amp size file)
  "(pvoc-c beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i 
		(* amp
		   (phase-vocoder sr 
				  (lambda (dir) (readin rd))
				  #f
				  #f
				  (lambda (closure)
				    (let ((N2 (floor (/ size 2))))
				      (do ((k 0 (+ 1 k)))
					  ((= k N2))
					(vct-set! (phase-vocoder-amps sr) k (+ ((phase-vocoder-amps sr) k) 
									       ((phase-vocoder-amp-increments sr) k)))
					(vct-set! (phase-vocoder-phase-increments sr) k (+ ((phase-vocoder-phase-increments sr) k) 
											   ((phase-vocoder-freqs sr) k)))
					(vct-set! (phase-vocoder-phases sr) k (+ ((phase-vocoder-phases sr) k)
										 ((phase-vocoder-phase-increments sr) k))))
				      (clm23-sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2)))
				  ))
		0)))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-c 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (format #t ";pvoc a-c: ~A" mx)))
|#


(define (pvoc-d beg dur amp size file)
  "(pvoc-d beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4))
	 (N2 (floor (/ size 2)))
	 (lastphases (make-vct N2 0.0))
	 (two-pi (* 2 pi)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i 
		(* amp
		   (phase-vocoder sr 
				  (lambda (dir) (readin rd))
				  #f
				  (lambda (closure)
				    (let* ((D (floor (/ size 4))) ; overlap = 4
					   (pscl (/ 1.0 D))
					   (kscl (/ two-pi size)))
				      (do ((k 0 (+ 1 k))
					   (ks 0.0 (+ ks kscl)))
					  ((= k N2))
					(let* ((freq ((phase-vocoder-freqs sr) k))
					       (diff (- freq (lastphases k))))
					  (set! (lastphases k) freq)
					  (if (> diff pi) (set! diff (- diff two-pi)))
					  (if (< diff (- pi)) (set! diff (+ diff two-pi)))
					  (set! ((phase-vocoder-freqs sr) k) (+ (* diff  pscl) ks))))
				      #f))
				  (lambda (closure)
				    (do ((k 0 (+ 1 k)))
					((= k N2))
				      (vct-set! (phase-vocoder-amps sr) k (+ ((phase-vocoder-amps sr) k) 
									     ((phase-vocoder-amp-increments sr) k)))
				      (vct-set! (phase-vocoder-phase-increments sr) k (+ ((phase-vocoder-phase-increments sr) k) 
											 ((phase-vocoder-freqs sr) k)))
				      (vct-set! (phase-vocoder-phases sr) k (+ ((phase-vocoder-phases sr) k)
									       ((phase-vocoder-phase-increments sr) k))))
				    (clm23-sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2))
				  ))
		0)))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-d 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (format #t ";pvoc a-d: ~A" mx)))
|#

(define (pvoc-e beg dur amp size file)
  "(pvoc-e beg dur amp size file) test instrument for phase-vocoder"
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (rd (make-readin file))
	 (sr (make-phase-vocoder :fft-size size :interp (/ size 4) :overlap 4))
	 (N2 (floor (/ size 2)))
	 (lastphases (make-vct N2 0.0))
	 (in-data (make-vct size 0.0))
	 (two-pi (* 2 pi))
	 (filptr 0)
	 (window (make-fft-window hamming-window size 0.0))
	 (D (floor (/ size 4)))) ; overlap = 4
    (vct-scale! window (/ 2.0 (* 0.54 size)))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i 
		(* amp
		   (phase-vocoder sr 
				  (lambda (dir) (readin rd))
				  
				  (lambda (closure input)
				    (let ((buf (modulo filptr size)))
				      (clear-array (phase-vocoder-freqs sr))
				      (if (= filptr 0)
					  (do ((k 0 (+ 1 k)))
					      ((= k size))
					    (set! (in-data k) (readin rd)))
					  (begin
					    (do ((k 0 (+ 1 k))
						 (j D (+ 1 j)))
						((= j size))
					      (set! (in-data k) (in-data j)))
					    (do ((k (- size D) (+ 1 k)))
						((= k size))
					      (set! (in-data k) (readin rd)))))
				      (do ((k 0 (+ 1 k)))
					  ((= k size))
					(vct-set! (phase-vocoder-amp-increments sr) buf (* (in-data k) (window k)))
					(set! buf (+ 1 buf))
					(if (>= buf size) (set! buf 0)))
				      (set! filptr (+ filptr D))
				      (mus-fft (phase-vocoder-amp-increments sr) (phase-vocoder-freqs sr) size 1)
				      (rectangular->polar (phase-vocoder-amp-increments sr) (phase-vocoder-freqs sr))
				      #f))
				  
				  (lambda (closure)
				    (let ((pscl (/ 1.0 D))
					  (kscl (/ two-pi size)))
				      (do ((k 0 (+ 1 k))
					   (ks 0.0 (+ ks kscl)))
					  ((= k N2))
					(let* ((freq ((phase-vocoder-freqs sr) k))
					       (diff (- freq (lastphases k))))
					  (set! (lastphases k) freq)
					  (if (> diff pi) (set! diff (- diff two-pi)))
					  (if (< diff (- pi)) (set! diff (+ diff two-pi)))
					  (vct-set! (phase-vocoder-freqs sr) k (+ (* diff  pscl) ks))))
				      #f))
				  
				  (lambda (closure)
				    (do ((k 0 (+ 1 k)))
					((= k N2))
				      (vct-set! (phase-vocoder-amps sr) k (+ ((phase-vocoder-amps sr) k) 
									     ((phase-vocoder-amp-increments sr) k)))
				      (vct-set! (phase-vocoder-phase-increments sr) k (+ ((phase-vocoder-phase-increments sr) k) 
											 ((phase-vocoder-freqs sr) k)))
				      (vct-set! (phase-vocoder-phases sr) k (+ ((phase-vocoder-phases sr) k)
									       ((phase-vocoder-phase-increments sr) k))))
				    (clm23-sine-bank (phase-vocoder-amps sr) (phase-vocoder-phases sr) N2))
				  ))
		0)))))

#|
(let* ((outfile (with-sound () (pvoc-a 0 2.3 1 256 "oboe.snd") (pvoc-e 0 2.3 -1 256 "oboe.snd")))
       (mx (mus-sound-maxamp outfile)))
  (if (fneq (cadr mx) 0.0)
      (format #t ";pvoc a-e: ~A" mx)))
|#

(define (or1)
  "(or1) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :length 10))
	(e2 (make-env '(0 1 1 0) :length 10))
	(e3 #f)
	(ok1 0.0))
    (run
     (do ((i 0 (+ i 1)))
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
	   (clm-print ";or1 ok1: ~A~%" ok1))))))

(define (or2)
  "(or2) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :length 10))
	(e2 (make-env '(0 1 1 0) :length 10))
	(e3 #f)
	(ok1 0.0)
	(oki 0)
	(okb #f))
    (run
     (do ((i 0 (+ i 1)))
	 ((= i 1))
       (declare (e1 clm) (e2 clm) (e3 clm)
		(ok1 float)
		(oki int)
		(okb boolean))
       (set! ok1 0.0)
       (set! oki (+ 1 oki))
       (set! okb #t)
       (if (or e1 e2)
	   (set! ok1 (+ ok1 (env e1)))
	   (clm-print ";or2 a~%"))
       (if (not (or e1 e2 okb))
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
	   (clm-print ";or1 ok1: ~A~%" ok1))))))

(define (or3)
  "(or3) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :length 10))
	(i1 (make-vector 3 32))
	(f1 (make-vct 3 3.14))
	(i2 (make-vector 3 3))
	(f2 (make-vector 3 1.5))
	(ok1 0.0)
	(oki 0))
    (run
     (do ((i 0 (+ i 1)))
	 ((= i 1))
       (cond (e1 (set! ok1 (+ ok1 (env e1))))
	     (#t (clm-print ";or3 1~%")))
       (if (or f1 f2)
	   (set! ok1 (+ ok1 (f2 0)))
	   (clm-print ";or3 a~%"))
       (if (not (or f2 f1))
	   (clm-print ";or3 2~%"))
       (if (and f2 f1)
	   (set! ok1 (+ ok1 (f1 1)))
	   (clm-print ";or3 b~%"))
       (if (or i1 i2)
	   (set! oki (+ oki (i2 0)))
	   (clm-print ";or3 d~%"))
       (if (not (or i2 i1))
	   (clm-print ";or3 3~%"))
       (if (and i2 i1)
	   (set! oki (+ oki (i1 1)))
	   (clm-print ";or3 e~%"))))))

(define (or4)
  "(or4) test function for or"
  (let ((e1 (make-env '(0 0 1 1) :length 10))
	(e2 (make-env '(0 1 1 0) :length 10))
  	(i1 (make-vector 3 32))
	(f1 (make-vct 3 3.14))
	(i2 (make-vector 3 3))
	(f2 (make-vector 3 1.5))
	(oki 0))
    (run
     (do ((i 0 (+ i 1)))
	 ((= i 1))
       (if (or (and e1 e2)
	       (and f1 f2)
	       (and i1 i2))
	   (set! oki (+ 1 oki))
	   (clm-print ";or4 a~%"))
       (if (and (or f1 f2)
		(not (or i1 i2))
		(or e1 e2))
	   (clm-print ";or4 1~%"))
       (if f1
	   (if e1
	       (if (not e2)
		   (clm-print ";or4 2~%")
		   (set! oki (+ 1 oki)))
	       (clm-print ";or4 3~%"))
	   (clm-print ";or4 4~%"))))))


;;; --------------------------------------------------------------------------------
;;;
;;; instruments and note lists from the documentation


(if (not (provided? 'snd-dsp.scm)) (load "dsp.scm"))
(if (not (provided? 'snd-jcrev.scm)) (load "jcrev.scm"))

;;; ins in docs + note lists

;;; fm.html

(define (fmdoc-pm beg end freq amp mc-ratio index)
  (let ((carrier-phase 0.0) ; set to pi/2 if someone tells you PM can't produce energy at 0Hz
        (carrier-phase-incr (hz->radians freq))
        (modulator-phase 0.0)
        (modulator-phase-incr (hz->radians (* mc-ratio freq))))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (let* ((modulation (* index (sin modulator-phase)))
	      (pm-val (* amp (sin (+ carrier-phase modulation))))) 
	 ;; no integration in phase modulation (it's a phase change)
	 (set! carrier-phase (+ carrier-phase carrier-phase-incr))
	 (set! modulator-phase (+ modulator-phase modulator-phase-incr))
	 (outa i pm-val))))))

(define (fmdoc-fm beg end freq amp mc-ratio index)
  (let* ((carrier-phase 0.0)
	 (carrier-phase-incr (hz->radians freq))
	 (modulator-phase-incr (hz->radians (* mc-ratio freq)))
	 (modulator-phase (* 0.5 (+ pi modulator-phase-incr)))
	 ;; (pi+incr)/2 to get (centered) sin after integration, to match pm case above
	 ;;   I believe this is what causes most of the confusion
	 (fm-index (hz->radians (* mc-ratio freq index))))
	;; fix up fm index (it's a frequency change)
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (let ((modulation (* fm-index (sin modulator-phase)))
	     (fm-val (* amp (sin carrier-phase))))
	 (set! carrier-phase (+ carrier-phase modulation carrier-phase-incr))
	 (set! modulator-phase (+ modulator-phase modulator-phase-incr))
	 (outb i fm-val))))))

(define* (fmdoc-fm-1 beg dur freq amp mc-ratio index (index-env '(0 1 100 1)))
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (cr (make-oscil freq))
         (md (make-oscil (* freq mc-ratio)))
         (fm-index (hz->radians (* index mc-ratio freq)))
         (ampf (make-env index-env :scaler amp :duration dur)) 
         (indf (make-env index-env :scaler fm-index :duration dur)))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* (env ampf)                  ; amplitude env
		  (oscil cr (* (env indf)     ; carrier + modulation env
			       (oscil md))))  ; modulation
	     )))))

(define (fmdoc-fm-2 beg dur freq amp mc-ratio index carrier-phase mod-phase)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq carrier-phase))
	 (md (make-oscil (* freq mc-ratio) mod-phase))
	 (fm-index (hz->radians (* index mc-ratio freq))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp (oscil cr (* fm-index (oscil md)))))))))

(define (fmdoc-fm-3 beg dur freq amp mc-ratio index car-phase mod-phase skew-func skew)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq car-phase))
	 (md (make-oscil (* freq mc-ratio) mod-phase))
	 (skewf (make-env skew-func :scaler (hz->radians (* skew mc-ratio freq)) :duration dur))
	 (fm-index (hz->radians (* index mc-ratio freq))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp (oscil cr (* fm-index (oscil md (env skewf))))))))))

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
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp (+ (* (oscil am0) (oscil cr0 (* fm-index (oscil md0))))
			 (* (oscil am1) (oscil cr1 (* fm-index (oscil md1)))))))))))

(define (fmdoc-fm-5 beg dur freq amp mc-ratios indexes carrier-phase mod-phases)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq carrier-phase))
         (n (length mc-ratios))
         (modulators (make-vector n))
         (fm-indices (make-vct n)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (set! (modulators i) (make-oscil (* freq (mc-ratios i)) (mod-phases i)))
      (set! (fm-indices i) (hz->radians (* freq (indexes i) (mc-ratios i)))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (let ((sum 0.0))
	 (do ((k 0 (+ 1 k)))
	     ((= k n))
	   (set! sum (+ sum (* (fm-indices k) (oscil (modulators k))))))
	 (outa i (* amp (oscil cr sum))))))))

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
     (do ((i start (+ i 1)))
	 ((= i end))
       (let ((vib (+ (triangle-wave pervib) (rand-interp ranvib))))
	 (outa i (* (env ampf)
		    (oscil carrier
			   (+ vib 
			      (* (env indf1) (oscil fmosc1 vib))
			      (* (env indf2) (oscil fmosc2 (* 3.0 vib)))
			      (* (env indf3) (oscil fmosc3 (* 4.0 vib))))))))))))

(define (fmdoc-cascade beg dur freq amp modrat modind casrat casind caspha)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq))
	 (md (make-oscil (* freq modrat)))
	 (ca (make-oscil (* freq casrat) caspha))
	 (fm-ind0 (hz->radians (* modind modrat freq)))
	 (fm-ind1 (hz->radians (* casind (/ casrat modrat) freq))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp 
		  (oscil cr (* fm-ind0 
			       (oscil md (* fm-ind1 
					    (oscil ca)))))))))))

(define (fmdoc-feedbk beg dur freq amp index)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (y 0.0)
	 (x-incr (hz->radians freq)))
    (run 
     (do ((i start (+ i 1))
	  (x 0.0 (+ x x-incr)))
	 ((= i end))
       (set! y (+ x (* index (sin y))))
       (outa i (* amp (sin y)))))))

(define* (fmdoc-vox beg dur freq amp (indexes '(.005 .01 .02)) (formant-amps '(.86 .13 .01)))
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (car-os (make-oscil 0))
         (evens (make-vector 3))
         (odds (make-vector 3))
         (amps (apply vct formant-amps))
         (ampf (make-env '(0 0 25 1 75 1 100 0) :scaler amp :duration dur))
         (frmfs (make-vector 3))
         (indices (apply vct indexes))
         (per-vib (make-triangle-wave 6 :amplitude (* freq .03)))
         (ran-vib (make-rand-interp 20 :amplitude (* freq .5 .02))))
    (do ((i 0 (+ i 1)))
	((= i 3))
      (set! (evens i) (make-oscil 0))
      (set! (odds i) (make-oscil 0)))

    (set! (frmfs 0) (make-env '(0 520 100 490) :duration dur)) 
    (set! (frmfs 1) (make-env '(0 1190 100 1350) :duration dur)) 
    (set! (frmfs 2) (make-env '(0 2390 100 1690) :duration dur))

    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (let* ((frq (+ freq (triangle-wave per-vib) (rand-interp ran-vib)))
	      (car (oscil car-os (hz->radians frq)))
	      (sum 0.0))
	 (do ((k 0 (+ 1 k)))
	     ((= k 3))
	   (let* ((frm (env (frmfs k)))
		  (frm0 (/ frm frq))
		  (frm-int (floor frm0))
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
	     (set! sum (+ sum (* (amps k) 
				 (+ (* even-amp 
				       (oscil (evens k) 
					      (+ even-freq (* (indices k) car))))
				    (* odd-amp 
				       (oscil (odds k) 
					      (+ odd-freq (* (indices k) car))))))))))
	 (outa i (* (env ampf) sum)))))))

;;; --------------------------------------------------------------------------------

;;; sndclm.html


(define (sndclmdoc-simp start end freq amp)
  (let ((os (make-oscil freq)))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (outa i (* amp (oscil os)))))))

(define (sndclmdoc-simp-1 beg dur freq amp)
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (outa i (* amp (oscil os)))))))

(define (sndclmdoc-simp-2 beg dur freq amp)
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (outa i (* amp (oscil os)))))))

(definstrument (sndclmdoc-simp-3 beg dur freq amp)
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (outa i (* amp (oscil os)))))))

(define (sndclmdoc-telephone start telephone-number)
  (let ((touch-tab-1 '(0 697 697 697 770 770 770 852 852 852 941 941 941))
	(touch-tab-2 '(0 1209 1336 1477 1209 1336 1477 1209 1336 1477 1209 1336 1477)))
    (do ((i 0 (+ i 1)))
	((= i (length telephone-number)))
      (let* ((num (telephone-number i))
	     (frq1 (touch-tab-1 num))
	     (frq2 (touch-tab-2 num)))
        (sndclmdoc-simp-3 (+ start (* i .4)) .3 frq1 .1)
        (sndclmdoc-simp-3 (+ start (* i .4)) .3 frq2 .1)))))

(definstrument (sndclmdoc-simp-4 beg dur freq amp envelope)
  (let* ((os (make-oscil freq))
         (amp-env (make-env envelope :duration dur :scaler amp))
	 (start (seconds->samples beg))
         (end (+ start (seconds->samples dur))))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (outa i (* (env amp-env) (oscil os)))))))

(define (make-my-oscil frequency)       ; we want our own oscil!
  (vct 0.0 (hz->radians frequency)))    ; current phase and frequency-based phase increment

(define (my-oscil gen fm)     ; the corresponding generator
  (let ((result (sin (gen 0)))) ; return sin(current-phase)
    (set! (gen 0) (+ (gen 0)  ; increment current phase
		     (gen 1)  ;    by frequency
		     fm))     ;    and FM
    result))                  ; return sine wave

(define (sndclmdoc-simp-5 start end freq amp frq-env)
  (let ((os (make-oscil freq)) 
        (frqe (make-env frq-env :length (- end start) :scaler (hz->radians freq))))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (outa i (* amp (oscil os (env frqe))))))))

(definstrument (sndclmdoc-simple-fm beg dur freq amp mc-ratio index amp-env index-env)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (cr (make-oscil freq))                     ; carrier
         (md (make-oscil (* freq mc-ratio)))        ; modulator
         (fm-index (hz->radians (* index mc-ratio freq)))
         (ampf (make-env (or amp-env '(0 0  .5 1  1 0)) :scaler amp :duration dur))
         (indf (make-env (or index-env '(0 0  .5 1  1 0)) :scaler fm-index :duration dur)))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* (env ampf) (oscil cr (* (env indf) (oscil md)))))))))

(define (sndclmdoc-simple-add beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (arr (make-vector 20)))     ; we'll create a tone with 20 equal amplitude harmonics
    (do ((i 0 (+ i 1)))               ;   use the 'f' button to check out the spectrum
	((= i 20))
      (set! (arr i) (make-oscil (* (+ i 1) freq))))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (let ((sum 0.0))
	 (do ((k 0 (+ 1 k)))
	     ((= k 20))
	   (set! sum (+ sum (oscil (arr k)))))
	 (out-any i (* amp .05 sum) 0))))))

(definstrument (sndclmdoc-mapenv beg dur frq amp en)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (osc (make-oscil frq))
	 (zv (make-env en 1.0 dur)))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (let ((zval (env zv))) 
	 (outa i 
	       (* amp 
		  (sin (* 0.5 pi zval zval zval)) 
		  (oscil osc))))))))

(definstrument (sndclmdoc-simple-table dur)
  (let ((tab (make-table-lookup :wave (partials->wave '(1 .5  2 .5)))))
    (run
     (do ((i 0 (+ i 1))) ((= i dur))
       (outa i (* .3 (table-lookup tab)))))))

(define (sndclmdoc-looper start dur sound freq amp)
  (let* ((beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur)))
	 (loop-data (mus-sound-loop-info sound)))
    (if (or (null? loop-data)
	    (<= (cadr loop-data) (car loop-data)))
	(throw 'no-loop-positions)
	(let* ((loop-start (car loop-data))
	       (loop-end (cadr loop-data))
	       (loop-length (+ 1 (- loop-end loop-start)))
	       (sound-section (file->array sound 0 loop-start loop-length (make-vct loop-length)))
	       (original-loop-duration (/ loop-length (srate sound)))
	       (tbl (make-table-lookup :frequency (/ freq original-loop-duration) :wave sound-section)))
	       ;; "freq" here is how fast we read (transpose) the sound -- 1.0 returns the original
	  (run
	   (do ((i beg (+ i 1)))
	       ((= i end))
	     (outa i (* amp (table-lookup tbl)))))))))

(definstrument (sndclmdoc-fm-table file start dur amp read-speed modulator-freq index-in-samples)
  (let* ((beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur)))
	 (table-length (frames file))
	 (tab (make-table-lookup :frequency (/ read-speed (mus-sound-duration file)) 
				 :wave (file->array file 0 0 table-length (make-vct table-length))))
	 (osc (make-oscil modulator-freq))
	 (index (/ (* (hz->radians modulator-freq) 2 pi index-in-samples) table-length)))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (outa i (* amp (table-lookup tab (* index (oscil osc)))))))))

(definstrument (sndclmdoc-bigbird start duration frequency freqskew amplitude freq-env amp-env partials)
  (let* ((beg (seconds->samples start))
         (end (+ beg (seconds->samples duration)))
         (gls-env (make-env freq-env (hz->radians freqskew) duration))
         (polyos (make-polyshape frequency :coeffs (partials->polynomial partials)))
         (fil (make-one-pole .1 .9))
         (amp-env (make-env amp-env amplitude duration)))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (outa i 
	     (one-pole fil   ; for distance effects
		       (* (env amp-env) 
			  (polyshape polyos 1.0 (env gls-env)))))))))

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
    (run
     (do ((i beg (+ i 1))) ((= i end))
       (let ((ax (oscil spacing-cos)))
	 (outa i (- (* (oscil carrier-sin) 
                       (oscil spacing-sin) 
		       (polynomial sin-coeffs ax))
		    (* (oscil carrier-cos) 
		       (polynomial cos-coeffs ax)))))))))

(definstrument (sndclmdoc-bl-saw start dur frequency order)
  (let ((norm (if (= order 1) 1.0           ; these peak amps were determined empirically
		  (if (= order 2) 1.3       ;   actual limit is supposed to be pi/2 (G&R 1.441)
		      (if (< order 9) 1.7   ;   but Gibbs phenomenon pushes it to 1.851
			  1.9))))           ;   if order>25, numerical troubles -- use table-lookup
	(freqs '()))
    (do ((i 1 (+ i 1)))
	((> i order))
      (set! freqs (cons (/ 1.0 (* norm i)) (cons i freqs))))
    (let* ((ccos (make-oscil frequency (/ pi 2.0)))
	   (csin (make-oscil frequency))
	   (coeffs (partials->polynomial (reverse freqs) mus-chebyshev-second-kind))
	   (beg (seconds->samples start))
	   (end (+ beg (seconds->samples dur))))
      (run 
       (do ((i beg (+ i 1))) 
	   ((= i end))
	 (outa i (* (oscil csin) 
		    (polynomial coeffs (oscil ccos)))))))))

(define (sndclmdoc-tritri start dur freq amp index mcr)
  (let* ((beg (seconds->samples start))
	 (end (+ beg (seconds->samples dur)))
	 (carrier (make-triangle-wave freq))
	 (modulator (make-triangle-wave (* mcr freq))))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (outa i (* amp (triangle-wave carrier 
				     (* index (triangle-wave modulator)))))))))

(define* (sndclmdoc-make-sinc-train (frequency 440.0) (width #f))
  (let ((range (or width (* pi (- (* 2 (floor (/ (mus-srate) (* 2.2 frequency)))) 1)))))
    ;; 2.2 leaves a bit of space before srate/2, (* 3 pi) is the minimum width, normally
    (list (- (* range 0.5))
	  range
	  (/ (* range frequency) (mus-srate)))))
	
(define* (sndclmdoc-sinc-train gen (fm 0.0))
  (let* ((ang (car gen))
	 (range (cadr gen))
	 (top (* 0.5 range))
	 (frq (caddr gen))
	 (val (if (= ang 0.0) 1.0 (/ (sin ang) ang)))
	 (new-ang (+ ang frq fm)))
    (if (> new-ang top)
	(set! (gen 0) (- new-ang range))
	(set! (gen 0) new-ang))
    val))

(define (sndclmdoc-make-sum-of-odd-sines frequency n)
  (vct 0.0 (hz->radians frequency) (* 1.0 n)))

(define (sndclmdoc-sum-of-odd-sines gen fm)
  (let* ((angle (gen 0))
	 (a2 (* angle 0.5))
	 (n (gen 2))
	 (den (* n (sin a2)))
	 (result (if (< (abs den) 1.0e-9)
		     0.0
		     (/ (* (sin (* n a2)) 
			   (sin (* (+ 1 n) a2)))
			den))))
    (set! (gen 0) (+ (gen 0) (gen 1) fm))
    result))

(definstrument (sndclmdoc-shift-pitch beg dur file freq (order 40))
  (let* ((st (seconds->samples beg))
         (nd (+ st (seconds->samples dur)))
	 (gen (make-ssb-am freq order))
	 (rd (make-readin file)))
    (run
     (do ((i st (+ i 1))) 
	 ((= i nd))
       (outa i (ssb-am gen (readin rd)))))))

(definstrument (sndclmdoc-repitch beg dur sound old-freq new-freq 
	         (amp 1.0) (pairs 10) (order 40) (bw 50.0))
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (ssbs (make-vector pairs))
	 (bands (make-vector pairs))
	 (factor (/ (- new-freq old-freq) old-freq))
	 (rd (make-readin sound)))
    (do ((i 1 (+ i 1)))
	((> i pairs))
      (let ((aff (* i old-freq))
	    (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(set! (ssbs (- i 1)) (make-ssb-am (* i factor old-freq)))
	(set! (bands (- i 1)) (make-bandpass (hz->radians (- aff bwf)) 
						 (hz->radians (+ aff bwf)) 
						 order))))
    (run
     (do ((i start (+ i 1))) 
	 ((= i end))
       (let ((sum 0.0)
	     (y (readin rd)))
	 (do ((band 0 (+ 1 band)))
	     ((= band pairs))
	   (set! sum (+ sum (ssb-am (ssbs band) 
				    (bandpass (bands band) y)))))
	   (outa i (* amp sum)))))))

(definstrument (sndclmdoc-fofins beg dur frq amp vib f0 a0 f1 a1 f2 a2 ve ae)
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
    (do ((i 0 (+ i 1)))
        ((= i foflen))
      (set! (foftab i) ;; this is not the pulse shape used by B&R
            (* (+ (* a0 (sin (* i frq0))) 
                  (* a1 (sin (* i frq1))) 
                  (* a2 (sin (* i frq2)))) 
               .5 (- 1.0 (cos (* i win-freq))))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* (env ampf) (wave-train wt0 (* (env vibenv) (oscil vibr)))))))))

(definstrument (sndclmdoc-echo beg dur scaler secs file)
  (let ((del (make-delay (seconds->samples secs)))
	(rd (make-sampler 0 file)))
    (run
     (do ((i beg (+ i 1)))
	 ((= i (+ beg dur)))
       (let ((inval (rd)))
	 (outa i (+ inval (delay del (* scaler (+ (tap del) inval))))))))))

(define* (sndclmdoc-make-moving-max (size 128))
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
    (run
      (do ((i beg (+ i 1))) ((= i end))
        (outa i (* (env aenv) (comb d0 (pulse-train s) (env zenv))))))))

(define (sndclmdoc-fir+comb beg dur freq amp size)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (dly (make-comb :scaler .9 :size size)) 
	 (flt (make-fir-filter :order size :xcoeffs (mus-data dly))) ; comb delay line as FIR coeffs
	 (r (make-rand freq)))                                       ; feed comb with white noise
    (run 
     (do ((i start (+ i 1))) 
	 ((= i end)) 
       (outa i (* amp (fir-filter flt (comb dly (rand r)))))))))

(definstrument (sndclmdoc-simple-src start-time duration amp srt srt-env filename)
  (let* ((senv (make-env :envelope srt-env :duration duration))
         (beg (seconds->samples start-time))
         (end (+ beg (seconds->samples duration)))
         (src-gen (make-src :input (make-readin filename) :srate srt)))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (outa i (* amp (src src-gen (env senv))))))))

(definstrument (sndclmdoc-srcer start-time duration amp srt fmamp fmfreq filename)
  (let* ((os (make-oscil :frequency fmfreq))
         (beg (seconds->samples start-time))
         (end (+ beg (seconds->samples duration)))
         (src-gen (make-src :input (make-readin filename) :srate srt)))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (outa i (* amp (src src-gen (* fmamp (oscil os)))))))))

(definstrument (sndclmdoc-convins beg dur filter file (size 128))
  (let* ((start (seconds->samples beg))
         (end (+ start (seconds->samples dur)))
         (ff (make-convolve :input (make-readin file) :fft-size size :filter filter)))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (convolve ff))))))

(definstrument (sndclmdoc-granulate-sound file beg dur (orig-beg 0.0) (exp-amt 1.0))
  (let* ((f-srate (srate file))
	 (f-start (round (* f-srate orig-beg)))
         (f (make-readin file :start f-start))
	 (st (seconds->samples beg))
	 (new-dur (or dur (- (mus-sound-duration file) orig-beg)))
	 (exA (make-granulate :input f :expansion exp-amt))
	 (nd (+ st (seconds->samples new-dur))))
    (run
     (do ((i st (+ i 1)))
	 ((= i nd))
       (outa i (granulate exA))))))

(definstrument (sndclmdoc-grev beg dur exp-amt file file-beg)
  (let ((exA (make-granulate :expansion exp-amt))
	(fil (make-file->sample file))
	(ctr file-beg))
    (run
     (do ((i beg (+ i 1)))
	 ((= i (+ beg dur)))
       (outa i (granulate exA
			  (lambda (dir)
			    (let ((inval (file->sample fil ctr 0)))
			      (if (> ctr 0) (set! ctr (- ctr 1)))
			      inval))))))))

(definstrument (sndclmdoc-simple-pvoc beg dur amp size file)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (sr (make-phase-vocoder (make-readin file) :fft-size size)))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp (phase-vocoder sr)))))))

(definstrument (sndclmdoc-asy beg dur freq amp index (r 1.0) (ratio 1.0))
  (let* ((st (seconds->samples beg))
         (nd (+ st (seconds->samples dur)))
         (asyf (make-asymmetric-fm :r r :ratio ratio :frequency freq)))
    (run
     (do ((i st (+ i 1))) 
	 ((= i nd))
       (outa i (* amp (asymmetric-fm asyf index 0.0)))))))

(define (sndclmdoc-simple-f2s beg dur amp file)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-file->sample file))
	 (ctr 0))
    (run
     (do ((i start (+ i 1))) ((= i end))
       (out-any i (* amp (file->sample fil ctr 0)) 0)
       (set! ctr (+ 1 ctr))))))

(definstrument (sndclmdoc-simple-ina beg dur amp file)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (fil (make-file->sample file)))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i 
             (* amp (in-any i 0 fil)) ; same as (ina i fil)
	     )))))

(definstrument (sndclmdoc-env-sound file beg (amp 1.0) (amp-env '(0 1 100 1)))
  (let* ((st (seconds->samples beg))
         (dur (mus-sound-duration file))
         (rev-amount .01)
         (rdA (make-readin file))
         (ampf (make-env amp-env amp dur))
         (nd (+ st (seconds->samples dur))))
    (run
     (do ((i st (+ i 1)))
	 ((= i nd))
       (let ((outval (* (env ampf) (readin rdA))))
	 (outa i outval)
	 (if *reverb* 
	     (outa i (* outval rev-amount) *reverb*)))))))

(definstrument (sndclmdoc-space file onset duration (distance-env '(0 1 100 10)) (amplitude-env '(0 1 100 1))
		     (degree-env '(0 45 50 0 100 90)) (reverb-amount .05))
  (let* ((beg (seconds->samples onset))
	 (end (+ beg (seconds->samples duration)))
         (loc (make-locsig :degree 0 :distance 1 :reverb reverb-amount))
         (rdA (make-readin :file file))
         (dist-env (make-env distance-env :duration duration))
         (amp-env (make-env amplitude-env :duration duration))
         (deg-env (make-env degree-env :scaler (/ 1.0 90.0) :duration duration))
         (dist-scaler 0.0))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (let ((rdval (* (readin rdA) (env amp-env)))
	     (degval (env deg-env))
	     (distval (env dist-env)))
	 (set! dist-scaler (/ 1.0 distval))
	 (locsig-set! loc 0 (* (- 1.0 degval) dist-scaler))
	 (if (> (channels *output*) 1)
	     (locsig-set! loc 1 (* degval dist-scaler)))
	 (if *reverb* 
	     (locsig-reverb-set! loc 0 (* reverb-amount (sqrt dist-scaler))))
	 (locsig loc i rdval))))))

(define (sndclmdoc-simple-dloc beg dur freq amp)
  "(simple-dloc-4 beg dur freq amp) test instrument for dlocsig"
  (let* ((os (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
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
				     (vector 0 1 2 3)))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (move-sound loc i (* amp (oscil os)))))))

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
       (do ((i beg (+ i 1)))
	   ((= i end))
	 (let ((gliss (env frqf)))
	   (outa i (* (env ampf) (wave-train grains gliss)))
	   (let ((click (pulse-train click-track gliss)))
	     (if (> click 0.0)
		 (let* ((scaler (max 0.1 (* 1.0 (/ (- i beg) len))))
			(comb-len 32)
			(c1 (make-comb scaler comb-len))
			(c2 (make-comb scaler (floor (* comb-len .75))))
			(c3 (make-comb scaler (floor (* comb-len 1.25)))))
		   (do ((k 0 (+ 1 k)))
		       ((= k grain-size))
		     (let ((x (original-grain k)))
		       (set! (grain k) (+ (comb c1 x) (comb c2 x) (comb c3 x))))))))))))))

(definstrument (move-formants start file amp radius move-env num-formants)
  (let* ((frms (make-vector num-formants))
	 (beg (seconds->samples start))
	 (dur (frames file))
	 (end (+ beg dur))
	 (rd (make-readin file))
	 (menv (make-env move-env :length dur)))
    (let ((start-frq (env menv)))
      (do ((i 0 (+ i 1)))
	  ((= i num-formants))
	(set! (frms i) (make-formant (* (+ i 1) start-frq) radius))))
    (run
     (do ((k beg (+ 1 k)))
	 ((= k end))
       (let ((sum 0.0)
	     (x (readin rd))
	     (frq (env menv)))
	 (do ((i 0 (+ i 1)))
	     ((= i num-formants))
	   (set! sum (+ sum (formant (frms i) x)))
	   (let ((curfrq (* (+ i 1) frq)))
	     (if (< (* 2 curfrq) (mus-srate))
		 (set! (mus-frequency (frms i)) curfrq))))
	 (outa k (* amp sum)))))))

(define (test-filter flt)
  (let* ((osc (make-oscil 0.0))
	 (samps (seconds->samples 0.5))
	 (ramp (make-env '(0 0 1 1) :scaler (hz->radians samps) :length samps)))
    (with-sound ()
      (do ((i 0 (+ i 1)))
	  ((= i samps))
        (outa i (flt (oscil osc (env ramp))))))))

(definstrument (flux start-time file frequency combs0 combs1 (scaler 0.99) (comb-len 32))
  (let* ((beg (seconds->samples start-time))
	 (len (frames file))
	 (end (+ beg len))
	 (num-combs0 (length combs0))
	 (num-combs1 (length combs1))
	 (cmbs0 (make-vector num-combs0))
	 (cmbs1 (make-vector num-combs1))
	 (osc (make-oscil frequency))
	 (rd (make-readin file)))
    (do ((k 0 (+ 1 k)))
	((= k num-combs0))
      (set! (cmbs0 k)
	    (make-comb scaler 
		       (floor (* comb-len (combs0 k))))))
    (do ((k 0 (+ 1 k)))
	((= k num-combs1))
      (set! (cmbs1 k)
	    (make-comb scaler 
		       (floor (* comb-len (combs1 k))))))
    (run
     (do ((i beg (+ i 1)))
	 ((= i end))
       (let ((interp (oscil osc))
	     (sum0 0.0)
	     (sum1 0.0)
	     (x (readin rd)))
	 (do ((k 0 (+ 1 k)))
	     ((= k num-combs0))
	   (set! sum0 (+ sum0 (comb (cmbs0 k) x))))
	 (do ((k 0 (+ 1 k)))
	     ((= k num-combs1))
	   (set! sum1 (+ sum1 (comb (cmbs1 k) x))))
	 (outa i (+ (* interp sum0) (* (- 1.0 interp) sum1))))))))





;;; ---------------- sndscm-osc ----------------

(defgenerator sndscm-osc freq phase) ;same as (defgenerator sndscm-osc (freq 0.0 :type float) (phase 0.0 :type float))

(define (sndscm-osc gen fm)
  (declare (gen sndscm-osc) (fm float))
  (let ((result (sin (sndscm-osc-phase gen))))
    (set! (sndscm-osc-phase gen) (+ (sndscm-osc-phase gen) (sndscm-osc-freq gen) fm))
    result))

(definstrument (sndscm-osc-fm beg dur freq amp mc-ratio fm-index)
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (carrier (make-sndscm-osc (hz->radians freq)))
	 (modulator (make-sndscm-osc (hz->radians (* mc-ratio freq))))
	 (index (hz->radians (* freq mc-ratio fm-index))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp (sndscm-osc carrier (* index (sndscm-osc modulator 0.0)))))))))




;;; ---------------- sndscm-osc1 ----------------

(defgenerator 
  (sndscm-osc1 :make-wrapper (lambda (gen)
			(set! (sndscm-osc1-freq gen) (hz->radians (sndscm-osc1-freq gen)))
			gen))
  (freq 0.0) (phase 0.0 :type float))

(define* (sndscm-osc1 gen fm)
  (declare (gen sndscm-osc1) (fm float))
  (let ((result (sin (sndscm-osc1-phase gen))))
    (set! (sndscm-osc1-phase gen) (+ (sndscm-osc1-phase gen) (sndscm-osc1-freq gen) fm))
    result))

(definstrument (sndscm-osc1-fm beg dur freq amp mc-ratio (fm-index 1.0))
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (carrier (make-sndscm-osc1 freq))
	 (modulator (make-sndscm-osc1 (* mc-ratio freq)))
	 (index (hz->radians (* freq mc-ratio fm-index))))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp (sndscm-osc1 carrier (* index (sndscm-osc1 modulator 0.0)))))))))




;;; ---------------- sndscm-osc2 ----------------

(defgenerator (sndscm-osc2 :make-wrapper (lambda (gen)
			       (set! (sndscm-osc2-freq gen) (hz->radians (sndscm-osc2-freq gen)))
			       gen)
			     :methods (list
				       (list 'mus-frequency 
					     (lambda (g) (radians->hz (sndscm-osc2-freq g)))
					     (lambda (g val) (set! (sndscm-osc2-freq g) (hz->radians val))))
				       
				       (list 'mus-phase 
					     (lambda (g) (sndscm-osc2-phase g))
					     (lambda (g val) (set! (sndscm-osc2-phase g) val)))
			      
				       (list 'mus-describe 
					     (lambda (g) (format #f "sndscm-osc2 freq: ~A, phase: ~A" 
								 (mus-frequency g) 
								 (mus-phase g))))))
  freq phase)

(define* (sndscm-osc2 gen fm)
  (declare (gen sndscm-osc2) (fm float))
  (let ((result (sin (sndscm-osc2-phase gen))))
    (set! (sndscm-osc2-phase gen) (+ (sndscm-osc2-phase gen) (sndscm-osc2-freq gen) fm))
    result))

(definstrument (sndscm-osc2-fm beg dur freq amp mc-ratio (fm-index 1.0))
  (let* ((start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (carrier (make-sndscm-osc2 freq))
	 (modulator (make-sndscm-osc2 (* mc-ratio freq)))
	 (index (hz->radians (* freq mc-ratio fm-index))))
    (if (fneq (mus-frequency carrier) freq)
	(format #t ";sndscm-osc2 (sclm23.scm) mus-frequency ~A: ~A ~A" (mus-describe carrier) (mus-frequency carrier) freq))
    (run
     (do ((i start (+ i 1)))
	 ((= i end))
       (outa i (* amp (sndscm-osc2 carrier (* index (sndscm-osc2 modulator 0.0)))))))))


;;; -------- asymmetric FM (bes-i0 case)

(defgenerator (dsp-asyfm :make-wrapper (lambda (gen)
				       (set! (dsp-asyfm-freq gen) (hz->radians (dsp-asyfm-freq gen)))
				       gen))
  freq (phase 0.0) (ratio 1.0) (r 1.0) (index 1.0))

(define (dsp-asyfm-J gen input)
  "(dsp-asyfm-J gen input) is the same as the CLM asymmetric-fm generator, set r != 1.0 to get the asymmetric spectra"
  (declare (gen dsp-asyfm) (input float))
  (let* ((phase (dsp-asyfm-phase gen))
	 (r (dsp-asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (dsp-asyfm-index gen))
	 (modphase (* (dsp-asyfm-ratio gen) phase))
	 (result (* (exp (* 0.5 index (- r r1) (cos modphase)))
		    (sin (+ phase (* 0.5 index (+ r r1) (sin modphase)))))))
    (set! (dsp-asyfm-phase gen) (+ phase input (dsp-asyfm-freq gen)))
    result))

(define (dsp-asyfm-I gen input)
  "(dsp-asyfm-I gen input) is the I0 case of the asymmetric-fm generator (dsp.scm)"
  (declare (gen dsp-asyfm) (input float))
  (let* ((phase (dsp-asyfm-phase gen))
	 (r (dsp-asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (dsp-asyfm-index gen))
	 (modphase (* (dsp-asyfm-ratio gen) phase))
	 (result (* (exp (- (* 0.5 index (+ r r1) (cos modphase))
			    (* 0.5 (log (bes-i0 (* index (+ r r1)))))))
		    (sin (+ phase (* 0.5 index (- r r1) (sin modphase)))))))
    (set! (dsp-asyfm-phase gen) (+ phase input (dsp-asyfm-freq gen)))
    result))


(defgenerator (sndclm-expcs 
		 :make-wrapper (lambda (g)
				 (if (<= (sndclm-expcs-et g) 0.0) (set! (sndclm-expcs-et g) 0.00001))
				 (set! (sndclm-expcs-frequency g) (hz->radians (sndclm-expcs-frequency g)))
				 (set! (sndclm-expcs-sinht g) (* 0.5 (sinh (sndclm-expcs-et g))))
				 (set! (sndclm-expcs-cosht g) (cosh (sndclm-expcs-et g)))
				 g))
  frequency phase et sinht cosht)

(define (sndclm-expcs gen fm)
  (declare (gen sndclm-expcs) (fm float))
  (let ((result (- (/ (sndclm-expcs-sinht gen) 
		      (- (sndclm-expcs-cosht gen) (cos (sndclm-expcs-phase gen))))
		   0.5)))
    (set! (sndclm-expcs-phase gen) (+ (sndclm-expcs-phase gen) (sndclm-expcs-frequency gen) fm))
    result))





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
		(do ((i 0 (+ i 1))) 
		    ((= i 10000)) 
		  (outa i (square-wave sqr)))))
  (with-sound () 
	      (run 
	       (lambda () 
		 (let ((osc (make-my-oscil 440.0)))
		   (do ((i 0 (+ i 1))) 
		       ((= i 22050))
		     (outa i (my-oscil osc 0.0)))))))
  (with-sound () (sndclmdoc-simp-5 0 10000 440 .1 '(0 0 1 1))) ; sweep up an octave
  (with-sound () (sndclmdoc-simple-fm 0 1 440 .1 2 1.0))
  (with-sound () (sndclmdoc-simple-add 0 1 220 .3))
  (with-sound () 
	      (sndclmdoc-mapenv 0 1 440 .4 '(0 0 50 1 75 0 86 .5 100 0)))
  (if (file-exists? "/home/bil/sf1/forest.aiff")
      (with-sound (:srate 44100) (sndclmdoc-looper 0 10 "/home/bil/sf1/forest.aiff" 1.0 0.5)))
  (with-sound ()
	      (sndclmdoc-bigbird 0 .05 1800 1800 .2
				 '(.00 .00 .40 1.00 .60 1.00 1.00 .0)         ; freq env
				 '(.00 .00 .25 1.00 .60 .70 .75 1.00 1.00 .0) ; amp env
				 '(1 .5 2 1 3 .5 4 .1 5 .01)))                ; bird song spectrum
  (with-sound () (sndclmdoc-pqw 0 1 200.0 1000.0 '(2 .2  3 .3  6 .5)))
  (with-sound (:srate 44100) (sndclmdoc-tritri 0 1 1000.0 0.5 0.1 0.01)) ; sci-fi laser gun
  (with-sound (:srate 44100) (sndclmdoc-tritri 0 1 4000.0 0.7 0.1 0.01)) ; a sparrow?
  (with-sound () (sndclmdoc-shift-pitch 0 3 "oboe.snd" 1108.0))
  (let* ((sound "oboe.snd")
	 (mx (maxamp sound))
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

  (with-sound ()
    (let ((gen (sndclmdoc-make-sum-of-odd-sines 440.0 10)))
      (sndclmdoc-sum-of-odd-sines gen 0.0))
    (let ((gen (sndclmdoc-make-sinc-train 440.0)))
      (sndclmdoc-sinc-train gen))
    (let ((gen (sndclmdoc-make-moving-max)))
      (sndclmdoc-moving-max gen 0.1))
    (sndclmdoc-asy 0 .1 440 .1 1.0)
    (sndclmdoc-simple-table 1000)
    (sndclmdoc-simple-f2s .1 .1 .1 "oboe.snd")
    (sndclmdoc-simp-2 .2 .1 440 .1)
    (sndclmdoc-fm-table "oboe.snd" .3 .1 .1 1.0 10.0 10)
    (sndclmdoc-bl-saw .5 .1 440 10))

  (with-sound (:channels 4)
	      (let ((loc (make-locsig))
		    (osc (make-oscil 440.0))
		    (j 0))
		(run  ; 360 notes one at each degree in a circle
		 (lambda ()
		   (do ((i 0 (+ i 1)))
		       ((= i 360))
		     (do ((k 0 (+ 1 k)))
			 ((= k 1000))
		       (let ((sig (* .5 (oscil osc))))
			 (locsig loc j sig)
			 (set! j (+ 1 j))))
		     (move-locsig loc (* 1.0 i) 1.0))))))
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

  (with-sound () (sndscm-osc-fm 0 1 440 .1 1 1))
  (with-sound () (sndscm-osc1-fm 0 1 440 .1 1))
  (with-sound () (sndscm-osc2-fm 0 1 440.0 .1 1))

  (with-sound () 
	      (let ((gen (make-dsp-asyfm :freq 2000 :ratio .1))) 
		(run 
		 (lambda () 
		   (do ((i 0 (+ i 1)))
		       ((= i 1000))
		     (outa i (dsp-asyfm-J gen 0.0)))))))
  
  (with-sound () 
	      (let ((gen (make-dsp-asyfm :freq 2000 :ratio .1))) 
		(run 
		 (lambda () 
		   (do ((i 0 (+ i 1)))
		       ((= i 1000))
		     (outa i (dsp-asyfm-I gen 0.0)))))))

  (with-sound ()
	      (let ((gen (make-sndclm-expcs :frequency 100 :et 1.0)))
		(run
		 (lambda ()
		   (do ((i 0 (+ i 1)))
		       ((= i 10000))
		     (outa i (sndclm-expcs gen 0.0)))))))

  (with-sound ()
	      (let ((gen (make-sndclm-expcs :frequency 100 :et 0.1))
		    (t-env (make-env '(0 .1 1 2) :length 10000)))
		(run 
		(do ((i 0 (+ i 1)))
		    ((= i 10000))
		  (let ((et (env t-env)))
		    (set! (sndclm-expcs-sinht gen) (* 0.5 (sinh et)))
		    (set! (sndclm-expcs-cosht gen) (cosh et))
		    (outa i (sndclm-expcs gen 0.0)))))))

  (for-each close-sound (sounds))
  )



