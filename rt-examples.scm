

;; Instructions:
;; 1. Start the jack sound server.
;; 2. (load-from-path "rt-compiler.scm")
;; 3. (load-from-path "rt-examples.scm")
;; 4. Browse the file and evaluate commented blocks.
;;
;; In case of stuck sounds, evaluate (rte-reset)
;; 
;; K.Matheussen 2005.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Realtime version of bird.scm.

(load-from-path "bird.scm")

(define bigbird-org bigbird)
(definstrument (bigbird-new start dur frequency freqskew amplitude freq-envelope amp-envelope partials)
  "(bigbird start dur frequency freqskew amplitude freq-envelope amp-envelope partials)"

  (define (sum-partials lst sum)
    (if (null? lst)
	sum
	(sum-partials (cddr lst) (+ sum (cadr lst)))))
    
  (define (scale-partials lst scl newlst)
    (if (null? lst)
	newlst
	(scale-partials (cddr lst) scl (append newlst (list (car lst) (* scl (cadr lst)))))))
    
  (define (normalize-partials lst)
    (scale-partials lst (/ 1.0 (sum-partials lst 0.0)) '()))
    
  (let* ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	 (os (make-polyshape frequency :coeffs (partials->polynomial (normalize-partials partials))))
	 (amp-env (make-env amp-envelope amplitude dur))
	 (beg (inexact->exact (round (* (mus-srate) start))))
	 (len (inexact->exact (round (* (mus-srate) dur))))
	 (end (+ beg len)))
    (ws-interrupt?)
    (rt-run start dur
	    (lambda ()
	      (out (* (env amp-env)
		      (polyshape os 1.0 (env gls-env))))))))

(define bird-org bird)
(definstrument (bird-new start dur frequency freqskew amplitude freq-envelope amp-envelope)
  "(bird start dur frequency freqskew amplitude freq-envelope amp-envelope)"
  (let* ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	 (os (make-oscil :frequency frequency))
	 (amp-env (make-env amp-envelope amplitude dur))
	 (len (inexact->exact (round (* (mus-srate) dur))))
	 (beg (inexact->exact (round (* (mus-srate) start))))
	 (end (+ beg len)))
    (ws-interrupt?)
    (rt-run start dur
	    (lambda ()
	      (out (* (env amp-env)
		      (oscil os (env gls-env))))))))
  

(define with-sound-org with-sound)
(defmacro with-sound-new (args . body)
  `(begin
     ,@body))
  
(define (make-rt-birds)
  (set! bird bird-new)
  (set! bigbird bigbird-new)
  (set! with-sound with-sound-new)

  (make-birds)
  
  (set! with-sound with-sound-org)
  (set! bird bird-org)
  (set! bigbird bigbird-org))



#!
(make-rt-birds)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI examples

(definstrument (make-osc-gui)
  (letrec* ((osc (make-oscil #:frequency 440))
	    (vol 0.4)
	    (instrument (rt-rt (lambda ()
				 (out (* vol (oscil osc))))))
	    (exit (lambda ()
		    (-> instrument stop)
		    (-> d hide)))
	    (d (<dialog> "Hard Realtime Common Lisp Music!"  exit
			 "Close" exit
			 "Stop" (<- instrument stop)
			 "Start" (<- instrument play))))
    (<slider> d "Frequency" 50 440 20000 (lambda (val) 
					   (set! (mus-frequency osc) val))
	      1)
    (<slider> d "Amplitude" 0 vol 2.0 (lambda (val) 
					(set! (-> instrument vol) val))
	      1000)
    (-> instrument play)
    (-> d show)))


(definstrument (make-fm-gui i)
  (letrec* ((freq i)
	    (amp 0.6)
	    (mc-ratio 0.1)
	    (index 4)
	    
	    (fm (make-oscil (* freq mc-ratio) :initial-phase (/ 3.14159 2.0)))
	    (carrier (make-oscil freq))
	    (fm_index (* (hz->radians freq) mc-ratio index))
	    
	    (instrument (rt-rt (lambda ()
				 (out (* amp
					 (oscil carrier (* fm_index
							   (oscil fm))))))))
	    (exit (lambda ()
		    (-> instrument stop)
		    (-> d hide)))
	    
	    (d (<dialog> "Hard Realtime Common Lisp Music!"  exit
			 "Close" exit
				"Stop" (<- instrument stop)
				"Start" (<- instrument play))))
	   (c-display "amp/fm_index" amp fm_index)
	   (<slider> d "Fm Frequency" 2 (mus-frequency fm) 1200 (lambda (val) 
								 (set! (mus-frequency fm) (* mc-ratio val)))
		     10)
	   (<slider> d "Carrier Frequency" 50 (mus-frequency carrier) 1200 (lambda (val) 
							  (set! (mus-frequency carrier) val))
		     10)
	   (<slider> d "Index" 0 (-> instrument fm_index) 50.0 (lambda (val) 
								 (set! (-> instrument fm_index) (* (hz->radians freq) mc-ratio val)))
		     100)
	   (<slider> d "Amplitude" 0 (-> instrument amp) 1.0 (lambda (val) 
							       (set! (-> instrument amp) val))
		     1000)
	   (-> instrument play)
	   (-> d show)))


#!
;; gui.scm requires gtk:
(c-load-from-path gui)

(make-osc-gui)
(make-fm-gui 200)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Oscilator

(definstrument (oscilator)
  (let ((osc (make-oscil))
	(vol 0.8))
    (rt-run 0 10
	    (lambda ()
	      (out (* (oscil osc)
		      vol))))))


#!
(define i (instrument))
(set! (-> i vol) 0.6)
(set! (mus-frequency (-> i osc)) 200)
(-> i stop)
#!




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fileplayers

(definstrument (loopplay filename)
  (let* ((rs (make-readin filename))
	 (player (rt-rt (lambda ()
			  (if (>= (mus-location rs) (mus-length rs))
			      (set! (mus-location rs) 0))
			  (out (readin rs))))))
    (-> player play)
    player))


(definstrument (backandforth-stereo filename pan)
  (let* ((read0 (make-readin filename #:channel 0))
	 (read1 (make-readin filename #:channel 1))
	 (player (rt-rt (lambda ()
			  (let ((readfunc (lambda (read)
					    (if (>= (mus-location read) (mus-length read))
						(set! (mus-increment read) -1)
						(if (<= (mus-location read) 0)
						    (set! (mus-increment read) 1)))
					    (readin read))))
			    ;; (Very stupid panner)
			    (out 0 (* (readfunc read0)
				      (if (< pan 0)
					  1
					  (- 1 pan))))
			    (out 1 (* (readfunc read1)
				      (if (> pan 0)
					  1
					  (+ 1 pan)))))))))
    (-> player play)
    player))



#!
(define filename "/home/kjetil/t1.wav")

(define p (loopplay filename))
(-> p stop)

(define p (backandforth-stereo filename 0))
(set! (-> p pan) -1)
(set! (-> p pan) 0)
(set! (-> p pan) 1)
(-> p stop)

!#

