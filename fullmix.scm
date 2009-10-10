(use-modules (ice-9 optargs))

(provide 'snd-fullmix.scm)

(if (and (not (provided? 'snd-ws.scm)) 
	 (not (provided? 'sndlib-ws.scm)))
    (load-from-path "ws.scm"))


(definstrument (fullmix in-file :optional beg outdur inbeg matrix srate reverb-amount)
  ;; "matrix" can be a simple amplitude or a list of lists
  ;;     each inner list represents one input channel's amps into one output channel
  ;;     each element of the list can be a number, a list (turned into an env) or an env
  (let* ((st (seconds->samples (or beg 0.0)))
	 (dur (or outdur
		  (/ (- (mus-sound-duration in-file) (or inbeg 0.0))
		     (or (and srate (abs srate)) 1.0))))
	 (samps (seconds->samples dur))
	 (nd (+ st samps))
	 (in-chans (mus-sound-chans in-file))
	 (inloc (inexact->exact (floor (* (or inbeg 0.0) (srate in-file)))))
	 (out-chans (mus-channels *output*))
	 (mx (if matrix
		 (make-mixer (max in-chans out-chans))
		 (make-scalar-mixer (max in-chans out-chans) 1.0)))
	 (rev-mx (if (and *reverb* reverb-amount (> reverb-amount 0.0))
		     (let ((rmx (make-mixer in-chans)))
		       (do ((i 0 (+ i 1)))
			   ((= i in-chans))
			 (mixer-set! rmx i 0 reverb-amount)) ; 0->assume 1 chan reverb stream, I think
		       rmx)
		     #f))
	 (revframe (if rev-mx (make-frame 1) #f))
	 (file (if (or (not srate) (= srate 1.0))
		   (make-file->frame in-file)
		   (let ((vect (make-vector in-chans #f)))
		     (do ((i 0 (+ i 1)))
			 ((= i in-chans))
		       (vector-set! vect i (make-readin in-file i inloc)))
		     vect)))
	 (envs #f))

    (if matrix
	(begin
	  (if (list? matrix) ; matrix is list of scalers, envelopes (lists), or env gens
	      (do ((inp 0 (+ 1 inp))
		   (off 0 (+ off out-chans)))
		  ((= inp in-chans))
		(let ((inlist (list-ref matrix inp)))
		  (do ((outp 0 (+ 1 outp)))
		      ((= outp out-chans))
		    (let ((outn (list-ref inlist outp)))
		      (if outn
			  (if (number? outn)
			      (mixer-set! mx inp outp outn)
			      (if (or (env? outn)
				      (list? outn))
				  (begin
				    (if (not envs)
					(set! envs (make-vector (* in-chans out-chans) #f)))
				    (if (env? outn)
					(vector-set! envs (+ off outp) outn)
					(vector-set! envs (+ off outp) (make-env outn :duration dur))))
				  (snd-warning (format #f "unknown element in matrix: ~A" outn)))))))))
	      (do ((inp 0 (+ 1 inp))) ; matrix is a number in this case (a global scaler)
		  ((= inp in-chans))
		(if (< inp out-chans)
		    (mixer-set! mx inp inp matrix))))))

    (if (or (not srate)
	    (= srate 1.0))
	(begin
	  ;; -------- no src
	  (mus-mix *output* file st samps inloc mx (if envs
						       (let ((v (make-vector in-chans)))
							 (do ((i 0 (+ i 1))
							      (off 0 (+ off out-chans)))
							     ((= i in-chans))
							   (let ((vo (make-vector out-chans #f)))
							     (vector-set! v i vo)
							     (do ((j 0 (+ j 1)))
								 ((= j out-chans))
							       (vector-set! vo j (vector-ref envs (+ off j))))))
							 v)
						       envs))
	  (if rev-mx
	      (mus-mix *reverb* file st samps inloc rev-mx #f)))

	;; -------- with src
	;; unroll the loops if 1 chan input
	(if (= in-chans 1)
	    (let ((sr (make-src :input (vector-ref file 0) :srate srate))
		  (outframe (make-frame out-chans)))
	      (if envs
		  (run 
		   (lambda ()
		     (declare (envs clm-vector))
		     (do ((i st (+ i 1)))
			 ((= i nd))
		       (do ((outp 0 (+ 1 outp)))
			   ((= outp out-chans))
			 (if (env? (vector-ref envs outp))
			     (mixer-set! mx 0 outp (env (vector-ref envs outp)))))
		       (let ((inframe (src sr)))
			 (frame->file *output* i (sample->frame mx inframe outframe))
			 (if rev-mx (frame->file *reverb* i (sample->frame rev-mx inframe revframe)))))))
		  
		  ;; no envs
		  (run 
		   (lambda ()
		     (do ((i st (+ i 1)))
			 ((= i nd))
		       (let ((inframe (src sr)))
			 (frame->file *output* i (sample->frame mx inframe outframe))
			 (if rev-mx (frame->file *reverb* i (sample->frame rev-mx inframe revframe)))))))))
	    
	    ;; more than 1 chan input
	    (let* ((inframe (make-frame in-chans))
		   (outframe (make-frame out-chans))
		   (srcs (make-vector in-chans #f)))
	      (do ((inp 0 (+ 1 inp)))
		  ((= inp in-chans))
		(vector-set! srcs inp (make-src :input (vector-ref file inp) :srate srate)))
	      
	      (if envs 
		  (run
		   (lambda ()
		     (declare (envs clm-vector))
		     (do ((i st (+ i 1)))
			 ((= i nd))
		       (do ((inp 0 (+ 1 inp))
			    (off 0 (+ off out-chans)))
			   ((= inp in-chans))
			 (do ((outp 0 (+ 1 outp)))
			     ((= outp out-chans))
			   (if (env? (vector-ref envs (+ off outp)))
			       (mixer-set! mx inp outp (env (vector-ref envs (+ off outp)))))))
		       (do ((inp 0 (+ 1 inp)))
			   ((= inp in-chans))
			 (frame-set! inframe inp (src (vector-ref srcs inp))))
		       (frame->file *output* i (frame->frame inframe mx outframe))
		       (if rev-mx (frame->file *reverb* i (frame->frame inframe rev-mx revframe))))))
		  
		  ;; no envs
		  (run 
		   (lambda ()
		     (do ((i st (+ i 1)))
			 ((= i nd))
		       (do ((inp 0 (+ 1 inp)))
			   ((= inp in-chans))
			 (frame-set! inframe inp (src (vector-ref srcs inp))))
		       (frame->file *output* i (frame->frame inframe mx outframe))
		       (if rev-mx (frame->file *reverb* i (frame->frame inframe rev-mx revframe))))))))))))
  
#|
(with-sound (:channels 2 :statistics #t)
  (fullmix "pistol.snd")
  (fullmix "2.snd" .5 1)
  (fullmix "2.snd" 1.5 1 0 #f 2.0)
  (fullmix "oboe.snd" 1 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5))))
  (fullmix "pistol.snd" 2 1 0 #f .5)
  (fullmix "2.snd" 0 2 0 (list (list .1 .2) (list .3 .4)) 2.0)
  (fullmix "oboe.snd" 3 2 0 (list (list .1 (make-env '(0 0 1 1) :duration 2 :scaler .5))) .25)
  (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	(e0->1 (make-env '(0 1 1 0) :duration 2))
	(e1->0 (make-env '(0 1 1 0) :duration 2))
	(e1->1 (make-env '(0 0 1 1) :duration 2)))
    (fullmix "2.snd" 4 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1))))
  (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	(e0->1 (make-env '(0 1 1 0) :duration 2))
	(e1->0 (make-env '(0 1 1 0) :duration 2))
	(e1->1 (make-env '(0 0 1 1) :duration 2)))
    (fullmix "2.snd" 6 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1)) 2.0)))

(with-sound (:channels 2 :statistics #t)
  (fullmix "2.snd" 0 2 0 (list (list .1 .2) (list .3 .4)) 2.0))

(with-sound (:channels 2)
  (let ((e0->0 (make-env '(0 0 1 1) :duration 2))
	(e0->1 (make-env '(0 1 1 0) :duration 2))
	(e1->0 (make-env '(0 1 1 0) :duration 2))
	(e1->1 (make-env '(0 0 1 1) :duration 2)))
    (fullmix "2.snd" 6 2 0 (list (list e0->0 e0->1) (list e1->0 e1->1))) 2.0))

  
(with-sound () (fullmix "pistol.snd"))
(with-sound () (fullmix "pistol.snd" 1))
(with-sound () (fullmix "pistol.snd" 1 1))
(with-sound () (fullmix "pistol.snd" 0 1 1))
(with-sound (:statistics #t) (fullmix "pistol.snd" 0 1 0 2.0))
(with-sound (:statistics #t :channels 2) (fullmix "pistol.snd" 0 1 0 2.0))
(with-sound (:statistics #t :channels 2) (fullmix "pistol.snd" 0 1 0 (list (list 0.1 0.7))))
(with-sound (:statistics #t :channels 2) (fullmix "pistol.snd" 0 1 0 (list (list 0.1 (list 0 0 1 1)))))

(with-sound (:channels 2 :output "one-2.snd") (do ((i 0 (+ i 1))) ((= i 10000)) (outa i 0.5) (outb i -0.5)))
(with-sound (:channels 4 :output "one-4.snd") (do ((i 0 (+ i 1))) ((= i 10000)) (outa i 0.5) (outb i -0.5) (outc i 0.1) (outd i -0.1)))

(with-sound (:statistics #t :channels 2) (fullmix "one-2.snd" 0 .2 0 '((1.0 0.5) (0.5 1.0))))
(with-sound (:statistics #t :channels 2) (fullmix "one-2.snd" 0 .2 0 (list (list 0.1 (list 0 0 1 1)) (list (list 0 1 1  0) .5))))
(with-sound (:statistics #t :channels 2) 
  (let ((e0->0 (make-env '(0 0 1 1) :end 10000))
	(e0->1 (make-env '(0 1 1 0) :end 10000))
	(e1->0 (make-env '(0 1 1 0) :end 10000))
	(e1->1 (make-env '(0 0 1 1) :end 10000)))
    (fullmix "one-2.snd" 0 .2 0 (list (list e0->0 e0->1) (list e1->0 e1->1)))))


(with-sound (:statistics #t :channels 2 :reverb jc-reverb) 
  (let ((e0->0 (make-env '(0 0 1 1) :end 10000))
	(e0->1 (make-env '(0 1 1 0) :end 10000))
	(e1->0 (make-env '(0 1 1 0) :end 10000))
	(e1->1 (make-env '(0 0 1 1) :end 10000)))
    (fullmix "one-2.snd" 0 .2 0 (list (list e0->0 e0->1) (list e1->0 e1->1)) #f .1)))
|#
