;;; multi-channel sound file expansion with srate and reverb.
;;; michael klingbeil (michael@klingbeil.com)
;;;
;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2005/10/16 22:15:44 $
;;;
;;; clm-4 and scheme May-08 bil
;;; split out cases to optimize May-09 bil

(provide 'snd-expandn.scm)

(if (provided? 'snd)
    (if (not (provided? 'snd-ws.scm)) (load "ws.scm"))
    (if (not (provided? 'sndlib-ws.scm)) (load "sndlib-ws.scm")))
(if (not (provided? 'snd-env.scm)) (load "env.scm")) ; min-envelope, max-envelope


(definstrument (expandn time duration filename amplitude
			(expand 1.0)
			(matrix #f)
			(ramp 0.4)
			(seglen 0.15)
			(srate 1.0)
			(hop .05)
			(amp-env '(0 0 50 1 100 0))
			(input-start 0.0)
			(grain-amp 0.8)
			(reverb #f))

  (let ((fnam (file-name filename)))
    (if (not (file-exists? fnam))
	(error 'no-such-file (list 'expandn filename))

	(let* ((beg (seconds->samples time))
	       (end (+ beg (seconds->samples duration)))
	       (min-exp-amt (if (list? expand) (min-envelope expand) expand))
	       (max-out-hop (if (list? hop) (max-envelope hop) hop)))
	  
	  (let ((in-chans (channels fnam))
		(out-chans (channels *output*))
		(rev-chans (if *reverb* (channels *reverb*) 0)))
	    
	    (let ((update-rate 100)
		  (ochans (max in-chans out-chans))
		  (max-seg-len (if (list? seglen) (max-envelope seglen) seglen))
		  (rampdata (if (list? ramp) ramp (list 0 ramp 1 ramp)))
		  (start (floor (* input-start (mus-sound-srate fnam))))
		  (max-in-hop (/ max-out-hop min-exp-amt))
		  (rev-mx (if (and *reverb* reverb (> reverb 0.0))
			      (let* ((rchans (max out-chans rev-chans))
				     (rmx (make-vector (list rchans rchans) 0.0 #t)))
				(do ((i 0 (+ i 1)))
				    ((= i rchans))
				  (set! (rmx i i reverb)))
				rmx)
			      #f)))
	      
	      (let ((mx (let ((v (make-vector (list ochans ochans) 0.0 #t)))
			  (if (pair? matrix)
			      (let ((mat-in (min ochans (length matrix)))
				    (mat-out (min ochans (length (car matrix)))))
				(do ((inp 0 (+ inp 1)))
				    ((= inp mat-in))
				  (do ((outp 0 (+ outp 1)))
				      ((= outp mat-out))
				    (set! (v inp outp) (matrix inp outp)))))
			      (do ((i 0 (+ i 1)))
				  ((= i ochans))
				(set! (v i i) 1.0)))
			  v))
		    
		    (revvals (if rev-mx (make-vector (max out-chans rev-chans) 0.0 #t) #f))
		    (update-envs (or (list? expand)
				     (list? seglen)
				     (list? ramp)
				     (list? hop)))
		    (update-ctr 0)
		    (expenv (make-env (if (list? expand) expand (list 0 expand 1 expand))
				      :duration (/ duration update-rate)))
		    (lenenv (make-env (if (list? seglen) seglen (list 0 seglen 1 seglen))
				      :duration (/ duration update-rate)))
		    (segment-scaler (if (> max-seg-len .15)
					(/ (* grain-amp .15) max-seg-len)
					grain-amp))
		    (srenv (if (list? srate) 
			       (make-env srate :duration duration) 
			       (make-env (list 0 srate) :duration duration)))
		    (rampenv (make-env rampdata :duration (/ duration update-rate)))
		    (minramp-bug (<= (min-envelope rampdata) 0.0))
		    (maxramp-bug (>= (max-envelope rampdata) 0.5))
		    (hopenv (make-env (if (list? hop) hop (list 0 hop 1 hop))
				      :duration (/ duration update-rate)))
		    (ampenv (make-env amp-env :duration duration :scaler amplitude))
		    (ex-array (make-vector in-chans #f))
		    
		    (max-len (ceiling (* *clm-srate*
					 (+ (max max-out-hop max-in-hop)
					    max-seg-len))))
		    (invals (make-vector ochans 0.0 #t))
		    (outvals (make-vector ochans 0.0 #t)))
		
		(if (or minramp-bug maxramp-bug)
		    (error 'out-of-range (list expand 
					       "ramp argument to expandn must always be "
					       (if (and minramp-bug maxramp-bug) "between 0.0 and 0.5"
						   (if minramp-bug "greater than 0.0"
						       "less than 0.5")))))
		
		;; setup granulate generators
		(do ((i 0 (+ i 1)))
		    ((= i in-chans))
		  (vector-set! ex-array i (make-granulate :input (make-readin fnam :start start :channel i)
							  :expansion (if (list? expand) (cadr expand) expand)
							  :max-size max-len
							  :ramp (if (list? ramp) (cadr ramp) ramp)
							  :hop (if (list? hop) (cadr hop) hop)
							  :length (if (list? seglen) (cadr seglen) seglen)
							  :scaler segment-scaler)))
		;; split out 1 and 2 chan input 
		(if (= in-chans 1)
		    (let ((ingen (vector-ref ex-array 0))
			  (sample-0 0.0)
			  (sample-1 0.0)
			  (ex-samp -1.0)
			  ;; these vars used for resampling
			  (next-samp 0.0))

		      (if (and (not (list? srate))
			       (not update-envs)
			       (= out-chans 1)
			       (not matrix)
			       (not rev-mx))

			  (let ((file-end (+ beg (seconds->samples (+ (* 2 seglen) 
								      (* (mus-sound-duration fnam) 
									 (/ (mus-sound-srate fnam) *clm-srate*)
									 (/ expand srate)))))))
			    (if (> end file-end)
				(set! end file-end))
			  
			    (do ((i beg (+ i 1)))
				((= i end))
			      
			      (let ((vol (env ampenv)))
				(if (negative? ex-samp)
				    (begin
				      (set! sample-0 (* vol (granulate ingen)))
				      (set! sample-1 (* vol (granulate ingen)))
				      (set! ex-samp (+ ex-samp 1))
				      (set! next-samp ex-samp)
				      (outa i sample-0))
				    (begin
				      (set! next-samp (+ next-samp srate))
				      (if (> next-samp (+ ex-samp 1))
					  (let ((samps (floor (- next-samp ex-samp))))
					    (if (= samps 2)
						(begin
						  (set! sample-0 (* vol (granulate ingen)))
						  (set! sample-1 (* vol (granulate ingen))))
						(do ((k 0 (+ k 1)))
						    ((= k samps))
						  (set! sample-0 sample-1)
						  (set! sample-1 (* vol (granulate ingen)))))
					    (set! ex-samp (+ ex-samp samps))))
				      (if (= next-samp ex-samp) 
					  (outa i sample-0) 
					  (outa i (+ sample-0 (* (- next-samp ex-samp) (- sample-1 sample-0))))))))))
			  
			  (do ((i beg (+ i 1)))
			      ((= i end))
			    
			    (let ((vol (env ampenv))
				  (resa (env srenv)))
			      
			      (if update-envs
				  (begin
				    (set! update-ctr (+ update-ctr 1))
				    (if (>= update-ctr update-rate)
					(let ((sl (floor (* (env lenenv) *clm-srate*))))
					  (set! update-ctr 0)
					  (set! (mus-length ingen) sl)
					  (set! (mus-ramp ingen) (floor (* sl (env rampenv))))
					  (set! (mus-frequency ingen) (env hopenv))
					  (set! (mus-increment ingen) (env expenv))))))
			      
			      (if (negative? ex-samp)
				  (begin
				    (set! sample-0 (* vol (granulate ingen)))
				    (set! sample-1 (* vol (granulate ingen)))
				    (set! ex-samp (+ ex-samp 1))
				    (set! next-samp ex-samp))
				  (begin
				    (set! next-samp (+ next-samp resa))
				    (if (> next-samp (+ ex-samp 1))
					(let ((samps (floor (- next-samp ex-samp))))
					  (if (= samps 2)
					      (begin
						(set! sample-0 (* vol (granulate ingen)))
						(set! sample-1 (* vol (granulate ingen))))
					      (do ((k 0 (+ k 1)))
						  ((= k samps))
						(set! sample-0 sample-1)
						(set! sample-1 (* vol (granulate ingen)))))
					  (set! ex-samp (+ ex-samp samps))))))
			      
			      (if (= next-samp ex-samp)
				  ;; output actual samples
				  (set! (invals 0) sample-0)
				  ;; output interpolated samples
				  (set! (invals 0) (+ sample-0 (* (- next-samp ex-samp) (- sample-1 sample-0)))))
			      
			      ;; output mixed result
			      (float-vector->file *output* i (float-vector-mix invals mx outvals))
			      ;; if reverb is turned on, output to the reverb streams
			      (if rev-mx
				  (float-vector->file *reverb* i (float-vector-mix outvals rev-mx revvals)))))))
		    
		    (if (= in-chans 2)
			(let ((sample-0-0 0.0)
			      (sample-1-0 0.0)
			      (sample-0-1 0.0)
			      (sample-1-1 0.0)
			      (ingen0 (vector-ref ex-array 0))
			      (ingen1 (vector-ref ex-array 1))
			      (ex-samp -1.0)
			      ;; these vars used for resampling
			      (next-samp 0.0))
			  (do ((i beg (+ i 1)))
			      ((= i end))
			    
			    (let ((vol (env ampenv))
				  (resa (env srenv)))
			      
			      (if update-envs
				  (begin
				    (set! update-ctr (+ update-ctr 1))
				    (if (>= update-ctr update-rate)
					(let ((expa (env expenv))                ;current expansion amount
					      (segl (env lenenv))                ;current segment length
					      (rmpl (env rampenv))               ;current ramp length (0 to .5)
					      (hp (env hopenv)))                 ;current hop size
					  (let* ((sl (floor (* segl *clm-srate*)))
						 (rl (floor (* rmpl sl))))
					    (set! update-ctr 0)
					    (set! (mus-length ingen0) sl)
					    (set! (mus-ramp ingen0) rl)
					    (set! (mus-frequency ingen0) hp)
					    (set! (mus-increment ingen0) expa)
					    (set! (mus-length ingen1) sl)
					    (set! (mus-ramp ingen1) rl)
					    (set! (mus-frequency ingen1) hp)
					    (set! (mus-increment ingen1) expa))))))
			      
			      (if (negative? ex-samp)
				  (begin
				    (set! sample-0-0 (* vol (granulate ingen0)))
				    (set! sample-1-0 (* vol (granulate ingen0)))
				    (set! sample-0-1 (* vol (granulate ingen1)))
				    (set! sample-1-1 (* vol (granulate ingen1)))
				    (set! ex-samp (+ ex-samp 1))
				    (set! next-samp ex-samp))
				  (begin
				    (set! next-samp (+ next-samp resa))
				    (if (> next-samp (+ ex-samp 1))
					(let ((samps (floor (- next-samp ex-samp))))
					  (do ((k 0 (+ k 1)))
					      ((= k samps))
					    (set! sample-0-0 sample-1-0)
					    (set! sample-1-0 (* vol (granulate ingen0)))
					    (set! sample-0-1 sample-1-1)
					    (set! sample-1-1 (* vol (granulate ingen1))))
					  (set! ex-samp (+ ex-samp samps))))))
			      
			      (if (= next-samp ex-samp)
				  ;; output actual samples
				  (begin
				    (set! (invals 0) sample-0-0)
				    (set! (invals 1) sample-0-1))
				  (begin
				    ;; output interpolated samples
				    (set! (invals 0) (+ sample-0-0 (* (- next-samp ex-samp) (- sample-1-0 sample-0-0))))
				    (set! (invals 1) (+ sample-0-1 (* (- next-samp ex-samp) (- sample-1-1 sample-0-1))))))
			      
			      ;; output mixed result
			      (float-vector->file *output* i (float-vector-mix invals mx outvals))
			      ;; if reverb is turned on, output to the reverb streams
			      (if rev-mx
				  (float-vector->file *reverb* i (float-vector-mix outvals rev-mx revvals))))))
			
			(let ((samples-0 (make-vector in-chans 0.0))
			      (samples-1 (make-vector in-chans 0.0)))
			  ;; more than 2 chans in input file
			  (do ((i beg (+ i 1)))
			      ((= i end))
			    (let ((vol (env ampenv))
				  (resa (env srenv)))
			      
			      (if update-envs
				  (begin
				    (set! update-ctr (+ update-ctr 1))
				    (if (>= update-ctr update-rate)
					(let ((expa (env expenv))                ;current expansion amount
					      (segl (env lenenv))                ;current segment length
					      (rmpl (env rampenv))               ;current ramp length (0 to .5)
					      (hp (env hopenv)))                 ;current hop size
					  (let* ((sl (floor (* segl *clm-srate*)))
						 (rl (floor (* rmpl sl))))
					    (set! update-ctr 0)
					    (do ((ix 0 (+ ix 1)))
						((= ix in-chans))
					      (let ((gen (vector-ref ex-array ix)))
						(set! (mus-length gen) sl)
						(set! (mus-ramp gen) rl)
						(set! (mus-frequency gen) hp)
						(set! (mus-increment gen) expa))))))))
			      
			      (if (negative? ex-samp)
				  (begin
				    (do ((ix 0 (+ ix 1)))
					((= ix in-chans))
				      (let ((gen (vector-ref ex-array ix)))
					(vector-set! samples-0 ix (* vol (granulate gen)))
					(vector-set! samples-1 ix (* vol (granulate gen)))))
				    (set! ex-samp (+ ex-samp 1))
				    (set! next-samp ex-samp))
				  (begin
				    (set! next-samp (+ next-samp resa))
				    (if (> next-samp (+ ex-samp 1))
					(let ((samps (floor (- next-samp ex-samp))))
					  (do ((k 0 (+ k 1)))
					      ((= k samps))
					    (do ((ix 0 (+ ix 1)))
						((= ix in-chans))
					      (let ((gen (vector-ref ex-array ix)))
						(vector-set! samples-0 ix (vector-ref samples-1 ix))
						(vector-set! samples-1 ix (* vol (granulate gen))))))
					  (set! ex-samp (+ ex-samp samps))))))
			      
			      (if (= next-samp ex-samp)
				  ;; output actual samples
				  (do ((ix 0 (+ ix 1)))
				      ((= ix in-chans))
				    (set! (invals ix) (vector-ref samples-0 ix)))
				  ;; output interpolated samples
				  (do ((ix 0 (+ ix 1)))
				      ((= ix in-chans))
				    (let ((v0 (vector-ref samples-0 ix))
					  (v1 (vector-ref samples-1 ix)))
				      (set! (invals ix) (+ v0 (* (- next-samp ex-samp) (- v1 v0)))))))
			      ;; output mixed result
			      (float-vector->file *output* i (float-vector-mix invals mx outvals))
			      ;; if reverb is turned on, output to the reverb streams
			      (if rev-mx
				  (float-vector->file *reverb* i (float-vector-mix outvals rev-mx revvals)))))))))))))))

;;; (with-sound () (expandn 0 1 "oboe.snd" 1 :expand 4))
