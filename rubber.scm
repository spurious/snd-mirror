;;; rubber.scm: rubber-sound stretches or contracts a sound (in time)
;;;   (rubber-sound 1.5) makes it 50% longer
;;;   rubber-sound looks for stable portions and either inserts or deletes periods 
;;;     period length is determined via autocorrelation

;;; TODO: no-vib (local resample), more-vib
;;; TODO: re-pitch (local resample, same end length)
;;; TODO: re-orch (cross-fade within steady state) -- could end up with one voice's consonants and another's vowels etc

(define zeros-checked 8)
(define extension 10.0)

;;; remove anything below 16Hz
;;; extend (src by 1/extension)
;;; collect upward zero-crossings
;;;   collect weights for each across next zeros-checked crossings
;;;   sort by least weight
;;;   ramp (out or in) and check if done

(define derumble-sound
  ;; remove rumbles and DC etc (since we're using zero crossings to find period starts)
  (lambda args
    (let* ((snd (if (not (null? args)) (car args) #f))
	   (chn (if (and (not (null? args)) (> (length args) 1)) (cadr args) #f))
	   (old-length (frames snd chn))
	   (pow2 (ceiling (/ (log (min old-length (srate snd))) (log 2))))
	   (fftlen (inexact->exact (expt 2 pow2)))
	   (flt-env (list 0.0 0.0 (/ (* 2 16.0) (srate snd)) 0.0 (/ (* 2 20.0) (srate snd)) 1.0 1.0 1.0)))
      (filter-sound flt-env fftlen snd chn)
      (set! (frames snd chn) old-length))))

(define sample-sound
  ;; prepare sound for analysis by interpolating samples
  (lambda args
    (let* ((snd (if (not (null? args)) (car args) #f))
	   (chn (if (and (not (null? args)) (> (length args) 1)) (cadr args) #f)))
      (if (not (= extension 1.0))
	  (src-sound (/ 1.0 extension) 1.0 snd chn)))))

(define unsample-sound
  ;; undo earlier interpolation
  (lambda args
    (let* ((snd (if (not (null? args)) (car args) #f))
	   (chn (if (and (not (null? args)) (> (length args) 1)) (cadr args) #f)))
      (if (not (= extension 1.0))
	  (src-sound extension 1.0 snd chn)))))

(define (crossings)
  ;; return number of upward zero crossings that don't look like silence
  (let* ((crosses 0)
	 (sr0 (make-sample-reader 0))
	 (samp0 (next-sample sr0))
	 (len (frames))
	 (sum 0.0)
	 (last-cross 0)
	 (silence (* extension .001)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i len))
	 (let ((samp1 (next-sample sr0)))
	   (if (and (<= samp0 0.0)
		    (> samp1 0.0))
	       (if (and (> (- i last-cross) 4)
			(> sum silence))
		   (begin
		     (set! crosses (+ crosses 1))
		     (set! last-cross i)
		     (set! sum 0.0))))
	   (set! sum (+ sum (abs samp0)))
	   (set! samp0 samp1)))))
    crosses))

(define env-add
  (lambda (s0 s1 samps)
    (let ((data (make-vct samps))
	  (x 1.0)
	  (xinc (/ 1.0 samps))
	  (sr0 (make-sample-reader (inexact->exact s0)))
	  (sr1 (make-sample-reader (inexact->exact s1))))
      (run
       (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i samps))
	(vct-set! data i (+ (* x (next-sample sr0))
			    (* (- 1.0 x) (next-sample sr1))))
	(set! x (+ x xinc)))))
      data)))

(define rubber-sound-1
  (lambda args
    (let* ((stretch (car args))
	   (snd (if (> (length args) 1) (list-ref args 1) #f))
	   (chn (if (> (length args) 2) (list-ref args 2) #f)))
      
      ;; prepare sound (get rid of low freqs, resample)
      
      (as-one-edit
       (lambda ()
	 (derumble-sound snd chn)
	 (sample-sound snd chn)
	 
	 (let* ((crosses (crossings))
		(cross-samples (make-vct crosses))
					;(cross-maxes (make-vct crosses))
					;(cross-to-max-times (make-vct crosses))
		(cross-weights (make-vct crosses))
		(cross-marks (make-vct crosses))
					;(cross-ids (make-vct crosses))
		(cross-periods (make-vct crosses)))
	   (run
	    (lambda ()
	      (let* ((sr0 (make-sample-reader 0 snd chn)) ;; get cross points (sample numbers)
		     (samp0 (next-sample sr0))
		     (len (frames))
		     (sum 0.0)
		     (last-cross 0)
		     (cross 0)
		     (silences 0)
		     (silence (* extension .001)))
		(do ((i 0 (1+ i)))
		    ((= i len))
		  (let ((samp1 (next-sample sr0)))
		    (if (and (<= samp0 0.0)
			     (> samp1 0.0)
			     (> (- i last-cross) 4)
			     (> sum silence))
			(begin
			  (set! last-cross i)
			  (set! sum 0.0)
			  (vct-set! cross-samples cross i)
			  (set! cross (+ cross 1))))
		    (set! sum (+ sum (abs samp0)))
		    (set! samp0 samp1))))))
	   
	   ;; now run through crosses getting period match info
	   (run
	    (lambda ()
	      (do ((i 0 (1+ i)))
		  ((= i (1- crosses)))
		(let* ((start (vct-ref cross-samples i))
		       (autolen 0))
		  (let* ((s0 start)
			 (pow2 (ceiling (/ (log (* extension (/ (srate) 40.0))) (log 2))))
			 (fftlen (inexact->exact (expt 2 pow2)))
			 (data (make-vct fftlen))
			 (reader (make-sample-reader (inexact->exact s0))))
		    (do ((i 0 (1+ i)))
			((= i fftlen))
		      (let ((val (next-sample reader)))
			(vct-set! data i val)))
		    (autocorrelate data)
		    (set! autolen 0)
		    (let ((happy #f))
		      (do ((i 1 (1+ i)))
			  ((or happy (= i 2046)))
			(if (and (< (vct-ref data i) (vct-ref data (+ i 1)))
				 (> (vct-ref data (+ i 1)) (vct-ref data (+ i 2))))
			    (begin
			      (set! autolen (* i 2))
			      (set! happy #t))))))
		  (let* ((next-start (+ start autolen))
			 (min-i (+ i 1))
			 (min-samps (abs (- (vct-ref cross-samples min-i) next-start))))
		    (do ((k (+ i 2) (1+ k)))
			((= k (min crosses (+ i zeros-checked))))
		      (let ((dist (abs (- (vct-ref cross-samples k) next-start))))
			(if (< dist min-samps)
			    (begin
			      (set! min-samps dist)
			      (set! min-i k)))))
		    (let* ((current-mark min-i)
			   (current-min 0.0))
		      (let* ((s0 start)
			     (s1 (vct-ref cross-samples current-mark))
			     (len autolen)
			     (sr0 (make-sample-reader (inexact->exact s0)))
			     (sr1 (make-sample-reader (inexact->exact s1)))
			     (ampsum 0.0)
			     (diffsum 0.0))
			(do ((i 0 (1+ i)))
			    ((= i len))
			  (let ((samp0 (next-sample sr0))
				(samp1 (next-sample sr1)))
			    (set! ampsum (+ ampsum (abs samp0)))
			    (set! diffsum (+ diffsum (abs (- samp1 samp0))))))
			(if (= diffsum 0.0)
			    (set! current-min 0.0)
			    (set! current-min (/ diffsum ampsum))))
		      (set! min-samps (* 0.5 current-min))
		      (let ((top (min (1- crosses) current-mark (+ i zeros-checked))))
			(do ((k (+ i 1) (1+ k)))
			    ((= k top))
			  (let ((wgt 0.0))
			    (let* ((s0 start)
				   (s1 (vct-ref cross-samples k))
				   (len autolen)
				   (sr0 (make-sample-reader (inexact->exact s0)))
				   (sr1 (make-sample-reader (inexact->exact s1)))
				   (ampsum 0.0)
				   (diffsum 0.0))
			      (do ((i 0 (1+ i)))
				  ((= i len))
				(let ((samp0 (next-sample sr0))
				      (samp1 (next-sample sr1)))
				  (set! ampsum (+ ampsum (abs samp0)))
				  (set! diffsum (+ diffsum (abs (- samp1 samp0))))))
			      (if (= diffsum 0.0)
				  (set! wgt 0.0)
				  (set! wgt (/ diffsum ampsum))))
			    (if (< wgt min-samps)
				(begin
				  (set! min-samps wgt)
				  (set! min-i k))))))
		      (if (not (= current-mark min-i))
			  (begin
			    ;; these are confused, so effectively erase them
			    (vct-set! cross-weights i 1000.0)
			    )
			  (begin
			    (vct-set! cross-weights i current-min)
			    (vct-set! cross-marks i current-mark)
			    (vct-set! cross-periods i (- (vct-ref cross-samples current-mark) (vct-ref cross-samples i)))
			    ))
		      ))
		  ))))
	   ;; now sort weights to scatter the changes as evenly as possible
	   (let* ((len (frames snd chn))
		  (adding (> stretch 1.0))
		  (samps (inexact->exact (* (abs (- stretch 1.0)) len)))
		  (handled 0)
		  (curs 0)
		  (edits (make-vct (vct-length cross-weights))))
	     (run (lambda ()
		    (do ()
			((>= handled samps))
		      ;; need to find enough splice points to add/delete samps
		      (let ((best-mark -1)
			    (old-handled handled))
			(let ((cur 0)
			      (curmin (vct-ref cross-weights 0))
			      (len (vct-length cross-weights)))
			  (do ((i 0 (1+ i)))
			      ((= i len))
			    (if (< (vct-ref cross-weights i) curmin)
				(begin
				  (set! cur i)
				  (set! curmin (vct-ref cross-weights i)))))
			  (set! best-mark cur))
			(set! handled (+ handled (inexact->exact (vct-ref cross-periods best-mark))))
			(if (or (< handled samps)
				(< (- handled samps) (- samps old-handled)))
			    (begin
			      (vct-set! edits curs best-mark)
			      (set! curs (1+ curs))))
			(vct-set! cross-weights best-mark 1000.0)))
		    ))
	     (do ((i 0 (1+ i)))
		 ((= i curs))
	       (let ((best-mark (vct-ref edits i)))
		 (if (> (vct-ref cross-periods best-mark) 0)
		     (if adding
			 (let ((beg (vct-ref cross-samples (vct-ref cross-marks best-mark)))
			       (len (vct-ref cross-periods best-mark)))
			   (let ((new-samps
				  (env-add (vct-ref cross-samples best-mark)
					   (vct-ref cross-samples (vct-ref cross-marks best-mark))
					   (vct-ref cross-periods best-mark))))
			     (insert-samples beg len new-samps)
			     (do ((j i (1+ j)))
				 ((= j curs))
			       (if (> (vct-ref cross-samples j) beg)
				   (vct-set! cross-samples j (+ (vct-ref cross-samples j) len))))))
			 (let* ((beg (vct-ref cross-samples best-mark))
				(delete-len (vct-ref cross-periods best-mark)))
			   (delete-samples beg delete-len)
			   (if (not (= delete-len 0))
			       (do ((j i (1+ j)))
				   ((= j curs))
				 (if (> (vct-ref cross-samples j) beg)
				     (vct-set! cross-samples j (- (vct-ref cross-samples j) delete-len len))))))))))
	     ;; delete from mark for period samps
	     ))
	 ;; and return to original srate
	 (unsample-sound snd chn)
	 ) ; end of as-one-edit thunk
       (format #f "(rubber-sound ~{~A~^ ~})" args)
       ))))


(define rubber-sound
  (lambda args
    (apply rubber-sound-1 args)
    (display (format #f "~%"))))

#!
;;; oboe.snd: (begin (rubber-sound 1.5) (revert-sound) (rubber-sound 0.5) (revert-sound) (delete-sample 10) (revert-sound))

;;;   not as one, no squelch:
derumble: 0.18 sample: 0.23 crossings: 3.81 cross: 3.79 weights: 27.49 sort: 6.92 change: 28.72 unsample: 0.13 overall: 71.31 amp_envs (641): time: 180 
derumble: 0.37 sample: 0.22 crossings: 2.81 cross: 2.85 weights: 26.87 sort: 6.76 change: 39.35 unsample: 0.04 overall: 79.28 amp_envs (1281): time: 50 

--------------------------------------------------------------------------------
;;; as one:
derumble: 0.12 sample: 0.23 crossings: 3.85 cross: 3.85 weights: 27.83 sort: 6.95 change: 3.12 unsample: 0.13 overall: 46.29 amp_envs (0): time: 0 
derumble: 0.16 sample: 0.22 crossings: 2.77 cross: 2.83 weights: 27.17 sort: 6.81 change: 2.81 unsample: 0.05 overall: 42.89 amp_envs (0): time: 0 
--------------------------------------------------------------------------------
;;; run crossings:
derumble: 0.12 sample: 0.22 crossings: 0.23 cross: 3.86 weights: 28.02 sort: 7.04 change: 2.39 unsample: 0.13 overall: 42.23 amp_envs (0): time: 0 
derumble: 0.16 sample: 0.23 crossings: 0.22 cross: 2.81 weights: 20.72 sort: 6.34 change: 1.71 unsample: 0.04 overall: 32.31 amp_envs (0): time: 0 
derumble: 0.12 sample: 0.23 crossings: 0.22 cross: 3.82 weights: 27.79 sort: 6.91 change: 2.84 unsample: 0.05 overall: 42.07
--------------------------------------------------------------------------------
;;; run cross:
derumble: 0.16 sample: 0.22 crossings: 0.23 cross: 0.23 weights: 27.49 sort: 9.93 change: 3.11 unsample: 0.12 overall: 41.71 amp_envs (0): time: 0 
derumble: 0.11 sample: 0.22 crossings: 0.23 cross: 0.22 weights: 27.38 sort: 9.91 change: 2.96 unsample: 0.04 overall: 41.16 amp_envs (0): time: 0 
--------------------------------------------------------------------------------
;;; run weigths/sort:
derumble: 0.17 sample: 0.23 crossings: 0.29 cross: 0.22 weights: 9.1 sort: 0.34 change: 3.16 unsample: 0.14 overall: 13.86 
derumble: 0.13 sample: 0.23 crossings: 0.25 cross: 0.23 weights: 9.18 sort: 0.34 change: 3.27 unsample: 0.09 overall: 14 
--------------------------------------------------------------------------------

!#
