;;; rubber.scm: rubber-sound stretches or contracts a sound (in time)
;;;   (rubber-sound 1.5) makes it 50% longer
;;;   rubber-sound looks for stable portions and either inserts or deletes periods 
;;;     period length is determined via autocorrelation

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(debug-enable 'debug 'backtrace)
(read-enable 'positions)

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

(define amp-weight
  ;; return difference / sum for current periods
  (lambda args
    (let* ((s0 (car args))
	   (s1 (cadr args))
	   (len (caddr args))
	   (snd (if (> (length args) 3) (list-ref args 3) #f))
	   (chn (if (> (length args) 4) (list-ref args 4) #f))
	   (sr0 (make-sample-reader s0 snd chn))
	   (sr1 (make-sample-reader s1 snd chn))
	   (ampsum 0.0)
	   (diffsum 0.0))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(let ((samp0 (next-sample sr0))
	      (samp1 (next-sample sr1)))
	  (set! ampsum (+ ampsum (abs samp0)))
	  (set! diffsum (+ diffsum (abs (- samp1 samp0))))))
      (free-sample-reader sr0)
      (free-sample-reader sr1)
      (/ diffsum ampsum))))

(define crossings
  ;; return number of upward zero crossings that don't look like silence
  (lambda args
    (let* ((snd (if (not (null? args)) (car args) #f))
	   (chn (if (and (not (null? args)) (> (length args) 1)) (cadr args) #f))
	   (crosses 0)
	   (sr0 (make-sample-reader 0 snd chn))
	   (samp0 (next-sample sr0))
	   (len (frames snd chn))
	   (sum 0.0)
	   (last-cross 0)
	   (silences 0)
	   (silence (* extension .001)))
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
		    (set! sum 0.0))
		  (set! silences (+ silences 1))))
	  (set! sum (+ sum (abs samp0)))
	  (set! samp0 samp1)))
      (free-sample-reader sr0)
      (list crosses silences))))

(define expected-length
  ;; return autocorrelation notion of probable longest significant period
  (lambda args
    (let* ((s0 (car args))
	   (snd (if (> (length args) 1) (list-ref args 1) #f))
	   (chn (if (> (length args) 2) (list-ref args 2) #f))
	   (pow2 (ceiling (/ (log (* extension (/ (srate snd) 40.0))) (log 2))))
	   (fftlen (inexact->exact (expt 2 pow2)))
	   (data (autocorrelate (samples->vct s0 fftlen snd chn))))
      (call-with-current-continuation
       (lambda (return)
	 (do ((i 1 (1+ i)))
	     ((= i 2046) 0)
	   (if (and (< (vct-ref data i) (vct-ref data (+ i 1)))
		    (> (vct-ref data (+ i 1)) (vct-ref data (+ i 2))))
	       (return (* i 2)))))))))

(define min-weight
  (lambda (arr)
    (let ((cur 0)
	  (curmin (vector-ref arr 0))
	  (len (vector-length arr)))
      (do ((i 0 (1+ i)))
	  ((= i len) cur)
	(if (and (number? (vector-ref arr i))
		 (< (vector-ref arr i) curmin))
	    (begin
	      (set! cur i)
	      (set! curmin (vector-ref arr i))))))))

(define env-add
  (lambda (s0 s1 samps snd chn)
    (let ((data (make-vector samps))
	  (x 1.0)
	  (xinc (/ 1.0 samps))
	  (sr0 (make-sample-reader s0 snd chn))
	  (sr1 (make-sample-reader s1 snd chn)))
      (do ((i 0 (1+ i)))
	  ((= i samps))
	(vector-set! data i (+ (* x (next-sample sr0))
			    (* (- 1.0 x) (next-sample sr1))))
	(set! x (+ x xinc)))
      (free-sample-reader sr0)
      (free-sample-reader sr1)
      data)))

(define rubber-sound
  (lambda args
    (let* ((stretch (car args))
	   (snd (if (> (length args) 1) (list-ref args 1) #f))
	   (chn (if (> (length args) 2) (list-ref args 2) #f)))

      ;; prepare sound (get rid of low freqs, resample)

      (derumble-sound snd chn)
      (sample-sound snd chn)

      (let* ((crosses (car (crossings snd chn)))
	     (cross-samples (make-vector crosses))
	     ;(cross-maxes (make-vector crosses))
	     ;(cross-to-max-times (make-vector crosses))
	     (cross-weights (make-vector crosses))
	     (cross-marks (make-vector crosses))
	     ;(cross-ids (make-vector crosses))
	     (cross-periods (make-vector crosses)))

	;; get cross points (sample numbers)
	(let* ((sr0 (make-sample-reader 0 snd chn))
	       (samp0 (next-sample sr0))
	       (len (frames snd chn))
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
		    (vector-set! cross-samples cross i)
		    (set! cross (+ cross 1))))
	      (set! sum (+ sum (abs samp0)))
	      (set! samp0 samp1)))
	  (free-sample-reader sr0))

	(set! (squelch-update snd chn) #t)
	;; now run through crosses getting period match info

	(do ((i 0 (1+ i)))
	    ((or (c-g?) (= i (1- crosses))))
	  (let* ((start (vector-ref cross-samples i))
		 (autolen (expected-length start snd chn)))

	    (let* (;(id (add-mark start snd chn))
		   (next-start (+ start autolen))
		   (min-i (+ i 1))
		   (min-samps (abs (- (vector-ref cross-samples min-i) next-start))))
	      (do ((k (+ i 2) (1+ k)))
		  ((= k (min crosses (+ i zeros-checked))))
		(let ((dist (abs (- (vector-ref cross-samples k) next-start))))
		  (if (< dist min-samps)
		      (begin
			(set! min-samps dist)
			(set! min-i k)))))

	      (let* ((current-mark min-i)
		     (current-min (amp-weight start (vector-ref cross-samples current-mark) autolen snd chn)))
		;(set! (mark-name id) (format #f "~A-~1,3F" autolen current-min))

		(set! min-samps (* 0.5 current-min))

		(let ((top (min (1- crosses) current-mark (+ i zeros-checked))))
		  (do ((k (+ i 1) (1+ k)))
		      ((= k top))
		    (let ((wgt (amp-weight start (vector-ref cross-samples k) autolen snd chn)))
		      (if (< wgt min-samps)
			  (begin
			    (set! min-samps wgt)
			    (set! min-i k)))))

		  (if (not (= current-mark min-i))
		      (begin
			;; these are confused, so effectively erase them
			(vector-set! cross-weights i 1000.0)
			)
		      (begin
			(vector-set! cross-weights i current-min)
			(vector-set! cross-marks i current-mark)
			;(vector-set! cross-ids i id)
			(vector-set! cross-periods i (- (vector-ref cross-samples current-mark) (vector-ref cross-samples i)))
			))

		  )))))
	
	;; now sort weights to scatter the changes as evenly as possible
	
	(let* ((len (frames snd chn))
	       (adding (> stretch 1.0))
	       (samps (inexact->exact (* (abs (- stretch 1.0)) len)))
	       (handled 0)
	       (edit-list '()))
	  
	  (do ()
	      ((>= handled samps))
	  ;; need to find enough splice points to add/delete samps
	    (let ((best-mark (min-weight cross-weights))
		  (old-handled handled))
	      (set! handled (+ handled (vector-ref cross-periods best-mark)))
	      (if (or (< handled samps)
		      (< (- handled samps) (- samps old-handled)))
		  (set! edit-list (cons best-mark edit-list)))
	      (vector-set! cross-weights best-mark 1000.0)))

	  (set! edit-list (sort edit-list >))
	  ;(snd-print (format #f "edits: ~A" edit-list))

	  (do ((i 0 (1+ i)))
	      ((= i (length edit-list)))
	      
	    (let ((best-mark (list-ref edit-list i)))
	      
	      (let ((new-samps
		     (env-add (vector-ref cross-samples best-mark)
			      (vector-ref cross-samples (vector-ref cross-marks best-mark))
			      (vector-ref cross-periods best-mark)
			      snd chn)))

;		(let ((id (add-mark (vector-ref cross-samples best-mark) snd chn)))
;		  (set! (mark-name id) (format #f "~D" best-mark)))

		(if adding
		    (insert-samples (vector-ref cross-samples (vector-ref cross-marks best-mark))
				    (vector-ref cross-periods best-mark)
				    new-samps
				    snd chn)
		    (let ((beg (vector-ref cross-samples best-mark)))
		      (delete-samples beg
				      (* 2 (vector-ref cross-periods best-mark))
				      snd chn)
		      (insert-samples beg
				      (vector-ref cross-periods best-mark)
				      new-samps
				      snd chn))))))
				      
		  ;; delete from mark for period samps


	  )



        (set! (squelch-update snd chn) #f)
	(update-time-graph snd chn)
	;; and return to original srate
	(unsample-sound snd chn)
	))))


