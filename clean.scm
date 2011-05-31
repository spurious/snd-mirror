;; clean-channel -- noise reduction

(provide 'snd-clean.scm)
(if (not (provided? 'snd-dsp.scm)) (load "dsp.scm"))

(define* (goertzel-channel freq beg dur snd chn)
  "(goertzel-channel freq beg dur snd chn) returns the amplitude of the 'freq' spectral component"
  (let* ((sr (srate snd))
	 (y2 0.0)
	 (y1 0.0)
	 (y0 0.0)
	 (rfreq (/ (* 2.0 pi freq) sr))
	 (cs (* 2.0 (cos rfreq))))
    (scan-channel (lambda (y)
		    (set! y2 y1)
		    (set! y1 y0)
		    (set! y0 (+ (- (* y1 cs) y2) y))
		    #f)
		  (or beg 0) 
		  (or dur (frames snd chn))
		  snd chn)
    (magnitude (- y0 (* y1 (exp (make-rectangular 0.0 (- rfreq))))))))

(define* (check-freq freq snd chn)
  (let ((hum 0.0))
    (do ((i 0 (+ 1 i))
	 (loc 0.0 (+ loc (round (/ (frames snd chn) 5)))))
	((= i 4))
      (set! hum (+ hum (goertzel-channel freq loc 2048 snd chn))))
    (/ hum 4.0)))


;;; -------- single sample clicks

(define* (remove-single-sample-clicks (jump 8) snd chn)
  (let* ((reader (make-sampler 0 snd chn))
	 (mx (make-moving-average 16)) ; local average of abs difference
	 (samp0 0.0)
	 (samp1 0.0)
	 (samp2 0.0)
	 (fixed 0)
	 (len (frames snd chn))
	 (block-size (min len (* 1024 1024))) ; do edits by blocks rather than sample-at-a-time (saves time, memory etc)
	 (block-ctr 0)
	 (block-beg 0)
	 (block (make-vct block-size))
	 (block-changed #f))
    (run
     (do ((ctr 0 (+ 1 ctr)))
	 ((= ctr len))
       (set! samp0 samp1)
       (set! samp1 samp2)
       (set! samp2 (next-sample reader))
       (set! (block block-ctr) samp2)
       (let* ((df1 (abs (- samp1 samp0)))
	      (df2 (abs (- samp2 samp1)))
	      (df3 (abs (- samp2 samp0)))
	      (local-max (moving-average mx df1)))
	 (if (and (> df1 (* jump local-max))
		  (> df2 (* jump local-max))
		  (or (< df3 local-max)
		      (and (< df3 (* 2 local-max))
			   (< (* (- samp2 samp0)
				 (- samp1 samp0))
			      0.0))))
	     (begin
	       (set! samp1 (* .5 (+ samp0 samp2)))
	       (set! (block (- block-ctr 1)) samp1)
	       (set! block-changed #t)
	       (set! fixed (+ 1 fixed)))))
       (set! block-ctr (+ 1 block-ctr))
       (if (>= block-ctr block-size)
	   (begin
	     (if block-changed
		 (begin
		   (vct->channel block block-beg block-size snd chn)
		   (set! block-changed #f)))
	     (set! block-beg (+ block-beg (- block-size 1)))
	     (set! block-ctr 1)
	     (set! (block 0) samp2))))
     (if block-changed
	 (vct->channel block block-beg block-ctr snd chn)))
    fixed))

(define (test-remove-single-clicks)
  (let ((test (new-sound "test.snd")))
    (let ((data (make-vct 1001)))
      (do ((i 2 (+ i 30))
	   (val .9 (- val .05)))
	  ((>= i 1000))
	(set! (data i) val))
      (set! (data 1000) .001)
      (vct->channel data)
      (remove-single-sample-clicks)
      (let ((mx (maxamp))
	    (loc (maxamp-position)))
	(if (> mx 0.06)
	    (format #t "~%;remove-single-sample-clicks 0: ~A (at ~D)" mx loc)))
      (revert-sound)
      (do ((i 0 (+ 1 i))
	   (ang 0.0 (+ ang .01)))
	  ((= i 1000))
	(set! (data i) (+ (data i) (* .2 (sin ang)))))
      (vct->channel data)
      (remove-single-sample-clicks)
      (if (fneq (maxamp) .2)
	  (format #t "~%;remove-single-sample-clicks sin max: ~A" (maxamp)))
      (let ((cur-data (channel->vct 0))
	    (diff 0.0))
	(do ((i 0 (+ 1 i))
	     (ang 0.0 (+ ang .01)))
	    ((= i 1000))
	  (set! diff (max diff (abs (- ( cur-data i) (* .2 (sin ang)))))))
	(if (> diff .2)
	    (format #t "~%;remove-single-sample-clicks sine max diff: ~A" diff))))
    (close-sound test)))

;;; -------- pops

(define* (smooth-vct data beg dur)
  (let* ((y0 (data beg))
	 (y1 (data (+ beg dur)))
	 (angle (if (> y1 y0) pi 0.0)) 
	 (off (* .5 (+ y0 y1))) 
	 (scale (* 0.5 (abs (- y1 y0))))
	 (incr (/ pi dur)))
    (do ((i 0 (+ 1 i)))
	((= i dur))
      (set! (data (+ beg i)) (+ off (* scale (cos (+ angle (* i incr)))))))))

(define* (remove-pops (size 8) snd chn)
  (let* ((reader (make-sampler 0 snd chn))
	 (dly0 (make-delay (* 4 size)))
	 (dly1 (make-delay (* 5 size)))
	 (mx-ahead (make-moving-average (* 4 size))) ; local average of abs difference ahead of current window
	 (mx-behind (make-moving-average (* 4 size))) ; local average of abs difference behind current window
	 (mx (make-moving-average size)) ; local average of abs difference

	 (last-ahead-samp 0.0)
	 (last-dly0-samp 0.0)
	 (last-dly1-samp 0.0)

	 (last-case 0)
	 (fixed 0)
	 (len (frames snd chn))

	 (pad (* 8 size))
	 (block-size (min (+ len pad) (* 1024 1024)))
	 (block-ctr 0)
	 (block-beg 0)
	 (block (make-vct block-size))
	 (block-changed #f))
    
    (run
     (let ((check-val 0.0)
	   (check-start 0)
	   (checker 0)
	   (checked 0)
	   (checking #f)
	   (moving-start #t))
       (do ((ctr 0 (+ 1 ctr)))
	   ((= ctr len))
	 (let* ((ahead-samp (next-sample reader))
		(diff-ahead (abs (- ahead-samp last-ahead-samp)))
		(avg-ahead (moving-average mx-ahead diff-ahead))
		(dly0-samp (delay dly0 ahead-samp))
		(cur-diff (abs (- dly0-samp last-dly0-samp)))
		(cur-avg (moving-average mx cur-diff))
		(dly1-samp (delay dly1 ahead-samp))
		(diff-behind (abs (- dly1-samp last-dly1-samp)))
		(avg-behind (moving-average mx-behind diff-behind)))
	   (set! last-ahead-samp ahead-samp)
	   (set! last-dly0-samp dly0-samp)
	   (set! last-dly1-samp dly1-samp)
	   (set! (block block-ctr) ahead-samp)
	   (if checking
	       (begin
		 (set! checked (+ 1 checked))
		 (if (or (>= checked (* 2 size))
			 (< cur-avg check-val))
		     (begin
		       (set! fixed (+ 1 fixed))
		       (set! checking #f)
		       (smooth-vct block (- check-start block-beg) (+ size checker))
		       (set! block-changed #t)))
		 (if moving-start
		     (begin
		       (set! moving-start (< cur-diff avg-behind))
		       (if moving-start
			   (set! check-start (+ 1 check-start)))))
		 (if (not moving-start)
		     (set! checker (+ 1 checker))))
	       (if (and (> (- ctr last-case) (* 2 size))
			(> cur-avg (* 4 avg-ahead))
			(> cur-avg (* 4 avg-behind)))
		   ;; possible pop
		   (begin
		     (set! check-start (max 0 (- ctr (* 5 size))))
		     (set! moving-start (< cur-diff avg-behind))
		     (if moving-start
			 (set! check-start (+ 1 check-start)))
		     (set! checking #t)
		     (set! check-val cur-avg)
		     (set! checker 0)
		     (set! checked 0)
		     (set! last-case ctr))))
	   
	   (set! block-ctr (+ 1 block-ctr))
	   (if (>= (+ block-ctr pad) block-size)
	       (begin
		 (if block-changed
		     (begin
		       (vct->channel block block-beg (- block-ctr pad) snd chn)
		       (set! block-changed #f)))
		 (set! block-beg (+ block-beg (- block-ctr pad)))
		 (do ((i 0 (+ 1 i))
		      (j (- block-ctr pad) (+ 1 j)))
		     ((= i pad))
		   (set! (block i) (block j)))
		 (set! block-ctr pad)))))
       
       (if block-changed
	   (vct->channel block block-beg block-ctr snd chn))))

    fixed))

(define (test-remove-pops)
  (new-sound "test.snd")
  (let ((data (make-vct 4001)))
    (do ((i 100 (+ i 200)))
	((>= i 3800))
      (let ((size (random 8)))
	(do ((k 0 (+ 1 k)))
	    ((= k size))
	  (set! (data (+ i k)) (- 1.0 (random 2.0))))))
    (vct->channel data)
    (remove-pops)
    (let ((mx (maxamp)))
      (if (> mx .01)
	  (format #t "~%;test remove-pops 0 case: ~A" mx)))
    (revert-sound)
    (do ((i 0 (+ 1 i))
	 (ang 0.0 (+ ang .01)))
	((= i 4000))
      (set! (data i) (+ (data i)
			(* .2 (sin ang)))))
    (vct->channel data)
    (remove-pops)
    (let ((mx (maxamp)))
      (if (fneq mx .2)
	  (format #t "~%;test remove-pops sine case: ~A" mx)))
    (close-sound)))
	

;;; -------- hum

(define (test-notch-hum)
  (let ((test (with-sound (:output "test.snd" :srate 22050)
		(let ((osc (make-oscil 60.0))
		      (e (make-env '(0 0 1 .5 9 .5 10 0) :length 44100)))
		  (run
		   (do ((i 0 (+ 1 i)))
		       ((= i 44100))
		     (outa i (* (env e) (oscil osc)))))))))
    
    (notch-channel (list 60.0) #f #f #f #f #f #f #t 8)
    (let ((mx (maxamp)))
      (if (> mx .02)
	  (format #t "~%;notch hum 0: ~A" mx)))
    (close-sound (find-sound test)))
  (let ((test (with-sound (:output "test.snd" :srate 22050)
		(let ((osc (make-oscil 60.0))
		      (osc1 (make-oscil 40.0))
		      (osc2 (make-oscil 80.0))
		      (e (make-env '(0 0 1 .3 9 .3 10 0) :length 44100)))
		  (run
		   (do ((i 0 (+ 1 i)))
		       ((= i 44100))
		     (outa i (* (env e) (+ (oscil osc) (oscil osc1) (oscil osc2))))))))))
    
    (let ((v60 (goertzel 60.0))
	  (v40 (goertzel 40.0))
	  (v80 (goertzel 80.0)))
      (notch-channel (list 60.0) #f #f #f #f #f #f #t 8)
      (let ((e60 (goertzel 60.0))
	    (e40 (goertzel 40.0))
	    (e80 (goertzel 80.0)))
	(if (or (fneq (/ e60 v60) 0.0)
		(fneq (/ e40 v40) 1.0)
		(fneq (/ e80 v80) 1.0))
	    (format #t "~%;notch 60: ~A ~A ~A -> ~A ~A ~A" v40 v60 v80 e40 e60 e80))))
    (close-sound (find-sound test)))

  (let ((test (with-sound (:output "test.snd" :srate 22050)
		(let ((osc (make-oscil 60.0))
		      (osc1 (make-oscil 55.0))
		      (osc2 (make-oscil 65.0))
		      (e (make-env '(0 0 1 .3 9 .3 10 0) :length 44100)))
		  (run
		   (do ((i 0 (+ 1 i)))
		       ((= i 44100))
		     (outa i (* (env e) (+ (oscil osc) (oscil osc1) (oscil osc2))))))))))
    
    (let ((v60 (goertzel 60.0))
	  (v40 (goertzel 55.0))
	  (v80 (goertzel 65.0)))
      (notch-channel (list 60.0) #f #f #f #f #f #f #t 2)
      (let ((e60 (goertzel 60.0))
	    (e40 (goertzel 55.0))
	    (e80 (goertzel 65.0)))
	(if (or (> (/ e60 v60) 0.01)
		(< (/ e40 v40) 0.99)
		(< (/ e80 v80) 0.99))
	    (format #t "~%;notch 60 tight: ~A ~A ~A -> ~A ~A ~A" v40 v60 v80 e40 e60 e80))))
    (close-sound (find-sound test))))


;;; -------- DC

(define (test-remove-DC)
  (let ((test (new-sound "test.snd"))
	(data (make-vct 4001)))
    (do ((i 0 (+ 1 i))
	 (ang 0.0 (+ ang .01)))
	((= i 4000))
      (set! (data i) (+ .1 (- 0.1 (random 0.2)) (* .2 (sin ang)))))
    (vct->channel data)
    (let ((dc (goertzel 0.0))
	  (sig (goertzel 35.0)))
      (let ((dcflt (make-filter 2 (vct 1 -1) (vct 0 -0.99))))
	(map-channel (lambda (y) (filter dcflt y)))
	(let ((ndc (goertzel 0.0))
	      (nsig (goertzel 35.0)))
	  (if (or (> (/ ndc dc) .1)
		  (< (/ nsig sig) .4))
	      (format #t "~%;remove-DC: ~A -> ~A (~A), ~A -> ~A (~A)" dc ndc (/ ndc dc) sig nsig (/ nsig sig))))))
    (close-sound test)))


(define* (tvf-channel snd chn)
  (let* ((size (frames snd chn))
	 (avg-data (make-vct size))
	 (ctr 0)
	 (mx (maxamp snd chn))
	 (avg-size 8192)
	 (rd0 (make-sampler 0 snd chn))
	 (xhat 0.0)
	 (frm (make-formant :radius (- 1.0 (/ 500.0 (srate snd))) :frequency 600))
	 (del (make-moving-sum avg-size))
	 (K 0.0)

	 (maxg 0.0)
	 (ming 1000.0)
	 (maxK 0.0)
	 (minK 1000.0)
	 )

    (do ((i 0 (+ 1 i)))
	((= i avg-size))
      (moving-sum del (formant frm (rd0))))

    (map-channel
     (lambda (datum)
       (let ((xhatminus xhat)
	     (avg (moving-sum del (formant frm (rd0)))))

	 (set! K (min 1.0 (+ .1 (/ avg 100.0))))
;	 (set! K .5)

	 (set! (avg-data ctr) K)
	 (set! ctr (+ 1 ctr))

	 (set! maxg (max maxg avg))
	 (set! ming (min ming avg))
	 (set! maxK (max maxK K))
	 (set! minK (min minK K))

	 (set! xhat (+ xhatminus
		       (* K (- datum xhatminus))))
	 xhatminus))
     0 size snd chn)

    (let ((mx1 (maxamp snd chn)))
      (scale-channel (/ mx mx1) snd chn))

;    (format #t ";K ~A to ~A, avg ~A to ~A" minK maxK ming maxg)
;    avg-data
    ))



(define* (clean-channel snd chn)

  ;; look for obvious simple clicks
  (let ((clicks (as-one-edit (lambda () (remove-single-sample-clicks 8 snd chn)))))
    (if (> clicks 0)
	(format #t "~%; fixed ~D single sample clicks" clicks)
	(format #t "~%; no single-sample clicks found")))

  ;; look for obvious clipping and try to reconstruct
  (let ((mx (maxamp snd chn)))
    (if (>= mx 1.0)
	(let ((clips (unclip-channel snd chn)))
	  (if (eq? clips 'no-clips)
	      (format #t "~%; no clipped portions found")
	      (format #t "~%; reconstructed ~D clipped portions" (list-ref clips 3))))
	(format #t "~%; no obvious clipping (max amp: ~A)" mx)))

  ;; look for pops
  (let ((total-pops 0))
    (call-with-exit
     (lambda (quit)
       (for-each
	(lambda (size)
	  (let ((pops (as-one-edit (lambda () (remove-pops size snd chn)))))
	    (set! total-pops (+ total-pops pops))
	    (if (> pops 0)
		(format #t "~%; fixed ~D ~D-sample ~A" pops size (if (= pops 1) "pop" "pops"))
		(quit))))
	(list 4 8 16 32))))
    (if (= total-pops 0)
	(format #t "~%; no pops found")))

  ;; look for hum
  (let* ((hum60 (check-freq 60.0 snd chn))
	 (hum55 (check-freq 55.0 snd chn))
	 (hum (max hum60 hum55)))
    (if (> hum 30.0)
	(let ((humf (if (> hum60 hum55) 60.0 55.0)))
	  (notch-channel (list humf) 4096 0 (frames snd chn) snd chn #f #t 4)
	  (format #t "~%; notch out ~D cycle hum: ~A -> ~A" (floor humf) hum (check-freq humf snd chn)))))

  ;; look for DC
  (let ((dc (check-freq 0.0 snd chn)))
    (if (> dc 30.0)
	(let ((dcflt (make-filter 2 (vct 1 -1) (vct 0 -0.99))))
	  (map-channel (lambda (y) (filter dcflt y)) 0 (frames snd chn) snd chn)
	  (format #t "~%; block DC: ~A -> ~A" dc (check-freq 0.0 snd chn)))))

  ;; time-varying low-pass filter
  (tvf-channel snd chn)
  )


(define* (clean-sound snd)
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "clean-sound" snd))
	(let ((chns (channels index)))
	  (do ((chn 0 (+ 1 chn)))
	      ((= chn chns))
	    (clean-channel index chn))))))
