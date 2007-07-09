(load "t.scm")

(define (cross-correlate snd0 chn0 snd1 chn1)
  (let* ((len0 (frames snd0 chn0))
	 (len1 (frames snd1 chn1))
	 (ilen (max len0 len1))
	 (pow2 (inexact->exact (ceiling (/ (log ilen) (log 2)))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftlen2 (/ fftlen 2))
	 (fftscale (/ 1.0 fftlen))
	 (rl1 (channel->vct 0 fftlen snd1 chn1))
	 (rl2 (channel->vct 0 fftlen snd0 chn0))
	 (im1 (make-vct fftlen))
	 (im2 (make-vct fftlen)))
    (fft rl1 im1 1)
    (fft rl2 im2 1)
    (let* ((tmprl (vct-copy rl1))
	   (tmpim (vct-copy im1))
	   (data3 (make-vct fftlen)))
      (vct-multiply! tmprl rl2)     ; (* tempr1 tempr2)
      (vct-multiply! tmpim im2)     ; (* tempi1 tempi2)
      (vct-multiply! im2 rl1)       ; (* tempr1 tempi2)
      (vct-multiply! rl2 im1)       ; (* tempr2 tempi1)
      (vct-add! tmprl tmpim)        ; add the first two
      (vct-subtract! im2 rl2)       ; subtract the 4th from the 3rd
      (vct-scale! (fft tmprl im2 -1) fftscale))))


(define (lag? snd0 chn0 snd1 chn1)
  ;; returns the probable lagtime between the two sounds (negative means second sound is delayed)
  (let* ((corr (cross-correlate snd0 chn0 snd1 chn1))
	 (len (vct-length corr))
	 (pk (- (vct-peak corr) .000001))
	 (pos -1)
	 (lag (do ((i 0 (1+ i)))
		  ((or (= i len)
		       (>= pos 0))
		   pos)
		(if (>= (vct-ref corr i) pk)
		    (set! pos i)))))
    (if (= lag -1)
	0
	(if (< lag (/ len 2))
	    lag
	    (- (- len lag))))))


(define* (snddiff-1 v0 v1 :optional (maxdiff 0.0))
  (let ((diff (vct-subtract! (vct-copy v0) v1)))
    (if (<= (vct-peak diff) maxdiff)
	'no-difference
	(let ((diffs 0)
	      (diff-data '())
	      (len (min (vct-length v0) (vct-length v1))))
	  (do ((i 0 (1+ i)))
	      ((or (> diffs 10)
		   (= i len)))
	    (if (> (abs (- (vct-ref v0 i) (vct-ref v1 i))) .00001)
		(begin
		  (set! diffs (1+ diffs))
		  (set! diff-data (cons (list i (vct-ref v0 i) (vct-ref v1 i)) diff-data)))))
	  (if (< diffs 10)
	      (list 'differences diff-data)
	      #f)))))

#|
(define (vct-size v)
  (sqrt (dot-product v v)))

(define (unconvolve-1 v0 v1 impulse-response)  ; assume here that v0 is the original and that we're aligned
  (let ((pos -1)
	(len (vct-length v0)))
    (do ((i 0 (1+ i)))
	((or (>= pos 0)
	     (= i len)))
      (if (not (= (vct-ref v1 i) 0.0))
	  (set! pos i)))
    (let ((scl (/ (vct-ref v1 pos) (vct-ref v0 pos)))
	  (size (vct-size v1)))
      (vct-subtract! v1 (vct-scale! (vct-copy v0) scl))

      ; (snd-display ";pos: ~A, scl: ~A, sizes: ~A ~A" pos scl size (vct-size v1))

      (if (< (vct-size v1) size)
	  (unconvolve-1 v0 v1 (cons scl impulse-response))
	  impulse-response))))

(define (unconvolve v0 v1)
  (let ((result (unconvolve-1 v0 (vct-copy v1) '())))
    (if (not (null? result))
	(reverse result)
	#f)))
|#  
  

(define (snddiff-2 snd0 chn0 snd1 chn1)
  ;; this can currently find initial delays, scaling differences, and scattered individual sample differences
  (let ((len0 (frames snd0 chn0))
	(len1 (frames snd1 chn1)))

    (or (and (= len0 len1)
	     (let ((s0 (channel->vct 0 #f snd0 chn0))
		   (s1 (channel->vct 0 #f snd1 chn1)))
	       (or (snddiff-1 s0 s1 0.0)
		   (let* ((pos (maxamp-position snd0 chn0))
			  (mx0 (sample pos snd0 chn0))
			  (mx1 (sample pos snd1 chn1)) ; use actual values to keep possible sign difference
			  (scl (/ mx1 mx0))
			  (diff (snddiff-1 (vct-scale! s0 scl) s1)))

		     (if (eq? diff 'no-difference)
			 (list 'scale scl)
			 (if (list? diff)
			     (list 'scale scl 'differences diff)
			     #f))))))

	;; align sounds and  zero out any non-overlapping sections, keeping track of whether they are zero beforehand
	(let ((lag (lag? snd0 chn0 snd1 chn1))
	      (pre0 0.0)
	      (pre1 0.0)
	      (post0 0.0)
	      (post1 0.0))

	  (if (> lag 0)
	      (begin
		(pad-channel 0 lag snd1 chn1)
		(set! pre0 (vct-peak (channel->vct 0 lag snd0 chn0)))
		(if (> pre0 0.0)
		    (scale-channel 0.0 0 lag snd0 chn0)))
	      (if (< lag 0)
		  (let ((pad (- lag)))
		    (pad-channel 0 pad snd0 chn0)
		    (set! pre1 (vct-peak (channel->vct 0 pad snd1 chn1)))
		    (if (> pre1 0.0)
			(scale-channel 0.0 0 pad snd1 chn1)))))

	  (set! len0 (frames snd0 chn0))
	  (set! len1 (frames snd1 chn1))
	  (if (> len0 len1)
	      (let ((dur (- len0 len1)))
		(set! post0 (vct-peak (channel->vct len1 dur snd0 chn0)))
		(scale-channel 0.0 len1 dur snd0 chn0))
	      (if (> len1 len0)
		  (let ((dur (- len1 len0)))
		    (set! post1 (vct-peak (channel->vct len0 dur snd1 chn1)))
		    (scale-channel 0.0 len0 dur snd1 chn1))))

	  (let ((s0 (channel->vct 0 #f snd0 chn0))
		(s1 (channel->vct 0 #f snd1 chn1)))
	    (or (and (snddiff-1 s0 s1 0.0)
		     (list 'lag lag pre0 pre1 post0 post1))
		(let* ((pos (maxamp-position snd0 chn0))
		       (mx0 (sample pos snd0 chn0))
		       (mx1 (sample pos snd1 chn1)) ; use actual values to keep possible sign difference
		       (scl (/ mx1 mx0))
		       (diff (snddiff-1 (vct-scale! s0 scl) s1 0.0001)))
		  
		  (if (eq? diff 'no-difference)
		      (list 'scale scl 'lag lag pre0 pre1 post0 post1)
		      (if (list? diff)
			  (list 'scale scl 'differences diff 'lag lag pre0 pre1 post0 post1)
			  #f))))))
	
	;; align and zero + scaling didn't find a match
	)))


(define (snddiff snd0 chn0 snd1 chn1)
  ;; a wrapper for snddiff to put things back the way they were
  (let ((edpos0 (edit-position snd0 chn0))
	(edpos1 (edit-position snd1 chn1)))
    (let ((result (snddiff-2 snd0 chn0 snd1 chn1)))
      (set! (edit-position snd0 chn0) edpos0)
      (set! (edit-position snd1 chn1) edpos1)
      result)))
