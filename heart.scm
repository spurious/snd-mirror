;;; use with-sound to write the data to a sound file
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))

;;; turn off clipping (the numbers will be between 70 and 150)
(set! (mus-clipping) #f)
(set! *clm-clipped* #f)

;;; these hooks may be drawing the graph in the upper right corner, which we don't want for now
(reset-hook! after-graph-hook)
(reset-hook! mouse-click-hook)
(reset-hook! update-hook)

;;; tell Snd not to try to load the data file
(set! (script-arg) (+ 1 (script-arg)))

;;; search for "...[<number>am|pm <number>/<number>...", put the two readings in a stereo file
(let* ((hpsum 0) ; for average readings
       (lpsum 0)
       (average (make-moving-average 14)) ; 2-week average
       (average1 (make-moving-average 90)) ; 3-month average
       (ind (find-sound
	     (with-sound (:channels 6 :data-format mus-lfloat) ; float output to be sure it can handle the full range
	      (let ((samp 0))	    
		(call-with-input-file 
		    (list-ref (script-args) 1) ; invocation arg = text file of data ("snd heart.scm data.txt")
		  (lambda (file)
		    (let loop ((line (read-line file 'concat)))
		      (or (eof-object? line)
			  (let ((len (string-length line)))
			    (do ((i 0 (+ 1 i)))
				((>= i (- len 14)))
			      (if (and (char=? (string-ref line i) #\[)
				       (char=? (string-ref line (+ i 3)) #\m)
				       (or (char=? (string-ref line (+ i 2)) #\a) 
					   (char=? (string-ref line (+ i 2)) #\p)))
				  (let ((hp (string->number (substring line (+ i 5) (+ i 5 3))))
					(lp (string->number (substring line (+ i 5 4) (+ i 5 6)))))
				    (set! hpsum (+ hpsum hp))
				    (set! lpsum (+ lpsum lp))
				    (out-any samp hp 0) ; output the readings
				    (out-any samp lp 1)
				    (out-any samp 120 2)
				    (out-any samp 80 3)
				    (out-any samp (max 90 (moving-average average (* 0.5 (+ lp hp)))) 4)
				    (out-any samp (max 90 (moving-average average1 (* 0.5 (+ lp hp)))) 5)
				    (set! samp (+ 1 samp)))))
			    (loop (read-line file 'concat))))))))))))

  ;; now display the data with y-axis bounds between 50 and 150, both traces in the same graph, x-axis in "samples" (readings)
  (set! (channel-style ind) channels-superimposed)
  (do ((chan 0 (+ 1 chan)))
      ((= chan 6))
    (set! (x-axis-style ind chan) x-axis-in-samples)
    (set! (x-axis-label ind chan) "days")
    (set! (y-bounds ind chan) (list 50 150)))

  ;; print the average readings over the full sequence
  (snd-print (format #f ";average: ~A/~A~%" 
		     (round (/ hpsum (frames)))
		     (round (/ lpsum (frames))))))

