(use-modules (ice-9 format))
(if (not (defined? 'read-line)) (use-modules (ice-9 rdelim)))

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
(set! (script-arg) (1+ (script-arg)))

;;; search for "...[<number>am|pm <number>/<number>...", put the two readings in a stereo file
(let* ((hpsum 0) ; for average readings
       (lpsum 0)
       (ind (find-sound
	     (with-sound (:channels 4 :data-format mus-lfloat) ; float output to be sure it can handle the full range
	      (let ((samp 0))	    
		(call-with-input-file 
		    (list-ref (script-args) 1) ; invocation arg = text file of data ("snd heart.scm data.txt")
		  (lambda (file)
		    (let loop ((line (read-line file 'concat)))
		      (or (eof-object? line)
			  (let ((len (string-length line)))
			    (do ((i 0 (1+ i)))
				((>= i (- len 14)))
			      (if (and (char=? (string-ref line i) #\[)
				       (char=? (string-ref line (+ i 3)) #\m)
				       (or (char=? (string-ref line (+ i 2)) #\a) 
					   (char=? (string-ref line (+ i 2)) #\p)))
				  (let ((hp (string->number (substring line (+ i 5) (+ i 5 3))))
					(lp (string->number (substring line (+ i 5 4) (+ i 5 6)))))
				    (set! hpsum (+ hpsum hp))
				    (set! lpsum (+ lpsum lp))
				    (out-any samp hp 0 *output*) ; output the readings
				    (out-any samp lp 1 *output*)
				    (out-any samp 120 2 *output*)
				    (out-any samp 80 3 *output*)
				    (set! samp (1+ samp)))))
			    (loop (read-line file 'concat))))))))))))

  ;; now display the data with y-axis bounds between 50 and 150, both traces in the same graph, x-axis in "samples" (readings)
  (set! (channel-style ind) channels-superimposed)
  (set! (y-bounds ind 0) (list 50 150))
  (set! (y-bounds ind 1) (list 50 150))
  (set! (y-bounds ind 2) (list 50 150))
  (set! (y-bounds ind 3) (list 50 150))
  (set! (x-axis-style) x-axis-in-samples)

  ;; print the average readings over the full sequence
  (snd-print (format #f ";average: ~A/~A~%" 
		     (inexact->exact (round (/ hpsum (frames)))) 
		     (inexact->exact (round (/ lpsum (frames)))))))

