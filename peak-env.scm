(use-modules (ice-9 common-list))

(define save-peak-env-info #t)
(define save-peak-env-info-directory "~/peaks")

(let ((saved-peak-info '()))

  (define saved-info
    (make-procedure-with-setter
     (lambda (snd)
       (find-if (lambda (n) 
		  (string=? (car n) (file-name snd))) 
		saved-peak-info))
     (lambda (snd new-info)
       (set! saved-peak-info
	     (cons new-info
		   (remove-if
		    (lambda (n)
		      (string=? (car n) (file-name snd)))
		    saved-peak-info))))))

  (define (peak-env-info-file-name snd chn)	
    (format #f "~~/peaks/~A-peaks-~D" (short-file-name snd) chn))

  (define (save-peak-env-info-at-close snd)
    ;; intended as a close-hook function
    (if (and save-peak-env-info
	     (not (null? (peak-env-info snd 0 0))))
	(let ((saved #f))
	  (do ((i 0 (1+ i)))
	      ((= i (chans snd)))
	    (let ((peak-file (mus-expand-filename (peak-env-info-file-name snd i))))
	      (if (or (not (file-exists? peak-file))
		      (< (file-write-date peak-file)
			 (file-write-date (file-name snd))))
		  (begin
		    (if (not saved)
			(begin
			  (set! (saved-info snd) (list (file-name snd) (data-format snd) (chans snd)))
			  (set! saved #t)))
		    (catch 'no-such-envelope
			   (lambda ()
			     (write-peak-env-info-file snd i peak-file))
			   (lambda args args))))))))
    #f)

  (define (restore-peak-env-info-upon-open snd chn dur)
    ;; intended as an initial-graph-hook-function
    (let ((peak-info (saved-info snd)))
      (if (or (not peak-info)
	      (and (= (data-format snd) (cadr peak-info))
		   (= (channels snd) (caddr peak-info))))
	  (let ((peak-file (mus-expand-filename (peak-env-info-file-name snd chn))))
	    (if (and (file-exists? peak-file)
		     (> (file-write-date peak-file)
			(file-write-date (file-name snd))))
		(begin
		  (set! (saved-info snd) (list (file-name snd) (data-format snd) (chans snd)))
		  (read-peak-env-info-file snd chn peak-file)))))
      #f))

  (add-hook! close-hook save-peak-env-info-at-close)
  (add-hook! initial-graph-hook restore-peak-env-info-upon-open)
  (add-hook! exit-hook 
	     (lambda () 
	       (for-each save-peak-env-info-at-close (sounds)) 
	       #f)))




