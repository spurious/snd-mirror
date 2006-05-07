(use-modules (ice-9 common-list) (ice-9 format))
(provide 'snd-peak-env.scm)

(if (not (defined? 'find-if))
    (define (find-if pred l)
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l))))))

(define save-peak-env-info? #t)
(define save-peak-env-info-directory "~/peaks")

;; intended as a close-hook function
(define save-peak-env-info
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
    
    (define (clean-string str)
      ;; full file name should be unique, so I think we need only fix it up to look like a flat name
      (let* ((len (string-length str))
	     (new-str (make-string len #\.)))
	(do ((i 0 (1+ i)))
	    ((= i len) new-str)
	  (let ((c (string-ref str i)))
	    (if (or (char=? c #\\)
		    (char=? c #\/))
		(string-set! new-str i #\_)
		(string-set! new-str i c))))))
    
    (define (peak-env-info-file-name snd chn)	
      (format #f "~~/peaks/~A-peaks-~D" (clean-string (file-name snd)) chn))
    
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
		    (catch 'bad-format
			   (lambda ()
			     (read-peak-env-info-file snd chn peak-file))
			   (lambda args
			     (display (format #f ";~A is in the wrong data format; will delete it..." peak-file))
			     ;; other errors are signs of trouble somewhere (no data, unreadable file, etc)
			     (delete-file peak-file)))))))
	#f))


    (add-hook! initial-graph-hook restore-peak-env-info-upon-open)
    (add-hook! update-hook
	       (lambda (snd)
		 (if save-peak-env-info?
		     (do ((i 0 (1+ i)))
			 ((= i (chans snd)))
		       (let ((peak-file (mus-expand-filename (peak-env-info-file-name snd i))))
			 (if (file-exists? peak-file)
			     (delete-file peak-file)))))
		 #f))

    (lambda (snd)
      (if (and save-peak-env-info?
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
			     (lambda args args)))))))))
    ))

(add-hook! close-hook save-peak-env-info)

(add-hook! exit-hook 
	   (lambda () 
	     (for-each save-peak-env-info (sounds))))

