(use-modules (ice-9 optargs) (ice-9 format))

;;; -------- with-sound for Snd!
;;;
;;; this is just a bare beginning, but it's the basic idea...
;;;
;;; in Common Lisp this is essentially
;;;    (defmacro with-sound ((&key (srate 22050) ...) &body body) (let (...) ,.body))
;;; so that a call looks like 
;;;    (with-sound (:srate 44100) (fm-violin 0 1 440 .1))

;;; here's a better version courtesy of Kalle Olavi Niemitalo
;;; but it doesn't seem to work in Guile 1.4 (it needs 1.4.1)

(define *snd-srate* (default-output-srate))
(define *snd-file-name* "test.snd")
(define *snd-channels* (default-output-chans))
(define *snd-explode* #f)                           ;with-mix-tags
(define *snd-data-format* (default-output-format))
(define *snd-header-type* (default-output-type))

(define* (with-sound-helper thunk 
			    #:key (srate *snd-srate*) 
			          (output *snd-file-name*) 
				  (channels *snd-channels*)
				  (explode *snd-explode*)
				  (header-type *snd-header-type*)
				  (data-format *snd-data-format*)
				  (comment #f)
				  (continue-old-file #f)
				  (statistics #f)
				  (scaled-to #f)
				  (scaled-by #f))
  (let ((old-srate (mus-srate))
	(old-tags (with-mix-tags)))
    (dynamic-wind 

     (lambda () 
       (set! (mus-srate) srate)
       (set! (with-mix-tags) explode))

     (lambda ()
       (let ((ind #f))
	 (if (not continue-old-file)
	     (begin
	       (if (find-sound output) 
		   (close-sound (find-sound output)))
	       (set! ind (new-sound output header-type data-format srate channels comment)))
	     (set! ind (find-sound output)))
	 (if (number? ind)
	     (let ((start (if statistics (get-internal-real-time))))
	       (thunk)
	       (if statistics
		   (snd-print 
		    (format #f "~A: maxamp: ~A, time: ~A"
			    output
			    (maxamp ind #t)
			    (/ (- (get-internal-real-time) start) 100))))
	       (if scaled-to
		   (scale-to scaled-to ind)
		   (if scaled-by
		       (scale-by scaled-by ind)))))))

     (lambda () 
       (set! (with-mix-tags) old-tags)
       (set! (mus-srate) old-srate)))))

(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))
