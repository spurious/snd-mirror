(use-modules (ice-9 optargs) (ice-9 format))

(define *srate* (default-output-srate))
(define *file-name* "test.snd")
(define *channels* (default-output-chans))
(define *data-format* (default-output-format))
(define *header-type* (default-output-type))

(define *reverb* #f) ; these are sample->file (outa) gens
(define *output* #f)

(define* (with-sound-helper thunk 
			    #:key (srate *srate*) 
			          (output *file-name*) 
				  (channels *channels*)
				  (header-type *header-type*)
				  (data-format *data-format*)
				  (comment #f)
				  (reverb #f)
				  (revfile "test.rev")
				  (reverb-data #f)
				  (continue-old-file #f)
				  (statistics #f)
				  (scaled-to #f)
				  (play #f)
				  (scaled-by #f))
  (let ((old-srate (mus-srate)))
    (dynamic-wind 

     (lambda () 
       (set! (mus-srate) srate))

     (lambda ()

       (if continue-old-file
	   (begin
	     (set! *output* (continue-sample->file output))
	     (set! (mus-srate) (mus-sound-srate output))
	     (if reverb (set! *reverb* (continue-sample->file revfile)))
	     (let ((ind (find-sound output)))
	       (if ind (close-sound ind))))
	   (begin
	     (if (file-exists? output) (delete-file output))
	     (if (and reverb (file-exists? revfile)) (delete-file revfile))
	     (set! *output* (make-sample->file output channels data-format header-type comment))
	     (if reverb (set! *reverb* (make-sample->file revfile 1 data-format header-type)))))

       (let ((start (if statistics (get-internal-real-time)))
	     (intp #f)
	     (cycles 0))
	 (catch 'interrupted
		thunk
		(lambda args 
		  (begin
		    (set! intp #t) 
		    (snd-print "interrupted!") 
		    args)))
	 (if (and reverb (not intp))
	     (begin
	       (mus-close *reverb*)
	       (set! *reverb* (make-file->sample revfile))
	       (reverb)
	       (mus-close *reverb*)
	       (set! *reverb* #f)))
	 (mus-close *output*)
	 (set! *output* #f)
	 (if statistics
	     (set! cycles (/ (- (get-internal-real-time) start) 100)))
	 (let ((snd-output (open-sound output)))
	   (set! (sync snd-output) #t)
	   (if statistics
	       (snd-print 
		(format #f "~A:~%  maxamp: ~A,~%  compute time: ~A"
			output
			(maxamp snd-output #t)
			cycles)))
	   (if scaled-to
	       (scale-to scaled-to snd-output)
	       (if scaled-by
		   (scale-by scaled-by snd-output)))
	   (if play (play-and-wait snd-output))
	   (update-time-graph snd-output)
	   output)))

     (lambda () 
       (if *reverb*
	   (begin
	     (mus-close *reverb*)
	     (set! *reverb* #f)))
       (if *output*
	   (begin
	     (mus-close *output*)
	     (set! *output* #f)))
       (set! (mus-srate) old-srate)))))

(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))
