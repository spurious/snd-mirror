(use-modules (ice-9 optargs) (ice-9 format))

(define *srate* (default-output-srate))
(define *file-name* "test.snd")
(define *channels* (default-output-chans))
(define *data-format* (default-output-format))
(define *header-type* (default-output-type))
(define *to-snd* #t)

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
				  (to-snd *to-snd*)
				  (scaled-by #f))
  (let ((old-srate (mus-srate))
	(old-output *output*)
	(old-reverb *reverb*)
	(output-1 output)) ; protect during nesting
    (dynamic-wind 

     (lambda () 
       (set! (mus-srate) srate))

     (lambda ()

       (if continue-old-file
	   (begin
	     (set! *output* (continue-sample->file output-1))
	     (set! (mus-srate) (mus-sound-srate output-1))
	     (if reverb (set! *reverb* (continue-sample->file revfile)))
	     (let ((ind (find-sound output-1)))
	       (if ind (close-sound ind))))
	   (begin
	     (if (file-exists? output-1) (delete-file output-1))
	     (if (and reverb (file-exists? revfile)) (delete-file revfile))
	     (set! *output* (make-sample->file output-1 channels data-format header-type comment))
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
	       (mus-close *reverb*)))
	 (mus-close *output*)
	 (if statistics
	     (set! cycles (/ (- (get-internal-real-time) start) 100)))
	 (if to-snd
	     (let ((snd-output (open-sound output-1)))
	       (set! (sync snd-output) #t)
	       (if statistics
		   (snd-print 
		    (format #f "~A:~%  maxamp: ~A,~%  compute time: ~A~%"
			    output-1
			    (maxamp snd-output #t)
			    cycles)))
	       (if scaled-to
		   (scale-to scaled-to snd-output)
		   (if scaled-by
		       (scale-by scaled-by snd-output)))
	       (if play (play-and-wait snd-output))
	       (update-time-graph snd-output)))
	 output-1))

     (lambda () 
       (if *reverb*
	   (begin
	     (mus-close *reverb*)
	     (set! *reverb* old-reverb)))
       (if *output*
	   (begin
	     (mus-close *output*)
	     (set! *output* old-output)))
       (set! (mus-srate) old-srate)))))

(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))

(defmacro with-temp-sound (args . body)
  ;; with-sound but using tempnam for output (can be over-ridden by explicit :output) and does not open result in Snd
  `(let ((old-file-name *file-name*)
	 (old-to-snd *to-snd*))
     (set! *file-name* (snd-tempnam))
     (set! *to-snd* #f)
     (let ((val (with-sound-helper (lambda () ,@body) ,@args)))
       (set! *to-snd* old-to-snd)
       (set! *file-name* old-file-name)
       val)))

;;; first stab at def-clm-struct

(defmacro* def-clm-struct (name #:rest fields)
  ;; (def-clm-struct fd loc (chan 1))
  (let* ((sname (if (string? name) name (symbol->string name)))
	 (field-names (map (lambda (n)
			     (symbol->string (if (list? n) (car n) n)))
			   fields)))
    `(begin
       (define ,(string->symbol (string-append sname "?"))
	 (lambda (obj)
	   (and (list? obj)
		(eq? (car obj) ',(string->symbol sname)))))
       (define* (,(string->symbol (string-append "make-" sname)) #:key ,@fields)
	 (list ',(string->symbol sname)
	       ,@(map string->symbol field-names)))
       (add-clm-type ,sname)
       ,@(map (let ((ctr 1))
		(lambda (n)
		  (let ((val `(define ,(string->symbol (string-append sname "-" n))
				(make-procedure-with-setter
				 (lambda (arg)
				   (list-ref arg ,ctr))
				 (lambda (arg val)
				   (list-set! arg ,ctr val))))))
		    (add-clm-field (string-append sname "-" n) ctr)
		    (set! ctr (1+ ctr))
		    val)))
	      field-names))))


;;; sound-let
;;;
;;; (with-sound () (sound-let ((a () (fm-violin 0 .1 440 .1))) (mus-mix "test.snd" a)))

(defmacro sound-let (snds . body) 
  `(let ((temp-files '())
	 (old-hook-list (hook->list new-sound-hook))) ; save old new-sound-hook (nested sound-lets etc)
     (begin
       (reset-hook! new-sound-hook)
       (add-hook! new-sound-hook (lambda (file)       ; save current sound-let temp file list
				   (set! temp-files (cons file temp-files))))
       (let ((val (let ,(map (lambda (arg) 
			       (if (> (length arg) 2)
				                      ; if with-sound, embed with-temp-sound
				   `(,(car arg) (with-temp-sound ,(cadr arg) ,@(cddr arg)))
				   arg))              ; else use direct (normal var in the let)
			     snds)
		    ,@body)))                         ; sound-let body
	 (for-each (lambda (file)                     ; clean up all local temps
		     (if (file-exists? file)
			 (delete-file file)))
		   temp-files)
	 (reset-hook! new-sound-hook)                 ; restore old new-sound-hook (should this happen before ,@body?)
	 (if (not (null? old-hook-list))
	     (for-each (lambda (proc)
			 (add-hook! new-sound-hook proc))
		       old-hook-list))
	 val))))                                      ; return body result

