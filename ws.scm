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

;;; second stab at def-clm-struct

(defmacro* def-clm-struct (name #:rest fields)
  ;; (def-clm-struct fd loc (chan 1))
  ;; (def-clm-struct hiho i x (s "hiho") (ii 3 :type int) (xx 0.0 :type float))
  ;; we need the :type indication if Snd's run macro is to have any hope of handling these structs as function args
  ;; should this code enforce the type indication?
  (let* ((sname (if (string? name) name (symbol->string name)))
	 (field-names (map (lambda (n)
			     (symbol->string (if (list? n) (car n) n)))
			   fields))
	 (field-types (map (lambda (n)
			     (if (and (list? n)
				      (= (length n) 4)
				      (eq? (list-ref n 2) :type))
				 (list-ref n 3)
				 #f))
			   fields)))
    `(begin
       (define ,(string->symbol (string-append sname "?"))
	 (lambda (obj)
	   (and (list? obj)
		(eq? (car obj) ',(string->symbol sname)))))
       (define* (,(string->symbol (string-append "make-" sname)) 
		 #:key ,@(map (lambda (n)
				(if (and (list? n)
					 (> (length n) 2))
				    (list (car n) (cadr n))
				    n))
			      fields))
	 (list ',(string->symbol sname)
	       ,@(map string->symbol field-names)))
       (add-clm-type ,sname)
       ,@(map (let ((ctr 1))
		(lambda (n type)
		  (let ((val `(define ,(string->symbol (string-append sname "-" n))
				(make-procedure-with-setter
				 (lambda (arg)
				   (list-ref arg ,ctr))
				 (lambda (arg val)
				   (list-set! arg ,ctr val))))))
		    (add-clm-field (string-append sname "-" n) ctr type)
		    (set! ctr (1+ ctr))
		    val)))
	      field-names field-types))))

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



;;; ---------------- Common Music ----------------

(define* (init-with-sound #:key 
			  (srate *srate*) 
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
	(start (if statistics (get-internal-real-time))))
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
    (list 'with-sound-data
	  output
	  reverb
	  revfile
	  old-srate
	  statistics
	  to-snd
	  scaled-to
	  scaled-by
	  play)))

(define (finish-with-sound wsd)
  (if (eq? (car wsd) 'with-sound-data)
      (let ((cycles 0)
	    (output (list-ref wsd 1))
	    (reverb (list-ref wsd 2))
	    (revfile (list-ref wsd 3))
	    (old-srate (list-ref wsd 4))
	    (statistics (list-ref wsd 5))
	    (to-snd (list-ref wsd 6))
	    (scaled-to (list-ref wsd 7))
	    (scaled-by (list-ref wsd 8))
	    (play (list-ref wsd 9)))
	(if reverb
	    (begin
	      (mus-close *reverb*)
	      (set! *reverb* (make-file->sample revfile))
	      (reverb)
	      (mus-close *reverb*)))
	(mus-close *output*)
	(if statistics
	    (set! cycles (/ (- (get-internal-real-time) start) 100)))
	(if to-snd
	    (let ((snd-output (open-sound output)))
	      (set! (sync snd-output) #t)
	      (if statistics
		  (snd-print 
		   (format #f "~A:~%  maxamp: ~A,~%  compute time: ~A~%"
			   output
			   (maxamp snd-output #t)
			   cycles)))
	      (if scaled-to
		  (scale-to scaled-to snd-output)
		  (if scaled-by
		      (scale-by scaled-by snd-output)))
	      (if play (play-and-wait snd-output))
	      (update-time-graph snd-output)))
	(set! (mus-srate) old-srate)
	output)
      (throw 'wrong-type-arg
	     (list "finish-with-sound" wsd))))

(define definstrument define*)
;;; this will be using #:optional etc -- not currently compatible with cm's formals->defobject
;;;
;;; *clm-channels* -> *channels*
;;; *clm-srate* -> *srate*
;;; *clm-with-sound-depth* -> not sure I need it in this context
