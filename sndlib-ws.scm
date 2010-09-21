;;; with-sound for a sndlib-only context (no Snd editor)

(provide 'sndlib-ws.scm)

(define *clm-srate* 44100)
(define *clm-file-name* "test.snd")
(define *clm-channels* 1)
(define *clm-data-format* mus-lfloat)
(define *clm-header-type* mus-next)
(define *clm-verbose* #f)
(define *clm-play* #f)
(define *clm-statistics* #f)
(define *clm-reverb* #f)
(define *clm-reverb-channels* 1)
(define *clm-reverb-data* '())
(define *clm-table-size* 512)
(define *clm-file-buffer-size* 65536)
(define *clm-locsig-type* mus-interp-linear)
(define *clm-clipped* #t)
(define *clm-array-print-length* 12)
(define *clm-player* #f) 
(define *clm-notehook* #f)
(define *clm-with-sound-depth* 0) ; for CM, not otherwise used
(define *clm-default-frequency* 0.0)
(define *clm-safety* 0)
(define *clm-delete-reverb* #f)   ; should with-sound clean up reverb stream
(define *clm-threads* 4)
(define *clm-output-safety* 0)    ; if 1, assume output buffers will not need to be flushed except at the very end


(define (times->samples beg dur) 
  "(times->samples beg dur) converts beg and (+ beg dur) to samples, returning both in a list"
  (list (seconds->samples beg) (seconds->samples (+ beg dur))))


;;; -------- definstrument --------

;(define definstrument define*) -- old form 2-Nov-05

(define *definstrument-hook* #f) ; for CM

(defmacro definstrument (args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names '()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a))
			  (if (symbol? a)
			      (set! arg-names (cons a arg-names))
			      (set! arg-names (cons (car a) arg-names)))))
		    targs)
		   (reverse arg-names)))
	 (doc (if (string? (car body))
		  (let ((val (car body)))
		    (set! body (cdr body))
		    val)
		  "no help")))
  `(begin 
     (define* (,name ,@targs)
       ,doc
       (if *clm-notehook*
	   (*clm-notehook* (symbol->string ',name) ,@utargs))
       ((lambda () ; for inner defines, if any
	  ,@body)))
     ,@(if *definstrument-hook*
           (list (*definstrument-hook* name targs))
           (list)))))



;;; -------- with-sound --------

(define* (with-sound-helper thunk 
			    (srate *clm-srate*) 
			    (output *clm-file-name*) 
			    (channels *clm-channels*)
			    (header-type *clm-header-type*)
			    (data-format *clm-data-format*)
			    (comment #f)
			    (verbose *clm-verbose*)
			    (reverb *clm-reverb*)
			    (revfile "test.rev")
			    (reverb-data *clm-reverb-data*)
			    (reverb-channels *clm-reverb-channels*)
			    (continue-old-file #f)
			    (statistics *clm-statistics*)
			    (scaled-to #f)
			    (scaled-by #f)
			    (play *clm-play*)
			    (clipped 'unset)
			    (notehook *clm-notehook*)               ; (with-sound (:notehook (lambda args (display args))) (fm-violin 0 1 440 .1))
			    (ignore-output #f)
			    (thread-output #f)
			    (thread-reverb #f)
			    (output-safety *clm-output-safety*))
  "with-sound-helper is the business portion of the with-sound macro"
  (let* ((old-srate (mus-srate))
	 (old-*output* *output*)
	 (old-*reverb* *reverb*)
	 (in-debugger #f)
	 (old-notehook *clm-notehook*)
	 (old-verbose *clm-verbose*)
	 (output-to-file (string? output))
	 (output-1 (if (and output-to-file
			    (or scaled-to scaled-by))
		       (string-append output ".temp") 
		       output))                    ; protect during nesting
	 (reverb-1 revfile)
	 (reverb-to-file (and reverb (string? revfile))))

    (if ignore-output
	(begin
	  (set! output-1 *clm-file-name*)
	  (set! output-to-file (string? output-1))))

    (dynamic-wind 

     (lambda () 
       (set! *clm-verbose* verbose)
       (set! *clm-notehook* notehook)
       (set! (clm-table-size) *clm-table-size*)
       (set! (clm-default-frequency) *clm-default-frequency*)
       (set! (mus-file-buffer-size) *clm-file-buffer-size*)
       (set! (locsig-type) *clm-locsig-type*)
       (set! (mus-array-print-length) *clm-array-print-length*)
       (if (equal? clipped 'unset)
	   (if (and (or scaled-by scaled-to)
		    (member data-format (list mus-bfloat mus-lfloat mus-bdouble mus-ldouble)))
	       (set! (mus-clipping) #f)
	       (set! (mus-clipping) *clm-clipped*))
	   (set! (mus-clipping) clipped))
       (set! (mus-srate) srate))

     (lambda ()
       (if (not thread-output)
	   (begin
	     
	     (if output-to-file
		 (begin
		   (if continue-old-file
		       (begin
			 (set! *output* (continue-sample->file output-1))
			 (set! (mus-srate) (mus-sound-srate output-1)))
		       (begin
			 (if (file-exists? output-1) 
			     (delete-file output-1))
			 (set! *output* (make-sample->file output-1 channels data-format header-type comment))))
		   (if *output*
		       (set! (mus-safety *output*) output-safety)))
		 (begin
		   (if (not continue-old-file)
		       (if (vct? output-1)
			   (vct-fill! output-1 0.0)
			   (if (sound-data? output-1)
			       (sound-data-fill! output-1 0.0))))
		   (set! *output* output-1)))

	     (if reverb
		 (if reverb-to-file
		     (begin
		       (if continue-old-file
			   (set! *reverb* (continue-sample->file reverb-1))
			   (begin
			     (if (file-exists? reverb-1) 
				 (delete-file reverb-1))
			     (set! *reverb* (make-sample->file reverb-1 reverb-channels data-format header-type))))
		       (if *reverb*
			   (set! (mus-safety *reverb*) output-safety)))
		     (begin
		       (if (not continue-old-file)
			   (if (vct? reverb-1)
			       (vct-fill! reverb-1 0.0)
			       (if (sound-data? reverb-1)
				   (sound-data-fill! reverb-1 0.0))))
		       (set! *reverb* reverb-1)))))

	   ;; else thread-output
	   (begin
	     (if (file-exists? output-1) 
		 (delete-file output-1))
	     (set! (thread-output) (make-sample->file output-1 channels data-format header-type comment))
	     (if thread-reverb
		 (begin
		   (if (file-exists? reverb-1) 
		       (delete-file reverb-1))
		   (set! (thread-reverb) (make-sample->file reverb-1 reverb-channels data-format header-type))))
	     (set! statistics #f)
	     ))

       (let ((start (if statistics (get-internal-real-time)))
	     (flush-reverb #f)
	     (cycles 0)
	     (revmax #f))

	 (catch 'mus-error
		thunk
		(lambda args
		  (display (format #f ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args)))
		  (set! flush-reverb #t)))
		  
	 (if (and reverb 
		  (not flush-reverb)) ; i.e. not interrupted by error and trying to jump out
	     (begin
	       (if thread-reverb
		   (mus-close (thread-reverb))
		   (if reverb-to-file
		       (mus-close *reverb*)))
	       (if statistics 
		   (if reverb-to-file
		       (set! revmax (cadr (mus-sound-maxamp reverb-1)))
		       (if (vct? reverb-1)
			   (set! revmax (vct-peak reverb-1))
			   (if (sound-data? reverb-1)
			       (set! revmax (sound-data-peak reverb-1))))))
	       (if (not thread-reverb)
		   (begin
		     (if reverb-to-file
			 (set! *reverb* (make-file->sample reverb-1)))
		     (apply reverb reverb-data)
		     (if reverb-to-file
			 (mus-close *reverb*))
		     (if (and reverb-to-file *clm-delete-reverb*)
			 (delete-file reverb-1))))))

	 (if thread-output
	     (mus-close (thread-output))
	     (if output-to-file
		 (mus-close *output*)))

	 (if statistics 
	     (begin
	       (set! cycles (exact->inexact (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
	       (display (format #f "~%;~A:~%  maxamp~A:~{ ~,4F~}~%~A  compute time: ~,3F~%"
				(if output-to-file
				    (if (or scaled-to scaled-by)
					(substring output-1 0 (- (string-length output-1) 5))
					output-1)
				    (if (vct? output-1) "vct" 
					(if (sound-data? output-1) "sound-data"
					    (if (procedure? output-1) "function" 
						"flush"))))
				(if (or scaled-to scaled-by) 
				    " (before scaling)" 
				    "")
				(if output-to-file
				    (let ((lst (mus-sound-maxamp output-1)))
				      (do ((i 0 (+ i 2)))
					  ((>= i (length lst)))
					(list-set! lst i (/ (list-ref lst i) (mus-srate))))
				      lst)
				    (if (vct? output-1)
					(list (vct-peak output-1))
					(if (sound-data? output-1)
					    (sound-data-maxamp output-1)
					    0.0)))
				(if revmax (format #f "  rev max: ~,4F~%" revmax) "")
				cycles))))

	 (if (or scaled-to scaled-by)
	     (if output-to-file
		 (let ((scaling
			(or scaled-by
			    (let* ((mx-lst (mus-sound-maxamp output-1))
				   (mx (if (not (null? mx-lst)) (cadr mx-lst) 1.0)))
			      (do ((i 1 (+ i 2)))
				  ((>= i (length mx-lst)) (/ scaled-to mx))
				(set! mx (max mx (list-ref mx-lst i)))))))
		       (out-file (substring output-1 0 (- (string-length output-1) 5))))
		   (mus-sound-close-output (mus-sound-open-output out-file srate channels data-format header-type) 0)
		   (mus-mix out-file output-1 0 (mus-sound-frames output-1) 0 (make-scalar-mixer channels scaling))
		   (delete-file output-1)
		   (set! output-1 (substring output-1 0 (- (string-length output-1) 5))))

		 (if (vct? output-1)
		     (if scaled-to
			 (let ((pk (vct-peak output-1)))
			   (if (> pk 0.0)
			       (vct-scale! output-1 (/ scaled-to pk))))
			 (vct-scale! output-1 scaled-by))
		     (if (sound-data? output-1)
			 (if scaled-to
			     (let ((pk (sound-data-peak output-1)))
			       (if (> pk 0.0)
				   (sound-data-scale! output-1 (/ scaled-to pk))))
			     (sound-data-scale! output-1 scaled-by))))))

	 (if (and *clm-player* play output-to-file)
	     (*clm-player* output-1)))

       output-1)

     (lambda () 
       (set! *clm-verbose* old-verbose)
       (set! *clm-notehook* old-notehook)
       (if *reverb*
	   (begin
	     (if thread-reverb
		 (mus-close (thread-reverb))
		 (mus-close *reverb*))
	     (set! *reverb* old-*reverb*)))
       (if *output*
	   (begin
	     (if thread-output
		 (mus-close (thread-output))
		 (if (mus-output? *output*)
		     (mus-close *output*)))
	     (set! *output* old-*output*)))
       (set! (mus-srate) old-srate)))))


(defmacro with-sound (args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))



;;; -------- with-temp-sound --------

(defmacro with-temp-sound (args . body)
  `(let ((old-file-name *clm-file-name*))
     ;; with-sound but using tempnam for output (can be over-ridden by explicit :output)
     (dynamic-wind
	 (lambda () 
	   (set! *clm-file-name* (tmpnam)))
	 (lambda ()
	   (with-sound-helper (lambda () ,@body) ,@args)) ; dynamic-wind returns this as its result
	 (lambda ()
	   (set! *clm-file-name* old-file-name)))))


;;; -------- clm-load --------

(define (clm-load file . args) 
  "(clm-load file . args) loads 'file' in the context of with-sound"
  (apply with-sound-helper (lambda () (load file)) args))



;;; -------- sound-let --------
;;;
;;; (with-sound () (sound-let ((a () (fm-violin 0 .1 440 .1))) (mus-mix "test.snd" a)))

(defmacro sound-let (snds . body) 
  `(let ((temp-files '()))
     (begin
       (let ((val (let ,(map (lambda (arg) 
			       (if (> (length arg) 2)
				   `(,(car arg) (with-temp-sound ,(cadr arg) ,@(cddr arg)))
				   arg))
			     snds)
		    ,@body)))                         ; sound-let body
	 (for-each (lambda (file)                     ; clean up all local temps
		     (if (and (string? file)          ; is it a file? (might be a vct or sound-data object)
			      (file-exists? file))
			 (delete-file file)))
		   temp-files)
	 val))))                                      ; return body result



;;; -------- Common Music --------

(define* (init-with-sound
	  (srate *clm-srate*) 
	  (output *clm-file-name*) 
	  (channels *clm-channels*)
	  (header-type *clm-header-type*)
	  (data-format *clm-data-format*)
	  (comment #f)
	  ;(verbose *clm-verbose*) ; why is this commented out?
	  (reverb *clm-reverb*)
	  (revfile "test.rev")
	  (reverb-data *clm-reverb-data*)
	  (reverb-channels *clm-reverb-channels*)
	  (continue-old-file #f)
	  (statistics *clm-statistics*)
	  (scaled-to #f)
	  (play *clm-play*)
	  (scaled-by #f))
  "(init-with-sound . args) is the first half of with-sound; it sets up the CLM output choices, reverb, etc. Use \
finish-with-sound to complete the process."
  (let ((old-srate (mus-srate))
	(start (if statistics (get-internal-real-time)))
	(output-to-file (string? output))
	(reverb-to-file (and reverb (string? revfile))))
    (if output-to-file
	(if continue-old-file
	    (begin
	      (set! *output* (continue-sample->file output))
	      (set! (mus-srate) (mus-sound-srate output)))
	    (begin
	      (if (file-exists? output) 
		  (delete-file output))
	      (set! *output* (make-sample->file output channels data-format header-type comment))))
	(begin
	  (if (not continue-old-file)
	      (if (vct? output)
		  (vct-fill! output 0.0)
		  (sound-data-fill! output 0.0)))
	  (set! *output* output)))

    (if reverb
	(if reverb-to-file
	    (if continue-old-file
		(set! *reverb* (continue-sample->file revfile))
		(begin
		  (if (file-exists? revfile) 
		      (delete-file revfile))
		  (set! *reverb* (make-sample->file revfile reverb-channels data-format header-type))))
	    (begin
	      (if (not continue-old-file)
		  (if (vct? revfile)
		      (vct-fill! revfile 0.0)
		      (sound-data-fill! revfile 0.0)))
	      (set! *reverb* revfile))))

    (list 'with-sound-data
	  output
	  reverb
	  revfile
	  old-srate
	  statistics
	  #f ;to-snd
	  scaled-to
	  scaled-by
	  play
	  reverb-data
	  start)))

(define (finish-with-sound wsd)
  "(finish-with-sound wsd) closes the notelist process started by init-with-sound"
  (if (eq? (car wsd) 'with-sound-data)
      (let ((cycles 0)
	    (output (list-ref wsd 1))
	    (reverb (list-ref wsd 2))
	    (revfile (list-ref wsd 3))
	    (old-srate (list-ref wsd 4))
	    (statistics (list-ref wsd 5))
	    ;(to-snd (list-ref wsd 6))
	    (scaled-to (list-ref wsd 7))
	    (scaled-by (list-ref wsd 8))
	    (play (list-ref wsd 9))
	    (reverb-data (list-ref wsd 10))
	    (start (list-ref wsd 11)))

	(if reverb
	    (begin
	      (mus-close *reverb*)
	      (if (string? revfile)
		  (set! *reverb* (make-file->sample revfile))
		  (set! *reverb* revfile))
	      (apply reverb reverb-data)
	      (mus-close *reverb*)))
	(if (mus-output? *output*)
	    (mus-close *output*))

	(if statistics
	    (set! cycles (/ (- (get-internal-real-time) start) 100)))
	(set! (mus-srate) old-srate)
	output)
      (throw 'wrong-type-arg
	     (list "finish-with-sound" wsd))))


(define wsdat-play ; for cm
  (make-procedure-with-setter
   (lambda (w)
     "accessor for play field of init-with-sound struct"
     (list-ref w 9))
   (lambda (w val)
     (list-set! w 9 val))))


(define ->frequency
  (let ((main-pitch (/ 440.0 (expt 2.0 (/ 57 12)))) ; a4 = 440Hz is pitch 57 in our numbering
	(last-octave 0)                             ; octave number can be omitted
	(ratios (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0)))
    (lambda* (pitch pythagorean)          ; pitch can be pitch name or actual frequency
      "(->frequency pitch pythagorean) returns the frequency (Hz) of the 'pitch', a CLM/CM style note name as a \
symbol: 'e4 for example.  If 'pythagorean', the frequency calculation uses small-integer ratios, rather than equal-tempered tuning."
      (if (symbol? pitch)
	  (let* ((name (string-downcase (symbol->string pitch)))
		 (base-char (string-ref name 0))
		 (sign-char (and (> (string-length name) 1)
				 (not (char-numeric? (string-ref name 1)))
				 (not (char=? (string-ref name 1) #\n))
				 (string-ref name 1)))
		 (octave-char (if (and (> (string-length name) 1)
				       (char-numeric? (string-ref name 1))) 
				  (string-ref name 1)
				  (if (and (> (string-length name) 2) 
					   (char-numeric? (string-ref name 2)))
				      (string-ref name 2)
				      #f)))
		 (base (modulo (+ 5 (- (char->integer base-char) (char->integer #\a))) 7)) ; c-based (diatonic) octaves
		 (sign (if (not sign-char) 0 (if (char=? sign-char #\f) -1 1)))
		 (octave (if octave-char (- (char->integer octave-char) (char->integer #\0)) last-octave))
		 (base-pitch (+ sign (case base ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
		 (et-pitch (+ base-pitch (* 12 octave))))
	    (set! last-octave octave)
	    (if pythagorean
		(* main-pitch (expt 2 octave) (ratios base-pitch))
		(* main-pitch (expt 2.0 (/ et-pitch 12)))))
	  pitch))))


(define (->sample beg)
  "(->sample time-in-seconds) -> time-in-samples"
  (round (* (if (not (null? (sounds))) (srate) (mus-srate)) beg)))



;;; -------- defgenerator --------

;;;  :(defgenerator (osc :make-wrapper (lambda (gen) (set! (osc-freq gen) (hz->radians (osc-freq gen))) gen)) freq phase)
;;;  #<unspecified>
;;;  :(define hi (make-osc 440.0 0.0))
;;;  #<unspecified>
;;;  :hi
;;;  (osc 0.125378749798983 0.0)

;;; besides setting up the list accessors, the make function, and the type predicate, defgenerator
;;;   calls add-clm-field to tell run the type of each list element (only actually needed if
;;;   there are different types in use)
;;; it also adds the built-in methods mus-name, mus-reset, mus-run, and mus-describe (if they don't already exist), and
;;;   mus-frequency if a "frequency" field exists (treated as radians)
;;;   mus-phase if a "phase" or "angle" field exists
;;;   mus-scaler if "r" or "amplitude",
;;;   mus-order if "n" or "order"
;;;   mus-offset if "ratio" (mimics nrxy*)

(define (find-if pred l)
  (cond ((null? l) #f)
	((pred (car l)) (car l))
	(else (find-if pred (cdr l)))))

(defmacro defgenerator (struct-name . fields)
  (let* ((name (if (list? struct-name) (car struct-name) struct-name))

	 (wrapper (or (and (list? struct-name)
			   (or (and (> (length struct-name) 2)
				    (equal? (struct-name 1) :make-wrapper)
				    (struct-name 2))
			       (and (= (length struct-name) 5)
				    (equal? (struct-name 3) :make-wrapper)
				    (struct-name 4))))
		      (lambda (gen) gen)))

	 (sname (if (string? name) name (symbol->string name)))

	 (field-names (map (lambda (n)
			     (symbol->string (if (list? n) (car n) n)))
			   fields))

	 (field-types (map (lambda (n)
			     (if (and (list? n) (cadr n) (eq? (cadr n) :type)) 
				 (snd-error (format #f ":type indication for defgenerator (~A) field (~A) should be after the default value" name n)))
			     (if (and (list? n)
				      (= (length n) 4)
				      (eq? (n 2) :type))
				 (n 3)
				 (if (and (list? n)
					  (= (length n) 2))
				     (if (number? (cadr n))
					 (if (rational? (cadr n))
					     'int
					     'float)
					 (if (string? (cadr n))
					     'string
					     (if (char? (cadr n))
						 'char
						 (if (or (equal? (cadr n) #t)
							 (equal? (cadr n) #f))
						     'boolean
						     'float))))
				     'float)))
			   fields))

	 (original-methods (or (and (list? struct-name)
				    (or (and (> (length struct-name) 2)
					     (equal? (struct-name 1) :methods)
					     (struct-name 2))
					(and (= (length struct-name) 5)
					     (equal? (struct-name 3) :methods)
					     (struct-name 4))))
			       (list)))

	 (method-exists? (lambda (method)
			   (and (not (null? original-methods))
				(find-if (lambda (g)
					   (and (list? g)
						(list? (cadr g))
						(eq? (car (cadr g)) method)))
					 (cdr original-methods)))))

	 (phase-field-name (and (not (method-exists? 'mus-phase))
				(let ((fld (find-if (lambda (name) 
						      (or (string=? name "phase") 
							  (string=? name "angle")))
						    field-names)))
				  (and fld (string-append "-" fld)))))

	 (frequency-field-name (and (not (method-exists? 'mus-frequency))
				    (find-if (lambda (name) 
					       (string=? name "frequency"))
					     field-names)
				    "-frequency"))

	 (offset-field-name (and (not (method-exists? 'mus-offset))
				 (find-if (lambda (name) 
					    (string=? name "ratio"))
					  field-names)
				 "-ratio"))

	 (scaler-field-name (and (not (method-exists? 'mus-scaler))
				 (let ((fld (find-if (lambda (name) 
						       (or (string=? name "r")
							   (string=? name "amplitude")))
						     field-names)))
				   (and fld (string-append "-" fld)))))

	 (order-field-name (and (not (method-exists? 'mus-order))
				(let ((fld (find-if (lambda (name) 
						      (or (string=? name "n") 
							  (string=? name "order")))
						    field-names)))
				  (and fld (string-append "-" fld)))))

	 ;; using append to splice out unwanted entries
	 (methods `(append ,original-methods
			   
			   (if ,phase-field-name
			       (list 
				(list 'mus-phase
				      (lambda (g)
					(,(string->symbol (string-append sname (or phase-field-name "oops"))) g))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or phase-field-name "oops"))) g) val))))
			       (list))
			   
			   (if ,frequency-field-name
			       (list 
				(list 'mus-frequency
				      (lambda (g)
					(radians->hz (,(string->symbol (string-append sname (or frequency-field-name "oops"))) g)))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or frequency-field-name "oops"))) g) (hz->radians val))
					val)))
			       (list))
			   
			   (if ,offset-field-name
			       (list 
				(list 'mus-offset
				      (lambda (g)
					(,(string->symbol (string-append sname (or offset-field-name "oops"))) g))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or offset-field-name "oops"))) g) val)
					val)))
			       (list))
			   
			   (if ,order-field-name
			       (list  ; not settable -- maybe use mus-length?
				(list 'mus-order
				      (lambda (g)
					(,(string->symbol (string-append sname (or order-field-name "oops"))) g))))
			       (list))
			   
			   (if ,scaler-field-name
			       (list 
				(list 'mus-scaler
				      (lambda (g)
					(,(string->symbol (string-append sname (or scaler-field-name "oops"))) g))
				      (lambda (g val)
					(set! (,(string->symbol (string-append sname (or scaler-field-name "oops"))) g) val))))
			       (list))
			   
			   (if ,(not (method-exists? 'mus-describe))
			       (list 
				(list 'mus-describe
				      (lambda (g)
					(let ((desc (mus-name g))
					      (first-time #t))
					  (for-each
					   (lambda (field)
					     (set! desc (string-append desc 
								       (format #f "~A~A: ~A"
									       (if first-time " " ", ")
									       field
									       (if (string=? field "frequency")
										   (radians->hz ((symbol->value (string->symbol (string-append ,sname "-" field))) g))
										   ((symbol->value (string->symbol (string-append ,sname "-" field))) g)))))
					     (set! first-time #f))
					   (list ,@field-names))
					  desc))))
			       (list))
			   
			   (if ,(not (method-exists? 'mus-run))
			       (list
				(list 'mus-run
				      (lambda (g arg1 arg2)
					(,(string->symbol sname) g arg1)))) ; this assumes the run-time function takes two args
			       (list))
			   
			   (if ,(not (method-exists? 'mus-reset))
			       (list 
				(list 'mus-reset
				      (lambda (g)
					(for-each
					 (lambda (name type orig)
					   (if (or (not (string=? type "clm"))
						   (not ((symbol->value (string->symbol (string-append ,sname "-" name))) g)))
					       (set! ((string->symbol (string-append ,sname "-" name)) g) orig)
					       (mus-reset ((symbol->value (string->symbol (string-append ,sname "-" name))) g))))
					 (list ,@field-names)
					 (list ,@(map symbol->string field-types))
					 (list ,@(map (lambda (n)
							(if (and (list? n)
								 (>= (length n) 2))
							    (cadr n)
							    0.0))
						      fields))))))
			       (list))
			   
			   (if ,(not (method-exists? 'mus-name))
			       (list 
				(list 'mus-name
				      (lambda (g) 
					,sname)
				      (lambda (g new-name)
					(set-car! (cdr (assoc 'mus-name (g (- (length g) 1))))
						  (lambda (g) 
						    new-name))))) ; depend on closures?
			       (list)))))
    
    `(begin
       (define ,(string->symbol (string-append sname "?"))
	 (lambda (obj)
	   (and (list? obj)
		(eq? (car obj) ',(string->symbol sname)))))

       (define ,(string->symbol (string-append sname "-methods"))
	 (lambda ()
	   ,methods))

       (define* (,(string->symbol (string-append "make-" sname))
		 ,@(map (lambda (n)
			  (if (and (list? n)
				   (>= (length n) 2))
			      (list (car n) (cadr n))
			      (list n 0.0)))
			fields))
	 (,wrapper (if (list? ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)
			     ,methods)
		       (list ',(string->symbol sname)
			     ,@(map string->symbol field-names)))))

       ,@(map (let ((ctr 1))
		(lambda (n type)
		  (let ((val `(define ,(string->symbol (string-append sname "-" n))
				(make-procedure-with-setter
				 (lambda (arg)
				   "generator field accessor"
				   (arg ,ctr))
				 (lambda (arg val)
				   (list-set! arg ,ctr val))))))
		    (add-clm-field sname (string-append sname "-" n) ctr type)
		    (set! ctr (+ 1 ctr))
		    val)))
	      field-names field-types))))


(define def-clm-struct defgenerator)

(define (ws-interrupt? . args) #f)



;;; -------- with-threaded-sound --------

(defmacro with-threaded-sound (args . body)
  (if (and (provided? 'threads)
	   (not (= (optimization) 0)))
      (let ((split 
	     (lambda (l n k)
	       (define (split-1 s l n i)
		 (if (null? l)
		     (reverse s)
		     (if (= i n)
			 (split-1 (cons (car l) s) (cdr l) n 1)
			 (split-1 s (cdr l) n (+ i 1)))))
	       (split-1 '() l n (- n k))))

	    (remove-output-and-reverb-args
	     (lambda (lst)
	       (let ((badarg #f)
		     (new-args '()))
		 (for-each 
		  (lambda (arg)
		    (if badarg
			(set! badarg #f)
			(if (not (member arg (list :output :reverb :revfile :reverb-data :reverb-channels)))
			    (set! new-args (cons arg new-args))
			    (set! badarg #t))))
		  lst)
		 (reverse new-args)))))

	(let ((lists '()))
	  (do ((i 0 (+ 1 i)))
	      ((= i *clm-threads*))
	    (set! lists (cons (split body *clm-threads* i) lists)))

	  (let ((new-args (remove-output-and-reverb-args args)))

	    `(with-sound-helper 
	      (lambda ()
		(let ((threads '())
		      (thread-output (make-thread-variable))
		      (thread-reverb (and *reverb* (make-thread-variable)))
		      (mix-lock (make-lock))
		      (main-output *output*)
		      (main-reverb *reverb*))

		  (set! *output* thread-output)
		  (if thread-reverb (set! *reverb* thread-reverb))
		  
		  ,@(map
		     (lambda (expr)
		       `(set! threads (cons (make-thread 
					     (lambda ()
					       (let* ((reverb-tmp (and *reverb* (tmpnam)))
						      (tmp (with-sound-helper 
							    (lambda ()
							      ,@expr
							      #f)
							    :thread-output thread-output
							    :thread-reverb thread-reverb
							    :output (tmpnam)
							    :revfile reverb-tmp
							    :to-snd #f
							    ,@new-args
							    )))
						 (grab-lock mix-lock)
						 (display (format #f "mix ~S [~D]~%" tmp (mus-safety main-output)))
						 (if (= (mus-safety main-output) 1)
						     (begin
						       (sample->file+ main-output (thread-output))
						       (mus-close (thread-output)))
						     (mus-mix main-output tmp))
						 (if *reverb*
						     (if (= (mus-safety main-output) 1)
							 (begin
							   (sample->file+ main-reverb (thread-reverb))
							   (mus-close (thread-reverb)))
							 (mus-mix main-reverb reverb-tmp)))
						 (release-lock mix-lock)
						 (delete-file tmp))))
					    threads)))
		     lists)
		  
		  (for-each 
		   (lambda (thread) 
		     (join-thread thread))
		   threads)
		  
		  (if main-reverb (set! *reverb* main-reverb))
		  (set! *output* main-output)))
	      
	      ,@args))))
      
      `(with-sound-helper
	(lambda ()
	  ,@body)
	,@args)))


;;; --------------------------------------------------------------------------------
;;;
;;; functions from Snd that are used in some instruments
;;;   these replacements assume that the Snd functions are not present

(define (file-name name) 
  (if (string? name) 
      (mus-expand-filename name) 
      (mus-file-name name)))

(define srate mus-sound-srate)

(define (channels obj)
  (if (string? obj)
      (mus-sound-chans obj)
      (mus-channels obj)))

;;; I think length is handled by s7 for all types

(define (frames obj)
  (if (string? obj)
      (mus-sound-frames obj)
      (length obj)))


(define snd-print display)
(define snd-warning display)
(define snd-display (lambda args (apply format (append (list #t) (cdr args)))))
(define (snd-error str) (error 'mus-error str))
(define snd-tempnam tmpnam)

