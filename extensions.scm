;;; various generally useful Snd extensions

;;; accessors for graph-style fields
;;; delete selected portion and smooth the splice
;;; eval over selection
;;; mix with result at original peak amp
;;; mix with envelope
;;; map-sound-files, for-each-sound-file, match-sound-files
;;; selection-members
;;; make-selection
;;; snd-debug
;;; read-listener-line


;;; -------- accessors for graph-style fields

(define time-graph-style
  (make-procedure-with-setter
   (lambda (snd chn)
     (logand (graph-style snd chn) #xff))
   (lambda (snd chn val)
     (set! (graph-style snd chn)
	   (logior (logand (graph-style snd chn) #xffff00)
		   val)))))

(define transform-graph-style
  (make-procedure-with-setter
   (lambda (snd chn)
     (let ((style (logand (ash (graph-style snd chn) -8) #xff)))
       (if (= style 0)
	   (time-graph-style snd chn)
	   (- style 1))))
   (lambda (snd chn val)
     ;; -1 will unset
     (set! (graph-style snd chn)
	   (logior (logand (graph-style snd chn) #xff00ff)
		   (ash (+ val 1) 8))))))

(define lisp-graph-style
  (make-procedure-with-setter
   (lambda (snd chn)
     (let ((style (logand (ash (graph-style snd chn) -16) #xff)))
       (if (= style 0)
	   (time-graph-style snd chn)
	   (- style 1))))
   (lambda (snd chn val)
     ;; -1 will unset
     (set! (graph-style snd chn)
	   (logior (logand (graph-style snd chn) #xffff)
		   (ash (+ val 1) 16))))))


(define (all-chans)
  (let ((sndlist '())
	(chnlist '()))
    (for-each (lambda (snd)
		(do ((i (1- (channels snd)) (1- i)))
		    ((< i 0))
		  (set! sndlist (cons snd sndlist))
		  (set! chnlist (cons i chnlist))))
	      (sounds))
    (list sndlist chnlist)))



;;; -------- mix with result at original peak amp

(define normalized-mix 
  (lambda (filename beg in-chan snd chn)
    "(normalized-mix filename beg in-chan snd chn) is like mix but mix result has same peak amp as unmixed snd/chn (returns scaler)"
    (let ((original-max-amp (maxamp snd chn)))
      (mix filename beg in-chan snd chn)
      (let ((new-max-amp (maxamp snd chn)))
	(if (not (= original-max-amp new-max-amp))
	    (let ((scaler (/ original-max-amp new-max-amp))
		  (old-sync (sync snd)))
	      (set! (sync snd) 0)
	      (scale-by scaler snd chn)
	      (set! (sync snd) old-sync)
	      scaler)
	    1.0)))))


;;;-------- mix with envelope on mixed-in file
;;;
;;; there are lots of ways to do this; this version uses functions from Snd, CLM, and Sndlib.

(define enveloped-mix
  (lambda (filename beg env)
    "(enveloped-mix filename beg env) mixes filename starting at beg with amplitude envelope env"
    (let ((len (mus-sound-frames filename))
	  (tmpfil (mus-sound-open-output "/tmp/tmp.snd" 22050 1 mus-bshort mus-next ""))
	  (mx (make-mixer 1 1.0))
	  (envs (make-vector 1))
	  (inenvs (make-vector 1)))
      (mus-sound-close-output tmpfil 0)
      (vector-set! inenvs 0 (make-env env :end len))
      (vector-set! envs 0 inenvs)
      (mus-mix "/tmp/tmp.snd" filename 0 len 0 mx envs)
      (mix "/tmp/tmp.snd" beg)
      (delete-file "/tmp/tmp.snd"))))

;(enveloped-mix "pistol.snd" 0 '(0 0 1 1 2 0))

;;; another way:

(define enveloped-mix-1
  (lambda (filename beg env)
    (as-one-edit
     (lambda ()
       (let* ((mix-id (mix filename beg))
	      (inchans (mix-chans mix-id)))
	 (do ((i 0 (1+ i)))
	     ((= i inchans))
	   (set! (mix-amp-env mix-id i) env)))))))


;;; -------- map-sound-files, match-sound-files
;;;
;;; apply a function to each sound in dir
;;;
;;;   (map-sound-files (lambda (n) (if (> (mus-sound-duration n) 10.0) (snd-print n))))

(define map-sound-files
  (lambda args
    "(map-sound-files func &optional dir) applies func to each sound file in dir"
    (map (car args) 
	 (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))))

(define for-each-sound-file
  (lambda args
    "(for-each-sound-file func &optional dir) applies func to each sound file in dir"
    (for-each 
     (car args) 
     (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))))

#!
 (for-each-sound-file
  (lambda (n) 
    (catch #t
           (lambda ()
 	      (if (not (null? (mus-sound-loop-info (string-append "/home/bil/sf/" n)))) 
 		  (snd-print (format #f "~%~A" n))))
           (lambda args #f)))
  "/home/bil/sf")
!#

(define match-sound-files
  (lambda args
    "(match-sound-files func &optional dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
    (let* ((func (car args))
	   (matches '()))
      (for-each
       (lambda (file)
	 (if (func file)
	     (set! matches (cons file matches))))
       (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
      matches)))
  
;;; we can use Guile's regexp support here to search for all .snd and .wav files:
#!
(let ((reg (make-regexp ".wav|.snd$")))
  (match-sound-files (lambda (file) (regexp-exec reg file))))
!#
;;; this argument to make-regexp is looking for *.wav and *.snd
;;; in fact, we could use regexp's in place of Snd's sound-files-in-directory.

;;; a prettier version might use a function written by Dirk Herrmann:

(define (filter-list pred? objects)
  (let loop ((objs objects)
	     (result '()))
    (cond ((null? objs) (reverse! result))
	  ((pred? (car objs)) (loop (cdr objs) (cons (car objs) result)))
	  (else (loop (cdr objs) result)))))

(define match-sound-files-1
  (lambda args
    (filter-list 
     (car args) 
     (sound-files-in-directory 
      (if (null? (cdr args)) 
	  "." 
	  (cadr args))))))

    
;;; -------- selection-members
;;;
;;; returns a list of lists of (snd chn): channels in current selection

(define selection-members
  (lambda ()
    (let ((sndlist '()))
      (if (selection?)
	  (map (lambda (snd)
		 (do ((i (1- (channels snd)) (1- i)))
		     ((< i 0))
		   (if (selection-member? snd i)
		       (set! sndlist (cons (list snd i) sndlist)))))
	       (sounds)))
      sndlist)))


;;; -------- make-selection

(define* (make-selection #:optional beg end snd chn)
  (let ((current-sound (or (and (number? snd) snd) (selected-sound))))
    (define (add-chan-to-selection s0 s1 s c)
      (set! (selection-member? s c) #t)
      (set! (selection-position s c) (or s0 0))
      (set! (selection-length s c) (- (or (and (number? s1) (1+ s1)) (frames s c)) (or s0 0))))
    (if (not (sound? current-sound))
	(throw 'no-such-sound (list "make-selection" beg end snd chn)))
    (let ((current-sync (sync current-sound)))
      (if (selection?) ; clear selection
	  (for-each
	   (lambda (s)
	     (do ((i 0 (1+ i)))
		 ((= i (chans s)))
	       (let ((need-update (selection-member? s i)))
		 (set! (selection-member? s i) #f)
		 (if need-update (update-time-graph s i)))))
	   (sounds)))
      (if chn
	  (add-chan-to-selection beg end snd chn)
	  (for-each
	   (lambda (s)
	     (if (or (eq? snd #t)
		     (= s current-sound)
		     (and (not (= current-sync 0))
			  (= current-sync (sync s))))
		 (do ((i 0 (1+ i)))
		     ((= i (chans s)))
		   (add-chan-to-selection beg end s i))))
	   (sounds))))))

;;; -------- delete selected portion and smooth the splice

(define delete-selection-and-smooth
  (lambda ()
    "(delete-selection-and-smooth) deletes the current selection and smooths the splice"
    (if (selection?)
	(let ((beg (selection-position))
	      (len (selection-length)))
	  (apply map (lambda (snd chn)
		       (if (selection-member? snd chn)
			   (let ((smooth-beg (max 0 (- beg 16))))
			     (delete-samples beg len snd chn)
			     (smooth-sound smooth-beg 32 snd chn))))
		 (all-chans))))))


;;; -------- eval over selection, replacing current samples, mapped to "C-x x" key using prompt-in-minibuffer
;;;
;;; when the user types C-x x (without modifiers) and there is a current selection,
;;;   the minibuffer prompts "selection eval:".  Eventually the user responds,
;;;   hopefully with a function of one argument, the current selection sample
;;;   the value returned by the function becomes the new selection value.

(bind-key (char->integer #\x) 0
	  (lambda ()
	    (if (selection?)
		(prompt-in-minibuffer "selection eval:" eval-over-selection)
		(report-in-minibuffer "no selection")))
	  #t)

(define eval-over-selection 
  (lambda (func)
    "(eval-over-selection func) evaluates func on each sample in the current selection"
    (if (and (procedure? func) 
	     (selection?))
	(let* ((beg (selection-position))
	       (len (selection-length)))
	  (apply map (lambda (snd chn)
		       (if (selection-member? snd chn)
			   (let ((new-data (make-vct len))
				 (old-data (samples->vct beg len snd chn)))
			     (do ((k 0 (1+ k))) ;here we're applying our function to each sample in the currently selected portion
				 ((= k len) (set-samples beg len new-data snd chn))
			       (vct-set! new-data k (func (vct-ref old-data k)))))))
		 (all-chans))))))

;;; the same idea can be used to apply a function between two marks (see eval-between-marks in marks.scm)



;;; --------- snd-debug
;;;
;;; this redirects the Guile debugger input/output to the Snd listener.
;;; If you hit an error, type (snd-debug) rather than (debug).

(use-modules (ice-9 debugger))

(define (snd-debug)
  (let* ((debugger-input "")
	 (debugger-char 0)
	 (debugger-strlen 0)
	 (stdout (current-output-port))
	 (stdin (current-input-port))
	 (snd-out (make-soft-port
		   (vector
		    (lambda (c) (snd-print c))
		    (lambda (s) (snd-print s))

		    (lambda () #f)
		    
		    (lambda ()
		      (do () ((or (and (>= debugger-char 0)
				       (< debugger-char debugger-strlen))
				  (c-g?))))
		      (let ((ch (string-ref debugger-input debugger-char)))
			(set! debugger-char (+ debugger-char 1))
			ch))
	      
		    (lambda () #f))
		   "rw")))

    (define (snd-debug-read str)
      (snd-print "\n")
      (set! debugger-input (string-append str "\n"))
      (set! debugger-char 0)
      (set! debugger-strlen (string-length debugger-input))
      #t)

    (dynamic-wind
     (lambda ()
       (snd-print "\n")
       (set-current-output-port snd-out)
       (set-current-input-port snd-out)
       (add-hook! read-hook snd-debug-read)
       (reset-listener-cursor)) ; going into snd-debug causes the cursor to change to the clock (wait) cursor which we don't want here
     (lambda ()
       (debug))
     (lambda ()
       (remove-hook! read-hook snd-debug-read)
       (set-current-output-port stdout)
       (set-current-input-port stdin)))))

;;; TODO: check step/trace -- trace output seems to be lost somewhere?
		 

;;; -------- read-listener-line

(define (read-listener-line prompt)
  (let ((res #f))
    (define (reader str) (set! res str) #t)
    (add-hook! read-hook reader)
    (reset-listener-cursor)
    (snd-print "\n")
    (snd-print prompt)
    (do () ((or (c-g?) res)))
    (remove-hook! read-hook reader)
    res))
