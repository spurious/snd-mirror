;;; various generally useful Snd extensions

;;; accessors for graph-style fields
;;; delete selected portion and smooth the splice
;;; eval over selection
;;; mix with result at original peak amp
;;; mix with envelope
;;; map-sound-files, for-each-sound-file, match-sound-files, directory->list
;;; selection-members
;;; make-selection
;;; snd-debug
;;; read-listener-line
;;; snd-trace
;;; check-for-unsaved-edits
;;; remember-sound-state

(use-modules (ice-9 common-list) (ice-9 optargs) (ice-9 format))


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
    (let ((original-maxamp (maxamp snd chn)))
      (mix filename beg in-chan snd chn)
      (let ((new-maxamp (maxamp snd chn)))
	(if (not (= original-maxamp new-maxamp))
	    (let ((scaler (/ original-maxamp new-maxamp))
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
(let ((reg (make-regexp "\\.(wav|snd)$")))
  (match-sound-files (lambda (file) (regexp-exec reg file))))
!#
;;; this argument to make-regexp is looking for *.wav and *.snd
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

;;; in fact, we could use regexp's in place of Snd's sound-files-in-directory,
;;; using two more of Dirk Herrman's procedures:

(define (grep rx strings)
  (let ((r (make-regexp rx)))
    (filter-list (lambda (x) (regexp-exec r x)) strings)))

(define (directory->list dir)
  (let ((dport (opendir dir)))
    (let loop ((entry (readdir dport))
	       (files '()))
      (if (not (eof-object? entry))
	  (loop (readdir dport) (cons entry files))
	  (begin
	    (closedir dport)
	    (reverse! files))))))

;(sort (grep "^[^.]" (directory->list ".")) string<?)


(define sound-file-extensions (list "snd" "aiff" "aif" "wav" "au" "aifc" "voc" "wve"))

(define (add-sound-file-extension-1 ext) 
  (set! sound-file-extensions (cons ext sound-file-extensions)))

(define* (sound-files-in-directory-1 #:optional (dir "."))
  (sort (grep
	 (format #f "\\.(~{~A~^|~})$" sound-file-extensions)
	 (directory->list dir))
	string<?))


    
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
      (if (number? chn)
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


;;; -------- with-trace 
;;; 
;;; this activates tracing and redirects its output to the Snd listener

(defmacro snd-trace (body)
  `(let* ((stderr (current-error-port))
	  (snd-err (make-soft-port
		    (vector
		     (lambda (c) (snd-print c))
		     (lambda (s) (snd-print s))
		     (lambda () #f)
		     #f
		     (lambda () #f))
		    "w")))
     (dynamic-wind
      (lambda ()
	(snd-print "\n")
	(set-current-error-port snd-err))
      (lambda ()
	(with-traps 
	 (lambda ()
	   (start-stack 'repl-stack ,body))))
      (lambda ()
	(set-current-error-port stderr)))))

       

;;; -------- check-for-unsaved-edits
;;;
;;; (check-for-unsaved-edits on): if 'on', add a function to the close-hook and exit-hook
;;;    that asks the user for confirmation before closing a sound if there are unsaved
;;;    edits on that sound.  if 'on' is #f, remove those hooks.


(define (unsaved-edits-at-close? ind)
  "(unsaved-edits? ind) -> #t if there are any unsaved edits in sound ind"
  (letrec ((unsaved-edits-in-chan? 
	    (lambda (chan)
	      (if (>= chan (channels ind))
		  #f
		  (let ((eds (edits ind chan)))
		    (if (> (car eds) 0)
			(not (yes-or-no? ;that is, "yes" => exit
			      (format #f "~A has ~D unsaved edit~P in channel ~D, exit anyway? " 
				      (short-file-name ind) 
				      (car eds)
				      (car eds)
				      chan)))
			(unsaved-edits-in-chan? (1+ chan))))))))
    (unsaved-edits-in-chan? 0)))

(define (unsaved-edits-at-exit?)
  (letrec ((unsaved-edits-at-exit-1?
	    (lambda (snds)
	      (and snds
		   (or (unsaved-edits-at-close? (car snds))
		       (unsaved-edits-at-exit-1? (cdr snds)))))))
    (unsaved-edits-at-exit-1? (sounds))))

(define check-for-unsaved-edits
  (lambda arg
    (if (or (null? arg)
	    (car arg))
	(begin
	  (if (not (member unsaved-edits-at-exit? (hook->list exit-hook)))
	      (add-hook! exit-hook unsaved-edits-at-exit?))
	  (if (not (member unsaved-edits-at-close? (hook->list close-hook)))
	      (add-hook! close-hook unsaved-edits-at-close?)))
	(begin
	  (remove-hook! exit-hook unsaved-edits-at-exit?)
	  (remove-hook! close-hook unsaved-edits-at-close?)))))


;;; -------- remember-sound-state

(define (remember-sound-state)
  ;; remember the state of a sound when it is closed, and if it is subsquently re-opened, restore that state
  (let ((states '())
	(sound-funcs (list sync cursor-follows-play))
	(channel-funcs (list graph-time? graph-transform? graph-lisp? x-bounds y-bounds cursor cursor-size
			     cursor-style show-marks show-y-zero wavo-hop wavo-trace max-transform-peaks
			     show-transform-peaks fft-log-frequency fft-log-magnitude verbose-cursor zero-pad
			     wavelet-type min-dB transform-size transform-graph-type time-graph-type fft-window
			     transform-type transform-normalization graph-style show-mix-waveforms dot-size
			     x-axis-style show-axes graphs-horizontal)))
    (define saved-state
      (make-procedure-with-setter
       (lambda (snd)
	 (find-if (lambda (n) 
		    (string=? (car n) (file-name snd))) 
		  states))
       (lambda (snd new-state)
	 (set! states (cons new-state
			    (remove-if
			     (lambda (n)
			       (string=? (car n) (file-name snd)))
			     states))))))

    (add-hook! close-hook (lambda (snd)
			    ;; save current state
			    (set! (saved-state snd)
				  (list (file-name snd)
					(file-write-date (file-name snd))
					(map (lambda (f) 
					       (f snd)) 
					     sound-funcs)
					(map (lambda (sc)
					       (map (lambda (f)
						      (f (car sc) (cadr sc)))
						    channel-funcs))
					     (let ((scs '()))
					       (do ((i 0 (1+ i)))
						   ((= i (chans snd)))
						 (set! scs (cons (list snd i) scs)))
					       (reverse scs)))))
			    #f))
    (add-hook! after-open-hook (lambda (snd)
				 ;; restore previous state, if any
				 (let ((state (saved-state snd)))
				   (if state
				       (if (= (file-write-date (file-name snd))
					      (cadr state))
					   ;; otherwise all bets are off (anything could have changed)
					   (begin
					     (for-each (lambda (f val)
							 (set! (f snd) val))
						       sound-funcs
						       (caddr state))
					     (do ((chn 0 (1+ chn)))
						 ((= chn (chans snd)))
					       (set! (squelch-update snd chn) #t)
					       (for-each (lambda (f val)
							   (set! (f snd chn) val))
							 channel-funcs
							 (list-ref (cadddr state) chn))
					       (set! (squelch-update snd chn) #f))))))))
    'remembering!))
