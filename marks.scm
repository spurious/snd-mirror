;;; examples of mark-related functions

;;; Contents:
;;;     mark-name->id is a global version of find-mark
;;;     move-syncd-marks moves all syncd marks together
;;;     describe-mark shows mark history
;;;     click marks between start-sync and stop-sync to sync them together
;;;     syncronize sounds at a given mark
;;;     fit selection between marks, expanding via granulate (this needs some tweaking...)
;;;     pad-marks inserts silence before each in a list of marks
;;;     play-syncd-marks
;;;     report-mark-names causes mark names to be posted in the minibuffer as a sound is played
;;;     eval-between-marks evaluates func between two marks


;;; -------- mark-name->id is a global version of find-mark

(define mark-name->id
  (lambda (name)
    "(mark-name->id name) is like find-mark but searches all currently accessible channels"
    (call-with-current-continuation
     (lambda (return-early)
       (do ((i 0 (1+ i)))
	   ((= i (max-sounds)) 'no-such-mark)
	 (if (sound? i)
	     (do ((j 0 (1+ j)))
		 ((= j (channels i)))
	       (let ((mark (find-mark name i j)))
		 (if (mark? mark) (return-early mark))))))))))


;;; -------- move-syncd-marks moves all syncd marks together

(define (move-syncd-marks sync diff)
  "(move-syncd-marks sync diff) moves all marks sharing sync by diff samples"
  (map (lambda (m)
         (set! (mark-sample m) (+ (mark-sample m) diff)))
       (syncd-marks sync)))


;;; -------- describe-mark shows mark history

(define (describe-mark id)
  "(describe-mark id) returns a description of the movements of mark id over the channel's edit history"
  (let ((mark-setting (mark-home id)))
    (if (eq? mark-setting 'no-such-mark)
        ;; not an active mark, so go scrounging for it
        ;;   we're looking at every edit of every channel
        (do ((i 0 (1+ i)))
            ((or (= i (max-sounds)) (list? mark-setting)))
          (if (sound? i)
              (do ((j 0 (1+ j)))
                  ((or (= j (channels i)) (list? mark-setting)))
                (let* ((max-edits (apply + (edits i j))))
                  (do ((k 0 (1+ k)))
                      ((or (> k max-edits) (list? mark-setting)))
                    (if (member id (marks i j k))
                        (set! mark-setting (list i j)))))))))
    (if (list? mark-setting)
        (let* ((snd (car mark-setting))
               (chn (cadr mark-setting))
               (max-edits (apply + (edits snd chn)))
               (descr '())
               (header (list 'mark id 'sound snd (short-file-name snd) 'channel chn)))
          (do ((i max-edits (1- i)))
              ((< i 0) descr)
            (if (member id (marks snd chn i))
                (set! descr (cons (mark-sample id i) descr))
                (set! descr (cons #f descr))))
          (cons header descr))
        (throw 'no-such-mark (list "describe-mark" id)))))


;;; -------- click marks between start-sync and stop-sync to sync them together
;;;  (easier than calling mark-sync over and over by hand)

(define mark-sync-number 0)
(define (start-sync) (set! mark-sync-number (+ (mark-sync-max) 1)))
(define (stop-sync) (set! mark-sync-number 0))
(define (click-to-sync id) (set! (mark-sync id) mark-sync-number) #f)
;(add-hook! mark-click-hook click-to-sync)


;;; -------- syncronize sounds at a given mark

(define syncup
  (lambda ids
    "(syncup ids) pads the channels with zeros so that all the marks in ids list occur at the same time"
    (let* ((samps (map mark-sample ids))
	   (max-samp (apply max samps)))
      (define (pad-to-sync lst-ids lst-samps)
	(if (not (null? lst-ids))
	    (begin
	      (if (< (car lst-samps) max-samp)
		  (let ((nsamps (- max-samp (car lst-samps)))
			(snd-chn (mark-home (car lst-ids))))
		    (insert-samples 0 nsamps (make-vct nsamps) (car snd-chn) (cadr snd-chn))))
	      (pad-to-sync (cdr lst-ids) (cdr lst-samps)))))
      (pad-to-sync ids samps))))


;;; -------- fit selection between marks, expanding via granulate (this needs some tweaking...)

(define fit-selection-between-marks
  (lambda (m1 m2)
    "(fit-selection-between-marks m1 m2) fits (and mixes) the current selection (via granulate) between the given marks"
    (let* ((m1-samp (mark-sample m1))
	   (m2-samp (mark-sample m2))
	   (m1-home (mark-home m1))
	   (m2-home (mark-home m2)))
      (if (not (equal? m1-home m2-home))
	  (snd-print (format #f "mark ~A is in ~A[~A] but mark ~A is in ~A[~A]?" 
			     m1 (car m1-home) (cadr m1-home)
			     m2 (car m2-home) (cadr m2-home)))
	  (let* ((mark-samps (- m2-samp m1-samp))
		 (selection-samps (selection-length))
		 (reg-data (region-samples->vct))
		 (reader (make-sample-reader m1-samp))
		 (new-data (make-vct mark-samps))
		 (gr (make-granulate :expansion (/ mark-samps selection-samps)))
		 (inctr 0))
	    (do ((i 0 (1+ i)))
		((= i mark-samps))
	      (vct-set! new-data i 
			(+ (next-sample reader)
			   (granulate gr
				      (lambda (dir)
					(if (>= inctr selection-samps)
					    0.0
					    (let ((val (vct-ref reg-data inctr)))
					      (set! inctr (+ inctr dir))
					      val)))))))
	    (free-sample-reader reader)
	    (vct->samples m1-samp mark-samps new-data (car m1-home) (cadr m1-home)))))))


;;; -------- pad-marks inserts silence before each in a list of marks

(define pad-marks 
  (lambda (ids secs)
    "(pad-marks ids secs) inserts secs seconds of silence before each mark in ids"
    (let* ((silence-length (inexact->exact (* secs (srate))))
	   (silence-samps (make-vct silence-length)))
      (as-one-edit
       (lambda ()
	 (map (lambda (n)
		(let ((samp (max 0 (- (mark-sample n) 1)))
		      (home (mark-home n)))
		  (insert-samples samp silence-length silence-samps (car home) (cadr home))))
	      ids))))))


;;; -------- play-syncd-marks

(define (play-syncd-marks sync)
  "(play-syncd-marks sync) starts playing from all marks sharing sync"
  (let ((chans 1)
	(rate 22050))
    (map (lambda (m)
	   (let* ((sound (car (mark-home m)))
		  (channel (cadr (mark-home m)))
		  (new-player (make-player sound channel)))
	     (add-player new-player (mark-sample m))
	     (set! chans (max chans (1+ channel)))
	     (set! rate (max rate (srate sound)))))
	 (syncd-marks sync))
    (start-playing chans rate)))


;;; -------- report-mark-names causes mark names to be posted in the minibuffer as a sound is played

(define (report-mark-names)
  (add-hook! start-playing-hook 
	     (lambda (snd)
	       (let* ((marklist (marks snd 0))
		      (samplist (map mark-sample marklist))
		      (samp 0))
		 (add-hook! stop-playing-hook
			    (lambda (snd)
			      (report-in-minibuffer "" snd)
			      (reset-hook! play-hook)
			      (reset-hook! stop-playing-hook)))
		 (add-hook! play-hook 
			    (lambda (size)
			      (set! samp (+ samp size))
			      (if (and (not (null? samplist))
				       (>= samp (car samplist)))
				  (begin
				    (report-in-minibuffer (mark-name (car marklist)) snd)
				    (set! marklist (cdr marklist))
				    (set! samplist (cdr samplist))))))))))



;;; -------- eval-between-marks

(define eval-between-marks
  (lambda (func)
    "(eval-between-marks func) evaluates func between the leftmost marks"
    (define (find-if pred l) ; this is from guile/ice-9/common-list.scm but returns l not car l
      (cond ((null? l) #f)
	    ((pred (car l)) l)
	    (else (find-if pred (cdr l)))))
    (if (procedure? func)
	;; find leftmost two marks in selected chn
	(let ((chan (selected-channel))
	      (snd (selected-sound)))
	  (if (< chan 0) (set! chan 0)) ;perhaps not current sound, so no selected channel in it
	  (let ((mlist (marks snd chan)))
	    (if (< (length mlist) 2)
		(report-in-minibuffer "need 2 marks")
		(let* ((left-samp (left-sample snd chan))
		       (winl (find-if (lambda (n) (> (mark-sample n) left-samp)) mlist)))
		  (if (and winl (> (length winl) 1))
		      (let* ((beg (mark-sample (car winl)))
			     (len (- (mark-sample (cadr winl)) beg))
			     (new-data (make-vct len))
			     (old-data (samples->vct beg len snd chan)))
			(do ((k 0 (1+ k)))
			    ((= k len) (set-samples beg len new-data snd chan))
			  (vct-set! new-data k (func (vct-ref old-data k)))))))))))))

;(bind-key (char->integer #\m) 0 (lambda () (prompt-in-minibuffer "mark eval:" eval-between-marks)))

