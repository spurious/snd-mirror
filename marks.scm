;;; examples of mark-related functions

(use-modules (ice-9 format))

;;; Contents:
;;;     mark-name->id is a global version of find-mark
;;;     move-syncd-marks moves all syncd marks together
;;;     describe-mark shows mark history
;;;     click marks between start-sync and stop-sync to sync them together
;;;     synchronize sounds at a given mark
;;;     fit selection between marks, expanding via granulate (this needs some tweaking...)
;;;     pad-marks inserts silence before each in a list of marks
;;;     play-syncd-marks and play-between-marks
;;;     report-mark-names causes mark names to be posted in the minibuffer as a sound is played
;;;     eval-between-marks evaluates func between two marks
;;;     snap-marks places marks at current selection boundaries
;;;     define-selection-via-marks selects the portion between two marks
;;;     snap-mark-to-beat forces dragged mark to end up on a beat
;;;     mark-explode splits a sound into a bunch of sounds based on mark placements


;;; -------- mark-name->id is a global version of find-mark

(define (mark-name->id name)
  "(mark-name->id name) is like find-mark but searches all currently accessible channels"
  (call-with-current-continuation
   (lambda (return)
     (map 
      (lambda (snd)
	(do ((chn 0 (1+ chn)))
	    ((= chn (channels snd)))
	     (let ((mark (find-mark name snd chn)))
	       (if (mark? mark) 
		   (return mark)))))
      (sounds))
     'no-such-mark)))


;;; -------- move-syncd-marks moves all syncd marks together

(define (move-syncd-marks sync diff)
  "(move-syncd-marks sync diff) moves all marks sharing sync by diff samples"
  (map (lambda (m)
         (set! (mark-sample m) (+ (mark-sample m) diff)))
       (syncd-marks sync)))


;;; -------- describe-mark shows mark history

(define (describe-mark id)
  "(describe-mark id) returns a description of the movements of mark id over the channel's edit history"
  (let ((mark-setting (catch 'no-such-mark (lambda () (mark-home id)) (lambda args #f))))
    (if (not mark-setting)
        ;; not an active mark, so go scrounging for it
        ;;   we're looking at every edit of every channel
	(set! mark-setting (call-with-current-continuation
			    (lambda (return)
			      (for-each
			       (lambda (snd)
				 (do ((chn 0 (1+ chn)))
				     ((= chn (channels snd)))
				   (let* ((max-edits (apply + (edits snd chn))))
				     (do ((ed 0 (1+ ed)))
					 ((> ed max-edits))
				       (if (member id (marks snd chn ed))
					   (return (list snd chn)))))))
			       (sounds))
			      #f))))
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

(define (fit-selection-between-marks m1 m2)
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
	  (vct-map! new-data
		    (lambda ()
		      (+ (next-sample reader)
			 (granulate gr
				    (lambda (dir)
				      (if (>= inctr selection-samps)
					  0.0
					  (let ((val (vct-ref reg-data inctr)))
					    (set! inctr (+ inctr dir))
					    val)))))))
	  (free-sample-reader reader)
	  (vct->samples m1-samp mark-samps new-data (car m1-home) (cadr m1-home))))))


;;; -------- pad-marks inserts silence before each in a list of marks

(define (pad-marks ids secs)
  "(pad-marks ids secs) inserts secs seconds of silence before each mark in ids"
  (let* ((silence-length (inexact->exact (* secs (srate))))
	 (silence-samps (make-vct silence-length)))
    (as-one-edit
     (lambda ()
       (map (lambda (n)
	      (let ((samp (max 0 (- (mark-sample n) 1)))
		    (home (mark-home n)))
		(insert-samples samp silence-length silence-samps (car home) (cadr home))))
	    ids)))))


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

(define play-between-marks
  (lambda args
    "(play-between-marks ...) plays the portion between the marks (searching for plausible default marks)"
    (let* ((snd (selected-sound))
	   (chn (selected-channel))
	   (m1 (if (> (length args) 0)
		   (car args)
		   (let find-mark ((ms (marks snd chn)))
		     (if (null? ms)
			 (begin
			   (snd-print ";no marks in current window?")
			   #f)
			 (if (>= (mark-sample (car ms)) (left-sample snd chn))
			     (car ms)
			     (find-mark (cdr ms)))))))
	   (m2 (and (mark? m1)
		    (if (> (length args) 1)
			(cadr args)
			(let find-another-mark ((ms (marks snd chn)))
			  (if (null? ms)
			      (begin
				(snd-print ";no second mark?")
				#f)
			      (if (> (mark-sample (car ms)) (mark-sample m1))
				  (car ms)
				  (find-another-mark (cdr ms)))))))))
      (if (and (mark? m1)
	       (mark? m2))
	  (play (mark-sample m1) 
		(car (mark-home m1)) 
		(cadr (mark-home m1)) 
		#f 
		(mark-sample m2))))))


;;; -------- report-mark-names causes mark names to be posted in the minibuffer as a sound is played

(define (report-mark-names)
  "(report-mark-names) causes mark names to be printed as they are passed while playing"
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

(define (eval-between-marks func)
  "(eval-between-marks func) evaluates func between the leftmost marks; func takes one arg, the original sample"
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
			  ((= k len) (vct->channel new-data beg len snd chan))
			(vct-set! new-data k (func (vct-ref old-data k))))))))))))

;(bind-key (char->integer #\m) 0 (lambda () (prompt-in-minibuffer "mark eval:" eval-between-marks)))


;;; -------- snap-marks

(define (snap-marks)
  "snap-marks places marks at current selection boundaries"
  (if (selection?)
      (for-each 
       (lambda (select)
	 (let ((pos  (apply selection-position select))
	       (len  (apply selection-length select)))
	   (apply add-mark pos select)
	   (apply add-mark (+ pos len) select)))
       (selection-members))))


;;; -------- define-selection-via-marks

(define (define-selection-via-marks m1 m2)
  "(define-selection-via-marks m1 m2) defines the current selection to lie between the marks given"
  (let ((m1sc (mark-home m1))
	(m2sc (mark-home m2)))
    (if (not (equal? m1sc m2sc))
	(snd-error "define-selection-via-marks assumes the marks are in the same channel")
	(let ((beg (min (mark-sample m1) (mark-sample m2)))
	      (end (max (mark-sample m1) (mark-sample m2)))
	      (snd (car m1sc))
	      (chn (cadr m1sc)))
	  (if (selection?)
	      (set! (selection-member? #t) #f)) ; clear entire current selection, if any
	  (set! (selection-member? snd chn) #t)
	  (set! (selection-position snd chn) beg)
	  (set! (selection-length snd chn) (1+ (- end beg)))))))


;;; -------- snap-mark-to-beat

(define (snap-mark-to-beat)
  "(snap-mark-to-beat) ensures that when a mark is dragged, its released position is always on a beat"
  (let ((mark-release 4))
    (add-hook! mark-hook 
	       (lambda (mrk snd chn reason)
		 (if (= reason mark-release)
		     (let* ((samp (mark-sample mrk))
			    (bps (/ (beats-per-minute snd chn) 60.0))
			    (sr (srate snd))
			    (beat (floor (/ (* samp bps) sr)))
			    (lower (inexact->exact (/ (* beat sr) bps)))
			    (higher (inexact->exact (/ (* (1+ beat) sr) bps))))
		       (set! (mark-sample mrk)
			     (if (< (- samp lower) (- higher samp))
				 lower
				 higher))))))))

;;; -------- mark-explode
;;;
;;; write out each section of a file between marks as a separate file

(define (mark-explode)
  (let ((start 0)
	(file-ctr 0))
    (for-each
     (lambda (mark)
       (let ((len (- (mark-sample mark) start)))
	 (array->file (format #f "mark-~D.snd" file-ctr)
		      (channel->vct start len)
		      len (srate) 1)
	 (set! file-ctr (1+ file-ctr))
	 (set! start (mark-sample mark))))
     (caar (marks)))))
