;;; -------- auto-save 
;;;
;;; this needs to be loaded before any sounds
;;; TODO: scan sounds upon startup

(define auto-save-interval 60.0) ;seconds between auto-save checks

(define auto-saving #f)

(define cancel-auto-save
  (lambda ()
    (set! auto-saving #f)))

(define auto-save-histories '())

(define unsaved-edits
  (lambda (snd)
    (let ((data (assoc snd auto-save-histories)))
      (if data 
	  (cdr data) 
	  0))))

(define clear-unsaved-edits
  (lambda (snd)
    (let ((old-data (assoc snd auto-save-histories)))
      (if old-data
	  (set-cdr! old-data 0)
	  (set! auto-save-histories (cons (cons snd 0) auto-save-histories))))))

(define increment-unsaved-edits
  (lambda (snd)
    (let ((old-data (assoc snd auto-save-histories)))
      (if old-data
	  (set-cdr! old-data (+ (cdr old-data) 1))
	  (set! auto-save-histories (cons (cons snd 1) auto-save-histories))))))

(define upon-edit
  (lambda (snd)
    (lambda ()
      (increment-unsaved-edits snd))))

(define auto-save-open-func
  (lambda (snd)
    (let ((temp-file (string-append "/tmp/#" (short-file-name snd) "#")))
      (if (and (file-exists? temp-file)
	       (< (file-write-date (file-name snd)) (file-write-date temp-file)))
	  (snd-warning (format #f "auto-saved version of ~S (~S) is newer"
			       (short-file-name snd)
			       temp-file)))
      (do ((i 0 (1+ i)))
	  ((= i (channels snd)))
	(if (hook-empty? (edit-hook snd i))
	    (add-hook! (edit-hook snd i) (upon-edit snd))))
      (clear-unsaved-edits snd))))

(add-hook! after-open-hook auto-save-open-func)

(define auto-save-done
  (lambda (snd)
    (let ((temp-file (string-append "/tmp/#" (short-file-name snd) "#")))
      (if (file-exists? temp-file)
	  (delete-file temp-file))
      (clear-unsaved-edits snd)
      #f)))

(add-hook! close-hook auto-save-done)
(add-hook! save-hook (lambda (snd name) (auto-save-done snd)))
(add-hook! exit-hook (lambda () (map auto-save-done (sounds))))

(define auto-save-func
  (lambda ()
    (if auto-saving
	(begin
	  (map (lambda (snd)
		 (if (> (unsaved-edits snd) 0)
		     (begin
		       (report-in-minibuffer "auto-saving..." snd)
		       (in (* 1000 3) (lambda () (report-in-minibuffer "" snd)))
		       (save-sound-as (string-append "/tmp/#" (short-file-name snd) "#") snd)
		       (clear-unsaved-edits snd))))
	     (sounds))
	  (in (* 1000 auto-save-interval) auto-save-func)))))

(define auto-save
  (lambda ()
    "(auto-save) starts watching files, automatically saving backup copies as edits accumulate"
    (set! auto-saving #t)
    (in (* 1000 auto-save-interval) auto-save-func)))

(auto-save)
