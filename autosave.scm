;;; -------- auto-save 

(use-modules (ice-9 format))

(if (not (defined? 'sound-property)) ; extensions.scm
    (define sound-property
      (make-procedure-with-setter
       
       (lambda (key snd)
	 "(sound-property key snd) returns the value associated with 'key' in the given sound's property list, or #f"
	 (let ((data (assoc key (sound-properties snd))))
	   (if data
	       (cdr data)
	       #f)))

       (lambda (key snd new-val)
	 (let ((old-val (assoc key (sound-properties snd))))
	   (if old-val
	       (set-cdr! old-val new-val)
	       (set! (sound-properties snd) (cons (cons key new-val) (sound-properties snd))))
	   new-val)))))


(define auto-save-interval 60.0) ;seconds between auto-save checks
(define auto-saving #f)

(define (cancel-auto-save)
  "(cancel-auto-save) turns off the auto-save mechanism"
  (set! auto-saving #f))

(define (auto-save)
  "(auto-save) starts watching files, automatically saving backup copies as edits accumulate"

  (define (auto-save-temp-name snd)
    (string-append (if (and (string? (temp-dir))
			    (> (string-length (temp-dir)) 0))
		       (string-append (temp-dir) "/")
		       "")
		   "#" (short-file-name snd) "#"))
  
  (define (unsaved-edits snd)
    (or (sound-property 'auto-save snd)
	0))

  (define (clear-unsaved-edits snd)
    (set! (sound-property 'auto-save snd) 0))
  
  (define (increment-unsaved-edits snd)
    (set! (sound-property 'auto-save snd) (+ 1 (sound-property 'auto-save snd))))
  
  (define (upon-edit snd)
    (lambda ()
      (increment-unsaved-edits snd)))
  
  (define (auto-save-open-func snd)
    (let ((temp-file (auto-save-temp-name snd)))
      (if (and (file-exists? temp-file)
	       (< (file-write-date (file-name snd)) (file-write-date temp-file)))
	  (snd-warning (format #f "auto-saved version of ~S (~S) is newer"
			       (short-file-name snd)
			       temp-file)))
      (do ((i 0 (1+ i)))
	  ((= i (channels snd)))
	(if (hook-empty? (edit-hook snd i))
	    (add-hook! (edit-hook snd i) (upon-edit snd))))
      (clear-unsaved-edits snd)))
  
  (define (auto-save-done snd)
    (let ((temp-file (auto-save-temp-name snd)))
      (if (file-exists? temp-file)
	  (delete-file temp-file))
      (clear-unsaved-edits snd)
      #f))
  
  (define (auto-save-func)
    (if auto-saving
	(begin
	  (map (lambda (snd)
		 (if (> (unsaved-edits snd) 0)
		     (begin
		       (report-in-minibuffer "auto-saving..." snd)
		       (in (* 1000 3) (lambda () (report-in-minibuffer "" snd)))
		       (save-sound-as (auto-save-temp-name snd) snd)
		       (clear-unsaved-edits snd))))
	       (sounds))
	  (in (* 1000 auto-save-interval) auto-save-func))))
  
  (if (not (member auto-save-done (hook->list close-hook)))
      (begin
	(if (not (null? (sounds)))
	    (map auto-save-open-func (sounds)))
	(add-hook! after-open-hook auto-save-open-func)
	(add-hook! close-hook auto-save-done)
	(add-hook! save-hook (lambda (snd name) (auto-save-done snd)))
	(add-hook! exit-hook (lambda () (map auto-save-done (sounds))))))
  (set! auto-saving #t)
  (in (* 1000 auto-save-interval) auto-save-func))

(auto-save)
