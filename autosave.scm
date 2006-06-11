;;; -------- auto-save 

(use-modules (ice-9 format))

(provide 'snd-autosave.scm)

(if (not (provided? 'snd-extensions.scm)) (load-from-path "extensions.scm"))

(define auto-save-interval 60.0) ;seconds between auto-save checks
(define auto-saving #f)

(define (cancel-auto-save)
  "(cancel-auto-save) turns off the auto-save mechanism"
  (set! auto-saving #f))

(define+ (auto-save)
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
      (clear-unsaved-edits snd)))
  
  (define (auto-save-func)
    (if auto-saving
	(begin
	  (for-each (lambda (snd)
		      (if (> (unsaved-edits snd) 0)
			  (let ((save-name (auto-save-temp-name snd)))
			    (report-in-minibuffer (string-append "auto-saving as " save-name "...") snd)
			    (in (* 1000 3) (lambda () (report-in-minibuffer "" snd)))
			    (save-sound-as save-name snd)
			    (clear-unsaved-edits snd))))
		    (sounds))
	  (in (* 1000 auto-save-interval) auto-save-func))))
  
  (if (not (member auto-save-done (hook->list close-hook)))
      (begin
	(if (not (null? (sounds)))
	    (for-each auto-save-open-func (sounds)))
	(add-hook! after-open-hook auto-save-open-func)
	(add-hook! close-hook auto-save-done)
	(add-hook! save-hook (lambda (snd name) (auto-save-done snd)))
	(add-hook! exit-hook (lambda () (for-each auto-save-done (sounds))))))
  (set! auto-saving #t)
  (in (* 1000 auto-save-interval) auto-save-func))

(auto-save)
