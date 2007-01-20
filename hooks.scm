;;; hook-related functions

(use-modules (ice-9 format))
(provide 'snd-hooks.scm)

;;; -------- snd-hooks

(define (snd-hooks)
  "(snd-hooks) -> list of all global (not channel-specific) hooks"
  (list after-graph-hook after-lisp-graph-hook lisp-graph-hook before-transform-hook mix-release-hook save-hook mus-error-hook
	mouse-enter-graph-hook mouse-leave-graph-hook open-raw-sound-hook select-channel-hook after-open-hook close-hook drop-hook update-hook
	mark-click-hook mark-drag-hook name-click-hook open-hook help-hook before-save-state-hook
	output-comment-hook play-hook snd-error-hook snd-warning-hook start-hook start-playing-hook stop-playing-hook
	mouse-enter-listener-hook mouse-leave-listener-hook window-property-changed-hook select-sound-hook
	print-hook exit-hook output-name-hook during-open-hook after-transform-hook mouse-enter-label-hook mouse-leave-label-hook initial-graph-hook
	graph-hook key-press-hook mouse-drag-hook mouse-press-hook enved-hook read-hook mouse-click-hook new-widget-hook
	mark-hook view-files-select-hook dac-hook stop-dac-hook stop-playing-selection-hook after-apply-controls-hook draw-mark-hook
	bad-header-hook save-state-hook new-sound-hook color-hook orientation-hook listener-click-hook mix-click-hook after-save-state-hook
	mouse-enter-text-hook mouse-leave-text-hook optimization-hook mix-drag-hook mark-drag-triangle-hook
	start-playing-selection-hook recorder-file-hook after-save-as-hook before-save-as-hook
	before-exit-hook before-close-hook clip-hook))

(define (reset-all-hooks)
  "(reset-all-hooks) removes all Snd hook functions"
  (for-each 
   (lambda (n)
     (reset-hook! n))
   (snd-hooks))
  (for-each 
   (lambda (snd)
     (do ((chn 0 (1+ chn)))
	 ((= chn (channels snd)))
       (reset-hook! (edit-hook snd chn))
       (reset-hook! (after-edit-hook snd chn))
       (reset-hook! (undo-hook snd chn))))
   (sounds)))


;;; -------- remove-local-hook!
;;;
;;; Guile's remove-hook! does not work with locally defined functions, and
;;;   the functions themselves are not eq/eqv/equal to themselves!  So,
;;;   we kludge around it using the procedure name as a string (but anonymous
;;;   lambdas have #f for a name).

(define+ (remove-local-hook! hook func)
  "(remove-local-hook! hook func) removes func from hook even if func is defined locally"
  (define (procedure-name-or-false f)
    ;; anonymous lambda procedure name is #f
    (let ((name (procedure-name f)))
      (if name
	  (symbol->string name)
	  #f)))
  (let ((name (procedure-name-or-false func)))
    (if name
	(let ((lst (hook->list hook)))
	  (reset-hook! hook)
	  (for-each (lambda (orig-func)
		      (let ((orig-name (procedure-name-or-false orig-func)))
			(if (not (and (string? orig-name)
				      (string=? orig-name name)))
			    (add-hook! hook orig-func))))
		    lst))))
  hook)


;;; -------- describe-hook

(define (describe-hook hook)
  "(describe-hook hook) -> description of functions on 'hook'"
  (for-each 
    (lambda (n) 
      (snd-print (format #f "~%~A" n)))
    (reverse (hook->list hook))))


;;; -------- local hook

(define+ (list->hook hook lst)
  "(list->hook hook lst) resets 'hook', then adds each function in 'lst' to it"
  (define (list->hook-1 hook l)
    (if (not (null? l))
	(begin
	  (add-hook! hook (car l))
	  (list->hook-1 hook (cdr l)))))
  (reset-hook! hook)
  (list->hook-1 hook lst)
  hook)

(define+ (with-local-hook hook local-hook-procs thunk)
  "(with-local-hook hook local-hook-procs thunk) evaluates thunk with hook set to local-hook-procs (a list), then restores hook to its previous state"
  (let ((old-hook-procs (hook->list hook)))
    (list->hook hook local-hook-procs)
    (let ((result (thunk)))
      (list->hook hook old-hook-procs)
      result)))


;;; -------- hook-member --------

(define (hook-member value hook) 
  "(hook-member value hook) returns non-#f if 'value' is a member of the hook's function list"
  (member value (hook->list hook)))
