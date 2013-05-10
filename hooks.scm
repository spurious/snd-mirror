;;; hook-related functions

(provide 'snd-hooks.scm)

;;; -------- snd-hooks

(define (snd-hooks)
  "(snd-hooks) -> list of all global (not channel-specific) hooks"
  (list after-graph-hook after-lisp-graph-hook lisp-graph-hook before-transform-hook mix-release-hook save-hook mus-error-hook
	mouse-enter-graph-hook mouse-leave-graph-hook open-raw-sound-hook select-channel-hook after-open-hook close-hook drop-hook update-hook
	mark-click-hook mark-drag-hook name-click-hook open-hook help-hook before-save-state-hook
	output-comment-hook play-hook snd-error-hook snd-warning-hook start-hook start-playing-hook stop-playing-hook
	mouse-enter-listener-hook mouse-leave-listener-hook select-sound-hook
	exit-hook output-name-hook during-open-hook after-transform-hook mouse-enter-label-hook mouse-leave-label-hook initial-graph-hook
	graph-hook key-press-hook mouse-drag-hook mouse-press-hook enved-hook mouse-click-hook new-widget-hook
	mark-hook view-files-select-hook dac-hook stop-dac-hook stop-playing-selection-hook after-apply-controls-hook draw-mark-hook
	bad-header-hook save-state-hook new-sound-hook color-hook orientation-hook listener-click-hook mix-click-hook after-save-state-hook
	mouse-enter-text-hook mouse-leave-text-hook mix-drag-hook 
	start-playing-selection-hook after-save-as-hook before-save-as-hook peak-env-hook draw-mix-hook
	before-exit-hook before-close-hook clip-hook info-popup-hook))

(define (reset-all-hooks)
  "(reset-all-hooks) removes all Snd hook functions"
  (for-each 
   (lambda (n)
     (set! (hook-functions n) ()))
   (snd-hooks))
  (for-each 
   (lambda (snd)
     (do ((chn 0 (+ chn 1)))
	 ((= chn (channels snd)))
       (set! (hook-functions (edit-hook snd chn)) ())
       (set! (hook-functions (after-edit-hook snd chn)) ())
       (set! (hook-functions (undo-hook snd chn)) ())))
   (sounds)))


(define (add-to-all-hook-inits func)
  (for-each 
   (lambda (n)
     (set! ((procedure-environment n) 'init) (list func ((procedure-environment n) 'init))))
   (snd-hooks)))

(define (add-to-all-hook-ends func)
  (for-each 
   (lambda (n)
     (set! ((procedure-environment n) 'end) (list func ((procedure-environment n) 'end))))
   (snd-hooks)))



;;; -------- describe-hook

(define (describe-hook hook)
  "(describe-hook hook) -> description of functions on 'hook'"
  (for-each 
    (lambda (n) 
      (snd-print (format #f "~%~A" n)))
    (reverse (hook-functions hook))))


;;; -------- local hook

(define (with-local-hook hook local-hook-procs thunk)
  "(with-local-hook hook local-hook-procs thunk) evaluates thunk with hook set to local-hook-procs (a list), then restores hook to its previous state"
  (let ((old-hook-procs (hook-functions hook)))
    (set! (hook-functions hook) local-hook-procs)
    (let ((result (thunk)))
      (set! (hook-functions hook) old-hook-procs)
      result)))


;;; -------- hook-member --------

(define (hook-member value hook) 
  "(hook-member value hook) returns non-#f if 'value' is a member of the hook's function list"
  (member value (hook-functions hook)))
