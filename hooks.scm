;;; hook-related functions

;;; -------- snd-hooks

(define (snd-hooks)
  (list after-graph-hook lisp-graph-hook before-transform-hook mix-position-changed-hook stop-playing-channel-hook save-hook mus-error-hook
	mouse-enter-graph-hook mouse-leave-graph-hook open-raw-sound-hook select-channel-hook after-open-hook close-hook drop-hook
	just-sounds-hook mark-click-hook mark-drag-hook mix-amp-changed-hook mix-speed-changed-hook name-click-hook open-hook
	output-comment-hook multichannel-mix-hook play-hook snd-error-hook snd-warning-hook start-hook start-playing-hook stop-playing-hook
	stop-playing-region-hook mouse-enter-listener-hook mouse-leave-listener-hook property-changed-hook select-sound-hook select-mix-hook
	print-hook exit-hook output-name-hook during-open-hook transform-hook mouse-enter-label-hook mouse-leave-label-hook initial-graph-hook
	graph-hook key-press-hook mouse-drag-hook mouse-press-hook mouse-release-hook enved-hook read-hook))

(define (reset-all-hooks)
  (for-each 
   (lambda (n)
     (reset-hook! n))
   (snd-hooks)))


;;; -------- describe-hook

(define (describe-hook hook)
  (for-each 
    (lambda (n) 
      (snd-print n))
    (reverse (hook->list hook))))


;;; -------- local hook

(define (with-local-hook hook local-hook-procs thunk)
  "evaluate thunk with hook set to local-hook-procs (a list), then restore hook to previous state"
  (define (list->hook hook lst)
    (define (list->hook-1 hook l)
      (if (not (null? l))
	  (begin
	    (add-hook! hook (car l))
	    (list->hook-1 hook (cdr l)))))
    (reset-hook! hook)
    (list->hook-1 hook lst)
    hook)

  (let ((old-hook-procs (hook->list hook)))
    (list->hook hook local-hook-procs)
    (let ((result (thunk)))
      (list->hook hook old-hook-procs)
      result)))

