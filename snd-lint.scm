;;; Snd extensions for lint

(require lint.scm)

;;; ---------------- deprecated funcs ---------------- 
(let ((deprecated-ops '((data-format . sample-type)
			(mus-sound-frames . mus-sound-framples)
			(mus-sound-data-format . mus-sound-sample-type)
			(mus-data-format-name . mus-sample-type-name)
			(mus-data-format->string . mus-sample-type->string))))

  (define (snd-lint-deprecate caller head form env)
    ((*lint* 'lint-format) "~A is deprecated; use ~A" caller head (cond ((assq head deprecated-ops) => cdr))))

  (for-each (lambda (op)
	      (hash-table-set! (*lint* 'special-case-functions) (car op) snd-lint-deprecate))
	    deprecated-ops))

;;; ---------------- snd-display ----------------
(hash-table-set! (*lint* 'special-case-functions) 'snd-display
		 (hash-table-ref (*lint* 'special-case-functions) 'format))

