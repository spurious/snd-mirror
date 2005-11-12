(use-modules (ice-9 format))
(provide 'snd-panic.scm)

(define panic-menu (add-to-main-menu "Panic Control"))

(add-to-menu panic-menu "Stop play!" (lambda () (stop-playing)))
(add-to-menu panic-menu "Stop all! C-g" c-g!)


