(provide 'snd-panic.scm)

(define panic-menu (add-to-main-menu "Panic Control"))

(add-to-menu panic-menu "Stop play!" (lambda () (stop-playing)))


