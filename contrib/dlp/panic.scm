(use-modules (ice-9 format))

(define panic-menu (add-to-main-menu "Panic Control"))

(add-to-menu panic-menu "Stop play!" stop-playing)
(add-to-menu panic-menu "Stop all!" c-g!)


