(use-modules (ice-9 format))

(define plugins-list '()) ; menu labels are updated to show current default settings

(define plugins-menu (add-to-main-menu "LADSPA Plugins" (lambda ()
						   (define (update-label plugins)
						     (if (not (null? plugins))
							 (begin
							   ((car plugins))
							   (update-label (cdr plugins)))))
						   (update-label plugins-list))))
(define (all-chans)
  (let ((sndlist '())
	(chnlist '()))
    (for-each (lambda (snd)
		(do ((i (1- (channels snd)) (1- i)))
		    ((< i 0))
		  (set! sndlist (cons snd sndlist))
		  (set! chnlist (cons i chnlist))))
	      (sounds))
    (list sndlist chnlist)))

(define map-chan-with-sync
  (lambda (func origin)
    (let ((snc (sync)))
      (if (> snc 0)
	  (apply map
		 (lambda (snd chn)
		   (if (= (sync snd) snc)
		       (map-chan (func) #f #f origin snd chn)))
		 (all-chans))
	  (map-chan (func) #f #f origin)))))

