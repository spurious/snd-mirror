;;; backwards compatibility for Snd-6

(use-modules (ice-9 optargs))
(provide 'snd-snd5.scm)

(define (Pixel pix) 
  "obsolete wrapper for Pixels" 
  (if (list? pix) pix (list 'Pixel pix)))

(define (Widget wid) 
  "obsolete wrapper for Widgets" 
  (if (list? wid) wid (list 'Widget wid)))

(define (GC gc) 
  "obsolete wrapper for GCs" 
  (if (list? gc) gc (list 'GC gc)))

(define (XtAppContext app) 
  "obsolete wrapper for XtAppContexts" 
  (if (list? app) app (list 'XtAppContext app)))

(define (snd-pixel pix) 
  "obsolete -- simply returns its argument" 
  pix)

;(define update-transform update-transform-graph)
;(define mix-length mix-frames)
;(define region-length region-frames)
;(define selection-length selection-frames)

(define* (forward-sample :optional (count 1) snd chn)
  "(forward-sample :optional (count 1) snd chn) moves the given channels' cursor forward 'count' samples"
  (set! (cursor snd chn) (+ (cursor snd chn) count)))

(define* (backward-sample :optional (count 1) snd chn)
  "(backward-sample :optional (count 1) snd chn) moves the given channels' cursor backward 'count' samples"
  (set! (cursor snd chn) (max 0 (- (cursor snd chn) count))))

(define (vct-do! v func) 
  "(vct-do! v func) applies func to an index, filling v"
  (vct-map! v (let ((i 0))
		(lambda ()
		  (let ((val (func i)))
		    (set! i (1+ i))
		    val)))))

(define (vcts-map! . args)
  "(vcts-map! . args) is obsolete and pointless"
  (let* ((arglen (length args))
	 (func (list-ref args (1- arglen)))
	 (vcts (reverse (cdr (reverse args))))
	 (num-vcts (length vcts))
	 (vlen (apply min (map vct-length vcts))))
    (do ((i 0 (1+ i)))
	((= i vlen))
      (for-each
       (lambda (v val) (vct-set! v i val))
       vcts 
       (func num-vcts)))))

(define (vcts-do! . args)
  "(vcts-do! . args) is obsolete and pointless"
  (let* ((arglen (length args))
	 (func (list-ref args (1- arglen)))
	 (vcts (reverse (cdr (reverse args))))
	 (num-vcts (length vcts))
	 (vlen (apply min (map vct-length vcts))))
    (do ((i 0 (1+ i)))
	((= i vlen))
      (for-each
       (lambda (v val) (vct-set! v i val))
       vcts 
       (func num-vcts i)))))
