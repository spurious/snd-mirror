;;; backwards compatibility for Snd-6

(use-modules (ice-9 optargs))

(define (Pixel pix) (if (list? pix) pix (list 'Pixel pix)))
(define (Widget wid) (if (list? wid) wid (list 'Widget wid)))
(define (GC gc) (if (list? gc) gc (list 'GC gc)))
(define (XtAppContext app) (if (list? app) app (list 'XtAppContext app)))
(define (snd-pixel pix) pix)

(define update-transform update-transform-graph)
(define mix-length mix-frames)
(define region-length region-frames)
(define selection-length selection-frames)

(define* (forward-sample #:optional (count 1) snd chn)
  (set! (cursor snd chn) (+ (cursor snd chn) count)))

(define* (backward-sample #:optional (count 1) snd chn)
  (set! (cursor snd chn) (max 0 (- (cursor snd chn) count))))

(define (vct-do! v func) 
  (vct-map! v (let ((i 0))
		(lambda ()
		  (let ((val (func i)))
		    (set! i (1+ i))
		    val)))))

(define (vcts-map! . args)
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


#!
;;; backwards compatibility for those using the -DSTR_OR=\"our\" flag
(define basic-colour basic-color)
(define colour->list color->list)
(define colour-cutoff color-cutoff)
(define colour-dialog color-dialog)
(define colour-inverted color-inverted)
(define colour? color?)
(define colour-scale color-scale)
(define cursor-colour cursor-color)
(define data-colour data-color)
(define enved-waveform-colour enved-waveform-color)
(define filter-waveform-colour filter-waveform-color)
(define foreground-colour foreground-color)
(define graph-colour graph-color)
(define highlight-colour highlight-color)
(define listener-colour listener-color)
(define listener-text-colour listener-text-color)
(define make-colour make-color)
(define mark-colour mark-color)
(define mix-colour mix-color)
(define position-colour position-color)
(define pushed-button-colour pushed-button-color)
(define sash-colour sash-color)
(define selected-data-colour selected-data-color)
(define selected-graph-colour selected-graph-color)
(define selected-mix-colour selected-mix-color)
(define selection-colour selection-color)
(define text-focus-colour text-focus-color)
(define zoom-colour zoom-color)
!#
