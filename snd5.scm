;;; backwards compatibility within Snd-5 versions

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

