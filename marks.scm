(define (move-syncd-marks sync diff)
  (map (lambda (m)
         (set-mark-sample m (+ (mark-sample m) diff)))
       (syncd-marks sync)))

(define (describe-mark id)
  (let ((mark-home (mark->sound id)))
    (if (eq? mark-home 'no-such-mark)
        ;; not an active mark, so go scrounging for it
        ;;   we're looking at every edit of every channel
        (do ((i 0 (1+ i)))
            ((or (= i (max-sounds)) (list? mark-home)))
          (if (ok? i)
              (do ((j 0 (1+ j)))
                  ((or (= j (channels i)) (list? mark-home)))
                (let* ((max-edits (apply + (edits i j))))
                  (do ((k 0 (1+ k)))
                      ((or (> k max-edits) (list? mark-home)))
                    (if (member id (marks i j k))
                        (set! mark-home (list i j)))))))))
    (if (list? mark-home)
        (let* ((snd (car mark-home))
               (chn (cadr mark-home))
               (max-edits (apply + (edits snd chn)))
               (descr '())
               (header (list 'mark id 'sound snd (short-file-name snd) 'channel chn)))
          (do ((i max-edits (1- i)))
              ((< i 0) descr)
            (if (member id (marks snd chn i))
                (set! descr (cons (mark-sample id i) descr))
                (set! descr (cons #f descr))))
          (cons header descr))
        'cant-find-mark)))


;;; -------- click marks between start-sync and stop-sync to sync them together
;;;  (easier than calling set-mark-sync over and over by hand)

(define mark-sync-number 0)
(define (start-sync) (set! mark-sync-number (+ (mark-sync-max) 1)))
(define (stop-sync) (set! mark-sync-number 0))
(define (click-to-sync id) (set-mark-sync id mark-sync-number) #f)
(add-hook! mark-click-hook click-to-sync)


;;; -------- syncronize sounds at a given mark

(define syncup
  (lambda ids
    (let* ((samps (map mark-sample ids))
	   (max-samp (apply max samps)))
      (define (pad-to-sync lst-ids lst-samps)
	(if (not (null? lst-ids))
	    (begin
	      (if (< (car lst-samps) max-samp)
		  (let ((nsamps (- max-samp (car lst-samps)))
			(snd-chn (mark->sound (car lst-ids))))
		    (insert-samples 0 nsamps (make-vct nsamps) (car snd-chn) (cadr snd-chn))))
	      (pad-to-sync (cdr lst-ids) (cdr lst-samps)))))
      (pad-to-sync ids samps))))
