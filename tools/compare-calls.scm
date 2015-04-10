;; find where two callgrind runs differ

(define (compare-calls f1 f2)
  (let ((h1 (with-input-from-file f1 read-calls))
	(total-diff 0)
	(diffs ())
	(scl 1e-6))

    (let ((h2 (with-input-from-file f2 read-calls)))
      (for-each 
       (lambda (kv1)
	 (let ((kv2 (hash-table-ref h2 (car kv1))))
	   (let ((diff (if kv2 (- kv2 (cdr kv1)) (- (cdr kv1)))))
	     (if (> (abs diff) 3e6)
		 (begin
		   (set! diffs (cons (list diff (car kv1) (cdr kv1) (or kv2 0)) diffs))
		   (set! total-diff (+ total-diff diff)))))))
       h1)
      (for-each
       (lambda (kv2)
	 (let ((kv1 (hash-table-ref h1 (car kv2))))
	   (if (not kv1)
	       (let ((diff (cdr kv2)))
		 (if (> (abs diff) 3e6)
		     (begin
		       (set! diffs (cons (list diff (car kv2) 0 (cdr kv2)) diffs))
		       (set! total-diff (+ total-diff diff))))))))
       h2))
		 
    (let ((vals (sort! diffs (lambda (a b) (> (car a) (car b))))))
      (format *stderr* "total: ~,3F~%" (* scl total-diff))
      (for-each
       (lambda (entry)
	 (format *stderr* "~A~,3F~12T(~,3F~24T~,3F)~40T~A~%" 
		 (if (negative? (list-ref entry 0)) "" " ")
		 (* scl (list-ref entry 0)) (* scl (list-ref entry 2)) (* scl (list-ref entry 3)) (list-ref entry 1)))
       vals)))
  (exit))

(define (string->number-ignoring-commas str)
  (let ((num 0)
	(tens 1)
	(len (length str)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) num)
      (if (char-numeric? (str i))
	  (begin
	    (set! num (+ num (* tens (- (char->integer (str i)) 48))))
	    (set! tens (* 10 tens)))))))

(define (read-calls)
  ;; throw away the header
  (do ((i 0 (+ i 1)))
      ((= i 25))
    (read-line))
  ;; read about 500 lines and store in a hash table as (func . timing)
  (let ((h (make-hash-table)))
    (call-with-exit
     (lambda (quit)
       (do ((i 0 (+ i 1)))
	   ((= i 500))
	 (let ((line (read-line)))
	   (if (eof-object? line)
	       (quit))
	   (let ((len (length line)))
	     (do ((k 0 (+ k 1)))
		 ((or (= k len)
		      (not (char-whitespace? (line k))))
		  (if (< k len)
		      (let ((end (char-position #\space line k)))
			(if end
			    (let ((num (string->number-ignoring-commas (substring line k end))))
			      (let ((func-end (char-position #\space line (+ end 2))))
				(if func-end
				    (let ((func (string->symbol (substring line (+ end 1) func-end))))
				      (hash-table-set! h func num)))))))))))))))
    h))
    

(define (get-overheads file)
  (with-input-from-file file
    (lambda ()
      (let ((overheads ())
	    (total 0))
	(do ((line (read-line) (read-line)))
	    ((eof-object? line) overheads)
	  (let ((len (min 20 (length line))))
	    ;(format *stderr* "line: ~A~%" line)
	    (do ((i 0 (+ i 1)))
		((or (= i len)
		     (not (char-whitespace? (line i))))
		 (if (and (< i (- len 4))
			  (char=? (line i) #\.)
			  (char=? (line (+ i 1)) #\space)
			  (char=? (line (+ i 2)) #\space)
			  (char-alphabetic? (line (+ i 3))))
		     (let ((next-line (read-line)))
		       (let ((nlen (length next-line)))
			 (if (char=? (next-line (- nlen 1)) #\{)
			     (do ((j 0 (+ j 1)))
				 ((or (= j nlen)
				      (and (char-numeric? (next-line j))
					   (let ((cost (string->number-ignoring-commas (substring next-line j (- nlen 3)))))
					     (set! total (+ total cost))
					     (set! overheads (cons (list cost (substring line (+ i 3) (min 80 (length line)))) overheads)))))))))))))))
	(set! overheads (sort! overheads (lambda (a b) (< (car a) (car b)))))
	(format *stderr* "~{~^~A~%~}" (list-tail overheads (max 10 (- (length overheads) 40))))
	(format *stderr* "total: ~A~%" total)))))

