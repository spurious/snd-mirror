#!/usr/local/bin/guile -s
!#

(use-modules (ice-9 popen) (ice-9 format))

(define (shell cmd)
  (with-output-to-string
    (lambda ()
      (let ((in-port (open-input-pipe cmd)))
	(let loop ((line (read-line in-port 'concat)))
	  (or (eof-object? line)
	      (begin
		(display line)
		(loop (read-line in-port 'concat)))))))))

(define (trim-from-cap-after-space str cap)
  (let ((len (string-length str))
	(start 0))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 1 (1+ i)))
	   ((= i len) #f)
	 (if (and
	      (> start 0)
	      (or (char=? (string-ref str i) #\space)
		  (char=? (string-ref str i) #\))
		  (char=? (string-ref str i) #\,)))
	     (return (substring str start i)))
	 (if (and (char=? (string-ref str i) cap)
		  (not (char-alphabetic? (string-ref str (1- i)))))
	     (set! start i)))))))

(define (scan-for-proc-name str)
  (let ((len (string-length str))
	(start 0)
	(in-field #f))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 1 (1+ i)))
	   ((= i len) #f)
	 (if (and
	      (> start 0)
	      (char=? (string-ref str i) #\,))
	     (return (substring str start (- i 2))))
	 (if (and in-field
		  (= start 0)
		  (char-alphabetic? (string-ref str i)))
	     (set! start i))
	 (if (char=? (string-ref str i) #\,)
	     (set! in-field #t))
	 )))))

(define (scan-for-semi str)
  (let ((len (string-length str)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 1 (1+ i)))
	   ((= i len) #f)
	 (if (char=? (string-ref str i) #\:)
	     (return (substring str 0 i))))))))

(define (scan-for-line str)
  (let ((len (string-length str)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 1 (1+ i)))
	   ((= i len) str)
	 (if (char=? (string-ref str i) #\:)
	     (let* ((line (substring str (+ i 1) (- len 1)))
		    (intlen (string-length line)))
	       (do ((j 1 (1+ j)))
		   ((= j intlen) 0)
		 (if (char=? (string-ref line j) #\:)
		     (let ((num (substring line 0 j)))
		       (return (string->number num))))))))))))

(let* ((name (cadr (command-line)))
       (filename (caddr (command-line)))
       (S_name_with_define (shell (format #f "fgrep '~S' *.[ch]" name))))
  (if (and (string? S_name_with_define)
	   (> (string-length S_name_with_define) 0))
      (let* ((S_name_alone (trim-from-cap-after-space S_name_with_define #\S))
	     (H_name_with_define (shell (format #f "fgrep 'XEN_DEFINE_PROCEDURE(~A,' *.c" S_name_alone))))
	(if (or (not (string? H_name_with_define))
		(= (string-length H_name_with_define) 0))
	    (set! H_name_with_define (shell (format #f "fgrep 'XEN_DEFINE_PROCEDURE_WITH_SETTER(~A,' *.c" S_name_alone))))
	(if (or (not (string? H_name_with_define))
		(= (string-length H_name_with_define) 0))
	    (set! H_name_with_define (shell (format #f "fgrep 'XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER(~A,' *.c" S_name_alone))))
	(if (and (string? H_name_with_define)
		 (> (string-length H_name_with_define) 0))
	    (let* ((H_name_alone (trim-from-cap-after-space H_name_with_define #\H))
		   (func (scan-for-proc-name H_name_with_define)))
	      (if (and (string? H_name_alone)
		       (> (string-length H_name_alone) 0))
		  (let ((H_loc (shell (format #f "fgrep '#define ~A ' *.[ch] --line-number" H_name_alone))))
		    (if (and (string? H_loc)
			     (> (string-length H_loc) 0))
			(let* ((file (scan-for-semi H_loc))
			       (line (scan-for-line H_loc)))
			  (shell (format #f "echo '(list ~S \"~A\" ~D \"~A\")\n' >> ~A" name file line func filename)))
			(display (format #f "~A: no H_loc~%" name))))
		  (display (format #f "~A: no h_name_alone~%" name))))
	    (display (format #f "~A: no h_name_with_define~%" name))))
      (display (format #f "~A: no s_name~%" name))))

(exit)

;;; TODO: scm locations and color names 
