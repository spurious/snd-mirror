;;; a repl

(require libc.scm)

(with-let (sublet *libc*)
  
  (define C-a (integer->char 1)) ; #\x01 etc
  (define C-b (integer->char 2))
  (define C-d (integer->char 4))
  (define C-e (integer->char 5))
  (define C-f (integer->char 6))
  (define C-h (integer->char 8))
  (define C-k (integer->char 11))
  (define C-m (integer->char 13)) ; #\return
  (define C-n (integer->char 14))
  (define C-p (integer->char 16))
  (define C-r (integer->char 18))
  (define C-t (integer->char 20))
  (define C-_ (integer->char 31))
  (define Tab (integer->char 9))
  (define Enter (integer->char 10)) ; #\linefeed
  (define Backspace (integer->char 127))
  
  (define cursor-home (format #f "~C[H" #\escape))
  (define cursor-right (format #f "~C[C" #\escape))
  (define cursor-left (format #f "~C[D" #\escape))
  (define erase-line (format #f "~C[2K" #\escape))
  (define erase-to-eol (format #f "~C[0K" #\escape))
  
  (define (bold text) (format #f "~C[1m~A~C[21m" #\escape text #\escape)) ; or is it 22??
  (define (underline text) (format #f "~C[4m~A~C[24m" #\escape text #\escape))
  (define (blink text) (format #f "~C[5m~A~C[25m" #\escape text #\escape))
  (define (red text) (format #f "~C[31m~A~C[0m" #\escape text #\escape)) ; black=30, green=32, yellow=33, blue=34
  (define (foreground text color) (format *stderr* "~C[38;5;~Dm~A~C[0m" #\escape color text #\escape)) ; color 0..255
  
  (define (cursor-forward n) (format #f "~C[~DC" #\escape n))
  (define (cursor-backward n) (format #f "~C[~DD" #\escape n))
  
  (define (symbol-completion text)
    (let ((st (symbol-table))
	  (len (length text))
	  (short-match #f)
	  (short-len 0))
      (call-with-exit
       (lambda (return)
	 (for-each
	  (lambda (symbol)
	    (let* ((name (symbol->string symbol))
		   (name-len (length name)))
	      (when (>= name-len len)
		(when (string=? text (substring name 0 len))
		  ;; symbol matches text, need to return shortest such match
		  (if (not short-match)
		      (begin
			(set! short-match name)
			(set! short-len name-len))
		      (if (< (length name) short-len)
			  (if (= name-len len)
			      (return text)
			      (begin
				(set! short-match name)
				(set! short-len name-len)))))))))
	  st)
	 (or short-match text)))))
  
  (define (filename-completion text)
    (let ((g (glob.make)))
      (glob (string-append text "*")
	    (logior (if (and (defined? 'GLOB_TILDE)
			     (char=? (text 0) #\~))
			GLOB_TILDE
			0)
		    GLOB_MARK)
	    g)
      (let ((files (glob.gl_pathv g)))
	(globfree g) 
	(if (null? files)
	    text
	    (if (null? (cdr files))
		(car files)
		(if (< (length (car files)) (length (cadr files)))
		    (car files)
		    (cadr files)))))))
  
  (let ((saved (termios.make))
	(fn (fileno stdin)))
    
    (define (tty-reset no)
      (tcsetattr fn TCSAFLUSH saved)
      (#_exit))
    
    (define (exit)
      (newline *stderr*)
      (tty-reset 0))
    
    (if (or (equal? (signal SIGINT tty-reset) SIG_ERR)
	    (equal? (signal SIGQUIT tty-reset) SIG_ERR)
	    (equal? (signal SIGTERM tty-reset) SIG_ERR)
	    (negative? (tcgetattr fn saved)))
	(exit))
    
    (let ((buf (termios.make))
	  (c (string #\null #\null)))
      
      (let ((cc (string->c-pointer c)))
	(tcgetattr fn buf)
	(termios.set_c_lflag buf (logand (termios.c_lflag buf) (lognot (logior ECHO ICANON))))
	(termios.set_c_cc buf VMIN 1)
	(termios.set_c_cc buf VTIME 0)
	(when (negative? (tcsetattr fn TCSAFLUSH buf))
	  (tty-reset fn))
	
	(let ((history-buffer (make-vector 100 #f))
	      (current-history-size 100)
	      (history-position 0)
	      (history-index 0))
	  
	  (define history-size (dilambda
				(lambda ()
				  current-history-size)
				(lambda (new-size)
				  (set! history-buffer (copy history-buffer (make-vector new-size #f)))
				  (set! current-history-size new-size))))
  
	  (define history (dilambda 
			   (lambda (back)
			     (let ((i (+ history-position back)))
			       (if (< i 0)
				   (history-buffer (+ current-history-size i))
				   (if (>= i current-history-size)
				       (history-buffer (- i current-history-size))
				       (history-buffer i)))))
			   (lambda (new-line)
			     (set! (history-buffer history-position) new-line)
			     (set! history-position (+ history-position 1))
			     (if (= history-position current-history-size)
				 (set! history-position 0)))))
	  
	  (let ((current-line "")
		(previous-line #f)
		(cursor-position 0))

	    (define keymap (make-vector 256 #f))
	    
	    
	    (do ((i (read fn cc 1) (read fn cc 1)))
		((not (= i 1))
		 (tty-reset fn))
	      (cond
	       
	       ((char=? (c 0) C-a) 
		(format *stderr* "~C" #\return)
		(set! cursor-position 0))
	       
	       ((char=? (c 0) C-b) 
		(when (> cursor-position 0)
		  (format *stderr* cursor-left)
		  (set! cursor-position (- cursor-position 1))))
	       
	       ((char=? (c 0) C-d)
		(let ((len (length current-line)))
		  (when (< cursor-position len)
		    (set! previous-line (copy current-line))
		    (do ((i cursor-position (+ i 1)))
			((>= i (- len 1)))
		      (set! (current-line i) (current-line (+ i 1))))
		    (set! current-line (substring current-line 0 (- len 1)))
		    (format *stderr* "~A~A~C" erase-to-eol (substring current-line cursor-position) #\return)
		    (if (> cursor-position 0)
			(format *stderr* (cursor-forward cursor-position))))))
	       
	       ((char=? (c 0) C-e) 
		(let ((len (length current-line)))
		  (when (< cursor-position len)
		    (format *stderr* (cursor-forward (- len cursor-position)))
		    (set! cursor-position len))))
	       
	       ((char=? (c 0) C-f) 
		(when (< cursor-position (length current-line))
		  (format *stderr* cursor-right)
		  (set! cursor-position (+ cursor-position 1))))
	       
	       ((char=? (c 0) C-k) 
		(when (< cursor-position (length current-line))
		  (set! previous-line (copy current-line))
		  (format *stderr* erase-to-eol)
		  (set! current-line (substring current-line 0 cursor-position))))
	       
	       ((char=? (c 0) C-t) 
		(when (and (> (length current-line) 1)
			   (> cursor-position 0))
		  (set! previous-line (copy current-line))
		  (let ((cur (if (= cursor-position (length current-line))
				 (- cursor-position 1)
				 cursor-position)))
		    (let ((tmp-c (current-line (- cur 1))))
		      (set! (current-line (- cur 1)) (current-line cur))
		      (set! (current-line cur) tmp-c)
		      (format *stderr* "~C~A~A" #\return erase-to-eol current-line)
		      (set! cursor-position (+ cur 1))
		      (format *stderr* "~C~A" #\return (cursor-forward cursor-position))))))
	       
	       ((char=? (c 0) C-_)
		(when previous-line
		  (set! current-line previous-line)
		  (set! previous-line #f)
		  (format *stderr* "~C~A~A" #\return erase-to-eol current-line)
		  (format *stderr* "~C~A" #\return (cursor-forward (set! cursor-position (min cursor-position (length current-line)))))))
	       
	       ((char=? (c 0) Enter)
		(set! (history) current-line)
		(catch #t
		  (lambda ()
		    (format *stderr* "~%~S~%" (eval-string current-line (curlet))))
		  (lambda (type info)
		    (apply format *stderr* info)
		    (newline *stderr*)))
		(set! current-line "")
		(set! cursor-position 0))
	       
	       ((memv (c 0) (list Backspace C-h))
		(when (> cursor-position 0)
		  (set! previous-line (copy current-line))
		  (let ((len (length current-line)))
		    (set! cursor-position (- cursor-position 1))
		    (format *stderr* cursor-left)
		    (do ((i cursor-position (+ i 1)))
			((>= i (- len 1)))
		      (set! (current-line i) (current-line (+ i 1))))
		    (set! current-line (substring current-line 0 (- len 1)))
		    (format *stderr* "~A~A~C" erase-to-eol (substring current-line cursor-position) #\return)
		    (if (> cursor-position 0)
			(format *stderr* (cursor-forward cursor-position))))))
	       
	       ((char=? (c 0) C-p)
		(when (and (zero? history-index) (> (length current-line) 0))
		  (set! (history) current-line)
		  (set! history-index -1))
		(set! history-index (max (- history-index 1) (- current-history-size)))
		(when (history history-index)
		  (set! current-line (history history-index))
		  (format *stderr* "~C" #\return)
		  (format *stderr* erase-line)
		  (format *stderr* "~A" current-line)
		  (set! cursor-position (length current-line))))
	       
	       ((char=? (c 0) C-n)
		(set! history-index (min 0 (+ history-index 1)))
		(when (history history-index)
		  (set! current-line (history history-index))
		  (format *stderr* "~C" #\return)
		  (format *stderr* erase-line)
		  (format *stderr* "~A" current-line)
		  (set! cursor-position (length current-line))))
	       
	       ((char=? (c 0) #\escape)
		;; arrow-keys etc
		(read fn cc 1)
		(when (char=? (c 0) #\[)
		  (read fn cc 1)
		  (case (c 0)
		    ((#\A) (format *stderr* "up arrow")) ; esc n A?
		    ((#\B) (format *stderr* "down arrow")) ; esc n B?
		    ((#\C) (format *stderr* "right arrow"))
		    ((#\D) (format *stderr* "left arrow"))
		    ((#\1) 
		     (read fn cc 1) ; random -- good up to F5 anyway?
		     (format *stderr* "F~D?" (- (char->integer (c 0)) (char->integer #\0)))
		     (read fn cc 1)) ; tilde (126)
		    (else (format *stderr* "got ~C" (c 0)))
		    )))
	       
	       ((char=? (c 0) Tab)
		;; completion: filename if in string, symbol else plus remember possible ' at start
		)
	       
	       (else 
					;(format *stderr* "~D" (char->integer (c 0)))
		(set! current-line (string-append current-line (string (c 0))))
		(set! cursor-position (+ cursor-position 1))
		(set! history-index 0)
		(format *stderr* "~C" (c 0)))
	       
	       ))))))))

;; need prompt handling, tab=>completion, paren matches, multiline edits
;; keymap + function keys I guess
;; remove readline from s7/Snd [xen.c for s7]
;; change the huge cond to a table, user-settable
;; save|load-history
;; doc repl.scm and add at ccrma
;; set outlet funclet of keymap entry so "free vars" like cursor-position map to the locals  
;; in|out port lint.scm