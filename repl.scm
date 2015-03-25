;;; a repl
;;;
;;; (load "repl.scm") ((*repl* 'run))

(provide 'repl.scm)
(require libc.scm)

(unless (defined? '*repl*)
  (define *repl*               ; environment that holds the REPL functions
    (let ((prompt #f)          ; function to get/set prompt
	  (keymap #f)          ; function to get/set keymap entries
	  (history #f)         ; function to get/set history buffer entries
	  (save-history #f)    ; function to save the current history buffer entries in a file
	  (restore-history #f) ; function to restore history buffer entries from a file
	  (run #f)             ; function that fires up a REPL
	  (repl-let            ; environment for keymap functions to access all the REPL innards (cursor-position etc)
      
      (with-let (sublet *libc*)

	;; -------- completion --------
	(define (symbol-completion text)
	  (let ((st (symbol-table))
		(text-len (length text))
		(match #f))
	    (call-with-exit
	     (lambda (return)
	       (for-each
		(lambda (symbol)
		  (let* ((sym (symbol->string symbol))
			 (sym-len (length sym)))
		    (when (and (>= sym-len text-len)
			       (string=? text (substring sym 0 text-len)))
		      (if match
			  (return text)
			  (set! match sym)))))
		st)
	       (or match text)))))
	
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
	      (if (or (null? files)
		      (not (null? (cdr files))))
		  text
		  (car files)))))


	;; -------- history --------
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
	  
	  (define* (save-history (file "repl-history.scm"))
	    (call-with-output-file file
	      (lambda (port)
		(format port "(set! history-position ~D)~%" history-position)
		(format port "(set! current-history-size ~D)~%" current-history-size)
		(let ((pl (*s7* 'print-length)))
		  (set! (*s7* 'print-length) 1024)
		  (format port "(set! history-buffer ~A)~%" (object->string history-buffer))
		  (set! (*s7* 'print-length) pl)))))
	  
	  (define* (restore-history (file "repl-history.scm"))
	    (load file (curlet)))

	  
	  ;; -------- evaluation ---------
	  (define (badsym h)
	    (set! (h 'result) ((*repl* 'repl-let) (h 'variable))))

	  (define (badexpr h)
	    (set! (h 'result) 'incomplete-expr))
	  
	  (define-macro (with-repl-let expr)
	    ;; for multiline edits, we will use *missing-close-paren-hook* rather than try to parse the input ourselves.
	    `(let ((old-badexpr-hook (hook-functions *missing-close-paren-hook*)))
	       (dynamic-wind
		   (lambda ()
		     (set! (hook-functions *missing-close-paren-hook*) (cons badexpr old-badexpr-hook)))
		   (lambda ()
		     ,expr)
		   (lambda ()
		     (set! (hook-functions *missing-close-paren-hook*) old-badexpr-hook)))))

	  
	  (let ((prompt-string "> ")
		(prompt-length 2)
		(current-line "")          ; current expression might be a better name
		(previous-line #f)         ; for undo via C-_
		(selection #f)             ; for C-y
		(cursor-position 0)        ; cursor-position is the logical position (index into current-line)
		(current-row 0)            ; catch window scrolling
		(red-par-pos #f))          ; paren match position (index into current-line)
		 
	    ;; -------- match parens --------
	    (define (char-constant? pos)
	      (and (> pos 2)
		   (char=? (current-line (- pos 1)) #\\)
		   (char=? (current-line (- pos 2)) #\#)))
	    
	    (define (check-parens)
	      (if (and (> cursor-position 1)
		       (char=? (current-line (- cursor-position 1)) #\)) ; ")" on left of cursor
		       (not (char-constant? (- cursor-position 1))))     ; it's not "#\)"
		  (let ((oparens 1)
			(dquotes 0)
			(new-red-pos #f))
		    (do ((i (- cursor-position 2) (- i 1)))
			((or new-red-pos (< i 0)))
		      (unless (char-constant? i)
			(case (current-line i)
			  ((#\)) 
			   (if (zero? dquotes)
			       (set! oparens (+ oparens 1))))
			  ((#\()
			   (set! oparens (- oparens 1))
			   (if (and (zero? dquotes)
				    (zero? oparens))
			       (set! new-red-pos i)))
			  ((#\") 
			   (when (or (= i 0)
				     (zero? dquotes)
				     (not (char=? (current-line (- i 1)) #\\)))
			     (set! dquotes (modulo (+ dquotes 1) 2))))
			  ((#\;) 
			   (if (zero? dquotes)
			       (set! new-red-pos #t))))))
		    (unless (equal? new-red-pos red-par-pos)
		      (if (number? new-red-pos)
			  (set! red-par-pos new-red-pos)
			  (set! red-par-pos #f))))
		  (if (number? red-par-pos)
		      (set! red-par-pos #f))))
	    
	    
	    ;; -------- indentation --------
	    (define (indent pos)
	      (let ((old-red red-par-pos)
		    (old-line (copy current-line))
		    (old-cursor cursor-position))
		(set! current-line (string-append (substring current-line 0 cursor-position) (string #\))))
		(set! cursor-position (length current-line))
		(check-parens)
		(set! current-line old-line)
		(set! cursor-position old-cursor)
		(let ((new-red red-par-pos))
		  (set! red-par-pos old-red)
		  (let ((col 0))
		    (do ((i 0 (+ i 1)))
			((= i new-red))
		      (if (char=? (current-line i) #\newline)
			  (set! col 0)
			  (set! col (+ col 1))))
		    (let ((sym (do ((i (+ new-red 1) (+ i 1)))
				   ((not (char-alphabetic? (current-line i)))
				    (substring current-line (+ new-red 1) i)))))
		      (let ((spaces (+ col (if (member sym '("or" "and" "cond" "if"))
					       (+ (length sym) 2)
					       2))))
			(if (= cursor-position (length current-line))
			    (begin
			      (set! current-line (format #f "~A~NC" current-line spaces #\space))
			      (set! cursor-position (length current-line)))
			    (begin
			      (set! current-line (format #f "~A~NC~A" 
							 (substring current-line 0 cursor-position)
							 spaces #\space
							 (substring current-line (+ cursor-position 1))))
			      (set! cursor-position (+ cursor-position spaces))))))))))


	    ;; -------- prompt --------
	    (define prompt (dilambda
			    (lambda ()
			      prompt-string)
			    (lambda (new-prompt)
			      (set! prompt-length (length new-prompt))
			      (set! prompt-string new-prompt))))
	    

	    ;; -------- vt100 --------
	    (define (bold text) (format #f "~C[1m~A~C[21m" #\escape text #\escape)) 
	    (define (red text) (format #f "~C[31m~A~C[0m" #\escape text #\escape))  ; black=30, green=32, yellow=33, blue=34
	    
	    (define* (rgb text (r 0) (g 0) (b 0) all-colors)
	      (if all-colors
		  (format #f "~C[38;5;~Dm~A~C[0m" #\escape (+ 16 (* 36 (round (* r 5))) (* 6 (round (* g 5))) (round (* b 5))) text #\escape)
		  (format #f "~C[~Dm~A~C[0m" #\escape (+ 30 (ash (round b) 2) (ash (round g) 1) (round r)) text #\escape)))

	    (define (cursor-coords)
	      (let* ((c (string #\null #\null))
		     (cc (string->c-pointer c))
		     (fn (fileno stdin)))
		(format *stderr* "~C[6n" #\escape)
		(do ((b (read fn cc 1) (read fn cc 1)))
		    ((char=? (c 0) #\escape)))
		(read fn cc 1) 
		(and (char=? (c 0) #\[)
		     (let ((y 0)
			   (x 0))
		       (do ((b (read fn cc 1) (read fn cc 1)))
			   ((not (char-numeric? (c 0))))
			 (set! y (+ (* 10 y) (- (char->integer (c 0)) (char->integer #\0)))))
		       (and (char=? (c 0) #\;)
			    (do ((b (read fn cc 1) (read fn cc 1)))
				((not (char-numeric? (c 0)))
				 (and (char=? (c 0) #\R)
				      (cons x y)))
			      (set! x (+ (* 10 x) (- (char->integer (c 0)) (char->integer #\0))))))))))


	    ;; -------- display --------
	    (let* ((c (string #\null #\null))
		   (cc (string->c-pointer c))
		   (fn (fileno stdin))
		   (saved #f)
		   (prompt-row 0)  ; need to tie everything to prompt position (it can move in media res if window scrolls)
		   (prompt-col 0)
		   (last-row 0)    ; these hold the window bounds when we start
		   (last-col 0))

	      (define (cursor-bounds)
		(let ((coords (cursor-coords)))
		  (set! prompt-col (car coords))
		  (set! prompt-row (cdr coords))
		  (format *stderr* "~C[~D;~DH" #\escape 4000 4000)
		  (let ((bounds (cursor-coords)))
		    (format *stderr* "~C[~D;~DH" #\escape (cdr coords) (car coords))
		    (set! last-col (car bounds))
		    (set! last-row (cdr bounds)))))
	      
	      (define (display-prompt)
		(format *stderr* "~A" prompt-string))
	      
	      (define (new-prompt)
		(set! current-line "")
		(set! cursor-position 0)
		(set! current-row 0)
		(display-prompt)
		(cursor-bounds))

	      (define (display-line start end)
		(if (and red-par-pos
			 (<= start red-par-pos)
			 (< red-par-pos end))
		    (string-append
		     (if (zero? start)
			 (format #f "~A" prompt-string)
			 (format #f "~NC" prompt-length #\space))
		     (if (= start red-par-pos)
			 (format #f "~A~A"
				 (bold (red "(")) 
				 (substring current-line (+ start 1) end))
			 (format #f "~A~A~A"
				 (substring current-line start red-par-pos) 
				 (bold (red "(")) 
				 (substring current-line (+ red-par-pos 1) end))))
		    (if (zero? start)
			(format #f "~A~A" prompt-string (substring current-line 0 end))
			(format #f "~NC~A" prompt-length #\space (substring current-line start end)))))
		
	      (define (display-lines)
		(format *stderr* "~C[~D;0H~C[J" #\escape prompt-row #\escape) ; set cursor back to prompt, erase down from there
		
		(let ((len (length current-line))
		      (new-line ""))
		  (let ((line-end 0))
		    (do ((i 0 (+ line-end 2)))
			((> i len))
		      (set! line-end (end-of-line i))
		      (set! new-line (string-append new-line (display-line i (min (+ line-end 2) len))))))
		  (format *stderr* "~A" new-line)
		  (let ((row 0)
			(start 0))
		    (do ((i 0 (+ i 1)))
			((or (= i len)
			     (= i cursor-position))
			 (format *stderr* "~C[~D;~DH" #\escape (+ prompt-row row) (+ prompt-col (- cursor-position start))))
		      (when (char=? (current-line i) #\newline)
			(set! row (+ row 1))
			(set! start (+ i 1)))))))
	      
	      
	      ;; -------- keymap --------
	      (define keymap-functions (make-vector 256 #f))
	      (define keymap (dilambda
			      (lambda (c)
				(keymap-functions (char->integer c)))
			      (lambda (c f)
				(set! (keymap-functions (char->integer c)) f))))
	      
	      (define C-a 1)     ; #\x01 etc
	      (define C-b 2)
	      (define C-d 4)
	      (define C-e 5)
	      (define C-f 6)
	      (define C-h 8)
	      (define C-k 11)
	      ;(define C-m 13)    ; #\return
	      (define C-n 14)
	      (define C-p 16)
	      ;(define C-r 18)
	      (define C-t 20)
	      (define C-y 25)
	      (define C-_ 31)
	      (define Tab 9)
	      (define Enter 10)  ; #\linefeed
	      (define Backspace 127)
	      (define Escape 27) ; #\escape
	      
	      (define (end-of-line pos)             ; prompt or #\newline mark the start of a line
		(let ((len (length current-line)))
		  (do ((i (min pos len) (+ i 1)))
		      ((or (>= i len)
			   (char=? (current-line i) #\newline))
		       (if (>= i len) len (- i 1))))))
	      
	      (define (start-of-line pos)
		(if (<= pos 0)
		    0
		    (do ((i (min pos (- (length current-line) 1)) (- i 1)))
			((or (zero? i)
			     (char=? (current-line i) #\newline))
			 i))))
	      
	      (define (count-newlines line)
		(let ((len (length line))
		      (newlines 0))
		  (do ((i 0 (+ i 1)))
		      ((= i len) newlines)
		    (if (char=? (current-line i) #\newline)
			(set! newlines (+ newlines 1))))))

	      (define (append-newline)
		(set! current-line (string-append current-line (string #\space #\newline)))
		(set! cursor-position (length current-line))
		(when (= last-row (+ prompt-row current-row))
		  (format *stderr* "~%")
		  (set! prompt-row (- prompt-row 1)))
		(set! current-row (+ current-row 1)))

	      (define (move-cursor-up)
		;; try to find corresponding column in previous line
		(let ((start (start-of-line cursor-position)))
		  (when (positive? start)
		    (let ((upstart (start-of-line (- start 1))))
		      (if (zero? upstart)
			  (set! cursor-position (min (- start 1) (- cursor-position start 1))) ; (min toplen curpos-in-line)
			  (set! cursor-position (+ upstart (min (- start upstart 1) (- cursor-position start)))))))))
	      
	      (define (move-cursor-down)
		;; try to find corresponding column in next line
		(let ((len (length current-line))
		      (next-start (+ (end-of-line cursor-position) 1)))      ; should be #\newline (if any)
		  (if (> len (+ next-start 1))                               ; not already at last line
		      (if (char=? (current-line (+ next-start 1)) #\newline) ; line is empty
			  (set! cursor-position next-start)
			  (let ((next-end (end-of-line (+ next-start 1)))
				(start (start-of-line cursor-position)))
			    (if (zero? start)                                  ; we're on first line now
				(set! cursor-position (min next-end (+ next-start cursor-position 1)))
				(set! cursor-position (min next-end (+ next-start (- cursor-position start))))))))))
	      
	      (define (word-break pos) ; assume we're at the start of a word
		(let ((len (length current-line)))
		  (let loop ((i pos))
		    (if (or (>= i len)
			    (not (or (char-alphabetic? (current-line i))
				     (char-numeric? (current-line i)))))
			i
			(loop (+ i 1))))))
	      
	      (let ((main-keyfunc (lambda (c)
				    (if (= cursor-position (length current-line))
					(set! current-line (string-append current-line (string c)))
					(if (= cursor-position 0)
					    (set! current-line (string-append (string c) current-line))
					    (set! current-line (string-append 
								(substring current-line 0 cursor-position) 
								(string c) 
								(substring current-line cursor-position)))))
				    (set! cursor-position (+ cursor-position 1))
				    (set! history-index 0)))
		    (no-op-keyfunc (lambda (c) #f)))
		(do ((i 0 (+ i 1)))
		    ((= i 32))
		  (set! (keymap-functions i) no-op-keyfunc))
		(do ((i 32 (+ i 1)))
		    ((= i 256))
		  (set! (keymap-functions i) main-keyfunc)))
	      
	      (set! (keymap-functions C-a) (lambda (c)
					     (let ((start (start-of-line cursor-position)))
					       (if (zero? start)
						   (set! cursor-position 0)
						   (set! cursor-position (+ start 1))))))
	      
	      (set! (keymap-functions C-e) (lambda (c)
					     (set! cursor-position (end-of-line cursor-position))))
	      
	      (set! (keymap-functions C-b) (lambda (c)
					     (when (> cursor-position 0)
					       (set! cursor-position (- cursor-position 1))
					       (if (char=? (current-line cursor-position) #\newline)
						   (set! cursor-position (- cursor-position 1))))))
	      
	      (set! (keymap-functions C-f) (lambda (c)
					     (let ((len (length current-line)))
					     (when (< cursor-position len)
					       (set! cursor-position (+ cursor-position 1))
					       (if (and (< cursor-position len)
							(char=? (current-line cursor-position) #\newline))
						   (set! cursor-position (+ cursor-position 1)))))))
	      
	      (set! (keymap-functions C-d) (lambda (c)
					     (let ((len (length current-line)))
					       (when (< cursor-position len)
						 (set! previous-line (copy current-line))
						 (do ((i cursor-position (+ i 1)))
						     ((>= i (- len 1)))
						   (set! (current-line i) (current-line (+ i 1))))
						 (set! current-line (substring current-line 0 (- len 1)))))))
	      
	      (set! (keymap-functions C-k) (lambda (c)
					     (let ((len (length current-line)))
					       (when (< cursor-position len)
						 (set! previous-line (copy current-line))
						 (let ((end (end-of-line cursor-position)))
						   (if (= end len)
						       (begin
							 (set! selection (substring current-line cursor-position))
							 (set! current-line (substring current-line 0 cursor-position)))
						       (if (= cursor-position end) ; delete next newline
							   (set! current-line (string-append (substring current-line 0 (+ end 1))
											     (substring current-line (+ end 2))))
							   (begin
							     (set! selection (substring current-line cursor-position (+ end 1)))
							     (set! current-line (string-append (substring current-line 0 cursor-position)
											       (substring current-line (+ end 1))))
							     (set! cursor-position (- cursor-position 1))
							     ))))))))
	      
	      (set! (keymap-functions C-y) (lambda (c)
					     (when selection
					       (set! previous-line (copy current-line))
					       (if (zero? cursor-position)
						   (set! current-line (string-append selection current-line))
						   (if (>= cursor-position (length current-line))
						       (set! current-line (string-append current-line selection))
						       (set! current-line (string-append (substring current-line 0 cursor-position)
											 selection
											 (substring current-line cursor-position)))))
					       (set! cursor-position (+ cursor-position (length selection))))))
	      
	      (set! (keymap-functions C-t) (lambda (c)
					     (let ((start (start-of-line cursor-position))
						   (end (end-of-line cursor-position))
						   (len (length current-line)))
					       (if (positive? start) 
						   (set! start (+ start 1)))
					       (when (and (> end start)
							  (> cursor-position start))
						 (set! previous-line (copy current-line))
						 (let ((cur (if (or (= cursor-position end)
								    (< end len))
								(- cursor-position 1)
								cursor-position)))
						   (let ((tmp-c (current-line (- cur 1))))
						     (set! (current-line (- cur 1)) (current-line cur))
						     (set! (current-line cur) tmp-c)
						     (set! cursor-position (+ cur 1))))))))
	      
	      (set! (keymap-functions C-_) (lambda (c)
					     (when previous-line
					       (set! current-line previous-line)
					       (set! previous-line #f)
					       (set! cursor-position (min cursor-position (length current-line)))
					       #f)))
	      
	      (set! (keymap-functions Enter) (lambda (c)

					       (call-with-exit
						(lambda (return)
						  (let ((len (length current-line)))

						    (do ((i 0 (+ i 1))) ; check for just whitespace
							((or (= i len)
							     (not (char-whitespace? (current-line i))))
							 (when (= i len)
							   (append-newline)
							   (return))))
						  
						    (set! (history) current-line)
						    (set! red-par-pos #f)
						    (display-lines)
						  
						    (with-repl-let
						     (catch #t
						       (lambda ()
							 
							 (catch 'incomplete-expr
							   (lambda ()
							     (set! cursor-position len)
							     (display-lines)
							     (format *stderr* "~%~S~%" (eval-string current-line (rootlet))))
							   (lambda args
							     (append-newline)
							     (return))))
						       
						       (lambda (type info)
							 (format *stderr* "~%~A: " (red "error"))
							 (apply format *stderr* info)
							 (newline *stderr*))))
						    
						    (new-prompt))))))
	      
	      (set! (keymap-functions C-h) (lambda (c)
					     (let ((start (start-of-line cursor-position)))
					       (when (> cursor-position start)
						 (set! previous-line (copy current-line))
						 (let ((len (length current-line)))
						   (set! cursor-position (- cursor-position 1))
						   (do ((i cursor-position (+ i 1)))
						       ((>= i (- len 1)))
						     (set! (current-line i) (current-line (+ i 1))))
						   (set! current-line (substring current-line 0 (- len 1))))))))
	      
	      (set! (keymap-functions Backspace) (keymap-functions C-h))
	      
	      (set! (keymap-functions C-p) (lambda (c)
					     (when (and (zero? history-index) 
							(> (length current-line) 0))
					       (set! (history) current-line)
					       (set! history-index -1))
					     (set! history-index (max (- history-index 1) (- current-history-size)))
					     (when (history history-index)
					       (set! current-line (history history-index))
					       (set! cursor-position (length current-line))
					       (let ((newlines (count-newlines current-line)))
						 (if (< last-row (+ prompt-row newlines))
						     (set! prompt-row (- prompt-row newlines)))
						 (set! current-row newlines)))
					     #f))
	      
	      
	      (set! (keymap-functions C-n) (lambda (c)
					     (set! history-index (min 0 (+ history-index 1)))
					     (when (history history-index)
					       (set! current-line (history history-index))
					       (set! cursor-position (length current-line))
					       (let ((newlines (count-newlines current-line)))
						 (if (< last-row (+ prompt-row newlines))
						     (set! prompt-row (- prompt-row newlines)))
						 (set! current-row newlines)))
					     #f))
	      
	      (set! (keymap-functions Escape) (lambda (esc)
						(let* ((c (string #\null #\null))
						       (cc (string->c-pointer c))
						       (fn (fileno stdin)))
						  ;; arrow-keys etc
						  (read fn cc 1)
						  
						  ;; do we collide with UTF-8 if we treat this as a meta bit?
						  ;;  that is, if (c 0) is #\c, did user type M-c?
						  
						  (case (c 0)
						    ((#\[)
						     (read fn cc 1)
						     (case (c 0)
						       ((#\A) (move-cursor-up))
						       ((#\B) (move-cursor-down))
						       ((#\C) ((keymap-functions C-f) C-f))
						       ((#\D) ((keymap-functions C-b) C-b))
						       
						       ;;((#\1) 
						       ;; (read fn cc 1) ; random -- good up to F5 anyway?
						       ;; (format *stderr* "F~D?" (- (char->integer (c 0)) (char->integer #\0)))
						       ;; (read fn cc 1)) ; tilde (126)
						       ;; (else (format *stderr* "got ~C" (c 0)))
						       ))
						    
						    ((#\<) (set! cursor-position 0))
						    ((#\>) (set! cursor-position (length current-line)))
						    
						    ((#\c) ; M-c
						     (let ((len (length current-line)))
						       (let loop ((i cursor-position))
							 (if (< i len)
							     (if (char-alphabetic? (current-line i))
								 (begin
								   (set! previous-line (copy current-line))
								   (set! (current-line i) (char-upcase (current-line i)))
								   (set! cursor-position (word-break i)))
								 (loop (+ i 1)))))))

						    ((#\u)
						     (let ((len (length current-line)))
						       (do ((i cursor-position (+ i 1)))
							   ((or (= i len)
								(char-alphabetic? (current-line i)))
							    (if (< i len)
								(do ((k i (+ k 1)))
								    ((or (= k len)
									 (not (char-alphabetic? (current-line k))))
								     (set! cursor-position k))
								  (set! (current-line k) (char-upcase (current-line k)))))))))

						    ((#\l)
						     (let ((len (length current-line)))
						       (do ((i cursor-position (+ i 1)))
							   ((or (= i len)
								(char-alphabetic? (current-line i)))
							    (if (< i len)
								(do ((k i (+ k 1)))
								    ((or (= k len)
									 (not (char-alphabetic? (current-line k))))
								     (set! cursor-position k))
								  (set! (current-line k) (char-downcase (current-line k)))))))))
						    ))))
	      
	      (set! (keymap-functions Tab) (lambda (c)
					     (let ((start (start-of-line cursor-position))
						   (end (end-of-line cursor-position))
						   (completion #f))
					       (if (and (> start 0)
							(= end (+ start 1)))
						   (indent start)
						   (if (= cursor-position end)
						       (let ((loc (do ((i (- end 1) (- i 1)))
								      ((or (< i 0)
									   (char-whitespace? (current-line i))
									   (memv (current-line i) '(#\( #\' #\" #\))))
								       i))))
							 (if (< loc 0)
							     (set! completion (symbol-completion current-line))
							     (if (char=? (current-line loc) #\")
								 (set! completion (filename-completion (substring current-line (+ loc 1))))
								 (set! completion (symbol-completion (substring current-line (+ loc 1))))))
							 (when (and completion
								    (> (length completion) (- end loc 1)))
							   (if (= end (length current-line))
							       (set! current-line (string-append (substring current-line 0 (+ loc 1)) completion))
							       (set! current-line (string-append (substring current-line 0 (+ loc 1)) 
												 completion
												 (substring current-line (+ end 1)))))
							   (set! cursor-position (end-of-line cursor-position)))))))))
	      
	      
	      ;; -------- terminal setup --------
	      ;; we're in libc here, so exit is libc's exit!
	      (define (tty-reset no)
		(tcsetattr fn TCSAFLUSH saved)
		(#_exit))
	      
	      (set! ((rootlet) 'exit) ; we'd like this to happen when user types "(exit)"
		    (lambda ()
		      (newline *stderr*)
		      (tty-reset 0)))
	      
	      (define (run)
		;; check for dumb terminal
		(let ((terminal (getenv "TERM")))
		  (if (string=? terminal "dumb")  ; just read direct -- emacs shell for example
		      (let ((buf (c-pointer->string (calloc 512 1) 512)))
			(format *stderr* "> ")
			(do ((b (fgets buf 512 stdin) (fgets buf 512 stdin)))
			    ((zero? (length b))
			     (#_exit))
			  (let ((len (strlen buf)))
			    (when (positive? len)
			      (do ((i 0 (+ i 1)))
				  ((or (not (char-whitespace? (buf i)))
				       (= i len))
				   (when (< i len)
				     (with-repl-let
				      (catch #t
					(lambda ()
					  (format *stderr* "~S~%" (eval-string (substring buf 0 (- (strlen buf) 1)) (rootlet))))
					(lambda (type info)
					  (format *stderr* "error: ")
					  (apply format *stderr* info)
					  (newline *stderr*))))
				     (format *stderr* "> "))))))))))
		
		;; not a dumb terminal -- hopefully all others accept vt100 codes
		(set! saved (termios.make))

		(if (or (equal? (signal SIGINT tty-reset) SIG_ERR)  ; are these needed?
			(equal? (signal SIGQUIT tty-reset) SIG_ERR)
			(equal? (signal SIGTERM tty-reset) SIG_ERR)
			(negative? (tcgetattr fn saved)))
		    (#_exit))

		(let ((buf (termios.make)))
		  (tcgetattr fn buf)
		  (termios.set_c_lflag buf (logand (termios.c_lflag buf) (lognot (logior ECHO ICANON))))
		  (termios.set_c_cc buf VMIN 1)
		  (termios.set_c_cc buf VTIME 0)
		  (when (negative? (tcsetattr fn TCSAFLUSH buf))
		    (tty-reset fn))
		  
		  (define (one-line text)
		    (if (string? text)
			(let ((ntext (copy text)))
			  (do ((i 0 (+ i 1)))
			      ((= i (length ntext))
			       ntext)
			    (if (char=? (ntext i) #\newline) (set! (ntext i) #\|))))
			text))

		  (define (help)
		    (let ((coords (cursor-coords))
			  (col (floor (/ last-col 2)))
			  (row (floor (/ last-row 2))))
		      (format *stderr* "~C[~D;~DH" #\escape row col)
		      (format *stderr* "+---------------------------------------")
		      (format *stderr* "~C[~D;~DH" #\escape (+ row 1) col)
		      (format *stderr* "~C[K| cursor: ~A, line: ~S" #\escape cursor-position (one-line current-line))
		      (format *stderr* "~C[~D;~DH" #\escape (+ row 2) col)
		      (format *stderr* "~C[K| len: ~D, selection: ~S" #\escape (length current-line) (one-line selection))
		      (format *stderr* "~C[~D;~DH" #\escape (+ row 3) col)
		      (format *stderr* "~C[K| current-row: ~A, prompt-row: ~A" #\escape current-row prompt-row)
		      (format *stderr* "~C[~D;~DH" #\escape (+ row 4) col)
		      (format *stderr* "+---------------------------------------")
		      (format *stderr* "~C[~D;~DH" #\escape (cdr coords) (car coords))))

		  
		  ;; -------- the repl --------
		  (display-prompt)
		  (cursor-bounds)

		  (do ((i (read fn cc 1) (read fn cc 1)))
		      ((not (= i 1))
		       (tty-reset fn))

		    (catch #t
		      (lambda ()

			((keymap-functions (char->integer (c 0))) (c 0))
			(check-parens)
			(display-lines)
			;(help)
			)

		      (lambda (type info)
			(format *stderr* "internal error: ")
			(apply format *stderr* info)
			(newline *stderr*)
			(format *stderr* "line ~A: ~A~%" ((owlet) 'error-line) ((owlet) 'error-code))
			(format *stderr* "current-line: ~A, selection: ~A, cursor-position: ~A, current-row: ~A,~%~
                                          red-par-pos: ~A, prompt-row: ~A, prompt-col: ~A, last-row: ~A, last-col: ~A~%"
				current-line selection cursor-position current-row red-par-pos prompt-row prompt-col last-row last-col)
			(new-prompt))))))
	      
	      (curlet)))))))

      (set! keymap (repl-let 'keymap))
      (set! history (repl-let 'history))
      (set! save-history (repl-let 'save-history))
      (set! restore-history (repl-let 'restore-history))
      (set! prompt (repl-let 'prompt))
      (set! run (repl-let 'run))

      (curlet))))
  
;((*repl* 'run))

#|
to add/change keymap entry:
(set! (*keymap* loc)
      (with-let (*repl* 'repl-let)
	(lambda (c)
	  ;; access here to all internal repl vars and funcs
	  )))

red lambda prompt: 
  (with-let (*repl* 'repl-let)
    (set! prompt-string (bold (red (string #\xce #\xbb #\> #\space))))
    (set! prompt-length 3)) ; until we get unicode length calc
|#

;; need some sort of auto-test (using repl < repl-test.input then check history?)
;; with_main->repl as default not special case
;; user-customizable help section (track variables etc)
;; C-p is > 2 lines doesn't cause n line scroll (just 1) -- true also of 2 lines -- it erases the previous value

