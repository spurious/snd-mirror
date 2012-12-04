;;; make-index.scm translated from index.cl
;;;   run this -noinit so that loads in ~/.snd_s7 don't confuse matters

;(set! (hook-functions *load-hook*) (list (lambda (filename) (format #t "loading ~S~%" filename))))
(set! (hook-functions *unbound-variable-hook*) '())

(define scheme-variable-names
  (let ((h (make-hash-table)))
    (for-each
     (lambda (name)
       (set! (h name) #t))
     (list "after-graph-hook" "lisp-graph-hook" "before-transform-hook" "mix-release-hook" "stop-playing-channel-hook" "save-hook" "mus-error-hook"
	   "mouse-enter-graph-hook" "mouse-leave-graph-hook" "open-raw-sound-hook" "select-channel-hook" "after-open-hook" "close-hook" "drop-hook" "update-hook"
	   "just-sounds-hook" "mark-click-hook" "mark-drag-hook" "name-click-hook" "open-hook" "help-hook"
	   "output-comment-hook" "play-hook" "snd-error-hook" "snd-warning-hook" "start-hook" "start-playing-hook" "stop-playing-hook"
	   "stop-playing-region-hook" "mouse-enter-listener-hook" "mouse-leave-listener-hook" "window-property-changed-hook" "select-sound-hook"
	   "print-hook" "exit-hook" "output-name-hook" "during-open-hook" "transform-hook" "mouse-enter-label-hook" "mouse-leave-label-hook" "initial-graph-hook"
	   "graph-hook" "key-press-hook" "mouse-drag-hook" "mouse-press-hook" "enved-hook" "read-hook" "mouse-click-hook" "new-widget-hook"
	   "mark-hook" "previous-files-select-hook" "dac-hook" "stop-dac-hook" "stop-playing-selection-hook" "after-apply-controls-hook"
	   "draw-mark-hook" "bad-header-hook" "save-state-hook" "new-sound-hook" "color-hook" "orientation-hook" "listener-click-hook"
	   "mix-click-hook" "after-save-state-hook" "mouse-enter-text-hook" "mouse-leave-text-hook" "mix-drag-hook"
	   "start-playing-selection-hook" "selection-changed-hook" "*current-sound*"
	   "before-save-state-hook" "after-save-as-hook" "after-transform-hook" "before-save-as-hook"))
    h))

(define scheme-constant-names
  (let ((h (make-hash-table)))
    (for-each
     (lambda (name)
       (set! (h name) #t))
     (list "mus-out-format" "mus-unsupported" "mus-next" "mus-aifc" "mus-riff" "mus-rf64" "mus-nist" "mus-raw" "mus-ircam" "mus-aiff" "mus-bicsf"
	   "mus-voc" "mus-svx" "mus-soundfont" "mus-unknown" "mus-bshort" "mus-lshort" "mus-mulaw" "mus-alaw" "mus-byte" "mus-ubyte"
	   "mus-bfloat" "mus-lfloat" "mus-bint" "mus-lint" "mus-bintn" "mus-lintn" "mus-b24int" "mus-l24int" "mus-bdouble" "mus-ldouble"
	   "mus-ubshort" "mus-ulshort" "mus-bdouble-unscaled" "mus-ldouble-unscaled" "mus-bfloat-unscaled" "mus-lfloat-unscaled"
	   "mus-audio-default"
	   "rectangular-window" "hann-window" "welch-window"
	   "parzen-window" "bartlett-window" "hamming-window" "blackman2-window" "blackman3-window" "blackman4-window" "exponential-window"
	   "riemann-window" "kaiser-window" "cauchy-window" "poisson-window" "gaussian-window" "tukey-window" "dolph-chebyshev-window"
	   "samaraki-window" "ultraspherical-window" "blackman5-window" "blackman6-window" "blackman7-window" "blackman8-window" 
	   "blackman9-window" "blackman10-window" "rv2-window" "rv3-window" "rv4-window"
	   "zoom-focus-left" "zoom-focus-right" "zoom-focus-active" "zoom-focus-middle" "graph-once"
	   "graph-as-wavogram" "graph-as-sonogram" "graph-as-spectrogram" "cursor-cross" "cursor-line" "graph-lines" "graph-dots"
	   "graph-filled" "graph-dots-and-lines" "graph-lollipops" "x-axis-in-seconds" "x-axis-in-samples" "x-axis-in-beats" "x-axis-in-measures"
	   "x-axis-as-percentage" "show-all-axes" "show-all-axes-unlabelled" "show-no-axes" "show-x-axis" "show-x-axis-unlabelled"
	   "cursor-in-view" "cursor-on-left" "cursor-on-right" "cursor-in-middle" "keyboard-no-action" "fourier-transform"
	   "wavelet-transform" "haar-transform" "cepstrum" "hadamard-transform" "walsh-transform" "autocorrelation" "dont-normalize"
	   "normalize-by-channel" "normalize-by-sound" "normalize-globally" "current-edit-position" "channels-separate"
	   "channels-combined" "channels-superimposed" "speed-control-as-float" "speed-control-as-ratio" "speed-control-as-semitone"
	   "enved-amplitude" "enved-spectrum" "enved-srate" "envelope-linear" "envelope-exponential" "enved-add-point"
	   "enved-delete-point" "enved-move-point" "time-graph" "transform-graph" "lisp-graph" "copy-context" "cursor-context"
	   "selection-context" "mark-context" "mus-interp-all-pass" "mus-interp-bezier" "mus-interp-hermite" "mus-interp-lagrange"
	   "mus-interp-linear" "mus-interp-none" "mus-interp-sinusoidal"
	   "sync-none" "sync-all" "sync-by-sound"
	   ))
    h))



(define (without-dollar-sign str)
  (if (char=? (str 0) #\$)
      (substring str 1)
      str))

(define (creation-date)
  (strftime "%d-%b-%y %H:%M %Z" (localtime (current-time))))

(define (alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c)))
(define-expansion (incf sym) `(set! ,sym (+ ,sym 1)))
(define-expansion (decf sym) `(set! ,sym (- ,sym 1)))
(define-expansion (when test . forms) `(if ,test (begin ,@forms)))
(define-expansion (push val sym) `(set! ,sym (cons ,val ,sym)))
(define-expansion (pop sym) (let ((v (gensym))) `(let ((,v (car ,sym))) (set! ,sym (cdr ,sym)) ,v)))

(define (find-if pred l)
  (cond ((null? l) #f)
	((pred (car l)) (car l))
	(else (find-if pred (cdr l)))))

(define (string-downcase str)
  (let ((len (length str)))
    (do ((i 0 (+ i 1)))
	((= i len) str)
      (set! (str i) (char-downcase (str i))))))

(define* (make-ind name sortby topic file general indexed char)
  (vector name sortby topic file general indexed char))

(define ind-name    (make-procedure-with-setter (lambda (obj) (obj 0)) (lambda (obj val) (set! (obj 0) val))))
(define ind-sortby  (make-procedure-with-setter (lambda (obj) (obj 1)) (lambda (obj val) (set! (obj 1) val))))
(define ind-topic   (make-procedure-with-setter (lambda (obj) (obj 2)) (lambda (obj val) (set! (obj 2) val))))
(define ind-file    (make-procedure-with-setter (lambda (obj) (obj 3)) (lambda (obj val) (set! (obj 3) val))))
(define ind-general (make-procedure-with-setter (lambda (obj) (obj 4)) (lambda (obj val) (set! (obj 4) val))))
(define ind-indexed (make-procedure-with-setter (lambda (obj) (obj 5)) (lambda (obj val) (set! (obj 5) val))))
(define ind-char    (make-procedure-with-setter (lambda (obj) (obj 6)) (lambda (obj val) (set! (obj 6) val))))


(define (write-line line file)
  (format file "~A~%" line))


(define (html-length str)
  (if (char-position #\& str)
      (- (length str) 3)
      (length str)))


(define* (remove item sequence count)
  (let* ((len (length sequence))
	 (num (if (number? count) count len)))
    (if (not (positive? num))
	sequence
	(let ((result '())
	      (changed 0))     
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (if (or (>= changed num)
		    (not (eq? item (sequence i))))
		(set! result (cons (sequence i) result))
		(set! changed (+ changed 1))))
	  (reverse result)))))


(define (count-table commands)
  (let ((count 0))
    (for-each
     (lambda (c)
       (if (memq c '(table TABLE))
	   (set! count (+ count 1))))
     commands)
    count))


(define (string</* a b)
  (and (not (= (length b) 0))
       (or (= (length a) 0)
	   (string=? a b)
	   (string<? (if (char=? (string-ref a 0) #\*) (substring a 1) a)
		     (if (char=? (string-ref b 0) #\*) (substring b 1) b)))))


(define (clean-and-downcase-first-char str caps topic file)
  (if (char=? (str 0) #\|)
      ;; this is a main-index entry
      (let* ((colonpos (or (char-position #\: str) 
			   (format #t "no : in ~A~%" str)))
	     (line (string-append "<a href=\"" 
				  (or file "") 
				  "#"
				  (substring str 1 colonpos) 
				  "\">" 
				  (substring str (+ 1 colonpos)) 
				  "</a>")))
	(make-ind :name line 
		  :topic topic 
		  :file file 
		  :sortby (string-downcase (substring str (+ 1 colonpos)))))

      (begin 
       (let ((def-pos (string-position " class=def" str)))
	 (when def-pos
	       ;(format #t "str: ~S, def-pos: ~A~%" str def-pos)
	   (set! str (string-append "<a "
				    (if (char=? (str (+ def-pos 10)) #\n)
					(substring str (+ def-pos 10))
					(values "name=" (substring str (+ def-pos 14))))))))
       
       (let* ((line (string-append "<a href=\"" 
				   (or file "") 
				   "#" 
				   (substring str 9)))
	      (ipos (string-position "<em" line)))
	 (when ipos
	   (let ((ispos (string-position "</em>" line)))
	     (set! line (string-append (substring line 0 ipos) 
				       (substring line (+ ipos 14) ispos) 
				       (substring line (+ ispos 5))))
	     (if (not line) 
		 (format #t "<em...> but no </em> for ~A~%" str))))
	 
	 (let ((hpos (let ((start (string-position "<h" line)))
		       (and start
			    (or (string-position "<h2>" line start) 
				(string-position "<h1>" line start) 
				(string-position "<h3>" line start) 
				(string-position "<h4>" line start))))))
	   (when hpos
	     (let ((hspos (let ((start (string-position "</h" line)))
			    (and start
				 (or (string-position "</h2>" line start) 
				     (string-position "</h1>" line start) 
				     (string-position "</h3>" line start) 
				     (string-position "</h4>" line start))))))
	       (set! line (string-append (substring line 0 hpos) 
					 (substring line (+ hpos 4) hspos) 
					 (substring line (+ hspos 5))))
	       (if (not line) 
		   (format #t "<hn> but no </hn> for ~A~%" str)))))
	 
	 (letrec ((search-caps ; caps is the list of names with upper case chars from the make-index-1 invocation ("AIFF" for example) 
		   (lambda (ln)
		     (call-with-exit
		      (lambda (return)
			(when caps
			      (for-each
			       (lambda (cap)
				 (when (string-position cap ln)
				       (return #t)))
			       caps)))))))
	   (when (not (search-caps line))
	     ;; now the hard part -- find the first character of the >name< business and downcase it
	     (let ((bpos (char-position #\> line)))
	       (set! (line (+ 1 bpos)) (char-downcase (line (+ 1 bpos)))))))
	 
	 (let ((bpos (char-position #\> line))
	       (epos (or (string-position "</a>" line) 
			 (string-position "</em>" line) 
			 (string-position "</A>" line))))
	   (make-ind :name line 
		     :topic topic 
		     :file file 
		     :sortby (string-downcase (substring line (+ bpos 1) epos))))))))


(define (create-general str file)
  (let ((mid (char-position #\: str)))
    (make-ind :name (string-append "<a href=\"" 
				   (or file "") 
				   "#" 
				   (substring str 0 mid) 
				   "\"><b>" 
				   (substring str (+ mid 1)) 
				   "</b></a>")
	      :topic #f
	      :file file
	      :general #t
	      :sortby (string-downcase (substring str (+ mid 1))))))


(define (scheme->ruby scheme-name)
  (if (string=? scheme-name "frame*")
      "frame_multiply"
      (if (string=? scheme-name "frame+")
	  "frame_add"
	  (if (string=? scheme-name "vct*")
	      "vct_multiply"
	      (if (string=? scheme-name "vct+")
		  "vct_add"
		  (if (string=? scheme-name "mixer*")
		      "mixer_multiply"
		      (if (string=? scheme-name "mixer+")
			  "mixer_add"
			  (if (string=? scheme-name "redo")
			      "redo_edit"
			      (if (string=? scheme-name "in")
				  "call_in"
				  (let* ((len (length scheme-name))
					 (var-case (hash-table-ref scheme-variable-names scheme-name))
					 (strlen (if var-case (+ 1 len) len))
					 (rb-name (make-string strlen #\space))
					 (i 0)
					 (j 0))
				    (if var-case
					(begin
					 (set! (rb-name 0) #\$)
					 (set! j 1))
					(if (hash-table-ref scheme-constant-names scheme-name)
					    (begin
					     (set! (rb-name 0) (char-upcase (scheme-name 0)))
					     (set! i 1)
					     (set! j 1))))
				    (do ()
					((>= i len))
				      (let ((c (scheme-name i)))
					(if (or (alphanumeric? c)
						(char=? c #\?)
						(char=? c #\!))
					    (begin
					     (set! (rb-name j) c)
					     (incf i)
					     (incf j))
					    (if (and (char=? c #\-)
						     (char=? (scheme-name (+ i 1)) #\>))
						(begin
						 (set! (rb-name j) #\2)
						 (incf j)
						 (set! i (+ i 2)))
						(begin
						 (set! (rb-name j) #\_)
						 (incf i)
						 (incf j))))))
				    (if (not (= j strlen))
					(substring rb-name 0 j)
					rb-name)))))))))))

(define (clean-up-xref xref file)
  (let* ((len (length xref))
	 (outstr (make-string (* len 2) #\space)))
    (let ((url-str "")
	  (i 0)
	  (j 0)
	  (need-start #f)
	  (in-bracket #f)
	  (in-href #f)
	  (in-name #f))
      (do ((loc 0))
	  ((>= loc len))
	(let* ((leof (or (char-position #\newline xref loc)
			 len))
	       (href-normal-start (or (string-position "<a href=" xref loc)
				      (string-position "<A HREF=" xref loc)))
	       (href-quiet-start (string-position "<a class=quiet href=" xref loc))
	       (href-def-start (string-position "<a class=def href=" xref loc))
	       (href-start (or href-normal-start href-quiet-start href-def-start))
	       (href-len (if href-normal-start 8 20))
	       (href-end (and href-start
			      (< href-start leof)
			      (char-position #\> xref (+ 1 href-start))))
	       (href (and href-start href-end (substring xref (+ href-start href-len) href-end))))
	  (if href
	      (if (char=? (href 1) #\#)
		  (set! url-str (string-append url-str (string #\") file (substring href 1) (format #f ",~%  ")))
		  (set! url-str (string-append url-str href (format #f ",~%  "))))
	      (set! url-str (string-append url-str (format #f "NULL,~%  "))))
	  (set! loc (+ 1 leof))
	  ))

      (set! (outstr j) #\")
      (incf j)
      (do ()
	  ((>= i len))
	(let ((c (xref i)))
	  (if in-bracket
	      (if (char=? c #\>)
		  (begin
		    (set! in-bracket #f)
		    (if in-href
			(set! in-name #t))
		    (set! in-href #f)))
	      (case c
		((#\<)
		 (if in-name
		     (begin
		       (set! (outstr j) #\})
		       (incf j)
		       (set! in-name #f)))
		 (set! in-bracket #t)
		 (if (or (and (< (+ i 7) len) 
			      (string=? "<a href" (substring xref i (+ i 7))))
			 (and (< (+ i 17) len) 
			      (string=? "<a class=def href" (substring xref i (+ i 17))))
			 (and (< (+ i 19) len) 
			      (string=? "<a class=quiet href" (substring xref i (+ i 19)))))
		     (begin
		       (if need-start
			   (begin
			     (set! (outstr j) #\,)
			     (incf j)
			     (set! (outstr j) #\newline)
			     (incf j)
			     (set! (outstr j) #\space)
			     (incf j)	    
			     (set! (outstr j) #\space)
			     (incf j)	    
			     (set! (outstr j) #\")
			     (incf j)
			     (set! need-start #f)))
		       (set! in-href #t)
		       (set! (outstr j) #\{)
		       (incf j))))
		
		((#\&)
		 (if (and (< (+ i 4) len) 
			  (string=? (substring xref i (+ i 4)) "&gt;"))
		     (begin
		       (set! (outstr j) #\>)
		       (incf j)
		       (set! i (+ i 3))))) ; incf'd again below
		
		((#\newline)
		 (begin
		   (set! (outstr j) #\")
		   (incf j)
		   (set! need-start #t)))
		
		((#\")
		 (begin
		   (set! (outstr j) #\\)
		   (incf j)
		   (set! (outstr j) c)
		   (incf j)))
		
		(else
		 (begin
		   (if need-start
		       (begin
			 (set! (outstr j) #\,)
			 (incf j)
			 (set! (outstr j) #\newline)
			 (incf j)
			 (set! (outstr j) #\space)
			 (incf j)	    
			 (set! (outstr j) #\space)
			 (incf j)	    
			 (set! (outstr j) #\")
			 (incf j)
			 (set! need-start #f)))
		   (set! (outstr j) c)
		   (incf j))))
	      )
	  )
	(incf i))
      (list 
       (substring outstr 0 j)
       url-str))))
  
  
  
;;; --------------------------------------------------------------------------------
;;; get indexer.data

(load "ws.scm")
					;(load "clm23.scm")		   
					;(load "edit123.scm")		   
					;(load "snd7.scm")
(load "snd11.scm")
					;(load "snd6.scm")
					;(load "snd9.scm")
					;(load "snd8.scm")
					;(load "snd10.scm")
(load "analog-filter.scm")	   
					; (load "new-backgrounds.scm")
(load "animals.scm")		   
					; (load "new-effects.scm")
(load "autosave.scm")	   
(load "noise.scm")
(load "binary-io.scm")
					;(load "bess1.scm")		   
(load "nrev.scm")
					;(load "bess.scm")		  
(load "numerics.scm")
(load "big-gens.scm")	  
					;(load "oscope.scm")
(load "bird.scm")		   
(load "clean.scm")	
(load "peak-phases.scm")
(load "piano.scm")
(load "clm-ins.scm")		   
(load "play.scm")
(load "dlocsig.scm")		   
(load "poly.scm")
(load "draw.scm")		  
					;(load "popup.scm")
(load "dsp.scm")		   
(load "prc95.scm")
(load "pretty-print.scm")
					; (load "edit-menu.scm")	   
(load "primes.scm")
					; (load "effects-utils.scm")	   
(load "pvoc.scm")
(load "enved.scm")		   
(load "rgb.scm")
(load "env.scm")		   
;(load "rtio.scm")
(load "examp.scm")		   
(load "rubber.scm")
(load "expandn.scm")		   
					;(load "s7-slib-init.scm")
(load "extensions.scm")	   
					;(load "s7test.scm")
(load "fade.scm")		   
(load "selection.scm")
					; (load "fft-menu.scm")	   
(load "singer.scm")
					; (load "fmv.scm")		   
(load "frame.scm")		   
(load "freeverb.scm")	   
(load "fullmix.scm")		   
(load "generators.scm")	   
(load "grani.scm")		   
					; (load "gtk-effects.scm")	   
(load "snddiff.scm")
					; (load "gtk-effects-utils.scm")  
					; (load "snd-gl.scm")
					; (load "gtk-popup.scm")	   
					; (load "snd-gtk.scm")
(load "hooks.scm")		   
					;(load "sndlib-ws.scm")
(load "index.scm")		   
					; (load "snd-motif.scm")
(load "jcrev.scm")		   
					;(load "snd-test.scm")
(load "jcvoi.scm")		   
(load "sndwarp.scm")

					; (load "kmenu.scm")	

					; (load "special-menu.scm")
(load "maraca.scm")		   
(load "spectr.scm")
					; (load "marks-menu.scm")	   
(load "spokenword.scm")
(load "marks.scm")		   
(load "stochastic.scm")
(load "maxf.scm")		   
(load "strad.scm")
					; (load "misc.scm")		   
(load "mixer.scm")		  
(load "v.scm")
(load "mix.scm")		   
(load "moog.scm")		   
					; (load "xm-enved.scm")
(load "musglyphs.scm")	   
(load "zip.scm")
(load "nb.scm")


(let ()
  (define (report-places)
    (let ((names ())
	  (places ()))
      
      (define (where-is func)
	(let ((addr (with-environment (procedure-environment func) __func__)))
	  ;; this misses scheme-side pws because their environment is (probably) the global env
	  (if (not (pair? addr))
	      #f
	      (cadr addr))))
      
      (define (apropos-1 alist)
	(for-each
	 (lambda (binding)
	   (if (pair? binding)
	       (let ((symbol (car binding))
		     (value (cdr binding)))
		 (if (and (procedure? value) 
			  (not (assq symbol names)))
		     (let ((file (where-is value)))
		       (if (and file
				(not (string=? file "~/.snd_s7"))
				(not (string=? file "/home/bil/.snd_s7"))
				(not (string=? file "t.scm"))
				(not (string=? file "/home/bil/cl/t.scm"))
				(not (string=? file "/home/bil/cl/make-index.scm"))
				)
			   (begin
			     (set! names (cons (cons symbol file) names))
			     (if (not (member file places))
				 (set! places (cons file places))))))))))
	 alist))
      
      ;; handle the main macros by hand
      (for-each
       (lambda (symbol-and-file)
	 (let ((symbol (car symbol-and-file))
	       (file (cadr symbol-and-file)))
	   (if (not (file-exists? file))
	       (format *stderr* ";~S says it is in ~S which does not exist~%" symbol file))
	   (set! names (cons (cons symbol file) names))
	   (if (not (member file places))
	       (set! places (cons file places)))))
       (list 
	(list 'with-sound "ws.scm")
	(list 'with-mixed-sound "ws.scm")
	(list 'with-full-sound "ws.scm")
	(list 'with-temp-sound "ws.scm")
	(list 'with-marked-sound "ws.scm")
	(list 'with-simple-sound "ws.scm")
	(list 'sound-let "ws.scm")
	(list 'definstrument "ws.scm")
	(list 'defgenerator "generators.scm")

	(list 'channel-sync "extensions.scm")
	(list 'cursor-follows-play "snd11.scm")
	(list 'xe-envelope "xm-enved.scm")
	))
      
      ;; and some of the main variables
      (for-each
       (lambda (symbol-and-file)
	 (let ((symbol (car symbol-and-file))
	       (file (cadr symbol-and-file)))
	   (if (not (file-exists? file))
	       (format *stderr* ";~S says it is in ~S which does not exist~%" symbol file))
	   (set! names (cons (cons symbol file) names))
	   (if (not (member file places))
	       (set! places (cons file places)))))
       
       (list
	(list '*clm-srate* "ws.scm")
	(list '*clm-file-name* "ws.scm")
	(list '*clm-channels* "ws.scm")
	(list '*clm-data-format* "ws.scm")
	(list '*clm-header-type* "ws.scm")
	(list '*clm-verbose* "ws.scm")
	(list '*clm-play* "ws.scm")
	(list '*clm-statistics* "ws.scm")
	(list '*clm-reverb* "ws.scm")
	(list '*clm-reverb-channels* "ws.scm")
	(list '*clm-reverb-data* "ws.scm")
	(list '*clm-table-size* "ws.scm")
	(list '*clm-file-buffer-size* "ws.scm")
	(list '*clm-locsig-type* "ws.scm")
	(list '*clm-clipped* "ws.scm")
	(list '*clm-array-print-length* "ws.scm")
	(list '*clm-player* "ws.scm")
	(list '*clm-notehook* "ws.scm")
	(list '*clm-with-sound-depth* "ws.scm")
	(list '*clm-default-frequency* "ws.scm")
;	(list '*clm-safety* "ws.scm")
	(list '*clm-delete-reverb* "ws.scm")
	(list '*clm-output-safety* "ws.scm")
	(list '*to-snd* "ws.scm")
	(list '*clm-search-list* "ws.scm")
	(list '*definstrument-hook* "ws.scm")))
      
      (apropos-1 (reverse (environment->list (global-environment))))

      (let* ((name-len (length names))
	     (nl-1 (- name-len 1))
	     (file-len (length places))
	     (fl-1 (- file-len 1)))
	(do ((i 0 (+ i 1)))
	    ((= i file-len))
	  (let* ((pos -1)
		 (place (list-ref places i))
		 (slen (length place)))
	    (do ((k 0 (+ k 1)))
		((= k slen))
	      (if (char=? (place k) #\/)
		  (set! pos k)))
	    (if (> pos -1)
		(set! (places i) (substring place (+ pos 1))))))
	
	(set! places (sort! places string<?))
	(set! names (sort! names (lambda (a b)
				   (string<? (symbol->string (car a)) 
					     (symbol->string (car b))))))
	
	(call-with-output-file "indexer.data"
	  (lambda (p)
	    
	    (define (find-file name)
	      (call-with-exit
	       (lambda (return)
		 (do ((i 0 (+ i 1)))
		     ((= i file-len) 
		      (format #t "oops! ~A~%" name) 0)
		   (if (string=? name (list-ref places i))
		       (return i))))))
	    
	    (format p "#define AUTOLOAD_FILES ~D~%~%" file-len)
	    (format p "static const char *autoload_files[AUTOLOAD_FILES] = {~%  ")
	    (do ((i 0 (+ i 1)))
		((= i fl-1))
	      (if (and (positive? i)
		       (= (modulo i 6) 0))
		  (format p "~S, ~%  " (places i))
		  (format p "~S, " (places i))))
	    (format p "~S};~%~%" (places fl-1))
	    
	    (format p "#define AUTOLOAD_NAMES ~D~%~%" name-len)
	    (format p "static const char *autoload_names[AUTOLOAD_NAMES] = {~%  ")
	    (do ((i 0 (+ i 1))
		 (cur-name names (cdr cur-name)))
		((= i nl-1))
	      (if (and (positive? i)
		       (= (modulo i 4) 0))
		  (format p "~S, ~%  " (symbol->string (caar cur-name)))
		  (format p "~S, " (symbol->string (caar cur-name)))))
	    (format p "~S};~%~%" (symbol->string (car (names nl-1))))
	    
	    (format p "static int autoload_indices[AUTOLOAD_NAMES] = {~%  ")
	    (do ((i 0 (+ i 1))
		 (cur-name names (cdr cur-name)))
		((= i nl-1))
	      (if (and (positive? i)
		       (= (modulo i 24) 0))
		  (format p "~D, ~%  " (find-file (cdar cur-name)))
		  (format p "~D, " (find-file (cdar cur-name)))))
	    (format p "~D};~%~%" (find-file (cdr (names nl-1)))))))))
  
  (report-places))



;;; --------------------------------------------------------------------------------
;;; make-index 



(define (make-vector-name str)
  (let ((len (length str)))
    (do ((i 0 (+ i 1)))
	((= i len) str)
      (if (char=? (str i) #\space)
	  (set! (str i) #\_)))))

(define ids (make-hash-table))
(define n-array-length 2048)
(define g-array-length 128)

(define* (make-index-1 file-names (output "test.html") (cols 3) (capitalized #f) no-bold with-scm with-clm-locals)
  ;; read html file, gather all names, create index (in lower-case, *=space in sort)
  (let ((n 0)
	(g 0)
	(xrefing #f)
	(current-general 0)
	(got-tr #f)
	(topic #f)
	(xrefs (make-vector g-array-length #f))
	(generals (make-vector g-array-length #f))
	(topics (make-vector n-array-length #f))
	(gfiles (make-vector g-array-length #f))
	(files (make-vector n-array-length #f))
	(names (make-vector n-array-length #f))
	(local-ids '())
	)
    (fill! ids ())
    
    (do ((file file-names (cdr file))
	 (file-ctr 0 (+ file-ctr 1)))
	((null? file))
      (set! local-ids '())
      (call-with-input-file (car file)
        (lambda (f)
	  (let line-loop ((line (read-line f)))
	    (if (not (eof-object? line))
		(let ((len (length line)))
		  (when (and line (positive? len))
		    (let* ((dline line)
			   (compos (string-position "<!-- INDEX" dline))
			   (indpos (string-position "<!-- main-index" dline))
			   (xpos (string-position "<TABLE " dline))
			   (unxpos (string-position "</TABLE>" dline))
			   (pos (and (not compos) 
				     (not indpos)
				     (string-position "<em class=def id=" dline)
				     ))
			   (id-pos (string-position " id=" dline))
			   (tpos (and (not pos) 
				      (string-position "<!-- TOPIC " line))))
		      (if unxpos 
			  (set! xrefing #f))

		      (if id-pos
			  (let* ((start (- (char-position #\" dline id-pos) id-pos))           ; (substring dline id-pos)))
				 (end-start (+ id-pos start 2))
				 (end (- (char-position #\" dline end-start) end-start))       ; (substring dline (+ id-pos start 2))))
				 (name (substring dline (+ id-pos start 1) (+ id-pos start 2 end))))
			    (let ((sym-name (string->symbol name)))
			      (if (not (hash-table-ref ids sym-name))
				  (hash-table-set! ids sym-name 0)
				  (if (memq sym-name local-ids)
				      (format #t "~S: id ~S is set twice~%" file sym-name)))
			      (set! local-ids (cons sym-name local-ids)))))

		      ;; (format #t "~A ~D ~D ~D: id: ~S~%" dline id-pos start end name)))

		      (if tpos
			  (let ((epos (string-position " -->" dline)))
			    (if (not epos) 
				(format #t "<!-- TOPIC but no --> for ~A~%" dline)
				(set! topic (substring dline (+ tpos 11) epos))))
			  (if compos
			      (let ((epos (string-position " -->" dline)))
				(if (not epos) 
				    (format #t "<!-- INDEX but no --> for ~A~%" dline)
				    (when (or (not no-bold)
					      with-scm)
				      (set! current-general g)
				      (set! (generals g) (substring dline (+ compos 11) epos))
				      (set! (gfiles g) (car file))
				      (set! (xrefs g) "")
				      (incf g))))
			      (if indpos
				  (let ((epos (string-position " -->" dline)))
				    (if (not epos) 
					(format #t "<!-- main-index but no --> for ~A~%" dline)
					(when (or (not no-bold)
						  with-scm)
					  (set! (names n) (substring dline (+ indpos 16) epos))
					  (set! (files n) (car file))
					  (incf n))))
				  (if xpos
				      (set! xrefing #t)
				      (do ()
					  ((not pos))
					(set! dline (substring dline pos))
					(let ((epos (or (string-position "</a>" dline) 
							(string-position "</em>" dline) 
							(string-position "</A>" dline))))
					  (if (not epos) 
					      (format #t "<a> but no </a> for ~A~%" dline)
					      (begin
						(set! (names n) (string-append (substring dline 0 epos) "</a>"))
						(set! (files n) (car file))
						(set! (topics n) topic)
						(incf n)
						(set! dline (substring dline (+ epos 4)))
						(set! pos (string-position "<em class=def id=" dline))
						))))))))
		      (if (and xrefing
			       (or (not (char=? (dline 0) #\<))
				   (string-position "<a href" dline)
				   (string-position "<a class=quiet href" dline)
				   (string-position "<a class=def href" dline)))
			  (set! (xrefs current-general) (string-append (xrefs current-general) dline (format #f "~%"))))
		      (when topic
			(let ((hpos (string-position "<hr>" dline)))
			  (when hpos 
			    (set! topic #f))))))
		  (line-loop (read-line f))))))))

      ;; end call-with-input-file loop

    (let ((tnames (make-vector (+ n g)))
	  (ctr 0))
      (do ((i 0 (+ i 1)))
	  ((= i n))
        (set! (tnames ctr)
	      (clean-and-downcase-first-char (names i) capitalized (topics i) (files i)))
	(if (positive? (length (ind-sortby (tnames ctr))))
	    (incf ctr)))

      (when (not (= ctr n))
        (set! n ctr)
	(let ((temp (make-vector n)))
	  (do ((i 0 (+ i 1)))
	      ((= i n))
	    (set! (temp i) (tnames i)))
	  (set! tnames temp)))
      
      (when (positive? g)
	(if (< (length tnames) (+ g n))
	  (let ((temp (make-vector (+ g n) #f)))
	    (do ((i 0 (+ i 1)))
		((= i n))
	      (set! (temp i) (tnames i)))
	    (set! tnames temp)))
	(do ((i 0 (+ i 1)))
	    ((= i g))
	  (set! (tnames (+ i n))
		(create-general (generals i) (gfiles i))))
	(set! n (+ n g)))

      (set! tnames (sort! tnames (lambda (a b)
				   (string</* (ind-sortby a) 
					      (ind-sortby b)))))

      (let ((len (length tnames)))
	(let ((new-names (make-vector (+ len 100)))
	      (j 0)
	      (last-char #f))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (let ((name (tnames i)))
	      (if (and name
		       (ind-sortby name))
		  (let ((this-char ((ind-sortby name) 0)))
		    (if (char-ci=? this-char #\*)
			(set! this-char ((ind-sortby name) 1)))
		    (if (and last-char
			     (not (char-ci=? last-char this-char)))
			(begin
			 (set! (new-names j) (make-ind :name #f :topic #f :file #f :sortby #f))
			 (set! j (+ j 1))
			 (set! (new-names j) (make-ind :name "    " 
						       :char (char-upcase this-char)
						       :topic #f :file #f :sortby #f))
			 (set! j (+ j 1))
			 (set! (new-names j) (make-ind :name #f :topic #f :file #f :sortby #f))
			 (set! j (+ j 1))))
		    (set! (new-names j) name)
		    (set! j (+ j 1))
		    (set! last-char this-char)))))
	  
	    (set! tnames new-names)
	    (set! n j)))
      
      (call-with-output-file output
        (lambda (ofil)
	  (format ofil "<!DOCTYPE html>
<html lang=\"en\">
<head>
<title>Snd Index</title>
<style type=\"text/css\">
	EM.red {color:red; font-style:normal}
        EM.typing {color:maroon; font-style: normal}
        EM.listener {color:darkblue; font-style: normal}
        EM.tab {font-size: small; font-style: normal}
	EM.def {font-weight: bold; font-style: normal}
	H1 {text-align: center}
	UL {list-style-type: none}
        DIV.centered {text-align: center}

	A {text-decoration:none}
	A:hover {text-decoration:underline}
	A.quiet {color:black; text-decoration:none}
	A.quiet:hover {text-decoration:underline}

        TD.green {background-color: lightgreen}
	TD.beige {background-color: beige}
        DIV.topheader {margin-top: 10px;
	            margin-bottom: 40px;
	            border: 4px solid #00ff00; /* green */
		    background-color: #f5f5dc; /* beige */
		    font-family: 'Helvetica';
		    font-size: 30px;
		    text-align: center;
		    padding-top: 10px;
		    padding-bottom: 10px;
	           }
        BODY.body {background-color: #ffffff;    /* white */
	           /* margin-left: 0.5cm; */
                   }
</style>
</head>
<body class=\"body\">
<div class=\"topheader\">Index</div>
<!-- created ~A -->~%" (creation-date))
		      
	  (format ofil "<table>~%  <tr>")
	  (set! got-tr #t)
	  (let ((row 0)
		(ctr 0)
		(offset (ceiling (/ n cols))))
	    (do ((i 0 (+ i 1)))
		((>= row offset))
	      (let ((x (+ row (* ctr offset))))
		(if (< x n)
		    (let ((name (tnames x)))
		      (format ofil 
			      "<td~A>~A~A~A</td>" 
			      (if (not (ind-name name))
				  ""
				  (if (not (ind-sortby name))
				      " class=\"green\""
				      ""))
			      (if (ind-char name)
				  "<div class=\"centered\">"
				  "<em class=tab>")
			      (if (ind-char name)
				  (ind-char name)
				  (if (ind-name name)
				      (ind-name name)
				      "    "))
;			      (if (and (not (ind-char name))
;				       (string? (ind-file name))
;				       (string=? (ind-file name) "s7.html"))
;				  " (s7)"
;				  "")
;; this looks kinda dumb
			      (if (ind-char name)
				  "</div>"
				  "</em>")
			      )
		      (if (ind-indexed name) 
			  (format #t "~A indexed twice~%" (ind-name name)))
		      (set! (ind-indexed name) #t))
		    (format ofil "~%")))

	      (incf ctr)
	      (when (< ctr cols)
	        (format ofil "<td></td>"))
			  
	      (when (= ctr cols)
		(if got-tr (begin (format ofil "</tr>~%") (set! got-tr #f)))
		(incf row)
		(if (< i n) (begin (format ofil "  <tr>") (set! got-tr #t)))
		(set! ctr 0))))
	  (format ofil "~%</table>~%</body></html>~%")))
      ;; end output

      (do ((i 0 (+ i 1)))
	  ((= i n))
	(if (not (ind-indexed (tnames i)))
	    (format #t "unindexed: ~A (~A)~%" (ind-name (tnames i)) i)))

      (do ((i 0 (+ i 1)))
	  ((= i (- n 1)))
	(let ((n1 (tnames i))
	      (n2 (tnames (+ i 1))))
	  (if (and (string? (ind-sortby n1))
		   (string? (ind-sortby n2))
		   (string=? (ind-sortby n1) (ind-sortby n2)))
	      (format #t "duplicated name: ~A (~A ~A)~%" (ind-sortby n1) (ind-name n1) (ind-name n2)))))
      
      (if with-scm
	  (begin
	   (call-with-output-file "test.c"
	     (lambda (sfil)
	       (let ((help-names '())
		     (help-urls '()))
		 (format sfil "/* Snd help index (generated by make-index.scm) */~%")
		 (do ((i 0 (+ i 1)))
		     ((= i n))
		   (if (and (tnames i)
			    (ind-sortby (tnames i)))
		       (let* ((line (substring (ind-name (tnames i)) 8))
			      (dpos (char-position #\> line))
			      (url (substring line 1 (- dpos 1)))
			      (epos (char-position #\< line))
			      (ind (substring line (+ 1 dpos) epos))
			      (gpos (string-position "&gt;" ind)))
			 (if gpos 
			     (set! ind (string-append (substring ind 0 gpos) 
						      ">" 
						      (substring ind (+ gpos 4)))))
			 (when (and ind
				    (string? ind)
				    (positive? (length ind)))
			   (push ind help-names)
			   (push url help-urls)))))

		 (set! help-names (reverse help-names))
		 (set! help-urls (reverse help-urls))

		 (let ((len (length help-names)))
		   (format sfil "#define HELP_NAMES_SIZE ~D~%" len)
		   (format sfil "#if HAVE_SCHEME || HAVE_FORTH~%")
		   (format sfil "static const char *help_names[HELP_NAMES_SIZE] = {~%  ")
		   (format sfil "~S" (car help-names))
		   (do ((ctr 1 (+ ctr 1))
			(lname (cdr help-names) (cdr lname)))
		       ((null? lname))
		     (let ((name (car lname)))
		       (if (= (modulo ctr 6) 0)
			   (format sfil ",~% ~S" name)
			   (format sfil ", ~S" name))))
		   (format sfil "};~%")

		   (format sfil "#endif~%#if HAVE_RUBY~%")
		   (format sfil "static const char *help_names[HELP_NAMES_SIZE] = {~%  ")
		   (format sfil "~S" (car help-names))
		   (do ((ctr 1 (+ ctr 1))
			(lname (cdr help-names) (cdr lname)))
		       ((null? lname))
		     (let ((name (car lname)))
		       (if (= (modulo ctr 6) 0)
			   (format sfil ",~% ~S" (without-dollar-sign (scheme->ruby name)))
			   (format sfil ", ~S" (without-dollar-sign (scheme->ruby name))))))

		   (format sfil "};~%#endif~%")
		   (format sfil "#if (!HAVE_EXTENSION_LANGUAGE)~%static const char **help_names = NULL;~%#endif~%")
		   (format sfil "static const char *help_urls[HELP_NAMES_SIZE] = {~%  ")
		   (format sfil "~S" (car help-names))

		   (do ((ctr 1 (+ ctr 1))
			(lname (cdr help-urls) (cdr lname)))
		       ((null? lname))
		     (let ((url (car lname)))
		       (if (= (modulo ctr 4) 0)
			   (format sfil ",~% ~S" url)
			   (format sfil ", ~S" url))))
		   (format sfil "};~%"))

		 (do ((i 0 (+ i 1)))
		     ((= i g))
		    (if (and (xrefs i)
			     (> (length (xrefs i)) 1))
			(let ((vals (clean-up-xref (xrefs i) (gfiles i))))
			  (format sfil "~%static const char *~A_xrefs[] = {~%  ~A,~%  NULL};~%"
				  (let* ((str (generals i))
					 (mid (char-position #\: str)))
				    (make-vector-name (substring str (+ 1 mid))))
				  (car vals))
			  (format sfil "~%static const char *~A_urls[] = {~%  ~ANULL};~%"
				  (let* ((str (generals i))
					 (mid (char-position #\: str)))
				    (make-vector-name (substring str (+ 1 mid))))
				  (cadr vals))
			  ))
		    )
		 
		 (format sfil "~%~%#if HAVE_SCHEME~%")

		 ;(run-indexer)

		 (call-with-input-file "indexer.data"
		   (lambda (pfil)
		     (let line-loop ((line (read-line pfil)))
		       (if (not (eof-object? line))
			   (begin
			     (write-line line sfil)
			     (line-loop (read-line pfil)))))))
			     
		 (format sfil "#endif~%~%")
		 ))
	     ))))))


;;; --------------------------------------------------------------------------------
;;; html-check

(define array-size (* 4 8192))

;;; (html-check '("sndlib.html" "snd.html" "sndclm.html" "extsnd.html" "grfsnd.html" "sndscm.html" "fm.html" "balance.html" "snd-contents.html" "s7.html"))

(define (html-check files)
  (let ((name 0)
	(href 0)
	(names (make-hash-table 2048)))
    (for-each
     (lambda (file)
       (call-with-input-file file
	 (lambda (f)
	   (let ((linectr -1)
		 (commands ())
		 (comments 0)
		 (openctr 0)
		 (warned #f)
		 (p-parens 0)
		 (p-quotes 0)
		 (p-curlys 0)
		 (in-comment #f)
		 (scripting #f))
	     (let line-loop ((line (read-line f)))
	       (if (not (eof-object? line))
		   (begin
		     (incf linectr)
		     (let ((len (length line)))
		       (when (and line 
				  (positive? len))
			 ;; open/close html entities
			 (do ((i 0 (+ i 1)))
			     ((>= i len))
			   (case (string-ref line i)
			     ((#\<)
			      (if (and (not (zero? openctr))
				       (not (positive? p-quotes)))
				  (if (not in-comment) 
				      (format #t "~A[~D]: ~A has unclosed <?~%" file linectr line)))
			      (incf openctr)
			      (if (and (< i (- len 3))
				       (char=? (line (+ i 1)) #\!)
				       (char=? (line (+ i 2)) #\-)
				       (char=? (line (+ i 3)) #\-))
				  (begin
				    (incf comments)
				    (if (> comments 1)
					(begin 
					  (format #t "~A[~D]: nested <!--?~%" file linectr)
					  (decf comments)))
				    (set! in-comment #t)))
			      (if (and (not in-comment)
				       (< i (- len 1))
				       (char=? (line (+ i 1)) #\space))
				  (format #t "~A[~D]: '< ' in ~A?~%" file linectr line)))
			     
			     ;; else c != <
			     
			     ((#\>)
			      
			      (decf openctr)
			      (if (and (>= i 2)
				       (char=? (line (- i 1)) #\-)
				       (char=? (line (- i 2)) #\-))
				  (begin
				    (set! in-comment #f)
				    (decf comments)
				    (if (< comments 0)
					(begin
					  (format #t "~A[~D]: extra -->?~%" file linectr)
					  (set! comments 0))))
				  (if (and (not (zero? openctr))
					   (not (positive? p-quotes)))
				      (if (not in-comment) 
					  (format #t "~A[~D]: ~A has unmatched >?~%" file linectr line))))
			      (set! openctr 0)
			      (if (and (not in-comment)
				       (>= i 2)
				       (char=? (line (- i 1)) #\-)
				       (not (char=? (line (- i 2)) #\-))
				       (< i (- len 1))
				       (alphanumeric? (line (+ i 1))))
				  (format #t "~A[~D]: untranslated '>': ~A~%" file linectr line)))
			     
			     ;; else c != < or >
			     
			     ((#\&) 
			      (if (and (not in-comment)
				       (not (string-ci=? "&gt;" (substring line i (min len (+ i 4)))))
				       (not (string-ci=? "&lt;" (substring line i (min len (+ i 4)))))
				       (not (string-ci=? "&amp;" (substring line i (min len (+ i 5)))))
				       (not (string-ci=? "&micro;" (substring line i (min len (+ i 7)))))
				       (not (string-ci=? "&quot;" (substring line i (min len (+ i 6)))))
				       (not (string-ci=? "&ouml;" (substring line i (min len (+ i 6)))))
				       (not (string-ci=? "&mdash;" (substring line i (min len (+ i 7)))))
				       (not (string-ci=? "&nbsp;" (substring line i (min len (+ i 6)))))
				       (not (string-ci=? "&&" (substring line i (min len (+ i 2)))))
				       (not (string-ci=? "& " (substring line i (min len (+ i 2)))))) ; following char -- should skip this
				  (format #t "~A[~D]: unknown escape sequence: ~A~%" file linectr line)))
			     
			     ((#\()
			      (incf p-parens))
			     
			     ((#\)) 
			      (decf p-parens))
			     
			     ((#\{) 
			      (incf p-curlys))
			     
			     ((#\})
			      (decf p-curlys))
			     
			     ((#\")
			      (if (or (= i 0)
				      (not (char=? (line (- i 1)) #\\)))
				  (incf p-quotes)))))
			 
			 ;; end line scan
			 (if (not in-comment)
			     (let ((start #f)
				   (closing #f))
			       (do ((i 0 (+ i 1)))
				   ((>= i len))
				 (case (string-ref line i)
				   ((#\<)
				    (if start
					(if (and (not scripting)
						 (not (positive? p-quotes)))
					    (format #t "~A[~D]: nested < ~A~%" file linectr line))
					(set! start i)))
				   ((#\/)
				    (if (and start (= start (- i 1)))
					(set! closing #t)))
				   
				   ((#\!)
				    (if (and start (= start (- i 1)))
					(set! start #f)))
				   
				   ((#\space #\>)
				    (if start
					(begin
					  (if closing
					      (let ((closer (string->symbol (substring line (+ start 2) i))))
						(if (memq closer '(center big font))
						    (format #t "~A[~D]: ~A is obsolete, ~A~%" file linectr closer line)
						    (if (eq? closer 'script)
							(set! scripting #f)
							(if (not scripting)
							    (if (not (memq closer commands))
								(format #t "~A[~D]: ~A without start? ~A from [~D:~D] (commands: ~A)~%" 
									file linectr closer line (+ start 2) i commands)
							    
								(if (memq closer '(ul tr td table TABLE small sub blockquote p
										      a A i b title pre span h1 h2 h3 code body html
										      em head h4 sup map smaller bigger th tbody div))
								    (begin
								      (if (not (eq? (car commands) closer))
									  (format #t "~A[~D]: ~A -> ~A?~%" file linectr closer commands))
								  
								      (if (memq closer '(p td pre))
									  (begin
									    (if (not (even? p-quotes))
										(format #t "~A[~D]: unmatched quote~%" file linectr))
									    (set! p-quotes 0)
									    (if (= p-curlys 1)
										(format #t "~A[~D]: extra '{'~%" file linectr)
										(if (= p-curlys -1)
										    (format #t "~A[~D]: extra '}'~%" file linectr)
										    (if (not (= p-curlys 0))
											(format #t "~A[~D]: curlys: ~D~%" file linectr p-curlys))))
									    (set! p-curlys 0)
									    (if (= p-parens 1)
										(format #t "~A[~D]: extra '('~%" file linectr)
										(if (= p-parens -1)
										    (format #t "~A[~D]: extra ')'~%" file linectr)
										    (if (not (= p-parens 0))
											(format #t "~A[~D]: parens: ~D~%" file linectr p-parens))))
									    (set! p-parens 0)))
								      (set! commands (remove closer commands 1))
								      (if (not warned)
									  (begin
									    (if (and (memq closer '(table TABLE))
										     (not (memq 'table commands))
										     (not (memq 'TABLE commands)))
										(begin
										  (if (memq 'tr commands)
										      (begin
											(set! warned #t)
											(set! commands (remove 'tr commands))
											(format #t "~A[~D]: unclosed tr at table (~A)~%" file linectr commands)))
										  (if (memq 'td commands)
										      (begin
											(set! warned #t)
											(set! commands (remove 'td commands))
											(format #t "~A[~D]: unclosed td at table (~A)~%" file linectr commands))))))))
								(set! commands (remove closer commands)))))))
						(set! closing #f))
						
					      ;; not closing
					      (if (not scripting)
						  (let ((opener (string->symbol (substring line (+ start 1) i))))

						    (if (memq opener '(center big font))
							(format #t "~A[~D]: ~A is obsolete, ~A~%" file linectr opener line)

							(if (eq? opener 'script)
							    (set! scripting #t)

							    (if (eq? opener 'img)
								(let* ((rest-line (substring line (+ start 4)))
								       (alt-pos (string-position "alt=" rest-line))
								       (src-pos (string-position "src=" rest-line)))
								  (if (not alt-pos)
								      (format #t "~A[~D]: img but no alt: ~A~%" file linectr line))
								  (if src-pos
								      (let ((png-pos (string-position ".png" rest-line)))
									(if png-pos
									    (let ((file (substring rest-line (+ src-pos 5) (+ png-pos 4))))
									      (if (not (file-exists? file))
										  (format #t "~A[~D]: src not found: ~S~%" file linectr file)))))))
								
								(if (and (not (memq opener '(br spacer li hr area 
												ul tr td table TABLE small sub blockquote)))
									 (memq opener commands)
									 (= p-quotes 0))
								    (format #t "~A[~D]: nested ~A? ~A from: ~A~%" file linectr opener line commands)
								    (begin
								      (case opener
									((td)
									 (if (not (eq? 'tr (car commands)))
									     (format #t "~A[~D]: td without tr?~%" file linectr))
									 (if (and (not warned)
										  (memq 'td commands)
										  (< (count-table commands) 2))
									     (begin
									       (set! warned #t)
									       (set! commands (remove 'td commands 1))
									       (format #t "~A[~D]: unclosed td at table~%" file linectr))))
									((tr)
									 (if (and (not (memq (car commands) '(table TABLE)))
										  (not (memq (cadr commands) '(table TABLE))))
									     (format #t "~A[~D]: tr without table?~%" file linectr))
									 (if (and (not warned)
										  (memq 'tr commands)
										  (< (count-table commands) 2))
									     (begin
									       (set! warned #t)
									       (set! commands (remove 'tr commands 1))
									       (format #t "~A[~D]: unclosed tr at table~%" file linectr))))
									((p)
									 (if (memq (car commands) '(table TABLE))
									     (format #t "~A[~D]: unclosed table?~%" file linectr)))
									
									((pre br table TABLE hr img ul)
									 (if (memq 'p commands)
									     (format #t "~A[~D]: ~A within <p>?~%" file linectr opener)))
									((li)
									 (if (not (memq 'ul commands))
									     (format #t "~A[~D]: li without ul~%" file linectr)))
									((small)
									 (if (memq (car commands) '(pre code))
									     (format #t "~A[~D]: small shouldn't follow ~A~%" file linectr (car commands))))
									((--)
									 (format #t "~A[~D]: <-- missing !?~%" file linectr)))
								      (if (not (memq opener '(br spacer li hr area)))
									  (set! commands (push opener commands)))))))))))
					  ;; end if closing
					  (set! start #f)))))))
			     ) ; if not in-comment...
					  
			 ;; search for name
			 (let* ((dline line)
				(pos-def1 (string-position "<em class=def id=" dline))
				(pos pos-def1) ;(or pos-def pos-def1))
				(pos-len 18))

			   (do ()
			       ((not pos))
			     (set! dline (substring dline (+ pos pos-len)))
			     (let ((epos (or (string-position "</a>" dline) 
					     (string-position "</em>" dline) 
					     (string-position "</A>" dline))))
			       ;;actually should look for close double quote
			       (if (not epos) 
				   (begin (format #t "~A[~D]: <em...> but no </em> for ~A~%" file linectr dline) (abort))
				   (let ((min-epos (char-position #\space dline)))
				     (set! epos (char-position #\> dline))
				     (if (and (number? min-epos)
					      (< min-epos epos))
					 (set! epos min-epos))

				     (let ((new-name (string-append file "#" (substring dline 0 (- epos 1)))))
				       (if (hash-table-ref names new-name)
					   (format #t "~A[~D]: ambiguous name: ~A~%" file linectr new-name))
				       (hash-table-set! names new-name file))

				     (incf name)
				     (set! dline (substring dline epos))
				     (set! pos-def1 (string-position "<em class=def id=" dline))
				     (set! pos pos-def1)
				     (set! pos-len 18))))))
					  
			 ;; search for href
			 (let* ((dline line)
				(pos (string-position " href=" dline)) ; ignore A HREF
				(pos-len 7))
			   (do ()
			       ((not pos))
			     ;; (format #t "~A dline: ~A~%" pos dline)
			     (if (zero? (length dline)) (exit))
			     (set! dline (substring dline (+ pos pos-len)))
			     (let ((epos (char-position #\> dline)))
			       (if (not epos) 
				   (format #t "~A[~D]: <a href but no </a> for ~A~%" file linectr dline)
				   (let ((cur-href #f))
				     (set! epos (char-position #\" dline 1))
				     (if (char=? (dline 0) #\#)
					 (set! cur-href (string-append file (substring dline 0 epos)))
					 (begin
					   (set! cur-href (substring dline 0 epos))
					   (let ((pos (char-position #\# cur-href)))
					     (if (and (not pos)
						      (> epos 5)
						      (not (string-ci=? (substring cur-href 0 4) "ftp:"))
						      (not (string-ci=? (substring cur-href 0 5) "http:"))
						      (not (file-exists? cur-href)))
						 (format #t "~A[~D]: reference to missing file ~S~%" file linectr cur-href)))))

				     ;; cur-href here is the full link: sndclm.html#make-filetosample for example
				     ;;   it can also be a bare file name
				     (let* ((start (char-position #\# cur-href))
					    (name (and (number? start) 
						       (string->symbol (substring cur-href (+ start 1)))))
					    (data (and (symbol? name) 
						       (hash-table-ref ids name))))
				       (if name 
					   (if (not data)
					       (format #t ";can't find id ~A~%" name)
					       (hash-table-set! ids name (+ data 1)))))
				     
				     (incf href)
				     (set! dline (substring dline epos))
				     (set! pos (string-position " href=" dline))
				     (set! pos-len 7))))))))
		     (line-loop (read-line f)))))
	     (if (not (null? commands)) 
		 (format #t "open directives at end of ~A: ~A~%" file commands))))))
     files)
    ;; end file scan
    
    (format #t "found ~D names and ~D references~%" name href)

    (for-each
     (lambda (data)
       (if (zero? (cdr data))
	   (format #t ";~A unref'd~%" (car data))))
     ids)
    ))


(make-index-1 '("snd.html" "extsnd.html" "grfsnd.html" "sndscm.html" "sndlib.html" "sndclm.html" "fm.html" "quick.html" "s7.html")
	      "test.html" 5 '("AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") #t #t)


(html-check '("sndlib.html" "snd.html" "extsnd.html" "grfsnd.html" "sndclm.html"
	      "sndscm.html" "fm.html" "quick.html" "s7.html"
	      "xen.html" "libxm.html" "index.html"))


(s7-version)
(exit)

