;;; -*- syntax: common-lisp; package: user; base: 10; mode: lisp -*-
;;;
;;; index -- read clm.html (or whatever) and make a column-ized index
;;; html-check -- look for dangling hrefs

;;; (index '("clm.html") "test.html" 5 '("XmHTML" "AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") nil nil t)

;;; (index '("cmn.html") "test.html" 4 nil nil nil t)

;;; (index '("extsnd.html" "grfsnd.html" "sndscm.html" "sndlib.html" "clm.html") "test.html" 5 '("XmHTML" "AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") t t)
;;;   use (make-index)

;;; for snd.html table, see snd-index.cl (snd-index "test.html")

#+cmu (declaim (optimize (extensions:inhibit-warnings 3))) 
#+cmu (setf extensions::*gc-verbose* nil)
#+cmu (setf *compile-print* nil)
#+cmu (setf *compile-verbose* nil)

#+sbcl (setf *compile-print* nil)
#+sbcl (setf *compile-verbose* nil)
#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))


;;; -------------------------------- index --------------------------------

(defvar names nil)
(defvar files nil)
(defvar gfiles nil)
(defvar generals nil)
(defvar xrefs nil)
(defvar topics nil)

(setf names (make-array 2048 :initial-element nil))
(setf files (make-array 2048 :initial-element nil))
(setf generals (make-array 1024))
(setf xrefs (make-array 1024))
(setf gfiles (make-array 1024 :initial-element nil))
(setf topics (make-array 2048 :initial-element nil))

(defstruct ind name sortby topic file general indexed)

(defun html-length (str)
  (if (find #\& str :test #'char=)
      (- (length str) 3)
    (length str)))

(defun my-subseq (str start &optional end)
  (let ((len (length str)))
    (if (>= start len)
	""
      (if end
	  (subseq str start (min end len))
	(subseq str start)))))

(defun string-lessp-but-no-star (a b)	;I want the *clm* asterisks ignored in the alphabetization
  (and (not (= (length b) 0))
       (or (= (length a) 0)
	   (string= a b)
	   (if (char= (char a 0) #\*) 
	       (string-lessp-but-no-star (my-subseq a 1) b)
	     (if (char= (char b 0) #\*)
		 (string-lessp-but-no-star a (my-subseq b 1))
	       (if (char= (char a 0) (char b 0))
		   (string-lessp-but-no-star (my-subseq a 1) (my-subseq b 1))
		 (char< (char a 0) (char b 0))))))))

(defun clean-and-downcase-first-char (str caps topic file)

  (if (char= (elt str 0) #\|)
    ;; this is a main-index entry
      (let* ((colonpos (or (search ":" str) (warn "no : in ~A" str)))
	     (line (concatenate 'string "<a href=\"" (or file "") "#" (subseq str 1 colonpos) "\">" (subseq str (1+ colonpos)) "</a>")))
	(make-ind :name line :topic topic :file file :sortby (string-downcase (subseq str (1+ colonpos)))))
     
    (progn 
      (let ((def-pos (search " class=def" str)))
	(when def-pos
	  (setf str (concatenate 'string (my-subseq str 0 def-pos) (my-subseq str (+ def-pos 10))))))
      
      (let* ((line (concatenate 'string "<a href=\"" (or file "") "#" (my-subseq str 9)))
	     (ipos (search "<em" line)))
	(when ipos
	  (let ((ispos (search "</em>" line)))
	    (setf line (concatenate 'string (my-subseq line 0 ipos) (my-subseq line (+ ipos 14) ispos) (my-subseq line (+ ispos 5))))
	    (if (not line) (warn "<em...> but no </em> for ~A" str))))
	(let ((hpos (or (search "<h2>" line) (search "<h1>" line) (search "<h3>" line) (search "<h4>" line))))
	  (when hpos
	    (let ((hspos (or (search "</h2>" line) (search "</h1>" line) (search "</h3>" line) (search "</h4>" line))))
	      (setf line (concatenate 'string (my-subseq line 0 hpos) (my-subseq line (+ hpos 4) hspos) (my-subseq line (+ hspos 5))))
	      (if (not line) (warn "<hn> but no </hn> for ~A" str)))))
	
	(flet ((search-caps (ln)
		 (when caps
		   (loop for cap in caps do
		     (when (search cap ln)
		       (return-from search-caps t))))))
	  (when (not (search-caps line))
	    ;; now the hard part -- find the first character of the >name< business and downcase it
	    (let* ((bpos (search ">" line)))
	      (setf (elt line (1+ bpos)) (char-downcase (elt line (1+ bpos)))))))
	(let ((bpos (search ">" line))
	      (epos (or (search "</a>" line) (search "</A>" line))))
	  (make-ind :name line :topic topic :file file :sortby (string-downcase (my-subseq line (1+ bpos) epos))))))))

(defun create-general (str file)
  (let* ((mid (search ":" str)))
    (make-ind :name (concatenate 'string "<a href=\"" (or file "") "#" (my-subseq str 0 mid) "\"><b>" (my-subseq str (1+ mid)) "</b></a>")
	      :topic nil
	      :file file
	      :general t
	      :sortby (string-downcase (my-subseq str (1+ mid))))))

(defvar scm-variable-names
  (list "after-graph-hook" "lisp-graph-hook" "before-transform-hook" "mix-release-hook" "stop-playing-channel-hook" "save-hook" "mus-error-hook"
	"mouse-enter-graph-hook" "mouse-leave-graph-hook" "open-raw-sound-hook" "select-channel-hook" "after-open-hook" "close-hook" "drop-hook" "update-hook"
	"just-sounds-hook" "mark-click-hook" "mark-drag-hook" "name-click-hook" "open-hook" "help-hook"
	"output-comment-hook" "play-hook" "snd-error-hook" "snd-warning-hook" "start-hook" "start-playing-hook" "stop-playing-hook"
	"stop-playing-region-hook" "mouse-enter-listener-hook" "mouse-leave-listener-hook" "window-property-changed-hook" "select-sound-hook"
	"print-hook" "exit-hook" "output-name-hook" "during-open-hook" "transform-hook" "mouse-enter-label-hook" "mouse-leave-label-hook" "initial-graph-hook"
	"graph-hook" "key-press-hook" "mouse-drag-hook" "mouse-press-hook" "enved-hook" "read-hook" "mouse-click-hook" "new-widget-hook"
	"mark-hook" "previous-files-select-hook" "dac-hook" "stop-dac-hook" "stop-playing-selection-hook" "after-apply-controls-hook"
	"draw-mark-hook" "bad-header-hook" "save-state-hook" "new-sound-hook" "color-hook" "orientation-hook" "listener-click-hook"
	"mix-click-hook" "after-save-state-hook" "mouse-enter-text-hook" "mouse-leave-text-hook" "optimization-hook" "mix-drag-hook"
	"mark-drag-triangle-hook" "start-playing-selection-hook" "recorder-file-hook" "selection-changed-hook" "*current-sound*"
	"before-save-state-hook" "after-save-as-hook" "after-transform-hook" "before-save-as-hook"))

(defvar scm-constant-names
  (list "mus-out-format" "mus-unsupported" "mus-next" "mus-aifc" "mus-riff" "mus-rf64" "mus-nist" "mus-raw" "mus-ircam" "mus-aiff" "mus-bicsf"
	"mus-voc" "mus-svx" "mus-soundfont" "mus-unknown" "mus-bshort" "mus-lshort" "mus-mulaw" "mus-alaw" "mus-byte" "mus-ubyte"
	"mus-bfloat" "mus-lfloat" "mus-bint" "mus-lint" "mus-bintn" "mus-lintn" "mus-b24int" "mus-l24int" "mus-bdouble" "mus-ldouble"
	"mus-ubshort" "mus-ulshort" "mus-bdouble-unscaled" "mus-ldouble-unscaled" "mus-bfloat-unscaled" "mus-lfloat-unscaled"
	"mus-audio-default" "mus-audio-duplex-default" "mus-audio-line-out" "mus-audio-line-in" "mus-audio-microphone"
	"mus-audio-speakers" "mus-audio-dac-out" "mus-audio-adat-in" "mus-audio-aes-in" "mus-audio-digital-in" "mus-audio-digital-out"
	"mus-audio-adat-out" "mus-audio-aes-out" "mus-audio-dac-filter" "mus-audio-mixer" "mus-audio-line1" "mus-audio-line2"
	"mus-audio-line3" "mus-audio-aux-input" "mus-audio-cd" "mus-audio-aux-output" "mus-audio-spdif-in" "mus-audio-spdif-out"
	"mus-audio-direction" "mus-audio-samples-per-channel" "mus-audio-amp" "mus-audio-srate" "mus-audio-channel" "mus-audio-format"
	"mus-audio-port" "mus-audio-imix" "mus-audio-igain" "mus-audio-reclev" "mus-audio-pcm" "mus-audio-pcm2" "mus-audio-ogain"
	"mus-audio-line" "mus-audio-synth" "mus-audio-bass" "mus-audio-treble" "rectangular-window" "hann-window" "welch-window"
	"parzen-window" "bartlett-window" "hamming-window" "blackman2-window" "blackman3-window" "blackman4-window" "exponential-window"
	"riemann-window" "kaiser-window" "cauchy-window" "poisson-window" "gaussian-window" "tukey-window" "dolph-chebyshev-window"
	"samaraki-window" "ultraspherical-window"
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
	"mus-interp-linear" "mus-interp-none" "mus-interp-sinusoidal"))	

(defun scm->rb (scm-name)
  (if (string= scm-name "frame*")
      "frame_multiply"
    (if (string= scm-name "frame+")
	"frame_add"
      (if (string= scm-name "vct*")
	  "vct_multiply"
	(if (string= scm-name "vct+")
	    "vct_add"
	  (if (string= scm-name "mixer*")
	      "mixer_multiply"
	    (if (string= scm-name "mixer+")
		"mixer_add"
	      (if (string= scm-name "redo")
		  "redo_edit"
		(if (string= scm-name "in")
		    "call_in"
		  (let* ((len (length scm-name))
			 (var-case (member scm-name scm-variable-names :test #'string=))
			 (strlen (if var-case (1+ len) len))
			 (rb-name (make-string strlen :initial-element #\space))
			 (i 0)
			 (j 0))
		    (if var-case
			(progn
			  (setf (char rb-name 0) #\$)
			  (setf j 1))
		      (if (member scm-name scm-constant-names :test #'string=)
			  (progn
			    (setf (char rb-name 0) (char-upcase (char scm-name 0)))
			    (setf i 1)
			    (setf j 1))))
		    (do ()
			((>= i len))
		      (let ((c (char scm-name i)))
			(if (or (alphanumericp c)
				(char= c #\?)
				(char= c #\!))
			    (progn
			      (setf (char rb-name j) c)
			      (incf i)
			      (incf j))
			  (if (and (char= c #\-)
				   (char= (char scm-name (+ i 1)) #\>))
			      (progn
				(setf (char rb-name j) #\2)
				(incf j)
				(setf i (+ i 2)))
			    (progn
			      (setf (char rb-name j) #\_)
			      (incf i)
			      (incf j))))))
		    (if (/= j strlen)
			(my-subseq rb-name 0 j)
		      rb-name)))))))))))

(defun clean-up-xref (xref file)
  (let* ((len (length xref))
	 (outstr (make-string (* len 2) :initial-element #\space))
	 (url-str "")
	 (i 0)
	 (j 0)
	 (need-start nil)
	 (in-bracket nil)
	 (in-href nil)
	 (in-name nil))
    
    (let ((loc 0))
      (do ()
	  ((>= loc len))
	(let* ((leof (or (search (string #\newline) xref :start2 loc) len))
	       (href-normal-start (or (search "<a href=" xref :start2 loc)
				      (search "<A HREF=" xref :start2 loc)))
	       (href-quiet-start (search "<a class=quiet href=" xref :start2 loc))
	       (href-def-start (search "<a class=def href=" xref :start2 loc))
	       (href-start (or href-normal-start href-quiet-start href-def-start))
	       (href-len (if href-normal-start 8 20))
	       (href-end (and href-start
			      (< href-start leof)
			      (search ">" xref :start2 (1+ href-start))))
	       (href (and href-start href-end (my-subseq xref (+ href-start href-len) href-end))))
	  (if href
	      (if (char= (char href 1) #\#)
		  (setf url-str (concatenate 'string url-str (string #\") file (my-subseq href 1) (format nil ",~%  ")))
		(setf url-str (concatenate 'string url-str href (format nil ",~%  "))))
	    (setf url-str (concatenate 'string url-str (format nil "NULL,~%  "))))
	  (setf loc (1+ leof))
	  ))
      )
    (setf (char outstr j) #\")
    (incf j)
    (do ()
	((>= i len))
      (let ((c (char xref i)))
	(if in-bracket
	    (if (char= c #\>)
		(progn
		  (setf in-bracket nil)
		  (if in-href
		      (setf in-name t))
		  (setf in-href nil)))
	  (if (char= c #\<)
	      (progn
		(if in-name
		    (progn
		      (setf (char outstr j) #\})
		      (incf j)
		      (setf in-name nil)))
		(setf in-bracket t)
		(if (or (and (< (+ i 7) len) (string= "<a href" (my-subseq xref i (+ i 7))))
			(and (< (+ i 17) len) (string= "<a class=def href" (my-subseq xref i (+ i 17))))
			(and (< (+ i 19) len) (string= "<a class=quiet href" (my-subseq xref i (+ i 19)))))
		    (progn
		      (if need-start
			  (progn
			    (setf (char outstr j) #\,)
			    (incf j)
			    (setf (char outstr j) #\newline)
			    (incf j)
			    (setf (char outstr j) #\space)
			    (incf j)	    
			    (setf (char outstr j) #\space)
			    (incf j)	    
			    (setf (char outstr j) #\")
			    (incf j)
			    (setf need-start nil)))
		      (setf in-href t)
		      (setf (char outstr j) #\{)
		      (incf j))))
	    (if (char= c #\&)
		(if (and (< (+ i 4) len) (string= (my-subseq xref i (+ i 4)) "&gt;"))
		    (progn
		      (setf (char outstr j) #\>)
		      (incf j)
		      (incf i 3))) ; incf'd again below
	      (if (char= c #\newline)
		  (progn
		    (setf (char outstr j) #\")
		    (incf j)
		    (setf need-start t))
		(if (char= c #\")
		    (progn
		      (setf (char outstr j) #\\)
		      (incf j)
		      (setf (char outstr j) c)
		      (incf j))
		  (progn
		    (if need-start
			(progn
			  (setf (char outstr j) #\,)
			  (incf j)
			  (setf (char outstr j) #\newline)
			  (incf j)
			  (setf (char outstr j) #\space)
			  (incf j)	    
			  (setf (char outstr j) #\space)
			  (incf j)	    
			  (setf (char outstr j) #\")
			  (incf j)
			  (setf need-start nil)))
		    (setf (char outstr j) c)
		    (incf j))))))))
      (incf i))
    (list 
     (my-subseq outstr 0 j)
     url-str)))

(defun make-array-name (str)
  (do ((i 0 (1+ i)))
      ((= i (length str)) str)
    (if (char= (char str i) #\space)
	(setf (char str i) #\_))))
	   
(defun index (file-names &optional (output "test.html") (cols 3) (capitalized nil) no-bold with-scm with-clm-locals)
  ;; read html file, gather all names, create index (in lower-case, *=space in sort)
  (let ((n 0)
	(g 0)
	(xrefing nil)
	(current-general 0)
	(got-tr nil)
	(topic nil))
    (dotimes (i 2048)
      (setf (aref names i) nil)
      (setf (aref files i) nil)
      (setf (aref topics i) nil))
    (dotimes (i 1024)
      (setf (aref gfiles i) nil)
      (setf (aref generals i) nil))
    (loop for file in file-names and file-ctr from 0 do
      (with-open-file (f file :if-does-not-exist nil)
	(let ((line t))
	  (loop while line do
	    (setf line (read-line f nil nil)) ;nil upon EOF with no error msg
	    (let ((len (length line)))
	      (when (and line (plusp len))
		(let* ((dline line)
		       (compos (search "<!-- INDEX" dline))
		       (indpos (search "<!-- main-index" dline))
		       (xpos (search "<TABLE " dline))
		       (unxpos (search "</TABLE>" dline))
		       (pos-simple (and (not compos) (not indpos)
					(or (search "<a name=" dline :test #'string=)
					    (and with-clm-locals (search "<a Name=" dline :test #'string=)))))
		       (pos-def (and (not compos) (not indpos)
				     (or (search "<a class=def name=" dline :test #'string=)
					 (and with-clm-locals (search "<a class=def Name=" dline :test #'string=)))))
		       (pos (or pos-simple pos-def))
		       (tpos (and (not pos) (search "<!-- TOPIC " line))))
		  (if unxpos (setf xrefing nil))
		  (if tpos
		      (let ((epos (search " -->" dline)))
			(if (not epos) 
			    (warn "<!-- TOPIC but no --> for ~A" dline)
			  (setf topic (my-subseq dline (+ tpos 11) epos))))
		    (if compos
			(let ((epos (search " -->" dline)))
			  (if (not epos) 
			      (warn "<!-- INDEX but no --> for ~A" dline)
			    (when (or (not no-bold)
				      (and with-scm
					   (not (string= "clm.html" file))))
			      (setf current-general g)
			      (setf (aref generals g) (my-subseq dline (+ compos 11) epos))
			      (setf (aref gfiles g) file)
			      (setf (aref xrefs g) "")
			      (incf g))))
		      (if indpos
			  (let ((epos (search " -->" dline)))
			    (if (not epos) 
				(warn "<!-- main-index but no --> for ~A" dline)
			      (when (or (not no-bold)
					(and with-scm
					     (not (string= "clm.html" file))))
				(setf (aref names n) (my-subseq dline (+ indpos 16) epos))
				(setf (aref files n) file)
				(incf n))))
			(if xpos
			    (setf xrefing t)
			  (loop while pos do
			    (setf dline (my-subseq dline pos))
			    (let ((epos (or (search "</a>" dline) (search "</A>" dline))))
			      (if (not epos) 
				  (warn "<a> but no </a> for ~A" dline)
				(progn
				  (setf (aref names n) (my-subseq dline 0 (+ epos 4)))
				  (setf (aref files n) file)
				  (setf (aref topics n) topic)
				  (incf n)
				  (setf dline (my-subseq dline (+ epos 4)))
				  (setf pos-simple (or (search "<a name=" dline :test #'string=)
						       (and with-clm-locals (search "<a Name=" dline :test #'string=))))
				  (setf pos-def (or (search "<a class=def name=" dline :test #'string=)
						    (and with-clm-locals (search "<a class=def Name=" dline :test #'string=))))
				  (setf pos (or pos-simple pos-def))
				  ))))))))
		  (if (and xrefing
			   (or (not (char= (elt dline 0) #\<))
			       (search "<a href" dline)
			       (search "<a class=quiet href" dline)
			       (search "<a class=def href" dline)))
		      (setf (aref xrefs current-general) (concatenate 'string (aref xrefs current-general) dline (format nil "~%"))))
		  (when topic
		    (let ((hpos (search "<hr>" dline)))
		      (when hpos (setf topic nil)))))))))))
    (let ((tnames (make-array (+ n g) :adjustable t))
	  (ctr 0))
      (dotimes (i n)
	(setf (aref tnames ctr)
	      (clean-and-downcase-first-char (aref names i) capitalized (aref topics i) (aref files i)))	
	(if (or (> (length (ind-sortby (aref tnames ctr))) 0)
		(find-if (lambda (n) (char/= n #\space)) (ind-sortby (aref tnames ctr))))
	    (incf ctr)))
      (when (/= ctr n)
	(setf n ctr)
	(adjust-array tnames n))

      (when (> g 0)
	(if (< (length tnames) (+ g n)) (adjust-array tnames (+ g n)))
	(dotimes (i g)
	  (setf (aref tnames (+ i n))
		(create-general (aref generals i) (aref gfiles i))))
	(incf n g))
      (setf tnames (sort tnames #'string-lessp-but-no-star :key #'ind-sortby))

;      (loop for i from 1 below n do
;	(if (string= (ind-sortby (aref tnames i)) (ind-sortby (aref tnames (1- i))))
;	    (let ((clm-case (if (string= (my-subseq (ind-name (aref tnames i)) 9 12) "clm")
;				i
;			      (if (string= (my-subseq (ind-name (aref tnames (1- i))) 9 12) "clm")
;				  (1- i)
;				nil))))
;	      (format t "duplicates: ~A (~A ~A)~%"
;		      (ind-sortby (aref tnames i))
;		      (ind-name (aref tnames (1- i)))
;		      (ind-name (aref tnames i)))
;	      (if clm-case
;		  (let ((curname (ind-name (aref tnames clm-case))))
;		    (setf (ind-name (aref tnames clm-case))
;			  (concatenate 'string
;				       (my-subseq curname 0 (- (length curname) 4))
;				       " (clm)</a>")))))))
		    
      (with-open-file (ofil output :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofil "<html>
<head>
<title>Snd Index</title>
<style type=\"text/css\">
<!-- 
	EM.red {color:red; font-style:normal}
        EM.typing {color:maroon; font-style: normal}
        EM.listener {color:darkblue; font-style: normal}
        EM.tab {font-size: small; font-style: normal}
	EM.def {font-weight: bold; font-style: normal}
	H1 {text-align: center}
	UL {list-style-type: none}

	A {text-decoration:none}
	A:hover {text-decoration:underline}
	A.quiet {color:black; text-decoration:none}
	A.quiet:hover {text-decoration:underline}
-->
</style>
</head>
<body bgcolor=white>

<table width=\"100%\" border=0><tr><td bgcolor=\"beige\" align=\"center\" valign=\"middle\"><h1>Index</h1></td></tr></table>
<br>
<!-- created ~A -->~%"
		    #+clm (clm::c-strftime "%a %d-%b-%y %H:%M %Z") #-clm "whenever")
	(format ofil "<table cellspacing=0 cellpadding=1>~%  <tr>")
	(setf got-tr t)
	(let ((row 0)
	      (ctr 0)
	      (offset (ceiling n cols)))
	  (do ((i 0 (1+ i)))
	      ((>= row offset))
	    (let ((x (+ row (* ctr offset))))
	      (if (< x n)
		  (progn
		    (format ofil "<td><em class=tab>~A</em></td>" (ind-name (aref tnames x)))
		    (if (ind-indexed (aref tnames x)) (format t "~A indexed twice~%" (ind-name (aref tnames x))))
		    (setf (ind-indexed (aref tnames x)) t))
		(format ofil "~%")))
	    (incf ctr)
	    (when (= ctr cols)
	      (if got-tr (progn (format ofil "</tr>~%") (setf got-tr nil)))
	      (incf row)
	      (if (< i n) (progn (format ofil "  <tr>") (setf got-tr t)))
	      (setf ctr 0))))
	(format ofil "~%</table>~%</body></html>~%"))
      (dotimes (i n)
	(if (not (ind-indexed (aref tnames i)))
	    (format t "unindexed: ~A (~A)~%" (ind-name (aref tnames i)) i)))

      (if with-scm
	  (progn
	    (with-open-file (sfil "test.c" :direction :output :if-exists :supersede :if-does-not-exist :create)
	      (let ((help-names '())
		    (help-urls '()))
		(format sfil "/* Snd help index (generated by index.cl) */~%")
		(dotimes (i n)
		  (let* ((line (my-subseq (ind-name (aref tnames i)) 8))
			 (dpos (search ">" line))
			 (url (my-subseq line 1 (1- dpos)))
			 (epos (search "<" line))
			 (ind (my-subseq line (1+ dpos) epos))
			 (gpos (search "&gt;" ind)))
		    (if gpos (setf ind (concatenate 'string (my-subseq ind 0 gpos) ">" (my-subseq ind (+ gpos 4)))))
		    (when (and ind
			       (stringp ind)
			       (> (length ind) 0))
		      (push ind help-names)
		      (push url help-urls))))
		(setf help-names (reverse help-names))
		(setf help-urls (reverse help-urls))
		(let ((len (length help-names)))
		  (format sfil "#define HELP_NAMES_SIZE ~D~%" len)
		  (format sfil "#if HAVE_SCHEME || HAVE_FORTH~%")
		  (format sfil "static char *help_names[HELP_NAMES_SIZE] = {~%  ")
		  (format sfil "~S" (car help-names))
		  (loop for ctr from 1 and name in (cdr help-names) do
		    (format sfil ",~%  ~S" name))
		  (format sfil "};~%")
		  (format sfil "#endif~%#if HAVE_RUBY~%")
		  (format sfil "static char *help_names[HELP_NAMES_SIZE] = {~%  ")
		  (format sfil "~S" (car help-names))
		  (loop for ctr from 1 and name in (cdr help-names) do
		    (format sfil ",~%  ~S" (scm->rb name)))
		  (format sfil "};~%#endif~%")
		  (format sfil "#if (!HAVE_EXTENSION_LANGUAGE)~%static char **help_names = NULL;~%#endif~%")
		  (format sfil "static char *help_urls[HELP_NAMES_SIZE] = {~%  ")
		  (format sfil "~S" (car help-names))
		  (loop for ctr from 1 and url in (cdr help-urls) do
		    (format sfil ",~%  ~S" url))
		  (format sfil "};~%"))
		(dotimes (i g)
		  (if (and (aref xrefs i)
			   (> (length (aref xrefs i)) 1))
		      (let ((vals (clean-up-xref (aref xrefs i) (aref gfiles i))))
			(format sfil "~%static char *~A_xrefs[] = {~%  ~A,~%  NULL};~%"
				(let* ((str (aref generals i))
				       (mid (search ":" str)))
				  (make-array-name (my-subseq str (1+ mid))))
				(car vals))
			(format sfil "~%static char *~A_urls[] = {~%  ~ANULL};~%"
				(let* ((str (aref generals i))
				       (mid (search ":" str)))
				  (make-array-name (my-subseq str (1+ mid))))
				(cadr vals))
			      ))
		  )))
	    )))))
				
			   
;;; --------------------------------------------------------------------------------
;;; html-check looks for dangling references etc

(defvar array-size 8192)

;;; (html-check '("sndlib.html" "snd.html" "clm.html" "extsnd.html" "grfsnd.html" "sndscm.html" "fm.html" "balance.html" "snd-contents.html"))

(defun html-check (files)
  (let ((name 0)
	(href 0)
	(names (make-array array-size :initial-element nil))
	(hrefs (make-array array-size :initial-element nil))
	(refs (make-array array-size :initial-element nil))
	(lines (make-array array-size :initial-element nil))
	(commands nil)
	;(tds (make-array 128 :initial-element 0))
	)
    (loop for file in files do
      (with-open-file (f file :if-does-not-exist nil)
	(let ((line t)
	      (linectr 0)
	      (comments 0)
	      (openctr 0)
	      (warned nil)
	      (p-parens 0)
	      (p-quotes 0)
	      (p-curlys 0)
	      (in-comment nil)
	      (scripting nil))
	  ;;(if (> (length commands) 0) (warn "commands before ~A: ~A" file commands))
	  (loop while line do
	    (setf line (read-line f nil nil)) ;nil upon EOF with no error msg
	    (let ((len (length line)))
	      (when (and line (plusp len))

		;; open/close html entities
		;(let ((c (count "td" commands :test #'string-equal)))
		;  (setf (aref tds (1+ c)) linectr))
		(loop for i from 0 below len do
		  (let ((c (elt line i)))
		    (if (char= c #\<)
			(progn
			  (if (and (not (= openctr 0))
				   (not (> p-quotes 0)))
			      (if (not in-comment) (warn "~A[~D]: ~A has unclosed <?" file linectr line)))
			  (incf openctr)
			  (if (and (< i (- len 3))
				   (char= (elt line (+ i 1)) #\!)
				   (char= (elt line (+ i 2)) #\-)
				   (char= (elt line (+ i 3)) #\-))
			      (progn
				(incf comments)
				(if (> comments 1)
				    (progn 
				      (warn "~A[~D]: nested <!--?" file linectr)
				      (decf comments)))
				(setf in-comment t))))
		      (if (char= c #\>)
			  (progn
			    (decf openctr)
			    (if (and (>= i 2)
				     (char= (elt line (- i 1)) #\-)
				     (char= (elt line (- i 2)) #\-))
				(progn
				  (setf in-comment nil)
				  (decf comments)
				  (if (< comments 0)
				      (progn
					(warn "~A[~D]: extra -->?" file linectr)
					(setf comments 0))))
			      (if (and (not (= openctr 0))
				       (not (> p-quotes 0)))
				  (if (not in-comment) (warn "~A[~D]: ~A has unmatched >?" file linectr line))))
			    (setf openctr 0))
			(if (char= c #\&)
			    (if (and (not (string-equal "&gt;" (my-subseq line i (+ i 4))))
				     (not (string-equal "&lt;" (my-subseq line i (+ i 4))))
				     (not (string-equal "&amp;" (my-subseq line i (+ i 5))))
				     (not (string-equal "&micro;" (my-subseq line i (+ i 7))))
				     (not (string-equal "&quot;" (my-subseq line i (+ i 6))))
				     (not (string-equal "&ouml;" (my-subseq line i (+ i 6))))
				     (not (string-equal "&&" (my-subseq line i (+ i 2))))
				     (not (string-equal "& " (my-subseq line i (+ i 2))))) ; following char -- should skip this
				(warn "~A[~D]: unknown escape sequence: ~A" file linectr line))
			  (if (char= c #\() (incf p-parens)
			    (if (char= c #\)) (decf p-parens)
			      (if (char= c #\{) (incf p-curlys)
				(if (char= c #\}) (decf p-curlys)
				  (if (and (char= c #\")
					   (or (= i 0)
					       (not (char= (elt line (- i 1)) #\\))))
				      (incf p-quotes))
			    )))))))))
		
		(let ((start nil)
		      (closing nil))
		  (loop for i from 0 below len do
		    (let ((c (elt line i)))
		      (if (char= c #\<)
			  (if start
			      (if (and (not scripting)
				       (not (> p-quotes 0)))
				  (warn "nested < ~A from ~A[~D]" line file linectr))
			    (setf start i))
			(if (char= c #\/)
			    (if (and start (= start (1- i)))
				(setf closing t))
			  (if (char= c #\!)
			      (if (and start (= start (1- i)))
				  (setf start nil))
			    (if (or (char= c #\space)
				    (char= c #\>))
				(if start
				    (progn
				      (if closing
					  (let ((closer (my-subseq line (+ start 2) i)))
					    (if (string-equal closer "script")
						(setf scripting nil)
					      (if (not scripting)
						  (if (not (member closer commands :test #'string-equal))
						      (warn "~A without start? ~A from ~A[~D][~D:~D]" closer line file linectr (+ start 2) i)
						    (if (member closer
								(list "ul" "tr" "td" "table" "small" "sub" "blockquote" "center" "p"
								      "a" "i" "b" "title" "pre" "span" "h1" "h2" "h3" "code" "body" "html"
								      "em" "head" "h4" "sup" "font" "map" "smaller" "th")
								:test #'string-equal)
							(progn

							  (if (not (string-equal (car commands)
										 closer))
							      (format t "~A -> ~A: ~A ~A?~%" closer commands file linectr))

							  (if (or (string-equal closer "p")
								  (string-equal closer "td")
								  (string-equal closer "pre"))
							      (progn
								(if (not (evenp p-quotes))
								    (format t "unmatched quote: ~A ~A~%" file linectr))
								(setf p-quotes 0)
								(if (= p-curlys 1)
								    (format t "extra '{': ~A ~A~%" file linectr)
								  (if (= p-curlys -1)
								      (format t "extra '}': ~A ~A~%" file linectr)
								    (if (/= p-curlys 0)
									(format t "curlys: ~D ~A ~A~%" p-curlys file linectr))))
								(setf p-curlys 0)
								(if (= p-parens 1)
								    (format t "extra '(': ~A ~A~%" file linectr)
								  (if (= p-parens -1)
								      (format t "extra ')': ~A ~A~%" file linectr)
								    (if (/= p-parens 0)
									(format t "parens: ~D ~A ~A~%" p-parens file linectr))))
								(setf p-parens 0)))
							  (setf commands (remove closer commands :test #'string-equal :count 1))
							  (if (not warned)
							      (progn
								(if (and (string-equal closer "table")
									 (not (member "table" commands :test #'string-equal)))
								    (progn
								      (if (member "tr" commands :test #'string-equal)
									  (progn
									    (setf warned t)
									    (remove "tr" commands :test #'string-equal)
									    (format t "unclosed tr at table ~A ~A~%" file linectr)))
								      (if (member "td" commands :test #'string-equal)
									  (progn
									    (setf warned t)
									    (remove "td" commands :test #'string-equal)
									    (format t "unclosed td at table ~A ~A~%" file linectr))))))))
						      (setf commands (remove closer commands :test #'string-equal))))))
					    (setf closing nil))
					(if (not scripting)
					    (let ((opener (my-subseq line (+ start 1) i)))
					      (if (string-equal opener "script")
						  (setf scripting t)
						(if (not (member opener (list "br" "spacer" "li" "img" "hr" "area") :test #'string-equal))
						    (if (and (member opener commands :test #'string-equal)
							     (= p-quotes 0)
							     (not (member opener (list "ul" "tr" "td" "table" "small" "sub" "blockquote") :test #'string-equal)))
							(warn "nested ~A? ~A from ~A[~D]: ~A" opener line file linectr commands)
						      (progn
							(if (not warned)
							    (progn
							      (if (and (string-equal opener "tr")
								       (member "tr" commands :test #'string-equal)
								       (< (count "table" commands :test #'string-equal) 2))
								  (progn
								    (setf warned t)
								    (remove "tr" commands :test #'string-equal :count 1)
								    (format t "unclosed tr at table ~A ~A~%" file linectr)))
							      (if (and (string-equal opener "td")
								       (member "td" commands :test #'string-equal)
								       (< (count "table" commands :test #'string-equal) 2))
								  (progn
								    (setf warned t)
								    (remove "td" commands :test #'string-equal :count 1)
								    (format t "unclosed td at table ~A ~A~%" file linectr)))))
							(if (string-equal opener "--")
							    (format t "~A[~D]: <-- missing !?~%" file linectr))
							(setf commands (push opener commands)))))))))
				      (setf start nil))))))))))
		
		;; search for name
		(let* ((dline line)
		       (pos-simple (search "<a name=" dline :test #'string-equal))
		       (pos-def (search "<a class=def name=" dline :test #'string-equal))
		       (pos (or pos-simple pos-def))
		       (pos-len (if pos-simple 9 19))
		       )
		  (loop while pos do
		    (setf dline (my-subseq dline (+ pos pos-len)))
		    (let ((epos (or (search "</a>" dline) (search "</A>" dline))))
					;actually should look for close double quote
		      (if (not epos) 
			  (warn "<a name but no </a> for ~A in ~A[~D]" dline file linectr)
			(progn
			  (setf epos (search ">" dline))
			  (setf (aref names name) (concatenate 'string file "#" (my-subseq dline 0 (- epos 1))))
			  (loop for i from 0 below name do
			    (if (string= (aref names i) (aref names name))
				(format t "ambiguous name: ~A (~A[~D])~%" (aref names i) file linectr)))
			  (incf name)
			  (setf dline (my-subseq dline epos))
			  (setf pos-simple (search "<a name=" dline :test #'string-equal))
			  (setf pos-def (search "<a class=def name=" dline :test #'string-equal))
			  (setf pos (or pos-simple pos-def))
			  (setf pos-len (if pos-simple 9 19))
			  )))))

		;; search for href
		(let* ((dline line)
		       (pos-norm (search "<a href=" dline)) ; ignore A HREF
		       (pos-quiet (search "<a class=quiet href=" dline))
		       (pos-def (search "<a class=def href=" dline))
		       (pos (or pos-norm pos-quiet pos-def))
		       (pos-len (if pos-norm 9 (if pos-def 19 21))))
		  (loop while pos do
		    (setf dline (my-subseq dline (+ pos pos-len)))
		    (let ((epos (or (search "</a>" dline) (search "</A>" dline))))
		      (if (not epos) 
			  (warn "<a href but no </a> for ~A in ~A[~D]" dline file linectr)
			(progn
			  (setf epos (search "\"" dline :start2 1))
			  (if (char= (elt dline 0) #\#)
			      (setf (aref hrefs href) (concatenate 'string file (my-subseq dline 0 epos)))
			    (progn
			      (setf (aref hrefs href) (my-subseq dline 0 epos))
			      (let ((pos (search "#" (aref hrefs href))))
				(if (and (not pos)
					 (not (string-equal (my-subseq (aref hrefs href) 0 4) "ftp:"))
					 (not (string-equal (my-subseq (aref hrefs href) 0 5) "http:"))
					 (not (probe-file (aref hrefs href))))
				    (format t "reference to missing file ~S in ~A[~D]~%"
					    (aref hrefs href)
					    file linectr)))
			      ))
			  (setf (aref lines href) linectr)
			  (setf (aref refs href) file)
			  (incf href)
			  (setf dline (my-subseq dline epos))
			  (setf pos-norm (search "<a href=" dline))
			  (setf pos-quiet (search "<a class=quiet href=" dline))
			  (setf pos-def (search "<a class=def href=" dline))
			  (setf pos (or pos-norm pos-quiet pos-def))
			  (setf pos-len (if pos-norm 9 (if pos-def 19 21)))
			  )))))))
	    (incf linectr)))
	(if commands (format t "open directives at end of ~A: ~A~%" file commands))
	(setf commands nil)
	))
    
    ;(format t "tds: ~A" tds)
    
    (format t "found ~D names and ~D references~%" name href)
    (loop for h from 0 below href do
      (if (and (not (find (aref hrefs h) names :test #'string=))
	       (search "#" (aref hrefs h)))
	  (format t "undef'd: ~A (~A: ~A)~%" (aref hrefs h) (aref refs h) (aref lines h))))
    (loop for h from 0 below name do
      (if (not (find (aref names h) hrefs :test #'string=))
	  (format t "unref'd: ~A~%" (aref names h))))
    (list names hrefs)))

(defun check-all ()
  (html-check '("sndlib.html" "snd.html" "clm.html" "extsnd.html" "grfsnd.html"
		"sndscm.html" "fm.html" "balance.html" "quick.html"
		"xen.html" "libxm.html" "cmn.html" "index.html")))

(defun make-index ()
  (check-all)
  (index '("snd.html" "extsnd.html" "grfsnd.html" "sndscm.html" "sndlib.html" "clm.html" "fm.html" "quick.html")
	 "test.html" 5 '("XmHTML" "AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") t t))


