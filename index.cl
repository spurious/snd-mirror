;;; -*- syntax: common-lisp; package: user; base: 10; mode: lisp -*-
;;;
;;; index -- read clm.html (or whatever) and make a column-ized index
;;; help -- ditto but create single-topic help files with an index for lisp's help feature
;;; html-check -- look for dangling hrefs

(require :loop)

;;; (index '("clm.html") t "test.html" 5 '("XmHTML" "AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") nil nil)

;;; (index '("cmn.html") nil "test.html" 3 nil nil nil)

;;; (index '("extsnd.html" "grfsnd.html" "sndscm.html" "sndlib.html" "clm.html") nil "test.html" 5 '("XmHTML" "AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") t t)
;;;   use (make-index)

;;; -------------------------------- index --------------------------------

(defvar names nil)
(defvar files nil)
(defvar gfiles nil)
(defvar generals nil)
(defvar topics nil)

(setf names (make-array 2048 :initial-element nil))
(setf files (make-array 2048 :initial-element nil))
(setf generals (make-array 1024))
(setf gfiles (make-array 1024 :initial-element nil))
(setf topics (make-array 2048 :initial-element nil))

(defstruct ind name sortby topic file general indexed)

(defun html-length (str)
  (if (find #\& str :test #'char=)
      (- (length str) 3)
    (length str)))

(defun string-lessp-but-no-star (a b)	;I want the *clm* asterisks ignored in the alphabetization
  (and (not (= (length b) 0))
       (or (= (length a) 0)
	   (string= a b)
	   (if (char= (char a 0) #\*) 
	       (string-lessp-but-no-star (subseq a 1) b)
	     (if (char= (char b 0) #\*)
		 (string-lessp-but-no-star a (subseq b 1))
	       (if (char= (char a 0) (char b 0))
		   (string-lessp-but-no-star (subseq a 1) (subseq b 1))
		 (char< (char a 0) (char b 0))))))))

(defun clean-and-downcase-first-char (str caps topic file)
  (let* ((line (concatenate 'string "<a href=\"" (or file "") "#" (subseq str 9)))
	 (ipos (search "<em" line)))
    (when ipos
      (let ((ispos (search "</em>" line)))
	(setf line (concatenate 'string (subseq line 0 ipos) (subseq line (+ ipos 14) ispos) (subseq line (+ ispos 5))))
	(if (not line) (warn "<em...> but no </em> for ~A" str))))
    (let ((hpos (or (search "<h2>" line) (search "<h1>" line) (search "<h3>" line) (search "<h4>" line))))
      (when hpos
	(let ((hspos (or (search "</h2>" line) (search "</h1>" line) (search "</h3>" line) (search "</h4>" line))))
	  (setf line (concatenate 'string (subseq line 0 hpos) (subseq line (+ hpos 4) hspos) (subseq line (+ hspos 5))))
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
	  (epos (search "</a>" line)))
      (make-ind :name line :topic topic :file file :sortby (string-downcase (subseq line (1+ bpos) epos))))))

(defun create-general (str file)
  (let* ((mid (search ":" str)))
    (make-ind :name (concatenate 'string "<a href=\"" (or file "") "#" (subseq str 0 mid) "\"><b>" (subseq str (1+ mid)) "</b></a>")
	      :topic nil
	      :file file
	      :general t
	      :sortby (string-downcase (subseq str (1+ mid))))))

(defun scm->rb (scm-name)
  (if (string= scm-name "frame*")
      "frame_multiply"
    (if (string= scm-name "frame+")
	"frame_add"
      (if (string= scm-name "mixer*")
	  "mixer_multiply"
	(if (string= scm-name "redo")
	    "redo_edit"
	  (if (string= scm-name "in")
	      "call_in"
	    (let* ((len (length scm-name))
		   (rb-name (make-string len :initial-element #\space))
		   (i 0)
		   (j 0))
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
	      rb-name)))))))
	   
(defun index (file-names make-help &optional (output "test.html") (cols 3) (capitalized nil) no-bold with-scm)
  ;; read html file, gather all names, create index (in lower-case, *=space in sort)
  (let ((n 0)
	(g 0)
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
		       (pos (and (not compos) (search "<a name=" dline :test #'string=)))
		       (tpos (and (not pos) (search "<!-- TOPIC " line))))
		  (if tpos
		      (let ((epos (search " -->" dline)))
			(if (not epos) 
			    (warn "<!-- TOPIC but no --> for ~A" dline)
			  (setf topic (subseq dline (+ tpos 11) epos))))
		    (if compos
			(let ((epos (search " -->" dline)))
			  (if (not epos) 
			      (warn "<!-- INDEX but no --> for ~A" dline)
			    (when (or (not no-bold)
				      (and with-scm
					   (not (string= "clm.html" file))))
			      (setf (aref generals g) (subseq dline (+ compos 11) epos))
			      (setf (aref gfiles g) file)
			      (incf g))))
		      (loop while pos do
			(setf dline (subseq dline pos))
			(let ((epos (search "</a>" dline)))
			  (if (not epos) 
			      (warn "<a> but no </a> for ~A" dline)
			    (progn
			      (setf (aref names n) (subseq dline 0 (+ epos 4)))
			      (setf (aref files n) file)
			      (setf (aref topics n) topic)
			      (incf n)
			      (setf dline (subseq dline (+ epos 4)))
			      (setf pos (search "<a name=" dline :test #'string=))))))))
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

      (loop for i from 1 below n do
	(if (string= (ind-sortby (aref tnames i)) (ind-sortby (aref tnames (1- i))))
	    (let ((clm-case (if (string= (subseq (ind-name (aref tnames i)) 9 12) "clm")
				i
			      (if (string= (subseq (ind-name (aref tnames (1- i))) 9 12) "clm")
				  (1- i)
				nil))))
	      (format t "duplicates: ~A (~A ~A)~%"
		      (ind-sortby (aref tnames i))
		      (ind-name (aref tnames (1- i)))
		      (ind-name (aref tnames i)))
	      (if clm-case
		  (let ((curname (ind-name (aref tnames clm-case))))
		    (setf (ind-name (aref tnames clm-case))
			  (concatenate 'string
				       (subseq curname 0 (- (length curname) 4))
				       " (clm)</a>")))))))
		    
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
-->
</style>
</head>
<body bgcolor=white>

<table width=\"100%\" border=1><tr><td bgcolor=\"beige\" align=\"center\" valign=\"middle\"><h1><A NAME=\"extsndindex\">Index</h1></td></tr></table>
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

      ;;; fgrep '#define H_open_sound ' *.c --line-number
      (if with-scm
	  (progn
	    (excl:shell "chmod 777 finder.scm")
	    (excl:shell "echo '(defvar all-locs nil) (setf all-locs (list ' > hiho.cl")
	    (dotimes (i n)
	      (if (not (ind-general (aref tnames i)))
		  (let* ((line (subseq (ind-name (aref tnames i)) 8))
			 (dpos (search ">" line))
			 (epos (search "<" line))
			 (ind (subseq line (1+ dpos) epos))
			 (gpos (search "&gt;" ind)))
		    (if gpos (setf ind (concatenate 'string (subseq ind 0 gpos) ">" (subseq ind (+ gpos 4)))))
		    (if (and (not (search "*" ind))
			     (not (search "&" ind)))
			(progn
					;(format t "find ~A..." ind)
			  (format t ".")
			  (if (> (length ind) 0)
			      (excl:shell (format nil "finder.scm ~S hiho.cl" ind))
			    (format t "nothing to find in ~A? " line))
					;(format t "ok~%")
			  )))))
	    (excl:shell "echo '))' >> hiho.cl")
	    (load "hiho.cl")

	    (with-open-file (sfil "test.scm" :direction :output :if-exists :supersede :if-does-not-exist :create)
	      (with-open-file (rfil "test.rb" :direction :output :if-exists :supersede :if-does-not-exist :create)
		(let ((rb-files '()))
		  (let ((dir (directory "/home/bil/dist/snd/")))
		    (loop for fil in dir do
		      (let ((typ (pathname-type fil))
			    (name (namestring fil)))
			(when (string= typ "rb")
			  (push name rb-files)))))
		  (format sfil ";;; Snd documentation index (generated by index.cl)~%~%(set! snd-names-and-urls~%  (list~%")
		  (format rfil "# Snd documentation index (generated by index.cl)~%~%Snd_names_and_urls =~%  [~%")		
		  (dotimes (i n)
		    (if (not (ind-general (aref tnames i)))
			(let* ((line (subseq (ind-name (aref tnames i)) 8))
			       (dpos (search ">" line))
			       (url (subseq line 1 (1- dpos)))
			       (epos (search "<" line))
			       (ind (subseq line (1+ dpos) epos))
			       (gpos (search "&gt;" ind)))
			  (if gpos (setf ind (concatenate 'string (subseq ind 0 gpos) ">" (subseq ind (+ gpos 4)))))
			  (let ((loc (find ind all-locs :key #'car :test #'string-equal)))
			    (when (and ind
				       (stringp ind)
				       (> (length ind) 0))
			      (if loc
				  (progn
				    (format sfil "    (list ~S ~S ~S)~%" ind url (cadr loc))
				    (if (string= (pathname-type (cadr loc)) "scm")
					(let ((name (scm->rb ind))
					      (happy nil))
					  ;; try to find rb name in an rb file
					  (loop for fil in rb-files do
					    (if (= (excl:shell (format nil "fgrep 'def ~A' ~A" name fil)) 0)
						(progn
						  ;;(format t "found ~A in ~A" name (pathname-name fil))
						  (setf happy t)
						  (format rfil "    [~S, ~S, ~S],~%"
							  (scm->rb ind)
							  url
							  (concatenate 'string (pathname-name fil) ".rb"))
						  (loop-finish))))
					  ;;(if (not happy) (format t "can't find ~A" name)
					  )
				      (format rfil "    [~S, ~S, ~S],~%" (scm->rb ind) url (cadr loc))))
				(progn
				  (format sfil "    (list ~S ~S)~%" ind url)
				  (format rfil "    [~S, ~S],~%" (scm->rb ind) url))))))))
	      (format sfil "  ))~%~%")
	      (format rfil "  ]~%~%"))))
	    ))
      (when make-help
	(with-open-file (tofil "clm-help.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (format tofil "(in-package :clm)~%;;; index for help files created by index.cl from clm.html ~%(defvar help-index '( ~%")
	  (dotimes (i n)
	    (if (ind-topic (aref tnames i))
		(format tofil "(~S ~S) " (ind-sortby (aref tnames i)) (ind-topic (aref tnames i)))))
	  (format tofil "))~%"))))))

;;; -------------------------------- Help Topics --------------------------------
;;;
;;; look for <!-- TOPIC name --> in clm.html and mus.lisp
;;; if found, create name.html with header/footer, add clm.html text up to next <hr>
;;; then in small font add mus.lisp text up to ;;; <hr>

(defun help (html-file lisp-file)
  (let ((outf nil)
	(state :reading)
	(topic nil))
    (with-open-file (htf html-file :if-does-not-exist nil)
      (with-open-file (lspf lisp-file)
	(let ((happy t))
	  (loop while happy do
	    (let* ((urline (read-line htf nil :EOF))
		   (line urline))
	      (setf happy (not (eq line :EOF)))
	      (when happy
		(case state
		  (:reading      
		   (let ((compos (search "<!-- TOPIC " line)))
		     (when compos
		       (let ((epos (search " -->" line)))
			 (if (not epos) 
			     (warn "<!-- TOPIC but no --> for ~A" line)
			   (progn
			     (setf topic (subseq line (+ compos 11) epos))
			     (setf state :html)
			     (let ((filename (concatenate 'string #+clm clm::*clm-source-directory* "help/" topic ".html")))
			       (format t "open ~A" filename)
			       (setf outf (open filename :direction :output :if-exists :supersede :if-does-not-exist :create))
			       (format outf "<html>~%<head><title>~A</title></head>~%<body bgcolor=\"#ffffff\">~%" topic))))))))
		  (:html
		   (let* ((compos (search "<hr>" line))
			  (srcpos (search "<!-- TOPIC " line)))
		     (if (not (or compos srcpos))
			 (let ((bpos (search "<a href=" line)))
			   (if bpos 
			       (if (char= #\# (elt line (+ bpos 9)))
				   (format outf "~A~%" (concatenate 'string (subseq urline 0 (+ bpos 9)) "../clm.html#" (subseq urline (+ bpos 10))))
				 (format outf "~A~%" (concatenate 'string (subseq urline 0 (+ bpos 9)) "../" (subseq urline (+ bpos 9)))))
			     (format outf "~A~%" urline)))
		       (progn
			 (setf state :lisp-init)
			 (file-position lspf :start)
			 ;; now search lspf for topic
			 (let ((lappy t)
			       (ltopic nil))
			   (loop while lappy do
			     (let* ((urlspline (read-line lspf nil :EOF))
				    (lspline urlspline))
			       (setf lappy (not (eq lspline :EOF)))
			       (when lappy
				 (case state
				   (:lisp-init
				    (let ((compos (search ";;; <!-- TOPIC " lspline)))
				      (when compos
					(let ((epos (search " -->" lspline)))
					  (if (not epos) 
					      (warn "<!-- TOPIC but no --> for ~A" lspline)
					    (progn
					      (setf ltopic (subseq lspline (+ compos 15) epos))
					      (if (string-equal ltopic topic) 
						  (progn
						    (format t " and include lisp")
						    (format outf "<hr><small><center>Lisp definition of ~A</center><pre>~%" topic)
						    (setf state :lisp)))))))))
				   (:lisp
				    (let ((compos (search ";;; <hr>" lspline)))
				      (if compos
					  (progn
					    (format t " code")
					    (format outf "</pre></small>~%")
					    (setf lappy nil))
					(let ((nline urlspline))
					  ;; look for < and > and & and fixup for HTML's benefit
					  (let ((len (length nline)))
					    (dotimes (k len)
					      (let ((c (elt nline k)))
						(if (char= c #\>) (format outf "&gt;")
						  (if (char= c #\<) (format outf "&lt;")
						    (if (char= c #\&) (format outf "&amp;")
						      (format outf "~C" c))))))
					    (format outf "~%")))))))))))
			 (format t ".~%")
			 (format outf "</body></html>~%")
			 (close outf)
			 (if (not srcpos)
			     (setf state :reading)
			   (let ((epos (search " -->" line)))
			     (if (not epos) 
				 (warn "<!-- TOPIC but no --> for ~A" line)
			       (progn
				 (setf topic (subseq line (+ srcpos 11) epos))
				 (setf state :html)
				 (let ((filename (concatenate 'string #+clm clm::*clm-source-directory* "help/" topic ".html")))
				   (format t "open ~A" filename)
				   (setf outf (open filename :direction :output :if-exists :supersede :if-does-not-exist :create))
				   (format outf "<html>~%<head><title>~A</title></head>~%<body bgcolor=white>~%" topic))))))
			 )))))))))))))
			 
				
			   
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
			  (if (not (= openctr 0))
			      (if (not in-comment) (warn "~A[~D]: ~A has unclosed <?" file linectr line)))
			  (incf openctr)
			  (if (and (< i (- len 3))
				   (char= (elt line (+ i 1)) #\!)
				   (char= (elt line (+ i 2)) #\-)
				   (char= (elt line (+ i 3)) #\-))
			      (setf in-comment t)))
		      (if (char= c #\>)
			  (progn
			    (decf openctr)
			    (if (and (> i 2)
				     (char= (elt line (- i 1)) #\-)
				     (char= (elt line (- i 2)) #\-))
				(setf in-comment nil)
			      (if (not (= openctr 0))
				  (if (not in-comment) (warn "~A[~D]: ~A has unmatched >?" file linectr line))))
			    (setf openctr 0))
			(if (char= c #\&)
			    (if (and (not (string-equal "&gt;" (subseq line i (+ i 4))))
				     (not (string-equal "&lt;" (subseq line i (+ i 4))))
				     (not (string-equal "&amp;" (subseq line i (+ i 5))))
				     (not (string-equal "&micro;" (subseq line i (+ i 7)))))
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
			      (if (not scripting)
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
					  (let ((closer (subseq line (+ start 2) i)))
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
					    (let ((opener (subseq line (+ start 1) i)))
					      (if (string-equal opener "script")
						  (setf scripting t)
						(if (not (member opener (list "br" "spacer" "li" "img" "hr" "area") :test #'string-equal))
						    (if (and (member opener commands :test #'string-equal)
							     (not (member opener (list "ul" "tr" "td" "table" "small" "sub" "blockquote") :test #'string-equal)))
							(warn "nested ~A? ~A from ~A[~D]" opener line file linectr)
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
							(setf commands (push opener commands)))))))))
				      (setf start nil))))))))))
		
		;; search for name
		(let* ((dline line)
		       (pos (search "<a name=" dline :test #'string-equal)))
		  (loop while pos do
		    (setf dline (subseq dline (+ pos 9)))
		    (let ((epos (or (search "</a>" dline)
				    (search "</A>" dline))))
					;actually should look for close double quote
		      (if (not epos) 
			  (warn "<a name but no </a> for ~A in ~A[~D]" dline file linectr)
			(progn
			  (setf epos (search ">" dline))
			  (setf (aref names name) (concatenate 'string file "#" (subseq dline 0 (- epos 1))))
			  (incf name)
			  (setf dline (subseq dline epos))
			  (setf pos (search "<a name=" dline :test #'string-equal)))))))

		;; search for href
		(let* ((dline line)
		       (pos (or (search "<a href=" dline)
				(search "<A HREF=" dline))))
		  (loop while pos do
		    (setf dline (subseq dline (+ pos 9)))
		    (let ((epos (or (search "</a>" dline)
				    (search "</A>" dline))))
		      (if (not epos) 
			  (warn "<a href but no </a> for ~A in ~A[~D]" dline file linectr)
			(progn
			  (setf epos (search "\"" dline :start2 1))
			  (if (char= (elt dline 0) #\#)
			      (setf (aref hrefs href) (concatenate 'string file (subseq dline 0 epos)))
			    (setf (aref hrefs href) (subseq dline 0 epos)))
			  (setf (aref lines href) linectr)
			  (setf (aref refs href) file)
			  (incf href)
			  (setf dline (subseq dline epos))
			  (setf pos (or (search "<a href=" dline)
					(search "<A HREF=" dline))))))))))
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
		"sndscm.html" "fm.html" "balance.html" "snd-contents.html"
		"xen.html" "libxm.html" "cmn.html" "index.html")))

(defun make-index ()
  (check-all)
  (index '("snd.html" "extsnd.html" "grfsnd.html" "sndscm.html" "sndlib.html" "clm.html" "fm.html")
	 nil "test.html" 4 '("XmHTML" "AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") t t))
