;;; index snd.html
(require :loop)

(defstruct tp name m mh o oh c ch l lh nh)

(defun snd-index (output)
  (let* ((index-size 2048)
	 (names (make-array index-size :initial-element nil))
	 (namesh (make-array index-size :initial-element nil))
	 (menu (make-array index-size :initial-element nil))
	 (other (make-array index-size :initial-element nil))
	 (lisp (make-array index-size :initial-element nil))
	 (cxcx (make-array index-size :initial-element nil))
	 (menuh (make-array index-size :initial-element nil))
	 (otherh (make-array index-size :initial-element nil))
	 (lisph (make-array index-size :initial-element nil))
	 (cxcxh(make-array index-size :initial-element nil))
	 (topic-ctr -1)
	 (topic -1)
    	 (snd-all-names (make-array (* 2 index-size) :initial-element nil))
	 (ext-all-names (make-array (* 2 index-size) :initial-element nil))
	 (grf-all-names (make-array (* 2 index-size) :initial-element nil))
	 (scm-all-names (make-array (* 2 index-size) :initial-element nil))
	 (clm-all-names (make-array (* 2 index-size) :initial-element nil))
	 (snd-name-ctr 0)
	 (ext-name-ctr 0)
	 (grf-name-ctr 0)
	 (scm-name-ctr 0)
	 (clm-name-ctr 0))

  (flet ((addext (name)
	   (if (find name ext-all-names :test #'string=)
	       (format nil "extsnd.html#~A" name)
	     (if (find name grf-all-names :test #'string=)
		 (format nil "grfsnd.html#~A" name)
	       (if (find name scm-all-names :test #'string=)
		   (format nil "sndscm.html#~A" name)
		 (if (find name clm-all-names :test #'string=)
		     (format nil "clm.html#~A" name)
		   (format nil "#~A" name)))))))
  
    (with-open-file (sndf "snd.html")
      (let ((happy t))
	(loop while happy do
	  (let* ((line (read-line sndf nil :EOF)))
	    (setf happy (not (eq line :EOF)))
	    (when happy
	      (let* ((simple-pos (or (search "<a name=" line) (search "<A NAME=" line)))
		     (fancy-pos (or (search "<a class=def name=" line) (search "<A class=def NAME=" line)))
		     (pos (or simple-pos fancy-pos))
		     (pos-len (if simple-pos 9 19)))
		(if pos
		    (let* ((epos (search ">" (subseq line pos)))
			   (name (subseq line (+ pos pos-len) (+ pos epos -1))))
		      (if (find name snd-all-names :test #'string=)
			  (print (format nil "multiple definitions of ~A?" name))
			(progn
			  (setf (aref snd-all-names snd-name-ctr) name)
			  (incf snd-name-ctr)))))))))))

    (print (format nil "found ~D names in snd.html" snd-name-ctr))
	   
    (with-open-file (sndf "extsnd.html")
      (let ((happy t))
	(loop while happy do
	  (let* ((line (read-line sndf nil :EOF)))
	    (setf happy (not (eq line :EOF)))
	    (when happy
	      (let* ((simple-pos (or (search "<a name=" line) (search "<A NAME=" line)))
		     (fancy-pos (or (search "<a class=def name=" line) (search "<A class=def NAME=" line)))
		     (pos (or simple-pos fancy-pos))
		     (pos-len (if simple-pos 9 19)))
		(if pos
		    (let* ((epos (search ">" (subseq line pos)))
			   (name (subseq line (+ pos pos-len) (+ pos epos -1))))
		      (if (or (find name ext-all-names :test #'string=)
			      (find name snd-all-names :test #'string=))
			  (print (format nil "multiple definitions of ~A?" name))
			(progn
			  (setf (aref ext-all-names ext-name-ctr) name)
			  (incf ext-name-ctr)))))))))))
    (print (format nil "found ~D names in extsnd.html" ext-name-ctr))
    
    (with-open-file (sndf "grfsnd.html")
      (let ((happy t))
	(loop while happy do
	  (let* ((line (read-line sndf nil :EOF)))
	    (setf happy (not (eq line :EOF)))
	    (when happy
	      (let* ((simple-pos (or (search "<a name=" line) (search "<A NAME=" line)))
		     (fancy-pos (or (search "<a class=def name=" line) (search "<A class=def NAME=" line)))
		     (pos (or simple-pos fancy-pos))
		     (pos-len (if simple-pos 9 19)))
		(if pos
		    (let* ((epos (search ">" (subseq line pos)))
			   (name (subseq line (+ pos pos-len) (+ pos epos -1))))
		      (if (or (find name grf-all-names :test #'string=)
			      (find name ext-all-names :test #'string=)
			      (find name snd-all-names :test #'string=))
			  (print (format nil "multiple definitions of ~A?" name))
			(progn
			  (setf (aref grf-all-names grf-name-ctr) name)
			  (incf grf-name-ctr)))))))))))
    (print (format nil "found ~D names in grfsnd.html" grf-name-ctr))

    (with-open-file (sndf "sndscm.html")
      (let ((happy t))
	(loop while happy do
	  (let* ((line (read-line sndf nil :EOF)))
	    (setf happy (not (eq line :EOF)))
	    (when happy
	      (let* ((simple-pos (or (search "<a name=" line) (search "<A NAME=" line)))
		     (fancy-pos (or (search "<a class=def name=" line) (search "<A class=def NAME=" line)))
		     (pos (or simple-pos fancy-pos))
		     (pos-len (if simple-pos 9 19)))
		(if pos
		    (let* ((epos (search ">" (subseq line pos)))
			   (name (subseq line (+ pos pos-len) (+ pos epos -1))))
		      (if (or (find name scm-all-names :test #'string=)
			      (find name grf-all-names :test #'string=)
			      (find name ext-all-names :test #'string=)
			      (find name snd-all-names :test #'string=))
			  (print (format nil "multiple definitions of ~A?" name))
			(progn
			  (setf (aref scm-all-names scm-name-ctr) name)
			  (incf scm-name-ctr)))))))))))
    (print (format nil "found ~D names in sndscm.html" scm-name-ctr))

    (with-open-file (sndf "clm.html")
      (let ((happy t))
	(loop while happy do
	  (let* ((line (read-line sndf nil :EOF)))
	    (setf happy (not (eq line :EOF)))
	    (when happy
	      (let* ((simple-pos (or (search "<a name=" line) (search "<A NAME=" line)))
		     (fancy-pos (or (search "<a class=def name=" line) (search "<A class=def NAME=" line)))
		     (pos (or simple-pos fancy-pos))
		     (pos-len (if simple-pos 9 19)))
		(if pos
		    (let* ((epos (search ">" (subseq line pos)))
			   (name (subseq line (+ pos pos-len) (+ pos epos -1))))
		      (if (or (find name clm-all-names :test #'string=)
			      (find name scm-all-names :test #'string=)
			      (find name grf-all-names :test #'string=)
			      (find name ext-all-names :test #'string=)
			      (find name snd-all-names :test #'string=))
			  (print (format nil "multiple definitions of ~A?" name))
			(progn
			  (setf (aref clm-all-names clm-name-ctr) name)
			  (incf clm-name-ctr)))))))))))
    (print (format nil "found ~D names in clm.html" clm-name-ctr))
    ;;; got names, now find index data

    (loop for file in '("snd.html" "extsnd.html" "grfsnd.html" "sndscm.html") do
    (with-open-file (sndf file)
      (let ((happy t))
	(loop while happy do
	  (let* ((urline (read-line sndf nil :EOF))
		 (line urline))
	    (setf happy (not (eq urline :EOF)))
	    (when happy
	      ;; search for <!-- I(...
	      (loop while line do
		(let ((pos (search "<!-- I(" line)))
		  (if pos
		      (let* ((epos (search "-->" line))
			     (nline (subseq line (+ pos 7) epos)))
			(setf line (subseq line (+ epos 3)))
			(let* ((paren (search "):" nline))
			       (curname (subseq nline 0 paren))
			       (tpos (position curname names :test #'(lambda (a b) (and a b (string-equal a b))))))
			  (if tpos 
			      (setf topic tpos)
			    (progn
			      (incf topic-ctr)
			      (setf topic topic-ctr)
			      (setf (aref names topic) curname)))
			  (setf nline (subseq nline (+ paren 2)))
			  (let ((type (elt nline 0)))
			    (setf paren (search ")" nline))
			    (let ((info (subseq nline 2 paren)))
			      (if (char= type #\M)
				  (progn
				    (if (aref menu topic) (warn "~A menu ~A overwrites ~A~%" (aref names topic) info (aref menu topic)))
				    (setf (aref menu topic) info))
				(if (char= type #\L)
				    (progn
				      (if (aref lisp topic) (warn "~A lisp ~A overwrites ~A~%" (aref names topic) info (aref lisp topic)))
				      (setf (aref lisp topic) info))
				  (if (char= type #\O)
				      (progn
					(if (aref other topic) (warn "~A other ~A overwrites ~A~%" (aref names topic) info (aref other topic)))
					(setf (aref other topic) info))
				    (if (char= type #\K)
					(progn
					  (if (aref cxcx topic) (warn "~A cxcx ~A overwrites ~A~%" (aref names topic) info (aref cxcx topic)))
					  (setf (aref cxcx topic) info))
				      (if (char= type #\A)
					  (progn
					    (if (aref namesh topic) (warn "~A namesh ~A overwrites ~A~%" (aref names topic) info (aref namesh topic)))
					    (setf (aref namesh topic) info))
					(error "unknown type: ~A in ~A from ~A" type nline urline)))))))
			    (setf nline (subseq nline (+ paren 1)))
			    (setf paren (search ")" nline))
			    (if paren
				(if (char= type #\M)
				    (progn
				      (setf (aref menuh topic) (subseq nline 1 paren)))
				  (if (char= type #\L)
				      (progn
					(setf (aref lisph topic) (subseq nline 1 paren)))
				    (if (char= type #\O)
					(progn
					  (setf (aref otherh topic) (subseq nline 1 paren)))
				      (if (char= type #\K)
					  (progn
					    (setf (aref cxcxh topic) (subseq nline 1 paren)))))))))))
		    (setf line nil))))))))))

    (let ((tparr (make-array (1+ topic-ctr) :element-type 'tp)))
      (loop for i from 0 to topic-ctr do
	(if (and (aref namesh i)
		 (not (find (aref namesh i) snd-all-names :test #'string=))
		 (not (find (aref namesh i) ext-all-names :test #'string=))
		 (not (find (aref namesh i) grf-all-names :test #'string=))
		 (not (find (aref namesh i) clm-all-names :test #'string=))
		 (not (find (aref namesh i) scm-all-names :test #'string=)))
	    (print (format nil "no definition of ~A (~A)?" (aref namesh i) (aref names i))))
	(if (and (aref menuh i)
		 (not (find (aref menuh i) snd-all-names :test #'string=))
		 (not (find (aref menuh i) ext-all-names :test #'string=))
		 (not (find (aref menuh i) grf-all-names :test #'string=))
		 (not (find (aref menuh i) clm-all-names :test #'string=))
		 (not (find (aref menuh i) scm-all-names :test #'string=)))
	    (print (format nil "no definition of ~A (~A)?" (aref menuh i) (aref menu i))))
	(if (and (aref otherh i)
		 (not (find (aref otherh i) snd-all-names :test #'string=))
		 (not (find (aref otherh i) ext-all-names :test #'string=))
		 (not (find (aref otherh i) grf-all-names :test #'string=))
		 (not (find (aref otherh i) clm-all-names :test #'string=))
		 (not (find (aref otherh i) scm-all-names :test #'string=)))
	    (print (format nil "no definition of ~A (~A)?" (aref otherh i) (aref other i))))
	(if (and (aref lisph i)
		 (not (find (aref lisph i) snd-all-names :test #'string=))
		 (not (find (aref lisph i) ext-all-names :test #'string=))
		 (not (find (aref lisph i) grf-all-names :test #'string=))
		 (not (find (aref lisph i) clm-all-names :test #'string=))
		 (not (find (aref lisph i) scm-all-names :test #'string=)))
	    (print (format nil "no definition of ~A (~A)?" (aref lisph i) (aref lisp i))))
	(if (and (aref cxcxh i)
		 (not (find (aref cxcxh i) snd-all-names :test #'string=))
		 (not (find (aref cxcxh i) ext-all-names :test #'string=))
		 (not (find (aref cxcxh i) grf-all-names :test #'string=))
		 (not (find (aref cxcxh i) clm-all-names :test #'string=))
		 (not (find (aref cxcxh i) scm-all-names :test #'string=)))
	    (print (format nil "no definition of ~A (~A)?" (aref cxcxh i) (aref cxcx i))))
	(setf (aref tparr i) (make-tp :name (aref names i) :nh (aref namesh i)
				      :m (aref menu i) :mh (aref menuh i)
				      :o (aref other i) :oh (aref otherh i)
				      :l (aref lisp i) :lh (aref lisph i)
				      :c (aref cxcx i) :ch (aref cxcxh i))))
      (setf tparr (sort tparr #'string-lessp :key #'tp-name))
      (with-open-file (ofil output :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofil "<html><body>~%~%<table border rows=~D>~%  ~
                                         <caption>Index</caption>~%  ~
                                         <!-- created ~A -->~%  ~
                                         <tr><th>Action</th><th>Menu</th><th>Keyboard</th><th>Lisp</th><th>Other</th></tr>~%"
		(1+ topic-ctr)
		#+clm (clm::c-strftime "%a %d-%b-%y %H:%M %Z") #-clm "whenever")
	(loop for i from 0 to topic-ctr do
	  (let ((top (aref tparr i)))
	    (format ofil "  <tr><td>~A</td><td>~A</td><td>~A</td><td>~A</td><td>~A</td></tr>~%"
		    (if (tp-nh top)
			(format nil "<a href=\"~A\">~A</a>" (addext (tp-nh top)) (tp-name top))
		      (tp-name top))
		    (let ((pos (and (tp-mh top) (search ":" (tp-m top)))))
		      (if pos
			  (format nil "<small>~A: <a href=\"~A\">~A</a></small>" 
				  (subseq (tp-m top) 0 pos) (addext (tp-mh top)) (subseq (tp-m top) (+ pos 2)))
			(if (tp-mh top)
			    (format nil "<a href=\"~A\"><small>~A</small></a>" (addext (tp-mh top)) (tp-m top))
			  (if (tp-m top) 
			      (format nil "<small>~A</small>" (tp-m top))
			    "<br>"))))
		    (let* ((str (tp-c top))
			   (str1 (make-string (length str)))
			   (j 0))
		      (dotimes (i (length str))
			(if (or (not (string= "-" (elt str i)))
				(= i 0))
			    (progn
			      (setf (elt str1 j) (elt str i))
			      (incf j))))
		      (if (tp-ch top)
			  (format nil "<a href=\"~A\"><small>~A</small></a>" (addext (tp-ch top)) (subseq str1 0 j))
			(if (tp-c top)
			    (format nil "<small>~A</small>" (subseq str1 0 j))
			  "<br>")))
		    (if (tp-lh top)
			(format nil "<a href=\"~A\"><small>~A</small></a>" (addext (tp-lh top)) (tp-l top))
		      (if (tp-l top)
			  (format nil "<small>~A</small>" (tp-l top))
			"<br>"))
		    (let* ((str (tp-o top))
			   (str1 (make-string (length str)))
			   (j 0))
		      (dotimes (i (length str))
			(if (or (not (string= "-" (elt str i)))
				(= i 0)
				(not (string= "C" (elt str (1- i)))))
			    (progn
			      (setf (elt str1 j) (elt str i))
			      (incf j))))
		      (if (tp-oh top)
			  (format nil "<a href=\"~A\"><small>~A</small></a>" (addext (tp-oh top)) (subseq str1 0 j))
			(if (tp-o top)
			    (format nil "<small>~A</small>" (subseq str1 0 j))
			  "<br>"))))))
	(format ofil "</table>~%~%</body></html>~%"))))))
		    

