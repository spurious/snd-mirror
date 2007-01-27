#!/usr/bin/guile -s
!#

;;; makegl.scm creates the GL/GLU bindings using gldata.scm, writes gl.c

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(use-modules (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(define gl-file (open-output-file "gl.c"))

(define (hey . args)
  (display (apply format #f args) gl-file))

(define (heyc arg)
  (display arg gl-file))

(define names '())
(define types '())
(define ints '())
(define funcs '())

(define x-types '())
(define x-funcs '())
(define x-ints '())
(define g-types '())
(define g-funcs '())
(define g-ints '())
(define g5-funcs '())
(define g5-ints '())

(define in-glu #f)

(define (check-glu name)
  (if in-glu
      (if (not (string=? "glu" (substring name 0 3)))
	  (begin
	    (set! in-glu #f)
	    (hey "#endif~%")))
      (if (string=? "glu" (substring name 0 3))
	  (begin
	    (set! in-glu #t)
	    (hey "#if HAVE_GLU~%")))))

(define (uncheck-glu)
  (if in-glu
      (begin
	(hey "#endif~%")
	(set! in-glu #f))))

(define (cadr-str data)
  (let ((sp1 -1)
	(len (string-length data)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) (substring data sp1))
	 (if (char=? (string-ref data i) #\space)
	     (if (= sp1 -1)
		 (set! sp1 i)
		 (return (substring data (1+ sp1) i)))))))))

(define (caddr-str data)
  (let ((sp1 -1)
	(sp2 -1)
	(len (string-length data)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) (substring data sp2))
	 (if (char=? (string-ref data i) #\space)
	     (if (= sp1 -1)
		 (set! sp1 i)
		 (if (= sp2 -1)
		     (set! sp2 i)
		     (return (substring data (1+ sp2)))))))))))

(define (car-str data)
  (let ((len (string-length data)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) data)
	 (if (char=? (string-ref data i) #\space)
	     (return (substring data 0 i))))))))

(define (cdr-str data)
  (let ((len (string-length data)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) data)
	 (if (char=? (string-ref data i) #\space)
	     (return (substring data (1+ i)))))))))

(define (string-upcase name)
  (let* ((len (string-length name))
	 (str (make-string len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (string-set! str i (char-upcase (string-ref name i))))
    str))

(define (ref-arg? arg)
  (and (= (length arg) 3)
       (string? (caddr arg))))

(define (null-arg? arg)
  (and (= (length arg) 3)
       (eq? (caddr arg) 'null)))

(define (opt-arg? arg)
  (and (= (length arg) 3)
       (eq? (caddr arg) 'opt)))

(define (ref-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (ref-arg? arg)
	   (set! ctr (1+ ctr))))
     args)
    ctr))

(define (opt-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (opt-arg? arg)
	   (set! ctr (1+ ctr))))
     args)
    ctr))

(define (deref-type arg)
  (let ((type (car arg)))
    (substring type 0 (1- (string-length type)))))

(define (deref-name arg)
  (let* ((name (cadr arg)))
    (string-append "ref_" name)))

(define (derefable type)
  (let ((len (string-length type)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i (1- len) (1- i))
	    (ctr 0 (1+ ctr)))
	   ((= i 0) #f)
	 (if (not (char=? (string-ref type i) #\*))
	     (return (> ctr 1))))))))

(define (has-stars type)
  (let ((len (string-length type)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i (1- len) (1- i))
	    (ctr 0 (1+ ctr)))
	   ((= i 0) #f)
	 (if (char=? (string-ref type i) #\*)
	     (return #t)))
       #f))))

(define (no-stars type)
  (if (string=? type "Display*")
      "Display"
      (if (string=? type "XVisualInfo*")
	  "XVisualInfo"
	  (let ((len (string-length type))
		(val (string-copy type)))
	    (do ((i 0 (1+ i)))
		((= i len) val)
	      (if (char=? (string-ref val i) #\*)
		  (string-set! val i #\_)))))))

(define (no-arg-or-stars name)
  (let ((len (string-length name)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) name)
	 (if (or (char=? (string-ref name i) #\()
		 (char=? (string-ref name i) #\*))
	     (return (substring name 0 i))))))))

(define* (parse-args args #:optional x)
  (let ((data '())
	(sp -1)
	(type #f)
	(len (string-length args)))
    (if (string=? args "void")
	'()
	(do ((i 0 (1+ i)))
	    ((= i len) (reverse data))
	  (let ((ch (string-ref args i)))
	    (if (or (char=? ch #\space)
		    (= i (1- len)))
		(begin
		  (if type
		      (let* ((given-name (substring args (1+ sp) (if (= i (1- len)) (1+ i) i)))
			     (reftype #f))
			(if (char=? (string-ref given-name 0) #\@)
			    (set! data (cons (list type 
						   (substring given-name 1 (string-length given-name))
						   'null)
					       data))
			    (if (char=? (string-ref given-name 0) #\#)
				(set! data (cons (list type 
						       (substring given-name 1 (string-length given-name))
						       'opt)
						 data))
				(if (char=? (string-ref given-name 0) #\[) 
				    (begin
				      (set! reftype (deref-type (list type)))
				      (set! data (cons (list type 
							     (substring given-name 1 (- (string-length given-name) 1))
							     given-name) 
						       data)))
				    (set! data (cons (list type given-name) data)))))
			(if reftype (set! type reftype))
			(if (eq? x 'x)
			    (if (not (member type x-types))
				(set! x-types (cons type x-types)))
			    (if (eq? x 'g)
				(if (not (member type g-types))
				    (set! g-types (cons type g-types)))
				(if (not (member type types))
				    (set! types (cons type types)))))
			(set! type #f))
		      (if (> i (1+ sp))
			  (set! type (substring args (1+ sp) i))))
		  (set! sp i))))))))

(define (helpify name type args)
  (let* ((initial (format #f "  #define H_~A \"~A ~A(" name type name))
	 (line-len (string-length initial))
	 (len (string-length args))
	 (typed #f)
	 (help-max 100))
    (hey initial)
    (do ((i 0 (1+ i)))
	((= i len))
      (let ((ch (string-ref args i)))
	(if (char=? ch #\space)
	    (if typed
		(begin
		  (heyc ", ")
		  (set! line-len (+ line-len 2))
		  (if (> line-len help-max)
		      (begin
			(hey "\\~%")
			(set! line-len 0)))
		  (set! typed #f))
		(begin
		  (set! line-len (1+ line-len))
		  (heyc " ")
		  (set! typed #t)))
	    (if (and (not (char=? ch #\@))
		     (not (char=? ch #\#)))
		(begin
		  (set! line-len (1+ line-len))
		  (heyc ch))))))
    (hey ")\"~%")))

(define direct-types 
  (list (cons "void" #f)
	(cons "GLvoid" #f)
	(cons "int" "INT")
	(cons "GLint" "INT")
	(cons "GLsizei" "INT")
	(cons "GLenum" "INT")
	(cons "GLfloat" "DOUBLE")
	(cons "GLclampf" "DOUBLE")
	(cons "GLdouble" "DOUBLE")
	(cons "GLclampd" "DOUBLE")
	(cons "double" "DOUBLE")
	(cons "char" "CHAR")
	(cons "char*" "STRING")
	(cons "GLbyte" "INT")
	(cons "GLshort" "INT")
	(cons "GLbitfield" "ULONG")
	(cons "GLboolean" "BOOLEAN")
	(cons "GLushort" "INT")
	(cons "GLuint" "ULONG")
	(cons "GLubyte" "INT")
	(cons "unsigned_long" "ULONG")
	(cons "Bool" "BOOLEAN")
	(cons "xen" #t)
	(cons "constchar*" "STRING")

	(cons "guint" "INT")
	(cons "gint" "INT")
	(cons "gboolean" "BOOLEAN")

	))

(define glu-1-2 '("GLUtesselator*" "gluBeginPolygon" "gluDeleteTess" "gluEndPolygon" "gluNextContour" "gluTessVertex"
		  "gluGetTessProperty" "gluTessBeginContour" "gluTessBeginPolygon" "gluTessEndContour" "gluTessEndPolygon"
		  "gluTessNormal" "gluTessProperty" "gluNewTess"))

(define (type-it type)
  (let ((typ (assoc type direct-types))
	(g2 '()))
    (if typ
	(if (cdr typ)
	    (begin
	      (if (string? (cdr typ))
		  (begin
		    (if (not (member (car typ)
				     (list "Display*" "XVisualInfo*" "int*" "Pixmap" "Font" "GLubyte*"
					   "GLdouble*" "GLfloat*" "GLvoid*" "GLuint*"
					   "GLboolean*" "void*" "GLint*" "GLshort*"
					   "GLsizei" "GLclampd" "GLclampf" "GLbitfield" "GLshort" "GLubyte" "GLbyte"
					   "unsigned_long"
					   "void**")))
			(if (string=? (car typ) "constchar*")
			    (hey "#define C_TO_XEN_~A(Arg) C_TO_XEN_~A((char *)(Arg))~%" (no-stars (car typ)) (cdr typ))
			    (hey "#define C_TO_XEN_~A(Arg) C_TO_XEN_~A(Arg)~%" (no-stars (car typ)) (cdr typ))))
		    (if (not (member (car typ)
				     (list "constchar*")))
			(hey "#define XEN_TO_C_~A(Arg) (~A)(XEN_TO_C_~A(Arg))~%" 
			     (no-stars (car typ)) (car typ) (cdr typ)))
		    (if (not (member (car typ)
				     (list "constchar*")))
			(hey "#define XEN_~A_P(Arg) XEN_~A_P(Arg)~%" 
			     (no-stars (car typ))
			     (if (string=? (cdr typ) "INT") 
				 "INTEGER" 
				 (if (string=? (cdr typ) "DOUBLE")
				     "NUMBER"
				     (cdr typ))))))
		  (begin
		    (hey "#define XEN_~A_P(Arg) 1~%" (no-stars (car typ)))
		    (hey "#define XEN_TO_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ)))))))
	(if (not (or (string=? type "Display*")
		     (string=? type "XVisualInfo*")))
	    (begin
	      (if (member type glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%"))
	      (hey "XL_TYPE~A~A(~A, ~A)~%" 
		   (if (has-stars type) "_PTR" "")
		   (if (member type (list "int*" "Pixmap" "Font" "GLubyte*" 
					  "GLubyte*" "GLdouble*" "GLfloat*" "GLvoid*" 
					  "GLuint*" "GLboolean*" "GLint*" "GLshort*"
					  "PangoFontDescription*" "GtkWidget*" "GdkGLConfigMode"
					  ))
		       "_1" 
		       (if (member type (list "GdkVisual*" "PangoFont*" "GdkColormap*"))
			   "_2" 
			   ""))
		   (no-stars type)
		   type)
	      (if (member type glu-1-2) (hey "#endif~%")))
	    (if (string=? type "Display*")
		(hey "XL_TYPE_1(Display, Display*)~%")
		(hey "XL_TYPE(XVisualInfo, XVisualInfo*)~%"))))))

(define* (CFNC data #:optional spec spec-name)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(display (format #f "~A CFNC~%" name))
	(let ((type (car-str data)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args)))
	    (if spec
		(set! funcs (cons (list name type strs args spec spec-name) funcs))
		(set! funcs (cons (list name type strs args) funcs)))
	    (set! names (cons (cons name 'fnc) names)))))))

(define* (CINT name #:optional type)
  (if (assoc name names)
      (display (format #f "~A CINT~%" name))
      (begin
	(set! ints (cons name ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CFNC-X data #:optional spec spec-name)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(display (format #f "~A CFNC-X~%" name))
	(let ((type (car-str data)))
	  (if (not (member type x-types))
	      (set! x-types (cons type x-types)))
	  (let ((strs (parse-args args 'x)))
	    (if spec
		(set! x-funcs (cons (list name type strs args spec spec-name) x-funcs))
		(set! x-funcs (cons (list name type strs args) x-funcs)))
	    (set! names (cons (cons name 'fnc) names)))))))

(define* (CINT-X name #:optional type)
  (if (assoc name names)
      (display (format #f "~A CINT-X~%" name))
      (begin
	(set! x-ints (cons name x-ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CFNC-G data #:optional spec spec-name)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(display (format #f "~A CFNC-G~%" name))
	(let ((type (car-str data)))
	  (if (not (member type g-types))
	      (set! g-types (cons type g-types)))
	  (let ((strs (parse-args args 'g)))
	    (if spec
		(set! g-funcs (cons (list name type strs args spec spec-name) g-funcs))
		(set! g-funcs (cons (list name type strs args) g-funcs)))
	    (set! names (cons (cons name 'fnc) names)))))))

(define* (CFNC-G5 data #:optional spec spec-name)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(display (format #f "~A CFNC-G5~%" name))
	(let ((type (car-str data)))
	  (if (not (member type g-types))
	      (set! g-types (cons type g-types)))
	  (let ((strs (parse-args args 'g)))
	    (if spec
		(set! g5-funcs (cons (list name type strs args spec spec-name) g5-funcs))
		(set! g5-funcs (cons (list name type strs args) g5-funcs)))
	    (set! names (cons (cons name 'fnc) names)))))))

(define* (CINT-G name #:optional type)
  (if (assoc name names)
      (display (format #f "~A CINT-G~%" name))
      (begin
	(set! g-ints (cons name g-ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-G5 name #:optional type)
  (if (assoc name names)
      (display (format #f "~A CINT-G5~%" name))
      (begin
	(set! g5-ints (cons name g5-ints))
	(set! names (cons (cons name 'int) names)))))

(define (no-arg name)
  (let ((len (string-length name)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) name)
	 (if (char=? (string-ref name i) #\()
	     (return (substring name 0 i))))))))

;;; ---------------------------------------- read data ---------------------------------------- 

(load "gldata.scm")

;;; ---------------------------------------- write output file ----------------------------------------
(hey "/* gl.c: Guile, Gauche, Ruby, and Forth bindings for GL, GLU~%")
(hey " *   generated automatically from makegl.scm and gldata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " * reference args are ignored if passed, resultant values are returned in a list.~%")
(hey " * the various \"v\" forms are omitted for now -- are they needed in this context?~%")
(hey " * 'gl is added to *features*~%")
(hey " *~%")
(hey " * HISTORY:~%")
(hey " *     30-Mar-06: check for glu.h, omit GLU_* if necessary.  Add Forth support.~%")
(hey " *     --------~%")
(hey " *     13-Jun-05: merged gl-ruby.c into gl.c.~%")
(hey " *     --------~%")
(hey " *     10-Mar:    Gl_Version.~%")
(hey " *     1-Feb-03:  glGet* funcs now try to handle multiple return values correctly.~%")
(hey " *     --------~%")
(hey " *     18-Nov:    added more GtkGlext bindings.~%")
(hey " *     1-Aug:     removed all 'EXT' junk.~%")
(hey " *     24-July:   changed Guile prefix (R5RS reserves vertical-bar).~%")
(hey " *     18-June:   GL 1.1 stubs.~%")
(hey " *     4-June:    GtkGLext support.~%")
(hey " *     20-May-02: initial version.~%")
(hey " */~%~%")

(hey "#include <mus-config.h>~%~%")

(hey "#if HAVE_EXTENSION_LANGUAGE~%")
(hey "#if USE_GTK~%")
(hey "  #include <gtk/gtkgl.h>~%")
(hey "#endif~%")
(hey "#include <GL/gl.h>~%")
(hey "#if HAVE_GLU~%")
(hey "  #include <GL/glu.h>~%")
(hey "#endif~%")
(hey "#if USE_MOTIF~%")
(hey "  #include <GL/glx.h>~%")
(hey "#endif~%")
(hey "#include <string.h>~%~%")

(hey "#if USE_SND~%")
(hey "  /* USE_SND causes xm to use Snd's error handlers which are much smarter than xen's fallback versions */~%")
(hey "  #include \"snd.h\"~%")
(hey "#else~%")
(hey "  #include \"xen.h\"~%")
(hey "#endif~%")
(hey "#ifndef CALLOC~%")
(hey "  #define CALLOC(a, b)  calloc((size_t)(a), (size_t)(b))~%")
(hey "  #define FREE(a)       free(a)~%")
(hey "#endif~%~%")

(hey "#ifndef unsigned_long~%")
(hey "  /* for FreeBSD (thanks to Michael Scholz) (can't use ulong here due to collisions elsewhere) */~%")
(hey "  typedef unsigned long unsigned_long;~%")
(hey "#endif~%~%")

(hey "/* prefix for all names */~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define XL_PRE \"\"~%")
(hey "  #define XL_POST \"\"~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY~%")
(hey "/* for Ruby, XG PRE needs to be uppercase */~%")
(hey "  #define XL_PRE \"R\"~%")
(hey "  #define XL_POST \"\"~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "  #define XL_PRE \"F\"~%")
(hey "  #define XL_POST \"\"~%")
(hey "#endif~%")
(hey "~%")

(hey "#define WRAP_FOR_XEN(Name, Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(Name), C_TO_XEN_ULONG((unsigned long)Value))~%")
(hey "#define WRAP_P(Name, Value) (XEN_LIST_P(Value) && \\~%")
(hey "                            (XEN_LIST_LENGTH(Value) >= 2) && \\~%")
(hey "                            (XEN_SYMBOL_P(XEN_CAR(Value))) && \\~%")
(hey "                            (strcmp(Name, XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))~%")
(hey "~%")
(hey "#define XL_TYPE(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "#define XL_TYPE_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XL_TYPE_PTR(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));} /* if NULL ok, should be explicit */~%")
(hey "#define XL_TYPE_PTR_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));} /* if NULL ok, should be explicit */~%")
(hey "#define XL_TYPE_PTR_2(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);}~%")

(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(hey "#if USE_MOTIF~%")
(for-each type-it (reverse x-types))
(hey "#endif~%")

(hey "#if USE_GTK~%")
(for-each type-it (reverse g-types))
(hey "#endif~%")

(for-each type-it (reverse types))

(hey "~%~%/* ---------------------------------------- state readback confusion ---------------------------------------- */~%~%")

(hey "static int how_many_vals(GLenum gl)~%")
(hey "{~%")
(hey "  switch (gl)~%")
(hey "    {~%")
(hey "    case GL_CURRENT_COLOR:~%")
(hey "    case GL_CURRENT_TEXTURE_COORDS:~%")
(hey "    case GL_CURRENT_RASTER_POSITION:~%")
(hey "    case GL_CURRENT_RASTER_COLOR:~%")
(hey "    case GL_CURRENT_RASTER_TEXTURE_COORDS:~%")
(hey "    case GL_VIEWPORT:~%")
(hey "    case GL_FOG_COLOR:~%")
(hey "    case GL_AMBIENT:~%")
(hey "    case GL_DIFFUSE:~%")
(hey "    case GL_SPECULAR:~%")
(hey "    case GL_EMISSION:~%")
(hey "    case GL_LIGHT_MODEL_AMBIENT:~%")
(hey "    case GL_SCISSOR_BOX:~%")
(hey "    case GL_COLOR_WRITEMASK:~%")
(hey "    case GL_COLOR_CLEAR_VALUE:~%")
(hey "      return(4);~%")
(hey "      break;~%")
(hey "    case GL_MODELVIEW_MATRIX:~%")
(hey "    case GL_PROJECTION_MATRIX:~%")
(hey "    case GL_TEXTURE_MATRIX:~%")
(hey "      return(16);~%")
(hey "      break;~%")
(hey "    case GL_CURRENT_NORMAL:~%")
(hey "    case GL_SPOT_DIRECTION:~%")
(hey "      return(3);~%")
(hey "      break;~%")
(hey "    case GL_DEPTH_RANGE:~%")
(hey "    case GL_LINE_WIDTH_RANGE:~%")
(hey "      return(2);~%")
(hey "      break;~%")
(hey "    default: return(1); break; /* try to squelch c++ babbling */~%")
(hey "    }~%")
(hey "  return(1);~%")
(hey "}~%")

(define need-vals-check (list "glGetIntegerv" "glGetFloatv" "glGetMaterialfv" "glGetLightfv" "glGetBooleanv"))


(hey "~%~%/* ---------------------------------------- functions ---------------------------------------- */~%~%")

(define handle-func
 (lambda (data)
   (let* ((name (car data))
	  (return-type (cadr data))
	  (args (caddr data))
	  (cargs (length args))
	  (refargs (ref-args args))
	  (xgargs (- cargs refargs))
	  (argstr (cadddr data))
	  (lambda-type (cdr (assoc name names)))
	  (arg-start 0)
	  (line-len 0)
	  (line-max 120)
	  (protect-arglist #f)
	  (max-args 10)) ; libguile/gsubr.h:#define SCM_GSUBR_MAX 10

     (define (hey-start)
       ;; start of checked line
       (set! line-len 0))

     (define (hey-mark)
       ;; start of checked line
       (set! arg-start line-len))

     (define (hey-on . args)
       ;; no cr -- just append
       (let ((line (apply format #f args)))
	 (set! line-len (+ line-len (string-length line)))
	 (heyc line)))

     (define (hey-ok arg)
       ;; cr ok after arg
       (set! line-len (+ line-len (string-length arg)))
       (heyc arg)
       (if (> line-len line-max)
	   (begin
	     (hey "~%")
	     (do ((i 0 (1+ i)))
		 ((= i arg-start))
	       (heyc " "))
	     (set! line-len arg-start))))

     (check-glu name)
     (if (member name glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%"))

     (if (and (> (length data) 4)
	      (eq? (list-ref data 4) 'if))
	 (hey "#if HAVE_~A~%" (string-upcase (symbol->string (list-ref data 5)))))
     (hey "static XEN gxg_~A(" name)
     (if (= (length args) 0)
	 (heyc "void")
	 (if (>= (length args) max-args)
	     (begin
	       (heyc "XEN arglist")
	       (set! protect-arglist #t))
	     (let ((previous-arg #f))
	       (for-each 
		(lambda (arg)
		  (let ((argname (cadr arg))
			(argtype (car arg)))
		    (if previous-arg (heyc ", "))
		    (set! previous-arg #t)
		    (hey "XEN ~A" argname)))
		args))))
     (hey ")~%{~%")
     (helpify name return-type argstr)
     (if (> refargs 0)
	 (for-each
	  (lambda (arg)
	    (if (ref-arg? arg)
		(if (member name need-vals-check)
		    (hey "  ~A ~A[16];~%" (deref-type arg) (deref-name arg))
		    (hey "  ~A ~A[1];~%" (deref-type arg) (deref-name arg)))))
	  args))
     (if (and (>= (length args) max-args)
	      (> xgargs 0))
	 (let ((previous-arg #f))
	   (heyc "  XEN ")
	   (for-each
	    (lambda (arg)
	      (if (not (ref-arg? arg)) ;(< (length arg) 3)
		  (begin
		    (if previous-arg (heyc ", "))
		    (set! previous-arg #t)
		    (hey "~A" (cadr arg)))))
	    args)
	   (hey ";~%")
	   (let ((ctr 0)) ; list-ref counts from 0
	     (for-each
	      (lambda (arg)
		(if (not (ref-arg? arg))
		    (hey "  ~A = XEN_LIST_REF(arglist, ~D);~%" (cadr arg) ctr))
		(set! ctr (1+ ctr)))
	      args))))
     (if (> (length args) 0)
	 (let ((ctr 1))
	   (for-each
	    (lambda (arg)
	      (let ((argname (cadr arg))
		    (argtype (car arg)))
		(if (not (ref-arg? arg))
		    (if (null-arg? arg)
			(hey "  XEN_ASSERT_TYPE(XEN_~A_P(~A) || XEN_FALSE_P(~A), ~A, ~D, ~S, ~S);~%" 
			     (no-stars argtype) argname argname argname ctr name argtype)
			(if (opt-arg? arg)
			    (begin
			      (hey "  if (XEN_NOT_BOUND_P(~A)) ~A = XEN_FALSE; ~%" argname argname)
			      (hey "  else XEN_ASSERT_TYPE(XEN_~A_P(~A), ~A, ~D, ~S, ~S);~%" 
				   (no-stars argtype) argname argname ctr name argtype))
			    (hey "  XEN_ASSERT_TYPE(XEN_~A_P(~A), ~A, ~D, ~S, ~S);~%"
				 (no-stars argtype) argname argname ctr name argtype))))
		(set! ctr (1+ ctr))))
	    args)))
     (let ((using-result #f)
	   (using-loc #f))
       (set! using-result (and (> refargs 0)
			       (not (string=? return-type "void"))))
       (if using-result
	   (begin
	     (hey "  {~%")
	     (hey "    XEN result = XEN_FALSE;~%")))
       (hey-start)
       (if (not (string=? return-type "void"))
	   (if (= refargs 0)
	       (hey-on "  return(C_TO_XEN_~A(" (no-stars return-type))
	       (hey-on "    result = C_TO_XEN_~A(" (no-stars return-type)))
	   (hey-on "  "))

       (hey-on "~A(" name)
       (hey-mark)
       (if (> (length args) 0)
	   (let ((previous-arg #f))
	     (for-each
	      (lambda (arg)
		(let ((argname (cadr arg))
		      (argtype (car arg)))
		  (if previous-arg (hey-ok ", "))
		  (if (and (not previous-arg)
			   (> (length data) 4)
			   (eq? (list-ref data 4) 'const))
		      (hey "(const ~A)" argtype))
		  (set! previous-arg #t)
		  (if (ref-arg? arg)
		      (hey-on "~A" (deref-name arg))
		      (hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname))))
	      args)))

       (if (> refargs 0)
	   (let* ((previous-arg using-result))
	     (if (not (string=? return-type "void")) 
		 (heyc ")"))
	     (hey ");~%")
	     (if using-result (heyc "  "))
	     (if (member name need-vals-check)
		 (begin
		   (hey "  {~%")
		   (if (not using-result)
		       (hey "    XEN result;~%"))
		   (hey "    int i, vals;~%")
		   (hey "    vals = how_many_vals(XEN_TO_C_GLenum(pname));~%")
		   (hey "    result = XEN_EMPTY_LIST;~%")
		   (hey "    for (i = 0; i < vals; i++)~%")
		   (hey "      result = XEN_CONS(C_TO_XEN_~A(~A[i]), result);~%" 
			(no-stars (deref-type (list-ref args (- (length args) 1))))
			(deref-name (list-ref args (- (length args) 1))))
		   (if protect-arglist
		       (hey "    return(xen_return_first(result, arglist));~%")
		       (hey "    return(result);~%"))
		   (hey "  }~%"))
		 (begin
		   (hey "  return(XEN_LIST_~D(" (+ refargs (if using-result 1 0)))
		   (if using-result (heyc "result"))
		   (for-each 
		    (lambda (arg)
		      (if (ref-arg? arg)
			  (begin
			    (if previous-arg (heyc ", "))
			    (hey "C_TO_XEN_~A(~A[0])" (no-stars (deref-type arg)) (deref-name arg))
			    (set! previous-arg #t))))
		    args)
		   (hey "));~%")))
	     (if using-result (hey "   }~%")))
	   (if (string=? return-type "void")
	       (begin
		 (hey ");~%")
		 (if protect-arglist
		     (hey "  return(xen_return_first(XEN_FALSE, arglist));~%")
		     (hey "  return(XEN_FALSE);~%")))
	       (hey ")));~%")))
       )

     (hey "}~%")
     (if (member name glu-1-2) (hey "#endif~%"))
     (hey "~%")
     )))

(hey "#if USE_MOTIF~%")
(for-each handle-func (reverse x-funcs))
(hey "#endif~%")

(hey "#if USE_GTK~%")
(for-each handle-func (reverse g-funcs))
(hey "#ifdef GTKGLEXT_MAJOR_VERSION~%")
(for-each handle-func (reverse g5-funcs))
(hey "#endif~%")
(hey "#endif~%~%")

(for-each handle-func (reverse funcs))
(uncheck-glu)


;;; ---------------- argify linkages

(hey "#ifdef XEN_ARGIFY_1~%")
(define (argify-func func)
  (let* ((cargs (length (caddr func)))
	 (name (car func))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs))
	 (if-fnc (and (> (length func) 4)
		      (eq? (list-ref func 4) 'if))))
    (check-glu name)
    (if (member name glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%"))
    (if if-fnc
	(hey "#if HAVE_~A~%" (string-upcase (symbol->string (list-ref func 5)))))
    (hey "XEN_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs 10) "VARGIFY"
	     (if (> refargs 0)
		 (format #f "ARGIFY_~D" cargs)
		 (format #f "NARGIFY_~D" cargs)))
	 (car func) (car func))
    (if if-fnc
	(hey "#endif~%"))
    (if (member (car func) glu-1-2) (hey "#endif~%"))))
	 
(hey "#if USE_MOTIF~%")
(for-each argify-func (reverse x-funcs))
(hey "#endif~%")

(hey "#if USE_GTK~%")
(for-each argify-func (reverse g-funcs))
(hey "#ifdef GTKGLEXT_MAJOR_VERSION~%")
(for-each argify-func (reverse g5-funcs))
(hey "#endif~%")
(hey "#endif~%~%")

(for-each argify-func (reverse funcs))
(uncheck-glu)

(hey "~%#else~%~%")

(define (unargify-func func)
  (let* ((cargs (length (caddr func)))
	 (name (car func))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs))
	 (if-fnc (and (> (length func) 4)
		      (eq? (list-ref func 4) 'if))))

    (check-glu name)
    (if (member name glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%"))

    (if if-fnc
	(hey "#if HAVE_~A~%" (string-upcase (symbol->string (list-ref func 5)))))
    (hey "#define gxg_~A_w gxg_~A~%" 
	 (car func) (car func))
    (if if-fnc
	(hey "#endif~%"))
    (if (member (car func) glu-1-2) (hey "#endif~%"))))
	 
(hey "#if USE_MOTIF~%")
(for-each unargify-func (reverse x-funcs))
(hey "#endif~%")

(hey "#if USE_GTK~%")
(for-each unargify-func (reverse g-funcs))
(hey "#ifdef GTKGLEXT_MAJOR_VERSION~%")
(for-each unargify-func (reverse g5-funcs))
(hey "#endif~%")
(hey "#endif~%~%")

(for-each unargify-func (reverse funcs))
(uncheck-glu)

(hey "#endif~%")


;;; ---------------- procedure linkages
(hey "static void define_functions(void)~%")
(hey "{~%")
(hey "  #define GL_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XL_PRE #Name XL_POST, Value, A1, A2, A3, Help)~%")

(define (defun func)
  (let* ((cargs (length (caddr func)))
	 (name (car func))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs)))
    (check-glu name)
    (if (member name glu-1-2) (hey "#ifdef GLU_VERSION_1_2~%"))

    (hey "  GL_DEFINE_PROCEDURE(~A, gxg_~A_w, ~D, ~D, ~D, H_~A);~%"
		     (car func) (car func) 
		     (if (>= cargs 10) 0 args)
		     (if (>= cargs 10) 0 refargs) ; optional ignored
		     (if (>= cargs 10) 1 0)
		     (car func))
    (if (member (car func) glu-1-2) (hey "#endif~%"))
    ))

(hey "#if USE_MOTIF~%")
(for-each defun (reverse x-funcs))
(hey "#endif~%")

(hey "#if USE_GTK~%")
(for-each defun (reverse g-funcs))
(hey "#ifdef GTKGLEXT_MAJOR_VERSION~%")
(for-each defun (reverse g5-funcs))
(hey "#endif~%")
(hey "#endif~%")

(for-each defun (reverse funcs))
(uncheck-glu)

(hey "}~%~%")


(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%~%")
(hey "#define DEFINE_INTEGER(Name) XEN_DEFINE(XL_PRE #Name XL_POST, C_TO_XEN_INT(Name))~%")
(hey "~%")

(hey "#if USE_MOTIF~%")
(for-each 
 (lambda (val) 
   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse x-ints))
(hey "#endif~%")

(hey "#if USE_GTK~%")
(for-each 
 (lambda (val) 
   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse g-ints))
(hey "#ifdef GTKGLEXT_MAJOR_VERSION~%")
(for-each 
 (lambda (val) 
   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse g5-ints))
(hey "#endif~%")
(hey "#endif~%")

(for-each 
 (lambda (val) 

     (if in-glu
	 (if (not (string=? "GLU" (substring val 0 3)))
	     (BEGIN
	       (set! in-glu #f)
	       (hey "#endif~%")))
	 (if (string=? "GLU" (substring val 0 3))
	     (begin
	       (set! in-glu #t)
	       (hey "#if HAVE_GLU~%"))))

   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse ints))

(if in-glu
    (begin
      (hey "#endif~%")
      (set! in-glu #f)))


(hey "}~%~%")

(hey "/* -------------------------------- initialization -------------------------------- */~%~%")
(hey "static bool gl_already_inited = false;~%~%")
(hey "void Init_libgl(void);~%")
(hey "void Init_libgl(void)~%")
(hey "{~%")
(hey "  if (!gl_already_inited)~%")
(hey "    {~%")
(hey "      define_integers();~%")
(hey "      define_functions();~%")
(hey "      XEN_YES_WE_HAVE(\"gl\");~%")
(hey "      XEN_DEFINE(\"gl-version\", C_TO_XEN_STRING(\"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "      gl_already_inited = true;~%")
(hey "    }~%")
(hey "}~%")
(hey "#else~%")
(hey " void Init_libgl(void);~%")
(hey " void Init_libgl(void)~%")
(hey "{~%")
(hey "}~%")
(hey "#endif~%")

(close-output-port gl-file)

