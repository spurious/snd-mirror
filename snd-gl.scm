;;; examples of using the GL bindings

(use-modules (ice-9 format))

(define (gl-info)
  ;; taken loosely from glxvisuals.c
  "(gl-info) prints out GL-related info"
  (define (class-of n)
    (cond ((= n |StaticGray) "static-gray")
	  ((= n |GrayScale) "gray-scale")
	  ((= n |StaticColor) "static-color")
	  ((= n |PseudoColor) "pseudo-color")
	  ((= n |TrueColor) "true-color")
	  ((= n |DirectColor) "direct-color")
	  (#t "??")))
  (let* ((cx (snd-glx-context))
	 (dpy (|XtDisplay (cadr (main-widgets))))
	 (version (|glXQueryVersion dpy 0 0)))
    (if (car version)
	(let* ((scr (|DefaultScreen dpy))
	       (visuals (|XGetVisualInfo dpy 0 (list 'XVisualInfo 0))))
	  (|glXMakeCurrent dpy (|XtWindow (cadr (main-widgets))) cx)
	  (snd-print (format #f "GL version: ~A.~A, (~A ~A ~A)~%"
			     (cadr version) (caddr version)
			     (|glGetString |GL_VENDOR) (|glGetString |GL_RENDERER) (|glGetString |GL_VERSION)))
	  (snd-print (format #f "  with: ~A~A~%"
			     (|glXQueryExtensionsString dpy (|XScreenNumberOfScreen (|DefaultScreenOfDisplay dpy)))
			     (if (|glXIsDirect dpy cx) ", direct rendering support" "")))
	  (for-each 
	   (lambda (visual)
	     (if (= (cadr (|glXGetConfig dpy visual |GLX_USE_GL)) 1)
		 ;; found a visual that can support GL
		 (let ((buffersize (cadr (|glXGetConfig dpy visual |GLX_BUFFER_SIZE)))
		       (level (cadr (|glXGetConfig dpy visual |GLX_LEVEL)))
		       (rgba (cadr (|glXGetConfig dpy visual |GLX_RGBA)))
		       (doublebuffer (cadr (|glXGetConfig dpy visual |GLX_DOUBLEBUFFER)))
		       (stereo (cadr (|glXGetConfig dpy visual |GLX_STEREO)))
		       (auxbuffers (cadr (|glXGetConfig dpy visual |GLX_AUX_BUFFERS)))
		       (redsize (cadr (|glXGetConfig dpy visual |GLX_RED_SIZE)))
		       (bluesize (cadr (|glXGetConfig dpy visual |GLX_BLUE_SIZE)))
		       (greensize (cadr (|glXGetConfig dpy visual |GLX_GREEN_SIZE)))
		       (alphasize (cadr (|glXGetConfig dpy visual |GLX_ALPHA_SIZE)))
		       (depthsize (cadr (|glXGetConfig dpy visual |GLX_DEPTH_SIZE)))
		       (stencilsize (cadr (|glXGetConfig dpy visual |GLX_STENCIL_SIZE)))
		       (acredsize (cadr (|glXGetConfig dpy visual |GLX_ACCUM_RED_SIZE)))
		       (acgreensize (cadr (|glXGetConfig dpy visual |GLX_ACCUM_GREEN_SIZE)))
		       (acbluesize (cadr (|glXGetConfig dpy visual |GLX_ACCUM_BLUE_SIZE)))
		       (acalphasize (cadr (|glXGetConfig dpy visual |GLX_ACCUM_ALPHA_SIZE))))
		   (snd-print (format #f "  id: #x~X depth: ~D class: ~S~%" (|visualid visual) (|depth visual) (class-of (|class visual))))
		   (snd-print (format #f "      buffersize: ~D, level: ~D, rgba: ~A, doublebuffer: ~A, stereo: ~A~%"
				      buffersize level
				      (if (= rgba 1) "#t" "#f")
				      (if (= doublebuffer 1) "#t" "#f")
				      (if (= stereo 1) "#t" "#f")))
		   (snd-print (format #f "      r: ~A, g: ~D, b: ~D, alpha: ~D, accum-r: ~D, accum-g: ~D, accum-b: ~D, accum-alpha: ~D~%"
				      redsize greensize bluesize alphasize 
				      acredsize acgreensize acbluesize acalphasize))
		   (snd-print (format #f "      auxbuffs: ~D, depth: ~D, acalpha: ~D~%"
				      auxbuffers depthsize stencilsize))

		   
		   )))
	   visuals))
	(snd-print "no GL found!"))))
