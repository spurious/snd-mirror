#!/home/bil/test/bin/guile -s
!#

;;; makexg.scm creates the gtk2/gdk/pango/glib bindings using xgdata.scm, writes xg.c and xg-ruby.c

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(use-modules (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(define include-deprecated #f) ; set to #t to get DISABLE_DEPRECATED entitities
(define worry-about-gtk-1 #f)

(define xg-file (open-output-file "xg.c"))
(define xg-ruby-file (open-output-file "xg-ruby.c"))

(define (hey . args)
  (display (apply format #f args) xg-file))

(define (heyc arg)
  (display arg xg-file))

(define (say . args)
  (display (apply format #f args) xg-ruby-file))

(define (say-hey . args)
  (apply hey args)
  (apply say args))

(define names '())
(define types '())
(define ints '())
(define ulongs '())
(define dbls '())
(define funcs '())
(define casts '())
(define checks '())
(define check-types '())
(define vars '())
(define atoms '())
(define strings '())
(define structs '())
(define make-structs '()) ; these have a xg-specific make function
(define struct-fields '())
;; "deprecated" for deprecated funcs
(define deprecated-types '())
(define deprecated-funcs '())
(define deprecated-casts '())
(define deprecated-checks '())
(define deprecated-ints '())
;;; "extra" for pango engine/backend
(define extra-types '())
(define extra-funcs '())
(define extra-casts '())
(define extra-checks '())
(define extra-ints '())
(define extra-strings '())
(define in-gtk1 #t)

(define* (check-gtk1 val #:optional addcr)
  (if worry-about-gtk-1
      (if (eq? val #t)
	  (if (not in-gtk1)
	      (begin
		(hey "#endif~%")
		(set! in-gtk1 #t)))
	  (if (member val gtk1)
	      (begin
		(if (not in-gtk1)
		    (begin
		      (hey "#endif~%")
		      (set! in-gtk1 #t)))
		(if addcr (hey "~%")))
	      (if in-gtk1
		  (begin
		    (if addcr (hey "~%"))
		    (hey "#if (!HAVE_GTK_1)~%")
		    (set! in-gtk1 #f))
		  (if addcr (hey "~%")))))))

(define idlers (list "gtk_idle_remove" "gtk_idle_remove_by_data" 
		     "gtk_quit_remove" "gtk_quit_remove_by_data" 
		     "gtk_input_remove" "gtk_key_snooper_remove"))

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
  (let ((len (string-length type))
	(val (string-copy type)))
    (do ((i 0 (1+ i)))
	((= i len) val)
      (if (char=? (string-ref val i) #\*)
	  (string-set! val i #\_)))))

(define (no-arg-or-stars name)
  (let ((len (string-length name)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) name)
	 (if (or (char=? (string-ref name i) #\()
		 (char=? (string-ref name i) #\*))
	     (return (substring name 0 i))))))))

(define (parse-args args broken)
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

			(if (eq? broken 'deprecated)
			    (if (and (not (member type types))
				     (not (member type deprecated-types)))
				(set! deprecated-types (cons type deprecated-types)))
			    (if (eq? broken 'extra)
				(if (and (not (member type types))
					 (not (member type extra-types)))
				    (set! extra-types (cons type extra-types)))
				(if (not (member type types))
				    (set! types (cons type types)))))
			(set! type #f))
		      (if (> i (1+ sp))
			  (set! type (substring args (1+ sp) i))))
		  (set! sp i))))))))

(define callbacks (list (list 'lambda2 ; unnamed gdk_window_invalidate_maybe_recurse argument
			      "gboolean"
			      "child_func"
			      (parse-args "GdkWindow* window lambda_data func_data" 'callback)
			      'temporary)
			(list 'lambda3 ; unnamed gtk_accel_group_find argument
			      "gboolean"
			      "find_func"
			      (parse-args "GtkAccelKey* key GClosure* closure lambda_data func_data" 'callback)
			      'temporary) ; ??
			(list 'GdkInputFunction
			      "void"
			      "input_func"
			      (parse-args "lambda_data func_data gint fd GdkInputCondition condition" 'callback)
			      'semi-permanent)
			(list 'GtkCallback
			      "void"
			      "func2"
			      (parse-args "GtkWidget* w lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkTimeoutFunction
			      "gint"
			      "timeout_func"
			      (parse-args "lambda_data func_data" 'callback)
			      'timeout)
			(list 'GtkDestroyNotify
			      "void"
			      "destroy_func"
			      (parse-args "lambda_data func_data" 'callback)
			      'permanent)
			(list 'GdkFilterFunc
			      "GdkFilterReturn"
			      "filter_func"
			      (parse-args "GdkXEvent* xevent GdkEvent* event lambda_data func_data" 'callback)
			      'permanent)
			(list 'GdkEventFunc
			      "void"
			      "event_func"
			      (parse-args "GdkEvent* event lambda_data func_data" 'callback)
			      'permanent)
			(list 'GdkSpanFunc
			      "void"
			      "span_func"
			      (parse-args "GdkSpan* span lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkFunction
			      "gboolean"
			      "func1"
			      (parse-args "lambda_data func_data" 'callback)
			      'semi-permanent)
			(list 'GtkKeySnoopFunc
			      "gint"
			      "snoop_func"
			      (parse-args "GtkWidget* widget GdkEventKey* event lambda_data func_data" 'callback)
			      'semi-permanent)
			(list 'GtkTranslateFunc
			      "gchar*"
			      "translate_func"
			      (parse-args "gchar* path lambda_data func_data" 'callback)
			      'permanent)
			(list 'GtkMenuPositionFunc
			      "void"
			      "menu_position_func"
			      (parse-args "GtkMenu* menu gint* x gint* y gboolean* push lambda_data func_data" 'callback)
			      'permanent)
			(list 'GtkTreeModelForeachFunc
			      "gboolean"
			      "model_func"
			      (parse-args "GtkTreeModel* model GtkTreePath* path GtkTreeIter* iter lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkTreeSelectionForeachFunc
			      "void"
			      "tree_selection_func"
			      (parse-args "GtkTreeModel* model GtkTreePath* path GtkTreeIter* iter lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkClipboardReceivedFunc
			      "void"
			      "clip_received"
			      (parse-args "GtkClipboard* clipboard GtkSelectionData* selection_data lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkClipboardTextReceivedFunc
			      "void"
			      "clip_text_received"
			      (parse-args "GtkClipboard* clipboard gchar* text lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkTreeViewColumnDropFunc
			      "gboolean"
			      "tree_column"
			      (parse-args "GtkTreeView* tree_view GtkTreeViewColumn* column GtkTreeViewColumn* prev_column GtkTreeViewColumn* next_column lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkTreeViewMappingFunc
			      "void"
			      "tree_mapping"
			      (parse-args "GtkTreeView* tree_view GtkTreePath* path lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkTreeViewSearchEqualFunc
			      "gboolean"
			      "tree_search"
			      (parse-args "GtkTreeModel* model gint column gchar* key GtkTreeIter* iter lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkTreeCellDataFunc
			      "void"
			      "cell_data"
			      (parse-args "GtkTreeViewColumn* tree_column GtkCellRenderer* cell GtkTreeModel* tree_model GtkTreeIter* iter lambda_data func_data" 'callback)
			      'permanent)
			(list 'GtkTreeIterCompareFunc
			      "gint"
			      "iter_compare"
			      (parse-args "GtkTreeModel* model GtkTreeIter* a GtkTreeIter* b lambda_data func_data" 'callback)
			      'permanent)
			(list 'GtkTreeSelectionFunc
			      "gboolean"
			      "tree_selection"
			      (parse-args "GtkTreeSelection* selection GtkTreeModel* model GtkTreePath* path gboolean path_currently_selected lambda_data func_data" 'callback)
			      'permanent)
			(list 'GtkClipboardGetFunc
			      "void"
			      "clip_get"
			      (parse-args "GtkClipboard* clipboard GtkSelectionData* selection_data guint info lambda_data func_data" 'callback)
			      'permanent)
			(list 'GtkClipboardClearFunc
			      "void"
			      "clip_clear"
			      (parse-args "GtkClipboard* clipboard lambda_data func_data" 'callback)
			      'permanent)

			;'GtkSignalFunc
			;'lambda can be whatever is indicated by caller (2 or 3 args)
			))

(define (callback-name func) (car func))
(define (callback-type func) (cadr func))
(define (callback-func func) (caddr func))
(define (callback-args func) (cadddr func))
(define (callback-gc func) (list-ref func 4))

(define (find-callback test)
  (define (find-callback-1 test funcs)
    (and (not (null? funcs))
	 (or (test (car funcs))
	     (find-callback-1 test (cdr funcs)))))
  (find-callback-1 test callbacks))

(define direct-types 
  (list (cons "void" #f)
	(cons "int" "INT")
	(cons "gint" "INT")
	(cons "guint32" "ULONG")
	(cons "gunichar" "ULONG")
	(cons "gunichar2" "INT")
	(cons "gulong" "ULONG")
	(cons "glong" "INT")
	(cons "gboolean" "BOOLEAN")
	(cons "gdouble" "DOUBLE")
	(cons "double" "DOUBLE")
	(cons "gfloat" "DOUBLE")
	(cons "char" "CHAR")
	(cons "gchar" "CHAR")
	(cons "char*" "STRING")
	(cons "gchar*" "STRING")
	;(cons "guchar*" "STRING") ; added 30-Jul-02 then removed (const char crap)
	(cons "guint" "ULONG")
	(cons "guint16" "INT")
	(cons "gint" "INT")
	(cons "gshort" "INT")
	(cons "gint16" "INT")
	(cons "guint8" "INT")
	(cons "guchar" "INT")
	(cons "gint8" "INT")
	(cons "xen" #t)

	;; since the various enums are handled directly (as ints) below, the associated types
	;;   need also to be direct (so that (= GDK_WHATEVER func-return-val) makes sense)
	;;   or should the constants be tagged? -- seems kinda silly

	(cons "GType" "ULONG")
	(cons "GQuark" "ULONG")
	(cons "GConnectFlags" "INT")
	(cons "GSignalMatchType" "INT")
	(cons "GSignalFlags" "INT")
	(cons "GdkInputCondition" "INT")
	(cons "GdkCursorType" "INT")
	(cons "GdkDragAction" "INT")
	(cons "GdkDragProtocol" "INT")
	(cons "GdkAxisUse" "INT")
	(cons "GdkGCValuesMask" "INT")
	(cons "GdkFill" "INT")
	(cons "GdkFunction" "INT")
	(cons "GdkSubwindowMode" "INT")
	(cons "GdkLineStyle" "INT")
	(cons "GdkCapStyle" "INT")
	(cons "GdkJoinStyle" "INT")
	(cons "GdkGrabStatus" "INT")
	(cons "GdkEventMask" "INT")
	(cons "GdkImageType" "INT")
	(cons "GdkInputSource" "INT")
	(cons "GdkInputMode" "INT")
	(cons "GdkNativeWindow" "ULONG")
	(cons "GdkModifierType" "INT")
	(cons "GdkExtensionMode" "INT")
	(cons "PangoDirection" "INT")
	(cons "GdkRgbDither" "INT")
	(cons "GdkPixbufAlphaMode" "INT")
	(cons "GdkPropMode" "INT")
	(cons "GdkFillRule" "INT")
	(cons "GdkOverlapType" "INT")
	(cons "GdkVisualType" "INT")
	(cons "GdkWindowType" "INT")
	(cons "GdkWindowState" "INT")
	(cons "GdkWMDecoration" "INT")
	(cons "GdkWMFunction" "INT")
	(cons "GdkWindowEdge" "INT")
	(cons "GtkAccelFlags" "INT")
	(cons "GtkArrowType" "INT")
	(cons "GtkShadowType" "INT")
	(cons "GtkButtonBoxStyle" "INT")
	(cons "GtkPathType" "INT")
	(cons "GtkPathPriorityType" "INT")
	(cons "GtkPackType" "INT")
	(cons "GtkReliefStyle" "INT")
	(cons "GtkCalendarDisplayOptions" "INT")
	(cons "GtkCellRendererState" "INT")
	(cons "GtkResizeMode" "INT")
	(cons "GtkCurveType" "INT")
	(cons "GtkDialogFlags" "INT")
	(cons "GtkDestDefaults" "INT")
	(cons "GtkPositionType" "INT")
	(cons "GtkTextDirection" "INT")
	(cons "GtkStateType" "INT")
	(cons "GtkImageType" "INT")
	(cons "GtkType" "ULONG")
	(cons "GtkIconSize" "INT")
	(cons "GtkJustification" "INT")
	(cons "GtkMessageType" "INT")
	(cons "GtkButtonsType" "INT")
	(cons "GtkProgressBarOrientation" "INT")
	(cons "GtkUpdateType" "INT")
	(cons "GtkMetricType" "INT")
	(cons "GtkPolicyType" "INT")
	(cons "GtkCornerType" "INT")
	(cons "GtkSizeGroupMode" "INT")
	(cons "GtkSpinButtonUpdatePolicy" "INT")
	(cons "GtkSpinType" "INT")
	(cons "GtkOrientation" "INT")
	(cons "GtkExpanderStyle" "INT")
	(cons "GtkAttachOptions" "INT")
	(cons "GtkTextSearchFlags" "INT")
	(cons "GtkTextWindowType" "INT")
	(cons "GtkWrapMode" "INT")
	(cons "GtkWindowPosition" "INT")
	(cons "GtkTreeViewColumnSizing" "INT")
	(cons "GtkTreeViewDropPosition" "INT")
	(cons "GtkToolbarChildType" "INT")
	(cons "GtkToolbarStyle" "INT")
	(cons "GtkTreeModelFlags" "INT")
	(cons "GtkSelectionMode" "INT")
	(cons "GtkSortType" "INT")
	(cons "GtkDirectionType" "INT")
	(cons "GtkWindowType" "INT")
	(cons "GdkWindowTypeHint" "INT")
	(cons "GdkGravity" "INT")
	(cons "GdkWindowHints" "INT")
	(cons "PangoAttrType" "INT")
	(cons "PangoStyle" "INT")
	(cons "PangoWeight" "INT")
	(cons "PangoVariant" "INT")
	(cons "PangoStretch" "INT")
	(cons "PangoUnderline" "INT")
	(cons "PangoFontMask" "INT")
	(cons "PangoWrapMode" "INT")
	(cons "PangoAlignment" "INT")
	(cons "PangoCoverageLevel" "INT")
	(cons "PangoGlyph" "ULONG")
	(cons "GdkEventType" "INT")
	(cons "GdkVisibilityState" "INT")
	(cons "GdkScrollDirection" "INT")
	(cons "GdkCrossingMode" "INT")
	(cons "GdkNotifyType" "INT")
	(cons "GdkSettingAction" "INT")
	(cons "GdkByteOrder" "INT")
	;(cons "GdkWChar" "ULONG")
	))

(if include-deprecated
    (set! direct-types (append direct-types
      (list		       
	(cons "GtkCellType" "INT")
	(cons "GtkCTreeLineStyle" "INT")
	(cons "GtkCTreeExpanderStyle" "INT")
	(cons "GtkScrollType" "INT")
	(cons "GtkPreviewType" "INT")
	(cons "GtkProgressBarStyle" "INT")
	(cons "GtkVisibility" "INT")
	(cons "GtkSignalRunType" "INT")))))

(define (type-it type)
  (let ((typ (assoc type direct-types))
	(g2 '()))
    (if typ
	(if (cdr typ)
	    (begin
	      (check-gtk1 #t)
	      (if (string? (cdr typ))
		  (begin
		    (hey "#define C_TO_XEN_~A(Arg) C_TO_XEN_~A(Arg)~%" (no-stars (car typ)) (cdr typ))
		    (hey "#define XEN_TO_C_~A(Arg) (~A)(XEN_TO_C_~A(Arg))~%" (no-stars (car typ)) (car typ) (cdr typ))
		    (hey "#define XEN_~A_P(Arg) XEN_~A_P(Arg)~%" 
			 (no-stars (car typ))
			 (if (string=? (cdr typ) "INT") 
			     "INTEGER" 
			     (if (string=? (cdr typ) "DOUBLE")
				 "NUMBER"
				 (cdr typ)))))
		  (begin
		    (if (not (cdr typ)) ; void special case
			(begin
			  (hey "#define XEN_~A_P(Arg) 1~%" (no-stars (car typ)))
			  (hey "#define XEN_TO_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ))))
			(begin          ; xen special case
			  (hey "#define XEN_~A_P(Arg) ((XEN_LIST_P(Arg)) && (XEN_LIST_LENGTH(Arg) > 2))~%" (no-stars (car typ)))
			  (hey "#define XEN_TO_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ)))))))))
	(if (and (not (string=? type "lambda"))
		 (not (string=? type "lambda_data"))
		 (not (find-callback 
		       (lambda (func)
			 (string=? type (symbol->string (car func))))))
		 (not (string=? type "GtkSignalFunc")))
	    (begin
	      (check-gtk1 (no-arg-or-stars type))
	      (hey "XM_TYPE~A(~A, ~A)~%" 
		   (if (has-stars type) "_PTR" "")
		   (no-stars type) 
		   type))))))

(define (CNAM name alias)
  (if (assoc name names)
      (no-way "~A CNAM~%" name)
      (begin
	(set! vars (cons (list name alias) vars))
	(set! names (cons (cons name 'name) names)))))

(define (func-type strs)
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (arg)
	(let ((callb (find-callback
		      (lambda (func)
			(and (string=? (car arg) (symbol->string (callback-name func)))
			     func)))))
	  (if callb
	      (return (callback-name callb))
	      (if (string=? (car arg) "lambda")
		  (return 'lambda)
		  (if (string=? (car arg) "GtkSignalFunc")
		      (return 'GtkSignalFunc))))))
      strs)
     'fnc)))

(define* (CFNC data #:optional spec spec-name)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC~%" name)
	(let ((type (car-str data)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args 'ok)))
	    (if spec
		(set! funcs (cons (list name type strs args spec spec-name) funcs))
		(set! funcs (cons (list name type strs args) funcs)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define (CFNC-dep data)
  (if include-deprecated
      (let ((name (cadr-str data))
	    (args (caddr-str data)))
	(if (assoc name names)
	    (no-way "~A CFNC-deprecated~%" name)
	    (let ((type (car-str data)))
	      (if (and (not (member type types))
		       (not (member type deprecated-types)))
		  (set! deprecated-types (cons type deprecated-types)))
	      (let ((strs (parse-args args 'deprecated)))
		(set! deprecated-funcs (cons (list name type strs args) deprecated-funcs))
		(set! names (cons (cons name (func-type strs)) names))))))))

(define (CFNC-extra data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-extra~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type extra-types)))
	      (set! extra-types (cons type extra-types)))
	  (let ((strs (parse-args args 'extra)))
	    (set! extra-funcs (cons (list name type strs args) extra-funcs))
	    (set! names (cons (cons name (func-type strs)) names)))))))

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

(define (CATOM name)
  (if (assoc name names)
      (no-way "~A CATOM~%" name)
      (begin
	(set! atoms (cons name atoms))
	(set! names (cons (cons name 'atom) names)))))

(define (CSTR name)
  (if (assoc name names)
      (no-way "~A CSTR~%" name)
      (begin
	(set! strings (cons name strings))
	(set! names (cons (cons name 'string) names)))))

(define (CSTR-extra name)
  (if (assoc name names)
      (no-way "~A CSTR~%" name)
      (begin
	(set! extra-strings (cons name extra-strings))
	(set! names (cons (cons name 'string) names)))))

(define (CDBL name)
  (if (assoc name names)
      (no-way "~A CDBL~%" name)
      (begin
	(set! dbls (cons name dbls))
	(set! names (cons (cons name 'dbl) names)))))

(define* (CLNG name #:optional type spec-name)
  (if (assoc name names)
      (no-way "~A CLNG~%" name)
      (begin
	(set! ulongs (cons (list name type spec-name) ulongs))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CINT name #:optional type)
  (if (assoc name names)
      (no-way "~A CINT~%" name)
      (begin
	(set! ints (cons name ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-extra name #:optional type)
  (if (assoc name names)
      (no-way "~A CINT-extra~%" name)
      (begin
	(set! extra-ints (cons name extra-ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-dep name #:optional type)
  (if include-deprecated
      (if (assoc name names)
	  (no-way "~A CINT-deprecated~%" name)
	  (begin
	    (set! deprecated-ints (cons name deprecated-ints))
	    (set! names (cons (cons name 'int) names))))))


(define (CCAST name type) ; this is the cast (type *)obj essentially but here it's (list type* (cadr obj))
  (if (assoc name names)
      (no-way "~A CCAST~%" name)
      (begin
	;;(if (not (member type types))
	;;    (set! types (cons type types)))
	(set! casts (cons (list name type) casts))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK name type) ; this is the check = (eq? type (car obj))
  (if (assoc name names)
      (no-way "~A CCHK~%" name)
      (begin
	(if (not (member type check-types))
	    (set! check-types (cons type check-types)))
	(set! checks (cons (list name type) checks))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-dep name type)
  (if include-deprecated
      (if (assoc name names)
	  (no-way "~A CCAST-deprecated~%" name)
	  (begin
	    (if (and (not (member type types))
		     (not (member type deprecated-types)))
		(set! deprecated-types (cons type deprecated-types)))
	    (set! deprecated-casts (cons (list name type) deprecated-casts))
	    (set! names (cons (cons name 'def) names))))))

(define (CCHK-dep name type)
  (if include-deprecated
      (if (assoc name names)
	  (no-way "~A CCHK~%" name)
	  (begin
	    (if (and (not (member type types))
		     (not (member type deprecated-types)))
		(set! deprecated-types (cons type deprecated-types)))
	    (set! deprecated-checks (cons (list name type) deprecated-checks))
	    (set! names (cons (cons name 'def) names))))))

(define (CCAST-extra name type)
  (if (assoc name names)
      (no-way "~A CCAST-extra~%" name)
      (begin
	(if (and (not (member type types))
		 (not (member type extra-types)))
	    (set! extra-types (cons type extra-types)))
	(set! extra-casts (cons (list name type) extra-casts))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-extra name type)
  (if (assoc name names)
      (no-way "~A CCHK~%" name)
      (begin
	(if (and (not (member type types))
		 (not (member type extra-types)))
	    (set! extra-types (cons type extra-types)))
	(set! extra-checks (cons (list name type) extra-checks))
	(set! names (cons (cons name 'def) names)))))

(define (STRUCT data)
  (let ((name (car-str data)) ; struct name (type)
	(args (cdr-str data)))
    (if (assoc name names)
	(no-way "~A STRUCT~%" name)
	(let ((strs (parse-args args 'ok)))
	  (if (not (member (string-append name "*") types))
	      (set! types (cons (string-append name "*") types)))
	  (for-each 
	   (lambda (field)
	     (if (not (member (cadr field) struct-fields))
		 (set! struct-fields (cons (cadr field) struct-fields))))
	   strs)
	  (set! structs (cons (list name strs args) structs))))))

(define (STRUCT-make data)
  (STRUCT data)
  (set! make-structs (cons (car-str data) make-structs)))

(define (find-struct name)
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (struct)
	(if (string=? name (car struct))
	    (return struct)))
      structs))))

(define (no-arg name)
  (let ((len (string-length name)))
    (call-with-current-continuation
     (lambda (return)
       (do ((i 0 (1+ i)))
	   ((= i len) name)
	 (if (char=? (string-ref name i) #\()
	     (return (substring name 0 i))))))))

;;; ---------------------------------------- read data ---------------------------------------- 
(load "xgdata.scm")

;(define listable-types (list "gint8*" "int*" "gint*" "gdouble*"))
(define listable-types '())
(for-each
 (lambda (type)
   (let* ((len (string-length type))
	  (dereftype (if (and (char=? (string-ref type (1- len)) #\*)
			      (not (string=? type "char*")) ; these are surely strings
			      (not (string=? type "gchar*")))
			 (substring type 0 (1- len)) 
			 #f)))
     (if (and dereftype
	      (assoc dereftype direct-types))
	 (set! listable-types (cons type listable-types)))))
 types)


(define (with-deprecated dpy thunk)
  (if include-deprecated
      (begin
	(check-gtk1 #t)
	(dpy "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED))) && (!(defined(GDK_PIXBUF_DISABLE_DEPRECATED)))~%")
	(thunk)
	(check-gtk1 #t)
	(dpy "#endif~%~%"))))

(define (with-extra dpy thunk)
  (check-gtk1 #t)
  (dpy "#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
  (thunk)
  (check-gtk1 #t)
  (dpy "#endif~%~%"))


;;; ---------------------------------------- write output files ----------------------------------------
(hey "/* xg.c: Guile and Ruby bindings for gdk/gtk/pango, some of glib~%")
(hey " *   generated automatically from makexg.scm and xgdata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " *   HAVE_GTK_1 if 1.2.n~%")
(hey " *   GDK_DISABLE_DEPRECATED, GTK_DISABLE_DEPRECATED, and GDK_PIXBUF_DISABLE_DEPRECATED are handled together~%")
(hey " *   PANGO_ENABLE_ENGINE and PANGO_ENABLE_BACKEND are handled together, and may be removed later~%")
(hey " *~%")
(hey " *   other flags:~%")

(let ((ifs '()))
  (for-each
   (lambda (func)
     (if (and (> (length func) 4)
	      (eq? (list-ref func 4) 'if)
	      (not (member (list-ref func 5) ifs)))
	 (begin
	   (hey " *     HAVE_~A~%" (string-upcase (symbol->string (list-ref func 5))))
	   (set! ifs (cons (list-ref func 5) ifs)))))
   funcs))

(hey " *~%")
(hey " * reference args are ignored if passed, resultant values are returned in a list.~%")
(hey " * null ptrs are passed and returned as #f, trailing \"user_data\" callback function arguments are optional (default: #f).~%")
(hey " * 'xg is added to *features*~%")
(hey " *~%")
(hey " * added funcs:~%")
(hey " *    (xm-version) -> date string.~%")
(hey " *    (c-array->list arr len) derefs each member of arr, returning lisp list~%")
(hey " *    (list->c-array lst ctype) packages each member of list as c-type \"type\" returning (wrapped) c array~%")

(for-each
 (lambda (name)
   (let ((args (cadr (find-struct name))))
     (if (> (length args) 0)
	 (hey " *    (~A #:optional" name)
	 (hey " *    (~A" name))
     (for-each
      (lambda (str)
	(hey " ~A" (cadr str)))
      args)
     (hey ") -> ~A struct~%" name)))
 (reverse make-structs))
 
(hey " *~%")
(hey " * omitted functions and macros:~%")
(hey " *     anything with a va_list or GtkArg* argument.  \"...\" args are ignored.~%")
(hey " *     most of the unusual keysym names~%")
(hey " *     all *_CLASS, *_IFACE macros~%")
(hey " *     deprecated macros that involve argument reordering~%")
(hey " *     deprecated struct field accessors, Pango struct field accessors~%")
(hey " *     win32-specific functions~%")
(hey " *~%")
(hey " * ~A: check out the g_signal handlers (gtk_signal_* is ok)~%" (string-append "T" "ODO"))
(hey " * ~A: GdkEvent casts~%" (string-append "T" "ODO"))
(hey " * ~A: struct print, more struct instance creators(?)~%" (string-append "T" "ODO"))
(hey " * ~A: tie into Snd (snd-motif translation)~%" (string-append "T" "ODO"))
(hey " * ~A: unprotect *_remove, unprotect old upon reset callback~%" (string-append "T" "ODO"))
(hey " * ~A: test suite (snd-test 24)~%" (string-append "T" "ODO"))
(hey " *~%")
(hey " * HISTORY:~%")
(hey " *     31-Jul:    removed GTK 1.n support (some of it can still be generated from makexg.scm)~%")
(hey " *     24-Jul:    changed Guile prefix (R5RS reserves vertical-bar).~%")
(hey " *     19-Jul:    XG_FIELD_PRE for change from using vertical-bar (reserved in R5RS)~%")
(hey " *     2-Jun:     removed deprecated and broken stuff (see include-deprecated switch in makexg.scm)~%")
(hey " *     4-Apr:     minor changes for Gtk 2.0.2~%")
(hey " *     13-Mar:    Gtk 2.0.0~%")
(hey " *     12-Mar:    support for GtkDestroyNotify callbacks~%")
(hey " *     27-Feb:    remove gtk_tree_view_column_cell_render, gtk_tree_view_column_cell_focus, ~%")
(hey " *                  gtk_tree_view_column_cell_draw_focus and gtk_tree_view_column_cell_set_dirty (privatized in 1.3.15)~%")
(hey " *                add (on HAVE-* switches) gtk_file_selection_get_selections, gtk_file_selection_set_select_multiple~%")
(hey " *                  and gtk_file_selection_get_select_multiple (new functions in 1.3.15)~%")
(hey " *                  also gtk_tree_path_new_first void and gtk_tree_model_get_iter_first (new names in 1.3.15)~%")
(hey " *     26-Feb:    Gtk 1.2.10 support, also 1.3.15~%")
(hey " *     25-Feb:    dialog example in libxm.html~%")
(hey " *                Ruby support via xg-ruby.c~%")
(hey " *     21-Feb:    #f=NULL throughout, gdk-pixbuf, GTypes.~%")
(hey " *     11-Feb-02: initial version.~%")
(hey " */~%~%")

(hey "#if defined(HAVE_CONFIG_H)~%  #include <config.h>~%#endif~%~%")

(hey "#include <glib.h>~%")
(hey "#include <gdk/gdk.h>~%")
(hey "#include <gdk/gdkkeysyms.h>~%")
(hey "#include <gtk/gtk.h>~%")
(if worry-about-gtk-1 (hey "#if (!HAVE_GTK_1)~%"))
(hey "#include <glib-object.h>~%")
(hey "#include <pango/pango.h>~%")
(if worry-about-gtk-1 (hey "#endif~%~%"))
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

(hey "/* -------------------------------- smob for GC -------------------------------- */~%")
(hey "static XEN_OBJECT_TYPE xm_obj_tag;~%")
(hey "#if HAVE_GUILE~%")
(hey "static size_t xm_obj_free(XEN obj)~%")
(hey "{~%")
(hey "  FREE((void *)XEN_OBJECT_REF(obj));~%")
(hey "  return(0);~%")
(hey "}~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY~%")
(hey "static void *xm_obj_free(XEN obj)~%")
(hey "{~%")
(hey "  FREE((void *)obj);~%")
(hey "  return(NULL);~%")
(hey "}~%")
(hey "#endif~%")
(hey "static XEN make_xm_obj(void *ptr)~%")
(hey "{~%")
(hey "  XEN_MAKE_AND_RETURN_OBJECT(xm_obj_tag, ptr, 0, xm_obj_free);~%")
(hey "}~%")
(hey "static void define_xm_obj(void)~%")
(hey "{~%")
(hey "  xm_obj_tag = XEN_MAKE_OBJECT_TYPE(\"XmObj\", sizeof(void *));~%")
(hey "#if HAVE_GUILE~%")
(hey "  scm_set_smob_free(xm_obj_tag, xm_obj_free);~%")
(hey "#endif~%")
(hey "}  ~%")
(hey "~%")

(hey "/* prefix for all names */~%")
(hey "#if HAVE_GUILE~%")
(hey "  #define XG_PRE \"\"~%")
(hey "  #define XG_FIELD_PRE \".\"~%")
(hey "  #define XG_POST \"\"~%")
(hey "#else~%")
(hey "/* for Ruby, XG PRE needs to be uppercase */~%")
(hey "  #define XG_PRE \"R\"~%")
(hey "  #define XG_POST \"\"~%")
(hey "  #define XG_FIELD_PRE \"R\"~%")
(hey "#endif~%")
(hey "~%")

(hey "#define WRAP_FOR_XEN(Name, Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(Name), C_TO_XEN_ULONG((unsigned long)Value))~%")
(hey "#define WRAP_P(Name, Value) (XEN_LIST_P(Value) && \\~%")
(hey "                            (XEN_LIST_LENGTH(Value) >= 2) && \\~%")
(hey "                            (XEN_SYMBOL_P(XEN_CAR(Value))) && \\~%")
(hey "                            (strcmp(Name, XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))~%")
(hey "~%")
(hey "#define XM_TYPE(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_PTR(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));} /* if NULL ok, should be explicit */~%")
(hey "~%")
(hey "/* type checks for callback wrappers */~%")

(for-each 
 (lambda (func)
   (hey "#define XEN_~A_P(Arg)  XEN_FALSE_P(Arg) || (XEN_PROCEDURE_P(Arg) && (XEN_REQUIRED_ARGS(Arg) == ~D))~%"
	(symbol->string (callback-name func))
	(length (callback-args func))))
 callbacks)

(hey "#define XEN_lambda_P(Arg) XEN_PROCEDURE_P(Arg)~%")
(hey "#define XEN_GtkSignalFunc_P(Arg) XEN_PROCEDURE_P(Arg) && (XEN_REQUIRED_ARGS(Arg) == 2)~%")

(for-each
 (lambda (func)
   (hey "#define XEN_TO_C_~A(Arg) XEN_FALSE_P(Arg) ? NULL : gxg_~A~%"
	(symbol->string (callback-name func))
	(callback-func func)))
 callbacks)

(hey "#define XEN_TO_C_GtkSignalFunc(Arg) (GtkSignalFunc)gxg_func2~%")
(hey "#define XEN_TO_C_lambda_data(Arg) (gpointer)gxg_ptr~%")
(hey "#define XEN_lambda_data_P(Arg) 1~%")

(hey "#define C_TO_XEN_GtkTreeViewSearchEqualFunc(Arg) WRAP_FOR_XEN(\"GtkTreeViewSearchEqualFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkTreeIterCompareFunc(Arg) WRAP_FOR_XEN(\"GtkTreeViewSearchEqualFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkTreeSelectionFunc(Arg) WRAP_FOR_XEN(\"GtkTreeSelectionFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkMenuPositionFunc(Arg) WRAP_FOR_XEN(\"GtkMenuPositionFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkDestroyNotify(Arg) WRAP_FOR_XEN(\"GtkDestroyNotify\", Arg)~%")
(hey "#define XEN_TO_C_GdkFilterReturn(Arg) (GdkFilterReturn)XEN_TO_C_INT(Arg)~%")


(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(for-each type-it (reverse types))

(define (check-type-it type)
  (check-gtk1 (no-arg-or-stars type))
  (hey "static XEN XEN_~A_p(XEN val) {return(C_TO_XEN_BOOLEAN(WRAP_P(~S, val)));}~%"
       (no-stars type) (no-stars type)))

(for-each check-type-it (reverse check-types))

(if (not (null? extra-types)) 
    (with-extra hey (lambda () 
		      (for-each type-it (reverse extra-types))
		      (for-each check-type-it  (reverse extra-types)))))

(if (not (null? deprecated-types))
    (with-deprecated hey (lambda () 
			   (for-each type-it (reverse deprecated-types))
			   (for-each check-type-it (reverse deprecated-types)))))


(hey "/* -------------------------------- gc protection -------------------------------- */~%")
(hey "~%")
(hey "static XEN xm_protected = XEN_FALSE;~%")
(hey "static int xm_protected_size = 0;~%")
(hey "static XEN xm_gc_table = XEN_FALSE;~%")
(hey "static int last_xm_unprotect = -1;~%")
(hey "~%")
(hey "static int xm_protect(XEN obj)~%")
(hey "{~%")
(hey "  int i, new_size;~%")
(hey "  XEN new_table;~%")
(hey "  XEN *older, *newer;~%")
(hey "  older = XEN_VECTOR_ELEMENTS(xm_protected);~%")
(hey "  if (last_xm_unprotect >= 0)~%")
(hey "    {~%")
(hey "      i = last_xm_unprotect;~%")
(hey "      if (XEN_FALSE_P(older[i]))~%")
(hey "	{~%")
(hey "	  older[i] = obj;~%")
(hey "	  last_xm_unprotect = -1;~%")
(hey "	  return(i);~%")
(hey "	}~%")
(hey "      last_xm_unprotect = -1;~%")
(hey "    }~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    if (XEN_FALSE_P(older[i]))~%")
(hey "      {~%")
(hey "	older[i] = obj;~%")
(hey "	return(i);~%")
(hey "      }~%")
(hey "  new_size = xm_protected_size * 2;~%")
(hey "  new_table = XEN_MAKE_VECTOR(new_size, XEN_FALSE);~%")
(hey "  newer = XEN_VECTOR_ELEMENTS(new_table);~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    {~%")
(hey "      newer[i] = older[i];~%")
(hey "      older[i] = XEN_FALSE;~%")
(hey "    }~%")
(hey "  newer[xm_protected_size] = obj;~%")
(hey "  XEN_VECTOR_SET(xm_gc_table, 0, new_table);~%")
(hey "  i = xm_protected_size;~%")
(hey "  xm_protected_size = new_size;~%")
(hey "  xm_protected = new_table;~%")
(hey "  return(i);~%")
(hey "}~%")
(hey "~%")
(hey "static void xm_unprotect_idler(guint id)~%")
(hey "{~%")
(hey "  int i;~%")
(hey "  XEN *velts;~%")
(hey "  XEN cur, idler;~%")
(hey "  velts = XEN_VECTOR_ELEMENTS(xm_protected);~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    {~%")
(hey "      cur = velts[i];~%")
(hey "      if ((XEN_LIST_P(cur)) && (XEN_LIST_LENGTH(cur) == 3) && (XEN_LIST_P(XEN_CADDR(cur))))~%")
(hey "        {~%")
(hey "          idler = XEN_CADDR(cur);~%")
(hey "          if ((XEN_SYMBOL_P(XEN_CAR(idler))) &&~%")
(hey "              (strcmp(\"idler\", XEN_SYMBOL_TO_C_STRING(XEN_CAR(idler))) == 0) &&~%")
(hey "              (id == XEN_TO_C_INT(XEN_CADR(idler))))~%")
(hey "            {~%")
(hey "              velts[i] = XEN_FALSE;~%")
(hey "              last_xm_unprotect = i;~%")
(hey "              return;~%")
(hey "            }}}~%")
(hey "}~%")
(hey "static void xm_unprotect_at(int ind)~%")
(hey "{~%")
(hey "  XEN *velts;~%")
(hey "  velts = XEN_VECTOR_ELEMENTS(xm_protected);~%")
(hey "  velts[ind] = XEN_FALSE;~%")
(hey "  last_xm_unprotect = ind;~%")
(hey "}~%~%")

(hey "~%~%/* ---------------------------------------- callback handlers ---------------------------------------- */~%~%")

(let ((funcs-done '()))
  (for-each
   (lambda (func)
     (let* ((name (callback-func func))
	    (type (callback-type func))
	    (args (callback-args func))
	    (gctype (callback-gc func))
	    (fname (callback-name func))
	    (void? (string=? type "void")))
       (check-gtk1 (symbol->string fname) #t)
       (if (not (member name funcs-done))
	   (begin
	     (set! funcs-done (cons name funcs-done))
	     (hey "static ~A gxg_~A("
		  type
		  name)
	     (if (eq? fname 'GtkTranslateFunc) ; sigh
		 (hey "const "))
	     (let ((previous-arg #f)
		   (ctr 0))
	       (for-each
		(lambda (arg)
		  (if previous-arg (hey ", "))
		  (if (or (and (eq? fname 'GtkClipboardTextReceivedFunc)
			       (= ctr 1))
			  (and (eq? fname 'GtkTreeViewSearchEqualFunc)
			       (= ctr 2)))
		      (hey "const "))
		  (set! ctr (1+ ctr))
		  (set! previous-arg #t)
		  (hey "~A ~A" 
		       (if (not (string=? (car arg) "lambda_data"))
			   (car arg)
			   "gpointer")
		       (cadr arg)))
		args)
	       (hey ")~%"))
	     (hey "{~%  ")
	     (if (eq? gctype 'timeout)
		 (begin
		   (hey "int call_again = 0;~%")
		   (hey "  XEN result = XEN_FALSE;~%")))
	     (let ((castlen (+ 12 (if (not void?) 
				      (if (eq? gctype 'timeout)
					  10
					  (+ 2 (string-length (format #f "return(XEN_TO_C_~A" (no-stars type)))))
				      1))))
	       (if (not void?)
		   (if (eq? gctype 'timeout)
		       (hey "  result = ")
		       (hey "return(XEN_TO_C_~A("
			    (no-stars type))))
	       (hey "XEN_CALL_~D(~A((XEN)func_data),~%"
		    (length args)
		    (if (eq? fname 'GtkClipboardClearFunc)
			"XEN_CADDR"
			(if (eq? fname 'GtkDestroyNotify)
			    "XEN_CADDDR"
			    "XEN_CAR")))
	       (for-each
		(lambda (arg)
		  (hey (substring "                                                                   " 0 castlen))
		  (if (not (string=? (car arg) "lambda_data"))
		      (hey "C_TO_XEN_~A(~A),~%"
			   (no-stars (car arg))
			   (cadr arg))
		      (hey "XEN_CADR((XEN)func_data),~%")))
		args)
	       (hey (substring "                                                                      " 0 castlen))
	       (hey "__FUNCTION__)")
	       (if void?
		   (hey ";~%")
		   (if (eq? gctype 'timeout)
		       (begin
			 (hey ";~%")
			 (hey "  if (XEN_BOOLEAN_P(result)) call_again = XEN_TO_C_BOOLEAN(result); else call_again = XEN_TO_C_INT_OR_ELSE(result, 0);~%")
			 (hey "  if (!call_again) xm_unprotect_at(XEN_TO_C_INT(XEN_CADDR((XEN)func_data)));~%")
			 (hey "  return(call_again);~%"))
		       (hey "));~%"))))
	     (hey "}~%")))))
   callbacks))

(check-gtk1 #t)
(hey "~%static void gxg_func3(GtkWidget *w, GdkEventAny *ev, gpointer data)~%")
(hey "{~%")
(hey "  XEN_CALL_3(XEN_CAR((XEN)data),~%")
(hey "             C_TO_XEN_GtkWidget_(w),~%")
(hey "             C_TO_XEN_GdkEventAny_(ev),~%")
(hey "             XEN_CADR((XEN)data),~%")
(hey "             __FUNCTION__);~%")
(hey "}~%~%")

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
	  (callback-data (and (not (eq? lambda-type 'fnc))
			      (find-callback 
			       (lambda (func)
				 (and (eq? (callback-name func) lambda-type)
				      func)))))
	  (arg-start 0)
	  (line-len 0)
	  (line-max 120)
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

     (check-gtk1 name #t)
     (if (and (> (length data) 4)
	      (eq? (list-ref data 4) 'if))
	 (hey "#if HAVE_~A~%" (string-upcase (symbol->string (list-ref data 5)))))
     (hey "static XEN gxg_~A(" name)
     (if (= (length args) 0)
	 (heyc "void")
	 (if (>= (length args) max-args)
	     (heyc "XEN arglist")
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
		(hey "  ~A ~A;~%" (deref-type arg) (deref-name arg))))
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
       (if (not (eq? lambda-type 'fnc))
	   (begin
	     (set! using-loc (or (eq? lambda-type 'GtkSignalFunc)
				 (and callback-data
				      (or (eq? (callback-gc callback-data) 'temporary)
					  (eq? (callback-gc callback-data) 'semi-permanent)
					  (eq? (callback-gc callback-data) 'timeout)))))
	     (set! using-result (and (not (string=? return-type "void"))
				     (not (eq? lambda-type 'lambda))))
	     (hey "  {~%")
	     (if using-result (hey "    XEN result = XEN_FALSE;~%"))
	     (if using-loc (hey "    int loc;~%"))
	     (hey "    XEN gxg_ptr = XEN_LIST_5(~A, func_data, XEN_FALSE, XEN_FALSE, XEN_FALSE);~%"
		  (call-with-current-continuation
		   (lambda (name-it)
		     (for-each
		      (lambda (arg)
			(let ((argname (cadr arg))
			      (argtype (car arg)))
			  (if (string=? argname "func")
			      (name-it "func"))))
		      args)
		     "XEN_FALSE")))
	     ;; TODO: if destroynotify(gdk/d) or callbackmarshal, append these to this list, add callbacks that access list-ref 3 and 4 using same func_data
	     (if using-loc
		 (hey "    loc = xm_protect(gxg_ptr);~%")
		 (hey "    xm_protect(gxg_ptr);~%"))
	     (if using-loc
		 (hey "    XEN_LIST_SET(gxg_ptr, 2, C_TO_XEN_INT(loc));~%")
		 (if (eq? lambda-type 'GtkClipboardGetFunc)
		     (hey "    XEN_LIST_SET(gxg_ptr, 2, clear_func);~%")))
	     (for-each
	      (lambda (arg)
		(let ((argname (cadr arg))
		      (argtype (car arg)))
		  (if (string=? argtype "GtkDestroyNotify")
		      (hey "    XEN_LIST_SET(gxg_ptr, 3, ~A);~%" argname))))
	      args)
	     (hey-start)
	     (if using-result
		 (hey-on "    result = C_TO_XEN_~A(" (no-stars return-type))
		 (heyc "    ")))
	   (begin
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
		 (hey-on "  "))))
       (if (not (eq? lambda-type 'lambda))
	   (begin
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
			    (hey-on "&~A" (deref-name arg))
			    (hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname))))
		    args)))
	     (if (not (eq? lambda-type 'fnc))
		 (if (not (string=? return-type "void")) 
		     (heyc ")"))
		 (if (not (string=? return-type "void"))
		     (if (= refargs 0)
			 (heyc "))")
			 (heyc ")"))))
	     (hey ");~%")
	     (if (not (eq? lambda-type 'fnc))
		 (begin
		   (if (and callback-data
			    (eq? (callback-gc callback-data) 'temporary))
		       (hey "    xm_unprotect_at(loc);~%"))
		   (if (and callback-data
			    (eq? (callback-gc callback-data) 'semi-permanent))
		       (hey "    XEN_LIST_SET(gxg_ptr, 2, XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(\"idler\"), ~A, C_TO_XEN_INT(loc)));~%"
			    (if (string=? return-type "void") "XEN_FALSE" "result")))
		   (if using-result
		       (hey "    return(result);~%")
		       (hey "    return(XEN_FALSE);~%"))
		   (hey "   }~%"))
		 (begin
		   (if (> refargs 0)
		       (let* ((previous-arg using-result))
			 (if using-result (heyc "  "))
			 (hey "  return(XEN_LIST_~D(" (+ refargs (if using-result 1 0)))
			 (if using-result (heyc "result"))
			 (for-each 
			  (lambda (arg)
			    (if (ref-arg? arg)
				(begin
				  (if previous-arg (heyc ", "))
				  (hey "C_TO_XEN_~A(~A)" (no-stars (deref-type arg)) (deref-name arg))
				  (set! previous-arg #t))))
			  args)
			 (hey "));~%")
			 (if using-result (hey "   }~%")))
		       (begin
			 (if (member name idlers)
			     (if (string=? name "gtk_idle_remove")
				 (hey "  xm_unprotect_idler(XEN_TO_C_guint(~A));~%" (cadr (car args)))
				 (hey "  xm_unprotect_at(XEN_TO_C_INT(XEN_CADDR(~A)));~%" (cadr (car args)))))
			 (if (string=? return-type "void")
			     (hey "  return(XEN_FALSE);~%")))))))
	   (begin ; 'lambda
	     (hey "if (XEN_REQUIRED_ARGS(func) == 2)~%")
	     (hey-start)
	     (if (not (string=? return-type "void"))
		 (hey-on "       return(C_TO_XEN_~A(~A(" (no-stars return-type) name)
		 (hey-on "       ~A(" name))
	     (hey-mark)
	     (let ((previous-arg #f))
	       (for-each
		(lambda (arg)
		  (let ((argname (cadr arg))
			(argtype (car arg)))
		    (if previous-arg (hey-ok ", "))
		    (set! previous-arg #t)
		    (if (string=? argtype "lambda")
			(hey-on "(GtkSignalFunc)gxg_func2")
			(hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname))))
		args))
	     (if (not (string=? return-type "void"))
		 (hey ")));~%")
		 (hey ");~%"))
	     (hey "     else~%")
	     (hey-start)
	     (if (not (string=? return-type "void"))
		 (hey-on "       return(C_TO_XEN_~A(~A(" (no-stars return-type) name)
		 (hey-on "       ~A(" name))
	     (hey-mark)
	     (let ((previous-arg #f))
	       (for-each
		(lambda (arg)
		  (let ((argname (cadr arg))
			(argtype (car arg)))
		    (if previous-arg (hey-ok ", "))
		    (set! previous-arg #t)
		    (if (string=? argtype "lambda")
			(hey-on "(GtkSignalFunc)gxg_func3")
			(hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname))))
		args))
	     (if (string=? return-type "void")
		 (begin
		   (hey ");~%")
		   (hey "    return(XEN_FALSE);~%"))
		 (hey ")));~%"))
	     (hey "  }~%"))))
     (hey "}~%")
     (if (and (> (length data) 4)
	      (eq? (list-ref data 4) 'if))
	 (hey "#endif~%"))
     ;(hey "~%")
     )))

(for-each handle-func (reverse funcs))
(if (not (null? extra-funcs)) (with-extra hey (lambda () (for-each handle-func (reverse extra-funcs)))))
(if (not (null? deprecated-funcs)) (with-deprecated hey (lambda () (for-each handle-func (reverse deprecated-funcs)))))

(define cast-it
 (lambda (cast)
   (let ((cast-name (car cast))
	 (cast-type (cadr cast)))
     (check-gtk1 (no-arg cast-name))
     (hey "static XEN gxg_~A(XEN obj)" (no-arg cast-name))
     (hey " {return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(~S), XEN_CADR(obj)));}~%" (no-stars cast-type)))))

(for-each cast-it (reverse casts))
(if (not (null? extra-casts)) (with-extra hey (lambda () (for-each cast-it (reverse extra-casts)))))
(if (not (null? deprecated-casts)) (with-deprecated hey (lambda () (for-each cast-it (reverse deprecated-casts)))))


;;; ---------------- Ruby step 1 ----------------
(say "/* Ruby connection for xg.c */~%~%")

(define (argify-func func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs))
	 (if-fnc (and (> (length func) 4)
		      (eq? (list-ref func 4) 'if))))
    (if if-fnc
	(say "#if HAVE_~A~%" (string-upcase (symbol->string (list-ref func 5)))))
    (say "XEN_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs 10) "VARGIFY"
	     (if (> refargs 0)
		 (format #f "ARGIFY_~D" cargs)
		 (format #f "NARGIFY_~D" cargs)))
	 (car func) (car func))
    (if if-fnc
	(say "#endif~%"))))
	 
(for-each argify-func (reverse funcs))
(if (not (null? extra-funcs)) (with-extra say (lambda () (for-each argify-func (reverse extra-funcs)))))
(if (not (null? deprecated-funcs)) (with-deprecated say (lambda () (for-each argify-func (reverse deprecated-funcs)))))

(define (ruby-cast func) (say "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-cast (reverse casts))
(if (not (null? extra-casts)) (with-extra say (lambda () (for-each ruby-cast (reverse extra-funcs)))))
(if (not (null? deprecated-casts)) (with-deprecated say (lambda () (for-each ruby-cast (reverse deprecated-casts)))))

(define (ruby-check func) (say "XEN_NARGIFY_1(XEN_~A_p_w, XEN_~A_p)~%" (no-stars (cadr func)) (no-stars (cadr func)))) 
(for-each ruby-check (reverse checks))
(if (not (null? extra-checks)) (with-extra say (lambda () (for-each ruby-check (reverse extra-checks)))))
(if (not (null? deprecated-checks)) (with-deprecated say (lambda () (for-each ruby-check (reverse deprecated-checks)))))

(for-each (lambda (field) (say "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" field field)) struct-fields)
(if worry-about-gtk-1 (say "#if (!HAVE_GTK_1)~%"))
(for-each (lambda (struct) 
	    (let* ((s (find-struct struct)))
	      (if (> (length (cadr s)) 0)
		  (say "XEN_VARGIFY(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
		  (say "XEN_NARGIFY_0(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
 (reverse make-structs))
(if worry-about-gtk-1 (say "#endif~%"))
(say "~%")
;;; ---------------- end Ruby step 1 ----------------

(hey "static XEN c_array_to_xen_list(XEN val, XEN clen);~%")
(hey "static XEN xen_list_to_c_array(XEN val, XEN type);~%~%")

(check-gtk1 #t)
(hey "#if HAVE_GUILE~%")
(say-hey "static void define_functions(void)~%")
(say-hey "{~%")
(say-hey "  #define XG_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XG_PRE #Name XG_POST, Value, A1, A2, A3, Help)~%")

(say-hey "  xm_gc_table = XEN_MAKE_VECTOR(1, XEN_FALSE);~%")
(say-hey "  XEN_PROTECT_FROM_GC(xm_gc_table);~%")
(say-hey "  xm_protected_size = 512;~%")
(say-hey "  xm_protected = XEN_MAKE_VECTOR(xm_protected_size, XEN_FALSE);~%")
(say-hey "  XEN_VECTOR_SET(xm_gc_table, 0, xm_protected);~%~%")

(hey "  XG_DEFINE_PROCEDURE(c-array->list, c_array_to_xen_list, 2, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(list->c-array, xen_list_to_c_array, 2, 0, 0, NULL);~%~%")

(define (defun func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs))
	 (if-fnc (and (> (length func) 4)
		      (eq? (list-ref func 4) 'if))))
    (check-gtk1 (car func))
    (if if-fnc
	(say-hey "#if HAVE_~A~%" (string-upcase (symbol->string (list-ref func 5)))))
    (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A, ~D, ~D, ~D, H_~A);~%"
		     (car func) (car func) 
		     (if (>= cargs 10) 0 args)
		     (if (>= cargs 10) 0 refargs) ; optional ignored
		     (if (>= cargs 10) 1 0)
		     (car func))
    (say "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, ~D, ~D, ~D, H_~A);~%"
		     (car func) (car func) 
		     (if (>= cargs 10) 0 args)
		     (if (>= cargs 10) 0 refargs) ; optional ignored
		     (if (>= cargs 10) 1 0)
		     (car func))
    (if if-fnc (say-hey "#endif~%"))))

(for-each defun (reverse funcs))
(if (not (null? extra-funcs)) (with-extra say-hey (lambda () (for-each defun (reverse extra-funcs)))))
(if (not (null? deprecated-funcs)) (with-deprecated say-hey (lambda () (for-each defun (reverse deprecated-funcs)))))

(define (cast-out func)
  (check-gtk1 (no-arg (car func)))
  (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func)))
  (say "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))

(for-each cast-out (reverse casts))
(if (not (null? extra-casts)) (with-extra say-hey (lambda () (for-each cast-out (reverse extra-casts)))))
(if (not (null? deprecated-casts)) (with-deprecated say-hey (lambda () (for-each cast-out (reverse deprecated-casts)))))

(define (check-out func)
  (check-gtk1 (no-arg-or-stars (cadr func)))
  (hey "  XG_DEFINE_PROCEDURE(~A, XEN_~A_p, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-stars (cadr func)))
  (say "  XG_DEFINE_PROCEDURE(~A, XEN_~A_p_w, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-stars (cadr func))))

(for-each check-out (reverse checks))
(if (not (null? extra-checks)) (with-extra say-hey (lambda () (for-each check-out (reverse extra-checks)))))
(if (not (null? deprecated-checks)) (with-deprecated say-hey (lambda () (for-each check-out (reverse deprecated-checks)))))

(check-gtk1 #t)
(say-hey "}~%~%")
(hey "#endif~%")


(hey "/* ---------------------------------------- structs ---------------------------------------- */~%~%")

(define (array->list type)
  (check-gtk1 (no-arg-or-stars (deref-type (list type))))
  (hey "  if (strcmp(ctype, ~S) == 0)~%" (no-stars type))
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)XEN_TO_C_ULONG(XEN_CADR(val)); ~%" type type)
  (hey "      for (i = len - 1; i >= 0; i--) result = XEN_CONS(C_TO_XEN_~A(arr[i]), result);~%" (no-stars (deref-type (list type))))
  (hey "    }~%"))

(define (list->array type)
  (check-gtk1 (no-arg-or-stars (deref-type (list type))))
  (hey "  if (strcmp(ctype, ~S) == 0)~%" type)
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)CALLOC(len, sizeof(~A));~%" type type (deref-type (list type)))
  (hey "      for (i = 0; i < len; i++, val = XEN_CDR(val)) arr[i] = XEN_TO_C_~A(XEN_CAR(val));~%" (no-stars (deref-type (list type))))
  (hey "      return(XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(~S), C_TO_XEN_ULONG((unsigned long)arr), make_xm_obj(arr)));~%" (no-stars type))
  (hey "    }~%"))

(hey "/* conversions */~%")
(hey "static XEN c_array_to_xen_list(XEN val, XEN clen)~%")
(hey "{~%")
(hey "  XEN result = XEN_EMPTY_LIST;~%")
(hey "  int i, len;~%")
(hey "  char *ctype;~%")
(hey "  len = XEN_TO_C_INT(clen);~%")
(hey "  if (!(XEN_LIST_P(val))) return(XEN_FALSE); /* type:location cons */~%")
(hey "  ctype = XEN_SYMBOL_TO_C_STRING(XEN_CAR(val));~%")
(for-each array->list listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (member (deref-type (list type)) types))
       (array->list type)))
 types)
;;; gotta handle GList* by hand
(hey "  if (strcmp(ctype, \"GList_\") == 0)~%")
(hey "    { /* tagging these pointers is currently up to the caller */~%")
(hey "      GList* lst;~%")
(hey "      lst = (GList*)XEN_TO_C_ULONG(XEN_CADR(val));~%")
(hey "      len = g_list_length(lst);~%")
(hey "      for (i = len - 1; i >= 0; i--) result = XEN_CONS(C_TO_XEN_ULONG(g_list_nth_data(lst, i)), result);~%")
(hey "    }~%")
(check-gtk1 #t)
(hey "  return(result);~%")
(hey "}~%~%")

(hey "static XEN xen_list_to_c_array(XEN val, XEN type)~%")
(hey "{~%")
(hey "  int i, len;~%")
(hey "  char *ctype;~%")
(hey "  len = XEN_LIST_LENGTH(val);~%")
(hey "  ctype = XEN_TO_C_STRING(type);~%")
(for-each list->array listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (member (deref-type (list type)) types))
       (list->array type)))
 types)
(check-gtk1 #t)
(hey "  return(XEN_FALSE);~%")
(hey "}~%~%")

(for-each
 (lambda (field)
   ;; gather structs that share field
   ;; if 1 or 2 assert type, if and return,
   ;;   else if on each, assert 0 at end and xen false
   (hey "~%")
   (check-gtk1 field)
   (hey "static XEN gxg_~A(XEN ptr)~%" field)
   (hey "{~%")
   (let ((vals '()))
     (for-each
      (lambda (struct)
	(let ((strs (cadr struct)))
	  ;; cadr of each inner list is field name, car is field type
	  (for-each
	   (lambda (str)
	     (if (string=? (cadr str) field)
		 (set! vals (cons (list (car struct) (car str)) vals))))
	   strs)))
      structs)
     ;; now vals is list of (struct-type field-type)
     (if (null? vals)
	 (hey "~A: not found" field)
	 (begin
	   (if (= (length vals) 1)
	       (hey "  XEN_ASSERT_TYPE(XEN_~A__P(ptr), ptr, XEN_ONLY_ARG, ~S, ~S);~%" 
		    (caar vals) field 
		    (caar vals))
	       (if (= (length vals) 2)
		   (hey "  XEN_ASSERT_TYPE(XEN_~A__P(ptr) || XEN_~A__P(ptr), ptr, XEN_ONLY_ARG, ~S, ~S \" or \" ~S);~%" 
			(caar vals) (car (cadr vals)) field 
			(caar vals) (car (cadr vals)))))))
     (let ((ctr 0))
       (for-each
	(lambda (val)
	  (let ((old-gtk1 in-gtk1))
	    (if in-gtk1 (check-gtk1 (no-arg-or-stars (cadr val))))
	    (if (or (> (length vals) 2)
		    (and (= (length vals) 2)
			 (= ctr 0)))
		(hey "  if (XEN_~A__P(ptr)) " (car val))
		(heyc "  "))
	    (set! ctr (+ ctr 1))
	    (hey "return(C_TO_XEN_~A((~A)((XEN_TO_C_~A_(ptr))->~A)));~%"
		 (no-stars (cadr val)) (cadr val) (car val) field)
	    (if (and old-gtk1 (not in-gtk1)) (check-gtk1 #t))))
      vals))
     (if (> (length vals) 2)
	 (hey "  XEN_ASSERT_TYPE(0, ptr, XEN_ONLY_ARG, ~S, \"pointer to struct with ~A field\");~%"
			  field field))
     (hey "}~%")
     (check-gtk1 #t)
     ))
 (reverse struct-fields))

(if worry-about-gtk-1 (hey "#if (!HAVE_GTK_1)~%"))
(for-each
 (lambda (name)
   (let* ((struct (find-struct name))
	  (strs (cadr struct)))
     ;; cadr of each inner list is field name, car is field type
     (if (= (length strs) 0)
	 (begin
	   (hey "static XEN gxg_make_~A(void)~%" name)
	   (hey "{~%")
	   (hey "  ~A* result;~%" name)
	   (hey "  result = (~A*)CALLOC(1, sizeof(~A));~%" name name)
	   (hey "  return(XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(~S), C_TO_XEN_ULONG((unsigned long)result), make_xm_obj(result)));~%" 
		(string-append name "_"))
	   (hey "}~%~%"))
	 (begin
	   (hey "static XEN gxg_make_~A(XEN arglist)~%" name)
	   (hey "{~%")
	   (hey "  ~A* result;~%" name)
	   (hey "  int i, len;~%")
	   (hey "  result = (~A*)CALLOC(1, sizeof(~A));~%" name name)
	   (hey "  len = XEN_LIST_LENGTH(arglist);~%")
	   (hey "  for (i = 0; i < len; i++)~%")
	   (hey "    switch (i)~%")
	   (hey "      {~%")
	   (let ((ctr 0))
	     (for-each
	      (lambda (str)
		(let ((field-name (cadr str))
		      (field-type (car str)))
		  (hey "      case ~D: result->~A = XEN_TO_C_~A(XEN_LIST_REF(arglist, ~D));~%"
		       ctr field-name (no-stars field-type) ctr)
		  (set! ctr (1+ ctr))))
	      strs))
	   (hey "      }~%")
	   (hey "  return(XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(~S), C_TO_XEN_ULONG((unsigned long)result), make_xm_obj(result)));~%" 
		(string-append name "_"))
	   (hey "}~%~%")))))
 (reverse make-structs))
(if worry-about-gtk-1 (hey "#endif~%~%"))

(hey "#if HAVE_GUILE~%")
(say-hey "static void define_structs(void)~%")
(say-hey "{~%~%")
(say-hey "  #define XGS_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XG_FIELD_PRE #Name XG_POST, Value, A1, A2, A3, Help)~%")

(for-each 
 (lambda (field)
   (check-gtk1 field)
   (hey "  XGS_DEFINE_PROCEDURE(~A, gxg_~A, 1, 0, 0, NULL);~%" field field)
   (say "  XGS_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, NULL);~%" field field))
 struct-fields)

(if worry-about-gtk-1 (hey "#if (!HAVE_GTK_1)~%"))
(for-each 
 (lambda (struct)
   (let* ((s (find-struct struct)))

     (hey "  XGS_DEFINE_PROCEDURE(~A, gxg_make_~A, 0, 0, ~D, NULL);~%" struct struct (if (> (length (cadr s)) 0) 1 0))
     (say "  XGS_DEFINE_PROCEDURE(~A, gxg_make_~A_w, 0, 0, ~D, NULL);~%" struct struct (if (> (length (cadr s)) 0) 1 0))))
 (reverse make-structs))
(if worry-about-gtk-1 (hey "#endif~%~%"))

(say-hey "}~%~%")
(hey "#else~%")
(hey "  #include \"xg-ruby.c\"~%")
(hey "#endif~%")


(hey "/* ---------------------------------------- macros ---------------------------------------- */~%~%")
(hey "static void define_macros(void)~%")
(hey "{~%")
(if worry-about-gtk-1 (hey "#if (!HAVE_GTK_1)~%"))
(with-deprecated hey
		 (lambda ()
		   (for-each
		    (lambda (mac)
		      (hey "  XEN_EVAL_C_STRING(\"(define \" XG_PRE \"~A\" XG_POST \" \" XG_PRE \"~A\" XG_POST \")\");~%"
			   (car mac) (cadr mac)))
		    (reverse vars))))
(if worry-about-gtk-1 (hey "#endif~%"))
(hey "}~%~%")

(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_INTEGER(Name) scm_c_define(XG_PRE #Name XG_POST, C_TO_XEN_INT(Name))~%")
(hey "  #define DEFINE_ULONG(Name) scm_c_define(XG_PRE #Name XG_POST, C_TO_XEN_ULONG(Name))~%")
(hey "#else~%")
(hey "  #define DEFINE_INTEGER(Name) gh_define(XG_PRE #Name XG_POST, C_TO_XEN_INT(Name))~%")
(hey "  #define DEFINE_ULONG(Name) gh_define(XG_PRE #Name XG_POST, C_TO_XEN_ULONG(Name))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_INTEGER(Name) rb_define_global_const(XG_PRE #Name XG_POST, C_TO_XEN_INT(Name))~%")
(hey "  #define DEFINE_ULONG(Name) rb_define_global_const(XG_PRE #Name XG_POST, C_TO_XEN_ULONG(Name))~%")
(hey "#endif~%")
(hey "~%")
(if worry-about-gtk-1 (hey "#if (!HAVE_GTK_1)~%"))
(hey "  g_type_init();~%")
(if worry-about-gtk-1 (hey "#endif~%~%"))

(for-each 
 (lambda (val) 
   (check-gtk1 val) 
   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse ints))

(for-each 
 (lambda (vals)
   (let ((val (car vals)))
     (check-gtk1 val) 
     (if (eq? (cadr vals) 'if)
	 (hey "#if HAVE_~A~%" (string-upcase (symbol->string (caddr vals)))))
     (hey "  DEFINE_ULONG(~A);~%" val)
     (if (eq? (cadr vals) 'if)
	 (hey "#endif~%"))))
 (reverse ulongs))

(check-gtk1 #t)
(if (not (null? extra-ints)) 
    (with-extra hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse extra-ints)))))
(if (not (null? deprecated-ints))
    (with-deprecated hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse deprecated-ints)))))

(check-gtk1 #t)
(hey "}~%~%")

(hey "static void define_doubles(void)~%")
(hey "{~%~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_DOUBLE(Name) scm_c_define(XG_PRE #Name XG_POST, C_TO_XEN_DOUBLE(Name))~%")
(hey "#else~%")
(hey "  #define DEFINE_DOUBLE(Name) gh_define(XG_PRE #Name XG_POST, C_TO_XEN_DOUBLE(Name))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_DOUBLE(Name) rb_define_global_const(XG_PRE #Name XG_POST, C_TO_XEN_DOUBLE(Name))~%")
(hey "#endif~%")
(hey "~%")

(for-each
 (lambda (val)
   (check-gtk1 val)
   (hey "  DEFINE_DOUBLE(~A);~%" val))
 (reverse dbls))
(check-gtk1 #t)
(hey "}~%~%")


(hey "/* -------------------------------- predefined Atoms -------------------------------- */~%")
(hey "~%")
(hey "static void define_atoms(void)~%")
(hey "{~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_ATOM(Name) scm_permanent_object(scm_c_define(XG_PRE #Name XG_POST, C_TO_XEN_GdkAtom(Name)))~%")
(hey "#else~%")
(hey "  #define DEFINE_ATOM(Name) gh_define(XG_PRE #Name XG_POST, C_TO_XEN_GdkAtom(Name))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_ATOM(Name) rb_define_global_const(XG_PRE #Name XG_POST, C_TO_XEN_GdkAtom(Name))~%")
(hey "#endif~%~%")

(for-each
 (lambda (atom)
   (check-gtk1 atom)
   (hey "  DEFINE_ATOM(~A);~%" atom))
 (reverse atoms))
(check-gtk1 #t)
(hey "}~%~%")


(hey "/* -------------------------------- strings -------------------------------- */~%")
(hey "~%")
(hey "static void define_strings(void)~%")
(hey "{~%")
(hey "  ~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_STRING(Name) scm_c_define(XG_PRE #Name XG_POST, scm_makfrom0str(Name))~%")
(hey "#else~%")
(hey "  #define DEFINE_STRING(Name) gh_define(XG_PRE #Name XG_POST, scm_makfrom0str(Name))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_STRING(Name) rb_define_global_const(XG_PRE #Name XG_POST, C_TO_XEN_STRING(Name))~%")
(hey "#endif~%")

(for-each (lambda (str) (check-gtk1 str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings))
(check-gtk1 #t)
(if (not (null? extra-strings))
    (with-extra hey (lambda () (for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse extra-strings)))))
(hey "}~%~%")


(hey "/* -------------------------------- initialization -------------------------------- */~%~%")
(hey "static int xg_already_inited = 0;~%~%")
(hey "#if HAVE_GUILE~%")
(hey " XEN init_xm(void);~%")
(hey " XEN init_xm(void)~%")
(hey "#else~%")
(hey " XEN Init_libxm(void);~%")
(hey " XEN Init_libxm(void)~%")
(hey "#endif~%")
(hey "{~%")
(hey "  if (!xg_already_inited)~%")
(hey "    {~%")
(hey "      define_xm_obj();~%")
(hey "      define_integers();~%")
(hey "      define_doubles();~%")
(hey "      define_functions();~%")
(hey "      define_structs();~%")
(hey "      define_macros();~%")
(hey "      define_structs();~%")
(hey "      define_atoms();~%")
(hey "      define_strings();~%")
(hey "      XEN_YES_WE_HAVE(\"xg\");~%")
(hey "#if HAVE_GUILE~%")
(hey "      XEN_EVAL_C_STRING(\"(define xm-version \\\"~A\\\")\");~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "#endif~%")
(hey "      xg_already_inited = 1;~%")
(hey "    }~%")
(hey "  return(XEN_FALSE);~%")
(hey "}~%")

;(hey "~A~%~A~%" funcs struct-fields)

(close-output-port xg-file)
(close-output-port xg-ruby-file)

#!
(define gad (open-output-file "gad"))

(use-modules (ice-9 popen) (ice-9 rdelim))
(define (shell cmd)
  (with-output-to-string
    (lambda ()
      (let ((in-port (open-input-pipe cmd)))
	(let loop ((line (read-line in-port 'concat)))
	  (or (eof-object? line)
	      (begin
		(display line)
		(loop (read-line in-port 'concat)))))))))
(define (gtk1? func)
  (let ((name (no-arg-or-stars (if (string? func) func (car func)))))
    (let ((loc (shell (format #f "fgrep ~A /usr/local/include/gtk/*.h" name))))
      (if (and (string? loc)
	       (> (string-length loc) 0))
	  (display (format #f "~S " name) gad)
	  (begin
	    (set! loc (shell (format #f "fgrep ~A /usr/local/include/gdk/*.h" name)))
	    (if (and (string? loc)
		     (> (string-length loc) 0))
		(display (format #f "~S " name) gad)))))))

(for-each gtk1? names)
(for-each gtk1? types)
(if include-deprecated (for-each gtk1? deprecated-types))
(for-each gtk1? struct-fields)
(close-output-port gad)
!#

;/* cc -c xg.c -g3 -DUSE_GTK -DDEBUGGING -DDEBUG_MEMORY -DLINUX -DUSE_SND -DHAVE_GNU_LIBC_VERSION_H -DHAVE_GSL -DHAVE_DLFCN_H -DHAVE_GUILE -DHAVE_LLONGS -DHAVE_APPLICABLE_SMOB -DHAVE_SCM_REMEMBER_UPTO_HERE -DHAVE_SCM_OBJECT_TO_STRING -DHAVE_SCM_NUM2LONG_LONG -DHAVE_SCM_C_MAKE_VECTOR -DHAVE_SCM_C_DEFINE -DHAVE_SCM_NUM2INT -DHAVE_SCM_C_DEFINE_GSUBR -DHAVE_SCM_LIST_N -DHAVE_SCM_C_EVAL_STRING -DHAVE_SCM_STR2SYMBOL -DHAVE_SCM_MAKE_REAL -DHAVE_SCM_T_CATCH_BODY -DHAVE_EXTENSION_LANGUAGE -DHAVE_STATIC_XM -DHAVE_GTK2 -I/home/bil/test/g3/include -I/home/bil/test/g3/include/glib-2.0 -I/home/bil/test/g3/include/pango-1.0 -I/home/bil/test/g3/include/gtk-2.0 -I/home/bil/test/g3/lib/gtk-2.0/include -I/home/bil/test/g3/include/atk-1.0 -I/home/bil/test/include -DPANGO_ENABLE_ENGINE -DPANGO_ENABLE_BACKEND */

;-DGDK_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED -DGDK_PIXBUF_DISABLE_DEPRECATED

