#!/usr/lib/snd/bin/guile -s
!#

;;; makexg.scm > xg.c creates the gtk2/gdk/pango/glib bindings using xgdata.scm

;;; TODO: Doc/Test/snd-motif-translations etc
;;; TODO: tie into libxm (configure.ac etc)
;;; TODO: g_signal handlers: "GtkSignalFunc" also "GtkEmissionHook"
;;;       lambda 4(etc) arg case: void hsc(GtkWidget *w, gint row, gint column, GdkEventButton *event, gpointer context) ; from clist -- obsolete?
;;; TODO: struct (and listable) print
;;; TODO: event casts?
;;;   GdkColor|GC|GCValues|Event* make?
;;; TODO: add gdk-pixbuf? (has GDK_PIXBUF_DISABLE_DEPRECATED)
;;; TODO: add unicode handlers from glib
;;; TODO: unprotect *_remove?

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(use-modules (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(define (hey . args)
  (display (apply format #f args)))

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
;;; "broken" for obsolete gdk/gtk funcs
(define broken-types '())
(define broken-funcs '())
(define broken-casts '())
(define broken-checks '())
(define broken-ints '())
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

(define (ref-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (= (length arg) 3)
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
			(if (char=? (string-ref given-name 0) #\[) 
			    (begin
			      (set! reftype (deref-type (list type)))
			      (set! data (cons (list type 
						     (substring given-name 1 (- (string-length given-name) 1))
						     given-name) 
					       data)))
			    (set! data (cons (list type given-name) data)))
			(if reftype (set! type reftype))

			(if (eq? broken 'broken)
			    (if (and (not (member type types))
				     (not (member type deprecated-types))
				     (not (member type broken-types)))
				(set! broken-types (cons type broken-types)))
			    (if (eq? broken 'deprecated)
				(if (and (not (member type types))
					 (not (member type deprecated-types)))
				    (set! deprecated-types (cons type deprecated-types)))
				(if (eq? broken 'extra)
				    (if (and (not (member type types))
					     (not (member type extra-types)))
					(set! extra-types (cons type extra-types)))
				    (if (not (member type types))
					(set! types (cons type types))))))
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
	(cons "guint" "ULONG")
	(cons "guint16" "INT")
	(cons "gint" "INT")
	(cons "gshort" "INT")
	(cons "gint16" "INT")
	(cons "guint8" "INT")
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
	
	;; deprecated
	(cons "GtkCellType" "INT")
	(cons "GtkCTreeLineStyle" "INT")
	(cons "GtkCTreeExpanderStyle" "INT")
	(cons "GtkScrollType" "INT")
	(cons "GtkPreviewType" "INT")
	(cons "GtkProgressBarStyle" "INT")
	(cons "GtkVisibility" "INT")
	(cons "GtkSignalRunType" "INT")
	(cons "GdkWChar" "ULONG")

	;; broken
	(cons "GtkTreeViewMode" "INT")
	))

(define (type-it type)
  (let ((typ (assoc type direct-types)))
    (if typ
	(if (cdr typ)
	    (begin
	      (if (string? (cdr typ))
		  (begin
		    (hey "#define C_TO_XEN_~A(Arg) C_TO_XEN_~A(Arg)~%" (no-stars (car typ)) (cdr typ))
		    (hey "#define XEN_TO_C_~A(Arg) XEN_TO_C_~A(Arg)~%" (no-stars (car typ)) (cdr typ))
		    (hey "#define XEN_~A_P(Arg) XEN_~A_P(Arg)~%" 
			 (no-stars (car typ))
			 (if (string=? (cdr typ) "INT") 
			     "INTEGER" 
			     (if (string=? (cdr typ) "DOUBLE")
				 "NUMBER"
				 (cdr typ)))))
		  (begin
		    (hey "#define XEN_~A_P(Arg) 1~%" (no-stars (car typ)))
		    (hey "#define XEN_TO_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ)))))))

	(if (and (not (string=? type "lambda"))
		 (not (string=? type "lambda_data"))
		 (not (find-callback 
		       (lambda (func)
			 (string=? type (symbol->string (car func))))))
		 (not (string=? type "GtkSignalFunc")))

	    (hey "XM_TYPE~A(~A, ~A)~%" 
		 (if (has-stars type) "" "_UNCHECKED")
		 (no-stars type) 
		 type)))))

(define (CNAM name alias)
  (if (assoc name names)
      (hey "~A CNAM~%" name)
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

(define (CFNC data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(hey "~A CFNC~%" name)
	(let ((type (car-str data)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args 'ok)))
	    (set! funcs (cons (list name type strs args) funcs))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define (CFNC-if data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(hey "~A CFNC-if~%" name)
	(let ((type (car-str data)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args 'ok)))
	    (set! funcs (cons (list name type strs args 'if) funcs))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define (CFNC-broken data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(hey "~A CFNC-broken~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type deprecated-types))
		   (not (member type broken-types)))
	      (set! broken-types (cons type broken-types)))
	  (let ((strs (parse-args args 'broken)))
	    (set! broken-funcs (cons (list name type strs args) broken-funcs))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define (CFNC-dep data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(hey "~A CFNC-deprecated~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type deprecated-types)))
	      (set! deprecated-types (cons type deprecated-types)))
	  (let ((strs (parse-args args 'deprecated)))
	    (set! deprecated-funcs (cons (list name type strs args) deprecated-funcs))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define (CFNC-extra data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(hey "~A CFNC-extra~%" name)
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
    (display initial)
    (do ((i 0 (1+ i)))
	((= i len))
      (if (char=? (string-ref args i) #\space)
	  (if typed
	      (begin
		(display ", ")
		(set! line-len (+ line-len 2))
		(if (> line-len help-max)
		    (begin
		      (hey "\\~%")
		      (set! line-len 0)))
		(set! typed #f))
	      (begin
		(set! line-len (1+ line-len))
		(display " ")
		(set! typed #t)))
	  (begin
	    (set! line-len (1+ line-len))
	    (display (string-ref args i)))))
    (hey ")\"~%")))

(define (CATOM name)
  (if (assoc name names)
      (hey "~A CATOM~%" name)
      (begin
	(set! atoms (cons name atoms))
	(set! names (cons (cons name 'atom) names)))))

(define (CSTR name)
  (if (assoc name names)
      (hey "~A CSTR~%" name)
      (begin
	(set! strings (cons name strings))
	(set! names (cons (cons name 'string) names)))))

(define (CSTR-extra name)
  (if (assoc name names)
      (hey "~A CSTR~%" name)
      (begin
	(set! extra-strings (cons name extra-strings))
	(set! names (cons (cons name 'string) names)))))

(define (CDBL name)
  (if (assoc name names)
      (hey "~A CDBL~%" name)
      (begin
	(set! dbls (cons name dbls))
	(set! names (cons (cons name 'dbl) names)))))

(define (CLNG name)
  (if (assoc name names)
      (hey "~A CLNG~%" name)
      (begin
	(set! ulongs (cons name ulongs))
	(set! names (cons (cons name 'ulong) names)))))

(define (CINT name)
  (if (assoc name names)
      (hey "~A CINT~%" name)
      (begin
	(set! ints (cons name ints))
	(set! names (cons (cons name 'int) names)))))

(define (CINT-extra name)
  (if (assoc name names)
      (hey "~A CINT-extra~%" name)
      (begin
	(set! extra-ints (cons name extra-ints))
	(set! names (cons (cons name 'int) names)))))

(define (CINT-broken name)
  (if (assoc name names)
      (hey "~A CINT-broken~%" name)
      (begin
	(set! broken-ints (cons name broken-ints))
	(set! names (cons (cons name 'int) names)))))

(define (CINT-dep name)
  (if (assoc name names)
      (hey "~A CINT-deprecated~%" name)
      (begin
	(set! deprecated-ints (cons name deprecated-ints))
	(set! names (cons (cons name 'int) names)))))


(define (CDEF name type) ; this is the cast (type *)obj essentially but here it's (list type* (cadr obj))
  (if (assoc name names)
      (hey "~A CDEF~%" name)
      (begin
	;;(if (not (member type types))
	;;    (set! types (cons type types)))
	(set! casts (cons (list name type) casts))
	(set! names (cons (cons name 'def) names)))))

(define (CDEF1 name type) ; this is the check = (eq? type (car obj))
  (if (assoc name names)
      (hey "~A CDEF1~%" name)
      (begin
	(if (not (member type check-types))
	    (set! check-types (cons type check-types)))
	(set! checks (cons (list name type) checks))
	(set! names (cons (cons name 'def) names)))))

(define (CDEF-broken name type)
  (if (assoc name names)
      (hey "~A CDEF-broken~%" name)
      (begin
	(if (and (not (member type types))
		 (not (member type broken-types)))
	    (set! broken-types (cons type broken-types)))
	(set! broken-casts (cons (list name type) broken-casts))
	(set! names (cons (cons name 'def) names)))))

(define (CDEF1-broken name type)
  (if (assoc name names)
      (hey "~A CDEF1~%" name)
      (begin
	(if (and (not (member type types))
		 (not (member type broken-types)))
	    (set! broken-types (cons type broken-types)))
	(set! broken-checks (cons (list name type) broken-checks))
	(set! names (cons (cons name 'def) names)))))

(define (CDEF-dep name type)
  (if (assoc name names)
      (hey "~A CDEF-deprecated~%" name)
      (begin
	(if (and (not (member type types))
		 (not (member type deprecated-types)))
	    (set! deprecated-types (cons type deprecated-types)))
	(set! deprecated-casts (cons (list name type) deprecated-casts))
	(set! names (cons (cons name 'def) names)))))

(define (CDEF1-dep name type)
  (if (assoc name names)
      (hey "~A CDEF1~%" name)
      (begin
	(if (and (not (member type types))
		 (not (member type deprecated-types)))
	    (set! deprecated-types (cons type deprecated-types)))
	(set! deprecated-checks (cons (list name type) deprecated-checks))
	(set! names (cons (cons name 'def) names)))))

(define (CDEF-extra name type)
  (if (assoc name names)
      (hey "~A CDEF-extra~%" name)
      (begin
	(if (and (not (member type types))
		 (not (member type extra-types)))
	    (set! extra-types (cons type extra-types)))
	(set! extra-casts (cons (list name type) extra-casts))
	(set! names (cons (cons name 'def) names)))))

(define (CDEF1-extra name type)
  (if (assoc name names)
      (hey "~A CDEF1~%" name)
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
	(hey "~A STRUCT~%" name)
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

(hey "/* xg.c: Guile (and someday Ruby) bindings for gdk/gtk/pango, some of glib~%")
(hey " *   generated automatically from makexg.scm and xgdata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " *   GDK_DISABLE_DEPRECATED and GTK_DISABLE_DEPRECATED are handled together~%")
(hey " *   GDK_ENABLE_BROKEN and GTK_ENABLE_BROKEN are handled together~%")
(hey " *   PANGO_ENABLE_ENGINE and PANGO_ENABLE_BACKEND are handled together, and may be removed later~%")
(hey " *~%")
(hey " *   other flags:~%")

(for-each
 (lambda (func)
   (if (and (> (length func) 4)
	    (eq? (list-ref func 4) 'if))
       (hey " *     HAVE_~A~%" (string-upcase (car func)))))
 funcs)

(hey " *~%")
(hey " * reference args are ignored if passed, resultant values are returned in a list, marked with \"[]\" in help strings~%")
(hey " *~%")
(hey " * added funcs:~%")
(hey " *    (xm-version) -> date string.~%")
(hey " *    (c-array->list arr len) derefs each member of arr, returning lisp list~%")
(hey " *    (list->c-array lst ctype) packages each member of list as c-type \"type\" returning (wrapped) c array~%")

(for-each
 (lambda (name)
   (let ((args (cadr (find-struct name))))
     (if (> (length args) 0)
	 (hey " *    (|~A #:optional" name)
	 (hey " *    (|~A" name))
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
(hey " * TODO:~%")
(hey " *     check out the g_signal handlers: GtkSignalFunc also GtkEmissionHook (gtk_signal_* should be ok)~%")
(hey " *     GdkEvent casts~%")
(hey " *     struct print, more struct instance creators(?)~%")
(hey " *     tie into libxm (configure.ac etc), Snd (snd-motif translation)~%")
(hey " *     add gdk-pixbuf? (has GDK_PIXBUF_DISABLE_DEPRECATED)~%")
(hey " *     add unicode handlers from glib -- anything else?~%")
(hey " *     unprotect *_remove, unprotect old upon reset callback~%")
(hey " *     document/test (libxm|grfsnd.html, snd-test.scm)~%")
(hey " *     add Ruby linkages~%")
(hey " *~%")
(hey " * HISTORY:~%")
(hey " *     11-Feb-02: initial version.~%")
(hey " */~%~%")

(hey "#if defined(HAVE_CONFIG_H)~%  #include \"config.h\"~%#endif~%~%")

(hey "#include <glib.h>~%")
(hey "#include <gdk/gdk.h>~%")
(hey "#include <gdk/gdkkeysyms.h>~%")
(hey "#include <gtk/gtk.h>~%")
(hey "#include <glib-object.h>~%")
(hey "#include <pango/pango.h>~%~%")
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
(hey "static size_t xm_obj_free(XEN obj)~%")
(hey "{~%")
(hey "  FREE((void *)XEN_OBJECT_REF(obj));~%")
(hey "  return(sizeof(void *));~%")
(hey "}~%")
(hey "static XEN make_xm_obj(void *ptr)~%")
(hey "{~%")
(hey "  XEN_MAKE_AND_RETURN_OBJECT(xm_obj_tag, ptr, 0, xm_obj_free);~%")
(hey "}~%")
(hey "static void define_xm_obj(void)~%")
(hey "{~%")
(hey "  xm_obj_tag = XEN_MAKE_OBJECT_TYPE(\"XmObj\", sizeof(void *));~%")
(hey "  scm_set_smob_free(xm_obj_tag, xm_obj_free);~%")
(hey "}  ~%")
(hey "~%")

(hey "#define XG_PRE \"|\"~%#define XG_POST \"\"~%~%")
(hey "#define WRAP_FOR_XEN_UNCHECKED(Name, Value) \\~%")
(hey "  XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(Name), C_TO_XEN_ULONG((unsigned long)Value))~%~%")
(hey "#define WRAP_FOR_XEN(Name, Value) \\~%")
(hey "  ((Value) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(Name), C_TO_XEN_ULONG((unsigned long)Value)) : XEN_FALSE)~%~%")
(hey "#define UNWRAP_FOR_C(Value) XEN_TO_C_ULONG(XEN_CADR(Value))~%")
(hey "#define WRAP_P(Name, Value) (XEN_LIST_P(Value) &&\\~%")
(hey "                            (XEN_LIST_LENGTH(Value) >= 2) &&\\~%")
(hey "                            (XEN_SYMBOL_P(XEN_CAR(Value))) &&\\~%")
(hey "                            (strcmp(Name, XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))~%~%")
(hey "#define XM_TYPE(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)UNWRAP_FOR_C(val));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%~%")
(hey "#define XM_TYPE_UNCHECKED(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN_UNCHECKED(#Name, val));} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)UNWRAP_FOR_C(val));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%~%")

(hey "~%/* type checks for callback wrappers */~%")

(for-each 
 (lambda (func)
   (hey "#define XEN_~A_P(Arg)  XEN_PROCEDURE_P(Arg) && (XEN_REQUIRED_ARGS(Arg) == ~D)~%"
	(symbol->string (callback-name func))
	(length (callback-args func))))
 callbacks)

(hey "#define XEN_lambda_P(Arg) XEN_PROCEDURE_P(Arg)~%")
(hey "#define XEN_GtkSignalFunc_P(Arg) XEN_PROCEDURE_P(Arg) && (XEN_REQUIRED_ARGS(Arg) == 2)~%")

(for-each
 (lambda (func)
   (hey "#define XEN_TO_C_~A(Arg) gxg_~A~%"
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
(hey "#define XEN_TO_C_GdkFilterReturn(Arg) XEN_TO_C_INT(Arg)~%")


(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(for-each type-it (reverse types))

(define (check-type-it type)
  (hey "static XEN XEN_~A_p(XEN val) {return(C_TO_XEN_BOOLEAN(WRAP_P(~S, val)));}~%"
       (no-stars type) (no-stars type)))

(for-each check-type-it (reverse check-types))

(if (not (null? extra-types))
    (begin
      (hey "~%#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each type-it (reverse extra-types))
      (for-each check-type-it  (reverse extra-types))
      (hey "#endif~%~%")))

(if (not (null? deprecated-types))
    (begin
      (hey "~%/* -------------------- deprecated types -------------------- */~%")
      (hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
      (for-each type-it (reverse deprecated-types))
      (for-each check-type-it (reverse deprecated-types))
      (hey "#endif~%~%")))

(if (not (null? broken-types))
    (begin
      (hey "~%/* -------------------- obsolete types -------------------- */~%")
      (hey "#if GDK_ENABLE_BROKEN && GTK_ENABLE_BROKEN~%")
      (for-each type-it (reverse broken-types))
      (for-each check-type-it (reverse broken-types))
      (hey "#endif~%~%")))


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
	       (hey "XEN_CALL_~D(~A(func_data),~%"
		    (length args)
		    (if (eq? fname 'GtkClipboardClearFunc)
			"XEN_CADDR"
			"XEN_CAR"))
	       (for-each
		(lambda (arg)
		  (display (substring "                                                                   " 0 castlen))
		  (if (not (string=? (car arg) "lambda_data"))
		      (hey "C_TO_XEN_~A(~A),~%"
			   (no-stars (car arg))
			   (cadr arg))
		      (hey "XEN_CADR(func_data),~%")))
		args)
	       (display (substring "                                                                      " 0 castlen))
	       (hey "__FUNCTION__)")
	       (if void?
		   (hey ";~%")
		   (if (eq? gctype 'timeout)
		       (begin
			 (hey ";~%")
			 (hey "  if (XEN_BOOLEAN_P(result)) call_again = XEN_TO_C_BOOLEAN(result); else call_again = XEN_TO_C_INT_OR_ELSE(result, 0);~%")
			 (hey "  if (!call_again) xm_unprotect_at(XEN_TO_C_INT(XEN_CADDR(func_data)));~%")
			 (hey "  return(call_again);~%"))
		       (hey "));~%"))))
	     (hey "}~%~%")))))
   callbacks))

(hey "static void gxg_func3(GtkWidget *w, GdkEventAny *ev, gpointer data)~%")
(hey "{~%")
(hey "  XEN_CALL_3(XEN_CAR(data),~%")
(hey "             C_TO_XEN_GtkWidget_(w),~%")
(hey "             C_TO_XEN_GdkEventAny_(ev),~%")
(hey "             XEN_CADR(data),~%")
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
	 (display line)))

     (define (hey-ok arg)
       ;; cr ok after arg
       (set! line-len (+ line-len (string-length arg)))
       (display arg)
       (if (> line-len line-max)
	   (begin
	     (display (format #f "~%"))
	     (do ((i 0 (1+ i)))
		 ((= i arg-start))
	       (display " "))
	     (set! line-len arg-start))))

     (if (and (> (length data) 4)
	      (eq? (list-ref data 4) 'if))
	 (hey "#if HAVE_~A~%" (string-upcase name)))
     (hey "static XEN gxg_~A(" name)
     (if (= (length args) 0)
	 (display "void")
	 (if (>= (length args) max-args)
	     (display "XEN arglist")
	     (let ((previous-arg #f))
	       (for-each 
		(lambda (arg)
		  (let ((argname (cadr arg))
			(argtype (car arg)))
		    (if previous-arg (display ", "))
		    (set! previous-arg #t)
		    (hey "XEN ~A" argname)))
		args))))
     (hey ")~%{~%")
     (helpify name return-type argstr)
     (if (> refargs 0)
	 (for-each
	  (lambda (arg)
	    (if (= (length arg) 3)
		(hey "  ~A ~A;~%" (deref-type arg) (deref-name arg))))
	  args))
     (if (and (>= (length args) max-args)
	      (> xgargs 0))
	 (let ((previous-arg #f))
	   (hey "  XEN ")
	   (for-each
	    (lambda (arg)
	      (if (< (length arg) 3)
		  (begin
		    (if previous-arg (display ", "))
		    (set! previous-arg #t)
		    (hey "~A" (cadr arg)))))
	    args)
	   (hey ";~%")
	   (let ((ctr 0)) ; list-ref counts from 0
	     (for-each
	      (lambda (arg)
		(if (= (length arg) 2) ; ref args ignored
		    (hey "  ~A = XEN_LIST_REF(arglist, ~D);~%" (cadr arg) ctr))
		(set! ctr (1+ ctr)))
	      args))))
     (if (> (length args) 0)
	 (let ((ctr 1))
	   (for-each
	    (lambda (arg)
	      (let ((argname (cadr arg))
		    (argtype (car arg)))
		(if (= (length arg) 2)
		    (hey "  XEN_ASSERT_TYPE(XEN_~A_P(~A), ~A, ~D, ~S, ~S);~%"
			 (no-stars argtype) argname argname ctr name argtype))
		(set! ctr (1+ ctr))))
	    args)))
     (if (not (eq? lambda-type 'fnc))
	 (begin
	   (hey "  {~%")
	   (hey "    XEN result = XEN_FALSE;~%")
	   (if (or (eq? lambda-type 'GtkSignalFunc)
		   (and callback-data
			(or (eq? (callback-gc callback-data) 'temporary)
			    (eq? (callback-gc callback-data) 'semi-permanent)
			    (eq? (callback-gc callback-data) 'timeout))))
	       (hey "    int loc;~%"))
	   (hey "    XEN gxg_ptr = XEN_LIST_3(func, func_data, XEN_FALSE);~%")
	   (if (and callback-data
		    (or (eq? (callback-gc callback-data) 'temporary)
			(eq? (callback-gc callback-data) 'timeout)
			(eq? (callback-gc callback-data) 'semi-permanent)))
	       (hey "    loc = xm_protect(gxg_ptr);~%")
	       (hey "    xm_protect(gxg_ptr);~%"))
	   (if (and callback-data
		    (eq? (callback-gc callback-data) 'timeout))
	       (hey "    XEN_LIST_SET(gxg_ptr, 2, C_TO_XEN_INT(loc));~%")
	       (if (eq? lambda-type 'GtkClipboardGetFunc)
		   (hey "    XEN_LIST_SET(gxg_ptr, 2, clear_func);~%")))
	   (hey-start)
	   (if (and (not (string=? return-type "void"))
		    (not (eq? lambda-type 'lambda)))
	       (hey-on "    result = C_TO_XEN_~A(" (no-stars return-type))
	       (hey "    ")))
	 (begin
	   (if (and (> refargs 0)
		    (not (string=? return-type "void")))
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
		      (set! previous-arg #t)
		      (if (= (length arg) 3)
			  (hey-on "&~A" (deref-name arg))
			  (hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname))))
		  args)))
	   (if (not (eq? lambda-type 'fnc))
	       (if (not (string=? return-type "void")) 
		   (hey ")"))
	       (if (not (string=? return-type "void"))
		   (if (= refargs 0)
		       (hey "))")
		       (hey ")"))))
	   (hey ");~%")
	   (if (not (eq? lambda-type 'fnc))
	       (begin
		 (if (and callback-data
			  (eq? (callback-gc callback-data) 'temporary))
		     (hey "    xm_unprotect_at(loc);~%"))
		 (if (and callback-data
			  (eq? (callback-gc callback-data) 'semi-permanent))
		     (hey "    XEN_LIST_SET(gxg_ptr, 2, XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(\"idler\"), result, C_TO_XEN_INT(loc)));~%"))
		 (hey "    return(result);~%")
		 (hey "   }~%"))
	       (begin
		 (if (> refargs 0)
		     (let* ((restoo (not (string=? return-type "void")))
			    (previous-arg restoo))
		       (if restoo (hey "  "))
		       (hey "  return(XEN_LIST_~D(" (+ refargs (if restoo 1 0)))
		       (if restoo
			   (hey "result"))
		       (for-each 
			(lambda (arg)
			  (if (= (length arg) 3)
			      (begin
				(if previous-arg (hey ", "))
				(hey "C_TO_XEN_~A(~A)" (no-stars (deref-type arg)) (deref-name arg))
				(set! previous-arg #t))))
			args)
		       (hey "));~%")
		       (if restoo (hey "   }~%")))
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
		      (display "(GtkSignalFunc)gxg_func2")
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
		      (display "(GtkSignalFunc)gxg_func3")
		      (hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname))))
	      args))
	   (if (not (string=? return-type "void"))
	       (hey ")));~%  }~%")
	       (hey ");~%  return(result);~%  }~%"))))
     (hey "}~%")
     (if (and (> (length data) 4)
	      (eq? (list-ref data 4) 'if))
	 (hey "#endif~%"))
     (hey "~%"))))

(for-each handle-func (reverse funcs))

(if (not (null? extra-funcs))
    (begin
      (hey "#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each handle-func (reverse extra-funcs))
      (hey "#endif~%~%")))

(if (not (null? deprecated-funcs))
    (begin
      (hey "~%/* -------------------- deprecated functions -------------------- */~%")
      (hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
      (for-each handle-func (reverse deprecated-funcs))
      (hey "#endif~%/* -------------------- end deprecated functions -------------------- */~%~%~%")))

(if (not (null? broken-funcs))
    (begin
      (hey "~%/* -------------------- obsolete functions -------------------- */~%")
      (hey "#if GDK_ENABLE_BROKEN && GTK_ENABLE_BROKEN~%")
      (for-each handle-func (reverse broken-funcs))
      (hey "#endif~%/* -------------------- end obsolete functions -------------------- */~%~%~%")))

(define cast-it
 (lambda (cast)
   (let ((cast-name (car cast))
	 (cast-type (cadr cast)))
     (hey "static XEN gxg_~A(XEN obj)" (no-arg cast-name))
     (hey " {return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(~S), XEN_CADR(obj)));}~%" (no-stars cast-type)))))

(hey "~%~%/* ---------------------------------------- casts ---------------------------------------- */~%~%")
(for-each cast-it (reverse casts))

(if (not (null? extra-casts))
    (begin
      (hey "~%#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each cast-it (reverse extra-casts))
      (hey "#endif~%~%")))

(if (not (null? deprecated-casts))
    (begin
      (hey "~%  /* -------------------- deprecated casts -------------------- */~%")
      (hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
      (for-each cast-it (reverse deprecated-casts))
      (hey "#endif~%~%")))

(if (not (null? broken-casts))
    (begin
      (hey "~%  /* -------------------- obsolete casts -------------------- */~%")
      (hey "#if GDK_ENABLE_BROKEN && GTK_ENABLE_BROKEN~%")
      (for-each cast-it (reverse broken-casts))
      (hey "#endif~%~%")))

(hey "static void define_functions(void)~%")
(hey "{~%")

(hey "  xm_gc_table = XEN_MAKE_VECTOR(1, XEN_FALSE);~%")
(hey "  XEN_PROTECT_FROM_GC(xm_gc_table);~%")
(hey "  xm_protected_size = 512;~%")
(hey "  xm_protected = XEN_MAKE_VECTOR(xm_protected_size, XEN_FALSE);~%")
(hey "  XEN_VECTOR_SET(xm_gc_table, 0, xm_protected);~%~%")

(define (defun func)
  (let* ((cargs (length (caddr func)))
	 (refargs (ref-args (caddr func)))
	 (args (- cargs refargs))
	 (if-fnc (and (> (length func) 4)
		      (eq? (list-ref func 4) 'if))))
    (if if-fnc
	(hey "#if HAVE_~A~%" (string-upcase (car func))))
    (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, gxg_~A, ~D, ~D, ~D, H_~A);~%"
		     (car func) (car func) 
		     (if (>= cargs 10) 0 args)
		     (if (>= cargs 10) 0 refargs) ; optional ignored
		     (if (>= cargs 10) 1 0)
		     (car func))
    (if if-fnc (hey "#endif~%"))))

(for-each defun (reverse funcs))

(if (not (null? extra-funcs))
    (begin
      (hey "~%#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each defun (reverse extra-funcs))
      (hey "#endif~%~%")))

(if (not (null? deprecated-funcs))
    (begin
      (hey "~%  /* -------------------- deprecated functions -------------------- */~%")
      (hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
      (for-each defun (reverse deprecated-funcs))
      (hey "#endif~%~%")))

(if (not (null? broken-funcs))
    (begin
      (hey "~%  /* -------------------- obsolete functions -------------------- */~%")
      (hey "#if GDK_ENABLE_BROKEN && GTK_ENABLE_BROKEN~%")
      (for-each defun (reverse broken-funcs))
      (hey "#endif~%~%")))

(for-each 
 (lambda (func) 
   (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, gxg_~A, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))
 (reverse casts))

(if (not (null? extra-casts))
    (begin
      (hey "~%#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each 
       (lambda (func) 
	 (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, gxg_~A, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))
       (reverse extra-casts))
      (hey "#endif~%~%")))

(if (not (null? deprecated-casts))
    (begin
      (hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
      (for-each
       (lambda (func)
	 (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, gxg_~A, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))
       (reverse deprecated-casts))
      (hey "#endif~%~%")))

(if (not (null? broken-casts))
    (begin
      (hey "#if GDK_ENABLE_BROKEN && GTK_ENABLE_BROKEN~%")
      (for-each
       (lambda (func)
	 (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, gxg_~A, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))
       (reverse broken-casts))
      (hey "#endif~%~%")))

(for-each 
 (lambda (func) 
   (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, XEN_~A_p, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-stars (cadr func))))
 (reverse checks))

(if (not (null? extra-checks))
    (begin
      (hey "~%#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each 
       (lambda (func) 
	 (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, XEN_~A_p, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-stars (cadr func))))
       (reverse extra-checks))
      (hey "#endif~%~%")))

(if (not (null? deprecated-checks))
    (begin
      (hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
      (for-each
       (lambda (func)
	 (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, XEN_~A_p, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-stars (cadr func))))
       (reverse deprecated-checks))
      (hey "#endif~%~%")))

(if (not (null? broken-checks))
    (begin
      (hey "#if GDK_ENABLE_BROKEN && GTK_ENABLE_BROKEN~%")
      (for-each
       (lambda (func)
	 (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, XEN_~A_p, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-stars (cadr func))))
       (reverse broken-checks))
      (hey "#endif~%~%")))

(hey "}~%~%")


(hey "/* ---------------------------------------- structs ---------------------------------------- */~%~%")

(define (array->list type)
  (hey "  if (strcmp(ctype, ~S) == 0)~%" type)
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)XEN_CADR(val); ~%" type type)
  (hey "      for (i = len - 1; i >= 0; i--) result = XEN_CONS(C_TO_XEN_~A(arr[i]), result);~%" (no-stars (deref-type (list type))))
  (hey "    }~%"))

(define (list->array type)
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
(hey "  ctype = XEN_SYMBOL_TO_C_STRING(XEN_CAR(val));~%")
(for-each array->list listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (member (deref-type (list type)) types))
       (array->list type)))
 types)
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
(hey "  return(XEN_FALSE);~%")
(hey "}~%~%")

(for-each
 (lambda (field)
   ;; gather structs that share field
   ;; if 1 or 2 assert type, if and return,
   ;;   else if on each, assert 0 at end and xen false
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
	 (if (= (length vals) 1)
	     (hey "  XEN_ASSERT_TYPE(XEN_~A__P(ptr), ptr, XEN_ONLY_ARG, ~S, ~S);~%" 
			      (caar vals) field 
			      (caar vals))
	     (if (= (length vals) 2)
		 (hey "  XEN_ASSERT_TYPE(XEN_~A__P(ptr) || XEN_~A__P(ptr), ptr, XEN_ONLY_ARG, ~S, ~S \" or \" ~S);~%" 
				  (caar vals) (car (cadr vals)) field 
				  (caar vals) (car (cadr vals))))))
     (let ((ctr 0))
       (for-each
	(lambda (val)
	  (if (or (> (length vals) 2)
		  (and (= (length vals) 2)
		       (= ctr 1)))
	      (hey "  if (XEN_~A__P(ptr)) " (car val))
	      (hey "  "))
	  (set! ctr (+ ctr 1))
	  (hey "return(C_TO_XEN_~A((~A)((XEN_TO_C_~A_(ptr))->~A)));~%"
	       (no-stars (cadr val)) (cadr val) (car val) field))
      vals))
     (if (> (length vals) 2)
	 (hey "  XEN_ASSERT_TYPE(0, ptr, XEN_ONLY_ARG, ~S, \"pointer to struct with ~A field\");~%"
			  field field))
     (hey "}~%~%")))
 (reverse struct-fields))

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

(hey "static void define_structs(void)~%")
(hey "{~%~%")

(hey "  XEN_DEFINE_PROCEDURE(XG_PRE \"c-array->list\" XG_POST, c_array_to_xen_list, 2, 0, 0, NULL);~%")
(hey "  XEN_DEFINE_PROCEDURE(XG_PRE \"list->c-array\" XG_POST, xen_list_to_c_array, 2, 0, 0, NULL);~%~%")

(for-each 
 (lambda (field)
   (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, gxg_~A, 1, 0, 0, NULL);~%"
		    field field))
 struct-fields)

(for-each 
 (lambda (struct)
   (let* ((s (find-struct struct)))
     (hey "  XEN_DEFINE_PROCEDURE(XG_PRE ~S XG_POST, gxg_make_~A, 0, 0, ~D, NULL);~%"
	  struct struct
	  (if (> (length (cadr s)) 0) 1 0)))) 
 (reverse make-structs))

(hey "}~%~%")



(hey "/* ---------------------------------------- macros ---------------------------------------- */~%~%")
(hey "static void define_macros(void)~%")
(hey "{~%")

(hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
(for-each
 (lambda (mac)
   (hey "  XEN_EVAL_C_STRING(\"(define \" XG_PRE \"~A\" XG_POST \" \" XG_PRE \"~A\" XG_POST \")\");~%"
		    (car mac) (cadr mac)))
 (reverse vars))
(hey "#endif~%")

(hey "}~%~%")


(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_INTEGER(Name, Value) scm_c_define(Name, scm_long2num(Value))~%")
(hey "  #define DEFINE_ULONG(Name, Value) scm_c_define(Name, scm_ulong2num(Value))~%")
(hey "#else~%")
(hey "  #define DEFINE_INTEGER(Name, Value) gh_define(Name, scm_long2num(Value))~%")
(hey "  #define DEFINE_ULONG(Name, Value) gh_define(Name, scm_ulong2num(Value))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_INTEGER(Name, Value) rb_define_global_const(Name, C_TO_XEN_INT(Value))~%")
(hey "  #define DEFINE_ULONG(Name, Value) rb_define_global_const(Name, C_TO_XEN_ULONG(Value))~%")
(hey "#endif~%")
(hey "~%")
(hey "  g_type_init();~%~%")

(for-each
 (lambda (val)
   (hey "  DEFINE_INTEGER(XG_PRE ~S XG_POST,~80,1T~A);~%" val val))
 (reverse ints))

(for-each
 (lambda (val)
   (hey "  DEFINE_ULONG(XG_PRE ~S XG_POST,~80,1T~A);~%" val val))
 (reverse ulongs))

(if (not (null? extra-ints))
    (begin
      (hey "~%#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each
       (lambda (val)
	 (hey "  DEFINE_INTEGER(XG_PRE ~S XG_POST,~80,1T~A);~%" val val))
       (reverse extra-ints))
      (hey "#endif~%~%")))

(if (not (null? deprecated-ints))
    (begin
      (hey "~%  /* -------------------- deprecated ints -------------------- */~%")
      (hey "#if (!(defined(GDK_DISABLE_DEPRECATED))) && (!(defined(GTK_DISABLE_DEPRECATED)))~%")
      (for-each
       (lambda (val)
	 (hey "  DEFINE_INTEGER(XG_PRE ~S XG_POST,~80,1T~A);~%" val val))
       (reverse deprecated-ints))
      (hey "#endif~%~%")))

(if (not (null? broken-ints))
    (begin
      (hey "~%  /* -------------------- obsolete ints -------------------- */~%")
      (hey "#if GDK_ENABLE_BROKEN && GTK_ENABLE_BROKEN~%")
      (for-each
       (lambda (val)
	 (hey "  DEFINE_INTEGER(XG_PRE ~S XG_POST,~80,1T~A);~%" val val))
       (reverse broken-ints))
      (hey "#endif~%~%")))

(hey "}~%~%")


(hey "static void define_doubles(void)~%")
(hey "{~%~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_DOUBLE(Name, Value) scm_c_define(Name, C_TO_XEN_DOUBLE(Value))~%")
(hey "#else~%")
(hey "  #define DEFINE_DOUBLE(Name, Value) gh_define(Name, C_TO_XEN_DOUBLE(Value))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_DOUBLE(Name, Value) rb_define_global_const(Name, C_TO_XEN_DOUBLE(Value))~%")
(hey "#endif~%")
(hey "~%")

(for-each
 (lambda (val)
   (hey "  DEFINE_DOUBLE(XG_PRE ~S XG_POST,~80,1T~A);~%" val val))
 (reverse dbls))

(hey "}~%~%")


(hey "/* -------------------------------- predefined Atoms -------------------------------- */~%")
(hey "~%")
(hey "static void define_atoms(void)~%")
(hey "{~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_ATOM(Name, Value) scm_permanent_object(scm_c_define(Name, C_TO_XEN_GdkAtom(Value)))~%")
(hey "#else~%")
(hey "  #define DEFINE_ATOM(Name, Value) gh_define(Name, C_TO_XEN_GdkAtom(Value))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_ATOM(Name, Value) rb_define_global_const(Name, C_TO_XEN_GdkAtom(Value))~%")
(hey "#endif~%~%")

(for-each
 (lambda (atom)
   (hey "  DEFINE_ATOM(XG_PRE ~S XG_POST,~80,1T~A);~%" atom atom))
 (reverse atoms))
(hey "}~%~%")


(hey "/* -------------------------------- strings -------------------------------- */~%")
(hey "~%")
(hey "static void define_strings(void)~%")
(hey "{~%")
(hey "  ~%")
(hey "#if HAVE_GUILE~%")
(hey "#if HAVE_SCM_C_DEFINE~%")
(hey "  #define DEFINE_STRING(Name, Value) scm_c_define(Name, scm_makfrom0str(Value))~%")
(hey "#else~%")
(hey "  #define DEFINE_STRING(Name, Value) gh_define(Name, scm_makfrom0str(Value))~%")
(hey "#endif~%")
(hey "#else~%")
(hey "  #define DEFINE_STRING(Name, Value) rb_define_global_const(Name, C_TO_XEN_STRING(Value))~%")
(hey "#endif~%")

(for-each
 (lambda (str)
   (hey "  DEFINE_STRING(XG_PRE ~S XG_POST,~80,1T~A);~%" str str))
 (reverse strings))

(if (not (null? extra-strings))
    (begin
      (hey "~%#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
      (for-each
       (lambda (str)
	 (hey "  DEFINE_STRING(XG_PRE ~S XG_POST,~80,1T~A);~%" str str))
       (reverse extra-strings))
      (hey "#endif~%~%")))

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
(hey "      XEN_EVAL_C_STRING(\"(define xm-version \\\"18-Feb-02\\\")\");~%")
(hey "#endif~%")
(hey "      xg_already_inited = 1;~%")
(hey "    }~%")
(hey "  return(XEN_FALSE);~%")
(hey "}~%")


;/* cc -c xg.c -g3 -DUSE_GTK -DDEBUGGING -DDEBUG_MEMORY -DLINUX -DUSE_SND -DWITH_BIG_COLORMAP -DHAVE_GNU_LIBC_VERSION_H -DHAVE_GSL -DHAVE_DLFCN_H -DHAVE_GUILE -DHAVE_LLONGS -DHAVE_APPLICABLE_SMOB -DHAVE_SCM_REMEMBER_UPTO_HERE -DHAVE_SCM_OBJECT_TO_STRING -DHAVE_SCM_NUM2LONG_LONG -DHAVE_SCM_C_MAKE_VECTOR -DHAVE_SCM_C_DEFINE -DHAVE_SCM_NUM2INT -DHAVE_SCM_C_DEFINE_GSUBR -DHAVE_SCM_LIST_N -DHAVE_SCM_C_EVAL_STRING -DHAVE_SCM_STR2SYMBOL -DHAVE_SCM_MAKE_REAL -DHAVE_SCM_T_CATCH_BODY -DHAVE_EXTENSION_LANGUAGE -DHAVE_STATIC_XM -DHAVE_GTK2 -I/home/bil/test/g3/include -I/home/bil/test/g3/include/glib-2.0 -I/home/bil/test/g3/include/pango-1.0 -I/home/bil/test/g3/include/gtk-2.0 -I/home/bil/test/g3/lib/gtk-2.0/include -I/home/bil/test/g3/include/atk-1.0 -I/home/bil/test/include -DGTK_ENABLE_BROKEN -DPANGO_ENABLE_ENGINE -DPANGO_ENABLE_BACKEND -DGDK_ENABLE_BROKEN */

