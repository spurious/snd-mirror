#!/usr/local/bin/guile -s
!#

;;; makexg.scm creates the gtk2/gdk/pango/glib bindings using xgdata.scm, writes xg.c and xg-ruby.c

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(use-modules (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(define xg-file (open-output-file "xg.c"))
(define xg-ruby-file (open-output-file "xg-ruby.c"))
(define xg-x11-file (open-output-file "xg-x11.h"))

(define (hey . args)
  (display (apply format #f args) xg-file))

(define (heyc arg)
  (display arg xg-file))

(define (say . args)
  (display (apply format #f args) xg-ruby-file))

(define (say-hey . args)
  (apply hey args)
  (apply say args))

(define (hey-x11 . args)
  (display (apply format #f args) xg-x11-file))

(define names '())
(define types '())
(define ints '())
(define ulongs '())
(define dbls '())
(define funcs '())
(define casts '())
(define checks '())
(define check-types '())
(define atoms '())
(define strings '())
(define strings-232 '())
(define strings-234 '())
(define structs '())
(define make-structs '()) ; these have a xg-specific make function
(define struct-fields '())
(define settable-struct-fields '())
;;; "extra" for pango engine/backend
(define extra-types '())
(define extra-funcs '())
(define extra-casts '())
(define extra-checks '())
(define extra-ints '())
(define extra-strings '())

(define funcs-21 '())
(define types-21 '())
(define casts-21 '())
(define checks-21 '())
(define check-types-21 '())
(define ulongs-21 '())

(define ints-22 '())
(define funcs-22 '())
(define types-22 '())

(define names-23 '())
(define funcs-23 '())
(define types-23 '())
(define casts-23 '())
(define checks-23 '())
(define check-types-23 '())
(define ulongs-23 '())
(define ints-23 '())
(define ints-234 '())
(define ints-235 '())

(define funcs-231 '())
(define funcs-232 '())
(define funcs-234 '())
(define funcs-235 '())
(define funcs-236 '())
(define casts-234 '())
(define checks-234 '())
(define types-234 '())
(define check-types-234 '())

(define idlers (list "g_source_remove" "g_idle_remove_by_data"
		     "gtk_quit_remove" "gtk_quit_remove_by_data" 
		     "gtk_key_snooper_remove"))

(define no-c-to-xen (list
		     "GClosureNotify" "GSignalAccumulator" "GSignalCMarshaller" "GSignalQuery*" "GQuark*" "GSignalEmissionHook" "GDestroyNotify" 
		     "GdkDragProtocol*" "GdkSegment*" "GdkModifierType*" "GdkGCValues*" "gint8*" "GdkWChar*" "GdkBitmap**" "gchar***" 
		     "GdkWindowAttr*" "GdkWMDecoration*" "GdkGeometry*" "GdkPixbufDestroyNotify" "guint8*" "GdkInterpType" "GTimeVal*" 
		     "gsize" "GtkAccelLabel*" "GtkAccelMapForeach" "GtkAccessible*" "GtkAlignment*" "GtkArrow*" "GtkAspectFrame*" "GtkButtonBox*" 
		     "GtkBin*" "GtkBox*" "GtkPackType*" "GtkButton*" "GtkCalendar*" "GtkCellRendererText*" "GtkCellRendererToggle*" "GtkCheckMenuItem*" 
		     "GtkTargetEntry*" "GtkColorSelection*" "GtkCombo*" "GtkItem*" "GtkContainer*" "GtkCurve*" "gfloat*" "GtkDialog*" "GtkEditable*" 
		     "GtkEntry*" "GtkFileSelection*" "GtkFixed*" "GtkFontSelection*" "GtkFontSelectionDialog*" "GtkFrame*" "GtkHandleBox*" 
		     "GtkImage*" "GtkImageMenuItem*" "GtkIMContextSimple*" "guint16*" "GtkIMMulticontext*" "GtkMenuShell*" "GtkItemFactoryEntry*" 
		     "GtkLabel*" "GtkLayout*" "GtkMenuDetachFunc" "GtkMenuItem*" "GtkMisc*" "GtkNotebook*" "GtkOptionMenu*" "GtkPaned*" 
		     "GtkPlug*" "GtkProgressBar*" "GtkRadioButton*" "GtkRadioMenuItem*" "GtkRange*" "GtkStateType*" "GtkPathPriorityType*" 
		     "GtkRuler*" "GtkScale*" "GtkScrolledWindow*" "GdkEventSelection*" "GParamSpec*" "GtkRcPropertyParser" "GString*" 
		     "GtkSettingsValue*" "GtkSocket*" "GtkSpinButton*" "GtkStatusbar*" "GtkTable*" "GtkTextCharPredicate" "GtkTextTagTableForeach" 
		     "GtkTextView*" "GtkToggleButton*" "GtkToolbar*" "GtkTreeDragSource*" "GtkTreeDragDest*" "GtkTreeModelSort*" "GtkTreeModel**" 
		     "GtkTreeSortable*" "GtkSortType*" "GtkViewport*" "GtkAllocation*" "PangoRectangle*" "PangoAttrList**" "gunichar*" "PangoAnalysis*" 
		     "PangoFontDescription**" "PangoFontMap*" "GdkEventExpose*" "GdkEventNoExpose*" "GdkEventVisibility*" "GdkEventMotion*" 
		     "GdkEventButton*" "GdkEventScroll*" "GdkEventCrossing*" "GdkEventFocus*" "GdkEventConfigure*" "GdkEventProperty*" "GdkEventProximity*" 
		     "GdkEventSetting*" "GdkEventWindowState*" "GdkEventDND*" "GtkCellRendererPixbuf*" "GtkColorSelectionDialog*" "GtkDrawingArea*" 
		     "GtkGammaCurve*" "GtkInputDialog*" "GtkInvisible*" "GtkMessageDialog*" "GdkScreen**" "GSignalAccumulator" "GSignalCMarshaller" 
		     "GSignalEmissionHook" "GDestroyNotify" "GdkPixbufDestroyNotify" "GdkInterpType" "gsize" "GtkAccelMapForeach" "GtkMenuDetachFunc" 
		     "GtkRcPropertyParser" "GtkTextCharPredicate" "GtkTextTagTableForeach" "gssize" "GtkCellLayoutDataFunc"
		     "GtkEventBox*" "GtkTreeModelFilter*" "GtkActionEntry*" "GtkToggleActionEntry*" "GtkComboBox*" "GtkComboBoxEntry*"
		     "GtkExpander*" "GtkFontButton*" "GtkColorButton*" "GtkEntryCompletionMatchFunc" "GtkUIManagerItemType" "GtkRadioToolButton*"
		     "GtkSeparatorToolItem*" "GtkToggleToolButton*" "GSourceFunc" "GtkFileFilterFunc" "GtkFileFilterInfo*" "GtkCellLayout*"
		     "CellLayoutDataFunc" "GtkFileChooser*" "GError**" "GtkIconLookupFlags" "GtkToolButton*" "GtkClipboardTargetsReceivedFunc"
		     "GType*" "PangoFontFace*"
		     ))

(define no-xen-p (list "GdkXEvent*" "GdkVisualType*" "GError*" "GSignalInvocationHint*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*"
		       "GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*"
		       "GdkDeviceKey*" "GtkWidget**" "GtkLabelSelectionInfo*" "GtkItemFactoryCallback" "GtkNotebookPage*" "GtkRangeLayout*"
		       "GData*" "GtkRangeStepTimer*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTextBTree*" "GtkTextLogAttrCache*"
		       "GtkTableRowCol*" "GtkAccelMap*" "GtkTooltipsData*"
		       ))

(define no-xen-to-c (list "GdkXEvent*" "GSignalInvocationHint*" "GdkVisualType*" "GError*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*" 
			  "GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*" 
			  "GdkDeviceKey*" "GtkWidget**" "GtkItemFactoryCallback" "GtkLabelSelectionInfo*" "GtkNotebookPage*" "GtkRangeLayout*" 
			  "GtkRangeStepTimer*" "GData*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTableRowCol*" "GtkTextBTree*" 
			  "GtkTextLogAttrCache*" "GtkAccelMap*" "GtkTooltipsData*"
			  ))

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

(define (settable-field? arg)
  (and (= (length arg) 3)
       (eq? (caddr arg) 'set)))

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

(define (deref-element-type arg)
  (let ((type (car arg)))
    (substring type 0 (- (string-length type) 2))))

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

(define (parse-args args extra)
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
				(if (or (char=? (string-ref given-name 0) #\[)
					(char=? (string-ref given-name 0) #\{)
					(char=? (string-ref given-name 0) #\|))
				    (begin
				      (set! reftype (deref-type (list type)))
				      (set! data (cons (list type 
							     (substring given-name 1 (- (string-length given-name) 1))
							     given-name) 
						       data)))
				    (if (char=? (string-ref given-name 0) #\&)
					(set! data (cons (list type 
							       (substring given-name 1 (string-length given-name))
							       'set)
							 data))
					(set! data (cons (list type given-name) data))))))
			(if reftype (set! type reftype))

			(if (eq? extra 'extra)
			    (if (and (not (member type types))
				     (not (member type extra-types)))
				(set! extra-types (cons type extra-types)))
			    (if (eq? extra '21)
				(if (and (not (member type types))
					 (not (member type types-21)))
				    (set! types-21 (cons type types-21)))
				(if (eq? extra '22)
				    (if (and (not (member type types))
					     (not (member type types-21))
					     (not (member type types-22)))
					(set! types-22 (cons type types-22)))
				    (if (eq? extra '23)
					(if (and (not (member type types))
						 (not (member type types-21))
						 (not (member type types-22))
						 (not (member type types-23)))
					    (set! types-23 (cons type types-23)))
					(if (or (eq? extra '234) (eq? extra '235) (eq? extra '236))
					    (if (and (not (member type types))
						     (not (member type types-21))
						     (not (member type types-22))
						     (not (member type types-23))
						     (not (member type types-234)))
						(set! types-234 (cons type types-234)))
					    (if (not (member type types))
						(set! types (cons type types))))))))
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
			(list 'GtkCallback
			      "void"
			      "func2"
			      (parse-args "GtkWidget* w lambda_data func_data" 'callback)
			      'temporary)
;typedef gboolean (*GSourceFunc)       (gpointer data);
			(list 'GSourceFunc
			      "gboolean"
			      "timer_func"
			      (parse-args "lambda_data func_data" 'callback)
			      'semi-permanent)
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
			(list 'GtkMenuPositionFunc
			      "void"
			      "menu_position_func"
			      (parse-args "GtkMenu* menu gint* x gint* y gboolean* push lambda_data func_data" 'callback)
			      'permanent)
			(list 'GtkTextTagTableForeach
			      "void"
			      "text_tag_table_foreach"
			      (parse-args "GtkTextTag* tag lambda_data func_data" 'callback)
			      'temporary)
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
			(list 'GtkClipboardTargetsReceivedFunc
			      "void"
			      "clip_targets_received"
			      (parse-args "GtkClipboard* clipboard GdkAtom *atoms gint n_atoms  lambda_data func_data" 'callback)
			      'temporary)
			(list 'GtkTextCharPredicate
			      "gboolean"
			      "text_char_predicate"
			      (parse-args "gunichar ch lambda_data func_data" 'callback)
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

			; GCallback 'lambda can be whatever is indicated by caller (2 or 3 args)
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
	(cons "char*" "String")
	(cons "gchar*" "String")
	(cons "guchar*" "String") ; added 30-Jul-02 then removed then put back...
	(cons "guint" "ULONG")
	(cons "guint16" "INT")
	(cons "gint" "INT")
	(cons "gshort" "INT")
	(cons "gint16" "INT")
	(cons "guint8" "INT")
	(cons "guchar" "INT")
	(cons "gint8" "INT")
	(cons "xen" #t)
	(cons "etc" #t)

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

	(cons "Drawable_was_Window*" "DRAWABLE_WAS_WINDOW")

	(cons "GtkFileChooserAction" "INT")
	(cons "GtkUIManagerItemType" "INT")
	(cons "GtkFileFilterFlags" "INT")
	(cons "GtkIconLookupFlags" "INT")
	(cons "GtkScrollStep" "INT")
	))

(define (type-it type)
  (let ((typ (assoc type direct-types))
	(g2 '()))
    (if typ
	(if (cdr typ)
	    (begin
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
			  (if (string=? type "etc")
			      (begin
				(hey "#define XEN_etc_P(Arg) (XEN_LIST_P(Arg))~%")
				(hey "#define XEN_TO_C_etc(Arg) ((gpointer)Arg)~%"))
			      (begin
				(hey "#define XEN_~A_P(Arg) ((XEN_LIST_P(Arg)) && (XEN_LIST_LENGTH(Arg) > 2))~%" (no-stars (car typ)))
				(hey "#define XEN_TO_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ)))))))))))
	(if (and (not (string=? type "lambda"))
		 (not (string=? type "lambda_data"))
		 (not (find-callback 
		       (lambda (func)
			 (string=? type (symbol->string (car func))))))
		 (not (string=? type "GCallback")))
	    (begin
	      (hey "XM_TYPE~A(~A, ~A)~%" 
		   (if (or (has-stars type) 
			   (string=? type "gpointer")
			   (string=? type "GClosureNotify"))
		       (if (member type no-c-to-xen)
			   "_PTR_1"
			   (if (member type no-xen-p)
			       (if (member type no-xen-to-c)
				   "_PTR_2"
				   "_PTR_NO_P")
			       "_PTR"))
		       (if (member type no-c-to-xen)
			   "_1"
			   (if (member type no-xen-p)
			       "_NO_P"
			       "")))
		   (no-stars type) 
		   type))))))

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
		  (if (string=? (car arg) "GCallback")
		      (return 'GCallback))))))
      strs)
     'fnc)))

(define (no-way str arg)
  (display (format #f str arg)))

(define* (CFNC data #:optional spec spec-data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC~%" name)
	(let ((type (car-str data)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args 'ok)))
	    (if spec
		(set! funcs (cons (list name type strs args spec spec-data) funcs))
		(set! funcs (cons (list name type strs args) funcs)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define (CFNC-PA data min-len max-len types)
  (let ((step (length types)))
    (CFNC data 'etc (list min-len max-len types))))

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

(define* (CFNC-21 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-21~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21)))
	      (set! types-21 (cons type types-21)))
	  (let ((strs (parse-args args '21)))
	    (set! funcs-21 (cons (list name type strs args) funcs-21))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-23 data #:optional spec spec-data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-23~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21))
		   (not (member type types-22))
		   (not (member type types-23)))
	      (set! types-23 (cons type types-23)))
	  (let ((strs (parse-args args '23)))
	    (if spec
		(set! funcs-23 (cons (list name type strs args spec spec-data) funcs-23))
		(set! funcs-23 (cons (list name type strs args) funcs-23)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define (CFNC-23-PA data min-len max-len types)
  (let ((step (length types)))
    (CFNC-23 data 'etc (list min-len max-len types))))

(define* (CFNC-231 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-231~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21))
		   (not (member type types-22))
		   (not (member type types-23)))
	      (set! types-23 (cons type types-23)))
	  (let ((strs (parse-args args '23)))
	    (set! funcs-231 (cons (list name type strs args) funcs-231))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-232 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-232~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21))
		   (not (member type types-22))
		   (not (member type types-23)))
	      (set! types-23 (cons type types-23)))
	  (let ((strs (parse-args args '23)))
	    (set! funcs-232 (cons (list name type strs args) funcs-232))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-234 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-234~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21))
		   (not (member type types-22))
		   (not (member type types-23))
		   (not (member type types-234)))
	      (set! types-234 (cons type types-234)))
	  (let ((strs (parse-args args '234)))
	    (set! funcs-234 (cons (list name type strs args) funcs-234))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-235 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-235~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21))
		   (not (member type types-22))
		   (not (member type types-23))
		   (not (member type types-234)))
	      (set! types-234 (cons type types-234)))
	  (let ((strs (parse-args args '235)))
	    (set! funcs-235 (cons (list name type strs args) funcs-235))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-236 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-236~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21))
		   (not (member type types-22))
		   (not (member type types-23))
		   (not (member type types-234)))
	      (set! types-234 (cons type types-234)))
	  (let ((strs (parse-args args '236)))
	    (set! funcs-236 (cons (list name type strs args) funcs-236))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-22 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-22~%" name)
	(let ((type (car-str data)))
	  (if (and (not (member type types))
		   (not (member type types-21))
		   (not (member type types-22)))
	      (set! types-22 (cons type types-22)))
	  (let ((strs (parse-args args '22)))
	    (set! funcs-22 (cons (list name type strs args) funcs-22))
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

(define (CSTR-232 name)
  (if (assoc name names-23)
      (no-way "~A CSTR-232~%" name)
      (begin
	(set! strings-232 (cons name strings-232))
	(set! names-23 (cons (cons name 'string) names-23)))))

(define (CSTR-234 name)
  (if (assoc name names-23)
      (no-way "~A CSTR-234~%" name)
      (begin
	(set! strings-234 (cons name strings-234))
	(set! names-23 (cons (cons name 'string) names-23)))))

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

(define declared-types '())
(define (save-declared-type type)
  (if (not (member type declared-types)) (set! declared-types (cons type declared-types))))

(define* (CLNG name #:optional type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG~%" name)
      (begin
	(set! ulongs (cons (list name type spec-name) ulongs))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-21 name #:optional type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-21~%" name)
      (begin
	(set! ulongs-21 (cons (list name type spec-name) ulongs-21))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-23 name #:optional type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-23~%" name)
      (begin
	(set! ulongs-23 (cons (list name type spec-name) ulongs-23))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CINT name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT~%" name)
      (begin
	(set! ints (cons name ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-22 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-22~%" name)
      (begin
	(set! ints-22 (cons name ints-22))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-23 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-23~%" name)
      (begin
	(set! ints-23 (cons name ints-23))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-234 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-234~%" name)
      (begin
	(set! ints-234 (cons name ints-234))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-235 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-235~%" name)
      (begin
	(set! ints-235 (cons name ints-235))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-extra name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-extra~%" name)
      (begin
	(set! extra-ints (cons name extra-ints))
	(set! names (cons (cons name 'int) names)))))

(define (CCAST name type) ; this is the cast (type *)obj essentially but here it's (list type* (cadr obj))
  (if (assoc name names)
      (no-way "~A CCAST~%" name)
      (begin
	;;(if (not (member type types))
	;;    (set! types (cons type types)))
	(set! casts (cons (list name type) casts))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-21 name type)
  (if (assoc name names)
      (no-way "~A CCAST-21~%" name)
      (begin
	(set! casts-21 (cons (list name type) casts-21))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-23 name type)
  (if (assoc name names)
      (no-way "~A CCAST-23~%" name)
      (begin
	(set! casts-23 (cons (list name type) casts-23))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-234 name type)
  (if (assoc name names)
      (no-way "~A CCAST-234~%" name)
      (begin
	(set! casts-234 (cons (list name type) casts-234))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK name type)
  (if (assoc name names)
      (no-way "~A CCHK~%" name)
      (begin
	(if (not (member type check-types))
	    (set! check-types (cons type check-types)))
	(set! checks (cons (list name type) checks))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-21 name type)
  (if (assoc name names)
      (no-way "~A CCHK-21~%" name)
      (begin
	(if (and (not (member type check-types))
		 (not (member type check-types-21)))
	    (set! check-types-21 (cons type check-types-21)))
	(set! checks-21 (cons (list name type) checks-21))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-23 name type)
  (if (assoc name names)
      (no-way "~A CCHK-23~%" name)
      (begin
	(if (and (not (member type check-types))
		 (not (member type check-types-21))
		 (not (member type check-types-23)))
	    (set! check-types-23 (cons type check-types-23)))
	(set! checks-23 (cons (list name type) checks-23))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-234 name type)
  (if (assoc name names)
      (no-way "~A CCHK-234~%" name)
      (begin
	(if (and (not (member type check-types))
		 (not (member type check-types-21))
		 (not (member type check-types-23))
		 (not (member type check-types-234)))
	    (set! check-types-234 (cons type check-types-234)))
	(set! checks-234 (cons (list name type) checks-234))
	(set! names (cons (cons name 'def) names)))))

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
	     (if (settable-field? field)
		 (if (not (member (cadr field) settable-struct-fields))
		     (set! settable-struct-fields (cons (cadr field) settable-struct-fields)))
		 (if (not (member (cadr field) struct-fields))
		     (set! struct-fields (cons (cadr field) struct-fields)))))
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


(define (with-extra dpy thunk)
  (dpy "#if PANGO_ENABLE_ENGINE && PANGO_ENABLE_BACKEND~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-21 dpy thunk)
  (dpy "#if HAVE_GDK_DRAW_PIXBUF~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-22 dpy thunk)
  (dpy "#ifdef GTK_CELL_RENDERER_FOCUSED~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-23 dpy thunk)
  (dpy "#if HAVE_GTK_FILE_CHOOSER_DIALOG_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-231 dpy thunk)
  (dpy "#if HAVE_GTK_EXPANDER_GET_USE_MARKUP~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-232 dpy thunk)
  (dpy "#if HAVE_GTK_MENU_SHELL_CANCEL~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-234 dpy thunk)
  (dpy "#if HAVE_GTK_COMBO_BOX_POPUP~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-235 dpy thunk)
  (dpy "#if HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-236 dpy thunk)
  (dpy "#if HAVE_GBOOLEAN_GTK_FILE_CHOOSER_SET_FILENAME~%")
  (thunk)
  (dpy "#endif~%~%"))


;;; ---------------------------------------- write output files ----------------------------------------
(hey "/* xg.c: Guile and Ruby bindings for gdk/gtk/pango, some of glib~%")
(hey " *   this file generated automatically from makexg.scm and xgdata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " *   PANGO_ENABLE_ENGINE and PANGO_ENABLE_BACKEND are handled together, and may be removed later~%")
(hey " *~%")
(hey " *   other flags:~%")
(hey " *     HAVE_GDK_DRAW_PIXBUF for gtk+-2.1 additions~%")
(hey " *     defined(GTK_CELL_RENDERER_FOCUSED) for gtk+-2.2~%")
(hey " *     HAVE_GTK_FILE_CHOOSER_DIALOG_NEW for gtk+-2.3~%")
(hey " *     HAVE_GTK_EXPANDER_GET_USE_MARKUP for gtk+-2.3.1~%")
(hey " *     HAVE_GTK_MENU_SHELL_CANCEL for gtk+-2.3.2~%")
(hey " *     HAVE_GTK_COMBO_BOX_POPUP for gtk+-2.3.4~%")
(hey " *     HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT for gtk+-2.3.5~%")
(hey " *     HAVE_GBOOLEAN_GTK_FILE_CHOOSER_SET_FILENAME for gtk+-2.3.6~%")
(hey " *~%")
(hey " * reference args initial values are usually ignored, resultant values are returned in a list.~%")
(hey " * null ptrs are passed and returned as #f, trailing \"user_data\" callback function arguments are optional (default: #f).~%")
(hey " * where supported, \"...\" args are passed as a list, trailing NULL or -1 should be omitted.~%")
(hey " * 'xg is added to *features*~%")
(hey " *~%")
(hey " * added funcs:~%")
(hey " *    (xm-version) -> date string.~%")
(hey " *    (->string val) interprets 'val' as a string.~%")
(hey " *    (c-array->list arr len) derefs each member of arr, returning lisp list, len=#f: null terminated array~%")
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
(hey " *     anything with a va_list or GtkArg* argument.~%")
(hey " *     most of the unusual keysym names~%")
(hey " *     all *_CLASS, *_IFACE macros~%")
(hey " *     win32-specific functions~%")
(hey " *~%")
(hey " * HISTORY:~%")
(hey " *     4-Apr:     various additions, deletions, and bugfixes for snd-test 26~%")
(hey " *     29-Mar:    support for some ... args.~%")
(hey " *     22-Mar:    g_source_remove and related changes.~%")
(hey " *     11-Mar:    gtk 2.3.6 changes.~%")
(hey " *     4-Mar:     gtk 2.3.5 changes.~%")
(hey " *     26-Feb:    gtk 3.2.4 changes.~%")
(hey " *     12-Feb:    g_list_nth_data (Kjetil S. Matheussen).~%")
(hey " *     6-Feb:     gtk 2.3.2 changes.~%")
(hey " *     --------~%")
(hey " *     16-Dec:    gtk 2.3.1 changes.~%")
(hey " *     1-Dec:     gtk 2.3 changes.~%")
(hey " *     15-Sep:    removed client_window GtkIMMulticontext struct field (for Gtk 2.2.4).~%")
(hey " *     26-May:    removed nugatory GdkInputFunction stuff and some unused type converters.~%")
(hey " *     7-Apr:     GTK_RC_STYLE has two incompatible definitions in gtk! (gtkwidget.h, gtkrc.h) -- will use int case.~%")
(hey " *     1-Apr:     gdk_property_get uses scm_mem2string in some cases now.~%")
(hey " *     31-Mar:    gchar* -> xen string bugfix (thanks to Friedrich Delgado Friedrichs).~%")
(hey " *     10-Mar:    Ruby Xm_Version.~%")
(hey " *     6-Jan-03:  gtk 2.2 changes.~%")
(hey " *     --------~%")
(hey " *     18-Nov:    Ruby/Gtk bugfixes.~%")
(hey " *     28-Oct:    gtk 2.1 additions.~%")
(hey " *     25-Oct:    removed (deprecated) gdk_set_pointer_hooks~%")
(hey " *     31-Jul:    removed GTK 1.n support~%")
(hey " *     24-Jul:    changed Guile prefix (R5RS reserves vertical-bar).~%")
(hey " *     19-Jul:    XG_FIELD_PRE for change from using vertical-bar (reserved in R5RS)~%")
(hey " *     2-Jun:     removed deprecated and broken stuff~%")
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
(hey "#if UNDEF_USE_SND~%  #undef USE_SND~%  #define USE_SND 0~%#endif~%~%")

(hey "#include <glib.h>~%")
(hey "#include <gdk/gdk.h>~%")
(hey "#include <gdk/gdkkeysyms.h>~%")
(hey "#include <gtk/gtk.h>~%")
(hey "#include <glib-object.h>~%")
(hey "#include <pango/pango.h>~%")
(hey "#include <string.h>~%")
(hey "#include <stdlib.h>~%~%")

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
(hey "  void *val;~%")
(hey "  val = (void *)XEN_OBJECT_REF(obj);~%")
(hey "  FREE(val);~%")
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
(hey "#define XM_TYPE_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_NO_P(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "~%")
(hey "#define XM_TYPE_PTR(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(XEN_FALSE_P(val) || (WRAP_P(#Name, val)));} /* if NULL ok, should be explicit? */~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_NO_P(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static int XEN_ ## Name ## _P(XEN val) {return(XEN_FALSE_P(val) || (WRAP_P(#Name, val)));} /* if NULL ok, should be explicit? */~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_2(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "~%")
(hey "/* type checks for callback wrappers */~%")

(for-each 
 (lambda (func)
   (hey "#define XEN_~A_P(Arg)  XEN_FALSE_P(Arg) || (XEN_PROCEDURE_P(Arg) && (XEN_REQUIRED_ARGS(Arg) == ~D))~%"
	(symbol->string (callback-name func))
	(length (callback-args func))))
 callbacks)

(hey "#define XEN_lambda_P(Arg) XEN_PROCEDURE_P(Arg)~%")
(hey "#define XEN_GCallback_P(Arg) XEN_PROCEDURE_P(Arg) && ((XEN_REQUIRED_ARGS(Arg) == 2) || (XEN_REQUIRED_ARGS(Arg) == 3))~%")

(for-each
 (lambda (func)
   (hey "#define XEN_TO_C_~A(Arg) XEN_FALSE_P(Arg) ? NULL : gxg_~A~%"
	(symbol->string (callback-name func))
	(callback-func func)))
 callbacks)

(hey "#define XEN_TO_C_GCallback(Arg) ((XEN_REQUIRED_ARGS(Arg) == 3) ? (GCallback)gxg_func3 : (GCallback)gxg_func2)~%")
(hey "#define XEN_TO_C_lambda_data(Arg) (gpointer)gxg_ptr~%")
(hey "#define XEN_lambda_data_P(Arg) 1~%")

(hey "#define C_TO_XEN_GtkTreeViewSearchEqualFunc(Arg) WRAP_FOR_XEN(\"GtkTreeViewSearchEqualFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkTreeIterCompareFunc(Arg) WRAP_FOR_XEN(\"GtkTreeViewSearchEqualFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkTreeSelectionFunc(Arg) WRAP_FOR_XEN(\"GtkTreeSelectionFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkMenuPositionFunc(Arg) WRAP_FOR_XEN(\"GtkMenuPositionFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkDestroyNotify(Arg) WRAP_FOR_XEN(\"GtkDestroyNotify\", Arg)~%")
(hey "#define XEN_TO_C_GdkFilterReturn(Arg) (GdkFilterReturn)XEN_TO_C_INT(Arg)~%")

(hey "#define XEN_TO_C_String(Arg) ((XEN_STRING_P(Arg)) ? XEN_TO_C_STRING(Arg) : NULL)~%")
(hey "#define C_TO_XEN_String(Arg) ((Arg != NULL) ? C_TO_XEN_STRING(Arg) : XEN_FALSE)~%")
(hey "#define XEN_String_P(Arg) ((XEN_FALSE_P(Arg)) || (XEN_STRING_P(Arg)))~%")

(hey "~%#ifdef GTK_CELL_RENDERER_FOCUSED~%")
(hey "  static GdkDrawable* XEN_TO_C_DRAWABLE_WAS_WINDOW (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((GdkDrawable *)XEN_TO_C_ULONG(XEN_CADR(val)));}~%")
(hey "  static int XEN_DRAWABLE_WAS_WINDOW_P(XEN val) {return(XEN_FALSE_P(val) || (WRAP_P(\"GdkDrawable_\", val)));}~%")
(hey "  #define Drawable_was_Window GdkDrawable~%")
(hey "#else~%")
(hey "  static GdkWindow* XEN_TO_C_DRAWABLE_WAS_WINDOW (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((GdkWindow *)XEN_TO_C_ULONG(XEN_CADR(val)));}~%")
(hey "  static int XEN_DRAWABLE_WAS_WINDOW_P(XEN val) {return(XEN_FALSE_P(val) || (WRAP_P(\"GdkWindow_\", val)));}~%")
(hey "  #define Drawable_was_Window GdkWindow~%")
(hey "#endif~%")


(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(for-each type-it (reverse types))

;;; (define (check-type-it type)
;;;   ;; not currently used -- this is the equivalent of Widget? in xm (but gtk uses GTK_IS_WIDGET etc)
;;;   (if (has-stars type)
;;;       (hey "static XEN XEN_~A_p(XEN val) {return(C_TO_XEN_BOOLEAN(XEN_FALSE_P(val) || (WRAP_P(~S, val))));}~%"
;;; 	   (no-stars type) (no-stars type))
;;;       (hey "static XEN XEN_~A_p(XEN val) {return(C_TO_XEN_BOOLEAN(WRAP_P(~S, val)));}~%"
;;; 	   (no-stars type) (no-stars type))))

;;; (for-each check-type-it (reverse check-types))

(if (not (null? extra-types)) 
    (with-extra hey (lambda () 
		      (for-each type-it (reverse extra-types))
;;;		      (for-each check-type-it (reverse extra-types))
		      )))

(if (not (null? types-21))
    (with-21 hey
	     (lambda ()
	       (for-each type-it (reverse types-21))
	       )))
(if (not (null? types-22))
    (with-22 hey
	     (lambda ()
	       (for-each type-it (reverse types-22))
	       )))
(if (not (null? types-23))
    (with-23 hey
	     (lambda ()
	       (for-each type-it (reverse types-23))
	       )))
(if (not (null? types-234))
    (with-234 hey
	     (lambda ()
	       (for-each type-it (reverse types-234))
	       )))

(hey "#define XLS(a, b) XEN_TO_C_gchar_(XEN_LIST_REF(a, b))~%")
(hey "#define XLI(a, b) XEN_TO_C_INT(XEN_LIST_REF(a, b))~%")
(hey "#define XLG(a, b) XEN_TO_C_GType(XEN_LIST_REF(a, b))~%")
(hey "#define XLT(a, b) XEN_TO_C_GtkTextTag_(XEN_LIST_REF(a, b))~%~%")

(hey "static XEN c_to_xen_string(XEN str)~%")
(hey "{~%")
(hey "  return(C_TO_XEN_STRING((char *)XEN_TO_C_ULONG(str)));~%")
(hey "}~%~%")


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
(hey "#if 0~%")
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
(hey "              (id == (guint)(XEN_TO_C_INT(XEN_CADR(idler)))))~%")
(hey "            {~%")
(hey "              velts[i] = XEN_FALSE;~%")
(hey "              last_xm_unprotect = i;~%")
(hey "              return;~%")
(hey "            }}}~%")
(hey "}~%")
(hey "#endif~%")
(hey "static void xm_unprotect_at(int ind)~%")
(hey "{~%")
(hey "  XEN *velts;~%")
(hey "  velts = XEN_VECTOR_ELEMENTS(xm_protected);~%")
(hey "  velts[ind] = XEN_FALSE;~%")
(hey "  last_xm_unprotect = ind;~%")
(hey "}~%~%")

(hey "~%~%/* ---------------------------------------- callback handlers ---------------------------------------- */~%~%")

(hey "#if WITH_GTK_AND_X11~%")
(hey "  #include \"xg-x11.h\"~%")
(hey "  #define gxg_static~%")
(hey "#else~%")
(hey "  #define gxg_static static~%")
(hey "#endif~%~%")

(hey-x11 "#ifndef XG_X11_H~%")
(hey-x11 "#define XG_X11_H~%~%")
(hey-x11 "/* functions shared by xg.c and xm.c if WITH_GTK_AND_X11 */~%")
(hey-x11 "/* created automatically by makexg.scm */~%")

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
	       (hey "c__FUNCTION__)")
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

(hey "~%static gboolean gxg_func3(GtkWidget *w, GdkEventAny *ev, gpointer data)~%")
(hey "{~%")
(hey "  return(XEN_TO_C_BOOLEAN(XEN_CALL_3(XEN_CAR((XEN)data),~%")
(hey "                          C_TO_XEN_GtkWidget_(w),~%")
(hey "                          C_TO_XEN_GdkEventAny_(ev),~%")
(hey "                          XEN_CADR((XEN)data),~%")
(hey "                          c__FUNCTION__)));~%")
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
	  (spec (and (> (length data) 4) (list-ref data 4)))
	  (spec-data (and (> (length data) 5) (list-ref data 5)))
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
		(if (has-stars (deref-type arg))
		    (hey "  ~A ~A = NULL;~%" (deref-type arg) (deref-name arg))
		    (hey "  ~A ~A;~%" (deref-type arg) (deref-name arg)))))
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
	 (let ((ctr 1)
	       (argc #f))
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
				 (no-stars argtype) argname argname ctr name argtype)))
		    (if (>= (length arg) 3)
			(if (char=? (string-ref (list-ref arg 2) 0) #\{)
			    (begin
			      (set! argc (deref-name arg))
			      (hey "  ~A = XEN_TO_C_~A(~A);~%" (deref-name arg) (deref-type arg) argname))
			    (if (char=? (string-ref (list-ref arg 2) 0) #\|)
				(begin
				  (hey "  ~A = (~A)calloc(~A, sizeof(~A));~%" 
				       (deref-name arg)
				       (deref-type arg)
				       argc
				       (deref-element-type arg))
				  (hey "  {~%   int i;~%   XEN lst;~%   lst = XEN_COPY_ARG(~A);~%" argname)
				  (hey "   for (i = 0; i < ~A; i++, lst = XEN_CDR(lst)) ~A[i] = XEN_TO_C_~A(XEN_CAR(lst));~%"
				       argc
				       (deref-name arg)
				       (no-stars (deref-element-type arg)))
				  (hey "  }~%"))))))
		(set! ctr (1+ ctr))))
	    args)))
     (let ((using-result #f)
	   (using-loc #f))
       (if (not (eq? lambda-type 'fnc))
	   (begin
	     (set! using-loc (or (eq? lambda-type 'GCallback)
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
	   (begin ; lambda-type = 'fnc
	     (set! using-result (and (> refargs 0)
				     (not (string=? return-type "void"))))
	     (if using-result
		 (begin
		   (hey "  {~%")
		   (hey "    XEN result = XEN_FALSE;~%")))
	     (hey-start)
	     (if (not (eq? spec 'etc))
		 (if (not (string=? return-type "void"))
		     (if (= refargs 0)
			 (hey-on "  return(C_TO_XEN_~A(" (no-stars return-type))
			 (hey-on "    result = C_TO_XEN_~A(" (no-stars return-type)))
		     (hey-on "  ")))))
       ;; pass args
       (if (eq? spec 'etc)
	   (begin
	     ;; goes to end
	     ;; need to check ... list, set up locals, send out switch, return result
	     (let* ((list-name (cadr (list-ref args (1- cargs))))
		    (min-len (car spec-data))
		    (max-len (cadr spec-data))
		    (types (caddr spec-data))
		    (with-minus-one (or (string=? name "gtk_list_store_set")
					(string=? name "gtk_tree_store_set")))
		    (with-null (and (not with-minus-one) 
				    (not (and (= (length types) 1) 
					      (string=? (car types) "GType")))))
		    (modlen (length types)))
	       (hey "  {~%")
	       (hey "    int etc_len = 0;~%")
	       (if (not (string=? return-type "void"))
		   (hey "    ~A result = ~A;~%" return-type (if (has-stars return-type) "NULL" "0")))
	       (do ((i 0 (1+ i)))
		   ((= i (1- cargs)))
		 (let ((arg (list-ref args i)))
		   (hey "    ~A p_arg~D;~%" (car arg) i)))
	       (hey "    if (XEN_LIST_P(~A)) etc_len = XEN_LIST_LENGTH(~A);~%" list-name list-name)
	       (if (> min-len 0)
		   (hey "    if (etc_len < ~D) XEN_OUT_OF_RANGE_ERROR(~S, ~A, ~A, \"... list must have at least ~D entr~A\");~%"
			min-len name (1- cargs) list-name min-len (if (= min-len 1) "y" "ies")))
	       (hey "    if (etc_len > ~D) XEN_OUT_OF_RANGE_ERROR(~S, ~A, ~A, \"... list too long (max len: ~D)\");~%"
		    max-len name (1- cargs) list-name max-len)
	       (if (not (= modlen 1))
		   (hey "    if ((etc_len % ~D) != 0) XEN_OUT_OF_RANGE_ERROR(~S, ~A, ~A, \"... list len must be multiple of ~D\");~%"
			modlen name (1- cargs) list-name modlen))
	       (do ((i 0 (1+ i)))
		   ((= i (1- cargs)))
		 (let ((arg (list-ref args i)))
		   (hey "    p_arg~D = XEN_TO_C_~A(~A);~%" i (no-stars (car arg)) (cadr arg))))
	       (hey "    switch (etc_len)~%")
	       (hey "      {~%")
	       (do ((i min-len (+ i modlen)))
		   ((> i max-len))
		 (if (not (string=? return-type "void"))
		     (hey "        case ~D: result = ~A(" i name)
		     (hey "        case ~D: ~A(" i name))
		 (do ((j 0 (1+ j)))
		     ((= j (1- cargs)))
		   (let ((arg (list-ref args j)))
		     (hey "p_arg~D, " j)))
		 ;; assume ending null for now
		 (let ((modctr 0))
		   (do ((j 0 (1+ j)))
		       ((= j i))
		     (let ((type (list-ref types modctr)))
		       (set! modctr (1+ modctr))
		       (if (>= modctr modlen) (set! modctr 0))
		       (if (string=? type "int")
			   (hey "XLI(")
			   (if (string=? type "gchar*")
			       (hey "XLS(")
			       (if (string=? type "GtkTextTag*")
				   (hey "XLT(")
				   (if (string=? type "GType")
				       (hey "XLG(")
				       (display (format #f "unknown etc element type: ~A~%" type)))))))
		     (hey "~A, ~D)" list-name j)
		     (if (or with-null with-minus-one (< j (1- i)))
			 (hey ", "))))
		 (if with-null
		     (hey "NULL); break;~%")
		     (if with-minus-one
			  (hey "-1); break;~%")
			  (hey "); break;~%"))))
	       (hey "      }~%")
	       (if (not (string=? return-type "void"))
		   (hey "    return(C_TO_XEN_~A(result));~%" (no-stars return-type))
		   (hey "    return(XEN_FALSE);~%"))
	       (hey "  }~%")
	     ))
	   (begin
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
			      (if (and ;(not previous-arg)
				   (or (string=? argtype "char**")
				       (string=? argtype "gchar*"))
				   (eq? spec 'const))
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
			     (if protect-arglist
				 (hey "    return(xen_return_first(result, arglist));~%")
				 (hey "    return(result);~%"))
			     (if protect-arglist
				 (hey "    return(xen_return_first(XEN_FALSE, arglist));~%")
				 (hey "    return(XEN_FALSE);~%")))
			 (hey "   }~%"))
		       (begin ;'fnc
			 (if (> refargs 0)
			     (let* ((previous-arg using-result))
			       (if using-result (heyc "  "))
			       (if (string=? name "gdk_property_get")
				   (begin
				     ;; special case -- type returned is dependent to some extent on atom
				     (hey "  {~%      XEN data_val = XEN_FALSE;~%\
#if HAVE_GUILE && HAVE_SCM_MEM2STRING~%\
      if (ref_actual_property_type == GDK_TARGET_STRING)~%\
	data_val = C_TO_XEN_STRING((char *)ref_data);~%\
      else if (ref_actual_length > 0) data_val = scm_mem2string((char *)ref_data, ref_actual_length * ref_actual_format / 8);~%\
#else~%\
      data_val = C_TO_XEN_STRING((char *)ref_data);~%\
#endif~%\
     return(XEN_LIST_5(result, C_TO_XEN_GdkAtom(ref_actual_property_type), C_TO_XEN_gint(ref_actual_format), ~%\
                       C_TO_XEN_gint(ref_actual_length), data_val));~%\
    }~%  }~%")
				     )
				   (begin
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
				     (if using-result (hey "   }~%")))))
			     ;; refargs = 0
			     (begin
			       (if (member name idlers)
				   (if (string=? name "gtk_idle_remove")
				       (hey "  xm_unprotect_idler(XEN_TO_C_guint(~A));~%" (cadr (car args)))
				       (hey "  xm_unprotect_at(XEN_TO_C_INT(XEN_CADDR(~A)));~%" (cadr (car args)))))
			       (if (string=? return-type "void")
				   (if protect-arglist
				       (hey "  return(xen_return_first(XEN_FALSE, arglist));~%")
				       (hey "  return(XEN_FALSE);~%"))))))))
		 (begin ; 'lambda (see line 1846)
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
			  (hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname)))
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
			  (hey-on "XEN_TO_C_~A(~A)" (no-stars argtype) argname)))
		      args))
		   (if (string=? return-type "void")
		       (begin
			 (hey ");~%")
			 (if protect-arglist
			     (hey "    return(xen_return_first(XEN_FALSE, arglist));~%")
			     (hey "    return(XEN_FALSE);~%")))
		       (hey ")));~%"))
		   (hey "  }~%")) ;'lambda
		 ))) ; 'begin
	   (hey "}~%")
	   ))))

(for-each handle-func (reverse funcs))
(if (not (null? extra-funcs)) (with-extra hey (lambda () (for-each handle-func (reverse extra-funcs)))))
(if (not (null? funcs-21)) (with-21 hey (lambda () (for-each handle-func (reverse funcs-21)))))
(if (not (null? funcs-22)) (with-22 hey (lambda () (for-each handle-func (reverse funcs-22)))))
(if (not (null? funcs-23)) (with-23 hey (lambda () (for-each handle-func (reverse funcs-23)))))
(if (not (null? funcs-231)) (with-231 hey (lambda () (for-each handle-func (reverse funcs-231)))))
(if (not (null? funcs-232)) (with-232 hey (lambda () (for-each handle-func (reverse funcs-232)))))
(if (not (null? funcs-234)) (with-234 hey (lambda () (for-each handle-func (reverse funcs-234)))))
(if (not (null? funcs-235)) (with-235 hey (lambda () (for-each handle-func (reverse funcs-235)))))
(if (not (null? funcs-236)) (with-236 hey (lambda () (for-each handle-func (reverse funcs-236)))))

(define cast-it
 (lambda (cast)
   (let ((cast-name (car cast))
	 (cast-type (cadr cast)))
     (hey "static XEN gxg_~A(XEN obj)" (no-arg cast-name))
     (hey " {return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(~S), XEN_CADR(obj)));}~%" (no-stars cast-type)))))

(for-each cast-it (reverse casts))
(if (not (null? extra-casts)) (with-extra hey (lambda () (for-each cast-it (reverse extra-casts)))))
(if (not (null? casts-21)) (with-21 hey (lambda () (for-each cast-it (reverse casts-21)))))
(if (not (null? casts-23)) (with-23 hey (lambda () (for-each cast-it (reverse casts-23)))))
(if (not (null? casts-234)) (with-234 hey (lambda () (for-each cast-it (reverse casts-234)))))

;;; checks have to use the built-in macros, not local symbol-based type checks
(define (make-check func)
  (hey "static XEN gxg_~A(XEN obj)" (no-arg (car func)))
  (hey " {return(C_TO_XEN_BOOLEAN(XEN_LIST_P(obj) && ~A((GTypeInstance *)XEN_TO_C_ULONG(XEN_CADR(obj)))));}~%" (no-arg (car func))))

(for-each make-check (reverse checks))
(if (not (null? extra-checks)) (with-extra hey (lambda () (for-each make-check (reverse extra-checks)))))
(if (not (null? checks-21)) (with-21 hey (lambda () (for-each make-check (reverse checks-21)))))
(if (not (null? checks-23)) (with-23 hey (lambda () (for-each make-check (reverse checks-23)))))
(if (not (null? checks-234)) (with-234 hey (lambda () (for-each make-check (reverse checks-234)))))


(hey "~%~%/* ---------------------------------------- special functions ---------------------------------------- */~%~%")

(hey "static XEN gxg_vector2GdkPoints(XEN arg1)~%")
(hey "{~%")
(hey "  #define H_vector2GdkPoints \"(vector->GdkPoints vect) packages point data in vect as (opaque) array of GdkPoints\"~%")
(hey "  int i, j, len;~%")
(hey "  XEN *velts;~%")
(hey "  GdkPoint *pt;~%")
(hey "  XEN_ASSERT_TYPE(XEN_VECTOR_P(arg1), arg1, XEN_ONLY_ARG, \"vector->GdkPoints\", \"vector of x,y values\");~%")
(hey "  len = XEN_VECTOR_LENGTH(arg1) / 2;~%")
(hey "  if (len <= 0) XEN_ASSERT_TYPE(0, arg1, 1, \"vector->GdkPoints\", \"positive integer\");~%")
(hey "  velts = XEN_VECTOR_ELEMENTS(arg1);~%")
(hey "  pt = (GdkPoint *)CALLOC(len, sizeof(GdkPoint));~%")
(hey "  for (i = 0, j = 0; i < len; i++, j += 2)~%")
(hey "    {~%")
(hey "      pt[i].x = XEN_TO_C_INT(velts[j]);~%")
(hey "      pt[i].y = XEN_TO_C_INT(velts[j + 1]);~%")
(hey "    }~%")
(hey "  return(C_TO_XEN_ULONG((unsigned long)pt));~%")
(hey "}~%")
(hey "~%")
(hey "static XEN gxg_freeGdkPoints(XEN arg1)~%")
(hey "{~%")
(hey "  void *pts;~%")
(hey "  #define H_freeGdkPoints \"(freeGdkPoints vect) frees an (opaque) GdkPoint array created by vector->Gdkpoints\"~%")
(hey "  XEN_ASSERT_TYPE(XEN_ULONG_P(arg1), arg1, XEN_ONLY_ARG, \"freeGdkPoints\", \"opaque GdkPoint array\");~%")
(hey "  pts = (void *)(XEN_TO_C_ULONG(arg1));~%") 
(hey "  FREE(pts);~%")
(hey "  return(XEN_FALSE);~%")
(hey "}~%")



;;; ---------------- Ruby step 1 ----------------
(say "/* Ruby connection for xg.c */~%~%")

(define (argify-func func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs)))
    (say "XEN_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs 10) "VARGIFY"
	     (if (> refargs 0)
		 (format #f "ARGIFY_~D" cargs)
		 (format #f "NARGIFY_~D" cargs)))
	 (car func) (car func))))
	 
(for-each argify-func (reverse funcs))
(if (not (null? extra-funcs)) (with-extra say (lambda () (for-each argify-func (reverse extra-funcs)))))
(if (not (null? funcs-21)) (with-21 say (lambda () (for-each argify-func (reverse funcs-21)))))
(if (not (null? funcs-22)) (with-22 say (lambda () (for-each argify-func (reverse funcs-22)))))
(if (not (null? funcs-23)) (with-23 say (lambda () (for-each argify-func (reverse funcs-23)))))
(if (not (null? funcs-231)) (with-231 say (lambda () (for-each argify-func (reverse funcs-231)))))
(if (not (null? funcs-232)) (with-232 say (lambda () (for-each argify-func (reverse funcs-232)))))
(if (not (null? funcs-234)) (with-234 say (lambda () (for-each argify-func (reverse funcs-234)))))
(if (not (null? funcs-235)) (with-235 say (lambda () (for-each argify-func (reverse funcs-235)))))
(if (not (null? funcs-236)) (with-236 say (lambda () (for-each argify-func (reverse funcs-236)))))

(define (ruby-cast func) (say "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-cast (reverse casts))
(if (not (null? extra-casts)) (with-extra say (lambda () (for-each ruby-cast (reverse extra-casts)))))
(if (not (null? casts-21)) (with-21 say (lambda () (for-each ruby-cast (reverse casts-21)))))
(if (not (null? casts-23)) (with-23 say (lambda () (for-each ruby-cast (reverse casts-23)))))
(if (not (null? casts-234)) (with-234 say (lambda () (for-each ruby-cast (reverse casts-234)))))

(define (ruby-check func) (say "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func))))
(for-each ruby-check (reverse checks))
(if (not (null? extra-checks)) (with-extra say (lambda () (for-each ruby-check (reverse extra-checks)))))
(if (not (null? checks-21)) (with-21 say (lambda () (for-each ruby-check (reverse checks-21)))))
(if (not (null? checks-23)) (with-23 say (lambda () (for-each ruby-check (reverse checks-23)))))
(if (not (null? checks-234)) (with-234 say (lambda () (for-each ruby-check (reverse checks-234)))))

(say "XEN_NARGIFY_2(c_array_to_xen_list_w, c_array_to_xen_list)~%")
(say "XEN_NARGIFY_2(xen_list_to_c_array_w, xen_list_to_c_array)~%")
(say "XEN_NARGIFY_1(gxg_freeGdkPoints_w, gxg_freeGdkPoints)~%")
(say "XEN_NARGIFY_1(gxg_vector2GdkPoints_w, gxg_vector2GdkPoints)~%")

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (say "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (say "#endif~%")
	       (set! in-x11 #f))))
     (say "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" field field))
   struct-fields)
  (if in-x11
      (say "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (say "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (say "#endif~%")
	       (set! in-x11 #f))))
     (say "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" field field)) 
   settable-struct-fields)
  (if in-x11
      (say "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (say "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (say "#endif~%")
	       (set! in-x11 #f))))
     (say "XEN_NARGIFY_2(gxg_set_~A_w, gxg_set_~A)~%" field field)) 
   settable-struct-fields)
  (if in-x11
      (say "#endif~%")))

(for-each (lambda (struct) 
	    (let* ((s (find-struct struct)))
	      (if (> (length (cadr s)) 0)
		  (say "XEN_VARGIFY(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
		  (say "XEN_NARGIFY_0(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
 (reverse make-structs))
(say "~%")
;;; ---------------- end Ruby step 1 ----------------

(hey "static XEN c_array_to_xen_list(XEN val, XEN clen);~%")
(hey "static XEN xen_list_to_c_array(XEN val, XEN type);~%~%")

(hey "  #define XG_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XG_PRE #Name XG_POST, Value, A1, A2, A3, Help)~%")

(hey "#if HAVE_GUILE~%")
(say-hey "static void define_functions(void)~%")
(say-hey "{~%")

(say-hey "  xm_gc_table = XEN_MAKE_VECTOR(1, XEN_FALSE);~%")
(say-hey "  XEN_PROTECT_FROM_GC(xm_gc_table);~%")
(say-hey "  xm_protected_size = 512;~%")
(say-hey "  xm_protected = XEN_MAKE_VECTOR(xm_protected_size, XEN_FALSE);~%")
(say-hey "  XEN_VECTOR_SET(xm_gc_table, 0, xm_protected);~%~%")

(hey "  XG_DEFINE_PROCEDURE(c-array->list, c_array_to_xen_list, 2, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(list->c-array, xen_list_to_c_array, 2, 0, 0, NULL);~%~%")
(hey "  XG_DEFINE_PROCEDURE(freeGdkPoints, gxg_freeGdkPoints, 1, 0, 0, H_freeGdkPoints);~%")
(hey "  XG_DEFINE_PROCEDURE(vector->GdkPoints, gxg_vector2GdkPoints, 1, 0, 0, H_vector2GdkPoints);~%")
(hey "  XG_DEFINE_PROCEDURE(->string, c_to_xen_string, 1, 0, 0, NULL);~%")

(define (defun func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs)))

    (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A, ~D, ~D, ~D, H_~A);~%"
		     (car func) (car func) 
		     (if (>= cargs 10) 0 args)
		     (if (>= cargs 10) 0 refargs)
		     (if (>= cargs 10) 1 0)
		     (car func))
    (say "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, ~D, ~D, ~D, H_~A);~%"
		     (car func) (car func) 
		     (if (>= cargs 10) 0 args)
		     (if (>= cargs 10) 0 refargs)
		     (if (>= cargs 10) 1 0)
		     (car func))))

(for-each defun (reverse funcs))
(if (not (null? extra-funcs)) (with-extra say-hey (lambda () (for-each defun (reverse extra-funcs)))))
(if (not (null? funcs-21)) (with-21 say-hey (lambda () (for-each defun (reverse funcs-21)))))
(if (not (null? funcs-22)) (with-22 say-hey (lambda () (for-each defun (reverse funcs-22)))))
(if (not (null? funcs-23)) (with-23 say-hey (lambda () (for-each defun (reverse funcs-23)))))
(if (not (null? funcs-231)) (with-231 say-hey (lambda () (for-each defun (reverse funcs-231)))))
(if (not (null? funcs-232)) (with-232 say-hey (lambda () (for-each defun (reverse funcs-232)))))
(if (not (null? funcs-234)) (with-234 say-hey (lambda () (for-each defun (reverse funcs-234)))))
(if (not (null? funcs-235)) (with-235 say-hey (lambda () (for-each defun (reverse funcs-235)))))
(if (not (null? funcs-236)) (with-236 say-hey (lambda () (for-each defun (reverse funcs-236)))))

(define (cast-out func)
  (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func)))
  (say "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))

(for-each cast-out (reverse casts))
(if (not (null? extra-casts)) (with-extra say-hey (lambda () (for-each cast-out (reverse extra-casts)))))
(if (not (null? casts-21)) (with-21 say-hey (lambda () (for-each cast-out (reverse casts-21)))))
(if (not (null? casts-23)) (with-23 say-hey (lambda () (for-each cast-out (reverse casts-23)))))
(if (not (null? casts-234)) (with-234 say-hey (lambda () (for-each cast-out (reverse casts-234)))))

(say "  XG_DEFINE_PROCEDURE(c-array->list, c_array_to_xen_list_w, 2, 0, 0, NULL);~%")
(say "  XG_DEFINE_PROCEDURE(list->c-array, xen_list_to_c_array_w, 2, 0, 0, NULL);~%")
(say "  XG_DEFINE_PROCEDURE(freeGdkPoints, gxg_freeGdkPoints_w, 1, 0, 0, H_freeGdkPoints);~%")
(say "  XG_DEFINE_PROCEDURE(vector->GdkPoints, gxg_vector2GdkPoints_w, 1, 0, 0, H_vector2GdkPoints);~%")
(say "  XG_DEFINE_PROCEDURE(->string, c_to_xen_string, 1, 0, 0, NULL);~%")

(define (check-out func)
  (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func)))
  (say "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))

(for-each check-out (reverse checks))
(if (not (null? extra-checks)) (with-extra say-hey (lambda () (for-each check-out (reverse extra-checks)))))
(if (not (null? checks-21)) (with-21 say-hey (lambda () (for-each check-out (reverse checks-21)))))
(if (not (null? checks-23)) (with-23 say-hey (lambda () (for-each check-out (reverse checks-23)))))
(if (not (null? checks-234)) (with-234 say-hey (lambda () (for-each check-out (reverse checks-234)))))

(say-hey "}~%~%")
(hey "#endif~%")


(hey "/* ---------------------------------------- structs ---------------------------------------- */~%~%")

(hey "  #define XG_DEFINE_READER(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XG_FIELD_PRE #Name XG_POST, Value, A1, A2, A3, Help)~%")
(hey "  #if HAVE_RUBY~%")
(hey "  #define XG_DEFINE_ACCESSOR(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "    XEN_DEFINE_PROCEDURE_WITH_SETTER(XG_FIELD_PRE #Name XG_POST, Value, NULL, XG_FIELD_PRE \"set_\" #Name XG_POST, SetValue, A1, A2, A3, A4)~%")
(hey "  #else~%")
(hey "  #define XG_DEFINE_ACCESSOR(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "    XEN_DEFINE_PROCEDURE_WITH_SETTER(XG_FIELD_PRE #Name XG_POST, Value, NULL, \"set! \" XG_FIELD_PRE #Name XG_POST, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%~%")

(define (array->list type)
  (hey "  if (strcmp(ctype, ~S) == 0)~%" (no-stars type))
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)XEN_TO_C_ULONG(XEN_CADR(val)); ~%" type type)
  (hey "      if (len == -1) {for (i = 0; arr[i]; i++); len = i;}~%")
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
(hey "  int i, len = -1;~%")
(hey "  char *ctype;~%")
(hey "  if (XEN_INTEGER_P(clen))~%")
(hey "    len = XEN_TO_C_INT(clen);~%")
(hey "  if (!(XEN_LIST_P(val))) return(XEN_FALSE); /* type:location cons */~%")
(hey "  ctype = XEN_SYMBOL_TO_C_STRING(XEN_CAR(val));~%")
(for-each array->list listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (not (member type listable-types))
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
	    (not (member type listable-types))
	    (member (deref-type (list type)) types))
       (list->array type)))
 types)
(hey "  return(XEN_FALSE);~%")
(hey "}~%~%")

(define (make-reader field)
  ;; gather structs that share field
  ;; if 1 or 2 assert type, if and return,
  ;;   else if on each, assert 0 at end and xen false
  (hey "~%")
  (if (or (member field with-x11-accessors)
	  (member field with-x11-readers))
      (begin
	(hey "gxg_static XEN gxg_~A(XEN ptr)~%" field)
	(hey-x11 "XEN gxg_~A(XEN ptr);~%" field))
      (hey "static XEN gxg_~A(XEN ptr)~%" field))
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
	 (if (or (> (length vals) 2)
		 (and (= (length vals) 2)
		      (= ctr 0)))
	     (hey "  if (XEN_~A__P(ptr)) " (car val))
	     (heyc "  "))
	 (set! ctr (+ ctr 1))
	 (hey "return(C_TO_XEN_~A((~A)((XEN_TO_C_~A_(ptr))->~A)));~%"
	      (no-stars (cadr val)) (cadr val) (car val) field))
       vals))
    (if (> (length vals) 2)
	(hey "  XEN_ASSERT_TYPE(0, ptr, XEN_ONLY_ARG, ~S, \"pointer to struct with ~A field\");~%"
	     field field))
    (hey "}~%")
    ))

(define (make-writer field)
  (hey "~%")
  (if (member field with-x11-accessors)
      (begin
	(hey "gxg_static XEN gxg_set_~A(XEN ptr, XEN val)~%" field)
	(hey-x11 "XEN gxg_set_~A(XEN ptr, XEN val);~%" field))
      (hey "static XEN gxg_set_~A(XEN ptr, XEN val)~%" field))
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
    (if (null? vals)
	(display (format #f "(writer) ~A: not found" field))
	(begin
	  (if (= (length vals) 1)
	      (hey "  XEN_ASSERT_TYPE(XEN_~A__P(ptr), ptr, XEN_ARG_1, ~S, ~S);~%" 
		   (caar vals) field 
		   (caar vals))
	      (if (= (length vals) 2)
		  (hey "  XEN_ASSERT_TYPE(XEN_~A__P(ptr) || XEN_~A__P(ptr), ptr, XEN_ARG_1, ~S, ~S \" or \" ~S);~%" 
		       (caar vals) (car (cadr vals)) field 
		       (caar vals) (car (cadr vals)))))))
    (let ((ctr 0))
      (for-each
       (lambda (val)
	 (if (or (> (length vals) 2)
		 (and (= (length vals) 2)
		      (= ctr 0)))
	     (hey "  if (XEN_~A__P(ptr)) " (car val))
	     (heyc "  "))
	 (set! ctr (+ ctr 1))
	 (hey "(XEN_TO_C_~A_(ptr))->~A = XEN_TO_C_~A(val);~%"
	      (car val) field (no-stars (cadr val))))
       vals))
    (if (> (length vals) 2)
	(hey "  XEN_ASSERT_TYPE(0, ptr, XEN_ARG_1, \"set! ~A\", \"pointer to struct with ~A field\");~%"
	     field field))
    (hey "  return(val);~%}~%")
    ))

(for-each make-reader (reverse struct-fields))
(for-each make-reader (reverse settable-struct-fields))
(for-each make-writer (reverse settable-struct-fields))

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

(hey "#if HAVE_GUILE~%")
(say-hey "static void define_structs(void)~%")
(say-hey "{~%~%")

(let ((in-x11 #f))
  (for-each 
   (lambda (field)
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (say-hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (say-hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "  XG_DEFINE_READER(~A, gxg_~A, 1, 0, 0, NULL);~%" field field)
     (say "  XG_DEFINE_READER(~A, gxg_~A_w, 1, 0, 0, NULL);~%" field field))
   struct-fields)
  (if in-x11
      (say-hey "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field)
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (say-hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (say-hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "  XG_DEFINE_ACCESSOR(~A, gxg_~A, gxg_set_~A, 1, 0, 2, 0);~%" field field field)
     (say "  XG_DEFINE_ACCESSOR(~A, gxg_~A_w, gxg_set_~A_w, 1, 0, 2, 0);~%" field field field))
   settable-struct-fields)
  (if in-x11
      (say-hey "#endif~%")))

(for-each 
 (lambda (struct)
   (let* ((s (find-struct struct)))
     (hey "  XG_DEFINE_PROCEDURE(~A, gxg_make_~A, 0, 0, ~D, NULL);~%" struct struct (if (> (length (cadr s)) 0) 1 0))
     (say "  XG_DEFINE_PROCEDURE(~A, gxg_make_~A_w, 0, 0, ~D, NULL);~%" struct struct (if (> (length (cadr s)) 0) 1 0))))
 (reverse make-structs))

(say-hey "}~%~%")
(hey "#else~%")
(hey "  #include \"xg-ruby.c\"~%")
(hey "#endif~%")


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
(hey "  g_type_init();~%")

(for-each 
 (lambda (val) 
   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse ints))

(if (not (null? ints-22))
    (with-22 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-22)))))
(if (not (null? ints-23))
    (with-23 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-23)))))
(if (not (null? ints-234))
    (with-234 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-234)))))
(if (not (null? ints-235))
    (with-235 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-235)))))


(for-each 
 (lambda (vals)
   (let ((val (car vals)))
     (hey "  DEFINE_ULONG(~A);~%" val)))
 (reverse ulongs))

(if (not (null? extra-ints)) 
    (with-extra hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse extra-ints)))))
(if (not (null? ulongs-21))
    (with-21 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-21)))))
(if (not (null? ulongs-23))
    (with-23 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-23)))))
     

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
   (hey "  DEFINE_DOUBLE(~A);~%" val))
 (reverse dbls))
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
   (hey "  DEFINE_ATOM(~A);~%" atom))
 (reverse atoms))
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

(for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings))
(if (not (null? extra-strings))
    (with-extra hey (lambda () (for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse extra-strings)))))
(if (not (null? strings-232))
    (with-232 hey (lambda () (for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings-232)))))
(if (not (null? strings-234))
    (with-234 hey (lambda () (for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings-234)))))
(hey "}~%~%")


(hey "/* -------------------------------- initialization -------------------------------- */~%~%")
(hey "static bool xg_already_inited = false;~%~%")
(hey "#if WITH_GTK_AND_X11~%")
(hey "#if HAVE_GUILE~%")
(hey " void init_x11(void);~%")
(hey "#else~%")
(hey " void Init_libx11(void);~%")
(hey "#endif~%")
(hey "#endif~%~%")
(hey "#if HAVE_GUILE~%")
(hey " void init_xm(void);~%")
(hey " void init_xm(void)~%")
(hey "#else~%")
(hey " void Init_libxm(void);~%")
(hey " void Init_libxm(void)~%")
(hey "#endif~%")
(hey "{~%")
(hey "  if (!xg_already_inited)~%")
(hey "    {~%")
(hey "      define_xm_obj();~%")
(hey "      define_integers();~%")
(hey "      define_doubles();~%")
(hey "      define_functions();~%")
(hey "      define_structs();~%")
(hey "      define_atoms();~%")
(hey "      define_strings();~%")
(hey "      XEN_YES_WE_HAVE(\"xg\");~%")
(hey "#if HAVE_GUILE~%")
(hey "      XEN_EVAL_C_STRING(\"(define xm-version \\\"~A\\\")\");~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "#endif~%")
(hey "#if HAVE_RUBY~%")
(hey "      rb_define_global_const(\"Xm_Version\", C_TO_XEN_STRING(\"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "#endif~%")
(hey "      xg_already_inited = true;~%")
(hey "#if WITH_GTK_AND_X11~%")
(hey "#if HAVE_GUILE~%")
(hey "      init_x11();~%")
(hey "#else~%")
(hey "      Init_libx11();~%")
(hey "#endif~%")
(hey "#endif~%~%")
(hey "    }~%")
(hey "}~%")

(hey-x11 "#endif~%")

(close-output-port xg-file)
(close-output-port xg-ruby-file)
(close-output-port xg-x11-file)

;(for-each
; (lambda (type)
;   (if (not (assoc type direct-types))
;       (display (format #f ";not direct: ~A~%" type))))
; declared-types)
;
;(for-each
; (lambda (v)
;   (if (not (member (car v) declared-types))
;       (display (format #f "~A " (car v)))))
; direct-types)

