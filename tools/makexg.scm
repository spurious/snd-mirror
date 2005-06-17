#!/usr/local/bin/guile -s
!#

;;; makexg.scm creates the gtk2/gdk/pango/glib bindings using xgdata.scm, writes xg.c, xg-ruby.c, xg-x11.h

(use-modules (ice-9 debug))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))
(use-modules (ice-9 common-list))

(debug-enable 'debug)
(debug-enable 'backtrace)
(read-enable 'positions)

(define xg-file (open-output-file "xg.c"))
(define xg-x11-file (open-output-file "xg-x11.h"))

(define (hey . args)
  (display (apply format #f args) xg-file))

(define (heyc arg)
  (display arg xg-file))

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
(define strings-250 '())
(define structs '())
(define make-structs '()) ; these have a xg-specific make function
(define struct-fields '())
(define settable-struct-fields '())

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

(define names-250 '())
(define funcs-250 '())
(define types-250 '())
(define casts-250 '())
(define checks-250 '())
(define check-types-250 '())
(define ulongs-250 '())
(define ints-250 '())

(define ints-251 '())
(define funcs-251 '())
(define types-251 '())

(define funcs-252 '())
(define types-252 '())
(define casts-252 '())
(define checks-252 '())
(define check-types-252 '())
(define ulongs-252 '())

(define funcs-254 '())
(define types-254 '())
(define casts-254 '())
(define checks-254 '())
(define check-types-254 '())
(define ulongs-254 '())
(define ints-254 '())

(define funcs-255 '())
(define types-255 '())

(define funcs-256 '())
(define types-256 '())
(define casts-256 '())
(define checks-256 '())
(define check-types-256 '())
(define ulongs-256 '())
(define ints-256 '())

(define funcs-260 '())
(define ints-260 '())
(define types-260 '())

(define all-types '())
(define all-check-types '())

(define idlers (list "g_source_remove" "g_idle_remove_by_data"
		     "gtk_quit_remove" "gtk_quit_remove_by_data" 
		     "gtk_key_snooper_remove"))

(define no-c-to-xen 
  (list "CellLayoutDataFunc" "GClosureNotify" "GDestroyNotify" "GError**" "GParamSpec*" "GQuark*" "GSignalAccumulator"
	"GSignalCMarshaller" "GSignalEmissionHook" "GSignalQuery*" "GSourceFunc" "GString*" "GTimeVal*" "GType*"
	"GdkBitmap**" "GdkDragProtocol*" "GdkEventButton*" "GdkEventConfigure*" "GdkEventCrossing*" "GdkEventDND*"
	"GdkEventExpose*" "GdkEventFocus*" "GdkEventMotion*" "GdkEventNoExpose*" "GdkEventProperty*" "GdkEventProximity*"
	"GdkEventScroll*" "GdkEventSelection*" "GdkEventSetting*" "GdkEventVisibility*" "GdkEventWindowState*" "GdkGCValues*"
	"GdkGeometry*" "GdkInterpType" "GdkModifierType*" "GdkPixbufDestroyNotify" "GdkScreen**" "GdkSegment*" "GdkWChar*"
	"GdkWMDecoration*"  "GdkWindowAttr*" "GtkAccelLabel*" "GtkAccelMapForeach" "GtkAccessible*" "GtkActionEntry*"
	"GtkAlignment*" "GtkAllocation*" "GtkArrow*" "GtkAspectFrame*" "GtkBin*" "GtkBox*" "GtkButton*" "GtkButtonBox*"
	"GtkCalendar*" "GtkCellLayout*" "GtkCellLayoutDataFunc" "GtkCellRendererPixbuf*" "GtkCellRendererText*" "GtkCellRendererToggle*"
	"GtkCheckMenuItem*" "GtkClipboardTargetsReceivedFunc" "GtkColorButton*" "GtkColorSelection*" "GtkColorSelectionDialog*"
	"GtkCombo*" "GtkComboBox*" "GtkComboBoxEntry*" "GtkContainer*" "GtkCurve*" "GtkDialog*" "GtkDrawingArea*" "GtkEditable*"
	"GtkEntry*" "GtkEventBox*" "GtkExpander*" "GtkFileChooser*" "GtkFileFilterFunc"
	"GtkFileSelection*" "GtkFixed*" "GtkFontButton*" "GtkFontSelection*" "GtkFontSelectionDialog*" "GtkFrame*" "GtkGammaCurve*"
	"GtkHandleBox*" "GtkIMContextSimple*" "GtkIMMulticontext*" "GtkIconLookupFlags" "GtkImage*" "GtkImageMenuItem*" "GtkInputDialog*"
	"GtkInvisible*" "GtkItem*" "GtkItemFactoryEntry*" "GtkLabel*" "GtkLayout*" "GtkMenuDetachFunc" "GtkMenuItem*" "GtkMenuShell*"
	"GtkMessageDialog*" "GtkMisc*" "GtkNotebook*" "GtkOptionMenu*" "GtkPackType*" "GtkPaned*" "GtkPathPriorityType*" "GtkPlug*"
	"GtkProgressBar*" "GtkRadioButton*" "GtkRadioMenuItem*" "GtkRadioToolButton*" "GtkRange*" "GtkRcPropertyParser" "GtkRuler*"
	"GtkScale*" "GtkScrolledWindow*" "GtkSeparatorToolItem*" "GtkSettingsValue*" "GtkSocket*" "GtkSortType*" "GtkSpinButton*"
	"GtkStateType*" "GtkStatusbar*" "GtkTable*" "GtkTextCharPredicate" "GtkTextTagTableForeach" "GtkTextView*"
	"GtkToggleActionEntry*" "GtkToggleButton*" "GtkToggleToolButton*" "GtkToolButton*" "GtkToolbar*" "GtkTreeDragDest*"
	"GtkTreeDragSource*" "GtkTreeModel**" "GtkTreeModelFilter*" "GtkTreeModelSort*" "GtkTreeSortable*" "GtkUIManagerItemType"
	"GtkViewport*" "PangoAnalysis*" "PangoAttrList**" "PangoFontDescription**" "PangoFontMap*" "PangoRectangle*"
	"gchar***" "gfloat*" "gint8*" "gsize" "gssize" "guint16*" "guint8*" "gunichar*" "GtkFileChooserButton*"
	"GtkCellView*" "GValue*" "GtkAboutDialog*" "PangoAttrFilterFunc" "PangoScript*" "GtkMenuToolButton*"
	"GtkClipboardImageReceivedFunc" "PangoMatrix*" "GdkTrapezoid*" "GdkPangoRenderer*" "PangoRenderPart"
	"GLogFunc"

	"GConnectFlags" "GSignalFlags" "GSignalMatchType" "GdkAxisUse" "GdkFillRule" "GdkGCValuesMask"
	"GdkPropMode" "GdkRgbDither" "GdkWMFunction" "GdkWindowEdge" "GdkWindowHints" "GtkAccelFlags" "GtkArrowType"
	"GtkAttachOptions" "GtkCellRendererState" "GtkCurveType" "GtkDestDefaults" "GtkDestroyNotify" "GtkDialogFlags"
	"GtkDirectionType" "GtkExpanderStyle" "GtkIconLookupFlags" "GtkMenuPositionFunc" "GtkPathType" "GtkSpinType"
	"GtkTextSearchFlags" "GtkTreeIterCompareFunc" "GtkTreeSelectionFunc" "GtkUIManagerItemType" "GtkWindowPosition"
	"GtkWindowType" "PangoGlyph" "PangoUnderline" "double" "gsize" "gssize" 
	))

(define no-xen-p 
  (list "GdkXEvent*" "GdkVisualType*" "GError*" "GSignalInvocationHint*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*"
	"GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*"
	"GdkDeviceKey*" "GtkWidget**" "GtkLabelSelectionInfo*" "GtkItemFactoryCallback" "GtkNotebookPage*" "GtkRangeLayout*"
	"GData*" "GtkRangeStepTimer*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTextBTree*" "GtkTextLogAttrCache*"
	"GtkTableRowCol*" "GtkAccelMap*" "GtkTooltipsData*" "GdkAtom*" "PangoScript" "PangoFontFace**"

	"GValue*" "GdkByteOrder" "GdkCrossingMode" "GdkEventType" "GdkGrabStatus" "GdkNotifyType"
	"GdkOverlapType" "GdkScrollDirection" "GdkSettingAction" "GdkVisibilityState" "GdkWindowState" "GdkWindowType"
	"GtkImageType" "GtkTreeModelFlags" "gint16" "gint8" "gshort" "guint8" "lambda" 
	))

(define no-xen-to-c 
  (list "GdkXEvent*" "GSignalInvocationHint*" "GdkVisualType*" "GError*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*" 
	"GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*" 
	"GdkDeviceKey*" "GtkWidget**" "GtkItemFactoryCallback" "GtkLabelSelectionInfo*" "GtkNotebookPage*" "GtkRangeLayout*" 
	"GtkRangeStepTimer*" "GData*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTableRowCol*" "GtkTextBTree*" 
	"GtkTextLogAttrCache*" "GtkAccelMap*" "GtkTooltipsData*" "GdkAtom*" "PangoScript" "PangoFontFace**"

	"GValue*" "GdkByteOrder" "GdkCrossingMode" "GdkEventType" "GdkGrabStatus" "GdkNotifyType"
	"GdkOverlapType" "GdkScrollDirection" "GdkSettingAction" "GdkVisibilityState" "GdkWindowState" "GdkWindowType"
	"GtkImageType" "GtkTreeModelFlags" "etc" "gint16" "gshort"
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

			(if (not (member type all-types))
			    (begin
			      (set! all-types (cons type all-types))
			      (if (eq? extra '21)
				  (set! types-21 (cons type types-21))
				  (if (eq? extra '22)
				      (set! types-22 (cons type types-22))
				      (if (or (eq? extra '23)
					      (eq? extra 'callback-23))
					  (set! types-23 (cons type types-23))
					  (if (or (eq? extra '234) (eq? extra '235) (eq? extra '236))
					      (set! types-234 (cons type types-234))
					      (if (or (eq? extra '250)
						      (eq? extra 'callback-250))
						  (set! types-250 (cons type types-250))
						  (if (or (eq? extra '251)
							  (eq? extra 'callback-251))
						      (set! types-251 (cons type types-251))
						      (if (eq? extra '252)
							  (set! types-252 (cons type types-252))
							  (if (eq? extra '254)
							      (set! types-254 (cons type types-254))
							      (if (eq? extra '255)
								  (set! types-255 (cons type types-255))
								  (if (eq? extra '256)
								      (set! types-256 (cons type types-256))
								      (if (eq? extra '256)
									  (set! types-260 (cons type types-260))
									  (if (not (member type types))
									      (set! types (cons type types))))))))))))))))
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
;			(list 'GdkSpanFunc
;			      "void"
;			      "span_func"
;			      (parse-args "GdkSpan* span lambda_data func_data" 'callback)
;			      'temporary)
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
			(list 'GtkAccelMapForeach
			      "void"
			      "accel_map_foreach"
			      (parse-args "lambda_data func_data gchar* accel_path guint accel_key GdkModifierType accel_mods gboolean changed" 'callback)
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
			      (parse-args "GtkClipboard* clipboard GdkAtom* atoms gint n_atoms lambda_data func_data" 'callback)
			      'temporary)
;			(list 'GtkMenuDetachFunc
;			      "void"
;			      "menu_detach_func"
;			      (parse-args "GtkWidget* attach_widget GtkMenu* menu" 'callback)
;			      'permanent)
;;; detach func is not passed user-data, so it would have to be implemented by hand
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

(define callbacks-23 (list
			(list 'GtkFileFilterFunc
			      "gboolean"
			      "file_filter"
			      (parse-args "GtkFileFilterInfo* info lambda_data func_data" 'callback-23)
			      'permanent)
			(list 'GtkEntryCompletionMatchFunc
			      "gboolean"
			      "entry_completion_match"
			      (parse-args "GtkEntryCompletion* completion gchar* key GtkTreeIter* iter lambda_data func_data" 'callback-23)
			      'permanent)
			))

(define callbacks-250 (list
		       (list 'GtkTreeViewRowSeparatorFunc
			     "gboolean"
			     "row_separator"
			     (parse-args "GtkTreeModel* model GtkTreeIter* iter lambda_data func_data" 'callback-250)
			     'permanent)
		       (list 'GtkIconViewForeachFunc
			     "void"
			     "icon_view_foreach"
			     (parse-args "GtkIconView* icon_view GtkTreePath* path lambda_data func_data" 'callback-250)
			     'permanent)
		       ))

(define callbacks-255 (list
			(list 'GtkClipboardImageReceivedFunc
			      "void"
			      "clip_image_received"
			      (parse-args "GtkClipboard* clipboard GdkPixbuf* pixbuf lambda_data func_data" 'callback-255)
			      'permanent)
			(list 'GLogFunc
			      "void"
			      "g_message_log_func"
			      (parse-args "gchar* domain GLogLevelFlags log_level gchar* message lambda_data func_data" 'callback-255)
			      'permanent)
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
  (or (find-callback-1 test callbacks)
      (find-callback-1 test callbacks-23)
      (find-callback-1 test callbacks-250)
      (find-callback-1 test callbacks-255)
      ))

(define direct-types 
  (list (cons "void" #f)
	(cons "int" "INT")
	(cons "gint" "INT")
	(cons "gint32" "INT")
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
	(cons "gssize" "INT")
	(cons "gsize" "INT")
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
	(cons "PangoEllipsizeMode" "INT")
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
	(cons "GtkFileChooserAction" "INT")
	(cons "GtkUIManagerItemType" "INT")
	(cons "GtkFileFilterFlags" "INT")
	(cons "GtkIconLookupFlags" "INT")
	(cons "GtkScrollStep" "INT")
	(cons "GLogLevelFlags" "INT")
	))

(define (type-it type)
  (let ((typ (assoc type direct-types))
	(g2 '()))
    (if typ
	(if (cdr typ)
	    (begin
	      (if (string? (cdr typ))
		  (begin
		    (if (not (member type no-c-to-xen))
			(hey "#define C_TO_XEN_~A(Arg) C_TO_XEN_~A(Arg)~%" (no-stars (car typ)) (cdr typ)))
		    (if (not (member type no-xen-to-c))
			(hey "#define XEN_TO_C_~A(Arg) (~A)(XEN_TO_C_~A(Arg))~%" (no-stars (car typ)) (car typ) (cdr typ)))
		    (if (not (member type no-xen-p))
			(hey "#define XEN_~A_P(Arg) XEN_~A_P(Arg)~%" 
			     (no-stars (car typ))
			     (if (string=? (cdr typ) "INT") 
				 "INTEGER" 
				 (if (string=? (cdr typ) "DOUBLE")
				     "NUMBER"
				     (cdr typ))))))
		  (begin
		    (if (not (cdr typ)) ; void special case
			(begin
			  (if (not (member type no-xen-p))
			      (hey "#define XEN_~A_P(Arg) 1~%" (no-stars (car typ))))
			  (if (not (member type no-xen-to-c))
			      (hey "#define XEN_TO_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ)))))
			(begin          ; xen special case
			  (if (string=? type "etc")
			      (hey "#define XEN_etc_P(Arg) (XEN_LIST_P(Arg))~%")
			      (begin
				(if (not (member type no-xen-p))
				    (hey "#define XEN_~A_P(Arg) ((XEN_LIST_P(Arg)) && (XEN_LIST_LENGTH(Arg) > 2))~%" (no-stars (car typ))))
				(if (not (member type no-xen-to-c))
				    (hey "#define XEN_TO_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ))))))))))))
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
			       (if (member type no-xen-to-c)
				   "_NO_P_2"
				   "_NO_P")
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

(define* (CFNC data #:optional spec spec-data) ; 'const -> const for arg cast, 'etc for ... args, 'free -> must free C val before return
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) (set! all-types (cons type all-types)))
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

(define* (CFNC-21 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-21~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-21 (cons type types-21))))
	  (let ((strs (parse-args args '21)))
	    (if spec
		(set! funcs-21 (cons (list name type strs args spec) funcs-21))
		(set! funcs-21 (cons (list name type strs args) funcs-21)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-23 data #:optional spec spec-data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-23~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-23 (cons type types-23))))
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
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-23 (cons type types-23))))
	  (let ((strs (parse-args args '23)))
	    (set! funcs-231 (cons (list name type strs args) funcs-231))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-232 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-232~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-23 (cons type types-23))))
	  (let ((strs (parse-args args '23)))
	    (set! funcs-232 (cons (list name type strs args) funcs-232))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-234 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-234~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-234 (cons type types-234))))
	  (let ((strs (parse-args args '234)))
	    (set! funcs-234 (cons (list name type strs args) funcs-234))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-235 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-235~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-234 (cons type types-234))))
	  (let ((strs (parse-args args '235)))
	    (set! funcs-235 (cons (list name type strs args) funcs-235))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-236 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-236~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-234 (cons type types-234))))
	  (let ((strs (parse-args args '236)))
	    (set! funcs-236 (cons (list name type strs args) funcs-236))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-250 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-250: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-250 (cons type types-250))))
	  (let ((strs (parse-args args '250)))
	    (if spec
		(set! funcs-250 (cons (list name type strs args spec) funcs-250))	    
		(set! funcs-250 (cons (list name type strs args) funcs-250)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-251 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-251: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-251 (cons type types-251))))
	  (let ((strs (parse-args args '251)))
	    (if spec
		(set! funcs-251 (cons (list name type strs args spec) funcs-251))
		(set! funcs-251 (cons (list name type strs args) funcs-251)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-252 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-252: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-252 (cons type types-252))))
	  (let ((strs (parse-args args '252)))
	    (if spec
		(set! funcs-252 (cons (list name type strs args spec) funcs-252))
		(set! funcs-252 (cons (list name type strs args) funcs-252)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-254 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-254: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-254 (cons type types-254))))
	  (let ((strs (parse-args args '254)))
	    (if spec
		(set! funcs-254 (cons (list name type strs args spec) funcs-254))
		(set! funcs-254 (cons (list name type strs args) funcs-254)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-255 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-255: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-255 (cons type types-255))))
	  (let ((strs (parse-args args '255)))
	    (if spec
		(set! funcs-255 (cons (list name type strs args spec) funcs-255))
		(set! funcs-255 (cons (list name type strs args) funcs-255)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-256 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-256: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-256 (cons type types-256))))
	  (let ((strs (parse-args args '256)))
	    (if spec
		(set! funcs-256 (cons (list name type strs args spec) funcs-256))
		(set! funcs-256 (cons (list name type strs args) funcs-256)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-260 data #:optional spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-260: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-260 (cons type types-260))))
	  (let ((strs (parse-args args '260)))
	    (if spec
		(set! funcs-260 (cons (list name type strs args spec) funcs-260))
		(set! funcs-260 (cons (list name type strs args) funcs-260)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-22 data)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "~A CFNC-22~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) 
	      (begin
		(set! all-types (cons type all-types))
		(set! types-22 (cons type types-22))))
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

(define (CSTR-250 name)
  (if (assoc name names-250)
      (no-way "~A CSTR-250~%" name)
      (begin
	(set! strings-250 (cons name strings-250))
	(set! names-250 (cons (cons name 'string) names-250)))))

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

(define* (CLNG-250 name #:optional type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-250~%" name)
      (begin
	(set! ulongs-250 (cons (list name type spec-name) ulongs-250))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-252 name #:optional type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-252~%" name)
      (begin
	(set! ulongs-252 (cons (list name type spec-name) ulongs-252))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-254 name #:optional type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-254~%" name)
      (begin
	(set! ulongs-254 (cons (list name type spec-name) ulongs-254))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-256 name #:optional type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-256~%" name)
      (begin
	(set! ulongs-256 (cons (list name type spec-name) ulongs-256))
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

(define* (CINT-250 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-250~%" name)
      (begin
	(set! ints-250 (cons name ints-250))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-251 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-251~%" name)
      (begin
	(set! ints-251 (cons name ints-251))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-254 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-254~%" name)
      (begin
	(set! ints-254 (cons name ints-254))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-256 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-256~%" name)
      (begin
	(set! ints-256 (cons name ints-256))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-260 name #:optional type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-260~%" name)
      (begin
	(set! ints-260 (cons name ints-260))
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

(define (CCAST-250 name type)
  (if (assoc name names)
      (no-way "~A CCAST-250~%" name)
      (begin
	(set! casts-250 (cons (list name type) casts-250))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-252 name type)
  (if (assoc name names)
      (no-way "~A CCAST-252~%" name)
      (begin
	(set! casts-252 (cons (list name type) casts-252))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-254 name type)
  (if (assoc name names)
      (no-way "~A CCAST-254~%" name)
      (begin
	(set! casts-254 (cons (list name type) casts-254))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-256 name type)
  (if (assoc name names)
      (no-way "~A CCAST-256~%" name)
      (begin
	(set! casts-256 (cons (list name type) casts-256))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK name type)
  (if (assoc name names)
      (no-way "~A CCHK~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types (cons type check-types))))
	(set! checks (cons (list name type) checks))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-21 name type)
  (if (assoc name names)
      (no-way "~A CCHK-21~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types-21 (cons type check-types-21))))
	(set! checks-21 (cons (list name type) checks-21))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-23 name type)
  (if (assoc name names)
      (no-way "~A CCHK-23~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types-23 (cons type check-types-23))))
	(set! checks-23 (cons (list name type) checks-23))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-234 name type)
  (if (assoc name names)
      (no-way "~A CCHK-234~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types-234 (cons type check-types-234))))
	(set! checks-234 (cons (list name type) checks-234))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-250 name type)
  (if (assoc name names)
      (no-way "~A CCHK-250~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types-250 (cons type check-types-250))))
	(set! checks-250 (cons (list name type) checks-250))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-252 name type)
  (if (assoc name names)
      (no-way "~A CCHK-252~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types-252 (cons type check-types-252))))
	(set! checks-252 (cons (list name type) checks-252))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-254 name type)
  (if (assoc name names)
      (no-way "~A CCHK-254~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types-254 (cons type check-types-254))))
	(set! checks-254 (cons (list name type) checks-254))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-256 name type)
  (if (assoc name names)
      (no-way "~A CCHK-256~%" name)
      (begin
	(if (not (member type all-check-types))
	    (begin
	      (set! all-check-types (cons type all-check-types))
	      (set! check-types-256 (cons type check-types-256))))
	(set! checks-256 (cons (list name type) checks-256))
	(set! names (cons (cons name 'def) names)))))

(define (STRUCT data)
  (let ((name (car-str data)) ; struct name (type)
	(args (cdr-str data)))
    (if (assoc name names)
	(no-way "~A STRUCT~%" name)
	(let ((strs (parse-args args 'ok))
	      (type-name (string-append name "*")))
	  (if (not (member type-name all-types))
	      (begin
		(set! all-types (cons type-name all-types))
		(set! types (cons type-name types))))
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


(define (with-21 dpy thunk)
  (dpy "#if HAVE_GDK_DRAW_PIXBUF~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-22 dpy thunk)
  (dpy "#if HAVE_GTK_TREE_VIEW_COLUMN_CELL_GET_POSITION~%")
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

(define (with-250 dpy thunk)
  (dpy "#if HAVE_GTK_ABOUT_DIALOG_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-251 dpy thunk)
  (dpy "#if HAVE_GTK_LABEL_SET_ELLIPSIZE~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-252 dpy thunk)
  (dpy "#if HAVE_GTK_FILE_CHOOSER_BUTTON_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-254 dpy thunk)
  (dpy "#if HAVE_GTK_MENU_TOOL_BUTTON_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-255 dpy thunk)
  (dpy "#if HAVE_GTK_LABEL_GET_SINGLE_LINE_MODE~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-256 dpy thunk)
  (dpy "#if HAVE_GDK_PANGO_RENDERER_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-260 dpy thunk)
  (dpy "#if HAVE_GTK_TEXT_LAYOUT_GET_ITER_AT_POSITION~%")
  (thunk)
  (dpy "#endif~%~%"))



;;; ---------------------------------------- write output files ----------------------------------------
(hey "/* xg.c: Guile and Ruby bindings for gdk/gtk/pango, some of glib~%")
(hey " *   this file generated automatically from makexg.scm and xgdata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " *   compile-time flags:~%")
(hey " *     HAVE_GDK_DRAW_PIXBUF for gtk 2.1 additions~%")
(hey " *     HAVE_GTK_TREE_VIEW_COLUMN_CELL_GET_POSITION for gtk 2.2~%")
(hey " *     HAVE_GTK_FILE_CHOOSER_DIALOG_NEW for gtk 2.3~%")
(hey " *     HAVE_GTK_EXPANDER_GET_USE_MARKUP for gtk 2.3.1~%")
(hey " *     HAVE_GTK_MENU_SHELL_CANCEL for gtk 2.3.2~%")
(hey " *     HAVE_GTK_COMBO_BOX_POPUP for gtk 2.3.4~%")
(hey " *     HAVE_GTK_COMBO_BOX_ENTRY_NEW_TEXT for gtk 2.3.5~%")
(hey " *     HAVE_GBOOLEAN_GTK_FILE_CHOOSER_SET_FILENAME for gtk 2.3.6~%")
(hey " *     HAVE_GTK_ABOUT_DIALOG_NEW for gtk 2.5.0~%")
(hey " *     HAVE_GTK_LABEL_SET_ELLIPSIZE for gtk 2.5.1~%")
(hey " *     HAVE_GTK_FILE_CHOOSER_BUTTON_NEW for gtk 2.5.2~%")
(hey " *     HAVE_GTK_MENU_TOOL_BUTTON_NEW for gtk 2.5.3 and 2.5.4~%")
(hey " *     HAVE_GTK_LABEL_GET_SINGLE_LINE_MODE for gtk 2.5.5~%")
(hey " *     HAVE_GDK_PANGO_RENDERER_NEW for gtk 2.5.6~%")
(hey " *     HAVE_GTK_TEXT_LAYOUT_GET_ITER_AT_POSITION for gtk 2.6.0~%")
(hey " *~%")
(hey " * reference args initial values are usually ignored, resultant values are returned in a list.~%")
(hey " * null ptrs are passed and returned as #f, trailing \"user_data\" callback function arguments are optional (default: #f).~%")
(hey " * where supported, \"...\" args are passed as a list, trailing NULL or -1 should be omitted.~%")
(hey " * 'xg is added to *features*~%")
(hey " *~%")
(hey " * added funcs:~%")
(hey " *    (xg-version) -> date string.~%")
(hey " *    (->string val) interprets 'val' as a string.~%")
(hey " *    (c-array->list arr len) derefs each member of arr, returning lisp list, len=#f: null terminated array~%")
(hey " *    (list->c-array lst ctype) packages each member of list as c-type \"type\" returning (wrapped) c array~%")
(hey " *    (make-target-entry lst) returns a GtkTargetEntry table, each member of 'lst' should be (list target flags info)~%")

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
(hey " *     13-June:   folded xg-ruby.c into xg.c.~%")
(hey " *     21-Feb:    changed libxm to libxg, xm-version to xg-version.~%")
(hey " *     10-Jan:    plugged some memory leaks.~%")
(hey " *     4-Jan:     removed deprecated XEN_VECTOR_ELEMENTS.~%")
(hey " *     --------~%")
(hey " *     30-Dec:    gtk 2.6.0 changes.~%")
(hey " *     8-Dec:     added some g_log handler funcs.~%")
(hey " *     6-Dec:     added check for lost callback context.~%")
(hey " *                tightened type (pointer) checking considerably (#f only acceptable if explicit @ used in xgdata.scm).~%")
(hey " *                gtk 2.5.6 and pango 1.7.0 changes.~%")
(hey " *     3-Dec:     changed GPOINTER cast func to accept non-lists.~%")
(hey " *     15-Nov:    gtk 2.5.5 changes.~%")
(hey " *     29-Oct:    gtk 2.5.4 changes.~%")
(hey " *     27-Aug:    gtk 2.5.2 changes. removed the PANGO_ENGINE and PANGO_BACKEND stuff.~%")
(hey " *     5-Aug:     gtk 2.5.1 changes.~%")
(hey " *     21-Jul:    gtk 2.5.0 changes.~%")
(hey " *     2-Jun:     gdk_atom_name needs to free return value~%")
(hey " *     28-May:    GtkFileSelection struct support put back in -- need ok_button et al.~%")
(hey " *     14-Apr:    make-target-entry.~%")
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
(hey " *     6-Jan:     gtk 2.2 changes.~%")
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

(hey "#include <config.h>~%~%")

(hey "#if HAVE_EXTENSION_LANGUAGE~%~%")

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
(hey "#if HAVE_SCHEME~%")
(hey "  #define XG_PRE \"\"~%")
(hey "  #define XG_FIELD_PRE \".\"~%")
(hey "  #define XG_POST \"\"~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY~%")
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
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_NO_P(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "~%")
(hey "#define XM_TYPE_NO_P_2(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_PTR(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_NO_P(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_2(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "~%")
(hey "/* type checks for callback wrappers */~%")

(define (callback-p func)
  (hey "#define XEN_~A_P(Arg)  XEN_FALSE_P(Arg) || (XEN_PROCEDURE_P(Arg) && (XEN_REQUIRED_ARGS_OK(Arg, ~D)))~%"
       (symbol->string (callback-name func))
       (length (callback-args func))))

(for-each callback-p callbacks)
(with-23 hey (lambda () (for-each callback-p callbacks-23)))
(with-250 hey (lambda () (for-each callback-p callbacks-250)))
(with-255 hey (lambda () (for-each callback-p callbacks-255)))

(hey "#define XEN_lambda_P(Arg) XEN_PROCEDURE_P(Arg)~%")
(hey "#define XEN_GCallback_P(Arg) XEN_PROCEDURE_P(Arg) && ((XEN_REQUIRED_ARGS_OK(Arg, 2)) || (XEN_REQUIRED_ARGS_OK(Arg, 3)))~%")

(define (xen-callback func)
  (hey "#define XEN_TO_C_~A(Arg) XEN_FALSE_P(Arg) ? NULL : gxg_~A~%"
       (symbol->string (callback-name func))
       (callback-func func)))

(for-each xen-callback callbacks)
(with-23 hey (lambda () (for-each xen-callback callbacks-23)))
(with-250 hey (lambda () (for-each xen-callback callbacks-250)))
(with-255 hey (lambda () (for-each xen-callback callbacks-255)))

(hey "#define XEN_TO_C_GCallback(Arg) ((XEN_REQUIRED_ARGS_OK(Arg, 3)) ? (GCallback)gxg_func3 : (GCallback)gxg_func2)~%")
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
(if (not (null? types-250))
    (with-250 hey
	     (lambda ()
	       (for-each type-it (reverse types-250))
	       )))

(if (not (null? types-251))
    (with-251 hey
	     (lambda ()
	       (for-each type-it (reverse types-251))
	       )))

(if (not (null? types-252))
    (with-252 hey
	     (lambda ()
	       (for-each type-it (reverse types-252))
	       )))

(if (not (null? types-254))
    (with-254 hey
	     (lambda ()
	       (for-each type-it (reverse types-254))
	       )))

(if (not (null? types-255))
    (with-255 hey
	     (lambda ()
	       (for-each type-it (reverse types-255))
	       )))

(if (not (null? types-256))
    (with-256 hey
	     (lambda ()
	       (for-each type-it (reverse types-256))
	       )))

(if (not (null? types-260))
    (with-260 hey
	     (lambda ()
	       (for-each type-it (reverse types-260))
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
(hey "static int last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "~%")
(hey "static int xm_protect(XEN obj)~%")
(hey "{~%")
(hey "  int i, new_size;~%")
(hey "  XEN new_table;~%")
(hey "  if (last_xm_unprotect >= 0)~%")
(hey "    {~%")
(hey "      i = last_xm_unprotect;~%")
(hey "      if (XEN_FALSE_P(XEN_VECTOR_REF(xm_protected, i)))~%")
(hey "	{~%")
(hey "	  XEN_VECTOR_SET(xm_protected, i, obj);~%")
(hey "	  last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "	  return(i);~%")
(hey "	}~%")
(hey "      last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "    }~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    if (XEN_FALSE_P(XEN_VECTOR_REF(xm_protected, i)))~%")
(hey "      {~%")
(hey "	XEN_VECTOR_SET(xm_protected, i, obj);~%")
(hey "	return(i);~%")
(hey "      }~%")
(hey "  new_size = xm_protected_size * 2;~%")
(hey "  new_table = XEN_MAKE_VECTOR(new_size, XEN_FALSE);~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    {~%")
(hey "      XEN_VECTOR_SET(new_table, i, XEN_VECTOR_REF(xm_protected, i));~%")
(hey "      XEN_VECTOR_SET(xm_protected, i, XEN_FALSE);~%")
(hey "    }~%")
(hey "  XEN_VECTOR_SET(new_table, xm_protected_size, obj);~%")
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
(hey "  XEN cur, idler;~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    {~%")
(hey "      cur = XEN_VECTOR_REF(xm_protected, i);~%")
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
(hey "  XEN_VECTOR_SET(xm_protected, ind, XEN_FALSE);~%")
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
  (let ((xc (lambda (func)
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
		  ;; ctr is 0-based here
		  (if (or (and (or (eq? fname 'GtkClipboardTextReceivedFunc)
				   (eq? fname 'GtkAccelMapForeach)
				   (eq? fname 'GtkEntryCompletionMatchFunc))
			       (= ctr 1))
			  (and (or (eq? fname 'GtkTreeViewSearchEqualFunc)
				   (eq? fname 'GLogFunc))
			       (= ctr 2))
			  (and (or (eq? fname 'GtkFileFilterFunc)
				   (eq? fname 'GLogFunc))
			       (= ctr 0)))
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
	     ;; I tried to use XEN_ERROR here but it was a no-op for some reason?? 
	     (hey "if (!XEN_LIST_P((XEN)func_data)) return~A;~%  "
		  (if void? 
		      ""
		      (format #f "((~A)0)" (no-stars type))))
	     (let ((castlen (+ 12 (if (not void?) 
				      (+ 2 (string-length (format #f "return(XEN_TO_C_~A" (no-stars type))))
				      1))))
	       (if (not void?)
		   (hey "return(XEN_TO_C_~A("
			(no-stars type)))
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
		      (hey "C_TO_XEN_~A(~A~A),~%"
			   (no-stars (car arg))
			   (if (string=? (car arg) "GtkFileFilterInfo*")
			       "(GtkFileFilterInfo *)"
			       "")
			   (cadr arg))
		      (hey "XEN_CADR((XEN)func_data),~%")))
		args)
	       (hey (substring "                                                                      " 0 castlen))
	       (hey "c__FUNCTION__)")
	       (if void?
		   (hey ";~%")
		   (hey "));~%")))
	     (hey "}~%")))))))
    (for-each xc callbacks)
    (with-23 hey (lambda () (for-each xc callbacks-23)))
    (with-250 hey (lambda () (for-each xc callbacks-250)))
    (with-255 hey (lambda () (for-each xc callbacks-255)))))

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
					  (eq? (callback-gc callback-data) 'semi-permanent)))))
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
			 (if (eq? spec 'free)
			     (hey-on "  {~%   ~A result;~%   XEN rtn;~%   result = " return-type)
			     (if (eq? spec 'const-return)
				 (hey "    return(C_TO_XEN_~A((~A)" (no-stars return-type) return-type)
				 (hey-on "  return(C_TO_XEN_~A(" (no-stars return-type))))
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
			      (if (and (eq? spec 'const)
				       (or (string=? argtype "char**")
					   (string=? argtype "gchar**")
					   (string=? argtype "gchar*")
					   (string=? argtype "char*")))
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
			       (if (not (eq? spec 'free)) (heyc "))"))
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
      if (ref_actual_property_type == GDK_TARGET_STRING)~%\
	data_val = C_TO_XEN_STRING((char *)ref_data);~%\
      else if (ref_actual_length > 0) data_val = C_TO_XEN_STRINGN((char *)ref_data, ref_actual_length * ref_actual_format / 8);~%\
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
		   (hey "if (XEN_REQUIRED_ARGS_OK(func, 2))~%")
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
       (if (eq? spec 'free)
	   (hey "   rtn = C_TO_XEN_~A(result);~%   g_free(result);~%   return(rtn);~%  }~%" (no-stars return-type)))
       (hey "}~%")
       ))))

(for-each handle-func (reverse funcs))
(if (not (null? funcs-21)) (with-21 hey (lambda () (for-each handle-func (reverse funcs-21)))))
(if (not (null? funcs-22)) (with-22 hey (lambda () (for-each handle-func (reverse funcs-22)))))
(if (not (null? funcs-23)) (with-23 hey (lambda () (for-each handle-func (reverse funcs-23)))))
(if (not (null? funcs-231)) (with-231 hey (lambda () (for-each handle-func (reverse funcs-231)))))
(if (not (null? funcs-232)) (with-232 hey (lambda () (for-each handle-func (reverse funcs-232)))))
(if (not (null? funcs-234)) (with-234 hey (lambda () (for-each handle-func (reverse funcs-234)))))
(if (not (null? funcs-235)) (with-235 hey (lambda () (for-each handle-func (reverse funcs-235)))))
(if (not (null? funcs-236)) (with-236 hey (lambda () (for-each handle-func (reverse funcs-236)))))
(if (not (null? funcs-250)) (with-250 hey (lambda () (for-each handle-func (reverse funcs-250)))))
(if (not (null? funcs-251)) (with-251 hey (lambda () (for-each handle-func (reverse funcs-251)))))
(if (not (null? funcs-252)) (with-252 hey (lambda () (for-each handle-func (reverse funcs-252)))))
(if (not (null? funcs-254)) (with-254 hey (lambda () (for-each handle-func (reverse funcs-254)))))
(if (not (null? funcs-255)) (with-255 hey (lambda () (for-each handle-func (reverse funcs-255)))))
(if (not (null? funcs-256)) (with-256 hey (lambda () (for-each handle-func (reverse funcs-256)))))
(if (not (null? funcs-260)) (with-260 hey (lambda () (for-each handle-func (reverse funcs-260)))))


(hey "#define WRAPPED_OBJECT_P(Obj) (XEN_LIST_P(Obj) && (XEN_LIST_LENGTH(Obj) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Obj))))~%~%")

(define cast-it
 (lambda (cast)
   (let ((cast-name (car cast))
	 (cast-type (cadr cast)))
     (hey "static XEN gxg_~A(XEN obj)" (no-arg cast-name))
     (hey " {return((WRAPPED_OBJECT_P(obj)) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(~S), XEN_CADR(obj)) : XEN_FALSE);}~%" (no-stars cast-type)))))

(hey "static XEN gxg_GPOINTER(XEN obj)")
(hey " {return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(\"gpointer\"), (WRAPPED_OBJECT_P(obj)) ? XEN_CADR(obj) : obj));}~%")

(for-each cast-it (reverse casts))
(if (not (null? casts-21)) (with-21 hey (lambda () (for-each cast-it (reverse casts-21)))))
(if (not (null? casts-23)) (with-23 hey (lambda () (for-each cast-it (reverse casts-23)))))
(if (not (null? casts-234)) (with-234 hey (lambda () (for-each cast-it (reverse casts-234)))))
(if (not (null? casts-250)) (with-250 hey (lambda () (for-each cast-it (reverse casts-250)))))
(if (not (null? casts-252)) (with-252 hey (lambda () (for-each cast-it (reverse casts-252)))))
(if (not (null? casts-254)) (with-254 hey (lambda () (for-each cast-it (reverse casts-254)))))
(if (not (null? casts-256)) (with-256 hey (lambda () (for-each cast-it (reverse casts-256)))))

;;; checks have to use the built-in macros, not local symbol-based type checks

(define (make-check func)
  (hey "static XEN gxg_~A(XEN obj)" (no-arg (car func)))
  (hey " {return(C_TO_XEN_BOOLEAN(WRAPPED_OBJECT_P(obj) && ~A((GTypeInstance *)XEN_TO_C_ULONG(XEN_CADR(obj)))));}~%" (no-arg (car func))))

(for-each make-check (reverse checks))
(if (not (null? checks-21)) (with-21 hey (lambda () (for-each make-check (reverse checks-21)))))
(if (not (null? checks-23)) (with-23 hey (lambda () (for-each make-check (reverse checks-23)))))
(if (not (null? checks-234)) (with-234 hey (lambda () (for-each make-check (reverse checks-234)))))
(if (not (null? checks-250)) (with-250 hey (lambda () (for-each make-check (reverse checks-250)))))
(if (not (null? checks-252)) (with-252 hey (lambda () (for-each make-check (reverse checks-252)))))
(if (not (null? checks-254)) (with-254 hey (lambda () (for-each make-check (reverse checks-254)))))
(if (not (null? checks-256)) (with-256 hey (lambda () (for-each make-check (reverse checks-256)))))


(hey "~%~%/* ---------------------------------------- special functions ---------------------------------------- */~%~%")

(hey "static XEN gxg_vector2GdkPoints(XEN arg1)~%")
(hey "{~%")
(hey "  #define H_vector2GdkPoints \"(vector->GdkPoints vect) packages point data in vect as an (opaque) array of GdkPoints\"~%")
(hey "  int i, j, len;~%")
(hey "  GdkPoint *pt;~%")
(hey "  XEN_ASSERT_TYPE(XEN_VECTOR_P(arg1), arg1, XEN_ONLY_ARG, \"vector->GdkPoints\", \"vector of x,y values\");~%")
(hey "  len = XEN_VECTOR_LENGTH(arg1) / 2;~%")
(hey "  if (len <= 0) XEN_ASSERT_TYPE(0, arg1, 1, \"vector->GdkPoints\", \"positive integer\");~%")
(hey "  pt = (GdkPoint *)CALLOC(len, sizeof(GdkPoint));~%")
(hey "  for (i = 0, j = 0; i < len; i++, j += 2)~%")
(hey "    {~%")
(hey "      pt[i].x = XEN_TO_C_INT(XEN_VECTOR_REF(arg1, j));~%")
(hey "      pt[i].y = XEN_TO_C_INT(XEN_VECTOR_REF(arg1, j + 1));~%")
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
(hey "~%")
(hey "static XEN gxg_make_target_entry(XEN lst)~%")
(hey "{~%")
(hey "  GtkTargetEntry* targets;~%")
(hey "  XEN val;~%")
(hey "  int i, len;~%")
(hey "  #define H_make_target_entry \"(make-target-entry lst) -> GtkTargetEntry*, each member of 'lst' should be (list target flags info)\"~%")
(hey "  XEN_ASSERT_TYPE(XEN_LIST_P(lst), lst, XEN_ONLY_ARG, \"make-target-entry\", \"a list of lists describing each target\");~%")
(hey "  len = XEN_LIST_LENGTH(lst);~%")
(hey "  if (len == 0) return(XEN_FALSE);~%")
(hey "  targets = (GtkTargetEntry *)CALLOC(len, sizeof(GtkTargetEntry));~%")
(hey "  for (i = 0; i < len; i++)~%")
(hey "    {~%")
(hey "      val = XEN_LIST_REF(lst, i);~%")
(hey "      targets[i].target = strdup(XEN_TO_C_STRING(XEN_LIST_REF(val, 0)));~%")
(hey "      targets[i].flags = (guint)XEN_TO_C_ULONG(XEN_LIST_REF(val, 1));~%")
(hey "      targets[i].info = (guint)XEN_TO_C_ULONG(XEN_LIST_REF(val, 2));~%")
(hey "    }~%")
(hey "  return(C_TO_XEN_GtkTargetEntry_(targets));~%")
(hey "}~%")

(hey "/* ---------------------------------------- structs ---------------------------------------- */~%~%")

(hey "  #define XG_DEFINE_READER(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XG_FIELD_PRE #Name XG_POST, Value, A1, A2, A3, Help)~%")
(hey "  #if HAVE_RUBY~%")
(hey "    #define XG_DEFINE_ACCESSOR(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      XEN_DEFINE_PROCEDURE_WITH_SETTER(XG_FIELD_PRE #Name XG_POST, Value, NULL, XG_FIELD_PRE \"set_\" #Name XG_POST, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%")
(hey "  #if HAVE_SCHEME~%")
(hey "    #define XG_DEFINE_ACCESSOR(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      XEN_DEFINE_PROCEDURE_WITH_SETTER(XG_FIELD_PRE #Name XG_POST, Value, NULL, \"set! \" XG_FIELD_PRE #Name XG_POST, SetValue, A1, A2, A3, A4)~%")
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
  (hey "      ~A arr; arr = (~A)CALLOC(len + 1, sizeof(~A));~%" type type (deref-type (list type)))
  (hey "      for (i = 0; i < len; i++, val = XEN_CDR(val)) arr[i] = XEN_TO_C_~A(XEN_CAR(val));~%" (no-stars (deref-type (list type))))
  (hey "      return(XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(~S), C_TO_XEN_ULONG((unsigned long)arr), make_xm_obj(arr)));~%" (no-stars type))
  (hey "    }~%"))

(hey "/* conversions */~%")
(hey "static XEN c_array_to_xen_list(XEN val_1, XEN clen)~%")
(hey "{~%")
(hey "  XEN result = XEN_EMPTY_LIST;~%")
(hey "  XEN val;~%")
(hey "  int i, len = -1;~%")
(hey "  char *ctype;~%")
(hey "  if (XEN_INTEGER_P(clen))~%")
(hey "    len = XEN_TO_C_INT(clen);~%")
(hey "  if (!(XEN_LIST_P(val_1))) return(XEN_FALSE); /* type:location cons */~%")
(hey "  val = XEN_COPY_ARG(val_1); /* protect Ruby arg */~%")
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

;;; ---------------- argify ----------------

(define (argify-func func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs)))
    (hey "XEN_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs 10) "VARGIFY"
	     (if (> refargs 0)
		 (format #f "ARGIFY_~D" cargs)
		 (format #f "NARGIFY_~D" cargs)))
	 (car func) (car func))))
	 
(define (unargify-func func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs)))
    (hey "#define gxg_~A_w gxg_~A~%" 
	 (car func) (car func))))
	 
(hey "~%#ifdef XEN_ARGIFY_1~%")
(for-each argify-func (reverse funcs))
(if (not (null? funcs-21)) (with-21 hey (lambda () (for-each argify-func (reverse funcs-21)))))
(if (not (null? funcs-22)) (with-22 hey (lambda () (for-each argify-func (reverse funcs-22)))))
(if (not (null? funcs-23)) (with-23 hey (lambda () (for-each argify-func (reverse funcs-23)))))
(if (not (null? funcs-231)) (with-231 hey (lambda () (for-each argify-func (reverse funcs-231)))))
(if (not (null? funcs-232)) (with-232 hey (lambda () (for-each argify-func (reverse funcs-232)))))
(if (not (null? funcs-234)) (with-234 hey (lambda () (for-each argify-func (reverse funcs-234)))))
(if (not (null? funcs-235)) (with-235 hey (lambda () (for-each argify-func (reverse funcs-235)))))
(if (not (null? funcs-236)) (with-236 hey (lambda () (for-each argify-func (reverse funcs-236)))))
(if (not (null? funcs-250)) (with-250 hey (lambda () (for-each argify-func (reverse funcs-250)))))
(if (not (null? funcs-251)) (with-251 hey (lambda () (for-each argify-func (reverse funcs-251)))))
(if (not (null? funcs-252)) (with-252 hey (lambda () (for-each argify-func (reverse funcs-252)))))
(if (not (null? funcs-254)) (with-254 hey (lambda () (for-each argify-func (reverse funcs-254)))))
(if (not (null? funcs-255)) (with-255 hey (lambda () (for-each argify-func (reverse funcs-255)))))
(if (not (null? funcs-256)) (with-256 hey (lambda () (for-each argify-func (reverse funcs-256)))))
(if (not (null? funcs-260)) (with-260 hey (lambda () (for-each argify-func (reverse funcs-260)))))

(define (ruby-cast func) (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func)))) 
(hey "XEN_NARGIFY_1(gxg_GPOINTER_w, gxg_GPOINTER)~%")
(hey "XEN_NARGIFY_2(c_array_to_xen_list_w, c_array_to_xen_list)~%")
(hey "XEN_NARGIFY_2(xen_list_to_c_array_w, xen_list_to_c_array)~%")
(hey "XEN_NARGIFY_1(gxg_freeGdkPoints_w, gxg_freeGdkPoints)~%")
(hey "XEN_NARGIFY_1(gxg_vector2GdkPoints_w, gxg_vector2GdkPoints)~%")
(hey "XEN_NARGIFY_1(gxg_make_target_entry_w, gxg_make_target_entry)~%")
(hey "XEN_NARGIFY_1(c_to_xen_string_w, c_to_xen_string)~%")

(define (ruby-cast func) (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-cast (reverse casts))
(if (not (null? casts-21)) (with-21 hey (lambda () (for-each ruby-cast (reverse casts-21)))))
(if (not (null? casts-23)) (with-23 hey (lambda () (for-each ruby-cast (reverse casts-23)))))
(if (not (null? casts-234)) (with-234 hey (lambda () (for-each ruby-cast (reverse casts-234)))))
(if (not (null? casts-250)) (with-250 hey (lambda () (for-each ruby-cast (reverse casts-250)))))
(if (not (null? casts-252)) (with-252 hey (lambda () (for-each ruby-cast (reverse casts-252)))))
(if (not (null? casts-254)) (with-254 hey (lambda () (for-each ruby-cast (reverse casts-254)))))
(if (not (null? casts-256)) (with-256 hey (lambda () (for-each ruby-cast (reverse casts-256)))))

(define (ruby-check func) (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func))))
(for-each ruby-check (reverse checks))
(if (not (null? checks-21)) (with-21 hey (lambda () (for-each ruby-check (reverse checks-21)))))
(if (not (null? checks-23)) (with-23 hey (lambda () (for-each ruby-check (reverse checks-23)))))
(if (not (null? checks-234)) (with-234 hey (lambda () (for-each ruby-check (reverse checks-234)))))
(if (not (null? checks-250)) (with-250 hey (lambda () (for-each ruby-check (reverse checks-250)))))
(if (not (null? checks-252)) (with-252 hey (lambda () (for-each ruby-check (reverse checks-252)))))
(if (not (null? checks-254)) (with-254 hey (lambda () (for-each ruby-check (reverse checks-254)))))
(if (not (null? checks-256)) (with-256 hey (lambda () (for-each ruby-check (reverse checks-256)))))


(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" field field))
   struct-fields)
  (if in-x11
      (hey "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" field field)) 
   settable-struct-fields)
  (if in-x11
      (hey "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "XEN_NARGIFY_2(gxg_set_~A_w, gxg_set_~A)~%" field field)) 
   settable-struct-fields)
  (if in-x11
      (hey "#endif~%")))

(for-each (lambda (struct) 
	    (let* ((s (find-struct struct)))
	      (if (> (length (cadr s)) 0)
		  (hey "XEN_VARGIFY(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
		  (hey "XEN_NARGIFY_0(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
 (reverse make-structs))
(hey "~%")

(hey "~%#else~%")
(hey "/* not XEN_ARGIFY_1 */~%")

(for-each unargify-func (reverse funcs))
(if (not (null? funcs-21)) (with-21 hey (lambda () (for-each unargify-func (reverse funcs-21)))))
(if (not (null? funcs-22)) (with-22 hey (lambda () (for-each unargify-func (reverse funcs-22)))))
(if (not (null? funcs-23)) (with-23 hey (lambda () (for-each unargify-func (reverse funcs-23)))))
(if (not (null? funcs-231)) (with-231 hey (lambda () (for-each unargify-func (reverse funcs-231)))))
(if (not (null? funcs-232)) (with-232 hey (lambda () (for-each unargify-func (reverse funcs-232)))))
(if (not (null? funcs-234)) (with-234 hey (lambda () (for-each unargify-func (reverse funcs-234)))))
(if (not (null? funcs-235)) (with-235 hey (lambda () (for-each unargify-func (reverse funcs-235)))))
(if (not (null? funcs-236)) (with-236 hey (lambda () (for-each unargify-func (reverse funcs-236)))))
(if (not (null? funcs-250)) (with-250 hey (lambda () (for-each unargify-func (reverse funcs-250)))))
(if (not (null? funcs-251)) (with-251 hey (lambda () (for-each unargify-func (reverse funcs-251)))))
(if (not (null? funcs-252)) (with-252 hey (lambda () (for-each unargify-func (reverse funcs-252)))))
(if (not (null? funcs-254)) (with-254 hey (lambda () (for-each unargify-func (reverse funcs-254)))))
(if (not (null? funcs-255)) (with-255 hey (lambda () (for-each unargify-func (reverse funcs-255)))))
(if (not (null? funcs-256)) (with-256 hey (lambda () (for-each unargify-func (reverse funcs-256)))))
(if (not (null? funcs-260)) (with-260 hey (lambda () (for-each unargify-func (reverse funcs-260)))))

(hey "#define gxg_GPOINTER_w gxg_GPOINTER~%")
(hey "#define c_array_to_xen_list_w c_array_to_xen_list~%")
(hey "#define xen_list_to_c_array_w xen_list_to_c_array~%")
(hey "#define gxg_freeGdkPoints_w gxg_freeGdkPoints~%")
(hey "#define gxg_vector2GdkPoints_w gxg_vector2GdkPoints~%")
(hey "#define gxg_make_target_entry_w gxg_make_target_entry~%")
(hey "#define c_to_xen_string_w c_to_xen_string~%")

(define (ruby-uncast func) (hey "#define gxg_~A_w gxg_~A~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-uncast (reverse casts))
(if (not (null? casts-21)) (with-21 hey (lambda () (for-each ruby-uncast (reverse casts-21)))))
(if (not (null? casts-23)) (with-23 hey (lambda () (for-each ruby-uncast (reverse casts-23)))))
(if (not (null? casts-234)) (with-234 hey (lambda () (for-each ruby-uncast (reverse casts-234)))))
(if (not (null? casts-250)) (with-250 hey (lambda () (for-each ruby-uncast (reverse casts-250)))))
(if (not (null? casts-252)) (with-252 hey (lambda () (for-each ruby-uncast (reverse casts-252)))))
(if (not (null? casts-254)) (with-254 hey (lambda () (for-each ruby-uncast (reverse casts-254)))))
(if (not (null? casts-256)) (with-256 hey (lambda () (for-each ruby-uncast (reverse casts-256)))))

(define (ruby-uncheck func) (hey "#define gxg_~A_w gxg_~A~%" (no-arg (car func)) (no-arg (car func))))
(for-each ruby-uncheck (reverse checks))
(if (not (null? checks-21)) (with-21 hey (lambda () (for-each ruby-uncheck (reverse checks-21)))))
(if (not (null? checks-23)) (with-23 hey (lambda () (for-each ruby-uncheck (reverse checks-23)))))
(if (not (null? checks-234)) (with-234 hey (lambda () (for-each ruby-uncheck (reverse checks-234)))))
(if (not (null? checks-250)) (with-250 hey (lambda () (for-each ruby-uncheck (reverse checks-250)))))
(if (not (null? checks-252)) (with-252 hey (lambda () (for-each ruby-uncheck (reverse checks-252)))))
(if (not (null? checks-254)) (with-254 hey (lambda () (for-each ruby-uncheck (reverse checks-254)))))
(if (not (null? checks-256)) (with-256 hey (lambda () (for-each ruby-uncheck (reverse checks-256)))))

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "#define gxg_~A_w gxg_~A~%" field field))
   struct-fields)
  (if in-x11
      (hey "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "#define gxg_~A_w gxg_~A~%" field field)) 
   settable-struct-fields)
  (if in-x11
      (hey "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field) 
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "#define gxg_set_~A_w gxg_set_~A~%" field field)) 
   settable-struct-fields)
  (if in-x11
      (hey "#endif~%")))

(for-each (lambda (struct) 
	    (let* ((s (find-struct struct)))
	      (if (> (length (cadr s)) 0)
		  (hey "#define gxg_make_~A_w gxg_make_~A~%" struct struct)
		  (hey "#define gxg_make_~A_w gxg_make_~A~%" struct struct))))
 (reverse make-structs))
(hey "~%")


(hey "~%#endif~%")

;;; --------------------------------------------------------------------------------
(hey "  #define XG_DEFINE_PROCEDURE(Name, Value, A1, A2, A3, Help) XEN_DEFINE_PROCEDURE(XG_PRE #Name XG_POST, Value, A1, A2, A3, Help)~%")

(hey "static void define_functions(void)~%")
(hey "{~%")

(hey "  xm_gc_table = XEN_MAKE_VECTOR(1, XEN_FALSE);~%")
(hey "  XEN_PROTECT_FROM_GC(xm_gc_table);~%")
(hey "  xm_protected_size = 512;~%")
(hey "  xm_protected = XEN_MAKE_VECTOR(xm_protected_size, XEN_FALSE);~%")
(hey "  XEN_VECTOR_SET(xm_gc_table, 0, xm_protected);~%")

(define (defun func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs)))

    (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, ~D, ~D, ~D, H_~A);~%"
		     (car func) (car func) 
		     (if (>= cargs 10) 0 args)
		     (if (>= cargs 10) 0 refargs)
		     (if (>= cargs 10) 1 0)
		     (car func))))

(for-each defun (reverse funcs))
(if (not (null? funcs-21)) (with-21 hey (lambda () (for-each defun (reverse funcs-21)))))
(if (not (null? funcs-22)) (with-22 hey (lambda () (for-each defun (reverse funcs-22)))))
(if (not (null? funcs-23)) (with-23 hey (lambda () (for-each defun (reverse funcs-23)))))
(if (not (null? funcs-231)) (with-231 hey (lambda () (for-each defun (reverse funcs-231)))))
(if (not (null? funcs-232)) (with-232 hey (lambda () (for-each defun (reverse funcs-232)))))
(if (not (null? funcs-234)) (with-234 hey (lambda () (for-each defun (reverse funcs-234)))))
(if (not (null? funcs-235)) (with-235 hey (lambda () (for-each defun (reverse funcs-235)))))
(if (not (null? funcs-236)) (with-236 hey (lambda () (for-each defun (reverse funcs-236)))))
(if (not (null? funcs-250)) (with-250 hey (lambda () (for-each defun (reverse funcs-250)))))
(if (not (null? funcs-251)) (with-251 hey (lambda () (for-each defun (reverse funcs-251)))))
(if (not (null? funcs-252)) (with-252 hey (lambda () (for-each defun (reverse funcs-252)))))
(if (not (null? funcs-254)) (with-254 hey (lambda () (for-each defun (reverse funcs-254)))))
(if (not (null? funcs-255)) (with-255 hey (lambda () (for-each defun (reverse funcs-255)))))
(if (not (null? funcs-256)) (with-256 hey (lambda () (for-each defun (reverse funcs-256)))))
(if (not (null? funcs-260)) (with-260 hey (lambda () (for-each defun (reverse funcs-260)))))

(define (cast-out func)
  (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))

(hey "  XG_DEFINE_PROCEDURE(GPOINTER, gxg_GPOINTER_w, 1, 0, 0, NULL);~%")

(for-each cast-out (reverse casts))
(if (not (null? casts-21)) (with-21 hey (lambda () (for-each cast-out (reverse casts-21)))))
(if (not (null? casts-23)) (with-23 hey (lambda () (for-each cast-out (reverse casts-23)))))
(if (not (null? casts-234)) (with-234 hey (lambda () (for-each cast-out (reverse casts-234)))))
(if (not (null? casts-250)) (with-250 hey (lambda () (for-each cast-out (reverse casts-250)))))
(if (not (null? casts-252)) (with-252 hey (lambda () (for-each cast-out (reverse casts-252)))))
(if (not (null? casts-254)) (with-254 hey (lambda () (for-each cast-out (reverse casts-254)))))
(if (not (null? casts-256)) (with-256 hey (lambda () (for-each cast-out (reverse casts-256)))))

(hey "  XG_DEFINE_PROCEDURE(c-array->list, c_array_to_xen_list_w, 2, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(list->c-array, xen_list_to_c_array_w, 2, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(freeGdkPoints, gxg_freeGdkPoints_w, 1, 0, 0, H_freeGdkPoints);~%")
(hey "  XG_DEFINE_PROCEDURE(vector->GdkPoints, gxg_vector2GdkPoints_w, 1, 0, 0, H_vector2GdkPoints);~%")
(hey "  XG_DEFINE_PROCEDURE(->string, c_to_xen_string_w, 1, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(make-target-entry, gxg_make_target_entry_w, 1, 0, 0, H_make_target_entry);~%")

(define (check-out func)
  (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, NULL);~%" (no-arg (car func)) (no-arg (car func))))

(for-each check-out (reverse checks))
(if (not (null? checks-21)) (with-21 hey (lambda () (for-each check-out (reverse checks-21)))))
(if (not (null? checks-23)) (with-23 hey (lambda () (for-each check-out (reverse checks-23)))))
(if (not (null? checks-234)) (with-234 hey (lambda () (for-each check-out (reverse checks-234)))))
(if (not (null? checks-250)) (with-250 hey (lambda () (for-each check-out (reverse checks-250)))))
(if (not (null? checks-252)) (with-252 hey (lambda () (for-each check-out (reverse checks-252)))))
(if (not (null? checks-254)) (with-254 hey (lambda () (for-each check-out (reverse checks-254)))))
(if (not (null? checks-256)) (with-256 hey (lambda () (for-each check-out (reverse checks-256)))))

(hey "}~%~%")


(hey "static void define_structs(void)~%")
(hey "{~%~%")

(let ((in-x11 #f))
  (for-each 
   (lambda (field)
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "  XG_DEFINE_READER(~A, gxg_~A_w, 1, 0, 0, NULL);~%" field field))
   struct-fields)
  (if in-x11
      (hey "#endif~%")))

(let ((in-x11 #f))
  (for-each 
   (lambda (field)
     (if (or (member field with-x11-accessors)
	     (member field with-x11-readers))
	 (if (not in-x11)
	     (begin
	       (hey "#if (!WITH_GTK_AND_X11)~%")
	       (set! in-x11 #t)))
	 (if in-x11
	     (begin
	       (hey "#endif~%")
	       (set! in-x11 #f))))
     (hey "  XG_DEFINE_ACCESSOR(~A, gxg_~A_w, gxg_set_~A_w, 1, 0, 2, 0);~%" field field field))
   settable-struct-fields)
  (if in-x11
      (hey "#endif~%")))

(for-each 
 (lambda (struct)
   (let* ((s (find-struct struct)))
     (hey "  XG_DEFINE_PROCEDURE(~A, gxg_make_~A_w, 0, 0, ~D, NULL);~%" struct struct (if (> (length (cadr s)) 0) 1 0))))
 (reverse make-structs))

(hey "}~%~%")



(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%~%")
(hey "  #define DEFINE_INTEGER(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_INT(Name))~%")
(hey "  #define DEFINE_ULONG(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_ULONG(Name))~%")
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
(if (not (null? ints-250))
    (with-250 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-250)))))
(if (not (null? ints-251))
    (with-251 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-251)))))
(if (not (null? ints-254))
    (with-254 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-254)))))
(if (not (null? ints-256))
    (with-256 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-256)))))
(if (not (null? ints-260))
    (with-260 hey (lambda () (for-each (lambda (val) (hey "  DEFINE_INTEGER(~A);~%" val)) (reverse ints-260)))))


(for-each 
 (lambda (vals)
   (let ((val (car vals)))
     (hey "  DEFINE_ULONG(~A);~%" val)))
 (reverse ulongs))

(if (not (null? ulongs-21))
    (with-21 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-21)))))
(if (not (null? ulongs-23))
    (with-23 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-23)))))
(if (not (null? ulongs-250))
    (with-250 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-250)))))
(if (not (null? ulongs-252))
    (with-252 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-252)))))
(if (not (null? ulongs-254))
    (with-254 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-254)))))
(if (not (null? ulongs-256))
    (with-256 hey (lambda () (for-each (lambda (vals) (let ((val (car vals))) (hey "  DEFINE_ULONG(~A);~%" val))) (reverse ulongs-256)))))
     

(hey "}~%~%")

(hey "static void define_doubles(void)~%")
(hey "{~%~%")
(hey "  #define DEFINE_DOUBLE(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_DOUBLE(Name))~%")
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
(hey "  #define DEFINE_ATOM(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_GdkAtom(Name))~%")
(hey "~%")

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
(hey "  #define DEFINE_STRING(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_STRING(Name))~%")

(for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings))
(if (not (null? strings-232))
    (with-232 hey (lambda () (for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings-232)))))
(if (not (null? strings-234))
    (with-234 hey (lambda () (for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings-234)))))
(if (not (null? strings-250))
    (with-250 hey (lambda () (for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings-250)))))
(hey "}~%~%")


(hey "/* -------------------------------- initialization -------------------------------- */~%~%")
(hey "static bool xg_already_inited = false;~%~%")
(hey "#if WITH_GTK_AND_X11~%")
(hey "  void Init_libx11(void); /* xm.c */ ~%")
(hey "#endif~%~%")
(hey " void Init_libxg(void);~%")
(hey " void Init_libxg(void)~%")
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
(hey "      XEN_DEFINE(\"xg-version\", C_TO_XEN_STRING(\"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "      xg_already_inited = true;~%")
(hey "#if WITH_GTK_AND_X11~%")
(hey "      Init_libx11();~%")
(hey "#endif~%~%")
(hey "    }~%")
(hey "}~%")
(hey "#else~%")
(hey " void Init_libxg(void);~%")
(hey " void Init_libxg(void)~%")
(hey "{~%")
(hey "}~%")
(hey "#endif~%") ; have_extension_language
(hey-x11 "#endif~%")

(close-output-port xg-file)
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

