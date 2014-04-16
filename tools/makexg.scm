;;; makexg.scm creates the gtk2|3/gdk/pango/glib/cairo bindings using xgdata.scm, writes xg.c

(define xg-file (open-output-file "xg.c"))

(define (hey . args)
  (display (apply format #f args) xg-file))

(define (heyc arg)
  (display arg xg-file))

(define names (make-hash-table))
(define types ())
(define ints ())
(define dbls ())
(define funcs ())
(define casts ())
(define checks ())
(define atoms ())
(define strings ())

(define all-types ())

;;; preset some types that are getting confused
(set! types (list "GdkEventMotion*" "gdouble*" "GdkEventAny*" "GdkEvent*" "gboolean*"
		  "cairo_t*" "cairo_font_options_t*" "PangoFontDescription*"))
(set! all-types (list "GdkEventMotion*" "gdouble*" "GdkEventAny*" "GdkEvent*"
		      "cairo_t*" "cairo_font_options_t*" "PangoFontDescription*"))

(define idlers (list "g_source_remove" "g_idle_remove_by_data"
		     "gtk_quit_remove" "gtk_quit_remove_by_data" 
		     ;"gtk_key_snooper_remove"
		     ))

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
	"GtkCheckMenuItem*" "GtkClipboardTargetsReceivedFunc" 
	"GtkCombo*" "GtkComboBox*" "GtkComboBoxEntry*" "GtkContainer*" "GtkCurve*" "GtkDialog*" "GtkDrawingArea*" "GtkEditable*"
	"GtkEventBox*" "GtkExpander*" "GtkFileChooser*" "GtkFileFilterFunc"
	"GtkFileSelection*" "GtkFixed*" "GtkFontButton*" "GtkFontSelection*" "GtkFontSelectionDialog*" "GtkFrame*" "GtkGammaCurve*"
	"GtkHandleBox*" "GtkIMContextSimple*" "GtkIMMulticontext*" "GtkIconLookupFlags" "GtkImage*" "GtkImageMenuItem*" "GtkInputDialog*"
	"GtkInvisible*" "GtkItem*" "GtkItemFactoryEntry*" "GtkLabel*" "GtkLayout*" "GtkMenuDetachFunc" "GtkMenuItem*" "GtkMenuShell*"
	"GtkMessageDialog*" "GtkMisc*" "GtkNotebook*" "GtkOptionMenu*" "GtkPackType*" "GtkPaned*" "GtkPlug*"
	"GtkProgressBar*" "GtkRadioButton*" "GtkRadioMenuItem*" "GtkRadioToolButton*" "GtkRange*" "GtkRcPropertyParser" "GtkRuler*"
	"GtkScale*" "GtkScrolledWindow*" "GtkSeparatorToolItem*" "GtkSettingsValue*" "GtkSocket*" "GtkSortType*" "GtkSpinButton*"
	"GtkStatusbar*" "GtkTable*" "GtkTextCharPredicate" "GtkTextTagTableForeach" "GtkTextView*"
	"GtkToggleActionEntry*" "GtkToggleButton*" "GtkToggleToolButton*" "GtkToolButton*" "GtkToolbar*" "GtkTreeDragDest*"
	"GtkTreeDragSource*" "GtkTreeModel**" "GtkTreeModelFilter*" "GtkTreeModelSort*" "GtkTreeSortable*" "GtkUIManagerItemType"
	"GtkViewport*" "PangoAnalysis*" "PangoAttrList**" "PangoFontDescription**" "PangoFontMap*" "PangoRectangle*"
	"gchar***" "gfloat*" "gint8*" "gssize" "guint16*" "gunichar*" "GtkFileChooserButton*" "GtkPathPriorityType"
	"GtkCellView*" "GValue*" "GtkAboutDialog*" "PangoAttrFilterFunc" "PangoScript*" "GtkMenuToolButton*"
	"GtkClipboardImageReceivedFunc" "PangoMatrix*" "GdkTrapezoid*" "GdkPangoRenderer*" "PangoRenderPart"
	"GLogFunc" "GError*" "guint32*" ;"GtkRecentFilterInfo*"
	
	"GConnectFlags" "GSignalFlags" "GSignalMatchType" 
					;"GdkAxisUse" 
	"GdkFillRule" "GdkGCValuesMask"
	"GdkPropMode" "GdkRgbDither" "GdkWMFunction" "GdkWindowEdge" "GdkWindowHints" "GtkAccelFlags" ; "GtkArrowType"
	"GtkAttachOptions" "GtkCellRendererState" "GtkCurveType" "GtkDestDefaults" "GtkDestroyNotify" "GtkDialogFlags"
	"GtkDirectionType" "GtkExpanderStyle" "GtkIconLookupFlags" ;"GtkMenuPositionFunc" 
	"GtkPathType" "GtkSpinType"
	"GtkTextSearchFlags" "GtkTreeIterCompareFunc" "GtkTreeSelectionFunc" "GtkUIManagerItemType" "GtkWindowPosition"
	"PangoGlyph" "PangoUnderline" "gssize" 
	
	"GtkMenuBar*" "GtkTranslateFunc" ;"GtkMenuPositionFunc" 
	"GtkTreeIterCompareFunc" "GtkTreeSelectionFunc"
	"GtkDestroyNotify"
	
	"GtkAssistant*" "GtkRecentChooser*" "GtkRecentChooserMenu*"
	"GtkTextBufferSerializeFunc" "GtkTextBufferDeserializeFunc" 
	"GtkRecentData*" "GtkNotebookWindowCreationFunc"
	
	"GtkUnit" "GtkPageSetupDoneFunc"
	
	"GtkPrintOperationPreview*" "GtkPrintSettingsFunc" "cairo_matrix_t*"
	"cairo_font_extents_t*" "cairo_text_extents_t*" "cairo_user_data_key_t*" "cairo_destroy_func_t"
	
	"GtkPrintOperationAction"
	"GtkTooltip*" "GtkCalendarDetailFunc" "GtkScaleButton*" "GtkEntryIconPosition"
	"GdkDragAction" "GdkImageType"
	
	"gdouble*" "GdkFill" "GdkSubwindowMode" "GdkLineStyle" "GdkCapStyle" "GdkJoinStyle"
	"GtkInfoBar*" "GtkSpinner*" "GtkToolShell*" "GtkToolPalette*" "GtkToolPaletteDragTargets"
	"GdkFunction" "GtkWrapBoxPacking" "GtkLinkButton*" "GtkActivatable*" "GtkOrientable*" "GtkCellArea*"
	"GdkNativeWindow"
	"GdkRectangle*" "PangoRenderer*" "cairo_glyph_t**" "cairo_text_cluster_t**"
;	"cairo_text_cluster_flags_t" 
;	"cairo_rectangle_int_t" 
	"cairo_rectangle_t*"
	"double*"

	"GtkContainerClass*" "GtkComboBoxText*" "GtkGrid*" "GtkScrollable*" "GtkSwitch*" 
	"cairo_text_cluster_flags_t" "cairo_text_cluster_flags_t*" "cairo_rectangle_int_t*"

	"GtkOverlay*" "cairo_pattern_t**" "GtkStyleProperties*" "GtkSymbolicColor*" "GtkWidgetPath*"
	;; "GtkRecentFilterInfo*" 
	;;    this one depends on c++
	"GtkFontChooser*" "GtkFontChooserDialog*"
	"GdkModifierIntent" "guint**" "GtkApplication*" "GVariant*" "GtkApplicationWindow*"
	"GdkEventKey*" "GtkColorChooser*"

	"GtkLevelBar*" "GtkMenuButton*" "GNormalizeMode"
;	"GIcon*"

	"GBytes" "GtkPlacesSidebar*" "GtkStackSwitcher*" "GtkRevealer*" "GtkHeaderBar*" "GtkListBox*" "GtkSearchBar*"
	))

(define no-xen-p 
  (list "GdkXEvent*" "GdkVisualType*" "GError*" "GSignalInvocationHint*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*"
	"GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*"
	"GdkDeviceKey*" "GtkWidget**" "GtkLabelSelectionInfo*" "GtkItemFactoryCallback" "GtkNotebookPage*" "GtkRangeLayout*"
	"GData*" "GtkRangeStepTimer*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTextBTree*" "GtkTextLogAttrCache*"
	"GtkTableRowCol*" "GtkAccelMap*" "GtkTooltipsData*" "PangoScript" "PangoFontFace**"
	
	"GValue*" "GdkByteOrder" "GdkCrossingMode" "GdkEventType" "GdkGrabStatus" "GdkNotifyType"
					;"GdkOverlapType" 
	"GdkScrollDirection" "GdkSettingAction" "GdkVisibilityState" "GdkWindowState" "GdkWindowType"
	"GtkImageType" "GtkTreeModelFlags" "gint8" "gshort" "guint8" "lambda" "gboolean*"
	
	"time_t" ;"GtkWindowGroup*" 
	"GtkSettings*" ;"GdkDevice*" 
	"GtkScaleButton*"
	"GtkPrintOperationResult" "GtkPrintStatus" "GtkSizeRequestMode"
	"GdkEventAny*" "GdkDeviceManager*"
	"cairo_font_type_t" "cairo_pattern_type_t" "cairo_surface_type_t" "cairo_bool_t" "cairo_region_overlap_t"

	"glong" "double"
	))

(define no-xen-to-c 
  (list "GdkXEvent*" "GSignalInvocationHint*" "GdkVisualType*" "GError*" "GtkAccelGroupEntry*" "GtkIconSize*" "AtkObject*" 
	"GtkWidgetAuxInfo*" "PangoFontFamily**" "PangoFontset*" "PangoEngineShape*" "PangoLayoutRun*" "GdkDeviceAxis*" 
	"GdkDeviceKey*" "GtkWidget**" "GtkItemFactoryCallback" "GtkLabelSelectionInfo*" "GtkNotebookPage*" "GtkRangeLayout*" 
	"GtkRangeStepTimer*" "GData*" "GtkRcContext*" "GdkGC**" "GdkPixmap**" "GArray*" "GtkTableRowCol*" "GtkTextBTree*" 
	"GtkTextLogAttrCache*" "GtkAccelMap*" "GtkTooltipsData*" "PangoScript" "PangoFontFace**"
	
	"GValue*" "GdkByteOrder" "GdkCrossingMode" "GdkEventType" "GdkGrabStatus" "GdkNotifyType"
					;"GdkOverlapType" 
	"GdkScrollDirection" "GdkSettingAction" "GdkVisibilityState" "GdkWindowState" "GdkWindowType"
	"GtkImageType" "GtkTreeModelFlags" "etc" "gshort"  "gboolean*"
	
					;"GtkWindowGroup*" 
	"time_t" "GtkSettings*" ;"GdkDevice*" 
	"GtkScaleButton*"
	"GtkPrintOperationResult" "GtkPrintStatus"
	"GdkDeviceManager*" "GdkEventAny*" "GtkSizeRequestMode"
	
	"cairo_surface_type_t" "cairo_pattern_type_t" "cairo_font_type_t" "cairo_bool_t"
	"cairo_region_overlap_t" "cairo_device_type_t"

	"glong" 
	))

(define (cadr-str data)
  (let ((sp1 -1)
	(len (string-length data)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i len) (substring data sp1))
	 (if (char=? (data i) #\space)
	     (if (= sp1 -1)
		 (set! sp1 i)
		 (return (substring data (+ 1 sp1) i)))))))))

(define (caddr-str data)
  (let ((sp1 -1)
	(sp2 -1)
	(len (string-length data)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i len) (substring data sp2))
	 (if (char=? (data i) #\space)
	     (if (= sp1 -1)
		 (set! sp1 i)
		 (if (= sp2 -1)
		     (set! sp2 i)
		     (return (substring data (+ 1 sp2)))))))))))

(define (car-str data)
  (let ((len (string-length data)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i len) data)
	 (if (char=? (data i) #\space)
	     (return (substring data 0 i))))))))

(define (cdr-str data)
  (let ((len (string-length data)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i len) data)
	 (if (char=? (data i) #\space)
	     (return (substring data (+ i 1)))))))))

(define (string-upcase name)
  (let* ((len (string-length name))
	 (str (make-string len)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (set! (str i) (char-upcase (name i))))
    str))

(define (remove-if p l)
  (cond ((null? l) ())
	((p (car l)) (remove-if p (cdr l)))
	(else (cons (car l) 
		    (remove-if p (cdr l))))))

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
	   (set! ctr (+ ctr 1))))
     args)
    ctr))

(define (opt-args args)
  (let ((ctr 0))
    (for-each
     (lambda (arg)
       (if (opt-arg? arg)
	   (set! ctr (+ ctr 1))))
     args)
    ctr))

(define (deref-type arg)
  (let ((type (car arg)))
    (substring type 0 (- (string-length type) 1))))

(define (deref-element-type arg)
  (let ((type (car arg)))
    (substring type 0 (- (string-length type) 2))))

(define (deref-name arg)
  (let ((name (cadr arg)))
    (string-append "ref_" name)))

(define (derefable type)
  (let ((len (string-length type)))
    (call-with-exit
     (lambda (return)
       (do ((i (- len 1) (- i 1))
	    (ctr 0 (+ ctr 1)))
	   ((= i 0) #f)
	 (if (not (char=? (type i) #\*))
	     (return (> ctr 1))))))))

(define (has-stars type)
  (let ((len (string-length type)))
    (call-with-exit
     (lambda (return)
       (do ((i (- len 1) (- i 1))
	    (ctr 0 (+ ctr 1)))
	   ((= i 0) #f)
	 (if (char=? (type i) #\*)
	     (return #t)))
       #f))))

(define (no-stars type)
  (let ((len (string-length type))
	(val (string-copy type)))
    (do ((i 0 (+ i 1)))
	((= i len) val)
      (if (char=? (val i) #\*)
	  (set! (val i) #\_)))))

(define (no-arg-or-stars name)
  (let ((len (string-length name)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i len) name)
	 (if (or (char=? (name i) #\()
		 (char=? (name i) #\*))
	     (return (substring name 0 i))))))))

(define (parse-args args extra)
  (let ((data ())
	(sp -1)
	(type #f)
	(len (string-length args)))
    (if (string=? args "void")
	()
	(do ((i 0 (+ i 1)))
	    ((= i len) (reverse data))
	  (let ((ch (args i)))
	    (if (or (char=? ch #\space)
		    (= i (- len 1)))
		(begin
		  (if type
		      (let ((given-name (substring args (+ 1 sp) (if (= i (- len 1)) (+ i 1) i)))
			    (reftype #f))
			(if (char=? (given-name 0) #\@)
			    (set! data (cons (list type 
						   (substring given-name 1 (string-length given-name))
						   'null)
					     data))
			    (if (char=? (given-name 0) #\#)
				(set! data (cons (list type 
						       (substring given-name 1 (string-length given-name))
						       'opt)
						 data))
				(if (or (char=? (given-name 0) #\[)
					(char=? (given-name 0) #\{)
					(char=? (given-name 0) #\|))
				    (begin
				      (set! reftype (deref-type (list type)))
				      (set! data (cons (list type 
							     (substring given-name 1 (- (string-length given-name) 1))
							     given-name) 
						       data)))
				    (if (char=? (given-name 0) #\&)
					(set! data (cons (list type 
							       (substring given-name 1 (string-length given-name))
							       'set)
							 data))
					(set! data (cons (list type given-name) data))))))
			(if reftype (set! type reftype))
			
			(if (not (member type all-types))
			    (begin
			      (set! all-types (cons type all-types))
			      (case extra
				((g-2.14)     (set! types-2.14 (cons type types-2.14)))
				((g-2.16)     (set! types-2.16 (cons type types-2.16)))
				((g-2.18)     (set! types-2.18 (cons type types-2.18)))
				((g-2.20)     (set! types-2.20 (cons type types-2.20)))
				((g-3.0)      (set! types-3.0 (cons type types-3.0)))
				((g-3.2)      (set! types-3.2 (cons type types-3.2)))
				((g-3.4)      (set! types-3.4 (cons type types-3.4)))
				((g-3.6)      (set! types-3.6 (cons type types-3.6)))
				((g-3.8)      (set! types-3.8 (cons type types-3.8)))
				((g-3.10)     (set! types-3.10 (cons type types-3.10)))
				((g-3.12)     (set! types-3.12 (cons type types-3.12)))
				((cairo)      (set! cairo-types (cons type cairo-types)))
				((cairo-810)  (set! cairo-types-810 (cons type cairo-types-810)))
				((cairo-912)  (set! cairo-types-912 (cons type cairo-types-912)))
				(else  	      (if (not (member type types))
						  (set! types (cons type types)))))))
			(set! type #f))
		      (if (> i (+ 1 sp))
			  (set! type (substring args (+ 1 sp) i))))
		  (set! sp i))))))))

(define callbacks (list            
					;                       (list 'lambda2 ; unnamed gdk_window_invalidate_maybe_recurse argument (2.90.6 now)
					;			      "gboolean"
					;			      "child_func"
					;			      (parse-args "GdkWindow* window lambda_data func_info" 'callback)
					;			      'temporary)
			(list 'lambda3 ; unnamed gtk_accel_group_find argument
			      "gboolean"
			      "find_func"
			      (parse-args "GtkAccelKey* key GClosure* closure lambda_data func_info" 'callback)
			      'temporary) ; ??
			(list 'GtkCallback
			      "void"
			      "func2"
			      (parse-args "GtkWidget* w lambda_data func_info" 'callback)
			      'temporary)
			(list 'GSourceFunc
			      "gboolean"
			      "timer_func"
			      (parse-args "lambda_data func_info" 'callback)
			      'semi-permanent)
			(list 'GtkDestroyNotify
			      "void"
			      "destroy_func"
			      (parse-args "lambda_data func_info" 'callback)
			      'permanent)
			(list 'GdkFilterFunc
			      "GdkFilterReturn"
			      "filter_func"
			      (parse-args "GdkXEvent* xevent GdkEvent* event lambda_data func_info" 'callback)
			      'permanent)
			(list 'GdkEventFunc
			      "void"
			      "event_func"
			      (parse-args "GdkEvent* event lambda_data func_info" 'callback)
			      'permanent)
					;			(list 'GdkSpanFunc
					;			      "void"
					;			      "span_func"
					;			      (parse-args "GdkSpan* span lambda_data func_info" 'callback)
					;			      'temporary)
					;			(list 'GtkFunction
					;			      "gboolean"
					;			      "func1"
					;			      (parse-args "lambda_data func_info" 'callback)
					;			      'semi-permanent)
					;			(list 'GtkKeySnoopFunc
					;			      "gint"
					;			      "snoop_func"
					;			      (parse-args "GtkWidget* widget GdkEventKey* event lambda_data func_info" 'callback)
					;			      'semi-permanent)
			(list 'GtkMenuPositionFunc
			      "void"
			      "menu_position_func"
			      (parse-args "GtkMenu* menu gint* [x] gint* [y] gboolean* [push] lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkTextTagTableForeach
			      "void"
			      "text_tag_table_foreach"
			      (parse-args "GtkTextTag* tag lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkAccelMapForeach
			      "void"
			      "accel_map_foreach"
			      (parse-args "lambda_data func_info gchar* accel_path guint accel_key GdkModifierType accel_mods gboolean changed" 'callback)
			      'temporary)
			(list 'GtkTreeModelForeachFunc
			      "gboolean"
			      "model_func"
			      (parse-args "GtkTreeModel* model GtkTreePath* path GtkTreeIter* iter lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkTreeSelectionForeachFunc
			      "void"
			      "tree_selection_func"
			      (parse-args "GtkTreeModel* model GtkTreePath* path GtkTreeIter* iter lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkClipboardReceivedFunc
			      "void"
			      "clip_received"
			      (parse-args "GtkClipboard* clipboard GtkSelectionData* selection_data lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkClipboardTextReceivedFunc
			      "void"
			      "clip_text_received"
			      (parse-args "GtkClipboard* clipboard gchar* text lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkClipboardTargetsReceivedFunc
			      "void"
			      "clip_targets_received"
			      (parse-args "GtkClipboard* clipboard GdkAtom* atoms gint n_atoms lambda_data func_info" 'callback)
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
			      (parse-args "gunichar ch lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkTreeViewColumnDropFunc
			      "gboolean"
			      "tree_column"
			      (parse-args "GtkTreeView* tree_view GtkTreeViewColumn* column GtkTreeViewColumn* prev_column GtkTreeViewColumn* next_column lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkTreeViewMappingFunc
			      "void"
			      "tree_mapping"
			      (parse-args "GtkTreeView* tree_view GtkTreePath* path lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkTreeViewSearchEqualFunc
			      "gboolean"
			      "tree_search"
			      (parse-args "GtkTreeModel* model gint column gchar* key GtkTreeIter* iter lambda_data func_info" 'callback)
			      'temporary)
			(list 'GtkTreeCellDataFunc
			      "void"
			      "cell_data"
			      (parse-args "GtkTreeViewColumn* tree_column GtkCellRenderer* cell GtkTreeModel* tree_model GtkTreeIter* iter lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkTreeIterCompareFunc
			      "gint"
			      "iter_compare"
			      (parse-args "GtkTreeModel* model GtkTreeIter* a GtkTreeIter* b lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkTreeSelectionFunc
			      "gboolean"
			      "tree_selection"
			      (parse-args "GtkTreeSelection* selection GtkTreeModel* model GtkTreePath* path gboolean path_currently_selected lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkClipboardGetFunc
			      "void"
			      "clip_get"
			      (parse-args "GtkClipboard* clipboard GtkSelectionData* selection_data guint info lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkClipboardClearFunc
			      "void"
			      "clip_clear"
			      (parse-args "GtkClipboard* clipboard lambda_data func_info" 'callback)
			      'permanent)
			
					; GCallback 'lambda can be whatever is indicated by caller (2 or more args)
			
			(list 'GtkFileFilterFunc
			      "gboolean"
			      "file_filter"
			      (parse-args "GtkFileFilterInfo* info lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkEntryCompletionMatchFunc
			      "gboolean"
			      "entry_completion_match"
			      (parse-args "GtkEntryCompletion* completion gchar* key GtkTreeIter* iter lambda_data func_info" 'callback)
			      'permanent)
			
			(list 'GtkTreeViewRowSeparatorFunc
			      "gboolean"
			      "row_separator"
			      (parse-args "GtkTreeModel* model GtkTreeIter* iter lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkIconViewForeachFunc
			      "void"
			      "icon_view_foreach"
			      (parse-args "GtkIconView* icon_view GtkTreePath* path lambda_data func_info" 'callback)
			      'permanent)
			
			(list 'GtkClipboardImageReceivedFunc
			      "void"
			      "clip_image_received"
			      (parse-args "GtkClipboard* clipboard GdkPixbuf* pixbuf lambda_data func_info" 'callback) ; 'callback)
			      ;; these arg types are not new in 256, but this parse-args precedes the basic ones, so comment out the callback
			      ;; the problem here (and below callback) is that parse-args sees a new type (new to it so far),
			      ;;   and chooses which type list to put it on based on the "extra" arg -- since these types
			      ;;   are not new in version 2.5.6, we don't want the callback flag to sequester them
			      ;;   on the 256-type list.
			      'permanent)
			(list 'GLogFunc
			      "void"
			      "g_message_log_func"
			      (parse-args "gchar* domain GLogLevelFlags log_level gchar* message lambda_data func_info" 'callback)
			      'permanent)
			
			(list 'GtkClipboardRichTextReceivedFunc
			      "void"
			      "clip_rich_text_received"
			      (parse-args "GtkClipboard* clipboard GdkAtom format guint8* text gsize length lambda_data func_info" 'callback); 'callback)
			      ;; guint8* is const
			      'permanent-gcc)
			(list 'GtkRecentFilterFunc
			      "gboolean"
			      "recent_filter"
			      (parse-args "GtkRecentFilterInfo* filter_info lambda_data func_info" 'callback)
			      ;; const filter info
			      'permanent-gcc)
			(list 'GtkTreeViewSearchPositionFunc
			      "void"
			      "search_position"
			      (parse-args "GtkTreeView* tree_view GtkWidget* search_dialog lambda_data func_info" 'callback)
			      'permanent)
			(list 'GtkAssistantPageFunc
			      "gint"
			      "page_func"
			      (parse-args "gint current_page lambda_data func_info" 'callback)
			      'permanent)
					;			(list 'GtkLinkButtonUriFunc
					;			      "void"
					;			      "link_button_uri"
					;			      (parse-args "GtkLinkButton* button gchar* link lambda_data func_info" 'callback)
					;			      ;; const gchar *link
					;			      'permanent)
			(list 'GtkRecentSortFunc
			      "gint"
			      "recent_sort"
			      (parse-args "GtkRecentInfo* a GtkRecentInfo* b lambda_data func_info" 'callback)
			      'permanent)
			))


(define callback-name car)
(define callback-type cadr)
(define callback-func caddr)
(define callback-args cadddr)
(define (callback-gc func) (func 4))

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
	(cons "guchar*" "String") ; added 30-Jul-02 then removed then put back... -- this is a real mess!

	(cons "guint" "ULONG")
	(cons "guint16" "INT")
	(cons "gint" "INT")
	(cons "gshort" "INT")
					;	(cons "gint16" "INT")
					;	(cons "guint8" "INT")
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
					;(cons "GdkAxisUse" "INT")
	(cons "GdkGCValuesMask" "INT")
	(cons "GdkFill" "INT")
	(cons "GdkFunction" "INT")
	(cons "GdkLineStyle" "INT")
	(cons "GdkCapStyle" "INT")
	(cons "GdkJoinStyle" "INT")
	(cons "GdkGrabStatus" "INT")
	(cons "GdkEventMask" "INT")
	(cons "GdkImageType" "INT")
					;(cons "GdkInputSource" "INT")
					;(cons "GdkInputMode" "INT")
	(cons "GdkNativeWindow" "ULONG")
	(cons "GdkModifierType" "INT")
					;(cons "GdkExtensionMode" "INT")
	(cons "PangoDirection" "INT")
	(cons "GdkRgbDither" "INT")
	(cons "GdkPixbufAlphaMode" "INT")
	(cons "GdkPropMode" "INT")
	(cons "GdkFillRule" "INT")
					;(cons "GdkOverlapType" "INT")
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
	(cons "GtkStateFlags" "INT")
	(cons "GtkImageType" "INT")
;	(cons "GtkIconSize" "INT")
	(cons "GtkJustification" "INT")
	(cons "GtkMessageType" "INT")
	(cons "GtkButtonsType" "INT")
	(cons "GtkTargetFlags" "INT")
					;(cons "GtkProgressBarOrientation" "INT")
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
					;(cons "GtkToolbarChildType" "INT")
	(cons "GtkToolbarStyle" "INT")
	(cons "GtkTreeModelFlags" "INT")
	(cons "GtkSelectionMode" "INT")
	(cons "GtkSortType" "INT")
	(cons "GtkDirectionType" "INT")
	(cons "GtkWindowType" "INT")
	(cons "GdkWindowTypeHint" "INT")
	(cons "GdkGravity" "INT")
	(cons "GdkWindowHints" "INT")
	(cons "GtkSizeRequestMode" "INT")
	
	(cons "GtkEntryIconPosition" "INT")
	
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
	(cons "PangoScript" "INT")
	
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
	(cons "GtkIconThemeError" "INT")
	(cons "GtkScrollStep" "INT")
	(cons "GLogLevelFlags" "INT")
	(cons "GtkPackDirection" "INT")
	(cons "GtkIconViewDropPosition" "INT")
	(cons "GtkFileChooserConfirmation" "INT")
	(cons "GtkFileChooserProp" "INT")
	(cons "GtkFileChooserError" "INT")
	(cons "GtkLicense" "INT")

	(cons "GtkWrapAllocationMode" "INT")
	(cons "GtkWrapBoxSpreading" "INT")
	(cons "GtkWrapBoxPacking" "INT")
	
	(cons "GtkSensitivityType" "INT")
	(cons "GtkTextBufferTargetInfo" "INT")
	(cons "GtkAssistantPageType" "INT")
	(cons "GtkCellRendererAccelMode" "INT")
	(cons "GtkRecentSortType" "INT")
	(cons "GtkRecentChooserError" "INT")
	(cons "GtkRecentFilterFlags" "INT")
	(cons "GtkRecentManagerError" "INT")
	(cons "GtkTreeViewGridLines" "INT")

	(cons "GNormalizeMode" "INT")
	(cons "gunichar" "INT")
	(cons "gunichar*" "String")	
					;(cons "GtkPrintCapabilities" "INT")
	(cons "GtkPrintStatus" "INT")
	(cons "GtkPrintOperationResult" "INT")
	(cons "GtkPrintOperationAction" "INT")
	(cons "GtkPrintError" "INT")
	
	(cons "cairo_status_t" "INT")
	(cons "cairo_content_t" "INT")
	(cons "cairo_operator_t" "INT")
	(cons "cairo_antialias_t" "INT")
	(cons "cairo_fill_rule_t" "INT")
	(cons "cairo_line_cap_t" "INT")
	(cons "cairo_line_join_t" "INT")
	(cons "cairo_font_slant_t" "INT")
	(cons "cairo_font_weight_t" "INT")
	(cons "cairo_subpixel_order_t" "INT")
	(cons "cairo_hint_style_t" "INT")
	(cons "cairo_hint_metrics_t" "INT")
	(cons "cairo_font_type_t" "INT")
	(cons "cairo_path_data_type_t" "INT")
	(cons "cairo_surface_type_t" "INT")
	(cons "cairo_format_t" "INT")
	(cons "cairo_pattern_type_t" "INT")
	(cons "cairo_extend_t" "INT")
	(cons "cairo_filter_t" "INT")
	(cons "cairo_bool_t" "INT")

	(cons "bool" "BOOLEAN")
	
					;(cons "PangoRenderPart" "INT")
	(cons "PangoTabAlign" "INT")
	(cons "GtkWidgetHelpType" "INT")
	(cons "GtkWidgetFlags" "INT")
	(cons "GtkRcTokenType" "INT")
					;(cons "GtkNotebookTab" "INT")
	(cons "GtkScrollType" "INT")
	(cons "GtkMovementStep" "INT")
	(cons "GtkMenuDirectionType" "INT")
	(cons "GtkDeleteType" "INT")
	(cons "GtkResponseType" "INT")
	(cons "GdkInterpType" "INT")
					;(cons "GdkPixbufError" "INT")
					;(cons "GdkColorspace" "INT")
	(cons "GdkWindowAttributesType" "INT")
					;(cons "GdkWindowClass" "INT")
	(cons "GdkStatus" "INT")
	(cons "GdkSubwindowMode" "INT")
	(cons "GdkPropertyState" "INT")
	(cons "GtkScrollablePolicy" "INT")

	(cons "GdkModifierIntent" "INT")
	))

(define (c-to-xen-macro-name typ str)
  (if (string=? str "INT") "C_int_to_Xen_integer"
      (if (string=? str "DOUBLE") "C_double_to_Xen_real"
	  (if (string=? str "BOOLEAN") "C_bool_to_Xen_boolean"
	      (if (string=? str "ULONG") "C_ulong_to_Xen_ulong"
		  (if (string=? str "String") 
		      (if (string=? (car typ) "guchar*") 
			  "C_to_Xen_String"
			  "C_string_to_Xen_string")
		      (format #f "~A unknown" str)))))))

(define (xen-to-c-macro-name str)
  (if (string=? str "INT") "Xen_integer_to_C_int"
      (if (string=? str "DOUBLE") "Xen_real_to_C_double"
	  (if (string=? str "BOOLEAN") "Xen_boolean_to_C_bool"
	      (if (string=? str "ULONG") "Xen_ulong_to_C_ulong"
		  (if (string=? str "String") "Xen_string_to_C_string"
		      (format #f "~A unknown" str)))))))

(define (type-it type)
  (let ((typ (assoc type direct-types)))
    (if typ
	(if (cdr typ)
	    (begin
	      (if (string? (cdr typ))
		  (begin
		    (if (not (member type no-c-to-xen))
			(hey "#define C_to_Xen_~A(Arg) ~A(Arg)~%" (no-stars (car typ)) (c-to-xen-macro-name typ (cdr typ))))
		    (if (not (member type no-xen-to-c))
			(hey "#define Xen_to_C_~A(Arg) (~A)(~A(Arg))~%" (no-stars (car typ)) (car typ) (xen-to-c-macro-name (cdr typ))))
		    (if (not (member type no-xen-p))
			(hey "#define Xen_is_~A(Arg) Xen_is_~A(Arg)~%" 
			     (no-stars (car typ))
			     (if (string=? (cdr typ) "INT") 
				 "integer" 
				 (if (string=? (cdr typ) "DOUBLE")
				     "number"
				     (if (string=? (cdr typ) "ULONG")
					 "ulong"
					 (apply string (map char-downcase (cdr typ)))))))))
		  (begin
		    (if (not (cdr typ)) ; void special case
			(begin
			  (if (not (member type no-xen-p))
			      (hey "#define Xen_is_~A(Arg) 1~%" (no-stars (car typ))))
			  (if (not (member type no-xen-to-c))
			      (hey "#define Xen_to_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ)))))
			(begin          ; xen special case
			  (if (string=? type "etc")
			      (hey "#define Xen_is_etc(Arg) (Xen_is_list(Arg))~%")
			      (begin
				(if (not (member type no-xen-p))
				    (hey "#define Xen_is_~A(Arg) ((Xen_is_list(Arg)) && (Xen_list_length(Arg) > 2))~%" (no-stars (car typ))))
				(if (not (member type no-xen-to-c))
				    (hey "#define Xen_to_C_~A(Arg) ((gpointer)Arg)~%" (no-stars (car typ))))))))))))
	(if (and (not (string=? type "lambda"))
		 (not (string=? type "lambda_data"))
		 (not (string=? type "GError*"))
		 (not (find-callback 
		       (lambda (func)
			 (string=? type (symbol->string (car func))))))
		 (not (string=? type "GCallback")))
	    (begin
	      (hey "Xm_type~A(~A, ~A)~%" 
		   (if (or (has-stars type) 
			   (string=? type "gpointer")
			   (string=? type "GClosureNotify"))
		       (if (member type no-c-to-xen)
			   "_Ptr_1"
			   (if (member type no-xen-p)
			       (if (member type no-xen-to-c)
				   "_Ptr_2"
				   "_Ptr_no_P")
			       (if (or (string=? type "guint8*")
				       (string=? type "GtkRecentFilterInfo*"))
				   "_Ptr_const"
				   "_Ptr")))
		       (if (member type no-c-to-xen)
			   "_1"
			   (if (member type no-xen-p)
			       (if (member type no-xen-to-c)
				   "_no_p_2"
				   "_no_p")
			       "")))
		   (no-stars type) 
		   type))))))

(define (func-type strs)
  (call-with-exit
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
  (format #t str arg))


(define-macro (make-fnc vname)
  (let* ((cfnc-name (string-append "CFNC-" vname))
	 (cfnc (string->symbol cfnc-name))
	 (g-fnc (string->symbol (string-append "g-" vname)))
	 (types (string->symbol (string-append "types-" vname)))
	 (funcs (string->symbol (string-append "funcs-" vname)))
	 (strfnc (string->symbol (string-append "CSTR-" vname)))
	 (strings (string->symbol (string-append "strings-" vname)))
	 (names (string->symbol (string-append "names-" vname)))
	 (intfnc (string->symbol (string-append "CINT-" vname)))
	 (ints (string->symbol (string-append "ints-" vname)))
	 (castfnc (string->symbol (string-append "CCAST-" vname)))
	 (casts (string->symbol (string-append "casts-" vname)))
	 (chkfnc (string->symbol (string-append "CCHK-" vname)))
	 (checks (string->symbol (string-append "checks-" vname)))
	 (withfnc (string->symbol (string-append "with-" vname)))
	 )
    `(begin
       (define ,funcs ())
       (define ,strings ())
       (define ,ints ())
       (define ,names ())
       (define ,types ())
       (define ,casts ())
       (define ,checks ())

       (define* (,cfnc data spec)         ; CFNC-2.12
	 (let ((name (cadr-str data))
	       (args (caddr-str data)))
	   (if (hash-table-ref names name)
	       (format #t "~A: ~A ~A~%" ',cfnc name data)
	       (let ((type (car-str data)))
		 (if (not (member type all-types))
		     (begin
		       (set! all-types (cons type all-types))
		       (set! ,types (cons type ,types))))
		 (let ((strs (parse-args args ',g-fnc)))
		   (if spec
		       (set! ,funcs (cons (list name type strs args spec) ,funcs))
		       (set! ,funcs (cons (list name type strs args) ,funcs)))
		   (hash-table-set! names name (func-type strs)))))))

       (define (,strfnc name)            ; CSTR-2.12
	 (if (assoc name ,names)
	     (format #t "~A ~A~%" name ',strfnc)
	     (begin
	       (set! ,strings (cons name ,strings))
	       (set! ,names (cons (cons name 'string) ,names)))))

       (define* (,intfnc name type)      ; CINT-2.12
	 (save-declared-type type)
	 (if (hash-table-ref names name)
	     (format #t "~A ~A~%" name ',intfnc)
	     (begin
	       (set! ,ints (cons name ,ints))
	       (hash-table-set! names name 'int))))

       (define (,castfnc name type)      ; CCAST-2.12
	 (if (hash-table-ref names name)
	     (format #t "~A ~A~%" name ',castfnc)
	     (begin
	       (set! ,casts (cons (list name type) ,casts))
	       (hash-table-set! names name 'def))))

       (define (,chkfnc name type)       ; CCHK-2.12
	 (if (hash-table-ref names name)
	     (format #t "~A ~A~%" name ',chkfnc)
	     (begin
	       (set! ,checks (cons (list name type) ,checks))
	       (hash-table-set! names name 'def))))

       (define (,withfnc dpy thunk)      ; with-2.12
	 (dpy (string-append "#if GTK_CHECK_VERSION(" (substring ,vname 0 1) ", " (substring ,vname 2) ", 0)~%"))
	 (thunk)
	 (dpy "#endif~%~%"))
       
       )))


(make-fnc "2.14")
(make-fnc "2.16")
(make-fnc "2.18")
(make-fnc "2.20")
(make-fnc "3.0") 
(make-fnc "3.2")
(make-fnc "3.4")
(make-fnc "3.6")
(make-fnc "3.8")
(make-fnc "3.10")
(make-fnc "3.12")

(define structs ())
(define make-structs ()) ; these have a xg-specific make function
(define cairo-make-structs ())
(define struct-fields ())
(define settable-struct-fields ())
(define make-structs-3.0 ()) ; these have a xg-specific make function


(define cairo-funcs ())
(define cairo-png-funcs ())
(define cairo-ints ())
(define cairo-types ())

(define cairo-funcs-810 ())
(define cairo-ints-810 ())
(define cairo-types-810 ())

(define cairo-funcs-912 ())
(define cairo-ints-912 ())
(define cairo-types-912 ())
(define cairo-strings-912 ())
(define cairo-names-912 ())

(define* (CFNC data spec spec-data) ; 'const -> const for arg cast, 'etc for ... args, 'free -> must free C val before return
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "~A CFNC~%" name)
	(let ((type (car-str data)))
	  (if (not (member type all-types)) (set! all-types (cons type all-types)))
	  (if (not (member type types))
	      (set! types (cons type types)))
	  (let ((strs (parse-args args 'ok)))
	    (if spec
		(set! funcs (cons (list name type strs args spec spec-data) funcs))
		(set! funcs (cons (list name type strs args) funcs)))
	    (hash-table-set! names name (func-type strs)))))))

(define (CFNC-PA data min-len max-len types)
  (CFNC data 'etc (list min-len max-len types)))

(define (CFNC-23-PA data min-len max-len types)
  (CFNC data 'etc (list min-len max-len types)))

(define* (CAIRO-FUNC data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-FUNC: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types (cons type cairo-types))))
	  (let ((strs (parse-args args 'cairo)))
	    (if spec
		(set! cairo-funcs (cons (list name type strs args spec) cairo-funcs))
		(set! cairo-funcs (cons (list name type strs args) cairo-funcs)))
	    (hash-table-set! names name (func-type strs)))))))

(define* (CAIRO-PNG-FUNC data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-PNG-FUNC: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types (cons type cairo-types))))
	  (let ((strs (parse-args args 'cairo)))
	    (if spec
		(set! cairo-png-funcs (cons (list name type strs args spec) cairo-png-funcs))
		(set! cairo-png-funcs (cons (list name type strs args) cairo-png-funcs)))
	    (hash-table-set! names name (func-type strs)))))))

(define* (CAIRO-FUNC-810 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-FUNC-810: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types-810 (cons type cairo-types-810))))
	  (let ((strs (parse-args args 'cairo-810)))
	    (if spec
		(set! cairo-funcs-810 (cons (list name type strs args spec) cairo-funcs-810))
		(set! cairo-funcs-810 (cons (list name type strs args) cairo-funcs-810)))
	    (hash-table-set! names name (func-type strs)))))))

(define* (CAIRO-FUNC-912 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (hash-table-ref names name)
	(no-way "CAIRO-FUNC-912: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! cairo-types-912 (cons type cairo-types-912))))
	  (let ((strs (parse-args args 'cairo-912)))
	    (if spec
		(set! cairo-funcs-912 (cons (list name type strs args spec) cairo-funcs-912))
		(set! cairo-funcs-912 (cons (list name type strs args) cairo-funcs-912)))
	    (hash-table-set! names name (func-type strs)))))))


(define (helpify name type args)
  (let* ((initial (format #f "  #define H_~A \"~A ~A(" name type name))
	 (line-len (string-length initial))
	 (len (string-length args))
	 (typed #f)
	 (help-max 100))
    (hey initial)
    (do ((i 0 (+ i 1)))
	((= i len))
      (let ((ch (args i)))
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
		  (set! line-len (+ 1 line-len))
		  (heyc " ")
		  (set! typed #t)))
	    (if (and (not (char=? ch #\@))
		     (not (char=? ch #\#)))
		(begin
		  (set! line-len (+ 1 line-len))
		  (heyc ch))))))
    (hey ")\"~%")))

(define (CATOM name)
  (if (hash-table-ref names name)
      (no-way "~A CATOM~%" name)
      (begin
	(set! atoms (cons name atoms))
	(hash-table-set! names name 'atom))))


(define (CSTR name)
  (if (hash-table-ref names name)
      (no-way "~A CSTR~%" name)
      (begin
	(set! strings (cons name strings))
	(hash-table-set! names name 'string))))


(define (CDBL name)
  (if (hash-table-ref names name)
      (no-way "~A CDBL~%" name)
      (begin
	(set! dbls (cons name dbls))
	(hash-table-set! names name 'dbl))))

(define declared-types ())
(define (save-declared-type type)
  (if (and type
	   (not (member type declared-types)))
      (set! declared-types (cons type declared-types))))

(define* (CINT name type)
  (save-declared-type type)
  (if (hash-table-ref names name)
      (no-way "~A CINT~%" name)
      (begin
	(set! ints (cons name ints))
	(hash-table-set! names name 'int))))


(define* (CAIRO-INT name type)
  (save-declared-type type)
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-INT~%" name)
      (begin
	(set! cairo-ints (cons name cairo-ints))
	(hash-table-set! names name 'int))))

(define* (CAIRO-INT-810 name type)
  (save-declared-type type)
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-INT-810~%" name)
      (begin
	(set! cairo-ints-810 (cons name cairo-ints-810))
	(hash-table-set! names name 'int))))

(define* (CAIRO-INT-912 name type)
  (save-declared-type type)
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-INT-912~%" name)
      (begin
	(set! cairo-ints-912 (cons name cairo-ints-912))
	(hash-table-set! names name 'int))))

(define (CAIRO-STRING-912 name)
  (if (hash-table-ref names name)
      (no-way "~A CAIRO-STRING-912~%" name)
      (begin
	(set! cairo-strings-912 (cons name cairo-strings-912))
	(hash-table-set! names name 'string))))


(define (CCAST name type) ; this is the cast (type *)obj essentially but here it's (list type* (cadr obj))
  (if (hash-table-ref names name)
      (no-way "~A CCAST~%" name)
      (begin
	;;(if (not (member type types))
	;;    (set! types (cons type types)))
	(set! casts (cons (list name type) casts))
	(hash-table-set! names name 'def))))


(define (CCHK name type)
  (if (hash-table-ref names name)
      (no-way "~A CCHK~%" name)
      (begin
	(set! checks (cons (list name type) checks))
	(hash-table-set! names name 'def))))


(define (STRUCT data)
  (let ((name (car-str data)) ; struct name (type)
	(args (cdr-str data)))
    (if (hash-table-ref names name)
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

(define (STRUCT-3.0-make data)
  (STRUCT data)
  (set! make-structs-3.0 (cons (car-str data) make-structs-3.0)))

(define (CAIRO-STRUCT-make data)
  (STRUCT data) ; fields not needed currently
  (set! cairo-make-structs (cons (car-str data) cairo-make-structs)))

(define (find-struct name)
  (call-with-exit
   (lambda (return)
     (for-each
      (lambda (struct)
	(if (string=? name (car struct))
	    (return struct)))
      structs))))

(define (no-arg name)
  (let ((len (string-length name)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i len) name)
	 (if (char=? (name i) #\()
	     (return (substring name 0 i))))))))

;;; ---------------------------------------- read data ---------------------------------------- 
(load "xgdata.scm")

					;(define listable-types (list "gint8*" "int*" "gint*" "gdouble*"))
(define listable-types ())
(for-each
 (lambda (type)
   (let* ((len (string-length type))
	  (dereftype (if (and (char=? (type (- len 1)) #\*)
			      (not (string=? type "char*")) ; these are surely strings (and set would need Xen_to_C_gchar etc)
			      (not (string=? type "GError*"))
			      (not (string=? type "GError**"))
			      (not (string=? type "gchar*")))
			 (substring type 0 (- len 1)) 
			 #f)))
     (if (and dereftype
	      (assoc dereftype direct-types))
	 (set! listable-types (cons type listable-types)))))
 types)

(define (with-cairo dpy thunk)
  (thunk)
  )

(define (with-cairo-png dpy thunk)
  (thunk)
  )

(define (with-cairo-810 dpy thunk)
  (dpy "#if HAVE_CAIRO_1_8~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-cairo-912 dpy thunk)
  (dpy "#if HAVE_CAIRO_1_9_12 && GTK_CHECK_VERSION(3, 0, 0)~%")
  (thunk)
  (dpy "#endif~%~%"))



(define all-ntypes (list types-2.14 types-2.16 types-2.18 types-2.20 
			types-3.0 types-3.2 types-3.4 types-3.6 types-3.8 types-3.10 types-3.12
			cairo-types cairo-types-810 cairo-types-912))
(define all-ntype-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			     with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12
			     with-cairo with-cairo-810 with-cairo-912))

(define all-funcs (list funcs-2.14 funcs-2.16 funcs-2.18 funcs-2.20 
			funcs-3.0 funcs-3.2 funcs-3.4 funcs-3.6 funcs-3.8 funcs-3.10 funcs-3.12
			cairo-funcs cairo-png-funcs cairo-funcs-810 cairo-funcs-912))
(define all-func-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			     with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12
			     with-cairo with-cairo-png with-cairo-810 with-cairo-912))

(define all-ints (list ints-2.14 ints-2.16 ints-2.18  
		       ints-3.0 ints-3.2 ints-3.4 ints-3.6 ints-3.8 ints-3.10 ints-3.12
		       cairo-ints cairo-ints-810 cairo-ints-912))
(define all-int-withs (list with-2.14 with-2.16 with-2.18 
			    with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12
			    with-cairo with-cairo-810 with-cairo-912))

(define all-casts (list casts-2.14 casts-2.16 casts-2.18 casts-2.20 
			casts-3.0 casts-3.2 casts-3.4 casts-3.6 casts-3.8 casts-3.10 casts-3.12))
(define all-cast-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			     with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12))

(define all-checks (list checks-2.14 checks-2.16 checks-2.18 checks-2.20 
			 checks-3.0 checks-3.2 checks-3.4 checks-3.6 checks-3.8 checks-3.10 checks-3.12))
(define all-check-withs (list with-2.14 with-2.16 with-2.18 with-2.20 
			      with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12))

(define all-strings (list strings-2.14 strings-2.16 
			  strings-3.0 strings-3.2 strings-3.4 strings-3.6 strings-3.8 strings-3.10 strings-3.12 cairo-strings-912))
(define all-string-withs (list with-2.14 with-2.16 
			       with-3.0 with-3.2 with-3.4 with-3.6 with-3.8 with-3.10 with-3.12 with-cairo-912))



;;; ---------------------------------------- write output file ----------------------------------------
(hey "/* xg.c: s7, Ruby, and Forth bindings for gtk/pango/cairo, some of glib~%")
(hey " *   this file generated automatically from makexg.scm and xgdata.scm~%")
(hey " *   needs xen.h~%")
(hey " *~%")
(hey " * reference args initial values are usually ignored, resultant values are returned in a list.~%")
(hey " * null ptrs are passed and returned as #f, trailing \"user_data\" callback function arguments are optional (default: #f).~%")
(hey " * where supported, \"...\" args are passed as a list, trailing NULL or -1 should be omitted.~%")
(hey " * 'xg is added to *features*~%")
(hey " *~%")
(hey " * added funcs:~%")
(hey " *    (xg-version): date string.~%")
(hey " *    (->string val) interprets 'val' as a string.~%")
(hey " *    (c-array->list arr len) derefs each member of arr, returning lisp list, len=#f: null terminated array~%")
(hey " *    (list->c-array lst ctype) packages each member of list as c-type \"type\" returning (wrapped) c array~%")
(hey " *    (make-target-entry lst) returns a GtkTargetEntry table, each member of 'lst' should be (list target flags info)~%")

(for-each
 (lambda (name)
   (let ((args (cadr (find-struct name))))
     (hey " *    (~A" name)
     (for-each
      (lambda (str)
	(hey " ~A" (cadr str)))
      args)
     (hey "): ~A struct~%" name)))
 (reverse make-structs))

(if (not (null? cairo-make-structs))
    (for-each
     (lambda (name)
       (let ((args (cadr (find-struct name))))
	 (hey " *    (~A" name)
	 (for-each
	  (lambda (str)
	    (hey " ~A" (cadr str)))
	  args)
	 (hey "): ~A struct (if cairo)~%" name)))
     (reverse cairo-make-structs)))

(hey " *~%")
(hey " * omitted functions and macros:~%")
(hey " *     anything with a va_list or GtkArg* argument.~%")
(hey " *     most of the unusual keysym names~%")
(hey " *     all *_CLASS, *_IFACE macros, *_get_type functions~%")
(hey " *     win32-specific functions~%")
(hey " *~%")
(hey " * HISTORY:~%")
(hey " *~%")
(hey " *     16-Apr:    changed max-args to 8.~%")
(hey " *     6-Mar:     changed most macros.~%")
(hey " *     21-Feb-14: changed _p to _is_.~%")
(hey " *     --------~%")
(hey " *     3-Sep:     use symbol directly in type checks, not the symbol name.~%")
(hey " *     18-Aug:    changed the gtk version macros to reflect the version number.~%")
(hey " *     7-Jun-13:  added mixed arg types to the ... arg lists.~%")
(hey " *     --------~%")
(hey " *     19-Aug-10: removed lots of Gdk stuff -- we assume Gtk 2.9 and cairo now.~%")
(hey " *     28-Jan-10: removed the rest of the struct accessors.~%")
(hey " *     --------~%")
(hey " *     16-Dec-09: removed Guile support.~%")
(hey " *     --------~%")
(hey " *     16-Oct:    removed Gauche support.~%")
(hey " *     1-Sep:     s7 support.~%")
(hey " *     8-Jul-08:  started removing all struct accessors (for Gtk 3).~%")
(hey " *     --------~%")
(hey " *     9-Mar:     removed all *_get_type functions (nearly 300!).~%")
(hey " *     5-Mar-07:  cairo and more gtkprint.~%")
(hey " *     --------~%")
(hey " *     26-Aug:    removed --with-x11, WITH_GTK_AND_X11, xg-x11.h.~%")
(hey " *     4-Aug:     added a form of g_object_get and gtk_settings_get_for_screen.~%")
(hey " *     20-Jul:    gtkprint stuff.~%")
(hey " *     17-Jul:    g_signal_connect and other related macros.~%")
(hey " *     21-Apr:    Gauche support.~%")
(hey " *     29-Mar:    Forth support.~%")
(hey " *     7-Mar-06:  if g_set_error, return the error message, not the GError pointer~%")
(hey " *     --------~%")
(hey " *     9-Jul:     Collapse 2.3.* into 2.3.6, 2.5.* into 2.5.6.~%")
(hey " *     13-Jun:    folded xg-ruby.c into xg.c.~%")
(hey " *     21-Feb:    changed libxm to libxg, xm-version to xg-version.~%")
(hey " *     10-Jan:    plugged some memory leaks.~%")
(hey " *     4-Jan-05:  removed deprecated Xen_VECTOR_ELEMENTS.~%")
(hey " *     --------~%")
(hey " *     8-Dec:     added some g_log handler funcs.~%")
(hey " *     6-Dec:     check for lost callback context.~%")
(hey " *                tightened type (pointer) checking considerably (#f only acceptable if explicit @ used in xgdata.scm).~%")
(hey " *     3-Dec:     changed GPOINTER cast func to accept non-lists.~%")
(hey " *     27-Aug:    removed the PANGO_ENGINE and PANGO_BACKEND stuff.~%")
(hey " *     2-Jun:     gdk_atom_name needs to free return value~%")
(hey " *     28-May:    GtkFileSelection struct support put back in -- need ok_button et al.~%")
(hey " *     14-Apr:    make-target-entry.~%")
(hey " *     4-Apr:     various additions, deletions, and bugfixes for snd-test 26~%")
(hey " *     29-Mar:    support for some ... args.~%")
(hey " *     22-Mar:    g_source_remove and related changes.~%")
(hey " *     12-Feb-04: g_list_nth_data (Kjetil S. Matheussen).~%")
(hey " *     --------~%")
(hey " *     15-Sep:    removed client_window GtkIMMulticontext struct field.~%")
(hey " *     26-May:    removed nugatory GdkInputFunction stuff and some unused type converters.~%")
(hey " *     1-Apr:     gdk_property_get uses scm_mem2string in some cases now.~%")
(hey " *     31-Mar:    gchar* -> xen string bugfix (thanks to Friedrich Delgado Friedrichs).~%")
(hey " *     10-Mar-03: Ruby Xm_Version.~%")
(hey " *     --------~%")
(hey " *     18-Nov:    Ruby/Gtk bugfixes.~%")
(hey " *     25-Oct:    removed (deprecated) gdk_set_pointer_hooks~%")
(hey " *     31-Jul:    removed GTK 1.n support~%")
(hey " *     24-Jul:    changed Guile prefix (R5RS reserves vertical-bar).~%")
(hey " *     19-Jul:    XG_FIELD_PRE for change from using vertical-bar (reserved in R5RS)~%")
(hey " *     2-Jun:     removed deprecated and broken stuff~%")
(hey " *     12-Mar:    support for GtkDestroyNotify callbacks~%")
(hey " *                Ruby support via xg-ruby.c~%")
(hey " *     21-Feb:    #f=NULL throughout, gdk-pixbuf, GTypes.~%")
(hey " *     11-Feb-02: initial version.~%")
(hey " */~%~%")

(hey "#include <mus-config.h>~%~%")

(hey "#define HAVE_GTK_3 (GTK_MAJOR_VERSION == 3)~%")
(hey "#define HAVE_CAIRO_1_8    ((CAIRO_VERSION_MAJOR >= 1) && (CAIRO_VERSION_MINOR >= 8))~%")
(hey "#define HAVE_CAIRO_1_9_12 ((CAIRO_VERSION_MAJOR >= 1) && (CAIRO_VERSION_MINOR >= 9) && (CAIRO_VERSION_MICRO >= 12))~%~%")

(hey "#if ((!__NetBSD__) && ((_MSC_VER) || (!defined(__STC__)) || (defined(__STDC_VERSION__) && (__STDC_VERSION__ < 199901L))))~%")
(hey "  #define __func__ __FUNCTION__~%")
(hey "#endif~%~%")

(hey "#if HAVE_EXTENSION_LANGUAGE~%~%")

;(hey "#if UNDEF_USE_SND~%  #undef USE_SND~%  #define USE_SND 0~%#endif~%~%")

(hey "#include <string.h>~%")
(hey "#include <stdlib.h>~%~%")

(hey "#include <glib.h>~%")
(hey "#include <gdk/gdk.h>~%")
(hey "#if (!HAVE_GTK_3)~%")
(hey "  #include <gdk/gdkkeysyms.h>~%")
(hey "#endif~%")
(hey "#include <gtk/gtk.h>~%")
(hey "#include <glib-object.h>~%")
(hey "#include <pango/pango.h>~%")
(with-cairo hey (lambda () (hey "#include <cairo/cairo.h>~%")))

(hey "#if USE_SND~%")
(hey "  /* USE_SND causes xm to use Snd's error handlers which are much smarter than xen's fallback versions */~%")
(hey "  #include \"snd.h\"~%")
(hey "#else~%")
(hey "  #include \"xen.h\"~%")
(hey "  #define NOT_A_GC_LOC -1~%")
(hey "#endif~%")

(hey "~%#ifndef PROC_FALSE~%")
(hey "  #if HAVE_RUBY~%")
(hey "    #define PROC_FALSE \"false\"~%")
(hey "    #define PROC_TRUE \"true\"~%")
(hey "  #else~%")
(hey "    #define PROC_FALSE \"#f\"~%")
(hey "    #define PROC_TRUE  \"#t\"~%")
(hey "  #endif~%")
(hey "#endif~%~%")

(hey "/* -------------------------------- GC -------------------------------- */~%")
(hey "static Xen_object_type_t xm_obj_tag;~%")
(hey "#if HAVE_RUBY~%")
(hey "static void *xm_obj_free(Xen obj)~%")
(hey "{~%")
(hey "  void *xobj;~%")
(hey "  xobj = (void *)obj;~%")
(hey "  free(xobj);~%")
(hey "  return(NULL);~%")
(hey "}~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "static void xm_obj_free(Xen obj)~%")
(hey "{~%")
(hey "  void *val;~%")
(hey "  val = (void *)Xen_object_ref(obj);~%")
(hey "  free(val);~%")
(hey "}~%")
(hey "#endif~%")
(hey "#if HAVE_SCHEME~%")
(hey "static void xm_obj_free(void *val)~%")
(hey "{~%")
(hey "  free(val);~%")
(hey "}~%")
(hey "static bool s7_equalp_xm(void *x1, void *x2)~%")
(hey "{~%")
(hey "  return(x1 == x2);~%")
(hey "}~%")
(hey "#endif~%")
(hey "static Xen make_xm_obj(void *ptr)~%")
(hey "{~%")
(hey "  return(Xen_make_object(xm_obj_tag, ptr, 0, xm_obj_free));~%")
(hey "}~%")
(hey "static void define_xm_obj(void)~%")
(hey "{~%")
(hey "#if HAVE_SCHEME~%")
(hey " xm_obj_tag = s7_new_type_x(\"<XmObj>\", NULL, xm_obj_free, s7_equalp_xm, NULL, NULL, NULL, NULL, NULL, NULL, NULL);~%")
(hey "#else~%")
(hey "  xm_obj_tag = Xen_make_object_type(\"XmObj\", sizeof(void *));~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "  fth_set_object_free(xm_obj_tag, xm_obj_free);~%")
(hey "#endif~%")
(hey "}  ~%")
(hey "~%")

(hey "/* prefix for all names */~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define Xg_pre \"\"~%")
(hey "  #define Xg_field_pre \".\"~%")
(hey "  #define Xg_post \"\"~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY~%")
(hey "/* for Ruby, XG PRE needs to be uppercase */~%")
(hey "  #define Xg_pre \"R\"~%")
(hey "  #define Xg_post \"\"~%")
(hey "  #define Xg_field_pre \"R\"~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "  #define Xg_pre \"F\"~%")
(hey "  #define Xg_post \"\"~%")
(hey "  #define Xg_field_pre \"F\"~%")
(hey "#endif~%")
(hey "~%")

(hey "static Xen xg_~A_symbol" (no-stars (car all-types)))
(for-each
 (lambda (typ)
   (hey ", xg_~A_symbol" (no-stars typ)))
 (cdr all-types))

(define other-types 
  (list 'idler 'GtkCellRendererPixbuf_ 'GtkCheckButton_ 'GtkDrawingArea_ 'GtkScrollbar_ 'GtkSeparator_ 'GtkSeparatorMenuItem_
	'GdkEventExpose_ 'GdkEventNoExpose_ 'GdkEventVisibility_ 'GdkEventButton_ 'GdkEventScroll_ 'GdkEventCrossing_
	'GdkEventFocus_ 'GdkEventConfigure_ 'GdkEventProperty_ 'GdkEventSelection_ 'GdkEventProximity_ 'GdkEventSetting_
	'GdkEventWindowState_ 'GdkEventDND_ 'GtkFileChooserDialog_ 'GtkFileChooserWidget_ 'GtkColorButton_ 'GtkAccelMap
	'GtkCellRendererCombo_ 'GtkCellRendererProgress_ 'GtkCellRendererAccel_ 'GtkCellRendererSpin_ 'GtkRecentChooserDialog_
	'GtkRecentChooserWidget_ 'GtkCellRendererSpinner_ 'gboolean_
	'GtkFontChooserDialog_ 'GtkFontChooserWidget_ 'GtkColorChooserDialog_ 'GtkColorChooserWidget_ 'GtkColorWidget_

	))

(for-each
 (lambda (typ)
   (hey ", xg_~A_symbol" typ))
 other-types)
 
(hey ";~%~%")


(hey "#define wrap_for_Xen(Name, Value) Xen_list_2(xg_ ## Name ## _symbol, Xen_wrap_C_pointer(Value))~%")
(hey "#define is_wrapped(Name, Value) (Xen_is_pair(Value) && (Xen_car(Value) == xg_ ## Name ## _symbol))~%")
(hey "~%")
(hey "#define Xm_type(Name, XType) \\~%")
;; these are not pointers, so should not use wrap_c_pointer and friends 
(hey "  static Xen C_to_Xen_ ## Name (XType val) {return(Xen_list_2(xg_ ## Name ## _symbol, C_ulong_to_Xen_ulong(val)));} \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_1(Name, XType) \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_ulong_to_C_ulong(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
					;(hey "#define Xm_type_no_P(Name, XType) \\~%")
					;(hey "  static Xen C_to_Xen_ ## Name (XType val) {return(wrap_for_Xen(Name, val));} \\~%")
					;(hey "  static XType Xen_to_C_ ## Name (Xen val) {return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
					;(hey "~%")
(hey "#define Xm_type_no_p_2(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {return(wrap_for_Xen(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {if (val) return(wrap_for_Xen(Name, val)); return(Xen_false);} \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr_const(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (const XType val) {if (val) return(wrap_for_Xen(Name, val)); return(Xen_false);} \\~%")
(hey "  static const XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((const XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr_1(Name, XType) \\~%")
(hey "  static XType Xen_to_C_ ## Name (Xen val) {if (Xen_is_false(val)) return(NULL); return((XType)Xen_unwrap_C_pointer(Xen_cadr(val)));} \\~%")
(hey "  static bool Xen_is_ ## Name (Xen val) {return(is_wrapped(Name, val));}~%")
(hey "~%")
(hey "#define Xm_type_Ptr_2(Name, XType) \\~%")
(hey "  static Xen C_to_Xen_ ## Name (XType val) {if (val) return(wrap_for_Xen(Name, val)); return(Xen_false);} \\~%")
(hey "~%")
(hey "/* type checks for callback wrappers */~%")

(define (callback-p func)
  (hey "#define Xen_is_~A(Arg)  Xen_is_false(Arg) || (Xen_is_procedure(Arg) && (Xen_is_aritable(Arg, ~D)))~%"
       (symbol->string (callback-name func))
       (length (callback-args func))))

(for-each callback-p callbacks)

					;(hey "#define Xen_is_lambda(Arg) Xen_is_procedure(Arg)~%")
(hey "#define Xen_is_GCallback(Arg) (Xen_is_procedure(Arg) && ((Xen_is_aritable(Arg, 2)) || (Xen_is_aritable(Arg, 3)) || (Xen_is_aritable(Arg, 4))))~%")

(define (xen-callback func)
  (hey "#define Xen_to_C_~A(Arg) Xen_is_false(Arg) ? NULL : gxg_~A~%"
       (symbol->string (callback-name func))
       (callback-func func)))

(for-each xen-callback callbacks)

(hey "#define Xen_to_C_GCallback(Arg) ((Xen_is_aritable(Arg, 4)) ? (GCallback)gxg_func4 : ((Xen_is_aritable(Arg, 3)) ? (GCallback)gxg_func3 : (GCallback)gxg_func2))~%")
;; (hey "#define Xen_to_C_GCallback(Arg) ((Xen_is_aritable(Arg, 3)) ? (GCallback)gxg_func3 : (GCallback)gxg_func2)~%")

(hey "#define Xen_to_C_lambda_data(Arg) (gpointer)gxg_ptr~%")
(hey "#define Xen_is_lambda_data(Arg) 1~%")

;; needed if func returns func of this type
(hey "#define C_to_Xen_GtkTreeViewSearchPositionFunc(Arg) wrap_for_Xen(GtkTreeViewSearchPositionFunc, Arg)~%")
(hey "#define C_to_Xen_GtkTreeViewSearchEqualFunc(Arg) wrap_for_Xen(GtkTreeViewSearchEqualFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkLinkButtonUriFunc(Arg) wrap_for_Xen(GtkLinkButtonUriFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkTreeIterCompareFunc(Arg) wrap_for_Xen(GtkTreeViewSearchEqualFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkTreeSelectionFunc(Arg) wrap_for_Xen(GtkTreeSelectionFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkMenuPositionFunc(Arg) wrap_for_Xen(GtkMenuPositionFunc, Arg)~%")
					;(hey "#define C_to_Xen_GtkDestroyNotify(Arg) wrap_for_Xen(GtkDestroyNotify, Arg)~%")
(hey "#define Xen_to_C_GdkFilterReturn(Arg) (GdkFilterReturn)Xen_integer_to_C_int(Arg)~%")

;(hey "#define Xen_to_C_String(Arg) Xen_string_to_C_string(Arg)~%")
(hey "#define C_to_Xen_String(Arg) C_string_to_Xen_string((char *)Arg)~%")

(hey "static Xen C_to_Xen_GError_(GError *err)~%")
(hey "{~%")
(hey "  if (err)~%")
(hey "    {~%")
(hey "      Xen msg;~%")
(hey "      msg = C_string_to_Xen_string(err->message);~%")
(hey "      g_error_free(err);~%")
(hey "      return(msg);~%")
(hey "    }~%")
(hey "  return(Xen_false);~%")
(hey "}~%")


(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(for-each type-it (reverse types))
(for-each
 (lambda (type-list with-func)
   (if (not (null? type-list)) 
       (with-func hey (lambda () 
			(for-each type-it (reverse type-list))))))
 all-ntypes all-ntype-withs)


(hey "#define XLS(a, b) Xen_to_C_gchar_(Xen_list_ref(a, b))~%")
(hey "#define XLI(a, b) ((int)Xen_integer_to_C_int(Xen_list_ref(a, b)))~%")
(hey "#define XLL(a, b) (Xen_llong_to_C_llong(Xen_list_ref(a, b)))~%")
(hey "#define XLG(a, b) Xen_to_C_GType(Xen_list_ref(a, b))~%")
(hey "#define XLT(a, b) Xen_to_C_GtkTextTag_(Xen_list_ref(a, b))~%")
(hey "#define XLA(a, b) ((Xen_is_integer(Xen_list_ref(a, b))) ? ((gpointer)XLL(a, b)) : ((Xen_is_string(Xen_list_ref(a, b))) ? ((gpointer)XLS(a, b)) : ((gpointer)XLG(a, b))))~%~%")

(hey "static Xen c_to_xen_string(Xen str)~%")
(hey "{~%")
(hey "  return(C_string_to_Xen_string((char *)Xen_unwrap_C_pointer(str)));~%")
(hey "}~%~%")


(hey "/* -------------------------------- gc protection -------------------------------- */~%")
(hey "~%")
(hey "static Xen xm_protected;~%")
(hey "static int xm_protected_size = 0;~%")
(hey "static Xen xm_gc_table;~%")
(hey "static int last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "~%")
(hey "static int xm_protect(Xen obj)~%")
(hey "{~%")
(hey "  int i, new_size;~%")
(hey "  Xen new_table;~%")
(hey "  if (last_xm_unprotect >= 0)~%")
(hey "    {~%")
(hey "      i = last_xm_unprotect;~%")
(hey "      if (Xen_is_false(Xen_vector_ref(xm_protected, i)))~%")
(hey "	{~%")
(hey "	  Xen_vector_set(xm_protected, i, obj);~%")
(hey "	  last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "	  return(i);~%")
(hey "	}~%")
(hey "      last_xm_unprotect = NOT_A_GC_LOC;~%")
(hey "    }~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    if (Xen_is_false(Xen_vector_ref(xm_protected, i)))~%")
(hey "      {~%")
(hey "	Xen_vector_set(xm_protected, i, obj);~%")
(hey "	return(i);~%")
(hey "      }~%")
(hey "  new_size = xm_protected_size * 2;~%")
(hey "  new_table = Xen_make_vector(new_size, Xen_false);~%")
(hey "  for (i = 0; i < xm_protected_size; i++)~%")
(hey "    {~%")
(hey "      Xen_vector_set(new_table, i, Xen_vector_ref(xm_protected, i));~%")
(hey "      Xen_vector_set(xm_protected, i, Xen_false);~%")
(hey "    }~%")
(hey "  Xen_vector_set(new_table, xm_protected_size, obj);~%")
(hey "  Xen_vector_set(xm_gc_table, 0, new_table);~%")
(hey "  i = xm_protected_size;~%")
(hey "  xm_protected_size = new_size;~%")
(hey "  xm_protected = new_table;~%")
(hey "  return(i);~%")
(hey "}~%")
(hey "~%")
(hey "static void xm_unprotect_at(int ind)~%")
(hey "{~%")
(hey "  Xen_vector_set(xm_protected, ind, Xen_false);~%")
(hey "  last_xm_unprotect = ind;~%")
(hey "}~%~%")

(hey "~%~%/* ---------------------------------------- callback handlers ---------------------------------------- */~%~%")

(let ((funcs-done ()))
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
					    (eq? fname 'GtkEntryCompletionMatchFunc)
					    ;(eq? fname 'GtkLinkButtonUriFunc)
					    )
					(= ctr 1))
				   (and (or (eq? fname 'GtkTreeViewSearchEqualFunc)
					    (eq? fname 'GLogFunc)
					    (eq? fname 'GtkClipboardRichTextReceivedFunc))
					(= ctr 2))
				   (and (or (eq? fname 'GtkFileFilterFunc)
					    (eq? fname 'GtkRecentFilterFunc)
					    (eq? fname 'GLogFunc))
					(= ctr 0)))
			       (hey "const "))
			   (set! ctr (+ ctr 1))
			   (set! previous-arg #t)
			   (hey "~A ~A" 
				(if (not (string=? (car arg) "lambda_data"))
				    (car arg)
				    "gpointer")
				(cadr arg)))
			 args)
			(hey ")~%"))
		      (hey "{~%  ")
		      ;; I tried to use Xen_error here but it was a no-op for some reason?? 
		      (hey "if (!Xen_is_list((Xen)func_info)) return~A;~%  "
			   (if void? 
			       ""
			       (format #f "((~A)0)" (no-stars type))))
		      (if (eq? gctype 'permanent-gcc)
			  (hey "#if (!(defined(__cplusplus)))~%  ")) ; const arg conversion causes trouble if g++
		      (let ((castlen (+ 12 (if (not void?) 
					       (+ 2 (string-length (format #f "return(Xen_to_C_~A" (no-stars type))))
					       1))))
			(if (not void?)
			    (hey "return(Xen_to_C_~A("
				 (no-stars type)))
			(hey "Xen_call_with_~A_arg~A(~A((Xen)func_info),~%"
			     (if (zero? (length args)) "no" (length args))
			     (if (= (length args) 1) "" "s")
			     (if (eq? fname 'GtkClipboardClearFunc)
				 "Xen_caddr"
				 (if (eq? fname 'GtkDestroyNotify)
				     "Xen_cadddr"
				     "Xen_car")))
			(for-each
			 (lambda (arg)
			   (hey (substring "                                                                   " 0 castlen))
			   (if (not (string=? (car arg) "lambda_data"))
			       (hey "C_to_Xen_~A(~A~A),~%"
				    (no-stars (car arg))
				    (if (string=? (car arg) "GtkFileFilterInfo*")
					"(GtkFileFilterInfo *)"
					"")
				    (cadr arg))
			       (hey "Xen_cadr((Xen)func_info),~%")))
			 args)
			(hey (substring "                                                                      " 0 castlen))
			(hey "__func__)")
			(if void?
			    (hey ";~%")
			    (hey "));~%")))
		      (if (eq? gctype 'permanent-gcc)
			  (begin
			    (if (not void?)
				(begin
				  (hey "  #else~%")
				  (hey "  return((~A)0);~%" (no-stars type))))
			    (hey "  #endif~%")))
		      (hey "}~%~%")))))))
    (for-each xc callbacks)
    ))

(hey "~%static gboolean gxg_func3(GtkWidget *w, GdkEventAny *ev, gpointer data)~%")
(hey "{~%")
(hey "  return(Xen_boolean_to_C_bool(Xen_call_with_3_args(Xen_car((Xen)data),~%")
(hey "                                     C_to_Xen_GtkWidget_(w),~%")
(hey "                                     C_to_Xen_GdkEventAny_(ev),~%")
(hey "                                     Xen_cadr((Xen)data),~%")
(hey "                                     __func__)));~%")
(hey "}~%")
(hey "~%static gboolean gxg_func4(GtkPrintOperation *op, GtkPrintContext *context, gint page_nr, gpointer data)~%")
(hey "{~%")
(hey "  return(Xen_boolean_to_C_bool(Xen_call_with_4_args(Xen_car((Xen)data),~%")
(hey "                                     C_to_Xen_GtkPrintOperation_(op),~%")
(hey "                                     C_to_Xen_GtkPrintContext_(context),~%")
(hey "                                     C_int_to_Xen_integer(page_nr),~%")
(hey "                                     Xen_cadr((Xen)data),~%")
(hey "                                     __func__)));~%")
(hey "}~%~%")


(hey "~%~%/* ---------------------------------------- functions ---------------------------------------- */~%~%")

(define max-args 8)

(define handle-func
  (lambda (data)
    (let* ((name (car data))
	   (return-type (cadr data))
	   (args (caddr data))
	   (cargs (length args))
	   (refargs (ref-args args))
	   (xgargs (- cargs refargs))
	   (argstr (cadddr data))
	   (lambda-type (hash-table-ref names name))
	   (callback-data (and (not (eq? lambda-type 'fnc))
			       (find-callback 
				(lambda (func)
				  (and (eq? (callback-name func) lambda-type)
				       func)))))
	   (spec (and (> (length data) 4) (data 4)))
	   (spec-data (and (> (length data) 5) (data 5)))
	   (arg-start 0)
	   (line-len 0)
	   (line-max 120))
      
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
	      (do ((i 0 (+ i 1)))
		  ((= i arg-start))
		(heyc " "))
	      (set! line-len arg-start))))
      
      (hey "static Xen gxg_~A(" name)
      (if (= (length args) 0)
	  (heyc "void")
	  (if (>= (length args) max-args)
	      (begin
		(heyc "Xen arglist"))
	      (let ((previous-arg #f))
		(for-each 
		 (lambda (arg)
		   (let ((argname (cadr arg))
			 ;(argtype (car arg))
			 )
		     (if previous-arg (heyc ", "))
		     (set! previous-arg #t)
		     (if (and (ref-arg? arg)
			      (not (member name (list "gdk_init" "gdk_init_check" "gtk_init" "gtk_init_check" "gtk_parse_args"))))
			 (hey "Xen ignore_~A" argname)
			 (hey "Xen ~A" argname))))
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
	    (heyc "  Xen ")
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
		     (hey "  ~A = Xen_list_ref(arglist, ~D);~%" (cadr arg) ctr))
		 (set! ctr (+ ctr 1)))
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
			 (hey "  Xen_check_type(Xen_is_~A(~A) || Xen_is_false(~A), ~A, ~D, ~S, ~S);~%" 
			      (no-stars argtype) argname argname argname ctr name argtype)
			 (if (opt-arg? arg)
			     (begin
			       (hey "  if (!Xen_is_bound(~A)) ~A = Xen_false; ~%" argname argname)
			       (hey "  else Xen_check_type(Xen_is_~A(~A), ~A, ~D, ~S, ~S);~%" 
				    (no-stars argtype) argname argname ctr name argtype))
			     (hey "  Xen_check_type(Xen_is_~A(~A), ~A, ~D, ~S, ~S);~%"
				  (no-stars argtype) argname argname ctr name argtype)))
		     (if (>= (length arg) 3)
			 (if (char=? ((arg 2) 0) #\{)
			     (begin
			       (set! argc (deref-name arg))
			       (hey "  ~A = Xen_to_C_~A(~A);~%" (deref-name arg) (deref-type arg) argname))
			     (if (char=? ((arg 2) 0) #\|)
				 (begin
				   (hey "  ~A = (~A)calloc(~A, sizeof(~A));~%" 
					(deref-name arg)
					(deref-type arg)
					argc
					(deref-element-type arg))
				   (hey "  {~%   int i;~%   Xen lst;~%   lst = Xen_copy_arg(~A);~%" argname)
				   (hey "   for (i = 0; i < ~A; i++, lst = Xen_cdr(lst)) ~A[i] = Xen_to_C_~A(Xen_car(lst));~%"
					argc
					(deref-name arg)
					(no-stars (deref-element-type arg)))
				   (hey "  }~%"))))))
		 (set! ctr (+ ctr 1))))
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
	      (if using-result (hey "    Xen result = Xen_false;~%"))
	      (if using-loc (hey "    int loc;~%"))
	      (hey "    Xen gxg_ptr = Xen_list_5(~A, func_info, Xen_false, Xen_false, Xen_false);~%"
		   (call-with-exit
		    (lambda (name-it)
		      (for-each
		       (lambda (arg)
			 (let ((argname (cadr arg))
			       ;(argtype (car arg))
			       )
			   (if (string=? argname "func")
			       (name-it "func"))))
		       args)
		      "Xen_false")))
	      (if using-loc
		  (hey "    loc = xm_protect(gxg_ptr);~%")
		  (hey "    xm_protect(gxg_ptr);~%"))
	      (if using-loc
		  (hey "    Xen_list_set(gxg_ptr, 2, C_int_to_Xen_integer(loc));~%")
		  (if (eq? lambda-type 'GtkClipboardGetFunc)
		      (hey "    Xen_list_set(gxg_ptr, 2, clear_func);~%")))
	      (for-each
	       (lambda (arg)
		 (let ((argname (cadr arg))
		       (argtype (car arg)))
		   (if (string=? argtype "GtkDestroyNotify")
		       (hey "    Xen_list_set(gxg_ptr, 3, ~A);~%" argname))))
	       args)
	      (hey-start)
	      (if using-result
		  (hey-on "    result = C_to_Xen_~A(" (no-stars return-type))
		  (heyc "    ")))
	    (begin ; lambda-type = 'fnc
	      (set! using-result (and (> refargs 0)
				      (not (string=? return-type "void"))))
	      (if using-result
		  (begin
		    (hey "  {~%")
		    (hey "    Xen result = Xen_false;~%")))
	      (hey-start)

	      (if (not (eq? spec 'etc))
		  (if (not (string=? return-type "void"))
		      (if (= refargs 0)
			  (if (eq? spec 'free)
			      (hey-on "  {~%   ~A result;~%   Xen rtn;~%   result = " return-type)
			      (if (eq? spec 'const-return)
				  (hey "    return(C_to_Xen_~A((~A)" (no-stars return-type) return-type)
				  (begin
				    (if (member name idlers)
					(begin
					  (hey "  xm_unprotect_at(Xen_integer_to_C_int(Xen_caddr(~A)));~%" (cadr (car args)))
					  (set! idlers (remove-if (lambda (x) (string=? x name)) idlers))))
				  (hey-on "  return(C_to_Xen_~A(" (no-stars return-type)))))
			  (hey-on "    result = C_to_Xen_~A(" (no-stars return-type)))
		      (hey-on "  ")))))

	;; pass args
	(if (eq? spec 'etc)
	    (begin
	      ;; goes to end
	      ;; need to check ... list, set up locals, send out switch, return result
	      (let* ((list-name (cadr (args (- cargs 1))))
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
		(do ((i 0 (+ i 1)))
		    ((= i (- cargs 1)))
		  (let ((arg (args i)))
		    (hey "    ~A p_arg~D;~%" (car arg) i)))
		(hey "    if (Xen_is_list(~A)) etc_len = Xen_list_length(~A);~%" list-name list-name)
		(if (> min-len 0)
		    (hey "    if (etc_len < ~D) Xen_out_of_range_error(~S, ~A, ~A, \"... list must have at least ~D entr~A\");~%"
			 min-len name (- cargs 1) list-name min-len (if (= min-len 1) "y" "ies")))
		(hey "    if (etc_len > ~D) Xen_out_of_range_error(~S, ~A, ~A, \"... list too long (max len: ~D)\");~%"
		     max-len name (- cargs 1) list-name max-len)
		(if (not (= modlen 1))
		    (hey "    if ((etc_len % ~D) != 0) Xen_out_of_range_error(~S, ~A, ~A, \"... list len must be multiple of ~D\");~%"
			 modlen name (- cargs 1) list-name modlen))
		(do ((i 0 (+ i 1)))
		    ((= i (- cargs 1)))
		  (let ((arg (args i)))
		    (hey "    p_arg~D = Xen_to_C_~A(~A);~%" i (no-stars (car arg)) (cadr arg))))
		(hey "    switch (etc_len)~%")
		(hey "      {~%")
		(do ((i min-len (+ i modlen)))
		    ((> i max-len))
		  (if (not (string=? return-type "void"))
		      (hey "        case ~D: result = ~A(" i name)
		      (hey "        case ~D: ~A(" i name))
		  (do ((j 0 (+ 1 j)))
		      ((= j (- cargs 1)))
		    (let () ;(arg (args j)))
		      (hey "p_arg~D, " j)))
		  ;; assume ending null for now
		  (let ((modctr 0))
		    (do ((j 0 (+ 1 j)))
			((= j i))
		      (let ((type (types modctr)))
			(set! modctr (+ 1 modctr))
			(if (>= modctr modlen) (set! modctr 0))
			(if (string=? type "int")
			    (hey "XLI(")
			    (if (string=? type "gchar*")
				(hey "XLS(")
				(if (string=? type "GtkTextTag*")
				    (hey "XLT(")
				    (if (string=? type "GType")
					(hey "XLG(")
					(hey "XLA("))))))
		      (hey "~A, ~D)" list-name j)
		      (if (or with-null with-minus-one (< j (- i 1)))
			  (hey ", "))))
		  (if with-null
		      (if (and (= i 0)
			       (string=? name "gtk_file_chooser_dialog_new"))
			  (hey "NULL, NULL); break;~%") ; extra NULL needed I guess for the valist pass-through -- gcc 4.1 grumbles about it
			  (hey "NULL); break;~%"))
		      (if with-minus-one
			  (hey "-1); break;~%")
			  (hey "); break;~%"))))
		(hey "      }~%")

		(if (not (string=? return-type "void"))
		    (hey "    return(C_to_Xen_~A(result));~%" (no-stars return-type))
		    (hey "    return(Xen_false);~%"))
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
					    (string=? argtype "char*")
					    (string=? argtype "GValue*")))
				   (hey "(const ~A)" argtype))
			       (set! previous-arg #t)
			       (if (ref-arg? arg)
				   (hey-on "&~A" (deref-name arg))
				   (hey-on "Xen_to_C_~A(~A)" (no-stars argtype) argname))))
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
			      (hey "    Xen_list_set(gxg_ptr, 2, Xen_list_3(xg_idler_symbol, ~A, C_int_to_Xen_integer(loc)));~%"
				   (if (string=? return-type "void") "Xen_false" "result")))
			  (if using-result
			      (hey "    return(result);~%")
			      (hey "    return(Xen_false);~%"))
			  (hey "   }~%"))
			(begin ;'fnc
			  (if (> refargs 0)
			      (let ((previous-arg using-result))
				(if using-result (heyc "  "))
				(if (string=? name "gdk_property_get")
				    (begin
				      ;; special case -- type returned is dependent to some extent on atom
				      (hey "  {~%      Xen data_val = Xen_false;~%\
      if (ref_actual_property_type == GDK_TARGET_STRING)~%\
	data_val = C_string_to_Xen_string((char *)ref_data);~%\
      else if (ref_actual_length > 0) data_val = C_string_to_Xen_string_with_length((char *)ref_data, ref_actual_length * ref_actual_format / 8);~%\
     return(Xen_list_5(result, C_to_Xen_GdkAtom(ref_actual_property_type), C_to_Xen_gint(ref_actual_format), ~%\
                       C_to_Xen_gint(ref_actual_length), data_val));~%\
    }~%  }~%")
				      )
				    (begin
				      (hey "  return(Xen_list_~D(" (+ refargs (if using-result 1 0)))
				      (if using-result (heyc "result"))
				      (for-each 
				       (lambda (arg)
					 (if (ref-arg? arg)
					     (begin
					       (if previous-arg (heyc ", "))
					       (hey "C_to_Xen_~A(~A)" (no-stars (deref-type arg)) (deref-name arg))
					       (set! previous-arg #t))))
				       args)
				      (hey "));~%")
				      (if using-result (hey "   }~%")))))
			      ;; refargs = 0
			      (begin
				(if (member name idlers)
				    (hey "  xm_unprotect_at(Xen_integer_to_C_int(Xen_caddr(~A)));~%" (cadr (car args))))
				(if (string=? return-type "void")
				    (hey "  return(Xen_false);~%")))))))
		  (begin ; 'lambda (see line 1846)
		    (hey "if (Xen_is_aritable(func, 2))~%")
		    (hey-start)
		    (if (not (string=? return-type "void"))
			(hey-on "       return(C_to_Xen_~A(~A(" (no-stars return-type) name)
			(hey-on "       ~A(" name))
		    (hey-mark)
		    (let ((previous-arg #f))
		      (for-each
		       (lambda (arg)
			 (let ((argname (cadr arg))
			       (argtype (car arg)))
			   (if previous-arg (hey-ok ", "))
			   (set! previous-arg #t)
			   (hey-on "Xen_to_C_~A(~A)" (no-stars argtype) argname)))
		       args))
		    (if (not (string=? return-type "void"))
			(hey ")));~%")
			(hey ");~%"))
		    (hey "     else~%")
		    (hey-start)
		    (if (not (string=? return-type "void"))
			(hey-on "       return(C_to_Xen_~A(~A(" (no-stars return-type) name)
			(hey-on "       ~A(" name))
		    (hey-mark)
		    (let ((previous-arg #f))
		      (for-each
		       (lambda (arg)
			 (let ((argname (cadr arg))
			       (argtype (car arg)))
			   (if previous-arg (hey-ok ", "))
			   (set! previous-arg #t)
			   (hey-on "Xen_to_C_~A(~A)" (no-stars argtype) argname)))
		       args))
		    (if (string=? return-type "void")
			(begin
			  (hey ");~%")
			  (hey "    return(Xen_false);~%"))
			(hey ")));~%"))
		    (hey "  }~%")) ;'lambda
		  ))) ; 'begin
	(if (eq? spec 'free)
	    (hey "   rtn = C_to_Xen_~A(result);~%   g_free(result);~%   return(rtn);~%  }~%" (no-stars return-type)))
	(hey "}~%~%")
	))))


(for-each handle-func (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (not (null? func-list)) 
       (with-func hey (lambda () 
			(for-each handle-func (reverse func-list))))))
 all-funcs all-func-withs)


(hey "#define Xen_is_wrapped_object(Obj) (Xen_is_list(Obj) && (Xen_list_length(Obj) >= 2) && (Xen_is_symbol(Xen_car(Obj))))~%~%")

(define cast-it
  (lambda (cast)
    (let ((cast-name (car cast))
	  (cast-type (cadr cast)))
      (hey "static Xen gxg_~A(Xen obj)" (no-arg cast-name))
      (hey " {return((Xen_is_wrapped_object(obj)) ? Xen_list_2(xg_~A_symbol, Xen_cadr(obj)) : Xen_false);}~%" (no-stars cast-type)))))

(hey "static Xen gxg_GPOINTER(Xen obj)")
(hey " {return(Xen_list_2(xg_gpointer_symbol, (Xen_is_wrapped_object(obj)) ? Xen_cadr(obj) : Xen_wrap_C_pointer(obj)));}~%")

(for-each cast-it (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (not (null? cast-list)) 
       (cast-func hey (lambda () 
			(for-each cast-it (reverse cast-list))))))
 all-casts all-cast-withs)

;;; checks have to use the built-in macros, not local symbol-based type checks

(define (make-check func)
  (hey "static Xen gxg_~A(Xen obj)" (no-arg (car func)))
  (hey " {return(C_bool_to_Xen_boolean(Xen_is_wrapped_object(obj) && ~A((GTypeInstance *)Xen_unwrap_C_pointer(Xen_cadr(obj)))));}~%" (no-arg (car func))))

(for-each make-check (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (not (null? check-list)) 
       (check-func hey (lambda () 
			 (for-each make-check (reverse check-list))))))
 all-checks all-check-withs)


(hey "~%~%/* ---------------------------------------- special functions ---------------------------------------- */~%~%")

;;; from Mike Scholz -- improve the error checking
(hey "static Xen gxg_gtk_init(Xen argc, Xen argv) ~%")
(hey "{ ~%")
(hey "  #define H_gtk_init \"void gtk_init(int* argc, char*** argv)\" ~%")
(hey "  int ref_argc = 0; ~%")
(hey "  char** ref_argv = NULL; ~%")
(hey "  if (Xen_is_bound(argv)) ~%")
(hey "    { ~%")
(hey "      if (Xen_is_bound(argc) && Xen_is_integer(argc) && Xen_to_C_int(argc) <= Xen_list_length(argv)) ~%")
(hey "	ref_argc = Xen_to_C_int(argc); ~%")
(hey "      else ref_argc = Xen_list_length(argv); ~%")
(hey "    } ~%")
(hey "  ref_argv = (char**)calloc(ref_argc, sizeof(char*)); ~%")
(hey "  { ~%")
(hey "    int i; ~%")
(hey "    Xen lst; ~%")
(hey "    lst = Xen_copy_arg(argv); ~%")
(hey "    for (i = 0; i < ref_argc; i++, lst = Xen_cdr(lst)) ref_argv[i] = Xen_to_C_char_(Xen_car(lst));~%")
(hey "  }~%")
(hey "  gtk_init(&ref_argc, &ref_argv);~%")
(hey "  return(Xen_list_2(C_to_Xen_int(ref_argc), C_to_Xen_char__(ref_argv)));~%")
(hey "} ~%")
(hey "~%")
(hey "static Xen gxg_gtk_init_check(Xen argc, Xen argv) ~%")
(hey "{ ~%")
(hey "  #define H_gtk_init_check \"gboolean gtk_init_check(int* argc, char*** argv)\" ~%")
(hey "  int ref_argc = 0; ~%")
(hey "  char** ref_argv = NULL; ~%")
(hey "  if (Xen_is_bound(argc) && Xen_is_list(argc)) ~%")
(hey "    { ~%")
(hey "      argv = argc; ~%")
(hey "      ref_argc = Xen_list_length(argv); ~%")
(hey "    } ~%")
(hey "  else ~%")
(hey "    {~%")
(hey "      if (Xen_is_bound(argv)) ~%")
(hey "	{ ~%")
(hey "	  int len; ~%")
(hey "	  Xen_check_type(Xen_is_integer(argc), argc, 1, \"gtk_init_check\", \"int argc\"); ~%")
(hey "	  Xen_check_type(Xen_is_list(argv), argv, 2, \"gtk_init_check\", \"char *argv[]\"); ~%")
(hey "	  len = Xen_list_length(argv); ~%")
(hey "	  ref_argc = Xen_to_C_int(argc); ~%")
(hey "	  if (ref_argc > len) ref_argc = len; ~%")
(hey "	}~%")
(hey "    }~%")
(hey "  ref_argv = (char**)calloc(ref_argc, sizeof(char*)); ~%")
(hey "  { ~%")
(hey "    int i; ~%")
(hey "    Xen lst; ~%")
(hey "    lst = Xen_copy_arg(argv); ~%")
(hey "    for (i = 0; i < ref_argc; i++, lst = Xen_cdr(lst)) ref_argv[i] = Xen_to_C_char_(Xen_car(lst));~%")
(hey "  }~%")
(hey "  {~%")
(hey "    Xen result = Xen_false;~%")
(hey "    result = C_to_Xen_gboolean(gtk_init_check(&ref_argc, &ref_argv));~%")
(hey "    return(Xen_list_3(result, C_to_Xen_int(ref_argc), C_to_Xen_char__(ref_argv)));~%")
(hey "  }~%")
(hey "}~%~%")

(hey "static Xen gxg_make_target_entry(Xen lst)~%")
(hey "{~%")
(hey "  GtkTargetEntry* targets;~%")
(hey "  Xen val;~%")
(hey "  int i, len;~%")
(hey "  #define H_make_target_entry \"(make-target-entry lst): GtkTargetEntry*, each member of 'lst' should be (list target flags info)\"~%")
(hey "  Xen_check_type(Xen_is_list(lst), lst, 1, \"make-target-entry\", \"a list of lists describing each target\");~%")
(hey "  len = Xen_list_length(lst);~%")
(hey "  if (len == 0) return(Xen_false);~%")
(hey "  targets = (GtkTargetEntry *)calloc(len, sizeof(GtkTargetEntry));~%")
(hey "  for (i = 0; i < len; i++)~%")
(hey "    {~%")
(hey "      val = Xen_list_ref(lst, i);~%")
(hey "      targets[i].target = xen_strdup(Xen_string_to_C_string(Xen_list_ref(val, 0)));~%")
(hey "      targets[i].flags = (guint)Xen_ulong_to_C_ulong(Xen_list_ref(val, 1));~%")
(hey "      targets[i].info = (guint)Xen_ulong_to_C_ulong(Xen_list_ref(val, 2));~%")
(hey "    }~%")
(hey "  return(C_to_Xen_GtkTargetEntry_(targets));~%")
(hey "}~%")

(hey "/* ---------------------------------------- structs ---------------------------------------- */~%~%")

;;; (hey "  #define Xg_define_reader(Name, Value, A1, A2, A3) Xen_define_procedure(Xg_field_pre #Name Xg_post, Value, A1, A2, A3, #Name \" field reader\")~%")
(hey "  #if HAVE_RUBY~%")
(hey "    #define Xg_define_accessor(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      Xen_define_procedure_with_setter(Xg_field_pre #Name Xg_post, Value, #Name \" field accessor\", Xg_field_pre \"set_\" #Name Xg_post, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%")
(hey "  #if HAVE_SCHEME~%")
(hey "    #define Xg_define_accessor(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      Xen_define_procedure_with_setter(Xg_field_pre #Name Xg_post, Value, #Name \" field accessor\", \"set! \" Xg_field_pre #Name Xg_post, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%~%")
(hey "  #if HAVE_FORTH~%")
(hey "    #define Xg_define_accessor(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      Xen_define_procedure_with_setter(Xg_field_pre #Name Xg_post, Value, #Name \" field accessor\", \"set-\" Xg_field_pre #Name Xg_post, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%")

(define (array->list type)
  (hey "  if (ctype == xg_~A_symbol)~%" (no-stars type))
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)Xen_unwrap_C_pointer(Xen_cadr(val)); ~%" type type)
  (hey "      if (len == -1) {for (i = 0; arr[i]; i++) {}; len = i;}~%")
  (hey "      for (i = len - 1; i >= 0; i--) result = Xen_cons(C_to_Xen_~A(arr[i]), result);~%" (no-stars (deref-type (list type))))
  (hey "    }~%"))

(define (list->array type)
  (hey "  if (type == xg_~A_symbol)~%" (no-stars type))
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)calloc(len + 1, sizeof(~A));~%" type type (deref-type (list type)))
  (hey "      for (i = 0; i < len; i++, val = Xen_cdr(val)) arr[i] = Xen_to_C_~A(Xen_car(val));~%" (no-stars (deref-type (list type))))
  (hey "      return(Xen_list_3(xg_~A_symbol, Xen_wrap_C_pointer(arr), make_xm_obj(arr)));~%" (no-stars type))
  (hey "    }~%"))

(hey "/* conversions */~%")
(hey "static Xen c_array_to_xen_list(Xen val_1, Xen clen)~%")
(hey "{~%")
(hey "  Xen result = Xen_empty_list;~%")
(hey "  Xen val, ctype;~%")
(hey "  int i, len = -1;~%")
(hey "  if (Xen_is_integer(clen))~%")
(hey "    len = Xen_integer_to_C_int(clen);~%")
(hey "  if (!(Xen_is_list(val_1))) return(Xen_false); /* type:location cons */~%")
(hey "  val = Xen_copy_arg(val_1); /* protect Ruby arg */~%")
(hey "  ctype = Xen_car(val);~%")
(for-each array->list listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (not (member type listable-types))
	    (not (string=? type "GError*"))
	    (not (string=? type "GError**"))
	    (member (deref-type (list type)) types))
       (array->list type)))
 types)

;;; gotta handle GList* by hand
(hey "  if (ctype == xg_GList__symbol)~%")
(hey "    { /* tagging these pointers is currently up to the caller */~%")
(hey "      GList* lst;~%")
(hey "      lst = (GList*)Xen_unwrap_C_pointer(Xen_cadr(val));~%")
(hey "      len = g_list_length(lst);~%")
(hey "      for (i = len - 1; i >= 0; i--) result = Xen_cons(C_ulong_to_Xen_ulong(g_list_nth_data(lst, i)), result);~%")
(hey "    }~%")
(hey "  return(result);~%")
(hey "}~%~%")

(hey "static Xen xg_object_get(Xen val, Xen name, Xen string_type)~%")
(hey "{~%")
(hey "  gint temp; gchar *str;~%")
(hey "  Xen_check_type(Xen_is_gpointer(val), val, 1, \"g_object_get\", \"gpointer\");~%")
(hey "  Xen_check_type(Xen_is_string(name), name, 2, \"g_object_get\", \"string\");~%")
(hey "  if (Xen_is_false(string_type))~%")
(hey "    {g_object_get(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), &temp, NULL); return(C_int_to_Xen_integer(temp));}~%")
(hey "  else {g_object_get(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), &str, NULL); return(C_string_to_Xen_string(str));}~%")
(hey "}~%~%")

;;; (g_object_get (GPOINTER (gtk_settings_get_default)) "gtk-enable-tooltips" #f)

(hey "static Xen xg_object_set(Xen val, Xen name, Xen new_val)~%")
(hey "{~%")
(hey "  Xen_check_type(Xen_is_gpointer(val), val, 1, \"g_object_set\", \"gpointer\");~%")
(hey "  Xen_check_type(Xen_is_string(name), name, 2, \"g_object_set\", \"string\");~%")
(hey "  if (Xen_is_boolean(new_val))~%")
(hey "    g_object_set(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), Xen_boolean_to_C_bool(new_val), NULL);~%")
(hey "  else~%")
(hey "    {~%")
(hey "      if (Xen_is_number(new_val))~%")
(hey "        g_object_set(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), Xen_integer_to_C_int(new_val), NULL);~%")
(hey "      else g_object_set(Xen_to_C_gpointer(val), (const gchar *)(Xen_string_to_C_string(name)), Xen_string_to_C_string(new_val), NULL);~%")
(hey "    }~%")
(hey "  return(new_val);~%")
(hey "}~%~%")

(hey "static Xen xg_gtk_event_keyval(Xen event)~%")
(hey "{~%")
(hey " GdkEventKey *e;~%")
(hey " e = Xen_to_C_GdkEventKey_(event);~%")
(hey " return(C_int_to_Xen_integer((int)(e->keyval)));~%")
(hey "}~%~%")

(hey "static Xen xen_list_to_c_array(Xen val, Xen type)~%")
(hey "{~%")
(hey "  int i, len;~%")
(hey "  len = Xen_list_length(val);~%")

(for-each list->array listable-types)
(for-each
 (lambda (type)
   (if (and (derefable type)
	    (not (member type listable-types))
	    (not (string=? type "GError*"))
	    (not (string=? type "GError**"))
	    (member (deref-type (list type)) types))
       (list->array type)))
 types)
(hey "  return(Xen_false);~%")
(hey "}~%~%")

(define (make-reader field)
  ;; gather structs that share field
  ;; if 1 or 2 assert type, if and return,
  ;;   else if on each, assert 0 at end and xen false
  (hey "~%")
  (hey "static Xen gxg_~A(Xen ptr)~%" field)
  (hey "{~%")
  (let ((vals ()))
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
	      (hey "  Xen_check_type(Xen_is__~A(ptr), ptr, 1, ~S, ~S);~%" 
		   (caar vals) field 
		   (caar vals))
	      (if (= (length vals) 2)
		  (hey "  Xen_check_type(Xen_is__~A(ptr) || Xen_is__~A(ptr), ptr, 1, ~S, ~S \" or \" ~S);~%" 
		       (caar vals) (car (cadr vals)) field 
		       (caar vals) (car (cadr vals)))))))
    (let ((ctr 0))
      (for-each
       (lambda (val)
	 (if (or (> (length vals) 2)
		 (and (= (length vals) 2)
		      (= ctr 0)))
	     (hey "  if (Xen_is__~A(ptr)) " (car val))
	     (heyc "  "))
	 (set! ctr (+ ctr 1))
	 (hey "return(C_to_Xen_~A((~A)((Xen_to_C_~A_(ptr))->~A)));~%"
	      (no-stars (cadr val)) (cadr val) (car val) field))
       vals))
    (if (> (length vals) 2)
	(hey "  Xen_check_type(0, ptr, 1, ~S, \"pointer to struct with ~A field\");~%  return(Xen_false);~%"
	     field field))
    (hey "}~%")
    ))

(define (make-writer field)
  (hey "~%")
  (hey "static Xen gxg_set_~A(Xen ptr, Xen val)~%" field)
  (hey "{~%")
  (let ((vals ()))
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
	(format #t "(writer) ~A: not found" field)
	(begin
	  (if (= (length vals) 1)
	      (hey "  Xen_check_type(Xen_is__~A(ptr), ptr, 1, ~S, ~S);~%" 
		   (caar vals) field 
		   (caar vals))
	      (if (= (length vals) 2)
		  (hey "  Xen_check_type(Xen_is__~A(ptr) || Xen_is__~A(ptr), ptr, 1, ~S, ~S \" or \" ~S);~%" 
		       (caar vals) (car (cadr vals)) field 
		       (caar vals) (car (cadr vals)))))))
    (let ((ctr 0))
      (for-each
       (lambda (val)
	 (if (or (> (length vals) 2)
		 (and (= (length vals) 2)
		      (= ctr 0)))
	     (hey "  if (Xen_is__~A(ptr)) " (car val))
	     (heyc "  "))
	 (set! ctr (+ ctr 1))
	 (hey "(Xen_to_C_~A_(ptr))->~A = Xen_to_C_~A(val);~%"
	      (car val) field (no-stars (cadr val))))
       vals))
    (if (> (length vals) 2)
	(hey "  Xen_check_type(0, ptr, 1, \"set! ~A\", \"pointer to struct with ~A field\");~% return(Xen_false);~%"
	     field field))
    (hey "  return(val);~%}~%")
    ))

(for-each make-reader (reverse struct-fields))
(for-each make-reader (reverse settable-struct-fields))
(for-each make-writer (reverse settable-struct-fields))

(define (define-struct name)
  (let* ((struct (find-struct name))
	 (strs (cadr struct)))
    ;; cadr of each inner list is field name, car is field type
    (if (= (length strs) 0)
	(begin
	  (hey "static Xen gxg_make_~A(void)~%" name)
	  (hey "{~%")
	  (hey "  ~A* result;~%" name)
	  (hey "  result = (~A*)calloc(1, sizeof(~A));~%" name name)
	  (hey "  return(Xen_list_3(C_string_to_Xen_symbol(~S), Xen_wrap_C_pointer(result), make_xm_obj(result)));~%" 
	       (string-append name "_"))
	  (hey "}~%~%"))
	(begin
	  (hey "static Xen gxg_make_~A(Xen arglist)~%" name)
	  (hey "{~%")
	  (hey "  ~A* result;~%" name)
	  (hey "  int i, len;~%")
	  (hey "  result = (~A*)calloc(1, sizeof(~A));~%" name name)
	  (hey "  len = Xen_list_length(arglist);~%")
	  (hey "  for (i = 0; i < len; i++)~%")
	  (hey "    switch (i)~%")
	  (hey "      {~%")
	  (let ((ctr 0))
	    (for-each
	     (lambda (str)
	       (let ((field-name (cadr str))
		     (field-type (car str)))
		 (hey "      case ~D: result->~A = Xen_to_C_~A(Xen_list_ref(arglist, ~D));~%"
		      ctr field-name (no-stars field-type) ctr)
		 (set! ctr (+ ctr 1))))
	     strs))
	  (hey "      }~%")
	  (hey "  return(Xen_list_3(C_string_to_Xen_symbol(~S), Xen_wrap_C_pointer(result), make_xm_obj(result)));~%" 
	       (string-append name "_"))
	  (hey "}~%~%")))))

(for-each define-struct (reverse make-structs))
(if (not (null? cairo-make-structs))
    (with-cairo hey 
		(lambda () 
		  (for-each define-struct (reverse cairo-make-structs)))))

(with-3.0 hey (lambda ()
		(for-each define-struct (reverse make-structs-3.0))))



;;; ---------------- argify ----------------

(define (argify-func func)
  (let ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 ;(args (- cargs refargs))
	 )
    (hey "Xen_wrap_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs max-args) 
	     "any_args"
	     (if (> refargs 0)
		 (format #f "~D_optional_arg~A" cargs (if (= cargs 1) "" "s"))
		 (format #f "~A_arg~A" (if (zero? cargs) "no" (number->string cargs)) (if (= cargs 1) "" "s"))))
	 (car func) (car func))))

(define (unargify-func func)
  (let (;(cargs (length (caddr func)))
	 ;(refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 ;(args (- cargs refargs))
	 )
    (hey "#define gxg_~A_w gxg_~A~%" 
	 (car func) (car func))))

(for-each argify-func (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (not (null? func-list)) 
       (with-func hey (lambda () 
			(for-each argify-func (reverse func-list))))))
 all-funcs all-func-withs)


(hey "Xen_wrap_1_arg(gxg_GPOINTER_w, gxg_GPOINTER)~%")
(hey "Xen_wrap_2_args(c_array_to_xen_list_w, c_array_to_xen_list)~%")
(hey "Xen_wrap_2_args(xen_list_to_c_array_w, xen_list_to_c_array)~%")
(hey "Xen_wrap_1_arg(gxg_make_target_entry_w, gxg_make_target_entry)~%")
(hey "Xen_wrap_1_arg(c_to_xen_string_w, c_to_xen_string)~%")
(hey "Xen_wrap_3_args(xg_object_get_w, xg_object_get)~%")
(hey "Xen_wrap_3_args(xg_object_set_w, xg_object_set)~%")
(hey "Xen_wrap_1_arg(xg_gtk_event_keyval_w, xg_gtk_event_keyval)~%")

(hey "Xen_wrap_2_optional_args(gxg_gtk_init_w, gxg_gtk_init)~%")
(hey "Xen_wrap_2_optional_args(gxg_gtk_init_check_w, gxg_gtk_init_check)~%")

(define (ruby-cast func) (hey "Xen_wrap_1_arg(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-cast (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (not (null? cast-list)) 
       (cast-func hey (lambda () 
			(for-each ruby-cast (reverse cast-list))))))
 all-casts all-cast-withs)

(define (ruby-check func) (hey "Xen_wrap_1_arg(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func))))
(for-each ruby-check (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (not (null? check-list)) 
       (check-func hey (lambda () 
			 (for-each ruby-check (reverse check-list))))))
 all-checks all-check-withs)


(for-each (lambda (field) (hey "Xen_wrap_1_arg(gxg_~A_w, gxg_~A)~%" field field)) struct-fields)
(for-each (lambda (field) (hey "Xen_wrap_1_arg(gxg_~A_w, gxg_~A)~%" field field)) settable-struct-fields)
(for-each (lambda (field) (hey "Xen_wrap_2_args(gxg_set_~A_w, gxg_set_~A)~%" field field)) settable-struct-fields)

(for-each (lambda (struct) 
	    (let ((s (find-struct struct)))
	      (if (> (length (cadr s)) 0)
		  (hey "Xen_wrap_any_args(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
		  (hey "Xen_wrap_no_args(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
	  (reverse make-structs))
(hey "~%")

(with-cairo hey
	    (lambda ()
	      (for-each 
	       (lambda (struct) 
		 (let ((s (find-struct struct)))
		   (if (> (length (cadr s)) 0)
		       (hey "Xen_wrap_any_args(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
		       (hey "Xen_wrap_no_args(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
	       (reverse cairo-make-structs))))
(hey "~%")

(with-3.0 hey (lambda ()
		(for-each (lambda (struct) 
			    (let ((s (find-struct struct)))
			      (if (> (length (cadr s)) 0)
				  (hey "Xen_wrap_any_args(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
				  (hey "Xen_wrap_no_args(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
			  (reverse make-structs-3.0))
		(hey "~%")))



;;; --------------------------------------------------------------------------------
(hey "  #define Xg_define_procedure(Name, Value, A1, A2, A3, Help) Xen_define_procedure(Xg_pre #Name Xg_post, Value, A1, A2, A3, Help)~%")

(hey "static void define_functions(void)~%")
(hey "{~%")

(hey "  xm_gc_table = Xen_make_vector(1, Xen_false);~%")
(hey "  Xen_GC_protect(xm_gc_table);~%")
(hey "  xm_protected_size = 512;~%")
(hey "  xm_protected = Xen_make_vector(xm_protected_size, Xen_false);~%")
(hey "  Xen_vector_set(xm_gc_table, 0, xm_protected);~%")

(define (defun func)
  (let* ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 (args (- cargs refargs)))
    
    (hey "  Xg_define_procedure(~A, gxg_~A_w, ~D, ~D, ~D, H_~A);~%"
	 (car func) (car func) 
	 (if (>= cargs max-args) 0 args)
	 (if (>= cargs max-args) 0 refargs)
	 (if (>= cargs max-args) 1 0)
	 (car func))))

(for-each defun (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (not (null? func-list)) 
       (with-func hey (lambda () 
			(for-each defun (reverse func-list))))))
 all-funcs all-func-withs)

(define (cast-out func)
  (hey "  Xg_define_procedure(~A, gxg_~A_w, 1, 0, 0, \"(~A obj) casts obj to ~A\");~%" 
       (no-arg (car func)) 
       (no-arg (car func))
       (no-arg (car func))
       (no-arg (car func))))

(hey "  Xg_define_procedure(GPOINTER, gxg_GPOINTER_w, 1, 0, 0, \"(GPOINTER obj) casts obj to GPOINTER\");~%")

(for-each cast-out (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (not (null? cast-list)) 
       (cast-func hey (lambda () 
			(for-each cast-out (reverse cast-list))))))
 all-casts all-cast-withs)


(hey "  Xg_define_procedure(c-array->list, c_array_to_xen_list_w, 2, 0, 0, NULL);~%")
(hey "  Xg_define_procedure(list->c-array, xen_list_to_c_array_w, 2, 0, 0, NULL);~%")
(hey "  Xg_define_procedure(->string, c_to_xen_string_w, 1, 0, 0, NULL);~%")
(hey "  Xg_define_procedure(make-target-entry, gxg_make_target_entry_w, 1, 0, 0, H_make_target_entry);~%")
(hey "  Xg_define_procedure(g_object_get, xg_object_get_w, 3, 0, 0, NULL);~%")
(hey "  Xg_define_procedure(g_object_set, xg_object_set_w, 3, 0, 0, NULL);~%")
(hey "  Xg_define_procedure(gtk_event_keyval, xg_gtk_event_keyval_w, 1, 0, 0, NULL);~%")

(hey "  Xg_define_procedure(gtk_init, gxg_gtk_init_w, 0, 2, 0, H_gtk_init);~%")
(hey "  Xg_define_procedure(gtk_init_check, gxg_gtk_init_check_w, 0, 2, 0, H_gtk_init_check);~%")

(define (check-out func)
  (hey "  Xg_define_procedure(~A, gxg_~A_w, 1, 0, 0, \"(~A obj): \" PROC_TRUE \" if obj is a ~A\");~%" 
       (no-arg (car func)) 
       (no-arg (car func))
       (no-arg (car func))
       (no-arg (car func))))

(for-each check-out (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (not (null? check-list)) 
       (check-func hey (lambda () 
			 (for-each check-out (reverse check-list))))))
 all-checks all-check-withs)

(hey "}~%~%")


(hey "static void define_structs(void)~%")
(hey "{~%")

(for-each (lambda (field) (hey "  Xg_define_reader(~A, gxg_~A_w, 1, 0, 0);~%" field field)) struct-fields)
(for-each (lambda (field) (hey "  Xg_define_accessor(~A, gxg_~A_w, gxg_set_~A_w, 1, 0, 2, 0);~%" field field field)) settable-struct-fields)

(for-each (lambda (struct)
	    (let ((s (find-struct struct)))
	      (hey "  Xg_define_procedure(~A, gxg_make_~A_w, 0, 0, ~D, \"(~A~A): a new ~A struct\");~%" 
		   struct 
		   struct 
		   (if (> (length (cadr s)) 0) 1 0)
		   struct
		   (if (> (length (cadr s)) 0) " ..." "")
		   struct)))
	  (reverse make-structs))

(if (not (null? cairo-make-structs))
    (with-cairo hey
		(lambda ()
		  (for-each 
		   (lambda (struct)
		     (let ((s (find-struct struct)))
		       (hey "  Xg_define_procedure(~A, gxg_make_~A_w, 0, 0, ~D, \"(~A~A): a new ~A struct\");~%" 
			    struct 
			    struct 
			    (if (> (length (cadr s)) 0) 1 0)
			    struct
			    (if (> (length (cadr s)) 0) " ..." "")
			    struct)))
		   (reverse cairo-make-structs)))))
(with-3.0 hey (lambda ()
		(for-each (lambda (struct)
			    (let ((s (find-struct struct)))
			      (hey "  Xg_define_procedure(~A, gxg_make_~A_w, 0, 0, ~D, \"(~A~A): a new ~A struct\");~%" 
				   struct 
				   struct 
				   (if (> (length (cadr s)) 0) 1 0)
				   struct
				   (if (> (length (cadr s)) 0) " ..." "")
				   struct)))
			  (reverse make-structs-3.0))))

(hey "}~%~%")



(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define define_integer(Name) s7_define_constant(s7, Xg_pre #Name Xg_post, C_int_to_Xen_integer(Name))~%")
(hey "#else~%")
(hey "  #define define_integer(Name) Xen_define(Xg_pre #Name Xg_post, C_int_to_Xen_integer(Name))~%")
(hey "#endif~%")
(hey "~%")
(hey "#if !GLIB_CHECK_VERSION(2,35,0)~%")
(hey "  g_type_init();~%")
(hey "#endif~%")

(for-each 
 (lambda (val) 
   (hey "  define_integer(~A);~%" val)) 
 (reverse ints))

(for-each
 (lambda (ints-list with-ints)
   (if (not (null? ints-list))
       (with-ints hey (lambda () 
			(for-each (lambda (val) 
				    (hey "  define_integer(~A);~%" val)) 
				  (reverse ints-list))))))
 all-ints all-int-withs)

(hey "}~%~%")

(hey "static void define_doubles(void)~%")
(hey "{~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define define_double(Name) s7_define_constant(s7, Xg_pre #Name Xg_post, C_double_to_Xen_real(Name))~%")
(hey "#else~%")
(hey "  #define define_double(Name) Xen_define(Xg_pre #Name Xg_post, C_double_to_Xen_real(Name))~%")
(hey "#endif~%")
(hey "~%")

(for-each
 (lambda (val)
   (hey "  define_double(~A);~%" val))
 (reverse dbls))
(hey "}~%~%")


(hey "/* -------------------------------- predefined Atoms -------------------------------- */~%")
(hey "~%")
(hey "static void define_atoms(void)~%")
(hey "{~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define define_atom(Name) s7_define_constant(s7, Xg_pre #Name Xg_post, C_to_Xen_GdkAtom(Name))~%")
(hey "#else~%")
(hey "  #define define_atom(Name) Xen_define(Xg_pre #Name Xg_post, C_to_Xen_GdkAtom(Name))~%")
(hey "#endif~%")
(hey "~%")

(for-each
 (lambda (atom)
   (hey "  define_atom(~A);~%" atom))
 (reverse atoms))
(hey "}~%~%")


(hey "/* -------------------------------- symbols -------------------------------- */~%")
(hey "~%")
(hey "static void define_symbols(void)~%")
(hey "{~%")

(for-each
 (lambda (typ)
   (hey "  xg_~A_symbol = C_string_to_Xen_symbol(\"~A\");~%" (no-stars typ) (no-stars typ)))
 all-types)
(for-each
 (lambda (typ)
   (hey "  xg_~A_symbol = C_string_to_Xen_symbol(\"~A\");~%" typ typ))
 other-types)
(hey "}~%~%")


(hey "/* -------------------------------- strings -------------------------------- */~%")
(hey "~%")
(hey "static void define_strings(void)~%")
(hey "{~%")
(hey "  ~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define define_string(Name) s7_define_constant(s7, Xg_pre #Name Xg_post, s7_make_permanent_string(Name))~%")
(hey "#else~%")
(hey "  #define define_string(Name) Xen_define(Xg_pre #Name Xg_post, C_string_to_Xen_string(Name))~%")
(hey "#endif~%")

(for-each (lambda (str) (hey "  define_string(~A);~%" str)) (reverse strings))
(for-each
 (lambda (strings-list with-strings)
   (if (not (null? strings-list))
       (with-strings hey (lambda () 
			   (for-each (lambda (str) 
				       (hey "  define_string(~A);~%" str)) 
				     (reverse strings-list))))))
 all-strings all-string-withs)

(hey "}~%~%")

(hey "/* -------------------------------- initialization -------------------------------- */~%~%")
(hey "static bool xg_already_inited = false;~%~%")
(hey "void Init_libxg(void);~%")
(hey "void Init_libxg(void)~%")
(hey "{~%")
(hey "  if (!xg_already_inited)~%")
(hey "    {~%")
(hey "      define_symbols();~%")
(hey "      define_xm_obj();~%")
(hey "      define_integers();~%")
(hey "      define_doubles();~%")
(hey "      define_functions();~%")
(hey "      define_structs();~%")
(hey "      define_atoms();~%")
(hey "      define_strings();~%")
(hey "      Xen_provide_feature(\"xg\");~%")
(hey "      #if HAVE_GTK_3~%")
(hey "        Xen_provide_feature(\"gtk3\");~%")
(hey "      #else~%")
(hey "        Xen_provide_feature(\"gtk2\");~%")
(hey "      #endif~%")
(hey "      Xen_define(\"xg-version\", C_string_to_Xen_string(\"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "      xg_already_inited = true;~%")
(hey "#if HAVE_SCHEME~%")
(hey "      /* these are macros in glib/gobject/gsignal.h, but we want the types handled in some convenient way in the extension language */~%")
(hey "      Xen_eval_C_string(\"(define (g_signal_connect obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (not (null? data)) (car data)) #f 0))\");~%")
(hey "      Xen_eval_C_string(\"(define (g_signal_connect_after obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (not (null? data)) (car data)) #f G_CONNECT_AFTER))\");~%")
(hey "      Xen_eval_C_string(\"(define (g_signal_connect_swapped obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (not (null? data)) (car data)) #f G_CONNECT_SWAPPED))\");~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY ~%")
(hey "      Xen_eval_C_string(\"def Rg_signal_connect(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, 0); end\"); ~%")
(hey "      Xen_eval_C_string(\"def Rg_signal_connect_after(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, RG_CONNECT_AFTER); end\"); ~%")
(hey "      Xen_eval_C_string(\"def Rg_signal_connect_swapped(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, RG_CONNECT_SWAPPED); end\"); ~%")
(hey "#endif ~%")
(hey "#if HAVE_FORTH ~%")
(hey "      Xen_eval_C_string(\": Fg_signal_connect <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f 0 Fg_signal_connect_data ;\"); ~%")
(hey "      Xen_eval_C_string(\": Fg_signal_connect_after <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f FG_CONNECT_AFTER Fg_signal_connect_data ;\"); ~%")
(hey "      Xen_eval_C_string(\": Fg_signal_connect_swapped <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f FG_CONNECT_SWAPPED Fg_signal_connect_data ;\"); ~%")
(hey "#endif ~%")
(hey "    }~%")
(hey "}~%")
(hey "#else~%")
(hey " void Init_libxg(void);~%")
(hey " void Init_libxg(void)~%")
(hey "{~%")
(hey "}~%")
(hey "#endif~%") ; have_extension_language

(close-output-port xg-file)

#|
(for-each
 (lambda (type)
   (if (not (assoc type direct-types))
       (format #t ";not direct: ~A~%" type)))
 declared-types)

(for-each
 (lambda (v)
   (if (not (member (car v) declared-types))
       (format #t "~A " (car v))))
 direct-types)
|#

(exit)
