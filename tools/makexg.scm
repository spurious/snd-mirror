;;; makexg.scm creates the gtk2|3/gdk/pango/glib/cairo bindings using xgdata.scm, writes xg.c

(define xg-file (open-output-file "xg.c"))

(define (hey . args)
  (display (apply format #f args) xg-file))

(define (heyc arg)
  (display arg xg-file))

(define names '())
(define types '())
(define ints '())
(define ulongs '())
(define dbls '())
(define funcs '())
(define casts '())
(define checks '())
(define atoms '())
(define strings '())

(define structs '())
(define make-structs '()) ; these have a xg-specific make function
(define cairo-make-structs '())
(define struct-fields '())
(define settable-struct-fields '())

(define funcs-213 '())
(define strings-213 '())
(define ints-213 '())
(define names-213 '())
(define types-213 '())
(define casts-213 '())
(define checks-213 '())
(define ulongs-213 '())

(define funcs-2134 '())
(define strings-2134 '())
(define ints-2134 '())
(define names-2134 '())
(define types-2134 '())
(define casts-2134 '())
(define checks-2134 '())
(define ulongs-2134 '())

(define funcs-2150 '())
(define strings-2150 '())
(define ints-2150 '())
(define names-2150 '())
(define types-2150 '())
(define casts-2150 '())
(define checks-2150 '())
(define ulongs-2150 '())

(define funcs-2172 '())
(define ints-2172 '())
(define names-2172 '())
(define types-2172 '())
(define casts-2172 '())
(define checks-2172 '())

(define funcs-2173 '())
(define ints-2173 '())
(define names-2173 '())
(define types-2173 '())
(define casts-2173 '())
(define checks-2173 '())
(define ulongs-2173 '())

(define funcs-2177 '())
(define ints-2177 '())
(define names-2177 '())
(define types-2177 '())

(define funcs-2190 '())
(define casts-2190 '())
(define checks-2190 '())
(define names-2190 '())
(define types-2190 '())

(define funcs-300 '())
(define casts-300 '())
(define checks-300 '())
(define names-300 '())
(define types-300 '())
(define ints-300 '())
(define strings-300 '())
(define make-structs-300 '()) ; these have a xg-specific make function

(define funcs-310 '())
(define casts-310 '())
(define checks-310 '())
(define names-310 '())
(define types-310 '())
(define ints-310 '())
(define strings-310 '())

(define funcs-312 '())
(define casts-312 '())
(define checks-312 '())
(define names-312 '())
(define types-312 '())
(define ints-312 '())
(define strings-312 '())

(define funcs-336 '())
(define casts-336 '())
(define checks-336 '())
(define names-336 '())
(define types-336 '())
(define ints-336 '())
(define strings-336 '())

(define funcs-3316 '())
(define casts-3316 '())
(define checks-3316 '())
(define names-3316 '())
(define types-3316 '())
(define ints-3316 '())
(define strings-3316 '())

(define funcs-358 '())
(define casts-358 '())
(define checks-358 '())
(define names-358 '())
(define types-358 '())
(define ints-358 '())
(define strings-358 '())

(define funcs-390 '())
(define casts-390 '())
(define checks-390 '())
(define names-390 '())
(define types-390 '())
(define ints-390 '())
(define strings-390 '())

(define funcs-gtk2 '())
(define casts-gtk2 '())
(define checks-gtk2 '())
(define names-gtk2 '())
(define types-gtk2 '())
(define ints-gtk2 '())
(define ulongs-gtk2 '())

(define cairo-funcs '())
(define cairo-png-funcs '())
(define cairo-ints '())
(define cairo-types '())

(define cairo-funcs-810 '())
(define cairo-ints-810 '())
(define cairo-types-810 '())

(define cairo-funcs-912 '())
(define cairo-ints-912 '())
(define cairo-types-912 '())
(define cairo-strings-912 '())
(define cairo-names-912 '())


(define all-types '())

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
	"GtkStateType*" "GtkStatusbar*" "GtkTable*" "GtkTextCharPredicate" "GtkTextTagTableForeach" "GtkTextView*"
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

	"glong"
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
  (let ((data '())
	(sp -1)
	(type #f)
	(len (string-length args)))
    (if (string=? args "void")
	'()
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
				((213 callback-213)   (set! types-213 (cons type types-213)))
				((2134 callback-2134) (set! types-2134 (cons type types-2134)))
				((2150 callback-2150) (set! types-2150 (cons type types-2150)))
				((2172 callback-2172) (set! types-2172 (cons type types-2172)))
				((2173 callback-2173) (set! types-2173 (cons type types-2173)))
				((2177 callback-2177) (set! types-2177 (cons type types-2177)))
				((2190)               (set! types-2190 (cons type types-2190)))
				((300)                (set! types-300 (cons type types-300)))
				((310)                (set! types-310 (cons type types-310)))
				((312)                (set! types-312 (cons type types-312)))
				((336)                (set! types-336 (cons type types-336)))
				((3316)               (set! types-3316 (cons type types-3316)))
				((358)                (set! types-358 (cons type types-358)))
				((390)                (set! types-390 (cons type types-390)))
				((gtk2)               (set! types-gtk2 (cons type types-gtk2)))
				((cairo)              (set! cairo-types (cons type cairo-types)))
				((cairo-810)          (set! cairo-types-810 (cons type cairo-types-810)))
				((cairo-912)          (set! cairo-types-912 (cons type cairo-types-912)))
				(else   	      (if (not (member type types))
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


(define (callback-name func) (car func))
(define (callback-type func) (cadr func))
(define (callback-func func) (caddr func))
(define (callback-args func) (cadddr func))
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
	(cons "GtkStateType" "INT")
	(cons "GtkStateFlags" "INT")
	(cons "GtkImageType" "INT")
	(cons "GtkIconSize" "INT")
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

(define (type-it type)
  (let ((typ (assoc type direct-types)))
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
		 (not (string=? type "GError*"))
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
			       (if (or (string=? type "guint8*")
				       (string=? type "GtkRecentFilterInfo*"))
				   "_PTR_CONST"
				   "_PTR")))
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

(define* (CFNC data spec spec-data) ; 'const -> const for arg cast, 'etc for ... args, 'free -> must free C val before return
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
  (CFNC data 'etc (list min-len max-len types)))

(define (CFNC-23-PA data min-len max-len types)
  (CFNC data 'etc (list min-len max-len types)))

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
	  (let ((strs (parse-args args 22)))
	    (set! funcs-22 (cons (list name type strs args) funcs-22))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-213 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-213: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! types-213 (cons type types-213))))
	  (let ((strs (parse-args args 213)))
	    (if spec
		(set! funcs-213 (cons (list name type strs args spec) funcs-213))
		(set! funcs-213 (cons (list name type strs args) funcs-213)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-2134 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-2134: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! types-2134 (cons type types-2134))))
	  (let ((strs (parse-args args 2134)))
	    (if spec
		(set! funcs-2134 (cons (list name type strs args spec) funcs-2134))
		(set! funcs-2134 (cons (list name type strs args) funcs-2134)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-2150 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-2150: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! types-2150 (cons type types-2150))))
	  (let ((strs (parse-args args 2150)))
	    (if spec
		(set! funcs-2150 (cons (list name type strs args spec) funcs-2150))
		(set! funcs-2150 (cons (list name type strs args) funcs-2150)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-2172 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-2172: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! types-2172 (cons type types-2172))))
	  (let ((strs (parse-args args 2172)))
	    (if spec
		(set! funcs-2172 (cons (list name type strs args spec) funcs-2172))
		(set! funcs-2172 (cons (list name type strs args) funcs-2172)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-2173 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-2173: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! types-2173 (cons type types-2173))))
	  (let ((strs (parse-args args 2173)))
	    (if spec
		(set! funcs-2173 (cons (list name type strs args spec) funcs-2173))
		(set! funcs-2173 (cons (list name type strs args) funcs-2173)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-2177 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-2177: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! types-2177 (cons type types-2177))))
	  (let ((strs (parse-args args 2177)))
	    (if spec
		(set! funcs-2177 (cons (list name type strs args spec) funcs-2177))
		(set! funcs-2177 (cons (list name type strs args) funcs-2177)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-2190 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
	(no-way "CFNC-2190: ~A~%" (list name data))
	(let ((type (car-str data)))
	  (if (not (member type all-types))
	      (begin
		(set! all-types (cons type all-types))
		(set! types-2190 (cons type types-2190))))
	  (let ((strs (parse-args args 2190)))
	    (if spec
		(set! funcs-2190 (cons (list name type strs args spec) funcs-2190))
		(set! funcs-2190 (cons (list name type strs args) funcs-2190)))
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CFNC-300 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
					;    (if (assoc name names)
					;	(no-way "CFNC-300: ~A~%" (list name data))
					; this does not apply because gtk2-only funcs may be on the list
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-300 (cons type types-300))))
      (let ((strs (parse-args args 300)))
	(if spec
	    (set! funcs-300 (cons (list name type strs args spec) funcs-300))
	    (set! funcs-300 (cons (list name type strs args) funcs-300)))
	(set! names (cons (cons name (func-type strs)) names))))))

(define* (CFNC-310 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-310 (cons type types-310))))
      (let ((strs (parse-args args 310)))
	(if spec
	    (set! funcs-310 (cons (list name type strs args spec) funcs-310))
	    (set! funcs-310 (cons (list name type strs args) funcs-310)))
	(set! names (cons (cons name (func-type strs)) names))))))

(define* (CFNC-312 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-312 (cons type types-312))))
      (let ((strs (parse-args args 312)))
	(if spec
	    (set! funcs-312 (cons (list name type strs args spec) funcs-312))
	    (set! funcs-312 (cons (list name type strs args) funcs-312)))
	(set! names (cons (cons name (func-type strs)) names))))))

(define* (CFNC-336 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-336 (cons type types-336))))
      (let ((strs (parse-args args 336)))
	(if spec
	    (set! funcs-336 (cons (list name type strs args spec) funcs-336))
	    (set! funcs-336 (cons (list name type strs args) funcs-336)))
	(set! names (cons (cons name (func-type strs)) names))))))

(define* (CFNC-3316 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-3316 (cons type types-3316))))
      (let ((strs (parse-args args 3316)))
	(if spec
	    (set! funcs-3316 (cons (list name type strs args spec) funcs-3316))
	    (set! funcs-3316 (cons (list name type strs args) funcs-3316)))
	(set! names (cons (cons name (func-type strs)) names))))))

(define* (CFNC-358 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-358 (cons type types-358))))
      (let ((strs (parse-args args 358)))
	(if spec
	    (set! funcs-358 (cons (list name type strs args spec) funcs-358))
	    (set! funcs-358 (cons (list name type strs args) funcs-358)))
	(set! names (cons (cons name (func-type strs)) names))))))

(define* (CFNC-390 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-390 (cons type types-390))))
      (let ((strs (parse-args args 390)))
	(if spec
	    (set! funcs-390 (cons (list name type strs args spec) funcs-390))
	    (set! funcs-390 (cons (list name type strs args) funcs-390)))
	(set! names (cons (cons name (func-type strs)) names))))))

(define* (CFNC-gtk2 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
					;    (if (assoc name names)
					;	(no-way "CFNC-gtk2: ~A~%" (list name data))
					; this does not apply because gtk3 funcs may be on the list
    (let ((type (car-str data)))
      (if (not (member type all-types))
	  (begin
	    (set! all-types (cons type all-types))
	    (set! types-gtk2 (cons type types-gtk2))))
      (let ((strs (parse-args args 'gtk2)))
	(if spec
	    (set! funcs-gtk2 (cons (list name type strs args spec) funcs-gtk2))
	    (set! funcs-gtk2 (cons (list name type strs args) funcs-gtk2)))
	(set! names (cons (cons name (func-type strs)) names))))))


(define* (CAIRO-FUNC data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
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
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CAIRO-PNG-FUNC data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
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
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CAIRO-FUNC-810 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
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
	    (set! names (cons (cons name (func-type strs)) names)))))))

(define* (CAIRO-FUNC-912 data spec)
  (let ((name (cadr-str data))
	(args (caddr-str data)))
    (if (assoc name names)
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
	    (set! names (cons (cons name (func-type strs)) names)))))))


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

(define (CSTR-213 name)
  (if (assoc name names-213)
      (no-way "~A CSTR-213~%" name)
      (begin
	(set! strings-213 (cons name strings-213))
	(set! names-213 (cons (cons name 'string) names-213)))))

(define (CSTR-2150 name)
  (if (assoc name names-2150)
      (no-way "~A CSTR-2150~%" name)
      (begin
	(set! strings-2150 (cons name strings-2150))
	(set! names-2150 (cons (cons name 'string) names-2150)))))

(define (CSTR-300 name)
  (if (assoc name names-300)
      (no-way "~A CSTR-300~%" name)
      (begin
	(set! strings-300 (cons name strings-300))
	(set! names-300 (cons (cons name 'string) names-300)))))

(define (CSTR-310 name)
  (if (assoc name names-310)
      (no-way "~A CSTR-310~%" name)
      (begin
	(set! strings-310 (cons name strings-310))
	(set! names-310 (cons (cons name 'string) names-310)))))

(define (CSTR-312 name)
  (if (assoc name names-312)
      (no-way "~A CSTR-312~%" name)
      (begin
	(set! strings-312 (cons name strings-312))
	(set! names-312 (cons (cons name 'string) names-312)))))

(define (CSTR-336 name)
  (if (assoc name names-336)
      (no-way "~A CSTR-336~%" name)
      (begin
	(set! strings-336 (cons name strings-336))
	(set! names-336 (cons (cons name 'string) names-336)))))

(define (CSTR-3316 name)
  (if (assoc name names-3316)
      (no-way "~A CSTR-3316~%" name)
      (begin
	(set! strings-3316 (cons name strings-3316))
	(set! names-3316 (cons (cons name 'string) names-3316)))))

(define (CSTR-358 name)
  (if (assoc name names-358)
      (no-way "~A CSTR-358~%" name)
      (begin
	(set! strings-358 (cons name strings-358))
	(set! names-358 (cons (cons name 'string) names-358)))))

(define (CSTR-390 name)
  (if (assoc name names-390)
      (no-way "~A CSTR-390~%" name)
      (begin
	(set! strings-390 (cons name strings-390))
	(set! names-390 (cons (cons name 'string) names-390)))))


(define (CDBL name)
  (if (assoc name names)
      (no-way "~A CDBL~%" name)
      (begin
	(set! dbls (cons name dbls))
	(set! names (cons (cons name 'dbl) names)))))

(define declared-types '())
(define (save-declared-type type)
  (if (and type
	   (not (member type declared-types)))
      (set! declared-types (cons type declared-types))))

(define* (CLNG name type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG~%" name)
      (begin
	(set! ulongs (cons (list name type spec-name) ulongs))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-gtk2 name type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-gtk2~%" name)
      (begin
	(set! ulongs-gtk2 (cons (list name type spec-name) ulongs-gtk2))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-213 name type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-213~%" name)
      (begin
	(set! ulongs-213 (cons (list name type spec-name) ulongs-213))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CLNG-2173 name type spec-name)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CLNG-2173~%" name)
      (begin
	(set! ulongs-2173 (cons (list name type spec-name) ulongs-2173))
	(set! names (cons (cons name 'ulong) names)))))

(define* (CINT name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT~%" name)
      (begin
	(set! ints (cons name ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-213 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-213~%" name)
      (begin
	(set! ints-213 (cons name ints-213))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-2134 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-2134~%" name)
      (begin
	(set! ints-2134 (cons name ints-2134))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-2150 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-2150~%" name)
      (begin
	(set! ints-2150 (cons name ints-2150))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-2172 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-2172~%" name)
      (begin
	(set! ints-2172 (cons name ints-2172))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-2173 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-2173~%" name)
      (begin
	(set! ints-2173 (cons name ints-2173))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-2177 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-2177~%" name)
      (begin
	(set! ints-2177 (cons name ints-2177))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-300 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-300~%" name)
      (begin
	(set! ints-300 (cons name ints-300))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-310 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-310~%" name)
      (begin
	(set! ints-310 (cons name ints-310))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-312 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-312~%" name)
      (begin
	(set! ints-312 (cons name ints-312))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-336 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-336~%" name)
      (begin
	(set! ints-336 (cons name ints-336))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-3316 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-3316~%" name)
      (begin
	(set! ints-3316 (cons name ints-3316))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-358 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-358~%" name)
      (begin
	(set! ints-358 (cons name ints-358))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-390 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-390~%" name)
      (begin
	(set! ints-390 (cons name ints-390))
	(set! names (cons (cons name 'int) names)))))

(define* (CINT-gtk2 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CINT-gtk2~%" name)
      (begin
	(set! ints-gtk2 (cons name ints-gtk2))
	(set! names (cons (cons name 'int) names)))))

(define* (CAIRO-INT name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CAIRO-INT~%" name)
      (begin
	(set! cairo-ints (cons name cairo-ints))
	(set! names (cons (cons name 'int) names)))))

(define* (CAIRO-INT-810 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CAIRO-INT-810~%" name)
      (begin
	(set! cairo-ints-810 (cons name cairo-ints-810))
	(set! names (cons (cons name 'int) names)))))

(define* (CAIRO-INT-912 name type)
  (save-declared-type type)
  (if (assoc name names)
      (no-way "~A CAIRO-INT-912~%" name)
      (begin
	(set! cairo-ints-912 (cons name cairo-ints-912))
	(set! names (cons (cons name 'int) names)))))

(define (CAIRO-STRING-912 name)
  (if (assoc name names)
      (no-way "~A CAIRO-STRING-912~%" name)
      (begin
	(set! cairo-strings-912 (cons name cairo-strings-912))
	(set! names (cons (cons name 'string) names)))))


(define (CCAST name type) ; this is the cast (type *)obj essentially but here it's (list type* (cadr obj))
  (if (assoc name names)
      (no-way "~A CCAST~%" name)
      (begin
	;;(if (not (member type types))
	;;    (set! types (cons type types)))
	(set! casts (cons (list name type) casts))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-213 name type)
  (if (assoc name names)
      (no-way "~A CCAST-213~%" name)
      (begin
	(set! casts-213 (cons (list name type) casts-213))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-2172 name type)
  (if (assoc name names)
      (no-way "~A CCAST-2172~%" name)
      (begin
	(set! casts-2172 (cons (list name type) casts-2172))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-2173 name type)
  (if (assoc name names)
      (no-way "~A CCAST-2173~%" name)
      (begin
	(set! casts-2173 (cons (list name type) casts-2173))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-2190 name type)
  (if (assoc name names)
      (no-way "~A CCAST-2190~%" name)
      (begin
	(set! casts-2190 (cons (list name type) casts-2190))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-300 name type)
  (if (assoc name names)
      (no-way "~A CCAST-300~%" name)
      (begin
	(set! casts-300 (cons (list name type) casts-300))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-310 name type)
  (if (assoc name names)
      (no-way "~A CCAST-310~%" name)
      (begin
	(set! casts-310 (cons (list name type) casts-310))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-312 name type)
  (if (assoc name names)
      (no-way "~A CCAST-312~%" name)
      (begin
	(set! casts-312 (cons (list name type) casts-312))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-336 name type)
  (if (assoc name names)
      (no-way "~A CCAST-336~%" name)
      (begin
	(set! casts-336 (cons (list name type) casts-336))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-3316 name type)
  (if (assoc name names)
      (no-way "~A CCAST-3316~%" name)
      (begin
	(set! casts-3316 (cons (list name type) casts-3316))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-358 name type)
  (if (assoc name names)
      (no-way "~A CCAST-358~%" name)
      (begin
	(set! casts-358 (cons (list name type) casts-358))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-390 name type)
  (if (assoc name names)
      (no-way "~A CCAST-390~%" name)
      (begin
	(set! casts-390 (cons (list name type) casts-390))
	(set! names (cons (cons name 'def) names)))))

(define (CCAST-gtk2 name type)
  (if (assoc name names)
      (no-way "~A CCAST-gtk2~%" name)
      (begin
	(set! casts-gtk2 (cons (list name type) casts-gtk2))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK name type)
  (if (assoc name names)
      (no-way "~A CCHK~%" name)
      (begin
	(set! checks (cons (list name type) checks))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-213 name type)
  (if (assoc name names)
      (no-way "~A CCHK-213~%" name)
      (begin
	(set! checks-213 (cons (list name type) checks-213))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-2172 name type)
  (if (assoc name names)
      (no-way "~A CCHK-2172~%" name)
      (begin
	(set! checks-2172 (cons (list name type) checks-2172))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-2173 name type)
  (if (assoc name names)
      (no-way "~A CCHK-2173~%" name)
      (begin
	(set! checks-2173 (cons (list name type) checks-2173))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-2190 name type)
  (if (assoc name names)
      (no-way "~A CCHK-2190~%" name)
      (begin
	(set! checks-2190 (cons (list name type) checks-2190))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-300 name type)
  (if (assoc name names)
      (no-way "~A CCHK-300~%" name)
      (begin
	(set! checks-300 (cons (list name type) checks-300))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-310 name type)
  (if (assoc name names)
      (no-way "~A CCHK-310~%" name)
      (begin
	(set! checks-310 (cons (list name type) checks-310))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-312 name type)
  (if (assoc name names)
      (no-way "~A CCHK-312~%" name)
      (begin
	(set! checks-312 (cons (list name type) checks-312))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-336 name type)
  (if (assoc name names)
      (no-way "~A CCHK-336~%" name)
      (begin
	(set! checks-336 (cons (list name type) checks-336))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-3316 name type)
  (if (assoc name names)
      (no-way "~A CCHK-3316~%" name)
      (begin
	(set! checks-3316 (cons (list name type) checks-3316))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-358 name type)
  (if (assoc name names)
      (no-way "~A CCHK-358~%" name)
      (begin
	(set! checks-358 (cons (list name type) checks-358))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-390 name type)
  (if (assoc name names)
      (no-way "~A CCHK-390~%" name)
      (begin
	(set! checks-390 (cons (list name type) checks-390))
	(set! names (cons (cons name 'def) names)))))

(define (CCHK-gtk2 name type)
  (if (assoc name names)
      (no-way "~A CCHK-gtk2~%" name)
      (begin
	(set! checks-gtk2 (cons (list name type) checks-gtk2))
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

(define (STRUCT-300-make data)
  (STRUCT data)
  (set! make-structs-300 (cons (car-str data) make-structs-300)))

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
(define listable-types '())
(for-each
 (lambda (type)
   (let* ((len (string-length type))
	  (dereftype (if (and (char=? (type (- len 1)) #\*)
			      (not (string=? type "char*")) ; these are surely strings (and set would need XEN_TO_C_gchar etc)
			      (not (string=? type "GError*"))
			      (not (string=? type "GError**"))
			      (not (string=? type "gchar*")))
			 (substring type 0 (- len 1)) 
			 #f)))
     (if (and dereftype
	      (assoc dereftype direct-types))
	 (set! listable-types (cons type listable-types)))))
 types)


(define (with-213 dpy thunk)
  (dpy "#if HAVE_GTK_TEST_WIDGET_CLICK~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-2134 dpy thunk)
  (dpy "#if HAVE_GTK_ADJUSTMENT_GET_UPPER~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-2150 dpy thunk)
  (dpy "#if HAVE_GTK_SCALE_ADD_MARK~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-2172 dpy thunk)
  (dpy "#if HAVE_GTK_INFO_BAR_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-2173 dpy thunk)
  (dpy "#if HAVE_GTK_STATUS_ICON_GET_TITLE~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-2177 dpy thunk)
  (dpy "#if HAVE_GTK_WIDGET_GET_VISIBLE~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-2190 dpy thunk)
  (dpy "#if HAVE_GTK_WIDGET_GET_MAPPED~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-300 dpy thunk)
  (dpy "#if HAVE_GTK_COMBO_BOX_NEW_WITH_AREA~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-310 dpy thunk)
  (dpy "#if HAVE_GTK_ADJUSTMENT_GET_MINIMUM_INCREMENT~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-312 dpy thunk)
  (dpy "#if HAVE_GTK_FONT_CHOOSER_GET_FONT_SIZE~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-336 dpy thunk)
  (dpy "#if HAVE_GTK_APPLICATION_WINDOW_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-3316 dpy thunk)
  (dpy "#if HAVE_GTK_COLOR_CHOOSER_DIALOG_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-358 dpy thunk)
  (dpy "#if HAVE_GTK_LEVEL_BAR_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-390 dpy thunk)
  (dpy "#if HAVE_GTK_HEADER_BAR_NEW~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-gtk2 dpy thunk)
  (dpy "#if (!HAVE_GTK_3)~%")
  (thunk)
  (dpy "#endif~%~%"))
  

(define (with-cairo dpy thunk)
  (thunk)
  )

(define (with-cairo-png dpy thunk)
  (thunk)
  )

(define (with-cairo-810 dpy thunk)
  (dpy "#if HAVE_CAIRO_GLYPH_ALLOCATE~%")
  (thunk)
  (dpy "#endif~%~%"))

(define (with-cairo-912 dpy thunk)
  (dpy "#if HAVE_CAIRO_REGION_XOR && HAVE_GTK_COMBO_BOX_NEW_WITH_AREA~%")
  (thunk)
  (dpy "#endif~%~%"))



(define all-types (list types-213 types-2134 types-2150 types-2172 types-2173 types-2177 types-2190 
			types-300 types-310 types-312 types-336 types-3316 types-358 types-390 types-gtk2
			cairo-types cairo-types-810 cairo-types-912))
(define all-type-withs (list with-213 with-2134 with-2150 with-2172 with-2173 with-2177 with-2190 
			     with-300 with-310 with-312 with-336 with-3316 with-358 with-390 with-gtk2
			     with-cairo with-cairo-810 with-cairo-912))

(define all-funcs (list funcs-213 funcs-2134 funcs-2150 funcs-2172 funcs-2173 funcs-2177 funcs-2190 
			funcs-300 funcs-310 funcs-312 funcs-336 funcs-3316 funcs-358 funcs-390 funcs-gtk2
			cairo-funcs cairo-png-funcs cairo-funcs-810 cairo-funcs-912))
(define all-func-withs (list with-213 with-2134 with-2150 with-2172 with-2173 with-2177 with-2190 
			     with-300 with-310 with-312 with-336 with-3316 with-358 with-390 with-gtk2
			     with-cairo with-cairo-png with-cairo-810 with-cairo-912))

(define all-ints (list ints-213 ints-2134 ints-2150 ints-2172 ints-2173 ints-2177 
		       ints-300 ints-310 ints-312 ints-336 ints-3316 ints-358 ints-390 ints-gtk2
		       cairo-ints cairo-ints-810 cairo-ints-912))
(define all-int-withs (list with-213 with-2134 with-2150 with-2172 with-2173 with-2177 
			    with-300 with-310 with-312 with-336 with-3316 with-358 with-390 with-gtk2
			    with-cairo with-cairo-810 with-cairo-912))

(define all-casts (list casts-213 casts-2134 casts-2150 casts-2172 casts-2173 casts-2190 
			casts-300 casts-310 casts-312 casts-336 casts-3316 casts-358 casts-390 casts-gtk2))
(define all-cast-withs (list with-213 with-2134 with-2150 with-2172 with-2173 with-2190 
			     with-300 with-310 with-312 with-336 with-3316 with-358 with-390 with-gtk2))

(define all-checks (list checks-213 checks-2134 checks-2150 checks-2172 checks-2173 checks-2190 
			 checks-300 checks-310 checks-312 checks-336 checks-3316 checks-358 checks-390 checks-gtk2))
(define all-check-withs (list with-213 with-2134 with-2150 with-2172 with-2173 with-2190 
			      with-300 with-310 with-312 with-336 with-3316 with-358 with-390 with-gtk2))

(define all-strings (list strings-213 strings-2134 strings-2150 
			  strings-300 strings-310 strings-312 strings-336 strings-3316 strings-358 strings-390 cairo-strings-912))
(define all-string-withs (list with-213 with-2134 with-2150 
			       with-300 with-310 with-312 with-336 with-3316 with-358 with-390 with-cairo-912))

(define all-ulongs (list ulongs-213 ulongs-2134 ulongs-2150 ulongs-2173 ulongs-gtk2))
(define all-ulong-withs (list with-213 with-2134 with-2150 with-2173 with-gtk2))



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
(hey " *     4-Jan-05:  removed deprecated XEN_VECTOR_ELEMENTS.~%")
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
(hey " *     25-Feb:    dialog example in libxm.html~%")
(hey " *                Ruby support via xg-ruby.c~%")
(hey " *     21-Feb:    #f=NULL throughout, gdk-pixbuf, GTypes.~%")
(hey " *     11-Feb-02: initial version.~%")
(hey " */~%~%")

(hey "#include <mus-config.h>~%~%")

(hey "#if HAVE_EXTENSION_LANGUAGE~%~%")

(hey "#if UNDEF_USE_SND~%  #undef USE_SND~%  #define USE_SND 0~%#endif~%~%")

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
(hey "static XEN_OBJECT_TYPE xm_obj_tag;~%")
(hey "#if HAVE_RUBY~%")
(hey "static void *xm_obj_free(XEN obj)~%")
(hey "{~%")
(hey "  void *xobj;~%")
(hey "  xobj = (void *)obj;~%")
(hey "  free(xobj);~%")
(hey "  return(NULL);~%")
(hey "}~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "static void xm_obj_free(XEN obj)~%")
(hey "{~%")
(hey "  void *val;~%")
(hey "  val = (void *)XEN_OBJECT_REF(obj);~%")
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
(hey "static XEN make_xm_obj(void *ptr)~%")
(hey "{~%")
(hey "  XEN_MAKE_AND_RETURN_OBJECT(xm_obj_tag, ptr, 0, xm_obj_free);~%")
(hey "}~%")
(hey "static void define_xm_obj(void)~%")
(hey "{~%")
(hey "#if HAVE_SCHEME~%")
(hey " xm_obj_tag = XEN_MAKE_OBJECT_TYPE(\"<XmObj>\", NULL, xm_obj_free, s7_equalp_xm, NULL, NULL, NULL, NULL, NULL, NULL, NULL);~%")
(hey "#else~%")
(hey "  xm_obj_tag = XEN_MAKE_OBJECT_TYPE(\"XmObj\", sizeof(void *));~%")
(hey "#endif~%")
(hey "#if HAVE_FORTH~%")
(hey "  fth_set_object_free(xm_obj_tag, xm_obj_free);~%")
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
(hey "#if HAVE_FORTH~%")
(hey "  #define XG_PRE \"F\"~%")
(hey "  #define XG_POST \"\"~%")
(hey "  #define XG_FIELD_PRE \"F\"~%")
(hey "#endif~%")
(hey "~%")

(hey "#define WRAP_FOR_XEN(Name, Value) XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(Name), XEN_WRAP_C_POINTER(Value))~%")
(hey "#define WRAP_P(Name, Value) (XEN_LIST_P(Value) && \\~%")
(hey "                            (XEN_LIST_LENGTH(Value) >= 2) && \\~%")
(hey "                            (XEN_SYMBOL_P(XEN_CAR(Value))) && \\~%")
(hey "                            (strcmp(Name, XEN_SYMBOL_TO_C_STRING(XEN_CAR(Value))) == 0))~%")
(hey "~%")
(hey "#define XM_TYPE(Name, XType) \\~%")
;; these are not pointers, so should not use wrap_c_pointer and friends 
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(#Name), C_TO_XEN_ULONG(val)));} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_TO_C_ULONG(XEN_CADR(val)));} \\~%")
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
					;(hey "#define XM_TYPE_NO_P(Name, XType) \\~%")
					;(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));} \\~%")
					;(hey "  static XType XEN_TO_C_ ## Name (XEN val) {return((XType)XEN_UNWRAP_C_POINTER(XEN_CADR(val)));} \\~%")
					;(hey "~%")
(hey "#define XM_TYPE_NO_P_2(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {return(WRAP_FOR_XEN(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_PTR(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_UNWRAP_C_POINTER(XEN_CADR(val)));} \\~%")
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_CONST(Name, XType) \\~%")
(hey "  static XEN C_TO_XEN_ ## Name (const XType val) {if (val) return(WRAP_FOR_XEN(#Name, val)); return(XEN_FALSE);} \\~%")
(hey "  static const XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((const XType)XEN_UNWRAP_C_POINTER(XEN_CADR(val)));} \\~%")
(hey "  static bool XEN_ ## Name ## _P(XEN val) {return(WRAP_P(#Name, val));}~%")
(hey "~%")
(hey "#define XM_TYPE_PTR_1(Name, XType) \\~%")
(hey "  static XType XEN_TO_C_ ## Name (XEN val) {if (XEN_FALSE_P(val)) return(NULL); return((XType)XEN_UNWRAP_C_POINTER(XEN_CADR(val)));} \\~%")
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

					;(hey "#define XEN_lambda_P(Arg) XEN_PROCEDURE_P(Arg)~%")
(hey "#define XEN_GCallback_P(Arg) (XEN_PROCEDURE_P(Arg) && ((XEN_REQUIRED_ARGS_OK(Arg, 2)) || (XEN_REQUIRED_ARGS_OK(Arg, 3)) || (XEN_REQUIRED_ARGS_OK(Arg, 4))))~%")

(define (xen-callback func)
  (hey "#define XEN_TO_C_~A(Arg) XEN_FALSE_P(Arg) ? NULL : gxg_~A~%"
       (symbol->string (callback-name func))
       (callback-func func)))

(for-each xen-callback callbacks)

(hey "#define XEN_TO_C_GCallback(Arg) ((XEN_REQUIRED_ARGS_OK(Arg, 4)) ? (GCallback)gxg_func4 : ((XEN_REQUIRED_ARGS_OK(Arg, 3)) ? (GCallback)gxg_func3 : (GCallback)gxg_func2))~%")
;; (hey "#define XEN_TO_C_GCallback(Arg) ((XEN_REQUIRED_ARGS_OK(Arg, 3)) ? (GCallback)gxg_func3 : (GCallback)gxg_func2)~%")

(hey "#define XEN_TO_C_lambda_data(Arg) (gpointer)gxg_ptr~%")
(hey "#define XEN_lambda_data_P(Arg) 1~%")

;; needed if func returns func of this type
(hey "#define C_TO_XEN_GtkTreeViewSearchPositionFunc(Arg) WRAP_FOR_XEN(\"GtkTreeViewSearchPositionFunc\", Arg)~%")
(hey "#define C_TO_XEN_GtkTreeViewSearchEqualFunc(Arg) WRAP_FOR_XEN(\"GtkTreeViewSearchEqualFunc\", Arg)~%")
					;(hey "#define C_TO_XEN_GtkLinkButtonUriFunc(Arg) WRAP_FOR_XEN(\"GtkLinkButtonUriFunc\", Arg)~%")
					;(hey "#define C_TO_XEN_GtkTreeIterCompareFunc(Arg) WRAP_FOR_XEN(\"GtkTreeViewSearchEqualFunc\", Arg)~%")
					;(hey "#define C_TO_XEN_GtkTreeSelectionFunc(Arg) WRAP_FOR_XEN(\"GtkTreeSelectionFunc\", Arg)~%")
					;(hey "#define C_TO_XEN_GtkMenuPositionFunc(Arg) WRAP_FOR_XEN(\"GtkMenuPositionFunc\", Arg)~%")
					;(hey "#define C_TO_XEN_GtkDestroyNotify(Arg) WRAP_FOR_XEN(\"GtkDestroyNotify\", Arg)~%")
(hey "#define XEN_TO_C_GdkFilterReturn(Arg) (GdkFilterReturn)XEN_TO_C_INT(Arg)~%")

(hey "#define XEN_TO_C_String(Arg) ((XEN_STRING_P(Arg)) ? XEN_TO_C_STRING(Arg) : NULL)~%")
(hey "#define C_TO_XEN_String(Arg) ((Arg != NULL) ? C_TO_XEN_STRING((char *)Arg) : XEN_FALSE)~%")
(hey "#define XEN_String_P(Arg) ((XEN_FALSE_P(Arg)) || (XEN_STRING_P(Arg)))~%")

(hey "static XEN C_TO_XEN_GError_(GError *err)~%")
(hey "{~%")
(hey "  if (err)~%")
(hey "    {~%")
(hey "      XEN msg;~%")
(hey "      msg = C_TO_XEN_STRING(err->message);~%")
(hey "      g_error_free(err);~%")
(hey "      return(msg);~%")
(hey "    }~%")
(hey "  return(XEN_FALSE);~%")
(hey "}~%")


(hey "~%~%/* ---------------------------------------- types ---------------------------------------- */~%~%")

(for-each type-it (reverse types))
(for-each
 (lambda (type-list with-func)
   (if (not (null? type-list)) 
       (with-func hey (lambda () 
			(for-each type-it (reverse type-list))))))
 all-types all-type-withs)


(hey "#define XLS(a, b) XEN_TO_C_gchar_(XEN_LIST_REF(a, b))~%")
(hey "#define XLI(a, b) ((int)XEN_TO_C_INT(XEN_LIST_REF(a, b)))~%")
(hey "#define XLL(a, b) (XEN_TO_C_OFF_T(XEN_LIST_REF(a, b)))~%")
(hey "#define XLG(a, b) XEN_TO_C_GType(XEN_LIST_REF(a, b))~%")
(hey "#define XLT(a, b) XEN_TO_C_GtkTextTag_(XEN_LIST_REF(a, b))~%")
(hey "#define XLA(a, b) ((XEN_INTEGER_P(XEN_LIST_REF(a, b))) ? ((gpointer)XLL(a, b)) : ((XEN_STRING_P(XEN_LIST_REF(a, b))) ? ((gpointer)XLS(a, b)) : ((gpointer)XLG(a, b))))~%~%")

(hey "static XEN c_to_xen_string(XEN str)~%")
(hey "{~%")
(hey "  return(C_TO_XEN_STRING((char *)XEN_UNWRAP_C_POINTER(str)));~%")
(hey "}~%~%")


(hey "/* -------------------------------- gc protection -------------------------------- */~%")
(hey "~%")
(hey "static XEN xm_protected;~%")
(hey "static int xm_protected_size = 0;~%")
(hey "static XEN xm_gc_table;~%")
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
		      ;; I tried to use XEN_ERROR here but it was a no-op for some reason?? 
		      (hey "if (!XEN_LIST_P((XEN)func_info)) return~A;~%  "
			   (if void? 
			       ""
			       (format #f "((~A)0)" (no-stars type))))
		      (if (eq? gctype 'permanent-gcc)
			  (hey "#if (!(defined(__cplusplus)))~%  ")) ; const arg conversion causes trouble if g++
		      (let ((castlen (+ 12 (if (not void?) 
					       (+ 2 (string-length (format #f "return(XEN_TO_C_~A" (no-stars type))))
					       1))))
			(if (not void?)
			    (hey "return(XEN_TO_C_~A("
				 (no-stars type)))
			(hey "XEN_CALL_~D(~A((XEN)func_info),~%"
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
			       (hey "XEN_CADR((XEN)func_info),~%")))
			 args)
			(hey (substring "                                                                      " 0 castlen))
			(hey "c__FUNCTION__)")
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
(hey "  return(XEN_TO_C_BOOLEAN(XEN_CALL_3(XEN_CAR((XEN)data),~%")
(hey "                                     C_TO_XEN_GtkWidget_(w),~%")
(hey "                                     C_TO_XEN_GdkEventAny_(ev),~%")
(hey "                                     XEN_CADR((XEN)data),~%")
(hey "                                     c__FUNCTION__)));~%")
(hey "}~%")
(hey "~%static gboolean gxg_func4(GtkPrintOperation *op, GtkPrintContext *context, gint page_nr, gpointer data)~%")
(hey "{~%")
(hey "  return(XEN_TO_C_BOOLEAN(XEN_CALL_4(XEN_CAR((XEN)data),~%")
(hey "                                     C_TO_XEN_GtkPrintOperation_(op),~%")
(hey "                                     C_TO_XEN_GtkPrintContext_(context),~%")
(hey "                                     C_TO_XEN_INT(page_nr),~%")
(hey "                                     XEN_CADR((XEN)data),~%")
(hey "                                     c__FUNCTION__)));~%")
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
	   (spec (and (> (length data) 4) (data 4)))
	   (spec-data (and (> (length data) 5) (data 5)))
	   (arg-start 0)
	   (line-len 0)
	   (line-max 120)
	   (max-args 10)) 
      
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
      
      (hey "static XEN gxg_~A(" name)
      (if (= (length args) 0)
	  (heyc "void")
	  (if (>= (length args) max-args)
	      (begin
		(heyc "XEN arglist"))
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
			 (hey "XEN ignore_~A" argname)
			 (hey "XEN ~A" argname))))
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
			 (if (char=? ((arg 2) 0) #\{)
			     (begin
			       (set! argc (deref-name arg))
			       (hey "  ~A = XEN_TO_C_~A(~A);~%" (deref-name arg) (deref-type arg) argname))
			     (if (char=? ((arg 2) 0) #\|)
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
	      (if using-result (hey "    XEN result = XEN_FALSE;~%"))
	      (if using-loc (hey "    int loc;~%"))
	      (hey "    XEN gxg_ptr = XEN_LIST_5(~A, func_info, XEN_FALSE, XEN_FALSE, XEN_FALSE);~%"
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
		(hey "    if (XEN_LIST_P(~A)) etc_len = XEN_LIST_LENGTH(~A);~%" list-name list-name)
		(if (> min-len 0)
		    (hey "    if (etc_len < ~D) XEN_OUT_OF_RANGE_ERROR(~S, ~A, ~A, \"... list must have at least ~D entr~A\");~%"
			 min-len name (- cargs 1) list-name min-len (if (= min-len 1) "y" "ies")))
		(hey "    if (etc_len > ~D) XEN_OUT_OF_RANGE_ERROR(~S, ~A, ~A, \"... list too long (max len: ~D)\");~%"
		     max-len name (- cargs 1) list-name max-len)
		(if (not (= modlen 1))
		    (hey "    if ((etc_len % ~D) != 0) XEN_OUT_OF_RANGE_ERROR(~S, ~A, ~A, \"... list len must be multiple of ~D\");~%"
			 modlen name (- cargs 1) list-name modlen))
		(do ((i 0 (+ i 1)))
		    ((= i (- cargs 1)))
		  (let ((arg (args i)))
		    (hey "    p_arg~D = XEN_TO_C_~A(~A);~%" i (no-stars (car arg)) (cadr arg))))
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
					    (string=? argtype "char*")
					    (string=? argtype "GValue*")))
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
			      (hey "    return(result);~%")
			      (hey "    return(XEN_FALSE);~%"))
			  (hey "   }~%"))
			(begin ;'fnc
			  (if (> refargs 0)
			      (let ((previous-arg using-result))
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
				    (hey "  return(XEN_FALSE);~%")))))))
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
			  (hey "    return(XEN_FALSE);~%"))
			(hey ")));~%"))
		    (hey "  }~%")) ;'lambda
		  ))) ; 'begin
	(if (eq? spec 'free)
	    (hey "   rtn = C_TO_XEN_~A(result);~%   g_free(result);~%   return(rtn);~%  }~%" (no-stars return-type)))
	(hey "}~%~%")
	))))


(for-each handle-func (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (not (null? func-list)) 
       (with-func hey (lambda () 
			(for-each handle-func (reverse func-list))))))
 all-funcs all-func-withs)


(hey "#define WRAPPED_OBJECT_P(Obj) (XEN_LIST_P(Obj) && (XEN_LIST_LENGTH(Obj) >= 2) && (XEN_SYMBOL_P(XEN_CAR(Obj))))~%~%")

(define cast-it
  (lambda (cast)
    (let ((cast-name (car cast))
	  (cast-type (cadr cast)))
      (hey "static XEN gxg_~A(XEN obj)" (no-arg cast-name))
      (hey " {return((WRAPPED_OBJECT_P(obj)) ? XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(~S), XEN_CADR(obj)) : XEN_FALSE);}~%" (no-stars cast-type)))))

(hey "static XEN gxg_GPOINTER(XEN obj)")
(hey " {return(XEN_LIST_2(C_STRING_TO_XEN_SYMBOL(\"gpointer\"), (WRAPPED_OBJECT_P(obj)) ? XEN_CADR(obj) : XEN_WRAP_C_POINTER(obj)));}~%")

(for-each cast-it (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (not (null? cast-list)) 
       (cast-func hey (lambda () 
			(for-each cast-it (reverse cast-list))))))
 all-casts all-cast-withs)

;;; checks have to use the built-in macros, not local symbol-based type checks

(define (make-check func)
  (hey "static XEN gxg_~A(XEN obj)" (no-arg (car func)))
  (hey " {return(C_TO_XEN_BOOLEAN(WRAPPED_OBJECT_P(obj) && ~A((GTypeInstance *)XEN_UNWRAP_C_POINTER(XEN_CADR(obj)))));}~%" (no-arg (car func))))

(for-each make-check (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (not (null? check-list)) 
       (check-func hey (lambda () 
			 (for-each make-check (reverse check-list))))))
 all-checks all-check-withs)

(hey "~%~%/* ---------------------------------------- special functions ---------------------------------------- */~%~%")

;;; from Mike Scholz -- improve the error checking
(hey "static XEN gxg_gtk_init(XEN argc, XEN argv) ~%")
(hey "{ ~%")
(hey "  #define H_gtk_init \"void gtk_init(int* argc, char*** argv)\" ~%")
(hey "  int ref_argc = 0; ~%")
(hey "  char** ref_argv = NULL; ~%")
(hey "  if (XEN_BOUND_P(argv)) ~%")
(hey "    { ~%")
(hey "      if (XEN_BOUND_P(argc) && XEN_INTEGER_P(argc) && XEN_TO_C_int(argc) <= XEN_LIST_LENGTH(argv)) ~%")
(hey "	ref_argc = XEN_TO_C_int(argc); ~%")
(hey "      else ref_argc = XEN_LIST_LENGTH(argv); ~%")
(hey "    } ~%")
(hey "  ref_argv = (char**)calloc(ref_argc, sizeof(char*)); ~%")
(hey "  { ~%")
(hey "    int i; ~%")
(hey "    XEN lst; ~%")
(hey "    lst = XEN_COPY_ARG(argv); ~%")
(hey "    for (i = 0; i < ref_argc; i++, lst = XEN_CDR(lst)) ref_argv[i] = XEN_TO_C_char_(XEN_CAR(lst));~%")
(hey "  }~%")
(hey "  gtk_init(&ref_argc, &ref_argv);~%")
(hey "  return(XEN_LIST_2(C_TO_XEN_int(ref_argc), C_TO_XEN_char__(ref_argv)));~%")
(hey "} ~%")
(hey "~%")
(hey "static XEN gxg_gtk_init_check(XEN argc, XEN argv) ~%")
(hey "{ ~%")
(hey "  #define H_gtk_init_check \"gboolean gtk_init_check(int* argc, char*** argv)\" ~%")
(hey "  int ref_argc = 0; ~%")
(hey "  char** ref_argv = NULL; ~%")
(hey "  if (XEN_BOUND_P(argc) && XEN_LIST_P(argc)) ~%")
(hey "    { ~%")
(hey "      argv = argc; ~%")
(hey "      ref_argc = XEN_LIST_LENGTH(argv); ~%")
(hey "    } ~%")
(hey "  else ~%")
(hey "    {~%")
(hey "      if (XEN_BOUND_P(argv)) ~%")
(hey "	{ ~%")
(hey "	  int len; ~%")
(hey "	  XEN_ASSERT_TYPE(XEN_INTEGER_P(argc), argc, 1, \"gtk_init_check\", \"int argc\"); ~%")
(hey "	  XEN_ASSERT_TYPE(XEN_LIST_P(argv), argv, 2, \"gtk_init_check\", \"char *argv[]\"); ~%")
(hey "	  len = XEN_LIST_LENGTH(argv); ~%")
(hey "	  ref_argc = XEN_TO_C_int(argc); ~%")
(hey "	  if (ref_argc > len) ref_argc = len; ~%")
(hey "	}~%")
(hey "    }~%")
(hey "  ref_argv = (char**)calloc(ref_argc, sizeof(char*)); ~%")
(hey "  { ~%")
(hey "    int i; ~%")
(hey "    XEN lst; ~%")
(hey "    lst = XEN_COPY_ARG(argv); ~%")
(hey "    for (i = 0; i < ref_argc; i++, lst = XEN_CDR(lst)) ref_argv[i] = XEN_TO_C_char_(XEN_CAR(lst));~%")
(hey "  }~%")
(hey "  {~%")
(hey "    XEN result = XEN_FALSE;~%")
(hey "    result = C_TO_XEN_gboolean(gtk_init_check(&ref_argc, &ref_argv));~%")
(hey "    return(XEN_LIST_3(result, C_TO_XEN_int(ref_argc), C_TO_XEN_char__(ref_argv)));~%")
(hey "  }~%")
(hey "}~%~%")

(hey "static XEN gxg_make_target_entry(XEN lst)~%")
(hey "{~%")
(hey "  GtkTargetEntry* targets;~%")
(hey "  XEN val;~%")
(hey "  int i, len;~%")
(hey "  #define H_make_target_entry \"(make-target-entry lst): GtkTargetEntry*, each member of 'lst' should be (list target flags info)\"~%")
(hey "  XEN_ASSERT_TYPE(XEN_LIST_P(lst), lst, XEN_ONLY_ARG, \"make-target-entry\", \"a list of lists describing each target\");~%")
(hey "  len = XEN_LIST_LENGTH(lst);~%")
(hey "  if (len == 0) return(XEN_FALSE);~%")
(hey "  targets = (GtkTargetEntry *)calloc(len, sizeof(GtkTargetEntry));~%")
(hey "  for (i = 0; i < len; i++)~%")
(hey "    {~%")
(hey "      val = XEN_LIST_REF(lst, i);~%")
(hey "      targets[i].target = xen_strdup(XEN_TO_C_STRING(XEN_LIST_REF(val, 0)));~%")
(hey "      targets[i].flags = (guint)XEN_TO_C_ULONG(XEN_LIST_REF(val, 1));~%")
(hey "      targets[i].info = (guint)XEN_TO_C_ULONG(XEN_LIST_REF(val, 2));~%")
(hey "    }~%")
(hey "  return(C_TO_XEN_GtkTargetEntry_(targets));~%")
(hey "}~%")

(hey "/* ---------------------------------------- structs ---------------------------------------- */~%~%")

;;; (hey "  #define XG_DEFINE_READER(Name, Value, A1, A2, A3) XEN_DEFINE_PROCEDURE(XG_FIELD_PRE #Name XG_POST, Value, A1, A2, A3, #Name \" field reader\")~%")
(hey "  #if HAVE_RUBY~%")
(hey "    #define XG_DEFINE_ACCESSOR(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      XEN_DEFINE_PROCEDURE_WITH_SETTER(XG_FIELD_PRE #Name XG_POST, Value, #Name \" field accessor\", XG_FIELD_PRE \"set_\" #Name XG_POST, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%")
(hey "  #if HAVE_SCHEME~%")
(hey "    #define XG_DEFINE_ACCESSOR(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      XEN_DEFINE_PROCEDURE_WITH_SETTER(XG_FIELD_PRE #Name XG_POST, Value, #Name \" field accessor\", \"set! \" XG_FIELD_PRE #Name XG_POST, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%~%")
(hey "  #if HAVE_FORTH~%")
(hey "    #define XG_DEFINE_ACCESSOR(Name, Value, SetValue, A1, A2, A3, A4) \\~%")
(hey "      XEN_DEFINE_PROCEDURE_WITH_SETTER(XG_FIELD_PRE #Name XG_POST, Value, #Name \" field accessor\", \"set-\" XG_FIELD_PRE #Name XG_POST, SetValue, A1, A2, A3, A4)~%")
(hey "  #endif~%")

(define (array->list type)
  (hey "  if (strcmp(ctype, ~S) == 0)~%" (no-stars type))
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)XEN_UNWRAP_C_POINTER(XEN_CADR(val)); ~%" type type)
  (hey "      if (len == -1) {for (i = 0; arr[i]; i++) {}; len = i;}~%")
  (hey "      for (i = len - 1; i >= 0; i--) result = XEN_CONS(C_TO_XEN_~A(arr[i]), result);~%" (no-stars (deref-type (list type))))
  (hey "    }~%"))

(define (list->array type)
  (hey "  if (strcmp(ctype, ~S) == 0)~%" type)
  (hey "    {~%")
  (hey "      ~A arr; arr = (~A)calloc(len + 1, sizeof(~A));~%" type type (deref-type (list type)))
  (hey "      for (i = 0; i < len; i++, val = XEN_CDR(val)) arr[i] = XEN_TO_C_~A(XEN_CAR(val));~%" (no-stars (deref-type (list type))))
  (hey "      return(XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(~S), XEN_WRAP_C_POINTER(arr), make_xm_obj(arr)));~%" (no-stars type))
  (hey "    }~%"))

(hey "/* conversions */~%")
(hey "static XEN c_array_to_xen_list(XEN val_1, XEN clen)~%")
(hey "{~%")
(hey "  XEN result = XEN_EMPTY_LIST;~%")
(hey "  XEN val;~%")
(hey "  int i, len = -1;~%")
(hey "  const char *ctype;~%")
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
	    (not (string=? type "GError*"))
	    (not (string=? type "GError**"))
	    (member (deref-type (list type)) types))
       (array->list type)))
 types)

;;; gotta handle GList* by hand
(hey "  if (strcmp(ctype, \"GList_\") == 0)~%")
(hey "    { /* tagging these pointers is currently up to the caller */~%")
(hey "      GList* lst;~%")
(hey "      lst = (GList*)XEN_UNWRAP_C_POINTER(XEN_CADR(val));~%")
(hey "      len = g_list_length(lst);~%")
(hey "      for (i = len - 1; i >= 0; i--) result = XEN_CONS(C_TO_XEN_ULONG(g_list_nth_data(lst, i)), result);~%")
(hey "    }~%")
(hey "  return(result);~%")
(hey "}~%~%")

(hey "static XEN xg_object_get(XEN val, XEN name, XEN string_type)~%")
(hey "{~%")
(hey "  gint temp; gchar *str;~%")
(hey "  XEN_ASSERT_TYPE(XEN_gpointer_P(val), val, 1, \"g_object_get\", \"gpointer\");~%")
(hey "  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, 2, \"g_object_get\", \"string\");~%")
(hey "  if (XEN_FALSE_P(string_type))~%")
(hey "    {g_object_get(XEN_TO_C_gpointer(val), (const gchar *)(XEN_TO_C_STRING(name)), &temp, NULL); return(C_TO_XEN_INT(temp));}~%")
(hey "  else {g_object_get(XEN_TO_C_gpointer(val), (const gchar *)(XEN_TO_C_STRING(name)), &str, NULL); return(C_TO_XEN_STRING(str));}~%")
(hey "}~%~%")

;;; (g_object_get (GPOINTER (gtk_settings_get_default)) "gtk-enable-tooltips" #f)

(hey "static XEN xg_object_set(XEN val, XEN name, XEN new_val)~%")
(hey "{~%")
(hey "  XEN_ASSERT_TYPE(XEN_gpointer_P(val), val, 1, \"g_object_set\", \"gpointer\");~%")
(hey "  XEN_ASSERT_TYPE(XEN_STRING_P(name), name, 2, \"g_object_set\", \"string\");~%")
(hey "  if (XEN_BOOLEAN_P(new_val))~%")
(hey "    g_object_set(XEN_TO_C_gpointer(val), (const gchar *)(XEN_TO_C_STRING(name)), XEN_TO_C_BOOLEAN(new_val), NULL);~%")
(hey "  else~%")
(hey "    {~%")
(hey "      if (XEN_NUMBER_P(new_val))~%")
(hey "        g_object_set(XEN_TO_C_gpointer(val), (const gchar *)(XEN_TO_C_STRING(name)), XEN_TO_C_INT(new_val), NULL);~%")
(hey "      else g_object_set(XEN_TO_C_gpointer(val), (const gchar *)(XEN_TO_C_STRING(name)), XEN_TO_C_STRING(new_val), NULL);~%")
(hey "    }~%")
(hey "  return(new_val);~%")
(hey "}~%~%")

(hey "static XEN xg_gtk_event_keyval(XEN event)~%")
(hey "{~%")
(hey " GdkEventKey *e;~%")
(hey " e = XEN_TO_C_GdkEventKey_(event);~%")
(hey " return(C_TO_XEN_INT((int)(e->keyval)));~%")
(hey "}~%~%")

(hey "static XEN xen_list_to_c_array(XEN val, XEN type)~%")
(hey "{~%")
(hey "  int i, len;~%")
(hey "  const char *ctype;~%")
(hey "  len = XEN_LIST_LENGTH(val);~%")
(hey "  ctype = XEN_TO_C_STRING(type);~%")
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
(hey "  return(XEN_FALSE);~%")
(hey "}~%~%")

(define (make-reader field)
  ;; gather structs that share field
  ;; if 1 or 2 assert type, if and return,
  ;;   else if on each, assert 0 at end and xen false
  (hey "~%")
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
	(hey "  XEN_ASSERT_TYPE(0, ptr, XEN_ONLY_ARG, ~S, \"pointer to struct with ~A field\");~%  return(XEN_FALSE);~%"
	     field field))
    (hey "}~%")
    ))

(define (make-writer field)
  (hey "~%")
  (hey "static XEN gxg_set_~A(XEN ptr, XEN val)~%" field)
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
	(format #t "(writer) ~A: not found" field)
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
	(hey "  XEN_ASSERT_TYPE(0, ptr, XEN_ARG_1, \"set! ~A\", \"pointer to struct with ~A field\");~% return(XEN_FALSE);~%"
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
	  (hey "static XEN gxg_make_~A(void)~%" name)
	  (hey "{~%")
	  (hey "  ~A* result;~%" name)
	  (hey "  result = (~A*)calloc(1, sizeof(~A));~%" name name)
	  (hey "  return(XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(~S), XEN_WRAP_C_POINTER(result), make_xm_obj(result)));~%" 
	       (string-append name "_"))
	  (hey "}~%~%"))
	(begin
	  (hey "static XEN gxg_make_~A(XEN arglist)~%" name)
	  (hey "{~%")
	  (hey "  ~A* result;~%" name)
	  (hey "  int i, len;~%")
	  (hey "  result = (~A*)calloc(1, sizeof(~A));~%" name name)
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
		 (set! ctr (+ ctr 1))))
	     strs))
	  (hey "      }~%")
	  (hey "  return(XEN_LIST_3(C_STRING_TO_XEN_SYMBOL(~S), XEN_WRAP_C_POINTER(result), make_xm_obj(result)));~%" 
	       (string-append name "_"))
	  (hey "}~%~%")))))

(for-each define-struct (reverse make-structs))
(if (not (null? cairo-make-structs))
    (with-cairo hey 
		(lambda () 
		  (for-each define-struct (reverse cairo-make-structs)))))

(with-300 hey (lambda ()
		(for-each define-struct (reverse make-structs-300))))



;;; ---------------- argify ----------------

(define (argify-func func)
  (let ((cargs (length (caddr func)))
	 (refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 ;(args (- cargs refargs))
	 )
    (hey "XEN_~A(gxg_~A_w, gxg_~A)~%" 
	 (if (>= cargs 10) "VARGIFY"
	     (if (> refargs 0)
		 (format #f "ARGIFY_~D" cargs)
		 (format #f "NARGIFY_~D" cargs)))
	 (car func) (car func))))

(define (unargify-func func)
  (let (;(cargs (length (caddr func)))
	 ;(refargs (+ (ref-args (caddr func)) (opt-args (caddr func))))
	 ;(args (- cargs refargs))
	 )
    (hey "#define gxg_~A_w gxg_~A~%" 
	 (car func) (car func))))

(hey "~%#ifdef XEN_ARGIFY_1~%")

(for-each argify-func (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (not (null? func-list)) 
       (with-func hey (lambda () 
			(for-each argify-func (reverse func-list))))))
 all-funcs all-func-withs)


(hey "XEN_NARGIFY_1(gxg_GPOINTER_w, gxg_GPOINTER)~%")
(hey "XEN_NARGIFY_2(c_array_to_xen_list_w, c_array_to_xen_list)~%")
(hey "XEN_NARGIFY_2(xen_list_to_c_array_w, xen_list_to_c_array)~%")
(hey "XEN_NARGIFY_1(gxg_make_target_entry_w, gxg_make_target_entry)~%")
(hey "XEN_NARGIFY_1(c_to_xen_string_w, c_to_xen_string)~%")
(hey "XEN_NARGIFY_3(xg_object_get_w, xg_object_get)~%")
(hey "XEN_NARGIFY_3(xg_object_set_w, xg_object_set)~%")
(hey "XEN_NARGIFY_1(xg_gtk_event_keyval_w, xg_gtk_event_keyval)~%")

(hey "XEN_ARGIFY_2(gxg_gtk_init_w, gxg_gtk_init)~%")
(hey "XEN_ARGIFY_2(gxg_gtk_init_check_w, gxg_gtk_init_check)~%")

(define (ruby-cast func) (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-cast (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (not (null? cast-list)) 
       (cast-func hey (lambda () 
			(for-each ruby-cast (reverse cast-list))))))
 all-casts all-cast-withs)

(define (ruby-check func) (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" (no-arg (car func)) (no-arg (car func))))
(for-each ruby-check (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (not (null? check-list)) 
       (check-func hey (lambda () 
			 (for-each ruby-check (reverse check-list))))))
 all-checks all-check-withs)


(for-each (lambda (field) (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" field field)) struct-fields)
(for-each (lambda (field) (hey "XEN_NARGIFY_1(gxg_~A_w, gxg_~A)~%" field field)) settable-struct-fields)
(for-each (lambda (field) (hey "XEN_NARGIFY_2(gxg_set_~A_w, gxg_set_~A)~%" field field)) settable-struct-fields)

(for-each (lambda (struct) 
	    (let ((s (find-struct struct)))
	      (if (> (length (cadr s)) 0)
		  (hey "XEN_VARGIFY(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
		  (hey "XEN_NARGIFY_0(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
	  (reverse make-structs))
(hey "~%")

(with-cairo hey
	    (lambda ()
	      (for-each 
	       (lambda (struct) 
		 (let ((s (find-struct struct)))
		   (if (> (length (cadr s)) 0)
		       (hey "XEN_VARGIFY(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
		       (hey "XEN_NARGIFY_0(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
	       (reverse cairo-make-structs))))
(hey "~%")

(with-300 hey (lambda ()
		(for-each (lambda (struct) 
			    (let ((s (find-struct struct)))
			      (if (> (length (cadr s)) 0)
				  (hey "XEN_VARGIFY(gxg_make_~A_w, gxg_make_~A)~%" struct struct)
				  (hey "XEN_NARGIFY_0(gxg_make_~A_w, gxg_make_~A)~%" struct struct))))
			  (reverse make-structs-300))
		(hey "~%")))


(hey "~%#else~%")
(hey "/* not XEN_ARGIFY_1 */~%")

(for-each unargify-func (reverse funcs))
(for-each
 (lambda (func-list with-func)
   (if (not (null? func-list)) 
       (with-func hey (lambda () 
			(for-each unargify-func (reverse func-list))))))
 all-funcs all-func-withs)


(hey "#define gxg_GPOINTER_w gxg_GPOINTER~%")
(hey "#define c_array_to_xen_list_w c_array_to_xen_list~%")
(hey "#define xen_list_to_c_array_w xen_list_to_c_array~%")
(hey "#define gxg_make_target_entry_w gxg_make_target_entry~%")
(hey "#define c_to_xen_string_w c_to_xen_string~%")
(hey "#define xg_object_get_w xg_object_get~%")
(hey "#define xg_object_set_w xg_object_set~%")
(hey "#define xg_gtk_event_keyval_w xg_gtk_event_keyval~%")

(hey "#define gxg_gtk_init_w gxg_gtk_init~%")
(hey "#define gxg_gtk_init_check_w gxg_gtk_init_check~%")

(define (ruby-uncast func) (hey "#define gxg_~A_w gxg_~A~%" (no-arg (car func)) (no-arg (car func)))) 
(for-each ruby-uncast (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (not (null? cast-list)) 
       (cast-func hey (lambda () 
			(for-each ruby-uncast (reverse cast-list))))))
 all-casts all-cast-withs)


(define (ruby-uncheck func) (hey "#define gxg_~A_w gxg_~A~%" (no-arg (car func)) (no-arg (car func))))
(for-each ruby-uncheck (reverse checks))
(for-each
 (lambda (check-list check-func)
   (if (not (null? check-list)) 
       (check-func hey (lambda () 
			 (for-each ruby-uncheck (reverse check-list))))))
 all-checks all-check-withs)

(for-each (lambda (field) (hey "#define gxg_~A_w gxg_~A~%" field field)) struct-fields)
(for-each (lambda (field) (hey "#define gxg_~A_w gxg_~A~%" field field)) settable-struct-fields)
(for-each (lambda (field) (hey "#define gxg_set_~A_w gxg_set_~A~%" field field)) settable-struct-fields)

(for-each (lambda (struct) 
	    (hey "#define gxg_make_~A_w gxg_make_~A~%" struct struct))
	  (reverse make-structs))
(hey "~%")

(if (not (null? cairo-make-structs))
    (with-cairo hey 
		(lambda () 
		  (for-each 
		   (lambda (struct) 
		     (hey "#define gxg_make_~A_w gxg_make_~A~%" struct struct))
		   (reverse cairo-make-structs)))))
(hey "~%")

(with-300 hey (lambda ()
		(for-each (lambda (struct) 
			    (hey "#define gxg_make_~A_w gxg_make_~A~%" struct struct))
			  (reverse make-structs-300))
		(hey "~%")))



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
(for-each
 (lambda (func-list with-func)
   (if (not (null? func-list)) 
       (with-func hey (lambda () 
			(for-each defun (reverse func-list))))))
 all-funcs all-func-withs)

(define (cast-out func)
  (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, \"(~A obj) casts obj to ~A\");~%" 
       (no-arg (car func)) 
       (no-arg (car func))
       (no-arg (car func))
       (no-arg (car func))))

(hey "  XG_DEFINE_PROCEDURE(GPOINTER, gxg_GPOINTER_w, 1, 0, 0, \"(GPOINTER obj) casts obj to GPOINTER\");~%")

(for-each cast-out (reverse casts))
(for-each
 (lambda (cast-list cast-func)
   (if (not (null? cast-list)) 
       (cast-func hey (lambda () 
			(for-each cast-out (reverse cast-list))))))
 all-casts all-cast-withs)


(hey "  XG_DEFINE_PROCEDURE(c-array->list, c_array_to_xen_list_w, 2, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(list->c-array, xen_list_to_c_array_w, 2, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(->string, c_to_xen_string_w, 1, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(make-target-entry, gxg_make_target_entry_w, 1, 0, 0, H_make_target_entry);~%")
(hey "  XG_DEFINE_PROCEDURE(g_object_get, xg_object_get_w, 3, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(g_object_set, xg_object_set_w, 3, 0, 0, NULL);~%")
(hey "  XG_DEFINE_PROCEDURE(gtk_event_keyval, xg_gtk_event_keyval_w, 1, 0, 0, NULL);~%")

(hey "  XG_DEFINE_PROCEDURE(gtk_init, gxg_gtk_init_w, 0, 2, 0, H_gtk_init);~%")
(hey "  XG_DEFINE_PROCEDURE(gtk_init_check, gxg_gtk_init_check_w, 0, 2, 0, H_gtk_init_check);~%")

(define (check-out func)
  (hey "  XG_DEFINE_PROCEDURE(~A, gxg_~A_w, 1, 0, 0, \"(~A obj): \" PROC_TRUE \" if obj is a ~A\");~%" 
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

(for-each (lambda (field) (hey "  XG_DEFINE_READER(~A, gxg_~A_w, 1, 0, 0);~%" field field)) struct-fields)
(for-each (lambda (field) (hey "  XG_DEFINE_ACCESSOR(~A, gxg_~A_w, gxg_set_~A_w, 1, 0, 2, 0);~%" field field field)) settable-struct-fields)

(for-each (lambda (struct)
	    (let ((s (find-struct struct)))
	      (hey "  XG_DEFINE_PROCEDURE(~A, gxg_make_~A_w, 0, 0, ~D, \"(~A~A): a new ~A struct\");~%" 
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
		       (hey "  XG_DEFINE_PROCEDURE(~A, gxg_make_~A_w, 0, 0, ~D, \"(~A~A): a new ~A struct\");~%" 
			    struct 
			    struct 
			    (if (> (length (cadr s)) 0) 1 0)
			    struct
			    (if (> (length (cadr s)) 0) " ..." "")
			    struct)))
		   (reverse cairo-make-structs)))))
(with-300 hey (lambda ()
		(for-each (lambda (struct)
			    (let ((s (find-struct struct)))
			      (hey "  XG_DEFINE_PROCEDURE(~A, gxg_make_~A_w, 0, 0, ~D, \"(~A~A): a new ~A struct\");~%" 
				   struct 
				   struct 
				   (if (> (length (cadr s)) 0) 1 0)
				   struct
				   (if (> (length (cadr s)) 0) " ..." "")
				   struct)))
			  (reverse make-structs-300))))

(hey "}~%~%")



(hey "/* ---------------------------------------- constants ---------------------------------------- */~%~%")
(hey "static void define_integers(void)~%")
(hey "{~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define DEFINE_INTEGER(Name) s7_define_constant(s7, XG_PRE #Name XG_POST, C_TO_XEN_INT(Name))~%")
(hey "  #define DEFINE_ULONG(Name) s7_define_constant(s7, XG_PRE #Name XG_POST, C_TO_XEN_ULONG(Name))~%")
(hey "#else~%")
(hey "  #define DEFINE_INTEGER(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_INT(Name))~%")
(hey "  #define DEFINE_ULONG(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_ULONG(Name))~%")
(hey "#endif~%")
(hey "~%")
(hey "#if !GLIB_CHECK_VERSION(2,35,0)~%")
(hey "  g_type_init();~%")
(hey "#endif~%")

(for-each 
 (lambda (val) 
   (hey "  DEFINE_INTEGER(~A);~%" val)) 
 (reverse ints))

(for-each
 (lambda (ints-list with-ints)
   (if (not (null? ints-list))
       (with-ints hey (lambda () 
			(for-each (lambda (val) 
				    (hey "  DEFINE_INTEGER(~A);~%" val)) 
				  (reverse ints-list))))))
 all-ints all-int-withs)


(for-each 
 (lambda (vals)
   (let ((val (car vals)))
     (hey "  DEFINE_ULONG(~A);~%" val)))
 (reverse ulongs))

(for-each
 (lambda (ulongs-list with-ulongs)
   (if (not (null? ulongs-list))
       (with-ulongs hey (lambda () 
			  (for-each (lambda (vals) 
				      (let ((val (car vals))) 
					(hey "  DEFINE_ULONG(~A);~%" val))) 
				    (reverse ulongs-list))))))
 all-ulongs all-ulong-withs)

(hey "}~%~%")

(hey "static void define_doubles(void)~%")
(hey "{~%")
(hey "#if HAVE_SCHEME~%")
(hey "  #define DEFINE_DOUBLE(Name) s7_define_constant(s7, XG_PRE #Name XG_POST, C_TO_XEN_DOUBLE(Name))~%")
(hey "#else~%")
(hey "  #define DEFINE_DOUBLE(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_DOUBLE(Name))~%")
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
(hey "#if HAVE_SCHEME~%")
(hey "  #define DEFINE_ATOM(Name) s7_define_constant(s7, XG_PRE #Name XG_POST, C_TO_XEN_GdkAtom(Name))~%")
(hey "#else~%")
(hey "  #define DEFINE_ATOM(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_GdkAtom(Name))~%")
(hey "#endif~%")
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
(hey "#if HAVE_SCHEME~%")
(hey "  #define DEFINE_STRING(Name) s7_define_constant(s7, XG_PRE #Name XG_POST, s7_make_permanent_string(Name))~%")
(hey "#else~%")
(hey "  #define DEFINE_STRING(Name) XEN_DEFINE(XG_PRE #Name XG_POST, C_TO_XEN_STRING(Name))~%")
(hey "#endif~%")

(for-each (lambda (str) (hey "  DEFINE_STRING(~A);~%" str)) (reverse strings))
(for-each
 (lambda (strings-list with-strings)
   (if (not (null? strings-list))
       (with-strings hey (lambda () 
			   (for-each (lambda (str) 
				       (hey "  DEFINE_STRING(~A);~%" str)) 
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
(hey "      define_xm_obj();~%")
(hey "      define_integers();~%")
(hey "      define_doubles();~%")
(hey "      define_functions();~%")
(hey "      define_structs();~%")
(hey "      define_atoms();~%")
(hey "      define_strings();~%")
(hey "      XEN_PROVIDE(\"xg\");~%")
(hey "      #if HAVE_GTK_3~%")
(hey "        XEN_PROVIDE(\"gtk3\");~%")
(hey "      #else~%")
(hey "        XEN_PROVIDE(\"gtk2\");~%")
(hey "      #endif~%")
(hey "      XEN_DEFINE(\"xg-version\", C_TO_XEN_STRING(\"~A\"));~%" (strftime "%d-%b-%y" (localtime (current-time))))
(hey "      xg_already_inited = true;~%")
(hey "#if HAVE_SCHEME~%")
(hey "      /* these are macros in glib/gobject/gsignal.h, but we want the types handled in some convenient way in the extension language */~%")
(hey "      XEN_EVAL_C_STRING(\"(define (g_signal_connect obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (not (null? data)) (car data)) #f 0))\");~%")
(hey "      XEN_EVAL_C_STRING(\"(define (g_signal_connect_after obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (not (null? data)) (car data)) #f G_CONNECT_AFTER))\");~%")
(hey "      XEN_EVAL_C_STRING(\"(define (g_signal_connect_swapped obj name func . data) (g_signal_connect_data (GPOINTER obj) name func (and (not (null? data)) (car data)) #f G_CONNECT_SWAPPED))\");~%")
(hey "#endif~%")
(hey "#if HAVE_RUBY ~%")
(hey "      XEN_EVAL_C_STRING(\"def Rg_signal_connect(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, 0); end\"); ~%")
(hey "      XEN_EVAL_C_STRING(\"def Rg_signal_connect_after(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, RG_CONNECT_AFTER); end\"); ~%")
(hey "      XEN_EVAL_C_STRING(\"def Rg_signal_connect_swapped(obj, name, func, data = false); Rg_signal_connect_data(RGPOINTER(obj), name, func, data, false, RG_CONNECT_SWAPPED); end\"); ~%")
(hey "#endif ~%")
(hey "#if HAVE_FORTH ~%")
(hey "      XEN_EVAL_C_STRING(\": Fg_signal_connect <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f 0 Fg_signal_connect_data ;\"); ~%")
(hey "      XEN_EVAL_C_STRING(\": Fg_signal_connect_after <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f FG_CONNECT_AFTER Fg_signal_connect_data ;\"); ~%")
(hey "      XEN_EVAL_C_STRING(\": Fg_signal_connect_swapped <{ obj name func :optional data #f -- n }> obj FGPOINTER name func data #f FG_CONNECT_SWAPPED Fg_signal_connect_data ;\"); ~%")
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
