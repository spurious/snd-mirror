var extsnd_addmark_tip = "(add-mark samp :optional snd chn name (sync 0)):<br>" +  
                         " add a mark at sample 'samp' returning the mark id.";

var extsnd_addsoundfileextension_tip = "(add-sound-file-extension ext):<br>" +
                                       " add the file extension 'ext' to the list of sound file extensions";

var extsnd_addtomenu_tip = "(add-to-menu menu label func :optional position):<br>" +
                           " add label to menu (a main menu index),<br>" +
                           " invoke func (a function of no args) when the new menu is activated.<br>" +
                           " Return the new menu label widget.";

var extsnd_addtransform_tip = "(add-transform name x-label low high func):<br>" +
                              " add the transform func to the transform lists;<br>" +
                              " func should be a function of two arguments, <br>" +
                              " the length of the transform and a sample-reader to get the data, <br>" +
                              " and should return a vct containing the transform results. <br>" +
                              " name is the transform's name, x-label is its x-axis label, <br>" +
                              " and the relevant returned data to be displayed goes from low to high (normally 0.0 to 1.0)";

var extsnd_afterapplycontrolshook_tip = "after-apply-controls-hook (snd): called when apply-controls finishes.";

var extsnd_aftergraphhook_tip = "after-graph-hook (snd chn): called after a graph is updated.";

var extsnd_afteropenhook_tip = "after-open-hook (snd): called just before the new file's window is displayed.<br>" +
                               " This provides a way to set various sound-specific defaults. <pre>" +
                               "  (add-hook! after-open-hook<br>" + 
                               "    (lambda (snd) <br>" +
                               "      (if (> (channels snd) 1) <br>" +
                               "          (set! (channel-style snd) channels-combined))))</pre>";

var extsnd_aftersaveashook_tip = "after-save-as-hook (saved-sound-index save-as-full-filename from-save-as-dialog):<br>" +
                                 " called upon File:Save as or save-sound-as completion.";

var extsnd_aftersavestatehook_tip = "after-save-state-hook (filename): called after Snd state has been saved;<br>" +
                                    " filename is the save state file.";

var extsnd_aftertransformhook_tip = "after-transform-hook (snd chn scaler): called just after a spectrum is calculated.";

var extsnd_ampcontrol_tip = "(amp-control :optional snd chn): current amp slider setting";

var extsnd_applycontrols_tip = "(apply-controls :optional snd (choice 0) (beg 0) (dur len)):<br>" +
                               " applies the current control panel state as an edit. <br>" +
                               " The 'choices' are 0 (apply to sound), 1 (apply to channel), and 2 (apply to selection).<br>" +
                               " If 'beg' is given, the apply starts there.";

var extsnd_asoneedit_tip = "(as-one-edit thunk :optional origin): evaluate thunk,<br>" +
                           " collecting all edits into one from the edit history's point of view";

var extsnd_axisinfo_tip = "(axis-info :optional snd chn (ax time-graph)): info about axis:<br>" +
                          "<pre> (list losamp hisamp x0 y0 x1 y1 xmin ymin xmax ymax pix_x0 pix_y0 pix_x1 pix_y1<br>" +
                          "       y_offset xscale yscale xlabel ylabel new-peaks)</pre>";

var extsnd_axislabelfont_tip = "(axis-label-font): font used for axis labels";

var extsnd_axisnumbersfont_tip = "(axis-numbers-font): font used for axis numbers";

var extsnd_badheaderhook_tip = "bad-header-hook (filename): called if a file has some bogus-looking header.<br>" +
                               " Return #t to give up on that file.";

var extsnd_basiccolor_tip = "(basic-color): Snd's basic color";

var extsnd_beatsperminute_tip = "(beats-per-minute :optional snd chn): beats per minute if x-axis-style is x-axis-in-beats";

var extsnd_beforeclosehook_tip = "before-close-hook (snd): called each time a file is closed (before the close).<br>" +
                                 " If it returns #t, the file is not closed.";

var extsnd_beforeexithook_tip = "before-exit-hook (): called upon exit. If it returns #t, Snd does not exit.<br>" +
                                " This can be used to check for unsaved edits.";

var extsnd_beforesaveashook_tip = "before-save-as-hook (index filename selection srate type format comment):<br>" +
                                  " called before File:Save as or save-sound-as. Provides a way to fixup a sound just before it is saved.";

var extsnd_beforesavestatehook_tip = "before-save-state-hook (filename): called before Snd state is saved.<br>" +
                                     " If the hook functions return #t, the save state process opens 'filename' for appending, rather than truncating.";

var extsnd_bindkey_tip = "(bind-key key modifiers func :optional extended origin prefs-info):<br>" +
                         " causes 'key' (an integer, character, or string) when typed with 'modifiers'<br>" +
                         " (0:none, 4:control, 8:meta) (and C-x if extended) to invoke 'func', a function of zero or one arguments.<br>" +
                         " If the function takes one argument, it is passed the preceding C-u number, if any.<br>" +
                         " The function should return one of the cursor choices (e.g. keyboard-no-action).<br>" +
                         "  'origin' is the name reported if an error occurs.<br>" +
                         " The 'key' argument can be the X/Gtk name of the key (e.g. \"plus\" for \"+\" or \"Home\"),<br>" +
                         " the character on the key (#\x07), or the integer corresponding to that character:<br>" +
                         " (\"(char->integer #\x07)\" in Scheme, or \"?a\" in Ruby.";

var extsnd_boldpeaksfont_tip = "(bold-peaks-font): bold font used by fft peak display";

var extsnd_cgp_tip = "(c-g?): allow pending user interface events to occur, returning #t if C-g was typed";

var extsnd_channels_tip = "(channels :optional snd): how many channels snd has";

var extsnd_channelstyle_tip = "(channel-style :optional snd): how multichannel sounds lay out the channels.<br>" +
                              " The default is channels-combined; other values are channels-separate and channels-superimposed.<br>" +
                              " As a global (if the 'snd' arg is omitted), it is the default setting for each sound's 'unite' button.";

var extsnd_channeltovct_tip = "(channel-&gt;vct :optional beg dur snd chn edpos): return a vct with the specified samples.";

var extsnd_channelwidgets_tip = "(channel-widgets :optional snd chn): a list of widgets: ((0)graph (1)w (2)f (3)sx (4)sy (5)zx (6)zy (7)edhist)";

var extsnd_chans_tip = "(channels :optional snd): how many channels snd has";

var extsnd_clearminibuffer_tip = "(clear-minibuffer :optional snd) clears snd's minibuffer (erasing any error message as well).";

var extsnd_cliphook_tip = "clip-hook (clipping-value) is called each time a sample is about to be clipped<br>" +
                          " upon being written to a sound file.  The hook function can return the new value to be written,<br>" +
                          " or rely on the default (-1.0 or 1.0 depending on the sign of 'clipping-value').";

var extsnd_clmchannel_tip = "(clm-channel gen :optional (beg 0) (dur len) snd chn edpos (overlap 0) origin):<br>" +
                            " apply gen to snd's channel chn starting at beg for dur samples.<br>" +
                            " overlap is the 'ring' time, if any.";

var extsnd_closehook_tip = "close-hook (snd): called each time a file is closed (before the close).";

var extsnd_closesound_tip = "(close-sound :optional snd): close snd";

var extsnd_comment_tip = "(comment :optional snd): snd's comment (in its header)";

var extsnd_countmatches_tip = "(count-matches func :optional (start-samp 0) snd chn edpos):<br>" +
                              " return how many samples satisfy func (a function of one argument,<br>" +
                              " the current sample, returning #t upon match):<br>" +
                              " <code>(count-matches (lambda (y) (> y .1)))</code>";

var extsnd_currentfont_tip = "(current-font :optional snd chn (ax time-graph)): current font id";

var extsnd_cursor_tip = "(cursor :optional snd chn edpos): current cursor location in snd's channel chn";

var extsnd_cursorcontext_tip = "graphics context for the cursor";

var extsnd_dachook_tip = "dac-hook (sdobj): called just before data is sent to DAC passing data as sound-data object";

var extsnd_dacsize_tip = "(dac-size): the current DAC buffer size in frames (256)";

var extsnd_datacolor_tip = "(data-color): color used to draw unselected data";

var extsnd_dataformat_tip = "(data-format :optional snd): snd's data format (e.g. mus-bshort)";

var extsnd_datalocation_tip = "(data-location :optional snd): snd's data location (bytes)";

var extsnd_declare_tip = "Functions embedded within run may need to declare the type of their arguments;<br>" +
                         " run assumes each variable has one type (integer by default) throughout its life.<br>" +
                         " So, the following code displays \"0\", rather than \"3.14\":<br><br>" +
                         " <code>  (run (lambda () (let ((x 3.14)) (define (a b) (display b)) (a x))))</code><br><br>" +
                         " The \"b\" argument to \"a\" is assumed to be an integer, and passing in a float<br>" +
                         " causes nothing but confusion.  To get this code to work right:<br><br>" +
                         " <code>  (run (lambda () (let ((x 3.14)) (define (a b) (<b>declare</b> (b real)) (display b)) (a x))))</code><br><br>" +
                         " The current declarable types include:<br><br>" +
                         " <code>    int float boolean char string list symbol keyword vct sample-reader mix-sample-reader</code><br>" +
                         " <code>    sound-data clm float-vector int-vector vct-vector list-vector clm-vector</code>";

var extsnd_defineenvelope_tip = "(define-envelope name data): define 'name' to have the value 'data'<br>" +
                                " (a list of breakpoints), and load it into the envelope editor.";

var extsnd_defvar_tip = "(defvar name data): define 'name' to have the value 'data'<br>" +
                        " (a list of breakpoints), and load it into the envelope editor.";

var extsnd_deletesamples_tip = "(delete-samples start-samp samps :optional snd chn edpos):<br>" +
                               " delete 'samps' samples from snd's channel chn starting at 'start-samp'";

var extsnd_dialogwidgets_tip = "(dialog-widgets): dialog widgets (each #f if not yet created):<br>" +
                               " <code>(list  (0 color-dialog) (1 orientation-dialog) (2 enved-dialog)<br>" +
                               " (3 #f) (4 #f) (5 transform-dialog)  (6 open-file-dialog) (7 save-sound-dialog)<br>" +
                               " (8 view-files-dialog) (9 raw data dialog) (10 new file dialog)<br>" +
                               " (11 mix-file-dialog) (12 edit-header-dialog) (13 find-dialog)<br>" +
                               " (14 help-dialog) (15 listener completion)  (16 view-mixes-dialog)<br>" +
                               " (17 print-dialog) (18 recorder-dialog) (19 view-regions-dialog)<br>" +
                               " (20 info-dialog) (21 #f) (22 save-selection-dialog)<br>" +
                               " (23 insert-file-dialog)  (24 save-region-dialog) (25 preferences-dialog))</code>";

var extsnd_dotsize_tip = "(dot-size :optional snd chn): size in pixels of dots when graphing with dots (1)";

var extsnd_drawline_tip = "(draw-line x0 y0 x1 y1 :optional snd chn (ax time-graph)): draw a line";

var extsnd_drawstring_tip = "(draw-string text x0 y0 :optional snd chn (ax time-graph)): draw a string";

var extsnd_duringopenhook_tip = "during-open-hook (fd name reason):<br>" +
                                " called after file is opened, but before data has been read.<br>" +
                                "<pre> (add-hook! during-open-hook<br>" +
                                "    (lambda (fd name reason) <br>" +
                                "      (if (= (mus-sound-header-type name) mus-raw)<br>" +
                                "          (set! (mus-file-prescaler fd) 500.0))))</pre>";

var extsnd_editfragment_tip = "(edit-fragment :optional (ctr current-edit-position) snd chn):<br>" +
                              " edit history entry at ctr associated with snd's channel chn;<br>" +
                              " the returned value is a list (origin type start-sample samps)";

var extsnd_editposition_tip = "(edit-position :optional snd chn): current edit history position in snd's channel chn";

var extsnd_edits_tip = "(edits :optional snd chn): -> (list undoable-edits redoable-edits) in snd's channel chn";

var extsnd_emarks_tip = "(marks :optional snd chn edpos): list of marks (ids) in snd/chn<br>" +
                        " at edit history position pos. mark list is: <br>" +
                        " if channel given: (id id ...), <br>" +
                        " if snd given: ((id id) (id id ...)), <br>" +
                        " if neither given: (((id ...) ...) ...).";

var extsnd_envchannel_tip = "(env-channel env-gen-or-envelope :optional (beg 0) (dur len) snd chn edpos):<br>" +
                            " apply amplitude envelope to snd's channel chn starting at beg for dur samples.";

var extsnd_envedtarget_tip = "(enved-target): determines how the envelope edit envelope is applied:<br>" +
                             " enved-amplitude, enved-srate (apply to speed), and enved-spectrum (apply as a filter).";

var extsnd_envedwaving_tip = "(enved-wave?): #t if the envelope editor is displaying the waveform to be edited";

var extsnd_envsound_tip = "(env-sound env :optional (start-samp 0) (samps len) (env-base 1.0) snd chn edpos):<br>" +
                          " apply an amplitude envelope (a list of breakpoints or a CLM env) to snd's channel chn<br>" +
                          " starting at start-samp, going either to the end of the sound or for samps samples,<br>" +
                          " with segments interpolating according to env-base (1 = linear).";

var extsnd_envsoundselection_tip = "(env-selection env :optional (env-base 1.0)):<br>" +
                                   " apply envelope to the selection using env-base to determine how breakpoints are connected";

var extsnd_eregions_tip = "(regions): current active regions (a list of region ids)";

var extsnd_exit_tip = "(exit): exit Snd";

var extsnd_exithook_tip = "exit-hook (): called upon exit.  This can be used to perform cleanup activities.";

var extsnd_expandcontrol_tip = "(expand-control :optional snd): current expand slider setting";

var extsnd_expandcontrolp_tip = "(expand-control? :optional snd): snd's control panel expand button state";

var extsnd_filename_tip = "(file-name :optional snd): snd's full filename";

var extsnd_fillrectangle_tip = "(fill-rectangle x0 y0 width height :optional snd chn (ax time-graph) erase): draw a filled rectangle";

var extsnd_filterchannel_tip = "(filter-channel env :optional order beg dur snd chn edpos (truncate #t) origin):<br>" +
                               " applies an FIR filter to snd's channel chn.<br>" +
                               " 'env' is the frequency response envelope, or a vct with the coefficients.";

var extsnd_filterselection_tip = "(filter-selection filter :optional order (truncate #t)):<br>" +
                                 " apply filter to selection.<br>" +
                                 " If truncate, cut off filter output at end of selection, else mix";

var extsnd_filtersound_tip = "(filter-sound filter :optional order snd chn edpos origin):<br>" +
                             " applies FIR filter to snd's channel chn.<br>" +
                             " 'filter' is either the frequency response envelope,<br>" +
                             " a CLM filter, or a vct with the actual coefficients";

var extsnd_findchannel_tip = "(find-channel func :optional (start-samp 0) snd chn edpos):<br>" +
                             " apply func, a function of one argument, the current sample,<br>" +
                             " to each sample in snd's channel chn, starting at 'start-samp'<br>" +
                             " until func returns something other than #f: <br>" +
                             " <code>  (find-channel (lambda (y) (> y .1)))</code>";

var extsnd_findmark_tip = "(find-mark samp-or-name :optional snd chn edpos):<br>" +
                          " find the mark in snd's channel chn at samp (if a number)<br>" +
                          " or with the given name (if a string);<br>" +
                          " return the mark id or #f if no mark found.";

var extsnd_focuswidget_tip = "(focus-widget widget): cause widget to receive input focus";

var extsnd_foregroundcolor_tip = "(foreground-color :optional snd chn (ax time-graph)): current drawing color";

var extsnd_frames_tip = "(frames :optional snd chn edpos): number of frames of data in snd's channel chn";

var extsnd_freesamplereader_tip = "(free-sample-reader reader): free a sample reader (of any kind)";

var extsnd_gin_tip = "(in msecs thunk): invoke thunk in msecs milliseconds (named call_in in Ruby)";

var extsnd_graph_tip = "(graph data :optional xlabel (x0 0.0) (x1 1.0) y0 y1 snd chn (force-display #t) show-axes):<br>" +
                       " displays 'data' as a graph with x axis label 'xlabel', axis units going from x0 to x1 and y0 to y1;<br>" +
                       " 'data' can be a list or a vct. If 'data' is a list of numbers, it is treated as an envelope.";

var extsnd_graphhook_tip = "graph-hook (snd chn y0 y1): called each time a graph is about to be updated. <br>" +
                           "If it returns #t, the display is not updated.";

var extsnd_graphonce_tip = "graph-once is the default value of the graph types (time-graph-type and transform-graph-type).";

var extsnd_graphshorizontal_tip = "(graphs-horizontal :optional snd chn):<br>" +
                                  " #t if the time domain, fft, and lisp graphs are layed out horizontally";

var extsnd_graphstyle_tip = "(graph-style :optional snd chn): graph style, <br>" +
                            " one of <code>graph-lines graph-dots graph-dots-and-lines graph-lollipops graph-filled</code>";

var extsnd_headertype_tip = "(header-type :optional snd): snd's header type (e.g. <code>mus-aiff</code>)";

var extsnd_helpdialog_tip = "(help-dialog subject message xrefs urls): start the Help window with subject and message";

var extsnd_hidewidget_tip = "(hide-widget widget): hide or undisplay widget";

var extsnd_highlightcolor_tip = "(highlight-color): color of highlighted text or buttons";

var extsnd_infodialog_tip = "(info-dialog subject message): start the Info window with subject and message";

var extsnd_initialgraphhook_tip = "initial-graph-hook (snd chn dur):<br>" +
                                  " called when a sound is displayed for the first time";

var extsnd_insertregion_tip = "(insert-region :optional (start-samp 0) (region-id 0) snd chn):<br>" +
                              " insert region data into snd's channel chn starting at start-samp";

var extsnd_insertsamples_tip = "(insert-samples start-samp samps data :optional snd chn edpos auto-delete origin):<br>" +
                               " insert data (either a vct, a list of samples, or a filename) into snd's channel chn <br>" +
                               " starting at 'start-samp' for 'samps' samples";

var extsnd_insertselection_tip = "(insert-selection :optional (beg 0) snd chn):<br>" +
                                 " insert the currently selected portion starting at beg";

var extsnd_insertsound_tip = "(insert-sound file :optional (beg 0) (file-chan 0) snd chn edpos auto-delete):<br>" +
                             " insert channel file-chan of file (or all chans if file-chan is not given)<br>" +
                             " into snd's channel chn at beg or at the cursor position.<br>" +
                             "<code>  (insert-sound \"oboe.snd\" 1000)</code><br>" +
                             " inserts all of oboe.snd starting at sample 1000.";

var extsnd_justsounds_tip = "(just-sounds): the 'just sounds' choice in the file chooser dialog";

var extsnd_leftsample_tip = "(left-sample :optional snd chn): left sample number in time domain window";

var extsnd_lispgraph_tip = "the lisp-graph is the 3rd graph displayed in the channel graphs.";

var extsnd_lispgraphhook_tip = "lisp-graph-hook (snd chn): called just before the lisp graph is updated.<br>" +
                               " If it returns a list of pixels, these are used in order by the list of graphs<br>" +
                               " (if any), rather than Snd's default set; this makes it possible to use different<br>" +
                               " colors for the various graphs. If it returns a function (of no arguments),<br>" +
                               " that function is called rather than the standard graph routine.";

var extsnd_listenerfont_tip = "(listener-font): font used by the lisp listener";

var extsnd_listenerprompt_tip = "(listener-prompt): the current lisp listener prompt character ('>') ";

var extsnd_listenertextcolor_tip = "(listener-text-color): text color in the lisp listener";

var extsnd_listtovct_tip = "(list->vct lst): returns a new vct filled with elements of list lst";

var extsnd_mainwidgets_tip = "(main-widgets): top level widgets<br>" +
                             " <code>(list (0)main-app (1)main-shell (2)main-pane (3)sound-pane (4)listener-pane (5)notebook-outer-pane)</code>";

var extsnd_makecolor_tip = "(make-color r g b): return a color object with the indicated rgb values";

var extsnd_makegraphdata_tip = "(make-graph-data :optional snd chn edpos low high):<br>" +
                               " return either a vct (if the graph has one trace), or a list of two vcts<br>" +
                               " (the two sides of the envelope graph).<br>" +
                               " 'edpos' defaults to the current-edit-position,<br>" +
                               " 'low' defaults to the current window left sample, and<br>" +
                               " 'high' defaults to the current rightmost sample.<br>" +
                               " <code>(graph-data (make-graph-data))</code> reimplements the time domain graph.";

var extsnd_makemixsamplereader_tip = "(make-mix-sample-reader id :optional (beg 0)):<br>" +
                                     " return a reader ready to access mix id";

var extsnd_makesamplereader_tip = "(make-sample-reader :optional (start-samp 0) snd chn (dir 1) edpos):<br>" +
                                  " return a reader ready to access snd's channel chn's data starting at start-samp,<br>" +
                                  " going in direction dir (1 = forward, -1 = backward),<br>" +
                                  " reading the version of the data indicated by edpos which defaults to the current version.<br>" +
                                  " snd can be a filename, or a sound index number.";

var extsnd_makesounddata_tip = "(make-sound-data chans frames): return a new sound-data object<br>" +
                               " with 'chans' channels, each having 'frames' samples";

var extsnd_makevct_tip = "(make-vct len :optional (initial-element 0)): <br>" +
                         " returns a new vct of length len filled with initial-element:<br>" +
                         "<code>  (define v (make-vct 32 1.0))</code>";

var extsnd_mapchannel_tip = "(map-channel func :optional (start 0) (dur len) snd chn edpos edname):<br>" +
                            " apply func to samples in current channel;<br>" +
                            " edname is the edit history name for this editing operation.<br>" +
                            "<code>  (map-channel (lambda (y) (* y 2.0)))</code>";

var extsnd_markclickhook_tip = "mark-click-hook (id): called when a mark is clicked;<br>" +
                               " return #t to squelch the default message.";

var extsnd_markdraghook_tip = "mark-drag-hook (id): called when a mark is dragged";

var extsnd_markhome_tip = "(mark-home :optional id): the sound (index) and channel that hold mark id";

var extsnd_markname_tip = "(mark-name :optional id snd chn): mark's name";

var extsnd_marksample_tip = "(mark-sample :optional id pos): mark's location (sample number) at edit history pos";

var extsnd_marksync_tip = "(mark-sync :optional id): mark's sync value (default: 0)";

var extsnd_marksyncmax_tip = "(mark-sync-max): max mark sync value seen so far";

var extsnd_maxamp_tip = "(maxamp :optional snd chn edpos): maxamp of data in snd's channel chn";

var extsnd_mix_tip = "(mix file :optional (beg 0) (file-chan 0) snd chn (with-tag with-mix-tags) auto-delete):<br>" +
                     " mix channel file-chan of file into snd's channel chn starting at beg (in the output),<br>" +
                     " returning the new mix's id.  if with-tag is #f, no draggable tag is created. <br>" +
                     " If auto-delete is #t, the input file is deleted when it is no longer needed.";

var extsnd_mixamp_tip = "(mix-amp id): mix's scaler";

var extsnd_mixcolor_tip = "(mix-color :optional mix-id): color of all mix tags<br>" +
                          " (if mix-id is omitted), or of mix-id's tag";

var extsnd_mixposition_tip = "(mix-position id): mix's begin time in the output in samples";

var extsnd_mixregion_tip = "(mix-region :optional (chn-samp 0) (region 0) snd chn (region-chan #t)):<br>" +
                           " mix region's channel region-chan (or all chans if region-chan is #t)<br>" +
                           " into snd's channel chn starting at chn-samp; return new mix id, if any.";

var extsnd_mixreleasehook_tip = "mix-release-hook (mix-id samps):<br>" +
                                " called after the mouse has dragged a mix to some new position.<br>" +
                                " 'samps' = samples moved in the course of the drag.<br>" +
                                " If the hook returns #t, the actual remix is the hook's responsibility.";

var extsnd_mixsamplereaderQ_tip = "(mix-sample-reader? obj): #t if obj is a mix-sample-reader";

var extsnd_mixselection_tip = "(mix-selection :optional (beg 0) snd chn (selection-channel #t)):<br>" +
                              " mix the currently selected portion starting at beg";

var extsnd_mixsync_tip = "(mix-sync id): mix sync field (an integer)";

var extsnd_mixsyncmax_tip = "(mix-sync-max): max mix sync value seen so far";

var extsnd_mixtagy_tip = "(mix-tag-y id): height of mix's tag";

var extsnd_musaudioclose_tip = "(mus-audio-close line): close the audio hardware line";

var extsnd_musaudiomixerread_tip = "(mus-audio-mixer-read device field channel vals):<br>" +
                                   " read some portion of the sound card mixer state.<br>" +
                                   " The device is the nominal audio device, normally <code>mus-audio-default</code>.<br>" +
                                   " The field describes what info we are requesting:<br>" +
                                   " to get the device's max available chans, use <code>mus-audio-channel</code>.<br>" +
                                   " The channel arg, when relevant, specifies which channel we want info on.<br>" +
                                   " The requested info is written into 'vals':<br>" +
                                   "<pre>  (let ((vals (make-vct 32)))<br><br>" +
                                   "    (mus-audio-mixer-read mus-audio-default mus-audio-format 32 vals))</pre><br><br>" +
                                   " sets <code>(vct-ref vals 0)</code> to the default device's desired audio sample data format.";

var extsnd_musaudiomixerwrite_tip = "(mus-audio-mixer-write device field channel vals):<br>" +
                                    " change some portion of the sound card mixer state";

var extsnd_musaudioopenoutput_tip = "(mus-audio-open-output device srate chans format bytes):<br>" +
                                    " open the audio device ready for output at the given srate and so on;<br>" +
                                    " return the audio line number:<br>" +
                                    "<code>  (mus-audio-open-output mus-audio-default 22050 1 mus-lshort 256)</code>";

var extsnd_musaudiowrite_tip = "(mus-audio-write line sdata frames):<br>" +
                               " write frames of data (channels * frames = samples) to the audio line from sound-data sdata.";

var extsnd_musbfloat_tip = "mus-bfloat data is big-endian float";

var extsnd_musbshort_tip = "mus-bshort data is big-endian signed 16-bit integer";

var extsnd_musdataformatname_tip = "(mus-data-format-name format): data format (e.g. mus-bshort) as a string";

var extsnd_musfileprescaler_tip = "sometimes sound files sample values are so small that they need<br>" +
                                  "to be boosted before Snd uses them.";

var extsnd_musheadertypename_tip = "(mus-header-type-name type): header type (e.g. mus-aiff) as a string";

var extsnd_muslfloat_tip = "mus-lfloat data is little-endian float";

var extsnd_muslshort_tip = "mus-lshort data is little-endian signed 16-bit integer";

var extsnd_musraw_tip = "mus-raw means 'no header'; see header-type.";

var extsnd_musosssetbuffers_tip = "(mus-oss-set-buffers num size): set Linux OSS 'fragment' number and size.<br>" +
                                  " If Snd's controls seem sluggish, try <code>(mus-oss-set-buffers 4 12)</code><br>" +
                                  " or even <code>(mus-oss-set-buffers 2 12)</code>.<br>" +
                                  " This reduces the on-card buffering, but may introduce clicks.";

var extsnd_mussoundchans_tip = "(mus-sound-chans filename): channels of data in sound file";

var extsnd_mussoundcloseinput_tip = "(mus-sound-close-input fd): close (low-level) file fd that was opened by mus-sound-open-input.";

var extsnd_mussoundcomment_tip = "(mus-sound-comment filename): comment (a string) found in sound file's header";

var extsnd_mussounddataformat_tip = "(mus-sound-data-format filename): data format (e.g. mus-bshort) of data in sound file";

var extsnd_mussoundduration_tip = "(mus-sound-duration filename): duration (in seconds) of sound file";

var extsnd_mussoundframes_tip = "(mus-sound-frames filename): frames (samples / channel) in sound file";

var extsnd_mussoundheadertype_tip = "(mus-sound-header-type filename): header type (e.g. mus-aifc) of sound file";

var extsnd_mussoundloopinfo_tip = "(mus-sound-loop-info filename): synth loop info for sound as a list:<br>" +
                                  "<code> (start1 end1 start2 end2 base-note base-detune mode1 mode2)</code>";

var extsnd_mussoundmaxamp_tip = "(mus-sound-maxamp filename): maxamps in sound<br>" +
                                " (a list of paired amps (as floats) and locations (in samples))";

var extsnd_mussoundmaxampexists_tip = "(mus-sound-maxamp-exists? filename): #t if sound's maxamp data is available;<br>" +
                                      " if it isn't, a call on mus-sound-maxamp has to open and read the data to get the maxamp.";

var extsnd_mussoundopeninput_tip = "(mus-sound-open-input filename): open filename for (low-level) sound input,<br>" +
                                   " return file descriptor (an integer)";

var extsnd_mussoundread_tip = "(mus-sound-read fd beg end chans sdata): read sound data from file fd,<br>" +
                              " filling sound-data sdata's buffers starting at beg (buffer location), going to end";

var extsnd_mussoundsamples_tip = "(mus-sound-samples filename): samples (frames * channels) in sound file";

var extsnd_mussoundsrate_tip = "(mus-sound-srate filename): sampling rate of sound file";

var extsnd_nameclickhook_tip = "name-click-hook (snd): called when sound name clicked.<br>" +
                               " If it returns #t, the usual informative minibuffer babbling is squelched.";

var extsnd_newsound_tip = "(new-sound :file :header-type :data-format :srate :channels :comment :size):<br>" +
                          " creates a new sound file with the indicated attributes; if any are omitted,<br>" +
                          " the corresponding default-output variable is used. <br>" +
                          " The 'size' argument sets the number of samples (zeros) in the newly created sound.<br>" +
                          "<code>  (new-sound \"test.snd\" mus-next mus-bshort 22050 1 \"no comment\" 1000)</code>";

var extsnd_nextsample_tip = "(next-sample reader): next sample from reader";

var extsnd_normalizefft_tip = "(transform-normalization :optional snd chn):<br>" +
                              " decides whether spectral data is normalized before display;<br>" +
                              " can be dont-normalize, normalize-by-channel (default), normalize-by-sound, or normalize-globally.";

var extsnd_openfiledialog_tip = "(open-file-dialog :optional (managed #t)):<br>" +
                                " create the file dialog if needed and display it if 'managed'";

var extsnd_openhook_tip = "open-hook (filename): called each time a file is opened<br>" +
                          " (before the actual open). If it returns #t, the file is not opened.";

var extsnd_openrawsoundhook_tip = "open-raw-sound-hook (filename current-choices):<br>" +
                                  " called when a headerless sound file is opened.<br>" +
                                  " Its result can be a list describing the raw file's attributes <br>" +
                                  " (thereby bypassing the Raw File Dialog and so on).<br>" +
                                  " The list (passed to subsequent hook functions as 'current-choice')<br>" +
                                  " is interpreted as <code>(list chans srate data-format data-location data-length)</code><br>" +
                                  " where trailing elements can be omitted (location defaults to 0,<br>" +
                                  " and length defaults to the file length in bytes).";

var extsnd_opensound_tip = "(open-sound filename): open filename <br>" +
                           " (as if opened from File:Open menu option), and return the new sound's index";

var extsnd_optimization_tip = "(optimization): the current 'run' optimization level<br>" +
                              " (default 0 = off, max is 6)";

var extsnd_optimizationhook_tip = "optimization-hook (msg): called if the run macro encounters something it can't optimize.<br>" +
                                  " 'msg' is a string description of the offending form:<br>" +
                                  "<code>  (add-hook! optimization-hook (lambda (msg) (snd-print msg)))</code>.<br>" +
                                  "  You can often slightly rewrite the form to make run happy.";

var extsnd_padchannel_tip = "(pad-channel beg dur :optional snd chn edpos): insert dur zeros at beg in snd's chn";

var extsnd_peaksfont_tip = "(peaks-font): normal font used by fft peak display";

var extsnd_play_tip = "(play :optional (start 0) snd chn sync end (pos -1) stop-proc out-chan):<br>" +
                      " play snd or snd's channel chn starting at start.<br>" +
                      " 'start' can also be a function or a filename:<br>" +
                      "<code>  (play \"oboe.snd\")</code>.<br>" +
                      " If 'sync' is true, all sounds syncd to snd are played.<br>" +
                      " If 'end' is not given, play plays to the end of the sound.<br>" +
                      " If 'pos' is -1 or not given, the current edit position is played.";

var extsnd_playandwait_tip = "(play-and-wait (start 0) snd chn syncd end (pos -1) stop-proc out-chan):<br>" +
                             " play snd or snd's channel chn starting at start and wait for the play to complete before returning.<br>" +
                             " 'start' can also be a function or a filename:<br>" +
                             "<code>  (play-and-wait \"oboe.snd\")</code>";

var extsnd_playhook_tip = "play-hook (samps): called each time a buffer is sent to the DAC.";

var extsnd_playregion_tip = "(play-region :optional (reg 0) wait stop-proc):<br>" +
                            " play region reg; if wait is #t, play to end before returning";

var extsnd_playselection_tip = "(play-selection :optional wait stop-proc): play the selection.<br>" +
                               " 'wait', if #t, causes play-selection to wait until the playing is finished before returning.";

var extsnd_positiontox_tip = "(position->x val :optional snd chn (ax time-graph)): x axis value corresponding to pixel val";

var extsnd_previoussample_tip = "(previous-sample reader): previous sample from reader";

var extsnd_promptinminibuffer_tip = "(prompt-in-minibuffer msg :optional callback snd raw):<br>" +
                                    " post msg in snd's minibuffer then when the user eventually responds,<br>" +
                                    " invoke the function callback, if any, with the response.<br>" +
                                    " If 'raw' is #t, the response is passed as a string to the prompt callback function;<br>" +
                                    " otherwise it is evaluated first as Scheme code.<br>" +
                                    "<code>   (prompt-in-minibuffer \"what?\" (lambda (response) (snd-print response)))</code>";

var extsnd_ptreechannel_tip = "(ptree-channel proc :optional (beg 0) (dur len) snd chn edpos peak-env-also init-func origin):<br>" +
                              " apply 'proc' as a 'virtual edit';<br>" +
                              " that is, the effect of 'proc' (a function of one argument, the current sample, if init-func is not specified),<br>" +
                              " comes about as an implicit change in the way the data is read.  This is similar to scaling and some envelope<br>" +
                              " operations in that no data actually changes.  If 'peak-env-also' is #t, the same function is applied to the peak<br>" +
                              " env values to get the new version. If 'proc' needs some state, it can be supplied in a vct returned by 'init-func'.<br>" +
                              " 'init-func' is a function of 2 or 3 args, the current fragment-relative begin position, the overall fragment duration,<br>" +
                              " and optionally the read direction. In this case, 'proc' is a function of 3 args: the current sample, the vct<br>" +
                              " returned by 'init-func', and the current read direction.";

var extsnd_readmixsample_tip = "(read-mix-sample reader): read sample from mix reader";

var extsnd_readonly_tip = "(read-only :optional snd): whether snd is write-protected";

var extsnd_readsample_tip = "(read-sample reader): get the next sample from the sample-reader";

var extsnd_redo_tip = "(redo :optional (count 1) snd chn): redo 'count' edits in snd's channel chn";

var extsnd_regionchans_tip = "(region-chans :optional (reg 0): region channels";

var extsnd_regionframes_tip = "(region-frames :optional (reg 0) (chan 0)): region length in frames";

var extsnd_regionok_tip = "(region? reg): #t if region is active";

var extsnd_regularizedargs_tip = "The \"regularized\" functions take arguments in the order<br>" +
                                 " begin time, duration (not end sample), sound index, channel number, and edit position.";

var extsnd_reportinminibuffer_tip = "(report-in-minibuffer msg :optional snd as-error):<br>" +
                                    " display msg in snd's minibuffer.<br>" +
                                    " If 'as-error' is #t, place the message in the minibuffer's error label.";

var extsnd_resetlistenercursor_tip = "(reset-listener-cursor): reset listener cursor to the default pointer";

var extsnd_restorecontrols_tip = "(restore-controls :optional snd): restore the previously saved control panel settings";

var extsnd_reversesound_tip = "(reverse-sound :optional snd chn edpos): reverse snd's channel chn";

var extsnd_revertsound_tip = "(revert-sound :optional snd): return 'snd' to its unedited state (undo all edits).";

var extsnd_rightsample_tip = "(right-sample :optional snd chn): right sample number in time domain window";

var extsnd_run_tip = "(run thunk): try to optimize the procedure passed as its argument,<br>" +
                     " then evaluate it; if the optimizer can't handle something in the procedure,<br>" +
                     " it is passed to Scheme and is equivalent to (thunk).";

var extsnd_sample_tip = "(sample samp :optional snd chn edpos):<br>" +
                        " return sample samp in snd's channel chn<br>" +
                        " (this is a slow access -- use sample-readers for speed)";

var extsnd_samplereaderatendQ_tip = "(sample-reader-at-end? obj): #t if sample-reader has reached the end of its data";

var extsnd_samplereaderposition_tip = "(sample-reader-position obj): current (sample-wise) location of sample-reader";

var extsnd_samples_tip = "(samples :optional (start-samp 0) (samps len) snd chn edpos):<br>" +
                         " return a vct containing snd channel chn's samples starting a start-samp for samps samples;<br>" +
                         " edpos is the edit history position to read (defaults to current position).";

var extsnd_savedir_tip = "(save-dir): name of directory for saved state data (or #f=null)";

var extsnd_savehook_tip = "save-hook (snd name): called each time a file is about to be saved.<br>" +
                          " If it returns #t, the file is not saved.<br>" +
                          " 'name' is #f unless the file is being saved under a new name (as in sound-save-as).";

var extsnd_savesound_tip = "(save-sound :optional snd): save snd<br>" +
                           " (update the on-disk data to match Snd's current version)";

var extsnd_savesoundas_tip = "(save-sound-as :file :sound :header-type :data-format :srate :channel :edit-position :comment):<br>" +
                             " save sound in file using the indicated attributes.<br>" +
                             " If channel is specified, only that channel is saved (extracted).<br>" +
                             " Omitted arguments take their value from the sound being saved.<br>" +
                             "<code>   (save-sound-as \"test.snd\" index mus-next mus-bshort)</code>";

var extsnd_savestatehook_tip = "save-state-hook (temp-filename): called each time the save-state<br>" +
                               " mechanism is about to create a new temporary file to save some edit history<br>" +
                               " sample values. temp-filename is the current file.<br>" +
                               " If the hook returns a string, it is treated as the new temp filename.<br>" +
                               " This hook provides a way to keep track of which files are in a given<br>" +
                               " saved state batch, and a way to rename or redirect those files.";

var extsnd_scaleby_tip = "(scale-by scalers :optional snd chn): scale snd by scalers (following sync);<br>" +
                         " scalers can be a float or a vct/list of floats";

var extsnd_scalechannel_tip = "(scale-channel scaler :optional (beg 0) (dur len) snd chn edpos):<br>" +
                              " scale samples in the given sound/channel between beg and beg + num by scaler.";

var extsnd_scaleselectionby_tip = "(scale-selection-by scalers): scale selected portion by scalers";

var extsnd_scaleto_tip = "(scale-to :optional (norms 1.0) snd chn): normalize snd to norms (following sync);<br>" +
                         " norms can be a float or a vct/list of floats";

var extsnd_scanchannel_tip = "(scan-channel func :optional (start 0) (dur len) snd chn edpos):<br>" +
                             " apply func to samples in current channel (or the specified channel).<br>" +
                             " func is a function of one argument, the current sample.<br>" +
                             " if func returns non-#f, the scan stops, and the value is returned to the caller<br>" +
                             " with the sample number.<br>" +
                             "<code>   (scan-channel (lambda (y) (> y .1)))</code>";

var extsnd_scriptarg_tip = "(script-arg): where we are in the startup arg list";

var extsnd_scriptargs_tip = "(script-args): the args passed to Snd at startup as a list of strings";

var extsnd_searchprocedure_tip = "(search-procedure :optional snd): global search function<br>" +
                                 " (if no 'snd' specified) or sound-local search function";

var extsnd_selectall_tip = "(select-all :optional snd chn): make a new selection containing all of snd's channel chn.<br>" +
                           " If sync is set, all chans are included. <br>" +
                           " The new region id is returned (if selection-creates-region is #t).";

var extsnd_selectedchannel_tip = "(selected-channel :optional snd): currently selected channel in snd (or #f if none)";

var extsnd_selecteddatacolor_tip = "(selected-data-color): color used for selected data";

var extsnd_selectedgraphcolor_tip = "(selected-graph-color): background color of selected data";

var extsnd_selectedsound_tip = "(selected-sound): index of currently selected sound (or #f if none)";

var extsnd_selectioncreatesregion_tip = "(selection-creates-region): #t if a region should be created each time a selection is made.<br>" +
                                        " The default is currently #t, but that may change.<br>" +
                                        " If you're dealing with large selections, and have no need of regions (saved selections),<br>" +
                                        " you can speed up many operations by setting this flag to #f";

var extsnd_selectionframes_tip = "(selection-frames :optional snd chn): selection length";

var extsnd_selectionmember_tip = "(selection-member? :optional snd chn): #t if snd's channel chn is a member of the current selection";

var extsnd_selectionok_tip = "(selection?): #t if selection is currently active, visible, etc";

var extsnd_selectionposition_tip = "(selection-position :optional snd chn): selection start samp";

var extsnd_selectionsrate_tip = "(selection-srate): selection srate";

var extsnd_setsamples_tip = "(set-samples start-samp samps data :optional snd chn truncate edname (infile-chan 0) edpos auto-delete):<br>" +
                            " set snd's channel chn's samples starting at start-samp for samps from data (a vct, vector, or string (filename));<br>" +
                            " start-samp can be beyond current data end;<br>" +
                            " if truncate is #t and start-samp is 0, the end of the file is set to match the new data's end.";

var extsnd_shortfilename_tip = "(short-file-name :optional snd): short form of snd's file name (no directory)";

var extsnd_showcontrols_tip = "(show-controls :optional snd): #t if snd's control panel is known to be open";

var extsnd_showindices_tip = "(show-indices): #t if sound name should be preceded by its index in the sound display.";

var extsnd_showlistener_tip = "(show-listener :optional (open #t)): if 'open' opens the lisp listener;<br>" +
                              " returns whether the listener is visible.";

var extsnd_showtransformpeaks_tip = "(show-transform-peaks :optional snd chn): #t if fft display should include peak list";

var extsnd_smoothsound_tip = "(smooth-sound :optional (start-samp 0) (samps len) snd chn): smooth data from start-samp for samps in snd's channel chn";

var extsnd_sndhelp_tip = "(snd-help :optional (arg 'snd-help) (formatted #t)): return the documentation associated with its argument.<br>" +
                         "<code> (snd-help 'make-vct)</code> for example, prints out a brief description of make-vct.<br>" +
                         " The argument can be a string, symbol, or in some cases, the object itself.<br>" +
                         " In the help descriptions, optional arguments are in parens with the default value (if any) as the 2nd entry.<br>" +
                         " A ':' as the start of the argument name marks a CLM-style optional keyword argument. <br>" +
                         " If you load index.scm the functions html and ? can be used in place of help to go to the HTML description,<br>" +
                         " and the location of the associated C code will be displayed, if it can be found.<br>" +
                         " If help-hook is not empty, it is invoked with the subject and the snd-help result and its value is returned.";

var extsnd_sndprint_tip = "(snd-print str): display str in the listener window";

var extsnd_sndspectrum_tip = "(snd-spectrum data :optional (window rectangular-window) (len data-len)<br>" +
                             " (linear #t) (beta 0.0) in-place (normalized #t)):<br>" +
                             " magnitude spectrum of data (a vct), in data if in-place, using fft-window win and fft length len.";

var extsnd_sndtempnam_tip = "(snd-tempnam): return a new temp file name using temp-dir.";

var extsnd_sounddatalength_tip = "(sound-data-length sd): length (in samples) of each channel of sound-data sd";

var extsnd_sounddataref_tip = "(sound-data-ref sd chan i): sample in channel chan at location i of sound-data sd:<br>" +
                              " sd[chan][i]";

var extsnd_sounddataset_tip = "(sound-data-set! sd chan i val): set sound-data sd's i-th element in channel chan to val:<br>" +
                              " sd[chan][i] = val";

var extsnd_sounddata_times_tip = "(sound-data* val1 val2): multiply val1 by val2 (either or both can be a sound-data object).";

var extsnd_soundfilep_tip = "(sound-file? name): #t if name has a known sound file extension";

var extsnd_soundfilesindirectory_tip = "(sound-files-in-directory :optional (directory \".\")):<br>" +
                                       " return a list of the sound files in 'directory'";

var extsnd_soundp_tip = "(sound? :optional (index 0)): #t if sound associated with 'index' is active (accessible)";

var extsnd_sounds_tip = "(sounds): list of active sounds (a list of indices)";

var extsnd_speedcontrol_tip = "(speed-control :optional snd): current speed (srate) slider setting";

var extsnd_squelchupdate_tip = "(squelch-update :optional snd chn): #t if updates (redisplays) are turned off in snd's channel chn";

var extsnd_srate_tip = "(srate :optional snd): snd's srate";

var extsnd_srcchannel_tip = "(src-channel ratio-or-env :optional (beg 0) (dur len) snd chn edpos):<br>" +
                            " sampling-rate convert snd's channel chn by ratio, or following an envelope <br>" +
                            " (a list or a CLM env generator).";

var extsnd_srcsound_tip = "(src-sound ratio-or-env :optional (base 1.0) snd chn edpos):<br>" +
                          " sampling-rate convert snd's channel chn by ratio, or following an envelope.<br>" +
                          " A negative ratio reverses the sound";

var extsnd_startplayinghook_tip = "start-playing-hook (snd): called when a play request is triggered.<br>" +
                                  " If it returns #t, the sound is not played.";

var extsnd_startplayingselectionhook_tip = "start-playing-selection-hook (): called when the selection starts playing";

var extsnd_stopdachook_tip = "stop-dac-hook (): called upon mus_audio_close (when DAC is turned off)";

var extsnd_stopplaying_tip = "(stop-playing :optional snd): stop play (DAC output) in progress";

var extsnd_stopplayinghook_tip = "stop-playing-hook (snd): called when a sound finishes playing.";

var extsnd_stopplayingselectionhook_tip = "stop-playing-selection-hook (): called when the selection stops playing";

var extsnd_sync_tip = "(sync :optional snd): snd's sync value (0 = no sync).<br>" +
                      "  Some editing operations are applied to all sounds sharing the sync value of the selected sound.";

var extsnd_tempdir_tip = "(temp-dir): name of directory for temp files (or #f=null)";

var extsnd_timegraphtype_tip = "(time-graph-type :optional snd chn): graph-as-wavogram if<br>" +
                               " Snd's time domain display is a 'wavogram',otherwise graph-once.";

var extsnd_tinyfont_tip = "(tiny-font): font use for some info in the graphs";

var extsnd_transformgraphp_tip = "(transform-graph? :optional snd chn): #t if fft display is active in snd's channel chn";

var extsnd_transformgraphtype_tip = "(transform-graph-type :optional snd chn) can be<br>" +
                                    " graph-once, graph-as-sonogram, or graph-as-spectrogram.";

var extsnd_transformsize_tip = "(transform-size :optional snd chn): current fft size (512)";

var extsnd_transformtovct_tip = "(transform->vct :optional snd chn obj): return a vct (obj if it's passed),<br>" +
                                " with the current transform data from snd's channel chn";

var extsnd_undo_tip = "(undo :optional (count 1) snd chn): undo 'count' edits in snd's channel chn";

var extsnd_updatesound_tip = "(update-sound :optional snd): update snd (re-read it from the disk after flushing pending edits)";

var extsnd_updatetimegraph_tip = "(update-time-graph :optional snd chn): redraw snd channel chn's graphs";

var extsnd_updatetransformgraph_tip = "(update-transform-graph :optional snd chn): redraw snd channel chn's fft display";

var extsnd_vct_tip = "(vct args...): returns a new vct with args as contents; same as list->vct: (vct 1 2 3)";

var extsnd_vctadd_tip = "(vct-add! v1 v2 :optional (offset 0)): element-wise add of vcts v1 and v2: v1[i + offset] += v2[i], returns v1";

var extsnd_vctcopy_tip = "(vct-copy v): returns a copy of vct v";

var extsnd_vctfill_tip = "(vct-fill! v val): set each element of v to val: v[i] = val, returns v";

var extsnd_vctlength_tip = "(vct-length v): length of vct v";

var extsnd_vctmap_tip = "(vct-map! v proc): set each element of v to value of proc (a thunk):<br>" +
                        " v[i] = (proc), returns v.<br>" +
                        "<code>  (vct-map! v (lambda () 3.0))</code> is the same as <code>(vct-fill! v 3.0)</code>";

var extsnd_vctmove_tip = "(vct-move! obj new old :optional backwards): moves vct obj data from old to new:<br>" +
                         " v[new++] = v[old++], or v[new--] = v[old--] if backwards is #f.";

var extsnd_vctmultiply_tip = "(vct-multiply! v1 v2): element-wise multiply of vcts v1 and v2:<br>" +
                             " v1[i] *= v2[i], returns v1";

var extsnd_vctoffset_tip = "(vct-offset! v val): add val to each element of v:<br>" +
                           " v[i] += val, returns v";

var extsnd_vctp_tip = "(vct? obj): is obj a vct";

var extsnd_vctpeak_tip = "(vct-peak v): max of abs of elements of v";

var extsnd_vctref_tip = "(vct-ref v n): element n of vct v, v[n]";

var extsnd_vctreverse_tip = "(vct-reverse! vct len): in-place reversal of vct contents";

var extsnd_vctscale_tip = "(vct-scale! v val): scale each element of v by val:<br>" +
                          " v[i] *= val, returns v";

var extsnd_vctset_tip = "(vct-set! v n val): sets element of vct v to val, v[n] = val";

var extsnd_vctsubseq_tip = "(vct-subseq v start :optional end len vnew): v[start..end],<br>" +
                           " placed in vnew if given or new vct";

var extsnd_vcttochannel_tip = "(vct->channel vct :optional (beg 0) (dur len) snd chn edpos origin):<br>" +
                              " set snd's channel chn's samples starting at beg for dur samps from vct data";

var extsnd_vcttosounddata_tip = "(vct->sound-data v sd chan): copies vct v's data into sound-data sd's channel chan";

var extsnd_widgetposition_tip = "(widget-position wid): widget's position, (list x y), in pixels";

var extsnd_widgetsize_tip = "(widget-size wid): widget's size, (list width height), in pixels";

var extsnd_windowheight_tip = "(window-height): current Snd window height in pixels";

var extsnd_windowproperty_tip = "(window-property win-name name): get or set the window property.";

var extsnd_windowwidth_tip = "(window-width): current Snd window width in pixels";

var extsnd_withmixtags_tip = "(with-mix-tags): #t if Snd should try to use virtual (tagged) mixing";

var extsnd_withtrackingcursor_tip = "(with-tracking-cursor :optional snd): #t if cursor moves along in waveform display as sound is played";

var extsnd_xaxislabel_tip = "(x-axis-label :optional snd chn (ax time-graph)): current x axis label";

var extsnd_xaxisstyle_tip = "(x-axis-style :optional snd chn): The x axis labelling of the time domain waveform<br>" +
                            " can be in seconds (x-axis-in-seconds), in samples (x-axis-in-samples),<br>" +
                            " expressed as a percentage of the overall duration (x-axis-as-percentage),<br>" +
                            " as a beat number (x-axis-in-beats), as a measure number (x-axis-in-measures),<br>" +
                            " or clock-style (dd:hh:mm:ss) (x-axis-as-clock).";

var extsnd_xbounds_tip = "(x-bounds :optional snd chn): a list (x0 x1) giving the current x axis bounds of snd channel chn";

var extsnd_xtoposition_tip = "(x->position val :optional snd chn (ax time-graph)): x pixel loc of val";

var extsnd_xzoomslider_tip = "(x-zoom-slider :optional snd chn): current x axis zoom slider of snd channel chn";

var extsnd_ybounds_tip = "(y-bounds :optional snd chn): a list (y0 y1) giving the current y axis bounds of snd channel chn";

var extsnd_ytoposition_tip = "(y->position val :optional snd chn (ax time-graph)): y pixel loc of val";

var extsnd_yzoomslider_tip = "(y-zoom-slider :optional snd chn): current y axis zoom slider of snd channel chn";

var sndclm_amplitude_modulate_tip = "(amplitude-modulate carrier in1 in2): in1 * (carrier + in2)";

var sndclm_array_interp_tip = "(array-interp v phase :optional size): v[phase] taking into account wrap-around<br>" +
                              " (size is size of data), with linear interpolation if phase is not an integer.";

var sndclm_comb_tip = "(comb gen :optional (val 0.0) (pm 0.0)): comb filter val, pm changes the delay length.";

var sndclm_continue_sampletofile_tip = "(continue-sample->file filename): return an output generator<br>" +
                                       " that reopens an existing sound file 'filename' ready for output via sample->file";

var sndclm_contrast_enhancement_tip = "(contrast-enhancement sig (index 1.0)): sin(sig * pi / 2 + index * sin(sig * 2 * pi))";

var sndclm_convolve_tip = "(convolve gen :optional input-func): next sample from convolution generator";

var sndclm_delay_tip = "(delay gen :optional (val 0.0) (pm 0.0)): delay val<br>" +
                       " according to the delay line's length and pm ('phase-modulation').<br>" +
                       " If pm is greater than 0.0, the max-size argument used to create gen<br>" +
                       " should have accommodated its maximum value.";

var sndclm_dot_product_tip = "(dot-product v1 v2 :optional size): sum of (vcts) v1[i] * v2[i] (also named scalar product)";

var sndclm_env_tip = "(env gen): next sample from envelope generator";

var sndclm_fft_tip = "(mus-fft rl im :optional len (dir 1)):<br>" +
                     " return the fft of vcts rl and im which contain <br>" +
                     " the real and imaginary parts of the data;<br>" +
                     " len should be a power of 2,<br>" +
                     " dir = 1 for fft, -1 for inverse-fft";

var sndclm_filetoarray_tip = "(file->array filename chan start samples data):<br>" +
                             " read the sound file 'filename' placing samples from channel 'chan'<br>" +
                             " into the vct 'data' starting in the file at frame 'start'<br>" +
                             " and reading 'samples' samples altogether.";

var sndclm_filetosample_tip = "(file->sample obj frame chan): sample value in sound file read by 'obj' in channel chan at frame";

var sndclm_filter_tip = "(filter gen :optional (input 0.0)): next sample from filter";

var sndclm_fir_filter_tip = "(fir-filter gen :optional (input 0.0)): next sample from FIR filter";

var sndclm_formant_tip = "(formant gen :optional (input 0.0) freq-in-radians): next sample from resonator generator";

var sndclm_frame_ref_tip = "(frame-ref f chan): f[chan] (the chan-th sample in frame f";

var sndclm_frame_set_tip = "(frame-set! f chan val) sets frame f's chan-th sample to val:<br>" +
                           " f[chan] = val";

var sndclm_frame_times_tip = "(frame* f1 f2 :optional outf): multiply f1 and f2 (elementwise)<br>" +
                             " returning outf; if outf is not given, a new frame is created.<br>" +
                             " outf[i] = f1[i] * f2[i].";

var sndclm_granulate_tip = "(granulate gen :optional input-func edit-func): next sample from granular synthesis generator";

var sndclm_hztoradians_tip = "(hz->radians hz): convert frequency in Hz to radians per sample: hz * 2 * pi / srate";

var sndclm_in_any_tip = "(in-any frame chan stream): input stream sample at frame in channel chan";

var sndclm_ina_tip = "(ina frame stream): input stream sample in channel 0 at frame";

var sndclm_locsig_set_tip = "(locsig-set! gen chan val): set the locsig generator's channel 'chan' scaler to 'val'";

var sndclm_locsig_tip = "(locsig gen loc val): add 'val' to the output of locsig at frame 'loc'";

var sndclm_make_comb_tip = "(make-comb :scaler :size :initial-contents (:initial-element 0.0) :max-size (:type mus-interp-linear)):<br>" +
                           " return a new comb filter (a delay line with a scaler on the feedback) of size elements.<br>" +
                           " If the comb length will be changing at run-time, max-size sets its maximum length.<br>" +
                           " initial-contents can be either a list or a vct.";

var sndclm_make_convolve_tip = "(make-convolve :input :filter :fft-size): <br>" +
                               " return a new convolution generator which convolves its input with the impulse response 'filter'.";

var sndclm_make_delay_tip = "(make-delay :size :initial-contents (:initial-element 0.0) (:max-size) (:type mus-interp-linear)):<br>" +
                            " return a new delay line of size elements.<br>" +
                            " If the delay length will be changing at run-time, max-size sets its maximum length,<br>" +
                            " so <code>(make-delay len :max-size (+ len 10))</code> provides 10 extra elements of delay<br>" +
                            " for subsequent phasing or flanging.<br>" +
                            " initial-contents can be either a list or a vct.";

var sndclm_make_env_tip = "(make-env :envelope (:scaler 1.0) :duration (:offset 0.0) (:base 1.0) :end :length):<br>" +
                          " return a new envelope generator.<br>" +
                          " 'envelope' is a list or vct of break-point pairs. To create the envelope,<br>" +
                          " these points are offset by 'offset', scaled by 'scaler', and mapped over the time interval<br>" +
                          " defined by either 'duration' (seconds) or 'length' (samples).<br>" +
                          " If 'base' is 1.0, the connecting segments are linear, if 0.0 you get a step function,<br>" +
                          " and anything else produces an exponential connecting segment.";

var sndclm_make_filetosample_tip = "(make-file->sample filename :optional buffer-size):<br>" +
                                   " return an input generator reading 'filename' (a sound file)";

var sndclm_make_filter_tip = "(make-filter :order :xcoeffs :ycoeffs):<br>" +
                             " return a new direct form FIR/IIR filter, coeff args are vcts";

var sndclm_make_fir_filter_tip = "(make-fir-filter :order :xcoeffs): return a new FIR filter, xcoeffs a vct";

var sndclm_make_formant_tip = "(make-formant :frequency :radius):<br>" +
                              " return a new formant generator (a resonator).<br>" +
                              " radius sets the pole radius (in terms of the 'unit circle').<br>" +
                              " frequency sets the resonance center frequency (Hz).";

var sndclm_make_frame_tip = "(make-frame chans val0 val1 ...):<br>" +
                            " return a new frame object with chans samples,<br>" +
                            " each sample set from the trailing arguments (defaulting to 0.0):<br>" +
                            "<code>  (make-frame 2 .1 .2)</code>";

var sndclm_make_granulate_tip = "(make-granulate :input (:expansion 1.0) (:length .15) (:scaler .6) (:hop .05) (:ramp .4) (:jitter 1.0) :max-size :edit):<br>" +
                                " return a new granular synthesis generator.<br>" +
                                " 'length' is the grain length (seconds),<br>" +
                                " 'expansion' is the ratio in timing between the new and old (expansion > 1.0 slows things down),<br>" +
                                " 'scaler' scales the grains to avoid overflows,<br>" +
                                " 'hop' is the spacing (seconds) between successive grains upon output,<br>" +
                                " 'jitter' controls the randomness in that spacing,<br>" +
                                " 'input' can be a file pointer.<br>" +
                                " 'edit' can be a function of one arg, the current granulate generator.<br>" +
                                "  It is called just before a grain is added into the output buffer.<br>" +
                                " The current grain is accessible via mus-data.<br>" +
                                " The edit function, if any, should return the length in samples of the grain, or 0.";

var sndclm_make_locsig_tip = "(make-locsig (:degree 0.0) (:distance 1.0) (:reverb 0.0) (:output *output*) (:revout *reverb*)<br>" +
                             " (:channels (mus-channels *output*)) (:type mus-interp-linear)):<br>" +
                             " return a new generator for signal placement in n channels.  Channel 0 corresponds to 0 degrees.";

var sndclm_make_moving_average_tip = "(make-moving-average :size :initial-contents (:initial-element 0.0)):<br>" +
                                     " return a new moving_average generator. initial-contents can be either a list or a vct.";

var sndclm_make_ncos_tip = "(make-ncos (:frequency *clm-default-frequency*) (:n 1)):<br>" +
                           " return a new ncos generator, producing a sum of 'n' equal amplitude cosines.";

var sndclm_make_one_pole_tip = "(make-one-pole :a0 :b1): return a new one-pole filter; a0*x(n) - b1*y(n-1)";

var sndclm_make_one_zero_tip = "(make-one-zero :a0 :a1): return a new one-zero filter;  a0*x(n) + a1*x(n-1)";

var sndclm_make_oscil_tip = "(make-oscil (:frequency *clm-default-frequency*) (:initial-phase 0.0)):<br>" +
                            " return a new oscil (sinewave) generator";

var sndclm_make_phase_vocoder_tip = "(make-phase-vocoder :input :fft-size :overlap :interp :pitch :analyze :edit :synthesize):<br>" +
                                    " return a new phase-vocoder generator;<br>" +
                                    " input is the input function (it can be set at run-time),<br>" +
                                    " analyze, edit, and synthesize are either #f or functions that replace the default innards of the generator,<br>" +
                                    " fft-size, overlap and interp set the fftsize, the amount of overlap between ffts, and the time between new analysis calls.<br>" +
                                    " 'analyze', if given, takes 2 args, the generator and the input function;<br>" +
                                    " if it returns #t, the default analysis code is also called.<br>" +
                                    "  'edit', if given, takes 1 arg, the generator; if it returns #t, the default edit code is run.<br>" +
                                    "  'synthesize' is a function of 1 arg, the generator; it is called to get the current vocoder output.";

var sndclm_make_polyshape_tip = "(make-polyshape (:frequency *clm-default-frequency*) (:initial-phase 0.0) :coeffs (:partials '(1 1)) (:kind mus-chebyshev-first-kind)):<br>" +
                                " return a new polynomial-based waveshaping generator:<br>" +
                                "<code>   (make-polyshape :coeffs (partials->polynomial '(1 1.0)))</code><br>" +
                                " is the same in effect as make-oscil";

var sndclm_make_pulse_train_tip = "(make-pulse-train (:frequency *clm-default-frequency*) (:amplitude 1.0) (:initial-phase 0.0)):<br>" +
                                  " return a new pulse-train generator.  This produces a sequence of impulses.";

var sndclm_make_rand_interp_tip = "(make-rand-interp (:frequency *clm-default-frequency*) (:amplitude 1.0) :envelope :distribution :size):<br>" +
                                  " return a new rand-interp generator, producing linearly interpolated random numbers.<br>" +
                                  " frequency is the rate at which new end-points are chosen.";

var sndclm_make_rand_tip = "(make-rand (:frequency *clm-default-frequency*) (:amplitude 1.0) :envelope :distribution :size):<br>" +
                           " return a new rand generator, producing a sequence of random numbers (a step  function).<br>" +
                           " frequency is the rate at which new numbers are chosen.";

var sndclm_make_readin_tip = "(make-readin :file (:channel 0) (:start 0) (:direction 1) :size):<br>" +
                             " return a new readin (file input) generator reading the sound file 'file'<br>" +
                             " starting at frame 'start' in channel 'channel' and reading forward if 'direction' is not -1";

var sndclm_make_sampletofile_tip = "(make-sample->file filename :optional chans data-format header-type comment):<br>" +
                                   " return an output generator writing the sound file 'filename'<br>" +
                                   " which is set up to have 'chans' channels of 'data-format' samples with a header of 'header-type'.<br>" +
                                   " The latter should be sndlib identifiers:<br>" +
                                   "<code>   (make-sample->file \"test.snd\" 2 mus-lshort mus-riff)</code>";

var sndclm_make_src_tip = "(make-src :input (:srate 1.0) (:width 10)): return a new sampling-rate conversion generator<br>" +
                          " (using 'warped sinc interpolation').<br>" +
                          " 'srate' is the ratio between the new rate and the old.<br>" +
                          " 'width' is the sine width (effectively the steepness of the low-pass filter), normally between 10 and 100.<br>" +
                          " 'input' if given is an open file stream.";

var sndclm_make_sum_of_cosines_tip = "(make-sum-of-cosines (:cosines 1) (:frequency *clm-default-frequency*) (:initial-phase 0.0)):<br>" +
                                     " return a new sum-of-cosines generator, producing a band-limited pulse train.";

var sndclm_make_triangle_wave_tip = "(make-triangle-wave (:frequency *clm-default-frequency*) (:amplitude 1.0) (:initial-phase 0.0)):<br>" +
                                    " return a new triangle-wave generator.";

var sndclm_make_two_zero_tip = "(make-two-zero :a0 :a1 :a2 or :frequency :radius):<br>" +
                               " return a new two-zero filter; a0*x(n) + a1*x(n-1) + a2*x(n-2)";

var sndclm_moving_average_tip = "(moving-average gen :optional (val 0.0)): moving window moving_average.";

var sndclm_mus_channels_tip = "(mus-channels gen): gen's mus-channels field";

var sndclm_mus_close_tip = "(mus-close gen): close the IO stream managed by 'gen' (a sample->file generator, for example)";

var sndclm_mus_data_tip = "(mus-data gen): gen's internal data (a vct)";

var sndclm_mus_frequency_tip = "(mus-frequency gen): gen's frequency (Hz)";

var sndclm_mus_increment_tip = "(mus-increment gen): gen's mus-increment field";

var sndclm_mus_length_tip = "(mus-length gen): gen's length";

var sndclm_mus_offset_tip = "(mus-offset gen): gen's offset";

var sndclm_mus_random_tip = "(mus-random val): a random number between -val and val.<br>" +
                            " the built-in 'random' function returns values between 0 and its argument";

var sndclm_mus_scaler_tip = "(mus-scaler gen): gen's scaler, if any.<br>" +
                            "  This is often an amplitude adjustment of some sort.";

var sndclm_mussrate_tip = "(mus-srate): current sampling rate";

var sndclm_ncos_tip = "(ncos gen :optional (fm 0.0)): get the next sample from 'gen', an ncos generator";

var sndclm_one_pole_tip = "(one-pole gen :optional (input 0.0)): one pole filter of input";

var sndclm_optional_key_tip = "One special aspect of each generator make function is the way it reads its arguments.<br>" +
                "I use the word <b>optional-key</b> to indicate that the arguments are keywords, but the keywords<br>" +
                "themselves are optional. Take the make-oscil call, defined as:<br><br>" +
                "<code>make-oscil :optional-key (frequency *clm-default-frequency*) (initial-phase 0.0)</code><br><br>" +
                "This says that make-oscil has two optional arguments, frequency (in Hz), and <br>" +
                "initial-phase (in radians).  The keywords associated with these values are<br>" +
                ":frequency and :initial-phase.  When make-oscil is called, it scans its arguments;<br>" +
                "if a keyword is seen, that argument and all following arguments are passed unchanged,<br>" +
                "but if a value is seen, the corresponding keyword is prepended in the argument list:<br><br>" +
                "<code>(make-oscil :frequency 440.0)<br>" +
                "(make-oscil :frequency 440.0 :initial-phase 0.0)<br>" +
                "(make-oscil 440.0)<br>" +
                "(make-oscil 440.0 :initial-phase 0.0)<br>" +
                "(make-oscil 440.0 0.0)</code>";

var sndclm_oscil_tip = "(oscil gen :optional (fm 0.0) (pm 0.0)):<br>" +
                       " next sample from oscil gen: val = sin(phase + pm); phase += (freq + fm)";

var sndclm_out_any_tip = "(out-any frame val chan stream): add val to output stream at frame in channel chan";

var sndclm_outa_tip = "(outa frame val stream): add val to output stream at frame in channel 0";

var sndclm_partialstopolynomial_tip = "(partials->polynomial partials :optional (kind mus-chebyshev-first-kind)):<br>" +
                                      " produce a Chebyshev polynomial suitable for use with the polynomial generator<br>" +
                                      " to create (via waveshaping) the harmonic spectrum described by the partials argument:<br>" +
                                      "<code>  (let ((v0 (partials->polynomial '(1 1.0 2 1.0)))<br>        (os (make-oscil)))<br>    (polynomial v0 (oscil os)))</code>";

var sndclm_phase_vocoder_tip = "(phase-vocoder gen input-function analyze-func edit-func synthesize-func): next phase vocoder value";

var sndclm_polynomial_tip = "(polynomial coeffs x): evaluate a polynomial at x.<br>" +
                            " coeffs are in order of degree, so coeff[0] is the constant term.";

var sndclm_polyshape_tip = "(polyshape gen :optional (index 1.0) (fm 0.0)): next sample of polynomial-based waveshaper";

var sndclm_pulse_train_tip = "(pulse-train gen :optional (fm 0.0)): next pulse train sample from generator";

var sndclm_rand_interp_tip = "(rand-interp gen :optional (fm 0.0)): gen's current (interpolating) random number.<br>" +
                             " fm modulates the rate at which new segment end-points are chosen.";

var sndclm_rand_tip = "(rand gen :optional (fm 0.0)): gen's current random number.<br>" +
                      " fm modulates the rate at which the current number is changed.";

var sndclm_readin_tip = "(readin gen): next sample from readin generator (a sound file reader)";

var sndclm_secondstosamples_tip = "(seconds->samples secs): use mus-srate to convert seconds to samples";

var sndclm_src_tip = "(src gen :optional (pm 0.0) input-function): next sampling rate conversion sample.<br>" +
                     " 'pm' can be used to change the sampling rate on a sample-by-sample basis.<br>" +
                     " 'input-function' is a function of one argument (the current input direction, normally ignored)<br>" +
                     " that is called internally whenever a new sample of input data is needed.<br>" +
                     " If the associated make-src included an 'input' argument, input-function is ignored.";

var sndclm_tap_tip = "(tap gen :optional (pm 0.0)): tap the delay generator offset by pm";

var sndclm_timestosamples_tip = "(times->samples beg dur) returns a list of beg and beg+dur in samples.";

var sndclm_triangle_wave_tip = "(triangle-wave gen :optional (fm 0.0)): next triangle wave sample from generator";

var sndscm_IIRfilters_tip = "These are simple 2nd order IIR filters in dsp.scm.";

var sndscm_analogfilterdoc_tip = "These are the standard 'analog' IIR filters: Butterworth, Chebyshev, etc.";

var sndscm_channelproperty_tip = "(channel-property key snd chn): returns the value associated with 'key'<br>" +
                                 " in the given channel's property list. To add or change a property,<br>" +
                                 " use set! with this procedure.<br><br>" +
                                 "<code>  (set! (channel-property 'info 0 0) \"this is sound 0, first channel\")</code><br>" +
                                 " now <code>(channel-property 'info 0 0)</code> returns \"this is sound 0, first channel\".";

var sndscm_def_clm_struct_tip = "def-clm-struct sets up a structure, an object with named slots that you can get and set,<br>" +
                                " and ties it into the optimizer so that you can use the structures without any speed penalty.<br>" +
                                " It also defines a \"make\" function to create an instance of the structure, and a predicate for it.<br><br>" +
                                "<code>    (def-clm-struct osc freq phase)</code><br><br>" +
                                " defines a struct named \"osc\" with the (float) fields freq and phase.<br>" +
                                "  it also defines make-osc which creates an osc, and osc? returns #t if passed an osc.<br>" +
                                " the struct fields are accessed via osc-freq and osc-phase.";

var sndscm_definstrument_tip = "definstrument is very much like define, but with added code to support notehook<br>" +
                               " and (for Common Music) *definstrument-hook*.";

var sndscm_envelopeinterp_tip = "(envelope-interp x env (base 1.0): returns value of 'env' at 'x'.<br>" +
                                " If 'base' is 0, 'env' is treated as a step function;<br>" +
                                " if 'base' is 1.0 (the default), the breakpoints of 'env' are connected by a straight line,<br>" +
                                " and any other 'base' connects the breakpoints with an exponential curve.";

var sndscm_envexptchannel_tip = "(env-expt-channel env exponent :optional (symmetric #t) beg dur snd chn edpos):<br>" +
                                " applies 'env' to the given channel using 'exponent' for the exponential base.<br>" +
                                " The 'symmetric' argument determines whether the up and down moving ramps look<br>" +
                                " symmetrical around a break point.";

var sndscm_exponentiallyweightedmovingaverage_tip = "exponentially-weighted-moving-average applies exponential weights<br>" +
                                                    " to a moving average (it is actually just a one-pole filter.";

var sndscm_fmviolin_tip = "The fm-violin instrument uses FM to produce a string-like sound;<br>" +
                          " It has many parameters, the principal ones being <code>startime dur frequency amplitude</code>.<br>" +
                          " The code is in v.scm.";

var sndscm_greendoc_tip = "brownian and 1/f noise from green.scm.";

var sndscm_hilberttransform_tip = "(hilbert-transform gen input) returns the Hilbert transform of 'input'.";

var sndscm_html_tip = "(html arg) where 'arg' can be a string, a symbol, or a procedure<br>" +
                      " sends the html reader to the corresponding url in the Snd documents.";

var sndscm_insertchannel_tip = "(insert-channel filedat :optional beg dur snd chn edpos):<br>" +
              " inserts the specified data ('filedat') in the given channel at the given location.<br>" +
              " 'filedat' can be either a filename (a string), a sound index, or a list containing<br>" +
              " the filename (or index), the start point in the file, and (optionally) the channel of the file to mix.";

var sndscm_makebandpass_tip = "(make-bandpass flo fhi :optional length) returns a bandpass filter.";

var sndscm_makebiquad_tip = "(make-biquad a0 a1 a2 b1 b2) returns a biquad filter section.";

var sndscm_makebutter_tip = "various 2nd order Butterworth filters in dsp.scm.";

var sndscm_makedifferentiator_tip = "(make-differentiator :optional length) returns a differentiating filter.";

var sndscm_makeframereader_tip = "(make-frame-reader start snd dir pos): creates a frame-reader<br>" +
                                 " reading the sound 'snd' starting at frame 'start'<br>" +
                                 " with initial read direction 'dir' (1=forward, -1=backward).<br>" +
                                 " 'pos' is the edit history position to read (it defaults to current position).";

var sndscm_makehighpass_tip = "(make-highpass fc :optional length) returns a highpass filter.";

var sndscm_makehilberttransform_tip = "(make-hilbert-transform :optional length) returns a Hilbert transformer.";

var sndscm_makelowpass_tip = "(make-lowpass fc :optional length) returns a lowpass filter.";

var sndscm_makeramp_tip = "(make-ramp :optional (size 128)): return a ramp generator.";

var sndscm_makeselection_tip = "(make-selection beg end snd chn): makes a selection,<br>" +
                               " like make-region but without creating a region.<br>" +
                               " It selects 'dur' samples starting at 'beg' in the given channel.";

var sndscm_makespencerfilter_tip = "(make-spencer-filter) returns an FIR filter with the Spencer (smoothing) coefficients.";

var sndscm_matchsoundfiles_tip = "(match-sound-files func :optional dir): apply 'func' to each sound file in 'dir'<br>" +
                                 " and return a list of files for which func does not return #f.";

var sndscm_maxenvelope_tip = "(max-envelope env): return the maximum y value in 'env'";

var sndscm_mixsound_tip = "(mix-sound file start): mix 'file' (all chans) into the currently selected sound at 'start'.";

var sndscm_moogfilter_tip = "(moog-filter gen input): return Moog-style 4-pole lowpass filtering of 'input'";

var sndscm_movingmax_tip = "(moving-max gen y): return moving window max given input 'y'.<br>" +
                           " moving-max is a specialization of the delay generator that produces<br>" +
                           " an envelope that tracks the peak amplitude of the last 'size' samples.";

var sndscm_movingsum_tip = "(moving-sum gen y): return moving window sum given input 'y'.";

var sndscm_mpg_tip = "(mpg mpgfile rawfile): call mpg123 to translate an MPEG format sound file<br>" +
                     " to a headerless (\"raw\") file containing 16-bit samples.";

var sndscm_musmix_tip = "(mus-mix outfile infile :optional (outloc 0) (frames) (inloc 0) mixer envs):<br>" +
                        " mix 'infile' into 'outfile' starting at 'outloc' in 'outfile'<br>" +
                        " and 'inloc' in 'infile', mixing 'frames' frames into 'outfile'.<br>" +
                        " 'frames' defaults to the length of 'infile'.<br>" +
                        " If 'mixer', use it to scale the various channels;<br>" +
                        " if 'envs' (an array of envelope generators), use it in conjunction with mixer<br>" +
                        " to scale and envelope all the various ins and outs.<br>" +
                        " 'outfile' can also be a frame-&gt;file generator, and<br>" +
                        " 'infile' can be a file-&gt;frame generator.";

var sndscm_openplayoutput_tip = "(open-play-output :optional chans srate format buffer-size):<br>" +
                                " opens an output audio port.  It takes the desired number of channels, <br>" +
                                " sampling rate, data format, and DAC buffer size (in samples),<br>" +
                                " and returns a list containing the audio port (-1 on failure),<br>" +
                                " the opened output channels, and the actual DAC buffer size.";

var sndscm_powerenv_tip = "(power-env env): an envelope generator where each segment has its own base.";

var sndscm_prc95doc_tip = "various physical modeling functions from Perry Cook.";

var sndscm_rmsgain_tip = "various RMS-related generators.";

var sndscm_run_tip = "(run  thunk) tries to optimize its argument using a sort of byte compiler.<br>" +
                     " When successful, it can speed up computation by about a factor of 10.<br>" +
                     " When unsuccessful, it falls back on Scheme, so you always get a sound. ";

var sndscm_savepeakenvinfo_tip = "The functions in peak-env.scm provide relatively robust access to<br>" +
                       " peak envelope files.  These files save Snd's overall amplitude envelopes<br>" +
                       " for a given sound so that a subsequent re-open of that sound has the<br>" +
                       " waveform immediately.  For very large sounds, this can save as much as<br>" +
                       " a minute during which Snd is running the amplitude envelope builders<br>" +
                       " in the background and displaying whatever it can.  That is, it makes<br>" +
                       " opening a large sound much faster after the initial read and save.<br>" +
                       " The variable 'save-peak-env-info' determines whether these envelopes are<br>" +
                       " being saved (its default is #t).";

var sndscm_scalemixes_tip = "(scale-mixes mix-list scl): scales the amplitude of each mix in 'mix-list' by 'scl'.";

var sndscm_sgfilter_tip = "(savitzky-golay-filter gen input): a Savitzky-Golay filter, assuming symmetrical positioning.<br>" +
                          " It is an FIR smoothing filter.";

var sndscm_sound_let_tip = "sound-let is a form of let* that creates temporary sound files within with-sound.<br>" +
                       " Its syntax is a combination of let* and with-sound:<br><br>" +
                       "<code> (sound-let ((temp-1 () (fm-violin 0 1 440 .1))<br>" +
                       "             (temp-2 () (fm-violin 0 2 660 .1)<br>" +
                       "                        (fm-violin .125 .5 880 .1)))<br>" +
                       "   (granulate-sound temp-1 0 2 0 2)     ;temp-1 is the name of the 1st temporary file<br>" +
                       "   (granulate-sound temp-2 1 1 0 2))</code><br><br>" +
                       " This creates two temporary files and passes them along to the subsequent calls<br>" +
                       " on granulate-sound.  The first list after the sound file identifier is the list of <br>" +
                       " with-sound options to be passed along when creating this temporary file.  These default<br>" +
                       " to :output with a unique name generated internally, and all other variables are taken from<br>" +
                       " the overall (enclosing) with-sound.  The rest of the list is the body of the associated with-sound.";

var sndscm_sounddatatosound_tip = "(sound-data-&gt;sound sd beg :optional dur snd): place the contents of<br>" +
                                  " its sound-data argument 'sd' into the sound 'snd' starting at 'beg' and going for 'dur' frames.<br>" +
                                  " 'dur' defaults to the sound-data object's length.";

var sndscm_soundinterp_tip = "(sound-interp reader loc): the sound-interp interpolating reader<br>" +
                             " reads a channel at an arbitary location, interpolating between samples if necessary.";

var sndscm_soundtosounddata_tip = "(sound-&gt;sound-data beg dur :optional snd):<br>" +
                                  " return a sound-data object containing the contents of the sound 'snd'<br>" +
                                  " starting from beg for dur frames.<br>" +
                                  " <code>  (sound-data-&gt;sound (sound-data* (sound-&gt;sound-data) 2.0))</code><br>" +
                                  " is yet another way to scale a sound by 2.0.";

var sndscm_syncdmixes_tip = "(syncd-mixes sync):  returns a list of all mixes whose mix-sync field is set to 'sync'.";

var sndscm_tofrequency_tip = "(-&gt;frequency pitch :optional ratio) takes either a number or a common-music pitch symbol<br>" +
                       " ('c4 is middle C), and returns either the number or the frequency associated with that pitch:<br>" +
                       " <code>(-&gt;frequency 'cs5)</code> returns 554 and change.<br>" +
                       " 'ratio' can be #t to get small integer ratios rather than equal temperment.";

var sndscm_tosample_tip = "(-&gt;sample time) returns a sample number given a time in seconds";

var sndscm_volterrafilter_tip = "(volterra-filter flt x): pass 'x' through the Volterra (non-linear) filter 'flt'.";

var sndscm_windowsamples_tip = "(window-samples :optional snd chn): returns (in a vct) the samples<br>" +
                               " displayed in the current graph window for the given channel.";

var sndscm_withtempsound_tip = "with-temp-sound is like sound-let (it sets up a temporary output<br>" +
                               " for with-sound) , but does not delete its output file.";

var sndscm_wsdoc_tip = "with-sound provides a simple way to package up a bunch of instrument calls into a new<br>" +
                       " sound file, and open that file in Snd when the computation is complete. <br>" +
                       " with-sound opens an output object, and optionally a reverb output object.<br>" +
                       " Each instrument uses out-any to add its sounds to the *output* results.<br>" +
                       "<pre> with-sound<br>" + 
                       "  :key (srate *clm-srate*)             ; output sampling rate (44100)<br>" + 
                       "       (output *clm-file-name*)        ; output file name (\"test.snd\")<br>" + 
                       "       (channels *clm-channels*)       ; channels in output (1)<br>" + 
                       "       (header-type *clm-header-type*) ; output header type (mus-next or mus-aifc)<br>" + 
                       "       (data-format *clm-data-format*) ; output sample data type (mus-bfloat)<br>" + 
                       "       (comment #f)                    ; any comment to store in the header (a string)<br>" + 
                       "       (reverb *clm-reverb*)           ; reverb instrument (jc-reverb)<br>" + 
                       "       (reverb-data *clm-reverb-data*) ; arguments passed to the reverb<br>" + 
                       "       (statistics *clm-statistics*)   ; if #t, print info at end of with-sound<br>" + 
                       "       (scaled-to #f)                  ; if a number, scale the output to peak at that amp<br>" + 
                       "       (play *clm-play*)               ; if #t, play the sound automatically</pre><br>" + 
                       " The with-sound syntax may look sightly odd; we include the arguments in the<br>" +
                       " first list, then everything after that is evaluated as a note list.<br>" +
                       "<pre>   (with-sound (:srate 44100 :channels 2 :output \"test.snd\")<br>" +
                       "      (fm-violin 0 1 440 .1)<br>" +
                       "      (fm-violin 1 1 660 .1))</pre><br>" +
                       " produces a sound file with two fm-violin notes; the sound file is named \"test.snd\",<br>" +
                       " is stero, and has a sampling rate of 44100.";

var sndscm_zipper_tip = "(zipper gen in1 in2): the digital zipper; a way to crossfade between in1 and in2.";



var sndlib_html_tip = "library that handles sound files and audio ports";

var sndclm_html_tip = "sound synthesis generators";

var sndscm_html_tip = "Scheme, Ruby, and Forth files included with Snd";

var fm_html_tip = "introduction to frequency modulation";

var extsnd_html_tip = "Snd extension and customization";

var grfsnd_html_tip = "Snd configuration, connection to other libraries and programs";

var snd_html_tip = "basic Snd user-interface documentation";

var libxm_html_tip = "library that ties Motif and Gtk into Snd";

var index_html_tip = "overall index";



var analog_filter_doc_tip = "These are the traditional IIR filters, any type, any even order<br>" +
                            "(Butterworth, Chebyshev, Inverse Chebyshev, Bessel, and Elliptic)<br>" +
                            "The elliptic function filters need GSL, and all of these work better<br>" +
                            "if you build Snd --with-doubles.";

var animals_doc_tip = "synthesis of birds, frogs, and insects";

var autosave_doc_tip = "periodically save current sound edits in a backup file, <br>" +
                       "useful if your machine crashes a lot.";

var bess_doc_tip = "This sets up a dialog to experiment with simple FM, <br>" +
                   "the fm-violin, or with a compositional algorithm";

var bird_doc_tip = "simple synthesis of about 50 birds using additive synthesis.<br>" +
                   "see animals.scm for much more elaborate versions of these birds";

var clean_doc_tip = "click, pop, and hum removal, and signal reconstruction";

var clm_ins_doc_tip = "Instruments using many standard synthesis techniques,<br>" +
                      " including a bagpipe, FOF synthesis, many FM examples,<br>" +
                      " granular synthesis, spectral modeling, reverbs, and physical modeling.";

var debug_doc_tip = "These are guile-specific debugging functions for tracing execution,<br>" +
                    " inserting breakpoints, and displaying a stack trace.";


var dlocsig_doc_tip = "dlocsig sets up envelopes to mimic a moving sound;<br>" +
                      " included are many path-specification functions";

var draw_doc_tip = "Examples of drawing extensions, primarily one that puts a thumbnail graph<br>" +
                   " of the current sound in the upper right corner";

var dsp_doc_tip = "This has all the usual DSP stuff: filters, ffts, sample rate conversion, <br>" +
                  " sound effects, statistics, scanned synthesis, transforms, etc";

var dot_snd_doc_tip = "Example initialization (~/.snd) files.<br>" +
                      " snd_frg.scm sets up key bindings to imitate Soundforge.";

var env_doc_tip = "Various operations on envelopes: add, scale, copy, stretch";

var enved_doc_tip = "This adds an envelope editor to each displayed channel.<br>" +
                    " You can set it up to be an amplitude envelope.";

var examp_doc_tip = "A bunch of examples of things like ffts, filters, marks, selections,<br>" +
                    " graphics extensions, sound effects, and generators.";

var extensions_doc_tip = "channel and sound property lists, several enveloping functions,<br>" +
                         " and commonly used editing sequences such as channel extraction.";

var fade_doc_tip = "sound mixing using envelopes in the frequency domain";

var frame_doc_tip = "various frame, vct, and sound-data functions";

var freeverb_doc_tip = "a reverberator along the lines of nrev, but with more options.";

var generators_doc_tip = "defgenerator and about 80 generators related to sums of sinusoids<br>" +
                         " bessel functions, adjustable square-waves, special envelopes, etc";

var grani_doc_tip = "this is a very flexible granular synthesis instrument";

var green_doc_tip = "generators similar to rand and rand-interp, producing 1/f noise, etc";

var heart_doc_tip = "This code is aimed at blood pressure readings.";

var hooks_doc_tip = "snd-hooks, describe-hook, with-local-hook, reset-all-hooks.";

var index_doc_tip = "this provides a connection between firefox and the snd-help mechanism.";

var inf_snd_doc_tip = "this provides a Snd emacs mode implementation.<br>" +
                      "  You can use emacs as the listener, rather than the built-in Snd window.";

var jcrev_doc_tip = "this is probably the first Schroeder reverb, based on all-pass and comb filters.";

var ladspa_doc_tip = "LADSPA is the linux audio group plugin standard. <br>" +
                     "There are a lot of plugin collections.";

var maraca_doc_tip = "this includes the maraca, tambourine, wind-chimes, etc";

var marks_doc_tip = "this includes describe-mark, eval-between-marks, mark-property,<br>" +
                    " play-between-marks, and snap-marks.";

var maxf_doc_tip = "This is a collection of modal synthesis demos.<br>" +
                   "  For the actual filter, see the firmant generator";

var menus_doc_tip = "Menu additions for things like crop, trim, fft notch filter,<br>" +
                    " mark and mix functions, etc.  The main added menu loads a huge<br>" +
                    " set of sound effects";

var mix_doc_tip = "mix-property, silence-all-mixes, mix-sound, save-mix, snap-mix-to-beat<br>" +
                  " and many functions acting on lists of mixes";

var mixer_doc_tip = "mixers and frames treated as matrices and vectors: <br>" +
                    " matrix determinant, transpose, invert, solve, mixer-poly, etc";

var moog_doc_tip = "Moog's four pole lowpass (24db/Oct) filter as a clm generator,<br>" +
                   " variable resonance, \"that warm, analog sound\".";

var musglyphs_doc_tip = "The CMN music symbol font built from bezier curves.<br>" +
                        "This file is a lisp-&gt;scheme wrapper for cmn-glyphs.lisp";

var nb_doc_tip = "As you move the mouse through the view-files list,<br>" +
                 " the help dialog posts information about the file underneath the mouse";

var noise_doc_tip = "This ancient noise instrument can produce those all-important whooshing<br>" +
                    " sounds.  noise.ins translated to Scheme/Ruby by Michael Scholz";

var numerics_doc_tip = "Various numerical functions: factorial, plgndr, gegenbaur, etc";

var oscope_doc_tip = "oscope.scm sets up a dialog with a Snd channel window<br>" +
                     " (time domain, fft etc) that displays data read from the<br>" +
                     " microphone in real time.";

var peak_env_doc_tip = "This saves the overall amplitude envelopes for a given sound so<br>" +
                       " that a subsequent re-open of the sound can display the full waveform quickly.";

var piano_doc_tip = "Scott van Duyne's piano model that includes multiple coupled strings,<br>" +
                    " a nonlinear hammer, and an arbitrarily large soundboard and enclosure";

var play_doc_tip = "play between marks, play continuously, play a set of sines, etc";

var poly_doc_tip = "polynomial addition, multiplication, division, gcd, roots, and discriminant";

var popup_doc_tip = "context-sensitive popup menus.  You get a different popup<br>" +
                    " menu over the main graph, the fft, the selection, the listener, etc";

var prc95_doc_tip = "The basic physical models: pluck, bow, clarinet, brass, flute";

var pvoc_doc_tip = "various versions of the Moore-Klingbeil-Trevisani-Edwards phase-vocoder.<br>" +
                   "see also the CLM phase-vocoder generator.";

var rgb_doc_tip = "this translates the standard X11 color names into Snd color objects.";

var rtex_doc_tip = "hard (glitch-free) real-time support for CLM instruments,<br>" +
                   " as well as numerous other extensions of Snd for user <br>" +
                   " interface development.  Included are rt-compiler.scm,<br>" +
                   " rt-engine.scm, rt-faust.scm, rt.tex, rt-clm-ins.scm, <br>" +
                   " rt-DotEmacs, rt-examples.scm, and rt-player.scm, <br>" +
                   " all thanks to Kjetil Matheussen";

var rtio_doc_tip = "show graph of real-time input and so on";

var rubber_doc_tip = "rubber-sound tries to stretch or contract a sound (in time);<br>" +
                     " it scans the sound looking for stable sections, then either <br>" +
                     " deletes periods or interpolates new ones to shorten or lengthen the sound";

var selection_doc_tip = "includes swap-selection-channels, replace-with-selection, <br>" +
                        " selection-members, make-selection, delete-selection-and-smooth,<br>" +
                        " filter-selection-and-smooth, and with-temporary-selection";

var singer_doc_tip = "This is based on Perry's singer.c and CLM's singer.ins";

var snd4_doc_tip = "These files (snd4.scm to snd9.scm) provide backwards compatibility<br>" +
                   " with earlier versions of Snd.";

var snddiff_doc_tip = "a diff or grep-like function for sounds. It can currently find<br>" +
                      " initial delays, scaling differences, and scattered individual<br>" +
                      " sample differences: <code>(snddiff snd0 chn0 snd1 chn1)</code>.";

var snd_gl_doc_tip = "This depends on access to openGL (Mesa); it includes a waterfall fft graph<br>" +
                     "and GL state readbacks";

var snd_motif_doc_tip = "user interface extensions using the libxm modules: add-mark-pane,<br>" +
                        " display-scanned-synthesis, load-font, with-level-meters,<br>" + 
                        " variable-display, smpte labels, and lots more.<br>" +  
                        " snd-motif is for Motif, snd-gtk for Gtk.";

var snd_test_doc_tip = "Snd regression test suite; zillions of examples.";

var sndwarp_doc_tip = "time stretching and whatnot";

var ws_doc_tip = "with-sound provides a simple way to package up a bunch of<br>" +
                 " instrument calls into a new sound file,  and open that file in Snd";

var zip_doc_tip = "The zipper marches through the two sounds taking equal short portions<br>" +
                  " of each, then abutting them while resampling so that as one sound<br>" +
                  " takes less overall frame space, the other takes more.";

var scheme_add_hook_tip = "(add-hook! hook function :optional append): adds 'function'<br>" +
                          "to the list of functions associated with 'hook'.  If 'append' is #t,<br>" +
                          "the function is added at the end of the list, otherwise at the start";

var scheme_reset_hook_tip = "(reset-hook! hook): removes all functions from 'hook'.";