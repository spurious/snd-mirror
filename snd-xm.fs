\ -*- snd-forth -*-
\ snd-xm.fs -- snd-motif.scm|snd-xm.rb --> snd-xm.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Mon Dec 26 22:36:46 CET 2005
\ Changed: Sat Dec 23 05:20:02 CET 2006

\ Commentary:
\
\ main-dpy                 ( -- dpy )
\ load-font                ( name -- fid|#f )
\ current-screen   	   ( -- scr )
\ white-pixel      	   ( -- pix )
\ black-pixel      	   ( -- pix )
\ screen-depth     	   ( -- n )
\ 
\ for-each-child   	   ( widget xt -- )
\ children->array          ( widget -- array )
\ find-child               ( widget name -- wid )
\ widget-exists?           ( widget name -- f )
\ main-widget-exists?      ( name -- f )
\ display-widget-tree      ( widget -- )
\ set-main-color-of-widget ( w -- )
\ host-name                ( -- host )
\ 
\ add-channel-pane         ( snd chn name type args -- wid )
\ add-sound-pane           ( snd name type args -- wid )
\ add-main-pane            ( name type args -- wid )
\ add-listener-pane        ( name type args -- wid )
\ 
\ raise-dialog             ( dialog -- )
\ activate-dialog          ( dialog -- )
\ add-mark-pane            ( -- )
\
\ show-smpte-label         ( on-or-off -- )
\ smpte-is-on              ( -- flag )
\ 
\ change-label     	   ( widget new-label -- )
\ show-disk-space          ( snd -- )
\ 
\ current-label    	   ( widget -- label )

\ Code:

require clm
require examp
'snd-motif provided? 'xm provided? not && [if] dl-load libxm Init_libxm [then]

'xm provided? [unless]
  'forth-error
  '( *filename* $" %s requires snd-motif and xm module or libxm.so" *filename* #f file-basename )
  fth-throw
[then]

: main-dpy ( -- dpy ) main-widgets cadr FXtDisplay ;

: load-font ( name -- fid|#f )
  { name }
  main-dpy name FXLoadQueryFont { fs }
  fs FXFontStruct? if fs Ffid else #f then 
;

: current-screen ( -- scr )
  doc" Returns the current X screen number of the current display."
  main-dpy FDefaultScreenOfDisplay
;
: white-pixel  ( -- pix ) current-screen FWhitePixelOfScreen ;
: black-pixel  ( -- pix ) current-screen FBlackPixelOfScreen ;
: screen-depth ( -- n )   current-screen FDefaultDepthOfScreen ;

\ --- apply func to every widget belonging to w ---

: for-each-child { wid xt -- }
  doc" Applies XT to WIDGET and each of its children."
  xt xt? if
    wid xt execute
  else
    xt proc? if
      xt '( wid ) run-proc drop
    then
  then
  wid FXtIsComposite if
    wid '( FXmNchildren 0 ) FXtVaGetValues cadr each ( w ) xt recurse end-each
  then
;

hide
: children->array-xt ( ary -- xt; child self -- )
  1 proc-create swap ,
 does> ( child self -- )
  ( self ) @ swap ( child ) array-push drop
;
set-current
: children->array ( widget -- array )
  #() { ary }
  ( widget ) ary children->array-xt for-each-child
  ary
;
previous
  
: find-child ( widget name -- wid )
  doc" Returns a widget named NAME, if one can be found in the widget hierarchy beneath WIDGET."
  { widget name }
  #f
  widget children->array each { w }
    w FXtName name string= if
      not
      w swap
      leave
    then
  end-each
  unless 'no-such-widget '( get-func-name name ) fth-throw then
;

: widget-exists? { widget name -- f }
  #f ( flag ) widget children->array each ( w ) FXtName name string= if not leave then end-each
;
: main-widget-exists? ( name -- f ) main-widgets cadr swap widget-exists? ;

hide
: display-widget <{ widget n -- }>
  widget FXtName empty? if
    $" <unnamed>"
  else
    widget FXtName
  then
  n spaces .string cr
  widget FXtIsComposite if
    widget '( FXmNchildren 0 ) FXtVaGetValues cadr each ( w ) n 2 + recurse end-each
  then
;
set-current
: display-widget-tree { widget -- }
  doc" Displays the hierarchy of widgets beneath WIDGET."
  ['] display-widget '( widget 0 ) run-proc drop
;
previous

hide
: change-color-xt <{ w -- }>
  w FXtIsWidget if
    w FXmIsScrollBar if
      w position-color FXmChangeColor drop
    else
      w basic-color FXmChangeColor drop
    then
  then
;
set-current
: set-main-color-of-widget ( widget -- )
  doc" Sets the background color of WIDGET."
  ['] change-color-xt for-each-child
;
previous

: host-name ( -- host )
  doc" Returns name of current machine."
  main-widgets cadr { wid }
  wid FXtWindow { win }
  main-dpy win main-dpy $" WM_CLIENT_MACHINE" #f FXInternAtom 0 32 #f FXA_STRING
  FXGetWindowProperty { host }
  host if host 5 list-ref else host then
;
  
\ --- add our own pane to the channel section ---

: add-channel-pane { snd chn name typ args -- wid }
  name typ snd chn channel-widgets 7 list-ref FXtParent FXtParent args undef FXtCreateManagedWidget
;

\ --- add our own pane to the sound section (underneath the controls in this case) ---

: add-sound-pane { snd name typ args -- wid }
  name typ snd sound-widgets car args undef FXtCreateManagedWidget
;

\ --- add our own pane to the overall Snd window (underneath the listener in this case) ---

: add-main-pane { name typ args -- wid }
  main-widgets 5 list-ref dup unless drop main-widgets 3 list-ref then { parent }
  name typ parent args undef FXtCreateManagedWidget
;

\ --- add a widget at the top of the listener ---

: add-listener-pane { name typ args -- wid }
  main-widgets cadr $" lisp-listener" find-child { listener }
  listener FXtParent { listener-scroll }
  listener-scroll FXtParent { listener-form }
  listener-scroll FXtUnmanageChild drop
  args
  '( FXmNleftAttachment  FXmATTACH_FORM
     FXmNrightAttachment FXmATTACH_FORM
     FXmNtopAttachment   FXmATTACH_FORM ) 2 list-append to args
  name typ listener-form args undef FXtCreateManagedWidget { top-widget }
  listener-scroll
  '( FXmNtopAttachment FXmATTACH_WIDGET FXmNtopWidget top-widget ) FXtVaSetValues drop
  listener-scroll FXtManageChild drop
  top-widget
;

\ 0 0 $" new-pane" FxmDrawingAreaWidgetClass
\ '( FXmNbackground graph-color FXmNforeground data-color ) add-channel-pane value draw-widget

\ --- bring possibly-obscured dialog to top ---

: raise-dialog ( dialog -- )
  { w }
  w FWidget? if
    w FXtIsManaged if
      w FXtParent { parent }
      parent FWidget? if
	parent FxmDialogShellWidgetClass FXtIsSubclass if
	  parent FXtGrabNone FXtPopup drop
	then
      then
    then
  then
;
: activate-dialog ( dialog -- ) dup FXtIsManaged if raise-dialog else FXtManageChild drop then ;

\ --- add-mark-pane ---

#f value including-mark-pane

hide
#() value mark-list-lengths
#() value mark-lists

: find-mark-list { snd chn dats -- lst }
  #f					\ flag
  dats each { dat }
    snd dat car  =
    chn dat cadr = && if
      drop				\ drop flag
      dat caddr
      leave
    then
  end-each
;
: mark-list-length ( snd chn -- len ) mark-list-lengths find-mark-list dup unless drop 0 then ;
: set-mark-list-length { snd chn len -- }
  mark-list-lengths each { dat }
    snd dat car  =
    chn dat cadr = && if
      mark-list-lengths i array-delete! drop
      leave
    then
  end-each
  mark-list-lengths '( snd chn len ) array-push drop
;
: mark-list ( snd chn -- lst ) mark-lists find-mark-list dup if caddr else drop '() then ;
: set-mark-list { snd chn lst -- } mark-lists '( snd chn lst ) array-push drop ;
: deactivate-channel { snd chn -- }
  snd chn mark-list-length 0>
  snd chn mark-list FWidget? && if
    snd chn mark-list '( FXmNchildren 0 ) FXtVaGetValues cadr each FXtUnmanageChild drop end-each
  then
;
: marks-focus-cb        <{ w c i -- }> w '( FXmNbackground white-pixel ) FXtVaSetValues drop ;
: marks-losing-focus-cb <{ w c i -- }> w '( FXmNbackground basic-color ) FXtVaSetValues drop ;
: marks-activate-cb <{ w c info -- }>
  w '( FXmNuserData 0 ) FXtVaGetValues cadr { id }
  w '( FXmNvalue 0 )    FXtVaGetValues cadr { txt }
  txt string? txt length 0> && if txt string->number else #f then { samp }
  samp if id samp set-mark-sample else id delete-mark then drop
  w '( FXmNbackground basic-color ) FXtVaSetValues drop
;
: marks-enter-cb <{ w c i f -- }> mouse-enter-text-hook '( w ) run-hook drop ;
: marks-leave-cb <{ w c i f -- }> mouse-leave-text-hook '( w ) run-hook drop ;
: make-mark-list { snd chn -- }
  snd chn mark-list-length { cur-len }
  snd chn deactivate-channel
  snd chn mark-list FWidget? unless
    snd chn $" mark-box" FxmFormWidgetClass
    '( FXmNbackground       basic-color
       FXmNorientation      FXmVERTICAL
       FXmNpaneMinimum      100
       FXmNbottomAttachment FXmATTACH_FORM ) add-channel-pane { mark-box }
    $" Marks" _ FxmLabelWidgetClass mark-box
    '( FXmNbackground       highlight-color
       FXmNleftAttachment   FXmATTACH_FORM
       FXmNrightAttachment  FXmATTACH_FORM
       FXmNalignment        FXmALIGNMENT_CENTER
       FXmNtopAttachment    FXmATTACH_FORM ) undef FXtCreateManagedWidget { mark-label }
    $" mark-scroller" FxmScrolledWindowWidgetClass mark-box
    '( FXmNbackground       basic-color
       FXmNscrollingPolicy  FXmAUTOMATIC
       FXmNscrollBarDisplayPolicy FXmSTATIC
       FXmNleftAttachment   FXmATTACH_FORM
       FXmNrightAttachment  FXmATTACH_FORM
       FXmNtopAttachment    FXmATTACH_WIDGET
       FXmNtopWidget        mark-label
       FXmNbottomAttachment FXmATTACH_FORM ) undef FXtCreateManagedWidget { mark-scroller }
    $" mark-list" FxmRowColumnWidgetClass mark-scroller
    '( FXmNorientation      FXmVERTICAL
       FXmNtopAttachment    FXmATTACH_FORM
       FXmNbottomAttachment FXmATTACH_FORM
       FXmNspacing          0 ) undef FXtCreateManagedWidget { mlist }
    mark-scroller set-main-color-of-widget
    mark-box '( FXmNpaneMinimum 1 ) FXtVaSetValues drop
    snd chn '( snd chn mlist ) set-mark-list
  then
  snd chn #f marks { new-marks }
  new-marks length cur-len > if
    snd chn mark-list { lst }
    new-marks length cur-len ?do
      $" field" FxmTextFieldWidgetClass lst
      '( FXmNbackground basic-color ) undef FXtCreateWidget { tf }
      tf FXmNfocusCallback       ['] marks-focus-cb        undef FXtAddCallback drop
      tf FXmNlosingFocusCallback ['] marks-losing-focus-cb undef FXtAddCallback drop
      tf FXmNactivateCallback    ['] marks-activate-cb     undef FXtAddCallback drop
      tf FEnterWindowMask #f     ['] marks-enter-cb        undef FXtAddEventHandler drop
      tf FLeaveWindowMask #f     ['] marks-leave-cb        undef FXtAddEventHandler drop
    loop
  then
  snd chn new-marks length set-mark-list-length
  snd chn mark-list '( FXmNchildren 0 ) FXtVaGetValues cadr each { wid }
    new-marks null? true? ?leave
    wid FXmIsTextField if
      wid
      '( FXmNvalue    new-marks car undef mark-sample number->string
	 FXmNuserData new-marks car ) FXtVaSetValues drop
      wid FXtManageChild drop
      new-marks cdr to new-marks
    then
  end-each
  #f
;
: remark <{ id snd chn reason -- }> snd chn make-mark-list ;
: unremark <{ snd -- }> snd channels 0 ?do snd i deactivate-channel loop ;
: marks-edit-cb { snd chn -- proc; self -- }
  0 proc-create chn , snd ,
 does> ( self -- )
  { self }
  self @ { chn }
  self cell+ @ { snd }
  snd chn mark-list FWidget? if snd chn make-mark-list then
;
: open-remarks <{ snd -- }>
  snd channels 0 ?do
    snd i after-edit-hook snd i marks-edit-cb add-hook!
    snd i undo-hook       snd i marks-edit-cb add-hook!
  loop
;
: marks-update-proc <{ snd -- }> snd channels 0 ?do snd i make-mark-list loop ;
: marks-update-cb <{ snd -- proc }> snd ['] marks-update-proc ;
set-current
: add-mark-pane ( -- )
  #t to including-mark-pane
  mark-hook       ['] remark          add-hook!
  close-hook      ['] unremark        add-hook!
  after-open-hook ['] open-remarks    add-hook!
  update-hook     ['] marks-update-cb add-hook!
;
previous

\  --- show-smpte-label ---

hide
24.0 value smpte-frames-per-second

: smpte-label { samp sr -- str }
  samp sr f/ { secs }
  secs smpte-frames-per-second f* { frms }
  secs 60.0 f/ floor { mins }
  mins 60.0 f/ floor { hours }
  $" %02d:%02d:%02d:%02d"
  '( hours f>s
     mins hours 60.0 f* f- f>s
     secs mins 60.0 f* f- f>s
     frms secs floor smpte-frames-per-second f* f- floor f>s ) string-format
;

: draw-smpte-label ( vars -- proc; snd chn self -- )
  2 proc-create swap ,
 does> ( snd chn self -- )
  { snd chn self }
  self @ { vars }
  snd chn undef axis-info { axinf }
  axinf 10 list-ref { x }
  axinf 13 list-ref { y }
  axinf 12 list-ref x - { grf-width }
  axinf 11 list-ref y - { grf-height }
  grf-height vars 'height hash-ref 2* >
  grf-width  vars 'width  hash-ref 1.5 f* f>s > &&
  snd chn time-graph? && if
    axinf car snd srate smpte-label { smpte }
    x y vars 'width hash-ref 2 snd chn undef fill-rectangle drop
    x y vars 'height hash-ref + vars 'width hash-ref 2 snd chn undef fill-rectangle drop
    x y 2 vars 'height hash-ref snd chn undef fill-rectangle drop
    x vars 'width hash-ref 2 - + y 2 vars 'height hash-ref snd chn undef fill-rectangle drop
    vars 'dpy hash-ref snd selected-channel chn = if
      snd-gcs cadr
    else
      snd-gcs car
    then vars 'fs hash-ref Ffid FXSetFont drop
    smpte x 4 + y 4 + snd chn undef draw-string drop
  then
;
set-current
: show-smpte-label ( on-or-off -- )
  doc" Turns on/off a label in the time-domain graph showing \
the current smpte frame of the leftmost sample."
  ( on-or-off ) if
    after-graph-hook ['] draw-smpte-label hook-member? unless
      #{} { vars }
      main-widgets cadr FXtDisplay { dpy }
      dpy axis-numbers-font FXLoadQueryFont { fs }
      vars 'width   fs $" 00:00:00:00" 11 FXTextWidth 8 +  hash-set!
      vars 'height  fs "0" 1 FXTextExtents caddr      8 +  hash-set!
      vars 'dpy dpy hash-set!
      vars 'fs  fs  hash-set!
      after-graph-hook vars draw-smpte-label add-hook!
      #t #t update-time-graph drop
    then
  else
    after-graph-hook ['] draw-smpte-label remove-hook! drop
    #t #t update-time-graph drop
  then
;

\ for prefs
: smpte-is-on ( -- flag ) after-graph-hook ['] draw-smpte-label hook-member? ;
previous

: change-label ( wid new-label -- )
  doc" Changes WIDGET's label to be NEW-LABEL."
  { wid new-label }
  new-label FXmStringCreateLocalized { str }
  wid '( FXmNlabelString str ) FXtVaSetValues drop
  str FXmStringFree drop
;

\ --- show-disk-space ---

hide
#() value labelled-snds

: kmg { num -- str }
  num 0<= if
    $" disk full!" _
  else
    "" { str }
    num 1024 > if
      num 1024 1024 * > if
	$" space: %6.3fG" _ '( num 1024.0 1024.0 f* f/ ) string-format
      else
	$" space: %6.3fM" _ '( num 1024.0 f/ )           string-format
      then
    else
      $" space: %10dK" _ '( num ) string-format
    then to str
    str
  then
;

: show-label <{ data id -- }>
  data car nil? unless
    data car   { snd }
    data cadr  { wid }
    data caddr { app }
    snd sound? if wid  snd file-name disk-kspace kmg change-label then
    app 10000 running-word data FXtAppAddTimeOut drop
  then
;
set-current

#f value showing-disk-space		\ for prefs

: show-disk-space <{ snd -- }>
  doc" Adds a label to the minibuffer area showing the current free space \
(for use with after-open-hook)."
  #f labelled-snds each { n } n car snd = if drop n leave then end-each { previous-label }
  previous-label unless
    snd sound? if
      #t to showing-disk-space
      main-widgets car { app }
      snd sound-widgets { widgets }
      widgets 3 list-ref { minibuffer }
      widgets 6 list-ref { unite-button }
      widgets 9 list-ref { sync-button }
      minibuffer FXtParent { name-form }
      snd file-name disk-kspace kmg FXmStringCreateLocalized { str }
      minibuffer FXtUnmanageChild drop
      minibuffer '( FXmNrightAttachment FXmATTACH_NONE ) FXtVaSetValues drop
      $" space" FxmLabelWidgetClass name-form
      '( FXmNbackground      basic-color
	 FXmNleftAttachment  FXmATTACH_NONE
	 FXmNlabelString     str
	 FXmNrightAttachment FXmATTACH_WIDGET
	 FXmNrightWidget     unite-button FXtIsManaged if unite-button else sync-button then
	 FXmNtopAttachment   FXmATTACH_FORM ) undef FXtCreateManagedWidget { new-label }
      minibuffer
      '( FXmNrightWidget new-label FXmNrightAttachment FXmATTACH_WIDGET ) FXtVaSetValues drop
      minibuffer FXtManageChild drop
      str FXmStringFree drop
      '( snd new-label app ) to previous-label
      labelled-snds previous-label array-push drop
      app 10000 ['] show-label previous-label FXtAppAddTimeOut drop
    else
      $" no sound found for disk space label" _ snd-error drop
    then
  then
;
\ after-open-hook ' show-disk-space add-hook!
previous

: current-label ( widget -- label )
  doc" Returns WIDGET's label."
  ( wid ) '( FXmNlabelString 0 ) FXtVaGetValues cadr FXmFONTLIST_DEFAULT_TAG FXmStringGetLtoR cadr
;

\ snd-xm.fs ends here
