\ -*- snd-forth -*-
\ popup.fs -- popup.scm|rb --> popup.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Dec 23 00:28:28 CET 2005
\ Changed: Thu Nov 26 18:32:35 CET 2009

\ Commentary:

\ selection-popup-menu
\ graph-popup-menu
\ fft-popup-menu
\ edit-history-menu
\ listener-popup-menu

\ edhist-help-edits            ( w c i -- )
\ add-popups                   ( -- )
\ change-menu-color            ( menu new-color -- )
\ change-selection-popup-color ( new-color -- )
\ change-graph-popup-color     ( new-color -- )
\ change-fft-popup-color       ( new-color -- )
\ change-edhist-popup-color    ( new-color -- )
\ change-listener-popup-color  ( new-color -- )

\ Code:

'snd-motif provided? [unless] skip-file [then]

require extensions
require snd-xm

\ for prefs
: edhist-help-edits <{ w c info  -- }>
  $" Edit History Functions"
  $" This popup menu gives access to the edit-list function handlers in Snd.  \
At any time you can backup in the edit list, 'save' the current trailing edits, make some \
new set of edits, then 'reapply' the saved edits.  The 'apply' choice gives access to all \
currently saved edit lists -- any such list can be applied to any channel.  'Clear' deletes \
all saved edit lists."
  #( $" {edit lists}" $" {edit-list->function}" )
  #( $" extsnd.html#editlists" $" extsnd.html#editlist_to_function" )
  help-dialog drop
;

hide
\ --- make-simple-popdown-menu for fft-popup-menu ---
: popup-cascade-cb { children cb -- prc; w c i self -- }
  3 proc-create cb , children ,
 does> { w c info self -- }
  self @ ( cb ) self cell+ @ ( children ) run-proc drop
;

: make-simple-popdown-menu { label popdown-labels parent cascade-cb -- }
  parent label #( FXmNbackground highlight-color ) undef FXmCreatePulldownMenu { top }
  label FxmCascadeButtonWidgetClass parent
  #( FXmNbackground highlight-color FXmNsubMenuId top ) undef FXtCreateManagedWidget { top-cascade }
  #() { children }
  popdown-labels proc? if
    \ edhist sends a proc to set TOP to edhist-widgets
    popdown-labels #( top ) run-proc drop
  else
    \ else arrays of #(name proc) lists
    popdown-labels each { poplab }
      poplab 0 array-ref FxmPushButtonWidgetClass top
      #( FXmNbackground highlight-color ) undef FXtCreateManagedWidget { child }
      child FXmNactivateCallback poplab 1 array-ref undef FXtAddCallback drop
      children child array-push drop
    end-each
  then
  cascade-cb if
    top-cascade FXmNcascadingCallback children cascade-cb popup-cascade-cb undef FXtAddCallback drop
  then
;

\ --- make-popdown-entry for listener-popup-menu ---
#() value listener-values
: collector-cb { func collector -- prc; w c i self -- val }
  3 proc-create func , collector ,
 does> { w c info self -- val }
  self       @ { func }
  self cell+ @ { collector }
  collector #( sounds ) run-proc ( lst ) 0 array-ref 1 >array func swap run-proc
;
: cas-cb ( func -- prc; w c i self -- val )
  3 proc-create swap ,
 does> { w c info self -- val }
  self @ ( func ) #( w current-label 0 find-sound ) run-proc
;
: popdown-cascade-cb { func collector menu children -- prc; w c i self -- }
  3 proc-create func , collector , menu , children ,
 does> { w c info self -- }
  self           @ { func }
  self   cell+   @ { collector }
  self 2 cells + @ { menu }
  self 3 cells + @ { children }
  children each FXtUnmanageChild drop end-each
  collector #( sounds ) run-proc { snds }
  children length { clen }
  snds length { slen }
  clen slen < if
    slen clen ?do
      "" FxmPushButtonWidgetClass menu
      #( FXmNbackground highlight-color ) undef FXtCreateManagedWidget { child }
      child FXmNactivateCallback func cas-cb undef FXtAddCallback drop
      children child array-push drop
    loop
  then
  slen if
    children each { child }
      snds i array-ref { snd }
      child snd short-file-name change-label
      child FXtManageChild drop
    end-each
  then
;

: make-popdown-entry { label parent func collector with-one -- values }
  #f { widget }
  #() { children }
  with-one if
    label FxmPushButtonWidgetClass parent
    #( FXmNbackground highlight-color ) undef FXtCreateManagedWidget to widget
    widget FXmNactivateCallback func collector collector-cb undef FXtAddCallback drop
  then
  parent label #( FXmNbackground highlight-color ) undef FXmCreatePulldownMenu { menu }
  label FxmCascadeButtonWidgetClass parent
  #( FXmNbackground highlight-color FXmNsubMenuId menu ) undef FXtCreateManagedWidget { menu-cas }
  menu-cas FXmNcascadingCallback
  func collector menu children popdown-cascade-cb undef FXtAddCallback drop
  listener-values  #( widget menu menu-cas collector )  array-push drop
;

\ --- make-popup-menu for graph-, fft-, and listener-menu ---
\
\ general entries:
\ entries: #( #( name type cb func ) ... )
\
\ simple popdown (fft-menu)
\ entries: #( #( name type labels-array cb ) ... )
\
\ special popdown (listener)
\ entries: #( #( name type func collector with-one ) ... )
: make-popup-menu ( name parent entries -- menu )
  { name parent entries }
  parent name
  #( FXmNpopupEnabled FXmPOPUP_AUTOMATIC
     FXmNbackground   highlight-color ) undef FXmCreatePopupMenu { menu }
  entries each { entry }
    #f { casc }
    entry 0 array-ref { label }		\ string
    entry 1 array-ref { typ }		\ symbols 'label, 'separator, 'cascade, or #f
    typ 'label = if
      FxmLabelWidgetClass
    else
      typ 'separator = if
	FxmSeparatorWidgetClass
      else
	FxmPushButtonWidgetClass
      then
    then { class }
    typ 'cascade = if
      entry length 4 = if		\ fft menu
	label
	entry 2 array-ref ( labels )
	menu
	entry 3 array-ref ( prc ) make-simple-popdown-menu
      else				\ listener menu
	label menu
	entry 2 array-ref ( func )
	entry 3 array-ref ( collector )
	entry 4 array-ref ( with-one ) make-popdown-entry
      then
    else
      label class menu #( FXmNbackground highlight-color ) undef FXtCreateManagedWidget { wid }
      entry 2 array-ref if		\ cb: proc of 3 args or #f
	wid FXmNactivateCallback entry 2 array-ref undef FXtAddCallback drop
      then
      entry 3 array-ref if		\ func: proc of 1 arg or #f
	entry 3 array-ref #( wid ) run-proc drop
      then
    then
  end-each
  menu
;

\ --- selection popup ---
: sel-stop-play-cb ( vars -- prc; self -- )
  0 proc-create swap ,
 does> { self -- }
  self @ { vars }
  vars :stopping array-assoc-ref if
    vars     :stopping #f array-assoc-set!
    ( vars ) :stop-widget array-assoc-ref FWidget? if
      vars :stop-widget array-assoc-ref "Play" change-label
    then
  then
;
: sel-play-cb ( vars -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { vars }
  vars :stopping array-assoc-ref if
    w "Play" change-label
    vars :stopping #f array-assoc-set!
    ( vars ) :stopping1 array-assoc-ref if
      vars   :stopping1 #f array-assoc-set!
      ( vars ) :stop-widget1 array-assoc-ref $" Loop play" change-label
    then
    undef stop-playing drop
  else
    w "Stop" change-label
    vars :stop-widget w array-assoc-set!
    ( vars ) :stopping #t array-assoc-set! drop
    selected-sound play drop
  then
;
: stop-playing-selection { w vars -- }
  w $" Loop play" change-label
  vars     :stopping1 #f array-assoc-set!
  ( vars ) :stopping array-assoc-ref if
    vars :stopping #f array-assoc-set!
    ( vars ) :stop-widget array-assoc-ref "Play" change-label
  then
;
: play-selection-again { w vars -- prc; reason self -- }
  1 proc-create w , vars ,
 does> { reason self -- }
  self       @ { w }
  self cell+ @ { vars }
  vars array? if
    c-g?                        not
    reason                       0= &&
    vars :stopping1 array-assoc-ref && if
      selected-sound  :stop w vars recurse  play drop
    else
      w vars stop-playing-selection
    then
  else
    $" %s: assoc array required, got %s" #( get-func-name vars ) string-format snd-warning drop
  then
;
: sel-loop-cb ( vars -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { vars }
  vars :stopping1 array-assoc-ref if
    w vars stop-playing-selection
    undef stop-playing drop
  else
    w "Stop!" change-label
    vars     :stop-widget1 w  array-assoc-set!
    ( vars ) :stopping1    #t array-assoc-set! to vars
    #f  :stop w vars play-selection-again  play drop
  then
;
: as-one-edit-thunk ( selection -- prc; self -- )
  0 proc-create swap ,
 does> { self -- }
  self @ { selection }
  selection 0 array-ref { snd }
  selection 1 array-ref { chn }
  snd chn selection-position { beg }
  snd chn selection-frames   { len }
  beg 0> if 0 beg snd chn delete-samples drop then
  len snd chn #f frames < if len 1+ snd chn #f frames len - snd chn delete-samples drop then
;

: sel-del  <{ w c info -- val }> delete-selection ;
: sel-zero <{ w c info -- val }> 0.0 scale-selection-by ;
: sel-crop <{ w c info -- }>
  selection-members each { selection }
    selection as-one-edit-thunk "" as-one-edit drop
  end-each
;
: sel-save-as <{ w c info -- val }> save-selection-dialog ;
: sel-copy <{ w c info -- }>
  snd-tempnam { new-file-name }
  new-file-name save-selection drop
  new-file-name open-sound drop
;
: sel-cut <{ w c info -- val }>
  snd-tempnam { new-file-name }
  new-file-name save-selection drop
  delete-selection drop
  new-file-name open-sound
;
: sel-marks <{ w c info -- }>
  selection-members each { select }
    select 0 array-ref { snd }
    select 1 array-ref { chn }
    snd chn selection-position { pos }
    snd chn selection-frames 1- { len }
    pos snd chn add-mark drop
    pos len + snd chn add-mark drop
  end-each
;
: sel-info <{ w c info -- val }>
  #f #f selection-position { beg }
  #f #f selection-frames   { len }
  $"     start: %d, %.3f\n" #( beg beg #f srate f/ ) string-format { str }
  $"       end: %d, %.3f\n" #( beg len + dup #f srate f/ ) string-format str swap << to str
  $"  duration: %d, %.3f\n" #( len len #f srate f/ ) string-format str swap << to str
  $"     chans: %d\n" selection-chans string-format str swap << to str
  $"    maxamp: %.3f\n" #f #f selection-maxamp string-format str swap << to str
  $" Selection Info" str info-dialog drop
;
\ choice 2 == selection
: sel-appcnt <{ w c info -- val }> #f 2 0 undef apply-controls ;
: sel-rescnt <{ w c info -- val }> #f           reset-controls ;
: sel-unsel  <{ w c info -- val }> #f #t set-selection-member? ;
: sel-rev    <{ w c info -- val }> reverse-selection ;
: sel-mix    <{ w c info -- val }> #f #f #f cursor mix-selection ;
: sel-invert <{ w c info -- val }> -1.0 scale-selection-by ;

let: ( -- menu )
  #a( :stopping #f :stopping1 #f :stop-widget #f :stop-widget1 #f ) { vars }
  stop-playing-selection-hook vars sel-stop-play-cb add-hook!
  "selection-popup" main-widgets 2 array-ref
  #( #( $" Selection"        'label     #f               #f )
     #( $" sep"              'separator #f               #f )
     #( $" Play"             #f         vars sel-play-cb #f )
     #( $" Loop play"        #f         vars sel-loop-cb #f )
     #( $" Delete"           #f         <'> sel-del      #f )
     #( $" Zero"             #f         <'> sel-zero     #f )
     #( $" Crop"             #f         <'> sel-crop     #f )
     #( $" Save as"          #f         <'> sel-save-as  #f )
     #( $" Copy->New"        #f         <'> sel-copy     #f )
     #( $" Cut->New"         #f         <'> sel-cut      #f )
     #( $" Snap marks"       #f         <'> sel-marks    #f )
     #( $" Selection Info"   #f         <'> sel-info     #f )
     #( $" Apply controls"   #f         <'> sel-appcnt   #f )
     #( $" Reset controls"   #f         <'> sel-rescnt   #f )
     #( $" Unselect"         #f         <'> sel-unsel    #f )
     #( $" Reverse"          #f         <'> sel-rev      #f )
     #( $" Mix"              #f         <'> sel-mix      #f )
     #( $" Invert"           #f         <'> sel-invert   #f ) ) make-popup-menu
;let constant selection-popup-menu

\ --- time domain popup ---
#f value graph-popup-snd
#f value graph-popup-chn

: stop-playing-cb ( vars -- prc; snd self -- )
  1 proc-create swap ,
 does> { snd self -- }
  self @ { vars }
  vars :stopping array-assoc-ref if
    vars     :stopping #f array-assoc-set!
    ( vars ) :stop-widget array-assoc-ref dup if "Play" change-label else drop then
  then
;
: play-cb ( vars -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { vars }
  vars :stopping array-assoc-ref if
    vars :stopping #f array-assoc-set! drop
    w "Play" change-label
    undef stop-playing drop
  else
    w "Stop" change-label
    vars :stopping #t array-assoc-set! drop
    graph-popup-snd play drop
  then
;
: stop-cb ( vars -- prc; widget self -- )
  1 proc-create swap ,
 does> { w self -- }
  self @ ( vars ) :stop-widget w array-assoc-set! drop
;
: pchan-cb ( vars -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { vars }
  vars     :stopping #t array-assoc-set!
  ( vars ) :stop-widget array-assoc-ref "Stop" change-label
  graph-popup-snd :channel graph-popup-chn play drop
;
: pcur-cb ( vars -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { vars }
  vars     :stopping #t array-assoc-set!
  ( vars ) :stop-widget array-assoc-ref "Stop" change-label
  graph-popup-snd :start graph-popup-snd graph-popup-chn #f cursor play drop
;
: pprev-cb ( vars -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { vars }
  vars     :stopping #t array-assoc-set!
  ( vars ) :stop-widget array-assoc-ref "Stop" change-label
  0 graph-popup-snd graph-popup-chn #f #f
  graph-popup-snd graph-popup-chn edit-position 1-
  undef undef play drop
;
: porig-cb ( vars -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { vars }
  vars     :stopping #t array-assoc-set!
  ( vars ) :stop-widget array-assoc-ref "Stop" change-label
  graph-popup-snd :channel graph-popup-chn :edit-position 0 play drop
;

: pundo-cb   <{ w c info -- val }> 1 graph-popup-snd graph-popup-chn undo ;
: predo-cb   <{ w c info -- val }> 1 graph-popup-snd graph-popup-chn redo ;
: prev-cb    <{ w c info -- val }> graph-popup-snd revert-sound ;
: popen-cb   <{ w c info -- val }> #t open-file-dialog ;
: psave-cb   <{ w c info -- val }> graph-popup-snd save-sound ;
: psaveas-cb <{ w c info -- val }> graph-popup-snd select-sound drop save-sound-dialog ;
: pupdate-cb <{ w c info -- val }> graph-popup-snd update-sound ;
: pclose-cb  <{ w c info -- val }> graph-popup-snd close-sound-extend #f ;
: pmixsel-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn #f cursor
  graph-popup-snd graph-popup-chn mix-selection
;
: pinssel-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn #f cursor
  graph-popup-snd graph-popup-chn insert-selection
;
: prepsel-cb <{ w c info -- val }>
  graph-popup-snd { snd }
  graph-popup-chn { chn }
  snd chn #f cursor { beg }
  snd chn selection-frames { len }
  snd chn selection-position { sbeg }
  snd chn selection-member? not
  beg len + sbeg < ||
  beg sbeg len + > || if
    beg len snd chn #f delete-samples drop
    beg snd chn insert-selection
  else
    beg sbeg < if beg sbeg beg - snd chn #f delete-samples then
  then
;
: pselall-cb <{ w c info -- val }> graph-popup-snd graph-popup-chn select-all ;
: punsel-cb  <{ w c info -- val }> #f #t set-selection-member? ;
: papcnt-cb  <{ w c info -- val }> #f 0 0 undef apply-controls ;
: precnt-cb  <{ w c info -- val }> #f reset-controls ;
: print-props { props -- str }
  "" { str }
  object-print-length { old-len }
  print-length        { old-vct-len }
  3 set-object-print-length
  3 set-print-length drop
  props each { prop }			\ ( key . val )
    str  $"   %s:  %s\n" #( prop 0 array-ref prop 1 array-ref ) string-format << to str
  end-each
  old-len       set-object-print-length
  old-vct-len   set-print-length drop
  str
;
: pinfo-cb <{ w c info -- val }>
  graph-popup-snd { snd }
  graph-popup-chn { chn }
  snd chn #f frames { frms }
  snd srate { sr }
  $"    chans: %d, srate: %d\n"     #( snd channels sr ) string-format { str }
  $"   format: %s [%s]\n"
  #( snd data-format mus-data-format-name snd header-type mus-header-type-name )
  string-format str swap << to str
  $"   length: %.3f  (%d frames)\n" #( frms sr f/ frms ) string-format str swap << to str
  snd #t #f maxamp each { mx }
    $" %6s %c: %.3f\n" #( "maxamp" [char] A i + mx ) string-format str swap << to str
  end-each
  snd comment empty? unless
    $"  comment: %S\n" #( snd comment )            string-format str swap << to str
  then
  snd file-name mus-sound-loop-info { loops }
  loops nil? unless
    $"     loop: %s\n"     #( loops )              string-format str swap << to str
  then
  snd header-type mus-soundfont = if
    $"   sounds: %s\n"     #( snd soundfont-info ) string-format str swap << to str
  then
  snd sound-properties { props }
  props nil? unless
    str "properties:\n" << to str
    str props print-props << to str
  then
  snd channels 0 ?do
    snd i channel-properties to props
    props nil? unless
      $" chan %d properties:\n" #( i ) string-format str swap << to str
      str props print-props << to str
    then
  loop
  snd file-name $"  info" $+ str info-dialog
;
: paddmrk-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn #f cursor
  graph-popup-snd graph-popup-chn add-mark
;
: pdelmrk-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn #f marks { ms }
  ms nil? if
    #f
  else
    ms length 1 = if
      ms 0 array-ref delete-mark drop
    else
      graph-popup-snd graph-popup-chn #f cursor { loc }
      ms 0 array-ref { id }
      loc id undef mark-sample - abs { cur-min }
      ms each { m }
	loc m undef mark-sample - abs { this-min }
	this-min cur-min < if
	  this-min to cur-min
	  m to id
	then
      end-each
      id delete-mark
    then
  then
;
: pdelamrk-cb <{ w c info -- val }> graph-popup-snd graph-popup-chn delete-marks ;
: pnextmrk-cb <{ w c info -- val }> [char] j 4 graph-popup-snd graph-popup-chn key ; \ C-j
: plastmrk-cb <{ w c info -- val }>
  [char] - 4 graph-popup-snd graph-popup-chn key drop \ C--
  [char] j 4 graph-popup-snd graph-popup-chn key \ C-j
;
: exit-cb <{ w c info -- val }> 0 snd-exit ;

let: ( -- menu )
  #a( :stopping #f :stop-widget #f ) { vars }
  stop-playing-hook vars stop-playing-cb add-hook!
  "graph-popup" main-widgets 2 array-ref
  #( #( $" Snd"              'label       #f              #f )
     #( $" sep"              'separator   #f              #f )
     #( $" Play"             #f         vars play-cb    vars stop-cb )
     #( $" Play channel"     #f         vars pchan-cb   #f )
     #( $" Play from cursor" #f         vars pcur-cb    #f )
     #( $" Play previous"    #f         vars pprev-cb   #f )
     #( $" Play original"    #f         vars porig-cb   #f )
     #( $" Undo"             #f         <'> pundo-cb    #f )
     #( $" Redo"             #f         <'> predo-cb    #f )
     #( $" Revert"           #f         <'> prev-cb     #f )
     #( $" Open"             #f         <'> popen-cb    #f )
     #( $" Save"             #f         <'> psave-cb    #f )
     #( $" Save as"          #f         <'> psaveas-cb  #f )
     #( $" Update"           #f         <'> pupdate-cb  #f )
     #( $" Close"            #f         <'> pclose-cb   #f )
     #( $" Mix selection"    #f         <'> pmixsel-cb  #f )
     #( $" Insert selection" #f         <'> pinssel-cb  #f )
     #( $" Replace with selection" #f   <'> prepsel-cb  #f )
     #( $" Select all"       #f         <'> pselall-cb  #f )
     #( $" Unselect"         #f         <'> punsel-cb   #f )
     #( $" Apply controls"   #f         <'> papcnt-cb   #f )
     #( $" Reset controls"   #f         <'> precnt-cb   #f )
     #( $" Info"             #f         <'> pinfo-cb    #f )
     #( $" Add mark"         #f         <'> paddmrk-cb  #f )
     #( $" Delete mark"      #f         <'> pdelmrk-cb  #f )
     #( $" Delete all marks" #f         <'> pdelamrk-cb #f )
     #( $" To next mark"     #f         <'> pnextmrk-cb #f )
     #( $" To last mark"     #f         <'> plastmrk-cb #f )
     #( $" sep"              'separator #f              #f )
     #( $" Exit"             #f         <'> exit-cb     #f ) ) make-popup-menu
;let constant graph-popup-menu

: graph-popup-cb { snd chn -- prc; widget self -- }
  1 proc-create chn , snd ,
 does> { w self -- }
  self @ { chn }
  self cell+ @ { snd }
  snd chn edits { eds }
  w FXtName { name }
  name $" Snd" string= if
    snd channels 1 > if
      $" %s[%d]" #( snd short-file-name chn ) string-format w swap change-label
    else
      w snd short-file-name change-label
    then
  else
    name $" Save"          string=
    name $" Undo"          string= ||
    name $" Revert"        string= ||
    name $" Play previous" string= || if
      w eds 0 array-ref 0> if FXtManageChild else FXtUnmanageChild then drop
    else
      name $" Play channel" string= if
	w snd channels 1 > if FXtManageChild else FXtUnmanageChild then drop
      else
	name $" Redo" string= if
	  w eds 1 array-ref 0> if FXtManageChild else FXtUnmanageChild then drop
	else
	  name $" Mix selection"          string=
	  name $" Insert selection"       string= ||
	  name $" Unselect"               string= ||
	  name $" Replace with selection" string= || if
	    w undef selection? if FXtManageChild else FXtUnmanageChild then drop
	  else
	    name $" Play from cursor" string= if
	      w snd chn #f cursor 0> if FXtManageChild else FXtUnmanageChild then drop
	    else
	      name $" Play original" string= if
		w eds 0 array-ref 1 > if FXtManageChild else FXtUnmanageChild then drop
	      else
		name $" Delete mark"       string=
		name $" Delete all marks"  string= ||
		name $" To next mark"      string= ||
		name $" To last mark"      string= || if
		  w snd chn #f marks nil? unless FXtManageChild else FXtUnmanageChild then drop
		then
	      then
	    then
	  then
	then
      then
    then
  then
;
: edit-graph-popup-menu ( snd chn -- )
  doc" Hides otiose entries, relabel others to reflect current state of SND and CHN."
  { snd chn }
  graph-popup-menu snd chn graph-popup-cb for-each-child
;

\ --- fft popup ---
: choose-chan ( -- chn )
  graph-popup-snd channel-style channels-separate = if graph-popup-chn else #t then
;
: fft-peaks-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn show-transform-peaks not
  graph-popup-snd choose-chan set-show-transform-peaks
;
: fft-db-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn fft-log-magnitude not
  graph-popup-snd choose-chan set-fft-log-magnitude
;
: fft-frq-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn fft-log-frequency not
  graph-popup-snd choose-chan set-fft-log-frequency
;
: fft-norm-cb <{ w c info -- val }>
  graph-popup-snd graph-popup-chn transform-normalization dont-normalize = if
    normalize-by-channel graph-popup-snd choose-chan set-transform-normalization
  else
    dont-normalize       graph-popup-snd choose-chan set-transform-normalization
  then
;
: grp-lst-cb ( val -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ ( val ) graph-popup-snd choose-chan set-transform-graph-type drop
;
: grp-labs ( -- ary )
  #( #( "once"        graph-once           grp-lst-cb )
     #( "sonogram"    graph-as-sonogram    grp-lst-cb )
     #( "spectrogram" graph-as-spectrogram grp-lst-cb ) )
;
: grp-set <{ lst -- }>
  lst each ( child )
    graph-popup-snd graph-popup-chn transform-graph-type i = if #f else #t then FXtSetSensitive drop
  end-each
;
#( 16 32 64 128 256 512 1024 2048 4096 8192 16384 65536 262144 1048576 ) constant fft-siz-sizes
: siz-lst-cb ( val -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ ( val ) graph-popup-snd choose-chan set-transform-size drop
;
: siz-labs ( -- ary )
  save-stack { stack }
  fft-siz-sizes map #( *key* object->string *key* siz-lst-cb ) end-map { ary }
  stack restore-stack
  ary
;
: siz-set <{ lst -- }>
  save-stack { stack }
  lst each ( child )
    fft-siz-sizes i array-ref { siz }
    graph-popup-snd graph-popup-chn transform-size siz <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
;
#( rectangular-window
   hann-window
   welch-window
   parzen-window
   bartlett-window
   hamming-window
   blackman2-window 
   blackman3-window
   blackman4-window
   blackman5-window
   blackman6-window
   blackman7-window
   blackman8-window
   blackman9-window
   blackman10-window
   exponential-window
   riemann-window
   kaiser-window
   cauchy-window
   poisson-window
   gaussian-window
   tukey-window
   dolph-chebyshev-window
   hann-poisson-window
   connes-window
   samaraki-window
   ultraspherical-window
   bartlett-hann-window
   bohman-window
   flat-top-window ) constant fft-win-windows
: win-lst-cb ( val -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ ( val ) graph-popup-snd choose-chan set-fft-window drop
;
: win-labs ( -- ary )
  #( "Rectangular"
     "Hann"
     "Welch"
     "Parzen"
     "Bartlett"
     "Hamming"
     "Blackman2"
     "Blackman3"
     "Blackman4"
     "Blackman5"
     "Blackman6"
     "Blackman7"
     "Blackman8"
     "Blackman9"
     "Blackman10"
     "Exponential"
     "Riemann"
     "Kaiser"
     "Cauchy"
     "Poisson"
     "Gaussian"
     "Tukey"
     "Dolph-Chebyshev"
     "Hann-Poisson"
     "Connes"
     "Samaraki"
     "Ultraspherical"
     "Bartlett-Hann"
     "Bohman"
     "Flat-top" ) map #( *key* fft-win-windows i array-ref win-lst-cb ) end-map
;
: win-set <{ lst -- }>
  save-stack { stack }
  lst each ( child )
    fft-win-windows i array-ref { win }
    graph-popup-snd graph-popup-chn fft-window win <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
;
#( fourier-transform wavelet-transform autocorrelation cepstrum walsh-transform haar-transform )
value fft-trn-transform
: trn-lst-cb ( val -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ ( val ) graph-popup-snd choose-chan set-transform-type drop
;
: trn-labs ( -- ary )
  save-stack { stack }
  #( "Fourier" "Wavelet" "Autocorrelate" "Cepstrum" "Walsh" "Haar" ) { names }
  names map #( *key* fft-trn-transform i array-ref trn-lst-cb ) end-map { ary }
  stack restore-stack
  ary
;
: trn-set <{ lst -- }>
  save-stack { stack }
  lst each ( child )
    fft-trn-transform i array-ref { trn }
    graph-popup-snd graph-popup-chn transform-type trn <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
;
: typ-lst-cb ( val -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ ( val ) graph-popup-snd choose-chan set-wavelet-type drop
;
: typ-labs ( -- ary )
  #( "doub4"
     "doub6"
     "doub8"
     "doub10"
     "doub12"
     "doub14"
     "doub16"
     "doub18"
     "doub20"
     "battle_lemarie"
     "burt_adelson"
     "beylkin"
     "coif2"
     "coif4"
     "coif6"
     "sym2"
     "sym3"
     "sym4"
     "sym5"
     "sym6" ) map #( *key* i typ-lst-cb ) end-map
;
: typ-set <{ lst -- }>
  save-stack { stack }
  lst each ( child )
    graph-popup-snd graph-popup-chn wavelet-type i <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
;
: fft-color <{ w c info -- val }> color-orientation-dialog ;

let: ( -- menu )
  $" fft-popup" main-widgets 2 array-ref
  #( #( $" Transform"        'label     #f               #f )
     #( $" sep"              'separator #f               #f )
     #( $" Peaks"            #f         <'> fft-peaks-cb #f )
     #( $" dB"               #f         <'> fft-db-cb    #f )
     #( $" Log freq"         #f         <'> fft-frq-cb   #f )
     #( $" Normalize"        #f         <'> fft-norm-cb  #f )
     #( $" Graph type"       'cascade   grp-labs         <'> grp-set )
     #( $" Size"             'cascade   siz-labs         <'> siz-set )
     #( $" Window"           'cascade   win-labs         <'> win-set )
     #( $" Transform type"   'cascade   trn-labs         <'> trn-set )
     #( $" Wavelet type"     'cascade   typ-labs         <'> typ-set )
     #( $" Color/Orientation" #f        <'> fft-color    #f ) ) make-popup-menu
;let constant fft-popup-menu

: fft-popup-cb { snd chn -- cb; widget self -- }
  1 proc-create chn , snd ,
 does> { w self -- }
  self @ { chn }
  self cell+ @ { snd }
  w FXtName { name }
  name $" Peaks" string= if
    w snd chn show-transform-peaks if $" No peaks" else $" Peaks" then change-label
  else
    name $" dB" string= if
      w snd chn fft-log-magnitude if $" Linear" else $" dB" then change-label
    else
      name $" Log freq" string= if
	w snd chn fft-log-frequency if $" Linear freq" else $" Log freq" then change-label
      then
    then
  then
;

: edit-fft-popup-menu ( snd chn -- wid )
  doc" Changes the fft-related popup menu to reflect the state of SND and CHN."
  { snd chn }
  save-stack { stack }
  fft-popup-menu snd chn fft-popup-cb for-each-child
  stack restore-stack
;
set-current
previous

\ --- edit history popup ---
1 $" edhist-save-edits calls it with PROC as its only argument." create-hook edhist-save-hook

hide
#() value edhist-funcs
#() value edhist-widgets
#f  value edhist-snd
#f  value edhist-chn

: edhist-clear-edits <{ w c info -- #f }> #() to edhist-funcs #f ;
: edhist-save-edits <{ w c info -- val }>
  edhist-funcs #( edhist-snd edhist-chn ) array-assoc-ref { old-proc }
  edhist-snd edhist-chn edits { cur-edits }
  edhist-snd edhist-chn cur-edits 0 array-ref 1+ 0 cur-edits each
    +
  end-each edit-list->function { proc }
  edhist-save-hook #( proc ) run-hook drop
  old-proc proc? if
    edhist-funcs #( edhist-snd edhist-chn ) proc array-assoc-set!
  else
    edhist-funcs #( #( edhist-snd edhist-chn ) proc ) array-push
  then to edhist-funcs
;
: edhist-reapply-edits <{ w c info -- val }>
  edhist-funcs #( edhist-snd edhist-chn ) array-assoc-ref #( edhist-snd edhist-chn ) run-proc
;
: edhist-set-wid <{ widget -- }> edhist-widgets widget array-push to edhist-widgets ;
: edhist-apply <{ w c info -- }>
  edhist-funcs c range? if
    edhist-funcs c array-ref 1 array-ref ( proc ) #( edhist-snd edhist-chn ) run-proc drop
  then
;
: edhist-apply-edits <{ lst -- }>
  edhist-widgets 0 array-ref { parent }
  edhist-widgets 1 nil array-subarray { wids }
  edhist-funcs each 0 array-ref { label }
    nil { button }
    wids nil? if
      $" wid" FxmPushButtonWidgetClass parent
      #( FXmNbackground highlight-color ) undef FXtCreateManagedWidget to button
      edhist-widgets #( button ) array-append to edhist-widgets
      button FXmNactivateCallback <'> edhist-apply i FXtAddCallback drop
    else
      wids 0 array-ref to button
      wids 1 nil array-subarray to wids
      button FXtManageChild drop
    then
    label array? if
      \ label: #(snd chn)
      button  $" %s[%s]" #( label 0 array-ref short-file-name label 1 array-ref ) format change-label
    else
      \ label: "file-name[chn]"
      button label change-label
    then
    button #( FXmNuserData i ) FXtVaSetValues drop
  end-each
  wids each FXtUnmanageChild drop end-each
;
: edhist-close-hook-cb <{ snd -- }>
  snd channels 0 ?do
    edhist-funcs #( snd i ) array-assoc { old-val }
    old-val array? if
      old-val  0  $" %s[%d]" #( snd short-file-name i ) string-format array-set!
    then
  loop
;

let: ( -- menu )
  close-hook <'> edhist-close-hook-cb add-hook!
  "edhist-popup" main-widgets 2 array-ref
  #( #( "Edits"   'label     #f                       #f )
     #( "sep"     'separator #f                       #f )
     #( "Save"    #f         <'> edhist-save-edits    #f )
     #( "Reapply" #f         <'> edhist-reapply-edits #f )
     #( "Apply"   'cascade   <'> edhist-set-wid       <'> edhist-apply-edits  )
     #( "Clear"   #f         <'> edhist-clear-edits   #f )
     #( "sep"     'separator #f                       #f )
     #( "Help"    #f         <'> edhist-help-edits    #f ) ) make-popup-menu
;let constant edit-history-menu

: edhist-popup-cb { snd chn -- cb; widget self -- }
  1 proc-create chn , snd ,
 does> { w self -- }
  self @ { chn }
  self cell+ @ { snd }
  w FXtName { name }
  name "Clear" string= name "Apply" string= || if
    w edhist-funcs empty? if #f else #t then FXtSetSensitive drop
  else
    name "Save" string= if
      w 0 snd chn edits each + end-each 0> if #t else #f then FXtSetSensitive drop
    else
      name "Reapply" string= if
	w edhist-funcs #( snd chn ) array-assoc-ref if #t else #f then FXtSetSensitive drop
      then
    then
  then
;
: edit-edhist-popup-menu ( snd chn -- wid )
  { snd chn }
  save-stack { stack }
  edit-history-menu snd chn edhist-popup-cb for-each-child
  stack restore-stack
;

\  --- activate the above menus ---
: edhist-popup-handler-cb { snd chn -- prc; w c i self -- }
  3 proc-create chn , snd ,
 does> { w c info self -- }
  self @ { chn }
  self cell+ @ { snd }
  info Fevent { ev }
  FButtonPress ev Ftype = if
    snd to edhist-snd
    chn to edhist-chn
    snd chn edit-edhist-popup-menu
    info edit-history-menu Fset_menuToPost drop
  then
;  
: popup-handler-cb { snd chn -- prc; w c i self -- }
  3 proc-create chn , snd ,
 does> { w c info self -- }
  self @ { chn }
  self cell+ @ { snd }
  info Fevent { ev }
  ev Fx_root w 0 0 FXtTranslateCoords 0 array-ref - { xe }
  FButtonPress ev Ftype = if
    snd to graph-popup-snd
    chn to graph-popup-chn
    snd channel-style channels-combined = if
      ev Fy { ye }
      #t				\ flag
      snd channels 0 ?do
	ye snd i undef axis-info 14 array-ref < if
	  i 1- to graph-popup-chn
	  not				\ toggle flag
	  leave
	then
      loop ( flag ) if snd channels 1- to graph-popup-chn then
    then
    snd chn transform-graph? if snd chn transform-graph axis-info else #f then { fax }
    snd chn lisp-graph?      if snd chn lisp-graph      axis-info else #f then { lax }
    fax if
      xe fax 10 array-ref >=
      xe fax 12 array-ref <= && if
	\ in fft
	snd chn edit-fft-popup-menu
	info fft-popup-menu Fset_menuToPost drop
	#f
      else
	#t
      then
    else
      #t
    then if
      lax if
	xe lax 10 array-ref >=
	xe lax 12 array-ref <= && if
	  \ in lisp
	  #f
	else
	  #t
	then
      else
	#t
      then if
	undef selection? if
	  snd graph-popup-chn selection-position { pos }
	  snd srate { sr }
	  pos sr f/ { beg }		\ BEG and END should be floats
	  pos snd graph-popup-chn selection-frames f+ sr f/ { end }
	  xe beg snd chn undef x->position >=
	  xe end snd chn undef x->position <= && if
	    info selection-popup-menu Fset_menuToPost drop
	    #f
	  else
	    #t
	  then
	else
	  #t
	then if
	  snd graph-popup-chn edit-graph-popup-menu
	  info graph-popup-menu Fset_menuToPost drop
	then
      then
    then
  then
;

#() constant popups

: add-popup <{ snd -- }>
  snd channels 0 ?do
    popups #( snd i ) array-member? unless
      popups #( snd i ) array-push drop
      snd i channel-widgets 7 array-ref  ( chn-edhist )
      FXmNpopupHandlerCallback snd i edhist-popup-handler-cb undef FXtAddCallback drop
      snd i channel-widgets 0 array-ref ( chn-grf )
      FXmNpopupHandlerCallback snd i popup-handler-cb undef FXtAddCallback drop
    then
  loop
;
set-current

: add-popups ( -- )
  doc" Adds context-sensitive popup menus to various parts of the interface."
  after-open-hook <'> add-popup add-hook!
  sounds each add-popup end-each
;
previous

hide
: change-color-col-cb ( col -- prc; w self -- )
  1 proc-create swap , 
 does> { w self -- }
  w self @ FXmChangeColor drop
;
set-current

: change-menu-color ( menu new-color -- )
  doc" Changes the color of MENU to NEW-COLOR.  \
NEW-COLOR can be the color name, an xm Pixel, a snd color, or a list of rgb values \
(as in Snd's make-color)."
  { menu new-color }
  new-color string? if			\ assuming X11 color names here
    main-widgets 1 array-ref { shell }
    shell FXtDisplay { dpy }
    dpy FDefaultScreen { scr }
    dpy scr FDefaultColormap { cmap }
    FXColor { col }
    dpy cmap new-color col col FXAllocNamedColor 0= if
      $" can't allocate %S" #( new-color ) string-format snd-error
    else
      col Fpixel
    then
  else
    new-color color? if
      new-color
    else
      new-color each end-each make-color
    then
  then ( color-pixel ) menu swap change-color-col-cb for-each-child
;
: change-selection-popup-color ( new-color -- )
  doc" Changes the selection popup menu's color: \"red\" change-selection-popup-color"
  selection-popup-menu swap change-menu-color
;
: change-graph-popup-color ( new-color -- )
  doc" Changes the time-domain popup menu's color: basic-color change-graph-popup-color"
  selection-popup-menu swap change-menu-color
;
: change-fft-popup-color ( new-color -- )
  doc" Changes the fft popup menu's color: #(0.5 0.5 0.5) change-fft-popup-color"
  fft-popup-menu swap change-menu-color
;
: change-edhist-popup-color ( new-color -- )
  doc" Changes the time-domain popup menu's color: basic-color change-graph-popup-color"
  edit-history-menu swap change-menu-color
;
previous

\ install all popups
add-popups

\ --- listener popup ---

hide
: identity-cb <{ snds -- lst }> snds ;
: edited-cb <{ snds -- lst }>
  snds each { snd }
    snd channels 0 ?do
      snd i edits 0 array-ref 0= if snds snd array-delete! to snds then
    loop
  end-each
  snds
;
: focused-cb    <{ snds -- lst }> snds length 1 > if snds else #() then ;
: list-play-cb  <{ snd -- val }>  snd play ;
: list-focus-cb <{ us -- val }>
  \ 5 == notebook-outer-pane
  main-widgets 5 array-ref FWidget? if
    us set-selected-sound
  else
    us sound-widgets 0 array-ref { pane }
    main-widgets 1 array-ref #( FXmNallowShellResize #f ) FXtVaSetValues drop
    sounds each ( them ) sound-widgets 0 array-ref FXtUnmanageChild drop end-each
    pane FXtManageChild drop
    main-widgets 1 array-ref #( FXmNallowShellResize auto-resize ) FXtVaSetValues
  then
;
: list-help-cb <{ w c info -- val }>
  listener-selection { selected }
  selected if
    selected undef snd-help { help }
    help if
      selected help undef undef help-dialog
    then
  then
;
: list-clear-cb <{ w c info -- val }> clear-listener ;
: listener-edit <{ w -- }>
  w FXtName { name }
  name "Help" string= if
    listener-selection { subject }
    subject if
      w $" Help on %S" #( subject ) string-format change-label
      w FXtManageChild drop
    else
      w FXtUnmanageChild drop
    then
  then
;
: listener-popup-cb ( menu -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { menu }
  FButtonPress info Fevent Ftype = if
    listener-values each { vals }
      vals array? if
	vals 0 array-ref { top-one }
	vals 1 array-ref { top-two }
	vals 2 array-ref { top-two-cascade }
	vals 3 array-ref #( sounds ) run-proc length { len }
	top-two FXtUnmanageChild drop
	top-two-cascade FXtUnmanageChild drop
	top-one if top-one FXtUnmanageChild drop then
	len 1 > if
	  top-two-cascade FXtManageChild drop
	  top-two FXtManageChild drop
	then
	top-one FWidget? len 1 = && if top-one FXtManageChild drop then
      then
    end-each
    menu <'> listener-edit for-each-child
    info menu Fset_menuToPost drop
  then
;

let: ( -- )
  main-widgets 4 array-ref dup unless
    drop
    #t show-listener drop
    #f set-show-listener drop
    main-widgets 4 array-ref
  then { parent }
  "listener-popup" parent
  #( #( $" Listener" 	   'label     #f           	     #f )
     #( $" sep"      	   'separator #f           	     #f )
     #( $" Play"     	   'cascade   <'> list-play-cb       <'> identity-cb #t )
     #( $" Help"           #f         <'> list-help-cb       #f )
     #( $" Open"     	   #f         <'> popen-cb           #f )
     #( $" Clear listener" #f         <'> list-clear-cb      #f )
     #( $" Close"    	   'cascade   <'> close-sound-extend <'> identity-cb #t )
     #( $" Save"     	   'cascade   <'> save-sound         <'> edited-cb   #t )
     #( $" Revert"   	   'cascade   <'> revert-sound       <'> edited-cb   #t )
     #( $" Focus"          'cascade   <'> list-focus-cb      <'> focused-cb  #f )
     #( $" sep"            'separator #f                     #f )
     #( $" Exit"           #f         <'> exit-cb            #f ) ) make-popup-menu { menu }
  parent FXmNpopupHandlerCallback menu listener-popup-cb undef FXtAddCallback drop
  menu
;let constant listener-popup-menu
set-current

: change-listener-popup-color ( new-color -- )
  doc" Changes the listener popup menu's color."
  listener-popup-menu swap change-menu-color
;
previous

\ popup.fs ends here
