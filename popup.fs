\ -*- snd-forth -*-
\ popup.fs -- popup.scm|rb --> popup.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Dec 23 00:28:28 CET 2005
\ Changed: Mon Sep 04 17:16:59 CEST 2006

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

require clm
require examp
require snd-xm

\ for prefs
: edhist-help-edits ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  $" Edit History Functions" _
  $" This popup menu gives access to the edit-list function handlers in Snd.  \
At any time you can backup in the edit list, 'save' the current trailing edits, make some \
new set of edits, then 'reapply' the saved edits.  The 'apply' choice gives access to all \
currently saved edit lists -- any such list can be applied to any channel.  'Clear' deletes \
all saved edit lists." _
  '( $" {edit lists}" $" {edit-list->function}" )
  '( $" extsnd.html#editlists" $" extsnd.html#editlist_to_function" )
  help-dialog drop
;

hide

\ --- make-simple-popdown-menu for fft-popup-menu ---
: popup-cascade-cb ( children xt -- proc; w c i self -- )
  lambda-create , , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self cell+ @ ( children ) self @ ( xt ) execute
;

: make-simple-popdown-menu ( label popdown-labels parent cascade-xt -- )
  { label popdown-labels parent cascade-xt }
  parent label '( FXmNbackground highlight-color ) undef FXmCreatePulldownMenu { top }
  label FxmCascadeButtonWidgetClass parent
  '( FXmNbackground highlight-color FXmNsubMenuId top ) undef FXtCreateManagedWidget { top-cascade }
  #() { children }
  popdown-labels proc? if
    \ edhist sends a proc to set TOP to edhist-widgets
    popdown-labels '( top ) run-proc drop
  else
    \ else arrays of '(name proc) lists
    popdown-labels each { poplab }
      poplab car FxmPushButtonWidgetClass top
      '( FXmNbackground highlight-color ) undef FXtCreateManagedWidget { child }
      child FXmNactivateCallback poplab cadr undef FXtAddCallback drop
      children child array-push drop
    end-each
  then
  cascade-xt if
    top-cascade FXmNcascadingCallback children cascade-xt popup-cascade-cb undef FXtAddCallback drop
  then
;

\ --- make-popdown-entry for listener-popup-menu ---
#() value listener-values
: collector-cb ( func collector -- proc; w c i self -- )
  lambda-create , , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { collector }
  self cell+ @ { func }
  sounds collector execute car func execute
;
: cas-cb ( func -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { func }
  w current-label 0 find-sound func execute
;
: popdown-cascade-cb ( func collector menu children -- proc; w c i self -- )
  lambda-create , , , , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { children }
  self cell+ @ { menu }
  self 2 cells + @ { collector }
  self 3 cells + @ { func }
  children each FXtUnmanageChild drop end-each
  sounds collector execute { snds }
  children length { clen }
  snds length { slen }
  clen slen < if
    slen clen ?do
      "" FxmPushButtonWidgetClass menu
      '( FXmNbackground highlight-color ) undef FXtCreateManagedWidget { child }
      child FXmNactivateCallback func cas-cb undef FXtAddCallback drop
      children child array-push drop
    loop
  then
  slen if
    children each { child }
      snds i list-ref { snd }
      child snd short-file-name change-label
      child FXtManageChild drop
    end-each
  then
;

: make-popdown-entry ( label parent func collector with-one -- values )
  { label parent func collector with-one }
  #f { widget }
  #() { children }
  with-one if
    label FxmPushButtonWidgetClass parent
    '( FXmNbackground highlight-color ) undef FXtCreateManagedWidget to widget
    widget FXmNactivateCallback func collector collector-cb undef FXtAddCallback drop
  then
  parent label '( FXmNbackground highlight-color ) undef FXmCreatePulldownMenu { menu }
  label FxmCascadeButtonWidgetClass parent
  '( FXmNbackground highlight-color FXmNsubMenuId menu ) undef FXtCreateManagedWidget { menu-cas }
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
  '( FXmNpopupEnabled FXmPOPUP_AUTOMATIC
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
	entry 3 array-ref ( xt ) make-simple-popdown-menu
      else				\ listener menu
	label menu
	entry 2 array-ref ( func )
	entry 3 array-ref ( collector )
	entry 4 array-ref ( with-one ) make-popdown-entry
      then
    else
      label class menu '( FXmNbackground highlight-color ) undef FXtCreateManagedWidget { wid }
      entry 2 array-ref if		\ cb: proc of 3 args or #f
	wid FXmNactivateCallback entry 2 array-ref undef FXtAddCallback drop
      then
      entry 3 array-ref if		\ func: xt of 1 arg or #f
	wid entry 3 array-ref execute
      then
    then
  end-each
  menu
;

\ --- selection popup ---

: sel-stop-play-cb ( vars -- proc; self -- )
  lambda-create , latestxt 0 make-proc
 does> ( self -- )
  @ { vars }
  vars 'stopping hash-ref if
    vars 'stopping #f hash-set!
    vars 'stop-widget hash-ref FWidget? if
      vars 'stop-widget hash-ref $" Play" _ change-label
    then
  then
;

: sel-play-cb ( vars -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self -- }
  self @ { vars }
  vars 'stopping hash-ref if
    vars 'stopping #f hash-set!
    w $" Play" _ change-label
    vars 'stopping1 hash-ref if
      vars 'stopping1 #f hash-set!
      vars 'stop-widget1 hash-ref $" Loop play" _ change-label
    then
    undef stop-playing drop
  else
    w $" Stop" _ change-label
    vars 'stop-widget w hash-set!
    vars 'stopping #t hash-set!
    #f undef play-selection drop
  then
;

: stop-playing-selection ( w vars -- )
  { w vars }
  vars 'stopping1 #f hash-set!
  w $" Loop play" _ change-label
  vars 'stopping hash-ref if
    vars 'stopping #f hash-set!
    vars 'stop-widget hash-ref $" Play" _ change-label
  then
;
: play-selection-again ( w vars -- proc; reason self -- )
  lambda-create , , latestxt 1 make-proc
 does> ( reason self -- )
  { reason self }
  self @ { vars }
  self cell+ @ { w }
  c-g? not
  reason 0= &&
  vars 'stopping1 hash-ref && if
    #f w vars recurse play-selection drop
  else
    w vars stop-playing-selection
  then
;
: sel-loop-cb ( vars -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self -- }
  self @ { vars }
  vars 'stopping1 hash-ref if
    w vars stop-playing-selection
    undef stop-playing drop
  else
    w $" Stop!" _ change-label
    vars 'stop-widget1 w hash-set!
    vars 'stopping1 #t hash-set!
    #f w vars play-selection-again play-selection drop
  then
;

: sel-del ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  delete-selection drop
;

: sel-zero ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  0.0 scale-selection-by drop
;

: as-one-edit-thunk ( selection -- proc; self -- )
  lambda-create , latestxt 0 make-proc
 does> ( self -- )
  @ { selection }
  selection car  { snd }
  selection cadr { chn }
  snd chn selection-position { beg }
  snd chn selection-frames   { len }
  beg 0> if 0 beg snd chn delete-samples drop then
  len snd chn #f frames < if len 1+ snd chn #f frames len - snd chn delete-samples drop then
;
: sel-crop ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  selection-members each { selection }
    selection as-one-edit-thunk "" as-one-edit drop
  end-each
;

: sel-save-as ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  save-selection-dialog drop
;

: sel-copy ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  snd-tempnam { new-file-name }
  new-file-name save-selection drop
  new-file-name open-sound drop
;

: sel-cut ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  snd-tempnam { new-file-name }
  new-file-name save-selection drop
  delete-selection drop
  new-file-name open-sound drop
;

: sel-marks ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  selection-members each { select }
    select car  { snd }
    select cadr { chn }
    snd chn selection-position { pos }
    snd chn selection-frames 1- { len }
    pos snd chn add-mark drop
    pos len + snd chn add-mark drop
  end-each
;

: sel-info ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f #f selection-position { beg }
  #f #f selection-frames   { len }
  $"     start: %d, %.3f\n" '( beg beg #f srate f/ ) string-format { str }
  $"       end: %d, %.3f\n" '( beg len + dup #f srate f/ ) string-format str swap << to str
  $"  duration: %d, %.3f\n" '( len len #f srate f/ ) string-format str swap << to str
  $"     chans: %d\n" selection-chans string-format str swap << to str
  $"    maxamp: %.3f\n" #f #f selection-maxamp string-format str swap << to str
  $" Selection Info" _ str info-dialog drop
;

\ choice 2 == selection
: sel-appcnt ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f 2 0 undef apply-controls drop
;

: sel-rescnt ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f reset-controls drop
;

: sel-unsel ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f #t set-selection-member? drop
;

: sel-rev ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  reverse-selection drop
;

: sel-mix ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f #f #f cursor mix-selection drop
;

: sel-invert ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  -1.0 scale-selection-by drop
;

let: ( -- menu )
  #{ 'stopping #f 'stopping1 #f 'stop-widget #f 'stop-widget1 #f } { vars }
  stop-playing-selection-hook vars sel-stop-play-cb add-hook!
  $" selection-popup" main-widgets caddr
  #( #( $" Selection"        _ 'label     #f               #f )
     #( $" sep"              _ 'separator #f               #f )
     #( $" Play"             _ #f         vars sel-play-cb #f )
     #( $" Loop play"        _ #f         vars sel-loop-cb #f )
     #( $" Delete"           _ #f         sel-del          #f )
     #( $" Zero"             _ #f         sel-zero         #f )
     #( $" Crop"             _ #f         sel-crop         #f )
     #( $" Save as"          _ #f         sel-save-as      #f )
     #( $" Copy->New"        _ #f         sel-copy         #f )
     #( $" Cut->New"         _ #f         sel-cut          #f )
     #( $" Snap marks"       _ #f         sel-marks        #f )
     #( $" Selection Info"   _ #f         sel-info         #f )
     #( $" Apply controls"   _ #f         sel-appcnt       #f )
     #( $" Reset controls"   _ #f         sel-rescnt       #f )
     #( $" Unselect"         _ #f         sel-unsel        #f )
     #( $" Reverse"          _ #f         sel-rev          #f )
     #( $" Mix"              _ #f         sel-mix          #f )
     #( $" Invert"           _ #f         sel-invert       #f ) ) make-popup-menu
;let value selection-popup-menu

\ --- time domain popup ---

#f value graph-popup-snd
#f value graph-popup-chn

: stop-playing-cb ( vars -- proc; snd self -- )
  lambda-create , latestxt 1 make-proc
 does> ( snd self -- )
  { snd self }
  self @ { vars }
  vars 'stopping hash-ref if
    vars 'stopping #f hash-set!
    vars 'stop-widget hash-ref dup if $" Play" _ change-label else drop then
  then
;

: play-cb ( vars -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { vars }
  vars 'stopping hash-ref if
    vars 'stopping #f hash-set!
    w $" Play" _ change-label
    undef stop-playing drop
  else
    w $" Stop" _ change-label
    vars 'stopping #t hash-set!
    0 graph-popup-snd undef undef undef undef undef undef play drop
  then
;
: stop-xt ( vars -- xt; widget self -- )
  lambda-create , latestxt
 does> ( widget self -- )
  { w self }
  self @ ( vars ) 'stop-widget w hash-set!
;

: pchan-cb ( vars -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { vars }
  vars 'stopping #t hash-set!
  vars 'stop-widget hash-ref $" Stop" _ change-label
  0 graph-popup-snd graph-popup-chn undef undef undef undef undef play drop
;

: pcur-cb ( vars -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { vars }
  vars 'stopping #t hash-set!
  vars 'stop-widget hash-ref $" Stop" _ change-label
  graph-popup-snd graph-popup-chn #f cursor
  graph-popup-snd undef undef undef undef undef undef play drop
;

: pprev-cb ( vars -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { vars }
  vars 'stopping #t hash-set!
  vars 'stop-widget hash-ref $" Stop" _ change-label
  0 graph-popup-snd graph-popup-chn #f #f
  graph-popup-snd graph-popup-chn edit-position 1-
  undef undef play drop
;

: porig-cb ( vars -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { vars }
  vars 'stopping #t hash-set!
  vars 'stop-widget hash-ref $" Stop" _ change-label
  0 graph-popup-snd graph-popup-chn #f #f 0 undef undef play drop
;

: pundo-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  1 graph-popup-snd graph-popup-chn undo drop
;

: predo-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  1 graph-popup-snd graph-popup-chn redo drop
;

: prev-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd revert-sound drop
;

: popen-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #t open-file-dialog drop
;

: psave-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd save-sound drop
;

: psaveas-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd select-sound drop
  save-sound-dialog drop
;

: pupdate-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd update-sound drop
;

: pclose-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd close-sound-extend
;

: pmixsel-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn #f cursor graph-popup-snd graph-popup-chn mix-selection drop
;

: pinssel-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn #f cursor graph-popup-snd graph-popup-chn insert-selection drop
;

: prepsel-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd { snd }
  graph-popup-chn { chn }
  snd chn #f cursor { beg }
  snd chn selection-frames { len }
  snd chn selection-position { sbeg }
  snd chn selection-member? not
  beg len + sbeg < ||
  beg sbeg len + > || if
    beg len snd chn #f delete-samples drop
    beg snd chn insert-selection drop
  else
    beg sbeg < if beg sbeg beg - snd chn #f delete-samples drop then
  then
;

: pselall-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn select-all drop
;

: punsel-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f #t set-selection-member? drop
;

: peqpan-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  equalize-panes drop
;

: papcnt-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f 0 0 undef apply-controls drop
;

: precnt-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #f reset-controls drop
;

: pinfo-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd { snd }
  graph-popup-chn { chn }
  snd chn #f frames { frms }
  snd srate { sr }
  $"    chans: %d, srate: %d\n"     '( snd channels sr ) string-format { str }
  $"   length: %.3f  (%d frames)\n" '( frms sr f/ frms ) string-format str swap << to str
  $"   format: %s [%s]\n"
  '( snd data-format mus-data-format-name snd header-type mus-header-type-name )
  string-format str swap << to str
  $"   maxamp: %s\n"       '( snd #t #f maxamp )   string-format str swap << to str
  snd comment empty? unless
    $"  comment: %S\n" '( snd comment )            string-format str swap << to str
  then
  snd file-name mus-sound-loop-info { loops }
  loops null? unless
    $"     loop: %s\n"     '( loops )              string-format str swap << to str
  then
  snd header-type mus-soundfont = if
    $"   sounds: %s\n"     '( snd soundfont-info ) string-format str swap << to str
  then
  snd sound-properties { props }
  props null? unless
    $" properties: %s\n"   '( props )              string-format str swap << to str
  then
  snd chn channel-properties to props
  props null? unless
    snd channels 0 ?do
      $"  chan %d properties: %s\n" '( i snd i channel-properties ) string-format str swap << to str
    loop
  then
  snd file-name $"  info" $+ str info-dialog drop
;

: paddmrk-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn #f cursor graph-popup-snd graph-popup-chn add-mark drop
;

: pdelmrk-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn #f marks { ms }
  ms null? unless
    ms length 1 = if
      ms car delete-mark drop
    else
      graph-popup-snd graph-popup-chn #f cursor { loc }
      ms car { id }
      loc id undef mark-sample - abs { cur-min }
      ms each { m }
	loc m undef mark-sample - abs { this-min }
	this-min cur-min < if
	  this-min to cur-min
	  m to id
	then
      end-each
      id delete-mark drop
    then
  then
;

: pdelamrk-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn delete-marks drop
;

: pnextmrk-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  [char] j 4 graph-popup-snd graph-popup-chn key drop \ C-j
;

: plastmrk-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  [char] - 4 graph-popup-snd graph-popup-chn key drop \ C--
  [char] j 4 graph-popup-snd graph-popup-chn key drop \ C-j
;

: exit-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  0 snd-exit drop
;

let: ( -- menu )
  #{ 'stopping #f 'stop-widget #f } { vars }
  stop-playing-hook vars stop-playing-cb add-hook!
  $" graph-popup" main-widgets caddr
  #( #( $" Snd"              'label     #f            #f )
     #( $" sep"              'separator #f            #f )
     #( $" Play"             _ #f         vars play-cb  vars stop-xt )
     #( $" Play channel"     _ #f         vars pchan-cb #f )
     #( $" Play from cursor" _ #f         vars pcur-cb  #f )
     #( $" Play previous"    _ #f         vars pprev-cb #f )
     #( $" Play original"    _ #f         vars porig-cb #f )
     #( $" Undo"             _ #f         pundo-cb      #f )
     #( $" Redo"             _ #f         predo-cb      #f )
     #( $" Revert"           _ #f         prev-cb       #f )
     #( $" Open"             _ #f         popen-cb      #f )
     #( $" Save"             _ #f         psave-cb      #f )
     #( $" Save as"          _ #f         psaveas-cb    #f )
     #( $" Update"           _ #f         pupdate-cb    #f )
     #( $" Close"            _ #f         pclose-cb     #f )
     #( $" Mix selection"    _ #f         pmixsel-cb    #f )
     #( $" Insert selection" _ #f         pinssel-cb    #f )
     #( $" Replace with selection" _ #f   prepsel-cb    #f )
     #( $" Select all"       _ #f         pselall-cb    #f )
     #( $" Unselect"         _ #f         punsel-cb     #f )
     #( $" Equalize panes"   _ #f         peqpan-cb     #f )
     #( $" Apply controls"   _ #f         papcnt-cb     #f )
     #( $" Reset controls"   _ #f         precnt-cb     #f )
     #( $" Info"             _ #f         pinfo-cb      #f )
     #( $" Add mark"         _ #f         paddmrk-cb    #f )
     #( $" Delete mark"      _ #f         pdelmrk-cb    #f )
     #( $" Delete all marks" _ #f         pdelamrk-cb   #f )
     #( $" To next mark"     _ #f         pnextmrk-cb   #f )
     #( $" To last mark"     _ #f         plastmrk-cb   #f )
     #( $" sep"                'separator #f            #f )
     #( $" Exit"             _ #f         exit-cb       #f ) ) make-popup-menu
;let value graph-popup-menu

: graph-popup-xt ( snd chn -- xt; widget self -- )
  lambda-create , , latestxt
 does> ( widget self -- )
  { w self }
  self @ { chn }
  self cell+ @ { snd }
  snd chn edits { eds }
  w FXtName { name }
  name $" Snd" string= if
    snd channels 1 > if
      $" %s[%d]" '( snd short-file-name chn ) string-format w swap change-label
    else
      w snd short-file-name change-label
    then
  else
    name $" Save"          _ string=
    name $" Undo"          _ string= ||
    name $" Revert"        _ string= ||
    name $" Play previous" _ string= || if
      w eds car 0> if FXtManageChild else FXtUnmanageChild then drop
    else
      name $" Play channel" _ string= if
	w snd channels 1 > if FXtManageChild else FXtUnmanageChild then drop
      else
	name $" Equalize panes" _ string= if
	  w snd channels 1 > sounds length 1 > || if FXtManageChild else FXtUnmanageChild then drop
	else
	  name $" Redo" _ string= if
	    w eds cadr 0> if FXtManageChild else FXtUnmanageChild then drop
	  else
	    name $" Mix selection"          _ string=
	    name $" Insert selection"       _ string= ||
	    name $" Unselect"               _ string= ||
	    name $" Replace with selection" _ string= || if
	      w selection? if FXtManageChild else FXtUnmanageChild then drop
	    else
	      name $" Play from cursor" _ string= if
		w snd chn #f cursor 0> if FXtManageChild else FXtUnmanageChild then drop
	      else
		name $" Play original" _ string= if
		  w eds car 1 > if FXtManageChild else FXtUnmanageChild then drop
		else
		  name $" Delete mark"       _ string=
		  name $" Delete all marks"  _ string= ||
		  name $" To next mark"      _ string= ||
		  name $" To last mark"      _ string= || if
		    w snd chn #f marks null? unless FXtManageChild else FXtUnmanageChild then drop
		  then
		then
	      then
	    then
	  then
	then
      then
    then
  then
;

: edit-graph-popup-menu { snd chn -- }
  doc" ( snd chn -- )  \
Hides otiose entries, relabel others to reflect current state of SND and CHN."
  graph-popup-menu snd chn graph-popup-xt for-each-child
;

\ --- fft popup ---

: choose-chan ( -- chn )
  graph-popup-snd channel-style channels-separate = if graph-popup-chn else #t then
;

: fft-peaks-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn show-transform-peaks not
  graph-popup-snd choose-chan set-show-transform-peaks drop
;

: fft-db-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn fft-log-magnitude not
  graph-popup-snd choose-chan set-fft-log-magnitude drop
;

: fft-frq-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn fft-log-frequency not
  graph-popup-snd choose-chan set-fft-log-frequency drop
;

: fft-norm-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  graph-popup-snd graph-popup-chn transform-normalization dont-normalize = if
    normalize-by-channel graph-popup-snd choose-chan set-transform-normalization drop
  else
    dont-normalize       graph-popup-snd choose-chan set-transform-normalization drop
  then
;

: grp-lst-cb ( val -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ ( val ) graph-popup-snd choose-chan set-transform-graph-type drop
;
: grp-labs ( -- ary )
  #( '( $" once"        _ graph-once           grp-lst-cb )
     '( $" sonogram"    _ graph-as-sonogram    grp-lst-cb )
     '( $" spectrogram" _ graph-as-spectrogram grp-lst-cb ) )
;
lambda: ( lst -- )
  each ( child )
    graph-popup-snd graph-popup-chn transform-graph-type i = if #f else #t then FXtSetSensitive drop
  end-each
; value grp-set

#( 16 32 64 128 256 512 1024 2048 4096 8192 16384 65536 262144 1048576 ) value fft-siz-sizes
: siz-lst-cb ( val -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ ( val ) graph-popup-snd choose-chan set-transform-size drop
;
: siz-labs ( -- ary )
  save-stack { stack }
  fft-siz-sizes map '( *key* object->string *key* siz-lst-cb ) end-map { ary }
  stack restore-stack
  ary
;
lambda: ( lst -- )
  { lst }
  save-stack { stack }
  lst each ( child )
    fft-siz-sizes i array-ref { siz }
    graph-popup-snd graph-popup-chn transform-size siz <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
; value siz-set

#( rectangular-window
   hann-window
   welch-window
   parzen-window
   bartlett-window
   hamming-window
   blackman2-window 
   blackman3-window
   blackman4-window
   exponential-window
   riemann-window
   kaiser-window
   cauchy-window
   poisson-window
   gaussian-window
   tukey-window
   dolph-chebyshev-window
   hann-poisson-window
   connes-window ) value fft-win-windows
: win-lst-cb ( val -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ ( val ) graph-popup-snd choose-chan set-fft-window drop
;
: win-labs ( -- ary )
  #( $" Rectangular"
     $" Hann"
     $" Welch"
     $" Parzen"
     $" Bartlett"
     $" Hamming"
     $" Blackman2"
     $" Blackman3"
     $" Blackman4"
     $" Exponential"
     $" Riemann"
     $" Kaiser"
     $" Cauchy"
     $" Poisson"
     $" Gaussian"
     $" Tukey"
     $" Dolph-Chebyshev"
     $" Hann-Poisson"
     $" Connes" ) map '( *key* fft-win-windows i array-ref win-lst-cb ) end-map
;
lambda: ( lst -- )
  { lst }
  save-stack { stack }
  lst each ( child )
    fft-win-windows i array-ref { win }
    graph-popup-snd graph-popup-chn fft-window win <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
; value win-set

#( fourier-transform wavelet-transform autocorrelation cepstrum walsh-transform haar-transform )
value fft-trn-transform
: trn-lst-cb ( val -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ ( val ) graph-popup-snd choose-chan set-transform-type drop
;
: trn-labs ( -- ary )
  save-stack { stack }
  #( $" Fourier" $" Wavelet" $" Autocorrealte" $" Cepstrum" $" Walsh" $" Haar" ) { names }
  names map '( *key* fft-trn-transform i array-ref trn-lst-cb ) end-map { ary }
  stack restore-stack
  ary
;
lambda: ( lst -- )
  { lst }
  save-stack { stack }
  lst each ( child )
    fft-trn-transform i array-ref { trn }
    graph-popup-snd graph-popup-chn transform-type trn <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
; value trn-set

: typ-lst-cb ( val -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ ( val ) graph-popup-snd choose-chan set-wavelet-type drop
;
: typ-labs ( -- ary )
  #( $" doub4"
     $" doub6"
     $" doub8"
     $" doub10"
     $" doub12"
     $" doub14"
     $" doub16"
     $" doub18"
     $" doub20"
     $" battle_lemarie"
     $" burt_adelson"
     $" beylkin"
     $" coif2"
     $" coif4"
     $" coif6"
     $" sym2"
     $" sym3"
     $" sym4"
     $" sym5"
     $" sym6" ) map '( *key* i typ-lst-cb ) end-map
;
lambda: ( lst -- )
  { lst }
  save-stack { stack }
  lst each ( child )
    graph-popup-snd graph-popup-chn wavelet-type i <> if #t else #f then FXtSetSensitive drop
  end-each
  stack restore-stack
; value typ-set

: fft-color ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  color-dialog drop
;

: fft-orient ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  #t orientation-dialog drop
;

let: ( -- menu )
  $" fft-popup" main-widgets caddr
  #( #( $" Transform"        _ 'label     #f            #f )
     #( $" sep"                'separator #f            #f )
     #( $" Peaks"            _ #f         fft-peaks-cb  #f )
     #( $" dB"               _ #f         fft-db-cb     #f )
     #( $" Log freq"         _ #f         fft-frq-cb    #f )
     #( $" Normalize"        _ #f         fft-norm-cb   #f )
     #( $" Graph type"       _ 'cascade   grp-labs      grp-set )
     #( $" Size"             _ 'cascade   siz-labs      siz-set )
     #( $" Window"           _ 'cascade   win-labs      win-set )
     #( $" Transform type"   _ 'cascade   trn-labs      trn-set )
     #( $" Wavelet type"     _ 'cascade   typ-labs      typ-set )
     #( $" Color"            _ #f         fft-color     #f )
     #( $" Orientation"      _ #f         fft-orient    #f ) ) make-popup-menu
;let value fft-popup-menu

: fft-popup-xt ( snd chn -- xt; widget self -- )
  lambda-create , , latestxt
 does> ( widget self -- )
  { w self }
  self @ { chn }
  self cell+ @ { snd }
  w FXtName { name }
  name $" Peaks" _ string= if
    w snd chn show-transform-peaks if $" No peaks" _ else $" Peaks" _ then change-label
  else
    name $" dB" _ string= if
      w snd chn fft-log-magnitude if $" Linear" _ else $" dB" _ then change-label
    else
      name $" Log freq" _ string= if
	w snd chn fft-log-frequency if $" Linear freq" _ else $" Log freq" _ then change-label
      then
    then
  then
;

: edit-fft-popup-menu { snd chn -- wid }
  doc" ( snd chn -- wid )  Changes the fft-related popup menu to reflect the state of SND and CHN."
  save-stack { stack }
  fft-popup-menu snd chn fft-popup-xt for-each-child
  stack restore-stack
;
set-current
previous

\ --- edit history popup ---

1 $" edhist-save-edits calls it with PROC as its only argument." create-hook edhist-save-hook

hide

'() value edhist-funcs
'() value edhist-widgets
#f  value edhist-snd
#f  value edhist-chn

: edhist-clear-edits ( -- proc; w c i self -- #f )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- #f )
  2drop 2drop
  '() to edhist-funcs
  #f
;

: edhist-save-edits ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- #f )
  2drop 2drop
  edhist-funcs '( edhist-snd edhist-chn ) list-assoc-ref { old-proc }
  edhist-snd edhist-chn edits { cur-edits }
  edhist-snd edhist-chn cur-edits car 1+ 0 cur-edits each + end-each edit-list->function { proc }
  edhist-save-hook '( proc ) run-hook drop
  old-proc proc? if
    edhist-funcs '( edhist-snd edhist-chn ) proc list-assoc-set!
  else
    '( edhist-snd edhist-chn ) proc edhist-funcs acons
  then to edhist-funcs
;

: edhist-reapply-edits ( -- proc; w c i self -- val )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- val )
  2drop 2drop
  edhist-funcs '( edhist-snd edhist-chn ) list-assoc-ref '( edhist-snd edhist-chn ) run-proc
;

: edhist-set-wid ( -- proc; widget self -- )
  lambda-create latestxt 1 make-proc
 does> ( widget self -- )
  { w self }
  w nil cons to edhist-widgets
;
: edhist-apply ( w c i -- )
  { w c i }
  edhist-funcs c range? if
    edhist-funcs c list-ref cdr ( proc ) '( edhist-snd edhist-chn ) run-proc
  then
;
lambda: ( lst -- ) drop
  edhist-widgets car { parent }
  edhist-widgets cdr { wids }
  edhist-funcs each car { label }
    nil { button }
    wids null? if
      $" wid" FxmPushButtonWidgetClass parent
      '( FXmNbackground highlight-color ) undef FXtCreateManagedWidget to button
      edhist-widgets '( button ) 2 list-append to edhist-widgets
      button FXmNactivateCallback ['] edhist-apply 3 make-proc i FXtAddCallback drop
    else
      wids car to button
      wids cdr to wids
      button FXtManageChild drop
    then
    label list? if
      \ label: '(snd chn)
      button  $" %s[%s]" '( label car short-file-name label cadr ) string-format change-label
    else
      \ label: "file-name[chn]"
      button label change-label
    then
    button '( FXmNuserData i ) FXtVaSetValues drop
  end-each
  wids each FXtUnmanageChild drop end-each
; value edhist-apply-edits

: edhist-close-hook-cb ( snd -- )
  { snd }
  snd channels 0 ?do
    edhist-funcs '( snd i ) list-assoc { old-val }
    old-val cons? if
      old-val  $" %s[%d]" '( snd short-file-name i ) string-format set-car! drop
    then
  loop
;

let: ( -- menu )
  close-hook ['] edhist-close-hook-cb 1 make-proc add-hook!
  $" edhist-popup" main-widgets caddr
  #( #( $" Edits"   _ 'label     #f      	            #f )
     #( $" sep"       'separator #f      	            #f )
     #( $" Save"    _ #f         edhist-save-edits    #f )
     #( $" Reapply" _ #f         edhist-reapply-edits #f )
     #( $" Apply"   _ 'cascade   edhist-set-wid       edhist-apply-edits  )
     #( $" Clear"   _ #f         edhist-clear-edits   #f )
     #( $" sep"       'separator #f                   #f )
     #( $" Help"    _ #f         edhist-help-edits    #f ) ) make-popup-menu
;let value edit-history-menu

: edhist-popup-xt ( snd chn -- xt; widget self -- )
  lambda-create , , latestxt
 does> ( widget self -- )
  { w self }
  self @ { chn }
  self cell+ @ { snd }
  w FXtName { name }
  name $" Clear" _ string= name $" Apply" _ string= || if
    w edhist-funcs empty? if #f else #t then FXtSetSensitive drop
  else
    name $" Save" _ string= if
      w 0 snd chn edits each + end-each 0> if #t else #f then FXtSetSensitive drop
    else
      name $" Reapply" _ string= if
	w edhist-funcs '( snd chn ) list-assoc-ref if #t else #f then FXtSetSensitive drop
      then
    then
  then
;

: edit-edhist-popup-menu ( snd chn -- wid )
  { snd chn }
  save-stack { stack }
  edit-history-menu snd chn edhist-popup-xt for-each-child
  stack restore-stack
;

\  --- activate the above menus ---

: edhist-popup-handler-cb ( snd chn -- proc; w c i self -- )
  lambda-create , , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
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

: popup-handler-cb ( snd chn -- proc; w c i self -- )
  lambda-create , , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { chn }
  self cell+ @ { snd }
  info Fevent { ev }
  ev Fx_root w 0 0 FXtTranslateCoords car - { xe }
  FButtonPress ev Ftype = if
    snd to graph-popup-snd
    chn to graph-popup-chn
    snd channel-style channels-combined = if
      ev Fy { ye }
      #t				\ flag
      snd channels 0 ?do
	ye snd i undef axis-info 14 list-ref < if
	  i 1- to graph-popup-chn
	  not				\ toggle flag
	  leave
	then
      loop ( flag ) if snd channels 1- to graph-popup-chn then
    then
    snd chn transform-graph? if snd chn transform-graph axis-info else #f then { fax }
    snd chn lisp-graph?      if snd chn lisp-graph      axis-info else #f then { lax }
    fax if
      xe fax 10 list-ref >=
      xe fax 12 list-ref <= && if
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
	xe lax 10 list-ref >=
	xe lax 12 list-ref <= && if
	  \ in lisp
	  #f
	else
	  #t
	then
      else
	#t
      then if
	selection? if
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

#() value popups

: add-popup ( snd -- )
  { snd }
  snd channels 0 ?do
    popups '( snd i ) array-member? unless
      popups '( snd i ) array-push drop
      snd i channel-widgets 7 list-ref  ( chn-edhist )
      FXmNpopupHandlerCallback snd i edhist-popup-handler-cb undef FXtAddCallback drop
      snd i channel-widgets car ( chn-grf )
      FXmNpopupHandlerCallback snd i popup-handler-cb undef FXtAddCallback drop
    then
  loop
;

set-current

: add-popups ( -- )
  doc" ( -- )  Adds context-sensitive popup menus to various parts of the interface."
  after-open-hook ['] add-popup 1 make-proc add-hook!
  sounds each add-popup end-each
;

previous

hide

: change-color-xt ( col -- xt; w self -- )
  lambda-create , latestxt
 does> ( w self -- )
  { w self }
  w self @ FXmChangeColor drop
;

set-current

: change-menu-color { menu new-color -- }
  doc" ( menu new-color -- )  Changes the color of MENU to NEW-COLOR.  \
NEW-COLOR can be the color name, an xm Pixel, a snd color, or a list of rgb values \
(as in Snd's make-color)."
  new-color string? if			\ assuming X11 color names here
    main-widgets cadr { shell }
    shell FXtDisplay { dpy }
    dpy FDefaultScreen { scr }
    dpy scr FDefaultColormap { cmap }
    FXColor { col }
    dpy cmap new-color col col FXAllocNamedColor 0= if
      $" can't allocate %S" _ '( new-color ) string-format snd-error
    else
      col Fpixel
    then
  else
    new-color color? if
      new-color
    else
      new-color each end-each make-color
    then
  then ( color-pixel )
  menu swap change-color-xt for-each-child
;

: change-selection-popup-color ( new-color -- )
  doc" ( new-color -- )  \
Changes the selection popup menu's color: \"red\" change-selection-popup-color"
  selection-popup-menu swap change-menu-color
;

: change-graph-popup-color ( new-color -- )
  doc" ( new-color -- )  \
Changes the time-domain popup menu's color: basic-color change-graph-popup-color"
  selection-popup-menu swap change-menu-color
;

: change-fft-popup-color ( new-color -- )
  doc" ( new-color -- )  \
Changes the fft popup menu's color: '(0.5 0.5 0.5) change-fft-popup-color"
  fft-popup-menu swap change-menu-color
;

: change-edhist-popup-color ( new-color -- )
  doc" ( new-color -- )  \
Changes the time-domain popup menu's color: basic-color change-graph-popup-color"
  edit-history-menu swap change-menu-color
;

previous

\ install all popups
add-popups

\ --- listener popup ---

hide

lambda: ( -- ) noop ; value identity-xt

lambda: ( snds -- lst )
  { snds }
  snds each { snd }
    snd channels 0 ?do
      snd i edits car 0= if snds snd list-delete to snds then
    loop
  end-each
  snds
; value edited-xt

lambda: ( snds -- lst ) { snds } snds length 1 > if snds else '() then ; value focused-xt

lambda: ( snd -- )
  { snd }
  0 snd undef undef undef undef undef undef play drop
; value list-play-cb

lambda: ( us -- )
  { us }
  \ 5 == notebook-outer-pane
  main-widgets 5 list-ref FWidget? if
    us set-selected-sound drop
  else
    us sound-widgets car { pane }
    main-widgets cadr '( FXmNallowShellResize #f ) FXtVaSetValues drop
    sounds each ( them ) sound-widgets car FXtUnmanageChild drop end-each
    pane FXtManageChild drop
    main-widgets cadr '( FXmNallowShellResize auto-resize ) FXtVaSetValues drop
  then
; value list-focus-cb

: list-help-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  listener-selection { selected }
  selected if
    selected undef snd-help { help }
    help if
      selected help undef undef help-dialog drop
    then
  then
;

: list-clear-cb ( -- proc; w c i self -- )
  lambda-create latestxt 3 make-proc
 does> ( w c i self -- )
  2drop 2drop
  clear-listener drop
;

: listener-edit ( widget -- )
  { w }
  w FXtName { name }
  name $" Equalize panes" _ string= if
    w sounds length 1 > if FXtManageChild else FXtUnmanageChild then drop
  else
    name $" Help" _ string= if
      listener-selection { subject }
      subject if
	w $" Help on %S" _ '( subject ) string-format change-label
	w FXtManageChild drop
      else
	w FXtUnmanageChild drop
      then
    then
  then
;

: listener-popup-cb ( menu -- proc; w c i self -- )
  lambda-create , latestxt 3 make-proc
 does> ( w c i self -- )
  { w c info self }
  self @ { menu }
  FButtonPress info Fevent Ftype = if
    listener-values each { vals }
      vals array? if
	vals 0 array-ref { top-one }
	vals 1 array-ref { top-two }
	vals 2 array-ref { top-two-cascade }
	sounds vals 3 array-ref execute length { len }
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
    menu ['] listener-edit for-each-child
    info menu Fset_menuToPost drop
  then
;

let: ( -- )
  main-widgets 4 list-ref dup unless
    drop
    #t show-listener drop
    #f set-show-listener drop
    main-widgets 4 list-ref
  then { parent }
  $" listener-popup" parent
  #( #( $" Listener" 	   _ 'label     #f           	       #f )
     #( $" sep"      	     'separator #f           	       #f )
     #( $" Play"     	   _ 'cascade   list-play-cb           identity-xt #t )
     #( $" Help"           _ #f         list-help-cb           #f )
     #( $" Open"     	   _ #f         popen-cb               #f )
     #( $" Clear listener" _ #f         list-clear-cb          #f )
     #( $" Close"    	   _ 'cascade   ['] close-sound-extend identity-xt #t )
     #( $" Save"     	   _ 'cascade   ['] save-sound         edited-xt   #t )
     #( $" Revert"   	   _ 'cascade   ['] revert-sound       edited-xt   #t )
     #( $" Equalize panes" _ #f         peqpan-cb              #f )
     #( $" Focus"          _ 'cascade   list-focus-cb          focused-xt  #f )
     #( $" sep"              'separator #f                     #f )
     #( $" Exit"           _ #f         exit-cb                #f ) ) make-popup-menu { menu }
  parent FXmNpopupHandlerCallback menu listener-popup-cb undef FXtAddCallback drop
  menu
;let value listener-popup-menu

set-current

: change-listener-popup-color ( new-color -- )
  doc" ( new-color -- )  Changes the listener popup menu's color."
  listener-popup-menu swap change-menu-color
;

previous

\ popup.fs ends here
