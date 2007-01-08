\ -*- snd-forth -*-
\ effects.fs -- effects.scm -> effects.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Sun Oct 16 23:04:30 CEST 2005
\ Changed: Sun Jan 07 18:54:04 CET 2007

\ Commentary:
\
\ make-menu                      ( name parent args -- gen )
\ menu-entry                     ( gen prc disp-prc -- )
\ make-main-menu                 ( name -- widget )
\ add-to-effects-menu            ( name prc -- )
\
\ effects-squelch-channel        ( amount gate-size :optional snd chn -- )
\ make-gain-dialog               ( name -- prc1 prc2; child self -- prc; self -- )
\ make-normalize-dialog          ( name -- prc1 prc2; child self -- prc; self -- )
\ make-gate-dialog               ( name -- prc1 prc2; child self -- prc; self -- )
\ 
\ effects-echo                   ( input-samps delay-time echo-amount :optional beg dur snd chn -- )
\ effects-flecho                 ( scaler secs input-samps :optional beg dur snd chn -- )
\ effects-zecho                  ( scaler secs freq amp input-samps :optional beg dur snd chn -- )
\ make-echo-dialog               ( name -- prc1 prc2; child self -- prc; self -- )
\ make-flecho-dialog             ( name -- prc1 prc2; child self -- prc; self -- )
\ make-zecho-dialog              ( name -- prc1 prc2; child self -- prc; self -- )
\
\ effects-bbp                    ( freq bw :optional beg dur snd chn -- res )
\ effects-bbr                    ( freq bw :optional beg dur snd chn -- res )
\ effects-bhp                    ( freq :optional beg dur snd chn -- res )
\ effects-blp                    ( freq :optional beg dur snd chn -- res )
\ effects-comb-filter            ( scaler size :optional beg dur snd chn -- res )
\ effects-comb-chord             ( scaler size amp :optional beg dur snd chn -- res )
\ effects-moog                   ( freq Q :optional beg dur snd chn -- res )
\ moog                           ( freq Q -- prc; inval self -- res )
\ make-band-pass-dialog          ( name -- prc1 prc2; child self -- prc; self -- )
\ make-notch-dialog              ( name -- prc1 prc2; child self -- prc; self -- )
\ make-high-pass-dialog          ( name -- prc1 prc2; child self -- prc; self -- )
\ make-low-pass-dialog           ( name -- prc1 prc2; child self -- prc; self -- )
\ make-comb-dialog               ( name -- prc1 prc2; child self -- prc; self -- )
\ make-comb-chord-dialog         ( name -- prc1 prc2; child self -- prc; self -- )
\ make-moog-dialog               ( name -- prc1 prc2; child self -- prc; self -- )
\
\ make-adsat-dialog              ( name -- prc1 prc2; child self -- prc; self -- )
\ make-src-dialog                ( name -- prc1 prc2; child self -- prc; self -- )
\ make-expsrc-dialog             ( name -- prc1 prc2; child self -- prc; self -- )
\ make-src-timevar-dialog        ( name -- prc1 prc2; child self -- prc; self -- )
\
\ effects-am 			 ( freq en :optional beg dur snd chn -- res )
\ effects-rm 			 ( freq en :optional beg dur snd chn -- res )
\ make-am-effect-dialog          ( name -- prc1 prc2; child self -- prc; self -- )
\ make-rm-effect-dialog          ( name -- prc1 prc2; child self -- prc; self -- )
\
\ effects-jc-reverb              ( samps volume -- prc; inval self -- res )
\ effects-jc-reverb-1            ( volume :optional beg dur snd chn -- res )
\ effects-cnv                    ( snd0 amp snd chn -- res )
\ make-reverb-dialog             ( name -- prc1 prc2; child self -- prc; self -- )
\ make-jc-reverb-dialog          ( name -- prc1 prc2; child self -- prc; self -- )
\ make-convolve-dialog           ( name -- prc1 prc2; child self -- prc; self -- )
\
\ effects-position-sound         ( mono-snd pos :optional snd chn -- res )
\ effects-place-sound            ( mono-snd stereo-snd pan-env -- res )
\ effects-flange                 ( amount speed time :optional beg dur snd chn -- res )
\ effects-cross-synthesis        ( snd amp fftsize r -- prc; inval self -- res )
\ effects-cross-synthesis-1      ( cross-snd amp fftsize r :optional beg dur snd chn -- res )
\ effects-fp                     ( srf amp freq :optional beg dur snd chn -- vct )
\ effects-hello-dentist          ( freq amp :optional beg dur snd chn -- res )
\ make-place-sound-dialog        ( name -- prc1 prc2; child self -- prc; self -- )
\ make-silence-dialog            ( name -- prc1 prc2; child self -- prc; self -- )
\ make-contrast-dialog     	 ( name -- prc1 prc2; child self -- prc; self -- )
\ make-cross-synth-dialog  	 ( name -- prc1 prc2; child self -- prc; self -- )
\ make-flange-dialog       	 ( name -- prc1 prc2; child self -- prc; self -- )
\ make-random-phase-dialog 	 ( name -- prc1 prc2; child self -- prc; self -- )
\ make-robotize-dialog     	 ( name -- prc1 prc2; child self -- prc; self -- )
\ make-rubber-dialog 		 ( name -- prc1 prc2; child self -- prc; self -- )
\ make-wobble-dialog 		 ( name -- prc1 prc2; child self -- prc; self -- )
\
\ make-effects-menu              ( -- widget )
\
\ effects-remove-clicks          ( :optional snd chn -- res )
\ effects-remove-dc              ( :optional snd chn -- res )
\ effects-compand                ( :optional snd chn -- res )

require clm
require examp
require env
require dsp
require xm-enved
require snd-xm
require rubber

\ === SND MENU ===

-1 value effects-menu			\ for prefs
#f value use-combo-box-for-fft-size

hide
: cascade-cb <{ w c i -- }> c each '() run-proc drop end-each ;

struct
  cell% field menu-parent
  cell% field menu-name
  cell% field menu-menu
  cell% field menu-args
  cell% field menu-children
  cell% field menu-display-cb
end-struct snd-menu%

: menu-parent@ 	 ( gen -- wid  ) menu-parent @ ;
: menu-name@   	 ( gen -- name ) menu-name @ ;
: menu-menu@   	 ( gen -- wid  ) menu-menu @ ;
: menu-args@   	 ( gen -- args ) menu-args @ ;
: menu-children@ ( gen -- ary  ) menu-children @ ;
: menu-display   ( gen -- )      menu-display-cb @ '() run-proc drop ;
set-current

: make-menu ( name parent args -- gen )
  { name parent args }
  snd-menu% %alloc { mn }
  parent name args undef FXmCreatePulldownMenu { menu }
  #() { lst }
  name FxmCascadeButtonWidgetClass parent
  '( FXmNsubMenuId  menu FXmNbackground basic-color ) undef FXtCreateManagedWidget ( cascade )
  FXmNcascadingCallback ['] cascade-cb lst FXtAddCallback drop
  parent mn menu-parent !
  name   mn menu-name !
  menu   mn menu-menu !
  args   mn menu-args !
  lst    mn menu-children !
  mn
;

' noop 0 make-proc constant effects-noop

: menu-entry ( gen prc disp-prc -- )
  { gen prc disp-prc }
  gen menu-name@ FxmPushButtonWidgetClass gen menu-menu@
  gen menu-args@ undef FXtCreateManagedWidget { child }
  child FXmNactivateCallback prc undef FXtAddCallback drop
  gen menu-children@  disp-prc '( child ) run-proc  array-push drop
;
: make-main-menu ( name -- widget ) effects-noop add-to-main-menu dup to effects-menu main-menu ;
: add-to-effects-menu ( name prc -- ) effects-menu -rot undef add-to-menu drop ;
previous

hide
: unmanage-cb <{ w c i -- }> c FXtUnmanageChild drop ;

: make-effect-dialog { label ok-prc help-prc reset-prc -- dialog }
  "Dismiss" _ FXmFONTLIST_DEFAULT_TAG FXmStringCreate { xdismiss }
  "Help"    _ FXmFONTLIST_DEFAULT_TAG FXmStringCreate { xhelp }
  "DoIt"    _ FXmFONTLIST_DEFAULT_TAG FXmStringCreate { xok }
  label     _ FXmFONTLIST_DEFAULT_TAG FXmStringCreate { titlestr }
  main-widgets cadr label
  '( FXmNcancelLabelString xdismiss
     FXmNhelpLabelString   xhelp
     FXmNokLabelString     xok
     FXmNautoUnmanage      #f
     FXmNdialogTitle       titlestr
     FXmNresizePolicy      FXmRESIZE_GROW
     FXmNnoResize          #f
     FXmNbackground        basic-color
     FXmNtransient         #f ) undef FXmCreateTemplateDialog { new-dialog }
  #( '( FXmDIALOG_HELP_BUTTON   help-button-color )
     '( FXmDIALOG_CANCEL_BUTTON quit-button-color )
     '( FXmDIALOG_OK_BUTTON     doit-button-color ) ) each { lst }
    new-dialog lst car ( button ) FXmMessageBoxGetChild ( widget )
    '( FXmNarmColor pushed-button-color FXmNbackground lst cadr ) FXtVaSetValues drop
  end-each
  new-dialog FXmNcancelCallback ['] unmanage-cb new-dialog FXtAddCallback drop
  new-dialog FXmNhelpCallback   help-prc        undef      FXtAddCallback drop
  new-dialog FXmNokCallback     ok-prc          undef      FXtAddCallback drop
  reset-prc if
    "Reset" _ FxmPushButtonWidgetClass new-dialog
    '( FXmNbackground      reset-button-color
       FXmNforeground      black-pixel
       FXmNarmColor        pushed-button-color ) undef FXtCreateManagedWidget ( reset-button )
    FXmNactivateCallback reset-prc undef FXtAddCallback drop
  then
  xhelp    FXmStringFree drop
  xok      FXmStringFree drop
  xdismiss FXmStringFree drop
  titlestr FXmStringFree drop
  new-dialog
;

\ log scaler widget

500 value log-scale-ticks

: scale-log->linear ( lo val hi -- lin )
  { lo val hi }
  2.0 flog { log2 }
  lo 1.0 fmax flog log2 f/ { log-lo }
  hi flog          log2 f/ { log-hi }
  val flog log2 f/   log-lo f-  log-hi log-lo f-  f/ log-scale-ticks f* floor f>s
;

: scale-linear->log ( lo val hi -- log )
  { lo val hi }
  2.0 flog { log2 }
  lo 1.0 fmax flog log2 f/ { log-lo }
  hi flog          log2 f/ { log-hi }
  2.0  log-lo val log-scale-ticks f/ log-hi log-lo f- f* f+  f**
;

: scale-log-label ( lo val hi -- str ) scale-linear->log "%.2f" swap 1 >list string-format ;

: scale-log-cb <{ w c info -- }>
  c car   { label }
  c cadr  { low }
  c caddr { high }
  label  low info Fvalue high scale-log-label  change-label
;

: create-log-scale-widget { parent title low init high cb -- scale-label-list }
  "%.2f" '( init ) string-format FxmLabelWidgetClass parent
  '( FXmNbackground    basic-color ) undef FXtCreateManagedWidget { label }
  "scale" FxmScaleWidgetClass parent
  '( FXmNorientation   FXmHORIZONTAL
     FXmNshowValue     #f
     FXmNminimum       0
     FXmNmaximum       log-scale-ticks
     FXmNvalue         low init high scale-log->linear
     FXmNdecimalPoints 0
     FXmNtitleString   title
     FXmNbackground    basic-color ) undef FXtCreateManagedWidget { scale }
  '( label low high ) { data }
  scale FXmNvalueChangedCallback ['] scale-log-cb data  FXtAddCallback drop
  scale FXmNvalueChangedCallback cb               undef FXtAddCallback drop
  scale FXmNdragCallback         ['] scale-log-cb data  FXtAddCallback drop
  scale FXmNdragCallback         cb               undef FXtAddCallback drop
  '( scale label )
;

\ semitone scaler widget

24 value semi-range

: semi-scale-label ( val -- str ) $" semitones: %s" _ swap semi-range - 1 >list string-format ;

: semitones->ratio ( val -- r )
  2.0 swap 12.0 f/ f**
;

: ratio->semitones ( ratio -- n )
  12.0 swap flog 2.0 flog f/ f* fround->s
;
: scale-semi-cb <{ w c info -- }> c  info Fvalue semi-scale-label  change-label ;

: create-semi-scale-widget { parent title init cb -- scale-label-list }
  $" semitones: %s" _ '( init ratio->semitones ) string-format { str }
  str FxmLabelWidgetClass parent
  '( FXmNbackground  basic-color ) undef FXtCreateManagedWidget { label }
  "scale" FxmScaleWidgetClass parent
  '( FXmNorientation   FXmHORIZONTAL
     FXmNshowValue     #f
     FXmNminimum       0
     FXmNmaximum       semi-range 2*
     FXmNvalue         semi-range init ratio->semitones +
     FXmNdecimalPoints 0
     FXmNtitleString   title
     FXmNbackground    basic-color ) undef FXtCreateManagedWidget { scale }
  scale FXmNvalueChangedCallback ['] scale-semi-cb label FXtAddCallback drop
  scale FXmNvalueChangedCallback cb                undef FXtAddCallback drop
  scale FXmNdragCallback         ['] scale-semi-cb label FXtAddCallback drop
  scale FXmNdragCallback         cb                undef FXtAddCallback drop
  '( scale label )
;

: add-sliders { dialog sliders -- sliders-array }
  "formd" FxmFormWidgetClass dialog
  '( FXmNleftAttachment   FXmATTACH_FORM
     FXmNrightAttachment  FXmATTACH_FORM
     FXmNtopAttachment    FXmATTACH_FORM
     FXmNbottomAttachment FXmATTACH_WIDGET
     FXmNbackground       highlight-color
     dialog FXmDIALOG_SEPARATOR FXmMessageBoxGetChild dup FWidget? if
       FXmNbottomWidget   swap
     else
       drop
     then ) undef FXtCreateManagedWidget { mainfrm }
  "rcd" FxmRowColumnWidgetClass mainfrm
  '( FXmNleftAttachment   FXmATTACH_FORM
     FXmNrightAttachment  FXmATTACH_FORM
     FXmNbackground       highlight-color
     FXmNorientation      FXmVERTICAL ) undef FXtCreateManagedWidget { mainform }
  sliders map
    *key* 0 array-ref FXmFONTLIST_DEFAULT_TAG FXmStringCreate { title }
    *key* 1 array-ref { low }
    *key* 2 array-ref { init }
    *key* 3 array-ref { high }
    *key* 4 array-ref { func }
    *key* 5 array-ref { scale }
    *key* length 7 = if
      *key* 6 array-ref 'log = if
	mainform title low init high func create-log-scale-widget ( scale-label-list )
      else
	mainform title init func create-semi-scale-widget ( scale-label-list )
      then
    else
      *key* 0 array-ref FxmScaleWidgetClass mainform
      '( FXmNorientation  	FXmHORIZONTAL
	 FXmNshowValue    	#t
	 FXmNminimum      	low  scale f* fround->s
	 FXmNmaximum      	high scale f* fround->s
	 FXmNvalue        	init scale f* fround->s
	 FXmNdecimalPoints
	 scale 10000 = if
	   4
	 else
	   scale 1000 = if
	     3
	   else
	     scale 100 = if
	       2
	     else
	       scale 10 = if
		 1
	       else
		 0
	       then
	     then
	   then
	 then
	 FXmNtitleString     title
	 FXmNleftAttachment  FXmATTACH_FORM
	 FXmNrightAttachment FXmATTACH_FORM
	 FXmNbackground      basic-color ) undef FXtCreateManagedWidget { new-slider }
      new-slider FXmNvalueChangedCallback func undef FXtAddCallback drop
      new-slider
    then
    title FXmStringFree drop
  end-map
;

: color->pixel ( color-str "name" --; self -- pixel )
  create #f , ,
 does> ( self -- pixel )
  { self }
  self @ unless
    main-widgets cadr { shell }
    shell FXtDisplay { dpy }
    dpy FDefaultScreen { scr }
    dpy scr FDefaultColormap { cmap }
    undef undef undef undef undef undef FXColor { col }
    dpy cmap self cell+ @ ( color-str ) col col FXAllocNamedColor 0= if
      $" can't allocate color!" _ snd-error drop
    else
      col Fpixel self !
    then
  then
  self @
;

"yellow" color->pixel yellow-pixel

\ c == '( prc type )
: target-arm-cb      <{ w c info -- }> c car ( prc ) c cdr ( type ) run-proc drop ;
: target-truncate-cb <{ w c info -- }> c     ( prc ) info Fset      run-proc drop ;

#() value selection-buttons

: add-target { mainform target-prc truncate-prc -- rc-wid }
  "sep" FxmSeparatorWidgetClass mainform
  '( FXmNorientation      FXmHORIZONTAL
     FXmNseparatorType    FXmSHADOW_ETCHED_OUT
     FXmNbackground       basic-color ) undef FXtCreateManagedWidget drop
  "rc" FxmRowColumnWidgetClass mainform
  '( FXmNorientation      FXmHORIZONTAL
     FXmNbackground       basic-color
     FXmNradioBehavior    #t
     FXmNradioAlwaysOne   #t
     FXmNbottomAttachment FXmATTACH_FORM
     FXmNleftAttachment   FXmATTACH_FORM
     FXmNrightAttachment  FXmATTACH_FORM
     FXmNentryClass       FxmToggleButtonWidgetClass
     FXmNisHomogeneous    #t ) undef FXtCreateManagedWidget { rc }
  #( '( $" entire sound"  _ 'sound     )
     '( $" selection"     _ 'selection )
     '( $" between marks" _ 'marks     ) ) each { lst }
    lst cadr { typ }
    lst car ( name ) FxmToggleButtonWidgetClass rc
    '( FXmNbackground     basic-color
       FXmNselectColor    yellow-pixel
       FXmNindicatorType  FXmONE_OF_MANY_ROUND
       FXmNarmCallback    '( ['] target-arm-cb '( target-prc typ ) ) )
    undef FXtCreateManagedWidget { wid }
    typ 'sound equal? if wid #t #t FXmToggleButtonSetState drop then
    typ 'selection equal? if
      selection-buttons wid array-push drop
      selection? unless wid #f FXtSetSensitive drop then
    then
  end-each
  truncate-prc if
    "trsep" FxmSeparatorWidgetClass mainform
    '( FXmNorientation FXmHORIZONTAL ) undef FXtCreateManagedWidget drop
    $" truncate at end" _ FxmToggleButtonWidgetClass mainform
    '( FXmNbackground  basic-color
       FXmNset         #t
       FXmNselectColor yellow-pixel ) undef FXtCreateManagedWidget ( trbutton )
    FXmNvalueChangedCallback ['] target-truncate-cb truncate-prc FXtAddCallback drop
  then
  rc
;

: help-cb { label message -- prc; w c i self -- }
  3 proc-create message , label ,
 does> ( w c i self -- )
  { w c info self }
  self cell+ @ ( label ) self @ ( message ) help-dialog drop
;

: marks-sort ( a b -- -1|0|1 )
  { a b }
  a b < if
    -1
  else
    a b = if
      0
    else
      1
    then
  then
;

\ returns a list of points
: plausible-mark-samples ( -- pts )
  selected-sound { snd }
  snd selected-channel { chn }
  #() { ms }
  snd chn #f marks each undef mark-sample ms swap array-push drop end-each
  ms length 2 < if
    #f
  else
    ms ['] marks-sort array-sort! drop
    ms length 2 = if
      ms array->list
    else
      snd chn left-sample { lw }
      snd chn right-sample { rw }
      snd chn undef cursor { cw }
      cw lw >= cw rw <= && if
	cw
      else
	lw rw + 2/
      then { favor }
      ms each { p1 }
	i ms length 2 - = if
	  '( p1 ms last-ref )
	  leave
	then
	ms i 1+ array-ref { p2 }
	ms i 2 + array-ref { p3 }
	p1 favor - abs p3 favor - abs < if
	  '( p1 p2 )
	  leave
	then
      end-each
    then
  then
;

: effect-frames ( target -- frms )
  { target }
  target 'sound equal? if
    #f #f #f frames 1-
  else
    target 'selection equal? if
      #f #f selection-frames
    else
      plausible-mark-samples { pts }
      pts if
	pts car pts cdr each - end-each abs 1+
      else
	0
      then
    then
  then
;

: map-chan-over-target-with-sync { func target origin-func decay -- }
  target 'selection equal? selection? not && if
    $" no selection" _ snd-warning drop
  else
    target 'sound equal? sounds null? && if
      $" no sound" _ snd-warning drop
    else
      target 'marks equal? sounds null? && if
	$" no marks" _ snd-warning drop
      else
	#f sync { snc }
	target 'marks = if
	  plausible-mark-samples
	else
	  '()
	then { pts }
	target 'sound equal? if
	  0
	else
	  target 'selection equal? if
	    #f #f selection-position
	  else
	    pts car
	  then
	then { beg }
	decay number? if
	  #f srate decay f* fround->s
	else
	  0
	then { overlap }
	snc 0> if
	  all-chans
	else
	  #( '( selected-sound dup selected-channel ) )
	then each { lst }
	  lst car  { snd }
	  lst cadr { chn }
	  target 'sound equal? if
	    snd chn undef frames 1-
	  else
	    target 'selection equal? if
	      #f #f selection-position #f #f selection-frames +
	    else
	      pts cadr
	    then
	  then { end }
	  end beg - { dur }
	  snd sync snc = if
	    origin-func '( target dur ) run-proc { name orig }
	    $" %s %s %s %s" '( orig beg
	       target 'sound equal? if #f else dur 1+ then
	       name ) string-format { origin }
	    func dur run-proc beg end overlap + 1+ snd chn #f origin map-channel drop
	  then
	end-each
      then
    then
  then
;
set-current
previous

\ === Effects Entries ===

hide
struct
  cell% field effects-label
  cell% field effects-dialog
  cell% field effects-target
  cell% field effects-truncate
  cell% field effects-sliders
  cell% field effects-scaler
  cell% field effects-freq
  cell% field effects-amp
  cell% field effects-delay
  cell% field effects-amount
  cell% field effects-envelope
  cell% field effects-size
end-struct effects-base%
set-current

: label@      ( gen -- lab ) effects-label @ ;
: label!      ( lab gen -- ) effects-label ! ;
: dialog@     ( gen -- wid ) effects-dialog @ ;
: dialog!     ( wid gen -- ) effects-dialog ! ;
: target@     ( gen -- tar ) effects-target @ ;
: target!     ( tar gen -- ) effects-target ! ;
: truncate@   ( gen -- tar ) effects-truncate @ ;
: truncate!   ( tar gen -- ) effects-truncate ! ;
: sliders@    ( gen -- ary ) effects-sliders @ ;
: sliders!    ( ary gen -- ) effects-sliders ! ;
: scaler@     ( gen -- scl ) effects-scaler @ ;
: scaler!     ( scl gen -- ) effects-scaler ! ;
: frequency@  ( gen -- frq ) effects-freq @ ;
: frequency!  ( frq gen -- ) effects-freq ! ;
: amplitude@  ( gen -- amp ) effects-amp @ ;
: amplitude!  ( amp gen -- ) effects-amp ! ;
: delay-time@ ( gen -- del ) effects-delay @ ;
: delay-time! ( del gen -- ) effects-delay ! ;
: amount@     ( gen -- amt ) effects-amount @ ;
: amount!     ( amt gen -- ) effects-amount ! ;
: envel@   ( gen -- env ) effects-envelope @ ;
: envel!   ( env gen -- ) effects-envelope ! ;
: size@       ( gen -- siz ) effects-size @ ;
: size!       ( siz gen -- ) effects-size ! ;

: make-base-effects ( label gen -- gen )
  { label gen }
  label  gen label!
  #f     gen dialog!
  'sound gen target!
  #t     gen truncate!
  #()    gen sliders!
  gen
;
: target-cb ( gen -- prc; target self -- )
  1 proc-create swap ,
 does> { target self }
  target self @ ( gen ) target!
;
: truncate-cb ( gen -- prc; trunc self -- )
  1 proc-create swap ,
 does> ( trunc self -- )
  { trunc self }
  trunc self @ ( gen ) truncate!
;
previous

\ === AMPLITUDE EFFECTS ===

hide
: squelch-cb { f0 f1 amount -- prc; y self -- val }
  1 proc-create amount , f1 , f0 ,
 does> ( y self -- val )
  { y self }
  self @ { amp }
  self 1 cells + @ { f1 }
  self 2 cells + @ { f0 }
  f1  f0 y y f* moving-average amp f< if 0.0 else 1.0 then  moving-average y f*
;
set-current
: effects-squelch-channel <{ amount gate-size :optional snd #f chn #f -- val }>
  :size gate-size make-moving-average { f0 }
  :size gate-size :initial-element 1.0 make-moving-average { f1 }
  $" %s %s %s" '( amount gate-size get-func-name ) string-format { origin }
  f0 f1 amount squelch-cb 0 #f snd chn #f origin map-channel
;
previous

\ === Gain (gain set by gain-amount) ===

hide
: gain-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen envel@ xe-envelope '( 0.0 1.0 1.0 1.0 ) equal? if
    #f
  else
    gen envel@ xe-envelope gen amount@ scale-envelope
  then { with-env }
  gen target@ 'sound equal? if
    with-env list? if
      with-env 0 undef 1.0 #f #f #f env-sound drop
    else
      gen amount@ #f #f scale-by drop
    then
  else
    gen target@ 'selection equal? if
      selection? if
	with-env list? if
	  with-env 1.0 env-selection drop
	else
	  gen amount@ scale-selection-by drop
	then
      else
	$" no selection" _ snd-warning drop
      then
    else
      plausible-mark-samples { pts }
      pts if
	with-env list? if
	  with-env  pts car  pts cadr  pts car -  1.0 #f #f #f env-sound drop
	else
	  gen amount@ pts car  pts cadr  pts car - #f #f #f normalize-channel drop
	then
      else
	$" no marks" _ snd-warning drop
      then
    then
  then
;
: gain-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init ) gen amount!
  gen envel@ '( 0.0 1.0 1.0 1.0 ) set-xe-envelope
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@ 100.0 f* f>s ) FXtVaSetValues drop
;
: gain-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amount!
;
: post-gain-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen gain-ok-cb
    gen label@ $" Move the slider to change the gain scaling amount." _ help-cb
    gen gain-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( "gain" _ 0.0 gen amount@ 5.0 gen gain-slider-cb 100 ) )
    add-sliders gen sliders!
    "fr" FxmFrameWidgetClass gen sliders@ 0 array-ref FXtParent FXtParent
    '( FXmNheight          200
       FXmNleftAttachment  FXmATTACH_FORM
       FXmNrightAttachment FXmATTACH_FORM
       FXmNtopAttachment   FXmATTACH_WIDGET
       FXmNtopWidget       gen sliders@ -1 array-ref
       FXmNshadowThickness 4
       FXmNshadowType      FXmSHADOW_ETCHED_OUT ) undef FXtCreateManagedWidget { fr }
    gen sliders@ 0 array-ref FXtParent FXtParent gen target-cb #f add-target { target-row }
    gen dialog@ activate-dialog
    gen label@ string-downcase
    fr
    '( 0.0 1.0 1.0 1.0 ) ( envelope )
    '( 0.0 1.0 0.0 1.0 ) ( axis-bounds )
    '( FXmNheight 200 ) make-xenved gen envel!
    fr '( FXmNbottomAttachment FXmATTACH_WIDGET FXmNbottomWidget target-row ) FXtVaSetValues drop
  else
    gen dialog@ activate-dialog
  then
;
set-current

: make-gain-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  1.0 gen amount!
  #f  gen envel!
  gen post-gain-dialog ( prc1 )
  1 proc-create gen ,  ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen amount@ ) string-format change-label
;
previous

\ === Normalize ===

hide
: normalize-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen target@ 'sound equal? if
    gen amount@ #f #f scale-to drop
  else
    gen target@ 'selection equal? if
      selection? if
	gen amount@ scale-selection-to drop
      else
	$" no selection" _ snd-warning drop
      then
    else
      plausible-mark-samples { pts }
      pts if
	gen amount@  pts car  pts cadr  pts car - #f #f #f normalize-channel drop
      else
	$" no marks" _ snd-warning drop
      then
    then
  then
;
: normalize-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init ) gen amount!
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@ 100.0 f* f>s ) FXtVaSetValues drop
;
: normalize-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amount!
;
: post-normalize-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen normalize-ok-cb
    gen label@ $" Normalize scales amplitude to the normalize amount.  \
Move the slider to change the scaling amount." _ help-cb
    gen normalize-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( "normalize" _ 0.0 gen amount@ 1.0 gen normalize-slider-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-normalize-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  1.0 gen amount!
  gen post-normalize-dialog ( prc1 )
  1 proc-create gen ,       ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen amount@ ) string-format change-label
;
previous

\ === Gate (gate set by gate-amount) ===

hide
effects-base%
  cell% field omit-silence
end-struct gate%

: omit-silence@ ( gen -- f )   omit-silence @ ;
: omit-silence! ( f gen -- )   omit-silence ! ;

: gate-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  selected-sound sync { snc }
  snc 0> if
    all-chans each { lst }
      lst car { snd }
      snd sync snc = if
	lst cadr { chn }
	gen amount@ dup f* gen size@ snd chn effects-squelch-channel drop
      then
    end-each
  else
    gen amount@ dup f* gen size@ #f #f effects-squelch-channel drop
  then
;
: gate-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init ) gen amount!
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@ 1000.0 f* f>s ) FXtVaSetValues drop
;
: gate-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 1000.0 f/ self @ ( gen ) amount!
;
: gate-omit-cb <{ w gen info -- }> info Fset gen omit-silence! ;
: post-gate-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen gate-ok-cb
    gen label@
    $" Move the slider to change the gate intensity. Higher values gate more of the sound." _
    help-cb
    gen gate-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( "gate" _ 0.0 gen amount@ 0.1 gen gate-slider-cb 1000 ) )
    add-sliders gen sliders!
    $" Omit silence" _ FXmStringCreateLocalized { s1 }
    $" Omit silence" _ FxmToggleButtonWidgetClass gen sliders@ 0 array-ref FXtParent
    '( FXmNselectColor pushed-button-color
       FXmNbackground  basic-color
       FXmNvalue       gen omit-silence@ if 1 else 0 then
       FXmNlabelString s1 ) undef FXtCreateManagedWidget ( toggle )
    FXmNvalueChangedCallback ['] gate-omit-cb gen FXtAddCallback drop
    s1 FXmStringFree drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-gate-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) gate% %alloc make-base-effects { gen }
  0.01 gen amount!
  128  gen size!
  #f   gen omit-silence!
  gen post-gate-dialog ( prc1 )
  1 proc-create gen ,  ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.4f)" '( gen label@ gen amount@ ) string-format change-label
;
previous

\ === DELAY EFFECTS ===

hide
: effects-echo-cb { samps amp del -- prc; inval self -- r }
  1 proc-create 0 , del , amp , samps ,
 does> ( inval self -- r )
  { inval self }
  self @ 1+ dup self ! { samp }
  self cell+ @ { del }
  self 2 cells + @ { amp }
  self 3 cells + @ { samps }
  del dup 0.0 tap samp samps <= if inval f+ then amp f* 0.0 delay inval f+
;
: effects-flecho-cb ( amp samps flt del -- prc; inval self -- r )
  { amp samps flt del }
  1 proc-create 0 , samps , flt , del , amp ,
 does> ( inval self -- r )
  { inval self }
  self @ 1+ dup self ! { samp }
  self cell+ @ { samps }
  self 2 cells + @ { flt }
  self 3 cells + @ { del }
  self 4 cells + @ { scl }
  del flt del 0.0 tap samp samps <= if inval f+ then scl f* fir-filter delay inval f+
;
: effects-zecho-cb ( scaler amp samps os del -- prc; inval self -- r )
  { scaler amp samps os del }
  1 proc-create 0 , samps , os , del , scaler , amp ,
 does> ( inval self -- r )
  { inval self }
  self @ 1+ dup self ! { samp }
  self cell+ @ { samps }
  self 2 cells + @ { os }
  self 3 cells + @ { del }
  self 4 cells + @ { scl }
  self 5 cells + @ { amp }
  del
  del 0.0 tap samp samps <= if inval f+ then scl f*
  os 0.0 0.0 oscil amp f*
  delay inval f+
;
set-current

: effects-echo <{ input-samps del-time amp :optional beg 0 dur #f snd #f chn #f -- res }>
  del-time snd srate f* fround->s make-delay { del }
  input-samps number? if
    input-samps
  else
    dur number? if
      dur
    else
      snd chn undef frames
    then
  then { samps }
  $" %s %s %s %s %s %s" '( input-samps del-time amp beg dur get-func-name ) string-format { origin }
  samps amp del effects-echo-cb beg dur snd chn #f origin map-channel
;

: effects-flecho <{ amp secs input-samps :optional beg 0 dur #f snd #f chn #f -- res }>
  :order 4 :xcoeffs vct( 0.125 0.25 0.25 0.125 ) make-fir-filter { flt }
  secs snd srate f* fround->s make-delay { del }
  input-samps number? if
    input-samps
  else
    dur number? if
      dur
    else
      snd chn undef frames
    then
  then { samps }
  $" %s %s %s %s %s %s" '( amp secs input-samps beg dur get-func-name ) string-format { origin }
  amp samps flt del effects-flecho-cb beg dur snd chn #f origin map-channel
;

: effects-zecho <{ scaler secs freq amp input-samps :optional beg 0 dur #f snd #f chn #f -- res }>
  freq make-oscil { os }
  secs snd srate f* fround->s { len }
  :size len :max-size len amp f>s 1 + + make-delay { del }
  input-samps number? if
    input-samps
  else
    dur number? if
      dur
    else
      snd chn undef frames
    then
  then { samps }
  $" %s %s %s %s %s %s %s %s" '( scaler secs freq amp input-samps beg dur get-func-name )
  string-format { origin }
  scaler amp samps os del effects-zecho-cb beg dur snd chn #f origin map-channel
;
previous

\ === Echo (controlled by delay-time and echo-amount) ===

hide
: echo-func-cb ( gen -- prc; samps self -- prc; inval self -- r )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen delay-time@ #f srate f* f>s make-delay { del }
  1 proc-create 0 , samps , del , gen ,
 does> ( inval self -- r )
  { inval self }
  self @ 1+ dup self ! { samp }
  self 1 cells + @ { samps }
  self 2 cells + @ { del }
  self 3 cells + @ { gen }
  del dup 0.0 tap samp samps <= if inval f+ then gen amount@ f* 0.0 delay inval f+
;
: echo-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target samps self -- name origin )
  { target samps self }
  self @ { gen }
  "effects-echo"
  $" %s %s %s"
  '( gen target@ 'sound equal? if
       #f
     else
       samps
     then gen delay-time@ gen amount@ ) string-format
;
: echo-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen echo-func-cb
  gen target@
  gen echo-origin-cb
  gen truncate@ if #f else 4.0 gen delay-time@ f* then map-chan-over-target-with-sync
;
: echo-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ , gen delay-time@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-echo )  gen amount!
  self 2 cells + @ ( init-delay ) gen delay-time!
  gen sliders@ 0 array-ref '( FXmNvalue gen delay-time@ 100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen amount@     100.0 f* f>s ) FXtVaSetValues drop
;
: echo-delay-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) delay-time!
;
: echo-amount-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amount!
;
: post-echo-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen echo-ok-cb
    gen label@ $" The sliders change the delay time and echo amount." _ help-cb
    gen echo-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" delay time"  _ 0.0 gen delay-time@  2.0 gen echo-delay-slider-cb  100 )
       #( $" echo amount" _ 0.0 gen amount@      1.0 gen echo-amount-slider-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb gen truncate-cb add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-echo-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  0.5 gen delay-time!
  0.2 gen amount!
  gen post-echo-dialog ( prc1 )
  1 proc-create gen ,  ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f)" '( gen label@ gen delay-time@ gen amount@ ) string-format change-label
;
previous

\ === Filtered Echo ===

hide
: flecho-func-cb ( gen -- prc; samps self -- prc; inval self -- r )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  :order 4 :xcoeffs vct( 0.125 0.25 0.25 0.125 ) make-fir-filter { flt }
  gen delay-time@ #f srate f* fround->s make-delay { del }
  1 proc-create 0 , samps , flt , del , gen amount@ ,
 does> ( inval self -- r )
  { inval self }
  self @ 1+ dup self ! { samp }
  self 1 cells + @ { samps }
  self 2 cells + @ { flt }
  self 3 cells + @ { del }
  self 4 cells + @ { scl }
  del flt del 0.0 tap samp samps <= if inval f+ then scl f* fir-filter delay inval f+
;
: flecho-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target samps self -- name origin )
  { target samps self }
  self @ { gen }
  $" effects-flecho"
  $" %s %s %s"
  '( gen amount@ gen delay-time@ gen target@ 'sound equal? if #f else samps then ) string-format
;
: flecho-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen flecho-func-cb
  gen target@
  gen flecho-origin-cb
  gen truncate@ if #f else 4.0 gen delay-time@ f* then map-chan-over-target-with-sync
;
: flecho-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen delay-time@ , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-delay )  gen delay-time!
  self 2 cells + @ ( init-scaler ) gen amount!
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@     100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen delay-time@ 100.0 f* f>s ) FXtVaSetValues drop
;
: post-flecho-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen flecho-ok-cb
    gen label@ $" Move the sliders to set the filter scaler and the delay time in seconds." _
    help-cb
    gen flecho-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" filter scaler"     _ 0.0 gen amount@     1.0 gen echo-amount-slider-cb 100 )
       #( $" delay time (secs)" _ 0.0 gen delay-time@ 3.0 gen echo-delay-slider-cb  100 ) )
   add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb gen truncate-cb add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-flecho-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  0.9 gen delay-time!
  0.5 gen amount!
  gen post-flecho-dialog ( prc1 )
  1 proc-create gen ,    ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f)" '( gen label@ gen amount@ gen delay-time@ ) string-format change-label
;
previous

\ === Modulated Echo ===

hide
: zecho-func-cb ( gen -- prc; samps self -- prc; inval self -- r )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen frequency@ make-oscil { os }
  gen delay-time@ #f srate f* fround->s { len }
  :size len :max-size len gen amplitude@ f>s 1+ + make-delay { del }
  1 proc-create 0 , samps , os , del , gen scaler@ , gen amplitude@ ,
 does> ( inval self -- r )
  { inval self }
  self @ { samp }
  1 self +! ( samp++ )
  self 1 cells + @ { samps }
  self 2 cells + @ { os }
  self 3 cells + @ { del }
  self 4 cells + @ { scl }
  self 5 cells + @ { amp }
  del                                                 ( del-gen )
  del 0.0 tap  samp samps < if inval f+ then  scl f*  ( input )
  os 0.0 0.0 oscil amp f*                             ( pm )
  delay inval f+
;
: zecho-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target samps self -- name origin )
  { target samps self }
  self @ { gen }
  $" effects-zecho"
  $" %s %s %s %s %s"
  '( gen scaler@ gen delay-time@ gen frequency@ gen amplitude@ gen target@ 'sound equal? if
    #f
  else
    samps
  then ) string-format
;
: zecho-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen zecho-func-cb
  gen target@
  gen zecho-origin-cb
  gen truncate@ if #f else 4.0 gen delay-time@ f* then map-chan-over-target-with-sync
;
: zecho-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amplitude@ , gen frequency@ , gen delay-time@ , gen scaler@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-amp )    gen amplitude!
  self 2 cells + @ ( init-freq )   gen frequency!
  self 3 cells + @ ( init-delay )  gen delay-time!
  self 4 cells + @ ( init-scaler ) gen scaler!
  gen sliders@ 0 array-ref '( FXmNvalue gen scaler@     100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen delay-time@ 100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen frequency@  100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 3 array-ref '( FXmNvalue gen amplitude@  100.0 f* f>s ) FXtVaSetValues drop
;
: zecho-scl-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) scaler!
;
: zecho-del-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) delay-time!
;
: zecho-frq-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) frequency!
;
: zecho-amp-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amplitude!
;
: post-zecho-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen zecho-ok-cb
    gen label@ $" Move the sliders to set the echo scaler, \
the delay time in seconds, the modulation frequency, and the echo amplitude." _ help-cb
    gen zecho-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" echo scaler"         _ 0.0 gen scaler@       1.0 gen zecho-scl-slider-cb 100 )
       #( $" delay time (secs)"   _ 0.0 gen delay-time@   3.0 gen zecho-del-slider-cb 100 )
       #( $" modulatio frequency" _ 0.0 gen frequency@  100.0 gen zecho-frq-slider-cb 100 )
       #( $" modulatio amplitude" _ 0.0 gen amplitude@  100.0 gen zecho-amp-slider-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb gen truncate-cb add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-zecho-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  0.50 gen scaler!
  0.75 gen delay-time!
  6.00 gen frequency!
  10.0 gen amplitude!
  gen post-zecho-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f %.2f %.2f)"
  '( gen label@ gen scaler@ gen delay-time@ gen frequency@ gen amplitude@ )
  string-format change-label
;
previous

\ === FILTER EFFECTS ===

: effects-bbp <{ freq bw :optional beg 0 dur #f snd #f chn #f -- res }>
  $" %s %s %s %s %s" '( freq bw beg dur get-func-name ) string-format { origin }
  freq bw make-butter-band-pass beg dur snd chn #f #f origin clm-channel
;

: effects-bbr <{ freq bw :optional beg 0 dur #f snd #f chn #f -- res }>
  $" %s %s %s %s %s" '( freq bw beg dur get-func-name ) string-format { origin }
  freq bw make-butter-band-reject beg dur snd chn #f #f origin clm-channel
;

: effects-bhp <{ freq :optional beg 0 dur #f snd #f chn #f -- res }>
  $" %s %s %s %s" '( freq beg dur get-func-name ) string-format { origin }
  freq make-butter-high-pass beg dur snd chn #f #f origin clm-channel
;

: effects-blp <{ freq :optional beg 0 dur #f snd #f chn #f -- res }>
  $" %s %s %s %s" '( freq beg dur get-func-name ) string-format { origin }
  freq make-butter-low-pass beg dur snd chn #f #f origin clm-channel
;

: effects-comb-filter <{ scaler size :optional beg 0 dur #f snd #f chn #f -- res }>
  $" %s %s %s %s %s" '( scaler size beg dur get-func-name ) string-format { origin }
  scaler size comb-filter beg dur snd chn #f origin map-channel
;

: effects-comb-chord <{ scaler size amp :optional beg 0 dur #f snd #f chn #f -- res }>
  $" %s %s %s %s %s %s" '( scaler size amp beg dur get-func-name ) string-format { origin }
  scaler size amp comb-chord beg dur snd chn #f origin map-channel
;

hide
: moog-cb ( gen -- prc; inval self -- res )
  1 proc-create swap ,
 does> ( inval self -- res )
  { inval self }
  self @ ( gen ) inval moog-filter
;
set-current
: effects-moog <{ freq Q :optional beg 0 dur #f snd #f chn #f -- res }>
  $" %s %s %s %s %s" '( freq Q beg dur get-func-name ) string-format { origin }
  freq Q make-moog-filter moog-cb beg dur snd chn #f origin map-channel
;
previous

: moog ( freq Q -- prc; inval self -- res )
  make-moog-filter { gen }
  1 proc-create gen ,
 does> ( inval self -- res )
  { inval self }
  self @ ( gen ) inval moog-filter
;

\ === Butterworth band-pass filter ===

hide
effects-base%
  cell% field band-pass-bw
end-struct bp-filter%

: band-pass-bw@   ( gen -- bw )  band-pass-bw @ ;
: band-pass-bw!   ( bw gen -- )  band-pass-bw ! ;

: bp-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen frequency@ gen band-pass-bw@ make-butter-band-pass { flt }
  gen target@ 'sound equal? if
    $" %s %s 0 #f effects-bbp" '( gen frequency@ gen band-pass-bw@ ) string-format { origin }
    flt #f #f #f #f origin filter-sound drop
  else
    gen target@ 'selection equal? if
      flt #f #f filter-selection drop
    else
      plausible-mark-samples { pts }
      pts car { bg }
      pts cadr bg - 1+ { nd }
      $" %s %s %s %s effects-bbp" '( gen frequency@ gen band-pass-bw@ bg nd )
      string-format { origin }
      flt bg nd #f #f #f #f origin clm-channel drop
    then
  then
;
: bp-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen band-pass-bw@ , gen frequency@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-bw )   gen band-pass-bw!
  self 2 cells + @ ( init-freq ) gen frequency!
  gen sliders@ 0 array-ref car
  '( FXmNvalue 20.0 gen frequency@ 22050.0 scale-log->linear ) FXtVaSetValues drop
  gen sliders@ 0 array-ref cadr gen frequency@ number->string change-label
  gen sliders@ 1 array-ref '( FXmNvalue gen band-pass-bw@ ) FXtVaSetValues drop
;
: bp-freq-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  20.0 info Fvalue 22050.0 scale-linear->log self @ ( gen ) frequency!
;
: bp-bw-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) band-pass-bw!
;
: post-band-pass-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen bp-ok-cb
    gen label@ $" Butterworth band-pass filter.  \
Move the sliders to change the center frequency and bandwidth." _ help-cb
    gen bp-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" center frequency" _ 20.0 gen frequency@    22050.0 gen bp-freq-slider-cb 1 'log )
       #( $" bandwidth"        _    0 gen band-pass-bw@    1000 gen bp-bw-slider-cb   1 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref car FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-band-pass-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name) bp-filter% %alloc make-base-effects { gen }
  1000.0 gen frequency!
  100 gen band-pass-bw!
  gen post-band-pass-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %d)" '( gen label@ gen frequency@ gen band-pass-bw@ ) string-format change-label
;
previous

\ === Butterworth band-reject (notch) filter ===

hide
effects-base%
  cell% field notch-bw
end-struct notch%

: notch-bw@   ( gen -- bw )  notch-bw @ ;
: notch-bw!   ( bw gen -- )  notch-bw ! ;

: br-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen frequency@ gen notch-bw@ make-butter-band-reject { flt }
  gen target@ 'sound equal? if
    $" %s %s 0 #f effects-bbr" '( gen frequency@ gen notch-bw@ ) string-format { origin }
    flt #f #f #f #f origin filter-sound drop
  else
    gen target@ 'selection equal? if
      flt #f #f filter-selection drop
    else
      plausible-mark-samples { pts }
      pts car { bg }
      pts cadr bg - 1+ { nd }
      $" %s %s %s %s effects-bbp" '( gen frequency@ gen notch-bw@ bg nd ) string-format { origin }
      flt bg nd #f #f #f #f origin clm-channel drop
    then
  then
;
: br-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen notch-bw@ , gen frequency@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-bw )   gen notch-bw!
  self 2 cells + @ ( init-freq ) gen frequency!
  gen sliders@ 0 array-ref car
  '( FXmNvalue 20.0 gen frequency@ 22050.0 scale-log->linear ) FXtVaSetValues drop
  gen sliders@ 0 array-ref cadr gen frequency@ number->string change-label
  gen sliders@ 1 array-ref '( FXmNvalue gen notch-bw@ ) FXtVaSetValues drop
;
: br-freq-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  20.0 info Fvalue 22050.0 scale-linear->log self @ ( gen ) frequency!
;
: br-bw-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) notch-bw!
;
: post-notch-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen br-ok-cb
    gen label@ $" Butterworth band-reject filter.  \
Move the sliders to change the center frequency and bandwidth." _ help-cb
    gen br-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" center frequency" _ 20.0 gen frequency@   22050.0 gen br-freq-slider-cb 1 'log )
       #( $" bandwidth"        _    0 gen notch-bw@       1000 gen br-bw-slider-cb   1  ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref car FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-notch-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) notch% %alloc make-base-effects { gen }
  100.0 gen frequency!
  100   gen notch-bw!
  gen post-notch-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %d)" '( gen label@ gen frequency@ gen notch-bw@ ) string-format change-label
;
previous

\ === Butterworth high-pass filter ===

hide
: hp-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen frequency@ make-butter-high-pass { flt }
  gen target@ 'sound equal? if
    $" %s 0 #f effects-bhp" gen frequency@ string-format { origin }
    flt #f #f #f #f origin filter-sound drop
  else
    gen target@ 'selection equal? if
      flt #f #f filter-selection drop
    else
      plausible-mark-samples { pts }
      pts car { bg }
      pts cadr bg - 1+ { nd }
      $" %s %s %s effects-bhp" '( gen frequency@ bg nd ) string-format { origin }
      flt bg nd #f #f #f #f origin clm-channel drop
    then
  then
;
: hp-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen frequency@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen  }
  self cell+ @ ( init-freq ) gen frequency!
  gen sliders@ 0 array-ref car
  '( FXmNvalue 20.0 gen frequency@ 22050.0 scale-log->linear ) FXtVaSetValues drop
  gen sliders@ 0 array-ref cadr gen frequency@ number->string change-label
;
: hp-freq-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  20.0 info Fvalue 22050.0 scale-linear->log self @ ( gen ) frequency!
;
: post-high-pass-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen hp-ok-cb
    gen label@ $" Butterworth high-pass filter.  \
Move the slider to change the high-pass cutoff frequency." _ help-cb
    gen hp-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" high-pass cutoff frequency" _
	  20.0 gen frequency@ 22050.0 gen hp-freq-slider-cb 1 'log ) ) add-sliders gen sliders!
    gen sliders@ 0 array-ref car FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-high-pass-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  100.0 gen frequency!
  gen post-high-pass-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen frequency@ ) string-format change-label
;
previous

\ === Butterworth low-pass filter ===

hide
: lp-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen frequency@ make-butter-low-pass { flt }
  gen target@ 'sound equal? if
    $" %s 0 #f effects-blp" gen frequency@ string-format { origin }
    flt #f #f #f #f origin filter-sound drop
  else
    gen target@ 'selection equal? if
      flt #f #f filter-selection drop
    else
      plausible-mark-samples { pts }
      pts car { bg }
      pts cadr bg - 1+ { nd }
      $" %s %s %s effects-blp" '( gen frequency@ bg nd ) string-format { origin }
      flt bg nd #f #f #f #f origin clm-channel drop
    then
  then
;
: lp-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen frequency@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init-freq ) gen frequency!
  gen sliders@ 0 array-ref car
  '( FXmNvalue 20.0 gen frequency@ 22050.0 scale-log->linear ) FXtVaSetValues drop
  gen sliders@ 0 array-ref cadr gen frequency@ number->string change-label
;
: lp-freq-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  20.0 info Fvalue 22050.0 scale-linear->log self @ ( gen ) frequency!
;
: post-low-pass-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen lp-ok-cb
    gen label@ $" Butterworth low-pass filter.  \
Move the slider to change the low-pass cutoff frequency." _ help-cb
    gen lp-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" low-pass cutoff frequency" _
	  20.0 gen frequency@ 22050.0 gen lp-freq-slider-cb 1 'log ) ) add-sliders gen sliders!
    gen sliders@ 0 array-ref car FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-low-pass-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  1000.0 gen frequency!
  gen post-low-pass-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f)" '( gen label@ gen frequency@ ) string-format change-label
;
previous

\ === Comb filter ===
hide
: comb-func-cb ( gen -- prc; samps self -- proc )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen scaler@ gen size@ comb-filter
;
: comb-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  $" effects-comb-filter" $" %s %s" '( gen scaler@ gen size@ ) string-format
;
: comb-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen comb-func-cb gen target@ gen comb-origin-cb #f map-chan-over-target-with-sync
;
: comb-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen size@ , gen scaler@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-size )   gen size!
  self 2 cells + @ ( init-scaler ) gen scaler!
  gen sliders@ 0 array-ref '( FXmNvalue gen scaler@ 100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen size@                ) FXtVaSetValues drop
;
: scaler-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) scaler!
;
: size-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) size!
;
: post-comb-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen comb-ok-cb
    gen label@ $" Move the slider to change the comb scaler and size." _ help-cb
    gen comb-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( "scaler" _ 0.0 gen scaler@ 1.0 gen scaler-slider-cb 100 )
       #( "size"   _   0 gen size@   100 gen size-slider-cb     1 ) ) add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-comb-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  0.1 gen scaler!
  50  gen size!
  gen post-comb-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %d)" '( gen label@ gen scaler@ gen size@ ) string-format change-label
;
previous

\ === Comb-chord filter ===

hide
: cc-func-cb ( gen -- prc; samps self -- proc )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen scaler@ gen size@ gen amplitude@ comb-chord
;
: cc-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  $" effects-comb-chord"
  $" %s %s %s" '( gen scaler@ gen size@ gen amplitude@ ) string-format
;
: cc-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen cc-func-cb gen target@ gen cc-origin-cb #f map-chan-over-target-with-sync
;
: cc-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amplitude@ , gen size@ , gen scaler@ ,
 does> ( w c i self -- )
  { w c info self }
  self           @ { gen }
  self 1 cells + @ ( init-amp )    gen amplitude!
  self 2 cells + @ ( init-size )   gen size!
  self 3 cells + @ ( init-scaler ) gen scaler!
  gen sliders@ 0 array-ref '( FXmNvalue gen scaler@          100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen size@                         ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen amplitude@       100.0 f* f>s ) FXtVaSetValues drop
;
: cc-scaler-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) scaler!
;
: cc-size-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self ( gen ) size!
;
: cc-amplitude-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amplitude!
;
: post-cc-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen cc-ok-cb
    gen label@ $" Creates chords by using filters at harmonically related sizes.  \
Move the sliders to set the comb chord parameters." _ help-cb
    gen cc-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" chord scaler" _ 0.0 gen scaler@          1.0 gen cc-scaler-cb    100 )
       #( $" chord size"   _   0 gen size@            100 gen cc-size-cb        1 )
       #( $" amplitude"    _ 0.0 gen amplitude@       1.0 gen cc-amplitude-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-comb-chord-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  0.95 gen scaler!
  60   gen size!
  0.3  gen amplitude!
  gen post-cc-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %d %.2f)" '( gen label@ gen scaler@ gen size@ gen amplitude@ )
  string-format change-label
;
previous

\ === Moog filter ===

hide
effects-base%
  cell% field moog-resonance
end-struct moog%

: moog-resonance@ ( gen -- res ) moog-resonance @ ;
: moog-resonance! ( res gen -- ) moog-resonance ! ;

: moog-func-cb ( gen -- prc; samps self -- proc )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen frequency@ gen moog-resonance@ moog
;
: moog-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  "effects-moog"
  $" %s %s" '( gen frequency@ gen moog-resonance@ ) string-format
;
: moog-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen moog-func-cb
  gen target@
  gen moog-origin-cb
  #f
  map-chan-over-target-with-sync
;
: moog-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen frequency@ , gen moog-resonance@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-freq ) gen frequency!
  self 2 cells + @ ( init-res )  gen moog-resonance!
  gen sliders@ 0 array-ref car
  '( FXmNvalue 20.0 gen frequency@ 22050.0 scale-log->linear ) FXtVaSetValues drop
  gen sliders@ 0 array-ref cadr gen frequency@ number->string change-label
  gen sliders@ 1 array-ref '( FXmNvalue gen moog-resonance@ 100.0 f* f>s ) FXtVaSetValues drop
;
: moog-freq-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  20.0 info Fvalue 22050.0 scale-linear->log self @ ( gen ) frequency!
;
: moog-res-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) moog-resonance!
;
: post-moog-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen moog-ok-cb
    gen label@ $" Moog-style 4-pole lowpass filter with 24db/oct rolloff and variable resonance.  \
Move the sliders to set the filter cutoff frequency and resonance." _ help-cb    
    gen moog-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" cutoff frequency" _ 20.0 gen frequency@ 22050.0 gen moog-freq-cb 1 'log )
       #( $" resonanze"        _  0.0 gen moog-resonance@ 1.0 gen moog-res-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref car FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-moog-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) moog% %alloc make-base-effects { gen }
  10000.0 gen frequency!
  0.5     gen moog-resonance!
  gen post-moog-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f)" '( gen label@ gen frequency@ gen moog-resonance@ ) string-format change-label
;
previous

\ === FREQUENCY EFFECTS ===

\ === Adaptive saturation ===

hide
: adsat-func-cb ( gen -- prc; samps self -- prc; val self -- res )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  1 proc-create gen , gen size@ 0.0 make-vct , 0.0 , 0.0 , 0 ,
 does> ( val self -- res )
  { val self }
  self @ { gen }
  self 1 cells + @ { vals }
  self 2 cells + @ { mn }
  self 3 cells + @ { mx }
  self 4 cells + @ { n }
  gen size@ n = if
    vals each { x }
      vals i  x f0>= if mx else mn then  vct-set! drop
      0.0 self 2 cells + ! ( mn )
      0.0 self 3 cells + ! ( mx )
      0   self 4 cells + ! ( n )
    end-each
    vals
  else
    vals n val vct-set! drop
    val mx f> if val self 3 cells + ! ( mx ) then
    val mn f< if val self 2 cells + ! ( mn ) then
    n 1+ self 4 cells + ! ( n++ )
    #f
  then
;
: adsat-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  "adsat"
  gen size@ number->string
;
: adsat-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen adsat-func-cb
  gen target@
  gen adsat-origin-cb
  #f
  map-chan-over-target-with-sync
;
: adsat-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen size@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init-size ) gen size!
  gen sliders@ 0 array-ref '( FXmNvalue gen size@ ) FXtVaSetValues drop
;
: adsat-size-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) size!
;
: post-adsat-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen adsat-ok-cb
    gen label@ $" Move the slider to change the saturation scaling factor." _ help-cb
    gen adsat-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( $" adaptive saturation size" _ 0 gen size@ 10 gen adsat-size-cb 1 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-adsat-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  4 gen size!
  gen post-adsat-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%d)" '( gen label@ gen size@ ) string-format change-label
;
previous

\ === Sample rate conversion (resample) ===

hide
: src-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen target@ 'sound equal? if
    gen amount@ 1.0 undef undef undef src-sound drop
  else
    gen target@ 'selection equal? if
      selection? if
	gen amount@ 1.0 src-selection drop
      else
	$" no selection" _ snd-warning drop
      then
    else
      $" can't apply src between marks yet" _ snd-warning drop
    then
  then
;
: src-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init-amount ) gen amount!
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@ 100.0 f* f>s ) FXtVaSetValues drop
;
: src-amount-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amount!
;
: post-src-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen src-ok-cb
    gen label@ $" Move the slider to change the sample rate.  \
Values greater than 1.0 speed up file play, negative values reverse it." _ help-cb
    gen src-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( $" sample rate" _ -2.0 gen amount@ 2.0 gen src-amount-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-src-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  0.0 gen amount!
  gen post-src-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen amount@ ) string-format change-label
;
previous

\ === Time and pitch scaling by granular synthesis and sampling rate conversion ===

hide
effects-base%
  cell% field time-scale
  cell% field ramp-scale
  cell% field pitch-scale
  cell% field segment-length
end-struct expsrc%

: time-scale@ 	  ( gen -- scl ) time-scale @ ;
: time-scale! 	  ( scl gen -- ) time-scale ! ;
: ramp-scale@ 	  ( gen -- scl ) ramp-scale @ ;
: ramp-scale! 	  ( scl gen -- ) ramp-scale ! ;
: pitch-scale@    ( gen -- scl ) pitch-scale @ ;
: pitch-scale!    ( scl gen -- ) pitch-scale ! ;
: segment-length@ ( gen -- len ) segment-length @ ;
: segment-length! ( len gen -- ) segment-length ! ;

: expsrc-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  selected-sound { snd }
  snd save-controls drop
  snd reset-controls drop
  gen pitch-scale@ snd set-speed-control drop
  gen pitch-scale@ gen time-scale@ f* { new-time }
  new-time 1.0 f<> if
    #t                  snd set-expand-control?       drop
    new-time            snd set-expand-control        drop
    gen size@           snd set-expand-control-hop    drop
    gen segment-length@ snd set-expand-control-length drop
    gen ramp-scale@     snd set-expand-control-ramp   drop
  then
  gen target@ 'marks equal? if
    plausible-mark-samples { pts }
    pts if
      snd 0  pts car  pts cadr pts car - 1+ apply-controls drop
    else
      $" no marks" _ snd-warning drop
    then
  else
    snd gen target@ 'sound equal? if 0 else 2 then undef undef apply-controls drop
  then
  snd restore-controls drop
;
: expsrc-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create
  gen ,
  gen time-scale@ ,
  gen size@ ,
  gen segment-length@ ,
  gen ramp-scale@ ,
  gen pitch-scale@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-time-scale )  gen time-scale!
  self 2 cells + @ ( init-size )        gen size!
  self 3 cells + @ ( init-seg-len )     gen segment-length!
  self 4 cells + @ ( init-ramp-scale )  gen ramp-scale!
  self 5 cells + @ ( init-pitch-scale ) gen pitch-scale!
  gen sliders@ 0 array-ref '( FXmNvalue gen time-scale@     100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen size@           100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen segment-length@ 100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 3 array-ref '( FXmNvalue gen ramp-scale@     100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 4 array-ref '( FXmNvalue gen pitch-scale@    100.0 f* f>s ) FXtVaSetValues drop
;
: expsrc-ts-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) time-scale!
;
: expsrc-hs-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) size!
;
: expsrc-sl-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) segment-length!
;
: expsrc-rs-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) ramp-scale!
;
: expsrc-ps-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) pitch-scale!
;
: post-expsrc-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen expsrc-ok-cb
    gen label@ $" Move the slider to change the time/pitch scaling parameter." _ help-cb
    gen expsrc-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" time scale"     _ 0.0 gen time-scale@     5.0 gen expsrc-ts-cb 100 )
       #( $" hop size"       _ 0.0 gen size@           1.0 gen expsrc-hs-cb 100 )
       #( $" segment-length" _ 0.0 gen segment-length@ 0.5 gen expsrc-sl-cb 100 )
       #( $" ramp scale"     _ 0.0 gen ramp-scale@     0.5 gen expsrc-rs-cb 100 )
       #( $" pitch scale"    _ 0.0 gen pitch-scale@    5.0 gen expsrc-ps-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-expsrc-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) expsrc% %alloc make-base-effects { gen }
  1.00 gen time-scale!
  0.05 gen size!
  0.15 gen segment-length!
  0.50 gen ramp-scale!
  1.00 gen pitch-scale!
  gen post-expsrc-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f)" '( gen label@ gen time-scale@ gen pitch-scale@ ) string-format change-label
;
previous

\ === Time-varying sample rate conversion (resample) ===
\ (KSM)

hide
: src-timevar-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen envel@ xe-envelope gen scaler@ scale-envelope { en }
  gen target@ 'sound equal? if
    en 1.0 #f #f #f src-sound drop
  else
    gen target@ 'selection equal? if
      selected-sound #f selection-member? if
	  en 1.0 src-selection drop
      else
	$" no selection" _ snd-warning drop
      then
    else
      plausible-mark-samples { pts }
      pts if
	pts car   { beg }
	pts cadr  { end }
	end beg - { len }
	:envelope en :end len make-env beg len selected-sound #f #f src-channel drop
      else
	$" no marks" _ snd-warning drop
      then
    then
  then
;
: src-timevar-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen scaler@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init ) gen scaler!
  gen envel@ '( 0.0 1.0 1.0 1.0 ) set-xe-envelope
  gen sliders@ 0 array-ref '( FXmNvalue gen scaler@ 100.0 f* f>s ) FXtVaSetValues drop
;
: src-timevar-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) scaler!
;
: post-src-timevar-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen src-timevar-ok-cb
    gen label@ $" Move the slider to change the src-timevar scaling amount." _ help-cb
    gen src-timevar-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( $" Resample factor" _ 0.0 gen scaler@ 10.0 gen src-timevar-slider-cb 100 ) )
    add-sliders gen sliders!
    "fr" FxmFrameWidgetClass gen sliders@ 0 array-ref FXtParent FXtParent
    '( FXmNheight          200
       FXmNleftAttachment  FXmATTACH_FORM
       FXmNrightAttachment FXmATTACH_FORM
       FXmNtopAttachment   FXmATTACH_WIDGET
       FXmNtopWidget       gen sliders@ -1 array-ref
       FXmNshadowThickness 4
       FXmNshadowType      FXmSHADOW_ETCHED_OUT ) undef FXtCreateManagedWidget { fr }
    gen sliders@ 0 array-ref FXtParent FXtParent gen target-cb #f add-target { target-row }
    gen dialog@ activate-dialog
    gen label@ string-downcase
    fr
    '( 0.0 1.0 1.0 1.0 ) ( envelope )
    '( 0.0 1.0 0.0 1.0 ) ( axis-bounds )
    '( FXmNheight 200 ) make-xenved gen envel!
    fr '( FXmNbottomAttachment FXmATTACH_WIDGET FXmNbottomWidget target-row ) FXtVaSetValues drop
  else
    gen dialog@ activate-dialog
  then
;
set-current

: make-src-timevar-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  1.0 gen scaler!
  #f  gen envel!
  gen post-src-timevar-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" Time-varying sample rate scaling" change-label
;
previous

\ === MODULATION EFFECTS ===

hide
: effects-am-env-cb { os e -- prc; x self -- res }
  1 proc-create e , os ,
 does> ( x self -- res )
  { inval self }
  self @ { e }
  self cell+ @ { os }
  1.0 inval e env os 0.0 0.0 oscil f* amplitude-modulate
;
: effects-am-cb ( os -- prc; x self -- res )
  1 proc-create swap ,
 does> ( x self -- res )
  { inval self }
  self @ { os }
  os 0.0 0.0 oscil inval f*
;
: effects-rm-env-cb { os e -- prc; x self -- res }
  1 proc-create e , os ,
 does> ( x self -- res )
  { inval self }
  self @ { e }
  self cell+ @ { os }
  os 0.0 0.0 oscil e env f* inval f*
;
: effects-rm-cb ( os -- prc; x self -- res )
  1 proc-create swap ,
 does> ( x self -- res )
  { inval self }
  self @ { os }
  1.0 inval os 0.0 0.0 oscil amplitude-modulate
;
set-current
: effects-am <{ freq en :optional beg 0 dur #f snd #f chn #f -- res }>
  freq make-oscil { os }
  en list? if :envelope en :end dur 1- make-env else #f then { e }
  $" %s %s %s %s %s" '( freq en beg dur get-func-name ) string-format { origin }
  e if os e effects-am-env-cb else os effects-am-cb then beg dur snd chn #f origin map-channel
;

: effects-rm <{ freq en :optional beg 0 dur #f snd #f chn #f -- res }>
  freq make-oscil { os }
  en list? if :envelope en :end dur 1- make-env else #f then { e }
  $" %s %s %s %s %s" '( freq en beg dur get-func-name ) string-format { origin }
  e if os e effects-rm-env-cb else os effects-rm-cb then beg dur snd chn #f origin map-channel
;
previous

\ === Amplitude modulation ===

hide
: am-func-cb ( gen -- prc; samps self -- proc )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen amount@ make-oscil { os }
  gen envel@ xe-envelope '( 0.0 1.0 1.0 1.0 ) equal? if
    #f
  else
    :envelope gen envel@ xe-envelope :end gen target@ effect-frames 1- make-env
  then { e }
  e if os e effects-am-env-cb else os effects-am-cb then
;
: am-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  "effects-am"
  $" %s %s" '( gen amount@
  gen envel@ xe-envelope '( 0.0 1.0 1.0 1.0 ) equal? if #f else gen envel@ xe-envelope then )
  string-format
;
: am-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen am-func-cb
  gen target@
  gen am-origin-cb
  #f
  map-chan-over-target-with-sync
;
: am-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self cell+ @ ( init ) gen amount!
  gen envel@ '( 0.0 1.0 1.0 1.0 ) set-xe-envelope
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@ f>s ) FXtVaSetValues drop
;
: am-slider-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) amount!
;
: post-am-effect-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen am-ok-cb
    gen label@ $" Move the slider to change the modulation amount." _ help-cb
    gen am-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( $" amplitude modulation" _ 0.0 gen amount@ 1000.0 gen am-slider-cb 1 ) )
    add-sliders gen sliders!
    "fr" FxmFrameWidgetClass gen sliders@ 0 array-ref FXtParent FXtParent
    '( FXmNheight          200
       FXmNleftAttachment  FXmATTACH_FORM
       FXmNrightAttachment FXmATTACH_FORM
       FXmNtopAttachment   FXmATTACH_WIDGET
       FXmNtopWidget       gen sliders@ -1 array-ref
       FXmNshadowThickness 4
       FXmNshadowType      FXmSHADOW_ETCHED_OUT ) undef FXtCreateManagedWidget { fr }
    gen sliders@ 0 array-ref FXtParent FXtParent gen target-cb #f add-target { target-row }
    gen dialog@ activate-dialog
    gen label@ string-downcase
    fr
    '( 0.0 1.0 1.0 1.0 ) ( envelope )
    '( 0.0 1.0 0.0 1.0 ) ( axis-bounds )
    '( FXmNheight 200 ) make-xenved gen envel!
    fr '( FXmNbottomAttachment FXmATTACH_WIDGET FXmNbottomWidget target-row ) FXtVaSetValues drop
  else
    gen dialog@ activate-dialog
  then
;
set-current

: make-am-effect-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  100.0 gen amount!
  #f  gen envel!
  gen post-am-effect-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen amount@ ) string-format change-label
;
previous

\ === Ring modulation ===

hide
: rm-func-cb ( gen -- prc; samps self -- proc )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen frequency@ make-oscil { os }
  gen envel@ xe-envelope '( 0.0 1.0 1.0 1.0 ) equal? if
    #f
  else
    :envelope gen envel@ xe-envelope :end gen target@ effect-frames 1- make-env
  then { e }
  e if os e effects-rm-env-cb else os effects-rm-cb then
;
: rm-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  "effects-rm"
  $" %s %s" '( gen frequency@
  gen envel@ xe-envelope '( 0.0 1.0 1.0 1.0 ) equal? if #f else gen envel@ xe-envelope then )
  string-format
;
: rm-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen rm-func-cb
  gen target@
  gen rm-origin-cb
  #f
  map-chan-over-target-with-sync
;
: rm-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen frequency@ , gen scaler@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-freq )    gen frequency!
  self 2 cells + @ ( init-radians ) gen scaler!
  gen envel@ '( 0.0 1.0 1.0 1.0 ) set-xe-envelope
  gen sliders@ 0 array-ref '( FXmNvalue gen frequency@ f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen scaler@    f>s ) FXtVaSetValues drop
;
: rm-freq-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) frequency!
;
: rm-radians-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) scaler!
;
: post-rm-effect-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen rm-ok-cb
    gen label@ $" Move the slider to change ring modulation parameters." _ help-cb
    gen rm-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" modulation frequency" _ 0 gen frequency@ 1000 gen rm-freq-cb    1 )
       #( $" modulation radians"   _ 0 gen scaler@     360 gen rm-radians-cb 1 ) )
    add-sliders gen sliders!
    "fr" FxmFrameWidgetClass gen sliders@ 0 array-ref FXtParent FXtParent
    '( FXmNheight          200
       FXmNleftAttachment  FXmATTACH_FORM
       FXmNrightAttachment FXmATTACH_FORM
       FXmNtopAttachment   FXmATTACH_WIDGET
       FXmNtopWidget       gen sliders@ -1 array-ref
       FXmNshadowThickness 4
       FXmNshadowType      FXmSHADOW_ETCHED_OUT ) undef FXtCreateManagedWidget { fr }
    gen sliders@ 0 array-ref FXtParent FXtParent gen target-cb #f add-target { target-row }
    gen dialog@ activate-dialog
    gen label@ string-downcase
    fr
    '( 0.0 1.0 1.0 1.0 ) ( envelope )
    '( 0.0 1.0 0.0 1.0 ) ( axis-bounds )
    '( FXmNheight 200 ) make-xenved gen envel!
    fr '( FXmNbottomAttachment FXmATTACH_WIDGET FXmNbottomWidget target-row ) FXtVaSetValues drop
  else
    gen dialog@ activate-dialog
  then
;
set-current

: make-rm-effect-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  100.0 gen frequency!
  100.0 gen scaler!
  #f    gen envel!
  gen post-rm-effect-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f)" '( gen label@ gen frequency@ gen scaler@ ) string-format change-label
;
previous

\ === REVERBS ===

: effects-jc-reverb ( samps volume -- prc; inval self -- res )
  { samps vol }
  -0.7 0.7 1051 make-all-pass { all1 }
  -0.7 0.7  337 make-all-pass { all2 }
  -0.7 0.7  113 make-all-pass { all3 }
  0.742 4799 make-comb { c1 }
  0.733 4999 make-comb { c2 }
  0.715 5399 make-comb { c3 }
  0.697 5801 make-comb { c4 }
  #f srate 0.013 f* fround->s make-delay { outdel }
  1 proc-create
  0 ( samp ),
  samps ,
  vol ,
  0.0 ( comb-sum ) ,
  0.0 ( comb-sum-1 ) ,
  0.0 ( comb-sum-2 ) ,
  all1 , all2 , all3 ,
  c1 , c2 , c3 , c4 ,
  outdel ,
 does> ( inval self -- res )
  { inval self }
  self @ ( samp++ ) 1+ self !
  self @ { samp }
  self  1 cells + @ { samps }
  self  2 cells + @ { volume }
  self 	3 cells + @ { comb-sum }
  self 	4 cells + @ { comb-sum-1 }
  self 	5 cells + @ { comb-sum-2 }
  self 	6 cells + @ { allpass1 }
  self 	7 cells + @ { allpass2 }
  self 	8 cells + @ { allpass3 }
  self 	9 cells + @ { comb1 }
  self 10 cells + @ { comb2 }
  self 11 cells + @ { comb3 }
  self 12 cells + @ { comb4 }
  self 13 cells + @ { outdel }
  allpass3 allpass2 allpass1
  samp samps <= if inval else 0.0 then 0.0 all-pass 0.0 all-pass 0.0 all-pass { allpass-sum }
  comb-sum-1 self 5 cells + ! ( comb-sum-2 )
  comb-sum   self 4 cells + ! ( comb-sum-1 )
  comb1 allpass-sum 0.0 comb
  comb2 allpass-sum 0.0 comb f+
  comb3 allpass-sum 0.0 comb f+
  comb4 allpass-sum 0.0 comb f+ self 3 cells + ! ( comb-sum )
  outdel comb-sum 0.0 delay volume f* inval f+
;

: effects-jc-reverb-1 <{ vol :optional beg 0 dur #f snd #f  chn #f -- res }>
  dur if dur else snd chn #f frames then { samps }
  $" %s %s %s %s" '( vol beg dur get-func-name ) string-format { origin }
  samps vol effects-jc-reverb beg dur snd chn #f origin map-channel
;

hide
: cnv-cb ( sf -- prc; dir self -- res )
  1 proc-create swap ,
 does> ( dir self -- res )
  { dir self }
  self @ ( sf ) next-sample
;
: cnv-vct-cb { cnv sf -- prc; self -- res }
  sf cnv-cb { func }
  0 proc-create func , cnv ,
 does> ( self -- res )
  { self }
  self @ { func }
  self cell+ @ { cnv }
  cnv func convolve
;
set-current
: effects-cnv <{ snd0 amp :optional snd #f chn #f -- res }>
  snd0 sound? unless sounds car to snd0 then
  snd0 #f #f frames { flt-len }
  snd chn #f frames flt-len + { total-len }
  :filter 0 flt-len snd0 #f #f channel->vct make-convolve { cnv }
  0 snd chn 1 #f make-sample-reader { sf }
  total-len 0.0 make-vct { out-data }
  cnv sf cnv-vct-cb { func }
  out-data func vct-map! drop
  sf free-sample-reader drop
  out-data amp vct-scale! drop
  out-data vct-peak { max-samp }
  $" %s %s %s" '( snd0 amp get-func-name ) string-format { origin }
  out-data 0 total-len snd chn #f origin vct->channel drop
  max-samp 1.0 f> if '( max-samp fnegate max-samp ) snd chn set-y-bounds drop then
  max-samp
;
previous

\ === Reverb from Michael McNabb's Nrev ===

hide
effects-base%
  cell% field reverb-filter
  cell% field reverb-feedback
end-struct nrev-reverb%

: reverb-filter@   ( gen -- val ) reverb-filter @ ;
: reverb-filter!   ( val gen -- ) reverb-filter ! ;
: reverb-feedback@ ( gen -- val ) reverb-feedback @ ;
: reverb-feedback! ( val gen -- ) reverb-feedback ! ;

: nrev-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  selected-sound { snd }
  snd save-controls drop
  snd reset-controls drop
  #t                   snd set-reverb-control?         drop
  gen amount@          snd set-reverb-control-scale    drop
  gen reverb-filter@   snd set-reverb-control-lowpass  drop
  gen reverb-feedback@ snd set-reverb-control-feedback drop
  gen target@ 'marks equal? if
    plausible-mark-samples { pts }
    pts list? if
      snd 0  pts car  pts cadr pts car - 1+ apply-controls drop
    else
      $" no marks" _ snd-warning drop
    then
  else
    snd gen target@ 'sound equal? if 0 else 2 then undef undef apply-controls drop
  then
  snd restore-controls drop
;
: nrev-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ , gen reverb-filter@ , gen reverb-feedback@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-amount )   gen amount!
  self 2 cells + @ ( init-filter )   gen reverb-filter!
  self 3 cells + @ ( init-feedback ) gen reverb-feedback!
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@          100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen reverb-filter@   100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen reverb-feedback@ 100.0 f* f>s ) FXtVaSetValues drop
;
: nrev-amount-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amount!
;
: nrev-filter-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) reverb-filter!
;
: nrev-feedback-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) reverb-feedback!
;
: post-reverb-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen nrev-ok-cb
    gen label@ $" Reverberator from Michael McNabb.  \
Adds reverberation scaled by reverb amount, lowpass filtering, and feedback.  \
Move the sliders to change the reverb parameters." _ help-cb
    gen nrev-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" reverb amount"   _ 0.0 gen amount@          1.00 gen nrev-amount-cb   100 )
       #( $" reverb filter"   _ 0.0 gen reverb-filter@   1.00 gen nrev-filter-cb   100 )
       #( $" reverb feedback" _ 0.0 gen reverb-feedback@ 1.25 gen nrev-feedback-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent FXtParent gen target-cb #f add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-reverb-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) nrev-reverb% %alloc make-base-effects { gen }
  0.10 gen amount!
  0.50 gen reverb-filter!
  1.09 gen reverb-feedback!
  gen post-reverb-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f %.2f)"
  '( gen label@ gen amount@ gen reverb-filter@ gen reverb-feedback@ ) string-format change-label
;
previous

\ === Chowning reverb ===

hide
effects-base%
  cell% field jc-reverb-decay
  cell% field jc-reverb-volume
end-struct jc-reverb%

: jc-reverb-decay@  ( gen -- val ) jc-reverb-decay @ ;
: jc-reverb-decay!  ( val gen -- ) jc-reverb-decay ! ;
: jc-reverb-volume@ ( gen -- val ) jc-reverb-volume @ ;
: jc-reverb-volume! ( val gen -- ) jc-reverb-volume ! ;

: jc-func-cb ( gen -- prc; samps self -- proc )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  samps gen jc-reverb-volume@ effects-jc-reverb
;
: jc-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  "effects-jc-reverb-1"
  gen jc-reverb-volume@ number->string
;
: jc-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen jc-func-cb
  gen target@
  gen jc-origin-cb
  gen truncate@ if #f else gen jc-reverb-decay@ then map-chan-over-target-with-sync
;
: jc-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen jc-reverb-decay@ , gen jc-reverb-volume@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-decay )  gen jc-reverb-decay!
  self 2 cells + @ ( init-volume ) gen jc-reverb-volume!
  gen sliders@ 0 array-ref '( FXmNvalue gen jc-reverb-decay@  100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen jc-reverb-volume@ 100.0 f* f>s ) FXtVaSetValues drop
;
: jc-decay-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) jc-reverb-decay!
;
: jc-volume-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) jc-reverb-volume!
;
: post-jc-reverb-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen jc-ok-cb
    gen label@ $" Nice reverb from John Chowning.  \
Move the sliders to set the reverb parameters." _ help-cb
    gen jc-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" decay duration" _ 0.0 gen jc-reverb-decay@  10.0 gen jc-decay-cb  100 )
       #( $" reverb volume"  _ 0.0 gen jc-reverb-volume@ 1.00 gen jc-volume-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent FXtParent gen target-cb gen truncate-cb add-target drop
  then
  gen dialog@ activate-dialog
;
set-current

: make-jc-reverb-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) jc-reverb% %alloc make-base-effects { gen }
  2.0 gen jc-reverb-decay!
  0.1 gen jc-reverb-volume!
  gen post-jc-reverb-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f)" '( gen label@ gen jc-reverb-decay@ gen jc-reverb-volume@ ) string-format
  change-label
;
previous

\ === Convolution ===

hide
effects-base%
  cell% field convolve-one
  cell% field convolve-two
end-struct effects-convolve%

: convolve-one@ ( gen -- val ) convolve-one @ ;
: convolve-one! ( val gen -- ) convolve-one ! ;
: convolve-two@ ( gen -- val ) convolve-two @ ;
: convolve-two! ( val gen -- ) convolve-two ! ;

: cnv-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen convolve-one@ { snd1 }
  gen convolve-two@ { snd2 }
  snd1 sound? if
    snd2 sound? if
      snd1 gen amplitude@ snd2 #f effects-cnv drop
    else
      $" no such sound two: %S" '( snd2 ) string-format snd-warning drop
    then
  else
    $" no such sound one: %S" '( snd1 ) string-format snd-warning drop
  then
;
: cnv-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen convolve-one@ , gen convolve-two@ , gen amplitude@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-one ) gen convolve-one!
  self 2 cells + @ ( init-two ) gen convolve-two!
  self 3 cells + @ ( init-amp ) gen amplitude!
  gen sliders@ 0 array-ref '( FXmNvalue gen convolve-one@           ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen convolve-two@           ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen amplitude@ 100.0 f* f>s ) FXtVaSetValues drop
;
: cnv-one-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) convolve-one!
;
: cnv-two-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) convolve-two!
;
: cnv-amp-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amplitude!
;
: post-convolve-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen cnv-ok-cb
    gen label@ $" Very simple convolution.  \
Move the sliders to set the reverb parameters the numbers of the soundfiles \
to be convolved and the amount for the amplitude scaler.  \
Output will be scaled to floating-point values, \
resulting in very large (but not clipped) amplitudes.  \
Use the Normalize amplitude effect to rescale the output.  \
The convolution data file typically defines a natural reverberation source, \
and the output from this effect can provide very striking reverb effects.  \
You can find convolution data files on sites listed at \
http://www.bright.net/~dlphilp/linux_csound.html under Impulse Response Data." _ help-cb
    gen cnv-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" impulse response file" _   0 gen convolve-one@   24 gen cnv-one-cb   1 )
       #( $" sound file"            _   0 gen convolve-two@   24 gen cnv-two-cb   1 )
       #( $" amplitude"             _ 0.0 gen amplitude@    0.10 gen cnv-amp-cb 100 ) )
    add-sliders gen sliders!
  then
  gen dialog@ activate-dialog
;
set-current

: make-convolve-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-convolve% %alloc make-base-effects { gen }
  0    gen convolve-one!
  1    gen convolve-two!
  0.01 gen amplitude!
  gen post-convolve-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%d %d %.2f)"
  '( gen label@ gen convolve-one@ gen convolve-two@ gen amplitude@ ) string-format change-label
;
previous

\ === VARIOUS AND MISCELLANEOUS ===

hide
: numb-cb { rd pos -- prc; y self -- res }
  1 proc-create pos , rd ,
 does> ( y self -- res )
  { y self }
  self cell+ @ ( rd ) read-sample self @ ( pos ) f* y f+
;
: env-numb-cb { rd en -- prc; y self -- res }
  1 proc-create en , rd ,
 does> ( y self -- res )
  { y self }
  self cell+ @ ( rd ) read-sample self @ ( en ) env f* y f+
;
: env-cb { rd en -- prc; y self -- res }
  1 proc-create en , rd ,
 does> ( y self -- res )
  { y self }
  self cell+ @ ( rd ) read-sample  1.0 self @ ( en ) env f-  f* y f+
;
set-current
: effects-position-sound <{ mono pos :optional snd #f chn #f -- res }>
  mono #f #f frames { len }
  0 mono #f 1 #f make-sample-reader { rd }
  $" %s %s %s" '( mono pos get-func-name ) string-format { origin }
  pos number? if
    rd pos numb-cb 0 len snd chn #f origin map-channel
  else
    :envelope pos :end len 1- make-env { e }
    chn number? chn 1 = && if
      rd e env-numb-cb 0 len snd chn #f origin map-channel
    else
      rd e env-cb 0 len snd chn #f origin map-channel
    then
  then
;
previous

: effects-place-sound ( mono stereo pan -- res )
  doc" Mixes a mono sound into a stereo sound, \
splitting it into two copies whose amplitudes depend on the envelope PAN-ENV.  \
If PAN-ENV is a number, the sound is split such that 0 is all in channel 0 \
and 90 is all in channel 1."
  { mono stereo pan }
  pan number? if
    pan 90.0 f/ { pos }
    mono pos        stereo 1 effects-position-sound
    mono 1.0 pos f- stereo 0 effects-position-sound
  else
    mono pan stereo 1 effects-position-sound
    mono pan stereo 0 effects-position-sound
  then
;

hide
: flange-cb { ri del -- prc; inval self -- res }
  1 proc-create del , ri ,
 does> ( inval self -- res )
  { inval self }
  self @ ( del ) inval  self cell+ @ ( ri ) 0.0 rand-interp  delay inval f+ 0.75 f*
;
set-current
: effects-flange <{ amnt speed time :optional beg 0 dur #f snd #f  chn #f  -- res}>
  :frequency speed :amplitude amnt make-rand-interp { ri }
  time snd srate f* fround->s { len }
  :size len :max-size amnt f>s len 1 + + make-delay { del }
  $" %s %s %s %s %s %s"
  '( amnt speed time beg
  dur number? snd chn #f frames dur <> && if dur else #f then
  get-func-name ) string-format { origin }
  ri del flange-cb  beg dur snd chn #f origin map-channel
;
previous

: effects-cross-synthesis ( snd amp fftsize r -- prc; inval self -- res )
  { snd amp fftsize r }
  fftsize 2/ { freq-inc }
  fftsize 0.0 make-vct { fdr }
  fftsize 0.0 make-vct { fdi }
  freq-inc 0.0 make-vct { spectr }
  1.0 r fftsize f/ f- { radius }
  #f srate fftsize / { bin }
  freq-inc nil make-array map! :radius radius :frequency bin i * make-formant end-map { formants }
  1 proc-create ( inctr ) 0 , ( ctr ) freq-inc , fdr , fdi , spectr , formants , snd , amp ,
 does> ( inval self -- res )
  { inval self }
  self @ { inctr }
  self 1 cells + @ { ctr }
  self 2 cells + @ { fdr }
  self 3 cells + @ { fdi }
  self 4 cells + @ { spectr }
  self 5 cells + @ { formants }
  self 6 cells + @ { snd }
  self 7 cells + @ { amp }
  fdr vct-length { fftsize }
  spectr vct-length { freq-inc }
  0.0 { outval }
  ctr freq-inc = if
    inctr fftsize snd 0 #f channel->vct self 2 cells + ! ( fdr )
    inctr freq-inc + self ! ( inctr )
    fdr fdi #f 2 spectrum drop
    fdr spectr vct-subtract! drop
    fdr freq-inc 1/f vct-scale! drop
    0 self 1 cells + ! ( ctr=0 )
  then
  ctr 1+ self 1 cells + ! ( ctr++ )
  spectr fdr 0 vct-add! drop
  spectr formants inval formant-bank amp f*
;

: effects-cross-synthesis-1 <{ csnd amp fftsize r :optional beg 0 dur #f snd #f  chn #f -- res }>
  { csnd amp fftsize r beg dur snd chn }
  $" %s %s %s %s %s %s %s" '( csnd amp fftsize r beg dur get-func-name ) string-format { origin }
  csnd sound? unless sounds car to csnd then
  csnd amp fftsize r effects-cross-synthesis beg dur snd chn #f origin map-channel
;

hide
: src-fp-read-cb ( sf -- prc; dir self -- samp )
  1 proc-create swap ,
 does> ( dir self -- samp )
  { dir self }
  self @ ( sf ) dir 0> if next-sample else previous-sample then
;
: vct-fp-cb ( os sr sf amp -- prc; self -- res )
  { os sr sf amp }
  sf src-fp-read-cb { src-cb }
  0 proc-create os , sr , amp , src-cb ,
 does> ( self -- res )
  { self }
  self cell+ @ ( sr )
  self @ ( os ) 0.0 0.0 oscil self 2 cells + @ ( amp ) f*
  self 3 cells + @ ( src-cb ) src
;
set-current
: effects-fp <{ srf amp freq :optional beg 0 dur #f snd #f  chn #f -- vct }>
  freq make-oscil { os }
  :srate srf make-src { sr }
  beg snd chn 1 #f make-sample-reader { sf }
  dur if dur else snd chn #f frames then { len }
  len 0.0 make-vct { out-data }
  out-data   os sr sf amp vct-fp-cb   vct-map! drop
  $" %s %s %s %s %s %s" '( srf amp freq beg dur get-func-name ) string-format { origin }
  out-data beg len snd chn #f origin vct->channel
;
previous

hide
: hello-src-cb { in-data idx -- prc; dir self -- samp }
  1 proc-create idx , in-data ,
 does> ( dir self -- samp )
  { dir self }
  self @ { idx }
  self cell+ @ { in-data }
  in-data idx range? if in-data idx vct-ref else 0.0 then ( val )
  idx dir + self ! ( idx )
;
set-current
: effects-hello-dentist <{ freq amp :optional beg 0 dur #f snd #f  chn #f -- res }>
  :frequency freq :amplitude amp make-rand-interp { rn }
  0 { idx }
  dur if dur else snd chn #f frames then { len }
  beg len snd chn #f channel->vct { in-data }
  amp f2* 1.0 f+ len f* fround->s ( out-len ) 0.0 make-vct { out-data }
  :srate 1.0 :input in-data idx hello-src-cb make-src { rd }
  out-data map!
    idx len = ?leave
    rd  rn  0.0 rand-interp  #f src
  end-map to out-data
  $" %s %s %s %s %s" '( freq amp beg dur get-func-name ) string-format { origin }
  out-data beg out-data vct-length snd chn #f origin vct->channel
;
previous

\ === Place sound ===

hide
effects-base%
  cell% field mono-snd
  cell% field stereo-snd
  cell% field pan-pos
end-struct effects-place-sound%

: mono-snd@   ( gen -- snd ) mono-snd @ ;
: mono-snd!   ( snd gen -- ) mono-snd ! ;
: stereo-snd@ ( gen -- snd ) stereo-snd @ ;
: stereo-snd! ( snd gen -- ) stereo-snd ! ;
: pan-pos@    ( gen -- pos ) pan-pos @ ;
: pan-pos!    ( pos gen -- ) pan-pos ! ;

: ps-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen envel@ xe-envelope { e }
  e '( 0.0 1.0 1.0 1.0 ) equal? if
    gen mono-snd@ gen stereo-snd@ gen pan-pos@ effects-place-sound drop
  else
    gen mono-snd@ gen stereo-snd@ e            effects-place-sound drop
  then
;
: ps-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen mono-snd@ , gen stereo-snd@ , gen pan-pos@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-mono )   gen mono-snd!
  self 2 cells + @ ( init-stereo ) gen stereo-snd!
  self 3 cells + @ ( init-pos )    gen pan-pos!
  gen envel@ '( 0.0 1.0 1.0 1.0 ) set-xe-envelope
  gen sliders@ 0 array-ref '( FXmNvalue gen mono-snd@   ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen stereo-snd@ ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen pan-pos@    ) FXtVaSetValues drop
;
: ps-mono-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) mono-snd!
;
: ps-stereo-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) stereo-snd!
;
: ps-pos-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) pan-pos!
;
: post-place-sound-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> { w c info self -- }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen ps-ok-cb
    gen label@ $" Mixes mono sound into stereo sound field." _ help-cb
    gen ps-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" mono sound"   _ 0 gen mono-snd@   50 gen ps-mono-cb   1 )
       #( $" stereo sound" _ 0 gen stereo-snd@ 50 gen ps-stereo-cb 1 )
       #( $" pan position" _ 0 gen pan-pos@    90 gen ps-pos-cb    1 ) ) add-sliders gen sliders!
    "fr" FxmFrameWidgetClass gen sliders@ 0 array-ref FXtParent FXtParent
    '( FXmNheight          200
       FXmNleftAttachment  FXmATTACH_FORM
       FXmNrightAttachment FXmATTACH_FORM
       FXmNtopAttachment   FXmATTACH_WIDGET
       FXmNtopWidget       gen sliders@ -1 array-ref
       FXmNshadowThickness 4
       FXmNshadowType      FXmSHADOW_ETCHED_OUT ) undef FXtCreateManagedWidget { fr }
    gen dialog@ activate-dialog
    "panning" fr
    '( 0.0 1.0 1.0 1.0 ) ( envelope )
    '( 0.0 1.0 0.0 1.0 ) ( axis-bounds )
    '( FXmNheight 200 ) make-xenved gen envel!
  else
    gen dialog@ activate-dialog
  then
;
set-current

: make-place-sound-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-place-sound% %alloc make-base-effects { gen }
  0  gen mono-snd!
  1  gen stereo-snd!
  45 gen pan-pos!
  gen post-place-sound-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%d %d %d)"
  '( gen label@ gen mono-snd@ gen stereo-snd@ gen pan-pos@ ) string-format change-label
;
previous

\ === Insert silence (at cursor, silence-amount in secs) ===

hide
: silence-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  #f #f #f cursor #f srate gen amount@ f* f>s #f #f insert-silence drop
;
: silence-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init ) gen amount!
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@ 100.0 f* f>s ) FXtVaSetValues drop
;
: silence-amount-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amount!
;
: post-silence-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen silence-ok-cb
    gen label@
    $" Move the slider to change the number of seconds of silence added at the cursor position." _
    help-cb
    gen silence-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( "silence" _ 0.0 gen amount@ 5.0 gen silence-amount-cb 100 ) )
    add-sliders gen sliders!
  then
  gen dialog@ activate-dialog
;
set-current

: make-silence-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  1.0 gen amount!
  gen post-silence-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen amount@ ) string-format change-label
;
previous

\ === Contrast (brightness control) ===

hide
: contrast-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  #f #f #f maxamp { peak }
  selected-sound { snd }
  snd save-controls drop
  snd reset-controls drop
  #t          snd set-contrast-control?       drop
  gen amount@ snd set-contrast-control        drop
  peak 1/f    snd set-contrast-control-amp    drop
  peak snd #f set-amp-control drop
  gen target@ 'marks equal? if
    plausible-mark-samples { pts }
    pts if
      snd 0  pts car  pts cadr pts car - 1+ apply-controls drop
    else
      $" no marks" _ snd-warning drop
    then
  else
    snd gen target@ 'sound equal? if 0 else 2 then 0 undef apply-controls drop
  then
  snd restore-controls drop
;
: contrast-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen amount@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init )  gen amount!
  gen sliders@ 0 array-ref '( FXmNvalue gen amount@ 100.0 f* f>s ) FXtVaSetValues drop
;
: contrast-amount-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amount!
;
: post-contrast-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen contrast-ok-cb
    gen label@ $" Move the slider to change the contrast intensity." _ help-cb
    gen contrast-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( $" contrast enhancement" _ 0.0 gen amount@ 10.0 gen contrast-amount-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-contrast-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  1.0 gen amount!
  gen post-contrast-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen amount@ ) string-format change-label
;
previous

\ === Cross synthesis ===

hide
effects-base%
  cell% field cross-synth-sound
  cell% field cross-synth-radius
  cell% field cross-synth-fft-widget
end-struct effects-cross%

: cs-sound@  	 ( gen -- snd ) cross-synth-sound @ ;
: cs-sound!  	 ( snd gen -- ) cross-synth-sound ! ;
: cs-radius@ 	 ( gen -- rad ) cross-synth-radius @ ;
: cs-radius! 	 ( rad gen -- ) cross-synth-radius ! ;
: cs-fft-widget@ ( gen -- wid ) cross-synth-fft-widget @ ;
: cs-fft-widget! ( wid gen -- ) cross-synth-fft-widget ! ;

: cs-func-cb ( gen -- prc; samps self -- proc )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  gen cs-sound@ gen amplitude@ gen size@ gen cs-radius@ effects-cross-synthesis
;
: cs-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  "effects-cross-synthesis-1"
  $" %s %s %s %s" '( gen cs-sound@ gen amplitude@ gen size@ gen cs-radius@ ) string-format
;
: cs-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen cs-func-cb
  gen target@
  gen cs-origin-cb
  #f
  map-chan-over-target-with-sync
;
: cs-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen cs-sound@ , gen amplitude@ , gen size@ , gen cs-radius@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-snd )   gen cs-sound!
  self 2 cells + @ ( init-amp )   gen amplitude!
  self 3 cells + @ ( init-size )  gen size!
  self 4 cells + @ ( init-rad )   gen cs-radius!
  gen sliders@ 0 array-ref '( FXmNvalue gen cs-sound@               ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen amplitude@ 100.0 f* f>s ) FXtVaSetValues drop
  gen cs-fft-widget@
  use-combo-box-for-fft-size if
    '( FXmNselectedPosition 1 ) FXtVaSetValues
  else
    #t #t FXmToggleButtonSetState
  then drop
  gen sliders@ 2 array-ref '( FXmNvalue gen cs-radius@ 100.0 f* f>s ) FXtVaSetValues drop
;
: cs-snd-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue self @ ( gen ) cs-sound!
;
: cs-amp-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amplitude!
;
: cs-rad-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) cs-radius!
;
: cs-sel-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fitem_or_text ( selected ) FXmFONTLIST_DEFAULT_TAG FXmStringGetLtoR \ size-as-list '(#t n)
  cadr self @ ( gen ) size!
;
: cs-sel-changed-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w size info self }
  info Fset if size self @ ( gen ) size! then
;
: post-cross-synth-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen cs-ok-cb
    gen label@ $" The sliders set the number of the soundfile to be cross-synthesized, \
the synthesis amplitude, the FFT size, and the radius value." _ help-cb
    gen cs-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" input sound" _   0 gen cs-sound@     20 gen cs-snd-cb   1 )
       #( $" amplitude"   _ 0.0 gen amplitude@   1.0 gen cs-amp-cb 100 )
       #( $" radius"      _ 0.0 gen cs-radius@ 360.0 gen cs-rad-cb 100 ) ) add-sliders gen sliders!
    '( 64 128 256 512 1024 4096 ) { sizes }
    $" FFT size" _ FXmStringCreateLocalized { s1 }
    "frame" FxmFrameWidgetClass gen sliders@ 0 array-ref FXtParent
    '( FXmNborderWidth   1
       FXmNshadowType    FXmSHADOW_ETCHED_IN
       FXmNpositionIndex 2 ) undef FXtCreateManagedWidget { frame }
    "frm" FxmFormWidgetClass frame
    '( FXmNleftAttachment   FXmATTACH_FORM
       FXmNrightAttachment  FXmATTACH_FORM
       FXmNtopAttachment    FXmATTACH_FORM
       FXmNbottomAttachment FXmATTACH_FORM
       FXmNbackground       basic-color ) undef FXtCreateManagedWidget { frm }
    use-combo-box-for-fft-size if
      $" FFT size" _ FxmLabelWidgetClass frm
      '( FXmNleftAttachment   FXmATTACH_FORM
	 FXmNrightAttachment  FXmATTACH_NONE
	 FXmNtopAttachment    FXmATTACH_FORM
	 FXmNbottomAttachment FXmATTACH_FORM
	 FXmNlabelString      s1
	 FXmNbackground       basic-color ) undef FXtCreateManagedWidget { lab }
      sizes map! *key* number->string FXmStringCreateLocalized end-map { fft-labels }
      "fftsize" FxmComboBoxWidgetClass frm
      '( FXmNleftAttachment   FXmATTACH_WIDGET
	 FXmNleftWidget       lab
	 FXmNrightAttachment  FXmATTACH_FORM
	 FXmNtopAttachment    FXmATTACH_FORM
	 FXmNbottomAttachment FXmATTACH_FORM
	 FXmNitems            fft-labels
	 FXmNitemCount        fft-labels length
	 FXmNcomboBoxType     FXmDROP_DOWN_COMBO_BOX
	 FXmNbackground       basic-color ) undef FXtCreateManagedWidget { combo }
      combo gen cs-fft-widget!
      fft-labels each FXmStringFree drop end-each
      combo '( FXmNselectedPosition 1 ) FXtVaSetValues drop
      combo FXmNselectionCallback gen cs-sel-cb undef FXtAddCallback drop
    else
      "rc" FxmRowColumnWidgetClass frm
      '( FXmNorientation      FXmHORIZONTAL
	 FXmNradioBehavior    #t
	 FXmNradioAlwaysOne   #t
	 FXmNentryClass       FxmToggleButtonWidgetClass
	 FXmNisHomogeneous    #t
	 FXmNleftAttachment   FXmATTACH_FORM
	 FXmNrightAttachment  FXmATTACH_FORM
	 FXmNtopAttachment    FXmATTACH_FORM
	 FXmNbottomAttachment FXmATTACH_NONE
	 FXmNbackground       basic-color ) undef FXtCreateManagedWidget { rc }
      $" FFT size" _ FxmLabelWidgetClass frm
      '( FXmNleftAttachment   FXmATTACH_FORM
	 FXmNrightAttachment  FXmATTACH_FORM
	 FXmNtopAttachment    FXmATTACH_WIDGET
	 FXmNtopWidget        rc
	 FXmNbottomAttachment FXmATTACH_FORM
	 FXmNlabelString      s1
	 FXmNalignment        FXmALIGNMENT_BEGINNING
	 FXmNbackground       basic-color ) undef FXtCreateManagedWidget { lab }
      sizes each { size }
	size number->string FxmToggleButtonWidgetClass rc
	'( FXmNbackground           basic-color
	   FXmNvalueChangedCallback '( gen cs-sel-changed-cb size )
	   FXmNset                  size gen size@ = ) undef FXtCreateManagedWidget { button }
	size gen size@ = if button gen cs-fft-widget! then
      end-each
    then
    s1 FXmStringFree drop
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-cross-synth-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-cross% %alloc make-base-effects { gen }
  1   gen cs-sound!
  0.5 gen amplitude!
  128 gen size!
  6.0 gen cs-radius!
  #f  gen cs-fft-widget!
  gen post-cross-synth-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%d %.2f %d %.2f)" '( gen label@ gen cs-sound@ gen amplitude@ gen size@ gen cs-radius@ )
  string-format change-label
;
previous

\ === Flange and phasing ===

hide
effects-base%
  cell% field flange-speed
  cell% field flange-time
end-struct effects-flange%

: flange-speed@ ( gen -- val ) flange-speed @ ;
: flange-speed! ( val gen -- ) flange-speed ! ;
: flange-time@  ( gen -- val ) flange-time @ ;
: flange-time!  ( val gen -- ) flange-time ! ;

: flange-func-cb ( gen -- prc; samps self -- prc; self -- )
  1 proc-create swap ,
 does> ( samps self -- proc )
  { samps self }
  self @ { gen }
  :frequency gen flange-speed@ :amplitude gen amount@ make-rand-interp { ri }
  gen flange-time@ #f srate f* fround->s { len }
  :size len :max-size gen amount@ 1.0 len f+ f+ f>s make-delay { del }
  1 proc-create del , ri ,
 does> ( inval self -- res )
  { inval self }
  self @ ( del ) inval self cell+ @ ( ri ) 0.0 rand-interp delay inval f+ 0.75 f*
;
: flange-origin-cb ( gen -- prc; target input-samps self -- name origin )
  2 proc-create swap ,
 does> ( target input-samps self -- name origin )
  { target samps self }
  self @ { gen }
  "effects-flange"
  $" %s %s %s" '( gen amount@ gen flange-speed@ gen flange-time@ ) string-format
;
: flange-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen flange-func-cb
  gen target@
  gen flange-origin-cb
  #f
  map-chan-over-target-with-sync
;
: flange-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen flange-speed@ , gen amount@ , gen flange-time@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-speed )  gen flange-speed!
  self 2 cells + @ ( init-amount ) gen amount!
  self 3 cells + @ ( init-time )   gen flange-time!
  gen sliders@ 0 array-ref '( FXmNvalue gen flange-speed@ 10.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen amount@       10.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen flange-time@ 100.0 f* f>s ) FXtVaSetValues drop
;
: flange-speed-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 10.0 f/ self @ ( gen ) flange-speed!
;
: flange-amount-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 10.0 f/ self @ ( gen ) amount!
;
: flange-time-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) flange-time!
;
: post-flange-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen flange-ok-cb
    gen label@ $" Move the slider to change the flange speed, amount, and time." _ help-cb
    gen flange-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" flange speed"  _ 0.0 gen flange-speed@ 100.0 gen flange-speed-cb   10 )
       #( $" flange amount" _ 0.0 gen amount@       100.0 gen flange-amount-cb  10 )
       #( $" flange time"   _ 0.0 gen flange-time@    1.0 gen flange-time-cb   100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-flange-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-flange% %alloc make-base-effects { gen }
  2.000 gen flange-speed!
  5.000 gen amount!
  0.001 gen flange-time!
  gen post-flange-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f %.2f)" '( gen label@ gen flange-speed@ gen amount@ gen flange-time@ )
  string-format change-label
;

\ === Randomize phase ===

: random-phase-cb ( scl -- prc; x self -- res )
  1 proc-create swap ,
 does> ( x self -- res )
  { x self }
  self @ ( scl ) random
;

hide
: rp-ok-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self --; x self -- res )
  { w c info self }
  self @ { gen }
  gen scaler@ random-phase-cb { prc }
  \ edit-list->function needs a usable proc-source-string
  prc $" %s random-phase-cb" gen scaler@ string-format proc-source-set!
  prc #f #f rotate-phase
;
: rp-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen scaler@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init ) gen scaler!
  gen sliders@ 0 array-ref '( FXmNvalue gen scaler@ 100.0 f* f>s ) FXtVaSetValues drop
;
: rp-scl-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) scaler!
;
: post-random-phase-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen rp-ok-cb
    gen label@ $" Move the slider to change the randomization amplitude scaler." _ help-cb
    gen rp-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( $" amplitude scaler" _ 0.0 gen scaler@ 100.0 gen rp-scl-cb 100 ) )
    add-sliders gen sliders!
  then
  gen dialog@ activate-dialog
;
set-current

: make-random-phase-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  3.14 gen scaler!
  gen post-random-phase-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen scaler@ ) string-format change-label
;

\ === Robotize ===

hide
effects-base%
  cell% field robotize-samp-rate
end-struct effects-robotize%

: samp-rate@ ( gen -- sr ) robotize-samp-rate @ ;
: samp-rate! ( sr gen -- ) robotize-samp-rate ! ;

: robotize-ok-cb ( gen -- prc; w c i self -- res )
  3 proc-create swap ,
 does> ( w c i self -- res )
  { w c info self }
  self @ { gen }
  gen samp-rate@ gen amplitude@ gen frequency@ \ beg dur follows
  gen target@ 'sound equal? if
    0  #f #f #f frames
  else
    gen target@ 'selection equal? if
      #f #f selection-position  #f #f selection-frames
    else
      plausible-mark-samples { pts }
      pts if
	pts car  pts cadr pts car -
      else
	'no-such-mark '( get-func-name pts ) fth-throw
      then
    then
  then #f #f effects-fp
;
: robotize-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen samp-rate@ , gen amplitude@ , gen frequency@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-sr )  gen samp-rate!
  self 2 cells + @ ( init-amp ) gen amplitude!
  self 3 cells + @ ( init-frq ) gen frequency!
  gen sliders@ 0 array-ref '( FXmNvalue gen samp-rate@ 100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen amplitude@ 100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 2 array-ref '( FXmNvalue gen frequency@ 100.0 f* f>s ) FXtVaSetValues drop
;
: robotize-sam-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) samp-rate!
;
: robotize-amp-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amplitude!
;
: robotize-frq-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) frequency!
;
: post-robotize-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen robotize-ok-cb
    gen label@
    $" Move the sliders to set the sample rate, oscillator amplitude, and oscillator frequency." _
    help-cb
    gen robotize-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" sample rate"          _ 0.0 gen samp-rate@  2.0 gen robotize-sam-cb 100 )
       #( $" oscillator amplitude" _ 0.0 gen amplitude@  1.0 gen robotize-amp-cb 100 )
       #( $" oscillator frequency" _ 0.0 gen frequency@ 60.0 gen robotize-frq-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-robotize-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-robotize% %alloc make-base-effects { gen }
  1.0  gen samp-rate!
  0.3  gen amplitude!
  20.0 gen frequency!
  gen post-robotize-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child )
  $" %s (%.2f %.2f %.2f)" '( gen label@ gen samp-rate@ gen amplitude@ gen frequency@ )
  string-format change-label
;

\ === Rubber sound ===

hide
effects-base%
  cell% field rubber-factor
end-struct effects-rubber%

: factor@ ( gen -- val ) rubber-factor @ ;
: factor! ( val gen -- ) rubber-factor ! ;

: rubber-ok-cb ( gen -- prc; w c i self -- res )
  3 proc-create swap ,
 does> ( w c i self -- res )
  { w c info self }
  self @ ( gen ) factor@ #f #f rubber-sound
;
: rubber-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen factor@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init ) gen factor!
  gen sliders@ 0 array-ref '( FXmNvalue gen factor@ 100.0 f* f>s ) FXtVaSetValues drop
;
: rubber-factor-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) factor!
;
: post-rubber-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen rubber-ok-cb
    gen label@
    $" Stretches or contracts the time of a sound.  \
Move the slider to change the stretch factor." _ help-cb
    gen rubber-reset-cb make-effect-dialog gen dialog!
    gen dialog@ #( #( $" stretch factor" _ 0.0 gen factor@ 5.0 gen rubber-factor-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-rubber-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-rubber% %alloc make-base-effects { gen }
  1.0 gen factor!
  gen post-rubber-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f)" '( gen label@ gen factor@ ) string-format change-label
;

\ === Wobble ===

hide
: wobble-ok-cb ( gen -- prc; w c i self -- res )
  3 proc-create swap ,
 does> ( w c i self -- res )
  { w c info self }
  self @ { gen }
  gen frequency@ gen amplitude@		\ beg dur follows
  gen target@ 'sound equal? if
    0  #f #f #f frames
  else
    gen target@ 'selection equal? if
      #f #f selection-position  #f #f selection-frames
    else
      plausible-mark-samples { pts }
      pts if
	pts car  pts cadr pts car -
      else
	'no-such-mark '( get-func-name pts ) fth-throw
      then
    then
  then #f #f effects-hello-dentist
;
: wobble-reset-cb ( gen -- prc; w c i self -- )
  { gen }
  3 proc-create gen , gen frequency@ , gen amplitude@ ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  self 1 cells + @ ( init-frq ) gen frequency!
  self 2 cells + @ ( init-amp ) gen amplitude!
  gen sliders@ 0 array-ref '( FXmNvalue gen frequency@ 100.0 f* f>s ) FXtVaSetValues drop
  gen sliders@ 1 array-ref '( FXmNvalue gen amplitude@ 100.0 f* f>s ) FXtVaSetValues drop
;
: wobble-frq-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) frequency!
;
: wobble-amp-cb ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  info Fvalue 100.0 f/ self @ ( gen ) amplitude!
;
: post-wobble-dialog ( gen -- prc; w c i self -- )
  3 proc-create swap ,
 does> ( w c i self -- )
  { w c info self }
  self @ { gen }
  gen dialog@ FWidget? unless
    gen label@
    gen wobble-ok-cb
    gen label@ $" Move the sliders to set the wobble frequency and amplitude." _ help-cb
    gen wobble-reset-cb make-effect-dialog gen dialog!
    gen dialog@
    #( #( $" wobble frequency" _ 0.0 gen frequency@ 100.0 gen wobble-frq-cb 100 )
       #( $" wobble amplitude" _ 0.0 gen amplitude@   1.0 gen wobble-amp-cb 100 ) )
    add-sliders gen sliders!
    gen sliders@ 0 array-ref FXtParent gen target-cb #f add-target
  then
  gen dialog@ activate-dialog
;
set-current

: make-wobble-dialog ( name -- prc1 prc2; child self -- prc; self -- )
  ( name ) effects-base% %alloc make-base-effects { gen }
  50.0 gen frequency!
  0.5  gen amplitude!
  gen post-wobble-dialog ( prc1 )
  1 proc-create gen ,   ( prc2 )
 does> ( child self -- prc; self -- )
  { child self }
  0 proc-create self @ ( gen ) , child ,
 does> ( self -- )
  { self }
  self @ { gen }
  self cell+ @ ( child ) $" %s (%.2f %.2f)" '( gen label@ gen frequency@ gen amplitude@ )
  string-format change-label
;
previous

$" Effects" _ value effects-menu-label

: make-effects-menu ( -- widget )
  effects-menu-label make-main-menu { main }
  '( FXmNbackground basic-color ) { args }
  $" Amplitude Effects"        	  _ main args make-menu { menu }
  menu $" Gain"                	  _ make-gain-dialog         menu-entry
  menu $" Normalize"           	  _ make-normalize-dialog    menu-entry
  menu $" Gate"                	  _ make-gate-dialog         menu-entry
  $" Delay Effects"            	  _ main args make-menu to   menu
  menu $" Echo"                	  _ make-echo-dialog         menu-entry
  menu $" Filtered echo"       	  _ make-flecho-dialog       menu-entry
  menu $" Modulated echo"      	  _ make-zecho-dialog        menu-entry
  $" Filter Effects"           	  _ main args make-menu to   menu
  menu $" Band-pass filter"    	  _ make-band-pass-dialog    menu-entry
  menu $" Band-reject filter"  	  _ make-notch-dialog        menu-entry
  menu $" High-pass filter"    	  _ make-high-pass-dialog    menu-entry
  menu $" Low-pass filter"     	  _ make-low-pass-dialog     menu-entry
  menu $" Comb filter"         	  _ make-comb-dialog         menu-entry
  menu $" Comb chord filter"   	  _ make-comb-chord-dialog   menu-entry
  menu $" Moog filter"         	  _ make-moog-dialog         menu-entry
  $" Frequency Effects"        	  _ main args make-menu to   menu
  menu $" Adaptive saturation" 	  _ make-adsat-dialog        menu-entry
  menu $" Sample rate conversion" _ make-src-dialog          menu-entry
  menu $" Time/pitch scaling"     _ make-expsrc-dialog       menu-entry
  menu $" Src-Timevar"            _ make-src-timevar-dialog  menu-entry
  $" Modulation Effects"       	  _ main args make-menu to   menu
  menu $" Amplitude modulation"	  _ make-am-effect-dialog    menu-entry
  menu $" Ring modulation"	  _ make-rm-effect-dialog    menu-entry
  $" Reverbs"       	          _ main args make-menu to   menu
  menu $" McNabb reverb"	  _ make-reverb-dialog       menu-entry
  menu $" Chowning reverb"	  _ make-jc-reverb-dialog    menu-entry
  menu $" Convolution"	          _ make-convolve-dialog     menu-entry
  $" Various"       	          _ main args make-menu to   menu
  menu $" Place sound"	          _ make-place-sound-dialog  menu-entry
  menu $" Add silence"	          _ make-silence-dialog      menu-entry
  menu $" Contrast enhancement"   _ make-contrast-dialog     menu-entry
  menu $" Cross synthesis"        _ make-cross-synth-dialog  menu-entry
  menu $" Flange"                 _ make-flange-dialog       menu-entry
  menu $" Randomize phase"        _ make-random-phase-dialog menu-entry
  menu $" Robotize"               _ make-robotize-dialog     menu-entry
  menu $" Rubber sound"           _ make-rubber-dialog       menu-entry
  menu $" Wobble"                 _ make-wobble-dialog       menu-entry
  main
;

hide
: find-click <{ loc :optional snd #f chn #f -- pos|#f }>
  loc snd chn 1 #f make-sample-reader { rd }
  0.0 0.0 0.0 { samp0 samp1 samp2 }
  10 0.0 make-vct { samps }
  #f 					\ flag
  snd chn #f frames loc ?do
    c-g? ?leave
    samp1 to samp0
    samp2 to samp1
    rd next-sample to samp2
    samps samp0 cycle-set!
    samps vct-peak 0.1 fmax { local-max }
    samp0 samp1 f- fabs local-max f>
    samp1 samp2 f- fabs local-max f> &&
    samp0 samp2 f- fabs local-max f2/ f< && if drop ( flag ) i leave then
  loop
;
: remove-click <{ loc :optional snd #f chn #f -- }>
  loc snd chn find-click { click }
  click c-g? not && if
    click 2 - 4 snd chn smooth-sound drop
    click 2 + snd chn recurse
  then
;
: effects-remove-dc-cb ( -- prc; inval self -- res )
  1 proc-create 0.0 ( lastx ) , 0.0 ( lasty ) ,
 does> ( inval self -- res )
  { inval self }
  self @ { lastx }
  self cell+ @ { lasty }
  0.999 lasty f* lastx f- inval f+ self cell+ ! ( lasty )
  inval self ! ( lastx )
  self cell+ @ ( lasty )
;
: effects-compand-cb ( tbl -- prc; inval self -- res )
  1 proc-create swap ,
 does> ( inval self -- res )
  { inval self }
  self @ { tbl }
  tbl inval 8.0 f* 8.0 f+ tbl length array-interp
;
set-current
: effects-remove-clicks <{ :optional snd #f chn #f -- res }>
  0 -rot remove-click #f ;
: effects-remove-dc <{ :optional snd #f chn #f -- res }>
  effects-remove-dc-cb 0 #f snd chn #f get-func-name map-channel
;
: effects-compand <{ :optional snd #f chn #f -- res }>
  vct( -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250
  0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000 ) { tbl }
  tbl effects-compand-cb 0 #f snd chn #f get-func-name map-channel
;
previous

effects-menu-label main-widget-exists? [unless] make-effects-menu drop [then]

#f effects-noop  add-to-effects-menu	\ separator
$" Octave-down"   _ lambda: <{ -- }> 2 #f #f down-oct ;            add-to-effects-menu
$" Remove clicks" _ lambda: <{ -- }> #f #f effects-remove-clicks ; add-to-effects-menu
$" Remove DC"     _ lambda: <{ -- }> #f #f effects-remove-dc ;     add-to-effects-menu
$" Spiker"        _ lambda: <{ -- }> #f #f spike ;                 add-to-effects-menu
$" Compand"       _ lambda: <{ -- }> #f #f effects-compand ;       add-to-effects-menu
$" Invert"        _ lambda: <{ -- }> -1 #f #f scale-by ;           add-to-effects-menu
$" Reverse"       _ lambda: <{ -- }> #f #f #f reverse-sound ;      add-to-effects-menu
$" Null phase"    _ lambda: <{ -- }> #f #f zero-phase ;            add-to-effects-menu

\ effects.fs ends here
