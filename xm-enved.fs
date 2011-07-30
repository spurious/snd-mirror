\ -*- snd-forth -*-
\ xm-enved.fs -- xm-enved.scm -> xm-enved.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Oct 21 18:22:57 CEST 2005
\ Changed: Sat Jul 30 18:31:47 CEST 2011

\ Commentary:
\
\ Requires --with-motif|gtk and module libxm.so|libxg.so or --with-static-xm|xg!
\
\ Tested with Snd 12.x
\             Fth 1.2.x
\             Motif 2.3.0 X11R6
\             Gtk+ 3.0.11, Glib 2.28.8, Pango 1.28.4, Cairo 1.10.2
\
\ This is an example of an object type written in Forth.
\
\ XENVED
\  xe-inspect 	  	( obj -- str )
\  xe->string 	  	( obj -- str )
\  xe-dump 	  	( obj -- str )
\  xe->array            ( obj -- ary )
\  xe-ref               ( obj index -- point )
\  xe-set!              ( obj index point -- )
\  xe-equal?  	  	( obj1 obj2 -- f )
\  xe-length            ( obj -- len )
\  xe-free    	  	( obj -- )
\
\ xenved?     	  	( obj -- f )
\ make-xenved 	  	( name parent :key envelope axis-bounds args -- xenved )
\ run-before-enved-hook ( obj point reason -- f )
\ xe-index           	( obj x -- index|-1 )
\ xe-insert!         	( obj index point -- )
\ xe-delete!         	( obj index -- )
\ xe-envelope 	  	( obj -- lst )
\ set-xe-envelope 	( obj lst -- )
\ xe-open               ( obj -- )
\ xe-close              ( obj -- )
\
\ xenved-test           ( name -- xe )

'snd-nogui provided? [if] skip-file [then]

require enved
require snd-xm

\ === XENVED OBJECT TYPE ===

5 $" ( gen pos x y reason -- f )  \
Will be called before changing a breakpoint in GEN's envelope.  \
This hook runs the global ENVED-HOOK at first, \
subsequent procedures can directly manipulate GEN's envelope \
or the returned array of the preceding hook procedure.\n\
This instance hook is like the global ENVED-HOOK; \
POS is ENVELOPE's x-position, X and Y are the new points, \
and REASON is one of the Snd constants ENVED-ADD-POINT, ENVED-DELETE-POINT, ENVED-MOVE-POINT.  \
If one of the hook procedures in the hook array returns #f, xenved changes the breakpoint, \
otherwise the last hook procedure is responsible for manipulating GEN's envelope itself."
create-hook before-enved-hook

: axis-bounds? ( obj -- f ) array-length 4 = ;

hide
enved%
  cell% field xe-enved
  cell% field xe-name
  cell% field xe-parent
  cell% field xe-args
  cell% field xe-drawer
  cell% field xe-gcs
  cell% field xe-bx0
  cell% field xe-bx1
  cell% field xe-by0
  cell% field xe-by1
  cell% field xe-px0
  cell% field xe-px1
  cell% field xe-py0
  cell% field xe-py1
  cell% field xe-mouse-up		\ float
  cell% field xe-mouse-down		\ float
  cell% field xe-mouse-pos
  cell% field xe-mouse-new
  cell% field xe-click-time
  cell% field xe-dragging
end-struct xenved%

: xe-enved@      ( obj -- env ) instance-gen-ref xe-enved @ ;
: xe-envelope@   ( obj -- val ) xe-enved@ envelope@ ;
: xe-envelope!   ( val obj -- ) xe-enved@ envelope! ;
: xe-name@       ( obj -- val ) instance-gen-ref xe-name @ ;
: xe-parent@     ( obj -- val ) instance-gen-ref xe-parent @ ;
: xe-args@       ( obj -- val ) instance-gen-ref xe-args @ ;
: xe-gcs@        ( obj -- val ) instance-gen-ref xe-gcs @ ;
: xe-drawer@     ( obj -- val ) instance-gen-ref xe-drawer @ ;
: xe-bx0@        ( obj -- val ) instance-gen-ref xe-bx0 @ ;
: xe-bx1@        ( obj -- val ) instance-gen-ref xe-bx1 @ ;
: xe-by0@        ( obj -- val ) instance-gen-ref xe-by0 @ ;
: xe-by1@        ( obj -- val ) instance-gen-ref xe-by1 @ ;
: xe-px0@        ( obj -- val ) instance-gen-ref xe-px0 @ ;
: xe-px0!        ( val obj -- ) instance-gen-ref xe-px0 ! ;
: xe-px1@        ( obj -- val ) instance-gen-ref xe-px1 @ ;
: xe-px1!        ( val obj -- ) instance-gen-ref xe-px1 ! ;
: xe-py0@        ( obj -- val ) instance-gen-ref xe-py0 @ ;
: xe-py0!        ( val obj -- ) instance-gen-ref xe-py0 ! ;
: xe-py1@        ( obj -- val ) instance-gen-ref xe-py1 @ ;
: xe-py1!        ( val obj -- ) instance-gen-ref xe-py1 ! ;
: xe-mouse-up@   ( obj -- val ) instance-gen-ref xe-mouse-up @ ;
: xe-mouse-up!   ( val obj -- ) instance-gen-ref xe-mouse-up ! ;
: xe-mouse-down@ ( obj -- val ) instance-gen-ref xe-mouse-down @ ;
: xe-mouse-down! ( val obj -- ) instance-gen-ref xe-mouse-down ! ;
: xe-mouse-pos@  ( obj -- val ) instance-gen-ref xe-mouse-pos @ ;
: xe-mouse-pos!  ( val obj -- ) instance-gen-ref xe-mouse-pos ! ;
: xe-mouse-new@  ( obj -- val ) instance-gen-ref xe-mouse-new @ ;
: xe-mouse-new!  ( val obj -- ) instance-gen-ref xe-mouse-new ! ;
: xe-click-time@ ( obj -- val ) instance-gen-ref xe-click-time @ ;
: xe-dragging@   ( obj -- val ) instance-gen-ref xe-dragging @ ;
: xe-dragging!   ( val obj -- ) instance-gen-ref xe-dragging ! ;
set-current

"xenved" make-object-type constant fth-xenved
fth-xenved make-?obj xenved?

\ before-enved-hook lambda: <{ gen pos x y reason -- f }>
\   enved-hook hook-empty? if
\     #f
\   else
\     gen xe-envelope@ { res }
\     enved-hook hook->array each { prc }
\       prc #( res pos x y reason ) run-proc to res
\       res false? ?leave
\     end-each
\     res array? if
\       res gen xe-envelope!
\     else
\       res enved? if
\         res envelope@ gen xe-envelope!
\       else
\         res xenved? if
\           res xe-envelope@ gen xe-envelope!
\         then
\       then
\     then
\     res
\   then
\ ; add-hook!

: run-before-enved-hook { gen point reason -- f }
  before-enved-hook hook-empty? if
    #t
  else
    #f					\ flag
    before-enved-hook hook->array each { prc }
      prc #( gen gen xe-mouse-pos@ point 0 array-ref point 1 array-ref reason ) run-proc false? if
	not				\ toggle flag
	leave
      then
    end-each
  then
;

: xe-length ( obj -- len ) xe-enved@ enved-length ;

: xe-inspect { obj -- str }
  $" #<%s[%d]: axis-bounds:  #( %s %s %s %s ), envelope: %s>"
  #( obj object-name
     obj xe-length
     obj xe-bx0@
     obj xe-bx1@
     obj xe-by0@
     obj xe-by1@
     obj xe-enved@ ) string-format
;

: xe->string ( obj -- str ) xe-enved@ enved->string ;  

: xe-dump { obj -- str }
  $" %S %S :envelope %S :axis-bounds #( %s %s %s %s ) :args %S make-xenved"
  #( obj xe-name@
     obj xe-parent@
     obj xe-envelope@
     obj xe-bx0@
     obj xe-bx1@
     obj xe-by0@
     obj xe-by1@
     obj xe-args@ ) string-format
;

: xe->array ( obj -- ary )         xe-enved@ enved->array ;  
: xe-ref    ( obj index -- point ) swap xe-enved@ swap enved-ref ;
: xe-set!   ( obj index point -- ) rot xe-enved@ -rot enved-set! ;

: xe-equal? { obj1 obj2 -- f }
  obj1 xenved? obj2 xenved? && if
    obj1 xe-enved@ obj2 xe-enved@ enved-equal?
  else
    #f
  then
;

: xe-free ( obj -- ) instance-gen-ref free throw ;

\ Init xenved
<'> xe-inspect  fth-xenved set-object-inspect   \ xe .inspect
<'> xe->string  fth-xenved set-object->string   \ xe object->string
<'> xe-dump     fth-xenved set-object-dump      \ xe object-dump
<'> xe->array   fth-xenved set-object->array    \ xe object->array
<'> xe-ref      fth-xenved set-object-value-ref \ xe index        object-ref => #( x y )
<'> xe-set!     fth-xenved set-object-value-set \ xe index #( x y ) object-set!
<'> xe-equal?   fth-xenved set-object-equal-p   \ obj1 obj2 equal?
<'> xe-length   fth-xenved set-object-length    \ xe object-length => number of points (lstlen/2)
<'> xe-free     fth-xenved set-object-free      \ for gc
<'> xe-ref      fth-xenved 1 set-object-apply   \ xe index apply => #( x y )

: xe-index   ( obj x -- index|-1 )  swap xe-enved@ swap enved-index ;
: xe-insert! ( obj index point -- ) rot  xe-enved@ -rot enved-insert! ;
: xe-delete! ( obj index -- )       swap xe-enved@ swap enved-delete! ;

0.03 constant mouse-radius

: grfx { x gen -- val }
  gen xe-px0@ gen xe-px1@ = if
    gen xe-px0@
  else
    gen xe-px0@ { px0 }
    gen xe-px1@ { px1 }
    gen xe-bx0@ { bx0 }
    gen xe-bx1@ { bx1 }
    x bx0 f-  bx1 bx0 f-  f/  px1 px0 f-  f*  px0  f+  floor f>s  px0  max  px1  min
  then
;

: grfy { y gen -- val }
  gen xe-py0@ gen xe-py1@ = if
    gen xe-py0@
  else
    gen xe-py0@ { py0 }
    gen xe-py1@ { py1 }
    gen xe-by0@ { by0 }
    gen xe-by1@ { by1 }
    y by1 f-  by0 by1 f-  f/  py0 py1 f-  f*  py1 f+  floor f>s  py1  max  py0  min
  then
;

: ungrfx { x gen -- val }
  gen xe-px0@ gen xe-px1@ = if
    gen xe-bx0@ s>f
  else
    gen xe-px0@ { px0 }
    gen xe-px1@ { px1 }
    gen xe-bx0@ { bx0 }
    gen xe-bx1@ { bx1 }
    x px0 f-  px1 px0 f-  f/  bx1 bx0 f-  f*  bx0  f+  bx0  fmax  bx1  fmin
  then
;

: ungrfy { y gen -- val }
  gen xe-py0@ gen xe-py1@ = if
    gen xe-by1@ s>f
  else
    gen xe-py0@ { py0 }
    gen xe-py1@ { py1 }
    gen xe-by0@ { by0 }
    gen xe-by1@ { by1 }
    py0 y f-  py0 py1 f-  f/  by1 by0 f-  f*  by0  f+  by0  fmax  by1  fmin
  then
;

'snd-motif provided? [if]
  360 64 * constant 360*64

  : xe-redraw { gen -- }
    gen xe-drawer@ { drawer }
    drawer is-managed?
    gen xe-py0@ gen xe-py1@ > && if
      gen xe-gcs@ { gc }
      drawer FXtDisplay { dpy }
      drawer FXtWindow { win }
      dpy win FXClearWindow drop
      \ Motif's DRAW-AXES takes 6 optional arguments.
      \ '( x0 y0 x1 y1 ) = draw-axes(wid gc label
      \                              x0=0.0 x1=1.0 y0=-1.0 y1=1.0
      \                              style=x-axis-in-seconds
      \                              axes=show-all-axes)
      \ arity #( 3 6 #f )
      drawer
      gc
      gen xe-name@
      gen xe-bx0@
      gen xe-bx1@
      gen xe-by0@
      gen xe-by1@
      x-axis-in-seconds
      show-all-axes draw-axes drop
      #f #f { lx ly }
      10 { mouse-d }
      5  { mouse-r }
      gen each { point }
	point 0 array-ref gen grfx { cx }
	point 1 array-ref gen grfy { cy }
	dpy win gc  cx mouse-r -  cy mouse-r -  mouse-d mouse-d 0 360*64 FXFillArc drop
	lx if dpy win gc lx ly cx cy FXDrawLine drop then
	cx to lx
	cy to ly
      end-each
    then
  ;
[else]
  : xe-redraw { gen -- }
    gen xe-drawer@ { drawer }
    drawer is-managed?
    gen xe-py0@ gen xe-py1@ > && if
      gen xe-gcs@ { gc }
      drawer FGTK_WIDGET widget-size { size }
      drawer make-cairo { cairo }
      cairo Fcairo_push_group drop
      cairo 1.0 1.0 1.0 Fcairo_set_source_rgb drop
      cairo 0 0 size 0 array-ref size 1 array-ref Fcairo_rectangle drop
      cairo Fcairo_fill drop
      \ Gtk's DRAW-AXES takes one more optional argument, a cairo object.
      \ '( x0 y0 x1 y1 ) = draw-axes(wid gc label
      \                              x0=0.0 x1=1.0 y0=-1.0 y1=1.0
      \                              style=x-axis-in-seconds
      \                              axes=show-all-axes
      \                              cairo)
      \ arity #( 3 7 #f )
      drawer
      gc
      gen xe-name@
      gen xe-bx0@
      gen xe-bx1@
      gen xe-by0@
      gen xe-by1@
      x-axis-in-seconds
      show-all-axes
      cairo draw-axes drop
      cairo 1.0 Fcairo_set_line_width drop
      cairo 0.0 0.0 0.0 Fcairo_set_source_rgb drop
      #f #f { lx ly }
      5 { mouse-r }
      gen each { point }
	point 0 array-ref gen grfx { cx }
	point 1 array-ref gen grfy { cy }
	cairo cx cy mouse-r 0.0 two-pi Fcairo_arc drop
	cairo Fcairo_fill drop
	lx if
	  cairo lx ly Fcairo_move_to drop
	  cairo cx cy Fcairo_line_to drop
	  cairo Fcairo_stroke drop
	then
	cx to lx
	cy to ly
      end-each
      cairo Fcairo_pop_group_to_source drop
      cairo Fcairo_paint drop
      cairo free-cairo drop
    then
  ;
[then]

: add-envelope-point { x y gen -- }
  gen xe-mouse-pos@ { mpos }
  gen x xe-index dup 0>= if
    to mpos
  else
    drop
    gen each 0 array-ref x f> if i to mpos leave then end-each
  then
  gen mpos #( x y ) xe-insert!
  mpos gen xe-mouse-pos!
;

: draw-axes-set-points { lst gen -- }
  lst 0 array-ref gen xe-px0!
  lst 1 array-ref gen xe-py0!
  lst 2 array-ref gen xe-px1!
  lst 3 array-ref gen xe-py1!
  gen xe-redraw
;

: mouse-press { gen xx yy -- }
  xx gen ungrfx { x }
  yy gen ungrfy { y }
  #f					\ flag
  gen each { point }
    point 0 array-ref x f- fabs mouse-radius f<
    point 1 array-ref y f- fabs mouse-radius f< && if drop ( flag ) i leave then
  end-each { pos }
  pos not gen xe-mouse-new!
  time gen xe-mouse-down!
  pos number? if
    pos gen xe-mouse-pos!
  else
    gen #( x y ) enved-add-point run-before-enved-hook if x y gen add-envelope-point then
    gen xe-redraw
  then
;

: mouse-release { gen -- }
  gen xe-mouse-pos@ { mpos }
  time gen xe-mouse-up!
  gen xe-mouse-new@ unless
    gen xe-mouse-up@ gen xe-mouse-down@ f- gen xe-click-time@ f<= if
      mpos 0<> if
	mpos gen xe-length 1- < if
	  gen mpos xe-ref { point }
	  gen point enved-delete-point run-before-enved-hook if gen mpos xe-delete! then
	  gen xe-redraw
	then
      then
    then
  then
  #f gen xe-mouse-new!
;

: mouse-drag { gen xx yy -- }
  xx gen ungrfx { x }
  yy gen ungrfy { y }
  gen xe-mouse-pos@ { mpos }
  mpos 0= if
    gen 0 xe-ref 0 array-ref
  else
    mpos gen xe-length 1- >= if
      gen -1 xe-ref 0 array-ref
    else
      gen mpos 1- xe-ref 0 array-ref  gen mpos 1+ xe-ref 0 array-ref  x fmin fmax
    then
  then to x
  gen #( x y ) enved-move-point run-before-enved-hook if gen mpos #( x y ) xe-set! then
  gen xe-redraw
;

'snd-motif provided? [if]
  : draw-axes-cb <{ w gen info -- }>
    gen xe-drawer@
    gen xe-gcs@
    gen xe-name@
    gen xe-bx0@
    gen xe-bx1@
    gen xe-by0@
    x-axis-in-seconds
    show-all-axes draw-axes gen draw-axes-set-points
  ;
  
  : mouse-press-cb     <{ w gen ev f -- }>   gen ev Fx ev Fy mouse-press ;
  : mouse-release-cb   <{ w gen ev f -- }>   gen mouse-release ;
  : mouse-drag-cb      <{ w gen ev f -- }>   gen ev Fx ev Fy mouse-drag ;
  : define-cursor-cb   <{ w gen ev f -- x }> w FXtDisplay w FXtWindow gen FXDefineCursor ;
  : undefine-cursor-cb <{ w gen ev f -- x }> w FXtDisplay w FXtWindow FXUndefineCursor ;

  : make-drawer { name parent args -- drawer }
    args FXmNbackground array-member? unless
      args FXmNbackground array-push graph-color array-push to args
    then
    args FXmNforeground array-member? unless
      args FXmNforeground array-push data-color array-push to args
    then
    name FxmDrawingAreaWidgetClass parent args undef FXtCreateManagedWidget ( drawer )
  ;
  
  : init-xenved { gen -- gen }
    gen xe-drawer@ { drawer }
    drawer FXtDisplay FXC_crosshair FXCreateFontCursor { arrow-cursor }
    drawer FXmNresizeCallback    <'> draw-axes-cb     gen          FXtAddCallback drop
    drawer FXmNexposeCallback    <'> draw-axes-cb     gen          FXtAddCallback drop
    drawer FButtonPressMask   #f <'> mouse-press-cb   gen          FXtAddEventHandler drop
    drawer FButtonReleaseMask #f <'> mouse-release-cb gen          FXtAddEventHandler drop
    drawer FButtonMotionMask  #f <'> mouse-drag-cb    gen          FXtAddEventHandler drop
    drawer FEnterWindowMask   #f <'> define-cursor-cb arrow-cursor FXtAddEventHandler drop
    drawer FLeaveWindowMask   #f <'> undefine-cursor-cb        #f  FXtAddEventHandler drop
    gen
  ;
[else]
  : draw-axes-cb <{ w ev gen -- f }>
    gen xe-drawer@ { win }
    win make-cairo { cairo }
    win
    gen xe-gcs@
    gen xe-name@
    gen xe-bx0@
    gen xe-bx1@
    gen xe-by0@
    gen xe-by1@
    x-axis-in-seconds
    show-all-axes
    cairo draw-axes gen draw-axes-set-points
    cairo free-cairo drop
    #f
  ;

  \ '( bool x y ) = gdk_event_get_coords(event x-win-return=undef y-win-return=undef)
  \ arity #( 1 2 #f )
  : get-coords ( ev -- x y ) undef undef Fgdk_event_get_coords dup 1 array-ref swap 2 array-ref ;

  : mouse-press-cb <{ w ev gen -- f }>
    #t gen xe-dragging!
    gen ev FGDK_EVENT get-coords mouse-press
    #f
  ;

  : mouse-release-cb <{ w ev gen -- f }>
    #f gen xe-dragging!
    gen mouse-release
    #f
  ;

  : mouse-drag-cb <{ w ev gen -- f }>
    gen xe-dragging@ if gen ev FGDK_EVENT get-coords mouse-drag then
    #f
  ;

  : define-cursor-cb <{ w ev cursor -- f }>
    w Fgtk_widget_get_window cursor Fgdk_window_set_cursor drop
    #f
  ;

  : make-drawer { name parent args -- drawer }
    Fgtk_drawing_area_new { drawer }
    drawer FGDK_ALL_EVENTS_MASK Fgtk_widget_set_events drop
    parent FGTK_BOX drawer #t #t 10 Fgtk_box_pack_start drop
    drawer Fgtk_widget_show drop
    drawer name Fgtk_widget_set_name drop
    drawer -1 200 Fgtk_widget_set_size_request drop
    drawer
  ;

  : init-xenved { gen -- gen }
    gen xe-drawer@ { drawer }
    drawer FGPOINTER
    'gtk3 provided? if
      "draw"
    else
      "expose_event"
    then drawer FG_OBJECT FG_OBJECT_TYPE Fg_signal_lookup
    0
    <'> draw-axes-cb gen #f Fg_cclosure_new
    #f
    Fg_signal_connect_closure_by_id drop
    drawer FGPOINTER
    "configure_event" drawer FG_OBJECT FG_OBJECT_TYPE Fg_signal_lookup
    0
    <'> draw-axes-cb gen #f Fg_cclosure_new
    #f
    Fg_signal_connect_closure_by_id drop
    drawer FGPOINTER
    "button_press_event" drawer FG_OBJECT FG_OBJECT_TYPE Fg_signal_lookup
    0
    <'> mouse-press-cb gen #f Fg_cclosure_new
    #f
    Fg_signal_connect_closure_by_id drop
    drawer FGPOINTER
    "button_release_event" drawer FG_OBJECT FG_OBJECT_TYPE Fg_signal_lookup
    0
    <'> mouse-release-cb gen #f Fg_cclosure_new
    #f
    Fg_signal_connect_closure_by_id drop
    drawer FGPOINTER
    "motion_notify_event" drawer FG_OBJECT FG_OBJECT_TYPE Fg_signal_lookup
    0
    <'> mouse-drag-cb gen #f Fg_cclosure_new
    #f
    Fg_signal_connect_closure_by_id drop
    drawer FGPOINTER
    "enter_notify_event" drawer FG_OBJECT FG_OBJECT_TYPE Fg_signal_lookup
    0
    <'> define-cursor-cb FGDK_CROSSHAIR Fgdk_cursor_new ( arrow-cursor ) #f Fg_cclosure_new
    #f
    Fg_signal_connect_closure_by_id drop
    drawer FGPOINTER
    "leave_notify_event" drawer FG_OBJECT FG_OBJECT_TYPE Fg_signal_lookup
    0
    <'> define-cursor-cb FGDK_LEFT_PTR Fgdk_cursor_new ( old-cursor ) #f Fg_cclosure_new
    #f
    Fg_signal_connect_closure_by_id drop
    gen
  ;
[then]

\ Arrays or lists as default values for <{ ... }> doesn't work very well.
\
\ make-xenved <{ name parent
\               :key
\               envelope    #( 0.0 0.0 1.0 1.0 )
\               axis-bounds #( 0.0 1.0 0.0 1.0 )
\               args        #() -- gen }>

: make-xenved ( name parent keyword-args -- gen )
  :envelope    #( 0.0 0.0 1.0 1.0 ) get-optkey { envelope }
  :axis-bounds #( 0.0 1.0 0.0 1.0 ) get-optkey { axis-bounds }
  :args        #()                  get-optkey { args }
  { name parent }
  parent      widget?      parent      2 $" a widget"                assert-type
  axis-bounds axis-bounds? axis-bounds 4 $" an array of axis bounds" assert-type
  xenved% %alloc { xe }
  xe unless 'system-error #( get-func-name $" cannot create xenved" ) fth-throw then
  envelope make-enved xe xe-enved !
  name string? unless "xe-test" to name then
  name parent args make-drawer xe xe-drawer !
  name   xe xe-name !
  parent xe xe-parent !
  args   xe xe-args !
  snd-gcs 0 array-ref xe xe-gcs !
  axis-bounds 0 array-ref xe xe-bx0 !
  axis-bounds 2 array-ref xe xe-by0 !
  axis-bounds 1 array-ref xe xe-bx1 !
  axis-bounds 3 array-ref xe xe-by1 !
  0 xe xe-px0 !				\ points == ints
  0 xe xe-py0 !
  0 xe xe-px1 !
  0 xe xe-py1 !
  0.0 xe xe-mouse-up !
  0.0 xe xe-mouse-down !
  0.5 xe xe-click-time !
  0   xe xe-mouse-pos !
  #f  xe xe-mouse-new !
  #f  xe xe-dragging !
  xe fth-xenved make-instance init-xenved ( gen )
;

: xe-envelope { gen -- lst }
  gen xenved? gen 1 $" an xenved object" assert-type
  gen xe-envelope@
;

: set-xe-envelope { gen lst -- }
  gen xenved? gen 1 $" an xenved object" assert-type
  lst array?  lst 2 $" an array"         assert-type
  lst gen xe-envelope!
  gen xe-redraw
;

: xe-open { gen -- }
  gen xenved? gen 1 $" an xenved object" assert-type
  gen xe-drawer@ widget? if gen xe-drawer@ show-widget drop then
;

: xe-close { gen -- }
  gen xenved? gen 1 $" an xenved object" assert-type
  gen xe-drawer@ widget? if gen xe-drawer@ hide-widget drop then
;
previous

#f value test-widget-type
#f value test-widget-args
#f value test-xenved-args

'snd-motif provided? [if]
  FxmFormWidgetClass  to test-widget-type
  #( FXmNheight 200 ) to test-widget-args
  #( FXmNleftAttachment   FXmATTACH_WIDGET
     FXmNtopAttachment    FXmATTACH_WIDGET
     FXmNbottomAttachment FXmATTACH_WIDGET
     FXmNrightAttachment  FXmATTACH_WIDGET ) to test-xenved-args
[then]

: xenved-test <{ :optional name "xenved" -- xe }>
  doc" create a drawing test widget\n\
xenved-test value xe\n\
xe             => #( 0.0 0.0 1.0 1.0 )\n\
xe xe-envelope => #( 0.0 0.0 1.0 1.0 )\n\
\\ some clicks later
xe xe-envelope => #( 0.0 0.0\n\
                     0.190736 0.562264\n\
                     0.632152 0.932075\n\
                     0.848774 0.316981\n\
                     1.0 1.0 )\n\
xe #( 0 1 1 1 ) set-xe-envelope
xe xe-envelope => #( 0 1 1 1 )\n\
xe xe-close"
  name
  name test-widget-type test-widget-args add-main-pane
  :envelope    #( 0.0 0.0 1.0 1.0 )
  :axis-bounds #( 0.0 1.0 0.0 1.0 )
  :args test-xenved-args make-xenved ( xe )
;

\ xm-enved.fs ends here
