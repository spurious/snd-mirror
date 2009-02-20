\ -*- snd-forth -*-
\ xm-enved.fs -- xm-enved.scm -> xm-enved.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Oct 21 18:22:57 CEST 2005
\ Changed: Sat Dec 20 02:32:30 CET 2008

\ Commentary:

\ This is an example of an object type written in Forth.
\
\ XENVED
\  xe-inspect 	  	( obj -- str )
\  xe->string 	  	( obj -- str )
\  xe-dump 	  	( obj -- str )
\  xe->array            ( obj -- ary )
\  xe-copy              ( obj1 -- obj2 )
\  xe-ref               ( obj index -- point )
\  xe-set!              ( obj index point -- )
\  xe-equal?  	  	( obj1 obj2 -- f )
\  xe-length            ( obj -- len )
\  xe-mark    	  	( obj -- )
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
\ xenved-test           ( :optional name -- )

'snd-motif provided? [unless] skip-file [then]

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
otherwise the last hook procedure is responsible for manipulating GEN's envelope itself." _
create-hook before-enved-hook

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
  cell% field xe-mouse-up
  cell% field xe-mouse-down
  cell% field xe-mouse-pos
  cell% field xe-mouse-new
  cell% field xe-click-time
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
\ 	res envelope@ gen xe-envelope!
\       else
\ 	res xenved? if
\ 	  res xe-envelope@ gen xe-envelope!
\ 	then
\       then
\     then
\     res #f <> if #t else #f then
\   then
\ ; add-hook!
: run-before-enved-hook ( gen point reason -- f )
  { gen point reason }
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

: xe-length   ( obj -- len ) xe-enved@ enved-length ;
: xe-inspect  ( obj -- str )
  { obj }
  $" #<%s[%d]: axis-bounds:  #( %s %s %s %s ), envelope: %s>"
  #( obj object-name
     obj xe-length
     obj xe-bx0@
     obj xe-bx1@
     obj xe-by0@
     obj xe-by1@
     obj xe-enved@ ) string-format
;
: xe->string  ( obj -- str ) xe-enved@ enved->string ;  
: xe-dump     ( obj -- str )
  { obj }
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
: xe->array   ( obj -- ary )         xe-enved@ enved->array ;  
defer xe-copy ( obj1 -- obj2 )
: xe-ref      ( obj index -- point ) swap xe-enved@ swap enved-ref ;
: xe-set!     ( obj index point -- ) rot xe-enved@ -rot enved-set! ;
: xe-equal?   ( obj1 obj2 -- f )
  { obj1 obj2 }
  obj1 xenved? obj2 xenved? && if
    obj1 xe-enved@ obj2 xe-enved@ enved-equal?
  else
    #f
  then
;
: xe-mark ( obj -- )
  { obj }
  obj xe-envelope@   object-mark
  obj xe-name@       object-mark
  obj xe-parent@     object-mark
  obj xe-args@       object-mark
  obj xe-drawer@     object-mark
  obj xe-gcs@        object-mark
  obj xe-bx0@ 	     object-mark
  obj xe-bx1@ 	     object-mark
  obj xe-by0@ 	     object-mark
  obj xe-by1@ 	     object-mark
  obj xe-mouse-up@   object-mark
  obj xe-mouse-down@ object-mark
  obj xe-click-time@ object-mark
;
: xe-free ( obj -- ) instance-gen-ref free throw ;

\ Init xenved
<'> xe-inspect  fth-xenved set-object-inspect   \ xe .inspect
<'> xe->string  fth-xenved set-object->string   \ xe object->string
<'> xe-dump     fth-xenved set-object-dump      \ xe object-dump
<'> xe->array   fth-xenved set-object->array    \ xe object->array
<'> xe-copy     fth-xenved set-object-copy      \ xe object-copy
<'> xe-ref      fth-xenved set-object-value-ref \ xe index        object-ref => #( x y )
<'> xe-set!     fth-xenved set-object-value-set \ xe index #( x y ) object-set!
<'> xe-equal?   fth-xenved set-object-equal-p   \ obj1 obj2 equal?
<'> xe-length   fth-xenved set-object-length    \ xe object-length => number of points (lstlen/2)
<'> xe-mark     fth-xenved set-object-mark      \ xe object-mark and for gc
<'> xe-free     fth-xenved set-object-free      \ for gc
<'> xe-ref      fth-xenved 1 set-object-apply   \ xe index apply => #( x y )

: xe-index   ( obj x -- index|-1 )  swap xe-enved@ swap enved-index ;
: xe-insert! ( obj index point -- ) rot  xe-enved@ -rot enved-insert! ;
: xe-delete! ( obj index -- )       swap xe-enved@ swap enved-delete! ;

0.03 constant mouse-radius
: grfx ( x gen -- val )
  { x gen }
  gen xe-px0@ gen xe-px1@ = if
    gen xe-px0@
  else
    gen xe-px0@ { px0 }
    gen xe-px1@ { px1 }
    gen xe-bx0@ { bx0 }
    gen xe-bx1@ { bx1 }
    x bx0 f-  bx1 bx0 f-  f/  px1 px0 -  f*  px0  f+  fround->s  px0  max  px1  min
  then
;
: grfy ( y gen -- val )
  { y gen }
  gen xe-py0@ gen xe-py1@ = if
    gen xe-py0@
  else
    gen xe-py0@ { py0 }
    gen xe-py1@ { py1 }
    gen xe-by0@ { by0 }
    gen xe-by1@ { by1 }
    y by1 f-  by0 by1 f-  f/  py0 py1 f-  f*  py1 f+  fround->s  py1  max  py0  min
  then
;
: ungrfx ( x gen -- val )
  { x gen }
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
: ungrfy ( y gen -- val )
  { y gen }
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
360 64 * constant 360*64
: xe-redraw ( gen -- )
  { gen }
  gen xe-drawer@ FXtIsManaged
  gen xe-py0@ gen xe-py1@ > && if
    gen xe-drawer@ { drawer }
    gen xe-gcs@ { gc }
    drawer FXtDisplay { dpy }
    drawer FXtWindow { win }
    dpy win FXClearWindow drop
    drawer gc gen xe-name@ gen xe-bx0@ gen xe-bx1@ gen xe-by0@ gen xe-by1@
    x-axis-in-seconds show-all-axes draw-axes drop
    #f #f { lx ly }
    10 { mouse-d }
    5  { mouse-r }
    gen each { point }
      dpy win gc
      point 0 array-ref gen grfx dup { cx } mouse-r -
      point 1 array-ref gen grfy dup { cy } mouse-r -
      mouse-d mouse-d
      0 360*64 FXFillArc drop
      lx if dpy win gc lx ly cx cy FXDrawLine drop then
      cx to lx
      cy to ly
    end-each
  then
;
: draw-axes-cb ( gen -- proc; w c i self -- )
  3 proc-create swap ( gen ) , ( proc )
 does> { w c i self -- }
  self @ { gen }
  gen xe-drawer@
  gen xe-gcs@
  gen xe-name@
  gen xe-bx0@
  gen xe-bx1@
  gen xe-by0@
  gen xe-by1@
  x-axis-in-seconds
  show-all-axes draw-axes { lst }
  lst 0 array-ref gen xe-px0!
  lst 1 array-ref gen xe-py0!
  lst 2 array-ref gen xe-px1!
  lst 3 array-ref gen xe-py1!
  gen xe-redraw
;
: add-envelope-point ( x y gen -- )
  { x y gen }
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
: mouse-press-cb ( gen -- proc; w c e f self -- )
  4 proc-create swap ,
 does> { w c ev f self -- }
  self @ { gen }
  ev Fx gen ungrfx 1.0 fmin 0.0 fmax { x }
  ev Fy gen ungrfy 1.0 fmin 0.0 fmax { y }
  #f
  gen each { point }
    point 0 array-ref x f- fabs mouse-radius f<
    point 1 array-ref y f- fabs mouse-radius f< && if drop i leave then
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
: mouse-release-cb ( gen -- proc; w c e f self -- )
  4 proc-create swap ,
 does> { w c ev f self -- }
  self @ { gen }
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
: mouse-drag-cb ( gen -- proc; w c e f self -- )
  4 proc-create swap ,
 does> { w c ev f self -- }
  self @ { gen }
  ev Fx gen ungrfx { x }
  ev Fy gen ungrfy { y }
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
: define-cursor-cb ( cursor -- proc; w c e f self -- )
  4 proc-create swap ,
 does> { wid c e f self -- }
  wid FXtDisplay wid FXtWindow self @ ( new-cursor ) FXDefineCursor drop
;
: undefine-cursor-cb ( -- proc; w c e f self -- )
  4 proc-create
 does> { wid c e f self -- }
  wid FXtDisplay wid FXtWindow FXUndefineCursor drop
;
: axis-bounds? ( obj -- f ) array-length 4 = ;
: make-xenved <{ name parent
     :key
     envelope    #( 0 0 1 1 )
     axis-bounds #( 0 1 0 1 )
     args        #() -- xe }>
  parent      FWidget?     parent      2 $" a widget"                _ assert-type
  axis-bounds axis-bounds? axis-bounds 4 $" an array of axis bounds" _ assert-type
  xenved% %alloc { xe }
  xe unless 'system-error #( get-func-name $" cannot create xenved" _ ) fth-throw then
  envelope make-enved xe xe-enved !
  name string? unless $" xe-test" to name then
  name   xe xe-name !
  parent xe xe-parent !
  args   xe xe-args !
  args FXmNbackground array-member? unless
    args #( FXmNbackground graph-color ) array-append to args
  then
  args FXmNforeground array-member? unless
    args #( FXmNforeground data-color ) array-append to args
  then
  name FxmDrawingAreaWidgetClass parent args undef FXtCreateManagedWidget { drawer }
  drawer xe xe-drawer !
  drawer FXtDisplay FXC_crosshair FXCreateFontCursor { arrow-cursor }
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
  xe fth-xenved make-instance { gen }
  drawer FXmNresizeCallback gen draw-axes-cb      	     #f FXtAddCallback drop
  drawer FXmNexposeCallback gen draw-axes-cb      	     #f FXtAddCallback drop
  drawer FButtonPressMask   #f gen mouse-press-cb    	     #f FXtAddEventHandler drop
  drawer FButtonReleaseMask #f gen mouse-release-cb 	     #f FXtAddEventHandler drop
  drawer FButtonMotionMask  #f gen mouse-drag-cb     	     #f FXtAddEventHandler drop
  drawer FEnterWindowMask   #f arrow-cursor define-cursor-cb #f FXtAddEventHandler drop
  drawer FLeaveWindowMask   #f undefine-cursor-cb            #f FXtAddEventHandler drop
  gen
;

lambda: ( obj1 -- obj2 )
  { obj }
  obj xe-name@
  obj xe-parent@
  :envelope    obj xe-envelope@ array-copy
  :args        obj xe-args@     array-copy make-xenved
; is xe-copy
: xe-envelope ( gen -- lst )
  { gen }
  gen xenved? gen 1 $" an xenved object" _ assert-type
  gen xe-envelope@
;
: set-xe-envelope ( gen lst -- )
  { gen lst }
  gen xenved? gen 1 $" an xenved object" _ assert-type
  lst array?  lst 2 $" an array"         _ assert-type
  lst gen xe-envelope!
  gen xe-redraw
;
: xe-open ( gen -- )
  { gen }
  gen xenved? gen 1 $" an xenved object" _ assert-type
  gen xe-drawer@ FWidget? if gen xe-drawer@ FXtManageChild drop then
;
: xe-close ( gen -- )
  { gen }
  gen xenved? gen 1 $" an xenved object" _ assert-type
  gen xe-drawer@ FWidget? if gen xe-drawer@ FXtUnmanageChild drop then
;
previous

: xenved-test <{ :optional name "xenved" -- xe }>
  name
  name FxmFormWidgetClass #( FXmNheight 200 ) add-main-pane
  :envelope    #( 0 0 1 1 )
  :axis-bounds #( 0 1 0 1 )
  :args
  #( FXmNleftAttachment   FXmATTACH_WIDGET
     FXmNtopAttachment    FXmATTACH_WIDGET
     FXmNbottomAttachment FXmATTACH_WIDGET
     FXmNrightAttachment  FXmATTACH_WIDGET ) make-xenved
;

\ xm-enved.fs ends here
