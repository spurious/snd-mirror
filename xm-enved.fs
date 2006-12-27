\ -*- snd-forth -*-
\ xm-enved.fs -- xm-enved.scm -> xm-enved.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Fri Oct 21 18:22:57 CEST 2005
\ Changed: Tue Dec 26 19:39:50 CET 2006

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
\ make-xenved 	  	( name parent lst axis-bounds args -- xenved )
\ run-before-enved-hook ( obj pos x y reason -- f )
\ xe-index           	( obj x -- index|-1 )
\ xe-insert!         	( obj index point -- )
\ xe-delete!         	( obj index -- )
\ xe-envelope 	  	( obj -- lst )
\ set-xe-envelope 	( obj lst -- )

\ Code:

require enved

'snd-motif provided? 'xm provided? not && [if] dl-load libxm Init_libxm [then]

'xm provided? [unless]
  'forth-error
  '( *filename* $" %s requires snd-motif and xm module or libxm.so" *filename* #f file-basename )
  fth-throw
[then]

\ === XENVED OBJECT TYPE ===
hide
enved%
  cell% field xe-enved
  cell% field xe-name
  cell% field xe-parent
  cell% field xe-args
  cell% field xe-bounds
  cell% field xe-drawer
  cell% field xe-gcs
  cell% field xe-x0
  cell% field xe-x1
  cell% field xe-y0
  cell% field xe-y1
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
: xe-bounds@     ( obj -- val ) instance-gen-ref xe-bounds @ ;
: xe-name@       ( obj -- val ) instance-gen-ref xe-name @ ;
: xe-parent@     ( obj -- val ) instance-gen-ref xe-parent @ ;
: xe-args@       ( obj -- val ) instance-gen-ref xe-args @ ;
: xe-gcs@        ( obj -- val ) instance-gen-ref xe-gcs @ ;
: xe-drawer@     ( obj -- val ) instance-gen-ref xe-drawer @ ;
: xe-x0@         ( obj -- val ) instance-gen-ref xe-x0 @ ;
: xe-x1@         ( obj -- val ) instance-gen-ref xe-x1 @ ;
: xe-y0@         ( obj -- val ) instance-gen-ref xe-y0 @ ;
: xe-y1@         ( obj -- val ) instance-gen-ref xe-y1 @ ;
: xe-px0@        ( obj -- val ) instance-gen-ref xe-px0 @ ;
: xe-px1@        ( obj -- val ) instance-gen-ref xe-px1 @ ;
: xe-py0@        ( obj -- val ) instance-gen-ref xe-py0 @ ;
: xe-py1@        ( obj -- val ) instance-gen-ref xe-py1 @ ;
: xe-px0!        ( val obj -- ) instance-gen-ref xe-px0 ! ;
: xe-px1!        ( val obj -- ) instance-gen-ref xe-px1 ! ;
: xe-py0!        ( val obj -- ) instance-gen-ref xe-py0 ! ;
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

$" xenved" make-object-type constant fth-xenved

fth-xenved make-?obj xenved?

: xe-length  ( obj -- len ) xe-enved@ enved-length ;
: xe-inspect ( obj -- str )
  { obj }
  $" #<%s[%d]: axis-bounds: '%s, envelope: '%s>"
  '( obj object-name obj xe-length obj xe-bounds@ obj xe-envelope@ ) string-format
;
: xe->string ( obj -- str ) xe-enved@ enved->string ;  
: xe-dump ( obj -- str )
  { obj }
  $" %s %s %s %s %s make-xenved"
  '( obj xe-name@     object-dump
     obj xe-parent@   object-dump
     obj xe-envelope@ object-dump
     obj xe-bounds@   object-dump
     obj xe-args@     object-dump ) string-format
;
: xe->array  ( obj -- ary )         xe-enved@ enved->array ;  
\ XE-COPY comes below after MAKE-XENVED
: xe-ref     ( obj index -- point ) swap xe-enved@ swap enved-ref ;
: xe-set!    ( obj index point -- ) rot xe-enved@ -rot enved-set! ;
: xe-equal?  ( obj1 obj2 -- f )
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
  obj xe-x0@ 	     object-mark
  obj xe-x1@ 	     object-mark
  obj xe-y0@ 	     object-mark
  obj xe-y1@ 	     object-mark
  obj xe-bounds@     object-mark
  obj xe-mouse-up@   object-mark
  obj xe-mouse-down@ object-mark
  obj xe-click-time@ object-mark
;
: xe-free ( obj -- ) instance-gen-ref free throw ;

\ Init xenved
' xe-inspect  fth-xenved set-object-inspect   \ xenved .inspect
' xe->string  fth-xenved set-object->string   \ xenved object->string
' xe-dump     fth-xenved set-object-dump      \ xenved object-dump
' xe->array   fth-xenved set-object->array    \ xenved object->array
\ XE-COPY comes below after MAKE-XENVED
' xe-ref      fth-xenved set-object-value-ref \ xenved index        object-ref => '(x y)
' xe-set!     fth-xenved set-object-value-set \ xenved index '(x y) object-set!
' xe-equal?   fth-xenved set-object-equal-p   \ obj1 obj2 equal?
' xe-length   fth-xenved set-object-length    \ xenved object-length => number of points (lstlen/2)
' xe-mark     fth-xenved set-object-mark      \ xenved object-mark and for gc
' xe-free     fth-xenved set-object-free      \ for gc
' xe-ref      fth-xenved 1 set-object-apply   \ xenved index apply => '(x y)

5 $" ( gen pos x y reason -- f )  \
Will be called before changing a breakpoint in GEN's envelope.  \
This hook runs the global ENVED-HOOK at first, \
subsequent procedures can directly manipulate GEN's envelope \
or the returned list of the preceding hook procedure.\n\
This instance hook is like the global ENVED-HOOK; \
POS is ENVELOPE's x-position, X and Y are the new points, \
and REASON is one of the Snd constants ENVED-ADD-POINT, ENVED-DELETE-POINT, ENVED-MOVE-POINT.  \
If one of the hook procedures in the hook list returns #f, xenved changes the breakpoint, \
otherwise the last hook procedure is responsible for manipulating GEN's envelope itself." _
create-hook before-enved-hook

before-enved-hook lambda: <{ gen pos x y reason -- f }>
  enved-hook hook-empty? if
    #f
  else
    gen xe-envelope@ { res }
    enved-hook hook->list each { prc }
      prc '( res pos x y reason ) run-proc to res
      res false? ?leave
    end-each
    res list? if
      res gen xe-envelope!
    else
      res enved? if
	res envelope@ gen xe-envelope!
      else
	res xenved? if
	  res xe-envelope@ gen xe-envelope!
	then
      then
    then
    res #f <> if #t else #f then
  then
; add-hook!

: run-before-enved-hook ( gen x y reason -- f )
  { gen x y reason }
  before-enved-hook hook-empty? if
    #t
  else
    #f					\ flag
    before-enved-hook hook->list each { prc }
      prc '( gen gen xe-mouse-pos@ x y reason ) run-proc false? if
	not				\ toggle flag
	leave
      then
    end-each
  then
;

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
    gen xe-x0@  { x0 }
    gen xe-x1@  { x1 }
    px1 px0  px0  px1 px0 - x x0 f- x1 x0 f- f/ f* fround->s + max min
  then
;
: grfy ( y gen -- val )
  { y gen }
  gen xe-py0@ gen xe-py1@ = if
    gen xe-py0@
  else
    gen xe-py0@ { py0 }
    gen xe-py1@ { py1 }
    gen xe-y0@  { y0 }
    gen xe-y1@  { y1 }
    py0 py1  py1  py0 py1 - y y1 f- y0 y1 f- f/ f* fround->s + max min
  then
;
: ungrfx ( x gen -- val )
  { x gen }
  gen xe-px0@ gen xe-px1@ = if
    gen xe-x0@
  else
    gen xe-px0@ { px0 }
    gen xe-px1@ { px1 }
    gen xe-x0@  { x0 }
    gen xe-x1@  { x1 }
    x1 x0  x0  x1 x0 f- x px0 f- px1 px0 f- f/ f* f+ fmax fmin
  then
;
: ungrfy ( y gen -- val )
  { y gen }
  gen xe-py0@ gen xe-py1@ = if
    gen xe-y1@
  else
    gen xe-py0@ { py0 }
    gen xe-py1@ { py1 }
    gen xe-y0@  { y0 }
    gen xe-y1@  { y1 }
    y1 y0  y0  y1 y0 f- py0 y f- py0 py1 f- f/ f* f+ fmax fmin
  then
;
: xe-redraw ( gen -- )
  { gen }
  gen xe-drawer@ FXtIsManaged
  gen xe-py0@ gen xe-py1@ > && if
    gen xe-drawer@ { drawer }
    gen xe-gcs@ car { gc }
    drawer FXtDisplay { dpy }
    drawer FXtWindow { win }
    dpy win FXClearWindow drop
    drawer gc gen xe-name@ gen xe-x0@ gen xe-x1@ gen xe-y0@ gen xe-y1@
    x-axis-in-seconds show-all-axes draw-axes drop
    #f #f { lx ly }
    10 { mouse-d }
    5  { mouse-r }
    gen each { point }
      dpy win gc
      point car  gen grfx dup { cx } mouse-r -
      point cadr gen grfy dup { cy } mouse-r -
      mouse-d mouse-d
      0 360 64 * FXFillArc drop
      lx if dpy win gc lx ly cx cy FXDrawLine drop then
      cx to lx
      cy to ly
    end-each
  then
;
: draw-axes-cb ( gen -- proc; w c i self -- )
  3 proc-create swap ,
 does> { w c i self -- }
  self @ { gen }
  gen xe-drawer@ { drawer }
  gen xe-gcs@ car { gc }
  drawer gc gen xe-name@ gen xe-x0@ gen xe-x1@ gen xe-y0@ gen xe-y1@
  x-axis-in-seconds show-all-axes draw-axes { lst }
  lst car    gen xe-px0!
  lst cadr   gen xe-py0!
  lst caddr  gen xe-px1!
  lst cadddr gen xe-py1!
  gen xe-redraw
;
: add-envelope-point ( x y gen -- )
  { x y gen }
  gen xe-mouse-pos@ { mpos }
  gen x xe-index dup 0>= if
    to mpos
  else
    drop
    gen each car x f> if i to mpos leave then end-each
  then
  gen mpos '( x y ) xe-insert!
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
    point car  x f- fabs mouse-radius f<
    point cadr y f- fabs mouse-radius f< && if drop i leave then
  end-each { pos }
  pos not gen xe-mouse-new!
  time gen xe-mouse-down!
  pos number? if
    pos gen xe-mouse-pos!
  else
    gen x y enved-add-point run-before-enved-hook if
      x y gen add-envelope-point
    then
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
	  gen point car point cadr enved-delete-point run-before-enved-hook if
	    gen mpos xe-delete!
	  then
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
    gen 0 xe-ref car
  else
    mpos gen xe-length 1- >= if
      gen -1 xe-ref car
    else
      gen mpos 1- xe-ref car  gen mpos 1+ xe-ref car  x fmin fmax
    then
  then to x
  gen x y enved-move-point run-before-enved-hook if
    gen mpos '( x y ) xe-set!
  then
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
: axis-bounds? { obj -- f } obj object-length 4 = obj list? && ;
: make-xenved <{ name parent envelope axis-bounds args -- xenved }>
  parent      FWidget?     parent      2 $" a widget"              _ assert-type
  axis-bounds axis-bounds? axis-bounds 4 $" a list of axis bounds" _ assert-type
  xenved% %alloc { xe }
  xe unless 'system-error '( get-func-name $" cannot create xenved" _ ) fth-throw then
  envelope make-enved xe xe-enved !
  name string? unless $" xe-test" to name then
  name   xe xe-name !
  parent xe xe-parent !
  args   xe xe-args !
  args FXmNbackground list-member? unless
    args '( FXmNbackground graph-color ) 2 list-append to args
  then
  args FXmNforeground list-member? unless
    args '( FXmNforeground data-color ) 2 list-append to args
  then
  name FxmDrawingAreaWidgetClass parent args undef FXtCreateManagedWidget { drawer }
  drawer xe xe-drawer !
  drawer FXtDisplay FXC_crosshair FXCreateFontCursor { arrow-cursor }
  '( snd-gcs car  snd-gcs 7 list-ref ) xe xe-gcs !
  axis-bounds car    xe xe-x0 !
  axis-bounds cadr   xe xe-x1 !
  axis-bounds caddr  xe xe-y0 !
  axis-bounds cadddr xe xe-y1 !
  '( axis-bounds car axis-bounds caddr axis-bounds cadr axis-bounds cadddr ) xe xe-bounds !
  #f xe xe-px0 !			\ points == ints
  #f xe xe-px1 !
  #f xe xe-py0 !
  #f xe xe-py1 !
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

: xe-copy ( obj1 -- obj2 )
  { obj }
  obj xe-name@
  obj xe-parent@
  obj xe-envelope@ list-copy
  obj xe-bounds@   list-copy
  obj xe-args@     list-copy make-xenved
;  
' xe-copy     fth-xenved set-object-copy      \ xenved object-copy

: xe-envelope ( gen -- lst )
  { gen }
  gen xenved? gen 1 $" an xenved object" _ assert-type
  gen xe-envelope@ dup list? unless
    drop
    gen xe-bounds@ { bnds }
    '( bnds 0 list-ref bnds 1 list-ref bnds 2 list-ref bnds 1 list-ref )
  then
;
: set-xe-envelope ( gen lst -- )
  { gen lst }
  gen xenved? gen 1 $" an xenved object" _ assert-type
  lst list?   lst 2 $" a list"           _ assert-type
  lst gen xe-envelope!
  gen xe-redraw
;

previous

\ xm-enved.fs ends here
