\ -*- snd-forth -*-
\ env.fs -- env.scm -> env.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Thu Oct 27 04:51:42 CEST 2005
\ Changed: Fri Jan 12 16:07:02 CET 2007

\ Commentary:
\
\ From env.scm|rb with original comments and doc strings from env.scm.
\ 
\ envelope?  	     	  ( obj -- f )
\ envelope-copy      	  ( env1 -- env2 )
\ 
\ envelope-interp    	  ( x env :optional base -- r )
\ interp             	  ( x env -- r )
\ window-envelope         ( beg end en1 -- en2 )
\ map-envelopes      	  ( env1 env2 xt -- env3 )
\ add-envelopes      	  ( env1 env2 -- env3 )      alias envelopes+
\ multiply-envelopes 	  ( env1 env2 -- env3 )      alias envelopes*
\ max-envelope       	  ( env -- r )
\ min-envelope       	  ( env -- r )
\ integrate-envelope      ( en -- r )
\ envelope-length    	  ( env -- n )
\ envelope-last-x    	  ( env -- r )
\ stretch-envelope   	  ( env old-attack new-attack :optional old-decay new-decay -- new-env )
\ scale-envelope     	  ( env1 scl :offset offset -- env2 )
\ reverse-envelope   	  ( env1 -- env2 )
\ envelope-concatenate    ( en1 ... enn n -- ne )
\ concatenate-envelopes   ( en1 ... enn n -- ne )
\ repeat-envelope         ( ur-env repeats :optional reflected normalized -- en )
\
\ make-power-env          ( envelope :key scaler offset duration -- pe )
\ power-env               ( pe -- val )
\ power-env-channel       ( pe :optional beg dur snd chn edpos edname -- curbeg )
\ powenv-channel          ( envelope :optional beg dur snd chn edpos -- val )
\ envelope-exp            ( en1 :optional power xgrid -- en2 )
\ rms-envelope            ( file :key beg dur rfreq db -- en )
\
\ normalize-envelope 	  ( env1 -- env2 )

require clm

: envelope? ( obj -- f )
  doc" Returns #t if OBJ is a list, a vct or an array with even length and length >= 2."
  ( obj ) length dup 2 mod 0= swap 2 >= &&
;
' object-copy alias envelope-copy
' envelope-copy $" ( en1 -- en2 )  Copies EN1 which may be a list, a vct, or an array." help-set!

\ ;;; -------- envelope-interp

: envelope-interp <{ x en :optional base 1.0 -- r }>
  doc" Returns value of ENV at X; BASE controls connecting segment type; \
ENV may be a list, a vct, or an array:\n\
0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 ) 1.0 envelope-interp => 0.6"
  en empty? if
    0.0
  else
    en length { size }
    x en 0 object-ref f<= size 2 <= || if
      en 1 object-ref
    else
      en 2 object-ref x f> if
	en 1 object-ref en 3 object-ref f= base f0= || if
	  en 1 object-ref
	else
	  base 1.0 f= if
	    en 1 object-ref
	    x en 0 object-ref f-
	    en 3 object-ref en 1 object-ref f- en 2 object-ref en 0 object-ref f- f/
	    f* f+
	  else
	    en 1 object-ref
	    en 3 object-ref en 1 object-ref f- base 1.0 f- f/
	    base x en 0 object-ref f- en 2 object-ref en 0 object-ref f- f/ f** 1.0 f-
	    f* f+
	  then
	then
      else
	x size 2 ?do en i object-ref loop size 2 - >array base recurse ( envelope-interp )
      then
    then
  then
;
: interp ( x env -- r )
  doc" 0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 ) interp => 0.6"
  1.0 envelope-interp
;

\ ;;; -------- window-envelope (a kinda brute-force translation from the CL version in env.lisp)

: window-envelope ( beg end en1 -- en2 )
  doc" Returns portion of EN1 lying between x axis values BEG and END:\n\
1.0 3.0 '( 0.0 0.0 5.0 1.0 ) window-envelope => '( 1.0 0.2 3.0 0.6 )"
  { beg end en1 }
  nil { en2 }
  en1 if en1 cadr else 0.0 then { lasty }
  #f { return-early? }
  en1 length 0 ?do
    en1 i    list-ref { x }
    en1 i 1+ list-ref { y }
    y to lasty
    en2 null? if
      x beg f>= if
	en2  beg  beg en1 1.0 envelope-interp  3 list-append to en2
	x beg f<> if
	  x end f>= if
	    en2  end  end en1 1.0 envelope-interp  3 list-append to en2
	    #t to return-early?
	    leave
	  else
	    en2 x y 3 list-append to en2
	  then
	then
      then
    else
      x end f<= if
	en2 x y 3 list-append to en2
	x end f= if
	  #t to return-early?
	  leave
	then
      else
	x end f> if
	  en2  end  end en1 1.0 envelope-interp  3 list-append to en2
	  #t to return-early?
	  leave
	then
      then
    then
  2 +loop
  return-early? unless en2 end lasty 3 list-append to en2 then
  en2
;

\ ;;; -------- map-envelopes like map-across-envelopes in env.lisp

hide
: fnumb-cmp ( a b -- -1|0|1 )
  { a b }
  a b f< if
    -1
  else
    a b f= if
      0
    else
      1
    then
  then
;
: (at0) ( en xs-array -- en' )
  { en xs }
  en 0 object-ref { diff }
  en -2 object-ref { lastx }
  en length 0 ?do
    en i object-ref diff f-  lastx f/  ( x ) xs over array-push drop  en i rot object-set!
  2 +loop
  en
;
set-current
: map-envelopes ( en1 en2 xt -- en3 )
  doc" Maps XT over the breakpoints in EN1 and EN2 returning a new envelope."
  { en1 en2 xt }
  #() { xs }
  nil { en3 }
  en1 empty? if
    en2 xs (at0) to en3
  else
    en2 empty? if
      en1 xs (at0) to en3
    else
      en1 xs (at0) { ee1 }
      en2 xs (at0) { ee2 }
      xs array-uniq! ['] fnumb-cmp array-sort! drop
      xs length 2* 0.0 make-array to en3
      xs each { x }
	en3 i 2* x array-set!
	x ee1 1.0 envelope-interp  x ee2 1.0 envelope-interp  xt execute  en3 i 2* 1+ rot array-set!
      end-each
    then
  then
  en1 array? if
    en3
  else
    en1 list? if
      en3 array->list
    else
      en1 vct? if en3 vector->vct then
    then
  then
;
previous

\ ;;; -------- multiply-envelopes, add-envelopes

: add-envelopes ( env1 env2 -- env3 )
  doc" Adds break-points of ENV1 and ENV2 returning a new envelope."
  ['] f+ map-envelopes
;
: multiply-envelopes ( env1 env2 -- env3 )
  doc" Multiplies break-points of ENV1 and ENV2 returning a new envelope:\n\
'( 0 0 2 0.5 ) '( 0 0 1 2 2 1 ) multiply-envelopes => '( 0.0 0.0 0.5 0.5 1.0 0.5 )"
  ['] f* map-envelopes
;
' add-envelopes      alias envelopes+
' multiply-envelopes alias envelopes*

\ ;;; -------- max-envelope

: max-envelope ( en -- r )
  doc" Returns max y value in EN."
  { en }
  en 1 object-ref en length 1 ?do en i object-ref fmax 2 +loop
;

\ ;;; -------- min-envelope

: min-envelope ( en -- r )
  doc" Returns min y value in EN."
  { en }
  en 1 object-ref en length 1 ?do en i object-ref fmin 2 +loop
;

\ ;;; -------- integrate-envelope

: integrate-envelope ( en -- r )
  doc" Area under env."
  { en }
  0.0 ( sum ) en length 3 - 0 ?do
    en i 1+ object-ref en i 3 + object-ref f+ 0.5 f*
    en i 2+ object-ref en i     object-ref f- f*  f+ ( sum+=... )
  2 +loop
  ( sum )
;
: envelope-length ( en -- n )
  doc" Returns number of points in EN."
  length 2/
;

\ ;;; -------- envelope-last-x

: envelope-last-x ( en -- r )
  doc" Returns max x axis break point position"
  -2 object-ref
;

\ ;;; -------- stretch-envelope

: stretch-envelope <{ fn old-attack new-attack :optional old-decay #f new-decay #f -- new-env }>
  doc" Takes ENV and returns a new envelope based on it \
but with the attack and optionally decay portions stretched or squeezed; \
OLD-ATTACK is the original x axis attack end point, \
NEW-ATTACK is where that section should end in the new envelope.  \
Similarly for OLD-DECAY and NEW-DECAY.  \
This mimics divseg in early versions of CLM and its antecedents in Sambox and Mus10 (linen).  \
ENV may be a list, a vct, or an array.\n\
'( 0 0 1 1 )     0.1 0.2         stretch-envelope => '( 0 0 0.2 0.1 1 1 )\n\
'( 0 0 1 1 2 0 ) 0.1 0.2 1.5 1.6 stretch-envelope => '( 0 0 0.2 0.1 1.1 1 1.6 0.5 2 0 )"
  old-decay new-decay not && if
    'argument-error
    '( get-func-name $" old-decay, %s, but no new-decay, %s?" '( old-decay new-decay ) 5 )
    fth-throw
  then
  fn length { len }
  fn 0 object-ref dup { x0 new-x }
  fn 1 object-ref { y0 }
  fn -2 object-ref { last-x }
  #( x0 y0 ) { new-fn }
  new-attack x0 f- 0.0001e old-attack x0 f- fmax f/ { scl }
  old-decay if
    old-decay old-attack f= if old-decay 0.000001e last-x f* f+ to old-decay then
  then
  len 1- 2 ?do
    fn i object-ref { x1 }
    fn i 1+ object-ref { y1 }
    x0 old-attack f<
    x1 old-attack f>= && if
      x1 old-attack f= if
	y1
      else
	y0 y1 y0 f- old-attack x0 f- x1 x0 f- f/ f* f+
      then to y0
      old-attack to x0
      new-attack to new-x
      new-fn new-x array-push y0 array-push drop
      old-decay if
	new-decay new-attack f- old-decay old-attack f- f/
      else
	last-x new-attack f- last-x old-attack f- f/
      then to scl
    then
    old-decay if
      x0 old-decay f<
      x1 old-decay f>= && if
	x1 old-decay f= if
	  y1
	else
	  y0 y1 y0 f- old-decay x0 f- x1 x0 f- f/ f* f+
	then to y0
	old-decay to x0
	new-decay to new-x
	new-fn new-x array-push y0 array-push drop
	last-x new-decay f- last-x old-decay f- f/ to scl
      then
    then
    x0 x1 f<> if
      new-x scl x1 x0 f- f* f+ to new-x
      new-fn new-x array-push y1 array-push drop
      x1 to x0
      y1 to y0
    then
  2 +loop
  fn array? if
    new-fn
  else
    fn list? if
      new-fn array->list
    else
      fn vct? if new-fn vector->vct then
    then
  then
;

\ ;;; -------- scale-envelope

: scale-envelope <{ en scl :optional offset 0 -- new-en }>
  doc" Scales y axis values by SCL and optionally adds OFFSET.  \
EN may be a list, a vct, or an array."
  en map *key* i 2 mod if scl f* offset f+ then end-map ( new-en )
;

\ ;;; -------- reverse-envelope

: reverse-envelope ( en1 -- en2 )
  doc" Reverses the breakpoints in EN1."
  { en1 }
  en1 length { size }
  en1 envelope-copy { en2 }
  size 2 - { idx }
  en1 -2 object-ref { xmax }
  en1 length 1- 0 ?do
    en2 idx     xmax en1 i    object-ref f-  object-set!
    en2 idx 1+       en1 i 1+ object-ref     object-set!
    idx 2 - to idx
  2 +loop
  en2
;

\ ;;; -------- envelope-concatenate from clm/env.lisp

: envelope-concatenate ( en1 ... enn n -- ne )
  doc" Concatenates N envelopes into a new envelope (from clm/env.lisp)."
  >list { envs }
  envs length 1 = if
    envs car dup list? if
      object-copy
    else
      object->array array->list
    then
  else
    0.0 { xoff }
    nil { ne }
    envs each { en }
      en first-ref { firstx }
      en length 0 ?do
	en i    object-ref { x }
	en i 1+ object-ref { y }
	x firstx f- xoff f+ ne cons y swap cons to ne
      2 +loop
      ne cadr 0.01 f+ to xoff
    end-each
    ne list-reverse
  then
;

\ ;;; -------- concatenate-envelopes from snd/env.scm

: concatenate-envelopes ( en1 ... enn n -- ne )
  doc" Concatenates N envelopes into a new envelope (from snd/env.scm)."
  >list { envs }
  envs length 1 = if
    envs car dup list? if
      object-copy
    else
      object->array array->list
    then
  else
    0.0 { xoff }
    nil { ne }
    envs each { en }
      en first-ref { firstx }
      ne null? not
      ne car en second-ref f= && if
	xoff 0.01 f- to xoff
	2
      else
	0
      then { beg }
      en length beg ?do
	en i    object-ref { x }
	en i 1+ object-ref { y }
	x firstx f- xoff f+ ne cons y swap cons to ne
      2 +loop
      ne cadr 0.01 f+ xoff f+ to xoff
    end-each
    ne list-reverse
  then
;

\ ;;; -------- repeat-envelope

: repeat-envelope <{ ur-env repeats :optional reflected #f normalized #f -- en }>
  doc" repeats UR-ENV REPEATS times.\n\
'( 0 0 100 1 ) 2 repeat-envelope => '( 0 0 100 1 101 0 201 1 )\n\
If the final y value is different from the first y value, \
a quick ramp is inserted between repeats.  \
NORMALIZED causes the new envelope's x axis to have the same extent as the original's.  \
REFLECTED causes every other repetition to be in reverse."
  repeats  reflected if 2/ then { times }
  nil { new-env }
  reflected if
    ur-env envelope-last-x   { lastx }
    ur-env list-reverse cddr { rev-env }
    ur-env list-reverse      to new-env
    begin
      rev-env
    while
	lastx rev-env cadr f- lastx f+ new-env cons rev-env car swap cons to new-env
	rev-env cddr to rev-env
    repeat
    new-env list-reverse
  else
    ur-env
  then { e }
  e cadr { first-y }
  e envelope-last-x { x-max }
  e car { x }
  first-y e last-ref f= { first-y-is-last-y }
  '( first-y x ) to new-env
  e length { len }
  times 0 ?do
    len 2 ?do
      e i list-ref e i 2- list-ref f- x f+ to x
      x new-env cons e i 1+ list-ref swap cons to new-env
    2 +loop
    i times 1- < first-y-is-last-y not && if
      x-max 100.0 f/ x f+ to x
      x new-env cons first-y swap cons to new-env
    then
  loop
  new-env list-reverse to new-env
  normalized if
    x-max x f/ { scl }
    new-env map! *key* i 2 mod 0= if scl f* then end-map
  else
    new-env
  then
;

\ ;;; -------- power-env 

: make-power-env <{ envelope :key scaler 1.0 offset 0.0 duration 1.0 -- pe }>
  envelope -3 object-ref envelope 0 object-ref f- { xext }
  0 { jj }
  envelope length 3.0 f/ fround->s 1- ( len ) nil make-array map!
    envelope jj     object-ref { x0 }
    envelope jj 3 + object-ref { x1 }
    envelope jj 1+  object-ref { y0 }
    envelope jj 4 + object-ref { y1 }
    envelope jj 2+  object-ref { base }
    3 +to jj
    :envelope '( 0.0 y0 1.0 y1 )
    :base     base
    :scaler   scaler
    :offset   offset
    :duration x1 x0 f- xext f/ duration f* make-env
  end-map { envs }
  #{ :envs         envs
     :current-env  0
     :current-pass envs 0 array-ref mus-length }
;

: power-env ( pe -- val )
  { pe }
  pe :envs hash-ref pe :current-env hash-ref array-ref env ( val )
  pe :current-pass hash-ref 1- { pass }
  pass 0= if
    pe :current-env  pe :current-env hash-ref 1+  hash-set!
    pe :envs hash-ref pe :current-env hash-ref array-ref mus-length to pass
  then
  pe :current-pass pass hash-set!
  ( val )
;

[defined] env-channel [if]
  hide
  : pec-cb { pe beg snd chn edpos -- prc; self -- curbeg }
    0 proc-create pe , beg , snd , chn , edpos , ( prc )
   does> { self -- curbeg }
    self           @ { pe }
    self   cell+   @ { beg }
    self 2 cells + @ { snd }
    self 3 cells + @ { chn }
    self 4 cells + @ { edpos }
    pe :envs hash-ref each { e }
      e mus-length 1+ { len }
      e beg len snd chn edpos env-channel drop
      len +to beg
    end-each
    beg
  ;
  set-current
  : power-env-channel <{ pe
       :optional beg 0 dur #f snd #f chn #f edpos #f edname "power-env-channel" -- curbeg }>
    pe beg snd chn edpos pec-cb edname as-one-edit
  ;
  previous
[then]

\ ;;; here's a simpler version that takes the breakpoint list, rather than the power-env structure:

[defined] xramp-channel [if]
  hide
  : powc-cb { en beg dur snd chn edpos -- prc; self -- base }
    dur snd chn #f frames || to dur
    0 proc-create en , beg , dur , snd , chn , edpos , ( prc )
   does> { self -- base }
    self           @ { en }
    self   cell+   @ { curbeg }
    self 2 cells + @ { dur }
    self 3 cells + @ { snd }
    self 4 cells + @ { chn }
    self 5 cells + @ { edpos }
    en car { x1 }
    en -3 object-ref x1 f- { xrange }
    en cadr { y1 }
    en caddr { base }
    0.0 0.0 { x0 y0 }
    en length 3 ?do
      x1 to x0
      y1 to y0
      en i    object-ref to x1
      en i 1+ object-ref to y1
      x1 x0 f- xrange f/ dur f* fround->s { curdur }
      y0 y1 base curbeg curdur snd chn edpos xramp-channel drop
      curdur +to curbeg
      en i 2+ object-ref to base
    3 +loop
    base
  ;
  set-current
  : powenv-channel <{ envelope :optional beg 0 dur #f snd #f chn #f edpos #f -- val }>
    \ ;; envelope with a separate base for each segment:
    \ '( 0 0 0.325  1 1 32.0 2 0 32.0 ) powenv-channel
    envelope length 3 = if
      envelope cadr beg dur snd chn edpos scale-channel
    else
      $" %s %s %s %s" '( envelope beg dur get-func-name ) string-format { origin }
      envelope beg dur snd chn edpos powc-cb origin as-one-edit
    then
  ;
  previous
[then]

\ ;;; by Anders Vinjar:
\ ;;;
\ ;;; envelope-exp can be used to create exponential segments to include in
\ ;;; envelopes.  Given 2 or more breakpoints, it approximates the
\ ;;; curve between them using 'xgrid linesegments and 'power as the
\ ;;; exponent. 
\ ;;; 
\ ;;; env is a list of x-y-breakpoint-pairs,
\ ;;; power applies to whole envelope,
\ ;;; xgrid is how fine a solution to sample our new envelope with.

: envelope-exp <{ en1 :optional power 1.0 xgrid 100 -- en2 }>
  en1 min-envelope { mn }
  en1 max-envelope mn f- { largest-diff }
  en1 first-ref { x-min }
  en1 envelope-last-x { x-max }
  x-max x-min f- xgrid f/ { x-incr }
  nil { en2 }
  x-min { x }
  0.0 { y }
  largest-diff f0= if
    begin
      x en1 1.0 envelope-interp to y
      x en2 cons y swap cons to en2
      x-incr x f+ to x
      x x-max f>=
    until
  else
    begin
      x en1 1.0 envelope-interp to y
      x en2 cons  y mn f- largest-diff f/  power  f** largest-diff f* mn f+  swap cons to en2
      x-incr x f+ to x
      x x-max f>=
    until
  then
  en2 list-reverse
;

\ ;;; rms-envelope

[defined] make-sample-reader [if]
  : rms-envelope <{ file :key beg 0.0 dur #f rfreq 30.0 db #f -- en }>
    file find-file to file
    file false? if 'no-such-file '( get-func-name file ) fth-throw then
    nil { en }
    rfreq 1/f { incr }
    file mus-sound-srate { fsr }
    incr fsr f* fround->s { incrsamps }
    beg fsr f* fround->s { start }
    start file 0 1 #f make-sample-reader { reader }
    dur if
      fsr dur f* start f+ fround->s  file mus-sound-frames  min
    else
      file mus-sound-frames
    then { end }
    :size incrsamps make-moving-average { rms }
    end 0 ?do
      0.0 { rms-val }
      incrsamps 0 ?do
	reader '() apply { val }
	rms val dup f* moving-average to rms-val
      loop
      i fsr f/ en cons to en
      rms-val fsqrt to rms-val
      db if
	rms-val 0.00001 f< if
	  -100.0
	else
	  rms-val flog 10.0 flog f/ 20.0 f*
	then
      else
	rms-val
      then en cons to en
    incrsamps +loop
    en list-reverse
  ;
[then]

\ ;;; -------- normalize-envelope

: normalize-envelope <{ en1 :optional new-max 1.0 -- en2 }>
  doc" Scales envelope by NEW-MAX / max-envelope(EN1)."
  en1 second-ref en1 length 1 ?do en1 i object-ref fabs fmax 2 +loop { peak }
  en1 new-max peak f/ 0.0 scale-envelope
;

\ env.fs ends here
