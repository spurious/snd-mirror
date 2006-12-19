\ -*- snd-forth -*-
\ env.fs -- env.scm -> env.fs

\ Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Thu Oct 27 04:51:42 CEST 2005
\ Changed: Sat Dec 16 04:28:01 CET 2006

\ Commentary:
\
\ A selection of functions from env.lisp|scm|rb.
\ 
\ envelope?  	     	  ( obj -- f )
\ envelope-copy      	  ( env1 -- env2 )
\ envelope-interp    	  ( x env base -- r )
\ interp             	  ( x env -- r )
\ map-envelopes      	  ( env1 env2 xt -- env3 )
\ add-envelopes      	  ( env1 env2 -- env3 )      alias envelopes+
\ multiply-envelopes 	  ( env1 env2 -- env3 )      alias envelopes*
\ max-envelope       	  ( env -- r )
\ min-envelope       	  ( env -- r )
\ envelope-length    	  ( env -- n )
\ envelope-last-x    	  ( env -- r )
\ stretch-envelope   	  ( env old-attack new-attack old-decay new-decay -- new-env )
\ scale-envelope     	  ( env1 scl offset -- env2 )
\ normalize-envelope 	  ( env1 -- env2 )
\ reverse-envelope   	  ( env1 -- env2 )

\ Code:

require clm

: envelope? ( obj -- f )
  doc" Returns #t if OBJ is a list, a vct or an array with even length and length >= 2."
  ( obj ) length dup 2 mod 0= swap 2 >= &&
;

' object-copy alias envelope-copy
' envelope-copy $" ( en1 -- en2 )  Copies EN1 which may be a list, a vct, or an array." help-set!

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

: max-envelope ( en -- r )
  doc" Returns max y value in EN."
  { en }
  en 1 object-ref en length 1 ?do en i object-ref fmax 2 +loop
;

: min-envelope ( en -- r )
  doc" Returns min y value in EN."
  { en }
  en 1 object-ref en length 1 ?do en i object-ref fmin 2 +loop
;

: envelope-length ( en -- n )
  doc" Returns number of points in EN."
  length 2/
;
: envelope-last-x ( en -- r )
  doc" Returns max x axis break point position"
  -2 object-ref
;
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

: scale-envelope <{ en scl :optional offset 0 -- new-en }>
  doc" Scales y axis values by SCL and optionally adds OFFSET.  \
EN may be a list, a vct, or an array."
  en map *key* i 2 mod if scl f* offset f+ then end-map ( new-en )
;

: normalize-envelope ( en1 -- en2 )
  doc" Scales envelope by 1 / max-envelope(EN1)."
  dup max-envelope 1/f 0.0 scale-envelope
;

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

\ env.fs ends here
