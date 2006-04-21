\ -*- snd-forth -*-
\ env.fs -- env.scm -> env.fs

\ Translator/Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Thu Oct 27 04:51:42 CEST 2005
\ Changed: Thu Mar 02 11:28:06 CET 2006

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
\ scale-envelope     	  ( lst1 scaler -- lst2 )
\ normalize-envelope 	  ( env1 -- env2 )
\ reverse-envelope   	  ( env1 -- env2 )

\ Code:

require clm

: envelope? { obj -- f }
  doc" ( obj -- f )  \
Returns #t if OBJ is a list, a vct or an array with even length and length >= 2."
  obj list?
  obj array? ||
  obj vct?   ||
  obj length dup 2 mod 0= swap 2 >= && &&
;

: envelope-copy { en1 -- en2 }
  doc" ( env1 -- env2 )  Copies ENV1 which may be a list, a vct, or an array."
  en1 envelope? en1 1 running-word $" an envelope" _ assert-type
  en1 object-copy
;

: envelope-interp ( x env base -- r )
  doc" ( x env base -- r )  Returns value of ENV at X; BASE controls connecting segment type; \
ENV may be a list, a vct, or an array:\n\
0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 ) 1.0 envelope-interp .g => 0.6"
  { x en base }
  x float?     x    1 running-word $" a float"     _ assert-type
  en envelope? en   2 running-word $" an envelope" _ assert-type
  base float?  base 3 running-word $" a float"     _ assert-type
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
  doc" ( x env -- r )  0.3 #( 0.0 0.0 0.5 1.0 1.0 0.0 ) interp .g => 0.6"
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
: map-envelopes { en1 en2 xt -- en3 }
  doc" ( env1 env2 xt -- env3 )  \
Maps XT over the breakpoints in ENV1 and ENV2 returning a new envelope."
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
      xs array-uniq ['] fnumb-cmp array-sort! to xs
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
  doc" ( env1 env2 -- env3 )  Adds break-points of ENV1 and ENV2 returning a new envelope."
  ['] f+ map-envelopes
;
: multiply-envelopes ( env1 env2 -- env3 )
  doc" ( env1 env2 -- env3 )  Multiplies break-points of ENV1 and ENV2 returning a new envelope:\n\
'( 0e 0e 2.0 0.5 ) '( 0e 0e 1.0 2.0 2.0 1.0 ) multiply-envelopes .g => (0.0 0.0 0.5 0.5 1.0 0.5)"
  ['] f* map-envelopes
;

' add-envelopes      alias envelopes+
' multiply-envelopes alias envelopes*

: max-envelope { en -- r }
  doc" ( env -- r )  Returns max y value in ENV."
  en 1 object-ref en length 1 do en i object-ref fmax 2 +loop
;

: min-envelope { en -- r }
  doc" ( env -- r )  Returns min y value in ENV."
  en 1 object-ref en length 1 do en i object-ref fmin 2 +loop
;

: envelope-length ( env -- n )
  doc" ( env -- n ) Returns number of points in ENV."
  length 2/
;
: envelope-last-x ( env -- r )
  doc" ( env -- r ) Returns max x axis break point position"
  -2 object-ref
;

: stretch-envelope { fn old-att new-att old-dec new-dec -- new-env }
  doc" ( env old-attack new-attack old-decay new-decay -- new-env )  \
Takes ENV and returns a new envelope based on it \
but with the attack and optionally decay portions stretched or squeezed; \
OLD-ATTACK is the original x axis attack end point, \
NEW-ATTACK is where that section should end in the new envelope.  \
Similarly for OLD-DECAY and NEW-DECAY.  \
This mimics divseg in early versions of CLM and its antecedents in Sambox and Mus10 (linen).  \
ENV may be a list, a vct, or an array.\n\
#( 0.0 0.0 1.0 1.0 ) 0.1 0.2 #f #f stretch-envelope .g =>\n\
  (0.0 0.0 0.2 0.1 1.0 1.0)\n\
#( 0.0 0.0 1.0 1.0 2.0 0.0 ) 0.1 0.2 1.5 1.6 stretch-envelope .g =>\n\
  (0.0 0.0 0.2 0.1 1.1 1.0 1.6 0.5 2.0 0.0)"
  fn envelope? fn 1 running-word $" an envelope" _ assert-type
  fn length { len }
  fn 0 object-ref dup { x0 new-x }
  fn 1 object-ref { y0 }
  fn -2 object-ref { last-x }
  #( x0 y0 ) { new-fn }
  new-att x0 f- 0.0001e old-att x0 f- fmax f/ { scl }
  old-dec float? if
    old-dec old-att f= if old-dec 0.000001e last-x f* f+ to old-dec then
  then
  len 1- 2 do
    fn i object-ref { x1 }
    fn i 1+ object-ref { y1 }
    x0 old-att f<
    x1 old-att f>= && if
      x1 old-att f= if
	y1
      else
	y0 y1 y0 f- old-att x0 f- x1 x0 f- f/ f* f+
      then to y0
      old-att to x0
      new-att to new-x
      new-fn new-x array-push y0 array-push drop
      old-dec float? if
	new-dec new-att f- old-dec old-att f- f/
      else
	last-x new-att f- last-x old-att f- f/
      then to scl
    then
    old-dec float? if
      x0 old-dec f<
      x1 old-dec f>= && if
	x1 old-dec f= if
	  y1
	else
	  y0 y1 y0 f- old-dec x0 f- x1 x0 f- f/ f* f+
	then to y0
	old-dec to x0
	new-dec to new-x
	new-fn new-x array-push y0 array-push drop
	last-x new-dec f- last-x old-dec f- f/ to scl
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

: scale-envelope { en1 scl -- en2 }
  doc" ( env1 scaler -- env2 )  \
Scales y axis values by SCALER.  ENV1 may be a list, a vct, or an array."
  en1 envelope-copy { en2 }
  en2 length 1 do en2 i scl object-set*! 2 +loop
  en2
;

: normalize-envelope ( env1 -- env2 )
  doc" ( env1 -- env2 )  Scales envelope by 1 / max-envelope(ENV1)."
  dup max-envelope 1/f scale-envelope
;

: reverse-envelope { en1 -- en2 }
  doc" ( env1 -- env2 )  Reverses the breakpoints in ENV."
  en1 envelope? en1 1 running-word $" an envelope" _ assert-type
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
