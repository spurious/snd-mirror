\ gfm-defs.fs -- general functions not found in fsndlib and csndlib -*- forth -*-

\ Copyright (C) 2004--2005 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Mon Sep 27 18:00:28 CEST 2004
\ Last: Fri Jan 14 00:17:07 CET 2005
\ Ident: $Id: gfm-defs.fs,v 1.82 2005/01/13 23:17:42 mike Exp $

\ This file is part of GFM Gforth Music.

\ This program is free software; you can redistribute it and/or
\ modify it under the terms of the GNU General Public License as
\ published by the Free Software Foundation; either version 2 of
\ the License, or (at your option) any later version.

\ This program is distributed in the hope that it will be
\ useful, but WITHOUT ANY WARRANTY; without even the implied
\ warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
\ PURPOSE.  See the GNU General Public License for more details.

\ You should have received a copy of the GNU General Public
\ License along with this program; if not, write to the Free
\ Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
\ MA 02111-1307 USA

\ Commentary:
\
\ These functions use partly make-vct and are required in fsndlib.fs
\ and csndlib.fs.  That's why they must be loaded in fsndlib.fs and
\ csndlib.fs after defining make-vct and make-sound-data but before
\ defining further functions in those files.
\
\ Vct
\ Sound-data
\ Envelop
\ normalize-partials
\ array>partials
\ inverse-integrate
\ spectrum>coeffs
\ Hook
\ Matrix
\ Note List
\ Pitches
\ Note Length

\ Code:

require utils.fs

\ === VCT ===
' object@  alias vct@
' object!  alias vct!
' object+! alias vct+!

: vct-first  ( v -- r ) 0 swap vct@ ;
: vct-second ( v -- r ) 1 swap vct@ ;
: vct-third  ( v -- r ) 2 swap vct@ ;
: vct-last   ( v -- r ) dup obj-len @ 1- swap vct@ ;
\ xt ( -- result ) see vct-fill!
: vct-map!    { xt v -- }
    assert1( v ?vct )
    v obj-len @ 0 do xt execute i v vct-store loop
;
: vct-each-xt ( val xt1 -- xt2; r1 self -- r2 )
    lambda-create f, , latestxt
  does> ( r1 self -- r2 )
    dup f@ float+ @ execute
;
\ xt ( val -- result ) see vct-each-xt and vct-scale!, vct-offset!
: vct-each!   { xt v -- }
    assert1( v ?vct )
    v obj-len @ 0 ?do i v vct-fetch xt execute i v vct-store loop
;
\ xt ( val idx -- result )
: vct-each-with-index! { xt v -- }
    assert1( v ?vct )
    v obj-len @ 0 ?do i v vct-fetch i xt execute i v vct-store loop
;
: vct-scale!  ( scl v -- ) ['] f* vct-each-xt swap vct-each! ;
: vct-scale   ( scl v -- v ) vct-copy ['] f* vct-each-xt over vct-each! ;
: vct-offset! ( val v -- ) ['] f+ vct-each-xt swap vct-each! ;
: vct-fill-xt ( val -- xt ) lambda-create f, latestxt does> ( self -- r ) f@ ;
: vct-fill!   ( val v -- ) vct-fill-xt swap vct-map! ;
: vct-fill    ( val v -- ) vct-fill-xt swap vct-copy dup >r vct-map! r> ;
: vct-index { f: val v -- idx|-1 } -1 v each val f= if drop i leave then end-each ;
: ?vct-member ( val v -- f ) vct-index -1 <> ;
\ vct version of binary-search
: fbinary-search { val v -- idx|-1 }
    0 { left }
    v length 1- { right }
    -1
    begin
	right left >=
    while
	    left right + 2/ { middle }
	    val middle v vct@ f= if drop middle exit then
	    val middle v vct@ f< if middle 1- to right else middle 1+ to left then
    repeat
;
: vct-index-bs ( val v -- idx|-1 ) fbinary-search ;
: ?vct-member-bs ( val v -- f ) fbinary-search -1 <> ;
: vct-clear   { v -- } v obj-data @ v obj-len @ sfloats erase ;
: vct-two-xt  { v xt -- xt; f: r1 idx self -- f: r2 }
    lambda-create v , xt , latestxt
  does> { f: r1 idx self -- f: r2 }
    r1 idx self @ vct@ self cell+ @ execute
;
: (vct-two!) { v1 v2 xt -- }
    assert1( v1 ?vct v2 ?vct and )
    v1 obj-len @ v2 obj-len @ = if
	v2 xt vct-two-xt v1 vct-each-with-index!
    else
	v1 obj-len @ v2 obj-len @ min 0 ?do
	    i v1 vct-fetch i v2 vct-fetch xt execute i v1 vct-store
	loop
    then
;
: (vct-two) { v1 v2 xt -- v3 }
    assert1( v1 ?vct v2 ?vct and )
    v1 obj-len @ v2 obj-len @ = if
	v1 vct-copy { v3 }
	v2 xt vct-two-xt v3 vct-each-with-index!
	v3
    else
	v1 obj-len @ v2 obj-len @ min make-vct { v3 }
	v3 obj-len @ 0 ?do i v1 vct-fetch i v2 vct-fetch xt execute i v3 vct-store loop
	v3
    then
;
: vct-add!      ( v1 v2 -- )    ['] f+ (vct-two!) ;
: vct-add       ( v1 v2 -- v3 ) ['] f+ (vct-two) ;
' vct-add! alias vct+
: vct-subtract! ( v1 v2 -- )    ['] f- (vct-two!) ;
: vct-subtract  ( v1 v2 -- v3 ) ['] f- (vct-two) ;
' vct-subtract! alias vct-
: vct-multiply! ( v1 v2 -- )    ['] f* (vct-two!) ;
: vct-multiply  ( v1 v2 -- v3 ) ['] f* (vct-two) ;
' vct-multiply! alias vct*
: vct-peak    { v -- r } 0e v each fabs fmax end-each ;
: vct-max     { v -- r } 0e v each fmax end-each ;
: vct-min     { v -- r } 0e v each fmin end-each ;
: (vct-swap)  { v i-1 i-2 -- } i-1 v vct-fetch i-2 v vct-fetch i-1 v vct-store i-2 v vct-store ;
: (vct-compare) { cmp-xt -- xt; v i-1 i-2 self -- f }
    lambda-create cmp-xt , latestxt
  does> { v i-1 i-2 self -- f }
    i-1 v vct-fetch i-2 v vct-fetch self @ execute 
;
\ vct[ 0.3e 0.1e -0.8e ] ' f< vct-sort!
: vct-sort!   ( cmp-xt v -- )
    assert1( dup ?vct ) dup 0 swap obj-len @ 1- 3 roll (vct-compare) ['] (vct-swap) qsort
;
: vct-sort    ( cmp-xt v1 -- v2 ) vct-copy swap over vct-sort! ;
: vct-sort-<! ( v -- )     ['] f< swap vct-sort! ;
: vct-sort-<  ( v1 -- v2 ) ['] f< swap vct-sort ;
: vct-sort->! ( v -- )     ['] f> swap vct-sort! ;
: vct-sort->  ( v1 -- v2 ) ['] f> swap vct-sort ;
: vct-move!   { i-to i-from back? v -- }
    assert1( i-to v ?range i-from v ?range and )
    i-to i-from > if
	i-to i-from - { diff }
	back? if
	    0 i-to -do i diff - v vct-fetch i v vct-store 1 -loop
	else
	    v obj-len @ i-to do i diff - v vct-fetch i v vct-store loop
	then
    else
	i-from i-to - { diff }
	back? if
	   0 i-from -do i v vct-fetch i diff - v vct-store 1 -loop
	else
	    v obj-len @ i-from do i v vct-fetch i diff - v vct-store loop
	then
    then
;
: vct-reverse! { v -- }
    v obj-len @ { len }
    len 1- { idx }
    len sfloats allocate throw { data }
    v obj-data @ data len sfloats move
    len 0 u+do data i sfloats + sf@  v obj-data @ idx sfloats + sf!  idx 1- to idx loop
    data free throw
;
: vct-push!   { f: val v -- }
    1 v obj-len +!
    v obj-len @ v v-orig @ c-vct-len !
    v obj-len @ v v-buf-len @ > if
	v v-buf-len @ *clm-array-buffer-length* + { buf-len }
	v obj-data @ buf-len sfloats resize throw v obj-data !
	buf-len v v-buf-len !
	v obj-len @ v v-orig @ c-vct-len !
	v obj-data @ v v-orig @ c-vct-data !
    then
    val v obj-data @ v obj-len @ 1- sfloats + sf!
;
: vct-pop!    { v -- f: val }
    v obj-len @ unless
	0e
    else
	-1 v obj-len +!
	v obj-len @ v v-orig @ c-vct-len !
	v obj-data @ v obj-len @ sfloats + sf@
    then
;
: vct-unshift! { f: val v -- }
    1 v obj-len +!
    v obj-len @ v v-orig @ c-vct-len !
    v obj-len @ v v-buf-len @ > if
	v v-buf-len @ *clm-array-buffer-length* + { buf-len }
	v obj-data @ buf-len sfloats resize throw v obj-data !
	buf-len v v-buf-len !
	v obj-len @ v v-orig @ c-vct-len !
	v obj-data @ v v-orig @ c-vct-data !
    then
    v obj-data @ v obj-data @ sfloat+ v obj-len @ 1- sfloats move
    val v obj-data @ sf!
;
: vct-shift!  { v -- f: val }
    v obj-len @ unless
	0e
    else
	-1 v obj-len +!
	v obj-len @ v v-orig @ c-vct-len !
	v obj-data @ sf@
	v obj-data @ sfloat+ v obj-data @ v obj-len @ sfloats move
    then
;
: vct-uniq    { v1 -- v2 }
    v1 ?empty if
	0 make-vct
    else
	0 v1 vct@ 1 >vct { v2 }
	v1 obj-len @ 1 do i v1 vct@ fdup v2 ?vct-member if fdrop else v2 vct-push! then loop
	v2
    then
;
: vct-uniq!   { v -- }
    v vct-uniq { v2 }
    v2 obj-len @ v obj-len !
    v2 obj-data @ v obj-data @ v2 obj-len @ sfloats move
    v2 free-vct
;
: vct-delete! { idx v -- f: val }
    idx 0 v obj-len @ within if
	v obj-data @ idx sfloats + sf@	\ value
	v obj-data @ idx 1+ sfloats + v obj-data @ idx sfloats + v obj-len @ idx - sfloats move
	-1 v obj-len +!
	v obj-len @ v v-orig @ c-vct-len !
    else
	0e
    then
;
: vct-insert! { f: val idx v -- }
    idx 0 v obj-len @ within if
	1 v obj-len +!
	v obj-len @ v v-orig @ c-vct-len !
	v obj-len @ v v-buf-len @ > if
	    v v-buf-len @ *clm-array-buffer-length* + { buf-len }
	    v obj-data @ buf-len sfloats resize throw v obj-data !
	    buf-len v v-buf-len !
	    v obj-len @ v v-orig @ c-vct-len !
	    v obj-data @ v v-orig @ c-vct-data !
	then
	v obj-data @ idx sfloats + v obj-data @ idx 1+ sfloats + v obj-len @ idx - sfloats move
	val v obj-data @ idx sfloats + sf!
    then
;
: vct-and { v1 v2 -- v3 }
    v1 vct-sort-< { vs1 }
    v2 vct-sort-< { vs2 }
    0 make-vct { v3 }
    vs1 length vs2 length >= if
	vs1 length 0 ?do i vs1 vct@ vs2 ?vct-member-bs if i vs1 vct@ v3 vct-push! then loop
    else
	vs2 length 0 ?do i vs2 vct@ vs1 ?vct-member-bs if i vs2 vct@ v3 vct-push! then loop
    then
    vs1 free-vct
    vs2 free-vct
    v3
;
: vct>stack { v -- values } v each end-each ;
\ vct[ 0e 1e 2e ] value v
: (set-vct)   ( addr u -- v ) fdepth >r evaluate fdepth r> - >vct ;
:noname [char] ] parse (set-vct) ;
:noname [char] ] parse postpone sliteral postpone (set-vct) ;
interpret/compile: vct[ immediate
: vct-subseq  { start len v1 -- v2 }
    assert1( start v1 ?range len v1 obj-len @ <= and )
    len start - make-vct { v2 }
    v1 obj-data @ start sfloats + v2 obj-data @ len start - sfloats move
    v2
;

\ float array pointer in csndlib.fs (data@, data! etc.)
: c-float-array@  ( idx ary -- r )   swap sfloats + sf@ ;
: c-float-array!  ( val idx ary -- ) swap sfloats + sf! ;
: c-float-array+! ( val idx ary -- ) 2dup c-float-array@ f+ c-float-array! ;

\ === Sound-Data ===
' length alias sound-data-length
' data-ptr alias sound-data-data

: sound-data-chans { sd -- n } sd sd-chans @ ;
: sound-data@ { frm chn sd -- r }
    assert1( sd ?sound-data )
    assert1( frm 0 sd obj-len @ within chn 0 sd sd-chans @ within and )
    sd obj-data @ chn cells + @ frm sfloats + sf@
;
: sound-data! { f: val frm chn sd -- }
    assert1( sd ?sound-data )
    assert1( frm 0 sd obj-len @ within chn 0 sd sd-chans @ within and )
    val sd obj-data @ chn cells + @ frm sfloats + sf!
;
: vct>sound-data { v chn sd -- }
    assert1( sd ?sound-data )
    assert1( chn 0 sd sd-chans @ within v ?vct and )
    v obj-len @ sd obj-len @ min { len }
    v obj-data @ sd obj-data @ chn cells + @ len sfloats move
;
: sound-data>vct { v chn sd -- }
    assert1( sd ?sound-data )
    assert1( chn 0 sd sd-chans @ within v ?vct and )
    v obj-len @ sd obj-len @ min { len }
    sd obj-data @ chn cells + @ v obj-data @ len sfloats move
;
: sound-data>sound-data { chn1 sd1 chn2 sd2 -- }
    sd1 obj-data @ chn1 cells + @ sd2 obj-data @ chn2 cells + @ sd1 obj-len @ sfloats move
;
: sound-data-scale! { f: scl sd -- }
    sd sound-data-chans 0 do
	sd length 0 do i j sd sound-data@ scl f* i j sd sound-data! loop
    loop
;
: sound-data-clear { sd -- }
    assert1( sd ?sound-data )
    sd sd-chans @ 0 do sd obj-data @ i cells + @ sd obj-len @ sfloats erase loop
;
: sound-data-maxamp { sd -- maxamps-vct }
    sd sd-chans @ make-vct { maxamps-v }
    sd sd-chans @ 0 do
	0e
	sd obj-len @ 0 do sd obj-data @ j cells + @ i sfloats + sf@  fabs fmax loop
	i maxamps-v vct-store
    loop
    maxamps-v
;

\ === Envelop ===
: ?envelope { en -- f }
    en ?vct if
	en length { size }
	size 3 > size 2 mod 0= and if
	    true
	    size 2 - 0 do i en vct@ i 2 + en vct@ f<= and 2 +loop
	else
	    false
	then
    else
	false
    then
;
: max-envelope { en -- r } 1 en vct@ en length 1 do i en vct@ fmax 2 +loop ;
: min-envelope { en -- r } 1 en vct@ en length 1 do i en vct@ fmin 2 +loop ;
: scale-envelope ( en1 scale optional-offset -- en2 )
    fdepth 2 < if 0e then { en1 f: scale f: offset }
    en1 vct-copy { en2 }
    en1 length 1 do i en1 vct@ scale f* offset f+ i en2 vct! 2 +loop
    en2
;
: envelope-last-x { en -- r } en length 2 - en vct@ ;
: envelope-length { en -- n } en length 2/ ;
: normalize-envelope { en1 -- en2 } en1 max-envelope 1/f 0e en1 scale-envelope ;
\ 0.3e vct[ 0e 0e 0.5e 1e 1e 0e ] 1e envelope-interp 3 f.r --> 0.600
: envelope-interp recursive
    fdepth 2 < if 1e then { f: x en f: base }
    assert1( en ?vct )
    en ?empty if
	0e
    else
	en length { size }
	x 0 en vct@ f<= size 2 <= or if
	    1 en vct@
	else
	    2 en vct@ x f> if
		1 en vct@ 3 en vct@ f= base f0= or if
		    1 en vct@
		else
		    base 1e f= if
			1 en vct@
			x 0 en vct@ f-
			3 en vct@ 1 en vct@ f- 2 en vct@ 0 en vct@ f- f/
			f* f+
		    else
			1 en vct@
			3 en vct@ 1 en vct@ f- base 1e f- f/
			base x 0 en vct@ f- 2 en vct@ 0 en vct@ f- f/ f** 1e f-
			f* f+
		    then
		then
	    else
		x size 2 do i en vct@ loop size 2 - >vct base envelope-interp
	    then
	then
    then
;
: stretch-envelope { fn f: old-att f: new-att f: old-dec f: new-dec -- new-fn }
    fn length { len }
    fn vct-first fdup { f: x0 f: new-x }
    fn vct-second { f: y0 }
    fn envelope-last-x { f: last-x }
    x0 y0 2 >vct { new-fn }
    new-att x0 f- 0.0001e old-att x0 f- fmax f/ { f: scl }
    old-dec f0<> old-dec old-att f= and if old-dec 0.000001e last-x f* f+ to old-dec then
    len 1- 2 do
	i fn vct@ { f: x1 }
	i 1+ fn vct@ { f: y1 }
	x0 old-att f<
	x1 old-att f>= and if
	    x1 old-att f= if
		y1
	    else
		y0 y1 y0 f- old-att x0 f- x1 x0 f- f/ f* f+
	    then to y0
	    old-att to x0
	    new-att to new-x
	    new-x new-fn vct-push!
	    y0 new-fn vct-push!
	    old-dec f0<> if
		new-dec new-att f- old-dec old-att f- f/
	    else
		last-x new-att f- last-x old-att f- f/
	    then to scl
	then
	old-dec f0<>
	x0 old-dec f< and
	x1 old-dec f>= and if
	    x1 old-dec f= if
		y1
	    else
		y0 y1 y0 f- old-dec x0 f- x1 x0 f- f/ f* f+
	    then to y0
	    old-dec to x0
	    new-dec to new-x
	    new-x new-fn vct-push!
	    y0 new-fn vct-push!
	    last-x new-dec f- last-x old-dec f- f/ to scl
	then
	x0 x1 f<> if
	    new-x scl x1 x0 f- f* f+ to new-x
	    new-x new-fn vct-push!
	    y1 new-fn vct-push!
	    x1 to x0
	    y1 to y0
	then
    2 +loop
    new-fn
;
: reverse-envelope { en1 -- en2 }
    en1 length { size }
    en1 vct-copy { en2 }
    size 4 >= if
	size 2 - { idx }
	idx en1 vct@ { f: xmax }
	en1 length 1- 0 do
	    xmax i en1 vct@ f- idx en2 vct!
	    i 1+ en1 vct@ idx 1+ en2 vct!
	    idx 2 - to idx
	2 +loop
    then
    en2
;
: (at0) { en xs -- en' }
    en vct-first { f: diff }
    en envelope-last-x { f: lastx }
    en length 0 ?do i en vct@ diff f- lastx f/ fdup xs vct-push! i en vct! 2 +loop
    en
;
: map-envelopes { xt en1 en2 -- en3 }
    0 make-vct { xs }
    en1 ?empty if
	en2 xs (at0)
    else
	en2 ?empty if
	    en2 xs (at0)
	else
	    en1 xs (at0) { ee1 }
	    en2 xs (at0) { ee2 }
	    xs vct-uniq vct-sort-< to xs
	    xs length 2* make-vct { en3 }
	    xs length 0 ?do
		i xs vct@ { f: x }
		x i 2* en3 vct!
		x ee1 1e envelope-interp x ee2 1e envelope-interp xt execute i 2* 1+ en3 vct!
	    loop
	    en3
	then
    then
    xs free-vct
;
: add-envelopes { en1 en2 -- en3 } ['] f+ en1 en2 map-envelopes ;
' add-envelopes alias envelopes+
: multiply-envelopes { en1 en2 -- en3 } ['] f* en1 en2 map-envelopes ;
' multiply-envelopes alias envelopes*
' vct= alias envelope=

\ === Needed by fsndlib.fs as well as by csndlib.fs ===
: normalize-partials { parts -- parts' }
    0e
    parts length 1 do i parts vct@ fabs f+ 2 +loop
    fdup f0= if ." all parts have 0.0 amplitude: " parts .vct cr then
    1/f
    parts length 1 do fdup i parts vct@ f* i parts vct! 2 +loop
    fdrop parts
;
: array>partials { data -- parts }
    0 data vct@ f>s data length 0 do i data vct@ f>s max 2 +loop
    1+ make-vct { parts }
    data length 0 do i 1+ data vct@ i data vct@ f>s parts vct! 2 +loop
    parts
;
: inverse-integrate { dist -- data }
    assert1( dist ?vct )
    512 { size }
    50 { e-size }
    dist vct-first { f: x0 }
    dist vct-second fdup { f: sum f: first-sum }
    dist envelope-last-x x0 f- e-size s>f f/ { f: incr }
    x0 { f: x }
    e-size 1+ 2* make-vct { en }
    en length 1- 0 do
	sum i en vct!
	x i 1+ en vct!
	x dist 1e envelope-interp sum f+ to sum
	x incr f+ to x
    2 +loop
    -2 en vct@ first-sum f- size 1- s>f f/ to incr

    first-sum to x
    size make-vct map x en 1e envelope-interp x incr f+ to x end-map { data }
    en free-vct
    data
;
\ snd-7/dsp.scm
: spectrum>coeffs { ord spectr -- coeffs }
    ord make-vct { coeffs }
    ord 1+ 2/ { m }
    ord s>f { f: n }
    ord 1+ s>f f2/ { f: am }
    two-pi n f/ { f: q }
    ord 1- { idx }
    m 0 do
	0 spectr vct@ f2/ { f: xt }
	m 1 do
	    i spectr vct@  q i s>f f*  am j s>f f- 1e f-  f* fcos f* xt f+ to xt
	loop
	xt n f/ f2* fdup i coeffs vct! idx coeffs vct!
	idx 1- to idx
    loop
    coeffs
;

\ === Hook ===
gfm-base%
    cell% field hook-name
    cell% field hook-arity-n
    cell% field hook-arity-r
    cell% field hook-return-n
    cell% field hook-return-r
    cell% field hook-help
    cell% field hook-xts
    cell% field hook-name-str
end-struct hook%
$" Hook" constant str-hook

: $(.hook) { hook -- }
    $" #<" str-hook $+
    $"  name: " $+ hook hook-name @ $+
    $" , arity(int/iret/float/fret): " $+
    hook hook-arity-n @ $(.) $+ $" /" $+
    hook hook-return-n @ $(.) $+ $" /" $+
    hook hook-arity-r @ $(.) $+ $" /" $+
    hook hook-return-r @ $(.) $+
    $" , xts[" $+ hook hook-xts @ length $(.) $+
    hook hook-xts @ { hooks }
    hooks length if
	$" ]: " $+ hooks each $" \"" $+ 0 rot array@ $+ $" \" " $+ end-each $" \b" $+
    else
	$" ]" $+
    then
    $" >" $+
;
' $(.hook) make-inspect .hook
str-hook make-?obj ?hook
: free-hook ( hook -- )
    dup ?hook if
	dup hook-xts @ free-array free throw
    else
	drop
    then
;
: hook-help@ ( hook -- str ) hook-help @ ;
: hook-help! ( str hook -- ) hook-help ! ;
: .hook-help ( hook -- ) hook-help@ .string ;
: make-hook ( name arity-n arity-r return-n return-r help -- hook )
    hook% %alloc { hook }
    hook hook-help !
    hook hook-return-r !
    hook hook-return-n !
    hook hook-arity-r !
    hook hook-arity-n !
    hook hook-name !
    0 make-array  hook hook-xts !
    ['] free-hook hook gfm-base-free !
    ['] .hook     hook gfm-base-inspect !
    ['] $(.hook)  hook gfm-base-to-string !
    str-hook      hook gfm-base-name !
    hook
;
: hook-add! { xt name hook -- } name xt 2 >array hook hook-xts @ array-push! ;
: hook-remove! { name hook -- }
    hook hook-xts @ { hooks }
    hooks each
	{ ary }
	0 ary array@ name string= if
	    0 ary array@ free-string
	    ary free-array
	    i hooks array-delete!
	    leave
	then
    end-each
;
: ?hook-empty ( hook -- f ) hook-xts @ ?empty ;
: ?hook-not-empty ( hook -- f ) hook-xts @ length ;
: hook-reset! { hook -- }
    hook hook-xts @ { hooks }
    hooks length 0 ?do
	hooks array-shift! { ary }
	0 ary array@ free-string
	ary free-array
    loop
;
: hook-run ( ?? hook -- ?? )
    { hook }
    hook hook-xts @ { hooks }
    hook hook-arity-n @ make-array { n-args }
    n-args array>stack
    hook hook-arity-r @ make-vct { r-args }
    r-args vct>stack
    hook hook-return-n @ make-array { n-ret }
    hook hook-return-r @ make-vct { r-ret }
    hooks each
	{ hk }
	n-args array>stack
	r-args vct>stack
	1 hk array@ execute
	n-ret length 0 ?do i n-ret array+! loop
	r-ret length   0 ?do i r-ret vct+!   loop
    end-each
    n-args free-array
    r-args free-vct
    n-ret array>stack
    n-ret free-array
    r-ret vct>stack
    r-ret free-vct
;

\ === Matrix ===
gfm-base%
    cell% field matrix-row-size
    cell% field matrix-col-size
    cell% field matrix-value
end-struct matrix%
$" Matrix" constant str-matrix

str-matrix make-?obj ?matrix
: ?matrix-square ( matrix -- f ) dup matrix-row-size @ swap matrix-col-size @ = ;
: ?matrix-range { row col matrix -- f }
    row 0 matrix matrix-row-size @ within
    col 0 matrix matrix-col-size @ within and
;
: matrix@ { row col matrix -- r }
    assert1( row col matrix ?matrix-range )
    col row matrix matrix-value @ array@ vct@
;
: matrix! { f: val row col matrix -- }
    assert1( row col matrix ?matrix-range )
    val col row matrix matrix-value @ array@ vct!
;
: matrix+! { f: val row col matrix -- } row col matrix matrix@ val f+ row col matrix matrix! ;
: matrix-row { row mat -- v }
    row mat matrix-value @ array@
;
: matrix-column { col mat -- v }
    mat matrix-row-size @ make-vct { v }
    mat matrix-row-size @ 0 do i col mat matrix@ i v vct! loop
    v
;

: free-matrix { gen -- }
    gen ?matrix if
	gen matrix-value @ gen-free
	nil gen gfm-base-name !
	gen free throw
    then
;
: $(.matrix) { gen -- str }
    $" #<" str-matrix $+ $space $+ gen matrix-value @ gen>string $+ $" >" $+
;
' $(.matrix) make-inspect .matrix
: make-simple-matrix { -- matrix }
    matrix% %alloc { matrix }
    0               matrix matrix-row-size !
    0               matrix matrix-col-size !
    nil             matrix matrix-value !
    ['] free-matrix matrix gfm-base-free !
    ['] .matrix     matrix gfm-base-inspect !
    ['] $(.matrix)  matrix gfm-base-to-string !
    str-matrix      matrix gfm-base-name !
    matrix
;
: make-matrix { row col -- matrix }
    row make-array map col make-vct end-map { ary }
    make-simple-matrix { matrix }
    row matrix matrix-row-size !
    col matrix matrix-col-size !
    ary matrix matrix-value !
    matrix
;
: make-matrix-diagonal { vals -- matrix }
    vals length dup make-matrix { matrix }
    vals each i i matrix matrix-value @ array@ vct! end-each
    matrix
;
: make-matrix-scalar { f: val rows -- matrix }
    rows make-vct { vals }
    val vals vct-fill!
    vals make-matrix-diagonal
    vals free-vct
;
: make-matrix-identity ( rows -- matrix ) 1e make-matrix-scalar ;
: make-matrix-zero ( rows -- matrix ) dup make-matrix ;
: array>matrix { ary row -- matrix }
    make-simple-matrix { matrix }
    row matrix matrix-row-size !
    ary array-first length matrix matrix-col-size !
    row make-array { mat }
    ary each vct-copy i mat array! end-each
    mat matrix matrix-value !
    matrix
;
: matrix-scale! { f: scl matrix -- }
    matrix matrix-value @ { mat }
    mat each scl vct-scale! end-each
;
: matrix-inverse { mat1 -- mat2 }
    mat1 matrix-row-size @ make-matrix-identity { mat2 }
    mat1 matrix-row-size @ { size }
    size 0 do
	i i mat1 matrix@ { f: aii }
	aii f0= if
	    i { idx }
	    begin
		mat2 ?matrix if
		    idx i mat1 matrix@ f0=
		else
		    false
		then
	    while
		idx 1+ to idx
		idx size >= if
		    mat2 free-matrix
		    nil to mat2
		then
	    repeat
	    mat2 ?matrix 0= ?leave
	    idx mat1 matrix-value @ array@
	    i   mat1 matrix-value @ array@ swap
	    i   mat1 matrix-value @ array!
	    idx mat1 matrix-value @ array!
	    idx mat2 matrix-value @ array@
	    i   mat2 matrix-value @ array@ swap
	    i   mat2 matrix-value @ array!
	    idx mat2 matrix-value @ array!
	    i i mat1 matrix@ to aii
	then
	size 0 do
	    i j <> if
		i j mat1 matrix@ aii f/ { f: q }
		0e i j mat1 matrix!
		size j 1+ u+do k i mat1 matrix@ q f* fnegate j i mat1 matrix+! loop
		size 0 do k i mat2 matrix@ q f* fnegate j i mat2 matrix+! loop
	    then
	loop
	size i 1+ u+do j i mat1 matrix@ aii f/ j i mat1 matrix! loop
	size 0 do j i mat2 matrix@ aii f/ j i mat2 matrix! loop
    loop
    mat2
;

\ === Note List ===
fvariable time-now   0.00e time-now f!
fvariable snd-tempo 60.00e snd-tempo f!
fvariable snd-beat   0.25e snd-beat f!

: now@   ( -- secs ) time-now f@ ;
: now!   ( secs -- ) time-now f! ;
: wait   ( secs -- ) now@ f+ now! ;
: tempo@ ( -- r ) snd-tempo f@ ;
: tempo! ( r -- ) snd-tempo f! ;

: show-instruments ( -- )
    ." \ --- " *clm-instruments* length . ." instruments ---" cr
    *clm-instruments* each ." \ " i 1+ 2 u.r ." : " .string cr end-each
;
: ins-info ( ins-name -- ) *notehook* ?dup-if execute else drop then ;
: instrument: ( -- )
    : latest name>string $>string { ins-name }
    ins-name *clm-instruments* array-push!
    ins-name postpone literal ['] ins-info compile,
;
: ;instrument ( -- ) postpone ; ; immediate

: event-info ( name -- ) *verbose* if !script-cr ." \    event: " .$ script-cr else drop then ;
: event: ( -- )
    : latest name>string $>string postpone literal ['] event-info compile,
;
: ;event ( -- ) postpone ; ; immediate

: interp ( x en -- r ) 1e envelope-interp ;
: rescale { f: number f: old-min f: old-max f: new-min f: new-max }
    old-max old-min f- { f: val1 }
    new-max new-min f- { f: val2 }
    number old-min f- val2 f* val1 f/ new-min f+
;

\ === Pitches ===
6.875e fconstant lowest-freq

: interval>hertz ( n -- r ) 2e 12e s>f 3e f+ f+ 12e f/ f** lowest-freq f* ;
: keynum>hertz   ( n -- r ) 2e s>f 3e f+ 12e f/ f** lowest-freq f* ;
: hertz>keynum   ( r -- n ) lowest-freq f/ 2e flogn 12e f* 3e f- f>s ;

: pitch ( interval octave "name" --; self -- freq )
    2e s>f 1e f+ 12e f* s>f 3e f+ f+ 12e f/ f** lowest-freq f*
    create f,
  does> ( self -- freq )
    f@
;

 0 0 pitch |C0    1 0 pitch |Cs0    1 0 pitch |Df0
 2 0 pitch |D0    3 0 pitch |Ds0    3 0 pitch |Ef0
 4 0 pitch |E0    4 0 pitch |Ff0    5 0 pitch |Es0
 5 0 pitch |F0    6 0 pitch |Fs0    6 0 pitch |Gf0
 7 0 pitch |G0    8 0 pitch |Gs0    8 0 pitch |Af0
 9 0 pitch |A0   10 0 pitch |As0   10 0 pitch |Bf0
11 0 pitch |B0   11 0 pitch |Cf0   12 0 pitch |Bs0

 0 1 pitch |C1    1 1 pitch |Cs1    1 1 pitch |Df1
 2 1 pitch |D1    3 1 pitch |Ds1    3 1 pitch |Ef1
 4 1 pitch |E1    4 1 pitch |Ff1    5 1 pitch |Es1
 5 1 pitch |F1    6 1 pitch |Fs1    6 1 pitch |Gf1
 7 1 pitch |G1    8 1 pitch |Gs1    8 1 pitch |Af1
 9 1 pitch |A1   10 1 pitch |As1   10 1 pitch |Bf1
11 1 pitch |B1   11 1 pitch |Cf1   12 1 pitch |Bs1

 0 2 pitch |C2    1 2 pitch |Cs2    1 2 pitch |Df2
 2 2 pitch |D2    3 2 pitch |Ds2    3 2 pitch |Ef2
 4 2 pitch |E2    4 2 pitch |Ff2    5 2 pitch |Es2
 5 2 pitch |F2    6 2 pitch |Fs2    6 2 pitch |Gf2
 7 2 pitch |G2    8 2 pitch |Gs2    8 2 pitch |Af2
 9 2 pitch |A2   10 2 pitch |As2   10 2 pitch |Bf2
11 2 pitch |B2   11 2 pitch |Cf2   12 2 pitch |Bs2

 0 3 pitch |C3    1 3 pitch |Cs3    1 3 pitch |Df3
 2 3 pitch |D3    3 3 pitch |Ds3    3 3 pitch |Ef3
 4 3 pitch |E3    4 3 pitch |Ff3    5 3 pitch |Es3
 5 3 pitch |F3    6 3 pitch |Fs3    6 3 pitch |Gf3
 7 3 pitch |G3    8 3 pitch |Gs3    8 3 pitch |Af3
 9 3 pitch |A3   10 3 pitch |As3   10 3 pitch |Bf3
11 3 pitch |B3   11 3 pitch |Cf3   12 3 pitch |Bs3

 0 4 pitch |C4    1 4 pitch |Cs4    1 4 pitch |Df4
 2 4 pitch |D4    3 4 pitch |Ds4    3 4 pitch |Ef4
 4 4 pitch |E4    4 4 pitch |Ff4    5 4 pitch |Es4
 5 4 pitch |F4    6 4 pitch |Fs4    6 4 pitch |Gf4
 7 4 pitch |G4    8 4 pitch |Gs4    8 4 pitch |Af4
 9 4 pitch |A4   10 4 pitch |As4   10 4 pitch |Bf4
11 4 pitch |B4   11 4 pitch |Cf4   12 4 pitch |Bs4

 0 5 pitch |C5    1 5 pitch |Cs5    1 5 pitch |Df5
 2 5 pitch |D5    3 5 pitch |Ds5    3 5 pitch |Ef5
 4 5 pitch |E5    4 5 pitch |Ff5    5 5 pitch |Es5
 5 5 pitch |F5    6 5 pitch |Fs5    6 5 pitch |Gf5
 7 5 pitch |G5    8 5 pitch |Gs5    8 5 pitch |Af5
 9 5 pitch |A5   10 5 pitch |As5   10 5 pitch |Bf5
11 5 pitch |B5   11 5 pitch |Cf5   12 5 pitch |Bs5

 0 6 pitch |C6    1 6 pitch |Cs6    1 6 pitch |Df6
 2 6 pitch |D6    3 6 pitch |Ds6    3 6 pitch |Ef6
 4 6 pitch |E6    4 6 pitch |Ff6    5 6 pitch |Es6
 5 6 pitch |F6    6 6 pitch |Fs6    6 6 pitch |Gf6
 7 6 pitch |G6    8 6 pitch |Gs6    8 6 pitch |Af6
 9 6 pitch |A6   10 6 pitch |As6   10 6 pitch |Bf6
11 6 pitch |B6   11 6 pitch |Cf6   12 6 pitch |Bs6

 0 7 pitch |C7    1 7 pitch |Cs7    1 7 pitch |Df7
 2 7 pitch |D7    3 7 pitch |Ds7    3 7 pitch |Ef7
 4 7 pitch |E7    4 7 pitch |Ff7    5 7 pitch |Es7
 5 7 pitch |F7    6 7 pitch |Fs7    6 7 pitch |Gf7
 7 7 pitch |G7    8 7 pitch |Gs7    8 7 pitch |Af7
 9 7 pitch |A7   10 7 pitch |As7   10 7 pitch |Bf7
11 7 pitch |B7   11 7 pitch |Cf7   12 7 pitch |Bs7

 0 8 pitch |C8    1 8 pitch |Cs8    1 8 pitch |Df8
 2 8 pitch |D8    3 8 pitch |Ds8    3 8 pitch |Ef8
 4 8 pitch |E8    4 8 pitch |Ff8    5 8 pitch |Es8
 5 8 pitch |F8    6 8 pitch |Fs8    6 8 pitch |Gf8
 7 8 pitch |G8    8 8 pitch |Gs8    8 8 pitch |Af8
 9 8 pitch |A8   10 8 pitch |As8   10 8 pitch |Bf8
11 8 pitch |B8   11 8 pitch |Cf8   12 8 pitch |Bs8

\ === Note Length ===
: bpm>seconds ( bpm -- secs ) 60e fswap f/ ;
: rhythm>seconds ( rhy -- secs ) 4e tempo@ bpm>seconds f* f* ;

: notelength ( scale "name" --; self -- r )
    create f,
  does> ( self -- r )
    f@ rhythm>seconds
;

 1e     notelength |W			\ whole
 2e 1/f notelength |H			\ half
 4e 1/f notelength |Q			\ quarter
 8e 1/f notelength |A			\ eighth
16e 1/f notelength |S			\ sixteenth
32e 1/f notelength |T			\ thirty-second
 1e      2e 1/f f+ notelength |W.
 2e 1/f  4e 1/f f+ notelength |H.
 4e 1/f  8e 1/f f+ notelength |Q.
 8e 1/f 16e 1/f f+ notelength |A.
16e 1/f 32e 1/f f+ notelength |S.

\ gfm-defs.fs ends here
