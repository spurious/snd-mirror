\ gfm-defs.fs -- general functions not found in fsndlib and csndlib -*- forth -*-

\ Copyright (C) 2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Mon Sep 27 18:00:28 CEST 2004
\ Last: Fri Oct 08 17:41:00 CEST 2004
\ Ident: $Id: gfm-defs.fs,v 1.17 2004/10/08 15:41:12 mike Exp $

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

\ Code:

require utils.fs

\ === Envelops ===
\ 0.3e 0e 0e 0.5e 1e 1e 0e 6 >vct envelope-interp 3 f.r --> 0.600
: envelope-interp recursive { f: x w: envel -- r }
    assert1( envel ?vct )
    envel vct-length { len }
    len 0= if
	0.0e
    else x 0 envel vct@ f<= len 2 <= or if
	    1 envel vct@
	else 2 envel vct@ x f> if
		1 envel vct@ 3 envel vct@ f= if
		    1 envel vct@
		else
		    1 envel vct@ x 0 envel vct@ f-
		    3 envel vct@ 1 envel vct@ f-
		    2 envel vct@ 0 envel vct@ f- f/ f* f+
		then
	    else
		x
		len 2 do i envel vct@ loop len 2 - >vct
		envelope-interp
	    then
	then
    then
;
' envelope-interp alias interp
' envelope-interp s\" envelope-interp ( f:x envel-vct -- r )\n" >$
s\" \\ 0.3e 0e 0e 0.5e 1e 1e 0e 6 >vct envelope-interp f. ( 0.6 )" $+ help!

: stretch-envelope { fn f: old-att f: new-att f: old-dec f: new-dec -- new-fn }
    fn vct-length { len }
    0 fn vct@ fdup { f: x0 f: new-x }
    1 fn vct@ { f: y0 }
    len 2 - fn vct@ { f: last-x }
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
' stretch-envelope s\" stretch-envelope ( fn f: old-att f: new-att f: old-dec f: new-dec )\n" >$
s\" \ 0e 0e 1e 1e 4 >vct 0.1e 0.2e 0e 0e stretch-envelope .vct\n" $+
s\" \ ==> #<vct[len=6]: 0.000, 0.000, 0.200, 0.100, 1.000, 1.000>\n" $+
s\" \ 0e 0e 1e 1e 2e 0e 6 >vct 0.1e 0.2e 1.5e 1.6e stretch-envelope .vct\n" $+
s\" \ ==> #<vct[len=10]: 0.000, 0.000, 0.200, 0.100, 1.100, 1.000, 1.600, 0.500, 2.000, 0.000>" $+ help!

: normalize-partials { parts -- parts' }
    0e
    parts vct-length 1 do i parts vct@ fabs f+ 2 +loop
    fdup f0= if ." all parts have 0.0 amplitude: " parts .vct cr then
    1/f
    parts vct-length 1 do fdup i parts vct@ f* i parts vct! 2 +loop
    fdrop parts
;
' normalize-partials s" normalize-partials ( parts -- parts' )" help!
: array>partials { data -- parts }
    0 data vct@ f>s data vct-length 0 do i data vct@ f>s max 2 +loop
    1+ make-vct { parts }
    data vct-length 0 do i 1+ data vct@ i data vct@ f>s parts vct! 2 +loop
    parts
;
: inverse-integrate { dist -- dist' }
    512 { size }
    50 { e-size }
    size make-vct { data }
    1 dist vct@ { f: sum }
    sum { f: first-sum }
    0 dist vct@ { f: x0 }
    dist vct-length 2 - dist vct@ { f: x1 }
    x1 x0 f- e-size s>f f/ { f: incr }
    x0 { f: x }
    e-size 2* make-vct { en }
    -1 e-size 1- -do
	sum i en vct!
	x i 1+ en vct!
	sum  x dist envelope-interp  f+ to sum
	x incr f+ to x
    2 -loop
    1 en vct@ first-sum f- size 1 - s>f f/ to incr
    first-sum to x
    size 0 do
	x en envelope-interp i data vct!
	x incr f+ to x
    loop
    en free-vct
    data
;
' inverse-integrate s" inverse-integrate ( vct -- vct' )" help!

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
' spectrum>coeffs s" spectrum>coeffs ( order spectr -- coeffs )" help!

\ === Buffer ===
music5%
    cell% field buf-empty
end-struct buffer%

: buf-increment! { f: val gen -- } val gen fill-time f! val f0= gen buf-empty ! ;
: ?buffer-empty ( gen -- f ) buf-empty @ ;
: ?buffer-full { gen -- f } gen fill-time f@ f>s gen buffer-length @ >= gen location @ 0= and ;

: data-free { gen -- } gen data-buffer @ free-vct gen free throw ;
: .buffer { gen -- }
    ." #<buffer: loc: "  gen location @ . bs
    ." , fill-time: " gen fill-time f@ 1 f.r bs
    ." , size: " gen buffer-length @ . bs
    ." , empty: " gen ?buffer-empty if ." true" else ." false" then
    ." , line: " gen data-buffer @ .vct ." >"
;

\ *table-size* 0e make-buffer value gen
: make-buffer { size f: ftime -- gen }
    buffer% %alloc { gen }
    ftime gen fill-time f!
    size gen buffer-length !
    size make-vct gen data-buffer !
    0 gen location !
    gen fill-time f@ f0= gen buf-empty !
    ['] .buffer gen get-inspect !
    ['] data-free gen get-gen-free !
    s" buffer" gen gen-name 2!
    gen
;
: ?buffer { obj -- f } try obj gen-name 2@ s" buffer" str= recover drop false endtry ;
: buffer>sample { gen -- r }
    gen location @ gen buffer-length @ < if gen location @ gen data-buffer @ vct@ else 0e then
    1 gen location +! gen location @ { loc }
    gen buf-empty @ 0= loc gen fill-time f@ f>s >= and if
	loc gen buffer-length @ < if
	    loc 0 do 0e i gen data-buffer @ vct! loop
	    0 { idx }
	    gen buffer-length @ loc do
		i gen data-buffer @ vct@ idx gen data-buffer @ vct!
		0e i gen data-buffer @ vct!
		idx 1+ to idx
	    loop
	else
	    gen buffer-length @ 0 do 0e i gen data-buffer @ vct! loop
	then
	gen fill-time f@ gen location @ s>f f- gen fill-time f!
	0 gen location ! true gen buf-empty !
    then
;
: sample>buffer { f: val gen -- }
    gen fill-time f@ f>s gen buffer-length @ >= if
	gen location @ 0= abort" ran out of buffer space"
	0 { idx }
	gen buffer-length @ gen location @ do
	    i gen data-buffer @ vct@ idx gen data-buffer @ vct!
	    idx 1+ to idx
	loop
	begin
	    idx gen buffer-length @ <
	while
		0e idx gen data-buffer @ vct!
		idx 1+ to idx
	repeat
	gen location @ negate s>f gen fill-time f@ f+ gen fill-time f!
	0 gen location !
    then
    val gen fill-time f@ f>s gen data-buffer @ vct!
    1e gen fill-time f@ f+ gen fill-time f!
;
' make-buffer s" make-buffer ( size f: ftime -- gen ) \ *table-size* 0e make-buffer value gen" help!
' ?buffer s" ?buffer ( obj -- f )" help!
' buffer>sample s" buffer>sample ( gen -- f: value )" help!
' sample>buffer s" sample>buffer ( f: value gen -- )" help!

\ gfm-defs.fs ends here
