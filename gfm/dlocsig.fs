\ dlocsig.fs -- dlocsig.lisp -*- forth -*-

\ Copyright (C) 2004--2005 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Fri Nov 26 07:45:28 CET 2004
\ Last: Thu Jan 13 15:40:06 CET 2005
\ Ident: $Id: dlocsig.fs,v 1.33 2005/01/13 14:40:37 mike Exp $

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

\ Original Copyright of Fernando Lopez Lezcano:

\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
\ ;;; Copyright (c) 92, 93, 94, 98, 99, 2000, 2001 Fernando Lopez Lezcano. 
\ ;;; All rights reserved.
\ ;;; Use and copying of this software and preparation of derivative works
\ ;;; based upon this software are permitted and may be copied as long as 
\ ;;; no fees or compensation are charged for use, copying, or accessing
\ ;;; this software and all copies of this software include this copyright
\ ;;; notice. Suggestions, comments and bug reports are welcome. Please 
\ ;;; address email to: nando@ccrma.stanford.edu
\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
\ ;;; Dynamic multichannel three-dimentional signal locator
\ ;;; (wow that sound good! :-)
\ ;;;
\ ;;; by Fernando Lopez Lezcano
\ ;;;    CCRMA, Stanford University
\ ;;;    nando@ccrma.stanford.edu
\ ;;;
\ ;;; Thanks to Juan Pampin for help in the initial coding of the new version
\ ;;; and for prodding me to finish it. To Joseph L. Anderson and Marcelo Perticone
\ ;;; for insights into the Ambisonics coding and decoding process. 
\ ;;; http://www.york.ac.uk/inst/mustech/3d_audio/ambison.htm for more details...

\ Commentary:

\ Code:

require utils.fs
only forth also definitions
also CLM-Sndlib definitions
also Utils
also GFMusic

1 constant amplitude-panning
2 constant b-format-ambisonics
3 constant decoded-ambisonics

false value *dlocsig-debug*

: dl-warn ( msg -- ) *dlocsig-debug* if warn else drop then ;

360e              fvalue    *dlocsig-one-turn*
344e              fvalue    *dlocsig-speed-of-sound*
two-pi 8e f/ fcos fconstant *dlocsig-point-707*
8                 constant  *dlocsig-path-maxcoeff*
    
gfm-base%
    cell% field sp-number
    cell% field sp-coords
    cell% field sp-groups
end-struct speaker-config%
$" Speaker-config" constant str-speaker-config

gfm-base%
    cell% field group-size
    cell% field group-vertices
    cell% field group-speakers
    cell% field group-matrix
end-struct group%
$" Group" constant str-group

speaker-config%
    cell%   field dl-path-path
    cell%   field dl-render-using
    cell%   field dl-out-channels
    cell%   field dl-rev-channels
    cell%   field dl-delay
    cell%   field dl-path
    cell%   field dl-start
    cell%   field dl-end
    cell%   field dl-output-gains
    cell%   field dl-reverb-gains
    cell%   field dl-prev-group
    cell%   field dl-output
    cell%   field dl-revput
    cell%   field dl-out-chan-gains
    cell%   field dl-rev-chan-gains
    cell%   field dl-delays
    cell%   field dl-speaker-config
    sfloat% field dl-prev-time
    sfloat% field dl-prev-dist
    sfloat% field dl-prev-x
    sfloat% field dl-prev-y
    sfloat% field dl-prev-z
    sfloat% field dl-first-dist
    sfloat% field dl-last-dist
    sfloat% field dl-min-dist
    sfloat% field dl-max-dist
    sfloat% field dl-speed-limit
    sfloat% field dl-output-power
    sfloat% field dl-reverb-power
end-struct dlocsig%
$" dlocsig" constant str-dlocsig

: cis { f: val -- complex } val fcos val fsin make-complex ;
: dl-distance { f: x f: y f: z -- r } x fdup f* y fdup f* f+ z fdup f* f+ fsqrt ;

str-group make-?obj ?group
: group-free { gen -- }
    gen ?group if
	gen group-vertices @ gen-free
	gen group-speakers @ gen-free
	gen group-matrix @ gen-free
	nil gen gfm-base-name !
	gen free throw
    then
;
: $(.group) { gen -- str }
    $" #<" str-group $+
    $"  size: " $+ gen group-size @ gen>string $+
    $" , vertices:" $+ gen group-vertices @ gen>string $+
    $" , speakers: " $+ gen group-speakers @ gen>string $+
    $" , matrix: " $+ gen group-matrix @ gen>string $+
    $" >" $+
;
' $(.group) make-inspect .group
: make-group { size vertice speakers matrix -- group }
    group% %alloc { group }
    size           group group-size !
    vertice        group group-vertices !
    speakers       group group-speakers !
    matrix         group group-matrix !
    ['] group-free group gfm-base-free !
    ['] .group     group gfm-base-inspect !
    ['] $(.group)  group gfm-base-to-string !
    str-group      group gfm-base-name !
    group
;

str-speaker-config make-?obj ?speaker-config
: speaker-config-free { gen -- }
    gen ?speaker-config if
	gen sp-coords @ gen-free
	gen sp-groups @ gen-free
	nil gen gfm-base-name !
	gen free throw
    then
;
: $(.speaker-config) { gen -- str }
    $" #<" str-speaker-config $+
    $"  number: " $+ gen sp-number @ gen>string $+
    $" , coords: " $+ gen sp-coords @ gen>string $+
    $" , groups: " $+ gen sp-groups @ gen>string $+
    $" >" $+
;
' $(.speaker-config) make-inspect .speaker-config
: make-speaker-config { size coords groups -- sp }
    speaker-config% %alloc { sp }
    size                    sp sp-number !
    coords                  sp sp-coords !
    groups                  sp sp-groups !
    ['] speaker-config-free sp gfm-base-free !
    ['] .speaker-config     sp gfm-base-inspect !
    ['] $(.speaker-config)  sp gfm-base-to-string !
    str-speaker-config      sp gfm-base-name !
    sp
;
: arrange-speakers { ary -- sc }
    0 ary array@ { speakers }
    1 ary array@ { groups }
    speakers length { len }
    len make-array map
	speakers ?array if
	    \ 3d
	    i speakers array@ { s }
	    s vct-first *dlocsig-one-turn* f/ two-pi f* cis
	    s vct-second *dlocsig-one-turn* f/ two-pi f* cis
	else
	    \ 2d
	    i speakers vct@ *dlocsig-one-turn* f/ two-pi f* cis
	    0e cis
	then { avec evec } 
	evec real@ avec image@ f* { f: x }
	evec real@ avec real@  f* { f: y }
	evec image@               { f: z }
	x y z dl-distance         { f: mag }
	x mag f/ y mag f/ z mag f/ 3 >vct
	avec gen-free
	evec gen-free
    end-map { coords }
    groups unless
	len make-array map i i 1+ 2 >array end-map to groups
	0 -1 groups array-last array!
    then
    len make-array map
	i groups array@ { grp }
	grp length { size }
	size make-array { vertices }
	size 0 ?do i grp array@ coords array@ i vertices array! loop
	size case
	    3 of vertices 3 array>matrix matrix-inverse endof
	    2 of vertices 2 array>matrix matrix-inverse endof
	    nil swap
	endcase { matrix }
	size vertices grp array-copy matrix make-group
    end-map { group }
    len coords group make-speaker-config
;

nil
    0e                                                    1 >vct nil 2 >array
  -60e   60e                                              2 >vct nil 2 >array
  -45e   45e   180e                                       3 >vct nil 2 >array
  -45e   45e   135e   225e                                4 >vct nil 2 >array
  -45e    0e    45e   135e   -135e                        5 >vct nil 2 >array
  -60e    0e    60e   120e    180e    240e                6 >vct nil 2 >array
  -45e    0e    45e   100e    140e   -140e   -100e        7 >vct nil 2 >array
-22.5e 22.5e  67.5e 112.5e  157.5e  202.5e  247.5e 292.5e 8 >vct nil 2 >array
9 >array
nil nil nil nil
vct[ -60e 0e ] vct[ 60e 0e ] vct[ 180e 0e ] vct[ 0e 90e ] 4 >array
array[ 0 1 3 ] array[ 1 2 3 ] array[ 2 0 3 ] array[ 0 1 2 ] 4 >array 2 >array
vct[ -45e 0e ] vct[ 45e 0e ] vct[ 135e 0e ] vct[ -135e 0e ] vct[ 0e 90e ] 5 >array
array[ 0 1 4 ] array[ 1 2 4 ]
array[ 2 3 4 ] array[ 3 0 4 ] array[ 0 1 2 ] array[ 2 3 0 ] 6 >array 2 >array
vct[ -45e 0e ] vct[ 45e 0e ] vct[ 135e 0e ] vct[ -135e 0e ] vct[ -90e 60e ] vct[ 90e 60e ] 6 >array
array[ 0 1 4 ] array[ 1 4 5 ] array[ 1 2 5 ] array[ 2 3 5 ]
array[ 3 4 5 ] array[ 3 0 4 ] array[ 0 1 2 ] array[ 2 3 0 ] 8 >array 2 >array
vct[ -45e 0e ] vct[ 45e 0e ] vct[ 135e 0e ] vct[ -135e 0e ]
vct[ -60e 60e ] vct[ 60e 60e ] vct[ 180e 60e ] 7 >array
array[ 0 1 4 ] array[ 1 4 5 ] array[ 1 2 5 ]
array[ 2 6 5 ] array[ 2 3 6 ] array[ 3 4 6 ]
array[ 3 0 4 ] array[ 4 5 6 ] array[ 0 1 2 ] array[ 2 3 0 ] 10 >array 2 >array
vct[ -45e -10e ] vct[ 45e -10e ] vct[ 135e -10e ] vct[ 225e -10e ]
vct[ -45e 45e ] vct[ 45e 45e ] vct[ 135e 45e ] vct[ 225e 45e ] 8 >array
array[ 0 4 5 ] array[ 0 5 1 ] array[ 5 1 2 ] array[ 2 6 5 ]
array[ 6 7 2 ] array[ 2 3 7 ] array[ 3 7 4 ] array[ 3 0 4 ]
array[ 4 7 6 ] array[ 6 5 4 ] array[ 0 1 2 ] array[ 2 3 0 ] 12 >array 2 >array
9 >array 2 >array constant *dlocsig-speaker-config*

: make-speakers { chans d3? gen -- }
    d3? chans 4 < and if false to d3? then
    chans d3? if 1 else 0 then *dlocsig-speaker-config* array@ array@ arrange-speakers { sp }
    sp             gen dl-speaker-config !
    sp sp-number @ gen sp-number !
    sp sp-coords @ gen sp-coords !
    sp sp-groups @ gen sp-groups !
;

: ?same { f: a0 f: b0 f: c0 f: a1 f: b1 f: c1 -- f } a0 a1 f= b0 b1 f= and c0 c1 f= and ;
: nearest-point { f: x0 f: y0 f: z0 f: x1 f: y1 f: z1 f: px f: py f: pz -- f: x f: y f: z }
    x0 y0 z0 px py pz ?same if
	x0 y0 z0
    else
	x1 y1 z1 px py pz ?same if
	    x1 y1 z1
	else
	    x0 y0 z0 x1 y1 z1 ?same if
		x0 y0 z0
	    else
		x1 x0 f- { f: xm0 }
		y1 y0 f- { f: ym0 }
		z1 z0 f- { f: zm0 }
		px x0 f- { f: xm1 }
		py y0 f- { f: ym1 }
		pz z0 f- { f: zm1 }
		xm0 ym0 zm0 dl-distance { f: d0 }
		xm1 ym1 zm1 dl-distance { f: d1 }
		xm0 xm1 f* ym0 ym1 f* f+ zm0 zm1 f* f+ d0 d1 f* f/ d1 f* d0 f/ { f: ratio }
		x0 xm0 ratio f* f+ ( x )
		y0 ym0 ratio f* f+ ( y )
		z0 zm0 ratio f* f+ ( z )
	    then
	then
    then
;
: fdist>samples ( f: dist -- n ) mus-srate@ f* *dlocsig-speed-of-sound* f/ ;
: dist>samples ( f: dist -- r ) fdist>samples fround f>s ;
: dist>seconds ( f: dist -- r ) *dlocsig-speed-of-sound* f/ ;

: tr3-cross { v1 v2 -- v3 }
    v1 vct-second v2 vct-third  f* v1 vct-third  v2 vct-second f* f-
    v1 vct-third  v2 vct-first  f* v1 vct-first  v2 vct-third  f* f-
    v1 vct-first  v2 vct-second f* v1 vct-second v2 vct-first  f* f- 3 >vct
;
: transition-point-3 { vert-a vert-b f: xa f: ya f: za f: xb f: yb f: zb -- f r1 r2 r3 }
    xa ya za 3 >vct { line-b }
    xb yb zb 3 >vct { line-a }
    line-a line-b vct-subtract { line-m }
    vert-a vert-b tr3-cross { normal }
    normal line-m dot-product { f: den }
    den 0.000001e f< if
	false 0e 0e 0e
    else
	true line-b normal line-b dot-product fnegate den f/ line-m vct-scale vct-add vct>stack
    then
    line-a free-vct
    line-b free-vct
    line-m free-vct
    normal free-vct
;
: transition-point-2 { vert f: xa f: ya f: xb f: yb -- f r1 r2 }
    vert vct-first { f: ax }
    xa xb f- { f: bx }
    vert vct-second { f: ay }
    ya yb f- { f: by }
    xa fnegate { f: cx }
    ya fnegate { f: cy }
    by cx f* bx cy f* f- { f: dd }
    ay bx f* ax by f* f- { f: ff }
    ff f0= if
	false 0e 0e
    else
	true
	dd ax f* ff f/
	dd ay f* ff f/
    then
;
: calculate-gains { f: x f: y f: z group -- inside-flag gains-vct }
    1e-10 { f: zero-coord }
    group group-matrix @ { mat }
    mat ?matrix if
	x fabs zero-coord f< y fabs zero-coord f< and z fabs zero-coord f< and if
	    true vct[ 1e 1e 1e ]
	else
	    group group-size @ case
		3 of
		    0 0 mat matrix@ x f* 0 1 mat matrix@ y f* f+ 0 2 mat matrix@ z f* f+ { f: g-a }
		    1 0 mat matrix@ x f* 1 1 mat matrix@ y f* f+ 1 2 mat matrix@ z f* f+ { f: g-b }
		    2 0 mat matrix@ x f* 2 1 mat matrix@ y f* f+ 2 2 mat matrix@ z f* f+ { f: g-c }
		    g-a g-b g-c dl-distance { f: mag }
		    g-a fabs zero-coord f< if 0e to g-a then
		    g-b fabs zero-coord f< if 0e to g-b then
		    g-c fabs zero-coord f< if 0e to g-c then
		    g-a f0>= g-b f0>= and g-c f0>= and ( flag )
		    g-a mag f/ g-b mag f/ g-c mag f/ 3 >vct
		endof
		2 of
		    0 0 mat matrix@ x f* 0 1 mat matrix@ y f* f+ { f: g-a }
		    1 0 mat matrix@ x f* 1 1 mat matrix@ y f* f+ { f: g-b }
		    g-a g-b 0e dl-distance { f: mag }
		    g-a fabs zero-coord f< if 0e to g-a then
		    g-b fabs zero-coord f< if 0e to g-b then
		    g-a f0>= g-b f0>= and ( flag )
		    g-a mag f/ g-b mag f/ 2 >vct
		endof
		1 of
		    true vct[ 1e ]
		endof
	    endcase
	then
    else
	true vct[ 1e 1e 1e ]
    then
;
: find-group { f: x f: y f: z gen -- group gains }
    gen sp-groups @ { grps }
    nil nil
    grps each { group }
	x y z group calculate-gains { gains } if 2drop group gains leave then
    end-each
;
: push-zero-gains { f: time gen -- }
    gen dl-out-channels @ 0 ?do
	time i gen dl-out-chan-gains @ array@ vct-push!
	0e   i gen dl-out-chan-gains @ array@ vct-push!
    loop
    gen dl-rev-channels @ 0 ?do
	time i gen dl-rev-chan-gains @ array@ vct-push!
	0e   i gen dl-rev-chan-gains @ array@ vct-push!
    loop
;
: push-gains { group gains f: dist f: time gen -- }
    gen dl-out-channels @ make-vct { outputs }
    gen dl-rev-channels @ make-vct { revputs }
    0e 0e { f: att f: ratt }
    dist 1e f>= if
	dist gen dl-output-power sf@ f** 1/f to att
	dist gen dl-reverb-power sf@ f** 1/f to ratt
	group group-speakers @ { speakers }
	speakers each { speaker }
	    i gains vct@ att f* speaker outputs vct!
	    gen dl-rev-channels @ 1 > if i gains vct@ ratt f* speaker revputs vct! then
	end-each
    else
	1e dist gen dl-output-power sf@ 1/f f** f- to att
	1e dist gen dl-reverb-power sf@ 1/f f** f- to ratt
	group group-speakers @ { speakers }
	gen sp-number @ 0 ?do
	    i speakers array-index { found }
	    found 0>= if
		found gains vct@ 1e fover f- att f* f+ i outputs vct!
		gen dl-rev-channels @ 1 > if
		    found gains vct@ 1e fover f- ratt f* f+ i revputs vct!
		then
	    else
		att i outputs vct!
		gen dl-rev-channels @ 1 > if ratt i revputs vct! then
	    then
	loop
    then
    outputs length 0 ?do
	time           i gen dl-out-chan-gains @ array@ vct-push!
	i outputs vct@ i gen dl-out-chan-gains @ array@ vct-push!
    loop
    gen dl-rev-channels @ 1 = if
	time 0 gen dl-rev-chan-gains @ array@ vct-push!
	ratt 0 gen dl-rev-chan-gains @ array@ vct-push!
    else
	gen dl-rev-channels @ 1 > if
	    revputs length 0 ?do
		time           i gen dl-rev-chan-gains @ array@ vct-push!
		i revputs vct@ i gen dl-rev-chan-gains @ array@ vct-push!
	    loop
	then
    then
;

$" dl-amplitude-panning: " constant str-dl-amplitude-panning
str-dl-amplitude-panning $" supersonic radial movement" $+ constant str-sra
str-dl-amplitude-panning $" outside of both adjacent groups" $+ constant str-outside
str-dl-amplitude-panning $" current time <= previous time" $+ constant str-time-mismatch
str-dl-amplitude-panning $" crossing between groups only one point in common" $+ constant str-1-pnt
str-dl-amplitude-panning $" crossing between groups with no common points" $+ constant str-no-points

: dl-amplitude-panning { f: x f: y f: z f: dist f: time gen -- }
    gen dl-prev-group @ if
	\ if previous group
	time gen dl-prev-time sf@ f<>
	dist gen dl-prev-dist sf@ ftuck f- time frot f- f/ gen dl-speed-limit sf@ f> and if
	    str-sra dl-warn
	then
	x y z gen dl-prev-group @ calculate-gains { gains } if
	    \ if inside
	    gen dl-prev-group @ gains dist time gen push-gains
	    x gen dl-prev-x sf!
	    y gen dl-prev-y sf!
	    z gen dl-prev-z sf!
	else
	    \ else not inside
	    x y z gen find-group to gains { group }
	    group if
		\ if group
		group group-vertices @ gen dl-prev-group @ group-vertices @ array-and { edge }
		edge length case
		    2 of
			edge array-first edge array-second
			x y z gen dl-prev-x sf@ gen dl-prev-y sf@ gen dl-prev-z sf@
			transition-point-3 { f: xi f: yi f: zi } if
			    xi yi zi dl-distance { f: di }
			    gen dl-prev-time sf@
			    xi gen dl-prev-x sf@ f-
			    yi gen dl-prev-y sf@ f-
			    zi gen dl-prev-z sf@ f- dl-distance
			    x gen dl-prev-x sf@ f-
			    y gen dl-prev-y sf@ f-
			    z gen dl-prev-z sf@ f- dl-distance f/
			    time gen dl-prev-time sf@ f- f* f+ { f: ti }
			    gen dl-prev-time sf@ ti f< if
				xi yi zi gen dl-prev-group @ calculate-gains to gains if
				    gen dl-prev-group @ gains di ti gen push-gains
				else
				    xi yi zi group calculate-gains to gains if
					group gains di ti gen push-gains
				    else
					str-outside error
				    then
				then
			    else
				str-time-mismatch dl-warn
			    then
			then
		    endof
		    1 of
			group group-size @ 2 = if
			    edge array-first x y
			    gen dl-prev-x sf@
			    gen dl-prev-y sf@ transition-point-2 { f: xi f: yi } if
				xi yi 0e dl-distance { f: di }
				gen dl-prev-time sf@
				xi gen dl-prev-x sf@ f-
				yi gen dl-prev-y sf@ f-
				0e dl-distance
				x gen dl-prev-x sf@ f-
				y gen dl-prev-y sf@ f-
				0e dl-distance f/
				time gen dl-prev-time sf@ f- f* f+ { f: ti }
				gen dl-prev-time sf@ ti f< if
				    xi yi 0e gen dl-prev-group @ calculate-gains to gains if
					gen dl-prev-group @ gains di ti gen push-gains
					xi yi 0e group calculate-gains to gains if
					    group gains di ti gen push-gains
					else
					    str-outside error
					then
				    then
				else
				    str-time-mismatch dl-warn
				then
			    then
			else
			    str-1-pnt dl-warn
			then
		    endof
		    0 of
			str-no-points dl-warn
		    endof
		endcase
		group gains dist time gen push-gains
		group gen dl-prev-group !
		x     gen dl-prev-x sf!
		y     gen dl-prev-y sf!
		z     gen dl-prev-z sf!
	    else
		\ else not group
		time gen push-zero-gains
		false gen dl-prev-group !
	    then
	then
    else
	\ else not previous group
	x y z gen find-group { group gains }
	group if
	    group gains dist time gen push-gains
	    group gen dl-prev-group !
	    x     gen dl-prev-x sf!
	    y     gen dl-prev-y sf!
	    z     gen dl-prev-z sf!
	else
	    time gen push-zero-gains
	    false gen dl-prev-group !
	then
    then
    time gen dl-prev-time sf!
    dist gen dl-prev-dist sf!
;
: dl-b-format-ambisonics { f: x f: y f: z f: dist f: time gen -- }
    gen dl-out-chan-gains @ { out-gains }
    gen dl-rev-chan-gains @ { rev-gains }
    dist 1e f> if
	dist 1/f gen dl-output-power sf@ f** { f: att }
	time                       0 out-gains array@ vct-push!
	*dlocsig-point-707* att f* 0 out-gains array@ vct-push!
	time                       1 out-gains array@ vct-push!
	y dist f/ att f*           1 out-gains array@ vct-push!
	time                       2 out-gains array@ vct-push!
	x fnegate dist f/ att f*   2 out-gains array@ vct-push!
	time                       3 out-gains array@ vct-push!
	z dist f/ att f*           3 out-gains array@ vct-push!
	gen dl-rev-channels @ 1 = if
	    time                                 0 rev-gains array@ vct-push!
	    dist gen dl-reverb-power sf@ f** 1/f 0 rev-gains array@ vct-push!
	else
	    gen dl-rev-channels @ 4 = if
		dist 1/f gen dl-reverb-power sf@ f** { f: ratt }
		time                        0 rev-gains array@ vct-push!
		*dlocsig-point-707* ratt f* 0 rev-gains array@ vct-push!
		time                        1 rev-gains array@ vct-push!
		y dist f/ ratt f*           1 rev-gains array@ vct-push!
		time                        2 rev-gains array@ vct-push!
		x fnegate dist f/ ratt f*   2 rev-gains array@ vct-push!
		time                        3 rev-gains array@ vct-push!
		z dist f/ ratt f*           3 rev-gains array@ vct-push!
	    then
	then
    else
	dist f0= if
	    time 0 out-gains array@ vct-push!
	    1e   0 out-gains array@ vct-push!
	    4 1 do
		time i out-gains array@ vct-push!
		0e   i out-gains array@ vct-push!
	    loop
	    gen dl-rev-channels @ 1 >= if
		time 0 rev-gains array@ vct-push!
		1e   0 rev-gains array@ vct-push!
	    then
	    gen dl-rev-channels @ 4 = if
		4 1 do
		    time i rev-gains array@ vct-push!
		    0e   i rev-gains array@ vct-push!
		loop
	    then
	else
	    dist gen dl-output-power sf@ 1/f f** { f: att }
	    time                     0 out-gains array@ vct-push!
	    1e 1e *dlocsig-point-707* f- dist gen dl-output-power sf@ f** f* f-
	    0 out-gains array@ vct-push!
	    time                     1 out-gains array@ vct-push!
	    y dist f/ att f*         1 out-gains array@ vct-push!
	    time                     2 out-gains array@ vct-push!
	    x fnegate dist f/ att f* 2 out-gains array@ vct-push!
	    time                     3 out-gains array@ vct-push!
	    z dist f/ att f*         3 out-gains array@ vct-push!
	    gen dl-rev-channels @ 1 = if
		time                            0 rev-gains array@ vct-push!
		1e dist gen dl-reverb-power sf@ 1/f f** f- 0 rev-gains array@ vct-push!
	    else
		gen dl-rev-channels @ 4 = if
		    dist gen dl-reverb-power sf@ 1/f f** { f: ratt }
		    time                      0 rev-gains array@ vct-push!
		    1e 1e *dlocsig-point-707* f- dist gen dl-reverb-power sf@ f** f* f-
		    0 rev-gains array@ vct-push!
		    time                      1 rev-gains array@ vct-push!
		    y dist f/ ratt f*         1 rev-gains array@ vct-push!
		    time                      2 rev-gains array@ vct-push!
		    x fnegate dist f/ ratt f* 2 rev-gains array@ vct-push!
		    time                      3 rev-gains array@ vct-push!
		    z dist f/ ratt f*         3 rev-gains array@ vct-push!
		then
	    then
	then
    then
;
: dl-decoded-ambisonics { f: x f: y f: z f: dist f: time gen -- }
    gen dl-out-chan-gains @ { out-gains }
    gen dl-rev-chan-gains @ { rev-gains }
    gen sp-coords @ { coords }
    nil { cs }
    dist 1e f> if
	dist 1/f gen dl-output-power sf@ f** { f: att }
	*dlocsig-point-707* fdup f* att f* { f: attw }
	x dist f/ att f* { f: attx }
	y dist f/ att f* { f: atty }
	z dist f/ att f* { f: attz }
	coords each to cs
	    time i out-gains array@ vct-push!
	    attw attx cs vct-first f* f+
	    atty cs vct-second f* f+
	    attz cs vct-third f* f+
	    *dlocsig-point-707* f* i out-gains array@ vct-push!
	end-each
	gen dl-rev-channels @ if
	    gen dl-rev-channels @ 1 = if
		time rev-gains array-first vct-push!
		dist gen dl-reverb-power sf@ f** 1/f rev-gains array-first vct-push!
	    else
		dist 1/f gen dl-reverb-power sf@ f** { f: ratt }
		*dlocsig-point-707* fdup f* ratt f* { f: rattw }
		x dist f/ ratt f* { f: rattx }
		y dist f/ ratt f* { f: ratty }
		z dist f/ ratt f* { f: rattz }
		gen dl-rev-channels @ 0 ?do
		    i coords array@ to cs
		    time i rev-gains array@ vct-push!
		    rattw rattx cs vct-first f* f+
		    ratty cs vct-second f* f+
		    rattz cs vct-third f* f+
		    *dlocsig-point-707* f* i rev-gains array@ vct-push!
		loop
	    then
	then
    else
	dist f0= if
	    *dlocsig-point-707* fdup f* { f: att }
	    coords length 0 ?do
		time i out-gains array@ vct-push!
		att  i out-gains array@ vct-push!
	    loop
	    gen dl-rev-channels @ 1 = if
		time 0 rev-gains array@ vct-push!
		1e   0 rev-gains array@ vct-push!
	    else
		gen dl-rev-channels @ 0 ?do
		    time i rev-gains array@ vct-push!
		    att  i rev-gains array@ vct-push!
		loop
	    then
	else
	    dist gen dl-output-power sf@ 1/f f** { f: att }
	    1e 1e *dlocsig-point-707* f- dist gen dl-output-power sf@ f** f* f-
	    *dlocsig-point-707* f* { f: attw }
	    x dist f/ att f* { f: attx }
	    y dist f/ att f* { f: atty }
	    z dist f/ att f* { f: attz }
	    coords each to cs
		time i out-gains array@ vct-push!
		attw attx cs vct-first f* f+
		atty cs vct-second f* f+
		attz cs vct-third f* f+
		*dlocsig-point-707* f* i out-gains array@ vct-push!
	    end-each
	    gen dl-rev-channels @ if
		gen dl-rev-channels @ 1 = if
		    time rev-gains array-first vct-push!
		    1e dist gen dl-reverb-power sf@ 1/f f** f- rev-gains array-first vct-push!
		else
		    dist gen dl-reverb-power sf@ 1/f f** { f: ratt }
		    1e 1e *dlocsig-point-707* f- dist gen dl-reverb-power sf@ f** f* f-
		    *dlocsig-point-707* f* { f: rattw }
		    x dist f/ ratt f* { f: rattx }
		    y dist f/ ratt f* { f: ratty }
		    z dist f/ ratt f* { f: rattz }
		    gen dl-rev-channels @ 0 ?do
			i coords array@ to cs
			time i rev-gains array@ vct-push!
			rattw rattx cs vct-first f* f+
			ratty cs vct-second f* f+
			rattz cs vct-third f* f+
			*dlocsig-point-707* f* i rev-gains array@ vct-push!
		    loop
		then
	    then
	then
    then
;

: walk-all-rooms  { f: xa f: ya f: za f: ta gen -- }
    xa ya za dl-distance { f: dist }
    gen dl-first-dist sf@ f0= if dist gen dl-first-dist sf! then
    dist gen dl-last-dist sf!
    gen dl-min-dist sf@ fdup f0= dist f> or if dist gen dl-min-dist sf! then
    gen dl-max-dist sf@ fdup f0= dist f< or if dist gen dl-max-dist sf! then
    ta                 gen dl-delay @ vct-push!
    dist fdist>samples gen dl-delay @ vct-push!
    gen dl-render-using @ case
	amplitude-panning   of xa ya za dist ta gen dl-amplitude-panning   endof
	b-format-ambisonics of xa ya za dist ta gen dl-b-format-ambisonics endof
	decoded-ambisonics  of xa ya za dist ta gen dl-decoded-ambisonics  endof
    endcase
;
: change-direction { f: xa f: ya f: za f: ta f: xb f: yb f: zb f: tb gen -- }
    xa ya za ta gen walk-all-rooms
    xa xb f<> ya yb f<> or za zb f<> or ta tb f<> or if
	xa ya za xb yb zb 0e 0e 0e nearest-point { f: xi f: yi f: zi }
	xa xb f< if xa xi f<= xi xb f<= and else xb xi f<= xi xa f<= and then
	ya yb f< if ya yi f<= yi yb f<= and else yb yi f<= yi ya f<= and then and
	za zb f< if za zi f<= zi zb f<= and else zb zi f<= zi za f<= and then and if
	    xi yi zi
	    xb xi f- yb yi f- zb zi f- dl-distance
	    xb xa f- yb ya f- zb za f- dl-distance f/ ta tb f- f* tb f+ gen walk-all-rooms
	then
    then
;
: intersects-inside-radius { f: xa f: ya f: za f: ta f: xb f: yb f: zb f: tb gen -- }
    xb xa f- yb ya f- zb za f- dl-distance { f: mag }
    xb xa f- mag f/ { f: vx }
    yb ya f- mag f/ { f: vy }
    zb za f- mag f/ { f: vz }
    xa vx f* ya vy f* f+ za vz f* f+ { f: bsq }
    bsq fdup f* xa fdup f* ya fdup f* f+ za fdup f* f+ 1e f- f- { f: disc }
    disc f0>= if
	disc fsqrt
	bsq fnegate fover f- { f: rin }
	bsq fnegate fswap f+ { f: rout }
	false false { in? out? }
	0e 0e 0e 0e 0e 0e 0e 0e { f: xi f: yi f: zi f: ti f: xo f: yo f: zo f: tto }
	rin f0> rin mag f< and if
	    true to in?
	    xa vx rin f* f+ to xi
	    ya vy rin f* f+ to yi
	    za vz rin f* f+ to zi
	    xb xi f- yb yi f- zb zi f- dl-distance
	    xb xa f- yb ya f- zb za f- dl-distance f/ ta tb f- f* tb f+ to ti
	then
	rout f0> rout fabs mag f< and if
	    true to out?
	    xa vx rout f* f+ to xo
	    ya vy rout f* f+ to yo
	    za vz rout f* f+ to zo
	    xb xo f- yb yo f- zb zo f- dl-distance
	    xb xa f- yb ya f- zb za f- dl-distance f/ ta tb f- f* tb f+ to tto
	then
	in? if
	    xa ya za ta xi yi zi ti gen change-direction
	    out? if
		xi yi zi ti xo yo zo tto gen change-direction
		xo yo zo tto xb yb zb tb gen change-direction
	    else
		xi yi zi ti xb yb zb tb gen change-direction
	    then
	else
	    out? if
		xa ya za ta xb yb zb tb gen change-direction
		xo yo zo tto xb yb zb tb gen change-direction
	    else
		xa ya za ta xb yb zb tb gen change-direction
	    then
	then
    else
	xa ya za ta xb yb zb tb gen change-direction
    then
;
: minimum-segment-length recursive { f: xa f: ya f: za f: ta  f: xb f: yb f: zb f: tb gen -- }
    xb xa f- yb ya f- zb za f- dl-distance 1e f< if
	xa ya za ta xb yb zb tb gen intersects-inside-radius
    else
	xa xb f+ f2/ { f: xi }
	ya yb f+ f2/ { f: yi }
	za zb f+ f2/ { f: zi }
	xb xi f- yb yi f- zb zi f- dl-distance
	xb xa f- yb ya f- zb za f- dl-distance f/ ta tb f- f* tb f+ { f: ti }
	xa ya za ta xi yi zi ti gen minimum-segment-length
	xi yi zi ti xb yb zb tb gen minimum-segment-length
    then
;

\ === Path ===
gfm-base%
    cell%   field path-bezier-path
    cell%   field path-open-bezier-path
    cell%   field path-closed-bezier-path
    cell%   field path-literal-path
    cell%   field path-spiral-path
    cell%   field path-type
    cell%   field path-path
    cell%   field path-3d
    cell%   field path-polar
    cell%   field path-initial-direction
    cell%   field path-final-direction
    cell%   field path-x
    cell%   field path-y
    cell%   field path-z
    cell%   field path-v
    cell%   field path-bx
    cell%   field path-by
    cell%   field path-bz
    cell%   field path-rx
    cell%   field path-ry
    cell%   field path-rz
    cell%   field path-rv
    cell%   field path-rt
    cell%   field path-tx
    cell%   field path-ty
    cell%   field path-tz
    cell%   field path-tt
    sfloat% field path-error
    sfloat% field path-start-angle
    sfloat% field path-turns
end-struct path%
$" Path" constant str-path

str-path make-?obj ?path
: path-free { gen -- }
    gen ?path if
	gen path-x @ gen-free
	gen path-y @ gen-free
	gen path-z @ gen-free
	gen path-v @ gen-free
	gen path-bx @ gen-free
	gen path-by @ gen-free
	gen path-bz @ gen-free
	gen path-rx @ gen-free
	gen path-ry @ gen-free
	gen path-rz @ gen-free
	gen path-rv @ gen-free
	gen path-rt @ gen-free
	nil gen gfm-base-name !
	gen free throw
    then
;
: $(.path) { gen -- str }
    $" #<" gen path-type @ $+
    $"  3d: " $+ gen path-3d @ true-false-string $+
    $" , polar: " $+ gen path-polar @ true-false-string $+
    $" , path: " $+ gen path-path @ gen>string $+
    $" , X: " $+ gen path-x @ gen>string $+
    $" , Y: " $+ gen path-y @ gen>string $+
    $" , Z: " $+ gen path-z @ gen>string $+
    $" , V: " $+ gen path-v @ gen>string $+
    $" , BX: " $+ gen path-bx @ gen>string $+
    $" , BY: " $+ gen path-by @ gen>string $+
    $" , BZ: " $+ gen path-bz @ gen>string $+
    $" >" $+
;
' $(.path) make-inspect .path

: make-base-path { -- path }
    path% %alloc { path }
    false         path path-bezier-path !
    false         path path-open-bezier-path !
    false         path path-closed-bezier-path !
    false         path path-literal-path !
    false         path path-spiral-path !
    nil           path path-path !
    0e            path path-start-angle sf!
    0e            path path-turns sf!
    nil   	  path path-x !
    nil   	  path path-y !
    nil   	  path path-z !
    nil   	  path path-v !
    nil   	  path path-bx !
    nil   	  path path-by !
    nil   	  path path-bz !
    nil           path path-rx !
    nil           path path-ry !
    nil           path path-rz !
    nil           path path-rv !
    nil           path path-rt !
    nil           path path-tx !
    nil           path path-ty !
    nil           path path-tz !
    nil           path path-tt !
    str-path      path path-type !
    ['] path-free path gfm-base-free !
    ['] .path     path gfm-base-inspect !
    ['] $(.path)  path gfm-base-to-string !
    str-path      path gfm-base-name !
    path
;

: reset-transformation { path -- }
    path path-tx @ gen-free
    path path-ty @ gen-free
    path path-tz @ gen-free
    path path-tt @ gen-free
    nil path path-tx !
    nil path path-ty !
    nil path path-tz !
    nil path path-tt !
;
: reset-rendering { path -- }
    path path-rx @ gen-free
    path path-ry @ gen-free
    path path-rz @ gen-free
    path path-rt @ gen-free
    path path-rv @ gen-free
    nil path path-rx !
    nil path path-ry !
    nil path path-rz !
    nil path path-rt !
    nil path path-rv !
    path reset-transformation
;
: parse-cartesian-coordinates { points 3d -- x y z v }
    points length { size }
    size make-vct { x }
    size make-vct { y }
    size make-vct { z }
    size make-vct { v }
    points ?array if
	size 0 ?do
	    i points array@ { vvc }
	    vvc length 4 = if
		vvc vct-copy
	    else
		4 make-vct { vv }
		vvc each i vv vct! end-each
		vv
	    then { vc }
	    vc vct-first  i x vct!
	    vc vct-second i y vct!
	    vc vct-third  i z vct!
	    3 vc vct@     i v vct!
	    vc gen-free
	loop
    else
	3d if
	    size 3 - 0 ?do
		i     points vct@ i x vct!
		i 1+  points vct@ i y vct!
		i 2 + points vct@ i z vct!
	    3 +loop
	else
	    size 2 - 0 ?do
		i     points vct@ i x vct!
		i 1+  points vct@ i y vct!
	    2 +loop
	then
    then
    x y z v
;
: parse-polar-coordinates { points 3d -- x y z v }
    points length { size }
    size make-vct { x }
    size make-vct { y }
    size make-vct { z }
    size make-vct { v }
    points ?array if
	points each { vvc }
	    vvc length 4 = if
		vvc vct-copy
	    else
		4 make-vct { vv }
		vvc each i vv vct! end-each
		vv
	    then { vc }
	    vc vct-first { f: d }
	    vc vct-second *dlocsig-one-turn* f/ two-pi f* cis { avec }
	    vc vct-third  *dlocsig-one-turn* f/ two-pi f* cis { evec }
	    d evec real@ f* { f: dxy }
	    dxy avec image@ f* i x vct!
	    dxy avec real@  f* i y vct!
	    d evec image@   f* i z vct!
	    3d if 3 else 2 then vc vct@ i v vct!
	    avec gen-free
	    evec gen-free
	    vc gen-free
	end-each
    else
	3d if
	    size 3 - 0 ?do
		i     points vct@ { f: d }
		i 1+  points vct@ *dlocsig-one-turn* f/ two-pi f* cis { avec }
		i 2 + points vct@ *dlocsig-one-turn* f/ two-pi f* cis { evec }
		d evec real@ f* { f: dxy }
		dxy avec image@ f* i x vct!
		dxy avec real@  f* i y vct!
		d evec image@   f* i z vct!
		avec gen-free
		evec gen-free
	    3 +loop
	else
	    size 2 - 0 ?do
		i     points vct@ { f: d }
		i 1+  points vct@ *dlocsig-one-turn* f/ two-pi f* cis { avec }
		d avec image@ f* i x vct!
		d avec real@  f* i y vct!
		avec gen-free
	    3 +loop
	then
    then
    x y z v
;

\ === Bezier-path ===
$" Bezier-path" constant str-bezier-path
str-bezier-path make-?obj ?bezier-path

: make-bezier-path { ary -- path }
    assert1( ary ?array ary ?vct or )
    make-base-path { path }
    ary   	    path path-path !
    true  	    path path-bezier-path !
    true  	    path path-3d !
    false 	    path path-polar !
    0.01e 	    path path-error sf!
    str-bezier-path path path-type !
    path
;
: parse-path { gen -- }
    gen path-polar @ if
	gen path-path @ gen path-3d @ parse-polar-coordinates
    else
	gen path-path @ gen path-3d @ parse-cartesian-coordinates
    then gen path-v ! gen path-z ! gen path-y ! gen path-x !
    gen path-v @ { v }
    v vct-min 0e f< if
	v vct-min f0< if $" velocities must be all positive, corrected" dl-warn then
	v map i v vct@ 0e fmax end-map drop
    then
    nil gen path-bx !
    nil gen path-by !
    nil gen path-bz !
    gen reset-rendering
;

vct[ 0.25e ]
vct[ 0.2667e -0.06667e ]
vct[ 0.2678e -0.0714e 0.0178e ]
vct[ 0.2679e -0.0718e 0.0191e -0.0047e ]
vct[ 0.2679e -0.0718e 0.0192e -0.0051e 0.0013e ]
vct[ 0.2679e -0.0718e 0.0192e -0.0052e 0.0014e -0.0003e ]
vct[ 0.2679e -0.0718e 0.0192e -0.0052e 0.0014e -0.0004e 9.2047e-05 ]
7 >array constant *dlocsig-path-a-even*
vct[ 0.3333e ]
vct[ 0.2727e -0.0909e ]
vct[ 0.2683e -0.0732e 0.0244e ]
vct[ 0.2680e -0.0719e 0.0196e -0.0065e ]
vct[ 0.2679e -0.0718e 0.0193e -0.0052e 0.0018e ]
vct[ 0.2679e -0.0718e 0.0192e -0.0052e 0.0014e -0.0005e ]
vct[ 0.2679e -0.0718e 0.0192e -0.0052e 0.0014e -0.0004e 0.0001e ]
7 >array constant *dlocsig-path-a-odd*

: ac { k n -- r } k 1- n *dlocsig-path-maxcoeff* min 2 - *dlocsig-path-a-even* array@ vct@ ;
: ref { zz jj ii dd nn -- r }
    ii nn > if
	 ii nn - jj zz array@ vct@
    else
	ii 0< if
	    ii nn + jj zz array@ vct@
	else
	    ii nn = if
		nn jj zz array@ vct@ jj nn dd matrix@ f-
	    else
		ii unless
		    0 jj zz array@ vct@ jj 0 dd matrix@ f+
		else
		    ii jj zz array@ vct@
		then
	    then
	then
    then
;
: open-bezier-calculate-fit { gen -- n p-ary d-mat }
    gen path-x @ length 1- { n }
    n 1- { m }
    gen path-x @ gen path-y @ gen path-z @ 3 >array { p }
    3 make-array map n 1+ make-vct end-map { d }
    d 3 array>matrix to d
    gen path-initial-direction @ { init }
    gen path-final-direction @   { fin }
    init ?vct if
	0 init vct@ 0 0 d matrix!
	1 init vct@ 1 0 d matrix!
	2 init vct@ 2 0 d matrix!
    else
	0e 0 0 d matrix!
	0e 1 0 d matrix!
	0e 2 0 d matrix!
    then
    fin ?vct if
	0 fin vct@ 0 n d matrix!
	1 fin vct@ 1 n d matrix!
	2 fin vct@ 2 n d matrix!
    else
	0e 0 n d matrix!
	0e 1 n d matrix!
	0e 2 n d matrix!
    then
    n 1 ?do
	*dlocsig-path-maxcoeff* 1- m min 1+ 1 ?do
	    3 0 do
		j n ac p i k j + d n ref p i k j - d n ref  f- f* i k d matrix+!
	    loop
	loop
    loop
    n p d
;
: open-bezier-fit-path { gen -- }
    gen path-x @ unless gen parse-path then
    gen path-x @ length case
	1 of
	    nil gen path-bx !
	    nil gen path-by !
	    nil gen path-bz !
	endof
	2 of
	    1 make-array { bx }
	    gen path-x @ vct-first fdup 
	    gen path-x @ vct-second fdup 4 >vct 0 bx array!
	    bx gen path-bx !
	    1 make-array { by }
	    gen path-y @ vct-first fdup
	    gen path-y @ vct-second fdup 4 >vct 0 by array!
	    by gen path-by !
	    1 make-array { bz }
	    gen path-z @ vct-first fdup
	    gen path-z @ vct-second fdup 4 >vct 0 bz array!
	    bz gen path-bz !
	endof
	\ default
	gen open-bezier-calculate-fit { n p d }
	n make-array map 1e 1e 2 >vct end-map { cs }
	n make-array map
	    i    0 p array@ vct@
	    i    0 p array@ vct@ 0 i    d matrix@ 0 i cs array@ vct@ f* f+
	    i 1+ 0 p array@ vct@ 0 i 1+ d matrix@ 1 i cs array@ vct@ f* f-
	    i 1+ 0 p array@ vct@ 4 >vct
	end-map { bx }
	bx gen path-bx !
	n make-array map
	    i    1 p array@ vct@
	    i    1 p array@ vct@ 1 i    d matrix@ 0 i cs array@ vct@ f* f+
	    i 1+ 1 p array@ vct@ 1 i 1+ d matrix@ 1 i cs array@ vct@ f* f-
	    i 1+ 1 p array@ vct@ 4 >vct
	end-map { by }
	by gen path-by !
	n make-array map
	    i    2 p array@ vct@
	    i    2 p array@ vct@ 2 i    d matrix@ 0 i cs array@ vct@ f* f+
	    i 1+ 2 p array@ vct@ 2 i 1+ d matrix@ 1 i cs array@ vct@ f* f-
	    i 1+ 2 p array@ vct@ 4 >vct
	end-map { bz }
	bz gen path-bz !
    endcase
    gen reset-rendering
;
: bezier-point { f: u c -- r1 r2 r3 }
    1e u f- { f: u1 }
    3 make-array map
	3 make-vct map i j c array@ vct@ u1 f* i 1+ j c array@ vct@ u f* f+ end-map
    end-map { crr }
    -1 1 -do
	i 1+ 0 ?do
	    3 0 do
		j i crr array@ vct@ u1 f* j 1+ i crr array@ vct@ u f* f+ j i crr array@ vct!
	    loop
	loop
    1 -loop
    0 0 crr array@ vct@
    0 1 crr array@ vct@
    0 2 crr array@ vct@
;
: berny recursive { f: xl f: yl f: zl f: xh f: yh f: zh f: ul f: u f: uh c gen -- v1 v2 v3 }
    u c bezier-point { f: x f: y f: z }
    xl yl zl xh yh zh x y z nearest-point { f: xn f: yn f: zn }
    xn x f- yn y f- zn z f- dl-distance gen path-error sf@ f> if
	xl yl zl x y z ul ul u f+ f2/ u c gen berny { xi yi zi }
	x y z xh yh zh u u uh f+ f2/ uh c gen berny { xj yj zj }
	x xi vct-push!
	xj each xi vct-push! end-each
	y yi vct-push!
	yj each yi vct-push! end-each
	z zi vct-push!
	zj each zi vct-push! end-each
	xi yi zi
    else
	0 make-vct 0 make-vct 0 make-vct
    then
;

: acc { k n -- r }
    *dlocsig-path-maxcoeff* 2* 1+ n min 2 mod if
	k 1- n 3 - 2/ *dlocsig-path-a-odd* array@ vct@
    else
	k 1- n 4 - 2/ *dlocsig-path-a-even* array@ vct@
    then
;
: cref { zz jj ii nn -- r }
    ii nn 1- > if
	 ii nn - jj zz array@ vct@
    else
	ii 0< if
	    ii nn + jj zz array@ vct@
	else
	    ii jj zz array@ vct@
	then
    then
;

: closed-bezier-calculate-fit { gen -- n p-ary d-mat }
    gen path-x @ length 1- { n }
    n n 2 mod if 3 else 4 then - 2/ { m }
    gen path-x @ gen path-y @ gen path-z @ 3 >array { p }
    3 make-array map n 1+ make-vct end-map 3 array>matrix { d }
    n 0 ?do
	m 1 ?do
	    3 0 do
		j n acc  p i k j + n cref  p i k j - n cref  f- f*  i k d matrix+!
	    loop
	loop
    loop
    n 1- p d
;
: closed-bezier-fit-path { gen -- }
    gen path-x @ unless gen parse-path then
    nil nil nil { xc yc zc }
    gen path-x @ length 4 > if
	gen closed-bezier-calculate-fit { n p d }
	n make-array to xc
	n 1- 0 ?do
	    i 0 p array@ vct@
	    i 0 p array@ vct@ 0 i d matrix@ f+
	    i 1+ 0 p array@ vct@ 0 i 1+ d matrix@ f-
	    i 1+ 0 p array@ vct@ 4 >vct i xc array!
	loop
	n make-array to yc
	n 1- 0 ?do
	    i 1 p array@ vct@
	    i 1 p array@ vct@ 1 i d matrix@ f+
	    i 1+ 1 p array@ vct@ 1 i 1+ d matrix@ f-
	    i 1+ 1 p array@ vct@ 4 >vct i yc array!
	loop
	n make-array to zc
	n 1- 0 ?do
	    i 2 p array@ vct@
	    i 2 p array@ vct@ 2 i d matrix@ f+
	    i 1+ 2 p array@ vct@ 2 i 1+ d matrix@ f-
	    i 1+ 2 p array@ vct@ 4 >vct i zc array!
	loop
	n 0 p array@ vct@
	n 0 p array@ vct@ 0 n d matrix@ f+
	0 0 p array@ vct@ 0 0 d matrix@ f-
	0 0 p array@ vct@ 4 >vct n 1- xc array!
	n 1 p array@ vct@
	n 1 p array@ vct@ 1 n d matrix@ f+
	0 1 p array@ vct@ 1 0 d matrix@ f-
	0 1 p array@ vct@ 4 >vct n 1- yc array!
	n 2 p array@ vct@
	n 2 p array@ vct@ 2 n d matrix@ f+
	0 2 p array@ vct@ 2 0 d matrix@ f-
	0 2 p array@ vct@ 4 >vct n 1- zc array!
    else
	gen path-x @ length 1- { size }
	size make-array to xc
	size make-array to yc
	size make-array to zc
	size 0 ?do
	    i gen path-x @ vct@ fdup i 1+ gen path-x @ vct@ fdup 4 >vct i xc array!
	    i gen path-y @ vct@ fdup i 1+ gen path-y @ vct@ fdup 4 >vct i yc array!
	    i gen path-z @ vct@ fdup i 1+ gen path-z @ vct@ fdup 4 >vct i zc array!
	loop
    then
    xc gen path-bx !
    yc gen path-by !
    zc gen path-bz !
    gen reset-rendering
;

: fit-path { gen -- }
    gen path-open-bezier-path @ if
	gen open-bezier-fit-path
    else
	gen closed-bezier-fit-path
    then
;

: bezier-render-path { gen -- }
    gen path-bx @ unless gen fit-path then
    0 make-vct { rx }
    0 make-vct { ry }
    0 make-vct { rz }
    0 make-vct { rv }
    gen path-v @ { pv }
    pv vct-first f0= if
	1e 0 pv vct!
	1e -1 pv vct!
    then
    gen path-x @ length 1 = if
	gen path-x @ gen path-rx !
	gen path-y @ gen path-ry !
	gen path-z @ gen path-rz !
	0e 1 >vct    gen path-rt !
    else
	0e 0e 0e 0e { f: xf-bz f: yf-bz f: zf-bz f: vf-bz }
	pv length 1- gen path-bx @ length min 0 ?do
	    i gen path-bx @ array@ { x-bz }
	    i gen path-by @ array@ { y-bz }
	    i gen path-bz @ array@ { z-bz }
	    i    pv vct@ { f: vi-bz }
	    i 1+ pv vct@ to vf-bz
	    0  x-bz vct@ { f: xi-bz }
	    -1 x-bz vct@ to xf-bz
	    0  y-bz vct@ { f: yi-bz }
	    -1 y-bz vct@ to yf-bz
	    0  z-bz vct@ { f: zi-bz }
	    -1 z-bz vct@ to zf-bz
	    xi-bz yi-bz zi-bz xf-bz yf-bz zf-bz 0e 0.5e 1e x-bz y-bz z-bz 3 >array gen berny
	    { xs ys zs }
	    xi-bz rx vct-push! xs each rx vct-push! end-each
	    yi-bz ry vct-push! ys each ry vct-push! end-each
	    zi-bz rz vct-push! zs each rz vct-push! end-each
	    vi-bz rv vct-push! xs each fdrop 0e rv vct-push! end-each
	loop
	xf-bz rx vct-push!
	yf-bz ry vct-push!
	zf-bz rz vct-push!
	vf-bz rv vct-push!
	0 rx vct@ 1 >vct { xseg }
	0 ry vct@ 1 >vct { yseg }
	0 rz vct@ 1 >vct { zseg }
	0 rv vct@ 1 >vct { vseg }
	0 rv vct@ { f: vi }
	0e { f: ti }
	0e 1 >vct { times }
	rx length 1 ?do
	    i rx vct@ { f: x }
	    i ry vct@ { f: y }
	    i rz vct@ { f: z }
	    i rv vct@ { f: v }
	    x xseg vct-push!
	    y yseg vct-push!
	    z zseg vct-push!
	    v vseg vct-push!
	    v f0<> if
		0e
		xseg length 1- make-vct map
		    i 1+ xseg vct@ i xseg vct@ f-
		    i 1+ yseg vct@ i yseg vct@ f-
		    i 1+ zseg vct@ i zseg vct@ f- dl-distance f+ fdup
		end-map { dseg }
		fdrop
		v vi f- v vi f+ f* dseg vct-last 4e f* f/ { f: aa }
		dseg each
		    { f: d }
		    vi f0<> v vi f= and if
			d vi f/
		    else
			aa f0<> if
			    vi fdup f* 4e aa f* d f* f+ 0.5e f** vi f- aa f2* f/
			else
			    0e
			then
		    then
		    ti f+ fdup
		    i dseg vct!
		    times vct-push!
		end-each
		x 1 >vct to xseg
		y 1 >vct to yseg
		z 1 >vct to zseg
		v 1 >vct to vseg
		v to vi
		dseg vct-last to ti
	    then
	loop
	rx gen path-rx !
	ry gen path-ry !
	rz gen path-rz !
	times vct-last { f: tf }
	times map i times vct@ tf f/ end-map gen path-rt !
    then
    gen reset-transformation
;

: literal-render-path { gen -- }
    gen path-polar @ if
	gen path-path @ gen path-3d @ parse-polar-coordinates
    else
	gen path-path @ gen path-3d @ parse-cartesian-coordinates
    then gen path-rv ! gen path-rz ! gen path-ry ! gen path-rx !
    gen path-rv @ vct-first f0= if
	1e 0 gen path-rv @ vct!
	1e -1 gen path-rv @ vct!
    then
    gen path-rx @ length 1 = if
	0e 1 >vct 0 gen path-rt !
    else
	gen path-rx @ { rx }
	gen path-ry @ { ry }
	gen path-rz @ { rz }
	gen path-rv @ { rv }
	0 rx vct@ 1 >vct { xseg }
	0 ry vct@ 1 >vct { yseg }
	0 rz vct@ 1 >vct { zseg }
	0 rv vct@ 1 >vct { vseg }
	0 rv vct@ { f: vi }
	0e { f: ti }
	0e 1 >vct { times }
	rx length 1 ?do
	    i rx vct@ { f: x }
	    i ry vct@ { f: y }
	    i rz vct@ { f: z }
	    i rv vct@ { f: v }
	    x xseg vct-push!
	    y yseg vct-push!
	    z zseg vct-push!
	    v vseg vct-push!
	    v f0<> if
		0e
		xseg length 1- make-vct map
		    i 1+ xseg vct@ i xseg vct@ f-
		    i 1+ yseg vct@ i yseg vct@ f-
		    i 1+ zseg vct@ i zseg vct@ f- dl-distance f+ fdup
		end-map { dseg }
		fdrop
		v vi f- v vi f+ f* dseg vct-last 4e f* f/ { f: aa }
		dseg each
		    { f: d }
		    vi f0<> v vi f= and if
			d vi f/
		    else
			aa f0<> if
			    vi fdup f* 4e aa f* d f* f+ 0.5e f** vi f- aa f2* f/
			else
			    0e
			then
		    then
		    ti f+ fdup
		    i dseg vct!
		    times vct-push!
		end-each
		x 1 >vct to xseg
		y 1 >vct to yseg
		z 1 >vct to zseg
		v 1 >vct to vseg
		v to vi
		dseg vct-last to ti
	    then
	loop
	times vct-last { f: tf }
	times map i times vct@ tf f/ end-map gen path-rt !
    then
    gen reset-transformation
;

: spiral-render-path { gen -- }
    gen path-start-angle sf@ gen path-turns sf@ f/ two-pi f* { f: start }
    gen path-turns sf@ f0= if two-pi else gen path-turns sf@ two-pi f* then { f: total }
    *dlocsig-one-turn* 100e f/ { f: step-angle }
    total step-angle *dlocsig-one-turn* f/ two-pi f* f/ fabs { f: steps }
    total steps fceil step-angle f0< if -1e else 1e then f* f/ { f: step }
    total step f/ fround fabs f>s { size }
    size make-vct { x }
    size make-vct { z }
    start
    size make-vct map
	fdup ( start ) cis { xy }
	step f+ ( start += step )
	10e xy image@  f* i x vct!
	10e xy real@   f* ( i y vct! )
	xy gen-free
    end-map { y }
    fdrop
    0e
    size 1- make-vct map
	i 1+ x vct@ i x vct@ f-
	i 1+ y vct@ i y vct@ f-
	i 1+ z vct@ i z vct@ f- dl-distance f+ fdup
    end-map { dp }
    fdrop
    0e
    dp length 1- make-vct map i 1+ dp vct@ i dp vct@ f- 4e f/ f+ fdup end-map { times }
    fdrop
    x gen path-rx !
    y gen path-ry !
    z gen path-rz !
    times vct-last { f: tf }
    times map i times vct@ tf f/ end-map gen path-rt !
    gen reset-transformation
;

\ === Open-bezier-path ===
$" Open-Bezier-Path" constant str-open-bezier-path
str-open-bezier-path make-?obj ?open-bezier-path

: make-open-bezier-path { ary d3 polar f: err init fin -- path }
    ary make-bezier-path { path }
    init ?vct if false else vct[ 0e 0e 0e ] to init true then { del-init }
    fin  ?vct if false else vct[ 0e 0e 0e ] to fin  true then { del-fin }
    true          	 path path-open-bezier-path !
    d3    	  	 path path-3d !
    polar 	  	 path path-polar !
    err   	  	 path path-error sf!
    init vct-copy 	 path path-initial-direction !
    fin  vct-copy 	 path path-final-direction !
    str-open-bezier-path path path-type !
    del-init if init gen-free then
    del-fin  if fin  gen-free then
    path
;

\ === Closed-bezier-path ===
$" Closed-Bezier-Path" constant str-closed-bezier-path
str-closed-bezier-path make-?obj ?closed-bezier-path

: make-closed-bezier-path { ary d3 polar f: err -- path }
    ary make-bezier-path { path }
    true  		   path path-closed-bezier-path !
    d3    		   path path-3d !
    polar 		   path path-polar !
    err   		   path path-error sf!
    str-closed-bezier-path path path-type !
    path
;

\ === Literal-path ===
$" Literal-Path" constant str-literal-path
str-literal-path make-?obj ?literal-path

: make-literal-path-new { ary d3 polar -- path }
    assert1( ary ?array ary ?vct or )
    make-base-path { path }
    ary   	     path path-path !
    true  	     path path-literal-path !
    d3    	     path path-3d !
    polar 	     path path-polar !
    str-literal-path path path-type !
    path
;

\ === Spiral-path ===
$" Spiral-Path" constant str-spiral-path
str-spiral-path make-?obj ?sprial-path

: make-spiral-path-new { f: angle f: turns -- path }
    make-base-path { path }
    true  	    path path-spiral-path !
    angle 	    path path-start-angle sf!
    turns 	    path path-turns sf!
    str-spiral-path path path-type !
    path
;

: render-path { gen -- }
    gen path-bezier-path @ if
	gen bezier-render-path
    else
	gen path-literal-path @ if
	    gen literal-render-path
	else
	    gen path-spiral-path @ if
		gen spiral-render-path
	    then
	then
    then
;
: path-x@ { gen -- r }
    gen path-tx @ if
	gen path-tx @
    else
	gen path-rx @ if
	    gen path-rx @
	else
	    gen render-path
	    gen path-rx @
	then
    then
;
: path-y@ { gen -- r }
    gen path-ty @ if
	gen path-ty @
    else
	gen path-ry @ if
	    gen path-ry @
	else
	    gen render-path
	    gen path-ry @
	then
    then
;
: path-z@ { gen -- r }
    gen path-tz @ if
	gen path-tz @
    else
	gen path-rz @ if
	    gen path-rz @
	else
	    gen render-path
	    gen path-rz @
	then
    then
;
: path-time@ { gen -- r }
    gen path-tt @ if
	gen path-tt @
    else
	gen path-rt @ if
	    gen path-rt @
	else
	    gen render-path
	    gen path-rt @
	then
    then
;

\ === Make-path ===
$" 3d"                constant :3d
$" polar"             constant :polar
$" error"             dup constant :error float-keyword!
$" initial-direction" constant :initial-direction
$" final-direction"   constant :final-direction
: make-path ( ary keyword-args -- path )
    true  :3d                get-args  { d3 }
    false :polar             get-args  { polar }
    0.01e :error             get-fargs { f: err }
    nil   :initial-direction get-args  { init }
    nil   :final-direction   get-args  { fin }
    { ary }
    ary d3 polar err init fin make-open-bezier-path
;
' make-path alias array>path
: make-polar-path ( ary keyword-args -- path ) :polar true make-path ;
: make-closed-path ( ary keyword-args -- path )
    true  :3d    get-args  { d3 }
    false :polar get-args  { polar }
    0.01e :error get-fargs { f: err }
    { ary }
    ary ?array if
	ary array-first ary array-last vct= unless ary array-first ary array-push! then
    then
    ary d3 polar err make-closed-bezier-path
;
: make-literal-path ( ary keyword-args -- path )
    true  :3d    get-args { d3 }
    false :polar get-args { polar }
    { ary }
    ary d3 polar make-literal-path-new
;
: make-literal-polar-path ( ary keyword-args -- path ) :polar true make-literal-path ;

$" start-angle" dup constant :start-angle float-keyword!
$" turns"       dup constant :turns       float-keyword!
: make-spiral-path ( keyword-args -- path )
    0e :start-angle get-fargs { f: angle }
    2e :turns       get-fargs { f: turns }
    angle turns make-spiral-path-new
;

\ === Dlocsig ===
: $(.render-type) ( tp -- str )
    case
	amplitude-panning   of $" amplitude-panning"   endof
	b-format-ambisonics of $" b-format-ambisonics" endof
	decoded-ambisonics  of $" decoded-ambisonics"  endof
	$" unknown" swap
    endcase
;
: .render-type ( tp -- ) $(.render-type) .$ ;

str-dlocsig make-?obj ?dlocsig
: dlocsig-free { gen -- }
    gen ?dlocsig if
	gen dl-out-chan-gains @ gen-free
	gen dl-rev-chan-gains @ gen-free
	gen dl-output-gains @ gen-free
	gen dl-reverb-gains @ gen-free
	gen dl-path @ gen-free
	gen dl-delays @ gen-free
	gen dl-speaker-config @ gen-free
	nil gen gfm-base-name !
	gen free throw
    then
;
: dlocsig= { g1 g2 -- f } g1 ?dlocsig g2 ?dlocsig and g1 g2 = and ;
: $(.dlocsig) { gen -- str }
    $" #<" str-dlocsig $+
    $"  channels(out): " $+ gen dl-out-channels @ gen>string $+
    $" , channels(rev): " $+ gen dl-rev-channels @ gen>string $+
    $" , render: " $+ gen dl-render-using @ $(.render-type) $+
    *dlocsig-debug* if
	$" , speaker-config: " $+ gen dl-speaker-config @ gen>string $+
	$" , path: " $+ gen dl-path-path @ gen>string $+
    then
    $" >" $+
;
' $(.dlocsig) make-inspect .dlocsig
: dlocsig { f: input loc gen -- }
    loc gen dl-start @ < if
	loc gen dl-end @ >= if 0e else input then gen dl-path @ delay-1 fdrop
	gen dl-out-channels @ 0 ?do 0e loc i gen dl-output @ out-any loop
    else
	loc gen dl-end @ >= if 0e else input then gen dl-delays @ env gen dl-path @ delay { f: scl }
	gen dl-output-gains @ { oary }
	oary each env scl f* loc i gen dl-output @ out-any end-each
	gen dl-reverb-gains @ { rary }
	rary each env scl f* loc i gen dl-revput @ out-any end-each
    then
;
: dlocsig-run { f: val f: loc gen -- r3 } val loc f>s gen dlocsig ;
: clm-make-dlocsig { f: start f: dur path f: scl f: rev f: opow f: rpow output revput rend -- gen }
    output ?output if output channels@ else 1 then { out-chans }
    revput ?output if revput channels@ else 0 then { rev-chans }
    out-chans rev-chans < if $" more reverb channels than output channels" error then
    rend b-format-ambisonics = if
	out-chans 4 <> if
	    $" b-format-ambisonics requires 4 output channels" error
	then
	rev-chans 0= rev-chans 1 = or rev-chans 4 = or unless
	    $" b-format-ambisonics accepts only 0, 1 or 4 reverb channels" error
	then
	scl 0.8e f* to scl
    then
    out-chans make-array map 0 make-vct end-map { out-chan-gains }
    rev-chans make-array map 0 make-vct end-map { rev-chan-gains }
    dlocsig% %alloc { gen }
    opow             gen dl-output-power sf!
    rpow             gen dl-reverb-power sf!
    output           gen dl-output !
    revput           gen dl-revput !
    rend             gen dl-render-using !
    out-chans        gen dl-out-channels !
    rev-chans        gen dl-rev-channels !
    out-chan-gains   gen dl-out-chan-gains !
    rev-chan-gains   gen dl-rev-chan-gains !
    0e               gen dl-prev-time sf!
    0e               gen dl-prev-dist sf!
    0e               gen dl-prev-x sf!
    0e               gen dl-prev-y sf!
    0e               gen dl-prev-z sf!
    0e               gen dl-first-dist sf!
    0e               gen dl-last-dist sf!
    0e               gen dl-min-dist sf!
    0e               gen dl-max-dist sf!
    false            gen dl-prev-group !
    0 make-vct       gen dl-delay !
    path             gen dl-path-path !
    ['] dlocsig=     gen gfm-base-equal !
    ['] .dlocsig     gen gfm-base-inspect !
    ['] $(.dlocsig)  gen gfm-base-to-string !
    ['] dlocsig-run  gen gfm-base-run !
    ['] dlocsig-free gen gfm-base-free !
    str-dlocsig      gen gfm-base-name !
    path ?path if
	path path-x@
	path path-y@
	path path-z@
	path path-time@
    else
	$" make-dlocsig: path required " error
    then { xpoints ypoints zpoints tpoints }
    out-chans false zpoints each f0<> if drop true leave then end-each
    gen make-speakers
    *dlocsig-speed-of-sound* tpoints vct-last tpoints vct-first f- f* dur f/ gen dl-speed-limit sf!
    xpoints length 1 = if
	xpoints vct-first ypoints vct-first zpoints vct-first tpoints vct-first gen walk-all-rooms
    else
	0e 0e 0e 0e { f: xb f: yb f: zb f: tb }
	tpoints length 1- 0 ?do
	    i xpoints vct@ ( xa )
	    i ypoints vct@ ( ya )
	    i zpoints vct@ ( za )
	    i tpoints vct@ ( ta )
	    i 1+ xpoints vct@ fdup to xb
	    i 1+ ypoints vct@ fdup to yb
	    i 1+ zpoints vct@ fdup to zb
	    i 1+ tpoints vct@ fdup to tb gen minimum-segment-length
	loop
	xb yb zb tb gen walk-all-rooms
    then
    gen dl-first-dist sf@ gen dl-min-dist sf@ f- dist>samples gen dl-start !
    start dur f+ seconds>samples gen dl-end !
    gen dl-last-dist sf@ gen dl-first-dist sf@ f- dist>seconds dur f+ { f: real-dur }
    gen dl-min-dist sf@ 1e fmax gen dl-output-power sf@ f** scl f* { f: sc }
    gen sp-number @ make-array map
	:envelope i gen dl-out-chan-gains @ array@ :scaler sc :duration real-dur make-env
    end-map gen dl-output-gains !
    rev-chans if
	rev-chans make-array map
	    :envelope i gen dl-rev-chan-gains @ array@
	    :scaler gen dl-min-dist sf@ 1e fmax gen dl-reverb-power sf@ f** scl f* rev f*
	    :duration real-dur make-env
	end-map
    else
	0 make-array
    then gen dl-reverb-gains !
    :envelope gen dl-delay @
    :offset gen dl-min-dist sf@ fdist>samples fnegate
    :duration real-dur make-env gen dl-delays !
    gen dl-delay @ gen-free
    :size 1 :max-size gen dl-max-dist sf@ dist>samples 1 max make-delay gen dl-path !
    gen
;

$" render-using" constant :render-using
$" output-power" dup constant :output-power float-keyword!
$" reverb-power" dup constant :reverb-power float-keyword!

: make-dlocsig-args ( keyword-args -- args )
    make-hash { args }
    1e                :scaler        args set-fargs
    0.05e	      :reverb-amount args set-fargs
    1.5e	      :output-power  args set-fargs
    0.5e	      :reverb-power  args set-fargs
    *output*          :output        args set-args 
    *reverb*          :revout        args set-args 
    amplitude-panning :render-using  args set-args 
    args
;

: make-dlocsig-with-args { f: start f: dur path args -- gen }
    start dur path
    :scaler        args fhash@
    :reverb-amount args fhash@
    :output-power  args fhash@
    :reverb-power  args fhash@
    :output        args hash@
    :revout        args hash@
    :render-using  args hash@ clm-make-dlocsig
;

: make-dlocsig ( f: start f: dur path keyword-args -- gen )
    make-dlocsig-args { args }
    { f: start f: dur pat }
    assert1( pat ?array pat ?vct or pat ?path or )
    pat dup ?array dup ?vct or if make-path then { path }
    start dur path args make-dlocsig-with-args
;

\ === Sinewave ===
$" path" constant :path
instrument: sinewave ( keyword-args -- )
    make-dlocsig-args { args }
    0e   :beg     get-fargs { f: start }
    1e   :dur     get-fargs { f: dur }
    440e :freq    get-fargs { f: freq }
    0.5e :amp     get-fargs { f: amp }
    nil  :path    get-args  { path }
    nil  :amp-env get-args  { ampenv }
    ampenv ?envelope if false else vct[ 0e 1e 1e 1e ] to ampenv true then { del-amp }
    path ?path path ?array or path ?vct or if
	false
    else
	vct[ -10e 10e 0e 1e ] vct[ 0e 5e 1e 1e ] vct[ 10e 10e 0e 1e ] 3 >array make-path to path
	true
    then { del-path }
    start dur path args make-dlocsig-with-args { dl }
    :frequency freq make-oscil { os }
    :envelope ampenv :scaler amp :duration dur make-env { en }
    start dur run en env 0e 0e os oscil f* i dl dlocsig loop
    os gen-free
    en gen-free
    dl gen-free
    del-amp if ampenv gen-free then
    del-path if path gen-free then
;instrument

\ === DL-Move ===
: dl-move { f: start file path f: scl f: reverb f: opow f: rpow rend -- }
    file sound-duration { f: dur }
    file sound-chans dup s>f { chns f: fchns }
    chns make-array map :file file :start start f>s :channel i make-readin end-map { rds }
    start dur path
    :scaler scl
    :reverb-amount reverb
    :output-power opow
    :reverb-power rpow
    :render-using rend make-dlocsig { dl }
    start dur run 0e rds each readin f+ end-each fchns f/ i dl dlocsig loop
    rds each mus-close end-each
    rds gen-free
    dl gen-free
;
: dl-move-with-args { f: start file path args -- }
    file sound-duration { f: dur }
    file sound-chans dup s>f { chns f: fchns }
    chns make-array map :file file :start start f>s :channel i make-readin end-map { rds }
    start dur path args make-dlocsig-with-args { dl }
    start dur run 0e rds each readin f+ end-each fchns f/ i dl dlocsig loop
    rds each mus-close end-each
    rds gen-free
    dl gen-free
;

\ === With-Move-Sound ===
: with-move-sound-body-xt { file path args -- }
    lambda-create file , path , args , latestxt
  does> { self -- }
    self           @ { file }
    self cell+     @ { path }
    self 2 cells + @ { args }
    \ :output and :revout must be set after opening outfiles in
    \ with-snd-with-args i.e. here in the xt-body
    :output *output* args hash!
    :revout *reverb* args hash!
    *verbose* if
	!script-cr
	." \  with-move-sound on " *channels* dup . ." channel" 1 > if ." s" then
	script-cr
    then
    0e file path args dl-move-with-args
;
: with-move-sound ( infile path keyword-args -- )
    ws-preserve
    make-with-sound-args { args }
    make-dlocsig-args { dl-args }
    { infile path }
    path ?path path ?array or path ?vct or if
	false
    else
	vct[ -10e 10e 0e 1e ] vct[ 0e 5e 1e 1e ] vct[ 10e 10e 0e 1e ] 3 >array make-path to path
	true
    then { del-path }
    infile path dl-args with-move-sound-body-xt args with-snd-with-args
    del-path if path gen-free then
    dl-args gen-free
    args gen-free
    ws-reset
;

\ === Move-Sound ===
: make-move-sound-cb { args -- xt; tempnam -- filename }
    lambda-create args , latestxt
  does> { to-move self -- filename }
    self @ { args }
    *verbose* if
	!script-cr
	." \ move-sound on " :channels args hash@ dup . ." channel" 1 > if ." s" then
	script-cr
    then
    0e to-move :path args hash@ args dl-move-with-args
    :infile to-move :outloc :begin args fhash@ seconds>samples mus-mix \ outfile
;

\ move-sound should be part of with-sound's body-xt:
\ lambda:
\   make-spiral-path { path }
\   0e 2e 330e 0.2e ['] fm-violin :path path :channels 4 move-sound drop
\   path gen-free
\ ; :play true :statistics true :channels 4 with-sound
: move-sound ( body-xt keyword-args -- outfile )
    make-dlocsig-args { args }
    0e  :begin    args set-fargs
    1   :channels args set-args
    nil :path     args set-args
    { body-xt }
    :path args hash@ { path }
    path ?path path ?array or path ?vct or if
	false
    else
	:path vct[ -10e 10e 0e 1e ] vct[ 0e 5e 1e 1e ] vct[ 10e 10e 0e 1e ] 3 >array args hash!
	true
    then { del-path }
    body-xt :channels :channels args hash@ :statistics false :play false 7 >array 1 >array { ws }
    ws args make-move-sound-cb sound-let \ returns outfile
    ws free-array
    del-path if :path args hash@ gen-free then
    args gen-free
;

\ === Dlocsig-Test ===
: body-xt { f: start f: dur -- }
    start dur 440e 0.3e fm-violin
    start 0.3e f+ dur 1020e 0.1e fm-violin
;

event: dlocsig-test ( keyword-args -- )
    0e :beg get-fargs now!
    1e :dur get-fargs { f: dur }

    vct[ -10e 10e 0e 1e ] vct[ 0e 5e 0e 0e ] vct[ 10e 10e 5e 1.5e ] 3 >array { ary }
    ary make-path { path }
    :beg now@ :dur dur :path path sinewave
    path gen-free
    ary gen-free

    dur 0.2e f+ wait

    vct[ -10e 10e 0e 5e 10e 10e ] to ary
    ary make-path to path
    now@ dur ['] body-xt :begin now@ :path path :channels 2 move-sound drop
    path gen-free
    ary gen-free

    dur 0.2e f+ wait

    vct[ -10e 10e ] vct[ 0.1e 0.1e ] vct[ 10e -10e ] 3 >array to ary
    ary make-literal-path to path
    now@ dur ['] body-xt :begin now@ :path path :channels 4 move-sound drop
    path gen-free
    ary gen-free

    dur 0.2e f+ wait

    :turns 5.5e :start-angle 180e make-spiral-path to path
    vct[ 0e 0e 10e 1e 50e 1e 100e 0e ] { ampenv }
    :beg now@ :dur dur :path path :amp-env ampenv :render-using decoded-ambisonics sinewave
    ampenv gen-free
    path gen-free

    dur wait
;event

\ dlocsig.fs ends here
