\ notelist.fs -- note list definitions and generators -*- forth -*-

\ Copyright (C) 2003--2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Sun Aug 03 03:44:32 CEST 2003
\ Last: Fri Oct 08 22:21:20 CEST 2004
\ Ident: $Id: notelist.fs,v 1.42 2004/10/08 20:22:15 mike Exp $

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

\ Helper functions for writing note lists.

\ Code:

require utils.fs
only forth also GFMusic definitions also CLM-Sndlib

\ === Note list ===
fvariable time-now 0e time-now f!
fvariable snd-tempo 60e snd-tempo f!
fvariable snd-beat 0.25e snd-beat f!

: run ( start dur -- ) postpone times>samples postpone do ; immediate
: now@ ( -- secs ) time-now f@ ;
: now! ( secs -- ) time-now f! ;
: wait ( secs -- ) now@ f+ now! ;
: tempo@ ( -- r ) snd-tempo f@ ;
: tempo! ( r -- ) snd-tempo f! ;

: show-instruments ( -- )
    ." \ --- " *clm-instruments* array-length . ." instruments ---" cr
    *clm-instruments* array-each
	." \ " i 1+ 2 u.r ." : " i *clm-instruments* array@ .string cr
    loop
;
: ins-info ( d: eword -- ) *notehook* ?dup-if execute else 2drop then ;
: instrument: ( -- )
    >in @ parse-word { d: eword } >in !
    eword $>string *clm-instruments* array-push! :
    eword postpone sliteral ['] ins-info compile,
;
: ;instrument ( -- ) postpone ; ; immediate

: event-info ( d: eword -- )
    *verbose* if script? 0= if cr then ." \    event: " type script? if cr then else 2drop then
;
: event: ( -- )
     >in @ parse-word { d: eword } >in ! :
    eword postpone sliteral ['] event-info compile,
;
: ;event ( -- ) postpone ; ; immediate

: rescale { f: number f: old-min f: old-max f: new-min f: new-max }
    old-max old-min f- { f: val1 }
    new-max new-min f- { f: val2 }
    number old-min f- val2 f* val1 f/ new-min f+
;
: rnd-pick { ary -- u ary }
    ary ?array if ary array-length else ary ?vct if ary vct-length else 0 then then irandom ary
;
: ?chance ( r -- f ) 1e random f> ;
' ?chance alias ?odds
: between ( nmin nmax -- n ) over - dup 0> if irandom + else 2drop 0 then ;
: fbetween ( rmin rmax -- r ) fover f- fdup f0> if random f+ else fdrop fdrop 0e then ;
: drunk { n1 w nmin nmax -- n2 }
    0 { n2 }
    begin n2 nmin nmax within 0= while n1 w - n1 w + between to n2 repeat
    n2
;

\ === Pitches ===
6.875e fconstant lowest-freq

: interval>hertz ( n -- r ) 2e 12e s>f 3e f+ f+ 12e f/ f** lowest-freq f* ;
: keynum>hertz ( n -- r ) 2e s>f 3e f+ 12e f/ f** lowest-freq f* ;
: hertz>keynum ( r -- n ) lowest-freq f/ 2e flogn 12e f* 3e f- f>s ;

: pitch ( interval octave "name" -- )
    2e s>f 1e f+ 12e f* s>f 3e f+ f+ 12e f/ f** lowest-freq f*
    create f,
  does> ( -- freq )
    f@
;

 0 0 pitch C0    1 0 pitch Cs0    1 0 pitch Df0
 2 0 pitch D0    3 0 pitch Ds0    3 0 pitch Ef0
 4 0 pitch E0    4 0 pitch Ff0    5 0 pitch Es0
 5 0 pitch F0    6 0 pitch Fs0    6 0 pitch Gf0
 7 0 pitch G0    8 0 pitch Gs0    8 0 pitch Af0
 9 0 pitch A0   10 0 pitch As0   10 0 pitch Bf0
11 0 pitch B0   11 0 pitch Cf0   12 0 pitch Bs0

 0 1 pitch C1    1 1 pitch Cs1    1 1 pitch Df1
 2 1 pitch D1    3 1 pitch Ds1    3 1 pitch Ef1
 4 1 pitch E1    4 1 pitch Ff1    5 1 pitch Es1
 5 1 pitch F1    6 1 pitch Fs1    6 1 pitch Gf1
 7 1 pitch G1    8 1 pitch Gs1    8 1 pitch Af1
 9 1 pitch A1   10 1 pitch As1   10 1 pitch Bf1
11 1 pitch B1   11 1 pitch Cf1   12 1 pitch Bs1

 0 2 pitch C2    1 2 pitch Cs2    1 2 pitch Df2
 2 2 pitch D2    3 2 pitch Ds2    3 2 pitch Ef2
 4 2 pitch E2    4 2 pitch Ff2    5 2 pitch Es2
 5 2 pitch F2    6 2 pitch Fs2    6 2 pitch Gf2
 7 2 pitch G2    8 2 pitch Gs2    8 2 pitch Af2
 9 2 pitch A2   10 2 pitch As2   10 2 pitch Bf2
11 2 pitch B2   11 2 pitch Cf2   12 2 pitch Bs2

 0 3 pitch C3    1 3 pitch Cs3    1 3 pitch Df3
 2 3 pitch D3    3 3 pitch Ds3    3 3 pitch Ef3
 4 3 pitch E3    4 3 pitch Ff3    5 3 pitch Es3
 5 3 pitch F3    6 3 pitch Fs3    6 3 pitch Gf3
 7 3 pitch G3    8 3 pitch Gs3    8 3 pitch Af3
 9 3 pitch A3   10 3 pitch As3   10 3 pitch Bf3
11 3 pitch B3   11 3 pitch Cf3   12 3 pitch Bs3

 0 4 pitch C4    1 4 pitch Cs4    1 4 pitch Df4
 2 4 pitch D4    3 4 pitch Ds4    3 4 pitch Ef4
 4 4 pitch E4    4 4 pitch Ff4    5 4 pitch Es4
 5 4 pitch F4    6 4 pitch Fs4    6 4 pitch Gf4
 7 4 pitch G4    8 4 pitch Gs4    8 4 pitch Af4
 9 4 pitch A4   10 4 pitch As4   10 4 pitch Bf4
11 4 pitch B4   11 4 pitch Cf4   12 4 pitch Bs4

 0 5 pitch C5    1 5 pitch Cs5    1 5 pitch Df5
 2 5 pitch D5    3 5 pitch Ds5    3 5 pitch Ef5
 4 5 pitch E5    4 5 pitch Ff5    5 5 pitch Es5
 5 5 pitch F5    6 5 pitch Fs5    6 5 pitch Gf5
 7 5 pitch G5    8 5 pitch Gs5    8 5 pitch Af5
 9 5 pitch A5   10 5 pitch As5   10 5 pitch Bf5
11 5 pitch B5   11 5 pitch Cf5   12 5 pitch Bs5

 0 6 pitch C6    1 6 pitch Cs6    1 6 pitch Df6
 2 6 pitch D6    3 6 pitch Ds6    3 6 pitch Ef6
 4 6 pitch E6    4 6 pitch Ff6    5 6 pitch Es6
 5 6 pitch F6    6 6 pitch Fs6    6 6 pitch Gf6
 7 6 pitch G6    8 6 pitch Gs6    8 6 pitch Af6
 9 6 pitch A6   10 6 pitch As6   10 6 pitch Bf6
11 6 pitch B6   11 6 pitch Cf6   12 6 pitch Bs6

 0 7 pitch C7    1 7 pitch Cs7    1 7 pitch Df7
 2 7 pitch D7    3 7 pitch Ds7    3 7 pitch Ef7
 4 7 pitch E7    4 7 pitch Ff7    5 7 pitch Es7
 5 7 pitch F7    6 7 pitch Fs7    6 7 pitch Gf7
 7 7 pitch G7    8 7 pitch Gs7    8 7 pitch Af7
 9 7 pitch A7   10 7 pitch As7   10 7 pitch Bf7
11 7 pitch B7   11 7 pitch Cf7   12 7 pitch Bs7

 0 8 pitch C8    1 8 pitch Cs8    1 8 pitch Df8
 2 8 pitch D8    3 8 pitch Ds8    3 8 pitch Ef8
 4 8 pitch E8    4 8 pitch Ff8    5 8 pitch Es8
 5 8 pitch F8    6 8 pitch Fs8    6 8 pitch Gf8
 7 8 pitch G8    8 8 pitch Gs8    8 8 pitch Af8
 9 8 pitch A8   10 8 pitch As8   10 8 pitch Bf8
11 8 pitch B8   11 8 pitch Cf8   12 8 pitch Bs8

\ === Note length ===
: bpm>seconds ( bpm -- secs ) 60e fswap f/ ;
: rhythm>seconds ( rhy -- secs ) 4e tempo@ bpm>seconds f* f* ;

: notelength ( scale "name" -- )
    create f,
  does> ( -- r )
    f@ rhythm>seconds
;

 1e     notelength W			\ whole
 2e 1/f notelength H			\ half
 4e 1/f notelength Q			\ quarter
 8e 1/f notelength A			\ eighth
16e 1/f notelength S			\ sixteenth
32e 1/f notelength T			\ thirty-second
 1e      2e 1/f f+ notelength W.
 2e 1/f  4e 1/f f+ notelength H.
 4e 1/f  8e 1/f f+ notelength Q.
 8e 1/f 16e 1/f f+ notelength A.
16e 1/f 32e 1/f f+ notelength S.

\ notelist.fs ends here
