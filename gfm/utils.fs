\ utils.fs -- utility definitions -*- forth -*-

\ Copyright (C) 2003--2004 Michael Scholz

\ Author: Michael Scholz <scholz-micha@gmx.de>
\ Created: Thu Aug 28 00:28:20 CEST 2003
\ Last: Fri Oct 08 22:35:17 CEST 2004
\ Ident: $Id: utils.fs,v 1.49 2004/10/08 20:45:29 mike Exp $

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

\ General purpose definitions which may be useful in other Forth
\ programs too.

\ Code:

\ === String ===
struct
    cell% field str-len
    cell% field str-ptr
    double% field str-name
end-struct string%

: bs ( -- ) #bs emit ;

: string-length ( str -- n ) str-len @ ;
\ .string is defered in gforth's main lib
:noname { str -- } str str-ptr @ str str-len @ type ; is .string
: make-string { len -- str }
    string% %alloc { str }
    len chars allocate throw dup len chars erase str str-ptr !
    len str str-len !
    s" string" str str-name 2!
    str
;
: ?string { obj -- f } try obj str-name 2@ s" string" str= recover drop false endtry ;
: free-string { str -- } str str-ptr @ free throw str free throw ;
: string+ { str1 str2 -- str3 }
    str1 str-len @ { len1 }
    str2 str-len @ { len2 }
    len1 len2 + make-string { str3 }
    str1 str-ptr @ str3 str-ptr @ len1 chars move
    str2 str-ptr @ str3 str-ptr @ len1 chars + len2 chars move
    str3
;
: $>string { addr u -- str }
    u make-string { str }
    addr str str-ptr @ u chars move
    str
;
: $>string+ { addr u str1 -- str2 } addr u $>string str1 string+ ;
: string>$ { str -- addr u } str str-ptr @ str str-len @ ;
: string= { str1 str2 -- f } str1 string>$ str2 string>$ str= ;

\ === Forth Strings ( addr u ) ===
: >$ { addr1 u1 -- addr2 u2 }
    u1 allocate throw { addr2 }
    addr1 addr2 u1 chars move
    addr2 u1
;
: $+ { addr1 u1 addr2 u2 -- addr3 u1+u2 }
    u1 u2 + allocate throw { addr3 }
    addr1 addr3 u1 chars move
    addr2 addr3 u1 chars + u2 chars move
    addr3 u1 u2 +
;
' str= alias $=

\ === Array ===
\
\ Usage: 0 0 100 1 4 >array value ary
\        ary .array          --> [0, 0, 100, 1]
\        33 0 ary vct!
\        0 ary array@ . --> 33
struct
    cell% field array-len
    cell% field array-ptr
    double% field array-name
end-struct array%

: array-data ( w -- w' ) array-ptr @ ;
: array-length ( w -- u ) array-len @ ;
: ?array { obj -- f } try obj array-name 2@ s" array" str= recover drop false endtry ;
: array@ { idx ary -- n } assert( idx 0 ary array-len @ within ) ary array-ptr @ idx cells + @ ;
: array! { n idx ary -- } assert( idx 0 ary array-len @ within ) n ary array-ptr @ idx cells + ! ;
: .array { ary -- }
    ary array-length { alen }
    ." #<array[" ary array-length . bs ." ]:"
    alen if ."  [" ary array-length 0 do i ary array@ . loop bs ." ]" then
    ." >"
;
: free-array { ary -- } ary array-ptr @ free throw ary free throw ;
\ Usage: ary array-each  i ary array@ .  loop
: array-each ( array -- ) postpone array-length 0 postpone literal postpone do ; immediate

: make-simple-array { len -- ary }
    assert1( len 0>= )
    array% %alloc { ary }
    len cells allocate throw { data }
    len ary array-len !
    data ary array-ptr !
    s" array" ary array-name 2!
    ary
;
: make-array { len -- w }
    len make-simple-array { ary }
    len if ary array-ptr @ len cells bounds u+do 0 i ! cell +loop then
    ary
;
: >array ( data u -- w ) { len }
    len make-simple-array { ary }
    len if ary array-ptr @ cell - len cells bounds swap u-do i ! cell -loop then
    ary
;
: array= { ary1 ary2 -- f }
    ary1 ary2 = if
	true
    else
	ary1 array-length ary2 array-length = if
	    true
	    ary1 array-length 0 do i ary1 array@ i ary2 array@ <> if drop false leave then loop
	else
	    false
	then
    then
;
: array-copy { ary1 -- ary2 }
    ary1 array-len @ { len }
    len make-simple-array { ary2 }
    ary1 array-ptr @ ary2 array-ptr @ len cells move
    ary2
;
: c-array>array { addr len -- ary }
    len make-simple-array { ary }
    addr ary array-ptr !
    ary
;
: array-push! { val ary -- }
    ary array-len @ 1+ { len }
    len cells allocate throw { data }
    ary array-ptr @ data len 1- cells move
    val data len 1- cells + !
    ary array-ptr @ free throw
    len ary array-len !
    data ary array-ptr !
;
: array-pop! { ary -- val }
    ary array-len @ 0= if
	0
    else
	ary array-len @ 1- { len }
	len ary array@			\ result
	len cells allocate throw { data }
	ary array-ptr @ data len cells move
	ary array-ptr @ free throw
	len ary array-len !
	data ary array-ptr !
    then
;
: array-unshift! { val ary -- }
    ary array-len @ 1+ { len }
    len cells allocate throw { data }
    val data !
    ary array-ptr @ data cell + len 1- cells move
    ary array-ptr @ free throw
    len ary array-len !
    data ary array-ptr !
;
: array-shift! { ary -- val }
    ary array-len @ 0= if
	0
    else
	ary array-len @ 1- { len }
	0 ary array@			\ result
	len cells allocate throw { data }
	ary array-ptr @ cell + data len cells move
	ary array-ptr @ free throw
	len ary array-len !
	data ary array-ptr !
    then
;
: array-reverse! { ary -- }
    ary array-len @ { len }
    len 1 > if
	len 1- { idx }
	len cells allocate throw { data }
	len 0 u+do ary array-ptr @ i cells + @  data idx cells + !  idx 1- to idx loop
	ary array-ptr @ free throw
	data ary array-ptr !
    then
;

\ === Online Help ===
struct
    cell% field help-name
    cell% field help-comment
end-struct help%

0 make-array value *clm-online-help*

: make-help { name-xt d: comm -- gen }
    help% %alloc { gen }
    name-xt gen help-name !
    comm $>string gen help-comment !
    gen
;
: help! ( name-xt d: comm -- ) make-help *clm-online-help* array-push! ;
' help! s\" help! ( xt addr u -- ) \\ ' lambda: s\" lambda: alias :noname\" help!" help!
: help ( "name" -- )
    parse-word find-name ?dup-if
	name>int { name-xt }
	false
	*clm-online-help* array-length 0 do
	    i *clm-online-help* array@ dup help-name @ name-xt = if
		cr help-comment @ .string
		true leave
	    else
		drop
	    then
	loop
	if drop else ."  no description available " then
    else
	."  unknown word "
    then
;
' help s\" help ( \"name\" -- ) \\ help make-oscil" help!

\ === Value definitions for Double, Float and Strings ===
: 2value ( d: val "name" -- ; addr -- d: val )
    create 2,
  does> ( addr -- d: val )
    2@
;
: 2to ( d: val "name" -- )
    parse-word find-name ?dup-if name>int >body 2! else true abort" 2value doesn't exist " then
;
: fvalue ( f: val "name" -- ; addr -- f: val )
    create f,
  does> ( addr -- f: val )
    f@
;
: fto ( f: val "name" -- )
    parse-word find-name ?dup-if name>int >body f! else true abort" fvalue doesn't exist" then
;
' 2value alias $value
' 2to alias $to

\ === Floats ===
: s>f ( n -- r ) s>d d>f ;
: f>s ( r -- n ) f>d d>s ;
\ F.R is similar to D.R and friends
\ usage: 17e 3 f.r --> 17.000
: f.r { f: val u -- }
    val f0< { s }			\ true == -1, i.e. negative
    val fabs to val
    u s>f { f: dig }
    dig 2e f+ fnegate falog 5e f* val f+ to val	\ try to round
    2e dig falog f* 1/f val f+ dig falog f* f>d	\ (1 / (2 * 10^u) + r) * 10^u
    <# u 0 ?do # loop [char] . hold #s s sign #> type space
;
: fmod { f: a f: b }
    a b f/ { f: x }
    x b f* x floor b f* f-
;
: flogn { f: r f: b -- r }
    r f0<= abort" r must be > 0"
    b f0<= abort" b must be > 0"
    r fln b fln f/
;
: truncate { f: r1 -- n r2 } r1 f>s { n } r1 n s>f f- n ;
: prime { val -- f }
    val 2 =
    val 2 mod
    true val s>f fsqrt f>s 1+ 3 do val i mod 0= if drop false leave then 2 +loop and or
;

\ === Data Type Short ===
2 constant short
: shorts ( n1 -- n2 ) short * ;
: short+ ( addr1 -- addr2 ) 1 shorts + ;
: s@ ( w -- s ) dup c@ swap 1+ c@ 8 lshift + dup 32768 >= if 65536 - then ;
: s! ( s w -- ) >r dup $ff and r@ c! 8 rshift $ff and r> 1+ c! ;
: s+! ( s w -- )
    dup >r 1+ c@ r@ c@ rot dup $ff and swap 8 rshift $ff and
    rot + rot rot + 8 lshift + dup 32768 >= if 65536 - then r>
    >r dup $ff and r@ c! 8 rshift $ff and r> 1+ c! \ <-- s!
;

\ === Timer ===
struct
    double% field real-time
    double% field cpu-utime
    double% field cpu-stime
end-struct utime%

variable current-time utime% %allot current-time !

: 3d.r ( d -- ) 1 100 m*/ 5. d+ 1 10 m*/ <# # # # [char] . hold #s #> type ;
: 2d.r ( d -- ) 1 1000 m*/ 5. d+ 1 10 m*/ <# # # [char] . hold #s #> type ;

: set-timer ( dutime dcpu-utime dcpu-stime -- )
    current-time @ dup >r cpu-stime 2! r@ cpu-utime 2! r> real-time 2!
;
: get-timer ( -- dutime dcpu-utime dcpu-stime )
    current-time @ dup >r real-time 2@ r@ cpu-utime 2@ r> cpu-stime 2@
;
: start-timer ( -- ) utime cputime set-timer ;
: stop-timer ( -- )
    utime { d: u1 } cputime { d: cu1 d: cs1 }
    get-timer { d: u2 d: cu2 d: cs2 }
    u1 u2 d- cu1 cu2 d- cs1 cs2 d- set-timer
;
: .timer ( -- )
    get-timer 2swap 2rot ." real: " 3d.r ."   (utime " 3d.r ." , stime " 3d.r ." )"
;
: .timer-ratio { n1 n2 -- }
    try
	get-timer 2drop 2>r n1 1 m*/ 1 n2 m*/ ." ratio: " 2d.r
	2r> n1 1 m*/ 1 n2 m*/ ."   (uratio " 2d.r ." )"
    recover
	drop
	." no ratio (division by zero)"
    endtry
;

\ === Others ===
: lambda-create noname create ;
' :noname alias lambda:

\ simple debug help;
\ usage: int1 int2 2 .debug
\        prints values of int1 and int2
: .debug ( ?? n -- ) ." \ === DEBUG: " 0 u+do . loop ." ===" cr ;

: str>upper ( c-addr u -- ) bounds do i c@ toupper emit 1 chars +loop ;
: str>capitalize ( c-addr u -- ) over c@ toupper emit bounds char+ do i c@ emit 1 chars +loop ;
: get-rcs-version ( addr u1 -- addr u2 ) 11 /string 2dup s"  $" search drop blank -trailing ;
: date>str ( -- c-addr u )
    time&date { nsec nmin nhour nday nmonth nyear }
    nday 0 <# # #s #> s" -" $+
    nmonth 0 <# # #s #> $+ s" -" $+
    nyear 0 <# #s #> $+ s"  " $+
    nhour 0 <# # #s #> $+ s" :" $+
    nmin 0 <# # #s #> $+
;
: .cstring ( w -- ) cstring>sstring type ;
\ convert forth string (addr len) to null terminated c-string (addr)
: c-string { addr1 len -- adr' }
    len 1+ allocate throw { addr2 }
    addr2 len 1+ erase
    addr1 addr2 len chars move
    addr2
;

\ Returns a forth string containing the output of the command `cmd'.
\ s" pwd" shell type --> /home/mike
1024 cells constant shell-buffer-size
: shell { d: cmd -- d: output }
    cmd r/o open-pipe throw { fd }
    shell-buffer-size allocate throw { output }
    output shell-buffer-size erase
    output shell-buffer-size fd read-file throw { len }
    fd close-pipe throw drop
    output len 1- >$			\ remove trailing `\n'
    output free throw
;
\ Try to load given file in current or $HOME dir;
\ if file doesn't exist, do nothing.
: load-init-file { d: fname -- }
    try s" pwd" shell s" /" $+ fname $+ included recover
	drop try s" HOME" getenv s" /" $+ fname $+ included recover drop endtry
    endtry
;

\ utils.fs ends here
