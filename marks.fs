\ -*- snd-forth -*-
\ marks.fs -- marks.scm|rb -> marks.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Dec 27 19:22:06 CET 2005
\ Changed: Mon Dec 11 22:43:11 CET 2006

\ Commentary:
\
\ add-named-mark       ( samp name snd chn -- mark )
\ mark-name->id        ( name -- m )
\ move-syncd-marks     ( sync diff -- )
\ describe-mark        ( id -- ary )
\
\ mark-properties      ( id -- props )
\ set-mark-properties  ( id val -- )
\ mark-property        ( id key -- val )
\ set-mark-property    ( id key val -- )
\ save-mark-properties ( -- )
\ mark-click-info      ( id -- #t )

\ Code:

require clm
require examp

\ from rubber.fs
: add-named-mark ( samp name snd chn -- mark )
  { samp name snd chn }
  samp snd chn add-mark { m }
  m name set-mark-name drop
  m
;

\ mark-name->id is a global version of find-mark

: mark-name->id { name -- m }
  doc" ( name -- m )  Like find-mark but searches all currently accessible channels."
  #f					\ flag
  sounds each { snd }
    snd channels 0 ?do
      name snd i undef find-mark { m }
      m mark? if
	drop				\ replace #f with mark
	m
	exit
      then
    loop
  end-each
;

: move-syncd-marks { syn diff }
  doc" ( sync diff -- )  Moves all marks sharing SYNC by DIFF samples."
  syn syncd-marks each ( m ) dup undef mark-sample diff + set-mark-sample drop end-each
;

: describe-mark { id -- ary }
  doc" ( id -- ary )  \
Returns a description of the movements of mark ID over the channel's edit history."
  id ['] mark-home 'no-such-mark nil fth-catch if
    sounds each { snd }
      snd channels 0 ?do
	0 snd i ( chn ) edits each + end-each 1+ ( max-edits ) 0 ?do
	  snd j ( chn ) #f marks { m }
	  m
	  id m list-member? && if
	    '( snd j ( chn ) )
	    leave
	  then
	loop
      loop
    end-each
  then { mark-setting }
  mark-setting list? if
    mark-setting car { snd }
    mark-setting cadr { chn }
    #( #( 'mark id 'sound snd snd short-file-name 'channel chn ) ) { descr }
    0 snd chn edits each + end-each 1+ 1 ?do
      descr
      snd chn i marks id list-member? if
	id i mark-sample
      else
	#f
      then
      array-push drop
    loop
    descr
  else
    'no-such-mark '( get-func-name id ) fth-throw
  then
;

\ --- mark property lists ---

: mark-properties     ( id -- props ) object-id 'mark-property property-ref ;
: set-mark-properties ( id val -- )   swap object-id 'mark-property rot property-set! ;


: mark-property { id key -- val }
  doc" ( id key -- val )  \
Returns the value associated with KEY in the given mark's property list, or #f."
  id mark? if
    id mark-properties dup hash? if key hash-ref else drop #f then
  else
    'no-such-mark '( get-func-name id ) fth-throw
  then
;

: set-mark-property { id key val -- }
  doc" ( id key val -- )  Sets the value VAL to KEY in the given mark's property list."
  id mark? if
    id mark-properties { props }
    props hash? if
      props key val hash-set!
    else
      make-hash to props
      props key val hash-set!
      id props set-mark-properties
    then
  else
    'no-such-mark '( get-func-name id ) fth-throw
  then
;

hide
: save-mark-properties-cb ( filename -- )
  { filename }
  filename w/a io-open { io }
  io $" \n\\ from save-mark-properties in %s\n\n" _ '( *filename* ) io-write-format
  io $" require marks\n\n" io-write
  undef undef undef marks each ( snd-m )
    each ( chn-m )
      each { m }
	m mark-properties { mp }
	mp if
	  m mark-home { mhome }
	  m undef mark-sample { msamp }
	  io $" let:\n" io-write
	  io $"   %S 0 find-sound { snd }\n" '( mhome car file-name ) io-write-format
	  io $"   snd sound? if\n" io-write
	  io $"     %d snd %d find-mark { mk }\n" '( msamp mhome cadr ) io-write-format
	  io $"     mk mark? if\n" io-write
	  io $"       mk %m set-mark-properties\n" '( mp ) io-write-format
	  io $"     then\n" io-write
	  io $"   then\n" io-write
	  io $" ;let\n" io-write
	then
      end-each
    end-each
  end-each
  io io-close
;
set-current
: save-mark-properties ( -- )
  doc" ( -- )  Sets up an after-save-state-hook function to save any mark-properties."
  after-save-state-hook ['] save-mark-properties-cb 1 make-proc add-hook!
;
previous

: mark-click-info { id -- #t }
  doc" ( id -- #t )  A mark-click-hook function that describes a mark and its properties.\n\
mark-click-hook ' mark-click-info 1 make-proc add-hook!"
  id mark-name empty? if "" else $"  (%S)" '( id mark-name ) string-format then { mname }
  $"     mark id: %d%s\n"   '( id mname )     string-format { info-string }
  $"      sample: %d (%.3f secs)\n"
  '( id undef mark-sample
     dup id mark-home car srate f/ ) string-format info-string swap $+ to info-string
  id mark-sync 0<> if
    $"        sync: %s\n" '( id mark-sync ) string-format info-string swap $+ to info-string
  then
  id mark-properties { props }
  props if $"  properties: %s" '( props ) string-format info-string swap $+ to info-string then
  $" Mark Info" info-string info-dialog drop
  #t
;

\ This code saves mark info in the sound file header, and reads it
\ back in when the sound is later reopened.

: marks->string { sndf }
  $" \nrequire marks\n" { str }
  str $" let:\n" $+ to str
  str $"   #f { mr }\n" $+ to str
  sndf marks each ( chan-marks )
    $" \n  \\ channel %d\n" '( i ) string-format str swap $+ to str
    each { m }
      m nil? ?leave
      $"   %s #f %d %S %d add-mark to mr\n"
      '( m undef mark-sample
	 j ( chn )
	 m mark-name length 0= if #f else m mark-name then
	 m mark-sync ) string-format str swap $+ to str
      m mark-properties { props }
      props if
	$"   mr %m set-mark-properties\n" '( props ) string-format str swap $+ to str
      then
    end-each
  end-each
  str $" ;let\n" $+
;

0 [if]
  output-comment-hook 1 lambda: { str } selected-sound marks->string ;proc add-hook!
  after-open-hook 1 lambda: { snd }
    snd comment { str }
    str ['] string-eval #t nil fth-catch if drop ( str ) then
  ;proc add-hook!
[then]

\ marks.fs ends here
