\ -*- snd-forth -*-
\ marks.fs -- marks.scm|rb -> marks.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Dec 27 19:22:06 CET 2005
\ Changed: Thu Dec 11 00:27:49 CET 2008

\ Commentary:
\
\ add-named-mark       ( samp name snd chn -- mark )
\ mark-name->id        ( name -- m )
\ move-syncd-marks     ( sync diff -- )
\ describe-mark        ( id -- ary )
\
\ mark-properties      ( id -- props )
\ set-mark-properties  ( id props -- )
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
: mark-name->id ( name -- m )
  doc" Like find-mark but searches all currently accessible channels."
  { name }
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

: move-syncd-marks ( sync diff )
  doc" Moves all marks sharing SYNC by DIFF samples."
  { syn diff }
  syn syncd-marks each ( m ) dup undef mark-sample diff + set-mark-sample drop end-each
;

: describe-mark ( id -- ary )
  doc" Returns a description of the movements of mark ID over the channel's edit history."
  { id }
  id <'> mark-home 'no-such-mark nil fth-catch if
    sounds each { snd }
      snd channels 0 ?do
	0 snd i ( chn ) edits each + end-each 1+ ( max-edits ) 0 ?do
	  snd j ( chn ) #f marks { m }
	  m
	  id m array-member? && if
	    #( snd j ( chn ) )
	    leave
	  then
	loop
      loop
    end-each
  then { mark-setting }
  mark-setting array? if
    mark-setting 0 array-ref { snd }
    mark-setting 1 array-ref { chn }
    #( #( 'mark id 'sound snd snd short-file-name 'channel chn ) ) { descr }
    0 snd chn edits each + end-each 1+ 1 ?do
      descr
      snd chn i marks id array-member? if
	id i mark-sample
      else
	#f
      then
      array-push drop
    loop
    descr
  else
    'no-such-mark #( get-func-name id ) fth-throw
  then
;

\ --- Mark Properties ---
: mark-properties ( id -- props )
  doc" Returns mark ID's entire property hash."
  { id }
  id mark? unless 'no-such-mark #( get-func-name id ) fth-throw then
  :mark-property id object-id property-ref
;
: set-mark-properties ( id props -- )
  { id props }
  id mark? unless 'no-such-mark #( get-func-name id ) fth-throw then
  :mark-property id object-id props property-set!
;
: mark-property ( id key -- val )
  doc" Returns the value associated with KEY in the given mark's property list, or #f."
  { id key }
  id mark? unless 'no-such-mark #( get-func-name id ) fth-throw then
  id mark-properties ?dup-if key hash-ref else #f then
;
: set-mark-property ( id key val -- )
  doc" Sets VAL to KEY in the given mark's property list."
  { id key val }
  id mark? unless 'no-such-mark #( get-func-name id ) fth-throw then
  id mark-properties ?dup-if
    key val hash-set!
  else
    id #{ key val } set-mark-properties
  then
;

hide
: save-mark-properties-cb <{ filename -- }>
  undef undef undef marks 0 array-ref 0 array-ref array? if
    filename :fam a/o io-open { io }
    io $" \n\\ from save-mark-properties in %s\n" _ #( *filename* ) io-write-format
    io $" require marks\n\n" io-write
    io $" let:\n" io-write
    io $"   nil nil { snd mk }\n" io-write
    undef undef undef marks each ( snd-m )
      each ( chn-m )
	each { m }
	  m mark-properties { mp }
	  mp if
	    m mark-home { mhome }
	    m undef mark-sample { msamp }
	    io $"   %S 0 find-sound to snd\n" #( mhome 0 array-ref file-name ) io-write-format
	    io $"   snd sound? if\n" io-write
	    io $"     %d snd %d find-mark to mk\n" #( msamp mhome 1 array-ref ) io-write-format
	    io $"     mk mark? if\n" io-write
	    io $"       mk %S set-mark-properties\n" #( mp ) io-write-format
	    io $"     then\n" io-write
	    io $"   then\n" io-write
	  then
	end-each
      end-each
    end-each
    io $" ;let\n" io-write
    io io-close
  then
;
set-current
: save-mark-properties ( -- )
  doc" Sets up an after-save-state-hook function to save any mark-properties."
  after-save-state-hook <'> save-mark-properties-cb add-hook!
;
previous

: mark-click-info <{ id -- #t }>
  doc" A mark-click-hook function that describes a mark and its properties.\n\
mark-click-hook <'> mark-click-info add-hook!"
  $"       mark id: %d\n" #( id ) string-format make-string-output-port { prt }
  id mark-name empty? unless prt $"          name: %s\n" #( id mark-name ) port-puts-format then
  prt $"        sample: %d (%.3f secs)\n"
  #( id undef mark-sample dup id mark-home 0 array-ref srate f/ ) port-puts-format
  id mark-sync if prt $"          sync: %s\n" #( id mark-sync ) port-puts-format then
  id mark-properties { props }
  props empty? unless prt $"    properties: %s" #( props ) port-puts-format then
  $" Mark Info" prt port->string info-dialog drop
  #t
;
\ mark-click-hook <'> mark-click-info add-hook!

\ This code saves mark info in the sound file header, and reads it
\ back in when the sound is later reopened.
: marks->string { sndf }
  $" \nrequire marks\n" make-string-output-port { prt }
  prt $" let:\n" port-puts
  prt $"   #f { mr }\n" port-puts
  sndf marks each ( chan-marks )
    prt $" \n  \\ channel %d\n" #( i ) port-puts-format
    each { m }
      m nil? ?leave
      $"   %s #f %d %S %d add-mark to mk\n"
      #( m undef mark-sample
	 j ( chn )
	 m mark-name length 0= if #f else m mark-name then
	 m mark-sync ) port-puts-format
      m mark-properties { props }
      props if $"   mk %S set-mark-properties\n" #( props ) port-puts-format then
    end-each
  end-each
  prt $" ;let\n" port-puts
  prt port->string
;

0 [if]
  output-comment-hook lambda: <{ str }> selected-sound marks->string ; add-hook!
  after-open-hook lambda: <{ snd -- }>
    snd comment ( str ) <'> string-eval #t nil fth-catch if ( str ) drop then
  ; add-hook!
[then]

\ marks.fs ends here
