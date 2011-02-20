\ hooks.fs -- hooks.scm -> hooks.fs

\ Author: Michael Scholz <mi-scholz@users.sourceforge.net>
\ Created: Tue Aug 08 23:27:50 CEST 2006
\ Changed: Sat Feb 19 17:27:17 CET 2011

\ snd-hooks         Array with all Snd hooks.
\ reset-all-hooks   ( -- )
\ with-local-hook   ( hook local-hook-procs thunk -- result )

require examp

[defined] after-apply-controls-hook [if]
  #( after-apply-controls-hook
     after-graph-hook
     after-lisp-graph-hook
     after-open-hook
     after-save-as-hook
     after-save-state-hook
     after-transform-hook
     bad-header-hook
     before-close-hook
     before-exit-hook
     before-save-as-hook
     before-save-state-hook
     before-transform-hook
     clip-hook
     close-hook
     color-hook
     dac-hook
     draw-mark-hook
     draw-mix-hook
     drop-hook
     during-open-hook
     effects-hook
     enved-hook
     exit-hook
     graph-hook
     help-hook
     initial-graph-hook
     key-press-hook
     lisp-graph-hook
     listener-click-hook
     mark-click-hook
     mark-drag-hook
     mark-hook
     mix-click-hook
     mix-drag-hook
     mix-release-hook
     mouse-click-hook
     mouse-drag-hook
     mouse-enter-graph-hook
     mouse-enter-label-hook
     mouse-enter-listener-hook
     mouse-enter-text-hook
     mouse-leave-graph-hook
     mouse-leave-label-hook
     mouse-leave-listener-hook
     mouse-leave-text-hook
     mouse-press-hook
     mus-error-hook
     name-click-hook
     new-sound-hook
     new-widget-hook
     open-hook
     open-raw-sound-hook
     optimization-hook
     orientation-hook
     output-comment-hook
     output-name-hook
     peak-env-hook
     play-hook
     print-hook
     read-hook
     save-hook
     save-state-hook
     select-channel-hook
     select-sound-hook
     snd-error-hook
     snd-warning-hook
     start-hook
     start-playing-hook
     start-playing-selection-hook
     stop-dac-hook
     stop-playing-hook
     stop-playing-selection-hook
     update-hook
     view-files-select-hook )
[else]
  #()
[then] constant snd-hooks

: reset-all-hooks ( -- )
  doc" Removes all Snd hook functions."
  snd-hooks each ( hook ) dup hook? if reset-hook! else drop then end-each
  nil nil nil { lst snd chn }
  all-chans each to lst
    lst 0 array-ref to snd
    lst 1 array-ref to chn
    snd chn edit-hook       dup hook? if reset-hook! else drop then
    snd chn after-edit-hook dup hook? if reset-hook! else drop then
    snd chn undo-hook       dup hook? if reset-hook! else drop then
  end-each
;

: with-local-hook <{ hook local-hook-procs thunk -- }>
  doc" Evaluates THUNK (an xt) with HOOK set to LOCAL-HOOK-PROCS (an array of procs), \
then restores HOOK to its previous state."
  hook hook?              hook             1 $" a hook"       assert-type
  local-hook-procs array? local-hook-procs 2 $" a array"      assert-type
  thunk word?             thunk            3 $" a proc or xt" assert-type
  hook hook->array { old-procs }
  hook reset-hook!
  local-hook-procs each ( proc ) hook swap add-hook! end-each
  thunk '() run-proc drop
  hook reset-hook!
  old-procs each ( proc ) hook swap add-hook! end-each
;

\ hooks.fs ends here
