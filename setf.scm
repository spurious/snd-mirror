;;; originally used setf, but new Guile allows us to use a generalized set! instead

(defmacro settable-0 (protected original setter)
  `(begin
     (define ,protected ,original)
     (define ,original
       (make-procedure-with-setter 
	(lambda () (,protected))
	(lambda (val) (,setter val))))))

(defmacro settable-1 (protected original setter)
  `(begin
     (define ,protected ,original)
     (define ,original
       (make-procedure-with-setter 
	(lambda val 
	  (,protected (if (null? val) #f (car val)) ))
	(lambda (arg1 . arg2) 
	  (,setter 
	   (if (null? arg2) arg1 (car arg2))
	   (if (null? arg2) #f arg1)))))))

(defmacro settable-2 (protected original setter)
  `(begin
     (define ,protected ,original)
     (define ,original
       (make-procedure-with-setter 
	(lambda args 
	  (,protected 
	   (if (null? args) #f (car args)) 
	   (if (or (null? args) (null? (cdr args))) 0 (cadr args))))
	(lambda (arg1 . argn)
	  (,setter 
	   (if (null? argn)
	       arg1
	       (if (null? (cdr argn))
		   (car argn)
		   (cadr argn)))
	   (if (null? argn)
	       #f
	       arg1)
	   (if (or (null? argn) (null? (cdr argn)))
	       0
	       (car argn))))))))

(settable-1 %amp amp set-amp)
(settable-0 %ask-before-overwrite ask-before-overwrite set-ask-before-overwrite)
(settable-0 %audio-output-device audio-output-device set-audio-output-device)
(settable-0 %audio-state-file audio-state-file set-audio-state-file)
(settable-0 %auto-resize auto-resize set-auto-resize)
(settable-0 %auto-update auto-update set-auto-update)
(settable-0 %axis-label-font axis-label-font set-axis-label-font)
(settable-0 %axis-numbers-font axis-numbers-font set-axis-numbers-font)
(settable-0 %basic-color basic-color set-basic-color)
(settable-0 %bold-button-font bold-button-font set-bold-button-font)
(settable-0 %button-font button-font set-button-font)
(settable-1 %channel-style channel-style set-channel-style)
(settable-0 %color-cutoff color-cutoff set-color-cutoff)
(settable-0 %color-inverted color-inverted set-color-inverted)
(settable-0 %color-scale color-scale set-color-scale)
(settable-0 %colormap colormap set-colormap)
(settable-1 %contrast contrast set-contrast)
(settable-1 %contrast-amp contrast-amp set-contrast-amp)
(settable-1 %contrast-func contrast-func set-contrast-func)
(settable-1 %contrasting contrasting set-contrasting)
(settable-0 %corruption-time corruption-time set-corruption-time)
(settable-2 %cursor cursor set-cursor)
(settable-0 %cursor-color cursor-color set-cursor-color)
(settable-2 %cursor-follows-play cursor-follows-play set-cursor-follows-play)
(settable-2 %cursor-style cursor-style set-cursor-style)
(settable-0 %dac-folding dac-folding set-dac-folding)
(settable-0 %dac-size dac-size set-dac-size)
(settable-0 %data-clipped data-clipped set-data-clipped)
(settable-0 %data-color data-color set-data-color)
(settable-0 %default-output-chans default-output-chans set-default-output-chans)
(settable-0 %default-output-format default-output-format set-default-output-format)
(settable-0 %default-output-srate default-output-srate set-default-output-srate)
(settable-0 %default-output-type default-output-type set-default-output-type)
(settable-2 %dot-size dot-size set-dot-size)
(settable-0 %enved-base enved-base set-enved-base)
(settable-0 %enved-clipping enved-clipping set-enved-clipping)
(settable-0 %enved-dBing enved-dBing set-enved-dBing)
(settable-0 %enved-exping enved-exping set-enved-exping)
(settable-0 %enved-power enved-power set-enved-power)
(settable-0 %enved-target enved-target set-enved-target)
(settable-0 %enved-waveform-color enved-waveform-color set-enved-waveform-color)
(settable-0 %enved-waving enved-waving set-enved-waving)
(settable-0 %eps-file eps-file set-eps-file)
(settable-1 %expand expand set-expand)
(settable-1 %expand-funcs expand-funcs set-expand-funcs)
(settable-1 %expand-hop expand-hop set-expand-hop)
(settable-1 %expand-length expand-length set-expand-length)
(settable-1 %expand-ramp expand-ramp set-expand-ramp)
(settable-1 %expanding expanding set-expanding)
(settable-2 %fft-beta fft-beta set-fft-beta)
(settable-2 %fft-log-frequency fft-log-frequency set-fft-log-frequency)
(settable-2 %fft-log-magnitude fft-log-magnitude set-fft-log-magnitude)
(settable-2 %fft-size fft-size set-fft-size)
(settable-2 %fft-style fft-style set-fft-style)
(settable-2 %fft-window fft-window set-fft-window)
(settable-2 %ffting ffting set-ffting)
(settable-1 %filter-dBing filter-dBing set-filter-dBing)
(settable-1 %filter-env filter-env set-filter-env)
(settable-1 %filter-env-order filter-env-order set-filter-env-order)
(settable-1 %filter-order filter-order set-filter-order)
(settable-0 %filter-waveform-color filter-waveform-color set-filter-waveform-color)
(settable-1 %filtering filtering set-filtering)
(settable-0 %fit-data-on-open fit-data-on-open set-fit-data-on-open)
(settable-0 %graph-color graph-color set-graph-color)
(settable-0 %graph-cursor graph-cursor set-graph-cursor)
(settable-2 %graph-style graph-style set-graph-style)
(settable-2 %graphing graphing set-graphing)
(settable-2 %graphs-horizontal graphs-horizontal set-graphs-horizontal)
(settable-0 %help-text-font help-text-font set-help-text-font)
(settable-0 %highlight-color highlight-color set-highlight-color)
(settable-0 %initial-x0 initial-x0 set-initial-x0)
(settable-0 %initial-x1 initial-x1 set-initial-x1)
(settable-0 %initial-y0 initial-y0 set-initial-y0)
(settable-0 %initial-y1 initial-y1 set-initial-y1)
(settable-2 %left-sample left-sample set-left-sample)
(settable-2 %line-size line-size set-line-size)
(settable-0 %listener-color listener-color set-listener-color)
(settable-0 %listener-font listener-font set-listener-font)
(settable-0 %listener-prompt listener-prompt set-listener-prompt)
(settable-1 %mark-color mark-color set-mark-color)
(settable-1 %mark-name mark-name set-mark-name)
(settable-1 %mark-sample mark-sample set-mark-sample)
(settable-1 %mark-sync mark-sync set-mark-sync)
(settable-2 %max-fft-peaks max-fft-peaks set-max-fft-peaks)
(settable-0 %max-regions max-regions set-max-regions)
(settable-2 %min-dB min-dB set-min-dB)
(settable-1 %mix-amp mix-amp set-mix-amp)
(settable-1 %mix-amp-env mix-amp-env set-mix-amp-env)
(settable-1 %mix-anchor mix-anchor set-mix-anchor)
(settable-1 %mix-color mix-color set-mix-color)
(settable-1 %mix-console-amp-scaler mix-console-amp-scaler set-mix-console-amp-scaler)
(settable-1 %mix-console-speed-scaler mix-console-speed-scaler set-mix-console-speed-scaler)
(settable-1 %mix-console-state mix-console-state set-mix-console-state)
(settable-1 %mix-console-y mix-console-y set-mix-console-y)
(settable-0 %mix-focus-color mix-focus-color set-mix-focus-color)
(settable-1 %mix-track mix-track set-mix-track)
(settable-1 %mix-length mix-length set-mix-length)
(settable-1 %mix-locked mix-locked set-mix-locked)
(settable-1 %mix-name mix-name set-mix-name)
(settable-1 %mix-position mix-position set-mix-position)
(settable-1 %mix-speed mix-speed set-mix-speed)
(settable-0 %mix-waveform-color mix-waveform-color set-mix-waveform-color)
(settable-1 %mix-waveform-height mix-waveform-height set-mix-waveform-height)
(settable-0 %movies movies set-movies)
(settable-2 %normalize-fft normalize-fft set-normalize-fft)
(settable-0 %normalize-on-open normalize-on-open set-normalize-on-open)
(settable-0 %position-color position-color set-position-color)
(settable-0 %prefix-arg prefix-arg set-prefix-arg)
(settable-0 %previous-files-sort previous-files-sort set-previous-files-sort)
(settable-0 %print-length print-length set-print-length)
(settable-0 %pushed-button-color pushed-button-color set-pushed-button-color)
(settable-0 %raw-chans raw-chans set-raw-chans)
(settable-0 %raw-format raw-format set-raw-format)
(settable-0 %raw-srate raw-srate set-raw-srate)
(settable-1 %read-only read-only set-read-only)
(settable-0 %recorder-autoload recorder-autoload set-recorder-autoload)
(settable-0 %recorder-buffer-size recorder-buffer-size set-recorder-buffer-size)
(settable-0 %recorder-file recorder-file set-recorder-file)
(settable-0 %recorder-gain recorder-gain set-recorder-gain)
(settable-0 %recorder-in-amp recorder-in-amp set-recorder-in-amp)
(settable-0 %recorder-in-format recorder-in-format set-recorder-in-format)
(settable-0 %recorder-max-duration recorder-max-duration set-recorder-max-duration)
(settable-0 %recorder-out-amp recorder-out-amp set-recorder-out-amp)
(settable-0 %recorder-out-chans recorder-out-chans set-recorder-out-chans)
(settable-0 %recorder-out-format recorder-out-format set-recorder-out-format)
(settable-0 %recorder-srate recorder-srate set-recorder-srate)
(settable-0 %recorder-trigger recorder-trigger set-recorder-trigger)
(settable-1 %reverb-decay reverb-decay set-reverb-decay)
(settable-1 %reverb-feedback reverb-feedback set-reverb-feedback)
(settable-1 %reverb-funcs reverb-funcs set-reverb-funcs)
(settable-1 %reverb-length reverb-length set-reverb-length)
(settable-1 %reverb-lowpass reverb-lowpass set-reverb-lowpass)
(settable-1 %reverb-scale reverb-scale set-reverb-scale)
(settable-1 %reverbing reverbing set-reverbing)
(settable-2 %right-sample right-sample set-right-sample)
(settable-2 %sample sample set-sample)
(settable-0 %save-dir save-dir set-save-dir)
(settable-0 %save-state-file save-state-file set-save-state-file)
(settable-0 %save-state-on-exit save-state-on-exit set-save-state-on-exit)
(settable-0 %selected-data-color selected-data-color set-selected-data-color)
(settable-0 %selected-graph-color selected-graph-color set-selected-graph-color)
(settable-0 %selection-color selection-color set-selection-color)
(settable-2 %show-axes show-axes set-show-axes)
(settable-2 %show-fft-peaks show-fft-peaks set-show-fft-peaks)
(settable-2 %show-marks show-marks set-show-marks)
(settable-2 %show-mix-consoles show-mix-consoles set-show-mix-consoles)
(settable-2 %show-mix-waveforms show-mix-waveforms set-show-mix-waveforms)
(settable-2 %show-selection-transform show-selection-transform set-show-selection-transform)
(settable-0 %show-usage-stats show-usage-stats set-show-usage-stats)
(settable-2 %show-y-zero show-y-zero set-show-y-zero)
(settable-1 %showing-controls showing-controls set-showing-controls)
(settable-0 %sinc-width sinc-width set-sinc-width)
(settable-2 %spectro-cutoff spectro-cutoff set-spectro-cutoff)
(settable-2 %spectro-hop spectro-hop set-spectro-hop)
(settable-2 %spectro-start spectro-start set-spectro-start)
(settable-2 %spectro-x-angle spectro-x-angle set-spectro-x-angle)
(settable-2 %spectro-x-scale spectro-x-scale set-spectro-x-scale)
(settable-2 %spectro-y-angle spectro-y-angle set-spectro-y-angle)
(settable-2 %spectro-y-scale spectro-y-scale set-spectro-y-scale)
(settable-2 %spectro-z-angle spectro-z-angle set-spectro-z-angle)
(settable-2 %spectro-z-scale spectro-z-scale set-spectro-z-scale)
(settable-1 %speed speed set-speed)
(settable-1 %speed-style speed-style set-speed-style)
(settable-1 %speed-tones speed-tones set-speed-tones)
(settable-2 %squelch-update squelch-update set-squelch-update)
(settable-1 %syncing syncing set-syncing)
(settable-0 %temp-dir temp-dir set-temp-dir)
(settable-0 %text-focus-color text-focus-color set-text-focus-color)
(settable-0 %tiny-font tiny-font set-tiny-font)
(settable-2 %transform-type transform-type set-transform-type)
(settable-0 %trap-segfault trap-segfault set-trap-segfault)
(settable-1 %uniting uniting set-uniting)
(settable-0 %use-raw-defaults use-raw-defaults set-use-raw-defaults)
(settable-2 %use-sinc-interp use-sinc-interp set-use-sinc-interp)
(settable-2 %verbose-cursor verbose-cursor set-verbose-cursor)
(settable-0 %vu-font vu-font set-vu-font)
(settable-0 %vu-font-size vu-font-size set-vu-font-size)
(settable-0 %vu-size vu-size set-vu-size)
(settable-2 %wavelet-type wavelet-type set-wavelet-type)
(settable-2 %waving waving set-waving)
(settable-2 %wavo wavo set-wavo)
(settable-2 %wavo-hop wavo-hop set-wavo-hop)
(settable-2 %wavo-trace wavo-trace set-wavo-trace)
(settable-0 %window-height window-height set-window-height)
(settable-0 %window-width window-width set-window-width)
(settable-0 %window-x window-x set-window-x)
(settable-0 %window-y window-y set-window-y)
(settable-2 %with-mix-consoles with-mix-consoles set-with-mix-consoles)
(settable-2 %x-axis-style x-axis-style set-x-axis-style)
(settable-2 %x-bounds x-bounds set-x-bounds)
(settable-2 %y-bounds y-bounds set-y-bounds)
(settable-2 %zero-pad zero-pad set-zero-pad)
(settable-0 %zoom-color zoom-color set-zoom-color)
(settable-0 %zoom-focus-style zoom-focus-style set-zoom-focus-style)
