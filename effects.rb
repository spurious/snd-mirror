# effects.rb -- Guile -> Ruby translation

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Fri Feb 07 23:56:21 CET 2003
# Last: Sun Feb 29 00:14:01 CET 2004

# Commentary:
#
# Requires --with-moitf or --with-gtk and module libxm.so or --with-static-xm!
#
# Tested with Snd 7.3, Motif 2.2.2, Gtk+ 2.2.1, Ruby 1.6.6, 1.6.8 and 1.9.0.
#
# module Effects (see new-effects.scm)
#  plausible_mark_samples
#  map_chan_over_target_with_sync(target, origin, decay) do |in| ... end
#  effect_frames(target)
#  scale_envelope(e, scl)
#  squelch_one_channel(silence, snd, chn, omit_silence)
#  flecho_1(scaler, secs, in_samps)
#  zecho_1(scaler, secs, frq, amp, in_samps)
#  comb_filter(scaler, size)
#  comb_chord(scaler, size, amp, interval_one, interval_two)
#  moog(freq, q)
#  jc_reverb_1(in_samps)
#  cnvtest(snd0, snd1, amp)
#  place_sound(mono_snd, stereo_snd, pan_env)
#  cross_synthesis(cross_snd, amp, fftsize, r)
#  fp_1(sr, osamp, osfrq, beg, fin)
#  hello_dentist_1(frq, amp, beg, fin)

# Code:

require "examp"
include Dsp, Moog
require "snd-xm"
include Snd_XM
require "env"
include Env
require "extensions"
require "xm-enved"
require "rubber"
include Rubber
include Math
require "hooks"

unless $selection_changed_hook.member?("selection-buttons-effects-hook")
  $selection_changed_hook.add_hook!("selection-buttons-effects-hook") do | |
    flag = selection?
    $selection_buttons.each do |w| set_sensitive(w, flag) end
  end
  
  mark_hook_proc = lambda do
    flag = marks?
    $mark_buttons.each do |w| set_sensitive(w, flag) end
  end
  
  $mark_hook.add_hook!("mark-buttons-effects-hook") do |id, snd, chn, reason|
    mark_hook_proc.call
  end

  $after_graph_hook.add_hook!("mark-buttons-effects-hook") do |snd, chn|
    mark_hook_proc.call
  end
end

module Effects
  def plausible_mark_samples
    snd = selected_sound
    chn = selected_channel
    if ms = marks(snd, chn)
      ms = ms.map do |x| mark_sample(x) end.sort
      if ms.length < 2
        snd_warning("no-such-mark: mark-related action requires two marks")
        false
      elsif ms.length == 2
        ms
      else
        lw = left_sample(snd, chn)
        rw = right_sample(snd, chn)
        cw = cursor(snd, chn)
        favor = if cw >= lw and cw <= rw
                  cw
                else
                  0.5 * (lw + rw)
                end
        centered_points = lambda do |points|
          if points.length == 2
            points
          else
            p1, p2, p3 = points[0, 3]
            if (p1 - favor).abs < (p3 - favor).abs
              [p1, p2]
            else
              centered_points.call(points[1..-1])
            end
          end
        end
        centered_points.call(ms)
      end
    else
      false
    end
  end

  def map_chan_over_target_with_sync(target = nil, origin = nil, decay = nil, &func)
    doc("map_chan_over_target_with_sync(target, origin, decay) do |in| ... end
target: 'marks -> beg=closest marked sample, dur=samples to next mark
y        'sound -> beg=0, dur=all samples in sound
        'selection -> beg=selection-position, dur=selection-frames
        'cursor -> beg=cursor, dur=samples to end of sound
decay is how long to run the effect past the end of the sound\n") if target == :help
    snc = sync()
    ssnd = selected_sound
    schn = selected_channel
    ms = (target == :marks and plausible_mark_samples)
    beg = case target
          when :sound
            0
          when :selection
            selection_position
          when :cursor
            cursor(selected_sound, selected_channel)
          else
            (ms ? ms[0] : 0)
          end
    overlap = (decay ? (srate().to_f * decay).round : 0)
    sndlst, chnlst = (snc > 0 ? all_chans() : [[ssnd], [schn]])
    sndlst.zip(chnlst) do |snd, chn|
      fin = if target == :sound or target == :cursor
              frames(snd, chn) - 1
            elsif target == :selection and selection?
              selection_position + selection_frames
            else
              (ms ? ms[1] : 0)
            end
      if sync(snd) == snc
        map_chan(func.call(fin - beg), beg, fin + overlap, origin, snd, chn)
      end
    end
  end

  def effect_frames(target)
    case target
    when :sound
      frames() - 1
    when :selection
      selection_frames
    else
      if ms = plausible_mark_samples
        y = ms.shift
        ms.each do |x| y -= x end
        1 + y.abs
      else
        1
      end
    end
  end
  
  def squelch_one_channel(silence, snd, chn, omit_silence)
    buffer_size = 128
    buffer0 = false
    sum0 = 0.0
    buffer1 = make_vct(buffer_size)
    chan_samples = frames(snd, chn)
    pad_samples = chan_samples + buffer_size
    tempfilename = snd_tempnam
    new_file = open_sound_file(tempfilename, 1, srate(snd))
    reader = make_sample_reader(0, snd, chn)
    buffers_per_progress_report = (chan_samples / (buffer_size * 20.0)).round
    start_progress_report(snd)
    k = 0
    0.step(pad_samples - 1, buffer_size) do |i|
      sum = 0.0
      buffer_size.times do |j|
        val = next_sample(reader)
        sum += (val * val)
        buffer1[j] = val
      end
      if buffer0
        all_zeros = false
        if sum > silence
          if sum0 <= silence
            incr = 0.0
            buffer_size.times do |j|
              buffer0[j] *= incr
              incr += (1.0 / buffer_size)
            end
          end
        else
          if sum0 <= silence
            vct_fill!(buffer0, 0.0)
            all_zeros = true
            incr = 1.0
            buffer_size.times do |j|
              buffer0[j] *= incr
              incr -= (1.0 / buffer_size)
            end
          end
        end
        unless omit_silence and all_zeros
          vct2sound_file(new_file, buffer0, buffer_size)
        end
      else
        buffer0 = make_vct(buffer_size)
      end
      k += 1
      if k >= buffers_per_progress_report
        k = 0
        progress_report(i / pad_samples.to_f, "squelch-one-channel", chn, 1, snd)
      end
      buffer0, buffer1 = buffer1, buffer0
      sum0 = sum
    end
    finish_progress_report(snd)
    free_sample_reader(reader)
    close_sound_file(new_file, chan_samples * 4)
    set_samples(0, chan_samples, tempfilename, snd, chn)
  end

  def flecho_1(scaler, secs, in_samps)
    flt = make_fir_filter(:order, 4, :xcoeffs, list2vct([0.125, 0.25, 0.25, 0.125]))
    del = make_delay((secs * srate()).round)
    samp = 0
    lambda do |inval|
      samp += 1
      inval + delay(del, fir_filter(flt, scaler * (tap(del) + (samp <= in_samps ? inval : 0.0))))
    end
  end

  def zecho_1(scaler, secs, frq, amp, in_samps)
    os = make_oscil(frq)
    len = (secs.to_f * srate()).round
    del = make_delay(len, false, 0.0, (len + amp + 1).round)
    samp = 0
    lambda do |inval|
      samp += 1
      inval + delay(del, scaler * (tap(del) + (samp <= in_samps ? inval : 0.0)), amp * oscil(os))
    end
  end
  
  def comb_filter(scaler, size)
    delay_line = Array.new(size, 0.0)
    delay_loc = 0
    lambda do |x|
      result = delay_line[delay_loc]
      delay_line[delay_loc] = x + scaler * result
      delay_loc += 1
      delay_loc = 0 if delay_loc == size
      result
    end
  end
  
  def comb_chord(scaler, size, amp, interval_one, interval_two)
    c1 = make_comb(scaler, size)
    c2 = make_comb(scaler, (size * interval_one).round)
    c3 = make_comb(scaler, (size * interval_two).round)
    lambda do |x|
      amp * (comb(c1, x) + comb(c2, x) + comb(c3, x))
    end
  end
  
  def moog(freq, q)
    gen = make_moog_filter(freq, q)
    lambda do |inval| moog_filter(gen, inval) end
  end

  def jc_reverb_1(in_samps, volume)
    allpass1 = make_all_pass(-0.7, 0.7, 1051)
    allpass2 = make_all_pass(-0.7, 0.7, 337)
    allpass3 = make_all_pass(-0.7, 0.7, 113)
    comb1 = make_comb(0.742, 4799)
    comb2 = make_comb(0.733, 4999)
    comb3 = make_comb(0.715, 5399)
    comb4 = make_comb(0.697, 5801)
    outdel1 = make_delay((0.013 * srate()).round)
    comb_sum = 0.0
    comb_sum_1 = 0.0
    comb_sum_2 = 0.0
    samp = 0
    lambda do |inval|
      allpass_sum = all_pass(allpass3,
                             all_pass(allpass2,
                                      all_pass(allpass1,
                                               (samp < in_samps ? inval : 0.0))))
      samp += 1
      comb_sum_2 = comb_sum_1
      comb_sum_1 = comb_sum
      comb_sum = (comb(comb1, allpass_sum) + comb(comb2, allpass_sum) + \
                  comb(comb3, allpass_sum) + comb(comb4, allpass_sum))
      inval + volume * delay(outdel1, comb_sum)
    end
  end

  def cnvtest(snd0, snd1, amp)
    flt_len = frames(snd0)
    total_len = flt_len + frames(snd1)
    cnv = make_convolve(:filter, channel2vct(0, flt_len, snd0))
    sf = make_sample_reader(0, snd1)
    out_data = make_vct(total_len)
    vct_map!(out_data, lambda do || convolve(cnv, lambda do |dir| next_sample(sf) end) end)
    free_sample_reader(sf)
    vct_scale!(out_data, amp)
    max_samp = vct_peak(out_data)
    vct2channel(out_data, 0, total_len, snd1)
    set_y_bounds([-max_samp, max_samp], snd1) if max_samp > 1.0
    max_samp
  end

  def place_sound(mono_snd, stereo_snd, pan_env)
    if !sound?(mono_snd) or !sound?(stereo_snd) or
        channels(mono_snd) != 1 or channels(stereo_snd) != 2
        snd_warning("no mono-snd or stereo-snd")
    else
      len = frames(mono_snd)
      unless array?(pan_env) or vct?(pan_env)
        pos = pan_env / 90.0
        reader0 = make_sample_reader(0, mono_snd)
        reader1 = make_sample_reader(0, mono_snd)
        map_channel(lambda do |y|
                      y + pos * read_sample(reader1)
                    end, 0, len, stereo_snd, 1)
        map_channel(lambda do |y|
                      y + (1.0 - pos) * read_sample(reader0)
                    end, 0, len, stereo_snd, 0)
      else
        e0 = make_env(pan_env, :end, len - 1)
        e1 = make_env(pan_env, :end, len - 1)
        reader0 = make_sample_reader(0, mono_snd)
        reader1 = make_sample_reader(0, mono_snd)
        map_channel(lambda do |y|
                      y + env(e1) * read_sample(reader1)
                    end, 0, len, stereo_snd, 1)
        map_channel(lambda do |y|
                      y + (1.0 - env(e0)) * read_sample(reader0)
                    end, 0, len, stereo_snd, 0)
      end
    end
  end

  def cross_synthesis(cross_snd, amp, fftsize, r)
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r / fftsize
    bin = srate() / fftsize
    formants = Array.new(freq_inc)
    (0...freq_inc).each do |i|
      formants[i] = make_formant(radius, i * bin)
    end
    lambda do |inval|
      outval = 0.0
      if ctr == freq_inc
        fdr = channel2vct(inctr, fftsize, cross_snd, 0)
        inctr += freq_inc
        spectrum(fdr, fdi, false, 2)
        vct_subtract!(fdr, spectr)
        vct_scale!(fdr, 1.0 / freq_inc)
        ctr = 0
      end
      ctr += 1
      vct_add!(spectr, fdr)
      amp * formant_bank(spectr, formants, inval)
    end
  end

  def fp_1(sr, osamp, osfrq, beg, fin)
    os = make_oscil(osfrq)
    sr = make_src(:srate, sr)
    len = fin - beg + 1
    sf = make_sample_reader(beg)
    out_data = make_vct(len)
    vct_map!(out_data,
             lambda do | |
               src(sr, osamp * oscil(os),
                   lambda do |dir|
                     if dir > 0
                       next_sample(sf)
                     else
                       previous_sample(sf)
                     end
                   end)
             end)
    free_sample_reader(sf)
    vct2channel(out_data, beg, len)
  end
  
  def hello_dentist_1(frq, amp, beg, fin)
    rn = make_rand_interp(:frequency, frq, :amplitude, amp)
    i = j = 0
    len = (fin - beg) + 1
    in_data = channel2vct(beg, len)
    out_len = (len * (1.0 + 2 * amp)).round
    out_data = make_vct(out_len)
    rd = make_src(:srate, 1.0, :input, lambda do |dir|
                    val = ((i >= 0 and i < len) ? vct_ref(in_data, i) : 0.0)
                    i += dir
                    val
                  end)
    until i == len or j == out_len
      vct_set!(out_data, j, src(rd, rand_interp(rn)))
      j += 1
    end
    vct2channel(out_data, beg, j)
  end
  
  #
  # --- Amplitude Effects ---
  #
  class Gain
    def initialize(label)
      @label = label
      @amount = 1.0
      @dlg = nil
      @target = :sound
      @envelope = nil
    end

    def inspect
      format("%s (%1.2f)", @label, @amount)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 1.0
        sliders = Array.new(1)
        fr = nil
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the gain scaling amount",
                           :reset_cb, lambda do |w, c, i|
                             @envelope.envelope = [0.0, 1.0, 1.0, 1.0]
                             set_scale_value(sliders[0].scale, @amount = init_amount, 100.0)
                           end) do |w, c, i|
          with_env = ((@envelope.envelope != [0.0, 1.0, 1.0, 1.0]) and @envelope.scale(@amount))
          case @target
          when :sound
            with_env ? env_sound(with_env) : scale_by(@amount)
          when :selection
            if selection?
              with_env ? env_selection(with_env) : scale_selection_by(@amount)
            else
              snd_warning("no selection")
            end
          else
            if pts = plausible_mark_samples
              if with_env
                env_sound(with_env, pts[0], pts[1] - pts[0])
              else
                scale_sound_by(@amount, pts[0], pts[1] - pts[0])
              end
            end
          end
        end
        sliders[0] = @dlg.add_slider("gain", 0.0, init_amount, 5.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
        if provided? "xm"
          frame = @dlg.add_frame([RXmNheight, 200])
          @dlg.add_target() do |t| @target = t end
        else
          frame = @dlg.parent
        end
        activate_dialog(@dlg.dialog)
        @envelope = make_xenved("gain", frame,
                                :envelope, [0.0, 1.0, 1.0, 1.0],
                                :axis_bounds, [0.0, 1.0, 0.0, 1.0])
        if provided? "xg"
          @dlg.add_target() do |t| @target = t end
        end
      else
        activate_dialog(@dlg.dialog)
      end
    end
  end

  class Normalize
    def initialize(label)
      @label = label
      @amount = 1.0
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f)", @label, @amount)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 1.0
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Normalize scales amplitude to the normalize amount. \
Move the slider to change the scaling amount.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @amount = init_amount, 100.0)
                           end) do |w, c, i|
          case @target
          when :sound
            scale_to(@amount)
          when :selection
            selection? ? scale_selection_to(@amount) : snd_warning("no selection")
          else
            if pts = plausible_mark_samples
              scale_sound_to(@amount, pts[0], pts[1] - pts[0])
            end
          end
        end
        sliders[0] = @dlg.add_slider("normalize", 0.0, init_amount, 1.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Gate
    def initialize(label)
      @label = label
      @amount = 1.0
      @dlg = nil
      @omit_silence = false
    end

    def inspect
      format("%s (%1.2f)", @label, @amount)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 1.0
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the gate intensity. \
Higher values gate more of the sound.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @amount = init_amount, 100.0)
                           end) do |w, c, i|
          if (snc = sync()) > 0
            sndlst, chnlst = all_chans()
            sndlst.zip(chnlst) do |snd, chn|
              squelch_one_channel(@amount, snd, chn, @omit_silence) if sync(snd) == snc
            end
          else
            squelch_one_channel(@amount, selected_sound, selected_channel, @omit_silence)
          end
        end
        sliders[0] = @dlg.add_slider("gate", 0.0, init_amount, 5.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
        @dlg.add_toggle("Omit silence", @omit_silence) do |t| @omit_silence = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  #
  # --- Delay Effects ---
  #
  class Echo
    def initialize(label)
      @label = label
      @delay_time = 0.5
      @amount = 0.2
      @dlg = nil
      @target = :sound
      @truncate = true
    end

    def inspect
      format("%s (%1.2f %1.2f)", @label, @delay_time, @amount)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_delay_time = 0.5
        init_amount = 0.2
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "The sliders change the delay time and echo amount.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @delay_time = init_delay_time, 100.0)
                             set_scale_value(sliders[1].scale, @amount = init_amount, 100.0)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "echo", (!@truncate and 4 * @delay_time)) do |s|
            size = (@delay_time * srate()).round
            d = make_delay(size)
            samp = 0
            lambda do |inval|
              samp += 1
              t = if samp <= s
                    inval
                  else
                    0.0
                  end
              inval + delay(d, @amount * (tap(d) + t))
            end
          end 
        end
        sliders[0] = @dlg.add_slider("delay time", 0.0, init_delay_time, 2.0, 100) do |w, c, i|
          @delay_time = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("echo amount", 0.0, init_amount, 1.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
        @dlg.add_toggle() do |t| @truncate = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Filtered_echo
    def initialize(label)
      @label = label
      @scaler = 0.5
      @delay = 0.9
      @dlg = nil
      @target = :sound
      @truncate = true
    end

    def inspect
      format("%s (%1.2f %1.2f)", @label, @scaler, @delay)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_scaler = 0.5
        init_del = 0.9
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "Move the sliders to the filter scaler and the \
delay time in seconds.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @scaler = init_scaler, 100.0)
                             set_scale_value(sliders[1].scale, @delay = init_del, 100.0)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "flecho", (!@truncate and 4 * @delay)) do |s|
            flecho_1(@scaler, @delay, s)
          end
        end
        sliders[0] = @dlg.add_slider("filter scaler", 0.0, init_scaler, 1.0, 100) do |w, c, i|
          @scaler = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("delay time (secs)", 0.0, init_del, 3.0, 100) do |w, c, i|
          @delay = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
        @dlg.add_toggle() do |t| @truncate = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Modulated_echo
    def initialize(label)
      @label = label
      @scaler = 0.5
      @delay = 0.75
      @freq = 6
      @amp = 10.0
      @dlg = nil
      @target = :sound
      @truncate = true
    end

    def inspect
      format("%s (%1.2f %1.2f %1.2f %1.2f)", @label, @scaler, @delay, @freq, @amp)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_scaler = 0.5
        init_del = 0.75
        init_freq = 6
        init_amp = 10.0
        sliders = Array.new(4)
        @dlg = make_dialog(@label,
                           :info, "Move the sliders to set the echo scaler, \
the delay time in seconds, the modulation frequency, and the echo amplitude.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @scaler = init_scaler, 100.0)
                             set_scale_value(sliders[1].scale, @delay = init_del, 100.0)
                             set_scale_value(sliders[2].scale, @freq = init_freq, 100.0)
                             set_scale_value(sliders[3].scale, @amp = init_amp, 100.0)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "zecho", (!@truncate and 4 * @delay)) do |s|
            zecho_1(@scaler, @delay, @freq, @amp, s)
          end
        end
        sliders[0] = @dlg.add_slider("echo scaler", 0.0, init_scaler, 1.0, 100) do |w, c, i|
          @scaler = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("delay time (secs)", 0.0, init_del, 3.0, 100) do |w, c, i|
          @delay = get_scale_value(w, i, 100.0)
        end
        sliders[2] = @dlg.add_slider("modulation frequency",
                                     0.0, init_freq, 100.0, 100) do |w, c, i|
          @freq = get_scale_value(w, i, 100.0)
        end
        sliders[3] = @dlg.add_slider("modulation amplitude",
                                     0.0, init_amp, 100.0, 100) do |w, c, i|
          @amp = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
        @dlg.add_toggle() do |t| @truncate = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  #
  # --- Filters ---
  #
  class Band_pass
    def initialize(label)
      @label = label
      @freq = 1000
      @bw = 100
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %d)", @label, @freq, @bw)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_freq = 1000
        init_bw = 100
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "Butterworth band-pass filter. \
Move the slider to change the center frequency and bandwidth.",
                           :reset_cb, lambda do |w, c, i|
                             @freq = init_freq
                             set_scale_value(sliders[0].scale, scale_log2linear(20, @freq, 22050))
                             if provided? "xm"
                               change_label(sliders[0].label, "%1.2f" % @freq)
                             end
                             set_scale_value(sliders[1].scale, @bw = init_bw)
                           end) do |w, c, i|
          case @target
          when :sound
            filter_sound(make_butter_band_pass(@freq, @bw))
          when :selection
            filter_selection(make_butter_band_pass(@freq, @bw))
          end
        end
        sliders[0] = @dlg.add_slider("center frequency",
                                     20, init_freq, 22050, 1, :log) do |w, c, i|
          @freq = if provided? "xm"
                    scale_linear2log(20, Rvalue(i), 22050)
                  else
                    scale_linear2log(20, Rvalue(RGTK_ADJUSTMENT(w)), 22050)
                  end
        end
        sliders[1] = @dlg.add_slider("bandwidth", 0, init_bw, 1000) do |w, c, i|
          @bw = get_scale_value(w, i)
        end
        @dlg.add_target([["entire sound", :sound, true],
                         ["selection", :selection, false]]) do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Band_reject
    def initialize(label)
      @label = label
      @freq = 100
      @bw = 100
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %d)", @label, @freq, @bw)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_freq = 100
        init_bw = 100
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "Butterworth band-reject filter. \
Move the slider to change the center frequency and bandwidth.",
                           :reset_cb, lambda do |w, c, i|
                             @freq = init_freq
                             set_scale_value(sliders[0].scale, scale_log2linear(20, @freq, 22050))
                             if provided? "xm"
                               change_label(sliders[0].label, "%1.2f" % @freq)
                             end
                             set_scale_value(sliders[1].scale, @bw = init_bw)
                           end) do |w, c, i|
          case @target
          when :sound
            filter_sound(make_butter_band_reject(@freq, @bw))
          when :selection
            filter_selection(make_butter_band_reject(@freq, @bw))
          end
        end
        sliders[0] = @dlg.add_slider("center frequency",
                                     20, init_freq, 22050, 1, :log) do |w, c, i|
          @freq = if provided? "xm"
                    scale_linear2log(20, Rvalue(i), 22050)
                  else
                    scale_linear2log(20, Rvalue(RGTK_ADJUSTMENT(w)), 22050)
                  end
        end
        sliders[1] = @dlg.add_slider("bandwidth", 0, init_bw, 1000) do |w, c, i|
          @bw = get_scale_value(w, i)
        end
        @dlg.add_target([["entire sound", :sound, true],
                         ["selection", :selection, false]]) do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class High_pass
    def initialize(label)
      @label = label
      @freq = 100
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f)", @label, @freq)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_freq = 100
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Butterworth high-pass filter. \
Move the slider to change the high-pass cutoff frequency.",
                           :reset_cb, lambda do |w, c, i|
                             @freq = init_freq
                             set_scale_value(sliders[0].scale, scale_log2linear(20, @freq, 22050))
                             if provided? "xm"
                               change_label(sliders[0].label, "%1.2f" % @freq)
                             end
                           end) do |w, c, i|
          case @target
          when :sound
            filter_sound(make_butter_high_pass(@freq))
          when :selection
            filter_selection(make_butter_high_pass(@freq))
          end
        end
        sliders[0] = @dlg.add_slider("high-pass cutoff frequency",
                                     20, init_freq, 22050, 1, :log) do |w, c, i|
          @freq = if provided? "xm"
                    scale_linear2log(20, Rvalue(i), 22050)
                  else
                    scale_linear2log(20, Rvalue(RGTK_ADJUSTMENT(w)), 22050)
                  end
        end
        @dlg.add_target([["entire sound", :sound, true],
                         ["selection", :selection, false]]) do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Low_pass
    def initialize(label)
      @label = label
      @freq = 1000
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f)", @label, @freq)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_freq = 1000
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Butterworth low-pass filter. \
Move the slider to change the low-pass cutoff frequency.",
                           :reset_cb, lambda do |w, c, i|
                             @freq = init_freq
                             set_scale_value(sliders[0].scale, scale_log2linear(20, @freq, 22050))
                             if provided? "xm"
                               change_label(sliders[0].label, "%1.2f" % @freq)
                             end
                           end) do |w, c, i|
          case @target
          when :sound
            filter_sound(make_butter_low_pass(@freq))
          when :selection
            filter_selection(make_butter_low_pass(@freq))
          end
        end
        sliders[0] = @dlg.add_slider("low-pass cutoff frequency",
                                     20, init_freq, 22050, 1, :log) do |w, c, i|
          @freq = if provided? "xm"
                    scale_linear2log(20, Rvalue(i), 22050)
                  else
                    scale_linear2log(20, Rvalue(RGTK_ADJUSTMENT(w)), 22050)
                  end
        end
        @dlg.add_target([["entire sound", :sound, true],
                         ["selection", :selection, false]]) do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Comb
    def initialize(label)
      @label = label
      @scaler = 0.1
      @size = 50
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %d)", @label, @scaler, @size)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_scaler = 0.1
        init_size = 50
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the comb scaler and size.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @scaler = init_scaler, 100.0)
                             set_scale_value(sliders[1].scale, @size = init_size)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "comb-filter", false) do |ignored|
            comb_filter(@scaler, @size)
          end
        end
        sliders[0] = @dlg.add_slider("scaler", 0.0, init_scaler, 1.0, 100) do |w, c, i|
          @scaler = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("size", 1, init_size, 100) do |w, c, i|
          @size = get_scale_value(w, i)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Comb_chord
    def initialize(label)
      @label = label
      @scaler = 0.95
      @size = 60
      @amp = 0.3
      @interval_one = 0.75
      @interval_two = 1.20
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %d %1.2f %1.2f %1.2f)",
             @label, @scaler, @size, @amp, @interval_one, @interval_two)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_scaler = 0.95
        init_size = 60
        init_amp = 0.3
        init_one = 0.75
        init_two = 1.20
        sliders = Array.new(5)
        @dlg = make_dialog(@label,
                           :info, "Comb chord filter:
Creates chords by using filters at harmonically related sizes. \
Move the sliders to set the comb chord parameters.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @scaler = init_scaler, 100.0)
                             set_scale_value(sliders[1].scale, @size = init_size)
                             set_scale_value(sliders[2].scale, @amp = init_amp, 100.0)
                             set_scale_value(sliders[3].scale, @interval_one = init_one, 100.0)
                             set_scale_value(sliders[4].scale, @interval_two = init_two, 100.0)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "comb-chord", false) do |ignored|
            comb_chord(@scaler, @size, @amp, @interval_one, @interval_two)
          end
        end
        sliders[0] = @dlg.add_slider("chord scaler", 0.0, init_scaler, 1.0, 100) do |w, c, i|
          @scaler = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("chord size", 1, init_size, 100) do |w, c, i|
          @size = get_scale_value(w, i)
        end
        sliders[2] = @dlg.add_slider("amplitude", 0.0, init_amp, 1.0, 100) do |w, c, i|
          @amp = get_scale_value(w, i, 100.0)
        end
        sliders[3] = @dlg.add_slider("interval one", 0.0, init_one, 2.0, 100) do |w, c, i|
          @interval_one = get_scale_value(w, i, 100.0)
        end
        sliders[4] = @dlg.add_slider("interval two", 0.0, init_two, 2.0, 100) do |w, c, i|
          @interval_two = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Moog_filter
    def initialize(label)
      @label = label
      @cutoff_freq = 10000
      @resonance = 0.5
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %1.2f)", @label, @cutoff_freq, @resonance)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_freq = 10000
        init_resonance = 0.5
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "Moog filter:
Moog-style 4-pole lowpass filter with 24db/oct rolloff and variable resonance. \
Move the sliders to set the filter cutoff frequency and resonance.",
                           :reset_cb, lambda do |w, c, i|
                             @cutoff_freq = init_freq
                             set_scale_value(sliders[0].scale,
                                             scale_log2linear(20, @cutoff_freq, 22050))
                             if provided? "xm"
                               change_label(sliders[0].label, "%1.2f" % @cutoff_freq)
                             end
                             set_scale_value(sliders[1].scale, @resonance = init_resonance, 100.0)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "moog-filter", false) do |ignored|
            moog(@cutoff_freq, @resonance)
          end
        end
        sliders[0] = @dlg.add_slider("cutoff frequency",
                                     20, init_freq, 22050, 1, :log) do |w, c, i|
          @cutoff_freq = if provided? "xm"
                           scale_linear2log(20, Rvalue(i), 22050)
                         else
                           scale_linear2log(20, Rvalue(RGTK_ADJUSTMENT(w)), 22050)
                         end
        end
        sliders[1] = @dlg.add_slider("resonance", 0.0, init_resonance, 1.0, 100) do |w, c, i|
          @resonance = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  #
  # --- Frequency Effects ---
  #
  class Adaptive
    def initialize(label)
      @label = label
      @size = 4
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%d)", @label, @size)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_size = 4
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the saturation scaling factor.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @size = init_size)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "adsat", false) do |ignored|
            mn = 0.0
            mx = 0.0
            n = 0
            vals = make_vct(@size)
            lambda do |val|
              if n == @size
                (0...@size).each do |i|
                  if vct_ref(vals, i) >= 0.0
                    vct_set!(vals, i, mx)
                  else
                    vct_set!(vals, i, mn)
                  end
                end
                n, mx, mn = 0, 0.0, 0.0
                vals
              else
                vct_set!(vals, n, val)
                mx = [mx, val].max
                mn = [mn, val].min
                n += 1
                false
              end
            end
          end
        end
        sliders[0] = @dlg.add_slider("adaptive saturation size", 0, init_size, 10) do |w, c, i|
          @size = get_scale_value(w, i).round
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Sample_rate
    def initialize(label)
      @label = label
      @amount = 0.0
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f)", @label, @amount)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 0.0
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the sample rate. \
Values greater than 1.0 speed up file play, negative values reverse it.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @amount = init_amount, 100.0)
                           end) do |w, c, i|
          case @target
          when :sound
            src_sound(@amount)
          when :selection
            selection? ? src_selection(@amount) : snd_warning("no selection")
          end
        end
        sliders[0] = @dlg.add_slider("sample rate", -2.0, init_amount, 2.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target([["entire sound", :sound, true],
                         ["selection", :selection, false]]) do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Time_pitch
    def initialize(label)
      @label = label
      @dlg = nil
      @target = :sound
      @time_scale = 1.0
      @hop_size = 0.05
      @segment_length = 0.15
      @ramp_scale = 0.5
      @pitch_scale = 1.0
    end

    def inspect
      format("%s (%1.2f %1.2f)", @label, @time_scale, @pitch_scale)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        time_scale = 1.0
        hop_size = 0.05
        seg_length = 0.15
        ramp_scale = 0.5
        pitch_scale = 1.0
        sliders = Array.new(5)
        @dlg = make_dialog(@label,
                           :info, "Move the sliders to change the time/pitch scaling parameters.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @time_scale = time_scale, 100.0)
                             set_scale_value(sliders[1].scale, @hop_size = hop_size, 100.0)
                             set_scale_value(sliders[2].scale, @segment_length = seg_length, 100.0)
                             set_scale_value(sliders[3].scale, @ramp_scale = ramp_scale, 100.0)
                             set_scale_value(sliders[4].scale, @pitch_scale = pitch_scale, 100.0)
                           end) do |w, c, i|
          save_controls
          reset_controls
          set_speed_control(@pitch_scale)
          new_time = @pitch_scale * @time_scale.to_f
          if new_time != 1.0
            set_expand_control?(true)
            set_expand_control(new_time)
            set_expand_control_hop(@hop_size)
            set_expand_control_length(@segment_length)
            set_expand_control_ramp(@ramp_scale)
          end
          if @target == :marks
            if ms = plausible_mark_samples
              apply_controls(selected_sound, 0, ms[0], 1 + (ms[1] - ms[0]))
            end
          else
            apply_controls(selected_sound, (@target == :sound ? 0 : 2))
          end
          restore_controls
        end
        sliders[0] = @dlg.add_slider("time scale", 0.0, time_scale, 5.0, 100) do |w, c, i|
          @time_scale = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("hop size", 0.0, hop_size, 1.0, 100) do |w, c, i|
          @hop_size = get_scale_value(w, i, 100.0)
        end
        sliders[2] = @dlg.add_slider("segment length", 0.0, seg_length, 0.5, 100) do |w, c, i|
          @segment_length = get_scale_value(w, i, 100.0)
        end
        sliders[3] = @dlg.add_slider("ramp scale", 0.0, ramp_scale, 0.5, 1000) do |w, c, i|
          @ramp_scale = get_scale_value(w, i, 100.0)
        end
        sliders[4] = @dlg.add_slider("pitch scale", 0.0, pitch_scale, 5.0, 100) do |w, c, i|
          @pitch_scale = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Src_timevar
    # Time-varying sample rate conversion (resample) (KSM)
    def initialize(label)
      @label = label
      @scale = 1.0
      @dlg = nil
      @target = :sound
      @envelope = nil
    end

    def inspect
      format("%s (%1.2f)", @label, @scale)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_scale = 1.0
        init_env = 
        sliders = Array.new(1)
        fr = nil
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the src-timevar scaling amount.",
                           :reset_cb, lambda do |w, c, i|
                             @envelope.envelope = [0.0, 1.0, 1.0, 1.0]
                             set_scale_value(sliders[0].scale, @scale = init_scale, 100.0)
                           end) do |w, c, i|
          env = @envelope.scale(@scale)
          case @target
          when :sound
            src_sound(env)
          when :selection
            if selection_member?(selected_sound)
              src_selection(env)
            else
              snd_warning("no selection")
            end
          else
            if pts = plausible_mark_samples
              beg = pts[0]
              len = pts[1] - beg
              src_channel(make_env(env, :end, len), beg, len, selected_sound)
            end
          end
        end
        sliders[0] = @dlg.add_slider("resample factor", 0.0, init_scale, 10.0, 100) do |w, c, i|
          @scale = get_scale_value(w, i, 100.0)
        end
        if provided? "xm"
          frame = @dlg.add_frame([RXmNheight, 200])
          @dlg.add_target() do |t| @target = t end
        else
          frame = @dlg.parent
        end
        activate_dialog(@dlg.dialog)
        # out-of-range error if envelope hits 0.0
        # therefore y0 = 0.0001
        @envelope = make_xenved("src-timevar", frame,
                                :envelope, [0.0, 1.0, 1.0, 1.0],
                                :axis_bounds, [0.0, 1.0, 0.0001, 1.0])
        if provided? "xg"
          @dlg.add_target() do |t| @target = t end
        end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  #
  # --- Modulation Effects ---
  #
  class Amplitude_modulation
    def initialize(label)
      @label = label
      @amount = 100.0
      @dlg = nil
      @target = :sound
      @envelope = nil
    end

    def inspect
      format("%s (%1.2f)", @label, @amount)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 100.0
        sliders = Array.new(1)
        fr = nil
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the modulation amount.",
                           :reset_cb, lambda do |w, c, i|
                             @envelope.envelope = [0.0, 1.0, 1.0, 1.0]
                             set_scale_value(sliders[0].scale, @amount = init_amount)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "am", false) do |ignored|
            os = make_oscil(@amount)
            need_env = (@envelope.envelope != [0.0, 1.0, 1.0, 1.0])
            e = (need_env and make_env(@envelope.envelope, :end, effect_frames(@target) - 1))
            if need_env
              lambda do |inval|
                amplitude_modulate(1.0, inval, env(e) * oscil(os))
              end
            else
              lambda do |inval|
                amplitude_modulate(1.0, inval, oscil(os))
              end
            end
          end
        end
        sliders[0] = @dlg.add_slider("amplitude modulation", 0.0, init_amount, 1000.0) do |w, c, i|
          @amount = get_scale_value(w, i)
        end
        if provided? "xm"
          frame = @dlg.add_frame([RXmNheight, 200])
          @dlg.add_target() do |t| @target = t end
        else
          frame = @dlg.parent
        end
        activate_dialog(@dlg.dialog)
        @envelope = make_xenved("amplitude modulation", frame,
                                :envelope, [0.0, 1.0, 1.0, 1.0],
                                :axis_bounds, [0.0, 1.0, 0.0, 1.0])
        if provided? "xg"
          @dlg.add_target() do |t| @target = t end
        end
      else
        activate_dialog(@dlg.dialog)
      end
    end
  end  

  class Ring_modulation
    def initialize(label)
      @label = label
      @frequency = 100
      @radians = 100
      @dlg = nil
      @target = :sound
      @envelope = nil
    end

    def inspect
      format("%s (%d %d)", @label, @frequency, @radians)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_frequency = 100
        init_radians = 100
        sliders = Array.new(2)
        fr = nil
        @dlg = make_dialog(@label,
                           :info, "Move the sliders to change the modulation parameters.",
                           :reset_cb, lambda do |w, c, i|
                             @envelope.envelope = [0.0, 1.0, 1.0, 1.0]
                             set_scale_value(sliders[0].scale, @frequency = init_frequency)
                             set_scale_value(sliders[1].scale, @radians = init_radians)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "ring-modulation", false) do |ignored|
            os = make_oscil(@frequency)
            need_env = (@envelope.envelope != [0.0, 1.0, 1.0, 1.0])
            e = (need_env and make_env(@envelope.envelope, :end, effect_frames(@target) - 1))
            len = frames()
            genv = make_env([0, 0, 1, hz2radians(@radians)], :end, len)
            if need_env
              lambda do |inval|
                inval * (env(e) * oscil(os))
              end
            else
              lambda do |inval|
                inval * oscil(os)
              end
            end
          end
        end
        sliders[0] = @dlg.add_slider("modulation frequency", 0, init_frequency, 1000) do |w, c, i|
          @frequency = get_scale_value(w, i)
        end
        sliders[1] = @dlg.add_slider("modulation radians", 0, init_radians, 360) do |w, c, i|
          @radians = get_scale_value(w, i)
        end
        if provided? "xm"
          frame = @dlg.add_frame([RXmNheight, 200])
          @dlg.add_target() do |t| @target = t end
        else
          frame = @dlg.parent
        end
        activate_dialog(@dlg.dialog)
        @envelope = make_xenved("ring modulation", frame,
                                :envelope, [0.0, 1.0, 1.0, 1.0],
                                :axis_bounds, [0.0, 1.0, 0.0, 1.0])
        if provided? "xg"
          @dlg.add_target() do |t| @target = t end
        end
      else
        activate_dialog(@dlg.dialog)
      end
    end
  end  

  #
  # --- Reverbs ---
  #
  class Nrev
    def initialize(label)
      @label = label
      @amount = 0.1
      @filter = 0.5
      @feedback = 1.09
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %1.2f %1.2f)", @label, @amount, @filter, @feedback)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 0.1
        init_filter = 0.5
        init_feedback = 1.09
        sliders = Array.new(3)
        @dlg = make_dialog(@label,
                           :info, "Reverberator from Michael McNabb. \
Adds reverberation scaled by reverb amount, lowpass filtering, and feedback. \
Move the sliders to change the reverb parameters.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @amount = init_amount, 100.0)
                             set_scale_value(sliders[1].scale, @filter = init_filter, 100.0)
                             set_scale_value(sliders[2].scale, @feedback = init_feedback, 100.0)
                           end) do |w, c, i|
          save_controls
          reset_controls
          set_reverb_control?(true)
          set_reverb_control_scale(@amount)
          set_reverb_control_lowpass(@filter)
          set_reverb_control_feedback(@feedback)
          if @target == :marks
            if ms = plausible_mark_samples
              apply_controls(selected_sound, 0, ms[0], 1 + (ms[1] - ms[0]))
            end
          else
            apply_controls(selected_sound, (@target == :sound ? 0 : 2))
          end
          restore_controls
        end
        sliders[0] = @dlg.add_slider("reverb amount", 0.0, init_amount, 1.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("reverb filter", 0.0, init_filter, 1.0, 100) do |w, c, i|
          @filter = get_scale_value(w, i, 100.0)
        end
        sliders[2] = @dlg.add_slider("reverb feedback", 0.0, init_feedback, 1.25, 100) do |w, c, i|
          @feedback = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Chowning
    def initialize(label)
      @label = label
      @decay = 2.0
      @volume = 0.1
      @dlg = nil
      @target = :sound
      @truncate = true
    end

    def inspect
      format("%s (%1.2f %1.2f)", @label, @decay, @volume)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_decay = 2.0
        init_volume = 0.1
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "Nice reverb from John Chowning. \
Move the sliders to change the reverb parameters.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @decay = init_decay, 100.0)
                             set_scale_value(sliders[1].scale, @volume = init_volume, 100.0)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "jc-reverb", (!@truncate and @decay)) do |x|
            jc_reverb_1(x, @volume)
          end
        end
        sliders[0] = @dlg.add_slider("decay duration", 0.0, init_decay, 10.0, 100) do |w, c, i|
          @decay = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("reverb volume", 0.0, init_volume, 1.0, 100) do |w, c, i|
          @volume = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
        @dlg.add_toggle() do |t| @truncate = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end
  
  class Convolution
    def initialize(label)
      @label = label
      @sound_one = 0
      @sound_two = 1
      @amp = 0.01
      @dlg = nil
    end

    def inspect
      format("%s (%d %d %1.2f)", @label, @sound_one, @sound_two, @amp)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_one = 0
        init_two = 1
        init_amp = 0.01
        sliders = Array.new(3)
        @dlg = make_dialog(@label,
                           :info, "Very simple convolution. \
Move the sliders to set the numbers of the soundfiles to be convolved and the \
amount for the amplitude scaler.

Output will be scaled to floating-point values, resulting in very large \
(but not clipped) amplitudes. Use the Normalize amplitude effect to rescale the output.

The convolution data file typically defines a natural reverberation source, \
and the output from this effect can provide very striking reverb effects. \
You can find convolution data files on sites listed at \
http://www.bright.net/~dlphilp/linux_csound.html under Impulse Response Data.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @sound_one = init_one)
                             set_scale_value(sliders[1].scale, @sound_two = init_two)
                             set_scale_value(sliders[2].scale, @amp = init_amp, 100.0)
                           end) do |w, c, i|
          if sound?(@sound_one) and sound?(@sound_two)
            cnvtest(@sound_one, @sound_two, @amp)
          else
            snd_warning(format("sound-one: %s, sound-two: %s",
                               sound?(@sound_one).to_s, sound?(@sound_two).to_s))
          end
        end
        sliders[0] = @dlg.add_slider("impulse response file", 0, init_one, 24) do |w, c, i|
          @sound_one = get_scale_value(w, i).round
        end
        sliders[1] = @dlg.add_slider("sound file", 0, init_two, 24) do |w, c, i|
          @sound_two = get_scale_value(w, i).round
        end
        sliders[2] = @dlg.add_slider("amplitude", 0.0, init_amp, 0.1, 100) do |w, c, i|
          @amp = get_scale_value(w, i, 100.0)
        end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  #
  # --- Various and Miscellaneous ---
  #
  class Placed
    def initialize(label)
      @label = label
      @mono_snd = 0
      @stereo_snd = 1
      @pan_pos = 45
      @dlg = nil
      @target = :sound
      @envelope = nil
    end

    def inspect
      format("%s (%d %d %d)", @label, @mono_snd, @stereo_snd, @pan_pos)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_mono_snd = 0
        init_stereo_snd = 1
        init_pan_pos = 45
        sliders = Array.new(3)
        @dlg = make_dialog(@label,
                           :info, "Mixes mono sound into stereo sound field.",
                           :reset_cb, lambda do |w, c, i|
                             @envelope.envelope = [0.0, 1.0, 1.0, 1.0]
                             set_scale_value(sliders[0].scale, @mono_snd = init_mono_snd)
                             set_scale_value(sliders[1].scale, @stereo_snd = init_stereo_snd)
                             set_scale_value(sliders[2].scale, @pan_pos = init_pan_pos)
                           end) do |w, c, i|
          unless (e = @envelope.envelope) == [0.0, 1.0, 1.0, 1.0]
            place_sound(@mono_snd, @stereo_snd, e)
          else
            place_sound(@mono_snd, @stereo_snd, @pan_pos)
          end
        end
        sliders[0] = @dlg.add_slider("mono sound", 0, init_mono_snd, 50) do |w, c, i|
          @mono_snd = get_scale_value(w, i).round
        end
        sliders[1] = @dlg.add_slider("stereo sound", 0, init_stereo_snd, 50) do |w, c, i|
        @stereo_snd = get_scale_value(w, i).round
        end
        sliders[2] = @dlg.add_slider("pan position", 0, init_pan_pos, 90) do |w, c, i|
        @pan_pos = get_scale_value(w, i)
        end
        if provided? "xm"
          frame = @dlg.add_frame([RXmNheight, 200])
          @dlg.add_target() do |t| @target = t end
        else
          frame = @dlg.parent
        end
        activate_dialog(@dlg.dialog)
        @envelope = make_xenved("panning", frame,
                                :envelope, [0.0, 1.0, 1.0, 1.0],
                                :axis_bounds, [0.0, 1.0, 0.0, 1.0])
        if provided? "xg"
          @dlg.add_target() do |t| @target = t end
        end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Add_silence
    def initialize(label)
      @label = label
      @amount = 1.0
      @dlg = nil
    end

    def inspect
      format("%s (%1.2f)", @label, @amount)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 1.0
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the number of seconds \
of silence added at the cursor position.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @amount = init_amount, 100.0)
                           end) do |w, c, i|
          insert_silence(cursor(), (srate().to_f * @amount).round)
        end
        sliders[0] = @dlg.add_slider("silence", 0.0, init_amount, 5.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Contrast
    def initialize(label)
      @label = label
      @amount = 1.0
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f)", @label, @amount)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_amount = 1.0
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the contrast intensity.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @amount = init_amount, 100.0)
                           end) do |w, c, i|
          peak = maxamp()
          save_controls
          reset_controls
          set_contrast_control?(true)
          set_contrast_control(@amount)
          set_contrast_control_amp(1.0 / peak)
          set_amp_control(peak)
          if @target == :marks
            if ms = plausible_mark_samples
              apply_controls(selected_sound, 0, ms[0], 1 + (ms[1] - ms[0]))
            end
          else
            apply_controls(selected_sound, (@target == :sound ? 0 : 2))
          end
          restore_controls
        end
        sliders[0] = @dlg.add_slider("contrast enhancement",
                                     0.0, init_amount, 10.0, 100) do |w, c, i|
          @amount = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Cross_synthesis
    def initialize(label)
      @label = label
      @sound = 1
      @amp = 0.5
      @fft_size = 128
      @radius = 6.0
      @dlg = nil
      @default_fft_widget = nil
      @target = :sound
    end

    def inspect
      format("%s (%d %1.2f %d %1.2f)", @label, @sound, @amp, @fft_size, @radius)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_sound = 1
        init_amp = 0.5
        init_fft_size = 128
        init_radius = 6.0
        sliders = Array.new(3)
        @dlg = make_dialog(@label,
                           :info, "The sliders set the number of the soundfile \
to be cross_synthesized, the synthesis amplitude, the FFT size, and the radius value.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @sound = init_sound)
                             set_scale_value(sliders[1].scale, @amp = init_amp, 100.0)
                             @fft_size = init_fft_size
                             if provided? "xm"
                               RXmToggleButtonSetState(@default_fft_widget, true, true)
                             end
                             set_scale_value(sliders[2].scale, @radius = init_radius, 100.0)
                           end) do |w, c, i|
          if sound?(@sound)
            map_chan_over_target_with_sync(@target, "Cross synthesis", false) do |ignored|
              cross_synthesis(@sound, @amp, @fft_size, @radius)
            end
          else
            snd_warning(format("cross-snd: %s", sound?(@sound).to_s))
          end
        end
        sliders[0] = @dlg.add_slider("input sound", 0, init_sound, 20) do |w, c, i|
          @sound = get_scale_value(w, i).round
        end
        sliders[1] = @dlg.add_slider("amplitude", 0.0, init_amp, 1.0, 100) do |w, c, i|
          @amp = get_scale_value(w, i, 100.0)
        end
        sliders[2] = @dlg.add_slider("radius", 0.0, init_radius, 360.0, 100) do |w, c, i|
          @radius = get_scale_value(w, i, 100.0)
        end
        if provided? "xm"
          frame = @dlg.add_frame([RXmNpositionIndex, 2])
          frm = RXtCreateManagedWidget("frm", RxmFormWidgetClass, frame,
                                       [RXmNleftAttachment, RXmATTACH_FORM,
                                        RXmNrightAttachment, RXmATTACH_FORM,
                                        RXmNtopAttachment, RXmATTACH_FORM,
                                        RXmNbottomAttachment, RXmATTACH_FORM,
                                        RXmNbackground, basic_color])
          rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, frm,
                                      [RXmNorientation, RXmHORIZONTAL,
                                       RXmNradioBehavior, true,
                                       RXmNradioAlwaysOne, true,
                                       RXmNentryClass, RxmToggleButtonWidgetClass,
                                       RXmNisHomogeneous, true,
                                       RXmNleftAttachment, RXmATTACH_FORM,
                                       RXmNrightAttachment, RXmATTACH_FORM,
                                       RXmNtopAttachment, RXmATTACH_FORM,
                                       RXmNbottomAttachment, RXmATTACH_NONE,
                                       RXmNbackground, basic_color])
          RXtCreateManagedWidget("FFT size", RxmLabelWidgetClass, frm,
                                 [RXmNalignment, RXmALIGNMENT_BEGINNING,
                                  RXmNtopAttachment, RXmATTACH_WIDGET,
                                  RXmNtopWidget, rc,
                                  RXmNbackground, basic_color])
          [64, 128, 256, 512, 1024, 4096].each do |s|
            button = RXtCreateManagedWidget(s.to_s, RxmToggleButtonWidgetClass, rc,
                                            [RXmNbackground, basic_color,
                                             RXmNvalueChangedCallback,
                                             [lambda do |w, c, i| @fft_size = c if Rset(i) end, s],
                                             RXmNset, (s == @fft_size)])
            @default_fft_widget = button if s == @fft_size
          end
          end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Flange
    def initialize(label)
      @label = label
      @speed = 2.0
      @amount = 5.0
      @time = 0.001
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %1.2f %1.3f)", @label, @speed, @amount, @time)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_speed = 2.0
        init_amount = 5.0
        init_time = 0.001
        sliders = Array.new(3)
        @dlg = make_dialog(@label,
                           :info, "Move the sliders to change the flange speed, amount, and time.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @speed = init_speed, 10.0)
                             set_scale_value(sliders[1].scale, @amount = init_amount, 10.0)
                             set_scale_value(sliders[2].scale, @time = init_time, 100.0)
                           end) do |w, c, i|
          map_chan_over_target_with_sync(@target, "flange", false) do |ignored|
            ri = make_rand_interp(:frequency, @speed, :amplitude, @amount)
            len = (@time.to_f * srate()).round
            del = make_delay(len, false, 0.0, (len + @amount + 1).round)
            lambda do |inval|
              0.75 * (inval + delay(del, inval, rand_interp(ri)))
            end
          end
        end
        sliders[0] = @dlg.add_slider("flange speed", 0.0, init_speed, 100.0, 10) do |w, c, i|
          @speed = get_scale_value(w, i, 10.0)
        end
        sliders[1] = @dlg.add_slider("flange amount", 0.0, init_amount, 100.0, 10) do |w, c, i|
          @amount = get_scale_value(w, i, 10.0)
        end
        sliders[2] = @dlg.add_slider("flange time", 0.0, init_time, 1.0, 100) do |w, c, i|
          @time = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Randomize_phase
    def initialize(label)
      @label = label
      @amp_scaler = 3.14
      @dlg = nil
    end

    def inspect
      format("%s (%1.2f)", @label, @amp_scaler)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_scaler = 3.14
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Move the slider to change the randomization amplitude scaler.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @amp_scaler = init_scaler, 100.0)
                           end) do |w, c, i|
          rotate_phase(lambda do |x| kernel_rand(@amp_scaler) end)
        end
        sliders[0] = @dlg.add_slider("amplitude scaler", 0.0, init_scaler, 100.0, 100) do |w, c, i|
          @amp_scaler = get_scale_value(w, i, 100.0)
        end
      end
      activate_dialog(@dlg.dialog)
    end
  end

  class Robotize
    def initialize(label)
      @label = label
      @samp_rate = 1.0
      @osc_amp = 0.3
      @osc_freq = 20
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %1.2f %1.2f)", @label, @samp_rate, @osc_amp, @osc_freq)
    end
    
    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_rate = 1.0
        init_amp = 0.3
        init_freq = 20
        sliders = Array.new(3)
        @dlg = make_dialog(@label,
                           :info, "Move the sliders to set the sample rate, \
oscillator amplitude, and oscillator frequency.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @samp_rate = init_rate, 100.0)
                             set_scale_value(sliders[1].scale, @osc_amp = init_amp, 100.0)
                             set_scale_value(sliders[2].scale, @osc_freq = init_freq, 100.0)
                           end) do |w, c, i|
          ms = (@target == :marks and plausible_mark_samples)
          fp_1(@samp_rate, @osc_amp, @osc_freq,
             case @target
             when :sound
               0
             when :selection
               selection_position
             else
               (ms ? ms[0] : 0)
             end, case @target
                  when :sound
                    frames() - 1
                  when :selection
                    selection_position + selection_frames
                  else
                    (ms ? ms[1] : 0)
                  end)
        end
        sliders[0] = @dlg.add_slider("sample rate", 0.0, init_rate, 2.0, 100) do |w, c, i|
          @samp_rate = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("oscillator amplitude", 0.0, init_amp, 1.0, 100) do |w, c, i|
          @osc_amp = get_scale_value(w, i, 100.0)
        end
        sliders[2] = @dlg.add_slider("oscillator frequency", 0.0, init_freq, 60, 100) do |w, c, i|
          @osc_freq = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Rubber_sound
    def initialize(label)
      @label = label
      @factor = 1.0
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f)", @label, @factor)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_factor = 1.0
        sliders = Array.new(1)
        @dlg = make_dialog(@label,
                           :info, "Stretches or contracts the time of a sound. \
Move the slider to change the stretch factor.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @factor = init_factor, 100.0)
                           end) do |w, c, i|
          rubber_sound(@factor)
        end
        sliders[0] = @dlg.add_slider("stretch factor", 0.0, init_factor, 5.0, 100) do |w, c, i|
          @factor = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end  

  class Wobble
    def initialize(label)
      @label = label
      @frequency = 50
      @amplitude = 0.5
      @dlg = nil
      @target = :sound
    end

    def inspect
      format("%s (%1.2f %1.2f)", @label, @frequency, @amplitude)
    end

    def post_dialog
      unless @dlg.kind_of?(Dialog) and widget?(@dlg.dialog)
        init_freq = 50
        init_amp = 0.5
        sliders = Array.new(2)
        @dlg = make_dialog(@label,
                           :info, "Move the sliders to set the wobble frequency and amplitude.",
                           :reset_cb, lambda do |w, c, i|
                             set_scale_value(sliders[0].scale, @frequency = init_freq, 100.0)
                             set_scale_value(sliders[1].scale, @amplitude = init_amp, 100.0)
                           end) do |w, c, i|
          ms = (@target == :marks and plausible_mark_samples)
          hello_dentist_1(@frequency, @amplitude,
                          case @target
                          when :sound
                            0
                          when :selection
                            selection_position
                          else
                            (ms ? ms[0] : 0)
                          end, case @target
                               when :sound
                                 frames() - 1
                               when :selection
                                 selection_position + selection_frames
                               else
                                 (ms ? ms[1] : 0)
                               end)
        end
        sliders[0] = @dlg.add_slider("wobble frequency", 0, init_freq, 100, 100) do |w, c, i|
          @frequency = get_scale_value(w, i, 100.0)
        end
        sliders[1] = @dlg.add_slider("wobble amplitude", 0.0, init_amp, 1.0, 100) do |w, c, i|
          @amplitude = get_scale_value(w, i, 100.0)
        end
        @dlg.add_target() do |t| @target = t end
      end
      activate_dialog(@dlg.dialog)
    end
  end
end

#
# example menu
#
unless defined? $__private_snd_menu__ and $__private_snd_menu__
  include Effects

  snd_main = make_snd_menu("Effects") do
    cascade("Amplitude Effects") do
      entry(Gain, "Gain")
      entry(Normalize, "Normalize")
      entry(Gate, "Gate")
    end
    cascade("Delay Effects") do
      entry(Echo, "Echo")
      entry(Filtered_echo, "Filtered echo")
      entry(Modulated_echo, "Modulated echo")
    end
    cascade("Filter Effects") do
      entry(Band_pass, "Band-pass filter")
      entry(Band_reject, "Band-reject filter")
      entry(High_pass, "High-pass filter")
      entry(Low_pass, "Low-pass filter")
      entry(Comb, "Comb filter")
      entry(Comb_chord, "Comb chord filter")
      entry(Moog_filter, "Moog filter")
    end
    cascade("Frequency Effects") do
      entry(Adaptive, "Adaptive saturation")
      entry(Sample_rate, "Sample rate conversion")
      entry(Time_pitch, "Time/pitch scaling")
      entry(Src_timevar, "Src-Timevar")
    end
    cascade("Modulation Effects") do
      entry(Amplitude_modulation, "Amplitude modulation")
      entry(Ring_modulation, "Ring modulation")
    end
    cascade("Reverbs") do
      entry(Nrev, "McNabb reverb")
      entry(Chowning, "Chowning reverb")
      entry(Convolution, "Convolution")
    end
    cascade("Various") do
      entry(Placed, "Place sound")
      entry(Add_silence, "Add silence")
      entry(Contrast, "Contrast enhancement")
      entry(Cross_synthesis, "Cross synthesis")
      entry(Flange, "Flange")
      entry(Randomize_phase, "Randomize phase")
      entry(Robotize, "Robotize")
      entry(Rubber_sound, "Rubber sound")
      entry(Wobble, "Wobble")
    end
    separator
    entry("Octave-down") do down_oct() end
    entry("Remove clicks") do
      find_click = lambda do |loc|
        reader = make_sample_reader(loc)
        samp0 = samp1 = samp2 = 0.0
        samps = make_vct(10)
        len = frames()
        samps_ctr = 0
        callcc do |ret|
          ctr = loc
          until c_g? or ctr == len
            samp0, samp1 = samp1, samp2
            samp2 = next_sample(reader)
            samps[samps_ctr] = samp0
            if samps_ctr < 9
              samps_ctr += 1
            else
              samps_ctr = 0
            end
            local_max = [0.1, vct_peak(samps)].max
            if ((samp0 - samp1).abs > local_max) and
                ((samp1 - samp2).abs > local_max) and
                ((samp0 - samp2).abs < (local_max / 2))
              ret.call(ctr - 1)
            end
            ctr += 1
          end
          false
        end
      end
      remove_click = lambda do |loc|
        if click = find_click.call(loc) and !c_g?
          smooth_sound(click - 2, 4)
          remove_click.call(click + 2)
        end
      end
      remove_click.call(2)
    end
    entry("Remove DC") do
      lastx = lasty = 0.0
      map_chan(lambda do |inval|
                 lasty = inval + (0.999 * lasty - lastx)
                 lastx = inval
                 lasty
               end)
    end
    entry("Spiker") do spike() end
    entry("Compand") do
      tbl = vct(-1.00, -0.96, -0.90, -0.82, -0.72, -0.60, -0.45, -0.25,
                0.00, 0.25, 0.45, 0.60, 0.72, 0.82, 0.90, 0.96, 1.00)
      map_chan(lambda do |inval| array_interp(tbl, 8.0 + 8.0 * inval, tbl.length) end)
    end
    entry("Invert") do scale_by(-1) end
    entry("Reverse") do reverse_sound() end
    entry("Null phase") do zero_phase() end
  end

  if provided? "xm"
    set_label_sensitive(menu_widgets[Top_menu_bar], "Effects", ((sounds() or []).length > 1))
  else
    set_sensitive(snd_main.menu, ((sounds() or []).length > 1))
  end
  
  unless $open_hook.member?("effects-menu-hook")
    $open_hook.add_hook!("effects-menu-hook") do |snd| 
      if provided? "xm"
        set_label_sensitive(menu_widgets[Top_menu_bar], "Effects", true)
      else
        set_sensitive(snd_main.menu, true)
      end
      false
    end
    
    $close_hook.add_hook!("effects-menu-hook") do |snd|
      if provided? "xm"
        set_label_sensitive(menu_widgets[Top_menu_bar], "Effects", ((sounds() or []).length > 1))
      else
        set_sensitive(snd_main.menu, ((sounds() or []).length > 1))
      end
      false
    end
  end
end

# effects.rb ends here
