# extensions.rb -- various generally useful Snd extensions (see extensions.scm)

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Sat Jan 03 17:30:23 CET 2004
# Last: Tue Feb 10 14:44:07 CET 2004

# Commentary:
#
# module Extensions
#
# channel_property(key, snd, chn)  set_channel_property(key, val, snd, chn)
# sound_property(key, snd)         set_sound_property(key, val, snd)
# channel_sync(snd, chn)           set_channel_sync(val, snd, chn)
#
# marks?(more_than_one)
# all_chans()
# normalized_mix(fname, beg, in_chan, snd, chn)
# enveloped_mix(fname, beg, env, del_tmp)
#
# map_sound_files(*args) do |f| ... end
# for_each_sound_file(*args) do |f| ... end
# match_sound_files(*args) do |f| ... end
#
# selection_members()
# make_selection(beg, len, snd, chn)
# delete_selection_and_smooth()
# eval_over_selection(func)
#
# check_for_unsaved_edits(check)
# remember_sound_state()
#
# mix_channel(fdata, beg, dur, snd, chn, edpos)
# insert_channel(fdata, beg, dur, snd, chn, edpos)
# redo_channel(edits, snd, chn)
# undo_channel(edits, snd, chn)
#
# any_env_channel(env, beg, dur, snd, chn, edpos) do |r0, r1, b, d, s, c, e| ... end
# sine_ramp(rmp0, rmp1, beg, dur, snd, chn, edpos)
# sine_env_channel(env, beg, dur, snd, chn, edpos)
# blackman4_ramp(rmp0, rmp1, beg, dur, snd, chn, edpos)
# blackman4_env_channel(env, beg, dur, snd, chn, edpos)
# ramp_squared(rmp0, rmp1, symmetric, beg, dur, snd, chn, edpos)
# env_squared_channel(env, symmetric, beg, dur, snd, chn, edpos)
# ramp_expt(rmp0, rmp1, exponent, symmetric, beg, dur, snd, chn, edpos)
# env_expt_channel(env, exponent, symmetric, beg, dur, snd, chn, edpos)
# offset_channel(amount, beg, dur, snd, chn, edpos)
# contrast_channel(index, beg, dur, snd, chn, edpos)

# Comments are mostly taken from extensions.scm.

# Code:

require "examp"
require "hooks"
include Math

module Extensions
  # Returns the value associated with KEY in the given CHN's
  # property list, or nil.
  def channel_property(key, snd = false, chn = false)
    if (h = channel_properties(snd, chn)).kind_of?(Hash)
      h[key]
    else
      nil
    end
  end
  
  # Sets KEY to VAL in the given CHN's property list and returns VAL.
  def set_channel_property(key, val, snd = false, chn = false)
    if (h = channel_properties(snd, chn)).kind_of?(Hash)
      h[key] = val
      set_channel_properties(h.rehash, snd, chn)
    else
      set_channel_properties({key, val}, snd, chn)
    end
    val
  end

  # Returns the value associated with KEY in the given SND's property
  # list, or nil.
  def sound_property(key, snd = false)
    if (h = sound_properties(snd)).kind_of?(Hash)
      h[key]
    else
      nil
    end
  end
  
  # Sets KEY to VAL in the given SND's property list and returns VAL.
  def set_sound_property(key, val, snd = false)
    if (h = sound_properties(snd)).kind_of?(Hash)
      h[key] = val
      set_sound_properties(h.rehash, snd)
    else
      set_sound_properties({key, val}, snd)
    end
    val
  end
  
  def channel_sync(snd = false, chn = false)
    channel_property(:sync, snd, chn)
  end
  
  def set_channel_sync(val, snd = false, chn = false)
    set_channel_property(:sync, val, snd, chn)
  end

  # Have we marks?  Similar to selections?().
  def marks?(more_than_one = true)
    if (m = marks(selected_sound(), selected_channel())).nil?
      false
    else
      if more_than_one
        m.length > 1
      else
        true
      end
    end
  end
  
  # Returns two parallel arrays, the first snd indices, the second
  # channel numbers.  E.g. snd one (0) has one channel, snd two (1) has
  # two channels --> [[0, 1, 1], [0, 0, 1]].
  def all_chans
    sndlist = []
    chnlist = []
    (sounds() or []).each do |snd|
      channels(snd).times do |i|
        sndlist << snd
        chnlist << i
      end
    end
    [sndlist, chnlist]
  end
  
  # Like mix but the mix result has same peak amp as unmixed snd/chn.
  def normalized_mix(fname, beg = 0, in_chan = 0, snd = false, chn = false)
    orig_maxamp = maxamp(snd, chn)
    mix(fname, beg, in_chan, snd, chn)
    new_maxamp = maxamp(snd, chn)
    if orig_maxamp == new_maxamp
      1.0
    else
      scl = orig_maxamp / new_maxamp
      old_sync = sync(snd)
      set_sync(false, snd)
      scale_by(scl, snd, chn)
      set_sync(old_sync, snd)
      scl
    end
  end
  
  # Mixes FNAME starting at BEG with amplitude envelope ENV.
  def enveloped_mix(fname, beg, env, del_tmp = true)
    len = mus_sound_frames(fname)
    if (tmp_name = temp_dir()).kind_of?(String) and tmp_name.length > 0
      tmp_name += "/tmp.snd"
    else
      tmp_name = "tmp.snd"
    end
    tmpfil = mus_sound_open_output(tmp_name, 22050, 1, Mus_lshort, Mus_next, "")
    mx = make_mixer(1, 1.0)
    mus_sound_close_output(tmpfil, 0)
    mus_mix(tmp_name, fname, 0, len, 0, mx, [[make_env(:envelope, env, :end, len)]])
    mix(tmp_name, beg)
    File.unlink(tmp_name) if del_tmp
  end
  # enveloped_mix("pistol.snd", 0, [0, 0, 1, 1, 2, 0])
  
  def map_sound_files(*args, &func)
    args << "." if args.empty?
    args.map do |dir|
      sound_files_in_directory(dir).map do |f| func.call(f) end if File.exist?(dir)
    end
  end
  # map_sound_files(".", "/usr/gnu/sound/SFiles") do |f| play_and_wait(f) end
  
  def for_each_sound_file(*args, &func)
    args << "." if args.empty?
    args.each do |dir|
      sound_files_in_directory(dir).each do |f| func.call(f) end if File.exist?(dir)
    end
  end
  # for_each_sound_file(".", "/usr/gnu/sound/SFiles") do |f| play_and_wait(f) end
  
  def match_sound_files(*args, &func)
    res = []
    args << "." if args.empty?
    args.each do |dir|
      sound_files_in_directory(dir).each do |f|
        res << f if func.call(f)
      end
    end
    res
  end
  # match_sound_files() do |f| f =~ /\.(wav|snd)$/ end
  
  # Returns an array of arrays of [snd, chn] indicating the channels
  # participating in the current selection.
  def selection_members
    sndlist = []
    if selection?
      (sounds() or []).each do |snd|
        channels(snd).times do |i|
          sndlist.push([snd, i]) if selection_member?(snd, i)
        end
      end
    end
    sndlist
  end
  
  # Makes a selection like make_region but without creating a region.
  # make_selection follows SND's sync field, and applies to all SND's
  # channels if CHN is not specified.
  def make_selection(beg = 0, len = false, snd = false, chn = false)
    add_chan_to_selection = lambda do |s0, s1, s, c|
      set_selection_member?(true, s, c)
      set_selection_position(s0, s, c)
      val = if s1.kind_of?(Numeric)
              s1 + 1
            else
              frames(s, c)
            end
      set_selection_frames(val - s0, s, c)
    end
    current_sound = (snd or selected_sound() or (s = sounds() and s[0]))
    unless sound?(current_sound)
      error("make_selection(%s, %s, %s, %s): no-such-sound",
            beg.inspect, len.inspect, snd.inspect, chn.inspect)
    end
    current_sync = sync(current_sound)
    if selection?
      (sounds() or []).each do |s|
        channels(s).times do |i|
          need_update = selection_member?(s, i)
          set_selection_member?(false, s, i)
          update_time_graph(s, i) if need_update
        end
      end
    end
    if chn
      add_chan_to_selection.call(beg, len, snd, chn)
    else
      (sounds() or []).each do |s|
        if snd == true or s == current_sound or (current_sync.nonzero? and current_sync == sync(s))
          channels(s).times do |i|
            add_chan_to_selection.call(beg, len, s, i)
          end
        end
      end
    end
  end
  
  # Deletes the current selection and smooths the splice.
  def delete_selection_and_smooth
    ret = nil
    if selection?
      beg = selection_position()
      len = selection_frames()
      sndlist, chnlist = all_chans()
      sndlist.zip(chnlist) do |snd, chn|
        if selection_member?(snd, chn)
          smooth_beg = [0, beg - 16].max
          delete_samples(beg, len, snd, chn)
          ret = smooth_sound(smooth_beg, 32, snd, chn)
        end
      end
    end
    ret
  end
  
  # Evaluates FUNC of one argument on each sample in the current
  # selection.
  def eval_over_selection(func)
    if func.kind_of?(Proc) and selection?
      beg = selection_position()
      len = selection_frames()
      sndlst, chnlst = all_chans()
      sndlst.zip(chnlst) do |snd, chn|
        if selection_member?(snd, chn)
          new_data = make_vct(len)
          old_data = samples2vct(beg, len, snd, chn)
          len.times do |i| new_data[i] = func.call(old_data[i]) end
          vct2channel(new_data, beg, len, snd, chn)
        end
      end
    end
  end
=begin
  # When the user types C-x x (without modifiers) and there is a current
  # selection, the minibuffer prompts "selection eval:".  Eventually the
  # user responds, hopefully with a function of one argument, the
  # current selection sample the value returned by the function becomes
  # the new selection value.

  bind_key(?x, 0, lambda do
             if selection?
               prompt_in_minibuffer("selection eval:", method(:eval_over_selection).to_proc)
             else
               report_in_minibuffer("no selection")
             end
           end, true, "C-x x: eval over selection")
=end

  # check_for_unsaved_edits(check)
  #
  # If CHECK is true, add a function to the close_hook and exit_hook
  # that asks the user for confirmation before closing a sound if there
  # are unsaved edits on that sound.  If CHECK is false, remove those
  # hooks.
  def check_for_unsaved_edits(check = true)
    unsaved_edits_at_close = lambda do |snd|
      callcc do |c|
        channels(snd).times do |chn|
          eds = edits(snd, chn)
          if eds[0] > 0
            unless yes_or_no?(format("%s has %d unsaved edit(s) in channel %d, exit anyway?",
                                     short_file_name(snd), eds[0], chn))
              c.call(true)
            end
          end
        end
        false
      end
    end
    if check
      unless $close_hook.member?("unsaved-edits-at-close?")
        $close_hook.add_hook!("unsaved-edits-at-close?") do |s| unsaved_edits_at_close.call(s) end
      end
      unless $exit_hook.member?("unsaved-edits-at-exit?")
        $exit_hook.add_hook!("unsaved-edits-at-exit?") do | |
          if (sounds() or []).detect do |snd| unsaved_edits_at_close.call(snd) end
            true
          else
            false
          end
        end
      end
    else
      $close_hook.remove_hook!("unsaved-edits-at-close?")
      $exit_hook.remove_hook!("unsaved-edits-at-exit?")
    end
  end
  
  # Remembers the state of a sound when it is closed, and if it is
  # subsequently re-opened, restores the state.
  def remember_sound_state
    # states = {file_name => [date, sound_funcs, channel_funcs], ...}
    states = {}
    sound_funcs = [:sync, :cursor_follows_play]
    channel_funcs = [:time_graph?, :transform_graph?, :lisp_graph?, :x_bounds, :y_bounds,
                     :cursor, :cursor_size, :cursor_style, :show_marks, :show_y_zero,
                     :wavo_hop, :wavo_trace, :max_transform_peaks, :show_transform_peaks,
                     :fft_log_frequency, :fft_log_magnitude, :verbose_cursor, :zero_pad,
                     :wavelet_type, :min_dB, :transform_size, :transform_graph_type,
                     :time_graph_type, :fft_window, :transform_type, :transform_normalization,
                     :time_graph_style, :show_mix_waveforms, :dot_size, :x_axis_style,
                     :show_axes, :graphs_horizontal, :lisp_graph_style, :transform_graph_style]
    $close_hook.add_hook!("remember-sound-state") do |snd|
      states[file_name(snd)] = [file_write_date(file_name(snd)),
                                sound_funcs.map do |f| send(f, snd) end,
                                (0..channels(snd) - 1).to_a.map do |chn|
                                  channel_funcs.map do |f| send(f, snd, chn) end
                                end]
      false
    end
    $after_open_hook.add_hook!("remember-sound-state") do |snd|
      state = states[file_name(snd)]
      if state.kind_of?(Array) and (not state.empty?)
        if file_write_date(file_name(snd)) == state[0]
          sound_funcs.zip(state[1]) do |f, val|
            send(format("set_%s", f.to_s).intern, val, snd)
          end
          channels(snd).times do |chn|
            set_squelch_update(true, snd, chn)
            channel_funcs.zip(state[2][chn]) do |f, val|
              send(format("set_%s", f.to_s).intern, val, snd, chn)
            end
            set_squelch_update(false, snd, chn)
          end
        end
      end
    end
    $open_hook.add_hook!("save-state") do |fname|
      if states.empty? and defined? $_saved_remember_sound_states_states_
        states = $_saved_remember_sound_states_states_
      end
      false
    end
    $after_save_state_hook.add_hook!("save-state") do |fname|
      File.open(File.expand_path(fname), "a+") do |f|
        f.printf("\n# from remember_sound_state() in extensions.rb\n")
        f.printf("$_saved_remember_sound_states_states_ = %s\n", states.inspect)
      end
    end
  end
  
  # Mixes in file.  File can be the file name or an array
  # [fname, beg = 0, chn = 0].
  def mix_channel(fdata, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    fname = if fdata.kind_of?(String)
              fdata
            elsif fdata.kind_of?(Array)
              fdata.first
            else
              error("mix_channel(fdata, beg, dur, snd, chn, edpos):
FDATA should be a file name or an array [fname, beg = 0, chn = 0] (%s)", fdata.inspect)
            end
    fbeg = if fdata.kind_of?(String) or fdata.length < 2
             0
           else
             fdata[1]
           end
    fchan = if fdata.kind_of?(String) or fdata.length < 3
              0
            else
              fdata[2]
            end
    len = (dur or (mus_sound_frames(fname) - fbeg))
    error("mix_channel(): no-such-sample (%d)", beg) if beg < 0
    if len > 0
      reader = make_sample_reader(fbeg, fname, fchan)
      map_channel(lambda do |val|
                    val + next_sample(reader)
                  end, beg, len, snd, chn, edpos, "mix_channel")
    end
  end
  
  # Inserts the file.  File can be the file name or an array
  # [fname, beg = 0, chn = 0].
  def insert_channel(fdata, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    fname = if fdata.kind_of?(String)
              fdata
            elsif fdata.kind_of?(Array)
              fdata.first
            else
              error("insert_channel(fdata, beg, dur, snd, chn, edpos):
FDATA should be a file name or an array [fname, beg = 0, chn = 0] (%s)", fdata.inspect)
            end
    fbeg = if fdata.kind_of?(String) or fdata.length < 2
             0
           else
             fdata[1]
           end
    fchan = if fdata.kind_of?(String) or fdata.length < 3
              0
            else
              fdata[2]
            end
    len = (dur or (mus_sound_frames(fname) - fbeg))
    error("insert_channel(): no-such-sample (%d)", beg) if beg < 0
    if len > 0
      reader = make_sample_reader(fbeg, fname, fchan)
      data = make_vct(len)
      vct_map!(data, lambda do | | next_sample(reader) end)
      insert_samples(beg, len, data, snd, chn, edpos)
    end
  end
  
  # The regularized version of redo_edit.
  def redo_channel(edits = 1, snd = false, chn = false)
    if snd and sync(snd).nonzero? and chn
      set_edit_position(edit_position(snd, chn) + edits, snd, chn)
    else
      redo_edit(edits, snd)
    end
  end
  
  # The regularized version of undo_edit.
  def undo_channel(edits = 1, snd = false, chn = false)
    if snd and sync(snd).nonzero? and chn
      set_edit_position([edit_position(snd, chn) - edits, 0].max, snd, chn)
    else
      undo_edit(edits, snd)
    end
  end
  
  # Take breakpoints in ENV, connect with FUNC, apply as envelope to
  # channel handled as a sequence of funcs and scales.
  def any_env_channel(env, beg = 0, dur = false, snd = false, chn = false, edpos = false, &func)
    if env.kind_of?(Array) and (not env.empty?)
      pts = env.length / 2
      if pts == 1
        scale_channel(env[0], beg, dur, snd, chn, edpos)
      else
        x0 = y0 = 0
        x1, y1 = env[0], env[1]
        xrange = (env[-2] - env[0]).to_f
        ramp_beg = beg
        dur = frames(snd, chn) unless dur.kind_of?(Numeric)
        as_one_edit(lambda do | |
                      2.step(env.length - 1, 2) do |i|
                        x0, y0, x1, y1 = x1, y1, env[i], env[i + 1]
                        ramp_dur = (dur * ((x1 - x0) / xrange)).round
                        if y0 == y1
                          scale_channel(y0, ramp_beg, ramp_dur, snd, chn, edpos)
                        else
                          func.call(y0, y1, ramp_beg, ramp_dur, snd, chn, edpos)
                        end
                        ramp_beg += ramp_dur
                      end
                    end)
      end
    end
  end
  
  # vct: angle, incr, off, scl
  def sine_ramp(rmp0, rmp1, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y, data, forward|
                    angle = data[0]
                    incr = data[1]
                    if forward
                      data[0] = angle + incr
                    else
                      data[0] = angle - incr
                    end
                    y * (data[2] + data[3] * (0.5 + 0.5 * cos(angle)))
                  end, beg, dur, snd, chn, edpos, true, lambda do |frag_beg, frag_dur|
                    incr = PI / frag_dur
                    vct(-PI + frag_beg * incr, incr, rmp0, rmp1 - rmp0)
                  end)
  end
  
  def sine_env_channel(env, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    any_env_channel(env, beg, dur, snd, chn, edpos) do |r0, r1, b, d, s, c, e|
      sine_ramp(r0, r1, b, d, s, c, e)
    end
  end
  # sine_env_channel([0, 0, 1, 1, 2, -0.5, 3, 1])
  
  # An obvious extension of this idea is to use the blackman fft window
  # formulas to get sharper sinusoids (i.e. use the sum of n cosines,
  # rather than just 1).
  
  # vct: angle, incr, off, scl
  def blackman4_ramp(rmp0, rmp1, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y, data, forward|
                    angle = data[0]
                    incr = data[1]
                    if forward
                      data[0] = angle + incr
                    else
                      data[0] = angle - incr
                    end
                    cx = cos(angle)
                    y * (data[2] + data[3] *
                                  (0.084037 + cx *
                                             (-0.29145 + cx *
                                                        (0.375696 + cx *
                                                                   (-0.20762 + cx * 0.041194)))))
                  end, beg, dur, snd, chn, edpos, true, lambda do |frag_beg, frag_dur|
                    incr = PI / frag_dur
                    vct(frag_beg * incr, incr, rmp0, rmp1 - rmp0)
                  end)
  end
  
  def blackman4_env_channel(env,  beg = 0, dur = false, snd = false, chn = false, edpos = false)
    any_env_channel(env, beg, dur, snd, chn, edpos) do |r0, r1, b, d, s, c, e|
      blackman4_ramp(r0, r1, b, d, s, c, e)
    end
  end
  
  # Any curve can be used as the connecting line between envelope
  # breakpoints in the same manner -- set up each ramp to take the
  # current position and increment, then return the value in
  # ptree-channel.  A simple one would have a table of values and use
  # array_interp.
  
  # vct: start, incr, off, scl
  def ramp_squared(rmp0, rmp1, symmetric = true,
                   beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y, data, forward|
                    angle = data[0]
                    incr = data[1]
                    if forward
                      data[0] = angle + incr
                    else
                      data[0] = angle - incr
                    end
                    y * (data[2] + angle * angle * data[3])
                  end, beg, dur, snd, chn, edpos, true, lambda do |frag_beg, frag_dur|
                    incr = 1.0 / frag_dur
                    if symmetric and rmp1 < rmp0
                      vct((frag_dur - frag_beg) * incr, -incr, rmp1, rmp0 - rmp1)
                    else
                      vct(frag_beg * incr, incr, rmp0, rmp1 - rmp0)
                    end
                  end)
  end
  
  def env_squared_channel(env, symmetric = true,
                          beg = 0, dur = false, snd = false, chn = false, edpos = false)
    any_env_channel(env, beg, dur, snd, chn, edpos) do |r0, r1, b, d, s, c, e|
      ramp_squared(r0, r1, symmetric, b, d, s, c, e)
    end
  end
  # env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 1])
  
  # vct: start, incr, off, scl, exponent
  def ramp_expt(rmp0, rmp1, exponent, symmetric = true,
                beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y, data, forward|
                    angle = data[0]
                    incr = data[1]
                    if forward
                      data[0] = angle + incr
                    else
                      data[0] = angle - incr
                    end
                    y * (data[2] + exp(log(angle) * data[4]) * data[3])
                  end, beg, dur, snd, chn, edpos, true, lambda do |frag_beg, frag_dur|
                    incr = 1.0 / frag_dur
                    if symmetric and rmp1 < rmp0
                      vct((frag_dur - frag_beg) * incr, -incr, rmp1, rmp0 - rmp1, exponent)
                    else
                      vct(frag_beg * incr, incr, rmp0, rmp1 - rmp0, exponent)
                    end
                  end)
  end
  
  def env_expt_channel(env, exponent, symmetric = true,
                       beg = 0, dur = false, snd = false, chn = false, edpos = false)
    if exponent == 1.0
      env_channel(env, beg, dur, snd, chn, edpos)
    else
      any_env_channel(env, beg, dur, snd, chn, edpos) do |r0, r1, b, d, s, c, e|
        ramp_expt(r0, r1, exponent, symmetric, b, d, s, c, e)
      end
    end
  end
  
  def offset_channel(amount, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y| y + amount end, beg, dur, snd, chn, edpos, true)
  end
  
  def contrast_channel(index, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y|
                    sin(y * HALF_PI + index * sin(y * TWO_PI))
                  end, beg, dur, snd, chn, edpos, false)
  end
end

include Extensions

# extensions.rb ends here
