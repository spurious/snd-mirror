# extensions.rb -- various generally useful Snd extensions (see extensions.scm)

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Sat Jan 03 17:30:23 CET 2004
# Last: Tue Mar 15 03:10:31 CET 2005

# Commentary:
# 
# module Compatibility
#  scan_sound_chans(beg, end, snd, edpos) do |y| ... end
#  map_sound_chans(beg, end, edname, snd, edpos) do |y| ... end
#  scan_all_chans(beg, end, edpos) do |y| ... end
#  map_all_chans(beg, end, edname, edpos) do |y| ... end
#  scan_chans(beg, end, edpos) do |y| ... end
#  map_chans(beg, end, edname, edpos) do |y| ... end
#  scan_across_all_chans(beg, end, snd, edpos) do |data, chns| ... end
#  map_across_all_chans(beg, end, edname, snd, edpos) do |data, chns| ... end
#  
#  forward_sample(count, snd, chn)
#  backward_sample(count, snd, chn)
#  
#  dismiss_all_dialogs
#  
#  back_or_forth_graph(count)
#  forward_graph(count)
#  backward_graph(count)
#  back_or_forth_mix(count)
#  forward_mix(count)
#  backward_mix(count)
#  back_or_forth_mark(count)
#  forward_mark(count)
#  backward_mark(count)
#  sound_data_channel2list(sd, chan)
#  sound_data2list(sd)
#  vct_convolve!(r1, r2)
#  old_map_channel(beg, dur, snd, chn, edpos, edname, &func)
#  mus_bank(gens, amps, in1, in2)
#  oscil_bank(amps, gens, in1, in2)
#  old_formant_bank(amps, gens, in1)
#  vct2samples(samp, samps, data, snd, chn)
#  samples2vct(samp, samps, snd, chn, nv, epos)
#  scale_sound_by(scl, beg, dur, snd, chn, edpos)
#  scale_sound_to(norm, beg, dur, snd, chn)
#
# module Extensions
#  channel_property(key, snd, chn)  set_channel_property(key, val, snd, chn)
#  sound_property(key, snd)         set_sound_property(key, val, snd)
#  channel_sync(snd, chn)           set_channel_sync(val, snd, chn)
#  show_sound_properties(snd)
#  show_channel_properties(snd, chn)
#  
#  remember_all_sound_properties(database, tmp_snd_p)
#  class Remember_sound_properties
#    initialize(database)
#    inspect
#    with_db do |db| ... end
#    load(snd)
#    save(snd)
#    each do |k, v| ... end
#    delete_if do |k, v| ... end
#    contents
#    reorganize
#    help
#  
#  add_comment(comm, samp, snd, chn)
#  remove_comment(comm, snd, chn)
#  show_comments(snd, chn)
#  
#  marks?(more_than_one)
#  all_chans
#  normalized_mix(fname, beg, in_chan, snd, chn)
#  enveloped_mix(fname, beg, env, del_tmp)
#  
#  map_sound_files(*args) do |f| ... end
#  for_each_sound_file(*args) do |f| ... end
#  match_sound_files(*args) do |f| ... end
#  
#  selection_members
#  make_selection(beg, len, snd, chn)
#  delete_selection_and_smooth()
#  eval_over_selection(func)
#  
#  check_for_unsaved_edits(check)
#  remember_sound_state
#  
#  mix_channel(fdata, beg, dur, snd, chn, edpos)
#  insert_channel(fdata, beg, dur, snd, chn, edpos)
#  redo_channel(edits, snd, chn)
#  undo_channel(edits, snd, chn)
#  
#  any_env_channel(env, beg, dur, snd, chn, edpos) do |r0, r1, b, d, s, c, e| ... end
#  sine_ramp(rmp0, rmp1, beg, dur, snd, chn, edpos)
#  sine_env_channel(env, beg, dur, snd, chn, edpos)
#  blackman4_ramp(rmp0, rmp1, beg, dur, snd, chn, edpos)
#  blackman4_env_channel(env, beg, dur, snd, chn, edpos)
#  ramp_squared(rmp0, rmp1, symmetric, beg, dur, snd, chn, edpos)
#  env_squared_channel(env, symmetric, beg, dur, snd, chn, edpos)
#  ramp_expt(rmp0, rmp1, exponent, symmetric, beg, dur, snd, chn, edpos)
#  env_expt_channel(env, exponent, symmetric, beg, dur, snd, chn, edpos)
#  offset_channel(amount, beg, dur, snd, chn, edpos)
#  dither_channel(amnt, beg, dur, snd, chn, edpos)
#  contrast_channel(index, beg, dur, snd, chn, edpos)
#  channels_eql?(snd1, chn1, snd2, chn2, allowable_difference)
#  channels_equal?(snd1, chn1, snd2, chn2, allowable_difference)
#  mono2stereo(new_name, snd1, chn1, snd2, chn2)
#  mono_files2stereo(new_name, chan1_name, chan2_name)
#  stereo2mono(orig_snd, chan1_name, chan2_name)

# Comments are mostly taken from extensions.scm.

# Code:

require "examp"
require "hooks"
include Math

module Compatibility
  # 
  # Snd-4 compatibility stuff
  # 
  add_help(:scan_sound_chans,
           "scan_sound_chans([beg=0, [end=flase, [snd=false, [edpos=false]]]], &proc) \
applies scan_chan with proc to each channel in a sound")
  def scan_sound_chans(beg = 0, fin = false, snd = false, edpos = false, &body)
    # Proc#arity returns -1 instead of 1 on older versions
    if body and body.arity.abs == 1
      val = nil
      if c = (0...chans(snd)).detect do |chn| val = scan_chan(body, beg, fin, snd, chn, edpos) end
        val += [snd_snd(snd), c]
      else
        nil
      end
    else
      snd_raise(:bad_arity, "scan_sound_chans([beg, [end, [snd, [edpos]]]]) do |y| ... end")
    end
  end

  add_help(:map_sound_chans,
           "map_sound_chans([beg=0, [end=flase, [snd=false, [edpos=false]]]], &proc) \
applies map_chan with proc to each channel in a sound")
  def map_sound_chans(beg = 0, fin = false, edname = false, snd = false, edpos = false, &body)
    if body and body.arity.abs == 1
      channels(snd).times do |chn| map_chan(body, beg, fin, edname, snd, chn, edpos) end
    else
      snd_raise(:bad_arity, "map_sound_chans([beg, [end, [ename, [snd, [edpos]]]]]) do |y| ... end")
    end
  end

  add_help(:scan_all_chans,
           "scan_all_chans([beg=0, [end=flase, [edpos=false]]], &proc) \
applies scan_chan with proc to all channels (all sounds)")
  def scan_all_chans(beg = 0, fin = false, edpos = false, &body)
    if body and body.arity.abs == 1
      catch(:done) do
        sounds2array.each do |snd|
          channels(snd).times do |chn|
            if res = scan_chan(body, beg, fin, snd, chn, edpos)
              throw(:done, res += [snd, chn])
            end
          end
        end
        false
      end
    else
      snd_raise(:bad_arity, "scan_all_chans([beg, [end, [edpos]]]) do |y| ... end")
    end
  end

  add_help(:map_all_chans,
           "map_all_chans([beg=0, [end=flase, [edpos=false]]], &proc) \
applies map_chan with proc to all channels (all sounds)")
  def map_all_chans(beg = 0, fin = false, edname = false, edpos = false, &body)
    if body and body.arity.abs == 1
      sounds2array.each do |snd|
        channels(snd).times do |chn|
          map_chan(body, beg, fin, edname, snd, chn, edpos)
        end
      end
    else
      snd_raise(:bad_arity, "map_all_chans([beg, [end, [ename, [edpos]]]]) do |y| ... end")
    end
  end

  add_help(:scan_chan,
           "scan_chans([beg=0, [end=flase, [edpos=false]]], &proc) \
applies scan_chan with proc to all channels sharing current sound's sync" )
  def scan_chans(beg = 0, fin = false, edpos = false, &body)
    if body and body.arity.abs == 1
      current_sync = sync(selected_sound)
      catch(:done) do
        sounds2array.each do |snd|
          if sync(snd) == current_sync
            channels(snd).times do |chn|
              if res = scan_chan(body, beg, fin, snd, chn, edpos)
                throw(:done, res += [snd, chn])
              end
            end
          end
        end
        false
      end
    else
      snd_raise(:bad_arity, "scan_chans([beg, [end, [edpos]]]) do |y| ... end")
    end
  end

  add_help(:map_chan,
           "map_chans([beg=0, [end=flase, [edpos=false]]], &proc) \
applies map_chan with proc to all channels sharing current sound's sync" )
  def map_chans(beg = 0, fin = false, edname = false, edpos = false, &body)
    if body and body.arity.abs == 1
      current_sync = sync(selected_sound)
      sounds2array.each do |snd|
        if sync(snd) == current_sync
          channels(snd).times do |chn|
            map_chan(body, beg, fin, edname, snd, chn, edpos)
          end
        end
      end
    else
      snd_raise(:bad_arity, "map_chans([beg, [end, [ename, [edpos]]]]) do |y| ... end")
    end
  end

  add_help(:scan_across_all_chans,
           "scan_across_all_chans([beg=0, [end=flase, [snd=false, [edpos=false]]]], &proc) \
applies map_chan with proc to all channels in parallel" )
  # body.call(data, chan_num)
  def scan_across_all_chans(beg = 0, fin = false, snd = false, edpos = false, &body)
    chans = all_chans
    chan_num = chans.first.length
    maxlen = 0
    sounds2array.each do |snd|
      channels(snd).times do |chn|
        maxlen = [maxlen, frames(snd, chn)].max
      end
    end
    len = fin.kind_of?(Integer) ? ([fin, maxlen].min - beg) : (maxlen - beg)
    data = make_array(chan_num, 0.0)
    fds = make_array(chan_num) do |chn|
      make_sample_reader(beg, chans[0].shift, chans[1].shift, 1, edpos)
    end
    catch(:done) do
      len.times do |i|
        data.map_with_index! do |d, chn| next_sample(fds[chn]) end
        if newdata = body.call(data, chan_num)
          throw(:done, [newdata, i + beg])
        end
      end
    end
  end
  
  add_help(:map_across_all_chans,
           "map_across_all_chans([beg=0, [end=flase, [edname=false, \
[snd=false, [edpos=false]]]]], &proc) \
applies map_chan with proc to all channels in parallel" )
  def map_across_all_chans(beg = 0, fin = false, edname = false, snd = false, edpos = false, &body)
    snds, chans = all_chans
    chan_num = snds.length
    maxlen = 0
    sounds2array.each do |sd|
      channels(sd).times do |chn|
        maxlen = [maxlen, frames(sd, chn)].max
      end
    end
    len = fin.kind_of?(Integer) ? ([fin, maxlen].min - beg) : (maxlen - beg)
    data = make_array(chan_num)
    fds = make_array(chan_num) do |chn|
      make_sample_reader(beg, snds[chn], chans[chn], 1, edpos)
    end
    filenames = make_array(chan_num) do |chn|
      snd_tempnam
    end
    outgs = make_array(chan_num) do |chn|
      make_sample2file(filenames[chn], 1, Mus_out_format, Mus_next)
    end
    outsamp = 0
    len.times do |i|
      data.map_with_index! do |d, chn| next_sample(fds[chn]) end
      if (newdata = body.call(data, chan_num))
        newdata.each_with_index do |dat, chn| out_any(outsamp, dat, 0, outgs[chn]) end
        outsamp += 1
      end
    end
    filenames.each_with_index do |file, chn|
      mus_close(outgs[chn])
      free_sample_reader(fds[chn])
      if outsamp > 0
        delete_samples(beg, len, snds[chn], chans[chn]) if outsamp != len
        set_samples(beg, outsamp, file, snds[chn], chans[chn], true, edname)
        delete_file(file)
      end
    end
  end
  
  # 
  # Snd-5 compatibility stuff
  # 
  def forward_sample(count = 1, snd = false, chn = false)
    set_cursor(cursor(snd, chn) + count, snd, chn)
  end
  
  def backward_sample(count = 1, snd = false, chn = false)
    set_cursor(cursor(snd, chn) - count, snd, chn)
  end

  # 
  # Snd-6 compatibility stuff
  # 
  def dismiss_all_dialogs
    if provided? "xm"
      dialog_widgets.each do |dialog|
        next unless dialog
        if provided?("snd-motif") and RXtIsManaged(dialog)
          RXtUnmanageChild(dialog)
        end
      end
    end
  end

  # 
  # Snd-7 compatibility stuff
  # 
  def back_or_forth_graph(count)
    if sounds
      cursnd = selected_sound or sounds.first
      curchn = selected_channel or 0
      curpos = 0
      pos = 0
      sndlst = []
      chnlst = []
      sounds.each do |snd|
        channels(snd).times do |chn|
          if cursnd == snd and curchn == chn
            curpos = pos
          end
          pos += 1
          sndlst.push(snd)
          chnlst.push(chn)
        end
      end
      newpos = (curpos + count) % sndlst.length
      set_selected_sound(sndlst[newpos])
      set_selected_channel(chnlst[newpos])
      [selected_sound, selected_channel]
    end
  end
  
  def forward_graph(count = 1)
    back_or_forth_graph(count)
  end
  
  def backward_graph(count = 1)
    back_or_forth_graph(-count)
  end

  def back_or_forth_mix(count, snd, chn)
    if count.nonzero? and (mx = mixes(snd, chn))
      if mx.length == 1
        set_cursor(mix_position(mx.first), snd, chn)
        mx.first
      else
        sorted_mx = mx.sort do |a, b|
          if mix_position(a) < mix_position(b)
            1
          elsif mix_position(a) > mix_position(b)
            -1
          else
            0
          end
        end
        pos = cursor(snd, chn)
        curpos = count > 0 ? -1 : 0
        if pos >= mix_position(sorted_mx.first)
          sorted_mx.each do |m|
            if (count > 0 and pos < mix_position(m)) or (count < 0 and pos <= mix_position(m))
              break
            else
              curpos += 1
            end
          end
        end
        curpos = (curpos + count) % mx.length
        set_cursor(mix_position(sorted_mx[curpos]), snd, chn)
        sorted_mx[curpos]
      end
    else
      false
    end
  end
  
  def forward_mix(count = 1, snd = false, chn = false)
    back_or_forth_mix(count, snd_snd(snd), snd_chn(chn))
  end

  def backward_mix(count = 1, snd = false, chn = false)
    back_or_forth_mix(-count, snd_snd(snd), snd_chn(chn))
  end

  def back_or_forth_mark(count, snd, chn)
    if count.nonzero? and (mk = marks(snd, chn))
      if mk.length == 1
        set_cursor(mark_sample(mk.first), snd, chn)
        mk.first
      else
        sorted_mk = mk.sort do |a, b|
          if mark_sample(a) < mark_sample(b)
            1
          elsif mark_sample(a) > mark_sample(b)
            -1
          else
            0
          end
        end
        pos = cursor(snd, chn)
        curpos = count > 0 ? -1 : 0
        if pos >= mark_sample(sorted_mk.first)
          sorted_mk.each do |m|
            if (count > 0 and pos < mark_sample(m)) or (count < 0 and pos <= mark_sample(m))
              break
            else
              curpos += 1
            end
          end
        end
        curpos = (curpos + count) % len
        set_cursor(mark_sample(sorted_mk[curpos]), snd, chn)
        sorted_mk[curpos]
      end
    else
      false
    end
  end

  def forward_mark(count = 1, snd = false, chn = false)
    back_or_forth_mark(count, snd_snd(snd), snd_chn(chn))
  end

  def backward_mark(count = 1, snd = false, chn = false)
    back_or_forth_mark(-count, snd_snd(snd), snd_chn(chn))
  end
  
  def sound_data_channel2list(sd, chan)
    v = make_vct(sound_data_length(sd))
    sound_data2vct(sd, chan, v)
    vct2list(v)
  end

  add_help(:sound_data2list,
           "sound_data2list(sd) \
turns a sound-data object's data into a list of lists (one for each channel)")
  def sound_data2list(sd)
    make_array(sound_data_chans(sd)) do |chn|
      sound_data_channel2list(sd, chn)
    end
  end

  def vct_convolve!(r1, r2)
    convolution(r1, r2, r1.length)
  end

  def old_map_channel(beg = 0, dur = false,
                      snd = false, chn = false, edpos = false, edname = false, &func)
    map_channel(lambda { |y|
                  if (val = func.call(y)).kind_of?(Array)
                    vector2vct(val)
                  else
                    val
                  end
                }, beg, dur, snd, chn, edpos, edname)
  end
  
  def mus_bank(gens, amps, in1 = false, in2 = false)
    sum = 0.0
    if amps.kind_of?(Array)
      amps = vector2vct(amps)
    end
    inp1 = in1.kind_of?(Array) ? vector2vct(in1) : (in1 or false)
    inp2 = in2.kind_of?(Array) ? vector2vct(in2) : (in2 or false)
    gens.each_with_index do |gen, i|
      sum += amps[i] * mus_run(gen, (inp1 ? inp1[i] : 0.0), (inp2 ? inp2[i] : 0.0))
    end
    sum
  end

  def oscil_bank(amps, gens, in1 = false, in2 = false)
    sum = 0.0
    if amps.kind_of?(Array)
      amps = vector2vct(amps)
    end
    inp1 = in1.kind_of?(Array) ? vector2vct(in1) : (in1 or false)
    inp2 = in2.kind_of?(Array) ? vector2vct(in2) : (in2 or false)
    gens.each_with_index do |gen, i|
      sum += amps[i] * oscil(gen, (inp1 ? inp1[i] : 0.0), (inp2 ? inp2[i] : 0.0))
    end
    sum
  end

  def old_formant_bank(amps, gens, in1 = 0.0)
    formant_bank(amps.kind_of?(Array) ? vector2vct(amps) : amps, gens, in1)
  end
  
  def vct2samples(samp, samps, data, snd = false, chn = false)
    vct2channel(data, samp, samps, snd, chn)
  end
  
  def samples2vct(samp, samps, snd = false, chn = false, nv = false, epos = false)
    if nv
      vct_subseq(channel2vct(samp, samps, snd, chn, epos), 0, samps, nv)
    else
      channel2vct(samp, samps, snd, chn, epos)
    end
  end

  def scale_sound_by(scl, beg = false, dur = false, snd = false, chn = false, edpos = false)
    if chn.kind_of?(Integer)
      scale_channel(scl, beg, dur, snd, chn, edpos)
    else
      channels(snd).times do |c| scale_channel(scl, beg, dur, snd, c) end
    end
  end

  def scale_sound_to(norm, beg = false, dur = false, snd = false, chn = false)
    if chn.kind_of?(Integer)
      mx = maxamp(snd, chn)
      if mx.nonzero? and mx != norm
        scale_channel(norm / mx, beg, dur, snd, chn)
      end
    else
      mx = maxamp(snd, true).max
      if mx.nonzero? and mx != norm
        channels(snd).times do |c| scale_channel(norm / mx, beg, dur, snd, c) end
      end
    end
  end
end

include Compatibility

module Extensions
  add_help(:channel_property,
           "channel_property(key, snd, chn) \
returns the value associated with 'key' in the given channel's property list, or nil")
  def channel_property(key, snd = false, chn = false)
    if (h = channel_properties(snd, chn)).kind_of?(Hash)
      h[key]
    else
      nil
    end
  end
  
  add_help(:set_channel_property,
           "set_channel_property(key, val, snd, chn) \
sets 'key' to val in the given channel's property list and returns 'val'")
  def set_channel_property(key, val, snd = false, chn = false)
    if (h = channel_properties(snd, chn)).kind_of?(Hash)
      h[key] = val
      set_channel_properties(h.rehash, snd, chn)
    else
      set_channel_properties({key, val}, snd, chn)
    end
    val
  end

  add_help(:sound_property,
           "sound_property(key, snd) \
returns the value associated with 'key' in the given sound's property list, or nil")
  def sound_property(key, snd = false)
    if (h = sound_properties(snd)).kind_of?(Hash)
      h[key]
    else
      nil
    end
  end
  
  add_help(:set_sound_property,
           "set_sound_property(key, val, snd) \
sets 'key' to 'val' in the given sound's property list and returns 'val'.")
           
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

  def show_sound_properties(snd = false)
    if props = sound_properties(snd)
      message("sound-properties of %s", file_name(snd).inspect)
      props.each do |k, v|
        message("%s --> %s", k, v.inspect)
      end
    else
      message("%s has no sound-properties", file_name(snd).inspect)
    end
    nil
  end

  def show_channel_properties(snd = false, chn = false)
    message("channel-properties of %s", file_name(snd).inspect)
    if chn.kind_of?(Integer)
      if props = channel_properties(snd, chn)
        message("==> channel %d <==", chn)
        props.each do |k, v|
          message("%s --> %s", k, v.inspect)
        end
      else
        message("==> channel %d has no channel-properties <==", chn)
      end
    else
      channels(snd).times do |chn|
        if props = channel_properties(snd, chn)
          message("==> channel %d <==", chn)
          props.each do |k, v|
            message("%s --> %s", k, v.inspect)
          end
        else
          message("==> channel %d has no channel-properties <==", chn)
        end
      end
    end
    nil
  end

  # [MS] Based on the idea of remember_sound_state() below, here is
  # another approach, using a database file.
  #
  # remember_all_sound_properties() in ~/.snd-ruby.rb installs the hooks
  # default database is ~/.snd-properties.db
  # default tmp_snd_p is false (discarding `.*0000_00.snd' and `.*.rev.*' filenames)
  def remember_all_sound_properties(database = ENV['HOME'] + "/.snd-properties", tmp_snd_p = false)
    rsp = Remember_sound_properties.new(database)
    unless $after_open_hook.member?("save-property-hook")
      $after_open_hook.add_hook!("save-property-hook") do |snd|
        rsp.load(snd)
      end
      $close_hook.add_hook!("save-property-hook") do |snd|
        if tmp_snd_p or (not file_name(snd) =~ /(.+\d+_\d+\.snd$)|(.*\.rev.*$)/)
          rsp.save(snd)
        end
        false
      end
    end
    rsp
  end

  class Remember_sound_properties
    add_help(:remember_sound_properties, "\
# class Remember_sound_properties
#   initialize(database)
#
# getter:
#   database
#
# methods:
#   inspect
#   with_db do ... end
#   load(snd)
#   save(snd)
#   each do |file, value| ... end
#   delete_if do |file, value| ... end
#   contents
#   reorganize
#   help           (alias info and description)
#
# Usage:
#
# rsp = Remember_sound_properties.new(database)
# unless $after_open_hook.member?(\"save-property-hook\")
#   $after_open_hook.add_hook!(\"save-property-hook\") do |snd|
#     rsp.load(snd)
#   end
#   $close_hook.add_hook!(\"save-property-hook\") do |snd|
#     # discarding `.*0000_00.snd' and `.*.rev.*' filenames
#     if tmp_snd_p or (not file_name(snd) =~ /(.+\\d+_\\d+\\.snd$)|(.*\\.rev.*$)/)
#       rsp.save(snd)
#     end
#     false
#   end
# end
#
# rsp.delete_if do |file, value|
#   file =~ /rb\\-test/
# end                        # deletes all files containing the string \"rb-test\"
# rsp.contents               # prints all filenames in database
# rsp.reorganize             # reorganizes the GDBM database
# rsp.each do |file, value|
#   message(file)
# end                        # the same as rsp.contents
# rsp.with_db do |db|
#   db.reorganize
# end                        # the same as rsp.reorganize")

    include Enumerable
    include Info
    with_silence do
      unless defined? GDBM.open
        require "gdbm"
      end
    end
    
    def initialize(database)
      @database = database
      @sound_funcs = [:sync, :cursor_follows_play]
      @channel_funcs = [:time_graph?, :transform_graph?, :lisp_graph?, :x_bounds, :y_bounds,
                        :cursor, :cursor_size, :cursor_style, :show_marks, :show_y_zero,
                        :wavo_hop, :wavo_trace, :max_transform_peaks, :show_transform_peaks,
                        :fft_log_frequency, :fft_log_magnitude, :verbose_cursor, :zero_pad,
                        :wavelet_type, :min_dB, :transform_size, :transform_graph_type,
                        :time_graph_type, :fft_window, :transform_type, :transform_normalization,
                        :time_graph_style, :show_mix_waveforms,
                        :dot_size, :x_axis_style, :show_axes, :graphs_horizontal,
                        :lisp_graph_style, :transform_graph_style]
      set_help
    end
    attr_reader :database
    alias help description
    
    def inspect
      format("#<%s: database: %s>", self.class, @database.inspect)
    end

    def with_db(&body)
      db = GDBM.open(@database)
      ret = body.call(db)
      db.close
      ret
    end
    
    def load(snd)
      snd_name = file_name(snd)
      with_db do |db|
        eval((db[snd_name] or ""))
      end
      if file_write_date(snd_name) == sound_property(:current_file_time, snd)
        @sound_funcs.each do |prop|
          if (val = sound_property(prop, snd))
            set_snd_var(prop, val, snd)
          end
        end
        channels(snd).times do |chn|
          set_squelch_update(true, snd, chn)
          @channel_funcs.each do |prop|
            if (val = channel_property(prop, snd, chn))
              set_snd_var(prop, val, snd, chn)
            end
          end
          set_squelch_update(false, snd, chn)
        end
      else
        @sound_funcs.each do |prop|
          new_prop = (sound_properties(snd) or {})
          new_prop.delete_if do |k, v| k == prop end
          set_sound_properties(new_prop.rehash, snd)
        end
        channels(snd).times do |chn|
          @channel_funcs.each do |prop|
            new_prop = (channel_properties(snd, chn) or {})
            new_prop.delete_if do |k, v| k == prop end
            set_channel_properties(new_prop.rehash, snd, chn)
          end
        end
      end
    end
    
    # the resulting string looks like
    # let(find_sound("snd_name")) do |snd|
    #   set_sound_properties(Marshal.load("marshaled props"), snd)
    #   if channels(snd) > 0
    #     set_channel_properties(Marshal.load("marshaled props"), snd, 0)
    #   end
    #   if channels(snd) > 1
    #     set_channel_properties(Marshal.load("marshaled props"), snd, 1)
    #   end
    #   ...
    # end
    # self.load evals this string
    def save(snd)
      snd_name = file_name(snd)
      props = (sound_properties(snd) or {})
      props[:current_file_time] = file_write_date(snd_name)
      @sound_funcs.each do |prop|
        props[prop] = snd_var(prop, snd)
      end
      res = format("let(find_sound(%s)) do |snd|\n", snd_name.inspect)
      res += format("  set_sound_properties(Marshal.load(%s), snd)\n", Marshal.dump(props).inspect)
      channels(snd).times do |chn|
        props = (channel_properties(snd, chn) or {})
        @channel_funcs.each do |prop|
          props[prop] = snd_var(prop, snd, chn)
        end
        res += format("  if channels(snd) > %d\n", chn)
        res += format("    set_channel_properties(Marshal.load(%s), snd, %d)\n",
                      Marshal.dump(props).inspect, chn)
        res += "  end\n"
      end
      res += "end\n"
      with_db do |db|
        db[snd_name] = res
      end
    end

    def each
      with_db do |db|
        db.each do |key, value|
          yield(key, value)
        end
      end
    end
    
   def delete_if(&body)
     with_db do |db|
       db.delete_if(&body)
     end
   end

    def contents
      message("contents of %s", @database.inspect)
      with_db do |db|
        db.each do |file, value|
          message(file)
        end
      end
      nil
    end

    def reorganize
      with_db do |db|
        db.reorganize
      end
    end
  
    private
    def set_help
      self.description = get_help(:remember_sound_properties)
    end
  end

  # add channel comments (see extsnd.html->Graphics->draw-string)
  def add_comment(comm, samp = cursor(), snd = selected_sound(), chn = selected_channel())
    comments = (channel_property(:comments, snd, chn) or {})
    comments[samp] = comm
    set_channel_property(:comments, comments, snd, chn)
  end

  def remove_comment(comm, snd = selected_sound(), chn = selected_channel())
    comments = (channel_property(:comments, snd, chn) or {})
    comments.delete_if do |k, v| v == comm end
    set_channel_property(:comments, comments.rehash, snd, chn)
  end

  def show_comments(snd, chn)
    (channel_property(:comments, snd, chn) or {}).each do |samp, text|
      width = 6 * text.length
      if samp.between?(left_sample(snd, chn), right_sample(snd, chn))
        xpos = x2position(samp / srate(snd).to_f, snd, chn)
        ypos = y2position(sample(samp), snd, chn)
        draw_line(xpos, 35, xpos, ypos - 4, snd, chn)
        draw_string(text, xpos - width / 2, 18, snd, chn)
      end
    end
  end
  # $after_graph_hook.add_hook!("show-comments") do |snd, chn| show_comments(snd, chn) end

  
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
  
  add_help(:all_chans,
           "all_chans() \
-> two parallel lists, the first snd indices, the second channel numbers.  \
If we have two sounds open (indices 0 and 1 for example), and the second has two channels, \
all_chans returns [[0, 1, 1], [0, 0, 1]]")
  def all_chans
    sndlist = []
    chnlist = []
    sounds2array.each do |snd|
      channels(snd).times do |chn|
        sndlist << snd
        chnlist << chn
      end
    end
    [sndlist, chnlist]
  end
  
  add_help(:normalized_mix,
           "normalized_mix(filename, beg, in_chan, snd, chn) \
is like mix but the mix result has same peak amp as unmixed snd/chn (returns scaler)")
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

  add_help(:enveloped_mix,
           "enveloped_mix(filename, beg, env) \
mixes filename starting at beg with amplitude envelope env. \
enveloped_mix(\"pistol.snd\", 0, [0, 0, 1, 1, 2, 0])")
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

  add_help(:map_sound_files,
           "map_sound_files(*dir_array) do |file| ... end \
applies body to each sound file in dir")
  def map_sound_files(*args, &func)
    args << "." if args.empty?
    args.map do |dir|
      sound_files_in_directory(dir).map do |f| func.call(f) end if File.exist?(dir)
    end
  end
  # map_sound_files(".", "/usr/gnu/sound/SFiles") do |f| play_and_wait(f) end
  
  add_help(:for_each_sound_file,
           "for_each_sound_file(*dir_args) do |file| ... end \
applies body to each sound file in dir")
  def for_each_sound_file(*args, &func)
    args << "." if args.empty?
    args.each do |dir|
      sound_files_in_directory(dir).each do |f| func.call(f) end if File.exist?(dir)
    end
  end
  # for_each_sound_file(".", "/usr/gnu/sound/SFiles") do |f| play_and_wait(f) end

  add_help(:match_sound_files,
           "match_sound_files(*dir_args) do |file| ... end \
applies func to each sound file in dir and returns a list of files \
for which body does not return false")
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
  
  add_help(:selection_members,
           "selection_members() \
-> list of lists of [snd, chn] indicating the channels participating in the current selection.")
  def selection_members
    sndlist = []
    if selection?
      sounds2array.each do |snd|
        channels(snd).times do |i|
          sndlist.push([snd, i]) if selection_member?(snd, i)
        end
      end
    end
    sndlist
  end
  
  add_help(:make_selection,
           "make_selection([beg=0, [end=false, [snd=false, [chn=false]]]]) \
makes a selection like make-region but without creating a region.
make_selection follows snd's sync field, and applies to all snd's channels if chn is not \
specified. end defaults to end of channel, beg defaults to 0, \
snd defaults to the currently selected sound.")
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
    current_sound = (snd or selected_sound() or (sounds and sounds[0]))
    unless sound?(current_sound)
      error("make_selection(%s, %s, %s, %s): no-such-sound",
            beg.inspect, len.inspect, snd.inspect, chn.inspect)
    end
    current_sync = sync(current_sound)
    if selection?
      sounds2array.each do |s|
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
      sounds2array.each do |s|
        if snd == true or s == current_sound or (current_sync.nonzero? and current_sync == sync(s))
          channels(s).times do |i|
            add_chan_to_selection.call(beg, len, s, i)
          end
        end
      end
    end
  end
  
  add_help(:delete_selection_and_smooth,
           "delete_selection_and_smooth() deletes the current selection and smooths the splice")
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

  add_help(:eval_over_selection,
          "eval_over_selection(func) evaluates func on each sample in the current selection")
  def eval_over_selection(func)
    if func.kind_of?(Proc) and selection?
      beg = selection_position()
      len = selection_frames()
      sndlst, chnlst = all_chans()
      sndlst.zip(chnlst) do |snd, chn|
        if selection_member?(snd, chn)
          new_data = make_vct(len)
          old_data = channel2vct(beg, len, snd, chn)
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
  add_help(:check_for_unsaved_edits,
           "check_for_unsaved_edits([check=true]) \
-> sets up hooks to check for and ask about unsaved edits when a sound is closed.
If 'check' is false, the hooks are removed.")
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
          if sounds2array.detect do |snd| unsaved_edits_at_close.call(snd) end
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
  
  add_help(:remember_sound_state,
           "remember_sound_state() \
remembers the state of a sound when it is closed, \
and if it is subsquently re-opened, restores that state")
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
                                sound_funcs.map do |f| snd_var(f, snd) end,
                                (0..channels(snd) - 1).to_a.map do |chn|
                                  channel_funcs.map do |f| snd_var(f, snd, chn) end
                                end]
      false
    end
    $after_open_hook.add_hook!("remember-sound-state") do |snd|
      state = states[file_name(snd)]
      if state.kind_of?(Array) and (not state.empty?)
        if file_write_date(file_name(snd)) == state[0]
          sound_funcs.zip(state[1]) do |f, val|
            set_snd_var(f, val, snd)
          end
          channels(snd).times do |chn|
            set_squelch_update(true, snd, chn)
            channel_funcs.zip(state[2][chn]) do |f, val|
              set_snd_var(f, val, snd, chn)
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
  
  add_help(:mix_channel,
           "mix_channel(file, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]) \
mixes in file. file can be the file name or a list [file_name, beg = 0, chn = 0]")
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
  
  add_help(:insert_channel,
           "insert_channel(file, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]) \
inserts the file. file can be the file name or a list [file_name, beg = 0, chn = 0]" )
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
  
  add_help(:redo_channel,
           "redo_channel([edits=1, [snd=false, [chn=false]]]) is the regularized version of redo")
  def redo_channel(edits = 1, snd = false, chn = false)
    if snd and sync(snd).nonzero? and chn
      set_edit_position(edit_position(snd, chn) + edits, snd, chn)
    else
      redo_edit(edits, snd)
    end
  end
  
  add_help(:undo_channel,
          "undo_channel([edits=1, [snd=false, [chn=false]]]) is the regularized version of undo" )
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
  add_help(:sine_ramp,
           "sine_ramp(rmp0, rmp1, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]) \
produces a sinsusoidal connection from rmp0 to rmp1")
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

  add_help(:sine_env_channel,
           "sine_env_channel(env, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]) \
connects env's dots with sinusoids")
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
  add_help(:ramp_squared,
           "ramp_squared(rmp0, rmp1, [symmetric=true, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]]) \
connects rmp0 and rmp1 with an x^2 curve")
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

  add_help(:env_squared_channel,
           "(env-squared-channel(env, [symmetric=true, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]]) \
connects env's dots with x^2 curves")
  def env_squared_channel(env, symmetric = true,
                          beg = 0, dur = false, snd = false, chn = false, edpos = false)
    any_env_channel(env, beg, dur, snd, chn, edpos) do |r0, r1, b, d, s, c, e|
      ramp_squared(r0, r1, symmetric, b, d, s, c, e)
    end
  end
  # env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 1])
  
  # vct: start, incr, off, scl, exponent
  add_help(:ramp_expt,
           "ramp_expt(rmp0, rmp1, exponent, [symmetric=true, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]]) connects rmp0 and rmp1 with an x^exponent curve")
  def ramp_expt(rmp0, rmp1, exponent, symmetric = true,
                beg = 0, dur = false, snd = false, chn = false, edpos = false)
    rmp0 = rmp0.to_f
    rmp1 = rmp1.to_f
    exponent = exponent.to_f
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
                    frag_beg = frag_beg.to_f
                    frag_dur = frag_dur.to_f
                    incr = 1.0 / frag_dur
                    if symmetric and rmp1 < rmp0
                      vct((frag_dur - frag_beg) * incr, -incr, rmp1, rmp0 - rmp1, exponent)
                    else
                      vct(frag_beg * incr, incr, rmp0, rmp1 - rmp0, exponent)
                    end
                  end)
  end

  add_help(:env_expt_channel,
           "env_expt_channel(env, exponent, [symmetric=true, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]]) connects env'e dots with x^exponent curves")
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
  
  add_help(:offset_channel,
           "offset_channel(amount, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]) \
adds amount to each sample")
  def offset_channel(amount, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y| y + amount end, beg, dur, snd, chn, edpos, true)
  end

  add_help(:dither_channel,
           "dither_channel([amount, 0.00006, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]]) adds amount dither to each sample")
  def dither_channel(amnt = 0.00006, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    dither = 0.5 * amnt
    ptree_channel(lambda do |y| mus_random(dither) + mus_random(dither) + y end,
                  beg, dur, snd, chn, edpos, true, false,
                  format("dither_channel %1.8f %s %s", amnt, beg, dur))
  end

  add_help(:contrast_channel,
           "contrast_channel(index, [beg=0, [dur=false, [snd=false, [chn=false, [edpos=false]]]]]) \
applies contrast enhancement to the sound" )
  def contrast_channel(index, beg = 0, dur = false, snd = false, chn = false, edpos = false)
    ptree_channel(lambda do |y|
                    sin(y * HALF_PI + index * sin(y * TWO_PI))
                  end, beg, dur, snd, chn, edpos, false)
  end

  # channels-equal
  add_help(:channels_eql?,
           "channels_eql?(s1, c1, s2, c2, [diff, 0.0]) \
-> true if the two channels are the same (within diff) modulo trailing 0's")
  def channels_eql?(snd1, chn1, snd2, chn2, allowable_difference = 0.0)
    if snd1 == snd2 and chn1 == chn2
      true
    else
      mx1 = maxamp(snd1, chn1)
      mx2 = maxamp(snd2, chn2)
      if (mx1 - mx2).abs > allowable_difference
        false
      else
        len1 = frames(snd1, chn1)
        len2 = frames(snd2, chn2)
        first_longer = (len1 >= len2)
        len = first_longer ? len1 : len2
        s1 = first_longer ? snd1 : snd2
        s2 = first_longer ? snd2 : snd1
        c1 = first_longer ? chn1 : chn2
        c2 = first_longer ? chn2 : chn1
        read2 = make_sample_reader(0, s2, c2)
        (not scan_channel(lambda do |y|
                            val = read_sample(read2)
                            (val - y).abs > allowable_difference
                          end, 0, len, s1, c1))
      end
    end
  end

  add_help(:channels_equal?,
           "channels_equal?(s1, c1, s2, c2, [diff=0.0]) \
-> true if the two channels are the same (within diff)")
  def channels_equal?(snd1, chn1, snd2, chn2, allowable_difference = 0.0)
    len1 = frames(snd1, chn1)
    len2 = frames(snd2, chn2)
    if len1 == len2
      channles_eql?(snd1, chn1, snd2, chn2, allowable_difference)
    else
      false
    end
  end

  # mono->stereo, mono-files->stereo
  def mono2stereo(new_name, snd1, chn1, snd2, chn2)
    old_ed1 = edit_position(snd1, chn1)
    old_ed2 = edit_position(snd2, chn2)
    ind = new_sound(new_name, :channels, 2, :srate, srate(snd1))
    swap_channels(ind, 0, snd1, chn1)
    swap_channels(ind, 1, snd2, chn2)
    set_edit_position(old_ed1, snd1, chn1)
    set_edit_position(old_ed2, snd2, chn2)
    ind
  end
  # mono2stereo("test.snd", 0, 0, 1, 0)

  def mono_files2stereo(new_name, chan1_name, chan2_name)
    ind1 = open_sound(chan1_name)
    ind2 = open_sound(chan2_name)
    ind3 = mono2stereo(new_name, ins1, 0, ind2, 0)
    close_sound(ind1)
    close_sound(ind2)
    ind3
  end
  # mono_files2stereo("test.snd", "oboe.snd", "pistol.snd")

  def stereo2mono(orig_snd, chan1_name, chan2_name)
    old_ed0 = edit_position(orig_snd, 0)
    old_ed1 = edit_position(orig_snd, 1)
    chan1 = new_sound(chan1_name, :srate, srate(orig_snd))
    chan2 = new_sound(chan2_name, :srate, srate(orig_snd))
    swap_channels(orig_snd, 0, chan1, 0)
    swap_channels(orig_snd, 1, chan2, 0)
    set_edit_position(old_ed0, orig_snd, 0)
    set_edit_position(old_ed1, orig_snd, 1)
    [chan1, chan2]
  end
  # stereo2mono(0, "hi1.snd", "hi2.snd")
end

include Extensions

# extensions.rb ends here
