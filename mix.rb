# mix.rb -- mix.scm --> mix.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Feb 22 13:40:33 CET 2005
# Last: Wed Feb 23 03:29:07 CET 2005

# Commentary:
#
# various mix and track related utilities
#
# module Mix
# mix_sound(file, start)
# delete_all_mixes
# find_mix(sample, snd, chn)
# pan_mix(name, beg, envelope, snd, chn, auto_delete)
# pan_mix_selection(beg, envelope, snd, chn)
# pan_mix_region(reg, beg, envelope, snd, chn)
# pan_mix_vct(v, beg, envelope, snd, chn)
# mix2vct(id)
# save_mix(id, filename)
# mix_maxamp(id)
# snap_mix_to_beat(at_tag_position)
# mix_properties(id)
# set_mix_properties(id, new_val)
# mix_property(key, id)
# set_mix_property(key, val, id)
# mix_name(id)
# set_mix_name(id, name)
# mix_name2id(name)
# mix_click_sets_amp
# delete_all_tracks
# track2vct(trk, chn)
# save_track(trk, filename, chn)
# track_maxamp(id, chn)
# transpose_track(trk, semitones)
# retempo_track(trk, tempo)
# filter_track(trk, fir_filter_coeffs)
# track_properties(id)
# set_track_properties(id, new_val)
# track_property(key, id)
# set_track_property(key, val, id)
# mix_click_info(id)

# Code:

require "examp"

module Mix
  # 
  # === MIX ===
  # 
  def mix_sound(file, start)
    mix(file, start, true)
  end
  
  def delete_all_mixes
    as_one_edit(lambda do (mixes or []).flatten.each do |id| delete_mix(id) end end)
  end
  
  # returns nil or mixer id
  def find_mix(sample, snd = false, chn = false)
    mixes(snd_snd(snd), snd_chn(chn)).detect do |n| mix_position(n) == sample end
  end
  
  # 
  # === PAN-MIX ===
  # 
  def pan_mix(name, beg = 0, envelope = 1.0, snd = false, chn = 0, auto_delete = false)
    index = snd_snd(snd)
    old_with_mix_tags = with_mix_tags
    snd_throw(:no_such_sound, snd) unless sound?(index)
    snd_throw(:no_such_file, name) unless File.exists?(name)
    new_mix = nil
    begin
      set_with_mix_tags(true)
      incoming_chans = mus_sound_chans(name)
      receiving_chans = chans(index)
      old_sync = sync(index)
      track_func = if envelope.kind_of?(Array)
                     envelope
                   else
                     [0, envelope, 1, envelope]
                   end
      if receiving_chans == 1
        if incoming_chans == 1
          id = mix(name, beg, 0, index, 0, true, auto_delete)
          if envelope.kind_of?(Array)
            set_mix_amp_env(id, 0, envelope)
          else
            set_mix_amp(id, 0, envelope)
          end
          new_mix = id
        else
          as_one_edit(lambda do
                        trk = make_track
                        mix0 = mix(name, beg, 0, index, 0, true, auto_delete, trk)
                        mix1 = mix(name, beg, 1, index, 0, true, auto_delete, trk)
                        set_mix_inverted?(mix1, true)
                        set_track_amp_env(trk, track_func)
                        new_mix = mix0
                      end)
        end
      else
        chan0 = chn
        chan1 = (chn + 1) % receiving_chans
        if incoming_chans == 1
          begin
            set_sync(index, false)
            as_one_edit(lambda do
                          trk = make_track
                          mix0 = mix(name, beg, 0, index, chan0, true, auto_delete, trk)
                          mix1 = mix(name, beg, 0, index, chan1, true, auto_delete, trk)
                          set_mix_inverted?(mix1, true)
                          set_track_amp_env(trk, track_func)
                          new_mix = mix0
                        end)
          rescue
          ensure
            set_sync(index, old_sync)
          end
        else
          new_sync = 0
          (sounds or []).each do |s|
            if (sn = sync(s)) >= new_sync
              new_sync = sn + 1
            end
          end
          begin
            set_sync(index, new_sync)
            as_one_edit(lambda do
                          trk = make_track
                          mix0 = mix(name, beg, 0, index, chan0, true, auto_delete, trk)
                          mix1 = mix(name, beg, 1, index, chan1, true, auto_delete, trk)
                          set_mix_inverted?(mix1, true)
                          set_track_amp_env(trk, track_func)
                          new_mix = mix0
                        end)
          rescue
          ensure
            set_sync(index, old_sync)
          end
        end
      end
    rescue
    ensure
      set_with_mix_tags(old_with_mix_tags)
    end
    if mix?(new_mix) and (not old_with_mix_tags)
      if track?(trk = mix_track(new_mix))
        lock_track(trk)
      else
        set_mix_locked?(new_mix, true)
      end
    end
    new_mix
  end

  def pan_mix_selection(beg = 0, envelope = 1.0, snd = false, chn = 0)
    snd_throw(:no_active_selection) unless selection?
    pan_mix(save_selection(snd_tempnam), beg, envelope, snd, chn, true)
  end

  def pan_mix_region(reg, beg = 0, envelope = 1.0, snd = false, chn = 0)
    snd_throw(:no_such_region, reg) unless region?(reg)
    pan_mix(save_region(reg, snd_tempnam), beg, envelope, snd, chn, true)
  end

  def pan_mix_vct(v, beg = 0, envelope = 1.0, snd = false, chn = 0)
    temp_file = snd_tempnam
    fd = open_sound_file(temp_file, 1, srate(snd), "")
    vct2sound_file(fd, v, v.length)
    close_sound_file(fd, 4 * v.length)
    pan_mix(temp_file, beg, envelope, snd, chn, true)
  end

  def mix2vct(id)
    snd_throw(:no_such_mix, id) unless mix?(id)
    len = mix_frames(id)
    v = make_vct(len)
    rd = make_mix_sample_reader(id)
    len.times do |i| v[i] = read_mix_sample(rd) end
    free_sample_reader(rd)
    v
  end

  def save_mix(id, filename)
    snd_throw(:no_such_mix, id) unless mix?(id)
    v = mix2vct(id)
    fd = open_sound_file(filename, 1, srate, "")
    vct2sound_file(fd, v, v.length)
    close_sound_file(fd, 4 * v.length)
  end

  def mix_maxamp(id)
    snd_throw(:no_such_mix, id) unless mix?(id)
    len = mix_frames(id)
    rd = make_mix_sample_reader(id)
    peak = read_mix_sample(rd).abs
    (1...len).each do peak = [peak, read_mix_sample(rd).abs].max end
    free_sample_reader(rd)
    peak
  end

  def snap_mix_to_beat(at_tag_position = false)
    $mix_release_hook.add_hook!(get_func_name) do |id, samps_moved|
      offset = at_tag_position ? mix_tag_position(id) : 0
      samp = samps_moved + mix_position(id) + offset
      snd = mix_home(id)[0]
      chn = mix_home(id)[1]
      bps = beats_per_minute(snd, chn) / 60.0
      sr = srate(snd).to_f
      beat = ((samp * bps) / sr).floor
      lower = ((beat * sr) / bps).floor
      higher = (((beat + 1) * sr) / bps).floor
      set_mix_position(id, if (samp - lower) < (higher - samp)
                             [0, lower - offset].max
                           else
                             higher - offset
                           end)
      true
    end
  end

  #
  # === Mix Properties ===
  # 
  $all_mix_properties = Array.new

  def mix_properties(id)
    $all_mix_properties[id]
  end

  def set_mix_properties(id, new_val)
    if (h = $all_mix_properties[id]).kind_of?(Hash)
      $all_mix_properties[id].update(new_val)
    else
      $all_mix_properties[id] = new_val
    end
  end

  def mix_property(key, id)
    snd_throw(:no_such_mix, id) unless mix?(id)
    if (h = mix_properties(id)).kind_of?(Hash)
      h[key]
    else
      nil
    end
  end

  def set_mix_property(key, val, id)
    snd_throw(:no_such_mix, id) unless mix?(id)
    if (h = mix_properties(id)).kind_of?(Hash)
      h[key] = val
      set_mix_properties(id, h.rehash)
      [key, val]
    else
      set_mix_properties(id, {:mixid, id, key, val})
      [key, val]
    end
  end

  def mix_name(id)
    mix_property(:name, id)
  end

  def set_mix_name(id, name)
    set_mix_property(:name, name, id)
  end

  def mix_name2id(name)
    (mixes or []).detect do |mx| mix_name(mx) == name end or snd_throw(:no_such_mix, name)
  end

  def mix_click_sets_amp
    $mix_click_hook.add_hook!(get_func_name) do |id|
      unless mix_property(:zero, id)
        amps = (0...mix_chans(id)).map do |i| mix_amp(id, i) end
        set_mix_property(:amps, amps, id)
        mix_chans(id).times do |i| set_mix_amp(id, i, 0.0) end
        set_mix_property(:zero, true, id)
      else
        (mix_property(:amps, id) or []).each_with_index do |amp, i| set_mix_amp(id, i, amp) end
        set_mix_property(:zero, false, id)
      end
      true
    end
  end
  
  # 
  # === Track ===
  # 
  def delete_all_tracks
    as_one_edit(lambda do
                  (mixes or []).flatten.each do |id| delete_mix(id) unless mix_track(id).zero? end
                end)
  end

  def track2vct(trk, chn = 0)
    snd_throw(:no_such_track, trk) unless track?(trk)
    snd_throw(:no_such_channel, chn) unless chn >= track_chans(trk)
    len = track_frames(trk, chn)
    v = make_vct(len)
    rd = make_track_sample_reader(trk, chn)
    len.times do |i| v[i] = read_track_sample(rd) end
    free_sample_reader(rd)
    v
  end

  def save_track(trk, filename, chn = true)
    snd_throw(:no_such_track, trk) unless track?(trk)
    chans = track_chans(trk)
    if chn == true and chans == 1 or chan.kind_of?(Numeric) and chn < chans
      v = track2vct(trk, (chan == true ? 0 : chan))
      fd = open_sound_file(filename, 1, srate, format("written by %s", get_func_name))
      vct2sound_file(fd, v, v.length)
      close_sound_file(fd, 4 * v.length)
    else
      if chn == true and chans > 0
        fd = open_sound_file(filename, chans, srate, format("written by %s", get_func_name))
        len = track_frames(trk)
        pos = track_position(trk)
        v = make_vct(chans * len)
        chans.times do |i|
          chan_len = track_frames(trk, i)
          chan_pos = track_position(trk, i) - pos
          rd = make_track_sample_reader(trk, i)
          chan_len.times do |j| v[i + chans * (chan_pos + j)] = read_track_sample(rd) end
          free_sample_reader(rd)
        end
        vct2sound_file(fd, v, v.length)
        close_sound_file(fd, 4 * v.length)
      else
        snd_throw(:no_such_channel, chn)
      end
    end
  end

  def track_maxamp(id, chn)
    snd_throw(:no_such_track, id) unless track?(id)
    len = track_frames(id)
    rd = make_track_sample_reader(id)
    peak = read_track_sample(rd).abs
    (1...len).each do peak = [peak, read_track_sample(rd).abs].max end
    free_sample_reader(rd)
    peak
  end

  def transpose_track(trk, semitones)
    set_track_speed(trk, track_speed(trk) * 2 ** (semitones / 12.0))
  end

  def retempo_track(trk, tempo)
    set_track_tempo(trk, track_tempo(trk) * tempo)
  end

  def filter_track(trk, fir_filter_coeffs)
    snd_throw(:no_such_track, trk) unless track?(trk)
    order = fir_filter_coeffs.length
    chans = track_chans(trk)
    chans.times do |chn|
      beg = track_position(trk, chn)
      dur = track_frames(trk, chn)
      flt = make_fir_filter(order, list2vct(fir_filter_coeffs))
      rd = make_track_sample_reader(trk, chn, 0)
      map_channel(lambda do |y|
                    val = read_track_sample(rd)
                    y + fir_filter(flt, val) - val
                  end, beg, dur + order, false, false, false, get_func_name)
    end
  end

  # 
  # === Track Properties ===
  # 
  $all_track_properties = Array.new

  def track_properties(id)
    $all_track_properties[id]
  end

  def set_track_properties(id, new_val)
    if (h = $all_track_properties[id]).kind_of?(Hash)
      $all_track_properties[id].update(new_val)
    else
      $all_track_properties[id] = new_val
    end
  end

  def track_property(key, id)
    snd_throw(:no_such_track, id) unless track?(id)
    if (h = track_properties(id)).kind_of?(Hash)
      h[key]
    else
      nil
    end
  end

  def set_track_property(key, val, id)
    snd_throw(:no_such_track, id) unless track?(id)
    if (h = track_properties(id)).kind_of?(Hash)
      h[key] = val
      set_track_properties(id, h.rehash)
      [key, val]
    else
      set_track_properties(id, {:trackid, id, key, val})
      [key, val]
    end
  end

  # 
  # === Mix Click Info ===
  # 
  def mix_click_info(id)
    snd_throw(:no_such_mix, id) unless mix?(id)
    info_dialog("Mix info", format("\
      mix id: %d%s
    position: %d (%1.3f secs)
      length: %d (%1.3f secs)
          in: %s[%d]%s%s%s
     scalers: %s
       speed: %s
        envs: %s%s%s",
                                   id,
                                   ((s = mix_name(id)) ?
                                    format("\n    mix name: %s", s.inspect) :
                                      ""),
                                   mix_position(id),
                                   mix_position(id) / srate(mix_home(id)[0]).to_f,
                                   mix_frames(id),
                                   mix_frames(id) / srate(mix_home(id)[0]).to_f,
                                   short_file_name(mix_home(id)[0]),
                                   mix_home(id)[1],
                                   (mix_locked?(id) ? " (locked)" : ""),
                                   (mix_inverted?(id) ? " (inverted)" : ""),
                                   (mix_track(id).nonzero? ?
                                    format("\n       track: %s", mix_track(id)) :
                                      ""),
                                   (0...mix_chans(id)).map do |i| mix_amp(id, i) end.inspect,
                                   mix_speed(id),
                                   (0...mix_chans(id)).map do |i| mix_amp_env(id, i) end.inspect,
                                   (mix_tag_position(id).nonzero? ?
                                    format("\ntag-position: %d", mix_tag_position(id)) :
                                      ""),
                                   ((props = mix_properties(id)) ?
                                    format("\n  properties: %s", props.inspect) :
                                      "")))
    true
  end
end

include Mix

=begin
$close_hook.add_hook!("mix.rb") do |snd|
  $all_mix_properties.delete_if do |h| (not mix?(h[:mixid])) end
  false
end

$mix_click_hook.add_hook!("mix-click-info") do |id|
  mix_click_info(id)
end
=end

# mix.rb ends here
