# mix.rb -- mix.scm --> mix.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Feb 22 13:40:33 CET 2005
# Changed: Mon Dec 11 05:02:03 CET 2006

# Commentary:
#
# various mix and track related utilities
#
# module Mix (see mix.scm)
#  mix_sound(file, start)
#  delete_all_mixes
#  find_mix(sample, snd, chn)
#  pan_mix(name, beg, envelope, snd, chn, auto_delete)
#  pan_mix_selection(beg, envelope, snd, chn)
#  pan_mix_region(reg, beg, envelope, snd, chn)
#  pan_mix_vct(v, beg, envelope, snd, chn)
#  mix2vct(id)
#  save_mix(id, filename)
#  mix_maxamp(id)
#  snap_mix_to_beat(at_tag_position)
#
#  mix_properties(id)
#  set_mix_properties(id, new_val)
#  remove_mix_properties(id)
#  mix_property(id, key)
#  set_mix_property(id, key, val)
#  remove_mix_property(id, key)
#  mix_name2id(name)
#  mix_click_sets_amp
#
#  delete_all_tracks
#  reverse_track(trk)
#  track2vct(trk, chn)
#  save_track(trk, filename, chn)
#  track_maxamp(id, chn)
#  transpose_track(trk, semitones)
#  retempo_track(trk, tempo)
#  filter_track(trk, fir_filter_coeffs)
#
#  track_properties(id)
#  set_track_properties(id, new_val)
#  remove_track_properties(id)
#  track_property(id, key)
#  set_track_property(id, key, val)
#  remove_track_property(id, key)
#  track_name2id(name)
#  mix_click_info(id)
#
# module Mixer_matrix (see mixer.scm)
#  mixer2matrix(mx)
#  matrix2mixer(mat)
#  mixer_equal?(mx1, mx2)
#  mixer_diagonal?(mx)
#  mixer_symmetric?(mx)
#  mixer_hermitian?(mx)
#  mixer_determinant(mx)
#  mixer_transpose(mx)
#  mixer_inverse(mx)
#  mixer_poly(mx, *coeffs)
#  mixer_normal?(mx)
#  mixer_orthogonal?(mx)
#  mixer_unitary?(mx)
#  mixer_trace(mx)
#  mixer_solve(a, b)
#  frame_reverse(fr)
#  make_zero_mixer(n)
#  

# Code:

require "examp"

module Mix
  # 
  # === MIX ===
  #
  add_help(:mix_sound,
           "mix(sound(file, start) \
mixes file (all chans) at start in the currently selected sound.")
  def mix_sound(file, start)
    mix(file, start, true)
  end

  add_help(:delete_all_mixes, "delete_all_mixes() removes all mixes (sets all amps to 0)")
  def delete_all_mixes
    as_one_edit_rb(get_func_name) do (mixes or []).flatten.each do |id| delete_mix(id) end end
  end

  add_help(:find_mix,
           "find_mix(sample, [snd=false, [chn=false]]) \
returns the id of the mix at the given sample, or nil")
  def find_mix(sample, snd = false, chn = false)
    (mixes(Snd.snd(snd), Snd.chn(chn)) or []).detect do |n| mix_position(n) == sample end
  end
  
  # 
  # === PAN-MIX ===
  # 
  add_help(:pan_mix,
           "pan_mix(file, [start=0, [envelope=1.0, [snd=false, [chn=0, auto_delete=false]]]]) \
mixes 'file' into the sound 'snd' starting at start (in samples) \
using 'envelope' to pan (0: all chan 0, 1: all chan 1).
So, pan_mix(\"oboe.snd\", 0.1, [0, 0, 1, 1]) goes from all chan 0 to all chan 1.  \
If 'envelope' is a scaler, it is turned into an evelope at that value.  \
'auto_delete' determines whether the in-coming file should be treated as a temporary file \
and deleted when the mix is no longer accessible.")
  def pan_mix(name, beg = 0, envelope = 1.0, snd = false, chn = 0, auto_delete = false)
    index = Snd.snd(snd)
    old_with_mix_tags = with_mix_tags
    Snd.raise(:no_such_sound, snd) unless sound?(index)
    Snd.raise(:no_such_file, name) unless File.exist?(name)
    new_mix = nil
    begin
      set_with_mix_tags(true)
      incoming_chans = mus_sound_chans(name)
      receiving_chans = channels(index)
      old_sync = sync(index)
      track_func = if array?(envelope)
                     envelope
                   else
                     [0, envelope, 1, envelope]
                   end
      if receiving_chans == 1
        if incoming_chans == 1
          id = mix(name, beg, 0, index, 0, true, auto_delete)
          if array?(envelope)
            set_mix_amp_env(id, 0, envelope)
          else
            set_mix_amp(id, 0, envelope)
          end
          new_mix = id
        else
          as_one_edit_rb(get_func_name) do
            trk = make_track
            mix0 = mix(name, beg, 0, index, 0, true, auto_delete, trk)
            mix1 = mix(name, beg, 1, index, 0, true, auto_delete, trk)
            set_mix_inverted?(mix1, true)
            set_track_amp_env(trk, track_func)
            new_mix = mix0
          end
        end
      else
        chan0 = chn
        chan1 = (chn + 1) % receiving_chans
        if incoming_chans == 1
          begin
            set_sync(false, index)
            as_one_edit_rb(get_func_name) do
              trk = make_track
              mix0 = mix(name, beg, 0, index, chan0, true, auto_delete, trk)
              mix1 = mix(name, beg, 0, index, chan1, true, auto_delete, trk)
              set_mix_inverted?(mix1, true)
              set_track_amp_env(trk, track_func)
              new_mix = mix0
            end
          rescue
            raise
          ensure
            set_sync(old_sync, index)
          end
        else
          new_sync = 0
          Snd.sounds.each do |s|
            if (sn = sync(s)) >= new_sync
              new_sync = sn + 1
            end
          end
          begin
            set_sync(new_sync, index)
            as_one_edit_rb(get_func_name) do
              trk = make_track
              mix0 = mix(name, beg, 0, index, chan0, true, auto_delete, trk)
              mix1 = mix(name, beg, 1, index, chan1, true, auto_delete, trk)
              set_mix_inverted?(mix1, true)
              set_track_amp_env(trk, track_func)
              new_mix = mix0
            end
          rescue
            raise
          ensure
            set_sync(old_sync, index)
          end
        end
      end
    rescue
      raise
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

  add_help(:pan_mix_selection,
           "pan_mix_selection([beg=0, [envelope=1.0, [snd=false, [chn=0]]]]) \
mixes the current selection  into the sound 'snd' starting at 'start' (in samples) \
using 'envelope' to pan (0: all chan 0, 1: all chan 1).")
  def pan_mix_selection(beg = 0, envelope = 1.0, snd = false, chn = 0)
    Snd.raise(:no_active_selection) unless selection?
    pan_mix(save_selection(snd_tempnam), beg, envelope, snd, chn, true)
  end

  add_help(:pan_mix_region,
           "pan_mix_region(reg, [beg=0, [envelope=1.0, [snd=false, [chn=0]]]]) \
mixes the given region into the sound 'snd' starting at 'start' (in samples) \
using 'envelope' to pan (0: all chan 0, 1: all chan 1).")
  def pan_mix_region(reg, beg = 0, envelope = 1.0, snd = false, chn = 0)
    Snd.raise(:no_such_region, reg) unless region?(reg)
    pan_mix(save_region(reg, snd_tempnam), beg, envelope, snd, chn, true)
  end

  add_help(:pan_mix_vct,
           "pan_mix_vct(v, [beg=0, [envelope=1.0, [snd=false, [chn=0]]]]) \
mixes the vct data into the sound 'snd' starting at 'start' (in samples) \
using 'envelope' to pan (0: all chan 0, 1: all chan 1).")
  def pan_mix_vct(v, beg = 0, envelope = 1.0, snd = false, chn = 0)
    temp_file = snd_tempnam
    fd = mus_sound_open_output(temp_file, srate(snd), 1, false, false, "")
    mus_sound_write(fd, 0, v.length - 1, 1, vct2sound_data(v))
    mus_sound_close_output(fd, 4 * v.length)
    pan_mix(temp_file, beg, envelope, snd, chn, true)
  end

  add_help(:mix2vct, "mix2vct(id) returns mix's data in vct")
  def mix2vct(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    len = mix_frames(id)
    v = make_vct(len)
    rd = make_mix_sample_reader(id)
    len.times do |i| v[i] = read_mix_sample(rd) end
    free_sample_reader(rd)
    v
  end

  add_help(:save_mix, "save_mix(id, filename) saves mix data (as floats) in file filename")
  def save_mix(id, filename)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    v = mix2vct(id)
    fd = mus_sound_open_output(filename, srate(), 1, false, false, "")
    mus_sound_write(fd, 0, v.length - 1, 1, vct2sound_data(v))
    mus_sound_close_output(fd, 4 * v.length)
  end

  add_help(:mix_maxamp, "mix_maxamp(id) returns the max amp in the given mix")
  def mix_maxamp(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    len = mix_frames(id)
    rd = make_mix_sample_reader(id)
    peak = read_mix_sample(rd).abs
    (1...len).each do peak = [peak, read_mix_sample(rd).abs].max end
    free_sample_reader(rd)
    peak
  end

  add_help(:snap_mix_to_beat,
           "snap_mix_to_beat() \
forces a dragged mix to end up on a beat (see beats-per-minute).  \
reset $mix_release_hook to cancel")
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
    property(id, :mix_property)
  end

  def set_mix_properties(id, new_val)
    set_property(id, :mix_property, new_val)
  end

  def remove_mix_properties(id)
    if hash?(mix_properties(id))
      properties.delete(id)
      $all_mix_properties.delete(id)
    end
  end

  add_help(:mix_property,
           "mix_property(id, key) \
returns the value associated with 'key' in the given mix's property list, or false")
  def mix_property(id, key)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    hash?(h = mix_properties(id)) and h[key]
  end

  add_help(:set_mix_property,
           "set_mix_property(id, key, val) \
sets the value 'val' to 'key' in the given mix's property list")
  def set_mix_property(id, key, val)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    unless hash?(h = mix_properties(id)) and h.store(key, val)
      $all_mix_properties.push(id)
      set_mix_properties(id, {key, val})
    end
  end

  add_help(:remove_mix_property,
           "remove_mix_property(id, key) \
removes the key-value pair in the given mix's property list")
  def remove_mix_property(id, key)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    if hash?(h = mix_properties(id))
      h.delete(key)
    else
      $all_mix_properties.delete(id)
    end
  end
  
=begin  
  $close_hook.add_hook!("remove-mix-properties") do |snd|
    $all_mix_properties.each do |id| (not mix?(id)) and remove_mix_properties(id) end
  end
=end

  add_help(:mix_name2id,
           "mix_name2id(name)  returns the mix id associated with NAME")
  def mix_name2id(name)
    callcc do |ret|
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          mixes(snd, chn).each do |m|
            if mix_name(m) == name
              ret.call(m)
            end
          end
        end
      end
      :no_such_mix
    end
  end

  def mix_click_sets_amp
    $mix_click_hook.add_hook!(get_func_name) do |id|
      unless mix_property(id, :zero)
        amps = (0...mix_chans(id)).map do |i| mix_amp(id, i) end
        set_mix_property(id, :amps, amps)
        mix_chans(id).times do |i| set_mix_amp(id, i, 0.0) end
        set_mix_property(id, :zero, true)
      else
        (mix_property(id, :amps) or []).each_with_index do |amp, i| set_mix_amp(id, i, amp) end
        set_mix_property(id, :zero, false)
      end
      true
    end
  end
  # $mix_click_hook.add_hook!("mix-click-info") do |id| mix_click_info(id) end
  
  # 
  # === Track ===
  # 
  add_help(:delete_all_tracks,
           "delete_all_tracks() \
removes all mixes that have an associated track (sets all amps to 0)")
  def delete_all_tracks
    as_one_edit_rb(get_func_name) do
      (mixes or []).flatten.each do |id| delete_mix(id) unless mix_track(id).zero? end
    end
  end

  add_help(:reverse_track,
           "reverse_track(trk) \
reverses the order of its mixes (it changes various mix begin times)")
  def reverse_track(trk)
    if track(trk).detect do |t| mix?(t) end
      ids_in_order = track(trk).sort do |a, b|
        if mix_position(a) > mix_position(b)
          1
        elsif mix_position(a) < mix_position(b)
          -1
        else
          0
        end
      end
      as_one_edit_rb(get_func_name) do
        ids_in_order.map do |id| mix_position(id) end.reverse.zip(ids_in_order).each do |pos, id|
          set_mix_position(id, pos)
        end
      end
    else
      Snd.raise(:no_such_track, trk)
    end
  end

  add_help(:track2vct, "track2vct(track, [chan=0]) places track data in vct") 
  def track2vct(trk, chn = 0)
    Snd.raise(:no_such_track, trk) unless track?(trk)
    Snd.raise(:no_such_channel, chn) unless chn < track_chans(trk)
    len = track_frames(trk, chn)
    v = make_vct(len)
    rd = make_track_sample_reader(trk, chn)
    len.times do |i| v[i] = read_track_sample(rd) end
    free_sample_reader(rd)
    v
  end

  add_help(:save_track,
           "save_track(track, filename, [chan=true]) saves track data (as floats) in file filename")
  def save_track(trk, filename, chn = true)
    Snd.raise(:no_such_track, trk) unless track?(trk)
    chans = track_chans(trk)
    if chn == true and chans == 1 or number?(chn) and chn < chans
      v = track2vct(trk, (chn == true ? 0 : chn))
      fd = mus_sound_open_output(filename, srate(), 1, false, false, "written by save-track")
      mus_sound_write(fd, 0, v.length - 1, 1, vct2sound_data(v))
      mus_sound_close_output(fd, 4 * v.length)
    else
      if chn == true and chans > 0
        fd = mus_sound_open_output(filename, srate(), chans, false, false, "written by save-track")
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
        mus_sound_write(fd, 0, v.length - 1, 1, vct2sound_data(v))
        mus_sound_close_output(fd, 4 * v.length)
      else
        Snd.raise(:no_such_channel, chn)
      end
    end
  end

  add_help(:track_maxamp, "track_maxamp(id, chan) returns the max amp in the given track")
  def track_maxamp(id, chn)
    Snd.raise(:no_such_track, id) unless track?(id)
    len = track_frames(id)
    rd = make_track_sample_reader(id)
    peak = read_track_sample(rd).abs
    (1...len).each do peak = [peak, read_track_sample(rd).abs].max end
    free_sample_reader(rd)
    peak
  end

  add_help(:transpose_track,
           "transpose_track(track, semitones) transposes each mix in track by semitones")
  def transpose_track(trk, semitones)
    set_track_speed(trk, track_speed(trk) * 2 ** (semitones / 12.0))
  end

  add_help(:retempo_track,
           "retempo_track(track, tempo) \
changes the inter-mix begin times of mixes in track by tempo (> 1.0 is faster)")
  def retempo_track(trk, tempo)
    set_track_tempo(trk, track_tempo(trk) * tempo)
  end

  add_help(:filter_track,
           "filter_track(track, coeffs) \
filters track data using FIR filter coeffs: filter_track(track-id, [0.1, 0.2, 0.3, 0.3, 0.2, 0.1])")
  def filter_track(trk, fir_filter_coeffs)
    Snd.raise(:no_such_track, trk) unless track?(trk)
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
    property(id, :track_property)
  end

  def set_track_properties(id, new_val)
    set_property(id, :track_property, new_val)
  end

  def remove_track_properties(id)
    if hash?(track_properties(id))
      properties.delete(id)
      $all_track_properties.delete(id)
    end
  end

  add_help(:track_property,
           "track_property(id, key) \
returns the value associated with 'key' in the given track's property list, or false")
  def track_property(id, key)
    Snd.raise(:no_such_track, id) unless track?(id)
    hash?(h = track_properties(id)) and h[key]
  end

  add_help(:set_track_property,
           "set_track_property(id, key, val) \
sets the value 'val' to 'key' in the given track's property list")
  def set_track_property(id, key, val)
    Snd.raise(:no_such_track, id) unless track?(id)
    unless hash?(h = track_properties(id)) and h.store(key, val)
      $all_track_properties.push(id)
      set_track_properties(id, {key, val})
    end
  end

  add_help(:remove_track_property,
           "remove_track_property(id, key) \
removes the key-value pair in the given track's property list")
  def remove_track_property(id, key)
    Snd.raise(:no_such_track, id) unless track?(id)
    if hash?(h = track_properties(id))
      h.delete(key)
    else
      $all_track_properties.delete(id)
    end
  end

  add_help(:track_name2id,
           "track_name2id(name)  returns the track id associated with NAME")
  def track_name2id(name)
    callcc do |ret|
      Snd.tracks.each do |trk|
        if track_name(trk) == name
          ret.call(trk)
        end
      end
      :no_such_track
    end
  end
  
=begin  
  $close_hook.add_hook!("remove-track-properties") do |snd|
    $all_track_properties.each do |id| (not track?(id)) and remove_track_properties(id) end
  end
=end

  # 
  # === Mix Click Info ===
  # 
  add_help(:mix_click_info,
           "mix_click_info(n) \
is a $mix_click_hook function that describes a mix and its properties")
  def mix_click_info(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
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

# 
# === MIXER.SCM ===
# 
with_silence do
  require "matrix"
end

class Matrix
  with_silence do
    # local variable rows renamed to rs
    def Matrix.diagonal(*values)
      size = values.size
      rs = (0...size).collect do |j|
        row = Array.new(size).fill(0, 0, size)
        row[j] = values[j]
        row
      end
      rows(rs, false)
    end
    # private :init_rows doesn't work any longer
    public :init_rows
  end
end

module Mixer_matrix
  def mixer2matrix(mx)
    if mixer?(mx)
      a = make_array(mx.channels) do |i|
        make_array(mx.channels) do |j|
          mixer_ref(mx, i, j)
        end
      end
      Matrix.columns(a)
    else
      false
    end
  end

  def matrix2mixer(mat)
    if mat.kind_of?(Matrix)
      mx = make_mixer(mat.row_size)
      mat.to_a.each_with_index do |row, i|
        row.each_with_index do |val, j|
          mixer_set!(mx, j, i, val)
        end
      end
      mx
    else
      false
    end
  end

  def mixer_equal?(mx1, mx2)
    a = if mixer?(mx1)
          mixer2matrix(mx1)
        else
          mx1
        end
    b = if mixer?(mx2)
          mixer2matrix(mx2)
        else
          mx2
        end
    a == b or callcc do |flag|
      a.to_a.zip(b.to_a) do |a1, b1|
        flag.call(false) unless vequal(a1, b1)
      end
      true
    end
  end

  def mixer_diagonal?(mx)
    if mx.length == 1
      true
    else
      callcc do |flag|
        mx.channels.times do |i|
          mx.channels.times do |j|
            flag.call(false) if i != j and mixer_ref(mx, i, j).nonzero?
          end
        end
        true
      end
    end
  end

  def mixer_symmetric?(mx)
    mixer_equal?(mx, mixer_transpose(mx))
  end
  alias mixer_hermitian? mixer_symmetric?

  def mixer_determinant(mx)
    mixer2matrix(mx).determinant
  end

  def mixer_transpose(mx)
    matrix2mixer(mixer2matrix(mx).transpose)
  end

  def mixer_inverse(mx)
    matrix2mixer(mixer2matrix(mx).inverse)
  rescue
    false
  end

  def mixer_poly(mx, *coeffs)
    n = coeffs.length
    nmx = make_scalar_mixer(mx.length, coeffs[-1])
    x = mixer_multiply(mx, 1.0)
    (n - 2).downto(0) do |i|
      nmx = mixer_add(nmx, mixer_multiply(x, coeffs[i]))
      x = mixer_multiply(mx, x)
    end
    nmx
  end

  def mixer_normal?(mx)
    mixer_equal?(mixer_multiply(mx, mixer_transpose(mx)),
                 mixer_multiply(mixer_transpose(mx), mx))
  end

  def mixer_orthogonal?(mx)
    mixer_equal?(mixer_transpose(mx),
                 mixer_inverse(mx))
  end
  alias mixer_unitary? mixer_orthogonal?

  def mixer_trace(mx)
    mixer2matrix(mx).trace
  end

  # Ax=b where A is mixer and b is frame, returns frame
  def mixer_solve(a, b)
    if a.length == 1
      if mixer_ref(a, 0, 0) != 0.0
        make_frame(1, frame_ref(b, 0) / mixer_ref(a, 0, 0))
      else
        false
      end
    else
      if mixer?(imx = mixer_inverse(a))
        frame2frame(imx, b)
      else
        false
      end
    end
  end

  def frame_reverse(fr)
    len = fr.length
    j = len - 1
    (0...(len / 2)).each do |i|
      temp = frame_ref(fr, i)
      frame_set!(fr, i, frame_ref(fr, j))
      frame_set!(fr, j, temp)
      j -= 1
    end
    fr
  end

  def frame_copy(fr)
    len = fr.length
    nfr = make_frame(len)
    len.times do |i| frame_set!(nfr, i, frame_ref(fr, i)) end
    nfr
  end

  def mixer_copy(mx)
    len = mx.length
    nmx = make_mixer(len)
    len.times do |i|
      len.times do |j|
        mixer_set!(nmx, i, j, mixer_ref(mx, i, j))
      end
    end
    nmx
  end
  alias make_zero_mixer make_mixer
end

# mix.rb ends here
