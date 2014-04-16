# mix.rb -- mix.scm --> mix.rb -*- snd-ruby -*-

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Tue Feb 22 13:40:33 CET 2005
# Changed: Sat Feb 06 18:21:12 CET 2010

# Commentary:
#
# various mix related functions
#
# module Mix (see mix.scm)
#  mix_sound(file, start)
#  silence_all_mixes
#  find_mix(sample, snd, chn)
#  mix2vct(id)
#  mix_maxamp(id)
#  snap_mix_to_beat(at_tag_position)
#
#  mix_click_sets_amp(id)
#  mix_click_info(id)
#  mix_name2id(name)
#
#  delete_mix(id)
#  scale_mixes(mix_list, scl)
#  silence_mixes(mix_list)
#  move_mixes(mix_list, samps)
#  src_mixes(mix_list, sr)
#  transpose_mixes(mix_list, semitones)
#  color_mixes(mix_list, col)
#  set_mixes_tag_y(mix_list, new_y)
#  mixes_maxamp(mix_list)
#  scale_tempo(mix_list, tempo_scl)
#  mixes_length(mix_list)
#
# module Mixer_matrix (see mixer.scm)
#  mixer_copy(mx)
#  make_zero_mixer(n)
#  mixer_diagonal?(mx)
#  mixer_transpose(mx)
#  sub_matrix(mx, row, col)
#  mixer_determinant(mx)
#  mixer_poly(mx, *coeffs)
#  mixer_trace(mx)
#  invert_matrix(mx, b, zero)
#  mixer_solve(a, b)
#  mixer_inverse(mx)
#
#  mixer_equal?(mx1, mx2)
#  mixer_normal?(mx)
#  mixer_orthogonal?(mx)
#  mixer_unitary?(mx)
#  mixer_symmetric?(mx)
#  mixer_hermitian?(mx)
#
#  frame_reverse(fr)
#  

# Code:

require "clm"

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

  add_help(:delete_all_mixes, "delete_all_mixes() sets all mix amps to 0.")
  def silence_all_mixes
    as_one_edit_rb(get_func_name) do (mixes or []).flatten.each do |id| set_mix_amp(id, 0.0) end end
  end

  add_help(:find_mix,
           "find_mix(sample, [snd=false, [chn=false]]) \
returns the id of the mix at the given sample, or nil.")
  def find_mix(sample, snd = false, chn = false)
    (mixes(Snd.snd(snd), Snd.chn(chn)) or []).detect do |n| mix_position(n) == sample end
  end

  add_help(:mix2vct, "mix2vct(id) returns mix's data in vct.")
  def mix2vct(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    len = mix_length(id)
    rd = make_mix_sampler(id)
    v = Vct.new(len) do |i| read_mix_sample(rd) end
    free_sampler(rd)
    v
  end

  add_help(:mix_maxamp, "mix_maxamp(id) returns the max amp in the given mix.")
  def mix_maxamp(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    len = mix_length(id)
    rd = make_mix_sampler(id)
    peak = read_mix_sample(rd).abs
    (1...len).each do peak = [peak, read_mix_sample(rd).abs].max end
    free_sampler(rd)
    peak
  end

  add_help(:snap_mix_to_beat,
           "snap_mix_to_beat() \
forces a dragged mix to end up on a beat (see beats-per-minute).  \
Reset $mix_release_hook to cancel.")
  def snap_mix_to_beat
    $mix_release_hook.add_hook!(get_func_name) do |id, samps_moved|
      samp = samps_moved + mix_position(id)
      snd = mix_home(id)[0]
      chn = mix_home(id)[1]
      bps = beats_per_minute(snd, chn) / 60.0
      sr = srate(snd).to_f
      beat = ((samp * bps) / sr).floor
      lower = ((beat * sr) / bps).floor
      higher = (((beat + 1) * sr) / bps).floor
      set_mix_position(id, if (samp - lower) < (higher - samp)
                             [0, lower].max
                           else
                             higher
                           end)
      true
    end
  end

  #
  # === Mix Property ===
  #
  def mix_click_sets_amp(id)
    unless mix_property(:zero, id)
      set_mix_property(:amp, id, mix_amp(id))
      set_mix_amp(id, 0.0)
      set_mix_property(:zero, id, true)
    else
      set_mix_amp(id, mix_property(:amp, id))
      set_mix_property(:zero, id, false)
    end
    true
  end
  # $mix_click_hook.add_hook!("mix-click-sets-amp", &method(:mix_click_sets_amp).to_proc)

  # 
  # === Mix Click Info ===
  # 
  add_help(:mix_click_info,
           "mix_click_info(n) \
is a $mix_click_hook function that describes a mix and its properties.")
  def mix_click_info(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    info_dialog("Mix Info", format("\
      mix id: %s%s
    position: %d (%1.3f secs)
      length: %d (%1.3f secs)
          in: %s[%d]
      scaler: %s
       speed: %s
         env: %s%s",
                                   id,
                                   ((s = mix_name(id)) ?
                                    format("\n    mix name: %s", s.inspect) :
                                    ""),
                                   mix_position(id), mix_position(id) / srate(mix_home(id)[0]).to_f,
                                   mix_length(id),   mix_length(id) / srate(mix_home(id)[0]).to_f,
                                   short_file_name(mix_home(id)[0]),
                                   mix_home(id)[1],
                                   mix_amp(id),
                                   mix_speed(id),
                                   mix_amp_env(id),
                                   ((props = mix_properties(id)) ?
                                    format("\n  properties: %s", props.inspect) :
                                    "")))
    true
  end
  # $mix_click_hook.add_hook!("mix-click-info", &method(:mix_click_info).to_proc)

  add_help(:mix_name2id,
           "mix_name2id(name)  returns the mix id associated with NAME.")
  def mix_name2id(name)
    ret = :no_such_mix
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        mixes(snd, chn).each do |m|
          if mix_name(m) == name
            ret = m
            break
          end
        end
      end
    end
    ret
  end

  # ;;; ---------------- backwards compatibilty

  def delete_mix(id)
    set_mix_amp(id, 0.0)
  end

  # ;;; -------- mix lists (used to be "tracks")

  def scale_mixes(mix_list, scl)
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        set_mix_amp(m, scl * mix_amp(m))
      end
    end
  end

  def silence_mixes(mix_list)
    scale_mixes(mix_list, 0.0)
  end

  def move_mixes(mix_list, samps)
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        set_mix_position(m, mix_position(m) + samps)
      end
    end
  end

  def src_mixes(mix_list, sr)
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        set_mix_speed(m, mix_speed(m) * sr)
      end
    end
  end

  add_help(:transpose_track,
           "transpose_mixes(mix_list, semitones)  \
transposes each mix in MIX_LIST by SEMITONES.")
  def transpose_mixes(mix_list, semitones)
    src_mixes(mix_list, 2.0 ** (semitones / 12.0))
  end

  def color_mixes(mix_list, col)
    mix_list.each do |m|
      set_mix_color(m, col)
    end
  end

  def set_mixes_tag_y(mix_list, new_y)
    mix_list.each do |m|
      set_mix_tag_y(m, new_y)
    end
  end

  def mixes_maxamp(mix_list)
    mx = 0.0
    mix_list.each do |m|
      mx = [mx, mix_maxamp(m)].max
    end
    mx
  end

  def scale_tempo(mix_list, tempo_scl)
    first_beg = last_beg = mix_position(mix_list.car)
    mix_list.cdr.each do |m|
      pos = mix_position(m)
      first_beg = [first_beg, pos].min
      last_beg  = [last_beg,  pos].max
    end
    tempo_scl = tempo_scl.to_f
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        diff = (tempo_scl * (mix_position(m) - first_beg)).round
        if diff != 0
          set_mix_position(m, first_beg + diff)
        end
      end
    end
  end
  # reverse_mix_list is scale_tempo(mix_list, -1.0)

  def mixes_length(mix_list)
    max_len = mix_list.map do |m| mix_position(m) + mix_length(m) end.max
    min_len = mix_list.map do |m| mix_position(m) end.min
    max_len - min_len + 1
  end
end

include Mix

# 
# === MIXER.SCM ===
# 
module Mixer_matrix
  def mixer_copy(mx)
    nmx = make_mixer(mx.length)
    mx.length.times do |i|
      mx.length.times do |j|
        mixer_set!(nmx, i, j, mixer_ref(mx, i, j))
      end
    end
    nmx
  end

  alias make_zero_mixer make_mixer

  def mixer_diagonal?(mx)
    if mx.length == 1
      true
    else
      mx.length.times do |i|
        mx.length.times do |j|
          if i != j and mixer_ref(mx, i, j).nonzero?
            return false
          end
        end
      end
      true
    end
  end

  def mixer_transpose(mx)
    nmx = make_zero_mixer(mx.length)
    mx.length.times do |i|
      mx.length.times do |j|
        mixer_set!(nmx, j, i, mixer_ref(mx, i, j))
      end
    end
    nmx
  end

  def sub_matrix(mx, row, col)
    nmx = make_zero_mixer(mx.length - 1)
    ni = 0
    mx.length.times do |i|
      if i != row
        nj = 0
        mx.length.times do |j|
          if j != col
            mixer_set!(nmx, ni, nj, mixer_ref(mx, i, j))
            nj += 1
          end
        end
        ni += 1
      end
    end
    nmx
  end
  
  def mixer_determinant(mx)
    if mx.length == 1
      mixer_ref(mx, 0, 0)
    else
      if mx.length == 2
        mixer_ref(mx, 0, 0) * mixer_ref(mx, 1, 1) - mixer_ref(mx, 0, 1) * mixer_ref(mx, 1, 0)
      else
        if mx.length == 3
          ((mixer_ref(mx, 0, 0) * mixer_ref(mx, 1, 1) * mixer_ref(mx, 2, 2) +
            mixer_ref(mx, 0, 1) * mixer_ref(mx, 1, 2) * mixer_ref(mx, 2, 0) +
            mixer_ref(mx, 0, 2) * mixer_ref(mx, 1, 0) * mixer_ref(mx, 2, 1)) -
           (mixer_ref(mx, 0, 0) * mixer_ref(mx, 1, 2) * mixer_ref(mx, 2, 1) +
            mixer_ref(mx, 0, 1) * mixer_ref(mx, 1, 0) * mixer_ref(mx, 2, 2) +
            mixer_ref(mx, 0, 2) * mixer_ref(mx, 1, 1) * mixer_ref(mx, 2, 0)))
        else
          sum = 0.0
          sign = 1
          mx.length.times do |i|
            mult = mixer_ref(mx, 0, i)
            if mult != 0.0
              sum = sum + sign * mult * mixer_determinant(sub_matrix(mx, 0, i))
            end
            sign = -sign
          end
          sum
        end
      end
    end
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

  def mixer_trace(mx)
    sum = 0.0
    mx.length.times do |i| sum += mixer_ref(mx, i, i) end
    sum
  end

  def invert_matrix(mx, b = nil, zero = 1.0e-7)
    # ;; translated from Numerical Recipes (gaussj)
    cols = make_array(mx.length, 0)
    rows = make_array(mx.length, 0)
    pivots = make_array(mx.length, 0)
    mx.length.times do |i|
      biggest = 0.0
      col = 0
      row = 0
      mx.length.times do |j|
        if pivots[j] != 1
          mx.length.times do |k|
            if pivots[k] == 0
              val = mixer_ref(mx, j, k).abs
              if val > biggest
                col = k
                row = j
                biggest = val
              else
                if pivots[k] == 1
                  return false
                end
              end
            end
          end
        end
      end
      pivots[col] += 1
      if row != col
        temp = (b ? frame_ref(b, row) : 0.0)
        if b
          frame_set!(b, row, frame_ref(b, col))
          frame_set!(b, col, temp)
        end
        mx.length.times do |k|
          temp = mixer_ref(mx, row, k)
          mixer_set!(mx, row, k, mixer_ref(mx, col, k))
          mixer_set!(mx, col, k, temp)
        end
      end
      cols[i] = col
      rows[i] = row
      if mixer_ref(mx, col, col).abs < zero
        return false
      end
      inverse_pivot = 1.0 / mixer_ref(mx, col, col)
      mixer_set!(mx, col, col, 1.0)
      mx.length.times do |k|
        mixer_set!(mx, col, k, inverse_pivot * mixer_ref(mx, col, k))
      end
      if b
        frame_set!(b, col, inverse_pivot * frame_ref(b, col))
      end
      mx.length.times do |k|
        if k != col
          scl = mixer_ref(mx, k, col)
          mixer_set!(mx, k, col, 0.0)
          mx.length.times do |m|
            mixer_set!(mx, k, m, mixer_ref(mx, k, m) - scl * mixer_ref(mx, col, m))
          end
          if b
            frame_set!(b, k, frame_ref(b, k) - scl * frame_ref(b, col))
          end
        end
      end
    end
    (mx.length - 1).downto(0) do |i|
      if rows[i] != cols[i]
        mx.length.times do |k|
          temp = mixer_ref(mx, k, rows[i])
          mixer_set!(mx, k, rows[i], mixer_ref(mx, k, cols[i]))
          mixer_set!(mx, k, cols[i], temp)
        end
      end
    end
    [mx, b]
  end

  # Ax=b where A is mixer and b is frame, returns frame
  def mixer_solve(a, b)
    val = invert_matrix(a, b)
    val and val[1]
  end

  def mixer_inverse(mx)
    val = invert_matrix(mx)
    val and val[0]
  end

  def mixer_equal?(mx1, mx2)
    if mx1.length == mx2.length
      mx1.length.times do |i|
        mx2.length.times do |j|
          if (mixer_ref(mx1, i, j) - mixer_ref(mx2, i, j)).abs > 0.001
            return false
          end
        end
      end
      true
    else
      false
    end
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

  def mixer_symmetric?(mx)
    mixer_equal?(mx, mixer_transpose(mx))
  end
  alias mixer_hermitian? mixer_symmetric?

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
end

# mix.rb ends here
