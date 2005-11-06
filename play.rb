# play.rb -- play.scm -> play.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Fri Apr 22 23:36:39 CEST 2005
# Changed: Sat Sep 24 17:25:24 CEST 2005

# Commentary:
#
# playing-related examples
#
# open_play_output(out_chans, out_srate, out_format, out_bufsize)
# play_sound(&func)
# play_often(n)
# play_until_c_g
# play_region_forever(reg)
# loop_between_marks(m1, m2, bufsize)
# start_dac
# stop_dac
# vector_synthesis(files, read_even_when_not_playing, &driver)
# play_with_amps(snd, *rest)
# play_sine(freq, amp)
# play_sines(freq_and_amps)
# 
# Code:

def open_play_output(out_chans = 1,
                     out_srate = 22050,
                     out_format = (little_endian? ? Mus_lshort : Mus_bshort),
                     out_bufsize = 256)
  unless out_format then out_format = little_endian? ? Mus_lshort : Mus_bshort end
  if no_error = $mus_error_hook.empty?
    $mus_error_hook.add_hook!(get_func_name) do |type, msg| true end
  end
  audio_fd = Snd.catch(:all, -1) do
    mus_audio_open_output(Mus_audio_default, out_srate, out_chans, out_format, out_bufsize * 2)
  end.first
  if no_error then $mus_error_hook.remove_hook!(get_func_name) end
  if audio_fd == -1
    vals = Vct.new(32)
    mus_audio_mixer_read(Mus_audio_default, Mus_audio_format, 32, vals)
    out_format = vals[1].to_i
    mus_audio_mixer_read(Mus_audio_default, Mus_audio_channel, 32, vals)
    out_chans = vals[0].to_i
    err = mus_audio_mixer_read(Mus_audio_default, Mus_audio_samples_per_channel, 2, vals)
    if err != -1 then out_bufsize = vals[0].to_i end
    bps = mus_bytes_per_sample(out_format)
    out_bytes = bps * out_bufsize * out_chans
    audio_fd = Snd.catch(:all, -1) do
      mus_audio_open_output(Mus_audio_default, out_srate, out_chans, out_format, out_bufsize * 2)
    end.first
  end
  if audio_fd != -1 then set_dac_size(out_bufsize) end
  [audio_fd, out_chans, out_bufsize]
end

add_help(:play_sound,
         "play_sound(&func)  \
plays the currently selected sound, calling func on each data buffer, if func exists")
def play_sound
  if sounds.null?
    Snd.display("no sounds open")
  else
    filechans = channels()
    audio_fd, outchans, pframes = open_play_output(filechans, srate, false, 256)
    if audio_fd == -1
      Snd.display("could not open dac")
    else
      len = frames
      data = SoundData.new(outchans, pframes)
      0.step(len, pframes) do |beg|
        break if c_g?
        if outchans > 1 and filechans > 1
          [outchans, filechans].min.times do |chn|
            samples2sound_data(beg, pframes, false, chn, data, Current_edit_position, chn)
          end
        else
          samples2sound_data(beg, pframes, false, 0, data)
        end
        if block_given? then yield(data) end
        mus_audio_write(audio_fd, data, pframes)
      end
      mus_audio_close(audio_fd)
    end
  end
end
# play_sound do |data| data.map! do |val| val * 2.0 end end

# play sound n times -- play_often(3) for example

add_help(:play_often,
         "play_often(n)  plays the selected sound 'n' times (interruptible via C-g)")
def play_often(n)
  plays = n - 1
  play_once = lambda do |reason|
    if plays > 0 and (not c_g?) and reason == 0
      plays -= 1
      play(0, false, false, false, false, false, play_once)
    end
  end
  play(0, false, false, false, false, false, play_once)
end
# bind_key(?p, 0, lambda do |n| play_often([1, n].max) end, false, "play often")

# play sound until c-g

add_help(:play_until_c_g,
         "play_often(n)  plays the selected sound 'n' times (interruptible via C-g)")
def play_until_c_g
  play_once = lambda do |reason|
    if (not c_g?) and reason == 0
      play(0, false, false, false, false, false, play_once)
    end
  end
  play(0, false, false, false, false, false, play_once)
end

# play region over and over until C-g typed

add_help(:play_region_forever,
         "play_region_forever(reg)  plays region 'reg' until you interrupt it via C-g")
def play_region_forever(reg)
  play_region_again = lambda do |reason|
    if (not c_g?) and reason == 0
      play_region(reg, false, play_region_again)
    end
  end
  play_region(reg, false, play_region_again)
end
# bind_key(?p, 0,
#          lambda do |n| play_region_forever(Snd.regions[[0, n].max]) end,
#          false, "play region forever")

# play while looping continuously between two movable marks

add_help(:loop_between_marks,
         "loop_between_marks(mark1, mark2, buffersize)  \plays while looping between two marks")
def loop_between_marks(m1, m2, bufsize)
  pos1 = make_sample(m1)
  pos2 = make_sample(m2)
  beg = [pos1, pos2].min
  last = [pos1, pos2].max
  all_data = samples2sound_data
  audio_data = SoundData.new(1, bufsize)
  bytes = 2 * bufsize
  audio_fd = mus_audio_open_output(Mus_audio_default, srate, 1, Mus_lshort, bytes)
  if audio_fd != -1
    until c_g?
      bufsize.times do |i|
        audio_data[0, i] = all_data[0, beg]
        beg += 1
        if beg == last
          pos1 = make_sample(m1)
          pos2 = make_sample(m2)
          beg = [pos1, pos2].min
          last = [pos1, pos2].max
        end
      end
      mus_audio_write(audio_fd, audio_data, bufsize)
    end
    mus_audio_close(audio_fd)
  end
end
# loop_between_marks(0, 1, 512)

# hold DAC open and play sounds via keyboard
#
# if for some reason you want the DAC to run continuously in the
# background, use the "end" argument to the first player seen upon
# starting the dac:

add_help(:start_dac, "start_dac()  starts the DAC running continuously in the background")
def start_dac
  hidden_player = make_player
  set_amp_control(0.0, hidden_player)
  add_player(hidden_player, 0, 123456789)
  start_playing(1, 22050)
end
alias stop_dac stop_playing
# bind_key(?o, 0, lambda do | | play("oboe.snd") end, false, "play oboe")
# bind_key(?p, 0, lambda do | | play("pistol.snd") end, false, "play pistol")

# in this particular case, there's no need to hold the DAC open but
# maybe this will come in handy someday

# "vector synthesis"
#
# this idea (and the weird name) from linux-audio-development mailing
# list discussion apparently some commercial synths (or software?)
# provide this

add_help(:vector_synthesis,
         "vector_synthesis(files, read_even_when_not_playing, &driver)  \
uses 'driver', a function of two args (the number of files, \
and the number of samples between calls) to decide which file to play.  \
If 'read_even_when_not_playing' is true (default is false), \
the input files are constantly read, even if not playing.  \
'files' is a list of files to be played.")
def vector_synthesis(files, read_even_when_not_playing, &driver)
  if (files_len = files.length) > 0
    bufsize = 256
    sr = mus_sound_srate(files.first)
    chns = files.map do |f| mus_sound_chans(f) end.max
    readers = files.map do |f| make_file2frame(f) end
    locs = Array.new(files_len, 0)
    current_file = 0
    reading = true
    if (out_port = mus_audio_open_output(Mus_audio_default, sr, chns, Mus_lshort, bufsize * 2)) < 0
      Snd.raise(:snd_error, "can\'t open audio port! %s", out_port)
    else
      pframes = Array.new(files_len) do |i| mus_sound_frames(files[i]) end
      Snd.catch(:all, lambda do |arg| Snd.display("%s: error %s", get_func_name, arg) end) do
        while reading
          if (next_file = driver.call(files_len, bufsize)) != current_file
            ramp_down = 1.0
            ramp = 1.0 / bufsize
            current = readers[current_file]
            current_loc = locs[current_file]
            nxt = readers[next_file]
            next_loc = locs[next_file]
            downs = current.channels
            ups = nxt.channels
            up = make_frame(ups)
            down = make_frame(downs)
            bufsize.times do |i|
              file2frame(nxt, next_loc + i, up)
              file2frame(current, current_loc + i, down)
              chans.times do |j|
                val1 = if j < downs
                         ramp_down * frame_ref(down, j)
                       else
                         0.0
                       end
                val2 = if j < ups
                         (1.0 - ramp_down) * frame_ref(up, j)
                       else
                         0.0
                       end
                data[j, i] = val1 + val2
              end
              ramp_down -= ramp
            end
            if read_even_when_not_playing
              locs.map! do |val| val += bufsize end
            else
              locs[current_file] += bufsize
              locs[next_file] += bufsize
            end
            current_file = next_file
          else
            current = readers[current_file]
            current_loc = locs[current_file]
            ons = current.channels
            on = make_frame(ons)
            bufsize.times do |i|
              file2frame(current, current_loc + i, on)
              chans.times do |j|
                data[j, i] = if j < ons
                               frame_ref(on, j)
                             else
                               0.0
                             end
              end
            end
            if read_even_when_not_playing
              locs.map! do |val| val += bufsize end
            else
              locs[current_file] += bufsize
            end
          end
          mus_audio_write(out_port, data, bufsize)
          reading = (not c_g?) and (0...files_len).detect do |i| locs[i] < pframes[i] end
        end
      end
      mus_audio_close(out_port)
    end
  end
end
# let(0, 0) do |ctr, file|
#   vector_synthesis(["oboe.snd", "pistol.snd"], true) do |files, bufsize|
#     if ctr > 4
#       file += 1
#       ctr = 0
#       if file >= files then file = 0 end
#     else
#       ctr += 1
#     end
#     file
#   end
# end

# play_with_amps -- play channels with individually settable amps

add_help(:play_with_amps,
         "play_with_amps(snd, *amps)  \
plays snd with each channel scaled by the corresponding amp: \
play_with_amps(0, 1.0, 0.5) plays channel 2 of stereo sound at half amplitude")
def play_with_amps(snd, *amps)
  channels(snd).times do |chn|
    player = make_player(snd, chn)
    set_amp_control(amps[chn], player)
    add_player(player)
  end
  start_playing(channels(snd), srate(snd))
end

# play_sine and play_sines

add_help(:play_sine, "play_sine(freq, amp)  plays a 1 second sinewave at freq and amp")
def play_sine(freq, amp)
  audio_fd, outchans, pframes = open_play_output(1, 22050, false, 256)
  if audio_fd != -1
    len = 22050
    osc = make_oscil(:frequency, freq)
    data = SoundData.new(outchans, pframes)
    0.step(len, pframes) do |beg|
      break if c_g?
      data.map!(0) do |val| amp * oscil(osc) end
      mus_audio_write(audio_fd, data, pframes)
    end
    mus_audio_close(audio_fd)
  else
    false
  end
end

add_help(:play_sine, "play_sine(freq_and_amps)  \
produces a tone given its spectrum: play_sines([[440, 0.4], [660, 0.3]])")
def play_sines(freq_and_amps)
  audio_fd, outchans, pframes = open_play_output(1, 22050, false, 256)
  if audio_fd != -1
    len = 22050
    oscs = freq_and_amps.map do |freq, amp| make_oscil(:frequency, freq) end
    amps = freq_and_amps.map do |freq, amp| amp end
    data = SoundData.new(outchans, pframes)
    0.step(len, pframes) do |beg|
      break if c_g?
      data.map!(0) do |val|
        val = 0.0
        oscs.zip(amps) do |o, a| val += a * oscil(o) end
        val
      end
      mus_audio_write(audio_fd, data, pframes)
    end
    mus_audio_close(audio_fd)
  else
    false
  end
end
# play_sines([[425, 0.05], [450, 0.01],
#              [470, 0.01], [546, 0.02],
#              [667, 0.01], [789, 0.034],
#              [910, 0.032]])

# play.rb ends here
