# rtio.rb -- Translation of Snd/Guile's rtio.scm.

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue May 20 08:35:13 CEST 2003
# Last: Sat Dec 27 17:14:47 CET 2003

# Commentary:
#
# make_rtio(*args) or RTIO.new(*args)
#
# default args (all args are get- and settable properties)
#  :chans     1
#  :srate     22050
#  :in_sys    0 (card #)
#  :out_sys   1 (card #)
#  :device    Mus_audio_default
#  :fmt       Mus_lshort or Mus_bshort depending on little_endian?
#  :bytes     512
#  :shorts    256
#  :func      nil (function of one arg: lambda do |data| ... end)
#
# RTIO methods:
#  rt = make_rtio
#  rt.help
#  rt.close(unlink_tmpfile = true)
#
#  rt.show_input(in_sys = @in_sys)
#  rt.show_input_fft(in_sys = @in_sys)
#  rt.show_draggable_input_fft(in_sys = @in_sys) alias show_drag(in_sys = @in_sys)
#
#  rt.in_out(in_sys = @in_sys, out_sys = @out_sys, func = nil)
#  rt.amplify(amp)
#  rt.lp
#  rt.hp(freq)

# make_rtio, RTIO.new and chans= look for an open sound with the
# corresponding number of channels, save its state of time-,
# transform-, and lisp_graph? and set the first two of them to false.
# If no appropriate sound is found, a temporary one will be opened
# with the required channels.
#
# rt.close resets the saved states of time-, transform- and
# lisp_graph?  to their old values and closes and removes temporary
# opened sounds.

# Code:

require "examp"
require "hooks"

def make_rtio(*args)
  RTIO.new(*args)
end

class RTIO
  class RTIO_Error < StandardError
  end

  def error(*args)
    str = format(*args)
    str += format(": %s\n%s", $!, $@.join("\n")) if $!
    raise(RTIO_Error, str, caller(1)[0])
  end
  private :error
  
  def initialize(*args)
    @sound_properties = []
    set_chans(get_args(args, :chans, 1))
    @srate   = get_args(args, :srate, 22050)
    @in_sys  = get_args(args, :in_sys, 0)
    @out_sys = get_args(args, :out_sys, 1)
    @device  = get_args(args, :device, Mus_audio_default)
    @fmt     = get_args(args, :fmt, little_endian? ? Mus_lshort : Mus_bshort)
    @bytes   = get_args(args, :bytes, 512)
    @shorts  = get_args(args, :shorts, 256)
    @func    = get_args(args, :func, nil)
  end
  attr_accessor :srate, :in_sys, :out_sys, :device, :fmt, :bytes, :shorts
  attr_reader :chans, :sound_properties
  
  def inspect
    format("#<%s: chans: %d, srate: %d, in_sys: %d, out_sys: %d, device: %d, \
fmt: %d, bytes: %d, shorts: %d, func: %s>",
           self.class, @chans, @srate, @in_sys, @out_sys, @device,
           @fmt, @bytes, @shorts, @func.inspect)
  end

  def help
    str = <<HELP
# You can set the following properties:
#{self.inspect}
# e.g.
#  rt = RTIO.new
#  rt.chans = 2
#  rt.srate = 44100
#  rt.device = Mus_audio_cd
# or even
#   rt = RTIO.new(:srate, 44100, :chans, 2, :device, Mus_audio_cd)
# 
# make_rtio(*args) or RTIO.new(*args)
#
# default args (all args are get- and settable properties)
#  :chans     1
#  :srate     22050
#  :in_sys    0 (card #)
#  :out_sys   1 (card #)
#  :device    Mus_audio_default
#  :fmt       Mus_lshort or Mus_bshort depending on little_endian?
#  :bytes     512
#  :shorts    256
#  :func      nil (function of one arg: lambda do |data| ... end)
#
# RTIO methods:
#  rt = make_rtio
#  rt.help
#  rt.close(unlink_tmpfile = true)
#
#  rt.show_input(in_sys = @in_sys)
#  rt.show_input_fft(in_sys = @in_sys)
#  rt.show_draggable_input_fft(in_sys = @in_sys) alias show_drag(in_sys = @in_sys)
#
#  rt.in_out(in_sys = @in_sys, out_sys = @out_sys, func = nil)
#  rt.amplify(amp)
#  rt.lp
#  rt.hp(freq)
#
# examples:
#  rt.show_drag
#  rt.amplify(2.0).in_out(1, 0)
#  rt.lp.in_out
#  rt.hp(1200).in_out
HELP
    ENV["EMACS"] ? puts(str) : str
  end

  def chans=(val)
    props = {}
    unless (snd = sounds.detect do |s| channels(s) == val end)
      snd = new_sound(snd_tempnam, default_output_header_type, default_output_data_format,
                      default_output_srate, val)
      props[:new?] = true
    end
    props[:snd] = select_sound(snd)
    props[:time_graph?] = Range.new(0, channels(snd) - 1).map do |chn|
      time_graph?(snd, chn)
    end
    props[:transform_graph?] = Range.new(0, channels(snd) - 1).map do |chn| 
      transform_graph?(snd, chn)
    end
    props[:lisp_graph?] = Range.new(0, channels(snd) - 1).map do |chn|
      lisp_graph?(snd, chn)
    end
    @sound_properties << props
    Range.new(0, channels(snd) - 1).map do |chn|
      set_time_graph?(false, snd, chn)
      set_transform_graph?(false, snd, chn)
      set_lisp_graph?(true, snd, chn)
    end
    @chans = val
  end
  alias set_chans chans=

  def close(unlink_tmpfile = true)
    @sound_properties.each do |props|
      if props[:new?]
        file = file_name(props[:snd])
        close_sound(props[:snd])
        File.unlink(file) if unlink_tmpfile
      else
        props[:time_graph?].each_with_index do |flag, chn|
          set_time_graph?(flag, props[:snd], chn)
        end
        props[:transform_graph?].each_with_index do |flag, chn|
          set_transform_graph?(flag, props[:snd], chn)
        end
        props[:lisp_graph?].each_with_index do |flag, chn|
          set_lisp_graph?(flag, props[:snd], chn)
        end
      end
    end
    @sound_properties = nil
  end
  
  def show_input(in_sys = @in_sys)
    in_port = audio_open(in_sys)
    data = make_sound_data(@chans, @shorts)
    vobj = make_vct(@shorts)
    until c_g?()
      mus_audio_read(in_port, data, @shorts)
      @chans.times do |chn|
        begin
          graph(sound_data2vct(data, chn, vobj), "input", 0.0, 1.0, -1.0, 1.0, false, chn)
        rescue
          error(get_func_name())
        end
      end
    end
    audio_close(in_port)
  end

  def show_input_fft(in_sys = @in_sys)
    in_port = audio_open(in_sys)
    data = make_sound_data(@chans, @shorts)
    vobj = make_vct(@shorts)
    until c_g?()
      mus_audio_read(in_port, data, @shorts)
      @chans.times do |chn|
        begin
          graph(snd_spectrum(sound_data2vct(data, chn, vobj), Blackman2_window, @shorts, true),
                "input fft", 0.0, 1.0, 0.0, 1.0, false, chn)
        rescue
          error(get_func_name())
        end
      end
    end
    audio_close(in_port)
  end

  def show_draggable_input_fft(in_sys = @in_sys)
    mouse_down = 0.0
    mouse_pos = 0.0
    x1 = 1.0
    in_port = audio_open(in_sys)
    data = make_sound_data(@chans, @shorts)
    vobj = make_vct(@shorts)
    $mouse_drag_hook.add_hook!("rtio_hook") do |snd, chn, button, state, x, y|
      xnew = x / x1.to_f
      lim = [1.0, [0.1, mouse_down + (mouse_pos - xnew)].max].min
      x1 = lim
    end
    $mouse_press_hook.add_hook!("rtio_hook") do |snd, chn, button, state, x, y|
      mouse_pos = x / x1.to_f
      mouse_down = x1
    end
    until c_g?()
      mus_audio_read(in_port, data, @shorts)
      maxpt = (x1 == 1.0 ? @shorts : (x1 * @shorts).round)
      @chans.times do |chn|
        begin
          graph(snd_spectrum(vct_subseq(sound_data2vct(data, chn, vobj), 0, maxpt),
                             Blackman2_window, maxpt, true),
                "spectrum", 0.0, x1, 0.0, 1.0, false, chn)
        rescue
          error(get_func_name())
        end
      end
    end
    $mouse_drag_hook.remove_hook!("rtio_hook")
    $mouse_press_hook.remove_hook!("rtio_hook")
    audio_close(in_port)
  end
  alias show_drag show_draggable_input_fft

  # rt.amplify(2.0).in_out
  # rt.amplify(2.0).in_out(1, 0)
  # rt.in_out(0, 1, lambda do |data| ... end)
  # rt.in_out(1, 0, method(:my_func).to_proc) with def my_func(data) ... end
  def in_out(in_sys = @in_sys, out_sys = @out_sys, func = nil)
    out_port = audio_open(out_sys, true)
    in_port = audio_open(in_sys)
    data = make_sound_data(@chans, @shorts)
    f = (func or @func)
    until c_g?()
      mus_audio_read(in_port, data, @shorts)
      begin
        f.call(data)
      rescue
        error(get_func_name())
      end
      mus_audio_write(out_port, data, @shorts)
    end
    audio_close(in_port, out_port)
  end

  # These are usable by in_out
  # amplify(amp)
  # lp
  # hp(freq)
  def amplify(amp)
    vobj = make_vct(@shorts)
    @func = lambda do |data|
      @shorts.times do |i|
        @chans.times do |chn|
          sound_data_set!(data, chn, i, sound_data_ref(data, chn, i) * amp)
        end
      end
      @chans.times do |chn|
        begin
          graph(snd_spectrum(sound_data2vct(data, chn, vobj), Blackman2_window, @shorts, true),
                "amplify(%.1f)" % amp,
                0.0, 1.0, 0.0, 1.0, false, chn)
        rescue
          error(get_func_name())
        end
      end
    end
    self
  end

  def lp
    val = 0.0
    vobj = make_vct(@shorts)
    @func = lambda do |data|
      @shorts.times do |i|
        @chans.times do |chn|
          curval = sound_data_ref(data, chn, i)
          sound_data_set!(data, chn, i, 0.5 * (curval + val))
          val = curval
        end
      end
      @chans.times do |chn|
        begin
          graph(snd_spectrum(sound_data2vct(data, chn, vobj), Blackman2_window, @shorts, true),
                "lp",
                0.0, 1.0, 0.0, 1.0, false, chn)
        rescue
          error(get_func_name())
        end
      end
    end
    self
  end

  def hp(freq)
    flt = make_formant(0.99, freq)
    vobj = make_vct(@shorts)
    @func = lambda do |data|
      @shorts.times do |i|
        @chans.times do |chn|
          curval = sound_data_ref(data, chn, i)
          sound_data_set!(data, chn, i, formant(flt, curval))
        end
      end
      @chans.times do |chn|
        begin
          graph(snd_spectrum(sound_data2vct(data, chn, vobj), Blackman2_window, @shorts, true),
                "hp(%.1f)" % freq,
                0.0, 1.0, 0.0, 1.0, false, chn)
        rescue
          error(get_func_name())
        end
      end
    end
    self
  end
  
  def audio_open(sys, output = false)
    device = sys << 16 | @device
    port = if output
             mus_audio_open_output(device, @srate, @chans, @fmt, @bytes)
           else
             mus_audio_open_input(device, @srate, @chans, @fmt, @bytes)
           end
    
    error("can't open %s_sys %d", output ? "out" : "in", sys) if port < 0
    port
  end
  private :audio_open
  
  def audio_close(*ports)
    ports.each do |prt| error("can't close port %d", prt) if mus_audio_close(prt) < 0 end
  end
  private :audio_close
end

# rtio.rb ends here
