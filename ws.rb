# ws.rb -- with_sound and friends for Snd/Ruby -*- snd-ruby -*-

# Copyright (c) 2003--2006 Michael Scholz <mi-scholz@users.sourceforge.net>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

# Commentary:
#
# module WS
#   ws_interrupt?
#   ws_break(*rest)
#   with_reverb(reverb, reverb_data, reverb_amount, snd, *with_sound_args)
#   with_sound(*args) do ... end
#   with_dac(*args, &body)
#   clm_load(rbm_file, *args)
#   with_temp_sound(snd) do |temp_snd_file_name| ... end
#   make_default_comment
#   remove_file(file)
#   each_sample(start, dur) do |samp| ... end
#   sound_data_frame_ref(data, samp, frm)
#   sound_data_frame_set!(data, samp, frm)
#   process_times
#
# class With_sound
#   initialize(*args, &body)
#
# properties:
#   output       # file name, String
#
# methods:
#   help   (or description, info)
#   inspect
#   to_s
#   describe   (show_local_variables)
#   with_sound(*args) do ... end
#   scaled_to(scale) do ... end
#   scaled_by(scale) do ... end
#   with_offset(secs) do ... end
#   with_current_sound(*args) do ... end
#   sound_let(*args) do |*sl_args| ... end
#   clm_load(rbm_file, *args)
#   with_mix(*args, file[, beg_time], body_string)
#   with_sound_info(instrument_name, start, dur)
#   run_instrument(start, dur, *locsig_args) do |samp| ... end
#   run_reverb(chan = 0) do |in_val, samp| ... end
#   clm_mix(filename, *args)
#   run do ... end
#
# class Instrument
#   my_simp(start, dur, freq, amp, amp_env = [0, 1, 1, 1])
#   make_ws_reader(file, *args)
#   ws_readin(rd)
#   close_ws_reader(file, rd)
#   ws_location(rd)
#   set_ws_location(rd, val)
#   ws_increment(rd)
#   set_ws_increment(rd, val)
#   ws_srate(file)
#   ws_channels(file)
#   ws_duration(file)
#
# Instruments have access to @ws_output and @ws_reverb variables as
# well as @srate, @channels, @reverb_channels etc.  It is no longer
# necessary (but possible) to use the global variables $output and
# $reverb.
#
# The classes (Snd_|CLM_)Instrument are not really necessary, but the
# name may be more clear if special instruments are added.
# 
#                With_sound
#                    |
#                    v    
#            --- Instrument ---
#            |                |
#            v                v
#      Snd_Instrument  CLM_Instrument
#       |        |            |
#       v        v            v
#  With_Snd   With_DAC     With_CLM
#
# Instruments can use the generalized run-loop `run_instrument'.
#
# RUN_INSTRUMENT(start, dur, *locsig_args) do |samp| ... (return next sample) end
#
# The body (or block) of `run_instrument' should return the next sample, e.g.:
#
# class Instrument
#   def my_simp(start, dur, freq, amp, amp_env = [0, 1, 1, 1])
#     os = make_oscil(:frequency, freq)
#     en = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
#     run_instrument(start, dur) do
#       env(en) * oscil(os)
#     end
#   end
# end
# 
# In Snd as well as in a Ruby script:
#      with_sound do my_simp(0, 1, 440, 0.2) end
#
# In addition, `with_dac do my_simp(0, 1, 440, 0.2) end' can use the
# same instrument for dac-output.
#
# Reverbs can use the generalized run-loop `run_reverb'.
#
# RUN_REVERB(chan = 0) do |value, samp| ... (return next frame) end
#
# VALUE is the next sample value on location SAMP of reverb file's
# CHANnel.  It replaces
#
#   (beg...len).each do |i|
#     ho = ina(i, $reverb)
#     ...
#   end
# by
#   run_reverb() do |ho, i|
#     ...
#   end
#
# The body should return a frame object of out-channels length.  The
# body is called seconds2sample(dur) times, like run_instrument.
#
# def my_reverb(start, dur, *rest)
#   ...
#   out_frames = make_frame(@channels)
#   run_reverb() do |ho, i|
#     ...
#     frame_set!(out_frames, 0, val0)
#     if @channels > 1
#       frame_set!(out_frames, 1, val1)
#     end
#     ...
#     out_frames
#   end
# end
#
# A special case occures in freeverb.rb.
#
#   run_reverb(:frames) do |in_frame, i|
#     ...
#   end
#
# IN_FRAME is a frame object of the I-th location in the reverb file
# (or snd), the rest is like above.
#
# The classes Snd_Instrument and CLM_Instrument can be used to define
# special instruments, see FULLMIX in clm-ins.rb.
#
# Class Snd_Instrument instruments can only be used in Snd.  The
# buffer can be created by with_sound or you can provide buffers for
# output and revout, vcts or sound_data objects.
#
# with_sound(:out_buffer, Vct.new(22050))
# with_sound(:out_buffer, SoundData.new(22050, 2), :channels, 2)
#
# Class CLM_Instrument instruments use sample2file output and can be
# used in Snd as well as in Ruby scripts, @ws_output and $output are
# the same, @ws_reverb and $reverb are the same too.

# Usage:
#
# Global variables can be set in ~/.snd_ruby or in other scripts
# before or after loading ws.rb.
#
# with_sound(:play, 3, :statistics, true, :reverb, :jc_reverb) do
#   fm_violin(0, 1, 440, 0.3)
# end
#
# clm_load("test.rbm", :play, 1, :statistics, true, :verbose, true)
#
# These functions can be called within with_sound or clm_load:
#   scaled_to(scale, &body)
#   scaled_by(scale, &body)
#   with_offset(secs, &body)
#   with_current_sound(*args, &body)
#   sound_let(*args, &body)
#   with_mix(*args) (does not use a block but a string as "body")
#
# SOUND_LET
#
# sound_let(*args) do |*sl_args| ... end
# args: [with_sound-args1, with_sound-body1],
#       [with_sound-args2, with_sound-body2],
#       with_sound_body3,
#       let_args1,
#       let_args2, ...
#
# sound_let works like let except for procedures which are handled by
# with_sound.  with_sound returns a filename which can be used in the
# sound_let body.
#
# sound_let(lambda do fm_violin(0, 1, 330, 0.5) end,
#           1024) do |tmp_file, val|
# end
#
# TMP_FILE is returned by with_sound and VAL is connected to 1024.
#
# If with_sound needs args, the args and the procedure must be in an
# array.
#
# sound_let([:scaled_to, 0.3, :output, "sl.snd", lambda do fm_violin(0, 1, 330, 0.5) end],
#           1024) do |tmp_file, val|
# end
# 
# Examples:
# 
# One with_sound-call and one temporary file name, arbitrary called TMP:
# 
# sound_let([:reverb, :jc_reverb, lambda do fm_violin(0, 1, 220, 0.2) end]) do |tmp|
#   mus_mix(@output, tmp)
# end
# 
# Two with_sound-calls and two temporary file names, arbitrary
# called TEMP_1 and TEMP_2:
# 
# sound_let([:reverb, :jc_reverb, lambda do fm_violin(0, 1, 220, 0.2) end],
#           lambda do fm_violin(0.5, 1, 440, 0.3) end) do |temp_1, temp_2|
#   mus_mix(temp_1, temp_2)
#   mus_mix(@output, temp_1)
# end
#
# WITH_MIX
#
# with_mix(file, body_string)
# with_mix(file, beg_time, body_string)
# with_mix(with_sound-args, file, beg_time, body_string)
#
# The FILE and BODY_STRING are necessary, BEG_TIME and
# WITH_SOUND-ARGS are optional, BEG_TIME defaults to 0.
#
# Creates a text file named FILE.rbm with contents BODY_STRING and a
# sound file named FILE.snd, the result of clm_load(FILE.rbm).  If
# BODY_STRING is changed, clm_load(FILE.rbm) is called before mixing
# FILE.snd in the underlying @output of with_sound.
#
# Example:
#
# with_sound() do
#   with_mix(:reverb, :jc_reverb, "foo", 0.2, %Q{
#     fm_violin(0, 1, 440, 0.1)
#   })
# end
#
# with_sound's NOTEHOOK and INFO options:
#
# Every time an instrument starts computing, @with_sound_note_hook is
# called with the instrument name, the start time and the duration.
#
# def my_notehook(name, start, dur)
#   if name =~ /violin/
#     Snd.message("%s: start %1.3f, dur %1.3f", name, start, dur)
#   end
# end
#
# with_sound(:notehook, :my_notehook) do
#   fm_violin(0, 1, 440, 0.1)
#   fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
#   abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
#   fm_bell(0.5, 2.0, 220, 0.5, abell, fbell, 1)
# end
# 
# installs the @with_sound_note_hook and prints the line
# 
#   `# fm_violin: start 0.000, dur 1.000'
#
# If option :info is true, every instrument-call prints the line
# mentioned above.
#
# with_sound's SAVE_BODY option:
# 
# Works only if proc (the with_sound-body) is located in a source file
# and proc.to_s returns something like
# #<Proc:0x00000000@(/path/to/source/file):2> (older Ruby versions
# return only #<Proc:0x00000000>, so we can't find the source file and
# line number).
#
# Tries to locate the body code and saves it in the comment string of
# the sound file.  Scanning the source code is very simple and may not
# work in every case.

# CLM examples (see clm.html) and their Snd/Ruby counterparts:
#
# ;; CLM examples
#  (with-sound () 
#    (mix (with-sound (:output "hiho.snd") 
#              (fm-violin 0 1 440 .1))
#            :amplitude .5))
# 
#  (with-sound ()
#    (with-mix () "s1" 0
#      (sound-let ((tmp ()
#                    (fm-violin 0 1 440 .1)))
#        (mix tmp))))
# 
#  (with-sound (:verbose t)
#    (with-mix () "s6" 0
#      (sound-let ((tmp ()
#                    (fm-violin 0 1 440 .1))
#                  (tmp1 (:reverb nrev)
#                    (mix "oboe.snd")))
#        (mix tmp1)
#        (mix tmp :amplitude .2 :output-frame *srate*))
#      (fm-violin .5 .1 330 .1)))
# 
#  (with-sound (:verbose t)
#    (sound-let ((tmp ()
#                  (with-mix () "s7" 0
#                    (sound-let ((tmp ()
#                                  (fm-violin 0 1 440 .1))
#                                (tmp1 ()
#                                  (mix "oboe.snd")))
#                     (mix tmp1)
#                     (mix tmp :output-frame *srate*))
#                   (fm-violin .5 .1 330 .1))))
#      (mix tmp :amplitude .5)))
#
=begin
# Snd/Ruby examples
with_sound() do
  clm_mix(with_sound(:output, "hiho.snd") do
            fm_violin(0, 1, 440, 0.1)
          end.output, :scale, 0.5)
end

with_sound() do
  with_mix "s1", %Q{
  sound_let(lambda do fm_violin(0, 1, 440, 0.1) end) do |tmp|
    clm_mix(tmp)
  end
  }
end

with_sound(:verbose, true) do
  with_mix "s6", %Q{
  sound_let(lambda do fm_violin(0, 1, 440, 0.1) end,
            [:reverb, :nrev, lambda do clm_mix("oboe.snd") end]) do |tmp, tmp1|
    clm_mix(tmp1)
    clm_mix(tmp, :scale, 0.2, :output_frame, seconds2samples(1))
  end
  fm_violin(0.5, 0.1, 330, 0.1)
  }
end

with_sound(:verbose, true) do
  sound_let(lambda do
                with_mix "s7", 0, %Q{
                  sound_let(lambda do fm_violin(0, 1, 440, 0.1) end,
                            lambda do clm_mix("oboe.snd") end) do |tmp, tmp1|
                    clm_mix(tmp1)
                    clm_mix(tmp, :output_frame, @srate)
                  end
                  fm_violin(0.5, 0.1, 330, 0.1)
                }
              end) do |tmp0|
    clm_mix(tmp0, :scale, 0.5)
  end
end
=end

# Code:

require "examp"
require "sndlib" unless provided? :sndlib
with_silence do
  unless defined? Etc.getlogin
    require "etc"
  end
  unless defined? Socket.gethostname
    require "socket"
  end
end
require "hooks"

$clm_version            = "28-Nov-2006"
$output                 = nil
$reverb                 = nil
$clm_file_name          = "test.snd" unless defined? $clm_file_name
$clm_comment            = nil        unless defined? $clm_comment
$clm_statistics         = false      unless defined? $clm_statistics
$clm_play               = 0          unless defined? $clm_play
$clm_player             = "sndplay"  unless defined? $clm_player
$clm_reverb_file_name   = nil        unless defined? $clm_reverb_file_name
$clm_reverb_channels    = 1          unless defined? $clm_reverb_channels
$clm_reverb             = nil        unless defined? $clm_reverb
$clm_reverb_data        = []         unless defined? $clm_reverb_data
$clm_delete_reverb      = false      unless defined? $clm_delete_reverb
$clm_verbose            = $VERBOSE   unless defined? $clm_verbose
$clm_info               = false      unless defined? $clm_info
$clm_clipped            = true       unless defined? $clm_clipped
$clm_notehook           = nil        unless defined? $clm_notehook
$clm_decay_time         = 1.0        unless defined? $clm_decay_time
$clm_audio_format       = Mus_lshort unless defined? $clm_audio_format
$clm_file_buffer_size   = 65536      unless defined? $clm_file_buffer_size
$clm_table_size         = 512        unless defined? $clm_table_size
$clm_array_print_length = 8          unless defined? $clm_array_print_length

if provided? :snd
  $clm_srate         = default_output_srate         unless defined? $clm_srate
  $clm_channels      = default_output_chans         unless defined? $clm_channels
  $clm_header_type   = default_output_header_type   unless defined? $clm_header_type
  $clm_data_format   = default_output_data_format   unless defined? $clm_data_format
  $clm_locsig_type   = locsig_type                  unless defined? $clm_locsig_type
  $clm_rt_bufsize    = dac_size                     unless defined? $clm_rt_bufsize
  $clm_output_device = audio_output_device          unless defined? $clm_output_device
else
  $clm_srate         = 22050             unless defined? $clm_srate
  $clm_channels      = 1                 unless defined? $clm_channels
  $clm_header_type   = Mus_next          unless defined? $clm_header_type
  $clm_data_format   = Mus_lfloat        unless defined? $clm_data_format
  $clm_locsig_type   = Mus_interp_linear unless defined? $clm_locsig_type
  $clm_rt_bufsize    = 512               unless defined? $clm_rt_bufsize
  $clm_output_device = Mus_audio_default unless defined? $clm_output_device
end

module WS
  ws_doc = "\
 with_sound(*args) do ... end
 
   :output             $clm_file_name        test.snd
   :channels           $clm_channels         1
   :srate              $clm_srate            22050
   :header_type        $clm_header_type      Mus_next
   :data_format        $clm_data_format      Mus_lfloat
   :audio_format       $clm_audio_format     Mus_lshort
 
   :reverb             $clm_reverb           nil
   :reverb_data        $clm_reverb_data      []
   :reverb_channels    $clm_reverb_channels  1
   :revfile            $clm_reverb_file_name nil
   :delete_reverb      $clm_delete_reverb    false
 
   :decay_time         $clm_decay_time       1.0
   :scaled_to          false
   :scaled_by          false
   :continue_old_file  false
   :notehook           $clm_notehook         nil
  
   :play               $clm_play             0
   :statistics         $clm_statistics       false
   :comment            $clm_comment          nil
   :locsig_type        $clm_locsig_type      Mus_interp_linear
   :verbose            $clm_verbose          false

# next option works only with newer Ruby versions
# save the with_sound-body in sound file's comment
   :save_body          false

# if an instrument uses run_instrument/run_reverb and INFO is true, a
# message will be printed: `instrument_name: start 0.000, dur 0.000'
   :info               $clm_info             false
   :clipped            $clm_clipped          true
   :player             $clm_player           sndplay
# save sound after computing
   :save               false
# special with_dac options:
   :bufsize            $clm_rt_bufsize       512
   :device             $clm_output_device    Mus_audio_default
 
 Usage: with_sound(:play, 1, :statistics, true) do fm_violin end"
  
  def ws_interrupt?
    if c_g?
      raise(Interrupt, format("%s interrupted by user (C-g)", get_func_name(3)), caller(1))
    end
  end

  def ws_break(*rest)
    msg = format("%s received Break", get_func_name(2))
    unless rest.null?
      msg += ":"
      rest.each do |s| msg += format(" %s,", s) end
    end
    raise(Break, msg.chomp(","), caller(1))
  end

  add_help(:with_reverb,
           "with_reverb(reverb, reverb_amount=0.05, *with_sound_args)
with_reverb(:jc_reverb)
require 'clm-ins'
with_reverb(:jl_reverb, 0.2)")
  def with_reverb(reverb, reverb_amount = 0.05, *args)
    ws = With_Snd.new(:reverb, reverb,
                      :output, file_name(snd),
                      *args)
    ws.with_reverb(reverb_amount)
    ws
  end
  
  add_help(:with_sound, ws_doc)
  def with_sound(*args, &body)
    clm = get_args(args, :clm, true)
    out = get_args(args, :out_buffer, false)
    if vct?(out) or sound_data?(out) then clm = false end
    ws = if clm
           With_CLM.new(*args, &body)
         else
           With_Snd.new(*args, &body)
         end
    if get_args(args, :help, false)
      Snd.message(ws.help)
    else
      ws.run
    end
    ws
  end
  
  add_help(:with_dac, ws_doc)
  def with_dac(*args, &body)
    ws = With_DAC.new(*args, &body)
    if get_args(args, :help, false)
      Snd.message(ws.help)
    else
      ws.run
    end
    ws
  end
  
  add_help(:clm_load, "clm_load(rbm_file, *with_sound_args)\n" + ws_doc)
  def clm_load(rbm_file, *args)
    assert_type(File.exists?(rbm_file), rbm_file, 1, "an existing file")
    with_sound(*args) do
      if @verbose then Snd.message("Loading %s", rbm_file.inspect) end
      eval(File.open(rbm_file).read, nil, format("(clm_load %s)", rbm_file), 1)
    end
  end

  add_help(:with_temp_sound,
           "with_temp_sound(snd = false) do |temp_snd_file_name| ... end \
Saves SND in a temporary file, which name can be accessed in the body code.  \
After finishing the body, the file will be removed.")
  def with_temp_sound(snd = false, &body)
    t = tempnam
    save_sound_as(t, snd)
    ret = body.call(t)
    remove_file(t)
    ret
  end

  def make_default_comment
    format("Written %s by %s at %s using clm (ruby) of %s",
           Time.new.localtime.strftime("%a %d-%b-%y %H:%M %Z"),
           Etc.getlogin,
           Socket.gethostname,
           $clm_version)
  end
  
  def remove_file(file)
    if provided? :snd and (snd = find_sound(file))
      revert_sound(snd)
      close_sound_extend(snd)
    end
     File.owned?(file) and File.unlink(file)
  end

  def each_sample(start, dur)
    beg, ends = times2samples(start, dur)
    (beg...ends).each do |samp| yield(samp) end
  end

  def sound_data_frame_ref(data, samp, frm)
    sound_data_chans(data).times do |chn|
      frame_set!(frm, chn, sound_data_ref(data, chn, samp))
    end
    frm
  end

  def sound_data_frame_set!(data, samp, frm)
    sound_data_chans(data).times do |chn|
      sound_data_set!(data, chn, samp, frame_ref(frm, chn))
    end
  end

  def process_times
    if defined? Process.times
      Process.times
    else
      Time.times
    end
  end
end

class With_sound
  include Info
  include WS
  @@file_number = 0

  def initialize(*args, &body)
    @output          = get_args(args, :output, $clm_file_name)
    @channels        = get_args(args, :channels, $clm_channels)
    @srate           = get_args(args, :srate, $clm_srate).to_f
    @header_type     = get_args(args, :header_type, $clm_header_type)
    @data_format     = get_args(args, :data_format, $clm_data_format)
    @audio_format    = get_args(args, :audio_format, $clm_audio_format)
    @out_buffer      = get_args(args, :out_buffer, false)

    @reverb          = get_args(args, :reverb, $clm_reverb)
    @reverb_data     = get_args(args, :reverb_data, $clm_reverb_data)
    @reverb_channels = get_args(args, :reverb_channels, $clm_reverb_channels)
    @revfile         = get_args(args, :reverb_file_name, nil)
    @delete_reverb   = get_args(args, :delete_reverb, $clm_delete_reverb)
    @rev_buffer      = get_args(args, :rev_buffer, false)

    @decay_time      = get_args(args, :decay_time, $clm_decay_time)
    @scaled_to       = get_args(args, :scaled_to, false)
    @scaled_by       = get_args(args, :scaled_by, false)

    @continue        = get_args(args, :continue_old_file, false)
    @notehook        = get_args(args, :notehook, $clm_notehook)
    @save_body       = get_args(args, :save_body, false)
    @save            = get_args(args, :save, false)
    @player          = get_args(args, :player, $clm_player)

    @play            = get_args(args, :play, $clm_play)
    @statistics      = get_args(args, :statistics, $clm_statistics)
    @verbose         = get_args(args, :verbose, $clm_verbose)
    @info            = get_args(args, :info, $clm_info)
    @comment         = get_args(args, :comment, $clm_comment)
    @locsig_type     = get_args(args, :locsig_type, $clm_locsig_type)
    @clipped         = get_args(args, :clipped, :undefined)
    @offset          = get_args(args, :offset, 0.0)

    @rtime = @utime = @stime = 0.0
    @body = body
    if @revfile.null? then @revfile = make_reverb_file_name() end
    @reverb_amount = 0.05
    # play: either :play, true
    #       or     :play, false
    #       or     :play, nil
    #       or     :play, integer (times to play)
    @play = case @play
            when TrueClass
              1
            when FalseClass, NilClass
              0
            when Numeric
              Integer(@play).abs
            else
              0
            end
    # without reverb: either :reverb_channels, 0
    #                 or     :reverb, false
    if @reverb_channels.zero?
      @reverb = $reverb = false
    end
    unless string?(@comment) then @comment = make_default_comment end
    if @save_body and proc?(@body) then @comment << "\n" << @body.to_body end
    @old_srate = mus_srate
    @old_update_interval = if defined? auto_update_interval
                             auto_update_interval
                           else
                             false
                           end
    @start_frame = 0
    @locsig = nil
    if @clm
      $output = @ws_output = false
      $reverb = @ws_reverb = false
    else
      $output = @ws_output = @out_buffer
      $reverb = @ws_reverb = @rev_buffer
    end
    @out_snd = @rev_snd  = false
    @clm_instruments = Hash.new
    @with_sound_note_hook = Hook.new("@with_sound_note_hook", 3, "\
lambda do |inst_name, start, dur| ... end: called if an instrument has
the run_instrument/run_reverb loop included.

Every time an instrument starts computing, @with_sound_note_hook is
called with the instrument name INST_NAME, the start time START, and
the duration DUR.

def my_notehook(name, start, dur)
  if name =~ /violin/
    Snd.message(\"%s: start %1.3f, dur %1.3f\", name, start, dur)
  end
end

with_sound(:notehook, :my_notehook) do
  fm_violin(0, 1, 440, 0.1)
  fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
  abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
  fm_bell(0.5, 2.0, 220, 0.5, abell, fbell, 1)
end

installs the @with_sound_note_hook and prints the line

  `# fm_violin: start 0.000, dur 1.000'.")
    if @notehook
      assert_type((proc?(@notehook) or func?(@notehook)), @notehook, 0, "a proc or a function name")
      prc = case @notehook
            when Proc
              @notehook
            when Symbol, String
              # older Ruby versions return -1 on method(:func).to_proc.arity
              if method(@notehook).to_proc.arity == 3
                method(@notehook).to_proc
              else
                lambda do |inst_name, start, dur| @notehook.call(inst_name, start, dur) end
              end
            end
      @with_sound_note_hook.add_hook!("with-sound-note-hook", &prc)
    end
    set_help
  end
  attr_reader :output, :with_sound_note_hook
  alias help description
  
  def to_s
    format("#<%s: output: %s, channels: %d, srate: %d%s>",
           self.class, @output.inspect, @channels, @srate.to_i,
           @reverb ? format(", reverb: %s, reverb-channels: %d", @reverb, @reverb_channels) : "")
  end
  alias inspect to_s

  def describe
    show_local_variables
  end
  
  def with_sound(*args, &body)
    com = format("%s#%s: temporary sound, args %s", self.class, get_func_name, args.inspect)
    ws = self.class.new(:output,            get_args(args, :output, @output),
                        :comment,           get_args(args, :comment, com),
                        :play,              get_args(args, :play, false),
                        :statistics,        get_args(args, :statistics, false),
                        :save,              get_args(args, :save, true),
                        :reverb,            get_args(args, :reverb, false),
                        :reverb_data,       get_args(args, :reverb_data, @reverb_data),
                        :reverb_file_name,  get_args(args, :reverb_file_name, @revfile),
                        :reverb_channels,   get_args(args, :reverb_channels, @reverb_channels),
                        :delete_reverb,     get_args(args, :delete_reverb, @delete_reverb),
                        :decay_time,        get_args(args, :decay_time, @decay_time),
                        :continue_old_file, get_args(args, :continue_old_file, @continue),
                        :out_buffer,        get_args(args, :out_buffer, @out_buffer),
                        :rev_buffer,        get_args(args, :rev_buffer, @rev_buffer),
                        :scaled_to,         get_args(args, :scaled_to, @scaled_to),
                        :scaled_by,         get_args(args, :scaled_by, @scaled_by),
                        :notehook,          get_args(args, :notehook, @notehook),
                        :save_body,         get_args(args, :save_body, @save_body),
                        :channels,          get_args(args, :channels, @channels),
                        :srate,             get_args(args, :srate, @srate).to_f,
                        :header_type,       get_args(args, :header_type, @header_type),
                        :data_format,       get_args(args, :data_format, @data_format),
                        :audio_format,      get_args(args, :audio_format, @audio_format),
                        :verbose,           get_args(args, :verbose, @verbose),
                        :info,              get_args(args, :info, @info),
                        :clipped,           get_args(args, :clipped, @clipped),
                        :offset,            get_args(args, :offset, 0.0),
                        &body)
    ws.run
    ws
  end

  def scaled_to(scale, &body)
    with_current_sound(:scaled_to, scale, &body)
  end

  def scaled_by(scale, &body)
    with_current_sound(:scaled_by, scale, &body)
  end
  
  def with_offset(secs, &body)
    with_current_sound(:offset, secs, &body)
  end
  
  def with_current_sound(*args, &body)
    output, offset, scaled_to, scaled_by = nil
    optkey(args, binding,
           [:output, tempnam],
           [:offset, 0.0],
           [:scaled_to, false],
           [:scaled_by, false])
    ws = with_sound(:output, output,
                    :scaled_to, scaled_to,
                    :scaled_by, scaled_by,
                    :offset, offset,
                    *args, &body)
    clm_mix(ws.output)
    remove_file(ws.output)
    ws
  end

  def sound_let(*args, &body)
    outfile_list = []
    arg_list = []
    args.each do |sl_args|
      if proc?(sl_args) or (array?(sl_args) and proc?(sl_args.last))
        if proc?(sl_args)
          ws_body = sl_args
          ws_args = []
        else
          ws_body = sl_args.pop
          ws_args = sl_args.dup
        end
        ws_args.push(:output, tempnam())
        outfile_list.push(with_sound(*ws_args, &ws_body).output)
        arg_list.push(outfile_list.last)
      else
        arg_list.push(sl_args)
      end
    end
    body.call(*arg_list)
  rescue
    raise
  ensure
    outfile_list.apply(:remove_file)
  end

  def with_mix(*args)
    body_str = args.pop
    assert_type(string?(body_str), body_str, 0, "a string (body string)")
    beg = if number?(args[-1])
            args.pop
          else
            0
          end
    fname = args.pop
    assert_type(string?(fname), fname, 0, "a string (filename)")
    out_file = fname + ".snd"
    rbm_file = fname + ".rbm"
    snd_time = if File.exists?(out_file)
                 File.mtime(out_file)
               else
                 :load
               end
    old_body = if File.exists?(rbm_file)
                 File.open(rbm_file).read
               else
                 ""
               end
    rbm_time = if old_body == body_str
                 File.mtime(rbm_file)
               else
                 File.open(rbm_file, "w") do |f| f << body_str end
                 :load
               end
    if snd_time == :load or rbm_time == :load or snd_time < rbm_time
      clm_load(rbm_file,
               :output, out_file,
               :comment, format("[%s, %s]", args.to_s.inspect, body_str.inspect),
               *args)
    end
    clm_mix(out_file, :output_frame, seconds2samples(beg))
  end

  def with_sound_info(name, start, dur)
    if @info
      Snd.message("%s: start %1.3f, dur %1.3f", name, start, dur)
    end
    if @notehook
      @with_sound_note_hook.call(name, start, dur)
    end
  end
  
  def run_instrument(start, dur, *locsig_args, &environ)
    @start_frame = seconds2samples(start + @offset)
    name = get_func_name(3)
    with_sound_info(name, start, dur)
    @clm_instruments.store(environ, [name, start, dur])
    degree, distance, reverb_amount = nil
    optkey(locsig_args, binding,
           [:degree, random(90.0)],
           [:distance, 1.0],
           [:reverb_amount, 0.05])
    unless @clm
      unless sound_data?(@ws_output) or vct?(@ws_output)
        len = seconds2samples(dur)
        @ws_output = if @channels == 1
                        Vct.new(len, 0.0)
                      else
                        SoundData.new(@channels, len)
                      end
      end
      if @reverb and (not sound_data?(@ws_reverb) or vct?(@ws_reverb))
        len += seconds2samples(@decay_time)
        @ws_reverb = if @reverb_channels == 1
                        Vct.new(len, 0.0)
                      else
                        SoundData.new(@reverb_channels, len)
                      end
      end
    end
    @reverb_amount = reverb_amount
    @locsig = make_locsig(:degree,   degree,
                          :distance, distance,
                          :reverb,   reverb_amount,
                          :output,   @ws_output,
                          :revout,   @ws_reverb,
                          :channels, @channels,
                          :type,     @locsig_type)
  end

  # case chan
  # when Integer
  #   # jc_reverb_rb, jl_reverb_rb, nrev_rb
  #   body.call(sample_of_CHAN, current_sample_number)
  # when :frames
  #   # freeverb_rb
  #   body.call(frame_of_reverb_file, current_sample_number)
  # end
  def run_reverb(chan, &environ)
    name = get_func_name(3)
    dur = ws_duration(@revfile) + @decay_time
    with_sound_info(name, 0, dur)
    @clm_instruments.store(environ, [name, 0, dur])
    if @verbose
      Snd.message("%s on %d in and %d out channel%s",
                  name,
                  @reverb_channels,
                  @channels,
                  (@channels > 1 ? "s" : ""))
    end
    unless chan.between?(0, @reverb_channels - 1)
      Snd.raise(:out_of_range, chan, @reverb_channels, "reverb channel number is out of range")
    end
  end
  
  def run(&body)
    @body ||= body
    set_clm_table_size($clm_table_size)
    set_mus_file_buffer_size($clm_file_buffer_size)
    set_mus_array_print_length($clm_array_print_length)
    if @clipped == :undefined
      if (@scaled_by or @scaled_to) and
          [Mus_bfloat, Mus_lfloat, Mus_bdouble, Mus_ldouble].member?(@data_format)
        set_mus_clipping(false)
      else
        set_mus_clipping($clm_clipped)
      end
    else
      set_mus_clipping(@clipped)
    end
    if defined? set_auto_update_interval then set_auto_update_interval(0.0) end
    before_output
    init_process_time
    run_body
    after_output
    stop_process_time
    finish_sound
    1.upto(@play) do play_it end
  end

  protected
  def run_body
    frm1 = ws_frame_location
    instance_eval(&@body)
  rescue Interrupt, ScriptError, NameError, StandardError
    finish_sound
    err, $! = $!, nil
    show_local_variables
    case err
    when Interrupt, Break
      # C-g, ws_break
      Snd.message("with_sound body: %s", err.message)
    else
      raise err
    end
  else
    frm2 = ws_frame_location
    if @scaled_to then scaled_to_sound(frm1, frm2 - frm1) end
    if @scaled_by then scaled_by_sound(frm1, frm2 - frm1) end
  end
  
  def run_reverb_body
    case @reverb
    when Proc
      @reverb.call(*@reverb_data)
    when String, Symbol
      snd_func(@reverb, *@reverb_data)
    end
  rescue Interrupt, ScriptError, NameError, StandardError
    finish_sound
    err, $! = $!, nil
    show_local_variables
    case err
    when Interrupt, Break
      # C-g, ws_break
      Snd.message("with_sound body (reverb): %s", err.message)
    else
      raise err
    end
  end

  def show_local_variables
    Snd.message()
    # {run_instrument|reverb-proc => [instrument-name, start, dur]}
    # sorted by value[start]
    # environ == proc
    @clm_instruments.sort do |a, b| a.cadr.cadr <=> b.cadr.cadr end.each do |environ, vals|
      Snd.message("=== %s [%1.3f-%1.3f] ===", *vals)
      eval("local_variables", environ).each do |var|
        Snd.message("%s = %s", var, eval(var, environ).inspect)
      end
      Snd.message()
    end
    @clm_instruments.values
  end
  
  def tempnam
    if provided? :snd
      snd_tempnam()
    else
      @@file_number += 1
      dir = (ENV.map do |k, v| v if /TMP/ =~ k end.compact.first or "/tmp")
      format("%s/snd_%d_%d.snd", dir, $$, @@file_number)
    end
  end

  def make_reverb_file_name
    path = File.split(@output).first
    file = File.basename(@output, ".*") + ".reverb"
    unless path == "." then file = path + "/" + file end
    file
  end

  def init_process_time
    @rtime = Time.now
    tms = process_times
    @utime = tms.utime
    @stime = tms.stime
  end
  
  def stop_process_time
    tms = process_times
    @rtime = Time.now - @rtime
    @utime = tms.utime - @utime
    @stime = tms.stime - @stime
    if @statistics then statistics end
  end
  
  def before_output
    set_mus_srate(@srate)
  end

  def after_output
  end

  def after_ws
    if provided? :snd
      if snd = find_sound(@output)
        update_sound(snd)
      else
        open_sound(@output)
      end
    end
  end
  
  def ws_frame_location
  end
  
  def scaled_to_sound(from, to)
  end

  def scaled_by_sound(from, to)
  end
  
  def statistics(frms, data_fmt, type)
    Snd.message("filename: %s", @output.inspect)
    Snd.message("   chans: %d, srate: %d", @channels, @srate.round)
    if data_fmt and type
      Snd.message("  format: %s [%s]", mus_data_format_name(data_fmt), mus_header_type_name(type))
    end
    if frms > 0
      Snd.message("  length: %1.3f (%d frames)", frms / @srate, frms)
    end
    Snd.message("    real: %1.3f  (utime %1.3f, stime %1.3f)", @rtime, @utime, @stime) # 
    if frms > 1
      rt = (@srate / frms)
      Snd.message("   ratio: %1.2f  (uratio %1.2f)", @rtime * rt, @utime * rt)
    end
  end
  
  def finish_sound
    if defined? set_auto_update_interval then set_auto_update_interval(@old_update_interval) end
    @reverb and @delete_reverb and (not @continue) and remove_file(@revfile)
    set_mus_srate(@old_srate)
  end

  def play_it
  end
  
  def set_help
    self.description = format("\
# with_sound(*args) do ... end
# 
#   :output             $clm_file_name (#{$clm_file_name.inspect})
#   :channels           $clm_channels (#$clm_channels)
#   :srate              $clm_srate (#$clm_srate)
#   :header_type        $clm_header_type (#$clm_header_type)
#   :data_format        $clm_data_format (#$clm_data_format)
#   :audio_format       $clm_audio_format (#$clm_audio_format)
#   :out_buffer         nil (vct or sound-data object)
# 
#   :reverb             $clm_reverb (#{$clm_reverb.inspect})
#   :reverb_data        $clm_reverb_data (#{$clm_reverb_data.inspect})
#   :reverb_channels    $clm_reverb_channels (#$clm_reverb_channels)
#   :revfile            $clm_reverb_file_name (#{$clm_reverb_file_name.inspect})
#   :delete_reverb      $clm_delete_reverb (#$clm_delete_reverb)
#   :rev_buffer         nil (vct or sound-data object)
# 
#   :decay_time         $clm_decay_time (#{$clm_decay_time})
#   :scaled_to          false
#   :scaled_by          false
#   :continue_old_file  false
#   :notehook           $clm_notehook (#{$clm_notehook.inspect})
#  
#   :play               $clm_play (#$clm_play)
#   :statistics         $clm_statistics (#$clm_statistics)
#   :comment            $clm_comment (#{$clm_comment.inspect})
#   :locsig_type        $clm_locsig_type (#{$clm_locsig_type})
#   :verbose            $clm_verbose (#$clm_verbose)
#
## next option works only with newer Ruby versions
## save the with_sound-body in sound file's comment
#   :save_body          false
#
## if an instrument uses run_instrument/run_reverb and INFO is true, a
## message will be printed: `instrument_name: start 0.000, dur 0.000'
#   :info               $clm_info (#$clm_info)
#   :clipped            $clm_clipped (#$clm_clipped)
#
#   :player             $clm_player (#{$clm_player.inspect})
#
## save sound after computing
#   :save               false
#
## special with_dac options:
#   :bufsize            $clm_rt_bufsize (#$clm_rt_bufsize)
#   :device             $clm_output_device (#$clm_output_device)
#
# Usage: with_sound(:play, 1, :statistics, true) do fm_violin end")
  end
end

class Instrument < With_sound
  # Actually it isn't necessary to define instruments as methods of
  # class Instrument.
  def my_simp(start = 0, dur = 1, freq = 440, amp = 0.5, amp_env = [0, 1, 1, 1])
    os = make_sum_of_cosines(:frequency, freq, :cosines, 3)
    en = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    fs = hz2radians(freq)
    pv = make_triangle_wave(:frequency, 6.0, :amplitude, 0.0025 * fs)
    rv = make_rand_interp(:frequency, 8.0, :amplitude, 0.005 * fs)
    run_instrument(start, dur) do
      sum_of_cosines(os, triangle_wave(pv) + rand_interp(rv)) * env(en)
    end
  end

  # simple violin, see snd/fm.html
  def fm_v(start = 0, dur = 1, freq = 440, amp = 0.5, fm_index = 1, amp_env = [0, 1, 1, 1])
    frq_scl = hz2radians(freq)
    maxdev = frq_scl * fm_index
    index1 = maxdev * (5.0 / log(freq))
    index2 = maxdev * 3.0 * ((8.5 - log(freq)) / (3.0 + freq / 1000.0))
    index3 = maxdev * (4.0 / sqrt(freq))
    carrier = make_oscil(:frequency, freq)
    fmosc1 = make_oscil(:frequency, freq)
    fmosc2 = make_oscil(:frequency, freq * 3.0)
    fmosc3 = make_oscil(:frequency, freq * 4.0)
    ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    indf1 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index1, :duration, dur)
    indf2 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index2, :duration, dur)
    indf3 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0], :scaler, index3, :duration, dur)
    pervib = make_triangle_wave(:frequency, 0.5, :amplitude, 0.0025 *  frq_scl)
    ranvib = make_rand_interp(:frequency, 16.0, :amplitude, 0.005 * frq_scl)
    run_instrument(start, dur) do
      vib = triangle_wave(pervib) + rand_interp(ranvib)
      env(ampf) * oscil(carrier,
                        vib + \
                        env(indf1) * oscil(fmosc1, vib) + \
                        env(indf2) * oscil(fmosc2, 3.0 * vib) + \
                        env(indf3) * oscil(fmosc3, 4.0 * vib))
    end
  end
end

# Channel2Vct and Sample_Reader are helper classes used by class
# Snd_Instrument.  sample_reader has no possibility to set location
# (and direction) which is needed by instrument grani (clm-ins.rb).
class Channel2Vct
  # (channel->vct (beg 0) (dur len) (snd #f) (chn #f) (edpos #f))
  def initialize(snd = false, chn = false, start = 0, dir = 1)
    @vct = channel2vct(0, frames(snd, chn), snd, chn, false)
    @location = ((dir > 0) ? (start - 1) : (start + 1))
    @start = start
    @direction = dir
    @snd = snd
    @chn = chn
    @last_location = @vct.length - 1
  end
  attr_accessor :direction
  
  def inspect
    format("%s.new(%s, %s, %s, %s)", self.class, @snd.inspect, @chn.inspect, @start, @direction)
  end
  
  def to_s
    format("#<%s snd: %s, chn: %s, location: %d, direction: %d, vct: %s>",
           self.class, @snd.inspect, @chn.inspect, location, @direction, @vct.to_str)
  end

  def next
    if @direction > 0
      if @location < @last_location
        @location += 1
        @vct[@location]
      else
        0.0
      end
    else
      if @location > 0
        @location -= 1
        @vct[@location]
      else
        0.0
      end
    end
  end
  
  def close
    # not needed
  end
  
  def location
    if @direction > 0
      @location + 1
    else
      @location - 1
    end
  end

  def location=(val)
    if @direction > 0
      @location = val - 1
    else
      @location = val + 1
    end
  end
end

class Sample_Reader
  # (make-sample-reader (start-samp 0) (snd #f) (chn #f) (dir 1) (edpos #f))
  def initialize(snd = false, chn = false, start = 0, dir = 1)
    @reader = make_sample_reader(start, snd, chn, dir, false)
    @direction = dir
    @start = start
    @direction = dir
    @snd = snd
    @chn = chn
  end
  attr_accessor :direction
  
  def inspect
    format("%s.new(%s, %s, %s, %s)", self.class, @snd.inspect, @chn.inspect, @start, @direction)
  end
  
  def to_s
    @reader.inspect
  end

  def next
    read_sample(@reader)
  end

  def close
    free_sample_reader(@reader)
  end
  
  def location
    sample_reader_position(@reader)
  end

  def location=(val)
    # sample_reader_position isn't settable
  end
end

class Snd_Instrument < Instrument
  # place holder for special Snd instruments, see FULLMIX in
  # clm-ins.rb.

  def get_snd(file)
    snd = if integer?(file)
            file
          elsif string?(file)
            if s = find_sound(file)
              s
            else
              open_sound(file)
            end
          end
    if integer?(snd) and sound?(snd)
      snd
    else
      Snd.snd(snd)
      #Snd.raise(:no_such_sound, snd.inspect)
    end
  end
  private :get_snd
  
  def make_ws_reader(file, *args)
    start, channel, direction = nil
    optkey(args, binding,
           [:start, 0],
           [:channel, 0],
           [:direction, 1])
    if get_args(args, :vct?, false)
      Channel2Vct.new(get_snd(file), channel, start, direction)
    else
      Sample_Reader.new(get_snd(file), channel, start, direction)
    end
  end

  # (read-sample reader)
  def ws_readin(rd)
    rd.next
  end

  def close_ws_reader(file, rd)
    rd.close
    if snd = (integer?(file) ? file : find_sound(file))
      close_sound_extend(snd)
    end
  end

  def ws_location(rd)
    rd.location
  end

  def set_ws_location(rd, val)
    rd.location = val.to_i
  end

  def ws_increment(rd)
    rd.direction
  end

  def set_ws_increment(rd, val)
    rd.direction = val.to_i
  end
  
  def ws_srate(file)
    srate(get_snd(file)).to_f
  end

  def ws_channels(file)
    channels(get_snd(file))
  end
  
  def ws_duration(file)
    snd = get_snd(file)
    frames(snd) / srate(snd).to_f
  end
end

class CLM_Instrument < Instrument
  # place holder for special Snd instruments, see FULLMIX in
  # clm-ins.rb.

  # (make-readin (:file) (:channel 0) (:start 0) (:direction 1)
  def make_ws_reader(file, *args)
    start, channel, direction = nil
    optkey(args, binding,
           [:start, 0],
           [:channel, 0],
           [:direction, 1])
    make_readin(:file, file, :channel, channel, :start, start, :direction, direction)
  end

  # (readin gen)
  def ws_readin(rd)
    readin(rd)
  end

  def close_ws_reader(file, rd)
    mus_close(rd)
  end

  def ws_location(rd)
    mus_location(rd)
  end

  def set_ws_location(rd, val)
    set_mus_location(rd, val)
  end

  def ws_increment(rd)
    mus_increment(rd)
  end

  def set_ws_increment(rd, val)
    set_mus_increment(rd, val)
  end

  def ws_srate(file)
    mus_sound_srate(file)
  end

  def ws_channels(file)
    mus_sound_chans(file)
  end
  
  def ws_duration(file)
    mus_sound_duration(file)
  end
end

class With_Snd < Snd_Instrument
  def initialize(*args, &body)
    @clm = false
    super(*args, &body)
  end
  
  def run_instrument(start, dur, *locsig_args, &body)
    super(start, dur, *locsig_args, &body)
    ws_interrupt?
    @ws_output.length.times do |samp|
      locsig(@locsig, samp, body.call(@start_frame + samp))
    end
    if sound_data?(@ws_output)
      @channels.times do |chn|
        mix_vct(@ws_output.to_vct(chn), @start_frame, @out_snd, chn, false)
      end
    elsif vct?(@ws_output)
      mix_vct(@ws_output, @start_frame, @out_snd, 0, false)
    end
    ws_interrupt?
    if @reverb
      if sound_data?(@ws_reverb)
        @reverb_channels.times do |chn|
          mix_vct(@ws_reverb.to_vct(chn), @start_frame, @rev_snd, chn, false)
        end
      elsif vct?(@ws_reverb)
        mix_vct(@ws_reverb, @start_frame, @rev_snd, 0, false)
      end
    end
    ws_interrupt?
  end
  
  def run_reverb(chan = 0, &body)
    rev_out = SoundData.new(@reverb_channels, @ws_reverb.length)
    ws_interrupt?
    case chan
    when Integer
      super(chan, &body)
      if sound_data?(@ws_reverb)
        rev_out.length.times do |i|
          sound_data_frame_set!(rev_out, i, body.call(@ws_reverb[chan, i], i))
        end
      elsif vct?(@ws_reverb)
        rev_out.length.times do |i|
          sound_data_frame_set!(rev_out, i, body.call(@ws_reverb[i], i))
        end
      end
    when :frames
      super(0, &body)
      if sound_data?(@ws_reverb)
        frm = make_frame(@reverb_channels)
        rev_out.length.times do |i|
          @ws_reverb.chans.times do |chn| frame_set!(frm, chn, @ws_reverb[chn, i]) end
          sound_data_frame_set!(rev_out, i, body.call(frm, i))
        end
      elsif vct?(@ws_reverb)
        frm = make_frame(1)
        rev_out.length.times do |i|
          frame_set!(frm, 0, @ws_reverb[i])
          sound_data_frame_set!(rev_out, i, body.call(frm, i))
        end
      end
    end
    ws_interrupt?
    @channels.times do |chn| mix_vct(rev_out.to_vct(chn), 0, @out_snd, chn, false) end
    ws_interrupt?
  end
  
  add_help(:clm_mix, "clm_mix(filename, *args)
        :input_frame  = 0
        :output_frame = 0
        :frames       = mus_sound_frames(filename)
        :scale        = 1.0
Example: clm_mix(\"tmp\")")
  def clm_mix(filename, *args)
    input_frame, output_frame, frames, scale = nil
    optkey(args, binding,
           [:input_frame, 0],
           [:output_frame, 0],
           [:frames, mus_sound_frames(filename)],
           [:scale, 1.0])
    unless snd = find_sound(filename)
      unless snd = open_sound(filename)
        Snd.raise(:no_such_file, filename, "file name required")
      end
    end
    [channels(snd), @channels].min.times do |chn|
      scale_channel(scale, input_frame, frames, snd, chn) if scale.nonzero?
      mix_vct(channel2vct(input_frame, frames, snd, chn), output_frame, @out_snd, chn, false)
    end
    revert_sound(snd)
    close_sound_extend(snd)
  end

  def with_reverb(reverb_amount)
    snd = Snd.snd
    @channels = channels(snd)
    set_mus_srate(@srate)
    len = frames(snd)
    @start_frame = 0
    if rsnd = find_sound(@revfile)
      close_sound_extend(rsnd)
      remove_file(@revfile)
      rsnd = new_sound(@revfile, @header_type, @data_format, @srate.to_i, @reverb_channels)
    else
      rsnd = new_sound(@revfile, @header_type, @data_format, @srate.to_i, @reverb_channels)
    end
    @out_snd = snd
    @rev_snd = rsnd
    @ws_reverb = SoundData.new(@reverb_channels, len)
    init_process_time
    @reverb_channels.times do |chn|
      channel2vct(0, len, @out_snd, chn).scale(reverb_amount).to_sound_data(@ws_reverb, chn)
    end
    after_output
    stop_process_time
    finish_sound
    1.upto(@play) do play_it end
  end

  protected
  def before_output
    super
    snd = rsnd = false
    sr = @srate.to_i
    if sound?(snd = find_sound(@output))
      if @continue
        set_mus_srate(srate(snd))
      else
        close_sound_extend(snd)
        remove_file(@output)
        snd = new_sound(@output, @header_type, @data_format, sr, @channels, @comment)
      end
    else
      snd = new_sound(@output, @header_type, @data_format, sr, @channels, @comment)
    end
    if @reverb
      if sound?(rsnd = find_sound(@revfile))
        unless @continue
          close_sound_extend(rsnd)
          remove_file(@revfile)
          rsnd = new_sound(@revfile, @header_type, @data_format, sr, @reverb_channels)
        end
      else
        rsnd = new_sound(@revfile, @header_type, @data_format, sr, @reverb_channels)
      end
    end
    $output = @out_snd = snd
    $reverb = @rev_snd = rsnd
  end

  def after_output
    if @reverb then run_reverb_body end
    after_ws
  end

  def ws_frame_location
    frames(@out_snd)
  end
  
  def scaled_to_sound(beg, len)
    @ws_output.scale!(@scaled_to / @ws_output.peak)
  end

  def scaled_by_sound(beg, len)
    @ws_output.scale!(@scaled_by)
  end

  def statistics
    super(frames(@out_snd), data_format(@out_snd), header_type(@out_snd))
    Snd.message(" max out: %s", maxamp(@out_snd, true).to_string)
    if @reverb
      Snd.message(" max rev: %s", maxamp(@rev_snd, true).to_string)
    end
  end

  def finish_sound
    if @save then save_sound(@out_snd) end
    select_sound(@out_snd)
    update_sound(@out_snd)
    super
  end

  def play_it
    play(0, @out_snd)
  end

  # with_closed_sound(snd) do |snd_name| ... end
  # returns new snd index
  def with_closed_sound(snd, &body)
    snd_name = file_name(snd)
    save_sound(snd)
    close_sound(snd)
    body.call(snd_name)
    open_sound(snd_name)
  end
end

class With_CLM < CLM_Instrument
  def initialize(*args, &body)
    @clm = true
    super(*args, &body)
  end

  def run_instrument(start, dur, *locsig_args, &body)
    super(start, dur, *locsig_args, &body)
    ws_interrupt?
    len = seconds2samples(dur) - 1
    @start_frame.upto(len) do |samp| locsig(@locsig, samp, body.call(samp)) end
    ws_interrupt?
  end

  def run_reverb(chan = 0, &body)
    @ws_reverb = $reverb
    ws_interrupt?
    len = @ws_reverb.length + seconds2samples(@decay_time)
    case chan
    when Integer
      super(chan, &body)
      len.times do |samp|
        frame2file(@ws_output, samp, body.call(in_any(samp, chan, @ws_reverb), samp))
      end
    when :frames
      super(0, &body)
      frm = make_frame(@reverb_channels)
      len.times do |samp|
        frame2file(@ws_output, samp, body.call(file2frame(@ws_reverb, samp, frm), samp))
      end
    end
    ws_interrupt?
  end

  add_help(:clm_mix, "clm_mix(filename, *args)
        :input_frame  = 0
        :output_frame = 0
        :frames       = mus_sound_frames(filename)
        :scale        = 1.0
Example: clm_mix(\"tmp\")")
  def clm_mix(filename, *args)
    input_frame, output_frame, frames, scale = nil
    optkey(args, binding,
           [:input_frame, 0],
           [:output_frame, 0],
           [:frames, mus_sound_frames(filename)],
           [:scale, 1.0])
    with_closed_output do
      if provided? :snd
        if scale.nonzero?
          unless (snd = find_sound(filename))
            snd = open_sound(filename)
          end
          channels(snd).times do |chn| scale_channel(scale, input_frame, frames, snd, chn) end
          save_sound(snd)
          close_sound_extend(snd)
        end
        mus_mix(@output, filename, output_frame, frames, input_frame)
      else
        mus_mix(@output, filename, output_frame, frames, input_frame,
                make_mixer(@channels, *(1..@channels * @channels).map do scale end))
      end
    end
  end
  
  protected
  def before_output
    super
    if @continue
      @ws_output = continue_sample2file(@output)
      if @reverb
        @ws_reverb = continue_sample2file(@revfile)
      end
      @srate = set_mus_srate(mus_sound_srate(@output))
      if provided?(:snd)
        if sound?(snd = find_sound(@output))
          close_sound(snd)
        end
      end
    else
      remove_file(@output)
      @ws_output = make_sample2file(@output, @channels, @data_format, @header_type, @comment)
      if @reverb
        remove_file(@revfile)
        @ws_reverb = make_sample2file(@revfile, @reverb_channels, @data_format, @header_type)
      end
    end
    $output = @ws_output
    $reverb = @ws_reverb
  end

  def after_output
    if @reverb
      if mus_output?(@ws_reverb) then mus_close(@ws_reverb) end
      old_reverb = @ws_reverb
      # non-RUN_REVERB...END functions need it here
      $reverb = make_file2sample(@revfile)
      run_reverb_body
      if mus_input?(@ws_reverb) then mus_close(@ws_reverb) end
      $reverb = @ws_reverb = old_reverb
    end
    after_ws
  end

  def ws_frame_location
    with_closed_output do
      mus_sound_frames(@output)
    end
  end

  def scaled_to_sound(beg, len)
    if provided? :snd
      unless snd = find_sound(@output)
        snd = open_sound(@output)
      end
      @channels.times do |chn| scale_to(@scaled_to, snd, chn) end
      save_sound(snd)
    else
      amax = mus_sound_maxamp(@output)
      tmpa = []
      1.step(amax.length - 1, 2) do |i| tmpa << amax[i] end
      scale = ([0.0, [1.0, @scaled_to].min].max / tmpa.max) - 1
      mus_mix(@output, @output, beg, len, beg,
              make_mixer(@channels, *(1..@channels * @channels).map do scale end))
    end
  end

  def scaled_by_sound(beg, len)
    if provided? :snd
      unless (snd = find_sound(@output))
        snd = open_sound(@output)
      end
      @channels.times do |chn| scale_channel(@scaled_by, beg, len, snd, chn) end
      save_sound(snd)
    else
      scale = @scaled_by - 1
      mus_mix(@output, @output, beg, len, beg,
              make_mixer(@channels, *(1..@channels * @channels).map do scale end))
    end
  end
  
  def statistics
    super(mus_sound_frames(@output), mus_sound_data_format(@output), mus_sound_header_type(@output))
    ch = "@"
    mus_sound_maxamp(@output).each_pair do |s, v|
      Snd.message("maxamp %s: %1.3f (near %1.3f secs)", ch.next!, v, s / @srate)
    end
    if @reverb
      ch = "@"
      mus_sound_maxamp(@revfile).each_pair do |s, v|
        Snd.message("revamp %s: %1.3f (near %1.3f secs)", ch.next!, v, s / @srate)
      end
    end
    unless (comm = mus_sound_comment(@output)).empty?
      Snd.message(" comment: %s", comm)
    end
  end
  
  def finish_sound
    if mus_output?(@ws_output) then mus_close(@ws_output) end
    if provided? :snd
      if @out_snd = find_sound(@output)
        select_sound(@out_snd)
        update_sound(@out_snd)
      else
        @out_snd = if @header_type == Mus_raw
                       open_raw_sound(@output, @channels, @srate.to_i, @data_format)
                     else
                       open_sound(@output)
                     end
      end
    end
    super
  end

  def play_it
    if provided? :snd
      play(0, @out_snd)
    else
      system(format("%s %s", @player, @output))
    end
  end

  def with_closed_output(&body)
    mus_close(@ws_output) if mus_output?(@ws_output)
    ret = body.call
    $output = @ws_output = continue_sample2file(@output)
    ret
  end
end

class With_DAC < Snd_Instrument
  # no reverb
  # handles instruments parallel if computing is fast enough
  def initialize(*args, &body)
    @clm = false
    super(*args, &body)
    @bufsize = get_args(args, :bufsize, $clm_rt_bufsize)
    @device  = get_args(args, :device, $clm_output_device)
    @ws_reverb = @ws_output = @reverb = false
    @output = "dac"
    # @instruments = [[first_samp, last_samp, body], ...]
    @instruments = []
    @current_sample = 0
  end

  def run_instrument(start, dur, *args, &body)
    # A bad idea; it scales all current parallel instruments in method
    # REAL_RUN with the last called instrument-locsig-gen.
    super(start, dur, *args, &body)
    beg, ends = times2samples(start, dur)
    @instruments.push([beg, ends, body])
    ws_interrupt?
    real_run(beg)
    ws_interrupt?
  end

  def real_run(sample)
    while @current_sample < sample
      idxs = []
      dac_vct = Vct.new(@bufsize)
      # calls bufsize times all instruments in current sample-window
      # and accumulates the result in dac_vct
      @instruments.each_with_index do |arg, idx|
        beg, ends, body = arg
        if @current_sample.between?(beg, ends)
          dac_vct.add!(Vct.new(@bufsize) do |i| body.call(@current_sample + i) end)
        elsif @current_sample >= ends
          idxs.push(idx)
        end
      end
      idxs.each do |idx| @instruments.delete_at(idx) end
      dac_data = SoundData.new(@channels, @bufsize)
      @channels.times do |chn|
        dac_vct.scale(locsig_ref(@locsig, chn)).to_sound_data(dac_data, chn)
      end
      mus_audio_write(@ws_output, dac_data, @bufsize)
      @current_sample += @bufsize
    end
  end

  protected
  def before_output
    super
    @ws_output = mus_audio_open_output(@device, @srate.round, @channels, @audio_format,
                                       @bufsize * @channels * 2)
    if @ws_output < 0
      Snd.raise(:mus_error, @ws_output, "can't open DAC")
    end
  end

  def after_output
    # flush contents of instrument array
    real_run(@current_sample + 1) until @instruments.empty?
  end

  def finish_sound
    if number?(@ws_output) then mus_audio_close(@ws_output) end
    @ws_output = false
    super
  end
  
  def statistics
    super(0, false, false)
  end
end

include WS
require "v"

# ws.rb ends here
