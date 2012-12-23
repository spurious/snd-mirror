# ws.rb -- with_sound and friends for Snd/Ruby

# Copyright (c) 2003-2012 Michael Scholz <mi-scholz@users.sourceforge.net>
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
#   ws_break(*rest)
#   with_reverb(reverb, reverb_amount, snd, *with_sound_args)
#   with_sound(*args) do ... end
#   with_full_sound(*args) do ... end
#   with_temp_sound(*args) do ... end
#   with_dac(*args) do ... end
#   clm_load(rbm_file, *args)
#   with_temp_snd(snd) do |temp_snd_file_name| ... end
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
#   output                    # file name, String
#   out_snd                   # sound index, Fixnum
#   with_sound_note_hook      # note hook, Hook
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
#   with_sound_info(instrument_name, start, dur, binding)
#   run_instrument(start, dur, *locsig_args) do |samp| ... end
#   run_reverb(chan = 0) do |in_val, samp| ... end
#   clm_mix(filename, *args)
#   run do ... end
#
# class Instrument
#   my_simp(start, dur, freq, amp, amp_env = [0, 1, 1, 1])
#   make_ws_reader(file, *args)
#   ws_readin(rd)
#   close_ws_reader(rd)
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
# RUN_INSTRUMENT(start, dur, *locsig_args) do |samp|
#   ...
#   (return next sample)
# end
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
# sound_let(Proc.new do | | fm_violin(0, 1, 330, 0.5) end,
#           1024) do |tmp_file, val|
# end
#
# TMP_FILE is returned by with_sound and VAL is connected to 1024.
#
# If with_sound needs args, the args and the procedure must be in an
# array.
#
# sound_let([:scaled_to, 0.3, :output, "sl.snd",
#            Proc.new do | | fm_violin(0, 1, 330, 0.5) end],
#           1024) do |tmp_file, val|
# end
# 
# Examples:
# 
# One with_sound-call and one temporary file name, arbitrary called TMP:
# 
# sound_let([:reverb, :jc_reverb,
#            Proc.new do | | fm_violin(0, 1, 220, 0.2) end]) do |tmp|
#   mus_mix(@output, tmp)
# end
# 
# Two with_sound-calls and two temporary file names, arbitrary
# called TEMP_1 and TEMP_2:
# 
# sound_let([:reverb, :jc_reverb,
#            Proc.new do | | fm_violin(0, 1, 220, 0.2) end],
#           Proc.new do | | fm_violin(0.5, 1, 440, 0.3) end) do |temp_1, temp_2|
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
  sound_let(Proc.new do | | fm_violin(0, 1, 440, 0.1) end) do |tmp|
    clm_mix(tmp)
  end
  }
end

with_sound(:verbose, true) do
  with_mix "s6", %Q{
  sound_let(Proc.new do | | fm_violin(0, 1, 440, 0.1) end,
            [:reverb, :nrev,
             Proc.new do | | clm_mix("oboe.snd") end]) do |tmp, tmp1|
    clm_mix(tmp1)
    clm_mix(tmp, :scale, 0.2, :output_frame, seconds2samples(1))
  end
  fm_violin(0.5, 0.1, 330, 0.1)
  }
end

with_sound(:verbose, true) do
  sound_let(Proc.new do | |
                with_mix "s7", 0, %Q{
                  sound_let(Proc.new do | | fm_violin(0, 1, 440, 0.1) end,
                            Proc.new do | | clm_mix("oboe.snd") end) do |t1, t2|
                    clm_mix(t2)
                    clm_mix(t1, :output_frame, @srate)
                  end
                  fm_violin(0.5, 0.1, 330, 0.1)
                }
              end) do |t0|
    clm_mix(t0, :scale, 0.5)
  end
end
=end

# Code:

require "clm"
require "hooks"

def clm_find_sound_file(file)
  if File.exists?(file)
    file
  else
    fname = false
    if array?($clm_search_list)
      $clm_search_list.each do |path|
        if File.exists?(fs = path + "/" + file)
          fname = fs
          break
        end
      end
    end
    fname
  end
end

def clm_player(s)
  if provided?(:snd) and sound?(s)
    play(s, :wait, true)
  elsif string?(s) and File.exists?(fs = clm_find_sound_file(s))
    system("sndplay #{fs}")
  else
    Snd.raise(:no_such_sound, s, "need a sound index or a file name")
  end
end

trace_var(:$clm_default_frequency) do |val| set_clm_default_frequency(val) end
trace_var(:$clm_table_size)        do |val| set_clm_table_size(val) end

with_silence do
  # warning: undefined variable
  $clm_version            = "ruby 23-Dec-2012"
  $output                 ||= false
  $reverb                 ||= false
  $clm_array_print_length ||= 8
  $clm_audio_format       ||= Mus_lshort
  $clm_clipped            ||= true
  $clm_comment            ||= nil
  $clm_decay_time         ||= 1.0
  $clm_delete_reverb      ||= false
  $clm_file_buffer_size   ||= 65536
  $clm_file_name          ||= "test.snd"
  $clm_info               ||= false
  $clm_notehook           ||= nil
  $clm_play               ||= 0
  $clm_player             ||= :clm_player
  $clm_reverb             ||= nil
  $clm_reverb_channels    ||= 1
  $clm_reverb_data        ||= []
  $clm_reverb_file_name   ||= nil
  $clm_statistics         ||= false
  $clm_table_size         ||= 512
  $clm_verbose            ||= false
  $clm_default_frequency  ||= 0.0
  $clm_search_list        ||= (ENV["CLM_SEARCH_PATH"] or ".").split(/:/)

  if provided? :snd
    $clm_channels      ||= default_output_chans
    $clm_data_format   ||= default_output_data_format
    $clm_header_type   ||= default_output_header_type
    $clm_locsig_type   ||= locsig_type
    $clm_output_device ||= audio_output_device
    $clm_rt_bufsize    ||= dac_size
    $clm_srate         ||= default_output_srate
  else
    $clm_channels      ||= 1
    $clm_data_format   ||= Mus_lfloat
    $clm_header_type   ||= Mus_next
    $clm_locsig_type   ||= Mus_interp_linear
    $clm_output_device ||= 0
    $clm_rt_bufsize    ||= 512
    $clm_srate         ||= 44100
  end
end

module WS
  ws_doc = "\
 with_sound(*args) do ... end
 
   :output             $clm_file_name        test.snd
   :channels           $clm_channels         1
   :srate              $clm_srate            44100
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
# special with_dac options:
   :bufsize            $clm_rt_bufsize       512
   :device             $clm_output_device    0
 
 Usage: with_sound(:play, 1, :statistics, true) do fm_violin end"

  with_silence do
    unless defined? Etc.getlogin
      require "etc"
    end
    unless defined? Socket.gethostname
      require "socket"
    end
  end

  def ws_getlogin
    if defined? Etc.getlogin
      Etc.getlogin
    else
      ENV["USER"] or "xen"
    end
  end

  def ws_gethostname
    if defined? Socket.gethostname
      Socket.gethostname
    else
      ENV["HOST"] or "localhost"
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
           "with_reverb(reverb, reverb_amount=0.05, snd=false, *with_sound_args)
with_reverb(:jc_reverb)
require 'clm-ins'
with_reverb(:jl_reverb, 0.2)")
  def with_reverb(reverb, reverb_amount = 0.05, snd = false, *args)
    ws = With_Snd.new(:reverb, reverb,
                      :output, file_name(Snd.snd(snd)),
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

  add_help(:with_full_sound, ws_doc)
  def with_full_sound(*args, &body)
    ws = with_sound(:clm, false, *args, &body)
    set_x_bounds([0, frames($snd_opened_sound) / srate($snd_opened_sound).to_f],
                 $snd_opened_sound)
    ws
  end

  add_help(:with_temp_sound, ws_doc)
  def with_temp_sound(*args, &body)
    old_output = $clm_file_name
    $clm_file_name = tempnam()
    ws = with_sound(:clm, true, *args, &body)
    $clm_file_name = old_output
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

  add_help(:with_temp_snd,
           "with_temp_snd(snd = false) do |temp_snd_file_name| ... end \
Saves SND in a temporary file, which name can be accessed in the body code.  \
After finishing the body, the file will be removed.")
  def with_temp_snd(snd = false, &body)
    t = tempnam()
    save_sound_as(t, snd)
    ret = body.call(t)
    remove_file(t)
    ret
  end

  if defined? snd_tempnam
    alias tempnam snd_tempnam
  else
    $file_number = 0
    def tempnam
      dir = (ENV.map do |k, v| v if /TMP/ =~ k end.compact.first or "/tmp")
      format("%s/snd_%d_%d.snd", dir, $$, $file_number += 1)
    end
  end

  def make_default_comment
    format("# Written %s by %s at %s using clm (%s)",
           Time.new.localtime.strftime("%a %d-%b-%y %H:%M %Z"),
           ws_getlogin,
           ws_gethostname,
           $clm_version)
  end
  
  def remove_file(file)
    if provided?(:snd) and sound?(snd = find_sound(file))
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

  def initialize(*args, &body)
    @output          = get_args(args, :output,            $clm_file_name)
    @channels        = get_args(args, :channels,          $clm_channels)
    @srate           = get_args(args, :srate,             $clm_srate)
    @header_type     = get_args(args, :header_type,       $clm_header_type)
    @data_format     = get_args(args, :data_format,       $clm_data_format)
    @audio_format    = get_args(args, :audio_format,      $clm_audio_format)
    @out_buffer      = get_args(args, :out_buffer,        false)

    @reverb          = get_args(args, :reverb,            $clm_reverb)
    @reverb_data     = get_args(args, :reverb_data,       $clm_reverb_data)
    @reverb_channels = get_args(args, :reverb_channels,   $clm_reverb_channels)
    @revfile         = get_args(args, :reverb_file_name,  nil)
    @delete_reverb   = get_args(args, :delete_reverb,     $clm_delete_reverb)
    @rev_buffer      = get_args(args, :rev_buffer,        false)

    @decay_time      = get_args(args, :decay_time,        $clm_decay_time)
    @scaled_to       = get_args(args, :scaled_to,         false)
    @scaled_by       = get_args(args, :scaled_by,         false)

    @continue        = get_args(args, :continue_old_file, false)
    @notehook        = get_args(args, :notehook,          $clm_notehook)
    @save_body       = get_args(args, :save_body,         false)
    @player          = get_args(args, :player,            $clm_player)

    @play            = get_args(args, :play,              $clm_play)
    @statistics      = get_args(args, :statistics,        $clm_statistics)
    @verbose         = get_args(args, :verbose,           $clm_verbose)
    @info            = get_args(args, :info,              $clm_info)
    @comment         = get_args(args, :comment,           $clm_comment)
    @locsig_type     = get_args(args, :locsig_type,       $clm_locsig_type)
    @clipped         = get_args(args, :clipped,           :undefined)
    @offset          = get_args(args, :offset,            0.0)

    @rtime = @utime = @stime = 0.0
    @stat_frames      = 0
    @stat_data_format = false
    @stat_header_type = false
    @stat_comment     = nil
    @stat_maxamp      = nil
    @stat_revamp      = nil
    @body = body
    if @reverb and @revfile.null?
      @revfile = make_reverb_file_name
    end
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
    $output = @ws_output = false
    $reverb = @ws_reverb = false
    @out_snd = @rev_snd = false
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
    case @notehook
    when Proc
      @with_sound_note_hook.add_hook!("wsnh") do |name, start, dur|
        @notehook.call(name, start, dur)
      end
    when Symbol, String
      @with_sound_note_hook.add_hook!("wsnh") do |name, start, dur|
        snd_func(@notehook, name, start, dur)
      end
    end
    set_help
  end
  attr_reader :output, :out_snd, :with_sound_note_hook
  alias help description
  
  def to_s
    if @reverb
      s = format(", reverb: %s, reverb-channels: %d", @reverb, @reverb_channels)
    else
      s = ""
    end
    format("#<%s: output: %s, channels: %d, srate: %d%s>",
           self.class, @output.inspect, @channels, @srate.to_i, s)
  end
  alias inspect to_s

  def describe
    show_local_variables
  end
  
  def with_sound(*args, &body)
    com = format("%s#%s: temporary sound, args %s",
                 self.class, get_func_name, args.inspect)
    ws = self.class.new(:output, get_args(args, :output, @output),
                        :comment, get_args(args, :comment, com),
                        :play, get_args(args, :play, false),
                        :statistics, get_args(args, :statistics, false),
                        :reverb, get_args(args, :reverb, false),
                        :reverb_data, get_args(args,
                                               :reverb_data, @reverb_data),
                        :reverb_file_name, get_args(args,
                                                    :reverb_file_name,
                                                    @revfile),
                        :reverb_channels, get_args(args,
                                                   :reverb_channels,
                                                   @reverb_channels),
                        :delete_reverb, get_args(args,
                                                 :delete_reverb,
                                                 @delete_reverb),
                        :decay_time, get_args(args, :decay_time, @decay_time),
                        :continue_old_file, get_args(args,
                                                     :continue_old_file,
                                                     @continue),
                        :out_buffer, get_args(args, :out_buffer, @out_buffer),
                        :rev_buffer, get_args(args, :rev_buffer, @rev_buffer),
                        :scaled_to, get_args(args, :scaled_to, @scaled_to),
                        :scaled_by, get_args(args, :scaled_by, @scaled_by),
                        :notehook, get_args(args, :notehook, @notehook),
                        :save_body, get_args(args, :save_body, @save_body),
                        :channels, get_args(args, :channels, @channels),
                        :srate, get_args(args, :srate, @srate),
                        :header_type, get_args(args,
                                               :header_type, @header_type),
                        :data_format, get_args(args,
                                               :data_format, @data_format),
                        :audio_format, get_args(args,
                                                :audio_format, @audio_format),
                        :verbose, get_args(args, :verbose, @verbose),
                        :info, get_args(args, :info, @info),
                        :clipped, get_args(args, :clipped, @clipped),
                        :offset, get_args(args, :offset, 0.0),
                        &body)
    ws.run
    ws
  end

  def with_current_sound(*args, &body)
    output, offset, scaled_to, scaled_by = nil
    optkey(args, binding,
           [:output,    tempnam()],
           [:offset,    0.0],
           [:scaled_to, false],
           [:scaled_by, false])
    ws = with_sound(:output,    output,
                    :scaled_to, scaled_to,
                    :scaled_by, scaled_by,
                    :offset,    offset,
                    *args, &body)
    clm_mix(ws.output)
    remove_file(ws.output)
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
    start = if number?(args[-1])
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
      if @verbose 
        Snd.message("mix remake %s at %1.3f", out_file, start)
      end
      comm  = format("# written %s (Snd: %s)\n", Time.now, snd_version)
      comm += format("[%s, %s]\n", args.to_s.inspect, body_str.inspect)
      self.with_sound(:output, out_file, :comment, comm, *args) do
        eval(File.open(rbm_file).read,
             nil, format("(with_mix_load %s)", rbm_file), 1)
      end
    else
      if @verbose
        Snd.message("mix %s at %1.3f", out_file, start)
      end
    end
    clm_mix(out_file, :output_frame, seconds2samples(start))
  end

  def with_sound_info(name, start, dur, environ = binding)
    @clm_instruments.store(environ, [name, start, dur])
    if @info
      Snd.message("%s: start %1.3f, dur %1.3f", name, start, dur)
    end
    if @notehook
      @with_sound_note_hook.call(name, start, dur)
    end
  end
  
  def run_instrument(start, dur, *locsig_args, &environ)
    @start_frame = seconds2samples(start + @offset)
    with_sound_info(get_func_name(3), start, dur, environ)
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
    dur = samples2seconds(@ws_reverb.length)
    if @clm then dur += @decay_time end
    with_sound_info("reverb " + name, 0, dur, environ)
    if @verbose
      Snd.message("%s on %d in and %d out channels",
                  name, @reverb_channels, @channels)
    end
    unless chan.kind_of?(Symbol)
      unless chan.between?(0, @reverb_channels - 1)
        Snd.raise(:out_of_range, chan, @reverb_channels,
                  "reverb channel is out of range")
      end
    end
  end
  
  def run(&body)
    @body ||= body
    set_mus_file_buffer_size($clm_file_buffer_size)
    set_mus_array_print_length($clm_array_print_length)
    if @clipped == :undefined
      if (@scaled_by or @scaled_to) and
        [Mus_bfloat, Mus_lfloat,
          Mus_bdouble, Mus_ldouble].member?(@data_format)
        set_mus_clipping(false)
      else
        set_mus_clipping($clm_clipped)
      end
    else
      set_mus_clipping(@clipped)
    end
    if defined? set_auto_update_interval
      set_auto_update_interval(0.0)
    end
    before_output
    frm1 = ws_frame_location
    init_process_time
    run_body
    after_output
    stop_process_time
    old_sync = false
    if provided? :snd
      if sound?(snd = find_sound(@output))
        old_sync = sync(snd)
        @out_snd = update_sound(snd)
      else
        if @header_type == Mus_raw
          @out_snd = open_raw_sound(@output, @channels,
                                    @srate.to_i, @data_format)
        else
          @out_snd = open_sound(@output)
        end
      end
      set_sync(true, @out_snd)
    end
    set_statistics
    frm2 = ws_frame_location
    if @scaled_to then scaled_to_sound(frm1, frm2 - frm1) end
    if @scaled_by then scaled_by_sound(frm1, frm2 - frm1) end
    if provided? :snd
      if sound?(snd = find_sound(@output))
        if old_sync
          set_sync(old_sync, snd)
        end
        update_time_graph(snd)
      end
    end
    finish_sound
    if @statistics then statistics end
    1.upto(@play) do play_it end
  end

  protected
  # INFO [ms]
  # Mon Nov 15 14:46:13 CET 2010
  #
  # previous (1.?.? ... 1.9.1)
  #   instance_eval do | | body end
  #
  # current (1.9.2 since July 2010)
  #   instance_eval do |self| body end
  #
  #   if BODY: lambda do | | ... end
  #     ArgumentError: wrong number of arguments (1 for 0)
  #   if BODY: Proc.new do | | ... end
  #     okay (Proc ignores arity)
  #
  # tomorrow?
  def run_body
    instance_eval(&@body)
  rescue Interrupt, ScriptError, NameError, StandardError
    finish_sound
    show_local_variables
    case $!
    when Interrupt, Break
      # C-g, ws_break
      Snd.message("with_sound body: %s", $!.message)
    else
      raise $!
    end
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
    show_local_variables
    case $!
    when Interrupt, Break
      # C-g, ws_break
      Snd.message("with_sound body (reverb): %s", $!.message)
    else
      raise $!
    end
  end
  
  def show_local_variables
    Snd.message()
    # run_instrument|reverb-proc => [instrument-name, start, dur]
    # sorted by values[start]
    @clm_instruments.sort do |a, b|
      a[1][1] <=> b[1][1]
    end.each do |proc, vals|
      Snd.message("=== %s [%1.3f-%1.3f] ===", *vals)
      each_variables(binding?(proc) ? proc : proc.binding) do |var, val|
        Snd.message("%s = %s", var, val)
      end
      Snd.message()
    end
    @clm_instruments.values
  end

  def make_reverb_file_name
    path = File.split(@output).first
    file = File.basename(@output, ".*") + ".reverb"
    unless path == "." then file = path + "/" + file end
    file
  end

  def init_process_time
    tms = process_times
    @rtime = Time.now
    @utime = tms.utime
    @stime = tms.stime
  end
  
  def stop_process_time
    tms = process_times
    @rtime = Time.now - @rtime
    @utime = tms.utime - @utime
    @stime = tms.stime - @stime
  end
  
  def before_output
    set_mus_srate(@srate)
  end
  
  def ws_frame_location
  end
  
  def scaled_to_sound(from, to)
  end

  def scaled_by_sound(from, to)
  end
  
  def statistics
    obj_name = case @ws_output
               when Mus, Vct, SoundData
                 " (" + @ws_output.name + ")"
               else
                 ""
               end
    Snd.message("filename: %s%s", @output.inspect, obj_name)
    Snd.message("   chans: %d, srate: %d", @channels, @srate.to_i)
    if @stat_data_format and @stat_header_type
      Snd.message("  format: %s [%s]",
                  mus_data_format_name(@stat_data_format),
                  mus_header_type_name(@stat_header_type))
    end
    if @stat_frames > 0
      Snd.message("  length: %1.3f (%d frames)",
                  @stat_frames / @srate.to_f, @stat_frames)
    end
    Snd.message("    real: %1.3f  (utime %1.3f, stime %1.3f)",
                @rtime, @utime, @stime)
    if @stat_frames > 1
      rt = (@srate.to_f / @stat_frames)
      Snd.message("   ratio: %1.2f  (uratio %1.2f)",
                  @rtime * rt, @utime * rt)
    end
    ws_maxamp_statistics
    unless (comm = @stat_comment).null?
      Snd.message(" comment: %s", comm)
    end
  end
  
  def finish_sound
    if defined? set_auto_update_interval
      set_auto_update_interval(@old_update_interval)
    end
    @reverb and @delete_reverb and (not @continue) and remove_file(@revfile)
    set_mus_srate(@old_srate)
    @stat_frames = ws_frame_location
  end

  def play_it
    if provided? :snd
      # Inside Snd we use a Proc or Method of one arg, a sound INDEX.
      case @player
      when Proc
        @player.call(@out_snd)
      when Method
        snd_func(@player, @out_snd)
      else
        play(@out_snd, :wait, true)
      end
    else
      # Outside Snd we use a Proc or Method of one arg, a sound FILE NAME.
      case @player
      when Proc
        @player.call(@output)
      when Method
        snd_func(@player, @output)
      else
        system("sndplay #{@output}")
      end
    end
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
  def ws_simp(start = 0, dur = 1, freq = 440, amp = 0.5, amp_env = [0, 1, 1, 1])
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
  def violin(start = 0, dur = 1, freq = 440, amp = 0.5, fm_index = 1,
             amp_env = [0, 1, 1, 1])
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
    indf1 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0],
                     :scaler, index1, :duration, dur)
    indf2 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0],
                     :scaler, index2, :duration, dur)
    indf3 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0],
                     :scaler, index3, :duration, dur)
    pervib = make_triangle_wave(:frequency, 0.5, :amplitude, 0.0025 *  frq_scl)
    ranvib = make_rand_interp(:frequency, 16.0, :amplitude, 0.005 * frq_scl)
    run_instrument(start, dur) do
      vib = triangle_wave(pervib) + rand_interp(ranvib)
      env(ampf) *
        oscil(carrier,
              vib +
              env(indf1) * oscil(fmosc1, vib) +
              env(indf2) * oscil(fmosc2, 3.0 * vib) +
              env(indf3) * oscil(fmosc3, 4.0 * vib))
    end
  end
end

# WSChannel2Vct and WSSampler are helper classes used by class
# Snd_Instrument.  sampler has no possibility to set location
# (and direction) which is needed by instrument grani (clm-ins.rb).
if provided? :snd
  class WSChannel2Vct
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
      format("%s.new(%s, %s, %s, %s)",
             self.class, @snd.inspect, @chn.inspect, @start, @direction)
    end
    
    def to_s
      format("#<%s snd: %s, chn: %s, location: %d, direction: %d, vct: %s>",
             self.class, @snd.inspect, @chn.inspect,
             location, @direction, @vct.inspect)
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
end

class WSSampler
  # (make-sampler (start-samp 0) (snd #f) (chn #f) (dir 1) (edpos #f))
  def initialize(snd = false, chn = false, start = 0, dir = 1)
    @reader = make_sampler(start, snd, chn, dir, false)
    @direction = dir
    @start = start
    @direction = dir
    @snd = snd
    @chn = chn
  end
  attr_accessor :direction
  
  def inspect
    format("%s.new(%s, %s, %s, %s)",
           self.class, @snd.inspect, @chn.inspect, @start, @direction)
  end
  
  def to_s
    @reader.inspect
  end

  def next
    @reader.call
  end

  def close
    free_sampler(@reader)
    if sound?(@snd)
      close_sound_extend(@snd)
    end
  end
  
  def location
    sampler_position(@reader)
  end

  def location=(val)
    # sampler_position isn't settable
  end
end

class Snd_Instrument < Instrument
  # place holder for special Snd instruments, see FULLMIX in
  # clm-ins.rb.

  def get_snd(file)
    snd = if integer?(file)
            integer2sound(file)
          elsif string?(file)
            if sound?(s = find_sound(file))
              s
            else
              open_sound(file)
            end
          end
    if sound?(snd)
      snd
    else
      Snd.snd(snd)
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
      WSChannel2Vct.new(get_snd(file), channel, start, direction)
    else
      WSSampler.new(get_snd(file), channel, start, direction)
    end
  end

  # (read-sample reader)
  def ws_readin(rd)
    rd.next
  end

  def close_ws_reader(rd)
    rd.close
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
    make_readin(:file, file, :channel, channel,
                :start, start, :direction, direction)
  end

  # (readin gen)
  def ws_readin(rd)
    readin(rd)
  end

  def close_ws_reader(rd)
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
  def initialize(*args)
    @clm = false
    super
  end

  def run_instrument(start, dur, *locsig_args)
    super
    degree, distance, reverb_amount = nil
    optkey(locsig_args, binding,
           [:degree, random(90.0)],
           [:distance, 1.0],
           [:reverb_amount, 0.05])
    @ws_output = if vct?(@out_buffer) or sound_data?(@out_buffer)
                   @out_buffer.fill(0.0)
                   @out_buffer
                 else
                   len = seconds2samples(dur)
                   if @channels == 1
                     Vct.new(len, 0.0)
                   else
                     SoundData.new(@channels, len)
                   end
                 end
    @ws_reverb = if @reverb
                   if vct?(@rev_buffer) or sound_data?(@rev_buffer)
                     @rev_buffer.fill(0.0)
                     @rev_buffer
                   else
                     len = seconds2samples(dur + @decay_time)
                     if @reverb_channels == 1
                       Vct.new(len, 0.0)
                     else
                       SoundData.new(@reverb_channels, len)
                     end
                   end
                 else
                   false
                 end
    @locsig = make_locsig(:degree,   degree,
                          :distance, distance,
                          :reverb,   reverb_amount,
                          :output,   @ws_output,
                          :revout,   @ws_reverb,
                          :channels, @channels,
                          :type,     @locsig_type)
    @ws_output.length.times do |samp|
      locsig(@locsig, samp, yield(@start_frame + samp))
    end
    @channels.times do |chn|
      mix_vct(@ws_output.to_vct(chn), @start_frame, @out_snd, chn, false)
    end
    if @reverb
      @reverb_channels.times do |chn|
        mix_vct(@ws_reverb.to_vct(chn), @start_frame, @rev_snd, chn, false)
      end
    end
  end

  def run_reverb(chan = 0)
    super
    rev_out = SoundData.new(@reverb_channels, @ws_reverb.length)
    case chan
    when Integer
      case @ws_reverb
      when Vct
        rev_out.length.times do |i|
          sound_data_frame_set!(rev_out, i, body.call(@ws_reverb[i], i))
        end
      when SoundData
        rev_out.length.times do |i|
          sound_data_frame_set!(rev_out, i, body.call(@ws_reverb[chan, i], i))
        end
      end
    when :frames
      case @ws_reverb
      when Vct
        frm = make_frame(1)
        rev_out.length.times do |i|
          frame_set!(frm, 0, @ws_reverb[i])
          sound_data_frame_set!(rev_out, i, body.call(frm, i))
        end
      when SoundData
        frm = make_frame(@reverb_channels)
        rev_out.length.times do |i|
          @ws_reverb.chans.times do |chn|
            frame_set!(frm, chn, @ws_reverb[chn, i])
          end
          sound_data_frame_set!(rev_out, i, body.call(frm, i))
        end
      end
    end
    if @channels == @reverb_channels
      @channels.times do |chn|
        mix_vct(rev_out.to_vct(chn), 0, @out_snd, chn, false)
      end
    else
      v = rev_out.to_vct
      @channels.times do |chn|
        mix_vct(v, 0, @out_snd, chn, false)
      end
    end
    rev_out = false
    @ws_reverb = false
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
    unless sound?(snd = find_sound(filename))
      unless snd = open_sound(filename)
        Snd.raise(:no_such_file, filename, "file name required")
      end
    end
    [channels(snd), @channels].min.times do |chn|
      if scale.nonzero?
        scale_channel(scale, input_frame, frames, snd, chn)
      end
      mix_vct(channel2vct(input_frame, frames, snd, chn),
              output_frame, snd, chn, false)
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
    if sound?(rsnd = find_sound(@revfile))
      close_sound_extend(rsnd)
      remove_file(@revfile)
      rsnd = new_sound(@revfile, @header_type,
                       @data_format, @srate.to_i, @reverb_channels)
    else
      rsnd = new_sound(@revfile, @header_type,
                       @data_format, @srate.to_i, @reverb_channels)
    end
    @out_snd = snd
    @rev_snd = rsnd
    @ws_reverb = SoundData.new(@reverb_channels, len)
    init_process_time
    @reverb_channels.times do |chn|
      channel2vct(0, len, @out_snd,
                  chn).scale(reverb_amount).to_sound_data(@ws_reverb, chn)
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
    sr = mus_srate.to_i
    if sound?(snd = find_sound(@output))
      if @continue
        @srate = set_mus_srate(srate(snd))
      else
        set_header_type(snd, @header_type)
        set_data_format(snd, @data_format)
        set_srate(snd, sr)
        set_channels(snd, @channels)
        set_comment(snd, @comment)
        channels(snd).times do |chn| set_frames(1, snd, chn) end
        snd = update_sound(snd)
      end
    else
      unless @continue then remove_file(@output) end
      snd = new_sound(@output, @header_type, @data_format,
                      sr, @channels, @comment)
    end
    if @reverb
      if sound?(rsnd = find_sound(@revfile)) and (not @continue)
        set_header_type(@header_type, rsnd)
        set_data_format(@data_format, rsnd)
        set_srate(sr, rsnd)
        set_channels(@reverb_channels, rsnd)
        set_frames(1, rsnd)
        update_sound(rsnd)
      else
        unless @continue
          remove_file(@revfile)
        end
        rsnd = new_sound(@revfile, @header_type, @data_format,
                         sr, @reverb_channels)
      end
    end
    $output = @out_snd = snd
    $reverb = @rev_snd = rsnd
  end

  def after_output
    @reverb and run_reverb_body
  end
  
  def ws_frame_location
    frames(@out_snd)
  end
  
  def scaled_to_sound(beg, len)
    scl = @scaled_to / maxamp(@out_snd, true).max
    @channels.times do |chn|
      scale_channel(scl, beg, len, @out_snd, chn)
    end
    save_sound(@out_snd)
  end

  def scaled_by_sound(beg, len)
    @channels.times do |chn|
      scale_channel(@scaled_by, beg, len, @out_snd, chn)
    end
    save_sound(@out_snd)
  end
  
  def set_statistics
    @stat_frames      = frames(@out_snd)
    @stat_data_format = data_format(@out_snd)
    @stat_header_type = header_type(@out_snd)
    @stat_comment     = comment(@out_snd)
    @stat_maxamp      = maxamp(@out_snd, true)
    if @reverb
      @stat_revamp    = maxamp(@rev_snd, true)
    end
  end
  
  def ws_maxamp_statistics
    Snd.message(" max out: %s%s",
                @stat_maxamp.to_string,
                (@scaled_to or @scaled_by) ? " (before scaling)" : "")
    if @reverb
      Snd.message(" max rev: %s", @stat_revamp.to_string)
    end
  end

  # with_closed_sound(snd) do |snd_name| ... end
  # returns new snd index
  # see clm-ins.rb, run_fullmix
  def with_closed_sound(snd, &body)
    snd_name = file_name(snd)
    update_sound(snd)
    close_sound_extend(snd)
    body.call(snd_name)
    open_sound(snd_name)
  end
end

class With_CLM < CLM_Instrument
  def initialize(*args)
    @clm = true
    super
  end

  def run_instrument(start, dur, *locsig_args)
    super
    degree, distance, reverb_amount = nil
    optkey(locsig_args, binding,
           [:degree, random(90.0)],
           [:distance, 1.0],
           [:reverb_amount, 0.05])
    @locsig = make_locsig(:degree,   degree,
                          :distance, distance,
                          :reverb,   reverb_amount,
                          :output,   @ws_output,
                          :revout,   @ws_reverb,
                          :channels, @channels,
                          :type,     @locsig_type)
    @start_frame.upto((@start_frame + seconds2samples(dur)) - 1) do |samp|
      locsig(@locsig, samp, yield(samp))
    end
  end

  def run_reverb(chan = 0)
    super
    case chan
    when Integer
      (@ws_reverb.length + seconds2samples(@decay_time)).times do |samp|
        frame2file(@ws_output, samp,
                   yield(file2sample(@ws_reverb, samp, chan), samp))
      end
    when :frames
      frm = make_frame(@reverb_channels)
      (@ws_reverb.length + seconds2samples(@decay_time)).times do |samp|
        frame2file(@ws_output, samp,
                   yield(file2frame(@ws_reverb, samp, frm), samp))
      end
    end
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
    mx = make_mixer(@channels, *(0...@channels * @channels).map do scale end)
    mus_mix(@output, filename, output_frame, frames, input_frame, mx)
  end
  
  protected
  def before_output
    super
    if @continue
      @ws_output = continue_sample2file(@output)
      @srate = set_mus_srate(mus_sound_srate(@output))
      if @reverb
        @ws_reverb = continue_sample2file(@revfile)
      end
      if provided?(:snd) and sound?(snd = find_sound(@output))
        close_sound_extend(snd)
      end
    else
      remove_file(@output)
      @ws_output = make_sample2file(@output, @channels,
                                    @data_format, @header_type, @comment)
      if @reverb
        remove_file(@revfile)
        @ws_reverb = make_sample2file(@revfile, @reverb_channels,
                                      @data_format, @header_type)
      end
    end
    $output = @ws_output
    $reverb = @ws_reverb
  end

  def after_output
    if @reverb
      mus_output?(@ws_reverb) and mus_close(@ws_reverb)
      old_reverb = @ws_reverb
      # non-RUN_REVERB...END functions need it here
      $reverb = @ws_reverb = make_file2sample(@revfile)
      run_reverb_body
      mus_input?(@ws_reverb) and mus_close(@ws_reverb)
      $reverb = @ws_reverb = old_reverb
    end
    mus_output?(@ws_output) and mus_close(@ws_output)
  end

  def ws_frame_location
    with_closed_output do
      mus_sound_frames(@output)
    end
  end

  def scale_it(beg, len, scale)
    mx = make_mixer(@channels, *(0...@channels * @channels).map do scale end)
    mus_mix(@output, @output, beg, len, beg, mx, false)
  end
  private :scale_it
  
  def scaled_to_sound(beg, len)
    if provided? :snd
      if sound?(@out_snd = find_sound(@output))
        @channels.times do |chn|
          scale_to(@scaled_to, @out_snd, chn)
        end
        save_sound(@out_snd)
      end
    else
      omax = mus_sound_maxamp(@output)
      mx = 0.0
      1.step(omax.length - 1, 2) do |i|
        mx = [omax[i].abs, mx].max
      end
      if mx.zero? then mx = 1.0 end
      scale_it(beg, len, @scaled_to / mx)
    end
  end

  def scaled_by_sound(beg, len)
    if provided? :snd
      if sound?(@out_snd = find_sound(@output))
        @channels.times do |chn|
          scale_channel(@scaled_by, beg, len, @out_snd, chn)
        end
        save_sound(@out_snd)
      end
    else
      scale_it(beg, len, @scaled_by)
    end
  end

  def set_statistics
    @stat_frames      = mus_sound_frames(@output)
    @stat_data_format = mus_sound_data_format(@output)
    @stat_header_type = mus_sound_header_type(@output)
    @stat_comment     = mus_sound_comment(@output)
    @stat_maxamp      = mus_sound_maxamp(@output)
    if @reverb
      @stat_revamp    = mus_sound_maxamp(@revfile)
    end
  end
  
  def ws_maxamp_statistics
    sr = @srate.to_f
    ch = "@"
    @stat_maxamp.each_pair do |s, v|
      Snd.message("maxamp %s: %1.3f (near %1.3f secs)%s",
                  ch.next!, v, s / sr,
                  (@scaled_to or @scaled_by) ? " (before scaling)" : "")
    end
    if @reverb
      ch = "@"
      @stat_revamp.each_pair do |s, v|
        Snd.message("revamp %s: %1.3f (near %1.3f secs)", ch.next!, v, s / sr)
      end
    end
  end

  def with_closed_output
    mus_output?(@ws_output) and mus_close(@ws_output)
    ret = yield
    $output = @ws_output = continue_sample2file(@output)
    ret
  end
end

class With_DAC < Snd_Instrument
  # no reverb
  # handles instruments parallel if computing is fast enough
  def initialize(*args)
    @clm = false
    super
    @bufsize = get_args(args, :bufsize, $clm_rt_bufsize)
    @device  = get_args(args, :device, $clm_output_device)
    @ws_reverb = @ws_output = @reverb = false
    @output = "dac"
    # @instruments = [[first_samp, last_samp, body], ...]
    @instruments = []
    @current_sample = 0
  end

  def run_instrument(start, dur, *args, &body)
    super
    degree, distance, reverb_amount = nil
    optkey(locsig_args, binding,
           [:degree, random(90.0)],
           [:distance, 1.0],
           [:reverb_amount, 0.05])
    @locsig = make_locsig(:degree,   degree,
                          :distance, distance,
                          :reverb,   reverb_amount,
                          :output,   @ws_output,
                          :revout,   @ws_reverb,
                          :channels, @channels,
                          :type,     @locsig_type)
    beg, ends = times2samples(start, dur)
    @instruments.push([beg, ends, body])
    real_run(beg)
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
          dac_vct.add!(Vct.new(@bufsize) do |i|
            body.call(@current_sample + i)
          end)
        elsif @current_sample >= ends
          idxs.push(idx)
        end
      end
      idxs.each do |idx|
        @instruments.delete_at(idx)
      end
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
    @ws_output = mus_audio_open_output(@device, @srate.round,
                                       @channels, @audio_format,
                                       @bufsize * @channels * 2)
    if @ws_output < 0
      Snd.raise(:mus_error, @ws_output, "can't open DAC")
    end
  end
  
  def ws_maxamp_statistics
  end
  
  def after_output
    # flush contents of instrument array
    real_run(@current_sample + 1) until @instruments.empty?
  end

  def finish_sound
    if defined? set_auto_update_interval
      set_auto_update_interval(@old_update_interval)
    end
    number?(@ws_output) and mus_audio_close(@ws_output)
    set_mus_srate(@old_srate)
    @ws_output = false
  end
end

include WS

# ws.rb ends here
