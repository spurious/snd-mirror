# dlocsig.rb -- CLM -> Snd/Ruby translation of dlocsig.lisp

# Copyright (C) 2003 Michael Scholz

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Mar 25 23:21:37 CET 2003
# Last: Wed Apr 09 05:39:46 CEST 2003
# Version: $Revision: 1.10 $

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

# Original Copyright of Fernando Lopez Lezcano:

# ;;; Copyright (c) 92, 93, 94, 98, 99, 2000, 2001 Fernando Lopez Lezcano. 
# ;;; All rights reserved.
# ;;; Use and copying of this software and preparation of derivative works
# ;;; based upon this software are permitted and may be copied as long as 
# ;;; no fees or compensation are charged for use, copying, or accessing
# ;;; this software and all copies of this software include this copyright
# ;;; notice. Suggestions, comments and bug reports are welcome. Please 
# ;;; address email to: nando@ccrma.stanford.edu
# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

# ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# ;;; Dynamic multichannel three-dimentional signal locator
# ;;; (wow that sound good! :-)
# ;;;
# ;;; by Fernando Lopez Lezcano
# ;;;    CCRMA, Stanford University
# ;;;    nando@ccrma.stanford.edu
# ;;;
# ;;; Thanks to Juan Pampin for help in the initial coding of the new version
# ;;; and for prodding me to finish it. To Joseph L. Anderson and Marcelo Perticone
# ;;; for insights into the Ambisonics coding and decoding process. 
# ;;; http://www.york.ac.uk/inst/mustech/3d_audio/ambison.htm for more details...

# Comments:

# The code is a translation of the Lisp code of Fernando Lopez Lezcano
# found in clm-2/dlocsig of the CLM distribution.  An extensive
# documentation of the purpose and usage of it can be found in
# clm-2/dlocsig/dlocsig.html.  The Ruby code is tested with current
# CVS-Ruby 1.8 and Snd 6.8.  In addition there is a simple
# implementation of a Snd-menu to set the points of the path via
# sliders.  The menu class needs the Motif module (xm.so) or compiling
# --with-static-xm, the Dlocsig-stuff works without Motif.
#
# The simple example
#
# [[-10, 10], [0, 5], [10, 10]].to_path.snd_plot
#
# draws the trajectory in the channel panels, and, if the current
# sound has more than one channels, the velocity, the doppler curve
# and, if the sound has four channels, the acceleration too.  If you
# have gnuplot installed, the example
#
# [[-10, 10], [0, 5], [10, 10]].to_path.pplot
#
# draws all four curves in one gnuplot window.  You can find more
# samples at the end of the file.  To install the simple Snd-menu, you
# can make something like this
#
# menu = Snd_menu.new("Dlocsig")
# menu.add_cascade("Dlocsig 2d", :d3, false, :error, 0.001) do |args| Dlocsig_menu.new(*args) end
# menu.add_cascade("Dlocsig 3d", :d3, true, :error, 0.001) do |args| Dlocsig_menu.new(*args) end
# menu.add_simple("Play") do play end

# Important functions:
#
#       arrange_speakers :speakers,            []
#                        :groups,              []
#                        :delays,              []
#                        :distances,           []
#                        :map,                 []
#
#   make_path
#   make_polar_path
#   make_closed_path
#   and to_path take options used by
#
#        Bezier_path.new :path,                nil
#                        :d3,                  true
#                        :polar,               false
#                        :error,               0.01
#                        :curvature,           nil     
#
#   Open_bezier_path.new :path,                nil
#                        :d3,                  true
#                        :polar,               false
#                        :error,               0.01
#                        :curvature,           nil     
#                        :initial_direction,   [0.0, 0.0, 0.0]
#                        :final_direction,     [0.0, 0.0, 0.0]
#
# Closed_bezier_path.new :path,                nil   
#                        :d3,                  true
#                        :polar,               false
#                        :error,               0.01
#                        :curvature,           nil
#
#   make_literal_path
#   make_literal_polar_path take options use by
#
#       Literal_path.new(:path,                nil   
#                        :d3,                  true
#                        :polar,               false
#
#   make_spiral_path takes options used by
#
#        Spiral_path.new :start_angle,         0.0   
#                        :total_angle,         nil
#                        :step_angle,          $dlocsig_one_turn / 100.0
#                        :turns,               nil
#                        :distance,            [0, 10, 1, 10]
#                        :height,              [0, 0, 1, 0]
#                        :velocity,            [0, 1, 1, 1]
#
#           make_dlocsig(startime, dur, *args)
#                        :path,                []
#                        :scaler,              1.0
#                        :direct_power,        1.5
#                        :inside_direct_power, 1.5
#                        :reverb_power,        0.5
#                        :inside_reverb_power, 0.5
#                        :reverb_amount,       0.04
#                        :initial_delay,       nil
#                        :unity_gain_dist,     nil
#                        :inside_radius,       1.0
#                        :minimum_seg_length,  1.0
#                        :render_using,        Amplitude_panning
#                        :out_channels,        nil
#                        :rev_channels,        nil
#
# Sample instruments (sinewave() and move() below) show how to replace
# the usual make_locsig() and locsig() by make_dlocsig() and
# dlocsig().  A replacement of the nrev() reverb function is
# dlocnrev() at the end of the file.

# User variables and constants:

Amplitude_panning = 1
B_format_ambisonics = 2
Decoded_ambisonics = 3

$dlocsig_one_turn = 360.0                 unless defined? $dlocsig_one_turn
$dlocsig_speed_of_sound = 344.0           unless defined? $dlocsig_speed_of_sound

# $dlocsig_one_turn can be changed by one_turn_is(unit),
# angles_in_degree(), angles_in_radians(), and angles_in_turns() while
# $dlocsig_speed_of_sound can be changed by distances_in_meters() or
# distances_in_feet().

# Sample functions at the end of the file:

# sinewave(start, dur, freq, amp, *args)
# move(start, file, *args)
# move_sound(*args) { ... }
# dlocnrev(startime, dur, *args)

# Classes and Modules:

# module Inject
#  inject(n)
#  sum(initial)
#  product(initial)
#
# module CLM
#  error(*args)
#  cis(r)
#  lastx(env)
#  x_norm(env, xmax)
#
# class Array
#  to_pairs
#  to_trias
#  to_path(*args)
#
# module Dlocsig
#  one_turn_is(unit)
#  angles_in_degree
#  angles_in_radians
#  angles_in_turns
#  distances_in_meters
#  distances_in_feet
#  arrange_speakers(*args)
#  set_speaker_configuration(config, configs)
#  get_speaker_configuration(channels, *args)
#  make_dlocsig(startime, dur, *args)
#  dlocsig(sloc, dloc, input)
#  make_path(*args)
#  make_polar_path(*args)
#  make_closed_path(*args)
#  make_literal_path(*args)
#  make_literal_polar_path(*args)
#  make_spiral_path(*args)
#  parse_cartesian_coordinates(points, d3)
#  parse_polar_coordinates(points, d3)
#  distance(x, y, z)
#  nearest_point(x0, y0, z0, x1, y1, z1, px, py, pz)
#  make_a_even
#  make_a_odd
#  a(k, n)
#  ac(k, n)
#  rotation_matrix(x, y, z, angle)
#
# class GnuPlot
#  initialize
#  gnuplot_open
#  gnuplot_close
#  command(*args)
#  reset
#  set_autoscale
#  set_x_range(range)
#  set_y_range(range)
#  set_z_range(range)
#  set_grid
#  set_surface
#  set_parametric
#  set_ticslevel
#  set_title
#  set_label
#  set_margins
#  set_border
#  start_multiplot
#  end_multiplot
#  size(xorigin, yorigin, xsize, ysize)
#  data(data, *args)
#  plot_2d_curve(curve, *args)
#  plot_2d_curves(curves, *args)
#  plot_3d_curve(curve, *args)
#
# class Path
#  initialize(*args)
#  not_rendered
#  not_transformed
#  reset_transformation
#  reset_rendering
#  path_x
#  path_y
#  path_z
#  path_time
#  transform_path(*args)
#  scale_path(scaling)
#  transform_path(translation)
#  rotate_path(rotation, *args)
#  path_trajectory
#  path_2d_trajectory
#  path_velocity
#  path_doppler
#  path_acceleration
#  plot_open
#  plot_close
#  cmd(*args)
#  plot_trajectory(*args)
#  plot_velocity(reset)
#  plot_doppler(reset)
#  plot_acceleration(reset)
#  pplot(normalize)
#  snd_trajectory(*args)
#  snd_velocity(*args)
#  snd_doppler(*args)
#  snd_acceleration(*args)
#  snd_plot
#
# class Bezier_path < Path
#  initialize(*args)
#  set_path(points)
#  set_polar_path(points)
#  set_path_curvature(curvature)
#  set_path_error(error)
#  not_parsed
#  reset_parsing
#  parse_path
#  not_fitted
#  reset_fit
#  fit_path
#  render_path
#
# class Open_bezier_path < Bezier_path
#  initialize(*args)
#  calculate_fit
#  fit_path
#
# class Closed_bezier_path < Bezier_path
#  initialize(*args)
#  calculate_fit
#  fit_path
#
# class Literal_path < Path
#  initialize(*args)
#  render_path
#
# class Spiral_path < Literal_path
#  initialize(*args)
#  render_path
#
# module Snd_menu_utils
#  update_label(lst)
#  string2compound(str)
#  change_label(widget, new_label)
#  make_dialog(*args)
#  add_sliders(dialog, sliders)
#  add_target(*args)
#  add_label(mainform, str)
#  add_text_widget(mainform, value)
#
# class Dlocsig_menu
#  initialize(*args)
#  cascade(*args)
#  set_sensitive(name, flag)
#  toggle_sensitive(name)
#  post_dlocsig_dialog(d3, error)
#
# class Snd_menu
#  initialize(name)
#  add_simple(name, sensitive) { |menu, name| ... }
#  add_cascade(name, *args) { |args| ... }

# Code:

require "English"
$HAVE_MOTIF = $LOADED_FEATURES.detect do |x| x == "xm" end
begin
  require "xm" unless $HAVE_MOTIF
rescue ScriptError
  $HAVE_MOTIF = false
end

require "examp"
require "ws"
require "v"
require "complex"
require "matrix"
include Math

# module Inject, see Thomas, David, Hunt, Andrew: Programming Ruby --
# The Pragmatic Programmer's Guide, 2001 Addison-Wesley, page 102n

module Inject
  def inject(n)
    each do |x| n = yield(n, x) end
    n
  end
  
  def sum(initial = 0)
    inject(initial) do |n, v| n + v end
  end
  
  def product(initial = 1)
    inject(initial) do |n, v| n * v end
  end
end

module CLM
  TWO_PI = 2.0 * PI

  class CLMError < StandardError
  end

  def error(*args)
    if $IN_SND
      snd_error(format(*args))
    else
      raise(CLMError, format(*args), caller(1)[0])
    end
  end

  def cis(r)
    Complex(cos(r), sin(r))
  end

  def lastx(env)
    env[-2]
  end
  
  def x_norm(env, xmax)
    scl = xmax / lastx(env).to_f
    env.map_with_index do |x, i|
      (i.even? ? (x * scl) : x)
    end
  end
end

# used by plotting curves
# to_pairs: [0, 1, 2, 3, 4, 5] --> [[0, 1], [2, 3], [4, 5]]
# to_trias: [0, 1, 2, 3, 4, 5] --> [[0, 1, 2], [3, 4, 5]]
# to_path:  [[-10, 10], [0, 5], [10, 10]].to_path <=> make_path([[-10, 10], [0, 5], [10, 10]])
# uses the same options as make_path()

class Array
  include Inject

  def to_pairs
    die("Array has odd length of %d!", self.length) if self.length.odd?
    ary = Array.new
    0.step(self.length - 2, 2) do |i| ary << [self[i], self[i + 1]] end
    ary
  end

  def to_trias
    if self.length.divmod(3) == 0
      die("Array must be divideable by 3 (%d)!", self.length)
    end
    ary = Array.new
    0.step(self.length - 2, 3) do |i| ary << [self[i], self[i + 1], self[i + 2]] end
    ary
  end

  def to_path(*args)
    make_path(:path, self, *args)
  end
end

#
# Dlocsig functions
#

module Dlocsig
  include CLM

  Group = Struct.new("Group", :id, :size, :vertices, :speakers, :matrix)
  Speaker_config = Struct.new("Speaker_config",
                              :number, :dimension, :coords, :groups, :delays, :map)
  Dlocs = Struct.new("Dlocs", :start, :end, :out_channels, :rev_channels,
                     :out_delays, :out_map, :gains, :rev_gains, :delay, :path, :rev)
  Point707 = cos(TWO_PI / 8.0)
  Path_maxcoeff = 8

  $dlocsig_speaker_configs = []
  $path_ak_even = nil
  $path_ak_odd = nil
  $path_gtab = nil
  $path_ftab = nil

  def one_turn_is(unit)
    $dlocsig_one_turn = unit.to_f
  end

  def angles_in_degree
    one_turn_is(360.0)
  end

  def angles_in_radians
    one_turn_is(TWO_PI)
  end

  def angles_in_turns
    one_turn_is(1.0)
  end

  def distances_in_meters
    $dlocsig_speed_of_sound = 344.0
  end

  def distances_in_feet
    $dlocsig_speed_of_sound = 1128.0
  end
  
  def arrange_speakers(*args)
    speakers  = get_args(args, :speakers, [])
    groups    = get_args(args, :groups, [])
    delays    = get_args(args, :delays, [])
    distances = get_args(args, :distances, [])
    map       = get_args(args, :map, [])
    if speakers.empty?
      error("a speaker configuration must have at least one speaker!")
    end
    unless groups.empty?
      first_len = groups[0].length
      if groups.detect do |group| group.length != first_len end
        error("all groups must be of the same length! (#{first_len})")
      end
    else
      unless speakers[0].kind_of?(Array)
        len = speakers.length
        groups = []
        speakers.each_index do |i|
          j = i + 1
          groups << if len != 1
                      [i, (j < len ? j : 0)]
                    else
                      [0]
                    end
        end
      end
    end
    if groups.empty?
      error("no groups specified, speakers must be arranged in groups")
    end
    if (not delays.empty?) and (not distances.empty?)
      error("please specify delays or distances but not both")
    end
    unless delays.empty?
      if speakers.length > delays.length
        error("all speaker delays have to be specified, only %d supplied [%s]",
              delays.length, delays.inspect)
      elsif speakers.length < delays.length
        error("more speaker delays than speakers, %d supplied instead of %d [%s]",
              delays.length, speakers.length, delays.inspect)
      end
      if x = delays.detect do |delay| delay < 0.0 end
        error("delays must be all positive, #{x} is negative")
      end
    end
    unless distances.empty?
      if speakers.length > distances.length
        error("all speaker distances have to be specified, only %d supplied [%s]",
              distances.length, distances.inspect)
      elsif speakers.length < distances.length
        error("more speaker distances than speakers, %d supplied instead of %d [%s]",
              distances.length, speakers.length, distances.inspect)
      end
      if x = distances.detect do |delay| delay < 0.0 end
        error("distances must be all positive, #{x} is negative")
      end
    end
    unless map.empty?
      if speakers.length > map.length
        error("must map all speakers to output channels, only %d mapped [%s]",
              map.length, map.inspect)
      elsif speakers.length < map.length
        error("trying to map more channels than there are speakers, \
%d supplied instead of %d [%s]",
              map.length, speakers.length, map.inspect)
      end
    end
    coords = speakers.map do |s|
      a = (s.kind_of?(Array) ? s[0] : s)
      e = (s.kind_of?(Array) ? (s[1].zero? ? 0.0 : s[1]) : 0.0)
      evec = cis((e / $dlocsig_one_turn) * TWO_PI)
      dxy = evec.real
      avec = cis((a / $dlocsig_one_turn) * TWO_PI)
      x = (dxy * avec.image)
      y = (dxy * avec.real)
      z = evec.image
      mag = distance(x, y, z)
      [x / mag, y / mag, z / mag]
    end
    min_dist = (distances.empty? ? 0 : distances.min)
    times = Array.new(speakers.length) do
      distance = distances.pop unless distances.empty?
      delay = delays.pop unless delays.empty?
      (delay or (distance ? ((distance - min_dist) / $dlocsig_speed_of_sound) : 0.0))
    end
    groups = groups.map_with_index do |group, id|
      size = group.length
      vertices = group.map do |vertice|
        coords[vertice]
      end
      matrix = case size
               when 3
                 m = Matrix[vertices[0], vertices[1], vertices[2]]
                 (m.regular? ? m.inverse.to_a : nil)
               when 2
                 m = Matrix[vertices[0][0..1], vertices[1][0..1]]
                 (m.regular? ? m.inverse.to_a : nil)
               else
                 nil
               end
      Group.new(id, size, vertices, group, matrix)
    end
    unless map.empty?
      entries = map.length
      if x = map.detect do |entry| entry >= entries end
        error("channel %d in map %s is out of range (max=%d)", x, map.inspect, entries)
      end
      if map.uniq.length != map.length
        error("there are duplicate channels in map %s", map.inspect)
      end
    end
    Speaker_config.new(speakers.length, groups[0][:size], coords, groups, times,
                       Array.new(speakers.length) do |chan|
                         whereto = map.shift if map
                         (whereto or chan)
                       end)
  end

  def set_speaker_configuration(config, configs = $dlocsig_speaker_configs)
    configs[(config[:dimension] < 3 ? 0 : 1)][config[:number]] = config
  end

  def get_speaker_configuration(channels, *args)
    d3      = get_args(args, :d3, false)
    configs = get_args(args, :configs, $dlocsig_speaker_configs)
    configs[(d3 ? 1 : 0)][channels] or error("no speaker configuration exists for %s %d \
output channel%s", (d3 ? "tridimensional" : "bidimensional"), channels, (channels == 1 ? "" : "s"))
  end

  def make_dlocsig(startime, dur, *args)
    path                = get_args(args, :path, [])
    scaler              = get_args(args, :scaler, 1.0)
    direct_power        = get_args(args, :direct_power, 1.5)
    inside_direct_power = get_args(args, :inside_direct_power, 1.5)
    reverb_power        = get_args(args, :reverb_power, 0.5)
    inside_reverb_power = get_args(args, :inside_reverb_power, 0.5)
    reverb_amount       = get_args(args, :reverb_amount, 0.04)
    initial_delay       = get_args(args, :initial_delay, nil)
    unity_gain_dist     = get_args(args, :unity_gain_dist, nil)
    inside_radius       = get_args(args, :inside_radius, 1.0)
    minimum_seg_length  = get_args(args, :minimum_seg_length, 1.0)
    render_using        = get_args(args, :render_using, Amplitude_panning)
    out_channels        = get_args(args, :out_channels, nil)
    rev_channels        = get_args(args, :rev_channels, nil)
    if render_using == B_format_ambisonics and ((out_channels or mus_channels($rbm_output) != 4))
      error("ambisonics b_format requires four output channels, current number is %d",
            (out_channels or mus_channels($rbm_output)))
    end
    out_channels = (out_channels or mus_channels($rbm_output))
    rev_channels = (rev_channels or ($rbm_reverb ? mus_channels($rbm_reverb) : 0))
    speakers = get_speaker_configuration(out_channels)
    channel_gains = Array.new(out_channels) do |i| [] end
    channel_rev_gains = Array.new(rev_channels) do |i| [] end
    max_out_delay = 0.0
    out_delays = Array.new(out_channels)
    xpoints = path.path_x()
    ypoints = path.path_y()
    zpoints = path.path_z()
    tpoints = path.path_time()
    speed_limit = ($dlocsig_speed_of_sound * (tpoints[-1] - tpoints[0])) / dur
    start = 0.0
    delay = []
    real_dur = 0.0
    prev_time = prev_dist = prev_group = prev_x = prev_y = prev_z = false
    first_dist = last_dist = 0
    min_dist = max_dist = min_delay = min_dist_unity = 0.0
    unity_gain = unity_rev_gain = 1.0
    run_beg = run_end = 0.0
    dist2samples = lambda do |d| d * ($rbm_srate / $dlocsig_speed_of_sound) end
    dist2seconds = lambda do |d| d / $dlocsig_speed_of_sound end
    time2samples = lambda do |time| time * $rbm_srate end
    transition_point_3 = lambda do |vert_a, vert_b, xa, ya, za, xb, yb, zb|
      cross = lambda do |v1, v2|
        [v1[1] * v2[2] - v1[2] * v2[1],
         v1[2] * v2[0] - v1[0] * v2[2],
         v[0] * v2[1] - v1[1] * v2[0]]
      end
      dot = lambda do |v1, v2| v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2] end
      sub = lambda do |v1, v2| [v1[0] - v2[0], v1[1] - v2[1], v1[2] - v2[2]] end
      add = lambda do |v1, v2| [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]] end
      scale = lambda do |v1, c| [v1[0] * c, v1[1] * c, v1[2] * c] end
      tolerance = 1e-6
      line_b = [xa, ya, za]
      line_m = sub.call([xb, yb, zb], line_b)
      normal = cross.call(vert_a, verb_b)
      denominator = dot.call(normal, line_m)
      if denominator.abs == tolerance
        nil
      else
        add.call(line_b, scale.call(line_m, -dot.call(normal, line_b) / denominator))
      end
    end
    transition_point_2 = lambda do |vert, xa, ya, xb, yb|
      ax = vert[0]
      bx = xa - xb
      ay = vert[1]
      by = ya - yb
      cx = -xa
      cy = -ya
      d = by * cx - bx * cy
      f = ay * bx - ax * by
      if f == 0
        nil
      else
        [(d * ax) / f, (d * ay) / f]
      end
    end
    calculate_gains = lambda do |x, y, z, group|
      zero_coord = 1e-10
      zero_gain = 1e-10
      size = group[:size]
      mat = group[:matrix]
      if x.abs < zero_coord and y.abs < zero_coord and z.abs < zero_coord
        [true, [1.0, 1.0, 1.0]]
      elsif size == 3
        gain_a = mat[0][0] * x + mat[1][0] * y + mat[2][0] * z
        gain_b = mat[0][1] * x + mat[1][1] * y + mat[2][1] * z
        gain_c = mat[0][2] * x + mat[1][2] * y + mat[2][2] * z
        mag = distance(gain_a, gain_b, gain_c)
        if gain_a.abs < zero_gain
          gain_a = 0.0
        end
        if gain_b.abs < zero_gain
          gain_b = 0.0
        end
        if gain_c.abs < zero_gain
          gain_c = 0.0
        end
        [(gain_a >= 0 and gain_b >= 0 and gain_c >= 0), [gain_a / mag, gain_b / mag, gain_c / mag]]
      elsif size == 2
        gain_a = mat[0][0] * x + mat[1][0] * y
        gain_b = mat[0][1] * x + mat[1][1] * y
        mag = distance(gain_a, gain_b, 0.0)
        if gain_a.abs < zero_gain
          gain_a = 0.0
        end
        if gain_b.abs < zero_gain
          gain_b = 0.0
        end
        [(gain_a >= 0 and gain_b >= 0), [gain_a / mag, gain_b / mag]]
      elsif size == 1
        [true, [1.0]]
      end
    end
    find_group = lambda do |x, y, z|
      grp, gns = speakers[:groups].detect do |group|
        inside, gains = calculate_gains.call(x, y, z, group)
        if inside
          break([group, gains])
        end
      end
      if grp
        [grp, gns]
      else
        [false, false]
      end
    end
    push_zero_gains = lambda do |time|
      speakers[:number].times do |i|
        channel_gains[i].push(time)
        channel_gains[i].push(0.0)
      end
      rev_channels.times do |i|
        channel_rev_gains[i].push(time)
        channel_rev_gains[i].push(0.0)
      end
    end
    push_gains = lambda do |group, gains, dist, time|
      outputs = Array.new(out_channels, 0.0)
      rev_outputs = Array.new(rev_channels, 0.0)
      att = if dist >= inside_radius
              1.0 / dist ** direct_power
            else
              1.0 - (dist / inside_radius) ** (1.0 / inside_direct_power)
            end
      ratt = if dist >= inside_radius
               1.0 / dist ** reverb_power
             else
               1.0 - (dist / inside_radius) ** (1.0 / inside_reverb_power)
             end
      if dist >= inside_radius
        group[:speakers].each_with_index do |speaker, i|
          gain = gains[i]
          outputs[speaker] = gain * att
          if rev_channels > 1
            rev_outputs[speaker] = gain * ratt
          end
        end
      else
        speakers[:number].times do |speaker|
          if (found = group[:speakers].index(speaker))
            gain = gains[found]
            outputs[speaker] = gain + (1.0 - gain) * att
            if rev_channels > 1
              rev_outputs[speaker] = gain + (1.0 - gain) * ratt
            end
          else
            outputs[speaker] = att
            if rev_channels > 1
              rev_outputs[speaker] = ratt
            end
          end
        end
      end
      speakers[:number].times do |i|
        channel_gains[i].push(time)
        channel_gains[i].push(outputs[i])
        if rev_channels > 1
          channel_rev_gains[i].push(time)
          channel_rev_gains[i].push(rev_outputs[i])
        end
      end
      if rev_channels == 1
        channel_rev_gains[0].push(time)
        channel_rev_gains[0].push(ratt)
      end
    end
    amplitude_panning = lambda do |x, y, z, dist, time, q|
      if prev_group
        inside, gains = calculate_gains.call(x, y, z, prev_group)
        if time != prev_time
          speed = (dist - prev_dist) / (time - prev_time)
          if speed > speed_limit and $VERBOSE
            warn(format("supersonic radial movement at [%f, %f, %f, %f], speed=%f",
                        x, y, z, time, speed))
          end
        end
        if inside
          push_gains.call(prev_group, gains, dist, time)
          prev_x = x
          prev_y = y
          prev_z = z
        else
          group, gains = find_group.call(x, y, z)
          if group
            edge = group[:vertices] & prev_group[:vertices]
            if edge.length == 2
              pint = transition_point_3.call(edge[0], edge[1], x, y, z, prev_x, prev_y, prev_z)
              if pint
                xi, yi, zi = pint
                di = distance(xi, yi, zi)
                ti = prev_time + (distance(xi - prev_x, yi - prev_y, zi - prev_z) /
                                  distance(x - prev_x, y - prev_y, z - prev_z)) *
                                 (time - prev_time)
                inside, gains = calculate_gains.call(xi, yi, zi, prev_group)
                if inside
                  push_gains.call(prev_group, gains, di, ti)
                else
                  inside, gains = calculate_gains.call(xi, yi, zi, group)
                  if inside
                    push_gains.call(group, gains, di, ti)
                  else
                    error("Outside of both adyacent groups [%s:%s:%s %s]", xi, yi, zi, ti.inspect)
                  end
                end
              end
            elsif edge.length == 1 and group[:size] == 2
              pint = transition_point_2.call(edge[0], x, y, prev_x, prev_y)
              if pint
                xi, yi = pint
                di = distance(xi, yi, 0.0)
                ti = prev_time + (distance(xi - prev_x, yi - prev_y, 0.0) /
                                  distance(x - prev_x, y - prev_y, 0.0)) * (time - prev_time)
                inside, gains = calculate_gains.call(xi, yi, 0.0, prev_group)
                if inside
                  push_gains.call(prev_group, gains, di, ti)
                  inside, gains = calculate_gains.call(xi, yi, 0.0, group)
                  if inside
                    push_gains.call(group, gains, di, ti)
                  else
                    error("Outside of both adyacent groups [%s:%s %s]", xi, yi, ti.inspect)
                  end
                end
              end
            elsif edge.length == 1
              speakers[:groups].each do |int_group|
                if int_group[:vertices].member?(edge[0]) and
                                               (int_group != group) and
                                               (int_group != prev_group)
                  edge1 = int_group[:vertices] & prev_group[:vertices]
                  edge2 = int_group[:vertices] & group[:vertices]
                  message("e1=%s; e2=%s", edge1, edge2)
                end
              end
              warn(format("crossing between groups with only one point in common
  prev=%s
  curr=%s", prev_group.inspect, group.inspect)) if $VERBOSE
            elsif edge.length == 0 and $VERBOSE
              warn(format("crossing between groups with no common points, %s%s to %s%s",
                          prev_group[:id], prev_group[:speakers], group[:id], group[:speakers]))
            end
            push_gains.call(group, gains, dist, time)
            prev_group = group
            prev_x = x
            prev_y = y
            prev_z = z
          else
            push_zero_gains.call(time)
            prev_group = false
          end
        end
      else
        group, gains = find_group.call(x, y, z)
        if group
          push_gains.call(group, gains, dist, time)
          prev_group = group
          prev_x = x
          prev_y = y
          prev_z = z
        else
          push_zero_gains.call(time)
          prev_group = false
        end
      end
      prev_time = time
      prev_dist = dist
    end
    b_format_ambisonics = lambda do |x, y, z, dist, time|
      att = if dist > inside_radius
              (inside_radius / dist) ** direct_power
            else
              (dist / inside_radius) ** (1.0 / inside_direct_power)
            end
      attw = if dist > inside_radius
               Point707 * att
             else
               1.0 - (1.0 - Point707) * ((dist / inside_radius) ** direct_power)
             end
      ratt = if dist > inside_radius
               (inside_radius / dist) ** reverb_power
             else
               (dist / inside_radius) ** (1.0 / inside_reverb_power)
             end
      rattw = if dist > inside_radius
                Point707 * ratt
              else
                1.0 - (1.0 - Point707) * ((dist / inside_radius) ** reverb_power)
              end
      channel_gains[0].push(time)
      channel_gains[0].push(attw)
      channel_gains[1].push(time)
      channel_gains[1].push((dist.zero? ? 0 : (y / dist)) * att)
      channel_gains[2].push(time)
      channel_gains[2].push((dist.zero? ? 0 : (-x / dist)) * att)
      channel_gains[3].push(time)
      channel_gains[3].push((dist.zero? ? 0 : (z / dist)) * att)
      unless rev_channels.zero?
        if rev_channels == 1
          channel_rev_gains[0].push(time)
          channel_rev_gains[0].push((if dist >= inside_radius
                                       1.0 / dist ** reverb_power
                                     else
                                       1.0 - (dist / inside_radius) ** (1.0 / inside_reverb_power)
                                     end))
        else
          channel_rev_gains[0].push(time)
          channel_rev_gains[0].push(rattw)
          channel_rev_gains[1].push(time)
          channel_rev_gains[1].push((dist.zero? ? 0 : (y / dist)) * ratt)
          channel_rev_gains[2].push(time)
        channel_rev_gains[2].push((dist.zero? ? 0 : (-x / dist)) * ratt)
          channel_rev_gains[3].push(time)
          channel_rev_gains[3].push((dist.zero? ? 0 : (z / dist)) * ratt)
        end
      end
    end
    decoded_ambisonics = lambda do |x, y, z, dist, time|
      att = if dist > inside_radius
              (inside_radius / dist) ** direct_power
            else
              (dist / inside_radius) ** (1.0 / inside_direct_power)
            end
      attw = if dist > inside_radius
               Point707 * att
             else
               1.0 - (1.0 - Point707) * ((dist / inside_radius) ** direct_power)
             end
      ratt = if dist > inside_radius
               (inside_radius / dist) ** reverb_power
             else
               (dist / inside_radius) ** (1.0 / inside_reverb_power)
             end
      rattw = if dist > inside_radius
                Point707 * ratt
              else
                1.0 - (1.0 - Point707) * ((dist / inside_radius) ** reverb_power)
              end
      speakers[:number].times do |i|
        s = speakers[:coords][i]
        signal = Point707 * (attw * Point707 +
                             att * (dist.zero? ? 0 : (y / dist)) * s[1] +
                             att * (dist.zero? ? 0 : (x / dist)) * s[0] +
                             att * (dist.zero? ? 0 : (z / dist)) * s[2])
        channel_gains[i].push(time)
        channel_gains[i].push(signal)
      end
      if rev_channels == 1
        channel_rev_gains[0].push(time)
        channel_rev_gains[0].push((if dist >= inside_radius
                                     1.0 / (dist ** reverb_power)
                                   else
                                     1.0 - (dist / inside_radius) ** (1.0 / inside_reverb_power)
                                   end))
      else
        rev_channels.times do |i|
          s = speakers[:coods]
          signal = Point707 * (rattw * Point707 +
                               ratt * (dist.zero? ? 0 : (y / dist)) * s[1] +
                               ratt * (dist.zero? ? 0 : (x / dist)) * s[0] +
                               ratt * (dist.zero? ? 0 : (z / dist)) * s[2])
          channel_rev_gains[i].push(time)
          channel_rev_gains[i].push(signal)
        end
      end
    end
    walk_all_rooms = lambda do |x, y, z, time|
      room = 0
      dist = distance(x, y, z)
      if first_dist.zero?
        first_dist = dist
      end
      last_dist = dist
      if min_dist.zero? or dist < min_dist
        min_dist = dist
      end
      if max_dist.zero? or dist > max_dist
        max_dist = dist
      end
      delay.push(time)
      delay.push(dist2samples.call(dist))
      case render_using
      when Amplitude_panning
        amplitude_panning.call(x, y, z, dist, time, 1)
      when B_format_ambisonics
        b_format_ambisonics.call(x, y, z, dist, time)
      when Decoded_ambisonics
        decoded_ambisonics.call(x, y, z, dist, time)
      end
      room += 1
    end
    change_direction = lambda do |xa, ya, za, ta, xb, yb, zb, tb|
      walk_all_rooms.call(xa, ya, za, ta)
      if xa != xb or ya != yb or za != zb or ta != tb
        xi, yi, zi = nearest_point(xa, ya, za, xb, yb, zb, 0.0, 0.0, 0.0)
        if (((xa < xb) ? (xa <= xi and xi <= xb) : (xb <= xi and xi <= xa)) and
            ((ya < yb) ? (ya <= yi and yi <= yb) : (yb <= yi and yi <= ya)) and
            ((za < zb) ? (za <= zi and zi <= zb) : (zb <= zi and zi <= za)))
          walk_all_rooms.call(xi, yi, zi, tb + (ta - tb) * (distance(xb - xi, yb - yi, zb - zi) /
                                                            distance(xb - xa, yb - ya, zb - za)))
        end
      end
    end
    intersects_inside_radius = lambda do |xa, ya, za, ta, xb, yb, zb, tb|
      mag = distance(xb - xa, yb - ya, zb - za)
      vx = (xb - xa) / mag
      vy = (yb - ya) / mag
      vz = (zb - za) / mag
      bsq = xa * vx + ya * vy + za * vz
      u = distance(xa, ya, za) - inside_radius * inside_radius
      disc = bsq * bsq - u
      if disc >= 0.0
        root = sqrt(disc)
        rin = -bsq - root
        rout = -bsq + root
        xi = xo = nil
        if rin > 0 and rin < mag
          xi = xa + vx * rin
          yi = ya + vy * rin
          zi = za + vz * rin
          ti = tb + (ta - tb) * (distance(xb - xi, yb - yi, zb - zi) /
                                 distance(xb - xa, yb - ya, zb - za))
        end
        if rout > 0 and rout.abs < mag
          xo = xa + vx * rout
          yo = ya + vy * rout
          zo = za + vz * rout
          to = tb + (ta - tb) * (distance(xb - xo, yb - yo, zb - zo) /
                                 distance(xb - xa, yb - ya, zb - za))
        end
        if xi
          change_direction.call(xa, ya, za, ta, xi, yi, zi, ti)
          if xo
            change_direction.call(xi, yi, zi, ti, xo, yo, zo, to)
            change_direction.call(xo, yo, zo, to, xb, yb, zb, tb)
          else
            change_direction.call(xi, yi, zi, ti, xb, yb, zb, tb)
          end
        else
          if xo
            change_direction.call(xa, ya, za, ta, xo, yo, zo, to)
            change_direction.call(xo, yo, zo, to, xb, yb, zb, tb)
          else
            change_direction.call(xa, ya, za, ta, xb, yb, zb, tb)
          end
        end
      else
        change_direction.call(xa, ya, za, ta, xb, yb, zb, tb)
      end
    end
    minimum_segment_length = lambda do |xa, ya, za, ta, xb, yb, zb, tb|
      dist = distance(xb - xa, yb - ya, zb - za)
      if dist < minimum_seg_length
        intersects_inside_radius.call(xa, ya, za, ta, xb, yb, zb, tb)
      else
        xi = (xa + xb) / 2.0
        yi = (ya + yb) / 2.0
        zi = (za + zb) / 2.0
        ti = tb + (ta - tb) * (distance(xb - xi, yb - yi, zb - zi) /
                               distance(xb - xa, yb - ya, zb - za))
        minimum_segment_length.call(xa, ya, za, ta, xi, yi, zi, ti)
        minimum_segment_length.call(xi, yi, zi, ti, xb, yb, zb, tb)
      end
    end
    if xpoints.length == 1
      walk_all_rooms.call(xpoints[0], ypoints[0], zpoints[0], tpoints[0])
    else
      let do
        xb, yb, zb, tb = 0.0
        (tpoints.length - 1).times do |i|
          xa = xpoints[i]
          ya = ypoints[i]
          za = zpoints[i]
          ta = tpoints[i]
          xb = xpoints[i + 1]
          yb = ypoints[i + 1]
          zb = zpoints[i + 1]
          tb = tpoints[i + 1]
          minimum_segment_length.call(xa, ya, za, ta, xb, yb, zb, tb)
        end
        walk_all_rooms.call(xb, yb, zb, tb)
      end
    end
    speakers[:delays].each_with_index do |del, channel|
      out_delays[channel] = if del != 0.0
                              make_delay(time2samples.call(del))
                            else
                              nil
                            end
      max_out_delay = [max_out_delay, del].max
    end
    min_delay = dist2samples.call(min_dist)
    start = dist2samples.call(first_dist - (initial_delay ? 0.0 : min_dist))
    real_dur = dur + dist2seconds.call(last_dist - first_dist)
    run_beg = time2samples.call(startime).floor
    run_end = ((time2samples.call(startime + dur) + dist2samples.call(last_dist) +
                time2samples.call(max_out_delay)) - (initial_delay ? 0.0 : min_delay)).floor
    min_dist_unity = if min_dist < inside_radius
                       inside_radius
                     else
                       min_dist
                     end
    unity_gain = scaler * (if unity_gain_dist.kind_of?(Numeric)
                             unity_gain_dist ** direct_power
                           elsif (not unity_gain_dist) or unity_gain_dist.empty?
                             min_dist_unity ** direct_power
                           else
                             1.0
                           end)
    unity_rev_gain = scaler * (if unity_gain_dist.kind_of?(Numeric)
                                 unity_gain_dist ** reverb_power
                               elsif (not unity_gain_dist) or unity_gain_dist.empty?
                                 min_dist_unity ** reverb_power
                               else
                                 1.0
                               end)
    [Dlocs.new(start,
               ((startime + dur) * $rbm_srate).floor,
               speakers[:number],
               rev_channels,
               out_delays,
               speakers[:map],
               Array.new(speakers[:number]) do |i|
                 make_env(:envelope, channel_gains[i],
                          :scaler, if render_using == B_format_ambisonics
                                     1.0
                                   else
                                     unity_gain
                                   end,
                          :duration, real_dur)
               end,
               if rev_channels.nonzero?
                 Array.new(rev_channels) do |i|
                   make_env(:envelope, channel_rev_gains[i],
                            :scaler, if render_using == B_format_ambisonics
                                       1.0
                                     else
                                       unity_rev_gain
                                     end,
                            :duration, real_dur)
                 end
               else
                 nil
               end,
               make_env(:envelope, delay,
                        :offset, (initial_delay ? 0.0 : -min_delay),
                        :duration, real_dur),
               make_delay(:size, 0,
                          "max-size".to_sym, [1, dist2samples.call(max_dist).ceil + 1].max),
               make_env(:envelope, if reverb_amount.kind_of?(Numeric)
                                     [0, reverb_amount, 1, reverb_amount]
                                   else
                                     reverb_amount
                                   end,
                        :duration, real_dur)),
     run_beg, run_end]
  end

  def dlocsig(sloc, dloc, input)
    if dloc < sloc[:start]
      delay(sloc[:path], (dloc > sloc[:end] ? 0.0 : input), 0.0)
      sloc[:out_channels].times do |chan|
        out_any(dloc, 0.0, chan, $rbm_output)
      end
    else
      sample = delay(sloc[:path], (dloc > sloc[:end] ? 0.0 : input), env(sloc[:delay]))
      sloc[:out_channels].times do |chan|
        out_any(dloc,
                if sloc[:out_delays][chan]
                  delay(sloc[:out_channels][chan], sample * env(sloc[:gains][chan]))
                else
                  sample * env(sloc[:gains][chan])
                end,
                sloc[:out_map][chan],
                $rbm_output)
      end
      if $rbm_reverb
        if sloc[:rev_channels] == 1
          outa(dloc, sample * env(sloc[:rev]) * env(sloc[:rev_gains][0]), $rbm_reverb)
        else
          amount = env(sloc[:rev])
          sloc[:rev_channels].times do |chan|
            out_any(dloc,
                    sample * amount * env(sloc[:rev_gains][chan]),
                    sloc[:out_map][chan],
                    $rbm_reverb)
          end
        end
      end
    end
  end

  def make_path(*args)
    path              = if args[0].kind_of?(Array)
                          args.shift
                        else
                          get_args(args, :path, nil)
                        end
    closed            = get_args(args, :closed, false)
    d3                = get_args(args, :d3, true)
    initial_direction = get_args(args, :initial_direction, nil)
    final_direction   = get_args(args, :final_direction, nil)
    if (not path) or (path.kind_of?(Array) and path.empty?)
      warn("Can't define a path with no points in it")
    end
    if closed and initial_direction
      error("Can't specify initial direction %s for a closed path %s",
            initial_direction.inspect, path.inspect)
    end
    if closed and final_direction
      error("Can't specify final direction %s for a closed path %s",
            final_direction.inspect, path.inspect)
    end
    if closed and (unless path[0].kind_of?(Array)
                     start = path[0]
                     fin = path[-1]
                     (start[0] == fin[0]) and
                     (start[1] == fin[1]) and
                     (d3 ? (start[2] == fin[2]) : true)
                   else
                     fin = path[(d3 ? -3 : -2)..-1]
                     (path[0] == fin[0]) and
                     (path[1] == fin[1]) and
                     (d3 ? (path[2] == fin[2]) : true)
                   end)
      error("Closed path %s is not closed", path.inspect)
    end
    closed ? Closed_bezier_path.new(:path, path, *args) : Open_bezier_path.new(:path, path, *args)
  end
  
  def make_polar_path(*args)
    path = if args[0].kind_of?(Array)
             args.shift
           else
             get_args(args, :path, nil)
           end
    make_path(:path, path, :polar, true, *args)
  end
  
  def make_closed_path(*args)
    path = if args[0].kind_of?(Array)
             args.shift
           else
             get_args(args, :path, nil)
           end
    make_path(:path, path, :closed, true, *args)
  end

  def make_literal_path(*args)
    path = if args[0].kind_of?(Array)
             args.shift
           else
             get_args(args, :path, nil)
           end
    Literal_path.new(:path, path, :polar, false, *args)
  end
  
  def make_literal_polar_path(*args)
    path = if args[0].kind_of?(Array)
             args.shift
           else
             get_args(args, :path, nil)
           end
    Literal_path.new(:path, path, :polar, true, *args)
  end

  def make_spiral_path(*args)
    total_angle = get_args(args, :total_angle, nil)
    turns       = get_args(args, :turns, nil)
    if total_angle and turns
      error("can't specify total_angle [%d] and turns [%d] at the same time for the spiral path",
            total_angle, turns)
    end
    Spiral_path.new(*args)
  end

  def parse_cartesian_coordinates(points, d3)
    if points[0].kind_of?(Array)
      x = points.map do |p| p[0] end
      y = points.map do |p| p[1] end
      z = points.map do |p| d3 ? (p[2] or 0.0) : 0.0 end
      v = points.map do |p| d3 ? p[3] : p[2] end
      [x, y, z, v]
    else
      if d3
        x = []
        y = []
        z = []
        0.step(points.length - 3, 3) do |i|
          x += [points[i]]
          y += [points[i + 1]]
          z += [points[i + 2]]
        end
        [x, y, z, x.map do |i| nil end]
      else
        x = []
        y = []
        0.step(points.length - 2, 2) do |i|
          x += [points[i]]
          y += [points[i + 1]]
        end
        [x, y, x.map do |i| 0.0 end, x.map do |i| nil end]
      end
    end
  end

  def parse_polar_coordinates(points, d3)
    if points[0].kind_of?(Array)
      x = []
      y = []
      z = []
      v = []
      points.each do |p|
        d = p[0]
        a = p[1]
        e = (d3 ? (p[2] or 0.0) : 0.0)
        evec = cis((e / $dlocsig_one_turn) * TWO_PI)
        dxy = d * evec.real
        avec = cis((a / $dlocsig_one_turn) * TWO_PI)
        z << (d * evec.image)
        x << (dxy * avec.image)
        y << (dxy * avec.real)
        v << (d3 ? p[3] : p[2])
      end
      [x, y, z, v]
    else
      if d3
        x = []
        y = []
        z = []
        0.step(points.length - 1, 3) do |i|
          d = points[i]
          a = points[i + 1]
          e = points[i + 2]
          evec = cis((e / $dlocsig_one_turn) * TWO_PI)
          dxy = (d * evec.real)
          avec = cis((a / $dlocsig_one_turn) * TWO_PI)
          z << (d * evec.image)
          x << (dxy * avec.image)
          y << (dxy * avec.real)
        end
        [x, y, z, x.map do |i| nil end]
      else
        x = []
        y = []
        0.step(points.length - 1, 2) do |i|
          d = points[i]
          a = points[i + 1]
          avec = cis((a / $dlocsig_one_turn) * TWO_PI)
          x << (d * avec.image)
          y << (d * avec.real)
        end
        [x, y, x.map do |i| 0.0 end, x.map do |i| nil end]
      end
    end
  end
  
  def distance(x, y, z)
    (x * x + y * y + z * z) ** 0.5
  end

  def nearest_point(x0, y0, z0, x1, y1, z1, px, py, pz)
    vcos = lambda do |a0, b0, c0, a1, b1, c1|
      (a0 * a1 + b0 * b1 + c0 * c1) / (distance(a0, b0, c0) * distance(a1, b1, c1))
    end
    same = lambda do |a0, b0, c0, a1, b1, c1|
      a0 == a1 and b0 == b1 and c0 == c1
    end
    if same.call(x0, y0, z0, px, py, pz)
      [x0, y0, z0]
    elsif same.call(x1, y1, z1, px, py, pz)
      [x1, y1, z1]
    elsif same.call(x0, y0, z0, x1, y1, z1)
      [x0, y0, z0]
    else
      xm0 = x1 - x0
      ym0 = y1 - y0
      zm0 = z1 - z0
      xm1 = px - x0
      ym1 = py - y0
      zm1 = pz - z0
      p = distance(xm1, ym1, zm1) * vcos.call(xm0, ym0, zm0, xm1, ym1, zm1)
      l = distance(xm0, ym0, zm0)
      ratio = p / l
      [x0 + xm0 * ratio, y0 + ym0 * ratio, z0 + zm0 * ratio]
    end
  end

  def make_a_even
    g = lambda do |m|
      $path_gtab = Array.new(Path_maxcoeff) unless $path_gtab
      $path_gtab[0] = 1.0
      $path_gtab[1] = -4.0
      (2...Path_maxcoeff).each do |i|
        $path_gtab[i] = -4.0 * $path_gtab[i - 1] - $path_gtab[i - 2]
      end
      $path_gtab[m]
    end
    $path_ak_even = Array.new(Path_maxcoeff - 1)
    (1...Path_maxcoeff).each do |m|
      $path_ak_even[m - 1] = Array.new(m)
      (1..m).each do |k|
        $path_ak_even[m - 1][k - 1] = (-g.call(m - k) / g.call(m)).to_f
      end
    end
  end

  def make_a_odd
    f = lambda do |m|
      $path_ftab = Array.new(Path_maxcoeff) unless $path_ftab
      $path_ftab[0] = 1.0
      $path_ftab[1] = -3.0
      (2...Path_maxcoeff).each do |i|
        $path_ftab[i] = -4.0 * $path_ftab[i - 1] - $path_ftab[i - 2]
      end
      $path_ftab[m]
    end
    $path_ak_odd = Array.new(Path_maxcoeff - 1)
    (1...Path_maxcoeff).each do |m|
      $path_ak_odd[m - 1] = Array.new(m)
      (1..m).each do |k|
        $path_ak_odd[m - 1][k - 1] = (-f.call(m - k) / f.call(m)).to_f
      end
    end
  end
  
  def a(k, n)
    if ([Path_maxcoeff * 2.0 + 1, n].min).odd?
      make_a_odd() unless $path_ak_odd
      $path_ak_odd[(n - 3) / 2][k - 1]
    else
      make_a_even() unless $path_ak_even
      $path_ak_even[(n - 4) / 2][k - 1]
    end
  end

  def ac(k, n)
    n = [n, Path_maxcoeff].min
    make_a_even() unless $path_ak_even
    $path_ak_even[n - 2][k - 1]
  end

  def rotation_matrix(x, y, z, angle)
    normalize = lambda do |a, b, c|
      mag = distance(a, b, c)
      [a / mag, b / mag, c / mag]
    end
    dx, dy, dz = normalize.call(x, y, z)
    ri = Matrix.I(3)
    ra = Matrix.rows([[0.0, dz, -dy], [-dz, 0.0, dx], [dy, -dx, 0.0]])
    raa = ra * ra
    sn = sin(-angle)
    omcs = 1 - cos(-angle)
    raa = raa.map do |x| omcs * x end
    ra = ra.map do |x| sn * x end
    (ri + ra + raa)
  end
end

include Dlocsig
$dlocsig_speaker_configs =
[[nil,
  arrange_speakers(:speakers, [0]),
  arrange_speakers(:speakers, [-60, 60]),
  arrange_speakers(:speakers, [-45, 45, 180]),
  arrange_speakers(:speakers, [-45, 45, 135, 225]),
  arrange_speakers(:speakers, [-45, 0, 45, 135, -135]),
  arrange_speakers(:speakers, [-60, 0, 60, 120, 180, 240]),
  arrange_speakers(:speakers, [-45, 0, 45, 100, 140, -140, -100]),
  arrange_speakers(:speakers, [-22.5, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5])],
 [nil, nil, nil, nil,
  arrange_speakers(:speakers, [[-60, 0], [60, 0], [180, 0], [0, 90]],
                   :groups, [[0, 1, 3], [1, 2, 3], [2, 0, 3], [0, 1, 2]]),
  arrange_speakers(:speakers, [[-45, 0], [45, 0], [135, 0], [-135, 0], [0, 90]],
                   :groups, [[0, 1, 4], [1, 2, 4], [2, 3, 4], [3, 0, 4], [0, 1, 2], [2, 3, 0]]),
  arrange_speakers(:speakers, [[-45, 0], [45, 0], [135, 0], [-135, 0], [-90, 60], [90, 60]],
                   :groups, [[0, 1, 4], [1, 4, 5], [1, 2, 5], [2, 3, 5], [3, 4, 5],
                             [3, 0, 4], [0, 1, 2], [2, 3, 0]]),
  arrange_speakers(:speakers, [[-45, 0], [45, 0], [135, 0], [-135, 0], [-60, 60], [60, 60],
                               [180, 60]],
                   :groups, [[0, 1, 4], [1, 4, 5], [1, 2, 5], [2, 6, 5], [2, 3, 6],
                             [3, 4, 6], [3, 0, 4], [4, 5, 6], [0, 1, 2], [2, 3, 0]]),
  arrange_speakers(:speakers, [[-45, 10], [45, -10], [135, -10], [225, -10], [-45, 45],
                               [45, 45], [135, 45], [225, 45]],
                   :groups, [[0, 4, 5], [0, 5, 1], [5, 1, 2], [2, 6, 5], [6, 7, 2], [2, 3, 7],
                             [3, 7, 4], [3, 0, 4], [4, 7, 6], [6, 5, 4], [0, 1, 2], [2, 3, 0]])]]

#
# Gnuplot class
#

class GnuPlot
  @@plot_stream = nil
  
  def initialize
    if (not @@plot_stream.kind_of?(IO)) or (@@plot_stream.kind_of?(IO) and @@plot_stream.closed?)
      gnuplot_open()
    end
  end

  def gnuplot_open
    @@plot_stream = IO.popen("gnuplot", "w")
  end

  def gnuplot_close
    unless @@plot_stream.closed?
      @@plot_stream.puts "quit"
      @@plot_stream.close
      @@plot_stream = nil
    end
  end

  def command(*args)
    gnuplot_open() if @@plot_stream.closed?
    @@plot_stream.printf(*args)
    format(*args).chomp
  rescue
    warn get_func_name
  end

  def reset
    command "reset\n"
  end

  def set_autoscale
    command "set autoscale\n"
  end

  def set_x_range(range = [])
    command("set xrange [%f:%f]\n", range[0], range[1]) if range.length == 2
  end

  def set_y_range(range = [])
    command("set yrange [%f:%f]\n", range[0], range[1]) if range.length == 2
  end

  def set_z_range(range = [])
    command("set zrange [%f:%f]\n", range[0], range[1]) if range.length == 2
  end

  def set_grid
    command "set grid xtics; set grid ytics; set grid ztics\n"
  end

  def set_surface
    command "set surface\n"
  end

  def set_parametric
    command "set parametric\n"
  end

  def set_ticslevel(level = 0)
    command("set ticslevel %.2f\n", level)
  end

  def set_title(title = "")
    command("set title \"%s\"\n", title) unless title.empty?
  end

  def set_label(label = "")
    command("set label \"%s\"\n", label) unless label.empty?
  end

  def set_margins(margin = 1)
    command("set tmargin %f\n", margin)
    command("set lmargin %f\n", margin)
    command("set rmargin %f\n", margin)
    command("set bmargin %f\n", margin)
  end

  def set_border(border = nil)
    command("set border %d\n", border.to_i) if border
  end

  def start_multiplot
    command "set multiplot\n"
  end

  def end_multiplot
    command "set nomultiplot\n"
  end

  def size(xorigin, yorigin, xsize, ysize)
    command("set origin %f,%f\n", xorigin.to_f, yorigin.to_f)
    command("set size %f,%f\n", xsize.to_f, ysize.to_f)
  end

  def data(data, *args)
    style = get_args(args, :style, "linespoints")
    label = get_args(args, :label, "")
    command("plot '-' %s %s\n", label.empty? ? "" : "title \"#{label}\"",
            style.empty? ? "" : "with #{style}")
    data.each_with_index do |y, x| command("%f %f\n", x, y) end
    command "e\n"
  end

  def plot_2d_curve(curve, *args)
    style = get_args(args, :style, "linespoints")
    label = get_args(args, :label, "")
    set_grid()
    command("plot '-' %s %s\n", label.empty? ? "" : "title \"#{label}\"",
            style.empty? ? "" : "with #{style}")
    curve.to_pairs.each do |c| command("%.8f %.8f\n", c[0], c[1]) end
    command "e\n"
  end

  def plot_2d_curves(curves, *args)
    styles = get_args(args, :styles, "linespoints")
    labels = get_args(args, :labels, "")
    set_grid()
    styles = curves.map do |i| styles end unless styles.kind_of?(Array)
    labels = curves.map do |i| labels end unless labels.kind_of?(Array)
    command "plot"
    curves.each_with_index do |x, i|
      style = styles[i]
      label = labels[i]
      command " '-' "
      command(" title \"%s\"", label) if label or (not label.empty?)
      command(" with %s", style) if style or (not style.empty?)
      command(", ") if i != (curves.length - 1)
    end
    command "\n"
    curves.each do |curve|
      curve.to_pairs.each do |c| command("%.8f %.8f\n", c[0], c[1]) end
      command "e\n"
    end
  end

  def plot_3d_curve(curve, *args)
    style  = get_args(args, :style, "linespoints")
    label  = get_args(args, :label, "")
    zstyle = get_args(args, :zstyle, "impulses")
    xrot   = get_args(args, :xrot, nil)
    zrot   = get_args(args, :zrot, nil)
    scale  = get_args(args, :scale, nil)
    zscale = get_args(args, :zscale, nil)
    set_border(127+256+512)
    set_grid()
    set_surface()
    set_parametric()
    set_ticslevel(0)
    if xrot or zrot or scale or zscale
      command("set view %s,%s,%s,%s\n", xrot, zrot, scale, zscale)
    end
    command "splot '-'"
    command(" title \"%s\"", label) unless label.empty?
    command(" with %s 1", style) unless style.empty?
    command(", '-' notitle with %s 1", zstyle) unless zstyle.empty?
    command "\n"
    curve.to_trias.each do |c| command("%.8f %.8f %.8f\n", c[0], c[1], c[2]) end
    command "e\n"
    if zstyle
      curve.to_trias.each do |c| command("%.8f %.8f %.8f\n", c[0], c[1], c[2]) end
      command "e\n"
    end
  end
end

#
# Path classes
#

class Path
  include Dlocsig

  $plotting = true
  
  attr_reader :plot

  def initialize(*args)
    @rx = @ry = @rz = @rv = @rt = @tx = @ty = @tz = @tt = nil
    @plot = GnuPlot.new if $plotting
  end

  def not_rendered
    @rx == nil
  end

  def not_transformed
    @tx == nil
  end

  def reset_transformation
    @tt = @tx = @ty = @tz = nil
  end

  def reset_rendering
    @rt = @rv = @rx = @ry = @rz = nil
    reset_transformation()
  end

  def path_x
    (@tx or (@rx or (render_path(); @rx)))
  end

  def path_y
    (@ty or (@ry or (render_path(); @ry)))
  end

  def path_z
    (@tz or (@rz or (render_path(); @rz)))
  end

  def path_time
    (@tt or (@rt or (render_path(); @rt)))
  end

  def transform_path(*args)
    scaling         = get_args(args, :scaling, nil)
    translation     = get_args(args, :translation, nil)
    rotation        = get_args(args, :rotation, nil)
    rotation_center = get_args(args, :rotation_center, nil)
    rotation_axis   = get_args(args, :rotation_axis, [0.0, 0.0, 1.0])
    render_path() if not_rendered()
    if scaling or translation or rotation
      rotation = if rotation
                   TWO_PI * (rotation / $dlocsig_one_turn)
                 end
      matrix = if rotation
                 rotation_matrix(rotation_axis[0], rotation_axis[1], rotation_axis[2], rotation)
               end
      xc = path_x()
      yc = path_y()
      zc = path_z()
      if rotation_center and (rotation_center.length != 3)
        error("rotation center has to have all three coordinates")
      end
      if rotation_axis and (rotation_axis.length != 3)
        error("rotation axis has to have all three coordinates")
      end
      let do
        xtr = []
        ytr = []
        ztr = []
        xc.each_with_index do |x, i|
          y = yc[i]
          z = zc[i]
          xw, yw, zw = x, y, z
          if rotation_center and rotation
            xw -= rotation_center[0]
            yw -= rotation_center[1]
            zw -= rotation_center[2]
          end
          if rotation
            mc = [xw, yw, zw]
            xv, yv, zv = matrix.column_vectors
            xr = xv.map_with_index do |xx, ii| xx * mc[ii] end.to_a.sum
            yr = yv.map_with_index do |xx, ii| xx * mc[ii] end.to_a.sum
            zr = zv.map_with_index do |xx, ii| xx * mc[ii] end.to_a.sum
            xw, yw, zw = xr, yr, zr
          end
          if rotation_center and rotation
            xw += rotation_center[0]
            yw += rotation_center[1]
            zw += rotation_center[2]
          end
          if scaling
            xw *= scaling[0]
            yw *= scaling[1] if scaling[1]
            zw *= scaling[2] if scaling[2]
          end
          if translation
            xw += translation[0]
            yw += translation[1] if translation[1]
            zw += translation[2] if translation[2]
          end
          xtr << xw
          ytr << yw
          ztr << zw
        end
        @tx, @ty, @tz = xtr, ytr, ztr
      end
    else
      @tt = @rt.dup
      @tx = @rx.dup
      @ty = @ry.dup
      @tz = @rz.dup
    end
  end

  def scale_path(scaling)
    transform_path(:scaling, scaling)
  end

  def translate_path(translation)
    transform_path(:translation, translation)
  end

  def rotate_path(rotation, *args)
    rotation_center = get_args(args, :rotation_center, nil)
    rotation_axis   = get_args(args, :rotation_axis, [0.0, 0.0, 1.0])
    transform_path(:rotation, rotation, :rotation_center,
                   rotation_center, :rotation_axis, rotation_axis)
  end
  
  def path_trajectory
    path_x.map_with_index do |d, i|
      [d, path_y[i], path_z[i]]
    end.flatten
  end

  def path_2d_trajectory
    path_x.map_with_index do |d, i|
      [d, path_y[i]]
    end.flatten
  end

  def path_velocity
    xp, yp, zp, tp = path_x, path_y, path_z, path_time
    (0...(tp.length - 1)).map do |i|
      xi = xp[i]
      yi = yp[i]
      zi = zp[i]
      ti = tp[i]
      xf = xp[i + 1]
      yf = yp[i + 1]
      zf = zp[i + 1]
      tf = tp[i + 1]
      [(ti + tf) / 2.0, distance(xf - xi, yf - yi, zf - zi) / (tf - ti)]
    end.flatten
  end
  
  def path_doppler
    xp, yp, zp, tp = path_x, path_y, path_z, path_time
    (0...(tp.length - 1)).map do |i|
      xi = xp[i]
      yi = yp[i]
      zi = zp[i]
      ti = tp[i]
      xf = xp[i + 1]
      yf = yp[i + 1]
      zf = zp[i + 1]
      tf = tp[i + 1]
      [(tf + ti) / 2.0, -((distance(xf, yf, zf) - distance(xi, yi, zi)) / (tf - ti))]
    end.flatten
  end

  def path_acceleration
    v = path_velocity()
    result = []
    0.step(v.length - 3, 2) do |i|
      ti = v[i]
      vi = v[i + 1]
      tf = v[i + 2]
      vf = v[i + 3]
      am = (vf - vi) / (tf - ti)
      result << ti << am << tf << am
    end
    result
  end

  def plot_open
    @plot = GnuPlot.new
  end
  
  def plot_close
    @plot.gnuplot_close
  end

  def cmd(*args)
    plot.command(format(*args) << "\n")
  end
  
  def plot_trajectory(*args)
    label  = get_args(args, :label, "trajectory")
    reset  = get_args(args, :reset, true)
    plot.reset() if reset
    plot.set_autoscale()
    if path_z.detect do |z| z.nonzero? end
      plot.plot_3d_curve(path_trajectory(), :label, label, *args)
    else
      plot.plot_2d_curve(path_2d_trajectory(), :label, label, *args)
    end
  rescue
    warn get_func_name
  end

  def plot_velocity(reset = true)
    plot.reset() if reset
    plot.set_autoscale()
    plot.plot_2d_curve(path_velocity(), :label, "velocity", :style, "steps")
  rescue
    warn get_func_name
  end

  def plot_doppler(reset = true)
    plot.reset() if reset
    plot.set_autoscale()
    plot.plot_2d_curve(path_doppler(), :label, "doppler", :style, "steps")
  rescue
    warn get_func_name
  end

  def plot_acceleration(reset = true)
    plot.reset() if reset
    plot.set_autoscale()
    plot.plot_2d_curve(path_acceleration(), :label, "acceleration", :style, "steps")
  rescue
    warn get_func_name
  end

  def pplot(normalize = true)
    norm = lambda do |env, nrm|
      unless nrm
        env
      else
        mx = env.to_pairs.map do |y| y[1] end.max
        if mx == 0
          env
        else
          env.to_pairs.map do |x| [x[0], x[1] / mx.to_f] end.flatten
        end
      end
    end
    plot.reset()
    plot.size(0, 0, 1, 1)
    plot.start_multiplot()
    plot.size(0.0, 0.333, 1.0, 0.667)
    plot_trajectory(:reset, false)
    plot.size(0.0, 0.0, 1.0, 0.333)
    plot.plot_2d_curves([norm.call(path_velocity(), normalize),
                         norm.call(path_acceleration(), normalize),
                         norm.call(path_doppler(), normalize)],
                        :labels, ["velocity", "acceleration", "doppler"],
                        :styles, ["steps", "steps", "steps"])
    plot.end_multiplot()
  rescue
    warn get_func_name
  end

  def snd_trajectory(*args)
    label = get_args(args, :label, "trajectory")
    snd   = get_args(args, :snd, false)
    chn   = get_args(args, :chn, false)
    graph(path_2d_trajectory(), label, false, false, false, false, snd, chn)
  rescue
    warn get_func_name
  end

  def snd_velocity(*args)
    label = get_args(args, :label, "velocity")
    snd   = get_args(args, :snd, false)
    chn   = get_args(args, :chn, false)
    graph(path_velocity(), label, false, false, false, false, snd, chn)
  rescue
    warn get_func_name
  end

  def snd_doppler(*args)
    label = get_args(args, :label, "doppler")
    snd   = get_args(args, :snd, false)
    chn   = get_args(args, :chn, false)
    graph(path_doppler(), label, false, false, false, false, snd, chn)
  rescue
    warn get_func_name
  end

  def snd_acceleration(*args)
    label = get_args(args, :label, "acceleration")
    snd   = get_args(args, :snd, false)
    chn   = get_args(args, :chn, false)
    graph(path_acceleration(), label, false, false, false, false, snd, chn)
  rescue
    warn get_func_name
  end

  def snd_plot
    len = channels()
    snd_trajectory(:chn, 0)
    snd_velocity(:chn, 1) if len > 1
    snd_acceleration(:chn, 2) if len > 2
    snd_doppler(:chn, 3) if len > 3
  rescue
    warn get_func_name
  end
end

class Bezier_path < Path
  def initialize(*args)
    @path      = get_args(args, :path, nil)
    @d3        = get_args(args, :d3, true)
    @polar     = get_args(args, :polar, false)
    @error     = get_args(args, :error, 0.01)
    @curvature = get_args(args, :curvature, nil)
    @x = @y = @z = @v = @bx = @by = @bz = nil
    if (not @path) or (@path.kind_of?(Array) and @path.empty?)
      error("Can't define a path with no points in it")
    end
    super
  end
  
  def set_path(points)
    @path = points
    @polar = false
    parse_path()
  end

  def set_polar_path(points)
    @path = points
    @polar = true
    parse_path()
  end

  def set_path_curvature(curvature)
    if curvature
      @curvature = curvature
      reset_fit()
    end
  end

  def set_path_error(error)
    if error
      @error = error
      reset_rendering()
    end
  end

  def not_parsed
    @x == nil
  end

  def reset_parsing
    @x = @y = @z = @v = nil
    reset_fit()
  end

  def parse_path
    if @polar
      @x, @y, @z, @v = parse_polar_coordinates(@path, @d3)
    else
      @x, @y, @z, @v = parse_cartesian_coordinates(@path, @d3)
    end
    if @v.min and @v.min < 0
      error("velocities for path %s must be all positive", @path.inspect)
    end
    reset_fit()
  end

  def not_fitted
    @bx == nil
  end

  def reset_fit
    @bx = @by = @bz = nil
    reset_rendering()
  end

  def fit_path
    parse_path() if not_parsed()
  end
  
  def render_path
    fit_path() if not_fitted()
    rx = []
    ry = []
    rz = []
    rv = []
    bezier_point = lambda do |u, c|
      u1 = 1.0 - u
      cr = Array.new(3) do |i| Array.new(3, 0.0) end
      cr.each_with_index do |x, i|
        cr[i] = (0..2).map do |j| u1 * c[i][j] + u * c[i][j + 1] end
      end
      1.downto(0) do |i|
        0.upto(i) do |j|
          3.times do |k| cr[k][j] = u1 * cr[k][j] + u * cr[k][j + 1] end
        end
      end
      [cr[0][0], cr[1][0], cr[2][0]]
    end
    berny = lambda do |xl, yl, zl, xh, yh, zh, ul, u, uh, c, err|
      x, y, z = bezier_point.call(u, c)
      xn, yn, zn = nearest_point(xl, yl, zl, xh, yh, zh, x, y, z)
      if distance(xn - x, yn - y, zn - z) > err
        xi, yi, zi = berny.call(xl, yl, zl, x, y, z, ul, (ul + u) / 2.0, u, c, err)
        xj, yj, zj = berny.call(x, y, z, xh, yh, zh, u, (u + uh) / 2.0, uh, c, err)
        [[xi, x, xj].flatten, [yi, y, yj].flatten, [zi, z, zj].flatten]
      else
        [[], [], []]
      end
    end
    if (not @v[0]) or @v[0].zero?
      @v[0] = 1
      @v[-1] = 1
    end
    if @x.length == 1
      @rx = @x
      @ry = @y
      @rz = @z
      @rt = [0.0]
      return
    end
    let do
      xf_bz = yf_bz = zf_bz = vf_bz = 0.0
      (@v.length - 1).times do |i|
        x_bz = @bx[i]
        y_bz = @by[i]
        z_bz = @bz[i]
        vi_bz = @v[i]
        vf_bz = @v[i + 1]
        xi_bz = x_bz[0]
        xf_bz = x_bz[-1]
        yi_bz = y_bz[0]
        yf_bz = y_bz[-1]
        zi_bz = z_bz[0]
        zf_bz = z_bz[-1]
        xs, ys, zs = berny.call(xi_bz, yi_bz, zi_bz, xf_bz, yf_bz, zf_bz, 0.0, 0.5, 1.0,
                                [x_bz, y_bz, z_bz], @error)
        rx = [rx, xi_bz, xs].flatten
        ry = [ry, yi_bz, ys].flatten
        rz = [rz, zi_bz, zs].flatten
        rv = [rv, vi_bz, xs.map do |x| nil end].flatten
      end
      rx = [rx, xf_bz].flatten
      ry = [ry, yf_bz].flatten
      rz = [rz, zf_bz].flatten
      rv = [rv, vf_bz].flatten
    end
    xseg = [rx[0]]
    yseg = [ry[0]]
    zseg = [rz[0]]
    vseg = [rv[0]]
    vi = rv[0]
    ti = 0.0
    times = [ti]
    (1...rx.length).each do |i|
      x = rx[i]
      y = ry[i]
      z = rz[i]
      v = rv[i]
      xseg << x
      yseg << y
      zseg << z
      vseg << v
      if v
        sofar = 0.0
        dseg = (0...xseg.length - 1).map do |j|
          xsi = xseg[j]
          ysi = yseg[j]
          zsi = zseg[j]
          xsf = xseg[j + 1]
          ysf = yseg[j + 1]
          zsf = zseg[j + 1]
          sofar += distance(xsf - xsi, ysf - ysi, zsf - zsi)
        end
        df = dseg[-1]
        vf = v
        aa = ((vf - vi) * (vf + vi)) / (df * 4.0)
        tseg = dseg.map do |d|
          ti + (if vf == vi
                  d / vi
                else
                  ((vi * vi + 4.0 * aa * d) ** 0.5 - vi) / (2.0 * aa)
                end)
        end
        times = [times, tseg].flatten
        xseg = [x]
        yseg = [y]
        zseg = [z]
        vseg = [v]
        vi = v
        ti = tseg[-1]
      end
    end
    @rx = rx
    @ry = ry
    @rz = rz
    # precision problems
    tf = Float("%f" % times[-1])
    @rt = times.map do |ti| Float("%f" % ti) / tf end
    reset_transformation()
  rescue
    warn get_func_name
  end
end

class Open_bezier_path < Bezier_path
  def initialize(*args)
    @initial_direction = get_args(args, :initial_direction, [0.0, 0.0, 0.0])
    @final_direction   = get_args(args, :final_direction, [0.0, 0.0, 0.0])
    super
  end

  def calculate_fit
    n = @x.length - 1
    m = n - 1
    p = [@x, @y, @z]
    d = Array.new(3) do |i| Array.new(n + 1, 0.0) end
    d = Matrix[d[0], d[1], d[2]].to_a
    ref = lambda do |z, j, i|
      if i > n
        z[j][i - n]
      elsif i < 0
        z[j][i + n]
      elsif i == n
        z[j][n] - d[j][n]
      elsif i == 0
        z[j][0] + d[j][0]
      else
        z[j][i]
      end
    end
    d[0][0] = (@initial_direction[0] or 0.0)
    d[1][0] = (@initial_direction[1] or 0.0)
    d[2][0] = (@initial_direction[2] or 0.0)
    d[0][n] = (@final_direction[0] or 0.0)
    d[1][n] = (@final_direction[1] or 0.0)
    d[2][n] = (@final_direction[2] or 0.0)
    (1...n).each do |i|
      (1..[Path_maxcoeff - 1, m].min).each do |j|
        3.times do |k|
          d[k][i] += ac(j, n) * (ref.call(p, k, i + j) - ref.call(p, k, i - j))
        end
      end
    end
    [n, p, d]
  end
  
  def fit_path
    parse_path() if not_parsed()
    points = @x.length
    case points
    when 1
      @bx = @by = @bz = nil
    when 2
      x1, x2 = @x[0..1]
      y1, y2 = @y[0..1]
      z1, z2 = @z[0..1]
      @bx = [[x1, x1, x2, x2]]
      @by = [[y1, y1, y2, y2]]
      @bz = [[z1, z1, z2, z2]]
    else
      n, p, d = calculate_fit()
      c = @curvature
      cs = Array.new(n)
      if c.kind_of?(NilClass) or (c.kind_of?(Array) and c.empty?)
        n.times do |i| cs[i] = [1.0, 1.0] end
      elsif c.kind_of?(Numeric)
        n.times do |i| cs[i] = [c, c] end
      elsif c.kind_of?(Array) and c.length == n
        c.each_with_index do |ci, i|
          cs[i] = if ci.kind_of?(Array)
                    if ci.length != 2
                      error("curvature sublist must have two elements %s", ci.inspect)
                    else
                      ci
                    end
                  else
                    [ci, ci]
                  end
        end
      else
        error("bad curvature argument %s to path, need %d elements", c.inspect, n)
      end
      @bx = (0...n).map do |i|
        [p[0][i], p[0][i] + d[0][i] * cs[i][0], p[0][i + 1] - d[0][i + 1] * cs[i][1], p[0][i + 1]]
      end
      @by = (0...n).map do |i|
        [p[1][i], p[1][i] + d[1][i] * cs[i][0], p[1][i + 1] - d[1][i + 1] * cs[i][1], p[1][i + 1]]
      end
      @bz = (0...n).map do |i|
        [p[2][i], p[2][i] + d[2][i] * cs[i][0], p[2][i + 1] - d[2][i + 1] * cs[i][1], p[2][i + 1]]
      end
    end
    reset_rendering()
  end
end

class Closed_bezier_path < Bezier_path
  def initialize(*args)
    super
  end

  def calculate_fit
    n = @x.length - 1
    m = (n - (n.odd? ? 3 : 4)) / 2
    p = [@x, @y, @z]
    d = Array.new(3) do |i| Array.new(n, 0.0) end
    ref = lambda do |z, j, i|
      if i > (n - 1)
        z[j][i - n]
      elsif i < 0
        z[j][i + n]
      else
        z[j][i]
      end
    end
    n.times do |i|
      (1..m).each do |j|
        3.times do |k|
          d[k][i] += a(j, n) * (ref.call(p, k, i + j) - ref.call(p, k, i - j))
        end
      end
    end
    if @curvature
      n.times do |i|
        curve = @curvature[i]
        d[0][i] *= curve
        d[1][i] *= curve
        d[2][i] *= curve
      end
    end
    [n - 1, p, d]
  end
  
  def fit_path
    parse_path() if not_parsed()
    if @x.length > 4
      n, p, d = calculate_fit()
      xc = (0...n).map do |i|
        [p[0][i], p[0][i] + d[0][i], p[0][i + 1] - d[0][i + 1], p[0][i + 1]]
      end
      yc = (0...n).map do |i|
        [p[1][i], p[1][i] + d[1][i], p[1][i + 1] - d[1][i + 1], p[1][i + 1]]
      end
      zc = (0...n).map do |i|
        [p[2][i], p[2][i] + d[2][i], p[2][i + 1] - d[2][i + 1], p[2][i + 1]]
      end
      @bx = xc + [[p[0][n], p[0][n] + d[0][n], p[0][0] - d[0][0], p[0][0]]]
      @by = yc + [[p[1][n], p[1][n] + d[1][n], p[1][0] - d[1][0], p[1][0]]]
      @bz = zc + [[p[2][n], p[2][n] + d[2][n], p[2][0] - d[2][0], p[2][0]]]
    else
      xc = []
      yc = []
      zc = []
      (@x.length - 1).times do |i|
        x1 = @x[i]
        x2 = @x[i + 1]
        y1 = @y[i]
        y2 = @y[i + 1]
        z1 = @z[i]
        z2 = @z[i + 1]
        xc << [x1, x1, x2, x2]
        yc << [y1, y1, y2, y2]
        zc << [z1, z1, z2, z2]
      end
      @bx = xc
      @by = yc
      @bz = zc
    end
    reset_rendering()
  end
end

class Literal_path < Path
  def initialize(*args)
    @path  = get_args(args, :path, nil)
    @d3    = get_args(args, :d3, true)
    @polar = get_args(args, :polar, false)
    super
  end
  
  def render_path
    if (not @path.kind_of?(Array)) or (@path.kind_of?(Array) and @path.empty?)
      error("Can't define a path with no points in it")
    end
    if @polar
      @rx, @ry, @rz, @rv = parse_polar_coordinates(@path, @d3)
    else
      @rx, @ry, @rz, @rv = parse_cartesian_coordinates(@path, @d3)
    end
    if (not @rv[0]) or @rv[0].zero?
      @rv[0] = 1
      @rv[-1] = 1
    end
    if @rx.length == 1
      @rt = [0.0]
      return
    end
    rx = @rx
    ry = @ry
    rz = @rz
    rv = @rv
    xseg = [rx[0]]
    yseg = [ry[0]]
    zseg = [rz[0]]
    vseg = [rv[0]]
    vi = rv[0]
    ti = 0.0
    times = [ti]
    (1...rx.length).each do |i|
      x = rx[i]
      y = ry[i]
      z = rz[i]
      v = rv[i]
      xseg << x
      yseg << y
      zseg << z
      vseg << v
      if v
        sofar = 0.0
        dseg = (0...xseg.length - 1).map do |j|
          xsi = xseg[j]
          ysi = yseg[j]
          zsi = zseg[j]
          xsf = xseg[j + 1]
          ysf = yseg[j + 1]
          zsf = zseg[j + 1]
          sofar += distance(xsf - xsi, ysf - ysi, zsf - zsi)
        end
        df = dseg[-1]
        vf = v
        aa = ((vf - vi) * (vf + vi)) / (df * 4.0)
        tseg = dseg.map do |d|
          ti + (if vf == vi
                  d / vi
                else
                  ((vi * vi + 4.0 * aa * d) ** 0.5 - vi) / (2.0 * aa)
                end)
        end
        times = [times, tseg].flatten
        xseg = [x]
        yseg = [y]
        zseg = [z]
        vseg = [v]
        vi = v
        ti = tseg[-1]
      end
    end
    # precision problems
    tf = Float("%f" % times[-1])
    @rt = times.map do |ti| Float("%f" % ti) / tf end 
    reset_transformation()
  end
end

class Spiral_path < Literal_path
  include Env
  
  def initialize(*args)
    @start_angle = get_args(args, :start_angle, 0.0)
    @total_angle = get_args(args, :total_angle, nil)
    @step_angle  = get_args(args, :step_angle, $dlocsig_one_turn / 100.0)
    @turns       = get_args(args, :turns, nil)
    @distance    = get_args(args, :distance, [0, 10, 1, 10])
    @height      = get_args(args, :height, [0, 0, 1, 0])
    @velocity    = get_args(args, :velocity, [0, 1, 1, 1])
    super
  end

  def render_path
    start = (@start_angle / $dlocsig_one_turn.to_f) * TWO_PI
    total = if @total_angle
              (@total_angle / $dlocsig_one_turn.to_f) * TWO_PI
            elsif @turns and (not @turns.zero?)
              @turns * TWO_PI
            else
              error("a spiral_path needs either a total_angle or turns, none specified")
            end
    steps = (total / ((@step_angle / $dlocsig_one_turn.to_f) * TWO_PI)).abs
    step = total / (steps.ceil * (@step_angle < 0 ? -1 : 1))
    dist = x_norm(@distance, total)
    height = x_norm(@height, total)
    let do
      x = []
      y = []
      z = []
      tot = start
      Array.new((total / step).abs) do tot += step end.unshift(start).each do |angle|
        xy = cis(angle)
        d = envelope_interp(angle, dist)
        x << (d * xy.image)
        y << (d * xy.real)
        z << envelope_interp(angle, height)
      end
      sofar = 0.0
      dp = (0...x.length - 1).map do |i|
        xi = x[i]
        xf = x[i + 1]
        yi = y[i]
        yf = y[i + 1]
        zi = z[i]
        zf = z[i + 1]
        sofar += distance(xf - xi, yf - yi, zf - zi)
      end
      df = dp[-1]
      td = 0
      tp = (0...dp.length - 1).map do |i|
        vp = x_norm(@velocity, df)
        di = dp[i]
        df = dp[i + 1]
        vi = envelope_interp(di, vp)
        vf = envelope_interp(df, vp)
        td += (df - di) / ((vi + vf) * 2.0)
      end
      @rx = x
      @ry = y
      @rz = z
      tf = tp[-1]
      @rt = tp.map do |ti| ti / tf end
    end
    reset_transformation()
  end
end

#
# Snd menu module and classes
#

module Snd_menu_utils
  def update_label(lst)
    unless lst.empty?
      lst[0].call
      update_label(lst[1..-1])
    end
  end

  def string2compound(str)
    RXmStringCreateLocalized(str)
  end

  def change_label(widget, new_label)
    RXtSetValues(widget, [RXmNlabelString, string2compound(new_label)])
  end

  def make_dialog(*args)
    label          = get_args(args, :label, "")
    ok_callback    = get_args(args, :ok_callback, nil)
    help_callback  = get_args(args, :help_callback, nil)
    reset_callback = get_args(args, :reset_callback, nil)
    clear_callback = get_args(args, :clear_callback, nil)
    bl             = get_args(args, :button_labels,
                                  ["DoIt", "Reset", "Clear", "Dismiss", "Help"])
    new_dialog = RXmCreateTemplateDialog(main_widgets()[1], label,
                                         [RXmNokLabelString, string2compound(bl[0]),
                                          RXmNcancelLabelString, string2compound(bl[3]),
                                          RXmNhelpLabelString, string2compound(bl[4]),
                                          RXmNautoUnmanage, false,
                                          RXmNdialogTitle, string2compound(label),
                                          RXmNresizePolicy, RXmRESIZE_GROW,
                                          RXmNnoResize, false,
                                          RXmNbackground, basic_color(),
                                          RXmNtransient, false])
    [RXmDIALOG_HELP_BUTTON, RXmDIALOG_CANCEL_BUTTON, RXmDIALOG_OK_BUTTON].each do |button|
      RXtVaSetValues(RXmMessageBoxGetChild(new_dialog, button),
                     [RXmNarmColor, pushed_button_color(), RXmNbackground, basic_color()])
    end
    RXtAddCallback(new_dialog, RXmNokCallback, ok_callback)
    RXtAddCallback(new_dialog, RXmNcancelCallback,
                   lambda do |w, c, i| RXtUnmanageChild(new_dialog) end)
    RXtAddCallback(new_dialog, RXmNhelpCallback, help_callback)
    if reset_callback
      rs = RXtCreateManagedWidget(bl[1], RxmPushButtonWidgetClass, new_dialog,
                             [RXmNbackground, basic_color(),
                              RXmNarmColor, pushed_button_color()])
      RXtAddCallback(rs, RXmNactivateCallback, reset_callback)
    end
    if clear_callback
      rs = RXtCreateManagedWidget(bl[2], RxmPushButtonWidgetClass, new_dialog,
                             [RXmNbackground, basic_color(),
                              RXmNarmColor, pushed_button_color()])
      RXtAddCallback(rs, RXmNactivateCallback, clear_callback)
    end
    new_dialog
  end

  def add_sliders(dialog, sliders)
    mainform = RXtCreateManagedWidget("formd", RxmRowColumnWidgetClass, dialog,
                                      [RXmNleftAttachment, RXmATTACH_FORM,
                                       RXmNrightAttachment, RXmATTACH_FORM,
                                       RXmNtopAttachment, RXmATTACH_FORM,
                                       RXmNbottomAttachment, RXmATTACH_WIDGET,
                                       RXmNbottomWidget,
                                       RXmMessageBoxGetChild(dialog, RXmDIALOG_SEPARATOR),
                                       RXmNbackground, highlight_color(),
                                       RXmNorientation, RXmVERTICAL])
    sliders.map do |slider_data|
      title = string2compound(slider_data[0])
      low = slider_data[1]
      initial = slider_data[2]
      high = slider_data[3]
      func = slider_data[4]
      scale = slider_data[5]
      new_slider = if slider_data.length == 7
                     if slider_data[6] == :log
                       create_log_scale_widget(mainform, title, low, initial, high, scale)
                     else
                       create_semi_scale_widget(mainform, title, initial)
                     end
                   else
                     RXtCreateManagedWidget(slider_data[0], RxmScaleWidgetClass, mainform,
                                            [RXmNorientation, RXmHORIZONTAL,
                                             RXmNshowValue, true,
                                             RXmNminimum, (low * scale).round,
                                             RXmNmaximum, (high * scale).round,
                                             RXmNvalue, (initial * scale).round,
                                             RXmNdecimalPoints, case scale
                                                                when 1000
                                                                  3
                                                                when 100
                                                                  2
                                                                when 10
                                                                  1
                                                                else
                                                                  0
                                                                end,
                                             RXmNtitleString, title,
                                             RXmNbackground, basic_color()])
                   end
      if slider_data.length == 7
        RXtAddCallback(new_slider.first, RXmNvalueChangedCallback, func)
      else
        RXtAddCallback(new_slider, RXmNvalueChangedCallback, func)
      end
      new_slider
    end
  end

  def add_target(*args)
    mainform        = get_args(args, :widget, nil)
    target_callback = get_args(args, :target, nil)
    reset_callback  = get_args(args, :reset, nil)
    str             = get_args(args, :label_string, nil)
    labels          = get_args(args, :labels, [["entire sound", :sound, true],
                                               ["selection", :selection, false],
                                               ["between marks", :marks, false]])
    # labels: [[name, type, on|off], ...]
    RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, mainform,
                           [RXmNorientation, RXmHORIZONTAL,
                            RXmNseparatorType, RXmSHADOW_ETCHED_OUT,
                            RXmNbackground, basic_color()])
    rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, mainform,
                                [RXmNorientation, RXmHORIZONTAL,
                                 RXmNbackground, basic_color(),
                                 RXmNradioBehavior, true,
                                 RXmNradioAlwaysOne, true,
                                 RXmNentryClass, RxmToggleButtonWidgetClass,
                                 RXmNisHomogeneous, true])
    labels.each do |name, type, on|
      RXtCreateManagedWidget(name, RxmToggleButtonWidgetClass, rc,
                             [RXmNbackground, basic_color(),
                              RXmNset, on,
                              RXmNselectColor, yellow_pixel(),
                              RXmNindicatorType, RXmONE_OF_MANY_ROUND,
                              RXmNarmCallback,
                              [lambda do |w, c, i| target_callback.call(type) end, false]])
    end
    if reset_callback
      RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, mainform,
                             [RXmNorientation, RXmHORIZONTAL])
      trbutton = RXtCreateManagedWidget("reset all entries", RxmToggleButtonWidgetClass, mainform,
                                        [RXmNbackground, basic_color(),
                                         RXmNset, true,
                                         RXmNselectColor, yellow_pixel()])
      RXtAddCallback(trbutton, RXmNvalueChangedCallback,
                     lambda do |w, c, i| reset_callback.call(Rset(i)) end)
    end
  end

  def add_label(mainform, str)
    RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, mainform,
                           [RXmNorientation, RXmHORIZONTAL,
                            RXmNseparatorType, RXmSHADOW_ETCHED_OUT,
                            RXmNbackground, basic_color()])
    rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, mainform,
                                [RXmNorientation, RXmHORIZONTAL,
                                 RXmNbackground, basic_color()])
    RXtCreateManagedWidget(str, RxmLabelWidgetClass, rc,
                           [RXmNbackground, basic_color(),
                            RXmNlabelString, string2compound(str)])
  end

  def add_text_widget(mainform, value)
    RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, mainform,
                           [RXmNorientation, RXmHORIZONTAL,
                            RXmNseparatorType, RXmSHADOW_ETCHED_OUT,
                            RXmNbackground, basic_color()])
    rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, mainform,
                                [RXmNorientation, RXmHORIZONTAL,
                                 RXmNbackground, basic_color()])
    txt = RXtCreateManagedWidget("text", RxmTextFieldWidgetClass, rc,
                                 [RXmNvalue, value,
                                  RXmNresizeWidth, 0,
                                  RXmNcolumns, 80,
                                  RXmNbackground, basic_color()])
    RXtAddCallback(txt, RXmNfocusCallback,
                   lambda do |w, c, i| RXtSetValues(w, [RXmNbackground, text_focus_color()]) end)
    RXtAddCallback(txt, RXmNlosingFocusCallback,
                   lambda do |w, c, i| RXtSetValues(w, [RXmNbackground, basic_color()]) end)
    RXtAddEventHandler(txt, REnterWindowMask, false,
		       lambda do |w, c, i, f| RXmProcessTraversal(w, RXmTRAVERSE_CURRENT) end)
    txt
  end
end if $IN_SND and $HAVE_MOTIF

class Dlocsig_menu
  include Snd_menu_utils

  def initialize(*args)
    @menu  = get_args(args, :menu, nil)
    @label = get_args(args, :label, nil)
    @amount = []
    @target = :collect
    @plot = :trajectory
    @dialog = nil
    @current = []
    cascade(*args)
  end

  def cascade(*args)
    d3    = get_args(args, :d3, true)
    error = get_args(args, :error, 0.01)
    dlocsig_menu_list = []
    dlocsig_menu = RXmCreatePulldownMenu(main_menu(@menu), @label,
                                         [RXmNbackground, basic_color()])
    dlocsig_cascade = RXtCreateManagedWidget(@label, RxmCascadeButtonWidgetClass,
                                             main_menu(@menu),
                                             [RXmNsubMenuId, dlocsig_menu,
                                              RXmNbackground, basic_color()])
    RXtAddCallback(dlocsig_cascade, RXmNcascadingCallback,
                   lambda do |w, c, i| update_label(dlocsig_menu_list) end)

    child = RXtCreateManagedWidget(@label, RxmPushButtonWidgetClass, dlocsig_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_dlocsig_dialog(d3, error) end)
    dlocsig_menu_list << lambda do
      change_label(child, format("%s (%s)", @label, @current.inspect))
    end
  end

  def set_sensitive(name = @label, flag = true)
    set_menu_sensitive(@menu, name, flag)
  end
  
  def toggle_sensitive(name = @label)
    set_sensitive(@menu, name, (not menu_sensitive(@menu, name)))
  end
  
  def post_dlocsig_dialog(d3, error)
    unless RWidget?(@dialog)
      initial_dlocsig_amount = 0
      sliders = []
      old_target = nil
      text_widget = nil
      set_value = lambda do
        RXmTextSetString(text_widget, @current.inspect.to_s)
      end
      get_value = lambda do
        ary = []
        str = RXmTextGetString(text_widget)
        while m = /\[+([-+\d\s.,]*)\]+/.match(str)
          ary << $1.split(/,/).map do |x| x.to_f end
          str = ((n = m.end(1)) ? str[n..-1] : nil)
        end
        ary = ary.delete_if do |x| x.empty? end
        nary = Array.new(ary.length) do Array.new((d3 ? 4 : 3), 0) end
        ary.each_with_index do |x, i| x.each_with_index do |y, j| nary[i][j] = y end end
        @current = nary.dup
      end
      test_value = lambda do
        ary = @current.dup
        # no negative velocity values are allowed
        vset = false
        @current.each do |x|
          x[-1] = 0 if x[-1] < 0
          vset = true if x[-1] > 0
        end
        # to prevent division by zero in render_path
        if vset and @current[-1][-1].zero?
          @current[-1][-1] = 0.1
        end
        res = true
        ary.flatten.each do |x| res = false if x.nonzero? end
        if res
          error("path contains only zeros (%s)", ary.inspect)
          false
        elsif ary.length < 3
          error("to draw a path at least three points are recommended (%d)", ary.length)
          false
        else
          true
        end
      end
      reset_value = lambda do
        @amount = Array.new(d3 ? 4 : 3, 0)
        @amount.length.times do |i| RXmScaleSetValue(sliders[i], (@amount[i] * 10).to_i) end
        set_value.call
      end
      @dialog = make_dialog(:label, @label,
                            :ok_callback, lambda do |w, c, i|
                              case @target
                              when :collect
                                get_value.call
                                @current << @amount
                                set_value.call
                                reset_value.call
                              when :sndplot
                                get_value.call
                                if test_value.call
                                  case @plot
                                  when :trajectory
                                    @current.to_path(:error, error,
                                                     :d3, d3).snd_trajectory()
                                  when :velocity
                                    @current.to_path(:error, error,
                                                     :d3, d3).snd_velocity()
                                  when :doppler
                                    @current.to_path(:error, error,
                                                     :d3, d3).snd_doppler()
                                  when :acceleration
                                    @current.to_path(:error, error,
                                                     :d3, d3).snd_acceleration()
                                  when :all
                                    @current.to_path(:error, error,
                                                     :d3, d3).snd_plot()
                                  end
                                end
                              when :gnuplot
                                get_value.call
                                if test_value.call
                                  case @plot
                                  when :trajectory
                                    @current.to_path(:error, error,
                                                     :d3, d3).plot_trajectory()
                                  when :velocity
                                    @current.to_path(:error, error,
                                                     :d3, d3).plot_velocity()
                                  when :doppler
                                    @current.to_path(:error, error,
                                                     :d3, d3).plot_doppler()
                                  when :acceleration
                                    @current.to_path(:error, error,
                                                     :d3, d3).plot_acceleration()
                                  when :all
                                    @current.to_path(:error, error,
                                                     :d3, d3).pplot()
                                  end
                                end
                              when :set
                                get_value.call
                                $path = @current.dup
                              when :edit
                                set_value.call
                                @target = old_target
                              end
                            end,
                            :reset_callback, lambda do |w, c, i|
                              reset_value.call
                            end,
                            :clear_callback, lambda do |w, c, i|
                              @current = []
                              reset_value.call
                            end,
                            :help_callback, lambda do |w, c, i|
                              help_dialog(@label,
                                          "Move the sliders to set the \
path.  To draw a path it should contain at least three points.

DoIt: o `collect' set: Takes the current slider values of x, y, " +
(d3 ?  "z, " : "" ) + "and velocity and collects them to the path.

      o `sndplay' set: Takes the collected list of lists and draws the \
given `trajectory', `velocity', `doppler', `acceleration', or all \
curves in Snd.

      o `gnuplot' set: The same as above but drawing is done by the \
external `gnuplot' program.

      o `set $path': Sets the global variable $path to use it by \
with_sound() etc. in the listener.

Reset: Resets the sliders.

Clear: Clears the list of lists.")
                            end)
      sliders = add_sliders(@dialog,
                            if d3
                              [["x coordinate", -20, initial_dlocsig_amount, 20,
                                lambda do |w, c, i| @amount[0] = Rvalue(i) / 10.0 end, 10],
                               ["y coordinate", -20, initial_dlocsig_amount, 20,
                                lambda do |w, c, i| @amount[1] = Rvalue(i) / 10.0 end, 10],
                               ["z coordinate", -20, initial_dlocsig_amount, 20,
                                lambda do |w, c, i| @amount[2] = Rvalue(i) / 10.0 end, 10],
                               ["velocity", 0, initial_dlocsig_amount, 20,
                                lambda do |w, c, i| @amount[3] = Rvalue(i) / 10.0 end, 10]]
                            else
                              [["x coordinate", -20, initial_dlocsig_amount, 20,
                                lambda do |w, c, i| @amount[0] = Rvalue(i) / 10.0 end, 10],
                               ["y coordinate", -20, initial_dlocsig_amount, 20,
                                lambda do |w, c, i| @amount[1] = Rvalue(i) / 10.0 end, 10],
                               ["velocity", 0, initial_dlocsig_amount, 20,
                                lambda do |w, c, i| @amount[2] = Rvalue(i) / 10.0 end, 10]]
                            end)
      add_label(RXtParent(sliders[0]), "current path")
      text_widget = add_text_widget(RXtParent(sliders[0]), @current.inspect.to_s)
      RXtAddCallback(text_widget, RXmNactivateCallback,
                     lambda do |w, c, i|
                       old_target = @target
                       @target = :edit
                       get_value.call
                     end)
      add_target(:widget, RXtParent(sliders[0]),
                 :target, lambda do |val| @plot = val end,
                 :labels, [["trajectory", :trajectory, true,],
                           ["velocity", :velocity, false],
                           ["doppler", :doppler, false],
                           ["acceleration", :acceleration, false],
                           ["plot all", :all, false]])
      add_target(:widget, RXtParent(sliders[0]),
                 :target, lambda do |val| @target = val end,
                 :labels, [["collect", :collect, true,],
                           ["sndplot", :sndplot, false],
                           ["gnuplot", :gnuplot, false],
                           ["set $path", :set, false]])
      reset_value.call
    end
    activate_dialog(@dialog)
  end
  private :post_dlocsig_dialog
end if $IN_SND and $HAVE_MOTIF

class Snd_menu
  doc "#{self.class} #{self.name}
Usage: menu = Snd_menu.new('main name')                   # main menu entry
       menu.add_simple('sub name1') { |menu, name| ... }  # simple sub menu entry
       menu.add_cascade('sub name2', args) { |args|       # cascade sub menu entry
          Dlocsig.new(*args)
       }\n"

  include Snd_menu_utils
  
  def initialize(name)
    @menu_list = []
    @snd_menu = add_to_main_menu(name, lambda do || update_label(@menu_list) end)
  end

  def add_simple(name, sensitive = true)
    if block_given?
      add_to_menu(@snd_menu, name, lambda do || yield(@snd_menu, name) end)
      set_menu_sensitive(@snd_menu, name, sensitive)
    else
      die("No block given! Usage: m.add_simple('name'[, sensitive]) { |menu, name| ... }\n")
    end
  end

  def add_cascade(name, *args)
    if block_given?
      yield(:menu, @snd_menu, :label, name, *args)
    else
      die("No block given! Usage: m.add_cascade('name'[, args]) { |args| ... }\n")
    end
  end
end if $IN_SND and $HAVE_MOTIF

#
# Sample functions (see clm-2/dlocsig/move-sound.ins and
# clm-2/dlocsig/dlocsig.html)
#

def sinewave(start, dur, freq, amp, *args)
  amp_env = get_args(args, :amp_env, [0, 1, 1, 1])
  path    = get_args(args, :path, [-10, 10, 0, 5, 10, 10].to_path)
  dloc, beg, len = make_dlocsig(start, dur, :path, path)
  osc = make_oscil(:frequency, freq)
  aenv = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
  beg.upto(len) do |i| dlocsig(dloc, i, env(aenv) * oscil(osc)) end
end

def move(start, file, *args)
  amp   = get_args(args, :amp, 1.0)
  paths = get_args(args, :paths, [])
  dur = mus_sound_duration(file)
  sr = mus_sound_srate(file)
  chns = mus_sound_chans(file)
  npaths = [paths.length, chns].min
  inary = Array.new(npaths) do |f| make_readin(:file, file, :channel, f) end
  loc = Array.new(npaths)
  beg = Array.new(npaths)
  fin = Array.new(npaths)
  min_beg = max_end = 0
  npaths.times do |d|
    loc[d], dbeg, dend = make_dlocsig(start, dur, :scaler, amp, :path, paths.shift)
    beg[d] = dbeg
    fin[d] = dend
    min_beg = dbeg if min_beg.zero? or dbeg < min_beg
    max_end = dend if max_end.zero? or dend > max_end
  end
  min_beg.upto(max_end) do |i|
    npaths.times do |c|
      dlocsig(loc[c], i, readin(inary[c])) if i >= beg[c] and i <= fin[c]
    end
  end
end

def move_sound(*args)
  doc("move_sound(*args) { ... }
     :path,     nil
     :paths,    nil
     :srate,    $rbm_srate (#$rbm_srate)
     :channels, nil") if get_args(args, :help, false)
  path  = get_args(args, :path, nil)
  paths = get_args(args, :paths, nil)
  sr    = get_args(args, :srate, $rbm_srate)
  chns  = get_args(args, :channels, nil)
  if path or paths
    chns = (chns or (path ? 1 : paths.length))
    sound_let("to_move", :srate, sr, :channels, chns) do |tmp_file|
      yield
      message("Moving sound on %d channels... ", chns)
      move(0, tmp_file, :paths, (path ? [path] : paths))
    end
  else
    yield
  end
end

#
# NREV (see clm-2/dlocsig/dlocsig.lisp)
#

def dlocnrev(startime, dur, *args)
  reverb_factor = get_args(args, :reverb_factor, 1.09)
  lp_coeff      = get_args(args, :lp_coeff, 0.7)
  lp_out_coeff  = get_args(args, :lp_out_coeff, 0.85)
  output_scale  = get_args(args, :output_scale, 1.0)
  amp_env       = get_args(args, :amp_env, [0, 1, 1, 1])
  volume        = get_args(args, :volume, 1.0)
  beg = (startime * $rbm_srate).floor
  len = beg + (dur * $rbm_srate).floor
  env_a = make_env(:envelope, amp_env, :scaler, output_scale, :duration, dur)
  srscale = $rbm_srate / 25641.0
  val = 0
  dly_len = [1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19]
  dly_len.map! do |x|
    val = (srscale * x).floor
    val += 1 if val.modulo(2).zero?
    val += 2 until val.prime?
    val
  end
  comb1 = make_comb(reverb_factor * 0.822, dly_len[0])
  comb2 = make_comb(reverb_factor * 0.802, dly_len[1])
  comb3 = make_comb(reverb_factor * 0.773, dly_len[2])
  comb4 = make_comb(reverb_factor * 0.753, dly_len[3])
  comb5 = make_comb(reverb_factor * 0.753, dly_len[4])
  comb6 = make_comb(reverb_factor * 0.733, dly_len[5])
  low = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_a = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_b = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_c = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  low_d = make_one_pole(lp_out_coeff, lp_coeff - 1.0)
  allpass1 = make_all_pass(-0.7, 0.7, dly_len[6])
  allpass2 = make_all_pass(-0.7, 0.7, dly_len[7])
  allpass3 = make_all_pass(-0.7, 0.7, dly_len[8])
  allpass4 = make_all_pass(-0.7, 0.7, dly_len[10])  # 9 for stereo
  allpass5 = make_all_pass(-0.7, 0.7, dly_len[11])
  allpass6 = make_all_pass(-0.7, 0.7, dly_len[12])
  allpass7 = make_all_pass(-0.7, 0.7, dly_len[13])
  allpass8 = make_all_pass(-0.7, 0.7, dly_len[14])
  sample_a = sample_b = sample_c = sample_d = rev = outrev = 0.0
  beg.upto(len) do |i|
    rev = volume * env(env_a) * ina(i, $rbm_reverb)
    outrev = all_pass(allpass4,
                      one_pole(low,
                               all_pass(allpass3,
                                        all_pass(allpass2,
                                                 all_pass(allpass1,
                                                          comb(comb1, rev) +
                                                          comb(comb2, rev) +
                                                          comb(comb3, rev) +
                                                          comb(comb4, rev) +
                                                          comb(comb5, rev) +
                                                          comb(comb6, rev))))))
    sample_a = output_scale * one_pole(low_a, all_pass(allpass5, outrev))
    sample_b = output_scale * one_pole(low_b, all_pass(allpass6, outrev))
    sample_c = output_scale * one_pole(low_c, all_pass(allpass7, outrev))
    sample_d = output_scale * one_pole(low_d, all_pass(allpass8, outrev))
    outa(i, ($rbm_channels == 2 ? ((sample_a + sample_d) / 2.0) : sample_a), $rbm_output)
    if $rbm_channels == 2 or $rbm_channels == 4
      outb(i, ($rbm_channels == 2 ? ((sample_b + sample_c) / 2.0) : sample_b), $rbm_output)
    end
    if $rbm_channels == 4
      outc(i, sample_c, $rbm_output)
      outd(i, sample_d, $rbm_output)
    end
  end
end

=begin
# Examples:

(with_sound(:channels, 6, :play, true, :statistics, true, :output, "rdloc06.snd") do
   path = make_path(:path, [[-10, 10], [0.5, 0.5], [10, 10]], :error, 0.001)
   sinewave(0, 10, 440, 0.5, :path, path)
 end)

(with_sound(:channels, 4, :play, true, :statistics, true, :output, "rdlocspiral.snd") do
   path = [make_spiral_path(:start_angle, 0, :turns, 2.5),
           make_spiral_path(:start_angle, 180, :turns, 3.5)]
   move(0, "/usr/gnu/sound/SFiles/bell.snd", :paths, path)
 end)

(let do
   path = [[-10, 10, 0, 0], [0, 5, 0, 1], [10, 10, 0, 0]].to_path(:error, 0.01)
   path.plot_velocity
 end)

(with_sound(:channels, 4, :play, true, :statistics, true,
            :reverb, :dlocnrev, :delay_time, 3.0,
            :reverb_channels, 4, :output, "rdlocnrev.snd") do
   path = make_path([[-10, 10, 0, 1], [-7, 7, 0, 0.9], [0, 5, 0, 0],
                     [7, 7, 0, 0.2], [10, 10, 0, 1]], :error, 0.001)
   sinewave(0, 2, 880, 0.5, :path, path)
 end)

(with_sound(:channels, 4, :play, 1, :statistics, true, :output, "rdlocmove.snd") do
   move_sound(:path, make_path([[-10, 10], [0.1, 0.1], [10, -10]])) do
     fm_violin(0, 1, 440, 0.1)
     fm_violin(0.3, 2, 1020, 0.05)
   end
 end)
=end

# dlocsig.rb ends here
