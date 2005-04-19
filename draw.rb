# draw.rb -- draw.scm --> draw.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Apr 05 00:17:04 CEST 2005
# Last: Mon Apr 11 03:19:34 CEST 2005

# Commentary:
#
# examples of extensions to Snd's graphics
# 
# module Draw
#  display_colored_samples(color, beg, dur, snd = false, chn = false)
#  display_samples_in_color(snd, chn)
#  color_samples(color, beg = 0, dur = false, snd = snd_snd, chn = snd_chn)
#  uncolor_samples(snd = snd_snd, chn = snd_chn)
#  display_previous_edits(snd, chn)
#  overlay_sounds(*rest)
#  samples_via_colormap(snd, chn)
#
#  class Current_window
#   initialize
#   update_location(snd)
#   display_location(snd, chn)
#   click_location(snd, chn, button, state, x, y, axis)
#
#  make_current_window_display
#  close_current_window_display
#  smart_line_cursor(snd, chn, ax)
#  click_for_listener_help(pos)
#  

# Code:

require "examp"
require "extensions"

module Draw
  add_help(:display_colored_samples,
           "display_colored_samples(color, beg, dur, [snd=false, [chn=false]])  \
displays samples from beg for dur in color whenever they\'re in the current view.")
  def display_colored_samples(color, beg, dur, snd = false, chn = false)
    unless array?(color) and number?(beg) and number?(dur)
      return
    end
    left = left_sample(snd, chn)
    right = right_sample(snd, chn)
    len = beg + dur
    old_color = foreground_color(snd, chn)
    if left < len and right > beg
      if vct?(data = make_graph_data(snd, chn))
        samps = [right, len].min - [left, beg].max
        offset = [0, beg - left].max
        new_data = data.subseq(offset, offset + samps)
        set_foreground_color(color, snd, chn)
        graph_data(new_data, snd, chn, Copy_context, [beg, left].max, [len, right].min)
        set_foreground_color(old_color, snd, chn)
      else
        low_data, high_data = data[0, 2]
        size = low_data.length
        samps = right - left
        left_offset = [0, beg - left].max
        left_bin = ((size.to_f * left_offset) / samps).floor
        right_offset = [len, right].min - left
        right_bin = ((size.to_f * right_offset) / samps).floor
        new_low_data = low_data.subseq(left_bin, right_bin)
        new_high_data = high_data.subseq(left_bin, right_bin)
        set_foreground_color(color, snd, chn)
        graph_data([new_low_data, new_high_data], snd, chn, Copy_context, left_bin, right_bin)
        set_foreground_color(old_color, snd, chn)
      end
    end
  end

  def display_samples_in_color(snd, chn)
    col, beg, dur = channel_property(:colored_samples, snd, chn)
    display_colored_samples(col, beg, dur, snd, chn)
  end

  add_help(:color_samples,
           "color_samples(color, [beg=0, [dur=false, [snd=false, [chn=false]]]])  \
causes samples from beg to beg+dur to be displayed in color")
  def color_samples(color, beg = 0, dur = false, snd = snd_snd, chn = snd_chn)
    unless $after_graph_hook.member?("display-samples-in-color")
      $after_graph_hook.add_hook!("display-samples-in-color") do |snd, chn|
        display_samples_in_color(snd, chn)
      end
    end
    unless dur then dur = frames(snd, chn) - beg end
    set_channel_property(:colored_samples, [color, beg, dur], snd, chn)
    update_time_graph(snd, chn)
  end

  add_help(:uncolor_samples,
           "uncolor_samples([snd=false, [chn=false]]) cancels sample coloring in the given channel")
  def uncolor_samples(snd = snd_snd, chn = snd_chn)
    set_channel_property(:colored_samples, [], snd, chn)
    update_time_graph(snd, chn)
  end

  add_help(:display_previous_edits,
           "display_previous_edits(snd, chn)  \
displays all edits of the current sound, with older versions gradually fading away")
  def display_previous_edits(snd, chn)
    edits = edit_position(snd, chn)
    old_color = foreground_color(snd, chn)
    clist = color2list(old_color)
    r = clist[0]
    g = clist[1]
    b = clist[2]
    rinc = (1.0 - r) / (edits + 1)
    ginc = (1.0 - g) / (edits + 1)
    binc = (1.0 - b) / (edits + 1)
    if edits > 0
      re = 1.0 - rinc
      ge = 1.0 - ginc
      be = 1.0 - binc
      0.upto(edits) do |pos|
        data = make_graph_data(snd, chn, pos)
        set_foreground_color(make_color(re, ge, be), snd, chn)
        graph_data(data, snd, chn)
        re -= rinc
        ge -= ginc
        be -= binc
      end
      set_foreground_color(old_color, snd, chn)
    end
  end

  add_help(:overlay_sounds,
           "overlay_sounds(*rest)  \
overlays onto its first argument all subsequent arguments: overlay_sounds(1, 0, 3)")
  def overlay_sounds(*rest)
    base = rest.shift
    $after_graph_hook.add_hook!(get_func_name) do |snd, chn|
      if sound?(base) and snd == base
        rest.each do |snd|
          if sound?(snd) and channels(snd) > chn and channels(base) > chn
            graph_data(make_graph_data(snd, chn), base, chn, Copy_context, -1, -1, Graph_dots)
          end
        end
      end
    end
  end

  add_help(:samples_via_colormap,
           "samples_via_colormap(snd, chn)  \
displays time domain graph using current colormap (just an example of colormap-ref)")
  def samples_via_colormap(snd, chn)
    left = left_sample(snd, chn)
    right = right_sample(snd, chn)
    old_color = foreground_color(snd, chn)
    data, ignore = make_graph_data(snd, chn)
    samps = right - left
    x0 = x2position(left / srate())
    y0 = y2position(data[0])
    colors = make_array(colormap_size)
    j = 1
    (left + 1).upto(left + data.length - 1) do |i|
      x1 = x2position(i / srate)
      y1 = y2position(data[j])
      x = data[j].abs
      ref = (colormap_size * x).floor
      unless colors[ref]
        colors[ref] = make_color(*colormap_ref(colormap, x))
      end
      set_foreground_color(colors[ref], snd, chn)
      draw_line(x0, y0, x1, y1)
      x0, y0 = x1, y1
      j += 1
    end
    set_foreground_color(old_color, snd, chn)
  end

  # inset overall waveform; if click, move to that location

  class Current_window
    def initialize
      @inset_width = 0.2
      @inset_heigth = 0.25
    end
    attr_reader :inset_width, :inset_heigth

    def update_location(snd)
      channels(snd).times do |chn|
        if vals = channel_property(:inset_envelope, snd, chn)
          vals[2] = -2
        end
      end
      false
    end

    def display_location(snd, chn)
      if time_graph?(snd, chn)
        axinf = axis_info(snd, chn)
        grf_width = axinf[12]
        width = (Float(@inset_width) * grf_width).round
        x_offset = (grf_width - width).to_i
        grf_height = axinf[11] - axinf[13]
        height = (Float(@inset_heigth) * grf_height).round
        chan_offset = axinf[13] - 10
        y_offset = chan_offset + (height / 2.0).round
        grf_chn = (channel_style(snd) == Channels_separate ? chn : 0)
        new_peaks = axinf[18]
        data0 = false
        data1 = false
        if width > 10 and height > 10 and frames(snd, chn) > 0 and
            (chn == 0 or channel_style(snd) != Channels_superimposed)
          fill_rectangle(x_offset, chan_offset + height, width, 2, snd, grf_chn)
          fill_rectangle(x_offset, chan_offset, 2, height, snd, grf_chn)
          rx = (width * (Float(right_sample(snd, chn)) / frames(snd, chn))).round
          lx = (width * (Float(left_sample(snd, chn)) / frames(snd, chn))).round
          fill_rectangle(x_offset + lx, chan_offset, [1, rx - lx].max, height, snd, grf_chn,
                         Selection_context)
          if (old_env = channel_property(:inset_envelope, snd, chn)) and
              (not new_peaks) and
              width == old_env[0] and
              height == old_env[1] and
              y_offset == old_env[5] and
              edit_position(snd, chn) == old_env[2]
            data0, data1 = old_env[3, 2]
          else
            data = make_graph_data(snd, chn, Current_edit_position, 0, frames(snd, chn))
            data_max = (vct?(data) ? data.peak : data.map do |d| d.peak end.max)
            data_scaler = (data_max > 0.0 ? (height / (2 * data_max)) : 0.0)
            new_len = width * 2
            data_len = (vct?(data) ? data.length : data.first.length)
            step = data_len / width
            if data_len > width
              data0 = make_array(new_len)
              data1 = ((not vct?(data)) and make_array(new_len))
              i = 0
              j = 0
              max_y = -data_max
              min_y = data_max
              stepper = 0.0
              until data_len == i or new_len == j
                if data1
                  max_y = [max_y, data[1][i]].max
                  min_y = [min_y, data[0][i]].min
                else
                  max_y = [max_y, data[i]].max
                end
                stepper += 1.0
                if stepper >= step
                  data0[j] = x_offset
                  data0[j + 1] = (y_offset - max_y * Float(data_scaler)).round
                  max_y = -data_max
                  if data1
                    data1[j] = x_offset
                    data1[j + 1] = (y_offset - min_y * Float(data_scaler)).round
                    min_y = data_max
                  end
                  x_offset += 1
                  stepper -= step
                  j += 2
                end
                i += 1
              end
              while j < new_len
                data0[j] = data0[j - 2]
                data0[j + 1] = data0[j - 1]
                if data1
                  data1[j] = data1[j - 2]
                  data1[j + 1] = data1[j - 1]
                end
                j += 2
              end
            else
              xstep = width / data_len
              data0 = make_array(data_len * 2)
              data1 = ((not vct?(data)) and make_array(data_len * 2))
              j = 0
              xj = x_offset
              data_len.times do |i|
                data0[j] = xj.round
                if data1
                  data0[j + 1] = (y_offset - data[1][i] * Float(data_scaler)).round
                  data1[j] = xj.floor
                  data1[j + 1] = (y_offset - data[0][i] * Float(data_scaler)).round
                else
                  data0[j + 1] = (y_offset - data[i] * Float(data_scaler)).round
                end
                j += 2
                xj += xstep
              end
            end
            set_channel_property(:inset_envelope,
                                 [width, height, edit_position(snd, chn), data0, data1, y_offset],
                                 snd, chn)
          end
          draw_lines(data0, snd, grf_chn)
          data1 and draw_lines(data1, snd, grf_chn)
        end
      end
    end

    def click_location(snd, chn, button, state, x, y, axis)
      if axis == Time_graph
        axinf = axis_info(snd, chn)
        grf_width = axinf[12]
        width = (Float(@inset_width) * grf_width).round
        x_offset = (grf_width - width).to_i
        grf_height = axinf[11] - axinf[13]
        height = (Float(@inset_heigth) * grf_height).round
        chan_offset = axinf[13] - 10
        y_offset = chan_offset + (height / 2.0).round
        if width > 0 and
            x.between?(x_offset, grf_width) and
            y.between?(chan_offset, chan_offset + height)
          samp = (Float(frames(snd, chn)) * ((x - Float(x_offset)) / width)).round
          ls = left_sample(snd, chn)
          rs = right_sample(snd, chn)
          set_cursor(samp, snd, chn)
          if samp.between?(ls, rs)
            rsamp = [[0, samp - (0.5 * (ls - rs)).round].max, frames(snd, chn) - 1].min
            set_right_sample(rsamp, snd, chn)
          end
          update_time_graph
          true
        else
          false
        end
      else
        false
      end
    end
  end

  def make_current_window_display
    hook_name = format("%s", Current_window)
    unless $after_graph_hook.member?(hook_name)
      cw = Current_window.new
      $after_open_hook.add_hook!(hook_name) do |snd|
        channels(snd).times do |chn|
          set_channel_property_save_state_ignore(:inset_envelope, snd, chn)
          undo_hook(snd, chn).add_hook!(get_func_name) do | |
            if vals = channel_property(:inset_envelope, snd, chn)
              vals[2] = -2
            end
          end
        end
      end
      $after_graph_hook.add_hook!(hook_name) do |snd, chn|
        cw.display_location(snd, chn)
      end
      $mouse_click_hook.add_hook!(hook_name) do |snd, chn, button, state, x, y, axis|
        cw.click_location(snd, chn, button, state, x, y, axis)
      end
      $update_hook.add_hook!(hook_name) do |snd|
        cw.update_location(snd)
      end
      true
    else
      false
    end
  end

  def close_current_window_display
    hook_name = format("%s", Current_window)
    if $after_graph_hook.member?(hook_name)
      $after_open_hook.remove_hook!(hook_name)
      $after_graph_hook.remove_hook!(hook_name)
      $mouse_click_hook.remove_hook!(hook_name)
      $update_hook.remove_hook!(hook_name)
      true
    else
      false
    end
  end

  add_help(:smart_line_cursor,
           "smart_line_cursor(snd, chn, ax)  \
is a cursor_style function that tries not to overwrite the thumbnail graph \
in the upper right corner")
  def smart_line_cursor(snd, chn, ax)
    cw = Current_window.new
    x, y = cursor_position
    x0, y0, x1, y1 = axis_info(snd, chn, ax)[10, 4]
    inset_x0 = x1 * (1.0 - cw.inset_width)
    inset_y0 = (y1 - 10.0) + (cw.inset_heigth * (y0 - y1))
    if x > (inset_x0 - 5)
      draw_line(x, y0, x, inset_y0 + 5, snd, chn, Cursor_context)
    else
      draw_line(x, y0, x, y1 - 5, snd, chn, Cursor_context)
    end
  end

  # click-for-listener-help

  $last_click_time = 0

  def click_for_listener_help(pos)
    help_moved = false
    time = Time.now.to_f
    click_time = time - $last_click_time
    $last_click_time = time
    if click_time < 25
      text = widget_text(main_widgets[4])
      subject = text[text.rindex(/\b/, pos)...text.index(/\b/, pos)]
      if help = snd_help(subject, false)
        help_dialog(subject, help)
      end
    end
  end
end

# $listener_click_hook.add_hook!("listener-help") do |pos| click_for_listener_help(pos) end

include Draw

# draw.rb ends here
