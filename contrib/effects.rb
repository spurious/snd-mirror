# effects.rb -- Guile -> Ruby translation

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Sa  Mär 01 05:32:19 CET 2003
# Version: $Revision: 1.5 $

# Requires the Motif module (xm.so) or --with-static-xm!

# Utilities
#  array?(ary)
#  to_rary(ary)
#
# module SNDMotif (see snd-motif.scm)
#  string2compound(str)
#  compound2string(xstr)
#  get_xtvalue(widget, item)
#  current_screen(doc)
#  white_pixel()
#  black_pixel()
#  screen_depth()
#  for_each_child(w, func)
#  find_child(widget, name)
#  display_widget_tree(widget)
#  add_channel_pane(snd, chn, name, type, args)
#  add_sound_pane(snd, name, type, args)
#  add_main_pane(name, type, args)
#  raise_dialog(w)
#  
# module XMEnved (see xm-enved.scm)
#  xe_envelope(drawer, new_env)
#  xe_create_enved(name, parent, args, axis_bounds)
#  xe_redraw(drawer)
#
# module Effects (see new-effects.scm)
#  update_label(effects)
#  all_chans()
#  plausible_mark_samples(doc)
#  map_chan_over_target_with_sync(func, target, origin, decay)
#  make_effect_dialog(label, ok_callback, help_callback, reset_callback)
#  change_label(widget, new_label)
#  scale_log2linear(lo, val, hi)
#  scale_linear2log(lo, val, hi)
#  scale_log_label(lo, val, hi)
#  create_log_scale_widget(parent, title, low, initial, high, scale)
#  semi_scale_label(val)
#  semitones2ratio(val)
#  ratio2semitones(ratio)
#  create_semi_scale_widget(parent, title, initial)
#  add_sliders(dialog, sliders)
#  yellow_pixel()
#  add_target(mainform, target_callback, truncate_callback)
#  activate_dialog(dialog)
#  effect_frames(target)
#  scale_envelope(e, scl)
#  post_gain_dialog()
#  post_normalize_dialog()
#  squelch_one_channel(silence, snd, chn)
#  post_gate_dialog()
#  post_echo_dialog()
#  flecho_1(scaler, secs, in_samps)
#  post_flecho_dialog()
#  zecho_1(scaler, secs, frq, amp, in_samps)
#  post_zecho_dialog()
#  post_band_pass_dialog()
#  post_notch_dialog()
#  post_high_pass_dialog()
#  post_low_pass_dialog()
#  comb_filter(scaler, size)
#  post_comb_dialog()
#  comb_chord(scaler, size, amp, interval_one, interval_two)
#  post_comb_chord_dialog()
#  moog(freq, q)
#  post_moog_dialog()
#  cp_adsat()
#  post_adsat_dialog()
#  post_src_dialog()
#  post_expsrc_dialog()
#  am_effect(freq)
#  post_am_effect_dialog()
#  rm_effect(freq, gliss_env)
#  post_rm_dialog()
#  post_reverb_dialog()
#  jc_reverb_1(in_samps)
#  post_jc_reverb_dialog()
#  cnvtest(snd0, snd1, amp)
#  post_convolve_dialog()
#  place_sound(mono_snd, stereo_snd, pan_env)
#  post_place_sound_dialog()
#  post_silence_dialog()
#  post_contrast_dialog()
#  cross_synthesis(cross_snd, amp, fftsize, r)
#  post_cross_synth_dialog()
#  post_flange_dialog()
#  post_random_phase_dialog()
#  fp_1(sr, osamp, osfrq, beg, fin)
#  post_robotize_dialog()
#  hello_dentist_1(frq, amp, beg, fin)
#  post_wobble_dialog()

require "English"
require "xm" unless $LOADED_FEATURES.grep(/xm/)
require "examp"

include Math

def array?(ary)
  ary.class == Array
end

def to_rary(ary)
  doc("to_rary(ary)
Gets an array of two arrays (produced e.g. by all_chans()) and returns
an rarray combined by that two.
to_rary([[0, 1, 2], [00, 11, 22]]) ==> [[0, 00], [1, 11], [2, 22]]\n") if ary == :help
  a = ary.first.dup
  b = ary.last.dup
  nary = []
  a.each_with_index do |x, i| nary << [x, b[i]] end
  nary
end

module SNDMotif
  doc "#{self.class} #{self.name}
Contains some definitions of snd-motif.scm.\n"

  def string2compound(str) RXmStringCreateLocalized(str); end

  def compound2string(xstr) RXmStringGetLtoR(xstr, "")[1]; end

  def get_xtvalue(widget, item) RXtVaGetValues(widget, [item, 0])[1]; end

  def current_screen(doc = nil)
    doc("current_screen()
Returns the current X screen number of the current display.\n") if doc == :help
    RDefaultScreenOfDisplay(RXtDisplay(main_widgets()[1]))
  end

  def white_pixel() RWhitePixelOfScreen(current_screen()); end
  
  def black_pixel() RBlackPixelOfScreen(current_screen()); end
  
  def screen_depth() RDefaultDepthOfScreen(current_screen()); end

  def for_each_child(w, func = nil)
    doc("for_each_child(w, func)
Applies FUNC to W and each of its children.\n") if w == :help
    func.call(w)
    get_xtvalue(w, RXmNchildren).each do |n| for_each_child(n, func) end if RXtIsComposite(w)
  end

  def find_child(widget, name = nil)
    doc("find_child(widget, name)
Returns a widget named NAME, if one can be found in the widget
hierachy beneath WIDGET.\n") if widget == :help
    callcc do |ret|
      for_each_child(widget, lambda { |child|
                                    ret.call(child) if RXtName(child) == name
                     })
    warn "find_child(#{name}): no such widget"
    end
  end

  def display_widget_tree(widget)
    doc("display_widget_tree(widget)
Displays the hierarchy of widgets beneath WIDGET.\n") if widget == :help
    display_widget = lambda do |w, spaces|
      name = RXtName(w)
      name = "<unnamed>" if not name or name.length == 0
      message("%s%s\n", spaces, name)
      if RXtIsComposite(w)
        get_xtvalue(w, RXmNchildren).each do |n| display_widget.call(n, spaces + "  ") end
      end
    end
    display_widget.call(widget, "")
  end

  def add_channel_pane(snd, chn = nil, name = nil, type = nil, args = nil)
    doc("add_channel_pane(snd, chn, name, type, args)
add our own pane to the channel section\n") if snd == :help
    RXtCreateManagedWidget(name, type, RXtParent(RXtParent(channel_widgets(snd, chn)[7])), args)
  end

  def add_sound_pane(snd, name = nil, type = nil, args = nil)
    doc("add_sound_pane(snd, name, type, args
add our own pane to the sound section (underneath the controls in this case)\n") if snd == :help
    RXtCreateManagedWidget(name, type, sound_widgets(snd)[0], args)
  end
  
  def add_main_pane(name, type = nil, args = nil)
    doc("add_main_pane(name, type, args)
add our own pane to the overall Snd window (underneath the listener in
this case)\n") if name == :help
    RXtCreateManagedWidget(name, type, (main_widgets()[5] or main_widgets()[3]), args)
  end

  def raise_dialog(w)
    doc("raise_dialog(w)
bring possibly-obscured dialog to top\n") if w == :help
    if RWidget?(w) and RXtIsManaged(w)
      parent = RXtParent(w)
      if RWidget?(parent) and RXtIsSubclass(parent, RxmDialogShellWidgetClass)
        RXtPopup(parent, RXtGrabNone)
      end
    end
  end
end

=begin
include SNDMotif
add_channel_pane(0, 0, "new_pane", RxmDrawingAreaWidgetClass, [RXmNbackground, graph_color(),
                                                               RXmNforeground, data_color()])
=end

module XMEnved
  doc "#{self.class} #{self.name}
Envelope editor (see xm-enved.scm and enved.scm) Works only with Motif
so far.  Contains all definitions of xm-enved.scm.\n"

  def xe_envelope(drawer, new_env = nil)
    doc("xe_envelope(drawer[, new_nev=nil]
setter: drawer = xe_envelope(drawer, new_env)
getter:          xe_envelope(drawer)\n") if drawer == :help
    if new_env                  # setter
      drawer[0] = new_env
      xe_redraw(drawer)
      drawer
    else                        # getter
      (drawer[0] or [drawer[3][0], drawer[3][1], drawer[3][2], drawer[3][1]])
    end
  end

  def xe_create_enved(name, parent, args, axis_bounds)
    xe_mouse_down = 0
    xe_mouse_up = 0
    xe_click_time = 10
    xe_mouse_pos = 0
    xe_mouse_new = false
    xe_add_envelope_point = lambda do |x, y, cur_env|
      new_env = []
      search_point = lambda do |e|
        if e.empty?
          new_env + [x, y]
        elsif e[0] == x
          new_env + [x, y] + e[2..-1]
        elsif e[0] > x
          new_env + [x, y] + e
        else
          new_env += [e[0], e[1]]
          search_point.call(e[2..-1])
        end
      end
      search_point.call(cur_env)
    end
    xe_edit_envelope_point = lambda do |pos, x, y, cur_env|
      new_env = []
      search_point = lambda do |e, npos|
        if npos == pos
          new_env + [x, y] + e[2..-1]
        else
          new_env += [e[0], e[1]]
          search_point.call(e[2..-1], npos + 2)
        end
      end
      search_point.call(cur_env, 0)
    end
    xe_remove_envelope_point = lambda do |pos, cur_env|
      new_env = []
      search_point = lambda do |e, npos|
        if e.empty?
          new_env
        elsif pos == npos
          new_env + e[2..-1]
        else
          new_env += [e[0], e[1]]
          search_point.call(e[2..-1], npos + 2)
        end
      end
      search_point.call(cur_env, 0)
    end
    xe_envelope_position = lambda do |x, cur_env|
      search_point = lambda do |e, pos|
        if e[0] == x
          pos
        else
          search_point.call(e[2..-1], pos + 2)
        end
      end
      search_point.call(cur_env, 0)
    end
    xe_on_dot_p = lambda do |x, y, cur_env, pos|
      xe_mouse_radius = 0.03
      (not cur_env.empty?) and
      ((((cur_env[0] - x).abs < xe_mouse_radius) and
        ((cur_env[1] - y).abs < xe_mouse_radius) and
        pos) or
       xe_on_dot_p.call(x, y, cur_env[2..-1], pos + 2))
    end
    xe_ungrfx = lambda do |drawer, x|
      bounds = drawer[3]
      locs = drawer[2]
      ax0 = bounds[0]
      ax1 = bounds[2]
      px0 = locs[0]
      px1 = locs[2]
      if px0 == px1
        ax0
      else
        [ax1, [ax0, ax0 + (ax1 - ax0) * (x - px0) / (px1 - px0).to_f].max].min
      end
    end
    xe_ungrfy = lambda do |drawer, y|
      bounds = drawer[3]
      locs = drawer[2]
      ay0 = bounds[1]
      ay1 = bounds[3]
      py0 = locs[1]
      py1 = locs[3]
      if py0 == py1
        ay1
      else
        [ay1, [ay0, ay0 + (ay1 - ay0) * (py0 - y) / (py0 - py1).to_f].max].min
      end
    end
    xe_mouse_press = lambda do |drawer, xx, yy|
      cur_env = xe_envelope(drawer)
      x = xe_ungrfx.call(drawer, xx)
      y = xe_ungrfy.call(drawer, yy)
      pos = xe_on_dot_p.call(x, y, cur_env, 0)
      xe_mouse_new = (not pos)
      xe_mouse_down = Time.now.to_f
      unless pos
        drawer = xe_envelope(drawer, xe_add_envelope_point.call(x, y, cur_env))
        xe_mouse_pos = xe_envelope_position.call(x, xe_envelope(drawer))
      else
        xe_mouse_pos = pos
      end
    end
    xe_mouse_drag = lambda do |drawer, xx, yy|
      cur_env = xe_envelope(drawer)
      x = xe_ungrfx.call(drawer, xx)
      y = xe_ungrfy.call(drawer, yy)
      ax_pix = drawer[2]
      lx = if xe_mouse_pos == 0
             cur_env[0]
           elsif xe_mouse_pos >= (cur_env.length - 2)
             cur_env[cur_env.length - 2]
           else
             [cur_env[xe_mouse_pos - 2], [x, cur_env[xe_mouse_pos + 2]].min].max
           end
      drawer = xe_envelope(drawer, xe_edit_envelope_point.call(xe_mouse_pos, lx, y, cur_env))
      xe_redraw(drawer)
    end
    xe_mouse_release = lambda do |drawer, xx, yy|
      cur_env = xe_envelope(drawer)
      x = xe_ungrfx.call(drawer, xx)
      y = xe_ungrfy.call(drawer, yy)
      ax_pix = drawer[2]
      xe_mouse_up = Time.now.to_f
      if (not xe_mouse_new) and
         ((xe_mouse_up - xe_mouse_down) <= xe_click_time) and
         (xe_mouse_pos != 0) and
         (xe_mouse_pos < (cur_env.length - 2))
        drawer = xe_envelope(drawer, xe_remove_envelope_point.call(xe_mouse_pos, cur_env))
      end
      xe_redraw(drawer)
      xe_mouse_new = false
    end
    args += [RXmNbackground, graph_color()] unless args.member?(RXmNbackground)
    args += [RXmNforeground, data_color()] unless args.member?(RXmNforeground)
    drawer = RXtCreateManagedWidget(name, RxmDrawingAreaWidgetClass, parent, args)
    gc = snd_gcs()[0]
    egc = snd_gcs()[7]
    x0 = axis_bounds[0]
    x1 = axis_bounds[1]
    y0 = axis_bounds[2]
    y1 = axis_bounds[3]
    editor = [[x0, y0, x1, y1], drawer, nil, [x0, y0, x1, y1], [gc, egc], name]
    RXtAddCallback(drawer, RXmNresizeCallback,
                   lambda do |w, c, i|
                     editor[2] = draw_axes(drawer, gc, name, x0, x1, y0, y1)
                     xe_redraw(editor)
                   end)
    RXtAddCallback(drawer, RXmNexposeCallback,
                   lambda do |w, c, i|
                     editor[2] = draw_axes(drawer, gc, name, x0, x1, y0, y1)
                     xe_redraw(editor)
                   end)
    RXtAddEventHandler(drawer, RButtonPressMask, false,
                       lambda do |w, c, e, f| xe_mouse_press.call(editor, Rx(e), Ry(e)) end)
    RXtAddEventHandler(drawer, RButtonMotionMask, false,
                       lambda do |w, c, e, f| xe_mouse_drag.call(editor, Rx(e), Ry(e)) end)
    RXtAddEventHandler(drawer, RButtonReleaseMask, false,
                       lambda do |w, c, e, f| xe_mouse_release.call(editor, Rx(e), Ry(e)) end)
    editor
  end
  
  def xe_redraw(drawer)
    cur_env = xe_envelope(drawer)
    widget = drawer[1]
    dpy = RXtDisplay(widget)
    wn = RXtWindow(widget)
    ax_pix = drawer[2]
    ax_inf = drawer[3]
    gc = drawer[4][0]
    egc = drawer[4][1]
    name = drawer[5]
    len = (array?(cur_env) and cur_env.length)
    if array?(ax_pix) and array?(cur_env) and RXtIsManaged(widget)
      px0 = ax_pix[0]
      px1 = ax_pix[2]
      py0 = ax_pix[1]
      py1 = ax_pix[3]
      ix0 = ax_inf[0]
      ix1 = ax_inf[2]
      iy0 = ax_inf[1]
      iy1 = ax_inf[3]
      mouse_d = 10
      mouse_r = 5
      xe_grfx = lambda do |x|
        if px0 == px1
          px0
        else
          [px1, [px0, (px0 + (px1 - px0) * (x - ix0) / (ix1 - ix0)).round].max].min
        end
      end
      xe_grfy = lambda do |y|
        if py0 == py1
          py0
        else
          [py0, [py1, (py1 + (py0 - py1) * (y - iy1) / (iy0 - iy1)).round].max].min
        end
      end
      if py0 > py1
        RXClearWindow(dpy, wn)
        draw_axes(widget, gc, name, ix0, ix1, iy0, iy1)
        lx = nil
        ly = nil
        0.step(len - 1, 2) do |i|
          cx = xe_grfx.call(cur_env[i])
          cy = xe_grfy.call(cur_env[i + 1])
          RXFillArc(dpy, wn, gc, cx - mouse_r, cy - mouse_r, mouse_d, mouse_d, 0, 360 * 64)
          RXDrawLine(dpy, wn, gc, lx, ly, cx, cy) if lx
          lx = cx
          ly = cy
        end
      end
    end
  end
end

=begin
include XMEnved
xe_create_enved("a name", add_main_pane("hiho", RxmFormWidgetClass, []),
                [RXmNleftAttachment, RXmATTACH_WIDGET,
                 RXmNtopAttachment, RXmATTACH_WIDGET,
                 RXmNbottomAttachment, RXmATTACH_WIDGET,
                 RXmNrightAttachment, RXmATTACH_WIDGET],
                [0.0, 1.0, 0.0, 1.0])
=end

module Effects
  doc "#{self.class} #{self.name}
Translation of new-effects.scm.  Contains all definitions of
new-effects.scm except the rubber widget.\n"
  
  @use_combo_box_for_fft_size = nil
  @effects_list = []

  include SNDMotif
  include XMEnved
  include Dsp
  include Moog

  def update_label(effects)
    unless effects.empty?
      effects[0].call
      update_label(effects[1..-1])
    end
  end

  @effects_menu = add_to_main_menu("Effects", lambda do || update_label(@effects_list) end)

  def all_chans
    sndlist = []
    chnlist = []
    sounds().each do |snd|
      (channels(snd) - 1).downto(0) do |i|
        sndlist.unshift(snd)
        chnlist.unshift(i)
      end
    end
    [sndlist, chnlist]
  end

  def plausible_mark_samples(doc = nil)
    doc("plausible_mark_samples()
find two marks in the current channel (in or nearest to current
window)\n") if doc == :help
    snd = selected_sound()
    chn = selected_channel()
    if marks(snd, chn)
      ms = marks(snd, chn).map do |x| mark_sample(x) end.sort
      if ms.length < 2
        raise "no_such_mark: mark-related action requires two marks"
      elsif ms.length == 2
        ms
      else
        lw = left_sample(snd, chn)
        rw = right_sample(snd, chn)
        cw = cursor(snd, chn)
        favor = if cw >= lw and cw <= rw
                  cw
                else
                  0.5 * (lw + rw)
                end
        centered_points = lambda do |points|
          if points.length == 2
            points
          else
            p1 = points[0]
            p2 = points[1]
            p3 = points[2]
            if (p1 - favor).abs < (p3 - favor).abs
              [p1, p2]
            else
              centered_points.call(points[1..-1])
            end
          end
        end
        centered_points.call(ms)
      end
    end
  end

  def map_chan_over_target_with_sync(func, target = nil, origin = nil, decay = nil)
    doc("map_chan_over_target_with_sync(func, target, origin, decay)
target: 'marks -> beg=closest marked sample, dur=samples to next mark
        'sound -> beg=0, dur=all samples in sound
        'selection -> beg=selection-position, dur=selection-frames
        'cursor -> beg=cursor, dur=samples to end of sound
decay is how long to run the effect past the end of the sound\n") if func == :help
    snc = sync()
    ssnd = selected_sound()
    schn = selected_channel()
    ms = (target == :marks and plausible_mark_samples())
    beg = case target
          when :sound
            0
          when :selection
            selection_position()
          when :cursor
            cursor(selected_sound, selected_channel)
          else
            ms[0]
          end
    overlap = (decay ? (srate() * decay).round : 0)
    to_rary((snc > 0 ? all_chans() : [[ssnd], [schn]])).each do |snd, chn|
      fin = if target == :sound or target == :cursor
              frames(snd, chn) - 1
            elsif target == :selection
              selection_position() + selection_frames()
            else
              ms[1]
            end
      if sync(snd) == snc
        map_chan(func.call(fin - beg), beg, fin + overlap, origin, snd, chn)
      end
    end
  end

  def make_effect_dialog(label, ok_callback, help_callback, reset_callback = nil)
    new_dialog = RXmCreateTemplateDialog(main_widgets()[1], label,
                                         [RXmNcancelLabelString, string2compound("Dismiss"),
                                          RXmNhelpLabelString, string2compound("Help"),
                                          RXmNokLabelString, string2compound("DoIt"),
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
    RXtAddCallback(new_dialog, RXmNcancelCallback,
                   lambda do |w, c, i|
                     RXtUnmanageChild(new_dialog)
                   end)
    RXtAddCallback(new_dialog, RXmNhelpCallback, help_callback)
    RXtAddCallback(new_dialog, RXmNokCallback, ok_callback)

    if reset_callback
      reset_button = RXtCreateManagedWidget("Reset", RxmPushButtonWidgetClass, new_dialog,
                                            [RXmNbackground, basic_color(),
                                             RXmNarmColor, pushed_button_color()])
      RXtAddCallback(reset_button, RXmNactivateCallback, reset_callback)
    end
    new_dialog
  end

  def change_label(widget, new_label)
    RXtSetValues(widget, [RXmNlabelString, string2compound(new_label)])
  end

  @log_scale_ticks = 500

  def scale_log2linear(lo, val = nil, hi = nil)
    doc("scale_log2linear(lo, val, hi)
given user-relative LOW..VAL..HI return VAL as scale-relative
(0..@log_scale_ticks)\n") if lo == :help
    log2 = log(2.0)
    log_lo = log([lo, 1.0].max) / log2
    log_hi = log(hi) / log2
    log_val = log(val) / log2
    (@log_scale_ticks * (log_val - log_lo) / (log_hi - log_lo)).round
  end

  def scale_linear2log(lo, val = nil, hi = nil)
    doc("scale_log2log(lo, val, ni)
given user-relative LO..HI and scale-relative VAL, return
user-relative VAL since log-scale widget assumes 0..@log_scale_ticks,
VAL can be used as ratio (log-wise) between LO and HI\n") if lo == :help
    log2 = log(2.0)
    log_lo = log([lo, 1.0].max) / log2
    log_hi = log(hi) / log2
    log_val = log_lo + ((val / @log_scale_ticks.to_f) * (log_hi - log_lo))
    2.0 ** log_val
  end

  def scale_log_label(lo, val, hi)
    "%.2f" % scale_linear2log(lo, val, hi)
  end

  def create_log_scale_widget(parent, title, low, initial, high, scale)
    label = RXtCreateManagedWidget("%.2f" % initial,
                                   RxmLabelWidgetClass, parent, [RXmNbackground, basic_color()])
    scale = RXtCreateManagedWidget("scale", RxmScaleWidgetClass, parent,
                                   [RXmNorientation, RXmHORIZONTAL,
                                    RXmNshowValue, false,
                                    RXmNminimum, 0,
                                    RXmNmaximum, @log_scale_ticks,
                                    RXmNvalue, scale_log2linear(low, initial, high),
                                    RXmNdecimalPoints, 0,
                                    RXmNtitleString, title,
                                    RXmNbackground, basic_color()])
    RXtAddCallback(scale, RXmNvalueChangedCallback,
                   lambda do |w, c, i|
                     change_label(label, scale_log_label(low, Rvalue(i), high))
                   end)
    RXtAddCallback(scale, RXmNdragCallback,
                   lambda do |w, c, i|
                     change_label(label, scale_log_label(low, Rvalue(i), high))
                   end)
    [scale, label]
  end

  @semi_range = 24

  def semi_scale_label(val)
    "semitones: %d" % (val - @semi_range)
  end

  def semitones2ratio(val)
    (2.0 ** val) / 12.0
  end

  def ratio2semitones(ratio)
    (12 * (log(ratio) / log(2.0))).round
  end

  def create_semi_scale_widget(parent, title, initial)
    label = RXtCreateManagedWidget("semitones: %d" % ratio2semitones(initial),
                                   RxmLabelWidgetClass, parent, [RXmNbackground, basic_color()])
    scale = RXtCreateManagedWidget("scale", RxmScaleWidgetClass, parent,
                                   [RXmNorientation, RXmHORIZONTAL,
                                    RXmNshowValue, false,
                                    RXmNminimum, 0,
                                    RXmNmaximum, 2 * @semi_range,
                                    RXmNvalue, @semi_range + ratio2semitones(initial),
                                    RXmNdecimalPoints, 0,
                                    RXmNtitleString, title,
                                    RXmNbackground, basic_color()])
    RXtAddCallback(scale, RXmNvalueChangedCallback,
                   lambda do |w, c, i| change_label(label, semi_scale_label(Rvalue(i))) end)
    RXtAddCallback(scale, RXmNdragCallback,
                   lambda do |w, c, i| change_label(label, semi_scale_label(Rvalue(i))) end)
    [scale, label]
  end

  def add_sliders(dialog, sliders = nil)
    doc("add_sliders(dialog, sliders)
sliders is a list of lists, each inner list being (title low initial
high callback scale ['log]) returns list of widgets (for reset
callbacks)\n") if dialog == :help
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

  def yellow_pixel
    pix = false
    unless pix
      shell = main_widgets()[1]
      dpy = RXtDisplay(shell)
      scr = RDefaultScreen(dpy)
      cmap = RDefaultColormap(dpy, scr)
      col = RXColor()
      if RXAllocNamedColor(dpy, cmap, "yellow", col, col) == 0
        warn "Color: can't allocate yellow!"
      else
        pix = Rpixel(col)
      end
    end
    pix
  end

  def add_target(mainform, target_callback = nil, truncate_callback = nil)
    doc("add_target(mainform, target_callback, truncate_callback)
add a set of 3 radio buttons at the bottom of the main section for
choice between sound, selection, between-marks
TARGET_CALLBACK should take one arg, a symbol: :sound, :selection,
:marks, and apply the effect accordingly (upon \"DoIt\")
TRUNCATE_CALLBACK (if any) takes one arg: boolean representing toggle
state (true = on)\n") if mainform == :help
    sep = RXtCreateManagedWidget("sep", RxmSeparatorWidgetClass, mainform,
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
    [["entire sound", :sound, true],
     ["selection", :selection, false],
     ["between marks", :marks, false]].each do |x|
      name = x[0]
      type = x[1]
      on = x[2]
      RXtCreateManagedWidget(name, RxmToggleButtonWidgetClass, rc,
                             [RXmNbackground, basic_color(),
                              RXmNset, on,
                              RXmNselectColor, yellow_pixel(),
                              RXmNindicatorType, RXmONE_OF_MANY_ROUND,
                              RXmNarmCallback,
                              [lambda do |w, c, i| target_callback.call(type) end, false]])
      end
    if truncate_callback
      trsep = RXtCreateManagedWidget("trsep", RxmSeparatorWidgetClass, mainform,
                                     [RXmNorientation, RXmHORIZONTAL])
      trbutton = RXtCreateManagedWidget("truncate at end", RxmToggleButtonWidgetClass, mainform,
                                        [RXmNbackground, basic_color(),
                                         RXmNset, true,
                                         RXmNselectColor, yellow_pixel()])
      RXtAddCallback(trbutton, RXmNvalueChangedCallback,
                     lambda do |w, c, i| truncate_callback.call(Rset(i)) end)
    end
  end

  def activate_dialog(dialog)
    RXtIsManaged(dialog) ? raise_dialog(dialog) : RXtManageChild(dialog)
  end

  def effect_frames(target)
    case target
    when :sound
      frames() - 1
    when :selection
      selection_frames()
    else
      y = plausible_mark_samples().shift
      plausible_mark_samples().each do |x| y -= x end
      1 + y.abs
    end
  end

  #
  # Begin Parametrized Effects
  #

  #
  # Amplitude Effects
  #
  amp_menu_list = []
  amp_menu = RXmCreatePulldownMenu(main_menu(@effects_menu), "Amplitude Effects",
                                   [RXmNbackground, basic_color()])
  amp_cascade = RXtCreateManagedWidget("Amplitude Effects", RxmCascadeButtonWidgetClass,
                                       main_menu(@effects_menu),
                                       [RXmNsubMenuId, amp_menu,
                                        RXmNbackground, basic_color()])
  RXtAddCallback(amp_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i| update_label(amp_menu_list) end)

  # Gain (gain set by @gain_amount)
  @gain_amount = 1.0
  @gain_label = "Gain"
  @gain_dialog = nil
  @gain_target = :sound
  @gain_envelope = nil

  def scale_envelope(e, scl)
    e.empty? ? [] : [e[0], scl * e[1]] + scale_envelope(e[2..-1], scl)
  end

  def post_gain_dialog
    unless RWidget?(@gain_dialog)
      initial_gain_amount = 1.0
      sliders = []
      fr = nil
      @gain_dialog =
      make_effect_dialog(@gain_label,
                         lambda do |w, c, i|
                           with_env = ((xe_envelope(@gain_envelope) != [0.0, 1.0, 1.0, 1.0]) and
                                       scale_envelope(xe_envelope(@gain_envelope), @gain_amount))
                           case @gain_target
                           when :sound
                             with_env ? env_sound(with_env) : scale_by(@gain_amount)
                           when :selection
                             if selection?()
                               with_env ? env_selection(with_env) :
                                                       scale_selection_by(@gain_amount)
                             else
                               warn "no selection"
                             end
                           else
                             pts = plausible_mark_samples()
                             if pts
                               if with_env
                                 env_sound(with_env, pts[0], pts[1] - pts[0])
                               else
                                 scale_sound_by(@gain_amount, pts[0], pts[1] - pts[0])
                               end
                             end
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@gain_label,
                                       "Move the slider to change the gain scaling amount")
                           end,
                         lambda do |w, c, i|
                           @gain_amount = initial_gain_amount
                           @gain_envelope = xe_envelope(@gain_envelope, [0.0, 1.0, 1.0, 1.0])
                           RXmScaleSetValue(sliders[0], (@gain_amount * 100).round)
                         end)
      sliders = add_sliders(@gain_dialog,
                            [["gain", 0.0, initial_gain_amount, 5.0,
                              lambda do |w, c, i| @gain_amount = Rvalue(i) / 100.0 end, 100]])
      fr = RXtCreateManagedWidget("fr", RxmFrameWidgetClass, RXtParent(sliders[0]),
                                  [RXmNheight, 200,
                                   RXmNshadowThickness, 4,
                                   RXmNshadowType, RXmSHADOW_ETCHED_OUT])
      add_target(RXtParent(sliders[0]), lambda do |target| @gain_target = target end, false)
      activate_dialog(@gain_dialog)
      @gain_envelope = xe_create_enved("gain", fr, [RXmNheight, 200], [0.0, 1.0, 0.0, 1.0])
      @gain_envelope = xe_envelope(@gain_envelope, [0.0, 1.0, 1.0, 1.0])
    else
      activate_dialog(@gain_dialog)
    end
  end

  lambda do
    child = RXtCreateManagedWidget(@gain_label, RxmPushButtonWidgetClass, amp_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| post_gain_dialog() end)
    amp_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @gain_label, @gain_amount))
    end
  end.call

  # Normalize
  @normalize_amount = 1.0
  @normalize_label = "Normalize"
  @normalize_dialog = nil
  @normalize_target = :sound

  def post_normalize_dialog
    unless RWidget?(@normalize_dialog)
      initialize_normalize_amount = 1.0
      sliders = []
      @normalize_dialog =
      make_effect_dialog(@normalize_label,
                         lambda do |w, c, i|
                           case @normalize_target
                           when :sound
                             scale_to(@normalize_amount)
                           when :selection
                             selection?() ? scale_selection_to(@normalize_amount) :
                                                              warn("no selection")
                           else
                             pts = plausible_mark_samples()
                             scale_sound_to(@normalize_amount, pts[0], pts[1] - pts[0]) if pts
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@normalize_label,
                                       "Normalize scales amplitude to the normalize amount. \
Move the slider to change the scaling amount.")
                         end,
                         lambda do |w, c, i|
                           @normalize_amount = initialize_normalize_amount
                           RXmScaleSetValue(sliders[0], (@normalize_amount * 100).round)
                         end)
      sliders = add_sliders(@normalize_dialog,
                            [["normalize", 0.0, initialize_normalize_amount, 1.0,
                              lambda do |w, c, i| @normalize_amount = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @normalize_target = target end, false)
    end
    activate_dialog(@normalize_dialog)
  end

  lambda do
    child = RXtCreateManagedWidget(@normalize_label, RxmPushButtonWidgetClass, amp_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_normalize_dialog() end)
    amp_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @normalize_label, @normalize_amount))
    end
  end.call

  # Gate (gate set by @gate_amount
  @gate_amount = 1.0
  @gate_label = "Gate"
  @gate_dialog = nil
  @omit_silence = false

  def squelch_one_channel(silence, snd, chn)
    buffer_size = 128
    buffer0 = nil
    tmp = nil
    sum0 = 0.0
    buffer1 = make_vct(buffer_size)
    chan_samples = frames(snd, chn)
    pad_samples = chan_samples + buffer_size
    tempfilename = snd_tempnam()
    new_file = open_sound_file(tempfilename, 1, srate(snd))
    reader = make_sample_reader(0, snd, chn)
    buffers_per_progress_report = (chan_samples / buffer_size * 20).round
    start_progress_report(snd)
    j = 0
    0.step(pad_samples - 1, buffer_size) do |i|
      sum = 0.0
      (0...buffer_size).each do |j|
        val = next_sample(reader)
        vct_set!(buffer1, j, val)
        sum += val * val
      end
      if buffer0
        all_zeros = false
        if sum > silence
          if sum0 <= silence
            incr = 0.0
            (0...buffer_size).each do |j|
              vct_set!(buffer0, j, vct_ref(buffer0, j) * incr)
              incr += 1.0 / buffer_size
            end
          end
        else
          if sum0 <= silence
            vct_fill!(buffer0, 0.0)
            all_zeros = true
            incr = 1.0
            (0...buffer_size).each do |j|
              vct_set!(buffer0, j, vct_ref(buffer0, j) * incr)
              incr -= 1.0 / buffer_size
            end
          end
        end
        unless @omit_silence and all_zeros
          vct2sound_file(new_file, buffer0, buffer_size)
        end
      else
        buffer0 = make_vct(buffer_size)
      end
      j += 1
      if j >= buffers_per_progress_report
        j = 0
        progress_report(i / pad_samples, "squelch_one_channel", chn, 1, snd)
      end
      tmp = buffer0
      buffer0 = buffer1
      buffer1 = tmp
      sum0 = sum
    end
    finish_progress_report(snd)
    free_sample_reader(reader)
    close_sound_file(new_file, chan_samples * 4)
    set_samples(0, chan_samples, tempfilename, snd, chn)
  end

  def post_gate_dialog
    unless RWidget?(@gate_dialog)
      initial_gate_amount = 1.0
      sliders = []
      @gate_dialog =
      make_effect_dialog(@gate_label,
                         lambda do |w, c, i|
                           snc = sync()
                           if snc > 0
                             all_chans().each do |x|
                               snd = x[0]
                               chn = x[1]
                               if sync(snd) == snc
                                 squelch_one_channel(@gate_amount, snd, chn)
                               end
                             end
                           else
                             squelch_one_channel(@gate_amount, selected_sound, selected_channel)
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@gate_label,
                                       "Move the slider to change the gate intensity. \
Higher values gate more of the sound.")
                         end,
                         lambda do |w, c, i|
                           @gate_amount = initial_gate_amount
                           RXmScaleSetValue(sliders[0], (@gate_amount * 100).round)
                         end)
      sliders = add_sliders(@gate_dialog,
                            [["gate", 0.0, initial_gate_amount, 5.0,
                              lambda do |w, c, i| @gate_amount = Rvalue(i) / 100.0 end, 100]])
      toggle = RXtCreateManagedWidget("Omit silence", RxmToggleButtonWidgetClass,
                                      RXtParent(sliders[0]),
                                      [RXmNselectColor, pushed_button_color(),
                                       RXmNbackground, basic_color(),
                                       RXmNvalue, @omit_silence ? 1 : 0,
                                       RXmNlabelString, string2compound("Omit silence")])
      RXtAddCallback(toggle, RXmNvalueChangedCallback,
                     lambda do |w, c, i| @omit_silence = Rset(i) end)
    end
    activate_dialog(@gate_dialog)
  end

  lambda do
    child = RXtCreateManagedWidget(@gate_label, RxmPushButtonWidgetClass, amp_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback, lambda do |w, c, i| post_gate_dialog() end)
    amp_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @gate_label, @gate_amount))
    end
  end.call
  
  #
  # Delay Effects
  #
  delay_menu_list = []
  delay_menu = RXmCreatePulldownMenu(main_menu(@effects_menu), "Delay Effects",
                                     [RXmNbackground, basic_color()])
  delay_cascade = RXtCreateManagedWidget("Delay Effects", RxmCascadeButtonWidgetClass,
                                         main_menu(@effects_menu),
                                         [RXmNsubMenuId, delay_menu,
                                          RXmNbackground, basic_color()])
  RXtAddCallback(delay_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i| update_label(delay_menu_list) end)
  
  # Echo (controlled by @delay_time and @echo_amount)
  @delay_time = 0.5
  @echo_amount = 0.2
  @echo_label = "Echo"
  @echo_dialog = nil
  @echo_target = :sound
  @echo_truncate = true

  def post_echo_dialog
    unless RWidget?(@echo_dialog)
      initial_delay_time = 0.5
      initial_echo_amount = 0.2
      sliders = []
      @echo_dialog =
      make_effect_dialog(@echo_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |in_samps|
                                                            del = make_delay((@delay_time *
                                                                              srate()).round)
                                                            samp = 0
                                                            lambda do |inval|
                                                              samp += 1
                                                              inval + delay(del, @echo_amount *
                                                                            (tap(del) +
                                                                             (samp <= in_samps ?
                                                                              inval : 0.0)))
                                                            end
                                                          end,
                                                          @echo_target, "echo",
                                                          ((not @echo_truncate) and
                                                           4 * @delay_time))
                         end,
                         lambda do |w, c, i|
                           help_dialog(@echo_label,
                                       "The sliders change the delay time and echo amount.")
                         end,
                         lambda do |w, c, i|
                           @delay_time = initial_delay_time
                           RXmScaleSetValue(sliders[0], (@delay_time * 100).round)
                           @echo_amount = initial_echo_amount
                           RXmScaleSetValue(sliders[1], (@echo_amount * 100).round)
                         end)
      sliders = add_sliders(@echo_dialog,
                            [["delay time", 0.0, initial_delay_time, 2.0,
                              lambda do |w, c, i| @delay_time = Rvalue(i) / 100.0 end, 100],
                             ["echo amount", 0.0, initial_echo_amount, 1.0,
                              lambda do |w, c, i| @echo_amount = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]),
                 lambda do |target| @echo_target = target end,
                 lambda do |truncate| @echo_truncate = truncate end)
    end
    activate_dialog(@echo_dialog)
  end

  lambda do
    child = RXtCreateManagedWidget(@echo_label, RxmPushButtonWidgetClass, delay_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_echo_dialog() end)
    delay_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f)", @echo_label, @delay_time, @echo_amount))
    end
  end.call

  # Filtered echo
  @flecho_scaler = 0.5
  @flecho_delay = 0.9
  @flecho_label = "Filtered echo"
  @flecho_dialog = nil
  @flecho_target = :sound
  @flecho_truncate = true

  def flecho_1(scaler, secs, in_samps)
    flt = make_fir_filter(:order, 4, :xcoeffs, list2vct([0.125, 0.25, 0.25, 0.125]))
    del = make_delay((secs * srate()).round)
    samp = 0
    lambda do |inval|
      samp += 1
      inval + delay(del, fir_filter(flt, scaler * (tap(del) + (samp <= in_samps ? inval : 0.0))))
    end
  end

  def post_flecho_dialog
    unless RWidget?(@flecho_dialog)
      initial_flecho_scaler = 0.5
      initial_flecho_delay = 0.9
      sliders = []
      @flecho_dialog =
      make_effect_dialog(@flecho_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |in_samps|
                                                            flecho_1(@flecho_scaler,
                                                                     @flecho_delay,
                                                                     in_samps)
                                                          end,
                                                          @flecho_target, "flecho",
                                                          ((not @flecho_truncate) and
                                                           4 * @flecho_delay))
                         end,
                         lambda do |w, c, i|
                           help_dialog(@flecho_label,
                                       "Move the sliders to the filter scaler \
and the delay time in seconds.")
                         end,
                         lambda do |w, c, i|
                           @flecho_scaler = initial_flecho_scaler
                           RXmScaleSetValue(sliders[0], (@flecho_scaler * 100).round)
                           @flecho_delay = initial_flecho_delay
                           RXmScaleSetValue(sliders[1], (@flecho_delay * 100).round)
                         end)
      sliders = add_sliders(@flecho_dialog,
                            [["filter scaler", 0.0, initial_flecho_scaler, 1.0,
                              lambda do |w, c, i| @flecho_scaler = Rvalue(i) / 100.0 end, 100],
                             ["delay time (secs)", 0.0, initial_flecho_delay, 3.0,
                              lambda do |w, c, i| @flecho_delay = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]),
                 lambda do |target| @flecho_target = target end,
                 lambda do |truncate| @flecho_truncate = truncate end)
    end
    activate_dialog(@flecho_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@flecho_label, RxmPushButtonWidgetClass, delay_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_flecho_dialog() end)
    delay_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f)", @flecho_label, @flecho_scaler, @flecho_delay))
    end
  end.call

  # Modulated echo
  @zecho_scaler = 0.5
  @zecho_delay = 0.75
  @zecho_freq = 6
  @zecho_amp = 10.0
  @zecho_label = "Modulated echo"
  @zecho_dialog = nil
  @zecho_target = :sound
  @zecho_truncate = true

  def zecho_1(scaler, secs, frq, amp, in_samps)
    os = make_oscil(frq)
    len = (secs * srate()).round
    del = make_delay(len, false, 0.0, (len + amp + 1).round)
    samp = 0
    lambda do |inval|
      samp += 1
      inval + delay(del, scaler * (tap(del) + (samp <= in_samps ? inval : 0.0)), amp * oscil(os))
    end
  end

  def post_zecho_dialog
    unless RWidget?(@zecho_dialog)
      initial_zecho_scaler = 0.5
      initial_zecho_delay = 0.75
      initial_zecho_freq = 6
      initial_zecho_amp = 10.0
      sliders = []
      @zecho_dialog =
      make_effect_dialog(@zecho_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |in_samps|
                                                            zecho_1(@zecho_scaler,
                                                                    @zecho_delay,
                                                                    @zecho_freq,
                                                                    @zecho_amp,
                                                                    in_samps)
                                                          end,
                                                          @zecho_target, "zecho",
                                                          ((not @zecho_truncate) and
                                                           4 * @zecho_delay))
                         end,
                         lambda do |w, c, i|
                           help_dialog(@zecho_label,
                                       "Move the sliders to set the echo scaler, \
the delay time in seconds, the modulation frequency, and the echo amplitude.")
                         end,
                         lambda do |w, c, i|
                           @zecho_scaler = initial_zecho_scaler
                           RXmScaleSetValue(sliders[0], (@zecho_scaler * 100).round)
                           @zecho_delay = initial_zecho_delay
                           RXmScaleSetValue(sliders[1], (@zecho_delay * 100).round)
                           @zecho_freq = initial_zecho_freq
                           RXmScaleSetValue(sliders[2], (@zecho_freq * 100).round)
                           @zecho_amp = initial_zecho_amp
                           RXmScaleSetValue(sliders[3], (@zecho_amp * 100).round)
                         end)
      sliders = add_sliders(@zecho_dialog,
                            [["echo scaler", 0.0, initial_zecho_scaler, 1.0,
                              lambda do |w, c, i| @zecho_scaler = Rvalue(i) / 100.0 end, 100],
                             ["delay time (secs)", 0.0, initial_zecho_delay, 3.0,
                              lambda do |w, c, i| @zecho_delay = Rvalue(i) / 100.0 end, 100],
                             ["modulation frequency", 0.0, initial_zecho_freq, 100.0,
                              lambda do |w, c, i| @zecho_freq = Rvalue(i) / 100.0 end, 100],
                             ["modulation amplitude", 0.0, initial_zecho_amp, 100.0,
                              lambda do |w, c, i| @zecho_amp = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]),
                 lambda do |target| @zecho_target = target end,
                 lambda do |truncate| @zecho_truncate = truncate end)
    end
    activate_dialog(@zecho_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@zecho_label, RxmPushButtonWidgetClass, delay_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_zecho_dialog() end)
    delay_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f %1.2f %1.2f)", @zecho_label,
                                 @zecho_scaler, @zecho_delay, @zecho_freq, @zecho_amp))
    end
  end.call

  #
  # Filters
  #
  filter_menu_list = []
  filter_menu = RXmCreatePulldownMenu(main_menu(@effects_menu), "Filter Effects",
                                      [RXmNbackground, basic_color()])
  filter_cascade = RXtCreateManagedWidget("Filter Effects", RxmCascadeButtonWidgetClass,
                                          main_menu(@effects_menu),
                                          [RXmNsubMenuId, filter_menu,
                                           RXmNbackground, basic_color()])
  RXtAddCallback(filter_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i| update_label(filter_menu_list) end)

  # Butterworth band-pass filter
  @band_pass_freq = 1000
  @band_pass_bw = 100
  @band_pass_label = "Band-pass filter"
  @band_pass_dialog = nil
  @band_pass_target = :sound

  def post_band_pass_dialog
    unless RWidget?(@band_pass_dialog)
      initial_band_pass_freq = 1000
      initial_band_pass_bw = 100
      sliders = []
      @band_pass_dialog =
      make_effect_dialog(@band_pass_label,
                         lambda do |w, c, i|
                           case @band_pass_target
                           when :sound
                             filter_sound(make_butter_band_pass(@band_pass_freq, @band_pass_bw))
                           when :selection
                             filter_selection(make_butter_band_pass(@band_pass_freq,
                                                                    @band_pass_bw))
                           else
                             warn "can't apply band-pass between marks yet"
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@band_pass_label,
                                       "Butterworth band-pass filter.
Move the slider to change the center frequency and bandwidth.")
                         end,
                         lambda do |w, c, i|
                           @band_pass_freq = initial_band_pass_freq
                           RXmScaleSetValue(sliders[0][0],
                                            scale_log2linear(20, @band_pass_freq, 22050))
                           change_label(sliders[0][1], "%.2f" % @band_pass_freq)
                           @band_pass_bw = initial_band_pass_bw
                           RXmScaleSetValue(sliders[1], @band_pass_bw.round)
                         end)
      sliders = add_sliders(@band_pass_dialog,
                            [["center frequency", 20, initial_band_pass_freq, 22050,
                              lambda do |w, c, i|
                                @band_pass_freq = scale_linear2log(20, Rvalue(i), 22050)
                              end, 1, :log],
                             ["bandwidth", 0, initial_band_pass_bw, 1000,
                              lambda do |w, c, i| @band_pass_bw = Rvalue(i) end, 1]])
      add_target(RXtParent(sliders[0][0]), lambda do |target| @band_pass_target = target end, false)
    end
    activate_dialog(@band_pass_dialog)
  end

  lambda do
    child = RXtCreateManagedWidget(@band_pass_label, RxmPushButtonWidgetClass, filter_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_band_pass_dialog() end)
    filter_menu_list << lambda do
      change_label(child, format("%s (%1.2f %d)",
                                 @band_pass_label, @band_pass_freq, @band_pass_bw))
    end
  end.call

  # Butterworth band-reject (notch) filter
  @notch_freq = 100
  @notch_bw = 100
  @notch_label = "Band-reject filter"
  @notch_dialog = nil
  @notch_target = :sound

  def post_notch_dialog
    unless RWidget?(@notch_dialog)
      initial_notch_freq = 100
      initial_notch_bw = 100
      sliders = []
      @notch_dialog =
      make_effect_dialog(@notch_label,
                         lambda do |w, c, i|
                           case @notch_target
                           when :sound
                             filter_sound(make_butter_band_reject(@notch_freq, @notch_bw))
                           when :selection
                             filter_selection(make_butter_band_reject(@notch_freq, @notch_bw))
                           else
                             warn "can't apply notch between marks yet"
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@notch_label,
                                       "Butterworth band-reject filter.
Move the slider to change the center frequency and bandwidth.")
                         end,
                         lambda do |w, c, i|
                           @notch_freq = initial_notch_freq
                           RXmScaleSetValue(sliders[0][0],
                                            scale_log2linear(20, @notch_freq, 22050))
                           change_label(sliders[0][1], "%.2f" % @notch_freq)
                           @notch_bw = initial_notch_bw
                           RXmScaleSetValue(sliders[1], @notch_bw.round)
                         end)
      sliders = add_sliders(@notch_dialog,
                            [["center frequency", 20, initial_notch_freq, 22050,
                              lambda do |w, c, i|
                                @notch_freq = scale_linear2log(20, Rvalue(i), 22050)
                              end, 1, :log],
                             ["bandwidth", 0, initial_notch_bw, 1000,
                              lambda do |w, c, i| @notch_bw = Rvalue(i) end, 1]])
      add_target(RXtParent(sliders[0][0]), lambda do |target| @notch_target = target end, false)
    end
    activate_dialog(@notch_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@notch_label, RxmPushButtonWidgetClass, filter_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_notch_dialog() end)
    filter_menu_list << lambda do
      change_label(child, format("%s (%1.2f %d)", @notch_label, @notch_freq, @notch_bw))
    end
  end.call

  # Butterworth high-pass filter
  @high_pass_freq = 100
  @high_pass_label = "High-pass filter"
  @high_pass_dialog = nil
  @high_pass_target = :sound

  def post_high_pass_dialog
    unless RWidget?(@high_pass_dialog)
      initial_high_pass_freq = 100
      sliders = []
      @high_pass_dialog =
      make_effect_dialog(@high_pass_label,
                         lambda do |w, c, i|
                           case @high_pass_target
                           when :sound
                             filter_sound(make_butter_high_pass(@high_pass_freq))
                           when :selection
                             filter_selection(make_butter_high_pass(@high_pass_freq))
                           else
                             warn "can't apply high-pass between marks yet"
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@high_pass_label,
                                       "Butterworth high-pass filter.
Move the slider to change the high-pass cutoff frequency.")
                         end,
                         lambda do |w, c, i|
                           @high_pass_freq = initial_high_pass_freq
                           RXmScaleSetValue(sliders[0][0],
                                            scale_log2linear(20, @high_pass_freq, 22050))
                           change_label(sliders[0][1], "%.2f" % @high_pass_freq)
                         end)
      sliders = add_sliders(@high_pass_dialog,
                            [["high-pass cutoff frequency", 20, initial_high_pass_freq, 22050,
                              lambda do |w, c, i|
                                @high_pass_freq = scale_linear2log(20, Rvalue(i), 22050)
                              end, 1, :log]])
      add_target(RXtParent(sliders[0][0]),
                 lambda do |target| @high_pass_target = target end, false)
    end
    activate_dialog(@high_pass_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@high_pass_label, RxmPushButtonWidgetClass, filter_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_high_pass_dialog() end)
    filter_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @high_pass_label, @high_pass_freq))
    end
  end.call

  # Butterworth low-pass filter
  @low_pass_freq = 1000
  @low_pass_label = "Low-pass filter"
  @low_pass_dialog = nil
  @low_pass_target = :sound

  def post_low_pass_dialog
    unless RWidget?(@low_pass_dialog)
      initial_low_pass_freq = 1000
      sliders = []
      @low_pass_dialog =
      make_effect_dialog(@low_pass_label,
                         lambda do |w, c, i|
                           case @low_pass_target
                           when :sound
                             filter_sound(make_butter_low_pass(@low_pass_freq))
                           when :selection
                             filter_selection(make_butter_low_pass(@low_pass_freq))
                           else
                             warn "can't apply low-pass between marks yet"
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@low_pass_label,
                                       "Butterworth low-pass filter.
Move the slider to change the low-pass cutoff frequency.")
                         end,
                         lambda do |w, c, i|
                           @low_pass_freq = initial_low_pass_freq
                           RXmScaleSetValue(sliders[0][0],
                                            scale_log2linear(20, @low_pass_freq, 22050))
                           change_label(sliders[0][1], "%.2f" % @low_pass_freq)
                         end)
      sliders = add_sliders(@low_pass_dialog,
                            [["low-pass cutoff frequency", 20, initial_low_pass_freq, 22050,
                              lambda do |w, c, i|
                                @low_pass_freq = scale_linear2log(20, Rvalue(i), 22050)
                              end, 1, :log]])
      add_target(RXtParent(sliders[0][0]), lambda do |target| @low_pass_target = target end, false)
    end
    activate_dialog(@low_pass_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@low_pass_label, RxmPushButtonWidgetClass, filter_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_low_pass_dialog() end)
    filter_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @low_pass_label, @low_pass_freq))
    end
  end.call

  # Comb filter
  @comb_scaler = 0.1
  @comb_size = 50
  @comb_label = "Comb filter"
  @comb_dialog = nil
  @comb_target = :sound

  def comb_filter(scaler, size)
    delay_line = Array.new(size, 0.0)
    delay_loc = 0
    lambda do |x|
      result = delay_line[delay_loc]
      delay_line[delay_loc] = x + scaler * result
      delay_loc += 1
      delay_loc = 0 if delay_loc == size
      result
    end
  end
  
  def post_comb_dialog
    unless RWidget?(@comb_dialog)
      initial_comb_scaler = 0.1
      initial_comb_size = 50
      sliders = []
      @comb_dialog =
      make_effect_dialog(@comb_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |ignored|
                                                            comb_filter(@comb_scaler, @comb_size)
                                                          end,
                                                          @comb_target, "comb-filter", false)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@comb_label,
                                       "Move the slider to change the comb scaler and size.")
                         end,
                         lambda do |w, c, i|
                           @comb_scaler = initial_comb_scaler
                           RXmScaleSetValue(sliders[0], (@comb_scaler * 100).round)
                           @comb_size = initial_comb_size
                           RXmScaleSetValue(sliders[1], @comb_size.round)
                         end)
      sliders = add_sliders(@comb_dialog,
                            [["scaler", 0.0, initial_comb_scaler, 1.0,
                              lambda do |w, c, i| @comb_scaler = Rvalue(i) / 100.0 end, 100],
                             ["size", 0, initial_comb_size, 100,
                              lambda do |w, c, i| @comb_size = Rvalue(i) end, 1]])
      add_target(RXtParent(sliders[0]), lambda do |target| @comb_target = target end, false)
    end
    activate_dialog(@comb_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@comb_label, RxmPushButtonWidgetClass, filter_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_comb_dialog() end)
    filter_menu_list << lambda do
      change_label(child, format("%s (%1.2f %d)", @comb_label, @comb_scaler, @comb_size))
    end
  end.call

  # Comb-chord filter
  @comb_chord_scaler = 0.95
  @comb_chord_size = 60
  @comb_chord_amp = 0.3
  @comb_chord_interval_one = 0.75
  @comb_chord_interval_two = 1.20
  @comb_chord_label = "Comb chord filter"
  @comb_chord_dialog = nil
  @comb_chord_target = :sound

  def comb_chord(scaler, size = nil, amp = nil, interval_one = nil, interval_two = nil)
    doc("comb_chord(scaler, size, amp, interval_one, interval_two)
Comb chord filter: create chords by using filters at harmonically
related sizes.\n") if scaler == :help
    c1 = make_comb(scaler, size)
    c2 = make_comb(scaler, (size * interval_one).round)
    c3 = make_comb(scaler, (size * interval_two).round)
    lambda do |x|
      amp * (comb(c1, x) + comb(c2, x) + comb(c3, x))
    end
  end
  
  def post_comb_chord_dialog
    unless RWidget?(@comb_chord_dialog)
      initial_comb_chord_scaler = 0.95
      initial_comb_chord_size = 60
      initial_comb_chord_amp = 0.3
      initial_comb_chord_interval_one = 0.75
      initial_comb_chord_interval_two = 1.20
      sliders = []
      @comb_chord_dialog =
      make_effect_dialog(@comb_chord_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |ignored|
                                                            comb_chord(@comb_chord_scaler,
                                                                       @comb_chord_size,
                                                                       @comb_chord_amp,
                                                                       @comb_chord_interval_one,
                                                                       @comb_chord_interval_two)
                                                          end,
                                                          @comb_chord_target,
                                                          "comb-chord", false)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@comb_chord_label,
                                       "Comb chord filter:
Creates chords by using filters at harmonically related sizes.  Move \
the sliders to set the comb chord parameters.")
                         end,
                         lambda do |w, c, i|
                           @comb_chord_scaler = initial_comb_chord_scaler
                           RXmScaleSetValue(sliders[0], (@comb_chord_scaler * 100).round)
                           @comb_chord_size = initial_comb_chord_size
                           RXmScaleSetValue(sliders[1], @comb_chord_size.round)
                           @comb_chord_amp = initial_comb_chord_amp
                           RXmScaleSetValue(sliders[2], (@comb_chord_amp * 100).round)
                           @comb_chord_interval_one = initial_comb_chord_interval_one
                           RXmScaleSetValue(sliders[3], (@comb_chord_interval_one * 100).round)
                           @comb_chord_interval_two = initial_comb_chord_interval_two
                           RXmScaleSetValue(sliders[4], (@comb_chord_interval_two * 100).round)
                         end)
      sliders = add_sliders(@comb_chord_dialog,
                            [["chord scaler", 0.0, initial_comb_chord_scaler, 1.0,
                              lambda do |w, c, i| @comb_chord_scaler = Rvalue(i) / 100.0 end, 100],
                             ["chord size", 0, initial_comb_chord_size, 100,
                              lambda do |w, c, i| @comb_chord_size = Rvalue(i) end, 1],
                             ["amplitude", 0.0, initial_comb_chord_amp, 1.0,
                              lambda do |w, c, i| @comb_chord_amp = Rvalue(i) / 100.0 end, 100],
                             ["interval one", 0.0, initial_comb_chord_interval_one, 2.0,
                              lambda do |w, c, i| @comb_chord_interval_one = Rvalue(i) / 100.0 end,
                              100],
                             ["interval two", 0.0, initial_comb_chord_interval_two, 2.0,
                              lambda do |w, c, i| @comb_chord_interval_two = Rvalue(i) / 100.0 end,
                              100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @comb_chord_target = target end, false)
    end
    activate_dialog(@comb_chord_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@comb_chord_label, RxmPushButtonWidgetClass, filter_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_comb_chord_dialog() end)
    filter_menu_list << lambda do
      change_label(child, format("%s (%1.2f %d %1.2f %1.2f %1.2f)",
                                 @comb_chord_label, @comb_chord_scaler,
                                 @comb_chord_size, @comb_chord_amp,
                                 @comb_chord_interval_one, @comb_chord_interval_two))
    end
  end.call

  # Moog filter
  @moog_cutoff_frequency = 10000
  @moog_resonance = 0.5
  @moog_label = "Moog filter"
  @moog_dialog = nil
  @moog_target = :sound

  def moog(freq, q)
    gen = make_moog_filter(freq, q)
    lambda do |inval|
      moog_filter(gen, inval)
    end
  end
  
  def post_moog_dialog
    unless RWidget?(@moog_dialog)
      initial_moog_cutoff_frequency = 10000
      initial_moog_resonance = 0.5
      sliders = []
      @moog_dialog =
      make_effect_dialog(@moog_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |ignored|
                                                            moog(@moog_cutoff_frequency,
                                                                 @moog_resonance)
                                                          end,
                                                          @moog_target, "moog-filter", false)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@moog_label,
                                       "Moog filter:
Moog-style 4-pole lowpass filter with 24db/oct rolloff and variable \
resonance.  Move the sliders to set the filter cutoff frequency and \
resonance.")
                         end,
                         lambda do |w, c, i|
                           @moog_cutoff_frequency = initial_moog_cutoff_frequency
                           RXmScaleSetValue(sliders[0][0],
                                            scale_log2linear(20, @moog_cutoff_frequency, 22050))
                           change_label(sliders[0][1], "%.2f" % @moog_cutoff_frequency)
                           @moog_resonance = initial_moog_resonance
                           RXmScaleSetValue(sliders[1], (@moog_resonance * 100).round)
                         end)
      sliders = add_sliders(@moog_dialog,
                            [["cutoff frequency", 20, initial_moog_cutoff_frequency, 22050,
                              lambda do |w, c, i|
                                @moog_cutoff_frequency = scale_linear2log(20, Rvalue(i), 22050)
                              end, 1, :log],
                             ["resonance", 0.0, initial_moog_resonance, 1.0,
                              lambda do |w, c, i| @moog_resonance = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0][0]), lambda do |target| @moog_target = target end, false)
    end
    activate_dialog(@moog_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@moog_label, RxmPushButtonWidgetClass, filter_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_moog_dialog() end)
    filter_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f",
                                 @moog_label, @moog_cutoff_frequency, @moog_resonance))
    end
  end.call

  #
  # Frequency Effects
  #
  freq_menu_list = []
  freq_menu = RXmCreatePulldownMenu(main_menu(@effects_menu), "Frequency Effects",
                                    [RXmNbackground, basic_color()])
  freq_cascade = RXtCreateManagedWidget("Frequency Effects", RxmCascadeButtonWidgetClass,
                                        main_menu(@effects_menu),
                                        [RXmNsubMenuId, freq_menu,
                                         RXmNbackground, basic_color()])
  RXtAddCallback(freq_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i| update_label(freq_menu_list) end)
  
  # Adaptive saturation
  @adsat_size = 4
  @adsat_label = "Adaptive saturation"
  @adsat_dialog = nil
  @adsat_target = :sound

  def cp_adsat
    map_chan_over_target_with_sync(lambda do |ignored|
                                     mn = 0.0
                                     mx = 0.0
                                     n = 0
                                     vals = make_vct(@adsat_size)
                                     lambda do |val|
                                       if n == @adsat_size
                                         (0...@adsat_size).each do |i|
                                           if vct_ref(vals, i) >= 0.0
                                             vct_set!(vals, i, mx)
                                           else
                                             vct_set!(vals, i, mn)
                                           end
                                         end
                                         n = 0
                                         mx = 0.0
                                         mn = 0.0
                                         vals
                                       else
                                         vct_set!(vals, n, val)
                                         mx = val if val > mx
                                         mn = val if val < mn
                                         n += 1
                                         false
                                       end
                                     end
                                   end, @adsat_target, "adsat", false)
  end
  
  def post_adsat_dialog
    unless RWidget?(@adsat_dialog)
      initial_adsat_size = 4
      sliders = []
      @adsat_dialog =
      make_effect_dialog(@adsat_label,
                         lambda do |w, c, i| cp_adsat() end,
                         lambda do |w, c, i|
                           help_dialog(@adsat_label,
                                       "Move the slider to change the saturation scaling factor.")
                         end,
                         lambda do |w, c, i|
                           @adsat_size = initial_adsat_size
                           RXmScaleSetValue(sliders[0], @adsat_size.round)
                         end)
      sliders = add_sliders(@adsat_dialog,
                            [["adaptive saturation size", 0, initial_adsat_size, 10,
                              lambda do |w, c, i| @adsat_size = Rvalue(i) end, 1]])
      add_target(RXtParent(sliders[0]), lambda do |target| @adsat_target = target end, false)
    end
    activate_dialog(@adsat_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@adsat_label, RxmPushButtonWidgetClass, freq_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_adsat_dialog() end)
    freq_menu_list << lambda do
      change_label(child, format("%s (%d)", @adsat_label, @adsat_size))
    end
  end.call

  # Sample rate conversion (resample)
  @src_amount = 0.0
  @src_label = "Sample rate conversion"
  @src_dialog = nil
  @src_menu_widget = nil
  @src_target = :sound

  def post_src_dialog
    unless RWidget?(@src_dialog)
      initial_src_amount = 0.0
      sliders = []
      @src_dialog =
      make_effect_dialog(@src_label,
                         lambda do |w, c, i|
                           case @src_target
                           when :sound
                             src_sound(@src_amount)
                           when :selection
                             selection?() ? src_selection(@src_amount) : warn("no selection")
                           else
                             warn "can't apply src between marks yet"
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@src_label,
                                       "Move the slider to change the sample rate.
Values greater than 1.0 speed up file play, negative values reverse it.")
                         end,
                         lambda do |w, c, i|
                           @src_amount = initial_src_amount
                           RXmScaleSetValue(sliders[0], (@src_amount * 100).round)
                         end)
      sliders = add_sliders(@src_dialog,
                            [["sample rate", -2.0, initial_src_amount, 2.0,
                              lambda do |w, c, i| @src_amount = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @src_target = target end, false)
    end
    activate_dialog(@src_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@src_label, RxmPushButtonWidgetClass, freq_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_src_dialog() end)
    freq_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @src_label, @src_amount))
    end
  end.call

  # Time and pitch scaling by granular synthesis and sampling rate
  # conversion
  @time_scale = 1.0
  @hop_size = 0.05
  @segment_length = 0.15
  @ramp_scale = 0.5
  @pitch_scale = 1.0
  @expsrc_label = "Time/pitch scaling"
  @expsrc_dialog = nil
  @expsrc_target = :sound

  def post_expsrc_dialog
    unless RWidget?(@expsrc_dialog)
      initial_time_scale = 1.0
      initial_hop_size = 0.05
      initial_segment_length = 0.15
      initial_ramp_scale = 0.5
      initial_pitch_scale = 1.0
      sliders = []
      @expsrc_dialog =
      make_effect_dialog(@expsrc_label,
                         lambda do |w, c, i|
                           save_controls()
                           reset_controls()
                           set_speed_control(@pitch_scale)
                           new_time = @pitch_scale * @time_scale
                           unless new_time == 1.0
                             set_expand_control?(true)
                             set_expand_control(new_time)
                             set_expand_control_hop(@hop_size)
                             set_expand_control_length(@segment_length)
                             set_expand_control_ramp(@ramp_scale)
                           end
                           if @expsrc_target == :marks
                             ms = plausible_mark_samples()
                             apply_controls(selected_sound(), 0, ms[0], 1 + (ms[1] - ms[0]))
                           else
                             apply_controls(selected_sound(), (@expsrc_target == :sound ? 0 : 2))
                           end
                           restore_controls()
                         end,
                         lambda do |w, c, i|
                           help_dialog(@expsrc_label,
                                       "Move the sliders to change the time/pitch scaling parameters.")
                         end,
                         lambda do |w, c, i|
                           @time_scale = initial_time_scale
                           RXmScaleSetValue(sliders[0], (@time_scale * 100).round)
                           @hop_size = initial_hop_size
                           RXmScaleSetValue(sliders[1], (@hop_size * 100).round)
                           @segment_length = initial_segment_length
                           RXmScaleSetValue(sliders[2], (@segment_length * 100).round)
                           @ramp_scale = initial_ramp_scale
                           RXmScaleSetValue(sliders[3], (@ramp_scale * 100).round)
                           @pitch_scale = initial_pitch_scale
                           RXmScaleSetValue(sliders[4], (@pitch_scale * 100).round)
                         end)
      sliders = add_sliders(@expsrc_dialog,
                            [["time scale", 0.0, initial_time_scale, 5.0,
                              lambda do |w, c, i| @time_scale = Rvalue(i) / 100.0 end, 100],
                             ["hop size", 0.0, initial_hop_size, 1.0,
                              lambda do |w, c, i| @hop_size = Rvalue(i) / 100.0 end, 100],
                             ["segment length", 0.0, initial_segment_length, 0.5,
                              lambda do |w, c, i| @segment_length = Rvalue(i) / 100.0 end, 100],
                             ["ramp scale", 0.0, initial_ramp_scale, 0.5,
                              lambda do |w, c, i| @ramp_scale = Rvalue(i) / 100.0 end, 1000],
                             ["pitch scale", 0.0, initial_pitch_scale, 5.0,
                              lambda do |w, c, i| @pitch_scale = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @expsrc_target = target end, false)
    end
    activate_dialog(@expsrc_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@expsrc_label, RxmPushButtonWidgetClass, freq_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_expsrc_dialog() end)
    freq_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f)", @expsrc_label, @time_scale, @pitch_scale))
    end
  end.call

  #
  # Modulation Effects
  #
  mod_menu_list = []
  mod_menu = RXmCreatePulldownMenu(main_menu(@effects_menu), "Modulation Effects",
                                   [RXmNbackground, basic_color()])
  mod_cascade = RXtCreateManagedWidget("Modulation Effects", RxmCascadeButtonWidgetClass,
                                       main_menu(@effects_menu),
                                       [RXmNsubMenuId, mod_menu,
                                        RXmNbackground, basic_color()])
  RXtAddCallback(mod_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i| update_label(mod_menu_list) end)
  
  # Amplitude modulation
  @am_effect_amount = 100.0
  @am_effect_label = "Amplitude modulation"
  @am_effect_dialog = nil
  @am_effect_target = :sound
  @am_effect_envelope = nil

  def am_effect(freq)
    os = make_oscil(freq)
    need_env = (not xe_envelope(@am_effect_envelope) == [0.0, 1.0, 1.0, 1.0])
    e = (need_env and
         make_env(xe_envelope(@am_effect_envelope), :end, effect_frames(@am_effect_target) - 1))
    if need_env
      lambda do |inval| amplitude_modulate(1.0, inval, env(e) * oscil(os)) end
    else
      lambda do |inval| amplitude_modulate(1.0, inval, oscil(os)) end
    end
  end
  
  def post_am_effect_dialog
    unless RWidget?(@am_effect_dialog)
      initial_am_effect_amount = 100.0
      sliders = []
      fr = nil
      @am_effect_dialog =
      make_effect_dialog(@am_effect_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |ignored|
                                                            am_effect(@am_effect_amount)
                                                          end,
                                                          @am_effect_target, "am", false)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@am_effect_label,
                                       "Move the slider to change the modulation amount.")
                         end,
                         lambda do |w, c, i|
                           @am_effect_amount = initial_am_effect_amount
                           @am_effect_envelope = xe_envelope(@am_effect_envelope,
                                                             [0.0, 1.0, 1.0, 1.0])
                           RXmScaleSetValue(sliders[0], @am_effect_amount.round)
                         end)
      sliders = add_sliders(@am_effect_dialog,
                            [["amplitude modulation", 0.0, initial_am_effect_amount, 1000.0,
                              lambda do |w, c, i| @am_effect_amount = Rvalue(i) end, 1]])
      fr = RXtCreateManagedWidget("fr", RxmFrameWidgetClass, RXtParent(sliders[0]),
                                  [RXmNheight, 200,
                                   RXmNshadowThickness, 4,
                                   RXmNshadowType, RXmSHADOW_ETCHED_OUT])
      add_target(RXtParent(sliders[0]), lambda do |target| @am_effect_target = target end, false)
      activate_dialog(@am_effect_dialog)
      @am_effect_envelope = xe_create_enved("am", fr, [RXmNheight, 200], [0.0, 1.0, 0.0, 1.0])
      @am_effect_envelope = xe_envelope(@am_effect_envelope, [0.0, 1.0, 1.0, 1.0])
    else
      activate_dialog(@am_effect_dialog)
    end
  end
  
  lambda do
    child = RXtCreateManagedWidget(@am_effect_label, RxmPushButtonWidgetClass, mod_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_am_effect_dialog() end)
    mod_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @am_effect_label, @am_effect_amount))
    end
  end.call

  # Ring modulation
  @rm_frequency = 100
  @rm_radians = 100
  @rm_label = "Ring modulation"
  @rm_dialog = nil
  @rm_target = :sound
  @rm_envelope = nil

  def rm_effect(freq, gliss_env)
    os = make_oscil(freq)
    need_env = (@rm_envelope and (not xe_envelope(@rm_envelope) == [0.0, 1.0, 1.0, 1.0]))
    e = (need_env and
         make_env(xe_envelope(@rm_envelope), :end, effect_frames(@rm_target) - 1))
    len = frames()
    genv = make_env(:envelope, gliss_env, :end, len)
    if need_env
      lambda do |inval| inval * (env(e) * oscil(os)) end
    else
      lambda do |inval| inval * oscil(os) end
    end
  end
  
  def post_rm_dialog
    unless RWidget?(@rm_dialog)
      initial_rm_frequency = 100
      initial_rm_radians = 100
      sliders = []
      fr = nil
      @rm_dialog =
      make_effect_dialog(@rm_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |ignored|
                                                            rm_effect(@rm_frequency,
                                                                      [0, 0, 1,
                                                                       hz2radians(@rm_radians)])
                                                          end,
                                                          @rm_target, "ring-modulation", false)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@rm_label,
                                       "Move the sliders to change the modulation parameters.")
                         end,
                         lambda do |w, c, i|
                           @rm_frequency = initial_rm_frequency
                           @rm_envelope = xe_envelope(@rm_envelope, [0.0, 1.0, 1.0, 1.0])
                           RXmScaleSetValue(sliders[0], @rm_frequency.round)
                           @rm_radians = initial_rm_radians
                           RXmScaleSetValue(sliders[1], @rm_radians.round)
                         end)
      sliders = add_sliders(@rm_dialog,
                            [["modulation frequency", 0, initial_rm_frequency, 1000,
                              lambda do |w, c, i| @rm_frequency = Rvalue(i) end, 1],
                             ["modulation radians", 0, initial_rm_radians, 360,
                              lambda do |w, c, i| @rm_radians = Rvalue(i) end, 1]])
      fr = RXtCreateManagedWidget("fr", RxmFrameWidgetClass, RXtParent(sliders[0]),
                                  [RXmNheight, 200,
                                   RXmNshadowThickness, 4,
                                   RXmNshadowType, RXmSHADOW_ETCHED_OUT])
      add_target(RXtParent(sliders[0]), lambda do |target| @rm_target = target end, false)
      activate_dialog(@rm_dialog)
      @rm_envelope = xe_create_enved("am", fr, [RXmNheight, 200], [0.0, 1.0, 0.0, 1.0])
      @rm_envelope = xe_envelope(@rm_envelope, [0.0, 1.0, 1.0, 1.0])
    else
      activate_dialog(@rm_dialog)
    end
  end
  
  lambda do
    child = RXtCreateManagedWidget(@rm_label, RxmPushButtonWidgetClass, mod_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_rm_dialog() end)
    mod_menu_list << lambda do
      change_label(child, format("%s (%d %d)", @rm_label, @rm_frequency, @rm_radians))
    end
  end.call

  #
  # Reverbs
  #
  reverb_menu_list = []
  reverb_menu = RXmCreatePulldownMenu(main_menu(@effects_menu), "Reverbs",
                                      [RXmNbackground, basic_color()])
  reverb_cascade = RXtCreateManagedWidget("Reverbs", RxmCascadeButtonWidgetClass,
                                          main_menu(@effects_menu),
                                          [RXmNsubMenuId, reverb_menu,
                                           RXmNbackground, basic_color()])
  RXtAddCallback(reverb_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i| update_label(reverb_menu_list) end)
  
  # Reverb from Michael McNabb's Nrev
  @reverb_amount = 0.1
  @reverb_filter = 0.5
  @reverb_feedback = 1.09
  @reverb_label = "McNabb reverb"
  @reverb_dialog = nil
  @reverb_target = :sound

  def post_reverb_dialog
    unless RWidget?(@reverb_dialog)
      initial_reverb_amount = 0.1
      initial_reverb_filter = 0.5
      initial_reverb_feedback = 1.09
      sliders = []
      @reverb_dialog =
      make_effect_dialog(@reverb_label,
                         lambda do |w, c, i|
                           save_controls()
                           reset_controls()
                           set_reverb_control?(true)
                           set_reverb_control_scale(@reverb_amount)
                           set_reverb_control_lowpass(@reverb_filter)
                           set_reverb_control_feedback(@reverb_feedback)
                           if @reverb_target == :marks
                             ms = plausible_mark_samples()
                             apply_controls(selected_sound(), 0, ms[0], 1 + (ms[1] - ms[0]))
                           else
                             apply_controls(selected_sound(), (@reverb_target == :sound ? 0 : 2))
                           end
                           restore_controls()
                         end,
                         lambda do |w, c, i|
                           help_dialog(@reverb_label,
                                       "Reverberator from Michael McNabb.
Adds reverberation scaled by reverb amount, lowpass filtering, and \
feedback.  Move the sliders to change the reverb parameters.")
                         end,
                         lambda do |w, c, i|
                           @reverb_amount = initial_reverb_amount
                           RXmScaleSetValue(sliders[0], (@reverb_amount * 100).round)
                           @reverb_filter = initial_reverb_filter
                           RXmScaleSetValue(sliders[1], (@reverb_filter * 100).round)
                           @reverb_feedback = initial_reverb_feedback
                           RXmScaleSetValue(sliders[2], (@reverb_feedback * 100).round)
                         end)
      sliders = add_sliders(@reverb_dialog,
                            [["reverb amount", 0.0, initial_reverb_amount, 1.0,
                              lambda do |w, c, i| @reverb_amount = Rvalue(i) / 100.0 end, 100],
                             ["reverb filter", 0.0, initial_reverb_filter, 1.0,
                              lambda do |w, c, i| @reverb_filter = Rvalue(i) / 100.0 end, 100],
                             ["reverb feedback", 0.0, initial_reverb_feedback, 1.25,
                              lambda do |w, c, i| @reverb_feedback = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @reverb_target = target end, false)
    end
    activate_dialog(@reverb_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@reverb_label, RxmPushButtonWidgetClass, reverb_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_reverb_dialog() end)
    reverb_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f %1.2f)",
                                 @reverb_label, @reverb_amount, @reverb_filter, @reverb_feedback))
    end
  end.call

  # Chowning reverb
  @jc_reverb_decay = 2.0
  @jc_reverb_volume = 0.1
  @jc_reverb_label = "Chowning reverb"
  @jc_reverb_dialog = nil
  @jc_reverb_target = :sound
  @jc_reverb_truncate = true

  def jc_reverb_1(in_samps)
    allpass1 = make_all_pass(-0.7, 0.7, 1051)
    allpass2 = make_all_pass(-0.7, 0.7, 337)
    allpass3 = make_all_pass(-0.7, 0.7, 113)
    comb1 = make_comb(0.742, 4799)
    comb2 = make_comb(0.733, 4999)
    comb3 = make_comb(0.715, 5399)
    comb4 = make_comb(0.697, 5801)
    outdel1 = make_delay((0.013 * srate()).round)
    comb_sum = 0.0
    comb_sum_1 = 0.0
    comb_sum_2 = 0.0
    samp = 0
    lambda do |inval|
      allpass_sum = all_pass(allpass3,
                             all_pass(allpass2,
                                      all_pass(allpass1,
                                               (samp < in_samps ? inval : 0.0))))
      samp += 1
      comb_sum_2 = comb_sum_1
      comb_sum_1 = comb_sum
      comb_sum = comb(comb1, allpass_sum) +
                     comb(comb2, allpass_sum) +
                         comb(comb3, allpass_sum) +
                             comb(comb4, allpass_sum)
      inval + @jc_reverb_volume * delay(outdel1, comb_sum)
    end
  end
  
  def post_jc_reverb_dialog
    unless RWidget?(@jc_reverb_dialog)
      initial_jc_reverb_decay = 2.0
      initial_jc_reverb_volume = 0.1
      sliders = []
      @jc_reverb_dialog =
      make_effect_dialog(@jc_reverb_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |x|
                                                            jc_reverb_1(x)
                                                          end,
                                                          @jc_reverb_target,
                                                          "jc-reverb",
                                                          ((not @jc_reverb_truncate) and
                                                           @jc_reverb_decay))
                         end,
                         lambda do |w, c, i|
                           help_dialog(@jc_reverb_label,
                                       "Nice reverb from John Chowning.
Move the sliders to change the reverb parameters.")
                         end,
                         lambda do |w, c, i|
                           @jc_reverb_decay = initial_jc_reverb_decay
                           RXmScaleSetValue(sliders[0], (@jc_reverb_decay * 100).round)
                           @jc_reverb_volume = initial_jc_reverb_volume
                           RXmScaleSetValue(sliders[1], (@jc_reverb_volume * 100).round)
                         end)
      sliders = add_sliders(@jc_reverb_dialog,
                            [["decay duration", 0.0, initial_jc_reverb_decay, 10.0,
                              lambda do |w, c, i| @jc_reverb_decay = Rvalue(i) / 100.0 end, 100],
                             ["reverb volume", 0.0, initial_jc_reverb_volume, 1.0,
                              lambda do |w, c, i| @jc_reverb_volume = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]),
                 lambda do |target| @jc_reverb_target = target end,
                 lambda do |truncate| @jc_reverb_truncate = truncate end)
    end
    activate_dialog(@jc_reverb_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@jc_reverb_label, RxmPushButtonWidgetClass, reverb_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_jc_reverb_dialog() end)
    reverb_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f)",
                                 @jc_reverb_label, @jc_reverb_decay, @jc_reverb_volume))
    end
  end.call

  # Convolution
  @convolve_sound_one = 0
  @convolve_sound_two = 1
  @convolve_amp = 0.01
  @convolve_label = "Convolution"
  @convolve_dialog = nil

  def cnvtest(snd0, snd1, amp)
    flt_len = frames(snd0)
    total_len = flt_len + frames(snd1)
    cnv = make_convolve(:filter, samples2vct(0, flt_len, snd0))
    sf = make_sample_reader(0, snd1)
    out_data = make_vct(total_len)
    vct_map!(out_data, lambda do || convolve(cnv, lambda do |dir| next_sample(sf) end) end)
    free_sample_reader(sf)
    vct_scale!(out_data, amp)
    max_samp = vct_peak(out_data)
    vct2samples(0, total_len, out_data, snd1)
    set_y_bounds([-max_samp, max_samp], snd1) if max_samp > 1.0
    max_samp
  end
  
  def post_convolve_dialog
    unless RWidget?(@convolve_dialog)
      initial_convolve_sound_one = 0
      initial_convolve_sound_two = 1
      initial_convolve_amp = 0.01
      sliders = []
      @convolve_dialog =
      make_effect_dialog(@convolve_label,
                         lambda do |w, c, i|
                           cnvtest(@convolve_sound_one, @convolve_sound_two, @convolve_amp)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@convolve_label,
                                       "Very simple convolution.
Move the sliders to set the numbers of the soundfiles to be convolved \
and the amount for the amplitude scaler.
Output will be scaled to floating-point values, resulting in very \
large (but not clipped) amplitudes. Use the Normalize amplitude effect \
to rescale the output.
The convolution data file typically defines a natural reverberation \
source, and the output from this effect can provide very striking \
reverb effects. You can find convolution data files on sites listed at \
http://www.bright.net/~dlphilp/linux_csound.html under Impulse \
Response Data.")
                         end,
                         lambda do |w, c, i|
                           @convolve_sound_one = initial_convolve_sound_one
                           RXmScaleSetValue(sliders[0], @convolve_sound_one.round)
                           @convolve_sound_two = initial_convolve_sound_two
                           RXmScaleSetValue(sliders[1], @convolve_sound_two.round)
                           @convolve_amp = initial_convolve_amp
                           RXmScaleSetValue(sliders[2], (@convolve_amp * 100).round)
                         end)
      sliders = add_sliders(@convolve_dialog,
                            [["impulse response file", 0, initial_convolve_sound_one, 24,
                              lambda do |w, c, i| @convolve_sound_one = Rvalue(i) end, 1],
                             ["sound file", 0, initial_convolve_sound_two, 24,
                              lambda do |w, c, i| @convolve_sound_two = Rvalue(i) end, 1],
                             ["amplitude", 0.0, initial_convolve_amp, 0.1,
                              lambda do |w, c, i| @convolve_amp = Rvalue(i) / 100.0 end, 1000]])
    end
    activate_dialog(@convolve_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@convolve_label, RxmPushButtonWidgetClass, reverb_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_convolve_dialog() end)
    reverb_menu_list << lambda do
      change_label(child, format("%s (%d %d %1.2f)",
                                 @convolve_label, @convolve_sound_one,
                                 @convolve_sound_two, @convolve_amp))
    end
  end.call

  #
  # Various and Miscellaneous
  #
  misc_menu_list = []
  misc_menu = RXmCreatePulldownMenu(main_menu(@effects_menu), "Various",
                                    [RXmNbackground, basic_color()])
  misc_cascade = RXtCreateManagedWidget("Various", RxmCascadeButtonWidgetClass,
                                        main_menu(@effects_menu),
                                        [RXmNsubMenuId, misc_menu,
                                         RXmNbackground, basic_color()])
  RXtAddCallback(misc_cascade, RXmNcascadingCallback,
                 lambda do |w, c, i| update_label(misc_menu_list) end)

  # Place sound
  @mono_snd = 0
  @stereo_snd = 1
  @pan_pos = 45
  @place_sound_label = "Place sound"
  @place_sound_dialog = nil
  @place_sound_target = :sound
  @place_sound_envelope = nil

  def place_sound(mono_snd, stereo_snd, pan_env)
    doc("place_sound(mono_snd, stereo_snd, pan_env)
mixes a mono sound into a stereo sound, splitting it into two copies
whose amplitudes depend on the envelope 'pan-env'.  If 'pan-env' is a
number, the sound is split such that 0 is all in channel 0 and 90 is
all in channel 1.\n") if mono_snd == :help
    len = frames(mono_snd)
    unless array?(pan_env) or vct?(pan_env)
      pos = pan_env / 90.0
      reader0 = make_sample_reader(0, mono_snd)
      reader1 = make_sample_reader(0, mono_snd)
      map_channel(lambda do |y| y + pos * read_sample(reader1) end, 0, len, stereo_snd, 1)
      map_channel(lambda do |y| y + (1.0 - pos) * read_sample(reader0) end, 0, len, stereo_snd, 0)
    else
      e0 = make_env(pan_env, :end, len - 1)
      e1 = make_env(pan_env, :end, len - 1)
      reader0 = make_sample_reader(0, mono_snd)
      reader1 = make_sample_reader(0, mono_snd)
      map_channel(lambda do |y|
                    y + env(e1) * read_sample(reader1)
                  end, 0, len, stereo_snd, 1)
      map_channel(lambda do |y|
                    y + (1.0 - env(e0)) * read_sample(reader0)
                  end, 0, len, stereo_snd, 0)
    end
  end

  def post_place_sound_dialog
    unless RWidget?(@place_sound_dialog)
      initial_mono_snd = 0
      initial_stereo_snd = 1
      initial_pan_pos = 45
      sliders = []
      fr = nil
      @place_sound_dialog =
      make_effect_dialog(@place_sound_label,
                         lambda do |w, c, i|
                           e = xe_envelope(@place_sound_envelope)
                           unless e == [0.0, 1.0, 1.0, 1.0]
                             place_sound(@mono_snd, @stereo_snd, e)
                           else
                             place_sound(@mono_snd, @stereo_snd, @pan_pos)
                           end
                         end,
                         lambda do |w, c, i|
                           help_dialog(@place_sound_label,
                                       "Mixes mono sound into stereo sound field.")
                         end,
                         lambda do |w, c, i|
                           @place_sound_envelope =
                           xe_envelope(@place_sound_envelope, [0.0, 1.0, 1.0, 1.0])
                           @mono_snd = initial_mono_snd
                           RXmScaleSetValue(sliders[0], @mono_snd.round)
                           @stereo_snd = initial_stereo_snd
                           RXmScaleSetValue(sliders[1], @stereo_snd.round)
                           @pan_pos = initial_pan_pos
                           RXmScaleSetValue(sliders[2], @pan_pos.round)
                         end)
      sliders = add_sliders(@place_sound_dialog,
                            [["mono sound", 0, initial_mono_snd, 50,
                              lambda do |w, c, i| @mono_snd = Rvalue(i) end, 1],
                             ["stereo sound", 0, initial_stereo_snd, 50,
                              lambda do |w, c, i| @stereo_snd = Rvalue(i) end, 1],
                             ["pan position", 0, initial_pan_pos, 90,
                              lambda do |w, c, i| @pan_pos = Rvalue(i) end, 1]])
      fr = RXtCreateManagedWidget("fr", RxmFrameWidgetClass, RXtParent(sliders[0]),
                                  [RXmNheight, 200,
                                   RXmNshadowThickness, 4,
                                   RXmNshadowType, RXmSHADOW_ETCHED_OUT])
      activate_dialog(@place_sound_dialog)
      @place_sound_envelope = xe_create_enved("panning", fr,
                                              [RXmNheight, 200], [0.0, 1.0, 0.0, 1.0])
      @place_sound_envelope = xe_envelope(@place_sound_envelope, [0.0, 1.0, 1.0, 1.0])
    end
    activate_dialog(@place_sound_dialog)
  end

  lambda do
    child = RXtCreateManagedWidget(@place_sound_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_place_sound_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%d %d %d)",
                                 @place_sound_label, @mono_snd, @stereo_snd, @pan_pos))
    end
  end.call

  # Insert silence(at cursor, @silence_amount in secs)
  @silence_amount = 1.0
  @silence_label = "Add silence"
  @silence_dialog = nil

  def post_silence_dialog
    unless RWidget?(@silence_dialog)
      initial_silence_amount = 1.0
      sliders = []
      fr = nil
      @silence_dialog =
      make_effect_dialog(@silence_label,
                         lambda do |w, c, i|
                           insert_silence(cursor(), (srate() * @silence_amount).round)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@silence_label,
                                       "Move the slider to change the number of seconds \
of silence added at the cursor position.")
                         end,
                         lambda do |w, c, i|
                           @silence_amount = initial_silence_amount
                           RXmScaleSetValue(sliders[0], (@silence_amount * 100).round)
                         end)
      sliders = add_sliders(@silence_dialog,
                            [["silence", 0.0, initial_silence_amount, 5.0,
                              lambda do |w, c, i| @silence_amount = Rvalue(i) / 100 end, 100]])
    end
    activate_dialog(@silence_dialog)
  end

  lambda do
    child = RXtCreateManagedWidget(@silence_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_silence_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @silence_label, @silence_amount))
    end
  end.call

  # Contrast (brightness control)
  @contrast_amount = 1.0
  @contrast_label = "Contrast enhancement"
  @contrast_dialog = nil
  @contrast_target = :sound

  def post_contrast_dialog
    unless RWidget?(@contrast_dialog)
      initial_contrast_amount = 1.0
      sliders = []
      @contrast_dialog =
      make_effect_dialog(@contrast_label,
                         lambda do |w, c, i|
                           peak = maxamp()
                           save_controls()
                           reset_controls()
                           set_contrast_control?(true)
                           set_contrast_control(@contrast_amount)
                           set_contrast_control_amp(1.0 / peak)
                           set_amp_control(peak)
                           if @contrast_target == :marks
                             ms = plausible_mark_samples()
                             apply_controls(selected_sound(), 0, ms[0], 1 + (ms[1] - ms[0]))
                           else
                             apply_controls(selected_sound(), (@contrast_target == :sound ? 0 : 2))
                           end
                           restore_controls()
                         end,
                         lambda do |w, c, i|
                           help_dialog(@contrast_label,
                                       "Move the slider to change the contrast intensity.")
                         end,
                         lambda do |w, c, i|
                           @contrast_amount = initial_contrast_amount
                           RXmScaleSetValue(sliders[0], (@contrast_amount * 100).round)
                         end)
      sliders = add_sliders(@contrast_dialog,
                            [["contrast enhancement", 0.0, initial_contrast_amount, 10.0,
                              lambda do |w, c, i| @contrast_amount = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @contrast_target = target end, false)
    end
    activate_dialog(@contrast_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@contrast_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_contrast_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @contrast_label, @contrast_amount))
    end
  end.call

  # Cross synthesis
  @cross_synth_sound = 1
  @cross_synth_amp = 0.5
  @cross_synth_fft_size = 128
  @cross_synth_radius = 6.0
  @cross_synth_label = "Cross synthesis"
  @cross_synth_dialog = nil
  @cross_synth_default_fft_widget = nil
  @cross_synth_target = :sound

  def cross_synthesis(cross_snd, amp = nil, fftsize = nil, r = nil)
    doc("cross_synthesis(cross_snd, amp, fftsize, r)
CROSS_SND is the index of the other sound (as opposed to the map-chan
sound)\n") if cross_snd == :help
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r / fftsize
    bin = srate() / fftsize
    formants = Array.new(freq_inc)
    (0...freq_inc).each do |i|
      formants[i] = make_formant(radius, i * bin)
    end
    lambda do |inval|
      outval = 0.0
      if ctr == freq_inc
        samples2vct(inctr, fftsize, cross_snd, 0, fdr)
        inctr += freq_inc
        spectrum(fdr, fdi, false, fftsize, 2)
        vct_subtract!(fdr, spectr)
        vct_scale!(fdr, 1.0 / freq_inc)
        ctr = 0
      end
      ctr += 1
      vct_add!(spectr, fdr)
      amp * formant_bank(spectr, formants, inval)
    end
  end
  
  def post_cross_synth_dialog
    unless RWidget?(@cross_synth_dialog)
      initial_cross_synth_sound = 1
      initial_cross_synth_amp = 0.5
      initial_cross_synth_fft_size = 128
      initial_cross_synth_radius = 6.0
      sliders = []
      @cross_synth_dialog =
      make_effect_dialog(@cross_synth_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |ignored|
                                                            cross_synthesis(@cross_synth_sound,
                                                                            @cross_synth_amp,
                                                                            @cross_synth_fft_size,
                                                                            @cross_synth_radius)
                                                          end,
                                                          @cross_synth_target,
                                                          "Cross synthesis", false)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@cross_synth_label,
                                       "The sliders set the number of the soundfile \
to be cross_synthesized, the synthesis amplitude, the FFT size, and the radius value.")
                         end,
                         lambda do |w, c, i|
                           @cross_synth_sound = initial_cross_synth_sound
                           RXmScaleSetValue(sliders[0], @cross_synth_sound.round)
                           @cross_synth_amp = initial_cross_synth_amp
                           RXmScaleSetValue(sliders[1], (@cross_synth_amp * 100).round)
                           @cross_synth_fft_size = initial_cross_synth_fft_size
                           if @use_combo_box_for_fft_size
                             RXtSetValues(@cross_synth_default_fft_widget,
                                          [RXmNselectedPosition, 1])
                           else
                             RXmToggleButtonSetState(@cross_synth_default_fft_widget, true, true)
                           end
                           @cross_synth_radius = initial_cross_synth_radius
                           RXmScaleSetValue(sliders[2], (@cross_synth_radius * 100).round)
                         end)
      sliders = add_sliders(@cross_synth_dialog,
                            [["input sound", 0, initial_cross_synth_sound, 20,
                              lambda do |w, c, i| @cross_synth_sound = Rvalue(i) end, 1],
                             ["amplitude", 0.0, initial_cross_synth_amp, 1.0,
                              lambda do |w, c, i| @cross_synth_amp = Rvalue(i) / 100.0 end, 100],
                             ["radius", 0.0, initial_cross_synth_radius, 360.0,
                              lambda do |w, c, i| @cross_synth_radius = Rvalue(i) / 100.0 end,
                              100]])
      s1 = string2compound("FFT size")
      frame = RXtCreateManagedWidget("frame", RxmFrameWidgetClass, RXtParent(sliders[0]),
                                     [RXmNborderWidth, 1,
                                      RXmNshadowType, RXmSHADOW_ETCHED_IN,
                                      RXmNpositionIndex, 2])
      frm = RXtCreateManagedWidget("frm", RxmFormWidgetClass, frame,
                                   [RXmNleftAttachment, RXmATTACH_FORM,
                                    RXmNrightAttachment, RXmATTACH_FORM,
                                    RXmNtopAttachment, RXmATTACH_FORM,
                                    RXmNbottomAttachment, RXmATTACH_FORM,
                                    RXmNbackground, basic_color()])
      if @use_combo_box_for_fft_size
        lab = RXtCreateManagedWidget("FFT size", RxmLabelWidgetClass, frm,
                                     [RXmNleftAttachment, RXmATTACH_FORM,
                                      RXmNrightAttachment, RXmATTACH_NONE,
                                      RXmNtopAttachment, RXmATTACH_FORM,
                                      RXmNbottomAttachment, RXmATTACH_FORM,
                                      RXmNlabelString, s1,
                                      RXmNbackground, basic_color()])
        fft_labels = ["64", "128", "256", "512", "1024", "4096"].map do |n| string2compound(n) end
        combo = RXtCreateManagedWidget("fftsize", RxmComboBoxWidgetClass, frm,
                                       [RXmNleftAttachment, RXmATTACH_WIDGET,
                                        RXmNleftWidget, lab,
                                        RXmNrightAttachment, RXmATTACH_FORM,
                                        RXmNtopAttachment, RXmATTACH_FORM,
                                        RXmNbottomAttachment, RXmATTACH_FORM,
                                        RXmNitems, fft_labels,
                                        RXmNitemCount, fft_labels.length,
                                        RXmNcomboBoxType, RXmDROP_DOWN_COMBO_BOX,
                                        RXmNbackground, basic_color()])
        @cross_synth_default_fft_widget = combo
        RXtSetValues(combo, [RXmNselectedPosition, 1])
        RXtAddCallback(combo, RXmNselectionCallback,
                       lambda do |w, c, i|
                         @cross_synth_fft_size = compound2string(Ritem_or_text(i)).to_i
                       end)
      else
        rc = RXtCreateManagedWidget("rc", RxmRowColumnWidgetClass, frm,
                                    [RXmNorientation, RXmHORIZONTAL,
                                     RXmNradioBehavior, true,
                                     RXmNradioAlwaysOne, true,
                                     RXmNentryClass, RxmToggleButtonWidgetClass,
                                     RXmNisHomogeneous, true,
                                     RXmNleftAttachment, RXmATTACH_FORM,
                                     RXmNrightAttachment, RXmATTACH_FORM,
                                     RXmNtopAttachment, RXmATTACH_FORM,
                                     RXmNbottomAttachment, RXmATTACH_NONE,
                                     RXmNbackground, basic_color()])
        lab = RXtCreateManagedWidget("FFT size", RxmLabelWidgetClass, frm,
                                     [RXmNleftAttachment, RXmATTACH_FORM,
                                      RXmNrightAttachment, RXmATTACH_FORM,
                                      RXmNtopAttachment, RXmATTACH_WIDGET,
                                      RXmNtopWidget, rc,
                                      RXmNbottomAttachment, RXmATTACH_FORM,
                                      RXmNlabelString, s1,
                                      RXmNalignment, RXmALIGNMENT_BEGINNING,
                                      RXmNbackground, basic_color()])
        [64, 128, 256, 512, 1024, 4096].each do |size|
          button = RXtCreateManagedWidget("#{size}", RxmToggleButtonWidgetClass, rc,
                                          [RXmNbackground, basic_color(),
                                           RXmNvalueChangedCallback,
                                           [lambda do |w, c, i|
                                              @cross_synth_fft_size = c if Rset(i)
                                            end, size],
                                           RXmNset, (size == @cross_synth_fft_size)])
          @cross_synth_default_fft_widget = button if size == @cross_synth_fft_size
        end
      end
      add_target(RXtParent(sliders[0]), lambda do |target| @cross_synth_target = target end, false)
    end
    activate_dialog(@cross_synth_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@cross_synth_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_cross_synth_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%d %1.2f %d %1.2f)",
                                 @cross_synth_label, @cross_synth_sound, @cross_synth_amp,
                                 @cross_synth_fft_size, @cross_synth_radius))
    end
  end.call

  # Flange and phasing
  @flange_speed = 2.0
  @flange_amount = 5.0
  @flange_time = 0.001
  @flange_label = "Flange"
  @flange_dialog = nil
  @flange_target = :sound

  def post_flange_dialog
    unless RWidget?(@flange_dialog)
      initial_flange_speed = 2.0
      initial_flange_amount = 5.0
      initial_flange_time = 0.001
      sliders = []
      @flange_dialog =
      make_effect_dialog(@flange_label,
                         lambda do |w, c, i|
                           map_chan_over_target_with_sync(lambda do |ignored|
                                                            ri =
                                                            make_rand_interp(:frequency,
                                                                             @flange_speed,
                                                                             :amplitude,
                                                                             @flange_amount)
                                                            len = (@flange_time * srate()).round
                                                            del = make_delay(len, false, 0.0,
                                                                             (len +
                                                                              @flange_amount +
                                                                              1).round)
                                                            lambda do |inval|
                                                              0.75 * (inval +
                                                                      delay(del, inval,
                                                                            rand_interp(ri)))
                                                            end
                                                          end,
                                                          @flange_target, "flange", false)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@flange_label,
                                       "Move the sliders to change the flange speed, \
amount, and time.")
                         end,
                         lambda do |w, c, i|
                           @flange_speed = initial_flange_speed
                           RXmScaleSetValue(sliders[0], (@flange_speed * 10).round)
                           @flange_amount = initial_flange_amount
                           RXmScaleSetValue(sliders[1], (@flange_amount * 10).round)
                           @flange_time = initial_flange_time
                           RXmScaleSetValue(sliders[2], (@flange_time * 100).round)
                         end)
      sliders = add_sliders(@flange_dialog,
                            [["flange speed", 0.0, initial_flange_speed, 100.0,
                              lambda do |w, c, i| @flange_speed = Rvalue(i) / 10.0 end, 10],
                             ["flange amount", 0.0, initial_flange_amount, 100.0,
                              lambda do |w, c, i| @flange_amount = Rvalue(i) / 10.0 end, 10],
                             ["flange time", 0.0, initial_flange_time, 1.0,
                              lambda do |w, c, i| @flange_time = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @flange_target = target end, false)
    end
    activate_dialog(@flange_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@flange_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_flange_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f %1.3f)",
                                 @flange_label, @flange_speed, @flange_amount, @flange_time))
    end
  end.call

  # Randomize phase
  @random_phase_amp_scaler = 3.14
  @random_phase_label = "Randomize phase"
  @random_phase_dialog = nil

  def post_random_phase_dialog
    unless RWidget?(@random_phase_dialog)
      initial_random_phase_amp_scaler = 3.14
      sliders = []
      @random_phase_dialog =
      make_effect_dialog(@random_phase_label,
                         lambda do |w, c, i|
                           rotate_phase(lambda do |x| kernel_rand(@random_phase_amp_scaler) end)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@random_phase_label,
                                       "Move the slider to change the randomization \
amplitude scaler.")
                         end,
                         lambda do |w, c, i|
                           @random_phase_amp_scaler = initial_random_phase_amp_scaler
                           RXmScaleSetValue(sliders[0], (@random_phase_amp_scaler * 100).round)
                         end)
      sliders = add_sliders(@random_phase_dialog,
                            [["amplitude scaler", 0.0, initial_random_phase_amp_scaler, 100.0,
                              lambda do |w, c, i| @random_phase_amp_scaler = Rvalue(i) / 100.0 end,
                              100]])
    end
    activate_dialog(@random_phase_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@random_phase_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_random_phase_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%1.2f)", @random_phase_label, @random_phase_amp_scaler))
    end
  end.call

  # Robotize
  @samp_rate = 1.0
  @osc_amp = 0.3
  @osc_freq = 20
  @robotize_label = "Robotize"
  @robotize_dialog = nil
  @robotize_target = :sound

  def fp_1(sr, osamp, osfrq, beg, fin)
    os = make_oscil(osfrq)
    sr = make_src(:srate, sr)
    len = fin - beg + 1
    sf = make_sample_reader(beg)
    out_data = make_vct(len)
    vct_map!(out_data,
             lambda do | |
               src(sr, osamp * oscil(os),
                   lambda do |dir|
                     if dir > 0
                       next_sample(sf)
                     else
                       previous_sample(sf)
                     end
                   end)
             end)
    free_sample_reader(sf)
    vct2samples(beg, len, out_data)
  end
  
  def post_robotize_dialog
    unless RWidget?(@robotize_dialog)
      initial_samp_rate = 1.0
      initial_osc_amp = 0.3
      initial_osc_freq = 20
      sliders = []
      @robotize_dialog =
      make_effect_dialog(@robotize_label,
                         lambda do |w, c, i|
                           ms = (@robotize_target == :marks and plausible_mark_samples())
                           fp_1(@samp_rate, @osc_amp, @osc_freq,
                                case @robotize_target
                                when :sound
                                  0
                                when :selection
                                  selection_position()
                                else
                                  ms[0]
                                end,
                                case @robotize_target
                                when :sound
                                  frames() - 1
                                when :selection
                                  selection_position() + selection_frames()
                                else
                                  ms[1]
                                end)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@robotize_label,
                                       "Move the sliders to set the sample rate, \
oscillator amplitude, and oscillator frequency.")
                         end,
                         lambda do |w, c, i|
                           @samp_rate = initial_samp_rate
                           RXmScaleSetValue(sliders[0], (@samp_rate * 100).round)
                           @osc_amp = initial_osc_amp
                           RXmScaleSetValue(sliders[1], (@osc_amp * 100).round)
                           @osc_freq = initial_osc_freq
                           RXmScaleSetValue(sliders[2], (@osc_freq * 100).round)
                         end)
      sliders = add_sliders(@robotize_dialog,
                            [["sample rate", 0.0, initial_samp_rate, 2.0,
                              lambda do |w, c, i| @samp_rate = Rvalue(i) / 100.0 end, 100],
                             ["oscillator amplitude", 0.0, initial_osc_amp, 1.0,
                              lambda do |w, c, i| @osc_amp = Rvalue(i) / 100.0 end, 100],
                             ["oscillator frequency", 0.0, initial_osc_freq, 60,
                              lambda do |w, c, i| @osc_freq = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @robotize_target = target end, false)
    end
    activate_dialog(@robotize_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@robotize_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_robotize_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f %1.2f)",
                                 @robotize_label, @samp_rate, @osc_amp, @osc_freq))
    end
  end.call

  # Wobble
  @wobble_frequency = 50
  @wobble_amplitude = 0.5
  @wobble_label = "Wobble"
  @wobble_dialog = nil
  @wobble_target = :sound
  
  def hello_dentist_1(frq, amp, beg, fin)
    rn = make_rand_interp(:frequency, frq, :amplitude, amp)
    i = j = 0
    len = (fin - beg) + 1
    in_data = samples2vct(beg, len)
    out_len = (len * (1.0 + 2 * amp)).round
    out_data = make_vct(out_len)
    rd = make_src(:srate, 1.0, :input, lambda do |dir|
                    val = ((i >= 0 and i < len) ? vct_ref(in_data, i) : 0.0)
                    i += dir
                    val
                  end)
    until i == len or j == out_len
      vct_set!(out_data, j, src(rd, rand_interp(rn)))
      j += 1
    end
    vct2samples(beg, j, out_data)
  end

  def post_wobble_dialog
    unless RWidget?(@wobble_dialog)
      initial_wobble_frequency = 50
      initial_wobble_amplitude = 0.5
      sliders = []
      @wobble_dialog =
      make_effect_dialog(@wobble_label,
                         lambda do |w, c, i|
                           ms = (@wobble_target == :marks and plausible_mark_samples())
                           hello_dentist_1(@wobble_frequency, @wobble_amplitude,
                                           case @wobble_target
                                           when :sound
                                             0
                                           when :selection
                                             selection_position()
                                           else
                                             ms[0]
                                           end,
                                           case @wobble_target
                                           when :sound
                                             frames() - 1
                                           when :selection
                                             selection_position() + selection_frames()
                                           else
                                             ms[1]
                                           end)
                         end,
                         lambda do |w, c, i|
                           help_dialog(@wobble_label,
                                       "Move the sliders to set the wobble \
frequency and amplitude.")
                         end,
                         lambda do |w, c, i|
                           @wobble_frequency = initial_wobble_frequency
                           RXmScaleSetValue(sliders[0], (@wobble_frequency * 100).round)
                           @wobble_amplitude = initial_wobble_amplitude
                           RXmScaleSetValue(sliders[1], (@wobble_amplitude * 100).round)
                         end)
      sliders = add_sliders(@wobble_dialog,
                            [["wobble frequency", 0, initial_wobble_frequency, 100,
                              lambda do |w, c, i| @wobble_frequency = Rvalue(i) / 100.0 end, 100],
                             ["wobble amplitude", 0.0, initial_wobble_amplitude, 1.0,
                              lambda do |w, c, i| @wobble_amplitude = Rvalue(i) / 100.0 end, 100]])
      add_target(RXtParent(sliders[0]), lambda do |target| @wobble_target = target end, false)
    end
    activate_dialog(@wobble_dialog)
  end
  
  lambda do
    child = RXtCreateManagedWidget(@wobble_label, RxmPushButtonWidgetClass, misc_menu,
                                   [RXmNbackground, basic_color()])
    RXtAddCallback(child, RXmNactivateCallback,
                   lambda do |w, c, i| post_wobble_dialog() end)
    misc_menu_list << lambda do
      change_label(child, format("%s (%1.2f %1.2f)",
                                 @wobble_label, @wobble_frequency, @wobble_amplitude))
    end
  end.call

  add_to_menu(@effects_menu, false, false)
  add_to_menu(@effects_menu, "Octave-down", lambda do || down_oct() end)
  add_to_menu(@effects_menu, "Remove clicks", lambda do ||
                find_click = lambda do |loc|
                  reader = make_sample_reader(loc)
                  samp0 = samp1 = samp2 = 0.0
                  samps = make_vct(10)
                  samps_ctr = 0
                  diff = 1.0
                  len = frames()
                  callcc do |ret|
                    ctr = loc
                    until c_g? or ctr == len
                      ctr += 1
                      samp0 = samp1
                      samp1 = samp2
                      samp2 = next_sample(reader)
                      if samps_ctr < 9
                        samps_ctr += 1
                      else
                        samps_ctr = 0
                      end
                      local_max = [0.1, vct_peak(samps)].max
                      if ((samp0 - samp1).abs > local_max) and
                         ((samp1 - samp2).abs > local_max) and
                         ((samp0 - samp2).abs < (local_max / 2))
                        ret.call(ctr - 2)
                      else
                        false
                      end
                    end
                  end
                end
                remove_click = lambda do |loc|
                  click = find_click.call(loc)
                  if click and (not c_g?)
                    smooth_sound(click - 2, 4)
                    remove_click.call(click + 2)
                  end
                end
                remove_click.call(0)
              end)
  add_to_menu(@effects_menu, "Remove DC", lambda do ||
                lastx = lasty = 0.0
                map_chan(lambda do |inval|
                           lasty = inval + (0.999 * lasty - lastx)
                           lastx = inval
                           lasty
                         end)
              end)
  add_to_menu(@effects_menu, "Spiker", lambda do || spike() end)
  add_to_menu(@effects_menu, "Compand", lambda do ||
                tbl = vct(-1.00, -0.96, -0.90, -0.82, -0.72, -0.60, -0.45, -0.25,
                          0.00, 0.25, 0.45, 0.60, 0.72, 0.82, 0.90, 0.96, 1.00)
                map_chan(lambda do |inval|
                           index = 8.0 + 8.0 * inval
                           array_interp(tbl, index, tbl.length)
                         end)
              end)
  add_to_menu(@effects_menu, "Invert", lambda do || scale_by(-1) end)
  add_to_menu(@effects_menu, "Reverse", lambda do || reverse_sound() end)
  add_to_menu(@effects_menu, "Null phase", lambda do || zero_phase() end)
end

include Effects

# effects.rb ends here
