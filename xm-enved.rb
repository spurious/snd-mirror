# xm-enved.rb -- Translation of xm-enved.scm

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Created: Tue Mar 18 00:18:35 CET 2003
# Last: Fri Feb 27 14:12:51 CET 2004

# Commentary:
#
# You can use class Enved without Motif or Gtk.  If you want drawing
# panes, Xenved requires --with-motif or --with-gtk and module
# libxm.so or --with-static-xm.
#
# Tested with Snd 7.3, Motif 2.2.2, Gtk+ 2.2.1, Ruby 1.6.6, 1.6.8 and 1.9.0.
#
# type: xe = xenved_test
#       xe.help
#
# make_enved(env)
#
# class Enved
#   initialize(env)
#
# getter and setter:
#   envelope=(new_env)
#   envelope
#
# methods:
#   interp(x, base)
#   stretch(oatt, natt, odec, ndec) stretch!(oatt, natt, odec, ndec)
#   scale(scale, offset)            scale!(scale, offset)
#   normalize(new_max)              normalize!(new_max)
#   reverse                         reverse!
#   max                             min
#   first   (first [x, y])          last   (last [x, y])
#   first_x                         last_x
#   each do |x, y| ... end          map do |x, y| ... end
#   length
#   point(idx, *args)               # point(idx) --> [x, y]
#                                   # point(idx, :x, x_val, :y, y_val)
#                                   # sets x, y or both and returns new [x, y]
#   in_range?(x)   (x > first_x and x < last_x)
#   help           (alias info and description)
#
# make_xenved(name, parent, *rest)
#   name     String
#   parent   Widget
#   *rest
#     :envelope,    [0, 0, 1, 1]   x0, y0, x1, y1, ...
#     :axis_bounds, [0, 1, 0, 1]   x0, x1, y0, y1
#     :args,        []             Motif properties
#     :axis_label,  nil            if axes labels should have
#                                  other values than axis_bounds,
#                                  (see dlocsig.rb)
#
# class Xenved < Enved (see xm-enved.scm)
#   initialize(name, parent, enved, axis_bounds, args, axis_label)
#   before_enved_hook              lambda do |pos, x, y, reason| ... end
#   after_enved_hook               lambda do |pos, reason| ... end
#
# getter and setter:
#   click_time=(val)
#   click_time
#   axis_bounds=(new_bounds)
#   axis_bounds
#
# interactive methods:
#   create   (alias open)
#   close
#   clear
#   help     (alias info and description)

=begin
# more examples in effects.rb

xe = xenved_test
xe.envelope                         # --> [0.0, 0.0, 1.0, 1.0]
xe.click_time                       # --> 0.2
xe.envelope = [0, 1, 1, 1]
# three clicks later
xe.envelope                         # --> [0.0, 1.0,
                                    #      0.190735694822888, 0.562264150943396,
                                    #      0.632152588555858, 0.932075471698113,
                                    #      0.848773841961853, 0.316981132075472,
                                    #      1.0, 1.0]
xe.help                             # this help
=end

# Code:

require "examp"
require "env"
require "hooks"

if provided? "snd-motif" or provided? "snd-gtk"
  require "snd-xm"
  include Snd_XM
end

def make_enved(enved = [0, 0, 1, 1])
  unless enved.kind_of?(Array) and enved.length >= 4 and enved.length.even?
    error("%s: need at least two points [x0, y0, x1, y1, ...], %s", get_func_name, enved.inspect)
  end
  Enved.new(enved)
end

if provided? "xm" or provided? "xg"
  def xenved_test(name = "xenved")
    widget = if provided? "xm"
               add_main_pane(name, RxmFormWidgetClass, [RXmNheight, 200])
             else
               add_main_pane(name)
             end
    args = if provided? "xm"
             [RXmNleftAttachment, RXmATTACH_WIDGET,
              RXmNtopAttachment, RXmATTACH_WIDGET,
              RXmNbottomAttachment, RXmATTACH_WIDGET,
              RXmNrightAttachment, RXmATTACH_WIDGET]
           else
             []
           end
    make_xenved(name, widget, :envelope, [0, 0, 1, 1], :axis_bounds, [0, 1, 0, 1], :args, args)
  end

  def make_xenved(name, parent, *rest)
    envelope = get_args(rest, :envelope, [0, 0, 1, 1])
    bounds   = get_args(rest, :axis_bounds, [0, 1, 0, 1])
    args     = get_args(rest, :args, [])
    label    = get_args(rest, :axis_label, nil)
    name = "xenved" unless name.kind_of?(String) and !name.empty?
    unless widget?(parent)
      error("%s: arg 2, %s, need a widget", get_func_name, parent.inspect)
    end
    unless bounds.kind_of?(Array) and bounds.length == 4
      error("%s: axis_bounds, %s, need an array of four numbers [x0, x1, y0, y1]",
            get_func_name, bounds.inspect)
    end
    unless label.kind_of?(Array) and label.length == 4
      label = bounds
    end
    Xenved.new(name, parent, envelope, bounds, args, label)
  end
end

class Enved
  include Env
  include Info

  def initialize(enved = [0, 0, 1, 1])
    @envelope = enved.map do |x| x.to_f end
    set_help
  end
  attr_reader :envelope
  
  def inspect
    format("#<%s: envelope: %s>", self.class, @envelope.to_string)
  end

  def envelope=(new_env)
    if new_env.kind_of?(Array) and new_env.length >= 4 and new_env.length.even?
      @envelope = new_env.map do |x| x.to_f end
    else
      error("%s#%s: need at least two points [x0, y0, x1, y1, ...], %s",
            self.class, get_func_name, new_env.inspect)
    end
  end

  def interp(x, base = 0)
    envelope_interp(x, @envelope, base)
  end

  def stretch(old_att = nil, new_att = nil, old_dec = nil, new_dec = nil)
    stretch_envelope(@envelope, old_att, new_att, old_dec, new_dec)
  end

  def stretch!(old_att = nil, new_att = nil, old_dec = nil, new_dec = nil)
    self.envelope = self.stretch(old_att, new_att, old_dec, new_dec)
  end

  def scale(scale = 1.0, offset = 0.0)
    scale_envelope(@envelope, scale, offset)
  end

  def scale!(scale = 1.0, offset = 0.0)
    self.envelope = self.scale(scale, offset)
  end

  def normalize(new_max = 1.0)
    self.scale(new_max / self.max)
  end

  def normalize!(new_max = 1.0)
    self.envelope = self.normalize(new_max)
  end
  
  def reverse
    reverse_envelope(@envelope)
  end
  
  def reverse!
    self.envelope = self.reverse
  end

  def point(idx, *args)
    x = get_args(args, :x, nil)
    y = get_args(args, :y, nil)
    if x
      @envelope[idx * 2] = x
    end
    if y
      @envelope[idx * 2 + 1] = y
    end
    @envelope[idx * 2, 2]
  end
  
  def min
    min_envelope(@envelope)
  end

  def max
    max_envelope(@envelope)
  end

  def length
    @envelope.length / 2
  end
  
  def first
    if @envelope.length > 1
      @envelope[0, 2]
    else
      [0.0, 0.0]
    end
  end

  def last
    if @envelope.length > 3
      @envelope[-2, 2]
    else
      [1.0, 0.0]
    end
  end

  def first_x
    @envelope[0]
  end
  
  def last_x
    @envelope[-2]
  end

  def in_range?(x)
    x > @envelope[0] and x < @envelope[-2]
  end

  def each
    0.step(@envelope.length - 1, 2) do |i| yield(@envelope[i, 2]) end
    @envelope
  end

  def map
    res = []
    0.step(@envelope.length - 1, 2) do |i| res.push(yield(@envelope[i, 2])) end
    res
  end
  
  private
  def set_help
    self.description = "\
# make_enved(env)
#
# class Enved
#   initialize(env)
#
# getter and setter:
#   envelope=(new_env)
#   envelope
#
# methods:
#   interp(x, base)
#   stretch(oatt, natt, odec, ndec) stretch!(oatt, natt, odec, ndec)
#   scale(scale, offset)            scale!(scale, offset)
#   normalize(new_max)              normalize!(new_max)
#   reverse                         reverse!
#   max                             min
#   first   (first [x, y])          last   (last [x, y])
#   first_x                         last_x
#   each do |x, y| ... end          map do |x, y| ... end
#   length
#   point(idx, *args)               # point(idx) --> [x, y]
#                                   # point(idx, :x, x_val, :y, y_val)
#                                   # sets x, y or both and returns new [x, y]
#   in_range?(x)   (x > first_x and x < last_x)
#   help           (alias info and description)
"
  end
end

class Xenved < Enved
  def initialize(name, parent, enved, bounds, args, axis_label)
    super(enved)
    @name = name
    @parent = parent
    @x0, @x1, @y0, @y1 = bounds.map do |x| x.to_f end
    @args = args
    if provided? "xm"
      @args += [RXmNforeground, data_color] unless @args.member?(RXmNforeground)
      @args += [RXmNbackground, graph_color] unless @args.member?(RXmNbackground)
    end
    @lx0, @lx1, @ly0, @ly1 = axis_label.map do |x| x.to_f end
    @init = @envelope.dup
    @mouse_up = 0.0
    @mouse_down = 0.0
    @click_time = 0.2
    @mouse_pos = 0
    @mouse_new = false
    @gc = snd_gcs[0]
    @drawer = @dpy = @window = nil
    @px0 = @px1 = @py0 = @py1 = nil
    @before_enved_hook = Hook.new("@before_enved_hook", 4, "\
lambda do |pos, x, y, reason| ... end: called before changing a
breakpoint in @envelope.  This hook runs the global $enved_hook as
first hook, subsequent procedures can directly manipulate @envelope.

This instance hook is like the global $enved_hook; POS is @envelope's
x-position, X and Y are the new points, and REASON is one of
Enved_add_point, Enved_delete_point, Enved_move_point.  If the last
hook procedure in the hook list returns `false', the class changes the
breakpoint, otherwise the hook procedures are responsible for
manipulating @envelope itself.

From dlocsig.rb:

@velocity = make_xenved(\"velocity (v)\", frame,
                        :envelope, [0.0, 0.0, 1.0, 0.0],
                        :axis_bounds, [0.0, 1.0, 0.0, 1.0],
                        :axis_label, [-20.0, 20.0, 0.0, 2.0])
@velocity.before_enved_hook.reset_hook!   # to prevent running $enved_hook
@velocity.before_enved_hook.add_hook!(\"dlocsig-hook\") do |pos, x, y, reason|
  if reason == Enved_move_point
    if @velocity.in_range?(x)
      old_x = @velocity.point(pos).first
      @velocity.stretch!(old_x, x)
      @velocity.point(pos, :y, y)
    else
      false
    end
  else
    false
  end
end

In contrast the same procedure on the global $enved_hook:

$enved_hook.add_hook!(\"snd-init-hook\") do |env, pt, x, y, reason|
  if reason == Enved_move_point
    if x > 0.0 and x < env[-2]
      old_x = env[2 * pt]
      new_env = stretch_envelope(env, old_x, x)
      new_env[pt * 2 + 1] = y
      new_env
    else
      # env               # first and last points are fixed
      false               # first and last points can be moved
    end
  else
    false
  end
end")
    @before_enved_hook.add_hook!("initialize-xm-enved-hook") do |pos, x, y, reason|
      if $enved_hook.empty?
        false
      else
        e = nil
        $enved_hook.run_hook do |prc|
          case e = prc.call(@envelope, pos, x, y, reason)
          when Array
            self.envelope = e
          when Enved, Xenved
            self.envelope = e.envelope
          end
        end
        e.class != FalseClass
      end
    end
    @after_enved_hook = Hook.new("@after_enved_hook", 2, "\
lambda do |pos, reason| ... end: called after redrawing new or changed
breakpoints.  POS is @envelope's x-position, and REASON is one of
Enved_add_point, Enved_delete_point, Enved_move_point.")
    set_help
    create
  end
  attr_accessor :click_time
  attr_reader :before_enved_hook
  attr_reader :after_enved_hook
  alias help description
  
  def inspect
    format("#<%s: name: %s, envelope: %s>", self.class, @name.inspect, @envelope.to_string)
  end
  
  def envelope=(new_env)
    super
    redraw
    self.envelope
  end

  def axis_bounds
    [@x0, @x1, @y0, @y1]
  end
  
  def axis_bounds=(bounds)
    if bounds.kind_of?(Array) and bounds.length == 4
      @x0, @x1, @y0, @y1 = bounds.map do |x| x.to_f end
      self.envelope = @init
    else
      error("%s#%s: need an array of four numbers [x0, x1, y0, y1], %s",
            self.class, get_func_name, bounds.inspect)
    end
  end

  def point(idx, *args)
    if args.length > 0
      super
      redraw
    end
    @envelope[idx * 2, 2]
  end

  def create
    if widget?(@drawer)
      show_widget(@drawer)
    else
      create_enved
    end
  end
  alias open create
  
  def close
    hide_widget(@drawer)
  end

  def clear
    self.envelope = @init
  end
  
  private
  def set_help
    super
    self.description += "\
#
# make_xenved(name, parent, *rest)
#   name     String
#   parent   Widget
#   *rest
#     :envelope,    [0, 0, 1, 1]   x0, y0, x1, y1, ...
#     :axis_bounds, [0, 1, 0, 1]   x0, x1, y0, y1
#     :args,        []             Motif properties
#     :axis_label,  nil            if axes labels should have
#                                  other values than axis_bounds,
#                                  (see dlocsig.rb)
#
# class Xenved < Enved (see xm-enved.scm)
#   initialize(name, parent, env, axis_bounds, args, axis_label)
#   before_enved_hook              lambda do |pos, x, y, reason| ... end
#   after_enved_hook               lambda do |pos, reason| ... end
#
# getter and setter:
#   click_time=(val)
#   click_time
#   axis_bounds=(new_bounds)
#   axis_bounds
#
# interactive methods:
#   create   (alias open)
#   close
#   clear
#   help     (alias info and description)

# more examples in effects.rb

xe = xenved_test
xe.envelope                         # --> [0.0, 0.0, 1.0, 1.0]
xe.click_time                       # --> 0.2
xe.envelope = [0, 1, 1, 1]
# three clicks later
xe.envelope                         # --> [0.0, 1.0,
                                    #      0.190735694822888, 0.562264150943396,
                                    #      0.632152588555858, 0.932075471698113,
                                    #      0.848773841961853, 0.316981132075472,
                                    #      1.0, 1.0]
xe.help                             # this help
"
  end

  if provided? "xm"
    def create_enved
      @drawer = RXtCreateManagedWidget(@name, RxmDrawingAreaWidgetClass, @parent, @args)
      @dpy = RXtDisplay(@drawer)
      @window = RXtWindow(@drawer)
      RXtAddCallback(@drawer, RXmNresizeCallback, lambda do |w, c, i| draw_axes_cb end)
      RXtAddCallback(@drawer, RXmNexposeCallback, lambda do |w, c, i| draw_axes_cb end)
      RXtAddEventHandler(@drawer, RButtonPressMask, false,
                         lambda do |w, c, e, f| mouse_press_cb(e) end)
      RXtAddEventHandler(@drawer, RButtonReleaseMask, false,
                         lambda do |w, c, e, f| mouse_release_cb end)
      RXtAddEventHandler(@drawer, RButtonMotionMask, false,
                         lambda do |w, c, e, f| mouse_drag_cb(e) end)
      new_cursor = RXCreateFontCursor(@dpy, RXC_crosshair)
      RXtAddEventHandler(@drawer, REnterWindowMask, false,
                         lambda do |w, c, e, f| RXDefineCursor(@dpy, @window, new_cursor) end)
      RXtAddEventHandler(@drawer, RLeaveWindowMask, false,
                         lambda do |w, c, e, f| RXUndefineCursor(@dpy, @window) end)
    end

    def clear_window
      RXClearWindow(@dpy, @window)
    end
    
    def fill_arc(x, y, width, height, angle1, angle2)
      RXFillArc(@dpy, @window, @gc, x, y, width, height, angle1, angle2)
    end

    def draw_line(x1, y1, x2, y2)
      RXDrawLine(@dpy, @window, @gc, x1, y1, x2, y2)
    end
  elsif provided? "xg"
    def create_enved
      @drawer = Rgtk_drawing_area_new()
      Rgtk_widget_set_events(@drawer, RGDK_ALL_EVENTS_MASK)
      Rgtk_widget_show(@parent)
      Rgtk_box_pack_start(RGTK_BOX(@parent), @drawer, true, true, 10)
      Rgtk_widget_show(@drawer)
      Rgtk_widget_set_name(@drawer, @name)
      @window = Rwindow(@drawer)
      Rgdk_window_set_background(@window, graph_color)
      Rgtk_widget_set_size_request(@drawer, -1, 200)
      add_event_handler(@drawer, "expose_event") do |w, e, d|
        draw_axes_cb
        false
      end
      add_event_handler(@drawer, "configure_event") do |w, e, d|
        draw_axes_cb
        false
      end
      add_event_handler(@drawer, "button_press_event") do |w, e, d|
        mouse_press_cb(RGDK_EVENT_BUTTON(e))
        false
      end
      add_event_handler(@drawer, "button_release_event") do |w, e, d|
        mouse_release_cb
        false
      end
      add_event_handler(@drawer, "motion_notify_event") do |w, e, d|
        ev = RGDK_EVENT_MOTION(e)
        if Ris_hint(ev) == 1
          xy = Rgdk_window_get_pointer(Rwindow(ev))
          if (xy[3] & RGDK_BUTTON1_MASK).nonzero?
            mouse_drag_cb(xy[1], xy[2])
          end
        else
          if (Rstate(ev) & RGDK_BUTTON1_MASK).nonzero?
            mouse_drag_cb(ev)
          end
        end
        false
      end
      new_cursor = Rgdk_cursor_new(RGDK_CROSSHAIR)
      old_cursor = Rgdk_cursor_new(RGDK_LEFT_PTR)
      add_event_handler(@drawer, "enter_notify_event") do |w, e, d|
        Rgdk_window_set_cursor(Rwindow(w), new_cursor)
        false
      end
      add_event_handler(@drawer, "leave_notify_event") do |w, e, d|
        Rgdk_window_set_cursor(Rwindow(w), old_cursor)
        false
      end
    end

    def clear_window
      Rgdk_window_clear(@window)
    end

    def fill_arc(x, y, width, height, angle1, angle2)
      Rgdk_draw_arc(RGDK_DRAWABLE(@window), @gc, true, x, y, width, height, angle1, angle2)
    end

    def draw_line(x1, y1, x2, y2)
      Rgdk_draw_line(RGDK_DRAWABLE(@window), @gc, x1, y1, x2, y2)
    end
  else
    error "neither Motif nor Gtk?"
  end

  # If the last hook procedure returns false, change the envelope,
  # otherwise the hook procedure is responsible.
  def run_before_enved_hook(x, y, reason)
    if @before_enved_hook.empty?
      true
    else
      e = nil
      @before_enved_hook.run_hook do |prc| e = prc.call(@mouse_pos / 2, x, y, reason) end
      e.class == FalseClass
    end
  end
  
  def add_envelope_point(x, y)
    idx = @mouse_pos
    test_env = @envelope.to_pairs
    if cur_pair = test_env.assoc(x)
      idx = test_env.index(cur_pair) * 2
      @envelope[idx + 1] = y
    else
      if cur_pair = test_env.detect do |pair| x < pair[0] end
        idx = test_env.index(cur_pair) * 2
        @envelope.insert(idx, x, y)
      end
    end
    @mouse_pos = idx
  end
  
  Mouse_radius = 0.03

  def mouse_press_cb(e)
    x = ungrfx(Rx(e))
    y = ungrfy(Ry(e))
    pos = false
    @envelope.to_pairs.each_with_index do |pair, idx|
      if (pair[0] - x).abs < Mouse_radius and (pair[1] - y).abs < Mouse_radius
        pos = idx * 2
        break
      end
    end
    @mouse_new = (not pos)
    @mouse_down = Time.now.to_f
    if pos
      @mouse_pos = pos
    else
      if run_before_enved_hook(x, y, Enved_add_point)
        add_envelope_point(x, y)
      end
      redraw
      @after_enved_hook.call(@mouse_pos / 2, Enved_add_point)
    end
  end

  # To prevent unexpected point movements if position is near first or
  # last point.
  Secure_distance = 0.0001
  
  def mouse_drag_cb(e, gtk = false)
    if gtk
      x = ungrfx(e)
      y = ungrfy(gtk)
    else
      x = ungrfx(Rx(e))
      y = ungrfy(Ry(e))
    end
    lx = if @mouse_pos.zero?
           @envelope[0]
         elsif @mouse_pos >= (@envelope.length - 2)
           @envelope[-2]
         else
           [@envelope[@mouse_pos - 2],
            [x + Secure_distance, @envelope[@mouse_pos + 2] - Secure_distance].min].max
         end
    if run_before_enved_hook(lx, y, Enved_move_point)
      @envelope[@mouse_pos, 2] = [lx, y]
    end
    redraw
    @after_enved_hook.call(@mouse_pos / 2, Enved_move_point)
  end
  
  def mouse_release_cb
    @mouse_up = Time.now.to_f
    if (not @mouse_new) and (@mouse_up - @mouse_down) <= @click_time and
        @mouse_pos.nonzero? and @mouse_pos < (@envelope.length - 2)
      if run_before_enved_hook(@envelope[@mouse_pos], @envelope[@mouse_pos + 1], Enved_delete_point)
        @envelope.slice!(@mouse_pos, 2)
      end
      redraw
      @after_enved_hook.call(@mouse_pos / 2, Enved_delete_point)
    end
    @mouse_new = false
  end

  def draw_axes_cb
    @px0, @py0, @px1, @py1 = draw_axes(@drawer, @gc, @name, @lx0, @lx1, @ly0, @ly1)
    redraw
  end

  Mouse_d = 10
  Mouse_r = 5
  
  def redraw
    if is_managed(@drawer) and @px0 and @py0 > @py1
      clear_window
      draw_axes(@drawer, @gc, @name, @lx0, @lx1, @ly0, @ly1)
      lx = ly = nil
      @envelope.each_pair do |x, y|
        cx = grfx(x)
        cy = grfy(y)
        fill_arc(cx - Mouse_r, cy - Mouse_r, Mouse_d, Mouse_d, 0, 360 * 64)
        draw_line(lx, ly, cx, cy) if lx
        lx, ly = cx, cy
      end
    end
  end

  def ungrfx(x)
    if @px0 == @px1
      @x0
    else
      [@x1, [@x0, @x0 + (@x1 - @x0) * ((x - @px0) / (@px1.to_f - @px0))].max].min
    end
  end
  
  def ungrfy(y)
    if @py0 == @py1
      @y1
    else
      [@y1, [@y0, @y0 + (@y1 - @y0) * ((@py0 - y) / (@py0.to_f - @py1))].max].min
    end
  end

  def grfx(x)
    if @px0 == @px1
      @px0
    else
      [@px1, [@px0, (@px0 + (@px1 - @px0) * (x - @x0) / (@x1 - @x0)).round].max].min
    end
  end

  def grfy(y)
    if @py0 == @py1
      @py0
    else
      [@py0, [@py1, (@py1 + (@py0 - @py1) * (y - @y1) / (@y0 - @y1)).round].max].min
    end
  end
end if provided? "xm" or provided? "xg"

# xm-enved.rb ends here
