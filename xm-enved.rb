# xm-enved.rb -- Translation of xm-enved.scm

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Tue Mar 18 02:22:47 CET 2003
# Version: $Revision: 1.2 $

# Requires the Motif module (xm.so) or --with-static-xm!

# module XMEnved (see xm-enved.scm)
#  xe_envelope(drawer, new_env)
#  xe_create_enved(name, parent, args, axis_bounds)
#  xe_redraw(drawer)

require "English"
require "xm" unless $LOADED_FEATURES.grep(/xm/)
require "examp"

module XMEnved
  doc "#{self.class} #{self.name}
Envelope editor (see xm-enved.scm and enved.scm).  Works only with Motif
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

# xm-enved.rb ends here
