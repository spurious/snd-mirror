# nb.rb -- translation of nb.rb

# Translator/Author: Michael Scholz <scholz-micha@gmx.de>
# Last: Wed Apr 09 00:08:01 CEST 2003
# Version: $Revision: 1.1 $

# User functions; all can be used independently
#  make_nb(path)            set mouse hooks
#  remove_nb()              remove mouse hooks
#  write_nb(*args)          write info in database
#  delete_nb(path, file)    delete info in database
#  prune_nb(path)           delete empty entries

# class NB (see nb.scm)
#  initialize(path)
#  remove()
#  nb(note, file), unb(file), prune_db
#  files_popup_info(type, position, file)
#  files_popup_quit(type, position, file)
#  file_info(file)

require "examp"

$nb_database = "nb" unless defined? $nb_database

def make_nb(path = $nb_database)
  doc("make_nb([path = $nb_database])
Install mouse enter and leave hooks in Files viewer.\n") if path == :help
  nb = NB.new(path)
  if defined? $mouse_enter_label_hook and $mouse_enter_label_hook
    old_after = $mouse_enter_label_hook
    $mouse_enter_label_hook = Hook.new("original") do |t, p, n|
      old_after.call(t, p, n) if old_after
    end
    $mouse_enter_label_hook.add_hook!("NB_Enter") do |t, p, n| nb.files_popup_info(t, p, n) end
  else
    $mouse_enter_label_hook = Hook.new("NB_Enter") do |t, p, n| nb.files_popup_info(t, p, n) end
  end
  if defined? $mouse_leave_label_hook and $mouse_leave_label_hook
    old_after = $mouse_leave_label_hook
    $mouse_leave_label_hook = Hook.new("original") do |t, p, n|
      old_after.call(t, p, n) if old_after
    end
    $mouse_leave_label_hook.add_hook!("NB_Leave") do |t, p, n| nb.files_popup_quit(t, p, n) end
  else
    $mouse_leave_label_hook = Hook.new("NB_Leave") do |t, p, n| nb.files_popup_quit(t, p, n) end
  end
  true
end

def remove_nb(doc = nil)
  doc("remove_nb()
Remove mouse enter and leave hooks in Files view.\n") if doc == :help
  $mouse_enter_label_hook.remove_hook!("NB_Enter")
  $mouse_leave_label_hook.remove_hook!("NB_Leave")
  NB.new.remove()
end

def write_nb(*args)
  doc("write_nb(*args)
     :path,    $nb_database (#$nb_database)
     :file,    nil
Write ARGS in the dbm database of current file.  ARGS means
format(*args).  If you want a special database or snd file you can
give the named options as first options. e.g.

write_nb(:file, file_name(3), :path, 'dbfile', \"%d points in curve\", 3)
") if get_args(args, :help, false)
  path = get_args(args, :path, $nb_database)
  file = get_args(args, :file, nil)
  while args[0] == :path or args[0] == :file
    args.shift
    args.shift
  end
  NB.new(path).nb(format(*args), file)
end

def delete_nb(path = $nb_database, file = nil)
  doc("delete_nb([path = $nb_database [, file = nil]])
Removes FILE's info.\n") if path == :help
  NB.new(path).unb(file)
end

def prune_nb(path = $nb_database)
  doc("prune_nb([path = $nb_database]
Clean non-existent file references out of the database.\n") if path == :help
  NB.new(path).prune_db()
end

class NB
  doc "#{self.class} #{self.name} is a translation of nb.scm.
Provide pop-up help in the Files viewer.
If you have `dbm', any data associated with the file in the dbm
database will also be posted.  The database name is defined by
$nb_database (#{$nb_database}).  You may replace the require statement
with an other database library which you have, e.g. `gdbm'.
The function nb(note[, file=@current_file]) adds NOTE to the info
currently associated with FILE.
To remove a file's info, unb([file=@current_file]).
To clean non-existent file references out of the database,
prune_db()."

  require "dbm"
  @@current_file = nil
  Current_file_viewer = 0
  Previous_file_viewer = 1
  Region_viewer = 2

  def initialize(path = $nb_database)
    @nb_database = path
  end

  def remove
    @@current_file = nil
  end

  def nb(note, file = nil)
    doc("nb(note[, file=nil])
Adds NOTE to the info associated with FILE (defaults to @@current_file.\n") if note == :help
    file = (file or (@@current_file or file_name(false)))
    ptr = file_info(file)[0]
    if ptr
      current_note = (ptr.key?(file) ? ptr.fetch(file) : "")
      ptr.store(file, current_note.empty? ? note : (note + "\n" + current_note))
      ptr.close
    end
    files_popup_info(Current_file_viewer, nil, file)
  end

  def unb(file = nil)
    doc("unb([file=nil])
Removes FILE's info from the nb (dbm) data base.  FILE defaults to
@@current_file.\n") if file == :help
    file = (file or (@@current_file or file_name(false)))
    ptr = file_info(file)[0]
    if ptr
      ptr.delete(file)
      ptr.close
    end
    files_popup_info(Current_file_viewer, nil, file)
  end

  def prune_db(doc = nil)
    doc("prune_db()
Cleans up the nb (dbm) data base by removing references to
non-existent files.\n") if doc == :help
    ptr = DBM.open(@nb_database) rescue warn("DBM.open(#{@nb_database}")
    ptr.delete_if do |k, v| k.empty? end if ptr
    ptr.close
  end

  def files_popup_info(type, position = nil, file = nil)
    doc("files_popup_info(type, position, file)
It's intended as a mouse-enter-label hook function.
It causes a description of the file to popup when the mouse crosses
the filename.\n") if type == :help
    ptr, str = file_info(file)
    alert_color = make_color(1.0, 1.0, 0.94)
    unless type == Region_viewer
      @@current_file = file
      help_dialog(file, str)
      help_widget = dialog_widgets()[14]
      unless help_widget
        files_dialog = dialog_widgets()[8]
        files_position = widget_position(files_dialog)
        files_size = widget_size(files_dialog)
        set_widget_position(help_widget, [files_position[0] + 10, files_position[1] + 10])
      end
      recolor_widget(help_widget, alert_color) if help_widget
    end
    ptr.close if ptr
  end

  def files_popup_quit(type, *args)
    doc("files_popup_quit(type, *args)
It's intended as a mouse-leave-label hook function.
It unhighlights the popped-up info about a file as the mouse leaves
the associated label.\n") if type == :help
    widget = dialog_widgets()[14]
    recolor_widget(widget, basic_color()) if widget
  end

  def file_info(file)
    ptr = DBM.open(@nb_database) rescue warn("DBM.open(#{@nb_database}")
    chans = mus_sound_chans(file)
    srate = mus_sound_srate(file)
    len = format("%.3f", (mus_sound_samples(file).to_f / (chans * srate)))
    d_format = mus_data_format_name(mus_sound_data_format(file))
    h_type = mus_header_type_name(mus_sound_header_type(file))
    date = Time.at(mus_sound_write_date(file)).localtime.strftime "%a %d-%b-%y %H:%M %Z"
    comm = mus_sound_comment(file)
    loops = mus_sound_loop_info(file)
    notes = ((ptr and ptr.key?(file)) ? ptr.fetch(file) : "")
    str = format("\
  chans: %d, srate: %d
 length: %.3f (%d samples)
 format: %s [%s]
written: %s
%s%s\n%s",
           chans, srate, len, mus_sound_frames(file), d_format, h_type, date,
           comm.empty? ? "" : "comment: #{comm}\n",
           loops ? "   loop: #{loops.inspect}\n" : "", (notes or ""))
    [ptr, str]
  end
  private :file_info
end

# nb.rb ends here
