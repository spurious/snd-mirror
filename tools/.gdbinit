define gp
set gdb_print($arg0)
print gdb_output
end
document gp
Executes (object->string arg): gp memo_sound
end

define ge
call gdb_read($arg0)
call gdb_eval(gdb_result)
set gdb_print(gdb_result)
print gdb_output
end
document ge
Executes (print (eval (read arg))): ge "(+ 1 2)"
end

define gh
call g_help(scm_str2symbol($arg0), 20)
set gdb_print($1)
print gdb_output
end
document gh
Prints help string for arg: gh "enved-target"
end

define r
  run
end
