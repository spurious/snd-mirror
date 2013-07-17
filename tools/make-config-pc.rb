#! /usr/bin/env ruby 

# ruby make-config-pc.rb > ruby.pc 

require "rbconfig" 
include RbConfig 
version = CONFIG["ruby_version"] 
arch = CONFIG["arch"] 
rubyhdrdir = CONFIG["rubyhdrdir"].chomp("/") 
dldflags = CONFIG["DLDFLAGS"] 
librubyarg = CONFIG["LIBRUBYARG"] 
libs = CONFIG["LIBS"] 

print <<OUT 
Name: Ruby 
Description: Object Oriented Script Language 
Version: #{version} 
URL: http://www.ruby-lang.org 
Cflags: -I#{rubyhdrdir}/#{arch} -I#{rubyhdrdir} 
Libs: #{dldflags} #{librubyarg} #{libs} 
Requires: 
OUT
