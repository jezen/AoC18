#!/usr/bin/ruby
# Usage: ./2.rb < input

log = []
lines = []
acc = 0
index = 0

ARGF.each_line {|l| lines << l.to_i }

while true do
  log << acc
  acc += lines[index]
  if log.include? acc
    puts acc
    break
  elsif index == (lines.length - 1)
    index = 0
  else
    index += 1
  end
end
