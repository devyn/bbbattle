#!/usr/bin/env ruby

unless ARGV.size >= 1
  abort "usage: bbbout-to-bbbattle.rb <bbbout-file>"
end

$bbbout = File.open(ARGV[0], "rb")

if $bbbout.read(8) != "bbbout1:"
  abort "E: unknown or invalid bbbout format!"
end

puts $bbbout.read(4).unpack("nn").join(" ")

if $bbbout.read(1) != "T"
  abort "E: expected start of team section in header at byte #{$bbbout.pos}"
end

teams = Hash[$bbbout.read(2).unpack("n")[0].times.map {
  trgb = $bbbout.read(5).unpack("s>CCC")
  [trgb.shift, trgb]
}]

def read_cell_set
  points = []

  rows = $bbbout.read(2).unpack("n")[0]

  rows.times {
    y, n_cells = $bbbout.read(4).unpack("nn")

    points += $bbbout.read(n_cells * 2).unpack("n*").map { |x| [x,y] }
  }

  points
end

if $bbbout.read(1) != "g"
  abort "E: expected start of generation at byte #{$bbbout.pos}"
end

gen_id = $bbbout.read(4).unpack("N")[0]

if gen_id != 0
  abort "E: first generation is not zero!"
end

while !$bbbout.eof? and $bbbout.read(1) == 't'
  team_id = $bbbout.read(2).unpack("s>")[0]

  if team_id == -1
    abort "E: neutral cells not allowed in zeroth generation!"
  elsif !teams.include? team_id
    abort "E: unknown team id #{team_id} (corrupt data?)"
  end

  print teams[team_id].join(" ") + ":"

  if $bbbout.read(1) == "a"
    alive_cells = read_cell_set

    print "a"
    print alive_cells.map { |pt| pt.join(",") }.join(" ")
    print "."
  else
    abort "E: expected start of alive cells section at byte #{$bbbout.pos}"
  end

  if $bbbout.read(1) == "d"
    dying_cells = read_cell_set

    print "d"
    print dying_cells.map { |pt| pt.join(",") }.join(" ")
    puts  "."
  else
    abort "E: expected start of dying cells section at byte #{$bbbout.pos}"
  end
end
