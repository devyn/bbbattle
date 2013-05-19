#!/usr/bin/env ruby

if ARGF.read(8) == "bbbout1:"
  puts "bbbout version 1"
else
  abort "E: unknown or invalid bbbout format!"
end

width, height = ARGF.read(4).unpack("nn")

puts "width: #{width}, height: #{height}"

if ARGF.read(1) != "T"
  abort "E: expected start of team section in header at byte #{ARGF.pos}"
end

n_teams = ARGF.read(2).unpack("n")[0]

puts "#{n_teams} teams:"

teams = Hash[n_teams.times.map {
  team_id, r, g, b = ARGF.read(5).unpack("s>CCC")

  puts "  id #{team_id}: rgb(#{r},#{g},#{b})"

  [team_id, [r, g, b]]
}]

def read_cell_set
  points = []

  rows = ARGF.read(2).unpack("n")[0]

  rows.times {
    y, n_cells = ARGF.read(4).unpack("nn")

    points += ARGF.read(n_cells * 2).unpack("n*").map { |x| [x,y] }
  }

  points
end

while !ARGF.eof? and ARGF.read(1) == 'g'
  gen_id = ARGF.read(4).unpack("N")[0]

  puts "- generation #{gen_id}"

  while !ARGF.eof? and ARGF.read(1) == 't'
    team_id = ARGF.read(2).unpack("s>")[0]

    if ARGF.read(1) == "a"
      alive_cells = read_cell_set
    else
      abort "E: expected start of alive cells section at byte #{ARGF.pos}"
    end

    if ARGF.read(1) == "d"
      dying_cells = read_cell_set
    else
      abort "E: expected start of dying cells section at byte #{ARGF.pos}"
    end

    ac = alive_cells.size
    dc = dying_cells.size

    if team_id == -1
      puts "    neutral:\t\t#{ac + dc} cells: #{ac} alive, #{dc} dying"
    else
      puts "    rgb(#{teams[team_id].join(",")}):\t#{ac + dc} cells: #{ac} alive, #{dc} dying"
    end
  end

  if !ARGF.eof?
    ARGF.pos -= 1
  end
end
