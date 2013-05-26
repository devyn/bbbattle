#!/usr/bin/env ruby

unless ARGV.size >= 1
  abort "usage: bbbout-info.rb <bbbout-file>"
end

$bbbout = File.open(ARGV[0], "rb")

if $bbbout.read(8) == "bbbout1:"
  puts "bbbout version 1"
else
  abort "E: unknown or invalid bbbout format!"
end

width, height = $bbbout.read(4).unpack("nn")

puts "width: #{width}, height: #{height}"

if $bbbout.read(1) != "T"
  abort "E: expected start of team section in header at byte #{$bbbout.pos}"
end

n_teams = $bbbout.read(2).unpack("n")[0]

puts "#{n_teams} teams:"

teams = Hash[n_teams.times.map {
  team_id, r, g, b = $bbbout.read(5).unpack("s>CCC")

  puts "  id #{team_id}: rgb(#{r},#{g},#{b})"

  [team_id, [r, g, b]]
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

previous_alive_cells = {}

while !$bbbout.eof? and $bbbout.read(1) == 'g'
  gen_id = $bbbout.read(4).unpack("N")[0]

  puts "- generation #{gen_id}"

  inc_teams = []

  while !$bbbout.eof? and $bbbout.read(1) == 't'
    team_id = $bbbout.read(2).unpack("s>")[0]

    inc_teams << team_id

    if $bbbout.read(1) == "a"
      alive_cells = read_cell_set
    else
      abort "E: expected start of alive cells section at byte #{$bbbout.pos}"
    end

    if !$bbbout.eof? and $bbbout.read(1) == "d"
      dying_cells = read_cell_set
    else
      dying_cells = previous_alive_cells[team_id] || []
      $bbbout.pos -= 1 unless $bbbout.eof?
    end

    ac = alive_cells.size
    dc = dying_cells.size

    if team_id == -1
      puts "    neutral:\t\t#{ac + dc} cells: #{ac} alive, #{dc} dying"
    else
      puts "    rgb(#{teams[team_id].join(",")}):\t#{ac + dc} cells: #{ac} alive, #{dc} dying"
    end

    previous_alive_cells[team_id] = alive_cells
  end

  (previous_alive_cells.keys - inc_teams).each { |team_id|
    dc = previous_alive_cells[team_id].size

    if team_id == -1
      puts "    neutral:\t\t#{dc} cells: 0 alive, #{dc} dying"
    else
      puts "    rgb(#{teams[team_id].join(",")}):\t#{dc} cells: 0 alive, #{dc} dying"
    end

    previous_alive_cells.delete team_id
  }

  unless $bbbout.eof?
    $bbbout.pos -= 1
  end
end
