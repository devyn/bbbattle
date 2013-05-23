File.open("#{ARGV[0]}.h", "w") { |f| f.puts "char *program_source = #{File.read(ARGV[0]).inspect};" }
