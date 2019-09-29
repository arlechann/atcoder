n, m = gets.chomp.split.map(&:to_i)
s = Array.new

m.times do
	k = gets.chomp.split.map(&:to_i)
	s.push(k.shift)
end

p = gets.chomp.split.map(&:to_i)
