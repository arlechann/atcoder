chars = ['A', 'B', 'X', 'Y']

def min(a, b)
	if a < b
		a
	else
		b
	end
end

n = gets().chomp().to_i()
c = gets().chomp()
result = 1000000000
(0...256).each do |i|
	l = '';
	r = '';
	(0...4).each do |j|
		x = (i >> (j * 2)) & 3
		if j < 2
			l << chars[x]
		else
			r << chars[x]
		end
	end
	result = min(result, c.gsub(l, 'L').gsub(r, 'R').size())
end

puts result