h, w = gets.split.map(&:to_i)

puts ((w + 1) / 2) * ((h + 1) / 2) + ((w / 2) * (h / 2))