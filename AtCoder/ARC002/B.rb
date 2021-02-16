require 'date'

ymd = Date.parse(gets().chomp().gsub('/', '-'))

year = ymd.year
month = ymd.month
day = ymd.day

while !(year % month == 0 && year / month % day == 0) do
	ymd = ymd.next
	year = ymd.year
	month = ymd.month
	day = ymd.day
end

puts sprintf('%02d/%02d/%02d', year, month, day)