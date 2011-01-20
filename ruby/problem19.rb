count = 0

1901.upto 2000 do |year|
  1.upto 12 do |month|
    time = Time.utc year, month, 1
    count += 1 if time.wday == 0
  end
end

puts count