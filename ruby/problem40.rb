#!/usr/bin/env ruby

digits = []

i = 1

while digits.length < 1000000
  digits.push *(i.to_s.split(//).map(&:to_i))
  i = i + 1
end

puts digits[0] * digits[9] * digits[99] * digits[999] * digits[9999] * digits[99999] * digits[999999]
