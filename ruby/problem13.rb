#!/usr/bin/env ruby

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem13.txt' ) do |f|
  @numbers = f.readlines.map &:to_i
end

summ = @numbers.inject :+

first_digits = summ.to_s[0..9]

puts first_digits
