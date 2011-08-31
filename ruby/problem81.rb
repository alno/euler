#!/usr/bin/env ruby

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem81.txt' ) do |f|
  @values = f.readlines.map{|l| l.split(/[, ]/).map &:to_i }
end

@sums = [ [] ]

sum = 0
0.upto @values.length - 1 do |i|
  sum += @values.first[i]
  @sums.first << sum
end

1.upto @values.length - 1 do |i|
  @sums << [ @sums.last.first + @values[i].first ]

  1.upto @values.last.length - 1 do |j|
    @sums.last << [ @sums[i-1][j], @sums[i][j-1] ].min +  @values[i][j]
  end
end

puts @sums.last.last
