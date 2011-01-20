#!/usr/bin/env ruby

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem67.txt' ) do |f|
  @numbers = f.readlines.map{|s| s.split(' ').map &:to_i }
end

@sums = [ @numbers[0] ]

1.upto @numbers.length - 1 do |i|
  prevSumm = @sums[i-1]
  currLine = @numbers[i]
  currSumm = []

  currSumm << prevSumm[0] + currLine[0]

  1.upto prevSumm.length - 1 do |j|
    currSumm << [ prevSumm[j-1], prevSumm[j] ].max + currLine[j]
  end

  currSumm << prevSumm[-1] + currLine[-1]

  @sums << currSumm
end

puts @sums.last.max