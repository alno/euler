#!/usr/bin/env ruby

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem82.txt' ) do |f|
  @values = f.readlines.map{|l| l.split(/[, ]/).map &:to_i }
end

@sums = []
@values.each_with_index do |s,i|
  @sums << [ s.first ]
end

1.upto @values.first.size - 1 do |j|
  
  @values.each_with_index do |s,i|
    @sums[i].push @sums[i].last + s[j]
  end
  
  improved = true
  while improved
    improved = false
    @values.each_with_index do |s,i|
      if i > 0 && @sums[i][j] > @sums[i-1][j] + s[j]
        @sums[i][j] = @sums[i-1][j] + s[j]
        improved = true
      elsif i < @values.length - 1 && @sums[i][j] > @sums[i+1][j] + s[j]
        @sums[i][j] = @sums[i+1][j] + s[j]
        improved = true
      end
    end
  end
  
end

puts @sums.map{|s|s.last}.min