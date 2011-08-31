#!/usr/bin/env ruby

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem22.txt' ) do |f|
  @names = f.readlines.first.split(/[",]/)
end

summScore = 0

@ofs = 'A'[0]

def letterScore( name )
  score = 0
  name.each_byte do |c|
    score += c - @ofs + 1
  end
  score
end

@names.reject! {|s| s == ''}
@names.sort!
@names.each_with_index do |name,index|
  summScore += (index + 1) * letterScore( name )
end

puts summScore
