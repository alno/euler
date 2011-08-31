#!/usr/bin/env ruby

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem99.txt' ) do |f|
  @numbers = f.readlines.map{|s| s.split(',').map(&:to_i) }.map{|p| Math.log( p[0] ) * p[1] }
end

puts( @numbers.index( @numbers.max ) + 1 )
