#!/usr/bin/env ruby

def cross(x1,y1,x2,y2)
  x1*y2 - x2*y1
end

def contains_origin( t )
  s1 = cross( t[0], t[1], t[2], t[3] )
  s2 = cross( t[2], t[3], t[4], t[5] )
  s3 = cross( t[4], t[5], t[0], t[1] )

  s1 * s2 > 0 && s2 * s3 > 0 && s3 * s1 > 0
end

count = 0
File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem102.txt' ) do |f|
  f.readlines.each do |s|
    count = count + 1 if contains_origin( s.split(",").map{|n| n.to_i } )
  end
end

puts count
