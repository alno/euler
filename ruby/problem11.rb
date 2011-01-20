#!/usr/bin/env ruby

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem11.txt' ) do |f|
  @grid = f.readlines.map{|s| s.split(" ").map{|n| n.to_i }}
end

@gridSize = @grid.length

@lineLength = 4

def hor x, y
  return 0 unless x < @gridSize - @lineLength

  (0..@lineLength-1).inject(1){|p,i| p * @grid[y][x+i]}
end

def ver x, y
  return 0 unless y < @gridSize - @lineLength

  (0..@lineLength-1).inject(1){|p,i| p * @grid[y+i][x]}
end

def dgm x, y
  return 0 unless x < @gridSize - @lineLength
  return 0 unless y < @gridSize - @lineLength

  (0..@lineLength-1).inject(1){|p,i| p * @grid[y+i][x+i]}
end

def dga x, y
  return 0 unless x > @lineLength
  return 0 unless y < @gridSize - @lineLength

  (0..@lineLength-1).inject(1){|p,i| p * @grid[y+i][x-i]}
end

max = 0

0.upto @gridSize-1 do |x|
  0.upto @gridSize-1 do |y|
    max = [:hor,:ver,:dgm,:dga].inject(max){|m,method| [ m, send( method, x, y ) ].max }
  end
end

puts max
