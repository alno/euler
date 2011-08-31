#!/usr/bin/env ruby

def normal_form( s )
  s = s.clone

  os = nil
  while os != s
    os = s.clone

    s.gsub! /IV(V*)/, '\1VIIII'
    s.gsub! /IIV(V*)/, '\1VIII'
    s.gsub! /IIIV(V*)/, '\1VII'
    s.gsub! /IIIIV(V*)/, '\1VI'
    s.gsub! /IX(X*)(I*V*)/, '\1\2VIIII'
    s.gsub! /IIX(X*)(I*V*)/, '\1\2VIII'
    s.gsub! /IIIX(X*)(I*V*)/, '\1\2VII'
    s.gsub! /IIIIX(X*)(I*V*)/, '\1\2VI'
  end

  os = nil
  while os != s
    os = s.clone

    s.gsub! /XL(L*)/, '\1LXXXX'
    s.gsub! /XXL(L*)/, '\1LXXX'
    s.gsub! /XXXL(L*)/, '\1LXX'
    s.gsub! /XXXXL(L*)/, '\1LX'
    s.gsub! /XC(C*)(X*L*)/, '\1\2LXXXX'
    s.gsub! /XXC(C*)(X*L*)/, '\1\2LXXX'
    s.gsub! /XXXC(C*)(X*L*)/, '\1\2LXX'
    s.gsub! /XXXXC(C*)(X*L*)/, '\1\2LX'
  end

  os = nil
  while os != s
    os = s.clone

    s.gsub! /CD(D*)/, '\1DCCCC'
    s.gsub! /CCD(D*)/, '\1DCCC'
    s.gsub! /CCCD(D*)/, '\1DCC'
    s.gsub! /CCCCD(D*)/, '\1DC'
    s.gsub! /CM(M*)(C*D*)/, '\1\2DCCCC'
    s.gsub! /CCM(M*)(C*D*)/, '\1\2DCCC'
    s.gsub! /CCCM(M*)(C*D*)/, '\1\2DCC'
    s.gsub! /CCCCM(M*)(C*D*)/, '\1\2DC'
  end

  s
end

def minimal_form( s )
  oos = s
  s = s.clone

  os = nil
  while os != s
    os = s.clone

    s.gsub! /VV/, 'X'
    s.gsub! /(V*)IIII/, 'IV\1'
  end

  os = nil
  while os != s
    os = s.clone

    s.gsub! /LL/, 'C'
    s.gsub! /(L*)XXXX/, 'XL\1'
  end

  os = nil
  while os != s
    os = s.clone

    s.gsub! /DD/, 'M'
    s.gsub! /(D*)CCCC/, 'CD\1'
  end

  [ s, (oos.length - s.length) ]
end

saved = 0

File.open File.join( File.dirname(__FILE__), '..', 'data', 'problem89.txt' ) do |f|
  f.readlines.each do |num|
    newnum, s = minimal_form num

    saved += s
  end
end

puts saved
