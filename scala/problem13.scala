#!/usr/bin/env scala
!#

import scala.io.Source

import java.math.BigInteger

val lines = Source.fromFile("../data/problem13.txt").getLines.filter( !_.isEmpty )
val numbers = lines.map( new BigInteger( _ ) )
val sum = numbers.reduceLeft(_ add _)

println( sum.toString.take(10) )