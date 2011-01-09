import Data.List

hasDigits digits num = digits == digitsFor num

digitsFor num = sort (show num)

numbers = [ x | x <- [1..], let sameDigits = hasDigits (digitsFor x), all sameDigits [ 2*x, 3*x, 4*x, 5*x, 6*x ] ]