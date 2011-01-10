import Data.Char
import Numeric

showBin x = showIntAtBase 2 intToDigit x ""

isPalyndromic s = s == reverse s

isPalyndromicDecAndBin x = isPalyndromic (show x) && isPalyndromic (showBin x)

palyndromicDecAndBin = filter isPalyndromicDecAndBin [1..999999]

sumPalyndromicDecAndBin = sum palyndromicDecAndBin
