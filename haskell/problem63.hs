digitCount n = length (show n)

canBeNDigit n = digitCount (9^n) >= n

numbers = [ x | p <- takeWhile canBeNDigit [1..], b <- [1..9], let x = b^p, digitCount x == p ]

-- Main
main :: IO ()
main = print (length numbers)
