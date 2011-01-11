num = sum (map (\x -> x^x) [1..1000])
digits = show num
lastDigits = drop (length digits - 10) digits