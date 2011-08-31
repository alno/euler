import Char

digitSum x = sum (map digitToInt (show x))

sums = [ s | a <- [1..99], digitSum a > 3, b <- [1..99], let s = digitSum (a^b) ]

-- Main
main :: IO ()
main = print (maximum sums)
