import Data.Ratio

digitNum x = length (show x)

expansions = (3 % 2) : map (\x -> 1 + 1 / (1 + x)) expansions

count = length (filter (\x -> digitNum (numerator x) > digitNum (denominator x) ) (take 1000 expansions))

-- Main
main :: IO ()
main = print count
