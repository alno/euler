import Ratio
import Data.List

-- 10 * a + x / 10 * x + b = a/b
-- 10 * x + a / 10 * b + x = a/b

-- b * (10 * a + x) = a * (10*x + b)

isValidFraction x a b = b * (10 * a + x) == a * (10*x + b)

fractions = nub [ (10 * a + x) % (10 * x + b) | a <- [1..9], b <- [1..9], x <- [1..9], a < x, isValidFraction x a b]
den = denominator $ product fractions

main :: IO ()
main = print den
