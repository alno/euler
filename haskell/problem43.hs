import Data.List

isValid 1  _ _ 0 = False
isValid 4  _ _ x = x `mod` 2 == 0
isValid 5  a b c = (a+b+c) `mod` 3 == 0
isValid 6  _ _ x = x `mod` 5 == 0
isValid 7  a b c = (100 * a + 10 * b + c) `mod` 7 == 0
isValid 8  a b c = (100 * a + 10 * b + c) `mod` 11 == 0
isValid 9  a b c = (100 * a + 10 * b + c) `mod` 13 == 0
isValid 10 a b c = (100 * a + 10 * b + c) `mod` 17 == 0
isValid _  _ _ _ = True

numbersFrom _ _ _ [] = [0]
numbersFrom n a b digits = [ d * m + r | d <- digits, isValid n a b d, r <- numbersFrom (n+1) b d (delete d digits) ] where m = 10^(10-n)

numbers = numbersFrom 1 0 0 [0..9]

main :: IO ()
main = print (sum numbers)