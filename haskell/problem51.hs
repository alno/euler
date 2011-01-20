import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime = noDivisors primes

replaceByMask list mask value = zipWith (\x y -> if y then value else x ) list mask

familyOf numStr mask = [ num | value <- values, let str = replaceByMask numStr mask value, head str /= '0', let num = read str, isPrime num ]
    where values = if last mask then "1379" else if head mask then ['1'..'9'] else ['0'..'9']

fullMasks numStr = [ map (==x) numStr | x <- "012", length (filter (==x) numStr) > 1 ] -- [nub numStr] in general case
                                                                                       -- No mask with only one digit may produce 8 prime numbers (division by 3)

addTrue(mask,tc) = (True:mask,tc+1)

addFalse (mask,tc) = (False:mask,tc)

allMasksFrom [] = [([],0)]
allMasksFrom [_] = [([False],1)] -- No mask with last digit could may 8 prime numbers (division by 2)
allMasksFrom (False:t) = map addFalse (allMasksFrom t)
allMasksFrom (True:t)  = map addFalse tailMasks ++ map addTrue tailMasks where tailMasks = (allMasksFrom t)

filterMasks masks = [ mask | (mask,tc) <- tail masks, tc > 1 ] -- No mask with only one digit may produce 8 prime numbers (division by 3)

possibleMasks numStr = concat (map (filterMasks.allMasksFrom) (fullMasks numStr))

families number = map (familyOf numStr) (possibleMasks numStr) where numStr = show number
familySize number = maximum (map length ([]:families number))

primesWithFamilySize = map (\p -> (p,familySize p)) primes

primeWithFamilyOf8 = head (filter (\t -> snd t >= 8) primesWithFamilySize)

-- Main
main :: IO ()
main = print primeWithFamilyOf8
