import Ratio

-- c/d < a/b < e/d
-- c < a*d / b < e

maxDenominator = 10^6

-- nextCandidates x = [ num % den | den <- [1..maxDenominator], let num = truncate (fromIntegral (numerator x * den) / fromIntegral (denominator x) ) + 1 ]

-- next x = minimum (nextCandidates x)

nextApp mx den pnum pden | den > maxDenominator = mx
                         | otherwise            = ((nextApp $! (min mx nx)) $! (den+1)) pnum pden
  where num = ((pnum * den) `div` pden ) + 1
        nx  = num % den

next x = nextApp (x + 1%1) 1 (numerator x) (denominator x)

prevApp mx den pnum pden | den > maxDenominator = mx
                         | md == 0              = (prevApp mx $! (den+1)) pnum pden
                         | otherwise            = ((prevApp $! (max mx nx)) $! (den+1)) pnum pden
  where (num,md) = ((pnum * den) `divMod` pden )
        nx  = num % den

prev x = prevApp (x - 1%1) 1 (numerator x) (denominator x)

nums = iterate next (1 % maxDenominator)

main :: IO ()
main = print (prev (3%7))
