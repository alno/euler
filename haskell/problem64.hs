
intSqrt = floor . sqrt . fromIntegral
fullSqr x = s * s == x where s = intSqrt x

sqrtExpansionStep s a0 (m,d,a) = (m',d',a')
  where m' = d*a - m
        d' = (s - m' * m') `div` d
        a' = (a0 + m') `div` d'

sqrtExpansion n = map d $ iterate (sqrtExpansionStep n a) (0,1,a)
  where a         = intSqrt n
        d (_,_,x) = x

sqrtExpansionCycle n = cyc s1
  where a  = intSqrt n
        s0 = (0,1,a)
        s1 = sqrtExpansionStep n a s0
        cyc s@(_,_,d) | ns == s1  = [d]
                      | otherwise = d:cyc ns
          where ns = sqrtExpansionStep n a s

oddSqrtExpansionCycles = [ n | n <- [1..10000], not $ fullSqr n, odd $ length $ sqrtExpansionCycle n]

main =
  print $ length oddSqrtExpansionCycles
