import Data.Ratio
import Data.List
import Data.Function

sqrtFloor = floor . sqrt . fromIntegral

sqrtExpansionStep s a0 (m,d,a) = (m',d',a')
  where m' = d*a - m
        d' = (s - m' * m') `div` d
        a' = (a0 + m') `div` d'

sqrtExpansion n = map d $ iterate (sqrtExpansionStep n a) (0,1,a)
  where a         = sqrtFloor n
        d (_,_,x) = x

isSqr x = s * s == x where s = sqrtFloor x

convergent (h:t) 1 = h % 1
convergent (h:t) n = h % 1 + 1 / convergent t (n-1)

convergents n = map (convergent $ sqrtExpansion n) [1..]

solution d = head $ [ x | c <- convergents d, let x = numerator c, let y = denominator c, x*x - d*y*y == 1 ]
solutions = [ (d, solution d) | d <- [1..1000], not (isSqr d) ]

main =
  print $ fst $ maximumBy (compare `on` snd) solutions
