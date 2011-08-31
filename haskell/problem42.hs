import Data.List
import Data.Char

splitByElemAcc []  c []    = []
splitByElemAcc acc c []    = [reverse acc]
splitByElemAcc acc c (h:t) | c /= h    = splitByElemAcc (h:acc) c t
                           | null acc  = splitByElemAcc [] c t
                           | otherwise = reverse acc : splitByElemAcc [] c t

splitByElem = splitByElemAcc []

dropBoth n = drop n . reverse . drop n . reverse

wordNumber = sum . map (\c -> ord c - ord 'A' + 1)

intSqrt :: Int -> Int
intSqrt n = truncate (sqrt (fromIntegral n))

-- n(n+1)/2 = ((2*n+1)^2 - 1)/8

isTriangle x = s*s == y && (s-1) `mod` 2 == 0 where y = 8 * x + 1
                                                    s = intSqrt y

main :: IO ()
main = do
  wordStr <- readFile "../data/problem42.txt"

  print $ length [ n | w <- map (dropBoth 1) . splitByElem ',' $ wordStr, let n = wordNumber w, isTriangle n ]
