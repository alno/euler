import Data.List

isCube x = x == r*r*r where r = round $ exp (log (fromIntegral x) / 3)

cubes = map (\x -> x*x*x) [3..]
cubesDigits = map (sort.show) cubes

perms s = perms' s True

last2 = nub $ map (reverse.take 2.reverse.show.(\x -> x*x*x)) [10..99]

perms' [] _ = [[]]
perms' s first | length s == 2 = intersect res last2
               | otherwise = res
  where ns = if first then delete '0' $ nub s else nub s
        res = concat [ map (x:) (perms' (delete x s) False) | x <- ns ]

numPerm x = length $ nub [ n | n <- map read (perms (show x)), n >= x, isCube n ]

numPerm' x = length $ filter (dx==) $ takeWhile (\d -> length d <= lx) cubesDigits
  where dx = sort $ show x
        lx = length dx

first5Perms = head [ c | c <- cubes, let p = numPerm' c, p == 5 ]

main :: IO ()
main = print first5Perms
