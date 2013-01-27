import Data.List.Split

firstLineDist = tail . scanl (+) 0

inf = 10^10

nextLineDist prevDist curLine = tail $ scanl nextDist inf $ prevDist `zip` curLine
  where nextDist leftDist (topDist, curVal) = min leftDist topDist + curVal

minDist matrix = last $ foldl nextLineDist (firstLineDist $ head matrix) (tail matrix)

main = do
  content <- readFile "../data/problem81.txt"
  let matrix = map (map read . splitOn ",") $ init $ splitOn "\r\n" content :: [[Integer]]

  print $ minDist matrix
