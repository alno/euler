import Data.List.Split
import Data.List
import Data.Function (on)
import Data.Map ((!))
import qualified Data.Map as M

firstLineDist = tail . scanl (+) 0

inf = 10^10

minDist matrix = step initialQueue initialDist
  where rows = length matrix
        cols = length (matrix !! 0)
        initialQueue = [ (r,c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1] ]
        initialDist = M.fromList ( ((0,0), matrix !! 0 !! 0) : map (\c -> (c, inf)) (tail initialQueue) )

        inMatrix (r,c) = r >= 0 && c >= 0 && r < rows && c < cols
        neighbors (r,c) = filter inMatrix [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

        step []    dist = dist ! (rows - 1, cols - 1)
        step queue dist = step newQueue newDist
          where cur = minimumBy (compare `on` (dist !)) queue
                curDist = dist ! cur

                neighborDist = M.fromList $ map neighborDist $ neighbors cur
                  where neighborDist n@(r,c) = (n, curDist + matrix !! r !! c)

                newQueue = delete cur queue
                newDist = M.unionWith min dist neighborDist

main = do
  content <- readFile "../data/problem83.txt"
  let matrix = map (map read . splitOn ",") $ init $ splitOn "\n" content :: [[Integer]]

  print $ minDist matrix
