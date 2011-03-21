import Ratio

monets :: [Int]
monets = [1, 2, 5, 10, 20, 50, 100, 200]

counts :: [[Integer]]

countCombinations s n = sum [ counts !! i !! (n - m) | (i,m) <- zip [0..] (takeWhile (<=n) . take s $ monets) ]

counts = map (\s -> 1:map (countCombinations s) [1..]) [1..]

main :: IO ()
main = print (counts !! 7 !! 200)
