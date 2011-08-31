
splitByElemAcc []  c []    = []
splitByElemAcc acc c []    = [reverse acc]
splitByElemAcc acc c (h:t) | c /= h    = splitByElemAcc (h:acc) c t
                           | null acc  = splitByElemAcc [] c t
                           | otherwise = reverse acc : splitByElemAcc [] c t

splitByElem = splitByElemAcc []

orderings [a,b,c] = [(a,b),(b,c),(c,c)]

dropLast n = reverse . drop n . reverse

onlyLeft n [] = True
onlyLeft n ((l,r):t) | r == n && l /= n = False
                     | otherwise        = onlyLeft n t

isInLeft n [] = False
isInLeft n ((l,_):t) | l == n    = True
                     | otherwise = isInLeft n t

dropLeft n [] = []
dropLeft n ((l,r):t) | l == n    = dropLeft n t
                     | otherwise = (l,r):dropLeft n t

firstSymbol orderPairs = head [ n | n <- ['0'..'9'], isInLeft n orderPairs, onlyLeft n orderPairs ]

passwordAcc acc []   = reverse acc
passwordAcc acc ords = passwordAcc (d:acc) (dropLeft d ords) where d = firstSymbol ords

password = passwordAcc []

main :: IO ()
main = do
  numStr <- readFile "../data/problem79.txt"

  let orderPairs = foldr1 (++) (map orderings (map (dropLast 1) (splitByElem '\n' numStr))) in
    print (password orderPairs)
