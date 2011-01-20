
splitAscendingFrom c []            = ([],Nothing,[])
splitAscendingFrom c (h:t) | h > c = let (a,b,c) = splitAscendingFrom h t in (h:a,b,c)
                           | h < c = ([],Just h,t)

splitAscending = splitAscendingFrom (pred '0')

nextRevPermutation l = let (a,m,c) = splitAscending l in case m of
                           Just b  -> Just (reverse (al ++ b:am) ++ (nb:c)) where al      = takeWhile (<b) a
                                                                                  (nb:am) = dropWhile (<b) a
                           Nothing -> Nothing

revDigits = reverse ['0'..'9']
revDigitPermutationFrom p 0 = Just p
revDigitPermutationFrom p i = let n = nextRevPermutation p in case n of
                                  Just np -> revDigitPermutationFrom np (i-1)
                                  Nothing -> Nothing

revDigitPermutation = revDigitPermutationFrom revDigits

digitPermutation i = reverse p where Just p = revDigitPermutation i

perm_1000000 = digitPermutation 999999

-- Main
main :: IO ()
main = print perm_1000000
