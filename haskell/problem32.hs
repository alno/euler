-- Need improvements

import Data.List

exclude what = filter (\x -> notElem x what)

sublists [] = [([],[])]
sublists (h:t) = map (\x -> (fst x,h:snd x)) st ++ map (\x -> (h:fst x,snd x)) st where st = sublists t

pandigitProductFor prod (aDig,bDig) | aDig == "" || bDig == "" = Nothing
                                    | null prods               = Nothing
                                    | otherwise                = Just (head prods)
  where prods = [ (a,b) | ad <- permutations aDig, bd <- permutations bDig, let a = read ad, let b = read bd, a < b && a * b == prod ]

pandigitProduct prod mDig | null prods = Nothing
                          | otherwise  = Just (head prods)
  where prods = [ ab | abDig <- (sublists mDig), let abm = pandigitProductFor prod abDig, abm /= Nothing, let Just ab = abm ]

digits = ['1'..'9']

-- 100 * 100 = 10000 3 3 5
-- 1000 * 100 = 10000 2 3 5

pandigitProducts = [prod | (pDig,mDig) <- tail . sublists $ digits, length pDig >= 4, length pDig <= 5, pd <- permutations pDig, let prod = read pd, let ab = pandigitProduct prod mDig, ab /= Nothing, let Just (a,b) = ab ]
sumProducts = sum pandigitProducts

main:: IO ()
main = print sumProducts