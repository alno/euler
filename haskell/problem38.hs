import Data.List

isPandigital s = sort s == ['1'..'9']

pandigitalFor i s l n | nl > 9               = Nothing
                      | intersect s ns /= [] = Nothing
                      | nl < 9               = pandigitalFor (i+1) (s++ns) nl n
                      | isPandigital (s++ns) = Just (s++ns)
                      | otherwise            = Nothing
    where ns = show (n*i)
          nl = length ns + l

pandigital = pandigitalFor 1 "" 0

pandigitals :: [Integer]
pandigitals = [ read pd | n <- [1..9999], let pdm = pandigital n, pdm /= Nothing, let Just pd = pdm ]

maxPandigital = last pandigitals