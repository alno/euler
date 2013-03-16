letterCount :: Int -> Int
letterCount x | elem x [0, 4, 5, 9]             = 4 -- zero, four, five, nine
              | elem x [1, 2, 6, 10]            = 3 -- one, two, six, ten
              | elem x [3, 7, 8, 40, 50, 60]    = 5 -- three, seven, eight, forty, fifty, sixty
              | elem x [11, 12, 20, 30, 80, 90] = 6 -- eleven, twelve, twenty, thirty, eighty, ninety
              | elem x [15, 16, 70]             = 7 -- fifteen, sixteen, seventeen
              | elem x [13, 14, 18, 19]         = 8 -- thirteen, fourteen, eighteen, nineteen
              | elem x [17]                     = 9 -- seventeen
              | x < 100   = letterDCount x
              | x < 1000  = letterHCount x
              | x == 1000 = 11-- one thousand
  where
    letterDCount x = dc + oc where
      r  = x `mod` 10
      dc = letterCount (x - r)
      oc = if r == 0
        then 0
        else letterCount r

    letterHCount x = hc + oc where
      r  = x `mod` 100
      hc = 7 + letterCount (x `div` 100) -- [x/100] hundreed
      oc = if r == 0
        then 0
        else 3 + letterCount r -- and [x%100]

main =
  print $ sum $ letterCount `map` [1..1000]
