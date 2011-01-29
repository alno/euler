
cnt l 0 = 1
cnt l n | n >= 4 = l !! (n-4) + l !! (n-3) + l !! (n-2) + l !! (n-1)
        | n >= 3 = l !! (n-3) + l !! (n-2) + l !! (n-1)
        | n >= 2 = l !! (n-2) + l !! (n-1)
        | otherwise = l !! (n-1)

counts = 1 : 1 : map (cnt counts) [2..]

main :: IO ()
main = print (counts !! 50)
