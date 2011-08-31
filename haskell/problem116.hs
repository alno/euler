
cnt step l 0 = 1
cnt step l n | n >= step = l !! (n-step) + l !! (n-1)
             | otherwise = l !! (n-1)

countsFor step = counts where counts = 1 : 1 : map (cnt step counts) [2..]

sumCounts n = (countsFor 2 !! n) + (countsFor 3 !! n) + (countsFor 4 !! n) - 3

main :: IO ()
main = print (sumCounts 50)
