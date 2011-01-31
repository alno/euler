minLength = 50
        
countsE :: [Integer]
countsNE :: [Integer]

cntE n = countsE !! (n-1) + countsNE !! (n-1)
cntNE n = sum (map (countsE !!) [0..n-minLength])

countsE = 1 : 1 : map cntE [2..]
countsNE = 0 : 0 : map cntNE [2..]

counts = zipWith (+) countsE countsNE

main :: IO ()
main = print (length (takeWhile (<=10^6) counts))
