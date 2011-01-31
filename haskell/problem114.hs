
        
countsE :: [Integer]
countsNE :: [Integer]

cntE n = countsE !! (n-1) + countsNE !! (n-1)
cntNE n = sum (map (countsE !!) [0..n-3])

countsE = 1 : 1 : map cntE [2..]
countsNE = 0 : 0 : map cntNE [2..]

counts = zipWith (+) countsE countsNE

main :: IO ()
main = print (counts !! 50)

-- n-3-1 = n - 4
-- n-n+3+2-1  = 2

-- 0..n-4