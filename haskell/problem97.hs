import Char

digNum = 10
base = 10

data FixNum = FixNum [Int] deriving Eq

-- Printing

printNum (FixNum a) = map intToDigit a

-- Convert

intToNumA i r | length r >= digNum = r
              | otherwise          = intToNumA di (mo:r) where (di,mo) = i `divMod` base
              
intToNum n = FixNum (intToNumA n [])

-- Sum

sumNumsA a r c1 c2 | length r >= digNum = r
sumNumsA a r (d1:t1) (d2:t2)            = sumNumsA na (nd:r) t1 t2
  where (na,nd) = (d1 + d2 + a) `divMod` base
        
sumNums (FixNum n1) (FixNum n2) = FixNum (sumNumsA 0 [] (reverse n1) (reverse n2))

-- Mul

shiftNum i (FixNum n) = FixNum (drop i n ++ take i (repeat 0))

mulNumDigA a r [] d    = r
mulNumDigA a r (h:t) d = mulNumDigA na (nh:r) t d
  where (na,nh) = (h*d + a) `divMod` base

mulNumDig (FixNum n) d = FixNum (mulNumDigA 0 [] (reverse n) d)

mulNumsA r n1 []    = r
mulNumsA r n1 (h:t) = mulNumsA (sumNums (shiftNum 1 r) (mulNumDig n1 h) ) n1 t

mulNums n1 (FixNum n2) = mulNumsA (intToNum 0) n1 n2

-- Instances

instance Show FixNum where
  show = printNum

instance Num FixNum where
  (+) = sumNums
  (*) = mulNums
  
  abs x = undefined
  signum x = undefined

  fromInteger = intToNum . fromInteger

-- Exp

expNum n 0 = 1
expNum n 1 = n
expNum n e | m == 0 = n2 * n2 
           | m == 1 = n2 * n2 * n
  where (en,m) = e `divMod` 2
        n2 = expNum n en
        
lastDigits = (expNum (intToNum 2) 7830457) * (intToNum 28433) + 1

main :: IO ()
main = print lastDigits

