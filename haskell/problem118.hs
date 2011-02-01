import Data.List

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

-- Function to check is the number is prime
isPrime x = x > 1 && noDivisors primes x

appFst h (a,b) = (h:a,b)
appSnd h (a,b) = (a,h:b)

split []    = [([],[])]
split (h:t) = map (appFst h) sl ++ map (appSnd h) sl where sl = split t

splitNE = filter (\t -> fst t /= [] && snd t /= []) . split

numbers [] = [0]
numbers dig = [ 10*n + d | d <- dig, n <- numbers (delete d dig) ]

number = foldl1 (\x y -> 10*x + y)

buildSets [x] mn | x > mn && isPrime x = [[x]]
                 | otherwise           = []
buildSets digits mn                    = [ k:t | (dig,other) <- splitNE digits, k <- numbers dig, k > mn, isPrime k, t <- buildSets other k ] ++ [ [k] | k <- numbers digits, k > mn, isPrime k ]


countSets [x] mn | x > mn && isPrime x = 1
                 | otherwise           = 0
countSets digits mn                    = sum [ countSets other k | (dig,other) <- splitNE digits, k <- numbers dig, k > mn, isPrime k ] + sum [ 1 | k <- numbers digits, k > mn, isPrime k ]


main :: IO ()
main = print (countSets [1..9] 0)
