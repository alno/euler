import Data.List

-- Brute-force, need improvements

noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Integer]
primes = 2 : 3 : filter (noDivisors primes) [ 6*n+ofs | n <- [1..], ofs <- [-1,1] ]

nDivsFrom c x y | m == 0 = nDivsFrom (c+1) d y
                | otherwise = (x,c)
    where (d,m) = divMod x y

nDivs = nDivsFrom 0

primeFactorsExpApp factors (h:t) x | h > x              = factors
                                   | h * h > x          = ((x,1):factors)
                                   | otherwise          = primeFactorsExpApp ((h,c):factors) t d where (d,c) = nDivs x h

primeFactorsExp = primeFactorsExpApp [] primes

solutionsFact []        = 1
solutionsFact ((p,q):t) = q*(2*c-1)+c where c = solutionsFact t

solutions = solutionsFact . primeFactorsExp

leastX = head (filter (\x -> solutions x > 10^2) [1..])

main :: IO ()
main = print "Not completed"

firstApproxes = scanl (\t p -> ((p,1):fst t, 3 * snd t - 1)) ([],1) primes

firstApprox lim = head . dropWhile (\x -> snd x <= lim) $ firstApproxes

decrFirst (h:t,s) = (t, solutionsFact t)

initial = ([],1,1)
limit = 4 * 10^6

notFilled l n = find (\x -> fst x > n && snd x > 1) l == Nothing

isIncreasing' f [] = True
isIncreasing' f ((_,h):t) = h >= f && isIncreasing' (max h f) t

isIncreasing (l,_,_) = isIncreasing' 0 l

addPrime [] x = [(x,1)]
addPrime l@((p,q):t) x | x >  p = (x,1):l
                       | x == p = (p,q+1):t
                       | x <  p = (p,q):addPrime t x

next (l,pr,so) x = (nl,pr * x, solutionsFact nl) where nl = addPrime l x
nextList p@(l,pr,_) = filter isIncreasing $ map (next p) (filter (notFilled l) . take (length l+1) $primes)

notLimit (l,pr,so) = so <= limit

sols = initial:concat (map nextList sols)
firstSol = head (dropWhile notLimit sols)

{--
 x
 n * (x * 2 - 1) + x = x * (2*n+1) - n
--}
