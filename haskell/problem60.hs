
noDivisors []    x                  = True
noDivisors (h:t) x | h*h > x        = True
                   | x `mod` h == 0 = False
                   | otherwise      = noDivisors t x

-- List of prime numbers
primes :: [Int]
primes = 2 : filter (noDivisors primes) [3,5..]

-- Function to check is the number is prime
isPrime = noDivisors primes

conc a b = read (show a ++ show b)

isAddPrime [] p = True
isAddPrime (h:t) p = isPrime (conc p h) && isPrime (conc h p) && isAddPrime t p

expandPrimes set = [ s:set | s <- dropWhile (<=m) primes, isAddPrime set s ] where m = head set

data Variant = Variant{ set :: [Int], cur :: Int } deriving (Eq, Show)

instance Ord Variant where
    compare x y = compare (cur x) (cur y)

splitPrimeVariants (v1:t1@(v2:_)) | v1 <  v2 = ([v1],t1)
                                  | v1 <= v2 = (v1:p1,p2) where (p1,p2) = splitPrimeVariants t1

updateVariants [] _ = []
updateVariants (v:t) c = v{cur = c}:updateVariants t c

expandPrimeVariants vars = let (p1,p2) = splitPrimeVariants vars; m1 = cur (head p1); m2 = cur (head p2) in
                               [ Variant (s:(set s1)) s | s <- takeWhile (<=m2) (dropWhile (<=m1) primes), s1 <- p1, isAddPrime (set s1) s ] ++ expandPrimeVariants (updateVariants p1 m2 ++ p2)

initial = map (\x -> Variant [x] x ) primes

thesePrimes = iterate expandPrimeVariants initial
