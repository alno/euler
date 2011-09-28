
intSqrt = ceiling . sqrt . fromIntegral

isSqr x = s * s == x where s = intSqrt x

squares = [x*x | x <- [1..]]


solution d = head [ intSqrt s | y <- squares, let s = d * y + 1, isSqr s ]

solutions = [ (d, solution d) | d <- [1..], not (isSqr d) ]

main = print "Not completed"
