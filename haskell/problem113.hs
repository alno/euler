firstIncr = map (10-) [0..9]

nextIncr prev = [ sum (drop start prev) | start <- [0..9] ]

increasing :: [[Integer]]
increasing = iterate nextIncr firstIncr

countNonBouncy n = increasing !! (n - 1) !! 0 + increasing !! (n - 1) !! 1 - 10

countNonBouncyBelow n = sum (map countNonBouncy [1..n])

main :: IO ()
main = print (countNonBouncyBelow 100)
