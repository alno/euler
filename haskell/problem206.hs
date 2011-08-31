
isValid ['1',_,'2',_,'3',_,'4',_,'5',_,'6',_,'7',_,'8',_,'9',_,'0'] = True
isValid _                                                           = False

isValidSquare x = isValid (show (x*x))

intSqrt :: Integer -> Integer
intSqrt n = truncate (sqrt (fromInteger n))

start = intSqrt 1020304050607080900
end = intSqrt 1929394959697989990

number = head (filter isValidSquare [start,start+10..end])

main :: IO ()
main = print number
