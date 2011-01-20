isPalindrom x = s == reverse s where s = show x

reverseNum x = (read . reverse . show) x

isLychrelApp it x | it > 50          = True
                  | isPalindrom next = False
                  | otherwise = isLychrelApp (it+1) next
    where next = x + reverseNum x

isLychrel x = isLychrelApp 1 x

lychrelNumbers = filter isLychrel [1..9999]

count = length lychrelNumbers

main :: IO ()
main = print count