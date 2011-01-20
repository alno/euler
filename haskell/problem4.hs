isPalindrome :: Show a => a -> Bool
isPalindrome x = s == reverse s where s = show x

minNum = 100
maxNum = 999

palindromes = [ x * y | x <- [minNum..maxNum], y <- [x..maxNum], isPalindrome (x*y) ]

max_palindrome = foldl1 max palindromes

-- Main
main :: IO ()
main = print max_palindrome
