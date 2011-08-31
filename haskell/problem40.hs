import Char

digits = map digitToInt (concat (map show [1..]))

digitProduct = (digits !! 0) * (digits !! 9) * (digits !! 99) * (digits !! 999) * (digits !! 9999) * (digits !! 99999) * (digits !! 999999)

main :: IO ()
main = print digitProduct
