import Data.List

fibNumbers = 0 : 1 : zipWith (+) fibNumbers (tail fibNumbers)

first_1000_dig_ind = findIndex (\x -> length (show x) >= 1000) fibNumbers