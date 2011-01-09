import Char

factprod 1 p = p
factprod x p = factprod (x-1) (x*p)

fact x = factprod x 1

fact_100_digit_sum = sum (map digitToInt (show (fact 100)))