monthLengths = [31,28,31,30,31,30,31,31,30,31,30,31]

isLeap year = year `mod` 400 == 0 || year `mod` 100 /= 0 && year `mod` 4 == 0

nextMonthDay (year,11,day) = (year+1,0,(day + 31) `mod` 7)
nextMonthDay (year,1,day) | isLeap year = (year,2,(day + 29) `mod` 7)
                          | otherwise   = (year,2,(day + 28) `mod` 7)
nextMonthDay (year,month,day) = (year,month + 1,(day + monthLengths !! month) `mod` 7)

monthStarts = iterate nextMonthDay (1900,0,0)

isSunday (_,_,6) = True
isSunday _       = False

yearLess y (year,_,_) = year < y

sundayCount = length (filter isSunday (takeWhile (yearLess 2001) (dropWhile (yearLess 1901) monthStarts)))

main :: IO ()
main = print sundayCount
