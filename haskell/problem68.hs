import Data.List

n = 5
numbers = [1..2*n]

--series e1 i1 = series' e1 i1 n (delete e1 numbers (delete i1 numbers))

--series' 1 e1 i1 s nums =
--series' e1 i1 s nums

series = do
  let nums = numbers
  e1 <- nums
  let nums1 = delete e1 nums
  i1 <- nums1
  let nums2 = delete i1 nums1
  i2 <- nums2
  let nums3 = delete i2 nums2

  t <- seriesCont e1 i1 (e1+i1+i2) (n-1) i2 nums3

  return ([e1,i1,i2] : t)

seriesCont e1 i1 s 1 i2 nums = do
  e2 <- filter (e1 <) nums
  True <- [s == e2 + i2 + i1]

  return [[e2,i2,i1]]

seriesCont e1 i1 s i i2 nums = do
  e2 <- filter (e1 <) nums
  let nums1 = delete e2 nums
  i3 <- nums1
  True <- [s == e2 + i2 + i3]
  let nums2 = delete i3 nums1

  t <- seriesCont e1 i1 s (i-1) i3 nums2

  return ([e2,i2,i3]:t)

numSeries :: [Integer]
numSeries = do
  s <- series
  let ss = foldl1 (++) $ map show $ concat s
  True <- [length ss == 16]

  return $ read ss

main = print $ maximum numSeries

