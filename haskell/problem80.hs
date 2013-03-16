
expandSqrt' n s = d:next
  where (nn, d) = last [ (n - dn, d) | d <- [0..9], let dn = d * (20*s + d), dn <= n ]
        next = if nn == 0
          then []
          else expandSqrt' (nn*100) (10*s+d)
expandSqrt n = expandSqrt' n 0

main =
  print $ sum [ sum expansion | n <-[1..99], let expansion = take 100 $ expandSqrt n, length expansion == 100 ]
