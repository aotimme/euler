-- What is the largest prime factor of the number 600851475143 ?
-- TODO: do function to get prime factors... should be faster
--       function would do both the sieve of Eratosthenes to get the
--       next prime number *and* add prime factors to a list, while
--       "chipping away" at the original number
--       when the "chipped away" number is a prime number, we are done!
--       and, in fact, the final "chipped away" number will be the
--       largest prime factor :)

-- ordered list difference
minus (x:xs) (y:ys) = case (compare x y) of 
  LT -> x : minus  xs  (y:ys)
  EQ ->     minus  xs     ys 
  GT ->     minus (x:xs)  ys
minus  xs     _     = xs

-- sieve of Eratosthenes
primesTo m = 2 : eratos [3,5..m]  where
  eratos []     = []
  eratos (p:xs) = p : eratos (xs `minus` [p, p+2*p..m])

largestPrimeFactor :: (Integral n) => n -> n
largestPrimeFactor number = factor where
  factor = head [ y | y <- (reverse $ primesTo number), number `mod` y == 0 ]

main = putStrLn ("factor = " ++ (show $ largestPrimeFactor number)) where
  number = 600851475143
