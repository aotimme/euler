-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

-- ordered list difference
minus (x:xs) (y:ys) = case (compare x y) of 
  LT -> x : minus  xs  (y:ys)
  EQ ->     minus  xs     ys 
  GT ->     minus (x:xs)  ys
minus  xs     _     = xs

-- sieve of Eratosthenes
primesTo m = 2 : sieve [3,5..m]  where
  sieve []     = []
  sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..m])

main = putStrLn ("Sum = " ++ (show $ sum $ primesTo 2000000))
