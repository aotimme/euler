-- What is the largest prime factor of the number 600851475143 ?
-- ANS: 6857

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

--getLargestFactor :: (Integral n) => n -> [n] -> n
getLargestFactor num (x:xs)
  | num == 1         = x
  | num `mod` x == 0 = getLargestFactor (num `div` x) (x:xs)
  | num `mod` x /= 0 = getLargestFactor num xs

largestPrimeFactor :: (Integral n) => n -> n
largestPrimeFactor number = getLargestFactor number (primesTo number)

main = putStrLn ("factor = " ++ (show $ largestPrimeFactor number)) where
  number = 600851475143
