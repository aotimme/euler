-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all
-- of the numbers from 1 to 20?


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

getFactorsFromList num (x:xs)
  | num == 1         = []
  | num `mod` x == 0 = x:(getFactorsFromList (num `div` x) (x:xs))
  | num `mod` x /= 0 = getFactorsFromList num xs

primeFactors :: (Integral x) => x -> [x]
primeFactors x = getFactorsFromList x (primesTo x)

-- like a set union, but keeping the max number of instances of any
-- value
mergeOrderedLists :: (Ord a) => [a] -> [a] -> [a]
mergeOrderedLists [] ys = ys
mergeOrderedLists xs [] = xs
mergeOrderedLists (x:xs) (y:ys)
  | x == y = x:(mergeOrderedLists xs ys)
  | x <  y = x:(mergeOrderedLists xs (y:ys))
  | x >  y = y:(mergeOrderedLists (x:xs) ys)

main = putStrLn ("Number = " ++ (show product)) where
  primes = foldl mergeOrderedLists [] $ map primeFactors [2..20]
  product = foldl1 (*) primes
