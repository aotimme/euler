-- By considering the terms in the Fibonacci sequence whose values do
-- not exceed four million, find the sum of the even-valued terms.

main = putStrLn ("sum = " ++ (show value)) where
  value = sum (takeWhile (< 4000000) [ y | n <- [1..], let y = fibs !! n, (even y) ])
    where
      fibs = 0 : 1 : zipWith (+) fibs (tail fibs)   -- Yikes!!
