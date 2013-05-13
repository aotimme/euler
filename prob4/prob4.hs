-- A palindromic number reads the same both ways. The largest palindrome
-- made from the product of two 2-digit numbers is 9009 = 91 x 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome :: Int -> Bool
isPalindrome x = reverse (show x) == (show x)

main = putStrLn ("palindrome = " ++ (show palindrome)) where
  palindrome = maximum [x * y | x <- [999,998..100], y <- [999,998..100], isPalindrome (x*y)]
