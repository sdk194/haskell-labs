reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome (x:xs) = ((reverseList (x:xs)) == (x:xs))

--im an idiot theres acc a reverse function in haskell...
isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 (x:xs) = reverse (x:xs) == (x:xs)