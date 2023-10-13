

reverseListAcc :: [a] -> [a] -> [a]
reverselListAcc [] x = x
reverseListAcc (x:xs) y = reverseListAcc xs (x:y)

reverseList :: [a] -> [a]
reverseList x = reverseListAcc x []