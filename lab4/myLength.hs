myLength :: [a] -> Int

myLength [h] = 1
myLength (h:t) = 1 + myLength t
