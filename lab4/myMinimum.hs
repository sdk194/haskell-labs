myMinimum :: Ord a => [a] -> a

myMinimum [h] = h
myMinimum (h:t) = let small = myMinimum t in
  if h < small
    then h
    else small

