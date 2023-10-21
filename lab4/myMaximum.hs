myMaximum :: Ord a => [a] -> a

myMaximum [h] = h
myMaximum (h:t) = let big = myMaximum t in
  if h > big
    then h
    else big

