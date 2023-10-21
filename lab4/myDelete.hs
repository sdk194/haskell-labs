myDelete :: Eq a => a -> [a] -> [a]

myDelete n [h] = []
myDelete n (h:t) = if n == h 
  then [] ++ t
  else [h] ++ myDelete n t
