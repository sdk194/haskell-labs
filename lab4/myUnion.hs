myElem :: Eq a => a -> [a] -> Bool

myElem n [] = False
myElem n (h:t) = if n == h 
  then True
  else myElem n t

myUnion :: Eq a => [a] -> [a] -> [a]

myUnion l [] = l 
myUnion l (h:t) = if myElem h l 
  then myUnion l t
  else myUnion (l ++ [h]) t
