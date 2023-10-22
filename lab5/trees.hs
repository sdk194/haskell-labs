data BinTree t = Empty | Root t (BinTree t) (BinTree t) deriving (Eq, Ord, Show)

myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty)) (Root 7 Empty Empty)

leaf x = Root x Empty Empty

myTree1 = Root 5 (Root 1 (Empty) (leaf 3)) (leaf 7)

addNode :: Ord a => a -> BinTree a -> BinTree a 
addNode n Empty = Root n Empty Empty
addNode n (Root x l r) = if n < x
  then Root x (addNode n l) r 
  else Root x l (addNode n r)

makeTree :: Ord a => [a] -> BinTree a
makeTree [h] = addNode h Empty
makeTree (h:t) = addNode h (makeTree t)

inorder :: BinTree a -> [a]
inorder (Root x Empty Empty) = [x]
inorder (Root x Empty r) = [x] ++ inorder r
inorder (Root x l r) = inorder l ++ [x] ++ inorder r

mpsort :: Ord a => [a] -> [a]
mpsort [h] = [h]
mpsort x = inorder (makeTree x)

hosort :: Ord a => (a->a->Bool) -> [a] -> [a]
hosort q (h1:h2:t)
 | q h1 h2 = reverse (mpsort ([h1] ++ [h2] ++ t))
 | otherwise = mpsort ([h1] ++ [h2] ++ t)
