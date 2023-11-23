data TriTree t = Nil | BiNode t (TriTree t) (TriTree t) | TriNode t t (TriTree t) (TriTree t) (TriTree t) deriving (Eq, Ord, Show)

myTree = TriNode 3 10 Nil (BiNode 6 Nil Nil) Nil

add :: Ord a => a -> TriTree a -> TriTree a
add n Nil = BiNode n Nil Nil
add n (BiNode x Nil Nil)
  |n < x = TriNode n x Nil Nil Nil
  |otherwise = TriNode x n Nil Nil Nil
  
add n (TriNode x1 x2 l m r) 
  |n < x1 = TriNode x1 x2 (add n l) m r
  |n >= x1 && n <= x2 = TriNode x1 x2 l (add n m) r 
  |otherwise = TriNode x1 x2 l m (add n r)

member :: Ord a => a -> TriTree a -> Bool
member n Nil = False
member n (BiNode n1 Nil Nil)
  |n == n1 = True
  |otherwise = False

member n (TriNode n1 n2 l m r)
  |n == n1 = True
  |n == n2 = True
  |n < n1 = member n l 
  |n > n1 && n < n2 = member n m 
  |otherwise = member n r

height :: Ord a => TriTree a -> Int 
height = \t -> 
  case t of
      Nil -> -1

      (BiNode x Nil Nil) -> 1

      (TriNode _ _ l m r) | height l >= height m && height l >= height r -> height l + 1

      (TriNode _ _ l m r) | height m >= height l && height m >= height r -> height m + 1

      (TriNode _ _ l m r) | height r >= height l && height r >= height m -> height r + 1
