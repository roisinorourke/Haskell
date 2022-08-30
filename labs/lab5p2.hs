data BinTree t = Empty | Root t (BinTree t)(BinTree t)
                 deriving (Eq, Ord, Show)

myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))(Root 7 Empty Empty)

leaf x = Root x Empty Empty


addNode :: Ord t => t -> BinTree t -> BinTree t
addNode a Empty = leaf a  
addNode x (Root a left right)   
    | x < a  = Root a (addNode x left) right  
    | otherwise  = Root a left (addNode x right)


makeTree :: Ord t => [t] -> BinTree t
makeTree [] = Empty
makeTree [x] = leaf x
makeTree (x:xs) = addNode x (makeTree xs)


inorder :: BinTree t -> [t]
inorder Empty = []
inorder (Root x left right) = inorder left ++ [x] ++ inorder right


mpsort :: Ord a => [a] -> [a]
mpsort x = inorder (makeTree x)


hoAddNode :: Ord t => (t -> t -> Bool) -> t -> BinTree t -> BinTree t
hoAddNode _ a Empty = leaf a
hoAddNode fn x (Root a left right)
     | fn x a = Root a (hoAddNode fn x left) right
     | otherwise = Root a left (hoAddNode fn x right)


hoMakeTree :: Ord t => (t -> t -> Bool) -> [t] -> BinTree t
hoMakeTree _ [] = Empty
hoMakeTree fn (x:xs) = hoAddNode fn x (hoMakeTree fn xs)


hosort :: Ord t => (t -> t -> Bool) -> [t] -> [t]
hosort fn x = inorder (hoMakeTree fn x)


