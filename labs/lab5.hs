data BinTree t = Empty | Root Int (BinTree t)(BinTree t)
                 deriving (Eq, Ord, Show)

myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))(Root 7 Empty Empty)

leaf x = Root x Empty Empty


addNode :: Int -> BinTree t -> BinTree t
addNode a Empty = leaf a  
addNode x (Root a left right)
    | x < a  = Root a (addNode x left) right  
    | otherwise  = Root a left (addNode x right)


makeTree :: [Int] -> BinTree t
makeTree [] = Empty
makeTree [x] = leaf x
makeTree (x:xs) = addNode x (makeTree xs)


inorder :: BinTree t -> [Int]
inorder Empty = []
inorder (Root x left right) = inorder left ++ [x] ++ inorder right

preorder :: BinTree t -> [Int]
preorder Empty = []
preorder (Root x left right) = [x] ++ preorder left ++ preorder right


mpsort :: [Int] -> [Int]
mpsort x = inorder (makeTree x)


hoAddNode :: (Int -> Int -> Bool) -> Int -> BinTree t -> BinTree t
hoAddNode _ a Empty = leaf a
hoAddNode fn x (Root a left right)
     | fn x a = Root a (hoAddNode fn x left) right
     | otherwise = Root a left (hoAddNode fn x right)


hoMakeTree :: (Int -> Int -> Bool) -> [Int] -> BinTree t
hoMakeTree _ [] = Empty
hoMakeTree fn (x:xs) = hoAddNode fn x (hoMakeTree fn xs)


hosort :: (Int -> Int -> Bool) -> [Int] -> [Int]
hosort fn x = inorder (hoMakeTree fn x)


isEven :: Int -> Bool
isEven 0 = True
isEven 1 = False
isEven x = isEven (x-2)


--  divisors n = [d | d <- [1..(n ‘div‘ 2)], n ‘mod‘ d == 0]