myAppend :: [a] -> [a] -> [a]
myAppend a b = 
    a ++ b

myHead :: [a] -> a 
myHead a = 
    a !! 0
    -- start where (start:_) = a

myLast :: [a] -> a
myLast a = 
    let l = length a - 1
    in a !! l

myTail :: [a] -> [a]
myTail a = end
    where (_:end) = a

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = [x] ++ myInit xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "list cant be empty"
myMaximum [x] = x
myMaximum (x:xs) = 
    if x > (myMaximum xs)
        then x
    else myMaximum xs

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "list cant be empty"
myMinimum [x] = x
myMinimum (x:xs) = 
    if x < (myMinimum xs)
        then x
    else myMinimum xs

myElem :: Eq a => a -> [a] -> Bool
myElem y [] = False
myElem y [x] = False
myElem y (x:xs) = 
    if y == x
        then True
    else myElem y xs

myDelete :: Eq a => a -> [a] -> [a]
myDelete y [] = []
myDelete y (x:xs) = 
    if y == x
        then xs
    else x:(myDelete y xs)

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion a [] = a
-- myUnion [] [] = []
myUnion a (x:xs) = if (elem x a) || (elem x xs)
                    then myUnion a xs
                    else myUnion (a ++ [x]) xs

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] y = []
myIntersect (x:xs) y = if (elem x y)
                        then x:(myIntersect xs y)
                        else myIntersect xs y