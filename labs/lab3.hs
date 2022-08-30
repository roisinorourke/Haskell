isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == reverse a

isP :: Eq a => [a] -> Bool
isP [x] = True
-- isP (x:xs) = 
--     x == y
--     where 

isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' xs  = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

encode xs = [(length x, head x) | x <- group xs]

-- shortest :: [[a]] -> [a]
-- shortest a = 
--     let b = map length a
--         c = minimum b
--     in a !! d
--     where length (a !! d) == c

shortest :: [[a]] -> [a]
shortest [x] = x
shortest (x:xs) = if length x < length (shortest xs)
                  then x
                  else shortest xs

type Poly = [Int]
sumPoly :: Poly -> Poly -> Poly
sumPoly [] p = p 
sumPoly p [] = p 
sumPoly (x:xs) (y:ys) = (x+y):(sumPoly xs ys)

evalPoly :: Int -> [Int] -> Int
evalPoly a [x] = x
evalPoly p (x:xs) = x + (p * (evalPoly p xs))


-- notNull :: [[a]] -> [[a]]
-- notNull x = filter (not(null x]))