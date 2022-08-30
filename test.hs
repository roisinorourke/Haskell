isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [a] = True
isPalindrome xs  = 
    let y = init xs
        middle = tail y
    in 
        (head xs) == (last xs) && (isPalindrome middle)

evalPoly :: Num a => a -> [a] -> a
evalPoly a [x] = x
evalPoly a (x:xs) = x + (a * (evalPoly a xs))

shortest :: Eq a => [[a]] -> [a]
shortest [x] = x
shortest (x:xs) 
    | length x < length (shortest xs) = x
    | otherwise = shortest xs

guards :: Int -> Int
guards x 
    | x `mod` 2 == 0 = 0
    | otherwise = 1