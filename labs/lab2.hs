diff :: Int -> Int -> Int
diff x y = abs (x-y)

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = if isTriangle a b c
    then
        let s = (a + b + c) / 2
        in sqrt (s * (s - a) * (s - b) * (s - c))
    else
        error "Not a triangle!"

isSum :: Int -> Int -> Int -> Bool
isSum a b c = a == (b + c) || b == (a + c) || c == (a + b)

isTriangle :: Float -> Float -> Float -> Bool
isTriangle a b c = a < (b + c) && b < (a + c) && c < (a + b)
