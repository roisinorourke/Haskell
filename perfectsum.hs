-- function that takes an integer and returns if it is a prime number
isPrime :: Integer -> Bool
isPrime x = 0 == length [ y | y <- [2..(x `div` 2)], x `mod` y == 0] && x >= 2
-- is a prime number if it has no factors except 1 and itself and is greater than 1

-- function that checks if an integer is prime and if it can be used to find a perfect number
usablePrime :: Integer -> Bool -- return true or false
usablePrime x = isPrime x && isPrime (2^x - 1) 

-- function that finds the largest possible number that can be used to find a perfect number
largestNum :: Integer -> Bool
largestNum x = (2^(x-1) * (2^x - 1)) < 1000000000000 -- calculation for finding a perfect number
                                                     -- has to be less than a trillion using that number

-- function that takes a boolean function as a parameter and will create a list up until a certain point
upUntil :: (Integer -> Bool) -> [Integer]
upUntil x = takeWhile x [1..] -- in this case it will create a list until the largest possible number is found for the calculation

-- function that takes 3 different functions as parameters and creates a list of perfect numbers less than a trillion
perfectNumbers :: ((Integer -> Bool) -> [Integer]) -> (Integer -> Bool) -> (Integer -> Bool) -> [Integer]
perfectNumbers a b c = [ 2^(x-1) * (2^x - 1) | x <- a b, c x ]
-- will create a list using integers up until the largest possible number where the integers are usable primes

-- function will create the list of perfect numbers and sum them together for the final solution
answer :: Integer
answer = sum (perfectNumbers upUntil largestNum usablePrime)