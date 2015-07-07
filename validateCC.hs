lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x <= 0 = []
    | otherwise = lastDigit x : toRevDigits (dropLastDigit x)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:zs)) = x : ((2 * y) : (doubleEveryOther zs))


dignify :: [Integer] -> [Integer]
dignify [] = []
dignify (x:xs) = (toRevDigits x) ++ (dignify xs)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (dignify xs)


luhn :: Integer -> Bool
luhn x = (sumDigits  doubledDigits) `mod` 10 == 0
    where 
        doubledDigits = doubleEveryOther (toRevDigits x)
