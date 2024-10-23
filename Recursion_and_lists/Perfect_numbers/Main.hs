module Main where
-- Функция для поиска всех делителей числа (кроме самого числа)
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]

-- Функция для проверки, является ли число совершенным
isPerfect :: Integer -> Bool
isPerfect n = sum (divisors n) == n

main :: IO ()
main = do
    print (isPerfect 6)    -- True
    print (isPerfect 28)   -- True
    print (isPerfect 12)   -- False
    print (isPerfect 496)  -- True
    print (isPerfect 8128) -- True
    print (isPerfect 27)   -- False

