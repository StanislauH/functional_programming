module Main where
-- Функция для вычисления значения числа по его разрядам
fromDigits :: Int -> [Int] -> Int
fromDigits n digits = foldl (\acc x -> acc * n + x) 0 digits

-- Функция для получения разрядов числа в n-ичной системе счисления
toDigits :: Int -> Int -> [Int]
toDigits n 0 = [0]  -- Если число 0, то разряд 0
toDigits n x = reverse (toDigitsHelper x)
  where
    toDigitsHelper 0 = []
    toDigitsHelper num = let (q, r) = num `divMod` n
                         in r : toDigitsHelper q

-- Функция для поразрядного сложения двух чисел
addDigitwise :: Int -> [Int] -> [Int] -> [Int]
addDigitwise n xs ys = reverse $ go (reverse xs) (reverse ys) 0
  where
    go [] [] carry = if carry == 0 then [] else [carry]
    go [] ys carry = addCarry ys carry
    go xs [] carry = addCarry xs carry
    go (x:xs) (y:ys) carry = let sum = x + y + carry
                                 in (sum `mod` n) : go xs ys (sum `div` n)

    addCarry [] carry = if carry == 0 then [] else [carry]
    addCarry (x:xs) carry = let sum = x + carry
                             in (sum `mod` n) : addCarry xs (sum `div` n)

main :: IO ()
main = do
    print (fromDigits 2 [1, 0, 1, 1, 0, 1])   -- Ожидается 45
    print (toDigits 2 45)                      -- Ожидается [1, 0, 1, 1, 0, 1]
    print (addDigitwise 2 [1, 0, 1, 1, 0, 1] [1, 1, 1])  -- Ожидается [1, 1, 0, 1, 0, 0]

