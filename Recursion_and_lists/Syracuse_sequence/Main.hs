module Main where
-- Функция вычисления длины сиракузской последовательности
collatz :: Integer -> Integer
collatz 1 = 1  -- Последовательность завершается на 1
collatz n
  | even n    = 1 + collatz (n `div` 2)  -- Чётное: делим на 2
  | otherwise = 1 + collatz (3 * n + 1)  -- Нечётное: 3n + 1
  
main :: IO ()
main = do
    print (collatz 7)  -- Должно вернуть 17
    print (collatz 1)  -- Должно вернуть 1
    print (collatz 10) -- Должно вернуть 7

