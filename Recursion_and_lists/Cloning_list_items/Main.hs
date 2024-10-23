module Main where
-- Функция, которая копирует каждый элемент списка n раз
clone :: Int -> [a] -> [a]
clone n xs = concatMap (replicate n) xs

main :: IO ()
main = do
    print (clone 3 [1, 2, 3])  -- Ожидается [1, 1, 1, 2, 2, 2, 3, 3, 3]
    print (clone 1 [1, 2, 3])  -- Ожидается [1, 2, 3]
    print (clone 0 [1, 2, 3])  -- Ожидается []
