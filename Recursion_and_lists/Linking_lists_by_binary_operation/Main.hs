module Main where
-- Реализация xZipWith
xZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
xZipWith _ [] _ = []  -- Если первый список пустой, результат пустой список
xZipWith _ _ [] = []  -- Если второй список пустой, результат пустой список
xZipWith f (x:xs) (y:ys) = f x y : xZipWith f xs ys  -- Применяем функцию и рекурсивно продолжаем

main :: IO ()
main = do
    print (xZipWith (+) [10, 20, 30] [9, 8, 7])         -- Ожидается [19, 28, 37]
    print (xZipWith (+) [10, 20, 30] [9, 8, 7, 6, 5, 4]) -- Ожидается [19, 28, 37]
    print (xZipWith (+) [10, 20, 30] [])                -- Ожидается []
    print (xZipWith (*) [1, 2, 3] [4, 5])               -- Ожидается [4, 10]
