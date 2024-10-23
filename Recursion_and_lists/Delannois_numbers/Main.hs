module Main where
-- Функция для вычисления числа Деланнуа
delannoy :: Integer -> Integer -> Integer
delannoy 0 _ = 1  -- Если одна из сторон 0, то единственный путь
delannoy _ 0 = 1  -- Если одна из сторон 0, то единственный путь
delannoy m n = delannoy (m - 1) n + delannoy m (n - 1) + delannoy (m - 1) (n - 1)

main :: IO ()
main = do
    print (delannoy 2 2)  -- Ожидается 13
    print (delannoy 3 3)  -- Ожидается 63
    print (delannoy 4 4)  -- Ожидается 321


