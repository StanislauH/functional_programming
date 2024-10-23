module Main where
-- Бесконечный список обычных чисел Фибоначчи
fibonacci :: [Integer]
fibonacci = fibHelper 0 1
  where
    fibHelper a b = a : fibHelper b (a + b)

-- Бесконечный список обобщённых чисел Фибоначчи
generalizedFibonacci :: [Integer] -> [Integer]
generalizedFibonacci initial = initial ++ genFibHelper initial
  where
    genFibHelper xs = let next = sum (take m xs)  -- сумма первых m элементов
                      in next : genFibHelper (tail xs ++ [next])
        where m = length initial

main :: IO ()
main = do
    print (take 10 fibonacci)  -- Ожидается [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
    print (take 10 (generalizedFibonacci [7, 3, 10, 0]))  -- Ожидается [7, 3, 10, 0, 20, 33, 63, 106, 222, 424]

