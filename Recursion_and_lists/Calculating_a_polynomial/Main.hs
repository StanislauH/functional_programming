module Main where
-- Функция вычисления значения многочлена при заданном значении переменной
evalPolynomial :: [Integer] -> Integer -> Integer
evalPolynomial coeffs x = foldl (\acc coeff -> acc * x + coeff) 0 coeffs

main :: IO ()
main = do
    print (evalPolynomial [2, 1, 5] 3)  -- Ожидается 26
    print (evalPolynomial [1, 0, -2, 1] 2)  -- Ожидается 1*2^3 + 0*2^2 - 2*2 + 1 = 5