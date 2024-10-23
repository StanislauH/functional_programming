module Main where

-- Функция для генерации всех маршрутов
delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths a b = pathsHelper a b []
  where
    pathsHelper 0 0 path = [reverse path]  -- Достигли правого верхнего угла
    pathsHelper a b path = 
      let rightPaths = if a > 0 then pathsHelper (a - 1) b (0:path) else []
          upPaths = if b > 0 then pathsHelper a (b - 1) (2:path) else []
          diagPaths = if a > 0 && b > 0 then pathsHelper (a - 1) (b - 1) (1:path) else []
      in rightPaths ++ upPaths ++ diagPaths  -- Собираем все возможные пути

main :: IO ()
main = do
    let paths = delannoyPaths 2 2
    mapM_ print paths  -- Печатаем все маршруты
