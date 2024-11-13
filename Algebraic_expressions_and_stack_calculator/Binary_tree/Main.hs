data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

treeTraverseD :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseD _ acc Empty = acc
treeTraverseD f acc (Node x left right) = 
    let accLeft = treeTraverseD f acc left
        accNode = f x accLeft
    in treeTraverseD f accNode right

treeTraverseW :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseW _ acc Empty = acc
treeTraverseW f acc root = go [root] acc
  where
    go [] acc = acc
    go (Empty:xs) acc = go xs acc
    go (Node x left right : xs) acc = 
        let acc' = f x acc
        in go (xs ++ [left, right]) acc'

exampleTree :: Tree Int
exampleTree = Node 1 
                (Node 2 
                    (Node 4 Empty Empty) 
                    (Node 5 Empty Empty))
                (Node 3 
                    (Node 6 Empty Empty) 
                    (Node 7 Empty Empty))

main :: IO ()
main = do
    putStrLn "Размер дерева:"
    print $ treeSize exampleTree -- Ожидаем 7

    putStrLn "Сумма всех узлов (обход в глубину):"
    print $ treeTraverseD (+) 0 exampleTree -- Ожидаем 28

    putStrLn "Сумма всех узлов (обход в ширину):"
    print $ treeTraverseW (+) 0 exampleTree -- Ожидаем 28

    putStrLn "Дерево с удвоенными значениями узлов:"
    print $ treeMap (*2) exampleTree -- Ожидаем дерево с элементами 2, 4, 6, 8, 10, 12, 14