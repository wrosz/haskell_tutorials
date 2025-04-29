{-# LANGUAGE BlockArguments #-}
module PD1 where
import Data.List(findIndex)

-- ## 1. Implementacja funkcji fold dla drzewa
-- Zdefiniuj typ drzewa binarnego:
-- ```haskell
-- data Tree a = Empty | Node a (Tree a) (Tree a)
-- ```
-- Następnie zaimplementuj funkcję:
-- ```haskell
-- foldTree :: (a -> b -> b) -> b -> Tree a -> b
-- ```
-- która działa podobnie jak `foldr` dla list, ale dla drzew. Użyj tej funkcji do implementacji funkcji:
-- - `sumTree :: Num a => Tree a -> a` - suma wszystkich wartości w drzewie
-- - `heightTree :: Tree a -> Int` - wysokość drzewa
-- - `treeToList :: Tree a -> [a]` - konwersja drzewa do listy (inorder)

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving Show

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ seed Empty = seed
foldTree f seed (Node x left right) = foldTree f (f x (foldTree f seed left)) right

sumTree :: (Num a) => Tree a -> a
sumTree = foldTree ( + ) 0

heightTree :: Tree a -> Int
heightTree tree = foldTree max 0 (g tree 0) where
    -- g tree 0 - drzewo izomorficzne z tree, ale każdy wierzchołek jest indeksowany jego poziomem
    g :: Tree a -> Int -> Tree Int
    g Empty _ = Empty
    g (Node _ left right) level = Node level (g left (level + 1)) (g right (level + 1))

treeToList :: Tree a -> [a]
treeToList = foldTree (\ x y -> y ++ [x]) []

-- przykład pomocniczy
exampletree :: Tree Integer
exampletree = Node 1 (Node 2 (Node 3 Empty Empty) Empty) (Node 4 Empty Empty)


-- ## 2. Rekurencja ogonowa w sortowaniu przez scalanie
-- Zaimplementuj funkcję `tailMergeSort :: Ord a => [a] -> [a]` wykorzystującą algorytm sortowania przez 
-- scalanie (merge sort), ale z użyciem rekurencji ogonowej. 

data Sort a = Sorted [a] | Unsorted [a]
toSort :: [a] -> Sort a
toSort [] = Sorted []
toSort [x] = Sorted [x]
toSort (x:xs) = Unsorted (x:xs)

-- jeśli argumenty są listami posortowanymi, to merge zwróci posortowaną konkatenację tych list
merge :: (Ord a) => [a] -> [a] -> [a]
merge list1 [] = list1
merge [] list2 = list2
merge list1 (x:xs) = merge (take i list1 ++ [x] ++ drop i list1) xs
    where
    i = case findIndex (>= x) list1 of
        Just j -> j
        Nothing -> length list1

tailMergeSort :: (Ord a) => [a] -> [a]
tailMergeSort list = go (Unsorted list) [] [] where
    go :: (Ord a) => Sort a -> [Sort a] -> [a] -> [a]
    go (Unsorted l) stack sorted = go (toSort x) (toSort y:stack) sorted where
        x = take (length l `div` 2) l
        y = drop (length l `div` 2) l
    go (Sorted l) (x:stack) sorted = go x stack (merge l sorted)
    go (Sorted l) [] sorted = merge l sorted


-- ## 3. Kalkulator wyrażeń
-- Zdefiniuj typ danych dla wyrażeń arytmetycznych:
-- ```haskell
-- data Expression = Number Double 
--                | Variable String 
--                | Add Expression Expression 
--                | Subtract Expression Expression 
--                | Multiply Expression Expression 
--                | Divide Expression Expression
-- ```
-- Zaimplementuj funkcje:
-- - `evaluate :: [(String, Double)] -> Expression -> Double` - oblicza wartość wyrażenia przy danych wartościach zmiennych,
-- - `differentiate :: Expression -> String -> Expression` - różniczkuje wyrażenie po nazwie zmiennej z drugiego argumentu.

data Expression = Number Double
                | Variable String
                | Add Expression Expression
                | Substract Expression Expression
                | Multiply Expression Expression
                | Divide Expression Expression
    deriving Show

evaluate :: [(String, Double)] -> Expression -> Double
evaluate _ (Number c) = c
evaluate list (Variable x) = snd $ head $ filter (\y -> fst y == x) list
evaluate list (Add expr1 expr2) = evaluate list expr1 + evaluate list expr2
evaluate list (Substract expr1 expr2) = evaluate list expr1 - evaluate list expr2
evaluate list (Multiply expr1 expr2) = evaluate list expr1 * evaluate list expr2
evaluate list (Divide expr1 expr2) = evaluate list expr1 / evaluate list expr2

differentiate :: Expression -> String -> Expression
differentiate (Number _) _ = Number 0
differentiate (Variable x) string = if x == string then Number 1 else Number 0
differentiate (Add expr1 expr2) string = Add (differentiate expr1 string) (differentiate expr2 string)
differentiate (Substract expr1 expr2) string = Substract (differentiate expr1 string) (differentiate expr2 string)
differentiate (Multiply expr1 expr2) string = Add
    (Multiply (differentiate expr1 string) expr2)
    (Multiply expr1 (differentiate expr2 string))
differentiate  (Divide expr1 expr2) string = Divide
    (Substract (Multiply (differentiate expr1 string) expr2) (Multiply expr1 (differentiate expr2 string)))
    (Multiply expr2 expr2)

expr :: Expression
expr = Multiply (Add (Number 5) (Variable "y")) (Divide (Variable "x") (Add (Number 3) (Variable "x")))
    -- (5 + y) * (x / (3 + x))
    -- d/dy expr = (x/x+3)
eval :: [(String, Double)]
eval = [("x", 1), ("y", 2)]

-- ## 4. Generator liczb Fibonacciego z rekurencją ogonkową
-- Napisz funkcję `fibTR :: Int -> Int`, która oblicza n-tą liczbę Fibonacciego używając rekurencji ogonowej. 

fibTR :: Int -> Int
fibTR n = go n 0 1 where
    go :: Int -> Int -> Int -> Int
    go 0 a _ = a
    go k a b = go (k-1) b (a + b)


-- ## 5. Implementacja [kopca binarnego](https://en.wikipedia.org/wiki/Binary_heap)
-- Zdefiniuj typ danych dla kopca binarnego (min-heap):
-- ```haskell
-- data Heap a = Empty | Node a (Heap a) (Heap a)
-- ```
-- Zaimplementuj następujące operacje:
-- - `emptyHeap :: Heap a` - tworzy pusty kopiec
-- - `insertHeap :: Ord a => a -> Heap a -> Heap a` - dodaje element do kopca
-- - `findMinHeap :: Heap a -> Maybe a` - zwraca najmniejszy element z kopca
-- - `deleteMinHeap :: Ord a => Heap a -> Heap a` - usuwa najmniejszy element z kopca
-- - `heapify :: Ord a => [a] -> Heap a` - tworzy kopiec z listy elementów
-- - `heapSort :: Ord a => [a] -> [a]` - sortuje listę używając kopca

data Heap a = EmptyH | NodeH a (Heap a) (Heap a)  -- Empty i Node były już zdefiniowane w zadaniu 1
    deriving Show

emptyHeap :: Heap a
emptyHeap = EmptyH

-- funkcja pomocnicza, zwraca rozmiar kopca
size :: Heap a -> Int
size EmptyH = 0
size (NodeH _ left' _) = 1 + size left'  -- kopiec jest zawsze uzupełniany od lewej

-- druga funkcja pomocnicza, zwraca długość ścieżki do ostatniego elementu po prawej
size' :: Heap a -> Int
size' EmptyH = 0
size' (NodeH _ _ right') = 1 + size' right'

insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x EmptyH = NodeH x EmptyH EmptyH
insertHeap x (NodeH y left right) =
    if x <= y then
        if size left > size' left  -- wolne miejsce jest po lewej
            then NodeH y (insertHeap x left) right
        else if size left > size' right  -- - wolne miejsce jest po prawej
            then NodeH y left (insertHeap x right)
        else  -- gdy ostatni poziom jest zapełniony, idziemy do pierwszego wierzchołka z lewej
                                    -- i nie sprawdzamy dalej rozmiarów (optymalizujemy liczbę operacji)
            insertLeft x (NodeH y left right)
    else insertHeap y (NodeH x left right)
    where
    insertLeft ::Ord a => a -> Heap a -> Heap a
    insertLeft x' EmptyH = NodeH x' EmptyH EmptyH
    insertLeft x' (NodeH y' left' right')
        | x' <= y' = NodeH y' (insertLeft x' left') right'
        | otherwise = NodeH x' (insertLeft y' left') right'

-- przykład pomocniczy
exampleheap :: Heap Integer
exampleheap = insertHeap 2 $ insertHeap 4 $ insertHeap 9 $ insertHeap 5 $ insertHeap 3 $ insertHeap 10 EmptyH

findMinHeap :: Ord a => Heap a -> Maybe a
findMinHeap EmptyH = Nothing
findMinHeap (NodeH x EmptyH _) = Just x
findMinHeap (NodeH _ left right)
    | size left > size right = findMinHeap left
    | otherwise = min (findMinHeap left) (findMinHeap right)

deleteMinHeap :: Ord a => Heap a -> Heap a
deleteMinHeap EmptyH = EmptyH
deleteMinHeap (NodeH _ EmptyH EmptyH) = EmptyH
deleteMinHeap (NodeH x left right)
    | size left > size right = NodeH x (deleteMinHeap left) right  -- po lewej jest dodatkowy poziom, więc tam usuwamy
    | size left == size' right =  -- cały poziom zapełniony, jeśli usuniemy najmniejszy element, to powinniśmy zamienić składowe kopca miejscami
        if findMinHeap left < findMinHeap right then NodeH x right (deleteMinHeap left)
        else NodeH x left (deleteMinHeap right)
    | otherwise =
        if findMinHeap left < findMinHeap right then NodeH x (deleteMinHeap left) right
        else NodeH x left (deleteMinHeap right)

heapify :: Ord a => [a] -> Heap a
heapify = foldr insertHeap EmptyH

heapSort :: Ord a => [a] -> [a]
heapSort list = go (heapify list) [] where
    go :: Ord a => Heap a -> [a] -> [a]
    go heap list' = case findMinHeap heap of
        Just x -> go (deleteMinHeap heap) (list'++[x])
        Nothing -> list'


-- ## 6. Rozwijanie i zwijanie danych
-- Zaimplementuj funkcję:
-- ```haskell
-- unfold :: (b -> Maybe (a, b)) -> b -> [a]
-- ```
-- gdzie `unfold` generuje listę z początkowego stanu.  
-- Następnie użyj jej do implementacji:
-- - `fibonacciSequence :: Int -> [Int]` - generuje n pierwszych liczb Fibonacciego
-- - `convertDecimalToBinary :: Int -> [Bool]` - konwertuje liczbę dziesiętną na jej postać w zapisie binarnym.

unfold :: (b -> Maybe (a, b)) -> b -> [a]
unfold state x = go state x [] where
    go :: (b -> Maybe (a, b)) -> b -> [a] -> [a]
    go state' x' list = case state' x' of
        Just (y, z) -> go state' z (list ++ [y])
        Nothing -> list

fibonacciSequence :: Int -> [Int]
fibonacciSequence n = unfold f (0, 1, 0) where
    f :: (Int, Int, Int) -> Maybe (Int, (Int, Int, Int))
    f (a, b, k) = if k <= n-1 then Just (a, (b, a + b, k + 1)) else Nothing

convertDecimalToBinary :: Int -> [Bool]
convertDecimalToBinary n = unfold f (n, floor ( logBase 2 (fromIntegral n))) where
    f :: (Int, Int) -> Maybe (Bool, (Int, Int))
    f (_, -1) = Nothing
    f (m, k)
        | m >= 2^k = Just (True, (m-(2^k), k-1))
        | otherwise = Just (False, (m, k-1))


-- ## 7. Graf i algorytmy grafowe
-- Zdefiniuj typ reprezentujący graf:
-- ```haskell
-- data Graph a = Graph [(a, [a])]
-- ```
-- gdzie każdy wierzchołek jest mapowany na listę sąsiadów. Zaimplementuj następujące funkcje:
-- - `dfs :: Eq a => a -> Graph a -> [a]` - przeszukiwanie grafu w głąb (DFS) zaczynając od danego wierzchołka
-- - `bfs :: Eq a => a -> Graph a -> [a]` - przeszukiwanie grafu wszerz (BFS) zaczynając od danego wierzchołka
-- - `hasPath :: Eq a => a -> a -> Graph a -> Bool` - sprawdza czy istnieje ścieżka pomiędzy dwoma wierzchołkami

data Graph a = Graph [(a, [a])]

-- funkcja pomocnicza, zwraca listę sąsiadów wierzchołka v w grafie G
neighbors :: Eq a => Graph a -> a -> [a]
neighbors (Graph list) x = snd $ head $ filter (\y -> fst y == x) list

examplegraph :: Graph Integer
examplegraph = Graph [(1,[2,4]), (2,[3,1]), (3,[1,2,4]), (4,[1])]

dfs :: Eq a => a -> Graph a -> [a]
dfs v (Graph list) = go (Graph list) [v] (neighbors (Graph list) v) where
    go :: Eq a => Graph a -> [a] -> [a] -> [a]
    go _ visited [] = visited
    go graph visited (x:xs)
        | x `elem` visited = go graph visited xs
        | otherwise = go graph (visited ++ [x]) (neighbors graph x ++ xs)

bfs :: Eq a => a -> Graph a -> [a]
bfs v (Graph list) =  go (Graph list) [v] (neighbors (Graph list) v) where
    go :: Eq a => Graph a -> [a] -> [a] -> [a]
    go _ visited [] = visited
    go graph visited (x:xs)
        | x `elem` visited = go graph visited xs
        | otherwise = go graph (visited ++ [x]) (xs ++ neighbors graph x)

hasPath :: Eq a => a -> a -> Graph a -> Bool
hasPath x y graph = y `elem` dfs x graph