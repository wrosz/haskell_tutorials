{-# LANGUAGE InstanceSigs #-}
module L04 where
import qualified Data.Monoid as M
import Data.Bits (Bits(xor))

-- # Foldables 

-- 1. **Implementacja map i filter za pomocą foldów**  

--    Zaimplementuj funkcje `myMap :: (a -> b) -> [a] -> [b]` oraz `myFilter :: (a -> Bool) -> [a] -> [a]` 
--    używając zarówno `foldr` jak i `foldl`. Porównaj ich działanie i wydajność w kontekście leniwej ewaluacji. 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldl (\ z y -> z ++ [f y]) []-- foldl :: ([b] -> a -> [b]) -> [b] -> [a] -> [b]

myMap2 :: (a -> b) -> [a] -> [b] -- foldr :: (a -> [b] -> [b]) -> [b] -> [a] -> [b]
myMap2 f = foldr (\ y z -> f y : z) []

func :: Num a => a -> a
func y = y + 1
exlist :: [Integer]
exlist = [1,2,3]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldl (\ b a -> if f a then b ++ [a] else b) []

-- 2. **Niestandardowy fold dla drzew**  

--    Zdefiniuj typ danych dla drzewa różanego [rose tree](https://en.wikipedia.org/wiki/Rose_tree):
--    ```haskell
--    data Tree a = Node a [Tree a]
--    ```
--    Napisz instancje `Functor` oraz `Foldable` dla typu `Tree a` a nastepnie użyj ich do implementacji:
--    - `treeSum :: Num a => Tree a -> a` - sumuje wszystkie wartości w drzewie
--    - `treeDepth :: Tree a -> Int` - znajduje głębokość drzewa
--    - `treeToList :: Tree a -> [a]` - konwertuje drzewo do listy (w porządku pre-order)
--    ***Uwaga*** Część funkcjonalności wymienionych wyżej jest dostepna dla dowolnej instancji `Foldable` 
--    (np. `sum` czy `toList` z `Data.Foldable`). W powyższym zadaniu nie należy z nich korzystać, 
--    a zdefiniować implementacje od zera, używając jedynie z `foldl, foldr` lub `foldMap`.

data Tree a = Node a [Tree a]
    deriving Show

instance Functor Tree where
    fmap f (Node x list) = Node (f x) (map (fmap f) list)

instance Foldable Tree where  -- (a -> b -> b) -> b -> Tree a -> b
    foldr f seed (Node x list) = f x (foldr (\ a b -> foldr f b a) seed list)
    -- fMap:: (Monoid m) => (a -> m) -> Tree a -> m
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Node x list) = f x <> foldr ((<>) . foldMap f) mempty list

exampletree :: Tree Int
exampletree = Node 0 [Node 1 [Node 2 [], Node 3 [Node 5 []]], Node 4 []]

treeSum :: (Num a) => Tree a -> a
treeSum = foldr (+) 0

instance Semigroup Int where
    x <> y = x + y

instance Monoid Int where
    mempty = 0

treeSum' :: Tree Int -> Int
treeSum' = foldMap id

treeToList :: Tree a -> [a]
treeToList = foldr (:) []

treeToList' :: Tree a -> [a]
treeToList' = foldMap (: [])  -- = foldMap return, return :: a -> [a]

treeDepth :: Tree a -> Int
treeDepth tree = foldr max 0 (g tree 1) where
    -- g :: Tree a -> a -> Tree Int
    -- tworzy drzewo izomorficzne do argumentu, ale każdy wierzchołek ma wartość swojego poziomu
    g (Node _ list) level = Node level (fmap (\y -> g y (level + 1)) list)


-- 3. **Fold z kontrolą akumulacji**  

--    Zaimplementuj funkcję `foldlWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c`, która 
--    działa jak `foldl`, ale pozwala na przerwanie obliczenia w dowolnym momencie, zwracając aktualny akumulator 
--    opakowany w `Left` lub finalny wynik w `Right`. Następnie użyj tej funkcji do implementacji:
--    - `findFirstThat :: (a -> Bool) -> [a] -> Maybe a` - znajduje pierwszy element spełniający warunek
--    - `takeWhileSum :: (Num a, Ord a) => a -> [a] -> [a]` - zwraca najdłuższy prefiks listy, którego suma nie przekracza podanej wartości
--    - `findSequence :: Eq a => [a] -> [a] -> Maybe Int` - znajduje indeks pierwszego wystąpienia podlisty w liście

foldWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c
foldWithControl f seed = foldl g (Left seed) where  -- seed :: b, list :: [a]
    -- g :: Either b c -> a -> Either b c
    g (Left y) x = f y x
    g (Right y) _ = Right y

-- bez foldl
foldWithControl' :: (b -> a -> Either b c) -> b -> [a] -> Either b c
foldWithControl' f seed list = go list (Left seed) where
    go [] acc = acc
    go (x:xs) (Left acc) = go xs (f acc x)
    go _ (Right acc) = Right acc

findFirstThat :: (a -> Bool) -> [a] -> Maybe a
findFirstThat condition list = g (foldWithControl' f Nothing (fmap Just list)) where
    -- f :: Maybe a -> Maybe a -> Either (Maybe a) (Maybe a)
    f _ Nothing = Left Nothing
    f _ (Just x)
        | condition x = Right (Just x)
        | otherwise = Left (Just x)
    g :: Either (Maybe a) (Maybe a) -> Maybe a
    g (Right x) = x
    g (Left _) = Nothing



-- 4. **Odwracanie foldów**  

--    Zaimplementuj funkcję `unfoldl :: (b -> Maybe (b, a)) -> b -> [a]`, która jest odwrotnością `foldl` - 
--    generuje listę z początkowego stanu. Użyj jej do implementacji:
--    - `countdown :: Int -> [Int]` - generuje odliczanie od n do 1
--    - `fib :: Int -> [Int]` - generuje n pierwszych liczb Fibonacciego
--    - `iterate' :: (a -> a) -> a -> [a]` - własna implementacja standardowej funkcji `iterate`
--    - `decToBin :: Int -> [Int]` - konwertuje liczbę dziesiętną na binarną reprezentację (listę 0 i 1)

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f state = go (f state) [] where
    go (Just (y, x)) list = go (f y) (list ++ [x])
    go Nothing list = list

-- składnia case f x of - bardzo przydatna
unfoldl' :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl' f state = go state [] where
    go s list = case f s of
        Just (x, y) -> go x (list ++ [y])
        Nothing -> list

fib :: Int -> [Int]
fib n = unfoldl' f (0,1,2) where
    -- unfoldl :: [Int] -> Maybe ([Int], Int) -> [Int] -> [Int]
    f :: (Int, Int, Int) -> Maybe ((Int, Int, Int), Int)
    f (a, b, k) = if k <= n + 1 then Just ((b, a + b, k + 1), a) else Nothing


-- 5. **Zaawansowana transformacja danych**  

--    Napisz funkcję `foldTransform :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c`, która łączy 
--    dwie listy, stosując do nich funkcję trójargumentową i akumulator. Użyj jej do implementacji:
--    - `zipFoldl :: (c -> a -> b -> c) -> c -> [a] -> [b] -> c` - podobne do `zipWith`, ale z akumulacją
--    - `matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]` - mnożenie macierzy przy użyciu foldów

-- 6. **Uniwersalna funkcja fold**  

--    Napisz funkcję `generalFold :: (a -> Either b c) -> ([c] -> b) -> [a] -> b`, która łączy elementy listy 
--    w bardziej złożony sposób:
--    - Każdy element jest transformowany przez pierwszą funkcję
--    - Jeśli wynik jest `Left b`, ten element kończy akumulację i zwraca b
--    - Jeśli wynik jest `Right c`, c jest zbierane do listy
--    - Na końcu druga funkcja jest aplikowana do zebranej listy

--    Następnie użyj `generalFold` do implementacji:
--    - `takeUntil :: (a -> Bool) -> [a] -> [a]` - zbiera elementy aż do spełnienia predykatu
--    - `groupBySum :: (Num a, Ord a) => a -> [a] -> [[a]]` - grupuje elementy listy tak, aby suma każdej grupy
--      nie przekraczała zadanej wartości