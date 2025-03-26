module L02 where
import Control.Exception (asyncExceptionToException)

-- # Rekurencja ogonkowa 

-- 1. **Rekurencja ogonkowa i NWD z rozszerzonym przetwarzaniem danych wejściowych**  

--     Napisz funkcję `tailGCD :: Integral a => a -> a -> a`, która oblicza największy 
--     wspólny dzielnik (NWD) dwóch liczb całkowitych.

naiveGCD :: Integral a => a -> a -> a
naiveGCD a 0 = a
naiveGCD a b = naiveGCD b (a `mod` b)  -- to już jest ogonkowe


-- 2. **Rekurencja ogonkowa i quicksort z użyciem jawnego stosu**  

--     Zaimplementuj wersję algorytmu quicksort: `tailQuickSort :: Ord a => [a] -> [a]`, 
--     która unika głębokiej rekurencji, używając akumulatora lub jawnego stosu do 
--     zarządzania podlistami, które trzeba posortować.

quickSort :: Ord a => [a] -> [a]  -- naiwna implementacja
quickSort [] = []
quickSort (x:xs) = quickSort (filter ( < x) xs) ++ [x] ++ quickSort (filter ( > x) xs)

data Stack a = Ok a | Unsorted [a]  -- utworzymy stos z nieposortowanymi podlistami (lub pojedynczymi elementami, które już są posortowane, czyli OK)
tailQuickSort :: Ord a => [a] -> [a]
tailQuickSort list = go [Unsorted list] []  -- pierwszy argument - elementy do posortowania, prawy - już posortowana lista
    where
        go [] xs = xs  -- stos pusty -> zwracam po prostu posortowaną listę
        go ((Ok a) : stack) xs = go stack (a:xs)  -- na stosie pojedynczy element -> dodaję go do posortowanej listy i dalej działam na następnym elemencie stosu
        go ((Unsorted [a]) : stack) xs = go ((Ok a) : stack) xs
        go ((Unsorted (a:as)) : stack) xs = go (Unsorted right : (Ok a) : (Unsorted left) : stack) xs
            where
                left = filter (<= a) as
                right = filter (> a) as


-- 3. **Rekurencja ogonkowa i obliczanie zbioru potęgowego (power set)**  
--     Napisz funkcję `tailPowerSet :: [a] -> [[a]]`, która oblicza zbiór potęgowy 
--     (power set) danej listy, wykorzystując tail recursion. 
--     Upewnij się, że:  
--     - Używasz akumulatora, który stopniowo buduje zbiór potęgowy.  
--     - Unikasz tworzenia się pośrednich wyrażeń (thunks) podczas łączenia podzbiorów.  
--     - Funkcja działa efektywnie nawet dla list o umiarkowanym rozmiarze.

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = fmap (x:) (powerSet xs) ++ (powerSet xs)

tailPowerSet :: [a] -> [[a]]
tailPowerSet list = go list [[]]
    where
        go :: [a] -> [[a]] -> [[a]]
        go [] list' = list'
        go (x:xs) list' = go xs (fmap (x: ) list' ++ list')


-- 4. **Rekurencja ogonkowa i sumowanie zagnieżdżonej struktury list**  

--     Zdefiniuj typ rekurencyjny dla zagnieżdżonych list:  
--     ```haskell
--     data NestedList a = Elem a | List [NestedList a]
--     ```
--     Następnie napisz tail-recursive funkcję: 
--     `sumNested :: Num a => NestedList a -> a`, 
--     która oblicza sumę wszystkich elementów w zagnieżdżonej liście.

data NestedList a = Elem a | List [NestedList a]

-- naiwnie
sumNested :: Num a => NestedList a -> a
sumNested (Elem a) = a
sumNested (List []) = 0
sumNested (List (x:xs)) = sumNested x + sumNested (List xs)

-- ogonkowo
tailSumNested :: Num a => NestedList a -> a
tailSumNested list = go [list] 0
    where
        go [] acc = acc
        go ((Elem a):as) acc = go as (a + acc)
        go ((List a):as) acc = go (a ++ as) acc

-- example = List [Elem 2, List [List [Elem 1], List [Elem 2, Elem 3], Elem 1]]
-- tailSumNested example  ==
-- == go [List [Elem 2, List [List [Elem 1], List [Elem 2, Elem 3], Elem 1]]] 0 
-- == go [Elem 2, List [List [Elem 1], List [Elem 2, Elem 3], Elem 1]] 0
-- == go [List [List [Elem 1], List [Elem 2, Elem 3], Elem 1]] 2
-- == go [List [Elem 1], List [Elem 2, Elem 3], Elem 1]] 2
-- == go [Elem 1, List [Elem 2, Elem 3], Elem 1] 2
-- == go [List [Elem 2, Elem 3], Elem 1] 3 
-- == go [Elem 2, Elem 3, Elem 1] 3 
-- == go [Elem 3, Elem 1] 5
-- == go [Elem 1] 8
-- == go [] 9
-- == 9

-- 5. **Rekurencja ogonkowa i przeglądanie drzewa**  

--     Dla drzewa binarnego zdefiniowanego jako:  
--     ```haskell
--     data Tree a = Empty | Node a (Tree a) (Tree a)
--     ```
--     napisz funkcję: `preorder :: Tree a -> [a]`, 
--     która odwiedza węzły drzewa w następującej kolejności: 
--     najpierw bieżący węzeł, potem jego lewe poddrzewo, a na końcu prawe poddrzewo, zwracając listę wartości w tej kolejności.

data Tree a = Empty | Node a (Tree a) (Tree a)
exampletree :: Tree Integer
exampletree = Node 1 (Node 2 Empty (Node 5 (Node 6 Empty Empty) (Node 7 Empty Empty))) (Node 3 Empty Empty)
 
-- naiwnie
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node t tree1 tree2) = [t] ++ preorder tree1 ++ preorder tree2

-- ogonkowo
tailPreorder:: Tree a -> [a]
tailPreorder tree = go [tree] []
    where
        go :: [Tree a] -> [a] -> [a]
        go [] acc = acc
        go (Empty:xs) acc = go xs acc
        go ((Node t t1 t2):xs) acc = go (t1:t2:xs) (acc ++ [t])


-- # Pytania dodatkowe

-- a. **Rekurencja ogonkowa i wyszukiwanie w drzewie BST**
-- Dla drzewa BST zdefiniowanego jako `data BST a = Empty | Node a (BST a) (BST a)`
-- napisz funkcję `tailSearch :: Ord a => a -> BST a -> Bool`, która wyszukuje podany element w drzewie, 
-- wykorzystując rekurencję ogonkową z zastosowaniem jawnego stosu lub akumulatora do zarządzania stanem przeszukiwania.

-- b. **Rekurencja ogonkowa i znajdowanie najmniejszego elementu**
-- Napisz funkcję `tailMinimum :: Ord a => [a] -> a`, 
-- która zwraca najmniejszy element niepustej listy, 
-- wykorzystując rekurencję ogonkową z akumulatorem w celu eliminacji zbędnych wyrażeń opóźnionych.

-- c. **Rekurencja ogonkowa i ewaluacja wyrażeń arytmetycznych**
-- Zdefiniuj abstrakcyjny typ danych dla wyrażeń arytmetycznych:
-- `data Expr = Val Int | Add Expr Expr | Mul Expr Expr | Sub Expr Expr`
-- Następnie napisz funkcję `tailEval :: Expr -> Int`, 
-- która ocenia dane wyrażenie, stosując rekurencję ogonkową z odpowiednimi 
-- akumulatorami do przechowywania częściowych wyników. 