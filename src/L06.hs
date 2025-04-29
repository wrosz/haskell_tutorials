module L06 where

import Control.Monad.State

-- # Monady i ich zastosowania

-- 1. **Podstawy Monady Maybe**  

--    Napisz funkcję `safeDivide :: Int -> Int -> Maybe Int`, która wykonuje bezpieczne dzielenie dwóch liczb całkowitych. 
--    Jeżeli dzielnik jest równy zero, funkcja powinna zwrócić `Nothing`, w przeciwnym razie `Just` z wynikiem dzielenia.
--    Następnie zaimplementuj funkcję `chainedDivision :: [Int] -> Maybe Int`, która przeprowadza sekwencję bezpiecznych 
--    dzieleń, zaczynając od pierwszej liczby i dzieląc ją przez każdą kolejną. Użyj operatora `>>=` (bind).

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

chainedDivision :: [Int] -> Maybe Int
chainedDivision [] = Nothing
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Maybe x >>= f = f x
chainedDivision [x] = Just x
chainedDivision (x:y:xs) = safeDivide x y >>= (\t -> chainedDivision (t:xs))


-- 2. **List Monada i eksploracja ścieżek**  

--    Napisz funkcję `knights :: (Int, Int) -> [(Int, Int)]`, która dla danej pozycji skoczka na szachownicy 
--    zwraca listę wszystkich możliwych ruchów skoczka. Następnie zaimplementuj funkcję `knightPaths :: Int -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]`, 
--    która znajduje wszystkie możliwe ścieżki o długości `n` ruchów z jednej pozycji do drugiej. Wykorzystaj monadę list oraz 
--    operator `>>=` do eksploracji wszystkich możliwych ścieżek.

knights :: (Int, Int) -> [(Int, Int)]
knights (x, y)
    | onboard (x, y) = filter onboard [(x + a, y + b) | (a, b) <- jumps]
    | otherwise = []
    where
        jumps = [(1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1)]
        onboard (s, t) = 0 < s && 0 < t && s <= 8 && t <= 8

knightPaths :: Int -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
-- List a >= (a -> List b) -> List b
-- (x:xs) >= f = (f x) ++ (xs >= f)
-- [(Int, Int)] >>= ((Int, Int) -> [[(Int, Int)]])
knightPaths 0 (x0, y0) (x1, y1) = [[(x0, y0)] | (x1, y1) == (x0, y0)]
knightPaths n (x0, y0) (x1, y1) = fmap (\path -> (x0,y0):path) (knights (x0, y0) >>= f) where
    f :: (Int, Int) -> [[(Int, Int)]]
    f (a, b) = knightPaths (n-1) (a, b) (x1, y1)


-- 3. **Monada State do śledzenia stanu**  

--    Zdefiniuj funkcję `runningSum :: [Int] -> [Int]`, która dla listy liczb całkowitych zwraca listę sum częściowych.
--    Przykładowo, dla listy `[1, 2, 3, 4]` wynikiem powinno być `[1, 3, 6, 10]`. Zaimplementuj tę funkcję 
--    używając monady State, korzystając z operacji `get`, `put` i funkcji `runState` lub `evalState`.


runningSum :: [Int] -> [Int]
runningSum [] = []
runningSum list = evalState (go list) 0 where
    go :: [Int] -> State Int [Int]
    -- [Int] -> (Int -> (Int, [Int]))
    go [] = pure []
    go (x:xs) = do
        lastSum <- get  -- pobierz ostatni zsumowany element
        let newSum = lastSum + x  -- dodaj do niego x
        put newSum  -- nową sumę zapisz w stanie
        rest <- go xs  -- go xs :: State Int [Int], rest to wynik ewaluacji go xs na stanie newSum
        return (newSum:rest)


-- 4. **Implementacja własnej monady**  

--    Zaimplementuj własną monadę `Logger a`, która będzie przechowywać wartość typu `a` wraz z logiem 
--    operacji (listą napisów). Zdefiniuj instancje `Functor`, `Applicative` i `Monad` dla tego typu.
--    Napisz funkcje pomocnicze:
--    - `logMessage :: String -> Logger ()`
--    - `runLogger :: Logger a -> (a, [String])`
--    Następnie użyj tej monady do zaimplementowania funkcji `factorial :: Int -> Logger Int`, 
--    która oblicza silnię i loguje każdy krok obliczeń.




-- 5. **Fish operator (>=>) w praktyce**  

--    Zaimplementuj funkcje `safeTail :: [a] -> Maybe [a]` i `safeHead :: [a] -> Maybe a`, 
--    które bezpiecznie zwracają ogon i głowę listy, obsługując przypadek pustej listy. 
--    Następnie użyj operatora Kleisli composition (`>=>`) do zdefiniowania funkcji `safeSecond :: [a] -> Maybe a`, 
--    która bezpiecznie zwraca drugi element listy.

-- 6. **Monada Writer do akumulacji wyników**  

--    Zaimplementuj funkcję `countNodes :: Tree a -> Writer (Sum Int) Int`, która liczy węzły w drzewie binarnym, 
--    używając monady Writer do akumulacji sumy. Typ drzewa zdefiniuj jako 
--    `data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)` oraz do rozwiazania uzyj funkcji `tell`. 

