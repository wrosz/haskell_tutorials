{-# LANGUAGE BangPatterns #-}
module L01 where

-- # List comprehensions

-- 1. **Trójki Pitagorejskie**  
--    Napisz funkcję `pythagoreanTriples :: Int -> [(Int, Int, Int)]`, która zwraca wszystkie trójki `(a, b, c)` spełniające warunki:  
--    - `1 ≤ a < b < c ≤ n`  
--    - `a² + b² == c²`  
--    Wykorzystaj list comprehensions do wygenerowania wyniku.

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = [(a, b, c) |
                          a <- [1 .. n],
                          b <- [1 .. n],
                          a < b,
                          c <- [1 .. n],
                          b < c,
                          a ^ 2 + b ^ 2 == c ^ 2]



-- 2. **Pary liczb, których suma jest liczbą pierwszą**  
--    Napisz funkcję `primeSumPairs :: [Int] -> [(Int, Int)]`, która przyjmuje listę liczb 
--    całkowitych i zwraca wszystkie unikalne pary `(x, y)` (przy założeniu, że `x < y`), 
--    dla których suma `x + y` jest liczbą pierwszą.

isPrime :: Int -> Bool
isPrime n = length [m | m <- [1..n], mod n m == 0] == 2
primeSumPairs :: [Int] -> [(Int, Int)]
primeSumPairs list = [(x, y) | x <- list, y <- list, x < y, isPrime (x + y)]


-- 3. **Wyodrębnianie podłańcuchów**  
--    Napisz funkcję `substrings :: String -> [String]`, która zwraca listę wszystkich niepustych 
--    podłańcuchów danego ciągu znaków. Na przykład dla ciągu `"abc"` wynik powinien 
--    zawierać `"a"`, `"ab"`, `"abc"`, `"b"`, `"bc"` oraz `"c"`. Wykorzystaj 
--    list comprehensions do wygenerowania wszystkich podłańcuchów.

substrings :: String -> [String]
substrings string = [take l $ drop s string | s <- [0..length string - 1], l <- [1..length string - s]]

-- 4. **Pary dzielników**  
--    Napisz funkcję `divisorPairs :: [Int] -> [(Int, Int)]`, która 
--    przyjmuje listę liczb całkowitych i zwraca wszystkie różne pary `(x, y)` 
--    (przy założeniu, że `x ≠ y`), dla których `x` dzieli `y` bez reszty (tj. `y mod x == 0`). 

-- 5. **Kombinacje**  
--     Napisz funkcję  
--     ```haskell
--     combinations :: Int -> [a] -> [[a]]
--     ```
--     która generuje wszystkie kombinacje k-elementowe z danej listy. 
--     Na przykład, dla `k = 2` i listy `[1,2,3]` wynikiem powinno być `[[1,2], [1,3], [2,3]]`.
combinations :: Int -> [a] -> [[a]]
combinations 0 [] = [[]]
combinations _ [] = []
combinations k (x:xs) = combinations k xs ++ [x:y | y <- combinations (k-1) xs]


-- # Leniwa/gorliwa ewaluacja, `seq` i bang patterns

-- 6. **Ścisła suma z użyciem `seq`**  
--    Napisz funkcję `strictSum :: [Int] -> Int`, która oblicza sumę listy liczb całkowitych, 
--    używając `seq` do wymuszenia ewaluacji akumulatora na każdym kroku. 
--    Porównaj jej działanie z naiwną, leniwą implementacją sumowania.

sumLazyTail :: [Int] -> Int
sumLazyTail list = go list 0
    where
        go :: [Int] -> Int -> Int
        go [] s = s
        go (x:xs) s = go xs (x + s)  -- żeby było gorliwie, wystarczy przy definicji dać !s zamiast samego s (bang patterns)
        -- rekurencja ogonkowa - mamy tylko jedno wywołanie rekurencyjne, na samym początku

-- tutaj z seq
strictSum :: [Int] -> Int
strictSum list = go list 0
    where
        go :: [Int] -> Int -> Int
        go [] s = s
        go (x:xs) s = let s' = s `seq` x + s in go xs s'  -- ewaluujemy s, definiujemy s' jako x + s (z wyliczonym s) i wsadzamy to s' do go x s'

-- 7. **Rekurencyjna funkcja silnia z użyciem bang patterns**  
--    Napisz rekurencyjną funkcję `factorial :: Int -> Int`, która oblicza silnię danej liczby. 
--    Użyj bang patterns w akumulatorze.

factorial :: Int -> Int
factorial n = go n 1
    where
        go :: Int -> Int -> Int
        go 0 s = s
        go n !s = go (n-1) n*s

-- 8. **Wymuszanie ewaluacji elementów krotki**  
--    Napisz funkcję `forceTuple :: (Int, Int) -> Int`, która przyjmuje krotkę dwóch liczb całkowitych, 
--    wymusza ewaluację obu elementów za pomocą `seq`, a następnie zwraca ich sumę. 
--    Wyjaśnij, dlaczego wymuszanie ewaluacji może być konieczne w niektórych sytuacjach.

forceTuple :: (Int, Int) -> Int
forceTuple (a, b) = let x = b `seq` a; y = x `seq` b in x + y


-- 9. **Liczby Fibonacciego z `seq` vs. bang patterns**  
--    Zaimplementuj dwie wersje generatora liczb Fibonacciego:  
--    - Pierwsza wersja wykorzystuje `seq` do wymuszenia ewaluacji w funkcji pomocniczej.  
--    - Druga wersja używa bang patterns w argumentach funkcji pomocniczej.


fibNaive :: Int -> Int
fibNaive 1 = 0
fibNaive 2 = 1
fibNaive n = fibNaive (n-1) + fibNaive (n-2)

fibSeq :: Int -> Int
fibSeq n = go n 0 1
    where
        go :: Int -> Int -> Int -> Int
        go 0 a b = a
        go n a b = a `seq` go (n-1) b (a+b)
-- fibSeq 0 = go 0 0 1 = 0
-- fibSeq 1 = go 1 0 1 = 0 `seq go 0 1 (0 + 1) = go 0 1 (0+1) = 1
-- fibSeq 2 = go 2 0 1 = 0 `seq` go 1 1 (0 + 1) = 1 `seq` go 0 (0+1) (1 + (0+1)) = go 0+1 = 0+1 = 1
-- fibSeq 3 = go 3 0 1 = 0 `seq` go 2 1 (0+1) = go 2 1 (0+1) = 1 `seq` go 1 (0+1) (1+(0+1)) = go 1 (0+1) (1+(0+1)) = (0+1)==1 `seq` go 0 (1+(0+1)) ((0+1)==1 +(1+(0+1))) = go 0 (1+(0+1)) (1+(1+(0+1))) = go (1+(0+1)) = ... = 2


fibBang :: Int -> Int
fibBang n = go n 0 1
    where
        go :: Int -> Int -> Int -> Int
        go 0 a b = a
        go n !a !b = go (n-1) b (a+b)
-- fibBang 1 = go 1 0 1 = go 0 1 (1+1) = 1
-- fibBang 2 = go 2 0 1 = go 1 1 (0+1)==1 = go 0 1 (1+1) = 1
-- fibBang 3 = go 3 0 1 = go 2 1 (0+1)==1 = go 1 1 (1+1)==2 = go 0 2 (1+2) = 2

-- fibBang 5 = go 5 0 1 = go 4 1 1 = go 3 1 2 = go 2 2 3 = go 1 3 5 = go 0 5 8 = 5




-- 10. **Unikanie wycieków pamięci w funkcji rekurencyjnej**  
--     Napisz funkcję `strictRecursive :: Int -> Int`, która oblicza wynik przy użyciu rekurencji, 
--     gdzie leniwa ewaluacja mogłaby prowadzić do wycieków pamięci. 
--     Zrefaktoryzuj funkcję, używając `seq` lub bang patterns, aby wymusić ścisłą ewaluację. 

