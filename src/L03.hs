{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module L03 where
import Control.Exception (ArrayException(UndefinedElement))
import Data.Bifoldable (bifoldl1)
import GHC.Base (VecElem(Int16ElemRep))

-- 5. **Znane typeclassy dla drzew** 
--     Zdefiniuj instancje `Show, Eq, Foldable, Functor` dla parametrycznego typu danych 
--     `data T a = EmptyT | LeafT a | InnerT (T a) (T a)`.

data T a = EmptyT | LeafT a | InnerT (T a) (T a)
example :: T Int
example = InnerT (InnerT (LeafT 0) (LeafT 1)) (InnerT (LeafT 2) (LeafT 3))

instance (Show a) => Show (T a) where
    show :: T a -> String
    show tree = "{" ++ go tree ++ "}" where
        go :: (Show a) => T a -> String
        go EmptyT = ""
        go (LeafT x) = show x
        go (InnerT tree1 tree2) = go tree1 ++ ", " ++ go tree2

instance (Eq a) => Eq (T a) where
    (==) :: Eq a => T a -> T a -> Bool
    EmptyT == EmptyT = True
    EmptyT == _ = False
    _ == EmptyT = False
    LeafT x == LeafT y = x == y
    LeafT x == _ = False
    _ == LeafT y = False
    InnerT t1 t2 == InnerT s1 s2 = (t1 == s1) && (t2 == s2)

-- Semigroup jest i Monoid oszukane, bo łączność nie będzie spełniona
instance Semigroup (T a) where
    (<>) :: T a -> T a -> T a
    t1 <> t2 = InnerT t1 t2

instance Functor T where
    -- fmap :: (a -> b) -> T a -> T b
    fmap f EmptyT = EmptyT
    fmap f (LeafT x) = LeafT (f x)
    fmap f (InnerT t1 t2) = InnerT (fmap f t1) (fmap f t2)

instance Foldable T where
    foldMap :: (Monoid m) => (a -> m) -> T a -> m
    foldMap _ EmptyT = mempty
    foldMap f (LeafT a) = f a
    foldMap f (InnerT t1 t2) = foldMap f t1 `mappend` foldMap f t2


-- # ADT i typeclassy

-- 1. **Słownik oparty na drzewie binarnym z balansowaniem**  

--     Zdefiniuj algebryczny typ danych reprezentujący drzewo wyszukiwań binarnych (BST), 
--     które będzie służyło jako słownik mapujący klucze na wartości. Następnie zaimplementuj 
--     następujące operacje:  
--     - *Wstawianie*: Dodanie pary klucz-wartość.  
--     - *Wyszukiwanie*: Pobranie wartości przypisanej do klucza.  
--     - *Usuwanie*: Usunięcie klucza (oraz odpowiadającej mu wartości) z drzewa.  
--     - *Aktualizacja*: Modyfikacja wartości przypisanej do klucza.  
--     - *Balansowanie*: Zaimplementuj procedurę balansowania (np. wykorzystując algorytm drzewa 
--         AVL lub czerwono-czarnego), aby drzewo pozostało zbalansowane po operacjach wstawiania i usuwania.

data BST k v = EmptyBST | BST {
    key :: k,
    value :: v,
    left :: BST k v,
    right :: BST k v
}
    deriving Show

-- wstawianie
insert :: (Ord k) => BST k v -> (k, v) -> BST k v
insert EmptyBST (key, val) = BST key val EmptyBST EmptyBST
insert (BST k1 v1 left1 right1) (key, val)
    | key < k1 = BST k1 v1 (insert left1 (key, val)) right1
    | otherwise = BST k1 v1 left1 (insert right1 (key, val))

-- tworzenie drzewa z listy
fromList :: (Ord k) => [(k, v)] -> BST k v
fromList = foldl insert EmptyBST

example2 :: BST Integer Integer
example2 = fromList [(1,4), (8,9), (2, 4), (3, 10), (4,3)]

-- wyszukiwanie
search :: (Ord k) => BST k v -> k -> Maybe v
search EmptyBST _ = Nothing
search (BST k1 v1 left1 right1) key
    | key < k1 = search left1 key
    | key == k1 = Just v1
    | otherwise = search right1 key


-- 2. **Interpreter wyrażeń z różniczkowaniem i upraszczaniem**  

--     Zdefiniuj algebraiczny typ danych reprezentujący wyrażenia arytmetyczne 
--     (uwzględniający zmienne, stałe, dodawanie, mnożenie). Napisz funkcje, które:  
--     - *Ewaluacja*: Obliczają wartość numeryczną wyrażenia, korzystając z mapowania zmiennych na liczby.  
--     - *Różniczkowanie*: Symbolicznie różniczkują wyrażenie względem danej zmiennej.  
--     - *Upraszczanie*: Redukują wyrażenie do prostszej formy poprzez stosowanie uproszczeń algebraicznych 
--     (np. eliminowanie składników zerowych, łączenie wyrazów podobnych).  

data Expr a = Var a | Val Int | Sum (Expr a) (Expr a) | Mul (Expr a) (Expr a)  -- wyrażenia arytmetyczne, których zmienne są typu a
    deriving Show

example3 :: Expr Char
example3 = Sum (Mul (Var 'x') (Val 7)) (Var 'y')

context :: Char -> Int
context 'x' = 10
context 'y' = 1

exprEval :: Expr a -> (a -> Int) -> Int
exprEval (Var x) f = f x
exprEval (Val n) _ = n
exprEval (Sum e1 e2) f = exprEval e1 f + exprEval e2 f
exprEval (Mul e1 e2) f = exprEval e1 f * exprEval e2 f

derivate :: (Eq a) => Expr a -> a -> Expr a
derivate (Var y) x
    | x == y = Val 1
    | otherwise = Val 0
derivate (Val _) _ = Val 0
derivate (Sum e1 e2) x = Sum (derivate e1 x) (derivate e2 x)
derivate (Mul e1 e2) x = Sum (Mul (derivate e1 x) e2) (Mul e1 (derivate e2 x))

simplify :: Expr a -> Expr a
simplify (Var x) = Var x
simplify (Val n) = Val n
simplify (Sum e1 (Val 0)) = simplify e1
simplify (Sum (Val 0) e2) = simplify e2
simplify (Sum (Val n) (Val m)) = Val (n + m)
simplify (Sum e1 e2) = simplify (Sum (simplify e1) (simplify e2))
simplify (Mul _ (Val 0)) = Val 0
simplify (Mul (Val 0) _) = Val 0
simplify (Mul e1 (Val 1)) = simplify e1
simplify (Mul (Val 1) e2) = simplify e2
simplify (Mul e1 e2) = simplify ( Mul (simplify e1) (simplify e2))
-- ...

-- 3. **Własna leniwa lista z obsługą nieskończoności**  

--     Stwórz własny typ listy (np. `data MyList a = Nil | Cons a (MyList a)`), który wspiera leniwą ewaluację. 
--     Zaimplementuj następujące funkcje:  
--     - `myMap`: Funkcję analogiczną do `map`.  
--     - `myFoldr`: Funkcję złożenia prawego (`foldr`), która potrafi działać na nieskończonych listach, jeśli to możliwe.  
--     - `myFilter`: Funkcję analogiczną do `filter`.  
--     Następnie zdefiniuj instancje `Semigroup, Monoid, Functor, Foldable` dla `Mylist`.

-- 4. **Reprezentacja grafu i algorytmy**  

--     Zdefiniuj algebryczny typ danych reprezentujący graf nieskierowany, w którym wierzchołki mogą 
--     przechowywać dowolne dane. Napisz funkcje, które:  
--     - *Przeszukiwanie w głąb (DFS)*: Przemierzają graf, zaczynając od danego wierzchołka.  
--     - *Wykrywanie cykli*: Sprawdzają, czy graf zawiera cykle.  
--     - *Znajdowanie ścieżki*: Znajdują ścieżkę między dwoma wierzchołkami (jeśli taka istnieje).  