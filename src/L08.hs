module L08 where

-- import Control.Monad.State
-- import GHC.Base (VecElem(Int16ElemRep))
-- import Text.Read (Lexeme(String), get)
-- import Control.Monad.Trans.Reader
-- -- -- ## Monada Stanu, Monada IO i Transformatory Monad

-- -- -- 1. **Algorytm losowy z użyciem State**
-- -- -- Zaimplementuj prosty generator liczb pseudolosowych przy użyciu monady State. Zdefiniuj funkcję 
-- -- -- `randomInt :: Int -> Int -> State Int Int`, która generuje liczbę całkowitą z podanego zakresu [a,b],
-- -- -- używając [liniowego generatora kongruencyjnego](https://en.wikipedia.org/wiki/Linear_congruential_generator). 
-- -- -- Następnie napisz funkcję `randomList :: Int -> Int -> Int -> State Int [Int]`, która generuje listę n losowych liczb
-- -- -- z zakresu [a,b]. Użyj funkcji `evalState` do uruchomienia obliczeń z określonym ziarnem (seed).

-- -- type Random = State Int

-- randomInt :: Int -> Int -> State Int Int
-- randomInt x y = do
--     s <- get
--     let s' = ( a * s + c ) `mod` m
--     put s'
--     return (x + (s `mod` (y - x)))
--     where
--         a = 22695477
--         c = 1
--         m = 2 ^ 31

-- randomList :: Int -> Int -> Int -> State Int [Int]
-- randomList _ _ 0 = return []
-- randomList x y n = do
--     number <- randomInt x y
--     rest <- randomList x y (n - 1)
--     return (number:rest)

-- exRandomList :: [Int]
-- exRandomList = evalState (randomList 20 40 10) 3


-- -- 2. **Drzewo binarne i etykietowanie z State**
-- --    Zdefiniuj typ drzewa binarnego `data Tree a = Empty | Node a (Tree a) (Tree a)`.
-- --    Następnie zaimplementuj funkcję `labelTree :: Tree a -> State Int (Tree (a, Int))`, która etykietuje każdy węzeł
-- --    drzewa unikalnym numerem, używając monady State do śledzenia licznika. Numeracja powinna być w porządku preorder.
-- --    Napisz również funkcję `countNodes :: Tree a -> State (Sum Int) (Tree a)`, która liczy węzły w drzewie,
-- --    używając monady State do akumulacji.

-- data Tree a = Empty | Node a (Tree a) (Tree a)

-- labelTree :: Tree a -> State Int (Tree (a, Int))
-- labelTree Empty = return Empty
-- labelTree (Node x left right) = do
--     counter <- get
--     put (counter + 1)
--     left' <- labelTree left
--     right' <- labelTree right
--     return (Node (x, counter) left' right')


-- -- 3. **Interaktywna kalkulacja z użyciem IO**
-- -- Napisz program `calculator :: IO ()`, który wczytuje od użytkownika dwie liczby oraz operację
-- -- (dodawanie, odejmowanie, mnożenie, dzielenie) i wypisuje wynik. Program powinien obsługiwać błędy (np. dzielenie przez zero)
-- -- i pytać użytkownika, czy chce kontynuować obliczenia. Użyj funkcji `getLine`, `readLn` oraz `putStrLn` do interakcji z użytkownikiem.

-- calculator :: IO ()
-- calculator = do
--     putStrLn "Podaj liczby:"
--     x <- readLn :: IO Double
--     y <- readLn :: IO Double
--     putStrLn "Podaj operację:"
--     op <- getLine
--     case op of
--         "+" -> print (x + y)
--         "-" -> print (x - y)
--         "*" -> print (x * y)
--         "/" -> print (x / y)
--         _ -> do
--             putStrLn "Podałeś zły symbol operacji."
--             calculator


-- -- 4. **Transformator ReaderT do konfiguracji aplikacji**
-- -- Zdefiniuj typ `Config`, który zawiera parametry aplikacji (np. `verbose :: Bool`, `maxRetries :: Int`).
-- -- Następnie zaimplementuj funkcję `processItem :: String -> ReaderT Config IO Bool`, która przetwarza element i raportuje wynik.
-- -- Funkcja powinna sprawdzać wartość `verbose` w konfiguracji i wypisywać dodatkowe informacje, gdy jest ustawiona na `True`.
-- -- Na koniec napisz funkcję `processItems :: [String] -> ReaderT Config IO [Bool]`, która przetwarza listę elementów
-- -- i zwraca listę wyników.

-- data Config = Config {
--     verbose :: Bool,
--     maxRetries :: Int
-- }

-- processItem :: String -> ReaderT Config IO Bool
-- processItem str = do
--     Config {..} <- get
--     case verbose of
--         True -> do

-- -- 5. **Obsługa błędów z ExceptT**
-- --    Napisz funkcję `readFileWithExcept :: FilePath -> ExceptT String IO String`, która próbuje odczytać zawartość pliku i obsługuje potencjalne błędy używając transformatora ExceptT. Następnie zaimplementuj funkcję `processFiles :: [FilePath] -> ExceptT String IO [String]`, która przetwarza listę plików, kontynuując nawet jeśli niektóre pliki nie mogą zostać odczytane. Dodaj funkcję pomocniczą `logError :: String -> ExceptT String IO ()`, która zapisuje błędy do pliku logów.

-- -- 6. **Łączenie transformatorów StateT i IO**
-- --    Zaimplementuj prosty symulator bankomatu używając transformatora StateT. Zdefiniuj typ `BankState` zawierający saldo konta. Napisz funkcje:
-- --    * `withdraw :: Int -> StateT BankState IO Bool` - próbuje wypłacić określoną kwotę
-- --    * `deposit :: Int -> StateT BankState IO ()` - wpłaca określoną kwotę
-- --    * `checkBalance :: StateT BankState IO Int` - sprawdza aktualne saldo
-- --    * `atmSession :: StateT BankState IO ()` - przeprowadza interaktywną sesję z użytkownikiem

-- --    Każda operacja powinna wypisywać odpowiednie komunikaty na ekranie oraz aktualizować stan konta.

-- -- 7. **Implementacja stosu transformatorów**
-- --    Zdefiniuj typ `AppM a = ReaderT Config (StateT AppState (ExceptT AppError IO)) a`, gdzie:
-- --    * `Config` zawiera parametry konfiguracyjne (np. `maxAttempts :: Int`)
-- --    * `AppState` zawiera stan aplikacji (np. `counter :: Int`, `lastOperation :: String`)
-- --    * `AppError` to typ reprezentujący możliwe błędy (np. `NetworkError String`, `ValidationError String`)

-- --    Następnie zaimplementuj funkcje pomocnicze:
-- --    * `getConfig :: AppM Config` - pobiera konfigurację
-- --    * `getState :: AppM AppState` - pobiera stan
-- --    * `modifyState :: (AppState -> AppState) -> AppM ()` - modyfikuje stan
-- --    * `throwAppError :: AppError -> AppM a` - zgłasza błąd
-- --    * `runApp :: Config -> AppState -> AppM a -> IO (Either AppError (a, AppState))` - uruchamia obliczenie

-- --    Na koniec zaimplementuj przykładową funkcję biznesową `processTransaction :: Transaction -> AppM Result`, która korzysta z powyższych funkcji pomocniczych.