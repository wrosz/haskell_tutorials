{-# LANGUAGE InstanceSigs #-}
module PD2 where

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (f . g)  -- Przekształci wynik, zachowując dostęp do środowiska

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure x = Reader (const x)  -- Tworzy Reader z wartością ignorującą środowisko

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    -- f :: r -> (a -> b)
    -- g :: r -> a
    Reader f <*> Reader g = Reader (\y -> f y (g y))  -- Aplikuje funkcję w kontekście Reader

instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    -- f :: r -> a
    -- g :: a -> Reader r b
    (Reader f) >>= g = Reader h where
        -- h :: r -> b
        h y = let
            y' = f y
            in runReader (g y') y  -- Sekwencjonuje operacje, przekazując środowisko


-- Pobiera całe środowisko
ask :: Reader r r
ask = Reader id

-- Stosuje funkcję do środowiska
asks :: (r -> a) -> Reader r a
asks = Reader

-- Modyfikuje środowisko dla określonego obliczenia
local :: (r -> r) -> Reader r a -> Reader r a
local f reader = do
  env <- ask
  return (runReader reader (f env))


-- Praktyczny przykład -- 

-- Konfiguracja aplikacji bankowej
data BankConfig = BankConfig
  { interestRate :: Double    -- Stopa procentowa
  , transactionFee :: Int     -- Opłata za transakcję
  , minimumBalance :: Int     -- Minimalne wymagane saldo
  } deriving (Show)

-- Struktura danych konta
data Account = Account
  { accountId :: String  -- Identyfikator konta
  , balance :: Int       -- Aktualny stan
  } deriving (Show)

exBankConfig :: BankConfig
exBankConfig = BankConfig 0.05 10 0
exAccount :: Account
exAccount = Account "a" 1000


-- Oblicza odsetki dla konta na podstawie konfiguracji
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = asks f where
  f :: BankConfig -> Int
  f bc = let
    rate = interestRate bc
    balanceDouble :: Double
    balanceDouble = fromIntegral $ balance acc
    in floor $ rate * balanceDouble

exInterest :: Int
exInterest = runReader (calculateInterest exAccount) exBankConfig


-- Pobiera opłatę za transakcję z konta
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  bc <- ask
  let fee = transactionFee bc
  return acc {balance = balance acc - fee}

exAccount2 :: Account
exAccount2 = runReader (applyTransactionFee exAccount) exBankConfig


-- Sprawdza czy konto spełnia wymóg minimalnego salda
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = Reader (\b -> balance acc >= minimumBalance b)

exCheck :: Bool
exCheck = runReader (checkMinimumBalance exAccount) exBankConfig


-- Przetwarza konto wykonując kilka operacji z konfiguracją
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  -- nakłada opłatę za transakcję, oblicza odsetki i sprawdza, czy wymóg minimalnego salda jest spełniony
  acc2 <- applyTransactionFee acc
  interest <- calculateInterest acc2
  check <- checkMinimumBalance acc2
  return (acc2, interest, check)

exProcess :: (Account, Int, Bool)
exProcess = runReader (processAccount exAccount) exBankConfig