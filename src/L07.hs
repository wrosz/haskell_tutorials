{-# LANGUAGE InstanceSigs #-}
module L07 where

-- DEFINICJA MONADY STATE --

newtype State s a = State { runState :: s -> (s, a) }

exampleState :: State Int String
exampleState = State (\n -> (n + 1, "To jest liczba " ++ show n))

-- funktorowość:
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f state = State $ (\(s, x) -> (s, f x)) . runState state

exampleF :: String -> Bool
exampleF string = even (length string)

exampleState2 :: State Int Bool
exampleState2 = fmap exampleF exampleState
-- exampleState2 = State {runState :: s -> (s + 1, "To jest liczba s") -> (s + 1, even "To jest liczba s")}

-- aplikatywność:
instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State (\s -> (s, x))

    liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
    liftA2 f (State f1) (State f2) = State g where
        -- f1 :: s -> (s, a)
        -- f2 :: s -> (s, b)
        -- f :: a -> b -> c
        g state = let
            (s1, x1) = f1 state
            (s2, x2) = f2 s1
            y = f x1 x2
            in (s2, y)

-- monadyczność:
instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    -- f :: a -> State (s -> (s, b))
    -- a -> s -> (s, b)
    -- (a, s) -> (s, b)
    -- (s, a) -> (s, b)
    state >>= f = State g where
        -- f' :: (s, a) -> (s, b)
        f' (st, x) = runState (f x) st
        g s0 = f' (runState state s0)
  -- inaczej:
  -- state >>= f = State $ (\(st, x) -> runState (f x) st) . runState state
  -- s -> (s', x) -> runState (f x) s' = (s'', y)
        

-- implementacja funkcjonalności monady State -- 
get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (const (x, ()))  -- (\_ -> (x, ()))

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put (f s)

-- przykład   
increment :: State Integer Integer
increment = do
  n <- get
  put (n + 1)
  return n


-- właściwe zadania
