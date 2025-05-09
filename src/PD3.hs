-- Zmiany względem oryginalnego kodu:
-- 1. getPlayerChoice jest typu IO Int, zamiast typu IO String
-- 2. Lokacja typu Decision jest wyposażona w dodatkowy argument będący listą opisów dostępnych dla gracza opcji,
--    printowaną przez funkcję makeDecision

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PD3 where
import Control.Monad.State
import System.IO (hFlush, stdout)  -- w celu uniknięcia przepełnienia bufora i printowania nie po kolei



-- Funkcja pomocnicza do przekonwertowania wartości wprowadzonej z klawiatury na Int
-- (do funkcji getPlayerChoice i getDiceRoll)
stringToInt :: IO Int
stringToInt = do
  input <- getLine
  case reads input :: [(Int, String)] of
    [(n, "")] -> return n
    _ -> do
      putStr "Nie podałeś liczby. Spróbuj jeszcze raz: "
      hFlush stdout
      stringToInt


-- DEFINICJE --

-- Typ reprezentujący lokację na planszy
data LocationType =
    Empty             -- Zwykłe pole
  | Decision [String] -- Punkt decyzji (+ lista opisów opcji dla gracza)
  | Obstacle Int      -- Przeszkoda (z wartością opóźnienia)
  | Treasure Int      -- Skarb (z wartością punktów)
  | Trap Int          -- Pułapka (z wartością ujemną punktów)
  | Goal              -- Cel gry - główny skarb
  deriving (Show, Eq)

-- Typ reprezentujący lokację
data Location = Location {
  locationType :: LocationType,
  description :: String,
  connections :: [Int]  -- Indeksy połączonych lokacji
} deriving (Show)

-- Typ reprezentujący stan gry
data GameState = GameState {
  playerPosition :: Int,     -- Aktualna pozycja gracza
  playerEnergy :: Int,       -- Pozostała energia gracza
  playerScore :: Int,        -- Wynik gracza
  gameMap :: [Location],     -- Mapa gry
  visitedLocations :: [Int], -- Odwiedzone lokacje
  turns :: Int               -- Liczba wykonanych tur
} deriving (Show)

-- Definicja typu dla gry przygodowej
type AdventureGame a = StateT GameState IO a


-- FUNKCJE TYPU AdventureGame a --

-- Funkcja przesuwająca gracza na podstawie wyniku rzutu
-- Zwraca liczbę pól, o które przesunął się gracz
-- Zasady dotyczące poruszania się:
-- Gracz jest przesuwany na miejsce zdefiniowane przez (wynik rzutu kośćmi) modulo (długość listy connections),
-- jeśli jest w lokacji innej niż typu Decision. Ta wartość jest zwracana przez funkcję movePlayerPosition.
-- W lokacji typu Decision gracz jest przesuwany na miejsce zależne od własnej decyzji.
movePlayer :: Int -> AdventureGame Int
movePlayer n = do
  GameState {..} <- get
  let loc = gameMap !! playerPosition
  let cons = connections loc
  let numberOfMoves = n `mod` length cons
  let newPos = cons !! numberOfMoves
  put GameState {playerPosition = newPos, visitedLocations = visitedLocations ++ [newPos], ..}
  lift $ putStrLn $ "Przesunąłeś się o " ++ show (numberOfMoves + 1) ++ case numberOfMoves of
    0 -> " pole."
    1 -> " pola."
    2 -> " pola."
    3 -> " pola."
    _ -> " pól."
  return numberOfMoves

-- Funkcja obsługująca punkt decyzji.
-- Prezentuje graczowi opcje i zwraca jego wybór (jako Int)

-- Oraz przesuwa gracza na pole na podstawie jego wyboru
makeDecision :: [String] -> AdventureGame Int
makeDecision options = do
    lift $ putStrLn "Wybierz drogę:"
    printDecision options
    lift $ putStr "Twój wybór: "
    lift $ hFlush stdout
    choice <- lift $ getPlayerChoice options
    movePlayer choice
    where
      printDecision :: [String] -> AdventureGame ()
      printDecision opts = lift $ mapM_ putStrLn opts

-- Funkcja obsługująca aktualną lokację gracza oraz przesuwająca go na następne pole
-- (w zależności od typu lokacji, będzie to zależne od wyniku rzutu kośćmi lub wyboru użytkownika).
-- Zwraca True jeśli gracz dotarł do celu
handleLocation :: AdventureGame Bool
handleLocation = do
    GameState {..} <- get
    let loc = gameMap !! playerPosition
    lift $ putStrLn $ description loc  -- wyświetl opis lokacji
    case locationType loc of
      Goal -> return True  -- zwróć True, gdy jesteś u celu
      -- Jeśli nie jesteś u celu, to przesuń gracza na inne, zmień stan gry i zwróć False (w zależności od typu lokacji)
      Empty -> do
        dice <- lift getDiceRoll
        _ <- movePlayer dice
        return False
      Decision options -> do
        _ <- makeDecision options
        return False
      Obstacle delay -> do
        lift $ putStrLn $ "\x1b[33mTracisz " ++ show delay ++ " energii.\x1b[0m"
        put GameState {playerEnergy = playerEnergy - delay, ..}
        dice <- lift getDiceRoll
        _ <- movePlayer dice
        return False
      Treasure award -> do
        lift $ putStrLn $ "\x1b[32mZnalazłeś skarb! +" ++ show award ++ case award of
          1 -> " punkt!\x1b[0m"
          2 -> " punkty!\x1b[0m"
          3 -> " punkty!\x1b[0m"
          4 -> " punkty!\x1b[0m"
          _ -> " punktów!\x1b[0m"
        put GameState {playerScore = playerScore + award, ..}
        dice <- lift getDiceRoll
        _ <- movePlayer dice
        return False
      Trap val -> do
        lift $ putStrLn $ "\x1b[31mStraciłeś " ++ show val ++ case val of
          1 -> " punkt!\x1b[0m"
          2 -> " punkty!\x1b[0m"
          3 -> " punkty!\x1b[0m"
          4 -> " punkty!\x1b[0m"
          _ -> " punktów!\x1b[0m"
        put GameState {playerScore = max (playerScore - val) 0, ..}
        dice <- lift getDiceRoll
        _ <- movePlayer dice
        return False

-- Funkcja obsługująca jedną turę gry
-- Zwraca True jeśli gra się zakończyła
playTurn :: AdventureGame Bool
playTurn = do
  gs <- get
  put gs {turns = turns gs + 1, playerEnergy = playerEnergy gs - 1}
  lift $ putStrLn ""
  lift $ displayGameState gs
  handleLocation

-- Funkcja prowadząca grę aż do jej zakończenia
playGame :: AdventureGame ()
playGame = do
  GameState {..} <- get
  if playerEnergy < 0 then lift $ putStrLn "\x1b[31mKoniec gry, skończyła Ci się energia. Spróbuj zagrać ponownie!\x1b[0m" else do
    ifGoal <- playTurn
    (if ifGoal then (do
      lift $ putStrLn "\x1b[32m\nGratulacje! Odnalazłeś legendarny skarb!\x1b[0m"
      lift $ putStrLn $ "Twój wynik końcowy: " ++ show playerScore ++ " punktów w " ++ show turns ++ " turach.") else playGame)


-- FUNKCJE DO INTERAKCJI Z UŻYTKOWNIKIEM --

-- Funkcja pobierająca od użytkownika wynik rzutu kośćmi
getDiceRoll :: IO Int
getDiceRoll = do
  putStr "Podaj wynik rzutu kośćmi (1-6): "
  hFlush stdout
  n <- stringToInt
  validate n where
      validate:: Int -> IO Int
      validate k = if 1 <= k && k <= 6 then return (k - 1) else do
        putStr "Podana liczba powinna być z zakresu 1-6. Spróbuj jeszcze raz: "
        hFlush stdout
        getDiceRoll

-- Funkcja wyświetlająca aktualny stan gry
displayGameState :: GameState -> IO ()
displayGameState GameState {..} = do
  putStrLn $ "Tura: " ++ show turns ++ " | " ++ "Energia: " ++ show playerEnergy ++ " | " ++ "Punkty: " ++ show playerScore

-- Funkcja pobierająca wybór gracza spośród dostępnych opcji
getPlayerChoice :: [String] -> IO Int
getPlayerChoice list = do
  n <- stringToInt
  if 1 <= n && n <= length list then return (n - 1) else do
    putStr $ "Możesz podać wartości z zakresu 1-" ++ show (length list) ++ ". Spróbuj jeszcze raz: "
    hFlush stdout
    getPlayerChoice list



-- URUCHAMIANIE PRZYKŁADOWEJ GRY --


-- Funkcja tworząca przykładową mapę gry
createGameMap :: [Location]
createGameMap = [
  -- 0
  Location Empty "Początek Twojej przygody. Ścieżka prowadzi w głąb lasu." [1],

  -- 1
  Location (Decision ["1. Leśna droga", "2. Górski szlak"]) "Rozwidlenie dróg. Możesz wejść w cienisty las lub pójść górskim szlakiem." [16, 2],

  -- 2
  Location (Obstacle 1) "Droga pod górę kosztuje Cię dużo energii. Nad tobą zbierają się burzowe chmury." [3, 11],

  -- 3
  Location Empty "Zaczyna padać. Postanawiasz schować się pod nawisem skalnym." [4],

  -- 4
  Location (Decision ["1. Znajdź inne schronienie", "2. Wejdź do jaskini"]) "Niedalekie uderzenie pioruna otworzyło tajemne wejście do groty, w której mogą mieszkać trolle.\nCzy odważysz się do niej wejść?" [11, 5],

  -- 5
  Location (Obstacle 1) "W grocie słyszysz trolle. Aby się przed nimi ukryć, schodzisz w głębokie korytarze i gubisz drogę." [6, 8],

  -- 6
  Location (Trap 10) "Trolle Cię odnalazły! Okradają Cię z dobytku. W ostatniej chwili rzucasz się do ucieczki." [7, 8, 10],

  -- 7
  Location (Obstacle 2) "Biegniesz na oślep przez korytarze. Masz wrażenie, że ciągle wracasz w to samo miejsce." [7, 8, 9, 10],

  -- 8
  Location (Treasure 20) "Odnalazłeś skarb trolli! W oddali widzisz światło wpadające przez wyjście z jaskini.\nCzy zdążysz uciec przed trollami?" [9, 10],

  -- 9
  Location (Obstacle 2) "Aby zgubić pogoń, ponownie zagłębiasz się w korytarze. Ucieczka pochłania dużo energii." [10],

  -- 10
  Location Empty "Udało Ci się uciec! Możesz kontynuować podróż górskim szlakiem." [11],

  -- 11
  Location (Decision ["1. Zignoruj orły i przejdź naokoło", "2. Podejdź do gniazda"]) "Wkrótce zaczyna się przejaśniać. Docierasz na szczyt góry, na którym znajduje się gniazdo magicznych orłów." [15, 12],

  -- 12
  Location Empty "Powoli zbliżasz się do gniazda. Nie jesteś pewien, czy orły są przyjaźnie nastawione." [13, 14],

  -- 13
  Location Empty "Orzeł, słysząc o twojej wyprawie, zgodził się zabrać Cię w pobliże świątyni. Pozostawił Cię w mglistej dolinie." [28],

  -- 14
  Location (Trap 5) "Orły okazały się być w zmowie z trollami. Ledwo udaje Ci się uciec, jednak po drodze gubisz część ekwipunku." [15],

  -- 15
  Location (Obstacle 2) "Górska lawina! Zostajesz ranny, co spowalnia Twoją podróż." [23],

  -- 16
  Location (Trap 1) "W lesie mieszkają złe chochliki, które zabierają Ci buty." [17, 18],

  -- 17
  Location (Treasure 5) "W gęstych zaroślach znajdujesz ukryte monety." [18],

  -- 18
  Location (Decision ["1. Bagna", "2. Rzeka"]) "Ścieżka urywa się na bagnach. Możesz spróbować przejść przez bagna lub pójść w górę rzeki." [19, 23],

  -- 19
  Location (Obstacle 2) "Grząskie bagna opóźniają twoją podróż." [20, 21, 22],

  -- 20
  Location (Trap 5) "Spotykasz upiora z bagien, który rzuca na Ciebie urok." [21, 22],

  -- 21
  Location (Treasure 10) "Na bagnach stoją ruiny zamku. W środku znajdujesz cudowny złoty kielich." [22],

  -- 22
  Location Empty "Błądząc po bagnie natrafiasz na magiczny portal, który może Cię przenieść w inne miejsce mapy." [31, 32, 33, 34],

  -- 23
  Location (Obstacle 2) "Aby kontynuować podróż, musisz przeprawić się na drugą stronę rzeki. Postanawiasz szukać tratwy." [24, 27],

  -- 24
  Location (Obstacle 1) "Podczas poszukiwań natrafiłeś na most strzeżony przez rzecznego węża. Musisz odpowiedzieć na zagadkę." [25, 26],

  -- 25
  Location (Treasure 1) "Udało Ci się! Wąż w nagrodę wręcza Ci swoją srebrną łuskę." [28],

  -- 26
  Location (Decision ["1. Próbuj dalej", "2. Zrezygnuj i poszukaj tratwy"]) "Nie udało Ci się rozwiązać tej zagadki. Wąż jest jednak dzisiaj w dobrym humorze\ni zamiaść Cię pożreć, pozwala Ci spróbować jeszcze raz lub kontynuować szukanie tratwy." [26, 27],

  -- 27
  Location (Obstacle 1) "W końcu udało Ci się znaleźć tratwę i z niewielkim trudem docierasz na drugi brzeg." [28],

  -- 28
  Location (Decision ["1. Do skarbu", "2. Do gniazda węży"]) "Ruszasz dalej ścieżką. Mgła jest coraz gęstsza, na szczęście trafiasz na rozstaj z drogowskazem." [30, 29],

  -- 29
  Location (Obstacle 2) "Droga rzeczywiście prowadziła do gniazda węży! Zawracasz i wybierasz drogę do skarbu." [30],

  -- 30
  Location Goal "Dotarłeś do ukrytej świątyni! Twój cel znajduje się przed tobą." [],

  -- POŁĄCZENIA Z MAGICZNYM PORTALEM

  -- 31
  Location Empty "Zostałeś cofnięty do początku swojej podróży!" [1],

  -- 32
  Location Empty "Zostałeś przeniesiony do jaskini górskich trolli!" [5],

  -- 33
  Location Empty "Zostałeś przeniesiony na drugą stronę bagien!" [28],

  -- 34
  Location Empty "Zostałeś przeniesiony w pobliże rzeki!" [23]
  ]



-- Funkcja main do uruchomienia gry
main :: IO ()
main = do
  putStrLn "Poszukiwacze Skarbów"
  putStrLn "===================="
  putStrLn "Rozpoczynasz poszukiwanie legendarnego skarbu ukrytego w starożytnej świątyni."
  putStrLn "Masz ograniczoną energię, więc wybieraj mądrze swoją trasę!"

  -- Inicjalizacja gry
  let initialState = GameState {
    playerPosition = 0,
    playerEnergy = 30,
    playerScore = 0,
    gameMap = createGameMap,
    visitedLocations = [],
    turns = 0
  }

  do
    runStateT playGame initialState
    return ()

  putStrLn "Dziękujemy za grę!"

