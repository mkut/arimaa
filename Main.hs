module Main where
import Prelude hiding (Left, Right)
import Data.Maybe
import Data.Array
import Data.Array.IO
import Data.Char (toUpper, toLower, isUpper, isLower, ord, chr, intToDigit)
import Data.List (sort, transpose)
import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, when, liftM2, liftM3)
import System.IO
import qualified System.Console.ANSI as ANSI
import GHC.Exts (groupWith)

-- main function
main :: IO ()
main = repeatM_ game

-- Arimaa data structures
data Piece = Rabbit
           | Cat
           | Dog
           | Horse
           | Camel
           | Elephant
           deriving (Eq, Ord, Enum, Show)

data Player = FirstP | LastP
            deriving (Eq, Show)

opponentOf :: Player -> Player
opponentOf FirstP = LastP
opponentOf LastP  = FirstP

type Position = (Char, Int)
type Tile = Maybe (Player, Piece)
type Board = Array Position Tile
type IOBoard = IOArray Position Tile

data Direction = Left | Down | Right | Up
               deriving (Eq, Enum)

data Move = Move Position Direction
          | Push Position Direction
          | Pull Position Direction
          deriving (Eq)

-- game control functions
game :: IO ()
game = do
   board <- newArray boardBound Nothing
   initialize board FirstP

initialize :: IOBoard -> Player -> IO ()
initialize board p = do
   printBoard p board
   putStrLn "Input your initial formation."
   formation <- appDefault <$> getLineWithPrompt
   if isValid formation
      then do
         mapM_ (uncurry $ writeArray board) $ zip (initialPositions p) (map (tileFromChar p) formation)
         if p == FirstP
            then initialize board (opponentOf p)
         else
            turn board (opponentOf p)
      else initialize board p
   where
      isValid = (== sort "EMHHDDCCRRRRRRRR") . sort
      initialPositions FirstP = [ (x, y) | y <- [2, 1], x <- ['a' .. 'h'] ]
      initialPositions LastP  = [ (x, y) | y <- [7, 8], x <- ['h', 'g' .. 'a'] ]
      appDefault "" = defaultFormation p
      appDefault x  = x
      defaultFormation FirstP = "RMREHRHRRCRDDRCR"
      defaultFormation LastP  = "HMCEHCDRRRRRDRRR"

turn :: IOBoard -> Player -> IO ()
turn board p = do
   chk <- winCheck board
   case chk of
      Nothing         -> turn' board p 4
      Just Nothing    -> putStrLn $ "DRAW"
      (Just (Just p)) -> putStrLn $ show p ++ " wins!"

turn' :: IOBoard -> Player -> Int -> IO ()
turn' board p 0 = turn board (opponentOf p)
turn' board p n = do
   printBoard p board
   cmd <- getLineWithPrompt
   try cmd
   where
      try "end"
         | n == 4    = retry "You must move a piece at least once."
         | otherwise = turn board (opponentOf p)
      try x
         | isValid x = try' (parse x)
         | otherwise = retry "Invalid format."
         where
            parse [a, b, c] = Move (a, read [b]) (directionFromChar c)
            parse [a, b, c, '+'] = Push (a, read[b]) (directionFromChar c)
            parse [a, b, c, '-'] = Pull (a, read[b]) (directionFromChar c)
            isValid [a, b, c]
               = and [ elem a columns
                     , elem b charRows
                     , elem c charDirections
                     ]
            isValid [a, b, c, d]
               = isValid [a, b, c] && (d == '+' || d == '-') && n >= 2
            isValid _ = False
      try' (Move pos dir) = do
         y <- readArray board pos
         case y of
            Nothing      -> retry "There isn't your piece."
            Just (y', yp) -> if y' /= p
               then retry "It's opponent's piece."
               else do
                  frz <- freezed board pos
                  if frz
                     then retry "The piece is freezed."
                     else case adjacent p pos dir of
                        Nothing     -> retry "No space to move."
                        Just adjpos -> do
                           e <- readArray board adjpos
                           case (y, e) of
                              (Just (_, piece), Nothing) -> do
                                 if piece == Rabbit && dir == Down
                                 then retry "Rabbit can't move back."
                                 else do
                                    writeArray board adjpos =<< readArray board pos
                                    writeArray board pos Nothing
                                    trapCheck board
                                    turn' board p (n-1)
                              _ -> retry "No space to move/There isn't your piece."
      try' (Push pos dir) = do
         y <- readArray board pos
         case y of
            Nothing      -> retry "There isn't your piece."
            Just (y', yp) -> if y' /= p
               then retry "It's opponent's piece."
               else do
                  frz <- freezed board pos
                  if frz
                     then retry "The piece is freezed."
                     else case pushadjacent p pos dir of
                        Nothing                 -> retry "No space to push."
                        Just (adjpos1, adjpos2) -> do
                           (o, e) <- liftM2 (,) (readArray board adjpos1) (readArray board adjpos2)
                           case (o, e) of
                              (Just (o', op), Nothing) -> if y' /= o' && yp > op
                                 then do
                                    writeArray board adjpos2 =<< readArray board adjpos1
                                    writeArray board adjpos1 =<< readArray board pos
                                    writeArray board pos Nothing
                                    trapCheck board
                                    turn' board p (n-2)
                                 else retry $ show yp ++ " is too weak to push" ++ show op ++ "."
                              _ -> retry "No space to push/There isn't your or opponent's piece."
      try' (Pull pos dir) = do
         y <- readArray board pos
         case y of
            Nothing      -> retry "There isn't your piece."
            Just (y', yp) -> if y' /= p
               then retry "It's opponent's piece."
               else do
                  frz <- freezed board pos
                  if frz
                     then retry "The piece is freezed."
                     else case pulladjacent p pos dir of
                        Nothing                 -> retry "No space to pull."
                        Just (adjpos1, adjpos2) -> do
                           (e, o) <- liftM2 (,) (readArray board adjpos1) (readArray board adjpos2)
                           case (e, o) of
                              (Nothing, Just (o, op)) -> if y' /= o && yp > op
                                 then do
                                    writeArray board adjpos1 =<< readArray board pos
                                    writeArray board pos =<< readArray board adjpos2
                                    writeArray board adjpos2 Nothing
                                    trapCheck board
                                    turn' board p (n-2)
                                 else retry $ show yp ++ " is too weak to pull" ++ show op ++ "."
                              _ -> retry "No space to pull/There isn't your or opponent's piece."
      retry str = do
         putStrLn $ "invalid input: " ++ str
         turn' board p n

trapCheck :: IOBoard -> IO ()
trapCheck board = forM_ traps $ \pos -> do
   tile <- readArray board pos
   case tile of
      Nothing         -> return ()
      Just (p, piece) -> do
         safe <- any ((==p) . fst) . catMaybes <$> mapM (readArray board) (adjacents pos)
         if safe
            then return ()
            else writeArray board pos Nothing

{- TODO endless repeat -}
winCheck :: IOBoard -> IO (Maybe (Maybe Player))
winCheck board = do
   x <- winCheck'
   case x of
      Just x  -> return (Just x)
      Nothing -> do
         y <- drawCheck
         case y of
            Just x -> return (Just x)
            Nothing -> return Nothing
   where
      winCheck' = do
         chk <- forM columns $ \x -> do
            tile <- readArray board (x, 8)
            case tile of
               Just (FirstP, Rabbit) -> return (Just FirstP)
               _                     -> return Nothing
         chk2 <- forM columns $ \x -> do
            tile <- readArray board (x, 1)
            case tile of
               Just (LastP, Rabbit) -> return (Just LastP)
               _                    -> return Nothing
         case catMaybes (chk ++ chk2) of
            [] -> drawCheck
            (FirstP:_) -> return (Just (Just FirstP))
            (LastP:_) -> return (Just (Just LastP))
      drawCheck = do
         chk <- elem Rabbit . map snd . catMaybes <$> getElems board
         if chk then return Nothing else return (Just Nothing)

freezed :: IOBoard -> Position -> IO Bool
freezed board pos = do
   tile <- readArray board pos
   case tile of
      Nothing -> return False
      Just (w, p) -> do
         adjs <- catMaybes <$> mapM (readArray board) (adjacents pos)
         return $ all ((/=w) . fst) adjs && any ((>p) . snd) adjs

adjacents :: Position -> [Position]
adjacents pos = mapMaybe (adjacent FirstP pos) directions

adjacent :: Player -> Position -> Direction -> Maybe Position
adjacent LastP  p      d     = adjacent FirstP p (invdir d)
adjacent FirstP (x, y) Left  = if x == 'a' then Nothing else Just (chr (ord x - 1), y)
adjacent FirstP (x, y) Right = if x == 'h' then Nothing else Just (chr (ord x + 1), y)
adjacent FirstP (x, y) Up    = if y == 8   then Nothing else Just (x, y + 1)
adjacent FirstP (x, y) Down  = if y == 1   then Nothing else Just (x, y - 1)

pushadjacent :: Player -> Position -> Direction -> Maybe (Position, Position)
pushadjacent LastP  p      d     = pushadjacent FirstP p (invdir d)
pushadjacent FirstP (x, y) Left  = if x <= 'b' then Nothing else Just ((chr (ord x - 1), y), (chr (ord x - 2), y))
pushadjacent FirstP (x, y) Right = if x >= 'g' then Nothing else Just ((chr (ord x + 1), y), (chr (ord x + 2), y))
pushadjacent FirstP (x, y) Up    = if y >= 7   then Nothing else Just ((x, y + 1), (x, y + 2))
pushadjacent FirstP (x, y) Down  = if y <= 2   then Nothing else Just ((x, y - 1), (x, y - 2))

pulladjacent :: Player -> Position -> Direction -> Maybe (Position, Position)
pulladjacent LastP  p      d     = pulladjacent FirstP p (invdir d)
pulladjacent FirstP (x, y) Left  = if x == 'a' || x == 'h' then Nothing else Just ((chr (ord x - 1), y), (chr (ord x + 1), y))
pulladjacent FirstP (x, y) Right = if x == 'a' || x == 'h' then Nothing else Just ((chr (ord x + 1), y), (chr (ord x - 1), y))
pulladjacent FirstP (x, y) Up    = if y == 1   || y == 8   then Nothing else Just ((x, y + 1), (x, y - 1))
pulladjacent FirstP (x, y) Down  = if y == 1   || y == 8   then Nothing else Just ((x, y - 1), (x, y + 1))

invdir :: Direction -> Direction
invdir Left  = Right
invdir Right = Left
invdir Up    = Down
invdir Down  = Up

-- game constants
boardBound :: (Position, Position)
boardBound = (('a', 1), ('h', 8))

columns :: [Char]
columns = ['a' .. 'h']

rows :: [Int]
rows = [1 .. 8]

charRows :: [Char]
charRows = ['1' .. '8']

pieces :: [Piece]
pieces = [Rabbit .. Elephant]

charPieces :: [Char]
charPieces = ['E', 'M', 'H', 'D', 'C', 'R']

directions :: [Direction]
directions = [Left .. Up]

charDirections :: [Char]
charDirections = ['L', 'D', 'R', 'U']

traps :: [Position]
traps = [('c', 3), ('c', 6), ('f', 3), ('f', 6)]

-- visualize functions
printBoard :: Player -> IOBoard -> IO ()
printBoard p board = do
   b <- toShowOrder <$> getAssocs board
   putStrLn $ " " ++ columnHeader
   forM_ (zip rowHeader b) $ \(i, line) -> do
      putChar i
      forM_ line $ \(pos, tile) -> do
         when (elem pos traps) $ setTerminalColor ANSI.Red
         putChar $ tileToChar p tile
         when (elem pos traps) $ setTerminalReset
      putStrLn ""
   where
      toShowOrder = if p == FirstP then toShowOrderForFirstPlayer else toShowOrderForLastPlayer
      toShowOrderForFirstPlayer = reverse . transpose . groupWith (fst . fst)
      toShowOrderForLastPlayer  = map reverse . transpose . groupWith (fst . fst)
      columnHeader = (if p == FirstP then id else reverse) columns
      rowHeader    = (if p == FirstP then reverse else id) $ map intToDigit rows

tileToChar :: Player -> Tile -> Char
tileToChar _ Nothing               = '.'
tileToChar p (Just (whose, piece)) = fixCase (whose == p) $ pieceToChar piece

pieceToChar :: Piece -> Char
pieceToChar Elephant = 'E'
pieceToChar Camel    = 'M'
pieceToChar Horse    = 'H'
pieceToChar Dog      = 'D'
pieceToChar Cat      = 'C'
pieceToChar Rabbit   = 'R'

fixCase :: Bool -> Char -> Char
fixCase True  = toUpper
fixCase False = toLower

setTerminalColor :: ANSI.Color -> IO ()
setTerminalColor c = ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]

setTerminalReset :: IO ()
setTerminalReset = ANSI.setSGR [ANSI.Reset]

-- input functions
tileFromChar :: Player -> Char -> Tile
tileFromChar p c = Just (playerFromChar p c, pieceFromChar c)

pieceFromChar :: Char -> Piece
pieceFromChar c = case toUpper c of
   'E' -> Elephant
   'M' -> Camel
   'H' -> Horse
   'D' -> Dog
   'C' -> Cat
   'R' -> Rabbit
   _   -> undefined

playerFromChar :: Player -> Char -> Player
playerFromChar p c
   | isUpper c = p
   | isLower c = opponentOf p
   | otherwise = undefined

directionFromChar :: Char -> Direction
directionFromChar 'L' = Left
directionFromChar 'D' = Down
directionFromChar 'R' = Right
directionFromChar 'U' = Up

-- general functions
repeatM_ :: IO a -> IO ()
repeatM_ x = do { x; repeatM_ x }

getLineWithPrompt :: IO String
getLineWithPrompt = do
   putStr "> "
   hFlush stdout
   getLine

-- vim: set expandtab:
