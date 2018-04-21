{-# LANGUAGE OverloadedStrings #-}


--TODO
--test samo 2 riječi


module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import System.Console.ANSI
import System.Random.Shuffle

data Game = Game
  { syntagmas :: [String]
  , guessed :: [String]
  , team :: (Int, String)
  , time :: Int
  , round :: Int
  } deriving (Eq, Show)



main :: IO ()
main = do
  clearScreen
  putStrLn "Enter number:"
  chooseMode

mainAgain :: IO ()
mainAgain = do
  putStrLn "Only these numbers are acceptable!"
  chooseMode

chooseMode :: IO ()
chooseMode = do
  putStrLn "1 - New Game"
  putStrLn "2 - Rules"
  putStrLn "3 - Settings"
  line <- getLine
  let number = T.strip $ T.pack line
  case number of
    "1" -> inputSyns []
    _ -> do
      clearScreen
      mainAgain

inputSyns :: [String] -> IO ()
inputSyns syns = do
  clearScreen
  if length syns > 0 -- just for players to have a feel that they inputed something
    then putStrLn $ "Currently " ++ show (length syns) ++ " syntagmas"
    else putStr ""
  putStrLn "Please enter a syntagma, \"play\" to start playing or \"quit\" to quit"
  putStrLn "For example \"COLORFUL CONCEPT\""
  syn <- getLine
  case syn of
    "play" -> do
      shuffledSyns <- shuffleM syns
      msyns <- newMVar shuffledSyns
      mguessed <- newMVar []
      playGame msyns mguessed 1

    "quit" -> return ()

    _      -> do
      let newSyns = syn:syns
      inputSyns newSyns

playGame :: MVar [String] -> MVar [String] -> Int ->  IO ()
playGame msyns mguessed r = do
  clearScreen
  putStrLn $ "Round " ++ show r ++ " Start!\n"
  putStrLn "Opis runde\n"
  putStrLn "Press ENTER to start"
  getLine
  clearScreen
  playRound msyns mguessed 1 60
  --shuffle for new round
  guessed <- takeMVar mguessed
  putMVar mguessed []
  shuffledSyns <- shuffleM guessed
  putMVar msyns shuffledSyns
  playGame msyns mguessed (r+1)


playRound :: MVar [String] -> MVar [String] -> Int -> Int ->  IO ()
playRound msyns mguessed team n = do
  syns <- readMVar msyns
  if null syns
    then do
      return ()
    else do
      putStrLn $ "Team " ++ show team ++ ", press ENTER to start!"
      getLine
      syns <- takeMVar msyns
      let x = head syns
      let xs = tail syns
      putMVar msyns xs
      m <- newMVar x
      t <- newMVar 10
      tid <- forkIO (changeWord msyns mguessed m t)
      countdown m t
      killThread tid
      putStrLn "Gotova runda. Press ENTER to continue!"
      playRound msyns mguessed (team+1) 60


  -- mapM_ putStrLn newSyns

changeWord :: MVar [String] -> MVar [String] -> MVar String -> MVar Int -> IO ()
changeWord msyns mguessed m t = do
  getLine
  syns <- readMVar msyns
  -- mapM_ putStrLn syns
  if null syns
    then do
      time <- takeMVar t
      putMVar t 0
      clearScreen
      return ()
    else do
      syns <- takeMVar msyns
      guessed <- takeMVar mguessed
      s <- takeMVar m
      putMVar mguessed (s:guessed)
      putMVar m (head syns)
      putMVar msyns (tail syns)
      display m t
      changeWord msyns mguessed m t

countdown :: MVar String -> MVar Int -> IO ()
countdown m t = do
  time <- readMVar t
  if time <= 0
    then do
      putStrLn "Time's up! Press ENTER to continue"
      return ()
    else do
      display m t
      garbage <- takeMVar t
      putMVar t (time-1)
      threadDelay 1000000
      countdown m t

display :: MVar String -> MVar Int -> IO ()
display m t = do
  clearScreen
  time <- readMVar t
  syn <- readMVar m
  putStrLn "press ENTER when guessed\n"
  print time
  putStrLn ""
  putStrLn syn

-- displaySyntagma :: [String] -> [String] -> Int -> IO ()
-- displaySyntagma [] guessed n = return ()
-- displaySyntagma syns@(x:xs) guessed n = do
--   time <- newMVar False
--   word <- newMVar x
--   guessedWords <- newMVar []
--   tid <- forkIO $ countdown n word time
--   forever $ do
--     getLine
--     l <- takeMVar time
--     putMVar time True
--     if null xs || l
--       then do
--         ss <- takeMVar word
--         gs <- takeMVar guessedWords
--         endGame (ss:xs) gs
--       else do
--         (newX:newXs) <- shuffleM xs
--         g <- takeMVar word
--         gs <- takeMVar guessedWords
--         putMVar guessedWords (g:gs)
--         putMVar word newX
--
-- countdown :: Int -> MVar String -> MVar Bool -> IO ()
-- countdown n word time = do
--   s <- takeMVar word
--   if n <= 0
--     then do
--       clearScreen
--       l <- takeMVar time
--       putMVar time True
--       threadDelay 1000000
--       return ()
--     else do
--       clearScreen
--       putStrLn "press ENTER when guessed\n"
--       print n
--       putStrLn ""
--       putStrLn s
--       threadDelay 1000000
--       putMVar word s
--       countdown (n-1) word time

endGame :: [String] -> [String] -> IO ()
endGame syns guessed = do
  mapM_ putStrLn syns
