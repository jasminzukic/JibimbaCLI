{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Char
import qualified Data.Text as T
import System.Console.ANSI
import System.Random.Shuffle

data Game = Game
  { syntagmas :: [String]
  , guessed :: [String]
  , teams :: [(String, Int)]
  , currentTeam :: Int
  , time :: Int
  , runda :: Int
  } deriving (Eq, Show)

type GameSettings = MVar Game

main :: IO ()
main = do
  clearScreen
  mGame <- newMVar $ Game { syntagmas = []
                           , guessed = []
                           , teams = []
                           , currentTeam = 0
                           , time = 5
                           , runda = 1
                           }
  inputTeams mGame 1

-- main :: IO ()
-- main = do
--   clearScreen
--   putStrLn "Enter number:"
--   chooseMode
--
-- mainAgain :: IO ()
-- mainAgain = do
--   putStrLn "Only these numbers are acceptable!"
--   chooseMode
--
-- chooseMode :: IO ()
-- chooseMode = do
--   putStrLn "1 - New Game"
--   putStrLn "2 - Rules"
--   putStrLn "3 - Settings"
--   line <- getLine
--   let number = T.strip $ T.pack line
--   case number of
--     "1" -> do
--       mGame <- newMVar $ Game { syntagmas = []
--                           , guessed = []
--                           , teams = []
--                           , currentTeam = 0
--                           , time = 5
--                           , runda = 1
--                           }
--       inputTeams mGame 1
--     _ -> do
--       clearScreen
--       mainAgain


inputTeams :: GameSettings -> Int -> IO ()
inputTeams mGame n = do
  clearScreen
  putStrLn $ "Please enter the name of Team "
          ++ (show n)
          ++ ". Enter empty name to continue."
  l <- getLine
  case l of
    "" -> inputSyns mGame
    _  -> do
      g <- takeMVar mGame
      let t = teams g
      let updatedGame = g { teams = (l,0):t }
      putMVar mGame updatedGame
      inputTeams mGame (n+1)

inputSyns :: GameSettings -> IO ()
inputSyns mGame = do
  clearScreen
  g <- readMVar mGame
  let syns = syntagmas g
  displaySyntagmaInput syns --
  syn <- getLine
  case syn of
    "play" -> do
      shuffledSyns <- shuffleM syns
      g <- takeMVar mGame
      let updatedGame = g { syntagmas = shuffledSyns }
      putMVar mGame updatedGame
      playGame mGame

    "quit" -> do
      putStrLn "You decided to quit the game. Press ENTER to confirm."
      getLine
      return ()

    _      -> validateSyntagma mGame syn

validateSyntagma :: GameSettings -> String -> IO ()
validateSyntagma mGame syn = do
  g <- takeMVar mGame
  let syns = syntagmas g
  let s = words syn
  if length s /= 2
    then do
      clearScreen
      putStrLn "You must input exactly 2 words! Press ENTER to continue."
      getLine
      putMVar mGame g
    else if or $ map (any (not . isLetter)) s
      then do
        clearScreen
        putStrLn $ "You've entered: " ++ syn
        putStrLn "Your words can only contain letters. No numbers or punctuation. Press ENTER to continue."
        getLine
        putMVar mGame g
      else do
        let updatedGame = g { syntagmas = syn:syns }
        putMVar mGame updatedGame
  inputSyns mGame

displaySyntagmaInput :: [String] -> IO ()
displaySyntagmaInput syns = do
  if length syns > 0 -- just for players to have a feel that they inputed something
    then putStrLn $ "Currently " ++ show (length syns) ++ " syntagmas"
    else putStr ""
  putStrLn "Please enter a syntagma, \"play\" to start playing or \"quit\" to quit"
  putStrLn "For example \"COLORFUL CONCEPT\""

playGame :: GameSettings -> IO ()
playGame mGame = do
  clearScreen
  g <- readMVar mGame
  let r = runda g
  putStrLn $ "Round " ++ show r ++ " Start!\n"

  displayRoundDescription r

  playRound mGame

  --prepare for new round
  g <- readMVar mGame
  if runda g >= 3
    then endGame g
    else do
      g <- takeMVar mGame
      newSyns <- shuffleM $ guessed g
      let updatedGame = g { syntagmas = newSyns, guessed = [], runda = r+1 }
      putMVar mGame updatedGame
      playGame mGame

displayRoundDescription :: Int -> IO ()
displayRoundDescription 1 = putStrLn
  "In the first round your goal is to describe the syntagma verbally.\n\
  \You can use all words except for roots and foreign languages. \n\
  \For a higher difficulty you can avoid using gesticulations.\n"
displayRoundDescription 2 = putStrLn
  "In the second round your goal is to describe the syntagma using only gesticulations.\n\
  \Like you're playing charades.\n\
  \For a higher difficulty you can avoid producing sounds or pointing to things in your vicinity.\n"
displayRoundDescription 3 = putStrLn
  "In the second round your goal is to describe the syntagma using only ONE word.\n\
  \Even though there are two words in a syntagma, use only one word to describe them both.\n\
  \For a higher difficulty you can avoid eye contact and reacting to your teammates guesses.\n"
displayRoundDescription _ = putStrLn "Rounds above 3 are not yet described"

endGame :: Game -> IO ()
endGame g = do
  clearScreen
  putStrLn "Game Over! Press ENTER to end.\n"
  displayScores g
  getLine
  return ()

playRound :: GameSettings -> IO ()
playRound mGame = do
  g <- readMVar mGame
  if null $ syntagmas g
    then do
      putStrLn "Gotova runda! Press ENTER to continue!\n"
      displayScores g
      getLine
      return ()
    else do
      let team = fst $ (teams g) !! currentTeam g
      putStrLn $ "Team " ++ team ++ ", press ENTER to start!"
      getLine
      tid <- forkIO (changeWord mGame)
      countdown mGame
      killThread tid
      g <- takeMVar mGame

      newSyns <- shuffleM $ syntagmas g
      let nextTeam = (currentTeam g + 1) `mod` length (teams g)
      let updatedGame = g { syntagmas = newSyns
                          , currentTeam = nextTeam
                          , time = 5 }
      putMVar mGame updatedGame
      playRound mGame

displayScores :: Game -> IO ()
displayScores g = do
  let teamList = teams g
  mapM_ printScores teamList
  return ()
  where
    printScores :: (String, Int) -> IO ()
    printScores (name, points) = do
      putStrLn $ "Team " ++ name ++ ": " ++ show points ++ "\n"

changeWord :: GameSettings -> IO ()
changeWord mGame = do
  getLine
  g <- readMVar mGame
  if null (syntagmas g)
    then do
      clearScreen
      return ()
    else do
      g <- takeMVar mGame
      let newSyns = tail $ syntagmas g
      let newGuessed = (head (syntagmas g)) : guessed g
      updatedTeams <- updateTeams g
      let updatedGame = g { syntagmas = newSyns
                          , guessed = newGuessed
                          , teams = updatedTeams
                          }
      putMVar mGame updatedGame
      display mGame
      changeWord mGame

updateTeams :: Game -> IO [(String, Int)]
updateTeams g = do
  let teamList = teams g
  let n = currentTeam g
  let currentT = teamList !! n
  let updatedTeam = (fst currentT, (snd currentT) + 1)
  let updatedTeams = (take n teamList)
                  ++ [updatedTeam]
                  ++ (drop (n+1) teamList)
  return updatedTeams

countdown :: GameSettings -> IO ()
countdown mGame = do
  g <- readMVar mGame
  if null (syntagmas g)
    then do
      return ()
    else if time g <= 0
      then do
        clearScreen
        putStr "Time's up! "
        return ()
      else do
        display mGame
        g <- takeMVar mGame
        let newTime = (time g) - 1
        let updatedGame = g { time = newTime }
        putMVar mGame updatedGame
        threadDelay 1000000
        countdown mGame

display :: GameSettings -> IO ()
display mGame = do
  clearScreen
  g <- readMVar mGame
  if null (syntagmas g)
    then putStrLn ""
    else do
      putStrLn "press ENTER when guessed\n"
      print $ time g
      putStrLn ""
      putStrLn $ head $ syntagmas g
