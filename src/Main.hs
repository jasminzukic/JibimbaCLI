{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Display
import Input
import Model

import Control.Concurrent
-- import qualified Data.Text as T ()
import System.Console.ANSI (clearScreen)
import System.Random.Shuffle (shuffleM)



main :: IO ()
main = do
  clearScreen
  let g = Game { syntagmas = [], guessed = []
                , teams = [], currentTeam = 0
                , time = 5
                , runda = 1 }
  g' <- inputTeams g 1
  g <- inputSyns g'
  mGame <- newMVar g
  playGame mGame

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


playGame :: GameSettings -> IO ()
playGame mGame = do
  clearScreen
  g <- readMVar mGame
  let r = runda g
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


changeWord :: GameSettings -> IO ()
changeWord mGame = do
  getLine -- when a player presses ENTER change the word and update game
  g <- readMVar mGame
  if null (syntagmas g)
    then do
      clearScreen
      return ()
    else do
      g <- takeMVar mGame
      let newSyns = tail $ syntagmas g
      let newGuessed = (head (syntagmas g)) : guessed g
      let updatedTeams = updateTeams g
      let updatedGame = g { syntagmas = newSyns
                          , guessed = newGuessed
                          , teams = updatedTeams
                          }
      putMVar mGame updatedGame
      displaySyntagma mGame
      changeWord mGame

updateTeams :: Game -> [(String, Int)]
updateTeams g = (take n teamList) ++ [updatedTeam] ++ (drop (n+1) teamList)
  where
    teamList = teams g
    updatedTeam = (fst currentT, (snd currentT) + 1)
    currentT = teamList !! n
    n = currentTeam g

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
        displaySyntagma mGame
        g <- takeMVar mGame
        let newTime = (time g) - 1
        let updatedGame = g { time = newTime }
        putMVar mGame updatedGame
        threadDelay 1000000
        countdown mGame
