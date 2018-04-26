{-# LANGUAGE OverloadedStrings #-}

module Main where

import Display
import Input
import Model

import Control.Concurrent
import Control.Monad
import Data.Char
import qualified Data.Text as T
import System.Console.ANSI
import System.Random.Shuffle



main :: IO ()
main = do
  clearScreen
  let g = Game { syntagmas = [], guessed = []
                , teams = [], currentTeam = 0
                , time = 5
                , runda = 1 }
  g <- inputTeams g 1
  g <- inputSyns g
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
