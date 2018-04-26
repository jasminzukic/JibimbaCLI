module Input where

import Display
import Model

import Control.Concurrent
import Control.Monad
import Data.Char
import qualified Data.Text as T
import System.Console.ANSI
import System.Random.Shuffle


inputTeams :: Game -> Int -> IO Game
inputTeams g n = do
  clearScreen
  displayTeamInput n
  l <- getLine
  case l of
    "" -> return g
    _  -> do
      let t = teams g
      let g' = g { teams = (l,0):t }
      inputTeams g' (n+1)


inputSyns :: Game -> IO Game -- TODO
inputSyns g = do
  clearScreen
  let syns = syntagmas g
  displaySyntagmaInput syns --
  syn <- getLine
  case syn of
    "play" -> do
      shuffledSyns <- shuffleM syns
      return $ g { syntagmas = shuffledSyns }

    -- "quit" -> do
    --   putStrLn "You decided to quit the game. Press ENTER to confirm."
    --   getLine
    --   return g -- TODO quit mora završiti igru

    _      -> do
      g <- validateSyntagma g syn
      inputSyns g

validateSyntagma :: Game -> String -> IO Game
validateSyntagma g syn = do
  let syns = syntagmas g
  let s = words syn
  if length s /= 2
    then do
      clearScreen
      putStrLn "You must input exactly 2 words! Press ENTER to continue."
      getLine
      return g
    else if or $ map (any (not . isLetter)) s
      then do
        clearScreen
        putStrLn $ "You've entered: " ++ syn
        putStrLn "Your words can only contain letters. No numbers or punctuation. Press ENTER to continue."
        getLine
        return g
      else return g { syntagmas = syn:syns }
