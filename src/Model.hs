module Model where

import Control.Concurrent (MVar)

data Game = Game
  { syntagmas :: [String]
  , guessed :: [String]
  , teams :: [(String, Int)]
  , currentTeam :: Int
  , time :: Int
  , runda :: Int
  } deriving (Eq, Show)

type GameSettings = MVar Game
