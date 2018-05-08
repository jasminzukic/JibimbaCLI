module Display where

import Model

import Control.Concurrent (readMVar)
import System.Console.ANSI (clearScreen)


displaySyntagma :: GameSettings -> IO ()
displaySyntagma mGame = do
  clearScreen
  g <- readMVar mGame
  if null (syntagmas g)
    then putStrLn ""
    else do
      putStrLn "press ENTER when guessed\n"
      putStrLn $ show (time g) ++ "\n"
      putStrLn $ head $ syntagmas g



displayTeamInput :: Int -> IO ()
displayTeamInput n = putStrLn $ "Please enter the name of Team "
                  ++ (show n)
                  ++ ". Enter empty name to continue."

displaySyntagmaInput :: [String] -> IO ()
displaySyntagmaInput syns = do
  if length syns > 0 -- just for players to have a feel that they inputed something
    then putStrLn $ "Currently " ++ show (length syns) ++ " syntagmas"
    else putStr ""
  putStrLn "Please enter a syntagma, \"play\" to start playing or \"quit\" to quit"
  putStrLn "For example \"COLORFUL CONCEPT\""



displayRoundDescription :: Int -> IO ()
displayRoundDescription r = do
  putStrLn $ "Round " ++ show r ++ " Start!\n"
  case r of
    1 -> putStrLn
      "In the first round your goal is to describe the syntagma verbally.\n\
      \You can use all words except for roots and foreign languages. \n\
      \For a higher difficulty you can avoid using gesticulations.\n"
    2 -> putStrLn
      "In the second round your goal is to describe the syntagma using only gesticulations.\n\
      \Like you're playing charades.\n\
      \For a higher difficulty you can avoid producing sounds or pointing to things in your vicinity.\n"
    3 -> putStrLn
      "In the second round your goal is to describe the syntagma using only ONE word.\n\
      \Even though there are two words in a syntagma, use only one word to describe them both.\n\
      \For a higher difficulty you can avoid eye contact and reacting to your teammates guesses.\n"
    _ -> putStrLn "Rounds above 3 are not yet described"


displayScores :: Game -> IO ()
displayScores g = do
  let teamList = teams g
  mapM_ printScores teamList
  return ()
  where
    printScores :: (String, Int) -> IO ()
    printScores (name, points) = do
      putStrLn $ "Team " ++ name ++ ": " ++ show points ++ "\n"
