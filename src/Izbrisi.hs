import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan

gossipGirl chan = do
  forever $ do
    gossip <- readChan chan
    putStrLn gossip

main :: IO ()
main = do
  putStrLn "Lets do some gossips"
  gossipChan <- newChan -- lets make new chan
  forkIO $ gossipGirl gossipChan -- spawn gossipGirl
  writeChan gossipChan "Garbage is garbage!"
  writeChan gossipChan "Garbage is garbage for reals!"
  getLine
  putStrLn "Thank You Sir for Info"
