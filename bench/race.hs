import Control.Concurrent.Async
import System.Environment
import Control.Monad
import Control.Concurrent

main = runInUnboundThread $ do
  [n] <- fmap (fmap read) getArgs
  replicateM_ n $ concurrently (return 1) (return 2)
