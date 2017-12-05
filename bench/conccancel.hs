import Control.Exception
import Control.Concurrent.Async
import System.Environment
import Control.Monad
import Control.Concurrent

main = runInUnboundThread $ do
  [n] <- fmap (fmap read) getArgs
  runConcurrently $ traverse Concurrently $
    replicate n (threadDelay 1000000) ++ [throwIO (ErrorCall "oops")]

