import Control.Monad.State

tick :: State Int Int
tick = do
  n <- get
  put (n+1)
  return n

runTick = runState tick 5


