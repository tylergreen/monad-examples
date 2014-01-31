import Control.Monad.Trans.State
import Control.Monad.Trans.Class

tick :: State Int Int
tick = do
  n <- get
  put (n+1)
  return n

runTick = runState tick 5


data Weather = Weather {
 _temperature :: Float
, _humidity :: Float
} deriving (Show)

promptW :: StateT Weather IO ()
promptW = do
 lift $ putStrLn "input current temp"
 t <- lift $ (readLn :: IO Float)
 w <- get
 put $ w { _temperature = t }
 w <- get
 lift $ putStrLn ("new temp" ++ (show w))

weather0 :: Weather
weather0 = Weather { _temperature = 32.1 , _humidity = 50.5}

runPrompt = runStateT promptW weather0

