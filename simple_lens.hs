{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

data Weather = Weather {
 _temperature :: Float
, _humidity :: Float
} deriving (Show)

makeLenses ''Weather

weather0 :: Weather
weather0 = Weather { _temperature = 32.1 , _humidity = 50.5 }

promptW :: StateT Weather IO ()
promptW = do
 lift $ putStrLn "input current temp"
 t <- lift $ (readLn :: IO Float)
 temperature .= t

promptWL :: StateT Weather IO ()
promptWL = do
 lift $ putStrLn "input current temp"
 t <- lift $ (readLn :: IO Float)
 w <- get
 lift $ putStrLn ("current temp " ++ (show (w ^. temperature)))
 temperature  .= t
 w <- get -- have to get the new state, otherwise w will refer to the old one
 lift $ putStrLn ("new temp " ++ (show (w ^. temperature)))

main = runState promptWL weather0




