import Control.Monad.Reader

simple :: Reader String String
simple =  do 
  x <- ask
  return ("you said " ++ x)

runSimple = runReader simple "hi!"

simple2 :: Reader Int Int
simple2 = do
  x <- ask
  return (x * 2)

runSimple2 = runReader simple2 3

printReaderContent :: ReaderT String IO ()
printReaderContent = do
  content  <- ask
  liftIO $ putStrLn ("reader says " ++ content)

main = do
  runReaderT printReaderContent "Some Content"
