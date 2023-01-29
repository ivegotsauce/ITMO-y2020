module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Set (fromList)
import HW3.Action
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      line <- getInputLine "hi> "
      case line of
        Nothing -> return ()
        Just ":q"  -> return ()
        Just input -> do
          let parsed = parse input
          case parsed of
            Left failure -> outputStrLn $ show failure
            Right expr -> do
              evaled <- liftIO $ runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              case evaled of
                Left  e -> outputStrLn $ show e
                Right v -> outputStrLn $ show $ prettyValue v
          loop
