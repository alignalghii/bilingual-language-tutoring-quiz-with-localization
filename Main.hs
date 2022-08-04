module Main (main) where

import BilingualPractice.Router (router)
import Web.Scotty (scotty)
import System.Environment (getArgs)

main :: IO ()
main = do
    logFlag <- (not . null) <$> getArgs
    scotty 3001 $ router logFlag
