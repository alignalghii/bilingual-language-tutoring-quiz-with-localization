{-# LANGUAGE LambdaCase #-}

module Main (main) where

import BilingualPractice.Router (router)
import Web.Scotty (scotty)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt (OptDescr (Option), ArgDescr (..), getOpt, ArgOrder (Permute), usageInfo)
import Data.Maybe (fromMaybe)
import Data.List.Extra (firstJust)


main :: IO ()
main = do
    progName <- getProgName
    argv <- getArgs
    case getOpt Permute options argv of
        (flags, []     , []    ) -> serveWith progName flags
        (_    , nonOpts, errors) -> report    progName nonOpts errors

report :: String -> [String] -> [String] -> IO ()
report progName nonOpts errors = do
    putStrLn "Incorrect usage"
    if null errors
        then do
            putStrLn "There should be no nonopts, thus don't provide such:"
            mapM_ (putStrLn . (" - " ++)) nonOpts
        else do
            putStrLn "There are errors:"
            mapM_ (putStrLn . (" - " ++)) errors
    putStrLn "Correct usage:"
    putStrLn $ usageInfo (progName ++ " <OPTIONS>") options

serveWith :: String -> [Flag] -> IO ()
serveWith progName flags = do
    if Help `elem` flags
        then putStrLn $ usageInfo (progName ++ " <OPTIONS>") options
        else do
            let portNumber = fromMaybe 3000 $ firstJust (\case {Port n -> Just n; _ -> Nothing}) flags
            let logFlag    = Log `elem` flags
            scotty portNumber $ router logFlag

data Flag = Port Int | Lang String | Log | Help deriving Eq

options :: [OptDescr Flag]
options = [  Option "p"  ["port"    ] (ReqArg (Port . read) "<PORT-NUMBER>") "Provide port number <PORT-NUMBER> the server should listen on",
             Option "n"  ["language"] (ReqArg Lang          "<LANGUAGE>"   ) "Provide which language the explanatory texts & widget labels should be",
             Option "g"  ["log"     ] (NoArg  Log                          ) "Determine whether the server should print logging messages",
             Option "h?" ["help"    ] (NoArg  Help                         ) "Help"
          ]
