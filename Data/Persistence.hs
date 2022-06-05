module Data.Persistence where

import System.IO.Strict (readFile)
import Prelude hiding (readFile)


writeData :: Show dat => FilePath -> dat -> IO ()
writeData fileName = writeFile fileName . show

writeData_typDed :: Show dat => FilePath -> dat -> IO dat
writeData_typDed fileName dat = writeData fileName dat >> return dat

readData :: Read dat => FilePath -> IO dat
readData = fmap read . readFile

modifyData :: (Read dat, Show dat) => FilePath -> (dat -> dat) -> IO ()
modifyData fileName f = readData fileName >>= (writeData fileName . f)

modifyData_typDed :: (Read dat, Show dat) => FilePath -> (dat -> dat) -> IO dat
modifyData_typDed fileName f = do
    dat <- readData fileName
    writeData fileName $ f dat
    return dat
