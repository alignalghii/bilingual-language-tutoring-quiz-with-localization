module Database.SimpleHackDBMS.FileStorage where

import System.IO.Strict (readFile)
import Prelude hiding (readFile)
import Data.Property (PropertyPredicate, propNot)
import Data.ListX (insertAfter)
import Control.Monad (void)


type TableName = String

allocate :: TableName -> FilePath
allocate tableName = "var/" ++ tableName ++ ".table"


writeTable :: Show record => TableName -> [record] -> IO ()
writeTable tableName = writeFile (allocate tableName) . show

writeTable_typDed :: Show record => TableName -> [record] -> IO [record]
writeTable_typDed tableName records = writeTable tableName records >> return records

readTable :: Read record => TableName -> IO [record]
readTable tableName = read <$> readFile (allocate tableName)


truncateTable :: Show record => TableName -> IO [record]
truncateTable tableName = writeTable_typDed tableName []

modifyTable :: (Read record, Show record) => TableName -> ([record] -> [record]) -> IO [record]
modifyTable tableName f = do
    records <- readTable tableName
    writeTable tableName $ f records
    return records

insertIntoTable :: (Read record, Show record) => TableName -> record -> IO ()
insertIntoTable tableName = void . modifyTable tableName . flip insertAfter

updateTable :: (Read record, Show record) => TableName -> (record -> record) -> IO [record]
updateTable tableName = modifyTable tableName . map

selectFromTable, deleteFromTable :: (Read record, Show record) => TableName -> PropertyPredicate record -> IO [record]
selectFromTable tableName = modifyTable tableName . filter
deleteFromTable tableName = selectFromTable tableName . propNot
