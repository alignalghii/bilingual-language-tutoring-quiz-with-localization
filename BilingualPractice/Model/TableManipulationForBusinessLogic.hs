{-# LANGUAGE NamedFieldPuns #-}

module BilingualPractice.Model.TableManipulationForBusinessLogic where

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, AnsweredQuestion (qst1Time), numeralsRelation, Practice (..))
import Database.SimpleHackDBMS.FileStorage (readTable, writeTable, truncateTable, insertIntoTable, updateTable, deleteFromTable)
import Data.Property (matchField)
import Data.ListX (filterIt_unsafe)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad (void)


readLexiconTable, readExtendedLexiconTable :: IO [LexiconEntry]
readLexiconTable         = readTable "lexicon"
readExtendedLexiconTable = (++ numeralsRelation) <$> readLexiconTable

preparePracticeControllingTables :: [LexiconEntry] -> IO [AnsweredQuestion] -- return type enables type deduction for truncateTable
preparePracticeControllingTables etalon = do
    prcStartTime <- getCurrentTime
    insertIntoTable "practice" Prc {prcStartTime, isOpen = True}
    writeTable "etalon" etalon
    truncateTable "personal" -- to help type deduction, we return with type [AnsweredQuestion] explicitly

readPracticeControllingTables :: IO ([LexiconEntry], [AnsweredQuestion])
readPracticeControllingTables = do
    etalon   <- readTable "etalon"
    personal <- readTable "personal"
    return (etalon, personal)

fetchOpenPractice :: IO Practice
fetchOpenPractice = filterIt_unsafe isOpen <$> readTable "practice"

closePractices :: IO ()
closePractices = void $ do
    updateTable "practice" $ \prc -> prc {isOpen = False}

saveAnswers :: [AnsweredQuestion] -> IO ()
saveAnswers = mapM_ $ insertIntoTable "answer"

deletePractice :: UTCTime -> IO ()
deletePractice utc = void $ do
    deleteFromTable "practice" $ matchField prcStartTime utc
    deleteFromTable "answer"   $ matchField qst1Time     utc
