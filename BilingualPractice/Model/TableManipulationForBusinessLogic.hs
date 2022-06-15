{-# LANGUAGE NamedFieldPuns #-}

module BilingualPractice.Model.TableManipulationForBusinessLogic where

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, AnsweredQuestion (qst1Time), numeralsRelation, Practice (..), Session (..))
import Database.SimpleHackDBMS.FileStorage (readTable, writeTable, truncateTable, insertIntoTable, appendToTable, updateTable, deleteFromTable)
import Data.Persistence (writeData, writeData_typDed, readData, modifyData, modifyData_typDed)
import Data.Property (matchField)
import Data.ListX (filterIt_unsafe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (isJust, fromJust)
import Control.Monad (void)


readLexiconTable, readExtendedLexiconTable :: IO [LexiconEntry]
readLexiconTable         = readTable "lexicon"
readExtendedLexiconTable = (++ numeralsRelation) <$> readLexiconTable

preparePracticeControllingTables :: [LexiconEntry] -> IO Bool
preparePracticeControllingTables etalon = do
    -- insertIntoTable "practice" Prc {prcStartTime, isOpen = True}
    -- putSession maybePracticeStart $ Just prcStartTime
    eitherZombieOrVirginPracticeStart <- openPracticeStart
    case eitherZombieOrVirginPracticeStart of
        Right _ -> do
            modifySession $ \s -> s {etalon = etalon}
            modifySession $ \s -> s {personal = []}
            return True
        Left _ -> return False

readPracticeControllingTables :: IO ([LexiconEntry], [AnsweredQuestion])
readPracticeControllingTables = do
    etalon   <- getSession etalon
    personal <- getSession personal
    return (etalon, personal)

-- fetchOpenPractice :: IO Practice
-- fetchOpenPractice = filterIt_unsafe isOpen <$> readTable "practice"

openPracticeStart :: IO (Either UTCTime UTCTime)
openPracticeStart = do
    maybePracticeStart <- getSession maybePracticeStart
    case maybePracticeStart of
        Nothing -> do
            virginPracticeStart <- getCurrentTime
            modifySession $ \s -> s {maybePracticeStart = Just virginPracticeStart}
            return $ Right virginPracticeStart
        Just zombiePracticeStart ->  return $ Left zombiePracticeStart

getPracticeStart_unsafe :: IO UTCTime
getPracticeStart_unsafe = fromJust <$> getSession maybePracticeStart

checkOpenPracticeStart :: IO Bool
checkOpenPracticeStart = isJust <$> getSession maybePracticeStart


closePracticeStart :: IO Bool  -- it should make differentiable the exception session not being open
closePracticeStart = do
    mbStart <- getSession maybePracticeStart
    modifySession $ \s -> s {maybePracticeStart = Nothing}
    return $ isJust mbStart

insertAsNewPractice :: UTCTime -> IO ()
insertAsNewPractice prcStartTime = insertIntoTable "practice" Prc {prcStartTime, isOpen = True}

saveAnswers :: [AnsweredQuestion] -> IO [AnsweredQuestion]
saveAnswers = appendToTable "answer"

deletePractice :: UTCTime -> IO ()
deletePractice utc = void $ do
    deleteFromTable "practice" $ matchField prcStartTime utc
    deleteFromTable "answer"   $ matchField qst1Time     utc


readSession :: IO Session
readSession = readData sessionFile

writeSession :: Session -> IO ()
writeSession = writeData sessionFile

writeSession_typDed :: Session -> IO Session
writeSession_typDed = writeData_typDed sessionFile

modifySession :: (Session -> Session) -> IO ()
modifySession = modifyData sessionFile

modifySession_typDed :: (Session -> Session) -> IO Session
modifySession_typDed = modifyData_typDed sessionFile

getSession :: (Session -> a) -> IO a
getSession = flip fmap readSession

-- putSession :: (Session -> a) -> IO ()
-- putSession selector = ???

sessionFile :: FilePath
sessionFile = "var/session.data"
