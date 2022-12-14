{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.PracticeController where

import BilingualPractice.Language (Language)
import Framework.Controller (blaze)
import BilingualPractice.Model.TableManipulationForBusinessLogic (preparePracticeControllingTables, readExtendedLexiconTable, closePracticeStart, deletePractice)
import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, entity, difficulty, qst1Time, restoreEtalonByAnswers, answersOfPracticeStart)
import BilingualPractice.Model.ViewModel (Viewable (view), viewPractice, conferAndViewCertificate)
import BilingualPractice.View.LanguageHelper (langRedirect')
import BilingualPractice.View.Practice.ExamenView        (examenView)
import BilingualPractice.View.Practice.IndexPracticeView (indexPracticeView)
import BilingualPractice.View.Practice.ShowPracticeView  (showPracticeView)
import BilingualPractice.View.Question.ResultView  (resultView) -- !! @TODO unorthodox
import Framework.Form (CheckList, makeCheckList, useCheckMatrixCNF)
import Framework.Url (Url)
-- import Database.SimpleHackDBMS.FileStorage (readTable)
import System.RandomX (randQuery)
import Data.Property (matchField)
import Web.Scotty (ActionM, param, params)
import Text.Blaze.Html5 (AttributeValue)
import Network.URI.Encode (decode) -- @TODO should come tom ViewModel
import Data.Text.Lazy (unpack)
import Data.ListX (preIntercalate)
import Data.String (fromString)
import Data.TimeX (keepDateAbbrevTime')
import Data.Time (getCurrentTimeZone)
import Data.Bool (bool)
import Control.Monad (liftM2)
import Control.Monad.Trans (liftIO)

import Database.Persist.Sqlite (selectList, entityVal) -- @TODO use database joins instead of Haskell-hacked list manipulation
import BilingualPractice.Model.EstablishBackendAndConnection (runConnection)


indexPracticeAction :: Language -> Url -> ActionM ()
indexPracticeAction lang selfUrl = do
    -- @TODO use database joins instead of Haskell-hacked list manipulation
    practices <- liftIO $ runConnection $ map entityVal <$> selectList [] [] -- readTable "practice"
    answers   <- liftIO $ runConnection $ map entityVal <$> selectList [] [] -- readTable "answer"
    timeZone  <- liftIO getCurrentTimeZone
    blaze $ indexPracticeView lang selfUrl $ viewPractice timeZone answers <$> practices

showPracticeAction :: Language -> Url -> ActionM ()
showPracticeAction lang selfUrl = do
    utc <- (read . decode . unpack) <$> param "utc"
    answers <- liftIO $ filter (matchField qst1Time utc) <$> runConnection (runConnection $ map entityVal <$> selectList [] []) -- readTable "answer"
    -- @TODO use database joins instead of Haskell-hacked list manipulation
    lexicon <- liftIO $ readExtendedLexiconTable
    timeZone <- liftIO getCurrentTimeZone
    blaze $ showPracticeView lang selfUrl utc (keepDateAbbrevTime' timeZone utc) $ conferAndViewCertificate lang timeZone lexicon answers

closePracticeAction :: Language -> ActionM ()
closePracticeAction lang = do
    redirectRoute <- fromString . preIntercalate "/" . map (unpack . snd) <$> params
    --let redirectRoute = mconcat ["/", redirectRoute1, "/", redirectRoute2]
    --    errorRoute    = "/error/navigationalinconsistency"
    liftIO closePracticeStart >>= langRedirect' lang . bool "/error/navigationinconsistency" redirectRoute
    -- liftIO $ print routes

deletePracticeAction :: Language -> ActionM ()
deletePracticeAction lang = do
   utc <- read <$> param "start"
   liftIO $ deletePractice utc
   langRedirect' lang "/practice/index"

repeatPracticeAction :: Language -> ActionM ()
repeatPracticeAction lang = do
    utc                <- read <$> param "start"
    (lexicon, answers) <- liftIO $ liftM2 (,) readExtendedLexiconTable $ runConnection $ map entityVal <$> selectList [] [] -- readTable "answer"
    flag               <- liftIO $ preparePracticeControllingTables $ restoreEtalonByAnswers (answersOfPracticeStart utc answers) lexicon
    langRedirect' lang $ bool "/error/navigationinconsistency" "/question" flag


proposeExamenAction :: Language -> Url -> ActionM ()
proposeExamenAction lang = blaze . examenView lang

performExamenAction :: Language -> ActionM ()
performExamenAction lang = do
    pars <- map (unpack . fst) <$> params
    numberOfQuestions <- read <$> param "number_of_questions" -- @TODO: form validation
    flag <- liftIO $ preparePracticeControllingTables =<< randQuery numberOfQuestions =<< filter (useCheckMatrixCNF lexiconEntryCheckMatrix pars) <$> readExtendedLexiconTable
    langRedirect' lang $ bool "/error/navigationinconsistency" "/question" flag

restartPracticeAction :: Language -> ActionM ()
restartPracticeAction lang = liftIO closePracticeStart >>= (langRedirect' lang . bool "/error/navigationinconsistency" "/practice/new")

-- lexiconEntryProps = selectorsPropertyCNF lexiconEntryProperties

lexiconEntryCheckMatrix :: [CheckList LexiconEntry]
lexiconEntryCheckMatrix = [makeCheckList entity, makeCheckList difficulty]
--[("number", matchField entity LUNumber), ("word", matchField entity LUWord), ("sentence", matchField entity LUSentence)]
--[("easy", matchField difficulty Easy), ("difficult", matchField difficulty Difficult)]
