{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.PracticeController where

import BilingualPractice.Language (Language)
import Framework.Controller (blaze)
import BilingualPractice.Model.TableManipulationForBusinessLogic (preparePracticeControllingTables, readExtendedLexiconTable, closePracticeStart, deletePractice)
import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, entity, difficulty, qst1Time, restoreEtalonByAnswers, answersOfPracticeStart)
import BilingualPractice.Model.ViewModel (Viewable (view), viewPractice, conferAndViewCertificate)
import BilingualPractice.View.Practice.ExamenView        (examenView)
import BilingualPractice.View.Practice.IndexPracticeView (indexPracticeView)
import BilingualPractice.View.Practice.ShowPracticeView  (showPracticeView)
import BilingualPractice.View.Question.ResultView  (resultView) -- !! @TODO unorthodox
import Framework.Form (CheckList, makeCheckList, useCheckMatrixCNF)
import Database.SimpleHackDBMS.FileStorage (readTable)
import System.RandomX (randQuery)
import Data.Property (matchField)
import Web.Scotty (ActionM, param, params, redirect)
import Network.URI.Encode (decode) -- @TODO should come tom ViewModel
import Data.Text.Lazy (unpack, intercalate)
import Data.TimeX (keepDateAbbrevTime')
import Data.Time (getCurrentTimeZone)
import Data.Bool (bool)
import Control.Monad (liftM2)
import Control.Monad.Trans (liftIO)


indexPracticeAction :: Language -> ActionM ()
indexPracticeAction _ = do
    practices <- liftIO $ readTable "practice"
    answers   <- liftIO $ readTable "answer"
    timeZone  <- liftIO getCurrentTimeZone
    blaze $ indexPracticeView $ viewPractice timeZone answers <$> practices

showPracticeAction :: Language -> ActionM ()
showPracticeAction lang = do
    utc <- (read . decode . unpack) <$> param "utc"
    answers <- liftIO $ filter (matchField qst1Time utc) <$> readTable "answer"
    lexicon <- liftIO $ readExtendedLexiconTable
    timeZone <- liftIO getCurrentTimeZone
    blaze $ showPracticeView utc (keepDateAbbrevTime' timeZone utc) $ conferAndViewCertificate lang timeZone lexicon answers

closePracticeAction :: ActionM ()
closePracticeAction = do
    redirectRoute <- ("/" <>) . intercalate "/" . map snd <$> params
    --let redirectRoute = mconcat ["/", redirectRoute1, "/", redirectRoute2]
    --    errorRoute    = "/error/navigationalinconsistency"
    liftIO closePracticeStart >>= redirect . bool "/error/navigationinconsistency" redirectRoute
    -- liftIO $ print routes

deletePracticeAction :: ActionM ()
deletePracticeAction = do
   utc <- read <$> param "start"
   liftIO $ deletePractice utc
   redirect "/practice/index"

repeatPracticeAction :: ActionM ()
repeatPracticeAction = do
    utc                <- read <$> param "start"
    (lexicon, answers) <- liftIO $ liftM2 (,) readExtendedLexiconTable (readTable "answer")
    flag               <- liftIO $ preparePracticeControllingTables $ restoreEtalonByAnswers (answersOfPracticeStart utc answers) lexicon
    redirect $ bool "/error/navigationinconsistency" "/question" flag


proposeExamenAction :: Language -> ActionM ()
proposeExamenAction = blaze . examenView

performExamenAction :: ActionM ()
performExamenAction = do
    pars <- map (unpack . fst) <$> params
    numberOfQuestions <- read <$> param "number_of_questions" -- @TODO: form validation
    flag <- liftIO $ preparePracticeControllingTables =<< randQuery numberOfQuestions =<< filter (useCheckMatrixCNF lexiconEntryCheckMatrix pars) <$> readExtendedLexiconTable
    redirect $ bool "/error/navigationinconsistency" "/question" flag

restartPracticeAction :: ActionM ()
restartPracticeAction = liftIO closePracticeStart >>= (redirect . bool "/error/navigationinconsistency" "/practice/new")

-- lexiconEntryProps = selectorsPropertyCNF lexiconEntryProperties

lexiconEntryCheckMatrix :: [CheckList LexiconEntry]
lexiconEntryCheckMatrix = [makeCheckList entity, makeCheckList difficulty]
--[("number", matchField entity LUNumber), ("word", matchField entity LUWord), ("sentence", matchField entity LUSentence)]
--[("easy", matchField difficulty Easy), ("difficult", matchField difficulty Difficult)]
