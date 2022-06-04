{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.PracticeController where

import Framework.Controller (blaze)
import BilingualPractice.Model.TableManipulationForBusinessLogic (preparePracticeControllingTables, readExtendedLexiconTable)
import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, entity, difficulty, qst1Time)
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
import Data.Text.Lazy (unpack)
import Data.TimeX (keepDateAbbrevTime')
import Data.Time (getCurrentTimeZone)
import Control.Monad.Trans (liftIO)


indexPracticeAction :: ActionM ()
indexPracticeAction = do
    practices <- liftIO $ readTable "practice"
    answers   <- liftIO $ readTable "answer"
    timeZone  <- liftIO getCurrentTimeZone
    blaze $ indexPracticeView $ viewPractice timeZone answers <$> practices

showPracticeAction :: ActionM ()
showPracticeAction = do
    utc <- (read . decode . unpack) <$> param "utc"
    answers <- liftIO $ filter (matchField qst1Time utc) <$> readTable "answer"
    lexicon <- liftIO $ readExtendedLexiconTable
    timeZone <- liftIO getCurrentTimeZone
    blaze $ showPracticeView (keepDateAbbrevTime' timeZone utc) $ conferAndViewCertificate timeZone lexicon answers

proposeExamenAction :: ActionM ()
proposeExamenAction = blaze examenView

performExamenAction :: ActionM ()
performExamenAction = do
    pars <- map (unpack . fst) <$> params
    numberOfQuestions <- read <$> param "number_of_questions" -- @TODO: form validation
    liftIO $ preparePracticeControllingTables =<< randQuery numberOfQuestions =<< filter (useCheckMatrixCNF lexiconEntryCheckMatrix pars) <$> readExtendedLexiconTable
    redirect "/question"

-- lexiconEntryProps = selectorsPropertyCNF lexiconEntryProperties

lexiconEntryCheckMatrix :: [CheckList LexiconEntry]
lexiconEntryCheckMatrix = [makeCheckList entity, makeCheckList difficulty]
--[("number", matchField entity LUNumber), ("word", matchField entity LUWord), ("sentence", matchField entity LUSentence)]
--[("easy", matchField difficulty Easy), ("difficult", matchField difficulty Difficult)]
