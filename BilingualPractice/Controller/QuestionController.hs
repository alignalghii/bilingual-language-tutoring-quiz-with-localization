{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.QuestionController where

import BilingualPractice.Language (Language)
import Framework.Controller (blaze)
import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, AnsweredQuestion (..), prcStartTime,
                                                        withFirstUnansweredQuestionIfAnyOrElse, conferPracticeCertificate,
                                                        maybePracticeStart, Session (personal))
import BilingualPractice.Model.TableManipulationForBusinessLogic (preparePracticeControllingTables, readPracticeControllingTables,
                                                                  getSession, getPracticeStart_unsafe, checkOpenPracticeStart, closePracticeStart, insertAsNewPractice, saveAnswers, modifySession)
import BilingualPractice.Model.ViewModel (conferAndViewCertificate)
import BilingualPractice.View.LanguageHelper (langRedirect)
import BilingualPractice.View.Question.QuestionView (questionView) -- !!
import BilingualPractice.View.Question.ResultView   (resultView) -- !!
import Database.SimpleHackDBMS.FileStorage (insertIntoTable)
import Web.Scotty (ActionM, param, redirect)
import Text.Blaze.Html5 (AttributeValue)
import Control.Monad.Trans (liftIO)
import Data.Time (getCurrentTime, getCurrentTimeZone)


poseFirstRemainingExamenQuestionOrAnounceResultAction :: Language -> AttributeValue -> ActionM ()
poseFirstRemainingExamenQuestionOrAnounceResultAction lang selfUrl = do
    flag <- liftIO checkOpenPracticeStart
    if flag
        then do
            (etalon, personal) <- liftIO readPracticeControllingTables
            let (ofAll, answd) = (length etalon, length personal)
                nth            = answd + 1
            withFirstUnansweredQuestionIfAnyOrElse (blaze . questionView lang selfUrl nth ofAll) (announceResult lang selfUrl) etalon personal
        else redirect "/error/navigationinconsistency"

receiveAnswerForQuestion :: Language -> ActionM ()
receiveAnswerForQuestion lang = do
    ansHu <- param "hu"
    ansEn <- param "en"
    prcStartTime <- liftIO getPracticeStart_unsafe
    liftIO $ do
        ansTime  <- getCurrentTime
        modifySession $ \s -> s {personal = personal s ++ [AnsQu {ansHu, ansEn, qst1Time = prcStartTime, ansTime}]}
    langRedirect lang "/question"

announceResult :: Language -> AttributeValue -> [LexiconEntry] -> [AnsweredQuestion] -> ActionM ()
announceResult lang selfUrl etalon personal = do
    let lexicon = etalon -- lexicon <- liftIO readExtendedLexiconTable
    case personal of
        AnsQu {qst1Time = prcStartTime} : _ -> do
            timeZone <- liftIO $ do
                insertAsNewPractice prcStartTime
                saveAnswers personal
                closePracticeStart
                getCurrentTimeZone
            blaze $ resultView lang selfUrl prcStartTime $ conferAndViewCertificate lang timeZone lexicon personal
        [] -> do
            liftIO closePracticeStart
            redirect "/error/emptydata"
