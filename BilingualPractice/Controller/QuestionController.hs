{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.QuestionController where

import Framework.Controller (blaze)
import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, AnsweredQuestion (..), prcStartTime,
                                                        withFirstUnansweredQuestionIfAnyOrElse, conferPracticeCertificate,
                                                        maybePracticeStart)
import BilingualPractice.Model.TableManipulationForBusinessLogic (preparePracticeControllingTables, readPracticeControllingTables,
                                                                  getSession, getPracticeStart_unsafe, checkOpenPracticeStart, closePracticeStart, insertAsNewPractice, saveAnswers)
import BilingualPractice.Model.ViewModel (conferAndViewCertificate)
import BilingualPractice.View.Question.QuestionView (questionView) -- !!
import BilingualPractice.View.Question.ResultView   (resultView) -- !!
import Database.SimpleHackDBMS.FileStorage (insertIntoTable)
import Web.Scotty (ActionM, param, redirect)
import Control.Monad.Trans (liftIO)
import Data.Time (getCurrentTime, getCurrentTimeZone)


poseFirstRemainingExamenQuestionOrAnounceResultAction :: ActionM ()
poseFirstRemainingExamenQuestionOrAnounceResultAction = do
    flag <- liftIO checkOpenPracticeStart
    if flag
        then do
            (etalon, personal) <- liftIO readPracticeControllingTables
            let (ofAll, answd) = (length etalon, length personal)
                nth            = answd + 1
            withFirstUnansweredQuestionIfAnyOrElse (blaze . questionView nth ofAll) announceResult etalon personal
        else redirect "/error/navigationinconsistency"

receiveAnswerForQuestion :: ActionM ()
receiveAnswerForQuestion = do
    ansHu <- param "hu"
    ansEn <- param "en"
    prcStartTime <- liftIO getPracticeStart_unsafe
    liftIO $ do
        ansTime  <- getCurrentTime
        insertIntoTable "personal" AnsQu {ansHu, ansEn, qst1Time = prcStartTime, ansTime}
    redirect "/question"

announceResult :: [LexiconEntry] -> [AnsweredQuestion] -> ActionM ()
announceResult etalon personal = do
    let lexicon = etalon -- lexicon <- liftIO readExtendedLexiconTable
    case personal of
        AnsQu {qst1Time = prcStartTime} : _ -> do
            timeZone <- liftIO $ do
                insertAsNewPractice prcStartTime
                saveAnswers personal
                closePracticeStart
                getCurrentTimeZone
            blaze $ resultView $ conferAndViewCertificate timeZone lexicon personal
        [] -> do
            liftIO closePracticeStart
            redirect "/error/emptydata"
