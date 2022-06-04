{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.QuestionController where

import Framework.Controller (blaze)
import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, AnsweredQuestion (..), prcStartTime,
                                                        withFirstUnansweredQuestionIfAnyOrElse, conferPracticeCertificate)
import BilingualPractice.Model.TableManipulationForBusinessLogic (preparePracticeControllingTables, readPracticeControllingTables,
                                                                  fetchOpenPractice, closePractices, saveAnswers)
import BilingualPractice.Model.ViewModel (conferAndViewCertificate)
import BilingualPractice.View.Question.QuestionView (questionView) -- !!
import BilingualPractice.View.Question.ResultView   (resultView) -- !!
import Database.SimpleHackDBMS.FileStorage (insertIntoTable)
import Web.Scotty (ActionM, param, redirect)
import Control.Monad.Trans (liftIO)
import Data.Time (getCurrentTime, getCurrentTimeZone)


poseFirstRemainingExamenQuestionOrAnounceResultAction :: ActionM ()
poseFirstRemainingExamenQuestionOrAnounceResultAction = do
    (etalon, personal) <- liftIO readPracticeControllingTables
    let (ofAll, answd) = (length etalon, length personal)
        nth            = answd + 1
    withFirstUnansweredQuestionIfAnyOrElse (blaze . questionView nth ofAll) announceResult etalon personal

receiveAnswerForQuestion :: ActionM ()
receiveAnswerForQuestion = do
    ansHu <- param "hu"
    ansEn <- param "en"
    liftIO $ do
        qst1Time <- prcStartTime <$> fetchOpenPractice
        ansTime  <- getCurrentTime
        insertIntoTable "personal" AnsQu {ansHu, ansEn, qst1Time, ansTime}
    redirect "/question"

announceResult :: [LexiconEntry] -> [AnsweredQuestion] -> ActionM ()
announceResult etalon personal = do
    liftIO $ closePractices >> saveAnswers personal
    timeZone <- liftIO getCurrentTimeZone
    blaze $ resultView $ conferAndViewCertificate timeZone etalon personal
