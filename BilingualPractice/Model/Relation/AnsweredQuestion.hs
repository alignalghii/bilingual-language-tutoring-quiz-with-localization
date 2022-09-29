{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, GADTs, GeneralizedNewtypeDeriving #-}

module BilingualPractice.Model.Relation.AnsweredQuestion where

import BilingualPractice.Model.Relation.AnsweredQuestionEntityDefs (answeredQuestionEntityDefs)
import Database.Persist.TH (mkPersist, sqlSettings)

import Data.Time (UTCTime)


-- data AnsweredQuestion = AnsQu {ansHu, ansEn :: String, qst1Time, ansTime :: UTCTime} deriving (Read, Show) -- Eq


mkPersist sqlSettings answeredQuestionEntityDefs


ansHu, ansEn :: AnsweredQuestion -> String
ansHu = answeredQuestionAnsHu
ansEn = answeredQuestionAnsEn

qst1Time, ansTime :: AnsweredQuestion -> UTCTime
qst1Time = answeredQuestionQst1Time
ansTime  = answeredQuestionAnsTime


withFieldsOfAnsweredQuestion :: (String -> String -> UTCTime -> UTCTime -> val) -> AnsweredQuestion -> val
withFieldsOfAnsweredQuestion f (AnsweredQuestion hu en entity difficulty) = f hu en entity difficulty

answeredQuestionAsTuple :: AnsweredQuestion -> (String, String, UTCTime, UTCTime)
answeredQuestionAsTuple = withFieldsOfAnsweredQuestion (,,,)
