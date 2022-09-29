{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module BilingualPractice.Model.Relation.AnsweredQuestionEntityDefs (answeredQuestionEntityDefs) where

import Database.Persist (EntityDef)
import Database.Persist.TH (persistLowerCase, derivePersistField)

import Data.Time (UTCTime)

-- data AnsweredQuestion = AnsQu {ansHu, ansEn :: String, qst1Time, ansTime :: UTCTime} deriving (Read, Show) -- Eq


answeredQuestionEntityDefs :: [EntityDef]
answeredQuestionEntityDefs = [persistLowerCase|
AnsweredQuestion
    ansHu String
    ansEn String
    qst1Time UTCTime
    ansTime UTCTime
    deriving Read Show
|]
