{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, GADTs, GeneralizedNewtypeDeriving #-}

module BilingualPractice.Model.Relation.Practice where

import BilingualPractice.Model.Relation.PracticeEntityDefs (practiceEntityDefs)
import Database.Persist.TH (mkPersist, sqlSettings)

import Data.Time (UTCTime)


-- data Practice = Prc {prcStartTime :: UTCTime, isOpen :: Bool} deriving (Read, Show)

mkPersist sqlSettings practiceEntityDefs

prcStartTime :: Practice -> UTCTime
prcStartTime = practicePrcStartTime

isOpen :: Practice -> Bool
isOpen = practiceIsOpen

withFieldsOfPractice :: (UTCTime -> Bool -> val) -> Practice -> val
withFieldsOfPractice f (Practice prcStartTime isOpen) = f prcStartTime isOpen

practiceAsTuple :: Practice -> (UTCTime, Bool)
practiceAsTuple = withFieldsOfPractice (,)
