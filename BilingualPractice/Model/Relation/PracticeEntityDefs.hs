{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module BilingualPractice.Model.Relation.PracticeEntityDefs (practiceEntityDefs) where

import Database.Persist (EntityDef)
import Database.Persist.TH (persistLowerCase, derivePersistField)

import Data.Time (UTCTime)

-- -- data Practice = Prc {prcStartTime :: UTCTime, isOpen :: Bool} deriving (Read, Show)

practiceEntityDefs :: [EntityDef]
practiceEntityDefs = [persistLowerCase|
Practice
    prcStartTime UTCTime
    isOpen Bool
    deriving Read Show
|]
