{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module BilingualPractice.Model.Relation.LexiconEntryEntityDefs (lexiconEntryEntityDefs, LinguisticalUnit (..), Difficulty (..)) where

import Database.Persist (EntityDef)
import Database.Persist.TH (persistLowerCase, derivePersistField)


data LinguisticalUnit = LUNumber | LUWord | LUPhrase | LUSentence deriving (Eq, Read, Show, Bounded, Enum)
derivePersistField "LinguisticalUnit"

data Difficulty = Easy | MiddleLevel | Difficult deriving (Eq, Read, Show, Bounded, Enum)
derivePersistField "Difficulty"

lexiconEntryEntityDefs :: [EntityDef]
lexiconEntryEntityDefs = [persistLowerCase|
LexiconEntry
    hu String
    en String
    entity LinguisticalUnit
    difficulty Difficulty
    deriving Read Show
|]
