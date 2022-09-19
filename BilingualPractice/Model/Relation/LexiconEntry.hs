{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, GADTs, GeneralizedNewtypeDeriving #-}

module BilingualPractice.Model.Relation.LexiconEntry (module BilingualPractice.Model.Relation.LexiconEntry, LinguisticalUnit (..), Difficulty (..)) where

import BilingualPractice.Model.Relation.LexiconEntryEntityDefs (lexiconEntryEntityDefs, LinguisticalUnit (..), Difficulty (..))
import Database.Persist.TH (mkPersist, sqlSettings)

mkPersist sqlSettings lexiconEntryEntityDefs


hu, en :: LexiconEntry -> String
hu = lexiconEntryHu
en = lexiconEntryEn

entity :: LexiconEntry -> LinguisticalUnit
entity = lexiconEntryEntity

difficulty :: LexiconEntry -> Difficulty
difficulty = lexiconEntryDifficulty

-- data LexiconEntry' = LxcE {hu', en' :: String, entity' :: LinguisticalUnit, difficulty' :: Difficulty} deriving (Read, Show)

withFieldsOfLexiconEntry :: (String -> String -> LinguisticalUnit -> Difficulty -> val) -> LexiconEntry -> val
withFieldsOfLexiconEntry f (LexiconEntry hu en entity difficulty) = f hu en entity difficulty

lexiconEntryAsTuple :: LexiconEntry -> (String, String, LinguisticalUnit, Difficulty)
lexiconEntryAsTuple = withFieldsOfLexiconEntry (,,,)
