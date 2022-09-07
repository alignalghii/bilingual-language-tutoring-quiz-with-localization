{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.Language where

import Data.String (IsString)


data Language = En | Hu deriving (Eq, Read, Show, Enum, Ord, Bounded)

languageAttrValue :: IsString string => Language -> string
languageAttrValue En = "en"
languageAttrValue Hu = "hu"
