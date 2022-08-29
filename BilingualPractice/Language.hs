{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.Language where

import Data.String (IsString)


data Language = En | Hu deriving (Eq, Read, Show)

languageAttrValue :: IsString string => Language -> string
languageAttrValue En = "en"
languageAttrValue Hu = "hu"
