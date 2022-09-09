{-# LANGUAGE OverloadedStrings #-}

module Framework.Helper where

import Framework.Form (FormParamable (formParam))
import Data.String (IsString)
import Data.List (intersperse)


parametrizeUrl :: (IsString string, Monoid string, FormParamable value) => [(string, value)] -> string -> string
parametrizeUrl paramNameValuePairs url = url <> "?" <> writeAssignments paramNameValuePairs

writeAssignments  :: (IsString string, Monoid string, FormParamable value) => [(string, value)] -> string
writeAssignments = mconcat . intersperse "&" . map (uncurry writeAssignment)

writeAssignment :: (IsString string, Semigroup string, FormParamable value) => string -> value -> string
writeAssignment name value = name <> "=" <> formParam value
