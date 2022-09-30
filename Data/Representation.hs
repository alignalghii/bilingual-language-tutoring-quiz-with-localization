{-# LANGUAGE OverloadedStrings #-}

module Data.Representation where

import Data.String (IsString (fromString))


represent :: (Show value, IsString repr) => value -> repr
represent = fromString . show
