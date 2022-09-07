{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Helper where

import BilingualPractice.Language (Language)
import BilingualPractice.Model.ViewModel () -- instance FormParamable Language
import Framework.Helper (parametrizeUrl)

import Text.Blaze.Html5 (Html, AttributeValue, (!), a)
import Text.Blaze.Html5.Attributes (href)

import Data.String (IsString)

langUrl :: (IsString url, Monoid url) => Language -> url -> url
langUrl lang = parametrizeUrl [("lang", lang)]

langLink :: Language -> AttributeValue -> (Language -> Html) -> Html
langLink language url snippet = a ! href (langUrl language url) $ snippet language
