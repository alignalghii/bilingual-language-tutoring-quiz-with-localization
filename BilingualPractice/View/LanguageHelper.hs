{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.LanguageHelper where

import BilingualPractice.Language (Language)
import BilingualPractice.Model.ViewModel () -- instance FormParamable Language
import Framework.Form (formParam)
import Framework.Helper (parametrizeUrl)
import Framework.Url (Url, addParam, QueryAssignment (QA))
import Data.Representation (represent)


import Web.Scotty (ActionM, redirect)
import Text.Blaze.Html5 (Html, Attribute, AttributeValue, (!), a)
import Text.Blaze.Html5.Attributes (href, action)

import Data.String (IsString)
import Data.Text.Lazy (Text)


langUrl :: (IsString url, Monoid url) => Language -> url -> url
langUrl lang = parametrizeUrl [("lang", lang)]

langLink :: Language -> AttributeValue -> (Language -> Html) -> Html
langLink language url snippet = a ! href (langUrl language url) $ snippet language

langAction :: Language -> AttributeValue -> Attribute
langAction lang = action . langUrl lang

langRedirect :: Language -> Text -> ActionM a
langRedirect language = redirect . langUrl language


langUrl' :: Language -> Url -> Url
langUrl' lang url = addParam url $ QA "lang" (formParam lang)

langLink' :: Language -> Url -> (Language -> Html) -> Html
langLink' language url snippet = a ! href (represent $ langUrl' language url) $ snippet language

langAction' :: Language -> Url -> Attribute
langAction' lang = action . represent . langUrl' lang

langRedirect' :: Language -> Url -> ActionM a
langRedirect' language = redirect . represent . langUrl' language
