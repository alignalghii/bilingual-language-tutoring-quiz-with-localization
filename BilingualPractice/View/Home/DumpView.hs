{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Home.DumpView (dumpView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, backHomeLinkTextSnippet, askLinguisticalUnitSnippet, askDifficultyLevelSnippet)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.View.LanguageHelper (langLink)

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry (..))
import BilingualPractice.Model.ViewModel (view)
import Prelude hiding (head)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, span, form)
import Control.Monad (forM_)

dumpView :: Language -> [LexiconEntry] -> Html
dumpView language vocabularyData = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        title $ titleSnippet language
    body $ do
        h1 $ titleSnippet language
        p $
            langLink language "/" backHomeLinkTextSnippet
        table $ do
            tr $ do
                th $ view language Hu
                th $ view language En
                th $ askLinguisticalUnitSnippet language
                th $ askDifficultyLevelSnippet  language
            forM_ vocabularyData $ \ LxcE {en, hu, entity, difficulty} -> do
                tr $ do
                    td $ toHtml hu
                    td $ toHtml en
                    td $ view language entity
                    td $ view language difficulty


titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Showing the underlying complete lexicon"
titleSnippet Hu = appTitleSnippet Hu <> " — Lexikon teljes megmutatása"
