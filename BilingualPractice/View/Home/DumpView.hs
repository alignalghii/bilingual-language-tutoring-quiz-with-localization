{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Home.DumpView (dumpView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, backHomeLinkTextSnippet, hungarianLanguageNameSnippet, englishLanguageNameSnippet, linguisticalUnitNameSnippet, difficultLevelNameSnippet)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

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
            a ! href "/" $ backHomeLinkTextSnippet language
        table $ do
            tr $ do
                th $ hungarianLanguageNameSnippet language
                th $ englishLanguageNameSnippet   language
                th $ linguisticalUnitNameSnippet  language
                th $ difficultLevelNameSnippet    language
            forM_ vocabularyData $ \ LxcE {en, hu, entity, difficulty} -> do
                tr $ do
                    td $ toHtml hu
                    td $ toHtml en
                    td $ toHtml $ view entity
                    td $ toHtml $ view difficulty


titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Showing the underlying complete lexicon"
titleSnippet Hu = appTitleSnippet Hu <> " — Lexikon teljes megmutatása"
