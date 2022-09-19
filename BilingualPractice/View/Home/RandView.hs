{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Home.RandView (randView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, languageSelectionFlagBarSnippet', backHomeLinkTextSnippet, askLinguisticalUnitSnippet, askDifficultyLevelSnippet)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.View.LanguageHelper (langLink')

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, lexiconEntryAsTuple)
import BilingualPractice.Model.ViewModel (view)
import Framework.Url (Url)
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Control.Monad (forM_)

randView :: Language -> Url -> [LexiconEntry] -> Html
randView language selfUrl records = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        title $ titleSnippet language
    body $ do
        languageSelectionFlagBarSnippet' language selfUrl
        h1 $ titleSnippet language
        p $ do
            langLink' language "/rand" resamplingLinkTextSnippet
            span " •|||• "
            langLink' language "/" backHomeLinkTextSnippet
        table $ do
            tr $ do
                th $ view language Hu
                th $ view language En
                th $ askLinguisticalUnitSnippet language
                th $ askDifficultyLevelSnippet  language
            forM_ (lexiconEntryAsTuple <$> records) $ \(en, hu, entity, difficulty) -> do
                tr $ do
                    td $ toHtml hu
                    td $ toHtml en
                    td $ view language entity
                    td $ view language difficulty


titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Lexicon random sampling"
titleSnippet Hu = appTitleSnippet Hu <> " — Lexikon véletlen mintakiválasztása"

resamplingLinkTextSnippet :: IsString string => Language -> string
resamplingLinkTextSnippet En = "Re-sampling again randomized"
resamplingLinkTextSnippet Hu = "Újraválogatás"
