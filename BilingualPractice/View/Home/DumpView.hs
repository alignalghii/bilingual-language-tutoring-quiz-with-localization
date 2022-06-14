{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Home.DumpView (dumpView) where

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry (..))
import BilingualPractice.Model.ViewModel (view)
import Prelude hiding (head)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, span, form)
import Control.Monad (forM_)

dumpView :: [LexiconEntry] -> Html
dumpView vocabularyData = docTypeHtml ! lang "en" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Hungarian-English word and sentence practice quiz-sets — Showing the underlying complete lexicon"
    body $ do
        h1 "Hungarian-English word and sentence practice quiz-sets — Showing the underlying complete lexicon"
        p $
            a ! href "/" $ "Back to the main page"
        table $ do
            tr $ do
                th "Hungarian"
                th "English"
                th "Word or sentence?"
                th "Difficulty level"
            forM_ vocabularyData $ \ LxcE {en, hu, entity, difficulty} -> do
                tr $ do
                    td $ toHtml hu
                    td $ toHtml en
                    td $ toHtml $ view entity
                    td $ toHtml $ view difficulty
