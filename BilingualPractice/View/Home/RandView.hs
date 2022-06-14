{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Home.RandView (randView) where

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry (..))
import BilingualPractice.Model.ViewModel (view)
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Control.Monad (forM_)

randView :: [LexiconEntry] -> Html
randView records = docTypeHtml ! lang "hu" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Hungarian-English word and sentence practice quiz-sets — Lexicon random sampling"
    body $ do
        h1 "Hungarian-English word and sentence practice quiz-sets — Lexicon random sampling"
        p $ do
            a ! href "/rand" $ "Re-sampling again randomized"
            span " •|||• "
            a ! href "/" $ "Back to the main page"
        table $ do
            tr $ do
                th "English"
                th "Hungarian"
                th "Word or sentence?"
                th "Difficulty level"
            forM_ records $ \ LxcE {en, hu, entity, difficulty} -> do
                tr $ do
                    td $ toHtml en
                    td $ toHtml hu
                    td $ toHtml $ view entity
                    td $ toHtml $ view difficulty
