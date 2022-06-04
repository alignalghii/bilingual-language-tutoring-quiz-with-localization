{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Practice.ShowPracticeView (showPracticeView) where

import BilingualPractice.Model.ViewModel (QuestionAnswerMatchView (..))
import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)

showPracticeView :: String -> [QuestionAnswerMatchView] -> Html
showPracticeView practiceBegin matches = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Magyar-angol szó- és mondatgyakorló — Gyakorlóvizsga (kérdéssor)"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Eddigi gyakorlataid listája"
        p $ do
            a ! href "/practice/index" $ "Vissza a többi régi gyakorlatod listájához"
            span " ||| "
            a ! href "/practice/new" $ "Új gyakorlat indítása"
            span " ||| "
            a ! href "/" $ "Vissza a főoldalra"
        table $ do
            caption $ toHtml practiceBegin
            tr $ do
                th "Magyar"
                th "Angol"
                th "A Te válaszod"
                th "Jó vagy rossz lett-e?"
                th "Kérdés időpontja"
                th "Válaszod időpontja"
                th "Szó vagy mondat?"
                th "Nehézségi szint"
            forM_ matches $ \QuAnsMtchVw {dictHuView, dictEnView, yourEnView, markView = (markMsg, markStl), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView} -> do
                tr $ do
                    td $ toHtml dictHuView
                    td $ toHtml dictEnView
                    td ! class_ (toValue markStl) $ toHtml yourEnView
                    td $ toHtml markMsg
                    td $ toHtml $ askedAtTimeView
                    td $ toHtml $ answeredAtTimeView
                    td $ toHtml dictEntityView
                    td $ toHtml dictDifficultyView
