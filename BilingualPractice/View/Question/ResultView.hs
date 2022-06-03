{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Question.ResultView (resultView) where

import BilingualPractice.Model.ViewModel (QuestionAnswerMatchView (..))
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, mark)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Time


resultView :: [QuestionAnswerMatchView] -> Html
resultView confer = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Magyar-angol szó- és mondatgyakorló — Eredményhirdetés"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Eredményhirdetés"
        p $ do
            a ! href "/practice/new" $ "Új vizsga"
            span " ||| "
            a ! href "/" $ "Vissza a főoldalra"
        table $ do
            tr $ do
                th "Magyar"
                th "Angol"
                th "A Te válaszod"
                th "Jó vagy rossz lett-e?"
                th "Kérdés időpontja"
                th "Válaszod időpontja"
                th "Szó vagy mondat?"
                th "Nehézségi szint"
            forM_ confer $ \QuAnsMtchVw {dictHuView, dictEnView, yourEnView, markView = (markMsg, markStl), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView} -> do
                tr $ do
                    td $ toHtml dictHuView
                    td $ toHtml dictEnView
                    td ! class_ (toValue markStl) $ toHtml yourEnView
                    td $ toHtml markMsg
                    td $ toHtml $ askedAtTimeView
                    td $ toHtml $ answeredAtTimeView
                    td $ toHtml dictEntityView
                    td $ toHtml dictDifficultyView
