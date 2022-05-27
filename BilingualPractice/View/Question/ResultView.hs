{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Question.ResultView (resultView) where

import BilingualPractice.Model.RelationalBusinessLogic (QuestionAnswerMatch (..)) -- code smell?
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, mark)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Time


resultView :: [QuestionAnswerMatch] -> Html
resultView confer = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "img/favicon.ico"
        link ! rel "stylesheet" ! href "style/table.css"
        title "Magyar-angol szó- és mondatgyakorló — Eredményhirdetés"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Eredményhirdetés"
        p $ do
            a ! href "/examen" $ "Új vizsga"
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
            forM_ confer $ \QuAnsMtch {dictEn, dictHu, yourEn, flag, mark, askedAtTime, answeredAtTime, dictEntity, dictDifficulty} -> do
                tr $ do
                    td $ toHtml dictHu
                    td $ toHtml dictEn
                    td ! class_ (bool "wrong" "ok" flag) $ toHtml yourEn
                    td $ toHtml mark
                    td $ toHtml $ askedAtTime
                    td $ toHtml $ answeredAtTime
                    td $ toHtml dictEntity
                    td $ toHtml dictDifficulty
