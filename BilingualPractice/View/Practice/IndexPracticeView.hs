{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Practice.IndexPracticeView (indexPracticeView) where

import BilingualPractice.Model.ViewModel (PracticeView (..))
import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)

indexPracticeView :: [PracticeView] -> Html
indexPracticeView practices = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Magyar-angol szó- és mondatgyakorló — Gyakorlóvizsga (kérdéssor)"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Eddigi gyakorlataid listája"
        p $ do
            a ! href "/" $ "Vissza a főoldalra"
            span " ||| "
            a ! href "/practice/new" $ "Új gyakorlat indítása"
        table $ do
            tr $ do
                th "A gyakorlat kezdőidőpontja"
                th "Kérdések száma"
                th "Megmutat"
                th "Töröl"
            forM_ practices $ \ PrcVw {prcStartTimeView, questionsCount} -> do
                tr $ do
                    td $ toHtml prcStartTimeView
                    td $ toHtml questionsCount
                    td ""
                    td ""
