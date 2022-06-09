{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Home.RandView (randView) where

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry (..))
import BilingualPractice.Model.ViewModel (view)
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Control.Monad (forM_)

randView :: [LexiconEntry] -> Html
randView records = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Magyar-angol szó- és mondatgyakorló — Lexikon véletlen mintakiválasztása"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Lexikon véletlen mintakiválasztása"
        p $ do
            a ! href "/rand" $ "Újraválogatás"
            span " •|||• "
            a ! href "/" $ "Vissza a főoldalra"
        table $ do
            tr $ do
                th "Angol"
                th "Magyar"
                th "Szó vagy mondat?"
                th "Nehézségi szint"
            forM_ records $ \ LxcE {en, hu, entity, difficulty} -> do
                tr $ do
                    td $ toHtml en
                    td $ toHtml hu
                    td $ toHtml $ view entity
                    td $ toHtml $ view difficulty
