{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Home.HomeView (homeView) where

import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Data.Bool (bool)

homeView :: Html
homeView = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        title "Magyar-angol szó- és mondatgyakorló"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló"
        ul $ do
            li $ do
                strong "Lexikon:"
                span "a program, a gyakolatok, a tanulás alapját képező szavak, szókapcsolatok, mondatok összessége"
                ul $ do
                    li $ do
                        a ! href "/dump" $ "Teljes kimutatás:"
                        span "itt láthatod tartalmilag a teljes anyagot a maga nyers összességében"
                    li $ do
                        a ! href "/rand" $ "Véletlen kiválasztás:"
                        span "itt láthastsz mintát arról, hogyan működik a véletlen mintvétel, mik  lexikont kitevő szó-, szókapcsolat- és mondatkincs arányai"
            li $ do
                strong "Gyakorlatok:"
                span "vagyis összeállított kérdéssorok (egyelőre véletlen leválogatással, később egyedi összeválogatott gyakorlósorok is feltölthetőek lesznek)"
                ul $ do
                    li $ do
                        a ! href "/practice/index" $ "Személyes történeted: vagyis elvégzett eddigi gyakorlataid"
                        span "— itt láthatod kapott kérdéseidre adott válaszaidat kérdés-felelet párokként, kiértékelve, időrendben"
                    li $ do
                        a ! href "/practice/new" $ "Új gyakorlat: véletlen kérdéssor generálása."
                        span "Tudásteszt, gyakorlás: megválaszolandó kérdések sorozata, majd válaszaid után kiértékelés, eredmény és mentés"
            li $ do
                strong "Felhasználók"
