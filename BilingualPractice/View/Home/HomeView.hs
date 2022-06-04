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
                span "Fejlesztőknek szóló segédfunkciók"
                ul $ do
                    li $ do
                        a ! href "/dump" $ "Teljes kimutatás"
                        span "(adminisztrátoroknak, fejlesztőknek)"
                    li $ do
                        a ! href "/rand" $ "Véletlen kiválasztás"
                        span "(mint előfázisa a valódi tudástesztnek, gyakorlásnak)"
            li $ do
                span "Gyakorlatok"
                ul $ do
                    li $ do
                        a ! href "/practice/index" $ "Listázás: eddigi gyakorlataid"
                        span "Azok a kérdéssorok (gyakorlatok), amelyeket eddig végeztél"
                    li $ do
                        a ! href "/practice/new" $ "Új gyakorlat: véletlen kérdéssor generálása"
                        span "Valódi tudásteszt, gyakorlás: interaktív szakasz, megválaszolandó kérdések sorozata"
