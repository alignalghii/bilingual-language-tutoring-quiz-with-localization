{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Home.ErrorView (errorView) where

import Prelude hiding (head, span, div)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Data.Bool (bool)

errorView :: String -> Html
errorView msg = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/general.css"
        link ! rel "stylesheet" ! href "/style/form.css"
        title "Magyar-angol szó- és mondatgyakorló — Hiba!"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Hiba!"
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
        p ! class_ "error" $ toHtml msg
        div "Lehetséges segítség:"
        ul $ do
            li $ do
                span "Üres adat hiba esetén nincs különösebb tennivalód:"
                ul $ do
                    li "várj az adatbázis nagyobb feltöltöttségére,"
                    li $ do
                        span "vagy"
                        a ! href "/practice/new" $ "enyhíts a szűrési feltételeken!"
            li $ do
                span "Bejárási következetlenég, lezáratlanul maradt cselekmény esetén"
                ul $ do
                    li $
                        form ! method "post" ! action "/practice/closefix" ! class_ "inline" $
                            button ! type_ "submit" $ "zárd be az esetleg szabálytalanul megszakított (lezárás nélkül) félbe maradt új gyakolatot."
            li "Előfordulhat, hogy nem kell semmit csinálnod: használd nyugodtan tovább az oldalt"
