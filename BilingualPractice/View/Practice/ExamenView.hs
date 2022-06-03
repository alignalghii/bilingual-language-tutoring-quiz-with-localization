{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Practice.ExamenView (examenView) where

import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)

examenView :: Html
examenView = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        title "Magyar-angol szó- és mondatgyakorló — Gyakorlóvizsga (kérdéssor)"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Gyakorlóvizsga (kérdéssor)"
        p $ do
            a ! href "/" $ "Vissza a főoldalra"
        form ! action "/practice/new" ! method "post" $ do
            div "Új gyakorlóvizsga indítása (a régi válaszaid, ha vannak, törlődnek)"
            label "Ennyi kérdésből álljon a gyakorlat:"
            input ! type_ "number" ! class_ "smallnum" ! min "1" ! max "30" ! name "number_of_questions" ! value "5"
            div "Szám, szó, vagy mondat gyakoroltatása legyen?"
            ul $ do
                li $ do
                    input ! type_ "checkbox" ! name "number" ! checked ""
                    label "Szám"
                li $ do
                    input ! type_ "checkbox" ! name "word" ! checked ""
                    label "Szó"
                li $ do
                    input ! type_ "checkbox" ! name "sentence" ! checked ""
                    label "Mondat"
            div "Milyen nehézségi szinten?"
            ul $ do
                li $ do
                    input ! type_ "checkbox" ! name "easy" ! checked ""
                    label "Könnyű"
                li $ do
                    input ! type_ "checkbox" ! name "difficult" ! checked ""
                    label "Nehéz"
            button ! type_ "submit" $ "Mehet"

