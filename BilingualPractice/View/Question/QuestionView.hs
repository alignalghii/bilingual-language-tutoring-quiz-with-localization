{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Question.QuestionView (questionView) where

import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, mark)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Time


questionView :: Int -> Int -> String -> Html
questionView nth ofAll hu = docTypeHtml $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "img/favicon.ico"
        title "Magyar-angol szó- és mondatgyakorló — Kérdés"
    body $ do
        h1 "Magyar-angol szó- és mondatgyakorló — Kérdés"
        p $ do
            a ! href "/practice/new" $ "Vizsga újraindítása, eddigi eredmények feldolgozatlan törlése"
            span " ||| "
            a ! href "/" $ "Vissza a főoldalra"
        p $ toHtml $ show nth ++ ". kérdés az " ++ show ofAll ++ " kérdésből:"
        form ! action "/question" ! method "post" $ do
            label "Magyarul:"
            span $ toHtml hu
            br
            label "Angolul:"
            input ! type_ "hidden" ! name "hu" ! value (toValue hu)
            input ! type_ "text"   ! name "en" ! autofocus ""
            button ! type_ "submit" $ "Mehet"
