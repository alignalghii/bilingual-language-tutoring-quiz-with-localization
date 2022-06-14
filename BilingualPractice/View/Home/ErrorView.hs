{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Home.ErrorView (errorView) where

import Prelude hiding (head, span, div)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Data.Bool (bool)

errorView :: String -> Html
errorView msg = docTypeHtml ! lang "hu" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/general.css"
        link ! rel "stylesheet" ! href "/style/form.css"
        title "Magyar-angol szó- és mondatgyakorló — Hiba!"
    body $ do
        ul $ do
            li $ do
                strong "Lexicon:"
                span "that is, all words, phrases, sentences that make up the contents of the practice quiz-sets"
                ul $ do
                    a ! href "/dump" $ "Whole-dump index list: showing the underlying complete lexicon"
                    span "— You can see the raw entry data of the entire lexicon here"
                    li $ do
                        a ! href "/rand" $ "Random selection:"
                        span "You can see the operation of the randomized sampling here, for example, what the proportions of various difficulty levels are and also the proportions of words, phrases and sentences are to each other."
            li $ do
                strong "Practices:"
                span "that is, series sets of question-answer pairs, i.e quiz-sets (by simple randomized sampling, for the time being, but in future also custom-assembled materials will be uploadable by a tutor)"
                ul $ do
                    li $ do
                        a ! href "/practice/index" $ "Your personal history: all the practices You have already done"
                        span "— You can see them here as Your answers given to the questions you received in Your former practicings, evaluated, and in time order"
                    li $ do
                        a ! href "/practice/new" $ "New practice: generating a random quiz"
                        span "sampled randomly from the lexicon: questions, to which You will answer, and Your done practice can be stored, viewed back and repeated, too."
            li $ do
                strong "Users"
        p ! class_ "error" $ toHtml msg
        div "Possible help hints to fix the error:"
        ul $ do
            li $ do
                span "In case of empty data error:"
                ul $ do
                    li $ do
                        span "either"
                        a ! href "/practice/new" $ "maybe You have set the filtering options too strict,"
                    li "or the database has not yet been filled with data"
            li $ do
                span "In case of inconsistent traversal of the site error: probably You have opened a practice and it got interrupted without closing due to some forced traversal."
                ul $ do
                    li $
                        form ! method "post" ! action "/practice/closefix" ! class_ "inline" $
                            button ! type_ "submit" $ "click here to close ."
            li "Előfordulhat, hogy nem kell semmit csinálnod: használd nyugodtan tovább az oldalt"
