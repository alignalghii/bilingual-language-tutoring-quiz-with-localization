{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Home.HomeView (homeView) where

import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Data.Bool (bool)

homeView :: Html
homeView = docTypeHtml ! lang "hu" ! lang "hu" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        title "Hungarian-English word and sentence practice quiz-sets"
    body $ do
        h1 "Hungarian-English word and sentence practice quiz-sets"
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
        h2 "Description for users"
        p $ do
            span "The user can practice on quiz-like question sets: Hungarian words, phrases, or short sentences come up, and the user has to type in his/her attempt for an English translation as answer. The words, phrases, and sentences are randomly selected from the pre-uploaded"
            q "lexicon"
            span "— the user can customize this selection by filtering (on dificulty level, and also on whether sentences, phrases, or words should come up). Filtering can be issued in any possible combinations."
        p "Each practice done by the user are saved automatically, the user can manage them: view again, repeat again, delete them. Later in futur e also some kind of cherry-picking, free assemblage of new practices will be added as feature, also statistics, maybe even multi-player contests."
        h2 "Description for developers"
        p $ do
            span "Technically, this project is an challenge: to explore the usefulness of declarative paradigms in web programming, especially the possible gains provided by functional languges like Haskell. In details: the"
            q "Scotty"
            span "micro-framework is underlying the whole project. The whole sorce-code is available freely: "
            a ! href "https://github.com/alignalghii/bilingual-Hungarian-English-practice-quiz-app" ! target "_plain" $ "GitHub project page."
        p $ do
            span "I have also a personal portfolio page about both my past in Haskell (and its underlying mathematics), and also aobut my future visions: see"
            a ! href "https://alignalghii.github.io"  ! target "_plain" $ "this profolio site"
            span "too."
