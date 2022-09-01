{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Practice.ShowPracticeView (showPracticeView) where

import BilingualPractice.Model.ViewModel (QuestionAnswerMatchView (..))
import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Time (UTCTime)


showPracticeView :: UTCTime -> String -> [QuestionAnswerMatchView] -> Html
showPracticeView startTime startTimeLocalised matches = docTypeHtml ! lang "en" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Hungarian-English word and sentence practice quiz-sets — Showing the selected former practice"
    body $ do
        h1 "Hungarian-English word and sentence practice quiz-sets  — Showing the selected former practice"
        p $ do
            form ! method "post" ! action "/practice/repeat" ! class_ "inline" $ button ! type_ "submit" ! name "start" ! value (toValue $ show startTime) $ "Repeat this very same practice!"
            span " •|||• "
            a ! href "/practice/new" $ "Start a new practice!"
            span " •|||• "
            a ! href "/practice/index" $ "Back to the complete list of all Your former practices"
            span " •|||• "
            a ! href "/" $ "Back to the main page"
        table $ do
            caption $ toHtml startTimeLocalised
            tr $ do
                th "Hungarian"
                th "English"
                th "Your answer"
                th "Correct or wrong?"
                th "Time when You received questions"
                th "Time when You provided the answer"
                th "Word or sentence?"
                th "Difficulty level"
            forM_ matches $ \QuAnsMtchVw {dictHuView, dictEnView, yourEnView, markViewAndStyle = (markMsg, markStl), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView} -> do
                tr $ do
                    td $ toHtml dictHuView
                    td $ toHtml dictEnView
                    td ! class_ (toValue markStl) $ toHtml yourEnView
                    td $ toHtml markMsg
                    td $ toHtml $ askedAtTimeView
                    td $ toHtml $ answeredAtTimeView
                    td $ toHtml dictEntityView
                    td $ toHtml dictDifficultyView
