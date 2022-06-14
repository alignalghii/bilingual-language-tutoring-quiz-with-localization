{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Question.ResultView (resultView) where

import BilingualPractice.Model.ViewModel (QuestionAnswerMatchView (..))
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, mark)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Time


resultView :: UTCTime -> [QuestionAnswerMatchView] -> Html
resultView startTime confer = docTypeHtml ! lang "en" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        link ! rel "stylesheet" ! href "/style/form.css"
        title "Hungarian-English word and sentence practice quiz-sets — Results of the practice"
    body $ do
        h1 "Hungarian-English word and sentence practice quiz-sets — Results of the practice"
        p $ do
            form ! method "post" ! action "/practice/repeat" ! class_ "inline" $ button ! type_ "submit" ! name "start" ! value (toValue $ show startTime) $ "Repeat this very same practice!"
            span " •|||• "
            a ! href "/practice/new" $ "New practice"
            span " •|||• "
            a ! href "/" $ "Back to the main page"
        table $ do
            tr $ do
                th "Hungarian"
                th "English"
                th "Your answer"
                th "Correct or wrong?"
                th "Time when You received questions"
                th "Time when You provided the answer"
                th "Word or sentence?"
                th "Difficulty level"
            forM_ confer $ \QuAnsMtchVw {dictHuView, dictEnView, yourEnView, markView = (markMsg, markStl), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView} -> do
                tr $ do
                    td $ toHtml dictHuView
                    td $ toHtml dictEnView
                    td ! class_ (toValue markStl) $ toHtml yourEnView
                    td $ toHtml markMsg
                    td $ toHtml $ askedAtTimeView
                    td $ toHtml $ answeredAtTimeView
                    td $ toHtml dictEntityView
                    td $ toHtml dictDifficultyView
