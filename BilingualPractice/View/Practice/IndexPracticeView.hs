{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Practice.IndexPracticeView (indexPracticeView) where

import BilingualPractice.Model.ViewModel (PracticeView (..))
import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label, div)
import Network.URI.Encode (encode)
import Control.Monad (forM_)
import Data.Time (UTCTime)
import Data.Bool (bool)

indexPracticeView :: [PracticeView] -> Html
indexPracticeView practices = docTypeHtml ! lang "hu" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        link ! rel "stylesheet" ! href "/style/table.css"
        title "Hungarian-English word and sentence practice quiz-sets — List of Your former practices"
    body $ do
        h1 "Hungarian-English word and sentence practice quiz-sets — List of Your former practices"
        p $ do
            a ! href "/" $ "Back to the main page"
            span " •|||• "
            a ! href "/practice/new" $ "Start a new practice"
        div $
            table $ do
                tr $ do
                    th "The start time of the practice"
                    th "Number of questions"
                    th "Show"
                    th "Delete"
                    th "Repeat"
                forM_ practices $ \ PrcVw {prcStartTimeId, prcStartTimeView, questionsCount} -> do
                    tr $ do
                        td $ toHtml prcStartTimeView
                        td $ toHtml questionsCount
                        td $ bool "" (showLink prcStartTimeId) (questionsCount > 0)
                        td $ showFormDel prcStartTimeId
                        td $ bool "" (showFormRep prcStartTimeId) (questionsCount > 0)

showLink :: UTCTime -> Html
showLink timeId = a ! href ("/practice/show/" <> (toValue $ encode $ encode $ show timeId)) $ "Show"

showFormDel, showFormRep :: UTCTime -> Html
showFormDel timeId = form ! method "post" ! action "/practice/delete" $ button ! type_ "submit" ! name "start" ! value (toValue $ show timeId) $ "Delete!"
showFormRep timeId = form ! method "post" ! action "/practice/repeat" $ button ! type_ "submit" ! name "start" ! value (toValue $ show timeId) $ "Repeat!"
