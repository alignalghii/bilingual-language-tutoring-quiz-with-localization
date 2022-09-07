{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Practice.IndexPracticeView (indexPracticeView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, backHomeLinkTextSnippet, newPracticeLinkTextSnippet)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.View.Helper (langLink)

import BilingualPractice.Model.ViewModel (PracticeView (..))
import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label, div)
import Network.URI.Encode (encode)
import Control.Monad (forM_)
import Data.Time (UTCTime)
import Data.Bool (bool)

indexPracticeView :: Language -> [PracticeView] -> Html
indexPracticeView language practices = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        link ! rel "stylesheet" ! href "/style/table.css"
        title $ titleSnippet language
    body $ do
        h1 $ titleSnippet language
        p $ do
            langLink language "/" backHomeLinkTextSnippet
            span " •|||• "
            langLink language "/practice/new" newPracticeLinkTextSnippet
        div $
            table $ do
                tr $ do
                    th $ practiceStartTableHeaderTextSnippet  language
                    th $ practiceSizeTableHeaderTextSnippet   language
                    th $ practiceShowTableHeaderTextSnippet   language
                    th $ practiceDeleteTableHeaderTextSnippet language
                    th $ practiceRepeatTableHeaderTextSnippet language
                forM_ practices $ \ PrcVw {prcStartTimeId, prcStartTimeView, questionsCount} -> do
                    tr $ do
                        td $ toHtml prcStartTimeView
                        td $ toHtml questionsCount
                        td $ bool "" (showLink language prcStartTimeId) (questionsCount > 0)
                        td $ showFormDel language prcStartTimeId
                        td $ bool "" (showFormRep language prcStartTimeId) (questionsCount > 0)

showLink :: Language -> UTCTime -> Html
showLink language timeId = a ! href ("/practice/show/" <> (toValue $ encode $ encode $ show timeId)) $ showLinkTextSnippet language

showFormDel, showFormRep :: Language-> UTCTime -> Html
showFormDel language timeId = form ! method "post" ! action "/practice/delete" $ button ! type_ "submit" ! name "start" ! value (toValue $ show timeId) $ deleteLinkTextSnippet language
showFormRep language timeId = form ! method "post" ! action "/practice/repeat" $ button ! type_ "submit" ! name "start" ! value (toValue $ show timeId) $ repeatLinkTextSnippet language


-- Localization snippets:

titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — List of Your former practices"
titleSnippet Hu = appTitleSnippet Hu <> " — Eddigi gyakorlataid listája"

showLinkTextSnippet :: IsString string => Language -> string
showLinkTextSnippet En = "Show"
showLinkTextSnippet Hu = "Mutat"

deleteLinkTextSnippet :: IsString string => Language -> string
deleteLinkTextSnippet En = "Delete!"
deleteLinkTextSnippet Hu = "Töröld!"

repeatLinkTextSnippet :: IsString string => Language -> string
repeatLinkTextSnippet En = "Repeat!"
repeatLinkTextSnippet Hu = "Ismételd meg!"

practiceStartTableHeaderTextSnippet :: IsString string => Language -> string
practiceStartTableHeaderTextSnippet En = "The start time of the practice"
practiceStartTableHeaderTextSnippet Hu = "A gyakorlat kezdőidőpontja"

practiceSizeTableHeaderTextSnippet :: IsString string => Language -> string
practiceSizeTableHeaderTextSnippet En = "Number of questions"
practiceSizeTableHeaderTextSnippet Hu = "Kérdések száma"

practiceShowTableHeaderTextSnippet :: IsString string => Language -> string
practiceShowTableHeaderTextSnippet En = "Show"
practiceShowTableHeaderTextSnippet Hu = "Megmutat"

practiceDeleteTableHeaderTextSnippet :: IsString string => Language -> string
practiceDeleteTableHeaderTextSnippet En = "Delete"
practiceDeleteTableHeaderTextSnippet Hu = "Töröl"

practiceRepeatTableHeaderTextSnippet :: IsString string => Language -> string
practiceRepeatTableHeaderTextSnippet En = "Repeat"
practiceRepeatTableHeaderTextSnippet Hu = "Megismétel"
