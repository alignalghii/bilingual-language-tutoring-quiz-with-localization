{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Question.ResultView (resultView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, backHomeLinkTextSnippet, newPracticeLinkTextSnippet, askLinguisticalUnitSnippet, askDifficultyLevelSnippet, askGuessCorrectnessStatusSnippet, echoDesignationSnippet, askQuestionReceivingTimeSnippet, askAnswerProvidingTimeSnippet, repeatSamePracticeCommandSnippet)
import BilingualPractice.Model.ViewModel (Viewable (view))
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.View.Helper (langLink)

import BilingualPractice.Model.ViewModel (QuestionAnswerMatchView (..))
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, mark)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Time


resultView :: Language -> UTCTime -> [QuestionAnswerMatchView] -> Html
resultView language startTime confer = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/table.css"
        link ! rel "stylesheet" ! href "/style/form.css"
        title $ titleSnippet language
    body $ do
        h1 $ titleSnippet language
        p $ do
            form ! method "post" ! action "/practice/repeat" ! class_ "inline" $ button ! type_ "submit" ! name "start" ! value (toValue $ show startTime) $ repeatSamePracticeCommandSnippet language
            span " •|||• "
            langLink language "/practice/new" newPracticeLinkTextSnippet
            span " •|||• "
            langLink language "/" backHomeLinkTextSnippet
        table $ do
            tr $ do
                th $ view language Hu
                th $ view language En
                th $ echoDesignationSnippet language
                th $ askGuessCorrectnessStatusSnippet language
                th $ askQuestionReceivingTimeSnippet language
                th $ askAnswerProvidingTimeSnippet language
                th $ askLinguisticalUnitSnippet language
                th $ askDifficultyLevelSnippet  language
            forM_ confer $ \QuAnsMtchVw {dictHuView, dictEnView, yourEnView, markViewAndStyle = (markMsg, markStl), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView} -> do
                tr $ do
                    td $ toHtml dictHuView
                    td $ toHtml dictEnView
                    td ! class_ (toValue markStl) $ toHtml yourEnView
                    td $ toHtml markMsg
                    td $ toHtml $ askedAtTimeView
                    td $ toHtml $ answeredAtTimeView
                    td $ toHtml dictEntityView
                    td $ toHtml dictDifficultyView


-- Localization snippets:

titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Results of the practice"
titleSnippet Hu = appTitleSnippet Hu <> " — Eredményhirdetés"
