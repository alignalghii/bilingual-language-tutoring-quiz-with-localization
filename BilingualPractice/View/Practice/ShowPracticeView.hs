{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Practice.ShowPracticeView (showPracticeView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, backHomeLinkTextSnippet, newPracticeLinkTextSnippet, askLinguisticalUnitSnippet, askDifficultyLevelSnippet, askGuessCorrectnessStatusSnippet)
import BilingualPractice.Model.ViewModel (Viewable (view))
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.Model.ViewModel (QuestionAnswerMatchView (..))
import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Time (UTCTime)


showPracticeView :: Language -> UTCTime -> String -> [QuestionAnswerMatchView] -> Html
showPracticeView language startTime startTimeLocalised matches = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        link ! rel "stylesheet" ! href "/style/table.css"
        title $ titleSnippet language
    body $ do
        h1 $ titleSnippet language
        p $ do
            form ! method "post" ! action "/practice/repeat" ! class_ "inline" $ button ! type_ "submit" ! name "start" ! value (toValue $ show startTime) $ repeatSamePracticeCommandSnippet language
            span " •|||• "
            a ! href "/practice/new" $ newPracticeLinkTextSnippet language <> "!"
            span " •|||• "
            a ! href "/practice/index" $ backToPracticeIndexLinkTextSnippet language
            span " •|||• "
            a ! href "/" $ backHomeLinkTextSnippet language
        table $ do
            caption $ toHtml startTimeLocalised
            tr $ do
                th $ view language Hu
                th $ view language En
                th $ echoDesignationSnippet language
                th $ askGuessCorrectnessStatusSnippet language
                th $ askQuestionReceivingTimeSnippet language
                th $ askAnswerProvidingTimeSnippet language
                th $ askLinguisticalUnitSnippet language
                th $ askDifficultyLevelSnippet  language
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


-- Localization snippets:

titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Showing the selected former practice"
titleSnippet Hu = appTitleSnippet Hu <> " — A kiválasztott korábbi gyakorlat megmutatása"

backToPracticeIndexLinkTextSnippet :: (IsString string, Semigroup string) => Language -> string
backToPracticeIndexLinkTextSnippet En = appTitleSnippet En <> "Back to the complete list of all Your former practices"
backToPracticeIndexLinkTextSnippet Hu = appTitleSnippet Hu <> "Vissza a többi régi gyakorlatod listájához"

echoDesignationSnippet :: IsString string => Language -> string
echoDesignationSnippet En = "Your answer"
echoDesignationSnippet Hu = "A Te válaszod"

askQuestionReceivingTimeSnippet :: IsString string => Language -> string
askQuestionReceivingTimeSnippet En = "Time when You received questions"
askQuestionReceivingTimeSnippet Hu = "Kérdés időpontja"

askAnswerProvidingTimeSnippet :: IsString string => Language -> string
askAnswerProvidingTimeSnippet En = "Time when You provided the answer"
askAnswerProvidingTimeSnippet Hu = "Válaszod időpontja"

repeatSamePracticeCommandSnippet :: IsString string => Language -> string
repeatSamePracticeCommandSnippet En = "Repeat this very same practice!"
repeatSamePracticeCommandSnippet Hu = "Ismételd meg ugyanezt a gyakorlatot!"
