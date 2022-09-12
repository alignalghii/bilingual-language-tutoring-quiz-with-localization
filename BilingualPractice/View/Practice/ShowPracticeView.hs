{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Practice.ShowPracticeView (showPracticeView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, languageSelectionFlagBarSnippet', backHomeLinkTextSnippet, newPracticeLinkTextSnippet, askLinguisticalUnitSnippet, askDifficultyLevelSnippet, askGuessCorrectnessStatusSnippet, echoDesignationSnippet, askQuestionReceivingTimeSnippet, askAnswerProvidingTimeSnippet, repeatSamePracticeCommandSnippet)
import BilingualPractice.Model.ViewModel (Viewable (view))
import BilingualPractice.Language (Language (..), languageAttrValue)
import Framework.Url (Url)
import Data.String (IsString)

import BilingualPractice.View.LanguageHelper (langLink', langAction')

import BilingualPractice.Model.ViewModel (QuestionAnswerMatchView (..))
import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Time (UTCTime)


showPracticeView :: Language -> Url -> UTCTime -> String -> [QuestionAnswerMatchView] -> Html
showPracticeView language selfUrl startTime startTimeLocalised matches = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        link ! rel "stylesheet" ! href "/style/table.css"
        title $ titleSnippet language
    body $ do
        languageSelectionFlagBarSnippet' language selfUrl
        h1 $ titleSnippet language
        p $ do
            form ! method "post" ! langAction' language "/practice/repeat" ! class_ "inline" $ button ! type_ "submit" ! name "start" ! value (toValue $ show startTime) $ repeatSamePracticeCommandSnippet language
            span " •|||• "
            langLink' language "/practice/new" $ (<> "!") . newPracticeLinkTextSnippet
            span " •|||• "
            langLink' language "/practice/index" backToPracticeIndexLinkTextSnippet
            span " •|||• "
            langLink' language "/" backHomeLinkTextSnippet
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
