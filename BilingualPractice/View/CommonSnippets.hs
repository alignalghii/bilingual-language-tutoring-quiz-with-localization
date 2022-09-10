{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.CommonSnippets where

import BilingualPractice.Model.ViewModel (view)
import BilingualPractice.View.LanguageHelper (langLink)
import BilingualPractice.Language (Language (..))
import Data.ReflectionX (allInhabitants)
import Text.Blaze.Html5 (Html, div, span, AttributeValue)
import Prelude hiding (div, span)
import Data.String (IsString)
import Data.List (intersperse)


appTitleSnippet :: IsString string => Language -> string
appTitleSnippet En = "Hungarian-English word and sentence practice quiz-sets"
appTitleSnippet Hu = "Magyar-angol szó- és mondatgyakorló"

backHomeLinkTextSnippet :: IsString string => Language -> string
backHomeLinkTextSnippet En = "Back to the main page"
backHomeLinkTextSnippet Hu = "Vissza a főoldalra"

portfolioLinkTextSnippet :: IsString string => Language -> string
portfolioLinkTextSnippet En = "https://alignalghii.github.io"
portfolioLinkTextSnippet Hu = "https://alignalghii.github.io/index.hu.html"

askLinguisticalUnitSnippet, askDifficultyLevelSnippet :: IsString string => Language -> string
askLinguisticalUnitSnippet En = "Word or sentence?"
askLinguisticalUnitSnippet Hu = "Szó vagy mondat?"
askDifficultyLevelSnippet En = "Difficulty level"
askDifficultyLevelSnippet Hu = "Nehézségi szint"

newPracticeLinkTextSnippet :: IsString string => Language -> string
newPracticeLinkTextSnippet En = "Start a new practice"
newPracticeLinkTextSnippet Hu = "Új gyakorlat indítása"

askGuessCorrectnessStatusSnippet :: IsString string => Language -> string
askGuessCorrectnessStatusSnippet En = "Correct or wrong?"
askGuessCorrectnessStatusSnippet Hu = "Jó vagy rossz lett-e?"

submitCommandSnippet :: IsString string => Language -> string
submitCommandSnippet En = "Go!"
submitCommandSnippet Hu = "Mehet!"


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

languageSelectionFlagBarSnippet :: Language -> AttributeValue -> Html
languageSelectionFlagBarSnippet language url = div $ sequence_ $ intersperse (span "|") $ languageFlagLink language url <$> allInhabitants

languageFlagLink :: Language -> AttributeValue -> Language -> Html
languageFlagLink how url which
    | how == which = span $ view how which
    | otherwise    = langLink which url $ view how
