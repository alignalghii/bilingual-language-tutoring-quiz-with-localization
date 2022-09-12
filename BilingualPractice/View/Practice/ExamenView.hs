{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Practice.ExamenView (examenView) where

import BilingualPractice.Model.ViewModel (Viewable (view))
import BilingualPractice.View.CommonSnippets (appTitleSnippet, languageSelectionFlagBarSnippet', backHomeLinkTextSnippet, submitCommandSnippet)
import BilingualPractice.View.StyleHelper (qEm)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Framework.Form (FormParamable (formParam))
import Framework.Url (Url)
import BilingualPractice.Model.RelationalBusinessLogic (LinguisticalUnit (..), Difficulty (..))
import Data.ReflectionX (allInhabitants)
import Data.String (IsString)

import BilingualPractice.View.LanguageHelper (langLink', langAction')

import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)


examenView :: Language -> Url -> Html
examenView language selfUrl = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        title $ titleSnippet language
    body $ do
        languageSelectionFlagBarSnippet' language selfUrl
        h1 $ titleSnippet language
        p $ do
            langLink' language "/" backHomeLinkTextSnippet
        form ! langAction' language "/practice/new" ! method "post" $ do
            p $ newPracticeDetailingSnippet language
            label $ askPracticeSizeSnippet language
            input ! type_ "number" ! class_ "smallnum" ! min "1" ! max "30" ! name "number_of_questions" ! value "5"
            div $ askLinguisticalUnitSnippet language
            listCheckboxesFor language (allInhabitants :: [LinguisticalUnit])
            div $ askDifficultyLevel language
            listCheckboxesFor language (allInhabitants :: [Difficulty])
            button ! type_ "submit" $ submitCommandSnippet language

listCheckboxesFor :: (FormParamable a, Viewable a) => Language -> [a] -> Html
listCheckboxesFor language values = ul $ forM_ values $ \value -> do
                           li $ do
                               input ! type_ "checkbox" ! name (formParam value) ! checked ""
                               label $ view language value


titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Start a new practice (quiz-set)"
titleSnippet Hu = appTitleSnippet Hu <> " — Új gyakorlat (véletlen kérdéssor) indítása"

newPracticeDetailingSnippet :: Language -> Html
newPracticeDetailingSnippet En = do
    "Here you can start a new practice: generate a new question sequence. Beware that if You have pending answers, they get deleted immediatelly upon Your clicking the" <> qEm "Start new practice button" <> "."
    "(You may have such pending [" <> qEm "zombie" <> "] answers in the case when You had earlier new practice generation attempts which got interruped in an irreguar way, i.e. nor have You done them, nor have You quitted them explicitely with the" <> qEm "Quit practice" <> "or the" <> qEm "Back to main page" <> "buttons.)"
newPracticeDetailingSnippet Hu = do
    "Új gyakorlóvizsga-kérdéssort indíthatsz, generáltathatsz itt. Ha vannak függő válaszaid korábbról, azok törlődni fognak, amikor megindítod itt az új gyakorlatsor generálását!"
    "(Efféle függő [" <> qEm "zombi" <> "] válaszaid akkor létezhetnek, ha már korábban is generáltattál új gyakorlatsort, de úgy, hogy azt nem zártad le, azaz szabálytalanul léptél ki belőlük: azaz anélkül, hogy végigcsináltad volna őket, vagy pedig a külön erre szolgáló" <> qEm "Megszakítás" <> "vagy" <> qEm "Vissza" <> "gombbal kifejezetten megszakítottad volna őket.)"

askPracticeSizeSnippet :: IsString string => Language -> string
askPracticeSizeSnippet En = "The practice should consist of that many questions:"
askPracticeSizeSnippet Hu = "Ennyi kérdésből álljon a gyakorlat:"

askLinguisticalUnitSnippet :: IsString string => Language -> string
askLinguisticalUnitSnippet En = "What kind of linguistical units should we practice: numbers, words, phrases, or sentences?"
askLinguisticalUnitSnippet Hu = "Szám, szó, vagy mondat gyakoroltatása legyen?"

askDifficultyLevel :: IsString string => Language -> string
askDifficultyLevel En = "On what difficulty level?"
askDifficultyLevel Hu = "Milyen nehézségi szinten?"
