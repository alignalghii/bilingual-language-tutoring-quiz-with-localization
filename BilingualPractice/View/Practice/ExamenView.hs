{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Practice.ExamenView (examenView) where

import BilingualPractice.Model.ViewModel (view)
import BilingualPractice.View.CommonSnippets (appTitleSnippet, backHomeLinkTextSnippet)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Framework.Form (formParam)
import BilingualPractice.Model.RelationalBusinessLogic (LinguisticalUnit (..), Difficulty (..))
import Data.String (IsString)

import Prelude hiding (head, div, span, min, max)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)

examenView :: Language -> Html
examenView language = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        title $ titleSnippet language
    body $ do
        h1 $ titleSnippet language
        p $ do
            a ! href "/" $ backHomeLinkTextSnippet language
        form ! action "/practice/new" ! method "post" $ do
            p $ newPracticeDetailingSnippet language
            label $ askPracticeSizeSnippet language
            input ! type_ "number" ! class_ "smallnum" ! min "1" ! max "30" ! name "number_of_questions" ! value "5"
            div $ askLinguisticalUnitSnippet language
            ul $ do
                li $ do
                    input ! type_ "checkbox" ! name (formParam LUNumber) ! checked ""
                    label $ view LUNumber language
                li $ do
                    input ! type_ "checkbox" ! name (formParam LUWord) ! checked ""
                    label $ view LUWord language
                li $ do
                    input ! type_ "checkbox" ! name (formParam LUPhrase) ! checked ""
                    label $ view LUPhrase language
                li $ do
                    input ! type_ "checkbox" ! name (formParam LUSentence) ! checked ""
                    label $ view LUSentence language
            div $ askDifficultyLevel language
            ul $ do
                li $ do
                    input ! type_ "checkbox" ! name (formParam Easy) ! checked ""
                    label $ view Easy language
                li $ do
                    input ! type_ "checkbox" ! name (formParam MiddleLevel) ! checked ""
                    label $ view MiddleLevel language
                li $ do
                    input ! type_ "checkbox" ! name (formParam Difficult) ! checked ""
                    label $ view Difficult language
            button ! type_ "submit" $ submitCommandSnippet language


titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Start a new practice (quiz-set)"
titleSnippet Hu = appTitleSnippet Hu <> " — Új gyakorlat (véletlen kérdéssor) indítása"

newPracticeDetailingSnippet :: IsString string => Language -> string
newPracticeDetailingSnippet En = "Here you can start a new practice: generate a new question sequence. Beware that if You have pending answers, they get deleted immediatelly upon Your clicking the &ldquo;Start new practice button&rdquo;. (You may have such pending (&ldquo;zombie&rdquo;) answers in the case when You had earlier new practice generation attempts which got interruped in an irreguar way, i.e. nor have You done them, nor have You quitted them explicitely with the &ldquo;Quit practice&rdquo; or the &ldquo;Back to main page&rdquo; buttons.)"
newPracticeDetailingSnippet Hu = "Új gyakorlóvizsga-kérdéssort indíthatsz, generáltathatsz itt. Ha vannak függő válaszaid korábbról, azok törlődni fognak, amikor megindítod itt az új gyakorlatsor generálását! (Efféle függő (&bdquo;zombi&rdquo;) válaszaid akkor létezhetnek, ha már korábban is generáltattál új gyakorlatsort, de úgy, hogy azt nem zártad le, azaz szabálytalanul léptél ki belőlük: azaz anélkül, hogy végigcsináltad volna őket, vagy pedig a külön erre szolgáló &bdquo;Megszakítás&rdquo; vagy &bdquo;Vissza&rdquo; gombbal kifejezetten megszakítottad volna őket.)"

askPracticeSizeSnippet :: IsString string => Language -> string
askPracticeSizeSnippet En = "The practice should consist of that many questions:"
askPracticeSizeSnippet Hu = "Ennyi kérdésből álljon a gyakorlat:"

askLinguisticalUnitSnippet :: IsString string => Language -> string
askLinguisticalUnitSnippet En = "What kind of linguistical units should we practice: numbers, words, phrases, or sentences?"
askLinguisticalUnitSnippet Hu = "Szám, szó, vagy mondat gyakoroltatása legyen?"

askDifficultyLevel :: IsString string => Language -> string
askDifficultyLevel En = "On what difficulty level?"
askDifficultyLevel Hu = "Milyen nehézségi szinten?"

submitCommandSnippet :: IsString string => Language -> string
submitCommandSnippet En = "Go!"
submitCommandSnippet Hu = "Mehet!"
