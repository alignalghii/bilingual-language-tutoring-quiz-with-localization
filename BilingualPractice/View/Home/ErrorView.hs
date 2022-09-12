{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Home.ErrorView (errorView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, languageSelectionFlagBarSnippet')
import BilingualPractice.Model.ViewModel (view)
import Framework.Url (Url)
import BilingualPractice.Model.Error (Error)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.View.LanguageHelper (langLink', langAction')

import Prelude hiding (head, span, div)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Data.Bool (bool)

errorView :: Error -> Language -> Url -> Html
errorView err language selfUrl = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/general.css"
        link ! rel "stylesheet" ! href "/style/form.css"
        title $ titleSnippet language
    body $ do
        languageSelectionFlagBarSnippet' language selfUrl
        h1 $ titleSnippet language
        ul $ do
            li $ do
                strong $ lexiconDefiniendumSnippet language
                span $ lexiconDefinitorSnippet language
                ul $ do
                    langLink' language "/dump" wholeDumpLinkTextSnippet
                    span $ wholeDumpExplanationSnippet language
                    li $ do
                        langLink' language "/rand" randomSelectionLinkTextSnippet
                        span $ randomSelectionExplanationSnippet language
            li $ do
                strong $ practicesDefiniendumSnippet language
                span $ practicesDefinitorSnippet language
                ul $ do
                    li $ do
                        langLink' language "/practice/new" newPracticeLinkTextSnippet
                        span $ newPracticeExplanationSnippet language
                    li $ do
                        langLink' language "/practice/index" indexPracticeLinkTextSnippet
                        span $ indexPracticeExplanationSnippet language
            li $ do
                strong $ usersDefiniendumSnippet language
        p ! class_ "error" $ view language err
        div $ errorFixHintHeadingSnippet language
        ul $ do
            li $ do
                span $ emptyDataErrorCaseHeadingSnippet language
                ul $ do
                    li $ do
                        span $ disjunctionWord1Snippet language
                        langLink' language "/practice/new" tooStrictFilteringHintSnippet
                    li $ do
                        span $ disjunctionWord2Snippet language
                        emptyDatabaseHintSnippet language
            li $ do
                span $ inconsistentTraversalErrorCaseHeadingSnippet language
                ul $ do
                    li $
                        form ! method "post" ! langAction' language "/practice/closefix" ! class_ "inline" $
                            button ! type_ "submit" $ closeFixButtonLabelSnippet language
            li $ noProblemTextSnippet language


titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Error!"
titleSnippet Hu = appTitleSnippet Hu <> " — Hiba!"

lexiconDefiniendumSnippet En = "Lexicon:"
lexiconDefiniendumSnippet Hu = "Lexikon:"

lexiconDefinitorSnippet :: IsString string => Language -> string
lexiconDefinitorSnippet En = "that is, all words, phrases, sentences that make up the contents of the practice quiz-sets"
lexiconDefinitorSnippet Hu = "a program, a gyakorlatok, a tanulás alapját képező szavak, szókapcsolatok, mondatok összessége"

wholeDumpLinkTextSnippet :: IsString string => Language -> string
wholeDumpLinkTextSnippet En = "Whole-dump index list: showing the underlying complete lexicon"
wholeDumpLinkTextSnippet Hu = "Teljes kimutatás:"

wholeDumpExplanationSnippet :: IsString string => Language -> string
wholeDumpExplanationSnippet En = "— You can see the raw entry data of the entire lexicon here"
wholeDumpExplanationSnippet Hu = "itt láthatod tartalmilag a teljes anyagot a maga nyers összességében"

randomSelectionLinkTextSnippet :: IsString string => Language -> string
randomSelectionLinkTextSnippet En = "Random selection:"
randomSelectionLinkTextSnippet Hu = "Véletlen kiválasztás:"

randomSelectionExplanationSnippet :: IsString string => Language -> string
randomSelectionExplanationSnippet En = "You can see the operation of the randomized sampling here, for example, what the proportions of various difficulty levels are and also the proportions of words, phrases and sentences are to each other."
randomSelectionExplanationSnippet Hu = "itt láthatsz mintát arról, hogyan működik a véletlen mintvétel, mik  lexikont kitevő szó-, szókapcsolat- és mondatkincs arányai"

practicesDefiniendumSnippet :: IsString string => Language -> string
practicesDefiniendumSnippet En = "Practices:"
practicesDefiniendumSnippet Hu = "Gyakorlatok:"

practicesDefinitorSnippet :: IsString string => Language -> string
practicesDefinitorSnippet En = "that is, series sets of question-answer pairs, i.e quiz-sets (by simple randomized sampling, for the time being, but in future also custom-assembled materials will be uploadable by a tutor)"
practicesDefinitorSnippet Hu = "vagyis összeállított kérdéssorok (egyelőre véletlen leválogatással, később egyedi összeválogatott gyakorlósorok is feltölthetőek lesznek)"

newPracticeLinkTextSnippet :: IsString string => Language -> string
newPracticeLinkTextSnippet En = "New practice: generating a random quiz"
newPracticeLinkTextSnippet Hu = "Új gyakorlat: véletlen kérdéssor generálása."

newPracticeExplanationSnippet :: IsString string => Language -> string
newPracticeExplanationSnippet En = "sampled randomly from the lexicon: questions, to which You will answer, and Your done practice can be stored, viewed back and repeated, too."
newPracticeExplanationSnippet Hu = "Tudásteszt, gyakorlás: megválaszolandó kérdések sorozata, majd válaszaid után kiértékelés, eredmény és mentés"

indexPracticeLinkTextSnippet :: IsString string => Language -> string
indexPracticeLinkTextSnippet En = "Your personal history: all the practices You have already done"
indexPracticeLinkTextSnippet Hu = "Személyes történeted: vagyis elvégzett eddigi gyakorlataid"

indexPracticeExplanationSnippet :: IsString string => Language -> string
indexPracticeExplanationSnippet En = "— You can see them here as Your answers given to the questions you received in Your former practicings, evaluated, and in time order"
indexPracticeExplanationSnippet Hu = "— itt láthatod kapott kérdéseidre adott válaszaidat kérdés-felelet párokként, kiértékelve, időrendben"

usersDefiniendumSnippet :: IsString string => Language -> string
usersDefiniendumSnippet En = "Users"
usersDefiniendumSnippet Hu = "Felhasználók"

errorFixHintHeadingSnippet :: IsString string => Language -> string
errorFixHintHeadingSnippet En = "Possible help hints to fix the error:"
errorFixHintHeadingSnippet Hu = "Lehetséges segítség:"

emptyDataErrorCaseHeadingSnippet :: IsString string => Language -> string
emptyDataErrorCaseHeadingSnippet En = "In case of empty data error:"
emptyDataErrorCaseHeadingSnippet Hu = "Üres adat hiba esetén nincs különösebb tennivalód:"

disjunctionWord1Snippet :: IsString string => Language -> string
disjunctionWord1Snippet En = "either"
disjunctionWord1Snippet Hu = "vagy"

disjunctionWord2Snippet :: IsString string => Language -> string
disjunctionWord2Snippet En = "or"
disjunctionWord2Snippet Hu = "vagy"

tooStrictFilteringHintSnippet :: IsString string => Language -> string
tooStrictFilteringHintSnippet En = "maybe You have set the filtering options too strict,"
tooStrictFilteringHintSnippet Hu = "enyhíts a szűrési feltételeken!"

emptyDatabaseHintSnippet :: IsString string => Language -> string
emptyDatabaseHintSnippet En = "or the database has not yet been filled with data"
emptyDatabaseHintSnippet Hu = "várj az adatbázis nagyobb feltöltöttségére,"

inconsistentTraversalErrorCaseHeadingSnippet :: IsString string => Language -> string
inconsistentTraversalErrorCaseHeadingSnippet En = "In case of inconsistent traversal of the site error: probably You have opened a practice and it got interrupted without closing due to some forced traversal."
inconsistentTraversalErrorCaseHeadingSnippet Hu = "Bejárási következetlenég, lezáratlanul maradt cselekmény esetén"

closeFixButtonLabelSnippet :: IsString string => Language -> string
closeFixButtonLabelSnippet En = "click here to close."
closeFixButtonLabelSnippet Hu = "zárd be az esetleg szabálytalanul megszakított (lezárás nélkül) félbe maradt új gyakolatot."

noProblemTextSnippet :: IsString string => Language -> string
noProblemTextSnippet En = "There is also a chance that no action has to be done: the error is temporary, You can use the site without any worries"
noProblemTextSnippet Hu = "Előfordulhat, hogy nem kell semmit csinálnod: használd nyugodtan tovább az oldalt"
