{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Home.ErrorView (errorView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, languageSelectionFlagBarSnippet', backHomeLinkTextSnippet)
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
            li $ do
                noProblemTextSnippet language
                langLink' language "/" backHomeLinkTextSnippet


titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " ??? Error!"
titleSnippet Hu = appTitleSnippet Hu <> " ??? Hiba!"

lexiconDefiniendumSnippet En = "Lexicon:"
lexiconDefiniendumSnippet Hu = "Lexikon:"

lexiconDefinitorSnippet :: IsString string => Language -> string
lexiconDefinitorSnippet En = "that is, all words, phrases, sentences that make up the contents of the practice quiz-sets"
lexiconDefinitorSnippet Hu = "a program, a gyakorlatok, a tanul??s alapj??t k??pez?? szavak, sz??kapcsolatok, mondatok ??sszess??ge"

wholeDumpLinkTextSnippet :: IsString string => Language -> string
wholeDumpLinkTextSnippet En = "Whole-dump index list: showing the underlying complete lexicon"
wholeDumpLinkTextSnippet Hu = "Teljes kimutat??s:"

wholeDumpExplanationSnippet :: IsString string => Language -> string
wholeDumpExplanationSnippet En = "??? You can see the raw entry data of the entire lexicon here"
wholeDumpExplanationSnippet Hu = "itt l??thatod tartalmilag a teljes anyagot a maga nyers ??sszess??g??ben"

randomSelectionLinkTextSnippet :: IsString string => Language -> string
randomSelectionLinkTextSnippet En = "Random selection:"
randomSelectionLinkTextSnippet Hu = "V??letlen kiv??laszt??s:"

randomSelectionExplanationSnippet :: IsString string => Language -> string
randomSelectionExplanationSnippet En = "You can see the operation of the randomized sampling here, for example, what the proportions of various difficulty levels are and also the proportions of words, phrases and sentences are to each other."
randomSelectionExplanationSnippet Hu = "itt l??thatsz mint??t arr??l, hogyan m??k??dik a v??letlen mintv??tel, mik  lexikont kitev?? sz??-, sz??kapcsolat- ??s mondatkincs ar??nyai"

practicesDefiniendumSnippet :: IsString string => Language -> string
practicesDefiniendumSnippet En = "Practices:"
practicesDefiniendumSnippet Hu = "Gyakorlatok:"

practicesDefinitorSnippet :: IsString string => Language -> string
practicesDefinitorSnippet En = "that is, series sets of question-answer pairs, i.e quiz-sets (by simple randomized sampling, for the time being, but in future also custom-assembled materials will be uploadable by a tutor)"
practicesDefinitorSnippet Hu = "vagyis ??ssze??ll??tott k??rd??ssorok (egyel??re v??letlen lev??logat??ssal, k??s??bb egyedi ??sszev??logatott gyakorl??sorok is felt??lthet??ek lesznek)"

newPracticeLinkTextSnippet :: IsString string => Language -> string
newPracticeLinkTextSnippet En = "New practice: generating a random quiz"
newPracticeLinkTextSnippet Hu = "??j gyakorlat: v??letlen k??rd??ssor gener??l??sa."

newPracticeExplanationSnippet :: IsString string => Language -> string
newPracticeExplanationSnippet En = "sampled randomly from the lexicon: questions, to which You will answer, and Your done practice can be stored, viewed back and repeated, too."
newPracticeExplanationSnippet Hu = "Tud??steszt, gyakorl??s: megv??laszoland?? k??rd??sek sorozata, majd v??laszaid ut??n ki??rt??kel??s, eredm??ny ??s ment??s"

indexPracticeLinkTextSnippet :: IsString string => Language -> string
indexPracticeLinkTextSnippet En = "Your personal history: all the practices You have already done"
indexPracticeLinkTextSnippet Hu = "Szem??lyes t??rt??neted: vagyis elv??gzett eddigi gyakorlataid"

indexPracticeExplanationSnippet :: IsString string => Language -> string
indexPracticeExplanationSnippet En = "??? You can see them here as Your answers given to the questions you received in Your former practicings, evaluated, and in time order"
indexPracticeExplanationSnippet Hu = "??? itt l??thatod kapott k??rd??seidre adott v??laszaidat k??rd??s-felelet p??rokk??nt, ki??rt??kelve, id??rendben"

usersDefiniendumSnippet :: IsString string => Language -> string
usersDefiniendumSnippet En = "Users"
usersDefiniendumSnippet Hu = "Felhaszn??l??k"

errorFixHintHeadingSnippet :: IsString string => Language -> string
errorFixHintHeadingSnippet En = "Possible help hints to fix the error:"
errorFixHintHeadingSnippet Hu = "Lehets??ges seg??ts??g:"

emptyDataErrorCaseHeadingSnippet :: IsString string => Language -> string
emptyDataErrorCaseHeadingSnippet En = "In case of empty data error:"
emptyDataErrorCaseHeadingSnippet Hu = "??res adat hiba eset??n nincs k??l??n??sebb tennival??d:"

disjunctionWord1Snippet :: IsString string => Language -> string
disjunctionWord1Snippet En = "either"
disjunctionWord1Snippet Hu = "vagy"

disjunctionWord2Snippet :: IsString string => Language -> string
disjunctionWord2Snippet En = "or"
disjunctionWord2Snippet Hu = "vagy"

tooStrictFilteringHintSnippet :: IsString string => Language -> string
tooStrictFilteringHintSnippet En = "maybe You have set the filtering options too strict,"
tooStrictFilteringHintSnippet Hu = "enyh??ts a sz??r??si felt??teleken!"

emptyDatabaseHintSnippet :: IsString string => Language -> string
emptyDatabaseHintSnippet En = "or the database has not yet been filled with data"
emptyDatabaseHintSnippet Hu = "v??rj az adatb??zis nagyobb felt??lt??tts??g??re,"

inconsistentTraversalErrorCaseHeadingSnippet :: IsString string => Language -> string
inconsistentTraversalErrorCaseHeadingSnippet En = "In case of inconsistent traversal of the site error: probably You have opened a practice and it got interrupted without closing due to some forced traversal."
inconsistentTraversalErrorCaseHeadingSnippet Hu = "Bej??r??si k??vetkezetlen??g, lez??ratlanul maradt cselekm??ny eset??n"

closeFixButtonLabelSnippet :: IsString string => Language -> string
closeFixButtonLabelSnippet En = "click here to close."
closeFixButtonLabelSnippet Hu = "z??rd be az esetleg szab??lytalanul megszak??tott (lez??r??s n??lk??l) f??lbe maradt ??j gyakolatot."

noProblemTextSnippet :: IsString string => Language -> string
noProblemTextSnippet En = "There is also a chance that no action has to be done: the error is temporary, You can use the site without any worries."
noProblemTextSnippet Hu = "El??fordulhat, hogy nem kell semmit csin??lnod: haszn??ld nyugodtan tov??bb az oldalt."
