{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.View.Home.HomeView (homeView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, portfolioLinkTextSnippet)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.View.Helper (langLink)

import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span)
import Data.Bool (bool)

homeView :: Language -> Html
homeView language = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        title $ titleSnippet language
    body $ do
        h1 $ titleSnippet language
        ul $ do
            li $ do
                strong $ lexiconDefiniendumSnippet language
                span $ lexiconDefinitorSnippet language
                ul $ do
                    langLink language "/dump" wholeDumpLinkTextSnippet
                    span $ wholeDumpExplanationSnippet language
                    li $ do
                        langLink language "/rand" randomSelectionLinkTextSnippet
                        span $ randomSelectionExplanationSnippet language
            li $ do
                strong $ practicesDefiniendumSnippet language
                span $ practicesDefinitorSnippet language
                ul $ do
                    li $ do
                        langLink language "/practice/new" newPracticeLinkTextSnippet
                        span $ newPracticeExplanationSnippet language
                    li $ do
                        langLink language "/practice/index" indexPracticeLinkTextSnippet
                        span $ indexPracticeExplanationSnippet language
            li $ do
                strong $ usersDefiniendumSnippet language
        h2 $ userDocumentationHeaderSnippet language
        p $ userDocumentationParagraph1ContentSnippet language
        p $ userDocumentationParagraph2TextSnippet language
        h2 $ developerDocumentationHeaderSnippet language
        p $ developerDocumentationParagraph1ContentSnippet language
        p $ developerDocumentationParagraph2ContentSnippet language


titleSnippet :: IsString string => Language -> string
titleSnippet = appTitleSnippet

lexiconDefiniendumSnippet :: IsString string => Language -> string
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

userDocumentationHeaderSnippet :: IsString string => Language -> string
userDocumentationHeaderSnippet En = "Description for users"
userDocumentationHeaderSnippet Hu = "Tartalmi leírás"

userDocumentationParagraph1ContentSnippet :: Language -> Html
userDocumentationParagraph1ContentSnippet En = do
    span "The user can practice on quiz-like question sets: Hungarian words, phrases, or short sentences come up, and the user has to type in his/her attempt for an English translation as answer. The words, phrases, and sentences are randomly selected from the pre-uploaded"
    q "lexicon"
    span "— the user can customize this selection by filtering (on dificulty level, and also on whether sentences, phrases, or words should come up). Filtering can be issued in any possible combinations."
userDocumentationParagraph1ContentSnippet Hu = do
    span "A program egyszerű közvetlen begépelős rövid fordítási kérdéseket, gyakorlatsorokat ad fel véletlenszerűen egy feltölthető nagy"
    q "lexikon"
    span "-tananyagból, amely szavakat, szókapcsolatokat, mondatokat tartalmaz, nehézségi szint szerint is kategorizálhatóan, szűrhetően."

userDocumentationParagraph2TextSnippet :: IsString string => Language -> string
userDocumentationParagraph2TextSnippet En = "Each practice done by the user are saved automatically, the user can manage them: view again, repeat again, delete them. Later in futur e also some kind of cherry-picking, free assemblage of new practices will be added as feature, also statistics, maybe even multi-player contests."
userDocumentationParagraph2TextSnippet Hu = "A tanuló utólag is visszanézheti korább elvégzett gyakorlatait, tetszés szerint megismételheti őket."

developerDocumentationHeaderSnippet :: IsString string => Language -> string
developerDocumentationHeaderSnippet En = "Description for developers"
developerDocumentationHeaderSnippet Hu = "Technikai háttér"

developerDocumentationParagraph1ContentSnippet :: Language -> Html
developerDocumentationParagraph1ContentSnippet En = do
    span "Technically, this project is an challenge: to explore the usefulness of declarative paradigms in web programming, especially the possible gains provided by functional languges like Haskell. In details: the"
    q "Scotty"
    span "micro-framework is underlying the whole project. The whole sorce-code is available freely: "
    a ! href "https://github.com/alignalghii/bilingual-Hungarian-English-practice-quiz-app" ! target "_plain" $ "GitHub project page."
developerDocumentationParagraph1ContentSnippet Hu = do
    span "Szakmailag a program a Haskell nevű funkcionális programnyelve alkalmazhatóságának feltérképezésére szolgál a webprogramozás területén (ezen belül konkrétabban a"
    q "Scotty"
    span "mikro-keretrendszer használatát mutatja fel). Íme a projekt forráskódjának szabadon elérhető"
    a ! href "https://github.com/alignalghii/bilingual-Hungarian-English-practice-quiz-app" ! target "_plain" $ "GitHub-profilja."

developerDocumentationParagraph2ContentSnippet :: Language -> Html
developerDocumentationParagraph2ContentSnippet En = do
    span "I have also a personal portfolio page about both my past in Haskell (and its underlying mathematics), and also about my future visions: see"
    a ! href (portfolioLinkTextSnippet En)  ! target "_plain" $ "this profolio site"
    span "too."
developerDocumentationParagraph2ContentSnippet Hu = do
    span "Ennek a nyelvnek, technológiának, paradigmának a terén személyes múltam, vízióim"
    a ! href (portfolioLinkTextSnippet Hu)  ! target "_plain" $ "külön portfólióoldalon"
    span "szerepeplnek."
