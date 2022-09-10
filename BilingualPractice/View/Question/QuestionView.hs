{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Question.QuestionView (questionView) where

import BilingualPractice.View.CommonSnippets (appTitleSnippet, languageSelectionFlagBarSnippet, backHomeLinkTextSnippet, submitCommandSnippet)
import BilingualPractice.View.LanguageHelper (langAction)
import BilingualPractice.Model.ViewModel (Viewable (view))
import BilingualPractice.Model.Grammar.VocalAgreement (vocalAgreement2)
import BilingualPractice.Language (Language (..), languageAttrValue)
import Data.String (IsString)

import BilingualPractice.Model.Grammar.Numeral (ordinalSuffix_en, singularOrPluralSuffix_en')
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, mark)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Time


questionView :: Language -> AttributeValue -> Int -> Int -> String -> Html
questionView language selfUrl nth ofAll hu = docTypeHtml ! lang (languageAttrValue language) $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        title $ titleSnippet language
    body $ do
        languageSelectionFlagBarSnippet language selfUrl
        h1 $ titleSnippet language
        p $ do
            form ! method "post" ! langAction language "practice/closefix?redir1=practice&redir2=new" ! class_ "inline" $
                button ! type_ "submit" $ quitAndRestartPracticeCommandSnippet language
            span " •|||• "
            form ! method "post" ! langAction language "practice/closefix" ! class_ "inline" $
                button ! type_ "submit" $ backHomeLinkTextSnippet language
        p $ askQuestionSnippet language nth ofAll
        form ! langAction language "/question" ! method "post" $ do
            label $ toHtml $ inWhichLanguageSnippet language Hu
            span $ toHtml hu
            br
            label $ toHtml $ inWhichLanguageSnippet language En
            input ! type_ "hidden" ! name "hu" ! value (toValue hu)
            input ! type_ "text"   ! name "en" ! autofocus ""
            button ! type_ "submit" $ submitCommandSnippet language


-- Localization snippets:

titleSnippet :: (IsString string, Semigroup string) => Language -> string
titleSnippet En = appTitleSnippet En <> " — Question"
titleSnippet Hu = appTitleSnippet Hu <> " — Kérdés"

quitAndRestartPracticeCommandSnippet :: IsString string => Language -> string
quitAndRestartPracticeCommandSnippet En = "Quit recent practice and restart it anew, delete all Your Former answers to this recent practice"
quitAndRestartPracticeCommandSnippet Hu = "Vizsga újraindítása, eddigi eredmények feldolgozatlan törlése"

askQuestionSnippet :: Language -> Int -> Int -> Html
askQuestionSnippet En nth ofAll = do
    span $ toHtml $ "The " ++ show nth
    sup  $ toHtml $ ordinalSuffix_en nth
    span $ toHtml $ "question from the " ++ singularOrPluralSuffix_en' ofAll "question" ++ ":"
askQuestionSnippet Hu nth ofAll = toHtml $ show nth ++ ". kérdés (" ++ show ofAll ++ " kérdésből):"

inWhichLanguageSnippet :: Language -> Language -> String
inWhichLanguageSnippet En language = "In " <> view En language <> ":"
inWhichLanguageSnippet Hu language = vocalAgreement2 (view Hu language) ("ul", "ül") <> ":"
