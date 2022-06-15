{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.View.Question.QuestionView (questionView) where

import BilingualPractice.Model.Grammar.Numeral (ordinalSuffix_en, singularOrPluralSuffix_en')
import Prelude hiding (head, span)
import Text.Blaze.Html5 as H hiding (map, mark)
import Text.Blaze.Html5.Attributes as HA hiding (title, form, span, label)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Time


questionView :: Int -> Int -> String -> Html
questionView nth ofAll hu = docTypeHtml ! lang "en" $ do
    head $ do
        meta ! charset "UTF-8"
        link ! rel "icon" ! href "/img/favicon.ico"
        link ! rel "stylesheet" ! href "/style/form.css"
        title "Hungarian-English word and sentence practice quiz-sets — Question"
    body $ do
        h1 "Hungarian-English word and sentence practice quiz-sets — Question"
        p $ do
            form ! method "post" ! action "practice/closefix?redir1=practice&redir2=new" ! class_ "inline" $
                button ! type_ "submit" $ "Quit recent practice and restart it anew, delete all Your Former answers to this recent practice"
            span " •|||• "
            form ! method "post" ! action "practice/closefix" ! class_ "inline" $
                button ! type_ "submit" $ "Back to the main page"
        p $ do
            span $ toHtml $ "The " ++ show nth
            sup $ toHtml $ ordinalSuffix_en nth
            span $ toHtml $ "question from the " ++ singularOrPluralSuffix_en' ofAll "question" ++ ":"
        form ! action "/question" ! method "post" $ do
            label "In Hungarian:"
            span $ toHtml hu
            br
            label "In English:"
            input ! type_ "hidden" ! name "hu" ! value (toValue hu)
            input ! type_ "text"   ! name "en" ! autofocus ""
            button ! type_ "submit" $ "Go!"
