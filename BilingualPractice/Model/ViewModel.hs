{-# LANGUAGE NamedFieldPuns #-}

module BilingualPractice.Model.ViewModel where

import BilingualPractice.Model.RelationalBusinessLogic (QuestionAnswerMatch (..), LinguisticalUnit (..), Difficulty (..), Practice (..), AnsweredQuestion (..))
import Framework.Form (FormParamable (formParam))
import Data.TimeX (abbrevTime, keepDateAbbrevTime')
import Data.Time (TimeZone)
import Data.Property (matchField)
import Data.Bool (bool)


class Viewable a where
    view :: a -> String

instance Viewable LinguisticalUnit where
    view LUNumber   = "szám"
    view LUWord     = "szó"
    view LUSentence = "mondat"

instance Viewable Difficulty where
    view Easy      = "könnyű"
    view Difficult = "nehéz"


instance FormParamable LinguisticalUnit where
    formParam LUNumber   = "number"
    formParam LUWord     = "word"
    formParam LUSentence = "sentence"

instance FormParamable Difficulty where
    formParam Easy      = "easy"
    formParam Difficult = "difficult"


data QuestionAnswerMatchView = QuAnsMtchVw {dictHuView, dictEnView, yourEnView :: String, markView :: (String, String), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView :: String}

viewMatch :: TimeZone -> QuestionAnswerMatch -> QuestionAnswerMatchView
viewMatch timeZone QuAnsMtch {dictHu, dictEn, yourEn, mark, askedAtTime, answeredAtTime, dictEntity, dictDifficulty} = QuAnsMtchVw {dictHuView = dictHu, dictEnView = dictEn, yourEnView = yourEn, markView = viewMark mark, askedAtTimeView = keepDateAbbrevTime' timeZone askedAtTime, answeredAtTimeView = keepDateAbbrevTime' timeZone answeredAtTime, dictEntityView = view dictEntity, dictDifficultyView = view dictDifficulty}


viewMark :: Bool -> (String, String)
viewMark = bool ("Rossz", "wrong") ("Jó", "ok")


data PracticeView = PrcVw {prcStartTimeView :: String, questionsCount :: Int}

viewPractice :: TimeZone -> [AnsweredQuestion] -> Practice -> PracticeView
viewPractice timeZone answers Prc {prcStartTime} = PrcVw {prcStartTimeView = keepDateAbbrevTime' timeZone prcStartTime, questionsCount = length $ filter (matchField qst1Time prcStartTime) answers}
