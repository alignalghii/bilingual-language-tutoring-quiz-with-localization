{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module BilingualPractice.Model.ViewModel where

import BilingualPractice.Language (Language (..))
import Data.String (IsString)

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, QuestionAnswerMatch (..), LinguisticalUnit (..), Difficulty (..), Practice (..), AnsweredQuestion (..), conferPracticeCertificate)
import Framework.Form (FormParamable (formParam))
import Data.TimeX (abbrevTime, keepDateAbbrevTime')
import Data.Time (UTCTime, TimeZone)
import Data.Property (matchField)
import Data.Bool (bool)


class Viewable a where
    view :: IsString string => a -> Language -> string

instance Viewable LinguisticalUnit where
    view LUNumber   En = "number"
    view LUNumber   Hu = "szám"
    view LUWord     En = "word"
    view LUWord     Hu = "szó"
    view LUPhrase   En = "phrase"
    view LUPhrase   Hu = "szókapcsolat"
    view LUSentence En = "sentence"
    view LUSentence Hu = "mondat"

instance Viewable Difficulty where
    view Easy        En = "easy"
    view Easy        Hu = "könnyű"
    view MiddleLevel En = "middle-level"
    view MiddleLevel Hu = "középszintű"
    view Difficult   En = "difficult"
    view Difficult   Hu = "nehéz"

instance Viewable Language where
    view En En = "English"
    view En Hu = "angol"
    view Hu En = "Hungarian"
    view Hu Hu = "magyar"


instance FormParamable LinguisticalUnit where
    formParam LUNumber   = "number"
    formParam LUWord     = "word"
    formParam LUPhrase   = "phrase"
    formParam LUSentence = "sentence"

instance FormParamable Difficulty where
    formParam Easy        = "easy"
    formParam MiddleLevel = "middleLevel"
    formParam Difficult   = "difficult"

instance FormParamable Language where



data QuestionAnswerMatchView = QuAnsMtchVw {dictHuView, dictEnView, yourEnView :: String, markView :: (String, String), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView :: String}

viewMatch :: Language -> TimeZone -> QuestionAnswerMatch -> QuestionAnswerMatchView
viewMatch lang timeZone QuAnsMtch {dictHu, dictEn, yourEn, mark, askedAtTime, answeredAtTime, dictEntity, dictDifficulty} = QuAnsMtchVw {dictHuView = dictHu, dictEnView = dictEn, yourEnView = yourEn, markView = viewMark mark, askedAtTimeView = keepDateAbbrevTime' timeZone askedAtTime, answeredAtTimeView = keepDateAbbrevTime' timeZone answeredAtTime, dictEntityView = view dictEntity lang, dictDifficultyView = view dictDifficulty lang}


viewMark :: Bool -> (String, String)
viewMark = bool ("Wrong", "wrong") ("OK", "ok")


data PracticeView = PrcVw {prcStartTimeId :: UTCTime, prcStartTimeView :: String, questionsCount :: Int}

viewPractice :: TimeZone -> [AnsweredQuestion] -> Practice -> PracticeView
viewPractice timeZone answers Prc {prcStartTime} = PrcVw {prcStartTimeId = prcStartTime, prcStartTimeView = keepDateAbbrevTime' timeZone prcStartTime, questionsCount = length $ filter (matchField qst1Time prcStartTime) answers}

conferAndViewCertificate :: Language -> TimeZone -> [LexiconEntry] -> [AnsweredQuestion] -> [QuestionAnswerMatchView]
conferAndViewCertificate lang timeZone lexicon personalAnswers = viewMatch lang timeZone <$> conferPracticeCertificate lexicon personalAnswers
