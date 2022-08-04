{-# LANGUAGE NamedFieldPuns #-}

module BilingualPractice.Model.ViewModel where

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, QuestionAnswerMatch (..), LinguisticalUnit (..), Difficulty (..), Practice (..), AnsweredQuestion (..), conferPracticeCertificate)
import Framework.Form (FormParamable (formParam))
import Data.TimeX (abbrevTime, keepDateAbbrevTime')
import Data.Time (UTCTime, TimeZone)
import Data.Property (matchField)
import Data.Bool (bool)


class Viewable a where
    view :: a -> String

instance Viewable LinguisticalUnit where
    view LUNumber   = "number"
    view LUWord     = "word"
    view LUPhrase   = "phrase"
    view LUSentence = "sentence"

instance Viewable Difficulty where
    view Easy        = "easy"
    view MiddleLevel = "middle-level"
    view Difficult   = "difficult"


instance FormParamable LinguisticalUnit where
    formParam LUNumber   = "number"
    formParam LUWord     = "word"
    formParam LUPhrase   = "phrase"
    formParam LUSentence = "sentence"

instance FormParamable Difficulty where
    formParam Easy        = "easy"
    formParam MiddleLevel = "middleLevel"
    formParam Difficult   = "difficult"


data QuestionAnswerMatchView = QuAnsMtchVw {dictHuView, dictEnView, yourEnView :: String, markView :: (String, String), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView :: String}

viewMatch :: TimeZone -> QuestionAnswerMatch -> QuestionAnswerMatchView
viewMatch timeZone QuAnsMtch {dictHu, dictEn, yourEn, mark, askedAtTime, answeredAtTime, dictEntity, dictDifficulty} = QuAnsMtchVw {dictHuView = dictHu, dictEnView = dictEn, yourEnView = yourEn, markView = viewMark mark, askedAtTimeView = keepDateAbbrevTime' timeZone askedAtTime, answeredAtTimeView = keepDateAbbrevTime' timeZone answeredAtTime, dictEntityView = view dictEntity, dictDifficultyView = view dictDifficulty}


viewMark :: Bool -> (String, String)
viewMark = bool ("Wrong", "wrong") ("OK", "ok")


data PracticeView = PrcVw {prcStartTimeId :: UTCTime, prcStartTimeView :: String, questionsCount :: Int}

viewPractice :: TimeZone -> [AnsweredQuestion] -> Practice -> PracticeView
viewPractice timeZone answers Prc {prcStartTime} = PrcVw {prcStartTimeId = prcStartTime, prcStartTimeView = keepDateAbbrevTime' timeZone prcStartTime, questionsCount = length $ filter (matchField qst1Time prcStartTime) answers}

conferAndViewCertificate :: TimeZone -> [LexiconEntry] -> [AnsweredQuestion] -> [QuestionAnswerMatchView]
conferAndViewCertificate timeZone lexicon personalAnswers = viewMatch timeZone <$> conferPracticeCertificate lexicon personalAnswers
