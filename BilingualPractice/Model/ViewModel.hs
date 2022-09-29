{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module BilingualPractice.Model.ViewModel where

import BilingualPractice.Language (Language (..)) -- should become BilingualPractice.Model.Language ?
import BilingualPractice.Model.Error (Error (..))
import Data.String (IsString)

import BilingualPractice.Model.RelationalBusinessLogic (LexiconEntry, QuestionAnswerMatch (..), LinguisticalUnit (..), Difficulty (..), Practice (..), AnsweredQuestion, qst1Time, conferPracticeCertificate)
import Framework.Form (FormParamable (formParam))
import Data.TimeX (abbrevTime, keepDateAbbrevTime')
import Data.Time (UTCTime, TimeZone)
import Data.Property (matchField)
import Data.Bool (bool)
import Control.Arrow ((&&&))


class Viewable a where
    view :: IsString string => Language -> a -> string

instance Viewable LinguisticalUnit where
    view En LUNumber   = "number"
    view Hu LUNumber   = "szám"
    view En LUWord     = "word"
    view Hu LUWord     = "szó"
    view En LUPhrase   = "phrase"
    view Hu LUPhrase   = "szókapcsolat"
    view En LUSentence = "sentence"
    view Hu LUSentence = "mondat"

instance Viewable Difficulty where
    view En Easy        = "easy"
    view Hu Easy        = "könnyű"
    view En MiddleLevel = "middle-level"
    view Hu MiddleLevel = "középszintű"
    view En Difficult   = "difficult"
    view Hu Difficult   = "nehéz"

instance Viewable Language where
    view En En = "English"
    view Hu En = "angol"
    view En Hu = "Hungarian"
    view Hu Hu = "magyar"

instance Viewable Error where
    view En NoDataError = "There are no data!"
    view Hu NoDataError = "Nincsenek adatok!"
    view En InconsistentTraversalError = "Inconsistent traversal of the site: probably You have opened a practice and it got interrupted without closing due to some forced traversal."
    view Hu InconsistentTraversalError = "Az oldal bejárása során következetlenség történhetett (valami lezáratlanul maradt cselekmény)!"


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
    formParam En = "en"
    formParam Hu = "hu"


data QuestionAnswerMatchView = QuAnsMtchVw {dictHuView, dictEnView, yourEnView :: String, markViewAndStyle :: (String, String), askedAtTimeView, answeredAtTimeView, dictEntityView, dictDifficultyView :: String}

viewMatch :: Language -> TimeZone -> QuestionAnswerMatch -> QuestionAnswerMatchView
viewMatch lang timeZone QuAnsMtch {dictHu, dictEn, yourEn, mark, askedAtTime, answeredAtTime, dictEntity, dictDifficulty} = QuAnsMtchVw {dictHuView = dictHu, dictEnView = dictEn, yourEnView = yourEn, markViewAndStyle = viewAndStyleMark lang mark, askedAtTimeView = keepDateAbbrevTime' timeZone askedAtTime, answeredAtTimeView = keepDateAbbrevTime' timeZone answeredAtTime, dictEntityView = view lang dictEntity, dictDifficultyView = view lang dictDifficulty}

viewAndStyleMark :: Language -> Bool -> (String, String)
viewAndStyleMark lang = viewMark lang &&& styleMark

viewMark :: Language -> Bool -> String
viewMark En = bool "Wrong" "OK"
viewMark Hu = bool "Rossz" "Jó"

styleMark :: Bool -> String
styleMark = bool "wrong" "ok"

data PracticeView = PrcVw {prcStartTimeId :: UTCTime, prcStartTimeView :: String, questionsCount :: Int}

viewPractice :: TimeZone -> [AnsweredQuestion] -> Practice -> PracticeView
viewPractice timeZone answers (Practice prcStartTime _) = PrcVw {prcStartTimeId = prcStartTime, prcStartTimeView = keepDateAbbrevTime' timeZone prcStartTime, questionsCount = length $ filter (matchField qst1Time prcStartTime) answers}

conferAndViewCertificate :: Language -> TimeZone -> [LexiconEntry] -> [AnsweredQuestion] -> [QuestionAnswerMatchView]
conferAndViewCertificate lang timeZone lexicon personalAnswers = viewMatch lang timeZone <$> conferPracticeCertificate lexicon personalAnswers
