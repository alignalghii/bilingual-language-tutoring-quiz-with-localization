{-# LANGUAGE NamedFieldPuns #-}

module BilingualPractice.Model.RelationalBusinessLogic where

import BilingualPractice.Model.Grammar.Numeral (numerals_en, numerals_hu)
import Data.Time (UTCTime)
import Data.ListX (maybeHead)
import Data.List (zipWith4, (\\))
import Data.Bool (bool)


data LinguisticalUnit = LUNumber | LUWord | LUSentence deriving (Eq, Read, Show, Bounded, Enum)

data Difficulty = Easy | Difficult deriving (Eq, Read, Show, Bounded, Enum)

data LexiconEntry = LxcE {hu, en :: String, entity :: LinguisticalUnit, difficulty :: Difficulty} deriving (Read, Show) -- Eq

numeralsRelation :: [LexiconEntry]
numeralsRelation = zipWith4 LxcE numerals_hu numerals_en  (repeat LUNumber) (repeat Easy)

findYourTranslation :: String -> [AnsweredQuestion] -> AnsweredQuestion
findYourTranslation sameHu = head . filter ((== sameHu) . ansHu)

data AnsweredQuestion = AnsQu {ansHu, ansEn :: String, qst1Time, ansTime :: UTCTime} deriving (Read, Show) -- Eq

data QuestionAnswerMatch = QuAnsMtch {dictHu, dictEn, yourEn :: String, mark :: Bool, askedAtTime, answeredAtTime :: UTCTime, dictEntity :: LinguisticalUnit, dictDifficulty :: Difficulty}

-- Governing a practice by the remaining questions:

withFirstUnansweredQuestionIfAnyOrElse :: (String -> a) -> ([LexiconEntry] -> [AnsweredQuestion] -> a) -> [LexiconEntry] -> [AnsweredQuestion] -> a
withFirstUnansweredQuestionIfAnyOrElse ask summarize etalon personal = maybe (summarize etalon personal)
                                                                             ask
                                                                             (maybeFirstUnansweredQuestion etalon personal)

maybeFirstUnansweredQuestion :: [LexiconEntry] -> [AnsweredQuestion] -> Maybe String
maybeFirstUnansweredQuestion etalon personal = let etalon_questions     = map hu etalon
                                                   answered_questions   = map ansHu personal
                                                   unanswered_questions = etalon_questions \\ answered_questions
                                               in maybeHead unanswered_questions

-- Summarizing a practice result into a user-readable certificate:

conferPracticeCertificate :: [LexiconEntry] -> [AnsweredQuestion] -> [QuestionAnswerMatch]
conferPracticeCertificate etalon personal = diffingTimes $ map (conferAnswer personal) etalon

conferAnswer :: [AnsweredQuestion] -> LexiconEntry -> QuestionAnswerMatch
conferAnswer personal LxcE {hu, en, entity, difficulty} = let AnsQu {ansEn, qst1Time, ansTime} = findYourTranslation hu personal
                                                              mark   = en == ansEn
                                                          in QuAnsMtch {dictHu = hu, dictEn = en, yourEn = ansEn, mark, askedAtTime = qst1Time, answeredAtTime = ansTime, dictEntity = entity, dictDifficulty = difficulty}

diffingTimes :: [QuestionAnswerMatch] -> [QuestionAnswerMatch]
diffingTimes []       = []
diffingTimes (m : ms) = m : diffingTimes (map (\m' -> m' {askedAtTime = answeredAtTime m}) ms)


data Practice = Prc {prcStartTime :: UTCTime, isOpen :: Bool} deriving (Read, Show)
