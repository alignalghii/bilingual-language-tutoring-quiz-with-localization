{-# LANGUAGE NamedFieldPuns #-}

module BilingualPractice.Model.RelationalBusinessLogic where

import BilingualPractice.Model.Grammar.Numeral (numerals_en, numerals_hu)
import Data.Property (matchField)
import Data.Time (UTCTime)
import Data.ListX (maybeHead, filterIt_unsafe)
import Data.List (zipWith4, (\\))
import Data.Bool (bool)


data LinguisticalUnit = LUNumber | LUWord | LUPhrase | LUSentence deriving (Eq, Read, Show, Bounded, Enum)

data Difficulty = Easy | MiddleLevel | Difficult deriving (Eq, Read, Show, Bounded, Enum)

data LexiconEntry = LxcE {hu, en :: String, entity :: LinguisticalUnit, difficulty :: Difficulty} deriving (Read, Show) -- Eq

numeralsRelation :: [LexiconEntry]
numeralsRelation = zipWith4 LxcE numerals_hu numerals_en  (repeat LUNumber) (repeat Easy)

 -- @TODO use a multi-value approach like >>= and use `findCorrectTranslations` instead:
 -- it should be more roboust to potential later lexicon streamlingings!
findCorrectTranslation :: [LexiconEntry] -> String -> LexiconEntry
findCorrectTranslation = flip $ filterIt_unsafe . matchField hu --head $ findCorrectTranslations lexicon sameHu

--findCorrectTranslations :: [LexiconEntry] -> String -> [LexiconEntry]
--findCorrectTranslations lexicon sameHu = filter (matchField hu sameHu) lexicon


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
conferPracticeCertificate lexicon personalAnswers = diffingTimes $ map (conferAnswer lexicon) personalAnswers

--pairingUp :: AnsweredQuestion -> LexiconEntry -> QuestionAnswerMatch
--pairingUp AnsQu {ansHu, ansEn, qst1Time, ansTime} LxcE {hu, en, entity, difficulty} = QuAnsMtch {dictHu = hu, dictEn = en, yourEn = ansEn, mark = ansEn == en, askedAtTime = qst1Time, answeredAtTime = ansTime, dictEntity = entity, dictDifficulty = difficulty}

conferAnswer :: [LexiconEntry] -> AnsweredQuestion -> QuestionAnswerMatch
conferAnswer lexicon AnsQu {ansHu, ansEn, qst1Time, ansTime} = let LxcE {hu, en, entity, difficulty} = findCorrectTranslation lexicon ansHu
                                                               in QuAnsMtch {dictHu = hu, dictEn = en, yourEn = ansEn, mark = ansEn == en, askedAtTime = qst1Time, answeredAtTime = ansTime, dictEntity = entity, dictDifficulty = difficulty}

--conferAnswers :: [LexiconEntry] -> AnsweredQuestion -> [QuestionAnswerMatch]
--conferAnswers lexicon answer = pairingUp answer <$> findCorrectTranslations lexicon (ansHu answer)

diffingTimes :: [QuestionAnswerMatch] -> [QuestionAnswerMatch]
diffingTimes []       = []
diffingTimes (m : ms) = m : diffingTimes (map (\m' -> m' {askedAtTime = answeredAtTime m}) ms)


data Practice = Prc {prcStartTime :: UTCTime, isOpen :: Bool} deriving (Read, Show)


data Session = Sssn {etalon :: [LexiconEntry], personal :: [AnsweredQuestion], maybePracticeStart :: Maybe UTCTime} deriving (Read, Show)


restoreEtalonByAnswers :: [AnsweredQuestion] -> [LexiconEntry] -> [LexiconEntry]
restoreEtalonByAnswers answers lexicon = findCorrectTranslation lexicon . ansHu <$> answers
-- filter $ flip elem (map ansHu answers-) . hu         -- does not keep the order of answers
-- answers >>= findCorrectTranslations lexicon . ansHu  -- unituitive duplications

answersOfPracticeStart :: UTCTime -> [AnsweredQuestion] -> [AnsweredQuestion]
answersOfPracticeStart = filter . matchField qst1Time
