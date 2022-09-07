{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.Router where

import BilingualPractice.Language (Language (..))
import BilingualPractice.Controller.HomeController (homeAction, dumpAction, randAction, errorAction)
import BilingualPractice.Controller.PracticeController (proposeExamenAction, performExamenAction, restartPracticeAction, indexPracticeAction, showPracticeAction, closePracticeAction, deletePracticeAction, repeatPracticeAction)
import BilingualPractice.Controller.QuestionController (poseFirstRemainingExamenQuestionOrAnounceResultAction, receiveAnswerForQuestion)
import BilingualPractice.BuiltinServer (builtinServerOptions)
import Web.Scotty (ScottyM, middleware, ActionM, get, post, Param, params)


router :: Bool -> Language -> ScottyM ()
router logFlag lang = do
    mapM_ middleware $ builtinServerOptions logFlag
    get  "/"                   $ withDetectLangDefaulting lang homeAction
    get  "/dump"               $ withDetectLangDefaulting lang dumpAction
    get  "/rand"               $ withDetectLangDefaulting lang randAction
    get  "/practice/index"     $ withDetectLangDefaulting lang indexPracticeAction
    get  "/practice/show/:utc" $ withDetectLangDefaulting lang showPracticeAction
    get  "/practice/new"       $ withDetectLangDefaulting lang proposeExamenAction
    post "/practice/new"       $                               performExamenAction
    post "/practice/restart"   $                               restartPracticeAction
    post "/practice/delete"    $                               deletePracticeAction
    post "/practice/repeat"    $                               repeatPracticeAction
    post "/practice/closefix"  $                               closePracticeAction
    get  "/question"           $ withDetectLangDefaulting lang poseFirstRemainingExamenQuestionOrAnounceResultAction
    post "/question"           $                               receiveAnswerForQuestion

    get  "/error/navigationinconsistency" $ withDetectLangDefaulting lang $ flip errorAction "Inconsistent traversal of the site: probably You have opened a practice and it got interrupted without closing due to some forced traversal."
    get  "/error/emptydata" $ withDetectLangDefaulting lang $ flip errorAction "There are no data!"
    --post "/error/closepractice" $ errorClosePracticeAction

-- Important note:
-- entering examenAction reloads the etalon table and empties the personal table,
-- so this should not be a get (a get must lack secondary effects)
-- that is why examen is a post

withDetectLangDefaulting :: Language -> (Language -> ActionM a) -> ActionM a
withDetectLangDefaulting lang paramAction = do
    paramNameValuePairs <- params
    ifWithLanguageParamThenElse paramNameValuePairs paramAction (paramAction lang)

ifWithLanguageParamThenElse :: [Param] -> (Language -> ActionM a) -> ActionM a -> ActionM a
ifWithLanguageParamThenElse paramNameValuePairs paramAction defaultAction
    | ("lang", "en") `elem` paramNameValuePairs = paramAction En
    | ("lang", "hu") `elem` paramNameValuePairs = paramAction Hu
    | otherwise                                 = defaultAction
