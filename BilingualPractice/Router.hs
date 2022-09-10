{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.Router where

import BilingualPractice.Language (Language (..))
import BilingualPractice.Controller.HomeController (homeAction, dumpAction, randAction, errorAction)
import BilingualPractice.Controller.PracticeController (proposeExamenAction, performExamenAction, restartPracticeAction, indexPracticeAction, showPracticeAction, closePracticeAction, deletePracticeAction, repeatPracticeAction)
import BilingualPractice.Controller.QuestionController (poseFirstRemainingExamenQuestionOrAnounceResultAction, receiveAnswerForQuestion)
import BilingualPractice.BuiltinServer (builtinServerOptions)
import Framework.Form (formParam)
import Web.Scotty (ScottyM, middleware, ActionM, get, post, Param, params, RoutePattern)
import Data.ReflectionX (invertFunction)


router :: Bool -> Language -> ScottyM ()
router logFlag lang = do
    mapM_ middleware $ builtinServerOptions logFlag
    getWith lang  "/"    homeAction
    getWith lang "/dump" dumpAction
    getWith lang "/rand" randAction
    getWith lang "/practice/index"     indexPracticeAction
    getWith lang "/practice/show/:utc" showPracticeAction
    getWith lang "/practice/new"       proposeExamenAction
    postWith lang "/practice/new"      performExamenAction
    postWith lang "/practice/restart"  restartPracticeAction
    postWith lang "/practice/delete"   deletePracticeAction
    postWith lang "/practice/repeat"   repeatPracticeAction
    postWith lang "/practice/closefix" closePracticeAction
    getWith lang "/question" poseFirstRemainingExamenQuestionOrAnounceResultAction
    postWith lang "/question"          receiveAnswerForQuestion

    getWith lang  "/error/navigationinconsistency" $ flip errorAction "Inconsistent traversal of the site: probably You have opened a practice and it got interrupted without closing due to some forced traversal."
    getWith  lang "/error/emptydata" $ flip errorAction "There are no data!"
    --post "/error/closepractice" $ errorClosePracticeAction

-- Important note:
-- entering examenAction reloads the etalon table and empties the personal table,
-- so this should not be a get (a get must lack secondary effects)
-- that is why examen is a post

routeCommandWithLang :: (url -> ActionM a -> scottyDoing) -> Language -> url -> (Language -> ActionM a) -> scottyDoing
routeCommandWithLang command lang url actionWithLang = command url $ withDetectLangDefaulting lang actionWithLang

getWith, postWith :: Language -> RoutePattern -> (Language -> ActionM ()) -> ScottyM ()
getWith  = routeCommandWithLang get
postWith = routeCommandWithLang post

withDetectLangDefaulting :: Language -> (Language -> ActionM a) -> ActionM a
withDetectLangDefaulting lang paramAction = do
    paramNameValuePairs <- params
    ifWithLanguageParamThenElse paramNameValuePairs paramAction (paramAction lang)

ifWithLanguageParamThenElse :: [Param] -> (Language -> ActionM a) -> ActionM a -> ActionM a
ifWithLanguageParamThenElse paramNameValuePairs paramAction defaultAction = maybe defaultAction paramAction $ lookup "lang" paramNameValuePairs >>= invertFunction formParam
