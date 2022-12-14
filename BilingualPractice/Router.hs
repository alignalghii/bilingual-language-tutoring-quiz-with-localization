{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.Router where

import BilingualPractice.Language (Language (..))
import BilingualPractice.Model.Error (Error (..))
import BilingualPractice.Controller.HomeController (homeAction, dumpAction, randAction, errorAction)
import BilingualPractice.Controller.PracticeController (proposeExamenAction, performExamenAction, restartPracticeAction, indexPracticeAction, showPracticeAction, closePracticeAction, deletePracticeAction, repeatPracticeAction)
import BilingualPractice.Controller.QuestionController (poseFirstRemainingExamenQuestionOrAnounceResultAction, receiveAnswerForQuestion)
import BilingualPractice.BuiltinServer (builtinServerOptions)
import Framework.Form (formParam)
import Framework.Url (Url, reqToUrl)
import Web.Scotty (ScottyM, middleware, ActionM, get, post, Param, params, RoutePattern, request)
import Network.Wai (Request)
import Text.Blaze.Html5 (AttributeValue)
import Data.ReflectionX (invertFunction)
import Data.String (fromString)


router :: Bool -> Language -> ScottyM ()
router logFlag lang = do
    mapM_ middleware $ builtinServerOptions logFlag
    getWith'' lang  "/"   homeAction
    getWith'' lang "/dump" dumpAction
    getWith'' lang "/rand" randAction
    getWith'' lang "/practice/index"     indexPracticeAction
    getWith'' lang "/practice/show/:utc" showPracticeAction
    getWith'' lang "/practice/new"       proposeExamenAction
    postWith lang "/practice/new"      performExamenAction
    postWith lang "/practice/restart"  restartPracticeAction
    postWith lang "/practice/delete"   deletePracticeAction
    postWith lang "/practice/repeat"   repeatPracticeAction
    postWith lang "/practice/closefix" closePracticeAction
    getWith'' lang "/question" poseFirstRemainingExamenQuestionOrAnounceResultAction
    postWith lang "/question"          receiveAnswerForQuestion

    getWith'' lang  "/error/navigationinconsistency" $ errorAction InconsistentTraversalError
    getWith'' lang "/error/emptydata" $ errorAction NoDataError
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

type UrlString = String

routeCommandWithLangAndSelf :: (RoutePattern -> ActionM a -> scottyDoing) -> Language -> UrlString -> (Language -> AttributeValue -> ActionM a) -> scottyDoing
routeCommandWithLangAndSelf command lang url actionWithLangAndSelf = command (fromString url) $ withDetectLangDefaulting lang (flip actionWithLangAndSelf $ fromString url)

getWith' :: Language -> UrlString -> (Language -> AttributeValue -> ActionM ()) -> ScottyM ()
getWith' = routeCommandWithLangAndSelf get

routeCommandWithLangAndSelf' :: (RoutePattern -> ActionM a -> scottyDoing) -> Language -> RoutePattern -> (Language -> Url -> ActionM a) -> scottyDoing
routeCommandWithLangAndSelf' command lang routePattern actionWithLangAndSelf = command routePattern $ withDetectLangDefaulting lang $ (detectSelfUrl >>=) . actionWithLangAndSelf

getWith'' :: Language -> RoutePattern -> (Language -> Url -> ActionM ()) -> ScottyM ()
getWith'' = routeCommandWithLangAndSelf' get

detectSelfUrl :: ActionM Url
detectSelfUrl = reqToUrl <$> request
