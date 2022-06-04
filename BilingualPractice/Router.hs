{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.Router where

import BilingualPractice.Controller.HomeController (homeAction, dumpAction, randAction)
import BilingualPractice.Controller.PracticeController (proposeExamenAction, performExamenAction, indexPracticeAction, showPracticeAction)
import BilingualPractice.Controller.QuestionController (poseFirstRemainingExamenQuestionOrAnounceResultAction, receiveAnswerForQuestion)
import BilingualPractice.BuiltinServer (builtinServerOptions)
import Web.Scotty (ScottyM, middleware, get, post)


router :: Bool -> ScottyM ()
router logFlag = do
    mapM_ middleware $ builtinServerOptions logFlag
    get  "/"         homeAction
    get  "/dump"     dumpAction
    get  "/rand"     randAction
    get  "/practice/index" indexPracticeAction
    get  "/practice/show/:utc" showPracticeAction
    get  "/practice/new"   proposeExamenAction
    post "/practice/new"   performExamenAction
    get  "/question" poseFirstRemainingExamenQuestionOrAnounceResultAction
    post "/question" receiveAnswerForQuestion

-- Important note:
-- entering examenAction reloads the etalon table and empties the personal table,
-- so this should not be a get (a get must lack secondary effects)
-- that is why examen is a post
