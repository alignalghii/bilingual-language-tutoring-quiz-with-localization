{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.HomeController where

import Framework.Controller (blaze)
import BilingualPractice.Model.TableManipulationForBusinessLogic (readExtendedLexiconTable, closePracticeStart)
import BilingualPractice.View.Home.HomeView     (homeView)
import BilingualPractice.View.Home.DumpView     (dumpView)
import BilingualPractice.View.Home.RandView     (randView)
import BilingualPractice.View.Home.ErrorView    (errorView)
import System.RandomX (randQuery)
import Web.Scotty (ActionM, redirect)
import Control.Monad.Trans (liftIO)


homeAction :: ActionM ()
homeAction = blaze homeView

dumpAction :: ActionM ()
dumpAction = liftIO readExtendedLexiconTable >>= (blaze . dumpView)

randAction :: ActionM ()
randAction = liftIO (readExtendedLexiconTable >>= randQuery 10) >>= (blaze . randView)

errorAction :: String -> ActionM ()
errorAction = blaze . errorView

--errorClosePracticeAction :: ActionM ()
--errorClosePracticeAction = do
--    liftIO closePracticeStart
--    redirect "/"
