{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module BilingualPractice.Controller.HomeController where

import BilingualPractice.Language (Language)
import Framework.Controller (blaze)
import BilingualPractice.Model.Error (Error)
import BilingualPractice.Model.TableManipulationForBusinessLogic (readExtendedLexiconTable, closePracticeStart)
import BilingualPractice.View.Home.HomeView     (homeView)
import BilingualPractice.View.Home.DumpView     (dumpView)
import BilingualPractice.View.Home.RandView     (randView)
import BilingualPractice.View.Home.ErrorView    (errorView)
import System.RandomX (randQuery)
import Web.Scotty (ActionM, redirect)
import Text.Blaze.Html5 (AttributeValue)
import Control.Monad.Trans (liftIO)


homeAction :: Language -> AttributeValue -> ActionM ()
homeAction lang = blaze . homeView lang

dumpAction :: Language -> AttributeValue -> ActionM ()
dumpAction lang selfUrl = liftIO readExtendedLexiconTable >>= (blaze . dumpView lang selfUrl)

randAction :: Language -> AttributeValue -> ActionM ()
randAction lang selfUrl = liftIO (readExtendedLexiconTable >>= randQuery 10) >>= (blaze . randView lang selfUrl)

errorAction :: Error -> Language -> AttributeValue -> ActionM ()
errorAction err lang = blaze . errorView err lang

--errorClosePracticeAction :: ActionM ()
--errorClosePracticeAction = do
--    liftIO closePracticeStart
--    redirect "/"
