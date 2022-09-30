{-# LANGUAGE OverloadedStrings #-}

module BilingualPractice.Model.EstablishBackendAndConnection (runConnection) where

import Database.Persist.Sqlite (runSqlite, SqlBackend)

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Text (Text)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Logger (NoLoggingT)
import           Conduit (ResourceT)


runSqlite' :: MonadUnliftIO m => Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runSqlite' = runSqlite

runConnection :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runConnection = runSqlite' ":memory:"
