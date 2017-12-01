{-# LANGUAGE OverloadedStrings #-}

module Internal
  ( module Database.MongoDB
  , module Control.Exception
  , runDB
  , testDBName
  ) where


import           Control.Exception
import           Database.MongoDB
import           System.Environment
import           System.IO.Error    (catchIOError)
import qualified Yu.Import.Text     as T


-- | envrionment variable for mongo db url
mongoHostEnvVar :: String
mongoHostEnvVar = "MONGODB_URL"

-- | test database name
testDBName :: T.Text
testDBName = "test"

-- | run db action
runDB :: Action IO a -> IO a
runDB action = do
    mongodbHost <- getEnv mongoHostEnvVar `catchIOError` (\_ -> return "localhost")
    pipe <- connect (readHostPort mongodbHost)
    result <- access pipe master testDBName action
    close pipe
    return result

