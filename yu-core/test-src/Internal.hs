--
--  This file is part of Yu.
--
--  Yu is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Yu is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Yu.  If not, see <http://www.gnu.org/licenses/>.
--

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

