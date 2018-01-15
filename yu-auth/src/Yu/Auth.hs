{-

 Copyright (C) 2017-2018 Johann Lee <me@qinka.pro>

 This file is part of Yu.

 Yu is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 Yu is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with Yu.  If not, see <http://www.gnu.org/licenses/>.

-}

{-|
Module:       Yu.Auth
Description:  The methods for authentication
Copyright:    (C) 2017-2018 Johann Lee <me@qinka.pro>
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The methods about authentication for Yu (using Yesod)
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Yu.Auth
  ( -- * check whether it is authorized
    checkAuth
  , -- * Type Class to limit
    Auth(..)
  , -- * reexport module
    module Yu.Auth.Core
  ) where

import           Control.Monad.IO.Class
import           Yesod.Core
import           Yu.Auth.Core
import           Yu.Import.ByteString   (ByteString)
import qualified Yu.Import.ByteString   as B
import qualified Yu.Import.Text as T
import Control.Applicative

-- | The class to limit the an application with it hash algorithm
--   and the password of the site.
class HashAlgorithm a => Auth site a | site -> a where
  -- | to get the HASH
  tokenHash :: MonadIO m => site -> m a
  -- | to get the password
  tokenItem :: MonadIO m => site -> m ByteString

-- | check auth
checkAuth :: Auth site hash
          => HandlerT site IO AuthResult -- ^ return result
checkAuth = do -- Handler _ IO
  site  <- getYesod
  item  <- tokenItem site
  hash  <- tokenHash site
  token <- fetchToken
  liftIO $ print $ generateHash hash item
  liftIO $ print token
  case verifyHash hash item <$> token of
    -- success
    Just True -> return   Authorized
    -- failure
    _         -> return $ Unauthorized "Who are you! The thing did not answer."


-- | fetch 
fetchToken :: Auth site hash
           => HandlerT site IO (Maybe ByteString)
fetchToken = do
  ct <- fmap T.encodeUtf8 <$> lookupCookie     "Token"
  gt <- fmap T.encodeUtf8 <$> lookupGetParam   "Token"
  pt <- fmap T.encodeUtf8 <$> lookupPostParam  "Token"
  ht <-                  lookupHeader     "Token"
  ca <- fmap T.encodeUtf8 <$> lookupCookie     "Authorization"
  ga <- fmap T.encodeUtf8 <$> lookupGetParam   "Authorization"
  pa <- fmap T.encodeUtf8 <$> lookupPostParam  "Authorization"
  ha <-                  lookupHeader     "Authorization"
  return $ ca <|> ct <|> pa <|> pt <|> ga <|> gt <|> ha <|> ht
