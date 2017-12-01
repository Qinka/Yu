{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

{-|
Module:       Yu.Auth
Description:  The methods for authentication
Copyright:    (C) Qinka 2017
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The methods about authentication for Yu (using Yesod)
-}

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
  token <- lookupHeader "token"
  liftIO $ print $ generateHash hash item
  liftIO $ print token
  case verifyHash hash item <$> token of
    -- success
    Just True -> return   Authorized
    -- failure
    _         -> return $ Unauthorized "Who are you! The thing did not answer."

