{-# LANGUAGE OverloadedStrings #-}

{-|
Module       : Yu.Utils.Handler
Description  : The some methods used in server
Copyright    : (C) Qinka, 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknown

The method which might be useful for server
-}

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


module Yu.Utils.Handler
  ( -- * about exception
    catchH
  , handlerH
  , tryH
  , -- * about functor and moand
    (<#>)
  , (<%>)
  , -- * return the exception
    returnE
  , returnET
  , returnEH
  , -- * others
    showJs
  , fromBinToBytestr
  , LogPath(..)
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either
import           Database.MongoDB
import           Text.Julius            (RawJavascript, rawJS)
import           Yesod.Core
import qualified Yu.Import.ByteString   as B
import qualified Yu.Import.Text         as T


-- | The method to catch the exception (in HadnlerT)
catchH :: Exception e
       => HandlerT site IO a        -- ^ the action
       -> (e -> HandlerT site IO a) -- ^ the exception handler
       -> HandlerT site IO a        -- ^ return
catchH m h = handlerToIO >>=
  (\hio -> liftIO $ catch (hio m) (hio.h))

-- | The method for handler, equal to @flip catchH@
handlerH :: Exception e
         => (e -> HandlerT site IO a) -- ^ the handler for exceptions
         -> HandlerT site IO a -- ^ operation
         -> HandlerT site IO a
handlerH = flip catchH

-- | The method for a try
tryH :: Exception e
     => HandlerT site IO a            -- ^ action
     -> HandlerT site IO (Either e a) -- ^ return the result or the exception
tryH m = handlerToIO >>= (\hio -> liftIO . try $ hio m)

-- | the operator just like <$>
infixl 4 <#>, <%>
(<#>) :: (Functor f1,Functor f2)
      => (a -> b)             -- ^ func
      -> f1 (f2 a)            -- ^ item
      -> f1 (f2 b)            -- ^ eq to (func <$>) <$> item
(<#>) f = ((f <$>) <$>)
(<%>) :: (Monad m, Functor f)
      => (a -> m b)           -- ^ func
      -> f (m a)              -- ^ item
      -> f (m b)              -- ^ eq to (func =<<) <$> item
(<%>) f = ((f =<<) <$>)

-- | a method to return a ``exception'' when it catched
returnE :: (Monad m,Exception e)
         => e                         -- ^ the exception
         -> m String                  -- ^ return as string
returnE = pure . (\str -> "{\"error\":\"exception\",\"context\":\"" ++ str ++ "\"}") . show

-- | the text-returned version for returnE
returnET :: (Monad m,Exception e)
           => e -- ^ exception
           -> m T.Text
returnET = (fmap T.pack) . returnE

-- | the HandlerT version for returnE and returnEt
returnEH :: SomeException -- ^ exception
           -> HandlerT site IO TypedContent
returnEH e = returnE e >>=
  (\str -> respondSource "application/json" $ do
      sendChunk str
      sendFlush
  )

-- | show the js
showJs :: Show a => a -> RawJavascript
showJs = rawJS . T.show


-- | from binary to bytestring
fromBinToBytestr :: Binary -> B.ByteString
fromBinToBytestr (Binary x) = x

-- | the path for logger
data LogPath = LogFile FilePath -- ^ using files
             | LogStdout        -- ^ using stdout
             | LogStderr        -- ^ using stderr

instance FromJSON LogPath where
  parseJSON (Yesod.Core.String v) = pure $ case T.toLower v of
    "stdout" -> LogStdout
    "stderr" -> LogStderr
    _        -> LogFile $ T.unpack v


-- | instance the error response to json
instance ToJSON ErrorResponse where
  toJSON NotFound =
    object ["error" .= ("not found" ::String)]
  toJSON (InternalError e) =
    object [ "error" .= ("internal error"::String)
           , "content" .= e
           ]
  toJSON (InvalidArgs es) =
    object [ "error" .= ("invalid args"::String)
           , "content" .= es
           ]
  toJSON NotAuthenticated =
    object ["error" .= ("not authenticated!"::String)]
  toJSON (PermissionDenied msg) =
    object [ "error" .= ("permission denied"::String)
           , "content" .= msg
           ]
  toJSON (BadMethod m) =
    object [ "error" .= ("bad method" :: String)
           , "content" .= show m
           ]

