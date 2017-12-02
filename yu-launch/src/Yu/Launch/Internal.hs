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

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Yu.Launch.Internal
       ( Xiao(..)
       , XiaoConfigServer(..)
       , XiaoConfigDatabase(..)
       , Route(..)
       , resourcesXiao
       ) where

import           Control.Monad.IO.Class
import           Data.Char
import           Data.String
import           Data.Version
import           Development.GitRev
import           Network.Wai
import           Paths_yu_launch
import           System.Environment
import           System.IO
import           Yesod.Core
import           Yu.Auth
import           Yu.Core.Control
import qualified Yu.Core.Info              as CInfo
import           Yu.Core.Model
import           Yu.Core.View
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString      as B
import qualified Yu.Import.ByteString.Lazy as BL
import qualified Yu.Import.Text            as T
import           Yu.Import.TH
import qualified Yu.Utils.Info             as UInfo


-- | basic config
data XiaoConfigServer = XCS
  { xcsPort  :: Int
  , xcsTitle :: T.Text
  , xcsKey   :: String
  , xcsDB    :: XiaoConfigDatabase
  }
  deriving (Show,Eq)
data XiaoConfigDatabase = XCD
  { xcdName :: T.Text
  , xcdHost :: String
  , xcdUser :: String
  , xcdPass :: String
  }
  deriving (Show,Eq)

deriveJSON defaultOptions {fieldLabelModifier = map toLower . drop 3 } ''XiaoConfigServer
deriveJSON defaultOptions {fieldLabelModifier = map toLower . drop 3} ''XiaoConfigDatabase

data Xiao = Xiao { xiaoTitle    :: T.Text
                 , xiaoDb       :: T.Text
                 , xiaoDBUP     :: (T.Text, T.Text)
                 , xiaoConnPool :: ConnectionPool
                 , xiaoPort     :: Int
                 , xiaoKey      :: B.ByteString
                 }

mkYesodData "Xiao" [parseRoutes| /*Texts UrlR GET PUT DELETE |]

instance Yesod Xiao where
  errorHandler er = selectRep $ do
    provideRepType "application/json" . return . T.decodeUtf8 . BL.toStrict $ encode er
    provideRep $ defaultLayout [whamlet| <h1> error
                                         <p> #{T.show er}
                                         |]
  isAuthorized (UrlR _) _ = do
    me <- requestMethod <$> waiRequest
    case me of
      "GET" -> return Authorized
      _     -> checkAuth
  defaultLayout = yuLayout layoutBootstrap
  maximumContentLength _ _ = Nothing

instance Auth Xiao SHA256 where
  tokenItem x = return $ xiaoKey x
  tokenHash _ = return SHA256

instance Hamletic Xiao (HandlerT Xiao IO) where
  getTitle = xiaoTitle <$> getYesod
  getFramePrefix = return ".frame"
  getVersion = return $(stringE (show version))
  getRaw = return False

instance Mongodic Xiao (HandlerT Xiao IO) where
  getDefaultAccessMode = return master
  getDefaultDb = xiaoDb <$> getYesod
  getDbUP = xiaoDBUP <$> getYesod
  getPool = xiaoConnPool <$> getYesod

instance Controly Xiao
