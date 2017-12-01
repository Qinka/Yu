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
       , XiaoConfig(..)
       , Route(..)
       , resourcesXiao
       , rainConfigIsServer
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
data XiaoConfig = RCServer
                  { rcsPort  :: Int
                  , rcsTitle :: T.Text
                  }
                | RCDatabase
                  { rcdDB     :: T.Text
                  , rcdDBAddr :: String
                  , rcdUser   :: String
                  , rcdPass   :: String
                  }
                  deriving (Show,Eq)
deriveJSON defaultOptions ''XiaoConfig
rainConfigIsServer :: XiaoConfig -> Bool
rainConfigIsServer RCServer{..}   = True
rainConfigIsServer RCDatabase{..} = False


data Xiao = Xiao { rainTitle    :: T.Text
                 , rainDb       :: T.Text
                 , rainDBUP     :: (T.Text, T.Text)
                 , rainConnPool :: ConnectionPool
                 , rainPort     :: Int
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
  tokenItem _ = liftIO $ B.pack <$> getEnv "RAIN_ENV"
  tokenHash _ = return SHA256

instance Hamletic Xiao (HandlerT Xiao IO) where
  getTitle = rainTitle <$> getYesod
  getFramePrefix = return ".frame"
  getVersion = return $(stringE (show version))
  getRaw = return False

instance Mongodic Xiao (HandlerT Xiao IO) where
  getDefaultAccessMode = return master
  getDefaultDb = rainDb <$> getYesod
  getDbUP = rainDBUP <$> getYesod
  getPool = rainConnPool <$> getYesod

instance Controly Xiao
