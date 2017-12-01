{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Yu.Launch
       ( createXiao
       , warp
       , module Yu.Launch.Internal
       ) where

import           System.IO
import           Yesod.Core.Dispatch
import           Yu.Core.Control
import           Yu.Core.Model
import qualified Yu.Import.Text      as T
import           Yu.Launch.Internal


mkYesodDispatch "Xiao" resourcesXiao

createXiao :: [XiaoConfig]
           -> IO (Maybe Xiao)
createXiao rcs = do
  case (filter rainConfigIsServer rcs, filter (not.rainConfigIsServer) rcs) of
    (RCServer{..}:_,RCDatabase{..}:_) -> do
      cp <- createPool (connect $ readHostPort rcdDBAddr) close 10 20 1000
      return $ Just $ Xiao { rainTitle    = rcsTitle
                           , rainDb       = rcdDB
                           , rainDBUP     = (T.pack rcdUser,T.pack rcdPass)
                           , rainConnPool = cp
                           , rainPort     = rcsPort
                           }
    _ -> do
      hPutStrLn stderr "invaild config"
      return Nothing

