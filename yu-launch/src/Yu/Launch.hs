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
import qualified Yu.Import.ByteString      as B
import qualified Yu.Import.Text            as T
import           Yu.Launch.Internal


mkYesodDispatch "Xiao" resourcesXiao

createXiao :: Maybe XiaoConfigServer
           -> IO (Maybe Xiao)
createXiao Nothing   = return Nothing
createXiao (Just xc) = do
  let XCS{..} = xc
      XCD{..} = xcsDB
  cp <- createPool (connect $ readHostPort xcdHost) close 10 20 1000
  return $ Just $ Xiao { xiaoTitle    = xcsTitle
                       , xiaoDb       = xcdName
                       , xiaoDBUP     = (T.pack xcdUser,T.pack xcdPass)
                       , xiaoConnPool = cp
                       , xiaoPort     = xcsPort
                       , xiaoKey      = B.pack xcsKey
                       }

