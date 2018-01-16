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

