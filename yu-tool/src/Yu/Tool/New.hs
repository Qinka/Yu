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
Module:       Yu.Tool.New
Description:  For command new.
Copyright:    (C) 2017-2018 Johann Lee <me@qinka.pro>
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

For command new.
-}

{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.New
  ( newHandler
  ) where

import           System.Directory
import           System.IO
import           Yu.Import
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString.Lazy as BL
import           Yu.Tool.Opt
import           Yu.Tool.Repo

-- | new item handler
newHandler :: Yu -> IO ()
newHandler New{..} = findRepo yuRepoName >>= \repo' -> case repo' of
  Just repo -> case (newId,newTyp,newPath,newContent) of
      (Just nid, Just ntyp, Just npath, Just ncontent) -> do
        sum <- toSummary newSum repo
        cur <- getCurrentTime
        item <- makePathRelateRepo repo
          Item { iSummary = Summary sum
               , iMIME    = newMIME
               , iPath    = npath
               , iWhose   = newWhose
               , iCreTime = cur
               , iId      = nid
               , iContent = ncontent
               , iTitle   = newTitle
               , iType    = ntyp
               , iTags    = newTags
               }
        BL.writeFile (repo ++ "/" ++ yuRepoName ++ "/" ++ nid ++ ".item.json") $ encode item
      _ -> hPutStrLn stderr "error: one(some) of id, typ, path, or content is(are) empty"
  where toSummary (Just s) repo = do
          is <- makeAbsolute s >>= doesPathExist
          return $ (if is then Left else Right) s
        toSummary Nothing _ = return $ Right ""
