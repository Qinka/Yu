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

{-|
Module:       Yu.Tool.Init
Description:  Initialization handler.
Copyright:    (C) Qinka 2017
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

Initialization handler.
-}

{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Init
  ( initHandler
  ) where

import           System.Directory
import           System.IO
import           System.IO.Echo
import           Yu.Import
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString.Lazy as BL
import           Yu.Tool.Opt
import           Yu.Tool.Repo

-- | initialization handler
initHandler :: Yu -> IO ()
initHandler Init{..} = do
  createDirectory yuRepoName
  url   <- case initSiteUrl of
    Just u -> return u
    Nothing -> do
      hPutStrLn stderr "warrning: site url unset"
      return ""
  token <- case initTokenFile of
    Just fp -> return fp
    Nothing -> do
      hPutStrLn stderr "warrning: token file unset"
      return ""
  BL.writeFile (yuRepoName ++ "/yual.json") $ encode (RepoCfg url)
  writeFile (yuRepoName ++ "/token.yu-ignore") token
  writeFile (yuRepoName ++ "/navlist.json") "[]"


