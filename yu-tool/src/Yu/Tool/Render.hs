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
Module:       Yu.Tool.Render
Description:  Render to html
Copyright:    (C) Qinka 2017
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

Render to html
-}

-}

{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Render
  ( renderHandler
  ) where

import           System.Directory
import           System.IO
import           Yu.Import
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString.Lazy as BL
import           Yu.Tool.Opt
import           Yu.Tool.Repo

data FileType = Markdown | Tex | Html | XtTex


renderHandler :: Yu -> IO ()
renderHandler Render{..} = do
  fileStr  <- getFile renderFile
  let fileType = getType renderFile renderType

  where getFile "-" = getContents
        getFile fp  = readFile fp
        getType _ (Just typ) = typ
        getType fn Nothing = case (reverse . takeWhile (/='.') . reverse) fn of
          "md" -> 