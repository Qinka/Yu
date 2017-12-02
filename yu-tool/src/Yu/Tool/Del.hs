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

{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Del
  ( delHandler
  ) where

import           System.Directory
import           System.IO
import           Yu.Tool.Opt
import           Yu.Tool.Repo


delHandler :: Yu -> IO ()
delHandler Del{..} = do
  case delId of
    Just i -> removePathForcibly i
    _      -> hPutStrLn stderr "error: path required"
