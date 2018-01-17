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
Module:       Yu.Tool.Helper
Description:  The helpers of the tool
Copyright:    (C) 2017-2018 Johann Lee <me@qinka.pro>
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The identifier helper of yu.
-}


{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Helper
  ( helperHandler
  ) where

import Data.Maybe
import           Yu.Tool.Repo
import           Yu.Tool.Opt

-- | Identifier helper
helperHandler :: Yu -> IO ()
helperHandler Helper{..} = case hperItem of
  Just "path" -> (fromMaybe "") <$> findRepo yuRepoName >>= putStrLn
  Nothing -> putStrLn "help info"
