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
Module:      Yu.Import.Version
Description: The reexport of the ``version''
Copyright:   (C) 2017-2018 Johann Lee <me@qinka.pro>
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of many
-}

module Yu.Import.Version
  ( -- * the reexported module
    module Data.Version
  , module Development.GitRev
  , module System.Info
  , -- * the methods
    compileTime
  , compileOs
  , compileCompiler
  ) where

import           Data.Char
import           Data.Version
import           Development.GitRev
import           System.Info
import           Yu.Import
import           Yu.Import.TH


-- | Get the time of compiling
compileTime :: Q Exp
compileTime = do
  now <- formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  stringE now

-- | Get the os of compiler-host
compileOs :: Q Exp
compileOs = stringE $ os ++ "-" ++ arch

-- | Get the compiler system
compileCompiler :: Q Exp
compileCompiler = stringE $ toUpper <$> compilerName ++ "-" ++ showVersion compilerVersion
