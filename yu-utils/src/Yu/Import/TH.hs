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
Module:       Yu.Import.TH
Description:  The reexport of the TemplatHaskell
Copyright:    (C) 2017-2018 Johann Lee <me@qinka.pro>
License:      GPL-3
Maintainer:   me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of the template haskell
-}

module Yu.Import.TH
  ( -- * the reexported module
    module Language.Haskell.TH
  , module Language.Haskell.TH.Quote
  , module Language.Haskell.TH.Syntax
  ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

