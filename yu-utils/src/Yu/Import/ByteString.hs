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
Module:      Yu.Import.ByteString
Description: reexport of the bytestring
Copyright:   (C) 2017-2018 Johann Lee <me@qinka.pro>
License:     GPL3
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of the bytestring
-}

module Yu.Import.ByteString
  ( -- * the reexported module
    module Data.ByteString
  , -- * the methods
    pack
  , unpack
  , read
  , show
  ) where

import           Data.ByteString       hiding (pack, unpack)
import           Data.ByteString.Char8 (pack, unpack)
import           Prelude               ((.))
import qualified Prelude               as P

-- | The read method for bytestring
read :: P.Read a
        => Data.ByteString.ByteString  -- ^ string
        -> a                           -- ^ result
read = P.read . unpack

-- | The show method for bytestring
show :: P.Show a
        => a                           -- ^ item
        -> Data.ByteString.ByteString  -- ^ result
show = pack . P.show
