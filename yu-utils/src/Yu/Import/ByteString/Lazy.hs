{-|
Module:       Yu.Import.ByteString.Lazy
Description:  The reexport of the Data.ByteString.Lazy
Copyright:    (C) 2017 Qinka
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The reexport of the Data.ByteString.Lazy
-}

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

module Yu.Import.ByteString.Lazy
       ( -- * the reexport modules
         module BL
       , BLC8.pack
       , BLC8.unpack
       , -- * the methods
         read
       , show
       ) where

import           Data.ByteString.Lazy       as BL hiding (pack, unpack)
import           Data.ByteString.Lazy.Char8 as BLC8
import           Prelude                    ((.))
import qualified Prelude

-- | the read method for Data.ByteString.Lazy.ByteString
read :: Prelude.Read a
        => BL.ByteString -- ^ string
        -> a                               -- ^ item
read = Prelude.read . BLC8.unpack


-- | the show method for Data.ByteString.Lazy.ByteString
show :: Prelude.Show a
        => a                               -- ^ item
        -> BL.ByteString -- ^ string
show = BLC8.pack . Prelude.show


