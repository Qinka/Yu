{-|
Module:      Yu.Import.Text
Description: The reexport of text
Copyright:   (C) Qinka 2017
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknows

The reexport of text
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

module Yu.Import.Text
       ( -- * the reexported module
         module Data.Text
       , module Data.Text.Encoding
       , -- * the methods
         read
       , show
       ) where

import           Data.Text
import           Data.Text.Encoding
import           Prelude            ((.))
import qualified Prelude            as P

-- | the read method for Text
read :: P.Read a
        => Data.Text.Text -- ^ the string
        -> a              -- ^ item
read = P.read . Data.Text.unpack

-- | the show method for Text
show :: P.Show a
        => a              -- ^ item
        -> Data.Text.Text -- ^ the string
show = Data.Text.pack . P.show
