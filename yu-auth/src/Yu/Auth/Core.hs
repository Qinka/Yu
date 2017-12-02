{-|
Module:      Yu.Auth.Core
Description:  Core method of the authentication
Copyright:   (C) Qinka 2017
License:     GPL-3
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The collection of core method for authentication.
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


module Yu.Auth.Core
       ( -- * method for hash
         --
         -- $hashMethod
         generateHash
       , verifyHash
       , hash
         -- * hash algorithm
       , SHA512(..)
       , SHA384(..)
       , SHA3_512(..)
       , SHA3_384(..)
       , SHA3_256(..)
       , SHA3_224(..)
       , SHA256(..)
       , SHA224(..)
       , SHA1(..)
       , SHA512t_256(..)
       , SHA512t_224(..)
       , HashAlgorithm(..)
       , ByteArrayAccess(..)
       ) where

import           Crypto.Hash
import           Data.ByteArray
import           Yu.Import
import           Yu.Import.ByteString (ByteString)
import qualified Yu.Import.ByteString as B

-- $hashMethod
--
-- These methods are used to generate the token to the hash
-- and verify the token and hash.
--
-- If you want the generate the hash for "12345qwert",
-- you should:
-- > let hashStr = generateHash SHA512 "12345qwert"
-- When you verify the hash, you need to
-- > let isOk = verifyHash SHA512 "xxxxx" "12345qwert"


-- | generate hash for the key
generateHash :: HashAlgorithm a
             => a            -- ^ Hash algorithm
             -> ByteString   -- ^ The hash of key
             -> ByteString   -- ^ Hash string
generateHash a = B.show . hashWith a


-- | verify the hash and key
verifyHash :: HashAlgorithm a
           => a          -- ^ Hash algorithm
           -> ByteString -- ^ Hash for key
           -> ByteString -- ^ Hash string
           -> Bool
verifyHash a hash token = token == generateHash a hash

