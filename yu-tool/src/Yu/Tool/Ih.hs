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
Module:       Yu.Tool.Ih
Description:  The identifier helper of yu.
Copyright:    (C) 2017-2018 Johann Lee <me@qinka.pro>
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The identifier helper of yu.
-}


{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Ih
  ( ihHandler
  ) where

import           Control.Monad        (when)
import           System.IO
import           Yu.Auth.Core         hiding (generateHash)
import qualified Yu.Auth.Core         as A
import qualified Yu.Import.ByteString as B
import           Yu.Tool.Opt

-- | Identifier helper
ihHandler :: Yu -> IO ()
ihHandler Ih{..} = case ihToken of
  Just token -> do
    cmds <- getContents
    put ihDebug $ generateHash token
  _ ->  hPutStrLn stderr "token required"
  where generateHash  = pack $ case ihHash of
          Just "sha512"      -> A.generateHash SHA512
          Just "sha384"      -> A.generateHash SHA384
          Just "sha3-512"    -> A.generateHash SHA3_512
          Just "sha3-384"    -> A.generateHash SHA3_384
          Just "sha3-256"    -> A.generateHash SHA3_256
          Just "sha3-224"    -> A.generateHash SHA3_224
          Just "sha256"      -> A.generateHash SHA256
          Just "sha224"      -> A.generateHash SHA224
          Just "sha512t-256" -> A.generateHash SHA512t_256
          Just "sha512t-224" -> A.generateHash SHA512t_224
          _                  -> A.generateHash SHA1
        pack f = B.unpack . f . B.pack
        put i t = hPutStr   stdout t
             >> when i (hPutStrLn stderr t)
