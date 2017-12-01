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


ihHandler :: Yu -> IO ()
ihHandler Ih{..} =
  case ihToken of
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
