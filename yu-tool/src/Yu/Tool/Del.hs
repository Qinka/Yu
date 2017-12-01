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
