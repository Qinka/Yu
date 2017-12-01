{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Init
  ( initHandler
  ) where

import           System.Directory
import           System.IO
import           System.IO.Echo
import           Yu.Import
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString.Lazy as BL
import           Yu.Tool.Opt
import           Yu.Tool.Repo

initHandler :: Yu -> IO ()
initHandler Init{..} = do
  createDirectory yuRepoName
  url   <- case initSiteUrl of
    Just u -> return u
    Nothing -> do
      hPutStrLn stderr "warrning: site url unset"
      return ""
  token <- case initTokenFile of
    Just fp -> return fp
    Nothing -> do
      hPutStrLn stderr "warrning: token file unset"
      return ""
  BL.writeFile (yuRepoName ++ "/yual.json") $ encode (RepoCfg url)
  writeFile (yuRepoName ++ "/token.yu-ignore") token
  writeFile (yuRepoName ++ "/navlist.json") "[]"


