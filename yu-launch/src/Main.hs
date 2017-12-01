{-# LANGUAGE RecordWildCards #-}

module Main
       ( main
       ) where

import           Control.Applicative       ((<|>))
import           Data.Maybe
import qualified Data.Yaml                 as Y
import           System.Console.CmdArgs
import           System.IO
import qualified Yu.Import.Aeson           as A
import qualified Yu.Import.ByteString      as B
import qualified Yu.Import.ByteString.Lazy as BL
import qualified Yu.Import.Text            as T
import           Yu.Launch
import Paths_yu_launch


main :: IO ()
main = do
  xiao <- createXiao =<< parseCfgFile <$> fetchConfig
  case xiao of
    Just x@Xiao{..} -> warp xiaoPort x
    _               -> hPutStrLn stderr "can not parse"
  return ()


fetchConfig :: IO String
fetchConfig = do
  cfg1 <- getContents
  etcP <- getSysconfDir
  if null cfg1
    then readFile $ etcP ++ "/xiao/config"
    else return cfg1


parseCfgFileJSON :: String -> Maybe XiaoConfigServer
parseCfgFileJSON str = A.decode $ BL.pack str
parseCfgFileYAML :: String -> Maybe XiaoConfigServer
parseCfgFileYAML str = Y.decode $  B.pack str
parseCfgFile :: String -> Maybe XiaoConfigServer
parseCfgFile str =  parseCfgFileYAML str <|> parseCfgFileJSON str
