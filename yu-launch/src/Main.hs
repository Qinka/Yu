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


main :: IO ()
main = do
  rain <- createXiao =<< parseCfgFile <$> getContents
  case rain of
    Just r@Xiao{..} -> warp rainPort r
    _               -> hPutStrLn stderr "can not parse"
  return ()


parseCfgFileJSON :: String -> Maybe [XiaoConfig]
parseCfgFileJSON str = A.decode $ BL.pack str
parseCfgFileYAML :: String -> Maybe [XiaoConfig]
parseCfgFileYAML str = Y.decode $  B.pack str
parseCfgFile :: String ->  [XiaoConfig]
parseCfgFile str =  fromMaybe [] $ parseCfgFileYAML str <|> parseCfgFileJSON str
