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

{-|
Module:       Yu.Tool.
Description:  
Copyright:    (C) Qinka 2017
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown


-}

{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Script
  ( scriptHandler
  ) where

import           System.FilePath
import           System.IO
import           Yu.Tool.Opt

scriptHandler :: Yu -> IO ()
scriptHandler Script{..} = do
  case sptKind of
    Just "cfg-upgrade" -> do
      ls <- lines <$> getContents
      mapM_ (putStrLn . cfgUpgrade) ls
    Just "cfg-rename" -> do
      ls <- lines <$> getContents
      mapM_ (putStrLn . cfgRename) ls
    Just "make-all" -> putStrLn makeUpdate
    _                  -> hPutStrLn stderr "help"



cfgUpgrade :: String -> String
cfgUpgrade filepath  =  "cat " ++ filepath
  ++ " | " ++ "sed 's/\"html\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"binary\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"text\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"url\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"var\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"create-time\"/\"cretime\"/g'"
  ++ " > " ++ filepath


cfgRename :: String -> String
cfgRename file =
  let (name,ext) = splitExtension file
  in name++ext++" "++name++".item"++ext

makeUpdate :: String
makeUpdate = "ls -1 .yu/*.item.json | awk -F/ '{print $2}' | sed  's/.item.json//g' | awk '{print \"make \"$1\" SITE_URL=$SITE_URL SITE_TOKEN=`yu ih -h sha256 -t $SITE_TOKEN ` \"}'  | sh"
