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
Module:       Yu.Tool.Make
Description:  Makefile render.
Copyright:    (C) Qinka 2017
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

Makefile render.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yu.Tool.Make
  ( makeHandler
  ) where

import           Control.Monad
import           Data.Functor
import           Data.String
import           Data.Text.Internal.Builder hiding (fromByteString)
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
import           System.Directory
import           System.FilePath
import           System.IO
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString       as B
import qualified Yu.Import.ByteString.Lazy  as BL
import qualified Yu.Import.Text             as T
import           Yu.Tool.MakeM
import           Yu.Tool.Opt                (Yu (Make, mkItem, mkOut))
import           Yu.Tool.Repo


-- | render clean command
mkClean :: MakeM ()
mkClean = do
  comment "clean"
  target "clean" [] $ do
    echo "Clean .ignore/tmp.*"
    stringT "@rm -rf .ignore/tmp.*" >> endLine
    echo "Down"

-- | render settings
mkSettings :: RepoCfg -> MakeM ()
mkSettings rc = do
  comment "site url"
  siteURL \=\ T.pack (siteUrl rc)
  comment $   "curl settings\n"
   `T.append` "detail: show details or not\n"
  curlDetail \=\ ""
  curlPath   \=\ "curl"
  comment "update time"
  updateTime \=\ "$(shell date -u \"+%Y-%m-%d %H:%M:%S UTC\")"
  comment "token for site"
  siteToken \=\ "`cat /dev/null`"
  return ()

-- | render comments
mkComment :: MakeM ()
mkComment = do
  "### Yu update Makefile\n" :: MakeM ()
  "### Copyright (C) 2017-2018\n"

-- | make the item
makeItem :: Item T.Text -> MakeM ()
makeItem item =  mkItemUpdate item >> mkItemDel item

-- | make item update items
mkItemUpdate :: Item T.Text -> MakeM ()
mkItemUpdate item@Item{..} = do
  let sumDepends = case iSummary of
        (Summary (Left p)) -> [T.pack p]
        _                  -> []
  target iId (iContent:sumDepends) $ do
    when (not (null sumDepends) && takeExtension (T.unpack $ head sumDepends) /= ".html") $
      "@pandoc -t html -o " >> stringT (head sumDepends `T.append` ".htmlout")
        >> " " >> stringT (head sumDepends) >> "\n"
    when (iType `elem` ["post","frame"] && takeExtension (T.unpack iContent) /= ".html") $ 
      "@pandoc -t html -o " >> stringT (iContent `T.append` ".htmlout")
        >> " " >> stringT iContent >> "\n"
    let url = macro siteURL `T.append` iPath
    "@"
    let commonF =
          [ curlF "type"        iType
          , curlF "create-time" (T.show iCreTime)
          , curlF "update-time" (macro updateTime)
          ]
        titleF = case iTitle of
          Just t -> [curlF "title" t]
          _      -> [curlF "title" iId]
        summaryF = if null sumDepends
          then let (Summary (Right s)) = iSummary in [curlF "summary" s]
          else [curlF "summary" ('@' `T.cons` head sumDepends `T.append` (if takeExtension (T.unpack $ head sumDepends) /= ".html" then ".htmlout" else ""))]
        whoseF = case iWhose of
          Just w -> [curlF "whose" w]
          _      -> []
        mimeF = case iMIME of
          Just m -> [curlF "mime" m]
          _      -> []
        tagsF = map (curlF "tag") iTags
        contentF = return $  case T.toLower iType of
          "post"   -> curlF "html"   ('@' `T.cons` iContent `T.append` (if takeExtension (T.unpack iContent) /= ".html" then ".htmlout" else ""))
          "frame"  -> curlF "html"   ('@' `T.cons` iContent `T.append` (if takeExtension (T.unpack iContent) /= ".html" then ".htmlout" else ""))
          "text"   -> curlF "text"   ('@' `T.cons` iContent)
          "binary" -> curlF "binary" ('@' `T.cons` iContent)
          "static" -> curlF "url"                  iContent
          "query"  -> curlF "var"                  iContent
          _        -> error $ "error type" ++ T.unpack iType
    curl "" "PUT" url $ commonF ++ contentF ++ summaryF ++ titleF ++ whoseF ++ mimeF ++ tagsF

-- | make delete rule of item
mkItemDel :: Item T.Text -> MakeM ()
mkItemDel item@Item{..} = target (iId `T.append` ".del") [] $ do
  let url = macro siteURL `T.append` iPath
  curl "" "DELETE" url [curlF "type" iType]

-- | renew the extension
renewExtensionT :: T.Text -> T.Text -> T.Text
renewExtensionT path new =
  let (name,ext) = T.breakOnEnd "." path
  in if T.null name || name == "." || T.null ext
     then path
     else name `T.append` new

-- | render nav bar list
makeNavList :: [Nav T.Text] -> MakeM ()
makeNavList navs = target "navs" [] $ do
  "@" >> curl "" "DELETE" "${SITE_URL}/.query/.nav" [] >> endLine
  forM_ navs $ \Nav{..} ->
    "@" >> curl "" "PUT" "${SITE_URL}/.query/.nav" [ curlF "order" (T.show nOrder)
                                        , curlF "url"            nUrl
                                        , curlF "label"          nLabel
                                        ] >> endLine

-- | make hander
makeHandler :: Yu -> IO ()
makeHandler Make{..} = do
  repo' <- findRepo yuRepoName
  case repo' of
    Nothing -> hPutStrLn stderr "Can not find out the repo"
    Just repo ->
      case mkItem of
        Nothing -> do
          let cfgPath = repo ++ "/.yu/yual.json"
          is <- doesFileExist cfgPath
          if is then do
            rcfg' <- decode <$> BL.readFile cfgPath
            case rcfg' of
              Nothing -> hPutStrLn stderr "Can not parse the yual config file"
              Just rcfg ->
                let text = show $ mkSettings rcfg
                in case mkOut of
                  Nothing   -> putStrLn text
                  Just file -> appendFile (repo ++ "/" ++ file) text
            else hPutStrLn stderr $ "Can not find file: " ++ cfgPath
        Just "navlist" -> do
          let navsPath = repo ++ "/.yu/navlist.json"
          is <- doesFileExist navsPath
          if is
            then do
            navs' <- decode <$> BL.readFile (repo ++ "/.yu/navlist.json")
            case navs' of
              Nothing   -> hPutStrLn stderr "Can not parse the json file for nav list"
              Just navs ->
                let text = show $ makeNavList navs
                in case mkOut of
                  Nothing   -> putStrLn text
                  Just file -> appendFile (repo ++ "/" ++ file) text
            else hPutStrLn stderr "Can not find the json file for nav-list"
        Just item' -> do
          let itemPath = repo ++ "/.yu/" ++ item' ++ ".item.json"
          is <- doesFileExist itemPath
          if is then do
            itemMaybe <- decode <$> BL.readFile itemPath
            case itemMaybe of
              Nothing -> hPutStrLn stderr "Can not find the json file of item"
              Just item'' -> do
                let item = makeAbsoluteRepoT repo item''
                    text = show $ makeItem item''
                case mkOut of
                  Nothing   -> putStrLn text
                  Just file -> appendFile (repo ++ "/" ++ file) text
            else hPutStrLn stderr $ "Can not find the json file for item file: " ++ item' ++ ".item.json"


