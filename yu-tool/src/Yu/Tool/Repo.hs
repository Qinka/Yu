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
Module:       Yu.Tool.Repo
Description:  Operations about repo.
Copyright:    (C) 2017-2018 Johann Lee
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

Operations about repo.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Yu.Tool.Repo
  ( RepoCfg(..)
  , Summary(..)
  , Item(..)
  , Nav(..)
  , findRepo
  , makeAbsoluteRepoT
  , makePathRelateRepo
  , yuRepoName
  , yuItemExt
  , yuRepoCfg
  , yuNavList
  ) where

import           Data.Char             (toLower)
import           Data.Function
import           Data.List
import           System.Directory
import           System.FilePath.Posix (makeRelative)
import           Yu.Import
import           Yu.Import.Aeson
import           Data.String
import qualified Yu.Import.Text        as T

newtype RepoCfg = RepoCfg { siteUrl :: String
                          }
                  deriving Show
deriveJSON defaultOptions{ fieldLabelModifier = map toLower
                         , constructorTagModifier = map toLower} ''RepoCfg

newtype Summary a = Summary (Either FilePath a)
                  deriving Show
deriveJSON defaultOptions ''Summary
instance Functor Summary where
  fmap f (Summary summary) = Summary $ f <$> summary

data Item a = Item { iSummary :: Summary a
                   , iMIME    :: Maybe a
                   , iPath    :: a
                   , iWhose   :: Maybe a
                   , iCreTime :: UTCTime
                   , iId      :: a
                   , iContent :: a
                   , iTitle   :: Maybe a
                   , iType    :: a
                   , iTags    :: [a]
                   }
          deriving Show
deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 1
                         , constructorTagModifier = map toLower} ''Item
instance Functor Item where
  fmap f Item{..} = Item
                    (f <$> iSummary)
                    (f <$> iMIME)
                    (f     iPath)
                    (f <$> iWhose)
                    (      iCreTime)
                    (f     iId)
                    (f     iContent)
                    (f <$> iTitle)
                    (f     iType)
                    (f <$> iTags)


data Nav a = Nav { nOrder :: Int
                 , nUrl   :: a -- url
                 , nLabel :: a -- name, or say label
                 }
           deriving Show
instance Functor Nav where
  fmap f Nav{..} = Nav nOrder (f nUrl) (f nLabel)
instance Eq a => Eq (Nav a) where
  (==) = (==) `on` nLabel

deriveJSON defaultOptions{ fieldLabelModifier = map toLower . drop 1
                         , constructorTagModifier = map toLower} ''Nav

-- | find out repo's dir
--   If the path of the .git direcory is @/path/to/repo/.git@
--   @the findRepo ".git"@ will return @/path/to/repo@
findRepo :: FilePath -> IO (Maybe FilePath)
findRepo fp = getCurrentDirectory >>= findRepoStep
  where findRepoStep "" = return Nothing
        findRepoStep dir = do
          list <- filter (== fp) <$>  listDirectory dir
          if null list
            then findRepoStep (uplevel dir)
            else return (Just dir)
        uplevel "/" = ""
        uplevel p = reverse
                  . dropWhile (\x -> x /= '/' && x /= '\\')
                  . dropWhile (\x -> x == '/' || x == '\\')
                  . reverse
                  $ p

makeAbsoluteRepoT :: FilePath -- ^ repo
                  -> Item T.Text
                  -> Item T.Text
makeAbsoluteRepoT repo item =
  let newCon = T.pack repo `T.append` iContent item
      newSum = case iSummary item of
        Summary (Left p) -> Summary $ Left $ repo ++ p
        _                -> iSummary item
  in item { iSummary = newSum, iContent = newCon}

yuRepoName :: IsString a => a
yuRepoName = ".yu"

yuItemExt :: IsString a => a
yuItemExt = ".item.json"

yuRepoCfg :: IsString a => a
yuRepoCfg = "yual.json"

yuNavList :: IsString a => a
yuNavList = "navlist.json"

makePathRelateRepo :: FilePath -- ^ repo path
                   -> Item String
                   -> IO (Item String)
makePathRelateRepo repo item = do
  newSum <- case iSummary item of
        Summary (Left path) ->
          (Summary . Left . makeRelative repo) <$> makeAbsolute path
        _ -> return (iSummary item)
  newCon <- makeRelative repo <$> makeAbsolute (iContent item)
  return item{iSummary = newSum, iContent = newCon}

