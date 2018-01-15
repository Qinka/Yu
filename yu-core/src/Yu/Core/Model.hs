{-
 Copyright (C) 2017-2018 Johann Lee

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
Module       : Yu.Core.Model
Description  : The module for model
Copyright    : (C) 2017-2018 Johann Lee <me@qinka.pro>
Maintainer   : me@qinka.pro
License      : GPL3
Stability    : experimental
Portability  : unknown

The codes for model
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


module Yu.Core.Model
       ( -- * run database
         runDb
       , runDbDefault
       , -- * model
         fetchFrame
       , updateFrame
       , fetchPost
       , updatePost
       , fetchResourceB
       , updateResourceB
       , fetchResourceT
       , updateResourceT
       , fetchStatic
       , updateStatic
       , fetchQuery
       , updateQuery
       , fetchMaybeI
       , fetchMaybeR
       , -- ** for navgation
         fetchNav
       , updateNav
       , deleteNav
       , -- re-export
         module Yu.Core.Model.Internal
       ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Pool
import           Text.Blaze.Html             (Html (..))
import qualified Text.Blaze.Html             as TBH
import           Yu.Core.Model.Internal
import           Yu.Core.Model.TH
import           Yu.Import
import           Yu.Import.ByteString        (ByteString (..))
import           Yu.Import.Text              (Text (..))
import           Yu.Utils.Handler
import           Yu.Utils.Handler

-- | escapted to html
preEscapedToHtml :: Text -> Html
preEscapedToHtml = TBH.preEscapedToHtml

-- | about frame
makeFetch 'preEscapedToHtml "frame" ''Html "html" "frame"
makeUpdate                 "frame" ''Text "html" "frame"

-- | about post
makeFetch 'preEscapedToHtml "post" ''Html "html" "post"
makeUpdate                 "post" ''Text "html" "post"

-- | about text resource
makeFetch 'id "resourceT" ''Text "text" "resource"
makeUpdate   "resourceT" ''Text "text" "resource"

-- | about binary resource
makeFetch 'fromBinary "resourceB" ''ByteString "binary" "resource"
makeUpdate           "resourceB" ''Binary "binary" "resource"

-- | about static
makeFetch 'id "static" ''Text "url" "static"
makeUpdate   "static" ''Text "url" "static"

-- | about query
makeFetch 'id "query" ''Text "var" "query"
makeUpdate   "query" ''Text "var" "query"


-- | fetch maybe index
fetchMaybeI :: MonadIO m
              => (ResT -> Action m (Maybe a))  -- ^ funcion for action
              -> [Text] -- ^ index
              -> Action m (Maybe a)
fetchMaybeI mf idx =
  fetchRes idx >>= fetchMaybeR mf

-- | fetch maybe resource
fetchMaybeR :: MonadIO m
              => (ResT -> Action m (Maybe a)) -- ^ function for action
              -> Maybe ResT -- ^ index
              -> Action m (Maybe a)
fetchMaybeR mf (Just r) = mf r
fetchMaybeR _  _        = return Nothing

-- | fetch the nav
fetchNav :: (MonadBaseControl IO m, MonadIO m)
             => Action m [Nav]
fetchNav = do
  cr <- find $ select [] "nav"
  navs <- map docToNav <$> rest cr
  closeCursor cr
  return $ catMaybes navs

-- | update nav
updateNav :: MonadIO m
           => Maybe Text  -- ^ label
           -> Maybe Text  -- ^ url
           -> Maybe Int     -- ^ order
           -> Action m ()
updateNav label url order =
  void $ upsert (select ["label" =: label] "nav") $ catMaybes
  [ Just ("index" =: label)
  , "url"   =@ url
  , "order" =@ order
  ]

-- | delete the nav
deleteNav :: MonadIO m
           => Maybe Text -- ^ label ( if it is Nothing, the all nav item will be delete)
           -> Action m ()
deleteNav label =
    delete $ select (catMaybes ["index" =@ label]) "nav"

-- | run mongo
runDb :: Mongodic site m
       => AccessMode  -- ^ access mode
       -> Database    -- ^ database
       -> Action m a  -- ^ action
       -> m a
runDb am db mf = getPool >>= \pool ->
  withResource pool $ \p -> do
  (user,pass) <- getDbUP
  access p am db $ do
    auth user pass
    mf

-- | run mongo with default
runDbDefault  :: Mongodic site m
                => Action m a  -- ^ action
                -> m a
runDbDefault mf = do
  am <- getDefaultAccessMode
  db <- getDefaultDb
  runDb am db mf

