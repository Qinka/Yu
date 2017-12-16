

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module       : Yu.Core.Control
Description  : The view of glob
Copyright    : (C) Qinka 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknow

The control part of the glob.
-}

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


module Yu.Core.Control
       ( Controly(..)
       , yuRoute
       , getUrlR
       , putUrlR
       , deleteUrlR
       ) where

import           Yesod.Core
import           Yu.Core.Control.Internal
import           Yu.Core.Model
import           Yu.Core.View
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString     as B
import           Yu.Import.Text           (Text)
import qualified Yu.Import.Text           as T
import           Yu.Utils.Handler
import Yesod.Routes.TH.Types

-- | default router
yuRoute :: [ResourceTree String]
yuRoute = [parseRoutes| /*Texts UrlR GET PUT DELETE |]

-- | get method router
getUrlR :: Controly site
        => [T.Text] -- ^ index
        -> HandlerT site IO TypedContent
getUrlR idx@(".query":_) = getQueryR idx =<< runDbDefault (fetchRes idx)
getUrlR idx = do
  res <- runDbDefault $ fetchRes idx
  case rType <$> res of
    Just "post"   -> getPostR           res
    Just "text"   -> getResourceR True  res
    Just "binary" -> getResourceR False res
    Just "static" -> getStaticR         res
    _             -> liftIO (print res) >> notFound

-- | put method router
putUrlR :: Controly site
        => [Text] -- ^ index
        -> HandlerT site IO TypedContent
putUrlR (".query":".nav":_) = putNavR
putUrlR idx = do
  typ <- lookupPostParam "type"
  case typ of
    Just "post"   -> putPostR           idx
    Just "text"   -> putResourceR True  idx
    Just "binary" -> putResourceR False idx
    Just "static" -> putStaticR         idx
    Just "frame"  -> putFrameR          idx
    Just "query"  -> putQueryR          idx
    _             -> notFound

-- | delete
deleteUrlR :: Controly site
           => [Text] -- ^ index
           -> HandlerT site IO TypedContent
deleteUrlR (".query":".nav":_) = delNavR
deleteUrlR idx = do
  typ <- lookupPostParam "type"
  db <- case typ of
    Just "post"   -> return "post"
    Just "text"   -> return "resource"
    Just "binary" -> return "resource"
    Just "static" -> return "static"
    Just "query"  -> return "query"
    Just "frame"  -> return "frame"
    _             -> notFound
  rt <- tryH.runDbDefault $ deleteItem idx db
  case rt of
    Left e  -> returnEH e
    Right _ -> returnSucc


-- | get post
getPostR :: Controly site
         => Maybe ResT -- ^ index
         -> HandlerT site IO TypedContent
getPostR (Just res@ResT{..}) = do
  html <- runDbDefault $ fetchPost res
  case html of
    Just pH -> respondPost res pH
    _       -> liftIO (putStrLn "Faile to get") >> notFound
getPostR _ = notFound

-- | put post
putPostR :: Controly site
            => [Text] -- ^ index
            -> HandlerT site IO TypedContent
putPostR idx = do
  unR  <- lookupPostUnResT idx
  html <- T.decodeUtf8 <#> getFile "html"
  putItem unR html updatePost

-- | get resource
getResourceR :: Controly site
                => Bool -- ^ whether item is text
                -> Maybe ResT
                -> HandlerT site IO TypedContent
getResourceR t (Just res@ResT{..}) = do
  ct <- runDbDefault $ fetchItem res
  case ct of
    Just (Left    text) -> respondResourceT res text
    Just (Right binary) -> respondResourceB res binary
    _                   -> notFound
  where
    fetchItem :: Controly site
                  => ResT
                  -> Action (HandlerT site IO) (Maybe (Either T.Text B.ByteString))
    fetchItem = if t
                 then (Left  <#>) <$> fetchResourceT
                 else (Right <#>) <$> fetchResourceB
getResourceR _ _ = notFound

-- | put resource
putResourceR :: Controly site
                => Bool -- ^ whether item is text
                -> [T.Text]
                -> HandlerT site IO TypedContent
putResourceR t idx = do
  unR  <- lookupPostUnResT idx
  text <- T.decodeUtf8 <#> getFile "text"
  bin  <- getFile "binary"
  if t
    then putItem unR            text  updateResourceT
    else putItem unR (Binary <$> bin) updateResourceB

-- | get static
getStaticR :: Controly site
              => Maybe ResT
              -> HandlerT site IO TypedContent
getStaticR (Just res@ResT{..}) = do
  url <- runDbDefault $ fetchStatic res
  case url of
    Just u -> respondStatic res u
    _      -> notFound
getStaticR _ = notFound

-- | put static
putStaticR :: Controly site
               => [Text]
               -> HandlerT site IO  TypedContent
putStaticR idx = do
  unR <- lookupPostUnResT idx
  url <- lookupPostParam "url"
  putItem unR url updateStatic


-- | put frame
putFrameR :: Controly site
             => [T.Text]
             -> HandlerT site IO TypedContent
putFrameR idx = do
  unR <- lookupPostUnResT idx
  html <- T.decodeUtf8 <#> getFile "html"
  putItem unR html updateFrame

-- | get query
getQueryR :: Controly site
             => [Text]
             -> Maybe ResT
             -> HandlerT site IO TypedContent
getQueryR idx r =
  case tail idx of
    ".version":"author":_ -> queryVersionAuthor
    ".version":"utils":_  -> queryVersionUtils
    ".version":"core":_   -> queryVersionCore
    ".version":_          -> queryVersion
    ".name":_             -> queryName
    ".buildinfo":_        -> queryBuildInfo
    ".servertime":_       -> queryServerTime
    ".nav":_              -> runDbDefault fetchNav >>= queryNav
    ".index":xs           -> runDbDefault fetchResAll >>= queryIndex (T.unpack $ T.concat xs)
    _ -> runDbDefault (fetchMaybeR fetchQuery r)
      >>= (\t -> case t of
              Just text -> queryQuery text
              _         -> notFound
          )

-- | put query
putQueryR :: Controly site
            => [T.Text]
            -> HandlerT site IO TypedContent
putQueryR idx = do
  unR <- lookupPostUnResT idx
  var <- lookupPostParam "var"
  putItem unR var updateQuery

-- | put navs
putNavR :: Controly site
           => HandlerT site IO TypedContent
putNavR = do
  idx   <- lookupPostParam "label"
  url   <- lookupPostParam "url"
  order <- lookupPostParam "order"
  runDbDefault $ updateNav idx url (T.read <$> order)
  returnSucc

-- | delete navs
delNavR :: Controly site
           => HandlerT site IO TypedContent
delNavR = do
  idx <- lookupPostParam "label"
  runDbDefault $ deleteNav idx
  returnSucc

