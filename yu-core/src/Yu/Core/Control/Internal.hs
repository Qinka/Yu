{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-|
Module       : Yu.Core.Control.Internal
Description  : The view of glob
Copyright    : (C) Qinka 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknow

The control part of the glob.
-}


module Yu.Core.Control.Internal
       ( lookupPostUnResT
       , getFile
       , getField
       , putItem
       , returnSucc
       , Controly(..)
       ) where

import           Data.Conduit
import           Yesod.Core
import           Yu.Core.Model
import           Yu.Core.View
import qualified Yu.Import.ByteString as B
import           Yu.Import.Text       (Text)
import qualified Yu.Import.Text       as T
import           Yu.Utils.Handler



-- | for control
class (Mongodic site (HandlerT site IO), MonadHandler (HandlerT site IO), Hamletic site (HandlerT site IO), Yesod site) => Controly site

-- | lookup the undefined index
lookupPostUnResT :: Controly site
                 => [Text] -- ^ index
                 -> HandlerT site IO (Maybe ResT)
lookupPostUnResT idx = do
  ty <- lookupPostParam  "type"
  ct <- lookupPostParam  "create-time"
  ut <- lookupPostParam  "update-time"
  ti <- lookupPostParam  "title"
  su <- getField         "summary"
  wh <- lookupPostParam  "whose"
  mi <- lookupPostParam  "mime"
  tg <- lookupPostParams "tag"
  ts <- T.words <#> lookupPostParams "tags"
  return $ case (ty,ct,ut,ti) of
    (Just t,Just c,Just u,Just i) -> Just . ResT
      idx undefined t (T.read c) (T.read u) i su wh mi . concat $ tg:ts
    _ -> Nothing

-- | get the uploaded file in ByteString
getFilesBS :: (MonadResource m, MonadHandler m)
           => [FileInfo] -- ^ file infos
           -> m (Maybe B.ByteString)
getFilesBS [] = return Nothing
getFilesBS xs = Just. B.concat.concat <$>
  mapM (sourceToList.fileSource) xs

-- | get the file via file name
getFile :: (MonadResource m, MonadHandler m)
        => T.Text -- ^ file name (field name)
        -> m (Maybe B.ByteString)
getFile file = getFilesBS =<< lookupFiles file

-- | get the field text
getField :: (MonadResource m, MonadHandler m)
         => T.Text -- ^ field name
         -> m (Maybe T.Text)
getField fieled = do
  su <- T.decodeUtf8 <#> getFile fieled
  case su of
    Just s -> return su
    _      -> lookupPostParam fieled

-- | for upload the items
putItem :: (Controly site, Val a)
        => Maybe ResT -- ^ resource index (maybe)
        -> Maybe a    -- ^ item (maybe)
        -> (a -> ResT -> Action (HandlerT site IO) ()) -- ^ upload action for database
        -> HandlerT site IO TypedContent
putItem unR item f = case (unR,item) of
  (Just r,Just i) -> do
    rt <- tryH.runDbDefault $ f i r
    returnI rt
  _ ->  invalidArgs [" args failed"]
  where
    returnI (Left e)  = returnEH e
    returnI (Right _) = respondSource "" $ sendChunkText "success"

-- | return sucecess
returnSucc :: HandlerT site IO TypedContent
returnSucc = respondSource "text/plain" $ sendChunkText "success"
