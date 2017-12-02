{-|
Model         : Yu.Core.Model.Internal
Description   : The basic methods for model and types
Copyright     : (C) Qinka 2017
License       : GPL3
Maintainer    : me@qinka.pro
Stability     : experimental
Portability   : unknown

The basic method and type for model in MVC
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

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}

module Yu.Core.Model.Internal
       ( -- * navigation bar
         Nav(..)
       , navToDoc
       , docToNav
       , -- * the resource
         ResT(..)
       , resToDoc
       , docToRes
       , -- * transform
         fromBinary
       , -- * about mongoDB
         Mongodic(..)
       , ConnectionPool
       , fetchContext
       , fetchRes
       , fetchResAll
       , updateContext
       , updateItem
       , updateRes
       , deleteContext
       , deleteItem
       , deleteRes
       , deleteContextMaybe
       , (=@)
       , module Database.MongoDB
       , module Data.Pool
       ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Pool
import           Database.MongoDB
import           Yu.Import
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString        as B
import qualified Yu.Import.Text              as T
import           Yu.Utils.Handler

-- | ConnectionPool
type ConnectionPool = Pool Pipe

-- | model for navigation bar
data Nav = Nav { navUrl   :: T.Text -- ^ The url of the link
               , navLabel :: T.Text -- ^ The label of the link
               , navOrder :: Int    -- ^ The order of the link
               }
         deriving (Show)

-- | instance eq, order for nav
instance Eq Nav where
  n1 == n2 = navOrder n1 == navOrder n2
instance Ord Nav where
 compare n1 n2 = compare (navOrder n1) (navOrder n2)

-- |transform between nav and document
navToDoc :: Nav -> Document
navToDoc Nav{..} =
  [ "index"   =: navLabel
  , "url"   =: navUrl
  , "order" =: navOrder
  ]

-- | transform Document to nav
docToNav :: Document -> Maybe Nav
docToNav doc = Nav
  <$> doc !? "url"
  <*> doc !? "index"
  <*> doc !? "order"

-- | instance Json(to)
instance ToJSON Nav where
  toJSON Nav{..} = object
    [ "label" .= navLabel
    , "url" .= navUrl
    , "order" .= navOrder
    ]

-- | the resource type for item
data ResT = ResT
            { rIndex   :: [T.Text]      -- ^ the path of the url
            , rRes     :: ObjectId      -- ^ the id of the object in the db
            , rType    :: T.Text        -- ^ the type of resource
            , rCTime   :: UTCTime       -- ^ the time when it created
            , rUTime   :: UTCTime       -- ^ the time when it updated
            , rTitle   :: T.Text        -- ^ the title
            , rSummary :: Maybe T.Text  -- ^ the summary of result
            , rWhose   :: Maybe T.Text  -- ^ the own of the result
            , rMIME    :: Maybe T.Text  -- ^ the MIME type of the result
            , rTags    :: [T.Text]      -- ^ the tags for result
            }
          deriving (Show)

instance Eq ResT where
  res1 == res2 = and
    [ abs (rCTime res1 `diffUTCTime` rCTime res2) <= 1
    , abs (rUTime res1 `diffUTCTime` rUTime res2) <= 1
    , rIndex   res1 == rIndex   res2
    , rRes     res1 == rRes     res2
    , rType    res1 == rType    res2
    , rTitle   res1 == rTitle   res2
    , rSummary res1 == rSummary res2
    , rWhose   res1 == rWhose   res2
    , rMIME    res1 == rMIME    res2
    , rTags    res1 == rTags    res2
    ]

-- | transform between res document
resToDoc :: ResT -> Document
resToDoc ResT{..} =
  [ "index"       =: rIndex
  , "res"         =: rRes
  , "type"        =: rType
  , "create-time" =: rCTime
  , "update-time" =: rUTime
  , "title"       =: rTitle
  , "summary"     =: rSummary
  , "whose"       =: rWhose
  , "mime"        =: rMIME
  , "tags"        =: rTags
  ]



-- | transform between res document
docToRes :: Document -> Maybe ResT
docToRes doc = ResT
  <$>       doc !? "index"
  <*>       doc !? "res"
  <*>       doc !? "type"
  <*>       doc !? "create-time"
  <*>       doc !? "update-time"
  <*>       doc !? "title"
  <*> Just (doc !? "summary")
  <*> Just (doc !? "whose")
  <*> Just (doc !? "mime")
  <*>  m2l (doc !? "tags")
  where
    m2l (Just xs) = Just xs
    m2l _         = Just []

instance ToJSON ResT where
  toJSON ResT{..} = object
    [ "index"       .= rIndex
    , "type"        .= rType
    , "create-time" .= rCTime
    , "update-time" .= rUTime
    , "title"       .= rTitle
    , "summary"     .= rSummary
    , "whose"       .= rWhose
    , "mime"        .= rMIME
    , "tags"        .= rTags
    ]


-- | the type-class which means mongoDB available.
class (MonadIO m,MonadBaseControl IO m) => Mongodic a m | m -> a where
  getDefaultAccessMode :: m AccessMode      -- ^ get the accedd mode
  getDefaultDb          :: m Database        -- ^ get the default database
  getDbUP              :: m (T.Text,T.Text) -- ^ get the user and pass
  getPool                :: m ConnectionPool  -- ^ get the connection pool


-- | fetch context
fetchContext :: (MonadIO m,Val a)
              => T.Text    -- ^ field name
              -> ResT      -- ^ resource index
              -> T.Text    -- ^ collection
              -> Action m (Maybe a) -- ^ result
fetchContext field ResT{..} =  ((!? field) <%>).findOne.select ["_id" =: rRes]

-- | fetch resource index
fetchRes :: MonadIO m
          => [T.Text]
          -> Action m (Maybe ResT)
fetchRes index = (docToRes <%>) . findOne $ select ["index" =: index] "index"

-- | fetch all resource index
fetchResAll :: (MonadIO m, MonadBaseControl IO m)
                 => Action m [ResT]
fetchResAll = do
  cur <- find $ select [] "index"
  rt  <- rest cur
  closeCursor cur
  return . catMaybes $ docToRes <$> rt

-- | update context
updateContext :: (MonadIO m, Val a)
               => T.Text            -- ^ collection
               -> Maybe ObjectId    -- ^ obj id of item
               -> T.Text            -- ^ field name
               -> a
               -> Action m ObjectId -- ^ return id
updateContext c oid field v = case oid of
  Just i -> upsert (select ["_id" =: i] c) [field =: v] >> return i
  _      -> (\(ObjId i) -> i) <$> insert c [field =: v]

-- | the update for item
updateItem :: (MonadIO m, Val a)
            => T.Text   -- ^ type, or say collection
            -> T.Text   -- ^ field name
            -> a        -- ^ item
            -> ResT     -- ^ ``undefined'' ResT
            -> Action m ()
updateItem t f v uR = do
  let index = rIndex uR
  res <- fetchRes index
  rr <- if (rType <$> res) /= Just t
        then deleteContextMaybe res >> return Nothing
        else return $ rRes <$> res
  rO <- updateContext t rr f v
  updateRes (uR {rRes = rO})

-- | the update for resource
updateRes :: MonadIO m
           => ResT     -- ^ the index
           -> Action m ()
updateRes res@ResT{..} =
  upsert (select ["index" =: rIndex] "index") $ resToDoc res

-- | delete the context
deleteContext :: MonadIO m
               => ResT           -- ^ index
               -> T.Text         -- ^ collection
               -> Action m ()
deleteContext ResT{..} c =
  delete $ select ["_id" =: rRes] c


-- | delete resource
deleteRes :: MonadIO m
           => ResT        -- ^ index
           -> Action m ()
deleteRes ResT{..} =
  delete $ select ["index" =: rIndex] "index"

-- | delete the resouce in maybe
deleteContextMaybe :: MonadIO m
                     => Maybe ResT -- ^ index
                     -> Action m ()
deleteContextMaybe (Just r) = deleteContext r $ rType r
deleteContextMaybe _        = return ()

-- | delete item
deleteItem :: MonadIO m
            => [T.Text] -- ^ url
            -> T.Text   -- ^ collection
            -> Action m ()
deleteItem index c = fetchRes index >>=
  (\res -> case res of
      Just r -> deleteContext r c >> deleteRes r
      _      -> return ())

-- | Binary to ByteString
fromBinary :: Binary -> B.ByteString
fromBinary (Binary b) = b


-- | update nothing
infix 0 =@
(=@) :: Val v
     => Label        -- ^ label
     -> Maybe v      -- ^ value
     -> Maybe Field  -- ^ maybe field
(=@) l = ((Just.(l =:)) =<<)
