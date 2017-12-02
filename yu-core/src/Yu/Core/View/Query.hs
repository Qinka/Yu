{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


{-|
Module        : Yu.Core.View.Query
Description   : The view for query and nav
Copyright     : (C) Qinka 2017
License       : GPLv3+
Maintainer    : me@qinka.pro
Stability     : experimental
Portability   : unknown

The View part for query command, nav query.
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



module Yu.Core.View.Query
       ( -- * querys
         -- ** query version
         queryVersion
       , queryVersionAuthor
       , queryVersionUtils
       , queryVersionCore
       , -- ** name
         queryName
       , -- ** build info
         queryBuildInfo
       , -- ** server time
         queryServerTime
       , -- ** navbar
         queryNav
       , -- ** index
         queryIndex
       , -- ** normal query
         queryQuery
       ) where

import           Data.Time
import           Yesod.Core
import           Yu.Auth.Info
import           Yu.Core.Info
import           Yu.Core.Model.Internal    (Nav, ResT)
import           Yu.Core.View.Internal
import           Yu.Core.View.Query.Parsec
import           Yu.Import
import           Yu.Import.Aeson
import           Yu.Import.Text            (Text)
import qualified Yu.Import.Text            as T
import           Yu.Utils.Info

-- | version of Blog version
queryVersion :: Hamletic a (HandlerT a IO)
              => HandlerT a IO TypedContent
queryVersion = getVersion >>= respondSource "text/plain" . sendChunkText

-- | version of yu-core
queryVersionAuthor :: HandlerT a IO TypedContent
queryVersionAuthor = respondSource "text/plain" $ sendChunkText $yuAuthVersionQuote

-- | version of yu-utils
queryVersionUtils :: HandlerT a IO TypedContent
queryVersionUtils = respondSource "text/plain" $ sendChunkText $yuUtilsVersionQuote

-- | version of glov-core
queryVersionCore :: HandlerT a IO TypedContent
queryVersionCore = respondSource "text/plain" $ sendChunkText $yuCoreVersionQuote

-- | name of this
queryName :: HandlerT a IO TypedContent
queryName = respondSource "text/plain" $ sendChunkText "Yu"

-- | build information
queryBuildInfo :: HandlerT a IO TypedContent
queryBuildInfo = respondSource "text/plain" $ sendChunkText $yuBuildInfoQuote

-- | server time
queryServerTime :: HandlerT a IO TypedContent
queryServerTime = T.show <$> (liftIO getCurrentTime) >>= respondSource "text/plain" . sendChunkText

-- | fetch the list for nav
queryNav :: [Nav] -- ^ navs (from Model)
          -> HandlerT a IO TypedContent
queryNav = respondSource "application/json" . sendChunkLBS . encode

-- | index
queryIndex :: String -- ^ parameters
            -> [ResT] -- ^ resources index (from Model)
            -> HandlerT a IO TypedContent
queryIndex t = respondSource "application/json" . sendChunkLBS . encode . (run $ runQp t)
  where run (Left e)  = error $ show e
        run (Right i) = i

-- | query
queryQuery :: Text -- ^ query value (from Model)
            -> HandlerT a IO TypedContent
queryQuery t = respondSource "text/plain" $ do
  sendChunkText t
  sendFlush

