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
Module       : Yu.Core.View
Description  : The view of Yu
Copyright    : (C) 2017-2018 Johann Lee <me@qinka.pro>
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknow

The view part of the Yu.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}


module Yu.Core.View
  ( -- * responds
    respondPost
  , respondResourceT
  , respondResourceB
  , respondStatic
  , -- * reexport module
    module Yu.Core.View.Internal
  , module Yu.Core.View.Query
  ) where

import           Control.Monad.Writer.Lazy
import           Data.Maybe
import           Data.Monoid
import           Network.HTTP.Types        (status301)
import           Text.Blaze.Html           (Html, preEscapedToHtml)
import           Yesod.Core
import           Yesod.Core.Handler
import           Yesod.Core.Widget
import           Yu.Core.Model             (ResT (..))
import           Yu.Core.View.Internal
import           Yu.Core.View.Query
import           Yu.Import.ByteString      (ByteString)
import qualified Yu.Import.ByteString      as B
import           Yu.Import.Text            (Text)
import qualified Yu.Import.Text            as T
import           Yu.Utils.Handler

-- | response the post with ResT and html
respondPost :: (Yesod a, Hamletic a (HandlerT a IO))
             => ResT -- ^ resource index
             -> Html -- ^ html body
             -> HandlerT a IO TypedContent
respondPost res@ResT{..} rawBody = do
  willRaw <- getRaw
  isRaw   <- null <$> lookupHeader "YuRAW"
  body    <- if willRaw == isRaw then return rawBody
             else defaultLayout $ withHtml rawBody res
  respondSource "text/html" $ do
    sendChunkHtml body
    sendFlush

-- | with tags, import tags to js
withTags :: [Text] -- ^ tags
          -> WidgetT site IO ()
withTags tags = let tagsJ = toJSON tags in toWidget [julius|tags=#{tagsJ};|]

-- | with summary, import summary to page when it not Nothing
withSummary :: Maybe Html -- ^ summary html
             -> WidgetT site IO ()
withSummary (Just summaryHtml) = [whamlet|<summary id=sum>#{summaryHtml}|]
withSummary _                  = return ()

-- | with whose, import the author to the js
withWhose :: Maybe Text -- ^ author
           -> WidgetT site IO ()
withWhose (Just whose) = let w = showJs whose in toWidget [julius|author=#{w};|]
withWhose _                    = return ()

-- | with html combine the parts to one
withHtml :: Html -- ^ the html for main part
          -> ResT -- ^ resource
          -> WidgetT site IO ()
withHtml body ResT{..} = do
  setTitle $ toHtml rTitle
  withSummary $ preEscapedToHtml <$> rSummary
  [whamlet|#{body}|]
  withWhose rWhose
  withTags rTags


-- | respond resource(text)
respondResourceT :: (Yesod a, Hamletic a (HandlerT a IO))
                   => ResT    -- ^ resource index
                   -> Text    -- ^ text
                   -> HandlerT a IO TypedContent
respondResourceT ResT{..} text = do
  respondSource (fromMaybe "" $ fmap T.encodeUtf8 rMIME) $ do
    sendChunkText text
    sendFlush

-- | respond resource(binary)
respondResourceB :: (Yesod a, Hamletic a (HandlerT a IO))
                   => ResT    -- ^ resource index
                   -> ByteString    -- ^ text
                   -> HandlerT a IO TypedContent
respondResourceB ResT{..} bin = do
  respondSource (fromMaybe "" $ fmap T.encodeUtf8 rMIME) $ do
    sendChunkBS bin
    sendFlush


-- | response the static url
respondStatic :: (Yesod a, Hamletic a (HandlerT a IO))
               => ResT -- ^ index for resource
               -> Text -- ^ Url
               -> HandlerT a IO TypedContent
respondStatic _ url = redirectWith status301 url
