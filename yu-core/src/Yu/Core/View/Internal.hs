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
Module       : Yu.Core.View.Internal
Description  : The internal module for view
Copyright    : (C) 2017-2018 Johann Lee <me@qinka.pro>
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknown

The internal party for View, including Hamletic.
-}


{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}


module Yu.Core.View.Internal
  ( Hamletic(..)
  , yuLayout
  , yuErrorHandler
    -- * layouts
  , layoutBootstrap
  ) where

import           Yesod.Core
import           Yesod.Core.Handler
import           Yesod.Core.Json
import           Yu.Core.Model
import           Yu.Import.Text     (Text)
import qualified Yu.Import.Text     as T
import           Yu.Utils.Handler

-- | Hamtletic
--
--   Limit, and test
class (MonadHandler m, Mongodic a m) => Hamletic a m | m -> a where
  getTitle        :: m Text  -- ^ get title
  getFramePrefix :: m Text  -- ^ get the prefix path of frame
  getVersion      :: m Text  -- ^ get the version of blog itself or application
  getRaw          :: m Bool  -- ^ return raw html


-- | layout for layout
type YuLayout site = (  PageContent (Route site)
                       -> Html -- hd
                       -> Text -- title
                       -> Html -- nav
                       -> Html -- top
                       -> Html -- bottom
                       -> ((Route site -> [(Text, Text)] -> Text) -> Html)
                       )

-- | layout with bootstrap
layoutBootstrap :: Yesod site => YuLayout site
layoutBootstrap pageContent hd title nav top bottom = [hamlet|
  $newline never
  $doctype 5
  <html>
    <head>
      <title> #{pageTitle pageContent} - #{title}
      <meta charset=utf-8>
      <script src=prelude.js>
      <meta name=viewport content="width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no">
      <script src=https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML async></script>
      #{hd}
      ^{pageHead pageContent}
    <body>
      #{nav}
      <div id="container">
        #{top}
        <div id="main-part">
          ^{pageBody pageContent}
      #{bottom}
  |]

{-|
The example, or say template for layout

@
layoutXx pageContent hd title nav top bottom = [hamlet|
  $newline never
  $doctype 5
  \<html>\
    \<head\>
      \<title\> #{pageTitle pageContent} - #{title}
      \<meta charset=utf-8\>
      \<meta name=viewport content="width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no"\>
      #{hd}
      ^{pageHead pageContent}
    \<body\>
      #{nav}
      #{top}
      ^{pageBody pageContent}
      #{bottom}
  |]
@
-}

-- | the default of yu with Yesod
yuLayout :: (Hamletic a (HandlerT a IO),Yesod a)
            => YuLayout a        -- ^ the layout for yu
            -> WidgetT a IO ()     -- ^ widget
            -> HandlerT a IO Html  -- ^ return
yuLayout layout w = do
  framePrefix <- getFramePrefix
  title        <- getTitle
  pageContent <- widgetToPageContent w
  htmls <- runDbDefault $ do
    topHtml    <- fetchMaybeI fetchFrame [framePrefix,"top"]
    bottomHtml <- fetchMaybeI fetchFrame [framePrefix,"bottom"]
    navHtml    <- fetchMaybeI fetchFrame [framePrefix,"nav"]
    header      <- fetchMaybeI fetchFrame [framePrefix,"header"]
    case (topHtml,bottomHtml,navHtml,header) of
      (Just top, Just bottom, Just nav, Just hd) -> return $ Right (top,bottom,nav,hd)
      _                                          -> return $ Left "cannot launch frames"
  case htmls of
    Left err -> error err
    Right (top,bottom,nav,hd) -> withUrlRenderer $ layout pageContent hd title nav top bottom



-- | handler the error
yuErrorHandler :: Yesod site
                      => ErrorResponse -- ^ error
                      -> HandlerT site IO TypedContent
yuErrorHandler er = selectRep $ do
  provideJson er
  provideRep $
    defaultLayout [whamlet|
                          <h1> error
                          <p> #{T.show er}
                          |]


