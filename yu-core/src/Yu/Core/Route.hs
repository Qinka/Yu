{-
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

Module      : Yu.Core.Route
Description : Export the default route configuration of Yu.
Copyright   : (C) Qinka 2017 - 2018
License     : GPL3
Maintainer  : me@qinka.pro
Stability   : experimental
Portability : unknow

Export the default router of site.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Yu.Core.Route
  ( yuRoute
  ) where

import           Yesod.Core
import           Yesod.Routes.TH.Types
import           Yu.Core.Control

-- | default router
yuRoute :: [ResourceTree String]
yuRoute = [parseRoutes| /*Texts UrlR GET PUT DELETE |]
