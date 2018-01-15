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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Network.Wai.Internal
import           Test.Hspec
import qualified Test.Hspec.Wai       as Test
import           Yesod.Core
import           Yesod.Test
import           Yu.Auth
import qualified Yu.Import.ByteString as B
import qualified Yu.Import.Text       as T

data App = App

mkYesod "App" [parseRoutes| auth AuthR GET POST |]

instance Yesod App where
  isAuthorized _ _ = do -- HandlerT _ IO
    method <- requestMethod <$> waiRequest
    case method of
      "GET" -> return Authorized
      _     -> checkAuth

instance Auth App SHA1 where
  tokenHash _ = return SHA1
  tokenItem _ = return "12345qwert"


getAuthR :: Handler T.Text
getAuthR = return "get"

postAuthR :: Handler T.Text
postAuthR = return "post"

appSpec :: Spec
appSpec = before (return (App,id)) $ do -- YesodExample
  describe "Yu-auth test(GET)" $ do
    it "load via get method with token" $ do
      request $ do -- RequestBuilder _
        setMethod "GET"
        setUrl AuthR
        hash  <- tokenHash App
        token <- generateHash hash <$> tokenItem App
        addRequestHeader ("token",token)
      statusIs 200
    it "load via get without token" $ do
      get AuthR
      statusIs 200
  describe "Yu-auth test(POST)" $ do
    it "load via post method with token" $ do
      request $ do -- RequestBuilder _
        setMethod "POST"
        setUrl AuthR
        hash  <- tokenHash App
        token <- generateHash hash <$> tokenItem App
        addRequestHeader ("token",token)
      statusIs 200
    it "load via post methdo with token" $ do
      post AuthR
      statusIs 403

main :: IO ()
main = hspec appSpec

