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

-- | The test module for model for glob
module ModelSpec
  ( spec
  ) where

import           Control.Monad
import           Data.IORef
import qualified Data.Text.Lazy           as TL
import           Data.Time                (UTCTime, diffUTCTime, getCurrentTime)
import           Internal
import           System.IO
import           Test.Hspec
import           Text.Blaze.Renderer.Text
import           Yu.Core.Model
import qualified Yu.Import.Text           as T

-- | step one: get pipe
getPipe :: IO Pipe
getPipe = connect $ readHostPort "localhost:27017"

cleanDatabase :: IO ()
cleanDatabase = void $ runDB $ dropDatabase testDBName


spec :: Spec
spec = do
  runIO $ putStrLn "Test for Model part for Yu"
  describe "Nav and Document" $ do
    let navItem  = Nav { navUrl   = "url"
                       , navLabel = "label"
                       , navOrder = 0
                       }
        docItem  = [ "index" =: ("label" :: String)
                   , "url"   =: ("url" :: String)
                   , "order" =: (0 :: Int)
                   ]
        docItem' = [ "labsel" =: ("label" :: String)
                   , "ural"   =: ("url" :: String)
                   , "order"  =: (0 :: Int)
                   ]
    it "transform nav to doc" $
      navToDoc navItem  `shouldMatchList` docItem
    it "transform doc to nav (just)" $
      docToNav docItem  `shouldBe` Just navItem
    it "transform doc to nav(nothing)" $
      docToNav docItem' `shouldBe` Nothing
  describe "ResT and Document" $ do
    now <- runIO $ getCurrentTime
    let resItem = ResT { rIndex   = ["the","path","to","resource"]
                       , rRes     = Oid 233 233
                       , rType    = "type"
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
        docItem   = [ "index"       =: ["the","path","to","resource" :: String]
                    , "res"         =: Oid 233 233
                    , "type"        =: ("type" :: String)
                    , "create-time" =: now
                    , "update-time" =: now
                    , "title"       =: ("title" :: String)
                    , "summary"     =: Just ("summary" :: String)
                    , "whose"       =: Just ("whose" :: String)
                    , "mime"        =: Just ("mime" :: String)
                    , "tags"        =: ["tag","s" :: String]
                    ]
        docItem'  = [ "inddex"      =: ["the","path","to","resource" :: String]
                    , "res"         =: Oid 233 233
                    , "type"        =: ("type" :: String)
                    , "create-time" =: now
                    , "update-time" =: now
                    , "title"       =: ("title" :: String)
                    , "summary"     =: Just ("summary" :: String)
                    , "whose"       =: Just ("whose" :: String)
                    , "mime"        =: Just ("mime" :: String)
                    , "tags"        =: ["tag","s" :: String]
                    ]
    it "transfrom res to doc" $
      resToDoc resItem  `shouldMatchList` docItem
    it "transform doc to res(just)" $
      docToRes docItem  `shouldBe` Just resItem
    it "transform doc to res(nothing)" $
      docToRes docItem' `shouldBe` Nothing
  describe "database actions" $ do
    describe "for context" $ beforeAll_ cleanDatabase $ do
      let contextTestCollection = "ctc"
          contextTestField      = "ctf"
          contextTestItem       = "cti"    :: String
          contextTestItemNew    = "ctinew" :: String
          contextTestOidBad (Oid a b) = Oid (a-1) (b-1)
      contextTestOid <- runIO $ newIORef Nothing
      it "update a context(new)" $ do
        cto <- readIORef contextTestOid
        oid <- runDB $ updateContext
          contextTestCollection
          cto
          contextTestField
          contextTestItem
        Just doc <- runDB $ findOne (select ["_id" =: oid] contextTestCollection)
        writeIORef contextTestOid $ Just oid
        doc `shouldContain` [ "_id"            =: oid
                            , contextTestField =: contextTestItem
                            ]
      it "update context(again)" $ do
        cto <- readIORef contextTestOid
        print cto
        oid <- runDB $ updateContext
          contextTestCollection
          cto
          contextTestField
          contextTestItemNew
        Just doc <- runDB $ findOne (select ["_id" =: oid] contextTestCollection)
        writeIORef contextTestOid $ Just oid
        doc `shouldContain` [ "_id"            =: oid
                            , contextTestField =: contextTestItemNew
                            ]
      it "fetch context(just)" $ do
        Just cto <- readIORef contextTestOid
        rt <- runDB $ fetchContext
          contextTestField
          ResT {rRes = cto}
          contextTestCollection
        rt `shouldBe` Just contextTestItemNew
      it "fetch context(nothing)" $ do
        Just cto <- readIORef contextTestOid
        rt <- runDB $ fetchContext
          contextTestField
          ResT {rRes = contextTestOidBad cto}
          contextTestCollection
        let shouldBE :: Maybe String -> Maybe String -> IO ()
            shouldBE = shouldBe
        rt `shouldBE` Nothing
      it "delete context" $ do
        Just cto <- readIORef contextTestOid
        runDB $ deleteContext ResT{rRes = cto} contextTestCollection
        rt <- runDB $ rest =<< find (select [] contextTestCollection)
        rt `shouldBe` []
    describe "for res" $ beforeAll_ cleanDatabase $ do
      now <- runIO $ getCurrentTime
      let resItem1 = ResT { rIndex   = ["the","path","to","resource","1"]
                          , rRes     = Oid 233 233
                          , rType    = "type"
                          , rCTime   = now
                          , rUTime   = now
                          , rTitle   = "title"
                          , rSummary = Just "summary"
                          , rWhose   = Just "whose"
                          , rMIME    = Just "mime"
                          , rTags    = ["tag","s"]
                          }
          resItem2 = ResT { rIndex   = ["the","path","to","resource","2"]
                          , rRes     = Oid 233 233
                          , rType    = "type"
                          , rCTime   = now
                          , rUTime   = now
                          , rTitle   = "title"
                          , rSummary = Just "summary"
                          , rWhose   = Just "whose"
                          , rMIME    = Just "mime"
                          , rTags    = ["tag","s"]
                          }
          docItem1  = [ "index"       =: ["the","path","to","resource","1" :: String]
                      , "res"         =: Oid 233 233
                      , "type"        =: ("type" :: String)
                      , "create-time" =: now
                      , "update-time" =: now
                      , "title"       =: ("title" :: String)
                      , "summary"     =: Just ("summary" :: String)
                      , "whose"       =: Just ("whose" :: String)
                      , "mime"        =: Just ("mime" :: String)
                      , "tags"        =: ["tag","s" :: String]
                      ]
          docItem2  = [ "index"       =: ["the","path","to","resource","2" :: String]
                      , "res"         =: Oid 233 233
                      , "type"        =: ("type" :: String)
                      , "create-time" =: now
                      , "update-time" =: now
                      , "title"       =: ("title" :: String)
                      , "summary"     =: Just ("summary" :: String)
                      , "whose"       =: Just ("whose" :: String)
                      , "mime"        =: Just ("mime" :: String)
                      , "tags"        =: ["tag","s" :: String]
                      ]
      it "update a new resource index" $ do
        runDB $ updateRes resItem1
        rt:_ <- runDB $ rest =<< find (select [] "index")
        docToRes rt `shouldBe` Just resItem1
      it "fetch a resource index" $ do
        rt1 <- runDB $ fetchRes ["the","path","to","resource","1"]
        rt2  <- runDB $ fetchRes ["the","path","to","resource","2"]
        rt1 `shouldBe` Just resItem1
        rt2 `shouldBe` Nothing
      it "fetch all inedxes, when there are indexes in database" $ do
        runDB $ updateRes resItem2
        rt <- runDB $ fetchResAll
        rt `shouldBe` [resItem1,resItem2]
      it "delete a index" $ do
        runDB $ deleteRes resItem1
        rt1:_ <- runDB $ rest =<< find (select [] "index")
        docToRes rt1 `shouldBe` Just resItem2
        runDB $ deleteRes resItem2
        rt2 <- runDB $ rest =<< find (select [] "index")
        rt2 `shouldBe` []
      it "fetch all indexes, when database is empty" $ do
        rt <- runDB $ fetchResAll
        rt `shouldBe` []
    describe "for item" $ beforeAll_ cleanDatabase $ do
      now <- runIO $ getCurrentTime
      let itemTestTypeOld = "itto"
          itemTestType    = "itt" :: T.Text
          itemTestIndex   = ["i","t","i"]
          itemTestField   = "itf"
          itemTestItem    = "item" :: T.Text
          resItemOld = ResT { rIndex   = itemTestIndex
                            , rRes     = undefined
                            , rType    = itemTestTypeOld
                            , rCTime   = now
                            , rUTime   = now
                            , rTitle   = "title"
                            , rSummary = Just "summary"
                            , rWhose   = Just "whose"
                            , rMIME    = Just "mime"
                            , rTags    = ["tag","s"]
                            }
          resItem = ResT { rIndex   = itemTestIndex
                         , rRes     = undefined
                         , rType    = itemTestType
                         , rCTime   = now
                         , rUTime   = now
                         , rTitle   = "title"
                         , rSummary = Just "summary"
                         , rWhose   = Just "whose"
                         , rMIME    = Just "mime"
                         , rTags    = ["tag","s"]
                         }
          docC = [ itemTestField =: itemTestItem ]
          docR = [ "index"       =: itemTestIndex
                 , "type"        =: itemTestType
                 , "create-time" =: now
                 , "update-time" =: now
                 , "title"       =: ("title" :: String)
                 , "summary"     =: Just ("summary" :: String)
                 , "whose"       =: Just ("whose" :: String)
                 , "mime"        =: Just ("mime" :: String)
                 , "tags"        =: ["tag","s" :: String]
                 ]
      it "update an old item" $ do
        runDB $ updateItem
          itemTestTypeOld
          itemTestField
          itemTestItem
          resItemOld
        rt1:_ <- runDB $ rest =<< find (select [] itemTestTypeOld)
        rt2:_ <- runDB $ rest =<< find (select [] "index")
        rt1 `shouldContain` docC
        let Just rt2' = docToRes rt2
        rt2' `shouldBe` resItemOld {rRes = rRes rt2'}
      it "update an new item" $ do
        runDB $ updateItem
          itemTestType
          itemTestField
          itemTestItem
          resItem
        rt1:_ <- runDB $ rest =<< find (select [] itemTestType)
        rt3   <- runDB $ rest =<< find (select [] itemTestTypeOld)
        rt2:_ <- runDB $ rest =<< find (select [] "index")
        rt1 `shouldContain` docC
        rt3 `shouldBe` []
        let Just rt2' = docToRes rt2
        rt2' `shouldBe` resItem {rRes = rRes rt2'}
      it "delete the item" $ do
        runDB $ deleteItem itemTestIndex itemTestType
        rt1 <- runDB $ rest =<< find (select [] itemTestType)
        rt2 <- runDB $ rest =<< find (select [] "index")
        rt1 `shouldBe` []
        rt2 `shouldBe` []
    describe "for update/fetch" $ do
      describe "for frame" $ beforeAll_ cleanDatabase $ do
        now <- runIO $ getCurrentTime
        let res = ResT { rIndex   = itemIndex
                       , rRes     = undefined
                       , rType    = itemType
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
            doc = [ "index"       =: itemIndex
                  , "type"        =: itemType
                  , "create-time" =: now
                  , "update-time" =: now
                  , "title"       =: ("title" :: String)
                  , "summary"     =: Just ("summary" :: String)
                  , "whose"       =: Just ("whose" :: String)
                  , "mime"        =: Just ("mime" :: String)
                  , "tags"        =: ["tag","s" :: String]
                  ]
            itemType = "frame"
            itemIndex = ["path","to","item"]
            item    = "123"
            itemDoc = ["html" =: item]
        it "update" $ do
          runDB $ updateFrame item res
          rt1:_ <- runDB $ rest =<< find (select [] itemType)
          rt2:_ <- runDB $ rest =<< find (select [] "index")
          rt1 `shouldContain` itemDoc
          let Just rt2' = docToRes rt2
          rt2' `shouldBe` res {rRes = rRes rt2'}
        it "fetch" $ do
          Just r  <- runDB $ fetchRes   itemIndex
          Just rt <- runDB $ fetchFrame r
          renderMarkup rt `shouldBe` TL.fromStrict item
      describe "for html" $ beforeAll_ cleanDatabase $ do
        now <- runIO $ getCurrentTime
        let res = ResT { rIndex   = itemIndex
                       , rRes     = undefined
                       , rType    = itemType
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
            doc = [ "index"       =: itemIndex
                  , "type"        =: itemType
                  , "create-time" =: now
                  , "update-time" =: now
                  , "title"       =: ("title" :: String)
                  , "summary"     =: Just ("summary" :: String)
                  , "whose"       =: Just ("whose" :: String)
                  , "mime"        =: Just ("mime" :: String)
                  , "tags"        =: ["tag","s" :: String]
                  ]
            itemType = "post"
            itemIndex = ["path","to","item"]
            item    = "123"
            itemDoc = ["html" =: item]
        it "update" $ do
          runDB $ updatePost item res
          rt1:_ <- runDB $ rest =<< find (select [] itemType)
          rt2:_ <- runDB $ rest =<< find (select [] "index")
          rt1 `shouldContain` itemDoc
          let Just rt2' = docToRes rt2
          rt2' `shouldBe` res {rRes = rRes rt2'}
        it "fetch" $ do
          Just r  <- runDB $ fetchRes   itemIndex
          Just rt <- runDB $ fetchPost r
          renderMarkup rt `shouldBe` TL.fromStrict item
      describe "for text" $ beforeAll_ cleanDatabase $ do
        now <- runIO $ getCurrentTime
        let res = ResT { rIndex   = itemIndex
                       , rRes     = undefined
                       , rType    = itemType
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
            doc = [ "index"       =: itemIndex
                  , "type"        =: itemType
                  , "create-time" =: now
                  , "update-time" =: now
                  , "title"       =: ("title" :: String)
                  , "summary"     =: Just ("summary" :: String)
                  , "whose"       =: Just ("whose" :: String)
                  , "mime"        =: Just ("mime" :: String)
                  , "tags"        =: ["tag","s" :: String]
                  ]
            itemType = "resource"
            itemIndex = ["path","to","item"]
            item    = "123"
            itemDoc = ["text" =: item]
        it "update" $ do
          runDB $ updateResourceT item res
          rt1:_ <- runDB $ rest =<< find (select [] itemType)
          rt2:_ <- runDB $ rest =<< find (select [] "index")
          rt1 `shouldContain` itemDoc
          let Just rt2' = docToRes rt2
          rt2' `shouldBe` res {rRes = rRes rt2'}
        it "fetch" $ do
          Just r  <- runDB $ fetchRes   itemIndex
          Just rt <- runDB $ fetchResourceT r
          rt `shouldBe` item
      describe "for binary" $ beforeAll_ cleanDatabase $ do
        now <- runIO $ getCurrentTime
        let res = ResT { rIndex   = itemIndex
                       , rRes     = undefined
                       , rType    = itemType
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
            doc = [ "index"       =: itemIndex
                  , "type"        =: itemType
                  , "create-time" =: now
                  , "update-time" =: now
                  , "title"       =: ("title" :: String)
                  , "summary"     =: Just ("summary" :: String)
                  , "whose"       =: Just ("whose" :: String)
                  , "mime"        =: Just ("mime" :: String)
                  , "tags"        =: ["tag","s" :: String]
                  ]
            itemType = "resource"
            itemIndex = ["path","to","item"]
            item    = "123"
            itemDoc = ["binary" =: Binary item]
        it "update" $ do
          runDB $ updateResourceB (Binary item) res
          rt1:_ <- runDB $ rest =<< find (select [] itemType)
          rt2:_ <- runDB $ rest =<< find (select [] "index")
          rt1 `shouldContain` itemDoc
          let Just rt2' = docToRes rt2
          rt2' `shouldBe` res {rRes = rRes rt2'}
        it "fetch" $ do
          Just r  <- runDB $ fetchRes   itemIndex
          Just rt <- runDB $ fetchResourceB r
          rt `shouldBe` item
      describe "for static" $ beforeAll_ cleanDatabase $ do
        now <- runIO $ getCurrentTime
        let res = ResT { rIndex   = itemIndex
                       , rRes     = undefined
                       , rType    = itemType
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
            doc = [ "index"       =: itemIndex
                  , "type"        =: itemType
                  , "create-time" =: now
                  , "update-time" =: now
                  , "title"       =: ("title" :: String)
                  , "summary"     =: Just ("summary" :: String)
                  , "whose"       =: Just ("whose" :: String)
                  , "mime"        =: Just ("mime" :: String)
                  , "tags"        =: ["tag","s" :: String]
                  ]
            itemType = "static"
            itemIndex = ["path","to","item"]
            item    = "123"
            itemDoc = ["url" =: item]
        it "update" $ do
          runDB $ updateStatic item res
          rt1:_ <- runDB $ rest =<< find (select [] itemType)
          rt2:_ <- runDB $ rest =<< find (select [] "index")
          rt1 `shouldContain` itemDoc
          let Just rt2' = docToRes rt2
          rt2' `shouldBe` res {rRes = rRes rt2'}
        it "fetch" $ do
          Just r  <- runDB $ fetchRes   itemIndex
          Just rt <- runDB $ fetchStatic r
          rt `shouldBe` item
      describe "for query" $ beforeAll_ cleanDatabase $ do
        now <- runIO $ getCurrentTime
        let res = ResT { rIndex   = itemIndex
                       , rRes     = undefined
                       , rType    = itemType
                       , rCTime   = now
                       , rUTime   = now
                       , rTitle   = "title"
                       , rSummary = Just "summary"
                       , rWhose   = Just "whose"
                       , rMIME    = Just "mime"
                       , rTags    = ["tag","s"]
                       }
            doc = [ "index"       =: itemIndex
                  , "type"        =: itemType
                  , "create-time" =: now
                  , "update-time" =: now
                  , "title"       =: ("title" :: String)
                  , "summary"     =: Just ("summary" :: String)
                  , "whose"       =: Just ("whose" :: String)
                  , "mime"        =: Just ("mime" :: String)
                  , "tags"        =: ["tag","s" :: String]
                  ]
            itemType = "query"
            itemIndex = ["path","to","item"]
            item    = "123"
            itemDoc = ["var" =: item]
        it "update" $ do
          runDB $ updateQuery item res
          rt1:_ <- runDB $ rest =<< find (select [] itemType)
          rt2:_ <- runDB $ rest =<< find (select [] "index")
          rt1 `shouldContain` itemDoc
          let Just rt2' = docToRes rt2
          rt2' `shouldBe` res {rRes = rRes rt2'}
        it "fetch" $ do
          Just r  <- runDB $ fetchRes   itemIndex
          Just rt <- runDB $ fetchQuery r
          rt `shouldBe` item
    describe "for navigate bar" $ beforeAll_ cleanDatabase $ do
      let label = "label"
          url   = "url"
          order = 0
          nav = Nav { navLabel = label
                    , navUrl   = url
                    , navOrder = order
                    }
          doc = [ "index" =: label
                , url     =: url
                , "order" =: order
                ]
      it "update/add one" $ do
        runDB $ updateNav (Just label) (Just url) (Just order)
        rt:_ <- runDB $ rest =<< find (select [] "nav")
        rt `shouldContain` doc
      it "fetch" $ do
        rt:_ <- runDB $ fetchNav
        rt `shouldBe` nav
      it "delete one" $ do
        runDB $ updateNav (Just label) (Just url) (Just order)
        runDB $ updateNav (Just "345") (Just url) (Just order)
        runDB $ updateNav (Just "123") (Just url) (Just order)
        runDB $ deleteNav $ Just "123"
        rt <- runDB $ fetchNav
        length rt `shouldBe` 3
      it "delete all" $ do
        runDB $ deleteNav Nothing
        rt <- runDB $ fetchNav
        rt `shouldBe` []







