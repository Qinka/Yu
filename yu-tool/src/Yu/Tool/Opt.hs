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
Module:       Yu.Tool.Opt
Description:  Opt for command
Copyright:    (C) 2017-2018 Johann Lee <me@qinka.pro>
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

Opt for command
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Yu.Tool.Opt
  ( yu
  , Yu(..)
  ) where


import           System.Console.CmdArgs

-- | command
data Yu = Ih -- ^ identification helper
            { ihToken :: Maybe String
            , ihHash  :: Maybe String
            , ihDebug :: Bool
            }
          | Init -- ^ init a repo
            { initSiteUrl   :: Maybe String
            , initTokenFile :: Maybe String
            }
          | New -- ^ create new one
            { newId      :: Maybe String        -- ^ id
            , newTyp     :: Maybe String        -- ^ type
            , newPath    :: Maybe String        -- ^ path (url)
            , newTitle   :: Maybe String        -- ^ title
            , newMIME    :: Maybe String  -- ^ mime
            , newTags    :: [String]      -- ^ tags
            , newSum     :: Maybe String  -- ^ summary for item
            , newContent :: Maybe String        -- ^ path for item
            , newWhose   :: Maybe String  -- ^ owner
            }
          | Del -- ^ delete one
            { delId :: Maybe String -- ^ id
            }
          | Make -- ^ build make file
            { mkItem :: Maybe String -- ^ make kind
            , mkOut  :: Maybe String -- ^ output file relate to repo
            }
          | Nav  -- ^ about nav
            { navOpt   :: Maybe String
            , navLabel :: Maybe String
            , navOrder :: Maybe Int
            , navUrl   :: Maybe String
            }
          | Script -- ^ about script
            { sptKind :: Maybe String
            }
          | Path
            { pathItem :: Maybe String
            }
          | Other -- other command
            { oCmds :: [String]
            }
            deriving (Show,Data)

ih :: Yu
ih = Ih { ihToken = def
                 &= help "token of identifying"
                 &= typ "TOKEN"
                 &= explicit &= name "token"
                 &= explicit &= name "t"
        , ihHash  = def
                 &= help "hash algorithm"
                 &= typ "shaXX"
                 &= explicit &= name "hash"
                 &= explicit &= name "h"
        , ihDebug = False
          &= help "debug to show the token"
          &= typ "BOOL"
          &= explicit &= name "debug"
          &= explicit &= name "d"
        }

init_ :: Yu
init_ = Init { initSiteUrl = def
               &= help "the url"
               &= typ "URL"
               &= explicit &= name "url"
               &= explicit &= name "u"
             , initTokenFile = def
               &= help " the path for the file where hold token"
               &= typFile
               &= explicit &= name "token"
               &= explicit &= name "t"
            }

new :: Yu
new = New { newId = def
            &= help "the id of item"
            &= typ "ID"
            &= explicit &= name "id"
            &= explicit &= name "i"
          , newTyp = def
            &= help "the type of item"
            &= typ "TYPE"
            &= explicit &= name "type"
            &= explicit &= name "k"
          , newPath = def
            &= help "the url of item"
            &= typ "URL"
            &= explicit &= name "url"
            &= explicit &= name "u"
          , newTitle = def
            &= help "the title of item"
            &= typ "TEXT"
            &= explicit &= name "title"
            &= explicit &= name "h"
          , newMIME = def
            &= help "the mime for the res"
            &= typ "MIME"
            &= explicit &= name "mime"
            &= explicit &= name "m"
          , newTags = def
            &= help "the tag(s) of item"
            &= typ "TAG"
            &= explicit &= name "tag"
            &= explicit &= name "t"
          , newSum = def
            &= help "the summory of item"
            &= typ "TEXT|FILE"
            &= explicit &= name "summary"
            &= explicit &= name "s"
          , newContent = def
            &= help "the context's path"
            &= typFile
            &= explicit &= name "content"
            &= explicit &= name "c"
          , newWhose = def
            &= help"the owner"
            &= typ "OWNER"
            &= explicit &= name "whose"
            &= explicit &= name "w"
          }

del :: Yu
del = Del { delId = def
            &= help "id"
            &= typ "ID"
            &= explicit &= name "id"
            &= explicit &= name "i"
          }

make :: Yu
make = Make { mkItem = def
              &= help "label"
              &= typ "ID"
              &= explicit &= name "label"
              &= explicit &= name "l"
            , mkOut = def
              &= help "output"
              &= typFile
              &= explicit &= name "output"
              &= explicit &= name "o"
            }

nav :: Yu
nav = Nav { navOpt = def
            &= help "opt"
            &= typ "add|del"
            &= explicit &= name "opt"
            &= explicit &= name "o"
          , navLabel = def
            &= help "label"
            &= typ "LABEL"
            &= explicit &= name "label"
            &= explicit &= name "l"
          , navUrl = def
            &= help "name"
            &= typ "URL"
            &= explicit &= name "url"
            &= explicit &= name "u"
          , navOrder = def
            &= help "order"
            &= typ "INT"
            &= explicit &= name "order"
            &= explicit &= name "r"
          }

script :: Yu
script = Script { sptKind = def
                  &= help "scirpt"
                  &= typ "TEXT"
                  &= explicit &= name "kind"
                  &= explicit &= name "k"
                }

path :: Yu
path = Path { pathItem = def
              &= help "get the paths"
              &= typ "label"
              &= explicit &= name "label"
              &= explicit &= name "l"
            }


other :: Yu
other = Other { oCmds = def
                &= help "sciprt"
                &= args
              }


yu :: Yu
yu = modes [ ih
             , init_
             , new
             , del
             , make
             , nav
             , script
             , path
             ]
  &= program "yu"
  &= summary "summary"
