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
Module:       Yu.Tool.MakeM
Description:  The MakeM
Copyright:    (C) 2017-2018 Johann Lee <me@qinka.pro>
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown


-}

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Yu.Tool.MakeM
  ( MakeM(..)
  , endLine
  , linesPrefix
  , string
  , stringT
  , stringLnT
  , charT
  , target
  , comment
  , macro
  , macroM
  , echo
  , curl
  , curlF
  , curlPath
  , curlDetail
  , siteURL
  , updateTime
  , siteToken
  , (\=\)
  ) where

import           Data.String
import           Data.Text.Internal.Builder hiding (fromString)
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.Lazy             as TL
import qualified Yu.Import.Text             as T

-- | MakeM
data MakeM a = MakeM { mkBuilder  :: Builder
                     , mkConstant :: a
                     }

instance Functor MakeM where
  fmap f (MakeM mf c) = MakeM mf (f c)

instance Applicative MakeM where
  pure = MakeM mempty
  (<*>) (MakeM a f) (MakeM b c) = MakeM (a `mappend` b) (f c)

instance Monad MakeM where
  (>>=) (MakeM a c) f = let MakeM b t = f c
                        in MakeM (a `mappend` b) t

instance (() ~ a) => IsString (MakeM a) where
  fromString str = MakeM (TB.fromString str) undefined

instance Show a => Show (MakeM a) where
  show (MakeM mf _) = TL.unpack (toLazyText mf)

-- endline for MakeM
endLine :: MakeM ()
endLine = MakeM "\n" ()

-- | add prefix for each line
--   o(n)
linesPrefix :: T.Text -- ^ prefix
            -> MakeM a -- ^ items
            -> MakeM a
linesPrefix prefix (MakeM builder c) =
  let (t:ts) = map (\x -> fromText prefix `mappend` fromText x) $
               T.lines $ TL.toStrict $ toLazyText builder
      new    = foldl (\a b -> a `mappend` singleton '\n' `mappend` b) t ts
  in MakeM new c

-- | transform string(String) to MakeM
string :: String -> MakeM ()
string str = MakeM (TB.fromString str) ()

-- | transform the string(Data.Text.Text) to MakeM 
stringT :: T.Text -> MakeM ()
stringT str = MakeM (fromText str) ()

-- | transform the string(Data.Text.Text) to MakeM (with end of line)
stringLnT :: T.Text -> MakeM ()
stringLnT str = MakeM (fromText str `mappend` "\n") ()

-- | transform the char to MakeM
charT :: Char -> MakeM ()
charT c = MakeM (singleton c) ()

-- | create a target rule
target :: T.Text -- ^ target
       -> [T.Text] -- ^ dependences
       -> MakeM () -- ^ body
       -> MakeM ()
target tar deps body = do
  "\n"
  stringT tar
  charT ':'
  mapM_ (\t -> charT ' ' >> stringT t) deps >> endLine
  linesPrefix "\t" body

-- | Comments
comment :: T.Text
        -> MakeM ()
comment c = linesPrefix "# " (stringT c) >> "\n"

-- | assign the value
(\=\) :: T.Text -> T.Text -> MakeM ()
var \=\ value = MakeM (fromText var `mappend` " = " `mappend` fromText value `mappend` singleton '\n') ()

-- | wrap a macro
macro :: T.Text -> T.Text
macro str = "${" `T.append` str `T.append` "}"
-- | call a macro
macroM :: T.Text -> MakeM ()
macroM = stringT . macro

-- | echo
echo :: T.Text -> MakeM ()
echo t = MakeM ("echo" `mappend` fromText t `mappend` singleton '\n') ()

-- | curl
curl :: T.Text -- ^ flags
     -> T.Text -- ^ http method
     -> T.Text -- ^ url
     -> [(T.Text,T.Text)] -- ^ settings
     -> MakeM ()
curl flags method url settings = do
  macroM curlPath >> " " >> macroM curlDetail >> " " >> stringT flags >> " \\\n"
  linesPrefix "\t" $ do
    "-X " >> stringT method >> " \\\n"
    let putSetting (label,value) = stringT $ "-F \"" `T.append` label `T.append` "="
          `T.append` value `T.append` "\" \\\n"
    mapM_ putSetting settings
    "-H \"Authorization:" >> macroM siteToken >> "\" \\\n"
    stringT url

-- curlF to pair
curlF :: T.Text -- param
      -> T.Text -- value
      -> (T.Text,T.Text) -- pair
curlF = (,)


----------------------
-- setttings
----------------------
-- | curl's program name(path) macro
curlPath :: T.Text
curlPath = "CURL_PATH"
-- | curl's option to show details
curlDetail :: T.Text
curlDetail = "CURL_DETAIL"
-- | site url
siteURL :: T.Text
siteURL = "SITE_URL"
-- | now
updateTime :: T.Text
updateTime = "NOW_TIME"
-- | token
siteToken :: T.Text
siteToken = "SITE_TOKEN"
