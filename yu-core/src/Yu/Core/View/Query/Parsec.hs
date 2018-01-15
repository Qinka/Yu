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
Module      : Yu.Core.View.Query.Parsec
Description : The parsec for query command
Copyright   : (C) 2017-2018 Johann Lee
License     : GPL v3+
Maintainer  : me@qinka.pro
Stability   : experimental
Portability : unknown

This module is for the query command, to parsec the query command
-}

{-# LANGUAGE RecordWildCards #-}

module Yu.Core.View.Query.Parsec
  ( -- | parse the query command for query
    --
    -- $query
    runQp
  ) where

import           Data.Time
import           Text.Parsec
import           Yu.Core.Model  (ResT (..))
import qualified Yu.Import.Text as T

-- $query commands
--
-- The query commands include
-- * type={t,f}={post,text,binary,query,..}
-- * take=SIZE
-- * drop=SIZE
-- * befor=DATE
-- * after=DATE
-- * tag={t,f}=TAG
-- * or
-- * and
-- * true
-- * false


-- | the ADT for parser
data QueryParser = QPTake  Int                 -- ^ like @take@
                 | QPDrop  Int                 -- ^ like @drop@
                 | QPBefor UTCTime      Bool   -- ^ select those whose dates are earlier than given date
                 | QPAfter UTCTime      Bool   -- ^ select those whose dates are later than given date
                 | QPTag   Bool         String -- ^ select those whose tags include or not include given tag
                 | QPType  Bool         String -- ^ select those whose content's type include or not include
                                               --   given type
                 | QPOr   [QueryParser]        -- ^ like @or@
                 | QPAnd  [QueryParser]        -- ^ like @and@
                 deriving (Show)


-- | get an empty command
qpEmpty :: Parsec String () [QueryParser]
qpEmpty = do
  string ";"
  return []
-- | get the command type
qpType :: Parsec String () [QueryParser]
qpType = do
  t <- string "type=" *> oneOf ['t','f'] <* char '='
  typ <- many letter <* char ';'
  return [QPType (t=='t') typ]
-- | get command take
qpTake :: Parsec String () [QueryParser]
qpTake = do
  len <- string "take=" *> many (oneOf ['0'..'9']) <* char ';'
  return [QPTake (read len)]
-- | get command drop
qpDrop :: Parsec String () [QueryParser]
qpDrop = do
  len <- string "drop=" *> many (oneOf ['0'..'9']) <* char ';'
  return [QPDrop (read len)]
-- | get command befor
qpBefor :: Parsec String () [QueryParser]
qpBefor = do
  date <- parseTimeM True defaultTimeLocale "%F+%T" =<<
    (string "befor=" *> many (noneOf "@") <* char '@')
  b <- true <|> false <* char ';'
  return [QPBefor date b]
-- | get true
true :: Parsec String () Bool
true = string "true" >> return True
-- | get false
false :: Parsec String () Bool
false = string "false" >> return False
-- | get command after
qpAfter :: Parsec String () [QueryParser]
qpAfter = do
  date <- parseTimeM True defaultTimeLocale "%F+%T" =<<
    (string "after=" *> many (noneOf "@") <* char '@')
  b <- true <|> false <* char ';'
  return [QPAfter date b]
-- | get command tag
qpTag :: Parsec String () [QueryParser]
qpTag = do
  t <- string "tag=" *> oneOf ['t','f'] <* char '='
  tag <- many (noneOf ";") <* char ';'
  return [QPTag (t=='t') tag]
-- | get command and
qpAnd :: Parsec String () [QueryParser]
qpAnd = do
  sub <- char '[' *> many qps <* char ']'
  return [QPAnd $ concat sub]
-- | get command or
qpOr :: Parsec String () [QueryParser]
qpOr = do
  sub <- char '{' *> many qps <* char '}'
  return [QPOr $ concat sub]
-- | parser for query command
qp :: Parsec String () [QueryParser]
qp =  concat <$> many qps
-- | single command
qps :: Parsec String () [QueryParser]
qps = foldl (<|>) qpEmpty $ try <$>
  [qpTake,qpDrop,qpBefor,qpAfter,qpTag,qpOr,qpAnd
  , qpType
  ]

-- | transform to filter
toFilter :: [QueryParser] -> ([ResT] -> [ResT])
toFilter [] = id
toFilter (QPTake i:xs) = take i . toFilter xs
toFilter (QPDrop i:xs) = drop i . toFilter xs
toFilter (QPBefor i b:xs) = filter (timeFilter i (>) b) . toFilter xs
toFilter (QPAfter i b:xs) = filter (timeFilter i (<) b) . toFilter xs
toFilter (QPTag  t i:xs) = filter ((==t) . tagFilter i) . toFilter xs
toFilter (QPType t i:xs) = filter ((==t) . typFilter i) . toFilter xs
toFilter (QPOr s:xs) = concat . map sg . toFilter xs
  where funcs = (\y -> toFilter [y]) <$> s
        sg y = take 1 $ concatMap (\f -> f [y]) funcs
toFilter (QPAnd s:xs) = toFilter s . toFilter xs

typFilter :: String -> ResT -> Bool
typFilter t ResT{..} = rType == T.pack t
tagFilter :: String -> ResT -> Bool
tagFilter t ResT{..} = T.pack t `elem` rTags
timeFilter :: UTCTime -> (UTCTime -> UTCTime -> Bool) -> Bool -> ResT -> Bool
timeFilter t o b ResT{..} = t `o` resTime
  where resTime = if b then rCTime else rUTime

-- | transform the command to function
runQp :: String -> Either ParseError ([ResT]->[ResT])
runQp str = toFilter <$> runP qp () "QueryPaserError" str

