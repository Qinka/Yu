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

module Main where

import           Control.Monad.Random
import           Data.Char
import           Data.String
import           System.Exit
import           System.Random
import           Yu.Auth.Core


import qualified Yu.Import.ByteString as B

main :: IO ()
main = do
  putStrLn "Yu.Auth.Core test"
  str <- mkRandomStrMax 100
  let token = generateHash SHA1 str
      rt1   = verifyHash SHA1         str  token
      rt2   = verifyHash SHA1 (B.init str) token
  print (rt1,rt2)
  if rt1 && (not rt2) then exitSuccess
    else exitFailure

mkRandomStr :: IsString str => Int -> IO str
mkRandomStr len = fromString . ((chr . (`mod` 128)) <$>) . take len <$> getRandoms

mkRandomStrMax :: IsString str => Int -> IO str
mkRandomStrMax len = (`mod` len) <$> getRandom >>= mkRandomStr
