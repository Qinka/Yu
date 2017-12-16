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


{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where


import           Control.Exception
import           Control.Monad
import           Data.Either
import           System.Console.CmdArgs
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import qualified Yu.Import.ByteString   as B
import           Yu.Tool.Del
import           Yu.Tool.Token
import           Yu.Tool.Init
import           Yu.Tool.Make
import           Yu.Tool.Nav
import           Yu.Tool.New
import           Yu.Tool.Opt
import           Yu.Tool.Script


main :: IO ()
main = do
  args' <- getArgs
  rt <- try $ cmdArgs yu :: IO (Either ExitCode Yu)
  case rt of
    Left e   -> otherProg e args'
    Right it -> matchArgs it
    -- callProcess fc fo
  where matchArgs it =
          case it of
            Token{..}     -> tokenHandler     it
            Init{..}   -> initHandler   it
            New{..}    -> newHandler    it
            Delete{..}    -> delHandler    it
            Make{..}   -> makeHandler   it
            Nav{..}    -> navHandler    it
            Script{..} -> scriptHandler it
        otherProg ExitSuccess _ = return ()
        otherProg e1 (fc:fo) = do
          rt <- try $ callProcess ("yu-" ++ fc) fo :: IO (Either ExitCode ())
          when (isLeft rt) $ do
            let Left e2 = rt
            print e1
            print e2



