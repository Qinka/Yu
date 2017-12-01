
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
import           Yu.Tool.Ih
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
            Ih{..}     -> ihHandler     it
            Init{..}   -> initHandler   it
            New{..}    -> newHandler    it
            Del{..}    -> delHandler    it
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



