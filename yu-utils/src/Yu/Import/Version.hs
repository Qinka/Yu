{-|
Module:      Yu.Import.Version
Description: The reexport of the ``version''
Copyright:   (C) Qinka 2017
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of many
-}


module Yu.Import.Version
       ( -- * the reexported module
         module Data.Version
       , module Development.GitRev
       , module System.Info
       , -- * the methods
         compileTime
       , compileOs
       , compileCompiler
       ) where

import           Data.Char
import           Data.Version
import           Development.GitRev
import           System.Info
import           Yu.Import
import           Yu.Import.TH


-- | Get the time of compiling
compileTime :: Q Exp
compileTime = do
  now <- formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  stringE now

-- | Get the os of compiler-host
compileOs :: Q Exp
compileOs = stringE $ os ++ "-" ++ arch

-- | Get the compiler system
compileCompiler :: Q Exp
compileCompiler = stringE $ toUpper <$> compilerName ++ "-" ++ showVersion compilerVersion
