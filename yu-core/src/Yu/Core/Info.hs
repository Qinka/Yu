{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module        : Yu.Core.Info
Description   : THe info of this package
Copyright     : Qinka 2017
License       : GPL-3
Maintainer    : qinka@live.com
                me@qinka.pro
Stability     : experimental
Portability   : x86/64

The information of thos package, such as version, git commit-id.
-}





module Yu.Core.Info
       ( -- * TemplateHaskell's variables
         yuCoreVersion
       , yuCoreVersionQuote
       , yuCoreGitBranchQuote
       , yuCoreGitCommitQuote
       , yuBuildInfoQuote
       ) where

import           Data.Char
import           Data.Time
import           Paths_yu_core
import           System.Info
import           Yu.Import.TH
import           Yu.Import.Version

-- | The version of this package, in Data.Version.Version
yuCoreVersion :: Version -- ^ The version
yuCoreVersion = version
-- | The version of the package, in a Q Exp
yuCoreVersionQuote :: Q Exp -- ^ String
yuCoreVersionQuote = stringE $ showVersion version



-- | The commit of the git
yuCoreGitCommitQuote :: Q Exp -- ^ String
yuCoreGitCommitQuote = gitHash
-- | The commit of the branch
yuCoreGitBranchQuote :: Q Exp -- ^ String
yuCoreGitBranchQuote = gitBranch

-- | build information
yuBuildInfoQuote :: Q Exp -- ^ String
yuBuildInfoQuote = do
  timeStr <- formatTime defaultTimeLocale "-%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  stringE $ os ++ "-" ++ arch ++ "-"  ++ map toUpper compilerName
    ++ "-" ++ showVersion compilerVersion ++ timeStr
    ++ "-git:" ++ $gitBranch ++ ":" ++ $gitHash



-- | Use these as a string
-- @
--   putStrLn $YuCoreVersionQuote ++ $YuCoreGitCommitQuote
-- @
