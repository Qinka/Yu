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
