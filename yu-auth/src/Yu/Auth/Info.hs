{-|
Module        : Yu.Auth.Info
Description   : THe info of this package
Copyright     : (C) Qinka 2017
License       : GPLv3
Maintainer    : me@qinka.pro
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

module Yu.Auth.Info
       ( -- * TemplateHaskell's variables
         yuAuthVersion
       , yuAuthVersionQuote
       , yuAuthGitBranchQuote
       , yuAuthGitCommitQuote
       ) where

import           Paths_yu_auth
import           Yu.Import.TH
import           Yu.Import.Version

-- | The version of this package, in Data.Version.Version
yuAuthVersion :: Version -- ^ The version
yuAuthVersion = version
-- | The version of the package, in a Q Exp
yuAuthVersionQuote :: Q Exp -- ^ String
yuAuthVersionQuote = stringE $ showVersion version



-- | The commit of the git
yuAuthGitCommitQuote :: Q Exp -- ^ String
yuAuthGitCommitQuote = gitHash
-- | The commit of the branch
yuAuthGitBranchQuote :: Q Exp -- ^ String
yuAuthGitBranchQuote = gitBranch


-- | Use these as a string
-- @
--   putStrLn $YuAuthVersionQuote ++ $YuAuthGitCommitQuote
-- @
