{-|
Module:      Yu.Utils.Info
Description: The information of the package
Copyright:   (C) Qinka 2017
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The information and version of the package.
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

module Yu.Utils.Info
       ( -- * version
         yuUtilsVersion
       , -- * quotes
         yuUtilsVersionQuote
       , yuUtilsGitBranchQuote
       , yuUtilsGitCommitQuote
       ) where

import           Paths_yu_utils
import           Yu.Import.TH
import           Yu.Import.Version


-- | Version of yu-common
yuUtilsVersion :: Version
yuUtilsVersion = version

-- | Version of yu-common (quote)
yuUtilsVersionQuote :: Q Exp
yuUtilsVersionQuote = stringE $ showVersion version

-- | Git Branch (quote)
yuUtilsGitBranchQuote :: Q Exp
yuUtilsGitBranchQuote = gitBranch

-- | Git Branch (quote)
yuUtilsGitCommitQuote :: Q Exp
yuUtilsGitCommitQuote = gitHash
