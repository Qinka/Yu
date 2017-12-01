{-|
Module:      Yu.Utils.Info
Description: The information of the package
Copyright:   (C) Qinka 2017
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The information and version of the package.
-}

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
