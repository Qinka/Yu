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
