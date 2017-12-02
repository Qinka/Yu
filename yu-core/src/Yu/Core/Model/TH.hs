{-|
Module        : Yu.Core.Model.TH
Description   : To generate the codes about module
Copyright     : (C) Qinka, 2017
Maintainer    : me@qinka.pro
License       : GPL3
Stability     : experimental
Portability   : unknown

The codes for generate the codes about modules
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


{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Yu.Core.Model.TH
       ( makeFetch
       , makeUpdate
       ) where

import           Control.Monad.IO.Class
import           Data.Char
import           Database.MongoDB       as Mongo
import           Yu.Core.Model.Internal
import           Yu.Import.TH


-- | Upper case the first letter
firstUpper :: String -> String
firstUpper (x:xs) = toUpper x:xs
firstUpper xs     = xs

-- | To create fetchXxx
makeFetch :: Name   -- ^ filter   name
          -> String -- ^ function name
          -> Name   -- ^ type
          -> String -- ^ field name
          -> String -- ^ collection
          -> Q [Dec]
makeFetch func n kind field collection =
  let name = mkName $ "fetch" ++  firstUpper n
      m    = mkName  "m"
      dec = SigD name $ ForallT [PlainTV m] [AppT (ConT ''MonadIO) (VarT m)]
        (AppT
         (AppT ArrowT (ConT ''ResT))
         (AppT (AppT (ConT ''Action) (VarT m)) (AppT (ConT ''Maybe) (ConT kind))))
      resv = mkName "res"
      body = FunD name
        [Clause [VarP resv] (NormalB $
                             (AppE
                              (AppE (VarE $ mkName "<#>") (VarE func))
                               (AppE (AppE (AppE (VarE 'fetchContext)
                                            (LitE $ StringL field))
                                      (VarE resv))
                                (LitE $ StringL collection)))) []]
  in return [dec,body]


-- | to create updateXxx
makeUpdate :: String -- ^ function name
            -> Name   -- ^ type
            -> String -- ^ field
            -> String -- ^ collection
            -> Q [Dec]
makeUpdate n kind f c =
  let field = LitE $ StringL f
      coll  = LitE $ StringL c
      name  = mkName $ "update" ++ firstUpper n
      m     = mkName "m"
      dec   = SigD name $ ForallT [PlainTV m] [AppT (ConT ''MonadIO) (VarT m)]
        (AppT
         (AppT ArrowT (ConT kind))
         (AppT (AppT ArrowT (ConT ''ResT))
          (AppT (AppT (ConT ''Action) (VarT m)) (ConT ''()))))
      body  = FunD name
        [Clause [] (NormalB $
                    (AppE (AppE (VarE 'updateItem) coll)
                     field)) []]
  in return [dec,body]
