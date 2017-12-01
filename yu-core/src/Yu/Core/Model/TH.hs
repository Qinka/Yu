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
