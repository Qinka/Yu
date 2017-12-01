{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.New
  ( newHandler
  ) where

import           System.Directory
import           System.IO
import           Yu.Import
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString.Lazy as BL
import           Yu.Tool.Opt
import           Yu.Tool.Repo

newHandler :: Yu -> IO ()
newHandler New{..} = findRepo yuRepoName >>= \repo' -> case repo' of
  Just repo -> do
    case (newId,newTyp,newPath,newContent) of
      (Just nid, Just ntyp, Just npath, Just ncontent) -> do
        sum <- toSummary newSum repo
        cur <- getCurrentTime
        item <- makePathRelateRepo repo $
          Item { iSummary = Summary sum
               , iMIME    = newMIME
               , iPath    = npath
               , iWhose   = newWhose
               , iCreTime = cur
               , iId      = nid
               , iContent = ncontent
               , iTitle   = newTitle
               , iType    = ntyp
               , iTags    = newTags
               }
        BL.writeFile (repo ++ "/" ++ yuRepoName ++ "/" ++ nid ++ ".item.json") $ encode item
      _ -> hPutStrLn stderr "error: one(some) of id, typ, path, or content is(are) empty"
  where toSummary (Just s) repo = do
          is <- makeAbsolute s >>= doesPathExist
          return $ (if is then Left else Right) s
        toSummary Nothing _ = return $ Right ""
