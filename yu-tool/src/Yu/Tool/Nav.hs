{-# LANGUAGE RecordWildCards #-}

module Yu.Tool.Nav
  ( navHandler
  ) where

import           Data.List                 (nub)
import           System.IO
import           Yu.Import.Aeson
import qualified Yu.Import.ByteString.Lazy as BL
import           Yu.Tool.Opt               (Yu (navLabel, navOpt, navOrder, navUrl))
import qualified Yu.Tool.Opt               as O
import           Yu.Tool.Repo

navHandler :: O.Yu -> IO ()
navHandler O.Nav{..} = do
  repo' <- findRepo yuRepoName
  case repo' of
    Nothing -> hPutStrLn stderr "Can not find the repo"
    Just repo -> let navListPath = repo ++ "/" ++ yuRepoName ++ "/navlist.json"
                 in  case navOpt of
      Nothing -> hPutStrLn stderr "Need options"
      Just "add" ->
        case (navLabel,navUrl,navOrder) of
          (Just label,Just url, Just order) -> do
            let nav = Nav order url label
            navs' <- decode <$> BL.readFile navListPath
            case navs' of
              Nothing   -> hPutStrLn stderr "Can not parse the nav list file"
              Just navs -> BL.writeFile navListPath $ encode $ nub (nav:navs)
          _ -> hPutStrLn stderr "need order, label, and url"
      Just "del" ->
        case navLabel of
          Nothing -> hPutStrLn stderr "Need nav's label"
          Just label -> do
            navs' <- decode <$> BL.readFile navListPath
            case navs' of
              Nothing   -> hPutStrLn stderr "Can not parse the nav list file"
              Just navs -> BL.writeFile navListPath $ encode $ filter (\x -> nLabel x /= label) navs
      _ -> hPutStrLn stderr "unknow option"
