{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
    ( main
    ) where

import Prelude hiding (readFile)

import Control.Exception.Base (bracket)
import Control.Monad (unless, void)
import Data.Aeson (eitherDecode, FromJSON)
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Foldable (for_)
import Data.List (words, isInfixOf, any)
import Data.List.Utils (replace)
import GHC.Generics
import System.Directory (listDirectory)
import System.Process (readProcess)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen, TagText), fromAttrib)

newtype URL = URL String
  deriving (Generic, Show, FromJSON)

newtype Blacklist = Blacklist [String]
  deriving (Generic, Show, FromJSON)

data Config = Config
  { url :: URL
  , blacklist :: Blacklist
  } deriving (Generic, Show, FromJSON)

newtype Name = Name String
  deriving Show
data Target = Target Name URL
  deriving Show

parseConfig :: ByteString -> Either String Config
parseConfig = eitherDecode

getFetchTargets :: URL -> Blacklist -> IO [Target]
getFetchTargets (URL url) (Blacklist blacklist) = do
  tags <- takeTriples . parseTags <$> readProcess "curl" ["-s", "--get", url] ""
  pure $ matchTargets =<< tags
  where
    takeTriples xs =
      zip3 xs (drop 1 xs) (drop 2 xs)
    matchTargets (td@(TagOpen "td" _), a@(TagOpen "a" _), TagText title) =
      if fromAttrib "class" td == "tlistname"
        then do
          let href = fromAttrib "href" a
          pure $ Target (Name title) (URL href)
        else mempty
    matchTargets _ = mempty

getExistingTargets :: IO [Name]
getExistingTargets = do
  files <- listDirectory "downloads"
  pure $ Name <$> files

filterTargets :: Blacklist -> [Name] -> [Target] -> [Target]
filterTargets (Blacklist blacklist) names =
  filter isNotBlacklisted . filter isNotDownloaded
  where
    isNotBlacklisted (Target (Name x) _) = not $ any (`isInfixOf` x) blacklist
    isNotDownloaded (Target (Name x) _) = not $ any (isInfixOf x) (extractNames <$> names)
    extractNames (Name x) = x

downloadTarget :: Target -> IO ()
downloadTarget (Target (Name name) (URL url))= do
  let url' = "https:" ++ replace "view" "download" url
  let path = "./downloads/" ++ name ++ ".torrent"
  void $ readProcess "curl" ["-s", url', "-o", path] ""
  putStrLn $ "downloaded " ++ path

main :: IO ()
main = do
  config <- parseConfig <$> readFile "config.json"
  case config of
    Right Config {url, blacklist} -> do
      fetchedTargets <- getFetchTargets url blacklist
      existingTargets <- getExistingTargets
      let targets = filterTargets blacklist existingTargets fetchedTargets
      if null targets
        then putStrLn "nothing new to download"
        else mapM_ downloadTarget targets
    Left errMsg ->
      putStrLn $ "Error parsing config: " ++ errMsg
