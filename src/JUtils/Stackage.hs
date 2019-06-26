{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ViewPatterns              #-}

module JUtils.Stackage (
    GHCMajor(..)
  , GHCFull(..)
  , Resolver(..)
  , VersionInfo(..)
  , versionInfo
  , prettyMajor, prettyFull
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson.Lens
import           Data.Bifunctor
import           Data.ByteString        (ByteString)
import           Data.Char
import           Data.Foldable
import           Data.Map               (Map)
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup
import           Data.Set               (Set)
import           Data.Text              (Text)
import           GHC.Generics
import           JUtils.Github
import           Numeric.Natural
import           Text.Printf
import           Text.Read              (readMaybe)
import qualified Data.Binary            as Bi
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Yaml              as Y
import qualified GitHub                 as G

data GHCMajor = GHCMajor { gmA :: Int, gmB :: Int }
  deriving (Show, Eq, Ord, Generic)

data GHCFull  = GHCFull { gfMajor :: GHCMajor, gfMinor :: Int }
  deriving (Show, Eq, Ord, Generic)

data Resolver = LTS Natural
              | Nightly
  deriving (Show, Eq, Ord, Generic)

newtype VersionInfo = VI
    { viMap :: Map GHCMajor (Int, Set Resolver)
    }
  deriving (Show, Eq, Ord, Generic)

instance Bi.Binary GHCMajor
instance Bi.Binary GHCFull
instance Bi.Binary Resolver
instance Bi.Binary VersionInfo

ghOwner :: G.Name G.Owner
ghOwner = "commercialhaskell"

ghRepo :: G.Name G.Repo
ghRepo = "stackage"

versionInfo :: Github VersionInfo
versionInfo = fmap ( VI
                   . (fmap . first) getMax
                   . M.fromListWith (<>)
                   . map (uncurry process)
                   . M.toList
                   )
            . traverse branchVersion
          =<< allLTS
  where
    process :: Resolver -> GHCFull -> (GHCMajor, (Max Int, Set Resolver))
    process r GHCFull{..} = ( gfMajor, (Max gfMinor, S.singleton r) )

allLTS :: Github (Map Resolver G.Branch)
allLTS = M.fromList
       . mapMaybe (\b -> (,b) <$> findLTS (G.branchName b))
       . toList
     <$> liftReq (G.branchesForR ghOwner ghRepo G.FetchAll)
  where
    findLTS b
      | ("lts", y) <- T.splitAt 3 b
            = fmap LTS . readMaybe . takeWhile isDigit . T.unpack $ y
      | b == "nightly"
            = Just Nightly
      | otherwise
            = Nothing

branchVersion :: G.Branch -> Github GHCFull
branchVersion G.Branch{..} = do
    G.GitCommit{..} <- liftReq $ G.gitCommitR
        ghOwner
        ghRepo
        (G.mkName (Proxy @G.GitCommit) (G.branchCommitSha branchCommit))
    tr <- liftReq $ G.treeR ghOwner ghRepo (G.treeSha gitCommitTree)
    let mp = M.mapMaybe (either (const Nothing) Just) $ treeMap tr
    bcyml <- getBlob "build-constraints.yaml" mp
    bc    <- liftEither
           . first (G.UserError . T.pack . Y.prettyPrintParseException)
           $ Y.decodeEither' @Y.Value bcyml
    maj <- m2e (G.UserError "No parse") $
      bc ^? key "ghc-major-version" . _String . folding parseMajor
    dk    <- T.decodeUtf8 <$> getBlob "Dockerfile" mp
    m2e (G.UserError "No minor version") $
      fullByMajor maj dk

m2e :: MonadError e m => e -> Maybe a -> m a
m2e e = maybe (throwError e) pure

getBlob :: Text -> Map Text (G.Name G.Blob) -> Github ByteString
getBlob fp mp = case M.lookup fp mp of
    Nothing -> throwError . G.UserError $ "No file " <> fp
    Just b  -> fmap (B64.decodeLenient . T.encodeUtf8 . G.blobContent)
             . liftReq
             $ G.blobR ghOwner ghRepo b

treeMap :: G.Tree -> Map Text (Either (G.Name G.Tree) (G.Name G.Blob))
treeMap G.Tree{..} = M.fromList
                   . mapMaybe go
                   . toList
                   $ treeGitTrees
  where
    go G.GitTree{..} = (gitTreePath,) <$> x
      where
        x | gitTreeType == "blob" = Just . Right . G.mkName (Proxy @G.Blob) $ G.untagName gitTreeSha
          | gitTreeType == "tree" = Just . Left  . G.mkName (Proxy @G.Tree) $ G.untagName gitTreeSha
          | otherwise             = Nothing

fullByMajor :: GHCMajor -> Text -> Maybe GHCFull
fullByMajor x = fmap (GHCFull x . getFirst)
              . foldMap (fmap First . uncurry go)
              . T.breakOnAll (T.pack pm)
  where
    pm   = prettyMajor x
    go _ = readMaybe
         . takeWhile isDigit
         . drop (length pm + 1)
         . T.unpack

prettyMajor :: GHCMajor -> String
prettyMajor GHCMajor{..} = printf "%d.%d" gmA gmB

prettyFull :: GHCFull -> String
prettyFull GHCFull{..} = printf "%s.%d" (prettyMajor gfMajor) gfMinor

parseMajor :: Text -> Maybe GHCMajor
parseMajor t = case T.unpack <$> T.splitOn "." t of
    [x,y] -> GHCMajor <$> readMaybe x <*> readMaybe y
    _     -> Nothing
