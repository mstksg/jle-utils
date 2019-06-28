{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ViewPatterns              #-}

module JUtils.Stackage (
    GHCMajor(..)
  , GHCFull(..)
  , Resolver(..)
  , resolverGHC
  , latestNightly
  , prettyMajor, prettyFull
  , majorParser, fullParser
  ) where

import           Control.Exception
import           Control.Lens hiding        ((<.>))
import           Control.Monad.Except
import           Data.Aeson.Lens
import           Data.Char
import           Data.Foldable
import           Data.Proxy
import           Data.Semigroup
import           Data.Void
import           GHC.Generics               (Generic)
import           JUtils.Github
import           Network.Curl
import           System.FilePath
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Yaml                  as Y
import qualified GitHub                     as G
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char.Lexer as P

data GHCMajor = GHCMajor { gmA :: Int, gmB :: Int }
  deriving (Show, Eq, Ord, Bounded, Generic)

data GHCFull  = GHCFull { gfMajor :: GHCMajor, gfMinor :: Int }
  deriving (Show, Eq, Ord, Bounded, Generic)

data Resolver = LTS Int Int
              | Nightly Int Int Int
  deriving (Show, Eq, Ord, Generic)

resolverUrl :: Resolver -> FilePath
resolverUrl (LTS x y) = "lts" </> show x
                              </> show y
                              <.> "yaml"
resolverUrl (Nightly y m d) = "nightly" </> show y
                                        </> show m
                                        </> show d
                                        <.> "yaml"

sOwner :: G.Name G.Owner
sOwner = "commercialhaskell"

sRepo :: G.Name G.Repo
sRepo = "stackage-snapshots"

snapshotsUrl :: String
snapshotsUrl = "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master"

resolverGHC
    :: Resolver
    -> IO GHCFull
resolverGHC r = do
    tyaml <- withCurlDo $ do
      (_,tfull) <- curlGetString (snapshotsUrl </> resolverUrl r) []
      Y.decodeThrow @_ @Y.Object . T.encodeUtf8 . T.pack $ tfull
    maybe (throwIO (userError "no parse ghc version")) pure $
      tyaml ^? ((ix "compiler") <> (ix "resolver" . key "compiler"))
             . _String
             . to (T.drop 4)
             . folding (P.parse fullParser "resolver file")

latestNightly :: Github (Int, Int, Int)
latestNightly = do
    G.Branch{..} <- maybe (throwError (G.UserError "No master")) pure
                  . find ((== "master") . G.branchName)
                  . toList
                =<< liftReq (G.branchesForR sOwner sRepo G.FetchAll)
    G.GitCommit{..} <- liftReq $ G.gitCommitR
        sOwner
        sRepo
        (G.mkName (Proxy @G.GitCommit) (G.branchCommitSha branchCommit))
    tr <- fmap treeMap
        . liftReq
        $ G.nestedTreeR sOwner sRepo (G.treeSha gitCommitTree)
    let candidate = fmap getMax
                  . foldMap (fmap Max . getNightly . T.unpack)
                  . M.keys
                  . M.mapMaybe (either (const Nothing) Just)
                  $ tr
    case candidate of
      Nothing -> throwError $ G.UserError "No nightlies"
      Just c  -> pure c
  where
    getNightly :: String -> Maybe (Int, Int, Int)
    getNightly fp
      | ["nightly/",y,m,d] <- splitPath fp
      = (,,) <$> readMaybe (filter isDigit y)
             <*> readMaybe (filter isDigit m)
             <*> readMaybe (filter isDigit d)
      | otherwise
      = Nothing

prettyMajor :: GHCMajor -> String
prettyMajor GHCMajor{..} = printf "%d.%d" gmA gmB

prettyFull :: GHCFull -> String
prettyFull GHCFull{..} = printf "%s.%d" (prettyMajor gfMajor) gfMinor

majorParser :: (P.Stream s, P.Token s ~ Char) => P.Parsec Void s GHCMajor
majorParser = GHCMajor <$> P.decimal
                       <*> (P.single '.' *> P.decimal)

fullParser :: (P.Stream s, P.Token s ~ Char) => P.Parsec Void s GHCFull
fullParser = GHCFull <$> majorParser
                     <*> (P.single '.' *> P.decimal)
