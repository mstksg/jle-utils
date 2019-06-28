{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module JUtils.Github (
    Github
  , runGithub
  , runGithubWith
  , liftReq
  , treeMap
  ) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Foldable
import           Data.Functor.Combinator
import           Data.Map                 (Map)
import           Data.Maybe
import           Data.Proxy
import           Data.Text                (Text)
import           GHC.Generics
import           GitHub
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified Data.ByteString          as BS
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Yaml                as Y
import qualified System.Console.Haskeline as H

data GithubF a = forall x. FromJSON x =>
        GithubF (Request 'RO x) (x -> a)

deriving instance Functor GithubF

newtype Github a = Github { unGithub :: ExceptT Error (Free GithubF) a }
  deriving (Functor, Applicative, Monad, MonadError Error)

newtype GithubConfig = GC
    { gcAuthToken :: Text
    }
  deriving (Generic, Show, Eq)

instance FromJSON GithubConfig where
    parseJSON = genericParseJSON defaultOptions
      { fieldLabelModifier = camelTo2 '-' . drop 2 }
instance ToJSON GithubConfig where
    toJSON = genericToJSON defaultOptions
      { fieldLabelModifier = camelTo2 '-' . drop 2 }

configFile :: IO GithubConfig
configFile = do
    dir <- getXdgDirectory XdgConfig "jle-utils"
    let fp = dir </> "github.yaml"
    createDirectoryIfMissing True dir
    bse <- tryJust (guard . isDoesNotExistError) $
                BS.readFile fp
    case bse of
      Right bs -> case Y.decodeEither' @GithubConfig bs of
        Left  e -> throwIO . userError $ Y.prettyPrintParseException e
        Right c -> pure c
      Left _   -> do
        c <- H.runInputT H.defaultSettings $
                H.getPassword Nothing "Enter OAuth Token: "
        case mfilter (== "") . fmap (T.strip . T.pack) $ c of
          Nothing -> throwIO . userError $ "No token inputted."
          Just t  -> do
            let gc = GC { gcAuthToken = t }
            gc <$ Y.encodeFile fp gc

runGithub :: Github a -> IO (Either Error a)
runGithub x = do
    GC{..} <- configFile
    runGithubWith x (OAuth (T.encodeUtf8 gcAuthToken))

runGithubWith :: Github a -> Auth -> IO (Either Error a)
runGithubWith = runReaderT . runExceptT . embed (interpret go) . unGithub
  where
    go :: GithubF ~> ExceptT Error (ReaderT Auth IO)
    go (GithubF x f) = do
      a <- ask
      fmap f . ExceptT . liftIO $ executeRequest a x

liftReq :: FromJSON a => Request 'RO a -> Github a
liftReq = Github . lift . inject . (`GithubF` id)

treeMap :: Tree -> Map Text (Either (Name Tree) (Name Blob))
treeMap Tree{..} = M.fromList
                   . mapMaybe go
                   . toList
                   $ treeGitTrees
  where
    go GitTree{..} = (gitTreePath,) <$> x
      where
        x | gitTreeType == "blob" = Just . Right . mkName (Proxy @Blob) $ untagName gitTreeSha
          | gitTreeType == "tree" = Just . Left  . mkName (Proxy @Tree) $ untagName gitTreeSha
          | otherwise             = Nothing

