{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module JUtils.Github (
    Github
  , runGithub
  , liftReq
  ) where

import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Functor.Combinator
import           GitHub

data GithubF a = forall x. FromJSON x =>
        GithubF (Request 'RO x) (x -> a)

deriving instance Functor GithubF

newtype Github a = Github { unGithub :: ExceptT Error (Free GithubF) a }
  deriving (Functor, Applicative, Monad, MonadError Error)

runGithub :: Github a -> Auth -> IO (Either Error a)
runGithub = runReaderT . runExceptT . embed (interpret go) . unGithub
  where
    go :: GithubF ~> ExceptT Error (ReaderT Auth IO)
    go (GithubF x f) = do
      a <- ask
      fmap f . ExceptT . liftIO $ executeRequest a x

liftReq :: FromJSON a => Request 'RO a -> Github a
liftReq = Github . lift . inject . (`GithubF` id)
