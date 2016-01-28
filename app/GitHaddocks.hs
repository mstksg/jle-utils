{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson.TH
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Tagged
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics
import Git.Libgit2
import Git.Reference
import Git.Repository
import Git.Tree
import Git.Tree.Builder
import Git.Types
import System.Directory
import System.FilePath
import System.Process
import Text.Printf
import qualified Conduit                  as C
import qualified Data.Aeson.Types         as A
import qualified Data.ByteString          as B
import qualified Data.HashMap.Strict      as H
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import qualified Data.Yaml                as Y

ghPagesRef :: T.Text
ghPagesRef = "refs/heads/gh-pages"

-- data GHError = GHECreateReference

data LocalInfo = LI { liPath    :: String
                    , liVersion :: String
                    }
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = (drop 2 . map toLower) } ''LocalInfo)

main :: IO ()
main = runStderrLoggingT $ do
    -- assumes project is root
    projNameVer <- liftIO $ do
      projName <- takeBaseName <$> getCurrentDirectory

      localsData <- fmap liVersion
                  . (H.lookup projName =<<)
                  . Y.decode
                  . T.encodeUtf8
                  . T.pack
                <$> readCreateProcess (shell "stack query locals") ""

      case localsData of
        Nothing -> throwM $ ErrorCall "Could not resolve project version."
        Just v  -> return $ projName ++ "-" ++ v

    logDebugN ("Resolved project name and version: " <> T.pack projNameVer)

    dr <- liftIO $ do
        callCommand "stack haddock"
        readCreateProcess (shell "stack path --local-doc-root") ""

    now <- liftIO getCurrentTime

    let docRoot = zipWith const dr (drop 1 dr) </> projNameVer
        tStr    = formatTime defaultTimeLocale "%c" now
        commitMsg = T.pack $ printf "Update gh-pages documentation for %s at %s"
                                    projNameVer tStr

    logDebugN ("Resolved documentation root: " <> T.pack docRoot)

    C.runResourceT . withRepository lgFactory "./" $ do
      r <- getPagesRef
      -- liftIO . putStrLn $ "Reference object: " ++ show r
      c <- lookupCommit $ Tagged r
      toid <- createTree (buildDocTree docRoot)
      tEnts <- listTreeEntries =<< lookupTree toid
      logDebugN . T.pack $ printf "Built tree with contained %d files."
                                  (length tEnts)
      createCommit [commitOid c]
                   toid
                   (commitAuthor c)
                   (commitCommitter c)
                   commitMsg
                   (Just ghPagesRef)
      return ()

  -- lol, all that work and it ends with a shell script
  -- liftIO $ callCommand "git push"

    -- liftIO . print $ commitAuthor c
    -- t <- lookupTree (commitTree c)
    -- liftIO . print . map fst =<< listTreeEntries t

getPagesRef
    :: forall r m. (MonadGit r m, MonadLogger m, MonadThrow m)
    => m (Oid r)
getPagesRef = do
    rPages <- resolveReference ghPagesRef
    case rPages of
      Nothing -> do
        logWarnN "gh-pages branch not found"
        rTarget <- runMaybeT . asum
                 $ map (MaybeT . lookupReference) ["HEAD", "refs/heads/master"]
        case rTarget of
          Nothing -> throwM
                   $ ErrorCall "No reference \"HEAD\" or \"master\" to create gh-pages branch from."
          Just r -> do
            logWarnN "Creating branch gh-pages"
            createReference ghPagesRef r
            rPages' <- resolveReference ghPagesRef
            case rPages' of
              Just r' -> return r'
              Nothing -> throwM
                       $ ErrorCall "Error creating gh-pages branch."
      Just r -> return r

    -- return undefined

buildDocTree
    :: forall m r. (C.MonadResource m, MonadIO m, MonadLogger m, MonadGit r m)
    => FilePath
    -> TreeT r m ()
buildDocTree rt = go ""
  where
    go :: FilePath -> TreeT r m ()
    go bn = do
      fns <- liftIO $ getDirectoryContents (rt </> bn)
      forM_ (filter (`notElem` [".",".."]) fns) $ \fn -> do
        let fullFn = T.unpack . T.pack $ rt </> bn </> fn
            bn'    = bn </> fn
            bn'T   = T.pack bn'
        isFile <- liftIO $ doesFileExist fullFn
        isDirectory <- liftIO $ doesDirectoryExist fullFn
        if| isFile -> do
              lift . logDebugN $ "Processing file " <> T.pack fullFn
              boid <- lift . createBlob $ BlobStream (C.sourceFile fullFn)
              lift . logDebugN $ "Writing to tree at " <> bn'T
              putBlob (T.encodeUtf8 bn'T) boid
          | isDirectory -> do
              lift . logDebugN $ "Descending down directory " <> T.pack fullFn
              go bn'
          | otherwise -> do
              liftIO . putStrLn $ "Bad file: " ++ fullFn





