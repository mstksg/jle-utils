{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

-- import Git.CmdLine
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson.TH
import Data.Char
import Data.Foldable
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
import qualified Conduit             as C
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Yaml           as Y

-- TODO: Make linking work for haddocks, to basically link to correct stack
-- snapshot documentation i guess?

ghPagesRef :: T.Text
ghPagesRef = "refs/heads/gh-pages"

data LocalInfo = LI { _liPath    :: String
                    , _liVersion :: String
                    }
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = (drop 3 . map toLower) } ''LocalInfo)

main :: IO ()
main = runStderrLoggingT $ do
    -- assumes project is root.
    -- TODO: fix this
    projNameVer <- liftIO $ do
      projName <- takeBaseName <$> getCurrentDirectory

      localsData <- fmap _liVersion
                  . (H.lookup projName =<<)
                  . Y.decode
                  . T.encodeUtf8
                  . T.pack
                <$> readCreateProcess (shell "stack query locals") ""

      case localsData of
        Nothing -> throwM $ ErrorCall "Could not resolve project version."
        Just v  -> return $ projName ++ "-" ++ v

    logDebugN ("Resolved project name and version: " <> T.pack projNameVer)

    logDebugN ("Rebuilding haddocks")
    dr <- liftIO $ do
        callCommand "stack haddock"
        readCreateProcess (shell "stack path --local-doc-root") ""

    now <- liftIO getCurrentTime

    let docRoot = zipWith const dr (drop 1 dr) </> projNameVer
        tStr    = formatTime defaultTimeLocale "%c" now
        commitMsg = T.pack $ printf "Update gh-pages documentation for %s at %s\n"
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

      if toid /= commitTree c
        then do
          logDebugN ("Creating commit")
          c' <- createCommit [commitOid c]
                             toid
                             (commitAuthor c)
                             (commitCommitter c)
                             commitMsg
                             (Just ghPagesRef)

          logDebugN ("Updating gh-pages branch")
          updateReference ghPagesRef (commitRefTarget c')
        else
          logWarnN "No changes detected from old documentation.  No commit written."

    -- TODO: push automatically.  but i guess it's not that important
    -- withRepository cliFactory "./" . void $ do
    --   cliPushCommit (Tagged (SHA $ T.encodeUtf8 ghPagesRef))
    --                 "origin"
    --                 ghPagesRef
    --                 (Just "/home/justin/.ssh/id_rsa")
      -- r <- resolveReference ghPagesRef
      -- case r of
      --   Nothing -> logErrorN $ "gitlib-cmdline cannot find gh-pages branch.  Please push manually."
      --   Just r' -> void $ cliPushCommit (Tagged r') "origin" ghPagesRef Nothing


getPagesRef
    :: forall r m. (MonadGit r m, MonadLogger m, MonadThrow m)
    => m (Oid r)
getPagesRef = do
    rPages <- resolveReference ghPagesRef
    case rPages of
      Nothing -> do
        logWarnN "gh-pages branch not found"
        rTarget <- runMaybeT . asum
                 $ map (MaybeT . resolveReference) ["HEAD", "refs/heads/master"]
        case rTarget of
          Nothing -> throwM
                   $ ErrorCall "No reference \"HEAD\" or \"master\" to create gh-pages branch from."
          Just r -> do
            logWarnN "Creating branch gh-pages"
            createReference ghPagesRef (RefObj r)

            rPages' <- resolveReference ghPagesRef
            case rPages' of
              Just r' -> return r'
              Nothing -> throwM
                       $ ErrorCall "Error creating gh-pages branch."
      Just r -> return r


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
