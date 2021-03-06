{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JUtils.GHPages
  ( updatePages
  , updatePages'
  , updatePagesLogging
  )
  where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Maybe
import           Data.Tagged
import           Data.Time.Format
import           Data.Time.LocalTime
import           Git.Libgit2
import           Git.Reference
import           Git.Repository
import           Git.Tree
import           Git.Tree.Builder
import           Git.Types
import           System.Directory
import           System.FilePath
import           Text.Printf
import qualified Conduit                        as C
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T

ghPagesRef :: T.Text
ghPagesRef = "refs/heads/gh-pages"

-- | Runs 'updatepages'' at the default log level, 'LevelInfo'.
updatePages
    :: FilePath         -- ^ The directory to copy over
    -> Maybe FilePath   -- ^ Optional: root directory of gh-pages copy
    -> Maybe T.Text     -- ^ CNAME contents
    -> IO ()
updatePages = updatePages' LevelInfo

-- | Runs 'updatePagesLogging' with the given log level.
--
-- - silent: 'LevelError'
-- - normal: 'LevelInfo'
-- - verbose: 'LevelDebug'
updatePages'
    :: LogLevel
    -> FilePath         -- ^ The directory to copy over
    -> Maybe FilePath   -- ^ Optional: root directory of gh-pages copy
    -> Maybe T.Text     -- ^ CNAME contents
    -> IO ()
updatePages' level fromDir toDir cname =
    runStderrLoggingT . getOnMy level $ updatePagesLogging fromDir toDir cname
  where
    getOnMy lev = filterLogger (\_ l -> l >= lev)

-- | Replaces the gh-pages branch with the contents of the given directory.
updatePagesLogging
    :: (MonadMask m, MonadLogger m, C.MonadUnliftIO m)
    => FilePath         -- ^ The directory to copy over
    -> Maybe FilePath   -- ^ Optional: root directory of gh-pages copy
    -> Maybe T.Text     -- ^ CNAME contents
    -> m ()
updatePagesLogging fromDir toDir cname = do
    now <- liftIO getZonedTime

    let tStr    = formatTime defaultTimeLocale "%c" now
        commitMsg = T.pack $ printf "Update gh-pages at %s\n" tStr

    C.runResourceT . withRepository lgFactory "./" $ do
      r <- getPagesRef
      c <- lookupCommit $ Tagged r
      toid <- createTree $ do
          buildPagesTree fromDir (fromMaybe "" toDir)
          mapM_ addCNAME cname

      tEnts <- listTreeEntries =<< lookupTree toid
      logDebugN . T.pack $ printf "Built tree with %d files."
                                  (length tEnts)

      if toid /= commitTree c
        then do
          logDebugN "Creating commit"
          c' <- createCommit [commitOid c]
                             toid
                             (commitAuthor c)    { signatureWhen = now }
                             (commitCommitter c) { signatureWhen = now }
                             commitMsg
                             (Just ghPagesRef)

          logInfoN "Updating gh-pages branch"
          updateReference ghPagesRef (commitRefTarget c')
          let hash = T.take 7 . renderObjOid $ commitOid c'
          logInfoN ("Succesfully updated branch to " <> hash)
        else
          logWarnN "No changes detected.  No commit written."

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
    :: forall m. (MonadLogger m, MonadMask m, C.MonadUnliftIO m, HasLgRepo m)
    => m OidPtr
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


buildPagesTree
    :: forall m r. (C.MonadResource m, MonadLogger m, MonadGit r m)
    => FilePath
    -> FilePath
    -> TreeT r m ()
buildPagesTree rt br = go ""
  where
    go :: FilePath -> TreeT r m ()
    go bn = do
      fns <- liftIO $ getDirectoryContents (rt </> bn)
      forM_ (filter (`notElem` [".",".."]) fns) $ \fn -> do
        let fullFn = T.unpack . T.pack $ rt </> bn </> fn
            bn'    = bn </> fn
            targ   = T.pack (br </> bn')
        isFile <- liftIO $ doesFileExist fullFn
        isDirectory <- liftIO $ doesDirectoryExist fullFn
        if| isFile -> do
              lift . logDebugN $ "Processing file " <> T.pack fullFn
              boid <- lift . createBlob $ BlobStream (C.sourceFile fullFn)
              lift . logDebugN $ "Writing to tree at " <> targ
              putBlob (T.encodeUtf8 targ) boid
          | isDirectory -> do
              lift . logDebugN $ "Descending down directory " <> T.pack fullFn
              go bn'
          | otherwise ->
              liftIO . putStrLn $ "Bad file: " ++ fullFn

addCNAME
    :: (C.MonadResource m, MonadGit r m)
    => T.Text
    -> TreeT r m ()
addCNAME cname = do
    boid <- lift . createBlob $ BlobString (T.encodeUtf8 cname)
    putBlob "CNAME" boid
