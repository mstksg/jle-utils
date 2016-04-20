{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JUtils.GHPages where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Monoid
import           Data.Tagged
import           Data.Time.Clock
import           Data.Time.Format
import           Foreign.ForeignPtr
import           Git.Libgit2
import           Git.Reference
import           Git.Repository
import           Git.Tree
import           Git.Tree.Builder
import           Git.Types
import           System.Directory
import           System.FilePath
import           Text.Printf
import qualified Conduit                     as C
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T

ghPagesRef :: T.Text
ghPagesRef = "refs/heads/gh-pages"

updatePages
    :: (MonadMask m, MonadLogger m, MonadIO m, MonadBaseControl IO m)
    => FilePath
    -> FilePath
    -> m ()
updatePages fromDir toDir = do
    now <- liftIO getCurrentTime

    let tStr    = formatTime defaultTimeLocale "%c" now
        commitMsg = T.pack $ printf "Update gh-pages at %s\n" tStr

    C.runResourceT . withRepository lgFactory "./" $ do
      r <- getPagesRef
      c <- lookupCommit $ Tagged r
      toid <- createTree (buildPagesTree fromDir toDir)

      tEnts <- listTreeEntries =<< lookupTree toid
      logDebugN . T.pack $ printf "Built tree with %d files."
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

          logInfoN ("Updating gh-pages branch")
          updateReference ghPagesRef (commitRefTarget c')
          let fOid = getOid . untag $ commitOid c'
          liftBaseOp (withForeignPtr fOid) $ \oid -> do
            hash <- take 7 . show <$> liftIO (oidToSha oid)
            logWarnN ("Succesfully updated branch to " <> T.pack hash)
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
    :: forall r m. (MonadGit r m, MonadLogger m, MonadThrow m)
    => m (Oid r)
getPagesRef = do
    rPages <- resolveReference ghPagesRef
    case rPages of
      Nothing -> do
        logWarnN "gh-pages branch not found"
        rTarget <- runMaybeT . asum
                 $ map (MaybeT . resolveReference) ["HEAD", masterRef]
        case rTarget of
          Nothing -> throwM
                   $ ErrorCall "No reference \"HEAD\" or \"master\" to create gh-pages branch from."
          Just r -> do
            logWarnN $ "Creating branch gh-pages"
            createReference ghPagesRef (RefObj r)

            rPages' <- resolveReference ghPagesRef
            case rPages' of
              Just r' -> return r'
              Nothing -> throwM
                       $ ErrorCall "Error creating gh-pages branch."
      Just r -> return r
  where
    masterRef  = "refs/heads/master"


buildPagesTree
    :: forall m r. (C.MonadResource m, MonadIO m, MonadLogger m, MonadGit r m)
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
          | otherwise -> do
              liftIO . putStrLn $ "Bad file: " ++ fullFn
