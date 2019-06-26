{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module JUtils.Cache (
    withCache
  ) where

import           Control.Exception
import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified Data.Binary          as Bi
import qualified Data.ByteString.Lazy as BSL

withCache
    :: forall a. Bi.Binary a
    => Bool
    -> String
    -> IO a
    -> IO a
withCache fce nm act = do
    dir <- getXdgDirectory XdgCache "jle-utils"
    let fp = dir </> nm
    createDirectoryIfMissing True dir
    old <- if fce
      then pure (Left ())
      else tryJust (guard . isDoesNotExistError) $
                BSL.readFile fp
    case old of
      Right o -> case Bi.decodeOrFail @a o of
        Right (_, _, x) -> pure x
        Left  (_, _, e) -> throwIO . userError $ e
      Left  _ -> do
        x <- act
        x <$ Bi.encodeFile fp x

