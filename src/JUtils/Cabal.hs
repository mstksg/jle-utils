
module JUtils.Cabal (
    loadPackageDescription
  ) where

import           Control.Exception
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
import           Distribution.Verbosity
import           System.Directory
import           System.FilePath


loadPackageDescription :: Verbosity -> IO GenericPackageDescription
loadPackageDescription vb = do
    fs <- fmap (filter isCabal) . listDirectory =<< getCurrentDirectory
    case fs of
      []   -> throwIO $ userError "No .cabal file found in directory"
      [c]  -> readGenericPackageDescription vb c
      _:_  -> throwIO . userError $ "Multiple .cabal files found: " ++ show fs
  where
    isCabal = (== ".cabal") . takeExtension

