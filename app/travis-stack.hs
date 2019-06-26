{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Exception
import           Data.Function
import           Data.Maybe
import           Distribution.Compiler
import           Distribution.PackageDescription
import           Distribution.Types.Version
import           Distribution.Types.VersionRange
import           Distribution.Verbosity
import           JUtils.Cabal
import           JUtils.Stackage

versionRange :: IO VersionRange
versionRange = maybe (throwIO (userError e)) pure
             . lookup GHC
             . testedWith
             . packageDescription
           =<< loadPackageDescription normal
  where
    e = "No version range for GHC found in 'tested-with' field."

majorsInRange :: VersionRange -> GHCFull -> Bool
majorsInRange = cataVersionRange $ \case
    AnyVersionF                 -> \_ -> True
    ThisVersionF v              -> (versionFull v ==) . Just
    LaterVersionF v             -> \g -> maybe False (<  g) $ versionFull v
    OrLaterVersionF v           -> \g -> maybe False (<= g) $ versionFull v
    EarlierVersionF v           -> \g -> maybe False (>  g) $ versionFull v
    OrEarlierVersionF v         -> \g -> maybe False (>= g) $ versionFull v
    WildcardVersionF v          -> \g -> maybe False (((==) `on` gmA . gfMajor) g)
                                 $ versionFull v
    MajorBoundVersionF v        -> \g -> fromMaybe False $ do
      vv  <- versionFull v
      vv' <- versionFull (majorUpperBound v)
      pure $ g >= vv && g < vv'
    UnionVersionRangesF x y     -> \g -> x g || y g
    IntersectVersionRangesF x y -> \g -> x g || y g
    VersionRangeParensF x       -> x
  where
    versionFull :: Version -> Maybe GHCFull
    versionFull v = case versionNumbers v of
      [x,y]   -> Just $ GHCFull (GHCMajor x y) 0
      [x,y,z] -> Just $ GHCFull (GHCMajor x y) z
      _       -> Nothing

main :: IO ()
main = do
    rng <- versionRange
    print rng

