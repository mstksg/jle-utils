{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Exception
import           Data.Function
import           Data.Maybe
import           Distribution.Compiler
import           Distribution.PackageDescription
import           Distribution.Types.Version
import           Distribution.Types.VersionRange
import           Distribution.Verbosity
import           JUtils.Cabal
import           JUtils.Cache
import           JUtils.Github
import           JUtils.Stackage
import qualified Data.Map                        as M

versionRange :: IO VersionRange
versionRange = maybe (throwIO (userError e)) pure
             . lookup GHC
             . testedWith
             . packageDescription
           =<< loadPackageDescription normal
  where
    e = "No version range for GHC found in 'tested-with' field."

majorInRange :: VersionRange -> GHCFull -> Bool
majorInRange = cataVersionRange $ \case
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
    vi <- withCache @VersionInfo False "travis-stack-lts-bounds" $ do
      putStrLn "Populating LTS version cache..."
      e <- runGithub versionInfo
      case e of
        Left e' -> throwIO e'
        Right x -> pure x
    let mp = M.filterWithKey (goodVI rng) . viMap $ vi
    print mp
  where
    goodVI rng mj (mn, _) = majorInRange rng (GHCFull mj mn)
