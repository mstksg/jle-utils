{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

import           Control.Applicative
import           Control.Exception
import           Control.Lens hiding             ((.=))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Foldable
import           Data.Function
import           Data.Maybe hiding               (mapMaybe)
import           Data.Text                       (Text)
import           Data.Void
import           Data.Witherable
import           Distribution.Compiler
import           Distribution.PackageDescription
import           Distribution.Types.Version
import           Distribution.Types.VersionRange
import           Distribution.Verbosity
import           JUtils.Cabal
import           JUtils.Github
import           JUtils.Stackage
import           Network.Curl
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import qualified Data.Yaml                       as Y
import qualified Text.Megaparsec                 as P
import qualified Text.Megaparsec.Char.Lexer      as P

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
    IntersectVersionRangesF x y -> \g -> x g && y g
    VersionRangeParensF x       -> x
  where
    versionFull :: Version -> Maybe GHCFull
    versionFull v = case versionNumbers v of
      [x]     -> Just $ GHCFull (GHCMajor x 0) 0
      [x,y]   -> Just $ GHCFull (GHCMajor x y) 0
      [x,y,z] -> Just $ GHCFull (GHCMajor x y) z
      _       -> Nothing

templateUrl :: URLString
templateUrl = "https://raw.githubusercontent.com/commercialhaskell/stack/stable/doc/travis-complex.yml"

floop :: forall t a f. (AsJSON t, ToJSON a, FromJSON a, Applicative f)
      => (a -> f (Maybe a))
      -> t -> f (Maybe t)
floop f = fmap join
        . sequenceA
        . fmap ((fmap . fmap) (review (_JSON @t @a)) . f)
        . preview (_JSON @t @a)

modifyInclude :: VersionRange -> Include -> IO (Maybe Include)
modifyInclude rng i = runMaybeT $ do
    cmp <- lift (ctCompiler (includeCompiler i))
    guard $ majorInRange rng cmp
    pure i

ctCompiler
    :: CompilerType
    -> IO GHCFull
ctCompiler = \case
    CTGHC Nothing       -> currentHead
    CTGHC (Just v)      -> pure v
    CTStack (Left v)  _ -> case v of
      SSNightly -> do
        (y,m,d) <- either throwIO pure
               =<< runGithub latestNightly
        resolverGHC (Nightly y m d)
      SSDefault -> do
        r <- currentResolver
        resolverGHC r
    CTStack (Right v) _ -> pure v

main :: IO ()
main = do
    rng   <- versionRange
    tyaml <- withCurlDo $ do
      (_,tfull) <- curlGetString templateUrl []
      Y.decodeThrow @_ @Object . T.encodeUtf8 . T.pack $ tfull
    modified <- tyaml
              & witherOf ( ix  "matrix"
                         . key "include"
                         . _Array
                         . wither
                         . floop
                         )
                (modifyInclude rng)
    T.putStrLn . T.decodeUtf8 $ Y.encode modified


currentResolver :: IO Resolver
currentResolver = do
    y <- Y.decodeFileThrow @_ @Object "stack.yaml"
    maybe (throwIO (userError "no parse")) pure $
       y ^? ix "resolver"
          . _String
          . folding (P.parse resolverParser "stack.yaml$resolver")
  where
    resolverParser :: P.Parsec Void Text Resolver
    resolverParser = P.choice
        [ LTS <$> (P.chunk "lts-" *> P.decimal)
              <*> (P.single '.'   *> P.decimal)
        , Nightly <$> (P.chunk "nightly-" *> P.decimal)
                  <*> (P.single '-'       *> P.decimal)
                  <*> (P.single '-'       *> P.decimal)
        ]

currentHead :: IO GHCFull
currentHead = do
    (_, tfull) <- withCurlDo $ curlGetString acUrl []
    either throwIO pure
       . P.parse (P.skipManyTill P.anySingle (P.try fullParser)) "configure.ac"
       $ tfull
  where
    acUrl = "https://raw.githubusercontent.com/ghc/ghc/master/configure.ac"


data StackSpecial = SSNightly
                  | SSDefault
  deriving Show

data CompilerType = CTGHC (Maybe GHCFull)
                  | CTStack (Either StackSpecial GHCFull) Bool
  deriving Show

data Include = Include
    { includeEnv      :: Text
    , includeCompiler :: CompilerType
    , includePackages :: Maybe [Text]
    , includeSources  :: Maybe [Text]
    , includeOS       :: Maybe Text
    }
  deriving Show

instance FromJSON CompilerType where
    parseJSON = withText "CompilerType" $ \t -> case T.words (T.drop 3 t) of
      ["GHC", x] -> CTGHC <$> case x of
        "HEAD" -> pure Nothing
        v      -> fmap Just . either (fail . show) pure $ P.parse fullParser "GHC" v
      "stack":x:xs -> do
        let isOsx = xs == ["osx"]
        ver <- case x of
          "nightly" -> pure $ Left SSNightly
          "default" -> pure $ Left SSDefault
          v         -> fmap Right . either (fail . show) pure $ P.parse fullParser "stack" v
        pure $ CTStack ver isOsx
      _ -> empty

instance ToJSON CompilerType where
    toJSON = \case
        CTGHC Nothing  -> String ": #GHC HEAD"
        CTGHC (Just v) -> String $ ": #GHC " <> T.pack (prettyFull v)
        CTStack sv b   -> String $ ": #stack " <> showSV sv <> showO b
      where
        showSV = \case
          Left SSNightly -> "nightly"
          Left SSDefault -> "default"
          Right v        -> T.pack $ prettyFull v
        showO = \case
          False -> ""
          True  -> " osx"

instance FromJSON Include where
    parseJSON = withObject "Include" $ \o -> do
        includeEnv      <- o .: "env"
        includeCompiler <- o .: "compiler"
        includePackages <- pure $
                o ^? ix  "addons"
                   . key "apt"
                   . key "packages"
                   . _JSON
        includeSources <- pure $
                o ^? ix  "addons"
                   . key "apt"
                   . key "sources"
                   . _JSON
        includeOS <- pure $
                o ^? ix  "osx"
                   . _JSON
        pure $ Include{..}

instance ToJSON Include where
    toJSON Include{..} = object . getConst $ do
        Const ["env"      .= includeEnv]
        Const ["compiler" .= includeCompiler]
        unless (null addons) $
          Const ["addons" .= object
                    [ "apt" .= object addons ]
                ]
        for_ includeOS $ \o ->
          Const [ "os" .= o ]
        pure ()
      where
        addons = getConst $ do
          for_ includePackages $ \p ->
            Const [ "packages" .= p ]
          for_ includeSources $ \s ->
            Const [ "sources" .= s ]
          pure ()
