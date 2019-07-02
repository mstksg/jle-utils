{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveGeneric       #-}
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
import           Data.Map                        (Map)
import           Data.Maybe hiding               (mapMaybe)
import           Data.Set                        (Set)
import           Data.Text                       (Text)
import           Data.Void
import           Data.Witherable
import           Distribution.Compiler
import           Distribution.PackageDescription
import           Distribution.Types.Version
import           Distribution.Types.VersionRange
import           Distribution.Verbosity
import           GHC.Generics                    (Generic)
import           JUtils.Cabal
import           JUtils.Github
import           JUtils.Stackage
import           Network.Curl
import           System.Directory
import           System.FilePath
import           System.Process
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Set.NonEmpty               as NES
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import qualified Data.Vector                     as V
import qualified Data.Yaml                       as Y
import qualified Text.Megaparsec                 as P
import qualified Text.Megaparsec.Char.Lexer      as P

versionRange :: IO VersionRange
versionRange = maybe recoop pure
             . lookup GHC
             . testedWith
             . packageDescription
           =<< loadPackageDescription normal
  where
    recoop = do
      putStrLn "Warning: No version range for GHC found in tested-with field."
      putStrLn "Using AnyVersion, but please specify an explicit range for the benefit of users."
      pure anyVersion

fullInRange :: VersionRange -> GHCFull -> Bool
fullInRange vr (GHCFull (GHCMajor x y) z) = withinRange v vr
  where
    v = mkVersion [x,y,z]

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
    guard $ fullInRange rng cmp
    pure i

modifyDeps :: Set Text -> Include -> Include
modifyDeps ds i
    | Just "osx" <- includeOS i = i
    | otherwise                 = i & pkg . _Just <>~ S.toList ds
  where
    pkg f j = (\p -> j { includePackages = p }) <$> f (includePackages j)

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
    cfg <- Y.decodeFileThrow @_ @Config . (</> "travis-stack.yaml") =<<
        getXdgDirectory XdgConfig "jle-utils"
    deps <- stackDeps
    let (extraAD, extraBD) = foldMap (findDeps cfg) deps
        installBD = case extraBD of
          NES.IsNonEmpty s -> V.singleton . String . T.unlines . mconcat $
            [ ["if [ `uname` = \"Darwin\" ]", "then", "  brew update"]
            , [ "  brew install " <> d
              | d <- toList s
              ]
            , ["fi"]
            ]
          NES.IsEmpty      -> mempty
    modified <- tyaml
              & ix "before_install" . _Array <>~ installBD
              & witherOf ( ix  "matrix"
                         . key "include"
                         . _Array
                         . wither
                         . floop
                         )
                (modifyInclude rng . modifyDeps extraAD)
    T.putStrLn . T.decodeUtf8 $ Y.encode modified
  where
    findDeps Config{..} a = ( fold (M.lookup a cfgExtraAptDeps )
                            , fold (M.lookup a cfgExtraBrewDeps)
                            )

data Config = Config
    { cfgExtraAptDeps :: Map Text (Set Text)
    , cfgExtraBrewDeps :: Map Text (Set Text)
    }
  deriving Generic

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 3
        }

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

stackDeps :: IO (Set Text)
stackDeps = S.fromList
          . mapMaybe (fmap T.pack . listToMaybe . words)
          . lines
        <$> readCreateProcess (shell "stack --test --bench ls dependencies") ""


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
                o ^? ix  "os"
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
