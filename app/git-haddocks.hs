{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson.TH
import           Data.Char
import           GHC.Generics
import           JUtils.GHPages
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.Process
import qualified Data.HashMap.Strict as H
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Yaml           as Y

-- TODO: Make linking work for haddocks, to basically link to correct stack
-- snapshot documentation i guess?

data LocalInfo = LI { _liPath    :: String
                    , _liVersion :: String
                    }
  deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3 . map toLower } ''LocalInfo)

data Opts = O { oLogLevel :: LogLevel
              , oDirBase  :: Maybe FilePath
              , oCNAME    :: Maybe String
              }
  deriving (Show, Eq)

parseOpts :: Parser Opts
parseOpts = O <$> ( flag' LevelDebug
                      ( short 'v'
                     <> long "verbose"
                     <> help "Display extra debug messages.  Will take priority over the \"silent\" flag"
                      )
                <|> flag LevelInfo LevelError
                      ( short 's'
                     <> long "silent"
                     <> help "Silence output except for output from haddock"
                      )
                  )
              <*> optional
                    (strOption
                       ( short 'd'
                      <> long "directory"
                      <> metavar "DIR"
                      <> help "Directory root in gh-pages to place haddock files in"
                       )
                    )
              <*> optional
                    (strOption
                       ( short 'c'
                      <> long "cname"
                      <> metavar "DOMAIN"
                      <> help "Contents of CNAME file, indicating custom github domain"
                       )
                    )

main :: IO ()
main = do
    O{..} <- execParser $ info (helper <*> parseOpts)
                               ( fullDesc
                              <> progDesc "Update gh-pages branch with haddock renders."
                              <> header "jle-git-haddocks - sync haddocks to gh-pages"
                               )

    runStderrLoggingT . filterLogger (\_ l -> l >= oLogLevel) $ do
      -- assumes project is root.
      -- TODO: fix this
      projNameVer <- liftIO $ do
        projName <- takeBaseName <$> getCurrentDirectory

        localsMap <- Y.decodeThrow @_ @(H.HashMap String LocalInfo)
                   . T.encodeUtf8
                   . T.pack
                 =<< readCreateProcess (shell "stack query locals") ""

        let localsData = _liVersion <$> H.lookup projName localsMap

        case localsData of
          Nothing -> throwM $ ErrorCall "Could not resolve project version."
          Just v  -> return $ projName ++ "-" ++ v

      logDebugN ("Resolved project name and version: " <> T.pack projNameVer)

      logDebugN "Rebuilding haddocks"
      dr <- liftIO $ do
          callCommand "stack haddock"
          readCreateProcess (shell "stack path --local-doc-root") ""

      let docRoot = zipWith const dr (drop 1 dr) </> projNameVer

      updatePagesLogging docRoot oDirBase (T.pack <$> oCNAME)
