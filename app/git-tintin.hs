{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           JUtils.GHPages
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.Process

-- TODO: Make linking work for haddocks, to basically link to correct stack
-- snapshot documentation i guess?

data Opts = O { oLogLevel :: LogLevel
              , oDirBase  :: Maybe FilePath
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
      currDir <- liftIO getCurrentDirectory
      logDebugN "Rebuilding tintin"
      liftIO $ callCommand "tintin --verbose"

      let docRoot = currDir </> ".stack-work/tintin/rendered"

      updatePagesLogging docRoot oDirBase
