{-# LANGUAGE RecordWildCards     #-}

import           Control.Monad.Logger
import           JUtils.GHPages
import           Options.Applicative
import qualified Data.Text            as T

data Opts = O { oLogLevel :: LogLevel
              , oToBase   :: Maybe FilePath
              , oFromBase :: FilePath
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
                       ( short 't'
                      <> long "target"
                      <> metavar "TARGET"
                      <> help "Directory root in gh-pages to place files in"
                      <> value ""
                      <> showDefaultWith (const ".")
                       )
                    )
              <*> argument str
                    ( metavar "DIR"
                   <> help "Folder to copy to gh-pages"
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
                              <> progDesc "Updates gh-pages branch with given folder"
                              <> header "jle-update-gh-pages - copy folder to gh-pages branch"
                               )

    updatePages' oLogLevel oFromBase oToBase (T.pack <$> oCNAME)
