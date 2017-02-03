{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Fixed
import           Data.Foldable
import           Data.Monoid
import           Data.Time.Format
import           Data.Time.LocalTime
import           GHC.Generics             (Generic)
import           Hledger.Data.Amount
import           Hledger.Data.Journal
import           Hledger.Data.MarketPrice
import           Hledger.Data.Types
import           Hledger.Read
import           Network.Wreq
import           Text.Printf
import           Text.Read
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

data Spot = Spot { _spotAmount   :: Centi
                 , _spotCurrency :: T.Text
                 }
    deriving (Generic, Show, Read)

instance FromJSON Spot where
    parseJSON j = manualParse j
              <|> genericParseJSON defaultOptions
                    { fieldLabelModifier = camelTo2 '-' . drop 4 }
                    j
      where
        manualParse (Object v) =
          Spot <$> (maybe empty return . readMaybe =<< v .: "amount")
               <*> v .: "currency"
        manualParse invalid = typeMismatch "Spot" invalid

instance ToJSON Spot where
    toEncoding = genericToEncoding defaultOptions
                   { fieldLabelModifier = camelTo2 '-' . drop 4 }

pricesPath :: FilePath
pricesPath = "/home/justin/.hledger/prices.journal"

main :: IO ()
main = do
    t <- zonedTimeToLocalTime <$> getZonedTime
    r <- getWith defaults "https://api.coinbase.com/v2/prices/BTC-USD/spot"
    let Just (Spot{..}) = r ^? responseBody . key "data" . _JSON
        mp = MarketPrice (localDay t) "BTC" (usd (realToFrac _spotAmount))

    Right j <- readJournalFile Nothing Nothing True pricesPath
    let mpmap = fmap (sortNubWith mpdate . ($ []))
              . M.fromListWith (.)
              . map (\p -> (mpcommodity p, (++ [p])))
              . jmarketprices
              . journalApplyCommodityStyles
              . over _jmarketprices (++ [mp])
              $ j

        heading = printf "; (last updated %s)\n"
                    (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" t)
        outstr = T.intercalate "\n"
               . ("; Historical commodity market prices" :)
               . (T.pack heading :)
               . flip M.foldMapWithKey mpmap $ \k mps ->
                   [ T.unlines $ ("; " <> k)
                               : map (T.pack . showMarketPrice) mps
                   ]

    T.writeFile pricesPath outstr
    putStrLn "Updated with new market price:"
    putStrLn $ showMarketPrice mp

_jmarketprices :: Lens' Journal [MarketPrice]
_jmarketprices f j = (\ps -> j { jmarketprices = ps })
                        <$> f (jmarketprices j)

sortNubWith :: Ord b => (a -> b) -> [a] -> [a]
sortNubWith f = toList . M.fromList . map (\x -> (f x, x))
