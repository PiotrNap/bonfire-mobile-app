{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import            Data.Aeson                as Json   (encode)
import            Data.ByteString.Lazy      qualified as LB
import            System.Environment        (getArgs)
import            Prelude                   hiding (id)
import            Data.String               (fromString)

import            Cardano.Api               (scriptDataToJson, ScriptDataJsonSchema(ScriptDataJsonDetailedSchema))
import            Cardano.Api.Shelley       (fromPlutusData)
import            Ledger                    hiding (singleton)


import            Escrow.BountyEscrowDraft  (BountyEscrowDatum(..))

-- Thanks to Adrian for writing:
-- https://gitlab.com/gimbalabs/ppbl/unsigs-market-plutus/-/blob/main/app/datum-json.hs

-- This module is here to convert Haskell Data Types to JSON files, particularly used for BountyEscrowDatum custom Datum Type.
-- To use this enter `cabal run datum-json <price> <seller> <id>`. (price in lovelace, seller as PubKeyHash)
-- You can use ( /testnet ) ./getPubKeyHash <wallet name> to get your wallet PubKeyHash

-- Constructs the JSON file for the datum, used as input to --tx-in-datum-file in cardano-cli

-- data BountyEscrowDatum = BountyEscrowDatum
--   {
--     issuerAddress       :: !PubKeyHash -- reference to our Bonfire DB
--   , contributorAddress  :: !PubKeyHash -- reference to our Bonfire DB
--   , lovelaceAmount      :: !Integer
--   , tokenAmount         :: !Integer
--   , expirationTime      :: !POSIXTime
--   }


main :: IO ()
main = do
  [issuerAddress', contributorAddress', lovelaceAmount', tokenAmount', expirationTime'] <- getArgs
  let lovelaceAmount        = read lovelaceAmount'
      tokenAmount           = read tokenAmount'
      issuerAddress         = fromString issuerAddress'
      contributorAddress    = fromString contributorAddress'
      expirationTime        = POSIXTime $ read expirationTime'
      bountyInstance        = BountyEscrowDatum issuerAddress contributorAddress lovelaceAmount tokenAmount expirationTime
  writeData ("output/datum.json") bountyInstance
  putStrLn "Done"

-- Datum also needs to be passed when sending the token to the script (aka putting for sale)
-- When doing this, the datum needs to be hashed, use cardano-cli transaction hash-script-data ...

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
