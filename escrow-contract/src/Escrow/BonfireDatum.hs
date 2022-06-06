{-# LANGUAGE OverloadedStrings #-}

module Escrow.BonfireDatum
  ( writeJSON,
    writeUnit,
    writeExampleDatum,
    writeExampleDisputeDatum,
  )
where

import Cardano.Api
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Escrow.Types
import PlutusTx (Data (..))
import qualified PlutusTx

exampleEvent :: BonfireEventEscrowDatum
exampleEvent =
  BonfireEventEscrowDatum
    { organizerReference = "localOrganizerTest",
      eventReference = "alterEgoMeeting",
      organizerPkh = "1a94b77705e9a1420655e6d952f133f85ccb514e7b68150e84c2ab7b",
      attendeePkh = "8ad46253eecbf732f01713bf78a5f7da8a373436c8dd42af01592062",
      eventCostLovelace = 25000000,
      eventCostPaymentToken = 1500000,
      eventStartTime = 1654719848000
    }

exampleDispute :: BonfireDisputeDatum
exampleDispute = BonfireDisputeDatum
  { bddOrganizerPkh = "1a94b77705e9a1420655e6d952f133f85ccb514e7b68150e84c2ab7b",
    bddAttendeePkh = "8ad46253eecbf732f01713bf78a5f7da8a373436c8dd42af01592062",
    bddEventCostLovelace = 25000000,
    bddEventCostPaymentToken = 1500000,
    bddEventID = "localOrganizerTest",
    bddDisputeID = "firstFIGHT"
  }

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs) = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B bs) = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: IO ()
writeUnit = writeJSON "output/bonfire-testnet/unit.json" ()

writeExampleDatum :: IO ()
writeExampleDatum = writeJSON "output/bonfire-testnet/example-datum.json" exampleEvent

writeExampleDisputeDatum :: IO ()
writeExampleDisputeDatum = writeJSON "output/bonfire-testnet/example-dispute-datum.json" exampleDispute

-- data BonfireEventEscrowDatum = BonfireEventEscrowDatum
--   {
--     organizerReference    :: !BuiltinByteString -- reference to our Bonfire DB (still thinking about how exactly to use this)
--   , eventReference        :: !BuiltinByteString -- reference to our Bonfire DB (still thinking about how exactly to use this)
--   , organizerPkh          :: !PubKeyHash
--   , attendeePkh           :: !PubKeyHash
--   , eventCostLovelace     :: !Integer
--   , eventCostPaymentToken :: !Integer -- In V0, "gimbal"
--   , eventStartTime        :: !POSIXTime
--   }
