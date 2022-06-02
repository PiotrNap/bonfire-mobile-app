{-# LANGUAGE OverloadedStrings #-}

module Escrow.BonfireDatum
  ( writeJSON,
    writeUnit,
    writeExampleDatum,
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
    { organizerReference = "mixaximOrganizer",
      eventReference = "jamesMeetMIxAxIM002",
      organizerPkh = "e2755525479cfef354384534039a1d504d31504a53d856fc98237044",
      attendeePkh = "22117fbd0f86a213ae4f4d824cd0d38eea29e49764ae22f5f50ba3d3",
      eventCostLovelace = 32000000,
      eventCostPaymentToken = 2000000,
      eventStartTime = 1654344492000
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
writeExampleDatum = writeJSON "output/bonfire-testnet/james-mix-test-002.json" exampleEvent

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
