{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Escrow.EscrowCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Escrow.BonfireEscrowContractDraft
import Escrow.Types
import Escrow.BonfireDispute
import qualified Ledger
import Plutus.V1.Ledger.Api (Data (B, Constr, I, List, Map), ToData, toData)

-- import Escrow.TTEscrowDraft -- Tutor Terrace
-- import Escrow.BountyEscrowDraft
-- import Escrow.BountyTreasury

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B b) = ScriptDataBytes b
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData k, dataToScriptData v) | (k, v) <- xs]
dataToScriptData (List xs) = ScriptDataList $ fmap dataToScriptData xs

writeJson :: ToData a => FilePath -> a -> IO ()
writeJson file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- bonfire-testnet tokens:
-- 61c8081a9aed827437c927074efb9ac07310d050256e587fc8b8cd83 = tpblTestAuth
-- cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059 = play

writeBonfireEscrowScript :: IO (Either (FileError ()) ())
writeBonfireEscrowScript =
  writeValidator "output/bonfire-testnet/bonfire-escrow-v2.plutus" $
    Escrow.BonfireEscrowContractDraft.validator $
      BonfireParam
        { organizerAccessSymbol = "0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce",
          ptSymbol          = "982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e",
          ptName            = "bonGimbal",
          treasuryPkh       = "f83d6f9d63a4b9541ad4efca5b48280bffdb8ac4e424c94432788109",
          disputeContract   = "12bdeb18d7ba86a22f1c21bb7921080a45b89f9a1e99b28e4397f94e"
        }

writeBonfireDispute :: IO (Either (FileError ()) ())
writeBonfireDispute =
  writeValidator "output/bonfire-testnet/bonfire-dispute-v2.plutus" $
    Escrow.BonfireDispute.validator $
      DisputeParam
        { bonfireAdminToken = "0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce",
          dpPtSymbol = "982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e",
          dpPtName = "bonGimbal",
          bonfireTreasuryPkh = "f83d6f9d63a4b9541ad4efca5b48280bffdb8ac4e424c94432788109"
        }
