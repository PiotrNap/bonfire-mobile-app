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
  writeValidator "output/bonfire-testnet/bonfire-escrow-000.plutus" $
    Escrow.BonfireEscrowContractDraft.validator $
      BonfireParam
        { organizerAccessSymbol = "",
          disputeContract = "",
          ptSymbol = "",
          ptName = ""
        }

writeBonfireDispute :: IO (Either (FileError ()) ())
writeBonfireDispute =
  writeValidator "output/bonfire-testnet/bonfire-dispute-000.plutus" $
    Escrow.BonfireDispute.validator $
      DisputeParam
        { bonfireAdminToken = "",
          dpPtSymbol = "cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059",
          dpPtName = "play",
          bonfireTreasuryPkh = "22117fbd0f86a213ae4f4d824cd0d38eea29e49764ae22f5f50ba3d3"
        }
