{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Escrow.EscrowCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger

import Plutus.V1.Ledger.Api (Data (B, Constr, I, List, Map), ToData, toData)

import Escrow.BonfireEscrowContractDraft
-- import Escrow.BonfireDispute
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
writeBonfireEscrowScript = writeValidator "output/bonfire-testnet/bonfire-escrow-000.plutus" $ Escrow.BonfireEscrowContractDraft.validator $ BonfireParam
    {
      organizerAccessSymbol = "61c8081a9aed827437c927074efb9ac07310d050256e587fc8b8cd83"
    , disputeContract = "9cb435a958e72371981537b57b178936d6d41b1fbd8cd1194f6a8014"
    , ptSymbol    = "cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059"
    , ptName      = "play"
    }

-- writeBountyTreasuryScript :: IO (Either (FileError ()) ())
-- writeBountyTreasuryScript = writeValidator "output/testnet-002/bounty-treasury-002.plutus" $ Escrow.BountyTreasury.validator $ TreasuryParam
--     {
--       authTokenSymbol = "5d6b6c332866044b2a8bdd147d92f77e42714986f1cb98cef70e201f"
--     , bountyContract  = "9cb435a958e72371981537b57b178936d6d41b1fbd8cd1194f6a8014"
--     , bSymbol         = "cb4a5cb63378a521cb82bdfacc4a8fd543b22ae19c094b75e13f7853"
--     , bName           = "tGimbal"
--     }
