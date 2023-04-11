{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Escrow.Demu where

import Ledger.Contexts
import Ledger.Typed.Scripts
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Api
import PlutusTx
import PlutusTx.Prelude qualified as Plutus ((.), (==))
import PlutusTx.Trace (traceIfFalse)

-- 1. Check if the address we're trying to spend coins to is the actual "artistPhk"
-- 2. Check if the output value is going to artist

{-# INLINEABLE mkValidator #-}
mkValidator :: DemuEscrowParam -> DemuEscrowDatum -> DemuEscrowAction -> ScriptContext -> Bool
mkValidator param eDatum eAction ctx =
  case eAction of
    Withdraw ->
      traceIfFalse "Output address and artistPkh do not match" checkArtistPkhOutAddr
    TopUp -> True
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    artistPubKeyHash :: PubKeyHash
    artistPubKeyHash = artistPkh eDatum

    artistAddr :: Address
    artistAddr = pubKeyHashAddress artistPubKeyHash

    checkArtistPkhOutAddr :: Bool
    checkArtistPkhOutAddr = all (\x -> txOutAddress x Plutus.== artistAddr) $ txInfoOutputs info

-- checkIfSpendingToArtist :: Bool
-- checkIfSpendingToArtist = valueToArtist == valueSpent info

-- valueToArtist :: Value
-- valueToArtist = valuePaidTo info artistPubKeyHash

-- txOutPubKeyHash :: Bool
-- txOutPubKeyHash = all ((==) utxoDatumPubKeyHash) txOutAddress $ ctx scriptContextTxInfo

data EscrowTypes

instance ValidatorTypes EscrowTypes where
  type DatumType EscrowTypes = DemuEscrowDatum
  type RedeemerType EscrowTypes = DemuEscrowAction

typedValidator :: DemuEscrowParam -> TypedValidator EscrowTypes
typedValidator bp =
  mkTypedValidator @EscrowTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode bp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @DemuEscrowDatum @DemuEscrowAction

validator :: DemuEscrowParam -> Validator
validator = validatorScript Plutus.. typedValidator
