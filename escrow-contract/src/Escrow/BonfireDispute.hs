{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Escrow.BonfireDispute where

import Escrow.Types
import Ledger hiding (singleton)
import Ledger.Ada
import Ledger.Typed.Scripts
import Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import PlutusTx.Prelude as Plutus
  ( Bool (..),
    Eq ((==)),
    Integer,
    Maybe (..),
    fromInteger,
    length,
    traceIfFalse,
    unsafeRatio,
    (&&),
    (*),
    (.),
    (<>),
    (>=),
  )
import PlutusTx.Ratio (numerator)

-- IF we create a separate "Dispute Contract", should its ValidatorHash be referenced here,
-- OR the other way around?

{-# INLINEABLE mkValidator #-}
mkValidator :: DisputeParam -> BonfireDisputeDatum -> DisputeResult -> ScriptContext -> Bool
mkValidator dp dd res ctx =
  case res of
    PayAttendee ->
      traceIfFalse "Must include Bonfire Admin Token" inputHasAdminToken
        && traceIfFalse "Must pay to Attendee" outputToAttendee
    PayOrganizer ->
      traceIfFalse "Must include Bonfire Admin Token" inputHasAdminToken
        && traceIfFalse "Must pay to Organizer" outputToOrganizer
    Split ->
      traceIfFalse "Must include Bonfire Admin Token" inputHasAdminToken
        && traceIfFalse "Must pay to Attendee and Organizer" outputToBoth
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    inputHasAdminToken :: Bool
    inputHasAdminToken = (bonfireAdminToken dp) `elem` inVals

    valueToAttendee :: Value
    valueToAttendee = valuePaidTo info $ bddAttendeePkh dd

    valueToOrganizer :: Value
    valueToOrganizer = valuePaidTo info $ bddOrganizerPkh dd

    valueToTreasury :: Value
    valueToTreasury = valuePaidTo info $ bonfireTreasuryPkh dp

    -- see https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Prelude.html
    -- unsafeRatio does not protect for zero denominator
    -- all denominators are specified here, so we are "safe"

    -- To Do: EventCost < 30 ada || EventCost > 30 ada

    outputToAttendee :: Bool
    outputToAttendee =
      fromInteger (valueOf valueToAttendee (dpPtSymbol dp) (dpPtName dp)) >= (9 `unsafeRatio` 10) * fromInteger (bddEventCostPaymentToken dd)
        && fromInteger (getLovelace $ fromValue valueToAttendee) >= (9 `unsafeRatio` 10) * fromInteger (bddEventCostLovelace dd)
        && fromInteger (valueOf valueToTreasury (dpPtSymbol dp) (dpPtName dp)) >= (1 `unsafeRatio` 10) * fromInteger (bddEventCostPaymentToken dd)
        && fromInteger (getLovelace $ fromValue valueToTreasury) >= (1 `unsafeRatio` 10) * fromInteger (bddEventCostLovelace dd)

    outputToOrganizer :: Bool
    outputToOrganizer =
      fromInteger (valueOf valueToOrganizer (dpPtSymbol dp) (dpPtName dp)) >= (9 `unsafeRatio` 10) * fromInteger (bddEventCostPaymentToken dd)
        && fromInteger (getLovelace $ fromValue valueToOrganizer) >= (9 `unsafeRatio` 10) * fromInteger (bddEventCostLovelace dd)
        && fromInteger (valueOf valueToTreasury (dpPtSymbol dp) (dpPtName dp)) >= (1 `unsafeRatio` 10) * fromInteger (bddEventCostPaymentToken dd)
        && fromInteger (getLovelace $ fromValue valueToTreasury) >= (1 `unsafeRatio` 10) * fromInteger (bddEventCostLovelace dd)

    outputToBoth :: Bool
    outputToBoth =
      fromInteger (valueOf valueToAttendee (dpPtSymbol dp) (dpPtName dp)) >= (45 `unsafeRatio` 100) * fromInteger (bddEventCostPaymentToken dd)
        && fromInteger (getLovelace $ fromValue valueToAttendee) >= (45 `unsafeRatio` 100) * fromInteger (bddEventCostLovelace dd)
        && fromInteger (valueOf valueToOrganizer (dpPtSymbol dp) (dpPtName dp)) >= (45 `unsafeRatio` 100) * fromInteger (bddEventCostPaymentToken dd)
        && fromInteger (getLovelace $ fromValue valueToOrganizer) >= (45 `unsafeRatio` 100) * fromInteger (bddEventCostLovelace dd)
        && fromInteger (valueOf valueToTreasury (dpPtSymbol dp) (dpPtName dp)) >= (1 `unsafeRatio` 10) * fromInteger (bddEventCostPaymentToken dd)
        && fromInteger (getLovelace $ fromValue valueToTreasury) >= (1 `unsafeRatio` 10) * fromInteger (bddEventCostLovelace dd)

data EscrowTypes

instance ValidatorTypes EscrowTypes where
  type DatumType EscrowTypes = BonfireDisputeDatum
  type RedeemerType EscrowTypes = DisputeResult

typedValidator :: DisputeParam -> TypedValidator EscrowTypes
typedValidator bp =
  mkTypedValidator @EscrowTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode bp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @BonfireDisputeDatum @DisputeResult

validator :: DisputeParam -> Validator
validator = validatorScript . typedValidator
