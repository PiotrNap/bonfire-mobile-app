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

module Escrow.BonfireEscrowContractDraft where

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

-- How do we guard against double-booking an event?
-- Does a token play a role in representing a single event?
-- Would that token be created at the time of event CREATION by ORGANIZER?
-- Or at the time of event BOOKING by ATTENDEE?

{-# INLINEABLE mkValidator #-}
mkValidator :: BonfireParam -> BonfireEventEscrowDatum -> EventAction -> ScriptContext -> Bool
mkValidator bp edatum action ctx =
  case action of
    AttCancel ->
      traceIfFalse "Attendee must sign Cancellation Tx" signedByAttendee
        && traceIfFalse "The Cancellation deadline has passed" beforeCancelDeadline
        && traceIfFalse "Output must be returned to Attendee" sufficientOutputToAttendee
    -- allow attendee to cancel up until (STARTTIME - 24 HOURS); but within 24 hours, they forfeit funds

    OrgCancel ->
      traceIfFalse "Organizer must sign Cancellation Tx" signedByOrganizer
        && traceIfFalse "Organizer must have Access Token" organizerHasAccessToken
        && traceIfFalse "Output must be returned to Attendee" sufficientOutputToAttendee
    Complete ->
      traceIfFalse "Organizer must sign Cancellation Tx" signedByOrganizer
        && traceIfFalse "Organizer must have Access Token" organizerHasAccessToken
        && traceIfFalse "It is too early to collect" afterDisputeDeadline
        && traceIfFalse "Output must be sent to Organizer" sufficientOutputToOrganizer
    Dispute ->
      traceIfFalse "Attendee must sign Cancellation Tx" signedByAttendee
        && traceIfFalse "Must pay to Dispute Contract" sufficientOutputToDisputeContract
        && traceIfFalse "It is too late for you to Dispute!" beforeDisputeDeadline
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Handle Signatures
    signedByAttendee :: Bool
    signedByAttendee = txSignedBy info $ attendeePkh edatum

    signedByOrganizer :: Bool
    signedByOrganizer = txSignedBy info $ organizerPkh edatum

    -- Handle Time
    disputeDeadline :: POSIXTime
    disputeDeadline = eventStartTime edatum + 3600000
    -- 3600000 milliseconds = 1 hour

    cancelDeadline :: POSIXTime
    cancelDeadline = eventStartTime edatum - 86400000
    -- 86400000 milliseconds = 1 day

    beforeCancelDeadline :: Bool
    beforeCancelDeadline = contains (to cancelDeadline) $ txInfoValidRange info

    afterDisputeDeadline :: Bool
    afterDisputeDeadline = contains (from disputeDeadline) $ txInfoValidRange info

    beforeDisputeDeadline :: Bool
    beforeDisputeDeadline = not afterDisputeDeadline

    -- Look for Access Token in Input(s)
    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- Check that list of CurrencySymbols includes Auth CurrencySymbol
    organizerHasAccessToken :: Bool
    organizerHasAccessToken = (organizerAccessSymbol bp) `elem` inVals

    -- Handle Outputs
    valueToAttendee :: Value
    valueToAttendee = valuePaidTo info $ attendeePkh edatum

    valueToOrganizer :: Value
    valueToOrganizer = valuePaidTo info $ organizerPkh edatum

    valueToTreasury :: Value
    valueToTreasury = valuePaidTo info $ treasuryPkh bp

    sufficientOutputToAttendee :: Bool
    sufficientOutputToAttendee =
      (getLovelace $ fromValue valueToAttendee) >= (eventCostLovelace edatum)
        && (valueOf valueToAttendee (ptSymbol bp) (ptName bp)) >= (eventCostPaymentToken edatum)

    -- Returns True if at least 2 ADA fee is paid, or if fee is at least 5% of event cost.
    -- This approach allows for options on front-end while maintining guarantee that Organizer gets at least (cost - 2) or (cost * 95%)
    -- The "threshold" for minimum event cost can therefore be "set" to anything.
    -- Think about front-end messaging and how to handle event cost of 35 ada, for example.
    lovelaceOutputsCorrect :: Bool
    lovelaceOutputsCorrect =
      (fromInteger (getLovelace $ fromValue valueToOrganizer) >= (95 `unsafeRatio` 100) * fromInteger (eventCostLovelace edatum)
        && fromInteger (getLovelace $ fromValue valueToTreasury) >= (5 `unsafeRatio` 100) * fromInteger (eventCostLovelace edatum)) ||
      (getLovelace (fromValue valueToOrganizer) >= (eventCostLovelace edatum - 2000000)
        && getLovelace (fromValue valueToTreasury) >= 2000000)

    -- gimbal costs can always stick to 95% / 5%
    gimbalOutputsCorrect :: Bool
    gimbalOutputsCorrect =
      fromInteger (valueOf valueToOrganizer (ptSymbol bp) (ptName bp)) >= (95 `unsafeRatio` 100) * fromInteger (eventCostPaymentToken edatum)
        && fromInteger (valueOf valueToTreasury (ptSymbol bp) (ptName bp)) >= (5 `unsafeRatio` 100) * fromInteger (eventCostPaymentToken edatum)

    sufficientOutputToOrganizer :: Bool
    sufficientOutputToOrganizer = gimbalOutputsCorrect && lovelaceOutputsCorrect

    -- Dispute
    valueToDisputeContract :: Value
    valueToDisputeContract = valueLockedBy info (disputeContract bp)

    sufficientOutputToDisputeContract :: Bool
    sufficientOutputToDisputeContract =
      (getLovelace $ fromValue valueToDisputeContract) >= (eventCostLovelace edatum)
        && (valueOf valueToDisputeContract (ptSymbol bp) (ptName bp)) >= (eventCostPaymentToken edatum)

data EscrowTypes

instance ValidatorTypes EscrowTypes where
  type DatumType EscrowTypes = BonfireEventEscrowDatum
  type RedeemerType EscrowTypes = EventAction

typedValidator :: BonfireParam -> TypedValidator EscrowTypes
typedValidator bp =
  mkTypedValidator @EscrowTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode bp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @BonfireEventEscrowDatum @EventAction

validator :: BonfireParam -> Validator
validator = validatorScript . typedValidator
