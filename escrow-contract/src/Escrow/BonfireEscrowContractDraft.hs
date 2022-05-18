{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Escrow.BonfireEscrowContractDraft where

import              Ledger              hiding (singleton)
import              Ledger.Ada
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))

-- IF we create a separate "Dispute Contract", should its ValidatorHash be referenced here,
-- OR the other way around?

data BonfireParam = BonfireParam
    { organizerAccessSymbol :: !CurrencySymbol
    , disputeContract       :: !ValidatorHash
    , ptSymbol              :: !CurrencySymbol
    , ptName                :: !TokenName
    }

PlutusTx.makeLift ''BonfireParam

data BonfireEventEscrowDatum = BonfireEventEscrowDatum
  {
    organizerReference    :: !BuiltinByteString -- reference to our Bonfire DB (still thinking about how exactly to use this)
  , eventReference        :: !BuiltinByteString -- reference to our Bonfire DB (still thinking about how exactly to use this)
  , organizerPkh          :: !PubKeyHash
  , attendeePkh           :: !PubKeyHash
  , eventCostLovelace     :: !Integer
  , eventCostPaymentToken :: !Integer -- In V0, "gimbal"
  , eventStartTime        :: !POSIXTime
  }

-- do we need event end time?

-- think about leveled auth/access NFTs for Organizers that unlock new payment tiers... (2022-04-06)
-- Organizer's scheduling window is an account preference stored in Bonfire DB

PlutusTx.unstableMakeIsData ''BonfireEventEscrowDatum

data EventAction = AttCancel | OrgCancel | Complete | Dispute
  deriving Show

PlutusTx.makeIsDataIndexed ''EventAction [('AttCancel, 0), ('OrgCancel, 1), ('Complete, 2), ('Dispute, 3)]
PlutusTx.makeLift ''EventAction

-- How do we guard against double-booking an event?
-- Does a token play a role in representing a single event?
-- Would that token be created at the time of event CREATION by ORGANIZER?
-- Or at the time of event BOOKING by ATTENDEE?

{-# INLINEABLE mkValidator #-}
mkValidator :: BonfireParam -> BonfireEventEscrowDatum -> EventAction -> ScriptContext -> Bool
mkValidator bp edatum action ctx =
  case action of
    AttCancel     ->  traceIfFalse "Attendee must sign Cancellation Tx" signedByAttendee &&
                      traceIfFalse "The Cancellation deadline has passed" beforeCancelDeadline &&
                      traceIfFalse "Output must be returned to Attendee" sufficientOutputToAttendee
                      -- allow attendee to cancel up until (STARTTIME - 24 HOURS); but within 24 hours, they forfeit funds

    OrgCancel     ->  traceIfFalse "Organizer must sign Cancellation Tx" signedByOrganizer &&
                      traceIfFalse "Output must be returned to Attendee" sufficientOutputToAttendee

    Complete      ->  traceIfFalse "Organizer must sign Cancellation Tx" signedByOrganizer &&
                      traceIfFalse "It is too early to collect" afterDisputeDeadline &&
                      traceIfFalse "Output must be sent to Organizer" sufficientOutputToOrganizer

                  -- TODO:
                  -- 1. Check for Organizer's Access Token
                  -- 2. What % is Bonfire Treasury taking?

    -- TODO: Write a simple dispute contract
    Dispute       ->  traceIfFalse "Attendee must sign Cancellation Tx" signedByAttendee &&
                      traceIfFalse "Must pay to Dispute Contract" sufficientOutputToDisputeContract &&
                      traceIfFalse "It is too late for you to Dispute!" beforeDisputeDeadline
                -- for now, what can we promise in terms of dispute resolution?
                -- (someday, we'll be passing users along to Loxe.inc)
                -- Send utxo to a different contract where our team can mediate disputes
                -- (this raises questions of scale)

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
    disputeDeadline = (eventStartTime edatum) + 3600000
    -- 3600000 milliseconds = 1 hour

    cancelDeadline :: POSIXTime
    cancelDeadline = (eventStartTime edatum) - 86400000
    -- 86400000 milliseconds = 1 day

    beforeCancelDeadline :: Bool
    beforeCancelDeadline = contains (to cancelDeadline) $ txInfoValidRange info

    afterDisputeDeadline :: Bool
    afterDisputeDeadline = contains (from disputeDeadline) $ txInfoValidRange info

    beforeDisputeDeadline :: Bool
    beforeDisputeDeadline = not afterDisputeDeadline

    -- Handle Outputs
    valueToAttendee :: Value
    valueToAttendee = valuePaidTo info $ attendeePkh edatum

    valueToOrganizer :: Value
    valueToOrganizer = valuePaidTo info $ organizerPkh edatum

    sufficientOutputToAttendee :: Bool
    sufficientOutputToAttendee =  (getLovelace $ fromValue valueToAttendee) >= (eventCostLovelace edatum) &&
                                  (valueOf valueToAttendee (ptSymbol bp) (ptName bp)) >= (eventCostPaymentToken edatum)

    sufficientOutputToOrganizer :: Bool
    sufficientOutputToOrganizer = (getLovelace $ fromValue valueToOrganizer) >= (eventCostLovelace edatum) &&
                                  (valueOf valueToOrganizer (ptSymbol bp) (ptName bp)) >= (eventCostPaymentToken edatum)

    -- Dispute
    valueToDisputeContract :: Value
    valueToDisputeContract = valueLockedBy info (disputeContract bp)

    sufficientOutputToDisputeContract :: Bool
    sufficientOutputToDisputeContract = (getLovelace $ fromValue valueToDisputeContract) >= (eventCostLovelace edatum) &&
                                        (valueOf valueToDisputeContract (ptSymbol bp) (ptName bp)) >= (eventCostPaymentToken edatum)


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
