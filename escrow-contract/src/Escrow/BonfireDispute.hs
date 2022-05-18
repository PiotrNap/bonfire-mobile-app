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

module Escrow.BonfireDispute where

import              Ledger              hiding (singleton)
import              Ledger.Ada
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))

-- IF we create a separate "Dispute Contract", should its ValidatorHash be referenced here,
-- OR the other way around?

data DisputeParam = DisputeParam
    { bonfireAdminToken         :: !CurrencySymbol
    , ptSymbol                  :: !CurrencySymbol
    , ptName                    :: !TokenName
    , bonfireTreasuryPkh        :: !PubKeyHash
    }

PlutusTx.makeLift ''DisputeParam

data BonfireDisputeDatum = BonfireDisputeDatum
  {
    organizerPkh          :: !PubKeyHash
  , attendeePkh           :: !PubKeyHash
  , eventCostLovelace     :: !Integer
  , eventCostPaymentToken :: !Integer
  , eventID               :: !BuiltinByteString
  , disputeID             :: !BuiltinByteString
  }

PlutusTx.unstableMakeIsData ''BonfireDisputeDatum

data DisputeResult = PayAttendee | PayOrganizer | Split
  deriving Show

PlutusTx.makeIsDataIndexed ''DisputeResult [('PayAttendee, 0), ('PayOrganizer, 1), ('Split, 2)]
PlutusTx.makeLift ''DisputeResult

{-# INLINEABLE mkValidator #-}
mkValidator :: DisputeParam -> BonfireDisputeDatum -> DisputeResult -> ScriptContext -> Bool
mkValidator dp dd res ctx =
  case result of
    PayAttendee     ->  traceIfFalse "Must include Bonfire Admin Token" inputHasAdminToken &&
                        traceIfFalse "Must pay to Attendee" outputToAttendee
    PayOrganizer    ->  traceIfFalse "Must include Bonfire Admin Token" inputHasAdminToken &&
                        traceIfFalse "Must pay to Organizer" outputToOrganizer
    Split           ->  traceIfFalse "Must include Bonfire Admin Token" inputHasAdminToken &&
                        traceIfFalse "Must pay to Attendee and Organizer" outputToBoth

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    inputHasAdminToken :: Bool
    inputHasAdminToken = (bonfireAdminToken dp) `elem` inVals

    valueToAttendee :: Value
    valueToAttendee = valuePaidTo info $ attendeePkh dd

    valueToOrganizer :: Value
    valueToOrganizer = valuePaidTo info $ organizerPkh dd

    valueToTreasury :: Value
    valueToTreasury = valuePaidTo info $ bonfireTreasuryPkh dp

    outputToAttendee :: Bool
    outputToAttendee = valueOf valueToAttendee (ptSymbol dp) (ptName dp) >= 9 % 10 * eventCostPaymentToken dd &&
                       getLovelace $ fromValue valueToAttendee >= 9 % 10 * eventCostLovelace dd &&
                       valueOf valueToTreasury (ptSymbol dp) (ptName dp) >= 1 % 10 * eventCostPaymentToken dd &&
                       getLovelace $ fromValue valueToTreasury >= 1 % 10 * eventCostLovelace dd

    outputToOrganizer :: Bool
    outputToOrganizer = valueOf valueToOrganizer (ptSymbol dp) (ptName dp) >= 9 % 10 * eventCostPaymentToken dd &&
                        getLovelace $ fromValue valueToOrganizer >= 9 % 10 * eventCostLovelace dd &&
                        valueOf valueToTreasury (ptSymbol dp) (ptName dp) >= 1 % 10 * eventCostPaymentToken dd &&
                        getLovelace $ fromValue valueToTreasury >= 1 % 10 * eventCostLovelace dd

    outputToBoth :: Bool
    outputToBoth = valueOf valueToAttendee (ptSymbol dp) (ptName dp) >= 45 % 100 * eventCostPaymentToken dd &&
                   getLovelace $ fromValue valueToAttendee >= 45 % 100 * eventCostLovelace dd &&
                   valueOf valueToOrganizer (ptSymbol dp) (ptName dp) >= 45 % 100 * eventCostPaymentToken dd &&
                   getLovelace $ fromValue valueToOrganizer >= 45 % 100 * eventCostLovelace dd &&
                   valueOf valueToTreasury (ptSymbol dp) (ptName dp) >= 1 % 10 * eventCostPaymentToken dd &&
                   getLovelace $ fromValue valueToTreasury >= 1 % 10 * eventCostLovelace dd


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
