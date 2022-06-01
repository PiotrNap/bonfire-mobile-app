{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Escrow.Types where

import Ledger hiding (singleton)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))

data BonfireParam = BonfireParam
  { organizerAccessSymbol :: !CurrencySymbol,
    disputeContract :: !ValidatorHash,
    ptSymbol :: !CurrencySymbol,
    ptName :: !TokenName,
    treasuryPkh :: !PubKeyHash
  }

PlutusTx.makeLift ''BonfireParam

data BonfireEventEscrowDatum = BonfireEventEscrowDatum
  { organizerReference :: !BuiltinByteString, -- reference to our Bonfire DB (still thinking about how exactly to use this)
    eventReference :: !BuiltinByteString, -- reference to our Bonfire DB (still thinking about how exactly to use this)
    organizerPkh :: !PubKeyHash,
    attendeePkh :: !PubKeyHash,
    eventCostLovelace :: !Integer,
    eventCostPaymentToken :: !Integer, -- In V0, "gimbal"
    eventStartTime :: !POSIXTime
  }

-- do we need event end time?

-- think about leveled auth/access NFTs for Organizers that unlock new payment tiers... (2022-04-06)
-- Organizer's scheduling window is an account preference stored in Bonfire DB

PlutusTx.unstableMakeIsData ''BonfireEventEscrowDatum

data EventAction = AttCancel | OrgCancel | Complete | Dispute
  deriving (Show)

PlutusTx.makeIsDataIndexed ''EventAction [('AttCancel, 0), ('OrgCancel, 1), ('Complete, 2), ('Dispute, 3)]
PlutusTx.makeLift ''EventAction

-- Dispute Contract
--
data DisputeParam = DisputeParam
  { bonfireAdminToken :: !CurrencySymbol,
    dpPtSymbol :: !CurrencySymbol,
    dpPtName :: !TokenName,
    bonfireTreasuryPkh :: !PubKeyHash
  }

PlutusTx.makeLift ''DisputeParam

data BonfireDisputeDatum = BonfireDisputeDatum
  { bddOrganizerPkh :: !PubKeyHash,
    bddAttendeePkh :: !PubKeyHash,
    bddEventCostLovelace :: !Integer,
    bddEventCostPaymentToken :: !Integer,
    bddEventID :: !BuiltinByteString,
    bddDisputeID :: !BuiltinByteString
  }

PlutusTx.unstableMakeIsData ''BonfireDisputeDatum

data DisputeResult = PayAttendee | PayOrganizer | Split

PlutusTx.makeIsDataIndexed ''DisputeResult [('PayAttendee, 0), ('PayOrganizer, 1), ('Split, 2)]
PlutusTx.makeLift ''DisputeResult
