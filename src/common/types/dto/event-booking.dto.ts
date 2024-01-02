export interface NewEventSlotReservation {
  eventId: string
  startDate: string
  duration: number
  durationCost: string
}

export interface EventSlotReservation {
  id: string // id of reserved booking-slot
  ttl: number // time until this reservation expires
}

export interface EventSlotTransactionInfo {
  id: string // id of reserved booking-slot
  lockingTxHash: string
  datumHash: string // used to reference the utxo for a payout
}

export interface EventBookingSlot {
  id: string
  organizerId: string
  organizerAlias: string
  attendeeId: string
  attendeeAlias: string
  eventId: string
  eventTitle: string
  eventDescription: string
  duration: number
  cost: string
  fromDate: string
  toDate: string
  unlockingTxHash: string
  lockingTxHash: string
  datumHash: string
}
