import {
  Cancellation,
  EventAvailability,
  EventVisibility,
} from "common/interfaces/newEventInterface"
import { NetworkId } from "lib/wallet/types"

export interface EventUser {
  id: string
  username: string
}

export interface CreateEventDto {
  title: string
  description: string
  availabilities: EventAvailability[]
  fromDate: string
  toDate: string
  hourlyRate: string // JSONSchema of payment tokens
  cancellation: Cancellation
  visibility: EventVisibility
  eventCardColor: string
  eventTitleColor: string
  organizerId: string
  note: string
  networkId: NetworkId
}
