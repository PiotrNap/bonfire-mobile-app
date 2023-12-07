import {
  Cancellation,
  EventAvailability,
  EventVisibility,
} from "common/interfaces/newEventInterface"

export interface EventUser {
  id: string
  username: string
  baseAddress: string
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
  organizer: EventUser
  note: string
}
