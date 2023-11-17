import {
  Cancellation,
  EventAvailability,
  EventVisibility,
  SelectedDays,
} from "common/interfaces/newEventInterface"

export interface EventUser {
  id: string
  username: string
}

export interface CreateEventDto {
  title: string
  description: string
  selectedDays: SelectedDays
  availabilities: EventAvailability[]
  fromDate: Date | null
  toDate: Date | null
  hourlyRate: string // AssetUnit[] converted to JSONSchema
  cancellation: Cancellation
  visibility: EventVisibility
  eventCardColor: string
  eventTitleColor: string
  organizer: EventUser
  gCalEventsBooking: boolean
  timeZoneOffset: number
}
