import * as React from "react"

import { EventCreationActions } from "common/types/contextTypes"
import { AssetUnit } from "lib/wallet/types"
import { MarkedDates } from "react-native-calendars/src/types"
import { Time } from "./myCalendarInterface"

export interface ProviderProps {
  children: React.ReactNode
}

export interface ContextObjectProps {
  state: InitialState
  dispatch: React.Dispatch<EventCreationActions>
}

export interface InitialState {
  textContent: TextContent
  availabilities: EventAvailability[]
  selectedDates: MarkedDates
  fromDate: Date | null
  toDate: Date | null
  hourlyRate: AssetUnit[]
  eventType: EventType
  imageURI: string
  selectedWeekDays: SelectedWeekDays[]
  cancellation: Cancellation
  visibility: EventVisibility
  eventCardColor: string
  eventTitleColor: string
  gCalEventsBooking: boolean
}

export type SelectedDays = { [key: string]: number }
export type SelectedWeekDays = { [key: string]: any }
export type EventType = "one-time" | "recurring"
export type EventVisibility = "public" | "private"

export interface TextContent {
  title: string
  summary: string
}

export interface Cancellation {
  fee: number // fee % based off of ADA cost
  window: number // cancellation window before event start date in hours
}

export interface EventAvailability {
  from: Time
  to: Time
  maxDuration: number
  minDuration: number
  localeTimeOffset?: number
}
