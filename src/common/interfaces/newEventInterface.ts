import * as React from "react"

import { EventCreationActions } from "common/types/contextTypes"

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
  selectedDays: SelectedDays
  tags: string[]
  fromDate: Date | null
  toDate: Date | null
  hourlyRate: HourlyRate
  eventType: EventType
  imageURI: string
  selectedWeekDays: SelectedWeekDays[]
  privateEvent: boolean
  eventCardColor: string
  eventTitleColor: string
  gCalEventsBooking: boolean
}

export type SelectedDays = { [key: string]: number }
export type SelectedWeekDays = { [key: string]: any }
export type EventType = "One-Time" | "Recurring"

export interface TextContent {
  title: string
  description: string
}

export interface HourlyRate {
  ada: number
  gimbals: number
}

export interface EventAvailability {
  from: Date | number
  to: Date | number
  maxDuration: number
  minDuration: number
  localeTimeOffset?: number
}
