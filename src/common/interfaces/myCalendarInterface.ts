export interface MyCalendarState {
  calendar: any
  registrationDate: number | null
  availabilities: Availabilities[] | null | undefined
  events: Events[] | null | undefined
  calendarHeader: CalendarHeader
  previewingDayEvents?: PreviewingDayEvents
  direction: null | "previous" | "next"
  organizerAvailabilities: any
  availabilitiesCalendar: any
  currentSelectedDay: number | null
}

export interface Availabilities {
  year: number
  months: AvailabilitiesMonth[]
}

export interface AvailabilitiesMonth {
  month: string
  days: AvailabilitiesDay[]
}

export interface AvailabilitiesDay {
  day: number
  timeSlots: AvailabilitySlot[]
  availableUntil?: number
}

export interface AvailabilitySlot {
  fromTime: number | string
  toTime: number | string
}

export interface Events {
  year: number
  months: EventsMonth[]
}

export interface EventsMonth {
  month: string
  days: EventsDay[]
  totalNumOfEvents: number
}

export interface EventsDay {
  day: number
  events: Event[]
}

export interface Event {
  id: string
  fromTimeSlot: string | number
  toTimeSlot: string | number
  title: string
  fromTime: number | string
  toTime: number | string
  description: string
  participants: string[]
  type: "booked slot" | "scheduled slot" | "active slot"
  fromDate?: string
  toDate?: string
  organizerAlias?: string
  organizerId?: string
  attendeeAlias?: string
  availabilities?: Availability[]
  availabilityDate?: number | string
}

export interface Availability {
  from: string | number
  to: string | number
  maxDuration: number
  minDuration: number
}

export interface MyCalendarContextProps {
  state: MyCalendarState
  dispatch: React.Dispatch<any>
}

export interface NewCalendarMonths {
  nextMonths: boolean
  month: number
  year?: number
  isBookingCalendar?: boolean
  isRegularCalendar?: boolean
}

export interface PreviewingDayEvents {
  month: string | undefined
  day: number | undefined
  events: Event[] | undefined | []
}

export interface CalendarHeader {
  month: string
  year: number
  numOfEvents?: number | undefined
}

export interface Month {
  name: string
  firstDayName: string
  year: number
  numOfEvents?: number
  numOfAvailabilities?: number
  numOfDays: number
  days: Day[]
}

export interface Day {
  name: string
  number: number
  isAvailable?: boolean
  isLastWeek?: boolean
  availabilities?: AvailabilitySlot[]
  events?: Event[]
  direction?: "previous" | "next" | undefined
}

export interface Date extends Day {
  year?: number
  month?: number
  day: number
}

export enum CalendarSectionTitles {
  today = "Today",
  thisMonth = "This month",
}
