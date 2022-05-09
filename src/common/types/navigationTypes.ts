import {
  UserBaseDTO,
  UserOrganizerDTO,
} from "common/interfaces/profileInterface"
import { AnyObject } from "yup/lib/types"

export type AppStackParamList = {
  "Log In": undefined
  Initial: undefined
  Pricing: undefined
  "Create Account": undefined
  "Deposit Successful":
    | {
        isBookingWalletTopUp: boolean | undefined
        fromScreen: any
      }
    | undefined
  Confirmation:
    | {
        isBookingWalletTopUp: boolean | undefined
        isBookingConfirmation: boolean | undefined
      }
    | undefined
  "Add Funds": { fromScreen: string }
  "Attendee Navigation Screens": undefined
  "Organizer Navigation Screens": undefined
  "User Registration Screens": undefined
  "Onboarding Screens": undefined
  "Duration Choice": any
  Wallet: undefined
}

export type AttendeeTabParamList = {
  "Home Stack": { id: string }
  "Browse Stack": any
  Wallet: undefined
  "Add Funds": { fromScreen: string }
  Profile: undefined
}

export type OrganizerTabParamList = AttendeeTabParamList & {
  "My Events": undefined
}

export type BookingStackParamList = {
  Browse: any
  "Available Dates":
    | {
        alias: string | undefined
        selectedEvent: any | undefined
      }
    | undefined
  "Available Event Days Selection": EventDescription
  "Available Times": EventDescription
  "Duration Choice": EventDescription
  "Event Description": EventDescription
  "Add Funds": EventDescription
  "Booking Confirmation": EventDescription
  Confirmation: any
  "Event Details": EventDescription
}

export type EventCreationParamList = {
  Home: undefined
  "New Event Description": undefined
  "Available Days Selection": undefined
  "Available Time Selection": { availabilities: any } | undefined
  "Image Cover Selection": undefined
  "Event Card Customization": undefined
  "Event Confirmation Details":
    | {
        isNewEvent?: boolean
        isCalendarEventPreview?: boolean
        organizerEvent?: AnyObject
        organizerCalendarEvent?: AnyObject
        bookedEvent?: AnyObject
        header?: string
      }
    | undefined
}

export type MyEventsStackParamList = {
  "My Events": undefined
  "Event Description": EventDescription
  "Event Details": EventDescription
}

export type ProfileStackParamList = {
  Profile: undefined
  "Edit Profile": { userInfo: UserBaseDTO | UserOrganizerDTO }
  "Profile Settings": undefined
}

/**
 * Navigation params interfaces
 */
interface EventDescription {
  title: string
  titleColor: string
  description: string
  organizerId: string
  organizerAlias: string
  fromDate: number | string
  toDate: number | string
  image: any
  color: string
  eventId: string
  fromScreen?: string
  isBookingWalletTopUp?: boolean
}

export enum DEEP_LINKING_PATHS {
  ADD_FUNDS = "add-funds",
  AVAILABLE_DAYS_SELECTION = "available-days-selection/:success",
  AVAILABLE_EVENTS_DAYS_SELECTION = "available-event-days-selection",
  BROWSE = "browse/:event-id",
  HOME = "home",
  NAVIGATION = "navigation",
  NEW_EVENT_DESCRIPTION = "new-event-description",
  ONBOARDING = "onboarding",
}

export enum DEEP_LINKING_URLS {
  ADD_FUNDS = "add-funds",
  AVAILABLE_DAYS_SELECTION = "available-days-selection",
  AVAILABLE_EVENTS_DAYS_SELECTION = "available-event-days-selection",
  BROWSE = "browse",
  HOME = "home",
  NAVIGATION = "navigation",
  NEW_EVENT_DESCRIPTION = "new-event-description",
  ONBOARDING = "onboarding",
}
