import { EventAvailability } from "common/interfaces/newEventInterface"
import { UserBaseDTO } from "common/interfaces/profileInterface"
import { WalletNavigationParams, PasswordSetUpFormValues } from "lib/wallet/types"
import { AnyObject } from "yup/lib/types"

export type AppStackParamList = {
  "Log In": undefined
  Initial: undefined
  Pricing: undefined
  "Create Account": undefined
  Success: {
    isBookingWalletTopUp?: boolean | undefined
    baseAddress?: string
    headerText?: string
    bodyText?: string
    isNewWalletCreation?: boolean
    navigationScreen: any
  }
  Confirmation:
    | {
        isBookingWalletTopUp: boolean | undefined
        isNewEvent: boolean | undefined
        isBookingConfirmation: boolean | undefined
        bodyText: string | undefined
      }
    | undefined
  "Add Funds": { fromScreen: string }
  "Attendee Navigation Screens": undefined
  "Navigation Screens": undefined
  "User Registration Screens": undefined
  "Onboarding Screens": undefined
  "Duration Choice": any
  "Legal Document": { type: "terms-of-service" | "privacy-policy" }
  "Qr-Code Scanner": any
  Wallet: undefined
}

export type NavigationTabParamList = {
  Calendar: { id: string }
  Browse: any
  "My Events": undefined
  Wallet: undefined
  "Add Funds": { fromScreen: string }
  Profile: undefined
}

export type BookingStackParamList = {
  "Search List": any
  "Available Dates":
    | {
        alias: string | undefined
        selectedEvent: any | undefined
      }
    | undefined
  "Available Event Dates Selection": { event: EventDescription }
  "Available Times": { event: EventDescription }
  "Duration Choice": { event: EventDescription }
  "Event Description": { event: EventDescription }
  "Add Funds": { event: EventDescription }
  "Booking Confirmation": { event: EventDescription }
  Confirmation: { event: EventDescription }
  "Event Details": { event: EventDescription }
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

export type MyEventsStackParamList = EventCreationParamList & {
  "User Events": undefined
  "Event Description": EventDescription
  "Event Details": EventDescription
}

export type ProfileStackParamList = {
  "Profile Main": undefined
  "Edit Profile": { userInfo: UserBaseDTO }
  "Profile Settings": undefined
}

export type WalletStackParamList = {
  "Wallet Main": { baseAddress: string } | undefined
  "Import Mnemonic": undefined
  "Import Mnemonic Confirmation": WalletNavigationParams
  "New Wallet Set Up": WalletNavigationParams
  "Mnemonic Preview": PasswordSetUpFormValues
  "Receive Transaction": any
  "Send Transaction": any
  "Preview Transaction": any
}

/**
 * Navigation params interfaces
 */
interface EventDescription {
  availabilities: EventAvailability[]
  title: string
  hourlyRate: any
  description: string
  organizerId: string
  organizerAlias: string
  fromDate: number | string
  toDate: number | string
  image: any
  eventTitleColor: string
  eventCardColor: string
  eventId: string
  fromScreen?: string
  isBookingWalletTopUp?: boolean
}

export enum DEEP_LINKING_PATHS {
  ADD_FUNDS = "add-funds",
  AVAILABLE_DAYS_SELECTION = "available-days-selection/:success",
  AVAILABLE_EVENTS_DAYS_SELECTION = "available-event-days-selection",
  AVAILABLE_TIMES = "available-times",
  BROWSE = "browse/:event-id",
  HOME = "home",
  NAVIGATION = "navigation",
  NEW_EVENT_DESCRIPTION = "new-event-description",
  ONBOARDING = "onboarding",
}

export enum DEEP_LINKING_URLS {
  ADD_FUNDS = "add-funds",
  AVAILABLE_TIMES = "available-times",
  AVAILABLE_DAYS_SELECTION = "available-days-selection",
  AVAILABLE_EVENTS_DAYS_SELECTION = "available-event-days-selection",
  BROWSE = "browse",
  HOME = "home",
  NAVIGATION = "navigation",
  NEW_EVENT_DESCRIPTION = "new-event-description",
  ONBOARDING = "onboarding",
}
