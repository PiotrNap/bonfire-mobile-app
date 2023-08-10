import * as React from "react"
import { ColorSchemeName } from "react-native"

import { AppContext } from "./appContext"
import { MyCalendarContext } from "./myCalendarContext"
import { BookingContext } from "./bookingContext"
import { EventCreationContext } from "./eventCreationContext"
import {
  Availabilities,
  CalendarHeader,
  NewCalendarMonths,
  PreviewingDayEvents,
  Event,
} from "interfaces/myCalendarInterface"
import { OrganizerRate } from "common/interfaces/bookingInterface"
import {
  EventAvailability,
  EventType,
  HourlyRate,
  SelectedWeekDays,
  TextContent,
} from "common/interfaces/newEventInterface"
import { EventCreationTypes } from "common/types/contextTypes"
import { JWTPayload, UserSettings } from "common/interfaces/appInterface"
import { WalletContext } from "./walletContext"

export const appContext = () => {
  const { state, dispatch } = React.useContext(AppContext)

  return {
    auth: state.authentication,
    accountType: state.accountType,
    colorScheme: state.colorScheme,
    pageIndex: state.pageIndex,
    appBgColor: state.appBgColor,
    validGoogleOAuth: state.validGoogleOAuth,
    favoriteOrganizers: state.favoriteOrganizers,
    receivingAddr: state.receivingAddr,
    ref: state.ref,
    textContent: state.textContent,
    userSettings: state.userSettings,
    resetAppState: () => dispatch({ type: "RESET_STATE" }),
    setUserSettings: (userSettings: UserSettings) =>
      dispatch({ type: "SET_USER_SETTINGS", payload: { userSettings } }),
    setRef: (ref: React.RefObject<any>) =>
      dispatch({ type: "SET_REF", payload: { ref } }),
    setJWT: (jwtPayload: JWTPayload) =>
      dispatch({ type: "SET_REF", payload: { jwtPayload } }),
    setReceivingAddr: (receivingAddr: string) =>
      dispatch({ type: "SET_REC_ADDR", payload: { receivingAddr } }),
    toggleAuth: (auth: boolean, accountType: string) => {
      dispatch({ type: "TOGGLE_AUTH", payload: { auth, accountType } })
    },
    setValidGoogleOAuth: (validGoogleOAuth: boolean) => {
      dispatch({
        type: "SET_VALID_GOOGLE_OAUTH",
        payload: { validGoogleOAuth },
      })
    },
    setFavoriteOrganizer: (alias: string) => {
      dispatch({ type: "SET_FAVORITE_ORGANIZER", payload: { alias } })
    },
    setColorScheme: (newColorScheme: ColorSchemeName) => {
      dispatch({
        type: "SET_COLOR_SCHEME",
        payload: { newColorScheme },
      })
    },
    setPageIndex: (pageIndex: number) => {
      dispatch({ type: "SET_PAGE_INDEX", payload: { pageIndex } })
    },
  }
}

export const eventCreationContext = () => {
  const { state, dispatch } = React.useContext(EventCreationContext)

  return {
    textContent: state.textContent,
    availabilities: state.availabilities,
    selectedDays: state.selectedDays,
    selectedWeekDays: state.selectedWeekDays,
    tags: state.tags,
    hourlyRate: state.hourlyRate,
    imageURI: state.imageURI,
    privateEvent: state.privateEvent,
    fromDate: state.fromDate,
    toDate: state.toDate,
    eventType: state.eventType,
    eventCardColor: state.eventCardColor,
    eventTitleColor: state.eventTitleColor,
    gCalEventsBooking: state.gCalEventsBooking,
    setTextContent: (textContent: TextContent) => {
      dispatch({
        type: EventCreationTypes.SetTextContent,
        payload: { textContent },
      })
    },
    setSelectedDays: (
      selectedDays: any[],
      isRecurringSelection: boolean = false,
      eventType: EventType
    ) => {
      dispatch({
        type: EventCreationTypes.SetSelectedDays,
        payload: { selectedDays, isRecurringSelection, eventType },
      })
    },
    setSelectedWeek: (selectedWeek: SelectedWeekDays) => {
      dispatch({
        type: EventCreationTypes.SetSelectedWeek,
        payload: { selectedWeek },
      })
    },
    addAvailability: (availability: EventAvailability) => {
      dispatch({
        type: EventCreationTypes.AddAvailability,
        payload: { availability },
      })
    },
    removeAvailability: (availability: EventAvailability) => {
      dispatch({
        type: EventCreationTypes.RemoveAvailability,
        payload: { availability },
      })
    },
    removeAvailabilities: () => {
      dispatch({
        type: EventCreationTypes.RemoveAvailabilities,
        payload: {},
      })
    },
    setHourlyRate: (hourlyRate: HourlyRate) => {
      dispatch({
        type: EventCreationTypes.SetHourlyRate,
        payload: { hourlyRate },
      })
    },
    setEventType: (eventType: EventType) => {
      dispatch({
        type: EventCreationTypes.SetEventType,
        payload: { eventType },
      })
    },
    setImageUri: (imageURI: string) => {
      dispatch({
        type: EventCreationTypes.SetImageURI,
        payload: { imageURI },
      })
    },
    setTags: (tags: string[]) => {
      dispatch({
        type: EventCreationTypes.SetTags,
        payload: { tags },
      })
    },
    setPrivateEvent: (privateEvent: boolean) => {
      dispatch({
        type: EventCreationTypes.SetPrivateEvent,
        payload: { privateEvent },
      })
    },
    setDateFrame: (fromDate: Date, toDate: Date) => {
      dispatch({
        type: EventCreationTypes.SetDateFrame,
        payload: { fromDate, toDate },
      })
    },
    setEventCardColor: (color: string) => {
      dispatch({
        type: EventCreationTypes.SetEventCardColor,
        payload: { eventCardColor: color },
      })
    },
    setEventTitleColor: (color: string) => {
      dispatch({
        type: EventCreationTypes.SetEventTitleColor,
        payload: { eventTitleColor: color },
      })
    },
    setGCalEventsBooking: (gCalEventsBooking: boolean) => {
      dispatch({
        type: EventCreationTypes.SetGCalEventsBooking,
        payload: { gCalEventsBooking },
      })
    },
    removeSelectedDays: () =>
      dispatch({ type: EventCreationTypes.RemoveSelectedDays }),
    removeSelectedWeeks: () =>
      dispatch({ type: EventCreationTypes.RemoveSelectedWeeks }),
    resetEventCreationState: () => {
      dispatch({ type: EventCreationTypes.ResetState })
    },
  }
}

export const bookingContext = () => {
  const { state, dispatch } = React.useContext(BookingContext)

  return {
    duration: state.duration,
    durationCost: state.durationCost,
    eventTitle: state.eventTitle,
    pickedDate: state.pickedDate,
    eventCardInfo: state.eventCardInfo,
    organizerRate: state.organizerRate,
    previewingOrganizer: state.previewingOrganizer,
    previewingEvent: state.previewingEvent,
    maxTimeSlotDuration: state.maxTimeSlotDuration,
    minTimeSlotDuration: state.minTimeSlotDuration,
    createGoogleCalEvent: state.createGoogleCalEvent,
    setDuration: (duration: number) =>
      dispatch({ type: "SET_DURATION", payload: { duration } }),
    setDurationCost: (durationCost: number) =>
      dispatch({
        type: "SET_DURATION_COST",
        payload: { durationCost },
      }),
    setEventTitle: (title: string) =>
      dispatch({ type: "SET_EVENT_TITLE", payload: { title } }),
    setOrganizerRate: (organizerRate: OrganizerRate) =>
      dispatch({
        type: "SET_ORGANIZER_RATE",
        payload: { organizerRate },
      }),
    setPickedDate: (pickedDate: number | null) =>
      dispatch({ type: "SET_PICKED_DATE", payload: { pickedDate } }),
    setMaxTimeSlotDuration: (maxTimeSlotDuration: number | undefined) =>
      dispatch({
        type: "SET_MAX_TIME_SLOT_DUR",
        payload: { maxTimeSlotDuration },
      }),
    setMinTimeSlotDuration: (minTimeSlotDuration: number | undefined) =>
      dispatch({
        type: "SET_MIN_TIME_SLOT_DUR",
        payload: { minTimeSlotDuration },
      }),
    setPreviewingOrganizer: (previewingOrganizer: any) =>
      dispatch({
        type: "SET_PREVIEWING_ORGANIZER",
        payload: { previewingOrganizer },
      }),
    setPreviewingEvent: (previewingEvent: any) =>
      dispatch({
        type: "SET_PREVIEWING_EVENT",
        payload: { previewingEvent },
      }),
    setCreateGoogleCalEvent: (createGoogleCalEvent: boolean) =>
      dispatch({
        type: "SET_CREATE_GCAL_EVENT",
        payload: { createGoogleCalEvent },
      }),
    resetBookingState: () => {
      dispatch({
        type: "RESET_STATE",
        payload: {},
      })
    },
  }
}

export const myCalendarContext = () => {
  const { state, dispatch } = React.useContext(MyCalendarContext)

  return {
    calendar: state.calendar,
    calendarHeader: state.calendarHeader,
    availabilities: state.availabilities,
    availabilitiesCalendar: state.availabilitiesCalendar,
    organizerAvailabilities: state.organizerAvailabilities,
    events: state.events,
    registrationDate: state.registrationDate,
    previewingDayEvents: state.previewingDayEvents,
    currentSelectedDay: state.currentSelectedDay,
    direction: state.direction,
    resetCalendarState: () => {
      dispatch({ type: "RESET_STATE" })
    },
    addEvent: (event: Event) => {
      dispatch({ type: "ADD_EVENT", payload: event })
    },
    addAvailability: (availabilities: Availabilities[]) => {
      dispatch({ type: "ADD_AVAILABILITY", payload: availabilities })
    },
    changeMonthHeader: (calendarHeader: CalendarHeader) => {
      dispatch({
        type: "CHANGE_MONTH_HEADER",
        payload: { calendarHeader },
      })
    },
    clearPreviewDayEvents: () => {
      dispatch({
        type: "CLEAR_DAY_PREVIEW",
      })
    },
    setCalendarDirection: (direction: null | "previous" | "next") => {
      dispatch({
        type: "SET_CALENDAR_DIRECTION",
        payload: { direction },
      })
    },
    loadMyCalendar: (calendarArgs: NewCalendarMonths) => {
      dispatch({ type: "LOAD_MY_CALENDAR", payload: { calendarArgs } })
    },
    loadInitialMyCalendar: () => {
      dispatch({ type: "LOAD_INITIAL_MY_CALENDAR" })
    },
    setAvailCalendar: (
      availabilities: any,
      calendarArgs?: NewCalendarMonths
    ) => {
      dispatch({
        type: "SET_AVAIL_CALENDAR",
        payload: { availabilities, calendarArgs },
      })
    },
    setCurrSelectedDay: (selectedDay: any) => {
      dispatch({
        type: "SET_CURR_SELECTED_DAY",
        payload: { selectedDay },
      })
    },
    setOrganizerAvail: (availabilities: any) => {
      dispatch({
        type: "SET_ORGANIZER_AVAIL",
        payload: { availabilities },
      })
    },
    setEvents: (events: any) => {
      dispatch({ type: "SET_EVENTS", payload: { events } })
    },
    previewDayEvents: (newPreviewingDayEvents: PreviewingDayEvents) => {
      dispatch({
        type: "PREVIEW_DAY_EVENTS",
        payload: { newPreviewingDayEvents },
      })
    },
    updateCalendarMonth: (calendarArgs: NewCalendarMonths) => {
      dispatch({
        type: "UPDATE_CALENDAR_MONTH",
        payload: { calendarArgs },
      })
    },
  }
}

export const walletContext = () => {
  const { state, dispatch } = React.useContext(WalletContext)

  return {
    mnemonic: state.mnemonic,
    lovelaceBalance: state.lovelaceBalance,
    setLovelaceBalance: (lovelace: BigInt) => {
      dispatch({
        type: "SET_LOVELACE_BALANCE",
        payload: { lovelace },
      })
    },
    setMnemonic: (mnemonic: {}) => {
      dispatch({
        type: "SET_MNEMONIC",
        payload: { mnemonic },
      })
    },
  }
}
