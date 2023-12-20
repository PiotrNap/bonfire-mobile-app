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
  Cancellation,
  EventAvailability,
  EventType,
  EventVisibility,
  SelectedWeekDays,
  TextContent,
} from "common/interfaces/newEventInterface"
import { EventCreationTypes } from "common/types/contextTypes"
import { JWTPayload, UserSettings } from "common/interfaces/appInterface"
import { WalletContext } from "./walletContext"
import {
  AssetUnit,
  SendLockingTxInfo,
  SendRegularTxInfo,
  SendUnlockingTxInfo,
  Assets,
  WalletKeys,
  SeedPhraseWordCount,
} from "lib/wallet/types"
import { TxInput } from "@hyperionbt/helios"
import { MarkedDates } from "react-native-calendars/src/types"

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
    bottomNavigationHeight: state.bottomNavigationHeight,
    ref: state.ref,
    textContent: state.textContent,
    userSettings: state.userSettings,
    qrCodeValue: state.qrCodeValue,
    resetAppState: () => dispatch({ type: "RESET_STATE" }),
    setQrCodeValue: (qrCodeValue: string) =>
      dispatch({ type: "SET_QRCODE_VALUE", payload: { qrCodeValue } }),
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
    setBottomNavigationHeight: (height: number) => {
      dispatch({ type: "SET_BOTTOMNAVIGATION_HEIGHT", payload: { height } })
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
    selectedDates: state.selectedDates,
    selectedWeekDays: state.selectedWeekDays,
    hourlyRate: state.hourlyRate,
    imageURI: state.imageURI,
    visibility: state.visibility,
    cancellation: state.cancellation,
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
    setSelectedDates: (selectedDates: MarkedDates) => {
      dispatch({
        type: EventCreationTypes.SetSelectedDates,
        payload: { selectedDates },
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
    setHourlyRate: (hourlyRate: AssetUnit[]) => {
      dispatch({
        type: EventCreationTypes.SetHourlyRate,
        payload: { hourlyRate },
      })
    },
    setImageUri: (imageURI: string) => {
      dispatch({
        type: EventCreationTypes.SetImageURI,
        payload: { imageURI },
      })
    },
    setCancellation: (cancellation: Cancellation) => {
      dispatch({
        type: EventCreationTypes.SetEventCancellation,
        payload: { cancellation },
      })
    },
    setEventVisibility: (visibility: EventVisibility) => {
      dispatch({
        type: EventCreationTypes.SetEventVisibility,
        payload: { visibility },
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
    removeSelectedWeeks: () => dispatch({ type: EventCreationTypes.RemoveSelectedWeeks }),
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
    pickedDateSlots: state.pickedDateSlots,
    pickedDateSlotsMinDuration: state.pickedDateSlotsMinDuration,
    pickedStartTime: state.pickedStartTime,
    eventCardInfo: state.eventCardInfo,
    organizerRate: state.organizerRate,
    previewingOrganizer: state.previewingOrganizer,
    previewingEvent: state.previewingEvent,
    maxTimeSlotDuration: state.maxTimeSlotDuration,
    minTimeSlotDuration: state.minTimeSlotDuration,
    createGoogleCalEvent: state.createGoogleCalEvent,
    setDuration: (duration: number) =>
      dispatch({ type: "SET_DURATION", payload: { duration } }),
    setPickedStartTime: (pickedStartTime: string) =>
      dispatch({ type: "SET_PICKED_START_TIME", payload: { pickedStartTime } }),
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
    setPickedDate: (pickedDate: string) =>
      dispatch({ type: "SET_PICKED_DATE", payload: { pickedDate } }),
    setPickedDateSlots: (pickedDateSlots: string) =>
      dispatch({ type: "SET_PICKED_DATE_SLOTS", payload: { pickedDateSlots } }),
    setPickedDateSlotsMinDuration: (pickedDateSlotsMinDuration: string) =>
      dispatch({
        type: "SET_PICKED_DATE_SLOTS_MIN_DURATION",
        payload: { pickedDateSlotsMinDuration },
      }),
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
    setAvailCalendar: (availabilities: any, calendarArgs?: NewCalendarMonths) => {
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
    baseAddress: state.baseAddress,
    rootKeyHex: state.rootKeyHex,
    accountPubKeyHex: state.accountPubKeyHex,
    accountKeyHex: state.accountKeyHex,
    isOfflineMnemonic: state.isOfflineMnemonic,
    txHistory: state.txHistory,
    walletUtxos: state.walletUtxos,
    walletAssets: state.walletAssets,
    sendTxInfo: state.sendTxInfo,
    seedPhraseWordCount: state.seedPhraseWordCount,
    setLovelaceBalance: (lovelace: bigint) => {
      dispatch({
        type: "SET_LOVELACE_BALANCE",
        payload: { lovelace },
      })
    },
    setBaseAddress: (baseAddress: string) => {
      dispatch({
        type: "SET_BASE_ADDRESS",
        payload: { baseAddress },
      })
    },
    setSendTxInfo: (
      sendTxInfo: SendRegularTxInfo | SendLockingTxInfo | SendUnlockingTxInfo
    ) => {
      dispatch({
        type: "SET_SEND_TX_INFO",
        payload: { sendTxInfo },
      })
    },
    setMnemonic: (mnemonic: {}) => {
      dispatch({
        type: "SET_MNEMONIC",
        payload: { mnemonic },
      })
    },
    setWalletKeys: (walletKeys: WalletKeys) => {
      dispatch({
        type: "SET_WALLET_KEYS",
        payload: { walletKeys },
      })
    },
    setIsOfflineMnemonic: (isOfflineMnemonic: boolean) => {
      dispatch({
        type: "SET_IS_OFFLINE_MNEMONIC",
        payload: { isOfflineMnemonic },
      })
    },
    setTxHistory: (txHistory: any[]) => {
      dispatch({
        type: "SET_TXHISTORY",
        payload: { txHistory },
      })
    },
    setWalletAssets: (walletAssets: Assets) => {
      dispatch({
        type: "SET_WALLET_ASSETS",
        payload: { walletAssets },
      })
    },
    setWalletUtxos: (txIns: TxInput[]) => {
      dispatch({
        type: "SET_WALLET_UTXOS",
        payload: { walletUtxos: txIns },
      })
    },
    setSeedPhraseWordCount: (seedPhraseWordCount: SeedPhraseWordCount) => {
      dispatch({
        type: "SET_SEED_PHRASE_WORD_COUNT",
        payload: { seedPhraseWordCount },
      })
    },
    resetSecrets: () => {
      dispatch({ type: "RESET_SECRETS" })
    },
    resetState: () => {
      dispatch({ type: "RESET_STATE" })
    },
  }
}
