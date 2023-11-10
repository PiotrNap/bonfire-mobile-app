import { TxInput } from "@hyperionbt/helios"
import { EventCardInfo, OrganizerRate } from "common/interfaces/bookingInterface"
import {
  EventAvailability,
  EventType,
  HourlyRate,
  SelectedWeekDays,
  TextContent,
} from "common/interfaces/newEventInterface"
import { ColorSchemeName, JWTPayload, UserSettings } from "interfaces/appInterface"
import {
  Availabilities,
  CalendarHeader,
  MyCalendarState,
  NewCalendarMonths,
  PreviewingDayEvents,
  Events,
} from "interfaces/myCalendarInterface"
import {
  SendRegularTxInfo,
  SendLockingTxInfo,
  SendUnlockingTxInfo,
  WalletKeys,
  WalletAssets,
} from "lib/wallet/types"

export enum AppTypes {
  ToggleAuth = "TOGGLE_AUTH",
  SetRef = "SET_REF",
  SetJWT = "SET_JWT",
  SetReceivingAddr = "SET_REC_ADDR",
  SetPageIndex = "SET_PAGE_INDEX",
  SetUserSettings = "SET_USER_SETTINGS",
  SetColorScheme = "SET_COLOR_SCHEME",
  SetValidGoogleOAuth = "SET_VALID_GOOGLE_OAUTH",
  SetFavoriteOrganizer = "SET_FAVORITE_ORGANIZER",
  SetBottomnavigationHeight = "SET_BOTTOMNAVIGATION_HEIGHT",
  SetQrCodeValue = "SET_QRCODE_VALUE",
  ResetState = "RESET_STATE",
}

export enum EventCreationTypes {
  SetTextContent = "SET_TEXT_CONTENT",
  AddAvailability = "SET_AVAILABILITIES",
  RemoveAvailability = "REMOVE_AVAILABILITY",
  RemoveAvailabilities = "REMOVE_AVAILABILITIES",
  SetSelectedDays = "SET_SELECTED_DAYS",
  RemoveSelectedDays = "REMOVE_SELECTED_DAYS",
  SetSelectedWeek = "SET_SELECTED_WEEKS",
  RemoveSelectedWeeks = "REMOVE_SELECTED_WEEKS",
  SetDateFrame = "SET_DATE_FRAME",
  SetTags = "SET_TAGS",
  SetImageURI = "SET_IMAGE_URI",
  SetHourlyRate = "SET_HOURLY_RATE",
  SetPrivateEvent = "SET_PRIVATE_EVENT",
  SetEventType = "SET_EVENT_TYPE",
  SetEventCardColor = "SET_EVENT_CARD_COLOR",
  SetEventTitleColor = "SET_EVENT_TITLE_COLOR",
  SetGCalEventsBooking = "SET_GCAL_EVENTS_BOOKING",
  ResetState = "RESET_STATE",
}

export enum WalletTypes {
  SetLovelaceBalance = "SET_LOVELACE_BALANCE",
  SetWalletUtxos = "SET_WALLET_UTXOS",
  SetTxHistory = "SET_TXHISTORY",
  SetMnemonic = "SET_MNEMONIC",
  SetWalletKeys = "SET_WALLET_KEYS",
  SetWalletAssets = "SET_WALLET_ASSETS",
  SetSendTxInfo = "SET_SEND_TX_INFO",
  SetBaseAddress = "SET_BASE_ADDRESS",
  SetIsOfflineMnemonic = "SET_IS_OFFLINE_MNEMONIC",
  ResetSecrets = "RESET_SECRETS",
  ResetState = "RESET_STATE",
}

export enum BookingTypes {
  SetPickedDate = "SET_PICKED_DATE",
  SetDuration = "SET_DURATION",
  SetEventTitle = "SET_EVENT_TITLE",
  SetDurationCost = "SET_DURATION_COST",
  SetOrganizerRate = "SET_ORGANIZER_RATE",
  SetPreviewingOrganizer = "SET_PREVIEWING_ORGANIZER",
  SetPreviewingEvent = "SET_PREVIEWING_EVENT",
  SetEventCardInfo = "SET_EVENT_CARD_INFO",
  SetMaxTimeSlotDuration = "SET_MAX_TIME_SLOT_DUR",
  SetMinTimeSlotDuration = "SET_MIN_TIME_SLOT_DUR",
  SetCreateGoogleCalEvent = "SET_CREATE_GCAL_EVENT",
  ResetState = "RESET_STATE",
}

export enum MyCalendarTypes {
  AddEvent = "ADD_EVENT",
  AddAvailability = "ADD_AVAILABILITY",
  LoadMyCalendar = "LOAD_MY_CALENDAR",
  LoadInitialMyCalendar = "LOAD_INITIAL_MY_CALENDAR",
  ChangeMonthHeader = "CHANGE_MONTH_HEADER",
  PreviewDayEvents = "PREVIEW_DAY_EVENTS",
  ClearDayPreview = "CLEAR_DAY_PREVIEW",
  CalendarDirection = "SET_CALENDAR_DIRECTION",
  SetAvailCalendar = "SET_AVAIL_CALENDAR",
  SetCurrentSelectedDay = "SET_CURR_SELECTED_DAY",
  SetEvents = "SET_EVENTS",
  UpdateCalendarMonth = "UPDATE_CALENDAR_MONTH",
  ResetState = "RESET_STATE",
}

/**
 * @name ActionMap
 * @description maps through indexed types of payloads types and assigns
 *              the payload accordingly (if the action type isn't undefined)
 */
export type ActionMap<M extends { [index: string]: any }> = {
  [Key in keyof M]: M[Key] extends undefined
    ? {
        type: Key
      }
    : {
        type: Key
        payload: M[Key]
      }
}

export type AppPayload = {
  [AppTypes.ToggleAuth]: {
    auth: boolean | null
    accountType: string | null
  }
  [AppTypes.SetRef]: {
    ref: any
  }
  [AppTypes.SetJWT]: {
    jwtPayload: JWTPayload
  }
  [AppTypes.SetBottomnavigationHeight]: {
    height: number
  }
  [AppTypes.SetPageIndex]: {
    pageIndex: number
  }
  [AppTypes.SetUserSettings]: {
    userSettings: UserSettings
  }
  [AppTypes.SetReceivingAddr]: {
    receivingAddr: string
  }
  [AppTypes.SetFavoriteOrganizer]: {
    alias: string
  }
  [AppTypes.SetColorScheme]: {
    newColorScheme: ColorSchemeName
  }
  [AppTypes.SetValidGoogleOAuth]: {
    validGoogleOAuth: boolean
  }
  [AppTypes.SetQrCodeValue]: {
    qrCodeValue: string
  }
  [AppTypes.ResetState]: {}
  ["unknown"]: any
}

export type EventCreationPayload = {
  [EventCreationTypes.AddAvailability]: {
    availability: EventAvailability
  }
  [EventCreationTypes.RemoveAvailability]: {
    availability: EventAvailability
  }
  [EventCreationTypes.RemoveAvailabilities]: {}
  [EventCreationTypes.SetSelectedDays]: {
    selectedDays: number[]
    isRecurringSelection?: boolean
    eventType: EventType
  }
  [EventCreationTypes.RemoveSelectedDays]: any
  [EventCreationTypes.SetSelectedWeek]: {
    selectedWeek: SelectedWeekDays
  }
  [EventCreationTypes.SetDateFrame]: {
    fromDate: Date
    toDate: Date
  }
  [EventCreationTypes.RemoveSelectedWeeks]: any
  [EventCreationTypes.SetHourlyRate]: {
    hourlyRate: HourlyRate
  }
  [EventCreationTypes.SetImageURI]: {
    imageURI: string
  }
  [EventCreationTypes.SetTags]: {
    tags: string[]
  }
  [EventCreationTypes.SetTextContent]: {
    textContent: TextContent
  }
  [EventCreationTypes.SetPrivateEvent]: {
    privateEvent: boolean
  }
  [EventCreationTypes.SetEventType]: {
    eventType: EventType
  }
  [EventCreationTypes.SetEventCardColor]: {
    eventCardColor: string
  }
  [EventCreationTypes.SetEventTitleColor]: {
    eventTitleColor: string
  }
  [EventCreationTypes.SetGCalEventsBooking]: {
    gCalEventsBooking: boolean
  }
  [EventCreationTypes.ResetState]: any
  ["unknown"]: any
}

export type BookingPayload = {
  [BookingTypes.SetDuration]: {
    duration: number
  }
  [BookingTypes.SetDurationCost]: {
    durationCost: number
  }
  [BookingTypes.SetEventTitle]: {
    title: string
  }
  [BookingTypes.SetOrganizerRate]: {
    organizerRate: OrganizerRate
  }
  [BookingTypes.SetPickedDate]: {
    pickedDate: number
  }
  [BookingTypes.SetMaxTimeSlotDuration]: {
    maxTimeSlotDuration: number
  }
  [BookingTypes.SetMinTimeSlotDuration]: {
    minTimeSlotDuration: number
  }
  [BookingTypes.SetPreviewingOrganizer]: {
    previewingOrganizer: any
  }
  [BookingTypes.SetPreviewingEvent]: {
    previewingEvent: any
  }
  [BookingTypes.SetEventCardInfo]: {
    eventCardInfo: EventCardInfo
  }
  [BookingTypes.SetCreateGoogleCalEvent]: {
    createGoogleCalEvent: boolean
  }
  [BookingTypes.ResetState]: {}
  ["unknown"]: any
}

export type MyCalendarPaylaod = {
  [MyCalendarTypes.AddEvent]: {
    event: Events
  }
  [MyCalendarTypes.AddAvailability]: {
    availabilities: Availabilities[]
  }
  [MyCalendarTypes.LoadInitialMyCalendar]: {
    calendarArgs: NewCalendarMonths
  }
  [MyCalendarTypes.LoadMyCalendar]: {
    calendarArgs: NewCalendarMonths
  }
  [MyCalendarTypes.ChangeMonthHeader]: {
    calendarHeader: CalendarHeader
  }
  [MyCalendarTypes.PreviewDayEvents]: {
    newPreviewingDayEvents: PreviewingDayEvents
  }
  [MyCalendarTypes.ResetState]: {
    calendarState?: MyCalendarState
  }
  [MyCalendarTypes.CalendarDirection]: {
    direction: null | "previous" | "next"
  }
  [MyCalendarTypes.SetAvailCalendar]: {
    availabilities?: any
    calendarArgs?: NewCalendarMonths
  }
  [MyCalendarTypes.SetCurrentSelectedDay]: {
    selectedDay: any
  }
  [MyCalendarTypes.SetEvents]: {
    events: any
  }
  [MyCalendarTypes.UpdateCalendarMonth]: {
    calendarArgs: NewCalendarMonths
  }
  [MyCalendarTypes.ClearDayPreview]: object
  ["unknown"]: any
}

export type WalletPayload = {
  [WalletTypes.SetLovelaceBalance]: {
    lovelace: bigint
  }
  [WalletTypes.SetWalletUtxos]: {
    walletUtxos: TxInput[]
  }
  [WalletTypes.SetTxHistory]: {
    txHistory: any[]
  }
  [WalletTypes.SetSendTxInfo]: {
    sendTxInfo: SendRegularTxInfo | SendLockingTxInfo | SendUnlockingTxInfo
  }
  [WalletTypes.SetMnemonic]: {
    mnemonic: {}
  }
  [WalletTypes.SetBaseAddress]: {
    baseAddress: {}
  }
  [WalletTypes.SetWalletKeys]: {
    walletKeys: WalletKeys
  }
  [WalletTypes.SetWalletAssets]: {
    walletAssets: WalletAssets
  }
  [WalletTypes.SetIsOfflineMnemonic]: {
    isOfflineMnemonic: boolean
  }
  [WalletTypes.ResetSecrets]: {}
  [WalletTypes.ResetState]: {}
  ["unknown"]: any
}

export type MyCalendarActions =
  ActionMap<MyCalendarPaylaod>[keyof ActionMap<MyCalendarPaylaod>]
export type AppActions = ActionMap<AppPayload>[keyof ActionMap<AppPayload>]
export type WalletActions = ActionMap<WalletPayload>[keyof ActionMap<WalletPayload>]
export type BookingActions = ActionMap<BookingPayload>[keyof ActionMap<BookingPayload>]
export type EventCreationActions =
  ActionMap<EventCreationPayload>[keyof ActionMap<EventCreationPayload>]
