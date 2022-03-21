import { OrganizerRate } from "common/interfaces/bookingInterface";
import {
  EventAvailability,
  SelectedWeekDays,
  TextContent,
} from "common/interfaces/newEventInterface";
import { ColorSchemeName, JWTPayload } from "interfaces/appInterface";
import {
  Availabilities,
  CalendarHeader,
  MyCalendarState,
  NewCalendarMonths,
  PreviewingDayEvents,
  Events,
} from "interfaces/myCalendarInterface";

export enum AppTypes {
  ToggleAuth = "TOGGLE_AUTH",
  SetRef = "SET_REF",
  SetJWT = "SET_JWT",
  SetPageIndex = "SET_PAGE_INDEX",
  SetColorScheme = "SET_COLOR_SCHEME",
  SetFavoriteOrganizer = "SET_FAVORITE_ORGANIZER",
}

export enum EventCreationTypes {
  SetTextContent = "SET_TEXT_CONTENT",
  AddAvailability = "SET_AVAILABILITIES",
  RemoveAvailability = "REMOVE_AVAILABILITIES",
  SetSelectedDays = "SET_SELECTED_DAYS",
  RemoveSelectedDays = "REMOVE_SELECTED_DAYS",
  SetSelectedWeek = "SET_SELECTED_WEEKS",
  RemoveSelectedWeeks = "REMOVE_SELECTED_WEEKS",
  SetDateFrame = "SET_DATE_FRAME",
  SetTags = "SET_TAGS",
  SetImageURI = "SET_IMAGE_URI",
  SetHourlyRate = "SET_HOURLY_RATE",
  SetPrivateEvent = "SET_PRIVATE_EVENT",
  SetEventCardColor = "SET_EVENT_CARD_COLOR",
  SetEventTitleColor = "SET_EVENT_TITLE_COLOR",
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
  SetMaxTimeSlotDuration = "SET_MAX_TIME_SLOT_DUR",
  SetMinTimeSlotDuration = "SET_MIN_TIME_SLOT_DUR",
  ResetState = "RESET_STATE",
}

export enum MyCalendarTypes {
  AddEvent = "ADD_EVENT",
  AddAvailability = "ADD_AVAILABILITY",
  ResetState = "RESET_STATE",
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
}

/**
 * @name ActionMap
 * @description maps through indexed types of payloads types and assigns
 *              the payload accordingly (if the action type isn't undefined)
 */
export type ActionMap<M extends { [index: string]: any }> = {
  [Key in keyof M]: M[Key] extends undefined
    ? {
        type: Key;
      }
    : {
        type: Key;
        payload: M[Key];
      };
};

export type AppPayload = {
  [AppTypes.ToggleAuth]: {
    auth: boolean | null;
    accountType: string | null;
  };
  [AppTypes.SetRef]: {
    ref: any;
  };
  [AppTypes.SetJWT]: {
    jwtPayload: JWTPayload;
  };
  [AppTypes.SetPageIndex]: {
    pageIndex: number;
  };
  [AppTypes.SetFavoriteOrganizer]: {
    alias: string;
  };
  [AppTypes.SetColorScheme]: {
    newColorScheme: ColorSchemeName;
  };
  ["unknown"]: any;
};

export type EventCreationPayload = {
  [EventCreationTypes.AddAvailability]: {
    availability: EventAvailability;
  };
  [EventCreationTypes.RemoveAvailability]: {
    availability: EventAvailability;
  };
  [EventCreationTypes.SetSelectedDays]: {
    selectedDays: number[];
    isRecurringSelection: boolean;
  };
  [EventCreationTypes.RemoveSelectedDays]: any;
  [EventCreationTypes.SetSelectedWeek]: {
    selectedWeek: SelectedWeekDays;
  };
  [EventCreationTypes.SetDateFrame]: {
    fromDate: Date;
    toDate: Date;
  };
  [EventCreationTypes.RemoveSelectedWeeks]: any;
  [EventCreationTypes.SetHourlyRate]: {
    hourlyRate: number;
  };
  [EventCreationTypes.SetImageURI]: {
    imageURI: string;
  };
  [EventCreationTypes.SetTags]: {
    tags: string[];
  };
  [EventCreationTypes.SetTextContent]: {
    textContent: TextContent;
  };
  [EventCreationTypes.SetPrivateEvent]: {
    privateEvent: boolean;
  };
  [EventCreationTypes.SetEventCardColor]: {
    eventCardColor: string;
  };
  [EventCreationTypes.SetEventTitleColor]: {
    eventTitleColor: string;
  };
  [EventCreationTypes.ResetState]: any;
  ["unknown"]: any;
};

export type BookingPayload = {
  [BookingTypes.SetDuration]: {
    duration: number;
  };
  [BookingTypes.SetDurationCost]: {
    durationCost: number;
  };
  [BookingTypes.SetEventTitle]: {
    title: string;
  };
  [BookingTypes.SetOrganizerRate]: {
    organizerRate: OrganizerRate;
  };
  [BookingTypes.SetPickedDate]: {
    pickedDate: number;
  };
  [BookingTypes.SetMaxTimeSlotDuration]: {
    maxTimeSlotDuration: number;
  };
  [BookingTypes.SetMinTimeSlotDuration]: {
    minTimeSlotDuration: number;
  };
  [BookingTypes.SetPreviewingOrganizer]: {
    previewingOrganizer: any;
  };
  [BookingTypes.SetPreviewingEvent]: {
    previewingEvent: any;
  };
  [BookingTypes.ResetState]: {};
  ["unknown"]: any;
};

export type MyCalendarPaylaod = {
  [MyCalendarTypes.AddEvent]: {
    event: Events;
  };
  [MyCalendarTypes.AddAvailability]: {
    availabilities: Availabilities[];
  };
  [MyCalendarTypes.LoadInitialMyCalendar]: {
    calendarArgs: NewCalendarMonths;
  };
  [MyCalendarTypes.LoadMyCalendar]: {
    calendarArgs: NewCalendarMonths;
  };
  [MyCalendarTypes.ChangeMonthHeader]: {
    calendarHeader: CalendarHeader;
  };
  [MyCalendarTypes.PreviewDayEvents]: {
    newPreviewingDayEvents: PreviewingDayEvents;
  };
  [MyCalendarTypes.ResetState]: {
    calendarState?: MyCalendarState;
  };
  [MyCalendarTypes.CalendarDirection]: {
    direction: null | "previous" | "next";
  };
  [MyCalendarTypes.SetAvailCalendar]: {
    availabilities?: any;
  };
  [MyCalendarTypes.SetCurrentSelectedDay]: {
    selectedDay: any;
  };
  [MyCalendarTypes.SetEvents]: {
    events: any;
  };
  [MyCalendarTypes.UpdateCalendarMonth]: {
    calendarArgs: NewCalendarMonths;
  };
  [MyCalendarTypes.ClearDayPreview]: object;
  ["unknown"]: any;
};

export type MyCalendarActions =
  ActionMap<MyCalendarPaylaod>[keyof ActionMap<MyCalendarPaylaod>];
export type AppActions = ActionMap<AppPayload>[keyof ActionMap<AppPayload>];
export type BookingActions =
  ActionMap<BookingPayload>[keyof ActionMap<BookingPayload>];
export type EventCreationActions =
  ActionMap<EventCreationPayload>[keyof ActionMap<EventCreationPayload>];
