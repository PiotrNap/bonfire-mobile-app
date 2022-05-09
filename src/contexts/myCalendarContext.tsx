/**
 * @name myCalendarContext
 * @description context for providing state of the user calendar,
 *              e.g booked meetings, events, scheduled 1 on 1 conversations
 */
import * as React from "react"
import {
  Month,
  MyCalendarContextProps,
  MyCalendarState,
} from "interfaces/myCalendarInterface"
import { MyCalendarActions, MyCalendarTypes } from "common/types/contextTypes"
import { getMonth, getCalendarMonth, getYear } from "lib/utils"

// import { customScheduledEvents as scheduledEvents } from "../api_data/customScheduledEvents";
// import { customAvailabilities as availabilities } from "../api_data/customAvailabilities";
import { months } from "common/types/calendarTypes"

export interface ContextProviderProps {
  children: React.ReactNode
}

export const initialState: MyCalendarState = {
  registrationDate: null,
  calendar: [
    ...getCalendarMonth(false, true),
    ...getCalendarMonth(true, false),
  ],
  availabilitiesCalendar: null,
  organizerAvailabilities: null,
  availabilities: null,
  events: null,
  direction: null,
  currentSelectedDay: null,
  calendarHeader: {
    month: months[getMonth()],
    year: getYear(),
    numOfEvents: 0,
    startingDate: null,
  },
}

const reducer = (state: MyCalendarState, action: MyCalendarActions) => {
  switch (action.type) {
    case MyCalendarTypes.AddEvent:
      if (state.events != null) {
        state.events.push(action.payload.event)
      }
      return {
        ...state,
      }
    case MyCalendarTypes.AddAvailability:
      return {
        ...state,
        availabilities: action.payload.availabilities,
      }
    case MyCalendarTypes.ChangeMonthHeader:
      return {
        ...state,
        calendarHeader: {
          month: action.payload.calendarHeader.month,
          year: action.payload.calendarHeader.year,
          numOfEvents: action.payload.calendarHeader.numOfEvents,
          startingDate: action.payload.calendarHeader.startingDate,
        },
      }
    case MyCalendarTypes.PreviewDayEvents:
      return {
        ...state,
        previewingDayEvents: action.payload.newPreviewingDayEvents,
      }
    case MyCalendarTypes.ClearDayPreview:
      delete state.previewingDayEvents
      return {
        ...state,
      }
    case MyCalendarTypes.CalendarDirection:
      return {
        ...state,
        direction: action.payload.direction,
      }
    case MyCalendarTypes.SetCurrentSelectedDay: {
      return {
        ...state,
        currentSelectedDay: action.payload.selectedDay,
      }
    }
    case MyCalendarTypes.LoadInitialMyCalendar: {
      const initialCalendar = [
        ...getCalendarMonth(
          false,
          true,
          undefined,
          undefined,
          [],
          state.events
        ),
        ...getCalendarMonth(
          true,
          false,
          undefined,
          undefined,
          [],
          state.events
        ),
      ]

      return {
        ...state,
        calendar: initialCalendar,
      }
    }
    case MyCalendarTypes.LoadMyCalendar:
      const nextMonths = action.payload.calendarArgs.nextMonths
      const year = action.payload.calendarArgs.year
      const month = action.payload.calendarArgs.month
      const _availabilities: any | undefined =
        action.payload.calendarArgs.availabilities
      const isNewCalendar = action.payload.calendarArgs.isNewCalendar
      const newCalendar: Month[] = [...state.calendar]
      const startFromCustomMonth =
        action.payload.calendarArgs.startFromCustomMonth

      if (nextMonths) {
        newCalendar.push(
          ...getCalendarMonth(
            true,
            false,
            month,
            year,
            _availabilities ?? state.availabilities,
            state.events,
            startFromCustomMonth
          )
        )
        newCalendar.splice(0, isNewCalendar ? 2 : 1)
      } else {
        newCalendar.splice(
          0,
          0,
          ...getCalendarMonth(
            false,
            true,
            month,
            year,
            _availabilities ?? state.availabilities,
            state.events,
            startFromCustomMonth
          )
        )
        newCalendar.splice(newCalendar.length - 1, 1)
      }
      return {
        ...state,
        calendar: newCalendar,
      }
    case MyCalendarTypes.UpdateCalendarMonth: {
      let year = action.payload.calendarArgs.year || new Date().getFullYear()
      let month = action.payload.calendarArgs.month
      const newCalendar: Month[] = [...state.calendar]
      const isNextMonth = action.payload.calendarArgs.nextMonths

      // 1. User goes from March to April ->
      //    ... new calendar gets updated ->
      //    ... April is now showing on calendar UI ->
      //    ... we need to get next month from `getCalendarMonth`
      //    like we'd do when going from March to April.
      //    Because the new calendar is now [April, May, June].
      if (month === 0) {
        if (isNextMonth) {
          year -= 1
          month = 11
        } else month = 1
      } else if (month === 11) {
        if (!isNextMonth) {
          year += 1
          month = 0
        } else month = 10
      }

      newCalendar.splice(
        1,
        1,
        ...getCalendarMonth(
          isNextMonth,
          !isNextMonth,
          month,
          year,
          state.availabilities,
          state.events
        )
      )

      return {
        ...state,
        calendar: newCalendar,
      }
    }
    case MyCalendarTypes.SetAvailCalendar:
      const availabilities =
        action.payload.availabilities != null
          ? action.payload.availabilities
          : state.organizerAvailabilities

      const calendar = [
        ...getCalendarMonth(false, true, undefined, undefined, availabilities),
        ...getCalendarMonth(true, false, undefined, undefined, availabilities),
      ]

      return {
        ...state,
        availabilities: action.payload.availabilities,
        calendar,
      }
    case MyCalendarTypes.SetEvents: {
      return {
        ...state,
        events: action.payload.events,
      }
    }
    case MyCalendarTypes.ResetState: {
      return initialState
    }
    default:
      throw Error(`Unknown type of action: ${action.type}`)
  }
}

export const MyCalendarContext = React.createContext<MyCalendarContextProps>({
  state: initialState,
  dispatch: () => null,
})

export const MyCalendarProvider = ({ children }: ContextProviderProps) => {
  const [state, dispatch] = React.useReducer(reducer, initialState)

  return (
    <MyCalendarContext.Provider value={{ state, dispatch }}>
      {children}
    </MyCalendarContext.Provider>
  )
}
