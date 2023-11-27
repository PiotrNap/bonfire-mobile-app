import * as React from "react"

import { BookingContextProps, InitialState } from "common/interfaces/bookingInterface"
import { BookingActions, BookingTypes } from "common/types/contextTypes"

const initialState: InitialState = {
  pickedDate: "", // selected date in UTC ,eg '2023-11-23'
  pickedDateSlots: [],
  pickedDateSlotsMinDuration: [],
  pickedStartTime: "",
  duration: 0,
  durationCost: new Map(),
  eventTitle: "",
  eventCardInfo: null,
  organizerRate: null,
  maxTimeSlotDuration: 0,
  minTimeSlotDuration: 0,
  previewingOrganizer: null,
  previewingEvent: null,
  createGoogleCalEvent: false,
}

const reducer = (state: InitialState, action: BookingActions) => {
  switch (action.type) {
    case BookingTypes.SetDuration:
      return {
        ...state,
        duration: action.payload.duration,
      }
    case BookingTypes.SetPickedStartTime:
      return {
        ...state,
        pickedStartTime: action.payload.pickedStartTime,
      }
    case BookingTypes.SetDurationCost:
      return {
        ...state,
        durationCost: action.payload.durationCost,
      }
    case BookingTypes.SetEventTitle:
      return {
        ...state,
        eventTitle: action.payload.title,
      }
    case BookingTypes.SetOrganizerRate:
      return {
        ...state,
        organizerRate: action.payload.organizerRate,
      }
    case BookingTypes.SetPickedDate:
      return {
        ...state,
        pickedDate: action.payload.pickedDate,
      }
    case BookingTypes.SetPickedDateSlots:
      return {
        ...state,
        pickedDateSlots: action.payload.pickedDateSlots,
      }
    case BookingTypes.SetPickedDateSlotsMinDuration:
      return {
        ...state,
        pickedDateSlotsMinDuration: action.payload.pickedDateSlotsMinDuration,
      }
    case BookingTypes.SetMaxTimeSlotDuration:
      return {
        ...state,
        maxTimeSlotDuration: action.payload.maxTimeSlotDuration,
      }
    case BookingTypes.SetMinTimeSlotDuration:
      return {
        ...state,
        minTimeSlotDuration: action.payload.minTimeSlotDuration,
      }
    case BookingTypes.SetPreviewingOrganizer:
      return {
        ...state,
        previewingOrganizer: action.payload.previewingOrganizer,
      }
    case BookingTypes.SetPreviewingEvent:
      return {
        ...state,
        previewingEvent: action.payload.previewingEvent,
      }
    case BookingTypes.SetEventCardInfo: {
      return {
        ...state,
        eventCardInfo: action.payload.eventCardInfo,
      }
    }
    case BookingTypes.SetCreateGoogleCalEvent: {
      return {
        ...state,
        createGoogleCalEvent: action.payload.createGoogleCalEvent,
      }
    }
    case BookingTypes.ResetState: {
      return initialState
    }
    default:
      throw Error(`Unknown type of action: ${action.type}`)
  }
}

export const BookingContext = React.createContext<BookingContextProps>({
  state: initialState,
  dispatch: () => null,
})

export const BookingContextProvider = ({ children }: { children: React.ReactNode }) => {
  const [state, dispatch] = React.useReducer(reducer, initialState)

  return (
    <BookingContext.Provider value={{ state, dispatch }}>
      {children}
    </BookingContext.Provider>
  )
}
