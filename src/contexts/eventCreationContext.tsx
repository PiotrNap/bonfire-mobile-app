import * as React from "react"

import {
  ContextObjectProps,
  InitialState,
  ProviderProps,
} from "common/interfaces/newEventInterface"
import { EventCreationActions, EventCreationTypes } from "common/types/contextTypes"

const initialState: InitialState = {
  textContent: {
    title: "",
    summary: "",
  },
  availabilities: [],
  selectedDates: {},
  fromDate: null,
  toDate: null,
  hourlyRate: [],
  cancellation: { fee: 0, window: 0 },
  eventType: "one-time",
  imageURI: "",
  eventCardColor: "",
  eventTitleColor: "",
  selectedWeekDays: [],
  visibility: "public",
  gCalEventsBooking: false,
}

const reducer = (state: InitialState, action: EventCreationActions): InitialState => {
  switch (action.type) {
    case EventCreationTypes.SetTextContent: {
      return {
        ...state,
        textContent: action.payload.textContent,
      }
    }
    case EventCreationTypes.AddAvailability: {
      const { from, to, minDuration, maxDuration } = action.payload.availability
      // Why is keep adding undefined...
      state.availabilities = state.availabilities.filter((el) => el != null)

      const availExists =
        !!state.availabilities.length &&
        state.availabilities.find(
          (el) =>
            el &&
            el.from === from &&
            el.to === to &&
            el.minDuration === minDuration &&
            el.maxDuration === maxDuration
        )
      if (!availExists) {
        state.availabilities.push(action.payload.availability)
      }

      return state
    }
    case EventCreationTypes.RemoveAvailability: {
      const { from, to, minDuration, maxDuration } = action.payload.availability
      const newAvailabilities = state.availabilities.filter(
        (el) =>
          el.from !== from ||
          el.to !== to ||
          el.minDuration !== minDuration ||
          el.maxDuration !== maxDuration
      )

      return {
        ...state,
        availabilities: newAvailabilities,
      }
    }
    case EventCreationTypes.RemoveAvailabilities: {
      return {
        ...state,
        availabilities: [],
      }
    }
    case EventCreationTypes.SetSelectedDates: {
      console.log("Here ???")
      return {
        ...state,
        selectedDates: action.payload.selectedDates,
      }
    }
    case EventCreationTypes.SetSelectedWeek: {
      const selectedWeek = state.selectedWeekDays.find(
        (week) => week.date === action.payload.selectedWeek.date
      )

      if (selectedWeek) {
        for (let key of Object.keys(selectedWeek)) {
          selectedWeek[key] = action.payload.selectedWeek[key]
        }
      } else {
        state.selectedWeekDays.push(action.payload.selectedWeek)
      }

      return state
    }
    case EventCreationTypes.SetHourlyRate: {
      return {
        ...state,
        hourlyRate: action.payload.hourlyRate,
      }
    }
    case EventCreationTypes.SetImageURI: {
      return {
        ...state,
        imageURI: action.payload.imageURI,
      }
    }
    case EventCreationTypes.SetEventType: {
      return {
        ...state,
        eventType: action.payload.eventType,
      }
    }
    case EventCreationTypes.SetEventCancellation: {
      return {
        ...state,
        cancellation: action.payload.cancellation,
      }
    }
    case EventCreationTypes.SetEventVisibility: {
      return {
        ...state,
        visibility: action.payload.visibility,
      }
    }
    case EventCreationTypes.SetEventCardColor: {
      return {
        ...state,
        eventCardColor: action.payload.eventCardColor,
      }
    }
    case EventCreationTypes.SetEventTitleColor: {
      return {
        ...state,
        eventTitleColor: action.payload.eventTitleColor,
      }
    }
    case EventCreationTypes.RemoveSelectedWeeks: {
      return {
        ...state,
        selectedWeekDays: [],
      }
    }
    case EventCreationTypes.SetDateFrame: {
      return {
        ...state,
        fromDate: action.payload.fromDate,
        toDate: action.payload.toDate,
      }
    }
    case EventCreationTypes.SetGCalEventsBooking: {
      return {
        ...state,
        gCalEventsBooking: action.payload.gCalEventsBooking,
      }
    }
    case EventCreationTypes.ResetState: {
      return initialState
    }
    default: {
      throw new Error(`Unknown type of action ${action.type}`)
    }
  }
}

export const EventCreationContext = React.createContext<ContextObjectProps>({
  state: initialState,
  dispatch: () => {},
})

export const EventCreationContextProvider = ({ children }: ProviderProps) => {
  const [state, dispatch] = React.useReducer(reducer, initialState)

  return (
    <EventCreationContext.Provider value={{ state, dispatch }}>
      {children}
    </EventCreationContext.Provider>
  )
}
