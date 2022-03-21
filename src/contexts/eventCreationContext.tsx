import * as React from "react";

import {
  ContextObjectProps,
  InitialState,
  ProviderProps,
} from "common/interfaces/newEventInterface";
import {
  EventCreationActions,
  EventCreationTypes,
} from "common/types/contextTypes";

const initialState: InitialState = {
  textContent: {
    title: "",
    description: "",
  },
  availabilities: [],
  selectedDays: {},
  fromDate: null,
  toDate: null,
  tags: [],
  hourlyRate: 0,
  imageURI: "",
  eventCardColor: "",
  eventTitleColor: "",
  selectedWeekDays: [],
  privateEvent: false,
};

const reducer = (
  state: InitialState,
  action: EventCreationActions
): InitialState => {
  switch (action.type) {
    case EventCreationTypes.SetTextContent: {
      return {
        ...state,
        textContent: action.payload.textContent,
      };
    }
    case EventCreationTypes.AddAvailability: {
      const { from, to, minDuration, maxDuration } =
        action.payload.availability;
      // Why is keep adding undefined...
      state.availabilities = state.availabilities.filter((el) => el != null);

      const availExists =
        !!state.availabilities.length &&
        state.availabilities.find(
          (el) =>
            el &&
            el.from === from &&
            el.to === to &&
            el.minDuration === minDuration &&
            el.maxDuration === maxDuration
        );
      if (!availExists) {
        state.availabilities.push(action.payload.availability);
      }

      return state;
    }
    case EventCreationTypes.RemoveAvailability: {
      const { from, to, minDuration, maxDuration } =
        action.payload.availability;
      const newAvailabilities = state.availabilities.filter(
        (el) =>
          el.from !== from ||
          el.to !== to ||
          el.minDuration !== minDuration ||
          el.maxDuration !== maxDuration
      );

      return {
        ...state,
        availabilities: newAvailabilities,
      };
    }
    case EventCreationTypes.SetSelectedDays: {
      let newSelectedDays: any = state.selectedDays || {};

      action.payload.selectedDays.map((day) => {
        if (
          !state.selectedDays ||
          !state.selectedDays[day] ||
          (action.payload.isRecurringSelection && true)
        ) {
          newSelectedDays[day] = day;
        } else {
          delete newSelectedDays[day];
          if (Object.keys(newSelectedDays).length === 0) newSelectedDays = null;
        }
      });

      return {
        ...state,
        selectedDays: newSelectedDays,
      };
    }
    case EventCreationTypes.SetSelectedWeek: {
      const selectedWeek = state.selectedWeekDays.find(
        (week) => week.date === action.payload.selectedWeek.date
      );

      if (selectedWeek) {
        for (let key of Object.keys(selectedWeek)) {
          selectedWeek[key] = action.payload.selectedWeek[key];
        }
      } else {
        state.selectedWeekDays.push(action.payload.selectedWeek);
      }

      return state;
    }
    case EventCreationTypes.SetHourlyRate: {
      return {
        ...state,
        hourlyRate: action.payload.hourlyRate,
      };
    }
    case EventCreationTypes.SetImageURI: {
      return {
        ...state,
        imageURI: action.payload.imageURI,
      };
    }
    case EventCreationTypes.SetTags: {
      return {
        ...state,
        tags: action.payload.tags,
      };
    }
    case EventCreationTypes.SetPrivateEvent: {
      return {
        ...state,
        privateEvent: action.payload.privateEvent,
      };
    }
    case EventCreationTypes.SetEventCardColor: {
      return {
        ...state,
        eventCardColor: action.payload.eventCardColor,
      };
    }
    case EventCreationTypes.SetEventTitleColor: {
      return {
        ...state,
        eventTitleColor: action.payload.eventTitleColor,
      };
    }
    case EventCreationTypes.RemoveSelectedDays: {
      return {
        ...state,
        selectedDays: {},
      };
    }
    case EventCreationTypes.RemoveSelectedWeeks: {
      return {
        ...state,
        selectedWeekDays: [],
      };
    }
    case EventCreationTypes.SetDateFrame: {
      return {
        ...state,
        fromDate: action.payload.fromDate,
        toDate: action.payload.toDate,
      };
    }
    case EventCreationTypes.ResetState: {
      return {
        textContent: {
          title: "",
          description: "",
        },
        availabilities: [],
        selectedDays: {},
        fromDate: null,
        toDate: null,
        tags: [],
        hourlyRate: 0,
        imageURI: "",
        eventCardColor: "",
        eventTitleColor: "",
        selectedWeekDays: [],
        privateEvent: false,
      };
    }
    default: {
      throw new Error(`Unknown type of action ${action.type}`);
    }
  }
};

export const EventCreationContext = React.createContext<ContextObjectProps>({
  state: initialState,
  dispatch: () => {},
});

export const EventCreationContextProvider = ({ children }: ProviderProps) => {
  const [state, dispatch] = React.useReducer(reducer, initialState);

  return (
    <EventCreationContext.Provider value={{ state, dispatch }}>
      {children}
    </EventCreationContext.Provider>
  );
};
