/**
 * @name appContext
 * @desc Context & reducer for managing high-level application state
 * e.g.: auth, dark/light mode, theme preferences etc.
 */
import * as React from "react"
import { Appearance, ColorSchemeName } from "react-native"
import {
  AppState,
  AppContextProviderProps,
  AppContextProps,
} from "interfaces/appInterface"
import { AppActions, AppTypes } from "common/types/contextTypes"
import { Colors } from "styles/index"
import { wallet } from "../assets/textContent"

// Get the user preffered color scheme (light or dark)
const colorScheme: ColorSchemeName = Appearance.getColorScheme()
const appTextContent = { wallet }

const initialAppState: AppState = {
  authentication: false,
  accountType: null,
  receivingAddr: "",
  validGoogleOAuth: false,
  // JWT: {
  //   expiresIn: null,
  //   accessToken: null,
  // },
  colorScheme: colorScheme == null ? "light" : colorScheme,
  appBgColor:
    colorScheme === "dark" ? Colors.neutral.s600 : Colors.primary.neutral,
  favoriteOrganizers: [],
  pageIndex: 0,
  ref: null,
  userSettings: null,
  textContent: { wallet },
}

const reducer = (state: AppState, action: AppActions) => {
  switch (action.type) {
    case AppTypes.SetRef:
      return {
        ...state,
        ref: action.payload.ref,
      }
    case AppTypes.SetJWT:
      const { expiresIn, accessToken } = action.payload.jwtPayload

      return {
        ...state,
        JWT: {
          expiresIn: expiresIn ?? null,
          accessToken: accessToken ?? null,
        },
      }
    case AppTypes.SetValidGoogleOAuth: {
      return {
        ...state,
        validGoogleOAuth: action.payload.validGoogleOAuth,
      }
    }
    case AppTypes.ToggleAuth:
      return {
        ...state,
        authentication:
          action.payload.auth != null
            ? action.payload.auth
            : !state.authentication,
        accountType: action.payload.accountType,
      }
    case AppTypes.SetPageIndex:
      return {
        ...state,
        pageIndex: action.payload.pageIndex,
      }
    case AppTypes.SetUserSettings:
      return {
        ...state,
        userSettings: action.payload.userSettings,
      }
    case AppTypes.SetColorScheme:
      const isDarkMode = action.payload.newColorScheme === "dark"
      return {
        ...state,
        colorScheme: action.payload.newColorScheme,
        appBgColor: isDarkMode ? Colors.neutral.s600 : Colors.primary.neutral,
      }
    case AppTypes.SetFavoriteOrganizer:
      if (state.favoriteOrganizers.includes(action.payload.alias)) {
        const newFavoriteOrganizers = state.favoriteOrganizers.filter(
          (org) => org !== action.payload.alias
        )
        return {
          ...state,
          favoriteOrganizers: [...newFavoriteOrganizers],
        }
      }
      const newState = {
        ...state,
        favoriteOrganizers: [...state.favoriteOrganizers, action.payload.alias],
      }
      return newState
    case AppTypes.ResetState: {
      return {
        ...state,
        authentication: false,
        receivingAddr: "",
        // JWT: null,
        favoriteOrganizers: [],
        pageIndex: 0,
        ref: null,
        textContent: appTextContent,
      }
    }
    default:
      throw Error(`Unknown type of action: ${action.type}`)
  }
}

export const AppContext = React.createContext<AppContextProps>({
  state: initialAppState,
  dispatch: () => null,
})

export const AppContextProvider = ({ children }: AppContextProviderProps) => {
  //@ts-ignore
  const [state, dispatch] = React.useReducer(reducer, initialAppState)
  return (
    <AppContext.Provider value={{ state, dispatch }}>
      {children}
    </AppContext.Provider>
  )
}
