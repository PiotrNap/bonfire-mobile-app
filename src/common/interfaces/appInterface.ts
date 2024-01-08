/**
 * @interfaces for appContext.tsx file.
 */

import { NetworkId } from "lib/wallet/types"
import { Wallet } from "./textContent/wallet"

export type ColorSchemeName = "light" | "dark"

export interface AppState {
  authentication: boolean | null
  receivingAddr: string
  colorScheme: ColorSchemeName
  networkId: NetworkId
  appBgColor: string
  favoriteOrganizers: any[]
  validGoogleOAuth: boolean
  pageIndex: number
  ref: any
  bottomNavigationHeight: number
  JWT?: JWTPayload | null
  userSettings: UserSettings | null
  textContent: { wallet: Wallet }
  qrCodeValue: string | null
  deviceTopInsent: number
}

export interface JWTPayload {
  expiresIn: string | null
  accessToken: string | null
}

export interface AppContextProviderProps {
  children: React.ReactNode
}

export interface AppContextProps {
  state: AppState
  dispatch: React.Dispatch<any>
}

export interface UserSettings {
  eventCreationHintHidden?: boolean
  showPastCalendarEvents?: boolean
}
