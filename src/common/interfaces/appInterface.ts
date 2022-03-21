/**
 * @interfaces for appContext.tsx file.
 */

export type ColorSchemeName = "light" | "dark";

export interface AppState {
  authentication: boolean | null;
  accountType: "attendee" | "organizer" | null;
  colorScheme: ColorSchemeName;
  favoriteOrganizers: any[];
  pageIndex: number;
  ref: any;
  JWT?: JWTPayload | null;
}

export interface JWTPayload {
  expiresIn: string | null;
  accessToken: string | null;
}

export interface AppContextProviderProps {
  children: React.ReactNode;
}

export interface AppContextProps {
  state: AppState;
  dispatch: React.Dispatch<any>;
}

export interface Transaction {
  withUser: string;
  oldUtxo: number;
  newUtxo: number;
  date: number;
}
