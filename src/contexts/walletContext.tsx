import { WalletActions, WalletTypes } from "common/types/contextTypes"
import * as React from "react"

interface InitialState {
  lovelaceBalance: number
  assetsBalance: null | {}
  txHistory: null | {}
  signedTx: null | {}
  mnemonic: null | {}
  rootKey: null | ArrayBuffer
}
interface WalletContextProps {
  state: InitialState
  dispatch: React.Dispatch<any>
}

const initState = {
  lovelaceBalance: 0,
  assetsBalance: null,
  txHistory: null,
  signedTx: null,
  mnemonic: null,
  rootKey: null,
}

const reducer = (state: InitialState, action: WalletActions) => {
  switch (action.type) {
    case WalletTypes.SetMnemonic:
      return {
        ...state,
        mnemonic: action.payload.mnemonic,
      }
    case WalletTypes.SetLovelaceBalance:
      return {
        ...state,
        lovelaceBalance: action.payload.lovelace,
      }
    case WalletTypes.ResetState: {
      return initState
    }
    default:
      throw Error(`Unknown type of action: ${action.type}`)
  }
}

export const WalletContext = React.createContext<WalletContextProps>({
  state: initState,
  dispatch: () => null,
})

export const WalletContextProvider = ({
  children,
}: {
  children: React.ReactNode
}) => {
  //@ts-ignore
  const [state, dispatch] = React.useReducer(reducer, initState)
  return (
    <WalletContext.Provider value={{ state, dispatch }}>
      {children}
    </WalletContext.Provider>
  )
}
