import { TxInput } from "@hyperionbt/helios"
import { WalletActions, WalletTypes } from "common/types/contextTypes"
import * as React from "react"

interface InitialState {
  lovelaceBalance: number
  assetsBalance: {}
  txHistory: any[]
  signedTx: {}
  mnemonic: {}
  txInputs: TxInput[]
  rootKeyHex: string
  baseAddress: string
  accountPubKeyHex: string
  accountKeyHex: string
  isOfflineMnemonic: boolean
}
interface WalletContextProps {
  state: InitialState
  dispatch: React.Dispatch<any>
}

const initState: InitialState = {
  lovelaceBalance: 0,
  assetsBalance: {},
  txHistory: [],
  signedTx: {},
  mnemonic: {},
  txInputs: [],
  rootKeyHex: "",
  baseAddress: "",
  accountKeyHex: "",
  accountPubKeyHex: "",
  isOfflineMnemonic: false,
}

const reducer = (state: InitialState, action: WalletActions) => {
  switch (action.type) {
    case WalletTypes.SetTxInputs:
      return {
        ...state,
        txInputs: action.payload.txInputs,
      }
    case WalletTypes.SetTxHistory:
      return {
        ...state,
        txHistory: action.payload.txHistory,
      }
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
    case WalletTypes.SetIsOfflineMnemonic:
      return {
        ...state,
        isOfflineMnemonic: action.payload.isOfflineMnemonic,
      }
    case WalletTypes.SetWalletKeys:
      const { walletKeys } = action.payload
      return {
        ...state,
        baseAddress: walletKeys.baseAddress,
        rootKeyHex: walletKeys.rootKeyHex,
        accountKeyHex: walletKeys.accountKeyHex,
        accountPubKeyHex: walletKeys.accountPubKeyHex,
      }
    case WalletTypes.ResetState: {
      return initState
    }
    case WalletTypes.ResetSecrets: {
      let newState: any = state
      delete newState.accountKeyHex
      delete newState.rootKeyHex
      delete newState.mnemonic

      return newState
    }
    default:
      throw Error(`Unknown type of action: ${action.type}`)
  }
}

export const WalletContext = React.createContext<WalletContextProps>({
  state: initState,
  dispatch: () => null,
})

export const WalletContextProvider = ({ children }: { children: React.ReactNode }) => {
  //@ts-ignore
  const [state, dispatch] = React.useReducer(reducer, initState)
  return (
    <WalletContext.Provider value={{ state, dispatch }}>
      {children}
    </WalletContext.Provider>
  )
}
