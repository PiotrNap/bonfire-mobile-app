import { TxInput } from "@hyperionbt/helios"
import { WalletActions, WalletTypes } from "common/types/contextTypes"
import {
  SendLockingTxInfo,
  SendRegularTxInfo,
  SendUnlockingTxInfo,
  Assets,
} from "lib/wallet/types"
import * as React from "react"

type SendTxInfo = SendRegularTxInfo | SendLockingTxInfo | SendUnlockingTxInfo

interface InitialState {
  lovelaceBalance: bigint
  assetsBalance: {}
  txHistory: any[]
  signedTx: {}
  mnemonic: {}
  walletUtxos: TxInput[]
  walletAssets: Assets | null
  rootKeyHex: string
  baseAddress: string
  accountPubKeyHex: string
  accountKeyHex: string
  isOfflineMnemonic: boolean
  sendTxInfo: SendTxInfo | null
}
interface WalletContextProps {
  state: InitialState
  dispatch: React.Dispatch<any>
}

const initState: InitialState = {
  lovelaceBalance: 0n,
  assetsBalance: {},
  txHistory: [],
  signedTx: {},
  mnemonic: {},
  walletUtxos: [],
  walletAssets: null,
  rootKeyHex: "",
  baseAddress: "",
  accountKeyHex: "",
  accountPubKeyHex: "",
  isOfflineMnemonic: false,
  sendTxInfo: null,
}

const reducer = (state: InitialState, action: WalletActions) => {
  switch (action.type) {
    case WalletTypes.SetWalletUtxos:
      return {
        ...state,
        walletUtxos: action.payload.walletUtxos,
      }
    case WalletTypes.SetWalletAssets:
      return {
        ...state,
        walletAssets: action.payload.walletAssets,
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
    case WalletTypes.SetSendTxInfo:
      return {
        ...state,
        sendTxInfo: action.payload.sendTxInfo,
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
    case WalletTypes.SetBaseAddress:
      return {
        ...state,
        baseAddress: action.payload.baseAddress,
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
