import { Redeemer } from "@emurgo/csl-mobile-bridge"
import { AnyObject } from "yup/lib/types"

export type TxHash = string
export type Unit = string
export type RedeemerType = "Cancel" | "Complete" | "Recycle"

export type WalletNavigationParams = {
  isNewWalletCreation?: boolean
  mnemonic?: string
  baseAddress?: string
  rootKey?: string
  accountKey?: string
  accountPubKey?: string
}
export type PasswordSetUpFormValues = {
  name?: string
  password: string
  password_confirm: string
}
export interface WalletKeys {
  baseAddress: string
  rootKeyHex: string
  accountKeyHex: string
  accountPubKeyHex: string
}
export type WalletAssets = Map<string, AssetUnit>
export type AssetUnit = {
  policyId: string
  name: string
  count: string
  label: string
}
export type BlockfrostTx = {
  tx_hash: string
  tx_index: number
  block_height: number
  block_time: string
}
export type BlockFrostUtxoInfo = Array<{
  address: string
  amount: Array<{
    unit: string
    quantity: string
  }>
  tx_hash: string
  output_index: number
  data_hash: null
  inline_datum: null
  reference_script_hash: null
  collateral: boolean
  reference: boolean
}>
export type BlockFrostDetailedTx = {
  hash: string
  block_time: string // added manually
  user_address: string // added manually
  inputs: BlockFrostUtxoInfo
  outputs: BlockFrostUtxoInfo
}
export type SendRegularTxInfo = {
  receiverAddress: string
  lovelace: number
  assets: WalletAssets
}
export type SendLockingTxInfo = SendRegularTxInfo & {
  datum: AnyObject
}
export type SendUnlockingTxInfo = SendRegularTxInfo & {
  datum: AnyObject
  redeemer: Redeemer
}
