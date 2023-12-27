import { Redeemer } from "@emurgo/csl-mobile-bridge"
import { Assets } from "@hyperionbt/helios"
import { AnyObject } from "yup/lib/types"

export type UTxOId = string // <transactionId>#<outputIdx>
export type TxHash = string
export type Unit = string
export type RedeemerType = "Cancel" | "Complete" | "Recycle"
export type NetworkId = "Mainnet" | "Preprod"
export type SeedPhraseWordCount = 12 | 15 | 18 | 21 | 24

export const TX_GET_SIZE = 20
export const PURPOSE = 2147485500
export const COIN_TYPE = 2147485463
export const HARD_DERIVATION_START = 2147483648

export type PromiseHandlerRes = { data: any; error: string; statusCode: number }

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
// base addresses for acct idx 0
export type Addresses = {
  mainnet: string
  testnet: string // preprod
}
export interface WalletKeys {
  addresses: Addresses
  rootKeyHex: string
  accountKeyHex: string
  accountPubKeyHex: string
}
export type WalletAssets = Map<string, AssetUnit>
export type AssetUnit = {
  policyId: string
  name: string
  displayName?: string
  count: string | number
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
  lovelace: number | bigint
  assets: WalletAssets | undefined
}
export type SendLockingTxInfo = SendRegularTxInfo & {
  datum: AnyObject
}
export type SendUnlockingTxInfo = SendRegularTxInfo & {
  datum: AnyObject
  redeemer: Redeemer
}
export type PaymentTokens = {
  lovelace: number
  assets: Assets
}
export type EscrowContractDatum = {
  beneficiaryPkh: string
  benefactorPkh: string
  releaseDate: number
  cancelFee: number
  cancelWindowStart: number
  cancelWindowEnd: number
  createdAt: number
  paymentTokens: string // JSON representation of hourly rate payment tokens
}
