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
