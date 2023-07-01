export type WalletNavigationParams = {
  isNewWalletCreation?: boolean
  mnemonic?: string
  baseAddress?: string
  rootKey?: string
  accountKey?: string
  accountPubKey?: string
}
export type WalletSetUpFormValues = {
  name?: string
  password: string
  password_confirm: string
}
export type WalletKeys = {
  baseAddress: string
  rootKey: string
  accountKey: string
  accountPubKey: string
}
