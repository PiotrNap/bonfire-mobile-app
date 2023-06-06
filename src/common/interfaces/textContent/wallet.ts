export interface Wallet {
  add_new_wallet: AddNewWallet
  import_wallet: ImportWallet
  create_wallet: CreateWallet
  risk_acknowledgement: RiskAcknowledgement
  common: Common
}

export interface AddNewWallet {
  modal: Modal
}

export interface Modal {
  body: string
  header: string
  button_title: string
  secondButton_title: string
}

export interface CreateWallet {
  mnemonic_info_modal: MnemonicInfoModal
}

export interface Common {
  wallet_set_up: WalletSetUp
}

export interface MnemonicInfoModal {
  header: string
  body: string
}

export interface ImportWallet {
  import_mnemonics: MnemonicInfoModal
  address_confirmation: MnemonicInfoModal
}

export interface RiskAcknowledgement {
  header: string
  body_items: BodyItem[]
}

export interface WalletSetUp {
  header: string
  body: string
}

export interface BodyItem {
  text: string
}
