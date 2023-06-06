export interface Wallet {
  add_new_wallet: AddNewWallet
  import_wallet: ImportWallet
  risk_acknowledgement: RiskAcknowledgement
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

export interface ImportWallet {
  import_mnemonics: AddressConfirmation
  address_confirmation: AddressConfirmation
}

export interface AddressConfirmation {
  header: string
  body: string
}

export interface RiskAcknowledgement {
  header: string
  body_items: BodyItem[]
}

export interface BodyItem {
  text: string
}
