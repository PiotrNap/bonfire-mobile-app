import * as R from "react"
import { Wallet } from "lib/wallet"
import { validateMnemonic, entropyToMnemonic } from "bip39"
import Crypto from "crypto"

const entropy = Crypto.randomBytes(16)
const generateMnemonic = () => entropyToMnemonic(entropy)

type Mnemonic = { [index: string]: string }

export const useWalletInit = () => {
  const [mnemonic, setMnemonic] = R.useState<Mnemonic | {}>({})
  const [mnemonicString, setMnemonicString] = R.useState<string>("")

  const init = (customMnemonic?: string) => {
    if (!mnemonic) return

    return new Wallet().init(customMnemonic || Object.values(mnemonic).join(" "))
  }

  const validMnemonic = () =>
    mnemonic && validateMnemonic(Object.values(mnemonic).join(" "))

  return {
    mnemonic,
    setMnemonic,
    setMnemonicString,
    mnemonicString,
    init,
    validMnemonic,
    generateMnemonic,
  }
}
