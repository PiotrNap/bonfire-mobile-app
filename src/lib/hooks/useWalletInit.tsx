import * as R from "react"
import { Wallet } from "lib/wallet"
import { randomBytes } from "react-native-randombytes"
import { generateMnemonic as bip39, validateMnemonic } from "bip39"

type Mnemonic = { [index: string]: string }

export const useWalletInit = () => {
  const [mnemonic, setMnemonic] = R.useState<Mnemonic | {}>({})
  const [mnemonicString, setMnemonicString] = R.useState<string>("")
  const [error, setError] = R.useState<string>("")

  const init = async (customMnemonic?: string) => {
    if (!mnemonic) return

    try {
      const wallet = await new Wallet().init(
        customMnemonic || Object.values(mnemonic).join(" ")
      )
      return wallet
    } catch (e) {
      setError(`Error: ${e.message}`)
      throw e
    }
  }

  const validMnemonic = () =>
    mnemonic && validateMnemonic(Object.values(mnemonic).join(" "))
  const generateMnemonic = () => bip39(128, randomBytes)

  return {
    error,
    mnemonic,
    setMnemonic,
    setMnemonicString,
    mnemonicString,
    init,
    validMnemonic,
    generateMnemonic,
  }
}
