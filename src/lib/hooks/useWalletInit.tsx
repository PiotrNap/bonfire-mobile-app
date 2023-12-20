import * as R from "react"
import { Wallet } from "lib/wallet"
import { validateMnemonic, entropyToMnemonic } from "bip39"
import Crypto from "crypto"
import { SeedPhraseWordCount } from "lib/wallet/types"

const generateMnemonic = (wordCount: SeedPhraseWordCount = 12) => {
  let entropyLength
  switch (wordCount) {
    case 12:
      entropyLength = 16
      break // 128 bits
    case 15:
      entropyLength = 20
      break // 160 bits
    case 18:
      entropyLength = 24
      break // 192 bits
    case 21:
      entropyLength = 28
      break // 224 bits
    case 24:
      entropyLength = 32
      break // 256 bits
    default:
      throw new Error("Invalid word count. Must be 12, 15, 18, 21, or 24.")
  }

  const entropy = Crypto.randomBytes(entropyLength)
  return entropyToMnemonic(entropy)
}

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
