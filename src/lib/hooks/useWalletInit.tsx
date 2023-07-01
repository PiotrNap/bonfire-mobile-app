import * as R from "react"
import { Wallet } from "lib/wallet"
import { randomBytes } from "react-native-randombytes"
import { generateMnemonic as bip39, validateMnemonic } from "bip39"

type Mnemonics = { [index: string]: string }

const testSeed = {
  "0": "faculty",
  "1": "soda",
  "2": "medal",
  "3": "pistol",
  "4": "club",
  "5": "paddle",
  "6": "muffin",
  "7": "lyrics",
  "8": "dumb",
  "9": "leopard",
  "10": "rack",
  "11": "morning",
  "12": "demise",
  "13": "under",
  "14": "conduct",
}

export const useWalletInit = () => {
  const [mnemonic, setMnemonic] = R.useState<Mnemonics | null>(testSeed)
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

  return { error, mnemonic, setMnemonic, init, validMnemonic, generateMnemonic }
}
